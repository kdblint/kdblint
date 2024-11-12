const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringIndexAdapter = std.hash_map.StringIndexAdapter;
const StringIndexContext = std.hash_map.StringIndexContext;

const kdb = @import("root.zig");
const Ast = kdb.Ast;
const Zir = kdb.Zir;

const AstGen = @This();

gpa: Allocator,
tree: *const Ast,
instructions: std.MultiArrayList(Zir.Inst) = .{},
extra: ArrayListUnmanaged(u32) = .empty,
string_bytes: ArrayListUnmanaged(u8) = .empty,
/// Used for temporary allocations; freed after AstGen is complete.
/// The resulting ZIR code has no references to anything in this arena.
arena: Allocator,
string_table: std.HashMapUnmanaged(
    u32,
    void,
    StringIndexContext,
    std.hash_map.default_max_load_percentage,
) = .empty,
compile_errors: ArrayListUnmanaged(Zir.Inst.CompileErrors.Item) = .empty,
/// Whether we are somewhere within a function. If `true`, any container decls may be
/// generic and thus must be tunneled through closure.
within_fn: bool = false,
/// Used for temporary storage when building payloads.
scratch: std.ArrayListUnmanaged(u32) = .empty,

const InnerError = error{ OutOfMemory, AnalysisFail };

fn addExtra(astgen: *AstGen, extra: anytype) Allocator.Error!u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    try astgen.extra.ensureUnusedCapacity(astgen.gpa, fields.len);
    return addExtraAssumeCapacity(astgen, extra);
}

fn addExtraAssumeCapacity(astgen: *AstGen, extra: anytype) u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    const extra_index: u32 = @intCast(astgen.extra.items.len);
    astgen.extra.items.len += fields.len;
    setExtra(astgen, extra_index, extra);
    return extra_index;
}

fn setExtra(astgen: *AstGen, index: usize, extra: anytype) void {
    const fields = std.meta.fields(@TypeOf(extra));
    var i = index;
    inline for (fields) |field| {
        astgen.extra.items[i] = switch (field.type) {
            u32 => @field(extra, field.name),

            Zir.Inst.Index,
            Zir.NullTerminatedString,
            => @intFromEnum(@field(extra, field.name)),

            i32,
            => @bitCast(@field(extra, field.name)),

            else => @compileError("bad field type"),
        };
        i += 1;
    }
}

pub fn generate(gpa: Allocator, tree: Ast) Allocator.Error!Zir {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var astgen: AstGen = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .tree = &tree,
    };
    defer astgen.deinit(gpa);

    // String table index 0 is reserved for `NullTerminatedString.empty`.
    try astgen.string_bytes.append(gpa, 0);

    // We expect at least as many ZIR instructions and extra data items
    // as AST nodes.
    try astgen.instructions.ensureTotalCapacity(gpa, tree.nodes.len);

    // First few indexes of extra are reserved and set at the end.
    const reserved_count = @typeInfo(Zir.ExtraIndex).@"enum".fields.len;
    try astgen.extra.ensureTotalCapacity(gpa, tree.nodes.len + reserved_count);
    astgen.extra.items.len += reserved_count;

    var top_scope: Scope.Top = .{};

    var gz_instructions: std.ArrayListUnmanaged(Zir.Inst.Index) = .empty;
    var gen_scope: GenZir = .{
        .is_comptime = true,
        .parent = &top_scope.base,
        .anon_name_strategy = .parent,
        .decl_node_index = 0,
        .decl_line = 0,
        .astgen = &astgen,
        .instructions = &gz_instructions,
        .instructions_top = 0,
    };
    defer gz_instructions.deinit(gpa);

    // The AST -> ZIR lowering process assumes an AST that does not have any
    // parse errors.
    if (tree.errors.len == 0) {
        if (AstGen.structDeclInner(
            &gen_scope,
            &gen_scope.base,
            0,
            tree.rootDecls(),
            .auto,
            0,
        )) |struct_decl_ref| {
            _ = struct_decl_ref; // autofix
            // assert(struct_decl_ref.toIndex().? == .main_struct_inst);
        } else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.AnalysisFail => {}, // Handled via compile_errors below.
        }
    } else {
        try lowerAstErrors(&astgen);
    }

    const err_index = @intFromEnum(Zir.ExtraIndex.compile_errors);
    if (astgen.compile_errors.items.len == 0) {
        astgen.extra.items[err_index] = 0;
    } else {
        try astgen.extra.ensureUnusedCapacity(gpa, 1 + astgen.compile_errors.items.len *
            @typeInfo(Zir.Inst.CompileErrors.Item).@"struct".fields.len);

        astgen.extra.items[err_index] = astgen.addExtraAssumeCapacity(Zir.Inst.CompileErrors{
            .items_len = @intCast(astgen.compile_errors.items.len),
        });

        for (astgen.compile_errors.items) |item| {
            _ = astgen.addExtraAssumeCapacity(item);
        }
    }

    return Zir{
        .instructions = astgen.instructions.toOwnedSlice(),
        .string_bytes = try astgen.string_bytes.toOwnedSlice(gpa),
        .extra = try astgen.extra.toOwnedSlice(gpa),
    };
}

fn deinit(astgen: *AstGen, gpa: Allocator) void {
    astgen.instructions.deinit(gpa);
    astgen.extra.deinit(gpa);
    astgen.string_bytes.deinit(gpa);
    astgen.compile_errors.deinit(gpa);
}

// TODO: Rename
fn structDeclInner(
    gz: *GenZir,
    scope: *Scope,
    node: Ast.Node.Index,
    blocks: []const Ast.Node.Index,
    layout: std.builtin.Type.ContainerLayout,
    backing_int_node: Ast.Node.Index,
) InnerError!Zir.Inst.Ref {
    _ = layout; // autofix
    _ = backing_int_node; // autofix

    const astgen = gz.astgen;
    const gpa = astgen.gpa;
    const tree = astgen.tree;
    _ = tree; // autofix

    const decl_inst = try gz.reserveInstructionIndex();

    var namespace: Scope.Namespace = .{
        .parent = scope,
        .node = node,
        .inst = decl_inst,
        .declaring_gz = gz,
        .maybe_generic = astgen.within_fn,
    };
    defer namespace.deinit(gpa);

    const scratch_top = astgen.scratch.items.len;
    defer astgen.scratch.shrinkRetainingCapacity(scratch_top);

    const decl_count = try astgen.scanContainer(&namespace, blocks, .@"struct");
    std.log.debug("decl_count = {d}", .{decl_count});
    var it = namespace.decls.keyIterator();
    while (it.next()) |key| {
        std.log.debug("decl = '{s}'", .{astgen.nullTerminatedString(key.*)});
    }

    return .none;
}

fn appendErrorTokNotes(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) !void {
    return appendErrorTokNotesOff(astgen, token, 0, format, args, notes);
}

fn appendErrorTokNotesOff(
    astgen: *AstGen,
    token: Ast.Token.Index,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) !void {
    @branchHint(.cold);
    const gpa = astgen.gpa;
    const string_bytes = &astgen.string_bytes;
    const msg: Zir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(gpa).print(format ++ "\x00", args);
    const notes_index: u32 = if (notes.len != 0) blk: {
        const notes_start = astgen.extra.items.len;
        try astgen.extra.ensureTotalCapacity(gpa, notes_start + 1 + notes.len);
        astgen.extra.appendAssumeCapacity(@intCast(notes.len));
        astgen.extra.appendSliceAssumeCapacity(notes);
        break :blk @intCast(notes_start);
    } else 0;
    try astgen.compile_errors.append(gpa, .{
        .msg = msg,
        .node = 0,
        .token = token,
        .byte_offset = byte_offset,
        .notes = notes_index,
    });
}

fn errNoteTok(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    return errNoteTokOff(astgen, token, 0, format, args);
}

fn errNoteTokOff(
    astgen: *AstGen,
    token: Ast.Token.Index,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Zir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(astgen.gpa).print(format ++ "\x00", args);
    return astgen.addExtra(Zir.Inst.CompileErrors.Item{
        .msg = msg,
        .node = 0,
        .token = token,
        .byte_offset = byte_offset,
        .notes = 0,
    });
}

fn errNoteNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!u32 {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Zir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(astgen.gpa).print(format ++ "\x00", args);
    return astgen.addExtra(Zir.Inst.CompileErrors.Item{
        .msg = msg,
        .node = node,
        .token = 0,
        .byte_offset = 0,
        .notes = 0,
    });
}

fn identAsString(astgen: *AstGen, ident_token: Ast.Token.Index) !Zir.NullTerminatedString {
    const gpa = astgen.gpa;
    const string_bytes = &astgen.string_bytes;
    const str_index: u32 = @intCast(string_bytes.items.len);
    try string_bytes.appendSlice(gpa, astgen.tree.tokenSlice(ident_token));
    const key: []const u8 = string_bytes.items[str_index..];
    const gop = try astgen.string_table.getOrPutContextAdapted(gpa, key, StringIndexAdapter{
        .bytes = string_bytes,
    }, StringIndexContext{
        .bytes = string_bytes,
    });
    if (gop.found_existing) {
        string_bytes.shrinkRetainingCapacity(str_index);
        return @enumFromInt(gop.key_ptr.*);
    } else {
        gop.key_ptr.* = str_index;
        try string_bytes.append(gpa, 0);
        return @enumFromInt(str_index);
    }
}

const Scope = struct {
    tag: Tag,

    fn cast(base: *Scope, comptime T: type) ?*T {
        if (T == Namespace) {
            switch (base.tag) {
                .namespace => return @alignCast(@fieldParentPtr("base", base)),
                else => return null,
            }
        }
        if (base.tag != T.base_tag)
            return null;

        return @alignCast(@fieldParentPtr("base", base));
    }

    const Tag = enum {
        gen_zir,
        local_val,
        local_ptr,
        namespace,
        top,
    };

    /// The category of identifier. These tag names are user-visible in compile errors.
    const IdCat = enum {
        @"function parameter",
        @"local variable",
    };

    /// This is always a `const` local and importantly the `inst` is a value type, not a pointer.
    /// This structure lives as long as the AST generation of the Block
    /// node that contains the variable.
    const LocalVal = struct {
        const base_tag: Tag = .local_val;
        base: Scope = Scope{ .tag = base_tag },
        /// Parents can be: `LocalVal`, `LocalPtr`, `GenZir`, `Defer`, `Namespace`.
        parent: *Scope,
        gen_zir: *GenZir,
        inst: Zir.Inst.Ref,
        /// Source location of the corresponding variable declaration.
        token_src: Ast.Token.Index,
        /// Track the first identifier where it is referenced.
        /// 0 means never referenced.
        used: Ast.Token.Index = 0,
        /// Track the identifier where it is discarded, like this `_ = foo;`.
        /// 0 means never discarded.
        discarded: Ast.Token.Index = 0,
        /// String table index.
        name: Zir.NullTerminatedString,
        id_cat: IdCat,
    };

    /// This could be a `const` or `var` local. It has a pointer instead of a value.
    /// This structure lives as long as the AST generation of the Block
    /// node that contains the variable.
    const LocalPtr = struct {
        const base_tag: Tag = .local_ptr;
        base: Scope = Scope{ .tag = base_tag },
        /// Parents can be: `LocalVal`, `LocalPtr`, `GenZir`, `Defer`, `Namespace`.
        parent: *Scope,
        gen_zir: *GenZir,
        ptr: Zir.Inst.Ref,
        /// Source location of the corresponding variable declaration.
        token_src: Ast.Token.Index,
        /// Track the first identifier where it is referenced.
        /// 0 means never referenced.
        used: Ast.Token.Index = 0,
        /// Track the identifier where it is discarded, like this `_ = foo;`.
        /// 0 means never discarded.
        discarded: Ast.Token.Index = 0,
        /// Whether this value is used as an lvalue after initialization.
        /// If not, we know it can be `const`, so will emit a compile error if it is `var`.
        used_as_lvalue: bool = false,
        /// String table index.
        name: Zir.NullTerminatedString,
        id_cat: IdCat,
        /// true means we find out during Sema whether the value is comptime.
        /// false means it is already known at AstGen the value is runtime-known.
        maybe_comptime: bool,
    };

    /// Represents a global scope that has any number of declarations in it.
    /// Each declaration has this as the parent scope.
    const Namespace = struct {
        const base_tag: Tag = .namespace;
        base: Scope = Scope{ .tag = base_tag },

        /// Parents can be: `LocalVal`, `LocalPtr`, `GenZir`, `Defer`, `Namespace`.
        parent: *Scope,
        /// Maps string table index to the source location of declaration,
        /// for the purposes of reporting name shadowing compile errors.
        decls: std.AutoHashMapUnmanaged(Zir.NullTerminatedString, Ast.Node.Index) = .empty,
        node: Ast.Node.Index,
        inst: Zir.Inst.Index,
        maybe_generic: bool,

        /// The astgen scope containing this namespace.
        /// Only valid during astgen.
        declaring_gz: ?*GenZir,

        /// Set of captures used by this namespace.
        captures: std.AutoArrayHashMapUnmanaged(Zir.Inst.Capture, void) = .empty,

        fn deinit(self: *Namespace, gpa: Allocator) void {
            self.decls.deinit(gpa);
            self.captures.deinit(gpa);
            self.* = undefined;
        }
    };

    const Top = struct {
        const base_tag: Scope.Tag = .top;
        base: Scope = Scope{ .tag = base_tag },
    };
};

/// This is a temporary structure; references to it are valid only
/// while constructing a `Zir`.
const GenZir = struct {
    const base_tag: Scope.Tag = .gen_zir;
    base: Scope = Scope{ .tag = base_tag },
    /// Whether we're already in a scope known to be comptime. This is set
    /// whenever we know Sema will analyze the current block with `is_comptime`,
    /// for instance when we're within a `struct_decl` or a `block_comptime`.
    is_comptime: bool,
    /// Whether we're in an expression within a `@TypeOf` operand. In this case, closure of runtime
    /// variables is permitted where it is usually not.
    is_typeof: bool = false,
    /// This is set to true for a `GenZir` of a `block_inline`, indicating that
    /// exits from this block should use `break_inline` rather than `break`.
    is_inline: bool = false,
    c_import: bool = false,
    /// How decls created in this scope should be named.
    anon_name_strategy: Zir.Inst.NameStrategy = .anon,
    /// The containing decl AST node.
    decl_node_index: Ast.Node.Index,
    /// The containing decl line index, absolute.
    decl_line: u32,
    /// Parents can be: `LocalVal`, `LocalPtr`, `GenZir`, `Defer`, `Namespace`.
    parent: *Scope,
    /// All `GenZir` scopes for the same ZIR share this.
    astgen: *AstGen,
    /// Keeps track of the list of instructions in this scope. Possibly shared.
    /// Indexes to instructions in `astgen`.
    instructions: *ArrayListUnmanaged(Zir.Inst.Index),
    /// A sub-block may share its instructions ArrayList with containing GenZir,
    /// if use is strictly nested. This saves prior size of list for unstacking.
    instructions_top: usize,

    fn reserveInstructionIndex(gz: *GenZir) !Zir.Inst.Index {
        const gpa = gz.astgen.gpa;
        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        try gz.astgen.instructions.ensureUnusedCapacity(gpa, 1);

        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        gz.astgen.instructions.len += 1;
        gz.instructions.appendAssumeCapacity(new_index);
        return new_index;
    }
};

/// This can only be for short-lived references; the memory becomes invalidated
/// when another string is added.
fn nullTerminatedString(astgen: AstGen, index: Zir.NullTerminatedString) [*:0]const u8 {
    return @ptrCast(astgen.string_bytes.items[@intFromEnum(index)..]);
}

/// Detects name conflicts for decls and fields, and populates `namespace.decls` with all named declarations.
/// Returns the number of declarations in the namespace, including unnamed declarations (e.g. `comptime` decls).
fn scanContainer(
    astgen: *AstGen,
    namespace: *Scope.Namespace,
    members: []const Ast.Node.Index,
    container_kind: enum { @"struct", @"union", @"enum", @"opaque" },
) !u32 {
    const gpa = astgen.gpa;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens = tree.nodes.items(.main_token);
    const datas: []Ast.Node.Data = tree.nodes.items(.data);

    // This type forms a linked list of source tokens declaring the same name.
    const NameEntry = struct {
        tok: Ast.Token.Index,
        /// Using a linked list here simplifies memory management, and is acceptable since
        ///ewntries are only allocated in error situations. The entries are allocated into the
        /// AstGen arena.
        next: ?*@This(),
    };

    // The maps below are allocated into this SFBA to avoid using the GPA for small namespaces.
    var sfba_state = std.heap.stackFallback(512, astgen.gpa);
    const sfba = sfba_state.get();

    var names: std.AutoArrayHashMapUnmanaged(Zir.NullTerminatedString, NameEntry) = .empty;
    defer names.deinit(sfba);

    var any_duplicates = false;
    var decl_count: u32 = 0;
    for (members) |member_node| {
        const name_token = switch (node_tags[member_node]) {
            .assign => blk: {
                decl_count += 1;
                break :blk main_tokens[datas[member_node].lhs];
            },

            else => unreachable,
        };

        const name_str_index = try astgen.identAsString(name_token);

        // Put the name straight into `decls`, even if there are compile errors.
        // This avoids incorrect "undeclared identifier" errors later on.
        try namespace.decls.put(gpa, name_str_index, member_node);

        {
            const gop = try names.getOrPut(sfba, name_str_index);
            const new_ent: NameEntry = .{
                .tok = name_token,
                .next = null,
            };
            if (gop.found_existing) {
                var e = gop.value_ptr;
                while (e.next) |n| e = n;
                e.next = try astgen.arena.create(NameEntry);
                e.next.?.* = new_ent;
                any_duplicates = true;
                continue;
            } else {
                gop.value_ptr.* = new_ent;
            }
        }

        const token_bytes = astgen.tree.tokenSlice(name_token);

        var s = namespace.parent;
        while (true) switch (s.tag) {
            .local_val => {
                const local_val = s.cast(Scope.LocalVal).?;
                if (local_val.name == name_str_index) {
                    try astgen.appendErrorTokNotes(name_token, "declaration '{s}' shadows {s} from outer scope", .{
                        token_bytes, @tagName(local_val.id_cat),
                    }, &.{
                        try astgen.errNoteTok(
                            local_val.token_src,
                            "previous declaration here",
                            .{},
                        ),
                    });
                    break;
                }
                s = local_val.parent;
            },
            .local_ptr => {
                const local_ptr = s.cast(Scope.LocalPtr).?;
                if (local_ptr.name == name_str_index) {
                    try astgen.appendErrorTokNotes(name_token, "declaration '{s}' shadows {s} from outer scope", .{
                        token_bytes, @tagName(local_ptr.id_cat),
                    }, &.{
                        try astgen.errNoteTok(
                            local_ptr.token_src,
                            "previous declaration here",
                            .{},
                        ),
                    });
                    break;
                }
                s = local_ptr.parent;
            },
            .namespace => s = s.cast(Scope.Namespace).?.parent,
            .gen_zir => s = s.cast(GenZir).?.parent,
            .top => break,
        };
    }

    if (!any_duplicates) return decl_count;

    for (names.keys(), names.values()) |name, first| {
        if (first.next == null) continue;
        var notes: std.ArrayListUnmanaged(u32) = .empty;
        var prev: NameEntry = first;
        while (prev.next) |cur| : (prev = cur.*) {
            try notes.append(astgen.arena, try astgen.errNoteTok(cur.tok, "duplicate name here", .{}));
        }
        try notes.append(astgen.arena, try astgen.errNoteNode(namespace.node, "{s} declared here", .{@tagName(container_kind)}));
        const name_duped = try astgen.arena.dupe(u8, std.mem.span(astgen.nullTerminatedString(name)));
        try astgen.appendErrorTokNotes(first.tok, "duplicate {s} member name '{s}'", .{ @tagName(container_kind), name_duped }, notes.items);
    }

    return decl_count;
}

fn lowerAstErrors(astgen: *AstGen) !void {
    const tree = astgen.tree;
    assert(tree.errors.len > 0);

    const gpa = astgen.gpa;
    const parse_err = tree.errors[0];

    var msg: std.ArrayListUnmanaged(u8) = .empty;
    defer msg.deinit(gpa);

    var notes: std.ArrayListUnmanaged(u32) = .empty;
    defer notes.deinit(gpa);

    for (tree.errors[1..]) |note| {
        if (!note.is_note) break;

        msg.clearRetainingCapacity();
        try tree.renderError(note, msg.writer(gpa));
        try notes.append(gpa, try astgen.errNoteTok(note.token, "{s}", .{msg.items}));
    }

    const extra_offset = tree.errorOffset(parse_err);
    msg.clearRetainingCapacity();
    try tree.renderError(parse_err, msg.writer(gpa));
    try astgen.appendErrorTokNotesOff(parse_err.token, extra_offset, "{s}", .{msg.items}, notes.items);
}
