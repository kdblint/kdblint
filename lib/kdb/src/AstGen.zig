const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const StringIndexAdapter = std.hash_map.StringIndexAdapter;
const StringIndexContext = std.hash_map.StringIndexContext;

const kdb = @import("root.zig");
const Ast = kdb.Ast;
const Zir = kdb.Zir;

const AstGen = @This();

gpa: Allocator,
arena: Allocator,
tree: *const Ast,
instructions: std.MultiArrayList(Zir.Inst) = .empty,
extra: std.ArrayListUnmanaged(u32) = .empty,
string_bytes: std.ArrayListUnmanaged(u8) = .empty,
/// Tracks the current byte offset within the source file.
/// Used to populate line deltas in the ZIR. AstGen maintains
/// this "cursor" throughout the entire AST lowering process in order
/// to avoid starting over the line/column scan for every declaration, which
/// would be O(N^2).
source_offset: u32 = 0,
/// Tracks the corresponding line of `source_offset`.
/// This value is absolute.
source_line: u32 = 0,
/// Tracks the corresponding column of `source_offset`.
/// This value is absolute.
source_column: u32 = 0,
string_table: std.HashMapUnmanaged(
    u32,
    void,
    StringIndexContext,
    std.hash_map.default_max_load_percentage,
) = .empty,
compile_errors: std.ArrayListUnmanaged(Zir.Inst.CompileErrors.Item) = .empty,
within_fn: bool = false,
/// Maps string table indexes to the first `@import` ZIR instruction
/// that uses this string as the operand.
imports: std.AutoArrayHashMapUnmanaged(Zir.NullTerminatedString, Ast.Token.Index) = .empty,
/// Used for temporary storage when building payloads.
scratch: std.ArrayListUnmanaged(u32) = .empty,
/// Whenever a `ref` instruction is needed, it is created and saved in this
/// table instead of being immediately appended to the current block body.
/// Then, when the instruction is being added to the parent block (typically from
/// setBlockBody), if it has a ref_table entry, then the ref instruction is added
/// there. This makes sure two properties are upheld:
/// 1. All pointers to the same locals return the same address. This is required
///    to be compliant with the language specification.
/// 2. `ref` instructions will dominate their uses. This is a required property
///    of ZIR.
/// The key is the ref operand; the value is the ref instruction.
ref_table: std.AutoHashMapUnmanaged(Zir.Inst.Index, Zir.Inst.Index) = .empty,

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

            Zir.Inst.Ref,
            Zir.Inst.Index,
            Zir.NullTerminatedString,
            => @intFromEnum(@field(extra, field.name)),

            i32,
            => @bitCast(@field(extra, field.name)),

            else => |t| @compileError("bad field type: " ++ @typeName(t)),
        };
        i += 1;
    }
}

pub fn generate(gpa: Allocator, tree: Ast) !Zir {
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
    defer gz_instructions.deinit(gpa);
    var gz: GenZir = .{
        .decl_node_index = 0,
        .decl_line = 0,
        .parent = &top_scope.base,
        .astgen = &astgen,
        .instructions = &gz_instructions,
        .instructions_top = 0,
    };

    // The AST -> ZIR lowering process assumes an AST that does not have any
    // parse errors.
    if (tree.errors.len == 0) {
        if (file(&gz)) |file_inst| {
            assert(file_inst.toIndex().? == .file_inst);
        } else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.AnalysisFail => {}, // Handled via compile_errors below.
        }
    } else {
        try astgen.lowerAstErrors();
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

    const imports_index = @intFromEnum(Zir.ExtraIndex.imports);
    if (astgen.imports.count() == 0) {
        astgen.extra.items[imports_index] = 0;
    } else {
        try astgen.extra.ensureUnusedCapacity(gpa, @typeInfo(Zir.Inst.Imports).@"struct".fields.len +
            astgen.imports.count() * @typeInfo(Zir.Inst.Imports.Item).@"struct".fields.len);

        astgen.extra.items[imports_index] = astgen.addExtraAssumeCapacity(Zir.Inst.Imports{
            .imports_len = @intCast(astgen.imports.count()),
        });

        var it = astgen.imports.iterator();
        while (it.next()) |entry| {
            _ = astgen.addExtraAssumeCapacity(Zir.Inst.Imports.Item{
                .name = entry.key_ptr.*,
                .token = entry.value_ptr.*,
            });
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
    astgen.string_table.deinit(gpa);
    astgen.string_bytes.deinit(gpa);
    astgen.compile_errors.deinit(gpa);
    astgen.imports.deinit(gpa);
    astgen.scratch.deinit(gpa);
    astgen.ref_table.deinit(gpa);
}

fn file(gz: *GenZir) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;

    const file_inst = try gz.makeFile();
    for (tree.getBlocks()) |node| {
        // TODO: Something should wrap expr to return an index to show or discard value.
        _ = try expr(gz, node);
    }
    try gz.setFile(file_inst);

    return file_inst.toRef();
}

fn expr(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);

    switch (node_tags[node]) {
        .root, // TODO: Test unreserved node.
        .empty,
        => unreachable,

        .lambda,
        .lambda_semicolon,
        => return lambda(gz, node),

        .assign => return assign(gz, node),
        .global_assign => return globalAssign(gz, node),

        .call => return call(gz, node),

        .apply_binary => return applyBinary(gz, node),

        .number_literal => return numberLiteral(gz, node),

        .identifier => return identifier(gz, node),

        inline else => |t| @panic(@tagName(t)),
    }

    unreachable;
}

fn lambda(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    astgen.advanceSourceCursorToNode(node);
    const lbrace_line = astgen.source_line - gz.decl_line;
    const lbrace_column = astgen.source_column;

    const lambda_inst = try gz.makeLambda(node);

    const full_lambda = tree.fullLambda(node);

    const prev_within_fn = astgen.within_fn;
    defer astgen.within_fn = prev_within_fn;
    astgen.within_fn = true;

    // Traverse body before params in the case that we have implicit params.
    var body_gz: GenZir = .{
        .decl_node_index = node,
        .decl_line = astgen.source_line,
        .parent = &gz.base,
        .astgen = astgen,
        .instructions = gz.instructions,
        .instructions_top = gz.instructions.items.len,
    };
    defer body_gz.unstack();

    for (full_lambda.body, 0..) |body_node, i| {
        if (i < full_lambda.body.len - 1) {
            _ = try expr(&body_gz, body_node);
        } else if (node_tags[body_node] == .empty) {
            _ = try body_gz.addUnTok(.ret_implicit, .null, full_lambda.r_brace);
        } else {
            const ref = try expr(&body_gz, body_node);
            if (node_tags[node] == .lambda) {
                _ = try body_gz.addUnNode(.ret_node, ref, body_node);
            } else {
                _ = try body_gz.addUnTok(.ret_implicit, .null, full_lambda.r_brace);
            }
        }
    }

    var params_gz: GenZir = .{
        .decl_node_index = node,
        .decl_line = astgen.source_line,
        .parent = &gz.base,
        .astgen = astgen,
        .instructions = gz.instructions,
        .instructions_top = gz.instructions.items.len,
    };
    defer params_gz.unstack();

    if (full_lambda.params) |p| {
        if (p.params.len > 8) return astgen.failNode(p.params[8], "too many parameters (8 max)", .{});

        var hash_map: std.AutoHashMapUnmanaged(Zir.NullTerminatedString, void) = .empty;
        defer hash_map.deinit(astgen.gpa);
        for (p.params) |param_node| {
            if (node_tags[param_node] == .identifier) {
                const param_name = try astgen.identAsString(main_tokens[param_node]);
                const gop = try hash_map.getOrPut(astgen.gpa, param_name);
                if (gop.found_existing) {
                    return astgen.failNode(
                        param_node,
                        "redeclaration of function parameter '{s}'",
                        .{tree.tokenSlice(main_tokens[param_node])},
                    );
                }

                _ = try params_gz.addStrNode(.param_node, param_name, param_node);
            } else {
                assert(node_tags[param_node] == .empty);
                assert(p.params.len == 1);
            }
        }
    } else {
        var found_y = false;
        var found_z = false;
        const zir_tags: []Zir.Inst.Tag = astgen.instructions.items(.tag);
        for (body_gz.instructionsSlice()) |inst| {
            switch (zir_tags[@intFromEnum(inst)]) {
                .identifier => {
                    const zir_datas: []Zir.Inst.Data = astgen.instructions.items(.data);
                    const index = zir_datas[@intFromEnum(inst)].str_tok.start;
                    const slice = astgen.string_bytes.items[@intFromEnum(index)..];
                    const str = slice[0..std.mem.indexOfScalar(u8, slice, 0).? :0];
                    if (std.mem.eql(u8, str, "z")) {
                        found_z = true;
                        break;
                    } else if (std.mem.eql(u8, str, "y")) {
                        found_y = true;
                    }
                },
                else => {},
            }
        }

        _ = try params_gz.addUnTok(.param_implicit, .x, full_lambda.l_brace);
        if (found_z) {
            _ = try params_gz.addUnTok(.param_implicit, .y, full_lambda.l_brace);
            _ = try params_gz.addUnTok(.param_implicit, .z, full_lambda.l_brace);
        } else if (found_y) {
            _ = try params_gz.addUnTok(.param_implicit, .y, full_lambda.l_brace);
        }
    }

    try astgen.validateLambda(params_gz.instructionsSlice(), body_gz.instructionsSlice());

    try gz.setLambda(lambda_inst, .{
        .lbrace_line = lbrace_line,
        .lbrace_column = lbrace_column,
        .rbrace = full_lambda.r_brace,
        .params_gz = &params_gz,
        .body_gz = &body_gz,
    });

    return lambda_inst.toRef();
}

fn assign(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);

    const data = node_datas[node];
    switch (node_tags[data.lhs]) {
        .identifier => {},
        // TODO: a[1]:1
        else => return astgen.failNode(data.lhs, "invalid left-hand side to assignment", .{}),
    }
    const rhs = try expr(gz, data.rhs);
    const lhs = try expr(gz, data.lhs);

    return gz.addPlNode(.assign, node, Zir.Inst.Bin{ .lhs = lhs, .rhs = rhs });
}

fn globalAssign(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);

    const data = node_datas[node];
    const rhs = try expr(gz, data.rhs);
    const lhs = try expr(gz, data.lhs);

    return gz.addPlNode(.global_assign, node, Zir.Inst.Bin{ .lhs = lhs, .rhs = rhs });
}

fn call(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;

    const full_call = tree.fullCall(node);
    _ = full_call; // autofix

    const call_inst = try gz.makePlNode(.call, node);
    return call_inst.toRef();
}

fn applyBinary(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const node_datas: []Ast.Node.Data = tree.nodes.items(.data);
    const node_tags: []Ast.Node.Tag = tree.nodes.items(.tag);
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    const data = node_datas[node];
    const rhs = try expr(gz, data.rhs);
    const lhs = try expr(gz, data.lhs);
    const op: Ast.Node.Index = main_tokens[node];
    const tag: Zir.Inst.Tag = switch (node_tags[op]) {
        .plus => .add,
        .asterisk => .multiply,
        else => |t| @panic(@tagName(t)),
    };

    return gz.addPlNode(tag, node, Zir.Inst.Bin{ .lhs = lhs, .rhs = rhs });
}

fn numberLiteral(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const main_tokens = tree.nodes.items(.main_token);
    const num_token = main_tokens[node];
    const bytes = tree.tokenSlice(num_token);

    const result: Zir.Inst.Ref = switch (bytes[0]) {
        '-' => switch (kdb.parseNumberLiteral(bytes[1..])) {
            .long => |num| switch (num) {
                1 => .negative_one,
                else => try gz.addLong(-num),
            },
            .failure => unreachable,
        },
        else => switch (kdb.parseNumberLiteral(bytes)) {
            .long => |num| switch (num) {
                0 => .zero,
                1 => .one,
                else => try gz.addLong(num),
            },
            .failure => unreachable,
        },
    };

    return result;
}

fn identifier(gz: *GenZir, node: Ast.Node.Index) InnerError!Zir.Inst.Ref {
    const astgen = gz.astgen;
    const tree = astgen.tree;
    const main_tokens: []Ast.Token.Index = tree.nodes.items(.main_token);

    const ident_token = main_tokens[node];
    const name_str_index = try astgen.identAsString(ident_token);

    return gz.addStrTok(.identifier, name_str_index, ident_token);
}

fn validateLambda(astgen: *AstGen, params: []const Zir.Inst.Index, body: []const Zir.Inst.Index) !void {
    const gpa = astgen.gpa;
    const zir_tags: []Zir.Inst.Tag = astgen.instructions.items(.tag);
    const zir_datas: []Zir.Inst.Data = astgen.instructions.items(.data);
    var locals: std.AutoArrayHashMapUnmanaged(Zir.NullTerminatedString, void) = .empty;
    defer locals.deinit(gpa);

    var globals: std.AutoArrayHashMapUnmanaged(Zir.NullTerminatedString, void) = .empty;
    defer globals.deinit(gpa);

    try locals.ensureUnusedCapacity(gpa, params.len);
    for (params) |param| {
        const param_name: Zir.NullTerminatedString = switch (zir_tags[@intFromEnum(param)]) {
            .param_node => zir_datas[@intFromEnum(param)].str_node.start,
            .param_implicit => unreachable, // TODO
            else => unreachable,
        };
        locals.putAssumeCapacityNoClobber(param_name, {});
    }

    for (body) |inst| {
        switch (zir_tags[@intFromEnum(inst)]) {
            .identifier => {
                const ident_name = zir_datas[@intFromEnum(inst)].str_tok.start;
                if (locals.get(ident_name)) |_| {
                    std.log.debug("local", .{});
                } else {
                    std.log.debug("global", .{});
                }
            },
            .assign => {},
            else => {},
        }
    }
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

const Scope = struct {
    tag: Tag,

    fn cast(base: *Scope, comptime T: type) ?*T {
        if (base.tag != T.base_tag) return null;
        return @alignCast(@fieldParentPtr("base", base));
    }

    const Tag = enum {
        gen_zir,
        top,
    };

    const Top = struct {
        const base_tag: Scope.Tag = .top;
        base: Scope = .{ .tag = base_tag },
    };
};

const GenZir = struct {
    const base_tag: Scope.Tag = .gen_zir;
    base: Scope = .{ .tag = base_tag },
    /// The containing decl AST node.
    decl_node_index: Ast.Node.Index,
    /// The containing decl line index, absolute.
    decl_line: u32,
    /// TODO: Parents can be: `LocalVal`, `LocalPtr`, `GenZir`, `Namespace`.
    parent: *Scope,
    /// All `GenZir` scopes for the same ZIR share this.
    astgen: *AstGen,
    /// Keeps track of the list of instructions in this scope. Possibly shared.
    /// Indexes to instructions in `astgen`.
    instructions: *std.ArrayListUnmanaged(Zir.Inst.Index),
    /// A sub-block may share its instructions ArrayList with containing GenZir,
    /// if use is strictly nested. This saves prior size of list for unstacking.
    instructions_top: usize,

    const unstacked_top = std.math.maxInt(usize);
    /// Call unstack before adding any new instructions to containing GenZir.
    fn unstack(self: *GenZir) void {
        if (self.instructions_top != unstacked_top) {
            self.instructions.items.len = self.instructions_top;
            self.instructions_top = unstacked_top;
        }
    }

    fn makeSubBlock(gz: *GenZir, scope: *Scope) GenZir {
        return .{
            .decl_node_index = gz.decl_node_index,
            .decl_line = gz.decl_line,
            .parent = scope,
            .astgen = gz.astgen,
            .instructions = gz.instructions,
            .instructions_top = gz.instructions.items.len,
        };
    }

    fn instructionsSlice(self: *const GenZir) []Zir.Inst.Index {
        return if (self.instructions_top == unstacked_top)
            &[0]Zir.Inst.Index{}
        else
            self.instructions.items[self.instructions_top..];
    }

    fn nodeIndexToRelative(gz: GenZir, node_index: Ast.Node.Index) i32 {
        return @as(i32, @bitCast(node_index)) - @as(i32, @bitCast(gz.decl_node_index));
    }

    fn tokenIndexToRelative(gz: GenZir, token: Ast.Token.Index) u32 {
        return token - gz.srcToken();
    }

    fn srcToken(gz: GenZir) Ast.Token.Index {
        return gz.astgen.tree.firstToken(gz.decl_node_index);
    }

    fn makeFile(gz: *GenZir) !Zir.Inst.Index {
        return gz.makePlNode(.file, 0);
    }

    /// Assumes nothing stacked on `gz`. Unstacks `gz`.
    fn setFile(gz: *GenZir, inst: Zir.Inst.Index) !void {
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        const body = gz.instructionsSlice();
        const body_len = astgen.countBodyLenAfterFixups(body);
        try astgen.extra.ensureUnusedCapacity(
            gpa,
            @typeInfo(Zir.Inst.Block).@"struct".fields.len + body_len,
        );
        const zir_datas: []Zir.Inst.Data = astgen.instructions.items(.data);
        zir_datas[@intFromEnum(inst)].pl_node.payload_index = astgen.addExtraAssumeCapacity(
            Zir.Inst.Block{ .body_len = body_len },
        );
        astgen.appendBodyWithFixups(body);
        gz.unstack();
    }

    fn makeLambda(gz: *GenZir, node: Ast.Node.Index) !Zir.Inst.Index {
        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        try gz.astgen.instructions.append(gz.astgen.gpa, .{
            .tag = .lambda,
            .data = .{ .lambda = .{
                .src_node = node,
                .payload_index = undefined,
            } },
        });
        return new_index;
    }

    /// Must be called with the following stack set up:
    ///  * gz (bottom)
    ///  * body_gz
    ///  * params_gz (top)
    /// Unstacks all of those except for `gz`.
    fn setLambda(gz: *GenZir, inst: Zir.Inst.Index, args: struct {
        lbrace_line: u32,
        lbrace_column: u32,
        rbrace: Ast.Token.Index,
        params_gz: *GenZir,
        body_gz: *GenZir,
    }) !void {
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        const tree = astgen.tree;
        const token_locs: []Ast.Token.Loc = tree.tokens.items(.loc);
        const zir_datas: []Zir.Inst.Data = astgen.instructions.items(.data);

        const rbrace_loc = token_locs[args.rbrace];
        astgen.advanceSourceCursor(rbrace_loc.start);
        const rbrace_line = astgen.source_line - gz.decl_line;
        const rbrace_column = astgen.source_column;

        const columns = args.lbrace_column | (rbrace_column << 16);

        const src_locs: Zir.Inst.Lambda.SrcLocs = .{
            .lbrace_line = args.lbrace_line,
            .rbrace_line = rbrace_line,
            .columns = columns,
        };

        const params = args.params_gz.instructionsSlice();
        const params_len = astgen.countBodyLenAfterFixups(params);
        args.params_gz.unstack();

        const body = args.body_gz.instructionsSlice();
        const body_len = astgen.countBodyLenAfterFixups(body);
        args.body_gz.unstack();

        try astgen.extra.ensureUnusedCapacity(
            gpa,
            @typeInfo(Zir.Inst.Lambda).@"struct".fields.len + params_len + body_len +
                @typeInfo(Zir.Inst.Lambda.SrcLocs).@"struct".fields.len,
        );

        zir_datas[@intFromEnum(inst)].lambda.payload_index = astgen.addExtraAssumeCapacity(Zir.Inst.Lambda{
            .params_len = params_len,
            .body_len = body_len,
        });
        astgen.appendBodyWithFixups(params);
        astgen.appendBodyWithFixups(body);
        _ = astgen.addExtraAssumeCapacity(src_locs);

        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        gz.instructions.appendAssumeCapacity(inst);
    }

    fn addStrNode(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        str_index: Zir.NullTerminatedString,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = tag,
            .data = .{ .str_node = .{
                .start = str_index,
                .src_node = gz.nodeIndexToRelative(src_node),
            } },
        });
    }

    fn addStrTok(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        str_index: Zir.NullTerminatedString,
        /// Absolute token index. This function does the conversion to Decl offset.
        abs_tok_index: Ast.Token.Index,
    ) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = tag,
            .data = .{ .str_tok = .{
                .start = str_index,
                .src_tok = gz.tokenIndexToRelative(abs_tok_index),
            } },
        });
    }

    fn addLong(gz: *GenZir, long: i64) !Zir.Inst.Ref {
        return gz.add(.{
            .tag = .long,
            .data = .{ .long = long },
        });
    }

    fn makePlNode(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Zir.Inst.Index {
        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        try gz.astgen.instructions.append(gz.astgen.gpa, .{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = gz.nodeIndexToRelative(src_node),
                .payload_index = undefined,
            } },
        });
        return new_index;
    }

    fn addPlNode(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
        extra: anytype,
    ) !Zir.Inst.Ref {
        const gpa = gz.astgen.gpa;
        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        try gz.astgen.instructions.ensureUnusedCapacity(gpa, 1);

        const payload_index = try gz.astgen.addExtra(extra);
        const new_index: Zir.Inst.Index = @enumFromInt(gz.astgen.instructions.len);
        gz.astgen.instructions.appendAssumeCapacity(.{
            .tag = tag,
            .data = .{ .pl_node = .{
                .src_node = gz.nodeIndexToRelative(src_node),
                .payload_index = payload_index,
            } },
        });
        gz.instructions.appendAssumeCapacity(new_index);
        return new_index.toRef();
    }

    fn addUnNode(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        operand: Zir.Inst.Ref,
        /// Absolute node index. This function does the conversion to offset from Decl.
        src_node: Ast.Node.Index,
    ) !Zir.Inst.Ref {
        assert(operand != .none);
        return gz.add(.{
            .tag = tag,
            .data = .{ .un_node = .{
                .operand = operand,
                .src_node = gz.nodeIndexToRelative(src_node),
            } },
        });
    }

    fn addUnTok(
        gz: *GenZir,
        tag: Zir.Inst.Tag,
        operand: Zir.Inst.Ref,
        /// Absolute token index. This function does the conversion to Decl offset.
        abs_tok_index: Ast.Token.Index,
    ) !Zir.Inst.Ref {
        assert(operand != .none);
        return gz.add(.{
            .tag = tag,
            .data = .{ .un_tok = .{
                .operand = operand,
                .src_tok = gz.tokenIndexToRelative(abs_tok_index),
            } },
        });
    }

    fn add(gz: *GenZir, inst: Zir.Inst) !Zir.Inst.Ref {
        return (try gz.addAsIndex(inst)).toRef();
    }

    fn addAsIndex(gz: *GenZir, inst: Zir.Inst) !Zir.Inst.Index {
        const astgen = gz.astgen;
        const gpa = astgen.gpa;
        try gz.instructions.ensureUnusedCapacity(gpa, 1);
        try astgen.instructions.ensureUnusedCapacity(gpa, 1);

        const new_index: Zir.Inst.Index = @enumFromInt(astgen.instructions.len);
        astgen.instructions.appendAssumeCapacity(inst);
        gz.instructions.appendAssumeCapacity(new_index);

        return new_index;
    }
};

/// Advances the source cursor to the beginning of `node`.
fn advanceSourceCursorToNode(astgen: *AstGen, node: Ast.Node.Index) void {
    const tree = astgen.tree;
    const token_locs: []Ast.Token.Loc = tree.tokens.items(.loc);
    const node_start = token_locs[tree.firstToken(node)].start;
    astgen.advanceSourceCursor(node_start);
}

/// Advances the source cursor to an absolute byte offset `end` in the file.
fn advanceSourceCursor(astgen: *AstGen, end: usize) void {
    const source = astgen.tree.source;
    var i = astgen.source_offset;
    var line = astgen.source_line;
    var column = astgen.source_column;
    assert(i <= end);
    while (i < end) : (i += 1) {
        if (source[i] == '\n') {
            line += 1;
            column = 0;
        } else {
            column += 1;
        }
    }
    astgen.source_offset = i;
    astgen.source_line = line;
    astgen.source_column = column;
}

/// Assumes capacity for body has already been added. Needed capacity taking into
/// account fixups can be found with `countBodyLenAfterFixups`.
fn appendBodyWithFixups(astgen: *AstGen, body: []const Zir.Inst.Index) void {
    return appendBodyWithFixupsArrayList(astgen, &astgen.extra, body);
}

fn appendBodyWithFixupsArrayList(
    astgen: *AstGen,
    list: *std.ArrayListUnmanaged(u32),
    body: []const Zir.Inst.Index,
) void {
    for (body) |body_inst| {
        appendPossiblyRefdBodyInst(astgen, list, body_inst);
    }
}

fn appendPossiblyRefdBodyInst(
    astgen: *AstGen,
    list: *std.ArrayListUnmanaged(u32),
    body_inst: Zir.Inst.Index,
) void {
    list.appendAssumeCapacity(@intFromEnum(body_inst));
    const kv = astgen.ref_table.fetchRemove(body_inst) orelse return;
    const ref_inst = kv.value;
    return appendPossiblyRefdBodyInst(astgen, list, ref_inst);
}

fn countBodyLenAfterFixups(astgen: *AstGen, body: []const Zir.Inst.Index) u32 {
    var count = body.len;
    for (body) |body_inst| {
        var check_inst = body_inst;
        while (astgen.ref_table.get(check_inst)) |ref_inst| {
            count += 1;
            check_inst = ref_inst;
        }
    }
    return @intCast(count);
}

fn failNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
) InnerError {
    return astgen.failNodeNotes(node, format, args, &[0]u32{});
}

fn appendErrorNode(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
) Allocator.Error!void {
    try astgen.appendErrorNodeNotes(node, format, args, &[0]u32{});
}

fn appendErrorNodeNotes(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) Allocator.Error!void {
    @branchHint(.cold);
    const string_bytes = &astgen.string_bytes;
    const msg: Zir.NullTerminatedString = @enumFromInt(string_bytes.items.len);
    try string_bytes.writer(astgen.gpa).print(format ++ "\x00", args);
    const notes_index: u32 = if (notes.len != 0) blk: {
        const notes_start = astgen.extra.items.len;
        try astgen.extra.ensureTotalCapacity(astgen.gpa, notes_start + 1 + notes.len);
        astgen.extra.appendAssumeCapacity(@intCast(notes.len));
        astgen.extra.appendSliceAssumeCapacity(notes);
        break :blk @intCast(notes_start);
    } else 0;
    try astgen.compile_errors.append(astgen.gpa, .{
        .msg = msg,
        .node = node,
        .token = 0,
        .byte_offset = 0,
        .notes = notes_index,
    });
}

fn failNodeNotes(
    astgen: *AstGen,
    node: Ast.Node.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) InnerError {
    try appendErrorNodeNotes(astgen, node, format, args, notes);
    return error.AnalysisFail;
}

fn failTok(
    astgen: *AstGen,
    token: Ast.TokenIndex,
    comptime format: []const u8,
    args: anytype,
) InnerError {
    return astgen.failTokNotes(token, format, args, &[0]u32{});
}

fn appendErrorTok(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
) !void {
    try astgen.appendErrorTokNotesOff(token, 0, format, args, &[0]u32{});
}

fn failTokNotes(
    astgen: *AstGen,
    token: Ast.Token.Index,
    comptime format: []const u8,
    args: anytype,
    notes: []const u32,
) InnerError {
    try appendErrorTokNotesOff(astgen, token, 0, format, args, notes);
    return error.AnalysisFail;
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

/// Same as `fail`, except given a token plus an offset from its starting byte
/// offset.
fn failOff(
    astgen: *AstGen,
    token: Ast.Token.Index,
    byte_offset: u32,
    comptime format: []const u8,
    args: anytype,
) InnerError {
    try appendErrorTokNotesOff(astgen, token, byte_offset, format, args, &.{});
    return error.AnalysisFail;
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
