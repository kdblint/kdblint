const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const lsp = @import("lsp");
const types = lsp.types;
const offsets = lsp.offsets;

const kdb = @import("../../kdb/root.zig");
const Ast = kdb.Ast;
const DocumentScope = kdb.DocumentScope;
const DocumentStore = @import("../DocumentStore.zig");
const goto = @import("goto.zig");
const hover = @import("hover.zig");

/// Only standard LSP token types are used so that clients theme them without
/// any custom scope mapping.
pub const TokenType = enum(u32) {
    /// A lambda parameter, explicit (`{[a]..}`) or implicit (`x`/`y`/`z`).
    parameter,
    /// A local or (with the `static` modifier) global variable.
    variable,
    /// A column name in a q-sql clause or table literal.
    property,
    /// A symbol literal.
    enumMember,
    /// A name assigned a lambda, or (with `defaultLibrary`) a builtin.
    function,
    /// A system command (`\l x.q`) or DSL marker (`k)`).
    macro,
    keyword,
    string,
    number,
    operator,
};

pub const TokenModifier = enum(u5) {
    /// The first assignment of a name, or an explicit parameter.
    declaration,
    /// A later re-assignment of an already declared name.
    modification,
    /// A global, i.e. a name declared in a file's root scope or undeclared.
    static,
    /// A builtin function or a name in one of q's own namespaces.
    defaultLibrary,
};

const Class = struct {
    type: TokenType,
    modifiers: u32 = 0,

    fn with(class: Class, modifier: TokenModifier) Class {
        return .{ .type = class.type, .modifiers = class.modifiers | (@as(u32, 1) << @backingInt(modifier)) };
    }
};

/// Namespaces owned by q itself; undeclared names under them are library
/// references rather than user globals.
const library_namespaces = [_][]const u8{ ".z.", ".Q.", ".q.", ".h.", ".j.", ".m.", ".o." };

const control_words = [_][]const u8{ "if", "do", "while" };

pub fn writeSemanticTokens(
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    document_store: *DocumentStore,
    handle: *DocumentStore.Handle,
    encoding: offsets.Encoding,
    overlapping_token_support: bool,
) !types.semantic_tokens.Result {
    const tree = handle.tree;
    const doc_scope = try handle.getDocumentScope(io, gpa);

    var builder: Builder = .{
        .arena = arena,
        .io = io,
        .gpa = gpa,
        .handle = handle,
        .other_handles = try document_store.collectHandles(arena),
        .tree = tree,
        .doc_scope = doc_scope,
        .encoding = encoding,
        .overlapping_token_support = overlapping_token_support,
        .overrides = try arena.alloc(?Override, tree.tokens.len),
        .in_column_context = try arena.alloc(bool, tree.tokens.len),
    };
    @memset(builder.overrides, null);
    @memset(builder.in_column_context, false);

    try builder.indexAssignments();
    try builder.collectOverrides();
    try builder.emitTokens();

    return .{ .data = try builder.token_buffer.toOwnedSlice(arena) };
}

const Override = union(enum) {
    class: Class,
    /// Emit nothing for this token (e.g. the `,` separating q-sql columns).
    skip,
};

const AssignKind = enum { plain, compound };

/// Target identifier node of every plain assignment -> the assigned value.
const AssignValues = std.AutoHashMapUnmanaged(Ast.Node.Index, Ast.Node.Index);

/// Everything needed to follow assignments within one document.
const FileContext = struct {
    handle: *DocumentStore.Handle,
    tree: Ast,
    doc_scope: *const DocumentScope,
    assign_values: *const AssignValues,
};

/// Identifies a declaration chain across documents.
const ChainKey = struct {
    handle: *DocumentStore.Handle,
    first: DocumentScope.Declaration.Index,
};

const Builder = struct {
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    handle: *DocumentStore.Handle,
    /// Every document in the store, including `handle`. Names undeclared in
    /// the current file are looked up in the root scope of each of these.
    other_handles: []*DocumentStore.Handle,
    tree: Ast,
    doc_scope: *const DocumentScope,
    encoding: offsets.Encoding,
    overlapping_token_support: bool,
    /// Per-token classification decided from AST context, taking precedence
    /// over the token's tag and name resolution.
    overrides: []?Override,
    /// Tokens inside a q-sql select/by/where clause, where an undeclared
    /// name is most likely a column.
    in_column_context: []bool,
    /// Name token of every assignment target.
    assign_targets: std.AutoHashMapUnmanaged(Ast.TokenIndex, AssignKind) = .empty,
    assign_values: AssignValues = .empty,
    /// Per-name result of `workspaceDeclaresFunction` for the current
    /// file's own references; a global is typically referenced many times.
    workspace_lambda_cache: std.StringHashMapUnmanaged(bool) = .empty,
    /// Lazily built `AssignValues` of each other document consulted.
    other_assign_values: std.AutoHashMapUnmanaged(*DocumentStore.Handle, AssignValues) = .empty,
    /// Declaration chains whose `chainIsFunction` evaluation is underway.
    /// Re-entering one means the aliases form a cycle (`a:b;b:a`).
    chains_in_progress: std.AutoHashMapUnmanaged(ChainKey, void) = .empty,
    token_buffer: std.ArrayList(u32) = .empty,
    previous_source_index: usize = 0,
    source_index: usize = 0,

    fn indexAssignments(b: *Builder) Allocator.Error!void {
        const tree = b.tree;
        try indexAssignValues(b.arena, tree, &b.assign_values);
        for (0..tree.nodes.len) |i| {
            const node: Ast.Node.Index = @fromBackingInt(@intCast(i));
            if (hover.assignmentAt(tree, node)) |assignment| {
                try b.assign_targets.put(b.arena, tree.nodeMainToken(assignment.target), .plain);
            } else if (tree.isCompoundAssignment(node)) {
                const lhs = tree.unwrapGroupedExpr(tree.nodeData(node).node_and_opt_node[0]);
                if (tree.nodeTag(lhs) != .identifier) continue;
                try b.assign_targets.put(b.arena, tree.nodeMainToken(lhs), .compound);
            }
        }
    }

    fn collectOverrides(b: *Builder) Allocator.Error!void {
        const tree = b.tree;
        for (0..tree.nodes.len) |i| {
            const node: Ast.Node.Index = @fromBackingInt(@intCast(i));
            switch (tree.nodeTag(node)) {
                .table_literal => {
                    const table = tree.fullTable(node);
                    for (table.keys) |n| b.markColumnTarget(n);
                    for (table.columns) |n| b.markColumnTarget(n);
                },
                .select => {
                    const select = tree.fullSelect(node);
                    b.setClass(select.select_token, .{ .type = .keyword });
                    if (select.limit) |limit| {
                        if (limit.order_column) |t| b.setClass(t, .{ .type = .operator });
                    }
                    if (select.distinct_token) |t| b.setClass(t, .{ .type = .keyword });
                    b.markColumns(select.select);
                    if (select.by) |by| {
                        b.setClass(by.by_token, .{ .type = .keyword });
                        b.markColumns(by.exprs);
                    }
                    b.setClass(select.from_token, .{ .type = .keyword });
                    if (select.where) |where| {
                        b.setClass(where.where_token, .{ .type = .keyword });
                        b.markClause(where.exprs);
                    }
                },
                .exec => {
                    const exec = tree.fullExec(node);
                    b.setClass(exec.exec_token, .{ .type = .keyword });
                    b.markColumns(exec.select);
                    if (exec.by) |by| {
                        b.setClass(by.by_token, .{ .type = .keyword });
                        b.markColumns(by.exprs);
                    }
                    b.setClass(exec.from_token, .{ .type = .keyword });
                    if (exec.where) |where| {
                        b.setClass(where.where_token, .{ .type = .keyword });
                        b.markClause(where.exprs);
                    }
                },
                .update => {
                    const update = tree.fullUpdate(node);
                    b.setClass(update.update_token, .{ .type = .keyword });
                    b.markColumns(update.select);
                    if (update.by) |by| {
                        b.setClass(by.by_token, .{ .type = .keyword });
                        b.markColumns(by.exprs);
                    }
                    b.setClass(update.from_token, .{ .type = .keyword });
                    if (update.where) |where| {
                        b.setClass(where.where_token, .{ .type = .keyword });
                        b.markClause(where.exprs);
                    }
                },
                .delete_rows => {
                    const delete = tree.fullDeleteRows(node);
                    b.setClass(delete.delete_token, .{ .type = .keyword });
                    b.setClass(delete.from_token, .{ .type = .keyword });
                    if (delete.where) |where| {
                        b.setClass(where.where_token, .{ .type = .keyword });
                        b.markClause(where.exprs);
                    }
                },
                .delete_cols => {
                    const delete = tree.fullDeleteCols(node);
                    b.setClass(delete.delete_token, .{ .type = .keyword });
                    for (delete.select_tokens) |t| b.setClass(t, .{ .type = .property });
                    b.setClass(delete.from_token, .{ .type = .keyword });
                },
                else => {},
            }
        }
    }

    fn setClass(b: *Builder, token: Ast.TokenIndex, class: Class) void {
        b.overrides[token] = .{ .class = class };
    }

    /// `name:expr` in a column position names a column, not a variable.
    fn markColumnTarget(b: *Builder, node: Ast.Node.Index) void {
        const assignment = hover.assignmentAt(b.tree, node) orelse return;
        b.setClass(b.tree.nodeMainToken(assignment.target), .{ .type = .property });
    }

    /// Column expressions of a select/by clause: `a:expr, b, ...`.
    fn markColumns(b: *Builder, exprs: []const Ast.Node.Index) void {
        b.markClause(exprs);
        for (exprs, 0..) |n, i| {
            b.markColumnTarget(n);
            if (i == 0) continue;
            const separator = b.tree.lastToken(exprs[i - 1]) + 1;
            if (b.tree.tokenTag(separator) == .comma) b.overrides[separator] = .skip;
        }
    }

    fn markClause(b: *Builder, exprs: []const Ast.Node.Index) void {
        for (exprs) |n| {
            const first = b.tree.firstToken(n);
            const last = b.tree.lastToken(n);
            @memset(b.in_column_context[first .. last + 1], true);
        }
    }

    fn emitTokens(b: *Builder) !void {
        const tree = b.tree;
        for (0..tree.tokens.len) |i| {
            const token: Ast.TokenIndex = @intCast(i);
            if (b.overrides[token]) |override| switch (override) {
                .class => |class| try b.add(token, class),
                .skip => {},
            } else if (try b.classifyByTag(token)) |class| {
                try b.add(token, class);
            }
        }
    }

    fn classifyByTag(b: *Builder, token: Ast.TokenIndex) !?Class {
        return switch (b.tree.tokenTag(token)) {
            .identifier => try b.classifyIdentifier(token),
            .prefix_builtin, .infix_builtin => Class.with(.{ .type = .function }, .defaultLibrary),
            .number_literal => .{ .type = .number },
            .string_literal => .{ .type = .string },
            .symbol_literal => .{ .type = .enumMember },
            .system, .dsl => .{ .type = .macro },
            .keyword_select, .keyword_exec, .keyword_update, .keyword_delete => .{ .type = .keyword },

            .colon,
            .colon_colon,
            .plus,
            .plus_colon,
            .minus,
            .minus_colon,
            .asterisk,
            .asterisk_colon,
            .percent,
            .percent_colon,
            .ampersand,
            .ampersand_colon,
            .pipe,
            .pipe_colon,
            .caret,
            .caret_colon,
            .equal,
            .equal_colon,
            .angle_bracket_left,
            .angle_bracket_left_colon,
            .angle_bracket_left_equal,
            .angle_bracket_left_right,
            .angle_bracket_right,
            .angle_bracket_right_colon,
            .angle_bracket_right_equal,
            .dollar,
            .dollar_colon,
            .comma,
            .comma_colon,
            .hash,
            .hash_colon,
            .underscore,
            .underscore_colon,
            .tilde,
            .tilde_colon,
            .bang,
            .bang_colon,
            .question_mark,
            .question_mark_colon,
            .at,
            .at_colon,
            .period,
            .period_colon,
            .zero_colon,
            .zero_colon_colon,
            .one_colon,
            .one_colon_colon,
            .two_colon,
            .apostrophe,
            .apostrophe_colon,
            .slash,
            .slash_colon,
            .backslash,
            .backslash_colon,
            => .{ .type = .operator },

            .l_paren,
            .r_paren,
            .l_bracket,
            .r_bracket,
            .l_brace,
            .r_brace,
            .semicolon,
            .invalid,
            .eob,
            .eof,
            => null,
        };
    }

    fn classifyIdentifier(b: *Builder, token: Ast.TokenIndex) !Class {
        const tree = b.tree;
        const doc_scope = b.doc_scope;
        const name = tree.tokenSlice(token);
        const start: u32 = @intCast(tree.tokenLoc(token).start);

        if (doc_scope.resolveName(doc_scope.innermostScopeAtIndex(start), name)) |resolved| {
            if (resolved.scope != .root) return try b.localClass(token, resolved.chain);
        }
        if (doc_scope.getScopeDeclarationChain(.{ .scope = .root, .name = name })) |chain| {
            const kind: TokenType = if (try b.chainIsFunction(b.currentFile(), chain)) .function else .variable;
            return b.withDeclarationModifiers(token, chain, Class.with(.{ .type = kind }, .static));
        }

        // Undeclared in this file.
        for (control_words) |word| {
            if (std.mem.eql(u8, name, word)) return .{ .type = .keyword };
        }
        // In q, load order defines globals, so a name declared at the root
        // scope of any other file is the same global. This outranks the
        // column heuristic: a function from another file called inside a
        // q-sql clause is still a function.
        if (try b.workspaceDeclaresFunction(name, b.handle)) return Class.with(.{ .type = .function }, .static);
        if (b.in_column_context[token]) return .{ .type = .property };
        const global = Class.with(.{ .type = .variable }, .static);
        for (library_namespaces) |prefix| {
            if (std.mem.startsWith(u8, name, prefix)) return global.with(.defaultLibrary);
        }
        return global;
    }

    fn localClass(b: *Builder, token: Ast.TokenIndex, chain: DocumentScope.DeclarationChain) !Class {
        const is_parameter = switch (b.declaration(chain.first)) {
            .function_parameter => true,
            // An implicit parameter is declared at its first use, a local at
            // its first assignment.
            .ast_node => |n| !b.assign_targets.contains(b.tree.nodeMainToken(n)),
        };
        const kind: TokenType = if (is_parameter)
            .parameter
        else if (try b.chainIsFunction(b.currentFile(), chain))
            .function
        else
            .variable;
        return b.withDeclarationModifiers(token, chain, .{ .type = kind });
    }

    /// Adds `declaration` or `modification` if `token` is one of the chain's
    /// declaration sites.
    fn withDeclarationModifiers(
        b: *Builder,
        token: Ast.TokenIndex,
        chain: DocumentScope.DeclarationChain,
        class: Class,
    ) Class {
        var it = b.doc_scope.iterateDeclarationChain(chain.first);
        while (it.next()) |decl_index| {
            const decl = b.declaration(decl_index);
            if (decl.nameToken(b.tree) != token) continue;
            switch (decl) {
                .function_parameter => return class.with(.declaration),
                .ast_node => {
                    // The first use of an implicit parameter is not an
                    // assignment and so gets no modifier.
                    if (!b.assign_targets.contains(token)) return class;
                    return class.with(if (decl_index == chain.first) .declaration else .modification);
                },
            }
        }
        // A compound assignment (`a+:1`) updates a name without AstGen
        // recording a new declaration for it.
        if (b.assign_targets.get(token) == .compound) return class.with(.modification);
        return class;
    }

    fn currentFile(b: *Builder) FileContext {
        return .{
            .handle = b.handle,
            .tree = b.tree,
            .doc_scope = b.doc_scope,
            .assign_values = &b.assign_values,
        };
    }

    fn fileContext(b: *Builder, handle: *DocumentStore.Handle) !FileContext {
        if (handle == b.handle) return b.currentFile();
        return .{
            .handle = handle,
            .tree = handle.tree,
            .doc_scope = try handle.getDocumentScope(b.io, b.gpa),
            .assign_values = try b.otherAssignValues(handle),
        };
    }

    /// Whether any declaration in `chain` is a plain assignment of a
    /// function value (see `valueIsFunction`).
    fn chainIsFunction(b: *Builder, file: FileContext, chain: DocumentScope.DeclarationChain) !bool {
        const key: ChainKey = .{ .handle = file.handle, .first = chain.first };
        const gop = try b.chains_in_progress.getOrPut(b.arena, key);
        if (gop.found_existing) return false;
        defer assert(b.chains_in_progress.remove(key));

        var it = file.doc_scope.iterateDeclarationChain(chain.first);
        while (it.next()) |decl_index| {
            const ident_node = switch (file.doc_scope.declarations.get(@backingInt(decl_index))) {
                .ast_node => |n| n,
                .function_parameter => continue,
            };
            const value = file.assign_values.get(ident_node) orelse continue;
            if (try b.valueIsFunction(file, value)) return true;
        }
        return false;
    }

    /// Whether the expression `value` evaluates to a function: a lambda or
    /// builtin, a chained assignment of one (`a:b:{x}`), or the name of
    /// one (`g:f`). Projections and compositions are not recognized since
    /// that would need arity inference.
    fn valueIsFunction(b: *Builder, file: FileContext, value: Ast.Node.Index) Allocator.Error!bool {
        const node = file.tree.unwrapGroupedExpr(value);
        switch (file.tree.nodeTag(node)) {
            .lambda, .builtin => return true,
            .identifier => return try b.nameIsFunction(file, node),
            else => {
                const assignment = hover.assignmentAt(file.tree, node) orelse return false;
                return try b.valueIsFunction(file, assignment.value);
            },
        }
    }

    /// Whether the identifier `node` in `file` refers to a function, using
    /// the same rule as `classifyIdentifier`: a name declared in `file` is
    /// decided by that file, any other name by the rest of the workspace.
    fn nameIsFunction(b: *Builder, file: FileContext, node: Ast.Node.Index) Allocator.Error!bool {
        const token = file.tree.nodeMainToken(node);
        const name = file.tree.tokenSlice(token);
        const start: u32 = @intCast(file.tree.tokenLoc(token).start);

        if (file.doc_scope.resolveName(file.doc_scope.innermostScopeAtIndex(start), name)) |resolved| {
            if (resolved.scope != .root) return try b.chainIsFunction(file, resolved.chain);
        }
        if (file.doc_scope.getScopeDeclarationChain(.{ .scope = .root, .name = name })) |chain| {
            return try b.chainIsFunction(file, chain);
        }
        return try b.workspaceDeclaresFunction(name, file.handle);
    }

    /// Whether the root scope of any file other than `except` declares
    /// `name` as a function.
    fn workspaceDeclaresFunction(b: *Builder, name: []const u8, except: *DocumentStore.Handle) Allocator.Error!bool {
        if (b.workspace_lambda_cache.get(name)) |cached| return cached;

        const result = for (b.other_handles) |other| {
            if (other == except) continue;
            const other_scope = try other.getDocumentScope(b.io, b.gpa);
            const chain = other_scope.getScopeDeclarationChain(.{ .scope = .root, .name = name }) orelse continue;
            if (try b.chainIsFunction(try b.fileContext(other), chain)) break true;
        } else false;

        // A result computed while resolving an alias may have been cut short
        // by the cycle check, and `except` varies, so only cache answers for
        // the current file's own references.
        if (b.chains_in_progress.count() == 0 and except == b.handle) {
            try b.workspace_lambda_cache.put(b.arena, name, result);
        }
        return result;
    }

    fn otherAssignValues(b: *Builder, other: *DocumentStore.Handle) !*const AssignValues {
        const gop = try b.other_assign_values.getOrPut(b.arena, other);
        if (!gop.found_existing) {
            gop.value_ptr.* = .empty;
            try indexAssignValues(b.arena, other.tree, gop.value_ptr);
        }
        return gop.value_ptr;
    }

    fn declaration(b: *const Builder, index: DocumentScope.Declaration.Index) DocumentScope.Declaration {
        return b.doc_scope.declarations.get(@backingInt(index));
    }

    fn add(b: *Builder, token: Ast.TokenIndex, class: Class) !void {
        const loc = b.tree.tokenLoc(token);
        assert(loc.start <= loc.end);
        assert(b.previous_source_index <= b.source_index);
        if (loc.start < b.previous_source_index) return;
        if (!b.overlapping_token_support and loc.start < b.source_index) return;

        const source = b.tree.source;
        const delta_text = source[b.previous_source_index..loc.start];
        const delta = offsets.indexToPosition(delta_text, delta_text.len, b.encoding);
        const length: u32 = @intCast(offsets.locLength(source, .{ .start = loc.start, .end = loc.end }, b.encoding));

        try b.token_buffer.appendSlice(b.arena, &.{
            delta.line,
            delta.character,
            length,
            @backingInt(class.type),
            class.modifiers,
        });
        b.previous_source_index = loc.start;
        b.source_index = loc.end;
    }
};

fn indexAssignValues(arena: Allocator, tree: Ast, assign_values: *AssignValues) Allocator.Error!void {
    for (0..tree.nodes.len) |i| {
        const node: Ast.Node.Index = @fromBackingInt(@intCast(i));
        const assignment = hover.assignmentAt(tree, node) orelse continue;
        try assign_values.put(arena, assignment.target, assignment.value);
    }
}

/// Decodes the LSP delta encoding into one `type[.modifier...] text` line per
/// token.
fn renderTokens(arena: Allocator, source: []const u8, data: []const u32) ![]const u8 {
    var out: std.ArrayList(u8) = .empty;
    var line: u32 = 0;
    var character: u32 = 0;
    var i: usize = 0;
    while (i < data.len) : (i += 5) {
        const delta_line, const delta_char, const length, const token_type, const modifiers = data[i..][0..5].*;
        if (delta_line > 0) {
            line += delta_line;
            character = delta_char;
        } else {
            character += delta_char;
        }
        const index = offsets.positionToIndex(source, .{ .line = line, .character = character }, .@"utf-8");

        try out.appendSlice(arena, @tagName(@as(TokenType, @fromBackingInt(@intCast(token_type)))));
        for (std.enums.values(TokenModifier)) |modifier| {
            if (modifiers & (@as(u32, 1) << @backingInt(modifier)) != 0) {
                try out.appendSlice(arena, ".");
                try out.appendSlice(arena, @tagName(modifier));
            }
        }
        try out.appendSlice(arena, try std.fmt.allocPrint(arena, " {s}\n", .{source[index .. index + length]}));
    }
    return out.items;
}

fn expectTokens(source: [:0]const u8, expected: []const u8) !void {
    try expectTokensMulti(&.{.{ .uri = "file:///test.q", .source = source }}, expected);
}

/// Checks the tokens of `files[0]` with the rest of `files` loaded in the
/// same document store.
fn expectTokensMulti(files: []const goto.TestFile, expected: []const u8) !void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var store: DocumentStore = .{ .io = io, .gpa = gpa };
    defer store.deinit();
    const handle = try goto.testStoreWithFiles(&store, files);

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const result = try writeSemanticTokens(arena, io, gpa, &store, handle, .@"utf-8", false);
    try std.testing.expectEqualStrings(expected, try renderTokens(arena, files[0].source, result.data));
}

test "global variable declaration" {
    try expectTokens("x:1",
        \\variable.declaration.static x
        \\operator :
        \\number 1
        \\
    );
}

test "global function with explicit parameters" {
    try expectTokens("f:{[a;b]a+b}",
        \\function.declaration.static f
        \\operator :
        \\parameter.declaration a
        \\parameter.declaration b
        \\parameter a
        \\operator +
        \\parameter b
        \\
    );
}

test "implicit parameters" {
    try expectTokens("{x+y}",
        \\parameter x
        \\operator +
        \\parameter y
        \\
    );
}

test "local declared then modified then read" {
    try expectTokens("{a:1;a:2;a}",
        \\variable.declaration a
        \\operator :
        \\number 1
        \\variable.modification a
        \\operator :
        \\number 2
        \\variable a
        \\
    );
}

test "compound assignment of a local is a modification" {
    try expectTokens("{a:1;a+:2}",
        \\variable.declaration a
        \\operator :
        \\number 1
        \\variable.modification a
        \\operator +:
        \\number 2
        \\
    );
}

test "calling a global function" {
    try expectTokens("f:{x};f 1",
        \\function.declaration.static f
        \\operator :
        \\parameter x
        \\function.static f
        \\number 1
        \\
    );
}

test "local assigned a lambda" {
    try expectTokens("{g:{x};g 1}",
        \\function.declaration g
        \\operator :
        \\parameter x
        \\function g
        \\number 1
        \\
    );
}

test "global assigned inside a lambda" {
    try expectTokens("g:1;f:{g::2}",
        \\variable.declaration.static g
        \\operator :
        \\number 1
        \\function.declaration.static f
        \\operator :
        \\variable.modification.static g
        \\operator ::
        \\number 2
        \\
    );
}

test "undeclared global and library namespace" {
    try expectTokens("t;.z.p",
        \\variable.static t
        \\variable.static.defaultLibrary .z.p
        \\
    );
}

test "builtins and literals" {
    try expectTokens("til 3;\"hi\";`a`b;1 2",
        \\function.defaultLibrary til
        \\number 3
        \\string "hi"
        \\enumMember `a
        \\enumMember `b
        \\number 1
        \\number 2
        \\
    );
}

test "control words" {
    try expectTokens("if[1;2]",
        \\keyword if
        \\number 1
        \\number 2
        \\
    );
}

test "system command and dsl marker" {
    try expectTokens("\\l foo.q\nk)1",
        \\macro \l foo.q
        \\macro k)
        \\number 1
        \\
    );
}

test "select with columns, by, from and where" {
    try expectTokens("v:1;select a:b+v,c by d from t where e>1",
        \\variable.declaration.static v
        \\operator :
        \\number 1
        \\keyword select
        \\property a
        \\operator :
        \\property b
        \\operator +
        \\variable.static v
        \\property c
        \\keyword by
        \\property d
        \\keyword from
        \\variable.static t
        \\keyword where
        \\property e
        \\operator >
        \\number 1
        \\
    );
}

test "where outside q-sql is a builtin" {
    try expectTokens("where 1 0",
        \\function.defaultLibrary where
        \\number 1
        \\number 0
        \\
    );
}

test "update and delete" {
    try expectTokens("update a:1 from t;delete b from t;delete from t where c",
        \\keyword update
        \\property a
        \\operator :
        \\number 1
        \\keyword from
        \\variable.static t
        \\keyword delete
        \\property b
        \\keyword from
        \\variable.static t
        \\keyword delete
        \\keyword from
        \\variable.static t
        \\keyword where
        \\property c
        \\
    );
}

test "table literal columns" {
    try expectTokens("([k:`a]v:1 2;w:x)",
        \\property k
        \\operator :
        \\enumMember `a
        \\property v
        \\operator :
        \\number 1
        \\number 2
        \\property w
        \\operator :
        \\variable.static x
        \\
    );
}

test "multi-line source" {
    try expectTokens("a:1\nf:{\n  a+1}",
        \\variable.declaration.static a
        \\operator :
        \\number 1
        \\function.declaration.static f
        \\operator :
        \\variable.static a
        \\operator +
        \\number 1
        \\
    );
}

test "function declared in another file" {
    try expectTokensMulti(&.{
        .{ .uri = "file:///main.q", .source = "f[1]" },
        .{ .uri = "file:///lib.q", .source = "f:{x+1}" },
    },
        \\function.static f
        \\number 1
        \\
    );
}

test "variable declared in another file" {
    try expectTokensMulti(&.{
        .{ .uri = "file:///main.q", .source = "v+1" },
        .{ .uri = "file:///lib.q", .source = "v:42" },
    },
        \\variable.static v
        \\operator +
        \\number 1
        \\
    );
}

test "function from another file inside a q-sql clause" {
    try expectTokensMulti(&.{
        .{ .uri = "file:///main.q", .source = "select a from t where f[b]" },
        .{ .uri = "file:///lib.q", .source = "f:{x>0}" },
    },
        \\keyword select
        \\property a
        \\keyword from
        \\variable.static t
        \\keyword where
        \\function.static f
        \\property b
        \\
    );
}

test "local declaration outranks another file's lambda" {
    try expectTokensMulti(&.{
        .{ .uri = "file:///main.q", .source = "f:1;f" },
        .{ .uri = "file:///lib.q", .source = "f:{x}" },
    },
        \\variable.declaration.static f
        \\operator :
        \\number 1
        \\variable.static f
        \\
    );
}

test "chained assignment of a lambda" {
    try expectTokens("a:b:{x}",
        \\function.declaration.static a
        \\operator :
        \\function.declaration.static b
        \\operator :
        \\parameter x
        \\
    );
}

test "alias of a function" {
    try expectTokens("f:{x};g:f;g 1",
        \\function.declaration.static f
        \\operator :
        \\parameter x
        \\function.declaration.static g
        \\operator :
        \\function.static f
        \\function.static g
        \\number 1
        \\
    );
}

test "alias of a builtin" {
    try expectTokens("cnt:count",
        \\function.declaration.static cnt
        \\operator :
        \\function.defaultLibrary count
        \\
    );
}

test "alias of a function from another file" {
    try expectTokensMulti(&.{
        .{ .uri = "file:///main.q", .source = "g:f" },
        .{ .uri = "file:///lib.q", .source = "f:{x}" },
    },
        \\function.declaration.static g
        \\operator :
        \\function.static f
        \\
    );
}

test "alias declared in another file" {
    try expectTokensMulti(&.{
        .{ .uri = "file:///main.q", .source = "g 1" },
        .{ .uri = "file:///lib.q", .source = "f:{x};g:f" },
    },
        \\function.static g
        \\number 1
        \\
    );
}

test "alias chain spanning files" {
    try expectTokensMulti(&.{
        .{ .uri = "file:///main.q", .source = "h:g" },
        .{ .uri = "file:///a.q", .source = "g:f" },
        .{ .uri = "file:///b.q", .source = "f:{x}" },
    },
        \\function.declaration.static h
        \\operator :
        \\function.static g
        \\
    );
}

test "local alias of a function" {
    try expectTokens("f:{x};{g:f;g 1}",
        \\function.declaration.static f
        \\operator :
        \\parameter x
        \\function.declaration g
        \\operator :
        \\function.static f
        \\function g
        \\number 1
        \\
    );
}

test "alias cycle is not a function" {
    try expectTokens("a:b;b:a",
        \\variable.declaration.static a
        \\operator :
        \\variable.static b
        \\variable.declaration.static b
        \\operator :
        \\variable.static a
        \\
    );
}

test "alias cycle across files is not a function" {
    try expectTokensMulti(&.{
        .{ .uri = "file:///main.q", .source = "a:b" },
        .{ .uri = "file:///lib.q", .source = "b:a" },
    },
        \\variable.declaration.static a
        \\operator :
        \\variable.static b
        \\
    );
}

test "alias cycle containing a lambda" {
    try expectTokens("a:b;a:{x};b:a",
        \\function.declaration.static a
        \\operator :
        \\function.static b
        \\function.modification.static a
        \\operator :
        \\parameter x
        \\function.declaration.static b
        \\operator :
        \\function.static a
        \\
    );
}

test "projection is not recognized as a function" {
    try expectTokens("f:{x+y};g:f[1]",
        \\function.declaration.static f
        \\operator :
        \\parameter x
        \\operator +
        \\parameter y
        \\variable.declaration.static g
        \\operator :
        \\function.static f
        \\number 1
        \\
    );
}

test "parse error still highlights tokens" {
    try expectTokens("f:{",
        \\variable.static f
        \\operator :
        \\
    );
}

test {
    std.testing.refAllDecls(@This());
}
