const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const lsp = @import("lsp");
const types = lsp.types;
const offsets = lsp.offsets;

const kdb = @import("../../kdb/root.zig");
const Ast = kdb.Ast;
const DocumentScope = kdb.DocumentScope;
const DocumentStore = @import("../DocumentStore.zig");
const goto = @import("goto.zig");

/// A name resolved to a single declaration in some document.
pub const Symbol = struct {
    handle: *DocumentStore.Handle,
    decl: DocumentScope.Declaration,
    /// `true` if the declaration lives in a file's root (global) scope.
    is_global: bool,
};

pub fn hover(
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    document_store: *DocumentStore,
    handle: *DocumentStore.Handle,
    position: types.Position,
    encoding: offsets.Encoding,
) !?types.Hover {
    const tree = handle.tree;
    const source_index = offsets.positionToIndex(tree.source, position, encoding);
    const ident_token = nameTokenAtIndex(tree, source_index) orelse return null;
    const name = tree.tokenSlice(ident_token);

    const markdown = try renderHover(arena, io, gpa, document_store, handle, source_index, name) orelse return null;

    const token_loc = tree.tokenLoc(ident_token);
    return .{
        .contents = .{ .markup_content = .{ .kind = .markdown, .value = markdown } },
        .range = offsets.locToRange(tree.source, .{ .start = token_loc.start, .end = token_loc.end }, encoding),
    };
}

fn renderHover(
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    document_store: *DocumentStore,
    handle: *DocumentStore.Handle,
    source_index: usize,
    name: []const u8,
) !?[]const u8 {
    if (try resolveSymbol(arena, io, gpa, document_store, handle, source_index, name)) |symbol| {
        switch (symbol.decl) {
            .function_parameter => return try fenced(arena, name, "parameter", null),
            .ast_node => |ident_node| {
                const sym_tree = symbol.handle.tree;
                const kind: []const u8 = if (symbol.is_global) "global" else "local";
                if (findAssignedValue(sym_tree, ident_node)) |value_node| {
                    if (sym_tree.nodeTag(value_node) == .lambda) {
                        const params = try lambdaParamNames(arena, sym_tree, value_node);
                        const label = try signatureLabel(arena, name, params);
                        return try fenced(arena, label, kind, null);
                    }
                    const value_src = nodeSource(sym_tree, value_node);
                    const decl_src = try std.fmt.allocPrint(arena, "{s}:{s}", .{ name, value_src });
                    return try fenced(arena, decl_src, kind, null);
                }
                // No assignment found (e.g. an implicit `x`/`y`/`z` parameter).
                return try fenced(arena, name, kind, null);
            },
        }
    }

    return null;
}

/// Builds a Markdown hover body: a fenced `q` code block, an italic kind line,
/// and an optional trailing description paragraph.
fn fenced(arena: Allocator, code: []const u8, kind: []const u8, doc: ?[]const u8) Allocator.Error![]const u8 {
    const body = try std.fmt.allocPrint(arena, "```q\n{s}\n```\n\n_{s}_", .{ code, kind });
    if (doc) |d| return std.fmt.allocPrint(arena, "{s}\n\n{s}", .{ body, d });
    return body;
}

/// Renders `name[p1;p2;...]` (or `name[]` when there are no parameters).
pub fn signatureLabel(arena: Allocator, name: []const u8, params: []const []const u8) Allocator.Error![]const u8 {
    const joined = try std.mem.join(arena, ";", params);
    return std.fmt.allocPrint(arena, "{s}[{s}]", .{ name, joined });
}

/// Resolves `name` to a single declaration, searching the current document's
/// scopes first and then every document's root scope (q globals are visible
/// across the workspace). Returns null for builtins and undeclared names.
pub fn resolveSymbol(
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    document_store: *DocumentStore,
    handle: *DocumentStore.Handle,
    source_index: usize,
    name: []const u8,
) !?Symbol {
    const doc_scope = try handle.getDocumentScope(io, gpa);
    if (doc_scope.resolveName(doc_scope.innermostScopeAtIndex(@intCast(source_index)), name)) |r| {
        if (r.scope != .root) {
            return .{ .handle = handle, .decl = lastDecl(doc_scope, r.chain), .is_global = false };
        }
    }

    // A root-scope or unresolved name is a workspace global: prefer a
    // definition in the current file, then fall back to other files.
    if (rootSymbol(io, gpa, handle, name)) |sym| return sym;
    const handles = try document_store.collectHandles(arena);
    for (handles) |other| {
        if (other == handle) continue;
        if (try rootSymbolChecked(io, gpa, other, name)) |sym| return sym;
    }
    return null;
}

fn rootSymbolChecked(io: Io, gpa: Allocator, handle: *DocumentStore.Handle, name: []const u8) !?Symbol {
    const doc_scope = try handle.getDocumentScope(io, gpa);
    const chain = doc_scope.getScopeDeclarationChain(.{ .scope = .root, .name = name }) orelse return null;
    return .{ .handle = handle, .decl = lastDecl(doc_scope, chain), .is_global = true };
}

fn rootSymbol(io: Io, gpa: Allocator, handle: *DocumentStore.Handle, name: []const u8) ?Symbol {
    return rootSymbolChecked(io, gpa, handle, name) catch null;
}

fn lastDecl(doc_scope: *const DocumentScope, chain: DocumentScope.DeclarationChain) DocumentScope.Declaration {
    var it = doc_scope.iterateDeclarationChain(chain.first);
    var last = chain.first;
    while (it.next()) |idx| last = idx;
    return doc_scope.declarations.get(@intFromEnum(last));
}

/// Finds the value assigned to `ident_node` in `tree`, i.e. the right-hand side
/// of an assignment whose target is that identifier node. Handles both the
/// infix form (`name:expr`) and the functional form (`:[name;expr]`). Returns
/// null if no such assignment exists (e.g. for an implicit parameter).
pub fn findAssignedValue(tree: Ast, ident_node: Ast.Node.Index) ?Ast.Node.Index {
    const count: u32 = @intCast(tree.nodes.len);
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        const node: Ast.Node.Index = @enumFromInt(i);
        switch (tree.nodeTag(node)) {
            .apply_binary => {
                const op: Ast.Node.Index = @enumFromInt(tree.nodeMainToken(node));
                switch (tree.nodeTag(op)) {
                    .colon, .colon_colon => {},
                    else => continue,
                }
                const lhs, const rhs = tree.nodeData(node).node_and_opt_node;
                if (tree.unwrapGroupedExpr(lhs) != ident_node) continue;
                return rhs.unwrap();
            },
            .call => {
                const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Ast.Node.Index);
                if (nodes.len != 3) continue;
                switch (tree.nodeTag(tree.unwrapGroupedExpr(nodes[0]))) {
                    .colon, .colon_colon => {},
                    else => continue,
                }
                if (tree.unwrapGroupedExpr(nodes[1]) != ident_node) continue;
                return nodes[2];
            },
            else => {},
        }
    }
    return null;
}

const implicit_params = [_][]const u8{ "x", "y", "z" };

/// The parameter names of a `.lambda` node. Explicit parameters are returned
/// verbatim; for an implicit-parameter lambda the rank is inferred from the
/// highest of `x`/`y`/`z` referenced in its body.
pub fn lambdaParamNames(arena: Allocator, tree: Ast, lambda_node: Ast.Node.Index) ![]const []const u8 {
    const lambda = tree.fullLambda(lambda_node);
    if (lambda.params) |p| {
        const names = try arena.alloc([]const u8, p.params.len);
        for (p.params, 0..) |param, i| {
            names[i] = if (tree.nodeTag(param) == .empty) "" else tree.tokenSlice(tree.nodeMainToken(param));
        }
        return names;
    }

    // Implicit parameters: x is always present; y and z are only parameters
    // when referenced, and referencing a later one implies the earlier ones.
    var rank: usize = 1;
    var token = lambda.l_brace + 1;
    while (token < lambda.r_brace) : (token += 1) {
        if (tree.tokenTag(token) != .identifier) continue;
        const slice = tree.tokenSlice(token);
        if (std.mem.eql(u8, slice, "z")) {
            rank = 3;
            break;
        } else if (std.mem.eql(u8, slice, "y")) {
            rank = @max(rank, 2);
        }
    }
    return implicit_params[0..rank];
}

/// Like `goto.identifierTokenAtIndex`, but also matches built-in function
/// tokens (`count`, `in`, ...), which tokenize distinctly from identifiers.
pub fn nameTokenAtIndex(tree: Ast, source_index: usize) ?Ast.TokenIndex {
    const locs = tree.tokens.items(.loc);

    var left: usize = 0;
    var right: usize = locs.len;
    while (left < right) {
        const mid = left + (right - left) / 2;
        if (locs[mid].start <= source_index) {
            left = mid + 1;
        } else {
            right = mid;
        }
    }
    if (left == 0) return null;
    const token: Ast.TokenIndex = @intCast(left - 1);

    if (isNameToken(tree.tokenTag(token)) and source_index <= locs[token].end) {
        return token;
    }
    if (token > 0 and isNameToken(tree.tokenTag(token - 1)) and locs[token - 1].end == source_index) {
        return token - 1;
    }
    return null;
}

fn isNameToken(tag: kdb.Token.Tag) bool {
    return switch (tag) {
        .identifier, .prefix_builtin, .infix_builtin => true,
        else => false,
    };
}

fn nodeSource(tree: Ast, node: Ast.Node.Index) []const u8 {
    const start = tree.tokenLoc(tree.firstToken(node)).start;
    const end = tree.tokenLoc(tree.lastToken(node)).end;
    return tree.source[start..end];
}

fn expectHover(source: [:0]const u8, cursor: usize, needles: ?[]const []const u8) !void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var store: DocumentStore = .{ .io = io, .gpa = gpa };
    defer store.deinit();
    const files = [_]goto.TestFile{.{ .uri = "file:///test.q", .source = source }};
    const handle = try goto.testStoreWithFiles(&store, &files);

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();

    const result = try hover(
        arena_instance.allocator(),
        io,
        gpa,
        &store,
        handle,
        offsets.indexToPosition(source, cursor, .@"utf-8"),
        .@"utf-8",
    );

    const expected = needles orelse {
        try std.testing.expectEqual(@as(?types.Hover, null), result);
        return;
    };

    const value = switch ((result orelse return error.TestExpectedHover).contents) {
        .markup_content => |m| m.value,
        else => return error.TestExpectedMarkup,
    };
    for (expected) |needle| {
        if (std.mem.indexOf(u8, value, needle) == null) {
            std.debug.print("hover content:\n{s}\nmissing: {s}\n", .{ value, needle });
            return error.TestMissingNeedle;
        }
    }
}

test "hover local variable" {
    try expectHover("f:{a:1;a+1}", 7, &.{ "a:1", "_local_" });
}

test "hover global function with explicit params" {
    try expectHover("f:{[a;b]a+b}", 0, &.{ "f[a;b]", "_global_" });
}

test "hover global function with implicit params" {
    try expectHover("f:{x+z}", 0, &.{ "f[x;y;z]", "_global_" });
}

test "hover function parameter" {
    try expectHover("{[p]p+1}", 4, &.{ "p", "_parameter_" });
}

test "hover undeclared identifier" {
    try expectHover("f:{a:1};b", 8, null);
}

test "hover cross-file global function" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var store: DocumentStore = .{ .io = io, .gpa = gpa };
    defer store.deinit();
    const files = [_]goto.TestFile{
        .{ .uri = "file:///b.q", .source = "f[1]" },
        .{ .uri = "file:///a.q", .source = "f:{[a]a}" },
    };
    const handle = try goto.testStoreWithFiles(&store, &files);

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();

    const result = try hover(
        arena_instance.allocator(),
        io,
        gpa,
        &store,
        handle,
        offsets.indexToPosition(files[0].source, 0, .@"utf-8"),
        .@"utf-8",
    );
    const value = switch ((result orelse return error.TestExpectedHover).contents) {
        .markup_content => |m| m.value,
        else => return error.TestExpectedMarkup,
    };
    try std.testing.expect(std.mem.indexOf(u8, value, "f[a]") != null);
}

test {
    std.testing.refAllDecls(@This());
}
