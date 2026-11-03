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
const hover = @import("hover.zig");

/// Builds the symbol outline of `handle`.
///
/// Every global declared in the file's root scope becomes a top-level symbol,
/// including globals assigned inside a lambda (`g::1` or `.ns.g:1`). Each
/// assignment of a global is listed, since q files commonly redefine a name.
/// A name whose value is a lambda is a `Function` whose children are the
/// lambda's locals (first assignment only); anything else is a `Variable`.
/// Parameters are not symbols; a function's parameters appear in its detail.
///
/// When `hierarchical` is false the same tree is flattened into
/// `SymbolInformation`s for clients without hierarchical symbol support.
pub fn documentSymbols(
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    handle: *DocumentStore.Handle,
    encoding: offsets.Encoding,
    hierarchical: bool,
) !types.DocumentSymbol.Result {
    const doc_scope = try handle.getDocumentScope(io, gpa);

    var builder: Builder = .{
        .arena = arena,
        .tree = handle.tree,
        .doc_scope = doc_scope,
        .encoding = encoding,
    };
    try builder.indexTree();
    const symbols = try builder.scopeSymbols(.root, .every_declaration);

    if (hierarchical) return .{ .document_symbols = symbols };

    var flat: std.ArrayList(types.SymbolInformation) = .empty;
    try flatten(arena, &flat, symbols, handle.uri.raw, null);
    return .{ .symbol_informations = flat.items };
}

const Builder = struct {
    arena: Allocator,
    tree: Ast,
    doc_scope: *const DocumentScope,
    encoding: offsets.Encoding,
    /// Assignment target (`.identifier` node) -> the assignment it belongs to.
    assignments: std.AutoHashMapUnmanaged(Ast.Node.Index, AssignmentSite) = .empty,
    /// Lambda node -> the `.function` scope AstGen created for it.
    lambda_scopes: std.AutoHashMapUnmanaged(Ast.Node.Index, DocumentScope.Scope.Index) = .empty,

    const AssignmentSite = struct {
        node: Ast.Node.Index,
        value: Ast.Node.Index,
    };

    const ChainPolicy = enum {
        /// One symbol per declaration, so a re-assigned global is listed at
        /// every assignment site.
        every_declaration,
        /// Only the first declaration of each name: a local re-assigned
        /// later in its body is still the same local.
        first_declaration,
    };

    fn indexTree(b: *Builder) Allocator.Error!void {
        for (0..b.tree.nodes.len) |i| {
            const node: Ast.Node.Index = @fromBackingInt(@intCast(i));
            const assignment = hover.assignmentAt(b.tree, node) orelse continue;
            try b.assignments.put(b.arena, assignment.target, .{ .node = node, .value = assignment.value });
        }
        for (b.doc_scope.scopes.items(.data), 0..) |data, i| {
            if (data.tag != .function) continue;
            try b.lambda_scopes.put(b.arena, data.node, @fromBackingInt(@intCast(i)));
        }
    }

    fn scopeSymbols(
        b: *Builder,
        scope: DocumentScope.Scope.Index,
        policy: ChainPolicy,
    ) Allocator.Error![]const types.DocumentSymbol {
        var symbols: std.ArrayList(types.DocumentSymbol) = .empty;
        for (b.doc_scope.getScopeDeclarationsConst(scope)) |first| {
            var it = b.doc_scope.iterateDeclarationChain(first);
            while (it.next()) |decl_index| {
                if (try b.declarationSymbol(decl_index)) |symbol| {
                    try symbols.append(b.arena, symbol);
                }
                if (policy == .first_declaration) break;
            }
        }
        // Root declarations are recorded in evaluation order, which differs
        // from source order for globals assigned inside lambdas and for
        // re-declaration chains.
        std.mem.sort(types.DocumentSymbol, symbols.items, {}, lessThanByStart);
        return symbols.items;
    }

    fn declarationSymbol(
        b: *Builder,
        decl_index: DocumentScope.Declaration.Index,
    ) Allocator.Error!?types.DocumentSymbol {
        const ident_node = switch (b.doc_scope.declarations.get(@backingInt(decl_index))) {
            .ast_node => |n| n,
            .function_parameter => return null,
        };
        // Implicit parameters (`x`/`y`/`z`) are declared at their first use
        // rather than by an assignment.
        const site = b.assignments.get(ident_node) orelse return null;
        const name_token = b.tree.nodeMainToken(ident_node);

        var symbol: types.DocumentSymbol = .{
            .name = b.tree.tokenSlice(name_token),
            .kind = .Variable,
            .range = b.nodeRange(site.node),
            .selectionRange = b.tokenRange(name_token),
        };

        const value_node = b.tree.unwrapGroupedExpr(site.value);
        if (b.tree.nodeTag(value_node) == .lambda) {
            symbol.kind = .Function;
            const params = try hover.lambdaParamNames(b.arena, b.tree, value_node);
            symbol.detail = try std.fmt.allocPrint(b.arena, "[{s}]", .{try std.mem.join(b.arena, ";", params)});
            if (b.lambda_scopes.get(value_node)) |scope| {
                const children = try b.scopeSymbols(scope, .first_declaration);
                if (children.len > 0) symbol.children = children;
            }
        }
        return symbol;
    }

    fn nodeRange(b: *const Builder, node: Ast.Node.Index) types.Range {
        return offsets.locToRange(b.tree.source, .{
            .start = b.tree.tokenLoc(b.tree.firstToken(node)).start,
            .end = b.tree.tokenLoc(b.tree.lastToken(node)).end,
        }, b.encoding);
    }

    fn tokenRange(b: *const Builder, token: Ast.TokenIndex) types.Range {
        const loc = b.tree.tokenLoc(token);
        return offsets.locToRange(b.tree.source, .{ .start = loc.start, .end = loc.end }, b.encoding);
    }

    fn lessThanByStart(_: void, a: types.DocumentSymbol, b: types.DocumentSymbol) bool {
        return offsets.orderPosition(a.selectionRange.start, b.selectionRange.start) == .lt;
    }
};

fn flatten(
    arena: Allocator,
    out: *std.ArrayList(types.SymbolInformation),
    symbols: []const types.DocumentSymbol,
    uri: []const u8,
    container_name: ?[]const u8,
) Allocator.Error!void {
    for (symbols) |symbol| {
        try out.append(arena, .{
            .name = symbol.name,
            .kind = symbol.kind,
            .location = .{ .uri = uri, .range = symbol.range },
            .containerName = container_name,
        });
        if (symbol.children) |children| {
            try flatten(arena, out, children, uri, symbol.name);
        }
    }
}

/// Renders symbols one per line as `Kind name[detail] @selection [range]`
/// with byte offsets, indenting children by two spaces.
fn renderSymbols(
    arena: Allocator,
    out: *std.ArrayList(u8),
    source: []const u8,
    symbols: []const types.DocumentSymbol,
    depth: usize,
) !void {
    for (symbols) |symbol| {
        try out.appendNTimes(arena, ' ', depth * 2);
        try out.appendSlice(arena, try std.fmt.allocPrint(arena, "{s} {s}{s} @{d} [{d}..{d}]\n", .{
            @tagName(symbol.kind),
            symbol.name,
            symbol.detail orelse "",
            offsets.positionToIndex(source, symbol.selectionRange.start, .@"utf-8"),
            offsets.positionToIndex(source, symbol.range.start, .@"utf-8"),
            offsets.positionToIndex(source, symbol.range.end, .@"utf-8"),
        }));
        if (symbol.children) |children| try renderSymbols(arena, out, source, children, depth + 1);
    }
}

fn expectSymbols(source: [:0]const u8, expected: []const u8) !void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var store: DocumentStore = .{ .io = io, .gpa = gpa };
    defer store.deinit();
    const files = [_]goto.TestFile{.{ .uri = "file:///test.q", .source = source }};
    const handle = try goto.testStoreWithFiles(&store, &files);

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const result = try documentSymbols(arena, io, gpa, handle, .@"utf-8", true);
    const symbols = switch (result) {
        .document_symbols => |s| s,
        .symbol_informations => return error.TestExpectedHierarchical,
    };

    var rendered: std.ArrayList(u8) = .empty;
    try renderSymbols(arena, &rendered, source, symbols, 0);
    try std.testing.expectEqualStrings(expected, rendered.items);
}

test "global variable" {
    try expectSymbols("x:1",
        \\Variable x @0 [0..3]
        \\
    );
}

test "global function with explicit parameters and a local" {
    try expectSymbols("f:{[a;b]c:a+b;c}",
        \\Function f[a;b] @0 [0..16]
        \\  Variable c @8 [8..13]
        \\
    );
}

test "implicit parameters are not symbols" {
    try expectSymbols("f:{x+y}",
        \\Function f[x;y] @0 [0..7]
        \\
    );
}

test "explicit parameters are not symbols" {
    try expectSymbols("f:{[a]a}",
        \\Function f[a] @0 [0..8]
        \\
    );
}

test "namespaced global" {
    try expectSymbols(".ns.f:{x}",
        \\Function .ns.f[x] @0 [0..9]
        \\
    );
}

test "every assignment of a global is listed" {
    try expectSymbols("g:1;g:2",
        \\Variable g @0 [0..3]
        \\Variable g @4 [4..7]
        \\
    );
}

test "a re-assigned local is listed once" {
    try expectSymbols("f:{a:1;a:2;a}",
        \\Function f[x] @0 [0..13]
        \\  Variable a @3 [3..6]
        \\
    );
}

test "nested lambda assigned to a local" {
    try expectSymbols("f:{g:{[p]q:p;q};g 1}",
        \\Function f[x] @0 [0..20]
        \\  Function g[p] @3 [3..15]
        \\    Variable q @9 [9..12]
        \\
    );
}

test "global assigned inside a lambda is top-level and in source order" {
    try expectSymbols("f:{g::1}",
        \\Function f[x] @0 [0..8]
        \\Variable g @3 [3..7]
        \\
    );
}

test "functional assignment form" {
    try expectSymbols(":[x;1]",
        \\Variable x @2 [0..6]
        \\
    );
}

test "grouped lambda value" {
    try expectSymbols("f:({[a]a})",
        \\Function f[a] @0 [0..10]
        \\
    );
}

test "multi-line symbols" {
    try expectSymbols("a:1\nf:{[p]\n  b:p;\n  b}\n",
        \\Variable a @0 [0..3]
        \\Function f[p] @4 [4..22]
        \\  Variable b @13 [13..16]
        \\
    );
}

test "parse error yields no symbols" {
    try expectSymbols("f:{", "");
}

test "flat fallback names the containing function" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var store: DocumentStore = .{ .io = io, .gpa = gpa };
    defer store.deinit();
    const files = [_]goto.TestFile{.{ .uri = "file:///test.q", .source = "f:{a:1};g:2" }};
    const handle = try goto.testStoreWithFiles(&store, &files);

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();

    const result = try documentSymbols(arena_instance.allocator(), io, gpa, handle, .@"utf-8", false);
    const infos = switch (result) {
        .symbol_informations => |s| s,
        .document_symbols => return error.TestExpectedFlat,
    };

    try std.testing.expectEqual(3, infos.len);
    try std.testing.expectEqualStrings("f", infos[0].name);
    try std.testing.expectEqual(types.SymbolKind.Function, infos[0].kind);
    try std.testing.expectEqual(null, infos[0].containerName);
    try std.testing.expectEqualStrings("a", infos[1].name);
    try std.testing.expectEqualStrings("f", infos[1].containerName.?);
    try std.testing.expectEqualStrings("g", infos[2].name);
    try std.testing.expectEqual(null, infos[2].containerName);
    try std.testing.expectEqualStrings("file:///test.q", infos[1].location.uri);
}

test {
    std.testing.refAllDecls(@This());
}
