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
const Uri = @import("../Uri.zig");
const goto = @import("goto.zig");

/// Known limitations, shared with goto definition:
/// - q-sql and table-literal column identifiers are plain identifier tokens
///   and can falsely match a variable of the same name.
/// - Name lookup is position-insensitive, so a use before the assignment
///   still matches.
pub fn findReferences(
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    handle: *DocumentStore.Handle,
    uri: types.DocumentUri,
    position: types.Position,
    include_declaration: bool,
    encoding: offsets.Encoding,
) !?[]const types.Location {
    const tree = handle.tree;
    const source_index = offsets.positionToIndex(tree.source, position, encoding);
    const ident_token = goto.identifierTokenAtIndex(tree, source_index) orelse return null;
    const name = tree.tokenSlice(ident_token);

    const doc_scope = try handle.getDocumentScope(io, gpa);

    const target = doc_scope.resolveName(
        doc_scope.innermostScopeAtIndex(@intCast(source_index)),
        name,
    ) orelse return null;

    // Declaration name tokens, used to filter out declaration sites when the
    // client does not want them. Chains are tiny, so a linear scan suffices.
    var decl_tokens: std.ArrayList(Ast.TokenIndex) = .empty;
    if (!include_declaration) {
        var it = doc_scope.iterateDeclarationChain(target.chain.first);
        while (it.next()) |decl_index| {
            const decl = doc_scope.declarations.get(@intFromEnum(decl_index));
            try decl_tokens.append(arena, decl.nameToken(tree));
        }
    }

    var locations: std.ArrayList(types.Location) = .empty;
    const locs = tree.tokens.items(.loc);
    for (0..tree.tokens.len) |i| {
        const token: Ast.TokenIndex = @intCast(i);
        if (tree.tokenTag(token) != .identifier) continue;
        if (!std.mem.eql(u8, tree.tokenSlice(token), name)) continue;
        if (std.mem.findScalar(Ast.TokenIndex, decl_tokens.items, token) != null) continue;

        const resolved = doc_scope.resolveName(
            doc_scope.innermostScopeAtIndex(@intCast(locs[token].start)),
            name,
        ) orelse continue;
        if (resolved.chain.first != target.chain.first) continue;

        try locations.append(arena, .{
            .uri = uri,
            .range = offsets.locToRange(
                tree.source,
                .{ .start = locs[token].start, .end = locs[token].end },
                encoding,
            ),
        });
    }

    return locations.items;
}

fn testFindReferences(
    source: [:0]const u8,
    cursor_offset: usize,
    include_declaration: bool,
    expected_offsets: ?[]const usize,
) !void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const test_uri = "file:///test.q";

    const uri = try Uri.parse(gpa, test_uri);
    defer uri.deinit(gpa);

    var handle: DocumentStore.Handle = try .init(io, gpa, uri, try gpa.dupeSentinel(u8, source, 0), true);
    defer handle.deinit(gpa);

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();

    const result = try findReferences(
        arena_instance.allocator(),
        io,
        gpa,
        &handle,
        test_uri,
        offsets.indexToPosition(source, cursor_offset, .@"utf-8"),
        include_declaration,
        .@"utf-8",
    );

    const expected = expected_offsets orelse {
        try std.testing.expectEqual(null, result);
        return;
    };
    const locations = result orelse return error.TestExpectedReferences;

    try std.testing.expectEqual(expected.len, locations.len);
    for (locations, expected) |location, expected_offset| {
        try std.testing.expectEqualStrings(test_uri, location.uri);
        const start = offsets.positionToIndex(source, location.range.start, .@"utf-8");
        try std.testing.expectEqual(expected_offset, start);
    }
}

test "local with multiple uses" {
    try testFindReferences("f:{a:1;a+a}", 7, true, &.{ 3, 7, 9 });
}

test "local with multiple uses excluding declaration" {
    try testFindReferences("f:{a:1;a+a}", 7, false, &.{ 7, 9 });
}

test "function parameter" {
    try testFindReferences("{[p]p+p}", 4, true, &.{ 2, 4, 6 });
}

test "implicit function parameter" {
    try testFindReferences("{x+x}", 3, true, &.{ 1, 3 });
}

test "implicit function parameter excluding declaration" {
    // The first use of an implicit parameter doubles as its declaration site.
    try testFindReferences("{x+x}", 3, false, &.{3});
}

test "global used at top level and inside lambda" {
    try testFindReferences("g:1;f:{g+1};g", 0, true, &.{ 0, 7, 12 });
}

test "redeclared global" {
    try testFindReferences("g:1;g:2;g", 8, true, &.{ 0, 4, 8 });
}

test "redeclared global excluding declarations" {
    try testFindReferences("g:1;g:2;g", 8, false, &.{8});
}

test "shadowing: query on inner local" {
    try testFindReferences("g:1;f:{g:2;g};g", 11, true, &.{ 7, 11 });
}

test "shadowing: query on outer global" {
    try testFindReferences("g:1;f:{g:2;g};g", 0, true, &.{ 0, 14 });
}

test "nested lambda: outer local excludes inner token" {
    try testFindReferences("f:{a:1;h:{a};a}", 3, true, &.{ 3, 13 });
}

test "nested lambda: inner token unresolvable" {
    try testFindReferences("f:{a:1;h:{a}}", 10, true, null);
}

test "nested lambda: global reachable" {
    try testFindReferences("a:1;f:{h:{a}}", 10, true, &.{ 0, 10 });
}

test "namespaced global" {
    try testFindReferences(".ns.f:1;.ns.f", 8, true, &.{ 0, 8 });
}

test "cursor on declaration site" {
    try testFindReferences("g:1;g+g", 0, true, &.{ 0, 4, 6 });
}

test "declaration without uses excluding declaration" {
    try testFindReferences("g:1", 0, false, &.{});
}

test "declaration without uses including declaration" {
    try testFindReferences("g:1", 0, true, &.{0});
}

test "builtin" {
    try testFindReferences("til 3", 0, true, null);
}

test "undeclared identifier" {
    try testFindReferences("f:{a:1};b", 8, true, null);
}

test "cursor on number literal" {
    try testFindReferences("g:1;g", 2, true, null);
}

test {
    std.testing.refAllDecls(@This());
}
