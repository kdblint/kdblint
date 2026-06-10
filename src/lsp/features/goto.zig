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

pub fn gotoDefinition(
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    handle: *DocumentStore.Handle,
    uri: types.DocumentUri,
    position: types.Position,
    encoding: offsets.Encoding,
) !?types.Definition.Result {
    const tree = handle.tree;
    const source_index = offsets.positionToIndex(tree.source, position, encoding);
    const ident_token = identifierTokenAtIndex(tree, source_index) orelse return null;
    const name = tree.tokenSlice(ident_token);

    const doc_scope = try handle.getDocumentScope(io, gpa);

    const resolved = doc_scope.resolveName(
        doc_scope.innermostScopeAtIndex(@intCast(source_index)),
        name,
    ) orelse return null;

    var locations: std.ArrayList(types.Location) = .empty;
    var it = doc_scope.iterateDeclarationChain(resolved.chain.first);
    while (it.next()) |decl_index| {
        const decl = doc_scope.declarations.get(@intFromEnum(decl_index));
        const name_token = decl.nameToken(tree);
        const loc = tree.tokenLoc(name_token);
        try locations.append(arena, .{
            .uri = uri,
            .range = offsets.locToRange(tree.source, .{ .start = loc.start, .end = loc.end }, encoding),
        });
    }

    return switch (locations.items.len) {
        0 => null,
        1 => .{ .definition = .{ .location = locations.items[0] } },
        else => .{ .definition = .{ .locations = locations.items } },
    };
}

/// Returns the `.identifier` token containing `source_index`, if any.
/// A cursor immediately after the identifier (`source_index == loc.end`)
/// also counts, matching typical editor behavior.
pub fn identifierTokenAtIndex(tree: Ast, source_index: usize) ?Ast.TokenIndex {
    const locs = tree.tokens.items(.loc);

    // Find the last token whose start is <= source_index.
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

    if (tree.tokenTag(token) == .identifier and source_index <= locs[token].end) {
        return token;
    }
    // A cursor immediately after an identifier may coincide with the start
    // of the next token (e.g. `a:1` with the cursor between `a` and `:`).
    if (token > 0 and tree.tokenTag(token - 1) == .identifier and locs[token - 1].end == source_index) {
        return token - 1;
    }
    return null;
}

fn testGotoDefinition(
    source: [:0]const u8,
    cursor_offset: usize,
    expected_decl_offsets: []const usize,
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

    const result = try gotoDefinition(
        arena_instance.allocator(),
        io,
        gpa,
        &handle,
        test_uri,
        offsets.indexToPosition(source, cursor_offset, .@"utf-8"),
        .@"utf-8",
    );

    if (expected_decl_offsets.len == 0) {
        try std.testing.expectEqual(null, result);
        return;
    }

    const definition = switch (result orelse return error.TestExpectedDefinition) {
        .definition => |definition| definition,
        .definition_links => unreachable,
    };
    var single: [1]types.Location = undefined;
    const locations: []const types.Location = switch (definition) {
        .location => |location| blk: {
            single[0] = location;
            break :blk &single;
        },
        .locations => |locations| locations,
    };

    try std.testing.expectEqual(expected_decl_offsets.len, locations.len);
    for (locations, expected_decl_offsets) |location, expected_offset| {
        try std.testing.expectEqualStrings(test_uri, location.uri);
        const start = offsets.positionToIndex(source, location.range.start, .@"utf-8");
        try std.testing.expectEqual(expected_offset, start);
    }
}

test "local variable" {
    try testGotoDefinition("f:{a:1;a+1}", 7, &.{3});
}

test "local variable re-assignment" {
    try testGotoDefinition("f:{a:1;a:2;a}", 11, &.{ 3, 7 });
}

test "function parameter" {
    try testGotoDefinition("{[p]p+1}", 4, &.{2});
}

test "implicit function parameter" {
    try testGotoDefinition("{x+x}", 3, &.{1});
}

test "global variable" {
    try testGotoDefinition("g:1;g", 4, &.{0});
}

test "global variable re-declaration" {
    try testGotoDefinition("g:1;g:2;g", 8, &.{ 0, 4 });
}

test "global variable assigned in lambda" {
    try testGotoDefinition("f:{g::2};g", 9, &.{3});
}

test "namespaced global" {
    try testGotoDefinition(".ns.f:1;.ns.f", 8, &.{0});
}

test "cursor on declaration site" {
    try testGotoDefinition("g:1;g", 0, &.{0});
}

test "cursor immediately after identifier" {
    try testGotoDefinition("g:1;g+1", 5, &.{0});
}

test "local shadows global" {
    try testGotoDefinition("g:1;f:{g:2;g}", 11, &.{7});
}

test "nested lambda does not capture outer local" {
    try testGotoDefinition("f:{a:1;h:{a}}", 10, &.{});
}

test "global resolved from nested lambda" {
    try testGotoDefinition("a:1;f:{h:{a}}", 10, &.{0});
}

test "global resolved from inside lambda" {
    try testGotoDefinition("g:1;f:{g+1}", 7, &.{0});
}

test "undeclared identifier" {
    try testGotoDefinition("f:{a:1};b", 8, &.{});
}

test "cursor on number literal" {
    try testGotoDefinition("g:1;g", 2, &.{});
}

test {
    std.testing.refAllDecls(@This());
}
