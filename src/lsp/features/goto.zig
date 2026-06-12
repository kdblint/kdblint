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
    document_store: *DocumentStore,
    handle: *DocumentStore.Handle,
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
    );

    var locations: std.ArrayList(types.Location) = .empty;

    if (resolved) |r| {
        if (r.scope != .root) {
            // A local or function parameter never crosses file boundaries.
            try appendChainLocations(arena, &locations, handle, doc_scope, r.chain, encoding);
            return resultFromLocations(locations.items);
        }
    }

    // A root-scope declaration or an unresolved identifier is a workspace
    // global: in q, load order defines globals, so every file's root-scope
    // declarations of this name are candidate definitions.
    const handles = try document_store.collectHandles(arena);
    try appendRootChainLocations(arena, &locations, io, gpa, handle, name, encoding);
    for (handles) |other| {
        if (other == handle) continue;
        try appendRootChainLocations(arena, &locations, io, gpa, other, name, encoding);
    }

    return resultFromLocations(locations.items);
}

fn resultFromLocations(locations: []const types.Location) ?types.Definition.Result {
    return switch (locations.len) {
        0 => null,
        1 => .{ .definition = .{ .location = locations[0] } },
        else => .{ .definition = .{ .locations = locations } },
    };
}

fn appendChainLocations(
    arena: Allocator,
    locations: *std.ArrayList(types.Location),
    handle: *DocumentStore.Handle,
    doc_scope: *const DocumentScope,
    chain: DocumentScope.DeclarationChain,
    encoding: offsets.Encoding,
) !void {
    var it = doc_scope.iterateDeclarationChain(chain.first);
    while (it.next()) |decl_index| {
        const decl = doc_scope.declarations.get(@intFromEnum(decl_index));
        const loc = handle.tree.tokenLoc(decl.nameToken(handle.tree));
        try locations.append(arena, .{
            .uri = handle.uri.percent_encoded,
            .range = offsets.locToRange(
                handle.tree.source,
                .{ .start = loc.start, .end = loc.end },
                encoding,
            ),
        });
    }
}

fn appendRootChainLocations(
    arena: Allocator,
    locations: *std.ArrayList(types.Location),
    io: Io,
    gpa: Allocator,
    handle: *DocumentStore.Handle,
    name: []const u8,
    encoding: offsets.Encoding,
) !void {
    const doc_scope = try handle.getDocumentScope(io, gpa);
    const chain = doc_scope.getScopeDeclarationChain(.{ .scope = .root, .name = name }) orelse return;
    try appendChainLocations(arena, locations, handle, doc_scope, chain, encoding);
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

pub const TestFile = struct { uri: []const u8, source: [:0]const u8 };
pub const TestLocation = struct { file: usize, offset: usize };

/// Builds a `DocumentStore` containing `files` and returns the handle of
/// `files[0]`, the "current" document. Shared with references.zig tests.
pub fn testStoreWithFiles(store: *DocumentStore, files: []const TestFile) !*DocumentStore.Handle {
    for (files) |file| {
        const uri = try Uri.parse(store.gpa, file.uri);
        defer uri.deinit(store.gpa);
        const source = try store.gpa.dupeSentinel(u8, file.source, 0);
        try store.loadDocumentFromSource(uri, source, .never);
    }

    const current_uri = try Uri.parse(store.gpa, files[0].uri);
    defer current_uri.deinit(store.gpa);
    return store.getHandle(current_uri).?;
}

/// Asserts that `location` points into `files[expected.file]` at byte offset
/// `expected.offset`. Shared with references.zig tests.
pub fn expectTestLocation(
    files: []const TestFile,
    location: types.Location,
    expected: TestLocation,
) !void {
    try std.testing.expectEqualStrings(files[expected.file].uri, location.uri);
    const start = offsets.positionToIndex(files[expected.file].source, location.range.start, .@"utf-8");
    try std.testing.expectEqual(expected.offset, start);
}

fn testGotoDefinitionMulti(
    files: []const TestFile,
    cursor_offset: usize,
    expected: []const TestLocation,
) !void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var store: DocumentStore = .{ .io = io, .gpa = gpa };
    defer store.deinit();
    const handle = try testStoreWithFiles(&store, files);

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();

    const result = try gotoDefinition(
        arena_instance.allocator(),
        io,
        gpa,
        &store,
        handle,
        offsets.indexToPosition(files[0].source, cursor_offset, .@"utf-8"),
        .@"utf-8",
    );

    if (expected.len == 0) {
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

    try std.testing.expectEqual(expected.len, locations.len);
    for (locations, expected) |location, expected_location| {
        try expectTestLocation(files, location, expected_location);
    }
}

fn testGotoDefinition(
    source: [:0]const u8,
    cursor_offset: usize,
    expected_decl_offsets: []const usize,
) !void {
    var expected_buf: [8]TestLocation = undefined;
    for (expected_decl_offsets, 0..) |offset, i| {
        expected_buf[i] = .{ .file = 0, .offset = offset };
    }
    try testGotoDefinitionMulti(
        &.{.{ .uri = "file:///test.q", .source = source }},
        cursor_offset,
        expected_buf[0..expected_decl_offsets.len],
    );
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

test "cross-file: global defined in another file" {
    try testGotoDefinitionMulti(&.{
        .{ .uri = "file:///b.q", .source = "g+1" },
        .{ .uri = "file:///a.q", .source = "g:1" },
    }, 0, &.{.{ .file = 1, .offset = 0 }});
}

test "cross-file: global defined in both files, current file first" {
    try testGotoDefinitionMulti(&.{
        .{ .uri = "file:///b.q", .source = "g:2;g" },
        .{ .uri = "file:///a.q", .source = "g:1" },
    }, 4, &.{ .{ .file = 0, .offset = 0 }, .{ .file = 1, .offset = 0 } });
}

test "cross-file: local in another file is not visible" {
    try testGotoDefinitionMulti(&.{
        .{ .uri = "file:///b.q", .source = "l+1" },
        .{ .uri = "file:///a.q", .source = "f:{l:1;l}" },
    }, 0, &.{});
}

test "cross-file: namespaced global" {
    try testGotoDefinitionMulti(&.{
        .{ .uri = "file:///b.q", .source = ".ns.f 1" },
        .{ .uri = "file:///a.q", .source = ".ns.f:{x}" },
    }, 0, &.{.{ .file = 1, .offset = 0 }});
}

test "cross-file: local shadowing keeps single-file behavior" {
    try testGotoDefinitionMulti(&.{
        .{ .uri = "file:///b.q", .source = "f:{g:2;g}" },
        .{ .uri = "file:///a.q", .source = "g:1" },
    }, 7, &.{.{ .file = 0, .offset = 3 }});
}

test {
    std.testing.refAllDecls(@This());
}
