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

/// Known limitations, shared with goto definition (and extended across
/// files for workspace globals):
/// - q-sql and table-literal column identifiers are plain identifier tokens
///   and can falsely match a variable of the same name.
/// - Name lookup is position-insensitive, so a use before the assignment
///   still matches.
pub fn findReferences(
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    document_store: *DocumentStore,
    handle: *DocumentStore.Handle,
    position: types.Position,
    include_declaration: bool,
    encoding: offsets.Encoding,
) !?[]const types.Location {
    const tree = handle.tree;
    const source_index = offsets.positionToIndex(tree.source, position, encoding);
    const ident_token = goto.identifierTokenAtIndex(tree, source_index) orelse return null;
    const name = tree.tokenSlice(ident_token);

    const doc_scope = try handle.getDocumentScope(io, gpa);
    const resolved = doc_scope.resolveName(
        doc_scope.innermostScopeAtIndex(@intCast(source_index)),
        name,
    );

    var locations: std.ArrayList(types.Location) = .empty;

    if (resolved) |target| {
        if (target.scope != .root) {
            // A local or function parameter never crosses file boundaries.
            try scanFileForLocal(arena, &locations, handle, doc_scope, target, name, include_declaration, encoding);
            return locations.items;
        }
    }

    // A root-scope declaration or an unresolved identifier is a workspace
    // global. It must be declared at the root scope of at least one file;
    // otherwise there is nothing to find references of.
    const handles = try document_store.collectHandles(arena);
    const any_declaration = for (handles) |h| {
        const ds = try h.getDocumentScope(io, gpa);
        if (ds.getScopeDeclarationChain(.{ .scope = .root, .name = name }) != null) break true;
    } else false;
    if (!any_declaration) return null;

    try scanFileForGlobal(arena, &locations, io, gpa, handle, name, include_declaration, encoding);
    for (handles) |other| {
        if (other == handle) continue;
        try scanFileForGlobal(arena, &locations, io, gpa, other, name, include_declaration, encoding);
    }
    return locations.items;
}

/// Collects tokens in `handle` that resolve to the same declaration chain as
/// `target` (a local or function parameter).
fn scanFileForLocal(
    arena: Allocator,
    locations: *std.ArrayList(types.Location),
    handle: *DocumentStore.Handle,
    doc_scope: *const DocumentScope,
    target: DocumentScope.ResolvedDeclaration,
    name: []const u8,
    include_declaration: bool,
    encoding: offsets.Encoding,
) !void {
    var decl_tokens: std.ArrayList(Ast.TokenIndex) = .empty;
    if (!include_declaration) {
        try collectChainNameTokens(arena, &decl_tokens, handle.tree, doc_scope, target.chain);
    }

    const tree = handle.tree;
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

        try appendTokenLocation(arena, locations, handle, token, encoding);
    }
}

/// Collects tokens in `handle` that refer to the workspace global `name`:
/// tokens whose own in-file resolution is either this file's root-scope
/// declaration or nothing at all (the global is defined in another file).
fn scanFileForGlobal(
    arena: Allocator,
    locations: *std.ArrayList(types.Location),
    io: Io,
    gpa: Allocator,
    handle: *DocumentStore.Handle,
    name: []const u8,
    include_declaration: bool,
    encoding: offsets.Encoding,
) !void {
    const doc_scope = try handle.getDocumentScope(io, gpa);

    var decl_tokens: std.ArrayList(Ast.TokenIndex) = .empty;
    if (!include_declaration) {
        if (doc_scope.getScopeDeclarationChain(.{ .scope = .root, .name = name })) |chain| {
            try collectChainNameTokens(arena, &decl_tokens, handle.tree, doc_scope, chain);
        }
    }

    const tree = handle.tree;
    const locs = tree.tokens.items(.loc);
    for (0..tree.tokens.len) |i| {
        const token: Ast.TokenIndex = @intCast(i);
        if (tree.tokenTag(token) != .identifier) continue;
        if (!std.mem.eql(u8, tree.tokenSlice(token), name)) continue;
        if (std.mem.findScalar(Ast.TokenIndex, decl_tokens.items, token) != null) continue;

        // Locals or parameters shadowing the global are excluded.
        if (doc_scope.resolveName(
            doc_scope.innermostScopeAtIndex(@intCast(locs[token].start)),
            name,
        )) |resolved| {
            if (resolved.scope != .root) continue;
        }

        try appendTokenLocation(arena, locations, handle, token, encoding);
    }
}

fn collectChainNameTokens(
    arena: Allocator,
    decl_tokens: *std.ArrayList(Ast.TokenIndex),
    tree: Ast,
    doc_scope: *const DocumentScope,
    chain: DocumentScope.DeclarationChain,
) !void {
    var it = doc_scope.iterateDeclarationChain(chain.first);
    while (it.next()) |decl_index| {
        const decl = doc_scope.declarations.get(@intFromEnum(decl_index));
        try decl_tokens.append(arena, decl.nameToken(tree));
    }
}

fn appendTokenLocation(
    arena: Allocator,
    locations: *std.ArrayList(types.Location),
    handle: *DocumentStore.Handle,
    token: Ast.TokenIndex,
    encoding: offsets.Encoding,
) !void {
    const loc = handle.tree.tokenLoc(token);
    try locations.append(arena, .{
        .uri = handle.uri.percent_encoded,
        .range = offsets.locToRange(
            handle.tree.source,
            .{ .start = loc.start, .end = loc.end },
            encoding,
        ),
    });
}

fn testFindReferencesMulti(
    files: []const goto.TestFile,
    cursor_offset: usize,
    include_declaration: bool,
    expected: ?[]const goto.TestLocation,
) !void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var store: DocumentStore = .{ .io = io, .gpa = gpa };
    defer store.deinit();
    const handle = try goto.testStoreWithFiles(&store, files);

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();

    const result = try findReferences(
        arena_instance.allocator(),
        io,
        gpa,
        &store,
        handle,
        offsets.indexToPosition(files[0].source, cursor_offset, .@"utf-8"),
        include_declaration,
        .@"utf-8",
    );

    const expected_locations = expected orelse {
        try std.testing.expectEqual(null, result);
        return;
    };
    const locations = result orelse return error.TestExpectedReferences;

    try std.testing.expectEqual(expected_locations.len, locations.len);
    for (locations, expected_locations) |location, expected_location| {
        try goto.expectTestLocation(files, location, expected_location);
    }
}

fn testFindReferences(
    source: [:0]const u8,
    cursor_offset: usize,
    include_declaration: bool,
    expected_offsets: ?[]const usize,
) !void {
    var expected_buf: [8]goto.TestLocation = undefined;
    const expected: ?[]const goto.TestLocation = if (expected_offsets) |off| blk: {
        for (off, 0..) |offset, i| expected_buf[i] = .{ .file = 0, .offset = offset };
        break :blk expected_buf[0..off.len];
    } else null;

    try testFindReferencesMulti(
        &.{.{ .uri = "file:///test.q", .source = source }},
        cursor_offset,
        include_declaration,
        expected,
    );
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

test "cross-file: global defined in one file, used in another" {
    // cursor in b.q, which never declares g
    try testFindReferencesMulti(&.{
        .{ .uri = "file:///b.q", .source = "g+1" },
        .{ .uri = "file:///a.q", .source = "g:1;g" },
    }, 0, true, &.{
        .{ .file = 0, .offset = 0 },
        .{ .file = 1, .offset = 0 },
        .{ .file = 1, .offset = 4 },
    });
}

test "cross-file: query from the defining file" {
    try testFindReferencesMulti(&.{
        .{ .uri = "file:///a.q", .source = "g:1;g" },
        .{ .uri = "file:///b.q", .source = "g+1" },
    }, 0, true, &.{
        .{ .file = 0, .offset = 0 },
        .{ .file = 0, .offset = 4 },
        .{ .file = 1, .offset = 0 },
    });
}

test "cross-file: excluding declarations filters every file's declarations" {
    try testFindReferencesMulti(&.{
        .{ .uri = "file:///b.q", .source = "g:2;g" },
        .{ .uri = "file:///a.q", .source = "g:1;g" },
    }, 4, false, &.{
        .{ .file = 0, .offset = 4 },
        .{ .file = 1, .offset = 4 },
    });
}

test "cross-file: lambda-local shadowing stays out of global references" {
    // b.q's lambda-local g must not appear; its top-level g must.
    try testFindReferencesMulti(&.{
        .{ .uri = "file:///a.q", .source = "g:1" },
        .{ .uri = "file:///b.q", .source = "f:{g:2;g};g" },
    }, 0, true, &.{
        .{ .file = 0, .offset = 0 },
        .{ .file = 1, .offset = 10 },
    });
}

test "cross-file: local in another file is not a reference" {
    try testFindReferencesMulti(&.{
        .{ .uri = "file:///a.q", .source = "f:{l:1;l}" },
        .{ .uri = "file:///b.q", .source = "l+1" },
    }, 3, true, &.{
        .{ .file = 0, .offset = 3 },
        .{ .file = 0, .offset = 7 },
    });
}

test "cross-file: name declared nowhere" {
    try testFindReferencesMulti(&.{
        .{ .uri = "file:///b.q", .source = "g+1" },
        .{ .uri = "file:///a.q", .source = "h:1" },
    }, 0, true, null);
}

test "cross-file: namespaced global" {
    try testFindReferencesMulti(&.{
        .{ .uri = "file:///b.q", .source = ".ns.f 1" },
        .{ .uri = "file:///a.q", .source = ".ns.f:{x}" },
    }, 0, true, &.{
        .{ .file = 0, .offset = 0 },
        .{ .file = 1, .offset = 0 },
    });
}

test "cross-file: three files keep current file first" {
    try testFindReferencesMulti(&.{
        .{ .uri = "file:///b.q", .source = "g" },
        .{ .uri = "file:///a.q", .source = "g:1" },
        .{ .uri = "file:///c.q", .source = "g+1" },
    }, 0, true, &.{
        .{ .file = 0, .offset = 0 },
        .{ .file = 1, .offset = 0 },
        .{ .file = 2, .offset = 0 },
    });
}

test {
    std.testing.refAllDecls(@This());
}
