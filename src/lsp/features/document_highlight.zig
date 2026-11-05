const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const lsp = @import("lsp");
const types = lsp.types;
const offsets = lsp.offsets;

const DocumentStore = @import("../DocumentStore.zig");
const goto = @import("goto.zig");
const references = @import("references.zig");

/// Highlights every token in `handle` that refers to the identifier under the
/// cursor. Declaration sites are reported as writes, everything else as reads.
///
/// Returns null when there is nothing to highlight so that the client can fall
/// back to its own textual highlighting (builtins, undeclared names, cursor
/// not on an identifier).
pub fn documentHighlight(
    arena: Allocator,
    io: Io,
    gpa: Allocator,
    document_store: *DocumentStore,
    handle: *DocumentStore.Handle,
    position: types.Position,
    encoding: offsets.Encoding,
) !?[]const types.DocumentHighlight {
    const source_index = offsets.positionToIndex(handle.tree.source, position, encoding);
    const refs = try references.collectReferences(
        arena,
        io,
        gpa,
        document_store,
        handle,
        source_index,
        .current_file,
    ) orelse return null;

    var highlights: std.ArrayList(types.DocumentHighlight) = try .initCapacity(arena, refs.len);
    for (refs) |ref| {
        assert(ref.handle == handle);
        highlights.appendAssumeCapacity(.{
            .range = references.tokenRange(handle, ref.token, encoding),
            .kind = if (ref.is_declaration) .Write else .Read,
        });
    }
    return highlights.items;
}

const TestHighlight = struct { offset: usize, kind: types.DocumentHighlight.Kind };

fn testDocumentHighlightMulti(
    files: []const goto.TestFile,
    cursor_offset: usize,
    expected: ?[]const TestHighlight,
) !void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var store: DocumentStore = .{ .io = io, .gpa = gpa };
    defer store.deinit();
    const handle = try goto.testStoreWithFiles(&store, files);

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();

    const result = try documentHighlight(
        arena_instance.allocator(),
        io,
        gpa,
        &store,
        handle,
        offsets.indexToPosition(files[0].source, cursor_offset, .@"utf-8"),
        .@"utf-8",
    );

    const expected_highlights = expected orelse {
        try std.testing.expectEqual(null, result);
        return;
    };
    const highlights = result orelse return error.TestExpectedHighlights;

    try std.testing.expectEqual(expected_highlights.len, highlights.len);
    for (highlights, expected_highlights) |highlight, expected_highlight| {
        const start = offsets.positionToIndex(files[0].source, highlight.range.start, .@"utf-8");
        try std.testing.expectEqual(expected_highlight.offset, start);
        try std.testing.expectEqual(expected_highlight.kind, highlight.kind.?);
    }
}

fn testDocumentHighlight(
    source: [:0]const u8,
    cursor_offset: usize,
    expected: ?[]const TestHighlight,
) !void {
    try testDocumentHighlightMulti(
        &.{.{ .uri = "file:///test.q", .source = source }},
        cursor_offset,
        expected,
    );
}

test "global: declaration is a write, uses are reads" {
    try testDocumentHighlight("g:1;f:{g+1};g", 0, &.{
        .{ .offset = 0, .kind = .Write },
        .{ .offset = 7, .kind = .Read },
        .{ .offset = 12, .kind = .Read },
    });
}

test "redeclared global" {
    try testDocumentHighlight("g:1;g:2;g", 8, &.{
        .{ .offset = 0, .kind = .Write },
        .{ .offset = 4, .kind = .Write },
        .{ .offset = 8, .kind = .Read },
    });
}

test "namespaced global with cursor inside the dotted name" {
    // cursor between `n` and `s` of the second `.ns.f`
    try testDocumentHighlight(".ns.f:1;.ns.f", 10, &.{
        .{ .offset = 0, .kind = .Write },
        .{ .offset = 8, .kind = .Read },
    });
}

test "namespaced global does not match a different namespace" {
    try testDocumentHighlight(".a.f:1;.b.f:2;.a.f;.b.f", 14, &.{
        .{ .offset = 0, .kind = .Write },
        .{ .offset = 14, .kind = .Read },
    });
}

test "local" {
    try testDocumentHighlight("f:{a:1;a+a}", 7, &.{
        .{ .offset = 3, .kind = .Write },
        .{ .offset = 7, .kind = .Read },
        .{ .offset = 9, .kind = .Read },
    });
}

test "implicit function parameter" {
    // The first use of an implicit parameter doubles as its declaration site.
    try testDocumentHighlight("{x+x}", 3, &.{
        .{ .offset = 1, .kind = .Write },
        .{ .offset = 3, .kind = .Read },
    });
}

test "shadowing: query on outer global" {
    try testDocumentHighlight("g:1;f:{g:2;g};g", 0, &.{
        .{ .offset = 0, .kind = .Write },
        .{ .offset = 14, .kind = .Read },
    });
}

test "builtin" {
    try testDocumentHighlight("til 3", 0, null);
}

test "undeclared identifier" {
    try testDocumentHighlight("f:{a:1};b", 8, null);
}

test "cursor on number literal" {
    try testDocumentHighlight("g:1;g", 2, null);
}

test "cross-file: only the current file is highlighted" {
    try testDocumentHighlightMulti(&.{
        .{ .uri = "file:///b.q", .source = "g+1;g" },
        .{ .uri = "file:///a.q", .source = "g:1;g" },
    }, 0, &.{
        .{ .offset = 0, .kind = .Read },
        .{ .offset = 4, .kind = .Read },
    });
}

test "cross-file: namespaced global declared elsewhere" {
    try testDocumentHighlightMulti(&.{
        .{ .uri = "file:///b.q", .source = ".ns.f 1" },
        .{ .uri = "file:///a.q", .source = ".ns.f:{x}" },
    }, 2, &.{
        .{ .offset = 0, .kind = .Read },
    });
}

test {
    std.testing.refAllDecls(@This());
}
