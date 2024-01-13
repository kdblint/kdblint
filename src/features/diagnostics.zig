const std = @import("std");
const lsp = @import("lsp");
const types = lsp.types;

const kdb = @import("../kdb.zig");
const Token = kdb.Token;
const Server = @import("../Server.zig");
const DocumentStore = @import("../DocumentStore.zig");
const offsets = @import("../offsets.zig");

pub fn generateDiagnostics(server: *Server, arena: std.mem.Allocator, handle: *DocumentStore.Handle) error{OutOfMemory}!types.PublishDiagnosticsParams {
    std.debug.assert(server.client_capabilities.supports_publish_diagnostics);

    const tree = handle.tree;

    var diagnostics = std.ArrayListUnmanaged(types.Diagnostic){};
    var start: ?usize = null;
    const start_index: usize = if (tree.tokens.items(.tag)[0] == .comment) 1 else 0;
    for (tree.tokens.items(.tag)[start_index..], tree.tokens.items(.loc)[start_index..], tree.tokens.items(.eob)[start_index..]) |tag, loc, eob| {
        if (tag == .eof) break;
        if (start == null) {
            start = loc.start;
        }
        if (eob) {
            var buffer = std.ArrayListUnmanaged(u8){};
            try buffer.writer(arena).writeAll("TEST");

            const range_start = offsets.indexToPosition(tree.source, start.?, server.position_encoding);
            const range_end = offsets.indexToPosition(tree.source, loc.end, server.position_encoding);
            try diagnostics.append(arena, .{
                .range = .{
                    .start = range_start,
                    .end = range_end,
                },
                .severity = .Information,
                .code = .{ .string = @tagName(tag) },
                .source = "kdblint",
                .message = try buffer.toOwnedSlice(arena),
            });
            start = null;
        }
    }

    // try diagnostics.ensureUnusedCapacity(arena, tree.tokens.len);
    // for (tree.tokens.items(.tag), 0..) |tag, i| {
    //     var buffer = std.ArrayListUnmanaged(u8){};
    //     try buffer.writer(arena).writeAll(tag.symbol());

    //     diagnostics.appendAssumeCapacity(.{
    //         .range = offsets.tokenToRange(tree, @intCast(i), server.position_encoding),
    //         .severity = .Information,
    //         .code = .{ .string = @tagName(tag) },
    //         .source = "kdblint",
    //         .message = try buffer.toOwnedSlice(arena),
    //     });
    // }

    return .{
        .uri = handle.uri,
        .diagnostics = diagnostics.items,
    };
}
