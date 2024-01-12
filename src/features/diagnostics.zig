const std = @import("std");
const lsp = @import("lsp");
const types = lsp.types;

const Server = @import("../Server.zig");
const DocumentStore = @import("../DocumentStore.zig");
const offsets = @import("../offsets.zig");

pub fn generateDiagnostics(server: *Server, arena: std.mem.Allocator, handle: *DocumentStore.Handle) error{OutOfMemory}!types.PublishDiagnosticsParams {
    std.debug.assert(server.client_capabilities.supports_publish_diagnostics);

    const tree = handle.tree;

    var diagnostics = std.ArrayListUnmanaged(types.Diagnostic){};
    try diagnostics.ensureUnusedCapacity(arena, tree.tokens.len);
    for (tree.tokens.items(.tag), 0..) |tag, i| {
        var buffer = std.ArrayListUnmanaged(u8){};
        try buffer.writer(arena).writeAll(tag.symbol());

        diagnostics.appendAssumeCapacity(.{
            .range = offsets.tokenToRange(tree, @intCast(i), server.position_encoding),
            .severity = .Information,
            .code = .{ .string = @tagName(tag) },
            .source = "kdblint",
            .message = try buffer.toOwnedSlice(arena),
        });
    }

    return .{
        .uri = handle.uri,
        .diagnostics = diagnostics.items,
    };
}
