const std = @import("std");
const lsp = @import("lsp");
const types = lsp.types;

const kdb = @import("../kdb.zig");
const Token = kdb.Token;
const Server = @import("../Server.zig");
const DocumentStore = @import("../DocumentStore.zig");
const offsets = @import("../offsets.zig");

const log = std.log.scoped(.kdbLint_diagnostics);

pub fn generateDiagnostics(server: *Server, arena: std.mem.Allocator, handle: *DocumentStore.Handle) error{OutOfMemory}!types.PublishDiagnosticsParams {
    std.debug.assert(server.client_capabilities.supports_publish_diagnostics);

    const tree = handle.tree;

    try tree.visit(arena);

    var diagnostics = std.ArrayListUnmanaged(types.Diagnostic){};
    try diagnostics.ensureUnusedCapacity(arena, tree.errors.len);
    for (tree.errors) |err| {
        var buffer = std.ArrayListUnmanaged(u8){};
        try buffer.writer(arena).writeAll(if (err.tag == .expected_token) @tagName(err.extra.expected_tag) else "TEST");

        diagnostics.appendAssumeCapacity(.{
            .range = offsets.tokenToRange(tree, err.token, server.position_encoding),
            .severity = .Error,
            .code = .{ .string = @tagName(err.tag) },
            .source = "kdblint",
            .message = try buffer.toOwnedSlice(arena),
        });
    }

    return .{
        .uri = handle.uri,
        .diagnostics = diagnostics.items,
    };
}
