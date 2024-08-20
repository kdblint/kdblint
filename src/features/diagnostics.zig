const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
const zls = @import("zls");
const types = zls.types;

const kdb = @import("../kdb.zig");
const Ast = kdb.Ast;
const LambdaVisitor = Ast.LambdaVisitor;
const Server = @import("../Server.zig");
const DocumentStore = @import("../DocumentStore.zig");
const offsets = @import("../offsets.zig");

const log = std.log.scoped(.kdblint_diagnostics);

const zero_instant = std.time.Instant{
    .timestamp = if (switch (builtin.os.tag) {
        .wasi => builtin.link_libc,
        .windows, .uefi => false,
        else => true,
    }) .{ .nsec = 0, .sec = 0 } else 0,
};

pub fn now() std.time.Instant {
    return std.time.Instant.now() catch zero_instant;
}

pub fn generateDiagnostics(server: *Server, arena: std.mem.Allocator, handle: *DocumentStore.Handle) !types.PublishDiagnosticsParams {
    assert(server.client_capabilities.supports_publish_diagnostics);

    const tree = handle.tree;

    try tree.debug(arena);

    var diagnostics = std.ArrayListUnmanaged(types.Diagnostic){};

    try diagnostics.ensureUnusedCapacity(arena, tree.errors.len);
    for (tree.errors) |err| {
        var buffer = std.ArrayListUnmanaged(u8){};
        try tree.renderError(err, buffer.writer(arena).any());

        diagnostics.appendAssumeCapacity(.{
            .range = offsets.tokenToRange(tree, err.token, server.offset_encoding),
            .severity = .Error,
            .code = .{ .string = @tagName(err.tag) },
            .source = "kdblint",
            .message = try buffer.toOwnedSlice(arena),
        });
    }

    var errors = std.ArrayList(Ast.Error).init(arena);
    defer errors.deinit();
    const visitor = try LambdaVisitor.create(server.allocator, &errors);
    defer visitor.destroy(server.allocator);
    tree.visit(visitor.any());

    try diagnostics.ensureUnusedCapacity(arena, errors.items.len);
    for (errors.items) |err| {
        var buffer = std.ArrayListUnmanaged(u8){};
        try tree.renderError(err, buffer.writer(arena).any());

        diagnostics.appendAssumeCapacity(.{
            .range = offsets.tokenToRange(tree, err.token, server.offset_encoding),
            .severity = .Error,
            .code = .{ .string = @tagName(err.tag) },
            .source = "kdblint",
            .message = try buffer.toOwnedSlice(arena),
        });
    }

    const tokenize_ms: f64 = @as(f64, @floatFromInt(tree.tokenize_duration)) / std.time.ns_per_ms;
    const parse_ms: f64 = @as(f64, @floatFromInt(tree.parse_duration)) / std.time.ns_per_ms;
    // const full_ms: f64 = @as(f64, @floatFromInt(now().since(server.start))) / std.time.ns_per_ms;

    log.debug("Tokenize duration: {d:.2}ms", .{tokenize_ms});
    log.debug("Parse duration: {d:.2}ms", .{parse_ms});
    // log.debug("Full duration: {d:.2}ms", .{full_ms});

    return .{
        .uri = handle.uri,
        .diagnostics = diagnostics.items,
    };
}

test {
    @import("std").testing.refAllDecls(@This());
}
