const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const zls = @import("zls");
const types = zls.types;
const URI = zls.URI;
const tracy = @import("tracy");

const kdb = @import("kdb");
const Ast = kdb.Ast;
const ErrorBundle = kdb.ErrorBundle;
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

pub fn generateDiagnostics(
    server: *Server,
    handle: *DocumentStore.Handle,
) error{OutOfMemory}!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const tokenize_ms: f64 = @as(f64, @floatFromInt(handle.tree.tokenize_duration)) / std.time.ns_per_ms;
    log.debug("Tokenize duration: {d:.2}ms", .{tokenize_ms});

    const parse_ms: f64 = @as(f64, @floatFromInt(handle.tree.parse_duration)) / std.time.ns_per_ms;
    log.debug("Parse duration: {d:.2}ms", .{parse_ms});

    if (handle.tree.errors.len == 0) {
        const tracy_zone2 = tracy.traceNamed(@src(), "ast-check");
        defer tracy_zone2.end();

        var error_bundle = try getAstCheckDiagnostics(server, handle);
        errdefer error_bundle.deinit(server.allocator);

        try server.diagnostics_collection.pushSingleDocumentDiagnostics(
            .parse,
            handle.uri,
            .{ .error_bundle = error_bundle },
        );
    } else {
        var wip: ErrorBundle.Wip = undefined;
        try wip.init(server.allocator);
        defer wip.deinit();

        try collectParseDiagnostics(handle.tree, &wip);

        var error_bundle = try wip.toOwnedBundle("");
        errdefer error_bundle.deinit(server.allocator);

        try server.diagnostics_collection.pushSingleDocumentDiagnostics(
            .parse,
            handle.uri,
            .{ .error_bundle = error_bundle },
        );
    }

    std.debug.assert(server.client_capabilities.supports_publish_diagnostics);
    server.diagnostics_collection.publishDiagnostics() catch |err| {
        log.err("failed to publish diagnostics: {}", .{err});
    };
}

fn collectParseDiagnostics(tree: Ast, eb: *ErrorBundle.Wip) error{OutOfMemory}!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (tree.errors.len == 0) return;

    const allocator = eb.gpa;

    var msg_buffer: std.ArrayListUnmanaged(u8) = .empty;
    defer msg_buffer.deinit(allocator);

    var notes: std.ArrayListUnmanaged(ErrorBundle.MessageIndex) = .empty;
    defer notes.deinit(allocator);

    const current_error = tree.errors[0];
    for (tree.errors[1..]) |err| {
        if (!err.is_note) break;

        msg_buffer.clearRetainingCapacity();
        try tree.renderError(err, msg_buffer.writer(allocator));
        try notes.append(allocator, try eb.addErrorMessage(.{
            .msg = try eb.addString(msg_buffer.items),
            .src_loc = try errorBundleSourceLocationFromToken(tree, eb, err.token),
            .kind = .note,
        }));
    }

    msg_buffer.clearRetainingCapacity();
    try tree.renderError(current_error, msg_buffer.writer(allocator));
    try eb.addRootErrorMessage(.{
        .msg = try eb.addString(msg_buffer.items),
        .src_loc = try errorBundleSourceLocationFromToken(tree, eb, current_error.token),
        .notes_len = @intCast(notes.items.len),
        .kind = .@"error",
    });

    const notes_start = try eb.reserveNotes(@intCast(notes.items.len));
    @memcpy(eb.extra.items[notes_start..][0..notes.items.len], @as([]const u32, @ptrCast(notes.items)));
}

fn errorBundleSourceLocationFromToken(
    tree: Ast,
    eb: *ErrorBundle.Wip,
    token: Ast.Token.Index,
) error{OutOfMemory}!ErrorBundle.SourceLocationIndex {
    const loc = offsets.tokenToLoc(tree, token);
    const pos = offsets.indexToPosition(tree.source, loc.start, .@"utf-8");
    const line = offsets.lineSliceAtIndex(tree.source, loc.start);

    return try eb.addSourceLocation(.{
        .src_path = try eb.addString(""),
        .line = pos.line,
        .column = pos.character,
        .span_start = @intCast(loc.start),
        .span_main = @intCast(loc.start),
        .span_end = @intCast(loc.end),
        .source_line = try eb.addString(line),
    });
}

/// caller owns the returned ErrorBundle
pub fn getAstCheckDiagnostics(server: *Server, handle: *DocumentStore.Handle) error{OutOfMemory}!ErrorBundle {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    std.debug.assert(handle.tree.errors.len == 0);

    const zir = try handle.getZir();

    const compile_ms: f64 = @as(f64, @floatFromInt(zir.compile_duration)) / std.time.ns_per_ms;
    log.debug("Compile duration: {d:.2}ms", .{compile_ms});

    if (!zir.hasCompileErrors() and !zir.hasCompileWarnings()) return .empty;

    var eb: ErrorBundle.Wip = undefined;
    try eb.init(server.allocator);
    defer eb.deinit();
    const src_path = URI.parse(server.allocator, handle.uri) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => unreachable,
    };
    defer server.allocator.free(src_path);
    try eb.addZirErrorMessages(zir, handle.tree, handle.tree.source, src_path);
    return try eb.toOwnedBundle("");
}
