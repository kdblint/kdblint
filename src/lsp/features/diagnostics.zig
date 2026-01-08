const std = @import("std");
const Io = std.Io;
const assert = std.debug.assert;
const lsp = @import("lsp");
const offsets = lsp.offsets;

const Server = @import("../Server.zig");
const DocumentStore = @import("../DocumentStore.zig");

const kdb = @import("../../kdb/root.zig");
const Ast = kdb.Ast;
const TokenIndex = Ast.TokenIndex;
const ErrorBundle = kdb.ErrorBundle;

pub fn generateDiagnostics(server: *Server, handle: *DocumentStore.Handle) !void {
    assert(server.client_capabilities.supports_publish_diagnostics);

    const tokenize_ms: f64 = @as(f64, @floatFromInt(handle.tree.tokenize_duration)) / std.time.ns_per_ms;
    std.log.debug("Tokenize duration: {d:.2}ms", .{tokenize_ms});

    const parse_ms: f64 = @as(f64, @floatFromInt(handle.tree.parse_duration)) / std.time.ns_per_ms;
    std.log.debug("Parse duration: {d:.2}ms", .{parse_ms});

    if (handle.tree.errors.len == 0) {
        var error_bundle = try getAstCheckDiagnostics(server, handle);
        errdefer error_bundle.deinit(server.gpa);

        try server.diagnostics_collection.pushSingleDocumentDiagnostics(
            .parse,
            handle.uri,
            .{ .error_bundle = error_bundle },
        );
    } else {
        var wip: ErrorBundle.Wip = undefined;
        try wip.init(server.gpa);
        defer wip.deinit();

        try collectParseDiagnostics(handle.tree, &wip);

        var error_bundle = try wip.toOwnedBundle("");
        errdefer error_bundle.deinit(server.gpa);

        try server.diagnostics_collection.pushSingleDocumentDiagnostics(
            .parse,
            handle.uri,
            .{ .error_bundle = error_bundle },
        );
    }

    server.diagnostics_collection.publishDiagnostics() catch |err| {
        std.log.err("failed to publish diagnostics: {}", .{err});
    };
}

fn collectParseDiagnostics(tree: Ast, eb: *ErrorBundle.Wip) !void {
    assert(tree.errors.len != 0);

    var msg_buffer: Io.Writer.Allocating = .init(eb.gpa);
    defer msg_buffer.deinit();

    var notes: std.ArrayList(ErrorBundle.MessageIndex) = .empty;
    defer notes.deinit(eb.gpa);

    const current_error = tree.errors[0];
    for (tree.errors[1..]) |err| {
        if (!err.is_note) break;

        msg_buffer.clearRetainingCapacity();
        try tree.renderError(err, &msg_buffer.writer);
        try notes.append(eb.gpa, try eb.addErrorMessage(.{
            .msg = try eb.addString(msg_buffer.written()),
            .src_loc = try errorBundleSourceLocationFromToken(tree, eb, err.token),
            .kind = .note,
        }));
    }

    msg_buffer.clearRetainingCapacity();
    try tree.renderError(current_error, &msg_buffer.writer);
    try eb.addRootErrorMessage(.{
        .msg = try eb.addString(msg_buffer.written()),
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
    token: TokenIndex,
) !ErrorBundle.SourceLocationIndex {
    const loc = tree.tokens.items(.loc)[token];
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

fn getAstCheckDiagnostics(server: *Server, handle: *DocumentStore.Handle) !ErrorBundle {
    assert(handle.tree.errors.len == 0);
    const zir = try handle.getZir(server.gpa);

    const compile_ms: f64 = @as(f64, @floatFromInt(zir.compile_duration)) / std.time.ns_per_ms;
    std.log.debug("Compile duration: {d:.2}ms", .{compile_ms});

    if (!zir.hasCompileErrors() and !zir.hasCompileWarnings()) return .empty;

    var eb: ErrorBundle.Wip = undefined;
    try eb.init(server.gpa);
    defer eb.deinit();
    try eb.addZirErrorMessages(zir, handle.tree, handle.tree.source, "");
    return try eb.toOwnedBundle("");
}
