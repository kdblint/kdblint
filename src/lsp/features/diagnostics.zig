const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const zls = @import("zls");
const types = zls.types;
const URI = zls.URI;

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
    arena: std.mem.Allocator,
    handle: *DocumentStore.Handle,
) !types.PublishDiagnosticsParams {
    assert(server.client_capabilities.supports_publish_diagnostics);

    const tree = handle.tree;

    var diagnostics: std.ArrayListUnmanaged(types.Diagnostic) = .empty;

    try diagnostics.ensureUnusedCapacity(arena, tree.errors.len);
    for (tree.errors) |err| {
        var buffer: std.ArrayListUnmanaged(u8) = .empty;
        try tree.renderError(err, buffer.writer(arena));

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

    log.debug("Tokenize duration: {d:.2}ms", .{tokenize_ms});
    log.debug("Parse duration: {d:.2}ms", .{parse_ms});

    if (tree.errors.len == 0) {
        var error_bundle = try getAstCheckDiagnostics(server, handle);
        defer error_bundle.deinit(server.allocator);
        var diagnostics_set: std.StringArrayHashMapUnmanaged(std.ArrayListUnmanaged(types.Diagnostic)) = .empty;
        try errorBundleToDiagnostics(error_bundle, arena, &diagnostics_set, "ast-check", server.offset_encoding);
        switch (diagnostics_set.count()) {
            0 => {},
            1 => try diagnostics.appendSlice(arena, diagnostics_set.values()[0].items),
            else => unreachable, // ast-check diagnostics only affect a single file
        }
    }

    return .{
        .uri = handle.uri,
        .diagnostics = diagnostics.items,
    };
}

fn getAstCheckDiagnostics(server: *Server, handle: *DocumentStore.Handle) !ErrorBundle {
    assert(handle.tree.errors.len == 0);

    var eb: ErrorBundle.Wip = undefined;
    try eb.init(server.allocator);
    defer eb.deinit();

    const zir = try handle.getZir();
    assert(handle.getZirStatus() == .done);

    const compile_ms: f64 = @as(f64, @floatFromInt(zir.compile_duration)) / std.time.ns_per_ms;
    log.debug("Compile duration: {d:.2}ms", .{compile_ms});

    if (zir.hasCompileErrors() or zir.hasCompileWarnings()) {
        const src_path = try URI.parse(server.allocator, handle.uri);
        defer server.allocator.free(src_path);
        try eb.addZirErrorMessages(zir, handle.tree, handle.tree.source, src_path);
    }

    return eb.toOwnedBundle("");
}

fn errorBundleToDiagnostics(
    error_bundle: ErrorBundle,
    arena: Allocator,
    diagnostics: *std.StringArrayHashMapUnmanaged(std.ArrayListUnmanaged(types.Diagnostic)),
    code: []const u8,
    offset_encoding: offsets.Encoding,
) !void {
    if (error_bundle.extra.len == 0) return;
    for (error_bundle.getMessages()) |msg_index| {
        const err = error_bundle.getErrorMessage(msg_index);
        if (err.src_loc == .none) continue;

        const src_loc = error_bundle.getSourceLocation(err.src_loc);
        const src_path = error_bundle.nullTerminatedString(src_loc.src_path);
        const src_range = errorBundleSourceLocationToRange(
            error_bundle,
            src_loc,
            offset_encoding,
        );

        const eb_notes = error_bundle.getNotes(msg_index);
        const lsp_notes = try arena.alloc(
            types.DiagnosticRelatedInformation,
            eb_notes.len,
        );
        for (lsp_notes, eb_notes) |*lsp_note, eb_note_index| {
            const eb_note = error_bundle.getErrorMessage(eb_note_index);
            if (eb_note.src_loc == .none) continue;

            const note_src_loc = error_bundle.getSourceLocation(eb_note.src_loc);
            const note_src_path = error_bundle.nullTerminatedString(note_src_loc.src_path);
            const note_src_range = errorBundleSourceLocationToRange(
                error_bundle,
                note_src_loc,
                offset_encoding,
            );

            lsp_note.* = .{
                .location = .{
                    .uri = try URI.fromPath(arena, note_src_path),
                    .range = note_src_range,
                },
                .message = try arena.dupe(u8, error_bundle.nullTerminatedString(eb_note.msg)),
            };
        }

        const uri = try URI.fromPath(arena, src_path);

        const gop = try diagnostics.getOrPutValue(arena, uri, .{});
        try gop.value_ptr.append(arena, .{
            .range = src_range,
            .severity = switch (err.kind) {
                .@"error" => .Error,
                else => .Warning,
            },
            .code = .{ .string = code },
            .source = "kdblint",
            .message = try arena.dupe(u8, error_bundle.nullTerminatedString(err.msg)),
            .relatedInformation = lsp_notes,
        });
    }
}

fn errorBundleSourceLocationToRange(
    error_bundle: ErrorBundle,
    src_loc: ErrorBundle.SourceLocation,
    offset_encoding: offsets.Encoding,
) types.Range {
    const source_line = error_bundle.nullTerminatedString(src_loc.source_line);

    const source_line_range_utf8: types.Range = .{
        .start = .{
            .line = 0,
            .character = src_loc.column - (src_loc.span_main - src_loc.span_start),
        },
        .end = .{ .line = 0, .character = src_loc.column + (src_loc.span_end - src_loc.span_main) },
    };
    const source_line_range = offsets.convertRangeEncoding(
        source_line,
        source_line_range_utf8,
        .@"utf-8",
        offset_encoding,
    );

    return .{
        .start = .{ .line = src_loc.line, .character = source_line_range.start.character },
        .end = .{ .line = src_loc.line, .character = source_line_range.end.character },
    };
}

test {
    @import("std").testing.refAllDecls(@This());
}
