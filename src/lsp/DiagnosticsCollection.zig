const std = @import("std");

const kdb = @import("kdb");
const ErrorBundle = kdb.ErrorBundle;
const zls = @import("zls");
const lsp = zls.lsp;
const URI = zls.URI;

const offsets = @import("offsets.zig");

allocator: std.mem.Allocator,
mutex: std.Thread.Mutex = .{},
tag_set: std.AutoArrayHashMapUnmanaged(Tag, struct {
    version: u32 = 0,
    error_bundle_src_base_path: ?[]const u8 = null,
    /// Used to store diagnostics from `pushErrorBundle`
    error_bundle: ErrorBundle = .empty,
    /// Used to store diagnostics from `pushSingleDocumentDiagnostics`
    diagnostics_set: std.StringArrayHashMapUnmanaged(struct {
        arena: std.heap.ArenaAllocator.State = .{},
        diagnostics: []lsp.types.Diagnostic = &.{},
        error_bundle: ErrorBundle = .empty,
    }) = .empty,
}) = .empty,
outdated_files: std.StringArrayHashMapUnmanaged(void) = .empty,
transport: ?lsp.AnyTransport = null,
offset_encoding: offsets.Encoding = .@"utf-16",

const DiagnosticsCollection = @This();

/// Diangostics with different tags are treated independently.
/// This enables the DiagnosticsCollection to differentiate syntax level errors from build-on-save errors.
/// Build on Save diagnostics have an tag that is the hash of the build step and the path to the `build.zig`
pub const Tag = enum(u32) {
    /// * `Ast.parse`
    /// * ast-check
    /// * warn_style
    parse,
    /// errors from `@cImport`
    cimport,
    _,
};

pub fn deinit(collection: *DiagnosticsCollection) void {
    for (collection.tag_set.values()) |*entry| {
        entry.error_bundle.deinit(collection.allocator);
        if (entry.error_bundle_src_base_path) |src_path| collection.allocator.free(src_path);
        for (entry.diagnostics_set.keys(), entry.diagnostics_set.values()) |uri, *lsp_diagnostic| {
            collection.allocator.free(uri);
            lsp_diagnostic.arena.promote(collection.allocator).deinit();
            lsp_diagnostic.error_bundle.deinit(collection.allocator);
        }
        entry.diagnostics_set.deinit(collection.allocator);
    }
    collection.tag_set.deinit(collection.allocator);
    for (collection.outdated_files.keys()) |uri| collection.allocator.free(uri);
    collection.outdated_files.deinit(collection.allocator);
    collection.* = undefined;
}

pub fn pushSingleDocumentDiagnostics(
    collection: *DiagnosticsCollection,
    tag: Tag,
    document_uri: []const u8,
    /// LSP and ErrorBundle will not override each other.
    ///
    /// Takes ownership on success.
    diagnostics: union(enum) {
        lsp: struct {
            arena: std.heap.ArenaAllocator.State,
            diagnostics: []lsp.types.Diagnostic,
        },
        error_bundle: ErrorBundle,
    },
) error{OutOfMemory}!void {
    collection.mutex.lock();
    defer collection.mutex.unlock();

    const gop_tag = try collection.tag_set.getOrPutValue(collection.allocator, tag, .{});

    {
        try collection.outdated_files.ensureUnusedCapacity(collection.allocator, 1);
        const duped_uri = try collection.allocator.dupe(u8, document_uri);
        if (collection.outdated_files.fetchPutAssumeCapacity(duped_uri, {})) |_| collection.allocator.free(duped_uri);
    }

    try gop_tag.value_ptr.diagnostics_set.ensureUnusedCapacity(collection.allocator, 1);
    const duped_uri = try collection.allocator.dupe(u8, document_uri);
    const gop_file = gop_tag.value_ptr.diagnostics_set.getOrPutAssumeCapacity(duped_uri);
    if (gop_file.found_existing) {
        collection.allocator.free(duped_uri);
    } else {
        gop_file.value_ptr.* = .{};
    }

    errdefer comptime unreachable;

    switch (diagnostics) {
        .lsp => |data| {
            if (gop_file.found_existing) gop_file.value_ptr.arena.promote(collection.allocator).deinit();
            gop_file.value_ptr.arena = data.arena;
            gop_file.value_ptr.diagnostics = data.diagnostics;
        },
        .error_bundle => |error_bundle| {
            if (gop_file.found_existing) gop_file.value_ptr.error_bundle.deinit(collection.allocator);
            gop_file.value_ptr.error_bundle = error_bundle;
        },
    }
}

pub fn pushErrorBundle(
    collection: *DiagnosticsCollection,
    /// All changes will affect diagnostics with the same tag.
    tag: Tag,
    /// * If the `version` is greater than the old version, all diagnostics get removed and the errors from `error_bundle` get added and the `version` is updated.
    /// * If the `version` is equal   to   the old version, the errors from `error_bundle` get added.
    /// * If the `version` is less    than the old version, the errors from `error_bundle` are ignored.
    version: u32,
    /// Used to resolve relative `ErrorBundle.SourceLocation.src_path`
    ///
    /// The current implementation assumes that the base path is always the same for the same tag.
    src_base_path: ?[]const u8,
    error_bundle: ErrorBundle,
) error{OutOfMemory}!void {
    var new_error_bundle: ErrorBundle.Wip = undefined;
    try new_error_bundle.init(collection.allocator);
    defer new_error_bundle.deinit();

    collection.mutex.lock();
    defer collection.mutex.unlock();

    const gop = try collection.tag_set.getOrPutValue(collection.allocator, tag, .{});
    const version_order = std.math.order(version, gop.value_ptr.version);

    switch (version_order) {
        .lt => return, // Ignore outdated diagnostics
        .eq => {},
        .gt => gop.value_ptr.version = version,
    }

    if (error_bundle.errorMessageCount() == 0 and gop.value_ptr.error_bundle.errorMessageCount() == 0) return;

    try collectUrisFromErrorBundle(collection.allocator, error_bundle, src_base_path, &collection.outdated_files);
    if (error_bundle.errorMessageCount() != 0) {
        try new_error_bundle.addBundleAsRoots(error_bundle);
    }

    if (version_order == .gt) {
        try collectUrisFromErrorBundle(collection.allocator, gop.value_ptr.error_bundle, src_base_path, &collection.outdated_files);
    } else {
        if (gop.value_ptr.error_bundle.errorMessageCount() != 0) {
            try new_error_bundle.addBundleAsRoots(gop.value_ptr.error_bundle);
        }
    }

    var owned_error_bundle = try new_error_bundle.toOwnedBundle("");
    errdefer owned_error_bundle.deinit(collection.allocator);

    const duped_error_bundle_src_base_path = if (src_base_path) |base_path| try collection.allocator.dupe(u8, base_path) else null;
    errdefer if (duped_error_bundle_src_base_path) |base_path| collection.allocator.free(base_path);

    errdefer comptime unreachable;

    gop.value_ptr.error_bundle.deinit(collection.allocator);
    gop.value_ptr.error_bundle = owned_error_bundle;

    if (duped_error_bundle_src_base_path) |base_path| {
        if (gop.value_ptr.error_bundle_src_base_path) |old_base_path| {
            collection.allocator.free(old_base_path);
            gop.value_ptr.error_bundle_src_base_path = null;
        }
        gop.value_ptr.error_bundle_src_base_path = base_path;
    }
}

pub fn clearErrorBundle(collection: *DiagnosticsCollection, tag: Tag) void {
    collection.mutex.lock();
    defer collection.mutex.unlock();

    const item = collection.tag_set.getPtr(tag) orelse return;

    collectUrisFromErrorBundle(
        collection.allocator,
        item.error_bundle,
        item.error_bundle_src_base_path,
        &collection.outdated_files,
    ) catch |err| switch (err) {
        error.OutOfMemory => return,
    };

    if (item.error_bundle_src_base_path) |base_path| {
        collection.allocator.free(base_path);
        item.error_bundle_src_base_path = null;
    }
    item.error_bundle.deinit(collection.allocator);
    item.error_bundle = .empty;
}

fn collectUrisFromErrorBundle(
    allocator: std.mem.Allocator,
    error_bundle: ErrorBundle,
    src_base_path: ?[]const u8,
    uri_set: *std.StringArrayHashMapUnmanaged(void),
) error{OutOfMemory}!void {
    if (error_bundle.errorMessageCount() == 0) return;
    for (error_bundle.getMessages()) |msg_index| {
        const err = error_bundle.getErrorMessage(msg_index);
        if (err.src_loc == .none) continue;
        const src_loc = error_bundle.getSourceLocation(err.src_loc);
        const src_path = error_bundle.nullTerminatedString(src_loc.src_path);

        try uri_set.ensureUnusedCapacity(allocator, 1);
        const uri = try pathToUri(allocator, src_base_path, src_path) orelse continue;
        if (uri_set.fetchPutAssumeCapacity(uri, {})) |_| {
            allocator.free(uri);
        }
    }
}

fn pathToUri(allocator: std.mem.Allocator, base_path: ?[]const u8, src_path: []const u8) error{OutOfMemory}!?[]const u8 {
    if (std.fs.path.isAbsolute(src_path)) {
        return try URI.fromPath(allocator, src_path);
    }
    const base = base_path orelse return null;
    const absolute_src_path = try std.fs.path.join(allocator, &.{ base, src_path });
    defer allocator.free(absolute_src_path);

    return try URI.fromPath(allocator, absolute_src_path);
}

pub fn publishDiagnostics(collection: *DiagnosticsCollection) (std.mem.Allocator.Error || lsp.AnyTransport.WriteError)!void {
    const transport = collection.transport orelse return;

    var arena_allocator: std.heap.ArenaAllocator = .init(collection.allocator);
    defer arena_allocator.deinit();

    while (true) {
        const json_message = blk: {
            collection.mutex.lock();
            defer collection.mutex.unlock();

            const entry = collection.outdated_files.popOrNull() orelse break;
            defer collection.allocator.free(entry.key);
            const document_uri = entry.key;

            _ = arena_allocator.reset(.retain_capacity);

            var diagnostics: std.ArrayListUnmanaged(lsp.types.Diagnostic) = .empty;
            try collection.collectLspDiagnosticsForDocument(document_uri, collection.offset_encoding, arena_allocator.allocator(), &diagnostics);

            const notification: lsp.TypedJsonRPCNotification(lsp.types.PublishDiagnosticsParams) = .{
                .method = "textDocument/publishDiagnostics",
                .params = .{
                    .uri = document_uri,
                    .diagnostics = diagnostics.items,
                },
            };

            // TODO make the diagnostics serializable without requiring the mutex to be locked
            break :blk try std.json.stringifyAlloc(collection.allocator, notification, .{ .emit_null_optional_fields = false });
        };
        defer collection.allocator.free(json_message);

        try transport.writeJsonMessage(json_message);
    }
}

fn collectLspDiagnosticsForDocument(
    collection: *DiagnosticsCollection,
    document_uri: []const u8,
    offset_encoding: offsets.Encoding,
    arena: std.mem.Allocator,
    diagnostics: *std.ArrayListUnmanaged(lsp.types.Diagnostic),
) error{OutOfMemory}!void {
    for (collection.tag_set.values()) |entry| {
        if (entry.diagnostics_set.get(document_uri)) |per_document| {
            try diagnostics.appendSlice(arena, per_document.diagnostics);

            try convertErrorBundleToLSPDiangostics(
                per_document.error_bundle,
                null,
                document_uri,
                offset_encoding,
                arena,
                diagnostics,
                true,
            );
        }

        try convertErrorBundleToLSPDiangostics(
            entry.error_bundle,
            entry.error_bundle_src_base_path,
            document_uri,
            offset_encoding,
            arena,
            diagnostics,
            false,
        );
    }
}

fn convertErrorBundleToLSPDiangostics(
    eb: ErrorBundle,
    error_bundle_src_base_path: ?[]const u8,
    document_uri: []const u8,
    offset_encoding: offsets.Encoding,
    arena: std.mem.Allocator,
    diagnostics: *std.ArrayListUnmanaged(lsp.types.Diagnostic),
    is_single_document: bool,
) error{OutOfMemory}!void {
    if (eb.errorMessageCount() == 0) return; // `getMessages` can't be called on an empty ErrorBundle
    for (eb.getMessages()) |msg_index| {
        const err = eb.getErrorMessage(msg_index);
        if (err.src_loc == .none) continue;

        const src_loc = eb.getSourceLocation(err.src_loc);
        const src_path = eb.nullTerminatedString(src_loc.src_path);

        if (!is_single_document) {
            const uri = try pathToUri(arena, error_bundle_src_base_path, src_path) orelse continue;
            if (!std.mem.eql(u8, document_uri, uri)) continue;
        }

        const src_range = errorBundleSourceLocationToRange(eb, src_loc, offset_encoding);

        const eb_notes = eb.getNotes(msg_index);
        const relatedInformation = if (eb_notes.len == 0) null else blk: {
            const lsp_notes = try arena.alloc(lsp.types.DiagnosticRelatedInformation, eb_notes.len);
            for (lsp_notes, eb_notes) |*lsp_note, eb_note_index| {
                const eb_note = eb.getErrorMessage(eb_note_index);
                if (eb_note.src_loc == .none) continue;

                const note_src_loc = eb.getSourceLocation(eb_note.src_loc);
                const note_src_path = eb.nullTerminatedString(note_src_loc.src_path);
                const note_src_range = errorBundleSourceLocationToRange(eb, note_src_loc, offset_encoding);

                const note_uri = if (is_single_document)
                    document_uri
                else
                    try pathToUri(arena, error_bundle_src_base_path, note_src_path) orelse continue;

                lsp_note.* = .{
                    .location = .{
                        .uri = note_uri,
                        .range = note_src_range,
                    },
                    .message = eb.nullTerminatedString(eb_note.msg),
                };
            }
            break :blk lsp_notes;
        };

        try diagnostics.append(arena, .{
            .range = src_range,
            .severity = switch (err.kind) {
                .@"error" => .Error,
                .warn => .Warning,
                else => unreachable,
            },
            .source = "kdblint",
            .message = eb.nullTerminatedString(err.msg),
            .relatedInformation = relatedInformation,
        });
    }
}

fn errorBundleSourceLocationToRange(
    error_bundle: ErrorBundle,
    src_loc: ErrorBundle.SourceLocation,
    offset_encoding: offsets.Encoding,
) lsp.types.Range {
    // We assume that the span is inside of the source line
    const source_line_range_utf8: lsp.types.Range = .{
        .start = .{ .line = 0, .character = src_loc.column - (src_loc.span_main - src_loc.span_start) },
        .end = .{ .line = 0, .character = src_loc.column + (src_loc.span_end - src_loc.span_main) },
    };

    if (src_loc.source_line == 0) {
        // Without the source line it is not possible to figure out the precise character value
        // The result will be incorrect if the line contains non-ascii characters
        return .{
            .start = .{ .line = src_loc.line, .character = source_line_range_utf8.start.character },
            .end = .{ .line = src_loc.line, .character = source_line_range_utf8.end.character },
        };
    }

    const source_line = error_bundle.nullTerminatedString(src_loc.source_line);
    const source_line_range = offsets.convertRangeEncoding(source_line, source_line_range_utf8, .@"utf-8", offset_encoding);

    return .{
        .start = .{ .line = src_loc.line, .character = source_line_range.start.character },
        .end = .{ .line = src_loc.line, .character = source_line_range.end.character },
    };
}
