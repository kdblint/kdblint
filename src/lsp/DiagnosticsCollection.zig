const std = @import("std");
const Allocator = std.mem.Allocator;
const Uri = std.Uri;
const lsp = @import("lsp");

const kdb = @import("../kdb/root.zig");
const ErrorBundle = kdb.ErrorBundle;

const DiagnosticsCollection = @This();

gpa: Allocator,
mutex: std.Thread.Mutex = .{},
tag_set: std.AutoArrayHashMapUnmanaged(Tag, struct {
    version: u32 = 0,
    error_bundle_src_base_path: ?[]const u8 = null,
    /// Used to store diagnostics from `pushErrorBundle`
    error_bundle: ErrorBundle = .empty,
    /// Used to store diagnostics from `pushSingleDocumentDiagnostics`
    diagnostics_set: std.ArrayHashMapUnmanaged(Uri, struct {
        arena: std.heap.ArenaAllocator.State = .{},
        diagnostics: []lsp.types.Diagnostic = &.{},
        error_bundle: ErrorBundle = .empty,
    }, Context, true) = .empty,
}) = .empty,
outdated_files: std.ArrayHashMapUnmanaged(Uri, void, Context, true) = .empty,
transport: *lsp.Transport,
offset_encoding: lsp.offsets.Encoding = .@"utf-16",

const Context = struct {
    pub fn hash(_: Context, key: Uri) u32 {
        return std.array_hash_map.hashString(key.path.percent_encoded);
    }

    pub fn eql(_: Context, a: Uri, b: Uri, _: usize) bool {
        return std.array_hash_map.eqlString(a.path.percent_encoded, b.path.percent_encoded);
    }
};

pub const Tag = enum(u32) {
    parse,
    _,
};

pub fn pushSingleDocumentDiagnostics(
    collection: *DiagnosticsCollection,
    tag: Tag,
    document_uri: Uri,
    diagnostics: union(enum) {
        lsp: struct {
            arena: std.heap.ArenaAllocator.State,
            diagnostics: []lsp.types.Diagnostic,
        },
        error_bundle: ErrorBundle,
    },
) !void {
    collection.mutex.lock();
    defer collection.mutex.unlock();

    const gop_tag = try collection.tag_set.getOrPutValue(collection.gpa, tag, .{});

    try collection.outdated_files.put(collection.gpa, document_uri, {});

    const gop_file = try gop_tag.value_ptr.diagnostics_set.getOrPut(collection.gpa, document_uri);
    if (!gop_file.found_existing) {
        gop_file.value_ptr.* = .{};
    }
    errdefer comptime unreachable;

    switch (diagnostics) {
        .lsp => |data| {
            if (gop_file.found_existing) gop_file.value_ptr.arena.promote(collection.gpa).deinit();
            gop_file.value_ptr.arena = data.arena;
            gop_file.value_ptr.diagnostics = data.diagnostics;
        },
        .error_bundle => |error_bundle| {
            if (gop_file.found_existing) gop_file.value_ptr.error_bundle.deinit(collection.gpa);
            gop_file.value_ptr.error_bundle = error_bundle;
        },
    }
}

pub fn clearSingleDocumentDiagnostics(collection: *DiagnosticsCollection, document_uri: Uri) void {
    collection.mutex.lock();
    defer collection.mutex.unlock();

    for (collection.tag_set.values()) |*item| {
        var kv = item.diagnostics_set.fetchSwapRemove(document_uri) orelse continue;
        kv.value.arena.promote(collection.gpa).deinit();
        kv.value.error_bundle.deinit(collection.gpa);
        collection.outdated_files.put(collection.gpa, document_uri, {}) catch {};
    }
}

pub fn publishDiagnostics(collection: *DiagnosticsCollection) !void {
    var arena_allocator: std.heap.ArenaAllocator = .init(collection.gpa);
    defer arena_allocator.deinit();

    while (true) {
        const json_message = blk: {
            collection.mutex.lock();
            defer collection.mutex.unlock();

            const entry = collection.outdated_files.pop() orelse break;
            const uri = entry.key;

            _ = arena_allocator.reset(.retain_capacity);

            var diagnostics: std.ArrayList(lsp.types.Diagnostic) = .empty;
            try collection.collectLspDiagnosticsForDocument(
                uri,
                collection.offset_encoding,
                arena_allocator.allocator(),
                &diagnostics,
            );

            const notification: lsp.TypedJsonRPCNotification(lsp.types.PublishDiagnosticsParams) = .{
                .method = "textDocument/publishDiagnostics",
                .params = .{
                    .uri = uri.path.percent_encoded,
                    .diagnostics = diagnostics.items,
                },
            };

            break :blk try std.json.Stringify.valueAlloc(
                collection.gpa,
                notification,
                .{ .emit_null_optional_fields = false },
            );
        };
        defer collection.gpa.free(json_message);

        try collection.transport.writeJsonMessage(json_message);
    }
}

fn collectLspDiagnosticsForDocument(
    collection: *DiagnosticsCollection,
    document_uri: Uri,
    offset_encoding: lsp.offsets.Encoding,
    arena: Allocator,
    diagnostics: *std.ArrayList(lsp.types.Diagnostic),
) !void {
    for (collection.tag_set.values()) |entry| {
        if (entry.diagnostics_set.get(document_uri)) |per_document| {
            try diagnostics.appendSlice(arena, per_document.diagnostics);

            try convertErrorBundleToLspDiagnostics(
                per_document.error_bundle,
                null,
                document_uri,
                offset_encoding,
                arena,
                diagnostics,
                true,
            );
        }

        try convertErrorBundleToLspDiagnostics(
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

fn convertErrorBundleToLspDiagnostics(
    eb: ErrorBundle,
    error_bundle_src_base_path: ?[]const u8,
    document_uri: Uri,
    offset_encoding: lsp.offsets.Encoding,
    arena: Allocator,
    diagnostics: *std.ArrayList(lsp.types.Diagnostic),
    is_single_document: bool,
) !void {
    if (eb.errorMessageCount() == 0) return; // `getMessages` can't be called on an empty ErrorBundle
    for (eb.getMessages()) |msg_index| {
        const err = eb.getErrorMessage(msg_index);
        if (err.src_loc == .none) continue;

        const src_loc = eb.getSourceLocation(err.src_loc);
        const src_path = eb.nullTerminatedString(src_loc.src_path);

        if (!is_single_document) {
            const src_uri = try pathToUri(arena, error_bundle_src_base_path, src_path) orelse continue;
            if (!std.mem.eql(u8, document_uri.path.percent_encoded, src_uri.path.percent_encoded)) continue;
        }

        const src_range = errorBundleSourceLocationToRange(eb, src_loc, offset_encoding);

        const eb_notes = eb.getNotes(msg_index);
        const related_information = if (eb_notes.len == 0) null else blk: {
            const lsp_notes = try arena.alloc(lsp.types.DiagnosticRelatedInformation, eb_notes.len);
            for (lsp_notes, eb_notes) |*lsp_note, eb_note_index| {
                const eb_note = eb.getErrorMessage(eb_note_index);
                if (eb_note.src_loc == .none) continue;

                const note_src_loc = eb.getSourceLocation(eb_note.src_loc);
                const note_src_path = eb.nullTerminatedString(note_src_loc.src_path);
                const note_src_range = errorBundleSourceLocationToRange(eb, note_src_loc, offset_encoding);

                const note_uri: Uri = if (is_single_document)
                    document_uri
                else
                    try pathToUri(arena, error_bundle_src_base_path, note_src_path) orelse continue;

                lsp_note.* = .{
                    .location = .{
                        .uri = note_uri.path.percent_encoded,
                        .range = note_src_range,
                    },
                    .message = eb.nullTerminatedString(eb_note.msg),
                };
            }
            break :blk lsp_notes;
        };

        var tags: std.ArrayList(lsp.types.DiagnosticTag) = .empty;

        const message = eb.nullTerminatedString(err.msg);

        if (std.mem.startsWith(u8, message, "unused ")) {
            try tags.append(arena, lsp.types.DiagnosticTag.Unnecessary);
        }

        try diagnostics.append(arena, .{
            .range = src_range,
            .severity = .Error,
            .source = "kdblint",
            .message = message,
            .tags = if (tags.items.len != 0) tags.items else null,
            .relatedInformation = related_information,
        });
    }
}

fn errorBundleSourceLocationToRange(
    error_bundle: ErrorBundle,
    src_loc: ErrorBundle.SourceLocation,
    offset_encoding: lsp.offsets.Encoding,
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
    const source_line_range = lsp.offsets.convertRangeEncoding(
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

fn pathToUri(gpa: Allocator, base_path: ?[]const u8, src_path: []const u8) !?Uri {
    if (std.fs.path.isAbsolute(src_path)) {
        return try .parse(src_path);
    }
    const base = base_path orelse return null;
    const absolute_src_path = try std.fs.path.join(gpa, &.{ base, src_path });
    defer gpa.free(absolute_src_path);

    return try .parse(absolute_src_path);
}
