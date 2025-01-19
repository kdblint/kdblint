const std = @import("std");

const zls = @import("zls");
const types = zls.lsp.types;

const Analyser = @import("../analysis.zig");
const DocumentStore = @import("../DocumentStore.zig");
const offsets = @import("../offsets.zig");

pub fn hover(
    analyser: *Analyser,
    arena: std.mem.Allocator,
    handle: *DocumentStore.Handle,
    source_index: usize,
    markup_kind: types.MarkupKind,
    offset_encoding: offsets.Encoding,
    client_name: ?[]const u8,
) !?types.Hover {
    _ = client_name; // autofix
    _ = offset_encoding; // autofix
    _ = markup_kind; // autofix
    _ = source_index; // autofix
    _ = handle; // autofix
    _ = arena; // autofix
    _ = analyser; // autofix
    return null;
}
