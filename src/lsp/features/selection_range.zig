const std = @import("std");

const zls = @import("zls");
const types = zls.lsp.types;

const DocumentStore = @import("../DocumentStore.zig");
const offsets = @import("../offsets.zig");

pub fn generateSelectionRanges(
    arena: std.mem.Allocator,
    handle: *DocumentStore.Handle,
    positions: []const types.Position,
    offset_encoding: offsets.Encoding,
) error{OutOfMemory}!?[]types.SelectionRange {
    _ = offset_encoding; // autofix
    _ = positions; // autofix
    _ = handle; // autofix
    _ = arena; // autofix
    return null;
}
