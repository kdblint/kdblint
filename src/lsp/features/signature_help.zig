const std = @import("std");

const zls = @import("zls");
const types = zls.lsp.types;

const Analyser = @import("../analysis.zig");
const DocumentStore = @import("../DocumentStore.zig");

pub fn getSignatureInfo(
    analyser: *Analyser,
    arena: std.mem.Allocator,
    handle: *DocumentStore.Handle,
    absolute_index: usize,
    markup_kind: types.MarkupKind,
) !?types.SignatureInformation {
    _ = markup_kind; // autofix
    _ = absolute_index; // autofix
    _ = handle; // autofix
    _ = arena; // autofix
    _ = analyser; // autofix
    return null;
}
