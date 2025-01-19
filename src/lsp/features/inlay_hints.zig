const std = @import("std");

const kdb = @import("kdb");
const Ast = kdb.Ast;
const zls = @import("zls");
const types = zls.lsp.types;

const Analyser = @import("../analysis.zig");
const Config = @import("../Config.zig");
const DocumentStore = @import("../DocumentStore.zig");
const offsets = @import("../offsets.zig");

/// creates a list of `InlayHint`'s from the given document
/// only parameter hints are created
/// only hints in the given loc are created
pub fn writeRangeInlayHint(
    arena: std.mem.Allocator,
    config: Config,
    analyser: *Analyser,
    handle: *DocumentStore.Handle,
    loc: offsets.Loc,
    hover_kind: types.MarkupKind,
    offset_encoding: offsets.Encoding,
) error{OutOfMemory}!?[]types.InlayHint {
    _ = offset_encoding; // autofix
    _ = hover_kind; // autofix
    _ = loc; // autofix
    _ = handle; // autofix
    _ = analyser; // autofix
    _ = config; // autofix
    _ = arena; // autofix
    return null;
}
