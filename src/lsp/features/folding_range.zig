const std = @import("std");

const kdb = @import("kdb");
const Ast = kdb.Ast;
const tracy = @import("tracy");
const zls = @import("zls");
const types = zls.lsp.types;

const offsets = @import("../offsets.zig");

pub fn generateFoldingRanges(
    allocator: std.mem.Allocator,
    tree: Ast,
    encoding: offsets.Encoding,
) error{OutOfMemory}!?[]types.FoldingRange {
    _ = encoding; // autofix
    _ = tree; // autofix
    _ = allocator; // autofix
    return null;
}
