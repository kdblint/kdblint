const std = @import("std");

const zls = @import("zls");
const types = zls.lsp.types;

const Analyser = @import("../analysis.zig");
const DocumentStore = @import("../DocumentStore.zig");
const Server = @import("../Server.zig");

pub fn completionAtIndex(
    server: *Server,
    analyser: *Analyser,
    arena: std.mem.Allocator,
    handle: *DocumentStore.Handle,
    source_index: usize,
) error{OutOfMemory}!?types.CompletionList {
    _ = source_index; // autofix
    _ = handle; // autofix
    _ = arena; // autofix
    _ = analyser; // autofix
    _ = server; // autofix
    return null;
}
