const std = @import("std");

const kdb = @import("kdb");
const Ast = kdb.Ast;
const zls = @import("zls");
const types = zls.lsp.types;

const Analyser = @import("../analysis.zig");
const Config = @import("../Config.zig");
const DocumentStore = @import("../DocumentStore.zig");
const offsets = @import("../offsets.zig");
const ast = @import("../ast.zig");

pub const InlayHint = struct {
    index: usize,
    label: []const u8,
    kind: types.InlayHintKind,
    tooltip: ?types.MarkupContent,
};

const Builder = struct {
    arena: std.mem.Allocator,
    analyser: *Analyser,
    config: *const Config,
    handle: *DocumentStore.Handle,
    hints: std.ArrayListUnmanaged(InlayHint) = .empty,
    hover_kind: types.MarkupKind,

    fn getInlayHints(self: *Builder, offset_encoding: offsets.Encoding) ![]types.InlayHint {
        const source_indices = try self.arena.alloc(usize, self.hints.items.len);
        for (source_indices, self.hints.items) |*index, hint| {
            index.* = hint.index;
        }

        const positions = try self.arena.alloc(types.Position, self.hints.items.len);

        try offsets.multiple.indexToPosition(
            self.arena,
            self.handle.tree.source,
            source_indices,
            positions,
            offset_encoding,
        );

        const converted_hints = try self.arena.alloc(types.InlayHint, self.hints.items.len);
        for (converted_hints, self.hints.items, positions) |*converted_hint, hint, position| {
            converted_hint.* = .{
                .position = position,
                .label = .{ .string = hint.label },
                .kind = hint.kind,
                .tooltip = if (hint.tooltip) |tooltip| .{ .MarkupContent = tooltip } else null,
                .paddingLeft = false,
                .paddingRight = hint.kind == .Parameter,
            };
        }

        return converted_hints;
    }
};

fn writeNodeInlayHint(builder: *Builder, tree: Ast, node: Ast.Node.Index) !void {
    _ = builder; // autofix
    _ = tree; // autofix
    _ = node; // autofix
}

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
    var builder: Builder = .{
        .arena = arena,
        .analyser = analyser,
        .config = &config,
        .handle = handle,
        .hover_kind = hover_kind,
    };

    const nodes = try ast.nodesAtLoc(arena, handle.tree, loc);

    for (nodes) |child| {
        try writeNodeInlayHint(&builder, handle.tree, child);
        try ast.iterateChildrenRecursive(handle.tree, child, &builder, error{OutOfMemory}, writeNodeInlayHint);
    }

    return try builder.getInlayHints(offset_encoding);
}
