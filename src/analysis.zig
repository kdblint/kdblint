const std = @import("std");
const Ast = @import("kdb.zig").Ast;

const log = std.log.scoped(.kdblint_analysis);

/// Collects all \l system commands we can find into a slice of import paths.
pub fn collectImports(allocator: std.mem.Allocator, tree: Ast) error{OutOfMemory}!std.ArrayListUnmanaged([]const u8) {
    var imports = std.ArrayListUnmanaged([]const u8){};
    errdefer imports.deinit(allocator);

    for (tree.nodes.items(.tag), tree.nodes.items(.data)) |tag, data| {
        if (tag != .load_file_or_directory) continue;

        const sub_range = tree.extraData(data.lhs, Ast.Node.SubRange);
        const extra_data = tree.extra_data[sub_range.start..sub_range.end];
        const str = tree.tokenSlice(extra_data[0]);
        try imports.append(allocator, str);
    }

    return imports;
}

test {
    @import("std").testing.refAllDecls(@This());
}
