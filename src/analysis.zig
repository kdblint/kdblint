const std = @import("std");
const Ast = @import("kdb.zig").Ast;

const log = std.log.scoped(.kdblint_analysis);

/// Collects all \l system commands we can find into a slice of import paths.
pub fn collectImports(allocator: std.mem.Allocator, tree: Ast) error{OutOfMemory}!std.ArrayListUnmanaged([]const u8) {
    var imports = std.ArrayListUnmanaged([]const u8){};
    errdefer imports.deinit(allocator);

    for (tree.nodes.items(.tag), tree.nodes.items(.data)) |tag, data| {
        if (tag != .load_file_or_directory) continue;

        const str = tree.tokenSlice(data.lhs);
        try imports.append(allocator, str);
    }

    return imports;
}

test {
    @import("std").testing.refAllDecls(@This());
}
