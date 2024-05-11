const std = @import("std");
const Ast = @import("kdb.zig").Ast;

/// Collects all `@import`'s we can find into a slice of import paths (without quotes).
pub fn collectImports(allocator: std.mem.Allocator, tree: Ast) error{OutOfMemory}!std.ArrayListUnmanaged([]const u8) {
    _ = tree; // autofix
    var imports = std.ArrayListUnmanaged([]const u8){};
    errdefer imports.deinit(allocator);

    // const tags = tree.tokens.items(.tag);

    // var i: usize = 0;
    // while (i < tags.len) : (i += 1) {
    //     if (tags[i] != .builtin)
    //         continue;
    //     const text = tree.tokenSlice(@intCast(i));

    //     if (std.mem.eql(u8, text, "@import")) {
    //         if (i + 3 >= tags.len)
    //             break;
    //         if (tags[i + 1] != .l_paren)
    //             continue;
    //         if (tags[i + 2] != .string_literal)
    //             continue;
    //         if (tags[i + 3] != .r_paren)
    //             continue;

    //         const str = tree.tokenSlice(@as(u32, @intCast(i + 2)));
    //         try imports.append(allocator, str[1 .. str.len - 1]);
    //     }
    // }

    return imports;
}

test {
    @import("std").testing.refAllDecls(@This());
}
