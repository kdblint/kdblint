const std = @import("std");
const Allocator = std.mem.Allocator;
const Color = std.zig.Color;

pub const Ast = @import("kdb/Ast.zig");
pub const Tokenizer = @import("kdb/tokenizer.zig").Tokenizer;

pub fn printAstErrorsToStderr(gpa: Allocator, tree: Ast, path: []const u8, color: Color) !void {
    _ = tree; // autofix
    _ = path; // autofix
    var wip_errors: std.zig.ErrorBundle.Wip = undefined;
    try wip_errors.init(gpa);
    defer wip_errors.deinit();

    // try putAstErrorsIntoBundle(gpa, tree, path, &wip_errors);

    var error_bundle = try wip_errors.toOwnedBundle("");
    defer error_bundle.deinit(gpa);
    error_bundle.renderToStdErr(color.renderOptions());
}

test {
    @import("std").testing.refAllDecls(@This());
}
