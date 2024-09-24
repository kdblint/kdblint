const std = @import("std");
const Allocator = std.mem.Allocator;
const Color = std.zig.Color;

const tokenizer = @import("tokenizer.zig");

pub const Ast = @import("Ast.zig");
pub const Parse = @import("Parse.zig");
pub const render = @import("render.zig");
pub const Token = tokenizer.Token;
pub const Tokenizer = tokenizer.Tokenizer;

pub fn printAstErrorsToStderr(gpa: Allocator, tree: Ast, path: []const u8, color: Color) !void {
    _ = path; // autofix
    var wip_errors: std.zig.ErrorBundle.Wip = undefined;
    try wip_errors.init(gpa);
    defer wip_errors.deinit();

    const writer = std.io.getStdErr().writer().any();
    for (tree.errors) |err| {
        tree.renderError(err, writer) catch {};
    }

    // try putAstErrorsIntoBundle(gpa, tree, path, &wip_errors);

    var error_bundle = try wip_errors.toOwnedBundle("");
    defer error_bundle.deinit(gpa);
    error_bundle.renderToStdErr(color.renderOptions());
}

test {
    @import("std").testing.refAllDecls(@This());
}
