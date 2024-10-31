const std = @import("std");
const Allocator = std.mem.Allocator;

const tokenizer = @import("tokenizer.zig");

pub const Ast = @import("Ast.zig");
pub const AstGen = @import("AstGen.zig");
pub const ErrorBundle = @import("ErrorBundle.zig");
pub const Parse = @import("Parse.zig");
pub const render = @import("render.zig");
pub const Token = tokenizer.Token;
pub const Tokenizer = tokenizer.Tokenizer;
pub const Zir = @import("Zir.zig");

pub const fmt = @import("fmt.zig").mainArgs;

pub const Color = enum {
    /// Determine whether stderr is a terminal or not automatically.
    auto,
    /// Assume stderr is not a terminal.
    off,
    /// Assume stderr is a terminal.
    on,

    pub fn get_tty_conf(color: Color) std.io.tty.Config {
        return switch (color) {
            .auto => std.io.tty.detectConfig(std.io.getStdErr()),
            .on => .escape_codes,
            .off => .no_color,
        };
    }

    pub fn renderOptions(color: Color) ErrorBundle.RenderOptions {
        const ttyconf = get_tty_conf(color);
        return .{
            .ttyconf = ttyconf,
            .include_source_line = ttyconf != .no_color,
            .include_reference_trace = ttyconf != .no_color,
        };
    }
};

pub fn printAstErrorsToStderr(gpa: Allocator, tree: Ast, path: []const u8, color: Color) !void {
    var wip_errors: ErrorBundle.Wip = undefined;
    try wip_errors.init(gpa);
    defer wip_errors.deinit();

    try putAstErrorsIntoBundle(gpa, tree, path, &wip_errors);

    var error_bundle = try wip_errors.toOwnedBundle("");
    defer error_bundle.deinit(gpa);
    error_bundle.renderToStdErr(color.renderOptions());
}

pub fn putAstErrorsIntoBundle(
    gpa: Allocator,
    tree: Ast,
    path: []const u8,
    wip_errors: *ErrorBundle.Wip,
) Allocator.Error!void {
    var zir = try AstGen.generate(gpa, tree);
    defer zir.deinit(gpa);

    try wip_errors.addZirErrorMessages(zir, tree, tree.source, path);
}

test {
    @import("std").testing.refAllDecls(@This());
}
