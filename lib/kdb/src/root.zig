const std = @import("std");
const Allocator = std.mem.Allocator;
const Color = std.zig.Color;

const tokenizer = @import("tokenizer.zig");

pub const Ast = @import("Ast.zig");
pub const AstGen = @import("AstGen.zig");
pub const ErrorBundle = @import("ErrorBundle.zig");
pub const Parse = @import("Parse.zig");
pub const render = @import("render.zig");
pub const Token = tokenizer.Token;
pub const Tokenizer = tokenizer.Tokenizer;
pub const Zir = @import("Zir.zig");
pub const number_literal = @import("number_literal.zig");
pub const print_zir = @import("print_zir.zig");

// Character literal parsing
pub const parseNumberLiteral = number_literal.parseNumberLiteral;

pub const File = struct {
    source_loaded: bool = false,
    tree_loaded: bool = false,
    zir_loaded: bool = false,
    sub_file_path: []const u8,
    source: [:0]const u8,
    tree: Ast,
    zir: Zir,
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
