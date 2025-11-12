const std = @import("std");
const Allocator = std.mem.Allocator;
const Color = std.zig.Color;

const tokenizer = @import("tokenizer.zig");

pub const Ast = @import("Ast.zig");
pub const AstGen = @import("AstGen.zig");
pub const ErrorBundle = @import("ErrorBundle.zig");
pub const Parse = @import("Parse.zig");
pub const Token = tokenizer.Token;
pub const Tokenizer = tokenizer.Tokenizer;
pub const Zir = @import("Zir.zig");
pub const number_parser = @import("number_parser.zig");
pub const print_zir = @import("print_zir.zig");
pub const DocumentScope = @import("DocumentScope.zig");
pub const InternPool = @import("InternPool.zig");

pub fn printAstErrorsToStderr(gpa: Allocator, tree: Ast, path: []const u8, color: Color) !void {
    var wip_errors: ErrorBundle.Wip = undefined;
    try wip_errors.init(gpa);
    defer wip_errors.deinit();

    try putAstErrorsIntoBundle(gpa, tree, path, &wip_errors);

    var error_bundle = try wip_errors.toOwnedBundle("");
    defer error_bundle.deinit(gpa);
    error_bundle.renderToStdErr(.{}, color);
}

pub fn putAstErrorsIntoBundle(
    gpa: Allocator,
    tree: Ast,
    path: []const u8,
    wip_errors: *ErrorBundle.Wip,
) Allocator.Error!void {
    var document_scope: DocumentScope = .{};
    defer document_scope.deinit(gpa);
    var context: DocumentScope.ScopeContext = .{
        .gpa = gpa,
        .tree = tree,
        .doc_scope = &document_scope,
    };
    defer context.deinit();
    var zir = try AstGen.generate(gpa, &context);
    defer zir.deinit(gpa);

    try wip_errors.addZirErrorMessages(zir, tree, tree.source, path);
}

pub fn printZirErrorsToStderr(gpa: Allocator, tree: Ast, zir: Zir, path: []const u8, color: Color) !void {
    var wip_errors: ErrorBundle.Wip = undefined;
    try wip_errors.init(gpa);
    defer wip_errors.deinit();

    try wip_errors.addZirErrorMessages(zir, tree, tree.source, path);

    var error_bundle = try wip_errors.toOwnedBundle("");
    defer error_bundle.deinit(gpa);
    error_bundle.renderToStdErr(.{}, color);
}

test {
    @import("std").testing.refAllDecls(@This());
}
