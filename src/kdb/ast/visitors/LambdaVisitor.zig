const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Ast = @import("../../Ast.zig");
const Node = Ast.Node;
const Token = Ast.Token;
const AnyVisitor = Ast.AnyVisitor;
const LambdaExpressionVisitor = Ast.LambdaExpressionVisitor;

const LambdaVisitor = @This();

allocator: Allocator,
errors: *ArrayList(Ast.Error),

pub fn create(allocator: Allocator, errors: *ArrayList(Ast.Error)) !*LambdaVisitor {
    const self = try allocator.create(LambdaVisitor);
    self.* = .{
        .allocator = allocator,
        .errors = errors,
    };
    return self;
}

pub fn destroy(self: *LambdaVisitor, allocator: Allocator) void {
    allocator.destroy(self);
}

pub fn any(self: *LambdaVisitor) AnyVisitor {
    return .{
        .ptr = self,
        .vtable = &.{
            .visit = LambdaVisitor.visit,
        },
    };
}

fn visit(ctx: *anyopaque, tag: Node.Tag, tree: Ast, i: Node.Index) !void {
    if (tag != .lambda) return;
    const self: *LambdaVisitor = @ptrCast(@alignCast(ctx));

    const expr_visitor = try LambdaExpressionVisitor.create(
        self.allocator,
        tree,
        i,
        self.errors,
    );
    defer expr_visitor.destroy(self.allocator);
    try tree.accept(expr_visitor.any(), i);
}
