const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Ast = @import("../../Ast.zig");
const Node = Ast.Node;
const AnyVisitor = Ast.AnyVisitor;
const LambdaExpressionVisitor = Ast.LambdaExpressionVisitor;

const NodeTagVisitor = @This();

tags: *ArrayList(Node.Tag),

pub fn create(allocator: Allocator, tags: *ArrayList(Node.Tag)) !*NodeTagVisitor {
    const self = try allocator.create(NodeTagVisitor);
    self.* = .{
        .tags = tags,
    };
    return self;
}

pub fn destroy(self: *NodeTagVisitor, allocator: Allocator) void {
    allocator.destroy(self);
}

pub fn any(self: *NodeTagVisitor) AnyVisitor {
    return .{
        .ptr = self,
        .vtable = &.{
            .visit = NodeTagVisitor.visit,
        },
    };
}

fn visit(ctx: *anyopaque, tag: Node.Tag, _: Ast, _: Node.Index) !void {
    const self: *NodeTagVisitor = @ptrCast(@alignCast(ctx));
    try self.tags.append(tag);
}
