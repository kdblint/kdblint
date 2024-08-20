const std = @import("std");

const Ast = @import("../../Ast.zig");
const Node = Ast.Node;

const AnyVisitor = @This();

ptr: *anyopaque,
vtable: *const VTable,

pub const VTable = struct {
    visit: *const fn (ctx: *anyopaque, tag: Node.Tag, tree: Ast, i: Node.Index) anyerror!void,
};

pub fn visit(self: AnyVisitor, tag: Node.Tag, tree: Ast, i: Node.Index) !void {
    try self.vtable.visit(self.ptr, tag, tree, i);
}
