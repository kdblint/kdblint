const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("../../Ast.zig");
const Node = Ast.Node;
const Token = Ast.Token;
const AnyVisitor = Ast.AnyVisitor;

const FirstIdentifierVisitor = @This();

identifier: Token.Index = 0,

pub fn create(allocator: Allocator) !*FirstIdentifierVisitor {
    const self = try allocator.create(FirstIdentifierVisitor);
    self.* = .{};
    return self;
}

pub fn destroy(self: *FirstIdentifierVisitor, allocator: Allocator) void {
    allocator.destroy(self);
}

pub fn any(self: *FirstIdentifierVisitor) AnyVisitor {
    return .{
        .ptr = self,
        .vtable = &.{
            .visit = FirstIdentifierVisitor.visit,
        },
    };
}

fn visit(ctx: *anyopaque, tag: Node.Tag, tree: Ast, i: Node.Index) !void {
    if (tag != .identifier) return;

    const self: *FirstIdentifierVisitor = @ptrCast(@alignCast(ctx));
    if (self.identifier > 0) return;

    self.identifier = tree.nodes.items(.main_token)[i];
}
