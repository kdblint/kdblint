const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Token = Ast.Token;

const AnyVisitor = @This();

const log = std.log.scoped(.kdblint_visitor);

ptr: *anyopaque,
vtable: *const VTable,

pub const VTable = struct {
    visit: *const fn (ctx: *anyopaque, tag: Node.Tag, tree: Ast, i: Node.Index) anyerror!void,
};

pub fn visit(self: AnyVisitor, tag: Node.Tag, tree: Ast, i: Node.Index) !void {
    try self.vtable.visit(self.ptr, tag, tree, i);
}

pub const LambdaVisitor = struct {
    allocator: Allocator,
    pub fn create(allocator: Allocator) !*LambdaVisitor {
        const self = try allocator.create(LambdaVisitor);
        self.* = .{
            .allocator = allocator,
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

        const datas: []Node.Data = tree.nodes.items(.data);
        const extra_datas: []Node.Index = tree.extra_data;

        const data = datas[i];
        const lambda = tree.extraData(data.lhs, Node.Lambda);

        const params: []Token.Index = extra_datas[lambda.params_start..lambda.params_end];
        {
            var list = std.ArrayList(u8).init(self.allocator);
            defer list.deinit();
            const writer = list.writer();
            if (params[0] == 0) {
                switch (params.len) {
                    1 => try writer.writeAll("`x"),
                    2 => try writer.writeAll("`x`y"),
                    3 => try writer.writeAll("`x`y`z"),
                    else => unreachable,
                }
            } else {
                for (params) |param| {
                    try writer.print("`{s}", .{tree.tokenSlice(param)});
                }
            }
            log.debug("params: {s}", .{list.items});
        }

        const locals: []Token.Index = extra_datas[lambda.locals_start..lambda.locals_end];
        if (locals.len > 0) {
            var list = std.ArrayList(u8).init(self.allocator);
            defer list.deinit();
            const writer = list.writer();
            for (locals) |local| {
                try writer.print("`{s}", .{tree.tokenSlice(local)});
            }
            log.debug("locals: {s}", .{list.items});
        } else {
            log.debug("locals: `symbol$()", .{});
        }

        const globals: []Token.Index = extra_datas[lambda.globals_start..lambda.globals_end];
        if (globals.len > 0) {
            var list = std.ArrayList(u8).init(self.allocator);
            defer list.deinit();
            const writer = list.writer();
            for (globals) |global| {
                try writer.print("`{s}", .{tree.tokenSlice(global)});
            }
            log.debug("globals: {s}", .{list.items});
        } else {
            log.debug("globals: `symbol$()", .{});
        }
    }
};
