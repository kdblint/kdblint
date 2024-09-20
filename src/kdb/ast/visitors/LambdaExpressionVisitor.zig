const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Ast = @import("../../Ast.zig");
const Node = Ast.Node;
const Token = Ast.Token;
const AnyVisitor = Ast.AnyVisitor;
const FirstIdentifierVisitor = Ast.FirstIdentifierVisitor;

const LambdaExpressionVisitor = @This();

allocator: Allocator,
start_node: Node.Index,
errors: *ArrayList(Ast.Error),
params: [][]const u8,
locals: std.StringHashMap(void),
globals: [][]const u8,
assigned_locals: std.StringHashMap(void),

pub fn create(allocator: Allocator, tree: Ast, start_node: Node.Index, errors: *ArrayList(Ast.Error)) !*LambdaExpressionVisitor {
    const self = try allocator.create(LambdaExpressionVisitor);

    const datas: []Node.Data = tree.nodes.items(.data);
    const extra_datas: []Node.Index = tree.extra_data;

    const data = datas[start_node];
    const lambda = tree.extraData(data.lhs, Node.Lambda);

    const params: [][]const u8 = params: {
        const params: []Token.Index = extra_datas[lambda.params_start..lambda.params_end];
        var list = std.ArrayList([]const u8).init(allocator);
        defer list.deinit();
        if (params[0] == 0) {
            switch (params.len) {
                1 => try list.append("x"),
                2 => try list.appendSlice(&.{ "x", "y" }),
                3 => try list.appendSlice(&.{ "x", "y", "z" }),
                else => unreachable,
            }
        } else {
            for (params) |param| {
                try list.append(tree.tokenSlice(param));
            }
        }
        break :params try list.toOwnedSlice();
    };
    errdefer allocator.free(params);

    var locals = std.StringHashMap(void).init(allocator);
    for (extra_datas[lambda.locals_start..lambda.locals_end]) |local| {
        try locals.putNoClobber(tree.tokenSlice(local), {});
    }
    errdefer locals.deinit();

    const globals: [][]const u8 = globals: {
        const globals: []Token.Index = extra_datas[lambda.globals_start..lambda.globals_end];
        var list = std.ArrayList([]const u8).init(allocator);
        defer list.deinit();
        for (globals) |global| {
            try list.append(tree.tokenSlice(global));
        }
        break :globals try list.toOwnedSlice();
    };
    errdefer comptime unreachable;

    const assigned_locals = std.StringHashMap(void).init(allocator);

    self.* = .{
        .allocator = allocator,
        .start_node = start_node,
        .errors = errors,
        .params = params,
        .locals = locals,
        .globals = globals,
        .assigned_locals = assigned_locals,
    };
    return self;
}

pub fn destroy(self: *LambdaExpressionVisitor, allocator: Allocator) void {
    allocator.free(self.params);
    self.locals.deinit();
    allocator.free(self.globals);
    self.assigned_locals.deinit();
    allocator.destroy(self);
}

pub fn any(self: *LambdaExpressionVisitor) AnyVisitor {
    return .{
        .ptr = self,
        .vtable = &.{
            .visit = LambdaExpressionVisitor.visit,
        },
    };
}

fn visit(ctx: *anyopaque, tag: Node.Tag, tree: Ast, i: Node.Index) !void {
    const self: *LambdaExpressionVisitor = @ptrCast(@alignCast(ctx));
    if (self.start_node == i) return;

    const datas: []Node.Data = tree.nodes.items(.data);
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);

    switch (tag) {
        .identifier => {
            const token: Token.Index = main_tokens[i];
            const slice = tree.tokenSlice(token);
            if (self.locals.contains(slice) and !self.assigned_locals.contains(slice)) {
                try self.errors.append(Ast.Error{
                    .tag = .undeclared_identifier,
                    .token = token,
                });
                // Prevent repeated errors
                try self.assigned_locals.put(slice, {});
            }
        },
        .assign => {
            const data = datas[i];
            if (data.lhs > 0) {
                const identifier_visitor = try FirstIdentifierVisitor.create(self.allocator);
                defer identifier_visitor.destroy(self.allocator);

                try tree.accept(identifier_visitor.any(), data.lhs);
                const slice = tree.tokenSlice(identifier_visitor.identifier);
                try self.assigned_locals.put(slice, {});
            }
        },
        else => {},
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
