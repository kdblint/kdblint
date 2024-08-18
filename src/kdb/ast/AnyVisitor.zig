const std = @import("std");
const Allocator = std.mem.Allocator;

const Tag = @import("Node.zig").Tag;

const AnyVisitor = @This();

ptr: *anyopaque,
vtable: *const VTable,

pub const VTable = struct {
    visit: *const fn (ctx: *anyopaque, tag: Tag) anyerror!void,
};

pub fn visit(self: AnyVisitor, tag: Tag) !void {
    try self.vtable.visit(self.ptr, tag);
}

pub const LambdaVisitor = struct {
    pub fn create(allocator: Allocator) !*LambdaVisitor {
        const self = try allocator.create(LambdaVisitor);
        self.* = .{};
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

    fn visit(ctx: *anyopaque, tag: Tag) !void {
        const self: *LambdaVisitor = @ptrCast(@alignCast(ctx));
        _ = self; // autofix
        std.log.debug("visiting: {s}", .{@tagName(tag)});
    }
};
