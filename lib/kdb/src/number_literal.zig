const std = @import("std");
const assert = std.debug.assert;

pub const Result = union(enum) {
    /// Result fits if it fits in i64
    long: i64,
    failure: Error,
};

pub const Error = union(enum) {
    nyi,
};

pub fn parseNumberLiteral(bytes: []const u8) Result {
    const result = std.zig.parseNumberLiteral(bytes);
    switch (result) {
        .int => |v| return .{ .long = @intCast(v) },
        else => return .{ .failure = .nyi },
    }
}
