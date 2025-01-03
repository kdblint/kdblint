const common = @import("common.zig");
const Result = common.Result;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const invalidBinary = common.invalidBinary;

pub fn parseBool(bytes: []const u8) Result {
    const value = switch (bytes[0]) {
        '0' => false,
        '1' => true,
        else => return .{ .failure = .{
            .invalid_digit = .{ .i = 0, .base = .binary },
        } },
    };

    if (bytes.len > 1) return .{ .failure = .{ .invalid_character = 1 } };

    return .{ .bool = value };
}

test "parse number literal - bool" {
    try testParse("0n", .bool, undefined, invalidCharacter(1));
    try testParse("0N", .bool, undefined, invalidCharacter(1));
    try testParse("0w", .bool, undefined, invalidCharacter(1));
    try testParse("0W", .bool, undefined, invalidCharacter(1));
    try testParse("0", .bool, undefined, .{ .bool = false });
    try testParse("1", .bool, undefined, .{ .bool = true });
    try testParse("2", .bool, undefined, invalidBinary(0));
}
