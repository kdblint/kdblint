const common = @import("common.zig");
const Result = common.Result;
const testParse = common.testParse;

const null_char = common.null_char;

pub fn parseChar(bytes: []const u8, allow_suffix: bool) Result {
    return switch (bytes.len) {
        0 => unreachable,
        1 => switch (bytes[0]) {
            '0'...'9' => .{ .char = bytes[0] },
            else => .{ .char = null_char },
        },
        2 => switch (bytes[0]) {
            '0'...'9' => if (allow_suffix and bytes[1] == 'c')
                .{ .char = bytes[0] }
            else
                .{ .char = null_char },
            else => .{ .char = null_char },
        },
        else => .{ .char = null_char },
    };
}

test "parse number literal - char" {
    try testParse("0n", .char, false, .{ .char = null_char });
    try testParse("0N", .char, false, .{ .char = null_char });
    try testParse("0w", .char, false, .{ .char = null_char });
    try testParse("0W", .char, false, .{ .char = null_char });
    try testParse("0", .char, false, .{ .char = '0' });
    try testParse("1", .char, false, .{ .char = '1' });
    try testParse("2", .char, false, .{ .char = '2' });
    try testParse("0n", .char, true, .{ .char = null_char });
    try testParse("0N", .char, true, .{ .char = null_char });
    try testParse("0w", .char, true, .{ .char = null_char });
    try testParse("0W", .char, true, .{ .char = null_char });
    try testParse("0", .char, true, .{ .char = '0' });
    try testParse("1", .char, true, .{ .char = '1' });
    try testParse("2", .char, true, .{ .char = '2' });

    try testParse("0nc", .char, false, .{ .char = null_char });
    try testParse("0Nc", .char, false, .{ .char = null_char });
    try testParse("0wc", .char, false, .{ .char = null_char });
    try testParse("0Wc", .char, false, .{ .char = null_char });
    try testParse("0c", .char, false, .{ .char = null_char });
    try testParse("1c", .char, false, .{ .char = null_char });
    try testParse("2c", .char, false, .{ .char = null_char });
    try testParse("0nc", .char, true, .{ .char = null_char });
    try testParse("0Nc", .char, true, .{ .char = null_char });
    try testParse("0wc", .char, true, .{ .char = null_char });
    try testParse("0Wc", .char, true, .{ .char = null_char });
    try testParse("0c", .char, true, .{ .char = '0' });
    try testParse("1c", .char, true, .{ .char = '1' });
    try testParse("2c", .char, true, .{ .char = '2' });
}
