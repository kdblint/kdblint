const common = @import("common.zig");
const Result = common.Result;
const testParse = common.testParse;

const null_long = common.null_long;
const inf_long = common.inf_long;

pub fn parseTimestamp(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .timestamp = null_long },
        'W', 'w' => return .{ .timestamp = inf_long },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'p' => .{ .timestamp = null_long },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'p' => .{ .timestamp = inf_long },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    return .{ .failure = .nyi };
}

test "parse number literal - timestamp" {
    return error.SkipZigTest;
}
