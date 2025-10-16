const common = @import("common.zig");
const Result = common.Result;

const null_float = common.null_float;
const inf_float = common.inf_float;

pub fn parseDatetime(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .datetime = null_float },
        'W', 'w' => return .{ .datetime = inf_float },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'z' => .{ .datetime = null_float },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'z' => .{ .datetime = inf_float },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    return .{ .failure = .nyi };
}

test "parse number literal - datetime" {
    return error.SkipZigTest;
}
