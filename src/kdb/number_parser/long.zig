const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;

const null_long = common.null_long;
const inf_long = common.inf_long;

pub fn parseLong(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .long = null_long },
        'W', 'w' => return .{ .long = inf_long },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'j' => .{ .long = null_long },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'j' => .{ .long = inf_long },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'j') bytes[0 .. bytes.len - 1] else bytes;
    const x = switch (parseSlice(i64, slice)) {
        .overflow => return .{ .failure = .overflow },
        .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
        .int => |int| int,
    };

    if (x == inf_long) return .{ .failure = .prefer_long_inf };
    if (x > inf_long) return .{ .failure = .overflow };

    return .{ .long = x };
}

test "parse number literal - long" {
    try testParse("0N", .none, false, .{ .long = null_long });
    try testParse("0W", .none, false, .{ .long = inf_long });
    try testParse("0", .none, false, .{ .long = 0 });
    try testParse("1", .none, false, .{ .long = 1 });
    try testParse("2", .none, false, .{ .long = 2 });
    try testParse("9223372036854775806", .none, false, .{
        .long = 9223372036854775806,
    });
    try testParse("9223372036854775807", .none, false, .{
        .failure = .prefer_long_inf,
    });
    try testParse("9223372036854775808", .none, false, overflow);
    try testParse("0N", .none, true, .{ .long = null_long });
    try testParse("0W", .none, true, .{ .long = inf_long });
    try testParse("0", .none, true, .{ .long = 0 });
    try testParse("1", .none, true, .{ .long = 1 });
    try testParse("2", .none, true, .{ .long = 2 });
    try testParse("9223372036854775806", .none, true, .{
        .long = 9223372036854775806,
    });
    try testParse("9223372036854775807", .none, true, .{
        .failure = .prefer_long_inf,
    });
    try testParse("9223372036854775808", .none, true, overflow);

    try testParse("0n", .long, false, .{ .long = null_long });
    try testParse("0N", .long, false, .{ .long = null_long });
    try testParse("0w", .long, false, .{ .long = inf_long });
    try testParse("0W", .long, false, .{ .long = inf_long });
    try testParse("0", .long, false, .{ .long = 0 });
    try testParse("1", .long, false, .{ .long = 1 });
    try testParse("2", .long, false, .{ .long = 2 });
    try testParse("9223372036854775806", .long, false, .{
        .long = 9223372036854775806,
    });
    try testParse("9223372036854775807", .long, false, .{
        .failure = .prefer_long_inf,
    });
    try testParse("9223372036854775808", .long, false, overflow);
    try testParse("0n", .long, true, .{ .long = null_long });
    try testParse("0N", .long, true, .{ .long = null_long });
    try testParse("0w", .long, true, .{ .long = inf_long });
    try testParse("0W", .long, true, .{ .long = inf_long });
    try testParse("0", .long, true, .{ .long = 0 });
    try testParse("1", .long, true, .{ .long = 1 });
    try testParse("2", .long, true, .{ .long = 2 });
    try testParse("9223372036854775806", .long, true, .{
        .long = 9223372036854775806,
    });
    try testParse("9223372036854775807", .long, true, .{
        .failure = .prefer_long_inf,
    });
    try testParse("9223372036854775808", .long, true, overflow);

    try testParse("0nj", .long, false, invalidCharacter(2));
    try testParse("0Nj", .long, false, invalidCharacter(2));
    try testParse("0wj", .long, false, invalidCharacter(2));
    try testParse("0Wj", .long, false, invalidCharacter(2));
    try testParse("0j", .long, false, invalidCharacter(1));
    try testParse("1j", .long, false, invalidCharacter(1));
    try testParse("2j", .long, false, invalidCharacter(1));
    try testParse("9223372036854775806j", .long, false, invalidCharacter(19));
    try testParse("9223372036854775807j", .long, false, invalidCharacter(19));
    try testParse("9223372036854775808j", .long, false, overflow);
    try testParse("0nj", .long, true, .{ .long = null_long });
    try testParse("0Nj", .long, true, .{ .long = null_long });
    try testParse("0wj", .long, true, .{ .long = inf_long });
    try testParse("0Wj", .long, true, .{ .long = inf_long });
    try testParse("0j", .long, true, .{ .long = 0 });
    try testParse("1j", .long, true, .{ .long = 1 });
    try testParse("2j", .long, true, .{ .long = 2 });
    try testParse("9223372036854775806j", .long, true, .{
        .long = 9223372036854775806,
    });
    try testParse("9223372036854775807j", .long, true, .{
        .failure = .prefer_long_inf,
    });
    try testParse("9223372036854775808j", .long, true, overflow);
}
