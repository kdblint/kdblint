const std = @import("std");

const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;

const null_int = common.null_int;
const inf_int = common.inf_int;

pub fn parseTime(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .time = null_int },
        'W', 'w' => return .{ .time = inf_int },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            't' => .{ .time = null_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            't' => .{ .time = inf_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 't') bytes[0 .. bytes.len - 1] else bytes;
    switch (slice.len) {
        0 => unreachable,
        1, 2 => {
            const hour_value = switch (parseSlice(i32, slice)) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const milliseconds = hour_value * std.time.ms_per_hour;
            return .{ .time = milliseconds };
        },
        3, 4, 5 => {
            const hour_value = switch (parseSlice(i32, slice[0 .. slice.len - 2])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const minute_value = switch (parseSlice(i32, slice[slice.len - 2 ..])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 2,
                } },
                .int => |int| int,
            };
            if (minute_value > 59) return .{ .failure = .overflow };
            var milliseconds: i32 = 0;
            {
                const res = @mulWithOverflow(hour_value, std.time.ms_per_hour);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            const res = @addWithOverflow(milliseconds, minute_value * std.time.ms_per_min);
            if (res[1] != 0) return .{ .failure = .overflow };
            milliseconds = res[0];
            return .{ .time = milliseconds };
        },
        9 => {
            const hour_value = switch (parseSlice(i32, slice[0 .. slice.len - 7])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const minute_value = switch (parseSlice(i32, slice[slice.len - 7 .. slice.len - 5])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 7,
                } },
                .int => |int| int,
            };
            if (minute_value > 59) return .{ .failure = .overflow };
            const second_value = switch (parseSlice(i32, slice[slice.len - 5 .. slice.len - 3])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 5,
                } },
                .int => |int| int,
            };
            if (second_value > 59) return .{ .failure = .overflow };
            const millisecond_value = switch (parseSlice(i32, slice[slice.len - 3 ..])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 3,
                } },
                .int => |int| int,
            };
            var milliseconds: i32 = 0;
            {
                const res = @mulWithOverflow(hour_value, std.time.ms_per_hour);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            {
                const res = @addWithOverflow(milliseconds, minute_value * std.time.ms_per_min);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            {
                const res = @addWithOverflow(milliseconds, second_value * std.time.ms_per_s);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            const res = @addWithOverflow(milliseconds, millisecond_value);
            if (res[1] != 0) return .{ .failure = .overflow };
            milliseconds = res[0];
            return .{ .time = milliseconds };
        },
        10, 11, 12, 13 => if (slice[2] == ':' and slice[5] == ':' and slice[8] == '.') {
            const hour_value = switch (parseSlice(i32, slice[0..2])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const minute_value = switch (parseSlice(i32, slice[3..5])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 3 } },
                .int => |int| int,
            };
            if (minute_value > 59) return .{ .failure = .overflow };
            const second_value = switch (parseSlice(i32, slice[6..8])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 6 } },
                .int => |int| int,
            };
            if (second_value > 59) return .{ .failure = .overflow };
            var millisecond_value: i32 = 0;
            var i: usize = 9;
            while (i < slice.len and i < 12) : (i += 1) {
                const c = slice[i];
                const digit = switch (c) {
                    '0'...'9' => c - '0',
                    else => return .{ .failure = .{ .invalid_character = i } },
                };
                millisecond_value += digit * std.math.pow(i32, 10, 11 - @as(i32, @intCast(i)));
            }
            var milliseconds: i32 = 0;
            {
                const res = @mulWithOverflow(hour_value, std.time.ms_per_hour);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            {
                const res = @addWithOverflow(milliseconds, minute_value * std.time.ms_per_min);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            {
                const res = @addWithOverflow(milliseconds, second_value * std.time.ms_per_s);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            const res = @addWithOverflow(milliseconds, millisecond_value);
            if (res[1] != 0) return .{ .failure = .overflow };
            milliseconds = res[0];
            return .{ .time = milliseconds };
        } else if (slice[3] == ':' and slice[6] == ':' and slice[9] == '.') {
            const hour_value = switch (parseSlice(i32, slice[0..3])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const minute_value = switch (parseSlice(i32, slice[4..6])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 4 } },
                .int => |int| int,
            };
            if (minute_value > 59) return .{ .failure = .overflow };
            const second_value = switch (parseSlice(i32, slice[7..9])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 7 } },
                .int => |int| int,
            };
            if (second_value > 59) return .{ .failure = .overflow };
            var millisecond_value: i32 = 0;
            var i: usize = 10;
            while (i < slice.len and i < 13) : (i += 1) {
                const c = slice[i];
                const digit = switch (c) {
                    '0'...'9' => c - '0',
                    else => return .{ .failure = .{ .invalid_character = i } },
                };
                millisecond_value += digit * std.math.pow(i32, 10, 12 - @as(i32, @intCast(i)));
            }
            var milliseconds: i32 = 0;
            {
                const res = @mulWithOverflow(hour_value, std.time.ms_per_hour);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            {
                const res = @addWithOverflow(milliseconds, minute_value * std.time.ms_per_min);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            {
                const res = @addWithOverflow(milliseconds, second_value * std.time.ms_per_s);
                if (res[1] != 0) return .{ .failure = .overflow };
                milliseconds = res[0];
            }
            const res = @addWithOverflow(milliseconds, millisecond_value);
            if (res[1] != 0) return .{ .failure = .overflow };
            milliseconds = res[0];
            return .{ .time = milliseconds };
        },
        else => {},
    }

    const hour_value = switch (parseSlice(i32, slice[0 .. slice.len - 4])) {
        .overflow => return .{ .failure = .overflow },
        .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
        .int => |int| int,
    };
    const minute_value = switch (parseSlice(i32, slice[slice.len - 4 .. slice.len - 2])) {
        .overflow => return .{ .failure = .overflow },
        .invalid_character => |i| return .{ .failure = .{
            .invalid_character = i + slice.len - 4,
        } },
        .int => |int| int,
    };
    if (minute_value > 59) return .{ .failure = .overflow };
    const second_value = switch (parseSlice(i32, slice[slice.len - 2 ..])) {
        .overflow => return .{ .failure = .overflow },
        .invalid_character => |i| return .{ .failure = .{
            .invalid_character = i + slice.len - 2,
        } },
        .int => |int| int,
    };
    if (second_value > 59) return .{ .failure = .overflow };
    var milliseconds: i32 = 0;
    {
        const res = @mulWithOverflow(hour_value, std.time.ms_per_hour);
        if (res[1] != 0) return .{ .failure = .overflow };
        milliseconds = res[0];
    }
    {
        const res = @addWithOverflow(milliseconds, minute_value * std.time.ms_per_min);
        if (res[1] != 0) return .{ .failure = .overflow };
        milliseconds = res[0];
    }
    const res = @addWithOverflow(milliseconds, second_value * std.time.ms_per_s);
    if (res[1] != 0) return .{ .failure = .overflow };
    milliseconds = res[0];
    return .{ .time = milliseconds };
}

test "parse number literal - time" {
    try testParse("0n", .time, false, .{ .time = null_int });
    try testParse("0N", .time, false, .{ .time = null_int });
    try testParse("0w", .time, false, .{ .time = inf_int });
    try testParse("0W", .time, false, .{ .time = inf_int });

    try testParse("0", .time, false, .{ .time = 0 });
    try testParse("9", .time, false, .{ .time = 32400000 });

    try testParse("00", .time, false, .{ .time = 0 });
    try testParse("99", .time, false, .{ .time = 356400000 });

    try testParse("000", .time, false, .{ .time = 0 });
    try testParse("959", .time, false, .{ .time = 35940000 });

    try testParse("0000", .time, false, .{ .time = 0 });
    try testParse("9959", .time, false, .{ .time = 359940000 });

    try testParse("00000", .time, false, .{ .time = 0 });
    try testParse("99959", .time, false, overflow);

    try testParse("000000", .time, false, .{ .time = 0 });
    try testParse("995959", .time, false, .{ .time = 359999000 });

    try testParse("0000000", .time, false, .{ .time = 0 });
    try testParse("9995959", .time, false, overflow);

    try testParse("00000000", .time, false, .{ .time = 0 });
    try testParse("99995959", .time, false, overflow);

    try testParse("000000000", .time, false, .{ .time = 0 });
    try testParse("995959999", .time, false, .{ .time = 359999999 });

    try testParse("0000000000", .time, false, .{ .time = 0 });
    try testParse("9999995959", .time, false, overflow);

    try testParse("00000000000", .time, false, .{ .time = 0 });
    try testParse("99999995959", .time, false, overflow);

    try testParse("000000000000", .time, false, .{ .time = 0 });
    try testParse("999999995959", .time, false, overflow);

    try testParse("0000000000000", .time, false, .{ .time = 0 });
    try testParse("9999999995959", .time, false, overflow);

    try testParse("00000000000000", .time, false, .{ .time = 0 });
    try testParse("21474836475959", .time, false, overflow);

    try testParse("00:00:00.0", .time, false, .{ .time = 0 });
    try testParse("00:00:00.00", .time, false, .{ .time = 0 });
    try testParse("00:00:00.000", .time, false, .{ .time = 0 });
    try testParse("00:00:00.0000", .time, false, .{ .time = 0 });

    try testParse("99:59:59.9", .time, false, .{ .time = 359999900 });
    try testParse("99:59:59.99", .time, false, .{ .time = 359999990 });
    try testParse("99:59:59.999", .time, false, .{ .time = 359999999 });
    try testParse("99:59:59.9999", .time, false, .{ .time = 359999999 });

    try testParse("000:00:00.0", .time, false, .{ .time = 0 });
    try testParse("000:00:00.00", .time, false, .{ .time = 0 });
    try testParse("000:00:00.000", .time, false, .{ .time = 0 });
    try testParse("999:59:59.9", .time, false, overflow);
    try testParse("999:59:59.99", .time, false, overflow);
    try testParse("999:59:59.999", .time, false, overflow);

    try testParse("0n", .time, true, .{ .time = null_int });
    try testParse("0N", .time, true, .{ .time = null_int });
    try testParse("0w", .time, true, .{ .time = inf_int });
    try testParse("0W", .time, true, .{ .time = inf_int });

    try testParse("0", .time, true, .{ .time = 0 });
    try testParse("9", .time, true, .{ .time = 32400000 });

    try testParse("00", .time, true, .{ .time = 0 });
    try testParse("99", .time, true, .{ .time = 356400000 });

    try testParse("000", .time, true, .{ .time = 0 });
    try testParse("959", .time, true, .{ .time = 35940000 });

    try testParse("0000", .time, true, .{ .time = 0 });
    try testParse("9959", .time, true, .{ .time = 359940000 });

    try testParse("00000", .time, true, .{ .time = 0 });
    try testParse("99959", .time, true, overflow);

    try testParse("000000", .time, true, .{ .time = 0 });
    try testParse("995959", .time, true, .{ .time = 359999000 });

    try testParse("0000000", .time, true, .{ .time = 0 });
    try testParse("9995959", .time, true, overflow);

    try testParse("00000000", .time, true, .{ .time = 0 });
    try testParse("99995959", .time, true, overflow);

    try testParse("000000000", .time, true, .{ .time = 0 });
    try testParse("995959999", .time, true, .{ .time = 359999999 });

    try testParse("0000000000", .time, true, .{ .time = 0 });
    try testParse("9999995959", .time, true, overflow);

    try testParse("00000000000", .time, true, .{ .time = 0 });
    try testParse("99999995959", .time, true, overflow);

    try testParse("000000000000", .time, true, .{ .time = 0 });
    try testParse("999999995959", .time, true, overflow);

    try testParse("0000000000000", .time, true, .{ .time = 0 });
    try testParse("9999999995959", .time, true, overflow);

    try testParse("00000000000000", .time, true, .{ .time = 0 });
    try testParse("21474836475959", .time, true, overflow);

    try testParse("00:00:00.0", .time, true, .{ .time = 0 });
    try testParse("00:00:00.00", .time, true, .{ .time = 0 });
    try testParse("00:00:00.000", .time, true, .{ .time = 0 });
    try testParse("00:00:00.0000", .time, true, .{ .time = 0 });

    try testParse("99:59:59.9", .time, true, .{ .time = 359999900 });
    try testParse("99:59:59.99", .time, true, .{ .time = 359999990 });
    try testParse("99:59:59.999", .time, true, .{ .time = 359999999 });
    try testParse("99:59:59.9999", .time, true, .{ .time = 359999999 });

    try testParse("000:00:00.0", .time, true, .{ .time = 0 });
    try testParse("000:00:00.00", .time, true, .{ .time = 0 });
    try testParse("000:00:00.000", .time, true, .{ .time = 0 });
    try testParse("999:59:59.9", .time, true, overflow);
    try testParse("999:59:59.99", .time, true, overflow);
    try testParse("999:59:59.999", .time, true, overflow);

    try testParse("0nt", .time, false, invalidCharacter(2));
    try testParse("0Nt", .time, false, invalidCharacter(2));
    try testParse("0wt", .time, false, invalidCharacter(2));
    try testParse("0Wt", .time, false, invalidCharacter(2));

    try testParse("0t", .time, false, invalidCharacter(1));
    try testParse("9t", .time, false, invalidCharacter(1));

    try testParse("00t", .time, false, invalidCharacter(2));
    try testParse("99t", .time, false, invalidCharacter(2));

    try testParse("000t", .time, false, invalidCharacter(3));
    try testParse("959t", .time, false, invalidCharacter(3));

    try testParse("0000t", .time, false, invalidCharacter(4));
    try testParse("9959t", .time, false, invalidCharacter(4));

    try testParse("00000t", .time, false, invalidCharacter(5));
    try testParse("99959t", .time, false, overflow);

    try testParse("000000t", .time, false, invalidCharacter(6));
    try testParse("995959t", .time, false, overflow);

    try testParse("0000000t", .time, false, invalidCharacter(7));
    try testParse("9995959t", .time, false, overflow);

    try testParse("00000000t", .time, false, invalidCharacter(8));
    try testParse("99995959t", .time, false, overflow);

    try testParse("000000000t", .time, false, invalidCharacter(9));
    try testParse("995959999t", .time, false, overflow);

    try testParse("0000000000t", .time, false, invalidCharacter(10));
    try testParse("9999995959t", .time, false, overflow);

    try testParse("00000000000t", .time, false, invalidCharacter(11));
    try testParse("99999995959t", .time, false, overflow);

    try testParse("000000000000t", .time, false, invalidCharacter(12));
    try testParse("999999995959t", .time, false, overflow);

    try testParse("0000000000000t", .time, false, invalidCharacter(13));
    try testParse("9999999995959t", .time, false, overflow);

    try testParse("00000000000000t", .time, false, invalidCharacter(14));
    try testParse("21474836475959t", .time, false, overflow);

    try testParse("00:00:00.0t", .time, false, invalidCharacter(10));
    try testParse("00:00:00.00t", .time, false, invalidCharacter(11));
    try testParse("00:00:00.000t", .time, false, .{ .time = 0 });
    try testParse("00:00:00.0000t", .time, false, invalidCharacter(2));

    try testParse("99:59:59.9t", .time, false, invalidCharacter(10));
    try testParse("99:59:59.99t", .time, false, invalidCharacter(11));
    try testParse("99:59:59.999t", .time, false, .{ .time = 359999999 });
    try testParse("99:59:59.9999t", .time, false, invalidCharacter(2));

    try testParse("000:00:00.0t", .time, false, invalidCharacter(11));
    try testParse("000:00:00.00t", .time, false, invalidCharacter(12));
    try testParse("000:00:00.000t", .time, false, invalidCharacter(3));
    try testParse("999:59:59.9t", .time, false, invalidCharacter(11));
    try testParse("999:59:59.99t", .time, false, invalidCharacter(12));
    try testParse("999:59:59.999t", .time, false, invalidCharacter(3));

    try testParse("0nt", .time, true, .{ .time = null_int });
    try testParse("0Nt", .time, true, .{ .time = null_int });
    try testParse("0wt", .time, true, .{ .time = inf_int });
    try testParse("0Wt", .time, true, .{ .time = inf_int });

    try testParse("0t", .time, true, .{ .time = 0 });
    try testParse("9t", .time, true, .{ .time = 32400000 });

    try testParse("00t", .time, true, .{ .time = 0 });
    try testParse("99t", .time, true, .{ .time = 356400000 });

    try testParse("000t", .time, true, .{ .time = 0 });
    try testParse("959t", .time, true, .{ .time = 35940000 });

    try testParse("0000t", .time, true, .{ .time = 0 });
    try testParse("9959t", .time, true, .{ .time = 359940000 });

    try testParse("00000t", .time, true, .{ .time = 0 });
    try testParse("99959t", .time, true, overflow);

    try testParse("000000t", .time, true, .{ .time = 0 });
    try testParse("995959t", .time, true, .{ .time = 359999000 });

    try testParse("0000000t", .time, true, .{ .time = 0 });
    try testParse("9995959t", .time, true, overflow);

    try testParse("00000000t", .time, true, .{ .time = 0 });
    try testParse("99995959t", .time, true, overflow);

    try testParse("000000000t", .time, true, .{ .time = 0 });
    try testParse("995959999t", .time, true, .{ .time = 359999999 });

    try testParse("0000000000t", .time, true, .{ .time = 0 });
    try testParse("9999995959t", .time, true, overflow);

    try testParse("00000000000t", .time, true, .{ .time = 0 });
    try testParse("99999995959t", .time, true, overflow);

    try testParse("000000000000t", .time, true, .{ .time = 0 });
    try testParse("999999995959t", .time, true, overflow);

    try testParse("0000000000000t", .time, true, .{ .time = 0 });
    try testParse("9999999995959t", .time, true, overflow);

    try testParse("00000000000000t", .time, true, .{ .time = 0 });
    try testParse("21474836475959t", .time, true, overflow);

    try testParse("00:00:00.0t", .time, true, .{ .time = 0 });
    try testParse("00:00:00.00t", .time, true, .{ .time = 0 });
    try testParse("00:00:00.000t", .time, true, .{ .time = 0 });
    try testParse("00:00:00.0000t", .time, true, .{ .time = 0 });

    try testParse("99:59:59.9t", .time, true, .{ .time = 359999900 });
    try testParse("99:59:59.99t", .time, true, .{ .time = 359999990 });
    try testParse("99:59:59.999t", .time, true, .{ .time = 359999999 });
    try testParse("99:59:59.9999t", .time, true, .{ .time = 359999999 });

    try testParse("000:00:00.0t", .time, true, .{ .time = 0 });
    try testParse("000:00:00.00t", .time, true, .{ .time = 0 });
    try testParse("000:00:00.000t", .time, true, .{ .time = 0 });
    try testParse("999:59:59.9t", .time, true, overflow);
    try testParse("999:59:59.99t", .time, true, overflow);
    try testParse("999:59:59.999t", .time, true, overflow);

    try testParse("060", .time, false, overflow);
    try testParse("960", .time, false, overflow);

    try testParse("0060", .time, false, overflow);
    try testParse("9960", .time, false, overflow);

    try testParse("00060", .time, false, overflow);
    try testParse("99960", .time, false, overflow);

    try testParse("006000", .time, false, overflow);
    try testParse("000060", .time, false, overflow);
    try testParse("996059", .time, false, overflow);
    try testParse("995960", .time, false, overflow);

    try testParse("0006000", .time, false, overflow);
    try testParse("0000060", .time, false, overflow);
    try testParse("9996059", .time, false, overflow);
    try testParse("9995960", .time, false, overflow);

    try testParse("00006000", .time, false, overflow);
    try testParse("00000060", .time, false, overflow);
    try testParse("99996059", .time, false, overflow);
    try testParse("99995960", .time, false, overflow);

    try testParse("006000000", .time, false, overflow);
    try testParse("000060000", .time, false, overflow);
    try testParse("996059999", .time, false, overflow);
    try testParse("995960999", .time, false, overflow);

    try testParse("0000006000", .time, false, overflow);
    try testParse("0000000060", .time, false, overflow);
    try testParse("9999996059", .time, false, overflow);
    try testParse("9999995960", .time, false, overflow);

    try testParse("00000006000", .time, false, overflow);
    try testParse("00000000060", .time, false, overflow);
    try testParse("99999996059", .time, false, overflow);
    try testParse("99999995960", .time, false, overflow);

    try testParse("000000006000", .time, false, overflow);
    try testParse("000000000060", .time, false, overflow);
    try testParse("999999996059", .time, false, overflow);
    try testParse("999999995960", .time, false, overflow);

    try testParse("0000000006000", .time, false, overflow);
    try testParse("0000000000060", .time, false, overflow);
    try testParse("9999999996059", .time, false, overflow);
    try testParse("9999999995960", .time, false, overflow);

    try testParse("00000000006000", .time, false, overflow);
    try testParse("00000000000060", .time, false, overflow);
    try testParse("21474836485959", .time, false, overflow);
    try testParse("21474836476059", .time, false, overflow);
    try testParse("21474836475960", .time, false, overflow);

    try testParse("00:00:00.", .time, false, invalidCharacter(2));
    try testParse("99:59:59.", .time, false, invalidCharacter(2));

    try testParse("000:00:00.", .time, false, .{ .time = 0 });
    try testParse("999:59:59.", .time, false, overflow);

    try testParse("00:0:0.0", .time, false, invalidCharacter(2));
    try testParse("00:0:00.0", .time, false, invalidCharacter(2));
    try testParse("00:00:0.0", .time, false, invalidCharacter(2));

    try testParse("00:60:00.000", .time, false, overflow);
    try testParse("00:00:60.000", .time, false, overflow);
    try testParse("99:60:59.999", .time, false, overflow);
    try testParse("99:59:60.999", .time, false, overflow);

    try testParse("060", .time, true, overflow);
    try testParse("960", .time, true, overflow);

    try testParse("0060", .time, true, overflow);
    try testParse("9960", .time, true, overflow);

    try testParse("00060", .time, true, overflow);
    try testParse("99960", .time, true, overflow);

    try testParse("006000", .time, true, overflow);
    try testParse("000060", .time, true, overflow);
    try testParse("996059", .time, true, overflow);
    try testParse("995960", .time, true, overflow);

    try testParse("0006000", .time, true, overflow);
    try testParse("0000060", .time, true, overflow);
    try testParse("9996059", .time, true, overflow);
    try testParse("9995960", .time, true, overflow);

    try testParse("00006000", .time, true, overflow);
    try testParse("00000060", .time, true, overflow);
    try testParse("99996059", .time, true, overflow);
    try testParse("99995960", .time, true, overflow);

    try testParse("006000000", .time, true, overflow);
    try testParse("000060000", .time, true, overflow);
    try testParse("996059999", .time, true, overflow);
    try testParse("995960999", .time, true, overflow);

    try testParse("0000006000", .time, true, overflow);
    try testParse("0000000060", .time, true, overflow);
    try testParse("9999996059", .time, true, overflow);
    try testParse("9999995960", .time, true, overflow);

    try testParse("00000006000", .time, true, overflow);
    try testParse("00000000060", .time, true, overflow);
    try testParse("99999996059", .time, true, overflow);
    try testParse("99999995960", .time, true, overflow);

    try testParse("000000006000", .time, true, overflow);
    try testParse("000000000060", .time, true, overflow);
    try testParse("999999996059", .time, true, overflow);
    try testParse("999999995960", .time, true, overflow);

    try testParse("0000000006000", .time, true, overflow);
    try testParse("0000000000060", .time, true, overflow);
    try testParse("9999999996059", .time, true, overflow);
    try testParse("9999999995960", .time, true, overflow);

    try testParse("00000000006000", .time, true, overflow);
    try testParse("00000000000060", .time, true, overflow);
    try testParse("21474836485959", .time, true, overflow);
    try testParse("21474836476059", .time, true, overflow);
    try testParse("21474836475960", .time, true, overflow);

    try testParse("00:00:00.", .time, true, invalidCharacter(2));
    try testParse("99:59:59.", .time, true, invalidCharacter(2));

    try testParse("000:00:00.", .time, true, .{ .time = 0 });
    try testParse("999:59:59.", .time, true, overflow);

    try testParse("00:0:0.0", .time, true, invalidCharacter(2));
    try testParse("00:0:00.0", .time, true, invalidCharacter(2));
    try testParse("00:00:0.0", .time, true, invalidCharacter(2));

    try testParse("00:60:00.000", .time, true, overflow);
    try testParse("00:00:60.000", .time, true, overflow);
    try testParse("99:60:59.999", .time, true, overflow);
    try testParse("99:59:60.999", .time, true, overflow);

    try testParse("060t", .time, false, invalidCharacter(3));
    try testParse("960t", .time, false, invalidCharacter(3));

    try testParse("0060t", .time, false, invalidCharacter(4));
    try testParse("9960t", .time, false, invalidCharacter(4));

    try testParse("00060t", .time, false, invalidCharacter(5));
    try testParse("99960t", .time, false, overflow);

    try testParse("006000t", .time, false, invalidCharacter(6));
    try testParse("000060t", .time, false, invalidCharacter(6));
    try testParse("996059t", .time, false, invalidCharacter(6));
    try testParse("995960t", .time, false, overflow);

    try testParse("0006000t", .time, false, invalidCharacter(7));
    try testParse("0000060t", .time, false, invalidCharacter(7));
    try testParse("9996059t", .time, false, invalidCharacter(7));
    try testParse("9995960t", .time, false, overflow);

    try testParse("00006000t", .time, false, overflow);
    try testParse("00000060t", .time, false, invalidCharacter(8));
    try testParse("99996059t", .time, false, overflow);
    try testParse("99995960t", .time, false, overflow);

    try testParse("006000000t", .time, false, invalidCharacter(9));
    try testParse("000060000t", .time, false, invalidCharacter(9));
    try testParse("996059999t", .time, false, overflow);
    try testParse("995960999t", .time, false, overflow);

    try testParse("0000006000t", .time, false, invalidCharacter(10));
    try testParse("0000000060t", .time, false, invalidCharacter(10));
    try testParse("9999996059t", .time, false, invalidCharacter(10));
    try testParse("9999995960t", .time, false, overflow);

    try testParse("00000006000t", .time, false, invalidCharacter(11));
    try testParse("00000000060t", .time, false, invalidCharacter(11));
    try testParse("99999996059t", .time, false, invalidCharacter(11));
    try testParse("99999995960t", .time, false, overflow);

    try testParse("000000006000t", .time, false, invalidCharacter(12));
    try testParse("000000000060t", .time, false, invalidCharacter(12));
    try testParse("999999996059t", .time, false, invalidCharacter(12));
    try testParse("999999995960t", .time, false, overflow);

    try testParse("0000000006000t", .time, false, invalidCharacter(13));
    try testParse("0000000000060t", .time, false, invalidCharacter(13));
    try testParse("9999999996059t", .time, false, overflow);
    try testParse("9999999995960t", .time, false, overflow);

    try testParse("00000000006000t", .time, false, invalidCharacter(14));
    try testParse("00000000000060t", .time, false, invalidCharacter(14));
    try testParse("21474836485959t", .time, false, overflow);
    try testParse("21474836476059t", .time, false, overflow);
    try testParse("21474836475960t", .time, false, overflow);

    try testParse("00:00:00.t", .time, false, invalidCharacter(9));
    try testParse("99:59:59.t", .time, false, invalidCharacter(9));

    try testParse("000:00:00.t", .time, false, invalidCharacter(10));
    try testParse("999:59:59.t", .time, false, invalidCharacter(10));

    try testParse("00:0:0.0t", .time, false, invalidCharacter(2));
    try testParse("00:0:00.0t", .time, false, invalidCharacter(2));
    try testParse("00:00:0.0t", .time, false, invalidCharacter(2));

    try testParse("00:60:00.000t", .time, false, overflow);
    try testParse("00:00:60.000t", .time, false, overflow);
    try testParse("99:60:59.999t", .time, false, overflow);
    try testParse("99:59:60.999t", .time, false, overflow);

    try testParse("060t", .time, true, overflow);
    try testParse("960t", .time, true, overflow);

    try testParse("0060t", .time, true, overflow);
    try testParse("9960t", .time, true, overflow);

    try testParse("00060t", .time, true, overflow);
    try testParse("99960t", .time, true, overflow);

    try testParse("006000t", .time, true, overflow);
    try testParse("000060t", .time, true, overflow);
    try testParse("996059t", .time, true, overflow);
    try testParse("995960t", .time, true, overflow);

    try testParse("0006000t", .time, true, overflow);
    try testParse("0000060t", .time, true, overflow);
    try testParse("9996059t", .time, true, overflow);
    try testParse("9995960t", .time, true, overflow);

    try testParse("00006000t", .time, true, overflow);
    try testParse("00000060t", .time, true, overflow);
    try testParse("99996059t", .time, true, overflow);
    try testParse("99995960t", .time, true, overflow);

    try testParse("006000000t", .time, true, overflow);
    try testParse("000060000t", .time, true, overflow);
    try testParse("996059999t", .time, true, overflow);
    try testParse("995960999t", .time, true, overflow);

    try testParse("0000006000t", .time, true, overflow);
    try testParse("0000000060t", .time, true, overflow);
    try testParse("9999996059t", .time, true, overflow);
    try testParse("9999995960t", .time, true, overflow);

    try testParse("00000006000t", .time, true, overflow);
    try testParse("00000000060t", .time, true, overflow);
    try testParse("99999996059t", .time, true, overflow);
    try testParse("99999995960t", .time, true, overflow);

    try testParse("000000006000t", .time, true, overflow);
    try testParse("000000000060t", .time, true, overflow);
    try testParse("999999996059t", .time, true, overflow);
    try testParse("999999995960t", .time, true, overflow);

    try testParse("0000000006000t", .time, true, overflow);
    try testParse("0000000000060t", .time, true, overflow);
    try testParse("9999999996059t", .time, true, overflow);
    try testParse("9999999995960t", .time, true, overflow);

    try testParse("00000000006000t", .time, true, overflow);
    try testParse("00000000000060t", .time, true, overflow);
    try testParse("21474836485959t", .time, true, overflow);
    try testParse("21474836476059t", .time, true, overflow);
    try testParse("21474836475960t", .time, true, overflow);

    try testParse("00:00:00.t", .time, true, invalidCharacter(2));
    try testParse("99:59:59.t", .time, true, invalidCharacter(2));

    try testParse("000:00:00.t", .time, true, .{ .time = 0 });
    try testParse("999:59:59.t", .time, true, overflow);

    try testParse("00:0:0.0t", .time, true, invalidCharacter(2));
    try testParse("00:0:00.0t", .time, true, invalidCharacter(2));
    try testParse("00:00:0.0t", .time, true, invalidCharacter(2));

    try testParse("00:60:00.000t", .time, true, overflow);
    try testParse("00:00:60.000t", .time, true, overflow);
    try testParse("99:60:59.999t", .time, true, overflow);
    try testParse("99:59:60.999t", .time, true, overflow);
}
