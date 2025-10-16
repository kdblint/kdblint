const std = @import("std");

const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;
const null_int = common.null_int;
const inf_int = common.inf_int;

pub fn parseSecond(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .second = null_int },
        'W', 'w' => return .{ .second = inf_int },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'v' => .{ .second = null_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'v' => .{ .second = inf_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'v') bytes[0 .. bytes.len - 1] else bytes;
    switch (slice.len) {
        0 => unreachable,
        1, 2 => {
            const hour_value = switch (parseSlice(i32, slice)) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const seconds = hour_value * std.time.s_per_hour;
            return .{ .second = seconds };
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
            const seconds = hour_value * std.time.s_per_hour + minute_value * std.time.s_per_min;
            return .{ .second = seconds };
        },
        8, 9 => if (slice[slice.len - 6] == ':' and slice[slice.len - 3] == ':') {
            const hour_value = switch (parseSlice(i32, slice[0 .. slice.len - 6])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const minute_value = switch (parseSlice(i32, slice[slice.len - 5 .. slice.len - 3])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 5,
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
            const seconds = hour_value * std.time.s_per_hour + minute_value * std.time.s_per_min + second_value;
            return .{ .second = seconds };
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

    var seconds: i32 = 0;
    {
        const res = @mulWithOverflow(hour_value, std.time.s_per_hour);
        if (res[1] != 0) return .{ .failure = .overflow };
        seconds = res[0];
    }
    {
        const res = @addWithOverflow(seconds, minute_value * std.time.s_per_min);
        if (res[1] != 0) return .{ .failure = .overflow };
        seconds = res[0];
    }
    const res = @addWithOverflow(seconds, second_value);
    if (res[1] != 0) return .{ .failure = .overflow };
    seconds = res[0];

    return .{ .second = seconds };
}

test "parse number literal - second" {
    try testParse("0n", .second, false, .{ .second = null_int });
    try testParse("0N", .second, false, .{ .second = null_int });
    try testParse("0w", .second, false, .{ .second = inf_int });
    try testParse("0W", .second, false, .{ .second = inf_int });

    try testParse("0", .second, false, .{ .second = 0 });
    try testParse("9", .second, false, .{ .second = 32400 });

    try testParse("00", .second, false, .{ .second = 0 });
    try testParse("99", .second, false, .{ .second = 356400 });

    try testParse("000", .second, false, .{ .second = 0 });
    try testParse("959", .second, false, .{ .second = 35940 });

    try testParse("0000", .second, false, .{ .second = 0 });
    try testParse("9959", .second, false, .{ .second = 359940 });

    try testParse("00000", .second, false, .{ .second = 0 });
    try testParse("99959", .second, false, .{ .second = 3599940 });

    try testParse("000000", .second, false, .{ .second = 0 });
    try testParse("995959", .second, false, .{ .second = 359999 });

    try testParse("0000000", .second, false, .{ .second = 0 });
    try testParse("9995959", .second, false, .{ .second = 3599999 });

    try testParse("00000000", .second, false, .{ .second = 0 });
    try testParse("99995959", .second, false, .{ .second = 35999999 });

    try testParse("000000000", .second, false, .{ .second = 0 });
    try testParse("999995959", .second, false, .{ .second = 359999999 });

    try testParse("0000000000", .second, false, .{ .second = 0 });
    try testParse("9999995959", .second, false, overflow);

    try testParse("00:00:00", .second, false, .{ .second = 0 });
    try testParse("99:59:59", .second, false, .{ .second = 359999 });

    try testParse("000:00:00", .second, false, .{ .second = 0 });
    try testParse("999:59:59", .second, false, .{ .second = 3599999 });

    try testParse("0n", .second, true, .{ .second = null_int });
    try testParse("0N", .second, true, .{ .second = null_int });
    try testParse("0w", .second, true, .{ .second = inf_int });
    try testParse("0W", .second, true, .{ .second = inf_int });

    try testParse("0", .second, true, .{ .second = 0 });
    try testParse("9", .second, true, .{ .second = 32400 });

    try testParse("00", .second, true, .{ .second = 0 });
    try testParse("99", .second, true, .{ .second = 356400 });

    try testParse("000", .second, true, .{ .second = 0 });
    try testParse("959", .second, true, .{ .second = 35940 });

    try testParse("0000", .second, true, .{ .second = 0 });
    try testParse("9959", .second, true, .{ .second = 359940 });

    try testParse("00000", .second, true, .{ .second = 0 });
    try testParse("99959", .second, true, .{ .second = 3599940 });

    try testParse("000000", .second, true, .{ .second = 0 });
    try testParse("995959", .second, true, .{ .second = 359999 });

    try testParse("0000000", .second, true, .{ .second = 0 });
    try testParse("9995959", .second, true, .{ .second = 3599999 });

    try testParse("00000000", .second, true, .{ .second = 0 });
    try testParse("99995959", .second, true, .{ .second = 35999999 });

    try testParse("000000000", .second, true, .{ .second = 0 });
    try testParse("999995959", .second, true, .{ .second = 359999999 });

    try testParse("0000000000", .second, true, .{ .second = 0 });
    try testParse("9999995959", .second, true, overflow);

    try testParse("00:00:00", .second, true, .{ .second = 0 });
    try testParse("99:59:59", .second, true, .{ .second = 359999 });

    try testParse("000:00:00", .second, true, .{ .second = 0 });
    try testParse("999:59:59", .second, true, .{ .second = 3599999 });

    try testParse("0nv", .second, false, invalidCharacter(2));
    try testParse("0Nv", .second, false, invalidCharacter(2));
    try testParse("0wv", .second, false, invalidCharacter(2));
    try testParse("0Wv", .second, false, invalidCharacter(2));

    try testParse("0v", .second, false, invalidCharacter(1));
    try testParse("9v", .second, false, invalidCharacter(1));

    try testParse("00v", .second, false, invalidCharacter(2));
    try testParse("99v", .second, false, invalidCharacter(2));

    try testParse("000v", .second, false, invalidCharacter(3));
    try testParse("959v", .second, false, invalidCharacter(3));

    try testParse("0000v", .second, false, invalidCharacter(4));
    try testParse("9959v", .second, false, invalidCharacter(4));

    try testParse("00000v", .second, false, invalidCharacter(5));
    try testParse("99959v", .second, false, overflow);

    try testParse("000000v", .second, false, invalidCharacter(6));
    try testParse("995959v", .second, false, overflow);

    try testParse("0000000v", .second, false, invalidCharacter(7));
    try testParse("9995959v", .second, false, overflow);

    try testParse("00000000v", .second, false, invalidCharacter(8));
    try testParse("99995959v", .second, false, overflow);

    try testParse("000000000v", .second, false, invalidCharacter(9));
    try testParse("999995959v", .second, false, overflow);

    try testParse("0000000000v", .second, false, invalidCharacter(10));
    try testParse("9999995959v", .second, false, overflow);

    try testParse("00:00:00v", .second, false, invalidCharacter(2));
    try testParse("99:59:59v", .second, false, invalidCharacter(2));

    try testParse("000:00:00v", .second, false, invalidCharacter(3));
    try testParse("999:59:59v", .second, false, invalidCharacter(3));

    try testParse("0nv", .second, true, .{ .second = null_int });
    try testParse("0Nv", .second, true, .{ .second = null_int });
    try testParse("0wv", .second, true, .{ .second = inf_int });
    try testParse("0Wv", .second, true, .{ .second = inf_int });

    try testParse("0v", .second, true, .{ .second = 0 });
    try testParse("9v", .second, true, .{ .second = 32400 });

    try testParse("00v", .second, true, .{ .second = 0 });
    try testParse("99v", .second, true, .{ .second = 356400 });

    try testParse("000v", .second, true, .{ .second = 0 });
    try testParse("959v", .second, true, .{ .second = 35940 });

    try testParse("0000v", .second, true, .{ .second = 0 });
    try testParse("9959v", .second, true, .{ .second = 359940 });

    try testParse("00000v", .second, true, .{ .second = 0 });
    try testParse("99959v", .second, true, .{ .second = 3599940 });

    try testParse("000000v", .second, true, .{ .second = 0 });
    try testParse("995959v", .second, true, .{ .second = 359999 });

    try testParse("0000000v", .second, true, .{ .second = 0 });
    try testParse("9995959v", .second, true, .{ .second = 3599999 });

    try testParse("00000000v", .second, true, .{ .second = 0 });
    try testParse("99995959v", .second, true, .{ .second = 35999999 });

    try testParse("000000000v", .second, true, .{ .second = 0 });
    try testParse("999995959v", .second, true, .{ .second = 359999999 });

    try testParse("0000000000v", .second, true, .{ .second = 0 });
    try testParse("9999995959v", .second, true, overflow);

    try testParse("00:00:00v", .second, true, .{ .second = 0 });
    try testParse("99:59:59v", .second, true, .{ .second = 359999 });

    try testParse("000:00:00v", .second, true, .{ .second = 0 });
    try testParse("999:59:59v", .second, true, .{ .second = 3599999 });

    try testParse("00:0:0", .second, false, invalidCharacter(2));
    try testParse("00:0:00", .second, false, invalidCharacter(2));
    try testParse("00:00:0", .second, false, invalidCharacter(2));

    try testParse("00:60:00", .second, false, overflow);
    try testParse("00:00:60", .second, false, overflow);
    try testParse("99:60:59", .second, false, overflow);
    try testParse("99:59:60", .second, false, overflow);

    try testParse("000:60:00", .second, false, overflow);
    try testParse("000:00:60", .second, false, overflow);
    try testParse("999:60:59", .second, false, overflow);
    try testParse("999:59:60", .second, false, overflow);
    try testParse("999:60:59", .second, false, overflow);
    try testParse("999:59:60", .second, false, overflow);

    try testParse("060", .second, false, overflow);
    try testParse("960", .second, false, overflow);

    try testParse("0060", .second, false, overflow);
    try testParse("9960", .second, false, overflow);

    try testParse("00060", .second, false, overflow);
    try testParse("99960", .second, false, overflow);

    try testParse("006000", .second, false, overflow);
    try testParse("000060", .second, false, overflow);
    try testParse("996059", .second, false, overflow);
    try testParse("995960", .second, false, overflow);

    try testParse("000000006000", .second, false, overflow);
    try testParse("000000000060", .second, false, overflow);
    try testParse("21474836485959", .second, false, overflow);
    try testParse("21474836476059", .second, false, overflow);
    try testParse("21474836475960", .second, false, overflow);

    try testParse("00:0:0", .second, true, invalidCharacter(2));
    try testParse("00:0:00", .second, true, invalidCharacter(2));
    try testParse("00:00:0", .second, true, invalidCharacter(2));

    try testParse("00:60:00", .second, true, overflow);
    try testParse("00:00:60", .second, true, overflow);
    try testParse("99:60:59", .second, true, overflow);
    try testParse("99:59:60", .second, true, overflow);

    try testParse("000:60:00", .second, true, overflow);
    try testParse("000:00:60", .second, true, overflow);
    try testParse("999:60:59", .second, true, overflow);
    try testParse("999:59:60", .second, true, overflow);
    try testParse("999:60:59", .second, true, overflow);
    try testParse("999:59:60", .second, true, overflow);

    try testParse("060", .second, true, overflow);
    try testParse("960", .second, true, overflow);

    try testParse("0060", .second, true, overflow);
    try testParse("9960", .second, true, overflow);

    try testParse("00060", .second, true, overflow);
    try testParse("99960", .second, true, overflow);

    try testParse("006000", .second, true, overflow);
    try testParse("000060", .second, true, overflow);
    try testParse("996059", .second, true, overflow);
    try testParse("995960", .second, true, overflow);

    try testParse("000000006000", .second, true, overflow);
    try testParse("000000000060", .second, true, overflow);
    try testParse("21474836485959", .second, true, overflow);
    try testParse("21474836476059", .second, true, overflow);
    try testParse("21474836475960", .second, true, overflow);

    try testParse("00:0:0v", .second, false, invalidCharacter(2));
    try testParse("00:0:00v", .second, false, invalidCharacter(2));
    try testParse("00:00:0v", .second, false, invalidCharacter(7));

    try testParse("00:60:00v", .second, false, invalidCharacter(2));
    try testParse("00:00:60v", .second, false, invalidCharacter(2));
    try testParse("99:60:59v", .second, false, invalidCharacter(2));
    try testParse("99:59:60v", .second, false, invalidCharacter(2));

    try testParse("000:60:00v", .second, false, invalidCharacter(3));
    try testParse("000:00:60v", .second, false, invalidCharacter(3));
    try testParse("999:60:59v", .second, false, invalidCharacter(3));
    try testParse("999:59:60v", .second, false, invalidCharacter(3));
    try testParse("999:60:59v", .second, false, invalidCharacter(3));
    try testParse("999:59:60v", .second, false, invalidCharacter(3));

    try testParse("060v", .second, false, invalidCharacter(3));
    try testParse("960v", .second, false, invalidCharacter(3));

    try testParse("0060v", .second, false, invalidCharacter(4));
    try testParse("9960v", .second, false, invalidCharacter(4));

    try testParse("00060v", .second, false, invalidCharacter(5));
    try testParse("99960v", .second, false, overflow);

    try testParse("006000v", .second, false, invalidCharacter(6));
    try testParse("000060v", .second, false, invalidCharacter(6));
    try testParse("996059v", .second, false, invalidCharacter(6));
    try testParse("995960v", .second, false, overflow);

    try testParse("000000006000v", .second, false, invalidCharacter(12));
    try testParse("000000000060v", .second, false, invalidCharacter(12));
    try testParse("21474836485959v", .second, false, overflow);
    try testParse("21474836476059v", .second, false, overflow);
    try testParse("21474836475960v", .second, false, overflow);

    try testParse("00:0:0v", .second, true, invalidCharacter(2));
    try testParse("00:0:00v", .second, true, invalidCharacter(2));
    try testParse("00:00:0v", .second, true, invalidCharacter(2));

    try testParse("00:60:00v", .second, true, overflow);
    try testParse("00:00:60v", .second, true, overflow);
    try testParse("99:60:59v", .second, true, overflow);
    try testParse("99:59:60v", .second, true, overflow);

    try testParse("000:60:00v", .second, true, overflow);
    try testParse("000:00:60v", .second, true, overflow);
    try testParse("999:60:59v", .second, true, overflow);
    try testParse("999:59:60v", .second, true, overflow);
    try testParse("999:60:59v", .second, true, overflow);
    try testParse("999:59:60v", .second, true, overflow);

    try testParse("060v", .second, true, overflow);
    try testParse("960v", .second, true, overflow);

    try testParse("0060v", .second, true, overflow);
    try testParse("9960v", .second, true, overflow);

    try testParse("00060v", .second, true, overflow);
    try testParse("99960v", .second, true, overflow);

    try testParse("006000v", .second, true, overflow);
    try testParse("000060v", .second, true, overflow);
    try testParse("996059v", .second, true, overflow);
    try testParse("995960v", .second, true, overflow);

    try testParse("000000006000v", .second, true, overflow);
    try testParse("000000000060v", .second, true, overflow);
    try testParse("21474836485959v", .second, true, overflow);
    try testParse("21474836476059v", .second, true, overflow);
    try testParse("21474836475960v", .second, true, overflow);
}
