const std = @import("std");

const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;

const null_int = common.null_int;
const inf_int = common.inf_int;

pub fn parseMinute(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .minute = null_int },
        'W', 'w' => return .{ .minute = inf_int },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'u' => .{ .minute = null_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'u' => .{ .minute = inf_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'u') bytes[0 .. bytes.len - 1] else bytes;
    switch (slice.len) {
        0 => unreachable,
        1, 2 => {
            const minute_value = switch (parseSlice(i32, slice)) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const seconds = minute_value * std.time.s_per_min;
            return .{ .minute = seconds };
        },
        5, 6 => if (slice[slice.len - 3] == ':') {
            const minute_value = switch (parseSlice(i32, slice[0 .. slice.len - 3])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const second_value = switch (parseSlice(i32, slice[slice.len - 2 ..])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 2,
                } },
                .int => |int| int,
            };
            if (second_value > 59) return .{ .failure = .overflow };
            const seconds = minute_value * std.time.s_per_min + second_value;
            return .{ .minute = seconds };
        },
        else => {},
    }

    const minute_value = switch (parseSlice(i32, slice[0 .. slice.len - 2])) {
        .overflow => return .{ .failure = .overflow },
        .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
        .int => |int| int,
    };
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
        const res = @mulWithOverflow(minute_value, std.time.s_per_min);
        if (res[1] != 0) return .{ .failure = .overflow };
        seconds = res[0];
    }
    const res = @addWithOverflow(seconds, second_value);
    if (res[1] != 0) return .{ .failure = .overflow };
    seconds = res[0];

    return .{ .minute = seconds };
}

test "parse number literal - minute" {
    const _std = @import("std");
    const _clock: _std.Io.Clock = .real;
    const _start = try _clock.now(_std.testing.io);
    const _file: _std.fs.File = .adaptFromNewApi(try _std.Io.Dir.cwd().createFile(_std.testing.io, @src().fn_name ++ ".log", .{}));
    defer _file.close();
    var _file_writer = _file.writer(&.{});
    const _writer = &_file_writer.interface;
    try _writer.writeAll(@src().fn_name);
    try _writer.flush();
    defer {
        _writer.print(": {d}ms\n", .{_start.durationTo(_clock.now(_std.testing.io) catch unreachable).toMilliseconds()}) catch unreachable;
        _writer.flush() catch unreachable;
    }
    try testParse("0n", .minute, false, .{ .minute = null_int });
    try testParse("0N", .minute, false, .{ .minute = null_int });
    try testParse("0w", .minute, false, .{ .minute = inf_int });
    try testParse("0W", .minute, false, .{ .minute = inf_int });

    try testParse("0", .minute, false, .{ .minute = 0 });
    try testParse("9", .minute, false, .{ .minute = 540 });

    try testParse("00", .minute, false, .{ .minute = 0 });
    try testParse("99", .minute, false, .{ .minute = 5940 });

    try testParse("000", .minute, false, .{ .minute = 0 });
    try testParse("959", .minute, false, .{ .minute = 599 });

    try testParse("0000", .minute, false, .{ .minute = 0 });
    try testParse("9959", .minute, false, .{ .minute = 5999 });

    try testParse("00000", .minute, false, .{ .minute = 0 });
    try testParse("99959", .minute, false, .{ .minute = 59999 });

    try testParse("000000", .minute, false, .{ .minute = 0 });
    try testParse("999959", .minute, false, .{ .minute = 599999 });

    try testParse("0000000", .minute, false, .{ .minute = 0 });
    try testParse("9999959", .minute, false, .{ .minute = 5999999 });

    try testParse("00000000", .minute, false, .{ .minute = 0 });
    try testParse("99999959", .minute, false, .{ .minute = 59999999 });

    try testParse("000000000", .minute, false, .{ .minute = 0 });
    try testParse("999999959", .minute, false, .{ .minute = 599999999 });

    try testParse("0000000000", .minute, false, .{ .minute = 0 });
    try testParse("9999999959", .minute, false, overflow);

    try testParse("00:00", .minute, false, .{ .minute = 0 });
    try testParse("99:59", .minute, false, .{ .minute = 5999 });

    try testParse("000:00", .minute, false, .{ .minute = 0 });
    try testParse("999:59", .minute, false, .{ .minute = 59999 });

    try testParse("0n", .minute, true, .{ .minute = null_int });
    try testParse("0N", .minute, true, .{ .minute = null_int });
    try testParse("0w", .minute, true, .{ .minute = inf_int });
    try testParse("0W", .minute, true, .{ .minute = inf_int });

    try testParse("0", .minute, true, .{ .minute = 0 });
    try testParse("9", .minute, true, .{ .minute = 540 });

    try testParse("00", .minute, true, .{ .minute = 0 });
    try testParse("99", .minute, true, .{ .minute = 5940 });

    try testParse("000", .minute, true, .{ .minute = 0 });
    try testParse("959", .minute, true, .{ .minute = 599 });

    try testParse("0000", .minute, true, .{ .minute = 0 });
    try testParse("9959", .minute, true, .{ .minute = 5999 });

    try testParse("00000", .minute, true, .{ .minute = 0 });
    try testParse("99959", .minute, true, .{ .minute = 59999 });

    try testParse("000000", .minute, true, .{ .minute = 0 });
    try testParse("999959", .minute, true, .{ .minute = 599999 });

    try testParse("0000000", .minute, true, .{ .minute = 0 });
    try testParse("9999959", .minute, true, .{ .minute = 5999999 });

    try testParse("00000000", .minute, true, .{ .minute = 0 });
    try testParse("99999959", .minute, true, .{ .minute = 59999999 });

    try testParse("000000000", .minute, true, .{ .minute = 0 });
    try testParse("999999959", .minute, true, .{ .minute = 599999999 });

    try testParse("0000000000", .minute, true, .{ .minute = 0 });
    try testParse("9999999959", .minute, true, overflow);

    try testParse("00:00", .minute, true, .{ .minute = 0 });
    try testParse("99:59", .minute, true, .{ .minute = 5999 });

    try testParse("000:00", .minute, true, .{ .minute = 0 });
    try testParse("999:59", .minute, true, .{ .minute = 59999 });

    try testParse("0nu", .minute, false, invalidCharacter(2));
    try testParse("0Nu", .minute, false, invalidCharacter(2));
    try testParse("0wu", .minute, false, invalidCharacter(2));
    try testParse("0Wu", .minute, false, invalidCharacter(2));

    try testParse("0u", .minute, false, invalidCharacter(1));
    try testParse("9u", .minute, false, invalidCharacter(1));

    try testParse("00u", .minute, false, invalidCharacter(2));
    try testParse("99u", .minute, false, invalidCharacter(2));

    try testParse("000u", .minute, false, invalidCharacter(3));
    try testParse("959u", .minute, false, invalidCharacter(3));

    try testParse("0000u", .minute, false, invalidCharacter(4));
    try testParse("9959u", .minute, false, invalidCharacter(4));

    try testParse("00000u", .minute, false, invalidCharacter(5));
    try testParse("99959u", .minute, false, invalidCharacter(5));

    try testParse("000000u", .minute, false, invalidCharacter(6));
    try testParse("999959u", .minute, false, invalidCharacter(6));

    try testParse("0000000u", .minute, false, invalidCharacter(7));
    try testParse("9999959u", .minute, false, invalidCharacter(7));

    try testParse("00000000u", .minute, false, invalidCharacter(8));
    try testParse("99999959u", .minute, false, invalidCharacter(8));

    try testParse("000000000u", .minute, false, invalidCharacter(9));
    try testParse("999999959u", .minute, false, invalidCharacter(9));

    try testParse("0000000000u", .minute, false, invalidCharacter(10));
    try testParse("9999999959u", .minute, false, invalidCharacter(10));

    try testParse("00:00u", .minute, false, invalidCharacter(2));
    try testParse("99:59u", .minute, false, invalidCharacter(2));

    try testParse("000:00u", .minute, false, invalidCharacter(3));
    try testParse("999:59u", .minute, false, invalidCharacter(3));

    try testParse("0nu", .minute, true, .{ .minute = null_int });
    try testParse("0Nu", .minute, true, .{ .minute = null_int });
    try testParse("0wu", .minute, true, .{ .minute = inf_int });
    try testParse("0Wu", .minute, true, .{ .minute = inf_int });

    try testParse("0u", .minute, true, .{ .minute = 0 });
    try testParse("9u", .minute, true, .{ .minute = 540 });

    try testParse("00u", .minute, true, .{ .minute = 0 });
    try testParse("99u", .minute, true, .{ .minute = 5940 });

    try testParse("000u", .minute, true, .{ .minute = 0 });
    try testParse("959u", .minute, true, .{ .minute = 599 });

    try testParse("0000u", .minute, true, .{ .minute = 0 });
    try testParse("9959u", .minute, true, .{ .minute = 5999 });

    try testParse("00000u", .minute, true, .{ .minute = 0 });
    try testParse("99959u", .minute, true, .{ .minute = 59999 });

    try testParse("000000u", .minute, true, .{ .minute = 0 });
    try testParse("999959u", .minute, true, .{ .minute = 599999 });

    try testParse("0000000u", .minute, true, .{ .minute = 0 });
    try testParse("9999959u", .minute, true, .{ .minute = 5999999 });

    try testParse("00000000u", .minute, true, .{ .minute = 0 });
    try testParse("99999959u", .minute, true, .{ .minute = 59999999 });

    try testParse("000000000u", .minute, true, .{ .minute = 0 });
    try testParse("999999959u", .minute, true, .{ .minute = 599999999 });

    try testParse("0000000000u", .minute, true, .{ .minute = 0 });
    try testParse("9999999959u", .minute, true, overflow);

    try testParse("00:00u", .minute, true, .{ .minute = 0 });
    try testParse("99:59u", .minute, true, .{ .minute = 5999 });

    try testParse("000:00u", .minute, true, .{ .minute = 0 });
    try testParse("999:59u", .minute, true, .{ .minute = 59999 });

    try testParse("00:", .minute, false, invalidCharacter(2));
    try testParse("00:0", .minute, false, invalidCharacter(2));
    try testParse("00:60", .minute, false, overflow);
    try testParse("000:", .minute, false, invalidCharacter(3));
    try testParse("000:0", .minute, false, invalidCharacter(3));
    try testParse("000:60", .minute, false, overflow);

    try testParse("060", .minute, false, overflow);
    try testParse("960", .minute, false, overflow);

    try testParse("00:", .minute, true, invalidCharacter(2));
    try testParse("00:0", .minute, true, invalidCharacter(2));
    try testParse("00:60", .minute, true, overflow);
    try testParse("000:", .minute, true, invalidCharacter(3));
    try testParse("000:0", .minute, true, invalidCharacter(3));
    try testParse("000:60", .minute, true, overflow);

    try testParse("060", .minute, true, overflow);
    try testParse("960", .minute, true, overflow);

    try testParse("00:u", .minute, false, invalidCharacter(2));
    try testParse("00:0u", .minute, false, invalidCharacter(4));
    try testParse("00:60u", .minute, false, invalidCharacter(2));
    try testParse("000:u", .minute, false, invalidCharacter(3));
    try testParse("000:0u", .minute, false, invalidCharacter(5));
    try testParse("000:60u", .minute, false, invalidCharacter(3));

    try testParse("060u", .minute, false, invalidCharacter(3));
    try testParse("960u", .minute, false, invalidCharacter(3));

    try testParse("00:u", .minute, true, invalidCharacter(2));
    try testParse("00:0u", .minute, true, invalidCharacter(2));
    try testParse("00:60u", .minute, true, overflow);
    try testParse("000:u", .minute, true, invalidCharacter(3));
    try testParse("000:0u", .minute, true, invalidCharacter(3));
    try testParse("000:60u", .minute, true, overflow);

    try testParse("060u", .minute, true, overflow);
    try testParse("960u", .minute, true, overflow);
}
