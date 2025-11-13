const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;

const null_int = common.null_int;
const inf_int = common.inf_int;

pub fn parseMonth(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .month = null_int },
        'W', 'w' => return .{ .month = inf_int },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'm' => .{ .month = null_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'm' => .{ .month = inf_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'm') bytes[0 .. bytes.len - 1] else bytes;
    switch (slice.len) {
        4 => {
            const year_value: i32 = switch (parseSlice(i32, slice[0..2])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const month_value: i32 = switch (parseSlice(i32, slice[2..4])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + 2,
                } },
                .int => |int| int,
            };
            const months = calculateMonths(
                if (year_value < 50) year_value + 2000 else year_value + 1900,
                month_value,
            ) catch return .{ .failure = .overflow };
            return .{ .month = months };
        },
        6, 7 => {
            const year_value: i32 = switch (parseSlice(i32, slice[0..4])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const month_value: i32 = switch (parseSlice(i32, slice[(slice.len - 2)..])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 2,
                } },
                .int => |int| int,
            };
            const months = calculateMonths(
                year_value,
                month_value,
            ) catch return .{ .failure = .overflow };
            return .{ .month = months };
        },
        else => return .{ .failure = .{ .invalid_character = slice.len - 1 } },
    }
}

fn calculateMonths(y: i32, m: i32) error{Overflow}!i32 {
    if (y == 0) return error.Overflow;
    if (m == 0 or m > 12) return error.Overflow;
    return (y - 2000) * 12 + m - 1;
}

test "parse number literal - month" {
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
    try testParse("0n", .month, false, .{ .month = null_int });
    try testParse("0N", .month, false, .{ .month = null_int });
    try testParse("0w", .month, false, .{ .month = inf_int });
    try testParse("0W", .month, false, .{ .month = inf_int });
    try testParse("1999.12", .month, false, .{ .month = -1 });
    try testParse("2000.01", .month, false, .{ .month = 0 });
    try testParse("2000.02", .month, false, .{ .month = 1 });
    try testParse("0001.01", .month, false, .{ .month = -23988 });
    try testParse("9999.12", .month, false, .{ .month = 95999 });

    try testParse("0001", .month, false, .{ .month = 0 });
    try testParse("0012", .month, false, .{ .month = 11 });
    try testParse("4912", .month, false, .{ .month = 599 });
    try testParse("5001", .month, false, .{ .month = -600 });
    try testParse("9901", .month, false, .{ .month = -12 });
    try testParse("9912", .month, false, .{ .month = -1 });

    try testParse("000101", .month, false, .{ .month = -23988 });
    try testParse("000112", .month, false, .{ .month = -23977 });
    try testParse("999901", .month, false, .{ .month = 95988 });
    try testParse("999912", .month, false, .{ .month = 95999 });

    try testParse("0nm", .month, true, .{ .month = null_int });
    try testParse("0Nm", .month, true, .{ .month = null_int });
    try testParse("0wm", .month, true, .{ .month = inf_int });
    try testParse("0Wm", .month, true, .{ .month = inf_int });
    try testParse("1999.12m", .month, true, .{ .month = -1 });
    try testParse("2000.01m", .month, true, .{ .month = 0 });
    try testParse("2000.02m", .month, true, .{ .month = 1 });
    try testParse("0001.01m", .month, true, .{ .month = -23988 });
    try testParse("9999.12m", .month, true, .{ .month = 95999 });

    try testParse("0001m", .month, true, .{ .month = 0 });
    try testParse("0012m", .month, true, .{ .month = 11 });
    try testParse("4912m", .month, true, .{ .month = 599 });
    try testParse("5001m", .month, true, .{ .month = -600 });
    try testParse("9901m", .month, true, .{ .month = -12 });
    try testParse("9912m", .month, true, .{ .month = -1 });

    try testParse("000101m", .month, true, .{ .month = -23988 });
    try testParse("000112m", .month, true, .{ .month = -23977 });
    try testParse("999901m", .month, true, .{ .month = 95988 });
    try testParse("999912m", .month, true, .{ .month = 95999 });

    try testParse("0", .month, false, invalidCharacter(0));
    try testParse("1", .month, false, invalidCharacter(0));
    try testParse("9", .month, false, invalidCharacter(0));

    try testParse("00", .month, false, invalidCharacter(1));
    try testParse("11", .month, false, invalidCharacter(1));
    try testParse("99", .month, false, invalidCharacter(1));

    try testParse("000", .month, false, invalidCharacter(2));
    try testParse("111", .month, false, invalidCharacter(2));
    try testParse("999", .month, false, invalidCharacter(2));

    try testParse("0000", .month, false, overflow);
    try testParse("0013", .month, false, overflow);
    try testParse("9999", .month, false, overflow);

    try testParse("00000", .month, false, invalidCharacter(4));
    try testParse("11111", .month, false, invalidCharacter(4));
    try testParse("99999", .month, false, invalidCharacter(4));

    try testParse("000000", .month, false, overflow);
    try testParse("000113", .month, false, overflow);
    try testParse("999999", .month, false, overflow);

    try testParse("1111111", .month, false, .{ .month = -10658 });

    try testParse("2000.1", .month, false, invalidCharacter(4));
    try testParse("2000.00", .month, false, overflow);
    try testParse("2000.13", .month, false, overflow);

    try testParse("0m", .month, true, invalidCharacter(0));
    try testParse("1m", .month, true, invalidCharacter(0));
    try testParse("9m", .month, true, invalidCharacter(0));

    try testParse("00m", .month, true, invalidCharacter(1));
    try testParse("11m", .month, true, invalidCharacter(1));
    try testParse("99m", .month, true, invalidCharacter(1));

    try testParse("000m", .month, true, invalidCharacter(2));
    try testParse("111m", .month, true, invalidCharacter(2));
    try testParse("999m", .month, true, invalidCharacter(2));

    try testParse("0000m", .month, true, overflow);
    try testParse("0013m", .month, true, overflow);
    try testParse("9999m", .month, true, overflow);

    try testParse("00000m", .month, true, invalidCharacter(4));
    try testParse("11111m", .month, true, invalidCharacter(4));
    try testParse("99999m", .month, true, invalidCharacter(4));

    try testParse("000000m", .month, true, overflow);
    try testParse("000113m", .month, true, overflow);
    try testParse("999999m", .month, true, overflow);

    try testParse("1111111m", .month, true, .{ .month = -10658 });

    try testParse("2000.1m", .month, true, invalidCharacter(4));
    try testParse("2000.00m", .month, true, overflow);
    try testParse("2000.13m", .month, true, overflow);
}
