const std = @import("std");
const assert = std.debug.assert;

const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;

const null_int = common.null_int;
const inf_int = common.inf_int;

pub fn parseDate(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .date = null_int },
        'W', 'w' => return .{ .date = inf_int },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'd' => .{ .date = null_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'd' => .{ .date = inf_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'd') bytes[0 .. bytes.len - 1] else bytes;
    switch (slice.len) {
        6 => {
            const year_value: u32 = switch (parseSlice(u32, slice[0..2])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const month_value: u32 = switch (parseSlice(u32, slice[2..4])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 2 } },
                .int => |int| int,
            };
            const day_value: u32 = switch (parseSlice(u32, slice[4..6])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 4 } },
                .int => |int| int,
            };
            const days = calculateDays(
                if (year_value < 50) year_value + 2000 else year_value + 1900,
                month_value,
                day_value,
            ) catch return .{ .failure = .overflow };
            return .{ .date = days };
        },
        8 => {
            const year_value: u32 = switch (parseSlice(u32, slice[0..4])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const month_value: u32 = switch (parseSlice(u32, slice[4..6])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 4 } },
                .int => |int| int,
            };
            const day_value: u32 = switch (parseSlice(u32, slice[6..8])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 6 } },
                .int => |int| int,
            };
            const days = calculateDays(
                year_value,
                month_value,
                day_value,
            ) catch return .{ .failure = .overflow };
            return .{ .date = days };
        },
        10 => {
            if (slice[4] != '.') return .{ .failure = .{ .invalid_character = 4 } };
            if (slice[7] != '.') return .{ .failure = .{ .invalid_character = 7 } };
            const year_value: u32 = switch (parseSlice(u32, slice[0..4])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const month_value: u32 = switch (parseSlice(u32, slice[5..7])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 5 } },
                .int => |int| int,
            };
            const day_value: u32 = switch (parseSlice(u32, slice[8..10])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + 8 } },
                .int => |int| int,
            };
            const days = calculateDays(
                year_value,
                month_value,
                day_value,
            ) catch return .{ .failure = .overflow };
            return .{ .date = days };
        },
        else => return .{ .failure = .{ .invalid_character = slice.len - 1 } },
    }
}

/// https://howardhinnant.github.io/date_algorithms.html#days_from_civil
fn calculateDays(y: u32, m: u32, d: u32) error{Overflow}!i32 {
    if (y == 0) return error.Overflow;
    if (m == 0 or m > 12) return error.Overflow;
    if (d == 0 or d > lastDayOfMonth(y, m)) return error.Overflow;

    const year = if (m <= 2) y - 1 else y;
    const era = @divFloor(if (year >= 0) year else year - 399, 400);
    const yoe = year - era * 400;
    const doy = @divFloor(153 * (if (m > 2) m - 3 else m + 9) + 2, 5) + d - 1;
    const doe = yoe * 365 + @divFloor(yoe, 4) - @divFloor(yoe, 100) + doy;
    return @as(i32, @intCast(era)) * 146097 + @as(i32, @intCast(doe)) - 730425;
}

fn isLeap(y: u32) bool {
    return y % 4 == 0 and (y % 100 != 0 or y % 400 == 0);
}

fn lastDayOfMonth(y: u32, m: u32) u32 {
    assert(m >= 1 and m <= 12);
    return switch (m) {
        1 => 31,
        2 => if (isLeap(y)) 29 else 28,
        3 => 31,
        4 => 30,
        5 => 31,
        6 => 30,
        7 => 31,
        8 => 31,
        9 => 30,
        10 => 31,
        11 => 30,
        12 => 31,
        else => unreachable,
    };
}

test "parse number literal - date" {
    try testParse("0n", .date, false, .{ .date = null_int });
    try testParse("0N", .date, false, .{ .date = null_int });
    try testParse("0w", .date, false, .{ .date = inf_int });
    try testParse("0W", .date, false, .{ .date = inf_int });
    try testParse("1999.12.31", .date, false, .{ .date = -1 });
    try testParse("2000.01.01", .date, false, .{ .date = 0 });
    try testParse("2000.01.02", .date, false, .{ .date = 1 });
    try testParse("2000.02.01", .date, false, .{ .date = 31 });
    try testParse("2000.03.01", .date, false, .{ .date = 60 });
    try testParse("2000.04.01", .date, false, .{ .date = 91 });
    try testParse("2000.05.01", .date, false, .{ .date = 121 });
    try testParse("2000.06.01", .date, false, .{ .date = 152 });
    try testParse("2000.07.01", .date, false, .{ .date = 182 });
    try testParse("2000.08.01", .date, false, .{ .date = 213 });
    try testParse("2000.09.01", .date, false, .{ .date = 244 });
    try testParse("2000.10.01", .date, false, .{ .date = 274 });
    try testParse("2000.11.01", .date, false, .{ .date = 305 });
    try testParse("2000.12.01", .date, false, .{ .date = 335 });
    try testParse("2000.12.31", .date, false, .{ .date = 365 });
    try testParse("2001.01.01", .date, false, .{ .date = 366 });
    try testParse("2001.03.01", .date, false, .{ .date = 425 });
    try testParse("2001.12.31", .date, false, .{ .date = 730 });
    try testParse("2002.01.01", .date, false, .{ .date = 731 });
    try testParse("2002.03.01", .date, false, .{ .date = 790 });
    try testParse("2003.01.01", .date, false, .{ .date = 1096 });
    try testParse("2003.03.01", .date, false, .{ .date = 1155 });
    try testParse("2004.01.01", .date, false, .{ .date = 1461 });
    try testParse("2004.03.01", .date, false, .{ .date = 1521 });
    try testParse("2005.01.01", .date, false, .{ .date = 1827 });
    try testParse("2005.03.01", .date, false, .{ .date = 1886 });
    try testParse("3000.12.31", .date, false, .{ .date = 365607 });
    try testParse("4000.12.31", .date, false, .{ .date = 730850 });
    try testParse("5000.12.31", .date, false, .{ .date = 1096092 });
    try testParse("6000.12.31", .date, false, .{ .date = 1461335 });
    try testParse("7000.12.31", .date, false, .{ .date = 1826577 });
    try testParse("8000.12.31", .date, false, .{ .date = 2191820 });
    try testParse("9000.12.31", .date, false, .{ .date = 2557062 });
    try testParse("0001.01.01", .date, false, .{ .date = -730119 });
    try testParse("9999.12.31", .date, false, .{ .date = 2921939 });

    try testParse("000101", .date, false, .{ .date = 0 });
    try testParse("000229", .date, false, .{ .date = 59 });
    try testParse("491231", .date, false, .{ .date = 18262 });
    try testParse("500101", .date, false, .{ .date = -18262 });
    try testParse("991231", .date, false, .{ .date = -1 });

    try testParse("00010101", .date, false, .{ .date = -730119 });
    try testParse("00011231", .date, false, .{ .date = -729755 });
    try testParse("99990101", .date, false, .{ .date = 2921575 });
    try testParse("99991231", .date, false, .{ .date = 2921939 });

    try testParse("0nd", .date, true, .{ .date = null_int });
    try testParse("0Nd", .date, true, .{ .date = null_int });
    try testParse("0wd", .date, true, .{ .date = inf_int });
    try testParse("0Wd", .date, true, .{ .date = inf_int });
    try testParse("1999.12.31d", .date, true, .{ .date = -1 });
    try testParse("2000.01.01d", .date, true, .{ .date = 0 });
    try testParse("2000.01.02d", .date, true, .{ .date = 1 });
    try testParse("2000.02.01d", .date, true, .{ .date = 31 });
    try testParse("2000.03.01d", .date, true, .{ .date = 60 });
    try testParse("2000.04.01d", .date, true, .{ .date = 91 });
    try testParse("2000.05.01d", .date, true, .{ .date = 121 });
    try testParse("2000.06.01d", .date, true, .{ .date = 152 });
    try testParse("2000.07.01d", .date, true, .{ .date = 182 });
    try testParse("2000.08.01d", .date, true, .{ .date = 213 });
    try testParse("2000.09.01d", .date, true, .{ .date = 244 });
    try testParse("2000.10.01d", .date, true, .{ .date = 274 });
    try testParse("2000.11.01d", .date, true, .{ .date = 305 });
    try testParse("2000.12.01d", .date, true, .{ .date = 335 });
    try testParse("2000.12.31d", .date, true, .{ .date = 365 });
    try testParse("2001.01.01d", .date, true, .{ .date = 366 });
    try testParse("2001.03.01d", .date, true, .{ .date = 425 });
    try testParse("2001.12.31d", .date, true, .{ .date = 730 });
    try testParse("2002.01.01d", .date, true, .{ .date = 731 });
    try testParse("2002.03.01d", .date, true, .{ .date = 790 });
    try testParse("2003.01.01d", .date, true, .{ .date = 1096 });
    try testParse("2003.03.01d", .date, true, .{ .date = 1155 });
    try testParse("2004.01.01d", .date, true, .{ .date = 1461 });
    try testParse("2004.03.01d", .date, true, .{ .date = 1521 });
    try testParse("2005.01.01d", .date, true, .{ .date = 1827 });
    try testParse("2005.03.01d", .date, true, .{ .date = 1886 });
    try testParse("3000.12.31d", .date, true, .{ .date = 365607 });
    try testParse("4000.12.31d", .date, true, .{ .date = 730850 });
    try testParse("5000.12.31d", .date, true, .{ .date = 1096092 });
    try testParse("6000.12.31d", .date, true, .{ .date = 1461335 });
    try testParse("7000.12.31d", .date, true, .{ .date = 1826577 });
    try testParse("8000.12.31d", .date, true, .{ .date = 2191820 });
    try testParse("9000.12.31d", .date, true, .{ .date = 2557062 });
    try testParse("0001.01.01d", .date, true, .{ .date = -730119 });
    try testParse("9999.12.31d", .date, true, .{ .date = 2921939 });

    try testParse("000101d", .date, true, .{ .date = 0 });
    try testParse("000229d", .date, true, .{ .date = 59 });
    try testParse("491231d", .date, true, .{ .date = 18262 });
    try testParse("500101d", .date, true, .{ .date = -18262 });
    try testParse("991231d", .date, true, .{ .date = -1 });

    try testParse("00010101d", .date, true, .{ .date = -730119 });
    try testParse("00011231d", .date, true, .{ .date = -729755 });
    try testParse("99990101d", .date, true, .{ .date = 2921575 });
    try testParse("99991231d", .date, true, .{ .date = 2921939 });

    try testParse("0", .date, false, invalidCharacter(0));
    try testParse("1", .date, false, invalidCharacter(0));
    try testParse("9", .date, false, invalidCharacter(0));

    try testParse("00", .date, false, invalidCharacter(1));
    try testParse("11", .date, false, invalidCharacter(1));
    try testParse("99", .date, false, invalidCharacter(1));

    try testParse("000", .date, false, invalidCharacter(2));
    try testParse("111", .date, false, invalidCharacter(2));
    try testParse("999", .date, false, invalidCharacter(2));

    try testParse("0000", .date, false, invalidCharacter(3));
    try testParse("1111", .date, false, invalidCharacter(3));
    try testParse("9999", .date, false, invalidCharacter(3));

    try testParse("00000", .date, false, invalidCharacter(4));
    try testParse("11111", .date, false, invalidCharacter(4));
    try testParse("99999", .date, false, invalidCharacter(4));

    try testParse("000000", .date, false, overflow);
    try testParse("111111", .date, false, .{ .date = 4332 });
    try testParse("999999", .date, false, overflow);

    try testParse("0000000", .date, false, invalidCharacter(6));
    try testParse("1111111", .date, false, invalidCharacter(6));
    try testParse("9999999", .date, false, invalidCharacter(6));

    try testParse("00000000", .date, false, overflow);
    try testParse("11111111", .date, false, .{ .date = -324387 });
    try testParse("99999999", .date, false, overflow);

    try testParse("000000000", .date, false, invalidCharacter(8));
    try testParse("111111111", .date, false, invalidCharacter(8));
    try testParse("999999999", .date, false, invalidCharacter(8));

    try testParse("0000000000", .date, false, invalidCharacter(4));
    try testParse("1111111111", .date, false, invalidCharacter(4));
    try testParse("9999999999", .date, false, invalidCharacter(4));
    try testParse("0000.00000", .date, false, invalidCharacter(7));
    try testParse("1111.11111", .date, false, invalidCharacter(7));
    try testParse("9999.99999", .date, false, invalidCharacter(7));
    try testParse("0000000.00", .date, false, invalidCharacter(4));
    try testParse("1111111.11", .date, false, invalidCharacter(4));
    try testParse("9999999.99", .date, false, invalidCharacter(4));
    try testParse("0000.00.00", .date, false, overflow);
    try testParse("1111.11.11", .date, false, .{ .date = -324387 });
    try testParse("9999.99.99", .date, false, overflow);

    try testParse("2000.1.01", .date, false, invalidCharacter(8));
    try testParse("0000.01.01", .date, false, overflow);
    try testParse("0001.00.01", .date, false, overflow);
    try testParse("0001.01.00", .date, false, overflow);
    try testParse("0001.13.01", .date, false, overflow);

    try testParse("1900.02.29", .date, false, overflow);
    try testParse("2000.01.32", .date, false, overflow);
    try testParse("2000.02.29", .date, false, .{ .date = 59 });
    try testParse("2000.02.30", .date, false, overflow);
    try testParse("2000.03.32", .date, false, overflow);
    try testParse("2000.04.31", .date, false, overflow);
    try testParse("2000.05.32", .date, false, overflow);
    try testParse("2000.06.31", .date, false, overflow);
    try testParse("2000.07.32", .date, false, overflow);
    try testParse("2000.08.32", .date, false, overflow);
    try testParse("2000.09.31", .date, false, overflow);
    try testParse("2000.10.32", .date, false, overflow);
    try testParse("2000.11.31", .date, false, overflow);
    try testParse("2000.12.32", .date, false, overflow);

    try testParse("0d", .date, true, invalidCharacter(0));
    try testParse("1d", .date, true, invalidCharacter(0));
    try testParse("9d", .date, true, invalidCharacter(0));

    try testParse("00d", .date, true, invalidCharacter(1));
    try testParse("11d", .date, true, invalidCharacter(1));
    try testParse("99d", .date, true, invalidCharacter(1));

    try testParse("000d", .date, true, invalidCharacter(2));
    try testParse("111d", .date, true, invalidCharacter(2));
    try testParse("999d", .date, true, invalidCharacter(2));

    try testParse("0000d", .date, true, invalidCharacter(3));
    try testParse("1111d", .date, true, invalidCharacter(3));
    try testParse("9999d", .date, true, invalidCharacter(3));

    try testParse("00000d", .date, true, invalidCharacter(4));
    try testParse("11111d", .date, true, invalidCharacter(4));
    try testParse("99999d", .date, true, invalidCharacter(4));

    try testParse("000000d", .date, true, overflow);
    try testParse("111111d", .date, true, .{ .date = 4332 });
    try testParse("999999d", .date, true, overflow);

    try testParse("0000000d", .date, true, invalidCharacter(6));
    try testParse("1111111d", .date, true, invalidCharacter(6));
    try testParse("9999999d", .date, true, invalidCharacter(6));

    try testParse("00000000d", .date, true, overflow);
    try testParse("11111111d", .date, true, .{ .date = -324387 });
    try testParse("99999999d", .date, true, overflow);

    try testParse("000000000d", .date, true, invalidCharacter(8));
    try testParse("111111111d", .date, true, invalidCharacter(8));
    try testParse("999999999d", .date, true, invalidCharacter(8));

    try testParse("0000000000d", .date, true, invalidCharacter(4));
    try testParse("1111111111d", .date, true, invalidCharacter(4));
    try testParse("9999999999d", .date, true, invalidCharacter(4));
    try testParse("0000.00000d", .date, true, invalidCharacter(7));
    try testParse("1111.11111d", .date, true, invalidCharacter(7));
    try testParse("9999.99999d", .date, true, invalidCharacter(7));
    try testParse("0000000.00d", .date, true, invalidCharacter(4));
    try testParse("1111111.11d", .date, true, invalidCharacter(4));
    try testParse("9999999.99d", .date, true, invalidCharacter(4));
    try testParse("0000.00.00d", .date, true, overflow);
    try testParse("1111.11.11d", .date, true, .{ .date = -324387 });
    try testParse("9999.99.99d", .date, true, overflow);

    try testParse("2000.1.01d", .date, true, invalidCharacter(8));
    try testParse("0000.01.01d", .date, true, overflow);
    try testParse("0001.00.01d", .date, true, overflow);
    try testParse("0001.01.00d", .date, true, overflow);
    try testParse("0001.13.01d", .date, true, overflow);

    try testParse("1900.02.29d", .date, true, overflow);
    try testParse("2000.01.32d", .date, true, overflow);
    try testParse("2000.02.29d", .date, true, .{ .date = 59 });
    try testParse("2000.02.30d", .date, true, overflow);
    try testParse("2000.03.32d", .date, true, overflow);
    try testParse("2000.04.31d", .date, true, overflow);
    try testParse("2000.05.32d", .date, true, overflow);
    try testParse("2000.06.31d", .date, true, overflow);
    try testParse("2000.07.32d", .date, true, overflow);
    try testParse("2000.08.32d", .date, true, overflow);
    try testParse("2000.09.31d", .date, true, overflow);
    try testParse("2000.10.32d", .date, true, overflow);
    try testParse("2000.11.31d", .date, true, overflow);
    try testParse("2000.12.32d", .date, true, overflow);
}
