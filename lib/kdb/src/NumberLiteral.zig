const std = @import("std");
const assert = std.debug.assert;

const Base = enum(u8) { decimal = 10, hex = 16, binary = 2, octal = 8 };
const FloatBase = enum(u8) { decimal = 10, hex = 16 };

pub const Result = union(enum) {
    bool: bool,
    byte: u8,
    short: i16,
    int: i32,
    long: i64,
    real: f32,
    float: f64,
    char: u8,
    failure: Error,
};

pub const Error = union(enum) {
    nyi,
    overflow,
    prefer_short_inf,
    prefer_int_inf,
    prefer_long_inf,

    /// The number has leading zeroes.
    leading_zero,
    /// Expected a digit after base prefix.
    digit_after_base,
    /// The base prefix is in uppercase.
    upper_case_base: usize,
    /// Float literal has an invalid base prefix.
    invalid_float_base: usize,
    /// Repeated '_' digit separator.
    repeated_underscore: usize,
    /// '_' digit separator after special character (+-.)
    invalid_underscore_after_special: usize,
    /// Invalid digit for the specified base.
    invalid_digit: struct { i: usize, base: Base },
    /// Invalid digit for an exponent.
    invalid_digit_exponent: usize,
    /// Float literal has multiple periods.
    duplicate_period,
    /// Float literal has multiple exponents.
    duplicate_exponent: usize,
    /// Exponent comes directly after '_' digit separator.
    exponent_after_underscore: usize,
    /// Special character (+-.) comes directly after exponent.
    special_after_underscore: usize,
    /// Number ends in special character (+-.)
    trailing_special: usize,
    /// Number ends in '_' digit separator.
    trailing_underscore: usize,
    /// Character not in [0-9a-zA-Z.+-_]
    invalid_character: usize,
    /// [+-] not immediately after [pPeE]
    invalid_exponent_sign: usize,
    /// Period comes directly after exponent.
    period_after_exponent: usize,
};

pub const TypeHint = enum {
    none,
    bool,
    guid,
    byte,
    short,
    int,
    long,
    real,
    float,
    char,
    timestamp,
    month,
    date,
    datetime,
    timespan,
    minute,
    second,
    time,

    real_or_float,

    pub fn get(slice: []const u8) error{InvalidSuffix}!TypeHint {
        switch (slice.len) {
            2 => switch (slice[0]) {
                '0' => switch (slice[1]) {
                    'N', 'n' => return .none,
                    'x' => return .byte,
                    else => {},
                },
                else => {},
            },
            else => {},
        }
        return if (std.mem.startsWith(u8, slice, "0x"))
            .byte // TODO: This is maybe indexing: 0 1 2 0x0001000001 => 0 1 0 0 1
        else switch (slice[slice.len - 1]) {
            'b' => .bool, // TODO: This is maybe indexing: 0 1 2 01001b => 0 1 0 0 1
            'g' => .guid,
            'h' => .short,
            'i' => .int,
            'j' => .long,
            'e' => .real,
            'f' => .float,
            'c' => .char,
            'p' => .timestamp,
            'm' => .month,
            'd' => .date,
            'z' => .datetime,
            'n' => .timespan,
            'u' => .minute,
            'v' => .second,
            't' => .time,
            '.' => .real_or_float,
            '0'...'9' => .none,
            else => error.InvalidSuffix,
        };
    }
};

const null_short = -32768;
const inf_short = 32767;

const null_int = -2147483648;
const inf_int = 2147483647;

const null_long = -9223372036854775808;
const inf_long = 9223372036854775807;

pub fn parse(bytes: []const u8, type_hint: TypeHint, allow_suffix: bool) Result {
    switch (type_hint) {
        .none => return parseNone(bytes, allow_suffix),
        .bool => return .{ .failure = .nyi },
        .guid => return .{ .failure = .nyi },
        .byte => return .{ .failure = .nyi },
        .short => return parseShort(bytes, allow_suffix),
        .int => return parseInt(bytes, allow_suffix),
        .long => return parseLong(bytes, allow_suffix),
        .real => return .{ .failure = .nyi },
        .float => return .{ .failure = .nyi },
        .char => return .{ .failure = .nyi },
        .timestamp => return .{ .failure = .nyi },
        .month => return .{ .failure = .nyi },
        .date => return .{ .failure = .nyi },
        .datetime => return .{ .failure = .nyi },
        .timespan => return .{ .failure = .nyi },
        .minute => return .{ .failure = .nyi },
        .second => return .{ .failure = .nyi },
        .time => return .{ .failure = .nyi },
        .real_or_float => return .{ .failure = .nyi },
    }
}

fn parseNone(bytes: []const u8, allow_suffix: bool) Result {
    var i: usize = 0;
    var base: u8 = 10;
    if (bytes.len >= 2 and bytes[0] == '0') switch (bytes[1]) {
        'x' => {
            base = 16;
            i = 2;
        },
        'X' => return .{ .failure = .{ .upper_case_base = 1 } },
        '.', 'e', 'E' => {},
        'n' => unreachable,
        'N' => switch (allow_suffix) {
            true => unreachable,
            false => switch (bytes.len) {
                0, 1 => unreachable,
                2 => return .{ .long = null_long },
                else => switch (bytes[2]) {
                    '0'...'9' => return .{ .failure = .{ .invalid_character = 1 } },
                    else => return .{ .failure = .{ .invalid_character = 2 } },
                },
            },
        },
        'w' => unreachable,
        'W' => switch (allow_suffix) {
            true => unreachable,
            false => switch (bytes.len) {
                0, 1 => unreachable,
                2 => return .{ .long = inf_long },
                else => switch (bytes[2]) {
                    '0'...'9' => return .{ .failure = .{ .invalid_character = 1 } },
                    else => return .{ .failure = .{ .invalid_character = 2 } },
                },
            },
        },
        else => return .{ .failure = .leading_zero },
    };
    if (bytes.len == 2 and base != 10) return .{ .failure = .digit_after_base };

    var x: u32 = 0;
    var underscore = false;
    var period = false;
    var special: u8 = 0;
    var exponent = false;
    var float = false;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        switch (c) {
            '_' => {
                if (i == 2 and base != 10) return .{ .failure = .{ .invalid_underscore_after_special = i } };
                if (special != 0) return .{ .failure = .{ .invalid_underscore_after_special = i } };
                if (underscore) return .{ .failure = .{ .repeated_underscore = i } };
                underscore = true;
                continue;
            },
            'e', 'E' => if (base == 10) {
                float = true;
                if (exponent) return .{ .failure = .{ .duplicate_exponent = i } };
                if (underscore) return .{ .failure = .{ .exponent_after_underscore = i } };
                special = c;
                exponent = true;
                continue;
            },
            'p', 'P' => if (base == 16) {
                if (i == 2) {
                    return .{ .failure = .{ .digit_after_base = {} } };
                }
                float = true;
                if (exponent) return .{ .failure = .{ .duplicate_exponent = i } };
                if (underscore) return .{ .failure = .{ .exponent_after_underscore = i } };
                special = c;
                exponent = true;
                continue;
            },
            '.' => {
                if (exponent) {
                    const digit_index = i - ".e".len;
                    if (digit_index < bytes.len) {
                        switch (bytes[digit_index]) {
                            '0'...'9' => return .{ .failure = .{ .period_after_exponent = i } },
                            else => {},
                        }
                    }
                }
                float = true;
                if (base != 10 and base != 16) return .{ .failure = .{ .invalid_float_base = 2 } };
                if (period) return .{ .failure = .duplicate_period };
                period = true;
                if (underscore) return .{ .failure = .{ .special_after_underscore = i } };
                special = c;
                continue;
            },
            '+', '-' => {
                switch (special) {
                    'p', 'P' => {},
                    'e', 'E' => if (base != 10) return .{ .failure = .{ .invalid_exponent_sign = i } },
                    else => return .{ .failure = .{ .invalid_exponent_sign = i } },
                }
                special = c;
                continue;
            },
            else => {},
        }
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'A'...'Z' => c - 'A' + 10,
            'a'...'z' => c - 'a' + 10,
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        if (digit >= base) return .{ .failure = .{ .invalid_digit = .{ .i = i, .base = @as(Base, @enumFromInt(base)) } } };
        if (exponent and digit >= 10) return .{ .failure = .{ .invalid_digit_exponent = i } };
        underscore = false;
        special = 0;

        if (float) continue;
        if (x != 0) {
            const res = @mulWithOverflow(x, base);
            if (res[1] != 0) return .{ .failure = .overflow };
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .{ .failure = .overflow };
        x = res[0];
    }
    if (underscore) return .{ .failure = .{ .trailing_underscore = bytes.len - 1 } };
    if (special != 0) return .{ .failure = .{ .trailing_special = bytes.len - 1 } };

    // if (float) return .{ .float = @as(FloatBase, @enumFromInt(base)) };
    return .{ .long = @intCast(x) };
}

fn parseShort(bytes: []const u8, allow_suffix: bool) Result {
    var i: usize = 0;
    var x: i16 = 0;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'h' => if (allow_suffix and i == bytes.len - 1)
                continue
            else
                return .{ .failure = .{ .invalid_character = i } },
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        if (x != 0) {
            const res = @mulWithOverflow(x, 10);
            if (res[1] != 0) return .{ .failure = .overflow };
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .{ .failure = .overflow };
        x = res[0];
    }

    if (x == inf_short or x == -inf_short) return .{ .failure = .prefer_short_inf };
    if (x > inf_short or x < -inf_short) return .{ .failure = .overflow };

    return .{ .short = x };
}

fn parseInt(bytes: []const u8, allow_suffix: bool) Result {
    var i: usize = 0;
    var x: i32 = 0;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'i' => if (allow_suffix and i == bytes.len - 1)
                continue
            else
                return .{ .failure = .{ .invalid_character = i } },
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        if (x != 0) {
            const res = @mulWithOverflow(x, 10);
            if (res[1] != 0) return .{ .failure = .overflow };
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .{ .failure = .overflow };
        x = res[0];
    }

    if (x == inf_int or x == -inf_int) return .{ .failure = .prefer_int_inf };
    if (x > inf_int or x < -inf_int) return .{ .failure = .overflow };

    return .{ .int = x };
}

fn parseLong(bytes: []const u8, allow_suffix: bool) Result {
    var i: usize = 0;
    var x: i64 = 0;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'j' => if (allow_suffix and i == bytes.len - 1)
                continue
            else
                return .{ .failure = .{ .invalid_character = i } },
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        if (x != 0) {
            const res = @mulWithOverflow(x, 10);
            if (res[1] != 0) return .{ .failure = .overflow };
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .{ .failure = .overflow };
        x = res[0];
    }

    if (x == inf_long or x == -inf_long) return .{ .failure = .prefer_long_inf };
    if (x > inf_long or x < -inf_long) return .{ .failure = .overflow };

    return .{ .long = x };
}

fn testParse(bytes: []const u8, type_hint: TypeHint, allow_suffix: bool, expected: Result) !void {
    const result = parse(bytes, type_hint, allow_suffix);
    try std.testing.expectEqual(expected, result);
}

const invalid_type: Result = .{ .failure = .invalid_type };
fn invalidCharacter(i: usize) Result {
    return .{ .failure = .{ .invalid_character = i } };
}

test "parse number literal - short" {
    try testParse("0n", .short, false, .{ .short = null_short });
    try testParse("0N", .short, false, .{ .short = null_short });
    try testParse("0w", .short, false, .{ .short = inf_short });
    try testParse("0W", .short, false, .{ .short = inf_short });
    try testParse("1", .short, false, .{ .short = 1 });
    try testParse("2", .short, false, .{ .short = 2 });
    try testParse("0n", .short, true, .{ .short = null_short });
    try testParse("0N", .short, true, .{ .short = null_short });
    try testParse("0w", .short, true, .{ .short = inf_short });
    try testParse("0W", .short, true, .{ .short = inf_short });
    try testParse("0", .short, true, .{ .short = 0 });
    try testParse("1", .short, true, .{ .short = 1 });
    try testParse("2", .short, true, .{ .short = 2 });

    try testParse("0nh", .none, false, invalidCharacter(2));
    try testParse("0Nh", .none, false, invalidCharacter(2));
    try testParse("0wh", .none, false, invalidCharacter(2));
    try testParse("0Wh", .none, false, invalidCharacter(2));
    try testParse("0h", .none, false, invalidCharacter(1));
    try testParse("1h", .none, false, invalidCharacter(1));
    try testParse("2h", .none, false, invalidCharacter(1));
    try testParse("0nh", .none, true, .{ .short = null_short });
    try testParse("0Nh", .none, true, .{ .short = null_short });
    try testParse("0wh", .none, true, .{ .short = inf_short });
    try testParse("0Wh", .none, true, .{ .short = inf_short });
    try testParse("0h", .none, true, .{ .short = 0 });
    try testParse("1h", .none, true, .{ .short = 1 });
    try testParse("2h", .none, true, .{ .short = 2 });

    try testParse("0nh", .short, false, invalidCharacter(2));
    try testParse("0Nh", .short, false, invalidCharacter(2));
    try testParse("0wh", .short, false, invalidCharacter(2));
    try testParse("0Wh", .short, false, invalidCharacter(2));
    try testParse("0h", .short, false, invalidCharacter(1));
    try testParse("1h", .short, false, invalidCharacter(1));
    try testParse("2h", .short, false, invalidCharacter(1));
    try testParse("0nh", .short, true, .{ .short = null_short });
    try testParse("0Nh", .short, true, .{ .short = null_short });
    try testParse("0wh", .short, true, .{ .short = inf_short });
    try testParse("0Wh", .short, true, .{ .short = inf_short });
    try testParse("0h", .short, true, .{ .short = 0 });
    try testParse("1h", .short, true, .{ .short = 1 });
    try testParse("2h", .short, true, .{ .short = 2 });
}

test "parse number literal - int" {
    try testParse("0n", .int, false, .{ .int = null_int });
    try testParse("0N", .int, false, .{ .int = null_int });
    try testParse("0w", .int, false, .{ .int = inf_int });
    try testParse("0W", .int, false, .{ .int = inf_int });
    try testParse("1", .int, false, .{ .int = 1 });
    try testParse("2", .int, false, .{ .int = 2 });
    try testParse("0n", .int, true, .{ .int = null_int });
    try testParse("0N", .int, true, .{ .int = null_int });
    try testParse("0w", .int, true, .{ .int = inf_int });
    try testParse("0W", .int, true, .{ .int = inf_int });
    try testParse("0", .int, true, .{ .int = 0 });
    try testParse("1", .int, true, .{ .int = 1 });
    try testParse("2", .int, true, .{ .int = 2 });

    try testParse("0ni", .none, false, invalidCharacter(2));
    try testParse("0Ni", .none, false, invalidCharacter(2));
    try testParse("0wi", .none, false, invalidCharacter(2));
    try testParse("0Wi", .none, false, invalidCharacter(2));
    try testParse("0i", .none, false, invalidCharacter(1));
    try testParse("1i", .none, false, invalidCharacter(1));
    try testParse("2i", .none, false, invalidCharacter(1));
    try testParse("0ni", .none, true, .{ .int = null_int });
    try testParse("0Ni", .none, true, .{ .int = null_int });
    try testParse("0wi", .none, true, .{ .int = inf_int });
    try testParse("0Wi", .none, true, .{ .int = inf_int });
    try testParse("0i", .none, true, .{ .int = 0 });
    try testParse("1i", .none, true, .{ .int = 1 });
    try testParse("2i", .none, true, .{ .int = 2 });

    try testParse("0ni", .int, false, invalidCharacter(2));
    try testParse("0Ni", .int, false, invalidCharacter(2));
    try testParse("0wi", .int, false, invalidCharacter(2));
    try testParse("0Wi", .int, false, invalidCharacter(2));
    try testParse("0i", .int, false, invalidCharacter(1));
    try testParse("1i", .int, false, invalidCharacter(1));
    try testParse("2i", .int, false, invalidCharacter(1));
    try testParse("0ni", .int, true, .{ .int = null_int });
    try testParse("0Ni", .int, true, .{ .int = null_int });
    try testParse("0wi", .int, true, .{ .int = inf_int });
    try testParse("0Wi", .int, true, .{ .int = inf_int });
    try testParse("0i", .int, true, .{ .int = 0 });
    try testParse("1i", .int, true, .{ .int = 1 });
    try testParse("2i", .int, true, .{ .int = 2 });
}

test "parse number literal - long" {
    try testParse("0N", .none, false, .{ .long = null_long });
    try testParse("0W", .none, false, .{ .long = inf_long });
    try testParse("0", .none, false, .{ .long = 0 });
    try testParse("1", .none, false, .{ .long = 1 });
    try testParse("2", .none, false, .{ .long = 2 });
    try testParse("0N", .none, true, .{ .long = null_long });
    try testParse("0W", .none, true, .{ .long = inf_long });
    try testParse("0", .none, true, .{ .long = 0 });
    try testParse("1", .none, true, .{ .long = 1 });
    try testParse("2", .none, true, .{ .long = 2 });

    try testParse("0n", .long, false, .{ .long = null_long });
    try testParse("0N", .long, false, .{ .long = null_long });
    try testParse("0w", .long, false, .{ .long = inf_long });
    try testParse("0W", .long, false, .{ .long = inf_long });
    try testParse("0", .long, false, .{ .long = 0 });
    try testParse("1", .long, false, .{ .long = 1 });
    try testParse("2", .long, false, .{ .long = 2 });
    try testParse("0n", .long, true, .{ .long = null_long });
    try testParse("0N", .long, true, .{ .long = null_long });
    try testParse("0w", .long, true, .{ .long = inf_long });
    try testParse("0W", .long, true, .{ .long = inf_long });
    try testParse("0", .long, true, .{ .long = 0 });
    try testParse("1", .long, true, .{ .long = 1 });
    try testParse("2", .long, true, .{ .long = 2 });

    try testParse("0nj", .none, false, invalidCharacter(2));
    try testParse("0Nj", .none, false, invalidCharacter(2));
    try testParse("0wj", .none, false, invalidCharacter(2));
    try testParse("0Wj", .none, false, invalidCharacter(2));
    try testParse("0j", .none, false, invalidCharacter(1));
    try testParse("1j", .none, false, invalidCharacter(1));
    try testParse("2j", .none, false, invalidCharacter(1));
    try testParse("0nj", .none, true, .{ .long = null_long });
    try testParse("0Nj", .none, true, .{ .long = null_long });
    try testParse("0wj", .none, true, .{ .long = inf_long });
    try testParse("0Wj", .none, true, .{ .long = inf_long });
    try testParse("0j", .none, true, .{ .long = 0 });
    try testParse("1j", .none, true, .{ .long = 1 });
    try testParse("2j", .none, true, .{ .long = 2 });

    try testParse("0nj", .long, false, invalidCharacter(2));
    try testParse("0Nj", .long, false, invalidCharacter(2));
    try testParse("0wj", .long, false, invalidCharacter(2));
    try testParse("0Wj", .long, false, invalidCharacter(2));
    try testParse("0j", .long, false, invalidCharacter(1));
    try testParse("1j", .long, false, invalidCharacter(1));
    try testParse("2j", .long, false, invalidCharacter(1));
    try testParse("0nj", .long, true, .{ .long = null_long });
    try testParse("0Nj", .long, true, .{ .long = null_long });
    try testParse("0wj", .long, true, .{ .long = inf_long });
    try testParse("0Wj", .long, true, .{ .long = inf_long });
    try testParse("0j", .long, true, .{ .long = 0 });
    try testParse("1j", .long, true, .{ .long = 1 });
    try testParse("2j", .long, true, .{ .long = 2 });
}
