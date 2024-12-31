const std = @import("std");
const assert = std.debug.assert;

const Base = enum(u8) { decimal = 10, hex = 16, binary = 2, octal = 8 };
const FloatBase = enum(u8) { decimal = 10, hex = 16 };

const Type = enum {
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
    failure,
};

pub const Result = union(Type) {
    bool: bool,
    guid,
    byte: u8,
    short: i16,
    int: i32,
    long: i64,
    real,
    float,
    char: u8,
    timestamp: i64,
    month: i32,
    date: i32,
    datetime,
    timespan: i64,
    minute: i32,
    second: i32,
    time: i32,
    failure: Error,
};

pub const Error = union(enum) {
    nyi,
    overflow,
    prefer_short_inf,
    prefer_int_inf,
    prefer_long_inf,

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

    pub fn get(bytes: []const u8) error{InvalidSuffix}!TypeHint {
        const slice = if (bytes[0] == '-') bytes[1..] else bytes;
        if (slice.len == 2 and slice[0] == '0') switch (slice[1]) {
            'N', 'W', 'n', 'w' => return .none,
            'x' => return .byte,
            else => {},
        };
        return if (std.mem.startsWith(u8, slice, "0x"))
            .byte
        else switch (slice[slice.len - 1]) {
            'b' => .bool,
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

pub const null_char = ' ';

pub const null_short = -32768;
pub const inf_short = 32767;

pub const null_int = -2147483648;
pub const inf_int = 2147483647;

pub const null_long = -9223372036854775808;
pub const inf_long = 9223372036854775807;

pub fn parse(bytes: []const u8, type_hint: TypeHint, allow_suffix: bool) Result {
    switch (type_hint) {
        .none => return parseNone(bytes, allow_suffix),
        .bool => return parseBool(bytes, allow_suffix),
        .guid => return parseGuid(bytes, allow_suffix),
        .byte => return parseByte(bytes),
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
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N' => return .{ .long = null_long },
        'n' => return .float,
        'W' => return .{ .long = inf_long },
        'w' => return .float,
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'c' => .{ .char = null_char },
            'd' => .{ .date = null_int },
            'e' => .real,
            'f' => .float,
            'g' => .guid,
            'h' => .{ .short = null_short },
            'i' => .{ .int = null_int },
            'j' => .{ .long = null_long },
            'm' => .{ .month = null_int },
            'n' => .{ .timespan = null_long },
            'p' => .{ .timestamp = null_long },
            't' => .{ .time = null_int },
            'u' => .{ .minute = null_int },
            'v' => .{ .second = null_int },
            'z' => .datetime,
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'c' => .{ .char = null_char },
            'd' => .{ .date = inf_int },
            'e' => .real,
            'f' => .float,
            'g' => .guid,
            'h' => .{ .short = inf_short },
            'i' => .{ .int = inf_int },
            'j' => .{ .long = inf_long },
            'm' => .{ .month = inf_int },
            'n' => .{ .timespan = inf_long },
            'p' => .{ .timestamp = inf_long },
            't' => .{ .time = inf_int },
            'u' => .{ .minute = inf_int },
            'v' => .{ .second = inf_int },
            'z' => .datetime,
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };
    var i: usize = 0;
    var base: u8 = 10;
    if (bytes.len >= 2 and bytes[0] == '0') switch (bytes[1]) {
        'x' => {
            base = 16;
            i = 2;
        },
        'X' => return .{ .failure = .{ .upper_case_base = 1 } },
        '.', 'e', 'E' => {},
        else => {},
    };

    var x: u32 = 0;
    var underscore = false;
    var period = false;
    var special: u8 = 0;
    var exponent = false;
    var float = false;
    var return_type: ?Type = null;
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
        if (digit >= base) {
            if (allow_suffix and i == bytes.len - 1) {
                switch (c) {
                    'c' => return_type = .char,
                    'd' => return_type = .date,
                    'e' => return_type = .real,
                    'f' => return_type = .float,
                    'g' => return_type = .guid,
                    'h' => return_type = .short,
                    'i' => return_type = .int,
                    'j' => return_type = .long,
                    'm' => return_type = .month,
                    'n' => return_type = .timespan,
                    'p' => return_type = .timestamp,
                    't' => return_type = .time,
                    'u' => return_type = .minute,
                    'v' => return_type = .second,
                    'z' => return_type = .datetime,
                    else => return .{ .failure = .{
                        .invalid_digit = .{ .i = i, .base = @enumFromInt(base) },
                    } },
                }
                continue;
            }

            return .{ .failure = .{
                .invalid_digit = .{ .i = i, .base = @enumFromInt(base) },
            } };
        }
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

    if (return_type) |rt| switch (rt) {
        .bool => {
            return .{ .bool = false };
        },
        .guid => {
            return .{ .bool = false };
        },
        .byte => {
            return .{ .bool = false };
        },
        .short => {
            if (x == inf_short) return .{ .failure = .prefer_short_inf };
            if (x > inf_short) return .{ .failure = .overflow };

            return .{ .short = @intCast(x) };
        },
        .int => {
            if (x == inf_int) return .{ .failure = .prefer_int_inf };
            if (x > inf_int) return .{ .failure = .overflow };

            return .{ .int = @intCast(x) };
        },
        .long => {
            if (x == inf_long) return .{ .failure = .prefer_long_inf };
            if (x > inf_long) return .{ .failure = .overflow };

            return .{ .long = x };
        },
        .real => {
            return .{ .bool = false };
        },
        .float => {
            return .{ .bool = false };
        },
        .char => {
            return .{ .bool = false };
        },
        .timestamp => {
            return .{ .bool = false };
        },
        .month => {
            return .{ .bool = false };
        },
        .date => {
            return .{ .bool = false };
        },
        .datetime => {
            return .{ .bool = false };
        },
        .timespan => {
            return .{ .bool = false };
        },
        .minute => {
            return .{ .bool = false };
        },
        .second => {
            return .{ .bool = false };
        },
        .time => {
            return .{ .bool = false };
        },
        .failure => unreachable,
    } else {
        if (float) return .float;
        return .{ .long = @intCast(x) };
    }
}

fn parseBool(bytes: []const u8, allow_suffix: bool) Result {
    const value = switch (bytes[0]) {
        '0' => false,
        '1' => true,
        else => return .{ .failure = .{
            .invalid_digit = .{ .i = 0, .base = .binary },
        } },
    };

    if (allow_suffix) {
        if (bytes.len > 1 and bytes[1] != 'b') return .{ .failure = .{ .invalid_character = 1 } };
        if (bytes.len > 2) return .{ .failure = .{ .invalid_character = 2 } };
    } else {
        if (bytes.len > 1) return .{ .failure = .{ .invalid_character = 1 } };
    }

    return .{ .bool = value };
}

fn parseGuid(bytes: []const u8, allow_suffix: bool) Result {
    return switch (bytes.len) {
        0 => unreachable,
        1 => .{ .failure = .{ .invalid_character = 0 } },
        2 => if (bytes[0] != '0')
            .{ .failure = .{ .invalid_character = 0 } }
        else switch (bytes[1]) {
            'n', 'N' => .guid,
            else => .{ .failure = .{ .invalid_character = 1 } },
        },
        else => if (bytes[0] != '0')
            .{ .failure = .{ .invalid_character = 0 } }
        else switch (bytes[1]) {
            'n', 'N' => if (allow_suffix and bytes[2] == 'g')
                .guid
            else
                .{ .failure = .{ .invalid_character = 2 } },
            else => .{ .failure = .{ .invalid_character = 1 } },
        },
    };
}

fn parseByte(bytes: []const u8) Result {
    const base: u8 = 16;

    var i: usize = 0;
    var x: u8 = 0;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'A'...'Z' => c - 'A' + 10,
            'a'...'z' => c - 'a' + 10,
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        if (digit >= base) {
            return .{ .failure = .{
                .invalid_digit = .{ .i = i, .base = @enumFromInt(base) },
            } };
        }

        if (x != 0) {
            const res = @mulWithOverflow(x, base);
            if (res[1] != 0) return .{ .failure = .overflow };
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .{ .failure = .overflow };
        x = res[0];
    }

    return .{ .byte = x };
}

fn parseShort(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .short = null_short },
        'W', 'w' => return .{ .short = inf_short },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'h' => .{ .short = null_short },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'h' => .{ .short = inf_short },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

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

    if (x == inf_short) return .{ .failure = .prefer_short_inf };
    if (x > inf_short) return .{ .failure = .overflow };

    return .{ .short = x };
}

fn parseInt(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .int = null_int },
        'W', 'w' => return .{ .int = inf_int },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'i' => .{ .int = null_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'i' => .{ .int = inf_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

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

    if (x == inf_int) return .{ .failure = .prefer_int_inf };
    if (x > inf_int) return .{ .failure = .overflow };

    return .{ .int = x };
}

fn parseLong(bytes: []const u8, allow_suffix: bool) Result {
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

    if (x == inf_long) return .{ .failure = .prefer_long_inf };
    if (x > inf_long) return .{ .failure = .overflow };

    return .{ .long = x };
}

fn testParse(bytes: []const u8, type_hint: TypeHint, allow_suffix: bool, expected: Result) !void {
    const result = parse(bytes, type_hint, allow_suffix);
    try std.testing.expectEqual(expected, result);
}

const overflow: Result = .{ .failure = .overflow };
fn invalidCharacter(i: usize) Result {
    return .{ .failure = .{ .invalid_character = i } };
}
fn invalidDigit(i: usize, base: Base) Result {
    return .{ .failure = .{
        .invalid_digit = .{ .i = i, .base = base },
    } };
}

test "parse number literal - bool" {
    try testParse("0n", .bool, false, invalidCharacter(1));
    try testParse("0N", .bool, false, invalidCharacter(1));
    try testParse("0w", .bool, false, invalidCharacter(1));
    try testParse("0W", .bool, false, invalidCharacter(1));
    try testParse("0", .bool, false, .{ .bool = false });
    try testParse("1", .bool, false, .{ .bool = true });
    try testParse("2", .bool, false, invalidDigit(0, .binary));
    try testParse("0n", .bool, true, invalidCharacter(1));
    try testParse("0N", .bool, true, invalidCharacter(1));
    try testParse("0w", .bool, true, invalidCharacter(1));
    try testParse("0W", .bool, true, invalidCharacter(1));
    try testParse("0", .bool, true, .{ .bool = false });
    try testParse("1", .bool, true, .{ .bool = true });
    try testParse("2", .bool, true, invalidDigit(0, .binary));

    try testParse("0nb", .bool, false, invalidCharacter(1));
    try testParse("0Nb", .bool, false, invalidCharacter(1));
    try testParse("0wb", .bool, false, invalidCharacter(1));
    try testParse("0Wb", .bool, false, invalidCharacter(1));
    try testParse("0b", .bool, false, invalidCharacter(1));
    try testParse("1b", .bool, false, invalidCharacter(1));
    try testParse("2b", .bool, false, invalidDigit(0, .binary));
    try testParse("0nb", .bool, true, invalidCharacter(1));
    try testParse("0Nb", .bool, true, invalidCharacter(1));
    try testParse("0wb", .bool, true, invalidCharacter(1));
    try testParse("0Wb", .bool, true, invalidCharacter(1));
    try testParse("0b", .bool, true, .{ .bool = false });
    try testParse("1b", .bool, true, .{ .bool = true });
    try testParse("2b", .bool, true, invalidDigit(0, .binary));
}

test "parse number literal - byte" {
    inline for (&.{ false, true }) |allow_suffix| {
        try testParse("0n", .byte, allow_suffix, invalidDigit(1, .hex));
        try testParse("0N", .byte, allow_suffix, invalidDigit(1, .hex));
        try testParse("0w", .byte, allow_suffix, invalidDigit(1, .hex));
        try testParse("0W", .byte, allow_suffix, invalidDigit(1, .hex));
        try testParse("0", .byte, allow_suffix, .{ .byte = 0x0 });
        try testParse("1", .byte, allow_suffix, .{ .byte = 0x1 });
        try testParse("2", .byte, allow_suffix, .{ .byte = 0x2 });
        try testParse("3", .byte, allow_suffix, .{ .byte = 0x3 });
        try testParse("4", .byte, allow_suffix, .{ .byte = 0x4 });
        try testParse("5", .byte, allow_suffix, .{ .byte = 0x5 });
        try testParse("6", .byte, allow_suffix, .{ .byte = 0x6 });
        try testParse("7", .byte, allow_suffix, .{ .byte = 0x7 });
        try testParse("8", .byte, allow_suffix, .{ .byte = 0x8 });
        try testParse("9", .byte, allow_suffix, .{ .byte = 0x9 });
        try testParse("a", .byte, allow_suffix, .{ .byte = 0xa });
        try testParse("b", .byte, allow_suffix, .{ .byte = 0xb });
        try testParse("c", .byte, allow_suffix, .{ .byte = 0xc });
        try testParse("d", .byte, allow_suffix, .{ .byte = 0xd });
        try testParse("e", .byte, allow_suffix, .{ .byte = 0xe });
        try testParse("f", .byte, allow_suffix, .{ .byte = 0xf });
        try testParse("00", .byte, allow_suffix, .{ .byte = 0x0 });
        try testParse("01", .byte, allow_suffix, .{ .byte = 0x1 });
        try testParse("02", .byte, allow_suffix, .{ .byte = 0x2 });
        try testParse("03", .byte, allow_suffix, .{ .byte = 0x3 });
        try testParse("04", .byte, allow_suffix, .{ .byte = 0x4 });
        try testParse("05", .byte, allow_suffix, .{ .byte = 0x5 });
        try testParse("06", .byte, allow_suffix, .{ .byte = 0x6 });
        try testParse("07", .byte, allow_suffix, .{ .byte = 0x7 });
        try testParse("08", .byte, allow_suffix, .{ .byte = 0x8 });
        try testParse("09", .byte, allow_suffix, .{ .byte = 0x9 });
        try testParse("0a", .byte, allow_suffix, .{ .byte = 0xa });
        try testParse("0b", .byte, allow_suffix, .{ .byte = 0xb });
        try testParse("0c", .byte, allow_suffix, .{ .byte = 0xc });
        try testParse("0d", .byte, allow_suffix, .{ .byte = 0xd });
        try testParse("0e", .byte, allow_suffix, .{ .byte = 0xe });
        try testParse("0f", .byte, allow_suffix, .{ .byte = 0xf });
        try testParse("10", .byte, allow_suffix, .{ .byte = 0x10 });
        try testParse("11", .byte, allow_suffix, .{ .byte = 0x11 });
        try testParse("12", .byte, allow_suffix, .{ .byte = 0x12 });
        try testParse("13", .byte, allow_suffix, .{ .byte = 0x13 });
        try testParse("14", .byte, allow_suffix, .{ .byte = 0x14 });
        try testParse("15", .byte, allow_suffix, .{ .byte = 0x15 });
        try testParse("16", .byte, allow_suffix, .{ .byte = 0x16 });
        try testParse("17", .byte, allow_suffix, .{ .byte = 0x17 });
        try testParse("18", .byte, allow_suffix, .{ .byte = 0x18 });
        try testParse("19", .byte, allow_suffix, .{ .byte = 0x19 });
        try testParse("1a", .byte, allow_suffix, .{ .byte = 0x1a });
        try testParse("1b", .byte, allow_suffix, .{ .byte = 0x1b });
        try testParse("1c", .byte, allow_suffix, .{ .byte = 0x1c });
        try testParse("1d", .byte, allow_suffix, .{ .byte = 0x1d });
        try testParse("1e", .byte, allow_suffix, .{ .byte = 0x1e });
        try testParse("1f", .byte, allow_suffix, .{ .byte = 0x1f });

        try testParse("g", .byte, allow_suffix, invalidDigit(0, .hex));
        try testParse("100", .byte, allow_suffix, overflow);
    }
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
