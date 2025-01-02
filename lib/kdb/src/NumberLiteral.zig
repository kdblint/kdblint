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
    prefer_month_inf,

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
            '.' => .float,
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
        .bool => return parseBool(bytes),
        .guid => return parseGuid(bytes, allow_suffix),
        .byte => return parseByte(bytes),
        .short => return parseShort(bytes, allow_suffix),
        .int => return parseInt(bytes, allow_suffix),
        .long => return parseLong(bytes, allow_suffix),
        .real => return parseReal(bytes, allow_suffix),
        .float => return parseFloat(bytes, allow_suffix),
        .char => return parseChar(bytes, allow_suffix),
        .timestamp => return .{ .failure = .nyi },
        .month => return parseMonth(bytes, allow_suffix),
        .date => return .{ .failure = .nyi },
        .datetime => return .{ .failure = .nyi },
        .timespan => return .{ .failure = .nyi },
        .minute => return .{ .failure = .nyi },
        .second => return .{ .failure = .nyi },
        .time => return .{ .failure = .nyi },
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

    var x: u64 = 0;
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

            return .{ .long = @intCast(x) };
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

        if (x == inf_long) return .{ .failure = .prefer_long_inf };
        if (x > inf_long) return .{ .failure = .overflow };

        return .{ .long = @intCast(x) };
    }
}

fn parseBool(bytes: []const u8) Result {
    const value = switch (bytes[0]) {
        '0' => false,
        '1' => true,
        else => return .{ .failure = .{
            .invalid_digit = .{ .i = 0, .base = .binary },
        } },
    };

    if (bytes.len > 1) return .{ .failure = .{ .invalid_character = 1 } };

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
    var x: u16 = 0;
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

    return .{ .short = @intCast(x) };
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
    var x: u32 = 0;
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

    return .{ .int = @intCast(x) };
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
    var x: u64 = 0;
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

    return .{ .long = @intCast(x) };
}

fn parseReal(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .real,
        'W', 'w' => return .real,
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'e' => .real,
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'e' => .real,
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    var i: usize = 0;
    var x: u64 = 0;
    var period = false;
    var special: u8 = 0;
    var exponent = false;
    var float = false;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        switch (c) {
            'e' => if (i == bytes.len - 1) {
                if (special != 0) return .{ .failure = .{ .trailing_special = i - 1 } };
                if (allow_suffix) continue;
                return .{ .failure = .{ .invalid_character = i } };
            } else {
                float = true;
                if (exponent) {
                    return .{ .failure = .{ .duplicate_exponent = i } };
                }
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
                if (period) return .{ .failure = .duplicate_period };
                period = true;
                continue;
            },
            '+', '-' => {
                switch (special) {
                    'e' => {},
                    else => return .{ .failure = .{ .invalid_exponent_sign = i } },
                }
                special = c;
                continue;
            },
            else => {},
        }
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'A'...'Z', 'a'...'z' => return .{ .failure = .{
                .invalid_digit = .{ .i = i, .base = .decimal },
            } },
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        special = 0;

        if (float) continue;
        if (x != 0) {
            const res = @mulWithOverflow(x, 10);
            if (res[1] != 0) return .{ .failure = .overflow };
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .{ .failure = .overflow };
        x = res[0];
    }
    if (special != 0) return .{ .failure = .{ .trailing_special = i - 1 } };

    return .real;
}

fn parseFloat(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .float,
        'W', 'w' => return .float,
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'f' => .float,
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'f' => .float,
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    var i: usize = 0;
    var x: u64 = 0;
    var period = false;
    var special: u8 = 0;
    var exponent = false;
    var float = false;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        switch (c) {
            'e' => {
                if (i + 1 < bytes.len) switch (bytes[i + 1]) {
                    '+', '-' => {},
                    '0'...'9' => {},
                    else => return .{ .failure = .{ .trailing_special = i } },
                };
                float = true;
                if (exponent) {
                    return .{ .failure = .{ .duplicate_exponent = i } };
                }
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
                if (period) return .{ .failure = .duplicate_period };
                period = true;
                continue;
            },
            '+', '-' => {
                switch (special) {
                    'e' => if (i + 1 < bytes.len) switch (bytes[i + 1]) {
                        '0'...'9' => {},
                        else => return .{ .failure = .{ .trailing_special = i } },
                    },
                    else => return .{ .failure = .{ .invalid_exponent_sign = i } },
                }
                special = c;
                continue;
            },
            else => {},
        }
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'f' => if (allow_suffix and i == bytes.len - 1)
                continue
            else
                return .{ .failure = .{ .invalid_character = i } },
            'A'...'Z', 'a'...'e', 'g'...'z' => return .{ .failure = .{
                .invalid_digit = .{ .i = i, .base = .decimal },
            } },
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        special = 0;

        if (float) continue;
        if (x != 0) {
            const res = @mulWithOverflow(x, 10);
            if (res[1] != 0) return .{ .failure = .overflow };
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .{ .failure = .overflow };
        x = res[0];
    }
    if (special != 0) return .{ .failure = .{ .trailing_special = i - 1 } };

    return .float;
}

fn parseChar(bytes: []const u8, allow_suffix: bool) Result {
    return switch (bytes.len) {
        0 => unreachable,
        1 => switch (bytes[0]) {
            '0'...'9' => .{ .char = bytes[0] },
            else => .{ .char = null_char },
        },
        2 => switch (bytes[0]) {
            '0'...'9' => if (allow_suffix and bytes[1] == 'c')
                .{ .char = bytes[0] }
            else
                .{ .char = null_char },
            else => .{ .char = null_char },
        },
        else => .{ .char = null_char },
    };
}

fn parseMonth(bytes: []const u8, allow_suffix: bool) Result {
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

    var i: usize = 0;
    var x: i32 = 0;
    var year: ?i32 = null;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'm' => if (allow_suffix and i == bytes.len - 1)
                continue
            else
                return .{ .failure = .{ .invalid_character = i } },
            '.' => {
                if (i != 4) return .{ .failure = .{ .invalid_character = i } };
                year = x - 2000;
                x = 0;
                continue;
            },
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

    if (year) |y| {
        if (x == 0 or x > 12) return .{ .failure = .overflow };

        return .{ .month = y * 12 + x - 1 };
    } else {
        if (x == inf_int) return .{ .failure = .prefer_month_inf };
        if (x > inf_int) return .{ .failure = .overflow };

        return .{ .month = x };
    }
}

fn testParse(bytes: []const u8, type_hint: TypeHint, allow_suffix: bool, expected: Result) !void {
    const result = parse(bytes, type_hint, allow_suffix);
    try std.testing.expectEqual(expected, result);
}

const overflow: Result = .{ .failure = .overflow };
fn invalidCharacter(i: usize) Result {
    return .{ .failure = .{ .invalid_character = i } };
}
fn trailingSpecial(i: usize) Result {
    return .{ .failure = .{ .trailing_special = i } };
}
fn periodAfterExponent(i: usize) Result {
    return .{ .failure = .{ .period_after_exponent = i } };
}
fn invalidBinary(i: usize) Result {
    return .{ .failure = .{ .invalid_digit = .{ .i = i, .base = .binary } } };
}
fn invalidHex(i: usize) Result {
    return .{ .failure = .{ .invalid_digit = .{ .i = i, .base = .hex } } };
}

test "parse number literal - bool" {
    try testParse("0n", .bool, undefined, invalidCharacter(1));
    try testParse("0N", .bool, undefined, invalidCharacter(1));
    try testParse("0w", .bool, undefined, invalidCharacter(1));
    try testParse("0W", .bool, undefined, invalidCharacter(1));
    try testParse("0", .bool, undefined, .{ .bool = false });
    try testParse("1", .bool, undefined, .{ .bool = true });
    try testParse("2", .bool, undefined, invalidBinary(0));
}

test "parse number literal - guid" {
    try testParse("0n", .guid, false, .guid);
    try testParse("0N", .guid, false, .guid);
    try testParse("0w", .guid, false, invalidCharacter(1));
    try testParse("0W", .guid, false, invalidCharacter(1));
    try testParse("0", .guid, false, invalidCharacter(0));
    try testParse("1", .guid, false, invalidCharacter(0));
    try testParse("2", .guid, false, invalidCharacter(0));
    try testParse("0n", .guid, true, .guid);
    try testParse("0N", .guid, true, .guid);
    try testParse("0w", .guid, true, invalidCharacter(1));
    try testParse("0W", .guid, true, invalidCharacter(1));
    try testParse("0", .guid, true, invalidCharacter(0));
    try testParse("1", .guid, true, invalidCharacter(0));
    try testParse("2", .guid, true, invalidCharacter(0));

    try testParse("0ng", .guid, false, invalidCharacter(2));
    try testParse("0Ng", .guid, false, invalidCharacter(2));
    try testParse("0wg", .guid, false, invalidCharacter(1));
    try testParse("0Wg", .guid, false, invalidCharacter(1));
    try testParse("0g", .guid, false, invalidCharacter(1));
    try testParse("1g", .guid, false, invalidCharacter(0));
    try testParse("2g", .guid, false, invalidCharacter(0));
    try testParse("0ng", .guid, true, .guid);
    try testParse("0Ng", .guid, true, .guid);
    try testParse("0wg", .guid, true, invalidCharacter(1));
    try testParse("0Wg", .guid, true, invalidCharacter(1));
    try testParse("0g", .guid, true, invalidCharacter(1));
    try testParse("1g", .guid, true, invalidCharacter(0));
    try testParse("2g", .guid, true, invalidCharacter(0));
}

test "parse number literal - byte" {
    try testParse("0n", .byte, undefined, invalidHex(1));
    try testParse("0N", .byte, undefined, invalidHex(1));
    try testParse("0w", .byte, undefined, invalidHex(1));
    try testParse("0W", .byte, undefined, invalidHex(1));
    try testParse("0", .byte, undefined, .{ .byte = 0x0 });
    try testParse("1", .byte, undefined, .{ .byte = 0x1 });
    try testParse("2", .byte, undefined, .{ .byte = 0x2 });
    try testParse("3", .byte, undefined, .{ .byte = 0x3 });
    try testParse("4", .byte, undefined, .{ .byte = 0x4 });
    try testParse("5", .byte, undefined, .{ .byte = 0x5 });
    try testParse("6", .byte, undefined, .{ .byte = 0x6 });
    try testParse("7", .byte, undefined, .{ .byte = 0x7 });
    try testParse("8", .byte, undefined, .{ .byte = 0x8 });
    try testParse("9", .byte, undefined, .{ .byte = 0x9 });
    try testParse("a", .byte, undefined, .{ .byte = 0xa });
    try testParse("b", .byte, undefined, .{ .byte = 0xb });
    try testParse("c", .byte, undefined, .{ .byte = 0xc });
    try testParse("d", .byte, undefined, .{ .byte = 0xd });
    try testParse("e", .byte, undefined, .{ .byte = 0xe });
    try testParse("f", .byte, undefined, .{ .byte = 0xf });
    try testParse("00", .byte, undefined, .{ .byte = 0x0 });
    try testParse("01", .byte, undefined, .{ .byte = 0x1 });
    try testParse("02", .byte, undefined, .{ .byte = 0x2 });
    try testParse("03", .byte, undefined, .{ .byte = 0x3 });
    try testParse("04", .byte, undefined, .{ .byte = 0x4 });
    try testParse("05", .byte, undefined, .{ .byte = 0x5 });
    try testParse("06", .byte, undefined, .{ .byte = 0x6 });
    try testParse("07", .byte, undefined, .{ .byte = 0x7 });
    try testParse("08", .byte, undefined, .{ .byte = 0x8 });
    try testParse("09", .byte, undefined, .{ .byte = 0x9 });
    try testParse("0a", .byte, undefined, .{ .byte = 0xa });
    try testParse("0b", .byte, undefined, .{ .byte = 0xb });
    try testParse("0c", .byte, undefined, .{ .byte = 0xc });
    try testParse("0d", .byte, undefined, .{ .byte = 0xd });
    try testParse("0e", .byte, undefined, .{ .byte = 0xe });
    try testParse("0f", .byte, undefined, .{ .byte = 0xf });
    try testParse("10", .byte, undefined, .{ .byte = 0x10 });
    try testParse("11", .byte, undefined, .{ .byte = 0x11 });
    try testParse("12", .byte, undefined, .{ .byte = 0x12 });
    try testParse("13", .byte, undefined, .{ .byte = 0x13 });
    try testParse("14", .byte, undefined, .{ .byte = 0x14 });
    try testParse("15", .byte, undefined, .{ .byte = 0x15 });
    try testParse("16", .byte, undefined, .{ .byte = 0x16 });
    try testParse("17", .byte, undefined, .{ .byte = 0x17 });
    try testParse("18", .byte, undefined, .{ .byte = 0x18 });
    try testParse("19", .byte, undefined, .{ .byte = 0x19 });
    try testParse("1a", .byte, undefined, .{ .byte = 0x1a });
    try testParse("1b", .byte, undefined, .{ .byte = 0x1b });
    try testParse("1c", .byte, undefined, .{ .byte = 0x1c });
    try testParse("1d", .byte, undefined, .{ .byte = 0x1d });
    try testParse("1e", .byte, undefined, .{ .byte = 0x1e });
    try testParse("1f", .byte, undefined, .{ .byte = 0x1f });

    try testParse("g", .byte, undefined, invalidHex(0));
    try testParse("100", .byte, undefined, overflow);
}

test "parse number literal - short" {
    try testParse("0n", .short, false, .{ .short = null_short });
    try testParse("0N", .short, false, .{ .short = null_short });
    try testParse("0w", .short, false, .{ .short = inf_short });
    try testParse("0W", .short, false, .{ .short = inf_short });
    try testParse("1", .short, false, .{ .short = 1 });
    try testParse("2", .short, false, .{ .short = 2 });
    try testParse("32766", .short, false, .{ .short = 32766 });
    try testParse("32767", .short, false, .{
        .failure = .prefer_short_inf,
    });
    try testParse("32768", .short, false, overflow);
    try testParse("0n", .short, true, .{ .short = null_short });
    try testParse("0N", .short, true, .{ .short = null_short });
    try testParse("0w", .short, true, .{ .short = inf_short });
    try testParse("0W", .short, true, .{ .short = inf_short });
    try testParse("0", .short, true, .{ .short = 0 });
    try testParse("1", .short, true, .{ .short = 1 });
    try testParse("2", .short, true, .{ .short = 2 });
    try testParse("32766", .short, true, .{ .short = 32766 });
    try testParse("32767", .short, true, .{
        .failure = .prefer_short_inf,
    });
    try testParse("32768", .short, true, overflow);

    try testParse("0nh", .short, false, invalidCharacter(2));
    try testParse("0Nh", .short, false, invalidCharacter(2));
    try testParse("0wh", .short, false, invalidCharacter(2));
    try testParse("0Wh", .short, false, invalidCharacter(2));
    try testParse("0h", .short, false, invalidCharacter(1));
    try testParse("1h", .short, false, invalidCharacter(1));
    try testParse("2h", .short, false, invalidCharacter(1));
    try testParse("32766h", .short, false, invalidCharacter(5));
    try testParse("32767h", .short, false, invalidCharacter(5));
    try testParse("32768h", .short, false, invalidCharacter(5));
    try testParse("0nh", .short, true, .{ .short = null_short });
    try testParse("0Nh", .short, true, .{ .short = null_short });
    try testParse("0wh", .short, true, .{ .short = inf_short });
    try testParse("0Wh", .short, true, .{ .short = inf_short });
    try testParse("0h", .short, true, .{ .short = 0 });
    try testParse("1h", .short, true, .{ .short = 1 });
    try testParse("2h", .short, true, .{ .short = 2 });
    try testParse("32766h", .short, true, .{ .short = 32766 });
    try testParse("32767h", .short, true, .{
        .failure = .prefer_short_inf,
    });
    try testParse("32768h", .short, true, overflow);
}

test "parse number literal - int" {
    try testParse("0n", .int, false, .{ .int = null_int });
    try testParse("0N", .int, false, .{ .int = null_int });
    try testParse("0w", .int, false, .{ .int = inf_int });
    try testParse("0W", .int, false, .{ .int = inf_int });
    try testParse("1", .int, false, .{ .int = 1 });
    try testParse("2", .int, false, .{ .int = 2 });
    try testParse("2147483646", .int, false, .{ .int = 2147483646 });
    try testParse("2147483647", .int, false, .{
        .failure = .prefer_int_inf,
    });
    try testParse("2147483648", .int, false, overflow);
    try testParse("0n", .int, true, .{ .int = null_int });
    try testParse("0N", .int, true, .{ .int = null_int });
    try testParse("0w", .int, true, .{ .int = inf_int });
    try testParse("0W", .int, true, .{ .int = inf_int });
    try testParse("0", .int, true, .{ .int = 0 });
    try testParse("1", .int, true, .{ .int = 1 });
    try testParse("2", .int, true, .{ .int = 2 });
    try testParse("2147483646", .int, true, .{ .int = 2147483646 });
    try testParse("2147483647", .int, true, .{
        .failure = .prefer_int_inf,
    });
    try testParse("2147483648", .int, true, overflow);

    try testParse("0ni", .int, false, invalidCharacter(2));
    try testParse("0Ni", .int, false, invalidCharacter(2));
    try testParse("0wi", .int, false, invalidCharacter(2));
    try testParse("0Wi", .int, false, invalidCharacter(2));
    try testParse("0i", .int, false, invalidCharacter(1));
    try testParse("1i", .int, false, invalidCharacter(1));
    try testParse("2i", .int, false, invalidCharacter(1));
    try testParse("2147483646i", .int, false, invalidCharacter(10));
    try testParse("2147483647i", .int, false, invalidCharacter(10));
    try testParse("2147483648i", .int, false, invalidCharacter(10));
    try testParse("0ni", .int, true, .{ .int = null_int });
    try testParse("0Ni", .int, true, .{ .int = null_int });
    try testParse("0wi", .int, true, .{ .int = inf_int });
    try testParse("0Wi", .int, true, .{ .int = inf_int });
    try testParse("0i", .int, true, .{ .int = 0 });
    try testParse("1i", .int, true, .{ .int = 1 });
    try testParse("2i", .int, true, .{ .int = 2 });
    try testParse("2147483646i", .int, true, .{ .int = 2147483646 });
    try testParse("2147483647i", .int, true, .{
        .failure = .prefer_int_inf,
    });
    try testParse("2147483648i", .int, true, overflow);
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
    try testParse("9223372036854775808j", .long, false, invalidCharacter(19));
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

test "parse number literal - real" {
    try testParse("0n", .real, false, .real);
    try testParse("0N", .real, false, .real);
    try testParse("0w", .real, false, .real);
    try testParse("0W", .real, false, .real);
    try testParse("0", .real, false, .real);
    try testParse("1", .real, false, .real);
    try testParse(".2", .real, false, .real);
    try testParse("3.", .real, false, .real);
    try testParse("3.4", .real, false, .real);
    try testParse("3.4e", .real, false, invalidCharacter(3));
    try testParse("3.4e+", .real, false, trailingSpecial(4));
    try testParse("3.4e-", .real, false, trailingSpecial(4));
    try testParse("3.4e0", .real, false, .real);
    try testParse("3.4e1", .real, false, .real);
    try testParse("3.4e+0", .real, false, .real);
    try testParse("3.4e-0", .real, false, .real);
    try testParse("3.4e00", .real, false, .real);
    try testParse("3.4e10", .real, false, .real);
    try testParse("3.4e+1", .real, false, .real);
    try testParse("3.4e-1", .real, false, .real);
    try testParse("3.4e01", .real, false, .real);
    try testParse("3.4e11", .real, false, .real);
    try testParse("3.4e+12", .real, false, .real);
    try testParse("3.4e-12", .real, false, .real);
    try testParse("3.4e012", .real, false, .real);
    try testParse("3.4e112", .real, false, .real);
    try testParse("3.4e+12.", .real, false, periodAfterExponent(7));
    try testParse("3.4e-12.", .real, false, periodAfterExponent(7));
    try testParse("3.4e012.", .real, false, periodAfterExponent(7));
    try testParse("3.4e112.", .real, false, periodAfterExponent(7));
    try testParse("3.4e+12.3", .real, false, periodAfterExponent(7));
    try testParse("3.4e-12.3", .real, false, periodAfterExponent(7));
    try testParse("3.4e012.3", .real, false, periodAfterExponent(7));
    try testParse("3.4e112.3", .real, false, periodAfterExponent(7));
    try testParse("0n", .real, true, .real);
    try testParse("0N", .real, true, .real);
    try testParse("0w", .real, true, .real);
    try testParse("0W", .real, true, .real);
    try testParse("0", .real, true, .real);
    try testParse("1", .real, true, .real);
    try testParse(".2", .real, true, .real);
    try testParse("3.", .real, true, .real);
    try testParse("3.4", .real, true, .real);
    try testParse("3.4e", .real, true, .real);
    try testParse("3.4e+", .real, true, trailingSpecial(4));
    try testParse("3.4e-", .real, true, trailingSpecial(4));
    try testParse("3.4e0", .real, true, .real);
    try testParse("3.4e1", .real, true, .real);
    try testParse("3.4e+0", .real, true, .real);
    try testParse("3.4e-0", .real, true, .real);
    try testParse("3.4e00", .real, true, .real);
    try testParse("3.4e10", .real, true, .real);
    try testParse("3.4e+1", .real, true, .real);
    try testParse("3.4e-1", .real, true, .real);
    try testParse("3.4e01", .real, true, .real);
    try testParse("3.4e11", .real, true, .real);
    try testParse("3.4e+12", .real, true, .real);
    try testParse("3.4e-12", .real, true, .real);
    try testParse("3.4e012", .real, true, .real);
    try testParse("3.4e112", .real, true, .real);
    try testParse("3.4e+12.", .real, true, periodAfterExponent(7));
    try testParse("3.4e-12.", .real, true, periodAfterExponent(7));
    try testParse("3.4e012.", .real, true, periodAfterExponent(7));
    try testParse("3.4e112.", .real, true, periodAfterExponent(7));
    try testParse("3.4e+12.3", .real, true, periodAfterExponent(7));
    try testParse("3.4e-12.3", .real, true, periodAfterExponent(7));
    try testParse("3.4e012.3", .real, true, periodAfterExponent(7));
    try testParse("3.4e112.3", .real, true, periodAfterExponent(7));

    try testParse("0ne", .real, false, invalidCharacter(2));
    try testParse("0Ne", .real, false, invalidCharacter(2));
    try testParse("0we", .real, false, invalidCharacter(2));
    try testParse("0We", .real, false, invalidCharacter(2));
    try testParse("0e", .real, false, invalidCharacter(1));
    try testParse("1e", .real, false, invalidCharacter(1));
    try testParse(".2e", .real, false, invalidCharacter(2));
    try testParse("3.e", .real, false, invalidCharacter(2));
    try testParse("3.4e", .real, false, invalidCharacter(3));
    try testParse("3.4ee", .real, false, trailingSpecial(3));
    try testParse("3.4e+e", .real, false, trailingSpecial(4));
    try testParse("3.4e-e", .real, false, trailingSpecial(4));
    try testParse("3.4e0e", .real, false, invalidCharacter(5));
    try testParse("3.4e1e", .real, false, invalidCharacter(5));
    try testParse("3.4e+0e", .real, false, invalidCharacter(6));
    try testParse("3.4e-0e", .real, false, invalidCharacter(6));
    try testParse("3.4e00e", .real, false, invalidCharacter(6));
    try testParse("3.4e10e", .real, false, invalidCharacter(6));
    try testParse("3.4e+1e", .real, false, invalidCharacter(6));
    try testParse("3.4e-1e", .real, false, invalidCharacter(6));
    try testParse("3.4e01e", .real, false, invalidCharacter(6));
    try testParse("3.4e11e", .real, false, invalidCharacter(6));
    try testParse("3.4e+12e", .real, false, invalidCharacter(7));
    try testParse("3.4e-12e", .real, false, invalidCharacter(7));
    try testParse("3.4e012e", .real, false, invalidCharacter(7));
    try testParse("3.4e112e", .real, false, invalidCharacter(7));
    try testParse("3.4e+12.e", .real, false, periodAfterExponent(7));
    try testParse("3.4e-12.e", .real, false, periodAfterExponent(7));
    try testParse("3.4e012.e", .real, false, periodAfterExponent(7));
    try testParse("3.4e112.e", .real, false, periodAfterExponent(7));
    try testParse("3.4e+12.3e", .real, false, periodAfterExponent(7));
    try testParse("3.4e-12.3e", .real, false, periodAfterExponent(7));
    try testParse("3.4e012.3e", .real, false, periodAfterExponent(7));
    try testParse("3.4e112.3e", .real, false, periodAfterExponent(7));
    try testParse("0ne", .real, true, .real);
    try testParse("0Ne", .real, true, .real);
    try testParse("0we", .real, true, .real);
    try testParse("0We", .real, true, .real);
    try testParse("0e", .real, true, .real);
    try testParse("1e", .real, true, .real);
    try testParse(".2e", .real, true, .real);
    try testParse("3.e", .real, true, .real);
    try testParse("3.4e", .real, true, .real);
    try testParse("3.4ee", .real, true, trailingSpecial(3));
    try testParse("3.4e+e", .real, true, trailingSpecial(4));
    try testParse("3.4e-e", .real, true, trailingSpecial(4));
    try testParse("3.4e0e", .real, true, .real);
    try testParse("3.4e1e", .real, true, .real);
    try testParse("3.4e+0e", .real, true, .real);
    try testParse("3.4e-0e", .real, true, .real);
    try testParse("3.4e00e", .real, true, .real);
    try testParse("3.4e10e", .real, true, .real);
    try testParse("3.4e+1e", .real, true, .real);
    try testParse("3.4e-1e", .real, true, .real);
    try testParse("3.4e01e", .real, true, .real);
    try testParse("3.4e11e", .real, true, .real);
    try testParse("3.4e+12e", .real, true, .real);
    try testParse("3.4e-12e", .real, true, .real);
    try testParse("3.4e012e", .real, true, .real);
    try testParse("3.4e112e", .real, true, .real);
    try testParse("3.4e+12.e", .real, true, periodAfterExponent(7));
    try testParse("3.4e-12.e", .real, true, periodAfterExponent(7));
    try testParse("3.4e012.e", .real, true, periodAfterExponent(7));
    try testParse("3.4e112.e", .real, true, periodAfterExponent(7));
    try testParse("3.4e+12.3e", .real, true, periodAfterExponent(7));
    try testParse("3.4e-12.3e", .real, true, periodAfterExponent(7));
    try testParse("3.4e012.3e", .real, true, periodAfterExponent(7));
    try testParse("3.4e112.3e", .real, true, periodAfterExponent(7));
}

test "parse number literal - float" {
    try testParse("0n", .float, false, .float);
    try testParse("0N", .float, false, .float);
    try testParse("0w", .float, false, .float);
    try testParse("0W", .float, false, .float);
    try testParse("0", .float, false, .float);
    try testParse("1", .float, false, .float);
    try testParse(".2", .float, false, .float);
    try testParse("3.", .float, false, .float);
    try testParse("3.4", .float, false, .float);
    try testParse("3.4e", .float, false, trailingSpecial(3));
    try testParse("3.4e+", .float, false, trailingSpecial(4));
    try testParse("3.4e-", .float, false, trailingSpecial(4));
    try testParse("3.4e0", .float, false, .float);
    try testParse("3.4e1", .float, false, .float);
    try testParse("3.4e+0", .float, false, .float);
    try testParse("3.4e-0", .float, false, .float);
    try testParse("3.4e00", .float, false, .float);
    try testParse("3.4e10", .float, false, .float);
    try testParse("3.4e+1", .float, false, .float);
    try testParse("3.4e-1", .float, false, .float);
    try testParse("3.4e01", .float, false, .float);
    try testParse("3.4e11", .float, false, .float);
    try testParse("3.4e+12", .float, false, .float);
    try testParse("3.4e-12", .float, false, .float);
    try testParse("3.4e012", .float, false, .float);
    try testParse("3.4e112", .float, false, .float);
    try testParse("3.4e+12.", .float, false, periodAfterExponent(7));
    try testParse("3.4e-12.", .float, false, periodAfterExponent(7));
    try testParse("3.4e012.", .float, false, periodAfterExponent(7));
    try testParse("3.4e112.", .float, false, periodAfterExponent(7));
    try testParse("3.4e+12.3", .float, false, periodAfterExponent(7));
    try testParse("3.4e-12.3", .float, false, periodAfterExponent(7));
    try testParse("3.4e012.3", .float, false, periodAfterExponent(7));
    try testParse("3.4e112.3", .float, false, periodAfterExponent(7));
    try testParse("0n", .float, true, .float);
    try testParse("0N", .float, true, .float);
    try testParse("0w", .float, true, .float);
    try testParse("0W", .float, true, .float);
    try testParse("0", .float, true, .float);
    try testParse("1", .float, true, .float);
    try testParse(".2", .float, true, .float);
    try testParse("3.", .float, true, .float);
    try testParse("3.4", .float, true, .float);
    try testParse("3.4e", .float, true, trailingSpecial(3));
    try testParse("3.4e+", .float, true, trailingSpecial(4));
    try testParse("3.4e-", .float, true, trailingSpecial(4));
    try testParse("3.4e0", .float, true, .float);
    try testParse("3.4e1", .float, true, .float);
    try testParse("3.4e+0", .float, true, .float);
    try testParse("3.4e-0", .float, true, .float);
    try testParse("3.4e00", .float, true, .float);
    try testParse("3.4e10", .float, true, .float);
    try testParse("3.4e+1", .float, true, .float);
    try testParse("3.4e-1", .float, true, .float);
    try testParse("3.4e01", .float, true, .float);
    try testParse("3.4e11", .float, true, .float);
    try testParse("3.4e+12", .float, true, .float);
    try testParse("3.4e-12", .float, true, .float);
    try testParse("3.4e012", .float, true, .float);
    try testParse("3.4e112", .float, true, .float);
    try testParse("3.4e+12.", .float, true, periodAfterExponent(7));
    try testParse("3.4e-12.", .float, true, periodAfterExponent(7));
    try testParse("3.4e012.", .float, true, periodAfterExponent(7));
    try testParse("3.4e112.", .float, true, periodAfterExponent(7));
    try testParse("3.4e+12.3", .float, true, periodAfterExponent(7));
    try testParse("3.4e-12.3", .float, true, periodAfterExponent(7));
    try testParse("3.4e012.3", .float, true, periodAfterExponent(7));
    try testParse("3.4e112.3", .float, true, periodAfterExponent(7));

    try testParse("0nf", .float, false, invalidCharacter(2));
    try testParse("0Nf", .float, false, invalidCharacter(2));
    try testParse("0wf", .float, false, invalidCharacter(2));
    try testParse("0Wf", .float, false, invalidCharacter(2));
    try testParse("0f", .float, false, invalidCharacter(1));
    try testParse("1f", .float, false, invalidCharacter(1));
    try testParse(".2f", .float, false, invalidCharacter(2));
    try testParse("3.f", .float, false, invalidCharacter(2));
    try testParse("3.4f", .float, false, invalidCharacter(3));
    try testParse("3.4ef", .float, false, trailingSpecial(3));
    try testParse("3.4e+f", .float, false, trailingSpecial(4));
    try testParse("3.4e-f", .float, false, trailingSpecial(4));
    try testParse("3.4e0f", .float, false, invalidCharacter(5));
    try testParse("3.4e1f", .float, false, invalidCharacter(5));
    try testParse("3.4e+0f", .float, false, invalidCharacter(6));
    try testParse("3.4e-0f", .float, false, invalidCharacter(6));
    try testParse("3.4e00f", .float, false, invalidCharacter(6));
    try testParse("3.4e10f", .float, false, invalidCharacter(6));
    try testParse("3.4e+1f", .float, false, invalidCharacter(6));
    try testParse("3.4e-1f", .float, false, invalidCharacter(6));
    try testParse("3.4e01f", .float, false, invalidCharacter(6));
    try testParse("3.4e11f", .float, false, invalidCharacter(6));
    try testParse("3.4e+12f", .float, false, invalidCharacter(7));
    try testParse("3.4e-12f", .float, false, invalidCharacter(7));
    try testParse("3.4e012f", .float, false, invalidCharacter(7));
    try testParse("3.4e112f", .float, false, invalidCharacter(7));
    try testParse("3.4e+12.f", .float, false, periodAfterExponent(7));
    try testParse("3.4e-12.f", .float, false, periodAfterExponent(7));
    try testParse("3.4e012.f", .float, false, periodAfterExponent(7));
    try testParse("3.4e112.f", .float, false, periodAfterExponent(7));
    try testParse("3.4e+12.3f", .float, false, periodAfterExponent(7));
    try testParse("3.4e-12.3f", .float, false, periodAfterExponent(7));
    try testParse("3.4e012.3f", .float, false, periodAfterExponent(7));
    try testParse("3.4e112.3f", .float, false, periodAfterExponent(7));
    try testParse("0nf", .float, true, .float);
    try testParse("0Nf", .float, true, .float);
    try testParse("0wf", .float, true, .float);
    try testParse("0Wf", .float, true, .float);
    try testParse("0f", .float, true, .float);
    try testParse("1f", .float, true, .float);
    try testParse(".2f", .float, true, .float);
    try testParse("3.f", .float, true, .float);
    try testParse("3.4f", .float, true, .float);
    try testParse("3.4ef", .float, true, trailingSpecial(3));
    try testParse("3.4e+f", .float, true, trailingSpecial(4));
    try testParse("3.4e-f", .float, true, trailingSpecial(4));
    try testParse("3.4e0f", .float, true, .float);
    try testParse("3.4e1f", .float, true, .float);
    try testParse("3.4e+0f", .float, true, .float);
    try testParse("3.4e-0f", .float, true, .float);
    try testParse("3.4e00f", .float, true, .float);
    try testParse("3.4e10f", .float, true, .float);
    try testParse("3.4e+1f", .float, true, .float);
    try testParse("3.4e-1f", .float, true, .float);
    try testParse("3.4e01f", .float, true, .float);
    try testParse("3.4e11f", .float, true, .float);
    try testParse("3.4e+12f", .float, true, .float);
    try testParse("3.4e-12f", .float, true, .float);
    try testParse("3.4e012f", .float, true, .float);
    try testParse("3.4e112f", .float, true, .float);
    try testParse("3.4e+12.f", .float, true, periodAfterExponent(7));
    try testParse("3.4e-12.f", .float, true, periodAfterExponent(7));
    try testParse("3.4e012.f", .float, true, periodAfterExponent(7));
    try testParse("3.4e112.f", .float, true, periodAfterExponent(7));
    try testParse("3.4e+12.3f", .float, true, periodAfterExponent(7));
    try testParse("3.4e-12.3f", .float, true, periodAfterExponent(7));
    try testParse("3.4e012.3f", .float, true, periodAfterExponent(7));
    try testParse("3.4e112.3f", .float, true, periodAfterExponent(7));
}

test "parse number literal - char" {
    try testParse("0n", .char, false, .{ .char = null_char });
    try testParse("0N", .char, false, .{ .char = null_char });
    try testParse("0w", .char, false, .{ .char = null_char });
    try testParse("0W", .char, false, .{ .char = null_char });
    try testParse("0", .char, false, .{ .char = '0' });
    try testParse("1", .char, false, .{ .char = '1' });
    try testParse("2", .char, false, .{ .char = '2' });
    try testParse("0n", .char, true, .{ .char = null_char });
    try testParse("0N", .char, true, .{ .char = null_char });
    try testParse("0w", .char, true, .{ .char = null_char });
    try testParse("0W", .char, true, .{ .char = null_char });
    try testParse("0", .char, true, .{ .char = '0' });
    try testParse("1", .char, true, .{ .char = '1' });
    try testParse("2", .char, true, .{ .char = '2' });

    try testParse("0nc", .char, false, .{ .char = null_char });
    try testParse("0Nc", .char, false, .{ .char = null_char });
    try testParse("0wc", .char, false, .{ .char = null_char });
    try testParse("0Wc", .char, false, .{ .char = null_char });
    try testParse("0c", .char, false, .{ .char = null_char });
    try testParse("1c", .char, false, .{ .char = null_char });
    try testParse("2c", .char, false, .{ .char = null_char });
    try testParse("0nc", .char, true, .{ .char = null_char });
    try testParse("0Nc", .char, true, .{ .char = null_char });
    try testParse("0wc", .char, true, .{ .char = null_char });
    try testParse("0Wc", .char, true, .{ .char = null_char });
    try testParse("0c", .char, true, .{ .char = '0' });
    try testParse("1c", .char, true, .{ .char = '1' });
    try testParse("2c", .char, true, .{ .char = '2' });
}

test "parse number literal - month" {
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

    try testParse("0", .month, false, invalidCharacter(1));
    try testParse("1", .month, false, invalidCharacter(1));
    try testParse("9", .month, false, invalidCharacter(1));

    try testParse("00", .month, false, invalidCharacter(2));
    try testParse("11", .month, false, invalidCharacter(2));
    try testParse("99", .month, false, invalidCharacter(2));

    try testParse("000", .month, false, invalidCharacter(3));
    try testParse("111", .month, false, invalidCharacter(3));
    try testParse("999", .month, false, invalidCharacter(3));

    try testParse("0000", .month, false, overflow);
    try testParse("0013", .month, false, overflow);
    try testParse("9999", .month, false, overflow);

    try testParse("00000", .month, false, invalidCharacter(5));
    try testParse("11111", .month, false, invalidCharacter(5));
    try testParse("99999", .month, false, invalidCharacter(5));

    try testParse("000000", .month, false, overflow);
    try testParse("000113", .month, false, overflow);
    try testParse("999999", .month, false, overflow);

    try testParse("1111111", .month, false, invalidCharacter(7));

    try testParse("2000.1", .month, false, invalidCharacter(6));
    try testParse("2000.00", .month, false, overflow);
    try testParse("2000.13", .month, false, overflow);

    try testParse("0m", .month, true, invalidCharacter(1));
    try testParse("1m", .month, true, invalidCharacter(1));
    try testParse("9m", .month, true, invalidCharacter(1));

    try testParse("00m", .month, true, invalidCharacter(2));
    try testParse("11m", .month, true, invalidCharacter(2));
    try testParse("99m", .month, true, invalidCharacter(2));

    try testParse("000m", .month, true, invalidCharacter(3));
    try testParse("111m", .month, true, invalidCharacter(3));
    try testParse("999m", .month, true, invalidCharacter(3));

    try testParse("0000m", .month, true, overflow);
    try testParse("0013m", .month, true, overflow);
    try testParse("9999m", .month, true, overflow);

    try testParse("00000m", .month, true, invalidCharacter(5));
    try testParse("11111m", .month, true, invalidCharacter(5));
    try testParse("99999m", .month, true, invalidCharacter(5));

    try testParse("000000m", .month, true, overflow);
    try testParse("000113m", .month, true, overflow);
    try testParse("999999m", .month, true, overflow);

    try testParse("1111111m", .month, true, invalidCharacter(7));

    try testParse("2000.1m", .month, true, invalidCharacter(6));
    try testParse("2000.00m", .month, true, overflow);
    try testParse("2000.13m", .month, true, overflow);
}
