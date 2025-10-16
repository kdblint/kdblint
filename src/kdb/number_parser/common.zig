const std = @import("std");

pub const Base = enum(u8) { decimal = 10, hex = 16, binary = 2 };

pub const Type = enum {
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
    real: f32,
    float: f64,
    char: u8,
    timestamp: i64,
    month: i32,
    date: i32,
    datetime: f64,
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

pub const null_real = std.math.nan(f32);
pub const inf_real = std.math.inf(f32);

pub const null_float = std.math.nan(f64);
pub const inf_float = std.math.inf(f64);

pub const epoch_nanoseconds = -10957 * std.time.ns_per_day;

pub fn parseSlice(T: type, bytes: []const u8) union(enum) { overflow, invalid_character: usize, int: T } {
    var x: T = 0;
    for (bytes, 0..) |c, i| {
        const digit = switch (c) {
            '0'...'9' => c - '0',
            else => return .{ .invalid_character = i },
        };
        if (x != 0) {
            const res = @mulWithOverflow(x, 10);
            if (res[1] != 0) return .overflow;
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .overflow;
        x = res[0];
    }

    return .{ .int = x };
}

pub const overflow: Result = .{ .failure = .overflow };
pub fn invalidCharacter(i: usize) Result {
    return .{ .failure = .{ .invalid_character = i } };
}
pub fn trailingSpecial(i: usize) Result {
    return .{ .failure = .{ .trailing_special = i } };
}
pub fn periodAfterExponent(i: usize) Result {
    return .{ .failure = .{ .period_after_exponent = i } };
}
pub fn invalidBinary(i: usize) Result {
    return .{ .failure = .{ .invalid_digit = .{ .i = i, .base = .binary } } };
}
pub fn invalidHex(i: usize) Result {
    return .{ .failure = .{ .invalid_digit = .{ .i = i, .base = .hex } } };
}

pub fn testParse(bytes: []const u8, type_hint: TypeHint, allow_suffix: bool, expected: Result) !void {
    const result = @import("../number_parser.zig").parse(
        bytes,
        type_hint,
        allow_suffix,
    );
    switch (expected) {
        inline .real, .float, .datetime => |float, t| if (std.math.isNan(float))
            try std.testing.expect(std.math.isNan(@field(result, @tagName(t))))
        else
            try std.testing.expectEqual(expected, result),
        else => try std.testing.expectEqual(expected, result),
    }
}
