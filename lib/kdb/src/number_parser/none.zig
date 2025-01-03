const parse_float = @import("parse_float.zig");

const common = @import("common.zig");
const Result = common.Result;
const Type = common.Type;

const null_char = common.null_char;
const null_short = common.null_short;
const inf_short = common.inf_short;
const null_int = common.null_int;
const inf_int = common.inf_int;
const null_long = common.null_long;
const inf_long = common.inf_long;
const null_real = common.null_real;
const inf_real = common.inf_real;
const null_float = common.null_float;
const inf_float = common.inf_float;

pub fn parseNone(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N' => return .{ .long = null_long },
        'n' => return .{ .float = null_float },
        'W' => return .{ .long = inf_long },
        'w' => return .{ .float = inf_float },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'c' => .{ .char = null_char },
            'd' => .{ .date = null_int },
            'e' => .{ .real = null_real },
            'f' => .{ .float = null_float },
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
            'z' => .{ .datetime = null_float },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'c' => .{ .char = null_char },
            'd' => .{ .date = inf_int },
            'e' => .{ .real = inf_real },
            'f' => .{ .float = inf_float },
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
            'z' => .{ .datetime = inf_float },
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
        if (float) {
            const value = parse_float.parseFloat(f64, bytes) catch return .{ .failure = .nyi };
            return .{ .float = value };
        }

        if (x == inf_long) return .{ .failure = .prefer_long_inf };
        if (x > inf_long) return .{ .failure = .overflow };

        return .{ .long = @intCast(x) };
    }
}
