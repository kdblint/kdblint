const std = @import("std");
const assert = std.debug.assert;

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

pub const Error = enum {
    nyi,
    invalid_type,
    invalid_character,
    overflow,
    prefer_short_inf,
    prefer_int_inf,
    prefer_long_inf,
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
    if (allow_suffix) {
        return switch (std.zig.parseNumberLiteral(bytes)) {
            .int => |i| .{ .long = @intCast(i) },
            else => .{ .failure = .nyi },
        };
    }

    return switch (std.zig.parseNumberLiteral(bytes)) {
        .int => |i| .{ .long = @intCast(i) },
        else => .{ .failure = .nyi },
    };
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
                return .{ .failure = .invalid_character },
            else => return .{ .failure = .invalid_character },
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
                return .{ .failure = .invalid_character },
            else => return .{ .failure = .invalid_character },
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
                return .{ .failure = .invalid_character },
            else => return .{ .failure = .invalid_character },
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
const invalid_character: Result = .{ .failure = .invalid_character };

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

    try testParse("0nh", .none, false, invalid_character);
    try testParse("0Nh", .none, false, invalid_character);
    try testParse("0wh", .none, false, invalid_character);
    try testParse("0Wh", .none, false, invalid_character);
    try testParse("0h", .none, false, invalid_character);
    try testParse("1h", .none, false, invalid_character);
    try testParse("2h", .none, false, invalid_character);
    try testParse("0nh", .none, true, .{ .short = null_short });
    try testParse("0Nh", .none, true, .{ .short = null_short });
    try testParse("0wh", .none, true, .{ .short = inf_short });
    try testParse("0Wh", .none, true, .{ .short = inf_short });
    try testParse("0h", .none, true, .{ .short = 0 });
    try testParse("1h", .none, true, .{ .short = 1 });
    try testParse("2h", .none, true, .{ .short = 2 });

    try testParse("0nh", .short, false, invalid_character);
    try testParse("0Nh", .short, false, invalid_character);
    try testParse("0wh", .short, false, invalid_character);
    try testParse("0Wh", .short, false, invalid_character);
    try testParse("0h", .short, false, invalid_character);
    try testParse("1h", .short, false, invalid_character);
    try testParse("2h", .short, false, invalid_character);
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

    try testParse("0ni", .none, false, invalid_character);
    try testParse("0Ni", .none, false, invalid_character);
    try testParse("0wi", .none, false, invalid_character);
    try testParse("0Wi", .none, false, invalid_character);
    try testParse("0i", .none, false, invalid_character);
    try testParse("1i", .none, false, invalid_character);
    try testParse("2i", .none, false, invalid_character);
    try testParse("0ni", .none, true, .{ .int = null_int });
    try testParse("0Ni", .none, true, .{ .int = null_int });
    try testParse("0wi", .none, true, .{ .int = inf_int });
    try testParse("0Wi", .none, true, .{ .int = inf_int });
    try testParse("0i", .none, true, .{ .int = 0 });
    try testParse("1i", .none, true, .{ .int = 1 });
    try testParse("2i", .none, true, .{ .int = 2 });

    try testParse("0ni", .int, false, invalid_character);
    try testParse("0Ni", .int, false, invalid_character);
    try testParse("0wi", .int, false, invalid_character);
    try testParse("0Wi", .int, false, invalid_character);
    try testParse("0i", .int, false, invalid_character);
    try testParse("1i", .int, false, invalid_character);
    try testParse("2i", .int, false, invalid_character);
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

    try testParse("0nj", .none, false, invalid_character);
    try testParse("0Nj", .none, false, invalid_character);
    try testParse("0wj", .none, false, invalid_character);
    try testParse("0Wj", .none, false, invalid_character);
    try testParse("0j", .none, false, invalid_character);
    try testParse("1j", .none, false, invalid_character);
    try testParse("2j", .none, false, invalid_character);
    try testParse("0nj", .none, true, .{ .long = null_long });
    try testParse("0Nj", .none, true, .{ .long = null_long });
    try testParse("0wj", .none, true, .{ .long = inf_long });
    try testParse("0Wj", .none, true, .{ .long = inf_long });
    try testParse("0j", .none, true, .{ .long = 0 });
    try testParse("1j", .none, true, .{ .long = 1 });
    try testParse("2j", .none, true, .{ .long = 2 });

    try testParse("0nj", .long, false, invalid_character);
    try testParse("0Nj", .long, false, invalid_character);
    try testParse("0wj", .long, false, invalid_character);
    try testParse("0Wj", .long, false, invalid_character);
    try testParse("0j", .long, false, invalid_character);
    try testParse("1j", .long, false, invalid_character);
    try testParse("2j", .long, false, invalid_character);
    try testParse("0nj", .long, true, .{ .long = null_long });
    try testParse("0Nj", .long, true, .{ .long = null_long });
    try testParse("0wj", .long, true, .{ .long = inf_long });
    try testParse("0Wj", .long, true, .{ .long = inf_long });
    try testParse("0j", .long, true, .{ .long = 0 });
    try testParse("1j", .long, true, .{ .long = 1 });
    try testParse("2j", .long, true, .{ .long = 2 });
}
