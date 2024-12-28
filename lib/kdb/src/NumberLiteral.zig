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

pub const Error = union(enum) {
    nyi,
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

pub fn parse(bytes: []const u8, type_hint: TypeHint) Result {
    switch (type_hint) {
        .none => {},
        else => return .{ .failure = .nyi },
    }
    const result = std.zig.parseNumberLiteral(bytes);
    switch (result) {
        .int => |v| return .{ .long = @intCast(v) },
        else => return .{ .failure = .nyi },
    }
}

test "longs" {
    // parse("0 1 2j", .long);
    // parse("0 1 2", .none);
}
