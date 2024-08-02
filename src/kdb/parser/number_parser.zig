const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("../Ast.zig");
const Parse = @import("../Parse.zig");

pub const null_guid: [16]u8 = .{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

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

pub const null_char = ' ';

pub fn parse(p: *Parse) Parse.Error!Ast.Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    var number_parser = NumberParser.init(p.gpa);
    defer number_parser.deinit();

    while (true) {
        if (p.eob) break;
        if (p.eatToken(.number_literal)) |number_literal| {
            const loc = p.token_locs[number_literal];
            const source = p.source[loc.start..loc.end];
            number_parser.parseNumber(source) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.ShouldBacktrack => {
                    p.tok_i -= 1;
                    p.eob = false;
                    break;
                },
                else => return p.failMsg(.{ .tag = .parse_error, .token = number_literal }),
            };
            try p.scratch.append(p.gpa, number_literal);
            switch (number_parser.result.?.value) {
                .boolean, .boolean_list, .byte, .byte_list => break,
                else => if (number_parser.result.?.has_suffix) {
                    if (p.peekTag() == .number_literal) {
                        return p.fail(.parse_error);
                    }
                } else continue,
            }
        }
        break;
    }

    if (number_parser.toOwnedResult()) |result| {
        const numbers = p.scratch.items[scratch_top..];
        return switch (numbers.len) {
            1 => p.addNode(.{
                .tag = result.tag(),
                .main_token = numbers[0],
                .data = .{
                    .lhs = try p.addValue(result.value),
                    .rhs = undefined,
                },
            }),
            else => p.addNode(.{
                .tag = result.tag(),
                .main_token = numbers[0],
                .data = .{
                    .lhs = try p.addValue(result.value),
                    .rhs = numbers[numbers.len - 1],
                },
            }),
        };
    }

    unreachable;
}

const ValueType = enum {
    boolean,
    boolean_list,
    guid,
    guid_list,
    byte,
    byte_list,
    char,
    char_list,
    short,
    short_list,
    int,
    int_list,
    month,
    month_list,
    date,
    date_list,
    minute,
    minute_list,
    second,
    second_list,
    time,
    time_list,
    long,
    long_list,
    timestamp,
    timestamp_list,
    timespan,
    timespan_list,
    real,
    real_list,
    float,
    float_list,
    datetime,
    datetime_list,
};

pub const Value = union(ValueType) {
    boolean: bool,
    boolean_list: []bool,

    guid: [16]u8,
    guid_list: [][16]u8,

    byte: u8,
    byte_list: []u8,
    char: u8,
    char_list: []u8,

    short: i16,
    short_list: []i16,

    int: i32,
    int_list: []i32,
    month: i32,
    month_list: []i32,
    date: i32,
    date_list: []i32,
    minute: i32,
    minute_list: []i32,
    second: i32,
    second_list: []i32,
    time: i32,
    time_list: []i32,

    long: i64,
    long_list: []i64,
    timestamp: i64,
    timestamp_list: []i64,
    timespan: i64,
    timespan_list: []i64,

    real: f32,
    real_list: []f32,

    float: f64,
    float_list: []f64,
    datetime: f64,
    datetime_list: []f64,

    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .boolean_list => |value| allocator.free(value),
            .guid_list => |value| allocator.free(value),
            .byte_list, .char_list => |value| allocator.free(value),
            .short_list => |value| allocator.free(value),
            .int_list, .month_list, .date_list, .minute_list, .second_list, .time_list => |value| allocator.free(value),
            .long_list, .timestamp_list, .timespan_list => |value| allocator.free(value),
            .real_list => |value| allocator.free(value),
            .float_list, .datetime_list => |value| allocator.free(value),
            else => {},
        }
    }

    pub fn tag(self: Value) Ast.Node.Tag {
        return switch (self) {
            .boolean => .boolean_literal,
            .boolean_list => .boolean_list_literal,
            .guid => .guid_literal,
            .guid_list => .guid_list_literal,
            .byte => .byte_literal,
            .byte_list => .byte_list_literal,
            .char => .char_literal,
            .char_list => .char_list_literal,
            .short => .short_literal,
            .short_list => .short_list_literal,
            .int => .int_literal,
            .int_list => .int_list_literal,
            .month => .month_literal,
            .month_list => .month_list_literal,
            .date => .date_literal,
            .date_list => .date_list_literal,
            .minute => .minute_literal,
            .minute_list => .minute_list_literal,
            .second => .second_literal,
            .second_list => .second_list_literal,
            .time => .time_literal,
            .time_list => .time_list_literal,
            .long => .long_literal,
            .long_list => .long_list_literal,
            .timestamp => .timestamp_literal,
            .timestamp_list => .timestamp_list_literal,
            .timespan => .timespan_literal,
            .timespan_list => .timespan_list_literal,
            .real => .real_literal,
            .real_list => .real_list_literal,
            .float => .float_literal,
            .float_list => .float_list_literal,
            .datetime => .datetime_literal,
            .datetime_list => .datetime_list_literal,
        };
    }

    pub fn isNull(self: Value) bool {
        return switch (self) {
            .guid => |value| std.mem.eql(u8, &value, &std.mem.zeroes([16]u8)),
            .guid_list => |value| {
                for (value) |v| if (!std.mem.eql(u8, &v, &std.mem.zeroes([16]u8))) return false;
                return true;
            },
            .char => |value| value == null_char,
            .char_list => |value| {
                for (value) |v| if (v != null_char) return false;
                return true;
            },
            .short => |value| value == null_short,
            .short_list => |value| {
                for (value) |v| if (v != null_short) return false;
                return true;
            },
            .int, .month, .date, .minute, .second, .time => |value| value == null_int,
            .int_list, .month_list, .date_list, .minute_list, .second_list, .time_list => |value| {
                for (value) |v| if (v != null_int) return false;
                return true;
            },
            .long, .timestamp, .timespan => |value| value == null_long,
            .long_list, .timestamp_list, .timespan_list => |value| {
                for (value) |v| if (v != null_long) return false;
                return true;
            },
            .real => |value| std.math.isNan(value),
            .real_list => |value| {
                for (value) |v| if (!std.math.isNan(v)) return false;
                return true;
            },
            .float, .datetime => |value| std.math.isNan(value),
            .float_list, .datetime_list => |value| {
                for (value) |v| if (!std.math.isNan(v)) return false;
                return true;
            },
            else => false,
        };
    }

    // TODO: Test cases for all types.
    pub fn format(self: Value, comptime _: anytype, _: anytype, writer: std.io.AnyWriter) !void {
        switch (self) {
            .boolean => |value| try writer.writeAll(if (value) "1b" else "0b"),
            .boolean_list => |value| {
                for (value) |v| {
                    try writer.writeByte(if (v) '1' else '0');
                }
                try writer.writeByte('b');
            },
            .guid => |value| {
                if (std.mem.eql(u8, &value, &std.mem.zeroes([16]u8))) {
                    try writer.writeAll("00000000-0000-0000-0000-000000000000");
                } else unreachable;
            },
            .guid_list => |value| {
                for (value[0 .. value.len - 1]) |v| {
                    if (std.mem.eql(u8, &v, &std.mem.zeroes([16]u8))) {
                        try writer.writeAll("00000000-0000-0000-0000-000000000000 ");
                    } else unreachable;
                }
                if (std.mem.eql(u8, &value[value.len - 1], &std.mem.zeroes([16]u8))) {
                    try writer.writeAll("00000000-0000-0000-0000-000000000000");
                } else unreachable;
            },
            .byte => unreachable,
            .byte_list => unreachable,
            .char => unreachable,
            .char_list => unreachable,
            .short => unreachable,
            .short_list => unreachable,
            .int => unreachable,
            .int_list => unreachable,
            .month => unreachable,
            .month_list => unreachable,
            .date => unreachable,
            .date_list => unreachable,
            .minute => unreachable,
            .minute_list => unreachable,
            .second => unreachable,
            .second_list => unreachable,
            .time => unreachable,
            .time_list => unreachable,
            .long => |value| {
                if (value == null_long) {
                    try writer.writeAll("0N");
                } else {
                    try writer.print("{d}", .{value});
                }
            },
            .long_list => |value| {
                var i: usize = 0;
                while (i < value.len - 1) : (i += 1) {
                    const v = value[i];
                    if (v == null_long) {
                        try writer.writeAll("0N ");
                    } else {
                        try writer.print("{d} ", .{v});
                    }
                }
                const v = value[i];
                if (v == null_long) {
                    try writer.writeAll("0N");
                } else {
                    try writer.print("{d}", .{v});
                }
            },
            .timestamp => unreachable,
            .timestamp_list => unreachable,
            .timespan => unreachable,
            .timespan_list => unreachable,
            .real => unreachable,
            .real_list => unreachable,
            .float => |value| {
                if (std.math.isNan(value)) {
                    try writer.writeAll("0n");
                } else {
                    if (@floor(value) == value) {
                        try writer.print("{d}f", .{value});
                    } else {
                        try writer.print("{d}", .{value});
                    }
                }
            },
            .float_list => |value| {
                var requires_suffix = true;
                var i: usize = 0;
                while (i < value.len - 1) : (i += 1) {
                    const v = value[i];
                    if (std.math.isNan(v)) {
                        requires_suffix = false;
                        try writer.writeAll("0n ");
                    } else {
                        requires_suffix = requires_suffix and @floor(v) == v;
                        try writer.print("{d} ", .{v});
                    }
                }
                const v = value[i];
                if (std.math.isNan(v)) {
                    try writer.writeAll("0n");
                } else {
                    if (requires_suffix and @floor(v) == v) {
                        try writer.print("{d}f", .{v});
                    } else {
                        try writer.print("{d}", .{v});
                    }
                }
            },
            .datetime => unreachable,
            .datetime_list => unreachable,
        }
    }
};

const ParseResult = struct {
    value: Value,
    has_suffix: bool,

    pub fn deinit(self: ParseResult, allocator: std.mem.Allocator) void {
        self.value.deinit(allocator);
    }

    pub fn tag(self: ParseResult) Ast.Node.Tag {
        return self.value.tag();
    }
};

const NumberParser = struct {
    allocator: std.mem.Allocator,
    result: ?ParseResult = null,
    str: []const u8 = undefined,

    pub fn init(allocator: std.mem.Allocator) NumberParser {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: NumberParser) void {
        if (self.result) |result| result.deinit(self.allocator);
    }

    pub fn toOwnedResult(self: *NumberParser) ?ParseResult {
        if (self.result) |result| {
            self.result = null;
            return result;
        }
        return null;
    }

    const ParseNumberError = error{
        InvalidCharacter,
        OutOfMemory,
        Overflow,
        ParseError,
        ShouldBacktrack,
    };

    pub fn parseNumber(self: *NumberParser, str: []const u8) ParseNumberError!void {
        self.str = str;

        const value = switch (str[0]) {
            '0' => switch (str.len) {
                1 => Value{ .long = 0 },
                2 => switch (str[1]) {
                    'N' => Value{ .long = null_long },
                    'W' => Value{ .long = inf_long },
                    'n' => Value{ .float = null_float },
                    'w' => Value{ .float = inf_float },
                    'x' => Value{ .byte_list = &[0]u8{} },
                    'b' => Value{ .boolean = false },
                    else => try self.number(1),
                },
                3 => switch (str[1]) {
                    'N', 'n' => try @"null"(str[2]),
                    'W', 'w' => try inf(str[2], .Positive),
                    'x' => try self.byte(),
                    '0', '1' => try self.maybeBoolean(2),
                    else => try self.number(1),
                },
                else => switch (str[1]) {
                    'N', 'n', 'W', 'w' => return error.InvalidCharacter,
                    'x' => try self.byte(),
                    '0', '1' => try self.maybeBoolean(2),
                    else => try self.number(1),
                },
            },
            '1' => try self.maybeBoolean(1),
            '.' => try self.maybeFloat(1),
            '-' => switch (str[1]) {
                '0' => switch (str.len) {
                    2 => Value{ .long = 0 },
                    3 => switch (str[2]) {
                        'W' => Value{ .long = -inf_long },
                        'w' => Value{ .float = -inf_float },
                        else => try self.number(2),
                    },
                    4 => switch (str[2]) {
                        'W', 'w' => try inf(str[3], .Negative),
                        else => try self.number(2),
                    },
                    else => try self.number(2),
                },
                '.' => try self.maybeFloat(2),
                else => try self.number(1),
            },
            else => try self.number(0),
        };
        errdefer value.deinit(self.allocator);

        if (self.result) |prev_result| {
            // TODO: Handle suffix
            if (prev_result.has_suffix) {
                std.debug.panic("parseNumber: has suffix - {s}", .{self.str});
            }

            // TODO: Test cases for null/inf conversions.
            switch (prev_result.value) {
                .boolean, .boolean_list => unreachable,
                .long => |prev_v| {
                    switch (value) {
                        .boolean, .boolean_list => return error.ShouldBacktrack,
                        .guid => |v| {
                            if (!prev_result.value.isNull()) return error.ParseError;
                            const list = try self.allocator.alloc([16]u8, 2);
                            list[0] = std.mem.zeroes([16]u8);
                            list[1] = v;
                            self.result = .{
                                .value = .{ .guid_list = list },
                                .has_suffix = true,
                            };
                        },
                        .long => |v| {
                            const list = try self.allocator.alloc(i64, 2);
                            list[0] = prev_v;
                            list[1] = v;
                            self.result = .{
                                .value = .{ .long_list = list },
                                .has_suffix = false, // TODO: Determine suffix
                            };
                        },
                        .float => |v| {
                            const list = try self.allocator.alloc(f64, 2);
                            list[0] = if (prev_v == null_long) null_float else @floatFromInt(prev_v);
                            list[1] = v;
                            self.result = .{
                                .value = .{ .float_list = list },
                                .has_suffix = false, // TODO: Determine suffix
                            };
                        },
                        else => |t| std.debug.panic("parseNumber: {s} -> {s} ({s})", .{ @tagName(prev_result.value), @tagName(t), self.str }),
                    }
                },
                .long_list => |prev_v| {
                    switch (value) {
                        .boolean, .boolean_list => return error.ShouldBacktrack,
                        .guid => {
                            if (!prev_result.value.isNull()) return error.ParseError;
                            defer prev_result.deinit(self.allocator);
                            const list = try self.allocator.alloc([16]u8, prev_v.len + 1);
                            for (list) |*l| {
                                l.* = std.mem.zeroes([16]u8);
                            }
                            self.result = .{
                                .value = .{ .guid_list = list },
                                .has_suffix = true,
                            };
                        },
                        .long => |v| {
                            defer prev_result.deinit(self.allocator);
                            const list = try self.allocator.alloc(i64, prev_v.len + 1);
                            @memcpy(list[0..prev_v.len], prev_v);
                            list[prev_v.len] = v;
                            self.result = .{
                                .value = .{ .long_list = list },
                                .has_suffix = false, // TODO: Determine suffix
                            };
                        },
                        .float => |v| {
                            defer prev_result.deinit(self.allocator);
                            const list = try self.allocator.alloc(f64, prev_v.len + 1);
                            for (prev_v, 0..) |prev, i| {
                                list[i] = if (prev == null_long) null_float else @floatFromInt(prev);
                            }
                            list[prev_v.len] = v;
                            self.result = .{
                                .value = .{ .float_list = list },
                                .has_suffix = false, // TODO: Determine suffix
                            };
                        },
                        else => |t| std.debug.panic("parseNumber: {s} -> {s} ({s})", .{ @tagName(prev_result.value), @tagName(t), self.str }),
                    }
                },
                .float => |prev_v| {
                    switch (value) {
                        .boolean, .boolean_list => return error.ShouldBacktrack,
                        .guid => |v| {
                            if (!prev_result.value.isNull()) return error.ParseError;
                            const list = try self.allocator.alloc([16]u8, 2);
                            list[0] = std.mem.zeroes([16]u8);
                            list[1] = v;
                            self.result = .{
                                .value = .{ .guid_list = list },
                                .has_suffix = true,
                            };
                        },
                        .long => |v| {
                            const list = try self.allocator.alloc(f64, 2);
                            list[0] = prev_v;
                            list[1] = if (v == null_long) null_float else @floatFromInt(v);
                            self.result = .{
                                .value = .{ .float_list = list },
                                .has_suffix = false, // TODO: Determine suffix
                            };
                        },
                        .float => |v| {
                            const list = try self.allocator.alloc(f64, 2);
                            list[0] = prev_v;
                            list[1] = v;
                            self.result = .{
                                .value = .{ .float_list = list },
                                .has_suffix = false, // TODO: Determine suffix
                            };
                        },
                        else => |t| std.debug.panic("parseNumber: {s} -> {s} ({s})", .{ @tagName(prev_result.value), @tagName(t), self.str }),
                    }
                },
                .float_list => |prev_v| {
                    switch (value) {
                        .boolean, .boolean_list => return error.ShouldBacktrack,
                        .guid => {
                            if (!prev_result.value.isNull()) return error.ParseError;
                            defer prev_result.deinit(self.allocator);
                            const list = try self.allocator.alloc([16]u8, prev_v.len + 1);
                            for (list) |*l| {
                                l.* = std.mem.zeroes([16]u8);
                            }
                            self.result = .{
                                .value = .{ .guid_list = list },
                                .has_suffix = true,
                            };
                        },
                        .long => |v| {
                            defer prev_result.deinit(self.allocator);
                            const list = try self.allocator.alloc(f64, prev_v.len + 1);
                            @memcpy(list[0..prev_v.len], prev_v);
                            list[prev_v.len] = if (v == null_long) null_float else @floatFromInt(v);
                            self.result = .{
                                .value = .{ .float_list = list },
                                .has_suffix = false, // TODO: Determine suffix
                            };
                        },
                        .float => |v| {
                            defer prev_result.deinit(self.allocator);
                            const list = try self.allocator.alloc(f64, prev_v.len + 1);
                            @memcpy(list[0..prev_v.len], prev_v);
                            list[prev_v.len] = v;
                            self.result = .{
                                .value = .{ .float_list = list },
                                .has_suffix = false, // TODO: Determine suffix
                            };
                        },
                        else => |t| std.debug.panic("parseNumber: {s} -> {s} ({s})", .{ @tagName(prev_result.value), @tagName(t), self.str }),
                    }
                },
                else => unreachable,
            }
        } else {
            self.result = .{
                .value = value,
                .has_suffix = false, // TODO: Determine suffix
            };
        }
    }

    fn maybeBoolean(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isBooleanDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return .{ .long = try std.fmt.parseInt(i64, self.str, 10) };

        if (i == self.str.len - 1 and self.str[i] == 'b') {
            if (i == 1) {
                return .{ .boolean = self.str[0] == '1' };
            } else {
                const slice = self.allocator.alloc(bool, i) catch std.debug.panic("Failed to allocate memory.", .{});
                for (self.str[0..i], slice) |c, *v| {
                    v.* = c == '1';
                }
                return .{ .boolean_list = slice };
            }
        }

        return self.number(i);
    }

    fn maybeFloat(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return self.float(i);

        return switch (self.str[i]) {
            'e' => self.real(i),
            'f' => self.float(i),
            'm' => self.month(i),
            '.' => self.maybeDate(i + 1),
            else => error.InvalidCharacter,
        };
    }

    fn maybeDate(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return self.date(i);

        return switch (self.str[i]) {
            'D' => self.maybeTimestampWithDate(i + 1),
            else => error.InvalidCharacter,
        };
    }

    fn maybeTimestampWithDate(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return self.shortTimestampWithDate(i);

        return switch (self.str[i]) {
            '.' => self.shortTimestampWithDate(i),
            ':' => self.maybeTimestamp(i + 1),
            else => error.InvalidCharacter,
        };
    }

    fn maybeTimestamp(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return self.timestamp(i);
        if (self.str[i] == ':') {
            i += 1;
            while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

            if (i == self.str.len) return self.timestamp(i);
        }

        return switch (self.str[i]) {
            '.' => self.timestamp(i),
            else => error.InvalidCharacter,
        };
    }

    fn maybeMinute(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return self.minute(i);

        return switch (self.str[i]) {
            ':' => switch (i == self.str.len - 1) {
                true => self.minute(i),
                false => self.maybeSecond(i + 1),
            },
            else => error.InvalidCharacter,
        };
    }

    fn maybeSecond(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return self.second(i);

        return switch (self.str[i]) {
            '.' => self.time(i),
            else => error.InvalidCharacter,
        };
    }

    fn @"null"(c: u8) !Value {
        return switch (c) {
            'c' => .{ .char = null_char },
            'd' => .{ .date = null_int },
            'e' => .{ .real = null_real },
            'f' => .{ .float = null_float },
            'g' => .{ .guid = null_guid },
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
            else => error.InvalidCharacter,
        };
    }

    fn inf(c: u8, comptime sign: enum { Positive, Negative }) !Value {
        return switch (c) {
            'd' => .{ .date = if (sign == .Positive) inf_int else -inf_int },
            'e' => .{ .real = if (sign == .Positive) inf_real else -inf_real },
            'f' => .{ .float = if (sign == .Positive) inf_float else -inf_float },
            'h' => .{ .short = if (sign == .Positive) inf_short else -inf_short },
            'i' => .{ .int = if (sign == .Positive) inf_int else -inf_int },
            'j' => .{ .long = if (sign == .Positive) inf_long else -inf_long },
            'm' => .{ .month = if (sign == .Positive) inf_int else -inf_int },
            'n' => .{ .timespan = if (sign == .Positive) inf_long else -inf_long },
            'p' => .{ .timestamp = if (sign == .Positive) inf_long else -inf_long },
            't' => .{ .time = if (sign == .Positive) inf_int else -inf_int },
            'u' => .{ .minute = if (sign == .Positive) inf_int else -inf_int },
            'v' => .{ .second = if (sign == .Positive) inf_int else -inf_int },
            'z' => .{ .datetime = if (sign == .Positive) inf_float else -inf_float },
            else => error.InvalidCharacter,
        };
    }

    fn number(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return self.long(i);

        return switch (self.str[i]) {
            'h' => self.short(i),
            'i' => self.int(i),
            'j' => self.long(i),
            'e' => self.real(i),
            'f' => self.float(i),
            'c' => self.char(i),
            'p' => self.shortTimestamp(i),
            'm' => self.month(i),
            'd' => self.shortDate(i),
            'z' => self.shortDatetime(i),
            'D' => self.maybeTimespan(i),
            'n' => self.shortTimespan(i),
            't' => self.shortTime(i),
            'u' => self.shortMinute(i),
            'v' => self.shortSecond(i),
            '.' => self.maybeFloat(i + 1),
            ':' => self.maybeMinute(i + 1),
            else => error.InvalidCharacter,
        };
    }

    fn byte(self: NumberParser) !Value {
        if (self.str.len < 5) {
            const c1 = try std.fmt.charToDigit(self.str[2], 16);
            if (self.str.len == 4) {
                const c2 = try std.fmt.charToDigit(self.str[3], 16);
                return .{ .byte = c1 * 16 + c2 };
            }
            return .{ .byte = c1 };
        }

        var i: usize = 2;
        var list = std.ArrayList(u8).init(self.allocator);
        defer list.deinit();
        while (i < self.str.len) : (i += 2) {
            const c1 = try std.fmt.charToDigit(self.str[i], 16);
            if (i + 1 < self.str.len) {
                const c2 = try std.fmt.charToDigit(self.str[i + 1], 16);
                try list.append(c1 * 16 + c2);
            } else {
                try list.append(c1);
            }
        }

        return .{ .byte_list = try list.toOwnedSlice() };
    }

    fn short(self: NumberParser, index: usize) !Value {
        if (index + 1 < self.str.len) return error.InvalidCharacter;
        if (std.mem.eql(u8, "-32768", self.str[0..index])) return error.Overflow;
        return .{ .short = try std.fmt.parseInt(i16, self.str[0..index], 10) };
    }

    fn int(self: NumberParser, index: usize) !Value {
        if (index + 1 < self.str.len) return error.InvalidCharacter;
        if (std.mem.eql(u8, "-2147483648", self.str[0..index])) return error.Overflow;
        return .{ .int = try std.fmt.parseInt(i32, self.str[0..index], 10) };
    }

    fn long(self: NumberParser, index: usize) !Value {
        if (index + 1 < self.str.len) return error.InvalidCharacter;
        if (std.mem.eql(u8, "-9223372036854775808", self.str[0..index])) return error.Overflow;
        return .{ .long = try std.fmt.parseInt(i64, self.str[0..index], 10) };
    }

    fn real(self: NumberParser, index: usize) !Value {
        if (index + 1 < self.str.len) return error.InvalidCharacter;
        return .{ .real = try std.fmt.parseFloat(f32, self.str[0..index]) };
    }

    fn float(self: NumberParser, index: usize) !Value {
        if (index + 1 < self.str.len) return error.InvalidCharacter;
        return .{ .float = try std.fmt.parseFloat(f64, self.str[0..index]) };
    }

    fn char(self: NumberParser, index: usize) !Value {
        if (index != 1) return error.InvalidCharacter;
        return .{ .char = self.str[0] };
    }

    fn timestamp(self: NumberParser, index: usize) !Value {
        const is_negative = self.str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        const date_value = try parseDate(self.str[start_index..]);
        const timespan_value = try parseTimespan(self.str[start_index + 11 .. index]);
        const nanoseconds_value = if (index < self.str.len - 1 and self.str[index] == '.') value: {
            const len: usize = @min(9, self.str.len - 1 - index);
            const multiplier: i32 = switch (len) {
                1 => 100000000,
                2 => 10000000,
                3 => 1000000,
                4 => 100000,
                5 => 10000,
                6 => 1000,
                7 => 100,
                8 => 10,
                9 => 1,
                else => 0,
            };
            const value = multiplier * try std.fmt.parseInt(i64, self.str[index + 1 .. index + len + 1], 10);
            break :value value + timespan_value;
        } else timespan_value;
        return .{ .timestamp = calculateNanoseconds(.{ .days = date_value, .nanoseconds = if (is_negative) -nanoseconds_value else nanoseconds_value }) };
    }

    fn shortTimestampWithDate(self: NumberParser, index: usize) !Value {
        const is_negative = self.str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        const date_value = try parseDate(self.str[start_index..]);
        const timespan_value = try parseShortTimespan(self.str[start_index + 11 .. index]);
        const nanoseconds_value = if (index < self.str.len - 1) value: {
            const len: usize = @min(9, self.str.len - 1 - index);
            const multiplier: i32 = switch (len) {
                1 => 100000000,
                2 => 10000000,
                3 => 1000000,
                4 => 100000,
                5 => 10000,
                6 => 1000,
                7 => 100,
                8 => 10,
                9 => 1,
                else => 0,
            };
            const value = multiplier * try std.fmt.parseInt(i64, self.str[index + 1 .. index + len + 1], 10);
            break :value value + timespan_value;
        } else timespan_value;
        return .{ .timestamp = calculateNanoseconds(.{ .days = date_value, .nanoseconds = if (is_negative) -nanoseconds_value else nanoseconds_value }) };
    }

    fn shortTimestamp(self: NumberParser, index: usize) !Value {
        return .{ .timestamp = try parseShortTimespan(self.str[0..index]) };
    }

    const epoch_days = -10957;

    const epoch_hours = epoch_days * hours_per_day;
    const hours_per_day = 24;

    const epoch_minutes = epoch_hours * minutes_per_hour;
    const minutes_per_day = minutes_per_hour * hours_per_day;
    const minutes_per_hour = 60;

    const epoch_seconds = epoch_minutes * seconds_per_minute;
    const seconds_per_day = seconds_per_hour * hours_per_day;
    const seconds_per_hour = seconds_per_minute * minutes_per_hour;
    const seconds_per_minute = 60;

    const epoch_milliseconds = epoch_seconds * milliseconds_per_second;
    const milliseconds_per_day = milliseconds_per_hour * hours_per_day;
    const milliseconds_per_hour = milliseconds_per_minute * minutes_per_hour;
    const milliseconds_per_minute = milliseconds_per_second * seconds_per_minute;
    const milliseconds_per_second = 1_000;

    const epoch_microseconds = epoch_milliseconds * microseconds_per_millisecond;
    const microseconds_per_day = microseconds_per_hour * hours_per_day;
    const microseconds_per_hour = microseconds_per_minute * minutes_per_hour;
    const microseconds_per_minute = microseconds_per_second * seconds_per_minute;
    const microseconds_per_second = microseconds_per_millisecond * milliseconds_per_second;
    const microseconds_per_millisecond = 1_000;

    const epoch_nanoseconds = epoch_microseconds * nanoseconds_per_microsecond;
    const nanoseconds_per_day = nanoseconds_per_hour * hours_per_day;
    const nanoseconds_per_hour = nanoseconds_per_minute * minutes_per_hour;
    const nanoseconds_per_minute = nanoseconds_per_second * seconds_per_minute;
    const nanoseconds_per_second = nanoseconds_per_millisecond * milliseconds_per_second;
    const nanoseconds_per_millisecond = nanoseconds_per_microsecond * microseconds_per_millisecond;
    const nanoseconds_per_microsecond = 1_000;

    fn calculateNanoseconds(arg: struct {
        days: ?i64 = null,
        hours: ?i64 = null,
        minutes: ?i64 = null,
        seconds: ?i64 = null,
        nanoseconds: i64 = 0,
    }) i64 {
        var nanoseconds = arg.nanoseconds;
        if (arg.days) |days| nanoseconds += days * nanoseconds_per_day;
        if (arg.hours) |hours| nanoseconds += hours *% nanoseconds_per_hour;
        if (arg.minutes) |minutes| nanoseconds += minutes * nanoseconds_per_minute;
        if (arg.seconds) |seconds| nanoseconds += seconds *% nanoseconds_per_second;
        return nanoseconds;
    }

    fn month(self: NumberParser, index: usize) !Value {
        return switch (index) {
            4 => blk: {
                const month_value = try std.fmt.parseInt(i32, self.str[2..4], 10);
                if (month_value == 0 or month_value > 12) return error.Overflow;

                const year_value = try std.fmt.parseInt(i32, self.str[0..2], 10);
                const year_offset: i32 = if (year_value < 50) 2000 else 1900;
                const months = calculateMonths(year_value + year_offset, month_value);
                break :blk .{ .month = months };
            },
            6 => blk: {
                const month_value = try std.fmt.parseInt(i32, self.str[4..6], 10);
                if (month_value == 0 or month_value > 12) return error.Overflow;

                const year_value = try std.fmt.parseInt(i32, self.str[0..4], 10);
                const months = calculateMonths(year_value, month_value);
                break :blk .{ .month = months };
            },
            7 => blk: {
                if (self.str[4] != '.') return error.InvalidCharacter;

                const month_value = try std.fmt.parseInt(i32, self.str[5..7], 10);
                if (month_value == 0 or month_value > 12) return error.Overflow;

                const year_value = try std.fmt.parseInt(i32, self.str[0..4], 10);
                const months = calculateMonths(year_value, month_value);
                break :blk .{ .month = months };
            },
            8 => blk: {
                if (self.str[0] != '-' or self.str[5] != '.') return error.InvalidCharacter;

                const month_value = try std.fmt.parseInt(i32, self.str[6..8], 10);
                if (month_value == 0 or month_value > 12) return error.Overflow;

                const year_value = try std.fmt.parseInt(i32, self.str[1..5], 10);
                const months = calculateMonths(year_value, month_value);
                break :blk .{ .month = -months };
            },
            else => error.InvalidCharacter,
        };
    }

    fn calculateMonths(year_value: i32, month_value: i32) i32 {
        return (year_value - 2000) * 12 + month_value - 1;
    }

    fn date(self: NumberParser, index: usize) !Value {
        return switch (index) {
            10 => .{ .date = try parseDate(self.str) },
            else => error.InvalidCharacter,
        };
    }

    fn parseDate(str: []const u8) !i32 {
        if (str[4] != '.' or str[7] != '.') return error.InvalidCharacter;

        const year_value = try std.fmt.parseInt(i32, str[0..4], 10);
        if (year_value == 0) return error.Overflow;

        const month_value = try std.fmt.parseInt(i32, str[5..7], 10);
        if (month_value == 0 or month_value > 12) return error.Overflow;

        const day_value = try std.fmt.parseInt(i32, str[8..10], 10);
        if (day_value == 0) return error.Overflow;

        return try calculateDays(year_value, month_value, day_value);
    }

    fn calculateDays(year_value: i32, month_value: i32, day_value: i32) !i32 {
        const is_century_year = @mod(year_value, 100) == 0;
        const leap_year_denominator: i32 = if (is_century_year) 400 else 4;
        const has_leap_day = @mod(year_value, leap_year_denominator) == 0;
        switch (month_value) {
            1, 3, 5, 7, 8, 10, 12 => if (day_value > 31) return error.Overflow,
            2 => switch (has_leap_day) {
                true => if (day_value > 29) return error.Overflow,
                false => if (day_value > 28) return error.Overflow,
            },
            4, 6, 9, 11 => if (day_value > 30) return error.Overflow,
            else => unreachable,
        }

        const completed_years = year_value - 2000;
        var days: i32 = completed_years * 365 + @divFloor(completed_years, 4) - @divFloor(completed_years, 100) + @divFloor(completed_years, 400);
        const feb_days: i32 = if (has_leap_day) 29 else 28;
        days += switch (month_value) {
            1 => 0,
            2 => 31,
            3 => feb_days + 31,
            4 => 31 + feb_days + 31,
            5 => 30 + 31 + feb_days + 31,
            6 => 31 + 30 + 31 + feb_days + 31,
            7 => 30 + 31 + 30 + 31 + feb_days + 31,
            8 => 31 + 30 + 31 + 30 + 31 + feb_days + 31,
            9 => 31 + 31 + 30 + 31 + 30 + 31 + feb_days + 31,
            10 => 30 + 31 + 31 + 30 + 31 + 30 + 31 + feb_days + 31,
            11 => 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31 + feb_days + 31,
            12 => 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31 + feb_days + 31,
            else => unreachable,
        };
        days += day_value;
        if (has_leap_day) days -= 1;

        return days;
    }

    fn shortDate(self: NumberParser, index: usize) !Value {
        return switch (index) {
            6 => blk: {
                const month_value = try std.fmt.parseInt(i32, self.str[2..4], 10);
                if (month_value == 0 or month_value > 12) return error.Overflow;

                const day_value = try std.fmt.parseInt(i32, self.str[4..6], 10);
                if (day_value == 0) return error.Overflow;

                const year_value = try std.fmt.parseInt(i32, self.str[0..2], 10);
                const days = try calculateDays(if (year_value < 50) year_value + 2000 else year_value + 1900, month_value, day_value);
                break :blk .{ .date = days };
            },
            8 => blk: {
                const year_value = try std.fmt.parseInt(i32, self.str[0..4], 10);
                if (year_value == 0) return error.Overflow;

                const month_value = try std.fmt.parseInt(i32, self.str[4..6], 10);
                if (month_value == 0 or month_value > 12) return error.Overflow;

                const day_value = try std.fmt.parseInt(i32, self.str[6..8], 10);
                if (day_value == 0) return error.Overflow;

                const days = try calculateDays(year_value, month_value, day_value);
                break :blk .{ .date = days };
            },
            else => error.InvalidCharacter,
        };
    }

    fn datetime(self: NumberParser, index: usize) !Value {
        _ = index;
        _ = self;
        unreachable;
    }

    fn shortDatetime(self: NumberParser, index: usize) !Value {
        const is_negative = self.str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        return switch (index - start_index) {
            1, 2, 3, 4, 5 => error.InvalidCharacter,
            6 => blk: {
                const month_value = try std.fmt.parseInt(i32, self.str[start_index + 2 .. start_index + 4], 10);
                if (month_value == 0 or month_value > 12) return error.Overflow;

                const day_value = try std.fmt.parseInt(i32, self.str[start_index + 4 .. start_index + 6], 10);
                if (day_value == 0) return error.Overflow;

                const year_value = try std.fmt.parseInt(i32, self.str[start_index .. start_index + 2], 10);
                const days = try calculateDays(if (year_value < 50) year_value + 2000 else year_value + 1900, month_value, day_value);
                break :blk .{ .datetime = @floatFromInt(if (is_negative) -days else days) };
            },
            7 => error.InvalidCharacter,
            8 => blk: {
                const year_value = try std.fmt.parseInt(i32, self.str[start_index .. start_index + 4], 10);
                if (year_value == 0) return error.Overflow;

                const month_value = try std.fmt.parseInt(i32, self.str[start_index + 4 .. start_index + 6], 10);
                if (month_value == 0 or month_value > 12) return error.Overflow;

                const day_value = try std.fmt.parseInt(i32, self.str[start_index + 6 .. start_index + 8], 10);
                if (day_value == 0) return error.Overflow;

                const days = try calculateDays(year_value, month_value, day_value);
                break :blk .{ .datetime = @floatFromInt(if (is_negative) -days else days) };
            },
            else => {
                const seconds_value = try std.fmt.parseFloat(f64, self.str[start_index..index]);

                const days: f64 = epoch_days + seconds_value / seconds_per_day;
                return .{ .datetime = if (is_negative) -days else days };
            },
        };
    }

    fn maybeTimespan(self: NumberParser, index: usize) !Value {
        var i = index + 1;
        while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

        var has_colon = false;

        if (i == self.str.len) return self.shortTimespanWithDays(index, i);
        if (self.str[i] == ':') {
            has_colon = true;
            i += 1;
            while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

            if (i == self.str.len) return self.timespan(index, i);
            if (self.str[i] == ':') {
                i += 1;
                while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

                if (i == self.str.len) return self.timespan(index, i);
            }
        }

        return switch (self.str[i]) {
            '.' => if (has_colon) self.timespan(index, i) else self.shortTimespanWithDays(index, i),
            else => error.InvalidCharacter,
        };
    }

    fn timespan(self: NumberParser, d_index: usize, index: usize) !Value {
        const is_negative = self.str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        const days_value = try std.fmt.parseInt(i32, self.str[start_index..d_index], 10);
        const timespan_value = try parseTimespan(self.str[d_index + 1 .. index]);
        const nanoseconds_value = if (index < self.str.len - 1 and self.str[index] == '.') value: {
            const len: usize = @min(9, self.str.len - 1 - index);
            const multiplier: i32 = switch (len) {
                1 => 100000000,
                2 => 10000000,
                3 => 1000000,
                4 => 100000,
                5 => 10000,
                6 => 1000,
                7 => 100,
                8 => 10,
                9 => 1,
                else => 0,
            };
            const value = multiplier * try std.fmt.parseInt(i64, self.str[index + 1 .. index + len + 1], 10);
            break :value value + timespan_value;
        } else timespan_value;
        const nanoseconds = calculateNanoseconds(.{ .days = days_value, .nanoseconds = nanoseconds_value });
        return .{ .timespan = if (is_negative) -nanoseconds else nanoseconds };
    }

    fn shortTimespanWithDays(self: NumberParser, d_index: usize, index: usize) !Value {
        const is_negative = self.str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        const days_value = try std.fmt.parseInt(i32, self.str[start_index..d_index], 10);
        const timespan_value = try parseShortTimespan(self.str[d_index + 1 .. index]);
        const nanoseconds_value = if (index < self.str.len - 1 and self.str[index] == '.') value: {
            const len: usize = @min(9, self.str.len - 1 - index);
            const multiplier: i32 = switch (len) {
                1 => 100000000,
                2 => 10000000,
                3 => 1000000,
                4 => 100000,
                5 => 10000,
                6 => 1000,
                7 => 100,
                8 => 10,
                9 => 1,
                else => 0,
            };
            const value = multiplier * try std.fmt.parseInt(i64, self.str[index + 1 .. index + len + 1], 10);
            break :value value + timespan_value;
        } else timespan_value;
        const nanoseconds = calculateNanoseconds(.{ .days = days_value, .nanoseconds = nanoseconds_value });
        return .{ .timespan = if (is_negative) -nanoseconds else nanoseconds };
    }

    fn parseTimespan(str: []const u8) !i64 {
        if (str.len == 0) return 0;

        return switch (str.len) {
            3 => blk: {
                if (str[2] != ':') return error.InvalidCharacter;

                const hours_value = try std.fmt.parseInt(i32, str[0..2], 10);
                break :blk calculateNanoseconds(.{ .hours = hours_value });
            },
            5 => blk: {
                if (str[2] != ':') return error.InvalidCharacter;

                const minutes_value = try std.fmt.parseInt(i32, str[3..5], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[0..2], 10);
                break :blk calculateNanoseconds(.{ .hours = hours_value, .minutes = minutes_value });
            },
            8 => blk: {
                if (str[2] != ':' or str[5] != ':') return error.InvalidCharacter;

                const minutes_value = try std.fmt.parseInt(i32, str[6..8], 10);
                if (minutes_value > 59) return error.Overflow;

                const seconds_value = try std.fmt.parseInt(i32, str[3..5], 10);
                if (seconds_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[0..2], 10);
                break :blk calculateNanoseconds(.{ .hours = hours_value, .minutes = minutes_value, .seconds = seconds_value });
            },
            else => error.InvalidCharacter,
        };
    }

    fn shortTimespan(self: NumberParser, index: usize) !Value {
        return .{ .timespan = try parseShortTimespan(self.str[0..index]) };
    }

    fn parseShortTimespan(str: []const u8) !i64 {
        if (str.len == 0) return 0;

        const is_negative = str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        return switch (str.len - start_index) {
            1, 2 => blk: {
                const hours_value = try std.fmt.parseInt(i32, str[start_index..], 10);
                const nanoseconds = calculateNanoseconds(.{ .hours = hours_value });
                break :blk if (is_negative) -nanoseconds else nanoseconds;
            },
            3, 4, 5 => blk: {
                const minutes_value = try std.fmt.parseInt(i64, str[str.len - 2 ..], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i64, str[start_index .. str.len - 2], 10);
                const nanoseconds = calculateNanoseconds(.{ .hours = hours_value, .minutes = minutes_value });
                break :blk if (is_negative) -nanoseconds else nanoseconds;
            },
            6, 7, 8 => blk: {
                const seconds_value = try std.fmt.parseInt(i64, str[str.len - 2 ..], 10);
                if (seconds_value > 59) return error.Overflow;

                const minutes_value = try std.fmt.parseInt(i64, str[str.len - 4 .. str.len - 2], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i64, str[start_index .. str.len - 4], 10);
                const nanoseconds = calculateNanoseconds(.{ .hours = hours_value, .minutes = minutes_value, .seconds = seconds_value });
                break :blk if (is_negative) -nanoseconds else nanoseconds;
            },
            15 => blk: {
                const seconds_value = try std.fmt.parseInt(i64, str[str.len - 11 .. str.len - 9], 10);
                if (seconds_value > 59) return error.Overflow;

                const minutes_value = try std.fmt.parseInt(i64, str[str.len - 13 .. str.len - 11], 10);
                if (minutes_value > 59) return error.Overflow;

                const nanoseconds_value = try std.fmt.parseInt(i64, str[str.len - 9 ..], 10);
                const hours_value = try std.fmt.parseInt(i64, str[start_index .. str.len - 13], 10);
                const nanoseconds = calculateNanoseconds(.{ .hours = hours_value, .minutes = minutes_value, .seconds = seconds_value, .nanoseconds = nanoseconds_value });
                break :blk if (is_negative) -nanoseconds else nanoseconds;
            },
            else => blk: {
                const seconds_value = try std.fmt.parseInt(i64, str[start_index..], 10);
                const nanoseconds = epoch_nanoseconds +% calculateNanoseconds(.{ .seconds = seconds_value });
                break :blk if (is_negative) -nanoseconds else nanoseconds;
            },
        };
    }

    fn minute(self: NumberParser, index: usize) !Value {
        return .{ .minute = try parseMinute(self.str[0..index]) };
    }

    fn parseMinute(str: []const u8) !i32 {
        const is_negative = str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        return switch (str.len - start_index) {
            3, 4 => blk: {
                if (str[str.len - 1] != ':') return error.InvalidCharacter;

                const minutes_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 1], 10);
                const seconds = minutes_value * 60;
                break :blk if (is_negative) -seconds else seconds;
            },
            5, 6 => switch (str[str.len - 1] == ':') {
                true => blk: {
                    const minutes_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 1], 10);
                    const seconds = minutes_value * 60;
                    break :blk if (is_negative) -seconds else seconds;
                },
                false => blk: {
                    if (str[str.len - 3] != ':') return error.InvalidCharacter;

                    const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                    if (seconds_value > 59) return error.Overflow;

                    const minutes_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 3], 10);
                    const seconds = seconds_value + minutes_value * 60;
                    break :blk if (is_negative) -seconds else seconds;
                },
            },
            else => error.InvalidCharacter,
        };
    }

    fn shortMinute(self: NumberParser, index: usize) !Value {
        return .{ .minute = try parseShortMinute(self.str[0..index]) };
    }

    fn parseShortMinute(str: []const u8) !i32 {
        const is_negative = str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        return switch (str.len - start_index) {
            1, 2 => blk: {
                const minutes_value = try std.fmt.parseInt(i32, str[start_index..], 10);
                const seconds = minutes_value * 60;
                break :blk if (is_negative) -seconds else seconds;
            },
            else => blk: {
                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                if (seconds_value > 59) return error.Overflow;

                const minutes_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 2], 10);
                const seconds = seconds_value + minutes_value *% 60;
                break :blk if (is_negative) -seconds else seconds;
            },
        };
    }

    fn second(self: NumberParser, index: usize) !Value {
        return .{ .second = try parseSecond(self.str[0..index]) };
    }

    fn parseSecond(str: []const u8) !i32 {
        const is_negative = str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        return switch (str.len - start_index) {
            8, 9 => blk: {
                if (str[str.len - 6] != ':' or str[str.len - 3] != ':') return error.InvalidCharacter;

                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 5 .. str.len - 3], 10);
                if (minutes_value > 59) return error.Overflow;

                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 .. str.len], 10);
                if (seconds_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 6], 10);
                const seconds = hours_value * 3600 + minutes_value * 60 + seconds_value;
                break :blk if (is_negative) -seconds else seconds;
            },
            else => error.InvalidCharacter,
        };
    }

    fn shortSecond(self: NumberParser, index: usize) !Value {
        return .{ .second = try parseShortSecond(self.str[0..index]) };
    }

    fn parseShortSecond(str: []const u8) !i32 {
        const is_negative = str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        return switch (str.len - start_index) {
            1, 2 => blk: {
                const hours_value = try std.fmt.parseInt(i32, str[start_index..str.len], 10);
                const seconds = hours_value * 3600;
                break :blk if (is_negative) -seconds else seconds;
            },
            3, 4, 5 => blk: {
                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 2], 10);
                const seconds = hours_value * 3600 + minutes_value * 60;
                break :blk if (is_negative) -seconds else seconds;
            },
            else => blk: {
                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                if (seconds_value > 59) return error.Overflow;
                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 4 .. str.len - 2], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 4], 10);
                const seconds = hours_value *% 3600 + minutes_value * 60 + seconds_value;
                break :blk if (is_negative) -seconds else seconds;
            },
        };
    }

    fn time(self: NumberParser, index: usize) !Value {
        const is_negative = self.str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        const time_value = try parseTime(self.str[start_index..index]);
        const milliseconds = if (index < self.str.len - 1 and self.str[index] == '.') value: {
            const len: usize = @min(3, self.str.len - 1 - index);
            const multiplier: i32 = switch (len) {
                1 => 100,
                2 => 10,
                3 => 1,
                else => 0,
            };
            const value = multiplier * try std.fmt.parseInt(i32, self.str[index + 1 .. index + len + 1], 10);
            break :value value + time_value;
        } else time_value;
        return .{ .time = if (is_negative) -milliseconds else milliseconds };
    }

    fn parseTime(str: []const u8) !i32 {
        const is_negative = str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        return switch (str.len - start_index) {
            8, 9 => blk: {
                if (str[str.len - 6] != ':' or str[str.len - 3] != ':') return error.InvalidCharacter;

                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                if (seconds_value > 59) return error.Overflow;

                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 5 .. str.len - 3], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 6], 10);
                const milliseconds = hours_value *% 3600000 + minutes_value * 60000 + seconds_value * 1000;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
            10 => blk: {
                if (str[start_index + 2] != ':' or str[start_index + 5] != ':' or str[start_index + 8] != '.') return error.InvalidCharacter;

                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 3 .. str.len - 1], 10);
                if (seconds_value > 59) return error.Overflow;

                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 6 .. str.len - 4], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 7], 10);
                const milliseconds = hours_value * 3600000 + minutes_value * 60000 + seconds_value * 1000;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
            else => error.InvalidCharacter,
        };
    }

    fn shortTime(self: NumberParser, index: usize) !Value {
        return .{ .time = try parseShortTime(self.str[0..index]) };
    }

    fn parseShortTime(str: []const u8) !i32 {
        const is_negative = str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        return switch (str.len - start_index) {
            1, 2 => blk: {
                const hours_value = try std.fmt.parseInt(i32, str[start_index..], 10);
                const milliseconds = hours_value * 3600000;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
            3, 4, 5 => blk: {
                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 2], 10);
                const milliseconds = hours_value *% 3600000 + minutes_value * 60000;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
            9 => blk: {
                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 5 .. str.len - 3], 10);
                if (seconds_value > 59) return error.Overflow;

                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 7 .. str.len - 5], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 7], 10);
                const milliseconds_value = try std.fmt.parseInt(i32, str[str.len - 3 ..], 10);
                const milliseconds = hours_value *% 3600000 + minutes_value * 60000 + seconds_value * 1000 + milliseconds_value;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
            else => blk: {
                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                if (seconds_value > 59) return error.Overflow;

                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 4 .. str.len - 2], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 4], 10);
                const milliseconds = hours_value *% 3600000 + minutes_value * 60000 + seconds_value * 1000;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
        };
    }

    fn isBooleanDigit(c: u8) bool {
        return switch (c) {
            '0', '1' => true,
            else => false,
        };
    }

    fn isDigit(c: u8) bool {
        return switch (c) {
            '0'...'9' => true,
            else => false,
        };
    }

    fn isTimespanDigit(c: u8) bool {
        return switch (c) {
            '0'...'9', ':' => true,
            else => false,
        };
    }
};

// Test cases:
// 01b 0 1 // valid - 01b@0 1
// 0x0001 0 1 // valid - 0x0001@0 1
// 0 1 2 3j 0 1 // invalid
// 0 1 010b // valid - 0 1@010b

pub fn testNumberParser(input: []const u8, comptime expected_type: ValueType, comptime expected_value: anytype) !void {
    var number_parser = NumberParser.init(std.testing.allocator);
    defer number_parser.deinit();

    try number_parser.parseNumber(input);

    try std.testing.expect(number_parser.result != null);
    const value = number_parser.result.?.value;
    try std.testing.expectEqual(expected_type, @as(ValueType, value));

    const actual_value = @field(value, @tagName(expected_type));
    const actual = if (@typeInfo(@TypeOf(actual_value)) == .Array) &actual_value else actual_value;
    switch (@typeInfo(@TypeOf(expected_value))) {
        .Array => |type_info| {
            try std.testing.expectEqualSlices(type_info.child, &expected_value, actual);
        },
        .Pointer => |type_info| {
            try std.testing.expectEqualSlices(@typeInfo(type_info.child).Array.child, expected_value, actual);
        },
        .Struct => |type_info| {
            const fields_info = type_info.fields;
            const T = if (fields_info.len > 0) fields_info[0].type else @typeInfo(@TypeOf(actual)).Pointer.child;
            const expected = try std.testing.allocator.alloc(T, fields_info.len);
            defer std.testing.allocator.free(expected);
            inline for (fields_info, 0..) |field, i| {
                expected[i] = @field(expected_value, field.name);
            }
            try std.testing.expectEqualSlices(T, expected, actual);
        },
        else => {
            switch (@TypeOf(expected_value)) {
                f32, f64 => |T| {
                    if (!std.math.isNan(expected_value) or !std.math.isNan(actual)) {
                        if (std.math.isInf(expected_value) or std.math.isInf(actual)) {
                            try std.testing.expectEqual(expected_value, actual);
                        } else if (!std.math.approxEqRel(T, expected_value, actual, std.math.sqrt(std.math.floatEps(T))) and
                            !std.math.approxEqAbs(T, expected_value, actual, std.math.floatEps(T)))
                        {
                            std.debug.print("actual {}, not within absolute or relative tolerance of expected {}\n", .{ actual, expected_value });
                            return error.TestExpectedApproxEq;
                        }
                    }
                },
                else => try std.testing.expectEqual(expected_value, actual),
            }
        },
    }
}

pub fn testNumberParserError(input: []const u8, expected: anyerror) !void {
    var number_parser = NumberParser.init(std.testing.allocator);
    defer number_parser.deinit();

    number_parser.parseNumber(input) catch |e| {
        try std.testing.expectEqual(expected, e);
        return;
    };

    return error.UnexpectedResult;
}

test {
    _ = @import("test/boolean.zig");
    _ = @import("test/byte.zig");
    _ = @import("test/char.zig");
    _ = @import("test/date.zig");
    _ = @import("test/datetime.zig");
    _ = @import("test/float.zig");
    _ = @import("test/guid.zig");
    _ = @import("test/int.zig");
    _ = @import("test/long.zig");
    _ = @import("test/minute.zig");
    _ = @import("test/real.zig");
    _ = @import("test/second.zig");
    _ = @import("test/short.zig");
    _ = @import("test/time.zig");
    _ = @import("test/timespan.zig");
    _ = @import("test/timestamp.zig");
}

test "edge cases" {
    if (true) return error.SkipZigTest;

    try testNumberParser("99:59:59.", .second, @as(i32, 359999));
}
