const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;

const kdb = @import("../../kdb.zig");
const Ast = kdb.Ast;
const Token = Ast.Token;
const Tokenizer = kdb.Tokenizer;
const Parse = @import("../Parse.zig");
const utils = @import("utils.zig");

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

pub const epoch_days = -10957;

pub const epoch_hours = epoch_days * hours_per_day;
pub const hours_per_day = 24;

pub const epoch_minutes = epoch_hours * minutes_per_hour;
pub const minutes_per_day = minutes_per_hour * hours_per_day;
pub const minutes_per_hour = 60;

pub const epoch_seconds = epoch_minutes * seconds_per_minute;
pub const seconds_per_day = seconds_per_hour * hours_per_day;
pub const seconds_per_hour = seconds_per_minute * minutes_per_hour;
pub const seconds_per_minute = 60;

pub const epoch_milliseconds = epoch_seconds * milliseconds_per_second;
pub const milliseconds_per_day = milliseconds_per_hour * hours_per_day;
pub const milliseconds_per_hour = milliseconds_per_minute * minutes_per_hour;
pub const milliseconds_per_minute = milliseconds_per_second * seconds_per_minute;
pub const milliseconds_per_second = 1_000;

pub const epoch_microseconds = epoch_milliseconds * microseconds_per_millisecond;
pub const microseconds_per_day = microseconds_per_hour * hours_per_day;
pub const microseconds_per_hour = microseconds_per_minute * minutes_per_hour;
pub const microseconds_per_minute = microseconds_per_second * seconds_per_minute;
pub const microseconds_per_second = microseconds_per_millisecond * milliseconds_per_second;
pub const microseconds_per_millisecond = 1_000;

pub const epoch_nanoseconds = epoch_microseconds * nanoseconds_per_microsecond;
pub const nanoseconds_per_day = nanoseconds_per_hour * hours_per_day;
pub const nanoseconds_per_hour = nanoseconds_per_minute * minutes_per_hour;
pub const nanoseconds_per_minute = nanoseconds_per_second * seconds_per_minute;
pub const nanoseconds_per_second = nanoseconds_per_millisecond * milliseconds_per_second;
pub const nanoseconds_per_millisecond = nanoseconds_per_microsecond * microseconds_per_millisecond;
pub const nanoseconds_per_microsecond = 1_000;

pub fn parse(p: *Parse) Parse.Error!Ast.Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    var value_type: ?ValueType = null;
    while (true) {
        if (p.eob) break;
        if (p.eatToken(.number_literal)) |number_literal| {
            try p.scratch.append(p.gpa, number_literal);

            const loc = p.token_locs[number_literal];
            const source = p.source[loc.start..loc.end];

            // 0x...
            if (source.len > 1 and source[0] == '0' and source[1] == 'x') {
                value_type = .byte;
                break;
            }

            // 2000.01.01
            if (source.len == 10 and source[4] == '.' and source[7] == '.') {
                if (value_type orelse .date != .date) return p.failMsg(.{ .tag = .parse_error, .token = number_literal });
                value_type = .date;
            }
            // 12:34, -12:34, 123:45
            else if ((source.len == 5 or source.len == 6) and source[source.len - 3] == ':') {
                if (value_type orelse .minute != .minute) return p.failMsg(.{ .tag = .parse_error, .token = number_literal });
                value_type = .minute;
            }
            // 12:34:45, -12:34:45, 123:45:67
            else if ((source.len == 8 or source.len == 9) and source[source.len - 6] == ':' and source[source.len - 3] == ':') {
                if (value_type orelse .second != .second) return p.failMsg(.{ .tag = .parse_error, .token = number_literal });
                value_type = .second;
            } else if (switch (source.len) {
                // 12:34:56.7
                10 => source[source.len - 8] == ':' and source[source.len - 5] == ':' and source[source.len - 2] == '.',
                // -12:34:56.7, 123:34:56.7, 12:34:56.78
                11 => (source[source.len - 8] == ':' and source[source.len - 5] == ':' and source[source.len - 2] == '.') or
                    (source[source.len - 9] == ':' and source[source.len - 6] == ':' and source[source.len - 3] == '.'),
                // -12:34:56.78, 123:34:56.78, 12:34:56.789
                12 => (source[source.len - 9] == ':' and source[source.len - 6] == ':' and source[source.len - 3] == '.') or
                    (source[source.len - 10] == ':' and source[source.len - 7] == ':' and source[source.len - 4] == '.'),
                // 123:34:56.789
                13 => source[source.len - 10] == ':' and source[source.len - 7] == ':' and source[source.len - 4] == '.',
                else => false,
            }) {
                if (value_type orelse .time != .time) return p.failMsg(.{ .tag = .parse_error, .token = number_literal });
                value_type = .time;
            }
            switch (source[source.len - 1]) {
                inline 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'm', 'p', 't', 'u', 'v', 'z' => |suffix| {
                    value_type = ValueType.fromSuffix(suffix);
                    break;
                },
                'n' => if (source.len != 2 or source[0] != '0') {
                    value_type = .timespan;
                    break;
                },
                else => {},
            }
            continue;
        }
        break;
    }

    if (value_type) |vt| switch (vt) {
        .boolean, .byte => {},
        else => if (p.peekTag() == .number_literal) {
            const loc = p.token_locs[p.tok_i];
            const source = p.source[loc.start..loc.end];
            if ((source[source.len - 1] != 'b') and (source.len <= 1 or source[1] != 'x')) return p.fail(.parse_error);
        },
    };

    var tokens = p.scratch.items[scratch_top..];
    const value = parseTokens(p, tokens, value_type) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ShouldBacktrack => blk: {
            p.tok_i -= 1;
            p.eob = false;

            tokens = tokens[0 .. tokens.len - 1];
            break :blk parseTokens(p, tokens, null) catch |e| switch (e) {
                error.OutOfMemory => return error.OutOfMemory,
                error.ShouldBacktrack => unreachable,
                else => return p.failMsg(.{ .tag = .parse_error, .token = tokens[0] }),
            };
        },
        else => return p.failMsg(.{ .tag = .parse_error, .token = tokens[0] }),
    };
    errdefer value.deinit(p.gpa);

    return switch (tokens.len) {
        1 => p.addNode(.{
            .tag = value.tag(),
            .main_token = tokens[0],
            .data = .{
                .lhs = try p.addValue(value),
                .rhs = undefined,
            },
        }),
        else => p.addNode(.{
            .tag = value.tag(),
            .main_token = tokens[0],
            .data = .{
                .lhs = try p.addValue(value),
                .rhs = tokens[tokens.len - 1],
            },
        }),
    };
}

fn parseTokens(p: *Parse, tokens: []const Ast.Node.Index, value_type: ?ValueType) NumberParser.ParseNumberError!Value {
    if (value_type) |vt| {
        return switch (tokens.len) {
            1 => switch (vt) {
                inline .boolean, .byte, .guid, .char, .short, .int, .month, .date, .minute, .second, .time, .long, .timestamp, .timespan, .real, .float, .datetime => |v| parseAtomType(p, tokens[0], v),
                else => panic("Unsupported type: {s}", .{@tagName(vt)}),
            },
            else => switch (vt) {
                .boolean, .byte => error.ShouldBacktrack,
                inline .guid, .char, .short, .int, .month, .date, .minute, .second, .time, .long, .timestamp, .timespan, .real, .float, .datetime => |v| parseListType(p, tokens, v),
                else => panic("Unsupported type: {s}", .{@tagName(vt)}),
            },
        };
    } else {
        return switch (tokens.len) {
            1 => parseAtom(p, tokens[0]),
            else => parseList(p, tokens),
        };
    }
}

fn parseAtom(p: *Parse, token: Ast.Node.Index) NumberParser.ParseNumberError!Value {
    var number_parser = NumberParser.init(p.gpa);

    const loc = p.token_locs[token];
    const source = p.source[loc.start..loc.end];
    return number_parser.parseNumber(source);
}

fn parseList(p: *Parse, tokens: []const Ast.Node.Index) NumberParser.ParseNumberError!Value {
    const value_list = try p.gpa.alloc(Value, tokens.len);
    defer p.gpa.free(value_list);

    var value_type: ValueType = .long;
    var index: usize = 0;
    for (tokens) |token| {
        defer index += 1;
        var number_parser = NumberParser.init(p.gpa);

        const loc = p.token_locs[token];
        const source = p.source[loc.start..loc.end];
        const value = try number_parser.parseNumber(source);

        value_list[index] = value;
        if (value == .float) {
            value_type = .float;
            break;
        }
    }

    if (value_type == .long) {
        const list = try p.gpa.alloc(i64, tokens.len);
        errdefer p.gpa.free(list);

        for (value_list[0..index], 0..) |value, i| {
            list[i] = utils.castValue(i64, value);
        }

        return .{ .long_list = list };
    } else {
        const list = try p.gpa.alloc(f64, tokens.len);
        errdefer p.gpa.free(list);

        for (value_list[0..index], 0..) |value, i| {
            list[i] = utils.castValue(f64, value);
        }

        for (tokens[index..], index..) |token, i| {
            var number_parser = NumberParser.init(p.gpa);

            const loc = p.token_locs[token];
            const source = p.source[loc.start..loc.end];
            const value = try number_parser.parseNumber(source);

            list[i] = utils.castValue(f64, value);
        }

        return .{ .float_list = list };
    }
}

fn parseAtomType(p: *Parse, token: Ast.Node.Index, comptime value_type: ValueType) NumberParser.ParseNumberError!Value {
    var number_parser = NumberParser.init(p.gpa);

    const loc = p.token_locs[token];
    const source = p.source[loc.start..loc.end];
    return number_parser.parseNumberType(value_type, source);
}

fn parseListType(p: *Parse, tokens: []const Ast.Node.Index, comptime value_type: ValueType) NumberParser.ParseNumberError!Value {
    const T = value_type.toType();
    const list = try p.gpa.alloc(T, tokens.len);
    errdefer p.gpa.free(list);

    for (tokens, 0..) |token, i| {
        var number_parser = NumberParser.init(p.gpa);

        const loc = p.token_locs[token];
        const source = p.source[loc.start..loc.end];
        const value = try number_parser.parseNumberType(value_type, source);
        list[i] = utils.castValue(T, value);
    }

    return @unionInit(Value, @tagName(value_type) ++ "_list", list);
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

    pub fn toType(comptime value_type: ValueType) type {
        return switch (value_type) {
            .guid => [16]u8,
            .char => u8,
            .short => i16,
            .int, .month, .date, .minute, .second, .time => i32,
            .long, .timestamp, .timespan => i64,
            .real => f32,
            .float, .datetime => f64,
            else => @compileError("Unsupported type: " ++ @tagName(value_type)),
        };
    }

    pub fn toSuffix(comptime value_type: ValueType) u8 {
        return switch (value_type) {
            .boolean => 'b',
            .guid => 'g',
            .char => 'c',
            .short => 'h',
            .int => 'i',
            .month => 'm',
            .date => 'd',
            .minute => 'u',
            .second => 'v',
            .time => 't',
            .long => 'j',
            .timestamp => 'p',
            .timespan => 'n',
            .real => 'e',
            .float => 'f',
            .datetime => 'z',
            else => @compileError("Unsupported type: " ++ @tagName(value_type)),
        };
    }

    pub fn fromSuffix(comptime suffix: u8) ValueType {
        return switch (suffix) {
            'b' => .boolean,
            'c' => .char,
            'd' => .date,
            'e' => .real,
            'f' => .float,
            'g' => .guid,
            'h' => .short,
            'i' => .int,
            'j' => .long,
            'm' => .month,
            'p' => .timestamp,
            't' => .time,
            'u' => .minute,
            'v' => .second,
            'z' => .datetime,
            else => @compileError("Unsupported suffix: " ++ .{suffix}),
        };
    }
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

    pub fn deinit(self: Value, gpa: Allocator) void {
        switch (self) {
            .boolean_list => |value| gpa.free(value),
            .guid_list => |value| gpa.free(value),
            .byte_list, .char_list => |value| gpa.free(value),
            .short_list => |value| gpa.free(value),
            .int_list, .month_list, .date_list, .minute_list, .second_list, .time_list => |value| gpa.free(value),
            .long_list, .timestamp_list, .timespan_list => |value| gpa.free(value),
            .real_list => |value| gpa.free(value),
            .float_list, .datetime_list => |value| gpa.free(value),
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
            .char => .char_number_literal,
            .char_list => .char_number_list_literal,
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

    // pub fn format(self: Value, comptime _: anytype, _: anytype, writer: Io.AnyWriter) !void {
    //     switch (self) {
    //         .boolean => |value| try writer.writeAll(if (value) "1b" else "0b"),
    //         .boolean_list => |values| {
    //             for (values) |value| {
    //                 try writer.writeByte(if (value) '1' else '0');
    //             }
    //             try writer.writeByte('b');
    //         },
    //         .guid => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("00000000-0000-0000-0000-000000000000");
    //             } else unreachable;
    //         },
    //         .guid_list => |values| {
    //             for (values[0 .. values.len - 1]) |value| {
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("00000000-0000-0000-0000-000000000000 ");
    //                 } else unreachable;
    //             }
    //             const value = values[values.len - 1];
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("00000000-0000-0000-0000-000000000000");
    //             } else unreachable;
    //         },
    //         .byte => |value| try writer.print("0x{x:0>2}", .{value}),
    //         .byte_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`byte$()");
    //             } else {
    //                 try writer.writeAll("0x");
    //                 for (values) |value| try writer.print("{x:0>2}", .{value});
    //             }
    //         },
    //         .char => |value| try writer.print("\"{c}\"", .{value}),
    //         .char_list => |values| try writer.print("\"{s}\"", .{values}),
    //         .short => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Nh");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0Wh");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0Wh");
    //             } else {
    //                 try writer.print("{d}h", .{value});
    //             }
    //         },
    //         .short_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`short$()");
    //             } else {
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         try writer.writeAll("0N ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         try writer.writeAll("0W ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         try writer.writeAll("-0W ");
    //                     } else {
    //                         try writer.print("{d} ", .{value});
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0Nh");
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0Wh");
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0Wh");
    //                 } else {
    //                     try writer.print("{d}h", .{value});
    //                 }
    //             }
    //         },
    //         .int => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Ni");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0Wi");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0Wi");
    //             } else {
    //                 try writer.print("{d}i", .{value});
    //             }
    //         },
    //         .int_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`int$()");
    //             } else {
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         try writer.writeAll("0N ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         try writer.writeAll("0W ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         try writer.writeAll("-0W ");
    //                     } else {
    //                         try writer.print("{d} ", .{value});
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0Ni");
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0Wi");
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0Wi");
    //                 } else {
    //                     try writer.print("{d}i", .{value});
    //                 }
    //             }
    //         },
    //         .month => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Nm");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0Wm");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0Wm");
    //             } else {
    //                 const year, const month = utils.monthsToYearMonth(value);
    //                 try writer.print("{d:0>4}.{d:0>2}m", .{ year, month });
    //             }
    //         },
    //         .month_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`month$()");
    //             } else {
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         try writer.writeAll("0N ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         try writer.writeAll("0W ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         try writer.writeAll("-0W ");
    //                     } else {
    //                         const year, const month = utils.monthsToYearMonth(value);
    //                         try writer.print("{d:0>4}.{d:0>2} ", .{ year, month });
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0Nm");
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0Wm");
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0Wm");
    //                 } else {
    //                     const year, const month = utils.monthsToYearMonth(value);
    //                     try writer.print("{d:0>4}.{d:0>2}m", .{ year, month });
    //                 }
    //             }
    //         },
    //         .date => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Nd");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0Wd");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0Wd");
    //             } else {
    //                 const year, const month, const day = utils.daysToYearMonthDay(value);
    //                 try writer.print("{d:0>4}.{d:0>2}.{d:0>2}", .{ year, month, day });
    //             }
    //         },
    //         .date_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`date$()");
    //             } else {
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         try writer.writeAll("0N ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         try writer.writeAll("0W ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         try writer.writeAll("-0W ");
    //                     } else {
    //                         const year, const month, const day = utils.daysToYearMonthDay(value);
    //                         try writer.print("{d:0>4}.{d:0>2}.{d:0>2} ", .{ year, month, day });
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0Nd");
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0Wd");
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0Wd");
    //                 } else {
    //                     const year, const month, const day = utils.daysToYearMonthDay(value);
    //                     try writer.print("{d:0>4}.{d:0>2}.{d:0>2}", .{ year, month, day });
    //                 }
    //             }
    //         },
    //         .minute => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Nu");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0Wu");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0Wu");
    //             } else {
    //                 const hour, const minute = utils.minutesToHourMinute(value);
    //                 if (hour > 99) {
    //                     if (value < 0) {
    //                         try writer.print("-**:{d:0>2}", .{minute});
    //                     } else {
    //                         try writer.print("**:{d:0>2}", .{minute});
    //                     }
    //                 } else {
    //                     if (value < 0) {
    //                         try writer.print("-{d:0>2}:{d:0>2}", .{ hour, minute });
    //                     } else {
    //                         try writer.print("{d:0>2}:{d:0>2}", .{ hour, minute });
    //                     }
    //                 }
    //             }
    //         },
    //         .minute_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`minute$()");
    //             } else {
    //                 var requires_suffix = true;
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         try writer.writeAll("0N ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         try writer.writeAll("0W ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         try writer.writeAll("-0W ");
    //                     } else {
    //                         requires_suffix = false;
    //                         const hour, const minute = utils.minutesToHourMinute(value);
    //                         if (hour > 99) {
    //                             if (value < 0) {
    //                                 try writer.print("-**:{d:0>2} ", .{minute});
    //                             } else {
    //                                 try writer.print("**:{d:0>2} ", .{minute});
    //                             }
    //                         } else {
    //                             if (value < 0) {
    //                                 try writer.print("-{d:0>2}:{d:0>2} ", .{ hour, minute });
    //                             } else {
    //                                 try writer.print("{d:0>2}:{d:0>2} ", .{ hour, minute });
    //                             }
    //                         }
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0N");
    //                     if (requires_suffix) try writer.writeByte('u');
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0W");
    //                     if (requires_suffix) try writer.writeByte('u');
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0W");
    //                     if (requires_suffix) try writer.writeByte('u');
    //                 } else {
    //                     const hour, const minute = utils.minutesToHourMinute(value);
    //                     if (hour > 99) {
    //                         if (value < 0) {
    //                             try writer.print("-**:{d:0>2}", .{minute});
    //                         } else {
    //                             try writer.print("**:{d:0>2}", .{minute});
    //                         }
    //                     } else {
    //                         if (value < 0) {
    //                             try writer.print("-{d:0>2}:{d:0>2}", .{ hour, minute });
    //                         } else {
    //                             try writer.print("{d:0>2}:{d:0>2}", .{ hour, minute });
    //                         }
    //                     }
    //                 }
    //             }
    //         },
    //         .second => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Nv");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0Wv");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0Wv");
    //             } else {
    //                 const hour, const minute, const second = utils.secondsToHourMinuteSecond(value);
    //                 if (hour > 99) {
    //                     if (value < 0) {
    //                         try writer.print("-**:{d:0>2}:{d:0>2}", .{ minute, second });
    //                     } else {
    //                         try writer.print("**:{d:0>2}:{d:0>2}", .{ minute, second });
    //                     }
    //                 } else {
    //                     if (value < 0) {
    //                         try writer.print("-{d:0>2}:{d:0>2}:{d:0>2}", .{ hour, minute, second });
    //                     } else {
    //                         try writer.print("{d:0>2}:{d:0>2}:{d:0>2}", .{ hour, minute, second });
    //                     }
    //                 }
    //             }
    //         },
    //         .second_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`second$()");
    //             } else {
    //                 var requires_suffix = true;
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         try writer.writeAll("0N ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         try writer.writeAll("0W ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         try writer.writeAll("-0W ");
    //                     } else {
    //                         requires_suffix = false;
    //                         const hour, const minute, const second = utils.secondsToHourMinuteSecond(value);
    //                         if (hour > 99) {
    //                             if (value < 0) {
    //                                 try writer.print("-**:{d:0>2}:{d:0>2} ", .{ minute, second });
    //                             } else {
    //                                 try writer.print("**:{d:0>2}:{d:0>2} ", .{ minute, second });
    //                             }
    //                         } else {
    //                             if (value < 0) {
    //                                 try writer.print("-{d:0>2}:{d:0>2}:{d:0>2} ", .{ hour, minute, second });
    //                             } else {
    //                                 try writer.print("{d:0>2}:{d:0>2}:{d:0>2} ", .{ hour, minute, second });
    //                             }
    //                         }
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0N");
    //                     if (requires_suffix) try writer.writeByte('v');
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0W");
    //                     if (requires_suffix) try writer.writeByte('v');
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0W");
    //                     if (requires_suffix) try writer.writeByte('v');
    //                 } else {
    //                     const hour, const minute, const second = utils.secondsToHourMinuteSecond(value);
    //                     if (hour > 99) {
    //                         if (value < 0) {
    //                             try writer.print("-**:{d:0>2}:{d:0>2}", .{ minute, second });
    //                         } else {
    //                             try writer.print("**:{d:0>2}:{d:0>2}", .{ minute, second });
    //                         }
    //                     } else {
    //                         if (value < 0) {
    //                             try writer.print("-{d:0>2}:{d:0>2}:{d:0>2}", .{ hour, minute, second });
    //                         } else {
    //                             try writer.print("{d:0>2}:{d:0>2}:{d:0>2}", .{ hour, minute, second });
    //                         }
    //                     }
    //                 }
    //             }
    //         },
    //         .time => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Nt");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0Wt");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0Wt");
    //             } else {
    //                 const hour, const minute, const second, const millisecond = utils.millisecondsToHourMinuteSecondMillisecond(value);
    //                 if (hour > 99) {
    //                     if (value < 0) {
    //                         try writer.print("-**:{d:0>2}:{d:0>2}.{d:0>3}", .{ minute, second, millisecond });
    //                     } else {
    //                         try writer.print("**:{d:0>2}:{d:0>2}.{d:0>3}", .{ minute, second, millisecond });
    //                     }
    //                 } else {
    //                     if (value < 0) {
    //                         try writer.print("-{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}", .{ hour, minute, second, millisecond });
    //                     } else {
    //                         try writer.print("{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}", .{ hour, minute, second, millisecond });
    //                     }
    //                 }
    //             }
    //         },
    //         .time_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`time$()");
    //             } else {
    //                 var requires_suffix = true;
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         try writer.writeAll("0N ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         try writer.writeAll("0W ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         try writer.writeAll("-0W ");
    //                     } else {
    //                         requires_suffix = false;
    //                         const hour, const minute, const second, const millisecond = utils.millisecondsToHourMinuteSecondMillisecond(value);
    //                         if (hour > 99) {
    //                             if (value < 0) {
    //                                 try writer.print("-**:{d:0>2}:{d:0>2}.{d:0>3} ", .{ minute, second, millisecond });
    //                             } else {
    //                                 try writer.print("**:{d:0>2}:{d:0>2}.{d:0>3} ", .{ minute, second, millisecond });
    //                             }
    //                         } else {
    //                             if (value < 0) {
    //                                 try writer.print("-{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3} ", .{ hour, minute, second, millisecond });
    //                             } else {
    //                                 try writer.print("{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3} ", .{ hour, minute, second, millisecond });
    //                             }
    //                         }
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0N");
    //                     if (requires_suffix) try writer.writeByte('t');
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0W");
    //                     if (requires_suffix) try writer.writeByte('t');
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0W");
    //                     if (requires_suffix) try writer.writeByte('t');
    //                 } else {
    //                     const hour, const minute, const second, const millisecond = utils.millisecondsToHourMinuteSecondMillisecond(value);
    //                     if (hour > 99) {
    //                         if (value < 0) {
    //                             try writer.print("-**:{d:0>2}:{d:0>2}.{d:0>3}", .{ minute, second, millisecond });
    //                         } else {
    //                             try writer.print("**:{d:0>2}:{d:0>2}.{d:0>3}", .{ minute, second, millisecond });
    //                         }
    //                     } else {
    //                         if (value < 0) {
    //                             try writer.print("-{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}", .{ hour, minute, second, millisecond });
    //                         } else {
    //                             try writer.print("{d:0>2}:{d:0>2}:{d:0>2}.{d:0>3}", .{ hour, minute, second, millisecond });
    //                         }
    //                     }
    //                 }
    //             }
    //         },
    //         .long => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0N");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0W");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0W");
    //             } else {
    //                 try writer.print("{d}", .{value});
    //             }
    //         },
    //         .long_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`long$()");
    //             } else {
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         try writer.writeAll("0N ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         try writer.writeAll("0W ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         try writer.writeAll("-0W ");
    //                     } else {
    //                         try writer.print("{d} ", .{value});
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0N");
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0W");
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0W");
    //                 } else {
    //                     try writer.print("{d}", .{value});
    //                 }
    //             }
    //         },
    //         .timestamp => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Np");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0Wp");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0Wp");
    //             } else {
    //                 try writer.print("{d}p", .{value}); // TODO: NYI
    //             }
    //         },
    //         .timestamp_list => unreachable,
    //         .timespan => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Nn");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0Wn");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0Wn");
    //             } else {
    //                 try writer.print("{d}n", .{value}); // TODO: NYI
    //             }
    //         },
    //         .timespan_list => unreachable,
    //         .real => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0Ne");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0We");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0We");
    //             } else {
    //                 try writer.print("{d}e", .{value});
    //             }
    //         },
    //         .real_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`real$()");
    //             } else {
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         try writer.writeAll("0N ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         try writer.writeAll("0W ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         try writer.writeAll("-0W ");
    //                     } else {
    //                         try writer.print("{d} ", .{value});
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0Ne");
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0We");
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0We");
    //                 } else {
    //                     try writer.print("{d}e", .{value});
    //                 }
    //             }
    //         },
    //         .float => |value| {
    //             if (utils.isNull(value)) {
    //                 try writer.writeAll("0n");
    //             } else if (utils.isPositiveInf(value)) {
    //                 try writer.writeAll("0w");
    //             } else if (utils.isNegativeInf(value)) {
    //                 try writer.writeAll("-0w");
    //             } else if (@floor(value) == value) {
    //                 try writer.print("{d}f", .{value});
    //             } else {
    //                 try writer.print("{d}", .{value});
    //             }
    //         },
    //         .float_list => |values| {
    //             if (values.len == 0) {
    //                 try writer.writeAll("`float$()");
    //             } else {
    //                 var requires_suffix = true;
    //                 for (values[0 .. values.len - 1]) |value| {
    //                     if (utils.isNull(value)) {
    //                         requires_suffix = false;
    //                         try writer.writeAll("0n ");
    //                     } else if (utils.isPositiveInf(value)) {
    //                         requires_suffix = false;
    //                         try writer.writeAll("0w ");
    //                     } else if (utils.isNegativeInf(value)) {
    //                         requires_suffix = false;
    //                         try writer.writeAll("-0w ");
    //                     } else {
    //                         requires_suffix = requires_suffix and @floor(value) == value;
    //                         try writer.print("{d} ", .{value});
    //                     }
    //                 }
    //                 const value = values[values.len - 1];
    //                 if (utils.isNull(value)) {
    //                     try writer.writeAll("0n");
    //                 } else if (utils.isPositiveInf(value)) {
    //                     try writer.writeAll("0w");
    //                 } else if (utils.isNegativeInf(value)) {
    //                     try writer.writeAll("-0w");
    //                 } else if (requires_suffix and @floor(value) == value) {
    //                     try writer.print("{d}f", .{value});
    //                 } else {
    //                     try writer.print("{d}", .{value});
    //                 }
    //             }
    //         },
    //         .datetime => unreachable,
    //         .datetime_list => unreachable,
    //     }
    // }
};

const ParseResult = struct {
    value: Value,
    has_suffix: bool,

    pub fn deinit(self: ParseResult, gpa: Allocator) void {
        self.value.deinit(gpa);
    }

    pub fn tag(self: ParseResult) Ast.Node.Tag {
        return self.value.tag();
    }
};

const NumberParser = struct {
    gpa: Allocator,
    str: []const u8 = undefined,

    pub fn init(gpa: Allocator) NumberParser {
        return .{
            .gpa = gpa,
        };
    }

    const ParseNumberError = error{
        InvalidCharacter,
        OutOfMemory,
        Overflow,
        ParseError,
        ShouldBacktrack,
    };

    /// TODO: Remove suffix checks once tests have been modified to use parse().
    pub fn parseNumber(self: *NumberParser, str: []const u8) ParseNumberError!Value {
        self.str = str;

        return if (self.str[self.str.len - 1] == 'c') switch (str.len) {
            2 => switch (str[0]) {
                '0' => Value{ .char = '0' },
                '1' => Value{ .char = '1' },
                '2' => Value{ .char = '2' },
                '3' => Value{ .char = '3' },
                '4' => Value{ .char = '4' },
                '5' => Value{ .char = '5' },
                '6' => Value{ .char = '6' },
                '7' => Value{ .char = '7' },
                '8' => Value{ .char = '8' },
                '9' => Value{ .char = '9' },
                else => Value{ .char = null_char },
            },
            else => Value{ .char = null_char },
        } else switch (str[0]) {
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
                    2 => .{ .long = 0 },
                    3 => switch (str[2]) {
                        'W' => .{ .long = -inf_long },
                        'w' => .{ .float = -inf_float },
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
    }

    pub fn parseNumberType(self: *NumberParser, comptime value_type: ValueType, str: []const u8) ParseNumberError!Value {
        if (value_type == .byte) {
            if (str.len == 2) return .{ .byte_list = &.{} };
            self.str = str;
            return self.byte();
        }

        const suffix = value_type.toSuffix();

        const len = if (str[str.len - 1] == suffix) str.len - 1 else str.len;
        self.str = str[0..len];

        switch (value_type) {
            .boolean => return self.boolean(self.str.len),
            .char => return self.char(self.str.len),
            else => {},
        }

        const T = value_type.toType();

        switch (self.str.len) {
            2 => if (self.str[0] == '0') switch (self.str[1]) {
                'N', 'n' => return @unionInit(Value, @tagName(value_type), utils.Null(T)),
                'W', 'w' => if (value_type != .guid) return @unionInit(Value, @tagName(value_type), utils.Inf(T)),
                else => {},
            },
            3 => if (self.str[0] == '-' and self.str[1] == '0') switch (self.str[2]) {
                'W', 'w' => if (value_type != .guid) return @unionInit(Value, @tagName(value_type), -utils.Inf(T)),
                else => {},
            },
            else => {},
        }

        // TODO: Remove references to parseNumber.
        return switch (value_type) {
            .guid => error.InvalidCharacter,
            .short => self.short(self.str.len),
            .int => self.int(self.str.len),
            .month => self.month(self.str.len),
            .date => self.date(self.str.len),
            .minute => self.minute(self.str.len),
            .second => self.second(self.str.len),
            .time => self.time(self.str.len),
            .long => self.long(self.str.len),
            .timestamp => self.parseNumber(str),
            .timespan => self.parseNumber(str),
            .real => self.real(self.str.len),
            .float => self.float(self.str.len),
            .datetime => self.parseNumber(str),
            else => |t| @compileError("Unsupported type: " ++ @tagName(t)),
        };
    }

    fn maybeBoolean(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isBooleanDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return .{ .long = try std.fmt.parseInt(i64, self.str, 10) };

        if (i == self.str.len - 1 and self.str[i] == 'b') {
            return self.boolean(i);
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
            '.' => self.maybeTime(i + 1),
            else => error.InvalidCharacter,
        };
    }

    fn maybeTime(self: NumberParser, index: usize) !Value {
        var i = index;
        while (i < self.str.len and isDigit(self.str[i])) : (i += 1) {}

        if (i == self.str.len) return self.time(i);

        return error.InvalidCharacter;
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
            'd' => self.date(i),
            'z' => self.shortDatetime(i),
            'D' => self.maybeTimespan(i),
            'n' => self.shortTimespan(i),
            't' => self.time(i),
            'u' => self.minute(i),
            'v' => self.second(i),
            '.' => self.maybeFloat(i + 1),
            ':' => self.maybeMinute(i + 1),
            else => error.InvalidCharacter,
        };
    }

    fn boolean(self: NumberParser, index: usize) !Value {
        if (index == 1) {
            return .{ .boolean = self.str[0] == '1' };
        } else {
            const list = self.gpa.alloc(bool, index) catch panic("Failed to allocate memory.", .{});
            for (self.str[0..index], 0..) |c, i| {
                list[i] = c == '1';
            }
            return .{ .boolean_list = list };
        }
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
        var list = std.ArrayList(u8).init(self.gpa);
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
        return .{ .char = if (index == 1) self.str[0] else null_char };
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
            6 => blk: {
                const year_value = try std.fmt.parseInt(u32, self.str[0..2], 10);
                const month_value = try std.fmt.parseInt(u32, self.str[2..4], 10);
                const day_value = try std.fmt.parseInt(u32, self.str[4..6], 10);
                const days = try calculateDays(if (year_value < 50) year_value + 2000 else year_value + 1900, month_value, day_value);
                break :blk .{ .date = days };
            },
            8 => blk: {
                const year_value = try std.fmt.parseInt(u32, self.str[0..4], 10);
                const month_value = try std.fmt.parseInt(u32, self.str[4..6], 10);
                const day_value = try std.fmt.parseInt(u32, self.str[6..8], 10);
                const days = try calculateDays(year_value, month_value, day_value);
                break :blk .{ .date = days };
            },
            10 => .{ .date = try parseDate(self.str) },
            else => error.InvalidCharacter,
        };
    }

    fn parseDate(str: []const u8) !i32 {
        if (str[4] != '.' or str[7] != '.') return error.InvalidCharacter;

        const year_value = try std.fmt.parseInt(u32, str[0..4], 10);
        const month_value = try std.fmt.parseInt(u32, str[5..7], 10);
        const day_value = try std.fmt.parseInt(u32, str[8..10], 10);
        return calculateDays(year_value, month_value, day_value);
    }

    /// https://howardhinnant.github.io/date_algorithms.html#days_from_civil
    fn calculateDays(y: u32, m: u32, d: u32) !i32 {
        if (y < 1) return error.Overflow;
        if (m < 1 or m > 12) return error.Overflow;
        if (d < 1 or d > lastDayOfMonth(y, m)) return error.Overflow;

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
                const year_value = try std.fmt.parseInt(u32, self.str[start_index .. start_index + 2], 10);
                const month_value = try std.fmt.parseInt(u32, self.str[start_index + 2 .. start_index + 4], 10);
                const day_value = try std.fmt.parseInt(u32, self.str[start_index + 4 .. start_index + 6], 10);
                const days = try calculateDays(if (year_value < 50) year_value + 2000 else year_value + 1900, month_value, day_value);
                break :blk .{ .datetime = @floatFromInt(if (is_negative) -days else days) };
            },
            7 => error.InvalidCharacter,
            8 => blk: {
                const year_value = try std.fmt.parseInt(u32, self.str[start_index .. start_index + 4], 10);
                const month_value = try std.fmt.parseInt(u32, self.str[start_index + 4 .. start_index + 6], 10);
                const day_value = try std.fmt.parseInt(u32, self.str[start_index + 6 .. start_index + 8], 10);
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
            1, 2 => blk: {
                const minutes_value = try std.fmt.parseInt(i32, str[start_index..], 10);
                const seconds = minutes_value * 60;
                break :blk if (is_negative) -seconds else seconds;
            },
            inline 5, 6 => |len| switch (str[str.len - 3] == ':') {
                true => blk: {
                    if (comptime len == 6 and is_negative) return error.InvalidCharacter;
                    const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                    if (seconds_value > 59) return error.Overflow;

                    const minutes_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 3], 10);
                    const seconds = seconds_value + minutes_value * 60;
                    break :blk if (is_negative) -seconds else seconds;
                },
                false => blk: {
                    const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                    if (seconds_value > 59) return error.Overflow;

                    const minutes_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 2], 10);
                    const seconds = seconds_value + minutes_value * 60;
                    break :blk if (is_negative) -seconds else seconds;
                },
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
            1, 2 => blk: {
                const hours_value = try std.fmt.parseInt(i32, str[start_index..], 10);
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
            inline 8, 9 => |len| switch (str[str.len - 6] == ':' and str[str.len - 3] == ':') {
                true => blk: {
                    if (comptime len == 9 and is_negative) return error.InvalidCharacter;
                    const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                    if (seconds_value > 59) return error.Overflow;
                    const minutes_value = try std.fmt.parseInt(i32, str[str.len - 5 .. str.len - 3], 10);
                    if (minutes_value > 59) return error.Overflow;

                    const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 6], 10);
                    const seconds = hours_value * 3600 + minutes_value * 60 + seconds_value;
                    break :blk if (is_negative) -seconds else seconds;
                },
                false => blk: {
                    const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..], 10);
                    if (seconds_value > 59) return error.Overflow;
                    const minutes_value = try std.fmt.parseInt(i32, str[str.len - 4 .. str.len - 2], 10);
                    if (minutes_value > 59) return error.Overflow;

                    const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 4], 10);
                    const seconds = hours_value *% 3600 + minutes_value * 60 + seconds_value;
                    break :blk if (is_negative) -seconds else seconds;
                },
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
        return .{ .time = try parseTime(self.str[0..index]) };
    }

    fn parseTime(str: []const u8) !i32 {
        const is_negative = str[0] == '-';
        const start_index: usize = if (is_negative) 1 else 0;
        return switch (str.len - start_index) {
            1, 2 => blk: {
                const hours_value = try std.fmt.parseInt(i32, str[start_index..], 10);
                const milliseconds = hours_value * milliseconds_per_hour;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
            3, 4, 5 => blk: {
                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 2 ..][0..2], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 2], 10);
                const milliseconds = hours_value *% milliseconds_per_hour +
                    minutes_value * milliseconds_per_minute;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
            6, 7, 8 => blk: {
                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..][0..2], 10);
                if (seconds_value > 59) return error.Overflow;
                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 4 ..][0..2], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 4], 10);
                const milliseconds = hours_value *% milliseconds_per_hour +
                    minutes_value * milliseconds_per_minute +
                    seconds_value * milliseconds_per_second;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
            9 => blk: {
                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 5 ..][0..2], 10);
                if (seconds_value > 59) return error.Overflow;
                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 7 ..][0..2], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 7], 10);
                const milliseconds_value = try std.fmt.parseInt(i32, str[str.len - 3 ..], 10);
                const milliseconds = hours_value *% milliseconds_per_hour +
                    minutes_value * milliseconds_per_minute +
                    seconds_value * milliseconds_per_second +
                    milliseconds_value;
                break :blk if (is_negative) -milliseconds else milliseconds;
            },
            inline 10, 11, 12, 13, 14 => |len| switch (str[start_index + 2] == ':') {
                true => blk: {
                    if (str[start_index + 2] != ':' or str[start_index + 5] != ':' or str[start_index + 8] != '.') return error.InvalidCharacter;

                    const minutes_value = try std.fmt.parseInt(i32, str[start_index + 3 ..][0..2], 10);
                    if (minutes_value > 59) return error.Overflow;
                    const seconds_value = try std.fmt.parseInt(i32, str[start_index + 6 ..][0..2], 10);
                    if (seconds_value > 59) return error.Overflow;

                    const hours_value = try std.fmt.parseInt(i32, str[start_index..][0..2], 10);
                    const milliseconds_len = if (len == 10) 1 else if (len == 11) 2 else 3;
                    const factor = if (len == 10) 100 else if (len == 11) 10 else 1;
                    const milliseconds_value = try std.fmt.parseInt(i32, str[start_index + 9 ..][0..milliseconds_len], 10) * factor;
                    const milliseconds = hours_value * milliseconds_per_hour +
                        minutes_value * milliseconds_per_minute +
                        seconds_value * milliseconds_per_second +
                        milliseconds_value;
                    break :blk if (is_negative) -milliseconds else milliseconds;
                },
                false => switch (str[start_index + 3] == ':') {
                    true => blk: {
                        if (len == 10) return error.InvalidCharacter;
                        if (is_negative) return error.InvalidCharacter;
                        if (str[start_index + 3] != ':' or str[start_index + 6] != ':' or str[start_index + 9] != '.') return error.InvalidCharacter;

                        const minutes_value = try std.fmt.parseInt(i32, str[start_index + 4 ..][0..2], 10);
                        if (minutes_value > 59) return error.Overflow;
                        const seconds_value = try std.fmt.parseInt(i32, str[start_index + 7 ..][0..2], 10);
                        if (seconds_value > 59) return error.Overflow;

                        const hours_value = try std.fmt.parseInt(i32, str[start_index..][0..3], 10);
                        const milliseconds_len = if (len == 11) 1 else if (len == 12) 2 else 3;
                        const factor = if (len == 11) 100 else if (len == 12) 10 else 1;
                        const milliseconds_value = try std.fmt.parseInt(i32, str[start_index + 10 ..][0..milliseconds_len], 10) * factor;
                        const milliseconds = hours_value *% milliseconds_per_hour +
                            minutes_value * milliseconds_per_minute +
                            seconds_value * milliseconds_per_second +
                            milliseconds_value;
                        break :blk if (is_negative) -milliseconds else milliseconds;
                    },
                    false => blk: {
                        const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..][0..2], 10);
                        if (seconds_value > 59) return error.Overflow;
                        const minutes_value = try std.fmt.parseInt(i32, str[str.len - 4 ..][0..2], 10);
                        if (minutes_value > 59) return error.Overflow;

                        const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 4], 10);
                        const milliseconds = hours_value *% milliseconds_per_hour +
                            minutes_value * milliseconds_per_minute +
                            seconds_value * milliseconds_per_second;
                        break :blk if (is_negative) -milliseconds else milliseconds;
                    },
                },
            },
            else => blk: {
                const seconds_value = try std.fmt.parseInt(i32, str[str.len - 2 ..][0..2], 10);
                if (seconds_value > 59) return error.Overflow;
                const minutes_value = try std.fmt.parseInt(i32, str[str.len - 4 ..][0..2], 10);
                if (minutes_value > 59) return error.Overflow;

                const hours_value = try std.fmt.parseInt(i32, str[start_index .. str.len - 4], 10);
                const milliseconds = hours_value *% milliseconds_per_hour +
                    minutes_value * milliseconds_per_minute +
                    seconds_value * milliseconds_per_second;
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

pub fn testNumberParser(input: [:0]const u8, comptime expected_type: ValueType, comptime expected_value: anytype) !void {
    const gpa = std.testing.allocator;

    var tree = try Ast.parse(gpa, input, .{ .version = .@"4.0", .language = .q });
    defer tree.deinit(gpa);

    const tags = tree.tokens.items(.tag);
    try std.testing.expectEqualSlices(Token.Tag, &.{.number_literal}, tags[0 .. tags.len - 1]);

    const errors = try gpa.alloc(Ast.Error.Tag, tree.errors.len);
    defer gpa.free(errors);
    for (tree.errors, 0..) |err, i| {
        errors[i] = err.tag;
    }
    try std.testing.expectEqualSlices(Ast.Error.Tag, &.{}, errors);
    try std.testing.expectEqual(2, tree.tokens.len);
    try std.testing.expectEqual(1, tree.values.len);

    const value = tree.values[0];
    try std.testing.expectEqual(expected_type, @as(ValueType, value));

    const actual_value = @field(value, @tagName(expected_type));
    const actual = if (@typeInfo(@TypeOf(actual_value)) == .array) &actual_value else actual_value;
    switch (@typeInfo(@TypeOf(expected_value))) {
        .array => |type_info| {
            try std.testing.expectEqualSlices(type_info.child, &expected_value, actual);
        },
        .pointer => |type_info| {
            try std.testing.expectEqualSlices(@typeInfo(type_info.child).Array.child, expected_value, actual);
        },
        .@"struct" => |type_info| {
            const fields_info = type_info.fields;
            const T = if (fields_info.len > 0) fields_info[0].type else @typeInfo(@TypeOf(actual)).pointer.child;
            const expected = try gpa.alloc(T, fields_info.len);
            defer gpa.free(expected);
            inline for (fields_info, 0..) |field, i| {
                expected[i] = @field(expected_value, field.name);
            }
            try std.testing.expectEqualSlices(T, expected, actual);
        },
        else => {
            switch (@TypeOf(expected_value)) {
                comptime_float => @compileError("comptime_float is not supported."),
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

pub fn testNumberParserError(input: [:0]const u8, expected: anyerror) !void {
    const gpa = std.testing.allocator;

    var tokenizer = Tokenizer.init(input, .q);
    const token = tokenizer.next();

    try std.testing.expectEqual(.eof, tokenizer.next().tag);

    const loc = token.loc;
    const str = input[loc.start..loc.end];

    var number_parser = NumberParser.init(gpa);
    _ = number_parser.parseNumber(str) catch |e| {
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
    _ = @import("test/month.zig");
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

test {
    @import("std").testing.refAllDecls(@This());
}
