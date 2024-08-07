const std = @import("std");
const panic = std.debug.panic;

const number_parser = @import("number_parser.zig");
const Value = number_parser.Value;

pub fn castValue(comptime T: type, value: Value) T {
    return switch (value) {
        .guid => |v| cast(T, v),
        .char => |v| cast(T, v),
        .short => |v| cast(T, v),
        .int, .month, .date, .minute, .second, .time => |v| cast(T, v),
        .long, .timestamp, .timespan => |v| cast(T, v),
        .real => |v| cast(T, v),
        .float, .datetime => |v| cast(T, v),
        else => |t| panic("Unsupported type: {s}", .{@tagName(t)}),
    };
}

fn cast(comptime T: type, value: anytype) T {
    const FromT = @TypeOf(value);
    if (T == FromT) return value;
    if (T == [16]u8 or FromT == [16]u8) return Null(T);

    if (T == u8) {
        if (isNullOrInf(value)) return Null(T);
    } else {
        if (isNull(value)) return Null(T);
        if (isPositiveInf(value)) return Inf(T);
        if (isNegativeInf(value)) return -Inf(T);
    }

    return switch (FromT) {
        u8, i16, i32, i64 => switch (T) {
            u8 => switch (value) {
                0 => '0',
                1 => '1',
                2 => '2',
                3 => '3',
                4 => '4',
                5 => '5',
                6 => '6',
                7 => '7',
                8 => '8',
                9 => '9',
                else => Null(T),
            },
            i16, i32, i64 => @intCast(value),
            f32, f64 => @floatFromInt(value),
            else => @compileError("Unsupported type: " ++ @typeName(T)),
        },
        f32, f64 => switch (T) {
            u8 => Null(T),
            i16, i32, i64 => @intFromFloat(value),
            f32, f64 => @floatCast(value),
            else => @compileError("Unsupported type: " ++ @typeName(T)),
        },
        else => @compileError("Unsupported type: " ++ @typeName(FromT)),
    };
}

pub fn isNull(value: anytype) bool {
    const T = @TypeOf(value);
    if (@typeInfo(T) == .Pointer) {
        for (value) |v| if (!isNull(v)) return false;
        return true;
    }
    return switch (T) {
        bool => false,
        [16]u8 => blk: {
            for (value) |v| if (v != 0) break :blk false;
            break :blk true;
        },
        f32, f64 => std.math.isNan(value),
        else => value == Null(T),
    };
}

pub fn isPositiveInf(value: anytype) bool {
    const T = @TypeOf(value);
    if (@typeInfo(T) == .Pointer) {
        for (value) |v| if (!isPositiveInf(v)) return false;
        return true;
    }
    return switch (T) {
        u8 => false,
        f32, f64 => std.math.isPositiveInf(value),
        else => value == Inf(T),
    };
}

pub fn isNegativeInf(value: anytype) bool {
    const T = @TypeOf(value);
    if (@typeInfo(T) == .Pointer) {
        for (value) |v| if (!isNegativeInf(v)) return false;
        return true;
    }
    return switch (T) {
        u8 => false,
        f32, f64 => std.math.isNegativeInf(value),
        else => value == -Inf(T),
    };
}

pub fn isNullOrInf(value: anytype) bool {
    const T = @TypeOf(value);
    if (@typeInfo(T) == .Pointer) {
        for (value) |v| if (!isNullOrInf(v)) return false;
        return true;
    }
    return isNull(value) or isPositiveInf(value) or isNegativeInf(value);
}

pub fn Null(comptime T: type) T {
    return switch (T) {
        [16]u8 => std.mem.zeroes([16]u8),
        u8 => number_parser.null_char,
        i16 => number_parser.null_short,
        i32 => number_parser.null_int,
        i64 => number_parser.null_long,
        f32 => number_parser.null_real,
        f64 => number_parser.null_float,
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    };
}

pub fn Inf(comptime T: type) T {
    return switch (T) {
        i16 => number_parser.inf_short,
        i32 => number_parser.inf_int,
        i64 => number_parser.inf_long,
        f32 => number_parser.inf_real,
        f64 => number_parser.inf_float,
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    };
}
