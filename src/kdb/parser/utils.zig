const std = @import("std");

const number_parser = @import("number_parser.zig");

pub fn cast(comptime T: type, value: anytype) T {
    const FromT = @TypeOf(value);
    if (T == FromT) return value;
    if (T == [16]u8) return std.mem.zeroes([16]u8);

    if (isNull(value)) return Null(T);
    if (isPositiveInf(value)) return Inf(T);
    if (isNegativeInf(value)) return -Inf(T);

    return switch (FromT) {
        i64 => switch (T) {
            i16, i32 => @intCast(value),
            f64 => @floatFromInt(value),
            else => @compileError("Unsupported type: " ++ @typeName(T)),
        },
        f64 => switch (T) {
            i16, i32, i64 => @intFromFloat(value),
            else => @compileError("Unsupported type: " ++ @typeName(T)),
        },
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    };
}

pub fn isNull(value: anytype) bool {
    const T = @TypeOf(value);
    if (@typeInfo(T) == .Pointer) {
        for (value) |v| if (!isNull(v)) return false;
        return true;
    }
    return switch (T) {
        i16, i32, i64 => value == Null(T),
        f64 => std.math.isNan(value),
        [16]u8 => blk: {
            for (value) |v| if (v != 0) break :blk false;
            break :blk true;
        },
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    };
}

pub fn isPositiveInf(value: anytype) bool {
    const T = @TypeOf(value);
    if (@typeInfo(T) == .Pointer) {
        for (value) |v| if (!isPositiveInf(v)) return false;
        return true;
    }
    return switch (T) {
        i16, i32, i64 => value == Inf(T),
        f64 => std.math.isPositiveInf(value),
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    };
}

pub fn isNegativeInf(value: anytype) bool {
    const T = @TypeOf(value);
    if (@typeInfo(T) == .Pointer) {
        for (value) |v| if (!isNegativeInf(v)) return false;
        return true;
    }
    return switch (T) {
        i16, i32, i64 => value == -Inf(T),
        f64 => std.math.isNegativeInf(value),
        else => @compileError("Unsupported type: " ++ @typeName(T)),
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

fn Null(comptime T: type) T {
    return switch (T) {
        i16 => number_parser.null_short,
        i32 => number_parser.null_int,
        i64 => number_parser.null_long,
        f64 => number_parser.null_float,
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    };
}

fn Inf(comptime T: type) T {
    return switch (T) {
        i16 => number_parser.inf_short,
        i32 => number_parser.inf_int,
        i64 => number_parser.inf_long,
        f64 => number_parser.inf_float,
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    };
}
