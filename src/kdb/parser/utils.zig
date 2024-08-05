const std = @import("std");

const number_parser = @import("number_parser.zig");

pub const null_guid = number_parser.null_guid;

pub const null_short = number_parser.null_short;
pub const inf_short = number_parser.inf_short;

pub const null_int = number_parser.null_int;
pub const inf_int = number_parser.inf_int;

pub const null_long = number_parser.null_long;
pub const inf_long = number_parser.inf_long;

pub const null_real = number_parser.null_real;
pub const inf_real = number_parser.inf_real;

pub const null_float = number_parser.null_float;
pub const inf_float = number_parser.inf_float;

pub const null_char = number_parser.null_char;

pub fn cast(comptime T: type, value: anytype) T {
    const FromT = @TypeOf(value);
    if (T == FromT) return value;

    if (isNull(value)) return Null(T);
    if (isPositiveInf(value)) return Inf(T);
    if (isNegativeInf(value)) return -Inf(T);

    return switch (FromT) {
        i64 => switch (T) {
            i16 => @intCast(value),
            f64 => @floatFromInt(value),
            else => @compileError("Unsupported type: " ++ @typeName(T)),
        },
        f64 => switch (T) {
            i16, i64 => @intFromFloat(value),
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
        i64 => value == null_long,
        f64 => std.math.isNan(value),
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
        i64 => value == inf_long,
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
        i64 => value == -inf_long,
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
        i16 => null_short,
        i64 => null_long,
        f64 => null_float,
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    };
}

fn Inf(comptime T: type) T {
    return switch (T) {
        i16 => inf_short,
        i64 => inf_long,
        f64 => inf_float,
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    };
}
