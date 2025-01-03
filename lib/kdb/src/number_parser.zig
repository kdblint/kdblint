const std = @import("std");

const common = @import("number_parser/common.zig");
pub const TypeHint = common.TypeHint;
pub const Result = common.Result;
pub const Error = common.Error;

pub const parseNone = @import("number_parser/none.zig").parseNone;
pub const parseBool = @import("number_parser/bool.zig").parseBool;
pub const parseGuid = @import("number_parser/guid.zig").parseGuid;
pub const parseByte = @import("number_parser/byte.zig").parseByte;
pub const parseShort = @import("number_parser/short.zig").parseShort;
pub const parseInt = @import("number_parser/int.zig").parseInt;
pub const parseLong = @import("number_parser/long.zig").parseLong;
pub const parseReal = @import("number_parser/real.zig").parseReal;
pub const parseFloat = @import("number_parser/float.zig").parseFloat;
pub const parseChar = @import("number_parser/char.zig").parseChar;
pub const parseTimestamp = @import("number_parser/timestamp.zig").parseTimestamp;
pub const parseMonth = @import("number_parser/month.zig").parseMonth;
pub const parseDate = @import("number_parser/date.zig").parseDate;
pub const parseDatetime = @import("number_parser/datetime.zig").parseDatetime;
pub const parseTimespan = @import("number_parser/timespan.zig").parseTimespan;
pub const parseMinute = @import("number_parser/minute.zig").parseMinute;
pub const parseSecond = @import("number_parser/second.zig").parseSecond;
pub const parseTime = @import("number_parser/time.zig").parseTime;

pub const null_char = common.null_char;

pub const null_short = common.null_short;
pub const inf_short = common.inf_short;

pub const null_int = common.null_int;
pub const inf_int = common.inf_int;

pub const null_long = common.null_long;
pub const inf_long = common.inf_long;

pub const null_real = common.null_real;
pub const inf_real = common.inf_real;

pub const null_float = common.null_float;
pub const inf_float = common.inf_float;

pub const epoch_nanoseconds = common.epoch_nanoseconds;

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
        .timestamp => return parseTimestamp(bytes, allow_suffix),
        .month => return parseMonth(bytes, allow_suffix),
        .date => return parseDate(bytes, allow_suffix),
        .datetime => return parseDatetime(bytes, allow_suffix),
        .timespan => return parseTimespan(bytes, allow_suffix),
        .minute => return parseMinute(bytes, allow_suffix),
        .second => return parseSecond(bytes, allow_suffix),
        .time => return parseTime(bytes, allow_suffix),
    }
}
