const std = @import("std");

const parseDate = @import("date.zig").parseDate;
const parseTimespan = @import("timespan.zig").parseTimespan;

const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;

const null_long = common.null_long;
const inf_long = common.inf_long;

pub fn parseTimestamp(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .timestamp = null_long },
        'W', 'w' => return .{ .timestamp = inf_long },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'p' => .{ .timestamp = null_long },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'p' => .{ .timestamp = inf_long },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    var slice = if (allow_suffix and bytes[bytes.len - 1] == 'p') bytes[0 .. bytes.len - 1] else bytes;
    var offset: usize = 0;

    var days_value: i64 = 0;
    if (std.mem.indexOfScalar(u8, slice, 'D')) |d_index| {
        days_value = switch (parseDate(slice[0..d_index], false)) {
            .date => |date| @intCast(date),
            .failure => |err| return .{ .failure = err },
            else => unreachable,
        };
        offset = d_index;
        slice = slice[d_index..];
    }

    const nanosecond_value = switch (parseTimespan(slice, false)) {
        .timespan => |timespan| timespan,
        .failure => |err| switch (err) {
            .invalid_character => |i| return .{ .failure = .{ .invalid_character = i + offset } },
            else => return .{ .failure = err },
        },
        else => unreachable,
    };

    const nanoseconds = days_value * std.time.ns_per_day + nanosecond_value;
    return .{ .timestamp = nanoseconds };
}

test "parse number literal - timestamp" {
    const _std = @import("std");
    const _clock: _std.Io.Clock = .real;
    const _start = try _clock.now(_std.testing.io);
    const _file: _std.fs.File = .adaptFromNewApi(try _std.Io.Dir.cwd().createFile(_std.testing.io, @src().fn_name ++ ".log", .{}));
    defer _file.close();
    var _file_writer = _file.writer(&.{});
    const _writer = &_file_writer.interface;
    try _writer.writeAll(@src().fn_name);
    try _writer.flush();
    defer {
        _writer.print(": {d}ms\n", .{_start.durationTo(_clock.now(_std.testing.io) catch unreachable).toMilliseconds()}) catch unreachable;
        _writer.flush() catch unreachable;
    }
    try testParse("0N", .timestamp, false, .{ .timestamp = null_long });
    try testParse("0n", .timestamp, false, .{ .timestamp = null_long });
    try testParse("0W", .timestamp, false, .{ .timestamp = inf_long });
    try testParse("0w", .timestamp, false, .{ .timestamp = inf_long });

    try testParse("0", .timestamp, false, .{ .timestamp = 0 });
    try testParse("9", .timestamp, false, .{ .timestamp = 32400000000000 });

    try testParse("00", .timestamp, false, .{ .timestamp = 0 });
    try testParse("99", .timestamp, false, .{ .timestamp = 356400000000000 });

    try testParse("000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("959", .timestamp, false, .{ .timestamp = 35940000000000 });

    try testParse("0000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("9959", .timestamp, false, .{ .timestamp = 359940000000000 });

    try testParse("00000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("99959", .timestamp, false, .{ .timestamp = 3599940000000000 });

    try testParse("000000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("995959", .timestamp, false, .{ .timestamp = 359999000000000 });

    try testParse("0000000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("9995959", .timestamp, false, .{ .timestamp = 3599999000000000 });

    try testParse("00000000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("99995959", .timestamp, false, .{ .timestamp = 35999999000000000 });

    try testParse("000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("999999999", .timestamp, false, .{ .timestamp = 53315199000000000 });

    try testParse("0000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("9999999999", .timestamp, false, overflow);

    try testParse("00000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("99999999999", .timestamp, false, overflow);

    try testParse("000000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("999999999999", .timestamp, false, overflow);

    try testParse("0000000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("9999999999999", .timestamp, false, overflow);

    try testParse("00000000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("99999999999999", .timestamp, false, overflow);

    try testParse("000000000000000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("995959999999999", .timestamp, false, .{ .timestamp = 359999999999999 });

    try testParse("0000000000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("9999999999999999", .timestamp, false, overflow);

    try testParse("00000000000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("99999999999999999", .timestamp, false, overflow);

    try testParse("000000000000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("999999999999999999", .timestamp, false, overflow);

    try testParse("0000000000000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("9223372036854775807", .timestamp, false, overflow);

    try testParse("2000.01.01D", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D.9999999999", .timestamp, false, .{ .timestamp = 999999999 });

    try testParse("2000.01.01D0", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D0.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D0.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D0.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9", .timestamp, false, .{ .timestamp = 32400000000000 });
    try testParse("2000.01.01D9.", .timestamp, false, .{ .timestamp = 32400000000000 });
    try testParse("2000.01.01D9.9", .timestamp, false, .{ .timestamp = 32400900000000 });
    try testParse("2000.01.01D9.999999999", .timestamp, false, .{ .timestamp = 32400999999999 });
    try testParse("2000.01.01D9.9999999999", .timestamp, false, .{ .timestamp = 32400999999999 });

    try testParse("2000.01.01D00", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00:.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99", .timestamp, false, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99.", .timestamp, false, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99.9", .timestamp, false, .{ .timestamp = 356400900000000 });
    try testParse("2000.01.01D99.999999999", .timestamp, false, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99.9999999999", .timestamp, false, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99:", .timestamp, false, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99:.", .timestamp, false, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99:.9", .timestamp, false, .{ .timestamp = 356400900000000 });
    try testParse("2000.01.01D99:.999999999", .timestamp, false, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99:.9999999999", .timestamp, false, .{ .timestamp = 356400999999999 });

    try testParse("2000.01.01D000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D000.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D000.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D000.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D000.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D959", .timestamp, false, .{ .timestamp = 35940000000000 });
    try testParse("2000.01.01D959.", .timestamp, false, .{ .timestamp = 35940000000000 });
    try testParse("2000.01.01D959.9", .timestamp, false, .{ .timestamp = 35940900000000 });
    try testParse("2000.01.01D959.999999999", .timestamp, false, .{ .timestamp = 35940999999999 });
    try testParse("2000.01.01D959.9999999999", .timestamp, false, .{ .timestamp = 35940999999999 });

    try testParse("2000.01.01D0000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D0000.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0000.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00:00.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9959", .timestamp, false, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D9959.", .timestamp, false, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D9959.9", .timestamp, false, .{ .timestamp = 359940900000000 });
    try testParse("2000.01.01D9959.999999999", .timestamp, false, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D9959.9999999999", .timestamp, false, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D99:59", .timestamp, false, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D99:59.", .timestamp, false, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D99:59.9", .timestamp, false, .{ .timestamp = 359940900000000 });
    try testParse("2000.01.01D99:59.999999999", .timestamp, false, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D99:59.9999999999", .timestamp, false, .{ .timestamp = 359940999999999 });

    try testParse("2000.01.01D00000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00000.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00000.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99959", .timestamp, false, .{ .timestamp = 3599940000000000 });
    try testParse("2000.01.01D99959.", .timestamp, false, .{ .timestamp = 3599940000000000 });
    try testParse("2000.01.01D99959.9", .timestamp, false, .{ .timestamp = 3599940900000000 });
    try testParse("2000.01.01D99959.999999999", .timestamp, false, .{ .timestamp = 3599940999999999 });
    try testParse("2000.01.01D99959.9999999999", .timestamp, false, .{ .timestamp = 3599940999999999 });

    try testParse("2000.01.01D000000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D000000.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D000000.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D000000.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D000000.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00:00", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00:00.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00:00.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00:00:00.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00:00.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D995959", .timestamp, false, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D995959.", .timestamp, false, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D995959.9", .timestamp, false, .{ .timestamp = 359999900000000 });
    try testParse("2000.01.01D995959.999999999", .timestamp, false, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D995959.9999999999", .timestamp, false, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D99:59:59", .timestamp, false, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D99:59:59.", .timestamp, false, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D99:59:59.9", .timestamp, false, .{ .timestamp = 359999900000000 });
    try testParse("2000.01.01D99:59:59.999999999", .timestamp, false, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D99:59:59.9999999999", .timestamp, false, .{ .timestamp = 359999999999999 });

    try testParse("2000.01.01D0000000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000000.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000000.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D0000000.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0000000.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9995959", .timestamp, false, .{ .timestamp = 3599999000000000 });
    try testParse("2000.01.01D9995959.", .timestamp, false, .{ .timestamp = 3599999000000000 });
    try testParse("2000.01.01D9995959.9", .timestamp, false, .{ .timestamp = 3599999900000000 });
    try testParse("2000.01.01D9995959.999999999", .timestamp, false, .{ .timestamp = 3599999999999999 });
    try testParse("2000.01.01D9995959.9999999999", .timestamp, false, .{ .timestamp = 3599999999999999 });

    try testParse("2000.01.01D00000000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000000.", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000000.9", .timestamp, false, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00000000.999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00000000.9999999999", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99995959", .timestamp, false, .{ .timestamp = 35999999000000000 });
    try testParse("2000.01.01D99995959.", .timestamp, false, .{ .timestamp = 35999999000000000 });
    try testParse("2000.01.01D99995959.9", .timestamp, false, .{ .timestamp = 35999999900000000 });
    try testParse("2000.01.01D99995959.999999999", .timestamp, false, .{ .timestamp = 35999999999999999 });
    try testParse("2000.01.01D99995959.9999999999", .timestamp, false, .{ .timestamp = 35999999999999999 });

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testParse("2000.01.01D000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D000000000.", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D000000000.9", .timestamp, false, .{ .timestamp = -946684799100000000 });
    try testParse("2000.01.01D000000000.999999999", .timestamp, false, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D000000000.9999999999", .timestamp, false, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D999999999", .timestamp, false, .{ .timestamp = 53315199000000000 });
    try testParse("2000.01.01D999999999.", .timestamp, false, .{ .timestamp = 53315199000000000 });
    try testParse("2000.01.01D999999999.9", .timestamp, false, .{ .timestamp = 53315199900000000 });
    try testParse("2000.01.01D999999999.999999999", .timestamp, false, .{ .timestamp = 53315199999999999 });
    try testParse("2000.01.01D999999999.9999999999", .timestamp, false, .{ .timestamp = 53315199999999999 });

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testParse("2000.01.01D000000000000000", .timestamp, false, .{ .timestamp = 0 });
    try testParse("2000.01.01D995959999999999", .timestamp, false, .{ .timestamp = 359999999999999 });

    try testParse("2000.01.01D0000000000000000000", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D0000000000000000000.", .timestamp, false, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D0000000000000000000.9", .timestamp, false, .{ .timestamp = -946684799100000000 });
    try testParse("2000.01.01D0000000000000000000.999999999", .timestamp, false, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D0000000000000000000.9999999999", .timestamp, false, .{ .timestamp = -946684799000000001 });

    try testParse("0N", .timestamp, true, .{ .timestamp = null_long });
    try testParse("0n", .timestamp, true, .{ .timestamp = null_long });
    try testParse("0W", .timestamp, true, .{ .timestamp = inf_long });
    try testParse("0w", .timestamp, true, .{ .timestamp = inf_long });

    try testParse("0", .timestamp, true, .{ .timestamp = 0 });
    try testParse("9", .timestamp, true, .{ .timestamp = 32400000000000 });

    try testParse("00", .timestamp, true, .{ .timestamp = 0 });
    try testParse("99", .timestamp, true, .{ .timestamp = 356400000000000 });

    try testParse("000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("959", .timestamp, true, .{ .timestamp = 35940000000000 });

    try testParse("0000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("9959", .timestamp, true, .{ .timestamp = 359940000000000 });

    try testParse("00000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("99959", .timestamp, true, .{ .timestamp = 3599940000000000 });

    try testParse("000000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("995959", .timestamp, true, .{ .timestamp = 359999000000000 });

    try testParse("0000000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("9995959", .timestamp, true, .{ .timestamp = 3599999000000000 });

    try testParse("00000000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("99995959", .timestamp, true, .{ .timestamp = 35999999000000000 });

    try testParse("000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("999999999", .timestamp, true, .{ .timestamp = 53315199000000000 });

    try testParse("0000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("9999999999", .timestamp, true, overflow);

    try testParse("00000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("99999999999", .timestamp, true, overflow);

    try testParse("000000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("999999999999", .timestamp, true, overflow);

    try testParse("0000000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("9999999999999", .timestamp, true, overflow);

    try testParse("00000000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("99999999999999", .timestamp, true, overflow);

    try testParse("000000000000000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("995959999999999", .timestamp, true, .{ .timestamp = 359999999999999 });

    try testParse("0000000000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("9999999999999999", .timestamp, true, overflow);

    try testParse("00000000000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("99999999999999999", .timestamp, true, overflow);

    try testParse("000000000000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("999999999999999999", .timestamp, true, overflow);

    try testParse("0000000000000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("9223372036854775807", .timestamp, true, overflow);

    try testParse("2000.01.01D", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D.9999999999", .timestamp, true, .{ .timestamp = 999999999 });

    try testParse("2000.01.01D0", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D0.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9", .timestamp, true, .{ .timestamp = 32400000000000 });
    try testParse("2000.01.01D9.", .timestamp, true, .{ .timestamp = 32400000000000 });
    try testParse("2000.01.01D9.9", .timestamp, true, .{ .timestamp = 32400900000000 });
    try testParse("2000.01.01D9.999999999", .timestamp, true, .{ .timestamp = 32400999999999 });
    try testParse("2000.01.01D9.9999999999", .timestamp, true, .{ .timestamp = 32400999999999 });

    try testParse("2000.01.01D00", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00:.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99", .timestamp, true, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99.", .timestamp, true, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99.9", .timestamp, true, .{ .timestamp = 356400900000000 });
    try testParse("2000.01.01D99.999999999", .timestamp, true, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99.9999999999", .timestamp, true, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99:", .timestamp, true, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99:.", .timestamp, true, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99:.9", .timestamp, true, .{ .timestamp = 356400900000000 });
    try testParse("2000.01.01D99:.999999999", .timestamp, true, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99:.9999999999", .timestamp, true, .{ .timestamp = 356400999999999 });

    try testParse("2000.01.01D000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D000.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D000.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D000.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D000.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D959", .timestamp, true, .{ .timestamp = 35940000000000 });
    try testParse("2000.01.01D959.", .timestamp, true, .{ .timestamp = 35940000000000 });
    try testParse("2000.01.01D959.9", .timestamp, true, .{ .timestamp = 35940900000000 });
    try testParse("2000.01.01D959.999999999", .timestamp, true, .{ .timestamp = 35940999999999 });
    try testParse("2000.01.01D959.9999999999", .timestamp, true, .{ .timestamp = 35940999999999 });

    try testParse("2000.01.01D0000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D0000.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0000.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00:00.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9959", .timestamp, true, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D9959.", .timestamp, true, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D9959.9", .timestamp, true, .{ .timestamp = 359940900000000 });
    try testParse("2000.01.01D9959.999999999", .timestamp, true, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D9959.9999999999", .timestamp, true, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D99:59", .timestamp, true, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D99:59.", .timestamp, true, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D99:59.9", .timestamp, true, .{ .timestamp = 359940900000000 });
    try testParse("2000.01.01D99:59.999999999", .timestamp, true, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D99:59.9999999999", .timestamp, true, .{ .timestamp = 359940999999999 });

    try testParse("2000.01.01D00000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00000.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00000.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99959", .timestamp, true, .{ .timestamp = 3599940000000000 });
    try testParse("2000.01.01D99959.", .timestamp, true, .{ .timestamp = 3599940000000000 });
    try testParse("2000.01.01D99959.9", .timestamp, true, .{ .timestamp = 3599940900000000 });
    try testParse("2000.01.01D99959.999999999", .timestamp, true, .{ .timestamp = 3599940999999999 });
    try testParse("2000.01.01D99959.9999999999", .timestamp, true, .{ .timestamp = 3599940999999999 });

    try testParse("2000.01.01D000000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D000000.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D000000.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D000000.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D000000.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00:00", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00:00.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00:00.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00:00:00.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00:00.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D995959", .timestamp, true, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D995959.", .timestamp, true, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D995959.9", .timestamp, true, .{ .timestamp = 359999900000000 });
    try testParse("2000.01.01D995959.999999999", .timestamp, true, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D995959.9999999999", .timestamp, true, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D99:59:59", .timestamp, true, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D99:59:59.", .timestamp, true, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D99:59:59.9", .timestamp, true, .{ .timestamp = 359999900000000 });
    try testParse("2000.01.01D99:59:59.999999999", .timestamp, true, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D99:59:59.9999999999", .timestamp, true, .{ .timestamp = 359999999999999 });

    try testParse("2000.01.01D0000000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000000.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000000.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D0000000.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0000000.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9995959", .timestamp, true, .{ .timestamp = 3599999000000000 });
    try testParse("2000.01.01D9995959.", .timestamp, true, .{ .timestamp = 3599999000000000 });
    try testParse("2000.01.01D9995959.9", .timestamp, true, .{ .timestamp = 3599999900000000 });
    try testParse("2000.01.01D9995959.999999999", .timestamp, true, .{ .timestamp = 3599999999999999 });
    try testParse("2000.01.01D9995959.9999999999", .timestamp, true, .{ .timestamp = 3599999999999999 });

    try testParse("2000.01.01D00000000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000000.", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000000.9", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00000000.999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00000000.9999999999", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99995959", .timestamp, true, .{ .timestamp = 35999999000000000 });
    try testParse("2000.01.01D99995959.", .timestamp, true, .{ .timestamp = 35999999000000000 });
    try testParse("2000.01.01D99995959.9", .timestamp, true, .{ .timestamp = 35999999900000000 });
    try testParse("2000.01.01D99995959.999999999", .timestamp, true, .{ .timestamp = 35999999999999999 });
    try testParse("2000.01.01D99995959.9999999999", .timestamp, true, .{ .timestamp = 35999999999999999 });

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testParse("2000.01.01D000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D000000000.", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D000000000.9", .timestamp, true, .{ .timestamp = -946684799100000000 });
    try testParse("2000.01.01D000000000.999999999", .timestamp, true, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D000000000.9999999999", .timestamp, true, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D999999999", .timestamp, true, .{ .timestamp = 53315199000000000 });
    try testParse("2000.01.01D999999999.", .timestamp, true, .{ .timestamp = 53315199000000000 });
    try testParse("2000.01.01D999999999.9", .timestamp, true, .{ .timestamp = 53315199900000000 });
    try testParse("2000.01.01D999999999.999999999", .timestamp, true, .{ .timestamp = 53315199999999999 });
    try testParse("2000.01.01D999999999.9999999999", .timestamp, true, .{ .timestamp = 53315199999999999 });

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testParse("2000.01.01D000000000000000", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D995959999999999", .timestamp, true, .{ .timestamp = 359999999999999 });

    try testParse("2000.01.01D0000000000000000000", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D0000000000000000000.", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D0000000000000000000.9", .timestamp, true, .{ .timestamp = -946684799100000000 });
    try testParse("2000.01.01D0000000000000000000.999999999", .timestamp, true, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D0000000000000000000.9999999999", .timestamp, true, .{ .timestamp = -946684799000000001 });

    try testParse("0Np", .timestamp, false, invalidCharacter(2));
    try testParse("0np", .timestamp, false, invalidCharacter(2));
    try testParse("0Wp", .timestamp, false, invalidCharacter(2));
    try testParse("0wp", .timestamp, false, invalidCharacter(2));

    try testParse("0p", .timestamp, false, invalidCharacter(1));
    try testParse("9p", .timestamp, false, invalidCharacter(1));

    try testParse("00p", .timestamp, false, invalidCharacter(2));
    try testParse("99p", .timestamp, false, invalidCharacter(2));

    try testParse("000p", .timestamp, false, invalidCharacter(3));
    try testParse("959p", .timestamp, false, invalidCharacter(3));

    try testParse("0000p", .timestamp, false, invalidCharacter(4));
    try testParse("9959p", .timestamp, false, invalidCharacter(4));

    try testParse("00000p", .timestamp, false, invalidCharacter(5));
    try testParse("99959p", .timestamp, false, overflow);

    try testParse("000000p", .timestamp, false, invalidCharacter(6));
    try testParse("995959p", .timestamp, false, overflow);

    try testParse("0000000p", .timestamp, false, invalidCharacter(7));
    try testParse("9995959p", .timestamp, false, overflow);

    try testParse("00000000p", .timestamp, false, invalidCharacter(8));
    try testParse("99995959p", .timestamp, false, invalidCharacter(8));

    try testParse("000000000p", .timestamp, false, invalidCharacter(9));
    try testParse("999999999p", .timestamp, false, invalidCharacter(9));

    try testParse("0000000000p", .timestamp, false, invalidCharacter(10));
    try testParse("9999999999p", .timestamp, false, invalidCharacter(10));

    try testParse("00000000000p", .timestamp, false, invalidCharacter(11));
    try testParse("99999999999p", .timestamp, false, invalidCharacter(11));

    try testParse("000000000000p", .timestamp, false, invalidCharacter(12));
    try testParse("999999999999p", .timestamp, false, invalidCharacter(12));

    try testParse("0000000000000p", .timestamp, false, invalidCharacter(13));
    try testParse("9999999999999p", .timestamp, false, invalidCharacter(13));

    try testParse("00000000000000p", .timestamp, false, invalidCharacter(14));
    try testParse("99999999999999p", .timestamp, false, overflow);

    try testParse("000000000000000p", .timestamp, false, invalidCharacter(15));
    try testParse("995959999999999p", .timestamp, false, invalidCharacter(15));

    try testParse("0000000000000000p", .timestamp, false, invalidCharacter(16));
    try testParse("9999999999999999p", .timestamp, false, invalidCharacter(16));

    try testParse("00000000000000000p", .timestamp, false, invalidCharacter(17));
    try testParse("99999999999999999p", .timestamp, false, invalidCharacter(17));

    try testParse("000000000000000000p", .timestamp, false, invalidCharacter(18));
    try testParse("999999999999999999p", .timestamp, false, invalidCharacter(18));

    try testParse("0000000000000000000p", .timestamp, false, invalidCharacter(19));
    try testParse("9223372036854775807p", .timestamp, false, invalidCharacter(19));

    try testParse("2000.01.01Dp", .timestamp, false, invalidCharacter(11));
    try testParse("2000.01.01D.p", .timestamp, false, invalidCharacter(12));
    try testParse("2000.01.01D.9p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });

    try testParse("2000.01.01D0p", .timestamp, false, invalidCharacter(12));
    try testParse("2000.01.01D0.p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D0.9p", .timestamp, false, invalidCharacter(14));
    try testParse("2000.01.01D0.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9p", .timestamp, false, invalidCharacter(12));
    try testParse("2000.01.01D9.p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D9.9p", .timestamp, false, invalidCharacter(14));
    try testParse("2000.01.01D9.999999999p", .timestamp, false, .{ .timestamp = 32400999999999 });
    try testParse("2000.01.01D9.9999999999p", .timestamp, false, .{ .timestamp = 32400999999999 });

    try testParse("2000.01.01D00p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D00.p", .timestamp, false, invalidCharacter(14));
    try testParse("2000.01.01D00.9p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D00.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D00:.p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D00:.9p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D00:.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D99.p", .timestamp, false, invalidCharacter(14));
    try testParse("2000.01.01D99.9p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D99.999999999p", .timestamp, false, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99.9999999999p", .timestamp, false, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99:p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D99:.p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D99:.9p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D99:.999999999p", .timestamp, false, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99:.9999999999p", .timestamp, false, .{ .timestamp = 356400999999999 });

    try testParse("2000.01.01D000p", .timestamp, false, invalidCharacter(14));
    try testParse("2000.01.01D000.p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D000.9p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D000.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D000.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D959p", .timestamp, false, invalidCharacter(14));
    try testParse("2000.01.01D959.p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D959.9p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D959.999999999p", .timestamp, false, .{ .timestamp = 35940999999999 });
    try testParse("2000.01.01D959.9999999999p", .timestamp, false, .{ .timestamp = 35940999999999 });

    try testParse("2000.01.01D0000p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D0000.p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D0000.9p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D0000.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0000.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D00:00.p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D00:00.9p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D00:00.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9959p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D9959.p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D9959.9p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D9959.999999999p", .timestamp, false, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D9959.9999999999p", .timestamp, false, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D99:59p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D99:59.p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D99:59.9p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D99:59.999999999p", .timestamp, false, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D99:59.9999999999p", .timestamp, false, .{ .timestamp = 359940999999999 });

    try testParse("2000.01.01D00000p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D00000.p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D00000.9p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D00000.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00000.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99959p", .timestamp, false, overflow);
    try testParse("2000.01.01D99959.p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D99959.9p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D99959.999999999p", .timestamp, false, .{ .timestamp = 3599940999999999 });
    try testParse("2000.01.01D99959.9999999999p", .timestamp, false, .{ .timestamp = 3599940999999999 });

    try testParse("2000.01.01D000000p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D000000.p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D000000.9p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D000000.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D000000.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00:00p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D00:00:00.p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D00:00:00.9p", .timestamp, false, invalidCharacter(21));
    try testParse("2000.01.01D00:00:00.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00:00.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D995959p", .timestamp, false, overflow);
    try testParse("2000.01.01D995959.p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D995959.9p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D995959.999999999p", .timestamp, false, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D995959.9999999999p", .timestamp, false, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D99:59:59p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D99:59:59.p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D99:59:59.9p", .timestamp, false, invalidCharacter(21));
    try testParse("2000.01.01D99:59:59.999999999p", .timestamp, false, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D99:59:59.9999999999p", .timestamp, false, .{ .timestamp = 359999999999999 });

    try testParse("2000.01.01D0000000p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D0000000.p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D0000000.9p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D0000000.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0000000.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9995959p", .timestamp, false, overflow);
    try testParse("2000.01.01D9995959.p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D9995959.9p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D9995959.999999999p", .timestamp, false, .{ .timestamp = 3599999999999999 });
    try testParse("2000.01.01D9995959.9999999999p", .timestamp, false, .{ .timestamp = 3599999999999999 });

    try testParse("2000.01.01D00000000p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D00000000.p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D00000000.9p", .timestamp, false, invalidCharacter(21));
    try testParse("2000.01.01D00000000.999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00000000.9999999999p", .timestamp, false, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99995959p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D99995959.p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D99995959.9p", .timestamp, false, invalidCharacter(21));
    try testParse("2000.01.01D99995959.999999999p", .timestamp, false, .{ .timestamp = 35999999999999999 });
    try testParse("2000.01.01D99995959.9999999999p", .timestamp, false, .{ .timestamp = 35999999999999999 });

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testParse("2000.01.01D000000000p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D000000000.p", .timestamp, false, invalidCharacter(21));
    try testParse("2000.01.01D000000000.9p", .timestamp, false, invalidCharacter(22));
    try testParse("2000.01.01D000000000.999999999p", .timestamp, false, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D000000000.9999999999p", .timestamp, false, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D999999999p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D999999999.p", .timestamp, false, invalidCharacter(21));
    try testParse("2000.01.01D999999999.9p", .timestamp, false, invalidCharacter(22));
    try testParse("2000.01.01D999999999.999999999p", .timestamp, false, .{ .timestamp = 53315199999999999 });
    try testParse("2000.01.01D999999999.9999999999p", .timestamp, false, .{ .timestamp = 53315199999999999 });

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testParse("2000.01.01D000000000000000p", .timestamp, false, invalidCharacter(26));
    try testParse("2000.01.01D995959999999999p", .timestamp, false, invalidCharacter(26));

    try testParse("2000.01.01D0000000000000000000p", .timestamp, false, invalidCharacter(30));
    try testParse("2000.01.01D0000000000000000000.p", .timestamp, false, invalidCharacter(31));
    try testParse("2000.01.01D0000000000000000000.9p", .timestamp, false, invalidCharacter(32));
    try testParse("2000.01.01D0000000000000000000.999999999p", .timestamp, false, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D0000000000000000000.9999999999p", .timestamp, false, .{ .timestamp = -946684799000000001 });

    try testParse("0Np", .timestamp, true, .{ .timestamp = null_long });
    try testParse("0np", .timestamp, true, .{ .timestamp = null_long });
    try testParse("0Wp", .timestamp, true, .{ .timestamp = inf_long });
    try testParse("0wp", .timestamp, true, .{ .timestamp = inf_long });

    try testParse("0p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("9p", .timestamp, true, .{ .timestamp = 32400000000000 });

    try testParse("00p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("99p", .timestamp, true, .{ .timestamp = 356400000000000 });

    try testParse("000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("959p", .timestamp, true, .{ .timestamp = 35940000000000 });

    try testParse("0000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("9959p", .timestamp, true, .{ .timestamp = 359940000000000 });

    try testParse("00000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("99959p", .timestamp, true, .{ .timestamp = 3599940000000000 });

    try testParse("000000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("995959p", .timestamp, true, .{ .timestamp = 359999000000000 });

    try testParse("0000000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("9995959p", .timestamp, true, .{ .timestamp = 3599999000000000 });

    try testParse("00000000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("99995959p", .timestamp, true, .{ .timestamp = 35999999000000000 });

    try testParse("000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("999999999p", .timestamp, true, .{ .timestamp = 53315199000000000 });

    try testParse("0000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("9999999999p", .timestamp, true, overflow);

    try testParse("00000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("99999999999p", .timestamp, true, overflow);

    try testParse("000000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("999999999999p", .timestamp, true, overflow);

    try testParse("0000000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("9999999999999p", .timestamp, true, overflow);

    try testParse("00000000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("99999999999999p", .timestamp, true, overflow);

    try testParse("000000000000000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("995959999999999p", .timestamp, true, .{ .timestamp = 359999999999999 });

    try testParse("0000000000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("9999999999999999p", .timestamp, true, overflow);

    try testParse("00000000000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("99999999999999999p", .timestamp, true, overflow);

    try testParse("000000000000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("999999999999999999p", .timestamp, true, overflow);

    try testParse("0000000000000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("9223372036854775807p", .timestamp, true, overflow);

    try testParse("2000.01.01Dp", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });

    try testParse("2000.01.01D0p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D0.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9p", .timestamp, true, .{ .timestamp = 32400000000000 });
    try testParse("2000.01.01D9.p", .timestamp, true, .{ .timestamp = 32400000000000 });
    try testParse("2000.01.01D9.9p", .timestamp, true, .{ .timestamp = 32400900000000 });
    try testParse("2000.01.01D9.999999999p", .timestamp, true, .{ .timestamp = 32400999999999 });
    try testParse("2000.01.01D9.9999999999p", .timestamp, true, .{ .timestamp = 32400999999999 });

    try testParse("2000.01.01D00p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00:.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99p", .timestamp, true, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99.p", .timestamp, true, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99.9p", .timestamp, true, .{ .timestamp = 356400900000000 });
    try testParse("2000.01.01D99.999999999p", .timestamp, true, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99.9999999999p", .timestamp, true, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99:p", .timestamp, true, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99:.p", .timestamp, true, .{ .timestamp = 356400000000000 });
    try testParse("2000.01.01D99:.9p", .timestamp, true, .{ .timestamp = 356400900000000 });
    try testParse("2000.01.01D99:.999999999p", .timestamp, true, .{ .timestamp = 356400999999999 });
    try testParse("2000.01.01D99:.9999999999p", .timestamp, true, .{ .timestamp = 356400999999999 });

    try testParse("2000.01.01D000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D000.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D000.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D000.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D000.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D959p", .timestamp, true, .{ .timestamp = 35940000000000 });
    try testParse("2000.01.01D959.p", .timestamp, true, .{ .timestamp = 35940000000000 });
    try testParse("2000.01.01D959.9p", .timestamp, true, .{ .timestamp = 35940900000000 });
    try testParse("2000.01.01D959.999999999p", .timestamp, true, .{ .timestamp = 35940999999999 });
    try testParse("2000.01.01D959.9999999999p", .timestamp, true, .{ .timestamp = 35940999999999 });

    try testParse("2000.01.01D0000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D0000.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0000.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00:00.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9959p", .timestamp, true, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D9959.p", .timestamp, true, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D9959.9p", .timestamp, true, .{ .timestamp = 359940900000000 });
    try testParse("2000.01.01D9959.999999999p", .timestamp, true, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D9959.9999999999p", .timestamp, true, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D99:59p", .timestamp, true, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D99:59.p", .timestamp, true, .{ .timestamp = 359940000000000 });
    try testParse("2000.01.01D99:59.9p", .timestamp, true, .{ .timestamp = 359940900000000 });
    try testParse("2000.01.01D99:59.999999999p", .timestamp, true, .{ .timestamp = 359940999999999 });
    try testParse("2000.01.01D99:59.9999999999p", .timestamp, true, .{ .timestamp = 359940999999999 });

    try testParse("2000.01.01D00000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00000.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00000.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99959p", .timestamp, true, .{ .timestamp = 3599940000000000 });
    try testParse("2000.01.01D99959.p", .timestamp, true, .{ .timestamp = 3599940000000000 });
    try testParse("2000.01.01D99959.9p", .timestamp, true, .{ .timestamp = 3599940900000000 });
    try testParse("2000.01.01D99959.999999999p", .timestamp, true, .{ .timestamp = 3599940999999999 });
    try testParse("2000.01.01D99959.9999999999p", .timestamp, true, .{ .timestamp = 3599940999999999 });

    try testParse("2000.01.01D000000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D000000.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D000000.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D000000.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D000000.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00:00p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00:00.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00:00:00.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00:00:00.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00:00:00.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D995959p", .timestamp, true, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D995959.p", .timestamp, true, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D995959.9p", .timestamp, true, .{ .timestamp = 359999900000000 });
    try testParse("2000.01.01D995959.999999999p", .timestamp, true, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D995959.9999999999p", .timestamp, true, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D99:59:59p", .timestamp, true, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D99:59:59.p", .timestamp, true, .{ .timestamp = 359999000000000 });
    try testParse("2000.01.01D99:59:59.9p", .timestamp, true, .{ .timestamp = 359999900000000 });
    try testParse("2000.01.01D99:59:59.999999999p", .timestamp, true, .{ .timestamp = 359999999999999 });
    try testParse("2000.01.01D99:59:59.9999999999p", .timestamp, true, .{ .timestamp = 359999999999999 });

    try testParse("2000.01.01D0000000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000000.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D0000000.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D0000000.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D0000000.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D9995959p", .timestamp, true, .{ .timestamp = 3599999000000000 });
    try testParse("2000.01.01D9995959.p", .timestamp, true, .{ .timestamp = 3599999000000000 });
    try testParse("2000.01.01D9995959.9p", .timestamp, true, .{ .timestamp = 3599999900000000 });
    try testParse("2000.01.01D9995959.999999999p", .timestamp, true, .{ .timestamp = 3599999999999999 });
    try testParse("2000.01.01D9995959.9999999999p", .timestamp, true, .{ .timestamp = 3599999999999999 });

    try testParse("2000.01.01D00000000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000000.p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D00000000.9p", .timestamp, true, .{ .timestamp = 900000000 });
    try testParse("2000.01.01D00000000.999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D00000000.9999999999p", .timestamp, true, .{ .timestamp = 999999999 });
    try testParse("2000.01.01D99995959p", .timestamp, true, .{ .timestamp = 35999999000000000 });
    try testParse("2000.01.01D99995959.p", .timestamp, true, .{ .timestamp = 35999999000000000 });
    try testParse("2000.01.01D99995959.9p", .timestamp, true, .{ .timestamp = 35999999900000000 });
    try testParse("2000.01.01D99995959.999999999p", .timestamp, true, .{ .timestamp = 35999999999999999 });
    try testParse("2000.01.01D99995959.9999999999p", .timestamp, true, .{ .timestamp = 35999999999999999 });

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testParse("2000.01.01D000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D000000000.p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D000000000.9p", .timestamp, true, .{ .timestamp = -946684799100000000 });
    try testParse("2000.01.01D000000000.999999999p", .timestamp, true, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D000000000.9999999999p", .timestamp, true, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D999999999p", .timestamp, true, .{ .timestamp = 53315199000000000 });
    try testParse("2000.01.01D999999999.p", .timestamp, true, .{ .timestamp = 53315199000000000 });
    try testParse("2000.01.01D999999999.9p", .timestamp, true, .{ .timestamp = 53315199900000000 });
    try testParse("2000.01.01D999999999.999999999p", .timestamp, true, .{ .timestamp = 53315199999999999 });
    try testParse("2000.01.01D999999999.9999999999p", .timestamp, true, .{ .timestamp = 53315199999999999 });

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testParse("2000.01.01D000000000000000p", .timestamp, true, .{ .timestamp = 0 });
    try testParse("2000.01.01D995959999999999p", .timestamp, true, .{ .timestamp = 359999999999999 });

    try testParse("2000.01.01D0000000000000000000p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D0000000000000000000.p", .timestamp, true, .{ .timestamp = -946684800000000000 });
    try testParse("2000.01.01D0000000000000000000.9p", .timestamp, true, .{ .timestamp = -946684799100000000 });
    try testParse("2000.01.01D0000000000000000000.999999999p", .timestamp, true, .{ .timestamp = -946684799000000001 });
    try testParse("2000.01.01D0000000000000000000.9999999999p", .timestamp, true, .{ .timestamp = -946684799000000001 });

    try testParse("060", .timestamp, false, overflow);
    try testParse("960", .timestamp, false, overflow);

    try testParse("0060", .timestamp, false, overflow);
    try testParse("9960", .timestamp, false, overflow);

    try testParse("00060", .timestamp, false, overflow);
    try testParse("99960", .timestamp, false, overflow);

    try testParse("000060", .timestamp, false, overflow);
    try testParse("995960", .timestamp, false, overflow);
    try testParse("006000", .timestamp, false, overflow);
    try testParse("996059", .timestamp, false, overflow);

    try testParse("0000060", .timestamp, false, overflow);
    try testParse("9995960", .timestamp, false, overflow);
    try testParse("0006000", .timestamp, false, overflow);
    try testParse("9996059", .timestamp, false, overflow);

    try testParse("00000060", .timestamp, false, overflow);
    try testParse("99995960", .timestamp, false, overflow);
    try testParse("00006000", .timestamp, false, overflow);
    try testParse("99996059", .timestamp, false, overflow);

    try testParse("000060000000000", .timestamp, false, overflow);
    try testParse("995960999999999", .timestamp, false, overflow);
    try testParse("006000000000000", .timestamp, false, overflow);
    try testParse("996059999999999", .timestamp, false, overflow);

    try testParse("9223372036854775808", .timestamp, false, overflow);

    try testParse("2000.01.01D0:", .timestamp, false, invalidCharacter(12));
    try testParse("2000.01.01D00:0", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D00:00:", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D00:00:0", .timestamp, false, invalidCharacter(13));

    try testParse("2000.01.01D060", .timestamp, false, overflow);
    try testParse("2000.01.01D060.", .timestamp, false, overflow);
    try testParse("2000.01.01D960", .timestamp, false, overflow);
    try testParse("2000.01.01D960.", .timestamp, false, overflow);

    try testParse("2000.01.01D0060", .timestamp, false, overflow);
    try testParse("2000.01.01D0060.", .timestamp, false, overflow);
    try testParse("2000.01.01D9960", .timestamp, false, overflow);
    try testParse("2000.01.01D9960.", .timestamp, false, overflow);

    try testParse("2000.01.01D00060", .timestamp, false, overflow);
    try testParse("2000.01.01D00060.", .timestamp, false, overflow);
    try testParse("2000.01.01D99960", .timestamp, false, overflow);
    try testParse("2000.01.01D99960.", .timestamp, false, overflow);

    try testParse("2000.01.01D000060", .timestamp, false, overflow);
    try testParse("2000.01.01D000060.", .timestamp, false, overflow);
    try testParse("2000.01.01D995960", .timestamp, false, overflow);
    try testParse("2000.01.01D995960.", .timestamp, false, overflow);
    try testParse("2000.01.01D006000", .timestamp, false, overflow);
    try testParse("2000.01.01D006000.", .timestamp, false, overflow);
    try testParse("2000.01.01D996059", .timestamp, false, overflow);
    try testParse("2000.01.01D996059.", .timestamp, false, overflow);

    try testParse("2000.01.01D0000060", .timestamp, false, overflow);
    try testParse("2000.01.01D0000060.", .timestamp, false, overflow);
    try testParse("2000.01.01D9995960", .timestamp, false, overflow);
    try testParse("2000.01.01D9995960.", .timestamp, false, overflow);
    try testParse("2000.01.01D0006000", .timestamp, false, overflow);
    try testParse("2000.01.01D0006000.", .timestamp, false, overflow);
    try testParse("2000.01.01D9996059", .timestamp, false, overflow);
    try testParse("2000.01.01D9996059.", .timestamp, false, overflow);

    try testParse("2000.01.01D00000060", .timestamp, false, overflow);
    try testParse("2000.01.01D00000060.", .timestamp, false, overflow);
    try testParse("2000.01.01D99995960", .timestamp, false, overflow);
    try testParse("2000.01.01D99995960.", .timestamp, false, overflow);
    try testParse("2000.01.01D00006000", .timestamp, false, overflow);
    try testParse("2000.01.01D00006000.", .timestamp, false, overflow);
    try testParse("2000.01.01D99996059", .timestamp, false, overflow);
    try testParse("2000.01.01D99996059.", .timestamp, false, overflow);

    // This one doesn't overflow with decimal points
    try testParse("2000.01.01D000060000000000", .timestamp, false, overflow);
    try testParse("2000.01.01D995960999999999", .timestamp, false, overflow);
    try testParse("2000.01.01D006000000000000", .timestamp, false, overflow);
    try testParse("2000.01.01D996059999999999", .timestamp, false, overflow);

    try testParse("2000.01.01D9223372036854775808", .timestamp, false, overflow);
    try testParse("2000.01.01D9223372036854775808.", .timestamp, false, overflow);

    try testParse("060", .timestamp, true, overflow);
    try testParse("960", .timestamp, true, overflow);

    try testParse("0060", .timestamp, true, overflow);
    try testParse("9960", .timestamp, true, overflow);

    try testParse("00060", .timestamp, true, overflow);
    try testParse("99960", .timestamp, true, overflow);

    try testParse("000060", .timestamp, true, overflow);
    try testParse("995960", .timestamp, true, overflow);
    try testParse("006000", .timestamp, true, overflow);
    try testParse("996059", .timestamp, true, overflow);

    try testParse("0000060", .timestamp, true, overflow);
    try testParse("9995960", .timestamp, true, overflow);
    try testParse("0006000", .timestamp, true, overflow);
    try testParse("9996059", .timestamp, true, overflow);

    try testParse("00000060", .timestamp, true, overflow);
    try testParse("99995960", .timestamp, true, overflow);
    try testParse("00006000", .timestamp, true, overflow);
    try testParse("99996059", .timestamp, true, overflow);

    try testParse("000060000000000", .timestamp, true, overflow);
    try testParse("995960999999999", .timestamp, true, overflow);
    try testParse("006000000000000", .timestamp, true, overflow);
    try testParse("996059999999999", .timestamp, true, overflow);

    try testParse("9223372036854775808", .timestamp, true, overflow);

    try testParse("2000.01.01D0:", .timestamp, true, invalidCharacter(12));
    try testParse("2000.01.01D00:0", .timestamp, true, invalidCharacter(13));
    try testParse("2000.01.01D00:00:", .timestamp, true, invalidCharacter(16));
    try testParse("2000.01.01D00:00:0", .timestamp, true, invalidCharacter(13));

    try testParse("2000.01.01D060", .timestamp, true, overflow);
    try testParse("2000.01.01D060.", .timestamp, true, overflow);
    try testParse("2000.01.01D960", .timestamp, true, overflow);
    try testParse("2000.01.01D960.", .timestamp, true, overflow);

    try testParse("2000.01.01D0060", .timestamp, true, overflow);
    try testParse("2000.01.01D0060.", .timestamp, true, overflow);
    try testParse("2000.01.01D9960", .timestamp, true, overflow);
    try testParse("2000.01.01D9960.", .timestamp, true, overflow);

    try testParse("2000.01.01D00060", .timestamp, true, overflow);
    try testParse("2000.01.01D00060.", .timestamp, true, overflow);
    try testParse("2000.01.01D99960", .timestamp, true, overflow);
    try testParse("2000.01.01D99960.", .timestamp, true, overflow);

    try testParse("2000.01.01D000060", .timestamp, true, overflow);
    try testParse("2000.01.01D000060.", .timestamp, true, overflow);
    try testParse("2000.01.01D995960", .timestamp, true, overflow);
    try testParse("2000.01.01D995960.", .timestamp, true, overflow);
    try testParse("2000.01.01D006000", .timestamp, true, overflow);
    try testParse("2000.01.01D006000.", .timestamp, true, overflow);
    try testParse("2000.01.01D996059", .timestamp, true, overflow);
    try testParse("2000.01.01D996059.", .timestamp, true, overflow);

    try testParse("2000.01.01D0000060", .timestamp, true, overflow);
    try testParse("2000.01.01D0000060.", .timestamp, true, overflow);
    try testParse("2000.01.01D9995960", .timestamp, true, overflow);
    try testParse("2000.01.01D9995960.", .timestamp, true, overflow);
    try testParse("2000.01.01D0006000", .timestamp, true, overflow);
    try testParse("2000.01.01D0006000.", .timestamp, true, overflow);
    try testParse("2000.01.01D9996059", .timestamp, true, overflow);
    try testParse("2000.01.01D9996059.", .timestamp, true, overflow);

    try testParse("2000.01.01D00000060", .timestamp, true, overflow);
    try testParse("2000.01.01D00000060.", .timestamp, true, overflow);
    try testParse("2000.01.01D99995960", .timestamp, true, overflow);
    try testParse("2000.01.01D99995960.", .timestamp, true, overflow);
    try testParse("2000.01.01D00006000", .timestamp, true, overflow);
    try testParse("2000.01.01D00006000.", .timestamp, true, overflow);
    try testParse("2000.01.01D99996059", .timestamp, true, overflow);
    try testParse("2000.01.01D99996059.", .timestamp, true, overflow);

    // This one doesn't overflow with decimal points
    try testParse("2000.01.01D000060000000000", .timestamp, true, overflow);
    try testParse("2000.01.01D995960999999999", .timestamp, true, overflow);
    try testParse("2000.01.01D006000000000000", .timestamp, true, overflow);
    try testParse("2000.01.01D996059999999999", .timestamp, true, overflow);

    try testParse("2000.01.01D9223372036854775808", .timestamp, true, overflow);
    try testParse("2000.01.01D9223372036854775808.", .timestamp, true, overflow);

    try testParse("060p", .timestamp, false, invalidCharacter(3));
    try testParse("960p", .timestamp, false, invalidCharacter(3));

    try testParse("0060p", .timestamp, false, invalidCharacter(4));
    try testParse("9960p", .timestamp, false, invalidCharacter(4));

    try testParse("00060p", .timestamp, false, invalidCharacter(5));
    try testParse("99960p", .timestamp, false, overflow);

    try testParse("000060p", .timestamp, false, invalidCharacter(6));
    try testParse("995960p", .timestamp, false, overflow);
    try testParse("006000p", .timestamp, false, invalidCharacter(6));
    try testParse("996059p", .timestamp, false, invalidCharacter(6));

    try testParse("0000060p", .timestamp, false, invalidCharacter(7));
    try testParse("9995960p", .timestamp, false, overflow);
    try testParse("0006000p", .timestamp, false, invalidCharacter(7));
    try testParse("9996059p", .timestamp, false, invalidCharacter(7));

    try testParse("00000060p", .timestamp, false, invalidCharacter(8));
    try testParse("99995960p", .timestamp, false, invalidCharacter(8));
    try testParse("00006000p", .timestamp, false, invalidCharacter(8));
    try testParse("99996059p", .timestamp, false, invalidCharacter(8));

    try testParse("000060000000000p", .timestamp, false, invalidCharacter(15));
    try testParse("995960999999999p", .timestamp, false, invalidCharacter(15));
    try testParse("006000000000000p", .timestamp, false, invalidCharacter(15));
    try testParse("996059999999999p", .timestamp, false, invalidCharacter(15));

    try testParse("9223372036854775808p", .timestamp, false, overflow);

    try testParse("2000.01.01D0:p", .timestamp, false, invalidCharacter(12));
    try testParse("2000.01.01D00:0p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D00:00:p", .timestamp, false, invalidCharacter(13));
    try testParse("2000.01.01D00:00:0p", .timestamp, false, invalidCharacter(18));

    try testParse("2000.01.01D060p", .timestamp, false, invalidCharacter(14));
    try testParse("2000.01.01D060.p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D960p", .timestamp, false, invalidCharacter(14));
    try testParse("2000.01.01D960.p", .timestamp, false, invalidCharacter(15));

    try testParse("2000.01.01D0060p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D0060.p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D9960p", .timestamp, false, invalidCharacter(15));
    try testParse("2000.01.01D9960.p", .timestamp, false, invalidCharacter(16));

    try testParse("2000.01.01D00060p", .timestamp, false, invalidCharacter(16));
    try testParse("2000.01.01D00060.p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D99960p", .timestamp, false, overflow);
    try testParse("2000.01.01D99960.p", .timestamp, false, invalidCharacter(17));

    try testParse("2000.01.01D000060p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D000060.p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D995960p", .timestamp, false, overflow);
    try testParse("2000.01.01D995960.p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D006000p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D006000.p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D996059p", .timestamp, false, invalidCharacter(17));
    try testParse("2000.01.01D996059.p", .timestamp, false, invalidCharacter(18));

    try testParse("2000.01.01D0000060p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D0000060.p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D9995960p", .timestamp, false, overflow);
    try testParse("2000.01.01D9995960.p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D0006000p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D0006000.p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D9996059p", .timestamp, false, invalidCharacter(18));
    try testParse("2000.01.01D9996059.p", .timestamp, false, invalidCharacter(19));

    try testParse("2000.01.01D00000060p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D00000060.p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D99995960p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D99995960.p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D00006000p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D00006000.p", .timestamp, false, invalidCharacter(20));
    try testParse("2000.01.01D99996059p", .timestamp, false, invalidCharacter(19));
    try testParse("2000.01.01D99996059.p", .timestamp, false, invalidCharacter(20));

    // This one doesn't overflow with decimal points
    try testParse("2000.01.01D000060000000000p", .timestamp, false, invalidCharacter(26));
    try testParse("2000.01.01D995960999999999p", .timestamp, false, invalidCharacter(26));
    try testParse("2000.01.01D006000000000000p", .timestamp, false, invalidCharacter(26));
    try testParse("2000.01.01D996059999999999p", .timestamp, false, invalidCharacter(26));

    try testParse("2000.01.01D9223372036854775808p", .timestamp, false, overflow);
    try testParse("2000.01.01D9223372036854775808.p", .timestamp, false, invalidCharacter(31));

    try testParse("060p", .timestamp, true, overflow);
    try testParse("960p", .timestamp, true, overflow);

    try testParse("0060p", .timestamp, true, overflow);
    try testParse("9960p", .timestamp, true, overflow);

    try testParse("00060p", .timestamp, true, overflow);
    try testParse("99960p", .timestamp, true, overflow);

    try testParse("000060p", .timestamp, true, overflow);
    try testParse("995960p", .timestamp, true, overflow);
    try testParse("006000p", .timestamp, true, overflow);
    try testParse("996059p", .timestamp, true, overflow);

    try testParse("0000060p", .timestamp, true, overflow);
    try testParse("9995960p", .timestamp, true, overflow);
    try testParse("0006000p", .timestamp, true, overflow);
    try testParse("9996059p", .timestamp, true, overflow);

    try testParse("00000060p", .timestamp, true, overflow);
    try testParse("99995960p", .timestamp, true, overflow);
    try testParse("00006000p", .timestamp, true, overflow);
    try testParse("99996059p", .timestamp, true, overflow);

    try testParse("000060000000000p", .timestamp, true, overflow);
    try testParse("995960999999999p", .timestamp, true, overflow);
    try testParse("006000000000000p", .timestamp, true, overflow);
    try testParse("996059999999999p", .timestamp, true, overflow);

    try testParse("9223372036854775808p", .timestamp, true, overflow);

    try testParse("2000.01.01D0:p", .timestamp, true, invalidCharacter(12));
    try testParse("2000.01.01D00:0p", .timestamp, true, invalidCharacter(13));
    try testParse("2000.01.01D00:00:p", .timestamp, true, invalidCharacter(16));
    try testParse("2000.01.01D00:00:0p", .timestamp, true, invalidCharacter(13));

    try testParse("2000.01.01D060p", .timestamp, true, overflow);
    try testParse("2000.01.01D060.p", .timestamp, true, overflow);
    try testParse("2000.01.01D960p", .timestamp, true, overflow);
    try testParse("2000.01.01D960.p", .timestamp, true, overflow);

    try testParse("2000.01.01D0060p", .timestamp, true, overflow);
    try testParse("2000.01.01D0060.p", .timestamp, true, overflow);
    try testParse("2000.01.01D9960p", .timestamp, true, overflow);
    try testParse("2000.01.01D9960.p", .timestamp, true, overflow);

    try testParse("2000.01.01D00060p", .timestamp, true, overflow);
    try testParse("2000.01.01D00060.p", .timestamp, true, overflow);
    try testParse("2000.01.01D99960p", .timestamp, true, overflow);
    try testParse("2000.01.01D99960.p", .timestamp, true, overflow);

    try testParse("2000.01.01D000060p", .timestamp, true, overflow);
    try testParse("2000.01.01D000060.p", .timestamp, true, overflow);
    try testParse("2000.01.01D995960p", .timestamp, true, overflow);
    try testParse("2000.01.01D995960.p", .timestamp, true, overflow);
    try testParse("2000.01.01D006000p", .timestamp, true, overflow);
    try testParse("2000.01.01D006000.p", .timestamp, true, overflow);
    try testParse("2000.01.01D996059p", .timestamp, true, overflow);
    try testParse("2000.01.01D996059.p", .timestamp, true, overflow);

    try testParse("2000.01.01D0000060p", .timestamp, true, overflow);
    try testParse("2000.01.01D0000060.p", .timestamp, true, overflow);
    try testParse("2000.01.01D9995960p", .timestamp, true, overflow);
    try testParse("2000.01.01D9995960.p", .timestamp, true, overflow);
    try testParse("2000.01.01D0006000p", .timestamp, true, overflow);
    try testParse("2000.01.01D0006000.p", .timestamp, true, overflow);
    try testParse("2000.01.01D9996059p", .timestamp, true, overflow);
    try testParse("2000.01.01D9996059.p", .timestamp, true, overflow);

    try testParse("2000.01.01D00000060p", .timestamp, true, overflow);
    try testParse("2000.01.01D00000060.p", .timestamp, true, overflow);
    try testParse("2000.01.01D99995960p", .timestamp, true, overflow);
    try testParse("2000.01.01D99995960.p", .timestamp, true, overflow);
    try testParse("2000.01.01D00006000p", .timestamp, true, overflow);
    try testParse("2000.01.01D00006000.p", .timestamp, true, overflow);
    try testParse("2000.01.01D99996059p", .timestamp, true, overflow);
    try testParse("2000.01.01D99996059.p", .timestamp, true, overflow);

    // This one doesn't overflow with decimal points
    try testParse("2000.01.01D000060000000000p", .timestamp, true, overflow);
    try testParse("2000.01.01D995960999999999p", .timestamp, true, overflow);
    try testParse("2000.01.01D006000000000000p", .timestamp, true, overflow);
    try testParse("2000.01.01D996059999999999p", .timestamp, true, overflow);

    try testParse("2000.01.01D9223372036854775808p", .timestamp, true, overflow);
    try testParse("2000.01.01D9223372036854775808.p", .timestamp, true, overflow);
}
