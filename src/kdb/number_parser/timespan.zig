const std = @import("std");

const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;

const null_long = common.null_long;
const inf_long = common.inf_long;

const epoch_nanoseconds = common.epoch_nanoseconds;

pub fn parseTimespan(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .timespan = null_long },
        'W', 'w' => return .{ .timespan = inf_long },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'n' => .{ .timespan = null_long },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'n' => .{ .timespan = inf_long },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'n') bytes[0 .. bytes.len - 1] else bytes;
    if (std.mem.indexOfScalar(u8, slice, 'D')) |d_index| {
        const days_value = switch (parseSlice(i64, slice[0..d_index])) {
            .overflow => return .{ .failure = .overflow },
            .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
            .int => |int| int,
        };
        var content: []const u8 = slice[d_index + 1 ..];
        const offset = d_index + 1;

        var nanosecond_value: i64 = 0;
        if (std.mem.indexOfScalar(u8, content, '.')) |dot_index| {
            for (content[dot_index + 1 ..], 0..) |c, i| {
                if (i == 9) break;
                const digit = switch (c) {
                    '0'...'9' => c - '0',
                    else => return .{ .failure = .{ .invalid_character = i + offset + dot_index + 1 } },
                };
                nanosecond_value += digit * std.math.pow(i64, 10, 8 - @as(i32, @intCast(i)));
            }
            content = content[0..dot_index];
        }

        switch (content.len) {
            0 => {
                const nanoseconds = days_value * std.time.ns_per_day + nanosecond_value;
                return .{ .timespan = nanoseconds };
            },
            1, 2 => {
                const hour_value = switch (parseSlice(i64, content)) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const nanoseconds = days_value * std.time.ns_per_day + hour_value * std.time.ns_per_hour +
                    nanosecond_value;
                return .{ .timespan = nanoseconds };
            },
            3, 5 => if (content[2] == ':') {
                const hour_value = switch (parseSlice(i64, content[0..2])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const minute_value = switch (parseSlice(i64, content[3..])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + 3,
                    } },
                    .int => |int| int,
                };
                if (minute_value > 59) return .{ .failure = .overflow };
                const nanoseconds = days_value * std.time.ns_per_day + hour_value * std.time.ns_per_hour +
                    minute_value * std.time.ns_per_min + nanosecond_value;
                return .{ .timespan = nanoseconds };
            } else {
                const hour_value = switch (parseSlice(i64, content[0 .. content.len - 2])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const minute_value = switch (parseSlice(i64, content[content.len - 2 ..])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 2,
                    } },
                    .int => |int| int,
                };
                if (minute_value > 59) return .{ .failure = .overflow };
                const nanoseconds = days_value * std.time.ns_per_day + hour_value * std.time.ns_per_hour +
                    minute_value * std.time.ns_per_min + nanosecond_value;
                return .{ .timespan = nanoseconds };
            },
            4 => {
                const hour_value = switch (parseSlice(i64, content[0 .. content.len - 2])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const minute_value = switch (parseSlice(i64, content[content.len - 2 ..])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 2,
                    } },
                    .int => |int| int,
                };
                if (minute_value > 59) return .{ .failure = .overflow };
                const nanoseconds = days_value * std.time.ns_per_day + hour_value * std.time.ns_per_hour +
                    minute_value * std.time.ns_per_min + nanosecond_value;
                return .{ .timespan = nanoseconds };
            },
            6 => if (content[2] == ':') {
                if (content[5] == ':') return .{ .failure = .{ .invalid_character = offset + 5 } };

                const hour_value = switch (parseSlice(i64, content[0..2])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const second_value = switch (parseSlice(i64, content[4..])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + 4,
                    } },
                    .int => |int| int,
                };
                if (second_value > 59) return .{ .failure = .overflow };
                const nanoseconds = days_value * std.time.ns_per_day + hour_value * std.time.ns_per_hour +
                    second_value * std.time.ns_per_s + nanosecond_value;
                return .{ .timespan = nanoseconds };
            } else {
                const hour_value = switch (parseSlice(i64, content[0 .. content.len - 4])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const minute_value = switch (parseSlice(
                    i64,
                    content[content.len - 4 .. content.len - 2],
                )) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 4,
                    } },
                    .int => |int| int,
                };
                if (minute_value > 59) return .{ .failure = .overflow };
                const second_value = switch (parseSlice(i64, content[content.len - 2 ..])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 2,
                    } },
                    .int => |int| int,
                };
                if (second_value > 59) return .{ .failure = .overflow };
                const nanoseconds = days_value * std.time.ns_per_day + hour_value * std.time.ns_per_hour +
                    minute_value * std.time.ns_per_min + second_value * std.time.ns_per_s + nanosecond_value;
                return .{ .timespan = nanoseconds };
            },
            7 => {
                if (content[2] == ':') return .{ .failure = .{ .invalid_character = offset + 2 } };
                if (content[5] == ':') return .{ .failure = .{ .invalid_character = offset + 5 } };

                const hour_value = switch (parseSlice(i64, content[0 .. content.len - 4])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const minute_value = switch (parseSlice(
                    i64,
                    content[content.len - 4 .. content.len - 2],
                )) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 4,
                    } },
                    .int => |int| int,
                };
                if (minute_value > 59) return .{ .failure = .overflow };
                const second_value = switch (parseSlice(i64, content[content.len - 2 ..])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 2,
                    } },
                    .int => |int| int,
                };
                if (second_value > 59) return .{ .failure = .overflow };
                const nanoseconds = days_value * std.time.ns_per_day + hour_value * std.time.ns_per_hour +
                    minute_value * std.time.ns_per_min + second_value * std.time.ns_per_s + nanosecond_value;
                return .{ .timespan = nanoseconds };
            },
            8 => if (content[2] == ':' and content[5] == ':') {
                const hour_value = switch (parseSlice(i64, content[0..2])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                    .int => |int| int,
                };
                const minute_value = switch (parseSlice(
                    i64,
                    content[3..5],
                )) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + 3,
                    } },
                    .int => |int| int,
                };
                if (minute_value > 59) return .{ .failure = .overflow };
                const second_value = switch (parseSlice(i64, content[6..])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + 6,
                    } },
                    .int => |int| int,
                };
                if (second_value > 59) return .{ .failure = .overflow };
                const nanoseconds = days_value * std.time.ns_per_day + hour_value * std.time.ns_per_hour +
                    minute_value * std.time.ns_per_min + second_value * std.time.ns_per_s + nanosecond_value;
                return .{ .timespan = nanoseconds };
            } else {
                const hour_value = switch (parseSlice(i64, content[0 .. content.len - 4])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const minute_value = switch (parseSlice(
                    i64,
                    content[content.len - 4 .. content.len - 2],
                )) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 4,
                    } },
                    .int => |int| int,
                };
                if (minute_value > 59) return .{ .failure = .overflow };
                const second_value = switch (parseSlice(i64, content[content.len - 2 ..])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 2,
                    } },
                    .int => |int| int,
                };
                if (second_value > 59) return .{ .failure = .overflow };
                const nanoseconds = days_value * std.time.ns_per_day + hour_value * std.time.ns_per_hour +
                    minute_value * std.time.ns_per_min + second_value * std.time.ns_per_s + nanosecond_value;
                return .{ .timespan = nanoseconds };
            },
            15 => {
                const hour_value = switch (parseSlice(i64, content[0 .. content.len - 13])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const minute_value = switch (parseSlice(
                    i64,
                    content[content.len - 13 .. content.len - 11],
                )) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 13,
                    } },
                    .int => |int| int,
                };
                if (minute_value > 59) return .{ .failure = .overflow };
                const second_value = switch (parseSlice(
                    i64,
                    content[content.len - 11 .. content.len - 9],
                )) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 11,
                    } },
                    .int => |int| int,
                };
                if (second_value > 59) return .{ .failure = .overflow };
                nanosecond_value += switch (parseSlice(i64, content[content.len - 9 ..])) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset + content.len - 9,
                    } },
                    .int => |int| int,
                };
                const nanoseconds = hour_value * std.time.ns_per_hour + minute_value * std.time.ns_per_min +
                    second_value * std.time.ns_per_s + nanosecond_value;
                return .{ .timespan = nanoseconds };
            },
            else => {
                const second_value = switch (parseSlice(i64, content)) {
                    .overflow => return .{ .failure = .overflow },
                    .invalid_character => |i| return .{ .failure = .{
                        .invalid_character = i + offset,
                    } },
                    .int => |int| int,
                };
                const res = @mulWithOverflow(second_value, std.time.ns_per_s);
                if (res[1] != 0) return .{ .failure = .overflow };
                const nanoseconds = days_value * std.time.ns_per_day + epoch_nanoseconds + res[0] + nanosecond_value;
                return .{ .timespan = nanoseconds };
            },
        }
    }

    switch (slice.len) {
        0 => unreachable,
        1, 2 => {
            const hour_value = switch (parseSlice(i64, slice)) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const nanoseconds = hour_value * std.time.ns_per_hour;
            return .{ .timespan = nanoseconds };
        },
        3, 4, 5 => {
            const hour_value = switch (parseSlice(i64, slice[0 .. slice.len - 2])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const minute_value = switch (parseSlice(i64, slice[slice.len - 2 ..])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 2,
                } },
                .int => |int| int,
            };
            if (minute_value > 59) return .{ .failure = .overflow };
            const nanoseconds = hour_value * std.time.ns_per_hour + minute_value * std.time.ns_per_min;
            return .{ .timespan = nanoseconds };
        },
        6, 7, 8 => {
            const hour_value = switch (parseSlice(i64, slice[0 .. slice.len - 4])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const minute_value = switch (parseSlice(i64, slice[slice.len - 4 .. slice.len - 2])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 4,
                } },
                .int => |int| int,
            };
            if (minute_value > 59) return .{ .failure = .overflow };
            const second_value = switch (parseSlice(i64, slice[slice.len - 2 ..])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 2,
                } },
                .int => |int| int,
            };
            if (second_value > 59) return .{ .failure = .overflow };
            const nanoseconds = hour_value * std.time.ns_per_hour + minute_value * std.time.ns_per_min +
                second_value * std.time.ns_per_s;
            return .{ .timespan = nanoseconds };
        },
        15 => {
            const hour_value = switch (parseSlice(i64, slice[0 .. slice.len - 13])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const minute_value = switch (parseSlice(
                i64,
                slice[slice.len - 13 .. slice.len - 11],
            )) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 13,
                } },
                .int => |int| int,
            };
            if (minute_value > 59) return .{ .failure = .overflow };
            const second_value = switch (parseSlice(
                i64,
                slice[slice.len - 11 .. slice.len - 9],
            )) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 11,
                } },
                .int => |int| int,
            };
            if (second_value > 59) return .{ .failure = .overflow };
            const nanosecond_value = switch (parseSlice(i64, slice[slice.len - 9 ..])) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{
                    .invalid_character = i + slice.len - 9,
                } },
                .int => |int| int,
            };
            const nanoseconds = hour_value * std.time.ns_per_hour + minute_value * std.time.ns_per_min +
                second_value * std.time.ns_per_s + nanosecond_value;
            return .{ .timespan = nanoseconds };
        },
        else => {
            const second_value = switch (parseSlice(i64, slice)) {
                .overflow => return .{ .failure = .overflow },
                .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
                .int => |int| int,
            };
            const res = @mulWithOverflow(second_value, std.time.ns_per_s);
            if (res[1] != 0) return .{ .failure = .overflow };
            const nanoseconds = epoch_nanoseconds + res[0];
            return .{ .timespan = nanoseconds };
        },
    }
}

test "parse number literal - timespan" {
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
    try testParse("0N", .timespan, false, .{ .timespan = null_long });
    try testParse("0n", .timespan, false, .{ .timespan = null_long });
    try testParse("0W", .timespan, false, .{ .timespan = inf_long });
    try testParse("0w", .timespan, false, .{ .timespan = inf_long });

    try testParse("9", .timespan, false, .{ .timespan = 32400000000000 });

    try testParse("00", .timespan, false, .{ .timespan = 0 });
    try testParse("99", .timespan, false, .{ .timespan = 356400000000000 });

    try testParse("000", .timespan, false, .{ .timespan = 0 });
    try testParse("959", .timespan, false, .{ .timespan = 35940000000000 });

    try testParse("0000", .timespan, false, .{ .timespan = 0 });
    try testParse("9959", .timespan, false, .{ .timespan = 359940000000000 });

    try testParse("00000", .timespan, false, .{ .timespan = 0 });
    try testParse("99959", .timespan, false, .{ .timespan = 3599940000000000 });

    try testParse("000000", .timespan, false, .{ .timespan = 0 });
    try testParse("995959", .timespan, false, .{ .timespan = 359999000000000 });

    try testParse("0000000", .timespan, false, .{ .timespan = 0 });
    try testParse("9995959", .timespan, false, .{ .timespan = 3599999000000000 });

    try testParse("00000000", .timespan, false, .{ .timespan = 0 });
    try testParse("99995959", .timespan, false, .{ .timespan = 35999999000000000 });

    try testParse("000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("999999999", .timespan, false, .{ .timespan = 53315199000000000 });

    try testParse("0000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("9999999999", .timespan, false, overflow);

    try testParse("00000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("99999999999", .timespan, false, overflow);

    try testParse("000000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("999999999999", .timespan, false, overflow);

    try testParse("0000000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("9999999999999", .timespan, false, overflow);

    try testParse("00000000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("99999999999999", .timespan, false, overflow);

    try testParse("000000000000000", .timespan, false, .{ .timespan = 0 });
    try testParse("995959999999999", .timespan, false, .{ .timespan = 359999999999999 });

    try testParse("0000000000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("9999999999999999", .timespan, false, overflow);

    try testParse("00000000000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("99999999999999999", .timespan, false, overflow);

    try testParse("000000000000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("999999999999999999", .timespan, false, overflow);

    try testParse("0000000000000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("9223372036854775807", .timespan, false, overflow);

    try testParse("0D", .timespan, false, .{ .timespan = 0 });
    try testParse("0D.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("9D", .timespan, false, .{ .timespan = 777600000000000 });
    try testParse("9D.", .timespan, false, .{ .timespan = 777600000000000 });
    try testParse("9D.9", .timespan, false, .{ .timespan = 777600900000000 });
    try testParse("9D.999999999", .timespan, false, .{ .timespan = 777600999999999 });
    try testParse("9D.9999999999", .timespan, false, .{ .timespan = 777600999999999 });

    try testParse("0D0", .timespan, false, .{ .timespan = 0 });
    try testParse("0D0.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D0.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D0.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D0.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D9", .timespan, false, .{ .timespan = 32400000000000 });
    try testParse("0D9.", .timespan, false, .{ .timespan = 32400000000000 });
    try testParse("0D9.9", .timespan, false, .{ .timespan = 32400900000000 });
    try testParse("0D9.999999999", .timespan, false, .{ .timespan = 32400999999999 });
    try testParse("0D9.9999999999", .timespan, false, .{ .timespan = 32400999999999 });

    try testParse("0D00", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D00.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00:.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00:.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D00:.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D99", .timespan, false, .{ .timespan = 356400000000000 });
    try testParse("0D99.", .timespan, false, .{ .timespan = 356400000000000 });
    try testParse("0D99.9", .timespan, false, .{ .timespan = 356400900000000 });
    try testParse("0D99.999999999", .timespan, false, .{ .timespan = 356400999999999 });
    try testParse("0D99.9999999999", .timespan, false, .{ .timespan = 356400999999999 });
    try testParse("0D99:", .timespan, false, .{ .timespan = 356400000000000 });
    try testParse("0D99:.", .timespan, false, .{ .timespan = 356400000000000 });
    try testParse("0D99:.9", .timespan, false, .{ .timespan = 356400900000000 });
    try testParse("0D99:.999999999", .timespan, false, .{ .timespan = 356400999999999 });
    try testParse("0D99:.9999999999", .timespan, false, .{ .timespan = 356400999999999 });

    try testParse("0D000", .timespan, false, .{ .timespan = 0 });
    try testParse("0D000.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D000.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D000.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D000.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D959", .timespan, false, .{ .timespan = 35940000000000 });
    try testParse("0D959.", .timespan, false, .{ .timespan = 35940000000000 });
    try testParse("0D959.9", .timespan, false, .{ .timespan = 35940900000000 });
    try testParse("0D959.999999999", .timespan, false, .{ .timespan = 35940999999999 });
    try testParse("0D959.9999999999", .timespan, false, .{ .timespan = 35940999999999 });

    try testParse("0D0000", .timespan, false, .{ .timespan = 0 });
    try testParse("0D0000.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D0000.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D0000.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D0000.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:00", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00:00.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00:00.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D00:00.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:00.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D9959", .timespan, false, .{ .timespan = 359940000000000 });
    try testParse("0D9959.", .timespan, false, .{ .timespan = 359940000000000 });
    try testParse("0D9959.9", .timespan, false, .{ .timespan = 359940900000000 });
    try testParse("0D9959.999999999", .timespan, false, .{ .timespan = 359940999999999 });
    try testParse("0D9959.9999999999", .timespan, false, .{ .timespan = 359940999999999 });
    try testParse("0D99:59", .timespan, false, .{ .timespan = 359940000000000 });
    try testParse("0D99:59.", .timespan, false, .{ .timespan = 359940000000000 });
    try testParse("0D99:59.9", .timespan, false, .{ .timespan = 359940900000000 });
    try testParse("0D99:59.999999999", .timespan, false, .{ .timespan = 359940999999999 });
    try testParse("0D99:59.9999999999", .timespan, false, .{ .timespan = 359940999999999 });

    try testParse("0D00000", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00000.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00000.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D00000.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00000.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D99959", .timespan, false, .{ .timespan = 3599940000000000 });
    try testParse("0D99959.", .timespan, false, .{ .timespan = 3599940000000000 });
    try testParse("0D99959.9", .timespan, false, .{ .timespan = 3599940900000000 });
    try testParse("0D99959.999999999", .timespan, false, .{ .timespan = 3599940999999999 });
    try testParse("0D99959.9999999999", .timespan, false, .{ .timespan = 3599940999999999 });

    try testParse("0D000000", .timespan, false, .{ .timespan = 0 });
    try testParse("0D000000.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D000000.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D000000.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D000000.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:00:00", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00:00:00.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00:00:00.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D00:00:00.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:00:00.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D995959", .timespan, false, .{ .timespan = 359999000000000 });
    try testParse("0D995959.", .timespan, false, .{ .timespan = 359999000000000 });
    try testParse("0D995959.9", .timespan, false, .{ .timespan = 359999900000000 });
    try testParse("0D995959.999999999", .timespan, false, .{ .timespan = 359999999999999 });
    try testParse("0D995959.9999999999", .timespan, false, .{ .timespan = 359999999999999 });
    try testParse("0D99:959", .timespan, false, .{ .timespan = 356459000000000 });
    try testParse("0D99:959.", .timespan, false, .{ .timespan = 356459000000000 });
    try testParse("0D99:959.9", .timespan, false, .{ .timespan = 356459900000000 });
    try testParse("0D99:959.999999999", .timespan, false, .{ .timespan = 356459999999999 });
    try testParse("0D99:959.9999999999", .timespan, false, .{ .timespan = 356459999999999 });
    try testParse("0D995:59", .timespan, false, invalidCharacter(5));
    try testParse("0D995:59.", .timespan, false, invalidCharacter(5));
    try testParse("0D995:59.9", .timespan, false, invalidCharacter(5));
    try testParse("0D995:59.999999999", .timespan, false, invalidCharacter(5));
    try testParse("0D995:59.9999999999", .timespan, false, invalidCharacter(5));
    try testParse("0D99::59", .timespan, false, .{ .timespan = 356459000000000 });
    try testParse("0D99::59.", .timespan, false, .{ .timespan = 356459000000000 });
    try testParse("0D99::59.9", .timespan, false, .{ .timespan = 356459900000000 });
    try testParse("0D99::59.999999999", .timespan, false, .{ .timespan = 356459999999999 });
    try testParse("0D99::59.9999999999", .timespan, false, .{ .timespan = 356459999999999 });
    try testParse("0D99:59:59", .timespan, false, .{ .timespan = 359999000000000 });
    try testParse("0D99:59:59.", .timespan, false, .{ .timespan = 359999000000000 });
    try testParse("0D99:59:59.9", .timespan, false, .{ .timespan = 359999900000000 });
    try testParse("0D99:59:59.999999999", .timespan, false, .{ .timespan = 359999999999999 });
    try testParse("0D99:59:59.9999999999", .timespan, false, .{ .timespan = 359999999999999 });

    try testParse("0D0000000", .timespan, false, .{ .timespan = 0 });
    try testParse("0D0000000.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D0000000.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D0000000.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D0000000.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D9995959", .timespan, false, .{ .timespan = 3599999000000000 });
    try testParse("0D9995959.", .timespan, false, .{ .timespan = 3599999000000000 });
    try testParse("0D9995959.9", .timespan, false, .{ .timespan = 3599999900000000 });
    try testParse("0D9995959.999999999", .timespan, false, .{ .timespan = 3599999999999999 });
    try testParse("0D9995959.9999999999", .timespan, false, .{ .timespan = 3599999999999999 });

    try testParse("0D00000000", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00000000.", .timespan, false, .{ .timespan = 0 });
    try testParse("0D00000000.9", .timespan, false, .{ .timespan = 900000000 });
    try testParse("0D00000000.999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00000000.9999999999", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D99995959", .timespan, false, .{ .timespan = 35999999000000000 });
    try testParse("0D99995959.", .timespan, false, .{ .timespan = 35999999000000000 });
    try testParse("0D99995959.9", .timespan, false, .{ .timespan = 35999999900000000 });
    try testParse("0D99995959.999999999", .timespan, false, .{ .timespan = 35999999999999999 });
    try testParse("0D99995959.9999999999", .timespan, false, .{ .timespan = 35999999999999999 });

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testParse("0D000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("0D000000000.", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("0D000000000.9", .timespan, false, .{ .timespan = -946684799100000000 });
    try testParse("0D000000000.999999999", .timespan, false, .{ .timespan = -946684799000000001 });
    try testParse("0D000000000.9999999999", .timespan, false, .{ .timespan = -946684799000000001 });
    try testParse("0D999999999", .timespan, false, .{ .timespan = 53315199000000000 });
    try testParse("0D999999999.", .timespan, false, .{ .timespan = 53315199000000000 });
    try testParse("0D999999999.9", .timespan, false, .{ .timespan = 53315199900000000 });
    try testParse("0D999999999.999999999", .timespan, false, .{ .timespan = 53315199999999999 });
    try testParse("0D999999999.9999999999", .timespan, false, .{ .timespan = 53315199999999999 });

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testParse("0D000000000000000", .timespan, false, .{ .timespan = 0 });
    try testParse("0D995959999999999", .timespan, false, .{ .timespan = 359999999999999 });

    try testParse("0D0000000000000000000", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("0D0000000000000000000.", .timespan, false, .{ .timespan = -946684800000000000 });
    try testParse("0D0000000000000000000.9", .timespan, false, .{ .timespan = -946684799100000000 });
    try testParse("0D0000000000000000000.999999999", .timespan, false, .{ .timespan = -946684799000000001 });
    try testParse("0D0000000000000000000.9999999999", .timespan, false, .{ .timespan = -946684799000000001 });
    try testParse("0D9223372036854775807", .timespan, false, overflow);
    try testParse("0D9223372036854775807.", .timespan, false, overflow);
    try testParse("0D9223372036854775807.9", .timespan, false, overflow);
    try testParse("0D9223372036854775807.999999999", .timespan, false, overflow);
    try testParse("0D9223372036854775807.9999999999", .timespan, false, overflow);

    try testParse("060", .timespan, false, overflow);
    try testParse("960", .timespan, false, overflow);

    try testParse("0060", .timespan, false, overflow);
    try testParse("9960", .timespan, false, overflow);

    try testParse("00060", .timespan, false, overflow);
    try testParse("99960", .timespan, false, overflow);

    try testParse("000060", .timespan, false, overflow);
    try testParse("995960", .timespan, false, overflow);
    try testParse("006000", .timespan, false, overflow);
    try testParse("996059", .timespan, false, overflow);

    try testParse("0000060", .timespan, false, overflow);
    try testParse("9995960", .timespan, false, overflow);
    try testParse("0006000", .timespan, false, overflow);
    try testParse("9996059", .timespan, false, overflow);

    try testParse("00000060", .timespan, false, overflow);
    try testParse("99995960", .timespan, false, overflow);
    try testParse("00006000", .timespan, false, overflow);
    try testParse("99996059", .timespan, false, overflow);

    try testParse("000060000000000", .timespan, false, overflow);
    try testParse("995960999999999", .timespan, false, overflow);
    try testParse("006000000000000", .timespan, false, overflow);
    try testParse("996059999999999", .timespan, false, overflow);

    try testParse("9223372036854775808", .timespan, false, overflow);

    try testParse("0D0:", .timespan, false, invalidCharacter(3));
    try testParse("0D00:0", .timespan, false, invalidCharacter(4));
    try testParse("0D00:0:", .timespan, false, invalidCharacter(6));
    try testParse("0D00:00:", .timespan, false, invalidCharacter(7));
    try testParse("0D00:00:0", .timespan, false, invalidCharacter(4));

    try testParse("0D060", .timespan, false, overflow);
    try testParse("0D060.", .timespan, false, overflow);
    try testParse("0D960", .timespan, false, overflow);
    try testParse("0D960.", .timespan, false, overflow);

    try testParse("0D0060", .timespan, false, overflow);
    try testParse("0D0060.", .timespan, false, overflow);
    try testParse("0D9960", .timespan, false, overflow);
    try testParse("0D9960.", .timespan, false, overflow);

    try testParse("0D00060", .timespan, false, overflow);
    try testParse("0D00060.", .timespan, false, overflow);
    try testParse("0D99960", .timespan, false, overflow);
    try testParse("0D99960.", .timespan, false, overflow);

    try testParse("0D000060", .timespan, false, overflow);
    try testParse("0D000060.", .timespan, false, overflow);
    try testParse("0D995960", .timespan, false, overflow);
    try testParse("0D995960.", .timespan, false, overflow);
    try testParse("0D006000", .timespan, false, overflow);
    try testParse("0D006000.", .timespan, false, overflow);
    try testParse("0D996059", .timespan, false, overflow);
    try testParse("0D996059.", .timespan, false, overflow);

    try testParse("0D0000060", .timespan, false, overflow);
    try testParse("0D0000060.", .timespan, false, overflow);
    try testParse("0D9995960", .timespan, false, overflow);
    try testParse("0D9995960.", .timespan, false, overflow);
    try testParse("0D0006000", .timespan, false, overflow);
    try testParse("0D0006000.", .timespan, false, overflow);
    try testParse("0D9996059", .timespan, false, overflow);
    try testParse("0D9996059.", .timespan, false, overflow);

    try testParse("0D00000060", .timespan, false, overflow);
    try testParse("0D00000060.", .timespan, false, overflow);
    try testParse("0D99995960", .timespan, false, overflow);
    try testParse("0D99995960.", .timespan, false, overflow);
    try testParse("0D00006000", .timespan, false, overflow);
    try testParse("0D00006000.", .timespan, false, overflow);
    try testParse("0D99996059", .timespan, false, overflow);
    try testParse("0D99996059.", .timespan, false, overflow);

    try testParse("0N", .timespan, true, .{ .timespan = null_long });
    try testParse("0n", .timespan, true, .{ .timespan = null_long });
    try testParse("0W", .timespan, true, .{ .timespan = inf_long });
    try testParse("0w", .timespan, true, .{ .timespan = inf_long });

    try testParse("9", .timespan, true, .{ .timespan = 32400000000000 });

    try testParse("00", .timespan, true, .{ .timespan = 0 });
    try testParse("99", .timespan, true, .{ .timespan = 356400000000000 });

    try testParse("000", .timespan, true, .{ .timespan = 0 });
    try testParse("959", .timespan, true, .{ .timespan = 35940000000000 });

    try testParse("0000", .timespan, true, .{ .timespan = 0 });
    try testParse("9959", .timespan, true, .{ .timespan = 359940000000000 });

    try testParse("00000", .timespan, true, .{ .timespan = 0 });
    try testParse("99959", .timespan, true, .{ .timespan = 3599940000000000 });

    try testParse("000000", .timespan, true, .{ .timespan = 0 });
    try testParse("995959", .timespan, true, .{ .timespan = 359999000000000 });

    try testParse("0000000", .timespan, true, .{ .timespan = 0 });
    try testParse("9995959", .timespan, true, .{ .timespan = 3599999000000000 });

    try testParse("00000000", .timespan, true, .{ .timespan = 0 });
    try testParse("99995959", .timespan, true, .{ .timespan = 35999999000000000 });

    try testParse("000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("999999999", .timespan, true, .{ .timespan = 53315199000000000 });

    try testParse("0000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("9999999999", .timespan, true, overflow);

    try testParse("00000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("99999999999", .timespan, true, overflow);

    try testParse("000000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("999999999999", .timespan, true, overflow);

    try testParse("0000000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("9999999999999", .timespan, true, overflow);

    try testParse("00000000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("99999999999999", .timespan, true, overflow);

    try testParse("000000000000000", .timespan, true, .{ .timespan = 0 });
    try testParse("995959999999999", .timespan, true, .{ .timespan = 359999999999999 });

    try testParse("0000000000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("9999999999999999", .timespan, true, overflow);

    try testParse("00000000000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("99999999999999999", .timespan, true, overflow);

    try testParse("000000000000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("999999999999999999", .timespan, true, overflow);

    try testParse("0000000000000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("9223372036854775807", .timespan, true, overflow);

    try testParse("0D", .timespan, true, .{ .timespan = 0 });
    try testParse("0D.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("9D", .timespan, true, .{ .timespan = 777600000000000 });
    try testParse("9D.", .timespan, true, .{ .timespan = 777600000000000 });
    try testParse("9D.9", .timespan, true, .{ .timespan = 777600900000000 });
    try testParse("9D.999999999", .timespan, true, .{ .timespan = 777600999999999 });
    try testParse("9D.9999999999", .timespan, true, .{ .timespan = 777600999999999 });

    try testParse("0D0", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D0.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D0.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D9", .timespan, true, .{ .timespan = 32400000000000 });
    try testParse("0D9.", .timespan, true, .{ .timespan = 32400000000000 });
    try testParse("0D9.9", .timespan, true, .{ .timespan = 32400900000000 });
    try testParse("0D9.999999999", .timespan, true, .{ .timespan = 32400999999999 });
    try testParse("0D9.9999999999", .timespan, true, .{ .timespan = 32400999999999 });

    try testParse("0D00", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00:.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D99", .timespan, true, .{ .timespan = 356400000000000 });
    try testParse("0D99.", .timespan, true, .{ .timespan = 356400000000000 });
    try testParse("0D99.9", .timespan, true, .{ .timespan = 356400900000000 });
    try testParse("0D99.999999999", .timespan, true, .{ .timespan = 356400999999999 });
    try testParse("0D99.9999999999", .timespan, true, .{ .timespan = 356400999999999 });
    try testParse("0D99:", .timespan, true, .{ .timespan = 356400000000000 });
    try testParse("0D99:.", .timespan, true, .{ .timespan = 356400000000000 });
    try testParse("0D99:.9", .timespan, true, .{ .timespan = 356400900000000 });
    try testParse("0D99:.999999999", .timespan, true, .{ .timespan = 356400999999999 });
    try testParse("0D99:.9999999999", .timespan, true, .{ .timespan = 356400999999999 });

    try testParse("0D000", .timespan, true, .{ .timespan = 0 });
    try testParse("0D000.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D000.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D000.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D000.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D959", .timespan, true, .{ .timespan = 35940000000000 });
    try testParse("0D959.", .timespan, true, .{ .timespan = 35940000000000 });
    try testParse("0D959.9", .timespan, true, .{ .timespan = 35940900000000 });
    try testParse("0D959.999999999", .timespan, true, .{ .timespan = 35940999999999 });
    try testParse("0D959.9999999999", .timespan, true, .{ .timespan = 35940999999999 });

    try testParse("0D0000", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0000.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0000.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D0000.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D0000.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:00", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:00.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:00.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00:00.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:00.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D9959", .timespan, true, .{ .timespan = 359940000000000 });
    try testParse("0D9959.", .timespan, true, .{ .timespan = 359940000000000 });
    try testParse("0D9959.9", .timespan, true, .{ .timespan = 359940900000000 });
    try testParse("0D9959.999999999", .timespan, true, .{ .timespan = 359940999999999 });
    try testParse("0D9959.9999999999", .timespan, true, .{ .timespan = 359940999999999 });
    try testParse("0D99:59", .timespan, true, .{ .timespan = 359940000000000 });
    try testParse("0D99:59.", .timespan, true, .{ .timespan = 359940000000000 });
    try testParse("0D99:59.9", .timespan, true, .{ .timespan = 359940900000000 });
    try testParse("0D99:59.999999999", .timespan, true, .{ .timespan = 359940999999999 });
    try testParse("0D99:59.9999999999", .timespan, true, .{ .timespan = 359940999999999 });

    try testParse("0D00000", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00000.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00000.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00000.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00000.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D99959", .timespan, true, .{ .timespan = 3599940000000000 });
    try testParse("0D99959.", .timespan, true, .{ .timespan = 3599940000000000 });
    try testParse("0D99959.9", .timespan, true, .{ .timespan = 3599940900000000 });
    try testParse("0D99959.999999999", .timespan, true, .{ .timespan = 3599940999999999 });
    try testParse("0D99959.9999999999", .timespan, true, .{ .timespan = 3599940999999999 });

    try testParse("0D000000", .timespan, true, .{ .timespan = 0 });
    try testParse("0D000000.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D000000.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D000000.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D000000.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:00:00", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:00:00.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:00:00.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00:00:00.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:00:00.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D995959", .timespan, true, .{ .timespan = 359999000000000 });
    try testParse("0D995959.", .timespan, true, .{ .timespan = 359999000000000 });
    try testParse("0D995959.9", .timespan, true, .{ .timespan = 359999900000000 });
    try testParse("0D995959.999999999", .timespan, true, .{ .timespan = 359999999999999 });
    try testParse("0D995959.9999999999", .timespan, true, .{ .timespan = 359999999999999 });
    try testParse("0D99:959", .timespan, true, .{ .timespan = 356459000000000 });
    try testParse("0D99:959.", .timespan, true, .{ .timespan = 356459000000000 });
    try testParse("0D99:959.9", .timespan, true, .{ .timespan = 356459900000000 });
    try testParse("0D99:959.999999999", .timespan, true, .{ .timespan = 356459999999999 });
    try testParse("0D99:959.9999999999", .timespan, true, .{ .timespan = 356459999999999 });
    try testParse("0D995:59", .timespan, true, invalidCharacter(5));
    try testParse("0D995:59.", .timespan, true, invalidCharacter(5));
    try testParse("0D995:59.9", .timespan, true, invalidCharacter(5));
    try testParse("0D995:59.999999999", .timespan, true, invalidCharacter(5));
    try testParse("0D995:59.9999999999", .timespan, true, invalidCharacter(5));
    try testParse("0D99::59", .timespan, true, .{ .timespan = 356459000000000 });
    try testParse("0D99::59.", .timespan, true, .{ .timespan = 356459000000000 });
    try testParse("0D99::59.9", .timespan, true, .{ .timespan = 356459900000000 });
    try testParse("0D99::59.999999999", .timespan, true, .{ .timespan = 356459999999999 });
    try testParse("0D99::59.9999999999", .timespan, true, .{ .timespan = 356459999999999 });
    try testParse("0D99:59:59", .timespan, true, .{ .timespan = 359999000000000 });
    try testParse("0D99:59:59.", .timespan, true, .{ .timespan = 359999000000000 });
    try testParse("0D99:59:59.9", .timespan, true, .{ .timespan = 359999900000000 });
    try testParse("0D99:59:59.999999999", .timespan, true, .{ .timespan = 359999999999999 });
    try testParse("0D99:59:59.9999999999", .timespan, true, .{ .timespan = 359999999999999 });

    try testParse("0D0000000", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0000000.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0000000.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D0000000.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D0000000.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D9995959", .timespan, true, .{ .timespan = 3599999000000000 });
    try testParse("0D9995959.", .timespan, true, .{ .timespan = 3599999000000000 });
    try testParse("0D9995959.9", .timespan, true, .{ .timespan = 3599999900000000 });
    try testParse("0D9995959.999999999", .timespan, true, .{ .timespan = 3599999999999999 });
    try testParse("0D9995959.9999999999", .timespan, true, .{ .timespan = 3599999999999999 });

    try testParse("0D00000000", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00000000.", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00000000.9", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00000000.999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00000000.9999999999", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D99995959", .timespan, true, .{ .timespan = 35999999000000000 });
    try testParse("0D99995959.", .timespan, true, .{ .timespan = 35999999000000000 });
    try testParse("0D99995959.9", .timespan, true, .{ .timespan = 35999999900000000 });
    try testParse("0D99995959.999999999", .timespan, true, .{ .timespan = 35999999999999999 });
    try testParse("0D99995959.9999999999", .timespan, true, .{ .timespan = 35999999999999999 });

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testParse("0D000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("0D000000000.", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("0D000000000.9", .timespan, true, .{ .timespan = -946684799100000000 });
    try testParse("0D000000000.999999999", .timespan, true, .{ .timespan = -946684799000000001 });
    try testParse("0D000000000.9999999999", .timespan, true, .{ .timespan = -946684799000000001 });
    try testParse("0D999999999", .timespan, true, .{ .timespan = 53315199000000000 });
    try testParse("0D999999999.", .timespan, true, .{ .timespan = 53315199000000000 });
    try testParse("0D999999999.9", .timespan, true, .{ .timespan = 53315199900000000 });
    try testParse("0D999999999.999999999", .timespan, true, .{ .timespan = 53315199999999999 });
    try testParse("0D999999999.9999999999", .timespan, true, .{ .timespan = 53315199999999999 });

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testParse("0D000000000000000", .timespan, true, .{ .timespan = 0 });
    try testParse("0D995959999999999", .timespan, true, .{ .timespan = 359999999999999 });

    try testParse("0D0000000000000000000", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("0D0000000000000000000.", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("0D0000000000000000000.9", .timespan, true, .{ .timespan = -946684799100000000 });
    try testParse("0D0000000000000000000.999999999", .timespan, true, .{ .timespan = -946684799000000001 });
    try testParse("0D0000000000000000000.9999999999", .timespan, true, .{ .timespan = -946684799000000001 });
    try testParse("0D9223372036854775807", .timespan, true, overflow);
    try testParse("0D9223372036854775807.", .timespan, true, overflow);
    try testParse("0D9223372036854775807.9", .timespan, true, overflow);
    try testParse("0D9223372036854775807.999999999", .timespan, true, overflow);
    try testParse("0D9223372036854775807.9999999999", .timespan, true, overflow);

    try testParse("060", .timespan, true, overflow);
    try testParse("960", .timespan, true, overflow);

    try testParse("0060", .timespan, true, overflow);
    try testParse("9960", .timespan, true, overflow);

    try testParse("00060", .timespan, true, overflow);
    try testParse("99960", .timespan, true, overflow);

    try testParse("000060", .timespan, true, overflow);
    try testParse("995960", .timespan, true, overflow);
    try testParse("006000", .timespan, true, overflow);
    try testParse("996059", .timespan, true, overflow);

    try testParse("0000060", .timespan, true, overflow);
    try testParse("9995960", .timespan, true, overflow);
    try testParse("0006000", .timespan, true, overflow);
    try testParse("9996059", .timespan, true, overflow);

    try testParse("00000060", .timespan, true, overflow);
    try testParse("99995960", .timespan, true, overflow);
    try testParse("00006000", .timespan, true, overflow);
    try testParse("99996059", .timespan, true, overflow);

    try testParse("000060000000000", .timespan, true, overflow);
    try testParse("995960999999999", .timespan, true, overflow);
    try testParse("006000000000000", .timespan, true, overflow);
    try testParse("996059999999999", .timespan, true, overflow);

    try testParse("9223372036854775808", .timespan, true, overflow);

    try testParse("0D0:", .timespan, true, invalidCharacter(3));
    try testParse("0D00:0", .timespan, true, invalidCharacter(4));
    try testParse("0D00:0:", .timespan, true, invalidCharacter(6));
    try testParse("0D00:00:", .timespan, true, invalidCharacter(7));
    try testParse("0D00:00:0", .timespan, true, invalidCharacter(4));

    try testParse("0D060", .timespan, true, overflow);
    try testParse("0D060.", .timespan, true, overflow);
    try testParse("0D960", .timespan, true, overflow);
    try testParse("0D960.", .timespan, true, overflow);

    try testParse("0D0060", .timespan, true, overflow);
    try testParse("0D0060.", .timespan, true, overflow);
    try testParse("0D9960", .timespan, true, overflow);
    try testParse("0D9960.", .timespan, true, overflow);

    try testParse("0D00060", .timespan, true, overflow);
    try testParse("0D00060.", .timespan, true, overflow);
    try testParse("0D99960", .timespan, true, overflow);
    try testParse("0D99960.", .timespan, true, overflow);

    try testParse("0D000060", .timespan, true, overflow);
    try testParse("0D000060.", .timespan, true, overflow);
    try testParse("0D995960", .timespan, true, overflow);
    try testParse("0D995960.", .timespan, true, overflow);
    try testParse("0D006000", .timespan, true, overflow);
    try testParse("0D006000.", .timespan, true, overflow);
    try testParse("0D996059", .timespan, true, overflow);
    try testParse("0D996059.", .timespan, true, overflow);

    try testParse("0D0000060", .timespan, true, overflow);
    try testParse("0D0000060.", .timespan, true, overflow);
    try testParse("0D9995960", .timespan, true, overflow);
    try testParse("0D9995960.", .timespan, true, overflow);
    try testParse("0D0006000", .timespan, true, overflow);
    try testParse("0D0006000.", .timespan, true, overflow);
    try testParse("0D9996059", .timespan, true, overflow);
    try testParse("0D9996059.", .timespan, true, overflow);

    try testParse("0D00000060", .timespan, true, overflow);
    try testParse("0D00000060.", .timespan, true, overflow);
    try testParse("0D99995960", .timespan, true, overflow);
    try testParse("0D99995960.", .timespan, true, overflow);
    try testParse("0D00006000", .timespan, true, overflow);
    try testParse("0D00006000.", .timespan, true, overflow);
    try testParse("0D99996059", .timespan, true, overflow);
    try testParse("0D99996059.", .timespan, true, overflow);

    try testParse("0Nn", .timespan, false, invalidCharacter(2));
    try testParse("0nn", .timespan, false, invalidCharacter(2));
    try testParse("0Wn", .timespan, false, invalidCharacter(2));
    try testParse("0wn", .timespan, false, invalidCharacter(2));

    try testParse("9n", .timespan, false, invalidCharacter(1));

    try testParse("00n", .timespan, false, invalidCharacter(2));
    try testParse("99n", .timespan, false, invalidCharacter(2));

    try testParse("000n", .timespan, false, invalidCharacter(3));
    try testParse("959n", .timespan, false, invalidCharacter(3));

    try testParse("0000n", .timespan, false, invalidCharacter(4));
    try testParse("9959n", .timespan, false, invalidCharacter(4));

    try testParse("00000n", .timespan, false, invalidCharacter(5));
    try testParse("99959n", .timespan, false, overflow);

    try testParse("000000n", .timespan, false, invalidCharacter(6));
    try testParse("995959n", .timespan, false, overflow);

    try testParse("0000000n", .timespan, false, invalidCharacter(7));
    try testParse("9995959n", .timespan, false, overflow);

    try testParse("00000000n", .timespan, false, invalidCharacter(8));
    try testParse("99995959n", .timespan, false, invalidCharacter(8));

    try testParse("000000000n", .timespan, false, invalidCharacter(9));
    try testParse("999999999n", .timespan, false, invalidCharacter(9));

    try testParse("0000000000n", .timespan, false, invalidCharacter(10));
    try testParse("9999999999n", .timespan, false, invalidCharacter(10));

    try testParse("00000000000n", .timespan, false, invalidCharacter(11));
    try testParse("99999999999n", .timespan, false, invalidCharacter(11));

    try testParse("000000000000n", .timespan, false, invalidCharacter(12));
    try testParse("999999999999n", .timespan, false, invalidCharacter(12));

    try testParse("0000000000000n", .timespan, false, invalidCharacter(13));
    try testParse("9999999999999n", .timespan, false, invalidCharacter(13));

    try testParse("00000000000000n", .timespan, false, invalidCharacter(14));
    try testParse("99999999999999n", .timespan, false, overflow);

    try testParse("000000000000000n", .timespan, false, invalidCharacter(15));
    try testParse("995959999999999n", .timespan, false, invalidCharacter(15));

    try testParse("0000000000000000n", .timespan, false, invalidCharacter(16));
    try testParse("9999999999999999n", .timespan, false, invalidCharacter(16));

    try testParse("00000000000000000n", .timespan, false, invalidCharacter(17));
    try testParse("99999999999999999n", .timespan, false, invalidCharacter(17));

    try testParse("000000000000000000n", .timespan, false, invalidCharacter(18));
    try testParse("999999999999999999n", .timespan, false, invalidCharacter(18));

    try testParse("0000000000000000000n", .timespan, false, invalidCharacter(19));
    try testParse("9223372036854775807n", .timespan, false, invalidCharacter(19));

    try testParse("0Dn", .timespan, false, invalidCharacter(2));
    try testParse("0D.n", .timespan, false, invalidCharacter(3));
    try testParse("0D.9n", .timespan, false, invalidCharacter(4));
    try testParse("0D.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("9Dn", .timespan, false, invalidCharacter(2));
    try testParse("9D.n", .timespan, false, invalidCharacter(3));
    try testParse("9D.9n", .timespan, false, invalidCharacter(4));
    try testParse("9D.999999999n", .timespan, false, .{ .timespan = 777600999999999 });
    try testParse("9D.9999999999n", .timespan, false, .{ .timespan = 777600999999999 });

    try testParse("0D0n", .timespan, false, invalidCharacter(3));
    try testParse("0D0.n", .timespan, false, invalidCharacter(4));
    try testParse("0D0.9n", .timespan, false, invalidCharacter(5));
    try testParse("0D0.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D0.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D9n", .timespan, false, invalidCharacter(3));
    try testParse("0D9.n", .timespan, false, invalidCharacter(4));
    try testParse("0D9.9n", .timespan, false, invalidCharacter(5));
    try testParse("0D9.999999999n", .timespan, false, .{ .timespan = 32400999999999 });
    try testParse("0D9.9999999999n", .timespan, false, .{ .timespan = 32400999999999 });

    try testParse("0D00n", .timespan, false, invalidCharacter(4));
    try testParse("0D00.n", .timespan, false, invalidCharacter(5));
    try testParse("0D00.9n", .timespan, false, invalidCharacter(6));
    try testParse("0D00.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:n", .timespan, false, invalidCharacter(4));
    try testParse("0D00:.n", .timespan, false, invalidCharacter(6));
    try testParse("0D00:.9n", .timespan, false, invalidCharacter(7));
    try testParse("0D00:.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D99n", .timespan, false, invalidCharacter(4));
    try testParse("0D99.n", .timespan, false, invalidCharacter(5));
    try testParse("0D99.9n", .timespan, false, invalidCharacter(6));
    try testParse("0D99.999999999n", .timespan, false, .{ .timespan = 356400999999999 });
    try testParse("0D99.9999999999n", .timespan, false, .{ .timespan = 356400999999999 });
    try testParse("0D99:n", .timespan, false, invalidCharacter(4));
    try testParse("0D99:.n", .timespan, false, invalidCharacter(6));
    try testParse("0D99:.9n", .timespan, false, invalidCharacter(7));
    try testParse("0D99:.999999999n", .timespan, false, .{ .timespan = 356400999999999 });
    try testParse("0D99:.9999999999n", .timespan, false, .{ .timespan = 356400999999999 });

    try testParse("0D000n", .timespan, false, invalidCharacter(5));
    try testParse("0D000.n", .timespan, false, invalidCharacter(6));
    try testParse("0D000.9n", .timespan, false, invalidCharacter(7));
    try testParse("0D000.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D000.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D959n", .timespan, false, invalidCharacter(5));
    try testParse("0D959.n", .timespan, false, invalidCharacter(6));
    try testParse("0D959.9n", .timespan, false, invalidCharacter(7));
    try testParse("0D959.999999999n", .timespan, false, .{ .timespan = 35940999999999 });
    try testParse("0D959.9999999999n", .timespan, false, .{ .timespan = 35940999999999 });

    try testParse("0D0000n", .timespan, false, invalidCharacter(6));
    try testParse("0D0000.n", .timespan, false, invalidCharacter(7));
    try testParse("0D0000.9n", .timespan, false, invalidCharacter(8));
    try testParse("0D0000.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D0000.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:00n", .timespan, false, invalidCharacter(7));
    try testParse("0D00:00.n", .timespan, false, invalidCharacter(8));
    try testParse("0D00:00.9n", .timespan, false, invalidCharacter(9));
    try testParse("0D00:00.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:00.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D9959n", .timespan, false, invalidCharacter(6));
    try testParse("0D9959.n", .timespan, false, invalidCharacter(7));
    try testParse("0D9959.9n", .timespan, false, invalidCharacter(8));
    try testParse("0D9959.999999999n", .timespan, false, .{ .timespan = 359940999999999 });
    try testParse("0D9959.9999999999n", .timespan, false, .{ .timespan = 359940999999999 });
    try testParse("0D99:59n", .timespan, false, invalidCharacter(7));
    try testParse("0D99:59.n", .timespan, false, invalidCharacter(8));
    try testParse("0D99:59.9n", .timespan, false, invalidCharacter(9));
    try testParse("0D99:59.999999999n", .timespan, false, .{ .timespan = 359940999999999 });
    try testParse("0D99:59.9999999999n", .timespan, false, .{ .timespan = 359940999999999 });

    try testParse("0D00000n", .timespan, false, invalidCharacter(7));
    try testParse("0D00000.n", .timespan, false, invalidCharacter(8));
    try testParse("0D00000.9n", .timespan, false, invalidCharacter(9));
    try testParse("0D00000.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00000.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D99959n", .timespan, false, overflow);
    try testParse("0D99959.n", .timespan, false, invalidCharacter(8));
    try testParse("0D99959.9n", .timespan, false, invalidCharacter(9));
    try testParse("0D99959.999999999n", .timespan, false, .{ .timespan = 3599940999999999 });
    try testParse("0D99959.9999999999n", .timespan, false, .{ .timespan = 3599940999999999 });

    try testParse("0D000000n", .timespan, false, invalidCharacter(8));
    try testParse("0D000000.n", .timespan, false, invalidCharacter(9));
    try testParse("0D000000.9n", .timespan, false, invalidCharacter(10));
    try testParse("0D000000.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D000000.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:00:00n", .timespan, false, invalidCharacter(4));
    try testParse("0D00:00:00.n", .timespan, false, invalidCharacter(11));
    try testParse("0D00:00:00.9n", .timespan, false, invalidCharacter(12));
    try testParse("0D00:00:00.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00:00:00.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D995959n", .timespan, false, overflow);
    try testParse("0D995959.n", .timespan, false, invalidCharacter(9));
    try testParse("0D995959.9n", .timespan, false, invalidCharacter(10));
    try testParse("0D995959.999999999n", .timespan, false, .{ .timespan = 359999999999999 });
    try testParse("0D995959.9999999999n", .timespan, false, .{ .timespan = 359999999999999 });
    try testParse("0D99:959n", .timespan, false, invalidCharacter(4));
    try testParse("0D99:959.n", .timespan, false, invalidCharacter(9));
    try testParse("0D99:959.9n", .timespan, false, invalidCharacter(10));
    try testParse("0D99:959.999999999n", .timespan, false, .{ .timespan = 356459999999999 });
    try testParse("0D99:959.9999999999n", .timespan, false, .{ .timespan = 356459999999999 });
    try testParse("0D995:59n", .timespan, false, invalidCharacter(5));
    try testParse("0D995:59.n", .timespan, false, invalidCharacter(9));
    try testParse("0D995:59.9n", .timespan, false, invalidCharacter(10));
    try testParse("0D995:59.999999999n", .timespan, false, invalidCharacter(5));
    try testParse("0D995:59.9999999999n", .timespan, false, invalidCharacter(5));
    try testParse("0D99::59n", .timespan, false, invalidCharacter(4));
    try testParse("0D99::59.n", .timespan, false, invalidCharacter(9));
    try testParse("0D99::59.9n", .timespan, false, invalidCharacter(10));
    try testParse("0D99::59.999999999n", .timespan, false, .{ .timespan = 356459999999999 });
    try testParse("0D99::59.9999999999n", .timespan, false, .{ .timespan = 356459999999999 });
    try testParse("0D99:59:59n", .timespan, false, invalidCharacter(4));
    try testParse("0D99:59:59.n", .timespan, false, invalidCharacter(11));
    try testParse("0D99:59:59.9n", .timespan, false, invalidCharacter(12));
    try testParse("0D99:59:59.999999999n", .timespan, false, .{ .timespan = 359999999999999 });
    try testParse("0D99:59:59.9999999999n", .timespan, false, .{ .timespan = 359999999999999 });

    try testParse("0D0000000n", .timespan, false, invalidCharacter(9));
    try testParse("0D0000000.n", .timespan, false, invalidCharacter(10));
    try testParse("0D0000000.9n", .timespan, false, invalidCharacter(11));
    try testParse("0D0000000.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D0000000.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D9995959n", .timespan, false, overflow);
    try testParse("0D9995959.n", .timespan, false, invalidCharacter(10));
    try testParse("0D9995959.9n", .timespan, false, invalidCharacter(11));
    try testParse("0D9995959.999999999n", .timespan, false, .{ .timespan = 3599999999999999 });
    try testParse("0D9995959.9999999999n", .timespan, false, .{ .timespan = 3599999999999999 });

    try testParse("0D00000000n", .timespan, false, invalidCharacter(10));
    try testParse("0D00000000.n", .timespan, false, invalidCharacter(11));
    try testParse("0D00000000.9n", .timespan, false, invalidCharacter(12));
    try testParse("0D00000000.999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D00000000.9999999999n", .timespan, false, .{ .timespan = 999999999 });
    try testParse("0D99995959n", .timespan, false, invalidCharacter(10));
    try testParse("0D99995959.n", .timespan, false, invalidCharacter(11));
    try testParse("0D99995959.9n", .timespan, false, invalidCharacter(12));
    try testParse("0D99995959.999999999n", .timespan, false, .{ .timespan = 35999999999999999 });
    try testParse("0D99995959.9999999999n", .timespan, false, .{ .timespan = 35999999999999999 });

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testParse("0D000000000n", .timespan, false, invalidCharacter(11));
    try testParse("0D000000000.n", .timespan, false, invalidCharacter(12));
    try testParse("0D000000000.9n", .timespan, false, invalidCharacter(13));
    try testParse("0D000000000.999999999n", .timespan, false, .{ .timespan = -946684799000000001 });
    try testParse("0D000000000.9999999999n", .timespan, false, .{ .timespan = -946684799000000001 });
    try testParse("0D999999999n", .timespan, false, invalidCharacter(11));
    try testParse("0D999999999.n", .timespan, false, invalidCharacter(12));
    try testParse("0D999999999.9n", .timespan, false, invalidCharacter(13));
    try testParse("0D999999999.999999999n", .timespan, false, .{ .timespan = 53315199999999999 });
    try testParse("0D999999999.9999999999n", .timespan, false, .{ .timespan = 53315199999999999 });

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testParse("0D000000000000000n", .timespan, false, invalidCharacter(17));
    try testParse("0D995959999999999n", .timespan, false, invalidCharacter(17));

    try testParse("0D0000000000000000000n", .timespan, false, invalidCharacter(21));
    try testParse("0D0000000000000000000.n", .timespan, false, invalidCharacter(22));
    try testParse("0D0000000000000000000.9n", .timespan, false, invalidCharacter(23));
    try testParse("0D0000000000000000000.999999999n", .timespan, false, .{ .timespan = -946684799000000001 });
    try testParse("0D0000000000000000000.9999999999n", .timespan, false, .{ .timespan = -946684799000000001 });
    try testParse("0D9223372036854775807n", .timespan, false, invalidCharacter(21));
    try testParse("0D9223372036854775807.n", .timespan, false, invalidCharacter(22));
    try testParse("0D9223372036854775807.9n", .timespan, false, invalidCharacter(23));
    try testParse("0D9223372036854775807.999999999n", .timespan, false, overflow);
    try testParse("0D9223372036854775807.9999999999n", .timespan, false, overflow);

    try testParse("060n", .timespan, false, invalidCharacter(3));
    try testParse("960n", .timespan, false, invalidCharacter(3));

    try testParse("0060n", .timespan, false, invalidCharacter(4));
    try testParse("9960n", .timespan, false, invalidCharacter(4));

    try testParse("00060n", .timespan, false, invalidCharacter(5));
    try testParse("99960n", .timespan, false, overflow);

    try testParse("000060n", .timespan, false, invalidCharacter(6));
    try testParse("995960n", .timespan, false, overflow);
    try testParse("006000n", .timespan, false, invalidCharacter(6));
    try testParse("996059n", .timespan, false, invalidCharacter(6));

    try testParse("0000060n", .timespan, false, invalidCharacter(7));
    try testParse("9995960n", .timespan, false, overflow);
    try testParse("0006000n", .timespan, false, invalidCharacter(7));
    try testParse("9996059n", .timespan, false, invalidCharacter(7));

    try testParse("00000060n", .timespan, false, invalidCharacter(8));
    try testParse("99995960n", .timespan, false, invalidCharacter(8));
    try testParse("00006000n", .timespan, false, invalidCharacter(8));
    try testParse("99996059n", .timespan, false, invalidCharacter(8));

    try testParse("000060000000000n", .timespan, false, invalidCharacter(15));
    try testParse("995960999999999n", .timespan, false, invalidCharacter(15));
    try testParse("006000000000000n", .timespan, false, invalidCharacter(15));
    try testParse("996059999999999n", .timespan, false, invalidCharacter(15));

    try testParse("9223372036854775808n", .timespan, false, overflow);

    try testParse("0D0:n", .timespan, false, invalidCharacter(3));
    try testParse("0D00:0n", .timespan, false, invalidCharacter(6));
    try testParse("0D00:0:n", .timespan, false, invalidCharacter(6));
    try testParse("0D00:00:n", .timespan, false, invalidCharacter(4));
    try testParse("0D00:00:0n", .timespan, false, invalidCharacter(9));

    try testParse("0D060n", .timespan, false, invalidCharacter(5));
    try testParse("0D060.n", .timespan, false, invalidCharacter(6));
    try testParse("0D960n", .timespan, false, invalidCharacter(5));
    try testParse("0D960.n", .timespan, false, invalidCharacter(6));

    try testParse("0D0060n", .timespan, false, invalidCharacter(6));
    try testParse("0D0060.n", .timespan, false, invalidCharacter(7));
    try testParse("0D9960n", .timespan, false, invalidCharacter(6));
    try testParse("0D9960.n", .timespan, false, invalidCharacter(7));

    try testParse("0D00060n", .timespan, false, invalidCharacter(7));
    try testParse("0D00060.n", .timespan, false, invalidCharacter(8));
    try testParse("0D99960n", .timespan, false, overflow);
    try testParse("0D99960.n", .timespan, false, invalidCharacter(8));

    try testParse("0D000060n", .timespan, false, invalidCharacter(8));
    try testParse("0D000060.n", .timespan, false, invalidCharacter(9));
    try testParse("0D995960n", .timespan, false, overflow);
    try testParse("0D995960.n", .timespan, false, invalidCharacter(9));
    try testParse("0D006000n", .timespan, false, invalidCharacter(8));
    try testParse("0D006000.n", .timespan, false, invalidCharacter(9));
    try testParse("0D996059n", .timespan, false, invalidCharacter(8));
    try testParse("0D996059.n", .timespan, false, invalidCharacter(9));

    try testParse("0D0000060n", .timespan, false, invalidCharacter(9));
    try testParse("0D0000060.n", .timespan, false, invalidCharacter(10));
    try testParse("0D9995960n", .timespan, false, overflow);
    try testParse("0D9995960.n", .timespan, false, invalidCharacter(10));
    try testParse("0D0006000n", .timespan, false, invalidCharacter(9));
    try testParse("0D0006000.n", .timespan, false, invalidCharacter(10));
    try testParse("0D9996059n", .timespan, false, invalidCharacter(9));
    try testParse("0D9996059.n", .timespan, false, invalidCharacter(10));

    try testParse("0D00000060n", .timespan, false, invalidCharacter(10));
    try testParse("0D00000060.n", .timespan, false, invalidCharacter(11));
    try testParse("0D99995960n", .timespan, false, invalidCharacter(10));
    try testParse("0D99995960.n", .timespan, false, invalidCharacter(11));
    try testParse("0D00006000n", .timespan, false, invalidCharacter(10));
    try testParse("0D00006000.n", .timespan, false, invalidCharacter(11));
    try testParse("0D99996059n", .timespan, false, invalidCharacter(10));
    try testParse("0D99996059.n", .timespan, false, invalidCharacter(11));

    try testParse("0Nn", .timespan, true, .{ .timespan = null_long });
    try testParse("0nn", .timespan, true, .{ .timespan = null_long });
    try testParse("0Wn", .timespan, true, .{ .timespan = inf_long });
    try testParse("0wn", .timespan, true, .{ .timespan = inf_long });

    try testParse("9n", .timespan, true, .{ .timespan = 32400000000000 });

    try testParse("00n", .timespan, true, .{ .timespan = 0 });
    try testParse("99n", .timespan, true, .{ .timespan = 356400000000000 });

    try testParse("000n", .timespan, true, .{ .timespan = 0 });
    try testParse("959n", .timespan, true, .{ .timespan = 35940000000000 });

    try testParse("0000n", .timespan, true, .{ .timespan = 0 });
    try testParse("9959n", .timespan, true, .{ .timespan = 359940000000000 });

    try testParse("00000n", .timespan, true, .{ .timespan = 0 });
    try testParse("99959n", .timespan, true, .{ .timespan = 3599940000000000 });

    try testParse("000000n", .timespan, true, .{ .timespan = 0 });
    try testParse("995959n", .timespan, true, .{ .timespan = 359999000000000 });

    try testParse("0000000n", .timespan, true, .{ .timespan = 0 });
    try testParse("9995959n", .timespan, true, .{ .timespan = 3599999000000000 });

    try testParse("00000000n", .timespan, true, .{ .timespan = 0 });
    try testParse("99995959n", .timespan, true, .{ .timespan = 35999999000000000 });

    try testParse("000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("999999999n", .timespan, true, .{ .timespan = 53315199000000000 });

    try testParse("0000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("9999999999n", .timespan, true, overflow);

    try testParse("00000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("99999999999n", .timespan, true, overflow);

    try testParse("000000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("999999999999n", .timespan, true, overflow);

    try testParse("0000000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("9999999999999n", .timespan, true, overflow);

    try testParse("00000000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("99999999999999n", .timespan, true, overflow);

    try testParse("000000000000000n", .timespan, true, .{ .timespan = 0 });
    try testParse("995959999999999n", .timespan, true, .{ .timespan = 359999999999999 });

    try testParse("0000000000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("9999999999999999n", .timespan, true, overflow);

    try testParse("00000000000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("99999999999999999n", .timespan, true, overflow);

    try testParse("000000000000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("999999999999999999n", .timespan, true, overflow);

    try testParse("0000000000000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("9223372036854775807n", .timespan, true, overflow);

    try testParse("0Dn", .timespan, true, .{ .timespan = 0 });
    try testParse("0D.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("9Dn", .timespan, true, .{ .timespan = 777600000000000 });
    try testParse("9D.n", .timespan, true, .{ .timespan = 777600000000000 });
    try testParse("9D.9n", .timespan, true, .{ .timespan = 777600900000000 });
    try testParse("9D.999999999n", .timespan, true, .{ .timespan = 777600999999999 });
    try testParse("9D.9999999999n", .timespan, true, .{ .timespan = 777600999999999 });

    try testParse("0D0n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D0.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D0.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D9n", .timespan, true, .{ .timespan = 32400000000000 });
    try testParse("0D9.n", .timespan, true, .{ .timespan = 32400000000000 });
    try testParse("0D9.9n", .timespan, true, .{ .timespan = 32400900000000 });
    try testParse("0D9.999999999n", .timespan, true, .{ .timespan = 32400999999999 });
    try testParse("0D9.9999999999n", .timespan, true, .{ .timespan = 32400999999999 });

    try testParse("0D00n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00:.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D99n", .timespan, true, .{ .timespan = 356400000000000 });
    try testParse("0D99.n", .timespan, true, .{ .timespan = 356400000000000 });
    try testParse("0D99.9n", .timespan, true, .{ .timespan = 356400900000000 });
    try testParse("0D99.999999999n", .timespan, true, .{ .timespan = 356400999999999 });
    try testParse("0D99.9999999999n", .timespan, true, .{ .timespan = 356400999999999 });
    try testParse("0D99:n", .timespan, true, .{ .timespan = 356400000000000 });
    try testParse("0D99:.n", .timespan, true, .{ .timespan = 356400000000000 });
    try testParse("0D99:.9n", .timespan, true, .{ .timespan = 356400900000000 });
    try testParse("0D99:.999999999n", .timespan, true, .{ .timespan = 356400999999999 });
    try testParse("0D99:.9999999999n", .timespan, true, .{ .timespan = 356400999999999 });

    try testParse("0D000n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D000.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D000.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D000.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D000.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D959n", .timespan, true, .{ .timespan = 35940000000000 });
    try testParse("0D959.n", .timespan, true, .{ .timespan = 35940000000000 });
    try testParse("0D959.9n", .timespan, true, .{ .timespan = 35940900000000 });
    try testParse("0D959.999999999n", .timespan, true, .{ .timespan = 35940999999999 });
    try testParse("0D959.9999999999n", .timespan, true, .{ .timespan = 35940999999999 });

    try testParse("0D0000n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0000.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0000.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D0000.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D0000.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:00n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:00.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:00.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00:00.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:00.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D9959n", .timespan, true, .{ .timespan = 359940000000000 });
    try testParse("0D9959.n", .timespan, true, .{ .timespan = 359940000000000 });
    try testParse("0D9959.9n", .timespan, true, .{ .timespan = 359940900000000 });
    try testParse("0D9959.999999999n", .timespan, true, .{ .timespan = 359940999999999 });
    try testParse("0D9959.9999999999n", .timespan, true, .{ .timespan = 359940999999999 });
    try testParse("0D99:59n", .timespan, true, .{ .timespan = 359940000000000 });
    try testParse("0D99:59.n", .timespan, true, .{ .timespan = 359940000000000 });
    try testParse("0D99:59.9n", .timespan, true, .{ .timespan = 359940900000000 });
    try testParse("0D99:59.999999999n", .timespan, true, .{ .timespan = 359940999999999 });
    try testParse("0D99:59.9999999999n", .timespan, true, .{ .timespan = 359940999999999 });

    try testParse("0D00000n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00000.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00000.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00000.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00000.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D99959n", .timespan, true, .{ .timespan = 3599940000000000 });
    try testParse("0D99959.n", .timespan, true, .{ .timespan = 3599940000000000 });
    try testParse("0D99959.9n", .timespan, true, .{ .timespan = 3599940900000000 });
    try testParse("0D99959.999999999n", .timespan, true, .{ .timespan = 3599940999999999 });
    try testParse("0D99959.9999999999n", .timespan, true, .{ .timespan = 3599940999999999 });

    try testParse("0D000000n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D000000.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D000000.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D000000.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D000000.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:00:00n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:00:00.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00:00:00.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00:00:00.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00:00:00.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D995959n", .timespan, true, .{ .timespan = 359999000000000 });
    try testParse("0D995959.n", .timespan, true, .{ .timespan = 359999000000000 });
    try testParse("0D995959.9n", .timespan, true, .{ .timespan = 359999900000000 });
    try testParse("0D995959.999999999n", .timespan, true, .{ .timespan = 359999999999999 });
    try testParse("0D995959.9999999999n", .timespan, true, .{ .timespan = 359999999999999 });
    try testParse("0D99:959n", .timespan, true, .{ .timespan = 356459000000000 });
    try testParse("0D99:959.n", .timespan, true, .{ .timespan = 356459000000000 });
    try testParse("0D99:959.9n", .timespan, true, .{ .timespan = 356459900000000 });
    try testParse("0D99:959.999999999n", .timespan, true, .{ .timespan = 356459999999999 });
    try testParse("0D99:959.9999999999n", .timespan, true, .{ .timespan = 356459999999999 });
    try testParse("0D995:59n", .timespan, true, invalidCharacter(5));
    try testParse("0D995:59.n", .timespan, true, invalidCharacter(5));
    try testParse("0D995:59.9n", .timespan, true, invalidCharacter(5));
    try testParse("0D995:59.999999999n", .timespan, true, invalidCharacter(5));
    try testParse("0D995:59.9999999999n", .timespan, true, invalidCharacter(5));
    try testParse("0D99::59n", .timespan, true, .{ .timespan = 356459000000000 });
    try testParse("0D99::59.n", .timespan, true, .{ .timespan = 356459000000000 });
    try testParse("0D99::59.9n", .timespan, true, .{ .timespan = 356459900000000 });
    try testParse("0D99::59.999999999n", .timespan, true, .{ .timespan = 356459999999999 });
    try testParse("0D99::59.9999999999n", .timespan, true, .{ .timespan = 356459999999999 });
    try testParse("0D99:59:59n", .timespan, true, .{ .timespan = 359999000000000 });
    try testParse("0D99:59:59.n", .timespan, true, .{ .timespan = 359999000000000 });
    try testParse("0D99:59:59.9n", .timespan, true, .{ .timespan = 359999900000000 });
    try testParse("0D99:59:59.999999999n", .timespan, true, .{ .timespan = 359999999999999 });
    try testParse("0D99:59:59.9999999999n", .timespan, true, .{ .timespan = 359999999999999 });

    try testParse("0D0000000n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0000000.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D0000000.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D0000000.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D0000000.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D9995959n", .timespan, true, .{ .timespan = 3599999000000000 });
    try testParse("0D9995959.n", .timespan, true, .{ .timespan = 3599999000000000 });
    try testParse("0D9995959.9n", .timespan, true, .{ .timespan = 3599999900000000 });
    try testParse("0D9995959.999999999n", .timespan, true, .{ .timespan = 3599999999999999 });
    try testParse("0D9995959.9999999999n", .timespan, true, .{ .timespan = 3599999999999999 });

    try testParse("0D00000000n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00000000.n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D00000000.9n", .timespan, true, .{ .timespan = 900000000 });
    try testParse("0D00000000.999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D00000000.9999999999n", .timespan, true, .{ .timespan = 999999999 });
    try testParse("0D99995959n", .timespan, true, .{ .timespan = 35999999000000000 });
    try testParse("0D99995959.n", .timespan, true, .{ .timespan = 35999999000000000 });
    try testParse("0D99995959.9n", .timespan, true, .{ .timespan = 35999999900000000 });
    try testParse("0D99995959.999999999n", .timespan, true, .{ .timespan = 35999999999999999 });
    try testParse("0D99995959.9999999999n", .timespan, true, .{ .timespan = 35999999999999999 });

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testParse("0D000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("0D000000000.n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("0D000000000.9n", .timespan, true, .{ .timespan = -946684799100000000 });
    try testParse("0D000000000.999999999n", .timespan, true, .{ .timespan = -946684799000000001 });
    try testParse("0D000000000.9999999999n", .timespan, true, .{ .timespan = -946684799000000001 });
    try testParse("0D999999999n", .timespan, true, .{ .timespan = 53315199000000000 });
    try testParse("0D999999999.n", .timespan, true, .{ .timespan = 53315199000000000 });
    try testParse("0D999999999.9n", .timespan, true, .{ .timespan = 53315199900000000 });
    try testParse("0D999999999.999999999n", .timespan, true, .{ .timespan = 53315199999999999 });
    try testParse("0D999999999.9999999999n", .timespan, true, .{ .timespan = 53315199999999999 });

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testParse("0D000000000000000n", .timespan, true, .{ .timespan = 0 });
    try testParse("0D995959999999999n", .timespan, true, .{ .timespan = 359999999999999 });

    try testParse("0D0000000000000000000n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("0D0000000000000000000.n", .timespan, true, .{ .timespan = -946684800000000000 });
    try testParse("0D0000000000000000000.9n", .timespan, true, .{ .timespan = -946684799100000000 });
    try testParse("0D0000000000000000000.999999999n", .timespan, true, .{ .timespan = -946684799000000001 });
    try testParse("0D0000000000000000000.9999999999n", .timespan, true, .{ .timespan = -946684799000000001 });
    try testParse("0D9223372036854775807n", .timespan, true, overflow);
    try testParse("0D9223372036854775807.n", .timespan, true, overflow);
    try testParse("0D9223372036854775807.9n", .timespan, true, overflow);
    try testParse("0D9223372036854775807.999999999n", .timespan, true, overflow);
    try testParse("0D9223372036854775807.9999999999n", .timespan, true, overflow);

    try testParse("060n", .timespan, true, overflow);
    try testParse("960n", .timespan, true, overflow);

    try testParse("0060n", .timespan, true, overflow);
    try testParse("9960n", .timespan, true, overflow);

    try testParse("00060n", .timespan, true, overflow);
    try testParse("99960n", .timespan, true, overflow);

    try testParse("000060n", .timespan, true, overflow);
    try testParse("995960n", .timespan, true, overflow);
    try testParse("006000n", .timespan, true, overflow);
    try testParse("996059n", .timespan, true, overflow);

    try testParse("0000060n", .timespan, true, overflow);
    try testParse("9995960n", .timespan, true, overflow);
    try testParse("0006000n", .timespan, true, overflow);
    try testParse("9996059n", .timespan, true, overflow);

    try testParse("00000060n", .timespan, true, overflow);
    try testParse("99995960n", .timespan, true, overflow);
    try testParse("00006000n", .timespan, true, overflow);
    try testParse("99996059n", .timespan, true, overflow);

    try testParse("000060000000000n", .timespan, true, overflow);
    try testParse("995960999999999n", .timespan, true, overflow);
    try testParse("006000000000000n", .timespan, true, overflow);
    try testParse("996059999999999n", .timespan, true, overflow);

    try testParse("9223372036854775808n", .timespan, true, overflow);

    try testParse("0D0:n", .timespan, true, invalidCharacter(3));
    try testParse("0D00:0n", .timespan, true, invalidCharacter(4));
    try testParse("0D00:0:n", .timespan, true, invalidCharacter(6));
    try testParse("0D00:00:n", .timespan, true, invalidCharacter(7));
    try testParse("0D00:00:0n", .timespan, true, invalidCharacter(4));

    try testParse("0D060n", .timespan, true, overflow);
    try testParse("0D060.n", .timespan, true, overflow);
    try testParse("0D960n", .timespan, true, overflow);
    try testParse("0D960.n", .timespan, true, overflow);

    try testParse("0D0060n", .timespan, true, overflow);
    try testParse("0D0060.n", .timespan, true, overflow);
    try testParse("0D9960n", .timespan, true, overflow);
    try testParse("0D9960.n", .timespan, true, overflow);

    try testParse("0D00060n", .timespan, true, overflow);
    try testParse("0D00060.n", .timespan, true, overflow);
    try testParse("0D99960n", .timespan, true, overflow);
    try testParse("0D99960.n", .timespan, true, overflow);

    try testParse("0D000060n", .timespan, true, overflow);
    try testParse("0D000060.n", .timespan, true, overflow);
    try testParse("0D995960n", .timespan, true, overflow);
    try testParse("0D995960.n", .timespan, true, overflow);
    try testParse("0D006000n", .timespan, true, overflow);
    try testParse("0D006000.n", .timespan, true, overflow);
    try testParse("0D996059n", .timespan, true, overflow);
    try testParse("0D996059.n", .timespan, true, overflow);

    try testParse("0D0000060n", .timespan, true, overflow);
    try testParse("0D0000060.n", .timespan, true, overflow);
    try testParse("0D9995960n", .timespan, true, overflow);
    try testParse("0D9995960.n", .timespan, true, overflow);
    try testParse("0D0006000n", .timespan, true, overflow);
    try testParse("0D0006000.n", .timespan, true, overflow);
    try testParse("0D9996059n", .timespan, true, overflow);
    try testParse("0D9996059.n", .timespan, true, overflow);

    try testParse("0D00000060n", .timespan, true, overflow);
    try testParse("0D00000060.n", .timespan, true, overflow);
    try testParse("0D99995960n", .timespan, true, overflow);
    try testParse("0D99995960.n", .timespan, true, overflow);
    try testParse("0D00006000n", .timespan, true, overflow);
    try testParse("0D00006000.n", .timespan, true, overflow);
    try testParse("0D99996059n", .timespan, true, overflow);
    try testParse("0D99996059.n", .timespan, true, overflow);
}
