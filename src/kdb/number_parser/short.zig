const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;

const null_short = common.null_short;
const inf_short = common.inf_short;

pub fn parseShort(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .short = null_short },
        'W', 'w' => return .{ .short = inf_short },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'h' => .{ .short = null_short },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'h' => .{ .short = inf_short },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'h') bytes[0 .. bytes.len - 1] else bytes;
    const x = switch (parseSlice(i16, slice)) {
        .overflow => return .{ .failure = .overflow },
        .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
        .int => |int| int,
    };

    if (x == inf_short) return .{ .failure = .prefer_short_inf };
    if (x > inf_short) return .{ .failure = .overflow };

    return .{ .short = x };
}

test "parse number literal - short" {
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
    try testParse("0n", .short, false, .{ .short = null_short });
    try testParse("0N", .short, false, .{ .short = null_short });
    try testParse("0w", .short, false, .{ .short = inf_short });
    try testParse("0W", .short, false, .{ .short = inf_short });
    try testParse("1", .short, false, .{ .short = 1 });
    try testParse("2", .short, false, .{ .short = 2 });
    try testParse("32766", .short, false, .{ .short = 32766 });
    try testParse("32767", .short, false, .{
        .failure = .prefer_short_inf,
    });
    try testParse("32768", .short, false, overflow);
    try testParse("0n", .short, true, .{ .short = null_short });
    try testParse("0N", .short, true, .{ .short = null_short });
    try testParse("0w", .short, true, .{ .short = inf_short });
    try testParse("0W", .short, true, .{ .short = inf_short });
    try testParse("0", .short, true, .{ .short = 0 });
    try testParse("1", .short, true, .{ .short = 1 });
    try testParse("2", .short, true, .{ .short = 2 });
    try testParse("32766", .short, true, .{ .short = 32766 });
    try testParse("32767", .short, true, .{
        .failure = .prefer_short_inf,
    });
    try testParse("32768", .short, true, overflow);

    try testParse("0nh", .short, false, invalidCharacter(2));
    try testParse("0Nh", .short, false, invalidCharacter(2));
    try testParse("0wh", .short, false, invalidCharacter(2));
    try testParse("0Wh", .short, false, invalidCharacter(2));
    try testParse("0h", .short, false, invalidCharacter(1));
    try testParse("1h", .short, false, invalidCharacter(1));
    try testParse("2h", .short, false, invalidCharacter(1));
    try testParse("32766h", .short, false, invalidCharacter(5));
    try testParse("32767h", .short, false, invalidCharacter(5));
    try testParse("32768h", .short, false, overflow);
    try testParse("0nh", .short, true, .{ .short = null_short });
    try testParse("0Nh", .short, true, .{ .short = null_short });
    try testParse("0wh", .short, true, .{ .short = inf_short });
    try testParse("0Wh", .short, true, .{ .short = inf_short });
    try testParse("0h", .short, true, .{ .short = 0 });
    try testParse("1h", .short, true, .{ .short = 1 });
    try testParse("2h", .short, true, .{ .short = 2 });
    try testParse("32766h", .short, true, .{ .short = 32766 });
    try testParse("32767h", .short, true, .{
        .failure = .prefer_short_inf,
    });
    try testParse("32768h", .short, true, overflow);
}
