const common = @import("common.zig");
const Result = common.Result;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;

pub fn parseGuid(bytes: []const u8, allow_suffix: bool) Result {
    return switch (bytes.len) {
        0 => unreachable,
        1 => .{ .failure = .{ .invalid_character = 0 } },
        2 => if (bytes[0] != '0')
            .{ .failure = .{ .invalid_character = 0 } }
        else switch (bytes[1]) {
            'n', 'N' => .guid,
            else => .{ .failure = .{ .invalid_character = 1 } },
        },
        else => if (bytes[0] != '0')
            .{ .failure = .{ .invalid_character = 0 } }
        else switch (bytes[1]) {
            'n', 'N' => if (allow_suffix and bytes[2] == 'g')
                .guid
            else
                .{ .failure = .{ .invalid_character = 2 } },
            else => .{ .failure = .{ .invalid_character = 1 } },
        },
    };
}

test "parse number literal - guid" {
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
    try testParse("0n", .guid, false, .guid);
    try testParse("0N", .guid, false, .guid);
    try testParse("0w", .guid, false, invalidCharacter(1));
    try testParse("0W", .guid, false, invalidCharacter(1));
    try testParse("0", .guid, false, invalidCharacter(0));
    try testParse("1", .guid, false, invalidCharacter(0));
    try testParse("2", .guid, false, invalidCharacter(0));
    try testParse("0n", .guid, true, .guid);
    try testParse("0N", .guid, true, .guid);
    try testParse("0w", .guid, true, invalidCharacter(1));
    try testParse("0W", .guid, true, invalidCharacter(1));
    try testParse("0", .guid, true, invalidCharacter(0));
    try testParse("1", .guid, true, invalidCharacter(0));
    try testParse("2", .guid, true, invalidCharacter(0));

    try testParse("0ng", .guid, false, invalidCharacter(2));
    try testParse("0Ng", .guid, false, invalidCharacter(2));
    try testParse("0wg", .guid, false, invalidCharacter(1));
    try testParse("0Wg", .guid, false, invalidCharacter(1));
    try testParse("0g", .guid, false, invalidCharacter(1));
    try testParse("1g", .guid, false, invalidCharacter(0));
    try testParse("2g", .guid, false, invalidCharacter(0));
    try testParse("0ng", .guid, true, .guid);
    try testParse("0Ng", .guid, true, .guid);
    try testParse("0wg", .guid, true, invalidCharacter(1));
    try testParse("0Wg", .guid, true, invalidCharacter(1));
    try testParse("0g", .guid, true, invalidCharacter(1));
    try testParse("1g", .guid, true, invalidCharacter(0));
    try testParse("2g", .guid, true, invalidCharacter(0));
}
