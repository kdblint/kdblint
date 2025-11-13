const common = @import("common.zig");
const Result = common.Result;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const invalidBinary = common.invalidBinary;

pub fn parseBool(bytes: []const u8) Result {
    const value = switch (bytes[0]) {
        '0' => false,
        '1' => true,
        else => return .{ .failure = .{
            .invalid_digit = .{ .i = 0, .base = .binary },
        } },
    };

    if (bytes.len > 1) return .{ .failure = .{ .invalid_character = 1 } };

    return .{ .bool = value };
}

test "parse number literal - bool" {
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
    try testParse("0n", .bool, undefined, invalidCharacter(1));
    try testParse("0N", .bool, undefined, invalidCharacter(1));
    try testParse("0w", .bool, undefined, invalidCharacter(1));
    try testParse("0W", .bool, undefined, invalidCharacter(1));
    try testParse("0", .bool, undefined, .{ .bool = false });
    try testParse("1", .bool, undefined, .{ .bool = true });
    try testParse("2", .bool, undefined, invalidBinary(0));
}
