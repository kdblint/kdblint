const number_parser = @import("../number_parser.zig");
const Parse = @import("../../Parse.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const testParse = Parse.testParse;
const testParseError = Parse.testParseError;
const null_guid = number_parser.null_guid;

test "valid guid inputs" {
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
    try testNumberParser("0ng", .guid, null_guid);
    try testNumberParser("0Ng", .guid, null_guid);

    try testParse("0ng", &.{ .implicit_return, .guid_literal }, "00000000-0000-0000-0000-000000000000");
    try testParse("0Ng", &.{ .implicit_return, .guid_literal }, "00000000-0000-0000-0000-000000000000");

    try testParse(
        "0N 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0N 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0N 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0n 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0n 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0N 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0N 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0n 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0n 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
}

test "invalid guid inputs" {
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
    try testNumberParserError("0wg", error.InvalidCharacter);
    try testNumberParserError("0Wg", error.InvalidCharacter);

    try testParseError("1 0Ng", &.{.parse_error});
    try testParseError("1 2 0Ng", &.{.parse_error});
    try testParseError("1. 0Ng", &.{.parse_error});
    try testParseError("1. 2. 0Ng", &.{.parse_error});
    try testParseError("1f 0Ng", &.{.parse_error});
    try testParseError("1 2f 0Ng", &.{.parse_error});
}

test {
    @import("std").testing.refAllDecls(@This());
}
