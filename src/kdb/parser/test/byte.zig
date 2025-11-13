const number_parser = @import("../number_parser.zig");
const Parse = @import("../../Parse.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const testParse = Parse.testParse;

test "valid byte inputs" {
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
    try testNumberParser("0x0", .byte, 0);
    try testNumberParser("0x00", .byte, 0);
    try testNumberParser("0x1", .byte, 1);
    try testNumberParser("0x01", .byte, 1);
    try testNumberParser("0xf", .byte, 15);
    try testNumberParser("0x0f", .byte, 15);
    try testNumberParser("0xF", .byte, 15);
    try testNumberParser("0x0F", .byte, 15);
    try testNumberParser("0x10", .byte, 16);
    try testNumberParser("0xf0", .byte, 240);
    try testNumberParser("0xF0", .byte, 240);
    try testNumberParser("0xff", .byte, 255);
    try testNumberParser("0xFF", .byte, 255);

    try testNumberParser("0x", .byte_list, .{});

    try testParse("0x00", &.{ .implicit_return, .byte_literal }, "0x00");

    try testParse("0 0x00", &.{ .implicit_return, .implicit_apply, .byte_literal, .long_literal }, "(0;0x00)");
    try testParse("0 0x", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .long_literal }, "(0;`byte$())");
    try testParse("0 0x0001", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .long_literal }, "(0;0x0001)");
    try testParse("0 1 0x00", &.{ .implicit_return, .implicit_apply, .byte_literal, .long_list_literal }, "(0 1;0x00)");
    try testParse("0 1 0x", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .long_list_literal }, "(0 1;`byte$())");
    try testParse("0 1 0x0001", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .long_list_literal }, "(0 1;0x0001)");

    try testParse("0f 0x00", &.{ .implicit_return, .implicit_apply, .byte_literal, .float_literal }, "(0f;0x00)");
    try testParse("0f 0x", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .float_literal }, "(0f;`byte$())");
    try testParse("0f 0x0001", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .float_literal }, "(0f;0x0001)");
    try testParse("0 1f 0x00", &.{ .implicit_return, .implicit_apply, .byte_literal, .float_list_literal }, "(0 1f;0x00)");
    try testParse("0 1f 0x", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .float_list_literal }, "(0 1f;`byte$())");
    try testParse("0 1f 0x0001", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .float_list_literal }, "(0 1f;0x0001)");

    try testParse("0x00 0", &.{ .implicit_return, .implicit_apply, .long_literal, .byte_literal }, "(0x00;0)");
    try testParse("0x00 0 1", &.{ .implicit_return, .implicit_apply, .long_list_literal, .byte_literal }, "(0x00;0 1)");
    try testParse("0x 0", &.{ .implicit_return, .implicit_apply, .long_literal, .byte_list_literal }, "(`byte$();0)");
    try testParse("0x 0 1", &.{ .implicit_return, .implicit_apply, .long_list_literal, .byte_list_literal }, "(`byte$();0 1)");
    try testParse("0x0001 0", &.{ .implicit_return, .implicit_apply, .long_literal, .byte_list_literal }, "(0x0001;0)");
    try testParse("0x0001 0 1", &.{ .implicit_return, .implicit_apply, .long_list_literal, .byte_list_literal }, "(0x0001;0 1)");
}

test "invalid byte inputs" {
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
    try testNumberParserError("0X", error.InvalidCharacter);
    try testNumberParserError("0xg", error.InvalidCharacter);
    try testNumberParserError("0xG", error.InvalidCharacter);
    try testNumberParserError("0xgg", error.InvalidCharacter);
    try testNumberParserError("0xGG", error.InvalidCharacter);
}

test {
    @import("std").testing.refAllDecls(@This());
}
