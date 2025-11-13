const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_float = number_parser.null_float;
const inf_float = number_parser.inf_float;

test "valid datetime inputs" {
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
    try testNumberParser("0Nz", .datetime, @as(f64, null_float));
    try testNumberParser("0nz", .datetime, @as(f64, null_float));
    try testNumberParser("0Wz", .datetime, @as(f64, inf_float));
    try testNumberParser("0wz", .datetime, @as(f64, inf_float));
    try testNumberParser("-0Wz", .datetime, @as(f64, -inf_float));
    try testNumberParser("-0wz", .datetime, @as(f64, -inf_float));

    try testNumberParser("000101z", .datetime, @as(f64, 0));
    try testNumberParser("-000101z", .datetime, @as(f64, 0));
    try testNumberParser("000229z", .datetime, @as(f64, 59));
    try testNumberParser("-000229z", .datetime, @as(f64, -59));
    try testNumberParser("491231z", .datetime, @as(f64, 18262));
    try testNumberParser("-491231z", .datetime, @as(f64, -18262));
    try testNumberParser("500101z", .datetime, @as(f64, -18262));
    try testNumberParser("-500101z", .datetime, @as(f64, 18262));
    try testNumberParser("991231z", .datetime, @as(f64, -1));
    try testNumberParser("-991231z", .datetime, @as(f64, 1));

    try testNumberParser("00010101z", .datetime, @as(f64, -730119));
    try testNumberParser("-00010101z", .datetime, @as(f64, 730119));
    try testNumberParser("00011231z", .datetime, @as(f64, -729755));
    try testNumberParser("-00011231z", .datetime, @as(f64, 729755));
    try testNumberParser("99990101z", .datetime, @as(f64, 2921575));
    try testNumberParser("-99990101z", .datetime, @as(f64, -2921575));
    try testNumberParser("99991231z", .datetime, @as(f64, 2921939));
    try testNumberParser("-99991231z", .datetime, @as(f64, -2921939));

    try testNumberParser("000000000z", .datetime, @as(f64, -10957));
    try testNumberParser("-000000000z", .datetime, @as(f64, 10957));
    try testNumberParser("999999999z", .datetime, @as(f64, 617.07406));
    try testNumberParser("-999999999z", .datetime, @as(f64, -617.07406));

    try testNumberParser("999999999999999999999999z", .datetime, @as(f64, 1.1574074e19));
    try testNumberParser("-999999999999999999999999z", .datetime, @as(f64, -1.1574074e19));

    return error.SkipZigTest;
}

test "invalid datetime inputs" {
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
    try testNumberParserError("000001z", error.Overflow);
    try testNumberParserError("-000001z", error.Overflow);
    try testNumberParserError("000100z", error.Overflow);
    try testNumberParserError("-000100z", error.Overflow);

    try testNumberParserError("00000101z", error.Overflow);
    try testNumberParserError("-00000101z", error.Overflow);
    try testNumberParserError("00010001z", error.Overflow);
    try testNumberParserError("-00010001z", error.Overflow);
    try testNumberParserError("00010100z", error.Overflow);
    try testNumberParserError("-00010100z", error.Overflow);
}

test {
    @import("std").testing.refAllDecls(@This());
}
