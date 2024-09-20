const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_float = number_parser.null_float;
const inf_float = number_parser.inf_float;

test "valid datetime inputs" {
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
