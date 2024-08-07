const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_int = number_parser.null_int;
const inf_int = number_parser.inf_int;

test "valid minute inputs" {
    try testNumberParser("0nu", .minute, null_int);
    try testNumberParser("0Nu", .minute, null_int);
    try testNumberParser("0wu", .minute, inf_int);
    try testNumberParser("0Wu", .minute, inf_int);
    try testNumberParser("-0wu", .minute, -inf_int);
    try testNumberParser("-0Wu", .minute, -inf_int);

    try testNumberParser("0u", .minute, 0);
    try testNumberParser("9u", .minute, 540);
    try testNumberParser("-9u", .minute, -540);

    try testNumberParser("00u", .minute, 0);
    try testNumberParser("99u", .minute, 5940);
    try testNumberParser("-99u", .minute, -5940);

    try testNumberParser("000u", .minute, 0);
    try testNumberParser("959u", .minute, 599);
    try testNumberParser("-959u", .minute, -599);

    try testNumberParser("0000u", .minute, 0);
    try testNumberParser("9959u", .minute, 5999);
    try testNumberParser("-9959u", .minute, -5999);

    try testNumberParser("00000u", .minute, 0);
    try testNumberParser("99959u", .minute, 59999);
    try testNumberParser("-99959u", .minute, -59999);

    try testNumberParser("000000000000u", .minute, 0);
    try testNumberParser("214748364759u", .minute, -1);
    try testNumberParser("-214748364759u", .minute, 1);

    try testNumberParser("00:", .minute, 0);
    try testNumberParser("99:", .minute, 5940);
    try testNumberParser("-99:", .minute, -5940);
    try testNumberParser("00:00", .minute, 0);
    try testNumberParser("99:59", .minute, 5999);
    try testNumberParser("-99:59", .minute, -5999);
    try testNumberParser("00:00:", .minute, 0);
    try testNumberParser("99:59:", .minute, 5999);
    try testNumberParser("-99:59:", .minute, -5999);

    try testNumberParser("000:", .minute, 0);
    try testNumberParser("999:", .minute, 59940);
    try testNumberParser("-999:", .minute, -59940);
    try testNumberParser("000:00", .minute, 0);
    try testNumberParser("999:59", .minute, 59999);
    try testNumberParser("-999:59", .minute, -59999);

    try testNumberParser("0000:", .minute, 0);
    try testNumberParser("9999:", .minute, 599940);
    try testNumberParser("-9999:", .minute, -599940);

    try testNumberParser("00000:", .minute, 0);
    try testNumberParser("99999:", .minute, 5999940);
    try testNumberParser("-99999:", .minute, -5999940);
}

test "invalid minute inputs" {
    try testNumberParserError("00:0", error.InvalidCharacter);
    try testNumberParserError("00:60", error.Overflow);
    try testNumberParserError("000:0", error.InvalidCharacter);
    try testNumberParserError("000:60", error.Overflow);

    try testNumberParserError("060u", error.Overflow);
    try testNumberParserError("960u", error.Overflow);
    try testNumberParserError("-960u", error.Overflow);

    try testNumberParserError("0060u", error.Overflow);
    try testNumberParserError("9960u", error.Overflow);
    try testNumberParserError("-9960u", error.Overflow);

    try testNumberParserError("000000000060u", error.Overflow);
    try testNumberParserError("214748364859u", error.Overflow);
    try testNumberParserError("214748364760u", error.Overflow);
    try testNumberParserError("-214748364859u", error.Overflow);
    try testNumberParserError("-214748364760u", error.Overflow);
}
