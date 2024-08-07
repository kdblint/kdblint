const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_int = number_parser.null_int;
const inf_int = number_parser.inf_int;

test "valid second inputs" {
    try testNumberParser("0nv", .second, null_int);
    try testNumberParser("0Nv", .second, null_int);
    try testNumberParser("0wv", .second, inf_int);
    try testNumberParser("0Wv", .second, inf_int);
    try testNumberParser("-0wv", .second, -inf_int);
    try testNumberParser("-0Wv", .second, -inf_int);

    try testNumberParser("0v", .second, 0);
    try testNumberParser("9v", .second, 32400);
    try testNumberParser("-9v", .second, -32400);

    try testNumberParser("00v", .second, 0);
    try testNumberParser("99v", .second, 356400);
    try testNumberParser("-99v", .second, -356400);

    try testNumberParser("000v", .second, 0);
    try testNumberParser("959v", .second, 35940);
    try testNumberParser("-959v", .second, -35940);

    try testNumberParser("0000v", .second, 0);
    try testNumberParser("9959v", .second, 359940);
    try testNumberParser("-9959v", .second, -359940);

    try testNumberParser("00000v", .second, 0);
    try testNumberParser("99959v", .second, 3599940);
    try testNumberParser("-99959v", .second, -3599940);

    try testNumberParser("000000v", .second, 0);
    try testNumberParser("995959v", .second, 359999);
    try testNumberParser("-995959v", .second, -359999);

    try testNumberParser("0000000v", .second, 0);
    try testNumberParser("9995959v", .second, 3599999);
    try testNumberParser("-9995959v", .second, -3599999);

    try testNumberParser("00000000000000v", .second, 0);
    try testNumberParser("21474836475959v", .second, -1);
    try testNumberParser("-21474836475959v", .second, 1);

    try testNumberParser("00:00:00", .second, 0);
    try testNumberParser("99:59:59", .second, 359999);
    try testNumberParser("-99:59:59", .second, -359999);

    try testNumberParser("000:00:00", .second, 0);
    try testNumberParser("999:59:59", .second, 3599999);
    try testNumberParser("-999:59:59", .second, -3599999);
}

test "invalid second inputs" {
    try testNumberParserError("00:0:0", error.InvalidCharacter);
    try testNumberParserError("00:0:00", error.InvalidCharacter);
    try testNumberParserError("00:00:0", error.InvalidCharacter);

    try testNumberParserError("00:60:00", error.Overflow);
    try testNumberParserError("00:00:60", error.Overflow);
    try testNumberParserError("99:60:59", error.Overflow);
    try testNumberParserError("99:59:60", error.Overflow);

    try testNumberParserError("000:60:00", error.Overflow);
    try testNumberParserError("000:00:60", error.Overflow);
    try testNumberParserError("999:60:59", error.Overflow);
    try testNumberParserError("999:59:60", error.Overflow);

    try testNumberParserError("060v", error.Overflow);
    try testNumberParserError("960v", error.Overflow);
    try testNumberParserError("-960v", error.Overflow);

    try testNumberParserError("0060v", error.Overflow);
    try testNumberParserError("9960v", error.Overflow);
    try testNumberParserError("-9960v", error.Overflow);

    try testNumberParserError("00060v", error.Overflow);
    try testNumberParserError("99960v", error.Overflow);
    try testNumberParserError("-99960v", error.Overflow);

    try testNumberParserError("006000v", error.Overflow);
    try testNumberParserError("000060v", error.Overflow);
    try testNumberParserError("996059v", error.Overflow);
    try testNumberParserError("995960v", error.Overflow);
    try testNumberParserError("-996059v", error.Overflow);
    try testNumberParserError("-995960v", error.Overflow);

    try testNumberParserError("000000006000v", error.Overflow);
    try testNumberParserError("000000000060v", error.Overflow);
    try testNumberParserError("21474836485959v", error.Overflow);
    try testNumberParserError("21474836476059v", error.Overflow);
    try testNumberParserError("21474836475960v", error.Overflow);
    try testNumberParserError("-21474836485959v", error.Overflow);
    try testNumberParserError("-21474836476059v", error.Overflow);
    try testNumberParserError("-21474836475960v", error.Overflow);
}
