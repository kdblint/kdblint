const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_int = number_parser.null_int;
const inf_int = number_parser.inf_int;

test "valid month inputs" {
    try testNumberParser("1999.12m", .month, @as(i32, -1));
    try testNumberParser("2000.01m", .month, @as(i32, 0));
    try testNumberParser("2000.02m", .month, @as(i32, 1));
    try testNumberParser("-1999.12m", .month, @as(i32, 1));
    try testNumberParser("0001.01m", .month, @as(i32, -23988));
    try testNumberParser("9999.12m", .month, @as(i32, 95999));
    try testNumberParser("0Nm", .month, @as(i32, null_int));
    try testNumberParser("0nm", .month, @as(i32, null_int));
    try testNumberParser("0Wm", .month, @as(i32, inf_int));
    try testNumberParser("0wm", .month, @as(i32, inf_int));
    try testNumberParser("-0Wm", .month, @as(i32, -inf_int));
    try testNumberParser("-0wm", .month, @as(i32, -inf_int));

    try testNumberParser("0001m", .month, @as(i32, 0));
    try testNumberParser("0012m", .month, @as(i32, 11));
    try testNumberParser("4912m", .month, @as(i32, 599));
    try testNumberParser("5001m", .month, @as(i32, -600));
    try testNumberParser("9901m", .month, @as(i32, -12));
    try testNumberParser("9912m", .month, @as(i32, -1));

    try testNumberParser("000101m", .month, @as(i32, -23988));
    try testNumberParser("000112m", .month, @as(i32, -23977));
    try testNumberParser("999901m", .month, @as(i32, 95988));
    try testNumberParser("999912m", .month, @as(i32, 95999));
}

test "invalid month inputs" {
    try testNumberParserError("0m", error.InvalidCharacter);
    try testNumberParserError("1m", error.InvalidCharacter);
    try testNumberParserError("9m", error.InvalidCharacter);

    try testNumberParserError("00m", error.InvalidCharacter);
    try testNumberParserError("11m", error.InvalidCharacter);
    try testNumberParserError("99m", error.InvalidCharacter);

    try testNumberParserError("000m", error.InvalidCharacter);
    try testNumberParserError("111m", error.InvalidCharacter);
    try testNumberParserError("999m", error.InvalidCharacter);

    try testNumberParserError("0000m", error.Overflow);
    try testNumberParserError("0013m", error.Overflow);
    try testNumberParserError("9999m", error.Overflow);

    try testNumberParserError("00000m", error.InvalidCharacter);
    try testNumberParserError("11111m", error.InvalidCharacter);
    try testNumberParserError("99999m", error.InvalidCharacter);

    try testNumberParserError("000000m", error.Overflow);
    try testNumberParserError("000113m", error.Overflow);
    try testNumberParserError("999999m", error.Overflow);

    try testNumberParserError("1111111m", error.InvalidCharacter);

    try testNumberParserError("-1111111m", error.InvalidCharacter);

    try testNumberParserError("2000.1m", error.InvalidCharacter);
    try testNumberParserError("2000.00m", error.Overflow);
    try testNumberParserError("2000.13m", error.Overflow);
}
