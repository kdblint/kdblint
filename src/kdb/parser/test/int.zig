const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_int = number_parser.null_int;
const inf_int = number_parser.inf_int;

test "valid int inputs" {
    try testNumberParser("0i", .int, @as(i32, 0));
    try testNumberParser("1i", .int, @as(i32, 1));
    try testNumberParser("-1i", .int, @as(i32, -1));
    try testNumberParser("2147483646i", .int, @as(i32, 2147483646));
    try testNumberParser("-2147483646i", .int, @as(i32, -2147483646));
    try testNumberParser("0Ni", .int, @as(i32, null_int));
    try testNumberParser("0ni", .int, @as(i32, null_int));
    try testNumberParser("0Wi", .int, @as(i32, inf_int));
    try testNumberParser("0wi", .int, @as(i32, inf_int));
    try testNumberParser("2147483647i", .int, @as(i32, inf_int));
    try testNumberParser("-0Wi", .int, @as(i32, -inf_int));
    try testNumberParser("-0wi", .int, @as(i32, -inf_int));
    try testNumberParser("-2147483647i", .int, @as(i32, -inf_int));
}

test "invalid int inputs" {
    try testNumberParserError("0ii", error.InvalidCharacter);
    try testNumberParserError("1ii", error.InvalidCharacter);
    try testNumberParserError("-1ii", error.InvalidCharacter);
    try testNumberParserError("2147483646ii", error.InvalidCharacter);
    try testNumberParserError("-2147483646ii", error.InvalidCharacter);
    try testNumberParserError(".1i", error.InvalidCharacter);
    try testNumberParserError("1.i", error.InvalidCharacter);
    try testNumberParserError("1.1i", error.InvalidCharacter);
    try testNumberParserError("2147483648i", error.Overflow);
    try testNumberParserError("-2147483648i", error.Overflow);
    try testNumberParserError("-2147483649i", error.Overflow);
}
