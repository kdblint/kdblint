const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_short = number_parser.null_short;
const inf_short = number_parser.inf_short;

test "valid short inputs" {
    try testNumberParser("0h", .short, @as(i16, 0));
    try testNumberParser("1h", .short, @as(i16, 1));
    try testNumberParser("-1h", .short, @as(i16, -1));
    try testNumberParser("32766h", .short, @as(i16, 32766));
    try testNumberParser("-32766h", .short, @as(i16, -32766));
    try testNumberParser("0Nh", .short, @as(i16, null_short));
    try testNumberParser("0nh", .short, @as(i16, null_short));
    try testNumberParser("0Wh", .short, @as(i16, inf_short));
    try testNumberParser("0wh", .short, @as(i16, inf_short));
    try testNumberParser("32767h", .short, @as(i16, inf_short));
    try testNumberParser("-0Wh", .short, @as(i16, -inf_short));
    try testNumberParser("-0wh", .short, @as(i16, -inf_short));
    try testNumberParser("-32767h", .short, @as(i16, -inf_short));
}

test "invalid short inputs" {
    try testNumberParserError("0hh", error.InvalidCharacter);
    try testNumberParserError("1hh", error.InvalidCharacter);
    try testNumberParserError("-1hh", error.InvalidCharacter);
    try testNumberParserError("32766hh", error.InvalidCharacter);
    try testNumberParserError("-32766hh", error.InvalidCharacter);
    try testNumberParserError(".1h", error.InvalidCharacter);
    try testNumberParserError("1.h", error.InvalidCharacter);
    try testNumberParserError("1.1h", error.InvalidCharacter);
    try testNumberParserError("32768h", error.Overflow);
    try testNumberParserError("-32768h", error.Overflow);
    try testNumberParserError("-32769h", error.Overflow);
}
