const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_char = number_parser.null_char;

test "valid char inputs" {
    try testNumberParser("0c", .char, @as(u8, '0'));
    try testNumberParser("1c", .char, @as(u8, '1'));
    try testNumberParser("2c", .char, @as(u8, '2'));
    try testNumberParser("3c", .char, @as(u8, '3'));
    try testNumberParser("4c", .char, @as(u8, '4'));
    try testNumberParser("5c", .char, @as(u8, '5'));
    try testNumberParser("6c", .char, @as(u8, '6'));
    try testNumberParser("7c", .char, @as(u8, '7'));
    try testNumberParser("8c", .char, @as(u8, '8'));
    try testNumberParser("9c", .char, @as(u8, '9'));
    try testNumberParser("0Nc", .char, @as(u8, null_char));
    try testNumberParser("0nc", .char, @as(u8, null_char));
}

test "invalid char inputs" {
    try testNumberParserError("-1c", error.InvalidCharacter);
    try testNumberParserError(".1c", error.InvalidCharacter);
    try testNumberParserError("1.c", error.InvalidCharacter);
    try testNumberParserError("1.1c", error.InvalidCharacter);
    try testNumberParserError("-.1c", error.InvalidCharacter);
    try testNumberParserError("-1.c", error.InvalidCharacter);
    try testNumberParserError("-1.1c", error.InvalidCharacter);
    try testNumberParserError("0Wc", error.InvalidCharacter);
    try testNumberParserError("0wc", error.InvalidCharacter);
    try testNumberParserError("-0Wc", error.InvalidCharacter);
    try testNumberParserError("-0wc", error.InvalidCharacter);
}
