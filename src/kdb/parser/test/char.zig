const number_parser = @import("../number_parser.zig");
const Parse = @import("../../Parse.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const testParse = Parse.testParse;
const testParseError = Parse.testParseError;
const null_char = number_parser.null_char;

test "valid char inputs" {
    try testNumberParser("0c", .char, '0');
    try testNumberParser("1c", .char, '1');
    try testNumberParser("2c", .char, '2');
    try testNumberParser("3c", .char, '3');
    try testNumberParser("4c", .char, '4');
    try testNumberParser("5c", .char, '5');
    try testNumberParser("6c", .char, '6');
    try testNumberParser("7c", .char, '7');
    try testNumberParser("8c", .char, '8');
    try testNumberParser("9c", .char, '9');
    try testNumberParser("0Nc", .char, null_char);
    try testNumberParser("0nc", .char, null_char);
    try testNumberParser("0Wc", .char, null_char);
    try testNumberParser("0wc", .char, null_char);
    try testNumberParser("-0Wc", .char, null_char);
    try testNumberParser("-0wc", .char, null_char);

    try testNumberParser("-1c", .char, null_char);
    try testNumberParser(".1c", .char, null_char);
    try testNumberParser("1.c", .char, null_char);
    try testNumberParser("1.1c", .char, null_char);
    try testNumberParser("-.1c", .char, null_char);
    try testNumberParser("-1.c", .char, null_char);
    try testNumberParser("-1.1c", .char, null_char);

    try testParse("1c", &.{ .implicit_return, .char_number_literal }, "\"1\"");
    try testParse("1.c", &.{ .implicit_return, .char_number_literal }, "\" \"");

    try testParse("1 2c", &.{ .implicit_return, .char_number_list_literal }, "\"12\"");
    try testParse("1 2.c", &.{ .implicit_return, .char_number_list_literal }, "\"1 \"");
    try testParse("1. 2c", &.{ .implicit_return, .char_number_list_literal }, "\" 2\"");
    try testParse("1. 2.c", &.{ .implicit_return, .char_number_list_literal }, "\"  \"");

    try testParse("1 2 3c", &.{ .implicit_return, .char_number_list_literal }, "\"123\"");
    try testParse("1 2 3.c", &.{ .implicit_return, .char_number_list_literal }, "\"12 \"");
    try testParse("1 2. 3c", &.{ .implicit_return, .char_number_list_literal }, "\"1 3\"");
    try testParse("1 2. 3.c", &.{ .implicit_return, .char_number_list_literal }, "\"1  \"");
    try testParse("1. 2 3c", &.{ .implicit_return, .char_number_list_literal }, "\" 23\"");
    try testParse("1. 2 3.c", &.{ .implicit_return, .char_number_list_literal }, "\" 2 \"");
    try testParse("1. 2. 3c", &.{ .implicit_return, .char_number_list_literal }, "\"  3\"");
    try testParse("1. 2. 3.c", &.{ .implicit_return, .char_number_list_literal }, "\"   \"");

    try testParse("0Nc", &.{ .implicit_return, .char_number_literal }, "\" \"");
    try testParse("0nc", &.{ .implicit_return, .char_number_literal }, "\" \"");

    try testParse("0N 0Nc", &.{ .implicit_return, .char_number_list_literal }, "\"  \"");
    try testParse("0N 0nc", &.{ .implicit_return, .char_number_list_literal }, "\"  \"");
    try testParse("0n 0Nc", &.{ .implicit_return, .char_number_list_literal }, "\"  \"");
    try testParse("0n 0nc", &.{ .implicit_return, .char_number_list_literal }, "\"  \"");

    try testParse("0N 0N 0Nc", &.{ .implicit_return, .char_number_list_literal }, "\"   \"");
    try testParse("0N 0N 0nc", &.{ .implicit_return, .char_number_list_literal }, "\"   \"");
    try testParse("0N 0n 0Nc", &.{ .implicit_return, .char_number_list_literal }, "\"   \"");
    try testParse("0N 0n 0nc", &.{ .implicit_return, .char_number_list_literal }, "\"   \"");
    try testParse("0n 0N 0Nc", &.{ .implicit_return, .char_number_list_literal }, "\"   \"");
    try testParse("0n 0N 0nc", &.{ .implicit_return, .char_number_list_literal }, "\"   \"");
    try testParse("0n 0n 0Nc", &.{ .implicit_return, .char_number_list_literal }, "\"   \"");
    try testParse("0n 0n 0nc", &.{ .implicit_return, .char_number_list_literal }, "\"   \"");
}

test {
    @import("std").testing.refAllDecls(@This());
}
