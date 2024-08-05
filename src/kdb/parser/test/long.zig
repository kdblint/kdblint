const number_parser = @import("../number_parser.zig");
const Parse = @import("../../Parse.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const testParse = Parse.testParse;
const null_long = number_parser.null_long;
const inf_long = number_parser.inf_long;

test "valid long inputs" {
    try testNumberParser("0j", .long, 0);
    try testNumberParser("1j", .long, 1);
    try testNumberParser("-1j", .long, -1);
    try testNumberParser("9223372036854775806j", .long, 9223372036854775806);
    try testNumberParser("-9223372036854775806j", .long, -9223372036854775806);
    try testNumberParser("0Nj", .long, null_long);
    try testNumberParser("0nj", .long, null_long);
    try testNumberParser("0Wj", .long, inf_long);
    try testNumberParser("0wj", .long, inf_long);
    try testNumberParser("9223372036854775807j", .long, inf_long);
    try testNumberParser("-0Wj", .long, -inf_long);
    try testNumberParser("-0wj", .long, -inf_long);
    try testNumberParser("-9223372036854775807j", .long, -inf_long);

    try testNumberParser("0", .long, 0);
    try testNumberParser("1", .long, 1);
    try testNumberParser("-1", .long, -1);
    try testNumberParser("9223372036854775806", .long, 9223372036854775806);
    try testNumberParser("-9223372036854775806", .long, -9223372036854775806);
    try testNumberParser("0N", .long, null_long);
    try testNumberParser("0W", .long, inf_long);
    try testNumberParser("9223372036854775807", .long, inf_long);
    try testNumberParser("-0W", .long, -inf_long);
    try testNumberParser("-9223372036854775807", .long, -inf_long);

    try testParse("0N 0N", &.{ .implicit_return, .long_list_literal }, "0N 0N");
    try testParse("0N 0Nj", &.{ .implicit_return, .long_list_literal }, "0N 0N");
    try testParse("0N 0nj", &.{ .implicit_return, .long_list_literal }, "0N 0N");
    try testParse("0n 0Nj", &.{ .implicit_return, .long_list_literal }, "0N 0N");
    try testParse("0n 0nj", &.{ .implicit_return, .long_list_literal }, "0N 0N");
    try testParse("0N 0N 0N", &.{ .implicit_return, .long_list_literal }, "0N 0N 0N");
    try testParse("0N 0N 0Nj", &.{ .implicit_return, .long_list_literal }, "0N 0N 0N");
    try testParse("0N 0N 0nj", &.{ .implicit_return, .long_list_literal }, "0N 0N 0N");
    try testParse("0N 0n 0Nj", &.{ .implicit_return, .long_list_literal }, "0N 0N 0N");
    try testParse("0N 0n 0nj", &.{ .implicit_return, .long_list_literal }, "0N 0N 0N");
    try testParse("0n 0N 0Nj", &.{ .implicit_return, .long_list_literal }, "0N 0N 0N");
    try testParse("0n 0N 0nj", &.{ .implicit_return, .long_list_literal }, "0N 0N 0N");
    try testParse("0n 0n 0Nj", &.{ .implicit_return, .long_list_literal }, "0N 0N 0N");
    try testParse("0n 0n 0nj", &.{ .implicit_return, .long_list_literal }, "0N 0N 0N");
}

test "invalid long inputs" {
    try testNumberParserError("0jj", error.InvalidCharacter);
    try testNumberParserError("1jj", error.InvalidCharacter);
    try testNumberParserError("-1jj", error.InvalidCharacter);
    try testNumberParserError("9223372036854775806jj", error.InvalidCharacter);
    try testNumberParserError("-9223372036854775806jj", error.InvalidCharacter);
    try testNumberParserError(".1j", error.InvalidCharacter);
    try testNumberParserError("1.j", error.InvalidCharacter);
    try testNumberParserError("1.1j", error.InvalidCharacter);
    try testNumberParserError("9223372036854775808j", error.Overflow);
    try testNumberParserError("-9223372036854775808j", error.Overflow);
    try testNumberParserError("-9223372036854775809j", error.Overflow);

    try testNumberParserError("9223372036854775808", error.Overflow);
    try testNumberParserError("-9223372036854775808", error.Overflow);
    try testNumberParserError("-9223372036854775809", error.Overflow);
}
