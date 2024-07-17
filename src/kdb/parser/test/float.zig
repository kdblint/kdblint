const number_parser = @import("../number_parser.zig");
const Parse = @import("../../Parse.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const testParse = Parse.testParse;
const null_float = number_parser.null_float;
const inf_float = number_parser.inf_float;

test "valid float inputs" {
    try testNumberParser("0f", .float, @as(f64, 0));
    try testNumberParser("1f", .float, @as(f64, 1));
    try testNumberParser("-1f", .float, @as(f64, -1));
    try testNumberParser(".1f", .float, @as(f64, 0.1));
    try testNumberParser("1.f", .float, @as(f64, 1));
    try testNumberParser("1.1f", .float, @as(f64, 1.1));
    try testNumberParser("-.1f", .float, @as(f64, -0.1));
    try testNumberParser("-1.f", .float, @as(f64, -1));
    try testNumberParser("-1.1f", .float, @as(f64, -1.1));
    try testNumberParser("0Nf", .float, @as(f64, null_float));
    try testNumberParser("0nf", .float, @as(f64, null_float));
    try testNumberParser("0Wf", .float, @as(f64, inf_float));
    try testNumberParser("0wf", .float, @as(f64, inf_float));
    try testNumberParser("-0Wf", .float, @as(f64, -inf_float));
    try testNumberParser("-0wf", .float, @as(f64, -inf_float));

    try testNumberParser(".1", .float, @as(f64, 0.1));
    try testNumberParser("1.", .float, @as(f64, 1));
    try testNumberParser("1.1", .float, @as(f64, 1.1));
    try testNumberParser("-.1", .float, @as(f64, -0.1));
    try testNumberParser("-1.", .float, @as(f64, -1));
    try testNumberParser("-1.1", .float, @as(f64, -1.1));
    try testNumberParser("0n", .float, @as(f64, null_float));
    try testNumberParser("0w", .float, @as(f64, inf_float));
    try testNumberParser("-0w", .float, @as(f64, -inf_float));

    try testParse("0n 0n", &.{ .implicit_return, .float_list_literal }, "0n 0n");
    try testParse("0n 0N", &.{ .implicit_return, .float_list_literal }, "0n 0n");
    try testParse("0N 0n", &.{ .implicit_return, .float_list_literal }, "0n 0n");
    try testParse("0n 0n 0n", &.{ .implicit_return, .float_list_literal }, "0n 0n 0n");
    try testParse("0n 0n 0N", &.{ .implicit_return, .float_list_literal }, "0n 0n 0n");
    try testParse("0n 0N 0n", &.{ .implicit_return, .float_list_literal }, "0n 0n 0n");
    try testParse("0n 0N 0N", &.{ .implicit_return, .float_list_literal }, "0n 0n 0n");
    try testParse("0N 0n 0n", &.{ .implicit_return, .float_list_literal }, "0n 0n 0n");
    try testParse("0N 0n 0N", &.{ .implicit_return, .float_list_literal }, "0n 0n 0n");
    try testParse("0N 0N 0n", &.{ .implicit_return, .float_list_literal }, "0n 0n 0n");

    try testParse("0 1f", &.{ .implicit_return, .float_list_literal }, "0 1f");
    try testParse("0 .1", &.{ .implicit_return, .float_list_literal }, "0 0.1");
}

test "invalid float inputs" {
    try testNumberParserError("0ff", error.InvalidCharacter);
    try testNumberParserError("1ff", error.InvalidCharacter);
    try testNumberParserError("-1ff", error.InvalidCharacter);
    try testNumberParserError(".1ff", error.InvalidCharacter);
    try testNumberParserError("1.ff", error.InvalidCharacter);
    try testNumberParserError("1.1ff", error.InvalidCharacter);
    try testNumberParserError("-.1ff", error.InvalidCharacter);
    try testNumberParserError("-1.ff", error.InvalidCharacter);
    try testNumberParserError("-1.1ff", error.InvalidCharacter);
}
