const number_parser = @import("../number_parser.zig");
const Parse = @import("../../Parse.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const testParse = Parse.testParse;

test "valid boolean inputs" {
    try testNumberParser("0b", .boolean, false);
    try testNumberParser("1b", .boolean, true);

    try testNumberParser("00b", .boolean_list, .{ false, false });
    try testNumberParser("01b", .boolean_list, .{ false, true });
    try testNumberParser("10b", .boolean_list, .{ true, false });
    try testNumberParser("11b", .boolean_list, .{ true, true });

    try testNumberParser("000b", .boolean_list, .{ false, false, false });
    try testNumberParser("001b", .boolean_list, .{ false, false, true });
    try testNumberParser("010b", .boolean_list, .{ false, true, false });
    try testNumberParser("011b", .boolean_list, .{ false, true, true });
    try testNumberParser("100b", .boolean_list, .{ true, false, false });
    try testNumberParser("101b", .boolean_list, .{ true, false, true });
    try testNumberParser("110b", .boolean_list, .{ true, true, false });
    try testNumberParser("111b", .boolean_list, .{ true, true, true });

    try testParse("01b 0", &.{
        .implicit_return,
        .implicit_apply,
        .long_literal,
        .boolean_list_literal,
    }, "(01b;0)");
    try testParse("0 1 01b 1", &.{
        .implicit_return,
        .implicit_apply,
        .implicit_apply,
        .long_literal,
        .boolean_list_literal,
        .long_list_literal,
    }, "(0 1;(01b;1))");

    try testParse("0 0b", &.{ .implicit_return, .implicit_apply, .boolean_literal, .long_literal }, "(0;0b)");
    try testParse("0 01b", &.{ .implicit_return, .implicit_apply, .boolean_list_literal, .long_literal }, "(0;01b)");
    try testParse("0 1 0b", &.{ .implicit_return, .implicit_apply, .boolean_literal, .long_list_literal }, "(0 1;0b)");
    try testParse("0 1 01b", &.{ .implicit_return, .implicit_apply, .boolean_list_literal, .long_list_literal }, "(0 1;01b)");

    try testParse("0f 0b", &.{ .implicit_return, .implicit_apply, .boolean_literal, .float_literal }, "(0f;0b)");
    try testParse("0f 01b", &.{ .implicit_return, .implicit_apply, .boolean_list_literal, .float_literal }, "(0f;01b)");
    try testParse("0 1f 0b", &.{ .implicit_return, .implicit_apply, .boolean_literal, .float_list_literal }, "(0 1f;0b)");
    try testParse("0 1f 01b", &.{ .implicit_return, .implicit_apply, .boolean_list_literal, .float_list_literal }, "(0 1f;01b)");

    try testParse("0b 0", &.{ .implicit_return, .implicit_apply, .long_literal, .boolean_literal }, "(0b;0)");
    try testParse("0b 0 1", &.{ .implicit_return, .implicit_apply, .long_list_literal, .boolean_literal }, "(0b;0 1)");
    try testParse("01b 0", &.{ .implicit_return, .implicit_apply, .long_literal, .boolean_list_literal }, "(01b;0)");
    try testParse("01b 0 1", &.{ .implicit_return, .implicit_apply, .long_list_literal, .boolean_list_literal }, "(01b;0 1)");
}

test "invalid boolean inputs" {
    try testNumberParserError("0bb", error.InvalidCharacter);
    try testNumberParserError("1bb", error.InvalidCharacter);

    try testNumberParserError("00bb", error.InvalidCharacter);
    try testNumberParserError("01bb", error.InvalidCharacter);
    try testNumberParserError("10bb", error.InvalidCharacter);
    try testNumberParserError("11bb", error.InvalidCharacter);

    try testNumberParserError("000bb", error.InvalidCharacter);
    try testNumberParserError("001bb", error.InvalidCharacter);
    try testNumberParserError("010bb", error.InvalidCharacter);
    try testNumberParserError("011bb", error.InvalidCharacter);
    try testNumberParserError("100bb", error.InvalidCharacter);
    try testNumberParserError("101bb", error.InvalidCharacter);
    try testNumberParserError("110bb", error.InvalidCharacter);
    try testNumberParserError("111bb", error.InvalidCharacter);

    try testNumberParserError("2b", error.InvalidCharacter);
    try testNumberParserError(".1b", error.InvalidCharacter);
    try testNumberParserError("1.b", error.InvalidCharacter);
    try testNumberParserError("1.1b", error.InvalidCharacter);
    try testNumberParserError("1b0", error.InvalidCharacter);
    try testNumberParserError("10b0", error.InvalidCharacter);
}
