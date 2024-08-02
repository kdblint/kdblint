const number_parser = @import("../number_parser.zig");
const Parse = @import("../../Parse.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const testParse = Parse.testParse;

test "valid byte inputs" {
    try testNumberParser("0x0", .byte, @as(u8, 0));
    try testNumberParser("0x00", .byte, @as(u8, 0));
    try testNumberParser("0x1", .byte, @as(u8, 1));
    try testNumberParser("0x01", .byte, @as(u8, 1));
    try testNumberParser("0xf", .byte, @as(u8, 15));
    try testNumberParser("0x0f", .byte, @as(u8, 15));
    try testNumberParser("0xF", .byte, @as(u8, 15));
    try testNumberParser("0x0F", .byte, @as(u8, 15));
    try testNumberParser("0x10", .byte, @as(u8, 16));
    try testNumberParser("0xf0", .byte, @as(u8, 240));
    try testNumberParser("0xF0", .byte, @as(u8, 240));
    try testNumberParser("0xff", .byte, @as(u8, 255));
    try testNumberParser("0xFF", .byte, @as(u8, 255));

    try testNumberParser("0x", .byte_list, .{});

    try testParse("0 0x00", &.{ .implicit_return, .implicit_apply, .byte_literal, .long_literal }, "(0;0x00)");
    try testParse("0 0x", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .long_literal }, "(0;`byte$())");
    try testParse("0 0x0001", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .long_literal }, "(0;0x0001)");
    try testParse("0 1 0x00", &.{ .implicit_return, .implicit_apply, .byte_literal, .long_list_literal }, "(0 1;0x00)");
    try testParse("0 1 0x", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .long_list_literal }, "(0 1;`byte$())");
    try testParse("0 1 0x0001", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .long_list_literal }, "(0 1;0x0001)");

    try testParse("0f 0x00", &.{ .implicit_return, .implicit_apply, .byte_literal, .float_literal }, "(0f;0x00)");
    try testParse("0f 0x", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .float_literal }, "(0f;`byte$())");
    try testParse("0f 0x0001", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .float_literal }, "(0f;0x0001)");
    try testParse("0 1f 0x00", &.{ .implicit_return, .implicit_apply, .byte_literal, .float_list_literal }, "(0 1f;0x00)");
    try testParse("0 1f 0x", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .float_list_literal }, "(0 1f;`byte$())");
    try testParse("0 1f 0x0001", &.{ .implicit_return, .implicit_apply, .byte_list_literal, .float_list_literal }, "(0 1f;0x0001)");

    try testParse("0x00 0", &.{ .implicit_return, .implicit_apply, .long_literal, .byte_literal }, "(0x00;0)");
    try testParse("0x00 0 1", &.{ .implicit_return, .implicit_apply, .long_list_literal, .byte_literal }, "(0x00;0 1)");
    try testParse("0x 0", &.{ .implicit_return, .implicit_apply, .long_literal, .byte_list_literal }, "(`byte$();0)");
    try testParse("0x 0 1", &.{ .implicit_return, .implicit_apply, .long_list_literal, .byte_list_literal }, "(`byte$();0 1)");
    try testParse("0x0001 0", &.{ .implicit_return, .implicit_apply, .long_literal, .byte_list_literal }, "(0x0001;0)");
    try testParse("0x0001 0 1", &.{ .implicit_return, .implicit_apply, .long_list_literal, .byte_list_literal }, "(0x0001;0 1)");
}

test "invalid byte inputs" {
    try testNumberParserError("0X", error.InvalidCharacter);
    try testNumberParserError("0xg", error.InvalidCharacter);
    try testNumberParserError("0xG", error.InvalidCharacter);
    try testNumberParserError("0xgg", error.InvalidCharacter);
    try testNumberParserError("0xGG", error.InvalidCharacter);
}
