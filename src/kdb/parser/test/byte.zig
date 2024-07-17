const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;

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
}

test "invalid byte inputs" {
    try testNumberParserError("0X", error.InvalidCharacter);
    try testNumberParserError("0xg", error.InvalidCharacter);
    try testNumberParserError("0xG", error.InvalidCharacter);
    try testNumberParserError("0xgg", error.InvalidCharacter);
    try testNumberParserError("0xGG", error.InvalidCharacter);
}
