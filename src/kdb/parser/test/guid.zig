const number_parser = @import("../number_parser.zig");
const Parse = @import("../../Parse.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const testParse = Parse.testParse;
const testParseError = Parse.testParseError;
const null_guid = number_parser.null_guid;

test "valid guid inputs" {
    try testNumberParser("0ng", .guid, null_guid);
    try testNumberParser("0Ng", .guid, null_guid);

    try testParse("0ng", &.{ .implicit_return, .guid_literal }, "00000000-0000-0000-0000-000000000000");
    try testParse("0Ng", &.{ .implicit_return, .guid_literal }, "00000000-0000-0000-0000-000000000000");

    try testParse(
        "0N 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0N 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0N 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0n 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0N 0n 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0N 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0N 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0n 0Ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
    try testParse(
        "0n 0n 0ng",
        &.{ .implicit_return, .guid_list_literal },
        "00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000 00000000-0000-0000-0000-000000000000",
    );
}

test "invalid guid inputs" {
    try testNumberParserError("0wg", error.InvalidCharacter);
    try testNumberParserError("0Wg", error.InvalidCharacter);

    try testParseError("1 0Ng", &.{.parse_error});
    try testParseError("1 2 0Ng", &.{.parse_error});
    try testParseError("1. 0Ng", &.{.parse_error});
    try testParseError("1. 2. 0Ng", &.{.parse_error});
    try testParseError("1f 0Ng", &.{.parse_error});
    try testParseError("1 2f 0Ng", &.{.parse_error});
}

test {
    @import("std").testing.refAllDecls(@This());
}
