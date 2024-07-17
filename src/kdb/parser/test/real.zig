const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_real = number_parser.null_real;
const inf_real = number_parser.inf_real;

test "valid real inputs" {
    try testNumberParser("0e", .real, @as(f32, 0));
    try testNumberParser("1e", .real, @as(f32, 1));
    try testNumberParser("-1e", .real, @as(f32, -1));
    try testNumberParser(".1e", .real, @as(f32, 0.1));
    try testNumberParser("1.e", .real, @as(f32, 1));
    try testNumberParser("1.1e", .real, @as(f32, 1.1));
    try testNumberParser("-.1e", .real, @as(f32, -0.1));
    try testNumberParser("-1.e", .real, @as(f32, -1));
    try testNumberParser("-1.1e", .real, @as(f32, -1.1));
    try testNumberParser("0Ne", .real, @as(f32, null_real));
    try testNumberParser("0ne", .real, @as(f32, null_real));
    try testNumberParser("0We", .real, @as(f32, inf_real));
    try testNumberParser("0we", .real, @as(f32, inf_real));
    try testNumberParser("-0We", .real, @as(f32, -inf_real));
    try testNumberParser("-0we", .real, @as(f32, -inf_real));
}

test "invalid real inputs" {
    try testNumberParserError("0ee", error.InvalidCharacter);
    try testNumberParserError("1ee", error.InvalidCharacter);
    try testNumberParserError("-1ee", error.InvalidCharacter);
    try testNumberParserError(".1ee", error.InvalidCharacter);
    try testNumberParserError("1.ee", error.InvalidCharacter);
    try testNumberParserError("1.1ee", error.InvalidCharacter);
    try testNumberParserError("-.1ee", error.InvalidCharacter);
    try testNumberParserError("-1.ee", error.InvalidCharacter);
    try testNumberParserError("-1.1ee", error.InvalidCharacter);
}
