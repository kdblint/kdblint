const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_int = number_parser.null_int;
const inf_int = number_parser.inf_int;

test "valid time inputs" {
    try testNumberParser("0nt", .time, null_int);
    try testNumberParser("0Nt", .time, null_int);
    try testNumberParser("0wt", .time, inf_int);
    try testNumberParser("0Wt", .time, inf_int);
    try testNumberParser("-0wt", .time, -inf_int);
    try testNumberParser("-0Wt", .time, -inf_int);

    try testNumberParser("0t", .time, 0);
    try testNumberParser("9t", .time, 32400000);
    try testNumberParser("-9t", .time, -32400000);

    try testNumberParser("00t", .time, 0);
    try testNumberParser("99t", .time, 356400000);
    try testNumberParser("-99t", .time, -356400000);

    try testNumberParser("000t", .time, 0);
    try testNumberParser("959t", .time, 35940000);
    try testNumberParser("-959t", .time, -35940000);

    try testNumberParser("0000t", .time, 0);
    try testNumberParser("9959t", .time, 359940000);
    try testNumberParser("-9959t", .time, -359940000);

    try testNumberParser("00000t", .time, 0);
    try testNumberParser("99959t", .time, -695027296);
    try testNumberParser("-99959t", .time, 695027296);

    try testNumberParser("000000t", .time, 0);
    try testNumberParser("995959t", .time, 359999000);
    try testNumberParser("-995959t", .time, -359999000);

    try testNumberParser("0000000t", .time, 0);
    try testNumberParser("9995959t", .time, -694968296);
    try testNumberParser("-9995959t", .time, 694968296);

    try testNumberParser("00000000t", .time, 0);
    try testNumberParser("99995959t", .time, 1640260632);
    try testNumberParser("-99995959t", .time, -1640260632);

    try testNumberParser("000000000t", .time, 0);
    try testNumberParser("995959999t", .time, 359999999);
    try testNumberParser("-995959999t", .time, -359999999);

    try testNumberParser("00000000000000t", .time, 0);
    try testNumberParser("21474836475959t", .time, -1000);
    try testNumberParser("-21474836475959t", .time, 1000);

    try testNumberParser("00:00:00.", .time, 0);
    try testNumberParser("00:00:00.0", .time, 0);
    try testNumberParser("00:00:00.00", .time, 0);
    try testNumberParser("00:00:00.000", .time, 0);
    try testNumberParser("00:00:00.0000", .time, 0);
    try testNumberParser("99:59:59.", .time, 359999000);
    try testNumberParser("99:59:59.9", .time, 359999900);
    try testNumberParser("99:59:59.99", .time, 359999990);
    try testNumberParser("99:59:59.999", .time, 359999999);
    try testNumberParser("99:59:59.9999", .time, 359999999);
    try testNumberParser("-99:59:59.", .time, -359999000);
    try testNumberParser("-99:59:59.9", .time, -359999900);
    try testNumberParser("-99:59:59.99", .time, -359999990);
    try testNumberParser("-99:59:59.999", .time, -359999999);
    try testNumberParser("-99:59:59.9999", .time, -359999999);

    try testNumberParser("000:00:00.", .time, 0);
    try testNumberParser("000:00:00.0", .time, 0);
    try testNumberParser("000:00:00.00", .time, 0);
    try testNumberParser("000:00:00.000", .time, 0);
    try testNumberParser("000:00:00.0000", .time, 0);
    try testNumberParser("999:59:59.", .time, -694968296);
    try testNumberParser("999:59:59.9", .time, -694967396);
    try testNumberParser("999:59:59.99", .time, -694967306);
    try testNumberParser("999:59:59.999", .time, -694967297);
    try testNumberParser("-999:59:59.", .time, 694968296);
    try testNumberParser("-999:59:59.9", .time, 694967396);
    try testNumberParser("-999:59:59.99", .time, 694967306);
    try testNumberParser("-999:59:59.999", .time, 694967297);
}

test "invalid time inputs" {
    try testNumberParserError("060t", error.Overflow);
    try testNumberParserError("960t", error.Overflow);
    try testNumberParserError("-960t", error.Overflow);

    try testNumberParserError("0060t", error.Overflow);
    try testNumberParserError("9960t", error.Overflow);
    try testNumberParserError("-9960t", error.Overflow);

    try testNumberParserError("00060t", error.Overflow);
    try testNumberParserError("99960t", error.Overflow);
    try testNumberParserError("-99960t", error.Overflow);

    try testNumberParserError("006000t", error.Overflow);
    try testNumberParserError("000060t", error.Overflow);
    try testNumberParserError("996059t", error.Overflow);
    try testNumberParserError("995960t", error.Overflow);
    try testNumberParserError("-996059t", error.Overflow);
    try testNumberParserError("-995960t", error.Overflow);

    try testNumberParserError("0006000t", error.Overflow);
    try testNumberParserError("0000060t", error.Overflow);
    try testNumberParserError("9996059t", error.Overflow);
    try testNumberParserError("9995960t", error.Overflow);
    try testNumberParserError("-9996059t", error.Overflow);
    try testNumberParserError("-9995960t", error.Overflow);

    try testNumberParserError("00006000t", error.Overflow);
    try testNumberParserError("00000060t", error.Overflow);
    try testNumberParserError("99996059t", error.Overflow);
    try testNumberParserError("99995960t", error.Overflow);
    try testNumberParserError("-99996059t", error.Overflow);
    try testNumberParserError("-99995960t", error.Overflow);

    try testNumberParserError("006000000t", error.Overflow);
    try testNumberParserError("000060000t", error.Overflow);
    try testNumberParserError("996059999t", error.Overflow);
    try testNumberParserError("995960999t", error.Overflow);
    try testNumberParserError("-996059999t", error.Overflow);
    try testNumberParserError("-995960999t", error.Overflow);

    try testNumberParserError("00000000006000t", error.Overflow);
    try testNumberParserError("00000000000060t", error.Overflow);
    try testNumberParserError("21474836476059t", error.Overflow);
    try testNumberParserError("21474836475960t", error.Overflow);
    try testNumberParserError("-21474836476059t", error.Overflow);
    try testNumberParserError("-21474836475960t", error.Overflow);

    try testNumberParserError("00:0:0.0", error.InvalidCharacter);
    try testNumberParserError("00:0:00.0", error.InvalidCharacter);
    try testNumberParserError("00:00:0.0", error.InvalidCharacter);

    try testNumberParserError("00:60:00.000", error.Overflow);
    try testNumberParserError("00:00:60.000", error.Overflow);
    try testNumberParserError("99:60:59.999", error.Overflow);
    try testNumberParserError("99:59:60.999", error.Overflow);
    try testNumberParserError("-99:60:59.999", error.Overflow);
    try testNumberParserError("-99:59:60.999", error.Overflow);
}
