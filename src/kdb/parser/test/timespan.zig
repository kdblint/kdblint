const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_long = number_parser.null_long;
const inf_long = number_parser.inf_long;

test "valid timespan inputs" {
    try testNumberParser("0Nn", .timespan, @as(i64, null_long));
    try testNumberParser("0nn", .timespan, @as(i64, null_long));
    try testNumberParser("0Wn", .timespan, @as(i64, inf_long));
    try testNumberParser("0wn", .timespan, @as(i64, inf_long));
    try testNumberParser("-0Wn", .timespan, @as(i64, -inf_long));
    try testNumberParser("-0wn", .timespan, @as(i64, -inf_long));

    try testNumberParser("-0n", .timespan, @as(i64, 0));
    try testNumberParser("9n", .timespan, @as(i64, 32400000000000));
    try testNumberParser("-9n", .timespan, @as(i64, -32400000000000));

    try testNumberParser("00n", .timespan, @as(i64, 0));
    try testNumberParser("99n", .timespan, @as(i64, 356400000000000));
    try testNumberParser("-99n", .timespan, @as(i64, -356400000000000));

    try testNumberParser("000n", .timespan, @as(i64, 0));
    try testNumberParser("959n", .timespan, @as(i64, 35940000000000));
    try testNumberParser("-959n", .timespan, @as(i64, -35940000000000));

    try testNumberParser("0000n", .timespan, @as(i64, 0));
    try testNumberParser("9959n", .timespan, @as(i64, 359940000000000));
    try testNumberParser("-9959n", .timespan, @as(i64, -359940000000000));

    try testNumberParser("00000n", .timespan, @as(i64, 0));
    try testNumberParser("99959n", .timespan, @as(i64, 3599940000000000));
    try testNumberParser("-99959n", .timespan, @as(i64, -3599940000000000));

    try testNumberParser("000000n", .timespan, @as(i64, 0));
    try testNumberParser("995959n", .timespan, @as(i64, 359999000000000));
    try testNumberParser("-995959n", .timespan, @as(i64, -359999000000000));

    try testNumberParser("0000000n", .timespan, @as(i64, 0));
    try testNumberParser("9995959n", .timespan, @as(i64, 3599999000000000));
    try testNumberParser("-9995959n", .timespan, @as(i64, -3599999000000000));

    try testNumberParser("00000000n", .timespan, @as(i64, 0));
    try testNumberParser("99995959n", .timespan, @as(i64, 35999999000000000));
    try testNumberParser("-99995959n", .timespan, @as(i64, -35999999000000000));

    try testNumberParser("000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("999999999n", .timespan, @as(i64, 53315199000000000));
    try testNumberParser("-999999999n", .timespan, @as(i64, -53315199000000000));

    try testNumberParser("0000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("9999999999n", .timespan, @as(i64, 9053315199000000000));
    try testNumberParser("-9999999999n", .timespan, @as(i64, -9053315199000000000));

    try testNumberParser("00000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("99999999999n", .timespan, @as(i64, 6819594830452241920));
    try testNumberParser("-99999999999n", .timespan, @as(i64, -6819594830452241920));

    try testNumberParser("000000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("999999999999n", .timespan, @as(i64, 2929135218684212736));
    try testNumberParser("-999999999999n", .timespan, @as(i64, -2929135218684212736));

    try testNumberParser("0000000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("9999999999999n", .timespan, @as(i64, 918027248423024128));
    try testNumberParser("-9999999999999n", .timespan, @as(i64, -918027248423024128));

    try testNumberParser("00000000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("99999999999999n", .timespan, @as(i64, -746308380479310336));
    try testNumberParser("-99999999999999n", .timespan, @as(i64, 746308380479310336));

    try testNumberParser("000000000000000n", .timespan, @as(i64, 0));
    try testNumberParser("995959999999999n", .timespan, @as(i64, 359999999999999));
    try testNumberParser("-995959999999999n", .timespan, @as(i64, -359999999999999));

    try testNumberParser("0000000000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("9999999999999999n", .timespan, @as(i64, 644213177359414784));
    try testNumberParser("-9999999999999999n", .timespan, @as(i64, -644213177359414784));

    try testNumberParser("00000000000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("99999999999999999n", .timespan, @as(i64, -3484449091115403776));
    try testNumberParser("-99999999999999999n", .timespan, @as(i64, 3484449091115403776));

    try testNumberParser("000000000000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("999999999999999999n", .timespan, @as(i64, -7877583628444486144));
    try testNumberParser("-999999999999999999n", .timespan, @as(i64, 7877583628444486144));

    try testNumberParser("0000000000000000000n", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("9223372036854775807n", .timespan, @as(i64, -946684801000000000));
    try testNumberParser("-9223372036854775807n", .timespan, @as(i64, 946684801000000000));

    try testNumberParser("0D", .timespan, @as(i64, 0));
    try testNumberParser("0D.", .timespan, @as(i64, 0));
    try testNumberParser("0D.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("9D", .timespan, @as(i64, 777600000000000));
    try testNumberParser("9D.", .timespan, @as(i64, 777600000000000));
    try testNumberParser("9D.9", .timespan, @as(i64, 777600900000000));
    try testNumberParser("9D.999999999", .timespan, @as(i64, 777600999999999));
    try testNumberParser("9D.9999999999", .timespan, @as(i64, 777600999999999));
    try testNumberParser("-9D", .timespan, @as(i64, -777600000000000));
    try testNumberParser("-9D.", .timespan, @as(i64, -777600000000000));
    try testNumberParser("-9D.9", .timespan, @as(i64, -777600900000000));
    try testNumberParser("-9D.999999999", .timespan, @as(i64, -777600999999999));
    try testNumberParser("-9D.9999999999", .timespan, @as(i64, -777600999999999));

    try testNumberParser("0D0", .timespan, @as(i64, 0));
    try testNumberParser("0D0.", .timespan, @as(i64, 0));
    try testNumberParser("0D0.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D0.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D0.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D9", .timespan, @as(i64, 32400000000000));
    try testNumberParser("0D9.", .timespan, @as(i64, 32400000000000));
    try testNumberParser("0D9.9", .timespan, @as(i64, 32400900000000));
    try testNumberParser("0D9.999999999", .timespan, @as(i64, 32400999999999));
    try testNumberParser("0D9.9999999999", .timespan, @as(i64, 32400999999999));
    try testNumberParser("-0D9", .timespan, @as(i64, -32400000000000));
    try testNumberParser("-0D9.", .timespan, @as(i64, -32400000000000));
    try testNumberParser("-0D9.9", .timespan, @as(i64, -32400900000000));
    try testNumberParser("-0D9.999999999", .timespan, @as(i64, -32400999999999));
    try testNumberParser("-0D9.9999999999", .timespan, @as(i64, -32400999999999));

    try testNumberParser("0D00", .timespan, @as(i64, 0));
    try testNumberParser("0D00.", .timespan, @as(i64, 0));
    try testNumberParser("0D00.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D00.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D00.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D00:", .timespan, @as(i64, 0));
    try testNumberParser("0D00:.", .timespan, @as(i64, 0));
    try testNumberParser("0D00:.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D00:.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D00:.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D99", .timespan, @as(i64, 356400000000000));
    try testNumberParser("0D99.", .timespan, @as(i64, 356400000000000));
    try testNumberParser("0D99.9", .timespan, @as(i64, 356400900000000));
    try testNumberParser("0D99.999999999", .timespan, @as(i64, 356400999999999));
    try testNumberParser("0D99.9999999999", .timespan, @as(i64, 356400999999999));
    try testNumberParser("0D99:", .timespan, @as(i64, 356400000000000));
    try testNumberParser("0D99:.", .timespan, @as(i64, 356400000000000));
    try testNumberParser("0D99:.9", .timespan, @as(i64, 356400900000000));
    try testNumberParser("0D99:.999999999", .timespan, @as(i64, 356400999999999));
    try testNumberParser("0D99:.9999999999", .timespan, @as(i64, 356400999999999));
    try testNumberParser("-0D99", .timespan, @as(i64, -356400000000000));
    try testNumberParser("-0D99.", .timespan, @as(i64, -356400000000000));
    try testNumberParser("-0D99.9", .timespan, @as(i64, -356400900000000));
    try testNumberParser("-0D99.999999999", .timespan, @as(i64, -356400999999999));
    try testNumberParser("-0D99.9999999999", .timespan, @as(i64, -356400999999999));
    try testNumberParser("-0D99:", .timespan, @as(i64, -356400000000000));
    try testNumberParser("-0D99:.", .timespan, @as(i64, -356400000000000));
    try testNumberParser("-0D99:.9", .timespan, @as(i64, -356400900000000));
    try testNumberParser("-0D99:.999999999", .timespan, @as(i64, -356400999999999));
    try testNumberParser("-0D99:.9999999999", .timespan, @as(i64, -356400999999999));

    try testNumberParser("0D000", .timespan, @as(i64, 0));
    try testNumberParser("0D000.", .timespan, @as(i64, 0));
    try testNumberParser("0D000.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D000.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D000.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D959", .timespan, @as(i64, 35940000000000));
    try testNumberParser("0D959.", .timespan, @as(i64, 35940000000000));
    try testNumberParser("0D959.9", .timespan, @as(i64, 35940900000000));
    try testNumberParser("0D959.999999999", .timespan, @as(i64, 35940999999999));
    try testNumberParser("0D959.9999999999", .timespan, @as(i64, 35940999999999));
    try testNumberParser("-0D959", .timespan, @as(i64, -35940000000000));
    try testNumberParser("-0D959.", .timespan, @as(i64, -35940000000000));
    try testNumberParser("-0D959.9", .timespan, @as(i64, -35940900000000));
    try testNumberParser("-0D959.999999999", .timespan, @as(i64, -35940999999999));
    try testNumberParser("-0D959.9999999999", .timespan, @as(i64, -35940999999999));

    try testNumberParser("0D0000", .timespan, @as(i64, 0));
    try testNumberParser("0D0000.", .timespan, @as(i64, 0));
    try testNumberParser("0D0000.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D0000.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D0000.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D00:00", .timespan, @as(i64, 0));
    try testNumberParser("0D00:00.", .timespan, @as(i64, 0));
    try testNumberParser("0D00:00.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D00:00.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D00:00.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D9959", .timespan, @as(i64, 359940000000000));
    try testNumberParser("0D9959.", .timespan, @as(i64, 359940000000000));
    try testNumberParser("0D9959.9", .timespan, @as(i64, 359940900000000));
    try testNumberParser("0D9959.999999999", .timespan, @as(i64, 359940999999999));
    try testNumberParser("0D9959.9999999999", .timespan, @as(i64, 359940999999999));
    try testNumberParser("0D99:59", .timespan, @as(i64, 359940000000000));
    try testNumberParser("0D99:59.", .timespan, @as(i64, 359940000000000));
    try testNumberParser("0D99:59.9", .timespan, @as(i64, 359940900000000));
    try testNumberParser("0D99:59.999999999", .timespan, @as(i64, 359940999999999));
    try testNumberParser("0D99:59.9999999999", .timespan, @as(i64, 359940999999999));
    try testNumberParser("-0D9959", .timespan, @as(i64, -359940000000000));
    try testNumberParser("-0D9959.", .timespan, @as(i64, -359940000000000));
    try testNumberParser("-0D9959.9", .timespan, @as(i64, -359940900000000));
    try testNumberParser("-0D9959.999999999", .timespan, @as(i64, -359940999999999));
    try testNumberParser("-0D9959.9999999999", .timespan, @as(i64, -359940999999999));
    try testNumberParser("-0D99:59", .timespan, @as(i64, -359940000000000));
    try testNumberParser("-0D99:59.", .timespan, @as(i64, -359940000000000));
    try testNumberParser("-0D99:59.9", .timespan, @as(i64, -359940900000000));
    try testNumberParser("-0D99:59.999999999", .timespan, @as(i64, -359940999999999));
    try testNumberParser("-0D99:59.9999999999", .timespan, @as(i64, -359940999999999));

    try testNumberParser("0D00000", .timespan, @as(i64, 0));
    try testNumberParser("0D00000.", .timespan, @as(i64, 0));
    try testNumberParser("0D00000.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D00000.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D00000.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D99959", .timespan, @as(i64, 3599940000000000));
    try testNumberParser("0D99959.", .timespan, @as(i64, 3599940000000000));
    try testNumberParser("0D99959.9", .timespan, @as(i64, 3599940900000000));
    try testNumberParser("0D99959.999999999", .timespan, @as(i64, 3599940999999999));
    try testNumberParser("0D99959.9999999999", .timespan, @as(i64, 3599940999999999));
    try testNumberParser("-0D99959", .timespan, @as(i64, -3599940000000000));
    try testNumberParser("-0D99959.", .timespan, @as(i64, -3599940000000000));
    try testNumberParser("-0D99959.9", .timespan, @as(i64, -3599940900000000));
    try testNumberParser("-0D99959.999999999", .timespan, @as(i64, -3599940999999999));
    try testNumberParser("-0D99959.9999999999", .timespan, @as(i64, -3599940999999999));

    try testNumberParser("0D000000", .timespan, @as(i64, 0));
    try testNumberParser("0D000000.", .timespan, @as(i64, 0));
    try testNumberParser("0D000000.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D000000.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D000000.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D00:00:00", .timespan, @as(i64, 0));
    try testNumberParser("0D00:00:00.", .timespan, @as(i64, 0));
    try testNumberParser("0D00:00:00.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D00:00:00.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D00:00:00.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D995959", .timespan, @as(i64, 359999000000000));
    try testNumberParser("0D995959.", .timespan, @as(i64, 359999000000000));
    try testNumberParser("0D995959.9", .timespan, @as(i64, 359999900000000));
    try testNumberParser("0D995959.999999999", .timespan, @as(i64, 359999999999999));
    try testNumberParser("0D995959.9999999999", .timespan, @as(i64, 359999999999999));
    try testNumberParser("0D99:59:59", .timespan, @as(i64, 359999000000000));
    try testNumberParser("0D99:59:59.", .timespan, @as(i64, 359999000000000));
    try testNumberParser("0D99:59:59.9", .timespan, @as(i64, 359999900000000));
    try testNumberParser("0D99:59:59.999999999", .timespan, @as(i64, 359999999999999));
    try testNumberParser("0D99:59:59.9999999999", .timespan, @as(i64, 359999999999999));
    try testNumberParser("-0D995959", .timespan, @as(i64, -359999000000000));
    try testNumberParser("-0D995959.", .timespan, @as(i64, -359999000000000));
    try testNumberParser("-0D995959.9", .timespan, @as(i64, -359999900000000));
    try testNumberParser("-0D995959.999999999", .timespan, @as(i64, -359999999999999));
    try testNumberParser("-0D995959.9999999999", .timespan, @as(i64, -359999999999999));
    try testNumberParser("-0D99:59:59", .timespan, @as(i64, -359999000000000));
    try testNumberParser("-0D99:59:59.", .timespan, @as(i64, -359999000000000));
    try testNumberParser("-0D99:59:59.9", .timespan, @as(i64, -359999900000000));
    try testNumberParser("-0D99:59:59.999999999", .timespan, @as(i64, -359999999999999));
    try testNumberParser("-0D99:59:59.9999999999", .timespan, @as(i64, -359999999999999));

    try testNumberParser("0D0000000", .timespan, @as(i64, 0));
    try testNumberParser("0D0000000.", .timespan, @as(i64, 0));
    try testNumberParser("0D0000000.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D0000000.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D0000000.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D9995959", .timespan, @as(i64, 3599999000000000));
    try testNumberParser("0D9995959.", .timespan, @as(i64, 3599999000000000));
    try testNumberParser("0D9995959.9", .timespan, @as(i64, 3599999900000000));
    try testNumberParser("0D9995959.999999999", .timespan, @as(i64, 3599999999999999));
    try testNumberParser("0D9995959.9999999999", .timespan, @as(i64, 3599999999999999));
    try testNumberParser("-0D9995959", .timespan, @as(i64, -3599999000000000));
    try testNumberParser("-0D9995959.", .timespan, @as(i64, -3599999000000000));
    try testNumberParser("-0D9995959.9", .timespan, @as(i64, -3599999900000000));
    try testNumberParser("-0D9995959.999999999", .timespan, @as(i64, -3599999999999999));
    try testNumberParser("-0D9995959.9999999999", .timespan, @as(i64, -3599999999999999));

    try testNumberParser("0D00000000", .timespan, @as(i64, 0));
    try testNumberParser("0D00000000.", .timespan, @as(i64, 0));
    try testNumberParser("0D00000000.9", .timespan, @as(i64, 900000000));
    try testNumberParser("0D00000000.999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D00000000.9999999999", .timespan, @as(i64, 999999999));
    try testNumberParser("0D99995959", .timespan, @as(i64, 35999999000000000));
    try testNumberParser("0D99995959.", .timespan, @as(i64, 35999999000000000));
    try testNumberParser("0D99995959.9", .timespan, @as(i64, 35999999900000000));
    try testNumberParser("0D99995959.999999999", .timespan, @as(i64, 35999999999999999));
    try testNumberParser("0D99995959.9999999999", .timespan, @as(i64, 35999999999999999));
    try testNumberParser("-0D99995959", .timespan, @as(i64, -35999999000000000));
    try testNumberParser("-0D99995959.", .timespan, @as(i64, -35999999000000000));
    try testNumberParser("-0D99995959.9", .timespan, @as(i64, -35999999900000000));
    try testNumberParser("-0D99995959.999999999", .timespan, @as(i64, -35999999999999999));
    try testNumberParser("-0D99995959.9999999999", .timespan, @as(i64, -35999999999999999));

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testNumberParser("0D000000000", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("0D000000000.", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("0D000000000.9", .timespan, @as(i64, -946684799100000000));
    try testNumberParser("0D000000000.999999999", .timespan, @as(i64, -946684799000000001));
    try testNumberParser("0D000000000.9999999999", .timespan, @as(i64, -946684799000000001));
    try testNumberParser("0D999999999", .timespan, @as(i64, 53315199000000000));
    try testNumberParser("0D999999999.", .timespan, @as(i64, 53315199000000000));
    try testNumberParser("0D999999999.9", .timespan, @as(i64, 53315199900000000));
    try testNumberParser("0D999999999.999999999", .timespan, @as(i64, 53315199999999999));
    try testNumberParser("0D999999999.9999999999", .timespan, @as(i64, 53315199999999999));
    try testNumberParser("-0D999999999", .timespan, @as(i64, -53315199000000000));
    try testNumberParser("-0D999999999.", .timespan, @as(i64, -53315199000000000));
    try testNumberParser("-0D999999999.9", .timespan, @as(i64, -53315199900000000));
    try testNumberParser("-0D999999999.999999999", .timespan, @as(i64, -53315199999999999));
    try testNumberParser("-0D999999999.9999999999", .timespan, @as(i64, -53315199999999999));

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testNumberParser("0D000000000000000", .timespan, @as(i64, 0));
    try testNumberParser("0D995959999999999", .timespan, @as(i64, 359999999999999));
    try testNumberParser("-0D995959999999999", .timespan, @as(i64, -359999999999999));

    try testNumberParser("0D0000000000000000000", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("0D0000000000000000000.", .timespan, @as(i64, -946684800000000000));
    try testNumberParser("0D0000000000000000000.9", .timespan, @as(i64, -946684799100000000));
    try testNumberParser("0D0000000000000000000.999999999", .timespan, @as(i64, -946684799000000001));
    try testNumberParser("0D0000000000000000000.9999999999", .timespan, @as(i64, -946684799000000001));
    try testNumberParser("0D9223372036854775807", .timespan, @as(i64, -946684801000000000));
    try testNumberParser("0D9223372036854775807.", .timespan, @as(i64, -946684801000000000));
    try testNumberParser("0D9223372036854775807.9", .timespan, @as(i64, -946684800100000000));
    try testNumberParser("0D9223372036854775807.999999999", .timespan, @as(i64, -946684800000000001));
    try testNumberParser("0D9223372036854775807.9999999999", .timespan, @as(i64, -946684800000000001));
    try testNumberParser("-0D9223372036854775807", .timespan, @as(i64, 946684801000000000));
    try testNumberParser("-0D9223372036854775807.", .timespan, @as(i64, 946684801000000000));
    try testNumberParser("-0D9223372036854775807.9", .timespan, @as(i64, 946684800100000000));
    try testNumberParser("-0D9223372036854775807.999999999", .timespan, @as(i64, 946684800000000001));
    try testNumberParser("-0D9223372036854775807.9999999999", .timespan, @as(i64, 946684800000000001));
}

test "invalid timespan inputs" {
    try testNumberParserError("060n", error.Overflow);
    try testNumberParserError("960n", error.Overflow);
    try testNumberParserError("-060n", error.Overflow);
    try testNumberParserError("-960n", error.Overflow);

    try testNumberParserError("0060n", error.Overflow);
    try testNumberParserError("9960n", error.Overflow);
    try testNumberParserError("-0060n", error.Overflow);
    try testNumberParserError("-9960n", error.Overflow);

    try testNumberParserError("00060n", error.Overflow);
    try testNumberParserError("99960n", error.Overflow);
    try testNumberParserError("-00060n", error.Overflow);
    try testNumberParserError("-99960n", error.Overflow);

    try testNumberParserError("000060n", error.Overflow);
    try testNumberParserError("995960n", error.Overflow);
    try testNumberParserError("006000n", error.Overflow);
    try testNumberParserError("996059n", error.Overflow);
    try testNumberParserError("-000060n", error.Overflow);
    try testNumberParserError("-995960n", error.Overflow);
    try testNumberParserError("-006000n", error.Overflow);
    try testNumberParserError("-996059n", error.Overflow);

    try testNumberParserError("0000060n", error.Overflow);
    try testNumberParserError("9995960n", error.Overflow);
    try testNumberParserError("0006000n", error.Overflow);
    try testNumberParserError("9996059n", error.Overflow);
    try testNumberParserError("-0000060n", error.Overflow);
    try testNumberParserError("-9995960n", error.Overflow);
    try testNumberParserError("-0006000n", error.Overflow);
    try testNumberParserError("-9996059n", error.Overflow);

    try testNumberParserError("00000060n", error.Overflow);
    try testNumberParserError("99995960n", error.Overflow);
    try testNumberParserError("00006000n", error.Overflow);
    try testNumberParserError("99996059n", error.Overflow);
    try testNumberParserError("-00000060n", error.Overflow);
    try testNumberParserError("-99995960n", error.Overflow);
    try testNumberParserError("-00006000n", error.Overflow);
    try testNumberParserError("-99996059n", error.Overflow);

    try testNumberParserError("000060000000000n", error.Overflow);
    try testNumberParserError("995960999999999n", error.Overflow);
    try testNumberParserError("006000000000000n", error.Overflow);
    try testNumberParserError("996059999999999n", error.Overflow);
    try testNumberParserError("-000060000000000n", error.Overflow);
    try testNumberParserError("-995960999999999n", error.Overflow);
    try testNumberParserError("-006000000000000n", error.Overflow);
    try testNumberParserError("-996059999999999n", error.Overflow);

    try testNumberParserError("9223372036854775808n", error.Overflow);
    try testNumberParserError("-9223372036854775808n", error.Overflow);

    try testNumberParserError("0D0:", error.InvalidCharacter);
    try testNumberParserError("0D00:0", error.InvalidCharacter);
    try testNumberParserError("0D00:00:", error.InvalidCharacter);
    try testNumberParserError("0D00:00:0", error.InvalidCharacter);
    try testNumberParserError("-0D0:", error.InvalidCharacter);
    try testNumberParserError("-0D00:0", error.InvalidCharacter);
    try testNumberParserError("-0D00:00:", error.InvalidCharacter);
    try testNumberParserError("-0D00:00:0", error.InvalidCharacter);

    try testNumberParserError("0D060", error.Overflow);
    try testNumberParserError("0D060.", error.Overflow);
    try testNumberParserError("0D960", error.Overflow);
    try testNumberParserError("0D960.", error.Overflow);
    try testNumberParserError("-0D060", error.Overflow);
    try testNumberParserError("-0D060.", error.Overflow);
    try testNumberParserError("-0D960", error.Overflow);
    try testNumberParserError("-0D960.", error.Overflow);

    try testNumberParserError("0D0060", error.Overflow);
    try testNumberParserError("0D0060.", error.Overflow);
    try testNumberParserError("0D9960", error.Overflow);
    try testNumberParserError("0D9960.", error.Overflow);
    try testNumberParserError("-0D0060", error.Overflow);
    try testNumberParserError("-0D0060.", error.Overflow);
    try testNumberParserError("-0D9960", error.Overflow);
    try testNumberParserError("-0D9960.", error.Overflow);

    try testNumberParserError("0D00060", error.Overflow);
    try testNumberParserError("0D00060.", error.Overflow);
    try testNumberParserError("0D99960", error.Overflow);
    try testNumberParserError("0D99960.", error.Overflow);
    try testNumberParserError("-0D00060", error.Overflow);
    try testNumberParserError("-0D00060.", error.Overflow);
    try testNumberParserError("-0D99960", error.Overflow);
    try testNumberParserError("-0D99960.", error.Overflow);

    try testNumberParserError("0D000060", error.Overflow);
    try testNumberParserError("0D000060.", error.Overflow);
    try testNumberParserError("0D995960", error.Overflow);
    try testNumberParserError("0D995960.", error.Overflow);
    try testNumberParserError("0D006000", error.Overflow);
    try testNumberParserError("0D006000.", error.Overflow);
    try testNumberParserError("0D996059", error.Overflow);
    try testNumberParserError("0D996059.", error.Overflow);
    try testNumberParserError("-0D000060", error.Overflow);
    try testNumberParserError("-0D000060.", error.Overflow);
    try testNumberParserError("-0D995960", error.Overflow);
    try testNumberParserError("-0D995960.", error.Overflow);
    try testNumberParserError("-0D006000", error.Overflow);
    try testNumberParserError("-0D006000.", error.Overflow);
    try testNumberParserError("-0D996059", error.Overflow);
    try testNumberParserError("-0D996059.", error.Overflow);

    try testNumberParserError("0D0000060", error.Overflow);
    try testNumberParserError("0D0000060.", error.Overflow);
    try testNumberParserError("0D9995960", error.Overflow);
    try testNumberParserError("0D9995960.", error.Overflow);
    try testNumberParserError("0D0006000", error.Overflow);
    try testNumberParserError("0D0006000.", error.Overflow);
    try testNumberParserError("0D9996059", error.Overflow);
    try testNumberParserError("0D9996059.", error.Overflow);
    try testNumberParserError("-0D0000060", error.Overflow);
    try testNumberParserError("-0D0000060.", error.Overflow);
    try testNumberParserError("-0D9995960", error.Overflow);
    try testNumberParserError("-0D9995960.", error.Overflow);
    try testNumberParserError("-0D0006000", error.Overflow);
    try testNumberParserError("-0D0006000.", error.Overflow);
    try testNumberParserError("-0D9996059", error.Overflow);
    try testNumberParserError("-0D9996059.", error.Overflow);

    try testNumberParserError("0D00000060", error.Overflow);
    try testNumberParserError("0D00000060.", error.Overflow);
    try testNumberParserError("0D99995960", error.Overflow);
    try testNumberParserError("0D99995960.", error.Overflow);
    try testNumberParserError("0D00006000", error.Overflow);
    try testNumberParserError("0D00006000.", error.Overflow);
    try testNumberParserError("0D99996059", error.Overflow);
    try testNumberParserError("0D99996059.", error.Overflow);
    try testNumberParserError("-0D00000060", error.Overflow);
    try testNumberParserError("-0D00000060.", error.Overflow);
    try testNumberParserError("-0D99995960", error.Overflow);
    try testNumberParserError("-0D99995960.", error.Overflow);
    try testNumberParserError("-0D00006000", error.Overflow);
    try testNumberParserError("-0D00006000.", error.Overflow);
    try testNumberParserError("-0D99996059", error.Overflow);
    try testNumberParserError("-0D99996059.", error.Overflow);

    // This one doesn't overflow with decimal points
    try testNumberParserError("0D000060000000000", error.Overflow);
    try testNumberParserError("0D995960999999999", error.Overflow);
    try testNumberParserError("0D006000000000000", error.Overflow);
    try testNumberParserError("0D996059999999999", error.Overflow);
    try testNumberParserError("-0D000060000000000", error.Overflow);
    try testNumberParserError("-0D995960999999999", error.Overflow);
    try testNumberParserError("-0D006000000000000", error.Overflow);
    try testNumberParserError("-0D996059999999999", error.Overflow);

    try testNumberParserError("0D9223372036854775808", error.Overflow);
    try testNumberParserError("0D9223372036854775808.", error.Overflow);
    try testNumberParserError("-0D9223372036854775808", error.Overflow);
    try testNumberParserError("-0D9223372036854775808.", error.Overflow);
}
