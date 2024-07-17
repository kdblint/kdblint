const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_long = number_parser.null_long;
const inf_long = number_parser.inf_long;

test "valid timestamp inputs" {
    try testNumberParser("0Np", .timestamp, @as(i64, null_long));
    try testNumberParser("0np", .timestamp, @as(i64, null_long));
    try testNumberParser("0Wp", .timestamp, @as(i64, inf_long));
    try testNumberParser("0wp", .timestamp, @as(i64, inf_long));
    try testNumberParser("-0Wp", .timestamp, @as(i64, -inf_long));
    try testNumberParser("-0wp", .timestamp, @as(i64, -inf_long));

    try testNumberParser("0p", .timestamp, @as(i64, 0));
    try testNumberParser("9p", .timestamp, @as(i64, 32400000000000));
    try testNumberParser("-9p", .timestamp, @as(i64, -32400000000000));

    try testNumberParser("00p", .timestamp, @as(i64, 0));
    try testNumberParser("99p", .timestamp, @as(i64, 356400000000000));
    try testNumberParser("-99p", .timestamp, @as(i64, -356400000000000));

    try testNumberParser("000p", .timestamp, @as(i64, 0));
    try testNumberParser("959p", .timestamp, @as(i64, 35940000000000));
    try testNumberParser("-959p", .timestamp, @as(i64, -35940000000000));

    try testNumberParser("0000p", .timestamp, @as(i64, 0));
    try testNumberParser("9959p", .timestamp, @as(i64, 359940000000000));
    try testNumberParser("-9959p", .timestamp, @as(i64, -359940000000000));

    try testNumberParser("00000p", .timestamp, @as(i64, 0));
    try testNumberParser("99959p", .timestamp, @as(i64, 3599940000000000));
    try testNumberParser("-99959p", .timestamp, @as(i64, -3599940000000000));

    try testNumberParser("000000p", .timestamp, @as(i64, 0));
    try testNumberParser("995959p", .timestamp, @as(i64, 359999000000000));
    try testNumberParser("-995959p", .timestamp, @as(i64, -359999000000000));

    try testNumberParser("0000000p", .timestamp, @as(i64, 0));
    try testNumberParser("9995959p", .timestamp, @as(i64, 3599999000000000));
    try testNumberParser("-9995959p", .timestamp, @as(i64, -3599999000000000));

    try testNumberParser("00000000p", .timestamp, @as(i64, 0));
    try testNumberParser("99995959p", .timestamp, @as(i64, 35999999000000000));
    try testNumberParser("-99995959p", .timestamp, @as(i64, -35999999000000000));

    try testNumberParser("000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("999999999p", .timestamp, @as(i64, 53315199000000000));
    try testNumberParser("-999999999p", .timestamp, @as(i64, -53315199000000000));

    try testNumberParser("0000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("9999999999p", .timestamp, @as(i64, 9053315199000000000));
    try testNumberParser("-9999999999p", .timestamp, @as(i64, -9053315199000000000));

    try testNumberParser("00000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("99999999999p", .timestamp, @as(i64, 6819594830452241920));
    try testNumberParser("-99999999999p", .timestamp, @as(i64, -6819594830452241920));

    try testNumberParser("000000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("999999999999p", .timestamp, @as(i64, 2929135218684212736));
    try testNumberParser("-999999999999p", .timestamp, @as(i64, -2929135218684212736));

    try testNumberParser("0000000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("9999999999999p", .timestamp, @as(i64, 918027248423024128));
    try testNumberParser("-9999999999999p", .timestamp, @as(i64, -918027248423024128));

    try testNumberParser("00000000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("99999999999999p", .timestamp, @as(i64, -746308380479310336));
    try testNumberParser("-99999999999999p", .timestamp, @as(i64, 746308380479310336));

    try testNumberParser("000000000000000p", .timestamp, @as(i64, 0));
    try testNumberParser("995959999999999p", .timestamp, @as(i64, 359999999999999));
    try testNumberParser("-995959999999999p", .timestamp, @as(i64, -359999999999999));

    try testNumberParser("0000000000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("9999999999999999p", .timestamp, @as(i64, 644213177359414784));
    try testNumberParser("-9999999999999999p", .timestamp, @as(i64, -644213177359414784));

    try testNumberParser("00000000000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("99999999999999999p", .timestamp, @as(i64, -3484449091115403776));
    try testNumberParser("-99999999999999999p", .timestamp, @as(i64, 3484449091115403776));

    try testNumberParser("000000000000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("999999999999999999p", .timestamp, @as(i64, -7877583628444486144));
    try testNumberParser("-999999999999999999p", .timestamp, @as(i64, 7877583628444486144));

    try testNumberParser("0000000000000000000p", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("9223372036854775807p", .timestamp, @as(i64, -946684801000000000));
    try testNumberParser("-9223372036854775807p", .timestamp, @as(i64, 946684801000000000));

    try testNumberParser("2000.01.01D", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("-2000.01.01D", .timestamp, @as(i64, 0));
    try testNumberParser("-2000.01.01D.", .timestamp, @as(i64, 0));
    try testNumberParser("-2000.01.01D.9", .timestamp, @as(i64, -900000000));
    try testNumberParser("-2000.01.01D.999999999", .timestamp, @as(i64, -999999999));
    try testNumberParser("-2000.01.01D.9999999999", .timestamp, @as(i64, -999999999));

    try testNumberParser("2000.01.01D0", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D0.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D0.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D0.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D0.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D9", .timestamp, @as(i64, 32400000000000));
    try testNumberParser("2000.01.01D9.", .timestamp, @as(i64, 32400000000000));
    try testNumberParser("2000.01.01D9.9", .timestamp, @as(i64, 32400900000000));
    try testNumberParser("2000.01.01D9.999999999", .timestamp, @as(i64, 32400999999999));
    try testNumberParser("2000.01.01D9.9999999999", .timestamp, @as(i64, 32400999999999));
    try testNumberParser("-2000.01.01D9", .timestamp, @as(i64, -32400000000000));
    try testNumberParser("-2000.01.01D9.", .timestamp, @as(i64, -32400000000000));
    try testNumberParser("-2000.01.01D9.9", .timestamp, @as(i64, -32400900000000));
    try testNumberParser("-2000.01.01D9.999999999", .timestamp, @as(i64, -32400999999999));
    try testNumberParser("-2000.01.01D9.9999999999", .timestamp, @as(i64, -32400999999999));

    try testNumberParser("2000.01.01D00", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D00.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D00.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D00:", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00:.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00:.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D00:.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D00:.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D99", .timestamp, @as(i64, 356400000000000));
    try testNumberParser("2000.01.01D99.", .timestamp, @as(i64, 356400000000000));
    try testNumberParser("2000.01.01D99.9", .timestamp, @as(i64, 356400900000000));
    try testNumberParser("2000.01.01D99.999999999", .timestamp, @as(i64, 356400999999999));
    try testNumberParser("2000.01.01D99.9999999999", .timestamp, @as(i64, 356400999999999));
    try testNumberParser("2000.01.01D99:", .timestamp, @as(i64, 356400000000000));
    try testNumberParser("2000.01.01D99:.", .timestamp, @as(i64, 356400000000000));
    try testNumberParser("2000.01.01D99:.9", .timestamp, @as(i64, 356400900000000));
    try testNumberParser("2000.01.01D99:.999999999", .timestamp, @as(i64, 356400999999999));
    try testNumberParser("2000.01.01D99:.9999999999", .timestamp, @as(i64, 356400999999999));
    try testNumberParser("-2000.01.01D99", .timestamp, @as(i64, -356400000000000));
    try testNumberParser("-2000.01.01D99.", .timestamp, @as(i64, -356400000000000));
    try testNumberParser("-2000.01.01D99.9", .timestamp, @as(i64, -356400900000000));
    try testNumberParser("-2000.01.01D99.999999999", .timestamp, @as(i64, -356400999999999));
    try testNumberParser("-2000.01.01D99.9999999999", .timestamp, @as(i64, -356400999999999));
    try testNumberParser("-2000.01.01D99:", .timestamp, @as(i64, -356400000000000));
    try testNumberParser("-2000.01.01D99:.", .timestamp, @as(i64, -356400000000000));
    try testNumberParser("-2000.01.01D99:.9", .timestamp, @as(i64, -356400900000000));
    try testNumberParser("-2000.01.01D99:.999999999", .timestamp, @as(i64, -356400999999999));
    try testNumberParser("-2000.01.01D99:.9999999999", .timestamp, @as(i64, -356400999999999));

    try testNumberParser("2000.01.01D000", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D000.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D000.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D000.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D000.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D959", .timestamp, @as(i64, 35940000000000));
    try testNumberParser("2000.01.01D959.", .timestamp, @as(i64, 35940000000000));
    try testNumberParser("2000.01.01D959.9", .timestamp, @as(i64, 35940900000000));
    try testNumberParser("2000.01.01D959.999999999", .timestamp, @as(i64, 35940999999999));
    try testNumberParser("2000.01.01D959.9999999999", .timestamp, @as(i64, 35940999999999));
    try testNumberParser("-2000.01.01D959", .timestamp, @as(i64, -35940000000000));
    try testNumberParser("-2000.01.01D959.", .timestamp, @as(i64, -35940000000000));
    try testNumberParser("-2000.01.01D959.9", .timestamp, @as(i64, -35940900000000));
    try testNumberParser("-2000.01.01D959.999999999", .timestamp, @as(i64, -35940999999999));
    try testNumberParser("-2000.01.01D959.9999999999", .timestamp, @as(i64, -35940999999999));

    try testNumberParser("2000.01.01D0000", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D0000.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D0000.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D0000.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D0000.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D00:00", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00:00.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00:00.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D00:00.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D00:00.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D9959", .timestamp, @as(i64, 359940000000000));
    try testNumberParser("2000.01.01D9959.", .timestamp, @as(i64, 359940000000000));
    try testNumberParser("2000.01.01D9959.9", .timestamp, @as(i64, 359940900000000));
    try testNumberParser("2000.01.01D9959.999999999", .timestamp, @as(i64, 359940999999999));
    try testNumberParser("2000.01.01D9959.9999999999", .timestamp, @as(i64, 359940999999999));
    try testNumberParser("2000.01.01D99:59", .timestamp, @as(i64, 359940000000000));
    try testNumberParser("2000.01.01D99:59.", .timestamp, @as(i64, 359940000000000));
    try testNumberParser("2000.01.01D99:59.9", .timestamp, @as(i64, 359940900000000));
    try testNumberParser("2000.01.01D99:59.999999999", .timestamp, @as(i64, 359940999999999));
    try testNumberParser("2000.01.01D99:59.9999999999", .timestamp, @as(i64, 359940999999999));
    try testNumberParser("-2000.01.01D9959", .timestamp, @as(i64, -359940000000000));
    try testNumberParser("-2000.01.01D9959.", .timestamp, @as(i64, -359940000000000));
    try testNumberParser("-2000.01.01D9959.9", .timestamp, @as(i64, -359940900000000));
    try testNumberParser("-2000.01.01D9959.999999999", .timestamp, @as(i64, -359940999999999));
    try testNumberParser("-2000.01.01D9959.9999999999", .timestamp, @as(i64, -359940999999999));
    try testNumberParser("-2000.01.01D99:59", .timestamp, @as(i64, -359940000000000));
    try testNumberParser("-2000.01.01D99:59.", .timestamp, @as(i64, -359940000000000));
    try testNumberParser("-2000.01.01D99:59.9", .timestamp, @as(i64, -359940900000000));
    try testNumberParser("-2000.01.01D99:59.999999999", .timestamp, @as(i64, -359940999999999));
    try testNumberParser("-2000.01.01D99:59.9999999999", .timestamp, @as(i64, -359940999999999));

    try testNumberParser("2000.01.01D00000", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00000.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00000.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D00000.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D00000.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D99959", .timestamp, @as(i64, 3599940000000000));
    try testNumberParser("2000.01.01D99959.", .timestamp, @as(i64, 3599940000000000));
    try testNumberParser("2000.01.01D99959.9", .timestamp, @as(i64, 3599940900000000));
    try testNumberParser("2000.01.01D99959.999999999", .timestamp, @as(i64, 3599940999999999));
    try testNumberParser("2000.01.01D99959.9999999999", .timestamp, @as(i64, 3599940999999999));
    try testNumberParser("-2000.01.01D99959", .timestamp, @as(i64, -3599940000000000));
    try testNumberParser("-2000.01.01D99959.", .timestamp, @as(i64, -3599940000000000));
    try testNumberParser("-2000.01.01D99959.9", .timestamp, @as(i64, -3599940900000000));
    try testNumberParser("-2000.01.01D99959.999999999", .timestamp, @as(i64, -3599940999999999));
    try testNumberParser("-2000.01.01D99959.9999999999", .timestamp, @as(i64, -3599940999999999));

    try testNumberParser("2000.01.01D000000", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D000000.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D000000.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D000000.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D000000.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D00:00:00", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00:00:00.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00:00:00.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D00:00:00.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D00:00:00.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D995959", .timestamp, @as(i64, 359999000000000));
    try testNumberParser("2000.01.01D995959.", .timestamp, @as(i64, 359999000000000));
    try testNumberParser("2000.01.01D995959.9", .timestamp, @as(i64, 359999900000000));
    try testNumberParser("2000.01.01D995959.999999999", .timestamp, @as(i64, 359999999999999));
    try testNumberParser("2000.01.01D995959.9999999999", .timestamp, @as(i64, 359999999999999));
    try testNumberParser("2000.01.01D99:59:59", .timestamp, @as(i64, 359999000000000));
    try testNumberParser("2000.01.01D99:59:59.", .timestamp, @as(i64, 359999000000000));
    try testNumberParser("2000.01.01D99:59:59.9", .timestamp, @as(i64, 359999900000000));
    try testNumberParser("2000.01.01D99:59:59.999999999", .timestamp, @as(i64, 359999999999999));
    try testNumberParser("2000.01.01D99:59:59.9999999999", .timestamp, @as(i64, 359999999999999));
    try testNumberParser("-2000.01.01D995959", .timestamp, @as(i64, -359999000000000));
    try testNumberParser("-2000.01.01D995959.", .timestamp, @as(i64, -359999000000000));
    try testNumberParser("-2000.01.01D995959.9", .timestamp, @as(i64, -359999900000000));
    try testNumberParser("-2000.01.01D995959.999999999", .timestamp, @as(i64, -359999999999999));
    try testNumberParser("-2000.01.01D995959.9999999999", .timestamp, @as(i64, -359999999999999));
    try testNumberParser("-2000.01.01D99:59:59", .timestamp, @as(i64, -359999000000000));
    try testNumberParser("-2000.01.01D99:59:59.", .timestamp, @as(i64, -359999000000000));
    try testNumberParser("-2000.01.01D99:59:59.9", .timestamp, @as(i64, -359999900000000));
    try testNumberParser("-2000.01.01D99:59:59.999999999", .timestamp, @as(i64, -359999999999999));
    try testNumberParser("-2000.01.01D99:59:59.9999999999", .timestamp, @as(i64, -359999999999999));

    try testNumberParser("2000.01.01D0000000", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D0000000.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D0000000.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D0000000.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D0000000.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D9995959", .timestamp, @as(i64, 3599999000000000));
    try testNumberParser("2000.01.01D9995959.", .timestamp, @as(i64, 3599999000000000));
    try testNumberParser("2000.01.01D9995959.9", .timestamp, @as(i64, 3599999900000000));
    try testNumberParser("2000.01.01D9995959.999999999", .timestamp, @as(i64, 3599999999999999));
    try testNumberParser("2000.01.01D9995959.9999999999", .timestamp, @as(i64, 3599999999999999));
    try testNumberParser("-2000.01.01D9995959", .timestamp, @as(i64, -3599999000000000));
    try testNumberParser("-2000.01.01D9995959.", .timestamp, @as(i64, -3599999000000000));
    try testNumberParser("-2000.01.01D9995959.9", .timestamp, @as(i64, -3599999900000000));
    try testNumberParser("-2000.01.01D9995959.999999999", .timestamp, @as(i64, -3599999999999999));
    try testNumberParser("-2000.01.01D9995959.9999999999", .timestamp, @as(i64, -3599999999999999));

    try testNumberParser("2000.01.01D00000000", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00000000.", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D00000000.9", .timestamp, @as(i64, 900000000));
    try testNumberParser("2000.01.01D00000000.999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D00000000.9999999999", .timestamp, @as(i64, 999999999));
    try testNumberParser("2000.01.01D99995959", .timestamp, @as(i64, 35999999000000000));
    try testNumberParser("2000.01.01D99995959.", .timestamp, @as(i64, 35999999000000000));
    try testNumberParser("2000.01.01D99995959.9", .timestamp, @as(i64, 35999999900000000));
    try testNumberParser("2000.01.01D99995959.999999999", .timestamp, @as(i64, 35999999999999999));
    try testNumberParser("2000.01.01D99995959.9999999999", .timestamp, @as(i64, 35999999999999999));
    try testNumberParser("-2000.01.01D99995959", .timestamp, @as(i64, -35999999000000000));
    try testNumberParser("-2000.01.01D99995959.", .timestamp, @as(i64, -35999999000000000));
    try testNumberParser("-2000.01.01D99995959.9", .timestamp, @as(i64, -35999999900000000));
    try testNumberParser("-2000.01.01D99995959.999999999", .timestamp, @as(i64, -35999999999999999));
    try testNumberParser("-2000.01.01D99995959.9999999999", .timestamp, @as(i64, -35999999999999999));

    // This one is weird, 0D000000000 means epoch 1970, so expression means 2000.01.01 + -10957D00:00:00.000000000 = 1970.01.01D00:00:00.000000000
    try testNumberParser("2000.01.01D000000000", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("2000.01.01D000000000.", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("2000.01.01D000000000.9", .timestamp, @as(i64, -946684799100000000));
    try testNumberParser("2000.01.01D000000000.999999999", .timestamp, @as(i64, -946684799000000001));
    try testNumberParser("2000.01.01D000000000.9999999999", .timestamp, @as(i64, -946684799000000001));
    try testNumberParser("2000.01.01D999999999", .timestamp, @as(i64, 53315199000000000));
    try testNumberParser("2000.01.01D999999999.", .timestamp, @as(i64, 53315199000000000));
    try testNumberParser("2000.01.01D999999999.9", .timestamp, @as(i64, 53315199900000000));
    try testNumberParser("2000.01.01D999999999.999999999", .timestamp, @as(i64, 53315199999999999));
    try testNumberParser("2000.01.01D999999999.9999999999", .timestamp, @as(i64, 53315199999999999));
    try testNumberParser("-2000.01.01D999999999", .timestamp, @as(i64, -53315199000000000));
    try testNumberParser("-2000.01.01D999999999.", .timestamp, @as(i64, -53315199000000000));
    try testNumberParser("-2000.01.01D999999999.9", .timestamp, @as(i64, -53315199900000000));
    try testNumberParser("-2000.01.01D999999999.999999999", .timestamp, @as(i64, -53315199999999999));
    try testNumberParser("-2000.01.01D999999999.9999999999", .timestamp, @as(i64, -53315199999999999));

    // Another weird one, adding a decimal point changes this to epoch 1970
    try testNumberParser("2000.01.01D000000000000000", .timestamp, @as(i64, 0));
    try testNumberParser("2000.01.01D995959999999999", .timestamp, @as(i64, 359999999999999));
    try testNumberParser("-2000.01.01D995959999999999", .timestamp, @as(i64, -359999999999999));

    try testNumberParser("2000.01.01D0000000000000000000", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("2000.01.01D0000000000000000000.", .timestamp, @as(i64, -946684800000000000));
    try testNumberParser("2000.01.01D0000000000000000000.9", .timestamp, @as(i64, -946684799100000000));
    try testNumberParser("2000.01.01D0000000000000000000.999999999", .timestamp, @as(i64, -946684799000000001));
    try testNumberParser("2000.01.01D0000000000000000000.9999999999", .timestamp, @as(i64, -946684799000000001));
    try testNumberParser("2000.01.01D9223372036854775807", .timestamp, @as(i64, -946684801000000000));
    try testNumberParser("2000.01.01D9223372036854775807.", .timestamp, @as(i64, -946684801000000000));
    try testNumberParser("2000.01.01D9223372036854775807.9", .timestamp, @as(i64, -946684800100000000));
    try testNumberParser("2000.01.01D9223372036854775807.999999999", .timestamp, @as(i64, -946684800000000001));
    try testNumberParser("2000.01.01D9223372036854775807.9999999999", .timestamp, @as(i64, -946684800000000001));
    try testNumberParser("-2000.01.01D9223372036854775807", .timestamp, @as(i64, 946684801000000000));
    try testNumberParser("-2000.01.01D9223372036854775807.", .timestamp, @as(i64, 946684801000000000));
    try testNumberParser("-2000.01.01D9223372036854775807.9", .timestamp, @as(i64, 946684800100000000));
    try testNumberParser("-2000.01.01D9223372036854775807.999999999", .timestamp, @as(i64, 946684800000000001));
    try testNumberParser("-2000.01.01D9223372036854775807.9999999999", .timestamp, @as(i64, 946684800000000001));
}

test "invalid timestamp inputs" {
    try testNumberParserError("060p", error.Overflow);
    try testNumberParserError("960p", error.Overflow);
    try testNumberParserError("-060p", error.Overflow);
    try testNumberParserError("-960p", error.Overflow);

    try testNumberParserError("0060p", error.Overflow);
    try testNumberParserError("9960p", error.Overflow);
    try testNumberParserError("-0060p", error.Overflow);
    try testNumberParserError("-9960p", error.Overflow);

    try testNumberParserError("00060p", error.Overflow);
    try testNumberParserError("99960p", error.Overflow);
    try testNumberParserError("-00060p", error.Overflow);
    try testNumberParserError("-99960p", error.Overflow);

    try testNumberParserError("000060p", error.Overflow);
    try testNumberParserError("995960p", error.Overflow);
    try testNumberParserError("006000p", error.Overflow);
    try testNumberParserError("996059p", error.Overflow);
    try testNumberParserError("-000060p", error.Overflow);
    try testNumberParserError("-995960p", error.Overflow);
    try testNumberParserError("-006000p", error.Overflow);
    try testNumberParserError("-996059p", error.Overflow);

    try testNumberParserError("0000060p", error.Overflow);
    try testNumberParserError("9995960p", error.Overflow);
    try testNumberParserError("0006000p", error.Overflow);
    try testNumberParserError("9996059p", error.Overflow);
    try testNumberParserError("-0000060p", error.Overflow);
    try testNumberParserError("-9995960p", error.Overflow);
    try testNumberParserError("-0006000p", error.Overflow);
    try testNumberParserError("-9996059p", error.Overflow);

    try testNumberParserError("00000060p", error.Overflow);
    try testNumberParserError("99995960p", error.Overflow);
    try testNumberParserError("00006000p", error.Overflow);
    try testNumberParserError("99996059p", error.Overflow);
    try testNumberParserError("-00000060p", error.Overflow);
    try testNumberParserError("-99995960p", error.Overflow);
    try testNumberParserError("-00006000p", error.Overflow);
    try testNumberParserError("-99996059p", error.Overflow);

    try testNumberParserError("000060000000000p", error.Overflow);
    try testNumberParserError("995960999999999p", error.Overflow);
    try testNumberParserError("006000000000000p", error.Overflow);
    try testNumberParserError("996059999999999p", error.Overflow);
    try testNumberParserError("-000060000000000p", error.Overflow);
    try testNumberParserError("-995960999999999p", error.Overflow);
    try testNumberParserError("-006000000000000p", error.Overflow);
    try testNumberParserError("-996059999999999p", error.Overflow);

    try testNumberParserError("9223372036854775808p", error.Overflow);
    try testNumberParserError("-9223372036854775808p", error.Overflow);

    try testNumberParserError("2000.01.01D0:", error.InvalidCharacter);
    try testNumberParserError("2000.01.01D00:0", error.InvalidCharacter);
    try testNumberParserError("2000.01.01D00:00:", error.InvalidCharacter);
    try testNumberParserError("2000.01.01D00:00:0", error.InvalidCharacter);
    try testNumberParserError("-2000.01.01D0:", error.InvalidCharacter);
    try testNumberParserError("-2000.01.01D00:0", error.InvalidCharacter);
    try testNumberParserError("-2000.01.01D00:00:", error.InvalidCharacter);
    try testNumberParserError("-2000.01.01D00:00:0", error.InvalidCharacter);

    try testNumberParserError("2000.01.01D060", error.Overflow);
    try testNumberParserError("2000.01.01D060.", error.Overflow);
    try testNumberParserError("2000.01.01D960", error.Overflow);
    try testNumberParserError("2000.01.01D960.", error.Overflow);
    try testNumberParserError("-2000.01.01D060", error.Overflow);
    try testNumberParserError("-2000.01.01D060.", error.Overflow);
    try testNumberParserError("-2000.01.01D960", error.Overflow);
    try testNumberParserError("-2000.01.01D960.", error.Overflow);

    try testNumberParserError("2000.01.01D0060", error.Overflow);
    try testNumberParserError("2000.01.01D0060.", error.Overflow);
    try testNumberParserError("2000.01.01D9960", error.Overflow);
    try testNumberParserError("2000.01.01D9960.", error.Overflow);
    try testNumberParserError("-2000.01.01D0060", error.Overflow);
    try testNumberParserError("-2000.01.01D0060.", error.Overflow);
    try testNumberParserError("-2000.01.01D9960", error.Overflow);
    try testNumberParserError("-2000.01.01D9960.", error.Overflow);

    try testNumberParserError("2000.01.01D00060", error.Overflow);
    try testNumberParserError("2000.01.01D00060.", error.Overflow);
    try testNumberParserError("2000.01.01D99960", error.Overflow);
    try testNumberParserError("2000.01.01D99960.", error.Overflow);
    try testNumberParserError("-2000.01.01D00060", error.Overflow);
    try testNumberParserError("-2000.01.01D00060.", error.Overflow);
    try testNumberParserError("-2000.01.01D99960", error.Overflow);
    try testNumberParserError("-2000.01.01D99960.", error.Overflow);

    try testNumberParserError("2000.01.01D000060", error.Overflow);
    try testNumberParserError("2000.01.01D000060.", error.Overflow);
    try testNumberParserError("2000.01.01D995960", error.Overflow);
    try testNumberParserError("2000.01.01D995960.", error.Overflow);
    try testNumberParserError("2000.01.01D006000", error.Overflow);
    try testNumberParserError("2000.01.01D006000.", error.Overflow);
    try testNumberParserError("2000.01.01D996059", error.Overflow);
    try testNumberParserError("2000.01.01D996059.", error.Overflow);
    try testNumberParserError("-2000.01.01D000060", error.Overflow);
    try testNumberParserError("-2000.01.01D000060.", error.Overflow);
    try testNumberParserError("-2000.01.01D995960", error.Overflow);
    try testNumberParserError("-2000.01.01D995960.", error.Overflow);
    try testNumberParserError("-2000.01.01D006000", error.Overflow);
    try testNumberParserError("-2000.01.01D006000.", error.Overflow);
    try testNumberParserError("-2000.01.01D996059", error.Overflow);
    try testNumberParserError("-2000.01.01D996059.", error.Overflow);

    try testNumberParserError("2000.01.01D0000060", error.Overflow);
    try testNumberParserError("2000.01.01D0000060.", error.Overflow);
    try testNumberParserError("2000.01.01D9995960", error.Overflow);
    try testNumberParserError("2000.01.01D9995960.", error.Overflow);
    try testNumberParserError("2000.01.01D0006000", error.Overflow);
    try testNumberParserError("2000.01.01D0006000.", error.Overflow);
    try testNumberParserError("2000.01.01D9996059", error.Overflow);
    try testNumberParserError("2000.01.01D9996059.", error.Overflow);
    try testNumberParserError("-2000.01.01D0000060", error.Overflow);
    try testNumberParserError("-2000.01.01D0000060.", error.Overflow);
    try testNumberParserError("-2000.01.01D9995960", error.Overflow);
    try testNumberParserError("-2000.01.01D9995960.", error.Overflow);
    try testNumberParserError("-2000.01.01D0006000", error.Overflow);
    try testNumberParserError("-2000.01.01D0006000.", error.Overflow);
    try testNumberParserError("-2000.01.01D9996059", error.Overflow);
    try testNumberParserError("-2000.01.01D9996059.", error.Overflow);

    try testNumberParserError("2000.01.01D00000060", error.Overflow);
    try testNumberParserError("2000.01.01D00000060.", error.Overflow);
    try testNumberParserError("2000.01.01D99995960", error.Overflow);
    try testNumberParserError("2000.01.01D99995960.", error.Overflow);
    try testNumberParserError("2000.01.01D00006000", error.Overflow);
    try testNumberParserError("2000.01.01D00006000.", error.Overflow);
    try testNumberParserError("2000.01.01D99996059", error.Overflow);
    try testNumberParserError("2000.01.01D99996059.", error.Overflow);
    try testNumberParserError("-2000.01.01D00000060", error.Overflow);
    try testNumberParserError("-2000.01.01D00000060.", error.Overflow);
    try testNumberParserError("-2000.01.01D99995960", error.Overflow);
    try testNumberParserError("-2000.01.01D99995960.", error.Overflow);
    try testNumberParserError("-2000.01.01D00006000", error.Overflow);
    try testNumberParserError("-2000.01.01D00006000.", error.Overflow);
    try testNumberParserError("-2000.01.01D99996059", error.Overflow);
    try testNumberParserError("-2000.01.01D99996059.", error.Overflow);

    // This one doesn't overflow with decimal points
    try testNumberParserError("2000.01.01D000060000000000", error.Overflow);
    try testNumberParserError("2000.01.01D995960999999999", error.Overflow);
    try testNumberParserError("2000.01.01D006000000000000", error.Overflow);
    try testNumberParserError("2000.01.01D996059999999999", error.Overflow);
    try testNumberParserError("-2000.01.01D000060000000000", error.Overflow);
    try testNumberParserError("-2000.01.01D995960999999999", error.Overflow);
    try testNumberParserError("-2000.01.01D006000000000000", error.Overflow);
    try testNumberParserError("-2000.01.01D996059999999999", error.Overflow);

    try testNumberParserError("2000.01.01D9223372036854775808", error.Overflow);
    try testNumberParserError("2000.01.01D9223372036854775808.", error.Overflow);
    try testNumberParserError("-2000.01.01D9223372036854775808", error.Overflow);
    try testNumberParserError("-2000.01.01D9223372036854775808.", error.Overflow);
}
