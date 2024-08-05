const number_parser = @import("../number_parser.zig");
const testNumberParser = number_parser.testNumberParser;
const testNumberParserError = number_parser.testNumberParserError;
const null_int = number_parser.null_int;
const inf_int = number_parser.inf_int;

test "valid date inputs" {
    try testNumberParser("1999.12.31", .date, -1);
    try testNumberParser("2000.01.01", .date, 0);
    try testNumberParser("2000.01.02", .date, 1);
    try testNumberParser("2000.02.01", .date, 31);
    try testNumberParser("2000.03.01", .date, 60);
    try testNumberParser("2000.04.01", .date, 91);
    try testNumberParser("2000.05.01", .date, 121);
    try testNumberParser("2000.06.01", .date, 152);
    try testNumberParser("2000.07.01", .date, 182);
    try testNumberParser("2000.08.01", .date, 213);
    try testNumberParser("2000.09.01", .date, 244);
    try testNumberParser("2000.10.01", .date, 274);
    try testNumberParser("2000.11.01", .date, 305);
    try testNumberParser("2000.12.01", .date, 335);
    try testNumberParser("2000.12.31", .date, 365);
    try testNumberParser("2001.01.01", .date, 366);
    try testNumberParser("2001.03.01", .date, 425);
    try testNumberParser("2001.12.31", .date, 730);
    try testNumberParser("2002.01.01", .date, 731);
    try testNumberParser("2002.03.01", .date, 790);
    try testNumberParser("2003.01.01", .date, 1096);
    try testNumberParser("2003.03.01", .date, 1155);
    try testNumberParser("2004.01.01", .date, 1461);
    try testNumberParser("2004.03.01", .date, 1521);
    try testNumberParser("2005.01.01", .date, 1827);
    try testNumberParser("2005.03.01", .date, 1886);
    try testNumberParser("3000.12.31", .date, 365607);
    try testNumberParser("4000.12.31", .date, 730850);
    try testNumberParser("5000.12.31", .date, 1096092);
    try testNumberParser("6000.12.31", .date, 1461335);
    try testNumberParser("7000.12.31", .date, 1826577);
    try testNumberParser("8000.12.31", .date, 2191820);
    try testNumberParser("9000.12.31", .date, 2557062);
    try testNumberParser("0001.01.01", .date, -730119);
    try testNumberParser("9999.12.31", .date, 2921939);
    try testNumberParser("0Nd", .date, null_int);
    try testNumberParser("0nd", .date, null_int);
    try testNumberParser("0Wd", .date, inf_int);
    try testNumberParser("0wd", .date, inf_int);
    try testNumberParser("-0Wd", .date, -inf_int);
    try testNumberParser("-0wd", .date, -inf_int);

    try testNumberParser("000101d", .date, 0);
    try testNumberParser("000229d", .date, 59);
    try testNumberParser("491231d", .date, 18262);
    try testNumberParser("500101d", .date, -18262);
    try testNumberParser("991231d", .date, -1);

    try testNumberParser("00010101d", .date, -730119);
    try testNumberParser("00011231d", .date, -729755);
    try testNumberParser("99990101d", .date, 2921575);
    try testNumberParser("99991231d", .date, 2921939);
}

test "invalid date inputs" {
    try testNumberParserError("0000.01.01", error.Overflow);
    try testNumberParserError("0001.00.01", error.Overflow);
    try testNumberParserError("0001.01.00", error.Overflow);
    try testNumberParserError("0001.13.01", error.Overflow);

    try testNumberParserError("1900.02.29", error.Overflow);
    try testNumberParserError("2000.01.32", error.Overflow);
    try testNumberParserError("2000.02.30", error.Overflow);
    try testNumberParserError("2000.03.32", error.Overflow);
    try testNumberParserError("2000.04.31", error.Overflow);
    try testNumberParserError("2000.05.32", error.Overflow);
    try testNumberParserError("2000.06.31", error.Overflow);
    try testNumberParserError("2000.07.32", error.Overflow);
    try testNumberParserError("2000.08.32", error.Overflow);
    try testNumberParserError("2000.09.31", error.Overflow);
    try testNumberParserError("2000.10.32", error.Overflow);
    try testNumberParserError("2000.11.31", error.Overflow);
    try testNumberParserError("2000.12.32", error.Overflow);

    try testNumberParserError("2000.1.01", error.InvalidCharacter);

    try testNumberParserError("1111111111d", error.InvalidCharacter);
}
