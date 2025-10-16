const common = @import("common.zig");
const Result = common.Result;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const invalidHex = common.invalidHex;
const overflow = common.overflow;

pub fn parseByte(bytes: []const u8) Result {
    const base: u8 = 16;

    var i: usize = 0;
    var x: u8 = 0;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'A'...'Z' => c - 'A' + 10,
            'a'...'z' => c - 'a' + 10,
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        if (digit >= base) {
            return .{ .failure = .{
                .invalid_digit = .{ .i = i, .base = @enumFromInt(base) },
            } };
        }

        if (x != 0) {
            const res = @mulWithOverflow(x, base);
            if (res[1] != 0) return .{ .failure = .overflow };
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .{ .failure = .overflow };
        x = res[0];
    }

    return .{ .byte = x };
}

test "parse number literal - byte" {
    try testParse("0n", .byte, undefined, invalidHex(1));
    try testParse("0N", .byte, undefined, invalidHex(1));
    try testParse("0w", .byte, undefined, invalidHex(1));
    try testParse("0W", .byte, undefined, invalidHex(1));
    try testParse("0", .byte, undefined, .{ .byte = 0x0 });
    try testParse("1", .byte, undefined, .{ .byte = 0x1 });
    try testParse("2", .byte, undefined, .{ .byte = 0x2 });
    try testParse("3", .byte, undefined, .{ .byte = 0x3 });
    try testParse("4", .byte, undefined, .{ .byte = 0x4 });
    try testParse("5", .byte, undefined, .{ .byte = 0x5 });
    try testParse("6", .byte, undefined, .{ .byte = 0x6 });
    try testParse("7", .byte, undefined, .{ .byte = 0x7 });
    try testParse("8", .byte, undefined, .{ .byte = 0x8 });
    try testParse("9", .byte, undefined, .{ .byte = 0x9 });
    try testParse("a", .byte, undefined, .{ .byte = 0xa });
    try testParse("b", .byte, undefined, .{ .byte = 0xb });
    try testParse("c", .byte, undefined, .{ .byte = 0xc });
    try testParse("d", .byte, undefined, .{ .byte = 0xd });
    try testParse("e", .byte, undefined, .{ .byte = 0xe });
    try testParse("f", .byte, undefined, .{ .byte = 0xf });
    try testParse("00", .byte, undefined, .{ .byte = 0x0 });
    try testParse("01", .byte, undefined, .{ .byte = 0x1 });
    try testParse("02", .byte, undefined, .{ .byte = 0x2 });
    try testParse("03", .byte, undefined, .{ .byte = 0x3 });
    try testParse("04", .byte, undefined, .{ .byte = 0x4 });
    try testParse("05", .byte, undefined, .{ .byte = 0x5 });
    try testParse("06", .byte, undefined, .{ .byte = 0x6 });
    try testParse("07", .byte, undefined, .{ .byte = 0x7 });
    try testParse("08", .byte, undefined, .{ .byte = 0x8 });
    try testParse("09", .byte, undefined, .{ .byte = 0x9 });
    try testParse("0a", .byte, undefined, .{ .byte = 0xa });
    try testParse("0b", .byte, undefined, .{ .byte = 0xb });
    try testParse("0c", .byte, undefined, .{ .byte = 0xc });
    try testParse("0d", .byte, undefined, .{ .byte = 0xd });
    try testParse("0e", .byte, undefined, .{ .byte = 0xe });
    try testParse("0f", .byte, undefined, .{ .byte = 0xf });
    try testParse("10", .byte, undefined, .{ .byte = 0x10 });
    try testParse("11", .byte, undefined, .{ .byte = 0x11 });
    try testParse("12", .byte, undefined, .{ .byte = 0x12 });
    try testParse("13", .byte, undefined, .{ .byte = 0x13 });
    try testParse("14", .byte, undefined, .{ .byte = 0x14 });
    try testParse("15", .byte, undefined, .{ .byte = 0x15 });
    try testParse("16", .byte, undefined, .{ .byte = 0x16 });
    try testParse("17", .byte, undefined, .{ .byte = 0x17 });
    try testParse("18", .byte, undefined, .{ .byte = 0x18 });
    try testParse("19", .byte, undefined, .{ .byte = 0x19 });
    try testParse("1a", .byte, undefined, .{ .byte = 0x1a });
    try testParse("1b", .byte, undefined, .{ .byte = 0x1b });
    try testParse("1c", .byte, undefined, .{ .byte = 0x1c });
    try testParse("1d", .byte, undefined, .{ .byte = 0x1d });
    try testParse("1e", .byte, undefined, .{ .byte = 0x1e });
    try testParse("1f", .byte, undefined, .{ .byte = 0x1f });

    try testParse("g", .byte, undefined, invalidHex(0));
    try testParse("100", .byte, undefined, overflow);
}
