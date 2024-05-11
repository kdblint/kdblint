pub const TokenType = enum(u32) {};

pub const TokenModifiers = packed struct(u16) {
    _: u16 = 0,
};

test {
    @import("std").testing.refAllDecls(@This());
}
