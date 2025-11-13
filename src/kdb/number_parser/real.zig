const parse_float = @import("parse_float.zig");

const common = @import("common.zig");
const Result = common.Result;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const trailingSpecial = common.trailingSpecial;
const periodAfterExponent = common.periodAfterExponent;
const overflow = common.overflow;

const null_real = common.null_real;
const inf_real = common.inf_real;

pub fn parseReal(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .real = null_real },
        'W', 'w' => return .{ .real = inf_real },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'e' => .{ .real = null_real },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'e' => .{ .real = inf_real },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    var i: usize = 0;
    var x: u64 = 0;
    var period = false;
    var special: u8 = 0;
    var exponent = false;
    var float = false;
    while (i < bytes.len) : (i += 1) {
        const c = bytes[i];
        switch (c) {
            'e' => if (i == bytes.len - 1) {
                if (special != 0) return .{ .failure = .{ .trailing_special = i - 1 } };
                if (allow_suffix) continue;
                return .{ .failure = .{ .invalid_character = i } };
            } else {
                float = true;
                if (exponent) {
                    return .{ .failure = .{ .duplicate_exponent = i } };
                }
                special = c;
                exponent = true;
                continue;
            },
            '.' => {
                if (exponent) {
                    const digit_index = i - ".e".len;
                    if (digit_index < bytes.len) {
                        switch (bytes[digit_index]) {
                            '0'...'9' => return .{ .failure = .{ .period_after_exponent = i } },
                            else => {},
                        }
                    }
                }
                float = true;
                if (period) return .{ .failure = .duplicate_period };
                period = true;
                continue;
            },
            '+', '-' => {
                switch (special) {
                    'e' => {},
                    else => return .{ .failure = .{ .invalid_exponent_sign = i } },
                }
                special = c;
                continue;
            },
            else => {},
        }
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'A'...'Z', 'a'...'z' => return .{ .failure = .{
                .invalid_digit = .{ .i = i, .base = .decimal },
            } },
            else => return .{ .failure = .{ .invalid_character = i } },
        };
        special = 0;

        if (float) continue;
        if (x != 0) {
            const res = @mulWithOverflow(x, 10);
            if (res[1] != 0) return .{ .failure = .overflow };
            x = res[0];
        }
        const res = @addWithOverflow(x, digit);
        if (res[1] != 0) return .{ .failure = .overflow };
        x = res[0];
    }
    if (special != 0) return .{ .failure = .{ .trailing_special = i - 1 } };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'e') bytes[0 .. bytes.len - 1] else bytes;
    const value = parse_float.parseFloat(f32, slice) catch return .{ .failure = .nyi };
    return .{ .real = value };
}

test "parse number literal - real" {
    const _std = @import("std");
    const _clock: _std.Io.Clock = .real;
    const _start = try _clock.now(_std.testing.io);
    const _file: _std.fs.File = .adaptFromNewApi(try _std.Io.Dir.cwd().createFile(_std.testing.io, @src().fn_name ++ ".log", .{}));
    defer _file.close();
    var _file_writer = _file.writer(&.{});
    const _writer = &_file_writer.interface;
    try _writer.writeAll(@src().fn_name);
    try _writer.flush();
    defer {
        _writer.print(": {d}ms\n", .{_start.durationTo(_clock.now(_std.testing.io) catch unreachable).toMilliseconds()}) catch unreachable;
        _writer.flush() catch unreachable;
    }
    try testParse("0n", .real, false, .{ .real = null_real });
    try testParse("0N", .real, false, .{ .real = null_real });
    try testParse("0w", .real, false, .{ .real = inf_real });
    try testParse("0W", .real, false, .{ .real = inf_real });
    try testParse("0", .real, false, .{ .real = 0 });
    try testParse("1", .real, false, .{ .real = 1 });
    try testParse(".2", .real, false, .{ .real = 0.2 });
    try testParse("3.", .real, false, .{ .real = 3 });
    try testParse("3.4", .real, false, .{ .real = 3.4 });
    try testParse("3.4e", .real, false, invalidCharacter(3));
    try testParse("3.4e+", .real, false, trailingSpecial(4));
    try testParse("3.4e-", .real, false, trailingSpecial(4));
    try testParse("3.4e0", .real, false, .{ .real = 3.4e0 });
    try testParse("3.4e1", .real, false, .{ .real = 3.4e1 });
    try testParse("3.4e+0", .real, false, .{ .real = 3.4e+0 });
    try testParse("3.4e-0", .real, false, .{ .real = 3.4e-0 });
    try testParse("3.4e00", .real, false, .{ .real = 3.4e00 });
    try testParse("3.4e10", .real, false, .{ .real = 3.4e10 });
    try testParse("3.4e+1", .real, false, .{ .real = 3.4e+1 });
    try testParse("3.4e-1", .real, false, .{ .real = 3.4e-1 });
    try testParse("3.4e01", .real, false, .{ .real = 3.4e01 });
    try testParse("3.4e11", .real, false, .{ .real = 3.4e11 });
    try testParse("3.4e+12", .real, false, .{ .real = 3.4e+12 });
    try testParse("3.4e-12", .real, false, .{ .real = 3.4e-12 });
    try testParse("3.4e012", .real, false, .{ .real = 3.4e012 });
    try testParse("3.4e112", .real, false, .{ .real = 3.4e112 });
    try testParse("3.4e+12.", .real, false, periodAfterExponent(7));
    try testParse("3.4e-12.", .real, false, periodAfterExponent(7));
    try testParse("3.4e012.", .real, false, periodAfterExponent(7));
    try testParse("3.4e112.", .real, false, periodAfterExponent(7));
    try testParse("3.4e+12.3", .real, false, periodAfterExponent(7));
    try testParse("3.4e-12.3", .real, false, periodAfterExponent(7));
    try testParse("3.4e012.3", .real, false, periodAfterExponent(7));
    try testParse("3.4e112.3", .real, false, periodAfterExponent(7));
    try testParse("0n", .real, true, .{ .real = null_real });
    try testParse("0N", .real, true, .{ .real = null_real });
    try testParse("0w", .real, true, .{ .real = inf_real });
    try testParse("0W", .real, true, .{ .real = inf_real });
    try testParse("0", .real, true, .{ .real = 0 });
    try testParse("1", .real, true, .{ .real = 1 });
    try testParse(".2", .real, true, .{ .real = 0.2 });
    try testParse("3.", .real, true, .{ .real = 3.0 });
    try testParse("3.4", .real, true, .{ .real = 3.4 });
    try testParse("3.4e", .real, true, .{ .real = 3.4 });
    try testParse("3.4e+", .real, true, trailingSpecial(4));
    try testParse("3.4e-", .real, true, trailingSpecial(4));
    try testParse("3.4e0", .real, true, .{ .real = 3.4e0 });
    try testParse("3.4e1", .real, true, .{ .real = 3.4e1 });
    try testParse("3.4e+0", .real, true, .{ .real = 3.4e+0 });
    try testParse("3.4e-0", .real, true, .{ .real = 3.4e-0 });
    try testParse("3.4e00", .real, true, .{ .real = 3.4e00 });
    try testParse("3.4e10", .real, true, .{ .real = 3.4e10 });
    try testParse("3.4e+1", .real, true, .{ .real = 3.4e+1 });
    try testParse("3.4e-1", .real, true, .{ .real = 3.4e-1 });
    try testParse("3.4e01", .real, true, .{ .real = 3.4e01 });
    try testParse("3.4e11", .real, true, .{ .real = 3.4e11 });
    try testParse("3.4e+12", .real, true, .{ .real = 3.4e+12 });
    try testParse("3.4e-12", .real, true, .{ .real = 3.4e-12 });
    try testParse("3.4e012", .real, true, .{ .real = 3.4e012 });
    try testParse("3.4e112", .real, true, .{ .real = 3.4e112 });
    try testParse("3.4e+12.", .real, true, periodAfterExponent(7));
    try testParse("3.4e-12.", .real, true, periodAfterExponent(7));
    try testParse("3.4e012.", .real, true, periodAfterExponent(7));
    try testParse("3.4e112.", .real, true, periodAfterExponent(7));
    try testParse("3.4e+12.3", .real, true, periodAfterExponent(7));
    try testParse("3.4e-12.3", .real, true, periodAfterExponent(7));
    try testParse("3.4e012.3", .real, true, periodAfterExponent(7));
    try testParse("3.4e112.3", .real, true, periodAfterExponent(7));

    try testParse("0ne", .real, false, invalidCharacter(2));
    try testParse("0Ne", .real, false, invalidCharacter(2));
    try testParse("0we", .real, false, invalidCharacter(2));
    try testParse("0We", .real, false, invalidCharacter(2));
    try testParse("0e", .real, false, invalidCharacter(1));
    try testParse("1e", .real, false, invalidCharacter(1));
    try testParse(".2e", .real, false, invalidCharacter(2));
    try testParse("3.e", .real, false, invalidCharacter(2));
    try testParse("3.4e", .real, false, invalidCharacter(3));
    try testParse("3.4ee", .real, false, trailingSpecial(3));
    try testParse("3.4e+e", .real, false, trailingSpecial(4));
    try testParse("3.4e-e", .real, false, trailingSpecial(4));
    try testParse("3.4e0e", .real, false, invalidCharacter(5));
    try testParse("3.4e1e", .real, false, invalidCharacter(5));
    try testParse("3.4e+0e", .real, false, invalidCharacter(6));
    try testParse("3.4e-0e", .real, false, invalidCharacter(6));
    try testParse("3.4e00e", .real, false, invalidCharacter(6));
    try testParse("3.4e10e", .real, false, invalidCharacter(6));
    try testParse("3.4e+1e", .real, false, invalidCharacter(6));
    try testParse("3.4e-1e", .real, false, invalidCharacter(6));
    try testParse("3.4e01e", .real, false, invalidCharacter(6));
    try testParse("3.4e11e", .real, false, invalidCharacter(6));
    try testParse("3.4e+12e", .real, false, invalidCharacter(7));
    try testParse("3.4e-12e", .real, false, invalidCharacter(7));
    try testParse("3.4e012e", .real, false, invalidCharacter(7));
    try testParse("3.4e112e", .real, false, invalidCharacter(7));
    try testParse("3.4e+12.e", .real, false, periodAfterExponent(7));
    try testParse("3.4e-12.e", .real, false, periodAfterExponent(7));
    try testParse("3.4e012.e", .real, false, periodAfterExponent(7));
    try testParse("3.4e112.e", .real, false, periodAfterExponent(7));
    try testParse("3.4e+12.3e", .real, false, periodAfterExponent(7));
    try testParse("3.4e-12.3e", .real, false, periodAfterExponent(7));
    try testParse("3.4e012.3e", .real, false, periodAfterExponent(7));
    try testParse("3.4e112.3e", .real, false, periodAfterExponent(7));
    try testParse("0ne", .real, true, .{ .real = null_real });
    try testParse("0Ne", .real, true, .{ .real = null_real });
    try testParse("0we", .real, true, .{ .real = inf_real });
    try testParse("0We", .real, true, .{ .real = inf_real });
    try testParse("0e", .real, true, .{ .real = 0 });
    try testParse("1e", .real, true, .{ .real = 1 });
    try testParse(".2e", .real, true, .{ .real = 0.2 });
    try testParse("3.e", .real, true, .{ .real = 3 });
    try testParse("3.4e", .real, true, .{ .real = 3.4 });
    try testParse("3.4ee", .real, true, trailingSpecial(3));
    try testParse("3.4e+e", .real, true, trailingSpecial(4));
    try testParse("3.4e-e", .real, true, trailingSpecial(4));
    try testParse("3.4e0e", .real, true, .{ .real = 3.4e0 });
    try testParse("3.4e1e", .real, true, .{ .real = 3.4e1 });
    try testParse("3.4e+0e", .real, true, .{ .real = 3.4e+0 });
    try testParse("3.4e-0e", .real, true, .{ .real = 3.4e-0 });
    try testParse("3.4e00e", .real, true, .{ .real = 3.4e00 });
    try testParse("3.4e10e", .real, true, .{ .real = 3.4e10 });
    try testParse("3.4e+1e", .real, true, .{ .real = 3.4e+1 });
    try testParse("3.4e-1e", .real, true, .{ .real = 3.4e-1 });
    try testParse("3.4e01e", .real, true, .{ .real = 3.4e01 });
    try testParse("3.4e11e", .real, true, .{ .real = 3.4e11 });
    try testParse("3.4e+12e", .real, true, .{ .real = 3.4e+12 });
    try testParse("3.4e-12e", .real, true, .{ .real = 3.4e-12 });
    try testParse("3.4e012e", .real, true, .{ .real = 3.4e012 });
    try testParse("3.4e112e", .real, true, .{ .real = 3.4e112 });
    try testParse("3.4e+12.e", .real, true, periodAfterExponent(7));
    try testParse("3.4e-12.e", .real, true, periodAfterExponent(7));
    try testParse("3.4e012.e", .real, true, periodAfterExponent(7));
    try testParse("3.4e112.e", .real, true, periodAfterExponent(7));
    try testParse("3.4e+12.3e", .real, true, periodAfterExponent(7));
    try testParse("3.4e-12.3e", .real, true, periodAfterExponent(7));
    try testParse("3.4e012.3e", .real, true, periodAfterExponent(7));
    try testParse("3.4e112.3e", .real, true, periodAfterExponent(7));
}
