const parse_float = @import("parse_float.zig");

const common = @import("common.zig");
const Result = common.Result;
const testParse = common.testParse;
const trailingSpecial = common.trailingSpecial;
const periodAfterExponent = common.periodAfterExponent;
const invalidCharacter = common.invalidCharacter;

const null_float = common.null_float;
const inf_float = common.inf_float;

pub fn parseFloat(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .float = null_float },
        'W', 'w' => return .{ .float = inf_float },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'f' => .{ .float = null_float },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'f' => .{ .float = inf_float },
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
            'e' => {
                if (i + 1 < bytes.len) switch (bytes[i + 1]) {
                    '+', '-' => {},
                    '0'...'9' => {},
                    else => return .{ .failure = .{ .trailing_special = i } },
                };
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
                    'e' => if (i + 1 < bytes.len) switch (bytes[i + 1]) {
                        '0'...'9' => {},
                        else => return .{ .failure = .{ .trailing_special = i } },
                    },
                    else => return .{ .failure = .{ .invalid_exponent_sign = i } },
                }
                special = c;
                continue;
            },
            else => {},
        }
        const digit = switch (c) {
            '0'...'9' => c - '0',
            'f' => if (allow_suffix and i == bytes.len - 1)
                continue
            else
                return .{ .failure = .{ .invalid_character = i } },
            'A'...'Z', 'a'...'e', 'g'...'z' => return .{ .failure = .{
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

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'f') bytes[0 .. bytes.len - 1] else bytes;
    const value = parse_float.parseFloat(f64, slice) catch return .{ .failure = .nyi };
    return .{ .float = value };
}

test "parse number literal - float" {
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
    try testParse("0n", .float, false, .{ .float = null_float });
    try testParse("0N", .float, false, .{ .float = null_float });
    try testParse("0w", .float, false, .{ .float = inf_float });
    try testParse("0W", .float, false, .{ .float = inf_float });
    try testParse("0", .float, false, .{ .float = 0 });
    try testParse("1", .float, false, .{ .float = 1 });
    try testParse(".2", .float, false, .{ .float = 0.2 });
    try testParse("3.", .float, false, .{ .float = 3 });
    try testParse("3.4", .float, false, .{ .float = 3.4 });
    try testParse("3.4e", .float, false, trailingSpecial(3));
    try testParse("3.4e+", .float, false, trailingSpecial(4));
    try testParse("3.4e-", .float, false, trailingSpecial(4));
    try testParse("3.4e0", .float, false, .{ .float = 3.4e0 });
    try testParse("3.4e1", .float, false, .{ .float = 3.4e1 });
    try testParse("3.4e+0", .float, false, .{ .float = 3.4e+0 });
    try testParse("3.4e-0", .float, false, .{ .float = 3.4e-0 });
    try testParse("3.4e00", .float, false, .{ .float = 3.4e00 });
    try testParse("3.4e10", .float, false, .{ .float = 3.4e10 });
    try testParse("3.4e+1", .float, false, .{ .float = 3.4e+1 });
    try testParse("3.4e-1", .float, false, .{ .float = 3.4e-1 });
    try testParse("3.4e01", .float, false, .{ .float = 3.4e01 });
    try testParse("3.4e11", .float, false, .{ .float = 3.4e11 });
    try testParse("3.4e+12", .float, false, .{ .float = 3.4e+12 });
    try testParse("3.4e-12", .float, false, .{ .float = 3.4e-12 });
    try testParse("3.4e012", .float, false, .{ .float = 3.4e012 });
    try testParse("3.4e112", .float, false, .{ .float = 3.4e112 });
    try testParse("3.4e+12.", .float, false, periodAfterExponent(7));
    try testParse("3.4e-12.", .float, false, periodAfterExponent(7));
    try testParse("3.4e012.", .float, false, periodAfterExponent(7));
    try testParse("3.4e112.", .float, false, periodAfterExponent(7));
    try testParse("3.4e+12.3", .float, false, periodAfterExponent(7));
    try testParse("3.4e-12.3", .float, false, periodAfterExponent(7));
    try testParse("3.4e012.3", .float, false, periodAfterExponent(7));
    try testParse("3.4e112.3", .float, false, periodAfterExponent(7));
    try testParse("0n", .float, true, .{ .float = null_float });
    try testParse("0N", .float, true, .{ .float = null_float });
    try testParse("0w", .float, true, .{ .float = inf_float });
    try testParse("0W", .float, true, .{ .float = inf_float });
    try testParse("0", .float, true, .{ .float = 0 });
    try testParse("1", .float, true, .{ .float = 1 });
    try testParse(".2", .float, true, .{ .float = 0.2 });
    try testParse("3.", .float, true, .{ .float = 3 });
    try testParse("3.4", .float, true, .{ .float = 3.4 });
    try testParse("3.4e", .float, true, trailingSpecial(3));
    try testParse("3.4e+", .float, true, trailingSpecial(4));
    try testParse("3.4e-", .float, true, trailingSpecial(4));
    try testParse("3.4e0", .float, true, .{ .float = 3.4e0 });
    try testParse("3.4e1", .float, true, .{ .float = 3.4e1 });
    try testParse("3.4e+0", .float, true, .{ .float = 3.4e+0 });
    try testParse("3.4e-0", .float, true, .{ .float = 3.4e-0 });
    try testParse("3.4e00", .float, true, .{ .float = 3.4e00 });
    try testParse("3.4e10", .float, true, .{ .float = 3.4e10 });
    try testParse("3.4e+1", .float, true, .{ .float = 3.4e+1 });
    try testParse("3.4e-1", .float, true, .{ .float = 3.4e-1 });
    try testParse("3.4e01", .float, true, .{ .float = 3.4e01 });
    try testParse("3.4e11", .float, true, .{ .float = 3.4e11 });
    try testParse("3.4e+12", .float, true, .{ .float = 3.4e+12 });
    try testParse("3.4e-12", .float, true, .{ .float = 3.4e-12 });
    try testParse("3.4e012", .float, true, .{ .float = 3.4e012 });
    try testParse("3.4e112", .float, true, .{ .float = 3.4e112 });
    try testParse("3.4e+12.", .float, true, periodAfterExponent(7));
    try testParse("3.4e-12.", .float, true, periodAfterExponent(7));
    try testParse("3.4e012.", .float, true, periodAfterExponent(7));
    try testParse("3.4e112.", .float, true, periodAfterExponent(7));
    try testParse("3.4e+12.3", .float, true, periodAfterExponent(7));
    try testParse("3.4e-12.3", .float, true, periodAfterExponent(7));
    try testParse("3.4e012.3", .float, true, periodAfterExponent(7));
    try testParse("3.4e112.3", .float, true, periodAfterExponent(7));

    try testParse("0nf", .float, false, invalidCharacter(2));
    try testParse("0Nf", .float, false, invalidCharacter(2));
    try testParse("0wf", .float, false, invalidCharacter(2));
    try testParse("0Wf", .float, false, invalidCharacter(2));
    try testParse("0f", .float, false, invalidCharacter(1));
    try testParse("1f", .float, false, invalidCharacter(1));
    try testParse(".2f", .float, false, invalidCharacter(2));
    try testParse("3.f", .float, false, invalidCharacter(2));
    try testParse("3.4f", .float, false, invalidCharacter(3));
    try testParse("3.4ef", .float, false, trailingSpecial(3));
    try testParse("3.4e+f", .float, false, trailingSpecial(4));
    try testParse("3.4e-f", .float, false, trailingSpecial(4));
    try testParse("3.4e0f", .float, false, invalidCharacter(5));
    try testParse("3.4e1f", .float, false, invalidCharacter(5));
    try testParse("3.4e+0f", .float, false, invalidCharacter(6));
    try testParse("3.4e-0f", .float, false, invalidCharacter(6));
    try testParse("3.4e00f", .float, false, invalidCharacter(6));
    try testParse("3.4e10f", .float, false, invalidCharacter(6));
    try testParse("3.4e+1f", .float, false, invalidCharacter(6));
    try testParse("3.4e-1f", .float, false, invalidCharacter(6));
    try testParse("3.4e01f", .float, false, invalidCharacter(6));
    try testParse("3.4e11f", .float, false, invalidCharacter(6));
    try testParse("3.4e+12f", .float, false, invalidCharacter(7));
    try testParse("3.4e-12f", .float, false, invalidCharacter(7));
    try testParse("3.4e012f", .float, false, invalidCharacter(7));
    try testParse("3.4e112f", .float, false, invalidCharacter(7));
    try testParse("3.4e+12.f", .float, false, periodAfterExponent(7));
    try testParse("3.4e-12.f", .float, false, periodAfterExponent(7));
    try testParse("3.4e012.f", .float, false, periodAfterExponent(7));
    try testParse("3.4e112.f", .float, false, periodAfterExponent(7));
    try testParse("3.4e+12.3f", .float, false, periodAfterExponent(7));
    try testParse("3.4e-12.3f", .float, false, periodAfterExponent(7));
    try testParse("3.4e012.3f", .float, false, periodAfterExponent(7));
    try testParse("3.4e112.3f", .float, false, periodAfterExponent(7));
    try testParse("0nf", .float, true, .{ .float = null_float });
    try testParse("0Nf", .float, true, .{ .float = null_float });
    try testParse("0wf", .float, true, .{ .float = inf_float });
    try testParse("0Wf", .float, true, .{ .float = inf_float });
    try testParse("0f", .float, true, .{ .float = 0 });
    try testParse("1f", .float, true, .{ .float = 1 });
    try testParse(".2f", .float, true, .{ .float = 0.2 });
    try testParse("3.f", .float, true, .{ .float = 3 });
    try testParse("3.4f", .float, true, .{ .float = 3.4 });
    try testParse("3.4ef", .float, true, trailingSpecial(3));
    try testParse("3.4e+f", .float, true, trailingSpecial(4));
    try testParse("3.4e-f", .float, true, trailingSpecial(4));
    try testParse("3.4e0f", .float, true, .{ .float = 3.4e0 });
    try testParse("3.4e1f", .float, true, .{ .float = 3.4e1 });
    try testParse("3.4e+0f", .float, true, .{ .float = 3.4e+0 });
    try testParse("3.4e-0f", .float, true, .{ .float = 3.4e-0 });
    try testParse("3.4e00f", .float, true, .{ .float = 3.4e00 });
    try testParse("3.4e10f", .float, true, .{ .float = 3.4e10 });
    try testParse("3.4e+1f", .float, true, .{ .float = 3.4e+1 });
    try testParse("3.4e-1f", .float, true, .{ .float = 3.4e-1 });
    try testParse("3.4e01f", .float, true, .{ .float = 3.4e01 });
    try testParse("3.4e11f", .float, true, .{ .float = 3.4e11 });
    try testParse("3.4e+12f", .float, true, .{ .float = 3.4e+12 });
    try testParse("3.4e-12f", .float, true, .{ .float = 3.4e-12 });
    try testParse("3.4e012f", .float, true, .{ .float = 3.4e012 });
    try testParse("3.4e112f", .float, true, .{ .float = 3.4e112 });
    try testParse("3.4e+12.f", .float, true, periodAfterExponent(7));
    try testParse("3.4e-12.f", .float, true, periodAfterExponent(7));
    try testParse("3.4e012.f", .float, true, periodAfterExponent(7));
    try testParse("3.4e112.f", .float, true, periodAfterExponent(7));
    try testParse("3.4e+12.3f", .float, true, periodAfterExponent(7));
    try testParse("3.4e-12.3f", .float, true, periodAfterExponent(7));
    try testParse("3.4e012.3f", .float, true, periodAfterExponent(7));
    try testParse("3.4e112.3f", .float, true, periodAfterExponent(7));
}
