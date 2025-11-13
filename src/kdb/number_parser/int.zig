const common = @import("common.zig");
const Result = common.Result;
const parseSlice = common.parseSlice;
const testParse = common.testParse;
const invalidCharacter = common.invalidCharacter;
const overflow = common.overflow;

const null_int = common.null_int;
const inf_int = common.inf_int;

pub fn parseInt(bytes: []const u8, allow_suffix: bool) Result {
    if (bytes.len == 2 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return .{ .int = null_int },
        'W', 'w' => return .{ .int = inf_int },
        else => {},
    };
    if (bytes.len == 3 and bytes[0] == '0') switch (bytes[1]) {
        'N', 'n' => return if (allow_suffix) switch (bytes[2]) {
            'i' => .{ .int = null_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        'W', 'w' => return if (allow_suffix) switch (bytes[2]) {
            'i' => .{ .int = inf_int },
            else => .{ .failure = .{ .invalid_character = 2 } },
        } else .{ .failure = .{ .invalid_character = 2 } },
        else => {},
    };

    const slice = if (allow_suffix and bytes[bytes.len - 1] == 'i') bytes[0 .. bytes.len - 1] else bytes;
    const x = switch (parseSlice(i32, slice)) {
        .overflow => return .{ .failure = .overflow },
        .invalid_character => |i| return .{ .failure = .{ .invalid_character = i } },
        .int => |int| int,
    };

    if (x == inf_int) return .{ .failure = .prefer_int_inf };
    if (x > inf_int) return .{ .failure = .overflow };

    return .{ .int = x };
}

test "parse number literal - int" {
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
    try testParse("0n", .int, false, .{ .int = null_int });
    try testParse("0N", .int, false, .{ .int = null_int });
    try testParse("0w", .int, false, .{ .int = inf_int });
    try testParse("0W", .int, false, .{ .int = inf_int });
    try testParse("1", .int, false, .{ .int = 1 });
    try testParse("2", .int, false, .{ .int = 2 });
    try testParse("2147483646", .int, false, .{ .int = 2147483646 });
    try testParse("2147483647", .int, false, .{
        .failure = .prefer_int_inf,
    });
    try testParse("2147483648", .int, false, overflow);
    try testParse("0n", .int, true, .{ .int = null_int });
    try testParse("0N", .int, true, .{ .int = null_int });
    try testParse("0w", .int, true, .{ .int = inf_int });
    try testParse("0W", .int, true, .{ .int = inf_int });
    try testParse("0", .int, true, .{ .int = 0 });
    try testParse("1", .int, true, .{ .int = 1 });
    try testParse("2", .int, true, .{ .int = 2 });
    try testParse("2147483646", .int, true, .{ .int = 2147483646 });
    try testParse("2147483647", .int, true, .{
        .failure = .prefer_int_inf,
    });
    try testParse("2147483648", .int, true, overflow);

    try testParse("0ni", .int, false, invalidCharacter(2));
    try testParse("0Ni", .int, false, invalidCharacter(2));
    try testParse("0wi", .int, false, invalidCharacter(2));
    try testParse("0Wi", .int, false, invalidCharacter(2));
    try testParse("0i", .int, false, invalidCharacter(1));
    try testParse("1i", .int, false, invalidCharacter(1));
    try testParse("2i", .int, false, invalidCharacter(1));
    try testParse("2147483646i", .int, false, invalidCharacter(10));
    try testParse("2147483647i", .int, false, invalidCharacter(10));
    try testParse("2147483648i", .int, false, overflow);
    try testParse("0ni", .int, true, .{ .int = null_int });
    try testParse("0Ni", .int, true, .{ .int = null_int });
    try testParse("0wi", .int, true, .{ .int = inf_int });
    try testParse("0Wi", .int, true, .{ .int = inf_int });
    try testParse("0i", .int, true, .{ .int = 0 });
    try testParse("1i", .int, true, .{ .int = 1 });
    try testParse("2i", .int, true, .{ .int = 2 });
    try testParse("2147483646i", .int, true, .{ .int = 2147483646 });
    try testParse("2147483647i", .int, true, .{
        .failure = .prefer_int_inf,
    });
    try testParse("2147483648i", .int, true, overflow);
}
