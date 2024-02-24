const std = @import("std");
const lsp = @import("lsp");
const types = lsp.types;

const kdb = @import("kdb.zig");
const Ast = kdb.Ast;
const Token = kdb.Token;

pub const Encoding = types.PositionEncodingKind;
pub const Loc = Token.Loc;

pub fn indexToPosition(text: []const u8, index: usize, encoding: Encoding) types.Position {
    const last_line_start = if (std.mem.lastIndexOfScalar(u8, text[0..index], '\n')) |line| line + 1 else 0;
    const line_count = std.mem.count(u8, text[0..last_line_start], "\n");

    return .{
        .line = @intCast(line_count),
        .character = @intCast(countCodeUnits(text[last_line_start..index], encoding)),
    };
}

pub fn maybePositionToIndex(text: []const u8, position: types.Position, encoding: Encoding) ?usize {
    var line: u32 = 0;
    var line_start_index: usize = 0;
    for (text, 0..) |c, i| {
        if (line == position.line) break;
        if (c == '\n') {
            line += 1;
            line_start_index = i + 1;
        }
    }

    if (line != position.line) return null;

    const line_text = std.mem.sliceTo(text[line_start_index..], '\n');
    const line_byte_length = getNCodeUnitByteCount(line_text, position.character, encoding);

    return line_start_index + line_byte_length;
}

pub fn tokenToIndex(tree: Ast, token_index: Ast.TokenIndex) usize {
    return tree.tokens.items(.loc)[token_index].start;
}

pub fn tokenToLoc(tree: Ast, token_index: Ast.TokenIndex) Loc {
    return tree.tokens.items(.loc)[token_index];
}

pub fn tokenToPosition(tree: Ast, token_index: Ast.TokenIndex, encoding: Encoding) types.Position {
    const start = tokenToIndex(tree, token_index);
    return indexToPosition(tree.source, start, encoding);
}

pub fn tokenToRange(tree: Ast, token_index: Ast.TokenIndex, encoding: Encoding) types.Range {
    const loc = tokenToLoc(tree, token_index);
    const start = indexToPosition(tree.source, loc.start, encoding);
    const end = indexToPosition(tree.source, loc.end, encoding);

    return .{
        .start = start,
        .end = end,
    };
}

pub fn locToRange(text: []const u8, loc: Loc, encoding: Encoding) types.Range {
    std.debug.assert(loc.start <= loc.end and loc.end <= text.len);
    const start = indexToPosition(text, loc.start, encoding);
    return .{
        .start = start,
        .end = advancePosition(text, start, loc.start, loc.end, encoding),
    };
}

pub fn locLength(text: []const u8, loc: Loc, encoding: Encoding) usize {
    return countCodeUnits(text[loc.start..loc.end], encoding);
}

pub fn lineLocUntilIndex(text: []const u8, index: usize) Loc {
    return .{
        .start = if (std.mem.lastIndexOfScalar(u8, text[0..index], '\n')) |idx| idx + 1 else 0,
        .end = index,
    };
}

// Helper functions

/// advance `position` which starts at `from_index` to `to_index` accounting for line breaks
pub fn advancePosition(text: []const u8, position: types.Position, from_index: usize, to_index: usize, encoding: Encoding) types.Position {
    var line = position.line;

    for (text[from_index..to_index]) |c| {
        if (c == '\n') {
            line += 1;
        }
    }

    const line_loc = lineLocUntilIndex(text, to_index);

    return .{
        .line = line,
        .character = @intCast(locLength(text, line_loc, encoding)),
    };
}

/// returns the number of code units in `text`
pub fn countCodeUnits(text: []const u8, encoding: Encoding) usize {
    switch (encoding) {
        .@"utf-8" => return text.len,
        .@"utf-16" => {
            var iter: std.unicode.Utf8Iterator = .{ .bytes = text, .i = 0 };

            var utf16_len: usize = 0;
            while (iter.nextCodepoint()) |codepoint| {
                if (codepoint < 0x10000) {
                    utf16_len += 1;
                } else {
                    utf16_len += 2;
                }
            }
            return utf16_len;
        },
        .@"utf-32" => return std.unicode.utf8CountCodepoints(text) catch unreachable,
    }
}

pub fn getNCodeUnitByteCount(text: []const u8, n: usize, encoding: Encoding) usize {
    switch (encoding) {
        .@"utf-8" => return @min(text.len, n),
        .@"utf-16" => {
            if (n == 0) return 0;
            var iter: std.unicode.Utf8Iterator = .{ .bytes = text, .i = 0 };

            var utf16_len: usize = 0;
            while (iter.nextCodepoint()) |codepoint| {
                if (codepoint < 0x10000) {
                    utf16_len += 1;
                } else {
                    utf16_len += 2;
                }
                if (utf16_len >= n) break;
            }
            return iter.i;
        },
        .@"utf-32" => {
            var i: usize = 0;
            var count: usize = 0;
            while (count != n) : (count += 1) {
                if (i >= text.len) break;
                i += std.unicode.utf8ByteSequenceLength(text[i]) catch unreachable;
            }
            return i;
        },
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
