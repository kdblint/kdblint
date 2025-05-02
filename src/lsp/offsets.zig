const std = @import("std");
const assert = std.debug.assert;

const zls = @import("zls");
const types = zls.types;

const kdb = @import("kdb");
const Ast = kdb.Ast;
const Token = Ast.Token;

pub const Encoding = zls.offsets.Encoding;
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

pub fn tokensToLoc(tree: Ast, first_token: Ast.TokenIndex, last_token: Ast.TokenIndex) Loc {
    return .{
        .start = tokenToIndex(tree, first_token),
        .end = tokenToLoc(tree, last_token).end,
    };
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

pub fn locToSlice(text: []const u8, loc: Loc) []const u8 {
    return text[loc.start..loc.end];
}

pub fn locToRange(text: []const u8, loc: Loc, encoding: Encoding) types.Range {
    assert(loc.start <= loc.end and loc.end <= text.len);
    const start = indexToPosition(text, loc.start, encoding);
    return .{
        .start = start,
        .end = advancePosition(text, start, loc.start, loc.end, encoding),
    };
}

pub fn rangeToLoc(text: []const u8, range: types.Range, encoding: Encoding) Loc {
    assert(orderPosition(range.start, range.end) != .gt);
    return .{
        .start = positionToIndex(text, range.start, encoding),
        .end = positionToIndex(text, range.end, encoding),
    };
}

pub fn nodeToLoc(tree: Ast, node: Ast.Node.Index) Loc {
    return tokensToLoc(tree, tree.firstToken(node), tree.lastToken(node));
}

pub fn lineLocAtIndex(text: []const u8, index: usize) Loc {
    return .{
        .start = if (std.mem.lastIndexOfScalar(u8, text[0..index], '\n')) |idx|
            idx + 1
        else
            0,
        .end = std.mem.indexOfScalarPos(u8, text, index, '\n') orelse text.len,
    };
}

pub fn lineSliceAtIndex(text: []const u8, index: usize) []const u8 {
    return locToSlice(text, lineLocAtIndex(text, index));
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

pub fn convertRangeEncoding(
    text: []const u8,
    range: types.Range,
    from_encoding: Encoding,
    to_encoding: Encoding,
) types.Range {
    assert(orderPosition(range.start, range.end) != .gt);
    if (from_encoding == to_encoding) return range;
    return .{
        .start = convertPositionEncoding(
            text,
            range.start,
            from_encoding,
            to_encoding,
        ),
        .end = convertPositionEncoding(
            text,
            range.end,
            from_encoding,
            to_encoding,
        ),
    };
}

/// returns true if a is inside b
pub fn locInside(inner: Loc, outer: Loc) bool {
    assert(inner.start <= inner.end and outer.start <= outer.end);
    return outer.start <= inner.start and inner.end <= outer.end;
}

/// returns the union of a and b
pub fn locMerge(a: Loc, b: Loc) Loc {
    std.debug.assert(a.start <= a.end and b.start <= b.end);
    return .{
        .start = @min(a.start, b.start),
        .end = @max(a.end, b.end),
    };
}

pub fn orderPosition(a: types.Position, b: types.Position) std.math.Order {
    const line_order = std.math.order(a.line, b.line);
    if (line_order != .eq) return line_order;
    return std.math.order(a.character, b.character);
}

/// More efficient conversion functions that operate on multiple elements.
pub const multiple = struct {
    /// a mapping from a source index to a line character pair
    pub const IndexToPositionMapping = struct {
        output: *types.Position,
        source_index: usize,

        fn lessThan(_: void, lhs: IndexToPositionMapping, rhs: IndexToPositionMapping) bool {
            return lhs.source_index < rhs.source_index;
        }
    };

    pub fn indexToPositionWithMappings(
        text: []const u8,
        mappings: []IndexToPositionMapping,
        encoding: Encoding,
    ) void {
        std.mem.sort(IndexToPositionMapping, mappings, {}, IndexToPositionMapping.lessThan);

        var last_index: usize = 0;
        var last_position: types.Position = .{ .line = 0, .character = 0 };
        for (mappings) |mapping| {
            const index = mapping.source_index;
            const position = advancePosition(text, last_position, last_index, index, encoding);
            defer last_index = index;
            defer last_position = position;

            mapping.output.* = position;
        }
    }

    pub fn indexToPosition(
        allocator: std.mem.Allocator,
        text: []const u8,
        source_indices: []const usize,
        result_positions: []types.Position,
        encoding: Encoding,
    ) error{OutOfMemory}!void {
        std.debug.assert(source_indices.len == result_positions.len);

        // one mapping for every start and end position
        const mappings = try allocator.alloc(IndexToPositionMapping, source_indices.len);
        defer allocator.free(mappings);

        for (mappings, source_indices, result_positions) |*mapping, index, *position| {
            mapping.* = .{ .output = position, .source_index = index };
        }

        indexToPositionWithMappings(text, mappings, encoding);
    }

    test "indexToPosition" {
        const text =
            \\hello
            \\world
        ;

        const source_indices: []const usize = &.{ 3, 9, 6, 0 };
        var result_positions: [4]types.Position = undefined;
        try multiple.indexToPosition(std.testing.allocator, text, source_indices, &result_positions, .@"utf-16");

        try std.testing.expectEqualSlices(types.Position, &.{
            .{ .line = 0, .character = 3 },
            .{ .line = 1, .character = 3 },
            .{ .line = 1, .character = 0 },
            .{ .line = 0, .character = 0 },
        }, &result_positions);
    }

    pub fn locToRange(
        allocator: std.mem.Allocator,
        text: []const u8,
        locs: []const Loc,
        ranges: []types.Range,
        encoding: Encoding,
    ) error{OutOfMemory}!void {
        std.debug.assert(locs.len == ranges.len);

        // one mapping for every start and end position
        var mappings = try allocator.alloc(IndexToPositionMapping, locs.len * 2);
        defer allocator.free(mappings);

        for (locs, ranges, 0..) |loc, *range, i| {
            mappings[2 * i + 0] = .{ .output = &range.start, .source_index = loc.start };
            mappings[2 * i + 1] = .{ .output = &range.end, .source_index = loc.end };
        }

        indexToPositionWithMappings(text, mappings, encoding);
    }

    test "locToRange" {
        const text =
            \\hello
            \\world
        ;

        const locs: []const Loc = &.{
            .{ .start = 3, .end = 9 },
            .{ .start = 6, .end = 0 },
        };
        var result_ranges: [2]types.Range = undefined;
        try multiple.locToRange(std.testing.allocator, text, locs, &result_ranges, .@"utf-16");

        try std.testing.expectEqualSlices(types.Range, &.{
            .{ .start = .{ .line = 0, .character = 3 }, .end = .{ .line = 1, .character = 3 } },
            .{ .start = .{ .line = 1, .character = 0 }, .end = .{ .line = 0, .character = 0 } },
        }, &result_ranges);
    }
};

comptime {
    std.testing.refAllDecls(multiple);
}

pub fn convertPositionEncoding(
    text: []const u8,
    position: types.Position,
    from_encoding: Encoding,
    to_encoding: Encoding,
) types.Position {
    if (from_encoding == to_encoding) return position;

    const line_loc = lineLocUntilPosition(text, position, from_encoding);

    return .{
        .line = position.line,
        .character = @intCast(locLength(text, line_loc, to_encoding)),
    };
}

pub fn lineLocUntilPosition(text: []const u8, position: types.Position, encoding: Encoding) Loc {
    return lineLocUntilIndex(text, positionToIndex(text, position, encoding));
}

pub fn positionToIndex(text: []const u8, position: types.Position, encoding: Encoding) usize {
    var line: u32 = 0;
    var line_start_index: usize = 0;
    for (text, 0..) |c, i| {
        if (line == position.line) break;
        if (c == '\n') {
            line += 1;
            line_start_index = i + 1;
        }
    } else return text.len;

    const line_text = std.mem.sliceTo(text[line_start_index..], '\n');
    const line_byte_length = getNCodeUnitByteCount(line_text, position.character, encoding);

    return line_start_index + line_byte_length;
}

pub fn sourceIndexToTokenIndex(tree: Ast, source_index: usize) Ast.TokenIndex {
    assert(source_index <= tree.source.len);

    const token_locs: []Ast.Token.Loc = tree.tokens.items(.loc);

    // at which point to stop dividing and just iterate
    // good results w/ 256 as well, anything lower/higher and the cost of
    // dividing overruns the cost of iterating and vice versa
    const threshold = 336;

    var upper_index: Ast.TokenIndex = @intCast(token_locs.len - 1); // The Ast always has a .eof token
    var lower_index: Ast.TokenIndex = 0;
    while (upper_index - lower_index > threshold) {
        const mid = lower_index + (upper_index - lower_index) / 2;
        if (token_locs[mid].start < source_index) {
            lower_index = mid;
        } else {
            upper_index = mid;
        }
    }

    while (upper_index > 0) : (upper_index -= 1) {
        const token_start = token_locs[upper_index].start;
        if (token_start > source_index) continue; // checking for equality here is suboptimal
        // Handle source_index being > than the last possible token_start (max_token_start < source_index < tree.source.len)
        if (upper_index == token_locs.len - 1) break;
        // Check if source_index is within current token
        // (`token_start - 1` to include it's loc.start source_index and avoid the equality part of the check)
        const is_within_current_token = (source_index > (token_start - 1)) and (source_index < token_locs[upper_index + 1].start);
        if (!is_within_current_token) upper_index += 1; // gone 1 past
        break;
    }

    assert(upper_index < tree.tokens.len);
    return upper_index;
}

test {
    @import("std").testing.refAllDecls(@This());
}
