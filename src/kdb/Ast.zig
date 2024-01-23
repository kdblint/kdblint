//! Abstract Syntax Tree for kdb+ source code.

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: TokenList.Slice,
/// The root AST ndoe is assumed to be index 0. Since there can be no
/// references to the root node, this means 0 is available to indicate null.
nodes: NodeList.Slice,
extra_data: []Node.Index,
mode: Mode = .q,

errors: []const Error,

pub const TokenIndex = u32;
pub const ByteOffset = u32;

pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

pub const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub fn deinit(tree: *Ast, gpa: Allocator) void {
    tree.tokens.deinit(gpa);
    tree.nodes.deinit(gpa);
    gpa.free(tree.extra_data);
    gpa.free(tree.errors);
    tree.* = undefined;
}

pub const RenderError = error{
    /// Ran out of memory allocating call stack frames to complete rendering, or
    /// ran out of memory allocating space in the output buffer.
    OutOfMemory,
};

pub const Mode = enum { k, q };

/// Result should be freed with tree.deinit() when there are
/// no more references to any of the tokens or nodes.
pub fn parse(gpa: Allocator, source: [:0]const u8, mode: Mode) Allocator.Error!Ast {
    var tokens = Ast.TokenList{};
    defer tokens.deinit(gpa);

    // Empirically, the zig std lib has an 8:1 ratio of source bytes to token count.
    const estimated_token_count = source.len / 8;
    try tokens.ensureTotalCapacity(gpa, estimated_token_count);

    var tokenizer = Tokenizer.init(source, mode);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(gpa, token);
        if (token.tag == .eof) break;
    }

    var parser: Parse = .{
        .source = source,
        .gpa = gpa,
        .token_tags = tokens.items(.tag),
        .token_locs = tokens.items(.loc),
        .token_eobs = tokens.items(.eob),
        .mode = mode,
    };
    defer parser.deinit();

    // Empirically, Zig source code has a 2:1 ratio of tokens to AST nodes.
    // Make sure at least 1 so we can use appendAssumeCapacity on the root node below.
    const estimated_node_count = (tokens.len + 2) / 2;
    try parser.nodes.ensureTotalCapacity(gpa, estimated_node_count);

    try parser.parseRoot();

    // TODO experiment with compacting the MultiArrayList slices here
    return Ast{
        .source = source,
        .mode = mode,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(gpa),
        .errors = try parser.errors.toOwnedSlice(gpa),
    };
}

pub fn extraData(tree: Ast, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        comptime assert(field.type == Node.Index);
        @field(result, field.name) = tree.extra_data[index + i];
    }
    return result;
}

pub const Error = struct {
    tag: Tag,
    is_note: bool = false,
    /// True if `token` points to the token before the token causing an issue.
    token_is_prev: bool = false,
    token: TokenIndex,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
    } = .{ .none = {} },

    pub const Tag = enum {
        /// `expected_tag` is populated.
        expected_token,

        expected_semi_after_arg,
        expected_expr,
        expected_block,
        expected_prefix_expr,
        expected_infix_expr,
    };
};

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Index = u32;

    comptime {
        // Goal is to keep this under one byte for efficiency.
        std.debug.assert(@sizeOf(Tag) <= 1);
    }

    pub const Tag = enum {
        /// extra_data[lhs...rhs]
        root,

        /// `(lhs)`. main_token is the `(`; rhs is the token index of the `)`.
        grouped_expression,

        /// Both lhs and rhs unused.
        number_literal,
        /// Both lhs and rhs unused.
        string_literal,
        /// Both lhs and rhs unused.
        symbol_literal,
        /// Both lhs and rhs unused.
        symbol_list_literal,
        /// Both lhs and rhs unused.
        identifier,

        /// Both lhs and rhs unused.
        colon,
        /// Both lhs and rhs unused.
        colon_colon,
        /// Both lhs and rhs unused.
        plus,
        /// Both lhs and rhs unused.
        plus_colon,
        /// Both lhs and rhs unused.
        minus,
        /// Both lhs and rhs unused.
        minus_colon,
        /// Both lhs and rhs unused.
        asterisk,
        /// Both lhs and rhs unused.
        asterisk_colon,
        /// Both lhs and rhs unused.
        percent,
        /// Both lhs and rhs unused.
        percent_colon,
        /// Both lhs and rhs unused.
        bang,
        /// Both lhs and rhs unused.
        bang_colon,
        /// Both lhs and rhs unused.
        ampersand,
        /// Both lhs and rhs unused.
        ampersand_colon,
        /// Both lhs and rhs unused.
        pipe,
        /// Both lhs and rhs unused.
        pipe_colon,
        /// Both lhs and rhs unused.
        angle_bracket_left,
        /// Both lhs and rhs unused.
        angle_bracket_left_colon,
        /// Both lhs and rhs unused.
        angle_bracket_left_equal,
        /// Both lhs and rhs unused.
        angle_bracket_left_right,
        /// Both lhs and rhs unused.
        angle_bracket_right,
        /// Both lhs and rhs unused.
        angle_bracket_right_colon,
        /// Both lhs and rhs unused.
        angle_bracket_right_equal,
        /// Both lhs and rhs unused.
        equal,
        /// Both lhs and rhs unused.
        equal_colon,
        /// Both lhs and rhs unused.
        tilde,
        /// Both lhs and rhs unused.
        tilde_colon,
        /// Both lhs and rhs unused.
        comma,
        /// Both lhs and rhs unused.
        comma_colon,
        /// Both lhs and rhs unused.
        caret,
        /// Both lhs and rhs unused.
        caret_colon,
        /// Both lhs and rhs unused.
        hash,
        /// Both lhs and rhs unused.
        hash_colon,
        /// Both lhs and rhs unused.
        underscore,
        /// Both lhs and rhs unused.
        underscore_colon,
        /// Both lhs and rhs unused.
        dollar,
        /// Both lhs and rhs unused.
        dollar_colon,
        /// Both lhs and rhs unused.
        question_mark,
        /// Both lhs and rhs unused.
        question_mark_colon,
        /// Both lhs and rhs unused.
        at,
        /// Both lhs and rhs unused.
        at_colon,
        /// Both lhs and rhs unused.
        dot,
        /// Both lhs and rhs unused.
        dot_colon,
        /// Both lhs and rhs unused.
        zero_colon,
        /// Both lhs and rhs unused.
        zero_colon_colon,
        /// Both lhs and rhs unused.
        one_colon,
        /// Both lhs and rhs unused.
        one_colon_colon,
        /// Both lhs and rhs unused.
        two_colon,

        /// `lhs : rhs`. main_token is `:`.
        assign,
        /// `lhs :: rhs`. main_token is `::`.
        global_assign,
        /// `lhs + rhs`. main_token is `+`.
        add,
        /// `lhs +: rhs`. main_token is `+:`.
        plus_assign,
        /// `lhs - rhs`. main_token is `-`.
        subtract,
        /// `lhs -: rhs`. main_token is `-:`.
        minus_assign,
        /// `lhs * rhs`. main_token is `*`.
        multiply,
        /// `lhs *: rhs`. main_token is `*:`.
        asterisk_assign,
        /// `lhs % rhs`. main_token is `%`.
        divide,
        /// `lhs %: rhs`. main_token is `%:`.
        percent_assign,
        /// `lhs ! rhs`. main_token is `!`.
        dict,
        /// `lhs !: rhs`. main_token is `!:`.
        bang_assign,
        /// `lhs & rhs`. main_token is `&`.
        lesser,
        /// `lhs &: rhs`. main_token is `&:`.
        ampersand_assign,
        /// `lhs | rhs`. main_token is `|`.
        greater,
        /// `lhs |: rhs`. main_token is `|:`.
        pipe_assign,
        /// `lhs < rhs`. main_token is `<`.
        less_than,
        /// `lhs <: rhs`. main_token is `<:`.
        angle_bracket_left_assign,
        /// `lhs <= rhs`. main_token is `<=`.
        less_than_equal,
        /// `lhs <> rhs`. main_token is `<>`.
        not_equal,
        /// `lhs > rhs`. main_token is `>`.
        greater_than,
        /// `lhs >: rhs`. main_token is `>:`.
        angle_bracket_right_assign,
        /// `lhs >= rhs`. main_token is `>=`.
        greater_than_equal,
        /// `lhs = rhs`. main_token is `=`.
        equals,
        /// `lhs =: rhs`. main_token is `=:`.
        equal_assign,
        /// `lhs ~ rhs`. main_token is `~`.
        match,
        /// `lhs ~: rhs`. main_token is `~:`.
        tilde_assign,
        /// `lhs , rhs`. main_token is `,`.
        join,
        /// `lhs ,: rhs`. main_token is `,:`.
        comma_assign,
        /// `lhs ^ rhs`. main_token is `^`.
        fill,
        /// `lhs ^: rhs`. main_token is `^:`.
        caret_assign,
        /// `lhs # rhs`. main_token is `#`.
        take,
        /// `lhs #: rhs`. main_token is `#:`.
        hash_assign,
        /// `lhs _ rhs`. main_token is `_`.
        drop,
        /// `lhs _: rhs`. main_token is `_:`.
        underscore_assign,
        /// `lhs $ rhs`. main_token is `$`.
        cast,
        /// `lhs $: rhs`. main_token is `$:`.
        dollar_assign,
        /// `lhs ? rhs`. main_token is `?`.
        find,
        /// `lhs ?: rhs`. main_token is `?:`.
        question_mark_assign,
        /// `lhs @ rhs`. main_token is `@`.
        apply,
        /// `lhs @: rhs`. main_token is `@:`.
        at_assign,
        /// `lhs . rhs`. main_token is `.`.
        apply_n,
        /// `lhs .: rhs`. main_token is `.:`.
        dot_assign,
        /// `lhs 0: rhs`. main_token is `0:`.
        file_text,
        /// `lhs 0:: rhs`. main_token is `0::`.
        zero_colon_assign,
        /// `lhs 1: rhs`. main_token is `1:`.
        file_binary,
        /// `lhs 1:: rhs`. main_token is `1::`.
        one_colon_assign,
        /// `lhs 2: rhs`. main_token is `2:`.
        dynamic_load,

        /// `lhs rhs`. main_token is unused.
        implicit_apply,

        /// `{[lhs]rhs}`. `SubRange[lhs]`. rhs or lhs can be omitted.
        /// main_token is `{`.
        lambda_one,
        /// Same as lambda_one but there is known to be a semicolon before the rbrace.
        lambda_one_semicolon,
        /// `{[lhs]}`. `SubRange[lhs]`. `SubRange[rhs]`. lhs can be omitted.
        /// main_token is `{`.
        lambda,
        /// Same as lambda but there is known to be a semicolon before the rbrace.
        lambda_semicolon,

        /// `[lhs rhs]`. rhs or lhs can be omitted.
        /// main_token is `[`.
        block_two,
        /// `[a;b;c]`. `extra_data[lhs..rhs]`.
        /// main_token is `[`.
        block,

        /// `lhs[rhs]`. rhs can be omitted.
        /// main_token is `[`.
        call_one,
        /// `lhs[a;b;c]`. `SubRange[rhs]`.
        /// main_token is `[`.
        call,

        /// `lhs`. rhs is unused. main_token is unused.
        implicit_return,
        /// `:lhs`. rhs is unused. main_token is ':'.
        @"return",
    };

    pub const Data = struct {
        lhs: Index,
        rhs: Index,
    };

    pub const SubRange = struct {
        /// Index into extra_data.
        start: Index,
        /// Index into extra_data.
        end: Index,
    };
};

fn getTokenTag(tree: Ast, i: Node.Index) Token.Tag {
    return tree.tokens.items(.tag)[tree.getMainToken(i)];
}

fn getTokenLoc(tree: Ast, i: Node.Index) Token.Loc {
    return tree.tokens.items(.loc)[tree.getMainToken(i)];
}

fn getTokenEob(tree: Ast, i: Node.Index) bool {
    return tree.tokens.items(.eob)[tree.getMainToken(i)];
}

fn getSource(tree: Ast, i: Node.Index) []const u8 {
    const loc = tree.getTokenLoc(i);
    return tree.source[loc.start..loc.end];
}

fn getTag(tree: Ast, i: Node.Index) Node.Tag {
    return tree.nodes.items(.tag)[i];
}

fn getMainToken(tree: Ast, i: Node.Index) TokenIndex {
    return tree.nodes.items(.main_token)[i];
}

fn getData(tree: Ast, i: Node.Index) Node.Data {
    return tree.nodes.items(.data)[i];
}

fn getExtraData(tree: Ast, i: usize) Node.Index {
    return tree.extra_data[i];
}

fn getLastToken(tree: Ast, i: Node.Index) TokenIndex {
    switch (tree.getTag(i)) {
        .root => unreachable,
        .grouped_expression => {
            var last_token = tree.getLastToken(tree.getData(i).lhs) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },
        .number_literal => {
            var index = i;
            while (true) {
                const data = tree.getData(index);
                if (data.rhs == 0) {
                    return tree.getMainToken(index);
                }
                index = data.rhs;
            }
        },
        .string_literal,
        .symbol_literal,
        .symbol_list_literal,
        .identifier,
        => return tree.getMainToken(i),
        .assign,
        .global_assign,
        .add,
        .plus_assign,
        .subtract,
        .minus_assign,
        .multiply,
        .asterisk_assign,
        .divide,
        .percent_assign,
        .dict,
        .bang_assign,
        .lesser,
        .ampersand_assign,
        .greater,
        .pipe_assign,
        .less_than,
        .angle_bracket_left_assign,
        .less_than_equal,
        .not_equal,
        .greater_than,
        .angle_bracket_right_assign,
        .greater_than_equal,
        .equals,
        .equal_assign,
        .match,
        .tilde_assign,
        .join,
        .comma_assign,
        .fill,
        .caret_assign,
        .take,
        .hash_assign,
        .drop,
        .underscore_assign,
        .cast,
        .dollar_assign,
        .find,
        .question_mark_assign,
        .apply,
        .at_assign,
        .apply_n,
        .dot_assign,
        .file_text,
        .zero_colon_assign,
        .file_binary,
        .one_colon_assign,
        .dynamic_load,
        => {
            const data = tree.getData(i);
            if (data.rhs > 0) {
                return tree.getLastToken(data.rhs);
            }
            return tree.getMainToken(i);
        },
        .colon,
        .colon_colon,
        .plus,
        .plus_colon,
        .minus,
        .minus_colon,
        .asterisk,
        .asterisk_colon,
        .percent,
        .percent_colon,
        .bang,
        .bang_colon,
        .ampersand,
        .ampersand_colon,
        .pipe,
        .pipe_colon,
        .angle_bracket_left,
        .angle_bracket_left_colon,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_colon,
        .angle_bracket_right_equal,
        .equal,
        .equal_colon,
        .tilde,
        .tilde_colon,
        .comma,
        .comma_colon,
        .caret,
        .caret_colon,
        .hash,
        .hash_colon,
        .underscore,
        .underscore_colon,
        .dollar,
        .dollar_colon,
        .question_mark,
        .question_mark_colon,
        .at,
        .at_colon,
        .dot,
        .dot_colon,
        .zero_colon,
        .zero_colon_colon,
        .one_colon,
        .one_colon_colon,
        .two_colon,
        => return tree.getMainToken(i),
        .implicit_apply => {
            const data = tree.getData(i);
            if (data.rhs > 0) {
                return tree.getLastToken(data.rhs);
            }
            unreachable;
        },
        .lambda_one,
        .lambda_one_semicolon,
        => {
            const data = tree.getData(i);
            var last_token = (if (data.rhs > 0) blk: {
                break :blk tree.getLastToken(data.rhs);
            } else blk: {
                break :blk tree.getMainToken(i);
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .semicolon => {},
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },
        .lambda,
        .lambda_semicolon,
        => {
            const data = tree.getData(i);
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            var last_token = tree.getLastToken(sub_range.end) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .semicolon => {},
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },
        .block_two => {
            const data = tree.getData(i);
            var last_token = (if (data.rhs > 0) blk: {
                break :blk tree.getLastToken(data.rhs);
            } else if (data.lhs > 0) blk: {
                break :blk tree.getLastToken(data.lhs);
            } else blk: {
                break :blk tree.getMainToken(i);
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .semicolon => {},
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },
        .block => {
            const data = tree.getData(i);
            const extra_data = tree.getExtraData(data.rhs - 1);
            var last_token = tree.getLastToken(extra_data) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .semicolon => {},
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },
        .call_one,
        .call,
        => {
            unreachable;
        },
        .implicit_return => return tree.getLastToken(tree.getData(i).lhs),
        .@"return" => unreachable,
    }
}

pub fn print(tree: Ast, i: Node.Index, stream: anytype, gpa: Allocator) !void {
    switch (tree.getTag(i)) {
        .root => unreachable,
        .grouped_expression,
        .implicit_return,
        => try tree.print(tree.getData(i).lhs, stream, gpa),
        .number_literal => {
            var index = i;
            while (true) {
                try stream.writeAll(tree.getSource(index));
                index = tree.getData(index).rhs;
                if (index == 0) break;
                try stream.writeAll(" ");
            }
        },
        .string_literal => try stream.writeAll(tree.getSource(i)),
        .symbol_literal,
        .symbol_list_literal,
        => try stream.print(",{s}", .{tree.getSource(i)}),
        .identifier => try stream.print("`{s}", .{tree.getSource(i)}),
        .assign,
        .global_assign,
        .add,
        .plus_assign,
        .subtract,
        .minus_assign,
        .multiply,
        .asterisk_assign,
        .divide,
        .percent_assign,
        .dict,
        .bang_assign,
        .lesser,
        .ampersand_assign,
        .greater,
        .pipe_assign,
        .less_than,
        .angle_bracket_left_assign,
        .less_than_equal,
        .not_equal,
        .greater_than,
        .angle_bracket_right_assign,
        .greater_than_equal,
        .equals,
        .equal_assign,
        .match,
        .tilde_assign,
        .join,
        .comma_assign,
        .fill,
        .caret_assign,
        .take,
        .hash_assign,
        .drop,
        .underscore_assign,
        .cast,
        .dollar_assign,
        .find,
        .question_mark_assign,
        .apply,
        .at_assign,
        .apply_n,
        .dot_assign,
        .file_text,
        .zero_colon_assign,
        .file_binary,
        .one_colon_assign,
        .dynamic_load,
        => {
            const data = tree.getData(i);
            if (data.rhs == 0) {
                try stream.writeAll("NYI");
            } else {
                const symbol = tree.getTokenTag(i).symbol();

                var rhs = std.ArrayList(u8).init(gpa);
                defer rhs.deinit();
                try tree.print(data.rhs, rhs.writer(), gpa);

                var lhs = std.ArrayList(u8).init(gpa);
                defer lhs.deinit();
                try tree.print(data.lhs, lhs.writer(), gpa);

                try stream.print("({s};{s};{s})", .{ symbol, lhs.items, rhs.items });
            }
        },
        .colon,
        .colon_colon,
        .plus,
        .plus_colon,
        .minus,
        .minus_colon,
        .asterisk,
        .asterisk_colon,
        .percent,
        .percent_colon,
        .bang,
        .bang_colon,
        .ampersand,
        .ampersand_colon,
        .pipe,
        .pipe_colon,
        .angle_bracket_left,
        .angle_bracket_left_colon,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_colon,
        .angle_bracket_right_equal,
        .equal,
        .equal_colon,
        .tilde,
        .tilde_colon,
        .comma,
        .comma_colon,
        .caret,
        .caret_colon,
        .hash,
        .hash_colon,
        .underscore,
        .underscore_colon,
        .dollar,
        .dollar_colon,
        .question_mark,
        .question_mark_colon,
        .at,
        .at_colon,
        .dot,
        .dot_colon,
        .zero_colon,
        .zero_colon_colon,
        .one_colon,
        .one_colon_colon,
        .two_colon,
        => try stream.writeAll(tree.getTokenTag(i).symbol()),
        .implicit_apply => {
            const data = tree.getData(i);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            try tree.print(data.rhs, rhs.writer(), gpa);

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
        },
        .lambda_one,
        .lambda_one_semicolon,
        .lambda,
        .lambda_semicolon,
        .block_two,
        .block,
        => {
            const start_token = tree.getMainToken(i);
            const start = tree.tokens.items(.loc)[start_token].start;

            const last_token = tree.getLastToken(i);
            const end = tree.tokens.items(.loc)[last_token].end;

            const source = tree.source[start..end];
            try stream.print("{s}", .{source});
        },
        .call_one => {
            const data = tree.getData(i);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            if (data.rhs == 0) {
                try rhs.appendSlice("::");
            } else {
                try tree.print(data.rhs, rhs.writer(), gpa);
            }

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
        },
        .call => {
            const data = tree.getData(i);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            for (sub_range.start..sub_range.end) |extra_data_i| {
                const node_i = tree.getExtraData(extra_data_i);
                if (node_i == 0) {
                    try rhs.appendSlice("::");
                } else {
                    try tree.print(node_i, rhs.writer(), gpa);
                }
                if (extra_data_i < sub_range.end - 1) {
                    try rhs.append(';');
                }
            }

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
        },
        .@"return" => {
            const data = tree.getData(i);

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("(\":\";{s})", .{lhs.items});
        },
    }
}

pub fn visit(tree: Ast, gpa: Allocator) !void {
    const data = tree.getData(0);
    for (data.lhs..data.rhs) |extra_data_i| {
        var list = std.ArrayList(u8).init(gpa);
        defer list.deinit();
        try tree.print(tree.getExtraData(extra_data_i), list.writer(), gpa);
        log.debug("{s}", .{list.items});
    }
}

const std = @import("std");
const kdb = @import("../kdb.zig");
const Token = kdb.Token;
const Tokenizer = kdb.Tokenizer;
const Ast = @This();
const Allocator = std.mem.Allocator;
const Parse = @import("Parse.zig");
const panic = std.debug.panic;
const assert = std.debug.assert;

const log = std.log.scoped(.kdbLint_Ast);

test {
    @import("std").testing.refAllDecls(@This());
}
