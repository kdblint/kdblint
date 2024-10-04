const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const kdb = @import("root.zig");
pub const Token = kdb.Token;
const Tokenizer = kdb.Tokenizer;
const Parse = kdb.Parse;

const Ast = @This();

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: std.MultiArrayList(Token).Slice,
/// The root AST node is assumed to be index 0. Since there can be no
/// references to the root node, this means 0 is available to indicate null.
nodes: std.MultiArrayList(Node).Slice,
extra_data: []Node.Index,
mode: Mode = .q,

errors: []const Error,

pub const ByteOffset = u32;

pub const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub const Span = struct {
    start: u32,
    end: u32,
    main: u32,
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

pub const Mode = enum { q, k };

/// Result should be freed with tree.deinit() when there are
/// no more references to any of the tokens or nodes.
pub fn parse(gpa: Allocator, source: [:0]const u8, mode: Mode) Allocator.Error!Ast {
    var tokens: std.MultiArrayList(Token) = .{};
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
        .gpa = gpa,
        .mode = mode,
        .source = source,
        .tokens = tokens,
    };
    defer parser.errors.deinit(gpa);
    defer parser.nodes.deinit(gpa);
    defer parser.extra_data.deinit(gpa);
    defer parser.scratch.deinit(gpa);

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

/// `gpa` is used for allocating the resulting formatted source code.
/// Caller owns the returned slice of bytes, allocated with `gpa`.
pub fn render(tree: Ast, gpa: Allocator) RenderError![]u8 {
    var buffer = std.ArrayList(u8).init(gpa);
    defer buffer.deinit();

    try tree.renderToArrayList(&buffer, .{});
    return buffer.toOwnedSlice();
}

pub const Fixups = kdb.render.Fixups;

pub fn renderToArrayList(tree: Ast, buffer: *std.ArrayList(u8), fixups: Fixups) RenderError!void {
    return kdb.render.renderTree(buffer, tree, fixups);
}

/// Returns an extra offset for column and byte offset of errors that
/// should point after the token in the error message.
pub fn errorOffset(tree: Ast, parse_error: Error) u32 {
    return if (parse_error.token_is_prev)
        @as(u32, @intCast(tree.tokenSlice(parse_error.token).len))
    else
        0;
}

pub fn tokenSlice(tree: Ast, token_index: Token.Index) []const u8 {
    const token_locs: []Token.Loc = tree.tokens.items(.loc);
    const token_tags: []Token.Tag = tree.tokens.items(.tag);
    const token_tag = token_tags[token_index];

    // Many tokens can be determined entirely by their tag.
    if (token_tag.lexeme()) |lexeme| {
        return lexeme;
    }

    const token_loc = token_locs[token_index];
    return tree.source[token_loc.start..token_loc.end];
}

pub fn tokenLen(tree: Ast, token_index: Token.Index) usize {
    const token_locs: []Token.Loc = tree.tokens.items(.loc);

    const token_loc = token_locs[token_index];
    return token_loc.end - token_loc.start;
}

pub fn extraData(tree: Ast, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        switch (@typeInfo(field.type)) {
            .int => comptime assert(field.type == Node.Index),
            .@"struct" => |ti| comptime assert(ti.layout == .@"packed" and ti.backing_integer.? == Node.Index),
            inline else => |tag| @compileError("Expected Node.Index or packed struct, found '" ++ @tagName(tag) ++ "'"),
        }
        @field(result, field.name) = @bitCast(tree.extra_data[index + i]);
    }
    return result;
}

pub fn rootDecls(tree: Ast) []const Node.Index {
    // Root is always index 0.
    const nodes_data = tree.nodes.items(.data);
    return tree.extra_data[nodes_data[0].lhs..nodes_data[0].rhs];
}

pub fn firstToken(tree: Ast, node: Node.Index) Token.Index {
    const tags: []Node.Tag = tree.nodes.items(.tag);
    const datas: []Node.Data = tree.nodes.items(.data);
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    const end_offset: Token.Index = 0;
    var n = node;
    while (true) switch (tags[n]) {
        .root,
        => return 0,

        .empty,
        .null,
        => return main_tokens[n] - end_offset,

        .grouped_expression,
        .empty_list,
        .list,
        => return main_tokens[n] - end_offset,

        .table_literal,
        => return main_tokens[n] - end_offset,

        .lambda,
        .lambda_semicolon,
        => return main_tokens[n] - end_offset,

        .expr_block,
        => return main_tokens[n] - end_offset,

        .assign,
        .global_assign,
        => n = datas[n].lhs,

        .plus,
        .minus,
        .asterisk,
        .percent,
        .ampersand,
        .pipe,
        .caret,
        .equal,
        .angle_bracket_left,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_equal,
        .dollar,
        .comma,
        .hash,
        .underscore,
        .tilde,
        .bang,
        .question_mark,
        .at,
        .period,
        .zero_colon,
        .one_colon,
        .two_colon,
        => return main_tokens[n] - end_offset,

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => {
            if (datas[n].lhs == 0) {
                return main_tokens[n] - end_offset;
            } else {
                n = datas[n].lhs;
            }
        },

        .apply_unary,
        .apply_binary,
        => n = datas[n].lhs,

        .number_literal,
        .number_list_literal,
        .string_literal,
        .symbol_literal,
        .symbol_list_literal,
        .identifier,
        => return main_tokens[n] + end_offset,

        .select,
        => return main_tokens[n] + end_offset,
    };
}

pub fn lastToken(tree: Ast, node: Node.Index) Token.Index {
    const tags: []Node.Tag = tree.nodes.items(.tag);
    const datas: []Node.Data = tree.nodes.items(.data);
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    var end_offset: Token.Index = 0;
    _ = &end_offset;
    var n = node;
    while (true) switch (tags[n]) {
        .root,
        => return @intCast(tree.tokens.len - 1),

        .empty,
        .null,
        => return main_tokens[n] + end_offset,

        .grouped_expression,
        .empty_list,
        .list,
        => return datas[n].rhs + end_offset,

        .table_literal,
        => return datas[n].rhs + end_offset,

        .lambda,
        .lambda_semicolon,
        => return datas[n].rhs + end_offset,

        .expr_block,
        => return datas[n].rhs + end_offset,

        .assign,
        .global_assign,
        => n = datas[n].rhs,

        .plus,
        .minus,
        .asterisk,
        .percent,
        .ampersand,
        .pipe,
        .caret,
        .equal,
        .angle_bracket_left,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_equal,
        .dollar,
        .comma,
        .hash,
        .underscore,
        .tilde,
        .bang,
        .question_mark,
        .at,
        .period,
        .zero_colon,
        .one_colon,
        .two_colon,
        => return main_tokens[n] + end_offset,

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => return main_tokens[n] + end_offset,

        .apply_unary,
        => n = datas[n].rhs,

        .apply_binary,
        => n = if (datas[n].rhs > 0) datas[n].rhs else main_tokens[n],

        .number_literal,
        .string_literal,
        .symbol_literal,
        .identifier,
        => return main_tokens[n] + end_offset,

        .number_list_literal,
        .symbol_list_literal,
        => return datas[n].lhs + end_offset,

        .select,
        => {
            const select = tree.extraData(datas[n].lhs, Ast.Node.Select);
            _ = select; // autofix
            unreachable;
        },
    };
}

pub fn tokensOnSameLine(tree: Ast, token1: Token.Index, token2: Token.Index) bool {
    const token_locs = tree.tokens.items(.loc);
    const source = tree.source[token_locs[token1].start..token_locs[token2].start];
    return mem.indexOfScalar(u8, source, '\n') == null;
}

pub fn renderError(tree: Ast, parse_error: Error, writer: anytype) !void {
    const token_tags: []Token.Tag = tree.tokens.items(.tag);
    switch (parse_error.tag) {
        .expected_expr => try writer.print("expected expression, found '{s}'", .{
            token_tags[parse_error.token].symbol(),
        }),
        .expected_prefix_expr => try writer.writeAll("expected prefix expr"),
        .cannot_project_operator_without_lhs => try writer.writeAll("cannot project operator without lhs"),
        .cannot_apply_operator_directly => try writer.writeAll("cannot apply operator directly"),
        .cannot_apply_iterator_directly => try writer.writeAll("cannot apply iterator directly"),
        .expected_whitespace => try writer.writeAll("expected whitespace"),

        .expected_qsql_token => {
            const found_tag = token_tags[parse_error.token];
            const expected_string = parse_error.extra.expected_string;
            switch (found_tag) {
                .invalid => return writer.print("expected '{s}', found invalid bytes", .{expected_string}),
                else => return writer.print("expected '{s}', found '{s}'", .{ expected_string, found_tag.symbol() }),
            }
        },

        .expected_token => {
            const found_tag = token_tags[parse_error.token];
            const expected_symbol = parse_error.extra.expected_tag.symbol();
            switch (found_tag) {
                .invalid => return writer.print("expected '{s}', found invalid bytes", .{expected_symbol}),
                else => return writer.print("expected '{s}', found '{s}'", .{ expected_symbol, found_tag.symbol() }),
            }
        },
    }
}

pub const Error = struct {
    tag: Tag,
    is_note: bool = false,
    /// True if `token` points to the token before the token causing an issue.
    token_is_prev: bool = false,
    token: Token.Index,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
        expected_string: []const u8,
    } = .{ .none = {} },

    pub const Tag = enum {
        expected_expr,
        expected_prefix_expr,
        cannot_project_operator_without_lhs,
        cannot_apply_operator_directly,
        cannot_apply_iterator_directly,
        expected_whitespace,

        /// `expected_string` is populated.
        expected_qsql_token,

        /// `expected_tag` is populated.
        expected_token,
    };
};

pub const Node = struct {
    tag: Tag,
    main_token: Token.Index,
    data: Data,

    // pub const Index = enum(u32) { _ };
    pub const Index = u32;

    pub const Type = enum {
        other,
        unary_operator,
        binary_operator,
        iterator,
    };

    comptime {
        // Goal is to keep this under one byte for efficiency.
        assert(@sizeOf(Tag) <= 1);
    }

    pub const Tag = enum {
        /// extra_data[lhs...rhs]
        root,
        /// main_token is the next token. Both lhs and rhs unused.
        empty,
        /// main_token is the `::`. Both lhs and rhs unused.
        null,

        /// `(lhs)`. main_token is the `(`. rhs is the token index of the `)`.
        grouped_expression,
        /// `()`. lhs unused. main_token is the `(`. rhs is the token index of the `)`.
        empty_list,
        /// `(lhs)`. main_token is the `(`. rhs is the token index of the `)`. `SubRange[lhs]`.
        list,
        /// `([]lhs)`. main_token is the `(`. rhs is the token index of the `)`. `Table[lhs]`.
        table_literal,

        /// `{lhs}`. main_token is the `{`. rhs is the token index of the `}`. `Lambda[lhs]`.
        lambda,
        /// Same as lambda but there is known to be a semicolon before the `}`.
        lambda_semicolon,

        /// `[lhs]`. main_token is the `[`. lhs can be omitted. rhs is the token index of the `]`. `SubRange[lhs]`.
        expr_block,

        /// `lhs : rhs`. main_token is the `:`.
        assign,
        /// `lhs :: rhs`. main_token is the `::`.
        global_assign,

        /// Both lhs and rhs unused. main_token is the `+`.
        plus,
        /// Both lhs and rhs unused. main_token is the `-`.
        minus,
        /// Both lhs and rhs unused. main_token is the `*`.
        asterisk,
        /// Both lhs and rhs unused. main_token is the `%`.
        percent,
        /// Both lhs and rhs unused. main_token is the `&`.
        ampersand,
        /// Both lhs and rhs unused. main_token is the `|`.
        pipe,
        /// Both lhs and rhs unused. main_token is the `^`.
        caret,
        /// Both lhs and rhs unused. main_token is the `=`.
        equal,
        /// Both lhs and rhs unused. main_token is the `<`.
        angle_bracket_left,
        /// Both lhs and rhs unused. main_token is the `<=`.
        angle_bracket_left_equal,
        /// Both lhs and rhs unused. main_token is the `<>`.
        angle_bracket_left_right,
        /// Both lhs and rhs unused. main_token is the `>`.
        angle_bracket_right,
        /// Both lhs and rhs unused. main_token is the `>=`.
        angle_bracket_right_equal,
        /// Both lhs and rhs unused. main_token is the `$`.
        dollar,
        /// Both lhs and rhs unused. main_token is the `,`.
        comma,
        /// Both lhs and rhs unused. main_token is the `#`.
        hash,
        /// Both lhs and rhs unused. main_token is the `_`.
        underscore,
        /// Both lhs and rhs unused. main_token is the `~`.
        tilde,
        /// Both lhs and rhs unused. main_token is the `!`.
        bang,
        /// Both lhs and rhs unused. main_token is the `?`.
        question_mark,
        /// Both lhs and rhs unused. main_token is the `@`.
        at,
        /// Both lhs and rhs unused. main_token is the `.`.
        period,
        /// Both lhs and rhs unused. main_token is the `0:`.
        zero_colon,
        /// Both lhs and rhs unused. main_token is the `1:`.
        one_colon,
        /// Both lhs and rhs unused. main_token is the `2:`.
        two_colon,

        /// `lhs'`. lhs can be omitted. rhs unused. main_token is the `'`.
        apostrophe,
        /// `lhs':`. lhs can be omitted. rhs unused. main_token is the `':`.
        apostrophe_colon,
        /// `lhs/`. lhs can be omitted. rhs unused. main_token is the `/`.
        slash,
        /// `lhs/:`. lhs can be omitted. rhs unused. main_token is the `/:`.
        slash_colon,
        /// `lhs\`. lhs can be omitted. rhs unused. main_token is the `\`.
        backslash,
        /// `lhs\:`. lhs can be omitted. rhs unused. main_token is the `\:`.
        backslash_colon,

        /// `lhs rhs`. main_token is unused.
        apply_unary,
        /// `lhs op rhs`. rhs can be omitted. main_token is the operator node.
        apply_binary,

        /// Both lhs and rhs unused.
        number_literal,
        /// main_token is the first number literal token. lhs is the last number literal token.
        /// rhs unused.
        number_list_literal,
        /// main_token is the string literal token.
        /// Both lhs and rhs unused.
        string_literal,
        /// main_token is the symbol literal token.
        /// Both lhs and rhs unused.
        symbol_literal,
        /// main_token is the first symbol literal token. lhs is the last symbol literal token.
        /// rhs unused.
        symbol_list_literal,
        /// Both lhs and rhs unused.
        /// Most identifiers will not have explicit AST nodes, however for expressions
        /// which could be one of many different kinds of AST nodes, there will be an
        /// identifier AST node for it.
        identifier,

        /// `select lhs`. rhs unused. main_token is the `select`. `Select[lhs]`.
        select,

        pub fn getType(tag: Tag) Type {
            return switch (tag) {
                .root,
                => unreachable,

                .empty,
                .null,
                => .other,

                .grouped_expression,
                .empty_list,
                .list,
                => .other,

                .table_literal,
                => .other,

                .lambda,
                .lambda_semicolon,
                => .other,

                .expr_block,
                => .other,

                .assign,
                .global_assign,
                => .other,

                .plus,
                .minus,
                .asterisk,
                .percent,
                .ampersand,
                .pipe,
                .caret,
                .equal,
                .angle_bracket_left,
                .angle_bracket_left_equal,
                .angle_bracket_left_right,
                .angle_bracket_right,
                .angle_bracket_right_equal,
                .dollar,
                .comma,
                .hash,
                .underscore,
                .tilde,
                .bang,
                .question_mark,
                .at,
                .period,
                .zero_colon,
                .one_colon,
                .two_colon,
                => .binary_operator,

                .apostrophe,
                .apostrophe_colon,
                .slash,
                .slash_colon,
                .backslash,
                .backslash_colon,
                => .iterator,

                .apply_unary,
                .apply_binary,
                => .other,

                .number_literal,
                .number_list_literal,
                .string_literal,
                .symbol_literal,
                .symbol_list_literal,
                .identifier,
                => .other,

                .select,
                => .other,
            };
        }
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

    pub const Lambda = struct {
        l_bracket: Token.Index,
        r_bracket: Token.Index,
        /// Index into extra_data.
        params_start: Index,
        /// Index into extra_data.
        params_end: Index,
        /// Index into extra_data.
        body_start: Index,
        /// Index into extra_data.
        body_end: Index,
    };

    pub const Table = struct {
        /// Index into extra_data.
        keys_start: Index,
        /// Index into extra_data.
        keys_end: Index,
        /// Index into extra_data.
        columns_start: Index,
        /// Index into extra_data.
        columns_end: Index,
    };

    pub const Select = struct {
        /// Index into extra_data.
        select_start: Index,
        /// Index into extra_data.
        select_end: Index,
        /// Index into extra_data.
        by_start: Index,
        /// Index into extra_data.
        by_end: Index,
        from: Index,
        /// Index into extra_data.
        where_start: Index,
        /// Index into extra_data.
        where_end: Index,
        data: packed struct(Index) {
            has_by: bool,
            distinct: bool,
            _: u30 = 0,
        },
    };
};

pub fn nodeToSpan(tree: *const Ast, node: u32) Span {
    return tokensToSpan(
        tree,
        tree.firstToken(node),
        tree.lastToken(node),
        tree.nodes.items(.main_token)[node],
    );
}

pub fn tokensToSpan(tree: *const Ast, start: Ast.Token.Index, end: Ast.Token.Index, main: Ast.Token.Index) Span {
    const token_locs = tree.tokens.items(.loc);
    var start_tok = start;
    var end_tok = end;

    if (tree.tokensOnSameLine(start, end)) {
        // do nothing
    } else if (tree.tokensOnSameLine(start, main)) {
        end_tok = main;
    } else if (tree.tokensOnSameLine(main, end)) {
        start_tok = main;
    } else {
        start_tok = main;
        end_tok = main;
    }
    const start_off = token_locs[start_tok].start;
    const end_off = token_locs[end_tok].start + @as(u32, @intCast(tree.tokenSlice(end_tok).len));
    return Span{
        .start = @intCast(start_off),
        .end = @intCast(end_off),
        .main = @intCast(token_locs[main].start),
    };
}

fn testAstRender(
    source_code: [:0]const u8,
    formatted_code: [:0]const u8,
    expected_tokens: []const Token.Tag,
    expected_nodes: []const Node.Tag,
) !void {
    inline for (@typeInfo(Mode).@"enum".fields) |field| {
        try testAstModeRender(
            @enumFromInt(field.value),
            source_code,
            formatted_code,
            expected_tokens,
            expected_nodes,
        );
    }
}

fn testAst(
    source_code: [:0]const u8,
    expected_tokens: []const Token.Tag,
    expected_nodes: []const Node.Tag,
) !void {
    inline for (@typeInfo(Mode).@"enum".fields) |field| {
        try testAstModeRender(
            @enumFromInt(field.value),
            source_code,
            null,
            expected_tokens,
            expected_nodes,
        );
    }
}

fn testAstMode(
    mode: Mode,
    source_code: [:0]const u8,
    expected_tokens: []const Token.Tag,
    expected_nodes: []const Node.Tag,
) !void {
    return testAstModeRender(
        mode,
        source_code,
        null,
        expected_tokens,
        expected_nodes,
    );
}

fn testAstModeRender(
    mode: Mode,
    source_code: [:0]const u8,
    expected_code: ?[:0]const u8,
    expected_tokens: []const Token.Tag,
    expected_nodes: []const Node.Tag,
) !void {
    const gpa = std.testing.allocator;

    var tree = try Ast.parse(gpa, source_code, mode);
    defer tree.deinit(gpa);

    // Token tags
    const actual_token_tags: []Token.Tag = tree.tokens.items(.tag);
    try std.testing.expectEqualSlices(Token.Tag, expected_tokens, actual_token_tags[0 .. actual_token_tags.len - 1]);
    try std.testing.expectEqual(.eof, actual_token_tags[actual_token_tags.len - 1]);

    // Node tags
    const actual_nodes: []Node.Tag = tree.nodes.items(.tag);
    try std.testing.expectEqualSlices(Node.Tag, expected_nodes, actual_nodes[1..]);
    try std.testing.expectEqual(.root, actual_nodes[0]);

    // Errors
    try std.testing.expect(tree.errors.len == 0);

    // Render
    const actual_source = try render(tree, gpa);
    defer gpa.free(actual_source);
    const expected_source = expected_source: {
        const expected = expected_code orelse source_code;
        if (expected.len == 0) break :expected_source try gpa.dupe(u8, expected);

        const expected_source = try gpa.alloc(u8, expected.len + 1);
        @memcpy(expected_source[0 .. expected_source.len - 1], expected);
        expected_source[expected_source.len - 1] = '\n';
        break :expected_source expected_source;
    };
    defer gpa.free(expected_source);
    try std.testing.expectEqualStrings(expected_source, actual_source);
}

fn failAst(
    source_code: [:0]const u8,
    expected_tokens: []const Token.Tag,
    expected_errors: []const Error.Tag,
) !void {
    inline for (@typeInfo(Mode).@"enum".fields) |field| {
        try failAstMode(
            @enumFromInt(field.value),
            source_code,
            expected_tokens,
            expected_errors,
        );
    }
}

fn failAstMode(
    mode: Mode,
    source_code: [:0]const u8,
    expected_tokens: []const Token.Tag,
    expected_errors: []const Error.Tag,
) !void {
    const gpa = std.testing.allocator;

    var tree = try Ast.parse(gpa, source_code, mode);
    defer tree.deinit(gpa);

    // Token tags
    const actual_tokens: []Token.Tag = tree.tokens.items(.tag);
    try std.testing.expectEqualSlices(Token.Tag, expected_tokens, actual_tokens[0 .. actual_tokens.len - 1]);
    try std.testing.expectEqual(.eof, actual_tokens[actual_tokens.len - 1]);

    // Errors
    const actual_errors = try gpa.alloc(Error.Tag, tree.errors.len);
    defer gpa.free(actual_errors);
    for (tree.errors, actual_errors) |err, *tag| tag.* = err.tag;
    try std.testing.expectEqualSlices(Error.Tag, expected_errors, actual_errors);
    try std.testing.expect(tree.errors.len > 0);
}

test "tokenize number" {
    try testAst("0", &.{.number_literal}, &.{.number_literal});
    try testAst("1", &.{.number_literal}, &.{.number_literal});
    try testAst("-1", &.{.number_literal}, &.{.number_literal});
    try testAst("123", &.{.number_literal}, &.{.number_literal});
    try testAst("-123", &.{.number_literal}, &.{.number_literal});
    try testAst(".1", &.{.number_literal}, &.{.number_literal});
    try testAst("-.1", &.{.number_literal}, &.{.number_literal});
    try testAst("1.1", &.{.number_literal}, &.{.number_literal});
    try testAst("-1.1", &.{.number_literal}, &.{.number_literal});
    try testAst("1.", &.{.number_literal}, &.{.number_literal});
    try testAst("-1.", &.{.number_literal}, &.{.number_literal});
    try testAst(
        "-.",
        &.{ .minus, .period },
        &.{ .minus, .period, .apply_binary },
    );
}

test "tokenize negative number" {
    try testAst(
        "(-1)",
        &.{ .l_paren, .number_literal, .r_paren },
        &.{ .grouped_expression, .number_literal },
    );
    try testAst(
        "(-.1)",
        &.{ .l_paren, .number_literal, .r_paren },
        &.{ .grouped_expression, .number_literal },
    );
    try testAst(
        "()-1",
        &.{ .l_paren, .r_paren, .minus, .number_literal },
        &.{ .empty_list, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "()-.1",
        &.{ .l_paren, .r_paren, .minus, .number_literal },
        &.{ .empty_list, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "() -1",
        &.{ .l_paren, .r_paren, .number_literal },
        &.{ .empty_list, .number_literal, .apply_unary },
    );
    try testAst(
        "() -.1",
        &.{ .l_paren, .r_paren, .number_literal },
        &.{ .empty_list, .number_literal, .apply_unary },
    );
    try testAst(
        "{-1}",
        &.{ .l_brace, .number_literal, .r_brace },
        &.{ .lambda, .number_literal },
    );
    try testAst(
        "{-.1}",
        &.{ .l_brace, .number_literal, .r_brace },
        &.{ .lambda, .number_literal },
    );
    try testAst(
        "{}-1",
        &.{ .l_brace, .r_brace, .minus, .number_literal },
        &.{ .lambda, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "{}-.1",
        &.{ .l_brace, .r_brace, .minus, .number_literal },
        &.{ .lambda, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "{} -1",
        &.{ .l_brace, .r_brace, .number_literal },
        &.{ .lambda, .number_literal, .apply_unary },
    );
    try testAst(
        "{} -.1",
        &.{ .l_brace, .r_brace, .number_literal },
        &.{ .lambda, .number_literal, .apply_unary },
    );
    try testAst(
        "[-1]",
        &.{ .l_bracket, .number_literal, .r_bracket },
        &.{ .expr_block, .number_literal },
    );
    try testAst(
        "[-.1]",
        &.{ .l_bracket, .number_literal, .r_bracket },
        &.{ .expr_block, .number_literal },
    );
    try testAst(
        "[]-1",
        &.{ .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .expr_block, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "[]-.1",
        &.{ .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .expr_block, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "[] -1",
        &.{ .l_bracket, .r_bracket, .number_literal },
        &.{ .expr_block, .number_literal, .apply_unary },
    );
    try testAst(
        "[] -.1",
        &.{ .l_bracket, .r_bracket, .number_literal },
        &.{ .expr_block, .number_literal, .apply_unary },
    );
    try testAstRender(
        ";-1",
        "-1",
        &.{ .semicolon, .number_literal },
        &.{.number_literal},
    );
    try testAstRender(
        ";-.1",
        "-.1",
        &.{ .semicolon, .number_literal },
        &.{.number_literal},
    );
    try testAst(
        "1-1",
        &.{ .number_literal, .minus, .number_literal },
        &.{ .number_literal, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "1-.1",
        &.{ .number_literal, .minus, .number_literal },
        &.{ .number_literal, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "1 -1",
        &.{ .number_literal, .number_literal },
        &.{.number_list_literal},
    );
    try testAst(
        "1 -.1",
        &.{ .number_literal, .number_literal },
        &.{.number_list_literal},
    );
    try testAst(
        "\"string\"-1",
        &.{ .string_literal, .minus, .number_literal },
        &.{ .string_literal, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "\"string\"-.1",
        &.{ .string_literal, .minus, .number_literal },
        &.{ .string_literal, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "`symbol-1",
        &.{ .symbol_literal, .minus, .number_literal },
        &.{ .symbol_literal, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "`symbol-.1",
        &.{ .symbol_literal, .minus, .number_literal },
        &.{ .symbol_literal, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "`symbol`list-1",
        &.{ .symbol_literal, .symbol_literal, .minus, .number_literal },
        &.{ .symbol_list_literal, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "`symbol`list-.1",
        &.{ .symbol_literal, .symbol_literal, .minus, .number_literal },
        &.{ .symbol_list_literal, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "identifier-1",
        &.{ .identifier, .minus, .number_literal },
        &.{ .identifier, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "identifier-.1",
        &.{ .identifier, .minus, .number_literal },
        &.{ .identifier, .minus, .number_literal, .apply_binary },
    );
}

// try testAst(
//     "{-1}[]-1",
//     &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
//     &.{
//         .lambda, .lambda_body, .number_literal, .expr_block, .apply_unary, .sub, .number_literal, .apply_binary,
//     },
// );
// try testAst(
//     "{-1}[]- 1",
//     &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
//     &.{
//         .lambda, .lambda_body, .number_literal, .expr_block, .apply_unary, .sub, .number_literal, .apply_binary,
//     },
// );
// try testAst(
//     "{-1}[] - 1",
//     &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
//     &.{
//         .lambda, .lambda_body, .number_literal, .expr_block, .apply_unary, .sub, .number_literal, .apply_binary,
//     },
// );
// try testAst(
//     "{-1}[] -1",
//     &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .number_literal },
//     &.{
//         .lambda, .lambda_body, .number_literal, .expr_block, .apply_unary, .number_literal, .apply_unary,
//     },
// );
// try testAst(
//     "([]-1)",
//     &.{ .l_paren, .l_bracket, .r_bracket, .number_literal, .r_paren },
//     &.{ .table, .number_literal },
// );
// try testAst(
//     "([x]-1)",
//     &.{ .l_paren, .l_bracket, .identifier, .r_bracket, .number_literal, .r_paren },
//     &.{ .table, .identifier, .number_literal },
// );

test "lambda renders on same line" {
    try testAst(
        "{}",
        &.{ .l_brace, .r_brace },
        &.{.lambda},
    );
    try testAst(
        "{1}",
        &.{ .l_brace, .number_literal, .r_brace },
        &.{ .lambda, .number_literal },
    );
    try testAst(
        "{1;}",
        &.{ .l_brace, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .number_literal },
    );
    try testAst(
        "{1;2}",
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .r_brace },
        &.{ .lambda, .number_literal, .number_literal },
    );
    try testAst(
        "{1;2;}",
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .number_literal, .number_literal },
    );
    try testAst(
        "{[]}",
        &.{ .l_brace, .l_bracket, .r_bracket, .r_brace },
        &.{.lambda},
    );
    try testAst(
        "{[]1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .number_literal },
    );
    try testAstMode(
        .k,
        "{[]-1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .number_literal },
    );
    try testAstMode(
        .q,
        "{[] -1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .number_literal },
    );
    try testAst(
        "{[]1;}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .number_literal },
    );
    try testAstMode(
        .k,
        "{[]-1;}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .number_literal },
    );
    try testAstMode(
        .q,
        "{[] -1;}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .number_literal },
    );
    try testAst(
        "{[]1;2}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .number_literal, .r_brace },
        &.{ .lambda, .number_literal, .number_literal },
    );
    try testAst(
        "{[]1;2;}",
        &.{
            .l_brace,   .l_bracket,      .r_bracket, .number_literal,
            .semicolon, .number_literal, .semicolon, .r_brace,
        },
        &.{ .lambda_semicolon, .number_literal, .number_literal },
    );
    try testAst(
        "{[x]}",
        &.{ .l_brace, .l_bracket, .identifier, .r_bracket, .r_brace },
        &.{ .lambda, .identifier },
    );
    try testAst(
        "{[x]1}",
        &.{ .l_brace, .l_bracket, .identifier, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .identifier, .number_literal },
    );
    try testAst(
        "{[x]1;}",
        &.{ .l_brace, .l_bracket, .identifier, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .identifier, .number_literal },
    );
    try testAst(
        "{[x]1;2}",
        &.{
            .l_brace, .l_bracket, .identifier, .r_bracket, .number_literal, .semicolon, .number_literal, .r_brace,
        },
        &.{ .lambda, .identifier, .number_literal, .number_literal },
    );
    try testAst(
        "{[x]1;2;}",
        &.{
            .l_brace,   .l_bracket,      .identifier, .r_bracket, .number_literal,
            .semicolon, .number_literal, .semicolon,  .r_brace,
        },
        &.{ .lambda_semicolon, .identifier, .number_literal, .number_literal },
    );
    try testAst(
        "{[x;y]}",
        &.{
            .l_brace,    .l_bracket, .identifier, .semicolon,
            .identifier, .r_bracket, .r_brace,
        },
        &.{ .lambda, .identifier, .identifier },
    );
    try testAst(
        "{[x;y]1}",
        &.{
            .l_brace, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket, .number_literal, .r_brace,
        },
        &.{ .lambda, .identifier, .identifier, .number_literal },
    );
    try testAst(
        "{[x;y]1;}",
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon, .identifier,
            .r_bracket, .number_literal, .semicolon,  .r_brace,
        },
        &.{ .lambda_semicolon, .identifier, .identifier, .number_literal },
    );
    try testAst(
        "{[x;y]1;2}",
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon,      .identifier,
            .r_bracket, .number_literal, .semicolon,  .number_literal, .r_brace,
        },
        &.{ .lambda, .identifier, .identifier, .number_literal, .number_literal },
    );
    try testAst(
        "{[x;y]1;2;}",
        &.{
            .l_brace,        .l_bracket, .identifier,     .semicolon, .identifier, .r_bracket,
            .number_literal, .semicolon, .number_literal, .semicolon, .r_brace,
        },
        &.{ .lambda_semicolon, .identifier, .identifier, .number_literal, .number_literal },
    );
}

test "lambda renders on multiple lines" {
    try testAst(
        \\{[]
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .r_brace },
        &.{.lambda},
    );
    try testAstRender(
        \\{[]
        \\  ;}
    ,
        \\{[]
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .semicolon, .r_brace },
        &.{.lambda_semicolon},
    );
    try testAstRender(
        \\{[]
        \\  ;
        \\  }
    ,
        \\{[]
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .semicolon, .r_brace },
        &.{.lambda_semicolon},
    );
    try testAst(
        \\{[]
        \\  -1}
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .number_literal },
    );
    try testAstRender(
        \\{[]
        \\  -1;}
    ,
        \\{[]
        \\  -1;
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .number_literal },
    );
    try testAst(
        \\{[]
        \\  -1;
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .number_literal },
    );

    try testAst(
        \\{[x;
        \\  y;
        \\  z]
        \\  1;2;3}
    ,
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon,      .identifier, .semicolon,      .identifier,
            .r_bracket, .number_literal, .semicolon,  .number_literal, .semicolon,  .number_literal, .r_brace,
        },
        &.{
            .lambda, .identifier, .identifier, .identifier, .number_literal, .number_literal, .number_literal,
        },
    );
    try testAst(
        \\{[x;
        \\  y;
        \\  z]
        \\  1;2;3;}
    ,
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon,      .identifier, .semicolon,      .identifier,
            .r_bracket, .number_literal, .semicolon,  .number_literal, .semicolon,  .number_literal, .semicolon,
            .r_brace,
        },
        &.{
            .lambda_semicolon, .identifier,     .identifier,     .identifier,
            .number_literal,   .number_literal, .number_literal,
        },
    );
    try testAst(
        \\{[x;y;z]
        \\  1;
        \\  2;
        \\  3}
    ,
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon,      .identifier, .semicolon,      .identifier,
            .r_bracket, .number_literal, .semicolon,  .number_literal, .semicolon,  .number_literal, .r_brace,
        },
        &.{
            .lambda, .identifier, .identifier, .identifier, .number_literal, .number_literal, .number_literal,
        },
    );
    try testAst(
        \\{[x;y;z]
        \\  1;
        \\  2;
        \\  3;
        \\  }
    ,
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon,      .identifier, .semicolon,      .identifier,
            .r_bracket, .number_literal, .semicolon,  .number_literal, .semicolon,  .number_literal, .semicolon,
            .r_brace,
        },
        &.{
            .lambda_semicolon, .identifier,     .identifier,     .identifier,
            .number_literal,   .number_literal, .number_literal,
        },
    );
    try testAst(
        \\{[x;
        \\  y;
        \\  z]
        \\  1;
        \\  2;
        \\  3}
    ,
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon,      .identifier, .semicolon,      .identifier,
            .r_bracket, .number_literal, .semicolon,  .number_literal, .semicolon,  .number_literal, .r_brace,
        },
        &.{
            .lambda, .identifier, .identifier, .identifier, .number_literal, .number_literal, .number_literal,
        },
    );
    try testAst(
        \\{[x;
        \\  y;
        \\  z]
        \\  1;
        \\  2;
        \\  3;
        \\  }
    ,
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon,      .identifier, .semicolon,      .identifier,
            .r_bracket, .number_literal, .semicolon,  .number_literal, .semicolon,  .number_literal, .semicolon,
            .r_brace,
        },
        &.{
            .lambda_semicolon, .identifier,     .identifier,     .identifier,
            .number_literal,   .number_literal, .number_literal,
        },
    );
}

test "precedence" {
    try testAst(
        "2*3+4",
        &.{ .number_literal, .asterisk, .number_literal, .plus, .number_literal },
        &.{
            .number_literal, .asterisk, .number_literal, .plus, .number_literal, .apply_binary, .apply_binary,
        },
    );
    try testAst(
        "(2*3)+4",
        &.{
            .l_paren, .number_literal, .asterisk, .number_literal, .r_paren, .plus, .number_literal,
        },
        &.{
            .grouped_expression, .number_literal, .asterisk,       .number_literal,
            .apply_binary,       .plus,           .number_literal, .apply_binary,
        },
    );
}

test "assign" {
    try testAst(
        "a:1",
        &.{ .identifier, .colon, .number_literal },
        &.{ .identifier, .assign, .number_literal },
    );
    try testAst(
        "3:1",
        &.{ .number_literal, .colon, .number_literal },
        &.{ .number_literal, .assign, .number_literal },
    );

    try testAst(
        "a::1",
        &.{ .identifier, .colon_colon, .number_literal },
        &.{ .identifier, .global_assign, .number_literal },
    );
    try testAst(
        "3::1",
        &.{ .number_literal, .colon_colon, .number_literal },
        &.{ .number_literal, .global_assign, .number_literal },
    );
}

test "table literals" {
    try testAst(
        "([]())",
        &.{ .l_paren, .l_bracket, .r_bracket, .l_paren, .r_paren, .r_paren },
        &.{ .table_literal, .empty_list },
    );
    try testAst(
        "([]1 2)",
        &.{ .l_paren, .l_bracket, .r_bracket, .number_literal, .number_literal, .r_paren },
        &.{ .table_literal, .number_list_literal },
    );
    try testAst(
        "([]a:1 2)",
        &.{
            .l_paren, .l_bracket, .r_bracket, .identifier, .colon, .number_literal, .number_literal, .r_paren,
        },
        &.{ .table_literal, .identifier, .assign, .number_list_literal },
    );
    try testAst(
        "([]a::1 2)",
        &.{
            .l_paren, .l_bracket, .r_bracket, .identifier, .colon_colon, .number_literal, .number_literal, .r_paren,
        },
        &.{ .table_literal, .identifier, .global_assign, .number_list_literal },
    );
    try testAst(
        "([]a:1 2;b:2)",
        &.{
            .l_paren,        .l_bracket, .r_bracket,  .identifier, .colon,          .number_literal,
            .number_literal, .semicolon, .identifier, .colon,      .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier, .assign, .number_list_literal, .identifier, .assign, .number_literal,
        },
    );
    try testAst(
        "([]a)",
        &.{ .l_paren, .l_bracket, .r_bracket, .identifier, .r_paren },
        &.{ .table_literal, .identifier },
    );
    try testAst(
        "([]b+sum a)",
        &.{ .l_paren, .l_bracket, .r_bracket, .identifier, .plus, .identifier, .identifier, .r_paren },
        &.{ .table_literal, .identifier, .plus, .identifier, .identifier, .apply_unary, .apply_binary },
    );
    try testAst(
        "([]sum[a]+b)",
        &.{
            .l_paren,    .l_bracket, .r_bracket, .identifier, .l_bracket,
            .identifier, .r_bracket, .plus,      .identifier, .r_paren,
        },
        &.{
            .table_literal, .identifier, .expr_block, .identifier, .apply_unary, .plus, .identifier, .apply_binary,
        },
    );
    try testAst(
        "([](a;b;c))",
        &.{
            .l_paren,    .l_bracket, .r_bracket,  .l_paren, .identifier, .semicolon,
            .identifier, .semicolon, .identifier, .r_paren, .r_paren,
        },
        &.{ .table_literal, .list, .identifier, .identifier, .identifier },
    );
    try testAst(
        "([]til 10;x1:1;2)",
        &.{
            .l_paren,    .l_bracket, .r_bracket,      .identifier, .number_literal, .semicolon,
            .identifier, .colon,     .number_literal, .semicolon,  .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier, .number_literal, .apply_unary,
            .identifier,    .assign,     .number_literal, .number_literal,
        },
    );
    try testAst(
        "([]a;a::til 10)",
        &.{
            .l_paren,    .l_bracket,   .r_bracket,  .identifier,     .semicolon,
            .identifier, .colon_colon, .identifier, .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier, .identifier, .global_assign, .identifier, .number_literal, .apply_unary,
        },
    );

    try testAst(
        "([()]())",
        &.{ .l_paren, .l_bracket, .l_paren, .r_paren, .r_bracket, .l_paren, .r_paren, .r_paren },
        &.{ .table_literal, .empty_list, .empty_list },
    );
    try testAst(
        "([1 2]1 2)",
        &.{
            .l_paren,   .l_bracket,      .number_literal, .number_literal,
            .r_bracket, .number_literal, .number_literal, .r_paren,
        },
        &.{ .table_literal, .number_list_literal, .number_list_literal },
    );
    try testAst(
        "([a:1 2]a:1 2)",
        &.{
            .l_paren,   .l_bracket,  .identifier, .colon,          .number_literal, .number_literal,
            .r_bracket, .identifier, .colon,      .number_literal, .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier, .assign, .number_list_literal, .identifier, .assign, .number_list_literal,
        },
    );
    try testAst(
        "([a::1 2]a::1 2)",
        &.{
            .l_paren,   .l_bracket,  .identifier,  .colon_colon,    .number_literal, .number_literal,
            .r_bracket, .identifier, .colon_colon, .number_literal, .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier,    .global_assign,       .number_list_literal,
            .identifier,    .global_assign, .number_list_literal,
        },
    );
    try testAst(
        "([a:1 2;b:2]a:1 2;b:2)",
        &.{
            .l_paren,        .l_bracket, .identifier,     .colon,     .number_literal, .number_literal, .semicolon,
            .identifier,     .colon,     .number_literal, .r_bracket, .identifier,     .colon,          .number_literal,
            .number_literal, .semicolon, .identifier,     .colon,     .number_literal, .r_paren,
        },
        &.{
            .table_literal,  .identifier, .assign, .number_list_literal, .identifier, .assign,
            .number_literal, .identifier, .assign, .number_list_literal, .identifier, .assign,
            .number_literal,
        },
    );
    try testAst(
        "([a]a)",
        &.{ .l_paren, .l_bracket, .identifier, .r_bracket, .identifier, .r_paren },
        &.{ .table_literal, .identifier, .identifier },
    );
    try testAst(
        "([b+sum a]b+sum a)",
        &.{
            .l_paren,   .l_bracket,  .identifier, .plus,       .identifier, .identifier,
            .r_bracket, .identifier, .plus,       .identifier, .identifier, .r_paren,
        },
        &.{
            .table_literal, .identifier, .plus,       .identifier, .identifier,  .apply_unary,  .apply_binary,
            .identifier,    .plus,       .identifier, .identifier, .apply_unary, .apply_binary,
        },
    );
    try testAst(
        "([sum[a]+b]sum[a]+b)",
        &.{
            .l_paren,   .l_bracket,  .identifier, .l_bracket,  .identifier, .r_bracket, .plus,       .identifier,
            .r_bracket, .identifier, .l_bracket,  .identifier, .r_bracket,  .plus,      .identifier, .r_paren,
        },
        &.{
            .table_literal, .identifier, .expr_block, .identifier, .apply_unary, .plus, .identifier,
            .apply_binary,  .identifier, .expr_block, .identifier, .apply_unary, .plus, .identifier,
            .apply_binary,
        },
    );
    try testAst(
        "([(a;b;c)](a;b;c))",
        &.{
            .l_paren, .l_bracket, .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier,
            .r_paren, .r_bracket, .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier,
            .r_paren, .r_paren,
        },
        &.{
            .table_literal, .list, .identifier, .identifier, .identifier, .list, .identifier, .identifier, .identifier,
        },
    );
    try testAst(
        "([til 10;x1:1;2]til 10;x1:1;2)",
        &.{
            .l_paren,        .l_bracket, .identifier,     .number_literal, .semicolon,      .identifier,     .colon,
            .number_literal, .semicolon, .number_literal, .r_bracket,      .identifier,     .number_literal, .semicolon,
            .identifier,     .colon,     .number_literal, .semicolon,      .number_literal, .r_paren,
        },
        &.{
            .table_literal,  .identifier,     .number_literal, .apply_unary,    .identifier,  .assign,
            .number_literal, .number_literal, .identifier,     .number_literal, .apply_unary, .identifier,
            .assign,         .number_literal, .number_literal,
        },
    );
    try testAst(
        "([a;a::til 10]a;a::til 10)",
        &.{
            .l_paren,        .l_bracket, .identifier, .semicolon, .identifier, .colon_colon, .identifier,
            .number_literal, .r_bracket, .identifier, .semicolon, .identifier, .colon_colon, .identifier,
            .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier, .identifier,    .global_assign, .identifier,     .number_literal, .apply_unary,
            .identifier,    .identifier, .global_assign, .identifier,    .number_literal, .apply_unary,
        },
    );
}

test "number literals" {
    try testAst(
        "1",
        &.{.number_literal},
        &.{.number_literal},
    );
    try testAst(
        "1 2",
        &.{ .number_literal, .number_literal },
        &.{.number_list_literal},
    );
    try testAst(
        "1 2 3",
        &.{ .number_literal, .number_literal, .number_literal },
        &.{.number_list_literal},
    );
    try testAst(
        "1 2 3+4 5 6+7 8 9",
        &.{
            .number_literal, .number_literal, .number_literal, .plus,           .number_literal, .number_literal,
            .number_literal, .plus,           .number_literal, .number_literal, .number_literal,
        },
        &.{
            .number_list_literal, .plus,         .number_list_literal, .plus,
            .number_list_literal, .apply_binary, .apply_binary,
        },
    );
}

test "symbol literals" {
    try testAst("`", &.{.symbol_literal}, &.{.symbol_literal});
    try testAst("`symbol", &.{.symbol_literal}, &.{.symbol_literal});
    try testAst(
        "`symbol`symbol",
        &.{ .symbol_literal, .symbol_literal },
        &.{.symbol_list_literal},
    );
}

test "operators" {
    try testAst("+", &.{.plus}, &.{.plus});
    try testAst(
        "1+",
        &.{ .number_literal, .plus },
        &.{ .number_literal, .plus, .apply_binary },
    );
    // try failAst(
    //     "+1",
    //     &.{ .plus, .number_literal },
    //     &.{.cannot_project_operator_without_lhs},
    // );
    try testAst(
        "1+2",
        &.{ .number_literal, .plus, .number_literal },
        &.{ .number_literal, .plus, .number_literal, .apply_binary },
    );
    try testAst(
        "(+)",
        &.{ .l_paren, .plus, .r_paren },
        &.{ .grouped_expression, .plus },
    );
    try testAst(
        "(1+)",
        &.{ .l_paren, .number_literal, .plus, .r_paren },
        &.{ .grouped_expression, .number_literal, .plus, .apply_binary },
    );
    // try failAst(
    //     "(+1)",
    //     &.{ .l_paren, .plus, .number_literal, .r_paren },
    //     &.{.cannot_project_operator_without_lhs},
    // );
    try testAst(
        "(+)1",
        &.{ .l_paren, .plus, .r_paren, .number_literal },
        &.{ .grouped_expression, .plus, .number_literal, .apply_unary },
    );
}

test "iterators" {
    try testAst(
        "(/)",
        &.{ .l_paren, .slash, .r_paren },
        &.{ .grouped_expression, .slash },
    );
    try testAst("+/", &.{ .plus, .slash }, &.{ .plus, .slash });
    try testAst(
        "1+/",
        &.{ .number_literal, .plus, .slash },
        &.{ .number_literal, .plus, .slash, .apply_binary },
    );
    try testAstMode(
        .k,
        "+/1",
        &.{ .plus, .slash, .number_literal },
        &.{ .plus, .slash, .number_literal, .apply_unary },
    );
    // try failAstMode(
    //     .q,
    //     "+/1",
    //     &.{ .plus, .slash, .number_literal },
    //     &.{.cannot_apply_iterator_directly},
    // );
    try testAst(
        "1+/1",
        &.{ .number_literal, .plus, .slash, .number_literal },
        &.{ .number_literal, .plus, .slash, .number_literal, .apply_binary },
    );

    try testAst(
        \\(\:)
    ,
        &.{ .l_paren, .backslash_colon, .r_paren },
        &.{ .grouped_expression, .backslash_colon },
    );
    try testAst(
        \\@\:
    ,
        &.{ .at, .backslash_colon },
        &.{ .at, .backslash_colon },
    );
    try testAst(
        \\f\:
    ,
        &.{ .identifier, .backslash_colon },
        &.{ .identifier, .backslash_colon },
    );
    try testAst(
        "@\\:[x;y]",
        &.{ .at, .backslash_colon, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .at, .backslash_colon, .expr_block, .identifier, .identifier, .apply_unary },
    );
    try testAst(
        "f\\:[x;y]",
        &.{
            .identifier, .backslash_colon, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket,
        },
        &.{ .identifier, .backslash_colon, .expr_block, .identifier, .identifier, .apply_unary },
    );
    try testAst(
        "x@\\:y",
        &.{ .identifier, .at, .backslash_colon, .identifier },
        &.{ .identifier, .at, .backslash_colon, .identifier, .apply_binary },
    );
    try testAst(
        "1+\\:1",
        &.{ .number_literal, .plus, .backslash_colon, .number_literal },
        &.{ .number_literal, .plus, .backslash_colon, .number_literal, .apply_binary },
    );
    try testAst(
        "x f\\:y",
        &.{ .identifier, .identifier, .backslash_colon, .identifier },
        &.{ .identifier, .identifier, .backslash_colon, .identifier, .apply_binary },
    );
    try testAst(
        "x{x+y}/y",
        &.{ .identifier, .l_brace, .identifier, .plus, .identifier, .r_brace, .slash, .identifier },
        &.{
            .identifier, .lambda, .identifier, .plus, .identifier, .apply_binary, .slash, .identifier, .apply_binary,
        },
    );
}

test "chained iterators" {
    try testAst(
        \\0 1 2,/:\:10 20 30
    ,
        &.{
            .number_literal,  .number_literal, .number_literal, .comma,          .slash_colon,
            .backslash_colon, .number_literal, .number_literal, .number_literal,
        },
        &.{
            .number_list_literal, .comma, .slash_colon, .backslash_colon, .number_list_literal, .apply_binary,
        },
    );
}

test "lists" {
    try testAst(
        "1 2 3",
        &.{ .number_literal, .number_literal, .number_literal },
        &.{.number_list_literal},
    );
    try testAst("()", &.{ .l_paren, .r_paren }, &.{.empty_list});
    try testAst(
        "(1)",
        &.{ .l_paren, .number_literal, .r_paren },
        &.{ .grouped_expression, .number_literal },
    );
    try testAst(
        "(;)",
        &.{ .l_paren, .semicolon, .r_paren },
        &.{ .list, .empty, .empty },
    );
    try testAst(
        "(;1)",
        &.{ .l_paren, .semicolon, .number_literal, .r_paren },
        &.{ .list, .empty, .number_literal },
    );
    try testAst(
        "(1;)",
        &.{ .l_paren, .number_literal, .semicolon, .r_paren },
        &.{ .list, .number_literal, .empty },
    );
    try testAst(
        "(;1)",
        &.{ .l_paren, .semicolon, .number_literal, .r_paren },
        &.{ .list, .empty, .number_literal },
    );
    try testAst(
        "(1;2)",
        &.{ .l_paren, .number_literal, .semicolon, .number_literal, .r_paren },
        &.{ .list, .number_literal, .number_literal },
    );
    try testAst(
        "(;;)",
        &.{ .l_paren, .semicolon, .semicolon, .r_paren },
        &.{ .list, .empty, .empty, .empty },
    );
    try testAst(
        "(1;;)",
        &.{ .l_paren, .number_literal, .semicolon, .semicolon, .r_paren },
        &.{ .list, .number_literal, .empty, .empty },
    );
    try testAst(
        "(;2;)",
        &.{ .l_paren, .semicolon, .number_literal, .semicolon, .r_paren },
        &.{ .list, .empty, .number_literal, .empty },
    );
    try testAst(
        "(;;3)",
        &.{ .l_paren, .semicolon, .semicolon, .number_literal, .r_paren },
        &.{ .list, .empty, .empty, .number_literal },
    );
    try testAst(
        "(1;2;3)",
        &.{
            .l_paren, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .r_paren,
        },
        &.{ .list, .number_literal, .number_literal, .number_literal },
    );
}

test "nested paren/bracket/brace" {
    try testAst(
        "((()))",
        &.{ .l_paren, .l_paren, .l_paren, .r_paren, .r_paren, .r_paren },
        &.{ .grouped_expression, .grouped_expression, .empty_list },
    );
}

// TODO: 0011b 0 1 2
test "number literal whitespace" {
    try testAst(
        "123\"string\"",
        &.{ .number_literal, .string_literal },
        &.{ .number_literal, .string_literal, .apply_unary },
    );
    try testAst(
        "123`symbol",
        &.{ .number_literal, .symbol_literal },
        &.{ .number_literal, .symbol_literal, .apply_unary },
    );
    try testAst(
        "123 identifier",
        &.{ .number_literal, .identifier },
        &.{ .number_literal, .identifier, .apply_unary },
    );
}

test "string literal whitespace" {
    try testAst(
        "\"string\"123",
        &.{ .string_literal, .number_literal },
        &.{ .string_literal, .number_literal, .apply_unary },
    );
    try testAst(
        "\"string\"\"string\"",
        &.{ .string_literal, .string_literal },
        &.{ .string_literal, .string_literal, .apply_unary },
    );
    try testAst(
        "\"string\"`symbol",
        &.{ .string_literal, .symbol_literal },
        &.{ .string_literal, .symbol_literal, .apply_unary },
    );
    try testAst(
        "\"string\"identifier",
        &.{ .string_literal, .identifier },
        &.{ .string_literal, .identifier, .apply_unary },
    );
}

test "symbol literal whitespace" {
    try testAst(
        "`symbol 123",
        &.{ .symbol_literal, .number_literal },
        &.{ .symbol_literal, .number_literal, .apply_unary },
    );
    try testAst(
        "`symbol\"string\"",
        &.{ .symbol_literal, .string_literal },
        &.{ .symbol_literal, .string_literal, .apply_unary },
    );
    try testAst(
        "`symbol `symbol",
        &.{ .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .symbol_literal, .apply_unary },
    );
    try testAst(
        "`symbol identifier",
        &.{ .symbol_literal, .identifier },
        &.{ .symbol_literal, .identifier, .apply_unary },
    );
}

test "identifier whitespace" {
    try testAst(
        "identifier 123",
        &.{ .identifier, .number_literal },
        &.{ .identifier, .number_literal, .apply_unary },
    );
    try testAst(
        "identifier\"string\"",
        &.{ .identifier, .string_literal },
        &.{ .identifier, .string_literal, .apply_unary },
    );
    try testAst(
        "identifier`symbol",
        &.{ .identifier, .symbol_literal },
        &.{ .identifier, .symbol_literal, .apply_unary },
    );
    try testAst(
        "identifier identifier",
        &.{ .identifier, .identifier },
        &.{ .identifier, .identifier, .apply_unary },
    );
}

test "multiple blocks" {
    try testAstRender(
        "1;;;;;2",
        "1;2",
        &.{
            .number_literal, .semicolon, .semicolon, .semicolon, .semicolon, .semicolon, .number_literal,
        },
        &.{ .number_literal, .number_literal },
    );
    try testAstRender(
        ";;;;;1;;;;;2",
        "1;2",
        &.{
            .semicolon, .semicolon, .semicolon, .semicolon, .semicolon, .number_literal,
            .semicolon, .semicolon, .semicolon, .semicolon, .semicolon, .number_literal,
        },
        &.{ .number_literal, .number_literal },
    );
    try testAstRender(
        "1\n\n\n\n\n2",
        "1\n2",
        &.{ .number_literal, .number_literal },
        &.{ .number_literal, .number_literal },
    );
    try testAstRender(
        ";\n\n\n\n1\n\n\n\n\n2",
        "1\n2",
        &.{ .semicolon, .number_literal, .number_literal },
        &.{ .number_literal, .number_literal },
    );
}

// TODO: Test unterminated block comments
test "render comments" {
    try testAst("", &.{}, &.{});
    try testAst(" comment", &.{}, &.{});
    try testAst("/comment", &.{}, &.{});
    try testAst("\\", &.{}, &.{});
    try testAst(
        \\/
        \\block comment
        \\\
    , &.{}, &.{});
    try testAst(
        \\ Starting comment
        \\ which should not change
        \\1
        \\\
        \\ trailing comment
        \\  which should not change
    , &.{.number_literal}, &.{.number_literal});
    try testAst(
        \\ Starting comment
        \\ which should not change
        \\
        \\1
        \\
        \\\
        \\ trailing comment
        \\  which should not change
    , &.{.number_literal}, &.{.number_literal});
    try testAstRender(
        \\ Starting comment
        \\ which should not change
        \\
        \\
        \\1
        \\
        \\
        \\\
        \\ trailing comment
        \\  which should not change
    ,
        \\ Starting comment
        \\ which should not change
        \\
        \\1
        \\
        \\\
        \\ trailing comment
        \\  which should not change
    , &.{.number_literal}, &.{.number_literal});

    try testAstRender(
        \\1
        \\/
        \\block comment 1
        \\\
        \\ 2
        \\
        \\/
        \\block comment 2
        \\\
        \\
        \\ 3
        \\
        \\
        \\/
        \\block comment 3
        \\\
    ,
        \\1
        \\/
        \\block comment 1
        \\\
        \\  2
        \\
        \\/
        \\block comment 2
        \\\
        \\
        \\  3
        \\
        \\/
        \\block comment 3
        \\\
    ,
        &.{ .number_literal, .number_literal, .number_literal },
        &.{.number_list_literal},
    );

    try testAstRender(
        \\ Starting comment
        \\ which should not change
        \\1 + 2     /line comment 1
        \\
        \\
        \\/
        \\block comment 1
        \\\
        \\
        \\
        \\1    /line comment 2
        \\
        \\/
        \\    block comment 2
        \\\
        \\
        \\ +  /line comment 3
        \\/
        \\        block comment 3
        \\\
        \\ 2      /line comment 4
        \\/
        \\            block comment 4
        \\\
    ,
        \\ Starting comment
        \\ which should not change
        \\1+2 /line comment 1
        \\
        \\/
        \\block comment 1
        \\\
        \\
        \\1 /line comment 2
        \\
        \\/
        \\    block comment 2
        \\\
        \\
        \\  + /line comment 3
        \\/
        \\        block comment 3
        \\\
        \\  2 /line comment 4
        \\/
        \\            block comment 4
        \\\
    ,
        &.{ .number_literal, .plus, .number_literal, .number_literal, .plus, .number_literal },
        &.{
            .number_literal, .plus, .number_literal, .apply_binary,
            .number_literal, .plus, .number_literal, .apply_binary,
        },
    );

    try testAstRender(
        \\ Starting comment
        \\ which should not change
        \\1 + 2;     /line comment 1
        \\
        \\/
        \\block comment 1
        \\\
        \\
        \\1    /line comment 2
        \\/
        \\    block comment 2
        \\\
        \\ +  /line comment 3
        \\/
        \\        block comment 3
        \\\
        \\ 2;      /line comment 4
    ,
        \\ Starting comment
        \\ which should not change
        \\1+2; /line comment 1
        \\
        \\/
        \\block comment 1
        \\\
        \\
        \\1 /line comment 2
        \\/
        \\    block comment 2
        \\\
        \\  + /line comment 3
        \\/
        \\        block comment 3
        \\\
        \\  2; /line comment 4
    ,
        &.{
            .number_literal, .plus, .number_literal, .semicolon, .number_literal, .plus, .number_literal, .semicolon,
        },
        &.{
            .number_literal, .plus, .number_literal, .apply_binary,
            .number_literal, .plus, .number_literal, .apply_binary,
        },
    );

    try testAstRender(
        \\ Starting comment
        \\ which should not change
        \\1 + 2 ;    /line comment 1
        \\\
        \\trailing comment
        \\ which should
        \\  not change
    ,
        \\ Starting comment
        \\ which should not change
        \\1+2; /line comment 1
        \\\
        \\trailing comment
        \\ which should
        \\  not change
    ,
        &.{ .number_literal, .plus, .number_literal, .semicolon },
        &.{ .number_literal, .plus, .number_literal, .apply_binary },
    );
}

test "number literals whitespace" {
    try testAst(
        "1(1;)1",
        &.{ .number_literal, .l_paren, .number_literal, .semicolon, .r_paren, .number_literal },
        &.{
            .number_literal, .list, .number_literal, .empty, .number_literal, .apply_unary, .apply_unary,
        },
    ); // l_paren/r_paren
    try testAst(
        "1{1}1",
        &.{ .number_literal, .l_brace, .number_literal, .r_brace, .number_literal },
        &.{
            .number_literal, .lambda, .number_literal, .number_literal, .apply_unary, .apply_unary,
        },
    ); // l_brace/r_brace
    try testAst(
        "[1;]1",
        &.{ .l_bracket, .number_literal, .semicolon, .r_bracket, .number_literal },
        &.{ .expr_block, .number_literal, .empty, .number_literal, .apply_unary },
    ); // r_bracket
    try testAst(
        "\"string\"1",
        &.{ .string_literal, .number_literal },
        &.{ .string_literal, .number_literal, .apply_unary },
    ); // string_literal
    try testAst(
        "`symbol 1",
        &.{ .symbol_literal, .number_literal },
        &.{ .symbol_literal, .number_literal, .apply_unary },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol 1",
        &.{ .symbol_literal, .symbol_literal, .number_literal },
        &.{ .symbol_list_literal, .number_literal, .apply_unary },
    ); // symbol_list_literal
    try testAst(
        "x 1",
        &.{ .identifier, .number_literal },
        &.{ .identifier, .number_literal, .apply_unary },
    ); // identifier
}

test "number list literals whitespace" {
    try testAst(
        "1 2 3(1 2 3;)1 2 3",
        &.{
            .number_literal, .number_literal, .number_literal, .l_paren,        .number_literal, .number_literal,
            .number_literal, .semicolon,      .r_paren,        .number_literal, .number_literal, .number_literal,
        },
        &.{
            .number_list_literal, .list, .number_list_literal, .empty, .number_list_literal, .apply_unary, .apply_unary,
        },
    ); // l_paren/r_paren
    try testAst(
        "1 2 3{1 2 3}1 2 3",
        &.{
            .number_literal, .number_literal, .number_literal, .l_brace,        .number_literal, .number_literal,
            .number_literal, .r_brace,        .number_literal, .number_literal, .number_literal,
        },
        &.{
            .number_list_literal, .lambda, .number_list_literal, .number_list_literal, .apply_unary, .apply_unary,
        },
    ); // l_brace/r_brace
    try testAst(
        "[1 2 3;]1 2 3",
        &.{
            .l_bracket, .number_literal, .number_literal, .number_literal, .semicolon,
            .r_bracket, .number_literal, .number_literal, .number_literal,
        },
        &.{ .expr_block, .number_list_literal, .empty, .number_list_literal, .apply_unary },
    ); // r_bracket
    try testAst(
        "\"string\"1 2 3",
        &.{ .string_literal, .number_literal, .number_literal, .number_literal },
        &.{ .string_literal, .number_list_literal, .apply_unary },
    ); // string_literal
    try testAst(
        "`symbol 1 2 3",
        &.{ .symbol_literal, .number_literal, .number_literal, .number_literal },
        &.{ .symbol_literal, .number_list_literal, .apply_unary },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol 1 2 3",
        &.{ .symbol_literal, .symbol_literal, .number_literal, .number_literal, .number_literal },
        &.{ .symbol_list_literal, .number_list_literal, .apply_unary },
    ); // symbol_list_literal
    try testAst(
        "x 1 2 3",
        &.{ .identifier, .number_literal, .number_literal, .number_literal },
        &.{ .identifier, .number_list_literal, .apply_unary },
    ); // identifier
}

test "string literals whitespace" {
    try testAst(
        "\"string\"(\"string\";)\"string\"",
        &.{ .string_literal, .l_paren, .string_literal, .semicolon, .r_paren, .string_literal },
        &.{
            .string_literal, .list, .string_literal, .empty, .string_literal, .apply_unary, .apply_unary,
        },
    ); // l_paren/r_paren
    try testAst(
        "\"string\"{\"string\"}\"string\"",
        &.{ .string_literal, .l_brace, .string_literal, .r_brace, .string_literal },
        &.{
            .string_literal, .lambda, .string_literal, .string_literal, .apply_unary, .apply_unary,
        },
    ); // l_brace/r_brace
    try testAst(
        "[\"string\";]\"string\"",
        &.{ .l_bracket, .string_literal, .semicolon, .r_bracket, .string_literal },
        &.{ .expr_block, .string_literal, .empty, .string_literal, .apply_unary },
    ); // r_bracket
    try testAst(
        "1\"string\"",
        &.{ .number_literal, .string_literal },
        &.{ .number_literal, .string_literal, .apply_unary },
    ); // number_literal
    try testAst(
        "\"string\"\"string\"",
        &.{ .string_literal, .string_literal },
        &.{ .string_literal, .string_literal, .apply_unary },
    ); // string_literal
    try testAst(
        "`symbol\"string\"",
        &.{ .symbol_literal, .string_literal },
        &.{ .symbol_literal, .string_literal, .apply_unary },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol\"string\"",
        &.{ .symbol_literal, .symbol_literal, .string_literal },
        &.{ .symbol_list_literal, .string_literal, .apply_unary },
    ); // symbol_list_literal
    try testAst(
        "x\"string\"",
        &.{ .identifier, .string_literal },
        &.{ .identifier, .string_literal, .apply_unary },
    ); // identifier
}

test "symbol literals whitespace" {
    try testAst(
        "`symbol(`symbol;)`symbol",
        &.{ .symbol_literal, .l_paren, .symbol_literal, .semicolon, .r_paren, .symbol_literal },
        &.{
            .symbol_literal, .list, .symbol_literal, .empty, .symbol_literal, .apply_unary, .apply_unary,
        },
    ); // l_paren/r_paren
    try testAst(
        "`symbol{`symbol}`symbol",
        &.{ .symbol_literal, .l_brace, .symbol_literal, .r_brace, .symbol_literal },
        &.{
            .symbol_literal, .lambda, .symbol_literal, .symbol_literal, .apply_unary, .apply_unary,
        },
    ); // r_paren/r_brace
    try testAst(
        "[`symbol;]`symbol",
        &.{ .l_bracket, .symbol_literal, .semicolon, .r_bracket, .symbol_literal },
        &.{ .expr_block, .symbol_literal, .empty, .symbol_literal, .apply_unary },
    ); // r_bracket
    try testAst(
        "1`symbol",
        &.{ .number_literal, .symbol_literal },
        &.{ .number_literal, .symbol_literal, .apply_unary },
    ); // number_literal
    try testAst(
        "\"string\"`symbol",
        &.{ .string_literal, .symbol_literal },
        &.{ .string_literal, .symbol_literal, .apply_unary },
    ); // string_literal
    try testAst(
        "`symbol `symbol",
        &.{ .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .symbol_literal, .apply_unary },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol `symbol",
        &.{ .symbol_literal, .symbol_literal, .symbol_literal },
        &.{ .symbol_list_literal, .symbol_literal, .apply_unary },
    ); // symbol_list_literal
    try testAst(
        "x`symbol",
        &.{ .identifier, .symbol_literal },
        &.{ .identifier, .symbol_literal, .apply_unary },
    ); // identifier

    try testAst(
        "`_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
    try testAst(
        "`_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
    try testAstMode(
        .k,
        "`a_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
    try testAstMode(
        .q,
        "`a_`",
        &.{ .symbol_literal, .symbol_literal },
        &.{.symbol_list_literal},
    );
    try testAstMode(
        .k,
        "`a_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
    try testAstMode(
        .q,
        "`a_`a",
        &.{ .symbol_literal, .symbol_literal },
        &.{.symbol_list_literal},
    );
    try testAstModeRender(
        .k,
        "`a_ `",
        "`a_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
    try testAstMode(
        .q,
        "`a_ `",
        &.{ .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .symbol_literal, .apply_unary },
    );
    try testAstModeRender(
        .k,
        "`a_ `a",
        "`a_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
    try testAstMode(
        .q,
        "`a_ `a",
        &.{ .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .symbol_literal, .apply_unary },
    );
    try testAstModeRender(
        .k,
        "`a _`",
        "`a_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
    try testAstMode(
        .q,
        "`a _`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
    try testAstModeRender(
        .k,
        "`a _`a",
        "`a_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
    try testAstMode(
        .q,
        "`a _`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .underscore, .symbol_literal, .apply_binary },
    );
}

test "symbol list literals whitespace" {
    try testAst(
        "`symbol`symbol(`symbol`symbol;)`symbol`symbol",
        &.{
            .symbol_literal, .symbol_literal, .l_paren,        .symbol_literal, .symbol_literal,
            .semicolon,      .r_paren,        .symbol_literal, .symbol_literal,
        },
        &.{
            .symbol_list_literal, .list, .symbol_list_literal, .empty, .symbol_list_literal, .apply_unary, .apply_unary,
        },
    ); // l_paren/r_paren
    try testAst(
        "`symbol`symbol{`symbol`symbol}`symbol`symbol",
        &.{
            .symbol_literal, .symbol_literal, .l_brace,        .symbol_literal,
            .symbol_literal, .r_brace,        .symbol_literal, .symbol_literal,
        },
        &.{
            .symbol_list_literal, .lambda, .symbol_list_literal, .symbol_list_literal, .apply_unary, .apply_unary,
        },
    ); // l_brace/r_brace
    try testAst(
        "[`symbol`symbol;]`symbol`symbol",
        &.{
            .l_bracket, .symbol_literal, .symbol_literal, .semicolon, .r_bracket, .symbol_literal, .symbol_literal,
        },
        &.{ .expr_block, .symbol_list_literal, .empty, .symbol_list_literal, .apply_unary },
    ); // r_bracket
    try testAst(
        "1`symbol`symbol",
        &.{ .number_literal, .symbol_literal, .symbol_literal },
        &.{ .number_literal, .symbol_list_literal, .apply_unary },
    ); // number_literal
    try testAst(
        "\"string\"`symbol`symbol",
        &.{ .string_literal, .symbol_literal, .symbol_literal },
        &.{ .string_literal, .symbol_list_literal, .apply_unary },
    ); // string_literal
    try testAst(
        "`symbol `symbol`symbol",
        &.{ .symbol_literal, .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .symbol_list_literal, .apply_unary },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol `symbol`symbol",
        &.{ .symbol_literal, .symbol_literal, .symbol_literal, .symbol_literal },
        &.{ .symbol_list_literal, .symbol_list_literal, .apply_unary },
    ); // symbol_list_literal
    try testAst(
        "x`symbol`symbol",
        &.{ .identifier, .symbol_literal, .symbol_literal },
        &.{ .identifier, .symbol_list_literal, .apply_unary },
    ); // identifier
}

test "identifiers whitespace" {
    try testAst(
        "x(x;)x",
        &.{ .identifier, .l_paren, .identifier, .semicolon, .r_paren, .identifier },
        &.{ .identifier, .list, .identifier, .empty, .identifier, .apply_unary, .apply_unary },
    ); // l_paren/r_paren
    try testAst(
        "x{x}x",
        &.{ .identifier, .l_brace, .identifier, .r_brace, .identifier },
        &.{ .identifier, .lambda, .identifier, .identifier, .apply_unary, .apply_unary },
    ); // l_brace/r_brace
    try testAst(
        "[x;]x",
        &.{ .l_bracket, .identifier, .semicolon, .r_bracket, .identifier },
        &.{ .expr_block, .identifier, .empty, .identifier, .apply_unary },
    ); // r_bracket
    try testAst(
        "1 x",
        &.{ .number_literal, .identifier },
        &.{ .number_literal, .identifier, .apply_unary },
    ); // number_literal
    try testAst(
        "\"string\"x",
        &.{ .string_literal, .identifier },
        &.{ .string_literal, .identifier, .apply_unary },
    ); // string_literal
    try testAst(
        "`symbol x",
        &.{ .symbol_literal, .identifier },
        &.{ .symbol_literal, .identifier, .apply_unary },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol x",
        &.{ .symbol_literal, .symbol_literal, .identifier },
        &.{ .symbol_list_literal, .identifier, .apply_unary },
    ); // symbol_list_literal
    try testAst(
        "x x",
        &.{ .identifier, .identifier },
        &.{ .identifier, .identifier, .apply_unary },
    ); // identifier
}

test "expression blocks" {
    try testAst("[]", &.{ .l_bracket, .r_bracket }, &.{.expr_block});
    try testAst(
        "[1]",
        &.{ .l_bracket, .number_literal, .r_bracket },
        &.{ .expr_block, .number_literal },
    );
    try testAst(
        "[;]",
        &.{ .l_bracket, .semicolon, .r_bracket },
        &.{ .expr_block, .empty, .empty },
    );
    try testAst(
        "[1;]",
        &.{ .l_bracket, .number_literal, .semicolon, .r_bracket },
        &.{ .expr_block, .number_literal, .empty },
    );
    try testAst(
        "[1;2]",
        &.{ .l_bracket, .number_literal, .semicolon, .number_literal, .r_bracket },
        &.{ .expr_block, .number_literal, .number_literal },
    );
    try testAst(
        "[;;]",
        &.{ .l_bracket, .semicolon, .semicolon, .r_bracket },
        &.{ .expr_block, .empty, .empty, .empty },
    );
    try testAst(
        "[1;;]",
        &.{ .l_bracket, .number_literal, .semicolon, .semicolon, .r_bracket },
        &.{ .expr_block, .number_literal, .empty, .empty },
    );
    try testAst(
        "[;2;]",
        &.{ .l_bracket, .semicolon, .number_literal, .semicolon, .r_bracket },
        &.{ .expr_block, .empty, .number_literal, .empty },
    );
    try testAst(
        "[;;3]",
        &.{ .l_bracket, .semicolon, .semicolon, .number_literal, .r_bracket },
        &.{ .expr_block, .empty, .empty, .number_literal },
    );
    try testAst(
        "[1;2;3]",
        &.{
            .l_bracket, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .r_bracket,
        },
        &.{ .expr_block, .number_literal, .number_literal, .number_literal },
    );
}

test "call" {
    try testAst(
        "{x}[]",
        &.{ .l_brace, .identifier, .r_brace, .l_bracket, .r_bracket },
        &.{ .lambda, .identifier, .expr_block, .apply_unary },
    );
    try testAst(
        "{x}[1]",
        &.{ .l_brace, .identifier, .r_brace, .l_bracket, .number_literal, .r_bracket },
        &.{ .lambda, .identifier, .expr_block, .number_literal, .apply_unary },
    );
    try testAst(
        "{x+y}[1;a]",
        &.{
            .l_brace,   .identifier,     .plus,      .identifier, .r_brace,
            .l_bracket, .number_literal, .semicolon, .identifier, .r_bracket,
        },
        &.{
            .lambda,     .identifier,     .plus,       .identifier,  .apply_binary,
            .expr_block, .number_literal, .identifier, .apply_unary,
        },
    );
    try testAst(
        "{1}[]-1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
        &.{
            .lambda, .number_literal, .expr_block, .apply_unary, .minus, .number_literal, .apply_binary,
        },
    );
}

test "mismatched parens/braces/brackets" {
    try failAst(")", &.{.r_paren}, &.{.expected_expr});
    try failAst("}", &.{.r_brace}, &.{.expected_expr});
    try failAst("]", &.{.r_bracket}, &.{.expected_expr});
    try failAst("(\n)", &.{ .l_paren, .r_paren }, &.{.expected_token});
    try failAst("{\n}", &.{ .l_brace, .r_brace }, &.{.expected_token});
    try failAst(
        "[\n]",
        &.{ .l_bracket, .r_bracket },
        &.{.expected_token},
    );
    try failAst(
        ")\n1",
        &.{ .r_paren, .number_literal },
        &.{.expected_expr},
    );
    try failAst(
        "}\n1",
        &.{ .r_brace, .number_literal },
        &.{.expected_expr},
    );
    try failAst(
        "]\n1",
        &.{ .r_bracket, .number_literal },
        &.{.expected_expr},
    );
    try failAst(
        "(\n)\n1",
        &.{ .l_paren, .r_paren, .number_literal },
        &.{.expected_token},
    );
    try failAst(
        "{\n}\n1",
        &.{ .l_brace, .r_brace, .number_literal },
        &.{.expected_token},
    );
    try failAst(
        "[\n]\n1",
        &.{ .l_bracket, .r_bracket, .number_literal },
        &.{.expected_token},
    );
}

test "render lists" {
    try testAstRender(
        "( )",
        "()",
        &.{ .l_paren, .r_paren },
        &.{.empty_list},
    );
    try testAstRender(
        \\(
        \\ )
    , "()", &.{ .l_paren, .r_paren }, &.{.empty_list});

    try testAstRender(
        \\( item1 ;  ; testing123 ;  )
    ,
        \\(item1;;testing123;)
    ,
        &.{ .l_paren, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_paren },
        &.{ .list, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\( item1 ; foo ; testing123 ; identifier )
    ,
        \\(item1;foo;testing123;identifier)
    ,
        &.{
            .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,
        },
        &.{ .list, .identifier, .identifier, .identifier, .identifier },
    );

    try testAstRender(
        \\(
        \\ item1 ;  ; testing123 ;  )
    ,
        \\(
        \\  item1;;testing123;)
    ,
        &.{ .l_paren, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_paren },
        &.{ .list, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\(
        \\ item1 ; foo ; testing123 ; identifier )
    ,
        \\(
        \\  item1;foo;testing123;identifier)
    ,
        &.{
            .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,
        },
        &.{ .list, .identifier, .identifier, .identifier, .identifier },
    );
    try testAstRender(
        \\(
        \\ item1 ;  ; testing123 ;
        \\ )
    ,
        \\(
        \\  item1;;testing123;)
    ,
        &.{ .l_paren, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_paren },
        &.{ .list, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\( item1 ; foo ; testing123 ; identifier
        \\ )
    ,
        \\(
        \\  item1;foo;testing123;identifier)
    ,
        &.{
            .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,
        },
        &.{ .list, .identifier, .identifier, .identifier, .identifier },
    );

    try testAstRender(
        \\( item1 ;  ; testing123
        \\ ; )
    ,
        \\(
        \\  item1;;testing123;)
    ,
        &.{ .l_paren, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_paren },
        &.{ .list, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\( item1 ; foo ; testing123 ;
        \\ identifier )
    ,
        \\(
        \\  item1     ;foo;testing123;
        \\  identifier)
    ,
        &.{
            .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,
        },
        &.{ .list, .identifier, .identifier, .identifier, .identifier },
    );
    try testAstRender(
        \\( item1 ;  ; testing123 ;
        \\ )
    ,
        \\(
        \\  item1;;testing123;)
    ,
        &.{ .l_paren, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_paren },
        &.{ .list, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\( item1 ; foo ; testing123 ;
        \\ identifier
        \\ )
    ,
        \\(
        \\  item1     ;foo;testing123;
        \\  identifier)
    ,
        &.{
            .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,
        },
        &.{ .list, .identifier, .identifier, .identifier, .identifier },
    );

    try testAstRender(
        \\( item1 ;  ;
        \\ testing123 ; )
    ,
        \\(
        \\  item1     ;;
        \\  testing123;)
    ,
        &.{ .l_paren, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_paren },
        &.{ .list, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\( item1 ; foo ;
        \\ testing123 ; identifier )
    ,
        \\(
        \\  item1     ;foo;
        \\  testing123;identifier)
    ,
        &.{
            .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,
        },
        &.{ .list, .identifier, .identifier, .identifier, .identifier },
    );
    try testAstRender(
        \\( item1 ;  ;
        \\ testing123 ;
        \\ )
    ,
        \\(
        \\  item1     ;;
        \\  testing123;)
    ,
        &.{ .l_paren, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_paren },
        &.{ .list, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\( item1 ; foo ;
        \\ testing123 ; identifier
        \\ )
    ,
        \\(
        \\  item1     ;foo;
        \\  testing123;identifier)
    ,
        &.{
            .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,
        },
        &.{ .list, .identifier, .identifier, .identifier, .identifier },
    );

    try testAstRender(
        \\( item1 ;
        \\  ; testing123 ; )
    ,
        \\(
        \\  item1;
        \\  ;
        \\  testing123;
        \\  )
    ,
        &.{ .l_paren, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_paren },
        &.{ .list, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\( item1 ;
        \\ foo ; testing123 ; identifier )
    ,
        \\(
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier)
    ,
        &.{
            .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,
        },
        &.{ .list, .identifier, .identifier, .identifier, .identifier },
    );
    try testAstRender(
        \\( item1 ;
        \\  ; testing123 ;
        \\ )
    ,
        \\(
        \\  item1;
        \\  ;
        \\  testing123;
        \\  )
    ,
        &.{ .l_paren, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_paren },
        &.{ .list, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\( item1 ;
        \\ foo ; testing123 ; identifier
        \\ )
    ,
        \\(
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier)
    ,
        &.{
            .l_paren, .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,
        },
        &.{ .list, .identifier, .identifier, .identifier, .identifier },
    );
}

test "render indentation" {
    try testAstRender(
        \\( ( item1 ; testing123 ; foo ; bar ; baz ; item10 ) ; ( item1 ; item2 ; item3 ) )
    ,
        \\((item1;testing123;foo;bar;baz;item10);(item1;item2;item3))
    , &.{
        .l_paren,    .l_paren,   .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon,
        .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,   .semicolon,  .l_paren,
        .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,   .r_paren,
    }, &.{
        .list,       .list,       .identifier, .identifier, .identifier, .identifier,
        .identifier, .identifier, .list,       .identifier, .identifier, .identifier,
    });

    try testAstRender(
        \\(
        \\ ( item1 ; testing123 ; foo ; bar ; baz ; item10 ) ; ( item1 ; item2 ; item3 ) )
    ,
        \\(
        \\  (item1;testing123;foo;bar;baz;item10);(item1;item2;item3))
    , &.{
        .l_paren,    .l_paren,   .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon,
        .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,   .semicolon,  .l_paren,
        .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,   .r_paren,
    }, &.{
        .list,       .list,       .identifier, .identifier, .identifier, .identifier,
        .identifier, .identifier, .list,       .identifier, .identifier, .identifier,
    });

    try testAstRender(
        \\(
        \\ ( item1 ; testing123 ; foo ; bar ; baz ; item10 ) ;
        \\ ( item1 ; item2 ; item3 ) )
    ,
        \\(
        \\  (item1;testing123;foo;bar;baz;item10);
        \\  (item1;item2;item3))
    , &.{
        .l_paren,    .l_paren,   .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon,
        .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,   .semicolon,  .l_paren,
        .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,   .r_paren,
    }, &.{
        .list,       .list,       .identifier, .identifier, .identifier, .identifier,
        .identifier, .identifier, .list,       .identifier, .identifier, .identifier,
    });

    try testAstRender(
        \\(
        \\ ( item1 ;
        \\ testing123 ; foo ; bar ; baz ; item10 ) ;
        \\ ( item1 ; item2 ; item3 ) )
    ,
        \\(
        \\  (
        \\    item1;
        \\    testing123;
        \\    foo;
        \\    bar;
        \\    baz;
        \\    item10);
        \\  (item1;item2;item3))
    , &.{
        .l_paren,    .l_paren,   .identifier, .semicolon, .identifier, .semicolon, .identifier, .semicolon,
        .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,   .semicolon,  .l_paren,
        .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_paren,   .r_paren,
    }, &.{
        .list,       .list,       .identifier, .identifier, .identifier, .identifier,
        .identifier, .identifier, .list,       .identifier, .identifier, .identifier,
    });
}

test "render expression blocks" {
    try testAstRender(
        "[ ]",
        "[]",
        &.{ .l_bracket, .r_bracket },
        &.{.expr_block},
    );
    try testAstRender(
        \\[
        \\ ]
    , "[]", &.{ .l_bracket, .r_bracket }, &.{.expr_block});

    try testAstRender(
        \\[ item1 ;  ; testing123 ;  ]
    ,
        \\[item1;;testing123;]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[ item1 ; foo ; testing123 ; identifier ]
    ,
        \\[item1;foo;testing123;identifier]
    ,
        &.{
            .l_bracket,  .identifier, .semicolon,  .identifier, .semicolon,
            .identifier, .semicolon,  .identifier, .r_bracket,
        },
        &.{ .expr_block, .identifier, .identifier, .identifier, .identifier },
    );

    try testAstRender(
        \\[
        \\ item1 ;  ; testing123 ;  ]
    ,
        \\[
        \\  item1;
        \\  ;
        \\  testing123;
        \\  ]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[
        \\ item1 ; foo ; testing123 ; identifier ]
    ,
        \\[
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier]
    ,
        &.{
            .l_bracket,  .identifier, .semicolon,  .identifier, .semicolon,
            .identifier, .semicolon,  .identifier, .r_bracket,
        },
        &.{ .expr_block, .identifier, .identifier, .identifier, .identifier },
    );
    try testAstRender(
        \\[
        \\ item1 ;  ; testing123 ;
        \\ ]
    ,
        \\[
        \\  item1;
        \\  ;
        \\  testing123;
        \\  ]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[ item1 ; foo ; testing123 ; identifier
        \\ ]
    ,
        \\[
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier]
    ,
        &.{
            .l_bracket,  .identifier, .semicolon,  .identifier, .semicolon,
            .identifier, .semicolon,  .identifier, .r_bracket,
        },
        &.{ .expr_block, .identifier, .identifier, .identifier, .identifier },
    );

    try testAstRender(
        \\[ item1 ;  ; testing123
        \\ ; ]
    ,
        \\[
        \\  item1;
        \\  ;
        \\  testing123;
        \\  ]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[ item1 ; foo ; testing123 ;
        \\ identifier ]
    ,
        \\[
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier]
    ,
        &.{
            .l_bracket,  .identifier, .semicolon,  .identifier, .semicolon,
            .identifier, .semicolon,  .identifier, .r_bracket,
        },
        &.{ .expr_block, .identifier, .identifier, .identifier, .identifier },
    );
    try testAstRender(
        \\[ item1 ;  ; testing123 ;
        \\ ]
    ,
        \\[
        \\  item1;
        \\  ;
        \\  testing123;
        \\  ]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[ item1 ; foo ; testing123 ;
        \\ identifier
        \\ ]
    ,
        \\[
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier]
    ,
        &.{
            .l_bracket,  .identifier, .semicolon,  .identifier, .semicolon,
            .identifier, .semicolon,  .identifier, .r_bracket,
        },
        &.{ .expr_block, .identifier, .identifier, .identifier, .identifier },
    );

    try testAstRender(
        \\[ item1 ;  ;
        \\ testing123 ; ]
    ,
        \\[
        \\  item1;
        \\  ;
        \\  testing123;
        \\  ]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[ item1 ; foo ;
        \\ testing123 ; identifier ]
    ,
        \\[
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier]
    ,
        &.{
            .l_bracket,  .identifier, .semicolon,  .identifier, .semicolon,
            .identifier, .semicolon,  .identifier, .r_bracket,
        },
        &.{ .expr_block, .identifier, .identifier, .identifier, .identifier },
    );
    try testAstRender(
        \\[ item1 ;  ;
        \\ testing123 ;
        \\ ]
    ,
        \\[
        \\  item1;
        \\  ;
        \\  testing123;
        \\  ]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[ item1 ; foo ;
        \\ testing123 ; identifier
        \\ ]
    ,
        \\[
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier]
    ,
        &.{
            .l_bracket,  .identifier, .semicolon,  .identifier, .semicolon,
            .identifier, .semicolon,  .identifier, .r_bracket,
        },
        &.{ .expr_block, .identifier, .identifier, .identifier, .identifier },
    );

    try testAstRender(
        \\[ item1 ;
        \\  ; testing123 ; ]
    ,
        \\[
        \\  item1;
        \\  ;
        \\  testing123;
        \\  ]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[ item1 ;
        \\ foo ; testing123 ; identifier ]
    ,
        \\[
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier]
    ,
        &.{
            .l_bracket,  .identifier, .semicolon,  .identifier, .semicolon,
            .identifier, .semicolon,  .identifier, .r_bracket,
        },
        &.{ .expr_block, .identifier, .identifier, .identifier, .identifier },
    );
    try testAstRender(
        \\[ item1 ;
        \\  ; testing123 ;
        \\ ]
    ,
        \\[
        \\  item1;
        \\  ;
        \\  testing123;
        \\  ]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[ item1 ;
        \\ foo ; testing123 ; identifier
        \\ ]
    ,
        \\[
        \\  item1;
        \\  foo;
        \\  testing123;
        \\  identifier]
    ,
        &.{
            .l_bracket,  .identifier, .semicolon,  .identifier, .semicolon,
            .identifier, .semicolon,  .identifier, .r_bracket,
        },
        &.{ .expr_block, .identifier, .identifier, .identifier, .identifier },
    );
}

test "select" {
    try testAst(
        "select from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier },
    );
    try testAst(
        "select a from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select a,b from x",
        &.{ .keyword_select, .identifier, .comma, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select by from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier },
        &.{ .select, .identifier },
    );
    try testAst(
        "select a by from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select a,b by from x",
        &.{ .keyword_select, .identifier, .comma, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select by c from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select by c,d from x",
        &.{ .keyword_select, .identifier, .identifier, .comma, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a by c from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a,b by c,d from x",
        &.{
            .keyword_select, .identifier, .comma,      .identifier, .identifier,
            .identifier,     .comma,      .identifier, .identifier, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select from x where e",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select from x where e,f",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier, .comma, .identifier },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a from x where e",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a,b from x where e,f",
        &.{
            .keyword_select, .identifier, .comma,      .identifier, .identifier,
            .identifier,     .identifier, .identifier, .comma,      .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select by from x where e,f",
        &.{
            .keyword_select, .identifier, .identifier, .identifier, .identifier, .identifier, .comma, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a by from x where e",
        &.{
            .keyword_select, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a,b by from x where e,f",
        &.{
            .keyword_select, .identifier, .comma,      .identifier, .identifier, .identifier,
            .identifier,     .identifier, .identifier, .comma,      .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select by c from x where e",
        &.{
            .keyword_select, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select by c,d from x where e,f",
        &.{
            .keyword_select, .identifier, .identifier, .comma, .identifier, .identifier,
            .identifier,     .identifier, .identifier, .comma, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a by c from x where e",
        &.{
            .keyword_select, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a,b by c,d from x where e,f",
        &.{
            .keyword_select, .identifier, .comma,      .identifier, .identifier, .identifier, .comma,
            .identifier,     .identifier, .identifier, .identifier, .identifier, .comma,      .identifier,
        },
        &.{
            .select, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
    );

    try testAst(
        "select`a from x",
        &.{ .keyword_select, .symbol_literal, .identifier, .identifier },
        &.{ .select, .symbol_literal, .identifier },
    );
    try testAst(
        "select`a`b from x",
        &.{ .keyword_select, .symbol_literal, .symbol_literal, .identifier, .identifier },
        &.{ .select, .symbol_list_literal, .identifier },
    );
    try testAst(
        "select`a,`c from x",
        &.{ .keyword_select, .symbol_literal, .comma, .symbol_literal, .identifier, .identifier },
        &.{ .select, .symbol_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "select`a,`c`d from x",
        &.{
            .keyword_select, .symbol_literal, .comma, .symbol_literal, .symbol_literal, .identifier, .identifier,
        },
        &.{ .select, .symbol_literal, .symbol_list_literal, .identifier },
    );
    try testAst(
        "select`a`b,`c from x",
        &.{
            .keyword_select, .symbol_literal, .symbol_literal, .comma, .symbol_literal, .identifier, .identifier,
        },
        &.{ .select, .symbol_list_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "select`a`b,`c`d from x",
        &.{
            .keyword_select, .symbol_literal, .symbol_literal, .comma,
            .symbol_literal, .symbol_literal, .identifier,     .identifier,
        },
        &.{ .select, .symbol_list_literal, .symbol_list_literal, .identifier },
    );
    try testAst(
        "select by`a from x",
        &.{ .keyword_select, .identifier, .symbol_literal, .identifier, .identifier },
        &.{ .select, .symbol_literal, .identifier },
    );
    try testAst(
        "select by`a`b from x",
        &.{ .keyword_select, .identifier, .symbol_literal, .symbol_literal, .identifier, .identifier },
        &.{ .select, .symbol_list_literal, .identifier },
    );
    try testAst(
        "select by`a,`c from x",
        &.{
            .keyword_select, .identifier, .symbol_literal, .comma, .symbol_literal, .identifier, .identifier,
        },
        &.{ .select, .symbol_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "select by`a,`c`d from x",
        &.{
            .keyword_select, .identifier,     .symbol_literal, .comma,
            .symbol_literal, .symbol_literal, .identifier,     .identifier,
        },
        &.{ .select, .symbol_literal, .symbol_list_literal, .identifier },
    );
    try testAst(
        "select by`a`b,`c from x",
        &.{
            .keyword_select, .identifier,     .symbol_literal, .symbol_literal,
            .comma,          .symbol_literal, .identifier,     .identifier,
        },
        &.{ .select, .symbol_list_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "select by`a`b,`c`d from x",
        &.{
            .keyword_select, .identifier,     .symbol_literal, .symbol_literal, .comma,
            .symbol_literal, .symbol_literal, .identifier,     .identifier,
        },
        &.{ .select, .symbol_list_literal, .symbol_list_literal, .identifier },
    );

    try testAst(
        "select distinct from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier },
        &.{ .select, .identifier },
    );
    try testAst(
        "select distinct a from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select distinct by from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier },
    );
    try testAst(
        "select distinct a by from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select distinct by b from x",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select distinct a by b from x",
        &.{
            .keyword_select, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier },
    );

    if (true) return error.SkipZigTest;

    try testAst(
        "select[1]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .number_literal },
    );
    try testAst(
        "select[a]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );

    try testAst(
        "select[1;<a]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .number_literal, .identifier },
    );
    try testAst(
        "select[a;<b]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .identifier, .identifier },
    );

    try testAst(
        "select[<a]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select[<:a]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select[<=a]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select[<>a]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select[>a]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select[>:a]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select[>=a]from x",
        &.{ .keyword_select, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
}
