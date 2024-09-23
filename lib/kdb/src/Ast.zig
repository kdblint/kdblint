const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const kdb = @import("root.zig");
const Token = kdb.Token;
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
    defer parser.ends_expr.deinit(gpa);

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
        comptime assert(field.type == Node.Index);
        @field(result, field.name) = tree.extra_data[index + i];
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

        .grouped_expression,
        .empty_list,
        .list,
        => return main_tokens[n] - end_offset,

        .lambda_params,
        .lambda,
        => return main_tokens[n] - end_offset,

        .lambda_body,
        .lambda_body_semicolon,
        => n = datas[n].lhs,

        .expr_block,
        => return main_tokens[n] - end_offset,

        .assign,
        => n = datas[n].lhs,

        .add,
        .sub,
        .mul,
        .div,
        .@"and",
        .@"or",
        .fill,
        .equal,
        .less_than,
        .less_or_equal,
        .not_equal,
        .greater_than,
        .greater_or_equal,
        .cast,
        .join,
        .take,
        .drop,
        .match,
        .dict,
        .find,
        .apply_at,
        .apply,
        .file_text,
        .file_binary,
        .dynamic_load,
        => return main_tokens[n] - end_offset,

        .each,
        .each_prior,
        .over,
        .each_right,
        .scan,
        .each_left,
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
    };
}

pub fn lastToken(tree: Ast, node: Node.Index) Token.Index {
    const tags: []Node.Tag = tree.nodes.items(.tag);
    const datas: []Node.Data = tree.nodes.items(.data);
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    var end_offset: Token.Index = 0;
    var n = node;
    while (true) switch (tags[n]) {
        .root,
        => return @intCast(tree.tokens.len - 1),

        .grouped_expression,
        .empty_list,
        .list,
        => return datas[n].rhs + end_offset,

        .lambda_params,
        => {
            end_offset += 1; // rbracket
            const params = tree.extra_data[datas[n].lhs..datas[n].rhs];
            if (params.len > 0) {
                n = params[params.len - 1];
            } else {
                return main_tokens[n] + end_offset;
            }
        },

        .lambda_body,
        .lambda_body_semicolon,
        => return main_tokens[n] + end_offset,

        .lambda,
        => n = datas[n].rhs,

        .expr_block,
        => return datas[n].rhs + end_offset,

        .assign,
        => n = datas[n].rhs,

        .add,
        .sub,
        .mul,
        .div,
        .@"and",
        .@"or",
        .fill,
        .equal,
        .less_than,
        .less_or_equal,
        .not_equal,
        .greater_than,
        .greater_or_equal,
        .cast,
        .join,
        .take,
        .drop,
        .match,
        .dict,
        .find,
        .apply_at,
        .apply,
        .file_text,
        .file_binary,
        .dynamic_load,
        => return main_tokens[n] + end_offset,

        .each,
        .each_prior,
        .over,
        .each_right,
        .scan,
        .each_left,
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
    };
}

pub fn tokensOnSameLine(tree: Ast, token1: Token.Index, token2: Token.Index) bool {
    const token_locs = tree.tokens.items(.loc);
    const source = tree.source[token_locs[token1].start..token_locs[token2].start];
    return mem.indexOfScalar(u8, source, '\n') == null;
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
    } = .{ .none = {} },

    pub const Tag = enum {
        expected_expr,
        expected_prefix_expr,
        cannot_project_operator_without_lhs,
        cannot_apply_operator_directly,
        cannot_apply_iterator_directly,

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
        /// sub_list[lhs...rhs]
        root,

        /// `(lhs)`. main_token is the `(`. rhs is the token index of the `)`.
        grouped_expression,
        /// `()`. lhs unused. main_token is the `(`. rhs is the token index of the `)`.
        empty_list,
        /// `(lhs)`. main_token is the `(`. rhs is the token index of the `)`. `SubRange[lhs]`.
        list,

        /// `{[]...}`. `sub_list[lhs..rhs]`.
        /// main_token is the `[`.
        lambda_params,
        /// `{[]...}`. `sub_list[lhs..rhs]`.
        /// main_token is the `}`.
        lambda_body,
        /// Same as lambda_body but there is known to be a semicolon before the `}`.
        lambda_body_semicolon,
        /// `{[lhs]rhs}`. lhs can be omitted. lhs is the lambda_params. rhs is the lambda_body.
        /// main_token is the `{`.
        lambda,

        /// `[lhs]`. main_token is the `[`. lhs can be omitted. rhs is the token index of the `]`. `SubRange[lhs]`.
        expr_block,

        /// `lhs : rhs`. main_token is the `:`.
        assign,

        /// Both lhs and rhs unused. main_token is the `+`.
        add,
        /// Both lhs and rhs unused. main_token is the `-`.
        sub,
        /// Both lhs and rhs unused. main_token is the `*`.
        mul,
        /// Both lhs and rhs unused. main_token is the `%`.
        div,
        /// Both lhs and rhs unused. main_token is the `&`.
        @"and",
        /// Both lhs and rhs unused. main_token is the `|`.
        @"or",
        /// Both lhs and rhs unused. main_token is the `^`.
        fill,
        /// Both lhs and rhs unused. main_token is the `=`.
        equal,
        /// Both lhs and rhs unused. main_token is the `<`.
        less_than,
        /// Both lhs and rhs unused. main_token is the `<=`.
        less_or_equal,
        /// Both lhs and rhs unused. main_token is the `<>`.
        not_equal,
        /// Both lhs and rhs unused. main_token is the `>`.
        greater_than,
        /// Both lhs and rhs unused. main_token is the `>=`.
        greater_or_equal,
        /// Both lhs and rhs unused. main_token is the `$`.
        cast,
        /// Both lhs and rhs unused. main_token is the `,`.
        join,
        /// Both lhs and rhs unused. main_token is the `#`.
        take,
        /// Both lhs and rhs unused. main_token is the `_`.
        drop,
        /// Both lhs and rhs unused. main_token is the `~`.
        match,
        /// Both lhs and rhs unused. main_token is the `!`.
        dict,
        /// Both lhs and rhs unused. main_token is the `?`.
        find,
        /// Both lhs and rhs unused. main_token is the `@`.
        apply_at,
        /// Both lhs and rhs unused. main_token is the `.`.
        apply,
        /// Both lhs and rhs unused. main_token is the `0:`.
        file_text,
        /// Both lhs and rhs unused. main_token is the `1:`.
        file_binary,
        /// Both lhs and rhs unused. main_token is the `2:`.
        dynamic_load,

        /// `lhs'`. lhs can be omitted. rhs unused. main_token is the `'`.
        each,
        /// `lhs':`. lhs can be omitted. rhs unused. main_token is the `':`.
        each_prior,
        /// `lhs/`. lhs can be omitted. rhs unused. main_token is the `/`.
        over,
        /// `lhs/:`. lhs can be omitted. rhs unused. main_token is the `/:`.
        each_right,
        /// `lhs\`. lhs can be omitted. rhs unused. main_token is the `\`.
        scan,
        /// `lhs\:`. lhs can be omitted. rhs unused. main_token is the `\:`.
        each_left,

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

        pub fn getType(tag: Tag) Type {
            return switch (tag) {
                .root,
                => unreachable,

                .grouped_expression,
                .empty_list,
                .list,
                => .other,

                .lambda_params,
                .lambda_body,
                .lambda_body_semicolon,
                .lambda,
                => .other,

                .expr_block,
                => .other,

                .assign,
                => @panic("NYI"),

                .add,
                .sub,
                .mul,
                .div,
                .@"and",
                .@"or",
                .fill,
                .equal,
                .less_than,
                .less_or_equal,
                .not_equal,
                .greater_than,
                .greater_or_equal,
                .cast,
                .join,
                .take,
                .drop,
                .match,
                .dict,
                .find,
                .apply_at,
                .apply,
                .file_text,
                .file_binary,
                .dynamic_load,
                => .binary_operator,

                .each,
                .each_prior,
                .over,
                .each_right,
                .scan,
                .each_left,
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
            };
        }
    };

    pub const Data = struct {
        lhs: Index,
        rhs: Index,
    };

    pub const SubRange = struct {
        /// Index into sub_list.
        start: Index,
        /// Index into sub_list.
        end: Index,
    };
};

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

    // Errors
    try std.testing.expect(tree.errors.len == 0);

    // Token tags
    const actual_token_tags: []Token.Tag = tree.tokens.items(.tag);
    try std.testing.expectEqualSlices(Token.Tag, expected_tokens, actual_token_tags[0 .. actual_token_tags.len - 1]);
    try std.testing.expectEqual(.eof, actual_token_tags[actual_token_tags.len - 1]);

    // Node tags
    const actual_nodes: []Node.Tag = tree.nodes.items(.tag);
    try std.testing.expectEqualSlices(Node.Tag, expected_nodes, actual_nodes[1..]);
    try std.testing.expectEqual(.root, actual_nodes[0]);

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

test "lambda renders on same line" {
    try testAst(
        "{}",
        &.{ .l_brace, .r_brace },
        &.{ .lambda, .lambda_body },
    );
    try testAst(
        "{1}",
        &.{ .l_brace, .number_literal, .r_brace },
        &.{ .lambda, .lambda_body, .number_literal },
    );
    try testAst(
        "{1;}",
        &.{ .l_brace, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .lambda_body_semicolon, .number_literal },
    );
    try testAst(
        "{1;2}",
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .r_brace },
        &.{ .lambda, .lambda_body, .number_literal, .number_literal },
    );
    try testAst(
        "{1;2;}",
        &.{
            .l_brace,        .number_literal, .semicolon,
            .number_literal, .semicolon,      .r_brace,
        },
        &.{ .lambda, .lambda_body_semicolon, .number_literal, .number_literal },
    );
    try testAst(
        "{[]}",
        &.{ .l_brace, .l_bracket, .r_bracket, .r_brace },
        &.{ .lambda, .lambda_params, .lambda_body },
    );
    try testAst(
        "{[]1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .lambda_params, .lambda_body, .number_literal },
    );
    try testAst(
        "{[]1;}",
        &.{
            .l_brace,        .l_bracket, .r_bracket,
            .number_literal, .semicolon, .r_brace,
        },
        &.{ .lambda, .lambda_params, .lambda_body_semicolon, .number_literal },
    );
    try testAst(
        "{[]1;2}",
        &.{
            .l_brace,   .l_bracket,      .r_bracket, .number_literal,
            .semicolon, .number_literal, .r_brace,
        },
        &.{ .lambda, .lambda_params, .lambda_body, .number_literal, .number_literal },
    );
    try testAst(
        "{[]1;2;}",
        &.{
            .l_brace,   .l_bracket,      .r_bracket, .number_literal,
            .semicolon, .number_literal, .semicolon, .r_brace,
        },
        &.{
            .lambda,         .lambda_params,  .lambda_body_semicolon,
            .number_literal, .number_literal,
        },
    );
    try testAst(
        "{[x]}",
        &.{ .l_brace, .l_bracket, .identifier, .r_bracket, .r_brace },
        &.{ .lambda, .lambda_params, .identifier, .lambda_body },
    );
    try testAst(
        "{[x]1}",
        &.{ .l_brace, .l_bracket, .identifier, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .lambda_params, .identifier, .lambda_body, .number_literal },
    );
    try testAst(
        "{[x]1;}",
        &.{
            .l_brace,        .l_bracket, .identifier, .r_bracket,
            .number_literal, .semicolon, .r_brace,
        },
        &.{ .lambda, .lambda_params, .identifier, .lambda_body_semicolon, .number_literal },
    );
    try testAst(
        "{[x]1;2}",
        &.{
            .l_brace,        .l_bracket, .identifier,     .r_bracket,
            .number_literal, .semicolon, .number_literal, .r_brace,
        },
        &.{
            .lambda,      .lambda_params,  .identifier,
            .lambda_body, .number_literal, .number_literal,
        },
    );
    try testAst(
        "{[x]1;2;}",
        &.{
            .l_brace,   .l_bracket,      .identifier, .r_bracket, .number_literal,
            .semicolon, .number_literal, .semicolon,  .r_brace,
        },
        &.{
            .lambda,                .lambda_params,  .identifier,
            .lambda_body_semicolon, .number_literal, .number_literal,
        },
    );
    try testAst(
        "{[x;y]}",
        &.{
            .l_brace,    .l_bracket, .identifier, .semicolon,
            .identifier, .r_bracket, .r_brace,
        },
        &.{ .lambda, .lambda_params, .identifier, .identifier, .lambda_body },
    );
    try testAst(
        "{[x;y]1}",
        &.{
            .l_brace,    .l_bracket, .identifier,     .semicolon,
            .identifier, .r_bracket, .number_literal, .r_brace,
        },
        &.{
            .lambda,     .lambda_params, .identifier,
            .identifier, .lambda_body,   .number_literal,
        },
    );
    try testAst(
        "{[x;y]1;}",
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon, .identifier,
            .r_bracket, .number_literal, .semicolon,  .r_brace,
        },
        &.{
            .lambda,     .lambda_params,         .identifier,
            .identifier, .lambda_body_semicolon, .number_literal,
        },
    );
    try testAst(
        "{[x;y]1;2}",
        &.{
            .l_brace,   .l_bracket,      .identifier, .semicolon,      .identifier,
            .r_bracket, .number_literal, .semicolon,  .number_literal, .r_brace,
        },
        &.{
            .lambda,      .lambda_params,  .identifier,     .identifier,
            .lambda_body, .number_literal, .number_literal,
        },
    );
    try testAst(
        "{[x;y]1;2;}",
        &.{
            .l_brace,        .l_bracket, .identifier,     .semicolon, .identifier, .r_bracket,
            .number_literal, .semicolon, .number_literal, .semicolon, .r_brace,
        },
        &.{
            .lambda,                .lambda_params,  .identifier,     .identifier,
            .lambda_body_semicolon, .number_literal, .number_literal,
        },
    );
}

test "lambda renders on multiple lines" {
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
            .lambda,      .lambda_params,  .identifier,     .identifier,     .identifier,
            .lambda_body, .number_literal, .number_literal, .number_literal,
        },
    );
    try testAst(
        \\{[x;
        \\  y;
        \\  z]
        \\  1;2;3;}
    ,
        &.{
            .l_brace,        .l_bracket, .identifier,     .semicolon, .identifier,     .semicolon, .identifier, .r_bracket,
            .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .semicolon, .r_brace,
        },
        &.{
            .lambda,                .lambda_params,  .identifier,     .identifier,     .identifier,
            .lambda_body_semicolon, .number_literal, .number_literal, .number_literal,
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
            .lambda,      .lambda_params,  .identifier,     .identifier,     .identifier,
            .lambda_body, .number_literal, .number_literal, .number_literal,
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
            .l_brace,        .l_bracket, .identifier,     .semicolon, .identifier,     .semicolon, .identifier, .r_bracket,
            .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .semicolon, .r_brace,
        },
        &.{
            .lambda,                .lambda_params,  .identifier,     .identifier,     .identifier,
            .lambda_body_semicolon, .number_literal, .number_literal, .number_literal,
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
            .lambda,      .lambda_params,  .identifier,     .identifier,     .identifier,
            .lambda_body, .number_literal, .number_literal, .number_literal,
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
            .l_brace,        .l_bracket, .identifier,     .semicolon, .identifier,     .semicolon, .identifier, .r_bracket,
            .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .semicolon, .r_brace,
        },
        &.{
            .lambda,                .lambda_params,  .identifier,     .identifier,     .identifier,
            .lambda_body_semicolon, .number_literal, .number_literal, .number_literal,
        },
    );
}

test "precedence" {
    try testAst(
        "2*3+4",
        &.{ .number_literal, .asterisk, .number_literal, .plus, .number_literal },
        &.{ .number_literal, .mul, .number_literal, .add, .number_literal, .apply_binary, .apply_binary },
    );
    try testAst(
        "(2*3)+4",
        &.{ .l_paren, .number_literal, .asterisk, .number_literal, .r_paren, .plus, .number_literal },
        &.{
            .grouped_expression,
            .number_literal,
            .mul,
            .number_literal,
            .apply_binary,
            .add,
            .number_literal,
            .apply_binary,
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
            .number_literal,
            .number_literal,
            .number_literal,
            .plus,
            .number_literal,
            .number_literal,
            .number_literal,
            .plus,
            .number_literal,
            .number_literal,
            .number_literal,
        },
        &.{
            .number_list_literal,
            .add,
            .number_list_literal,
            .add,
            .number_list_literal,
            .apply_binary,
            .apply_binary,
        },
    );
}

test "symbol literals" {
    try testAst("`", &.{.symbol_literal}, &.{.symbol_literal});
    try testAst("`symbol", &.{.symbol_literal}, &.{.symbol_literal});
    try testAst("`symbol`symbol", &.{ .symbol_literal, .symbol_literal }, &.{.symbol_list_literal});
}

test "operators" {
    try testAst("+", &.{.plus}, &.{.add});
    try testAst(
        "1+",
        &.{ .number_literal, .plus },
        &.{ .number_literal, .add, .apply_binary },
    );
    try failAst(
        "+1",
        &.{ .plus, .number_literal },
        &.{.cannot_project_operator_without_lhs},
    );
    try testAst(
        "1+2",
        &.{ .number_literal, .plus, .number_literal },
        &.{ .number_literal, .add, .number_literal, .apply_binary },
    );
    try testAst("(+)", &.{ .l_paren, .plus, .r_paren }, &.{ .grouped_expression, .add });
    try testAst(
        "(1+)",
        &.{ .l_paren, .number_literal, .plus, .r_paren },
        &.{ .grouped_expression, .number_literal, .add, .apply_binary },
    );
    try failAst(
        "(+1)",
        &.{ .l_paren, .plus, .number_literal, .r_paren },
        &.{.cannot_project_operator_without_lhs},
    );

    try testAst("(+)1", &.{
        .l_paren,
        .plus,
        .r_paren,
        .number_literal,
    }, &.{
        .grouped_expression,
        .add,
        .number_literal,
        .apply_unary,
    });
}

test "iterators" {
    try testAst("(/)", &.{ .l_paren, .slash, .r_paren }, &.{ .grouped_expression, .over });
    try testAst("+/", &.{ .plus, .slash }, &.{ .add, .over });
    try testAst(
        "1+/",
        &.{ .number_literal, .plus, .slash },
        &.{ .number_literal, .add, .over, .apply_binary },
    );
    try testAstMode(
        .k,
        "+/1",
        &.{ .plus, .slash, .number_literal },
        &.{ .add, .over, .number_literal, .apply_unary },
    );
    try failAstMode(
        .q,
        "+/1",
        &.{ .plus, .slash, .number_literal },
        &.{.cannot_apply_iterator_directly},
    );
    try testAst(
        "1+/1",
        &.{ .number_literal, .plus, .slash, .number_literal },
        &.{ .number_literal, .add, .over, .number_literal, .apply_binary },
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
        "(;1)",
        &.{ .l_paren, .semicolon, .number_literal, .r_paren },
        &.{ .list, .number_literal },
    );
    try testAst(
        "(1;)",
        &.{ .l_paren, .number_literal, .semicolon, .r_paren },
        &.{ .list, .number_literal },
    );
    try testAst(
        "(;1)",
        &.{ .l_paren, .semicolon, .number_literal, .r_paren },
        &.{ .list, .number_literal },
    );
    try testAst(
        "(1;2)",
        &.{ .l_paren, .number_literal, .semicolon, .number_literal, .r_paren },
        &.{ .list, .number_literal, .number_literal },
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
        &.{ .number_literal, .semicolon, .semicolon, .semicolon, .semicolon, .semicolon, .number_literal },
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
        &.{ .number_literal, .add, .number_literal, .apply_binary, .number_literal, .add, .number_literal, .apply_binary },
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
        &.{ .number_literal, .plus, .number_literal, .semicolon, .number_literal, .plus, .number_literal, .semicolon },
        &.{ .number_literal, .add, .number_literal, .apply_binary, .number_literal, .add, .number_literal, .apply_binary },
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
        &.{ .number_literal, .add, .number_literal, .apply_binary },
    );
}

test "number literals whitespace" {
    try testAst(
        "1(1;)1",
        &.{ .number_literal, .l_paren, .number_literal, .semicolon, .r_paren, .number_literal },
        &.{ .number_literal, .list, .number_literal, .number_literal, .apply_unary, .apply_unary },
    ); // l_paren/r_paren
    try testAst(
        "1{1}1",
        &.{ .number_literal, .l_brace, .number_literal, .r_brace, .number_literal },
        &.{ .number_literal, .lambda, .lambda_body, .number_literal, .number_literal, .apply_unary, .apply_unary },
    ); // l_brace/r_brace
    try testAst(
        "[1;]1",
        &.{ .l_bracket, .number_literal, .semicolon, .r_bracket, .number_literal },
        &.{ .expr_block, .number_literal, .number_literal, .apply_unary },
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
        &.{ .number_list_literal, .list, .number_list_literal, .number_list_literal, .apply_unary, .apply_unary },
    ); // l_paren/r_paren
    try testAst(
        "1 2 3{1 2 3}1 2 3",
        &.{
            .number_literal, .number_literal, .number_literal, .l_brace,        .number_literal, .number_literal,
            .number_literal, .r_brace,        .number_literal, .number_literal, .number_literal,
        },
        &.{ .number_list_literal, .lambda, .lambda_body, .number_list_literal, .number_list_literal, .apply_unary, .apply_unary },
    ); // l_brace/r_brace
    try testAst(
        "[1 2 3;]1 2 3",
        &.{ .l_bracket, .number_literal, .number_literal, .number_literal, .semicolon, .r_bracket, .number_literal, .number_literal, .number_literal },
        &.{ .expr_block, .number_list_literal, .number_list_literal, .apply_unary },
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
        &.{ .string_literal, .list, .string_literal, .string_literal, .apply_unary, .apply_unary },
    ); // l_paren/r_paren
    try testAst(
        "\"string\"{\"string\"}\"string\"",
        &.{ .string_literal, .l_brace, .string_literal, .r_brace, .string_literal },
        &.{ .string_literal, .lambda, .lambda_body, .string_literal, .string_literal, .apply_unary, .apply_unary },
    ); // l_brace/r_brace
    try testAst(
        "[\"string\";]\"string\"",
        &.{ .l_bracket, .string_literal, .semicolon, .r_bracket, .string_literal },
        &.{ .expr_block, .string_literal, .string_literal, .apply_unary },
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
        &.{ .symbol_literal, .list, .symbol_literal, .symbol_literal, .apply_unary, .apply_unary },
    ); // l_paren/r_paren
    try testAst(
        "`symbol{`symbol}`symbol",
        &.{ .symbol_literal, .l_brace, .symbol_literal, .r_brace, .symbol_literal },
        &.{ .symbol_literal, .lambda, .lambda_body, .symbol_literal, .symbol_literal, .apply_unary, .apply_unary },
    ); // r_paren/r_brace
    try testAst(
        "[`symbol;]`symbol",
        &.{ .l_bracket, .symbol_literal, .semicolon, .r_bracket, .symbol_literal },
        &.{ .expr_block, .symbol_literal, .symbol_literal, .apply_unary },
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

    try testAst("`_`", &.{ .symbol_literal, .underscore, .symbol_literal }, &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary });
    try testAst("`_`a", &.{ .symbol_literal, .underscore, .symbol_literal }, &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary });
    try testAstMode(
        .k,
        "`a_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary },
    );
    try testAstMode(.q, "`a_`", &.{ .symbol_literal, .symbol_literal }, &.{.symbol_list_literal});
    try testAstMode(
        .k,
        "`a_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary },
    );
    try testAstMode(.q, "`a_`a", &.{ .symbol_literal, .symbol_literal }, &.{.symbol_list_literal});
    try testAstModeRender(
        .k,
        "`a_ `",
        "`a_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary },
    );
    try testAstMode(.q, "`a_ `", &.{ .symbol_literal, .symbol_literal }, &.{ .symbol_literal, .symbol_literal, .apply_unary });
    try testAstModeRender(
        .k,
        "`a_ `a",
        "`a_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary },
    );
    try testAstMode(.q, "`a_ `a", &.{ .symbol_literal, .symbol_literal }, &.{ .symbol_literal, .symbol_literal, .apply_unary });
    try testAstModeRender(
        .k,
        "`a _`",
        "`a_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary },
    );
    try testAstMode(
        .q,
        "`a _`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary },
    );
    try testAstModeRender(
        .k,
        "`a _`a",
        "`a_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary },
    );
    try testAstMode(
        .q,
        "`a _`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .drop, .symbol_literal, .apply_binary },
    );
}

test "symbol list literals whitespace" {
    try testAst(
        "`symbol`symbol(`symbol`symbol;)`symbol`symbol",
        &.{ .symbol_literal, .symbol_literal, .l_paren, .symbol_literal, .symbol_literal, .semicolon, .r_paren, .symbol_literal, .symbol_literal },
        &.{ .symbol_list_literal, .list, .symbol_list_literal, .symbol_list_literal, .apply_unary, .apply_unary },
    ); // l_paren/r_paren
    try testAst(
        "`symbol`symbol{`symbol`symbol}`symbol`symbol",
        &.{ .symbol_literal, .symbol_literal, .l_brace, .symbol_literal, .symbol_literal, .r_brace, .symbol_literal, .symbol_literal },
        &.{ .symbol_list_literal, .lambda, .lambda_body, .symbol_list_literal, .symbol_list_literal, .apply_unary, .apply_unary },
    ); // l_brace/r_brace
    try testAst(
        "[`symbol`symbol;]`symbol`symbol",
        &.{ .l_bracket, .symbol_literal, .symbol_literal, .semicolon, .r_bracket, .symbol_literal, .symbol_literal },
        &.{ .expr_block, .symbol_list_literal, .symbol_list_literal, .apply_unary },
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
        &.{ .identifier, .list, .identifier, .identifier, .apply_unary, .apply_unary },
    ); // l_paren/r_paren
    try testAst(
        "x{x}x",
        &.{ .identifier, .l_brace, .identifier, .r_brace, .identifier },
        &.{ .identifier, .lambda, .lambda_body, .identifier, .identifier, .apply_unary, .apply_unary },
    ); // l_brace/r_brace
    try testAst(
        "[x;]x",
        &.{ .l_bracket, .identifier, .semicolon, .r_bracket, .identifier },
        &.{ .expr_block, .identifier, .identifier, .apply_unary },
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
    try testAst("[1]", &.{ .l_bracket, .number_literal, .r_bracket }, &.{ .expr_block, .number_literal });
    try testAstRender(
        "[;]",
        "[]",
        &.{ .l_bracket, .semicolon, .r_bracket },
        &.{.expr_block},
    );
    try testAst(
        "[1;]",
        &.{ .l_bracket, .number_literal, .semicolon, .r_bracket },
        &.{ .expr_block, .number_literal },
    );
    try testAst(
        "[1;2]",
        &.{ .l_bracket, .number_literal, .semicolon, .number_literal, .r_bracket },
        &.{ .expr_block, .number_literal, .number_literal },
    );
}
