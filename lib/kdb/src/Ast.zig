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
        @as(u32, @intCast(tree.tokenLen(parse_error.token)))
    else
        0;
}

pub fn tokenSlice(tree: Ast, token_index: Token.Index) []const u8 {
    const loc: Token.Loc = tree.tokens.items(.loc)[token_index];
    return tree.source[loc.start..loc.end];
}

pub fn tokenLen(tree: Ast, token_index: Token.Index) usize {
    const loc: Token.Loc = tree.tokens.items(.loc)[token_index];
    return loc.end - loc.start;
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

pub fn endsBlock(tree: Ast, token_index: Token.Index) bool {
    const tags: []Token.Tag = tree.tokens.items(.tag);
    const locs: []Token.Loc = tree.tokens.items(.loc);

    const tag = tags[token_index];
    switch (tag) {
        .eof => return true,
        else => {
            if (tags[token_index + 1] == .eof) return true;
            const next_token_start = locs[token_index + 1].start;
            return next_token_start == tree.source.len or tree.source[next_token_start - 1] == '\n';
        },
    }
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

        .@"return",
        .signal,
        => return main_tokens[n] - end_offset,

        .assign,
        .global_assign,
        => n = datas[n].lhs,

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
        .ampersand,
        .ampersand_colon,
        .pipe,
        .pipe_colon,
        .caret,
        .caret_colon,
        .equal,
        .equal_colon,
        .angle_bracket_left,
        .angle_bracket_left_colon,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_colon,
        .angle_bracket_right_equal,
        .dollar,
        .dollar_colon,
        .comma,
        .comma_colon,
        .hash,
        .hash_colon,
        .underscore,
        .underscore_colon,
        .tilde,
        .tilde_colon,
        .bang,
        .bang_colon,
        .question_mark,
        .question_mark_colon,
        .at,
        .at_colon,
        .period,
        .period_colon,
        .zero_colon,
        .zero_colon_colon,
        .one_colon,
        .one_colon_colon,
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

        .call,
        => n = datas[n].lhs,

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
        .exec,
        .update,
        .delete_rows,
        .delete_cols,
        => return main_tokens[n] + end_offset,

        .do,
        .@"if",
        .@"while",
        => return main_tokens[n] + end_offset,
    };
}

pub fn lastToken(tree: Ast, node: Node.Index) Token.Index {
    const tags: []Node.Tag = tree.nodes.items(.tag);
    const datas: []Node.Data = tree.nodes.items(.data);
    const main_tokens: []Token.Index = tree.nodes.items(.main_token);
    const end_offset: Token.Index = 0;
    var n = node;
    while (true) switch (tags[n]) {
        .root,
        => return @intCast(tree.tokens.len - 1),

        .empty,
        => return main_tokens[n] + end_offset,

        .grouped_expression,
        .empty_list,
        .list,
        => return datas[n].rhs + end_offset,

        .table_literal,
        => return datas[n].rhs + end_offset,

        .lambda,
        .lambda_semicolon,
        => return tree.fullLambda(n).r_brace,

        .expr_block,
        => return datas[n].rhs + end_offset,

        .@"return",
        .signal,
        => n = datas[n].rhs,

        .assign,
        .global_assign,
        => n = datas[n].rhs,

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
        .ampersand,
        .ampersand_colon,
        .pipe,
        .pipe_colon,
        .caret,
        .caret_colon,
        .equal,
        .equal_colon,
        .angle_bracket_left,
        .angle_bracket_left_colon,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_colon,
        .angle_bracket_right_equal,
        .dollar,
        .dollar_colon,
        .comma,
        .comma_colon,
        .hash,
        .hash_colon,
        .underscore,
        .underscore_colon,
        .tilde,
        .tilde_colon,
        .bang,
        .bang_colon,
        .question_mark,
        .question_mark_colon,
        .at,
        .at_colon,
        .period,
        .period_colon,
        .zero_colon,
        .zero_colon_colon,
        .one_colon,
        .one_colon_colon,
        .two_colon,
        => return main_tokens[n] + end_offset,

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => return main_tokens[n] + end_offset,

        .call,
        => return tree.fullCall(n).r_bracket,

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
            const select = tree.fullSelect(n);
            n = if (select.where) |where| where.exprs[where.exprs.len - 1] else select.from;
        },

        .exec,
        => {
            const exec = tree.fullExec(n);
            n = if (exec.where) |where| where.exprs[where.exprs.len - 1] else exec.from;
        },

        .update,
        => {
            const update = tree.fullUpdate(n);
            n = if (update.where) |where| where.exprs[where.exprs.len - 1] else update.from;
        },

        .delete_rows,
        => {
            const delete = tree.fullDeleteRows(n);
            n = if (delete.where) |where| where.exprs[where.exprs.len - 1] else delete.from;
        },

        .delete_cols,
        => n = tree.fullDeleteCols(n).from,

        .do,
        .@"if",
        .@"while",
        => return tree.fullStatement(n).r_bracket,
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

        .cannot_apply_operator_directly => try writer.writeAll("cannot apply operator directly"),
        .cannot_apply_iterator_directly => try writer.writeAll("cannot apply iterator directly"),
        .expected_whitespace => try writer.writeAll("expected whitespace"),

        .cannot_combine_limit_expression_and_distinct,
        => try writer.writeAll("Cannot combine limit expression and distinct in select statement"),
        .cannot_define_where_cond_in_delete_cols,
        => try writer.writeAll("Cannot define where condition in delete columns statement"),

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

pub fn fullLambda(tree: Ast, node: Node.Index) full.Lambda {
    const node_tags: []Node.Tag = tree.nodes.items(.tag);
    assert(node_tags[node] == .lambda or node_tags[node] == .lambda_semicolon);

    const data: Node.Data = tree.nodes.items(.data)[node];
    const has_trailing = node_tags[node] == .lambda_semicolon;

    const params = blk: {
        const sub_range = tree.extraData(data.lhs, Node.SubRange);
        break :blk tree.extra_data[sub_range.start..sub_range.end];
    };
    const body = blk: {
        const sub_range = tree.extraData(data.rhs, Node.SubRange);
        break :blk tree.extra_data[sub_range.start..sub_range.end];
    };

    const l_brace = tree.nodes.items(.main_token)[node];
    const r_bracket: ?Token.Index = if (params.len > 0)
        if (node_tags[params[params.len - 1]] == .empty)
            tree.lastToken(params[params.len - 1])
        else
            tree.lastToken(params[params.len - 1]) + 1
    else
        null;
    const offset: u32 = if (has_trailing) 1 else 0;
    const r_brace = if (body.len > 0)
        tree.lastToken(body[body.len - 1]) + 1 + offset
    else if (r_bracket) |rb|
        rb + 1 + offset
    else
        l_brace + 1 + offset;

    const full_params: ?full.Lambda.Params = if (r_bracket) |rb| .{
        .l_bracket = tree.firstToken(params[0]) - 1,
        .params = params,
        .r_bracket = rb,
    } else null;

    return .{
        .l_brace = l_brace,
        .params = full_params,
        .body = body,
        .r_brace = r_brace,
        .has_trailing = has_trailing,
    };
}

pub fn fullCall(tree: Ast, node: Node.Index) full.Call {
    assert(tree.nodes.items(.tag)[node] == .call);

    const data: Node.Data = tree.nodes.items(.data)[node];
    const sub_range = tree.extraData(data.rhs, Node.SubRange);

    const func = data.lhs;
    const args = tree.extra_data[sub_range.start..sub_range.end];

    const l_bracket = tree.nodes.items(.main_token)[node];
    const r_bracket = if (args.len > 0)
        if (tree.nodes.items(.tag)[args[args.len - 1]] == .empty)
            tree.lastToken(args[args.len - 1])
        else
            tree.lastToken(args[args.len - 1]) + 1
    else
        l_bracket + 1;

    return .{
        .func = func,
        .l_bracket = l_bracket,
        .args = args,
        .r_bracket = r_bracket,
    };
}

pub fn fullSelect(tree: Ast, node: Node.Index) full.Select {
    assert(tree.nodes.items(.tag)[node] == .select);

    const data = tree.nodes.items(.data)[node];
    const select = tree.extraData(data.lhs, Node.Select);

    const select_exprs = tree.extra_data[select.select_start..select.by_start];
    const by_exprs = tree.extra_data[select.by_start..select.where_start];
    const where_exprs = tree.extra_data[select.where_start..select.where_end];

    const select_token = tree.nodes.items(.main_token)[node];
    const limit_expr: ?Node.Index = if (select.limit > 0) select.limit else null;
    const order_token: ?Token.Index = if (select.order > 0) select.order else null;
    const distinct_token: ?Token.Index = if (select.data.distinct) select_token + 1 else null;
    const from_token = tree.firstToken(select.from) - 1;

    const limit: ?full.Select.Limit = if (select.limit > 0 or select.order > 0) .{
        .l_bracket = select_token + 1,
        .expr = limit_expr,
        .order_token = order_token,
        .r_bracket = if (order_token) |tok|
            tok + 1
        else if (limit_expr) |expr|
            tree.lastToken(expr) + 1
        else
            unreachable,
    } else null;

    const by: ?full.Select.By = if (select.data.has_by) .{
        .by_token = if (select_exprs.len > 0)
            tree.lastToken(select_exprs[select_exprs.len - 1]) + 1
        else if (distinct_token) |tok|
            tok + 1
        else if (limit) |l|
            l.r_bracket + 1
        else
            select_token + 1,
        .exprs = by_exprs,
    } else null;

    const where: ?full.Select.Where = if (where_exprs.len > 0) .{
        .where_token = tree.lastToken(select.from) + 1,
        .exprs = where_exprs,
    } else null;

    return .{
        .select_token = select_token,
        .limit = limit,
        .distinct_token = distinct_token,
        .select = select_exprs,
        .by = by,
        .from_token = from_token,
        .from = select.from,
        .where = where,
    };
}

pub fn fullExec(tree: Ast, node: Node.Index) full.Exec {
    assert(tree.nodes.items(.tag)[node] == .exec);

    const data = tree.nodes.items(.data)[node];
    const exec = tree.extraData(data.lhs, Node.Exec);

    const select_exprs = tree.extra_data[exec.select_start..exec.by_start];
    const by_exprs = tree.extra_data[exec.by_start..exec.where_start];
    const where_exprs = tree.extra_data[exec.where_start..exec.where_end];

    const exec_token = tree.nodes.items(.main_token)[node];
    const from_token = tree.firstToken(exec.from) - 1;

    const by: ?full.Exec.By = if (by_exprs.len > 0) .{
        .by_token = if (select_exprs.len > 0)
            tree.lastToken(select_exprs[select_exprs.len - 1]) + 1
        else
            exec_token + 1,
        .exprs = by_exprs,
    } else null;

    const where: ?full.Exec.Where = if (where_exprs.len > 0) .{
        .where_token = tree.lastToken(exec.from) + 1,
        .exprs = where_exprs,
    } else null;

    return .{
        .exec_token = exec_token,
        .select = select_exprs,
        .by = by,
        .from_token = from_token,
        .from = exec.from,
        .where = where,
    };
}

pub fn fullUpdate(tree: Ast, node: Node.Index) full.Update {
    assert(tree.nodes.items(.tag)[node] == .update);

    const data = tree.nodes.items(.data)[node];
    const update = tree.extraData(data.lhs, Node.Update);

    const select_exprs = tree.extra_data[update.select_start..update.by_start];
    const by_exprs = tree.extra_data[update.by_start..update.where_start];
    const where_exprs = tree.extra_data[update.where_start..update.where_end];

    const update_token = tree.nodes.items(.main_token)[node];
    const from_token = tree.firstToken(update.from) - 1;

    const by: ?full.Update.By = if (by_exprs.len > 0) .{
        .by_token = if (select_exprs.len > 0)
            tree.lastToken(select_exprs[select_exprs.len - 1]) + 1
        else
            update_token + 1,
        .exprs = by_exprs,
    } else null;

    const where: ?full.Update.Where = if (where_exprs.len > 0) .{
        .where_token = tree.lastToken(update.from) + 1,
        .exprs = where_exprs,
    } else null;

    return .{
        .update_token = update_token,
        .select = select_exprs,
        .by = by,
        .from_token = from_token,
        .from = update.from,
        .where = where,
    };
}

pub fn fullDeleteRows(tree: Ast, node: Node.Index) full.DeleteRows {
    assert(tree.nodes.items(.tag)[node] == .delete_rows);

    const data = tree.nodes.items(.data)[node];
    const delete = tree.extraData(data.lhs, Node.DeleteRows);

    const where_exprs = tree.extra_data[delete.where_start..delete.where_end];

    const delete_token = tree.nodes.items(.main_token)[node];
    const from_token = tree.firstToken(delete.from) - 1;

    const where: ?full.DeleteRows.Where = if (where_exprs.len > 0) .{
        .where_token = tree.lastToken(delete.from) + 1,
        .exprs = where_exprs,
    } else null;

    return .{
        .delete_token = delete_token,
        .from_token = from_token,
        .from = delete.from,
        .where = where,
    };
}

pub fn fullDeleteCols(tree: Ast, node: Node.Index) full.DeleteCols {
    assert(tree.nodes.items(.tag)[node] == .delete_cols);

    const data = tree.nodes.items(.data)[node];
    const delete = tree.extraData(data.lhs, Node.DeleteCols);

    const select_exprs = tree.extra_data[delete.select_start..delete.select_end];

    const delete_token = tree.nodes.items(.main_token)[node];
    const from_token = tree.firstToken(delete.from) - 1;

    return .{
        .delete_token = delete_token,
        .select = select_exprs,
        .from_token = from_token,
        .from = delete.from,
    };
}

pub fn fullStatement(tree: Ast, node: Node.Index) full.Statement {
    const token_tags: []Token.Tag = tree.tokens.items(.tag);
    const tags: []Node.Tag = tree.nodes.items(.tag);

    const data = tree.nodes.items(.data)[node];

    const condition = data.lhs;
    const sub_range = tree.extraData(data.rhs, Node.SubRange);
    const body = tree.extra_data[sub_range.start..sub_range.end];

    const main_token = tree.nodes.items(.main_token)[node];
    const l_bracket = main_token + 1;
    const r_bracket = if (body.len > 0)
        if (tags[body[body.len - 1]] == .empty)
            tree.lastToken(body[body.len - 1])
        else
            tree.lastToken(body[body.len - 1]) + 1
    else blk: {
        const last_token = tree.lastToken(condition);
        break :blk if (token_tags[last_token + 1] == .r_bracket) last_token + 1 else last_token + 2;
    };

    return .{
        .main_token = main_token,
        .l_bracket = l_bracket,
        .condition = condition,
        .body = body,
        .r_bracket = r_bracket,
    };
}

/// Fully assembled AST node information.
pub const full = struct {
    pub const Lambda = struct {
        l_brace: Token.Index,
        params: ?Params,
        body: []Node.Index,
        r_brace: Token.Index,
        has_trailing: bool,

        pub const Params = struct {
            l_bracket: Token.Index,
            params: []Node.Index,
            r_bracket: Token.Index,
        };
    };

    pub const Call = struct {
        func: Node.Index,
        l_bracket: Token.Index,
        args: []Node.Index,
        r_bracket: Token.Index,
    };

    pub const Select = struct {
        select_token: Token.Index,
        limit: ?Limit,
        distinct_token: ?Token.Index,
        select: []Node.Index,
        by: ?By,
        from_token: Token.Index,
        from: Node.Index,
        where: ?Where,

        pub const Limit = struct {
            l_bracket: Token.Index,
            expr: ?Node.Index,
            order_token: ?Token.Index,
            r_bracket: Token.Index,
        };

        pub const By = struct {
            by_token: Token.Index,
            exprs: []Node.Index,
        };

        pub const Where = struct {
            where_token: Token.Index,
            exprs: []Node.Index,
        };
    };

    pub const Exec = struct {
        exec_token: Token.Index,
        select: []Node.Index,
        by: ?By,
        from_token: Token.Index,
        from: Node.Index,
        where: ?Where,

        pub const By = struct {
            by_token: Token.Index,
            exprs: []Node.Index,
        };

        pub const Where = struct {
            where_token: Token.Index,
            exprs: []Node.Index,
        };
    };

    pub const Update = struct {
        update_token: Token.Index,
        select: []Node.Index,
        by: ?By,
        from_token: Token.Index,
        from: Node.Index,
        where: ?Where,

        pub const By = struct {
            by_token: Token.Index,
            exprs: []Node.Index,
        };

        pub const Where = struct {
            where_token: Token.Index,
            exprs: []Node.Index,
        };
    };

    pub const DeleteRows = struct {
        delete_token: Token.Index,
        from_token: Token.Index,
        from: Node.Index,
        where: ?Where,

        pub const Where = struct {
            where_token: Token.Index,
            exprs: []Node.Index,
        };
    };

    pub const DeleteCols = struct {
        delete_token: Token.Index,
        select: []Node.Index,
        from_token: Token.Index,
        from: Node.Index,
    };

    pub const Statement = struct {
        main_token: Token.Index,
        l_bracket: Token.Index,
        condition: Node.Index,
        body: []Node.Index,
        r_bracket: Token.Index,
    };
};

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
        cannot_apply_operator_directly,
        cannot_apply_iterator_directly,
        expected_whitespace,

        cannot_combine_limit_expression_and_distinct,
        cannot_define_where_cond_in_delete_cols,

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

        /// `(lhs)`. main_token is the `(`. rhs is the token index of the `)`.
        grouped_expression,
        /// `()`. lhs unused. main_token is the `(`. rhs is the token index of the `)`.
        empty_list,
        /// `(lhs)`. main_token is the `(`. rhs is the token index of the `)`. `SubRange[lhs]`.
        list,
        /// `([]lhs)`. main_token is the `(`. rhs is the token index of the `)`. `Table[lhs]`.
        table_literal,

        /// `{[lhs]rhs}`. main_token is the `{`.
        lambda,
        /// Same as lambda but there is known to be a semicolon before the `}`.
        lambda_semicolon,

        /// `[lhs]`. main_token is the `[`. lhs can be omitted. rhs is the token index of the `]`. `SubRange[lhs]`.
        expr_block,

        /// `{:rhs}`. main_token is the `:`.
        @"return",
        /// `'rhs`. main_token is the `'`.
        signal,

        /// `lhs : rhs`. main_token is the `:`.
        assign,
        /// `lhs :: rhs`. main_token is the `::`.
        global_assign,

        /// Both lhs and rhs unused. main_token is the `:`.
        colon,
        /// Both lhs and rhs unused. main_token is the `::`.
        colon_colon,
        /// Both lhs and rhs unused. main_token is the `+`.
        plus,
        /// Both lhs and rhs unused. main_token is the `+:`.
        plus_colon,
        /// Both lhs and rhs unused. main_token is the `-`.
        minus,
        /// Both lhs and rhs unused. main_token is the `-:`.
        minus_colon,
        /// Both lhs and rhs unused. main_token is the `*`.
        asterisk,
        /// Both lhs and rhs unused. main_token is the `*:`.
        asterisk_colon,
        /// Both lhs and rhs unused. main_token is the `%`.
        percent,
        /// Both lhs and rhs unused. main_token is the `%:`.
        percent_colon,
        /// Both lhs and rhs unused. main_token is the `&`.
        ampersand,
        /// Both lhs and rhs unused. main_token is the `&:`.
        ampersand_colon,
        /// Both lhs and rhs unused. main_token is the `|`.
        pipe,
        /// Both lhs and rhs unused. main_token is the `|:`.
        pipe_colon,
        /// Both lhs and rhs unused. main_token is the `^`.
        caret,
        /// Both lhs and rhs unused. main_token is the `^:`.
        caret_colon,
        /// Both lhs and rhs unused. main_token is the `=`.
        equal,
        /// Both lhs and rhs unused. main_token is the `=:`.
        equal_colon,
        /// Both lhs and rhs unused. main_token is the `<`.
        angle_bracket_left,
        /// Both lhs and rhs unused. main_token is the `<:`.
        angle_bracket_left_colon,
        /// Both lhs and rhs unused. main_token is the `<=`.
        angle_bracket_left_equal,
        /// Both lhs and rhs unused. main_token is the `<>`.
        angle_bracket_left_right,
        /// Both lhs and rhs unused. main_token is the `>`.
        angle_bracket_right,
        /// Both lhs and rhs unused. main_token is the `>:`.
        angle_bracket_right_colon,
        /// Both lhs and rhs unused. main_token is the `>=`.
        angle_bracket_right_equal,
        /// Both lhs and rhs unused. main_token is the `$`.
        dollar,
        /// Both lhs and rhs unused. main_token is the `$:`.
        dollar_colon,
        /// Both lhs and rhs unused. main_token is the `,`.
        comma,
        /// Both lhs and rhs unused. main_token is the `,:`.
        comma_colon,
        /// Both lhs and rhs unused. main_token is the `#`.
        hash,
        /// Both lhs and rhs unused. main_token is the `#:`.
        hash_colon,
        /// Both lhs and rhs unused. main_token is the `_`.
        underscore,
        /// Both lhs and rhs unused. main_token is the `_:`.
        underscore_colon,
        /// Both lhs and rhs unused. main_token is the `~`.
        tilde,
        /// Both lhs and rhs unused. main_token is the `~:`.
        tilde_colon,
        /// Both lhs and rhs unused. main_token is the `!`.
        bang,
        /// Both lhs and rhs unused. main_token is the `!:`.
        bang_colon,
        /// Both lhs and rhs unused. main_token is the `?`.
        question_mark,
        /// Both lhs and rhs unused. main_token is the `?:`.
        question_mark_colon,
        /// Both lhs and rhs unused. main_token is the `@`.
        at,
        /// Both lhs and rhs unused. main_token is the `@:`.
        at_colon,
        /// Both lhs and rhs unused. main_token is the `.`.
        period,
        /// Both lhs and rhs unused. main_token is the `.:`.
        period_colon,
        /// Both lhs and rhs unused. main_token is the `0:`.
        zero_colon,
        /// Both lhs and rhs unused. main_token is the `0::`.
        zero_colon_colon,
        /// Both lhs and rhs unused. main_token is the `1:`.
        one_colon,
        /// Both lhs and rhs unused. main_token is the `1::`.
        one_colon_colon,
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

        /// `lhs[rhs]`. main_token is the `[`. `SubRange[rhs]`.
        call,
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
        /// `exec lhs`. rhs unused. main_token is the `exec`. `Exec[lhs]`.
        exec,
        /// `update lhs`. rhs unused. main_token is the `update`. `Update[lhs]`.
        update,
        /// `delete lhs`. rhs unused. main_token is the `delete`. `DeleteRows[lhs]`.
        delete_rows,
        /// `delete lhs`. rhs unused. main_token is the `delete`. `DeleteCols[lhs]`.
        delete_cols,

        /// `do[lhs;rhs]`. main_token is the `do`. `SubRange[rhs]`.
        do,
        /// `if[lhs;rhs]`. main_token is the `if`. `SubRange[rhs]`.
        @"if",
        /// `while[lhs;rhs]`. main_token is the `while`. `SubRange[rhs]`.
        @"while",

        pub fn getType(tag: Tag) Type {
            return switch (tag) {
                .root,
                => unreachable,

                .empty,
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

                .@"return",
                .signal,
                => .other,

                .assign,
                .global_assign,
                => .other,

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
                .ampersand,
                .ampersand_colon,
                .pipe,
                .pipe_colon,
                .caret,
                .caret_colon,
                .equal,
                .equal_colon,
                .angle_bracket_left,
                .angle_bracket_left_colon,
                .angle_bracket_left_equal,
                .angle_bracket_left_right,
                .angle_bracket_right,
                .angle_bracket_right_colon,
                .angle_bracket_right_equal,
                .dollar,
                .dollar_colon,
                .comma,
                .comma_colon,
                .hash,
                .hash_colon,
                .underscore,
                .underscore_colon,
                .tilde,
                .tilde_colon,
                .bang,
                .bang_colon,
                .question_mark,
                .question_mark_colon,
                .at,
                .at_colon,
                .period,
                .period_colon,
                .zero_colon,
                .zero_colon_colon,
                .one_colon,
                .one_colon_colon,
                .two_colon,
                => .unary_operator,

                .apostrophe,
                .apostrophe_colon,
                .slash,
                .slash_colon,
                .backslash,
                .backslash_colon,
                => .iterator,

                .call,
                => .other,

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
                .exec,
                .update,
                .delete_rows,
                .delete_cols,
                => .other,

                .do,
                .@"if",
                .@"while",
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
        limit: Index,
        order: Token.Index,
        /// Index into extra_data.
        select_start: Index,
        /// Index into extra_data.
        by_start: Index,
        from: Index,
        /// Index into extra_data.
        where_start: Index,
        /// Index into extra_data.
        where_end: Index,
        data: packed struct(Index) {
            has_by: bool,
            distinct: bool,
            ascending: bool,
            _: u29 = 0,
        },
    };

    pub const Exec = struct {
        /// Index into extra_data.
        select_start: Index,
        /// Index into extra_data.
        by_start: Index,
        from: Index,
        /// Index into extra_data.
        where_start: Index,
        /// Index into extra_data.
        where_end: Index,
    };

    pub const Update = struct {
        /// Index into extra_data.
        select_start: Index,
        /// Index into extra_data.
        by_start: Index,
        from: Index,
        /// Index into extra_data.
        where_start: Index,
        /// Index into extra_data.
        where_end: Index,
    };

    pub const DeleteRows = struct {
        from: Index,
        /// Index into extra_data.
        where_start: Index,
        /// Index into extra_data.
        where_end: Index,
    };

    pub const DeleteCols = struct {
        /// Index into extra_data.
        select_start: Index,
        /// Index into extra_data.
        select_end: Index,
        from: Index,
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
    const end_off = token_locs[end_tok].start + @as(u32, @intCast(tree.tokenLen(end_tok)));
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

    // Errors
    if (tree.errors.len > 0) {
        std.debug.print("error\n", .{});
        try @import("root.zig").printAstErrorsToStderr(gpa, tree, "test", .auto);
        return error.Unexpected;
    }

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
    for (tree.errors, 0..) |err, i| actual_errors[i] = err.tag;
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

    try testAst(
        "{-1}[]-1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .lambda, .number_literal, .call, .minus, .number_literal, .apply_binary },
    );
    try testAstRender(
        "{-1}[]- 1",
        "{-1}[]-1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .lambda, .number_literal, .call, .minus, .number_literal, .apply_binary },
    );
    try testAstRender(
        "{-1}[] - 1",
        "{-1}[]-1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .lambda, .number_literal, .call, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "{-1}[] -1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .number_literal },
        &.{ .lambda, .number_literal, .call, .number_literal, .apply_unary },
    );

    try testAstMode(
        .k,
        "([]-1)",
        &.{ .l_paren, .l_bracket, .r_bracket, .minus, .number_literal, .r_paren },
        &.{ .table_literal, .minus, .number_literal, .apply_unary },
    );
    try failAstMode(
        .q,
        "([]-1)",
        &.{ .l_paren, .l_bracket, .r_bracket, .minus, .number_literal, .r_paren },
        &.{.cannot_apply_operator_directly},
    );
    try testAstMode(
        .k,
        "([x]-1)",
        &.{ .l_paren, .l_bracket, .identifier, .r_bracket, .minus, .number_literal, .r_paren },
        &.{ .table_literal, .identifier, .minus, .number_literal, .apply_unary },
    );
    try failAstMode(
        .q,
        "([x]-1)",
        &.{ .l_paren, .l_bracket, .identifier, .r_bracket, .minus, .number_literal, .r_paren },
        &.{.cannot_apply_operator_directly},
    );
}

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
        &.{ .lambda, .empty },
    );
    try testAst(
        "{[]1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .empty, .number_literal },
    );
    try testAstMode(
        .k,
        "{[]-1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .minus, .number_literal, .r_brace },
        &.{ .lambda, .empty, .minus, .number_literal, .apply_unary },
    );
    try failAstMode(
        .q,
        "{[]-1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .minus, .number_literal, .r_brace },
        &.{.cannot_apply_operator_directly},
    );
    try testAst(
        "{[] -1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .empty, .number_literal },
    );
    try testAst(
        "{[]1;}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .empty, .number_literal },
    );
    try testAstMode(
        .k,
        "{[]-1;}",
        &.{ .l_brace, .l_bracket, .r_bracket, .minus, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .empty, .minus, .number_literal, .apply_unary },
    );
    try failAstMode(
        .q,
        "{[]-1;}",
        &.{ .l_brace, .l_bracket, .r_bracket, .minus, .number_literal, .semicolon, .r_brace },
        &.{.cannot_apply_operator_directly},
    );
    try testAst(
        "{[] -1;}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .empty, .number_literal },
    );
    try testAst(
        "{[]1;2}",
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .number_literal, .r_brace },
        &.{ .lambda, .empty, .number_literal, .number_literal },
    );
    try testAst(
        "{[]1;2;}",
        &.{
            .l_brace,   .l_bracket,      .r_bracket, .number_literal,
            .semicolon, .number_literal, .semicolon, .r_brace,
        },
        &.{ .lambda_semicolon, .empty, .number_literal, .number_literal },
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
        &.{ .lambda, .empty },
    );
    try testAstRender(
        \\{[]
        \\  ;}
    ,
        \\{[]
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .empty },
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
        &.{ .lambda_semicolon, .empty },
    );
    try testAst(
        \\{[]
        \\  -1}
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .empty, .number_literal },
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
        &.{ .lambda_semicolon, .empty, .number_literal },
    );
    try testAst(
        \\{[]
        \\  -1;
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda_semicolon, .empty, .number_literal },
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
    try testAstRender(
        \\{[x;
        \\  y;
        \\  z]
        \\  1;2;3;}
    ,
        \\{[x;
        \\  y;
        \\  z]
        \\  1;2;3;
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
            .table_literal, .identifier, .call, .identifier, .plus, .identifier, .apply_binary,
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
            .table_literal, .identifier, .call,       .identifier, .plus,       .identifier,   .apply_binary,
            .identifier,    .call,       .identifier, .plus,       .identifier, .apply_binary,
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
    try testAstMode(
        .k,
        "+1",
        &.{ .plus, .number_literal },
        &.{ .plus, .number_literal, .apply_unary },
    );
    try failAstMode(
        .q,
        "+1",
        &.{ .plus, .number_literal },
        &.{.cannot_apply_operator_directly},
    );
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
    try testAstMode(
        .k,
        "(+1)",
        &.{ .l_paren, .plus, .number_literal, .r_paren },
        &.{ .grouped_expression, .plus, .number_literal, .apply_unary },
    );
    try failAstMode(
        .q,
        "(+1)",
        &.{ .l_paren, .plus, .number_literal, .r_paren },
        &.{.cannot_apply_operator_directly},
    );
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
    try failAstMode(
        .q,
        "+/1",
        &.{ .plus, .slash, .number_literal },
        &.{.cannot_apply_iterator_directly},
    );
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
        &.{ .at, .backslash_colon, .call, .identifier, .identifier },
    );
    try testAst(
        "f\\:[x;y]",
        &.{
            .identifier, .backslash_colon, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket,
        },
        &.{ .identifier, .backslash_colon, .call, .identifier, .identifier },
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
    try testAst(
        "x f[1]/y",
        &.{ .identifier, .identifier, .l_bracket, .number_literal, .r_bracket, .slash, .identifier },
        &.{ .identifier, .identifier, .call, .number_literal, .slash, .identifier, .apply_binary },
    );
    try testAst(
        "f[x;y]'[z]",
        &.{
            .identifier, .l_bracket,  .identifier, .semicolon,  .identifier,
            .r_bracket,  .apostrophe, .l_bracket,  .identifier, .r_bracket,
        },
        &.{ .identifier, .call, .identifier, .identifier, .apostrophe, .call, .identifier },
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

test "select/exec/update/delete whitespace" {
    try testAst(
        "first select from x",
        &.{ .identifier, .keyword_select, .identifier, .identifier },
        &.{ .identifier, .select, .identifier, .apply_unary },
    );
    try testAst(
        "first exec from x",
        &.{ .identifier, .keyword_exec, .identifier, .identifier },
        &.{ .identifier, .exec, .identifier, .apply_unary },
    );
    try testAst(
        "first update from x",
        &.{ .identifier, .keyword_update, .identifier, .identifier },
        &.{ .identifier, .update, .identifier, .apply_unary },
    );
    try testAst(
        "first delete from x",
        &.{ .identifier, .keyword_delete, .identifier, .identifier },
        &.{ .identifier, .delete_rows, .identifier, .apply_unary },
    );
    try testAst(
        "first delete a from x",
        &.{ .identifier, .keyword_delete, .identifier, .identifier, .identifier },
        &.{ .identifier, .delete_cols, .identifier, .identifier, .apply_unary },
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
    try testAst(
        "1\n2",
        &.{ .number_literal, .number_literal },
        &.{ .number_literal, .number_literal },
    );
    try testAstRender(
        "1\n\n\n\n\n2",
        "1\n\n2",
        &.{ .number_literal, .number_literal },
        &.{ .number_literal, .number_literal },
    );
    try testAstRender(
        ";\n\n\n\n1\n\n\n\n\n2",
        "1\n\n2",
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
        \\/
        \\block comment 2
        \\\
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
        \\/
        \\    block comment 2
        \\\
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
        &.{ .lambda, .identifier, .call },
    );
    try testAst(
        "{x}[1]",
        &.{ .l_brace, .identifier, .r_brace, .l_bracket, .number_literal, .r_bracket },
        &.{ .lambda, .identifier, .call, .number_literal },
    );
    try testAst(
        "{x+y}[1;a]",
        &.{
            .l_brace,   .identifier,     .plus,      .identifier, .r_brace,
            .l_bracket, .number_literal, .semicolon, .identifier, .r_bracket,
        },
        &.{
            .lambda, .identifier, .plus, .identifier, .apply_binary, .call, .number_literal, .identifier,
        },
    );
    try testAst(
        "{1}[]-1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .lambda, .number_literal, .call, .minus, .number_literal, .apply_binary },
    );
    try testAst(
        "{x+y}[1] -1",
        &.{
            .l_brace,   .identifier,     .plus,      .identifier,     .r_brace,
            .l_bracket, .number_literal, .r_bracket, .number_literal,
        },
        &.{
            .lambda, .identifier,     .plus,           .identifier,  .apply_binary,
            .call,   .number_literal, .number_literal, .apply_unary,
        },
    );
    try testAst(
        "{x}[a;]",
        &.{ .l_brace, .identifier, .r_brace, .l_bracket, .identifier, .semicolon, .r_bracket },
        &.{ .lambda, .identifier, .call, .identifier, .empty },
    );
}

test "projection" {
    try testAst(
        "{x+y}[1;]",
        &.{
            .l_brace, .identifier, .plus, .identifier, .r_brace, .l_bracket, .number_literal, .semicolon, .r_bracket,
        },
        &.{ .lambda, .identifier, .plus, .identifier, .apply_binary, .call, .number_literal, .empty },
    );
    try testAst(
        "{x+y}[;2]",
        &.{
            .l_brace, .identifier, .plus, .identifier, .r_brace, .l_bracket, .semicolon, .number_literal, .r_bracket,
        },
        &.{ .lambda, .identifier, .plus, .identifier, .apply_binary, .call, .empty, .number_literal },
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

    try testAst(
        \\a:{[x;y]
        \\  b:{[x;y]
        \\    c:{[x;y]
        \\      x+y};
        \\    :c[x;y];
        \\    };
        \\  b[x;y]}
    ,
        &.{
            .identifier, .colon,      .l_brace,    .l_bracket,  .identifier, .semicolon, .identifier, .r_bracket,
            .identifier, .colon,      .l_brace,    .l_bracket,  .identifier, .semicolon, .identifier, .r_bracket,
            .identifier, .colon,      .l_brace,    .l_bracket,  .identifier, .semicolon, .identifier, .r_bracket,
            .identifier, .plus,       .identifier, .r_brace,    .semicolon,  .colon,     .identifier, .l_bracket,
            .identifier, .semicolon,  .identifier, .r_bracket,  .semicolon,  .r_brace,   .semicolon,  .identifier,
            .l_bracket,  .identifier, .semicolon,  .identifier, .r_bracket,  .r_brace,
        },
        &.{
            .identifier,       .assign,     .lambda,     .identifier, .identifier,   .identifier, .assign,
            .lambda_semicolon, .identifier, .identifier, .identifier, .assign,       .lambda,     .identifier,
            .identifier,       .identifier, .plus,       .identifier, .apply_binary, .@"return",  .identifier,
            .call,             .identifier, .identifier, .identifier, .call,         .identifier, .identifier,
        },
    );
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

// TODO: delete
test "select/exec/update with commas" {
    try testAst(
        "select(a,b),c by(d,e),f from x where(g,h),i",
        &.{
            .keyword_select, .l_paren,    .identifier, .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,     .l_paren,    .identifier, .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,     .identifier, .identifier, .l_paren, .identifier, .comma,   .identifier, .r_paren,
            .comma,          .identifier,
        },
        &.{
            .select,       .grouped_expression, .identifier,         .comma,      .identifier, .apply_binary,
            .identifier,   .grouped_expression, .identifier,         .comma,      .identifier, .apply_binary,
            .identifier,   .identifier,         .grouped_expression, .identifier, .comma,      .identifier,
            .apply_binary, .identifier,
        },
    );
    try testAst(
        "exec(a,b),c by(d,e),f from x where(g,h),i",
        &.{
            .keyword_exec, .l_paren,    .identifier, .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,   .l_paren,    .identifier, .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,   .identifier, .identifier, .l_paren, .identifier, .comma,   .identifier, .r_paren,
            .comma,        .identifier,
        },
        &.{
            .exec,         .grouped_expression, .identifier,         .comma,      .identifier, .apply_binary,
            .identifier,   .grouped_expression, .identifier,         .comma,      .identifier, .apply_binary,
            .identifier,   .identifier,         .grouped_expression, .identifier, .comma,      .identifier,
            .apply_binary, .identifier,
        },
    );
    try testAst(
        "update(a,b),c by(d,e),f from x where(g,h),i",
        &.{
            .keyword_update, .l_paren,    .identifier, .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,     .l_paren,    .identifier, .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,     .identifier, .identifier, .l_paren, .identifier, .comma,   .identifier, .r_paren,
            .comma,          .identifier,
        },
        &.{
            .update,       .grouped_expression, .identifier,         .comma,      .identifier, .apply_binary,
            .identifier,   .grouped_expression, .identifier,         .comma,      .identifier, .apply_binary,
            .identifier,   .identifier,         .grouped_expression, .identifier, .comma,      .identifier,
            .apply_binary, .identifier,
        },
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

    try testAst(
        "select[1]from x",
        &.{ .keyword_select, .l_bracket, .number_literal, .r_bracket, .identifier, .identifier },
        &.{ .select, .number_literal, .identifier },
    );
    try testAst(
        "select[1]by from x",
        &.{
            .keyword_select, .l_bracket, .number_literal, .r_bracket, .identifier, .identifier, .identifier,
        },
        &.{ .select, .number_literal, .identifier },
    );
    try failAst(
        "select[1]distinct from x",
        &.{
            .keyword_select, .l_bracket, .number_literal, .r_bracket, .identifier, .identifier, .identifier,
        },
        &.{.cannot_combine_limit_expression_and_distinct},
    );
    try testAst(
        "select[1] -1*a from x",
        &.{
            .keyword_select, .l_bracket,  .number_literal, .r_bracket,  .number_literal,
            .asterisk,       .identifier, .identifier,     .identifier,
        },
        &.{
            .select, .number_literal, .number_literal, .asterisk, .identifier, .apply_binary, .identifier,
        },
    );
    try testAst(
        "select[a]from x",
        &.{ .keyword_select, .l_bracket, .identifier, .r_bracket, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select[a] -1*b from x",
        &.{
            .keyword_select, .l_bracket,  .identifier, .r_bracket,  .number_literal,
            .asterisk,       .identifier, .identifier, .identifier,
        },
        &.{
            .select, .identifier, .number_literal, .asterisk, .identifier, .apply_binary, .identifier,
        },
    );

    try testAst(
        "select[1;<a]from x",
        &.{
            .keyword_select, .l_bracket, .number_literal, .semicolon,  .angle_bracket_left,
            .identifier,     .r_bracket, .identifier,     .identifier,
        },
        &.{ .select, .number_literal, .identifier },
    );
    try testAst(
        "select[1;<a] -1*b from x",
        &.{
            .keyword_select, .l_bracket,      .number_literal, .semicolon,  .angle_bracket_left, .identifier,
            .r_bracket,      .number_literal, .asterisk,       .identifier, .identifier,         .identifier,
        },
        &.{
            .select, .number_literal, .number_literal, .asterisk, .identifier, .apply_binary, .identifier,
        },
    );
    try testAst(
        "select[a;<b]from x",
        &.{
            .keyword_select, .l_bracket, .identifier, .semicolon,  .angle_bracket_left,
            .identifier,     .r_bracket, .identifier, .identifier,
        },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select[a;<b] -1*c from x",
        &.{
            .keyword_select, .l_bracket,      .identifier, .semicolon,  .angle_bracket_left, .identifier,
            .r_bracket,      .number_literal, .asterisk,   .identifier, .identifier,         .identifier,
        },
        &.{
            .select, .identifier, .number_literal, .asterisk, .identifier, .apply_binary, .identifier,
        },
    );

    try testAst(
        "select[<a]from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_left, .identifier, .r_bracket, .identifier, .identifier,
        },
        &.{ .select, .identifier },
    );
    try testAst(
        "select[<a] -1*b from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_left, .identifier, .r_bracket,
            .number_literal, .asterisk,  .identifier,         .identifier, .identifier,
        },
        &.{ .select, .number_literal, .asterisk, .identifier, .apply_binary, .identifier },
    );
    try testAst(
        "select[<:a]from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_left_colon, .identifier, .r_bracket, .identifier, .identifier,
        },
        &.{ .select, .identifier },
    );
    try testAst(
        "select[<:a] -1*b from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_left_colon, .identifier, .r_bracket,
            .number_literal, .asterisk,  .identifier,               .identifier, .identifier,
        },
        &.{ .select, .number_literal, .asterisk, .identifier, .apply_binary, .identifier },
    );
    try testAst(
        "select[<=a]from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_left_equal, .identifier, .r_bracket, .identifier, .identifier,
        },
        &.{ .select, .identifier },
    );
    try testAst(
        "select[<=a] -1*b from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_left_equal, .identifier, .r_bracket,
            .number_literal, .asterisk,  .identifier,               .identifier, .identifier,
        },
        &.{ .select, .number_literal, .asterisk, .identifier, .apply_binary, .identifier },
    );
    try testAst(
        "select[<>a]from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_left_right, .identifier, .r_bracket, .identifier, .identifier,
        },
        &.{ .select, .identifier },
    );
    try testAst(
        "select[<>a] -1*b from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_left_right, .identifier, .r_bracket,
            .number_literal, .asterisk,  .identifier,               .identifier, .identifier,
        },
        &.{ .select, .number_literal, .asterisk, .identifier, .apply_binary, .identifier },
    );
    try testAst(
        "select[>a]from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_right, .identifier, .r_bracket, .identifier, .identifier,
        },
        &.{ .select, .identifier },
    );
    try testAst(
        "select[>a] -1*b from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_right, .identifier, .r_bracket,
            .number_literal, .asterisk,  .identifier,          .identifier, .identifier,
        },
        &.{ .select, .number_literal, .asterisk, .identifier, .apply_binary, .identifier },
    );
    try testAst(
        "select[>:a]from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_right_colon, .identifier, .r_bracket, .identifier, .identifier,
        },
        &.{ .select, .identifier },
    );
    try testAst(
        "select[>:a] -1*b from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_right_colon, .identifier, .r_bracket,
            .number_literal, .asterisk,  .identifier,                .identifier, .identifier,
        },
        &.{ .select, .number_literal, .asterisk, .identifier, .apply_binary, .identifier },
    );
    try testAst(
        "select[>=a]from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_right_equal, .identifier, .r_bracket, .identifier, .identifier,
        },
        &.{ .select, .identifier },
    );
    try testAst(
        "select[>=a] -1*b from x",
        &.{
            .keyword_select, .l_bracket, .angle_bracket_right_equal, .identifier, .r_bracket,
            .number_literal, .asterisk,  .identifier,                .identifier, .identifier,
        },
        &.{ .select, .number_literal, .asterisk, .identifier, .apply_binary, .identifier },
    );
}

test "exec" {
    try testAst(
        "exec from x",
        &.{ .keyword_exec, .identifier, .identifier },
        &.{ .exec, .identifier },
    );
    try testAst(
        "exec a from x",
        &.{ .keyword_exec, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .identifier },
    );
    try testAst(
        "exec a:a from x",
        &.{ .keyword_exec, .identifier, .colon, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .assign, .identifier, .identifier },
    );
    try testAst(
        "exec a,b from x",
        &.{ .keyword_exec, .identifier, .comma, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec by c from x",
        &.{ .keyword_exec, .identifier, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .identifier },
    );
    try testAst(
        "exec by c:c from x",
        &.{ .keyword_exec, .identifier, .identifier, .colon, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .assign, .identifier, .identifier },
    );
    try testAst(
        "exec by c,d from x",
        &.{ .keyword_exec, .identifier, .identifier, .comma, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a by c from x",
        &.{ .keyword_exec, .identifier, .identifier, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a:a by c from x",
        &.{
            .keyword_exec, .identifier, .colon, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .assign, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a by c:c from x",
        &.{
            .keyword_exec, .identifier, .identifier, .identifier, .colon, .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .assign, .identifier, .identifier },
    );
    try testAst(
        "exec a:a by c:c from x",
        &.{
            .keyword_exec, .identifier, .colon,      .identifier, .identifier,
            .identifier,   .colon,      .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .assign, .identifier, .identifier, .assign, .identifier, .identifier },
    );
    try testAst(
        "exec a,b by c,d from x",
        &.{
            .keyword_exec, .identifier, .comma,      .identifier, .identifier,
            .identifier,   .comma,      .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec from x where e",
        &.{ .keyword_exec, .identifier, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .identifier },
    );
    try testAst(
        "exec from x where e,f",
        &.{ .keyword_exec, .identifier, .identifier, .identifier, .identifier, .comma, .identifier },
        &.{ .exec, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a from x where e",
        &.{ .keyword_exec, .identifier, .identifier, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a:a from x where e",
        &.{
            .keyword_exec, .identifier, .colon, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .assign, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a,b from x where e,f",
        &.{
            .keyword_exec, .identifier, .comma,      .identifier, .identifier,
            .identifier,   .identifier, .identifier, .comma,      .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec by c from x where e",
        &.{
            .keyword_exec, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec by c:c from x where e",
        &.{
            .keyword_exec, .identifier, .identifier, .colon,      .identifier,
            .identifier,   .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .assign, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec by c,d from x where e,f",
        &.{
            .keyword_exec, .identifier, .identifier, .comma, .identifier, .identifier,
            .identifier,   .identifier, .identifier, .comma, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a by c from x where e",
        &.{
            .keyword_exec, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a:a by c from x where e",
        &.{
            .keyword_exec, .identifier, .colon,      .identifier, .identifier,
            .identifier,   .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .assign, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a by c:c from x where e",
        &.{
            .keyword_exec, .identifier, .identifier, .identifier, .colon,
            .identifier,   .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .assign, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a:a by c:c from x where e",
        &.{
            .keyword_exec, .identifier, .colon,      .identifier, .identifier, .identifier,
            .colon,        .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{
            .exec, .identifier, .assign, .identifier, .identifier, .assign, .identifier, .identifier, .identifier,
        },
    );
    try testAst(
        "exec a,b by c,d from x where e,f",
        &.{
            .keyword_exec, .identifier, .comma,      .identifier, .identifier, .identifier, .comma,
            .identifier,   .identifier, .identifier, .identifier, .identifier, .comma,      .identifier,
        },
        &.{
            .exec, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
    );

    try testAst(
        "exec`a from x",
        &.{ .keyword_exec, .symbol_literal, .identifier, .identifier },
        &.{ .exec, .symbol_literal, .identifier },
    );
    try testAst(
        "exec`a`b from x",
        &.{ .keyword_exec, .symbol_literal, .symbol_literal, .identifier, .identifier },
        &.{ .exec, .symbol_list_literal, .identifier },
    );
    try testAst(
        "exec`a,`c from x",
        &.{ .keyword_exec, .symbol_literal, .comma, .symbol_literal, .identifier, .identifier },
        &.{ .exec, .symbol_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "exec`a,`c`d from x",
        &.{
            .keyword_exec, .symbol_literal, .comma, .symbol_literal, .symbol_literal, .identifier, .identifier,
        },
        &.{ .exec, .symbol_literal, .symbol_list_literal, .identifier },
    );
    try testAst(
        "exec`a`b,`c from x",
        &.{
            .keyword_exec, .symbol_literal, .symbol_literal, .comma, .symbol_literal, .identifier, .identifier,
        },
        &.{ .exec, .symbol_list_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "exec`a`b,`c`d from x",
        &.{
            .keyword_exec,   .symbol_literal, .symbol_literal, .comma,
            .symbol_literal, .symbol_literal, .identifier,     .identifier,
        },
        &.{ .exec, .symbol_list_literal, .symbol_list_literal, .identifier },
    );
    try testAst(
        "exec by`a from x",
        &.{ .keyword_exec, .identifier, .symbol_literal, .identifier, .identifier },
        &.{ .exec, .symbol_literal, .identifier },
    );
    try testAst(
        "exec by`a`b from x",
        &.{ .keyword_exec, .identifier, .symbol_literal, .symbol_literal, .identifier, .identifier },
        &.{ .exec, .symbol_list_literal, .identifier },
    );
    try testAst(
        "exec by`a,`c from x",
        &.{
            .keyword_exec, .identifier, .symbol_literal, .comma, .symbol_literal, .identifier, .identifier,
        },
        &.{ .exec, .symbol_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "exec by`a,`c`d from x",
        &.{
            .keyword_exec,   .identifier,     .symbol_literal, .comma,
            .symbol_literal, .symbol_literal, .identifier,     .identifier,
        },
        &.{ .exec, .symbol_literal, .symbol_list_literal, .identifier },
    );
    try testAst(
        "exec by`a`b,`c from x",
        &.{
            .keyword_exec, .identifier,     .symbol_literal, .symbol_literal,
            .comma,        .symbol_literal, .identifier,     .identifier,
        },
        &.{ .exec, .symbol_list_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "exec by`a`b,`c`d from x",
        &.{
            .keyword_exec,   .identifier,     .symbol_literal, .symbol_literal, .comma,
            .symbol_literal, .symbol_literal, .identifier,     .identifier,
        },
        &.{ .exec, .symbol_list_literal, .symbol_list_literal, .identifier },
    );
}

test "update" {
    try testAst(
        "update a from x",
        &.{ .keyword_update, .identifier, .identifier, .identifier },
        &.{ .update, .identifier, .identifier },
    );
    try testAst(
        "update a,b from x",
        &.{ .keyword_update, .identifier, .comma, .identifier, .identifier, .identifier },
        &.{ .update, .identifier, .identifier, .identifier },
    );
    try testAst(
        "update a by c from x",
        &.{ .keyword_update, .identifier, .identifier, .identifier, .identifier, .identifier },
        &.{ .update, .identifier, .identifier, .identifier },
    );
    try testAst(
        "update a,b by c,d from x",
        &.{
            .keyword_update, .identifier, .comma,      .identifier, .identifier,
            .identifier,     .comma,      .identifier, .identifier, .identifier,
        },
        &.{ .update, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "update a from x where e",
        &.{ .keyword_update, .identifier, .identifier, .identifier, .identifier, .identifier },
        &.{ .update, .identifier, .identifier, .identifier },
    );
    try testAst(
        "update a,b from x where e,f",
        &.{
            .keyword_update, .identifier, .comma,      .identifier, .identifier,
            .identifier,     .identifier, .identifier, .comma,      .identifier,
        },
        &.{ .update, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "update a by c from x where e",
        &.{
            .keyword_update, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
        &.{ .update, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "update a,b by c,d from x where e,f",
        &.{
            .keyword_update, .identifier, .comma,      .identifier, .identifier, .identifier, .comma,
            .identifier,     .identifier, .identifier, .identifier, .identifier, .comma,      .identifier,
        },
        &.{
            .update, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier, .identifier,
        },
    );

    try testAst(
        "update`a from x",
        &.{ .keyword_update, .symbol_literal, .identifier, .identifier },
        &.{ .update, .symbol_literal, .identifier },
    );
    try testAst(
        "update`a`b from x",
        &.{ .keyword_update, .symbol_literal, .symbol_literal, .identifier, .identifier },
        &.{ .update, .symbol_list_literal, .identifier },
    );
    try testAst(
        "update`a,`c from x",
        &.{ .keyword_update, .symbol_literal, .comma, .symbol_literal, .identifier, .identifier },
        &.{ .update, .symbol_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "update`a,`c`d from x",
        &.{
            .keyword_update, .symbol_literal, .comma, .symbol_literal, .symbol_literal, .identifier, .identifier,
        },
        &.{ .update, .symbol_literal, .symbol_list_literal, .identifier },
    );
    try testAst(
        "update`a`b,`c from x",
        &.{
            .keyword_update, .symbol_literal, .symbol_literal, .comma, .symbol_literal, .identifier, .identifier,
        },
        &.{ .update, .symbol_list_literal, .symbol_literal, .identifier },
    );
    try testAst(
        "update`a`b,`c`d from x",
        &.{
            .keyword_update, .symbol_literal, .symbol_literal, .comma,
            .symbol_literal, .symbol_literal, .identifier,     .identifier,
        },
        &.{ .update, .symbol_list_literal, .symbol_list_literal, .identifier },
    );
}

test "delete rows" {
    try testAst(
        "delete from x",
        &.{ .keyword_delete, .identifier, .identifier },
        &.{ .delete_rows, .identifier },
    );
    try testAst(
        "delete from x where a",
        &.{ .keyword_delete, .identifier, .identifier, .identifier, .identifier },
        &.{ .delete_rows, .identifier, .identifier },
    );
    try testAst(
        "delete from x where a,b",
        &.{ .keyword_delete, .identifier, .identifier, .identifier, .identifier, .comma, .identifier },
        &.{ .delete_rows, .identifier, .identifier, .identifier },
    );
}

test "delete columns" {
    try testAst(
        "delete a from x",
        &.{ .keyword_delete, .identifier, .identifier, .identifier },
        &.{ .delete_cols, .identifier, .identifier },
    );
    try testAst(
        "delete a,b from x",
        &.{ .keyword_delete, .identifier, .comma, .identifier, .identifier, .identifier },
        &.{ .delete_cols, .identifier, .identifier, .identifier },
    );
    try failAst(
        "delete a from x where b",
        &.{ .keyword_delete, .identifier, .identifier, .identifier, .identifier, .identifier },
        &.{.cannot_define_where_cond_in_delete_cols},
    );
}

test "do" {
    try failAst(
        "do a",
        &.{ .keyword_do, .identifier },
        &.{.expected_token},
    );
    try testAst(
        "do[a]",
        &.{ .keyword_do, .l_bracket, .identifier, .r_bracket },
        &.{ .do, .identifier },
    );
    try testAst(
        "do[a;]",
        &.{ .keyword_do, .l_bracket, .identifier, .semicolon, .r_bracket },
        &.{ .do, .identifier, .empty },
    );
    try testAst(
        "do[a;b]",
        &.{ .keyword_do, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .do, .identifier, .identifier },
    );
    try testAst(
        "do[a;b;]",
        &.{ .keyword_do, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .do, .identifier, .identifier, .empty },
    );
    try testAst(
        "do[a;b;c]",
        &.{
            .keyword_do, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_bracket,
        },
        &.{ .do, .identifier, .identifier, .identifier },
    );
    try testAst(
        "do[a;b;;c]",
        &.{
            .keyword_do, .l_bracket, .identifier, .semicolon, .identifier,
            .semicolon,  .semicolon, .identifier, .r_bracket,
        },
        &.{ .do, .identifier, .identifier, .empty, .identifier },
    );
}

test "if" {
    try failAst(
        "if a",
        &.{ .keyword_if, .identifier },
        &.{.expected_token},
    );
    try testAst(
        "if[a]",
        &.{ .keyword_if, .l_bracket, .identifier, .r_bracket },
        &.{ .@"if", .identifier },
    );
    try testAst(
        "if[a;]",
        &.{ .keyword_if, .l_bracket, .identifier, .semicolon, .r_bracket },
        &.{ .@"if", .identifier, .empty },
    );
    try testAst(
        "if[a;b]",
        &.{ .keyword_if, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .@"if", .identifier, .identifier },
    );
    try testAst(
        "if[a;b;]",
        &.{ .keyword_if, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .@"if", .identifier, .identifier, .empty },
    );
    try testAst(
        "if[a;b;c]",
        &.{
            .keyword_if, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_bracket,
        },
        &.{ .@"if", .identifier, .identifier, .identifier },
    );
    try testAst(
        "if[a;b;;c]",
        &.{
            .keyword_if, .l_bracket, .identifier, .semicolon, .identifier,
            .semicolon,  .semicolon, .identifier, .r_bracket,
        },
        &.{ .@"if", .identifier, .identifier, .empty, .identifier },
    );
}

test "while" {
    try failAst(
        "while a",
        &.{ .keyword_while, .identifier },
        &.{.expected_token},
    );
    try testAst(
        "while[a]",
        &.{ .keyword_while, .l_bracket, .identifier, .r_bracket },
        &.{ .@"while", .identifier },
    );
    try testAst(
        "while[a;]",
        &.{ .keyword_while, .l_bracket, .identifier, .semicolon, .r_bracket },
        &.{ .@"while", .identifier, .empty },
    );
    try testAst(
        "while[a;b]",
        &.{ .keyword_while, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .@"while", .identifier, .identifier },
    );
    try testAst(
        "while[a;b;]",
        &.{ .keyword_while, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .@"while", .identifier, .identifier, .empty },
    );
    try testAst(
        "while[a;b;c]",
        &.{
            .keyword_while, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_bracket,
        },
        &.{ .@"while", .identifier, .identifier, .identifier },
    );
    try testAst(
        "while[a;b;;c]",
        &.{
            .keyword_while, .l_bracket, .identifier, .semicolon, .identifier,
            .semicolon,     .semicolon, .identifier, .r_bracket,
        },
        &.{ .@"while", .identifier, .identifier, .empty, .identifier },
    );
}

fn testRender(file_path: []const u8) !void {
    const gpa = std.testing.allocator;

    var dir = try std.fs.openDirAbsolute(@import("test_options").path, .{});
    defer dir.close();
    const source_code = try dir.readFileAllocOptions(
        gpa,
        file_path,
        1_000_000,
        null,
        @alignOf(u8),
        0,
    );
    defer gpa.free(source_code);
    var buf: [100]u8 = undefined;
    const expected_path = try std.fmt.bufPrint(
        &buf,
        "{s}.expected.q",
        .{file_path[0 .. file_path.len - 2]},
    );
    const expected_source = try dir.readFileAlloc(gpa, expected_path, 1_000_000);
    defer gpa.free(expected_source);

    var tree = try Ast.parse(gpa, source_code, .q);
    defer tree.deinit(gpa);

    // Errors
    const actual_errors = try gpa.alloc(Error.Tag, tree.errors.len);
    defer gpa.free(actual_errors);
    for (tree.errors, 0..) |err, i| actual_errors[i] = err.tag;
    try std.testing.expectEqualSlices(Error.Tag, &.{}, actual_errors);

    // Render
    const actual_source = try render(tree, gpa);
    defer gpa.free(actual_source);
    try std.testing.expectEqualStrings(expected_source, actual_source);
}

test "render lambda.q" {
    try testRender("lambda.q");
}

test "render number_literal.q" {
    try testRender("number_list_literal.q");
}

test "render call.q" {
    try testRender("call_0.q");
    try testRender("call_1.q");
    try testRender("call_2.q");
}
