const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;
const Timer = std.time.Timer;
const Writer = std.Io.Writer;

const kdb = @import("root.zig");
pub const Token = kdb.Token;
const Tokenizer = kdb.Tokenizer;
const Parse = kdb.Parse;
pub const Render = @import("Render.zig");
const RenderSettings = Render.RenderSettings;
const Ast = @This();

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: TokenList.Slice,
nodes: NodeList.Slice,
extra_data: []u32,
mode: Mode = .q,

errors: []const Error,

tokenize_duration: u64,
parse_duration: u64,

pub const ByteOffset = u32;

pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

/// Index into `tokens`.
pub const TokenIndex = u32;

/// Index into `tokens`, or null.
pub const OptionalTokenIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn unwrap(oti: OptionalTokenIndex) ?TokenIndex {
        return if (oti == .none) null else @intFromEnum(oti);
    }

    pub fn fromToken(ti: TokenIndex) OptionalTokenIndex {
        return @enumFromInt(ti);
    }

    pub fn fromOptional(oti: ?TokenIndex) OptionalTokenIndex {
        return if (oti) |ti| @enumFromInt(ti) else .none;
    }
};

/// A relative token index.
pub const TokenOffset = enum(i32) {
    zero = 0,
    _,

    pub fn init(base: TokenIndex, destination: TokenIndex) TokenOffset {
        const base_i64: i64 = base;
        const destination_i64: i64 = destination;
        return @enumFromInt(destination_i64 - base_i64);
    }

    pub fn toOptional(to: TokenOffset) OptionalTokenOffset {
        const result: OptionalTokenOffset = @enumFromInt(@intFromEnum(to));
        assert(result != .none);
        return result;
    }

    pub fn toAbsolute(offset: TokenOffset, base: TokenIndex) TokenIndex {
        return @intCast(@as(i64, base) + @intFromEnum(offset));
    }
};

/// A relative token index, or null.
pub const OptionalTokenOffset = enum(i32) {
    none = std.math.maxInt(i32),
    _,

    pub fn unwrap(oto: OptionalTokenOffset) ?TokenOffset {
        return if (oto == .none) null else @enumFromInt(@intFromEnum(oto));
    }
};

pub fn tokenTag(tree: *const Ast, token_index: TokenIndex) Token.Tag {
    return tree.tokens.items(.tag)[token_index];
}

pub fn tokenStart(tree: *const Ast, token_index: TokenIndex) ByteOffset {
    return @intCast(tree.tokens.items(.loc)[token_index].start);
}

pub fn tokenEnd(tree: *const Ast, token_index: TokenIndex) ByteOffset {
    return @intCast(tree.tokens.items(.loc)[token_index].end);
}

pub fn nodeTag(tree: *const Ast, node: Node.Index) Node.Tag {
    return tree.nodes.items(.tag)[@intFromEnum(node)];
}

pub fn nodeMainToken(tree: *const Ast, node: Node.Index) TokenIndex {
    return tree.nodes.items(.main_token)[@intFromEnum(node)];
}

pub fn nodeData(tree: *const Ast, node: Node.Index) Node.Data {
    return tree.nodes.items(.data)[@intFromEnum(node)];
}

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

pub const Mode = enum { k, q };

pub const Version = enum {
    @"4.0",
};

pub const ParseSettings = struct {
    mode: Mode,
    version: Version,
};

/// Result should be freed with tree.deinit() when there are
/// no more references to any of the tokens or nodes.
pub fn parse(gpa: Allocator, source: [:0]const u8, settings: ParseSettings) !Ast {
    var tokens: TokenList = .empty;
    defer tokens.deinit(gpa);

    // Empirically, the zig std lib has an 8:1 ratio of source bytes to token count.
    const estimated_token_count = source.len / 8;
    try tokens.ensureTotalCapacity(gpa, estimated_token_count);

    const tokenize_duration: u64 = tokenize: {
        var timer = Timer.start() catch null;
        var tokenizer: Tokenizer = .init(source, settings.mode);
        tokenizer.skipComments();
        while (true) {
            const token = tokenizer.next();
            try tokens.append(gpa, token);
            if (token.tag == .eof) break;
        }
        break :tokenize if (timer) |*t| t.read() else 0;
    };

    var parser: Parse = .{
        .source = source,
        .gpa = gpa,
        .mode = settings.mode,
        .tokens = tokens.slice(),
        .errors = .empty,
        .nodes = .empty,
        .extra_data = .empty,
        .scratch = .empty,
        .tok_i = 0,
        .eob = false,
        .ends_expr = .empty,
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

    const parse_duration: u64 = parse: {
        var timer = Timer.start() catch null;
        try parser.parseRoot();
        break :parse if (timer) |*t| t.read() else 0;
    };

    // TODO experiment with compacting the MultiArrayList slices here
    return Ast{
        .source = source,
        .mode = settings.mode,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(gpa),
        .errors = try parser.errors.toOwnedSlice(gpa),
        .tokenize_duration = tokenize_duration,
        .parse_duration = parse_duration,
    };
}

pub fn renderAlloc(tree: Ast, gpa: Allocator, settings: RenderSettings) error{OutOfMemory}![]u8 {
    var aw: std.Io.Writer.Allocating = .init(gpa);
    defer aw.deinit();
    render(tree, gpa, &aw.writer, settings) catch |err| switch (err) {
        error.WriteFailed, error.OutOfMemory => return error.OutOfMemory,
    };
    return aw.toOwnedSlice();
}

pub fn render(tree: Ast, gpa: Allocator, w: *Writer, settings: RenderSettings) Render.Error!void {
    return Render.renderTree(gpa, w, tree, settings);
}

/// Returns an extra offset for column and byte offset of errors that
/// should point after the token in the error message.
pub fn errorOffset(tree: Ast, parse_error: Error) u32 {
    return if (parse_error.token_is_prev)
        @as(u32, @intCast(tree.tokenLen(parse_error.token)))
    else
        0;
}

pub fn tokenSlice(tree: Ast, token_index: TokenIndex) []const u8 {
    const loc: Token.Loc = tree.tokens.items(.loc)[token_index];
    return tree.source[loc.start..loc.end];
}

pub fn extraDataSlice(tree: Ast, range: Node.SubRange, comptime T: type) []const T {
    return @ptrCast(tree.extra_data[@intFromEnum(range.start)..@intFromEnum(range.end)]);
}

pub fn tokenLen(tree: Ast, token_index: TokenIndex) usize {
    const loc: Token.Loc = tree.tokens.items(.loc)[token_index];
    return loc.end - loc.start;
}

pub fn extraData(tree: Ast, index: ExtraIndex, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        @field(result, field.name) = switch (field.type) {
            Node.Index,
            Node.OptionalIndex,
            OptionalTokenIndex,
            ExtraIndex,
            => @enumFromInt(tree.extra_data[@intFromEnum(index) + i]),
            TokenIndex => tree.extra_data[@intFromEnum(index) + i],
            else => @compileError("unexpected field type: " ++ @typeName(field.type)),
        };
    }
    return result;
}

pub fn getBlocks(tree: Ast) []const Node.Index {
    // Root is always index 0.
    return tree.extraDataSlice(tree.nodeData(.root).extra_range, Node.Index);
}

pub fn endsBlock(tree: Ast, token_index: TokenIndex) bool {
    switch (tree.tokenTag(token_index)) {
        .eof => return true,
        else => {
            if (tree.tokenTag(token_index + 1) == .eof) return true;
            const next_token_start = tree.tokenStart(token_index + 1);
            return next_token_start == tree.source.len or tree.source[next_token_start - 1] == '\n';
        },
    }
}

pub fn firstToken(tree: Ast, node: Node.Index) TokenIndex {
    const end_offset: TokenIndex = 0;
    var n = node;
    while (true) switch (tree.nodeTag(n)) {
        .root,
        => return 0,

        .empty,
        => return tree.nodeMainToken(n) - end_offset,

        .grouped_expression,
        .empty_list,
        .list,
        => return tree.nodeMainToken(n) - end_offset,

        .table_literal,
        => return tree.nodeMainToken(n) - end_offset,

        .lambda,
        => return tree.nodeMainToken(n) - end_offset,

        .expr_block,
        => return tree.nodeMainToken(n) - end_offset,

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
        => return tree.nodeMainToken(n) - end_offset,

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => n = tree.nodeData(n).opt_node.unwrap() orelse return tree.nodeMainToken(n) - end_offset,

        .call,
        => n = tree.extraDataSlice(tree.nodeData(n).extra_range, Node.Index)[0],

        .apply_unary,
        => n = tree.nodeData(n).node_and_node[0],

        .apply_binary,
        => n = tree.nodeData(n).node_and_opt_node[0],

        .number_literal,
        .number_list_literal,
        .string_literal,
        .symbol_literal,
        .symbol_list_literal,
        .identifier,
        .builtin,
        .system,
        => return tree.nodeMainToken(n) - end_offset,

        .select,
        .exec,
        .update,
        .delete_rows,
        .delete_cols,
        => return tree.nodeMainToken(n) - end_offset,
    };
}

pub fn lastToken(tree: Ast, node: Node.Index) TokenIndex {
    const end_offset: TokenIndex = 0;
    var n = node;
    while (true) switch (tree.nodeTag(n)) {
        .root,
        => return @intCast(tree.tokens.len - 1),

        .empty,
        => return tree.nodeMainToken(n) + end_offset,

        .grouped_expression,
        => return tree.nodeData(n).node_and_token[1] + end_offset,

        .empty_list,
        => return tree.nodeData(n).token + end_offset,

        .list,
        => {
            const list = tree.fullList(n);
            return list.r_paren + end_offset;
        },

        .table_literal,
        => return tree.nodeData(n).extra_and_token[1] + end_offset,

        .lambda,
        => return tree.nodeData(n).extra_and_token[1] + end_offset,

        .expr_block,
        => {
            const block = tree.fullBlock(n);
            return block.r_bracket + end_offset;
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
        => return tree.nodeMainToken(n) + end_offset,

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => return tree.nodeMainToken(n) + end_offset,

        .call,
        => {
            const call = tree.fullCall(n);
            return call.r_bracket + end_offset;
        },

        .apply_unary,
        => n = tree.nodeData(n).node_and_node[1],

        .apply_binary,
        => n = tree.nodeData(n).node_and_opt_node[1].unwrap() orelse @enumFromInt(tree.nodeMainToken(n)),

        .number_literal,
        .string_literal,
        .symbol_literal,
        .identifier,
        .builtin,
        .system,
        => return tree.nodeMainToken(n) + end_offset,

        .number_list_literal,
        .symbol_list_literal,
        => return tree.nodeData(n).token + end_offset,

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
        => n = tree.extraData(tree.nodeData(n).extra, Node.DeleteCols).from,
    };
}

pub fn tokensOnSameLine(tree: Ast, token1: TokenIndex, token2: TokenIndex) bool {
    const token_locs = tree.tokens.items(.loc);
    const source = tree.source[token_locs[token1].start..token_locs[token2].start];
    return mem.indexOfScalar(u8, source, '\n') == null;
}

pub fn getNodeSource(tree: Ast, node: Node.Index) []const u8 {
    const token_locs: []Token.Loc = tree.tokens.items(.loc);
    const first_token = tree.firstToken(node);
    const last_token = tree.lastToken(node);
    const start = token_locs[first_token].start;
    const end = token_locs[last_token].end;
    return tree.source[start..end];
}

pub fn renderError(tree: Ast, parse_error: Error, writer: *std.Io.Writer) !void {
    const token_tags: []Token.Tag = tree.tokens.items(.tag);
    switch (parse_error.tag) {
        .expected_expr => try writer.print("expected expression, found '{s}'", .{
            token_tags[parse_error.token].symbol(),
        }),

        .cannot_apply_operator_directly => try writer.writeAll("cannot apply operator directly"),
        .cannot_apply_iterator_directly => try writer.writeAll("cannot apply iterator directly"),

        .cannot_combine_limit_expression_and_distinct,
        => try writer.writeAll("Cannot combine limit expression and distinct in select statement"),
        .cannot_define_where_cond_in_delete_cols,
        => try writer.writeAll("Cannot define where condition in delete columns statement"),

        .expected_qsql_token => try writer.print("expected '{s}', found '{s}'", .{
            parse_error.extra.expected_string,
            token_tags[parse_error.token].symbol(),
        }),

        .expected_token => try writer.print("expected '{s}', found '{s}'", .{
            parse_error.extra.expected_tag.symbol(),
            token_tags[parse_error.token].symbol(),
        }),
    }
}

pub fn fullLambda(tree: Ast, node: Node.Index) full.Lambda {
    assert(tree.nodeTag(node) == .lambda);

    const l_brace = tree.nodeMainToken(node);
    assert(tree.tokenTag(l_brace) == .l_brace);
    const extra_index, const r_brace = tree.nodeData(node).extra_and_token;
    assert(tree.tokenTag(r_brace) == .r_brace);
    const lambda = tree.extraData(extra_index, Node.Lambda);

    const params = tree.extraDataSlice(.{ .start = lambda.params_start, .end = lambda.params_end }, Node.Index);
    const body = tree.extraDataSlice(.{ .start = lambda.body_start, .end = lambda.body_end }, Node.Index);
    assert(body.len > 0);

    const full_params: ?full.Lambda.Params = if (params.len > 0) params: {
        const l_bracket = tree.firstToken(params[0]) - 1;
        assert(tree.tokenTag(l_bracket) == .l_bracket);
        const r_bracket = tree.lastToken(params[params.len - 1]) + @intFromBool(tree.nodeTag(params[params.len - 1]) != .empty);
        assert(tree.tokenTag(r_bracket) == .r_bracket);
        break :params .{
            .l_bracket = l_bracket,
            .params = params,
            .r_bracket = r_bracket,
        };
    } else null;

    return .{
        .l_brace = l_brace,
        .params = full_params,
        .body = body,
        .r_brace = r_brace,
    };
}

pub fn fullTable(tree: Ast, node: Node.Index) full.Table {
    assert(tree.nodeTag(node) == .table_literal);

    const extra_index, const r_paren = tree.nodeData(node).extra_and_token;
    assert(tree.tokenTag(r_paren) == .r_paren);
    const table = tree.extraData(extra_index, Node.Table);
    const keys = tree.extraDataSlice(.{ .start = table.keys_start, .end = table.keys_end }, Node.Index);
    const columns = tree.extraDataSlice(.{ .start = table.columns_start, .end = table.columns_end }, Node.Index);
    assert(columns.len > 0);

    const l_paren = tree.nodeMainToken(node);
    assert(tree.tokenTag(l_paren) == .l_paren);
    const l_bracket = l_paren + 1;
    assert(tree.tokenTag(l_bracket) == .l_bracket);
    const r_bracket = tree.firstToken(columns[0]) - 1;
    assert(tree.tokenTag(r_bracket) == .r_bracket);

    return .{
        .l_paren = l_paren,
        .l_bracket = l_bracket,
        .keys = keys,
        .r_bracket = r_bracket,
        .columns = columns,
        .r_paren = r_paren,
    };
}

pub fn fullList(tree: Ast, node: Node.Index) full.List {
    assert(tree.nodeTag(node) == .list);

    const elements = tree.extraDataSlice(tree.nodeData(node).extra_range, Node.Index);
    assert(elements.len > 1);

    const l_paren = tree.nodeMainToken(node);
    assert(tree.tokenTag(l_paren) == .l_paren);
    const r_paren = tree.lastToken(elements[elements.len - 1]) + @intFromBool(tree.nodeTag(elements[elements.len - 1]) != .empty);
    assert(tree.tokenTag(r_paren) == .r_paren);

    return .{
        .l_paren = l_paren,
        .elements = elements,
        .r_paren = r_paren,
    };
}

pub fn fullCall(tree: Ast, node: Node.Index) full.Call {
    assert(tree.nodeTag(node) == .call);

    const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Node.Index);

    const func = tree.unwrapGroupedExpr(nodes[0]);
    const args = nodes[1..];

    const l_bracket = tree.nodeMainToken(node);
    assert(tree.tokenTag(l_bracket) == .l_bracket);
    const r_bracket = tree.lastToken(nodes[nodes.len - 1]) + @intFromBool(tree.nodeTag(nodes[nodes.len - 1]) != .empty);
    assert(tree.tokenTag(r_bracket) == .r_bracket);

    return .{
        .func = func,
        .l_bracket = l_bracket,
        .args = args,
        .r_bracket = r_bracket,
    };
}

pub fn fullBlock(tree: Ast, node: Node.Index) full.Block {
    assert(tree.nodeTag(node) == .expr_block);

    const nodes = tree.extraDataSlice(tree.nodeData(node).extra_range, Node.Index);

    const l_bracket = tree.nodeMainToken(node);
    assert(tree.tokenTag(l_bracket) == .l_bracket);
    const r_bracket = if (nodes.len > 0) tree.lastToken(nodes[nodes.len - 1]) + @intFromBool(tree.nodeTag(nodes[nodes.len - 1]) != .empty) else l_bracket + 1;
    assert(tree.tokenTag(r_bracket) == .r_bracket);

    return .{
        .l_bracket = l_bracket,
        .body = if (nodes.len > 0) nodes else null,
        .r_bracket = r_bracket,
    };
}

pub fn fullSelect(tree: Ast, node: Node.Index) full.Select {
    assert(tree.nodeTag(node) == .select);

    const select = tree.extraData(tree.nodeData(node).extra, Node.Select);
    const select_exprs = tree.extraDataSlice(.{ .start = select.select_start, .end = select.by_start }, Node.Index);
    const by_exprs = tree.extraDataSlice(.{ .start = select.by_start, .end = select.where_start }, Node.Index);
    const where_exprs = tree.extraDataSlice(.{ .start = select.where_start, .end = select.where_end }, Node.Index);

    const select_token = tree.nodeMainToken(node);
    assert(std.mem.eql(u8, tree.tokenSlice(select_token), "select"));
    const limit_expr = if (select.limit.unwrap()) |limit| limit else null;
    const order_column = if (select.order.unwrap()) |order| order else null;
    const distinct_token = if (select.distinct.unwrap()) |distinct| distinct: {
        assert(std.mem.eql(u8, tree.tokenSlice(distinct), "distinct"));
        break :distinct distinct;
    } else null;
    const from_token = tree.firstToken(select.from) - 1;
    assert(std.mem.eql(u8, tree.tokenSlice(from_token), "from"));

    const limit: ?full.Select.Limit = if (select.limit != .none or select.order != .none) limit: {
        const l_bracket = select_token + 1;
        assert(tree.tokenTag(l_bracket) == .l_bracket);
        const r_bracket = if (order_column) |tok|
            tok + 1
        else if (limit_expr) |expr|
            tree.lastToken(expr) + 1
        else
            unreachable;
        assert(tree.tokenTag(r_bracket) == .r_bracket);
        break :limit .{
            .l_bracket = l_bracket,
            .expr = limit_expr,
            .order_column = order_column,
            .r_bracket = r_bracket,
        };
    } else null;

    const by: ?full.Select.By = if (select.by_token.unwrap()) |by_token| by: {
        assert(std.mem.eql(u8, tree.tokenSlice(by_token), "by"));
        break :by .{
            .by_token = by_token,
            .exprs = by_exprs,
        };
    } else null;

    const where: ?full.Select.Where = if (where_exprs.len > 0) where: {
        const where_token = tree.lastToken(select.from) + 1;
        assert(std.mem.eql(u8, tree.tokenSlice(where_token), "where"));
        break :where .{
            .where_token = where_token,
            .exprs = where_exprs,
        };
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
    assert(tree.nodeTag(node) == .exec);

    const exec = tree.extraData(tree.nodeData(node).extra, Node.Exec);
    const select_exprs = tree.extraDataSlice(.{ .start = exec.select_start, .end = exec.by_start }, Node.Index);
    const by_exprs = tree.extraDataSlice(.{ .start = exec.by_start, .end = exec.where_start }, Node.Index);
    const where_exprs = tree.extraDataSlice(.{ .start = exec.where_start, .end = exec.where_end }, Node.Index);

    const exec_token = tree.nodeMainToken(node);
    assert(std.mem.eql(u8, tree.tokenSlice(exec_token), "exec"));
    const from_token = tree.firstToken(exec.from) - 1;
    assert(std.mem.eql(u8, tree.tokenSlice(from_token), "from"));

    const by: ?full.Exec.By = if (by_exprs.len > 0) by: {
        const by_token = if (select_exprs.len > 0)
            tree.lastToken(select_exprs[select_exprs.len - 1]) + 1
        else
            exec_token + 1;
        assert(std.mem.eql(u8, tree.tokenSlice(by_token), "by"));
        break :by .{
            .by_token = by_token,
            .exprs = by_exprs,
        };
    } else null;

    const where: ?full.Exec.Where = if (where_exprs.len > 0) where: {
        const where_token = tree.lastToken(exec.from) + 1;
        assert(std.mem.eql(u8, tree.tokenSlice(where_token), "where"));
        break :where .{
            .where_token = where_token,
            .exprs = where_exprs,
        };
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
    assert(tree.nodeTag(node) == .update);

    const update = tree.extraData(tree.nodeData(node).extra, Node.Update);
    const select_exprs = tree.extraDataSlice(.{ .start = update.select_start, .end = update.by_start }, Node.Index);
    const by_exprs = tree.extraDataSlice(.{ .start = update.by_start, .end = update.where_start }, Node.Index);
    const where_exprs = tree.extraDataSlice(.{ .start = update.where_start, .end = update.where_end }, Node.Index);

    const update_token = tree.nodeMainToken(node);
    assert(std.mem.eql(u8, tree.tokenSlice(update_token), "update"));
    const from_token = tree.firstToken(update.from) - 1;
    assert(std.mem.eql(u8, tree.tokenSlice(from_token), "from"));

    const by: ?full.Update.By = if (by_exprs.len > 0) by: {
        const by_token = if (select_exprs.len > 0)
            tree.lastToken(select_exprs[select_exprs.len - 1]) + 1
        else
            update_token + 1;
        assert(std.mem.eql(u8, tree.tokenSlice(by_token), "by"));
        break :by .{
            .by_token = by_token,
            .exprs = by_exprs,
        };
    } else null;

    const where: ?full.Update.Where = if (where_exprs.len > 0) where: {
        const where_token = tree.lastToken(update.from) + 1;
        assert(std.mem.eql(u8, tree.tokenSlice(where_token), "where"));
        break :where .{
            .where_token = where_token,
            .exprs = where_exprs,
        };
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
    assert(tree.nodeTag(node) == .delete_rows);

    const delete = tree.extraData(tree.nodeData(node).extra, Node.DeleteRows);
    const where_exprs = tree.extraDataSlice(.{ .start = delete.where_start, .end = delete.where_end }, Node.Index);

    const delete_token = tree.nodeMainToken(node);
    assert(std.mem.eql(u8, tree.tokenSlice(delete_token), "delete"));
    const from_token = tree.firstToken(delete.from) - 1;
    assert(std.mem.eql(u8, tree.tokenSlice(from_token), "from"));

    const where: ?full.DeleteRows.Where = if (where_exprs.len > 0) where: {
        const where_token = tree.lastToken(delete.from) + 1;
        assert(std.mem.eql(u8, tree.tokenSlice(where_token), "where"));
        break :where .{
            .where_token = where_token,
            .exprs = where_exprs,
        };
    } else null;

    return .{
        .delete_token = delete_token,
        .from_token = from_token,
        .from = delete.from,
        .where = where,
    };
}

pub fn fullDeleteCols(tree: Ast, node: Node.Index) full.DeleteCols {
    assert(tree.nodeTag(node) == .delete_cols);

    const delete = tree.extraData(tree.nodeData(node).extra, Node.DeleteCols);
    const select_tokens = tree.extraDataSlice(.{ .start = delete.select_token_start, .end = delete.select_token_end }, TokenIndex);

    const delete_token = tree.nodeMainToken(node);
    assert(std.mem.eql(u8, tree.tokenSlice(delete_token), "delete"));
    const from_token = tree.firstToken(delete.from) - 1;
    assert(std.mem.eql(u8, tree.tokenSlice(from_token), "from"));

    return .{
        .delete_token = delete_token,
        .select_tokens = select_tokens,
        .from_token = from_token,
        .from = delete.from,
    };
}

/// Fully assembled AST node information.
pub const full = struct {
    pub const Lambda = struct {
        l_brace: TokenIndex,
        params: ?Params,
        body: []const Node.Index,
        r_brace: TokenIndex,

        pub const Params = struct {
            l_bracket: TokenIndex,
            params: []const Node.Index,
            r_bracket: TokenIndex,
        };
    };

    pub const Table = struct {
        l_paren: TokenIndex,
        l_bracket: TokenIndex,
        keys: []const Node.Index,
        r_bracket: TokenIndex,
        columns: []const Node.Index,
        r_paren: TokenIndex,
    };

    pub const List = struct {
        l_paren: TokenIndex,
        elements: []const Node.Index,
        r_paren: TokenIndex,
    };

    pub const Call = struct {
        func: Node.Index,
        l_bracket: TokenIndex,
        args: []const Node.Index,
        r_bracket: TokenIndex,
    };

    pub const Block = struct {
        l_bracket: TokenIndex,
        body: ?[]const Node.Index,
        r_bracket: TokenIndex,
    };

    pub const Select = struct {
        select_token: TokenIndex,
        limit: ?Limit,
        distinct_token: ?TokenIndex,
        select: []const Node.Index,
        by: ?By,
        from_token: TokenIndex,
        from: Node.Index,
        where: ?Where,

        pub const Limit = struct {
            l_bracket: TokenIndex,
            expr: ?Node.Index,
            order_column: ?TokenIndex,
            r_bracket: TokenIndex,
        };

        pub const By = struct {
            by_token: TokenIndex,
            exprs: []const Node.Index,
        };

        pub const Where = struct {
            where_token: TokenIndex,
            exprs: []const Node.Index,
        };
    };

    pub const Exec = struct {
        exec_token: TokenIndex,
        select: []const Node.Index,
        by: ?By,
        from_token: TokenIndex,
        from: Node.Index,
        where: ?Where,

        pub const By = struct {
            by_token: TokenIndex,
            exprs: []const Node.Index,
        };

        pub const Where = struct {
            where_token: TokenIndex,
            exprs: []const Node.Index,
        };
    };

    pub const Update = struct {
        update_token: TokenIndex,
        select: []const Node.Index,
        by: ?By,
        from_token: TokenIndex,
        from: Node.Index,
        where: ?Where,

        pub const By = struct {
            by_token: TokenIndex,
            exprs: []const Node.Index,
        };

        pub const Where = struct {
            where_token: TokenIndex,
            exprs: []const Node.Index,
        };
    };

    pub const DeleteRows = struct {
        delete_token: TokenIndex,
        from_token: TokenIndex,
        from: Node.Index,
        where: ?Where,

        pub const Where = struct {
            where_token: TokenIndex,
            exprs: []const Node.Index,
        };
    };

    pub const DeleteCols = struct {
        delete_token: TokenIndex,
        select_tokens: []const TokenIndex,
        from_token: TokenIndex,
        from: Node.Index,
    };
};

pub const Error = struct {
    tag: Tag,
    is_note: bool = false,
    /// True if `token` points to the token before the token causing an issue.
    token_is_prev: bool = false,
    token: TokenIndex,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
        expected_string: []const u8,
    } = .{ .none = {} },

    pub const Tag = enum {
        expected_expr,
        cannot_apply_operator_directly,
        cannot_apply_iterator_directly,

        cannot_combine_limit_expression_and_distinct,
        cannot_define_where_cond_in_delete_cols,

        /// `expected_string` is populated.
        expected_qsql_token,

        /// `expected_tag` is populated.
        expected_token,
    };
};

/// Index into `extra_data`.
pub const ExtraIndex = enum(u32) {
    _,
};

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Type = enum {
        other,
        unary_operator,
        iterator,
    };

    /// Index into `nodes`.
    pub const Index = enum(u32) {
        root = 0,
        _,

        pub fn toOptional(i: Index) OptionalIndex {
            const result: OptionalIndex = @enumFromInt(@intFromEnum(i));
            assert(result != .none);
            return result;
        }

        pub fn toOffset(base: Index, destination: Index) Offset {
            const base_i64: i64 = @intFromEnum(base);
            const destination_i64: i64 = @intFromEnum(destination);
            return @enumFromInt(destination_i64 - base_i64);
        }
    };

    /// Index into `nodes`, or null.
    pub const OptionalIndex = enum(u32) {
        root = 0,
        none = std.math.maxInt(u32),
        _,

        pub fn unwrap(oi: OptionalIndex) ?Index {
            return if (oi == .none) null else @enumFromInt(@intFromEnum(oi));
        }

        pub fn fromIndex(i: Index) OptionalIndex {
            return i.toOptional();
        }

        pub fn fromOptional(oi: ?Index) OptionalIndex {
            return if (oi) |i| i.toOptional() else .none;
        }
    };

    /// A relative node index.
    pub const Offset = enum(i32) {
        zero = 0,
        _,

        pub fn toOptional(o: Offset) OptionalOffset {
            const result: OptionalOffset = @enumFromInt(@intFromEnum(o));
            assert(result != .none);
            return result;
        }

        pub fn toAbsolute(offset: Offset, base: Index) Index {
            return @enumFromInt(@as(i64, @intFromEnum(base)) + @intFromEnum(offset));
        }
    };

    /// A relative node index, or null.
    pub const OptionalOffset = enum(i32) {
        none = std.math.maxInt(i32),
        _,

        pub fn unwrap(oo: OptionalOffset) ?Offset {
            return if (oo == .none) null else @enumFromInt(@intFromEnum(oo));
        }
    };

    comptime {
        // Goal is to keep this under one byte for efficiency.
        assert(@sizeOf(Tag) == 1);
    }

    pub const Tag = enum {
        /// The root node which is guaranteed to be at `Node.Index.root`.
        ///
        /// The `main_token` field is the first token for the source file.
        root,
        /// The `data` field is unused.
        ///
        /// The `main_token` field is the next token.
        empty,

        /// `(expr)`.
        ///
        /// The `data` field is a `.node_and_token`:
        ///   1. a `Node.Index` to the sub-expression.
        ///   2. a `TokenIndex` to the `)` token.
        ///
        /// The `main_token` field is the `(` token.
        grouped_expression,
        /// `()`.
        ///
        /// The `data` field is a `.token` of the `)`.
        ///
        /// The `main_token` field is the `(` token.
        empty_list,
        /// `(a;b;...)`.
        ///
        /// The `data` field is a `.extra_range` that stores a `Node.Index` for
        /// each element.
        ///
        /// The `main_token` field is the `(` token.
        list,
        /// `([]a;b;...)`.
        ///
        /// The `data` field is a `.extra_and_token`:
        ///   1. a `ExtraIndex` to a `Table`.
        ///   2. a `TokenIndex` to the `)` token.
        ///
        /// The `main_token` field is the `(` token.
        table_literal,

        /// `{[]expr}`.
        ///
        /// The `data` field is a `.extra_and_token`:
        ///   1. a `ExtraIndex` to a `Lambda`.
        ///   2. a `TokenIndex` to the `}` token.
        ///
        /// The `main_token` field is the `{` token.
        lambda,

        /// `[expr]`.
        ///
        /// The `data` field is a `.extra_range` that stores a `Node.Index` for
        /// each element.
        ///
        /// The `main_token` field is the `[` token.
        expr_block,

        /// The `main_token` field is the `:` token.
        colon,
        /// The `main_token` field is the `::` token.
        colon_colon,
        /// The `main_token` field is the `+` token.
        plus,
        /// The `main_token` field is the `+:` token.
        plus_colon,
        /// The `main_token` field is the `-` token.
        minus,
        /// The `main_token` field is the `-:` token.
        minus_colon,
        /// The `main_token` field is the `*` token.
        asterisk,
        /// The `main_token` field is the `*:` token.
        asterisk_colon,
        /// The `main_token` field is the `%` token.
        percent,
        /// The `main_token` field is the `%:` token.
        percent_colon,
        /// The `main_token` field is the `&` token.
        ampersand,
        /// The `main_token` field is the `&:` token.
        ampersand_colon,
        /// The `main_token` field is the `|` token.
        pipe,
        /// The `main_token` field is the `|:` token.
        pipe_colon,
        /// The `main_token` field is the `^` token.
        caret,
        /// The `main_token` field is the `^:` token.
        caret_colon,
        /// The `main_token` field is the `=` token.
        equal,
        /// The `main_token` field is the `=:` token.
        equal_colon,
        /// The `main_token` field is the `<` token.
        angle_bracket_left,
        /// The `main_token` field is the `<:` token.
        angle_bracket_left_colon,
        /// The `main_token` field is the `<=` token.
        angle_bracket_left_equal,
        /// The `main_token` field is the `<>` token.
        angle_bracket_left_right,
        /// The `main_token` field is the `>` token.
        angle_bracket_right,
        /// The `main_token` field is the `>:` token.
        angle_bracket_right_colon,
        /// The `main_token` field is the `>=` token.
        angle_bracket_right_equal,
        /// The `main_token` field is the `$` token.
        dollar,
        /// The `main_token` field is the `$:` token.
        dollar_colon,
        /// The `main_token` field is the `,` token.
        comma,
        /// The `main_token` field is the `,:` token.
        comma_colon,
        /// The `main_token` field is the `#` token.
        hash,
        /// The `main_token` field is the `#:` token.
        hash_colon,
        /// The `main_token` field is the `_` token.
        underscore,
        /// The `main_token` field is the `_:` token.
        underscore_colon,
        /// The `main_token` field is the `~` token.
        tilde,
        /// The `main_token` field is the `~:` token.
        tilde_colon,
        /// The `main_token` field is the `!` token.
        bang,
        /// The `main_token` field is the `!:` token.
        bang_colon,
        /// The `main_token` field is the `?` token.
        question_mark,
        /// The `main_token` field is the `?:` token.
        question_mark_colon,
        /// The `main_token` field is the `@` token.
        at,
        /// The `main_token` field is the `@:` token.
        at_colon,
        /// The `main_token` field is the `.` token.
        period,
        /// The `main_token` field is the `.:` token.
        period_colon,
        /// The `main_token` field is the `0:` token.
        zero_colon,
        /// The `main_token` field is the `0::` token.
        zero_colon_colon,
        /// The `main_token` field is the `1:` token.
        one_colon,
        /// The `main_token` field is the `1::` token.
        one_colon_colon,
        /// The `main_token` field is the `2:` token.
        two_colon,

        /// `expr'`.
        ///
        /// The `data` field is a `.opt_node`.
        ///
        /// The `main_token` field is the `'` token.
        apostrophe,
        /// `expr':`.
        ///
        /// The `data` field is a `.opt_node`.
        ///
        /// The `main_token` field is the `':` token.
        apostrophe_colon,
        /// `expr/`.
        ///
        /// The `data` field is a `.opt_node`.
        ///
        /// The `main_token` field is the `/` token.
        slash,
        /// `expr/:`.
        ///
        /// The `data` field is a `.opt_node`.
        ///
        /// The `main_token` field is the `/:` token.
        slash_colon,
        /// `expr\`.
        ///
        /// The `data` field is a `.opt_node`.
        ///
        /// The `main_token` field is the `\` token.
        backslash,
        /// `expr\:`.
        ///
        /// The `data` field is a `.opt_node`.
        ///
        /// The `main_token` field is the `\:` token.
        backslash_colon,

        /// `expr[a;b;...]`.
        ///
        /// The `data` field is a `.extra_range` that stores a `Node.Index` for
        /// each element.
        ///
        /// The `main_token` field is the `[` token.
        call,
        /// `expr expr`.
        ///
        /// The `data` field is a `.node_and_node`.
        ///
        /// The `main_token` field is unused.
        apply_unary,
        /// `expr op expr`.
        ///
        /// The `data` field is a `.node_and_opt_node`.
        ///
        /// The `main_token` field is the operator node.
        apply_binary,

        /// The `main_token` field is the number literal token.
        number_literal,
        /// `1 2 ...`.
        ///
        /// The `data` field is a `.token` that stores the last number literal token.
        ///
        /// The `main_token` field is the first number literal token.
        number_list_literal,
        /// The `main_token` field is the string literal token.
        string_literal,
        /// The `main_token` field is the symbol literal token.
        symbol_literal,
        /// `` `a`b...``.
        ///
        /// The `data` field is a `.token` that stores the last symbol literal token.
        ///
        /// The `main_token` field is the first symbol literal token.
        symbol_list_literal,
        /// The `main_token` field is the identifier token.
        identifier,
        /// The `main_token` field is the builtin token.
        builtin,
        /// The `main_token` field is the system token.
        system,

        /// `select ...`.
        ///
        /// The `data` field is a `.extra` to a `Select`.
        ///
        /// The `main_token` field is the `select` token.
        select,
        /// `exec ...`.
        ///
        /// The `data` field is a `.extra` to a `Exec`.
        ///
        /// The `main_token` field is the `exec` token.
        exec,
        /// `update ...`.
        ///
        /// The `data` field is a `.extra` to a `Update`.
        ///
        /// The `main_token` field is the `update` token.
        update,
        /// `delete ...`.
        ///
        /// The `data` field is a `.extra` to a `DeleteRows`.
        ///
        /// The `main_token` field is the `delete` token.
        delete_rows,
        /// `delete ...`.
        ///
        /// The `data` field is a `.extra` to a `DeleteCols`.
        ///
        /// The `main_token` field is the `delete` token.
        delete_cols,

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
                => .other,

                .expr_block,
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
                .builtin,
                .system,
                => .other,

                .select,
                .exec,
                .update,
                .delete_rows,
                .delete_cols,
                => .other,
            };
        }

        pub fn isCompoundAssignment(tag: Tag) bool {
            return switch (tag) {
                .plus_colon,
                .minus_colon,
                .asterisk_colon,
                .percent_colon,
                .ampersand_colon,
                .pipe_colon,
                .caret_colon,
                .equal_colon,
                .angle_bracket_left_colon,
                .angle_bracket_right_colon,
                .dollar_colon,
                .comma_colon,
                .hash_colon,
                .underscore_colon,
                .tilde_colon,
                .bang_colon,
                .question_mark_colon,
                .at_colon,
                .period_colon,
                => true,

                else => false,
            };
        }
    };

    pub const Data = union {
        node: Index,
        opt_node: OptionalIndex,
        token: TokenIndex,
        extra: ExtraIndex,
        node_and_node: struct { Index, Index },
        opt_node_and_opt_node: struct { OptionalIndex, OptionalIndex },
        node_and_opt_node: struct { Index, OptionalIndex },
        opt_node_and_node: struct { OptionalIndex, Index },
        node_and_extra: struct { Index, ExtraIndex },
        extra_and_node: struct { ExtraIndex, Index },
        extra_and_opt_node: struct { ExtraIndex, OptionalIndex },
        extra_and_token: struct { ExtraIndex, TokenIndex },
        node_and_token: struct { Index, TokenIndex },
        token_and_node: struct { TokenIndex, Index },
        token_and_token: struct { TokenIndex, TokenIndex },
        opt_node_and_token: struct { OptionalIndex, TokenIndex },
        opt_token_and_node: struct { OptionalTokenIndex, Index },
        opt_token_and_opt_node: struct { OptionalTokenIndex, OptionalIndex },
        opt_token_and_opt_token: struct { OptionalTokenIndex, OptionalTokenIndex },
        extra_range: SubRange,
    };

    pub const SubRange = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    pub const Table = struct {
        keys_start: ExtraIndex,
        keys_end: ExtraIndex,
        columns_start: ExtraIndex,
        columns_end: ExtraIndex,
    };

    pub const Lambda = struct {
        params_start: ExtraIndex,
        params_end: ExtraIndex,
        body_start: ExtraIndex,
        body_end: ExtraIndex,
    };

    pub const Select = struct {
        limit: OptionalIndex,
        order: OptionalTokenIndex,
        distinct: OptionalTokenIndex,
        select_start: ExtraIndex,
        by_token: OptionalTokenIndex,
        by_start: ExtraIndex,
        from: Index,
        where_start: ExtraIndex,
        where_end: ExtraIndex,
    };

    pub const Exec = struct {
        select_start: ExtraIndex,
        by_start: ExtraIndex,
        from: Index,
        where_start: ExtraIndex,
        where_end: ExtraIndex,
    };

    pub const Update = struct {
        select_start: ExtraIndex,
        by_start: ExtraIndex,
        from: Index,
        where_start: ExtraIndex,
        where_end: ExtraIndex,
    };

    pub const DeleteRows = struct {
        from: Index,
        where_start: ExtraIndex,
        where_end: ExtraIndex,
    };

    pub const DeleteCols = struct {
        select_token_start: ExtraIndex,
        select_token_end: ExtraIndex,
        from: Index,
    };
};

pub fn unwrapGroupedExpr(tree: *const Ast, node: Node.Index) Node.Index {
    var n = node;
    while (tree.nodeTag(n) == .grouped_expression) {
        n = tree.nodeData(n).node_and_token[0];
    }
    return n;
}

pub fn nodeToSpan(tree: *const Ast, node: Node.Index) Span {
    assert(node != .root);
    const main = switch (tree.nodeTag(node)) {
        .apply_unary => tree.firstToken(node),
        .apply_binary => tree.firstToken(@enumFromInt(tree.nodeMainToken(node))),
        else => tree.nodeMainToken(node),
    };
    return tokensToSpan(tree, tree.firstToken(node), tree.lastToken(node), main);
}

pub fn tokensToSpan(tree: *const Ast, start: TokenIndex, end: TokenIndex, main: TokenIndex) Span {
    const token_locs: []Token.Loc = tree.tokens.items(.loc);
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
    const end_off = token_locs[end_tok].end;
    return Span{
        .start = @intCast(start_off),
        .end = @intCast(end_off),
        .main = @intCast(token_locs[main].start),
    };
}

pub fn isCompoundAssignment(tree: *const Ast, node: Node.Index) bool {
    return switch (tree.nodeTag(node)) {
        .apply_binary => tree.nodeTag(@enumFromInt(tree.nodeMainToken(node))).isCompoundAssignment(),
        else => false,
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

    var tree = try Ast.parse(gpa, source_code, .{
        .mode = mode,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    // Errors
    if (tree.errors.len > 0) {
        std.debug.print("error\n", .{});
        try kdb.printAstErrorsToStderr(gpa, tree, "test", .auto);
        return error.Unexpected;
    }

    // Token tags
    const actual_token_tags: []Token.Tag = tree.tokens.items(.tag);
    try std.testing.expectEqualSlices(
        Token.Tag,
        expected_tokens,
        actual_token_tags[0 .. actual_token_tags.len - 1],
    );
    if (expected_tokens.len > 0) {
        const tags: []Token.Tag = tree.tokens.items(.tag);
        const blocks = tree.getBlocks();
        if (expected_tokens[0] != .semicolon) try std.testing.expectEqual(
            expected_tokens[0],
            tags[tree.firstToken(blocks[0])],
        );
        if (expected_tokens[expected_tokens.len - 1] != .semicolon) try std.testing.expectEqual(
            expected_tokens[expected_tokens.len - 1],
            tags[tree.lastToken(blocks[blocks.len - 1])],
        );
    }
    try std.testing.expectEqual(.eof, actual_token_tags[actual_token_tags.len - 1]);

    // Node tags
    var actual_nodes: std.array_list.Managed(Node.Tag) = .init(gpa);
    defer actual_nodes.deinit();
    for (tree.nodes.items(.tag)[1..]) |tag| {
        if (tag == .root) continue; // Unreserved node
        try actual_nodes.append(tag);
    }
    try std.testing.expectEqualSlices(Node.Tag, expected_nodes, actual_nodes.items);
    try std.testing.expectEqual(.root, tree.nodes.items(.tag)[0]);

    // Render
    var actual_source: std.Io.Writer.Allocating = .init(gpa);
    defer actual_source.deinit();
    try render(tree, gpa, &actual_source.writer, .{
        .indent_char = ' ',
        .indent_delta = 2,
    });
    const expected_source = expected_source: {
        const expected = expected_code orelse source_code;
        if (expected.len == 0) break :expected_source try gpa.dupe(u8, expected);

        const expected_source = try gpa.alloc(u8, expected.len + 1);
        @memcpy(expected_source[0 .. expected_source.len - 1], expected);
        expected_source[expected_source.len - 1] = '\n';
        break :expected_source expected_source;
    };
    defer gpa.free(expected_source);
    try std.testing.expectEqualStrings(expected_source, actual_source.written());
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

    var tree = try Ast.parse(gpa, source_code, .{
        .mode = mode,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    // Token tags
    const actual_tokens: []Token.Tag = tree.tokens.items(.tag);
    try std.testing.expectEqualSlices(
        Token.Tag,
        expected_tokens,
        actual_tokens[0 .. actual_tokens.len - 1],
    );
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
        &.{ .minus, .apply_binary, .period },
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
        &.{ .empty_list, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "()-.1",
        &.{ .l_paren, .r_paren, .minus, .number_literal },
        &.{ .empty_list, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "() -1",
        &.{ .l_paren, .r_paren, .number_literal },
        &.{ .empty_list, .apply_unary, .number_literal },
    );
    try testAst(
        "() -.1",
        &.{ .l_paren, .r_paren, .number_literal },
        &.{ .empty_list, .apply_unary, .number_literal },
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
        &.{ .lambda, .empty, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "{}-.1",
        &.{ .l_brace, .r_brace, .minus, .number_literal },
        &.{ .lambda, .empty, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "{} -1",
        &.{ .l_brace, .r_brace, .number_literal },
        &.{ .lambda, .empty, .apply_unary, .number_literal },
    );
    try testAst(
        "{} -.1",
        &.{ .l_brace, .r_brace, .number_literal },
        &.{ .lambda, .empty, .apply_unary, .number_literal },
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
        &.{ .expr_block, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "[]-.1",
        &.{ .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .expr_block, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "[] -1",
        &.{ .l_bracket, .r_bracket, .number_literal },
        &.{ .expr_block, .apply_unary, .number_literal },
    );
    try testAst(
        "[] -.1",
        &.{ .l_bracket, .r_bracket, .number_literal },
        &.{ .expr_block, .apply_unary, .number_literal },
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
        &.{ .number_literal, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "1-.1",
        &.{ .number_literal, .minus, .number_literal },
        &.{ .number_literal, .apply_binary, .minus, .number_literal },
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
        &.{ .string_literal, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "\"string\"-.1",
        &.{ .string_literal, .minus, .number_literal },
        &.{ .string_literal, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "`symbol-1",
        &.{ .symbol_literal, .minus, .number_literal },
        &.{ .symbol_literal, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "`symbol-.1",
        &.{ .symbol_literal, .minus, .number_literal },
        &.{ .symbol_literal, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "`symbol`list-1",
        &.{ .symbol_literal, .symbol_literal, .minus, .number_literal },
        &.{ .symbol_list_literal, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "`symbol`list-.1",
        &.{ .symbol_literal, .symbol_literal, .minus, .number_literal },
        &.{ .symbol_list_literal, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "identifier-1",
        &.{ .identifier, .minus, .number_literal },
        &.{ .identifier, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "identifier-.1",
        &.{ .identifier, .minus, .number_literal },
        &.{ .identifier, .apply_binary, .minus, .number_literal },
    );

    try testAst(
        "{-1}[]-1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .lambda, .number_literal, .call, .empty, .apply_binary, .minus, .number_literal },
    );
    try testAstRender(
        "{-1}[]- 1",
        "{-1}[]-1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .lambda, .number_literal, .call, .empty, .apply_binary, .minus, .number_literal },
    );
    try testAstRender(
        "{-1}[] - 1",
        "{-1}[]-1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .lambda, .number_literal, .call, .empty, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "{-1}[] -1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .number_literal },
        &.{ .lambda, .number_literal, .call, .empty, .apply_unary, .number_literal },
    );

    try testAstMode(
        .k,
        "([]-1)",
        &.{ .l_paren, .l_bracket, .r_bracket, .minus, .number_literal, .r_paren },
        &.{ .table_literal, .minus, .apply_unary, .number_literal },
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
        &.{ .table_literal, .identifier, .minus, .apply_unary, .number_literal },
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
        &.{ .lambda, .empty },
    );
    try testAst(
        "{1}",
        &.{ .l_brace, .number_literal, .r_brace },
        &.{ .lambda, .number_literal },
    );
    try testAst(
        "{1;}",
        &.{ .l_brace, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .number_literal, .empty },
    );
    try testAst(
        "{1;2}",
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .r_brace },
        &.{ .lambda, .number_literal, .number_literal },
    );
    try testAst(
        "{1;2;}",
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .number_literal, .number_literal, .empty },
    );
    try testAst(
        "{[]}",
        &.{ .l_brace, .l_bracket, .r_bracket, .r_brace },
        &.{ .lambda, .empty, .empty },
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
        &.{ .lambda, .empty, .minus, .apply_unary, .number_literal },
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
        &.{ .lambda, .empty, .number_literal, .empty },
    );
    try testAstMode(
        .k,
        "{[]-1;}",
        &.{ .l_brace, .l_bracket, .r_bracket, .minus, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .empty, .minus, .apply_unary, .number_literal, .empty },
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
        &.{ .lambda, .empty, .number_literal, .empty },
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
        &.{ .lambda, .empty, .number_literal, .number_literal, .empty },
    );
    try testAst(
        "{[x]}",
        &.{ .l_brace, .l_bracket, .identifier, .r_bracket, .r_brace },
        &.{ .lambda, .identifier, .empty },
    );
    try testAst(
        "{[x]1}",
        &.{ .l_brace, .l_bracket, .identifier, .r_bracket, .number_literal, .r_brace },
        &.{ .lambda, .identifier, .number_literal },
    );
    try testAst(
        "{[x]1;}",
        &.{ .l_brace, .l_bracket, .identifier, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .identifier, .number_literal, .empty },
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
        &.{ .lambda, .identifier, .number_literal, .number_literal, .empty },
    );
    try testAst(
        "{[x;y]}",
        &.{ .l_brace, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket, .r_brace },
        &.{ .lambda, .identifier, .identifier, .empty },
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
        &.{ .lambda, .identifier, .identifier, .number_literal, .empty },
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
        &.{ .lambda, .identifier, .identifier, .number_literal, .number_literal, .empty },
    );
}

test "lambda renders on multiple lines" {
    try testAst(
        \\{
        \\  }
    ,
        &.{ .l_brace, .r_brace },
        &.{ .lambda, .empty },
    );
    try testAstRender(
        \\{
        \\  ;}
    ,
        \\{
        \\  ;
        \\  }
    ,
        &.{ .l_brace, .semicolon, .r_brace },
        &.{ .lambda, .empty, .empty },
    );
    try testAst(
        \\{
        \\  ;
        \\  }
    ,
        &.{ .l_brace, .semicolon, .r_brace },
        &.{ .lambda, .empty, .empty },
    );
    try testAst(
        \\{
        \\  -1}
    ,
        &.{ .l_brace, .number_literal, .r_brace },
        &.{ .lambda, .number_literal },
    );
    try testAstRender(
        \\{
        \\  -1;}
    ,
        \\{
        \\  -1;
        \\  }
    ,
        &.{ .l_brace, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .number_literal, .empty },
    );
    try testAst(
        \\{
        \\  -1;
        \\  }
    ,
        &.{ .l_brace, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .number_literal, .empty },
    );

    try testAst(
        \\{
        \\  1;2;3}
    ,
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .r_brace },
        &.{ .lambda, .number_literal, .number_literal, .number_literal },
    );
    try testAstRender(
        \\{
        \\  1;2;3;}
    ,
        \\{
        \\  1;2;3;
        \\  }
    ,
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .number_literal, .number_literal, .number_literal, .empty },
    );
    try testAst(
        \\{
        \\  1;
        \\  2;
        \\  3}
    ,
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .r_brace },
        &.{ .lambda, .number_literal, .number_literal, .number_literal },
    );
    try testAst(
        \\{
        \\  1;
        \\  2;
        \\  3;
        \\  }
    ,
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .number_literal, .number_literal, .number_literal, .empty },
    );
    try testAst(
        \\{
        \\  1;
        \\  2;
        \\  3}
    ,
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .r_brace },
        &.{ .lambda, .number_literal, .number_literal, .number_literal },
    );
    try testAst(
        \\{
        \\  1;
        \\  2;
        \\  3;
        \\  }
    ,
        &.{ .l_brace, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .number_literal, .number_literal, .number_literal, .empty },
    );

    try testAst(
        \\{[]
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .r_brace },
        &.{ .lambda, .empty, .empty },
    );
    try testAstRender(
        \\{[]
        \\  ;}
    ,
        \\{[]
        \\  ;
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .semicolon, .r_brace },
        &.{ .lambda, .empty, .empty, .empty },
    );
    try testAst(
        \\{[]
        \\  ;
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .semicolon, .r_brace },
        &.{ .lambda, .empty, .empty, .empty },
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
        &.{ .lambda, .empty, .number_literal, .empty },
    );
    try testAst(
        \\{[]
        \\  -1;
        \\  }
    ,
        &.{ .l_brace, .l_bracket, .r_bracket, .number_literal, .semicolon, .r_brace },
        &.{ .lambda, .empty, .number_literal, .empty },
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
        &.{ .lambda, .identifier, .identifier, .identifier, .number_literal, .number_literal, .number_literal },
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
        &.{ .lambda, .identifier, .identifier, .identifier, .number_literal, .number_literal, .number_literal, .empty },
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
        &.{ .lambda, .identifier, .identifier, .identifier, .number_literal, .number_literal, .number_literal },
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
        &.{ .lambda, .identifier, .identifier, .identifier, .number_literal, .number_literal, .number_literal, .empty },
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
        &.{ .lambda, .identifier, .identifier, .identifier, .number_literal, .number_literal, .number_literal },
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
        &.{ .lambda, .identifier, .identifier, .identifier, .number_literal, .number_literal, .number_literal, .empty },
    );
}

test "precedence" {
    try testAst(
        "2*3+4",
        &.{ .number_literal, .asterisk, .number_literal, .plus, .number_literal },
        &.{
            .number_literal, .apply_binary, .asterisk, .number_literal, .apply_binary, .plus, .number_literal,
        },
    );
    try testAst(
        "(2*3)+4",
        &.{
            .l_paren, .number_literal, .asterisk, .number_literal, .r_paren, .plus, .number_literal,
        },
        &.{
            .grouped_expression, .number_literal, .apply_binary, .asterisk,
            .number_literal,     .apply_binary,   .plus,         .number_literal,
        },
    );
}

test "assign" {
    try testAst(
        "a:1",
        &.{ .identifier, .colon, .number_literal },
        &.{ .identifier, .apply_binary, .colon, .number_literal },
    );

    try testAst(
        "a::1",
        &.{ .identifier, .colon_colon, .number_literal },
        &.{ .identifier, .apply_binary, .colon_colon, .number_literal },
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
        &.{ .table_literal, .identifier, .apply_binary, .colon, .number_list_literal },
    );
    try testAst(
        "([]a::1 2)",
        &.{
            .l_paren, .l_bracket, .r_bracket, .identifier, .colon_colon, .number_literal, .number_literal, .r_paren,
        },
        &.{ .table_literal, .identifier, .apply_binary, .colon_colon, .number_list_literal },
    );
    try testAst(
        "([]a:1 2;b:2)",
        &.{
            .l_paren,        .l_bracket, .r_bracket,  .identifier, .colon,          .number_literal,
            .number_literal, .semicolon, .identifier, .colon,      .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier,   .apply_binary, .colon,          .number_list_literal,
            .identifier,    .apply_binary, .colon,        .number_literal,
        },
    );
    try testAst(
        "([]a)",
        &.{ .l_paren, .l_bracket, .r_bracket, .identifier, .r_paren },
        &.{ .table_literal, .identifier },
    );
    try testAst(
        "([]b+sum a)",
        &.{
            .l_paren, .l_bracket, .r_bracket, .identifier, .plus, .prefix_builtin, .identifier, .r_paren,
        },
        &.{ .table_literal, .identifier, .apply_binary, .plus, .builtin, .apply_unary, .identifier },
    );
    try testAst(
        "([]sum[a]+b)",
        &.{
            .l_paren,    .l_bracket, .r_bracket, .prefix_builtin, .l_bracket,
            .identifier, .r_bracket, .plus,      .identifier,     .r_paren,
        },
        &.{ .table_literal, .builtin, .call, .identifier, .apply_binary, .plus, .identifier },
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
            .l_paren,    .l_bracket, .r_bracket,      .prefix_builtin, .number_literal, .semicolon,
            .identifier, .colon,     .number_literal, .semicolon,      .number_literal, .r_paren,
        },
        &.{
            .table_literal, .builtin, .apply_unary,    .number_literal, .identifier,
            .apply_binary,  .colon,   .number_literal, .number_literal,
        },
    );
    try testAst(
        "([]a;a::til 10)",
        &.{
            .l_paren,    .l_bracket,   .r_bracket,      .identifier,     .semicolon,
            .identifier, .colon_colon, .prefix_builtin, .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier, .identifier,  .apply_binary,
            .colon_colon,   .builtin,    .apply_unary, .number_literal,
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
            .table_literal, .identifier,   .apply_binary, .colon,               .number_list_literal,
            .identifier,    .apply_binary, .colon,        .number_list_literal,
        },
    );
    try testAst(
        "([a::1 2]a::1 2)",
        &.{
            .l_paren,   .l_bracket,  .identifier,  .colon_colon,    .number_literal, .number_literal,
            .r_bracket, .identifier, .colon_colon, .number_literal, .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier,   .apply_binary, .colon_colon,         .number_list_literal,
            .identifier,    .apply_binary, .colon_colon,  .number_list_literal,
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
            .table_literal,       .identifier, .apply_binary,   .colon,      .number_list_literal, .identifier,
            .apply_binary,        .colon,      .number_literal, .identifier, .apply_binary,        .colon,
            .number_list_literal, .identifier, .apply_binary,   .colon,      .number_literal,
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
            .l_paren,   .l_bracket,  .identifier, .plus,           .prefix_builtin, .identifier,
            .r_bracket, .identifier, .plus,       .prefix_builtin, .identifier,     .r_paren,
        },
        &.{
            .table_literal, .identifier,   .apply_binary, .plus,    .builtin,     .apply_unary, .identifier,
            .identifier,    .apply_binary, .plus,         .builtin, .apply_unary, .identifier,
        },
    );
    try testAst(
        "([sum[a]+b]sum[a]+b)",
        &.{
            .l_paren,   .l_bracket,  .prefix_builtin, .l_bracket,      .identifier, .r_bracket,
            .plus,      .identifier, .r_bracket,      .prefix_builtin, .l_bracket,  .identifier,
            .r_bracket, .plus,       .identifier,     .r_paren,
        },
        &.{
            .table_literal, .builtin, .call,       .identifier,   .apply_binary, .plus,       .identifier,
            .builtin,       .call,    .identifier, .apply_binary, .plus,         .identifier,
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
            .l_paren,        .l_bracket, .prefix_builtin, .number_literal, .semicolon,      .identifier,     .colon,
            .number_literal, .semicolon, .number_literal, .r_bracket,      .prefix_builtin, .number_literal, .semicolon,
            .identifier,     .colon,     .number_literal, .semicolon,      .number_literal, .r_paren,
        },
        &.{
            .table_literal, .builtin,        .apply_unary,    .number_literal, .identifier,     .apply_binary,
            .colon,         .number_literal, .number_literal, .builtin,        .apply_unary,    .number_literal,
            .identifier,    .apply_binary,   .colon,          .number_literal, .number_literal,
        },
    );
    try testAst(
        "([a;a::til 10]a;a::til 10)",
        &.{
            .l_paren,        .l_bracket, .identifier, .semicolon, .identifier, .colon_colon, .prefix_builtin,
            .number_literal, .r_bracket, .identifier, .semicolon, .identifier, .colon_colon, .prefix_builtin,
            .number_literal, .r_paren,
        },
        &.{
            .table_literal, .identifier,  .identifier,     .apply_binary, .colon_colon,
            .builtin,       .apply_unary, .number_literal, .identifier,   .identifier,
            .apply_binary,  .colon_colon, .builtin,        .apply_unary,  .number_literal,
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
            .number_list_literal, .apply_binary, .plus,                .number_list_literal,
            .apply_binary,        .plus,         .number_list_literal,
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
        &.{ .number_literal, .apply_binary, .plus },
    );
    try testAstMode(
        .k,
        "+1",
        &.{ .plus, .number_literal },
        &.{ .plus, .apply_unary, .number_literal },
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
        &.{ .number_literal, .apply_binary, .plus, .number_literal },
    );
    try testAst(
        "(+)",
        &.{ .l_paren, .plus, .r_paren },
        &.{ .grouped_expression, .plus },
    );
    try testAst(
        "(1+)",
        &.{ .l_paren, .number_literal, .plus, .r_paren },
        &.{ .grouped_expression, .number_literal, .apply_binary, .plus },
    );
    try testAstMode(
        .k,
        "(+1)",
        &.{ .l_paren, .plus, .number_literal, .r_paren },
        &.{ .grouped_expression, .plus, .apply_unary, .number_literal },
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
        &.{ .grouped_expression, .plus, .apply_unary, .number_literal },
    );
}

test "return" {
    try testAst(
        ":1",
        &.{ .colon, .number_literal },
        &.{ .colon, .apply_unary, .number_literal },
    );
    try testAst(
        "{:1}",
        &.{ .l_brace, .colon, .number_literal, .r_brace },
        &.{ .lambda, .colon, .apply_unary, .number_literal },
    );
    try testAst(
        "{[]:1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .colon, .number_literal, .r_brace },
        &.{ .lambda, .empty, .colon, .apply_unary, .number_literal },
    );
    try testAst(
        "{(:1)}",
        &.{ .l_brace, .l_paren, .colon, .number_literal, .r_paren, .r_brace },
        &.{ .lambda, .grouped_expression, .colon, .apply_unary, .number_literal },
    );
    try testAst(
        "{[](:1)}",
        &.{ .l_brace, .l_bracket, .r_bracket, .l_paren, .colon, .number_literal, .r_paren, .r_brace },
        &.{ .lambda, .empty, .grouped_expression, .colon, .apply_unary, .number_literal },
    );
    try testAst(
        "{[][:1]}",
        &.{
            .l_brace, .l_bracket, .r_bracket, .l_bracket, .colon, .number_literal, .r_bracket, .r_brace,
        },
        &.{ .lambda, .empty, .expr_block, .colon, .apply_unary, .number_literal },
    );
    try testAst(
        "{;:1}",
        &.{ .l_brace, .semicolon, .colon, .number_literal, .r_brace },
        &.{ .lambda, .empty, .colon, .apply_unary, .number_literal },
    );
    try testAst(
        "{[];:1}",
        &.{ .l_brace, .l_bracket, .r_bracket, .semicolon, .colon, .number_literal, .r_brace },
        &.{ .lambda, .empty, .empty, .colon, .apply_unary, .number_literal },
    );
    try failAstMode(
        .q,
        "{+1}",
        &.{ .l_brace, .plus, .number_literal, .r_brace },
        &.{.cannot_apply_operator_directly},
    );
    try testAstMode(
        .k,
        "{+1}",
        &.{ .l_brace, .plus, .number_literal, .r_brace },
        &.{ .lambda, .plus, .apply_unary, .number_literal },
    );
    try failAstMode(
        .q,
        "{1+ :1}",
        &.{ .l_brace, .number_literal, .plus, .colon, .number_literal, .r_brace },
        &.{.cannot_apply_operator_directly},
    );
    try testAstMode(
        .k,
        "{1+ :1}",
        &.{ .l_brace, .number_literal, .plus, .colon, .number_literal, .r_brace },
        &.{ .lambda, .number_literal, .apply_binary, .plus, .colon, .apply_unary, .number_literal },
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
        &.{ .number_literal, .apply_binary, .plus, .slash },
    );
    try testAstMode(
        .k,
        "+/1",
        &.{ .plus, .slash, .number_literal },
        &.{ .plus, .slash, .apply_unary, .number_literal },
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
        &.{ .number_literal, .apply_binary, .plus, .slash, .number_literal },
    );

    try testAst(
        "(\\:)",
        &.{ .l_paren, .backslash_colon, .r_paren },
        &.{ .grouped_expression, .backslash_colon },
    );
    try testAst(
        "@\\:",
        &.{ .at, .backslash_colon },
        &.{ .at, .backslash_colon },
    );
    try testAst(
        "f\\:",
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
        &.{ .identifier, .apply_binary, .at, .backslash_colon, .identifier },
    );
    try testAst(
        "1+\\:1",
        &.{ .number_literal, .plus, .backslash_colon, .number_literal },
        &.{ .number_literal, .apply_binary, .plus, .backslash_colon, .number_literal },
    );
    try testAst(
        "x f\\:y",
        &.{ .identifier, .identifier, .backslash_colon, .identifier },
        &.{ .identifier, .apply_binary, .identifier, .backslash_colon, .identifier },
    );
    try testAst(
        "x{x+y}/y",
        &.{ .identifier, .l_brace, .identifier, .plus, .identifier, .r_brace, .slash, .identifier },
        &.{
            .identifier, .apply_binary, .lambda, .identifier, .apply_binary, .plus, .identifier, .slash, .identifier,
        },
    );
    try testAst(
        "x f[1]/y",
        &.{ .identifier, .identifier, .l_bracket, .number_literal, .r_bracket, .slash, .identifier },
        &.{ .identifier, .apply_binary, .identifier, .call, .number_literal, .slash, .identifier },
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
        "0 1 2,/:\\:10 20 30",
        &.{
            .number_literal,  .number_literal, .number_literal, .comma,          .slash_colon,
            .backslash_colon, .number_literal, .number_literal, .number_literal,
        },
        &.{
            .number_list_literal, .apply_binary, .comma, .slash_colon, .backslash_colon, .number_list_literal,
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
        &.{ .prefix_builtin, .keyword_select, .identifier, .identifier },
        &.{ .builtin, .apply_unary, .select, .identifier },
    );
    try testAst(
        "first exec from x",
        &.{ .prefix_builtin, .keyword_exec, .identifier, .identifier },
        &.{ .builtin, .apply_unary, .exec, .identifier },
    );
    try testAst(
        "first update from x",
        &.{ .prefix_builtin, .keyword_update, .identifier, .identifier },
        &.{ .builtin, .apply_unary, .update, .identifier },
    );
    try testAst(
        "first delete from x",
        &.{ .prefix_builtin, .keyword_delete, .identifier, .identifier },
        &.{ .builtin, .apply_unary, .delete_rows, .identifier },
    );
    try testAst(
        "first delete a from x",
        &.{ .prefix_builtin, .keyword_delete, .identifier, .identifier, .identifier },
        &.{ .builtin, .apply_unary, .delete_cols, .identifier },
    );
}

// TODO: 0011b 0 1 2
test "number literal whitespace" {
    try testAst(
        "123\"string\"",
        &.{ .number_literal, .string_literal },
        &.{ .number_literal, .apply_unary, .string_literal },
    );
    try testAst(
        "123`symbol",
        &.{ .number_literal, .symbol_literal },
        &.{ .number_literal, .apply_unary, .symbol_literal },
    );
    try testAst(
        "123 identifier",
        &.{ .number_literal, .identifier },
        &.{ .number_literal, .apply_unary, .identifier },
    );
}

test "string literal whitespace" {
    try testAst(
        "\"string\"123",
        &.{ .string_literal, .number_literal },
        &.{ .string_literal, .apply_unary, .number_literal },
    );
    try testAst(
        "\"string\"\"string\"",
        &.{ .string_literal, .string_literal },
        &.{ .string_literal, .apply_unary, .string_literal },
    );
    try testAst(
        "\"string\"`symbol",
        &.{ .string_literal, .symbol_literal },
        &.{ .string_literal, .apply_unary, .symbol_literal },
    );
    try testAst(
        "\"string\"identifier",
        &.{ .string_literal, .identifier },
        &.{ .string_literal, .apply_unary, .identifier },
    );
}

test "symbol literal whitespace" {
    try testAst(
        "`symbol 123",
        &.{ .symbol_literal, .number_literal },
        &.{ .symbol_literal, .apply_unary, .number_literal },
    );
    try testAst(
        "`symbol\"string\"",
        &.{ .symbol_literal, .string_literal },
        &.{ .symbol_literal, .apply_unary, .string_literal },
    );
    try testAst(
        "`symbol `symbol",
        &.{ .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .apply_unary, .symbol_literal },
    );
    try testAst(
        "`symbol identifier",
        &.{ .symbol_literal, .identifier },
        &.{ .symbol_literal, .apply_unary, .identifier },
    );
}

test "identifier whitespace" {
    try testAst(
        "identifier 123",
        &.{ .identifier, .number_literal },
        &.{ .identifier, .apply_unary, .number_literal },
    );
    try testAst(
        "identifier\"string\"",
        &.{ .identifier, .string_literal },
        &.{ .identifier, .apply_unary, .string_literal },
    );
    try testAst(
        "identifier`symbol",
        &.{ .identifier, .symbol_literal },
        &.{ .identifier, .apply_unary, .symbol_literal },
    );
    try testAst(
        "identifier identifier",
        &.{ .identifier, .identifier },
        &.{ .identifier, .apply_unary, .identifier },
    );
}

test "comment after block expression with semicolon" {
    try testAstRender(
        \\1 + 2  /comment
        \\ ;
    ,
        \\1+2 /comment
        \\  ;
    ,
        &.{ .number_literal, .plus, .number_literal, .semicolon },
        &.{ .number_literal, .apply_binary, .plus, .number_literal },
    );

    try testAstRender(
        \\f : { [ ]
        \\ 1 + 2  /comment
        \\ ;
        \\ 1 + 2  /comment
        \\ ;
        \\ }
    ,
        \\f:{[]
        \\  1+2 /comment
        \\    ;
        \\  1+2 /comment
        \\    ;
        \\  }
    ,
        &.{
            .identifier,     .colon,     .l_brace,        .l_bracket, .r_bracket,      .number_literal, .plus,
            .number_literal, .semicolon, .number_literal, .plus,      .number_literal, .semicolon,      .r_brace,
        },
        &.{
            .identifier, .apply_binary,   .colon,          .lambda,       .empty, .number_literal, .apply_binary,
            .plus,       .number_literal, .number_literal, .apply_binary, .plus,  .number_literal, .empty,
        },
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
        \\ block comment 2
        \\\
        \\
        \\ 3
        \\
        \\
        \\/
        \\  block comment 3
        \\\
    ,
        \\1
        \\/
        \\block comment 1
        \\\
        \\  2
        \\
        \\/
        \\ block comment 2
        \\\
        \\
        \\  3
        \\
        \\/
        \\  block comment 3
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
            .number_literal, .apply_binary, .plus, .number_literal,
            .number_literal, .apply_binary, .plus, .number_literal,
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
            .number_literal, .apply_binary, .plus, .number_literal,
            .number_literal, .apply_binary, .plus, .number_literal,
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
        &.{ .number_literal, .apply_binary, .plus, .number_literal },
    );
}

test "number literals whitespace" {
    try testAst(
        "1(1;)1",
        &.{ .number_literal, .l_paren, .number_literal, .semicolon, .r_paren, .number_literal },
        &.{
            .number_literal, .apply_unary, .list, .number_literal, .empty, .apply_unary, .number_literal,
        },
    ); // l_paren/r_paren
    try testAst(
        "1{1}1",
        &.{ .number_literal, .l_brace, .number_literal, .r_brace, .number_literal },
        &.{
            .number_literal, .apply_unary, .lambda, .number_literal, .apply_unary, .number_literal,
        },
    ); // l_brace/r_brace
    try testAst(
        "[1;]1",
        &.{ .l_bracket, .number_literal, .semicolon, .r_bracket, .number_literal },
        &.{ .expr_block, .number_literal, .empty, .apply_unary, .number_literal },
    ); // r_bracket
    try testAst(
        "\"string\"1",
        &.{ .string_literal, .number_literal },
        &.{ .string_literal, .apply_unary, .number_literal },
    ); // string_literal
    try testAst(
        "`symbol 1",
        &.{ .symbol_literal, .number_literal },
        &.{ .symbol_literal, .apply_unary, .number_literal },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol 1",
        &.{ .symbol_literal, .symbol_literal, .number_literal },
        &.{ .symbol_list_literal, .apply_unary, .number_literal },
    ); // symbol_list_literal
    try testAst(
        "x 1",
        &.{ .identifier, .number_literal },
        &.{ .identifier, .apply_unary, .number_literal },
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
            .number_list_literal, .apply_unary, .list, .number_list_literal, .empty, .apply_unary, .number_list_literal,
        },
    ); // l_paren/r_paren
    try testAst(
        "1 2 3{1 2 3}1 2 3",
        &.{
            .number_literal, .number_literal, .number_literal, .l_brace,        .number_literal, .number_literal,
            .number_literal, .r_brace,        .number_literal, .number_literal, .number_literal,
        },
        &.{
            .number_list_literal, .apply_unary, .lambda, .number_list_literal, .apply_unary, .number_list_literal,
        },
    ); // l_brace/r_brace
    try testAst(
        "[1 2 3;]1 2 3",
        &.{
            .l_bracket, .number_literal, .number_literal, .number_literal, .semicolon,
            .r_bracket, .number_literal, .number_literal, .number_literal,
        },
        &.{ .expr_block, .number_list_literal, .empty, .apply_unary, .number_list_literal },
    ); // r_bracket
    try testAst(
        "\"string\"1 2 3",
        &.{ .string_literal, .number_literal, .number_literal, .number_literal },
        &.{ .string_literal, .apply_unary, .number_list_literal },
    ); // string_literal
    try testAst(
        "`symbol 1 2 3",
        &.{ .symbol_literal, .number_literal, .number_literal, .number_literal },
        &.{ .symbol_literal, .apply_unary, .number_list_literal },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol 1 2 3",
        &.{ .symbol_literal, .symbol_literal, .number_literal, .number_literal, .number_literal },
        &.{ .symbol_list_literal, .apply_unary, .number_list_literal },
    ); // symbol_list_literal
    try testAst(
        "x 1 2 3",
        &.{ .identifier, .number_literal, .number_literal, .number_literal },
        &.{ .identifier, .apply_unary, .number_list_literal },
    ); // identifier
}

test "string literals whitespace" {
    try testAst(
        "\"string\"(\"string\";)\"string\"",
        &.{ .string_literal, .l_paren, .string_literal, .semicolon, .r_paren, .string_literal },
        &.{
            .string_literal, .apply_unary, .list, .string_literal, .empty, .apply_unary, .string_literal,
        },
    ); // l_paren/r_paren
    try testAst(
        "\"string\"{\"string\"}\"string\"",
        &.{ .string_literal, .l_brace, .string_literal, .r_brace, .string_literal },
        &.{ .string_literal, .apply_unary, .lambda, .string_literal, .apply_unary, .string_literal },
    ); // l_brace/r_brace
    try testAst(
        "[\"string\";]\"string\"",
        &.{ .l_bracket, .string_literal, .semicolon, .r_bracket, .string_literal },
        &.{ .expr_block, .string_literal, .empty, .apply_unary, .string_literal },
    ); // r_bracket
    try testAst(
        "1\"string\"",
        &.{ .number_literal, .string_literal },
        &.{ .number_literal, .apply_unary, .string_literal },
    ); // number_literal
    try testAst(
        "\"string\"\"string\"",
        &.{ .string_literal, .string_literal },
        &.{ .string_literal, .apply_unary, .string_literal },
    ); // string_literal
    try testAst(
        "`symbol\"string\"",
        &.{ .symbol_literal, .string_literal },
        &.{ .symbol_literal, .apply_unary, .string_literal },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol\"string\"",
        &.{ .symbol_literal, .symbol_literal, .string_literal },
        &.{ .symbol_list_literal, .apply_unary, .string_literal },
    ); // symbol_list_literal
    try testAst(
        "x\"string\"",
        &.{ .identifier, .string_literal },
        &.{ .identifier, .apply_unary, .string_literal },
    ); // identifier
}

test "symbol literals whitespace" {
    try testAst(
        "`symbol(`symbol;)`symbol",
        &.{ .symbol_literal, .l_paren, .symbol_literal, .semicolon, .r_paren, .symbol_literal },
        &.{
            .symbol_literal, .apply_unary, .list, .symbol_literal, .empty, .apply_unary, .symbol_literal,
        },
    ); // l_paren/r_paren
    try testAst(
        "`symbol{`symbol}`symbol",
        &.{ .symbol_literal, .l_brace, .symbol_literal, .r_brace, .symbol_literal },
        &.{
            .symbol_literal, .apply_unary, .lambda, .symbol_literal, .apply_unary, .symbol_literal,
        },
    ); // r_paren/r_brace
    try testAst(
        "[`symbol;]`symbol",
        &.{ .l_bracket, .symbol_literal, .semicolon, .r_bracket, .symbol_literal },
        &.{ .expr_block, .symbol_literal, .empty, .apply_unary, .symbol_literal },
    ); // r_bracket
    try testAst(
        "1`symbol",
        &.{ .number_literal, .symbol_literal },
        &.{ .number_literal, .apply_unary, .symbol_literal },
    ); // number_literal
    try testAst(
        "\"string\"`symbol",
        &.{ .string_literal, .symbol_literal },
        &.{ .string_literal, .apply_unary, .symbol_literal },
    ); // string_literal
    try testAst(
        "`symbol `symbol",
        &.{ .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .apply_unary, .symbol_literal },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol `symbol",
        &.{ .symbol_literal, .symbol_literal, .symbol_literal },
        &.{ .symbol_list_literal, .apply_unary, .symbol_literal },
    ); // symbol_list_literal
    try testAst(
        "x`symbol",
        &.{ .identifier, .symbol_literal },
        &.{ .identifier, .apply_unary, .symbol_literal },
    ); // identifier

    try testAst(
        "`_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
    );
    try testAst(
        "`_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
    );
    try testAstMode(
        .k,
        "`a_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
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
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
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
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
    );
    try testAstMode(
        .q,
        "`a_ `",
        &.{ .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .apply_unary, .symbol_literal },
    );
    try testAstModeRender(
        .k,
        "`a_ `a",
        "`a_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
    );
    try testAstMode(
        .q,
        "`a_ `a",
        &.{ .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .apply_unary, .symbol_literal },
    );
    try testAstModeRender(
        .k,
        "`a _`",
        "`a_`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
    );
    try testAstMode(
        .q,
        "`a _`",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
    );
    try testAstModeRender(
        .k,
        "`a _`a",
        "`a_`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
    );
    try testAstMode(
        .q,
        "`a _`a",
        &.{ .symbol_literal, .underscore, .symbol_literal },
        &.{ .symbol_literal, .apply_binary, .underscore, .symbol_literal },
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
            .symbol_list_literal, .apply_unary, .list, .symbol_list_literal, .empty, .apply_unary, .symbol_list_literal,
        },
    ); // l_paren/r_paren
    try testAst(
        "`symbol`symbol{`symbol`symbol}`symbol`symbol",
        &.{
            .symbol_literal, .symbol_literal, .l_brace,        .symbol_literal,
            .symbol_literal, .r_brace,        .symbol_literal, .symbol_literal,
        },
        &.{
            .symbol_list_literal, .apply_unary, .lambda, .symbol_list_literal, .apply_unary, .symbol_list_literal,
        },
    ); // l_brace/r_brace
    try testAst(
        "[`symbol`symbol;]`symbol`symbol",
        &.{
            .l_bracket, .symbol_literal, .symbol_literal, .semicolon, .r_bracket, .symbol_literal, .symbol_literal,
        },
        &.{ .expr_block, .symbol_list_literal, .empty, .apply_unary, .symbol_list_literal },
    ); // r_bracket
    try testAst(
        "1`symbol`symbol",
        &.{ .number_literal, .symbol_literal, .symbol_literal },
        &.{ .number_literal, .apply_unary, .symbol_list_literal },
    ); // number_literal
    try testAst(
        "\"string\"`symbol`symbol",
        &.{ .string_literal, .symbol_literal, .symbol_literal },
        &.{ .string_literal, .apply_unary, .symbol_list_literal },
    ); // string_literal
    try testAst(
        "`symbol `symbol`symbol",
        &.{ .symbol_literal, .symbol_literal, .symbol_literal },
        &.{ .symbol_literal, .apply_unary, .symbol_list_literal },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol `symbol`symbol",
        &.{ .symbol_literal, .symbol_literal, .symbol_literal, .symbol_literal },
        &.{ .symbol_list_literal, .apply_unary, .symbol_list_literal },
    ); // symbol_list_literal
    try testAst(
        "x`symbol`symbol",
        &.{ .identifier, .symbol_literal, .symbol_literal },
        &.{ .identifier, .apply_unary, .symbol_list_literal },
    ); // identifier
}

test "identifiers whitespace" {
    try testAst(
        "x(x;)x",
        &.{ .identifier, .l_paren, .identifier, .semicolon, .r_paren, .identifier },
        &.{ .identifier, .apply_unary, .list, .identifier, .empty, .apply_unary, .identifier },
    ); // l_paren/r_paren
    try testAst(
        "x{x}x",
        &.{ .identifier, .l_brace, .identifier, .r_brace, .identifier },
        &.{ .identifier, .apply_unary, .lambda, .identifier, .apply_unary, .identifier },
    ); // l_brace/r_brace
    try testAst(
        "[x;]x",
        &.{ .l_bracket, .identifier, .semicolon, .r_bracket, .identifier },
        &.{ .expr_block, .identifier, .empty, .apply_unary, .identifier },
    ); // r_bracket
    try testAst(
        "1 x",
        &.{ .number_literal, .identifier },
        &.{ .number_literal, .apply_unary, .identifier },
    ); // number_literal
    try testAst(
        "\"string\"x",
        &.{ .string_literal, .identifier },
        &.{ .string_literal, .apply_unary, .identifier },
    ); // string_literal
    try testAst(
        "`symbol x",
        &.{ .symbol_literal, .identifier },
        &.{ .symbol_literal, .apply_unary, .identifier },
    ); // symbol_literal
    try testAst(
        "`symbol`symbol x",
        &.{ .symbol_literal, .symbol_literal, .identifier },
        &.{ .symbol_list_literal, .apply_unary, .identifier },
    ); // symbol_list_literal
    try testAst(
        "x x",
        &.{ .identifier, .identifier },
        &.{ .identifier, .apply_unary, .identifier },
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
        &.{ .lambda, .identifier, .call, .empty },
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
            .lambda, .identifier, .apply_binary, .plus, .identifier, .call, .number_literal, .identifier,
        },
    );
    try testAst(
        "{1}[]-1",
        &.{ .l_brace, .number_literal, .r_brace, .l_bracket, .r_bracket, .minus, .number_literal },
        &.{ .lambda, .number_literal, .call, .empty, .apply_binary, .minus, .number_literal },
    );
    try testAst(
        "{x+y}[1] -1",
        &.{
            .l_brace,   .identifier,     .plus,      .identifier,     .r_brace,
            .l_bracket, .number_literal, .r_bracket, .number_literal,
        },
        &.{
            .lambda, .identifier,     .apply_binary, .plus,           .identifier,
            .call,   .number_literal, .apply_unary,  .number_literal,
        },
    );
    try testAst(
        "{x}[a;]",
        &.{ .l_brace, .identifier, .r_brace, .l_bracket, .identifier, .semicolon, .r_bracket },
        &.{ .lambda, .identifier, .call, .identifier, .empty },
    );

    try testAst(
        "if[a;b]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier },
    );

    try testAst(
        \\f[1;
        \\  2;3;
        \\  4;5;6]
    ,
        &.{
            .identifier,     .l_bracket, .number_literal, .semicolon, .number_literal, .semicolon,
            .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .semicolon,
            .number_literal, .r_bracket,
        },
        &.{
            .identifier,     .call,           .number_literal, .number_literal,
            .number_literal, .number_literal, .number_literal, .number_literal,
        },
    );

    try testAst(
        \\f[1;2;
        \\  3;4;5;
        \\  6]
    ,
        &.{
            .identifier,     .l_bracket, .number_literal, .semicolon, .number_literal, .semicolon,
            .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .semicolon,
            .number_literal, .r_bracket,
        },
        &.{
            .identifier,     .call,           .number_literal, .number_literal,
            .number_literal, .number_literal, .number_literal, .number_literal,
        },
    );
}

test "projection" {
    try testAst(
        "{x+y}[;]",
        &.{ .l_brace, .identifier, .plus, .identifier, .r_brace, .l_bracket, .semicolon, .r_bracket },
        &.{ .lambda, .identifier, .apply_binary, .plus, .identifier, .call, .empty, .empty },
    );
    try testAst(
        "{x+y}[1;]",
        &.{
            .l_brace, .identifier, .plus, .identifier, .r_brace, .l_bracket, .number_literal, .semicolon, .r_bracket,
        },
        &.{ .lambda, .identifier, .apply_binary, .plus, .identifier, .call, .number_literal, .empty },
    );
    try testAst(
        "{x+y}[;2]",
        &.{
            .l_brace, .identifier, .plus, .identifier, .r_brace, .l_bracket, .semicolon, .number_literal, .r_bracket,
        },
        &.{ .lambda, .identifier, .apply_binary, .plus, .identifier, .call, .empty, .number_literal },
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
            .identifier,   .apply_binary, .colon,       .lambda,     .identifier, .identifier,   .identifier,
            .apply_binary, .colon,        .lambda,      .identifier, .identifier, .identifier,   .apply_binary,
            .colon,        .lambda,       .identifier,  .identifier, .identifier, .apply_binary, .plus,
            .identifier,   .colon,        .apply_unary, .identifier, .call,       .identifier,   .identifier,
            .empty,        .identifier,   .call,        .identifier, .identifier,
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
    try testAst(
        \\[
        \\  ]
    , &.{ .l_bracket, .r_bracket }, &.{.expr_block});

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
        \\  item1;;testing123;
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
        \\  item1;foo;testing123;identifier]
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
        \\[item1;foo;testing123;identifier]
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
        \\[item1;
        \\  ;
        \\  testing123;
        \\  ]
    ,
        &.{ .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .expr_block, .identifier, .empty, .identifier, .empty },
    );
    try testAstRender(
        \\[item1 ; foo ; testing123 ;
        \\ identifier ]
    ,
        \\[item1;
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
        \\[item1;
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
        \\[item1;
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
        \\[item1;
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
        \\[item1;
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
        \\[item1;
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
        \\[item1;
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
        \\[item1;
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
        \\[item1;
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
        \\[item1;
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
        \\[item1;
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

test "select/exec/update/delete with commas" {
    try testAst(
        "select(a,b),c by(d,e),f from x where(g,h),i",
        &.{
            .keyword_select, .l_paren,    .identifier,     .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,     .l_paren,    .identifier,     .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,     .identifier, .prefix_builtin, .l_paren, .identifier, .comma,   .identifier, .r_paren,
            .comma,          .identifier,
        },
        &.{
            .select,     .grouped_expression, .identifier,         .apply_binary, .comma,        .identifier,
            .identifier, .grouped_expression, .identifier,         .apply_binary, .comma,        .identifier,
            .identifier, .identifier,         .grouped_expression, .identifier,   .apply_binary, .comma,
            .identifier, .identifier,
        },
    );
    try testAst(
        "exec(a,b),c by(d,e),f from x where(g,h),i",
        &.{
            .keyword_exec, .l_paren,    .identifier,     .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,   .l_paren,    .identifier,     .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,   .identifier, .prefix_builtin, .l_paren, .identifier, .comma,   .identifier, .r_paren,
            .comma,        .identifier,
        },
        &.{
            .exec,       .grouped_expression, .identifier,         .apply_binary, .comma,        .identifier,
            .identifier, .grouped_expression, .identifier,         .apply_binary, .comma,        .identifier,
            .identifier, .identifier,         .grouped_expression, .identifier,   .apply_binary, .comma,
            .identifier, .identifier,
        },
    );
    try testAst(
        "update(a,b),c by(d,e),f from x where(g,h),i",
        &.{
            .keyword_update, .l_paren,    .identifier,     .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,     .l_paren,    .identifier,     .comma,   .identifier, .r_paren, .comma,      .identifier,
            .identifier,     .identifier, .prefix_builtin, .l_paren, .identifier, .comma,   .identifier, .r_paren,
            .comma,          .identifier,
        },
        &.{
            .update,     .grouped_expression, .identifier,         .apply_binary, .comma,        .identifier,
            .identifier, .grouped_expression, .identifier,         .apply_binary, .comma,        .identifier,
            .identifier, .identifier,         .grouped_expression, .identifier,   .apply_binary, .comma,
            .identifier, .identifier,
        },
    );
    try testAst(
        "delete from x where(g,h),i",
        &.{
            .keyword_delete, .identifier, .identifier, .prefix_builtin, .l_paren,    .identifier,
            .comma,          .identifier, .r_paren,    .comma,          .identifier,
        },
        &.{
            .delete_rows,  .identifier, .grouped_expression, .identifier,
            .apply_binary, .comma,      .identifier,         .identifier,
        },
    );
    try failAst(
        "delete(a,b),c from x",
        &.{
            .keyword_delete, .l_paren, .identifier, .comma,      .identifier,
            .r_paren,        .comma,   .identifier, .identifier, .identifier,
        },
        &.{.expected_token},
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
        "select first a from x",
        &.{ .keyword_select, .prefix_builtin, .identifier, .identifier, .identifier },
        &.{ .select, .builtin, .apply_unary, .identifier, .identifier },
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
        &.{ .keyword_select, .identifier, .identifier, .prefix_builtin, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select from x where e,f",
        &.{
            .keyword_select, .identifier, .identifier, .prefix_builtin, .identifier, .comma, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a from x where e",
        &.{ .keyword_select, .identifier, .identifier, .identifier, .prefix_builtin, .identifier },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a,b from x where e,f",
        &.{
            .keyword_select, .identifier,     .comma,      .identifier, .identifier,
            .identifier,     .prefix_builtin, .identifier, .comma,      .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select by from x where e,f",
        &.{
            .keyword_select, .identifier, .identifier, .identifier, .prefix_builtin, .identifier, .comma, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a by from x where e",
        &.{
            .keyword_select, .identifier, .identifier, .identifier, .identifier, .prefix_builtin, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a,b by from x where e,f",
        &.{
            .keyword_select, .identifier,     .comma,      .identifier, .identifier, .identifier,
            .identifier,     .prefix_builtin, .identifier, .comma,      .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select by c from x where e",
        &.{
            .keyword_select, .identifier, .identifier, .identifier, .identifier, .prefix_builtin, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select by c,d from x where e,f",
        &.{
            .keyword_select, .identifier,     .identifier, .comma, .identifier, .identifier,
            .identifier,     .prefix_builtin, .identifier, .comma, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a by c from x where e",
        &.{
            .keyword_select, .identifier, .identifier,     .identifier,
            .identifier,     .identifier, .prefix_builtin, .identifier,
        },
        &.{ .select, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "select a,b by c,d from x where e,f",
        &.{
            .keyword_select, .identifier, .comma,      .identifier,     .identifier, .identifier, .comma,
            .identifier,     .identifier, .identifier, .prefix_builtin, .identifier, .comma,      .identifier,
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
        &.{ .keyword_select, .prefix_builtin, .identifier, .identifier },
        &.{ .select, .identifier },
    );
    try testAst(
        "select distinct a from x",
        &.{ .keyword_select, .prefix_builtin, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select distinct by from x",
        &.{ .keyword_select, .prefix_builtin, .identifier, .identifier, .identifier },
        &.{ .select, .identifier },
    );
    try testAst(
        "select distinct a by from x",
        &.{ .keyword_select, .prefix_builtin, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select distinct by b from x",
        &.{ .keyword_select, .prefix_builtin, .identifier, .identifier, .identifier, .identifier },
        &.{ .select, .identifier, .identifier },
    );
    try testAst(
        "select distinct a by b from x",
        &.{
            .keyword_select, .prefix_builtin, .identifier, .identifier, .identifier, .identifier, .identifier,
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
            .keyword_select, .l_bracket, .number_literal, .r_bracket, .prefix_builtin, .identifier, .identifier,
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
            .select, .number_literal, .number_literal, .apply_binary, .asterisk, .identifier, .identifier,
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
            .select, .identifier, .number_literal, .apply_binary, .asterisk, .identifier, .identifier,
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
            .select, .number_literal, .number_literal, .apply_binary, .asterisk, .identifier, .identifier,
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
            .select, .identifier, .number_literal, .apply_binary, .asterisk, .identifier, .identifier,
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
        &.{ .select, .number_literal, .apply_binary, .asterisk, .identifier, .identifier },
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
        &.{ .select, .number_literal, .apply_binary, .asterisk, .identifier, .identifier },
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
        &.{ .select, .number_literal, .apply_binary, .asterisk, .identifier, .identifier },
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
        &.{ .select, .number_literal, .apply_binary, .asterisk, .identifier, .identifier },
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
        &.{ .select, .number_literal, .apply_binary, .asterisk, .identifier, .identifier },
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
        &.{ .select, .number_literal, .apply_binary, .asterisk, .identifier, .identifier },
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
        &.{ .select, .number_literal, .apply_binary, .asterisk, .identifier, .identifier },
    );

    try testAst(
        "select`a by`b from`c where`d",
        &.{
            .keyword_select, .symbol_literal, .identifier,     .symbol_literal,
            .identifier,     .symbol_literal, .prefix_builtin, .symbol_literal,
        },
        &.{ .select, .symbol_literal, .symbol_literal, .symbol_literal, .symbol_literal },
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
        "exec first a from x",
        &.{ .keyword_exec, .prefix_builtin, .identifier, .identifier, .identifier },
        &.{ .exec, .builtin, .apply_unary, .identifier, .identifier },
    );
    try testAst(
        "exec a:a from x",
        &.{ .keyword_exec, .identifier, .colon, .identifier, .identifier, .identifier },
        &.{ .exec, .identifier, .apply_binary, .colon, .identifier, .identifier },
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
        &.{ .exec, .identifier, .apply_binary, .colon, .identifier, .identifier },
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
        &.{ .exec, .identifier, .apply_binary, .colon, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a by c:c from x",
        &.{
            .keyword_exec, .identifier, .identifier, .identifier, .colon, .identifier, .identifier, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .apply_binary, .colon, .identifier, .identifier },
    );
    try testAst(
        "exec a:a by c:c from x",
        &.{
            .keyword_exec, .identifier, .colon,      .identifier, .identifier,
            .identifier,   .colon,      .identifier, .identifier, .identifier,
        },
        &.{
            .exec,       .identifier,   .apply_binary, .colon,      .identifier,
            .identifier, .apply_binary, .colon,        .identifier, .identifier,
        },
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
        &.{ .keyword_exec, .identifier, .identifier, .prefix_builtin, .identifier },
        &.{ .exec, .identifier, .identifier },
    );
    try testAst(
        "exec from x where e,f",
        &.{
            .keyword_exec, .identifier, .identifier, .prefix_builtin, .identifier, .comma, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a from x where e",
        &.{ .keyword_exec, .identifier, .identifier, .identifier, .prefix_builtin, .identifier },
        &.{ .exec, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a:a from x where e",
        &.{
            .keyword_exec, .identifier, .colon, .identifier, .identifier, .identifier, .prefix_builtin, .identifier,
        },
        &.{ .exec, .identifier, .apply_binary, .colon, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a,b from x where e,f",
        &.{
            .keyword_exec, .identifier,     .comma,      .identifier, .identifier,
            .identifier,   .prefix_builtin, .identifier, .comma,      .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec by c from x where e",
        &.{
            .keyword_exec, .identifier, .identifier, .identifier, .identifier, .prefix_builtin, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec by c:c from x where e",
        &.{
            .keyword_exec, .identifier, .identifier,     .colon,      .identifier,
            .identifier,   .identifier, .prefix_builtin, .identifier,
        },
        &.{ .exec, .identifier, .apply_binary, .colon, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec by c,d from x where e,f",
        &.{
            .keyword_exec, .identifier,     .identifier, .comma, .identifier, .identifier,
            .identifier,   .prefix_builtin, .identifier, .comma, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a by c from x where e",
        &.{
            .keyword_exec, .identifier, .identifier,     .identifier,
            .identifier,   .identifier, .prefix_builtin, .identifier,
        },
        &.{ .exec, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "exec a:a by c from x where e",
        &.{
            .keyword_exec, .identifier, .colon,      .identifier,     .identifier,
            .identifier,   .identifier, .identifier, .prefix_builtin, .identifier,
        },
        &.{
            .exec, .identifier, .apply_binary, .colon, .identifier, .identifier, .identifier, .identifier,
        },
    );
    try testAst(
        "exec a by c:c from x where e",
        &.{
            .keyword_exec, .identifier, .identifier, .identifier,     .colon,
            .identifier,   .identifier, .identifier, .prefix_builtin, .identifier,
        },
        &.{
            .exec, .identifier, .identifier, .apply_binary, .colon, .identifier, .identifier, .identifier,
        },
    );
    try testAst(
        "exec a:a by c:c from x where e",
        &.{
            .keyword_exec, .identifier, .colon,      .identifier, .identifier,     .identifier,
            .colon,        .identifier, .identifier, .identifier, .prefix_builtin, .identifier,
        },
        &.{
            .exec,         .identifier, .apply_binary, .colon,      .identifier, .identifier,
            .apply_binary, .colon,      .identifier,   .identifier, .identifier,
        },
    );
    try testAst(
        "exec a,b by c,d from x where e,f",
        &.{
            .keyword_exec, .identifier, .comma,      .identifier,     .identifier, .identifier, .comma,
            .identifier,   .identifier, .identifier, .prefix_builtin, .identifier, .comma,      .identifier,
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

    try testAst(
        "exec`a by`b from`c where`d",
        &.{
            .keyword_exec, .symbol_literal, .identifier,     .symbol_literal,
            .identifier,   .symbol_literal, .prefix_builtin, .symbol_literal,
        },
        &.{ .exec, .symbol_literal, .symbol_literal, .symbol_literal, .symbol_literal },
    );
}

test "update" {
    try testAst(
        "update a from x",
        &.{ .keyword_update, .identifier, .identifier, .identifier },
        &.{ .update, .identifier, .identifier },
    );
    try testAst(
        "update first a from x",
        &.{ .keyword_update, .prefix_builtin, .identifier, .identifier, .identifier },
        &.{ .update, .builtin, .apply_unary, .identifier, .identifier },
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
        &.{ .keyword_update, .identifier, .identifier, .identifier, .prefix_builtin, .identifier },
        &.{ .update, .identifier, .identifier, .identifier },
    );
    try testAst(
        "update a,b from x where e,f",
        &.{
            .keyword_update, .identifier,     .comma,      .identifier, .identifier,
            .identifier,     .prefix_builtin, .identifier, .comma,      .identifier,
        },
        &.{ .update, .identifier, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "update a by c from x where e",
        &.{
            .keyword_update, .identifier, .identifier,     .identifier,
            .identifier,     .identifier, .prefix_builtin, .identifier,
        },
        &.{ .update, .identifier, .identifier, .identifier, .identifier },
    );
    try testAst(
        "update a,b by c,d from x where e,f",
        &.{
            .keyword_update, .identifier, .comma,      .identifier,     .identifier, .identifier, .comma,
            .identifier,     .identifier, .identifier, .prefix_builtin, .identifier, .comma,      .identifier,
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

    try testAst(
        "update`a by`b from`c where`d",
        &.{
            .keyword_update, .symbol_literal, .identifier,     .symbol_literal,
            .identifier,     .symbol_literal, .prefix_builtin, .symbol_literal,
        },
        &.{ .update, .symbol_literal, .symbol_literal, .symbol_literal, .symbol_literal },
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
        &.{ .keyword_delete, .identifier, .identifier, .prefix_builtin, .identifier },
        &.{ .delete_rows, .identifier, .identifier },
    );
    try testAst(
        "delete from x where a,b",
        &.{
            .keyword_delete, .identifier, .identifier, .prefix_builtin, .identifier, .comma, .identifier,
        },
        &.{ .delete_rows, .identifier, .identifier, .identifier },
    );

    try testAst(
        "delete from`a where`b",
        &.{ .keyword_delete, .identifier, .symbol_literal, .prefix_builtin, .symbol_literal },
        &.{ .delete_rows, .symbol_literal, .symbol_literal },
    );
}

test "delete columns" {
    try testAst(
        "delete a from x",
        &.{ .keyword_delete, .identifier, .identifier, .identifier },
        &.{ .delete_cols, .identifier },
    );
    try failAst(
        "delete first a from x",
        &.{ .keyword_delete, .prefix_builtin, .identifier, .identifier, .identifier },
        &.{.expected_token},
    );
    try testAst(
        "delete a,b from x",
        &.{ .keyword_delete, .identifier, .comma, .identifier, .identifier, .identifier },
        &.{ .delete_cols, .identifier },
    );
    try failAst(
        "delete a from x where b",
        &.{ .keyword_delete, .identifier, .identifier, .identifier, .prefix_builtin, .identifier },
        &.{.cannot_define_where_cond_in_delete_cols},
    );

    try testAst(
        "delete a from`b",
        &.{ .keyword_delete, .identifier, .identifier, .symbol_literal },
        &.{ .delete_cols, .symbol_literal },
    );
}

test "do" {
    try testAst(
        "do a",
        &.{ .identifier, .identifier },
        &.{ .identifier, .apply_unary, .identifier },
    );
    try testAst(
        "do[a]",
        &.{ .identifier, .l_bracket, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier },
    );
    try testAst(
        "do[a;]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .r_bracket },
        &.{ .identifier, .call, .identifier, .empty },
    );
    try testAst(
        "do[a;b]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier },
    );
    try testAst(
        "do[a;b;]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier, .empty },
    );
    try testAst(
        "do[a;b;c]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier, .identifier },
    );
    try testAst(
        "do[a;b;;c]",
        &.{
            .identifier, .l_bracket, .identifier, .semicolon, .identifier,
            .semicolon,  .semicolon, .identifier, .r_bracket,
        },
        &.{ .identifier, .call, .identifier, .identifier, .empty, .identifier },
    );
}

test "if" {
    try testAst(
        "if a",
        &.{ .identifier, .identifier },
        &.{ .identifier, .apply_unary, .identifier },
    );
    try testAst(
        "if[a]",
        &.{ .identifier, .l_bracket, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier },
    );
    try testAst(
        "if[a;]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .r_bracket },
        &.{ .identifier, .call, .identifier, .empty },
    );
    try testAst(
        "if[a;b]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier },
    );
    try testAst(
        "if[a;b;]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier, .empty },
    );
    try testAst(
        "if[a;b;c]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier, .identifier },
    );
    try testAst(
        "if[a;b;;c]",
        &.{
            .identifier, .l_bracket, .identifier, .semicolon, .identifier,
            .semicolon,  .semicolon, .identifier, .r_bracket,
        },
        &.{ .identifier, .call, .identifier, .identifier, .empty, .identifier },
    );
}

test "while" {
    try testAst(
        "while a",
        &.{ .identifier, .identifier },
        &.{ .identifier, .apply_unary, .identifier },
    );
    try testAst(
        "while[a]",
        &.{ .identifier, .l_bracket, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier },
    );
    try testAst(
        "while[a;]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .r_bracket },
        &.{ .identifier, .call, .identifier, .empty },
    );
    try testAst(
        "while[a;b]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier },
    );
    try testAst(
        "while[a;b;]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier, .empty },
    );
    try testAst(
        "while[a;b;c]",
        &.{ .identifier, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier, .identifier, .identifier },
    );
    try testAst(
        "while[a;b;;c]",
        &.{
            .identifier, .l_bracket, .identifier, .semicolon, .identifier,
            .semicolon,  .semicolon, .identifier, .r_bracket,
        },
        &.{ .identifier, .call, .identifier, .identifier, .empty, .identifier },
    );
}

test "cond" {
    try testAst(
        "$[]",
        &.{ .dollar, .l_bracket, .r_bracket },
        &.{ .dollar, .call, .empty },
    );
    try testAst(
        "$[x]",
        &.{ .dollar, .l_bracket, .identifier, .r_bracket },
        &.{ .dollar, .call, .identifier },
    );
    try testAst(
        "$[;]",
        &.{ .dollar, .l_bracket, .semicolon, .r_bracket },
        &.{ .dollar, .call, .empty, .empty },
    );
    try testAst(
        "$[;y]",
        &.{ .dollar, .l_bracket, .semicolon, .identifier, .r_bracket },
        &.{ .dollar, .call, .empty, .identifier },
    );
    try testAst(
        "$[x;]",
        &.{ .dollar, .l_bracket, .identifier, .semicolon, .r_bracket },
        &.{ .dollar, .call, .identifier, .empty },
    );
    try testAst(
        "$[x;y]",
        &.{ .dollar, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .dollar, .call, .identifier, .identifier },
    );
    try testAst(
        "$[;;]",
        &.{ .dollar, .l_bracket, .semicolon, .semicolon, .r_bracket },
        &.{ .dollar, .call, .empty, .empty, .empty },
    );
    try testAst(
        "$[;;z]",
        &.{ .dollar, .l_bracket, .semicolon, .semicolon, .identifier, .r_bracket },
        &.{ .dollar, .call, .empty, .empty, .identifier },
    );
    try testAst(
        "$[;y;]",
        &.{ .dollar, .l_bracket, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .dollar, .call, .empty, .identifier, .empty },
    );
    try testAst(
        "$[;y;z]",
        &.{ .dollar, .l_bracket, .semicolon, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .dollar, .call, .empty, .identifier, .identifier },
    );
    try testAst(
        "$[x;;]",
        &.{ .dollar, .l_bracket, .identifier, .semicolon, .semicolon, .r_bracket },
        &.{ .dollar, .call, .identifier, .empty, .empty },
    );
    try testAst(
        "$[x;;z]",
        &.{ .dollar, .l_bracket, .identifier, .semicolon, .semicolon, .identifier, .r_bracket },
        &.{ .dollar, .call, .identifier, .empty, .identifier },
    );
    try testAst(
        "$[x;y;]",
        &.{ .dollar, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .r_bracket },
        &.{ .dollar, .call, .identifier, .identifier, .empty },
    );
    try testAst(
        "$[x;y;z]",
        &.{ .dollar, .l_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .dollar, .call, .identifier, .identifier, .identifier },
    );
}

test "apply unary" {
    try testAst(
        "+[1;2]",
        &.{ .plus, .l_bracket, .number_literal, .semicolon, .number_literal, .r_bracket },
        &.{ .plus, .call, .number_literal, .number_literal },
    );
    try testAst(
        "1++[1;2]",
        &.{
            .number_literal, .plus, .plus, .l_bracket, .number_literal, .semicolon, .number_literal, .r_bracket,
        },
        &.{ .number_literal, .apply_binary, .plus, .plus, .call, .number_literal, .number_literal },
    );
    try failAstMode(
        .q,
        "1++:2",
        &.{ .number_literal, .plus, .plus_colon, .number_literal },
        &.{.cannot_apply_operator_directly},
    );
    try testAstMode(
        .k,
        "1++:2",
        &.{ .number_literal, .plus, .plus_colon, .number_literal },
        &.{ .number_literal, .apply_binary, .plus, .plus_colon, .apply_unary, .number_literal },
    );
    try testAst(
        "a:[]",
        &.{ .identifier, .colon, .l_bracket, .r_bracket },
        &.{ .identifier, .apply_unary, .colon, .call, .empty },
    );
    try testAst(
        \\'"signal"
    ,
        &.{ .apostrophe, .string_literal },
        &.{ .apostrophe, .apply_unary, .string_literal },
    );
    try testAst(
        "'`signal",
        &.{ .apostrophe, .symbol_literal },
        &.{ .apostrophe, .apply_unary, .symbol_literal },
    );
    try testAst(
        "'signal",
        &.{ .apostrophe, .identifier },
        &.{ .apostrophe, .apply_unary, .identifier },
    );
    try failAstMode(
        .q,
        "f'x",
        &.{ .identifier, .apostrophe, .identifier },
        &.{.cannot_apply_iterator_directly},
    );
    try testAstMode(
        .k,
        "f'x",
        &.{ .identifier, .apostrophe, .identifier },
        &.{ .identifier, .apostrophe, .apply_unary, .identifier },
    );
}

test "apply builtins" {
    try testAst(
        "f x",
        &.{ .identifier, .identifier },
        &.{ .identifier, .apply_unary, .identifier },
    );
    try testAst(
        "first x",
        &.{ .prefix_builtin, .identifier },
        &.{ .builtin, .apply_unary, .identifier },
    );

    try testAst(
        "f[x]",
        &.{ .identifier, .l_bracket, .identifier, .r_bracket },
        &.{ .identifier, .call, .identifier },
    );
    try testAst(
        "first[x]",
        &.{ .prefix_builtin, .l_bracket, .identifier, .r_bracket },
        &.{ .builtin, .call, .identifier },
    );

    try testAst(
        "f each x",
        &.{ .identifier, .infix_builtin, .identifier },
        &.{ .identifier, .apply_binary, .builtin, .identifier },
    );
    try testAst(
        "first each x",
        &.{ .prefix_builtin, .infix_builtin, .identifier },
        &.{ .builtin, .apply_binary, .builtin, .identifier },
    );

    try testAst(
        "each[f;x]",
        &.{ .infix_builtin, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .builtin, .call, .identifier, .identifier },
    );
    try testAst(
        "each[first;x]",
        &.{ .infix_builtin, .l_bracket, .prefix_builtin, .semicolon, .identifier, .r_bracket },
        &.{ .builtin, .call, .builtin, .identifier },
    );

    try testAst(
        "f[x],/:y",
        &.{ .identifier, .l_bracket, .identifier, .r_bracket, .comma, .slash_colon, .identifier },
        &.{ .identifier, .call, .identifier, .apply_binary, .comma, .slash_colon, .identifier },
    );
    try testAst(
        "first[x],/:y",
        &.{ .prefix_builtin, .l_bracket, .identifier, .r_bracket, .comma, .slash_colon, .identifier },
        &.{ .builtin, .call, .identifier, .apply_binary, .comma, .slash_colon, .identifier },
    );

    try testAst(
        "f[x],/:f y",
        &.{
            .identifier, .l_bracket, .identifier, .r_bracket, .comma, .slash_colon, .identifier, .identifier,
        },
        &.{
            .identifier,  .call,       .identifier,  .apply_binary, .comma,
            .slash_colon, .identifier, .apply_unary, .identifier,
        },
    );
    try testAst(
        "first[x],/:first y",
        &.{
            .prefix_builtin, .l_bracket, .identifier, .r_bracket, .comma, .slash_colon, .prefix_builtin, .identifier,
        },
        &.{
            .builtin, .call, .identifier, .apply_binary, .comma, .slash_colon, .builtin, .apply_unary, .identifier,
        },
    );

    try testAst(
        "f g[x],/:y",
        &.{
            .identifier, .identifier, .l_bracket, .identifier, .r_bracket, .comma, .slash_colon, .identifier,
        },
        &.{
            .identifier,   .apply_unary, .identifier,  .call,       .identifier,
            .apply_binary, .comma,       .slash_colon, .identifier,
        },
    );
    try testAst(
        "f first[x],/:y",
        &.{
            .identifier, .prefix_builtin, .l_bracket, .identifier, .r_bracket, .comma, .slash_colon, .identifier,
        },
        &.{
            .identifier, .apply_unary, .builtin, .call, .identifier, .apply_binary, .comma, .slash_colon, .identifier,
        },
    );

    try testAst(
        "f g[x],/:f y",
        &.{
            .identifier, .identifier,  .l_bracket,  .identifier, .r_bracket,
            .comma,      .slash_colon, .identifier, .identifier,
        },
        &.{
            .identifier, .apply_unary, .identifier, .call,        .identifier, .apply_binary,
            .comma,      .slash_colon, .identifier, .apply_unary, .identifier,
        },
    );
    try testAst(
        "f first[x],/:first y",
        &.{
            .identifier, .prefix_builtin, .l_bracket,      .identifier, .r_bracket,
            .comma,      .slash_colon,    .prefix_builtin, .identifier,
        },
        &.{
            .identifier, .apply_unary, .builtin, .call,        .identifier, .apply_binary,
            .comma,      .slash_colon, .builtin, .apply_unary, .identifier,
        },
    );

    try testAst(
        "f x@'y",
        &.{ .identifier, .identifier, .at, .apostrophe, .identifier },
        &.{ .identifier, .apply_unary, .identifier, .apply_binary, .at, .apostrophe, .identifier },
    );
    try testAst(
        "f first@'y",
        &.{ .identifier, .prefix_builtin, .at, .apostrophe, .identifier },
        &.{ .identifier, .apply_unary, .builtin, .apply_binary, .at, .apostrophe, .identifier },
    );
    try testAst(
        "sum first@'y",
        &.{ .prefix_builtin, .prefix_builtin, .at, .apostrophe, .identifier },
        &.{ .builtin, .apply_unary, .builtin, .apply_binary, .at, .apostrophe, .identifier },
    );

    try testAst(
        "f each[x;y]",
        &.{ .identifier, .infix_builtin, .l_bracket, .identifier, .semicolon, .identifier, .r_bracket },
        &.{ .identifier, .apply_unary, .builtin, .call, .identifier, .identifier },
    );
    try testAst(
        "f each[x]y",
        &.{ .identifier, .infix_builtin, .l_bracket, .identifier, .r_bracket, .identifier },
        &.{ .identifier, .apply_unary, .builtin, .call, .identifier, .apply_unary, .identifier },
    );

    try testAst(
        "` sv'x",
        &.{ .symbol_literal, .infix_builtin, .apostrophe, .identifier },
        &.{ .symbol_literal, .apply_binary, .builtin, .apostrophe, .identifier },
    );
}

test "normalize empty lines" {
    try testAstRender(
        \\
        \\
        \\x  /comment1
        \\
        \\
        \\y  /comment2
        \\
        \\
        \\z  /comment3
    ,
        \\x /comment1
        \\
        \\y /comment2
        \\
        \\z /comment3
    ,
        &.{ .identifier, .identifier, .identifier },
        &.{ .identifier, .identifier, .identifier },
    );
    try testAstRender(
        \\
        \\
        \\ comment1
        \\x  /comment2
        \\
        \\
        \\/comment3
        \\y  /comment4
        \\
        \\
        \\/comment5
        \\z  /comment6
    ,
        \\ comment1
        \\x /comment2
        \\
        \\/comment3
        \\y /comment4
        \\
        \\/comment5
        \\z /comment6
    ,
        &.{ .identifier, .identifier, .identifier },
        &.{ .identifier, .identifier, .identifier },
    );

    try testAstRender(
        \\
        \\
        \\ comment1
        \\f:{[x;y;z]  /comment2
        \\
        \\
        \\  x;  /comment3
        \\
        \\
        \\  y;  /comment4
        \\
        \\
        \\  z}  /comment5
    ,
        \\ comment1
        \\f:{[x;y;z] /comment2
        \\
        \\  x; /comment3
        \\
        \\  y; /comment4
        \\
        \\  z} /comment5
    ,
        &.{
            .identifier, .colon,     .l_brace,    .l_bracket, .identifier, .semicolon, .identifier, .semicolon,
            .identifier, .r_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_brace,
        },
        &.{
            .identifier, .apply_binary, .colon,      .lambda,     .identifier,
            .identifier, .identifier,   .identifier, .identifier, .identifier,
        },
    );
    try testAstRender(
        \\
        \\
        \\ comment1
        \\f:{[x;y;z]  /comment2
        \\
        \\
        \\  /comment3
        \\  x;  /comment4
        \\
        \\
        \\  /comment5
        \\  y;  /comment6
        \\
        \\
        \\  /comment7
        \\  z}  /comment8
    ,
        \\ comment1
        \\f:{[x;y;z] /comment2
        \\
        \\  /comment3
        \\  x; /comment4
        \\
        \\  /comment5
        \\  y; /comment6
        \\
        \\  /comment7
        \\  z} /comment8
    ,
        &.{
            .identifier, .colon,     .l_brace,    .l_bracket, .identifier, .semicolon, .identifier, .semicolon,
            .identifier, .r_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_brace,
        },
        &.{
            .identifier, .apply_binary, .colon,      .lambda,     .identifier,
            .identifier, .identifier,   .identifier, .identifier, .identifier,
        },
    );

    try testAstRender(
        \\
        \\
        \\ comment1
        \\f:{[x;y;z]
        \\
        \\
        \\  /comment2
        \\  x;  /comment3
        \\
        \\
        \\  /comment4
        \\  y;  /comment5
        \\
        \\
        \\  /comment6
        \\  z}  /comment7
        \\
        \\
        \\/comment8
        \\f:{[x;y;z]
        \\
        \\
        \\  /comment9
        \\  x;  /comment10
        \\
        \\
        \\  /comment11
        \\  y;  /comment12
        \\
        \\
        \\  /comment13
        \\  z}  /comment14
    ,
        \\ comment1
        \\f:{[x;y;z]
        \\
        \\  /comment2
        \\  x; /comment3
        \\
        \\  /comment4
        \\  y; /comment5
        \\
        \\  /comment6
        \\  z} /comment7
        \\
        \\/comment8
        \\f:{[x;y;z]
        \\
        \\  /comment9
        \\  x; /comment10
        \\
        \\  /comment11
        \\  y; /comment12
        \\
        \\  /comment13
        \\  z} /comment14
    ,
        &.{
            .identifier, .colon,     .l_brace,    .l_bracket, .identifier, .semicolon, .identifier, .semicolon,
            .identifier, .r_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_brace,
            .identifier, .colon,     .l_brace,    .l_bracket, .identifier, .semicolon, .identifier, .semicolon,
            .identifier, .r_bracket, .identifier, .semicolon, .identifier, .semicolon, .identifier, .r_brace,
        },
        &.{
            .identifier, .apply_binary, .colon,      .lambda,       .identifier, .identifier, .identifier, .identifier,
            .identifier, .identifier,   .identifier, .apply_binary, .colon,      .lambda,     .identifier, .identifier,
            .identifier, .identifier,   .identifier, .identifier,
        },
    );
}

test "rewrite expressions to reduce parens" {
    if (true) return error.SkipZigTest;
    try testAstRender(
        \\(((a)))
    ,
        \\a
    ,
        &.{ .l_paren, .l_paren, .l_paren, .identifier, .r_paren, .r_paren, .r_paren },
        &.{ .grouped_expression, .grouped_expression, .grouped_expression, .identifier },
    );
    try testAst(
        \\(lower@)a
    ,
        &.{ .l_paren, .prefix_builtin, .at, .r_paren, .identifier },
        &.{ .grouped_expression, .builtin, .apply_binary, .at, .apply_unary, .identifier },
    );
    try testAst(
        \\(string lower@)a
    ,
        &.{ .l_paren, .prefix_builtin, .prefix_builtin, .at, .r_paren, .identifier },
        &.{
            .grouped_expression, .builtin, .apply_unary, .builtin, .apply_binary, .at, .apply_unary, .identifier,
        },
    );
    try testAst(
        \\(lower@)each a
    ,
        &.{ .l_paren, .prefix_builtin, .at, .r_paren, .infix_builtin, .identifier },
        &.{ .grouped_expression, .builtin, .apply_binary, .at, .apply_binary, .builtin, .identifier },
    );
    try testAst(
        \\(string lower@)each a
    ,
        &.{ .l_paren, .prefix_builtin, .prefix_builtin, .at, .r_paren, .infix_builtin, .identifier },
        &.{
            .grouped_expression, .builtin,      .apply_unary, .builtin,    .apply_binary,
            .at,                 .apply_binary, .builtin,     .identifier,
        },
    );
    try testAst(
        \\(" "vs')a
    ,
        &.{ .l_paren, .string_literal, .infix_builtin, .apostrophe, .r_paren, .identifier },
        &.{
            .grouped_expression, .string_literal, .apply_binary, .builtin, .apostrophe, .apply_unary, .identifier,
        },
    );
    try testAst(
        \\(" "vs')each a
    ,
        &.{
            .l_paren, .string_literal, .infix_builtin, .apostrophe, .r_paren, .infix_builtin, .identifier,
        },
        &.{
            .grouped_expression, .string_literal, .apply_binary, .builtin,
            .apostrophe,         .apply_binary,   .builtin,      .identifier,
        },
    );
    try testAstRender(
        \\(((a),'(b)),'(c))
    ,
        \\(a,'b),'c
    ,
        &.{
            .l_paren, .l_paren, .l_paren, .identifier, .r_paren, .comma,      .apostrophe, .l_paren, .identifier,
            .r_paren, .r_paren, .comma,   .apostrophe, .l_paren, .identifier, .r_paren,    .r_paren,
        },
        &.{
            .grouped_expression, .grouped_expression, .grouped_expression, .identifier,   .apply_binary, .comma,
            .apostrophe,         .grouped_expression, .identifier,         .apply_binary, .comma,        .apostrophe,
            .grouped_expression, .identifier,
        },
    );
    try testAstRender(
        \\((a),'((b),'(c)))
    ,
        \\a,'b,'c
    ,
        &.{
            .l_paren, .l_paren, .identifier, .r_paren, .comma,      .apostrophe, .l_paren, .l_paren, .identifier,
            .r_paren, .comma,   .apostrophe, .l_paren, .identifier, .r_paren,    .r_paren, .r_paren,
        },
        &.{
            .grouped_expression, .grouped_expression, .identifier, .apply_binary, .comma, .apostrophe,
            .grouped_expression, .grouped_expression, .identifier, .apply_binary, .comma, .apostrophe,
            .grouped_expression, .identifier,
        },
    );
    try testAstRender(
        \\((string`int$`date$execTime),'(value3)),'(value4)
    ,
        \\((string`int$`date$execTime),'value3),'value4
    ,
        &.{
            .l_paren,    .l_paren, .prefix_builtin, .symbol_literal, .dollar,     .symbol_literal, .dollar,
            .identifier, .r_paren, .comma,          .apostrophe,     .l_paren,    .identifier,     .r_paren,
            .r_paren,    .comma,   .apostrophe,     .l_paren,        .identifier, .r_paren,
        },
        &.{
            .grouped_expression, .grouped_expression, .builtin,            .apply_unary, .symbol_literal, .apply_binary,
            .dollar,             .symbol_literal,     .apply_binary,       .dollar,      .identifier,     .apply_binary,
            .comma,              .apostrophe,         .grouped_expression, .identifier,  .apply_binary,   .comma,
            .apostrophe,         .grouped_expression, .identifier,
        },
    );
    try testAstRender(
        \\(string`int$`date$execTime),'(value3),'(value4)
    ,
        \\(string`int$`date$execTime),'value3,'value4
    ,
        &.{
            .l_paren,    .prefix_builtin, .symbol_literal, .dollar,     .symbol_literal, .dollar, .identifier, .r_paren,
            .comma,      .apostrophe,     .l_paren,        .identifier, .r_paren,        .comma,  .apostrophe, .l_paren,
            .identifier, .r_paren,
        },
        &.{
            .grouped_expression, .builtin,            .apply_unary, .symbol_literal, .apply_binary, .dollar,
            .symbol_literal,     .apply_binary,       .dollar,      .identifier,     .apply_binary, .comma,
            .apostrophe,         .grouped_expression, .identifier,  .apply_binary,   .comma,        .apostrophe,
            .grouped_expression, .identifier,
        },
    );
    try testAstRender(
        \\{(((a)))}
    ,
        \\{a}
    ,
        &.{
            .l_brace, .l_paren, .l_paren, .l_paren, .identifier, .r_paren, .r_paren, .r_paren, .r_brace,
        },
        &.{ .lambda, .grouped_expression, .grouped_expression, .grouped_expression, .identifier },
    );
    try testAstRender(
        \\{(((a),'(b)),'(c))}
    ,
        \\{(a,'b),'c}
    ,
        &.{
            .l_brace,    .l_paren, .l_paren, .l_paren, .identifier, .r_paren, .comma,      .apostrophe, .l_paren,
            .identifier, .r_paren, .r_paren, .comma,   .apostrophe, .l_paren, .identifier, .r_paren,    .r_paren,
            .r_brace,
        },
        &.{
            .lambda,     .grouped_expression, .grouped_expression, .grouped_expression, .identifier,   .apply_binary,
            .comma,      .apostrophe,         .grouped_expression, .identifier,         .apply_binary, .comma,
            .apostrophe, .grouped_expression, .identifier,
        },
    );
    try testAstRender(
        \\{((a),'((b),'(c)))}
    ,
        \\{a,'b,'c}
    ,
        &.{
            .l_brace,    .l_paren, .l_paren, .identifier, .r_paren, .comma,      .apostrophe, .l_paren, .l_paren,
            .identifier, .r_paren, .comma,   .apostrophe, .l_paren, .identifier, .r_paren,    .r_paren, .r_paren,
            .r_brace,
        },
        &.{
            .lambda,     .grouped_expression, .grouped_expression, .identifier, .apply_binary, .comma,
            .apostrophe, .grouped_expression, .grouped_expression, .identifier, .apply_binary, .comma,
            .apostrophe, .grouped_expression, .identifier,
        },
    );
    try testAstRender(
        \\((((a)));(((a))))
    ,
        \\(a;a)
    ,
        &.{
            .l_paren, .l_paren, .l_paren, .l_paren,    .identifier, .r_paren, .r_paren, .r_paren, .semicolon,
            .l_paren, .l_paren, .l_paren, .identifier, .r_paren,    .r_paren, .r_paren, .r_paren,
        },
        &.{
            .list,               .grouped_expression, .grouped_expression, .grouped_expression, .identifier,
            .grouped_expression, .grouped_expression, .grouped_expression, .identifier,
        },
    );
    try testAstRender(
        \\((((a),'(b)),'(c));)
    ,
        \\((a,'b),'c;)
    ,
        &.{
            .l_paren,    .l_paren, .l_paren, .l_paren, .identifier, .r_paren, .comma,      .apostrophe, .l_paren,
            .identifier, .r_paren, .r_paren, .comma,   .apostrophe, .l_paren, .identifier, .r_paren,    .r_paren,
            .semicolon,  .r_paren,
        },
        &.{
            .list,       .grouped_expression, .grouped_expression, .grouped_expression, .identifier,   .apply_binary,
            .comma,      .apostrophe,         .grouped_expression, .identifier,         .apply_binary, .comma,
            .apostrophe, .grouped_expression, .identifier,         .empty,
        },
    );
    try testAstRender(
        \\(((a),'((b),'(c)));)
    ,
        \\(a,'b,'c;)
    ,
        &.{
            .l_paren,    .l_paren,   .l_paren, .identifier, .r_paren, .comma,      .apostrophe, .l_paren, .l_paren,
            .identifier, .r_paren,   .comma,   .apostrophe, .l_paren, .identifier, .r_paren,    .r_paren, .r_paren,
            .r_brace,    .semicolon, .r_paren,
        },
        &.{
            .list,       .grouped_expression, .grouped_expression, .identifier, .apply_binary, .comma,
            .apostrophe, .grouped_expression, .grouped_expression, .identifier, .apply_binary, .comma,
            .apostrophe, .grouped_expression, .identifier,         .empty,
        },
    );
}

fn testRender(file_path: []const u8) !void {
    const gpa = std.testing.allocator;

    var dir = try std.fs.openDirAbsolute(@import("build_options").tests_path, .{});
    defer dir.close();
    const source_code = try dir.readFileAllocOptions(file_path, gpa, .unlimited, .of(u8), 0);
    defer gpa.free(source_code);
    var buf: [100]u8 = undefined;
    const expected_path = try std.fmt.bufPrint(
        &buf,
        "{s}.expected.q",
        .{file_path[0 .. file_path.len - 2]},
    );
    const expected_source = try dir.readFileAlloc(expected_path, gpa, .unlimited);
    defer gpa.free(expected_source);

    var tree = try Ast.parse(gpa, source_code, .{
        .mode = .q,
        .version = .@"4.0",
    });
    defer tree.deinit(gpa);

    // Errors
    const actual_errors = try gpa.alloc(Error.Tag, tree.errors.len);
    defer gpa.free(actual_errors);
    for (tree.errors, 0..) |err, i| actual_errors[i] = err.tag;
    try std.testing.expectEqualSlices(Error.Tag, &.{}, actual_errors);

    // Render
    var actual_source: std.Io.Writer.Allocating = .init(gpa);
    defer actual_source.deinit();
    try render(tree, gpa, &actual_source.writer, .{
        .indent_char = ' ',
        .indent_delta = 2,
    });
    try std.testing.expectEqualStrings(expected_source, actual_source.written());

    // Re-render to check determinism
    const duped_actual_source = try gpa.dupeZ(u8, actual_source.written());
    defer gpa.free(duped_actual_source);

    var det_tree = try Ast.parse(gpa, duped_actual_source, .{
        .mode = .q,
        .version = .@"4.0",
    });
    defer det_tree.deinit(gpa);

    // Errors
    const det_errors = try gpa.alloc(Error.Tag, det_tree.errors.len);
    defer gpa.free(det_errors);
    for (det_tree.errors, 0..) |err, i| det_errors[i] = err.tag;
    try std.testing.expectEqualSlices(Error.Tag, &.{}, det_errors);

    // Render
    var det_source: std.Io.Writer.Allocating = .init(gpa);
    defer det_source.deinit();
    try render(det_tree, gpa, &det_source.writer, .{
        .indent_char = ' ',
        .indent_delta = 2,
    });
    try std.testing.expectEqualStrings(expected_source, det_source.written());
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

test "render nested_lambdas_with_comments_and_newlines.q" {
    try testRender("nested_lambdas_with_comments_and_newlines.q");
}

test "render nested_if_with_comments_and_newlines.q" {
    try testRender("nested_if_with_comments_and_newlines.q");
}

test "render nested_call_with_comments_and_newlines.q" {
    try testRender("nested_call_with_comments_and_newlines.q");
}

test "render if_do_while.q" {
    try testRender("if_do_while.q");
}
