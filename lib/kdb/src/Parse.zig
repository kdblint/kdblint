//! Represents in-progress parsing, will be converted to an Ast after completion.
const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const kdb = @import("root.zig");
const Token = kdb.Token;
const Ast = kdb.Ast;
const Node = Ast.Node;

const Parse = @This();

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
mode: Ast.Mode,
source: []const u8,
tokens: std.MultiArrayList(Token) = .{},
tok_i: Token.Index = 0,
depth: u32 = 0,
eob: bool = false,
errors: std.ArrayListUnmanaged(Ast.Error) = .{},
nodes: std.MultiArrayList(Node) = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},

const Blocks = struct {
    len: usize,
    lhs: Node.Index,
    rhs: Node.Index,

    fn toSpan(self: Blocks, p: *Parse) !Node.SubRange {
        if (self.len <= 2) {
            const nodes = [2]Node.Index{ self.lhs, self.rhs };
            return p.listToSpan(nodes[0..self.len]);
        } else {
            return Node.SubRange{ .start = self.lhs, .end = self.rhs };
        }
    }
};

fn listToSpan(p: *Parse, list: []const Node.Index) !Node.SubRange {
    try p.extra_data.appendSlice(p.gpa, list);
    return Node.SubRange{
        .start = @intCast(p.extra_data.items.len - list.len),
        .end = @intCast(p.extra_data.items.len),
    };
}

fn addNode(p: *Parse, elem: Ast.Node) !Node.Index {
    const result = @as(Node.Index, @intCast(p.nodes.len));
    try p.nodes.append(p.gpa, elem);
    return result;
}

fn setNode(p: *Parse, i: usize, elem: Ast.Node) Node.Index {
    p.nodes.set(i, elem);
    return @as(Node.Index, @intCast(i));
}

fn reserveNode(p: *Parse, tag: Ast.Node.Tag) !usize {
    try p.nodes.resize(p.gpa, p.nodes.len + 1);
    p.nodes.items(.tag)[p.nodes.len - 1] = tag;
    return p.nodes.len - 1;
}

fn unreserveNode(p: *Parse, node_index: usize) void {
    if (p.nodes.len == node_index) {
        p.nodes.resize(p.gpa, p.nodes.len - 1) catch unreachable;
    } else {
        // There is zombie node left in the tree, let's make it as inoffensive as possible
        // (sadly there's no no-op node)
        p.nodes.items(.tag)[node_index] = .root; // TODO: It's probably a bad idea to use root here...
        p.nodes.items(.main_token)[node_index] = p.tok_i;
    }
}

fn addExtra(p: *Parse, extra: anytype) !Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.extra_data.ensureUnusedCapacity(p.gpa, fields.len);
    const result = @as(u32, @intCast(p.extra_data.items.len));
    inline for (fields) |field| {
        switch (@typeInfo(field.type)) {
            .int => comptime assert(field.type == Node.Index),
            .@"struct" => |ti| comptime assert(ti.layout == .@"packed" and ti.backing_integer.? == Node.Index),
            inline else => |tag| @compileError("Expected Node.Index or packed struct, found '" ++ @tagName(tag) ++ "'"),
        }
        p.extra_data.appendAssumeCapacity(@bitCast(@field(extra, field.name)));
    }
    return result;
}

fn warnExpected(p: *Parse, expected_token: Token.Tag) !void {
    @branchHint(.cold);
    try p.warnMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn warn(p: *Parse, error_tag: Ast.Error.Tag) !void {
    @branchHint(.cold);
    try p.warnMsg(.{ .tag = error_tag, .token = p.tok_i });
}

fn warnMsg(p: *Parse, msg: Ast.Error) !void {
    @branchHint(.cold);
    try p.errors.append(p.gpa, msg);
}

fn fail(p: *Parse, tag: Ast.Error.Tag) error{ ParseError, OutOfMemory } {
    @branchHint(.cold);
    return p.failMsg(.{ .tag = tag, .token = p.tok_i });
}

fn failExpected(p: *Parse, expected_token: Token.Tag) error{ ParseError, OutOfMemory } {
    @branchHint(.cold);
    return p.failMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn failMsg(p: *Parse, msg: Ast.Error) error{ ParseError, OutOfMemory } {
    @branchHint(.cold);
    try p.warnMsg(msg);
    return error.ParseError;
}

fn tokenSlice(p: *Parse, token_idex: Token.Index) []const u8 {
    const loc = p.tokens.items(.loc)[token_idex];
    return p.source[loc.start..loc.end];
}

// TODO: Add tests
fn validateUnaryApplication(p: *Parse, lhs: Node.Index, rhs: Node.Index) !void {
    const tags: []Node.Tag = p.nodes.items(.tag);
    const datas: []Node.Data = p.nodes.items(.data);
    const main_tokens: []Token.Index = p.nodes.items(.main_token);

    const tag = tags[lhs];
    switch (tag.getType()) {
        // Fail if we are applying a unary operator directly in q.
        .unary_operator => if (p.mode == .q) {
            return p.warnMsg(.{
                .tag = .cannot_apply_operator_directly,
                .token = main_tokens[lhs],
            });
        },

        // Fail if we are projecting an operator with no lhs.
        .binary_operator => if (datas[lhs].lhs == null_node) {
            return p.warnMsg(.{
                .tag = .cannot_project_operator_without_lhs,
                .token = main_tokens[lhs],
            });
        },

        // Fail if we are applying an iterator directly in q.
        .iterator => if (p.mode == .q and tags[rhs] != .expr_block) {
            return p.warnMsg(.{
                .tag = .cannot_apply_iterator_directly,
                .token = main_tokens[lhs],
            });
        },

        .other => {},
    }
}

/// Root <- Blocks EOF
pub fn parseRoot(p: *Parse) Allocator.Error!void {
    // Root node must be index 0.
    const root_index = try p.reserveNode(.root);
    errdefer p.unreserveNode(root_index);

    const blocks = try p.parseBlocks();
    if (p.peekTag() != .eof) {
        try p.warnExpected(.eof);
    }

    const root_decls = try blocks.toSpan(p);
    _ = p.setNode(root_index, .{
        .tag = .root,
        .main_token = undefined,
        .data = .{
            .lhs = root_decls.start,
            .rhs = root_decls.end,
        },
    });
}

/// Blocks <- Block*
fn parseBlocks(p: *Parse) !Blocks {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() != .eof) {
        const block_node = try p.parseBlock();
        if (block_node != null_node) {
            try p.scratch.append(p.gpa, block_node);
        }
        if (!p.eob) {
            try p.warn(.expected_expr);
            p.skipBlock();
        }
        assert(p.eob);
        p.eob = false;
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => return Blocks{
            .len = 0,
            .lhs = null_node,
            .rhs = null_node,
        },
        1 => return Blocks{
            .len = 1,
            .lhs = items[0],
            .rhs = null_node,
        },
        2 => return Blocks{
            .len = 2,
            .lhs = items[0],
            .rhs = items[1],
        },
        else => {
            const span = try p.listToSpan(items);
            return Blocks{
                .len = items.len,
                .lhs = span.start,
                .rhs = span.end,
            };
        },
    }
}

/// Block <- Expr SEMICOLON?
fn parseBlock(p: *Parse) !Node.Index {
    const expr = p.parseExpr() catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => blk: {
            p.skipBlock();
            break :blk null_node;
        },
    };

    // TODO: Use trailing.
    _ = p.eatToken(.semicolon);

    return expr;
}

const Precedence = enum(u8) {
    stop,
    none,
    iterator,
    call,

    _,
};

const oper_table = std.enums.directEnumArrayDefault(
    Token.Tag,
    Precedence,
    .none,
    0,
    .{
        .l_bracket = .call,

        .apostrophe = .iterator,
        .apostrophe_colon = .iterator,
        .slash = .iterator,
        .slash_colon = .iterator,
        .backslash = .iterator,
        .backslash_colon = .iterator,

        .r_paren = .stop,
        .r_brace = .stop,
        .r_bracket = .stop,
        .semicolon = .stop,
        .eob = .stop,
        .eof = .stop,
    },
);

/// Expr <- Noun Verb*
fn parseExpr(p: *Parse) !Node.Index {
    return p.parsePrecedence(.none);
}

fn expectExpr(p: *Parse) !Token.Index {
    const expr = try p.parseExpr();
    if (expr != null_node) return expr;
    return p.fail(.expected_expr);
}

fn parsePrecedence(p: *Parse, min_prec: Precedence) Error!Node.Index {
    var node = try p.parseNoun();
    if (node == null_node) {
        return null_node;
    }

    while (true) {
        const prec = oper_table[@intFromEnum(p.peekTag())];
        if (@intFromEnum(min_prec) > @intFromEnum(prec)) break;
        node = try p.parseVerb(node, null);
    }

    return node;
}

fn parseSqlExpr(p: *Parse, comptime sql_identifier: SqlIdentifier) !Node.Index {
    return p.parseSqlPrecedence(.none, sql_identifier);
}

fn expectSqlExpr(p: *Parse, comptime sql_identifier: SqlIdentifier) !Token.Index {
    const expr = try p.parseSqlExpr(sql_identifier);
    if (expr != null_node) return expr;
    return p.fail(.expected_expr);
}

fn parseSqlPrecedence(p: *Parse, min_prec: Precedence, comptime sql_identifier: SqlIdentifier) Error!Node.Index {
    if (p.peekIdentifier(sql_identifier)) |_| return null_node;
    var node = try p.parseNoun();
    if (node == null_node) {
        return null_node;
    }

    while (true) {
        if (p.peekIdentifier(sql_identifier)) |_| break;
        const prec = if (p.peekTag() == .comma)
            .stop
        else
            oper_table[@intFromEnum(p.peekTag())];
        if (@intFromEnum(min_prec) > @intFromEnum(prec)) break;
        node = try p.parseVerb(node, sql_identifier);
    }

    return node;
}

/// Noun
///     <- (Group
///      / Lambda
///      / ExprBlock
///      / Operator
///      / NumberLiteral
///      / STRING_LITERAL
///      / SymbolLiteral
///      / IDENTIFIER) Iterator*
fn parseNoun(p: *Parse) !Node.Index {
    const tag = p.peekTag();
    const noun = switch (tag) {
        .l_paren,
        => try p.parseGroup(),

        .l_brace,
        => try p.parseLambda(),

        .l_bracket,
        => try p.parseExprBlock(),

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
        => try p.parseOperator(),

        .colon_colon,
        => try p.addNode(.{
            .tag = .null,
            .main_token = p.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),

        .number_literal,
        => try p.parseNumberLiteral(),

        .string_literal,
        => try p.parseToken(.string_literal, .string_literal),

        .symbol_literal,
        => try p.parseSymbolLiteral(),

        .identifier,
        => try p.parseToken(.identifier, .identifier),

        .keyword_select,
        => try p.parseSelect(),

        .keyword_exec,
        => try p.parseExec(),

        else => null_node,
    };
    return p.parseIterator(noun);
}

/// Verb <- Expr*
fn parseVerb(p: *Parse, lhs: Node.Index, comptime sql_identifier: ?SqlIdentifier) !Node.Index {
    assert(lhs != null_node);

    const tag = p.peekTag();
    switch (tag) {
        .l_paren,
        .l_brace,
        .number_literal,
        .string_literal,
        .symbol_literal,
        .identifier,
        => {
            const op = if (sql_identifier) |sql_id|
                try p.parseSqlPrecedence(.iterator, sql_id)
            else
                try p.parsePrecedence(.iterator);
            assert(op != null_node);

            if (p.isIterator(op)) {
                const rhs = if (sql_identifier) |sql_id|
                    try p.parseSqlExpr(sql_id)
                else
                    try p.parseExpr();
                return p.addNode(.{
                    .tag = .apply_binary,
                    .main_token = op,
                    .data = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    },
                });
            } else {
                const rhs = try p.parseVerb(op, sql_identifier);
                return p.addNode(.{
                    .tag = .apply_unary,
                    .main_token = undefined,
                    .data = .{
                        .lhs = lhs,
                        .rhs = rhs,
                    },
                });
            }
        },

        .l_bracket,
        => {
            const rhs = if (sql_identifier) |sql_id|
                try p.parseSqlPrecedence(.call, sql_id)
            else
                try p.parsePrecedence(.call);
            const apply_unary = try p.addNode(.{
                .tag = .apply_unary,
                .main_token = undefined,
                .data = .{
                    .lhs = lhs,
                    .rhs = rhs,
                },
            });

            return p.parseVerb(apply_unary, sql_identifier);
        },

        inline .colon,
        .colon_colon,
        => |t| {
            const main_token = p.assertToken(t);

            const assign_tag: Node.Tag = comptime if (t == .colon) .assign else .global_assign;
            const assign_index = try p.reserveNode(assign_tag);
            errdefer p.unreserveNode(assign_index);

            const rhs = if (sql_identifier) |sql_id|
                try p.parseSqlExpr(sql_id)
            else
                try p.parseExpr();

            return p.setNode(assign_index, .{
                .tag = assign_tag,
                .main_token = main_token,
                .data = .{
                    .lhs = lhs,
                    .rhs = rhs,
                },
            });
        },

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
        => {
            const op = if (sql_identifier) |sql_id|
                try p.parseSqlPrecedence(.iterator, sql_id)
            else
                try p.parsePrecedence(.iterator);
            assert(op != null_node);

            const rhs = if (sql_identifier) |sql_id|
                try p.parseSqlExpr(sql_id)
            else
                try p.parseExpr();
            return p.addNode(.{
                .tag = .apply_binary,
                .main_token = op,
                .data = .{
                    .lhs = lhs,
                    .rhs = rhs,
                },
            });
        },

        else => return lhs,
    }
}

fn parseToken(p: *Parse, token_tag: Token.Tag, node_tag: Node.Tag) !Node.Index {
    const main_token = p.assertToken(token_tag);
    return p.addNode(.{
        .tag = node_tag,
        .main_token = main_token,
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });
}

fn parseEmpty(p: *Parse) !Node.Index {
    const token_tags = p.tokens.items(.tag);
    const tag = token_tags[p.tok_i];
    assert(tag == .semicolon or tag == .r_paren or tag == .r_bracket);
    return p.addNode(.{
        .tag = .empty,
        .main_token = p.tok_i,
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });
}

/// Group
///     <- Table
///      | LPAREN Expr* RPAREN
fn parseGroup(p: *Parse) !Node.Index {
    const l_paren = p.assertToken(.l_paren);
    if (p.eatToken(.r_paren)) |r_paren| {
        return p.addNode(.{
            .tag = .empty_list,
            .main_token = l_paren,
            .data = .{
                .lhs = undefined,
                .rhs = r_paren,
            },
        });
    }

    p.depth += 1;
    defer p.depth -= 1;

    const table = try p.parseTable(l_paren);
    if (table != null_node) {
        return table;
    }

    const list_index = try p.reserveNode(.list);
    errdefer p.unreserveNode(list_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        const expr = try p.parseExpr();
        try p.scratch.append(p.gpa, if (expr == null_node)
            try p.parseEmpty()
        else
            expr);
        if (p.eatToken(.semicolon)) |_| continue;
        break;
    }
    const r_paren = try p.expectToken(.r_paren);

    const list = p.scratch.items[scratch_top..];
    switch (list.len) {
        0 => unreachable,
        1 => return p.setNode(list_index, .{
            .tag = .grouped_expression,
            .main_token = l_paren,
            .data = .{
                .lhs = list[0],
                .rhs = r_paren,
            },
        }),
        else => return p.setNode(list_index, .{
            .tag = .list,
            .main_token = l_paren,
            .data = .{
                .lhs = try p.addExtra(try p.listToSpan(list)),
                .rhs = r_paren,
            },
        }),
    }
}

/// Table <- LPAREN LBRACKET (Expr (SEMICOLON Expr)*)* RBRACKET Expr (SEMICOLON Expr)* RPAREN
fn parseTable(p: *Parse, l_paren: Token.Index) !Node.Index {
    _ = p.eatToken(.l_bracket) orelse return null_node;

    const table_index = try p.reserveNode(.table_literal);
    errdefer p.unreserveNode(table_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const keys_top = p.scratch.items.len;
    if (p.peekTag() != .r_bracket) {
        while (true) {
            const expr = try p.expectExpr();
            try p.scratch.append(p.gpa, expr);
            if (p.eatToken(.semicolon)) |_| continue;
            break;
        }
    }
    _ = try p.expectToken(.r_bracket);

    const columns_top = p.scratch.items.len;

    while (true) {
        const expr = try p.expectExpr();
        try p.scratch.append(p.gpa, expr);
        if (p.eatToken(.semicolon)) |_| continue;
        break;
    }
    const r_paren = try p.expectToken(.r_paren);

    const keys = try p.listToSpan(p.scratch.items[keys_top..columns_top]);
    const columns = try p.listToSpan(p.scratch.items[columns_top..]);
    const table: Node.Table = .{
        .keys_start = keys.start,
        .keys_end = keys.end,
        .columns_start = columns.start,
        .columns_end = columns.end,
    };
    return p.setNode(table_index, .{
        .tag = .table_literal,
        .main_token = l_paren,
        .data = .{
            .lhs = try p.addExtra(table),
            .rhs = r_paren,
        },
    });
}

/// Lambda <- LBRACE ParamList? (Expr (SEMICOLON Expr)*)? RBRACE
///
/// ParamList <- LBRACKET (IDENTIFIER (SEMICOLON IDENTIFIER)*)? RBRACKET
fn parseLambda(p: *Parse) !Node.Index {
    const l_brace = p.assertToken(.l_brace);

    p.depth += 1;
    defer p.depth -= 1;

    const lambda_index = try p.reserveNode(.lambda);
    errdefer p.unreserveNode(lambda_index);

    var l_bracket: Token.Index = 0;
    var r_bracket: Token.Index = 0;
    const params: Node.SubRange = if (p.eatToken(.l_bracket)) |token| params: {
        l_bracket = token;

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        if (p.peekTag() != .r_bracket) {
            while (true) {
                const node = try p.parseToken(.identifier, .identifier);
                try p.scratch.append(p.gpa, node);
                _ = p.eatToken(.semicolon) orelse break;
            }
        }

        r_bracket = try p.expectToken(.r_bracket);

        break :params try p.listToSpan(p.scratch.items[scratch_top..]);
    } else .{ .start = 0, .end = 0 };

    // Check for negative number after params
    if (p.mode == .q and params.end > params.start and p.peekTag() == .number_literal) {
        const loc = p.peekLoc();
        if (p.source[loc.start] == '-' and p.prevLoc().end == loc.start) {
            try p.warn(.expected_whitespace);
        }
    }

    const body: Node.SubRange = body: {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const expr = try p.parseExpr();
            if (expr != null_node) {
                try p.scratch.append(p.gpa, expr);
            }
            if (p.eatToken(.semicolon)) |_| continue;
            break;
        }

        break :body try p.listToSpan(p.scratch.items[scratch_top..]);
    };
    const tag: Node.Tag = if (p.prevTag() == .semicolon) .lambda_semicolon else .lambda;
    const r_brace = try p.expectToken(.r_brace);

    const lambda = try p.addExtra(Node.Lambda{
        .l_bracket = l_bracket,
        .r_bracket = r_bracket,
        .params_start = params.start,
        .params_end = params.end,
        .body_start = body.start,
        .body_end = body.end,
    });
    return p.setNode(lambda_index, .{
        .tag = tag,
        .main_token = l_brace,
        .data = .{
            .lhs = lambda,
            .rhs = r_brace,
        },
    });
}

/// ExprBlock <- LBRACKET Exprs? RBRACKET
fn parseExprBlock(p: *Parse) !Node.Index {
    const l_bracket = p.assertToken(.l_bracket);
    if (p.eatToken(.r_bracket)) |r_bracket| {
        return p.addNode(.{
            .tag = .expr_block,
            .main_token = l_bracket,
            .data = .{
                .lhs = null_node,
                .rhs = r_bracket,
            },
        });
    }

    p.depth += 1;
    defer p.depth -= 1;

    const expr_block_index = try p.reserveNode(.expr_block);
    errdefer p.unreserveNode(expr_block_index);

    const exprs: Node.SubRange = exprs: {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const expr = try p.parseExpr();
            try p.scratch.append(p.gpa, if (expr == null_node)
                try p.parseEmpty()
            else
                expr);
            if (p.eatToken(.semicolon)) |_| continue;
            break;
        }

        break :exprs try p.listToSpan(p.scratch.items[scratch_top..]);
    };
    const r_bracket = try p.expectToken(.r_bracket);

    return p.setNode(expr_block_index, .{
        .tag = .expr_block,
        .main_token = l_bracket,
        .data = .{
            .lhs = try p.addExtra(exprs),
            .rhs = r_bracket,
        },
    });
}

fn parseSelect(p: *Parse) !Node.Index {
    const select_token = p.assertToken(.keyword_select);

    const select_node_index = try p.reserveNode(.select);
    errdefer p.unreserveNode(select_node_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    // Limit expression
    var distinct = false;
    var ascending = false;
    var limit_expr: Node.Index = 0;
    var order_tok: Token.Index = 0;
    if (p.eatIdentifier(.{ .distinct = true })) |_| {
        distinct = true;
    } else if (p.eatToken(.l_bracket)) |_| {
        p.depth += 1;
        defer p.depth -= 1;

        // TODO: Warn only first character is checked for ascending condition.
        // TODO: Should this be switched to consume expressions instead of tokens?
        switch (p.peekTag()) {
            .angle_bracket_left,
            .angle_bracket_left_colon,
            .angle_bracket_left_equal,
            .angle_bracket_left_right,
            => {
                _ = p.nextToken();
                ascending = true;
                order_tok = try p.expectToken(.identifier);
            },

            .angle_bracket_right,
            .angle_bracket_right_colon,
            .angle_bracket_right_equal,
            => {
                _ = p.nextToken();
                order_tok = try p.expectToken(.identifier);
            },

            else => {
                limit_expr = try p.expectExpr();
                if (p.eatToken(.semicolon)) |_| {
                    switch (p.peekTag()) {
                        .angle_bracket_left,
                        .angle_bracket_left_colon,
                        .angle_bracket_left_equal,
                        .angle_bracket_left_right,
                        => {
                            _ = p.nextToken();
                            ascending = true;
                            order_tok = try p.expectToken(.identifier);
                        },

                        .angle_bracket_right,
                        .angle_bracket_right_colon,
                        .angle_bracket_right_equal,
                        => {
                            _ = p.nextToken();
                            order_tok = try p.expectToken(.identifier);
                        },

                        else => {},
                    }
                }
            },
        }
        _ = try p.expectToken(.r_bracket);

        if (p.peekIdentifier(.{ .distinct = true })) |_| {
            try p.warn(.cannot_combine_limit_expression_and_distinct);
        }
    }

    // Select phrase
    const select_top = p.scratch.items.len;
    if (p.peekIdentifier(.{ .by = true, .from = true }) == null) {
        while (true) {
            const expr = try p.expectSqlExpr(.{ .by = true, .from = true });
            try p.scratch.append(p.gpa, expr);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    // By phrase
    const by_top = p.scratch.items.len;
    const has_by = p.eatIdentifier(.{ .by = true }) != null;
    if (has_by) {
        const first_expr = try p.parseSqlExpr(.{ .from = true });
        if (first_expr > 0) {
            try p.scratch.append(p.gpa, first_expr);
            while (p.eatToken(.comma)) |_| {
                const expr = try p.expectSqlExpr(.{ .from = true });
                try p.scratch.append(p.gpa, expr);
            }
        }
    }

    // From phrase
    _ = try p.expectIdentifier(.{ .from = true });
    const from_expr = try p.expectSqlExpr(.{ .where = true });

    // Where phrase
    const where_top = p.scratch.items.len;
    if (p.eatIdentifier(.{ .where = true })) |_| {
        while (true) {
            const expr = try p.expectSqlExpr(.{});
            try p.scratch.append(p.gpa, expr);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    const select = try p.listToSpan(p.scratch.items[select_top..by_top]);
    const by = try p.listToSpan(p.scratch.items[by_top..where_top]);
    const where = try p.listToSpan(p.scratch.items[where_top..]);
    const select_node: Node.Select = .{
        .limit = limit_expr,
        .order = order_tok,
        .select_start = select.start,
        .by_start = by.start,
        .from = from_expr,
        .where_start = where.start,
        .where_end = where.end,
        .data = .{
            .has_by = has_by,
            .distinct = distinct,
            .ascending = ascending,
        },
    };
    return p.setNode(select_node_index, .{
        .tag = .select,
        .main_token = select_token,
        .data = .{
            .lhs = try p.addExtra(select_node),
            .rhs = undefined,
        },
    });
}

fn parseExec(p: *Parse) !Node.Index {
    const exec_token = p.assertToken(.keyword_exec);

    const exec_node_index = try p.reserveNode(.exec);
    errdefer p.unreserveNode(exec_node_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    // Select phrase
    const select_top = p.scratch.items.len;
    if (p.peekIdentifier(.{ .by = true, .from = true }) == null) {
        while (true) {
            const expr = try p.expectSqlExpr(.{ .by = true, .from = true });
            try p.scratch.append(p.gpa, expr);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    // By phrase
    const by_top = p.scratch.items.len;
    if (p.eatIdentifier(.{ .by = true })) |_| {
        while (true) {
            const expr = try p.expectSqlExpr(.{ .from = true });
            try p.scratch.append(p.gpa, expr);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    // From phrase
    _ = try p.expectIdentifier(.{ .from = true });
    const from_expr = try p.expectSqlExpr(.{ .where = true });

    // Where phrase
    const where_top = p.scratch.items.len;
    if (p.eatIdentifier(.{ .where = true })) |_| {
        while (true) {
            const expr = try p.expectSqlExpr(.{});
            try p.scratch.append(p.gpa, expr);
            if (p.eatToken(.comma)) |_| continue;
            break;
        }
    }

    const select = try p.listToSpan(p.scratch.items[select_top..by_top]);
    const by = try p.listToSpan(p.scratch.items[by_top..where_top]);
    const where = try p.listToSpan(p.scratch.items[where_top..]);
    const exec_node: Node.Exec = .{
        .select_start = select.start,
        .by_start = by.start,
        .from = from_expr,
        .where_start = where.start,
        .where_end = where.end,
    };
    return p.setNode(exec_node_index, .{
        .tag = .exec,
        .main_token = exec_token,
        .data = .{
            .lhs = try p.addExtra(exec_node),
            .rhs = undefined,
        },
    });
}

/// Operator
///     <- PLUS
///      / MINUS
///      / ASTERISK
///      / PERCENT
///      / AMPERSAND
///      / PIPE
///      / CARET
///      / EQUAL
///      / ANGLE_BRACKET_LEFT
///      / ANGLE_BRACKET_LEFT_EQUAL
///      / ANGLE_BRACKET_LEFT_RIGHT
///      / ANGLE_BRACKET_RIGHT
///      / ANGLE_BRACKET_RIGHT_EQUAL
///      / DOLLAR
///      / COMMA
///      / HASH
///      / UNDERSCORE
///      / TILDE
///      / BANG
///      / QUESTION_MARK
///      / AT
///      / PERIOD
///      / ZERO_COLON
///      / ONE_COLON
///      / TWO_COLON
fn parseOperator(p: *Parse) !Node.Index {
    const token_tag: Token.Tag = p.peekTag();
    const node_tag: Node.Tag = switch (token_tag) {
        .plus => .plus,
        .minus => .minus,
        .asterisk => .asterisk,
        .percent => .percent,
        .ampersand => .ampersand,
        .pipe => .pipe,
        .caret => .caret,
        .equal => .equal,
        .angle_bracket_left => .angle_bracket_left,
        .angle_bracket_left_equal => .angle_bracket_left_equal,
        .angle_bracket_left_right => .angle_bracket_left_right,
        .angle_bracket_right => .angle_bracket_right,
        .angle_bracket_right_equal => .angle_bracket_right_equal,
        .dollar => .dollar,
        .comma => .comma,
        .hash => .hash,
        .underscore => .underscore,
        .tilde => .tilde,
        .bang => .bang,
        .question_mark => .question_mark,
        .at => .at,
        .period => .period,
        .zero_colon => .zero_colon,
        .one_colon => .one_colon,
        .two_colon => .two_colon,
        else => unreachable,
    };
    return p.parseToken(token_tag, node_tag);
}

/// Iterator
///     <- (APOSTROPHE
///      / APOSTROPHE_COLON
///      / SLASH
///      / SLASH_COLON
///      / BACKSLASH
///      / BACKSLASH_COLON)*
fn parseIterator(p: *Parse, lhs: Node.Index) !Node.Index {
    const token_tag = p.peekTag();
    const node_tag: Node.Tag = switch (token_tag) {
        .apostrophe => .apostrophe,
        .apostrophe_colon => .apostrophe_colon,
        .slash => .slash,
        .slash_colon => .slash_colon,
        .backslash => .backslash,
        .backslash_colon => .backslash_colon,
        else => return lhs,
    };
    const iterator = try p.addNode(.{
        .tag = node_tag,
        .main_token = p.assertToken(token_tag),
        .data = .{
            .lhs = lhs,
            .rhs = undefined,
        },
    });
    return p.parseIterator(iterator);
}

/// NumberLiteral <- NUMBER_LITERAL+
/// TODO: Should we validate number suffixes here?
fn parseNumberLiteral(p: *Parse) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() == .number_literal) {
        const number_literal = p.assertToken(.number_literal);
        try p.scratch.append(p.gpa, number_literal);
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => unreachable,
        1 => return p.addNode(.{
            .tag = .number_literal,
            .main_token = items[0],
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        else => return p.addNode(.{
            .tag = .number_list_literal,
            .main_token = items[0],
            .data = .{
                .lhs = items[items.len - 1],
                .rhs = undefined,
            },
        }),
    }
}

/// SymbolLiteral <- SYMBOL_LITERAL+
fn parseSymbolLiteral(p: *Parse) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() == .symbol_literal) {
        const symbol_literal = p.assertToken(.symbol_literal);
        try p.scratch.append(p.gpa, symbol_literal);
        if (p.prevLoc().end != p.peekLoc().start) break;
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => unreachable,
        1 => return p.addNode(.{
            .tag = .symbol_literal,
            .main_token = items[0],
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        else => return p.addNode(.{
            .tag = .symbol_list_literal,
            .main_token = items[0],
            .data = .{
                .lhs = items[items.len - 1],
                .rhs = undefined,
            },
        }),
    }
}

fn isIterator(p: *Parse, node: Node.Index) bool {
    const tag: Node.Tag = p.nodes.items(.tag)[node];
    return tag.getType() == .iterator;
}

fn eatToken(p: *Parse, tag: Token.Tag) ?Token.Index {
    return if (p.peekTag() == tag) p.nextToken() else null;
}

const SqlIdentifier = packed struct(u8) {
    by: bool = false,
    from: bool = false,
    where: bool = false,
    distinct: bool = false,
    _: u4 = 0,
};

fn peekIdentifier(p: *Parse, comptime sql_identifier: SqlIdentifier) ?Token.Index {
    const tag = p.peekTag();
    if (tag == .identifier) {
        const slice = p.tokenSlice(p.tok_i);
        if (sql_identifier.by and std.mem.eql(u8, slice, "by")) return p.tok_i;
        if (sql_identifier.from and std.mem.eql(u8, slice, "from")) return p.tok_i;
        if (sql_identifier.where and std.mem.eql(u8, slice, "where")) return p.tok_i;
        if (sql_identifier.distinct and std.mem.eql(u8, slice, "distinct")) return p.tok_i;
    }
    return null;
}

fn identifierEql(p: *Parse, identifier: []const u8, token_index: Token.Index) bool {
    return p.peekTag() == .identifier and std.mem.eql(u8, p.tokenSlice(token_index), identifier);
}

fn eatIdentifier(p: *Parse, comptime sql_identifier: SqlIdentifier) ?Token.Index {
    if (p.peekIdentifier(sql_identifier)) |i| {
        _ = p.nextToken();
        return i;
    }
    return null;
}

fn expectIdentifier(p: *Parse, comptime sql_identifier: SqlIdentifier) !Token.Index {
    if (p.eatIdentifier(sql_identifier)) |i| return i;
    return p.failMsg(.{
        .tag = .expected_qsql_token,
        .token = p.tok_i,
        .extra = .{
            .expected_string = if (sql_identifier.by)
                "by"
            else if (sql_identifier.from)
                "from"
            else if (sql_identifier.where)
                "where"
            else
                comptime unreachable,
        },
    });
}

fn assertToken(p: *Parse, tag: Token.Tag) Token.Index {
    assert(p.peekTag() == tag);
    return p.nextToken();
}

fn expectToken(p: *Parse, tag: Token.Tag) !Token.Index {
    if (p.eatToken(tag)) |token| return token;
    return p.failExpected(tag);
}

fn prevTag(p: *Parse) Token.Tag {
    const tag: Token.Tag = p.tokens.items(.tag)[p.tok_i - 1];
    return tag;
}

fn prevLoc(p: *Parse) Token.Loc {
    const loc: Token.Loc = p.tokens.items(.loc)[p.tok_i - 1];
    return loc;
}

fn peekTag(p: *Parse) Token.Tag {
    if (p.eob) return .eob;
    const tag: Token.Tag = p.tokens.items(.tag)[p.tok_i];
    return tag;
}

fn peekLoc(p: *Parse) Token.Loc {
    const loc: Token.Loc = p.tokens.items(.loc)[p.tok_i];
    return loc;
}

fn nextToken(p: *Parse) Token.Index {
    if (p.tok_i == p.tokens.len - 1) {
        p.eob = true;
        return p.tok_i;
    }

    const result = p.tok_i;
    p.eob = p.isEob(result);
    std.log.debug("nextToken: {s} '{s}' {}", .{
        @tagName(p.tokens.items(.tag)[p.tok_i]),
        slice: {
            const loc = p.tokens.items(.loc)[p.tok_i];
            break :slice p.source[loc.start..loc.end];
        },
        p.eob,
    });
    p.tok_i += 1;
    return result;
}

// TODO: Test
fn skipBlock(p: *Parse) void {
    if (p.eob) {
        _ = p.nextToken();
    } else {
        while (!p.eob) {
            _ = p.nextToken();
        }
    }
}

fn isEob(p: *Parse, token_index: Token.Index) bool {
    const tags: []Token.Tag = p.tokens.items(.tag);
    const locs: []Token.Loc = p.tokens.items(.loc);

    const tag = tags[token_index];
    switch (tag) {
        .semicolon => return p.depth == 0,
        .eof => return true,
        else => {
            if (tags[token_index + 1] == .eof) return true;
            const next_token_start = locs[token_index + 1].start;
            return next_token_start == p.source.len or p.source[next_token_start - 1] == '\n';
        },
    }
}

const null_node: Node.Index = 0;
