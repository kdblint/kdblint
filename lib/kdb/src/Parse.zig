//! Represents in-progress parsing, will be converted to an Ast after completion.
const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const kdb = @import("root.zig");
const Token = kdb.Token;
const Ast = kdb.Ast;
const Node = Ast.Node;

const Parse = @This();

pub const Error = error{ ParseError, EndOfBlock } || Allocator.Error;

gpa: Allocator,
mode: Ast.Mode,
source: []const u8,
tokens: std.MultiArrayList(Token) = .{},
tok_i: Token.Index = 0,
eob: bool = false,
errors: std.ArrayListUnmanaged(Ast.Error) = .{},
nodes: std.MultiArrayList(Node) = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},
ends_expr: std.ArrayListUnmanaged(Token.Tag) = .{},

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
        comptime assert(field.type == Node.Index);
        p.extra_data.appendAssumeCapacity(@field(extra, field.name));
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
    // switch (msg.tag) {
    //     .expected_container,
    //     => if (msg.token != 0 and !p.tokensOnSameLine(msg.token - 1, msg.token)) {
    //         var copy = msg;
    //         copy.token_is_prev = true;
    //         copy.token -= 1;
    //         return p.errors.append(p.gpa, copy);
    //     },
    //     else => {},
    // }
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

fn validateUnaryApplication(p: *Parse, node: Node.Index) !void {
    const tags: []Node.Tag = p.nodes.items(.tag);
    const datas: []Node.Data = p.nodes.items(.data);
    const main_tokens: []Token.Index = p.nodes.items(.main_token);

    const tag = tags[node];
    switch (tag.getType()) {
        // Fail if we are applying a unary operator directly in q.
        .unary_operator => if (p.mode == .q) {
            return p.failMsg(.{
                .tag = .cannot_apply_operator_directly,
                .token = main_tokens[node],
            });
        },

        // Fail if we are projecting an operator with no lhs.
        .binary_operator => if (datas[node].lhs == null_node) {
            return p.failMsg(.{
                .tag = .cannot_project_operator_without_lhs,
                .token = main_tokens[node],
            });
        },

        // Fail if we are applying an iterator directly in q.
        .iterator => if (p.mode == .q) {
            return p.failMsg(.{
                .tag = .cannot_apply_iterator_directly,
                .token = main_tokens[node],
            });
        },

        .other => {},
    }
}

/// Root <- Blocks eof
pub fn parseRoot(p: *Parse) !void {
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
        .main_token = null_token,
        .data = .{
            .lhs = root_decls.start,
            .rhs = root_decls.end,
        },
    });
}

/// Blocks <- Block*
fn parseBlocks(p: *Parse) Allocator.Error!Blocks {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() != .eof) {
        const block_node = p.parseBlock() catch |err| switch (err) {
            error.OutOfMemory,
            => return error.OutOfMemory,

            error.ParseError,
            error.EndOfBlock,
            => blk: {
                p.eob = false;
                while (true) {
                    std.log.debug("{s}: {s} '{s}'", .{
                        @errorName(err),
                        @tagName(p.tokens.items(.tag)[p.tok_i]),
                        slice: {
                            const loc = p.tokens.items(.loc)[p.tok_i];
                            break :slice p.source[loc.start..loc.end];
                        },
                    });
                    _ = p.nextToken() catch break;
                }
                break :blk null_node;
            },
        };
        if (block_node != null_node) {
            try p.scratch.append(p.gpa, block_node);
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
    assert(p.ends_expr.items.len == 0);
    if (p.peekTag() == .eof) return null_node;

    const expr = try p.parseExpr();
    if (expr == null_node) return error.ParseError;

    // TODO: Use trailing.
    _ = p.eatToken(.semicolon) catch {};

    return expr;
}

fn parseExpr(p: *Parse) Error!Node.Index {
    return p.parseExprPrecedence();
}

fn expectExpr(p: *Parse) !Node.Index {
    const node = try p.parseExpr();
    if (node == null_node) {
        return p.fail(.expected_expr);
    } else {
        return node;
    }
}

// A table of binary operators.
const operTable = std.enums.directEnumArray(Token.Tag, ?Node.Tag, 0, .{
    // Punctuation
    .l_paren = .apply_unary,
    .r_paren = null,
    .l_brace = .apply_unary,
    .r_brace = null,
    .l_bracket = .apply_unary,
    .r_bracket = null,
    .semicolon = null,

    // Verbs
    .colon = .assign,
    .colon_colon = null,
    .plus = .apply_binary,
    .plus_colon = null,
    .minus = .apply_binary,
    .minus_colon = null,
    .asterisk = .apply_binary,
    .asterisk_colon = null,
    .percent = .apply_binary,
    .percent_colon = null,
    .ampersand = .apply_binary,
    .ampersand_colon = null,
    .pipe = .apply_binary,
    .pipe_colon = null,
    .caret = .apply_binary,
    .caret_colon = null,
    .equal = .apply_binary,
    .equal_colon = null,
    .angle_bracket_left = .apply_binary,
    .angle_bracket_left_colon = null,
    .angle_bracket_left_equal = .apply_binary,
    .angle_bracket_left_right = .apply_binary,
    .angle_bracket_right = .apply_binary,
    .angle_bracket_right_colon = null,
    .angle_bracket_right_equal = .apply_binary,
    .dollar = .apply_binary,
    .dollar_colon = null,
    .comma = .apply_binary,
    .comma_colon = null,
    .hash = .apply_binary,
    .hash_colon = null,
    .underscore = .apply_binary,
    .underscore_colon = null,
    .tilde = .apply_binary,
    .tilde_colon = null,
    .bang = .apply_binary,
    .bang_colon = null,
    .question_mark = .apply_binary,
    .question_mark_colon = null,
    .at = .apply_binary,
    .at_colon = null,
    .period = .apply_binary,
    .period_colon = null,
    .zero_colon = .apply_binary,
    .zero_colon_colon = null,
    .one_colon = .apply_binary,
    .one_colon_colon = null,
    .two_colon = .apply_binary,

    // Adverbs
    .apostrophe = null,
    .apostrophe_colon = null,
    .slash = null,
    .slash_colon = null,
    .backslash = null,
    .backslash_colon = null,

    // Literals
    .number_literal = .apply_unary,
    .string_literal = .apply_unary,
    .symbol_literal = .apply_unary,
    .identifier = .apply_unary,

    // Miscellaneous
    .invalid = null,
    .eof = null,
});

fn parseExprPrecedence(p: *Parse) !Node.Index {
    var node = try p.parsePrefixExpr();
    if (node == null_node) {
        return null_node;
    }

    while (!p.eob) {
        const tag = operTable[@intCast(@intFromEnum(p.peekTag()))] orelse break;
        const main_token: Token.Index = switch (tag) {
            .apply_unary => blk: {
                try p.validateUnaryApplication(node);
                break :blk null_token;
            },
            .apply_binary => try p.parsePrefixExpr(),
            else => try p.nextToken(),
        };
        const rhs = try p.parseExprPrecedence();
        node = try p.addNode(.{
            .tag = tag,
            .main_token = main_token,
            .data = .{
                .lhs = node,
                .rhs = rhs,
            },
        });
    }

    return node;
}

/// PrefixExpr
///     <- Group
///      / Lambda
///      / Operator
///      / NumberLiteral
///      / STRING_LITERAL
///      / SYMBOL_LITERAL
///      / IDENTIFIER
fn parsePrefixExpr(p: *Parse) !Node.Index {
    const tag = p.peekTag();
    switch (tag) {
        .l_paren => {
            return p.parseGroup() catch |err| switch (err) {
                error.EndOfBlock => return p.failExpected(.r_paren),
                inline else => |e| return e,
            };
        },
        .l_brace => {
            return p.parseLambda() catch |err| switch (err) {
                error.EndOfBlock => return p.failExpected(.r_brace),
                inline else => |e| return e,
            };
        },
        .l_bracket => {
            return p.parseExprBlock() catch |err| switch (err) {
                error.EndOfBlock => return p.failExpected(.r_bracket),
                inline else => |e| return e,
            };
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
        => return p.parseOperator(),

        .apostrophe,
        .apostrophe_colon,
        .slash,
        .slash_colon,
        .backslash,
        .backslash_colon,
        => return p.parseIterator(null_node),

        .number_literal => return p.parseNumberLiteral(),
        .string_literal => return p.addNode(.{
            .tag = .string_literal,
            .main_token = try p.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        .symbol_literal => return p.parseSymbolLiteral(),
        .identifier => return p.addNode(.{
            .tag = .identifier,
            .main_token = try p.nextToken(),
            .data = .{
                .lhs = undefined,
                .rhs = undefined,
            },
        }),
        else => return null_node,
    }
}

/// Group
///     <- Table
///      | LPAREN Expr* RPAREN
fn parseGroup(p: *Parse) !Node.Index {
    const l_paren = try p.assertToken(.l_paren);
    if (try p.eatToken(.r_paren)) |r_paren| {
        return p.addNode(.{
            .tag = .empty_list,
            .main_token = l_paren,
            .data = .{
                .lhs = undefined,
                .rhs = r_paren,
            },
        });
    }

    try p.ends_expr.append(p.gpa, .r_paren);
    defer {
        const ends_expr = p.ends_expr.pop();
        assert(ends_expr == .r_paren);
    }

    const table = try p.parseTable(l_paren);
    if (table != null_node) {
        return table;
    }

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const list_index = try p.reserveNode(.list);
    errdefer p.unreserveNode(list_index);

    const r_paren = while (true) {
        const expr = try p.parseExpr();
        try p.scratch.append(p.gpa, expr);
        if (try p.eatToken(.r_paren)) |r_paren| break r_paren;
        if (try p.eatToken(.semicolon)) |_| continue;
        if (p.eob or p.peekTag() == .eof) return p.failExpected(.r_paren);
    };

    const list = p.scratch.items[scratch_top..];
    switch (list.len) {
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

/// Table <- TODO
fn parseTable(p: *Parse, l_paren: Token.Index) !Node.Index {
    _ = l_paren; // autofix
    if (try p.eatToken(.l_bracket)) |l_bracket| {
        _ = l_bracket; // autofix
        @panic("NYI");
    }

    return null_node;
}

/// Lambda <- LBRACE ParamList? Exprs? RBRACE
fn parseLambda(p: *Parse) !Node.Index {
    const l_brace = try p.assertToken(.l_brace);

    try p.ends_expr.append(p.gpa, .r_brace);
    defer {
        const ends_expr = p.ends_expr.pop();
        assert(ends_expr == .r_brace);
    }

    const lambda_index = try p.reserveNode(.lambda);
    errdefer p.unreserveNode(lambda_index);

    var l_bracket: Token.Index = null_token;
    var r_bracket: Token.Index = null_token;
    const params: Node.SubRange = if (try p.eatToken(.l_bracket)) |token| params: {
        l_bracket = token;

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        if (p.peekTag() != .r_bracket) {
            while (true) {
                const param = try p.expectToken(.identifier);
                const node = try p.addNode(.{
                    .tag = .identifier,
                    .main_token = param,
                    .data = .{
                        .lhs = undefined,
                        .rhs = undefined,
                    },
                });
                try p.scratch.append(p.gpa, node);
                _ = try p.eatToken(.semicolon) orelse break;
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

    const body = try p.parseExprs();
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
    const l_bracket = try p.assertToken(.l_bracket);
    if (try p.eatToken(.r_paren)) |r_bracket| {
        return p.addNode(.{
            .tag = .expr_block,
            .main_token = l_bracket,
            .data = .{
                .lhs = undefined,
                .rhs = r_bracket,
            },
        });
    }

    try p.ends_expr.append(p.gpa, .r_bracket);
    defer {
        const ends_expr = p.ends_expr.pop();
        assert(ends_expr == .r_bracket);
    }

    const expr_block_index = try p.reserveNode(.expr_block);
    errdefer p.unreserveNode(expr_block_index);

    const span = try p.parseExprs();
    const r_bracket = try p.expectToken(.r_bracket);
    return p.setNode(expr_block_index, .{
        .tag = .expr_block,
        .main_token = l_bracket,
        .data = .{
            .lhs = try p.addExtra(span),
            .rhs = r_bracket,
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
    const tag: Node.Tag = switch (p.peekTag()) {
        .plus => .add,
        .minus => .sub,
        .asterisk => .mul,
        .percent => .div,
        .ampersand => .@"and",
        .pipe => .@"or",
        .caret => .fill,
        .equal => .equal,
        .angle_bracket_left => .less_than,
        .angle_bracket_left_equal => .less_or_equal,
        .angle_bracket_left_right => .not_equal,
        .angle_bracket_right => .greater_than,
        .angle_bracket_right_equal => .greater_or_equal,
        .dollar => .cast,
        .comma => .join,
        .hash => .take,
        .underscore => .drop,
        .tilde => .match,
        .bang => .dict,
        .question_mark => .find,
        .at => .apply_at,
        .period => .apply,
        .zero_colon => .file_text,
        .one_colon => .file_binary,
        .two_colon => .dynamic_load,
        else => return null_node,
    };
    const node = try p.addNode(.{
        .tag = tag,
        .main_token = try p.nextToken(),
        .data = .{
            .lhs = undefined,
            .rhs = undefined,
        },
    });

    const iterator = try p.parseIterator(node);
    if (iterator != null_node) {
        return iterator;
    }

    return node;
}

/// Iterator
///     <- APOSTROPHE
///      / APOSTROPHE_COLON
///      / SLASH
///      / SLASH_COLON
///      / BACKSLASH
///      / BACKSLASH_COLON
fn parseIterator(p: *Parse, lhs: Node.Index) !Node.Index {
    const tag: Node.Tag = switch (p.peekTag()) {
        .apostrophe => .each,
        .ampersand_colon => .each_prior,
        .slash => .over,
        .slash_colon => .each_right,
        .backslash => .scan,
        .backslash_colon => .each_left,
        else => return null_node,
    };
    return p.addNode(.{
        .tag = tag,
        .main_token = try p.nextToken(),
        .data = .{
            .lhs = lhs,
            .rhs = undefined,
        },
    });
}

/// NumberLiteral <- NUMBER_LITERAL+
/// TODO: Should we validate number suffixes here?
fn parseNumberLiteral(p: *Parse) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() == .number_literal) {
        const number_literal = try p.assertToken(.number_literal);
        try p.scratch.append(p.gpa, number_literal);
        if (p.eob) break;
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

fn parseSymbolLiteral(p: *Parse) !Node.Index {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (p.peekTag() == .symbol_literal) {
        const symbol_literal = try p.assertToken(.symbol_literal);
        try p.scratch.append(p.gpa, symbol_literal);
        if (p.eob) break;
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

fn parseLambdaParams(p: *Parse) !Node.Index {
    const l_bracket = try p.eatToken(.l_bracket) orelse return null_node;

    const lambda_params_index = try p.reserveNode(.lambda_params);
    errdefer p.unreserveNode(lambda_params_index);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    if (p.peekTag() != .r_bracket) {
        while (true) {
            const param = try p.expectToken(.identifier);
            const node = try p.addNode(.{
                .tag = .identifier,
                .main_token = param,
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
            try p.scratch.append(p.gpa, node);
            _ = try p.eatToken(.semicolon) orelse break;
        }
    }

    _ = try p.expectToken(.r_bracket);

    const span = try p.listToSpan(p.scratch.items[scratch_top..]);
    return p.setNode(lambda_params_index, .{
        .tag = .lambda_params,
        .main_token = l_bracket,
        .data = .{
            .lhs = span.start,
            .rhs = span.end,
        },
    });
}

/// Exprs <- Expr (SEMICOLON Expr?)*
fn parseExprs(p: *Parse) !Node.SubRange {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    const ends_expr = p.ends_expr.getLast();
    while (p.peekTag() != ends_expr) {
        const expr = try p.parseExpr();
        if (expr != null_node) {
            try p.scratch.append(p.gpa, expr);
        }
        if (try p.eatToken(.semicolon)) |_| continue;
    }

    return p.listToSpan(p.scratch.items[scratch_top..]);
}

fn parseLambdaBody(p: *Parse) !Node.Index {
    const lambda_body_index = try p.reserveNode(.lambda_body);
    errdefer p.unreserveNode(lambda_body_index);

    const span = try p.parseExprs();
    const tag: Node.Tag = if (p.prevTag() == .semicolon) .lambda_body_semicolon else .lambda_body;
    const r_brace = try p.expectToken(.r_brace);

    return p.setNode(lambda_body_index, .{
        .tag = tag,
        .main_token = r_brace,
        .data = .{
            .lhs = span.start,
            .rhs = span.end,
        },
    });
}

fn eatToken(p: *Parse, tag: Token.Tag) !?Token.Index {
    return if (p.peekTag() == tag) try p.nextToken() else null;
}

fn assertToken(p: *Parse, tag: Token.Tag) !Token.Index {
    assert(p.peekTag() == tag);
    return p.nextToken();
}

fn expectToken(p: *Parse, tag: Token.Tag) !Token.Index {
    if (p.peekTag() != tag) {
        return p.failMsg(.{
            .tag = .expected_token,
            .token = p.tok_i,
            .extra = .{ .expected_tag = tag },
        });
    }
    return p.nextToken();
}

fn prevTag(p: *Parse) Token.Tag {
    const tag = p.tokens.items(.tag)[p.tok_i - 1];
    return tag;
}

fn prevLoc(p: *Parse) Token.Loc {
    const loc = p.tokens.items(.loc)[p.tok_i - 1];
    return loc;
}

fn peekTag(p: *Parse) Token.Tag {
    const tag = p.tokens.items(.tag)[p.tok_i];
    return tag;
}

fn peekLoc(p: *Parse) Token.Loc {
    const loc = p.tokens.items(.loc)[p.tok_i];
    return loc;
}

fn nextToken(p: *Parse) error{EndOfBlock}!Token.Index {
    if (p.eob) return error.EndOfBlock;

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

fn isEob(p: *Parse, token_index: Token.Index) bool {
    const tags: []Token.Tag = p.tokens.items(.tag);
    const locs: []Token.Loc = p.tokens.items(.loc);

    const tag = tags[token_index];
    switch (tag) {
        .semicolon => return p.ends_expr.items.len == 0,
        .eof => return true,
        else => {
            if (tags[token_index + 1] == .eof) return true;
            const next_token_start = locs[token_index + 1].start;
            return next_token_start == p.source.len or p.source[next_token_start - 1] == '\n';
        },
    }
}

const null_node: Node.Index = 0;
const null_token: Token.Index = 0;
