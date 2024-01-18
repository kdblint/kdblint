//! Represents in-progress parsing, will be converted to an Ast after completion.

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
source: []const u8,
token_tags: []const Token.Tag,
token_locs: []const Token.Loc,
token_eobs: []const bool,
tok_i: TokenIndex = 0,
eob: bool = false,
errors: std.ArrayListUnmanaged(AstError) = .{},
nodes: Ast.NodeList = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},

pub fn deinit(p: *Parse) void {
    defer p.errors.deinit(p.gpa);
    defer p.nodes.deinit(p.gpa);
    defer p.extra_data.deinit(p.gpa);
    defer p.scratch.deinit(p.gpa);
}

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

const Precedence = enum {
    none,
    secondary,
    primary,
};

const OperInfo = struct {
    prefix: ?*const fn (*Parse) Error!Node.Index,
    infix: ?*const fn (*Parse, Node.Index) Error!Node.Index,
    prec: Precedence,
};

fn listToSpan(p: *Parse, list: []const Node.Index) !Node.SubRange {
    try p.extra_data.appendSlice(p.gpa, list);
    return Node.SubRange{
        .start = @as(Node.Index, @intCast(p.extra_data.items.len - list.len)),
        .end = @as(Node.Index, @intCast(p.extra_data.items.len)),
    };
}

fn addNode(p: *Parse, elem: Ast.Node) Allocator.Error!Node.Index {
    const result: Node.Index = @intCast(p.nodes.len);
    try p.nodes.append(p.gpa, elem);
    return result;
}

fn addExtra(p: *Parse, extra: anytype) Allocator.Error!Node.Index {
    const fields = std.meta.fields(@TypeOf(extra));
    try p.extra_data.ensureUnusedCapacity(p.gpa, fields.len);
    const result = @as(u32, @intCast(p.extra_data.items.len));
    inline for (fields) |field| {
        comptime assert(field.type == Node.Index);
        p.extra_data.appendAssumeCapacity(@field(extra, field.name));
    }
    return result;
}

fn warnExpected(p: *Parse, expected_token: Token.Tag) error{OutOfMemory}!void {
    @setCold(true);
    try p.warnMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn warn(p: *Parse, error_tag: AstError.Tag) error{OutOfMemory}!void {
    @setCold(true);
    try p.warnMsg(.{ .tag = error_tag, .token = p.tok_i });
}

fn warnMsg(p: *Parse, msg: Ast.Error) error{OutOfMemory}!void {
    @setCold(true);
    switch (msg.tag) {
        .expected_semi_after_decl,
        .expected_semi_after_stmt,
        .expected_comma_after_field,
        .expected_comma_after_arg,
        .expected_comma_after_param,
        .expected_comma_after_initializer,
        .expected_comma_after_switch_prong,
        .expected_comma_after_for_operand,
        .expected_comma_after_capture,
        .expected_semi_or_else,
        .expected_semi_or_lbrace,
        .expected_token,
        .expected_block,
        .expected_block_or_assignment,
        .expected_block_or_expr,
        .expected_block_or_field,
        .expected_expr,
        .expected_expr_or_assignment,
        .expected_fn,
        .expected_inlinable,
        .expected_labelable,
        .expected_param_list,
        .expected_prefix_expr,
        .expected_primary_type_expr,
        .expected_pub_item,
        .expected_return_type,
        .expected_suffix_op,
        .expected_type_expr,
        .expected_var_decl,
        .expected_var_decl_or_fn,
        .expected_loop_payload,
        .expected_container,
        => if (msg.token != 0 and !p.tokensOnSameLine(msg.token - 1, msg.token)) {
            var copy = msg;
            copy.token_is_prev = true;
            copy.token -= 1;
            return p.errors.append(p.gpa, copy);
        },
        else => {},
    }
    try p.errors.append(p.gpa, msg);
}

fn fail(p: *Parse, tag: Ast.Error.Tag) error{ ParseError, OutOfMemory } {
    @setCold(true);
    return p.failMsg(.{ .tag = tag, .token = p.tok_i });
}

fn failExpected(p: *Parse, expected_token: Token.Tag) error{ ParseError, OutOfMemory } {
    @setCold(true);
    return p.failMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{ .expected_tag = expected_token },
    });
}

fn failMsg(p: *Parse, msg: Ast.Error) error{ ParseError, OutOfMemory } {
    @setCold(true);
    try p.warnMsg(msg);
    return error.ParseError;
}

pub fn parseRoot(p: *Parse) Allocator.Error!void {
    // Root node must be index 0.
    p.nodes.appendAssumeCapacity(.{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });

    const blocks = p.parseBlocks() catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => return,
    };
    const root_decls = try blocks.toSpan(p);
    p.nodes.items(.data)[0] = .{
        .lhs = root_decls.start,
        .rhs = root_decls.end,
    };
}

fn parseBlocks(p: *Parse) Error!Blocks {
    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.peekTag() == .eof) break;

        const expr = try p.parseBlock();
        if (expr != 0) {
            try p.scratch.append(p.gpa, expr);
        }
    }

    const items = p.scratch.items[scratch_top..];
    switch (items.len) {
        0 => return Blocks{
            .len = 0,
            .rhs = 0,
            .lhs = 0,
        },
        1 => return Blocks{
            .len = 1,
            .lhs = items[0],
            .rhs = 0,
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

fn parseBlock(p: *Parse) Error!Node.Index {
    const node = p.parseExpr();
    p.nextBlock();
    return node;
}

fn parseExpr(p: *Parse) Error!Node.Index {
    return p.parseExprPrecedence(Precedence.secondary);
}

fn expectExpr(p: *Parse) Error!Node.Index {
    const node = try p.parseExpr();
    if (node == 0) {
        return p.fail(.expected_expr);
    } else {
        return node;
    }
}

fn Prefix(comptime tag: Node.Tag) *const fn (*Parse) Error!Node.Index {
    return struct {
        fn impl(p: *Parse) Error!Node.Index {
            return p.addNode(.{
                .tag = tag,
                .main_token = p.nextToken(),
                .data = .{
                    .lhs = undefined,
                    .rhs = undefined,
                },
            });
        }
    }.impl;
}

fn Infix(comptime tag: Node.Tag) *const fn (*Parse, Node.Index) Error!Node.Index {
    return struct {
        fn impl(p: *Parse, lhs: Node.Index) Error!Node.Index {
            const main_token = p.nextToken();
            const rhs = try p.parseExpr();
            const node = try p.addNode(.{
                .tag = tag,
                .main_token = main_token,
                .data = .{
                    .lhs = lhs,
                    .rhs = rhs,
                },
            });
            return if (rhs == 0) node else node; // TODO: Composition
        }
    }.impl;
}

fn grouping(p: *Parse) Error!Node.Index {
    return p.addNode(.{
        .tag = .grouped_expression,
        .main_token = p.nextToken(),
        .data = .{
            .lhs = try p.expectExpr(),
            .rhs = try p.expectToken(.r_paren),
        },
    });
}

fn noOp(p: *Parse) Error!Node.Index {
    _ = p.nextToken();
    return 0;
}

fn apply(p: *Parse, lhs: Node.Index) Error!Node.Index {
    return p.addNode(.{
        .tag = .implicit_apply,
        .main_token = undefined,
        .data = .{
            .lhs = lhs,
            .rhs = try p.parseExpr(),
        },
    });
}

fn tokensOnSameLine(p: *Parse, token1: TokenIndex, token2: TokenIndex) bool {
    return std.mem.indexOfScalar(u8, p.source[p.token_locs[token1].start..p.token_locs[token2].start], '\n') == null;
}

fn expectToken(p: *Parse, tag: Token.Tag) Error!TokenIndex {
    if (p.peekTag() != tag) {
        return p.failMsg(.{
            .tag = .expected_token,
            .token = p.tok_i,
            .extra = .{ .expected_tag = tag },
        });
    }
    return p.nextToken();
}

fn skipComments(p: *Parse) void {
    while (p.token_tags[p.tok_i] == .comment) {
        p.tok_i += 1;
    }
}

fn peekTag(p: *Parse) Token.Tag {
    p.skipComments();
    return p.token_tags[p.tok_i];
}

fn nextToken(p: *Parse) TokenIndex {
    if (p.eob) return null_node;

    p.skipComments();
    const result = p.tok_i;
    p.eob = p.token_eobs[result];
    log.debug("nextToken = '{s}' {}", .{ p.source[p.token_locs[result].start..p.token_locs[result].end], p.eob });
    p.tok_i += 1;
    return result;
}

fn nextBlock(p: *Parse) void {
    assert(p.eob);
    p.eob = false;
}

fn getRule(p: *Parse) OperInfo {
    if (p.eob) {
        return .{ .prefix = noOp, .infix = null, .prec = Precedence.none };
    }
    return operTable[@intFromEnum(p.peekTag())];
}

fn parseExprPrecedence(p: *Parse, precedence: Precedence) Error!Node.Index {
    const prefix = p.getRule().prefix orelse {
        try p.warn(.expected_prefix_expr);
        return null_node;
    };
    var node = try prefix(p);

    while (@intFromEnum(precedence) <= @intFromEnum(p.getRule().prec)) {
        const infix = p.getRule().infix orelse {
            try p.warn(.expected_infix_expr);
            break;
        };
        node = try infix(p, node);
    }

    return node;
}

const operTable = std.enums.directEnumArray(Token.Tag, OperInfo, 0, .{
    // Punctuation
    .l_paren = .{ .prefix = grouping, .infix = apply, .prec = Precedence.secondary },
    .r_paren = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .l_brace = .{ .prefix = null, .infix = apply, .prec = Precedence.secondary },
    .r_brace = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .l_bracket = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .r_bracket = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .semicolon = .{ .prefix = noOp, .infix = null, .prec = Precedence.none },

    // Verbs
    .colon = .{ .prefix = Prefix(.colon), .infix = Infix(.assign), .prec = Precedence.secondary },
    .colon_colon = .{ .prefix = Prefix(.colon_colon), .infix = Infix(.global_assign), .prec = Precedence.secondary },
    .plus = .{ .prefix = Prefix(.plus), .infix = Infix(.add), .prec = Precedence.secondary },
    .plus_colon = .{ .prefix = Prefix(.plus_colon), .infix = Infix(.plus_assign), .prec = Precedence.secondary },
    .minus = .{ .prefix = Prefix(.minus), .infix = Infix(.subtract), .prec = Precedence.secondary },
    .minus_colon = .{ .prefix = Prefix(.minus_colon), .infix = Infix(.minus_assign), .prec = Precedence.secondary },
    .asterisk = .{ .prefix = Prefix(.asterisk), .infix = Infix(.multiply), .prec = Precedence.secondary },
    .asterisk_colon = .{ .prefix = Prefix(.asterisk_colon), .infix = Infix(.asterisk_assign), .prec = Precedence.secondary },
    .percent = .{ .prefix = Prefix(.percent), .infix = Infix(.divide), .prec = Precedence.secondary },
    .percent_colon = .{ .prefix = Prefix(.percent_colon), .infix = Infix(.percent_assign), .prec = Precedence.secondary },
    .bang = .{ .prefix = Prefix(.bang), .infix = Infix(.dict), .prec = Precedence.secondary },
    .bang_colon = .{ .prefix = Prefix(.bang_colon), .infix = Infix(.bang_assign), .prec = Precedence.secondary },
    .ampersand = .{ .prefix = Prefix(.ampersand), .infix = Infix(.lesser), .prec = Precedence.secondary },
    .ampersand_colon = .{ .prefix = Prefix(.ampersand_colon), .infix = Infix(.ampersand_assign), .prec = Precedence.secondary },
    .pipe = .{ .prefix = Prefix(.pipe), .infix = Infix(.greater), .prec = Precedence.secondary },
    .pipe_colon = .{ .prefix = Prefix(.pipe_colon), .infix = Infix(.pipe_assign), .prec = Precedence.secondary },
    .angle_bracket_left = .{ .prefix = Prefix(.angle_bracket_left), .infix = Infix(.less_than), .prec = Precedence.secondary },
    .angle_bracket_left_colon = .{ .prefix = Prefix(.angle_bracket_left_colon), .infix = Infix(.angle_bracket_left_assign), .prec = Precedence.secondary },
    .angle_bracket_left_equal = .{ .prefix = Prefix(.angle_bracket_left_equal), .infix = Infix(.less_than_equal), .prec = Precedence.secondary },
    .angle_bracket_left_right = .{ .prefix = Prefix(.angle_bracket_left_right), .infix = Infix(.not_equal), .prec = Precedence.secondary },
    .angle_bracket_right = .{ .prefix = Prefix(.angle_bracket_right), .infix = Infix(.greater_than), .prec = Precedence.secondary },
    .angle_bracket_right_colon = .{ .prefix = Prefix(.angle_bracket_right_colon), .infix = Infix(.angle_bracket_right_assign), .prec = Precedence.secondary },
    .angle_bracket_right_equal = .{ .prefix = Prefix(.angle_bracket_right_equal), .infix = Infix(.greater_than_equal), .prec = Precedence.secondary },
    .equal = .{ .prefix = Prefix(.equal), .infix = Infix(.equals), .prec = Precedence.secondary },
    .equal_colon = .{ .prefix = Prefix(.equal_colon), .infix = Infix(.equal_assign), .prec = Precedence.secondary },
    .tilde = .{ .prefix = Prefix(.tilde), .infix = Infix(.match), .prec = Precedence.secondary },
    .tilde_colon = .{ .prefix = Prefix(.tilde_colon), .infix = Infix(.tilde_assign), .prec = Precedence.secondary },
    .comma = .{ .prefix = Prefix(.comma), .infix = Infix(.join), .prec = Precedence.secondary },
    .comma_colon = .{ .prefix = Prefix(.comma_colon), .infix = Infix(.comma_assign), .prec = Precedence.secondary },
    .caret = .{ .prefix = Prefix(.caret), .infix = Infix(.fill), .prec = Precedence.secondary },
    .caret_colon = .{ .prefix = Prefix(.caret_colon), .infix = Infix(.caret_assign), .prec = Precedence.secondary },
    .hash = .{ .prefix = Prefix(.hash), .infix = Infix(.take), .prec = Precedence.secondary },
    .hash_colon = .{ .prefix = Prefix(.hash_colon), .infix = Infix(.hash_assign), .prec = Precedence.secondary },
    .underscore = .{ .prefix = Prefix(.underscore), .infix = Infix(.drop), .prec = Precedence.secondary },
    .underscore_colon = .{ .prefix = Prefix(.underscore_colon), .infix = Infix(.underscore_assign), .prec = Precedence.secondary },
    .dollar = .{ .prefix = Prefix(.dollar), .infix = Infix(.cast), .prec = Precedence.secondary },
    .dollar_colon = .{ .prefix = Prefix(.dollar_colon), .infix = Infix(.dollar_assign), .prec = Precedence.secondary },
    .question_mark = .{ .prefix = Prefix(.question_mark), .infix = Infix(.find), .prec = Precedence.secondary },
    .question_mark_colon = .{ .prefix = Prefix(.question_mark_colon), .infix = Infix(.question_mark_assign), .prec = Precedence.secondary },
    .at = .{ .prefix = Prefix(.at), .infix = Infix(.apply), .prec = Precedence.secondary },
    .at_colon = .{ .prefix = Prefix(.at_colon), .infix = Infix(.at_assign), .prec = Precedence.secondary },
    .dot = .{ .prefix = Prefix(.dot), .infix = Infix(.apply_n), .prec = Precedence.secondary },
    .dot_colon = .{ .prefix = Prefix(.dot_colon), .infix = Infix(.dot_assign), .prec = Precedence.secondary },
    .zero_colon = .{ .prefix = Prefix(.zero_colon), .infix = Infix(.file_text), .prec = Precedence.secondary },
    .zero_colon_colon = .{ .prefix = Prefix(.zero_colon_colon), .infix = Infix(.zero_colon_assign), .prec = Precedence.secondary },
    .one_colon = .{ .prefix = Prefix(.one_colon), .infix = Infix(.file_binary), .prec = Precedence.secondary },
    .one_colon_colon = .{ .prefix = Prefix(.one_colon_colon), .infix = Infix(.one_colon_assign), .prec = Precedence.secondary },
    .two_colon = .{ .prefix = Prefix(.two_colon), .infix = Infix(.dynamic_load), .prec = Precedence.secondary },

    // Adverbs
    .apostrophe = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .apostrophe_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .slash = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .slash_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .backslash = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .backslash_colon = .{ .prefix = null, .infix = null, .prec = Precedence.none },

    // Literals
    .number_literal = .{ .prefix = Prefix(.number_literal), .infix = apply, .prec = Precedence.secondary },
    .string_literal = .{ .prefix = Prefix(.string_literal), .infix = apply, .prec = Precedence.secondary },
    .symbol_literal = .{ .prefix = Prefix(.symbol_literal), .infix = apply, .prec = Precedence.secondary },
    .symbol_list_literal = .{ .prefix = Prefix(.symbol_list_literal), .infix = apply, .prec = Precedence.secondary },
    .identifier = .{ .prefix = Prefix(.identifier), .infix = apply, .prec = Precedence.secondary },

    // Misc.
    .comment = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .system = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .invalid = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .eof = .{ .prefix = null, .infix = null, .prec = Precedence.none },

    // Keywords
    .keyword_abs = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_acos = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_asin = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_atan = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_avg = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_bin = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_binr = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_cor = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_cos = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_cov = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_delete = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_dev = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_div = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_do = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_enlist = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_exec = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_exit = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_exp = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_getenv = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_hopen = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_if = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_in = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_insert = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_last = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_like = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_log = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_max = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_min = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_prd = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_select = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_setenv = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_sin = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_sqrt = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_ss = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_sum = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_tan = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_update = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_var = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_wavg = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_while = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_within = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_wsum = .{ .prefix = null, .infix = null, .prec = Precedence.none },
    .keyword_xexp = .{ .prefix = null, .infix = null, .prec = Precedence.none },
});

const null_node: Node.Index = 0;

const Parse = @This();
const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;
const Allocator = std.mem.Allocator;
const kdb = @import("../kdb.zig");
const Ast = kdb.Ast;
const Node = Ast.Node;
const AstError = Ast.Error;
const TokenIndex = Ast.TokenIndex;
const Token = kdb.Token;
const log = std.log.scoped(.kdbLint_Parse);

test {
    @import("std").testing.refAllDecls(@This());
}
