//! Represents in-progress parsing, will be converted to an Ast after completion.

pub const Error = error{ParseError} || Allocator.Error;

gpa: Allocator,
source: []const u8,
token_tags: []const Token.Tag,
token_locs: []const Token.Loc,
token_eobs: []const bool,
mode: Ast.Mode,
tok_i: TokenIndex = 0,
eob: bool = false,
ends_expr_tags: std.ArrayListUnmanaged(Token.Tag) = .{},
errors: std.ArrayListUnmanaged(AstError) = .{},
nodes: Ast.NodeList = .{},
extra_data: std.ArrayListUnmanaged(Node.Index) = .{},
scratch: std.ArrayListUnmanaged(Node.Index) = .{},

pub fn deinit(p: *Parse) void {
    p.ends_expr_tags.deinit(p.gpa);
    p.errors.deinit(p.gpa);
    p.nodes.deinit(p.gpa);
    p.extra_data.deinit(p.gpa);
    p.scratch.deinit(p.gpa);
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
    var node = try p.parseExpr(.semicolon);
    if (p.eatToken(.semicolon)) |_| {} else {
        node = try p.addNode(.{
            .tag = .implicit_return,
            .main_token = undefined,
            .data = .{
                .lhs = node,
                .rhs = undefined,
            },
        });
    }
    try p.nextBlock();
    return node;
}

fn parseExpr(p: *Parse, ends_expr: Token.Tag) Error!Node.Index {
    try p.ends_expr_tags.append(p.gpa, ends_expr);
    defer _ = p.ends_expr_tags.pop();

    return p.parsePrecedence(Precedence.secondary);
}

fn expectExpr(p: *Parse, ends_expr: Token.Tag) Error!Node.Index {
    const node = try p.parseExpr(ends_expr);
    if (node == 0) {
        return p.fail(.expected_expr);
    } else {
        return node;
    }
}

fn NoOp(comptime eat_token: bool) *const fn (*Parse) Error!Node.Index {
    return struct {
        fn impl(p: *Parse) Error!Node.Index {
            if (eat_token) _ = p.nextToken();
            return null_node;
        }
    }.impl;
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
            const rhs = try p.parsePrecedence(Precedence.secondary);
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
            .lhs = try p.expectExpr(.r_paren),
            .rhs = try p.expectToken(.r_paren),
        },
    });
}

fn lambda(p: *Parse) Error!Node.Index {
    const l_brace = p.assertToken(.l_brace);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    if (p.eatToken(.l_bracket)) |_| {
        if (p.peekTag() != .r_bracket) {
            while (true) {
                const identifier = try p.expectToken(.identifier);
                try p.scratch.append(p.gpa, identifier);
                if (p.eatToken(.semicolon)) |_| continue;
                break;
            }
        }
        _ = try p.expectToken(.r_bracket);
    }

    const body_top = p.scratch.items.len;

    while (true) {
        if (p.eob) return p.failExpected(.r_brace);
        if (p.eatToken(.r_brace)) |_| break;
        if (p.eatToken(.semicolon)) |_| continue;
        const expr = try p.parseExpr(.r_brace);
        if (expr > 0) {
            try p.scratch.append(p.gpa, expr);
        }
    }

    var i = p.tok_i - 2;
    while (true) : (i -= 1) {
        if (p.token_tags[i] != .comment) break;
    }
    const semicolon = p.token_tags[i] == .semicolon;
    const expressions = p.scratch.items[body_top..];

    const params_len = body_top - scratch_top;
    const params = if (params_len > 0) try p.addExtra(try p.listToSpan(p.scratch.items[scratch_top..body_top])) else null_node;
    switch (expressions.len) {
        0 => return p.addNode(.{
            .tag = if (semicolon) .lambda_one_semicolon else .lambda_one,
            .main_token = l_brace,
            .data = .{
                .lhs = params,
                .rhs = 0,
            },
        }),
        1 => return p.addNode(.{
            .tag = if (semicolon) .lambda_one_semicolon else .lambda_one,
            .main_token = l_brace,
            .data = .{
                .lhs = params,
                .rhs = expressions[0],
            },
        }),
        else => return p.addNode(.{
            .tag = if (semicolon) .lambda_semicolon else .lambda,
            .main_token = l_brace,
            .data = .{
                .lhs = params,
                .rhs = try p.addExtra(try p.listToSpan(expressions)),
            },
        }),
    }
}

fn block(p: *Parse) Error!Node.Index {
    const l_bracket = p.assertToken(.l_bracket);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.eob) return p.failExpected(.r_bracket);
        if (p.eatToken(.r_bracket)) |_| break;
        if (p.eatToken(.semicolon)) |_| continue;
        const expr = try p.parseExpr(.r_bracket);
        if (expr > 0) {
            try p.scratch.append(p.gpa, expr);
        }
    }

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        0 => return p.addNode(.{
            .tag = .block_two,
            .main_token = l_bracket,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        }),
        1 => return p.addNode(.{
            .tag = .block_two,
            .main_token = l_bracket,
            .data = .{
                .lhs = expressions[0],
                .rhs = 0,
            },
        }),
        2 => return p.addNode(.{
            .tag = .block_two,
            .main_token = l_bracket,
            .data = .{
                .lhs = expressions[0],
                .rhs = expressions[1],
            },
        }),
        else => {
            const span = try p.listToSpan(expressions);
            return p.addNode(.{
                .tag = .block,
                .main_token = l_bracket,
                .data = .{
                    .lhs = span.start,
                    .rhs = span.end,
                },
            });
        },
    }
}

fn do(p: *Parse) Error!Node.Index {
    const do_token = p.assertToken(.keyword_do);
    _ = try p.expectToken(.l_bracket);
    const iter = try p.expectExpr(.semicolon);
    _ = try p.expectToken(.semicolon);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.eob) return p.failExpected(.r_bracket);
        const expr = try p.parseExpr(.r_bracket);
        try p.scratch.append(p.gpa, expr);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_bracket => {
                _ = p.nextToken();
                break;
            },
            else => {},
        }
    }

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        0 => return p.addNode(.{
            .tag = .do_one,
            .main_token = do_token,
            .data = .{
                .lhs = iter,
                .rhs = 0,
            },
        }),
        1 => return p.addNode(.{
            .tag = .do_one,
            .main_token = do_token,
            .data = .{
                .lhs = iter,
                .rhs = expressions[0],
            },
        }),
        else => {
            return p.addNode(.{
                .tag = .do,
                .main_token = do_token,
                .data = .{
                    .lhs = iter,
                    .rhs = try p.addExtra(try p.listToSpan(expressions)),
                },
            });
        },
    }
}

fn @"if"(p: *Parse) Error!Node.Index {
    const if_token = p.assertToken(.keyword_if);
    _ = try p.expectToken(.l_bracket);
    const condition = try p.expectExpr(.semicolon);
    _ = try p.expectToken(.semicolon);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.eob) return p.failExpected(.r_bracket);
        const expr = try p.parseExpr(.r_bracket);
        try p.scratch.append(p.gpa, expr);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_bracket => {
                _ = p.nextToken();
                break;
            },
            else => {},
        }
    }

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        0 => return p.addNode(.{
            .tag = .if_one,
            .main_token = if_token,
            .data = .{
                .lhs = condition,
                .rhs = 0,
            },
        }),
        1 => return p.addNode(.{
            .tag = .if_one,
            .main_token = if_token,
            .data = .{
                .lhs = condition,
                .rhs = expressions[0],
            },
        }),
        else => {
            return p.addNode(.{
                .tag = .@"if",
                .main_token = if_token,
                .data = .{
                    .lhs = condition,
                    .rhs = try p.addExtra(try p.listToSpan(expressions)),
                },
            });
        },
    }
}

fn @"while"(p: *Parse) Error!Node.Index {
    const while_token = p.assertToken(.keyword_while);
    _ = try p.expectToken(.l_bracket);
    const condition = try p.expectExpr(.semicolon);
    _ = try p.expectToken(.semicolon);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.eob) return p.failExpected(.r_bracket);
        const expr = try p.parseExpr(.r_bracket);
        try p.scratch.append(p.gpa, expr);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_bracket => {
                _ = p.nextToken();
                break;
            },
            else => {},
        }
    }

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        0 => return p.addNode(.{
            .tag = .while_one,
            .main_token = while_token,
            .data = .{
                .lhs = condition,
                .rhs = 0,
            },
        }),
        1 => return p.addNode(.{
            .tag = .while_one,
            .main_token = while_token,
            .data = .{
                .lhs = condition,
                .rhs = expressions[0],
            },
        }),
        else => {
            return p.addNode(.{
                .tag = .@"while",
                .main_token = while_token,
                .data = .{
                    .lhs = condition,
                    .rhs = try p.addExtra(try p.listToSpan(expressions)),
                },
            });
        },
    }
}

fn select(p: *Parse) Error!Node.Index {
    _ = p; // autofix
    unreachable;
}

fn exec(p: *Parse) Error!Node.Index {
    _ = p; // autofix
    unreachable;
}

fn update(p: *Parse) Error!Node.Index {
    _ = p; // autofix
    unreachable;
}

fn delete(p: *Parse) Error!Node.Index {
    _ = p; // autofix
    unreachable;
}

fn applyNumber(p: *Parse, lhs: Node.Index) Error!Node.Index {
    if (p.nodes.items(.tag)[lhs] == .number_literal) {
        p.nodes.items(.data)[lhs].rhs = try p.parsePrecedence(.primary);
        return lhs;
    }
    return p.apply(lhs);
}

fn apply(p: *Parse, lhs: Node.Index) Error!Node.Index {
    return p.addNode(.{
        .tag = .implicit_apply,
        .main_token = undefined,
        .data = .{
            .lhs = lhs,
            .rhs = try p.parsePrecedence(Precedence.secondary),
        },
    });
}

fn call(p: *Parse, lhs: Node.Index) Error!Node.Index {
    const l_paren = p.assertToken(.l_bracket);

    const scratch_top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(scratch_top);

    while (true) {
        if (p.eob) return p.failExpected(.r_bracket);
        const expr = try p.parseExpr(.r_bracket);
        try p.scratch.append(p.gpa, expr);
        switch (p.peekTag()) {
            .semicolon => _ = p.nextToken(),
            .r_bracket => {
                _ = p.nextToken();
                break;
            },
            else => {},
        }
    }

    const params = p.scratch.items[scratch_top..];

    const expressions = p.scratch.items[scratch_top..];
    switch (expressions.len) {
        0 => unreachable,
        1 => return p.addNode(.{
            .tag = .call_one,
            .main_token = l_paren,
            .data = .{
                .lhs = lhs,
                .rhs = params[0],
            },
        }),
        else => {
            return p.addNode(.{
                .tag = .call,
                .main_token = l_paren,
                .data = .{
                    .lhs = lhs,
                    .rhs = try p.addExtra(try p.listToSpan(params)),
                },
            });
        },
    }
}

fn tokensOnSameLine(p: *Parse, token1: TokenIndex, token2: TokenIndex) bool {
    return std.mem.indexOfScalar(u8, p.source[p.token_locs[token1].start..p.token_locs[token2].start], '\n') == null;
}

fn skipComments(p: *Parse) void {
    while (p.token_tags[p.tok_i] == .comment) {
        p.tok_i += 1;
    }
}

fn peekTag(p: *Parse) Token.Tag {
    p.skipComments();
    // TODO: What if skipComments brought us to eof?
    return p.token_tags[p.tok_i];
}

fn eatToken(p: *Parse, tag: Token.Tag) ?TokenIndex {
    return if (p.peekTag() == tag) p.nextToken() else null;
}

fn assertToken(p: *Parse, tag: Token.Tag) TokenIndex {
    const token = p.nextToken();
    assert(p.token_tags[token] == tag);
    return token;
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

fn nextToken(p: *Parse) TokenIndex {
    if (p.eob) return null_node;

    p.skipComments();
    const result = p.tok_i;
    p.eob = p.token_eobs[result];
    log.debug("nextToken = '{s}' {}", .{ p.source[p.token_locs[result].start..p.token_locs[result].end], p.eob });
    p.tok_i += 1;
    return result;
}

fn nextBlock(p: *Parse) Error!void {
    // assert(p.eob);
    if (!p.eob) {
        try p.warn(.expected_block);
        while (!p.eob) {
            _ = p.nextToken();
        }
    }
    p.eob = false;
}

fn getRule(p: *Parse) OperInfo {
    if (p.eob) {
        return .{ .prefix = NoOp(true), .infix = null, .prec = .none };
    }
    const tag = p.peekTag();
    if (p.ends_expr_tags.getLastOrNull()) |t| if (tag == t or tag == .semicolon) {
        return .{ .prefix = NoOp(false), .infix = null, .prec = .none };
    };
    return operTable[@intFromEnum(tag)];
}

fn parsePrecedence(p: *Parse, precedence: Precedence) Error!Node.Index {
    const prefix = p.getRule().prefix orelse {
        try p.warn(.expected_prefix_expr);
        _ = p.nextToken();
        return null_node;
    };
    var node = try prefix(p);

    while (@intFromEnum(precedence) <= @intFromEnum(p.getRule().prec)) {
        const infix = p.getRule().infix orelse {
            try p.warn(.expected_infix_expr);
            _ = p.nextToken();
            break;
        };
        node = try infix(p, node);
    }

    return node;
}

const operTable = std.enums.directEnumArray(Token.Tag, OperInfo, 0, .{
    // Punctuation
    .l_paren = .{ .prefix = grouping, .infix = apply, .prec = .secondary },
    .r_paren = .{ .prefix = null, .infix = null, .prec = .none },
    .l_brace = .{ .prefix = lambda, .infix = apply, .prec = .secondary },
    .r_brace = .{ .prefix = null, .infix = null, .prec = .none },
    .l_bracket = .{ .prefix = block, .infix = call, .prec = .secondary },
    .r_bracket = .{ .prefix = null, .infix = null, .prec = .none },
    .semicolon = .{ .prefix = null, .infix = null, .prec = .none },

    // Verbs
    .colon = .{ .prefix = Prefix(.colon), .infix = Infix(.assign), .prec = .secondary },
    .colon_colon = .{ .prefix = Prefix(.colon_colon), .infix = Infix(.global_assign), .prec = .secondary },
    .plus = .{ .prefix = Prefix(.plus), .infix = Infix(.add), .prec = .secondary },
    .plus_colon = .{ .prefix = Prefix(.plus_colon), .infix = Infix(.plus_assign), .prec = .secondary },
    .minus = .{ .prefix = Prefix(.minus), .infix = Infix(.subtract), .prec = .secondary },
    .minus_colon = .{ .prefix = Prefix(.minus_colon), .infix = Infix(.minus_assign), .prec = .secondary },
    .asterisk = .{ .prefix = Prefix(.asterisk), .infix = Infix(.multiply), .prec = .secondary },
    .asterisk_colon = .{ .prefix = Prefix(.asterisk_colon), .infix = Infix(.asterisk_assign), .prec = .secondary },
    .percent = .{ .prefix = Prefix(.percent), .infix = Infix(.divide), .prec = .secondary },
    .percent_colon = .{ .prefix = Prefix(.percent_colon), .infix = Infix(.percent_assign), .prec = .secondary },
    .bang = .{ .prefix = Prefix(.bang), .infix = Infix(.dict), .prec = .secondary },
    .bang_colon = .{ .prefix = Prefix(.bang_colon), .infix = Infix(.bang_assign), .prec = .secondary },
    .ampersand = .{ .prefix = Prefix(.ampersand), .infix = Infix(.lesser), .prec = .secondary },
    .ampersand_colon = .{ .prefix = Prefix(.ampersand_colon), .infix = Infix(.ampersand_assign), .prec = .secondary },
    .pipe = .{ .prefix = Prefix(.pipe), .infix = Infix(.greater), .prec = .secondary },
    .pipe_colon = .{ .prefix = Prefix(.pipe_colon), .infix = Infix(.pipe_assign), .prec = .secondary },
    .angle_bracket_left = .{ .prefix = Prefix(.angle_bracket_left), .infix = Infix(.less_than), .prec = .secondary },
    .angle_bracket_left_colon = .{ .prefix = Prefix(.angle_bracket_left_colon), .infix = Infix(.angle_bracket_left_assign), .prec = .secondary },
    .angle_bracket_left_equal = .{ .prefix = Prefix(.angle_bracket_left_equal), .infix = Infix(.less_than_equal), .prec = .secondary },
    .angle_bracket_left_right = .{ .prefix = Prefix(.angle_bracket_left_right), .infix = Infix(.not_equal), .prec = .secondary },
    .angle_bracket_right = .{ .prefix = Prefix(.angle_bracket_right), .infix = Infix(.greater_than), .prec = .secondary },
    .angle_bracket_right_colon = .{ .prefix = Prefix(.angle_bracket_right_colon), .infix = Infix(.angle_bracket_right_assign), .prec = .secondary },
    .angle_bracket_right_equal = .{ .prefix = Prefix(.angle_bracket_right_equal), .infix = Infix(.greater_than_equal), .prec = .secondary },
    .equal = .{ .prefix = Prefix(.equal), .infix = Infix(.equals), .prec = .secondary },
    .equal_colon = .{ .prefix = Prefix(.equal_colon), .infix = Infix(.equal_assign), .prec = .secondary },
    .tilde = .{ .prefix = Prefix(.tilde), .infix = Infix(.match), .prec = .secondary },
    .tilde_colon = .{ .prefix = Prefix(.tilde_colon), .infix = Infix(.tilde_assign), .prec = .secondary },
    .comma = .{ .prefix = Prefix(.comma), .infix = Infix(.join), .prec = .secondary },
    .comma_colon = .{ .prefix = Prefix(.comma_colon), .infix = Infix(.comma_assign), .prec = .secondary },
    .caret = .{ .prefix = Prefix(.caret), .infix = Infix(.fill), .prec = .secondary },
    .caret_colon = .{ .prefix = Prefix(.caret_colon), .infix = Infix(.caret_assign), .prec = .secondary },
    .hash = .{ .prefix = Prefix(.hash), .infix = Infix(.take), .prec = .secondary },
    .hash_colon = .{ .prefix = Prefix(.hash_colon), .infix = Infix(.hash_assign), .prec = .secondary },
    .underscore = .{ .prefix = Prefix(.underscore), .infix = Infix(.drop), .prec = .secondary },
    .underscore_colon = .{ .prefix = Prefix(.underscore_colon), .infix = Infix(.underscore_assign), .prec = .secondary },
    .dollar = .{ .prefix = Prefix(.dollar), .infix = Infix(.cast), .prec = .secondary },
    .dollar_colon = .{ .prefix = Prefix(.dollar_colon), .infix = Infix(.dollar_assign), .prec = .secondary },
    .question_mark = .{ .prefix = Prefix(.question_mark), .infix = Infix(.find), .prec = .secondary },
    .question_mark_colon = .{ .prefix = Prefix(.question_mark_colon), .infix = Infix(.question_mark_assign), .prec = .secondary },
    .at = .{ .prefix = Prefix(.at), .infix = Infix(.apply), .prec = .secondary },
    .at_colon = .{ .prefix = Prefix(.at_colon), .infix = Infix(.at_assign), .prec = .secondary },
    .dot = .{ .prefix = Prefix(.dot), .infix = Infix(.apply_n), .prec = .secondary },
    .dot_colon = .{ .prefix = Prefix(.dot_colon), .infix = Infix(.dot_assign), .prec = .secondary },
    .zero_colon = .{ .prefix = Prefix(.zero_colon), .infix = Infix(.file_text), .prec = .secondary },
    .zero_colon_colon = .{ .prefix = Prefix(.zero_colon_colon), .infix = Infix(.zero_colon_assign), .prec = .secondary },
    .one_colon = .{ .prefix = Prefix(.one_colon), .infix = Infix(.file_binary), .prec = .secondary },
    .one_colon_colon = .{ .prefix = Prefix(.one_colon_colon), .infix = Infix(.one_colon_assign), .prec = .secondary },
    .two_colon = .{ .prefix = Prefix(.two_colon), .infix = Infix(.dynamic_load), .prec = .secondary },

    // Adverbs
    .apostrophe = .{ .prefix = null, .infix = null, .prec = .none },
    .apostrophe_colon = .{ .prefix = null, .infix = null, .prec = .none },
    .slash = .{ .prefix = null, .infix = null, .prec = .none },
    .slash_colon = .{ .prefix = null, .infix = null, .prec = .none },
    .backslash = .{ .prefix = null, .infix = null, .prec = .none },
    .backslash_colon = .{ .prefix = null, .infix = null, .prec = .none },

    // Literals
    .number_literal = .{ .prefix = Prefix(.number_literal), .infix = applyNumber, .prec = .primary },
    .string_literal = .{ .prefix = Prefix(.string_literal), .infix = apply, .prec = .secondary },
    .symbol_literal = .{ .prefix = Prefix(.symbol_literal), .infix = apply, .prec = .secondary },
    .symbol_list_literal = .{ .prefix = Prefix(.symbol_list_literal), .infix = apply, .prec = .secondary },
    .identifier = .{ .prefix = Prefix(.identifier), .infix = apply, .prec = .secondary },

    // Misc.
    .comment = .{ .prefix = null, .infix = null, .prec = .none },
    .system = .{ .prefix = null, .infix = null, .prec = .none },
    .invalid = .{ .prefix = null, .infix = null, .prec = .none },
    .eof = .{ .prefix = null, .infix = null, .prec = .none },

    // Keywords
    .keyword_abs = .{ .prefix = Prefix(.abs), .infix = apply, .prec = .secondary },
    .keyword_acos = .{ .prefix = Prefix(.acos), .infix = apply, .prec = .secondary },
    .keyword_asin = .{ .prefix = Prefix(.asin), .infix = apply, .prec = .secondary },
    .keyword_atan = .{ .prefix = Prefix(.atan), .infix = apply, .prec = .secondary },
    .keyword_avg = .{ .prefix = Prefix(.avg), .infix = apply, .prec = .secondary },
    .keyword_bin = .{ .prefix = Prefix(.bin), .infix = Infix(.bin_infix), .prec = .secondary },
    .keyword_binr = .{ .prefix = Prefix(.binr), .infix = Infix(.binr_infix), .prec = .secondary },
    .keyword_cor = .{ .prefix = Prefix(.cor), .infix = Infix(.cor_infix), .prec = .secondary },
    .keyword_cos = .{ .prefix = Prefix(.cos), .infix = apply, .prec = .secondary },
    .keyword_cov = .{ .prefix = Prefix(.cov), .infix = Infix(.cov_infix), .prec = .secondary },
    .keyword_delete = .{ .prefix = delete, .infix = apply, .prec = .secondary },
    .keyword_dev = .{ .prefix = Prefix(.dev), .infix = apply, .prec = .secondary },
    .keyword_div = .{ .prefix = Prefix(.div), .infix = Infix(.div_infix), .prec = .secondary },
    .keyword_do = .{ .prefix = do, .infix = apply, .prec = .secondary },
    .keyword_enlist = .{ .prefix = Prefix(.enlist), .infix = apply, .prec = .secondary },
    .keyword_exec = .{ .prefix = exec, .infix = apply, .prec = .secondary },
    .keyword_exit = .{ .prefix = Prefix(.exit), .infix = apply, .prec = .secondary },
    .keyword_exp = .{ .prefix = Prefix(.exp), .infix = apply, .prec = .secondary },
    .keyword_getenv = .{ .prefix = Prefix(.getenv), .infix = apply, .prec = .secondary },
    .keyword_hopen = .{ .prefix = Prefix(.hopen), .infix = apply, .prec = .secondary },
    .keyword_if = .{ .prefix = @"if", .infix = apply, .prec = .secondary },
    .keyword_in = .{ .prefix = Prefix(.in), .infix = Infix(.in_infix), .prec = .secondary },
    .keyword_insert = .{ .prefix = Prefix(.insert), .infix = Infix(.insert_infix), .prec = .secondary },
    .keyword_last = .{ .prefix = Prefix(.last), .infix = apply, .prec = .secondary },
    .keyword_like = .{ .prefix = Prefix(.like), .infix = Infix(.like_infix), .prec = .secondary },
    .keyword_log = .{ .prefix = Prefix(.log), .infix = apply, .prec = .secondary },
    .keyword_max = .{ .prefix = Prefix(.max), .infix = apply, .prec = .secondary },
    .keyword_min = .{ .prefix = Prefix(.min), .infix = apply, .prec = .secondary },
    .keyword_prd = .{ .prefix = Prefix(.prd), .infix = apply, .prec = .secondary },
    .keyword_select = .{ .prefix = select, .infix = apply, .prec = .secondary },
    .keyword_setenv = .{ .prefix = Prefix(.setenv), .infix = Infix(.setenv_infix), .prec = .secondary },
    .keyword_sin = .{ .prefix = Prefix(.sin), .infix = apply, .prec = .secondary },
    .keyword_sqrt = .{ .prefix = Prefix(.sqrt), .infix = apply, .prec = .secondary },
    .keyword_ss = .{ .prefix = Prefix(.ss), .infix = Infix(.ss_infix), .prec = .secondary },
    .keyword_sum = .{ .prefix = Prefix(.sum), .infix = apply, .prec = .secondary },
    .keyword_tan = .{ .prefix = Prefix(.tan), .infix = apply, .prec = .secondary },
    .keyword_update = .{ .prefix = update, .infix = apply, .prec = .secondary },
    .keyword_var = .{ .prefix = Prefix(.@"var"), .infix = apply, .prec = .secondary },
    .keyword_wavg = .{ .prefix = Prefix(.wavg), .infix = Infix(.wavg_infix), .prec = .secondary },
    .keyword_while = .{ .prefix = @"while", .infix = apply, .prec = .secondary },
    .keyword_within = .{ .prefix = Prefix(.within), .infix = Infix(.within_infix), .prec = .secondary },
    .keyword_wsum = .{ .prefix = Prefix(.wsum), .infix = Infix(.wsum_infix), .prec = .secondary },
    .keyword_xexp = .{ .prefix = Prefix(.xexp), .infix = Infix(.xexp_infix), .prec = .secondary },
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

fn appendTags(tree: Ast, i: Node.Index, tags: *std.ArrayList(Node.Tag)) !void {
    const tag = tree.nodes.items(.tag)[i];
    try tags.append(tag);

    switch (tree.nodes.items(.tag)[i]) {
        .implicit_return => {
            const data = tree.nodes.items(.data)[i];
            try appendTags(tree, data.lhs, tags);
        },
        .call_one, .add => {
            const data = tree.nodes.items(.data)[i];
            if (data.rhs > 0) {
                try appendTags(tree, data.rhs, tags);
            }
            try appendTags(tree, data.lhs, tags);
        },
        .lambda_one,
        .lambda_one_semicolon,
        => {
            const data = tree.nodes.items(.data)[i];
            if (data.rhs > 0) {
                try appendTags(tree, data.rhs, tags);
            }
        },
        .lambda,
        .lambda_semicolon,
        => {
            const data = tree.nodes.items(.data)[i];
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            for (sub_range.start..sub_range.end) |extra_data_i| {
                const node_i = tree.extra_data[extra_data_i];
                try appendTags(tree, node_i, tags);
            }
        },
        .identifier, .number_literal => {},
        .call => {
            const data = tree.nodes.items(.data)[i];
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            for (sub_range.start..sub_range.end, 0..) |_, temp_i| {
                const extra_data_i = sub_range.end - temp_i - 1;
                const node_i = tree.extra_data[extra_data_i];
                if (node_i > 0) {
                    try appendTags(tree, node_i, tags);
                }
            }
            try appendTags(tree, data.lhs, tags);
        },
        else => |t| panic("{s}", .{@tagName(t)}),
    }
}

fn testParse(source: [:0]const u8, expected_tags: []const Node.Tag, expected_parse_tree: []const u8) !void {
    inline for (&.{ .k, .q }) |mode| {
        try testParseMode(mode, source, expected_tags, expected_parse_tree);
    }
}

fn testParseMode(comptime mode: Ast.Mode, source: [:0]const u8, expected_tags: []const Node.Tag, expected_parse_tree: []const u8) !void {
    var tree = try Ast.parse(std.testing.allocator, source, mode);
    defer tree.deinit(std.testing.allocator);

    const errors = try std.testing.allocator.alloc(Ast.Error.Tag, tree.errors.len);
    defer std.testing.allocator.free(errors);
    for (tree.errors, 0..) |err, i| {
        errors[i] = err.tag;
    }
    try std.testing.expectEqualSlices(Ast.Error.Tag, &.{}, errors);

    const data = tree.nodes.items(.data)[0];
    const i = tree.extra_data[data.lhs];

    var tags = std.ArrayList(Node.Tag).init(std.testing.allocator);
    defer tags.deinit();
    try appendTags(tree, i, &tags);

    try std.testing.expectEqualSlices(Node.Tag, expected_tags, tags.items);

    var parse_tree = std.ArrayList(u8).init(std.testing.allocator);
    defer parse_tree.deinit();
    try tree.print(i, parse_tree.writer(), std.testing.allocator);

    try std.testing.expectEqualSlices(u8, expected_parse_tree, parse_tree.items);
}

test "expressions" {
    try testParse("1", &.{ .implicit_return, .number_literal }, "1");
    try testParse("1;", &.{.number_literal}, "1");
}

test "lambda" {
    try testParse("{x}", &.{
        .implicit_return,
        .lambda_one,
        .identifier,
    }, "{x}");
    try testParse("{x;}", &.{
        .implicit_return,
        .lambda_one_semicolon,
        .identifier,
    }, "{x;}");
    try testParse("{x;y}", &.{
        .implicit_return,
        .lambda,
        .identifier,
        .identifier,
    }, "{x;y}");
    try testParse("{x;y;}", &.{
        .implicit_return,
        .lambda_semicolon,
        .identifier,
        .identifier,
    }, "{x;y;}");
}

test "call" {
    try testParse("{x}[]", &.{
        .implicit_return,
        .call_one,
        .lambda_one,
        .identifier,
    }, "({x};::)");
    try testParse("{x}[1]", &.{
        .implicit_return,
        .call_one,
        .number_literal,
        .lambda_one,
        .identifier,
    }, "({x};1)");
    try testParse("{x+y}[1;a]", &.{
        .implicit_return,
        .call,
        .identifier,
        .number_literal,
        .lambda_one,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};1;`a)");
}

test "call - explicit projection" {
    try testParse("{x+y}[;]", &.{
        .implicit_return,
        .call,
        .lambda_one,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};::;::)");
    try testParse("{x+y}[1;]", &.{
        .implicit_return,
        .call,
        .number_literal,
        .lambda_one,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};1;::)");
    try testParse("{x+y}[;2]", &.{
        .implicit_return,
        .call,
        .number_literal,
        .lambda_one,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};::;2)");
}

test "call - implicit projection" {
    try testParse("{x+y}[1]", &.{
        .implicit_return,
        .call_one,
        .number_literal,
        .lambda_one,
        .add,
        .identifier,
        .identifier,
    }, "({x+y};1)");
}

test {
    @import("std").testing.refAllDecls(@This());
}
