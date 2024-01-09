const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,
    eob: bool,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const keywords = std.ComptimeStringMap(Tag, .{});

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const Tag = enum {
        // Punctuation
        l_paren,
        r_paren,
        l_brace,
        r_brace,
        l_bracket,
        r_bracket,
        semicolon,

        // Verbs
        colon,
        colon_colon,
        plus,
        plus_colon,
        minus,
        minus_colon,
        asterisk,
        asterisk_colon,
        percent,
        percent_colon,
        bang,
        bang_colon,
        ampersand,
        ampersand_colon,
        pipe,
        pipe_colon,
        angle_bracket_left,
        angle_bracket_left_angle_bracket_right,
        angle_bracket_left_colon,
        angle_bracket_left_equal,
        angle_bracket_right,
        angle_bracket_right_colon,
        angle_bracket_right_equal,
        equal,
        equal_colon,
        tilde,
        tilde_colon,
        comma,
        comma_colon,
        caret,
        caret_colon,
        hash,
        hash_colon,
        underscore,
        underscore_colon,
        dollar,
        dollar_colon,
        question_mark,
        question_mark_colon,
        at,
        at_colon,
        dot,
        dot_colon,
        zero_colon,
        zero_colon_colon,
        one_colon,
        one_colon_colon,
        two_colon,

        // Adverbs
        apostrophe,
        apostrophe_colon,
        slash,
        slash_colon,
        backslash,
        backslash_colon,

        // Literals
        number_literal,
        number_list_literal,
        string_literal,
        symbol_literal,
        symbol_list_literal,
        identifier,

        // Misc.
        comment,
        system,
        invalid,
        eof,

        // Keywords : -1","sv"keyword_",/:string .Q.res;
        keyword_abs,
        keyword_acos,
        keyword_asin,
        keyword_atan,
        keyword_avg,
        keyword_bin,
        keyword_binr,
        keyword_cor,
        keyword_cos,
        keyword_cov,
        keyword_delete,
        keyword_dev,
        keyword_div,
        keyword_do,
        keyword_enlist,
        keyword_exec,
        keyword_exit,
        keyword_exp,
        keyword_getenv,
        keyword_hopen,
        keyword_if,
        keyword_in,
        keyword_insert,
        keyword_last,
        keyword_like,
        keyword_log,
        keyword_max,
        keyword_min,
        keyword_prd,
        keyword_select,
        keyword_setenv,
        keyword_sin,
        keyword_sqrt,
        keyword_ss,
        keyword_sum,
        keyword_tan,
        keyword_update,
        keyword_var,
        keyword_wavg,
        keyword_while,
        keyword_within,
        keyword_wsum,
        keyword_xexp,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .number_literal,
                .number_list_literal,
                .string_literal,
                .symbol_literal,
                .symbol_list_literal,
                .identifier,
                .comment,
                .system,
                .invalid,
                .eob,
                .eof,
                => null,

                .l_paren => "(",
                .r_paren => ")",
                .l_brace => "{",
                .r_brace => "}",
                .l_bracket => "[",
                .r_bracket => "]",
                .semicolon => ";",
                .colon => ":",
                .colon_colon => "::",
                .plus => "+",
                .plus_colon => "+:",
                .minus => "-",
                .minus_colon => "-:",
                .asterisk => "*",
                .asterisk_colon => "*:",
                .percent => "%",
                .percent_colon => "%:",
                .bang => "!",
                .bang_colon => "!:",
                .ampersand => "&",
                .ampersand_colon => "&:",
                .pipe => "|",
                .pipe_colon => "|:",
                .angle_bracket_left => "<",
                .angle_bracket_left_angle_bracket_right => "<>",
                .angle_bracket_left_colon => "<:",
                .angle_bracket_left_equal => "<=",
                .angle_bracket_right => ">",
                .angle_bracket_right_colon => ">:",
                .angle_bracket_right_equal => ">=",
                .equal => "=",
                .equal_colon => "=:",
                .tilde => "~",
                .tilde_colon => "~:",
                .comma => ",",
                .comma_colon => ",:",
                .caret => "^",
                .caret_colon => "^:",
                .hash => "#",
                .hash_colon => "#:",
                .underscore => "_",
                .underscore_colon => "_:",
                .dollar => "$",
                .dollar_colon => "$:",
                .question_mark => "?",
                .question_mark_colon => "?:",
                .at => "@",
                .at_colon => "@:",
                .dot => ".",
                .dot_colon => ".:",
                .zero_colon => "0:",
                .zero_colon_colon => "0::",
                .one_colon => "1:",
                .one_colon_colon => "1::",
                .two_colon => "2:",
                .apostrophe => "'",
                .apostrophe_colon => "':",
                .slash => "/",
                .slash_colon => "/:",
                .backslash => "\\",
                .backslash_colon => "\\:",

                .keyword_abs => "abs",
                .keyword_acos => "acos",
                .keyword_asin => "asin",
                .keyword_atan => "atan",
                .keyword_avg => "avg",
                .keyword_bin => "bin",
                .keyword_binr => "binr",
                .keyword_cor => "cor",
                .keyword_cos => "cos",
                .keyword_cov => "cov",
                .keyword_delete => "delete",
                .keyword_dev => "dev",
                .keyword_div => "div",
                .keyword_do => "do",
                .keyword_enlist => "enlist",
                .keyword_exec => "exec",
                .keyword_exit => "exit",
                .keyword_exp => "exp",
                .keyword_getenv => "getenv",
                .keyword_hopen => "hopen",
                .keyword_if => "if",
                .keyword_in => "in",
                .keyword_insert => "insert",
                .keyword_last => "last",
                .keyword_like => "like",
                .keyword_log => "log",
                .keyword_max => "max",
                .keyword_min => "min",
                .keyword_prd => "prd",
                .keyword_select => "select",
                .keyword_setenv => "setenv",
                .keyword_sin => "sin",
                .keyword_sqrt => "sqrt",
                .keyword_ss => "ss",
                .keyword_sum => "sum",
                .keyword_tan => "tan",
                .keyword_update => "update",
                .keyword_var => "var",
                .keyword_wavg => "wavg",
                .keyword_while => "while",
                .keyword_within => "within",
                .keyword_wsum => "wsum",
                .keyword_xexp => "xexp",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .number_literal => "a number literal",
                .number_list_literal => "a number list literal",
                .string_literal => "a string literal",
                .symbol_literal => "a symbol literal",
                .symbol_list_literal => " a symbol list literal",
                .identifier => "an identifier",
                .comment => "a comment",
                .system => "a system command",
                .invalid => "invalid bytes",
                .eob => "end of block",
                .eof => "EOF",
                else => unreachable,
            };
        }
    };
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    parens_count: u32 = 0,
    braces_count: u32 = 0,
    brackets_count: u32 = 0,

    /// For debugging purposes
    pub fn dump(self: *Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // Skip the UTF-8 BOM if present
        const src_start: usize = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0;
        return Tokenizer{
            .buffer = buffer,
            .index = src_start,
        };
    }

    const State = enum {
        start,
    };

    pub fn next(self: *Tokenizer) Token {
        const state: State = .start;
        var result = Token{
            .tag = .eof,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
            .eob = true,
        };

        while (true) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                .start => switch (c) {
                    0 => {
                        if (self.index == self.buffer.len) {
                            break;
                        }
                        result.tag = .invalid;
                        self.index += 1;
                        break;
                    },
                    ' ', '\n', '\t', '\r' => {
                        result.loc.start = self.index + 1;
                    },
                    '(' => {
                        result.tag = .l_paren;
                        self.index += 1;
                        self.parens_count += 1;
                        break;
                    },
                    ')' => {
                        result.tag = .r_paren;
                        self.index += 1;
                        self.parens_count = @max(0, @as(i32, @intCast(self.parens_count)) - 1);
                        break;
                    },
                    '{' => {
                        result.tag = .l_brace;
                        self.index += 1;
                        self.braces_count += 1;
                        break;
                    },
                    '}' => {
                        result.tag = .r_brace;
                        self.index += 1;
                        self.braces_count = @max(0, @as(i32, @intCast(self.braces_count)) - 1);
                        break;
                    },
                    '[' => {
                        result.tag = .l_bracket;
                        self.index += 1;
                        self.brackets_count += 1;
                        break;
                    },
                    ']' => {
                        result.tag = .r_bracket;
                        self.index += 1;
                        self.brackets_count = @max(0, @as(i32, @intCast(self.brackets_count)) - 1);
                        break;
                    },
                    ';' => {
                        result.tag = .semicolon;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .invalid;
                        self.index += 1;
                        break;
                    },
                },
            }
        }

        result.loc.end = self.index;

        if (result.tag != .semicolon or self.parens_count != 0 or self.braces_count != 0 or self.brackets_count != 0) {
            for (self.buffer[self.index..]) |c| {
                if (!std.ascii.isWhitespace(c)) {
                    result.eob = false;
                    break;
                }

                self.index += 1;
                if (c == '\n' and !std.ascii.isWhitespace(self.buffer[self.index])) {
                    self.parens_count = 0;
                    self.braces_count = 0;
                    self.brackets_count = 0;
                    break;
                }
            }
        }

        return result;
    }
};

fn testTokenize(source: [:0]const u8, expected: []const Token) !void {
    var tokenizer = Tokenizer.init(source);

    const actual = try std.testing.allocator.alloc(Token, expected.len);
    defer std.testing.allocator.free(actual);
    for (actual) |*token| {
        token.* = tokenizer.next();
    }

    try std.testing.expectEqualSlices(Token, expected, actual);
    const last_token = tokenizer.next();
    try std.testing.expectEqual(Token{
        .tag = .eof,
        .loc = .{ .start = source.len, .end = source.len },
        .eob = true,
    }, last_token);
}

test "tokenize blocks" {
    try testTokenize("1", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = true },
    });
    try testTokenize("1\n", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = true },
    });
    try testTokenize("1\n2", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 2, .end = 3 }, .eob = true },
    });
    try testTokenize("1\n 2", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1 \n2", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1 \n 2", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("1 \n2 ", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1 \n 2 ", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });

    try testTokenize("1;", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 1, .end = 2 }, .eob = true },
    });
    try testTokenize("1 ;", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
    });
    try testTokenize("1;2", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 1, .end = 2 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 2, .end = 3 }, .eob = true },
    });
    try testTokenize("1 ;2", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1; 2", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 1, .end = 2 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1 ; 2", &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });

    try testTokenize("(1;2)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("((1;2);3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_paren, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("{1;2}", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("{{1;2};3}", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_brace, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("[1;2]", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("[[1;2];3]", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });

    try testTokenize("(1;\n2;3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 5, .end = 6 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 7, .end = 8 }, .eob = true },
    });
    try testTokenize("(1; \n2;3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("(1;\n 2;3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("(1; \n 2;3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 8, .end = 9 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 9, .end = 10 }, .eob = true },
    });
}

test "bracket counting overflow" {
    try testTokenize(")", &.{
        .{ .tag = .r_paren, .loc = .{ .start = 0, .end = 1 }, .eob = true },
    });
    try testTokenize("}", &.{
        .{ .tag = .r_brace, .loc = .{ .start = 0, .end = 1 }, .eob = true },
    });
    try testTokenize("]", &.{
        .{ .tag = .r_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = true },
    });
}

test {
    @import("std").testing.refAllDecls(@This());
}
