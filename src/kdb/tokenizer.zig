const std = @import("std");
const Ast = @import("Ast.zig");
const Mode = Ast.Mode;

pub const Token = struct {
    tag: Tag,
    loc: Loc,
    eob: bool,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const keywords = std.ComptimeStringMap(Tag, .{
        .{ "abs", .keyword_abs },
        .{ "acos", .keyword_acos },
        .{ "asin", .keyword_asin },
        .{ "atan", .keyword_atan },
        .{ "avg", .keyword_avg },
        .{ "bin", .keyword_bin },
        .{ "binr", .keyword_binr },
        .{ "cor", .keyword_cor },
        .{ "cos", .keyword_cos },
        .{ "cov", .keyword_cov },
        .{ "delete", .keyword_delete },
        .{ "dev", .keyword_dev },
        .{ "div", .keyword_div },
        .{ "do", .keyword_do },
        .{ "enlist", .keyword_enlist },
        .{ "exec", .keyword_exec },
        .{ "exit", .keyword_exit },
        .{ "exp", .keyword_exp },
        .{ "getenv", .keyword_getenv },
        .{ "hopen", .keyword_hopen },
        .{ "if", .keyword_if },
        .{ "in", .keyword_in },
        .{ "insert", .keyword_insert },
        .{ "last", .keyword_last },
        .{ "like", .keyword_like },
        .{ "log", .keyword_log },
        .{ "max", .keyword_max },
        .{ "min", .keyword_min },
        .{ "prd", .keyword_prd },
        .{ "select", .keyword_select },
        .{ "setenv", .keyword_setenv },
        .{ "sin", .keyword_sin },
        .{ "sqrt", .keyword_sqrt },
        .{ "ss", .keyword_ss },
        .{ "sum", .keyword_sum },
        .{ "tan", .keyword_tan },
        .{ "update", .keyword_update },
        .{ "var", .keyword_var },
        .{ "wavg", .keyword_wavg },
        .{ "while", .keyword_while },
        .{ "within", .keyword_within },
        .{ "wsum", .keyword_wsum },
        .{ "xexp", .keyword_xexp },
    });

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
        angle_bracket_left_colon,
        angle_bracket_left_equal,
        angle_bracket_left_right,
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
                .string_literal,
                .symbol_literal,
                .symbol_list_literal,
                .identifier,
                .comment,
                .system,
                .invalid,
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
                .angle_bracket_left_colon => "<:",
                .angle_bracket_left_equal => "<=",
                .angle_bracket_left_right => "<>",
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
                .string_literal => "a string literal",
                .symbol_literal => "a symbol literal",
                .symbol_list_literal => " a symbol list literal",
                .identifier => "an identifier",
                .comment => "a comment",
                .system => "a system command",
                .invalid => "invalid bytes",
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

    mode: Mode,

    /// For debugging purposes
    pub fn dump(self: *Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8, mode: Mode) Tokenizer {
        // Skip the UTF-8 BOM if present
        const src_start: usize = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0;
        return Tokenizer{
            .buffer = buffer,
            .index = src_start,
            .mode = mode,
        };
    }

    const State = enum {
        start,
        dot,
        operator,
        angle_bracket_left,
        angle_bracket_right,
        zero,
        one,
        two,
        number,
        symbol_start,
        symbol,
        symbol_handle,
        identifier,
    };

    pub fn next(self: *Tokenizer) Token {
        var state: State = .start;
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

                    ':' => {
                        result.tag = .colon;
                        state = .operator;
                    },
                    '+' => {
                        result.tag = .plus;
                        state = .operator;
                    },
                    '-' => {
                        result.tag = .minus;
                        state = .operator;
                    },
                    '*' => {
                        result.tag = .asterisk;
                        state = .operator;
                    },
                    '%' => {
                        result.tag = .percent;
                        state = .operator;
                    },
                    '!' => {
                        result.tag = .bang;
                        state = .operator;
                    },
                    '&' => {
                        result.tag = .ampersand;
                        state = .operator;
                    },
                    '|' => {
                        result.tag = .pipe;
                        state = .operator;
                    },
                    '<' => {
                        result.tag = .angle_bracket_left;
                        state = .angle_bracket_left;
                    },
                    '>' => {
                        result.tag = .angle_bracket_right;
                        state = .angle_bracket_right;
                    },
                    '=' => {
                        result.tag = .equal;
                        state = .operator;
                    },
                    '~' => {
                        result.tag = .tilde;
                        state = .operator;
                    },
                    ',' => {
                        result.tag = .comma;
                        state = .operator;
                    },
                    '^' => {
                        result.tag = .caret;
                        state = .operator;
                    },
                    '#' => {
                        result.tag = .hash;
                        state = .operator;
                    },
                    '_' => {
                        result.tag = .underscore;
                        state = .operator;
                    },
                    '$' => {
                        result.tag = .dollar;
                        state = .operator;
                    },
                    '?' => {
                        result.tag = .question_mark;
                        state = .operator;
                    },
                    '@' => {
                        result.tag = .at;
                        state = .operator;
                    },
                    '.' => {
                        result.tag = .dot;
                        state = .dot;
                    },
                    '\'' => {
                        result.tag = .apostrophe;
                        state = .operator;
                    },
                    '/' => {
                        result.tag = .slash;
                        state = .operator;
                    },
                    '\\' => {
                        result.tag = .backslash;
                        state = .operator;
                    },
                    '0' => {
                        result.tag = .number_literal;
                        state = .zero;
                    },
                    '1' => {
                        result.tag = .number_literal;
                        state = .one;
                    },
                    '2' => {
                        result.tag = .number_literal;
                        state = .two;
                    },

                    '3'...'9' => {
                        result.tag = .number_literal;
                        state = .number;
                    },

                    '`' => {
                        result.tag = .symbol_literal;
                        state = .symbol_start;
                    },

                    'a'...'z', 'A'...'Z' => {
                        result.tag = .identifier;
                        state = .identifier;
                    },

                    else => {
                        result.tag = .invalid;
                        self.index += 1;
                        break;
                    },
                },

                .dot => switch (c) {
                    ':' => {
                        result.tag = .dot_colon;
                        self.index += 1;
                        break;
                    },
                    '0'...'9' => {
                        result.tag = .number_literal;
                        state = .number;
                    },
                    'a'...'z', 'A'...'Z' => {
                        result.tag = .identifier;
                        state = .identifier;
                    },
                    else => break,
                },

                .operator => switch (c) {
                    ':' => {
                        result.tag = @enumFromInt(@intFromEnum(result.tag) + 1);
                        self.index += 1;
                        break;
                    },
                    else => break,
                },
                .angle_bracket_left => switch (c) {
                    ':' => {
                        result.tag = .angle_bracket_left_colon;
                        self.index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .angle_bracket_left_equal;
                        self.index += 1;
                        break;
                    },
                    '>' => {
                        result.tag = .angle_bracket_left_right;
                        self.index += 1;
                        break;
                    },
                    else => break,
                },
                .angle_bracket_right => switch (c) {
                    ':' => {
                        result.tag = .angle_bracket_right_colon;
                        self.index += 1;
                        break;
                    },
                    '=' => {
                        result.tag = .angle_bracket_right_equal;
                        self.index += 1;
                        break;
                    },
                    else => break,
                },

                .zero => switch (c) {
                    ':' => {
                        result.tag = .zero_colon;
                        state = .operator;
                    },
                    '0'...'9', 'a'...'z', 'A'...'Z', '.' => {
                        state = .number;
                    },
                    else => break,
                },
                .one => switch (c) {
                    ':' => {
                        result.tag = .one_colon;
                        state = .operator;
                    },
                    '0'...'9', 'a'...'z', 'A'...'Z', '.' => {
                        state = .number;
                    },
                    else => break,
                },
                .two => switch (c) {
                    ':' => {
                        result.tag = .two_colon;
                        self.index += 1;
                        break;
                    },
                    '0'...'9', 'a'...'z', 'A'...'Z', '.' => {
                        state = .number;
                    },
                    else => break,
                },

                .number => switch (c) {
                    '0'...'9', 'a'...'z', 'A'...'Z', '.' => {},
                    else => break,
                },

                .symbol_start => switch (c) {
                    '`' => {
                        result.tag = .symbol_list_literal;
                    },
                    'a'...'z', 'A'...'Z', '0'...'9', '.' => {
                        state = .symbol;
                    },
                    ':' => {
                        state = .symbol_handle;
                    },
                    else => break,
                },
                .symbol => switch (c) {
                    '`' => {
                        result.tag = .symbol_list_literal;
                        state = .symbol_start;
                    },
                    'a'...'z', 'A'...'Z', '0'...'9', '.' => {},
                    ':' => {
                        state = .symbol_handle;
                    },
                    '_' => {
                        if (self.mode == .k) break;
                    },
                    else => break,
                },
                .symbol_handle => switch (c) {
                    '`' => {
                        result.tag = .symbol_list_literal;
                        state = .symbol_start;
                    },
                    'a'...'z', 'A'...'Z', '0'...'9', '.', ':', '/' => {},
                    '_' => {
                        if (self.mode == .k) break;
                    },
                    else => break,
                },

                .identifier => switch (c) {
                    'a'...'z', 'A'...'Z', '0'...'9', '.' => {},
                    '_' => {
                        if (self.mode == .k) {
                            if (Token.getKeyword(self.buffer[result.loc.start..self.index])) |tag| {
                                result.tag = tag;
                            }
                            break;
                        }
                    },
                    else => {
                        if (Token.getKeyword(self.buffer[result.loc.start..self.index])) |tag| {
                            result.tag = tag;
                        }
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
    var tokenizer = Tokenizer.init(source, .q);

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
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
    });
    try testTokenize("1\n", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
    });
    try testTokenize("1\n2", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 2, .end = 3 }, .eob = true },
    });
    try testTokenize("1\n 2", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1 \n2", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1 \n 2", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("1 \n2 ", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1 \n 2 ", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });

    try testTokenize("1;", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 1, .end = 2 }, .eob = true },
    });
    try testTokenize("1 ;", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
    });
    try testTokenize("1;2", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 1, .end = 2 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 2, .end = 3 }, .eob = true },
    });
    try testTokenize("1 ;2", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1; 2", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 1, .end = 2 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("1 ; 2", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });

    try testTokenize("(1;2)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("((1;2);3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_paren, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("{1;2}", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("{{1;2};3}", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_brace, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("[1;2]", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("[[1;2];3]", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });

    try testTokenize("(1;\n2;3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 5, .end = 6 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 7, .end = 8 }, .eob = true },
    });
    try testTokenize("(1; \n2;3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("(1;\n 2;3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("(1; \n 2;3)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 8, .end = 9 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 9, .end = 10 }, .eob = true },
    });
}

test "Token tags" {
    inline for (@typeInfo(Token.Tag).Enum.fields) |field| {
        const tag: Token.Tag = @enumFromInt(field.value);
        if (tag.lexeme()) |lexeme| {
            const source = try std.testing.allocator.dupeZ(u8, lexeme);
            defer std.testing.allocator.free(source);
            try testTokenize(source, &.{.{ .tag = tag, .loc = .{ .start = 0, .end = source.len }, .eob = true }});
        }
    }
}

test "tokenize number" {
    try std.testing.expect(false);
}

test "tokenize string" {
    try std.testing.expect(false);
}

test "tokenize symbol" {
    try std.testing.expect(false);
}

test "tokenize symbol list" {
    try std.testing.expect(false);
}

test "tokenize identifier" {
    try std.testing.expect(false);
}

test {
    @import("std").testing.refAllDecls(@This());
}
