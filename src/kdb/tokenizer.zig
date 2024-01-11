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
    pending_token: ?Token = null,

    is_first: bool = true,

    parens_count: u32 = 0,
    braces_count: u32 = 0,
    brackets_count: u32 = 0,

    mode: Mode,

    /// private field
    impl: struct {
        index: usize,
        line: u32,
        character: u32,
    },

    /// For debugging purposes
    pub fn dump(self: *Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8, mode: Mode) Tokenizer {
        // Skip the UTF-8 BOM if present
        const src_start: usize = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0;
        var tokenizer = Tokenizer{
            .buffer = buffer,
            .mode = mode,
            .impl = .{
                .index = src_start,
                .line = 0,
                .character = 0,
            },
        };
        tokenizer.consumeStartingComment();
        return tokenizer;
    }

    const CommentState = enum {
        start,

        start_trailing,
        trailing,

        maybe_block,
        block,
        end_block,

        line,
    };

    const State = enum {
        start,
        minus,
        dot,
        operator,
        angle_bracket_left,
        angle_bracket_right,
        zero,
        one,
        two,
        number,
        string,
        escaped_string,
        symbol_start,
        symbol,
        symbol_handle,
        identifier,

        maybe_trailing_comment,
        start_trailing_comment,
        trailing_comment,

        maybe_block_comment,
        block_comment,
        end_block_comment,

        skip_line,
    };

    fn consumeStartingComment(self: *Tokenizer) void {
        var state: CommentState = .start;
        const start_index = self.impl.index;

        var backtrack: @TypeOf(self.impl) = undefined;
        while (true) : (self.advance()) {
            const c = self.buffer[self.impl.index];
            switch (state) {
                .start => switch (c) {
                    0 => break,
                    ' ', '\t', '\n', '\r' => {},
                    '/' => {
                        if (self.impl.character == 0) {
                            state = .maybe_block;
                        }
                    },
                    '\\' => {
                        if (self.impl.character == 0) {
                            if (!std.ascii.isWhitespace(self.buffer[self.impl.index + 1])) {
                                break;
                            }
                            backtrack = self.impl;
                            state = .start_trailing;
                        }
                    },
                    else => {
                        if (self.impl.character == 0) {
                            break;
                        }
                    },
                },

                .start_trailing => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .trailing;
                    },
                    else => {
                        self.impl = backtrack;
                        break;
                    },
                },
                .trailing => switch (c) {
                    0 => break,
                    else => {},
                },

                .maybe_block => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .block;
                    },
                    else => {
                        state = .line;
                    },
                },
                .block => switch (c) {
                    0 => break,
                    '\\' => {
                        if (self.impl.character == 0) {
                            state = .end_block;
                        }
                    },
                    else => {},
                },
                .end_block => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .start;
                    },
                    else => {
                        state = .block;
                    },
                },

                .line => switch (c) {
                    0 => break,
                    '\n' => {
                        state = .start;
                    },
                    else => {},
                },
            }
        }

        if (self.impl.index > start_index) {
            self.pending_token = Token{
                .tag = .comment,
                .loc = .{
                    .start = start_index,
                    .end = self.impl.index,
                },
                .eob = false,
            };
        }
    }

    fn advance(self: *Tokenizer) void {
        if (self.buffer[self.impl.index] == '\n') {
            self.impl.line += 1;
            self.impl.character = 0;
        } else {
            self.impl.character += 1;
        }
        self.impl.index += 1;
    }

    fn advanceN(self: *Tokenizer, comptime n: comptime_int) void {
        inline for (0..n) |_| {
            self.advance();
        }
    }

    pub fn next(self: *Tokenizer) Token {
        if (self.pending_token) |token| {
            self.pending_token = null;
            return token;
        }

        var state: State = .start;
        var result = Token{
            .tag = .eof,
            .loc = .{
                .start = self.impl.index,
                .end = undefined,
            },
            .eob = false,
        };

        while (true) : (self.advance()) {
            const c = self.buffer[self.impl.index];
            switch (state) {
                .start => switch (c) {
                    0 => {
                        if (self.impl.index == self.buffer.len) {
                            break;
                        }
                        result.tag = .invalid;
                        self.advance();
                        break;
                    },
                    ' ', '\t', '\n', '\r' => {
                        result.loc.start = self.impl.index + 1;
                    },

                    '(' => {
                        result.tag = .l_paren;
                        self.advance();
                        self.parens_count += 1;
                        break;
                    },
                    ')' => {
                        result.tag = .r_paren;
                        self.advance();
                        self.parens_count = @max(0, @as(i32, @intCast(self.parens_count)) - 1);
                        break;
                    },
                    '{' => {
                        result.tag = .l_brace;
                        self.advance();
                        self.braces_count += 1;
                        break;
                    },
                    '}' => {
                        result.tag = .r_brace;
                        self.advance();
                        self.braces_count = @max(0, @as(i32, @intCast(self.braces_count)) - 1);
                        break;
                    },
                    '[' => {
                        result.tag = .l_bracket;
                        self.advance();
                        self.brackets_count += 1;
                        break;
                    },
                    ']' => {
                        result.tag = .r_bracket;
                        self.advance();
                        self.brackets_count = @max(0, @as(i32, @intCast(self.brackets_count)) - 1);
                        break;
                    },
                    ';' => {
                        result.tag = .semicolon;
                        self.advance();
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
                        state = .minus;
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
                        if (self.impl.character == 0) {
                            result.tag = .comment;
                            state = .maybe_block_comment;
                        } else if (std.ascii.isWhitespace(self.buffer[self.impl.index - 1])) {
                            result.tag = .comment;
                            state = .skip_line;
                        } else {
                            result.tag = .slash;
                            state = .operator;
                        }
                    },
                    '\\' => {
                        if (self.impl.character == 0) {
                            result.tag = .comment;
                            state = .maybe_trailing_comment;
                        } else {
                            result.tag = .backslash;
                            state = .operator;
                        }
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

                    '"' => {
                        result.tag = .string_literal;
                        state = .string;
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
                        self.advance();
                        break;
                    },
                },

                .minus => switch (c) {
                    ':' => {
                        result.tag = .minus_colon;
                        self.advance();
                        break;
                    },
                    '.' => {
                        if (!std.ascii.isDigit(self.buffer[self.impl.index + 1])) {
                            break;
                        }

                        if (self.impl.index >= 2) switch (self.buffer[self.impl.index - 2]) {
                            ' ', '\t', '\n', '\r', '(', '{', '[', ';' => {},
                            else => break,
                        };

                        result.tag = .number_literal;
                        state = .number;
                        self.advance();
                    },
                    '0'...'9' => {
                        if (self.impl.index >= 2) switch (self.buffer[self.impl.index - 2]) {
                            ' ', '\t', '\n', '\r', '(', '{', '[', ';' => {},
                            else => break,
                        };

                        result.tag = .number_literal;
                        state = .number;
                    },
                    else => break,
                },

                .dot => switch (c) {
                    ':' => {
                        result.tag = .dot_colon;
                        self.advance();
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
                        self.advance();
                        break;
                    },
                    else => break,
                },
                .angle_bracket_left => switch (c) {
                    ':' => {
                        result.tag = .angle_bracket_left_colon;
                        self.advance();
                        break;
                    },
                    '=' => {
                        result.tag = .angle_bracket_left_equal;
                        self.advance();
                        break;
                    },
                    '>' => {
                        result.tag = .angle_bracket_left_right;
                        self.advance();
                        break;
                    },
                    else => break,
                },
                .angle_bracket_right => switch (c) {
                    ':' => {
                        result.tag = .angle_bracket_right_colon;
                        self.advance();
                        break;
                    },
                    '=' => {
                        result.tag = .angle_bracket_right_equal;
                        self.advance();
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
                        self.advance();
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

                .string => switch (c) {
                    0 => {
                        result.tag = .invalid;
                        break;
                    },
                    '"' => {
                        self.advance();
                        break;
                    },
                    '\\' => {
                        state = .escaped_string;
                    },
                    '\n' => {
                        if (!std.ascii.isWhitespace(self.buffer[self.impl.index + 1])) {
                            result.tag = .invalid;
                            break;
                        }
                    },
                    else => {},
                },
                .escaped_string => switch (c) {
                    0 => {
                        result.tag = .invalid;
                        break;
                    },
                    '"', '\\', 'n', 'r', 't' => {
                        state = .string;
                    },
                    '0'...'9' => {
                        state = .string;
                        if (std.ascii.isDigit(self.buffer[self.impl.index + 1]) and std.ascii.isDigit(self.buffer[self.impl.index + 2])) {
                            self.advanceN(2);
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    else => {
                        result.tag = .invalid;
                        state = .string;
                    },
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
                            if (Token.getKeyword(self.buffer[result.loc.start..self.impl.index])) |tag| {
                                result.tag = tag;
                            }
                            break;
                        }
                    },
                    else => {
                        if (Token.getKeyword(self.buffer[result.loc.start..self.impl.index])) |tag| {
                            result.tag = tag;
                        }
                        break;
                    },
                },

                .maybe_trailing_comment => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {
                        state = .start_trailing_comment;
                    },
                    '\n' => {
                        state = .trailing_comment;
                    },
                    else => {
                        // TODO: Check if system should skip block rather than line
                        result.tag = .system;
                        state = .skip_line;
                    },
                },
                .start_trailing_comment => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .trailing_comment;
                    },
                    else => {
                        result.tag = .invalid;
                        state = .skip_line;
                    },
                },
                .trailing_comment => switch (c) {
                    0 => break,
                    else => {},
                },

                .maybe_block_comment => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .block_comment;
                    },
                    else => {
                        state = .skip_line;
                    },
                },
                .block_comment => switch (c) {
                    0 => break,
                    '\\' => {
                        if (self.impl.character == 0) {
                            state = .end_block_comment;
                        }
                    },
                    else => {},
                },
                .end_block_comment => switch (c) {
                    0, '\n' => break,
                    ' ', '\t', '\r' => {},
                    else => {
                        state = .block_comment;
                    },
                },

                .skip_line => switch (c) {
                    0, '\n' => break,
                    else => {},
                },
            }
        }

        result.loc.end = self.impl.index;

        if (result.tag == .semicolon and self.parens_count == 0 and self.braces_count == 0 and self.brackets_count == 0) {
            result.eob = true;
        } else if (result.tag != .comment) {
            self.nextComment();

            while (true) : (self.advance()) {
                const c = self.buffer[self.impl.index];
                switch (c) {
                    0 => {
                        result.eob = true;
                        break;
                    },
                    ' ', '\t', '\n', '\r' => {},
                    else => {
                        if (self.impl.character == 0) {
                            result.eob = true;
                            self.parens_count = 0;
                            self.braces_count = 0;
                            self.brackets_count = 0;
                        }
                        break;
                    },
                }
            }
        }

        return result;
    }

    fn nextComment(self: *Tokenizer) void {
        var state: CommentState = .start;
        var found_comment = false;
        var result = Token{
            .tag = .comment,
            .loc = .{
                .start = self.impl.index,
                .end = undefined,
            },
            .eob = false,
        };

        while (true) : (self.advance()) {
            const c = self.buffer[self.impl.index];
            switch (state) {
                .start => switch (c) {
                    ' ', '\t', '\n', '\r' => {
                        result.loc.start = self.impl.index + 1;
                    },
                    '/' => {
                        if (self.impl.character == 0) {
                            state = .maybe_block;
                        } else if (std.ascii.isWhitespace(self.buffer[self.impl.index - 1])) {
                            found_comment = true;
                            state = .line;
                        } else {
                            break;
                        }
                    },
                    else => break,
                },

                .start_trailing => switch (c) {
                    else => break,
                },
                .trailing => switch (c) {
                    else => break,
                },

                .maybe_block => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        found_comment = true;
                        state = .block;
                    },
                    else => {
                        found_comment = true;
                        state = .line;
                    },
                },
                .block => switch (c) {
                    0 => break,
                    '\\' => {
                        if (self.impl.character == 0) {
                            state = .end_block;
                        }
                    },
                    else => {},
                },
                .end_block => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {},
                    '\n' => break,
                    else => {
                        state = .block;
                    },
                },

                .line => switch (c) {
                    0, '\n' => break,
                    else => {},
                },
            }
        }

        if (found_comment) {
            result.loc.end = self.impl.index;
            self.pending_token = result;
        }
    }
};

fn testTokenize(source: [:0]const u8, expected: []const Token) !void {
    try testTokenizeMode(.k, source, expected);
    try testTokenizeMode(.q, source, expected);
}

fn testTokenizeMode(mode: Mode, source: [:0]const u8, expected: []const Token) !void {
    var tokenizer = Tokenizer.init(source, mode);

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
    try testTokenize("1", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true }});
    try testTokenize("1\n", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true }});
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
            const source = try std.testing.allocator.allocSentinel(u8, lexeme.len + 2, 0);
            defer std.testing.allocator.free(source);
            source[0] = '(';
            @memcpy(source[1 .. lexeme.len + 1], lexeme);
            source[lexeme.len + 1] = ')';
            try testTokenize(source, &.{
                .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
                .{ .tag = tag, .loc = .{ .start = 1, .end = lexeme.len + 1 }, .eob = false },
                .{ .tag = .r_paren, .loc = .{ .start = lexeme.len + 1, .end = lexeme.len + 2 }, .eob = true },
            });
        }
    }
}

test "tokenize number" {
    try testTokenize("0", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true }});
    try testTokenize("1", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true }});
    try testTokenize("-1", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 2 }, .eob = true }});
    try testTokenize("123", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 3 }, .eob = true }});
    try testTokenize("-123", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 4 }, .eob = true }});
    try testTokenize(".1", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 2 }, .eob = true }});
    try testTokenize("-.1", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 3 }, .eob = true }});
    try testTokenize("1.1", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 3 }, .eob = true }});
    try testTokenize("-1.1", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 4 }, .eob = true }});
    try testTokenize("1.", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 2 }, .eob = true }});
    try testTokenize("-1.", &.{.{ .tag = .number_literal, .loc = .{ .start = 0, .end = 3 }, .eob = true }});
    try testTokenize("-.", &.{
        .{ .tag = .minus, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .dot, .loc = .{ .start = 1, .end = 2 }, .eob = true },
    });
}

test "tokenize negative number" {
    try testTokenize("(-1)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 3 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("(-.1)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 4 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("()-1", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("()-.1", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 5 }, .eob = true },
    });
    try testTokenize("() -1", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 5 }, .eob = true },
    });
    try testTokenize("() -.1", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 6 }, .eob = true },
    });
    try testTokenize("{-1}", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 3 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("{-.1}", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 4 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("{}-1", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("{}-.1", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 5 }, .eob = true },
    });
    try testTokenize("{} -1", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 5 }, .eob = true },
    });
    try testTokenize("{} -.1", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 6 }, .eob = true },
    });
    try testTokenize("[-1]", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 3 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("[-.1]", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 4 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize("[]-1", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 4 }, .eob = true },
    });
    try testTokenize("[]-.1", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 5 }, .eob = true },
    });
    try testTokenize("[] -1", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 5 }, .eob = true },
    });
    try testTokenize("[] -.1", &.{
        .{ .tag = .l_bracket, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 6 }, .eob = true },
    });
    try testTokenize(";-1", &.{
        .{ .tag = .semicolon, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 3 }, .eob = true },
    });
    try testTokenize(";-.1", &.{
        .{ .tag = .semicolon, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 4 }, .eob = true },
    });
    try testTokenize("1-1", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 2, .end = 3 }, .eob = true },
    });
    try testTokenize("1-.1", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 2, .end = 4 }, .eob = true },
    });
    try testTokenize("1 -1", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 2, .end = 4 }, .eob = true },
    });
    try testTokenize("1 -.1", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 2, .end = 5 }, .eob = true },
    });
    try testTokenize("\"string\"-1", &.{
        .{ .tag = .string_literal, .loc = .{ .start = 0, .end = 8 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 8, .end = 9 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 9, .end = 10 }, .eob = true },
    });
    try testTokenize("\"string\"-.1", &.{
        .{ .tag = .string_literal, .loc = .{ .start = 0, .end = 8 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 8, .end = 9 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 9, .end = 11 }, .eob = true },
    });
    try testTokenize("`symbol-1", &.{
        .{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 7 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("`symbol-.1", &.{
        .{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 7 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 8, .end = 10 }, .eob = true },
    });
    try testTokenize("`symbol`list-1", &.{
        .{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 12 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 12, .end = 13 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 13, .end = 14 }, .eob = true },
    });
    try testTokenize("`symbol`list-.1", &.{
        .{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 12 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 12, .end = 13 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 13, .end = 15 }, .eob = true },
    });
    try testTokenize("identifier-1", &.{
        .{ .tag = .identifier, .loc = .{ .start = 0, .end = 10 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 10, .end = 11 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 11, .end = 12 }, .eob = true },
    });
    try testTokenize("identifier-.1", &.{
        .{ .tag = .identifier, .loc = .{ .start = 0, .end = 10 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 10, .end = 11 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 11, .end = 13 }, .eob = true },
    });
}

test "tokenize string" {
    try testTokenize(
        \\"this is a string"
    , &.{.{ .tag = .string_literal, .loc = .{ .start = 0, .end = 18 }, .eob = true }});
    try testTokenize(
        \\"this is a string\"with\\embedded\nescape\rchars\t"
    , &.{.{ .tag = .string_literal, .loc = .{ .start = 0, .end = 51 }, .eob = true }});
    try testTokenize(
        \\"this is \a string with bad ch\ars"
    , &.{.{ .tag = .invalid, .loc = .{ .start = 0, .end = 35 }, .eob = true }});
    try testTokenize(
        \\"\012"
    , &.{.{ .tag = .string_literal, .loc = .{ .start = 0, .end = 6 }, .eob = true }});

    try testTokenize(
        \\"\
    , &.{.{ .tag = .invalid, .loc = .{ .start = 0, .end = 2 }, .eob = true }});
    try testTokenize(
        \\"\0
    , &.{.{ .tag = .invalid, .loc = .{ .start = 0, .end = 3 }, .eob = true }});
    try testTokenize(
        \\"\0"
    , &.{.{ .tag = .invalid, .loc = .{ .start = 0, .end = 4 }, .eob = true }});
    try testTokenize(
        \\"\01
    , &.{.{ .tag = .invalid, .loc = .{ .start = 0, .end = 4 }, .eob = true }});
    try testTokenize(
        \\"\01"
    , &.{.{ .tag = .invalid, .loc = .{ .start = 0, .end = 5 }, .eob = true }});

    try testTokenize(
        \\"this is a valid
        \\ multiline string"
    , &.{.{ .tag = .string_literal, .loc = .{ .start = 0, .end = 35 }, .eob = true }});
    try testTokenize(
        \\"this is an invalid
        \\multiline string"
    , &.{
        .{ .tag = .invalid, .loc = .{ .start = 0, .end = 19 }, .eob = true },
        .{ .tag = .identifier, .loc = .{ .start = 20, .end = 29 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 30, .end = 36 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 36, .end = 37 }, .eob = true },
    });
}

test "tokenize symbol" {
    try testTokenize("`", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true }});
    try testTokenize("`a", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 2 }, .eob = true }});
    try testTokenize("`symbol", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 7 }, .eob = true }});
    try testTokenize("`1", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 2 }, .eob = true }});
    try testTokenize("`1test", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 6 }, .eob = true }});
    try testTokenize("`UPPERCASE", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 10 }, .eob = true }});
    try testTokenize("`symbol.with.dot", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 16 }, .eob = true }});
    try testTokenize("`.symbol.with.leading.dot", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 25 }, .eob = true }});
    try testTokenize("`symbol/with/slash", &.{
        .{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 7 }, .eob = false },
        .{ .tag = .slash, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 8, .end = 12 }, .eob = false },
        .{ .tag = .slash, .loc = .{ .start = 12, .end = 13 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 13, .end = 18 }, .eob = true },
    });
    try testTokenize("`:handle/with/slash", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 19 }, .eob = true }});
    try testTokenize("`symbol:with/slash/after/colon", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 30 }, .eob = true }});
    try testTokenize("`symbol/with/slash:before:colon", &.{
        .{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 7 }, .eob = false },
        .{ .tag = .slash, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 8, .end = 12 }, .eob = false },
        .{ .tag = .slash, .loc = .{ .start = 12, .end = 13 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 13, .end = 18 }, .eob = false },
        .{ .tag = .colon, .loc = .{ .start = 18, .end = 19 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 19, .end = 25 }, .eob = false },
        .{ .tag = .colon, .loc = .{ .start = 25, .end = 26 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 26, .end = 31 }, .eob = true },
    });

    try testTokenizeMode(.k, "`symbol_with_underscore", &.{
        .{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 7 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 8, .end = 12 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 12, .end = 13 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 13, .end = 23 }, .eob = true },
    });
    try testTokenizeMode(.q, "`symbol_with_underscore", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 23 }, .eob = true }});
    try testTokenizeMode(.k, "`_symbol_with_leading_underscore", &.{
        .{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 2, .end = 8 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 8, .end = 9 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 9, .end = 13 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 13, .end = 14 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 14, .end = 21 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 21, .end = 22 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 22, .end = 32 }, .eob = true },
    });
    try testTokenizeMode(.q, "`_symbol_with_leading_underscore", &.{
        .{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 2, .end = 32 }, .eob = true },
    });
}

test "tokenize symbol list" {
    try testTokenize("``", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 2 }, .eob = true }});
    try testTokenize("`a`a", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 4 }, .eob = true }});
    try testTokenize("`symbol`symbol", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 14 }, .eob = true }});
    try testTokenize("`1`1", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 4 }, .eob = true }});
    try testTokenize("`1test`1test", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 12 }, .eob = true }});
    try testTokenize("`UPPERCASE`UPPERCASE", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 20 }, .eob = true }});
    try testTokenize("`symbol.with.dot`symbol.with.dot", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 32 }, .eob = true }});
    try testTokenize("`.symbol.with.leading.dot`.symbol.with.leading.dot", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 50 }, .eob = true }});
    try testTokenize("`:handle/with/slash`:handle/with/slash", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 38 }, .eob = true }});
    try testTokenize("`symbol:with/slash/after/colon`symbol:with/slash/after/colon", &.{.{ .tag = .symbol_list_literal, .loc = .{ .start = 0, .end = 60 }, .eob = true }});
}

test "tokenize identifier" {
    try testTokenize("a", &.{.{ .tag = .identifier, .loc = .{ .start = 0, .end = 1 }, .eob = true }});
    try testTokenize("identifier", &.{.{ .tag = .identifier, .loc = .{ .start = 0, .end = 10 }, .eob = true }});
    try testTokenize("test1", &.{.{ .tag = .identifier, .loc = .{ .start = 0, .end = 5 }, .eob = true }});
    try testTokenize("UPPERCASE", &.{.{ .tag = .identifier, .loc = .{ .start = 0, .end = 9 }, .eob = true }});
    try testTokenize("identifier.with.dot", &.{.{ .tag = .identifier, .loc = .{ .start = 0, .end = 19 }, .eob = true }});
    try testTokenize(".identifier.with.leading.dot", &.{.{ .tag = .identifier, .loc = .{ .start = 0, .end = 28 }, .eob = true }});

    try testTokenizeMode(.k, "identifier_with_underscore", &.{
        .{ .tag = .identifier, .loc = .{ .start = 0, .end = 10 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 10, .end = 11 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 11, .end = 15 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 15, .end = 16 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 16, .end = 26 }, .eob = true },
    });
    try testTokenizeMode(.q, "identifier_with_underscore", &.{.{ .tag = .identifier, .loc = .{ .start = 0, .end = 26 }, .eob = true }});
    try testTokenizeMode(.k, "_identifier_with_leading_underscore", &.{
        .{ .tag = .underscore, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 1, .end = 11 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 11, .end = 12 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 12, .end = 16 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 16, .end = 17 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 17, .end = 24 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 24, .end = 25 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 25, .end = 35 }, .eob = true },
    });
    try testTokenizeMode(.q, "_identifier_with_leading_underscore", &.{
        .{ .tag = .underscore, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 1, .end = 35 }, .eob = true },
    });
}

// TODO: Test multiple comments
// TODO: Validate comment tokens with trailing newlines match
test "tokenize starting comment" {
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\/ and has some comments
        \\/
        \\inside.
        \\\
        \\ it also continues after
        \\ some comments and ends
        \\here
    , &.{
        .{ .tag = .comment, .loc = .{ .start = 0, .end = 141 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 141, .end = 145 }, .eob = true },
    });
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\\
        \\and has a trailing
        \\comment inside
    , &.{.{ .tag = .comment, .loc = .{ .start = 0, .end = 91 }, .eob = false }});
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\\d .
        \\identifier
    , &.{
        .{ .tag = .comment, .loc = .{ .start = 0, .end = 56 }, .eob = false },
        .{ .tag = .system, .loc = .{ .start = 56, .end = 60 }, .eob = true },
        .{ .tag = .identifier, .loc = .{ .start = 61, .end = 71 }, .eob = true },
    });
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\\ d .
        \\identifier
    , &.{
        .{ .tag = .comment, .loc = .{ .start = 0, .end = 56 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 56, .end = 61 }, .eob = true },
        .{ .tag = .identifier, .loc = .{ .start = 62, .end = 72 }, .eob = true },
    });
}

test "tokenize block comment" {
    try testTokenize(
        \\;
        \\/
        \\block comment
        \\\
        \\1
    , &.{
        .{ .tag = .semicolon, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 19 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 20, .end = 21 }, .eob = true },
    });
    try testTokenize(
        \\1
        \\/
        \\block comment
        \\\
        \\ 2
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 19 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 21, .end = 22 }, .eob = true },
    });
    try testTokenize(
        \\(1;
        \\/
        \\block comment
        \\\
        \\ 2)
    , &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 21 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 23, .end = 24 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 24, .end = 25 }, .eob = true },
    });
}

test "tokenize line comment" {
    try testTokenize("1 /line comment", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 15 }, .eob = false },
    });
    try testTokenize("1 / line comment", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 16 }, .eob = false },
    });
    try testTokenize("1/not a line comment", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .slash, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 2, .end = 5 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 8, .end = 12 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 13, .end = 20 }, .eob = true },
    });
    try testTokenize("1/ not a line comment", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .slash, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 3, .end = 6 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 9, .end = 13 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 14, .end = 21 }, .eob = true },
    });
    try testTokenize(
        \\1 /line comment
        \\ 2
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 15 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 17, .end = 18 }, .eob = true },
    });
    try testTokenize(
        \\(1; /line comment
        \\ 2)
    , &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 17 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 19, .end = 20 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 20, .end = 21 }, .eob = true },
    });
}

test "tokenize trailing comment" {
    try testTokenize(
        \\;
        \\\
        \\this is a
        \\trailing comment
    , &.{
        .{ .tag = .semicolon, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 30 }, .eob = false },
    });
    try testTokenize(
        \\;
        \\\ this is not a trailing comment
        \\1
    , &.{
        .{ .tag = .semicolon, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 2, .end = 34 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 35, .end = 36 }, .eob = true },
    });
}

test {
    @import("std").testing.refAllDecls(@This());
}
