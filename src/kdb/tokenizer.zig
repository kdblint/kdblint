const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

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
        eob,
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
    prev: ?u8,

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
            .prev = null,
        };
    }

    const State = enum {
        start,
        string_literal,
        string_literal_backslash,
        invalid_string_literal,
        invalid_string_literal_backslash,
        number_literal,
    };

    pub fn next(self: *Tokenizer) Token {
        if (self.prev) |prev| {
            if (prev == '\n' and !std.ascii.isWhitespace(self.buffer[self.index + 1])) {
                self.prev = null;
                return Token{
                    .tag = .eob,
                    .loc = .{
                        .start = self.index - 1,
                        .end = self.index,
                    },
                };
            }
        }

        var state: State = .start;
        var result = Token{
            .tag = .eof,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };
        const seen_escape_digits: usize = undefined;
        _ = seen_escape_digits;
        const remaining_code_units: usize = undefined;
        _ = remaining_code_units;
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
                    '"' => {
                        state = .string_literal;
                        result.tag = .string_literal;
                    },
                    '0'...'9' => {
                        state = .number_literal;
                        result.tag = .number_literal;
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

                .string_literal => switch (c) {
                    0 => {
                        if (self.index == self.buffer.len) {
                            result.tag = .invalid;
                            break;
                        }
                    },
                    '\\' => {
                        state = .string_literal_backslash;
                    },
                    '"' => {
                        self.index += 1;
                        break;
                    },
                    else => self.checkLiteralCharacter(),
                },
                .string_literal_backslash => switch (c) {
                    0 => {
                        result.tag = .invalid;
                        if (self.index == self.buffer.len) {
                            break;
                        }
                        state = .invalid_string_literal;
                    },
                    '"', '\\', 'n', 'r', 't' => {
                        state = .string_literal;
                    },
                    '0'...'9' => {
                        if (self.index + 2 < self.buffer.len and
                            std.ascii.isDigit(self.buffer[self.index + 1]) and
                            std.ascii.isDigit(self.buffer[self.index + 2]))
                        {
                            self.index += 2;
                            state = .string_literal;
                        } else {
                            state = .invalid_string_literal;
                            result.tag = .invalid;
                        }
                    },
                    else => {
                        state = .invalid_string_literal;
                        result.tag = .invalid;
                    },
                },
                .invalid_string_literal => switch (c) {
                    0 => {
                        if (self.index == self.buffer.len) break;
                    },
                    '\\' => {
                        state = .invalid_string_literal_backslash;
                    },
                    '"' => {
                        self.index += 1;
                        break;
                    },
                    else => self.checkLiteralCharacter(),
                },
                .invalid_string_literal_backslash => {
                    state = .invalid_string_literal;
                },

                .number_literal => switch (c) {
                    '0'...'9' => {},
                    else => break,
                },
            }
        }

        if (result.tag == .eof) {
            result.loc.start = self.index;
        } else {
            self.prev = self.buffer[self.index];
        }

        result.loc.end = self.index;
        return result;
    }

    fn checkLiteralCharacter(self: *Tokenizer) void {
        const c0 = self.buffer[self.index];
        if (!std.ascii.isASCII(c0)) {
            const length = std.unicode.utf8ByteSequenceLength(c0) catch return;
            if (self.index + length > self.buffer.len) {
                self.index = self.buffer.len - 1;
            } else {
                self.index += length - 1;
            }
        }
    }
};

fn testTokenize(source: [:0]const u8, expected: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(source);

    const actual = try std.testing.allocator.alloc(Token.Tag, expected.len);
    defer std.testing.allocator.free(actual);
    for (actual) |*tag| {
        tag.* = tokenizer.next().tag;
    }

    try std.testing.expectEqualSlices(Token.Tag, expected, actual);
    const last_token = tokenizer.next();
    try std.testing.expectEqual(.eof, last_token.tag);
    try std.testing.expectEqual(source.len, last_token.loc.start);
    try std.testing.expectEqual(source.len, last_token.loc.end);
}

test "tokenize blocks" {
    try testTokenize(
        \\123
    , &.{.number_literal});
    // try testTokenize(
    //     \\123
    //     \\ 456
    // , &.{.number_list_literal});
    try testTokenize(
        \\123
        \\456
    , &.{ .number_literal, .eob, .number_literal });
    try testTokenize(
        \\123;
    , &.{ .number_literal, .semicolon });
    // try testTokenize(
    //     \\123
    //     \\ 456;
    // , &.{ .number_list_literal, .semicolon });
    try testTokenize(
        \\123;
        \\456;
    , &.{ .number_literal, .semicolon, .eob, .number_literal, .semicolon });
    return error.SkipZigTest;
}

test "tokenize string literals" {
    try testTokenize(
        \\"a"
    , &.{.string_literal});
    try testTokenize(
        \\"\"test"
    , &.{.string_literal});
    try testTokenize(
        \\"\012"
    , &.{.string_literal});
    try testTokenize(
        \\"\0123"
    , &.{.string_literal});
    try testTokenize(
        \\"\012test"
    , &.{.string_literal});
    try testTokenize(
        \\"\
    , &.{.invalid});
    try testTokenize(
        \\"\"
    , &.{.invalid});
    try testTokenize(
        \\"\0
    , &.{.invalid});
    try testTokenize(
        \\"\0"
    , &.{.invalid});
    try testTokenize(
        \\"\01
    , &.{.invalid});
    try testTokenize(
        \\"\01"
    , &.{.invalid});
    try testTokenize(
        \\"\012
    , &.{.invalid});
    try testTokenize(
        \\"\012
        \\ "
    , &.{.string_literal});
    // try testTokenize(
    //     \\"\012
    //     \\"
    // , &.{ .invalid, .eob, .invalid });
    return error.SkipZigTest;
}

test {
    @import("std").testing.refAllDecls(@This());
}
