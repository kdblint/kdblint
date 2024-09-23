const std = @import("std");

const kdb = @import("root.zig");
const Mode = kdb.Ast.Mode;

/// The root token is assumed to be index 0. Since there can be no
/// references to the root token, this means 0 is available to indicate null.
pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    // pub const Index = enum(u32) { _ };
    pub const Index = u32;

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{});

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    comptime {
        std.debug.assert(@sizeOf(Tag) <= 1);
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
        ampersand,
        ampersand_colon,
        pipe,
        pipe_colon,
        caret,
        caret_colon,
        equal,
        equal_colon,
        angle_bracket_left,
        angle_bracket_left_colon,
        angle_bracket_left_equal,
        angle_bracket_left_right,
        angle_bracket_right,
        angle_bracket_right_colon,
        angle_bracket_right_equal,
        dollar,
        dollar_colon,
        comma,
        comma_colon,
        hash,
        hash_colon,
        underscore,
        underscore_colon,
        tilde,
        tilde_colon,
        bang,
        bang_colon,
        question_mark,
        question_mark_colon,
        at,
        at_colon,
        period,
        period_colon,
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
        identifier,

        // Miscellaneous
        invalid,
        eof,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                // Punctuation
                .l_paren => "(",
                .r_paren => ")",
                .l_brace => "{",
                .r_brace => "}",
                .l_bracket => "[",
                .r_bracket => "]",
                .semicolon => ";",

                // Verbs
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
                .ampersand => "&",
                .ampersand_colon => "&:",
                .pipe => "|",
                .pipe_colon => "|:",
                .caret => "^",
                .caret_colon => "^:",
                .equal => "=",
                .equal_colon => "=:",
                .angle_bracket_left => "<",
                .angle_bracket_left_colon => "<:",
                .angle_bracket_left_equal => "<=",
                .angle_bracket_left_right => "<>",
                .angle_bracket_right => ">",
                .angle_bracket_right_colon => ">:",
                .angle_bracket_right_equal => ">=",
                .dollar => "$",
                .dollar_colon => "$:",
                .comma => ",",
                .comma_colon => ",:",
                .hash => "#",
                .hash_colon => "#:",
                .underscore => "_",
                .underscore_colon => "_:",
                .tilde => "~",
                .tilde_colon => "~:",
                .bang => "!",
                .bang_colon => "!:",
                .question_mark => "?",
                .question_mark_colon => "?:",
                .at => "@",
                .at_colon => "@:",
                .period => ".",
                .period_colon => ".:",
                .zero_colon => "0:",
                .zero_colon_colon => "0::",
                .one_colon => "1:",
                .one_colon_colon => "1::",
                .two_colon => "2:",

                // Adverb
                .apostrophe => "'",
                .apostrophe_colon => "':",
                .slash => "/",
                .slash_colon => "/:",
                .backslash => "\\",
                .backslash_colon => "\\:",

                // Literals
                .number_literal,
                .string_literal,
                .symbol_literal,
                .identifier,
                => null,

                // Miscellaneous
                .invalid,
                .eof,
                => null,
            };
        }
    };
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    mode: Mode,
    index: usize,
    parens_count: u32 = 0,
    braces_count: u32 = 0,
    brackets_count: u32 = 0,

    /// For debugging purposes.
    pub fn dump(self: *Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    // TODO: Check for null byte.
    pub fn init(buffer: [:0]const u8, mode: Mode) Tokenizer {
        // Skip the UTF-8 BOM if present.
        const start_index: usize = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0;
        var index = start_index;
        var checkpoint = start_index;
        state: switch (StartState.start) {
            .start => switch (buffer[index]) {
                0 => if (index != buffer.len) {
                    index += 1;
                    continue :state .skip_line;
                },
                ' ', '\t', '\r' => {
                    index += 1;
                    continue :state .skip_line;
                },
                '\n' => {
                    index += 1;
                    continue :state .block;
                },
                '/' => {
                    index += 1;
                    continue :state .comment;
                },
                '\\' => {
                    index += 1;
                    continue :state .maybe_trailing_comment;
                },
                else => {},
            },

            .skip_line => switch (buffer[index]) {
                0 => if (index != buffer.len) {
                    index += 1;
                    continue :state .skip_line;
                },
                '\n' => {
                    index += 1;
                    continue :state .block;
                },
                else => {
                    index += 1;
                    continue :state .skip_line;
                },
            },

            .block => switch (buffer[index]) {
                0 => if (index != buffer.len) {
                    index += 1;
                    continue :state .skip_line;
                },
                ' ', '\t', '\r' => {
                    index += 1;
                    continue :state .skip_line;
                },
                '\n' => {
                    index += 1;
                    continue :state .block;
                },
                else => {},
            },

            .comment => switch (buffer[index]) {
                0 => if (index != buffer.len) {
                    index += 1;
                    continue :state .skip_line;
                },
                ' ', '\t', '\r' => {
                    index += 1;
                    continue :state .comment;
                },
                '\n' => {
                    index += 1;
                    continue :state .block_comment;
                },
                else => {
                    index += 1;
                    continue :state .skip_line;
                },
            },

            .block_comment => switch (buffer[index]) {
                '\n' => {
                    index += 1;
                    continue :state .end_block_comment;
                },
                else => {
                    index += 1;
                    continue :state .block_comment;
                },
            },

            .end_block_comment => switch (buffer[index]) {
                '\\' => {
                    index += 1;
                    continue :state .skip_line;
                },
                else => {
                    index += 1;
                    continue :state .block_comment;
                },
            },

            .maybe_trailing_comment => switch (buffer[index]) {
                0 => index = buffer.len,
                ' ', '\t', '\r' => {
                    checkpoint = index;
                    index += 1;
                    continue :state .trailing_comment;
                },
                '\n' => index = buffer.len,
                else => {},
            },

            .trailing_comment => switch (buffer[index]) {
                0 => index = buffer.len,
                ' ', '\t', '\r' => {
                    index += 1;
                    continue :state .trailing_comment;
                },
                '\n' => index = buffer.len,
                else => index = checkpoint,
            },
        }
        return .{
            .buffer = buffer,
            .index = index,
            .mode = mode,
        };
    }

    const StartState = enum {
        start,
        skip_line,
        block,
        comment,
        block_comment,
        end_block_comment,
        maybe_trailing_comment,
        trailing_comment,
    };

    const State = enum {
        start,
        colon,
        plus,
        minus,
        asterisk,
        percent,
        ampersand,
        pipe,
        caret,
        equal,
        angle_bracket_left,
        angle_bracket_right,
        dollar,
        comma,
        hash,
        underscore,
        tilde,
        bang,
        question_mark,
        at,
        period,
        zero,
        zero_colon,
        one,
        one_colon,
        two,
        apostrophe,
        slash,
        line_comment,
        block_comment_start,
        block_comment,
        block_comment_end,
        backslash,
        trailing_comment,
        int,
        int_exponent,
        int_period,
        float,
        float_exponent,
        string_literal,
        string_literal_backslash,
        octal_char_start,
        octal_char_end,
        multiline_string_literal,
        symbol_literal_start,
        symbol_literal,
        file_handle,
        identifier,
        expect_newline,
        invalid,
    };

    /// After this returns invalid, it will reset on the next newline, returning tokens starting from there.
    /// An eof token will always be returned at the end.
    pub fn next(self: *Tokenizer) Token {
        var result: Token = .{
            .tag = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };
        state: switch (State.start) {
            .start => switch (self.buffer[self.index]) {
                0 => if (self.index != self.buffer.len) {
                    continue :state .invalid;
                } else return .{
                    .tag = .eof,
                    .loc = .{
                        .start = self.index,
                        .end = self.index,
                    },
                },
                ' ', '\n', '\t', '\r' => {
                    self.index += 1;
                    result.loc.start = self.index;
                    continue :state .start;
                },
                '"' => {
                    result.tag = .string_literal;
                    continue :state .string_literal;
                },
                '`' => {
                    result.tag = .symbol_literal;
                    continue :state .symbol_literal_start;
                },
                'a'...'z', 'A'...'Z' => {
                    result.tag = .identifier;
                    continue :state .identifier;
                },
                '(' => {
                    result.tag = .l_paren;
                    self.index += 1;
                    self.parens_count += 1;
                },
                ')' => {
                    result.tag = .r_paren;
                    self.index += 1;
                    self.parens_count -= 1;
                },
                '{' => {
                    result.tag = .l_brace;
                    self.index += 1;
                    self.braces_count += 1;
                },
                '}' => {
                    result.tag = .r_brace;
                    self.index += 1;
                    self.braces_count -= 1;
                },
                '[' => {
                    result.tag = .l_bracket;
                    self.index += 1;
                    self.brackets_count += 1;
                },
                ']' => {
                    result.tag = .r_bracket;
                    self.index += 1;
                    self.brackets_count -= 1;
                },
                ';' => {
                    result.tag = .semicolon;
                    self.index += 1;
                },
                ':' => continue :state .colon,
                '+' => continue :state .plus,
                '-' => continue :state .minus,
                '*' => continue :state .asterisk,
                '%' => continue :state .percent,
                '&' => continue :state .ampersand,
                '|' => continue :state .pipe,
                '^' => continue :state .caret,
                '=' => continue :state .equal,
                '<' => continue :state .angle_bracket_left,
                '>' => continue :state .angle_bracket_right,
                '$' => continue :state .dollar,
                ',' => continue :state .comma,
                '#' => continue :state .hash,
                '_' => continue :state .underscore,
                '~' => continue :state .tilde,
                '!' => continue :state .bang,
                '?' => continue :state .question_mark,
                '@' => continue :state .at,
                '.' => continue :state .period,
                '0' => {
                    result.tag = .number_literal;
                    continue :state .zero;
                },
                '1' => {
                    result.tag = .number_literal;
                    continue :state .one;
                },
                '2' => {
                    result.tag = .number_literal;
                    continue :state .two;
                },
                '\'' => continue :state .apostrophe,
                '/' => continue :state .slash,
                '\\' => continue :state .backslash,
                '3'...'9' => {
                    result.tag = .number_literal;
                    self.index += 1;
                    continue :state .int;
                },
                else => continue :state .invalid,
            },

            .colon => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .colon_colon;
                        self.index += 1;
                    },
                    else => result.tag = .colon,
                }
            },

            .plus => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .plus_colon;
                        self.index += 1;
                    },
                    else => result.tag = .plus,
                }
            },

            .minus => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .minus_colon;
                        self.index += 1;
                    },
                    else => result.tag = .minus,
                }
            },

            .asterisk => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .asterisk_colon;
                        self.index += 1;
                    },
                    else => result.tag = .asterisk,
                }
            },

            .percent => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .percent_colon;
                        self.index += 1;
                    },
                    else => result.tag = .percent,
                }
            },

            .ampersand => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .ampersand_colon;
                        self.index += 1;
                    },
                    else => result.tag = .ampersand,
                }
            },

            .pipe => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .pipe_colon;
                        self.index += 1;
                    },
                    else => result.tag = .pipe,
                }
            },

            .caret => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .caret_colon;
                        self.index += 1;
                    },
                    else => result.tag = .caret,
                }
            },

            .equal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .equal_colon;
                        self.index += 1;
                    },
                    else => result.tag = .equal,
                }
            },

            .angle_bracket_left => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .angle_bracket_left_colon;
                        self.index += 1;
                    },
                    '=' => {
                        result.tag = .angle_bracket_left_equal;
                        self.index += 1;
                    },
                    '>' => {
                        result.tag = .angle_bracket_left_right;
                        self.index += 1;
                    },
                    else => result.tag = .angle_bracket_left,
                }
            },

            .angle_bracket_right => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .angle_bracket_right_colon;
                        self.index += 1;
                    },
                    '=' => {
                        result.tag = .angle_bracket_right_equal;
                        self.index += 1;
                    },
                    else => result.tag = .angle_bracket_right,
                }
            },

            .dollar => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .dollar_colon;
                        self.index += 1;
                    },
                    else => result.tag = .dollar,
                }
            },

            .comma => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .comma_colon;
                        self.index += 1;
                    },
                    else => result.tag = .comma,
                }
            },

            .hash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .hash_colon;
                        self.index += 1;
                    },
                    else => result.tag = .hash,
                }
            },

            .underscore => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .underscore_colon;
                        self.index += 1;
                    },
                    else => result.tag = .underscore,
                }
            },

            .tilde => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .tilde_colon;
                        self.index += 1;
                    },
                    else => result.tag = .tilde,
                }
            },

            .bang => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .bang_colon;
                        self.index += 1;
                    },
                    else => result.tag = .bang,
                }
            },

            .question_mark => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .question_mark_colon;
                        self.index += 1;
                    },
                    else => result.tag = .question_mark,
                }
            },

            .at => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .at_colon;
                        self.index += 1;
                    },
                    else => result.tag = .at,
                }
            },

            .period => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .period_colon;
                        self.index += 1;
                    },
                    'a'...'z', 'A'...'Z' => {
                        result.tag = .identifier;
                        continue :state .identifier;
                    },
                    else => result.tag = .period,
                }
            },

            .zero => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => continue :state .zero_colon,
                    else => continue :state .int,
                }
            },

            .zero_colon => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .zero_colon_colon;
                        self.index += 1;
                    },
                    else => result.tag = .zero_colon,
                }
            },

            .one => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => continue :state .one_colon,
                    else => continue :state .int,
                }
            },

            .one_colon => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .one_colon_colon;
                        self.index += 1;
                    },
                    else => result.tag = .one_colon,
                }
            },

            .two => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .two_colon;
                        self.index += 1;
                    },
                    else => continue :state .int,
                }
            },

            .apostrophe => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .apostrophe_colon;
                        self.index += 1;
                    },
                    else => result.tag = .apostrophe,
                }
            },

            .slash => {
                switch (self.buffer[self.index - 1]) {
                    ' ', '\t' => continue :state .line_comment,
                    '\r' => continue :state .invalid,
                    '\n' => continue :state .block_comment_start,
                    else => {},
                }

                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .slash_colon;
                        self.index += 1;
                    },
                    else => result.tag = .slash,
                }
            },

            .line_comment => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    } else return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.index,
                            .end = self.index,
                        },
                    },
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    '\r' => continue :state .expect_newline,
                    0x01...0x09, 0x0b...0x0c, 0x0e...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .line_comment,
                }
            },

            .block_comment_start => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    } else return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.index,
                            .end = self.index,
                        },
                    },
                    ' ', '\t' => continue :state .block_comment_start,
                    '\r' => if (self.buffer[self.index + 1] == '\n') {
                        self.index += 1;
                        continue :state .block_comment;
                    } else continue :state .invalid,
                    '\n' => continue :state .block_comment,
                    else => continue :state .line_comment,
                }
            },

            .block_comment => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    } else return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.index,
                            .end = self.index,
                        },
                    },
                    '\\' => if (self.buffer[self.index - 1] == '\n') {
                        continue :state .block_comment_end;
                    } else continue :state .block_comment,
                    else => continue :state .block_comment,
                }
            },

            .block_comment_end => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    } else return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.index,
                            .end = self.index,
                        },
                    },
                    ' ', '\t' => continue :state .block_comment_end,
                    '\r' => continue :state .expect_newline,
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    else => continue :state .block_comment,
                }
            },

            .backslash => {
                switch (self.buffer[self.index - 1]) {
                    '\r' => continue :state .invalid,
                    '\n' => continue :state .trailing_comment,
                    else => {},
                }

                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .backslash_colon;
                        self.index += 1;
                    },
                    else => result.tag = .backslash,
                }
            },

            .trailing_comment => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    } else return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.index,
                            .end = self.index,
                        },
                    },
                    ' ', '\t' => continue :state .trailing_comment,
                    '\r' => if (self.buffer[self.index + 1] != '\n') {
                        continue :state .invalid;
                    } else return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.buffer.len,
                            .end = self.buffer.len,
                        },
                    },
                    '\n' => return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.buffer.len,
                            .end = self.buffer.len,
                        },
                    },
                    else => continue :state .backslash, // TODO: Backtrack?
                }
            },

            // TODO: kdb number types
            .int => switch (self.buffer[self.index]) {
                '.' => continue :state .int_period,
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                    self.index += 1;
                    continue :state .int;
                },
                'e', 'E', 'p', 'P' => {
                    continue :state .int_exponent;
                },
                else => {},
            },
            .int_exponent => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '-', '+' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    else => continue :state .int,
                }
            },
            .int_period => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    'e', 'E', 'p', 'P' => {
                        continue :state .float_exponent;
                    },
                    else => self.index -= 1,
                }
            },
            .float => switch (self.buffer[self.index]) {
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                    self.index += 1;
                    continue :state .float;
                },
                'e', 'E', 'p', 'P' => {
                    continue :state .float_exponent;
                },
                else => {},
            },
            .float_exponent => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '-', '+' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    else => continue :state .float,
                }
            },

            .string_literal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else continue :state .invalid,
                    '\n' => continue :state .multiline_string_literal,
                    '\\' => continue :state .string_literal_backslash,
                    '"' => self.index += 1,
                    0x01...0x09, 0x0b...0x1f, 0x7f => continue :state .invalid,
                    else => continue :state .string_literal,
                }
            },

            .string_literal_backslash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else continue :state .invalid,
                    '"', '\\', 'n', 'r', 't' => continue :state .string_literal,
                    '0'...'9' => continue :state .octal_char_start,
                    else => continue :state .invalid,
                }
            },

            .octal_char_start => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else continue :state .invalid,
                    '0'...'9' => continue :state .octal_char_end,
                    else => continue :state .invalid,
                }
            },

            .octal_char_end => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else continue :state .invalid,
                    '0'...'9' => continue :state .string_literal,
                    else => continue :state .invalid,
                }
            },

            .multiline_string_literal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else continue :state .invalid,
                    ' ', '\t' => continue :state .string_literal,
                    '\r' => if (self.buffer[self.index + 1] == '\n') {
                        self.index += 1;
                        continue :state .multiline_string_literal;
                    } else continue :state .invalid,
                    '\n' => continue :state .multiline_string_literal,
                    else => continue :state .invalid,
                }
            },

            .symbol_literal_start => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        result.tag = .invalid;
                    },
                    ':' => continue :state .file_handle,
                    'a'...'z', 'A'...'Z', '0'...'9', '.' => continue :state .symbol_literal,
                    else => {},
                }
            },

            .symbol_literal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        result.tag = .invalid;
                    },
                    ':' => continue :state .file_handle,
                    '_' => if (self.mode == .q) {
                        continue :state .symbol_literal;
                    },
                    'a'...'z', 'A'...'Z', '0'...'9', '.' => continue :state .symbol_literal,
                    else => {},
                }
            },

            .file_handle => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        result.tag = .invalid;
                    },
                    'a'...'z', 'A'...'Z', '0'...'9', '.', '/' => continue :state .file_handle,
                    else => {},
                }
            },

            .identifier => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '_' => if (self.mode == .q) {
                        continue :state .identifier;
                    },
                    'a'...'z', 'A'...'Z', '0'...'9', '.' => continue :state .identifier,
                    else => {
                        const ident = self.buffer[result.loc.start..self.index];
                        if (Token.getKeyword(ident)) |tag| {
                            result.tag = tag;
                        }
                    },
                }
            },

            .expect_newline => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else continue :state .invalid,
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    else => continue :state .invalid,
                }
            },

            .invalid => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    },
                    '\n' => result.tag = .invalid,
                    else => continue :state .invalid,
                }
            },
        }

        result.loc.end = self.index;

        return result;
    }
};

test "unknown length pointer and then c pointer" {
    try testTokenize(
        \\[*]u8
        \\[*c]u8
    , &.{
        .l_bracket,
        .asterisk,
        .r_bracket,
        .identifier,
        .l_bracket,
        .asterisk,
        .identifier,
        .r_bracket,
        .identifier,
    });
}

test "float literal e exponent" {
    try testTokenize("a = 4.94065645841246544177e-324;\n", &.{
        .identifier,
        .equal,
        .number_literal,
        .semicolon,
    });
}

test "float literal p exponent" {
    try testTokenize("a = 0x1.a827999fcef32p+1022;\n", &.{
        .identifier,
        .equal,
        .number_literal,
        .semicolon,
    });
}

test "invalid token characters" {
    try testTokenize("#", &.{.hash});
    try testTokenize("`", &.{.symbol_literal});
    try testTokenize("'c", &.{ .apostrophe, .identifier });
    try testTokenize("'", &.{.apostrophe});
    try testTokenize("'\n'", &.{ .apostrophe, .apostrophe });
}

test "invalid literal/comment characters" {
    try testTokenize("\"\x00\"", &.{.invalid});
}

test "utf8" {
    try testTokenize("//\xc2\x80", &.{});
    try testTokenize("//\xf4\x8f\xbf\xbf", &.{});
}

test "invalid utf8" {
    try testTokenize("//\x80", &.{});
    try testTokenize("//\xbf", &.{});
    try testTokenize("//\xf8", &.{});
    try testTokenize("//\xff", &.{});
    try testTokenize("//\xc2\xc0", &.{});
    try testTokenize("//\xe0", &.{});
    try testTokenize("//\xf0", &.{});
    try testTokenize("//\xf0\x90\x80\xc0", &.{});
}

test "illegal unicode codepoints" {
    // unicode newline characters.U+0085, U+2028, U+2029
    try testTokenize("//\xc2\x84", &.{});
    try testTokenize("//\xc2\x85", &.{});
    try testTokenize("//\xc2\x86", &.{});
    try testTokenize("//\xe2\x80\xa7", &.{});
    try testTokenize("//\xe2\x80\xa8", &.{});
    try testTokenize("//\xe2\x80\xa9", &.{});
    try testTokenize("//\xe2\x80\xaa", &.{});
}

test "pipe and then invalid" {
    try testTokenize("||=", &.{
        .pipe,
        .pipe,
        .equal,
    });
}

test "line comment and doc comment" {
    try testTokenize("//", &.{});
    try testTokenize("// a / b", &.{});
    try testTokenize("// /", &.{});
    try testTokenize("/// a", &.{});
    try testTokenize("///", &.{});
    try testTokenize("////", &.{});
    try testTokenize("//!", &.{});
    try testTokenize("//!!", &.{});
}

test "line comment followed by identifier" {
    try testTokenize(
        \\    Unexpected,
        \\    // another
        \\    Another,
    , &.{});
}

test "UTF-8 BOM is recognized and skipped" {
    try testTokenize("\xEF\xBB\xBFa;\n", &.{
        .identifier,
        .semicolon,
    });
}

test "correctly parse pointer dereference followed by asterisk" {
    try testTokenize("\"b\".* ** 10", &.{
        .string_literal,
        .period,
        .asterisk,
        .asterisk,
        .asterisk,
        .number_literal,
    });

    try testTokenize("(\"b\".*)** 10", &.{
        .l_paren,
        .string_literal,
        .period,
        .asterisk,
        .r_paren,
        .asterisk,
        .asterisk,
        .number_literal,
    });

    try testTokenize("\"b\".*** 10", &.{
        .string_literal,
        .period,
        .asterisk,
        .asterisk,
        .asterisk,
        .number_literal,
    });
}

test "number literals decimal" {
    try testTokenize("0", &.{.number_literal});
    try testTokenize("1", &.{.number_literal});
    try testTokenize("2", &.{.number_literal});
    try testTokenize("3", &.{.number_literal});
    try testTokenize("4", &.{.number_literal});
    try testTokenize("5", &.{.number_literal});
    try testTokenize("6", &.{.number_literal});
    try testTokenize("7", &.{.number_literal});
    try testTokenize("8", &.{.number_literal});
    try testTokenize("9", &.{.number_literal});
    try testTokenize("0a", &.{.number_literal});
    try testTokenize("9b", &.{.number_literal});
    try testTokenize("1z", &.{.number_literal});
    try testTokenize("1z_1", &.{.number_literal});
    try testTokenize("9z3", &.{.number_literal});

    try testTokenize("0_0", &.{.number_literal});
    try testTokenize("0001", &.{.number_literal});
    try testTokenize("01234567890", &.{.number_literal});
    try testTokenize("012_345_6789_0", &.{.number_literal});
    try testTokenize("0_1_2_3_4_5_6_7_8_9_0", &.{.number_literal});

    try testTokenize("00_", &.{.number_literal});
    try testTokenize("0_0_", &.{.number_literal});
    try testTokenize("0__0", &.{.number_literal});
    try testTokenize("0_0f", &.{.number_literal});
    try testTokenize("0_0_f", &.{.number_literal});
    try testTokenize("0_0_f_00", &.{.number_literal});
    try testTokenize("1_,", &.{ .number_literal, .comma });

    try testTokenize("0.0", &.{.number_literal});
    try testTokenize("1.0", &.{.number_literal});
    try testTokenize("10.0", &.{.number_literal});
    try testTokenize("0e0", &.{.number_literal});
    try testTokenize("1e0", &.{.number_literal});
    try testTokenize("1e100", &.{.number_literal});
    try testTokenize("1.0e100", &.{.number_literal});
    try testTokenize("1.0e+100", &.{.number_literal});
    try testTokenize("1.0e-100", &.{.number_literal});
    try testTokenize("1_0_0_0.0_0_0_0_0_1e1_0_0_0", &.{.number_literal});

    try testTokenize("1.", &.{ .number_literal, .period });
    try testTokenize("1e", &.{.number_literal});
    try testTokenize("1.e100", &.{.number_literal});
    try testTokenize("1.0e1f0", &.{.number_literal});
    try testTokenize("1.0p100", &.{.number_literal});
    try testTokenize("1.0p-100", &.{.number_literal});
    try testTokenize("1.0p1f0", &.{.number_literal});
    try testTokenize("1.0_,", &.{ .number_literal, .comma });
    try testTokenize("1_.0", &.{.number_literal});
    try testTokenize("1._", &.{.number_literal});
    try testTokenize("1.a", &.{.number_literal});
    try testTokenize("1.z", &.{.number_literal});
    try testTokenize("1._0", &.{.number_literal});
    try testTokenize("1.+", &.{ .number_literal, .period, .plus });
    try testTokenize("1._+", &.{ .number_literal, .plus });
    try testTokenize("1._e", &.{.number_literal});
    try testTokenize("1.0e", &.{.number_literal});
    try testTokenize("1.0e,", &.{ .number_literal, .comma });
    try testTokenize("1.0e_", &.{.number_literal});
    try testTokenize("1.0e+_", &.{.number_literal});
    try testTokenize("1.0e-_", &.{.number_literal});
    try testTokenize("1.0e0_+", &.{ .number_literal, .plus });
}

test "number literals binary" {
    try testTokenize("0b0", &.{.number_literal});
    try testTokenize("0b1", &.{.number_literal});
    try testTokenize("0b2", &.{.number_literal});
    try testTokenize("0b3", &.{.number_literal});
    try testTokenize("0b4", &.{.number_literal});
    try testTokenize("0b5", &.{.number_literal});
    try testTokenize("0b6", &.{.number_literal});
    try testTokenize("0b7", &.{.number_literal});
    try testTokenize("0b8", &.{.number_literal});
    try testTokenize("0b9", &.{.number_literal});
    try testTokenize("0ba", &.{.number_literal});
    try testTokenize("0bb", &.{.number_literal});
    try testTokenize("0bc", &.{.number_literal});
    try testTokenize("0bd", &.{.number_literal});
    try testTokenize("0be", &.{.number_literal});
    try testTokenize("0bf", &.{.number_literal});
    try testTokenize("0bz", &.{.number_literal});

    try testTokenize("0b0000_0000", &.{.number_literal});
    try testTokenize("0b1111_1111", &.{.number_literal});
    try testTokenize("0b10_10_10_10", &.{.number_literal});
    try testTokenize("0b0_1_0_1_0_1_0_1", &.{.number_literal});
    try testTokenize("0b1.", &.{ .number_literal, .period });
    try testTokenize("0b1.0", &.{.number_literal});

    try testTokenize("0B0", &.{.number_literal});
    try testTokenize("0b_", &.{.number_literal});
    try testTokenize("0b_0", &.{.number_literal});
    try testTokenize("0b1_", &.{.number_literal});
    try testTokenize("0b0__1", &.{.number_literal});
    try testTokenize("0b0_1_", &.{.number_literal});
    try testTokenize("0b1e", &.{.number_literal});
    try testTokenize("0b1p", &.{.number_literal});
    try testTokenize("0b1e0", &.{.number_literal});
    try testTokenize("0b1p0", &.{.number_literal});
    try testTokenize("0b1_,", &.{ .number_literal, .comma });
}

test "number literals octal" {
    try testTokenize("0o0", &.{.number_literal});
    try testTokenize("0o1", &.{.number_literal});
    try testTokenize("0o2", &.{.number_literal});
    try testTokenize("0o3", &.{.number_literal});
    try testTokenize("0o4", &.{.number_literal});
    try testTokenize("0o5", &.{.number_literal});
    try testTokenize("0o6", &.{.number_literal});
    try testTokenize("0o7", &.{.number_literal});
    try testTokenize("0o8", &.{.number_literal});
    try testTokenize("0o9", &.{.number_literal});
    try testTokenize("0oa", &.{.number_literal});
    try testTokenize("0ob", &.{.number_literal});
    try testTokenize("0oc", &.{.number_literal});
    try testTokenize("0od", &.{.number_literal});
    try testTokenize("0oe", &.{.number_literal});
    try testTokenize("0of", &.{.number_literal});
    try testTokenize("0oz", &.{.number_literal});

    try testTokenize("0o01234567", &.{.number_literal});
    try testTokenize("0o0123_4567", &.{.number_literal});
    try testTokenize("0o01_23_45_67", &.{.number_literal});
    try testTokenize("0o0_1_2_3_4_5_6_7", &.{.number_literal});
    try testTokenize("0o7.", &.{ .number_literal, .period });
    try testTokenize("0o7.0", &.{.number_literal});

    try testTokenize("0O0", &.{.number_literal});
    try testTokenize("0o_", &.{.number_literal});
    try testTokenize("0o_0", &.{.number_literal});
    try testTokenize("0o1_", &.{.number_literal});
    try testTokenize("0o0__1", &.{.number_literal});
    try testTokenize("0o0_1_", &.{.number_literal});
    try testTokenize("0o1e", &.{.number_literal});
    try testTokenize("0o1p", &.{.number_literal});
    try testTokenize("0o1e0", &.{.number_literal});
    try testTokenize("0o1p0", &.{.number_literal});
    try testTokenize("0o_,", &.{ .number_literal, .comma });
}

test "number literals hexadecimal" {
    try testTokenize("0x0", &.{.number_literal});
    try testTokenize("0x1", &.{.number_literal});
    try testTokenize("0x2", &.{.number_literal});
    try testTokenize("0x3", &.{.number_literal});
    try testTokenize("0x4", &.{.number_literal});
    try testTokenize("0x5", &.{.number_literal});
    try testTokenize("0x6", &.{.number_literal});
    try testTokenize("0x7", &.{.number_literal});
    try testTokenize("0x8", &.{.number_literal});
    try testTokenize("0x9", &.{.number_literal});
    try testTokenize("0xa", &.{.number_literal});
    try testTokenize("0xb", &.{.number_literal});
    try testTokenize("0xc", &.{.number_literal});
    try testTokenize("0xd", &.{.number_literal});
    try testTokenize("0xe", &.{.number_literal});
    try testTokenize("0xf", &.{.number_literal});
    try testTokenize("0xA", &.{.number_literal});
    try testTokenize("0xB", &.{.number_literal});
    try testTokenize("0xC", &.{.number_literal});
    try testTokenize("0xD", &.{.number_literal});
    try testTokenize("0xE", &.{.number_literal});
    try testTokenize("0xF", &.{.number_literal});
    try testTokenize("0x0z", &.{.number_literal});
    try testTokenize("0xz", &.{.number_literal});

    try testTokenize("0x0123456789ABCDEF", &.{.number_literal});
    try testTokenize("0x0123_4567_89AB_CDEF", &.{.number_literal});
    try testTokenize("0x01_23_45_67_89AB_CDE_F", &.{.number_literal});
    try testTokenize("0x0_1_2_3_4_5_6_7_8_9_A_B_C_D_E_F", &.{.number_literal});

    try testTokenize("0X0", &.{.number_literal});
    try testTokenize("0x_", &.{.number_literal});
    try testTokenize("0x_1", &.{.number_literal});
    try testTokenize("0x1_", &.{.number_literal});
    try testTokenize("0x0__1", &.{.number_literal});
    try testTokenize("0x0_1_", &.{.number_literal});
    try testTokenize("0x_,", &.{ .number_literal, .comma });

    try testTokenize("0x1.0", &.{.number_literal});
    try testTokenize("0xF.0", &.{.number_literal});
    try testTokenize("0xF.F", &.{.number_literal});
    try testTokenize("0xF.Fp0", &.{.number_literal});
    try testTokenize("0xF.FP0", &.{.number_literal});
    try testTokenize("0x1p0", &.{.number_literal});
    try testTokenize("0xfp0", &.{.number_literal});
    try testTokenize("0x1.0+0xF.0", &.{ .number_literal, .plus, .number_literal });

    try testTokenize("0x1.", &.{ .number_literal, .period });
    try testTokenize("0xF.", &.{ .number_literal, .period });
    try testTokenize("0x1.+0xF.", &.{ .number_literal, .period, .plus, .number_literal, .period });
    try testTokenize("0xff.p10", &.{.number_literal});

    try testTokenize("0x0123456.789ABCDEF", &.{.number_literal});
    try testTokenize("0x0_123_456.789_ABC_DEF", &.{.number_literal});
    try testTokenize("0x0_1_2_3_4_5_6.7_8_9_A_B_C_D_E_F", &.{.number_literal});
    try testTokenize("0x0p0", &.{.number_literal});
    try testTokenize("0x0.0p0", &.{.number_literal});
    try testTokenize("0xff.ffp10", &.{.number_literal});
    try testTokenize("0xff.ffP10", &.{.number_literal});
    try testTokenize("0xffp10", &.{.number_literal});
    try testTokenize("0xff_ff.ff_ffp1_0_0_0", &.{.number_literal});
    try testTokenize("0xf_f_f_f.f_f_f_fp+1_000", &.{.number_literal});
    try testTokenize("0xf_f_f_f.f_f_f_fp-1_00_0", &.{.number_literal});

    try testTokenize("0x1e", &.{.number_literal});
    try testTokenize("0x1e0", &.{.number_literal});
    try testTokenize("0x1p", &.{.number_literal});
    try testTokenize("0xfp0z1", &.{.number_literal});
    try testTokenize("0xff.ffpff", &.{.number_literal});
    try testTokenize("0x0.p", &.{.number_literal});
    try testTokenize("0x0.z", &.{.number_literal});
    try testTokenize("0x0._", &.{.number_literal});
    try testTokenize("0x0_.0", &.{.number_literal});
    try testTokenize("0x0_.0.0", &.{ .number_literal, .period, .number_literal });
    try testTokenize("0x0._0", &.{.number_literal});
    try testTokenize("0x0.0_", &.{.number_literal});
    try testTokenize("0x0_p0", &.{.number_literal});
    try testTokenize("0x0_.p0", &.{.number_literal});
    try testTokenize("0x0._p0", &.{.number_literal});
    try testTokenize("0x0.0_p0", &.{.number_literal});
    try testTokenize("0x0._0p0", &.{.number_literal});
    try testTokenize("0x0.0p_0", &.{.number_literal});
    try testTokenize("0x0.0p+_0", &.{.number_literal});
    try testTokenize("0x0.0p-_0", &.{.number_literal});
    try testTokenize("0x0.0p0_", &.{.number_literal});
}

test "null byte before eof" {
    try testTokenize("123 \x00 456", &.{ .number_literal, .invalid });
    try testTokenize("//\x00", &.{});
    // try testTokenize("\\\\\x00", &.{ .backslash, .backslash, .invalid });
    try testTokenize("\x00", &.{});
    try testTokenize("// NUL\x00\n", &.{});
    try testTokenize("///\x00\n", &.{});
    try testTokenize("/// NUL\x00\n", &.{});
}

test "fuzzable properties upheld" {
    return std.testing.fuzz(testPropertiesUpheld, .{});
}

test "trailing whitespace" {
    try testTokenize("1;\n2", &.{ .number_literal, .semicolon, .number_literal });
    try testTokenize("1;\n2\n", &.{ .number_literal, .semicolon, .number_literal });
    try testTokenize("1;\n2\n\n", &.{ .number_literal, .semicolon, .number_literal });
}

test "symbols" {
    try testTokenize("`", &.{.symbol_literal});
    try testTokenize("`a", &.{.symbol_literal});
    try testTokenize("`symbol", &.{.symbol_literal});
    try testTokenize("`1", &.{.symbol_literal});
    try testTokenize("`1test", &.{.symbol_literal});
    try testTokenize("`UPPERCASE", &.{.symbol_literal});
    try testTokenize("`symbol.with.dot", &.{.symbol_literal});
    try testTokenize("`.symbol.with.leading.dot", &.{.symbol_literal});
    try testTokenize("`symbol/with/slash", &.{ .symbol_literal, .slash, .identifier, .slash, .identifier });
    try testTokenize("`:handle/with/slash", &.{.symbol_literal});
    try testTokenize("`symbol:with/slash/after/colon", &.{.symbol_literal});
    try testTokenize(
        "`symbol/with/slash:before:colon",
        &.{ .symbol_literal, .slash, .identifier, .slash, .identifier, .colon, .identifier, .colon, .identifier },
    );

    try testTokenizeMode(.k, "`symbol_with_underscore", &.{ .symbol_literal, .underscore, .identifier, .underscore, .identifier });
    try testTokenizeMode(.q, "`symbol_with_underscore", &.{.symbol_literal});
    try testTokenizeMode(
        .k,
        "`_symbol_with_leading_underscore",
        &.{ .symbol_literal, .underscore, .identifier, .underscore, .identifier, .underscore, .identifier, .underscore, .identifier },
    );
    try testTokenizeMode(.q, "`_symbol_with_leading_underscore", &.{ .symbol_literal, .underscore, .identifier });

    try testTokenize("``", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`a`a", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`symbol`symbol", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`1`1", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`1test`1test", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`UPPERCASE`UPPERCASE", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`symbol.with.dot`symbol.with.dot", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`.symbol.with.leading.dot`.symbol.with.leading.dot", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`:handle/with/slash`:handle/with/slash", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`symbol:with/slash/after/colon`symbol:with/slash/after/colon", &.{ .symbol_literal, .symbol_literal });
}

test "identifiers" {
    try testTokenize("a", &.{.identifier});
    try testTokenize("identifier", &.{.identifier});
    try testTokenize("test1", &.{.identifier});
    try testTokenize("UPPERCASE", &.{.identifier});
    try testTokenize("identifier.with.dot", &.{.identifier});
    try testTokenize(".identifier.with.leading.dot", &.{.identifier});

    try testTokenizeMode(.k, "identifier_with_underscore", &.{ .identifier, .underscore, .identifier, .underscore, .identifier });
    try testTokenizeMode(.q, "identifier_with_underscore", &.{.identifier});
    try testTokenizeMode(
        .k,
        "_identifier_with_leading_underscore",
        &.{ .underscore, .identifier, .underscore, .identifier, .underscore, .identifier, .underscore, .identifier },
    );
    try testTokenizeMode(.q, "_identifier_with_leading_underscore", &.{ .underscore, .identifier });
}

test "strings" {
    try testTokenize(
        \\"this is a string"
    , &.{.string_literal});
    try testTokenize(
        \\"this is a string\"with\\embedded\nescape\rchars\t"
    , &.{.string_literal});
    try testTokenize(
        \\"Zürich"
    , &.{.string_literal});
    try testTokenize(
        \\"this is \a string with bad ch\ars"
    , &.{.invalid});
    try testTokenize(
        \\"\012"
    , &.{.string_literal});

    try testTokenize(
        \\"
    , &.{.invalid});
    try testTokenize(
        \\"\
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
        \\"this is a valid
        \\ multiline string"
    , &.{.string_literal});
    try testTokenize(
        \\"this is an invalid
        \\multiline string"
    , &.{.invalid});
}

fn testTokenize(
    source: [:0]const u8,
    expected_token_tags: []const Token.Tag,
) !void {
    inline for (@typeInfo(Mode).@"enum".fields) |field| {
        try testTokenizeMode(
            @enumFromInt(field.value),
            source,
            expected_token_tags,
        );
    }
}

fn testTokenizeMode(
    mode: Mode,
    source: [:0]const u8,
    expected_token_tags: []const Token.Tag,
) !void {
    var tokenizer = Tokenizer.init(source, mode);
    for (expected_token_tags) |expected_token_tag| {
        const token = tokenizer.next();
        try std.testing.expectEqual(expected_token_tag, token.tag);
    }
    // Last token should always be eof, even when the last token was invalid,
    // in which case the tokenizer is in an invalid state, which can only be
    // recovered by opinionated means outside the scope of this implementation.
    const last_token = tokenizer.next();
    try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
    try std.testing.expectEqual(source.len, last_token.loc.start);
    try std.testing.expectEqual(source.len, last_token.loc.end);
}

fn testPropertiesUpheld(source: []const u8) anyerror!void {
    const source0 = try std.testing.allocator.dupeZ(u8, source);
    defer std.testing.allocator.free(source0);
    var tokenizer = Tokenizer.init(source0, .q);
    var tokenization_failed = false;
    while (true) {
        const token = tokenizer.next();

        // Property: token end location after start location (or equal)
        try std.testing.expect(token.loc.end >= token.loc.start);

        switch (token.tag) {
            .invalid => {
                tokenization_failed = true;

                // Property: invalid token always ends at newline or eof
                try std.testing.expect(source0[token.loc.end] == '\n' or source0[token.loc.end] == 0);
            },
            .eof => {
                // Property: EOF token is always 0-length at end of source.
                try std.testing.expectEqual(source0.len, token.loc.start);
                try std.testing.expectEqual(source0.len, token.loc.end);
                break;
            },
            else => continue,
        }
    }

    if (source0.len > 0) for (source0, source0[1..][0..source0.len]) |cur, next| {
        // Property: No null byte allowed except at end.
        if (cur == 0) {
            try std.testing.expect(tokenization_failed);
        }
        // Property: No ASCII control characters other than \n and \t are allowed.
        if (std.ascii.isControl(cur) and cur != '\n' and cur != '\t') {
            try std.testing.expect(tokenization_failed);
        }
        // Property: All '\r' must be followed by '\n'.
        if (cur == '\r' and next != '\n') {
            try std.testing.expect(tokenization_failed);
        }
    };
}
