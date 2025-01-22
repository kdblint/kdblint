const std = @import("std");

const kdb = @import("root.zig");
const Mode = kdb.Ast.Mode;

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    // pub const Index = enum(u32) { _ };
    pub const Index = u32;

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "select", .keyword_select },
        .{ "exec", .keyword_exec },
        .{ "update", .keyword_update },
        .{ "delete", .keyword_delete },
    });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const BuiltinType = enum {
        prefix,
        infix,
    };

    pub const builtins = std.StaticStringMap(BuiltinType).initComptime(.{
        .{ "abs", .prefix },
        .{ "acos", .prefix },
        .{ "aj", .prefix },
        .{ "aj0", .prefix },
        .{ "ajf", .prefix },
        .{ "ajf0", .prefix },
        .{ "all", .prefix },
        .{ "and", .infix },
        .{ "any", .prefix },
        .{ "asc", .prefix },
        .{ "asin", .prefix },
        .{ "asof", .infix },
        .{ "atan", .prefix },
        .{ "attr", .prefix },
        .{ "avg", .prefix },
        .{ "avgs", .prefix },
        .{ "bin", .infix },
        .{ "binr", .infix },
        .{ "ceiling", .prefix },
        .{ "cols", .prefix },
        .{ "cor", .infix },
        .{ "cos", .prefix },
        .{ "count", .prefix },
        .{ "cov", .infix },
        .{ "cross", .infix },
        .{ "csv", .prefix },
        .{ "cut", .infix },
        .{ "deltas", .prefix },
        .{ "desc", .prefix },
        .{ "dev", .prefix },
        .{ "differ", .prefix },
        .{ "distinct", .prefix },
        .{ "div", .infix },
        .{ "dsave", .infix },
        .{ "each", .infix },
        .{ "ej", .prefix },
        .{ "ema", .infix },
        .{ "enlist", .prefix },
        .{ "eval", .prefix },
        .{ "except", .infix },
        .{ "exit", .prefix },
        .{ "exp", .prefix },
        .{ "fby", .infix },
        .{ "fills", .prefix },
        .{ "first", .prefix },
        .{ "fkeys", .prefix },
        .{ "flip", .prefix },
        .{ "floor", .prefix },
        .{ "get", .prefix },
        .{ "getenv", .prefix },
        .{ "group", .prefix },
        .{ "gtime", .prefix },
        .{ "hclose", .prefix },
        .{ "hcount", .prefix },
        .{ "hdel", .prefix },
        .{ "hopen", .prefix },
        .{ "hsym", .prefix },
        .{ "iasc", .prefix },
        .{ "idesc", .prefix },
        .{ "ij", .infix },
        .{ "ijf", .infix },
        .{ "in", .infix },
        .{ "insert", .infix },
        .{ "inter", .infix },
        .{ "inv", .prefix },
        .{ "key", .prefix },
        .{ "keys", .prefix },
        .{ "last", .prefix },
        .{ "like", .infix },
        .{ "lj", .infix },
        .{ "ljf", .infix },
        .{ "load", .prefix },
        .{ "log", .prefix },
        .{ "lower", .prefix },
        .{ "lsq", .infix },
        .{ "ltime", .prefix },
        .{ "ltrim", .prefix },
        .{ "mavg", .infix },
        .{ "max", .prefix },
        .{ "maxs", .prefix },
        .{ "mcount", .infix },
        .{ "md5", .prefix },
        .{ "mdev", .prefix },
        .{ "med", .prefix },
        .{ "meta", .prefix },
        .{ "min", .prefix },
        .{ "mins", .prefix },
        .{ "mmax", .infix },
        .{ "mmin", .prefix },
        .{ "mmu", .infix },
        .{ "mod", .infix },
        .{ "msum", .infix },
        .{ "neg", .prefix },
        .{ "next", .prefix },
        .{ "not", .prefix },
        .{ "null", .prefix },
        .{ "or", .infix },
        .{ "over", .infix },
        .{ "parse", .prefix },
        .{ "peach", .infix },
        .{ "pj", .infix },
        .{ "prd", .prefix },
        .{ "prds", .prefix },
        .{ "prev", .prefix },
        .{ "prior", .infix },
        .{ "rand", .prefix },
        .{ "rank", .prefix },
        .{ "ratios", .prefix },
        .{ "raze", .prefix },
        .{ "read0", .prefix },
        .{ "read1", .prefix },
        .{ "reciprocal", .prefix },
        .{ "reval", .prefix },
        .{ "reverse", .prefix },
        .{ "rload", .prefix },
        .{ "rotate", .infix },
        .{ "rsave", .prefix },
        .{ "rtrim", .prefix },
        .{ "save", .prefix },
        .{ "scan", .infix },
        .{ "scov", .infix },
        .{ "sdev", .prefix },
        .{ "set", .infix },
        .{ "setenv", .infix },
        .{ "show", .prefix },
        .{ "signum", .prefix },
        .{ "sin", .prefix },
        .{ "sqrt", .prefix },
        .{ "ss", .infix },
        .{ "ssr", .prefix },
        .{ "string", .prefix },
        .{ "sublist", .infix },
        .{ "sum", .prefix },
        .{ "sums", .prefix },
        .{ "sv", .infix },
        .{ "svar", .prefix },
        .{ "system", .prefix },
        .{ "tables", .prefix },
        .{ "tan", .prefix },
        .{ "til", .prefix },
        .{ "trim", .prefix },
        .{ "type", .prefix },
        .{ "uj", .infix },
        .{ "ujf", .infix },
        .{ "ungroup", .prefix },
        .{ "union", .infix },
        .{ "upper", .prefix },
        .{ "upsert", .infix },
        .{ "value", .prefix },
        .{ "var", .prefix },
        .{ "view", .prefix },
        .{ "views", .prefix },
        .{ "vs", .infix },
        .{ "wavg", .infix },
        .{ "where", .prefix },
        .{ "within", .infix },
        .{ "wj", .prefix },
        .{ "wj1", .prefix },
        .{ "wsum", .infix },
        .{ "ww", .prefix },
        .{ "xasc", .infix },
        .{ "xbar", .infix },
        .{ "xcol", .infix },
        .{ "xcols", .infix },
        .{ "xdesc", .infix },
        .{ "xexp", .infix },
        .{ "xgroup", .infix },
        .{ "xkey", .infix },
        .{ "xlog", .infix },
        .{ "xprev", .infix },
        .{ "xrank", .infix },
    });

    pub fn getBuiltin(bytes: []const u8) ?BuiltinType {
        return builtins.get(bytes);
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
        prefix_builtin,
        infix_builtin,
        system,

        // Miscellaneous
        invalid,
        eob,
        eof,

        // Keywords
        keyword_select,
        keyword_exec,
        keyword_update,
        keyword_delete,

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
                .prefix_builtin,
                .infix_builtin,
                .system,
                => null,

                // Miscellaneous
                .invalid,
                .eob,
                .eof,
                => null,

                // Keywords
                .keyword_select => "select",
                .keyword_exec => "exec",
                .keyword_update => "update",
                .keyword_delete => "delete",
            };
        }

        test "Token tags which have a known lexeme tokenize as expected" {
            inline for (std.meta.fields(Token.Tag)) |field| {
                const tag: Token.Tag = @enumFromInt(field.value);
                if (comptime tag.lexeme()) |l| {
                    try testTokenize("(" ++ l ++ ")", &.{ .l_paren, tag, .r_paren });
                }
            }
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .number_literal => "a number literal",
                .string_literal => "a string literal",
                .symbol_literal => "a symbol literal",
                .identifier => "an identifier",
                .prefix_builtin, .infix_builtin => "a builtin function",
                .system => "a system command",
                .invalid => "invalid bytes",
                .eob => "EOB",
                .eof => "EOF",
                else => if (@import("builtin").is_test) @panic(@tagName(tag)) else unreachable,
            };
        }

        test "Token tags are not unreachable when calling symbol()" {
            inline for (std.meta.fields(Token.Tag)) |field| {
                const tag: Token.Tag = @enumFromInt(field.value);
                _ = tag.symbol();
            }
        }

        pub fn isKeyword(tag: Tag) bool {
            return switch (tag) {
                inline else => |t| keywords.has(t.symbol()),
            };
        }
    };
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    mode: Mode,
    index: usize,
    prev_tag: Token.Tag = undefined,

    /// For debugging purposes.
    pub fn dump(self: *Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8, mode: Mode) Tokenizer {
        // Skip the UTF-8 BOM if present.
        const slice = buffer[if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0..];
        var index: usize = 0;
        state: switch (StartState.start) {
            .start => switch (slice[index]) {
                0 => {},
                ' ', '\t', '\r' => {
                    index += 1;
                    continue :state .skip_line;
                },
                '\n' => {
                    index += 1;
                    continue :state .start;
                },
                '/' => {
                    index += 1;
                    continue :state .comment;
                },
                else => {},
            },

            .skip_line => switch (slice[index]) {
                0 => {},
                '\n' => {
                    index += 1;
                    continue :state .start;
                },
                else => {
                    index += 1;
                    continue :state .skip_line;
                },
            },

            .comment => switch (slice[index]) {
                0 => {},
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

            .block_comment => switch (slice[index]) {
                0 => {},
                '\n' => {
                    index += 1;
                    continue :state .end_block_comment;
                },
                else => {
                    index += 1;
                    continue :state .block_comment;
                },
            },

            .end_block_comment => switch (slice[index]) {
                0 => {},
                '\\' => {
                    index += 1;
                    continue :state .skip_line;
                },
                else => {
                    index += 1;
                    continue :state .block_comment;
                },
            },
        }
        return .{
            .buffer = slice,
            .index = index,
            .mode = mode,
        };
    }

    const StartState = enum {
        start,
        skip_line,
        comment,
        block_comment,
        end_block_comment,
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
        maybe_system,
        system,
        trailing_comment,
        negative,
        negative_period,
        int,
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
        const token = self.nextImpl();
        self.prev_tag = token.tag;
        return token;
    }

    fn nextImpl(self: *Tokenizer) Token {
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
                },
                ')' => {
                    result.tag = .r_paren;
                    self.index += 1;
                },
                '{' => {
                    result.tag = .l_brace;
                    self.index += 1;
                },
                '}' => {
                    result.tag = .r_brace;
                    self.index += 1;
                },
                '[' => {
                    result.tag = .l_bracket;
                    self.index += 1;
                },
                ']' => {
                    result.tag = .r_bracket;
                    self.index += 1;
                },
                ';' => {
                    result.tag = .semicolon;
                    self.index += 1;
                },
                ':' => continue :state .colon,
                '+' => continue :state .plus,
                '-' => {
                    if (self.index == 0) continue :state .negative;
                    switch (self.buffer[self.index - 1]) {
                        ' ', '\t' => continue :state .negative,
                        else => switch (self.prev_tag) {
                            .l_paren,
                            .l_brace,
                            .l_bracket,
                            .semicolon,

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

                            .apostrophe,
                            .apostrophe_colon,
                            .slash,
                            .slash_colon,
                            .backslash,
                            .backslash_colon,
                            => continue :state .negative,

                            else => continue :state .minus,
                        },
                    }
                },
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
                '\\' => {
                    if (self.index == 0 or self.buffer[self.index - 1] == '\n') continue :state .maybe_system;
                    continue :state .backslash;
                },
                '3'...'9' => {
                    result.tag = .number_literal;
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
                    '0'...'9' => {
                        result.tag = .number_literal;
                        continue :state .int;
                    },
                    else => result.tag = .period,
                }
            },

            .zero => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    },
                    ':' => continue :state .zero_colon,
                    '0'...'9', 'a'...'z', 'A'...'Z', '.' => continue :state .int,
                    else => {},
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
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    },
                    ':' => continue :state .one_colon,
                    '0'...'9', 'a'...'z', 'A'...'Z', '.' => continue :state .int,
                    else => {},
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
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    },
                    ':' => {
                        result.tag = .two_colon;
                        self.index += 1;
                    },
                    '0'...'9', 'a'...'z', 'A'...'Z', '.' => continue :state .int,
                    else => {},
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
                switch (self.buffer[self.index + 1]) {
                    ':' => {
                        result.tag = .slash_colon;
                        self.index += 2;
                    },
                    else => switch (self.buffer[self.index - 1]) {
                        ' ', '\t' => continue :state .line_comment,
                        '\r' => continue :state .invalid,
                        '\n' => continue :state .block_comment_start,
                        else => {
                            result.tag = .slash;
                            self.index += 1;
                        },
                    },
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
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .backslash_colon;
                        self.index += 1;
                    },
                    else => result.tag = .backslash,
                }
            },

            .maybe_system => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    } else return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.buffer.len,
                            .end = self.buffer.len,
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
                    else => {
                        result.tag = .system;
                        continue :state .system;
                    },
                }
            },

            .system => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    },
                    '\n' => {},
                    else => continue :state .system,
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
                    else => continue :state .invalid,
                }
            },

            .int => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index != self.buffer.len) {
                        continue :state .invalid;
                    },
                    'e' => {
                        if (self.index + 1 < self.buffer.len) switch (self.buffer[self.index + 1]) {
                            '+', '-' => switch (self.buffer[self.index + 2]) {
                                '0'...'9' => self.index += 2,
                                else => {},
                            },
                            else => {},
                        };
                        continue :state .int;
                    },
                    '0'...'9', 'a'...'d', 'f'...'z', 'A'...'Z', '.' => continue :state .int,
                    else => {},
                }
            },

            .negative => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '.' => {
                        result.tag = .minus;
                        continue :state .negative_period;
                    },
                    '0'...'9' => {
                        result.tag = .number_literal;
                        continue :state .int;
                    },
                    ':' => {
                        result.tag = .minus_colon;
                        self.index += 1;
                    },
                    else => result.tag = .minus,
                }
            },
            .negative_period => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '0'...'9' => {
                        result.tag = .number_literal;
                        continue :state .int;
                    },
                    else => self.index -= 1,
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
                    '_' => if (self.mode == .q) {
                        continue :state .file_handle;
                    },
                    'a'...'z', 'A'...'Z', '0'...'9', '.', '/', ':' => continue :state .file_handle,
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
                        } else if (Token.getBuiltin(ident)) |builtin_type| {
                            result.tag = switch (builtin_type) {
                                .prefix => .prefix_builtin,
                                .infix => .infix_builtin,
                            };
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

test "invalid literal/comment characters" {
    try testTokenize("\"\x00\"", &.{.invalid});
}

test "line comment followed by identifier" {
    try testTokenize(
        \\    Unexpected,
        \\    // another
        \\    Another,
    , &.{});
}

test "null byte before eof" {
    try testTokenize("123 \x00 456", &.{ .number_literal, .invalid });
    try testTokenize("\x00", &.{.invalid});
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
    try testTokenize(
        "`symbol/with/slash",
        &.{ .symbol_literal, .slash, .identifier, .slash, .identifier },
    );
    try testTokenize("`:handle/with/slash", &.{.symbol_literal});
    try testTokenize("`symbol:with/slash/after/colon", &.{.symbol_literal});
    try testTokenize(
        "`symbol/with/slash:before:colon",
        &.{
            .symbol_literal, .slash, .identifier, .slash, .identifier, .colon, .identifier, .colon, .identifier,
        },
    );

    try testTokenizeMode(
        .k,
        "`symbol_with_underscore",
        &.{ .symbol_literal, .underscore, .identifier, .underscore, .identifier },
    );
    try testTokenizeMode(.q, "`symbol_with_underscore", &.{.symbol_literal});
    try testTokenizeMode(
        .k,
        "`_symbol_with_leading_underscore",
        &.{
            .symbol_literal, .underscore, .identifier, .underscore, .identifier,
            .underscore,     .identifier, .underscore, .identifier,
        },
    );
    try testTokenizeMode(
        .q,
        "`_symbol_with_leading_underscore",
        &.{ .symbol_literal, .underscore, .identifier },
    );

    try testTokenize("``", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`a`a", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`symbol`symbol", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`1`1", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`1test`1test", &.{ .symbol_literal, .symbol_literal });
    try testTokenize("`UPPERCASE`UPPERCASE", &.{ .symbol_literal, .symbol_literal });
    try testTokenize(
        "`symbol.with.dot`symbol.with.dot",
        &.{ .symbol_literal, .symbol_literal },
    );
    try testTokenize(
        "`.symbol.with.leading.dot`.symbol.with.leading.dot",
        &.{ .symbol_literal, .symbol_literal },
    );
    try testTokenize(
        "`:handle/with/slash`:handle/with/slash",
        &.{ .symbol_literal, .symbol_literal },
    );
    try testTokenize(
        "`symbol:with/slash/after/colon`symbol:with/slash/after/colon",
        &.{ .symbol_literal, .symbol_literal },
    );
}

test "identifiers" {
    try testTokenize("a", &.{.identifier});
    try testTokenize("identifier", &.{.identifier});
    try testTokenize("test1", &.{.identifier});
    try testTokenize("UPPERCASE", &.{.identifier});
    try testTokenize("identifier.with.dot", &.{.identifier});
    try testTokenize(".identifier.with.leading.dot", &.{.identifier});

    try testTokenizeMode(
        .k,
        "identifier_with_underscore",
        &.{ .identifier, .underscore, .identifier, .underscore, .identifier },
    );
    try testTokenizeMode(.q, "identifier_with_underscore", &.{.identifier});
    try testTokenizeMode(
        .k,
        "_identifier_with_leading_underscore",
        &.{
            .underscore, .identifier, .underscore, .identifier, .underscore, .identifier, .underscore, .identifier,
        },
    );
    try testTokenizeMode(
        .q,
        "_identifier_with_leading_underscore",
        &.{ .underscore, .identifier },
    );
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

test "negative number literals" {
    try testTokenize("-1", &.{.number_literal});
    try testTokenize("--1", &.{ .minus, .number_literal });
    try testTokenize("---1", &.{ .minus, .minus, .number_literal });

    try testTokenizeMode(.k, "x_-1", &.{ .identifier, .underscore, .number_literal });
    try testTokenizeMode(.q, "x_-1", &.{ .identifier, .minus, .number_literal });

    try testTokenizeMode(.k, "`symbol_-1", &.{
        .symbol_literal, .underscore, .number_literal,
    });
    try testTokenizeMode(.q, "`symbol_-1", &.{
        .symbol_literal, .minus, .number_literal,
    });

    try testTokenize(".-1", &.{ .period, .number_literal });
    try testTokenize("0.-1", &.{ .number_literal, .minus, .number_literal });
}

test "tokenize blocks" {
    try testTokenize("1", &.{.number_literal});
    try testTokenize("1\n", &.{.number_literal});
    try testTokenize("1\n2", &.{ .number_literal, .number_literal });
    try testTokenize("1\n 2", &.{ .number_literal, .number_literal });
    try testTokenize("1 \n2", &.{ .number_literal, .number_literal });
    try testTokenize("1 \n 2", &.{ .number_literal, .number_literal });
    try testTokenize("1 \n2 ", &.{ .number_literal, .number_literal });
    try testTokenize("1 \n 2 ", &.{ .number_literal, .number_literal });
    try testTokenize("1;", &.{ .number_literal, .semicolon });
    try testTokenize("1 ;", &.{ .number_literal, .semicolon });
    try testTokenize("1;2", &.{ .number_literal, .semicolon, .number_literal });
    try testTokenize("1 ;2", &.{ .number_literal, .semicolon, .number_literal });
    try testTokenize("1; 2", &.{ .number_literal, .semicolon, .number_literal });
    try testTokenize("1 ; 2", &.{ .number_literal, .semicolon, .number_literal });
    try testTokenize("(1;2)", &.{
        .l_paren, .number_literal, .semicolon, .number_literal, .r_paren,
    });
    try testTokenize("((1;2);3)", &.{
        .l_paren, .l_paren,   .number_literal, .semicolon, .number_literal,
        .r_paren, .semicolon, .number_literal, .r_paren,
    });
    try testTokenize("{1;2}", &.{
        .l_brace, .number_literal, .semicolon, .number_literal, .r_brace,
    });
    try testTokenize("{{1;2};3}", &.{
        .l_brace, .l_brace,   .number_literal, .semicolon, .number_literal,
        .r_brace, .semicolon, .number_literal, .r_brace,
    });
    try testTokenize("[1;2]", &.{
        .l_bracket, .number_literal, .semicolon, .number_literal, .r_bracket,
    });
    try testTokenize("[[1;2];3]", &.{
        .l_bracket, .l_bracket, .number_literal, .semicolon, .number_literal,
        .r_bracket, .semicolon, .number_literal, .r_bracket,
    });
    try testTokenize("(1;\n2;3)", &.{
        .l_paren, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .r_paren,
    });
    try testTokenize("(1; \n2;3)", &.{
        .l_paren, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .r_paren,
    });
    try testTokenize("(1;\n 2;3)", &.{
        .l_paren, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .r_paren,
    });
    try testTokenize("(1; \n 2;3)", &.{
        .l_paren, .number_literal, .semicolon, .number_literal, .semicolon, .number_literal, .r_paren,
    });
}

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
    , &.{.identifier});
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\\
        \\trailing comment
    , &.{});
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\\d .
        \\identifier
    , &.{ .system, .identifier });
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\\ d .
        \\identifier
    , &.{ .invalid, .identifier });
}

test "tokenize block comment" {
    try testTokenize(
        \\1
        \\/
        \\block comment 1
        \\\
        \\/
        \\block comment 2
        \\\
        \\1
    , &.{ .number_literal, .number_literal });
    try testTokenize(
        \\1
        \\/
        \\block comment 1
        \\\
        \\/
        \\block comment 2
        \\\
        \\ 2
    , &.{ .number_literal, .number_literal });
    try testTokenize(
        \\(1;
        \\/
        \\block comment 1
        \\\
        \\/
        \\block comment 2
        \\\
        \\2)
    , &.{ .l_paren, .number_literal, .semicolon, .number_literal, .r_paren });
    try testTokenize(
        \\(1;
        \\/
        \\block comment 1
        \\\
        \\/
        \\block comment 2
        \\\
        \\ 2)
    , &.{ .l_paren, .number_literal, .semicolon, .number_literal, .r_paren });
}

test "tokenize line comment" {
    try testTokenize("1 /line comment", &.{.number_literal});
    try testTokenize("1 / line comment", &.{.number_literal});
    try testTokenize("1/not a line comment", &.{
        .number_literal, .slash, .prefix_builtin, .identifier, .identifier, .identifier,
    });
    try testTokenize("1/ not a line comment", &.{
        .number_literal, .slash, .prefix_builtin, .identifier, .identifier, .identifier,
    });
    try testTokenize(
        "1 /:not a line comment",
        &.{ .number_literal, .slash_colon, .prefix_builtin, .identifier, .identifier, .identifier },
    );
    try testTokenize(
        \\1 /line comment 1
        \\/line comment 2
        \\2
    , &.{ .number_literal, .number_literal });
    try testTokenize(
        \\1 /line comment 1
        \\ /line comment 2
        \\2
    , &.{ .number_literal, .number_literal });
    try testTokenize(
        \\1 /line comment 1
        \\/line comment 2
        \\ 2
    , &.{ .number_literal, .number_literal });
    try testTokenize(
        \\1 /line comment 1
        \\ /line comment 2
        \\ 2
    , &.{ .number_literal, .number_literal });
    try testTokenize(
        \\(1; /line comment 1
        \\/line comment 2
        \\2)
    , &.{ .l_paren, .number_literal, .semicolon, .number_literal, .r_paren });
    try testTokenize(
        \\(1; /line comment 1
        \\ /line comment 2
        \\2)
    , &.{ .l_paren, .number_literal, .semicolon, .number_literal, .r_paren });
    try testTokenize(
        \\(1; /line comment 1
        \\/line comment 2
        \\ 2)
    , &.{ .l_paren, .number_literal, .semicolon, .number_literal, .r_paren });
    try testTokenize(
        \\(1; /line comment 1
        \\ /line comment 2
        \\ 2)
    , &.{ .l_paren, .number_literal, .semicolon, .number_literal, .r_paren });
}

test "tokenize trailing comment" {
    try testTokenize(
        \\1
        \\\
        \\this is a
        \\trailing comment
    , &.{.number_literal});
    try testTokenize(
        \\1
        \\\ this is not a trailing comment
        \\1
    , &.{ .number_literal, .invalid, .number_literal });
}

test "tokenize system commands" {
    try testTokenize(
        \\\ls
    , &.{.system});
    try testTokenize(
        \\\ls /not a comment
    , &.{.system});
    try testTokenize(
        \\\ls .
    , &.{.system});
    try testTokenize(
        \\\ls
        \\ ls
    , &.{ .system, .identifier });
    try testTokenize(
        \\\ls
        \\ ls
        \\1
    , &.{ .system, .identifier, .number_literal });
    try testTokenize(
        \\\ls
        \\/line comment
    , &.{.system});
    try testTokenize(
        \\\ls
        \\ /line comment
    , &.{.system});
    try testTokenize(
        \\\ls
        \\/line comment
        \\1
    , &.{ .system, .number_literal });
    try testTokenize(
        \\1
        \\/line comment
        \\ 2
    , &.{ .number_literal, .number_literal });
    try testTokenize(
        \\\ls
        \\/line comment
        \\ ls
    , &.{ .system, .identifier });
    try testTokenize(
        \\\ls
        \\/line comment
        \\ ls
        \\/line comment
    , &.{ .system, .identifier });
    try testTokenize(
        \\\ls
        \\/line comment
        \\ ls
        \\/line comment
        \\ ls
    , &.{ .system, .identifier, .identifier });
    try testTokenize(
        \\\ls
        \\/
        \\block comment
        \\\
        \\ ls
    , &.{ .system, .identifier });
    try testTokenize(
        \\\ls
        \\/
        \\block comment
        \\\
        \\ ls
        \\/
        \\block comment
        \\\
    , &.{ .system, .identifier });
    try testTokenize(
        \\\ls
        \\/
        \\block comment
        \\\
        \\ ls
        \\/
        \\block comment
        \\\
        \\ ls
    , &.{ .system, .identifier, .identifier });
    try testTokenize(
        \\\ls
        \\/
        \\block comment
        \\\
        \\\
        \\ trailing comment
    , &.{.system});
    try testTokenize(
        \\\ls
        \\/
        \\block comment
        \\\
        \\ ls
        \\\
        \\ trailing comment
    , &.{ .system, .identifier });
    try testTokenize(
        \\\ls
        \\\
        \\this is a trailing comment
    , &.{.system});
}

test "number literals" {
    try testTokenize("123e+e", &.{ .number_literal, .plus, .identifier });
    try testTokenize("123e-e", &.{ .number_literal, .minus, .identifier });
    try testTokenize("123e45", &.{.number_literal});
    try testTokenize("123e+45", &.{.number_literal});
    try testTokenize("123e-45", &.{.number_literal});
}

test "symbol literals" {
    try testTokenize("`symbol", &.{.symbol_literal});
    try testTokenize("`symbol:colon", &.{.symbol_literal});
    try testTokenize("`symbol:colon:colon", &.{.symbol_literal});
    try testTokenize("`symbol:colon/slash", &.{.symbol_literal});
    try testTokenize("`symbol/slash:slash", &.{ .symbol_literal, .slash, .identifier, .colon, .identifier });
    try testTokenize("`symbol/slash/slash", &.{ .symbol_literal, .slash, .identifier, .slash, .identifier });

    try testTokenizeMode(.k, "`symbol_underscore", &.{ .symbol_literal, .underscore, .identifier });
    try testTokenizeMode(.q, "`symbol_underscore", &.{.symbol_literal});
    try testTokenize("`_symbol", &.{ .symbol_literal, .underscore, .identifier });
    try testTokenizeMode(.k, "`symbol:colon_underscore", &.{ .symbol_literal, .underscore, .identifier });
    try testTokenizeMode(.q, "`symbol:colon_underscore", &.{.symbol_literal});
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
    const gpa = std.testing.allocator;

    var tokenizer = Tokenizer.init(source, mode);
    var tokens: std.ArrayList(Token.Tag) = .init(gpa);
    defer tokens.deinit();
    const last_token = while (true) {
        const token = tokenizer.next();
        if (token.tag == .eof) break token;
        try tokens.append(token.tag);
    };

    try std.testing.expectEqualSlices(Token.Tag, expected_token_tags, tokens.items);

    // Last token should always be eof, even when the last token was invalid,
    // in which case the tokenizer is in an invalid state, which can only be
    // recovered by opinionated means outside the scope of this implementation.
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
