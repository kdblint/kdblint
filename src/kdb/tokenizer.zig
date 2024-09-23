const std = @import("std");
const builtin = @import("builtin");
const Ast = @import("Ast.zig");
const Language = Ast.Language;

const Token = Ast.Token;

const log = std.log.scoped(.kdblint_tokenizer);

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    pending_token: ?Token = null,

    next_state: ?State = null,

    start_lambda: bool = false,
    start_table: bool = false,
    start_bracket: bool = false,
    end_bracket: bool = false,

    parens_count: u32 = 0,
    braces_count: u32 = 0,
    brackets_count: u32 = 0,

    language: Language,

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

    pub fn init(buffer: [:0]const u8, language: Language) Tokenizer {
        // Skip the UTF-8 BOM if present
        const src_start: usize = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0;
        var tokenizer = Tokenizer{
            .buffer = buffer,
            .language = language,
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

        os_param,
        os_param_no_comment,
        os_param_allow_comment,

        maybe_trailing_comment,
        start_trailing_comment,
        trailing_comment,

        maybe_block_comment,
        block_comment,
        end_block_comment,

        skip_line,

        skip_block_start,
        skip_block,
        skip_block_line,
        skip_block_maybe_block_comment,
        skip_block_block_comment,
        skip_block_end_block_comment,
    };

    fn consumeStartingComment(self: *Tokenizer) void {
        var state: CommentState = .start;

        var checkpoint = self.impl;
        while (true) : (self.advance()) {
            const c = self.buffer[self.impl.index];
            switch (state) {
                .start => switch (c) {
                    0 => {
                        self.impl = checkpoint;
                        break;
                    },
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        checkpoint = self.impl;
                    },
                    '/' => {
                        if (self.impl.character == 0) {
                            state = .maybe_block;
                        }
                    },
                    else => {
                        if (self.impl.character == 0) {
                            self.impl = checkpoint;
                            break;
                        }
                    },
                },

                .maybe_block => switch (c) {
                    0 => {
                        self.impl = checkpoint;
                        break;
                    },
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .block;
                    },
                    else => {
                        state = .line;
                    },
                },
                .block => switch (c) {
                    0 => {
                        self.impl = checkpoint;
                        break;
                    },
                    '\\' => {
                        if (self.impl.character == 0) {
                            state = .end_block;
                        }
                    },
                    else => {},
                },
                .end_block => switch (c) {
                    0 => {
                        self.impl = checkpoint;
                        break;
                    },
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        checkpoint = self.impl;
                        state = .start;
                    },
                    else => {
                        state = .block;
                    },
                },

                .line => switch (c) {
                    0 => {
                        self.impl = checkpoint;
                        break;
                    },
                    '\n' => {
                        checkpoint = self.impl;
                        state = .start;
                    },
                    else => {},
                },
            }
        }

        if (self.impl.index > 0) {
            self.pending_token = Token{
                .tag = .comment,
                .loc = .{
                    .start = 0,
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

        var state = self.next_state orelse .start;
        var result = Token{
            .tag = .eof,
            .loc = .{
                .start = self.impl.index,
                .end = undefined,
            },
            .eob = false,
        };

        var checkpoint: @TypeOf(self.impl) = undefined;
        while (true) : (self.advance()) {
            const c = self.buffer[self.impl.index];
            switch (state) {
                .start => switch (c) {
                    0 => break,
                    ' ', '\t', '\n', '\r' => {
                        result.loc.start = self.impl.index + 1;
                    },

                    '(' => {
                        result.tag = .l_paren;
                        self.advance();
                        self.parens_count += 1;
                        self.start_table = true;
                        self.start_bracket = false;
                        self.end_bracket = false;
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
                        self.start_lambda = true;
                        self.start_bracket = false;
                        self.end_bracket = false;
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
                        if (self.start_lambda) {
                            self.start_lambda = false;
                            self.start_bracket = true;
                        } else if (self.start_table) {
                            self.start_table = false;
                            self.start_bracket = true;
                        }
                        break;
                    },
                    ']' => {
                        result.tag = .r_bracket;
                        self.advance();
                        self.brackets_count = @max(0, @as(i32, @intCast(self.brackets_count)) - 1);
                        if (self.start_bracket) {
                            self.start_bracket = false;
                            self.end_bracket = true;
                        }
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

                        if (!self.end_bracket and self.impl.index >= 2) switch (self.buffer[self.impl.index - 2]) {
                            ' ', '\t', '\n', '\r', '(', '{', '[', ';' => {},
                            else => break,
                        };

                        result.tag = .number_literal;
                        state = .number;
                        self.advance();
                    },
                    '0'...'9' => {
                        if (!self.end_bracket and self.impl.index >= 2) switch (self.buffer[self.impl.index - 2]) {
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
                    '0'...'9', 'a'...'z', 'A'...'Z', '.', ':' => {},
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
                        if (self.language == .k) break;
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
                        if (self.language == .k) break;
                    },
                    else => break,
                },

                .identifier => switch (c) {
                    'a'...'z', 'A'...'Z', '0'...'9', '.' => {},
                    '_' => {
                        if (self.language == .k) {
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

                .os_param => switch (c) {
                    0, ' ', '\t', '\r' => break,
                    '\n' => {
                        self.next_state = .os_param_allow_comment;
                        break;
                    },
                    else => {},
                },
                .os_param_no_comment => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {
                        result.loc.start = self.impl.index + 1;
                    },
                    '\n' => {
                        result.loc.start = self.impl.index + 1;
                        state = .os_param_allow_comment;
                        self.next_state = .os_param_allow_comment;
                    },
                    else => {
                        result.tag = .os_param;
                        state = .os_param;
                    },
                },
                .os_param_allow_comment => switch (c) {
                    0 => break,
                    ' ', '\t', '\r', '\n' => {
                        result.loc.start = self.impl.index + 1;
                    },
                    '/' => {
                        if (self.impl.character == 0) {
                            result.tag = .comment;
                            state = .maybe_block_comment;
                        } else if (std.ascii.isWhitespace(self.buffer[self.impl.index - 1])) {
                            result.tag = .comment;
                            state = .skip_line;
                        }
                    },
                    else => {
                        result.tag = .os_param;
                        state = .os_param;
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
                        result.tag = .os;
                        state = .os_param;
                        self.next_state = .os_param_no_comment;
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
                        state = .skip_block_start;
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

                .skip_block_start => switch (c) {
                    0 => break,
                    '\n' => {
                        checkpoint = self.impl;
                        state = .skip_block;
                    },
                    else => {},
                },
                .skip_block => switch (c) {
                    0 => {
                        self.impl = checkpoint;
                        break;
                    },
                    ' ', '\t', '\r' => {
                        if (self.impl.character == 0) {
                            state = .skip_block_line;
                        }
                    },
                    '\n' => {},
                    '/' => {
                        if (self.impl.character == 0) {
                            state = .skip_block_maybe_block_comment;
                        }
                    },
                    else => {
                        if (self.impl.character == 0) {
                            self.impl = checkpoint;
                            break;
                        }
                    },
                },
                .skip_block_line => switch (c) {
                    0 => break,
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .skip_block;
                    },
                    else => {
                        state = .skip_block_start;
                    },
                },
                .skip_block_maybe_block_comment => switch (c) {
                    0 => {
                        self.impl = checkpoint;
                        break;
                    },
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .skip_block_block_comment;
                    },
                    else => {
                        state = .skip_block;
                    },
                },
                .skip_block_block_comment => switch (c) {
                    0 => {
                        self.impl = checkpoint;
                        break;
                    },
                    '\\' => {
                        if (self.impl.character == 0) {
                            state = .skip_block_end_block_comment;
                        }
                    },
                    else => {},
                },
                .skip_block_end_block_comment => switch (c) {
                    0 => {
                        self.impl = checkpoint;
                        break;
                    },
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .skip_block;
                    },
                    else => {
                        state = .skip_block_block_comment;
                    },
                },
            }
        }

        result.loc.end = self.impl.index;

        if (result.tag == .semicolon and self.parens_count == 0 and self.braces_count == 0 and self.brackets_count == 0) {
            result.eob = true;
        } else if (result.tag != .comment and self.endsBlock()) {
            result.eob = true;
            self.parens_count = 0;
            self.braces_count = 0;
            self.brackets_count = 0;
            self.next_state = null;
        }

        if (self.start_lambda and switch (result.tag) {
            .l_brace, .comment => false,
            else => true,
        }) {
            self.start_lambda = false;
            self.start_bracket = false;
            self.end_bracket = false;
        }

        if (self.start_table and switch (result.tag) {
            .l_paren, .comment => false,
            else => true,
        }) {
            self.start_table = false;
            self.start_bracket = false;
            self.end_bracket = false;
        }

        if (self.end_bracket and switch (result.tag) {
            .r_bracket, .comment => false,
            else => true,
        }) {
            self.start_lambda = false;
            self.start_table = false;
            self.start_bracket = false;
            self.end_bracket = false;
        }

        return result;
    }

    fn endsBlock(self: *Tokenizer) bool {
        const checkpoint = self.impl;
        defer self.impl = checkpoint;

        var state: CommentState = .start;
        while (true) : (self.advance()) {
            const c = self.buffer[self.impl.index];
            switch (state) {
                .start => switch (c) {
                    0 => return true,
                    ' ', '\t', '\n', '\r' => {},
                    '/' => {
                        if (self.impl.character == 0) {
                            state = .maybe_block;
                        } else if (std.ascii.isWhitespace(self.buffer[self.impl.index - 1])) {
                            if (self.next_state == .os_param_no_comment) return false;
                            state = .line;
                        } else {
                            return false;
                        }
                    },
                    else => return self.impl.character == 0,
                },

                .maybe_block => switch (c) {
                    0 => return true,
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .block;
                    },
                    else => {
                        state = .line;
                    },
                },
                .block => switch (c) {
                    0 => return true,
                    '\\' => {
                        if (self.impl.character == 0) {
                            state = .end_block;
                        }
                    },
                    else => {},
                },
                .end_block => switch (c) {
                    0 => return true,
                    ' ', '\t', '\r' => {},
                    '\n' => {
                        state = .start;
                    },
                    else => {
                        state = .block;
                    },
                },

                .line => switch (c) {
                    0 => return true,
                    '\n' => {
                        state = .start;
                    },
                    else => {},
                },
            }
        }

        unreachable;
    }
};

pub fn testTokenize(source: [:0]const u8, expected: []const Token) !void {
    inline for (&.{ .k, .q }) |language| {
        try testTokenizeLanguage(language, source, expected);
    }
}

fn testTokenizeLanguage(language: Language, source: [:0]const u8, expected: []const Token) !void {
    var tokenizer = Tokenizer.init(source, language);

    var actual = std.ArrayList(Token).init(std.testing.allocator);
    defer actual.deinit();
    while (true) {
        const token = tokenizer.next();
        try actual.append(token);
        if (token.tag == .eof) break;

        // Tokens should never end in a newline.
        try std.testing.expect(source[token.loc.end - 1] != '\n');
    }

    try std.testing.expectEqualSlices(Token, expected, actual.items[0 .. actual.items.len - 1]);
    try std.testing.expectEqual(Token{
        .tag = .eof,
        .loc = .{ .start = source.len, .end = source.len },
        .eob = true,
    }, actual.items[actual.items.len - 1]);
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
        .{ .tag = .comment, .loc = .{ .start = 0, .end = 140 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 141, .end = 145 }, .eob = true },
    });
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\\
        \\trailing comment
    , &.{
        .{ .tag = .comment, .loc = .{ .start = 0, .end = 55 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 56, .end = 74 }, .eob = false },
    });
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\\d .
        \\identifier
    , &.{
        .{ .tag = .comment, .loc = .{ .start = 0, .end = 55 }, .eob = false },
        .{ .tag = .os, .loc = .{ .start = 56, .end = 58 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 59, .end = 60 }, .eob = true },
        .{ .tag = .identifier, .loc = .{ .start = 61, .end = 71 }, .eob = true },
    });
    try testTokenize(
        \\ this is a starting
        \\ comment that spans
        \\ multiple lines
        \\\ d .
        \\identifier
    , &.{
        .{ .tag = .comment, .loc = .{ .start = 0, .end = 55 }, .eob = false },
        .{ .tag = .invalid, .loc = .{ .start = 56, .end = 61 }, .eob = true },
        .{ .tag = .identifier, .loc = .{ .start = 62, .end = 72 }, .eob = true },
    });
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
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 21 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 22, .end = 41 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 42, .end = 43 }, .eob = true },
    });
    try testTokenize(
        \\1
        \\/
        \\block comment 1
        \\\
        \\/
        \\block comment 2
        \\\
        \\ 2
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 21 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 22, .end = 41 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 43, .end = 44 }, .eob = true },
    });
    try testTokenize(
        \\(1;
        \\/
        \\block comment 1
        \\\
        \\/
        \\block comment 2
        \\\
        \\2)
    , &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 23 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 24, .end = 43 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 44, .end = 45 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 45, .end = 46 }, .eob = true },
    });
    try testTokenize(
        \\(1;
        \\/
        \\block comment 1
        \\\
        \\/
        \\block comment 2
        \\\
        \\ 2)
    , &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 23 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 24, .end = 43 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 45, .end = 46 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 46, .end = 47 }, .eob = true },
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
        .{ .tag = .keyword_q_not, .loc = .{ .start = 2, .end = 5 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 8, .end = 12 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 13, .end = 20 }, .eob = true },
    });
    try testTokenize("1/ not a line comment", &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .slash, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .keyword_q_not, .loc = .{ .start = 3, .end = 6 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 9, .end = 13 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 14, .end = 21 }, .eob = true },
    });
    try testTokenize(
        \\1 /line comment 1
        \\/line comment 2
        \\2
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 17 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 18, .end = 33 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 34, .end = 35 }, .eob = true },
    });
    try testTokenize(
        \\1 /line comment 1
        \\ /line comment 2
        \\2
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 17 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 19, .end = 34 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 35, .end = 36 }, .eob = true },
    });
    try testTokenize(
        \\1 /line comment 1
        \\/line comment 2
        \\ 2
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 17 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 18, .end = 33 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 35, .end = 36 }, .eob = true },
    });
    try testTokenize(
        \\1 /line comment 1
        \\ /line comment 2
        \\ 2
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 17 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 19, .end = 34 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 36, .end = 37 }, .eob = true },
    });
    try testTokenize(
        \\(1; /line comment 1
        \\/line comment 2
        \\2)
    , &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 19 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 20, .end = 35 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 36, .end = 37 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 37, .end = 38 }, .eob = true },
    });
    try testTokenize(
        \\(1; /line comment 1
        \\ /line comment 2
        \\2)
    , &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 19 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 21, .end = 36 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 37, .end = 38 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 38, .end = 39 }, .eob = true },
    });
    try testTokenize(
        \\(1; /line comment 1
        \\/line comment 2
        \\ 2)
    , &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 19 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 20, .end = 35 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 37, .end = 38 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 38, .end = 39 }, .eob = true },
    });
    try testTokenize(
        \\(1; /line comment 1
        \\ /line comment 2
        \\ 2)
    , &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .semicolon, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 19 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 21, .end = 36 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 38, .end = 39 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 39, .end = 40 }, .eob = true },
    });
}

test "tokenize trailing comment" {
    try testTokenize(
        \\1
        \\\
        \\this is a
        \\trailing comment
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 30 }, .eob = false },
    });
    try testTokenize(
        \\1
        \\\ this is not a trailing comment
        \\1
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = true },
        .{ .tag = .invalid, .loc = .{ .start = 2, .end = 34 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 35, .end = 36 }, .eob = true },
    });
}

test "tokenize OS" {
    try testTokenize(
        \\\ls
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = true },
    });
    try testTokenize(
        \\\ls /not a comment
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 4, .end = 8 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 9, .end = 10 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 11, .end = 18 }, .eob = true },
    });
    try testTokenize(
        \\\ls .
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 4, .end = 5 }, .eob = true },
    });
    try testTokenize(
        \\\ls
        \\ ls
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 5, .end = 7 }, .eob = true },
    });
    try testTokenize(
        \\\ls
        \\ ls
        \\1
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 5, .end = 7 }, .eob = true },
        .{ .tag = .number_literal, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize(
        \\\ls
        \\/line comment
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 17 }, .eob = false },
    });
    try testTokenize(
        \\\ls
        \\ /line comment
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 5, .end = 18 }, .eob = false },
    });
    try testTokenize(
        \\\ls
        \\/line comment
        \\1
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 17 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 18, .end = 19 }, .eob = true },
    });
    try testTokenize(
        \\1
        \\/line comment
        \\ 2
    , &.{
        .{ .tag = .number_literal, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 2, .end = 15 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 17, .end = 18 }, .eob = true },
    });
    try testTokenize(
        \\\ls
        \\/line comment
        \\ ls
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 17 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 19, .end = 21 }, .eob = true },
    });
    try testTokenize(
        \\\ls
        \\/line comment
        \\ ls
        \\/line comment
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 17 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 19, .end = 21 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 22, .end = 35 }, .eob = false },
    });
    try testTokenize(
        \\\ls
        \\/line comment
        \\ ls
        \\/line comment
        \\ ls
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 17 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 19, .end = 21 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 22, .end = 35 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 37, .end = 39 }, .eob = true },
    });
    try testTokenize(
        \\\ls
        \\/
        \\block comment
        \\\
        \\ ls
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 21 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 23, .end = 25 }, .eob = true },
    });
    try testTokenize(
        \\\ls
        \\/
        \\block comment
        \\\
        \\ ls
        \\/
        \\block comment
        \\\
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 21 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 23, .end = 25 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 26, .end = 43 }, .eob = false },
    });
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
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 21 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 23, .end = 25 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 26, .end = 43 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 45, .end = 47 }, .eob = true },
    });
    try testTokenize(
        \\\ls
        \\/
        \\block comment
        \\\
        \\\
        \\ trailing comment
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 21 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 22, .end = 41 }, .eob = false },
    });
    try testTokenize(
        \\\ls
        \\/
        \\block comment
        \\\
        \\ ls
        \\\
        \\ trailing comment
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = false },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 21 }, .eob = false },
        .{ .tag = .os_param, .loc = .{ .start = 23, .end = 25 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 26, .end = 45 }, .eob = false },
    });
    try testTokenize(
        \\\ls
        \\\
        \\this is a trailing comment
    , &.{
        .{ .tag = .os, .loc = .{ .start = 0, .end = 3 }, .eob = true },
        .{ .tag = .comment, .loc = .{ .start = 4, .end = 32 }, .eob = false },
    });
}

test {
    @import("std").testing.refAllDecls(@This());
}
