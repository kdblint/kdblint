//! Abstract Syntax Tree for kdb+ source code.

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: TokenList.Slice,
/// The root AST ndoe is assumed to be index 0. Since there can be no
/// references to the root node, this means 0 is available to indicate null.
nodes: NodeList.Slice,
extra_data: []Node.Index,
table_columns: [][]const u8,

errors: []const Error,

tokenize_duration: u64,
parse_duration: u64,

pub const TokenIndex = u32;
pub const ByteOffset = u32;

pub const TokenList = std.MultiArrayList(Token);
pub const NodeList = std.MultiArrayList(Node);

pub const Location = struct {
    line: usize,
    column: usize,
    line_start: usize,
    line_end: usize,
};

pub fn deinit(tree: *Ast, gpa: Allocator) void {
    tree.tokens.deinit(gpa);
    tree.nodes.deinit(gpa);
    gpa.free(tree.extra_data);
    for (tree.table_columns) |slice| gpa.free(slice);
    gpa.free(tree.table_columns);
    gpa.free(tree.errors);
    tree.* = undefined;
}

pub const RenderError = error{
    /// Ran out of memory allocating call stack frames to complete rendering, or
    /// ran out of memory allocating space in the output buffer.
    OutOfMemory,
};

pub const Version = enum {
    v4_0,
};

pub const Language = enum {
    k,
    q,
};

pub const ParseSettings = struct {
    version: Version,
    language: Language,
};

/// Result should be freed with tree.deinit() when there are
/// no more references to any of the tokens or nodes.
pub fn parse(gpa: Allocator, source: [:0]const u8, settings: ParseSettings) Allocator.Error!Ast {
    const tokenize_start = diagnostics.now();
    var tokens = Ast.TokenList{};
    defer tokens.deinit(gpa);

    // Empirically, the zig std lib has an 8:1 ratio of source bytes to token count.
    const estimated_token_count = source.len / 8;
    try tokens.ensureTotalCapacity(gpa, estimated_token_count);

    var tokenizer = Tokenizer.init(source, settings.language);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(gpa, token);
        if (token.tag == .eof) break;
    }
    const tokenize_duration = diagnostics.now().since(tokenize_start);

    const parse_start = diagnostics.now();
    var parser: Parse = .{
        .source = source,
        .gpa = gpa,
        .token_tags = tokens.items(.tag),
        .token_locs = tokens.items(.loc),
        .token_eobs = tokens.items(.eob),
        .version = settings.version,
        .language = settings.language,
    };
    defer parser.deinit();

    // Empirically, Zig source code has a 2:1 ratio of tokens to AST nodes.
    // Make sure at least 1 so we can use appendAssumeCapacity on the root node below.
    const estimated_node_count = (tokens.len + 2) / 2;
    try parser.nodes.ensureTotalCapacity(gpa, estimated_node_count);

    try parser.parseRoot();
    const parse_duration = diagnostics.now().since(parse_start);

    // TODO experiment with compacting the MultiArrayList slices here
    return Ast{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(gpa),
        .table_columns = try parser.table_columns.toOwnedSlice(gpa),
        .errors = try parser.errors.toOwnedSlice(gpa),
        .tokenize_duration = tokenize_duration,
        .parse_duration = parse_duration,
    };
}

pub fn extraData(tree: Ast, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        comptime assert(switch (field.type) {
            Node.Index, Node.SelectData, Node.ExecData => true,
            else => false,
        });
        @field(result, field.name) = @bitCast(tree.extra_data[index + i]);
    }
    return result;
}

pub const Error = struct {
    tag: Tag,
    is_note: bool = false,
    /// True if `token` points to the token before the token causing an issue.
    token_is_prev: bool = false,
    token: TokenIndex,
    extra: union {
        none: void,
        expected_tag: Token.Tag,
        expected_string: []const u8,
    } = .{ .none = {} },

    pub const Tag = enum {
        /// `expected_tag` is populated.
        expected_token,

        expected_semi_after_arg,
        expected_expr,
        expected_end_of_block,
        expected_prefix_expr,
        expected_infix_expr,

        not_yet_implemented,
        parse_error,

        /// `expected_string` is populated.
        expected_qsql_token,

        expected_from,
        expected_select_phrase,
        expected_by_phrase,
    };
};

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Index = u32;

    comptime {
        // Goal is to keep this under one byte for efficiency.
        std.debug.assert(@sizeOf(Tag) <= 1);
    }

    pub const Tag = enum {
        /// extra_data[lhs...rhs]
        root,

        /// `(lhs)`. main_token is the `(`; rhs is the token index of the `)`.
        grouped_expression,
        /// `()`. lhs is unused. main_token is the `(`; rhs is the token index of the `)`.
        empty_list,
        /// `(lhs)`. `SubRange[lhs]`. main_token is the `(`; rhs is the token index of the `)`.
        list,
        /// `([]lhs)`. `Table[lhs]`. main_token is the `(`; rhs is the token index of the `)`.
        table_literal,

        /// Both lhs and rhs unused.
        number_literal,
        /// Both lhs and rhs unused.
        string_literal,
        /// Both lhs and rhs unused.
        symbol_literal,
        /// Both lhs and rhs unused.
        symbol_list_literal,
        /// Both lhs and rhs unused.
        identifier,

        /// Both lhs and rhs unused.
        colon,
        /// Both lhs and rhs unused.
        colon_colon,
        /// Both lhs and rhs unused.
        plus,
        /// Both lhs and rhs unused.
        plus_colon,
        /// Both lhs and rhs unused.
        minus,
        /// Both lhs and rhs unused.
        minus_colon,
        /// Both lhs and rhs unused.
        asterisk,
        /// Both lhs and rhs unused.
        asterisk_colon,
        /// Both lhs and rhs unused.
        percent,
        /// Both lhs and rhs unused.
        percent_colon,
        /// Both lhs and rhs unused.
        bang,
        /// Both lhs and rhs unused.
        bang_colon,
        /// Both lhs and rhs unused.
        ampersand,
        /// Both lhs and rhs unused.
        ampersand_colon,
        /// Both lhs and rhs unused.
        pipe,
        /// Both lhs and rhs unused.
        pipe_colon,
        /// Both lhs and rhs unused.
        angle_bracket_left,
        /// Both lhs and rhs unused.
        angle_bracket_left_colon,
        /// Both lhs and rhs unused.
        angle_bracket_left_equal,
        /// Both lhs and rhs unused.
        angle_bracket_left_right,
        /// Both lhs and rhs unused.
        angle_bracket_right,
        /// Both lhs and rhs unused.
        angle_bracket_right_colon,
        /// Both lhs and rhs unused.
        angle_bracket_right_equal,
        /// Both lhs and rhs unused.
        equal,
        /// Both lhs and rhs unused.
        equal_colon,
        /// Both lhs and rhs unused.
        tilde,
        /// Both lhs and rhs unused.
        tilde_colon,
        /// Both lhs and rhs unused.
        comma,
        /// Both lhs and rhs unused.
        comma_colon,
        /// Both lhs and rhs unused.
        caret,
        /// Both lhs and rhs unused.
        caret_colon,
        /// Both lhs and rhs unused.
        hash,
        /// Both lhs and rhs unused.
        hash_colon,
        /// Both lhs and rhs unused.
        underscore,
        /// Both lhs and rhs unused.
        underscore_colon,
        /// Both lhs and rhs unused.
        dollar,
        /// Both lhs and rhs unused.
        dollar_colon,
        /// Both lhs and rhs unused.
        question_mark,
        /// Both lhs and rhs unused.
        question_mark_colon,
        /// Both lhs and rhs unused.
        at,
        /// Both lhs and rhs unused.
        at_colon,
        /// Both lhs and rhs unused.
        dot,
        /// Both lhs and rhs unused.
        dot_colon,
        /// Both lhs and rhs unused.
        zero_colon,
        /// Both lhs and rhs unused.
        zero_colon_colon,
        /// Both lhs and rhs unused.
        one_colon,
        /// Both lhs and rhs unused.
        one_colon_colon,
        /// Both lhs and rhs unused.
        two_colon,

        /// `lhs : rhs`. main_token is `:`.
        assign,
        /// `lhs :: rhs`. main_token is `::`.
        global_assign,
        /// `lhs + rhs`. main_token is `+`.
        add,
        /// `lhs +: rhs`. main_token is `+:`.
        plus_assign,
        /// `lhs - rhs`. main_token is `-`.
        subtract,
        /// `lhs -: rhs`. main_token is `-:`.
        minus_assign,
        /// `lhs * rhs`. main_token is `*`.
        multiply,
        /// `lhs *: rhs`. main_token is `*:`.
        asterisk_assign,
        /// `lhs % rhs`. main_token is `%`.
        divide,
        /// `lhs %: rhs`. main_token is `%:`.
        percent_assign,
        /// `lhs ! rhs`. main_token is `!`.
        dict,
        /// `lhs !: rhs`. main_token is `!:`.
        bang_assign,
        /// `lhs & rhs`. main_token is `&`.
        lesser,
        /// `lhs &: rhs`. main_token is `&:`.
        ampersand_assign,
        /// `lhs | rhs`. main_token is `|`.
        greater,
        /// `lhs |: rhs`. main_token is `|:`.
        pipe_assign,
        /// `lhs < rhs`. main_token is `<`.
        less_than,
        /// `lhs <: rhs`. main_token is `<:`.
        angle_bracket_left_assign,
        /// `lhs <= rhs`. main_token is `<=`.
        less_than_equal,
        /// `lhs <> rhs`. main_token is `<>`.
        not_equal,
        /// `lhs > rhs`. main_token is `>`.
        greater_than,
        /// `lhs >: rhs`. main_token is `>:`.
        angle_bracket_right_assign,
        /// `lhs >= rhs`. main_token is `>=`.
        greater_than_equal,
        /// `lhs = rhs`. main_token is `=`.
        equals,
        /// `lhs =: rhs`. main_token is `=:`.
        equal_assign,
        /// `lhs ~ rhs`. main_token is `~`.
        match,
        /// `lhs ~: rhs`. main_token is `~:`.
        tilde_assign,
        /// `lhs , rhs`. main_token is `,`.
        join,
        /// `lhs ,: rhs`. main_token is `,:`.
        comma_assign,
        /// `lhs ^ rhs`. main_token is `^`.
        fill,
        /// `lhs ^: rhs`. main_token is `^:`.
        caret_assign,
        /// `lhs # rhs`. main_token is `#`.
        take,
        /// `lhs #: rhs`. main_token is `#:`.
        hash_assign,
        /// `lhs _ rhs`. main_token is `_`.
        drop,
        /// `lhs _: rhs`. main_token is `_:`.
        underscore_assign,
        /// `lhs $ rhs`. main_token is `$`.
        cast,
        /// `lhs $: rhs`. main_token is `$:`.
        dollar_assign,
        /// `lhs ? rhs`. main_token is `?`.
        find,
        /// `lhs ?: rhs`. main_token is `?:`.
        question_mark_assign,
        /// `lhs @ rhs`. main_token is `@`.
        apply,
        /// `lhs @: rhs`. main_token is `@:`.
        at_assign,
        /// `lhs . rhs`. main_token is `.`.
        apply_n,
        /// `lhs .: rhs`. main_token is `.:`.
        dot_assign,
        /// `lhs 0: rhs`. main_token is `0:`.
        file_text,
        /// `lhs 0:: rhs`. main_token is `0::`.
        zero_colon_assign,
        /// `lhs 1: rhs`. main_token is `1:`.
        file_binary,
        /// `lhs 1:: rhs`. main_token is `1::`.
        one_colon_assign,
        /// `lhs 2: rhs`. main_token is `2:`.
        dynamic_load,

        /// `lhs rhs`. main_token is unused.
        implicit_apply,

        /// Both lhs and rhs unused.
        apostrophe,
        /// Both lhs and rhs unused.
        apostrophe_colon,
        /// Both lhs and rhs unused.
        slash,
        /// Both lhs and rhs unused.
        slash_colon,
        /// Both lhs and rhs unused.
        backslash,
        /// Both lhs and rhs unused.
        backslash_colon,

        /// `lhs ' rhs`. rhs can be omitted. main_token is `'`.
        apostrophe_infix,
        /// `lhs ': rhs`. rhs can be omitted. main_token is `':`.
        apostrophe_colon_infix,
        /// `lhs / rhs`. rhs can be omitted. main_token is `/`.
        slash_infix,
        /// `lhs /: rhs`. rhs can be omitted. main_token is `/:`.
        slash_colon_infix,
        /// `lhs \ rhs`. rhs can be omitted. main_token is `\`.
        backslash_infix,
        /// `lhs \: rhs`. rhs can be omitted. main_token is `\:`.
        backslash_colon_infix,

        /// `{[lhs]rhs}`. `SubRange[lhs]`. rhs or lhs can be omitted. main_token is `{`.
        lambda_one,
        /// Same as lambda_one but there is known to be a semicolon before the rbrace.
        lambda_one_semicolon,
        /// `{[lhs]rhs}`. `SubRange[lhs]`. `SubRange[rhs]`. lhs can be omitted. main_token is `{`.
        lambda,
        /// Same as lambda but there is known to be a semicolon before the rbrace.
        lambda_semicolon,

        /// `[a;b;c]`. `extra_data[lhs..rhs]`. main_token is `[`.
        block,

        /// `lhs[rhs]`. rhs can be omitted. main_token is `[`.
        call_one,
        /// `lhs[a;b;c]`. `SubRange[rhs]`. main_token is `[`.
        call,

        /// `lhs`. rhs is unused. main_token is unused.
        implicit_return,
        /// `:lhs`. rhs is unused. main_token is ':'.
        @"return",

        /// Both lhs and rhs unused.
        abs,
        /// Both lhs and rhs unused.
        acos,
        /// Both lhs and rhs unused.
        asin,
        /// Both lhs and rhs unused.
        atan,
        /// Both lhs and rhs unused.
        avg,
        /// Both lhs and rhs unused.
        cos,
        /// Both lhs and rhs unused.
        dev,
        /// Both lhs and rhs unused.
        enlist,
        /// Both lhs and rhs unused.
        exit,
        /// Both lhs and rhs unused.
        exp,
        /// Both lhs and rhs unused.
        getenv,
        /// Both lhs and rhs unused.
        hopen,
        /// Both lhs and rhs unused.
        last,
        /// Both lhs and rhs unused.
        log,
        /// Both lhs and rhs unused.
        max,
        /// Both lhs and rhs unused.
        min,
        /// Both lhs and rhs unused.
        prd,
        /// Both lhs and rhs unused.
        sin,
        /// Both lhs and rhs unused.
        sqrt,
        /// Both lhs and rhs unused.
        sum,
        /// Both lhs and rhs unused.
        tan,
        /// Both lhs and rhs unused.
        @"var",

        /// Both lhs and rhs unused.
        bin,
        /// `lhs bin rhs`. main_token is `bin`.
        bin_infix,
        /// Both lhs and rhs unused.
        binr,
        /// `lhs binr rhs`. main_token is `binr`.
        binr_infix,
        /// Both lhs and rhs unused.
        cor,
        /// `lhs cor rhs`. main_token is `cor`.
        cor_infix,
        /// Both lhs and rhs unused.
        cov,
        /// `lhs cov rhs`. main_token is `cov`.
        cov_infix,
        /// Both lhs and rhs unused.
        div,
        /// `lhs div rhs`. main_token is `div`.
        div_infix,
        /// Both lhs and rhs unused.
        in,
        /// `lhs in rhs`. main_token is `in`.
        in_infix,
        /// Both lhs and rhs unused.
        insert,
        /// `lhs insert rhs`. main_token is `insert`.
        insert_infix,
        /// Both lhs and rhs unused.
        like,
        /// `lhs like rhs`. main_token is `like`.
        like_infix,
        /// Both lhs and rhs unused.
        setenv,
        /// `lhs setenv rhs`. main_token is `setenv`.
        setenv_infix,
        /// Both lhs and rhs unused.
        ss,
        /// `lhs ss rhs`. main_token is `ss`.
        ss_infix,
        /// Both lhs and rhs unused.
        wavg,
        /// `lhs wavg rhs`. main_token is `wavg`.
        wavg_infix,
        /// Both lhs and rhs unused.
        within,
        /// `lhs within rhs`. main_token is `within`.
        within_infix,
        /// Both lhs and rhs unused.
        wsum,
        /// `lhs wsum rhs`. main_token is `wsum`.
        wsum_infix,
        /// Both lhs and rhs unused.
        xexp,
        /// `lhs xexp rhs`. main_token is `xexp`.
        xexp_infix,

        /// `do[lhs;rhs]`. rhs can be omitted. main_token is `do`.
        do_one,
        /// `do[lhs;rhs]`. `SubRange[rhs]`. main_token is `do`.
        do,
        /// `if[lhs;rhs]`. rhs can be omitted. main_token is `if`.
        if_one,
        /// `if[lhs;rhs]`. `SubRange[rhs]`. main_token is `if`.
        @"if",
        /// `while[lhs;rhs]`. rhs can be omitted. main_token is `while`.
        while_one,
        /// `while[lhs;rhs]`. `SubRange[rhs]`. main_token is `while`.
        @"while",

        /// `select rhs`. `Select[rhs]`. main_token is `select`.
        select,
        /// `exec rhs`. `Exec[rhs]`. main_token is `exec`.
        exec,
        /// `update rhs`. `Update[rhs]`. main_token is `update`.
        update,
        /// `delete rhs`. `DeleteRows[rhs]`. main_token is `delete`.
        delete_rows,
        /// `delete rhs`. `DeleteCols[rhs]`. main_token is `delete`.
        delete_cols,
    };

    pub const Data = struct {
        lhs: Index,
        rhs: Index,
    };

    pub const SubRange = struct {
        /// Index into extra_data.
        start: Index,
        /// Index into extra_data.
        end: Index,
    };

    pub const Table = struct {
        /// Index into table_columns.
        column_start: Index,
        /// Index into extra_data.
        expr_start: Index,
        len: u32,
    };

    pub const SelectData = packed struct(u32) {
        has_by: bool,
        distinct: bool,
        ascending: bool,
        _: u29 = 0,
    };

    pub const Select = struct {
        from: Index,
        /// Index into extra_data.
        where: Index,
        /// Index into extra_data.
        by: Index,
        /// Index into table_columns.
        by_columns: Index,
        /// Index into extra_data.
        select: Index,
        /// Index into extra_data.
        select_end: Index,
        /// Index into table_columns.
        select_columns: Index,
        limit: Index,
        order: TokenIndex,
        data: SelectData,
    };

    pub const ExecData = packed struct(u32) {
        has_by_columns: bool,
        has_select_columns: bool,
        _: u30 = 0,
    };

    pub const Exec = struct {
        from: Index,
        /// Index into extra_data.
        where: Index,
        /// Index into extra_data.
        by: Index,
        /// Index into table_columns.
        by_columns: Index,
        /// Index into extra_data.
        select: Index,
        /// Index into extra_data.
        select_end: Index,
        /// Index into table_columns.
        select_columns: Index,
        data: ExecData,
    };

    pub const Update = struct {
        from: Index,
        /// Index into extra_data.
        where: Index,
        /// Index into extra_data.
        by: Index,
        /// Index into table_columns.
        by_columns: Index,
        /// Index into extra_data.
        select: Index,
        /// Index into extra_data.
        select_end: Index,
        /// Index into table_columns.
        select_columns: Index,
    };

    pub const DeleteRows = struct {
        from: Index,
        /// Index into extra_data.
        where: Index,
        /// Index into extra_data.
        where_end: Index,
    };

    pub const DeleteColumns = struct {
        from: Index,
        /// Index into table_columns.
        select_columns: Index,
        /// Index into table_columns.
        select_columns_end: Index,
    };
};

fn getTokenTag(tree: Ast, i: Node.Index) Token.Tag {
    return tree.tokens.items(.tag)[tree.getMainToken(i)];
}

fn getTokenLoc(tree: Ast, i: Node.Index) Token.Loc {
    return tree.tokens.items(.loc)[tree.getMainToken(i)];
}

fn getTokenEob(tree: Ast, i: Node.Index) bool {
    return tree.tokens.items(.eob)[tree.getMainToken(i)];
}

fn getSource(tree: Ast, i: Node.Index) []const u8 {
    const loc = tree.getTokenLoc(i);
    return tree.source[loc.start..loc.end];
}

fn getTag(tree: Ast, i: Node.Index) Node.Tag {
    return tree.nodes.items(.tag)[i];
}

fn getMainToken(tree: Ast, i: Node.Index) TokenIndex {
    return tree.nodes.items(.main_token)[i];
}

fn getData(tree: Ast, i: Node.Index) Node.Data {
    return tree.nodes.items(.data)[i];
}

fn getExtraData(tree: Ast, i: usize) Node.Index {
    return tree.extra_data[i];
}

fn getLastToken(tree: Ast, i: Node.Index) TokenIndex {
    switch (tree.getTag(i)) {
        .root => unreachable,
        .grouped_expression,
        .empty_list,
        .list,
        .table_literal,
        => return tree.getData(i).rhs,
        .number_literal => {
            var index = i;
            while (true) {
                const data = tree.getData(index);
                if (data.rhs == 0) {
                    return tree.getMainToken(index);
                }
                index = data.rhs;
            }
        },
        .string_literal,
        .symbol_literal,
        .symbol_list_literal,
        .identifier,
        => return tree.getMainToken(i),
        .assign,
        .global_assign,
        .add,
        .plus_assign,
        .subtract,
        .minus_assign,
        .multiply,
        .asterisk_assign,
        .divide,
        .percent_assign,
        .dict,
        .bang_assign,
        .lesser,
        .ampersand_assign,
        .greater,
        .pipe_assign,
        .less_than,
        .angle_bracket_left_assign,
        .less_than_equal,
        .not_equal,
        .greater_than,
        .angle_bracket_right_assign,
        .greater_than_equal,
        .equals,
        .equal_assign,
        .match,
        .tilde_assign,
        .join,
        .comma_assign,
        .fill,
        .caret_assign,
        .take,
        .hash_assign,
        .drop,
        .underscore_assign,
        .cast,
        .dollar_assign,
        .find,
        .question_mark_assign,
        .apply,
        .at_assign,
        .apply_n,
        .dot_assign,
        .file_text,
        .zero_colon_assign,
        .file_binary,
        .one_colon_assign,
        .dynamic_load,

        .apostrophe_infix,
        .apostrophe_colon_infix,
        .slash_infix,
        .slash_colon_infix,
        .backslash_infix,
        .backslash_colon_infix,

        .bin_infix,
        .binr_infix,
        .cor_infix,
        .cov_infix,
        .div_infix,
        .in_infix,
        .insert_infix,
        .like_infix,
        .setenv_infix,
        .ss_infix,
        .wavg_infix,
        .within_infix,
        .wsum_infix,
        .xexp_infix,
        => {
            const data = tree.getData(i);
            if (data.rhs > 0) {
                return tree.getLastToken(data.rhs);
            }
            return tree.getMainToken(i);
        },
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
        .bang,
        .bang_colon,
        .ampersand,
        .ampersand_colon,
        .pipe,
        .pipe_colon,
        .angle_bracket_left,
        .angle_bracket_left_colon,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_colon,
        .angle_bracket_right_equal,
        .equal,
        .equal_colon,
        .tilde,
        .tilde_colon,
        .comma,
        .comma_colon,
        .caret,
        .caret_colon,
        .hash,
        .hash_colon,
        .underscore,
        .underscore_colon,
        .dollar,
        .dollar_colon,
        .question_mark,
        .question_mark_colon,
        .at,
        .at_colon,
        .dot,
        .dot_colon,
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
        => return tree.getMainToken(i),
        .implicit_apply => {
            const data = tree.getData(i);
            if (data.rhs > 0) {
                return tree.getLastToken(data.rhs);
            }
            unreachable;
        },
        .lambda_one,
        .lambda_one_semicolon,
        => {
            const data = tree.getData(i);
            var last_token = (if (data.rhs > 0) blk: {
                break :blk tree.getLastToken(data.rhs);
            } else if (data.lhs > 0) blk: {
                const sub_range = tree.extraData(data.lhs, Node.SubRange);
                break :blk tree.getExtraData(sub_range.end - 1);
            } else blk: {
                break :blk tree.getMainToken(i);
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .semicolon,
                    .comment,
                    .l_bracket,
                    .r_bracket,
                    => {},
                    else => break,
                }
            }
            return last_token;
        },
        .lambda,
        .lambda_semicolon,
        => {
            const data = tree.getData(i);
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            const node_i = tree.getExtraData(sub_range.end - 1);
            var last_token = tree.getLastToken(node_i) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .semicolon => {},
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },
        .block => {
            const data = tree.getData(i);
            var last_token = (if (data.lhs == data.rhs) blk: {
                break :blk tree.getMainToken(i);
            } else blk: {
                for (data.lhs..data.rhs, 1..) |_, temp_i| {
                    const node_i = tree.getExtraData(data.rhs - temp_i);
                    if (node_i > 0) {
                        break :blk tree.getLastToken(node_i);
                    }
                }
                break :blk tree.getMainToken(i);
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .semicolon => {},
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },
        .call_one => {
            const data = tree.getData(i);
            var last_token = (if (data.rhs > 0) blk: {
                break :blk tree.getLastToken(data.rhs);
            } else blk: {
                break :blk tree.getMainToken(i);
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },
        .call,
        .do,
        .@"if",
        .@"while",
        => {
            const data = tree.getData(i);
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            var last_token = (if (sub_range.start == sub_range.end) blk: {
                break :blk tree.getMainToken(i);
            } else blk: {
                var extra_data_i = sub_range.end - 1;
                var node_i = tree.getExtraData(extra_data_i);
                while (node_i == 0 and extra_data_i > sub_range.start) {
                    extra_data_i -= 1;
                    node_i = tree.getExtraData(extra_data_i);
                }
                break :blk if (node_i == 0) tree.getMainToken(i) else tree.getLastToken(node_i);
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .semicolon => {},
                    .comment => {},
                    else => break,
                }
            }
            return last_token;
        },
        .implicit_return => return tree.getLastToken(tree.getData(i).lhs),
        .@"return" => unreachable,

        .abs,
        .acos,
        .asin,
        .atan,
        .avg,
        .cos,
        .dev,
        .enlist,
        .exit,
        .exp,
        .getenv,
        .hopen,
        .last,
        .log,
        .max,
        .min,
        .prd,
        .sin,
        .sqrt,
        .sum,
        .tan,
        .@"var",

        .bin,
        .binr,
        .cor,
        .cov,
        .div,
        .in,
        .insert,
        .like,
        .setenv,
        .ss,
        .wavg,
        .within,
        .wsum,
        .xexp,
        => return tree.getMainToken(i),

        .do_one,
        .if_one,
        .while_one,
        => {
            const data = tree.getData(i);
            var last_token = (if (data.rhs > 0) blk: {
                break :blk tree.getLastToken(data.rhs);
            } else blk: {
                break :blk tree.getLastToken(data.lhs);
            }) + 1;
            while (true) : (last_token += 1) {
                switch (tree.tokens.items(.tag)[last_token]) {
                    .comment => {},
                    .semicolon => {},
                    else => break,
                }
            }
            return last_token;
        },

        .select => {
            const data = tree.getData(i);
            const select_node = tree.extraData(data.rhs, Node.Select);

            if (select_node.by > select_node.where) {
                const node = tree.getExtraData(select_node.by - 1);
                return tree.getLastToken(node);
            }

            return tree.getLastToken(select_node.from);
        },
        .exec => {
            const data = tree.getData(i);
            const exec_node = tree.extraData(data.rhs, Node.Exec);

            if (exec_node.by > exec_node.where) {
                const node = tree.getExtraData(exec_node.by - 1);
                return tree.getLastToken(node);
            }

            return tree.getLastToken(exec_node.from);
        },
        .update => {
            const data = tree.getData(i);
            const update_node = tree.extraData(data.rhs, Node.Update);

            if (update_node.by > update_node.where) {
                const node = tree.getExtraData(update_node.by - 1);
                return tree.getLastToken(node);
            }

            return tree.getLastToken(update_node.from);
        },
        .delete_rows => {
            const data = tree.getData(i);
            const delete_node = tree.extraData(data.rhs, Node.DeleteRows);

            if (delete_node.where_end > delete_node.where) {
                const node = tree.getExtraData(delete_node.where_end - 1);
                return tree.getLastToken(node);
            }

            return tree.getLastToken(delete_node.from);
        },
        .delete_cols => {
            const data = tree.getData(i);
            const delete_node = tree.extraData(data.rhs, Node.DeleteColumns);
            return tree.getLastToken(delete_node.from);
        },
    }
}

pub fn print(tree: Ast, i: Node.Index, stream: anytype, gpa: Allocator) Allocator.Error!void {
    switch (tree.getTag(i)) {
        .root => unreachable,
        .grouped_expression,
        .implicit_return,
        => try tree.print(tree.getData(i).lhs, stream, gpa),
        .empty_list => try stream.writeAll("()"),
        .list => {
            const data = tree.getData(i);
            try stream.writeAll("(enlist;");
            const sub_range = tree.extraData(data.lhs, Node.SubRange);
            for (sub_range.start..sub_range.end) |extra_data_i| {
                const node_i = tree.getExtraData(extra_data_i);
                if (node_i == 0) {
                    try stream.writeAll("::");
                } else {
                    try tree.print(node_i, stream, gpa);
                }
                if (extra_data_i < sub_range.end - 1) {
                    try stream.writeAll(";");
                }
            }
            try stream.writeAll(")");
        },
        .table_literal => {
            const data = tree.getData(i);
            const table = tree.extraData(data.lhs, Node.Table);

            var keys = std.ArrayList(u8).init(gpa);
            defer keys.deinit();
            try keys.append(',');
            if (table.len == 1) {
                try keys.writer().print(",`{s}", .{tree.table_columns[table.column_start]});
            } else {
                for (table.column_start..table.column_start + table.len) |table_columns_i| {
                    const table_column = tree.table_columns[table_columns_i];
                    try keys.writer().print("`{s}", .{table_column});
                }
            }

            var values = std.ArrayList(u8).init(gpa);
            defer values.deinit();
            for (table.expr_start..table.expr_start + table.len) |extra_data_i| {
                const node_i = tree.getExtraData(extra_data_i);
                try tree.print(node_i, values.writer(), gpa);
                if (extra_data_i < (table.expr_start + table.len) - 1) {
                    try values.append(';');
                }
            }

            try stream.print("(+:;(!;{s};(enlist;{s})))", .{ keys.items, values.items });
        },
        .number_literal => {
            var index = i;
            while (true) {
                try stream.writeAll(tree.getSource(index));
                index = tree.getData(index).rhs;
                if (index == 0) break;
                try stream.writeAll(" ");
            }
        },
        .string_literal => try stream.writeAll(tree.getSource(i)),
        .symbol_literal,
        .symbol_list_literal,
        => try stream.print(",{s}", .{tree.getSource(i)}),
        .identifier => try stream.print("`{s}", .{tree.getSource(i)}),
        .assign,
        .global_assign,
        .add,
        .plus_assign,
        .subtract,
        .minus_assign,
        .multiply,
        .asterisk_assign,
        .divide,
        .percent_assign,
        .dict,
        .bang_assign,
        .lesser,
        .ampersand_assign,
        .greater,
        .pipe_assign,
        .less_than,
        .angle_bracket_left_assign,
        .less_than_equal,
        .not_equal,
        .greater_than,
        .angle_bracket_right_assign,
        .greater_than_equal,
        .equals,
        .equal_assign,
        .match,
        .tilde_assign,
        .join,
        .comma_assign,
        .fill,
        .caret_assign,
        .take,
        .hash_assign,
        .drop,
        .underscore_assign,
        .cast,
        .dollar_assign,
        .find,
        .question_mark_assign,
        .apply,
        .at_assign,
        .apply_n,
        .dot_assign,
        .file_text,
        .zero_colon_assign,
        .file_binary,
        .one_colon_assign,
        .dynamic_load,

        .apostrophe_infix,
        .apostrophe_colon_infix,
        .slash_infix,
        .slash_colon_infix,
        .backslash_infix,
        .backslash_colon_infix,

        .bin_infix,
        .binr_infix,
        .cor_infix,
        .cov_infix,
        .div_infix,
        .in_infix,
        .insert_infix,
        .like_infix,
        .setenv_infix,
        .ss_infix,
        .wavg_infix,
        .within_infix,
        .wsum_infix,
        .xexp_infix,
        => {
            const data = tree.getData(i);
            if (data.rhs == 0) {
                try stream.writeAll("NYI");
            } else {
                const symbol = tree.getTokenTag(i).symbol();

                var rhs = std.ArrayList(u8).init(gpa);
                defer rhs.deinit();
                try tree.print(data.rhs, rhs.writer(), gpa);

                var lhs = std.ArrayList(u8).init(gpa);
                defer lhs.deinit();
                try tree.print(data.lhs, lhs.writer(), gpa);

                try stream.print("({s};{s};{s})", .{ symbol, lhs.items, rhs.items });
            }
        },
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
        .bang,
        .bang_colon,
        .ampersand,
        .ampersand_colon,
        .pipe,
        .pipe_colon,
        .angle_bracket_left,
        .angle_bracket_left_colon,
        .angle_bracket_left_equal,
        .angle_bracket_left_right,
        .angle_bracket_right,
        .angle_bracket_right_colon,
        .angle_bracket_right_equal,
        .equal,
        .equal_colon,
        .tilde,
        .tilde_colon,
        .comma,
        .comma_colon,
        .caret,
        .caret_colon,
        .hash,
        .hash_colon,
        .underscore,
        .underscore_colon,
        .dollar,
        .dollar_colon,
        .question_mark,
        .question_mark_colon,
        .at,
        .at_colon,
        .dot,
        .dot_colon,
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
        => try stream.writeAll(tree.getTokenTag(i).symbol()),
        .implicit_apply => {
            const data = tree.getData(i);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            try tree.print(data.rhs, rhs.writer(), gpa);

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
        },
        .lambda_one,
        .lambda_one_semicolon,
        .lambda,
        .lambda_semicolon,
        => {
            const start_token = tree.getMainToken(i);
            const start = tree.tokens.items(.loc)[start_token].start;

            const last_token = tree.getLastToken(i);
            const end = tree.tokens.items(.loc)[last_token].end;

            const source = tree.source[start..end];
            try stream.print("{s}", .{source});
        },
        .block => {
            const data = tree.getData(i);

            if (data.rhs - data.lhs > 1) {
                try stream.writeAll("(\";\";");
            }

            for (data.lhs..data.rhs) |extra_data_i| {
                const node_i = tree.getExtraData(extra_data_i);
                if (node_i == 0) {
                    try stream.writeAll("::");
                } else {
                    try tree.print(node_i, stream, gpa);
                }
                if (extra_data_i < data.rhs - 1) {
                    try stream.writeAll(";");
                }
            }

            if (data.rhs - data.lhs > 1) {
                try stream.writeAll(")");
            }
        },
        .call_one => {
            const data = tree.getData(i);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            if (data.rhs == 0) {
                try rhs.appendSlice("::");
            } else {
                try tree.print(data.rhs, rhs.writer(), gpa);
            }

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
        },
        .call => {
            const data = tree.getData(i);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            for (sub_range.start..sub_range.end) |extra_data_i| {
                const node_i = tree.getExtraData(extra_data_i);
                if (node_i == 0) {
                    try rhs.appendSlice("::");
                } else {
                    try tree.print(node_i, rhs.writer(), gpa);
                }
                if (extra_data_i < sub_range.end - 1) {
                    try rhs.append(';');
                }
            }

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
        },
        .@"return" => {
            const data = tree.getData(i);

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            try stream.print("(\":\";{s})", .{lhs.items});
        },

        .abs,
        .acos,
        .asin,
        .atan,
        .avg,
        .cos,
        .dev,
        .enlist,
        .exit,
        .exp,
        .getenv,
        .hopen,
        .last,
        .log,
        .max,
        .min,
        .prd,
        .sin,
        .sqrt,
        .sum,
        .tan,
        .@"var",

        .bin,
        .binr,
        .cor,
        .cov,
        .div,
        .in,
        .insert,
        .like,
        .setenv,
        .ss,
        .wavg,
        .within,
        .wsum,
        .xexp,
        => try stream.writeAll(tree.getSource(i)),

        .do_one,
        .if_one,
        .while_one,
        => {
            const data = tree.getData(i);

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            if (data.rhs == 0) {
                try rhs.appendSlice("::");
            } else {
                try tree.print(data.rhs, rhs.writer(), gpa);
            }

            try stream.print("(`{s};{s};{s})", .{ tree.getSource(i), lhs.items, rhs.items });
        },
        .do,
        .@"if",
        .@"while",
        => {
            const data = tree.getData(i);

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            try tree.print(data.lhs, lhs.writer(), gpa);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            const sub_range = tree.extraData(data.rhs, Node.SubRange);
            for (sub_range.start..sub_range.end) |extra_data_i| {
                const node_i = tree.getExtraData(extra_data_i);
                if (node_i == 0) {
                    try rhs.appendSlice("::");
                } else {
                    try tree.print(node_i, rhs.writer(), gpa);
                }
                if (extra_data_i < sub_range.end - 1) {
                    try rhs.append(';');
                }
            }

            try stream.print("(`{s};{s};{s})", .{ tree.getSource(i), lhs.items, rhs.items });
        },

        .select => {
            const data = tree.getData(i);
            const select_node = tree.extraData(data.rhs, Node.Select);

            var from = std.ArrayList(u8).init(gpa);
            defer from.deinit();
            try tree.print(select_node.from, from.writer(), gpa);

            var where = std.ArrayList(u8).init(gpa);
            defer where.deinit();
            const where_slice = tree.extra_data[select_node.where..select_node.by];
            if (where_slice.len == 0) {
                try where.appendSlice("()");
            } else {
                var temp = std.ArrayList(u8).init(gpa);
                defer temp.deinit();
                try where.append(',');
                if (where_slice.len > 1) try temp.append('(');
                for (where_slice, 0..) |where_i, temp_i| {
                    try tree.print(where_i, temp.writer(), gpa);
                    if (temp_i < where_slice.len - 1) try temp.append(';');
                }
                if (where_slice.len > 1) try temp.append(')');
                if (temp.items[0] != '(' or temp.items[1] == ')') try where.append(',');
                try where.appendSlice(temp.items);
            }

            var by = std.ArrayList(u8).init(gpa);
            defer by.deinit();
            if (select_node.data.has_by) {
                const by_slice = tree.extra_data[select_node.by..select_node.select];
                if (by_slice.len == 0) {
                    try by.appendSlice("()!()");
                } else {
                    try tree.printColumns(select_node.by_columns, by_slice.len, by.writer());
                    try by.append('!');
                    try tree.printList(by_slice, by.writer(), gpa);
                }
            } else if (select_node.data.distinct) {
                try by.appendSlice("1b");
            } else {
                try by.appendSlice("0b");
            }

            var select = std.ArrayList(u8).init(gpa);
            defer select.deinit();
            const select_slice = tree.extra_data[select_node.select..select_node.select_end];
            if (select_slice.len == 0) {
                try select.appendSlice("()");
            } else {
                try tree.printColumns(select_node.select_columns, select_slice.len, select.writer());
                try select.append('!');
                try tree.printList(select_slice, select.writer(), gpa);
            }

            if (select_node.order > 0) {
                var limit = std.ArrayList(u8).init(gpa);
                defer limit.deinit();
                if (select_node.limit > 0) {
                    try tree.print(select_node.limit, limit.writer(), gpa);
                } else {
                    try limit.appendSlice("0W");
                }

                var order = std.ArrayList(u8).init(gpa);
                defer order.deinit();
                const c: u8 = if (select_node.data.ascending) '<' else '>';
                const loc = tree.tokens.items(.loc)[select_node.order];
                try order.writer().print(",({c}:;`{s})", .{ c, tree.source[loc.start..loc.end] });

                try stream.print("(?;{s};{s};{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items, limit.items, order.items });
            } else if (select_node.limit > 0) {
                var limit = std.ArrayList(u8).init(gpa);
                defer limit.deinit();
                try tree.print(select_node.limit, limit.writer(), gpa);

                try stream.print("(?;{s};{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items, limit.items });
            } else {
                try stream.print("(?;{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items });
            }
        },
        .exec => {
            const data = tree.getData(i);
            const exec_node = tree.extraData(data.rhs, Node.Exec);

            var from = std.ArrayList(u8).init(gpa);
            defer from.deinit();
            try tree.print(exec_node.from, from.writer(), gpa);

            var where = std.ArrayList(u8).init(gpa);
            defer where.deinit();
            const where_slice = tree.extra_data[exec_node.where..exec_node.by];
            if (where_slice.len == 0) {
                try where.appendSlice("()");
            } else {
                var temp = std.ArrayList(u8).init(gpa);
                defer temp.deinit();
                try where.append(',');
                if (where_slice.len > 1) try temp.append('(');
                for (where_slice, 0..) |where_i, temp_i| {
                    try tree.print(where_i, temp.writer(), gpa);
                    if (temp_i < where_slice.len - 1) try temp.append(';');
                }
                if (where_slice.len > 1) try temp.append(')');
                if (temp.items[0] != '(' or temp.items[1] == ')') try where.append(',');
                try where.appendSlice(temp.items);
            }

            var by = std.ArrayList(u8).init(gpa);
            defer by.deinit();
            const by_slice = tree.extra_data[exec_node.by..exec_node.select];
            if (by_slice.len == 0) {
                try by.appendSlice("()");
            } else if (exec_node.data.has_by_columns) {
                try tree.printColumns(exec_node.by_columns, by_slice.len, by.writer());
                try by.append('!');
                try tree.printList(by_slice, by.writer(), gpa);
            } else {
                try tree.printList(by_slice, by.writer(), gpa);
            }

            var select = std.ArrayList(u8).init(gpa);
            defer select.deinit();
            const select_slice = tree.extra_data[exec_node.select..exec_node.select_end];
            if (select_slice.len == 0) {
                try select.appendSlice("()");
            } else if (exec_node.data.has_select_columns) {
                try tree.printColumns(exec_node.select_columns, select_slice.len, select.writer());
                try select.append('!');
                try tree.printList(select_slice, select.writer(), gpa);
            } else {
                try tree.printList(select_slice, select.writer(), gpa);
            }

            try stream.print("(?;{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items });
        },
        .update => {
            const data = tree.getData(i);
            const update_node = tree.extraData(data.rhs, Node.Update);

            var from = std.ArrayList(u8).init(gpa);
            defer from.deinit();
            try tree.print(update_node.from, from.writer(), gpa);

            var where = std.ArrayList(u8).init(gpa);
            defer where.deinit();
            const where_slice = tree.extra_data[update_node.where..update_node.by];
            if (where_slice.len == 0) {
                try where.appendSlice("()");
            } else {
                var temp = std.ArrayList(u8).init(gpa);
                defer temp.deinit();
                try where.append(',');
                if (where_slice.len > 1) try temp.append('(');
                for (where_slice, 0..) |where_i, temp_i| {
                    try tree.print(where_i, temp.writer(), gpa);
                    if (temp_i < where_slice.len - 1) try temp.append(';');
                }
                if (where_slice.len > 1) try temp.append(')');
                if (temp.items[0] != '(' or temp.items[1] == ')') try where.append(',');
                try where.appendSlice(temp.items);
            }

            var by = std.ArrayList(u8).init(gpa);
            defer by.deinit();
            const by_slice = tree.extra_data[update_node.by..update_node.select];
            if (by_slice.len == 0) {
                try by.appendSlice("0b");
            } else {
                try tree.printColumns(update_node.by_columns, by_slice.len, by.writer());
                try by.append('!');
                try tree.printList(by_slice, by.writer(), gpa);
            }

            var select = std.ArrayList(u8).init(gpa);
            defer select.deinit();
            const select_slice = tree.extra_data[update_node.select..update_node.select_end];
            if (select_slice.len == 0) {
                try select.appendSlice("()");
            } else {
                try tree.printColumns(update_node.select_columns, select_slice.len, select.writer());
                try select.append('!');
                try tree.printList(select_slice, select.writer(), gpa);
            }

            try stream.print("(!;{s};{s};{s};{s})", .{ from.items, where.items, by.items, select.items });
        },
        .delete_rows,
        .delete_cols,
        => unreachable,
    }
}

fn printColumns(tree: Ast, start: Node.Index, len: usize, stream: anytype) Allocator.Error!void {
    const columns = tree.table_columns[start .. start + len];
    assert(columns.len > 0);
    if (columns.len == 1) {
        try stream.print("(,`{s})", .{columns[0]});
    } else {
        for (columns) |column| {
            try stream.print("`{s}", .{column});
        }
    }
}

fn printList(tree: Ast, list: []Node.Index, stream: anytype, gpa: Allocator) Allocator.Error!void {
    switch (list.len) {
        0 => {
            try stream.writeAll("()");
        },
        1 => {
            try stream.writeAll(",");
            for (list) |i| {
                try tree.print(i, stream, gpa);
            }
        },
        else => {
            var hash_set = std.AutoHashMap(Node.Tag, void).init(gpa);
            defer hash_set.deinit();

            try hash_set.put(tree.getTag(list[0]), {});
            for (list[1..]) |i| {
                const tag = tree.getTag(i);
                const result = try hash_set.getOrPut(tag);
                if (!result.found_existing) break;
            } else {
                var iter = hash_set.keyIterator();
                const tag = iter.next().?.*;
                switch (tag) {
                    .identifier => {
                        for (list) |i| {
                            try tree.print(i, stream, gpa);
                        }
                        return;
                    },
                    else => {},
                }
            }

            try stream.writeAll("(");
            for (list, 0..) |i, temp_i| {
                try tree.print(i, stream, gpa);
                if (temp_i < list.len - 1) {
                    try stream.writeAll(";");
                }
            }
            try stream.writeAll(")");
        },
    }
}

pub fn visit(tree: Ast, gpa: Allocator) !void {
    const data = tree.getData(0);
    for (data.lhs..data.rhs) |extra_data_i| {
        var list = std.ArrayList(u8).init(gpa);
        defer list.deinit();
        try tree.print(tree.getExtraData(extra_data_i), list.writer(), gpa);
        log.debug("{s}", .{list.items});
    }
}

const std = @import("std");
const kdb = @import("../kdb.zig");
const Token = kdb.Token;
const Tokenizer = kdb.Tokenizer;
const Ast = @This();
const Allocator = std.mem.Allocator;
const Parse = @import("Parse.zig");
const panic = std.debug.panic;
const assert = std.debug.assert;
const diagnostics = @import("../features/diagnostics.zig");

const log = std.log.scoped(.kdbLint_Ast);

fn testLastToken(source: [:0]const u8) !void {
    inline for (&.{.v4_0}) |version| {
        inline for (&.{ .k, .q }) |language| {
            try testLastTokenSettings(.{
                .version = version,
                .language = language,
            }, source);
        }
    }
}

fn testLastTokenVersion(version: Ast.Version, source: [:0]const u8) !void {
    inline for (&.{ .k, .q }) |language| {
        try testLastTokenSettings(.{
            .version = version,
            .language = language,
        }, source);
    }
}

fn testLastTokenLanguage(language: Ast.Language, source: [:0]const u8) !void {
    inline for (&.{.v4_0}) |version| {
        try testLastTokenSettings(.{
            .version = version,
            .language = language,
        }, source);
    }
}

fn testLastTokenSettings(settings: Ast.ParseSettings, source: [:0]const u8) !void {
    var tree = try parse(std.testing.allocator, source, settings);
    defer tree.deinit(std.testing.allocator);

    const data = tree.nodes.items(.data)[0];
    const i = tree.extra_data[data.lhs];

    var actual = std.ArrayList(u8).init(std.testing.allocator);
    defer actual.deinit();
    try tree.print(i, actual.writer(), std.testing.allocator);
    try std.testing.expectEqualSlices(u8, source[0..source.len], actual.items);
}

test "getLastToken" {
    try testLastToken("{f[]}");
    try testLastToken("{f[];}");
    try testLastToken("{f[1]}");
    try testLastToken("{f[1];}");
    try testLastToken("{f[;]}");
    try testLastToken("{f[;];}");
    try testLastToken("{f[1;]}");
    try testLastToken("{f[1;];}");
    try testLastToken("{f[;2]}");
    try testLastToken("{f[;2];}");
    try testLastToken("{f[1;2]}");
    try testLastToken("{f[1;2];}");

    try testLastToken("{do[0;]}");
    try testLastToken("{do[0;];}");
    try testLastToken("{do[0;1]}");
    try testLastToken("{do[0;1];}");
    try testLastToken("{do[0;1;]}");
    try testLastToken("{do[0;1;];}");
    try testLastToken("{do[0;1;2]}");
    try testLastToken("{do[0;1;2];}");

    try testLastToken("{if[0;]}");
    try testLastToken("{if[0;];}");
    try testLastToken("{if[0;1]}");
    try testLastToken("{if[0;1];}");
    try testLastToken("{if[0;1;]}");
    try testLastToken("{if[0;1;];}");
    try testLastToken("{if[0;1;2]}");
    try testLastToken("{if[0;1;2];}");

    try testLastToken("{while[0;]}");
    try testLastToken("{while[0;];}");
    try testLastToken("{while[0;1]}");
    try testLastToken("{while[0;1];}");
    try testLastToken("{while[0;1;]}");
    try testLastToken("{while[0;1;];}");
    try testLastToken("{while[0;1;2]}");
    try testLastToken("{while[0;1;2];}");

    try testLastToken("{[]}");
    try testLastToken("{[x]}");
    try testLastToken("{[x;y]}");
    try testLastToken("{[x;y;z]}");

    try testLastToken("{[][]}");
    try testLastToken("{[][0]}");
    try testLastToken("{[][0;]}");
    try testLastToken("{[][;1]}");
    try testLastToken("{[][0;1]}");
    try testLastToken("{[][0;1;]}");
    try testLastToken("{[][0;1;2]}");

    try testLastToken("{[]()}");
    try testLastToken("{[](0)}");
    try testLastToken("{[](0;)}");
    try testLastToken("{[](;1)}");
    try testLastToken("{[](0;1)}");
    try testLastToken("{[](0;1;)}");
    try testLastToken("{[](0;1;2)}");
}

test {
    @import("std").testing.refAllDecls(@This());
}
