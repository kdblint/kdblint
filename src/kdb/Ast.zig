//! Abstract Syntax Tree for kdb+ source code.

/// Reference to externally-owned data.
source: [:0]const u8,

tokens: TokenList.Slice,
/// The root AST ndoe is assumed to be index 0. Since there can be no
/// references to the root node, this means 0 is available to indicate null.
nodes: NodeList.Slice,
extra_data: []Node.Index,
mode: Mode = .q,

errors: []const Error,

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
    gpa.free(tree.errors);
    tree.* = undefined;
}

pub const RenderError = error{
    /// Ran out of memory allocating call stack frames to complete rendering, or
    /// ran out of memory allocating space in the output buffer.
    OutOfMemory,
};

pub const Mode = enum { k, q };

/// Result should be freed with tree.deinit() when there are
/// no more references to any of the tokens or nodes.
pub fn parse(gpa: Allocator, source: [:0]const u8, mode: Mode) Allocator.Error!Ast {
    var tokens = Ast.TokenList{};
    defer tokens.deinit(gpa);

    // Empirically, the zig std lib has an 8:1 ratio of source bytes to token count.
    const estimated_token_count = source.len / 8;
    try tokens.ensureTotalCapacity(gpa, estimated_token_count);

    var tokenizer = Tokenizer.init(source, mode);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(gpa, token);
        if (token.tag == .eof) break;
    }

    var parser: Parse = .{
        .source = source,
        .gpa = gpa,
        .token_tags = tokens.items(.tag),
        .token_locs = tokens.items(.loc),
        .token_eobs = tokens.items(.eob),
        .errors = .{},
        .nodes = .{},
        .extra_data = .{},
        .scratch = .{},
        .tok_i = 0,
    };
    defer parser.errors.deinit(gpa);
    defer parser.nodes.deinit(gpa);
    defer parser.extra_data.deinit(gpa);
    defer parser.scratch.deinit(gpa);

    // Empirically, Zig source code has a 2:1 ratio of tokens to AST nodes.
    // Make sure at least 1 so we can use appendAssumeCapacity on the root node below.
    const estimated_node_count = (tokens.len + 2) / 2;
    try parser.nodes.ensureTotalCapacity(gpa, estimated_node_count);

    switch (mode) {
        .q => try parser.parseRoot(),
        .k => unreachable,
    }

    // TODO experiment with compacting the MultiArrayList slices here
    return Ast{
        .source = source,
        .mode = mode,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra_data.toOwnedSlice(gpa),
        .errors = try parser.errors.toOwnedSlice(gpa),
    };
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
    } = .{ .none = {} },

    pub const Tag = enum {
        asterisk_after_ptr_deref,
        chained_comparison_operators,
        decl_between_fields,
        expected_block,
        expected_block_or_assignment,
        expected_block_or_expr,
        expected_block_or_field,
        expected_container_members,
        expected_expr,
        expected_expr_or_assignment,
        expected_expr_or_var_decl,
        expected_fn,
        expected_inlinable,
        expected_labelable,
        expected_param_list,
        expected_prefix_expr,
        expected_infix_expr,
        expected_primary_type_expr,
        expected_pub_item,
        expected_return_type,
        expected_semi_or_else,
        expected_semi_or_lbrace,
        expected_statement,
        expected_suffix_op,
        expected_type_expr,
        expected_var_decl,
        expected_var_decl_or_fn,
        expected_loop_payload,
        expected_container,
        extern_fn_body,
        extra_addrspace_qualifier,
        extra_align_qualifier,
        extra_allowzero_qualifier,
        extra_const_qualifier,
        extra_volatile_qualifier,
        ptr_mod_on_array_child_type,
        invalid_bit_range,
        same_line_doc_comment,
        unattached_doc_comment,
        test_doc_comment,
        comptime_doc_comment,
        varargs_nonfinal,
        expected_continue_expr,
        expected_semi_after_decl,
        expected_semi_after_stmt,
        expected_comma_after_field,
        expected_comma_after_arg,
        expected_comma_after_param,
        expected_comma_after_initializer,
        expected_comma_after_switch_prong,
        expected_comma_after_for_operand,
        expected_comma_after_capture,
        expected_initializer,
        mismatched_binary_op_whitespace,
        invalid_ampersand_ampersand,
        c_style_container,
        expected_var_const,
        wrong_equal_var_decl,
        var_const_decl,
        extra_for_capture,
        for_input_not_captured,

        zig_style_container,
        previous_field,
        next_field,

        /// `expected_tag` is populated.
        expected_token,
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
        /// sub_list[lhs...rhs]
        root,

        /// `(lhs)`. main_token is the `(`; rhs is the token index of the `)`.
        grouped_expression,

        /// Both lhs and rhs unused.
        number_literal,
        /// Both lhs and rhs unused.
        string_literal,
        /// Both lhs and rhs unused.
        symbol_literal,
        /// Both lhs and rhs unused.
        symbol_list_literal,
        /// Both lhs and rhs unused.
        /// Most identifiers will not have explicit AST nodes, however for expressions
        /// which could be one of many different kinds of AST nodes, there will be an
        /// identifier AST node for it.
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
    };

    // TODO: Remove
    pub const ZTag = enum {
        /// sub_list[lhs...rhs]
        root,
        /// `usingnamespace lhs;`. rhs unused. main_token is `usingnamespace`.
        @"usingnamespace",
        /// lhs is test name token (must be string literal or identifier), if any.
        /// rhs is the body node.
        test_decl,
        /// lhs is the index into extra_data.
        /// rhs is the initialization expression, if any.
        /// main_token is `var` or `const`.
        global_var_decl,
        /// `var a: x align(y) = rhs`
        /// lhs is the index into extra_data.
        /// main_token is `var` or `const`.
        local_var_decl,
        /// `var a: lhs = rhs`. lhs and rhs may be unused.
        /// Can be local or global.
        /// main_token is `var` or `const`.
        simple_var_decl,
        /// `var a align(lhs) = rhs`. lhs and rhs may be unused.
        /// Can be local or global.
        /// main_token is `var` or `const`.
        aligned_var_decl,
        /// lhs is the identifier token payload if any,
        /// rhs is the deferred expression.
        @"errdefer",
        /// lhs is unused.
        /// rhs is the deferred expression.
        @"defer",
        /// lhs catch rhs
        /// lhs catch |err| rhs
        /// main_token is the `catch` keyword.
        /// payload is determined by looking at the next token after the `catch` keyword.
        @"catch",
        /// `lhs.a`. main_token is the dot. rhs is the identifier token index.
        field_access,
        /// `lhs.?`. main_token is the dot. rhs is the `?` token index.
        unwrap_optional,
        /// `lhs == rhs`. main_token is op.
        equal_equal,
        /// `lhs != rhs`. main_token is op.
        bang_equal,
        /// `lhs < rhs`. main_token is op.
        less_than,
        /// `lhs > rhs`. main_token is op.
        greater_than,
        /// `lhs <= rhs`. main_token is op.
        less_or_equal,
        /// `lhs >= rhs`. main_token is op.
        greater_or_equal,
        /// `lhs *= rhs`. main_token is op.
        assign_mul,
        /// `lhs /= rhs`. main_token is op.
        assign_div,
        /// `lhs %= rhs`. main_token is op.
        assign_mod,
        /// `lhs += rhs`. main_token is op.
        assign_add,
        /// `lhs -= rhs`. main_token is op.
        assign_sub,
        /// `lhs <<= rhs`. main_token is op.
        assign_shl,
        /// `lhs <<|= rhs`. main_token is op.
        assign_shl_sat,
        /// `lhs >>= rhs`. main_token is op.
        assign_shr,
        /// `lhs &= rhs`. main_token is op.
        assign_bit_and,
        /// `lhs ^= rhs`. main_token is op.
        assign_bit_xor,
        /// `lhs |= rhs`. main_token is op.
        assign_bit_or,
        /// `lhs *%= rhs`. main_token is op.
        assign_mul_wrap,
        /// `lhs +%= rhs`. main_token is op.
        assign_add_wrap,
        /// `lhs -%= rhs`. main_token is op.
        assign_sub_wrap,
        /// `lhs *|= rhs`. main_token is op.
        assign_mul_sat,
        /// `lhs +|= rhs`. main_token is op.
        assign_add_sat,
        /// `lhs -|= rhs`. main_token is op.
        assign_sub_sat,
        /// `lhs = rhs`. main_token is op.
        assign,
        /// `a, b, ... = rhs`. main_token is op. lhs is index into `extra_data`
        /// of an lhs elem count followed by an array of that many `Node.Index`,
        /// with each node having one of the following types:
        /// * `global_var_decl`
        /// * `local_var_decl`
        /// * `simple_var_decl`
        /// * `aligned_var_decl`
        /// * Any expression node
        /// The first 3 types correspond to a `var` or `const` lhs node (note
        /// that their `rhs` is always 0). An expression node corresponds to a
        /// standard assignment LHS (which must be evaluated as an lvalue).
        /// There may be a preceding `comptime` token, which does not create a
        /// corresponding `comptime` node so must be manually detected.
        assign_destructure,
        /// `lhs || rhs`. main_token is the `||`.
        merge_error_sets,
        /// `lhs * rhs`. main_token is the `*`.
        mul,
        /// `lhs / rhs`. main_token is the `/`.
        div,
        /// `lhs % rhs`. main_token is the `%`.
        mod,
        /// `lhs ** rhs`. main_token is the `**`.
        array_mult,
        /// `lhs *% rhs`. main_token is the `*%`.
        mul_wrap,
        /// `lhs *| rhs`. main_token is the `*|`.
        mul_sat,
        /// `lhs + rhs`. main_token is the `+`.
        add,
        /// `lhs - rhs`. main_token is the `-`.
        sub,
        /// `lhs ++ rhs`. main_token is the `++`.
        array_cat,
        /// `lhs +% rhs`. main_token is the `+%`.
        add_wrap,
        /// `lhs -% rhs`. main_token is the `-%`.
        sub_wrap,
        /// `lhs +| rhs`. main_token is the `+|`.
        add_sat,
        /// `lhs -| rhs`. main_token is the `-|`.
        sub_sat,
        /// `lhs << rhs`. main_token is the `<<`.
        shl,
        /// `lhs <<| rhs`. main_token is the `<<|`.
        shl_sat,
        /// `lhs >> rhs`. main_token is the `>>`.
        shr,
        /// `lhs & rhs`. main_token is the `&`.
        bit_and,
        /// `lhs ^ rhs`. main_token is the `^`.
        bit_xor,
        /// `lhs | rhs`. main_token is the `|`.
        bit_or,
        /// `lhs orelse rhs`. main_token is the `orelse`.
        @"orelse",
        /// `lhs and rhs`. main_token is the `and`.
        bool_and,
        /// `lhs or rhs`. main_token is the `or`.
        bool_or,
        /// `op lhs`. rhs unused. main_token is op.
        bool_not,
        /// `op lhs`. rhs unused. main_token is op.
        negation,
        /// `op lhs`. rhs unused. main_token is op.
        bit_not,
        /// `op lhs`. rhs unused. main_token is op.
        negation_wrap,
        /// `op lhs`. rhs unused. main_token is op.
        address_of,
        /// `op lhs`. rhs unused. main_token is op.
        @"try",
        /// `op lhs`. rhs unused. main_token is op.
        @"await",
        /// `?lhs`. rhs unused. main_token is the `?`.
        optional_type,
        /// `[lhs]rhs`.
        array_type,
        /// `[lhs:a]b`. `ArrayTypeSentinel[rhs]`.
        array_type_sentinel,
        /// `[*]align(lhs) rhs`. lhs can be omitted.
        /// `*align(lhs) rhs`. lhs can be omitted.
        /// `[]rhs`.
        /// main_token is the asterisk if a pointer or the lbracket if a slice
        /// main_token might be a ** token, which is shared with a parent/child
        /// pointer type and may require special handling.
        ptr_type_aligned,
        /// `[*:lhs]rhs`. lhs can be omitted.
        /// `*rhs`.
        /// `[:lhs]rhs`.
        /// main_token is the asterisk if a pointer or the lbracket if a slice
        /// main_token might be a ** token, which is shared with a parent/child
        /// pointer type and may require special handling.
        ptr_type_sentinel,
        /// lhs is index into ptr_type. rhs is the element type expression.
        /// main_token is the asterisk if a pointer or the lbracket if a slice
        /// main_token might be a ** token, which is shared with a parent/child
        /// pointer type and may require special handling.
        ptr_type,
        /// lhs is index into ptr_type_bit_range. rhs is the element type expression.
        /// main_token is the asterisk if a pointer or the lbracket if a slice
        /// main_token might be a ** token, which is shared with a parent/child
        /// pointer type and may require special handling.
        ptr_type_bit_range,
        /// `lhs[rhs..]`
        /// main_token is the lbracket.
        slice_open,
        /// `lhs[b..c]`. rhs is index into Slice
        /// main_token is the lbracket.
        slice,
        /// `lhs[b..c :d]`. rhs is index into SliceSentinel. Slice end "c" can be omitted.
        /// main_token is the lbracket.
        slice_sentinel,
        /// `lhs.*`. rhs is unused.
        deref,
        /// `lhs[rhs]`.
        array_access,
        /// `lhs{rhs}`. rhs can be omitted.
        array_init_one,
        /// `lhs{rhs,}`. rhs can *not* be omitted
        array_init_one_comma,
        /// `.{lhs, rhs}`. lhs and rhs can be omitted.
        array_init_dot_two,
        /// Same as `array_init_dot_two` except there is known to be a trailing comma
        /// before the final rbrace.
        array_init_dot_two_comma,
        /// `.{a, b}`. `sub_list[lhs..rhs]`.
        array_init_dot,
        /// Same as `array_init_dot` except there is known to be a trailing comma
        /// before the final rbrace.
        array_init_dot_comma,
        /// `lhs{a, b}`. `sub_range_list[rhs]`. lhs can be omitted which means `.{a, b}`.
        array_init,
        /// Same as `array_init` except there is known to be a trailing comma
        /// before the final rbrace.
        array_init_comma,
        /// `lhs{.a = rhs}`. rhs can be omitted making it empty.
        /// main_token is the lbrace.
        struct_init_one,
        /// `lhs{.a = rhs,}`. rhs can *not* be omitted.
        /// main_token is the lbrace.
        struct_init_one_comma,
        /// `.{.a = lhs, .b = rhs}`. lhs and rhs can be omitted.
        /// main_token is the lbrace.
        /// No trailing comma before the rbrace.
        struct_init_dot_two,
        /// Same as `struct_init_dot_two` except there is known to be a trailing comma
        /// before the final rbrace.
        struct_init_dot_two_comma,
        /// `.{.a = b, .c = d}`. `sub_list[lhs..rhs]`.
        /// main_token is the lbrace.
        struct_init_dot,
        /// Same as `struct_init_dot` except there is known to be a trailing comma
        /// before the final rbrace.
        struct_init_dot_comma,
        /// `lhs{.a = b, .c = d}`. `sub_range_list[rhs]`.
        /// lhs can be omitted which means `.{.a = b, .c = d}`.
        /// main_token is the lbrace.
        struct_init,
        /// Same as `struct_init` except there is known to be a trailing comma
        /// before the final rbrace.
        struct_init_comma,
        /// `lhs(rhs)`. rhs can be omitted.
        /// main_token is the lparen.
        call_one,
        /// `lhs(rhs,)`. rhs can be omitted.
        /// main_token is the lparen.
        call_one_comma,
        /// `async lhs(rhs)`. rhs can be omitted.
        async_call_one,
        /// `async lhs(rhs,)`.
        async_call_one_comma,
        /// `lhs(a, b, c)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        call,
        /// `lhs(a, b, c,)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        call_comma,
        /// `async lhs(a, b, c)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        async_call,
        /// `async lhs(a, b, c,)`. `SubRange[rhs]`.
        /// main_token is the `(`.
        async_call_comma,
        /// `switch(lhs) {}`. `SubRange[rhs]`.
        @"switch",
        /// Same as switch except there is known to be a trailing comma
        /// before the final rbrace
        switch_comma,
        /// `lhs => rhs`. If lhs is omitted it means `else`.
        /// main_token is the `=>`
        switch_case_one,
        /// Same ast `switch_case_one` but the case is inline
        switch_case_inline_one,
        /// `a, b, c => rhs`. `SubRange[lhs]`.
        /// main_token is the `=>`
        switch_case,
        /// Same ast `switch_case` but the case is inline
        switch_case_inline,
        /// `lhs...rhs`.
        switch_range,
        /// `while (lhs) rhs`.
        /// `while (lhs) |x| rhs`.
        while_simple,
        /// `while (lhs) : (a) b`. `WhileCont[rhs]`.
        /// `while (lhs) : (a) b`. `WhileCont[rhs]`.
        while_cont,
        /// `while (lhs) : (a) b else c`. `While[rhs]`.
        /// `while (lhs) |x| : (a) b else c`. `While[rhs]`.
        /// `while (lhs) |x| : (a) b else |y| c`. `While[rhs]`.
        /// The cont expression part `: (a)` may be omitted.
        @"while",
        /// `for (lhs) rhs`.
        for_simple,
        /// `for (lhs[0..inputs]) lhs[inputs + 1] else lhs[inputs + 2]`. `For[rhs]`.
        @"for",
        /// `lhs..rhs`. rhs can be omitted.
        for_range,
        /// `if (lhs) rhs`.
        /// `if (lhs) |a| rhs`.
        if_simple,
        /// `if (lhs) a else b`. `If[rhs]`.
        /// `if (lhs) |x| a else b`. `If[rhs]`.
        /// `if (lhs) |x| a else |y| b`. `If[rhs]`.
        @"if",
        /// `suspend lhs`. lhs can be omitted. rhs is unused.
        @"suspend",
        /// `resume lhs`. rhs is unused.
        @"resume",
        /// `continue`. lhs is token index of label if any. rhs is unused.
        @"continue",
        /// `break :lhs rhs`
        /// both lhs and rhs may be omitted.
        @"break",
        /// `return lhs`. lhs can be omitted. rhs is unused.
        @"return",
        /// `fn (a: lhs) rhs`. lhs can be omitted.
        /// anytype and ... parameters are omitted from the AST tree.
        /// main_token is the `fn` keyword.
        /// extern function declarations use this tag.
        fn_proto_simple,
        /// `fn (a: b, c: d) rhs`. `sub_range_list[lhs]`.
        /// anytype and ... parameters are omitted from the AST tree.
        /// main_token is the `fn` keyword.
        /// extern function declarations use this tag.
        fn_proto_multi,
        /// `fn (a: b) addrspace(e) linksection(f) callconv(g) rhs`. `FnProtoOne[lhs]`.
        /// zero or one parameters.
        /// anytype and ... parameters are omitted from the AST tree.
        /// main_token is the `fn` keyword.
        /// extern function declarations use this tag.
        fn_proto_one,
        /// `fn (a: b, c: d) addrspace(e) linksection(f) callconv(g) rhs`. `FnProto[lhs]`.
        /// anytype and ... parameters are omitted from the AST tree.
        /// main_token is the `fn` keyword.
        /// extern function declarations use this tag.
        fn_proto,
        /// lhs is the fn_proto.
        /// rhs is the function body block.
        /// Note that extern function declarations use the fn_proto tags rather
        /// than this one.
        fn_decl,
        /// `anyframe->rhs`. main_token is `anyframe`. `lhs` is arrow token index.
        anyframe_type,
        /// Both lhs and rhs unused.
        anyframe_literal,
        /// Both lhs and rhs unused.
        char_literal,
        /// Both lhs and rhs unused.
        number_literal,
        /// Both lhs and rhs unused.
        unreachable_literal,
        /// Both lhs and rhs unused.
        /// Most identifiers will not have explicit AST nodes, however for expressions
        /// which could be one of many different kinds of AST nodes, there will be an
        /// identifier AST node for it.
        identifier,
        /// lhs is the dot token index, rhs unused, main_token is the identifier.
        enum_literal,
        /// main_token is the string literal token
        /// Both lhs and rhs unused.
        string_literal,
        /// main_token is the first token index (redundant with lhs)
        /// lhs is the first token index; rhs is the last token index.
        /// Could be a series of multiline_string_literal_line tokens, or a single
        /// string_literal token.
        multiline_string_literal,
        /// `(lhs)`. main_token is the `(`; rhs is the token index of the `)`.
        grouped_expression,
        /// `@a(lhs, rhs)`. lhs and rhs may be omitted.
        /// main_token is the builtin token.
        builtin_call_two,
        /// Same as builtin_call_two but there is known to be a trailing comma before the rparen.
        builtin_call_two_comma,
        /// `@a(b, c)`. `sub_list[lhs..rhs]`.
        /// main_token is the builtin token.
        builtin_call,
        /// Same as builtin_call but there is known to be a trailing comma before the rparen.
        builtin_call_comma,
        /// `error{a, b}`.
        /// rhs is the rbrace, lhs is unused.
        error_set_decl,
        /// `struct {}`, `union {}`, `opaque {}`, `enum {}`. `extra_data[lhs..rhs]`.
        /// main_token is `struct`, `union`, `opaque`, `enum` keyword.
        container_decl,
        /// Same as ContainerDecl but there is known to be a trailing comma
        /// or semicolon before the rbrace.
        container_decl_trailing,
        /// `struct {lhs, rhs}`, `union {lhs, rhs}`, `opaque {lhs, rhs}`, `enum {lhs, rhs}`.
        /// lhs or rhs can be omitted.
        /// main_token is `struct`, `union`, `opaque`, `enum` keyword.
        container_decl_two,
        /// Same as ContainerDeclTwo except there is known to be a trailing comma
        /// or semicolon before the rbrace.
        container_decl_two_trailing,
        /// `struct(lhs)` / `union(lhs)` / `enum(lhs)`. `SubRange[rhs]`.
        container_decl_arg,
        /// Same as container_decl_arg but there is known to be a trailing
        /// comma or semicolon before the rbrace.
        container_decl_arg_trailing,
        /// `union(enum) {}`. `sub_list[lhs..rhs]`.
        /// Note that tagged unions with explicitly provided enums are represented
        /// by `container_decl_arg`.
        tagged_union,
        /// Same as tagged_union but there is known to be a trailing comma
        /// or semicolon before the rbrace.
        tagged_union_trailing,
        /// `union(enum) {lhs, rhs}`. lhs or rhs may be omitted.
        /// Note that tagged unions with explicitly provided enums are represented
        /// by `container_decl_arg`.
        tagged_union_two,
        /// Same as tagged_union_two but there is known to be a trailing comma
        /// or semicolon before the rbrace.
        tagged_union_two_trailing,
        /// `union(enum(lhs)) {}`. `SubRange[rhs]`.
        tagged_union_enum_tag,
        /// Same as tagged_union_enum_tag but there is known to be a trailing comma
        /// or semicolon before the rbrace.
        tagged_union_enum_tag_trailing,
        /// `a: lhs = rhs,`. lhs and rhs can be omitted.
        /// main_token is the field name identifier.
        /// lastToken() does not include the possible trailing comma.
        container_field_init,
        /// `a: lhs align(rhs),`. rhs can be omitted.
        /// main_token is the field name identifier.
        /// lastToken() does not include the possible trailing comma.
        container_field_align,
        /// `a: lhs align(c) = d,`. `container_field_list[rhs]`.
        /// main_token is the field name identifier.
        /// lastToken() does not include the possible trailing comma.
        container_field,
        /// `comptime lhs`. rhs unused.
        @"comptime",
        /// `nosuspend lhs`. rhs unused.
        @"nosuspend",
        /// `{lhs rhs}`. rhs or lhs can be omitted.
        /// main_token points at the lbrace.
        block_two,
        /// Same as block_two but there is known to be a semicolon before the rbrace.
        block_two_semicolon,
        /// `{}`. `sub_list[lhs..rhs]`.
        /// main_token points at the lbrace.
        block,
        /// Same as block but there is known to be a semicolon before the rbrace.
        block_semicolon,
        /// `asm(lhs)`. rhs is the token index of the rparen.
        asm_simple,
        /// `asm(lhs, a)`. `Asm[rhs]`.
        @"asm",
        /// `[a] "b" (c)`. lhs is 0, rhs is token index of the rparen.
        /// `[a] "b" (-> lhs)`. rhs is token index of the rparen.
        /// main_token is `a`.
        asm_output,
        /// `[a] "b" (lhs)`. rhs is token index of the rparen.
        /// main_token is `a`.
        asm_input,
        /// `error.a`. lhs is token index of `.`. rhs is token index of `a`.
        error_value,
        /// `lhs!rhs`. main_token is the `!`.
        error_union,

        pub fn isContainerField(tag: Tag) bool {
            return switch (tag) {
                .container_field_init,
                .container_field_align,
                .container_field,
                => true,

                else => false,
            };
        }
    };

    pub const Data = struct {
        lhs: Index,
        rhs: Index,
    };

    pub const SubRange = struct {
        /// Index into sub_list.
        start: Index,
        /// Index into sub_list.
        end: Index,
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

pub fn print(tree: Ast, i: Node.Index, stream: anytype, gpa: Allocator) !void {
    switch (tree.getTag(i)) {
        .root => unreachable,
        .grouped_expression => try tree.print(tree.getData(i).lhs, stream, gpa),
        .number_literal,
        .string_literal,
        => try stream.writeAll(tree.getSource(i)),
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
        => try stream.writeAll(tree.getTokenTag(i).symbol()),
        .implicit_apply => {
            const data = tree.getData(i);

            var rhs = std.ArrayList(u8).init(gpa);
            defer rhs.deinit();
            if (data.rhs > 0) {
                try tree.print(data.rhs, rhs.writer(), gpa);
            }

            var lhs = std.ArrayList(u8).init(gpa);
            defer lhs.deinit();
            if (data.lhs > 0) {
                try tree.print(data.lhs, lhs.writer(), gpa);
            }

            try stream.print("({s};{s})", .{ lhs.items, rhs.items });
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

const log = std.log.scoped(.kdbLint_Ast);

test {
    @import("std").testing.refAllDecls(@This());
}
