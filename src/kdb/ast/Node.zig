const TokenIndex = @import("../Ast.zig").TokenIndex;

tag: Tag,
main_token: TokenIndex,
data: Data,

pub const Index = u32;

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

    /// `Value[lhs]`. rhs is unused.
    boolean_literal,
    /// `Value[lhs]`. rhs is unused.
    boolean_list_literal,
    /// `Value[lhs]`. rhs is unused.
    guid_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    guid_list_literal,
    /// `Value[lhs]`. rhs is unused.
    byte_literal,
    /// `Value[lhs]`. rhs is unused.
    byte_list_literal,
    /// `Value[lhs]`. rhs is unused.
    short_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    short_list_literal,
    /// `Value[lhs]`. rhs is unused.
    int_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    int_list_literal,
    /// `Value[lhs]`. rhs is unused.
    long_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    long_list_literal,
    /// `Value[lhs]`. rhs is unused.
    real_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    real_list_literal,
    /// `Value[lhs]`. rhs is unused.
    float_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    float_list_literal,
    /// `Value[lhs]`. rhs is unused.
    char_number_literal,
    /// `Value[lhs]. main_token is the first token index; rhs is the last token index.
    char_number_list_literal,
    /// `Value[lhs]`. rhs is unused.
    char_literal,
    /// `Value[lhs]`. rhs is unused.
    char_list_literal,
    /// `Value[lhs]`. rhs is unused.
    symbol_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    symbol_list_literal,
    /// `Value[lhs]`. rhs is unused.
    timestamp_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    timestamp_list_literal,
    /// `Value[lhs]`. rhs is unused.
    month_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    month_list_literal,
    /// `Value[lhs]`. rhs is unused.
    date_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    date_list_literal,
    /// `Value[lhs]`. rhs is unused.
    datetime_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    datetime_list_literal,
    /// `Value[lhs]`. rhs is unused.
    timespan_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    timespan_list_literal,
    /// `Value[lhs]`. rhs is unused.
    minute_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    minute_list_literal,
    /// `Value[lhs]`. rhs is unused.
    second_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    second_list_literal,
    /// `Value[lhs]`. rhs is unused.
    time_literal,
    /// `Value[lhs]`. main_token is the first token index; rhs is the last token index.
    time_list_literal,
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

    /// `lhs : rhs`. lhs and rhs can be omitted. main_token is `:`.
    assign,
    /// `lhs :: rhs`. lhs and rhs can be omitted. main_token is `::`.
    global_assign,
    /// `lhs + rhs`. lhs and rhs can be omitted. main_token is `+`.
    add,
    /// `lhs +: rhs`. lhs and rhs can be omitted. main_token is `+:`.
    plus_assign,
    /// `lhs - rhs`. lhs and rhs can be omitted. main_token is `-`.
    subtract,
    /// `lhs -: rhs`. lhs and rhs can be omitted. main_token is `-:`.
    minus_assign,
    /// `lhs * rhs`. lhs and rhs can be omitted. main_token is `*`.
    multiply,
    /// `lhs *: rhs`. lhs and rhs can be omitted. main_token is `*:`.
    asterisk_assign,
    /// `lhs % rhs`. lhs and rhs can be omitted. main_token is `%`.
    divide,
    /// `lhs %: rhs`. lhs and rhs can be omitted. main_token is `%:`.
    percent_assign,
    /// `lhs ! rhs`. lhs and rhs can be omitted. main_token is `!`.
    dict,
    /// `lhs !: rhs`. lhs and rhs can be omitted. main_token is `!:`.
    bang_assign,
    /// `lhs & rhs`. lhs and rhs can be omitted. main_token is `&`.
    lesser,
    /// `lhs &: rhs`. lhs and rhs can be omitted. main_token is `&:`.
    ampersand_assign,
    /// `lhs | rhs`. lhs and rhs can be omitted. main_token is `|`.
    greater,
    /// `lhs |: rhs`. lhs and rhs can be omitted. main_token is `|:`.
    pipe_assign,
    /// `lhs < rhs`. lhs and rhs can be omitted. main_token is `<`.
    less_than,
    /// `lhs <: rhs`. lhs and rhs can be omitted. main_token is `<:`.
    angle_bracket_left_assign,
    /// `lhs <= rhs`. lhs and rhs can be omitted. main_token is `<=`.
    less_than_equal,
    /// `lhs <> rhs`. lhs and rhs can be omitted. main_token is `<>`.
    not_equal,
    /// `lhs > rhs`. lhs and rhs can be omitted. main_token is `>`.
    greater_than,
    /// `lhs >: rhs`. lhs and rhs can be omitted. main_token is `>:`.
    angle_bracket_right_assign,
    /// `lhs >= rhs`. lhs and rhs can be omitted. main_token is `>=`.
    greater_than_equal,
    /// `lhs = rhs`. lhs and rhs can be omitted. main_token is `=`.
    equals,
    /// `lhs =: rhs`. lhs and rhs can be omitted. main_token is `=:`.
    equal_assign,
    /// `lhs ~ rhs`. lhs and rhs can be omitted. main_token is `~`.
    match,
    /// `lhs ~: rhs`. lhs and rhs can be omitted. main_token is `~:`.
    tilde_assign,
    /// `lhs , rhs`. lhs and rhs can be omitted. main_token is `,`.
    join,
    /// `lhs ,: rhs`. lhs and rhs can be omitted. main_token is `,:`.
    comma_assign,
    /// `lhs ^ rhs`. lhs and rhs can be omitted. main_token is `^`.
    fill,
    /// `lhs ^: rhs`. lhs and rhs can be omitted. main_token is `^:`.
    caret_assign,
    /// `lhs # rhs`. lhs and rhs can be omitted. main_token is `#`.
    take,
    /// `lhs #: rhs`. lhs and rhs can be omitted. main_token is `#:`.
    hash_assign,
    /// `lhs _ rhs`. lhs and rhs can be omitted. main_token is `_`.
    drop,
    /// `lhs _: rhs`. lhs and rhs can be omitted. main_token is `_:`.
    underscore_assign,
    /// `lhs $ rhs`. lhs and rhs can be omitted. main_token is `$`.
    cast,
    /// `lhs $: rhs`. lhs and rhs can be omitted. main_token is `$:`.
    dollar_assign,
    /// `lhs ? rhs`. lhs and rhs can be omitted. main_token is `?`.
    find,
    /// `lhs ?: rhs`. lhs and rhs can be omitted. main_token is `?:`.
    question_mark_assign,
    /// `lhs @ rhs`. lhs and rhs can be omitted. main_token is `@`.
    apply,
    /// `lhs @: rhs`. lhs and rhs can be omitted. main_token is `@:`.
    at_assign,
    /// `lhs . rhs`. lhs and rhs can be omitted. main_token is `.`.
    apply_n,
    /// `lhs .: rhs`. lhs and rhs can be omitted. main_token is `.:`.
    dot_assign,
    /// `lhs 0: rhs`. lhs and rhs can be omitted. main_token is `0:`.
    file_text,
    /// `lhs 0:: rhs`. lhs and rhs can be omitted. main_token is `0::`.
    zero_colon_assign,
    /// `lhs 1: rhs`. lhs and rhs can be omitted. main_token is `1:`.
    file_binary,
    /// `lhs 1:: rhs`. lhs and rhs can be omitted. main_token is `1::`.
    one_colon_assign,
    /// `lhs 2: rhs`. lhs and rhs can be omitted. main_token is `2:`.
    dynamic_load,

    /// `lhs rhs`. main_token is unused.
    implicit_apply,

    /// `lhs'`. lhs can be omitted. rhs unused. main_token is `'`.
    apostrophe,
    /// `lhs':`. lhs can be omitted. rhs unused. main_token is `':`.
    apostrophe_colon,
    /// `lhs/`. lhs can be omitted. rhs unused. main_token is `/`.
    slash,
    /// `lhs/:`. lhs can be omitted. rhs unused. main_token is `/:`.
    slash_colon,
    /// `lhs\`. lhs can be omitted. rhs unused. main_token is `\`.
    backslash,
    /// `lhs\:`. lhs can be omitted. rhs unused. main_token is `\:`.
    backslash_colon,

    /// `x lhs' y`. `Iterator[rhs]`. y can be omitted. main_token is `'`.
    apostrophe_infix,
    /// `x lhs': y`. `Iterator[rhs]`. y can be omitted. main_token is `':`.
    apostrophe_colon_infix,
    /// `x lhs/ y`. `Iterator[rhs]`. y can be omitted. main_token is `/`.
    slash_infix,
    /// `x lhs/: y`. `Iterator[rhs]`. y can be omitted. main_token is `/:`.
    slash_colon_infix,
    /// `x lhs\ y`. `Iterator[rhs]`. y can be omitted. main_token is `\`.
    backslash_infix,
    /// `x lhs\: y`. `Iterator[rhs]`. y can be omitted. main_token is `\:`.
    backslash_colon_infix,

    /// `{lhs}`. `Lambda[lhs]`. main_token is `{`. rhs is token index of `}`.
    lambda,
    /// Same as lambda but there is known to be a semicolon before the rbrace.
    lambda_semicolon,

    /// `[a;b;c]`. `SubRange[lhs]`. main_token is `[`. rhs is the token index of the `]`.
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
    aj,
    /// Both lhs and rhs unused.
    aj0,
    /// Both lhs and rhs unused.
    ajf,
    /// Both lhs and rhs unused.
    ajf0,
    /// Both lhs and rhs unused.
    all,
    /// Both lhs and rhs unused.
    any,
    /// Both lhs and rhs unused.
    asc,
    /// Both lhs and rhs unused.
    asin,
    /// Both lhs and rhs unused.
    atan,
    /// Both lhs and rhs unused.
    attr,
    /// Both lhs and rhs unused.
    avg,
    /// Both lhs and rhs unused.
    avgs,
    /// Both lhs and rhs unused.
    ceiling,
    /// Both lhs and rhs unused.
    cols,
    /// Both lhs and rhs unused.
    cos,
    /// Both lhs and rhs unused.
    count,
    /// Both lhs and rhs unused.
    csv,
    /// Both lhs and rhs unused.
    deltas,
    /// Both lhs and rhs unused.
    desc,
    /// Both lhs and rhs unused.
    dev,
    /// Both lhs and rhs unused.
    differ,
    /// Both lhs and rhs unused.
    distinct,
    /// Both lhs and rhs unused.
    ej,
    /// Both lhs and rhs unused.
    enlist,
    /// Both lhs and rhs unused.
    eval,
    /// Both lhs and rhs unused.
    exit,
    /// Both lhs and rhs unused.
    exp,
    /// Both lhs and rhs unused.
    fills,
    /// Both lhs and rhs unused.
    first,
    /// Both lhs and rhs unused.
    fkeys,
    /// Both lhs and rhs unused.
    flip,
    /// Both lhs and rhs unused.
    floor,
    /// Both lhs and rhs unused.
    get,
    /// Both lhs and rhs unused.
    getenv,
    /// Both lhs and rhs unused.
    group,
    /// Both lhs and rhs unused.
    gtime,
    /// Both lhs and rhs unused.
    hclose,
    /// Both lhs and rhs unused.
    hcount,
    /// Both lhs and rhs unused.
    hdel,
    /// Both lhs and rhs unused.
    hopen,
    /// Both lhs and rhs unused.
    hsym,
    /// Both lhs and rhs unused.
    iasc,
    /// Both lhs and rhs unused.
    idesc,
    /// Both lhs and rhs unused.
    inv,
    /// Both lhs and rhs unused.
    key,
    /// Both lhs and rhs unused.
    keys,
    /// Both lhs and rhs unused.
    last,
    /// Both lhs and rhs unused.
    load,
    /// Both lhs and rhs unused.
    log,
    /// Both lhs and rhs unused.
    lower,
    /// Both lhs and rhs unused.
    ltime,
    /// Both lhs and rhs unused.
    ltrim,
    /// Both lhs and rhs unused.
    max,
    /// Both lhs and rhs unused.
    maxs,
    /// Both lhs and rhs unused.
    md5,
    /// Both lhs and rhs unused.
    med,
    /// Both lhs and rhs unused.
    meta,
    /// Both lhs and rhs unused.
    min,
    /// Both lhs and rhs unused.
    mins,
    /// Both lhs and rhs unused.
    neg,
    /// Both lhs and rhs unused.
    next,
    /// Both lhs and rhs unused.
    not,
    /// Both lhs and rhs unused.
    null,
    /// Both lhs and rhs unused.
    parse,
    /// Both lhs and rhs unused.
    prd,
    /// Both lhs and rhs unused.
    prds,
    /// Both lhs and rhs unused.
    prev,
    /// Both lhs and rhs unused.
    rand,
    /// Both lhs and rhs unused.
    rank,
    /// Both lhs and rhs unused.
    ratios,
    /// Both lhs and rhs unused.
    raze,
    /// Both lhs and rhs unused.
    read0,
    /// Both lhs and rhs unused.
    read1,
    /// Both lhs and rhs unused.
    reciprocal,
    /// Both lhs and rhs unused.
    reval,
    /// Both lhs and rhs unused.
    reverse,
    /// Both lhs and rhs unused.
    rload,
    /// Both lhs and rhs unused.
    rsave,
    /// Both lhs and rhs unused.
    rtrim,
    /// Both lhs and rhs unused.
    save,
    /// Both lhs and rhs unused.
    sdev,
    /// Both lhs and rhs unused.
    show,
    /// Both lhs and rhs unused.
    signum,
    /// Both lhs and rhs unused.
    sin,
    /// Both lhs and rhs unused.
    sqrt,
    /// Both lhs and rhs unused.
    ssr,
    /// Both lhs and rhs unused.
    string,
    /// Both lhs and rhs unused.
    sum,
    /// Both lhs and rhs unused.
    sums,
    /// Both lhs and rhs unused.
    svar,
    /// Both lhs and rhs unused.
    system,
    /// Both lhs and rhs unused.
    tables,
    /// Both lhs and rhs unused.
    tan,
    /// Both lhs and rhs unused.
    til,
    /// Both lhs and rhs unused.
    trim,
    /// Both lhs and rhs unused.
    type,
    /// Both lhs and rhs unused.
    ungroup,
    /// Both lhs and rhs unused.
    @"union",
    /// Both lhs and rhs unused.
    upper,
    /// Both lhs and rhs unused.
    value,
    /// Both lhs and rhs unused.
    @"var",
    /// Both lhs and rhs unused.
    view,
    /// Both lhs and rhs unused.
    views,
    /// Both lhs and rhs unused.
    where,
    /// Both lhs and rhs unused.
    wj,
    /// Both lhs and rhs unused.
    wj1,
    /// Both lhs and rhs unused.
    ww,

    /// Both lhs and rhs unused.
    @"and",
    /// Both lhs and rhs unused.
    asof,
    /// Both lhs and rhs unused.
    bin,
    /// Both lhs and rhs unused.
    binr,
    /// Both lhs and rhs unused.
    cor,
    /// Both lhs and rhs unused.
    cov,
    /// Both lhs and rhs unused.
    cross,
    /// Both lhs and rhs unused.
    cut,
    /// Both lhs and rhs unused.
    div,
    /// Both lhs and rhs unused.
    dsave,
    /// Both lhs and rhs unused.
    each,
    /// Both lhs and rhs unused.
    ema,
    /// Both lhs and rhs unused.
    except,
    /// Both lhs and rhs unused.
    fby,
    /// Both lhs and rhs unused.
    ij,
    /// Both lhs and rhs unused.
    ijf,
    /// Both lhs and rhs unused.
    in,
    /// Both lhs and rhs unused.
    insert,
    /// Both lhs and rhs unused.
    inter,
    /// Both lhs and rhs unused.
    like,
    /// Both lhs and rhs unused.
    lj,
    /// Both lhs and rhs unused.
    ljf,
    /// Both lhs and rhs unused.
    lsq,
    /// Both lhs and rhs unused.
    mavg,
    /// Both lhs and rhs unused.
    mcount,
    /// Both lhs and rhs unused.
    mdev,
    /// Both lhs and rhs unused.
    mmax,
    /// Both lhs and rhs unused.
    mmin,
    /// Both lhs and rhs unused.
    mmu,
    /// Both lhs and rhs unused.
    mod,
    /// Both lhs and rhs unused.
    msum,
    /// Both lhs and rhs unused.
    @"or",
    /// Both lhs and rhs unused.
    over,
    /// Both lhs and rhs unused.
    peach,
    /// Both lhs and rhs unused.
    pj,
    /// Both lhs and rhs unused.
    prior,
    /// Both lhs and rhs unused.
    rotate,
    /// Both lhs and rhs unused.
    scan,
    /// Both lhs and rhs unused.
    scov,
    /// Both lhs and rhs unused.
    set,
    /// Both lhs and rhs unused.
    setenv,
    /// Both lhs and rhs unused.
    ss,
    /// Both lhs and rhs unused.
    sublist,
    /// Both lhs and rhs unused.
    sv,
    /// Both lhs and rhs unused.
    uj,
    /// Both lhs and rhs unused.
    ujf,
    /// Both lhs and rhs unused.
    upsert,
    /// Both lhs and rhs unused.
    vs,
    /// Both lhs and rhs unused.
    wavg,
    /// Both lhs and rhs unused.
    within,
    /// Both lhs and rhs unused.
    wsum,
    /// Both lhs and rhs unused.
    xasc,
    /// Both lhs and rhs unused.
    xbar,
    /// Both lhs and rhs unused.
    xcol,
    /// Both lhs and rhs unused.
    xcols,
    /// Both lhs and rhs unused.
    xdesc,
    /// Both lhs and rhs unused.
    xexp,
    /// Both lhs and rhs unused.
    xgroup,
    /// Both lhs and rhs unused.
    xkey,
    /// Both lhs and rhs unused.
    xlog,
    /// Both lhs and rhs unused.
    xprev,
    /// Both lhs and rhs unused.
    xrank,
    /// `lhs and rhs`. rhs can be omitted. main_token is `and`.
    and_infix,
    /// `lhs asof rhs`. rhs can be omitted. main_token is `asof`.
    asof_infix,
    /// `lhs bin rhs`. rhs can be omitted. main_token is `bin`.
    bin_infix,
    /// `lhs binr rhs`. rhs can be omitted. main_token is `binr`.
    binr_infix,
    /// `lhs cor rhs`. rhs can be omitted. main_token is `cor`.
    cor_infix,
    /// `lhs cov rhs`. rhs can be omitted. main_token is `cov`.
    cov_infix,
    /// `lhs cross rhs`. rhs can be omitted. main_token is `cross`.
    cross_infix,
    /// `lhs cut rhs`. rhs can be omitted. main_token is `cut`.
    cut_infix,
    /// `lhs div rhs`. rhs can be omitted. main_token is `div`.
    div_infix,
    /// `lhs dsave rhs`. rhs can be omitted. main_token is `dsave`.
    dsave_infix,
    /// `lhs each rhs`. rhs can be omitted. main_token is `each`.
    each_infix,
    /// `lhs ema rhs`. rhs can be omitted. main_token is `ema`.
    ema_infix,
    /// `lhs except rhs`. rhs can be omitted. main_token is `except`.
    except_infix,
    /// `lhs fby rhs`. rhs can be omitted. main_token is `fby`.
    fby_infix,
    /// `lhs ij rhs`. rhs can be omitted. main_token is `ij`.
    ij_infix,
    /// `lhs ijf rhs`. rhs can be omitted. main_token is `ijf`.
    ijf_infix,
    /// `lhs in rhs`. rhs can be omitted. main_token is `in`.
    in_infix,
    /// `lhs insert rhs`. rhs can be omitted. main_token is `insert`.
    insert_infix,
    /// `lhs inter rhs`. rhs can be omitted. main_token is `inter`.
    inter_infix,
    /// `lhs like rhs`. rhs can be omitted. main_token is `like`.
    like_infix,
    /// `lhs lj rhs`. rhs can be omitted. main_token is `lj`.
    lj_infix,
    /// `lhs ljf rhs`. rhs can be omitted. main_token is `ljf`.
    ljf_infix,
    /// `lhs lsq rhs`. rhs can be omitted. main_token is `lsq`.
    lsq_infix,
    /// `lhs mavg rhs`. rhs can be omitted. main_token is `mavg`.
    mavg_infix,
    /// `lhs mcount rhs`. rhs can be omitted. main_token is `mcount`.
    mcount_infix,
    /// `lhs mdev rhs`. rhs can be omitted. main_token is `mdev`.
    mdev_infix,
    /// `lhs mmax rhs`. rhs can be omitted. main_token is `mmax`.
    mmax_infix,
    /// `lhs mmin rhs`. rhs can be omitted. main_token is `mmin`.
    mmin_infix,
    /// `lhs mmu rhs`. rhs can be omitted. main_token is `mmu`.
    mmu_infix,
    /// `lhs mod rhs`. rhs can be omitted. main_token is `mod`.
    mod_infix,
    /// `lhs msum rhs`. rhs can be omitted. main_token is `msum`.
    msum_infix,
    /// `lhs or rhs`. rhs can be omitted. main_token is `or`.
    or_infix,
    /// `lhs over rhs`. rhs can be omitted. main_token is `over`.
    over_infix,
    /// `lhs peach rhs`. rhs can be omitted. main_token is `peach`.
    peach_infix,
    /// `lhs pj rhs`. rhs can be omitted. main_token is `pj`.
    pj_infix,
    /// `lhs prior rhs`. rhs can be omitted. main_token is `prior`.
    prior_infix,
    /// `lhs rotate rhs`. rhs can be omitted. main_token is `rotate`.
    rotate_infix,
    /// `lhs scan rhs`. rhs can be omitted. main_token is `scan`.
    scan_infix,
    /// `lhs scov rhs`. rhs can be omitted. main_token is `scov`.
    scov_infix,
    /// `lhs set rhs`. rhs can be omitted. main_token is `set`.
    set_infix,
    /// `lhs setenv rhs`. rhs can be omitted. main_token is `setenv`.
    setenv_infix,
    /// `lhs ss rhs`. rhs can be omitted. main_token is `ss`.
    ss_infix,
    /// `lhs sublist rhs`. rhs can be omitted. main_token is `sublist`.
    sublist_infix,
    /// `lhs sv rhs`. rhs can be omitted. main_token is `sv`.
    sv_infix,
    /// `lhs uj rhs`. rhs can be omitted. main_token is `uj`.
    uj_infix,
    /// `lhs ujf rhs`. rhs can be omitted. main_token is `ujf`.
    ujf_infix,
    /// `lhs upsert rhs`. rhs can be omitted. main_token is `upsert`.
    upsert_infix,
    /// `lhs vs rhs`. rhs can be omitted. main_token is `vs`.
    vs_infix,
    /// `lhs wavg rhs`. rhs can be omitted. main_token is `wavg`.
    wavg_infix,
    /// `lhs within rhs`. rhs can be omitted. main_token is `within`.
    within_infix,
    /// `lhs wsum rhs`. rhs can be omitted. main_token is `wsum`.
    wsum_infix,
    /// `lhs xasc rhs`. rhs can be omitted. main_token is `xasc`.
    xasc_infix,
    /// `lhs xbar rhs`. rhs can be omitted. main_token is `xbar`.
    xbar_infix,
    /// `lhs xcol rhs`. rhs can be omitted. main_token is `xcol`.
    xcol_infix,
    /// `lhs xcols rhs`. rhs can be omitted. main_token is `xcols`.
    xcols_infix,
    /// `lhs xdesc rhs`. rhs can be omitted. main_token is `xdesc`.
    xdesc_infix,
    /// `lhs xexp rhs`. rhs can be omitted. main_token is `xexp`.
    xexp_infix,
    /// `lhs xgroup rhs`. rhs can be omitted. main_token is `xgroup`.
    xgroup_infix,
    /// `lhs xkey rhs`. rhs can be omitted. main_token is `xkey`.
    xkey_infix,
    /// `lhs xlog rhs`. rhs can be omitted. main_token is `xlog`.
    xlog_infix,
    /// `lhs xprev rhs`. rhs can be omitted. main_token is `xprev`.
    xprev_infix,
    /// `lhs xrank rhs`. rhs can be omitted. main_token is `xrank`.
    xrank_infix,

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

    /// `select lhs`. `Select[lhs]`. rhs is unused. main_token is `select`.
    select,
    /// `exec lhs`. `Exec[lhs]`. rhs is unused. main_token is `exec`.
    exec,
    /// `update lhs`. `Update[lhs]`. rhs is unused. main_token is `update`.
    update,
    /// `delete lhs`. `DeleteRows[lhs]`. rhs is unused. main_token is `delete`.
    delete_rows,
    /// `delete lhs`. `DeleteCols[lhs]`. rhs is unused. main_token is `delete`.
    delete_cols,

    /// `\os lhs`. `SubRange[lhs]`. lhs can be omitted. rhs is unused. main_token is `\os`.
    os,
    /// `\cd`. lhs is unused. rhs is unused. main_token is `\cd`.
    current_directory,
    /// `\cd lhs`. rhs is unused. main_token is `\cd`.
    change_directory,
    /// `\l lhs rhs`. rhs can be omitted. main_token is `\l`.
    load_file_or_directory,
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

pub const Lambda = struct {
    /// Index into extra_data.
    params_start: Index,
    /// Index into extra_data.
    params_end: Index,
    /// Index into extra_data.
    body_start: Index,
    /// Index into extra_data.
    body_end: Index,
};

pub const Iterator = struct {
    lhs: Index,
    rhs: Index,
};

pub const Table = struct {
    /// Index into table_columns.
    column_start: Index,
    /// Index into extra_data.
    expr_start: Index,
    key_len: u32,
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
