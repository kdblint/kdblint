const std = @import("std");
const builtin = @import("builtin");
const Ast = @import("Ast.zig");
const Language = Ast.Language;

const log = std.log.scoped(.kdblint_tokenizer);

pub const Token = struct {
    tag: Tag,
    loc: Loc,
    eob: bool,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
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

        .{ "aj", .keyword_q_aj },
        .{ "aj0", .keyword_q_aj0 },
        .{ "ajf", .keyword_q_ajf },
        .{ "ajf0", .keyword_q_ajf0 },
        .{ "all", .keyword_q_all },
        .{ "and", .keyword_q_and },
        .{ "any", .keyword_q_any },
        .{ "asc", .keyword_q_asc },
        .{ "asof", .keyword_q_asof },
        .{ "attr", .keyword_q_attr },
        .{ "avgs", .keyword_q_avgs },
        .{ "ceiling", .keyword_q_ceiling },
        .{ "cols", .keyword_q_cols },
        .{ "count", .keyword_q_count },
        .{ "cross", .keyword_q_cross },
        .{ "csv", .keyword_q_csv },
        .{ "cut", .keyword_q_cut },
        .{ "deltas", .keyword_q_deltas },
        .{ "desc", .keyword_q_desc },
        .{ "differ", .keyword_q_differ },
        .{ "distinct", .keyword_q_distinct },
        .{ "dsave", .keyword_q_dsave },
        .{ "each", .keyword_q_each },
        .{ "ej", .keyword_q_ej },
        .{ "ema", .keyword_q_ema },
        .{ "eval", .keyword_q_eval },
        .{ "except", .keyword_q_except },
        .{ "fby", .keyword_q_fby },
        .{ "fills", .keyword_q_fills },
        .{ "first", .keyword_q_first },
        .{ "fkeys", .keyword_q_fkeys },
        .{ "flip", .keyword_q_flip },
        .{ "floor", .keyword_q_floor },
        .{ "get", .keyword_q_get },
        .{ "group", .keyword_q_group },
        .{ "gtime", .keyword_q_gtime },
        .{ "hclose", .keyword_q_hclose },
        .{ "hcount", .keyword_q_hcount },
        .{ "hdel", .keyword_q_hdel },
        .{ "hsym", .keyword_q_hsym },
        .{ "iasc", .keyword_q_iasc },
        .{ "idesc", .keyword_q_idesc },
        .{ "ij", .keyword_q_ij },
        .{ "ijf", .keyword_q_ijf },
        .{ "inter", .keyword_q_inter },
        .{ "inv", .keyword_q_inv },
        .{ "key", .keyword_q_key },
        .{ "keys", .keyword_q_keys },
        .{ "lj", .keyword_q_lj },
        .{ "ljf", .keyword_q_ljf },
        .{ "load", .keyword_q_load },
        .{ "lower", .keyword_q_lower },
        .{ "lsq", .keyword_q_lsq },
        .{ "ltime", .keyword_q_ltime },
        .{ "ltrim", .keyword_q_ltrim },
        .{ "mavg", .keyword_q_mavg },
        .{ "maxs", .keyword_q_maxs },
        .{ "mcount", .keyword_q_mcount },
        .{ "md5", .keyword_q_md5 },
        .{ "mdev", .keyword_q_mdev },
        .{ "med", .keyword_q_med },
        .{ "meta", .keyword_q_meta },
        .{ "mins", .keyword_q_mins },
        .{ "mmax", .keyword_q_mmax },
        .{ "mmin", .keyword_q_mmin },
        .{ "mmu", .keyword_q_mmu },
        .{ "mod", .keyword_q_mod },
        .{ "msum", .keyword_q_msum },
        .{ "neg", .keyword_q_neg },
        .{ "next", .keyword_q_next },
        .{ "not", .keyword_q_not },
        .{ "null", .keyword_q_null },
        .{ "or", .keyword_q_or },
        .{ "over", .keyword_q_over },
        .{ "parse", .keyword_q_parse },
        .{ "peach", .keyword_q_peach },
        .{ "pj", .keyword_q_pj },
        .{ "prds", .keyword_q_prds },
        .{ "prev", .keyword_q_prev },
        .{ "prior", .keyword_q_prior },
        .{ "rand", .keyword_q_rand },
        .{ "rank", .keyword_q_rank },
        .{ "ratios", .keyword_q_ratios },
        .{ "raze", .keyword_q_raze },
        .{ "read0", .keyword_q_read0 },
        .{ "read1", .keyword_q_read1 },
        .{ "reciprocal", .keyword_q_reciprocal },
        .{ "reval", .keyword_q_reval },
        .{ "reverse", .keyword_q_reverse },
        .{ "rload", .keyword_q_rload },
        .{ "rotate", .keyword_q_rotate },
        .{ "rsave", .keyword_q_rsave },
        .{ "rtrim", .keyword_q_rtrim },
        .{ "save", .keyword_q_save },
        .{ "scan", .keyword_q_scan },
        .{ "scov", .keyword_q_scov },
        .{ "sdev", .keyword_q_sdev },
        .{ "set", .keyword_q_set },
        .{ "show", .keyword_q_show },
        .{ "signum", .keyword_q_signum },
        .{ "ssr", .keyword_q_ssr },
        .{ "string", .keyword_q_string },
        .{ "sublist", .keyword_q_sublist },
        .{ "sums", .keyword_q_sums },
        .{ "sv", .keyword_q_sv },
        .{ "svar", .keyword_q_svar },
        .{ "system", .keyword_q_system },
        .{ "tables", .keyword_q_tables },
        .{ "til", .keyword_q_til },
        .{ "trim", .keyword_q_trim },
        .{ "type", .keyword_q_type },
        .{ "uj", .keyword_q_uj },
        .{ "ujf", .keyword_q_ujf },
        .{ "ungroup", .keyword_q_ungroup },
        .{ "union", .keyword_q_union },
        .{ "upper", .keyword_q_upper },
        .{ "upsert", .keyword_q_upsert },
        .{ "value", .keyword_q_value },
        .{ "view", .keyword_q_view },
        .{ "views", .keyword_q_views },
        .{ "vs", .keyword_q_vs },
        .{ "where", .keyword_q_where },
        .{ "wj", .keyword_q_wj },
        .{ "wj1", .keyword_q_wj1 },
        .{ "ww", .keyword_q_ww },
        .{ "xasc", .keyword_q_xasc },
        .{ "xbar", .keyword_q_xbar },
        .{ "xcol", .keyword_q_xcol },
        .{ "xcols", .keyword_q_xcols },
        .{ "xdesc", .keyword_q_xdesc },
        .{ "xgroup", .keyword_q_xgroup },
        .{ "xkey", .keyword_q_xkey },
        .{ "xlog", .keyword_q_xlog },
        .{ "xprev", .keyword_q_xprev },
        .{ "xrank", .keyword_q_xrank },
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
        invalid,
        os,
        os_param,
        eof,

        // Keywords : -1","sv"keyword_",/:string asc .Q.res;
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

        // Q Keywords : -1","sv"keyword_q_",/:string asc 1_key`.q;
        keyword_q_aj,
        keyword_q_aj0,
        keyword_q_ajf,
        keyword_q_ajf0,
        keyword_q_all,
        keyword_q_and,
        keyword_q_any,
        keyword_q_asc,
        keyword_q_asof,
        keyword_q_attr,
        keyword_q_avgs,
        keyword_q_ceiling,
        keyword_q_cols,
        keyword_q_count,
        keyword_q_cross,
        keyword_q_csv,
        keyword_q_cut,
        keyword_q_deltas,
        keyword_q_desc,
        keyword_q_differ,
        keyword_q_distinct,
        keyword_q_dsave,
        keyword_q_each,
        keyword_q_ej,
        keyword_q_ema,
        keyword_q_eval,
        keyword_q_except,
        keyword_q_fby,
        keyword_q_fills,
        keyword_q_first,
        keyword_q_fkeys,
        keyword_q_flip,
        keyword_q_floor,
        keyword_q_get,
        keyword_q_group,
        keyword_q_gtime,
        keyword_q_hclose,
        keyword_q_hcount,
        keyword_q_hdel,
        keyword_q_hsym,
        keyword_q_iasc,
        keyword_q_idesc,
        keyword_q_ij,
        keyword_q_ijf,
        keyword_q_inter,
        keyword_q_inv,
        keyword_q_key,
        keyword_q_keys,
        keyword_q_lj,
        keyword_q_ljf,
        keyword_q_load,
        keyword_q_lower,
        keyword_q_lsq,
        keyword_q_ltime,
        keyword_q_ltrim,
        keyword_q_mavg,
        keyword_q_maxs,
        keyword_q_mcount,
        keyword_q_md5,
        keyword_q_mdev,
        keyword_q_med,
        keyword_q_meta,
        keyword_q_mins,
        keyword_q_mmax,
        keyword_q_mmin,
        keyword_q_mmu,
        keyword_q_mod,
        keyword_q_msum,
        keyword_q_neg,
        keyword_q_next,
        keyword_q_not,
        keyword_q_null,
        keyword_q_or,
        keyword_q_over,
        keyword_q_parse,
        keyword_q_peach,
        keyword_q_pj,
        keyword_q_prds,
        keyword_q_prev,
        keyword_q_prior,
        keyword_q_rand,
        keyword_q_rank,
        keyword_q_ratios,
        keyword_q_raze,
        keyword_q_read0,
        keyword_q_read1,
        keyword_q_reciprocal,
        keyword_q_reval,
        keyword_q_reverse,
        keyword_q_rload,
        keyword_q_rotate,
        keyword_q_rsave,
        keyword_q_rtrim,
        keyword_q_save,
        keyword_q_scan,
        keyword_q_scov,
        keyword_q_sdev,
        keyword_q_set,
        keyword_q_show,
        keyword_q_signum,
        keyword_q_ssr,
        keyword_q_string,
        keyword_q_sublist,
        keyword_q_sums,
        keyword_q_sv,
        keyword_q_svar,
        keyword_q_system,
        keyword_q_tables,
        keyword_q_til,
        keyword_q_trim,
        keyword_q_type,
        keyword_q_uj,
        keyword_q_ujf,
        keyword_q_ungroup,
        keyword_q_union,
        keyword_q_upper,
        keyword_q_upsert,
        keyword_q_value,
        keyword_q_view,
        keyword_q_views,
        keyword_q_vs,
        keyword_q_where,
        keyword_q_wj,
        keyword_q_wj1,
        keyword_q_ww,
        keyword_q_xasc,
        keyword_q_xbar,
        keyword_q_xcol,
        keyword_q_xcols,
        keyword_q_xdesc,
        keyword_q_xgroup,
        keyword_q_xkey,
        keyword_q_xlog,
        keyword_q_xprev,
        keyword_q_xrank,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .number_literal,
                .string_literal,
                .symbol_literal,
                .symbol_list_literal,
                .identifier,
                .comment,
                .invalid,
                .eof,
                .os,
                .os_param,
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

                .keyword_q_aj => "aj",
                .keyword_q_aj0 => "aj0",
                .keyword_q_ajf => "ajf",
                .keyword_q_ajf0 => "ajf0",
                .keyword_q_all => "all",
                .keyword_q_and => "and",
                .keyword_q_any => "any",
                .keyword_q_asc => "asc",
                .keyword_q_asof => "asof",
                .keyword_q_attr => "attr",
                .keyword_q_avgs => "avgs",
                .keyword_q_ceiling => "ceiling",
                .keyword_q_cols => "cols",
                .keyword_q_count => "count",
                .keyword_q_cross => "cross",
                .keyword_q_csv => "csv",
                .keyword_q_cut => "cut",
                .keyword_q_deltas => "deltas",
                .keyword_q_desc => "desc",
                .keyword_q_differ => "differ",
                .keyword_q_distinct => "distinct",
                .keyword_q_dsave => "dsave",
                .keyword_q_each => "each",
                .keyword_q_ej => "ej",
                .keyword_q_ema => "ema",
                .keyword_q_eval => "eval",
                .keyword_q_except => "except",
                .keyword_q_fby => "fby",
                .keyword_q_fills => "fills",
                .keyword_q_first => "first",
                .keyword_q_fkeys => "fkeys",
                .keyword_q_flip => "flip",
                .keyword_q_floor => "floor",
                .keyword_q_get => "get",
                .keyword_q_group => "group",
                .keyword_q_gtime => "gtime",
                .keyword_q_hclose => "hclose",
                .keyword_q_hcount => "hcount",
                .keyword_q_hdel => "hdel",
                .keyword_q_hsym => "hsym",
                .keyword_q_iasc => "iasc",
                .keyword_q_idesc => "idesc",
                .keyword_q_ij => "ij",
                .keyword_q_ijf => "ijf",
                .keyword_q_inter => "inter",
                .keyword_q_inv => "inv",
                .keyword_q_key => "key",
                .keyword_q_keys => "keys",
                .keyword_q_lj => "lj",
                .keyword_q_ljf => "ljf",
                .keyword_q_load => "load",
                .keyword_q_lower => "lower",
                .keyword_q_lsq => "lsq",
                .keyword_q_ltime => "ltime",
                .keyword_q_ltrim => "ltrim",
                .keyword_q_mavg => "mavg",
                .keyword_q_maxs => "maxs",
                .keyword_q_mcount => "mcount",
                .keyword_q_md5 => "md5",
                .keyword_q_mdev => "mdev",
                .keyword_q_med => "med",
                .keyword_q_meta => "meta",
                .keyword_q_mins => "mins",
                .keyword_q_mmax => "mmax",
                .keyword_q_mmin => "mmin",
                .keyword_q_mmu => "mmu",
                .keyword_q_mod => "mod",
                .keyword_q_msum => "msum",
                .keyword_q_neg => "neg",
                .keyword_q_next => "next",
                .keyword_q_not => "not",
                .keyword_q_null => "null",
                .keyword_q_or => "or",
                .keyword_q_over => "over",
                .keyword_q_parse => "parse",
                .keyword_q_peach => "peach",
                .keyword_q_pj => "pj",
                .keyword_q_prds => "prds",
                .keyword_q_prev => "prev",
                .keyword_q_prior => "prior",
                .keyword_q_rand => "rand",
                .keyword_q_rank => "rank",
                .keyword_q_ratios => "ratios",
                .keyword_q_raze => "raze",
                .keyword_q_read0 => "read0",
                .keyword_q_read1 => "read1",
                .keyword_q_reciprocal => "reciprocal",
                .keyword_q_reval => "reval",
                .keyword_q_reverse => "reverse",
                .keyword_q_rload => "rload",
                .keyword_q_rotate => "rotate",
                .keyword_q_rsave => "rsave",
                .keyword_q_rtrim => "rtrim",
                .keyword_q_save => "save",
                .keyword_q_scan => "scan",
                .keyword_q_scov => "scov",
                .keyword_q_sdev => "sdev",
                .keyword_q_set => "set",
                .keyword_q_show => "show",
                .keyword_q_signum => "signum",
                .keyword_q_ssr => "ssr",
                .keyword_q_string => "string",
                .keyword_q_sublist => "sublist",
                .keyword_q_sums => "sums",
                .keyword_q_sv => "sv",
                .keyword_q_svar => "svar",
                .keyword_q_system => "system",
                .keyword_q_tables => "tables",
                .keyword_q_til => "til",
                .keyword_q_trim => "trim",
                .keyword_q_type => "type",
                .keyword_q_uj => "uj",
                .keyword_q_ujf => "ujf",
                .keyword_q_ungroup => "ungroup",
                .keyword_q_union => "union",
                .keyword_q_upper => "upper",
                .keyword_q_upsert => "upsert",
                .keyword_q_value => "value",
                .keyword_q_view => "view",
                .keyword_q_views => "views",
                .keyword_q_vs => "vs",
                .keyword_q_where => "where",
                .keyword_q_wj => "wj",
                .keyword_q_wj1 => "wj1",
                .keyword_q_ww => "ww",
                .keyword_q_xasc => "xasc",
                .keyword_q_xbar => "xbar",
                .keyword_q_xcol => "xcol",
                .keyword_q_xcols => "xcols",
                .keyword_q_xdesc => "xdesc",
                .keyword_q_xgroup => "xgroup",
                .keyword_q_xkey => "xkey",
                .keyword_q_xlog => "xlog",
                .keyword_q_xprev => "xprev",
                .keyword_q_xrank => "xrank",
            };
        }

        test "Token tags which have a known lexeme parse as expected" {
            inline for (@typeInfo(Token.Tag).Enum.fields) |field| {
                const tag: Token.Tag = @enumFromInt(field.value);
                if (tag.lexeme()) |l| {
                    const source = try std.testing.allocator.allocSentinel(u8, l.len + 2, 0);
                    defer std.testing.allocator.free(source);
                    source[0] = '(';
                    @memcpy(source[1 .. l.len + 1], l);
                    source[l.len + 1] = ')';
                    try testTokenize(source, &.{
                        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
                        .{ .tag = tag, .loc = .{ .start = 1, .end = l.len + 1 }, .eob = false },
                        .{ .tag = .r_paren, .loc = .{ .start = l.len + 1, .end = l.len + 2 }, .eob = true },
                    });
                }
            }
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .number_literal => "a number literal",
                .string_literal => "a string literal",
                .symbol_literal => "a symbol literal",
                .symbol_list_literal => " a symbol list literal",
                .identifier => "an identifier",
                .comment => "a comment",
                .invalid => "invalid bytes",
                .eof => "EOF",
                .os => "an OS command",
                .os_param => "an OS command parameter",
                else => if (builtin.is_test) @panic(@tagName(tag)) else unreachable,
            };
        }

        test "Token tags are not unreachable when calling symbol()" {
            inline for (@typeInfo(Token.Tag).Enum.fields) |field| {
                const tag: Token.Tag = @enumFromInt(field.value);
                _ = tag.symbol();
            }
        }
    };
};

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

fn testTokenize(source: [:0]const u8, expected: []const Token) !void {
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
    try testTokenize("{1}[]-1", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 6, .end = 7 }, .eob = true },
    });
    try testTokenize("{1}[]- 1", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 5, .end = 6 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 7, .end = 8 }, .eob = true },
    });
    try testTokenize("{1}[] - 1", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .minus, .loc = .{ .start = 6, .end = 7 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 8, .end = 9 }, .eob = true },
    });
    try testTokenize("{1}[] -1", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 4, .end = 5 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 6, .end = 8 }, .eob = true },
    });
    try testTokenize("{[]-1}", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 5 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 5, .end = 6 }, .eob = true },
    });
    try testTokenize("{[x]-1}", &.{
        .{ .tag = .l_brace, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 4, .end = 6 }, .eob = false },
        .{ .tag = .r_brace, .loc = .{ .start = 6, .end = 7 }, .eob = true },
    });
    try testTokenize("([]-1)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 3, .end = 5 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 5, .end = 6 }, .eob = true },
    });
    try testTokenize("([x]-1)", &.{
        .{ .tag = .l_paren, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .l_bracket, .loc = .{ .start = 1, .end = 2 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 2, .end = 3 }, .eob = false },
        .{ .tag = .r_bracket, .loc = .{ .start = 3, .end = 4 }, .eob = false },
        .{ .tag = .number_literal, .loc = .{ .start = 4, .end = 6 }, .eob = false },
        .{ .tag = .r_paren, .loc = .{ .start = 6, .end = 7 }, .eob = true },
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
        .{ .tag = .keyword_q_string, .loc = .{ .start = 30, .end = 36 }, .eob = false },
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

    try testTokenizeLanguage(.k, "`symbol_with_underscore", &.{
        .{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 7 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 7, .end = 8 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 8, .end = 12 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 12, .end = 13 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 13, .end = 23 }, .eob = true },
    });
    try testTokenizeLanguage(.q, "`symbol_with_underscore", &.{.{ .tag = .symbol_literal, .loc = .{ .start = 0, .end = 23 }, .eob = true }});
    try testTokenizeLanguage(.k, "`_symbol_with_leading_underscore", &.{
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
    try testTokenizeLanguage(.q, "`_symbol_with_leading_underscore", &.{
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

    try testTokenizeLanguage(.k, "identifier_with_underscore", &.{
        .{ .tag = .identifier, .loc = .{ .start = 0, .end = 10 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 10, .end = 11 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 11, .end = 15 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 15, .end = 16 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 16, .end = 26 }, .eob = true },
    });
    try testTokenizeLanguage(.q, "identifier_with_underscore", &.{.{ .tag = .identifier, .loc = .{ .start = 0, .end = 26 }, .eob = true }});
    try testTokenizeLanguage(.k, "_identifier_with_leading_underscore", &.{
        .{ .tag = .underscore, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 1, .end = 11 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 11, .end = 12 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 12, .end = 16 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 16, .end = 17 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 17, .end = 24 }, .eob = false },
        .{ .tag = .underscore, .loc = .{ .start = 24, .end = 25 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 25, .end = 35 }, .eob = true },
    });
    try testTokenizeLanguage(.q, "_identifier_with_leading_underscore", &.{
        .{ .tag = .underscore, .loc = .{ .start = 0, .end = 1 }, .eob = false },
        .{ .tag = .identifier, .loc = .{ .start = 1, .end = 35 }, .eob = true },
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
