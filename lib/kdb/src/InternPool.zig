/// Represents an index into `map`. It represents the canonical index
/// of a `Value` within this `InternPool`. The values are typed.
/// Two values which have the same type can be equality compared simply
/// by checking if their indexes are equal, provided they are both in
/// the same `InternPool`.
/// When adding a tag to this enum, consider adding a corresponding entry to
/// `primitives` in AstGen.zig.
pub const Index = enum(u32) {
    /// `0` (comptime_int)
    zero,
    /// `1` (comptime_int)
    one,
    /// `-1` (comptime_int)
    negative_one,
    /// `::` (untyped)
    null,
    /// `()` (untyped)
    empty_list,
    /// `x`
    x,
    /// `y`
    y,
    /// `z`
    z,

    assign,
    add,
    subtract,
    multiply,
    divide,
    lesser,
    greater,
    fill,
    equal,
    less_than,
    less_than_or_equal,
    not_equal,
    greater_than,
    greater_than_or_equal,
    cast,
    join,
    take,
    drop,
    match,
    dict,
    find,
    apply_at,
    apply_dot,
    file_text,
    file_binary,
    dynamic_load,

    each,
    each_prior,
    over,
    each_right,
    scan,
    each_left,
};
