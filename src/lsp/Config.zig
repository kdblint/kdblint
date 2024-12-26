const kdb = @import("kdb");
const Version = kdb.Ast.Version;

kdb_version: Version = .@"4.0",

/// Set level of semantic tokens. `partial` only includes information that requires semantic analysis.
semantic_tokens: enum {
    none,
    partial,
    full,
} = .full,

test {
    @import("std").testing.refAllDecls(@This());
}
