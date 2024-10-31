const kdb = @import("kdb");
const Version = kdb.Ast.Version;

kdb_version: Version = .@"4.0",

test {
    @import("std").testing.refAllDecls(@This());
}
