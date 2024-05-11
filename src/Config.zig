const Version = @import("kdb.zig").Ast.Version;

kdb_version: Version = .@"4.0",

test {
    @import("std").testing.refAllDecls(@This());
}
