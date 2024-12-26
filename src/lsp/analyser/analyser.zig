pub const InternPool = @import("InternPool.zig");

comptime {
    @import("std").testing.refAllDeclsRecursive(@This());
}
