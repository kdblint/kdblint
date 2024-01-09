const std = @import("std");
const types = @import("lsp").types;
const offsets = @import("offsets.zig");

pub fn applyContentChanges(
    allocator: std.mem.Allocator,
    text: []const u8,
    content_changes: []const types.TextDocumentContentChangeEvent,
    encoding: offsets.Encoding,
) error{ OutOfMemory, InvalidParams }![:0]const u8 {
    var last_full_text_change: ?usize = null;
    var i: usize = content_changes.len;
    while (i > 0) {
        i -= 1;
        if (content_changes[i] == .literal_1) {
            last_full_text_change = i;
            continue;
        }
    }

    var text_array = std.ArrayListUnmanaged(u8){};
    errdefer text_array.deinit(allocator);

    try text_array.appendSlice(allocator, if (last_full_text_change) |index| content_changes[index].literal_1.text else text);

    // don't even bother applying changes before a full text change
    const changes = content_changes[if (last_full_text_change) |index| index + 1 else 0..];
    for (changes) |item| {
        const range = item.literal_0.range;

        const start = offsets.maybePositionToIndex(text_array.items, range.start, encoding) orelse return error.InvalidParams;
        const end = offsets.maybePositionToIndex(text_array.items, range.end, encoding) orelse return error.InvalidParams;
        try text_array.replaceRange(allocator, start, end - start, item.literal_0.text);
    }

    return try text_array.toOwnedSliceSentinel(allocator, 0);
}

test {
    @import("std").testing.refAllDecls(@This());
}
