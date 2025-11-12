const std = @import("std");
const Allocator = std.mem.Allocator;
const lsp = @import("lsp");

pub fn applyContentChanges(
    gpa: Allocator,
    text: []const u8,
    content_changes: []const lsp.types.TextDocumentContentChangeEvent,
    encoding: lsp.offsets.Encoding,
) ![:0]const u8 {
    const last_full_text_index, const last_full_text = blk: {
        var i = content_changes.len;
        while (i != 0) {
            i -= 1;
            switch (content_changes[i]) {
                .literal_0 => continue, // TextDocumentContentChangePartial
                .literal_1 => |content_change| break :blk .{ i, content_change.text }, // TextDocumentContentChangeWholeDocument
            }
        }
        break :blk .{ null, text };
    };

    var text_array: std.ArrayList(u8) = .empty;
    errdefer text_array.deinit(gpa);

    try text_array.appendSlice(gpa, last_full_text);

    // don't even bother applying changes before a full text change
    const changes = content_changes[if (last_full_text_index) |index| index + 1 else 0..];
    for (changes) |item| {
        const content_change = item.literal_0; // TextDocumentContentChangePartial
        const loc = lsp.offsets.rangeToLoc(text_array.items, content_change.range, encoding);
        try text_array.replaceRange(gpa, loc.start, loc.end - loc.start, content_change.text);
    }

    return try text_array.toOwnedSliceSentinel(gpa, 0);
}
