const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const lsp = @import("lsp");
const types = lsp.types;
const offsets = lsp.offsets;

pub fn applyContentChanges(
    gpa: Allocator,
    text: []const u8,
    content_changes: []const types.TextDocument.ContentChangeEvent,
    encoding: offsets.Encoding,
) ![:0]const u8 {
    const last_full_text_index, const last_full_text = blk: {
        var i = content_changes.len;
        while (i != 0) {
            i -= 1;
            switch (content_changes[i]) {
                .text_document_content_change_partial => continue,
                .text_document_content_change_whole_document => |content_change| break :blk .{ i, content_change.text },
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
        const content_change = item.text_document_content_change_partial;
        const loc = offsets.rangeToLoc(text_array.items, content_change.range, encoding);
        try text_array.replaceRange(gpa, loc.start, loc.end - loc.start, content_change.text);
    }

    return try text_array.toOwnedSliceSentinel(gpa, 0);
}

test {
    std.testing.refAllDecls(@This());
}
