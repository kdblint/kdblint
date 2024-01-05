const std = @import("std");
const lsp = @import("zig-lsp").types;

const log = std.log.scoped(.kdbLint);

const Connection = @import("zig-lsp").Connection(std.fs.File.Reader, std.fs.File.Writer, Context);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var context = Context.init(allocator);
    defer context.deinit();

    const reader = std.io.getStdIn().reader();
    const writer = std.io.getStdOut().writer();

    var conn = Connection.init(allocator, reader, writer, &context);
    defer conn.callback_map.deinit(allocator);
    defer conn.write_buffer.deinit(allocator);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    while (true) {
        const arena_allocator = arena.allocator();
        conn.accept(arena_allocator) catch return;
    }
}

const Context = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        _ = self;
    }

    pub fn initialize(_: *Connection, _: lsp.RequestId, _: lsp.InitializeParams) !lsp.InitializeResult {
        log.info("initialize", .{});

        return lsp.InitializeResult{
            .capabilities = .{
                .positionEncoding = .@"utf-16",
                .textDocumentSync = .{ .TextDocumentSyncKind = lsp.TextDocumentSyncKind.Incremental },
                .notebookDocumentSync = .{ .NotebookDocumentSyncOptions = .{ .notebookSelector = &.{} } },
            },
            .serverInfo = .{
                .name = "kdbLint Language Server",
                .version = "0.1.0",
            },
        };
    }

    pub fn initialized(_: *Connection, _: lsp.InitializedParams) !void {
        log.debug("initialized", .{});
    }
};
