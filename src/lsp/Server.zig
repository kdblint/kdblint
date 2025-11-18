const std = @import("std");
const builtin = @import("builtin");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const lsp = @import("lsp");
const types = lsp.types;
const build_options = @import("build_options");

const DiagnosticsCollection = @import("DiagnosticsCollection.zig");
const DocumentStore = @import("DocumentStore.zig");
const diagnostics_gen = @import("features/diagnostics.zig");
const diff = @import("diff.zig");
const Uri = @import("Uri.zig");

const Server = @This();

io: Io,
gpa: Allocator,
document_store: DocumentStore,
diagnostics_collection: DiagnosticsCollection,
transport: *lsp.Transport,
offset_encoding: lsp.offsets.Encoding = .@"utf-16",
status: Status = .uninitialized,

thread_pool: std.Thread.Pool,
wait_group: std.Thread.WaitGroup = .{},
client_capabilities: ClientCapabilities = .{},

const ClientCapabilities = struct {
    supports_publish_diagnostics: bool = false,
};

pub fn create(io: Io, gpa: Allocator, transport: *lsp.Transport) !*Server {
    const server = try gpa.create(Server);
    errdefer gpa.destroy(server);

    server.* = .{
        .io = io,
        .gpa = gpa,
        .document_store = .{
            .io = io,
            .gpa = gpa,
            .thread_pool = &server.thread_pool,
        },
        .diagnostics_collection = .{
            .gpa = gpa,
            .transport = transport,
        },
        .transport = transport,
        .thread_pool = undefined, // set below
    };

    try server.thread_pool.init(.{
        .allocator = gpa,
        .n_jobs = @min(4, std.Thread.getCpuCount() catch 1),
    });
    errdefer comptime unreachable;

    return server;
}

pub fn destroy(server: *Server) void {
    server.thread_pool.deinit();
    server.document_store.deinit();
    server.diagnostics_collection.deinit();
    server.gpa.destroy(server);
}

pub fn keepRunning(server: Server) bool {
    return switch (server.status) {
        .exiting_success, .exiting_failure => false,
        else => true,
    };
}

pub fn loop(server: *Server) !void {
    while (server.keepRunning()) {
        const json_message = try server.transport.readJsonMessage(server.gpa);
        defer server.gpa.free(json_message);

        var arena_allocator: std.heap.ArenaAllocator = .init(server.gpa);
        errdefer arena_allocator.deinit();

        const message = Message.parseFromSliceLeaky(
            arena_allocator.allocator(),
            json_message,
            .{ .ignore_unknown_fields = true, .allocate = .alloc_always },
        ) catch return error.ParseError;
        errdefer comptime unreachable;

        if (isBlockingMessage(message)) {
            server.thread_pool.waitAndWork(&server.wait_group);
            server.wait_group.reset();
            server.processMessageReportError(arena_allocator.state, message);
        } else {
            server.thread_pool.spawnWg(&server.wait_group, processMessageReportError, .{ server, arena_allocator.state, message });
        }
    }
}

fn initialize(
    server: *Server,
    _: Allocator,
    request: types.InitializeParams,
) !types.InitializeResult {
    server.status = .initializing;

    std.log.info("kdblint {s} {t} {t}-{t}", .{
        build_options.version_string,
        builtin.mode,
        builtin.cpu.arch,
        builtin.os.tag,
    });

    if (request.clientInfo) |client_info| {
        std.log.info("The client is '{s}' ({s})", .{
            client_info.name,
            client_info.version orelse "unknown version",
        });
    }

    if (request.capabilities.general) |general| {
        if (general.positionEncodings) |position_encodings| {
            server.offset_encoding = outer: for (position_encodings) |encoding| {
                switch (encoding) {
                    .@"utf-8" => break :outer .@"utf-8",
                    .@"utf-16" => break :outer .@"utf-16",
                    .@"utf-32" => break :outer .@"utf-32",
                    .custom_value => {},
                }
            } else server.offset_encoding;
            server.diagnostics_collection.offset_encoding = server.offset_encoding;
        }
    }

    if (request.capabilities.textDocument) |text_document| {
        server.client_capabilities.supports_publish_diagnostics = text_document.publishDiagnostics != null;
    }

    return .{
        .serverInfo = .{
            .name = "kdblint Language Server",
            .version = build_options.version_string,
        },
        .capabilities = .{
            .positionEncoding = switch (server.offset_encoding) {
                .@"utf-8" => .@"utf-8",
                .@"utf-16" => .@"utf-16",
                .@"utf-32" => .@"utf-32",
            },
            .textDocumentSync = .{
                .TextDocumentSyncOptions = .{
                    .openClose = true,
                    .change = .Incremental,
                },
            },
            .workspace = .{
                .workspaceFolders = .{
                    .supported = true,
                    .changeNotifications = .{ .bool = true },
                },
            },
        },
    };
}

fn shutdown(
    server: *Server,
    _: Allocator,
    _: void,
) !?void {
    server.status = .shutdown;
}

fn initialized(
    server: *Server,
    _: Allocator,
    _: types.InitializedParams,
) !void {
    server.status = .initialized;
}

fn exit(
    server: *Server,
    _: Allocator,
    _: void,
) !void {
    server.status = switch (server.status) {
        .shutdown => .exiting_success,
        else => .exiting_failure,
    };
}

fn @"textDocument/didOpen"(
    server: *Server,
    arena: Allocator,
    notification: types.DidOpenTextDocumentParams,
) !void {
    const document_uri = Uri.parse(arena, notification.textDocument.uri) catch return error.InvalidParams;
    try server.document_store.openLspSyncedDocument(document_uri, notification.textDocument.text);
    server.generateDiagnostics(server.document_store.getHandle(document_uri).?);
}

fn @"textDocument/didChange"(
    server: *Server,
    arena: Allocator,
    notification: types.DidChangeTextDocumentParams,
) !void {
    if (notification.contentChanges.len == 0) return;

    const document_uri = Uri.parse(arena, notification.textDocument.uri) catch return error.InvalidParams;
    const handle = server.document_store.getHandle(document_uri) orelse return;

    const new_text = try diff.applyContentChanges(
        server.gpa,
        handle.tree.source,
        notification.contentChanges,
        server.offset_encoding,
    );
    errdefer server.gpa.free(new_text);

    try server.document_store.refreshLspSyncedDocument(handle.uri, new_text);
    server.generateDiagnostics(handle);
}

fn @"textDocument/didSave"(
    server: *Server,
    arena: Allocator,
    notification: types.DidSaveTextDocumentParams,
) !void {
    _ = server; // autofix
    _ = arena; // autofix
    _ = notification; // autofix
}

fn @"textDocument/didClose"(
    server: *Server,
    arena: Allocator,
    notification: types.DidCloseTextDocumentParams,
) !void {
    const document_uri = Uri.parse(arena, notification.textDocument.uri) catch return error.InvalidParams;
    server.document_store.closeLspSyncedDocument(document_uri);

    if (server.client_capabilities.supports_publish_diagnostics) {
        server.diagnostics_collection.clearSingleDocumentDiagnostics(document_uri);
        server.diagnostics_collection.publishDiagnostics() catch |err| {
            std.log.err("failed to publish diagnostics: {t}", .{err});
        };
    }
}

fn @"workspace/didChangeWatchedFiles"(
    server: *Server,
    arena: Allocator,
    notification: types.DidChangeWatchedFilesParams,
) !void {
    _ = server; // autofix
    _ = arena; // autofix
    _ = notification; // autofix
}

fn @"workspace/didChangeWorkspaceFolders"(
    server: *Server,
    arena: Allocator,
    notification: types.DidChangeWorkspaceFoldersParams,
) !void {
    _ = server; // autofix
    _ = arena; // autofix
    _ = notification; // autofix
}

fn @"workspace/didChangeConfiguration"(
    server: *Server,
    arena: Allocator,
    notification: types.DidChangeConfigurationParams,
) !void {
    _ = server; // autofix
    _ = arena; // autofix
    _ = notification; // autofix
}

fn generateDiagnostics(server: *Server, handle: *DocumentStore.Handle) void {
    if (!server.client_capabilities.supports_publish_diagnostics) return;
    const do = struct {
        fn do(param_server: *Server, param_handle: *DocumentStore.Handle) void {
            diagnostics_gen.generateDiagnostics(param_server, param_handle) catch |err| switch (err) {
                error.OutOfMemory => {},
                error.WriteFailed => {},
            };
        }
    }.do;
    server.thread_pool.spawnWg(&server.wait_group, do, .{ server, handle });
}

const HandledRequestParams = union(enum) {
    initialize: types.InitializeParams,
    shutdown,
    other: lsp.MethodWithParams,
};

const HandledNotificationParams = union(enum) {
    initialized: types.InitializedParams,
    exit,
    @"textDocument/didOpen": types.DidOpenTextDocumentParams,
    @"textDocument/didChange": types.DidChangeTextDocumentParams,
    @"textDocument/didSave": types.DidSaveTextDocumentParams,
    @"textDocument/didClose": types.DidCloseTextDocumentParams,
    @"workspace/didChangeWatchedFiles": types.DidChangeWatchedFilesParams,
    @"workspace/didChangeWorkspaceFolders": types.DidChangeWorkspaceFoldersParams,
    @"workspace/didChangeConfiguration": types.DidChangeConfigurationParams,
    other: lsp.MethodWithParams,
};

const Message = lsp.Message(HandledRequestParams, HandledNotificationParams, .{});

fn isBlockingMessage(message: Message) bool {
    return switch (message) {
        .request => |request| switch (request.params) {
            .initialize, .shutdown => true,
            .other => false,
        },
        .notification => |notification| switch (notification.params) {
            .initialized,
            .exit,
            .@"textDocument/didOpen",
            .@"textDocument/didChange",
            .@"textDocument/didSave",
            .@"textDocument/didClose",
            .@"workspace/didChangeWatchedFiles",
            .@"workspace/didChangeWorkspaceFolders",
            .@"workspace/didChangeConfiguration",
            => true,
            .other => false,
        },
        .response => true,
    };
}

pub const Error = error{
    OutOfMemory,
    ParseError,
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
    /// Error code indicating that a server received a notification or
    /// request before the server has received the `initialize` request.
    ServerNotInitialized,
    /// A request failed but it was syntactically correct, e.g the
    /// method name was known and the parameters were valid. The error
    /// message should contain human readable information about why
    /// the request failed.
    ///
    /// @since 3.17.0
    RequestFailed,
    /// The server cancelled the request. This error code should
    /// only be used for requests that explicitly support being
    /// server cancellable.
    ///
    /// @since 3.17.0
    ServerCancelled,
    /// The server detected that the content of a document got
    /// modified outside normal conditions. A server should
    /// NOT send this error code if it detects a content change
    /// in it unprocessed messages. The result even computed
    /// on an older state might still be useful for the client.
    ///
    /// If a client decides that a result is not of any use anymore
    /// the client should cancel the request.
    ContentModified,
    /// The client has canceled a request and a server as detected
    /// the cancel.
    RequestCancelled,
};

pub const Status = enum {
    /// the server has not received a `initialize` request
    uninitialized,
    /// the server has received a `initialize` request and is awaiting the `initialized` notification
    initializing,
    /// the server has been initialized and is ready to received requests
    initialized,
    /// the server has been shutdown and can't handle any more requests
    shutdown,
    /// the server is received a `exit` notification and has been shutdown
    exiting_success,
    /// the server is received a `exit` notification but has not been shutdown
    exiting_failure,
};

fn sendToClientResponse(server: *Server, id: lsp.JsonRPCMessage.ID, result: anytype) ![]u8 {
    // TODO validate result type is a possible response
    // TODO validate response is from a client to server request
    // TODO validate result type

    const response: lsp.TypedJsonRPCResponse(@TypeOf(result)) = .{
        .id = id,
        .result_or_error = .{ .result = result },
    };
    return sendToClientInternal(server.gpa, server.transport, response);
}

fn sendToClientRequest(server: *Server, id: lsp.JsonRPCMessage.ID, method: []const u8, params: anytype) ![]u8 {
    // TODO validate method is a request
    // TODO validate method is server to client
    // TODO validate params type

    const request: lsp.TypedJsonRPCRequest(@TypeOf(params)) = .{
        .id = id,
        .method = method,
        .params = params,
    };
    return sendToClientInternal(server.gpa, server.transport, request);
}

fn sendToClientNotification(server: *Server, method: []const u8, params: anytype) ![]u8 {
    // TODO validate method is a notification
    // TODO validate method is server to client
    // TODO validate params type

    const notification: lsp.TypedJsonRPCNotification(@TypeOf(params)) = .{
        .method = method,
        .params = params,
    };
    return sendToClientInternal(server.gpa, server.transport, notification);
}

fn sendToClientResponseError(server: *Server, id: lsp.JsonRPCMessage.ID, err: lsp.JsonRPCMessage.Response.Error) ![]u8 {
    const response: lsp.JsonRPCMessage = .{
        .response = .{ .id = id, .result_or_error = .{ .@"error" = err } },
    };

    return sendToClientInternal(server.gpa, server.transport, response);
}

fn sendToClientInternal(gpa: Allocator, transport: ?*lsp.Transport, message: anytype) error{OutOfMemory}![]u8 {
    const message_stringified = try std.json.Stringify.valueAlloc(gpa, message, .{
        .emit_null_optional_fields = false,
    });
    errdefer gpa.free(message_stringified);

    if (transport) |t| {
        t.writeJsonMessage(message_stringified) catch |err| {
            std.log.err("failed to write message: {}", .{err});
        };
    }

    return message_stringified;
}

pub fn sendJsonMessageSync(server: *Server, json_message: []const u8) !?[]u8 {
    const parsed_message = Message.parseFromSlice(
        server.gpa,
        json_message,
        .{ .ignore_unknown_fields = true, .allocate = .alloc_always },
    ) catch return error.ParseError;
    defer parsed_message.deinit();
    return server.processMessage(parsed_message.arena.allocator(), parsed_message.value);
}

pub fn sendRequestSync(
    server: *Server,
    arena: Allocator,
    comptime method: []const u8,
    params: lsp.ParamsType(method),
) !lsp.ResultType(method) {
    comptime assert(lsp.isRequestMethod(method));

    const Params = std.meta.Tag(HandledRequestParams);
    if (!@hasField(Params, method)) return null;

    return switch (@field(Params, method)) {
        .initialize => try server.initialize(arena, params),
        .shutdown => try server.shutdown(arena, params),
        .other => return null,
    };
}

pub fn sendNotificationSync(
    server: *Server,
    arena: Allocator,
    comptime method: []const u8,
    params: lsp.ParamsType(method),
) !void {
    comptime assert(lsp.isNotificationMethod(method));

    const Params = std.meta.Tag(HandledNotificationParams);
    if (!@hasField(Params, method)) return null;

    return switch (@field(Params, method)) {
        .initialized => try server.initialized(arena, params),
        .exit => try server.exit(arena, params),
        .@"textDocument/didOpen" => try server.@"textDocument/didOpen"(arena, params),
        .@"textDocument/didChange" => try server.@"textDocument/didChange"(arena, params),
        .@"textDocument/didSave" => try server.@"textDocument/didSave"(arena, params),
        .@"textDocument/didClose" => try server.@"textDocument/didClose"(arena, params),
        .@"workspace/didChangeWatchedFiles" => try server.@"workspace/didChangeWatchedFiles"(arena, params),
        .@"workspace/didChangeWorkspaceFolders" => try server.@"workspace/didChangeWorkspaceFolders"(arena, params),
        .@"workspace/didChangeConfiguration" => try server.@"workspace/didChangeConfiguration"(arena, params),
        .other => {},
    };
}

pub fn sendMessageSync(
    server: *Server,
    arena: Allocator,
    comptime method: []const u8,
    params: lsp.ParamsType(method),
) !lsp.ResultType(method) {
    comptime assert(lsp.isRequestMethod(method) or lsp.isNotificationMethod(method));

    if (comptime lsp.isRequestMethod(method)) {
        return server.sendRequestSync(arena, method, params);
    } else if (comptime lsp.isNotificationMethod(method)) {
        return server.sendNotificationSync(arena, method, params);
    } else unreachable;
}

fn processMessage(server: *Server, arena: Allocator, message: Message) Error!?[]u8 {
    try server.validateMessage(message);

    std.log.debug("{f}", .{fmtMessage(message)});

    switch (message) {
        .request => |request| switch (request.params) {
            .other => return try server.sendToClientResponse(request.id, @as(?void, null)),
            inline else => |params, method| {
                const result = try server.sendRequestSync(arena, @tagName(method), params);
                return try server.sendToClientResponse(request.id, result);
            },
        },
        .notification => |notification| switch (notification.params) {
            .other => {},
            inline else => |params, method| try server.sendNotificationSync(arena, @tagName(method), params),
        },
        .response => |response| try server.handleResponse(response),
    }
    return null;
}

fn processMessageReportError(server: *Server, arena_state: std.heap.ArenaAllocator.State, message: Message) void {
    var arena_allocator = arena_state.promote(server.gpa);
    defer arena_allocator.deinit();

    if (server.processMessage(arena_allocator.allocator(), message)) |json_message| {
        server.gpa.free(json_message orelse return);
    } else |err| {
        std.log.err("failed to process {f}: {}", .{ fmtMessage(message), err });
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace);
        }

        switch (message) {
            .request => |request| {
                const json_message = server.sendToClientResponseError(request.id, .{
                    .code = @enumFromInt(switch (err) {
                        error.OutOfMemory => @intFromEnum(types.ErrorCodes.InternalError),
                        error.ParseError => @intFromEnum(types.ErrorCodes.ParseError),
                        error.InvalidRequest => @intFromEnum(types.ErrorCodes.InvalidRequest),
                        error.MethodNotFound => @intFromEnum(types.ErrorCodes.MethodNotFound),
                        error.InvalidParams => @intFromEnum(types.ErrorCodes.InvalidParams),
                        error.InternalError => @intFromEnum(types.ErrorCodes.InternalError),
                        error.ServerNotInitialized => @intFromEnum(types.ErrorCodes.ServerNotInitialized),
                        error.RequestFailed => @intFromEnum(types.LSPErrorCodes.RequestFailed),
                        error.ServerCancelled => @intFromEnum(types.LSPErrorCodes.ServerCancelled),
                        error.ContentModified => @intFromEnum(types.LSPErrorCodes.ContentModified),
                        error.RequestCancelled => @intFromEnum(types.LSPErrorCodes.RequestCancelled),
                    }),
                    .message = @errorName(err),
                }) catch return;
                server.gpa.free(json_message);
            },
            .notification, .response => return,
        }
    }
}

fn validateMessage(server: *const Server, message: Message) !void {
    const method = switch (message) {
        .request => |request| switch (request.params) {
            .other => |info| info.method,
            else => @tagName(request.params),
        },
        .notification => |notification| switch (notification.params) {
            .other => |info| info.method,
            else => @tagName(notification.params),
        },
        .response => return, // validation happens in `handleResponse`
    };

    // https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#dollarRequests
    if (message == .request and std.mem.startsWith(u8, method, "$/")) return error.MethodNotFound;
    if (message == .notification and std.mem.startsWith(u8, method, "$/")) return;

    switch (server.status) {
        .uninitialized => blk: {
            if (std.mem.eql(u8, method, "initialize")) break :blk;
            if (std.mem.eql(u8, method, "exit")) break :blk;

            return error.ServerNotInitialized; // server received a request before being initialized!
        },
        .initializing => blk: {
            if (std.mem.eql(u8, method, "initialized")) break :blk;
            if (std.mem.eql(u8, method, "$/progress")) break :blk;

            return error.InvalidRequest; // server received a request during initialization!
        },
        .initialized => {},
        .shutdown => blk: {
            if (std.mem.eql(u8, method, "exit")) break :blk;

            return error.InvalidRequest; // server received a request after shutdown!
        },
        .exiting_success,
        .exiting_failure,
        => unreachable,
    }
}

fn handleResponse(_: *Server, response: lsp.JsonRPCMessage.Response) !void {
    if (response.id == null) {
        std.log.warn("received response from client without id!", .{});
        return;
    }

    const id: []const u8 = switch (response.id.?) {
        .string => |id| id,
        .number => |id| {
            std.log.warn("received response from client with id '{d}' that has no handler!", .{id});
            return;
        },
    };

    switch (response.result_or_error) {
        .result => {},
        .@"error" => |err| {
            std.log.err("Error response for '{s}': {}, {s}", .{ id, err.code, err.message });
            return;
        },
    }

    std.log.warn("received response from client with id '{s}' that has no handler!", .{id});
}

fn formatMessage(message: Message, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (message) {
        .request => |request| try writer.print("request-{f}-{t}", .{ std.json.fmt(request.id, .{}), request.params }),
        .notification => |notification| try writer.print("notification-{t}", .{notification.params}),
        .response => |response| try writer.print("response-{f}", .{std.json.fmt(response.id, .{})}),
    }
}

fn fmtMessage(message: Message) std.fmt.Alt(Message, formatMessage) {
    return .{ .data = message };
}
