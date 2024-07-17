const Server = @This();

const std = @import("std");
const zig_builtin = @import("builtin");
const build_options = @import("build_options");
const Config = @import("Config.zig");
const configuration = @import("configuration.zig");
const DocumentStore = @import("DocumentStore.zig");
const zls = @import("zls");
const lsp = zls.lsp;
const types = zls.types;
const Analyser = @import("analysis.zig");
const offsets = @import("offsets.zig");
const tracy = @import("tracy");
const diff = zls.diff;
const known_folders = @import("known_folders");

const semantic_tokens = @import("features/semantic_tokens.zig");
const diagnostics_gen = @import("features/diagnostics.zig");

const log = std.log.scoped(.kdblint_server);
const message_logger = std.log.scoped(.message);

// public fields
allocator: std.mem.Allocator,
// use updateConfiguration or updateConfiguration2 for setting config options
config: Config = .{},
/// will default to lookup in the system and user configuration folder provided by known-folders.
config_path: ?[]const u8 = null,
document_store: DocumentStore,
transport: ?lsp.AnyTransport = null,
message_tracing: bool = false,
offset_encoding: offsets.Encoding = .@"utf-16",
status: Status = .uninitialized,

// private fields
thread_pool: if (zig_builtin.single_threaded) void else std.Thread.Pool,
wait_group: if (zig_builtin.single_threaded) void else std.Thread.WaitGroup,
job_queue: std.fifo.LinearFifo(Job, .Dynamic),
job_queue_lock: std.Thread.Mutex = .{},
// ip: InternPool = .{},
// ensure that build on save is only executed once at a time
running_build_on_save_processes: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),
/// avoid Zig deadlocking when spawning multiple `zig ast-check` processes at the same time.
/// See https://github.com/ziglang/zig/issues/16369
zig_ast_check_lock: std.Thread.Mutex = .{},
config_arena: std.heap.ArenaAllocator.State = .{},
client_capabilities: ClientCapabilities = .{},

// Code was based off of https://github.com/andersfr/zig-lsp/blob/master/server.zig

const ClientCapabilities = struct {
    supports_snippets: bool = false,
    supports_apply_edits: bool = false,
    supports_will_save: bool = false,
    supports_will_save_wait_until: bool = false,
    supports_publish_diagnostics: bool = false,
    supports_code_action_fixall: bool = false,
    hover_supports_md: bool = false,
    signature_help_supports_md: bool = false,
    completion_doc_supports_md: bool = false,
    supports_completion_insert_replace_support: bool = false,
    /// deprecated can be marked through the `CompletionItem.deprecated` field
    supports_completion_deprecated_old: bool = false,
    /// deprecated can be marked through the `CompletionItem.tags` field
    supports_completion_deprecated_tag: bool = false,
    label_details_support: bool = false,
    supports_configuration: bool = false,
    supports_workspace_did_change_configuration_dynamic_registration: bool = false,
    supports_textDocument_definition_linkSupport: bool = false,
    /// The detail entries for big structs such as std.zig.CrossTarget were
    /// bricking the preview window in Sublime Text.
    /// https://github.com/zigtools/zls/pull/261
    max_detail_length: u32 = 1024 * 1024,
    workspace_folders: []types.URI = &.{},

    fn deinit(self: *ClientCapabilities, allocator: std.mem.Allocator) void {
        for (self.workspace_folders) |uri| allocator.free(uri);
        allocator.free(self.workspace_folders);
        self.* = undefined;
    }
};

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

const Job = union(enum) {
    incoming_message: std.json.Parsed(Message),
    generate_diagnostics: DocumentStore.Uri,

    fn deinit(self: Job, allocator: std.mem.Allocator) void {
        switch (self) {
            .incoming_message => |parsed_message| parsed_message.deinit(),
            .generate_diagnostics => |uri| allocator.free(uri),
        }
    }

    const SynchronizationMode = enum {
        /// this `Job` requires exclusive access to `Server` and `DocumentStore`
        /// all previous jobs will be awaited
        exclusive,
        /// this `Job` requires shared access to `Server` and `DocumentStore`
        /// other non exclusive jobs can be processed in parallel
        shared,
        /// this `Job` operates atomically and does not require any synchronisation
        atomic,
    };

    fn syncMode(self: Job) SynchronizationMode {
        return switch (self) {
            .incoming_message => |parsed_message| if (isBlockingMessage(parsed_message.value)) .exclusive else .shared,
            .generate_diagnostics => .shared,
        };
    }
};

fn sendToClientResponse(server: *Server, id: lsp.JsonRPCMessage.ID, result: anytype) error{OutOfMemory}![]u8 {
    const tracy_zone = tracy.traceNamed(@src(), "sendToClientResponse(" ++ @typeName(@TypeOf(result)) ++ ")");
    defer tracy_zone.end();

    // TODO validate result type is a possible response
    // TODO validate response is from a client to server request
    // TODO validate result type

    return try server.sendToClientInternal(id, null, null, "result", result);
}

fn sendToClientRequest(server: *Server, id: lsp.JsonRPCMessage.ID, method: []const u8, params: anytype) error{OutOfMemory}![]u8 {
    const tracy_zone = tracy.traceNamed(@src(), "sendToClientRequest(" ++ @typeName(@TypeOf(params)) ++ ")");
    defer tracy_zone.end();

    // TODO validate method is a request
    // TODO validate method is server to client
    // TODO validate params type

    return try server.sendToClientInternal(id, method, null, "params", params);
}

fn sendToClientNotification(server: *Server, method: []const u8, params: anytype) error{OutOfMemory}![]u8 {
    const tracy_zone = tracy.traceNamed(@src(), "sendToClientRequest(" ++ @typeName(@TypeOf(params)) ++ ")");
    defer tracy_zone.end();

    // TODO validate method is a notification
    // TODO validate method is server to client
    // TODO validate params type

    return try server.sendToClientInternal(null, method, null, "params", params);
}

fn sendToClientResponseError(server: *Server, id: lsp.JsonRPCMessage.ID, err: ?lsp.JsonRPCMessage.Response.Error) error{OutOfMemory}![]u8 {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    return try server.sendToClientInternal(id, null, err, "", null);
}

fn sendToClientInternal(
    server: *Server,
    maybe_id: ?lsp.JsonRPCMessage.ID,
    maybe_method: ?[]const u8,
    maybe_err: ?lsp.JsonRPCMessage.Response.Error,
    extra_name: []const u8,
    extra: anytype,
) error{OutOfMemory}![]u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    errdefer buffer.deinit(server.allocator);
    var writer = buffer.writer(server.allocator);
    try writer.writeAll(
        \\{"jsonrpc":"2.0"
    );
    if (maybe_id) |id| {
        try writer.writeAll(
            \\,"id":
        );
        try std.json.stringify(id, .{}, writer);
    }
    if (maybe_method) |method| {
        try writer.writeAll(
            \\,"method":
        );
        try std.json.stringify(method, .{}, writer);
    }
    switch (@TypeOf(extra)) {
        void => {},
        ?void => {
            try writer.print(
                \\,"{s}":null
            , .{extra_name});
        },
        else => {
            try writer.print(
                \\,"{s}":
            , .{extra_name});
            try std.json.stringify(extra, .{ .emit_null_optional_fields = false }, writer);
        },
    }
    if (maybe_err) |err| {
        try writer.writeAll(
            \\,"error":
        );
        try std.json.stringify(err, .{}, writer);
    }
    try writer.writeByte('}');

    if (server.transport) |transport| {
        const tracy_zone_transport = tracy.traceNamed(@src(), "Transport.writeJsonMessage");
        defer tracy_zone_transport.end();

        if (server.message_tracing) message_logger.debug("sent: {s}", .{buffer.items});

        transport.writeJsonMessage(buffer.items) catch |err| {
            log.err("failed to write response: {}", .{err});
        };
    }
    return buffer.toOwnedSlice(server.allocator);
}

fn showMessage(
    server: *Server,
    message_type: types.MessageType,
    comptime fmt: []const u8,
    args: anytype,
) void {
    const message = std.fmt.allocPrint(server.allocator, fmt, args) catch return;
    defer server.allocator.free(message);
    switch (message_type) {
        .Error => log.err("{s}", .{message}),
        .Warning => log.warn("{s}", .{message}),
        .Info => log.info("{s}", .{message}),
        .Log, .Debug => log.debug("{s}", .{message}),
        _ => log.debug("{s}", .{message}),
    }
    switch (server.status) {
        .initializing,
        .initialized,
        => {},
        .uninitialized,
        .shutdown,
        .exiting_success,
        .exiting_failure,
        => return,
    }
    if (server.sendToClientNotification("window/showMessage", types.ShowMessageParams{
        .type = message_type,
        .message = message,
    })) |json_message| {
        server.allocator.free(json_message);
    } else |err| {
        log.warn("failed to show message: {}", .{err});
    }
}

fn initAnalyser(server: *Server, handle: ?*DocumentStore.Handle) Analyser {
    return Analyser.init(
        server.allocator,
        &server.document_store,
        &server.ip,
        handle,
        server.config.dangerous_comptime_experiments_do_not_enable,
    );
}

fn initializeHandler(server: *Server, arena: std.mem.Allocator, request: types.InitializeParams) Error!types.InitializeResult {
    var skip_set_fixall = false;

    if (request.clientInfo) |clientInfo| {
        log.info("Client is '{s}-{s}'", .{ clientInfo.name, clientInfo.version orelse "<no version>" });

        if (std.mem.eql(u8, clientInfo.name, "Sublime Text LSP")) {
            server.client_capabilities.max_detail_length = 256;
            // TODO investigate why fixall doesn't work in sublime text
            server.client_capabilities.supports_code_action_fixall = false;
            skip_set_fixall = true;
        } else if (std.mem.eql(u8, clientInfo.name, "Visual Studio Code")) {
            server.client_capabilities.supports_code_action_fixall = true;
            skip_set_fixall = true;
        }
    }

    if (request.capabilities.general) |general| {
        var supports_utf8 = false;
        var supports_utf16 = false;
        var supports_utf32 = false;
        if (general.positionEncodings) |position_encodings| {
            for (position_encodings) |encoding| {
                switch (encoding) {
                    .@"utf-8" => supports_utf8 = true,
                    .@"utf-16" => supports_utf16 = true,
                    .@"utf-32" => supports_utf32 = true,
                    .custom_value => {},
                }
            }
        }

        if (supports_utf8) {
            server.offset_encoding = .@"utf-8";
        } else if (supports_utf32) {
            server.offset_encoding = .@"utf-32";
        } else {
            server.offset_encoding = .@"utf-16";
        }
    }

    if (request.capabilities.textDocument) |textDocument| {
        server.client_capabilities.supports_publish_diagnostics = textDocument.publishDiagnostics != null;
        if (textDocument.hover) |hover| {
            if (hover.contentFormat) |content_format| {
                for (content_format) |format| {
                    if (format == .plaintext) {
                        break;
                    }
                    if (format == .markdown) {
                        server.client_capabilities.hover_supports_md = true;
                        break;
                    }
                }
            }
        }
        if (textDocument.completion) |completion| {
            if (completion.completionItem) |completionItem| {
                server.client_capabilities.label_details_support = completionItem.labelDetailsSupport orelse false;
                server.client_capabilities.supports_snippets = completionItem.snippetSupport orelse false;
                server.client_capabilities.supports_completion_deprecated_old = completionItem.deprecatedSupport orelse false;
                server.client_capabilities.supports_completion_insert_replace_support = completionItem.insertReplaceSupport orelse false;
                if (completionItem.tagSupport) |tagSupport| {
                    for (tagSupport.valueSet) |tag| {
                        switch (tag) {
                            .Deprecated => {
                                server.client_capabilities.supports_completion_deprecated_tag = true;
                                break;
                            },
                            _ => {},
                        }
                    }
                }
                if (completionItem.documentationFormat) |documentation_format| {
                    for (documentation_format) |format| {
                        if (format == .plaintext) {
                            break;
                        }
                        if (format == .markdown) {
                            server.client_capabilities.completion_doc_supports_md = true;
                            break;
                        }
                    }
                }
            }
        }
        if (textDocument.synchronization) |synchronization| {
            server.client_capabilities.supports_will_save = synchronization.willSave orelse false;
            server.client_capabilities.supports_will_save_wait_until = synchronization.willSaveWaitUntil orelse false;
        }
        if (textDocument.codeAction) |codeaction| {
            if (codeaction.codeActionLiteralSupport) |literalSupport| {
                if (!skip_set_fixall) {
                    for (literalSupport.codeActionKind.valueSet) |code_action_kind| {
                        if (code_action_kind.eql(.@"source.fixAll")) {
                            server.client_capabilities.supports_code_action_fixall = true;
                            break;
                        }
                    }
                }
            }
        }
        if (textDocument.definition) |definition| {
            server.client_capabilities.supports_textDocument_definition_linkSupport = definition.linkSupport orelse false;
        }
        if (textDocument.signatureHelp) |signature_help_capabilities| {
            if (signature_help_capabilities.signatureInformation) |signature_information| {
                if (signature_information.documentationFormat) |content_format| {
                    for (content_format) |format| {
                        if (format == .plaintext) {
                            break;
                        }
                        if (format == .markdown) {
                            server.client_capabilities.signature_help_supports_md = true;
                            break;
                        }
                    }
                }
            }
        }
    }

    if (request.capabilities.workspace) |workspace| {
        server.client_capabilities.supports_apply_edits = workspace.applyEdit orelse false;
        server.client_capabilities.supports_configuration = workspace.configuration orelse false;
        if (workspace.didChangeConfiguration) |did_change| {
            if (did_change.dynamicRegistration orelse false) {
                server.client_capabilities.supports_workspace_did_change_configuration_dynamic_registration = true;
            }
        }
    }

    if (request.workspaceFolders) |workspace_folders| {
        server.client_capabilities.workspace_folders = try server.allocator.alloc(types.URI, workspace_folders.len);
        @memset(server.client_capabilities.workspace_folders, "");
        for (server.client_capabilities.workspace_folders, workspace_folders) |*dest, src| {
            dest.* = try server.allocator.dupe(u8, src.uri);
        }
    }

    if (request.trace) |trace| {
        // To support --enable-message-tracing, only allow turning this on here
        if (trace != .off) {
            server.message_tracing = true;
        }
    }

    log.debug("Offset Encoding: {s}", .{@tagName(server.offset_encoding)});

    server.status = .initializing;

    if (request.initializationOptions) |initialization_options| {
        if (std.json.parseFromValueLeaky(Config, arena, initialization_options, .{})) |new_cfg| {
            try server.updateConfiguration2(new_cfg);
        } else |err| {
            log.err("failed to read initialization_options: {}", .{err});
        }
    }

    if (!zig_builtin.is_test) {
        var maybe_config_result = if (server.config_path) |config_path|
            configuration.loadFromFile(server.allocator, config_path)
        else
            configuration.load(server.allocator);

        if (maybe_config_result) |*config_result| {
            defer config_result.deinit(server.allocator);
            switch (config_result.*) {
                .success => |config_with_path| try server.updateConfiguration2(config_with_path.config.value),
                .failure => |payload| blk: {
                    try server.updateConfiguration(.{});
                    const message = try payload.toMessage(server.allocator) orelse break :blk;
                    defer server.allocator.free(message);
                    server.showMessage(.Error, "Failed to load configuration options:\n{s}", .{message});
                },
                .not_found => {
                    log.info("No config file zls.json found. This is not an error.", .{});
                    try server.updateConfiguration(.{});
                },
            }
        } else |err| {
            log.err("failed to load configuration: {}", .{err});
        }
    } else {
        server.updateConfiguration(.{}) catch |err| {
            log.err("failed to load configuration: {}", .{err});
        };
    }

    return .{
        .serverInfo = .{
            .name = "zls",
            .version = build_options.version_string,
        },
        .capabilities = .{
            .positionEncoding = switch (server.offset_encoding) {
                .@"utf-8" => .@"utf-8",
                .@"utf-16" => .@"utf-16",
                .@"utf-32" => .@"utf-32",
            },
            .signatureHelpProvider = .{
                .triggerCharacters = &.{"("},
                .retriggerCharacters = &.{","},
            },
            .textDocumentSync = .{
                .TextDocumentSyncOptions = .{
                    .openClose = true,
                    .change = .Incremental,
                    .save = .{ .bool = true },
                    .willSave = true,
                    .willSaveWaitUntil = true,
                },
            },
            .renameProvider = .{ .bool = true },
            .completionProvider = .{
                .resolveProvider = false,
                .triggerCharacters = &[_][]const u8{ ".", ":", "@", "]", "/" },
                .completionItem = .{ .labelDetailsSupport = true },
            },
            .documentHighlightProvider = .{ .bool = true },
            .hoverProvider = .{ .bool = true },
            .codeActionProvider = .{ .bool = true },
            .declarationProvider = .{ .bool = true },
            .definitionProvider = .{ .bool = true },
            .typeDefinitionProvider = .{ .bool = true },
            .implementationProvider = .{ .bool = false },
            .referencesProvider = .{ .bool = true },
            .documentSymbolProvider = .{ .bool = true },
            .colorProvider = .{ .bool = false },
            .documentFormattingProvider = .{ .bool = true },
            .documentRangeFormattingProvider = .{ .bool = false },
            .foldingRangeProvider = .{ .bool = true },
            .selectionRangeProvider = .{ .bool = true },
            .workspaceSymbolProvider = .{ .bool = false },
            .workspace = .{
                .workspaceFolders = .{
                    .supported = true,
                    .changeNotifications = .{ .bool = true },
                },
            },
            .semanticTokensProvider = .{
                .SemanticTokensOptions = .{
                    .full = .{ .bool = true },
                    .range = .{ .bool = true },
                    .legend = .{
                        .tokenTypes = std.meta.fieldNames(semantic_tokens.TokenType),
                        .tokenModifiers = std.meta.fieldNames(semantic_tokens.TokenModifiers),
                    },
                },
            },
            .inlayHintProvider = .{ .bool = true },
        },
    };
}

fn initializedHandler(server: *Server, _: std.mem.Allocator, notification: types.InitializedParams) Error!void {
    _ = notification;

    if (server.status != .initializing) {
        log.warn("received a initialized notification but the server has not send a initialize request!", .{});
    }

    server.status = .initialized;

    if (server.client_capabilities.supports_workspace_did_change_configuration_dynamic_registration) {
        try server.registerCapability("workspace/didChangeConfiguration");
    }

    if (server.client_capabilities.supports_configuration)
        try server.requestConfiguration();

    if (std.crypto.random.intRangeLessThan(usize, 0, 32768) == 0) {
        server.showMessage(.Warning, "HELP ME, I AM STUCK INSIDE AN LSP!", .{});
    }
}

fn shutdownHandler(server: *Server, _: std.mem.Allocator, _: void) Error!?void {
    defer server.status = .shutdown;
    if (server.status != .initialized) return error.InvalidRequest; // received a shutdown request but the server is not initialized!
}

fn exitHandler(server: *Server, _: std.mem.Allocator, _: void) Error!void {
    server.status = switch (server.status) {
        .initialized => .exiting_failure,
        .shutdown => .exiting_success,
        else => unreachable,
    };
}

fn cancelRequestHandler(server: *Server, _: std.mem.Allocator, request: types.CancelParams) Error!void {
    _ = server;
    _ = request;
    // TODO implement $/cancelRequest
}

fn setTraceHandler(server: *Server, _: std.mem.Allocator, request: types.SetTraceParams) Error!void {
    server.message_tracing = request.value != .off;
}

fn registerCapability(server: *Server, method: []const u8) Error!void {
    const id = try std.fmt.allocPrint(server.allocator, "register-{s}", .{method});
    defer server.allocator.free(id);

    log.debug("Dynamically registering method '{s}'", .{method});

    const json_message = try server.sendToClientRequest(
        .{ .string = id },
        "client/registerCapability",
        types.RegistrationParams{ .registrations = &.{
            types.Registration{
                .id = id,
                .method = method,
            },
        } },
    );
    server.allocator.free(json_message);
}

fn requestConfiguration(server: *Server) Error!void {
    const configuration_items = comptime config: {
        var comp_config: [std.meta.fields(Config).len]types.ConfigurationItem = undefined;
        for (std.meta.fields(Config), 0..) |field, index| {
            comp_config[index] = .{
                .section = "kdblint." ++ field.name,
            };
        }

        break :config comp_config;
    };

    const json_message = try server.sendToClientRequest(
        .{ .string = "kdblint_configuration" },
        "workspace/configuration",
        types.ConfigurationParams{
            .items = &configuration_items,
        },
    );
    server.allocator.free(json_message);
}

fn handleConfiguration(server: *Server, json: std.json.Value) error{OutOfMemory}!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const fields = std.meta.fields(configuration.Configuration);
    const result = switch (json) {
        .array => |arr| if (arr.items.len == fields.len) arr.items else {
            log.err("workspace/configuration expects an array of size {d} but received {d}", .{ fields.len, arr.items.len });
            return;
        },
        else => {
            log.err("workspace/configuration expects an array but received {s}", .{@tagName(json)});
            return;
        },
    };

    var arena_allocator = std.heap.ArenaAllocator.init(server.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var new_config: configuration.Configuration = .{};

    inline for (fields, result) |field, json_value| {
        const maybe_new_value = std.json.parseFromValueLeaky(field.type, arena, json_value, .{}) catch |err| blk: {
            log.err("failed to parse configuration option '{s}': {}", .{ field.name, err });
            break :blk null;
        };
        if (maybe_new_value) |new_value| {
            @field(new_config, field.name) = new_value;
        }
    }

    server.updateConfiguration(new_config) catch |err| {
        log.err("failed to update configuration: {}", .{err});
    };
}

fn didChangeWorkspaceFoldersHandler(server: *Server, arena: std.mem.Allocator, notification: types.DidChangeWorkspaceFoldersParams) Error!void {
    _ = arena;

    var folders = std.ArrayListUnmanaged(types.URI).fromOwnedSlice(server.client_capabilities.workspace_folders);
    errdefer folders.deinit(server.allocator);

    var i: usize = 0;
    while (i < folders.items.len) {
        const uri = folders.items[i];
        for (notification.event.removed) |removed| {
            if (std.mem.eql(u8, removed.uri, uri)) {
                server.allocator.free(folders.swapRemove(i));
                break;
            }
        } else {
            i += 1;
        }
    }

    try folders.ensureUnusedCapacity(server.allocator, notification.event.added.len);
    for (notification.event.added) |added| {
        folders.appendAssumeCapacity(try server.allocator.dupe(u8, added.uri));
    }

    server.client_capabilities.workspace_folders = try folders.toOwnedSlice(server.allocator);
}

fn didChangeConfigurationHandler(server: *Server, arena: std.mem.Allocator, notification: types.DidChangeConfigurationParams) Error!void {
    const settings = switch (notification.settings) {
        .null => {
            if (server.client_capabilities.supports_configuration) {
                try server.requestConfiguration();
            }
            return;
        },
        .object => |object| object.get("kdblint") orelse notification.settings,
        else => notification.settings,
    };

    const new_config = std.json.parseFromValueLeaky(
        configuration.Configuration,
        arena,
        settings,
        .{ .ignore_unknown_fields = true },
    ) catch |err| {
        log.err("failed to parse 'workspace/didChangeConfiguration' response: {}", .{err});
        return error.ParseError;
    };

    try server.updateConfiguration(new_config);
}

pub fn updateConfiguration2(server: *Server, new_config: Config) error{OutOfMemory}!void {
    var cfg: configuration.Configuration = .{};
    inline for (std.meta.fields(Config)) |field| {
        @field(cfg, field.name) = @field(new_config, field.name);
    }
    try server.updateConfiguration(cfg);
}

pub fn updateConfiguration(server: *Server, new_config: configuration.Configuration) error{OutOfMemory}!void {
    // NOTE every changed configuration will increase the amount of memory allocated by the arena
    // This is unlikely to cause any big issues since the user is probably not going set settings
    // often in one session
    var config_arena_allocator = server.config_arena.promote(server.allocator);
    defer server.config_arena = config_arena_allocator.state;
    const config_arena = config_arena_allocator.allocator();

    var new_cfg: configuration.Configuration = .{};
    inline for (std.meta.fields(Config)) |field| {
        @field(new_cfg, field.name) = if (@field(new_config, field.name)) |new_value| new_value else @field(server.config, field.name);
    }

    server.validateConfiguration(&new_cfg);

    // <---------------------------------------------------------->
    //                        apply changes
    // <---------------------------------------------------------->

    const new_kdb_version =
        new_config.kdb_version != null and
        server.config.kdb_version != new_config.kdb_version;

    inline for (std.meta.fields(Config)) |field| {
        if (@field(new_cfg, field.name)) |new_config_value| {
            const old_config_value = @field(server.config, field.name);
            switch (@TypeOf(old_config_value)) {
                ?[]const u8 => {
                    const override_old_value =
                        if (old_config_value) |old_value| !std.mem.eql(u8, old_value, new_config_value) else true;
                    if (override_old_value) {
                        log.info("Set config option '{s}' to '{s}'", .{ field.name, new_config_value });
                        @field(server.config, field.name) = try config_arena.dupe(u8, new_config_value);
                    }
                },
                []const u8 => {
                    if (!std.mem.eql(u8, old_config_value, new_config_value)) {
                        log.info("Set config option '{s}' to '{s}'", .{ field.name, new_config_value });
                        @field(server.config, field.name) = try config_arena.dupe(u8, new_config_value);
                    }
                },
                else => {
                    if (old_config_value != new_config_value) {
                        switch (@typeInfo(@TypeOf(new_config_value))) {
                            .Bool,
                            .Int,
                            .Float,
                            => log.info("Set config option '{s}' to '{}'", .{ field.name, new_config_value }),
                            .Enum => log.info("Set config option '{s}' to '{s}'", .{ field.name, @tagName(new_config_value) }),
                            else => @compileError("unexpected config type ++ (" ++ @typeName(@TypeOf(new_config_value)) ++ ")"),
                        }
                        @field(server.config, field.name) = new_config_value;
                    }
                },
            }
        }
    }

    server.document_store.config = server.config;

    if (server.status == .initialized) {
        if (new_kdb_version and server.client_capabilities.supports_publish_diagnostics) {
            for (server.document_store.handles.values()) |handle| {
                if (!handle.isOpen()) continue;
                try server.pushJob(.{ .generate_diagnostics = try server.allocator.dupe(u8, handle.uri) });
            }
        }

        const json_message = try server.sendToClientRequest(
            .{ .string = "semantic_tokens_refresh" },
            "workspace/semanticTokens/refresh",
            {},
        );
        server.allocator.free(json_message);
    }

    // <---------------------------------------------------------->
    //  don't modify config options after here, only show messages
    // <---------------------------------------------------------->
}

fn validateConfiguration(server: *Server, config: *configuration.Configuration) void {
    inline for (comptime std.meta.fieldNames(Config)) |field_name| {
        const FileCheckInfo = struct {
            kind: enum { file, directory },
            is_accessible: bool,
        };

        @setEvalBranchQuota(2_000);
        const file_info: FileCheckInfo = comptime if (std.mem.indexOf(u8, field_name, "path") != null) blk: {
            if (std.mem.eql(u8, field_name, "zig_exe_path") or
                std.mem.eql(u8, field_name, "builtin_path") or
                std.mem.eql(u8, field_name, "build_runner_path"))
            {
                break :blk .{ .kind = .file, .is_accessible = true };
            } else if (std.mem.eql(u8, field_name, "zig_lib_path")) {
                break :blk .{ .kind = .directory, .is_accessible = true };
            } else if (std.mem.eql(u8, field_name, "global_cache_path")) {
                break :blk .{ .kind = .directory, .is_accessible = false };
            } else {
                @compileError(std.fmt.comptimePrint(
                    \\config option '{s}' contains the word 'path'.
                    \\Please add config option validation checks above if necessary.
                    \\If not necessary, just add a continue switch-case to ignore this error.
                    \\
                , .{field_name}));
            }
        } else continue;

        const is_ok = if (@field(config, field_name)) |path| ok: {
            if (path.len == 0) break :ok false;

            if (!std.fs.path.isAbsolute(path)) {
                server.showMessage(.Warning, "config option '{s}': expected absolute path but got '{s}'", .{ field_name, path });
                break :ok false;
            }

            switch (file_info.kind) {
                .file => {
                    const file = std.fs.openFileAbsolute(path, .{}) catch |err| {
                        if (file_info.is_accessible) {
                            server.showMessage(.Warning, "config option '{s}': invalid file path '{s}': {}", .{ field_name, path, err });
                            break :ok false;
                        }
                        break :ok true;
                    };
                    defer file.close();

                    const stat = file.stat() catch |err| {
                        log.err("failed to get stat of '{s}': {}", .{ path, err });
                        break :ok true;
                    };
                    switch (stat.kind) {
                        .directory => {
                            server.showMessage(.Warning, "config option '{s}': expected file path but '{s}' is a directory", .{ field_name, path });
                            break :ok false;
                        },
                        .file => {},
                        // are there file kinds that should warn?
                        // what about symlinks?
                        else => {},
                    }
                    break :ok true;
                },
                .directory => {
                    var dir = std.fs.openDirAbsolute(path, .{}) catch |err| {
                        if (file_info.is_accessible) {
                            server.showMessage(.Warning, "config option '{s}': invalid directory path '{s}': {}", .{ field_name, path, err });
                            break :ok false;
                        }
                        break :ok true;
                    };
                    defer dir.close();
                    const stat = dir.stat() catch |err| {
                        log.err("failed to get stat of '{s}': {}", .{ path, err });
                        break :ok true;
                    };
                    switch (stat.kind) {
                        .file => {
                            server.showMessage(.Warning, "config option '{s}': expected directory path but '{s}' is a file", .{ field_name, path });
                            break :ok false;
                        },
                        .directory => {},
                        // are there file kinds that should warn?
                        // what about symlinks?
                        else => {},
                    }
                    break :ok true;
                },
            }
        } else true;

        if (!is_ok) {
            @field(config, field_name) = null;
        }
    }
}

fn openDocumentHandler(server: *Server, _: std.mem.Allocator, notification: types.DidOpenTextDocumentParams) Error!void {
    if (notification.textDocument.text.len > DocumentStore.max_document_size) {
        log.err("open document `{s}` failed: text size ({d}) is above maximum length ({d})", .{
            notification.textDocument.uri,
            notification.textDocument.text.len,
            DocumentStore.max_document_size,
        });
        return error.InternalError;
    }

    try server.document_store.openDocument(notification.textDocument.uri, notification.textDocument.text);

    if (server.client_capabilities.supports_publish_diagnostics) {
        try server.pushJob(.{
            .generate_diagnostics = try server.allocator.dupe(u8, notification.textDocument.uri),
        });
    }
}

fn changeDocumentHandler(server: *Server, _: std.mem.Allocator, notification: types.DidChangeTextDocumentParams) Error!void {
    const handle = server.document_store.getHandle(notification.textDocument.uri) orelse return;

    const new_text = try diff.applyContentChanges(server.allocator, handle.tree.source, notification.contentChanges, server.offset_encoding);

    if (new_text.len > DocumentStore.max_document_size) {
        log.err("change document `{s}` failed: text size ({d}) is above maximum length ({d})", .{
            notification.textDocument.uri,
            new_text.len,
            DocumentStore.max_document_size,
        });
        return error.InternalError;
    }

    try server.document_store.refreshDocument(handle.uri, new_text);

    if (server.client_capabilities.supports_publish_diagnostics) {
        try server.pushJob(.{
            .generate_diagnostics = try server.allocator.dupe(u8, handle.uri),
        });
    }
}

fn saveDocumentHandler(server: *Server, arena: std.mem.Allocator, notification: types.DidSaveTextDocumentParams) Error!void {
    _ = server; // autofix
    _ = arena; // autofix
    _ = notification; // autofix
}

fn closeDocumentHandler(server: *Server, _: std.mem.Allocator, notification: types.DidCloseTextDocumentParams) error{}!void {
    server.document_store.closeDocument(notification.textDocument.uri);

    if (server.client_capabilities.supports_publish_diagnostics) {
        // clear diagnostics on closed file
        const json_message = server.sendToClientNotification("textDocument/publishDiagnostics", .{
            .uri = notification.textDocument.uri,
            .diagnostics = &.{},
        }) catch return;
        server.allocator.free(json_message);
    }
}

fn willSaveWaitUntilHandler(server: *Server, arena: std.mem.Allocator, request: types.WillSaveTextDocumentParams) Error!?[]types.TextEdit {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn semanticTokensFullHandler(server: *Server, arena: std.mem.Allocator, request: types.SemanticTokensParams) Error!?types.SemanticTokens {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn semanticTokensRangeHandler(server: *Server, arena: std.mem.Allocator, request: types.SemanticTokensRangeParams) Error!?types.SemanticTokens {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn completionHandler(server: *Server, arena: std.mem.Allocator, request: types.CompletionParams) Error!lsp.ResultType("textDocument/completion") {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn signatureHelpHandler(server: *Server, arena: std.mem.Allocator, request: types.SignatureHelpParams) Error!?types.SignatureHelp {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn gotoDefinitionHandler(
    server: *Server,
    arena: std.mem.Allocator,
    request: types.DefinitionParams,
) Error!lsp.ResultType("textDocument/definition") {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn gotoTypeDefinitionHandler(server: *Server, arena: std.mem.Allocator, request: types.TypeDefinitionParams) Error!lsp.ResultType("textDocument/typeDefinition") {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn gotoImplementationHandler(server: *Server, arena: std.mem.Allocator, request: types.ImplementationParams) Error!lsp.ResultType("textDocument/implementation") {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn gotoDeclarationHandler(server: *Server, arena: std.mem.Allocator, request: types.DeclarationParams) Error!lsp.ResultType("textDocument/declaration") {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn hoverHandler(server: *Server, arena: std.mem.Allocator, request: types.HoverParams) Error!?types.Hover {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn documentSymbolsHandler(server: *Server, arena: std.mem.Allocator, request: types.DocumentSymbolParams) Error!lsp.ResultType("textDocument/documentSymbol") {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn formattingHandler(server: *Server, arena: std.mem.Allocator, request: types.DocumentFormattingParams) Error!?[]types.TextEdit {
    const handle = server.document_store.getHandle(request.textDocument.uri) orelse return null;

    if (handle.tree.errors.len != 0) return null;

    // TODO: formatting settings
    const formatted = try handle.tree.render(arena, .{});

    if (std.mem.eql(u8, handle.tree.source, formatted)) return null;

    const text_edits = try diff.edits(arena, handle.tree.source, formatted, server.offset_encoding);
    return text_edits.items;
}

fn renameHandler(server: *Server, arena: std.mem.Allocator, request: types.RenameParams) Error!?types.WorkspaceEdit {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn referencesHandler(server: *Server, arena: std.mem.Allocator, request: types.ReferenceParams) Error!?[]types.Location {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn documentHighlightHandler(server: *Server, arena: std.mem.Allocator, request: types.DocumentHighlightParams) Error!?[]types.DocumentHighlight {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn inlayHintHandler(server: *Server, arena: std.mem.Allocator, request: types.InlayHintParams) Error!?[]types.InlayHint {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn codeActionHandler(server: *Server, arena: std.mem.Allocator, request: types.CodeActionParams) Error!lsp.ResultType("textDocument/codeAction") {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn foldingRangeHandler(server: *Server, arena: std.mem.Allocator, request: types.FoldingRangeParams) Error!?[]types.FoldingRange {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

fn selectionRangeHandler(server: *Server, arena: std.mem.Allocator, request: types.SelectionRangeParams) Error!?[]types.SelectionRange {
    _ = server; // autofix
    _ = arena; // autofix
    _ = request; // autofix
    return null;
}

const HandledRequestParams = union(enum) {
    initialize: types.InitializeParams,
    shutdown,
    @"textDocument/willSaveWaitUntil": types.WillSaveTextDocumentParams,
    @"textDocument/semanticTokens/full": types.SemanticTokensParams,
    @"textDocument/semanticTokens/range": types.SemanticTokensRangeParams,
    @"textDocument/inlayHint": types.InlayHintParams,
    @"textDocument/completion": types.CompletionParams,
    @"textDocument/signatureHelp": types.SignatureHelpParams,
    @"textDocument/definition": types.DefinitionParams,
    @"textDocument/typeDefinition": types.TypeDefinitionParams,
    @"textDocument/implementation": types.ImplementationParams,
    @"textDocument/declaration": types.DeclarationParams,
    @"textDocument/hover": types.HoverParams,
    @"textDocument/documentSymbol": types.DocumentSymbolParams,
    @"textDocument/formatting": types.DocumentFormattingParams,
    @"textDocument/rename": types.RenameParams,
    @"textDocument/references": types.ReferenceParams,
    @"textDocument/documentHighlight": types.DocumentHighlightParams,
    @"textDocument/codeAction": types.CodeActionParams,
    @"textDocument/foldingRange": types.FoldingRangeParams,
    @"textDocument/selectionRange": types.SelectionRangeParams,
    other: lsp.MethodWithParams,
};

const HandledNotificationParams = union(enum) {
    initialized: types.InitializedParams,
    exit,
    @"$/cancelRequest": types.CancelParams,
    @"$/setTrace": types.SetTraceParams,
    @"textDocument/didOpen": types.DidOpenTextDocumentParams,
    @"textDocument/didChange": types.DidChangeTextDocumentParams,
    @"textDocument/didSave": types.DidSaveTextDocumentParams,
    @"textDocument/didClose": types.DidCloseTextDocumentParams,
    @"workspace/didChangeWorkspaceFolders": types.DidChangeWorkspaceFoldersParams,
    @"workspace/didChangeConfiguration": types.DidChangeConfigurationParams,
    other: lsp.MethodWithParams,
};

const Message = lsp.Message(.{
    .RequestParams = HandledRequestParams,
    .NotificationParams = HandledNotificationParams,
});

fn isBlockingMessage(msg: Message) bool {
    switch (msg) {
        .request => |request| switch (request.params) {
            .initialize,
            .shutdown,
            => return true,
            .@"textDocument/willSaveWaitUntil",
            .@"textDocument/semanticTokens/full",
            .@"textDocument/semanticTokens/range",
            .@"textDocument/inlayHint",
            .@"textDocument/completion",
            .@"textDocument/signatureHelp",
            .@"textDocument/definition",
            .@"textDocument/typeDefinition",
            .@"textDocument/implementation",
            .@"textDocument/declaration",
            .@"textDocument/hover",
            .@"textDocument/documentSymbol",
            .@"textDocument/formatting",
            .@"textDocument/rename",
            .@"textDocument/references",
            .@"textDocument/documentHighlight",
            .@"textDocument/codeAction",
            .@"textDocument/foldingRange",
            .@"textDocument/selectionRange",
            => return false,
            .other => return false,
        },
        .notification => |notification| switch (notification.params) {
            .@"$/cancelRequest" => return false,
            .initialized,
            .exit,
            .@"$/setTrace",
            .@"textDocument/didOpen",
            .@"textDocument/didChange",
            .@"textDocument/didSave",
            .@"textDocument/didClose",
            .@"workspace/didChangeWorkspaceFolders",
            .@"workspace/didChangeConfiguration",
            => return true,
            .other => return false,
        },
        .response => return true,
    }
}

/// make sure to also set the `transport` field
pub fn create(allocator: std.mem.Allocator) !*Server {
    const server = try allocator.create(Server);
    errdefer server.destroy();
    server.* = Server{
        .allocator = allocator,
        .config = .{},
        .document_store = .{
            .allocator = allocator,
            .config = Config{},
            .thread_pool = if (zig_builtin.single_threaded) {} else undefined, // set below
        },
        .job_queue = std.fifo.LinearFifo(Job, .Dynamic).init(allocator),
        .thread_pool = undefined, // set below
        .wait_group = if (zig_builtin.single_threaded) {} else .{},
    };

    if (zig_builtin.single_threaded) {
        server.thread_pool = {};
    } else {
        try server.thread_pool.init(.{
            .allocator = allocator,
            .n_jobs = 4, // what is a good value here?
        });
        server.document_store.thread_pool = &server.thread_pool;
    }

    // server.ip = try InternPool.init(allocator);

    return server;
}

pub fn destroy(server: *Server) void {
    if (!zig_builtin.single_threaded) {
        server.wait_group.wait();
        server.thread_pool.deinit();
    }

    while (server.job_queue.readItem()) |job| job.deinit(server.allocator);
    server.job_queue.deinit();
    server.document_store.deinit();
    // server.ip.deinit(server.allocator);
    server.client_capabilities.deinit(server.allocator);
    server.config_arena.promote(server.allocator).deinit();
    server.allocator.destroy(server);
}

pub fn keepRunning(server: Server) bool {
    switch (server.status) {
        .exiting_success, .exiting_failure => return false,
        else => return true,
    }
}

pub fn waitAndWork(server: *Server) void {
    if (zig_builtin.single_threaded) return;
    server.thread_pool.waitAndWork(&server.wait_group);
    server.wait_group.reset();
}

pub fn loop(server: *Server) !void {
    std.debug.assert(server.transport != null);
    while (server.keepRunning()) {
        const json_message = try server.transport.?.readJsonMessage(server.allocator);
        defer server.allocator.free(json_message);

        if (server.message_tracing) message_logger.debug("received: {s}", .{json_message});
        try server.sendJsonMessage(json_message);

        while (server.job_queue.readItem()) |job| {
            if (zig_builtin.single_threaded) {
                server.processJob(job, null);
                continue;
            }

            switch (job.syncMode()) {
                .exclusive => {
                    server.waitAndWork();
                    server.processJob(job, null);
                },
                .shared => {
                    server.wait_group.start();
                    errdefer job.deinit(server.allocator);
                    try server.thread_pool.spawn(processJob, .{ server, job, &server.wait_group });
                },
                .atomic => {
                    errdefer job.deinit(server.allocator);
                    try server.thread_pool.spawn(processJob, .{ server, job, null });
                },
            }
        }
    }
}

pub fn sendJsonMessage(server: *Server, json_message: []const u8) Error!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const parsed_message = Message.parseFromSlice(
        server.allocator,
        json_message,
        .{ .ignore_unknown_fields = true, .max_value_len = null, .allocate = .alloc_always },
    ) catch return error.ParseError;
    try server.pushJob(.{ .incoming_message = parsed_message });
}

pub fn sendJsonMessageSync(server: *Server, json_message: []const u8) Error!?[]u8 {
    const parsed_message = Message.parseFromSlice(
        server.allocator,
        json_message,
        .{ .ignore_unknown_fields = true, .max_value_len = null, .allocate = .alloc_always },
    ) catch return error.ParseError;
    defer parsed_message.deinit();
    return try server.processMessage(parsed_message.value);
}

pub fn sendRequestSync(server: *Server, arena: std.mem.Allocator, comptime method: []const u8, params: lsp.ParamsType(method)) Error!lsp.ResultType(method) {
    comptime std.debug.assert(lsp.isRequestMethod(method));
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();
    tracy_zone.setName(method);

    return switch (comptime std.meta.stringToEnum(std.meta.Tag(HandledRequestParams), method) orelse return null) {
        .initialize => try server.initializeHandler(arena, params),
        .shutdown => try server.shutdownHandler(arena, params),
        .@"textDocument/willSaveWaitUntil" => try server.willSaveWaitUntilHandler(arena, params),
        .@"textDocument/semanticTokens/full" => try server.semanticTokensFullHandler(arena, params),
        .@"textDocument/semanticTokens/range" => try server.semanticTokensRangeHandler(arena, params),
        .@"textDocument/inlayHint" => try server.inlayHintHandler(arena, params),
        .@"textDocument/completion" => try server.completionHandler(arena, params),
        .@"textDocument/signatureHelp" => try server.signatureHelpHandler(arena, params),
        .@"textDocument/definition" => try server.gotoDefinitionHandler(arena, params),
        .@"textDocument/typeDefinition" => try server.gotoTypeDefinitionHandler(arena, params),
        .@"textDocument/implementation" => try server.gotoImplementationHandler(arena, params),
        .@"textDocument/declaration" => try server.gotoDeclarationHandler(arena, params),
        .@"textDocument/hover" => try server.hoverHandler(arena, params),
        .@"textDocument/documentSymbol" => try server.documentSymbolsHandler(arena, params),
        .@"textDocument/formatting" => try server.formattingHandler(arena, params),
        .@"textDocument/rename" => try server.renameHandler(arena, params),
        .@"textDocument/references" => try server.referencesHandler(arena, params),
        .@"textDocument/documentHighlight" => try server.documentHighlightHandler(arena, params),
        .@"textDocument/codeAction" => try server.codeActionHandler(arena, params),
        .@"textDocument/foldingRange" => try server.foldingRangeHandler(arena, params),
        .@"textDocument/selectionRange" => try server.selectionRangeHandler(arena, params),
        .other => return null,
    };
}

pub fn sendNotificationSync(server: *Server, arena: std.mem.Allocator, comptime method: []const u8, params: lsp.ParamsType(method)) Error!void {
    comptime std.debug.assert(lsp.isNotificationMethod(method));
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();
    tracy_zone.setName(method);

    return switch (comptime std.meta.stringToEnum(std.meta.Tag(HandledNotificationParams), method) orelse return) {
        .initialized => try server.initializedHandler(arena, params),
        .exit => try server.exitHandler(arena, params),
        .@"$/cancelRequest" => try server.cancelRequestHandler(arena, params),
        .@"$/setTrace" => try server.setTraceHandler(arena, params),
        .@"textDocument/didOpen" => try server.openDocumentHandler(arena, params),
        .@"textDocument/didChange" => try server.changeDocumentHandler(arena, params),
        .@"textDocument/didSave" => try server.saveDocumentHandler(arena, params),
        .@"textDocument/didClose" => try server.closeDocumentHandler(arena, params),
        .@"workspace/didChangeWorkspaceFolders" => try server.didChangeWorkspaceFoldersHandler(arena, params),
        .@"workspace/didChangeConfiguration" => try server.didChangeConfigurationHandler(arena, params),
        .other => {},
    };
}

pub fn sendMessageSync(server: *Server, arena: std.mem.Allocator, comptime method: []const u8, params: lsp.ParamsType(method)) Error!lsp.ResultType(method) {
    comptime std.debug.assert(lsp.isRequestMethod(method) or lsp.isNotificationMethod(method));

    if (comptime lsp.isRequestMethod(method)) {
        return try server.sendRequestSync(arena, method, params);
    } else if (comptime lsp.isNotificationMethod(method)) {
        return try server.sendNotificationSync(arena, method, params);
    } else unreachable;
}

fn processMessage(server: *Server, message: Message) Error!?[]u8 {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    var timer = std.time.Timer.start() catch null;
    defer if (timer) |*t| {
        const total_time = @divFloor(t.read(), std.time.ns_per_ms);
        if (zig_builtin.single_threaded) {
            log.debug("Took {d}ms to process {}", .{ total_time, fmtMessage(message) });
        } else {
            const thread_id = std.Thread.getCurrentId();
            log.debug("Took {d}ms to process {} on Thread {d}", .{ total_time, fmtMessage(message), thread_id });
        }
    };

    try server.validateMessage(message);

    var arena_allocator = std.heap.ArenaAllocator.init(server.allocator);
    defer arena_allocator.deinit();

    switch (message) {
        .request => |request| switch (request.params) {
            .other => return try server.sendToClientResponse(request.id, null),
            inline else => |params, method| {
                const result = try server.sendRequestSync(arena_allocator.allocator(), @tagName(method), params);
                return try server.sendToClientResponse(request.id, result);
            },
        },
        .notification => |notification| switch (notification.params) {
            .other => {},
            inline else => |params, method| try server.sendNotificationSync(arena_allocator.allocator(), @tagName(method), params),
        },
        .response => |response| try server.handleResponse(response),
    }
    return null;
}

fn processMessageReportError(server: *Server, message: Message) ?[]const u8 {
    return server.processMessage(message) catch |err| {
        log.err("failed to process {}: {}", .{ fmtMessage(message), err });
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }

        switch (message) {
            .request => |request| return server.sendToClientResponseError(request.id, lsp.JsonRPCMessage.Response.Error{
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
            }) catch null,
            .notification, .response => return null,
        }
    };
}

fn processJob(server: *Server, job: Job, wait_group: ?*std.Thread.WaitGroup) void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();
    tracy_zone.setName(@tagName(job));
    defer if (!zig_builtin.single_threaded and wait_group != null) wait_group.?.finish();

    defer job.deinit(server.allocator);

    switch (job) {
        .incoming_message => |parsed_message| {
            const response = server.processMessageReportError(parsed_message.value) orelse return;
            server.allocator.free(response);
        },
        .generate_diagnostics => |uri| {
            const handle = server.document_store.getHandle(uri) orelse return;
            var arena_allocator = std.heap.ArenaAllocator.init(server.allocator);
            defer arena_allocator.deinit();
            const diagnostics = diagnostics_gen.generateDiagnostics(server, arena_allocator.allocator(), handle) catch return;
            const json_message = server.sendToClientNotification("textDocument/publishDiagnostics", diagnostics) catch return;
            server.allocator.free(json_message);
        },
    }
}

fn validateMessage(server: *const Server, message: Message) Error!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

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

fn handleResponse(server: *Server, response: lsp.JsonRPCMessage.Response) Error!void {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    if (response.id == null) {
        log.warn("received response from client without id!", .{});
        return;
    }

    const id: []const u8 = switch (response.id.?) {
        .string => |id| id,
        .number => |id| {
            log.warn("received response from client with id '{d}' that has no handler!", .{id});
            return;
        },
    };

    const result = switch (response.result_or_error) {
        .result => |result| result,
        .@"error" => |err| {
            log.err("Error response for '{s}': {}, {s}", .{ id, err.code, err.message });
            return;
        },
    };

    if (std.mem.eql(u8, id, "semantic_tokens_refresh")) {
        //
    } else if (std.mem.startsWith(u8, id, "register")) {
        //
    } else if (std.mem.eql(u8, id, "apply_edit")) {
        //
    } else if (std.mem.eql(u8, id, "kdblint_configuration")) {
        try server.handleConfiguration(result orelse .null);
    } else {
        log.warn("received response from client with id '{s}' that has no handler!", .{id});
    }
}

/// takes ownership of `job`
fn pushJob(server: *Server, job: Job) error{OutOfMemory}!void {
    server.job_queue_lock.lock();
    defer server.job_queue_lock.unlock();
    server.job_queue.writeItem(job) catch |err| {
        job.deinit(server.allocator);
        return err;
    };
}

pub fn formatMessage(
    message: Message,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = options;
    if (fmt.len != 0) std.fmt.invalidFmtError(fmt, message);
    switch (message) {
        .request => |request| try writer.print("request-{}-{s}", .{ std.json.fmt(request.id, .{}), @tagName(request.params) }),
        .notification => |notification| try writer.print("notification-{s}", .{@tagName(notification.params)}),
        .response => |response| try writer.print("response-{?}", .{std.json.fmt(response.id, .{})}),
    }
}

fn fmtMessage(message: Message) std.fmt.Formatter(formatMessage) {
    return .{ .data = message };
}
