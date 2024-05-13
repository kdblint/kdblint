const std = @import("std");
const builtin = @import("builtin");

const tracy = @import("tracy");
const known_folders = @import("known_folders");

const Config = @import("Config.zig");
const offsets = @import("offsets.zig");

const logger = std.log.scoped(.kdblint_config);

pub fn getLocalConfigPath(allocator: std.mem.Allocator) known_folders.Error!?[]const u8 {
    const folder_path = try known_folders.getPath(allocator, .local_configuration) orelse return null;
    defer allocator.free(folder_path);
    return try std.fs.path.join(allocator, &.{ folder_path, "kdblint.json" });
}

pub fn getGlobalConfigPath(allocator: std.mem.Allocator) known_folders.Error!?[]const u8 {
    const folder_path = try known_folders.getPath(allocator, .global_configuration) orelse return null;
    defer allocator.free(folder_path);
    return try std.fs.path.join(allocator, &.{ folder_path, "kdblint.json" });
}

pub fn load(allocator: std.mem.Allocator) error{OutOfMemory}!LoadConfigResult {
    const local_config_path = getLocalConfigPath(allocator) catch |err| blk: {
        logger.warn("failed to resolve local configuration path: {}", .{err});
        break :blk null;
    };
    defer if (local_config_path) |path| allocator.free(path);

    const global_config_path = getGlobalConfigPath(allocator) catch |err| blk: {
        logger.warn("failed to resolve global configuration path: {}", .{err});
        break :blk null;
    };
    defer if (global_config_path) |path| allocator.free(path);

    for ([_]?[]const u8{ local_config_path, global_config_path }) |config_path| {
        const result = try loadFromFile(allocator, config_path orelse continue);
        switch (result) {
            .success, .failure => return result,
            .not_found => {},
        }
    }

    return .not_found;
}

pub const LoadConfigResult = union(enum) {
    success: struct {
        config: std.json.Parsed(Config),
        /// file path of the config.json
        path: []const u8,
    },
    failure: struct {
        /// `null` indicates that the error has already been logged
        error_bundle: ?std.zig.ErrorBundle,

        pub fn toMessage(self: @This(), allocator: std.mem.Allocator) error{OutOfMemory}!?[]u8 {
            const error_bundle = self.error_bundle orelse return null;
            var msg: std.ArrayListUnmanaged(u8) = .{};
            errdefer msg.deinit(allocator);
            error_bundle.renderToWriter(.{ .ttyconf = .no_color }, msg.writer(allocator)) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                else => unreachable, // why does renderToWriter return `anyerror!void`?
            };
            return try msg.toOwnedSlice(allocator);
        }
    },
    not_found,

    pub fn deinit(self: *LoadConfigResult, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .success => |*config_with_path| {
                config_with_path.config.deinit();
                allocator.free(config_with_path.path);
            },
            .failure => |*payload| {
                if (payload.error_bundle) |*error_bundle| error_bundle.deinit(allocator);
            },
            .not_found => {},
        }
    }
};

pub fn loadFromFile(allocator: std.mem.Allocator, file_path: []const u8) error{OutOfMemory}!LoadConfigResult {
    const tracy_zone = tracy.trace(@src());
    defer tracy_zone.end();

    const file_buf = std.fs.cwd().readFileAlloc(allocator, file_path, std.math.maxInt(u32)) catch |err| switch (err) {
        error.FileNotFound => return .not_found,
        error.OutOfMemory => |e| return e,
        else => {
            logger.warn("Error while reading configuration file: {}", .{err});
            return .{ .failure = .{ .error_bundle = null } };
        },
    };
    defer allocator.free(file_buf);

    const parse_options = std.json.ParseOptions{
        .ignore_unknown_fields = true,
        .allocate = .alloc_always,
    };
    var parse_diagnostics: std.json.Diagnostics = .{};

    var scanner = std.json.Scanner.initCompleteInput(allocator, file_buf);
    defer scanner.deinit();
    scanner.enableDiagnostics(&parse_diagnostics);

    @setEvalBranchQuota(10000);
    const config = std.json.parseFromTokenSource(
        Config,
        allocator,
        &scanner,
        parse_options,
    ) catch |err| {
        var eb: std.zig.ErrorBundle.Wip = undefined;
        try eb.init(allocator);
        errdefer eb.deinit();

        const src_path = try eb.addString(file_path);
        const msg = try eb.addString(@errorName(err));

        const src_loc = try eb.addSourceLocation(.{
            .src_path = src_path,
            .line = @intCast(parse_diagnostics.getLine()),
            .column = @intCast(parse_diagnostics.getColumn()),
            .span_start = @intCast(parse_diagnostics.getByteOffset()),
            .span_main = @intCast(parse_diagnostics.getByteOffset()),
            .span_end = @intCast(parse_diagnostics.getByteOffset()),
        });
        try eb.addRootErrorMessage(.{
            .msg = msg,
            .src_loc = src_loc,
        });

        return .{ .failure = .{ .error_bundle = try eb.toOwnedBundle("") } };
    };

    return .{ .success = .{
        .config = config,
        .path = try allocator.dupe(u8, file_path),
    } };
}

/// the same struct as Config but every field is optional
pub const Configuration = getConfigurationType();

// returns a Struct which is the same as `Config` except that every field is optional.
fn getConfigurationType() type {
    var config_info: std.builtin.Type = @typeInfo(Config);
    var fields: [config_info.Struct.fields.len]std.builtin.Type.StructField = undefined;
    for (config_info.Struct.fields, &fields) |field, *new_field| {
        new_field.* = field;
        if (@typeInfo(field.type) != .Optional) {
            new_field.type = @Type(std.builtin.Type{
                .Optional = .{ .child = field.type },
            });
        }
        new_field.default_value = &@as(new_field.type, null);
    }
    config_info.Struct.fields = fields[0..];
    config_info.Struct.decls = &.{};
    return @Type(config_info);
}

test {
    @import("std").testing.refAllDecls(@This());
}
