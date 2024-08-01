const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zls = b.dependency("zls", .{});
    const zls_module = zls.module("zls");

    const known_folders = zls.builder.dependency("known_folders", .{});
    const known_folders_module = known_folders.module("known-folders");

    const tracy_module = zls.module("tracy");

    const version = try Version.init(b);
    defer version.deinit(b.allocator);

    const build_options = b.addOptions();
    build_options.step.name = "kdblint build options";
    const build_options_module = build_options.createModule();
    build_options.addOption(std.SemanticVersion, "version", try std.SemanticVersion.parse(version.version));
    build_options.addOption([]const u8, "version_string", version.version);

    const full_build = b.option(bool, "full", "Full Build") orelse false;
    if (full_build) {
        inline for (&.{
            .{ .tag = .linux, .arch = .aarch64 },
            .{ .tag = .linux, .arch = .x86_64 },
            .{ .tag = .macos, .arch = .aarch64 },
            .{ .tag = .macos, .arch = .x86_64 },
            .{ .tag = .windows, .arch = .aarch64 },
            .{ .tag = .windows, .arch = .x86_64 },
        }) |resolved_target| {
            install(
                b,
                resolved_target.tag,
                resolved_target.arch,
                optimize,
                build_options_module,
                zls_module,
                known_folders_module,
                tracy_module,
            );
        }
    } else {
        install(
            b,
            builtin.os.tag,
            builtin.cpu.arch,
            optimize,
            build_options_module,
            zls_module,
            known_folders_module,
            tracy_module,
        );
    }

    const exe_check = b.addExecutable(.{
        .name = "kdblint",
        .root_source_file = b.path("src/main.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = builtin.cpu.arch,
            .os_tag = builtin.os.tag,
        }),
        .optimize = optimize,
    });
    exe_check.root_module.addImport("build_options", build_options_module);
    exe_check.root_module.addImport("zls", zls_module);
    exe_check.root_module.addImport("known_folders", known_folders_module);
    exe_check.root_module.addImport("tracy", tracy_module);

    const check = b.step("check", "Check");
    check.dependOn(&exe_check.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_unit_tests.root_module.addImport("build_options", build_options_module);
    exe_unit_tests.root_module.addImport("zls", zls_module);
    exe_unit_tests.root_module.addImport("known_folders", known_folders_module);
    exe_unit_tests.root_module.addImport("tracy", tracy_module);
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}

fn install(
    b: *std.Build,
    comptime tag: std.Target.Os.Tag,
    comptime arch: std.Target.Cpu.Arch,
    optimize: std.builtin.OptimizeMode,
    build_options: *std.Build.Module,
    zls: *std.Build.Module,
    known_folders: *std.Build.Module,
    tracy: *std.Build.Module,
) void {
    const exe = b.addExecutable(.{
        .name = if (optimize == .Debug) "kdblint.Debug" else "kdblint",
        .root_source_file = b.path("src/main.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = arch,
            .os_tag = tag,
        }),
        .optimize = optimize,
    });
    exe.root_module.addImport("build_options", build_options);
    exe.root_module.addImport("zls", zls);
    exe.root_module.addImport("known_folders", known_folders);
    exe.root_module.addImport("tracy", tracy);

    b.getInstallStep().dependOn(&b.addInstallArtifact(exe, .{
        .dest_dir = .{
            .override = .{
                .custom = "../dist/" ++ @tagName(tag) ++ "/" ++ @tagName(arch),
            },
        },
    }).step);
}

const Version = struct {
    version: []const u8,

    pub fn init(b: *std.Build) !Version {
        const package_json_file = try std.fs.openFileAbsolute(b.path("package.json").getPath(b), .{});
        defer package_json_file.close();

        const package_json_slice = try package_json_file.readToEndAlloc(b.allocator, 1_000_000);
        defer b.allocator.free(package_json_slice);

        const parsed_version = try std.json.parseFromSlice(Version, b.allocator, package_json_slice, .{ .ignore_unknown_fields = true });
        defer parsed_version.deinit();

        const version = try b.allocator.dupe(u8, parsed_version.value.version);
        errdefer b.allocator.free(version);

        return .{
            .version = version,
        };
    }

    pub fn deinit(self: Version, allocator: std.mem.Allocator) void {
        allocator.free(self.version);
    }
};
