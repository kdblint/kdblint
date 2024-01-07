const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zig_lsp = b.dependency("zig-lsp", .{});
    const zig_lsp_module = zig_lsp.module("zig-lsp");

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
            install(b, resolved_target.tag, resolved_target.arch, optimize, zig_lsp_module);
        }
    } else {
        install(b, builtin.os.tag, builtin.cpu.arch, optimize, zig_lsp_module);
    }

    const exe_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe_unit_tests.root_module.addImport("lsp", zig_lsp_module);
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}

fn install(
    b: *std.Build,
    comptime tag: std.Target.Os.Tag,
    comptime arch: std.Target.Cpu.Arch,
    optimize: std.builtin.OptimizeMode,
    lsp_module: *std.Build.Module,
) void {
    const exe = b.addExecutable(.{
        .name = "kdblint",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = b.resolveTargetQuery(.{
            .cpu_arch = arch,
            .os_tag = tag,
        }),
        .optimize = optimize,
    });
    exe.root_module.addImport("lsp", lsp_module);

    b.getInstallStep().dependOn(&b.addInstallArtifact(exe, .{
        .dest_dir = .{
            .override = .{
                .custom = "../dist/" ++ @tagName(tag) ++ "/" ++ @tagName(arch),
            },
        },
    }).step);
}
