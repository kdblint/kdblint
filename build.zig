const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zig_lsp = b.dependency("zig-lsp", .{});
    const zig_lsp_module = zig_lsp.module("zig-lsp");

    inline for (&.{
        .{ .os_tag = .linux, .cpu_arch = .aarch64 },
        .{ .os_tag = .linux, .cpu_arch = .x86_64 },
        .{ .os_tag = .macos, .cpu_arch = .aarch64 },
        .{ .os_tag = .macos, .cpu_arch = .x86_64 },
        .{ .os_tag = .windows, .cpu_arch = .aarch64 },
        .{ .os_tag = .windows, .cpu_arch = .x86_64 },
    }) |cross_target| {
        const exe = b.addExecutable(.{
            .name = "kdblint",
            .root_source_file = .{ .path = "src/main.zig" },
            .target = cross_target,
            .optimize = optimize,
        });
        exe.addModule("zig-lsp", zig_lsp_module);

        b.getInstallStep().dependOn(&b.addInstallArtifact(exe, .{
            .dest_dir = .{
                .override = .{
                    .custom = "bin/" ++ @tagName(cross_target.os_tag) ++ "/" ++ @tagName(cross_target.cpu_arch),
                },
            },
        }).step);
    }

    const exe_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
