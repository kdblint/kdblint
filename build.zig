const std = @import("std");
const builtin = @import("builtin");

const kdblint_version: std.SemanticVersion = .{ .major = 0, .minor = 1, .patch = 0 };

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run all the tests");
    const no_bin = b.option(bool, "no-bin", "skip emitting binary") orelse false;

    const exe = addCompilerStep(b, .{
        .optimize = optimize,
        .target = target,
    });

    if (no_bin) {
        b.getInstallStep().dependOn(&exe.step);
    } else {
        const install_exe = b.addInstallArtifact(exe, .{
            .dest_dir = .{
                .override = .{
                    .custom = b.fmt("../dist/{s}/{s}", .{ @tagName(target.result.os.tag), @tagName(target.result.cpu.arch) }),
                },
            },
        });
        b.getInstallStep().dependOn(&install_exe.step);

        if (target.result.os.tag == builtin.target.os.tag and target.result.cpu.arch == builtin.target.cpu.arch) {
            const install_native_exe = b.addInstallArtifact(exe, .{});
            b.getInstallStep().dependOn(&install_native_exe.step);
        }
    }

    test_step.dependOn(&exe.step);

    const exe_options = b.addOptions();
    exe.root_module.addOptions("build_options", exe_options);

    const version_slice = v: {
        if (!std.process.can_spawn) {
            std.debug.print("error: version info cannot be retrieved from git.", .{});
            std.process.exit(1);
        }
        const version_string = b.fmt("{d}.{d}.{d}", .{ kdblint_version.major, kdblint_version.minor, kdblint_version.patch });

        var code: u8 = undefined;
        const git_describe_untrimmed = b.runAllowFail(&[_][]const u8{
            "git",
            "-C",
            b.build_root.path orelse ".",
            "describe",
            "--match",
            "*.*.*",
            "--tags",
            "--abbrev=9",
        }, &code, .Ignore) catch {
            break :v version_string;
        };
        const git_describe = std.mem.trim(u8, git_describe_untrimmed, " \n\r");

        switch (std.mem.count(u8, git_describe, "-")) {
            0 => {
                // Tagged release version (e.g. 0.10.0).
                if (!std.mem.eql(u8, git_describe, version_string)) {
                    std.debug.print("kdbLint version '{s}' does not match git tag '{s}'\n", .{ version_string, git_describe });
                    std.process.exit(1);
                }
                break :v version_string;
            },
            2 => {
                // Untagged development build (e.g. 0.10.0-dev.2025+ecf0050a9).
                var it = std.mem.splitScalar(u8, git_describe, '-');
                const tagged_ancestor = it.first();
                const commit_height = it.next().?;
                const commit_id = it.next().?;

                const ancestor_ver = try std.SemanticVersion.parse(tagged_ancestor);
                if (kdblint_version.order(ancestor_ver) != .gt) {
                    std.debug.print("kdbLint version '{}' must be greater than tagged ancestor '{}'\n", .{ kdblint_version, ancestor_ver });
                    std.process.exit(1);
                }

                // Check that the commit hash is prefixed with a 'g' (a Git convention).
                if (commit_id.len < 1 or commit_id[0] != 'g') {
                    std.debug.print("Unexpected `git describe` output: {s}\n", .{git_describe});
                    break :v version_string;
                }

                // The version is reformatted in accordance with the https://semver.org specification.
                break :v b.fmt("{s}-dev.{s}+{s}", .{ version_string, commit_height, commit_id[1..] });
            },
            else => {
                std.debug.print("Unexpected `git describe` output: {s}\n", .{git_describe});
                break :v version_string;
            },
        }

        break :v "";
    };
    const version = try b.allocator.dupeZ(u8, version_slice);
    exe_options.addOption([:0]const u8, "version", version);

    const semver = try std.SemanticVersion.parse(version);
    exe_options.addOption(std.SemanticVersion, "semver", semver);

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    addImports(b, unit_tests);
    unit_tests.root_module.addOptions("build_options", exe_options);

    const run_exe_unit_tests = b.addRunArtifact(unit_tests);
    test_step.dependOn(&run_exe_unit_tests.step);
}

const AddCompilerStepOptions = struct {
    optimize: std.builtin.OptimizeMode,
    target: std.Build.ResolvedTarget,
};

fn addCompilerStep(b: *std.Build, options: AddCompilerStepOptions) *std.Build.Step.Compile {
    const exe = b.addExecutable(.{
        .name = if (options.optimize == .Debug) "kdblint.Debug" else "kdblint",
        .root_source_file = b.path("src/main.zig"),
        .target = options.target,
        .optimize = options.optimize,
    });
    addImports(b, exe);
    return exe;
}

fn addImports(b: *std.Build, step: *std.Build.Step.Compile) void {
    const zls = b.dependency("zls", .{});
    const zls_module = zls.module("zls");
    const tracy_module = zls.module("tracy");

    const known_folders = zls.builder.dependency("known_folders", .{});
    const known_folders_module = known_folders.module("known-folders");

    step.root_module.addImport("zls", zls_module);
    step.root_module.addImport("tracy", tracy_module);
    step.root_module.addImport("known_folders", known_folders_module);
}
