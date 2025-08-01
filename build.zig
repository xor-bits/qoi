const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const qoi = b.addModule("qoi", .{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const qoi_tests = b.addTest(.{
        .root_module = qoi,
    });

    const run_qoi_tests = b.addRunArtifact(qoi_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_qoi_tests.step);
}
