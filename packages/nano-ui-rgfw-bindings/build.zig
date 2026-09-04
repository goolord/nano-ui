const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "rgfw",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    lib.addCSourceFile(.{
        .file = b.path("cbits/RGFW.c"),
        .flags = &.{"-DRGFW_EXPORT"},
    });
    lib.addIncludePath(b.path("cbits"));
    if (target.result.os.tag == .windows) {
        lib.linkSystemLibrary("gdi32");
        lib.linkSystemLibrary("user32");
        lib.linkSystemLibrary("shell32");
    }
    lib.linkLibC();
    b.installArtifact(lib);
}
