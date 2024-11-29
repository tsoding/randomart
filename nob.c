#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "nob.h"

// Folders must end with forward slash /
#define BUILD_FOLDER "build/"
#define SRC_FOLDER "src/"

// TODO: redefine for your platform
#define builder_cc(cmd) \
    cmd_append(cmd, "cc")
#define builder_flags(cmd) \
    cmd_append(cmd, "-Wall", "-Wextra", "-Wswitch-enum", "-ggdb", "-I.")
#define builder_output(cmd, output_path) \
    cmd_append(cmd, "-o", output_path)
#define builder_inputs(cmd, ...) \
    cmd_append(cmd, __VA_ARGS__)
#define builder_raylib_include_path(cmd) \
    cmd_append(cmd, "-I./raylib/raylib-5.0_linux_amd64/include")
#define builder_raylib_lib(cmd) \
    cmd_append(cmd, "-L./raylib/raylib-5.0_linux_amd64/lib/", "-l:libraylib.a")
#define builder_libs(cmd) \
    cmd_append(cmd, "-lm")

int main(int argc, char **argv)
{
    NOB_GO_REBUILD_URSELF(argc, argv);

    Cmd cmd = {0};
    const char *program_name = shift(argv, argc);

    if (!mkdir_if_not_exists(BUILD_FOLDER)) return 1;

    builder_cc(&cmd);
    builder_flags(&cmd);
    builder_raylib_include_path(&cmd);
#ifdef _WIN32
    builder_inputs(&cmd, SRC_FOLDER"randomart.c", SRC_FOLDER"ffmpeg_windows.c");
#else
    builder_inputs(&cmd, SRC_FOLDER"randomart.c", SRC_FOLDER"ffmpeg_linux.c");
#endif
    builder_output(&cmd, BUILD_FOLDER"randomart");
    builder_raylib_lib(&cmd);
    builder_libs(&cmd);
    if (!cmd_run_sync_and_reset(&cmd)) return 1;

    if (argc > 0) {
        const char *command_name = shift(argv, argc);
        if (strcmp(command_name, "run") == 0) {
            cmd_append(&cmd, BUILD_FOLDER "randomart");
            da_append_many(&cmd, argv, argc);
            if (!cmd_run_sync_and_reset(&cmd)) return 1;
        } else {
            nob_log(ERROR, "Unknown command %s", command_name);
            return 1;
        }
    }

    return 0;
}
