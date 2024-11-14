#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define READ_END 0
#define WRITE_END 1

typedef struct {
    int pid;
    int pipe;
} FFMPEG;

FFMPEG *ffmpeg_start_rendering(size_t width, size_t height, size_t fps)
{
    int pipefd[2];

    if (pipe(pipefd) < 0) {
        fprintf(stderr, "ERROR: could not create a pipe: %s\n", strerror(errno));
        return NULL;
    }

    pid_t child = fork();
    if (child < 0) {
        fprintf(stderr, "ERROR: could not fork a child: %s\n", strerror(errno));
        return NULL;
    }

    if (child == 0) {
        if (dup2(pipefd[READ_END], STDIN_FILENO) < 0) {
            fprintf(stderr, "ERROR: could not reopen read end of pipe as stdin: %s\n", strerror(errno));
            exit(1);
        }
        close(pipefd[WRITE_END]);

        char resolution[64];
        snprintf(resolution, sizeof(resolution), "%zux%zu", width, height);
        char framerate[64];
        snprintf(framerate, sizeof(framerate), "%zu", fps);

        int ret = execlp("ffmpeg",
            "ffmpeg",
            "-loglevel", "verbose",
            "-y",

            "-f", "rawvideo",
            "-pix_fmt", "rgba",
            "-s", resolution,
            "-r", framerate,
            "-i", "-",

            "-c:v", "libx264",
            "-vb", "4500k",
            "-c:a", "aac",
            "-ab", "200k",
            "-pix_fmt", "yuv420p",
            "output.mp4",

            NULL
        );
        if (ret < 0) {
            fprintf(stderr, "ERROR: could not run ffmpeg as a child process: %s\n", strerror(errno));
            exit(1);
        }
        assert(0 && "unreachable");
    }

    close(pipefd[READ_END]);

    FFMPEG *ffmpeg = malloc(sizeof(FFMPEG));
    assert(ffmpeg != NULL && "Buy MORE RAM lol!!");
    ffmpeg->pid = child;
    ffmpeg->pipe = pipefd[WRITE_END];
    return ffmpeg;
}

void ffmpeg_end_rendering(FFMPEG *ffmpeg)
{
    close(ffmpeg->pipe);
    waitpid(ffmpeg->pid, NULL, 0);
}

void ffmpeg_send_frame(FFMPEG *ffmpeg, void *data, size_t width, size_t height)
{
    write(ffmpeg->pipe, data, sizeof(uint32_t)*width*height);
}

void ffmpeg_send_frame_flipped(FFMPEG *ffmpeg, void *data, size_t width, size_t height)
{
    for (size_t y = height; y > 0; --y) {
        write(ffmpeg->pipe, (uint32_t*)data + (y - 1)*width, sizeof(uint32_t)*width);
    }
}
