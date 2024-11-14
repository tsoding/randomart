#ifndef FFMPEG_H_
#define FFMPEG_H_

#include <stddef.h>

typedef void FFMPEG;

FFMPEG *ffmpeg_start_rendering(size_t width, size_t height, size_t fps);
// TODO: handle potential error that may happen in all of the ffmpeg function
void ffmpeg_send_frame(FFMPEG *ffmpeg, void *data, size_t width, size_t height);
// TODO: use -vflip of ffmpeg instead
void ffmpeg_send_frame_flipped(FFMPEG *ffmpeg, void *data, size_t width, size_t height);
void ffmpeg_end_rendering(FFMPEG *ffmpeg);

#endif // FFMPEG_H_
