// SPDX-License-Identifier: AGPL-3.0-or-later

#pragma once

#define _GNU_SOURCE
#define _DARWIN_C_SOURCE
#define _POSIX_C_SOURCE 200809L

#include "vips_shim.h"
#include "webp_animation.h"

#include <assert.h>
#include <errno.h>
#include <libavcodec/avcodec.h>
#include <libavfilter/avfilter.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavformat/avformat.h>
#include <libavutil/display.h>
#include <libavutil/imgutils.h>
#include <libavutil/log.h>
#include <libavutil/mem.h>
#include <libavutil/opt.h>
#include <libavutil/pixdesc.h>
#include <libswscale/swscale.h>
#include <libheif/heif.h>
#include <libyuv.h>
#include <lcms2.h>
#include <limits.h>
#include <math.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <vips/vips.h>
#include <webp/demux.h>
#include <webp/encode.h>
#include <webp/mux.h>

#if LIBAVFILTER_VERSION_INT < AV_VERSION_INT(11, 0, 100)
#error "media-proxy needs FFmpeg >= 8.0 (buffersink pixel_formats array option)"
#endif
#if !LIBHEIF_HAVE_VERSION(1, 19, 0)
#error "media-proxy needs libheif >= 1.19.0 (security limits + cancel_decoding)"
#endif
#if VIPS_MAJOR_VERSION < 8 || (VIPS_MAJOR_VERSION == 8 && VIPS_MINOR_VERSION < 13)
#error "media-proxy needs libvips >= 8.13 (vips_block_untrusted_set)"
#endif

#define FLUXER_MAX_VIDEO_FRAME_DIMENSION 16384
#define FLUXER_MAX_VIDEO_RGBA_BYTES ((size_t)128 * 1024 * 1024)
#define FLUXER_MAX_VIDEO_PIXELS (FLUXER_MAX_VIDEO_RGBA_BYTES / 4u)
#define FLUXER_ALLOWED_VIDEO_DECODERS \
    "gif,apng,h264,hevc,vp8,vp9,libvpx-vp9,av1,libaom-av1,libdav1d," \
    "mpeg1video,mpeg2video,mpeg4,h263,theora,flv,vp6,vp6f,vp6a," \
    "wmv1,wmv2,wmv3,vc1,mjpeg,bmp"
#define FLUXER_AV_INPUT_FORMATS \
    "mov,matroska,webm,avi,flv,ogg,mpegts,mpeg,mpegvideo,asf,mp3,wav,flac,aac,aiff"
#define FLUXER_AV_BMP_INPUT_FORMAT "bmp_pipe"
#define FLUXER_NSFW_FRAME_MAX_DIMENSION 512
#define FLUXER_MAX_VIDEO_THUMBNAIL_PACKETS 512
#define FLUXER_ANIMATION_PACKETS_PER_FRAME_LIMIT 4
#define FLUXER_MIN_ANIMATION_FRAME_DELAY_MS 20
/* A frame that declares no delay at all is not a "very fast" frame: GIF authoring tools emit 0
 * and every browser renders it at 100 ms. Clamping it to the 20 ms minimum instead would play
 * the animation five times too fast. */
#define FLUXER_DEFAULT_ANIMATION_FRAME_DELAY_MS 100
#define FLUXER_MAX_AV_STREAMS 32u
#define FLUXER_MAX_GIF_STRUCTURE_BLOCKS 262144u
#define FLUXER_MAX_APNG_CHUNKS 262144u
#define FLUXER_WEBP_MAX_FRAME_DURATION_MS ((1 << 24) - 1)
#define FLUXER_MAX_THREADS_PER_PIPELINE 4
#define FLUXER_VIDEO_DEADLINE_ROWS 64
#define FLUXER_MAX_NSFW_SAMPLES 3

struct ff_mem_reader {
    const uint8_t *data;
    size_t len;
    size_t offset;
};

typedef struct {
    struct ff_mem_reader reader;
    unsigned char *avio_buffer;
    AVIOContext *avio;
    AVFormatContext *format;
    AVCodecContext *decoder;
    AVPacket *packet;
    AVFrame *frame;
    AVStream *stream;
    int stream_index;
    int strict_decode;
    long long deadline_monotonic_ms;
} ff_thumbnail_context;

int fluxer_ffmpeg_decoder_threads_valid(int decoder_threads);
int fluxer_native_deadline_status(long long deadline_monotonic_ms);
int fluxer_ffmpeg_interrupt_deadline(void *opaque);
int fluxer_native_status_from_av_error(int error);
int fluxer_native_status_from_av_error_with_deadline(
    int error, long long deadline_monotonic_ms);
int fluxer_native_status_from_heif_error(struct heif_error error);
int ff_mem_read_packet(void *opaque, uint8_t *buf, int buf_size);
int64_t ff_mem_seek(void *opaque, int64_t offset, int whence);
int fluxer_prepare_untrusted_av_input(AVFormatContext *format);
int fluxer_restrict_untrusted_av_context(AVFormatContext *format,
                                        const char *format_whitelist);
int fluxer_find_stream_info_bounded(AVFormatContext *format);
int fluxer_video_decoder_allowed(const AVStream *stream, const AVCodec *decoder);
int ff_validate_rgba_geometry(int width, int height, size_t *out_size);
int ff_stream_is_attached_picture(const AVStream *stream);
int ff_find_primary_video_stream(AVFormatContext *format, const AVCodec **out_codec);
int fluxer_av_frame_convert_to_rgba(AVFrame *frame, int source_width,
                                    int source_height, int output_width,
                                    int output_height, struct SwsContext **sws,
                                    long long deadline_monotonic_ms,
                                    uint8_t *dst);
int fluxer_validate_complete_apng(const uint8_t *data, size_t len,
                                  int max_frames, size_t max_total_pixels,
                                  int *out_width, int *out_height,
                                  int *out_expected_frames,
                                  uint32_t *out_num_plays);
int fluxer_vips_read_animation_delays_ms(VipsImage *image, int n_pages,
                                         int **out_delays);
int fluxer_heif_checked_rgba_size(int width, int height, size_t *out_size);
int fluxer_heif_detect_hdr_gain_map(struct heif_image_handle *handle,
                                    long long deadline_monotonic_ms,
                                    int *detected);
int fluxer_heif_decode_to_sdr_rgba8(struct heif_image_handle *handle,
                                    uint8_t *destination,
                                    size_t destination_capacity,
                                    int width, int height,
                                    long long deadline_monotonic_ms,
                                    int *out_was_hdr);
int fluxer_vips_extract_animation_rgba_strip(VipsImage *input,
                                             uint8_t *destination,
                                             size_t destination_capacity,
                                             long long deadline_monotonic_ms,
                                             size_t *out_size);
uint16_t fluxer_gif_read_le16(const uint8_t *value);
int fluxer_skip_gif_sub_blocks_checked(const uint8_t *bytes, size_t len,
                                       size_t *offset, size_t *block_count);
int fluxer_validate_complete_gif(const uint8_t *bytes, size_t len,
                                 int max_frames, size_t max_total_pixels,
                                 int *out_expected_frames);
int fluxer_read_gif_frame_delays(const uint8_t *bytes, size_t len,
                                 int *delays_cs, int expected_frames,
                                 int max_duration_ms,
                                 int *out_loop_count);
int fluxer_patch_gif_frame_delays(uint8_t *bytes, size_t len,
                                  const int *delays_cs, int n_delays);
int fluxer_gif_animation_frame_budget(const int *delays_cs, int n_frames,
                                      int max_frames, int max_duration_ms,
                                      int *out_frames);
int fluxer_gif_setup_filter_graph(AVFilterGraph **out_graph,
                                  AVFilterContext **out_source,
                                  AVFilterContext **out_sink,
                                  int source_width, int source_height,
                                  enum AVPixelFormat source_format,
                                  AVRational frame_time_base,
                                  int target_width, int target_height);
int fluxer_vips_image_write_to_buffer_bounded(VipsImage *image,
                                               const char *suffix,
                                               long long deadline_monotonic_ms,
                                               size_t max_output_size,
                                               void **out_buf,
                                               size_t *out_size,
                                               size_t *out_capacity);
int fluxer_vips_image_write_to_memory_deadline(
    VipsImage *image, long long deadline_monotonic_ms,
    size_t max_output_size, void **out_buf, size_t *out_size);
int ff_fit_frame_image(VipsImage **image, int max_width, int max_height);
int ff_emit_frame_thumbnail(AVFrame *frame, AVCodecContext *dec_ctx,
                            AVFormatContext *in_fmt, AVStream *in_stream,
                            const char *suffix, int max_width, int max_height,
                            long long deadline_monotonic_ms,
                            size_t max_output_size,
                            int *out_display_width, int *out_display_height,
                            void **out_buf, size_t *out_size,
                            size_t *out_capacity);
void ff_thumbnail_context_clear(ff_thumbnail_context *context);
int ff_thumbnail_context_open(ff_thumbnail_context *context,
                              const void *media_data, size_t media_len,
                              const char *format_whitelist, int strict_decode,
                              long long deadline_monotonic_ms);
int ff_thumbnail_decoder_open(ff_thumbnail_context *context, int decoder_threads);
void fluxer_nsfw_frames_reset(struct fluxer_nsfw_frame_out *frames,
                              size_t count);
int fluxer_nsfw_animation_selection_valid(const int *indices,
                                          size_t count,
                                          int expected_frames);
