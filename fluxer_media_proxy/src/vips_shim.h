// SPDX-License-Identifier: AGPL-3.0-or-later

#pragma once

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _VipsImage VipsImage;
struct fluxer_webp_anim_limits;

int fluxer_vips_image_is_hdr(VipsImage *image);

int fluxer_vips_tone_map_hdr_to_sdr(VipsImage *in, VipsImage **out);

int fluxer_heif_decode_animated(const void *buf, size_t len, VipsImage **out,
                                int n_max_pages, size_t max_total_pixels);

int fluxer_heif_decode_animated_ex(const void *buf, size_t len, VipsImage **out,
                                   int n_max_pages, size_t max_total_pixels,
                                   int *was_hdr);

int fluxer_heif_decode_animated_ex2(const void *buf, size_t len, VipsImage **out,
                                    int n_max_pages, size_t max_total_pixels,
                                    int *was_hdr, int *had_hdr_gain_map);

int fluxer_heif_aux_type_is_hdr_gain_map_for_test(const char *type);

int fluxer_heif_has_tmap_item_for_test(const void *buf, size_t len);

int fluxer_avif_parse_track_delays_for_test(const void *buf, size_t len,
                                            int **out_delays_ms, int *out_n_samples);
void fluxer_avif_free_delays(int *delays);

void fluxer_vips_set_anim_metadata_for_test(VipsImage *image, int page_height,
                                            int n_pages, const int *delays_ms);

int fluxer_hdr_to_sdr_test(uint16_t r, uint16_t g, uint16_t b,
                           int bit_depth, int transfer,
                           uint8_t out_rgb[3]);

extern const int fluxer_vips_format_uchar;

int fluxer_vips_init(const char *argv0);
void fluxer_vips_error_clear(void);
void fluxer_vips_tune_for_server(int per_pipeline_threads);
int fluxer_vips_probe_animated(const void *buf, size_t len, int *width, int *height, int *pages);
VipsImage *fluxer_vips_image_new_from_buffer(const void *buf, size_t len, const char *option_string);
VipsImage *fluxer_vips_image_new_from_memory_copy(const void *data, size_t size, int width, int height, int bands, int format);
int fluxer_vips_image_write_to_buffer(VipsImage *image, const char *suffix, void **buf, size_t *size);
int fluxer_vips_image_get_width(VipsImage *image);
int fluxer_vips_image_get_height(VipsImage *image);
int fluxer_vips_image_get_bands(VipsImage *image);
int fluxer_vips_image_get_int(VipsImage *image, const char *field, int *out);
void fluxer_vips_set_page_height(VipsImage *image, int page_height);
int fluxer_vips_read_delays_ms(VipsImage *image, int n_pages, int **out_delays, int *out_len);
int fluxer_vips_autorot(VipsImage *in, VipsImage **out);
int fluxer_vips_extract_area(VipsImage *in, VipsImage **out, int left, int top, int width, int height);
int fluxer_vips_resize(VipsImage *in, VipsImage **out, double scale);
int fluxer_vips_thumbnail_buffer(const void *buf, size_t len, VipsImage **out, int width, int height, int n);

#define FLUXER_THUMB_CROP_NONE   0
#define FLUXER_THUMB_CROP_CENTRE 1
int fluxer_vips_thumbnail_buffer_ex(const void *buf, size_t len, VipsImage **out, int width, int height, int n, int crop_mode);
int fluxer_vips_extract_rgba(VipsImage *in, void **out_buf, size_t *out_size);
typedef int (*fluxer_vips_write_cb)(void *user_data, const void *bytes, size_t len);
int fluxer_vips_image_write_to_callback(VipsImage *image, const char *suffix, fluxer_vips_write_cb cb, void *user_data);
void fluxer_vips_unref(VipsImage *image);
void fluxer_vips_free(void *mem);
void fluxer_free_int_array(int *values);

int fluxer_ffmpeg_resize_gif(
    const void *gif_data,
    size_t gif_len,
    int target_width,
    int target_height,
    long long deadline_unix_ms,
    long long max_frames,
    long long max_total_pixels,
    void **out_buf,
    size_t *out_size
);

int fluxer_ffmpeg_video_thumbnail(
    const void *media_data,
    size_t media_len,
    const char *suffix,
    int max_packets,
    void **out_buf,
    size_t *out_size
);

int fluxer_ffmpeg_decode_apng(
    const void *apng_data,
    size_t apng_len,
    VipsImage **out,
    int max_frames,
    size_t max_total_pixels
);

struct fluxer_webp_anim_limits {
    int max_frames;
    int max_duration_ms;
    long long deadline_unix_ms;
};

int fluxer_webp_encode_animated(
    VipsImage *image,
    int quality,
    int lossless,
    int effort,
    int alpha_q,
    int smart_subsample,
    int loop_count,
    int full_canvas_frames,
    const struct fluxer_webp_anim_limits *limits,
    unsigned char *scratch,
    size_t scratch_cap,
    void **out_buf,
    size_t *out_size
);

void fluxer_webp_free(void *mem);

int fluxer_av_probe(
    const void *media_data,
    size_t media_len,
    int *out_has_video,
    int *out_has_audio,
    double *out_duration_seconds
);

struct fluxer_nsfw_frame_out {
    void *data;
    size_t len;
};

int fluxer_av_extract_frames_for_nsfw(
    const void *media_data,
    size_t media_len,
    const double *timestamps_secs,
    size_t n_timestamps,
    struct fluxer_nsfw_frame_out *out_frames
);

void fluxer_nsfw_frames_free(struct fluxer_nsfw_frame_out *frames, size_t n);

#ifdef __cplusplus
}
#endif
