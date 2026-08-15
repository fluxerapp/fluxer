// SPDX-License-Identifier: AGPL-3.0-or-later

#define _GNU_SOURCE
#define _DARWIN_C_SOURCE
#define _POSIX_C_SOURCE 200809L

#include "vips_shim.h"

#include <errno.h>
#include <libavcodec/avcodec.h>
#include <libavfilter/avfilter.h>
#include <libavfilter/buffersink.h>
#include <libavfilter/buffersrc.h>
#include <libavformat/avformat.h>
#include <libavutil/display.h>
#include <libavutil/imgutils.h>
#include <libavutil/mem.h>
#include <libavutil/opt.h>
#include <libswscale/swscale.h>
#include <libheif/heif.h>
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
#include <webp/encode.h>
#include <webp/mux.h>
#include "srgb_profile.h"

const int fluxer_vips_format_uchar = VIPS_FORMAT_UCHAR;

#define FLUXER_MAX_VIDEO_FRAME_DIMENSION 16384
#define FLUXER_MAX_VIDEO_RGBA_BYTES ((size_t)512 * 1024 * 1024)
#define FLUXER_SWS_ROW_PADDING 64

int fluxer_vips_init(const char *argv0) {
    return vips_init(argv0);
}

void fluxer_vips_error_clear(void) {
    vips_error_clear();
}

const char *fluxer_vips_error_buffer(void) {
    return vips_error_buffer();
}

void fluxer_vips_tune_for_server(int per_pipeline_threads) {
    if (per_pipeline_threads <= 0) per_pipeline_threads = 1;
    vips_concurrency_set(per_pipeline_threads);
    vips_cache_set_max(0);
    vips_cache_set_max_mem(0);
    vips_cache_set_max_files(0);
    vips_leak_set(FALSE);
}

int fluxer_vips_probe_animated(const void *buf, size_t len, int *width, int *height, int *pages) {
    if (buf == NULL || len == 0 || width == NULL || height == NULL || pages == NULL) {
        return -1;
    }
    VipsImage *header = vips_image_new_from_buffer(buf, len, "n=1", NULL);
    if (header == NULL) {
        return -1;
    }
    *width = vips_image_get_width(header);
    *height = vips_image_get_height(header);
    int n_pages = 1;
    if (vips_image_get_typeof(header, "n-pages") != 0) {
        if (vips_image_get_int(header, "n-pages", &n_pages) != 0) {
            n_pages = 1;
        }
    }
    *pages = n_pages > 0 ? n_pages : 1;
    g_object_unref(header);
    return 0;
}

VipsImage *fluxer_vips_image_new_from_buffer(const void *buf, size_t len, const char *option_string) {
    return vips_image_new_from_buffer(buf, len, option_string, NULL);
}

VipsImage *fluxer_vips_image_new_from_memory_copy(const void *data, size_t size, int width, int height, int bands, int format) {
    return vips_image_new_from_memory_copy(data, size, width, height, bands, format);
}

int fluxer_vips_image_write_to_buffer(VipsImage *image, const char *suffix, void **buf, size_t *size) {
    return vips_image_write_to_buffer(image, suffix, buf, size, NULL);
}

int fluxer_vips_image_get_width(VipsImage *image) {
    return vips_image_get_width(image);
}

int fluxer_vips_image_get_height(VipsImage *image) {
    return vips_image_get_height(image);
}

int fluxer_vips_image_get_bands(VipsImage *image) {
    return vips_image_get_bands(image);
}

int fluxer_vips_image_get_int(VipsImage *image, const char *field, int *out) {
    return vips_image_get_int(image, field, out);
}

void fluxer_vips_set_page_height(VipsImage *image, int page_height) {
    if (image != NULL && page_height > 0) {
        vips_image_set_int(image, "page-height", page_height);
    }
}

int fluxer_vips_autorot(VipsImage *in, VipsImage **out) {
    return vips_autorot(in, out, NULL);
}

int fluxer_vips_extract_area(VipsImage *in, VipsImage **out, int left, int top, int width, int height) {
    return vips_extract_area(in, out, left, top, width, height, NULL);
}

int fluxer_vips_resize(VipsImage *in, VipsImage **out, double scale) {
    return vips_resize(in, out, scale, NULL);
}

int fluxer_vips_thumbnail_buffer(const void *buf, size_t len, VipsImage **out, int width, int height, int n) {
    return fluxer_vips_thumbnail_buffer_ex(buf, len, out, width, height, n, FLUXER_THUMB_CROP_NONE);
}

int fluxer_vips_thumbnail_buffer_ex(const void *buf, size_t len, VipsImage **out, int width, int height, int n, int crop_mode) {
    if (out == NULL) return -1;
    *out = NULL;
    char option_string[64];
    const unsigned char *bytes = (const unsigned char *)buf;
    int is_jpeg = len >= 3 && bytes[0] == 0xff && bytes[1] == 0xd8 && bytes[2] == 0xff;
    if (n == -1) {
        snprintf(option_string, sizeof(option_string), is_jpeg ? "n=-1,access=sequential" : "n=-1,access=sequential,fail=true");
    } else {
        snprintf(option_string, sizeof(option_string), is_jpeg ? "access=sequential" : "access=sequential,fail=true");
    }

    int effective_crop = crop_mode;
    if (n == -1 && crop_mode == FLUXER_THUMB_CROP_CENTRE) effective_crop = FLUXER_THUMB_CROP_NONE;

    if (effective_crop != FLUXER_THUMB_CROP_CENTRE) {
        int target_w = width > 0 ? width : VIPS_MAX_COORD;
        int height_cap = height > 0 ? height : VIPS_MAX_COORD;
        return vips_thumbnail_buffer(
            (void *)buf, len, out, target_w,
            "height", height_cap,
            "size", VIPS_SIZE_DOWN,
            "no_rotate", FALSE,
            "option_string", option_string,
            NULL
        );
    }

    if (width <= 0 || height <= 0) return -1;

    VipsImage *loaded = vips_image_new_from_buffer(buf, len, option_string, NULL);
    if (loaded == NULL) return -1;

    VipsImage *oriented = NULL;
    if (vips_autorot(loaded, &oriented, NULL) != 0) {
        g_object_unref(loaded);
        return -1;
    }
    g_object_unref(loaded);

    int src_w = vips_image_get_width(oriented);
    int total_h = vips_image_get_height(oriented);
    int page_height = total_h;
    int n_pages = 1;
    if (vips_image_get_typeof(oriented, "page-height") != 0) {
        if (vips_image_get_int(oriented, "page-height", &page_height) != 0 || page_height <= 0) {
            page_height = total_h;
        }
    }
    if (page_height > 0 && total_h > page_height && (total_h % page_height) == 0) {
        n_pages = total_h / page_height;
    } else {
        page_height = total_h;
        n_pages = 1;
    }
    int src_h = page_height;
    if (src_w <= 0 || src_h <= 0) {
        g_object_unref(oriented);
        return -1;
    }

    double scale_w = (double)width / (double)src_w;
    double scale_h = (double)height / (double)src_h;
    double scale = scale_w > scale_h ? scale_w : scale_h;
    if (scale > 1.0) scale = 1.0;

    VipsImage *resized = NULL;
    if (scale < 0.999) {
        if (vips_resize(oriented, &resized, scale, NULL) != 0) {
            g_object_unref(oriented);
            return -1;
        }
        g_object_unref(oriented);
    } else {
        resized = oriented;
    }

    int scaled_w = vips_image_get_width(resized);
    int scaled_h_total = vips_image_get_height(resized);
    int scaled_page_h = scaled_h_total / n_pages;
    int final_w = scaled_w < width ? scaled_w : width;
    int final_h = scaled_page_h < height ? scaled_page_h : height;

    if (final_w == scaled_w && final_h == scaled_page_h) {
        *out = resized;
        return 0;
    }

    int left = (scaled_w - final_w) / 2;
    int top = (scaled_page_h - final_h) / 2;

    if (n_pages == 1) {
        VipsImage *cropped = NULL;
        int rc = vips_extract_area(resized, &cropped, left, top, final_w, final_h, NULL);
        g_object_unref(resized);
        if (rc != 0) return -1;
        *out = cropped;
        return 0;
    }

    VipsImage **pages = g_alloca(sizeof(VipsImage *) * n_pages);
    for (int i = 0; i < n_pages; i++) pages[i] = NULL;
    int rc = 0;
    for (int i = 0; i < n_pages; i++) {
        if (vips_extract_area(resized, &pages[i], left, top + i * scaled_page_h, final_w, final_h, NULL) != 0) {
            rc = -1;
            break;
        }
    }
    g_object_unref(resized);
    if (rc != 0) {
        for (int i = 0; i < n_pages; i++) if (pages[i]) g_object_unref(pages[i]);
        return -1;
    }
    VipsImage *joined = NULL;
    if (vips_arrayjoin(pages, &joined, n_pages, "across", 1, NULL) != 0) {
        for (int i = 0; i < n_pages; i++) if (pages[i]) g_object_unref(pages[i]);
        return -1;
    }
    for (int i = 0; i < n_pages; i++) if (pages[i]) g_object_unref(pages[i]);
    vips_image_set_int(joined, "page-height", final_h);
    *out = joined;
    return 0;
}

struct fluxer_vips_write_ctx {
    fluxer_vips_write_cb cb;
    void *user_data;
    int err;
};

static gint64 fluxer_vips_target_write_adapter(VipsTargetCustom *target, const void *bytes, gint64 length, void *gp) {
    (void)target;
    struct fluxer_vips_write_ctx *c = gp;
    if (length <= 0) return 0;
    if (c->cb(c->user_data, bytes, (size_t)length) != 0) {
        c->err = -1;
        return -1;
    }
    return length;
}

int fluxer_vips_image_write_to_callback(VipsImage *image, const char *suffix, fluxer_vips_write_cb cb, void *user_data) {
    if (image == NULL || suffix == NULL || cb == NULL) return -1;
    VipsTargetCustom *target = vips_target_custom_new();
    if (target == NULL) return -1;

    struct fluxer_vips_write_ctx ctx = { .cb = cb, .user_data = user_data, .err = 0 };
    g_signal_connect(target, "write", G_CALLBACK(fluxer_vips_target_write_adapter), &ctx);

    int rc = vips_image_write_to_target(image, suffix, (VipsTarget *)target, NULL);
    g_object_unref(target);
    if (rc != 0) return -1;
    if (ctx.err != 0) return ctx.err;
    return 0;
}

int fluxer_vips_extract_rgba(VipsImage *in, void **out_buf, size_t *out_size) {
    if (out_buf == NULL || out_size == NULL) {
        return -1;
    }
    *out_buf = NULL;
    *out_size = 0;
    VipsImage *srgb = NULL;
    if (vips_colourspace(in, &srgb, VIPS_INTERPRETATION_sRGB, NULL)) {
        return -1;
    }
    VipsImage *rgba = NULL;
    int bands = vips_image_get_bands(srgb);
    if (bands < 4) {
        if (vips_addalpha(srgb, &rgba, NULL)) {
            g_object_unref(srgb);
            return -1;
        }
        g_object_unref(srgb);
    } else if (bands > 4) {
        if (vips_extract_band(srgb, &rgba, 0, "n", 4, NULL)) {
            g_object_unref(srgb);
            return -1;
        }
        g_object_unref(srgb);
    } else {
        rgba = srgb;
    }


    VipsImage *uchar_img = NULL;
    if (vips_cast_uchar(rgba, &uchar_img, NULL)) {
        g_object_unref(rgba);
        return -1;
    }
    g_object_unref(rgba);

    size_t size = 0;
    void *buf = vips_image_write_to_memory(uchar_img, &size);
    g_object_unref(uchar_img);
    if (buf == NULL) {
        return -1;
    }
    *out_buf = buf;
    *out_size = size;
    return 0;
}

void fluxer_vips_unref(VipsImage *image) {
    if (image != NULL) {
        g_object_unref(image);
    }
}

void fluxer_vips_free(void *mem) {
    g_free(mem);
}

void fluxer_webp_free(void *mem) {
    if (mem) free(mem);
}

static long long fluxer_monotonic_ms(void) {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return 0;
    return (long long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static int fluxer_deadline_expired(long long deadline_unix_ms) {
    if (deadline_unix_ms <= 0) return 0;
    long long now_ms = fluxer_monotonic_ms();
    return now_ms > 0 && now_ms >= deadline_unix_ms;
}

struct ff_mem_reader {
    const uint8_t *data;
    size_t len;
    size_t offset;
};

static int ff_mem_read_packet(void *opaque, uint8_t *buf, int buf_size) {
    struct ff_mem_reader *r = (struct ff_mem_reader *)opaque;
    if (buf_size <= 0) return 0;
    if (r->offset >= r->len) return AVERROR_EOF;
    size_t remaining = r->len - r->offset;
    size_t n = remaining < (size_t)buf_size ? remaining : (size_t)buf_size;
    memcpy(buf, r->data + r->offset, n);
    r->offset += n;
    return (int)n;
}

static int64_t ff_mem_seek(void *opaque, int64_t offset, int whence) {
    struct ff_mem_reader *r = (struct ff_mem_reader *)opaque;
    if (whence == AVSEEK_SIZE) return (int64_t)r->len;
    int mode = whence & ~AVSEEK_FORCE;
    int64_t base = 0;
    if (mode == SEEK_SET) {
        base = 0;
    } else if (mode == SEEK_CUR) {
        base = (int64_t)r->offset;
    } else if (mode == SEEK_END) {
        base = (int64_t)r->len;
    } else {
        return AVERROR(EINVAL);
    }
    int64_t next = base + offset;
    if (next < 0 || (uint64_t)next > r->len) return AVERROR(EINVAL);
    r->offset = (size_t)next;
    return next;
}

static int write_encoded_gif_packets(AVFormatContext *out_fmt, AVCodecContext *enc_ctx,
                                     AVStream *out_stream, AVFrame *frame) {
    int rc = avcodec_send_frame(enc_ctx, frame);
    if (rc < 0) return rc;
    AVPacket *pkt = av_packet_alloc();
    if (pkt == NULL) return AVERROR(ENOMEM);
    while (1) {
        rc = avcodec_receive_packet(enc_ctx, pkt);
        if (rc == AVERROR(EAGAIN) || rc == AVERROR_EOF) {
            av_packet_free(&pkt);
            return 0;
        }
        if (rc < 0) {
            av_packet_free(&pkt);
            return rc;
        }
        av_packet_rescale_ts(pkt, enc_ctx->time_base, out_stream->time_base);
        pkt->stream_index = out_stream->index;
        rc = av_interleaved_write_frame(out_fmt, pkt);
        av_packet_unref(pkt);
        if (rc < 0) {
            av_packet_free(&pkt);
            return rc;
        }
    }
}

static int push_frame_delay_cs(int **values, int *len, int *cap, int delay_cs) {
    if (values == NULL || len == NULL || cap == NULL) return -1;
    if (*len >= *cap) {
        int next_cap = *cap > 0 ? *cap * 2 : 16;
        if (next_cap <= *len) next_cap = *len + 1;
        int *next = (int *)realloc(*values, (size_t)next_cap * sizeof(int));
        if (next == NULL) return -1;
        *values = next;
        *cap = next_cap;
    }
    if (delay_cs < 1) delay_cs = 1;
    if (delay_cs > 65535) delay_cs = 65535;
    (*values)[*len] = delay_cs;
    *len += 1;
    return 0;
}

static void skip_gif_sub_blocks(uint8_t *bytes, size_t len, size_t *offset) {
    while (*offset < len) {
        uint8_t block_len = bytes[*offset];
        *offset += 1;
        if (block_len == 0) return;
        if (*offset > len || (size_t)block_len > len - *offset) {
            *offset = len;
            return;
        }
        *offset += block_len;
    }
}

static int patch_gif_frame_delays(uint8_t *bytes, size_t len, const int *delays_cs, int n_delays) {
    if (bytes == NULL || len < 13 || delays_cs == NULL || n_delays <= 0) return 0;
    if (memcmp(bytes, "GIF87a", 6) != 0 && memcmp(bytes, "GIF89a", 6) != 0) return 0;

    size_t off = 13;
    if (bytes[10] & 0x80) {
        size_t gct_entries = (size_t)1 << ((bytes[10] & 0x07) + 1);
        size_t gct_bytes = gct_entries * 3;
        if (gct_bytes > len - off) return 0;
        off += gct_bytes;
    }

    int patched = 0;
    while (off < len && patched < n_delays) {
        uint8_t introducer = bytes[off];
        if (introducer == 0x3b) break;
        if (introducer == 0x21) {
            if (off + 1 >= len) break;
            uint8_t label = bytes[off + 1];
            if (label == 0xf9) {
                if (off + 7 >= len) break;
                if (bytes[off + 2] == 4) {
                    int delay = delays_cs[patched];
                    if (delay < 1) delay = 1;
                    if (delay > 65535) delay = 65535;
                    bytes[off + 4] = (uint8_t)(delay & 0xff);
                    bytes[off + 5] = (uint8_t)((delay >> 8) & 0xff);
                    patched++;
                    off += 8;
                    continue;
                }
            }
            off += 2;
            skip_gif_sub_blocks(bytes, len, &off);
            continue;
        }
        if (introducer == 0x2c) {
            if (off + 9 >= len) break;
            uint8_t packed = bytes[off + 9];
            off += 10;
            if (packed & 0x80) {
                size_t lct_entries = (size_t)1 << ((packed & 0x07) + 1);
                size_t lct_bytes = lct_entries * 3;
                if (lct_bytes > len - off) break;
                off += lct_bytes;
            }
            if (off >= len) break;
            off += 1;
            skip_gif_sub_blocks(bytes, len, &off);
            continue;
        }
        break;
    }
    return patched;
}

static int fluxer_gif_setup_filter_graph(
    AVFilterGraph **out_graph,
    AVFilterContext **out_src,
    AVFilterContext **out_sink,
    int src_w, int src_h,
    enum AVPixelFormat src_fmt,
    AVRational src_tb,
    int dst_w, int dst_h
) {
    AVFilterGraph *graph = avfilter_graph_alloc();
    if (graph == NULL) return -1;
    AVFilterContext *src_ctx = NULL;
    AVFilterContext *sink_ctx = NULL;
    char src_args[512];
    snprintf(src_args, sizeof(src_args),
             "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=1/1",
             src_w, src_h, (int)src_fmt, src_tb.num, src_tb.den > 0 ? src_tb.den : 100);
    if (avfilter_graph_create_filter(&src_ctx, avfilter_get_by_name("buffer"),
                                     "in", src_args, NULL, graph) < 0)
        goto fail;
    sink_ctx = avfilter_graph_alloc_filter(graph, avfilter_get_by_name("buffersink"), "out");
    if (sink_ctx == NULL) goto fail;
    enum AVPixelFormat sink_fmts[] = { AV_PIX_FMT_PAL8, AV_PIX_FMT_NONE };
    if (av_opt_set_int_list(sink_ctx, "pix_fmts", sink_fmts, AV_PIX_FMT_NONE,
                            AV_OPT_SEARCH_CHILDREN) < 0)
        goto fail;
    if (avfilter_init_dict(sink_ctx, NULL) < 0) goto fail;

    char descr[256];
    snprintf(descr, sizeof(descr),
             "scale=%d:%d:flags=lanczos,format=rgba,"
             "split[a][b];"
             "[a]palettegen=reserve_transparent=1:stats_mode=full[p];"
             "[b][p]paletteuse=alpha_threshold=128:dither=none",
             dst_w, dst_h);
    AVFilterInOut *outputs = avfilter_inout_alloc();
    AVFilterInOut *inputs = avfilter_inout_alloc();
    if (outputs == NULL || inputs == NULL) {
        avfilter_inout_free(&outputs);
        avfilter_inout_free(&inputs);
        goto fail;
    }
    outputs->name = av_strdup("in");
    outputs->filter_ctx = src_ctx;
    outputs->pad_idx = 0;
    outputs->next = NULL;
    inputs->name = av_strdup("out");
    inputs->filter_ctx = sink_ctx;
    inputs->pad_idx = 0;
    inputs->next = NULL;
    if (avfilter_graph_parse_ptr(graph, descr, &inputs, &outputs, NULL) < 0) {
        avfilter_inout_free(&outputs);
        avfilter_inout_free(&inputs);
        goto fail;
    }
    avfilter_inout_free(&outputs);
    avfilter_inout_free(&inputs);
    if (avfilter_graph_config(graph, NULL) < 0) goto fail;

    *out_graph = graph;
    *out_src = src_ctx;
    *out_sink = sink_ctx;
    return 0;
fail:
    avfilter_graph_free(&graph);
    return -1;
}

int fluxer_ffmpeg_resize_gif(
    const void *gif_data,
    size_t gif_len,
    int target_width,
    int target_height,
    long long deadline_unix_ms,
    void **out_buf,
    size_t *out_size
) {
    if (gif_data == NULL || gif_len == 0 || target_width <= 0 || target_height <= 0 ||
        out_buf == NULL || out_size == NULL) {
        return -1;
    }
    *out_buf = NULL;
    *out_size = 0;

    int rc = -1;
    struct ff_mem_reader reader = { .data = (const uint8_t *)gif_data, .len = gif_len, .offset = 0 };
    unsigned char *input_avio_buffer = NULL;
    AVIOContext *input_avio = NULL;
    AVFormatContext *in_fmt = NULL;
    AVCodecContext *dec_ctx = NULL;
    AVFormatContext *out_fmt = NULL;
    AVIOContext *out_avio = NULL;
    AVCodecContext *enc_ctx = NULL;
    AVFilterGraph *filter_graph = NULL;
    AVFilterContext *filter_src = NULL;
    AVFilterContext *filter_sink = NULL;
    AVPacket *packet = NULL;
    AVFrame *frame = NULL;
    AVFrame *scaled = NULL;
    uint8_t *dyn_buf = NULL;
    int *frame_delays_cs = NULL;
    int frame_delays_len = 0;
    int frame_delays_cap = 0;

    input_avio_buffer = av_malloc(64 * 1024);
    if (input_avio_buffer == NULL) goto cleanup;
    input_avio = avio_alloc_context(input_avio_buffer, 64 * 1024, 0, &reader,
                                    ff_mem_read_packet, NULL, ff_mem_seek);
    if (input_avio == NULL) {
        av_free(input_avio_buffer);
        input_avio_buffer = NULL;
        goto cleanup;
    }
    in_fmt = avformat_alloc_context();
    if (in_fmt == NULL) goto cleanup;
    in_fmt->pb = input_avio;
    in_fmt->flags |= AVFMT_FLAG_CUSTOM_IO;
    in_fmt->probesize = 5 * 1024 * 1024;
    in_fmt->max_analyze_duration = 5 * AV_TIME_BASE;
    if (avformat_open_input(&in_fmt, NULL, NULL, NULL) < 0) goto cleanup;
    if (avformat_find_stream_info(in_fmt, NULL) < 0) goto cleanup;

    const AVCodec *decoder = NULL;
    int stream_index = av_find_best_stream(in_fmt, AVMEDIA_TYPE_VIDEO, -1, -1, &decoder, 0);
    if (stream_index < 0) goto cleanup;
    AVStream *in_stream = in_fmt->streams[stream_index];
    if (decoder == NULL) decoder = avcodec_find_decoder(in_stream->codecpar->codec_id);
    if (decoder == NULL) goto cleanup;
    dec_ctx = avcodec_alloc_context3(decoder);
    if (dec_ctx == NULL) goto cleanup;
    if (avcodec_parameters_to_context(dec_ctx, in_stream->codecpar) < 0) goto cleanup;
    if (avcodec_open2(dec_ctx, decoder, NULL) < 0) goto cleanup;

    if (avformat_alloc_output_context2(&out_fmt, NULL, "gif", NULL) < 0 || out_fmt == NULL) goto cleanup;
    if (avio_open_dyn_buf(&out_avio) < 0 || out_avio == NULL) goto cleanup;
    out_fmt->pb = out_avio;

    const AVCodec *encoder = avcodec_find_encoder(AV_CODEC_ID_GIF);
    if (encoder == NULL) goto cleanup;
    AVStream *out_stream = avformat_new_stream(out_fmt, NULL);
    if (out_stream == NULL) goto cleanup;
    enc_ctx = avcodec_alloc_context3(encoder);
    if (enc_ctx == NULL) goto cleanup;
    enc_ctx->width = target_width;
    enc_ctx->height = target_height;
    enc_ctx->pix_fmt = AV_PIX_FMT_PAL8;
    enc_ctx->time_base = (AVRational){ 1, 100 };
    enc_ctx->framerate = (AVRational){ 25, 1 };
    if (out_fmt->oformat != NULL && (out_fmt->oformat->flags & AVFMT_GLOBALHEADER)) {
        enc_ctx->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;
    }
    AVDictionary *enc_opts = NULL;
    av_dict_set(&enc_opts, "gifflags", "-offsetting", 0);
    if (avcodec_open2(enc_ctx, encoder, &enc_opts) < 0) {
        av_dict_free(&enc_opts);
        goto cleanup;
    }
    av_dict_free(&enc_opts);
    if (avcodec_parameters_from_context(out_stream->codecpar, enc_ctx) < 0) goto cleanup;
    out_stream->time_base = enc_ctx->time_base;
    if (avformat_write_header(out_fmt, NULL) < 0) goto cleanup;

    if (fluxer_gif_setup_filter_graph(&filter_graph, &filter_src, &filter_sink,
                                      dec_ctx->width, dec_ctx->height, dec_ctx->pix_fmt,
                                      in_stream->time_base,
                                      target_width, target_height) < 0)
        goto cleanup;

    packet = av_packet_alloc();
    frame = av_frame_alloc();
    scaled = av_frame_alloc();
    if (packet == NULL || frame == NULL || scaled == NULL) goto cleanup;

    int64_t next_pts = 0;
    int64_t last_packet_duration = 0;
    AVRational out_tb = (AVRational){ 1, 100 };
    int read_rc = 0;
    while ((read_rc = av_read_frame(in_fmt, packet)) >= 0) {
        if (fluxer_deadline_expired(deadline_unix_ms)) {
            rc = -2;
            goto cleanup;
        }
        if (packet->stream_index != stream_index) {
            av_packet_unref(packet);
            continue;
        }
        last_packet_duration = packet->duration;
        int send_rc = avcodec_send_packet(dec_ctx, packet);
        av_packet_unref(packet);
        if (send_rc < 0) continue;
        while (1) {
            int recv_rc = avcodec_receive_frame(dec_ctx, frame);
            if (recv_rc == AVERROR(EAGAIN) || recv_rc == AVERROR_EOF) break;
            if (recv_rc < 0) goto cleanup;
            frame->pts = next_pts;
            int64_t duration = frame->duration > 0 ? frame->duration : last_packet_duration;
            int64_t duration_cs = duration > 0 ? av_rescale_q(duration, in_stream->time_base, out_tb) : 2;
            if (duration_cs < 2) duration_cs = 2;
            if (push_frame_delay_cs(&frame_delays_cs, &frame_delays_len, &frame_delays_cap, (int)duration_cs) != 0)
                goto cleanup;
            frame->duration = duration_cs;
            next_pts += duration_cs;
            if (av_buffersrc_add_frame_flags(filter_src, frame, AV_BUFFERSRC_FLAG_KEEP_REF) < 0)
                goto cleanup;
            av_frame_unref(frame);
        }
    }
    if (read_rc != AVERROR_EOF) goto cleanup;
    if (avcodec_send_packet(dec_ctx, NULL) >= 0) {
        while (1) {
            int recv_rc = avcodec_receive_frame(dec_ctx, frame);
            if (recv_rc == AVERROR(EAGAIN) || recv_rc == AVERROR_EOF) break;
            if (recv_rc < 0) goto cleanup;
            frame->pts = next_pts;
            if (push_frame_delay_cs(&frame_delays_cs, &frame_delays_len, &frame_delays_cap, 2) != 0)
                goto cleanup;
            frame->duration = 2;
            next_pts += 2;
            if (av_buffersrc_add_frame_flags(filter_src, frame, AV_BUFFERSRC_FLAG_KEEP_REF) < 0)
                goto cleanup;
            av_frame_unref(frame);
        }
    }
    if (next_pts <= 0) goto cleanup;
    if (av_buffersrc_add_frame_flags(filter_src, NULL, 0) < 0) goto cleanup;
    while (1) {
        if (fluxer_deadline_expired(deadline_unix_ms)) {
            rc = -2;
            goto cleanup;
        }
        int sink_rc = av_buffersink_get_frame(filter_sink, scaled);
        if (sink_rc == AVERROR_EOF) break;
        if (sink_rc == AVERROR(EAGAIN)) break;
        if (sink_rc < 0) goto cleanup;
        if (write_encoded_gif_packets(out_fmt, enc_ctx, out_stream, scaled) < 0) {
            av_frame_unref(scaled);
            goto cleanup;
        }
        av_frame_unref(scaled);
    }
    if (write_encoded_gif_packets(out_fmt, enc_ctx, out_stream, NULL) < 0) goto cleanup;
    if (av_write_trailer(out_fmt) < 0) goto cleanup;

    int dyn_len = avio_close_dyn_buf(out_avio, &dyn_buf);
    out_avio = NULL;
    out_fmt->pb = NULL;
    if (dyn_len <= 0 || dyn_buf == NULL) goto cleanup;
    (void)patch_gif_frame_delays(dyn_buf, (size_t)dyn_len, frame_delays_cs, frame_delays_len);
    void *copy = malloc((size_t)dyn_len);
    if (copy == NULL) goto cleanup;
    memcpy(copy, dyn_buf, (size_t)dyn_len);
    *out_buf = copy;
    *out_size = (size_t)dyn_len;
    rc = 0;

cleanup:
    if (dyn_buf != NULL) av_free(dyn_buf);
    if (out_avio != NULL) {
        uint8_t *discard = NULL;
        avio_close_dyn_buf(out_avio, &discard);
        if (discard != NULL) av_free(discard);
        if (out_fmt != NULL) out_fmt->pb = NULL;
    }
    if (scaled != NULL) av_frame_free(&scaled);
    if (frame != NULL) av_frame_free(&frame);
    if (packet != NULL) av_packet_free(&packet);
    if (filter_graph != NULL) avfilter_graph_free(&filter_graph);
    if (frame_delays_cs != NULL) free(frame_delays_cs);
    if (enc_ctx != NULL) avcodec_free_context(&enc_ctx);
    if (out_fmt != NULL) avformat_free_context(out_fmt);
    if (dec_ctx != NULL) avcodec_free_context(&dec_ctx);
    if (in_fmt != NULL) {
        avformat_close_input(&in_fmt);
    }
    if (input_avio != NULL) {
        if (input_avio->buffer != NULL) av_freep(&input_avio->buffer);
        avio_context_free(&input_avio);
    }
    return rc;
}

static VipsAngle ff_display_matrix_angle(const AVFrame *frame, const AVStream *stream) {
    const int32_t *matrix = NULL;
    const AVFrameSideData *fsd = av_frame_get_side_data(frame, AV_FRAME_DATA_DISPLAYMATRIX);
    if (fsd != NULL && fsd->data != NULL && fsd->size >= 9 * sizeof(int32_t)) {
        matrix = (const int32_t *)fsd->data;
    } else if (stream != NULL && stream->codecpar != NULL) {
        const AVPacketSideData *psd = av_packet_side_data_get(
            stream->codecpar->coded_side_data, stream->codecpar->nb_coded_side_data,
            AV_PKT_DATA_DISPLAYMATRIX);
        if (psd != NULL && psd->data != NULL && psd->size >= 9 * sizeof(int32_t)) {
            matrix = (const int32_t *)psd->data;
        }
    }
    if (matrix == NULL) return VIPS_ANGLE_D0;
    double ccw = av_display_rotation_get(matrix);
    if (isnan(ccw)) return VIPS_ANGLE_D0;
    long quarter = lround(-ccw / 90.0) % 4;
    if (quarter < 0) quarter += 4;
    switch (quarter) {
        case 1: return VIPS_ANGLE_D90;
        case 2: return VIPS_ANGLE_D180;
        case 3: return VIPS_ANGLE_D270;
        default: return VIPS_ANGLE_D0;
    }
}

static int ff_validate_rgba_geometry(int width, int height, size_t *out_size) {
    if (width <= 0 || height <= 0 ||
        width > FLUXER_MAX_VIDEO_FRAME_DIMENSION ||
        height > FLUXER_MAX_VIDEO_FRAME_DIMENSION) {
        return -1;
    }
    size_t row_bytes = (size_t)width * 4;
    if (row_bytes == 0 || (size_t)height > SIZE_MAX / row_bytes) return -1;
    size_t rgba_size = row_bytes * (size_t)height;
    if (rgba_size == 0 || rgba_size > FLUXER_MAX_VIDEO_RGBA_BYTES) return -1;
    if (out_size != NULL) *out_size = rgba_size;
    return 0;
}

static int ff_validate_vips_image_bounds(VipsImage *image) {
    if (image == NULL) return -1;
    size_t ignored = 0;
    return ff_validate_rgba_geometry(
        vips_image_get_width(image),
        vips_image_get_height(image),
        &ignored);
}

static int ff_convert_frame_to_rgba(
    AVFrame *frame,
    int width,
    int height,
    struct SwsContext **sws,
    uint8_t *dst
) {
    if (frame == NULL || sws == NULL || dst == NULL) return -1;
    size_t rgba_size = 0;
    if (ff_validate_rgba_geometry(width, height, &rgba_size) != 0) return -1;
    int packed_linesize = width * 4;
    int padded_linesize = packed_linesize + FLUXER_SWS_ROW_PADDING;
    if (padded_linesize <= packed_linesize) return -1;
    if ((size_t)height > (SIZE_MAX - FLUXER_SWS_ROW_PADDING) / (size_t)padded_linesize) return -1;
    size_t padded_size = (size_t)padded_linesize * (size_t)height + FLUXER_SWS_ROW_PADDING;
    uint8_t *padded = (uint8_t *)malloc(padded_size);
    if (padded == NULL) return -1;

    *sws = sws_getCachedContext(*sws,
                                width, height, (enum AVPixelFormat)frame->format,
                                width, height, AV_PIX_FMT_RGBA,
                                SWS_FAST_BILINEAR, NULL, NULL, NULL);
    if (*sws == NULL) {
        free(padded);
        return -1;
    }
    uint8_t *dst_data[4] = { padded, NULL, NULL, NULL };
    int dst_linesize[4] = { padded_linesize, 0, 0, 0 };
    int scaled_rows = sws_scale(*sws, (const uint8_t * const *)frame->data, frame->linesize,
                                0, height, dst_data, dst_linesize);
    if (scaled_rows != height) {
        free(padded);
        return -1;
    }
    for (int y = 0; y < height; y++) {
        memcpy(dst + (size_t)y * (size_t)packed_linesize,
               padded + (size_t)y * (size_t)padded_linesize,
               (size_t)packed_linesize);
    }
    free(padded);
    (void)rgba_size;
    return 0;
}

static VipsImage *ff_frame_to_rgba_image(AVFrame *frame, int fallback_width, int fallback_height) {
    int width = frame != NULL && frame->width > 0 ? frame->width : fallback_width;
    int height = frame != NULL && frame->height > 0 ? frame->height : fallback_height;
    size_t rgba_size = 0;
    if (ff_validate_rgba_geometry(width, height, &rgba_size) != 0) return NULL;
    uint8_t *rgba = (uint8_t *)malloc(rgba_size);
    if (rgba == NULL) return NULL;
    struct SwsContext *sws = NULL;
    if (ff_convert_frame_to_rgba(frame, width, height, &sws, rgba) != 0) {
        if (sws != NULL) sws_freeContext(sws);
        free(rgba);
        return NULL;
    }
    if (sws != NULL) sws_freeContext(sws);
    VipsImage *image = vips_image_new_from_memory_copy(rgba, rgba_size, width, height, 4, VIPS_FORMAT_UCHAR);
    free(rgba);
    return image;
}

static int ff_apply_display_geometry(VipsImage **image, AVFormatContext *fmt, AVStream *stream, AVFrame *frame) {
    if (image == NULL || *image == NULL) return -1;
    AVRational sar = av_guess_sample_aspect_ratio(fmt, stream, frame);
    if (sar.num > 0 && sar.den > 0 && sar.num != sar.den) {
        double hscale = 1.0;
        double vscale = 1.0;
        if (sar.num > sar.den) {
            hscale = (double)sar.num / (double)sar.den;
        } else {
            vscale = (double)sar.den / (double)sar.num;
        }
        if (!isfinite(hscale) || hscale <= 0.0 ||
            !isfinite(vscale) || vscale <= 0.0) {
            return -1;
        }
        double projected_width = (double)vips_image_get_width(*image) * hscale;
        double projected_height = (double)vips_image_get_height(*image) * vscale;
        if (!isfinite(projected_width) ||
            !isfinite(projected_height) ||
            projected_width < 1.0 ||
            projected_height < 1.0 ||
            projected_width > (double)FLUXER_MAX_VIDEO_FRAME_DIMENSION ||
            projected_height > (double)FLUXER_MAX_VIDEO_FRAME_DIMENSION) {
            return -1;
        }
        if (ff_validate_rgba_geometry((int)ceil(projected_width), (int)ceil(projected_height), NULL) != 0) {
            return -1;
        }
        VipsImage *scaled = NULL;
        if (vips_resize(*image, &scaled, hscale, "vscale", vscale, NULL) != 0) return -1;
        if (ff_validate_vips_image_bounds(scaled) != 0) {
            g_object_unref(scaled);
            return -1;
        }
        g_object_unref(*image);
        *image = scaled;
    }
    VipsAngle angle = ff_display_matrix_angle(frame, stream);
    if (angle != VIPS_ANGLE_D0) {
        VipsImage *rotated = NULL;
        if (vips_rot(*image, &rotated, angle, NULL) != 0) return -1;
        if (ff_validate_vips_image_bounds(rotated) != 0) {
            g_object_unref(rotated);
            return -1;
        }
        g_object_unref(*image);
        *image = rotated;
    }
    return 0;
}

static int ff_emit_frame_thumbnail(
    AVFrame *frame, AVCodecContext *dec_ctx, AVFormatContext *fmt, AVStream *stream,
    const char *suffix, void **out_buf, size_t *out_size
) {
    if (frame == NULL || dec_ctx == NULL || suffix == NULL || out_buf == NULL || out_size == NULL) {
        return -1;
    }
    *out_buf = NULL;
    *out_size = 0;
    int width = frame->width > 0 ? frame->width : dec_ctx->width;
    int height = frame->height > 0 ? frame->height : dec_ctx->height;
    VipsImage *image = ff_frame_to_rgba_image(frame, width, height);
    if (image == NULL) return -1;
    if (ff_apply_display_geometry(&image, fmt, stream, frame) != 0) {
        g_object_unref(image);
        return -1;
    }
    int write_rc = vips_image_write_to_buffer(image, suffix, out_buf, out_size, NULL);
    g_object_unref(image);
    if (write_rc != 0 || *out_buf == NULL || *out_size == 0) {
        if (*out_buf != NULL) {
            g_free(*out_buf);
            *out_buf = NULL;
        }
        *out_size = 0;
        return -1;
    }
    return 0;
}

int fluxer_ffmpeg_video_thumbnail(
    const void *media_data,
    size_t media_len,
    const char *suffix,
    int max_packets,
    void **out_buf,
    size_t *out_size
) {
    if (media_data == NULL || media_len == 0 || suffix == NULL || out_buf == NULL || out_size == NULL) {
        return -1;
    }
    *out_buf = NULL;
    *out_size = 0;
    if (max_packets <= 0) {
        max_packets = 512;
    }

    int rc = -1;
    struct ff_mem_reader reader = { .data = (const uint8_t *)media_data, .len = media_len, .offset = 0 };
    unsigned char *input_avio_buffer = NULL;
    AVIOContext *input_avio = NULL;
    AVFormatContext *in_fmt = NULL;
    AVCodecContext *dec_ctx = NULL;
    AVPacket *packet = NULL;
    AVFrame *frame = NULL;

    input_avio_buffer = av_malloc(64 * 1024);
    if (input_avio_buffer == NULL) goto cleanup;
    input_avio = avio_alloc_context(input_avio_buffer, 64 * 1024, 0, &reader,
                                    ff_mem_read_packet, NULL, ff_mem_seek);
    if (input_avio == NULL) {
        av_free(input_avio_buffer);
        input_avio_buffer = NULL;
        goto cleanup;
    }
    in_fmt = avformat_alloc_context();
    if (in_fmt == NULL) goto cleanup;
    in_fmt->pb = input_avio;
    in_fmt->flags |= AVFMT_FLAG_CUSTOM_IO;
    in_fmt->probesize = 5 * 1024 * 1024;
    in_fmt->max_analyze_duration = 5 * AV_TIME_BASE;
    if (avformat_open_input(&in_fmt, NULL, NULL, NULL) < 0) goto cleanup;
    if (avformat_find_stream_info(in_fmt, NULL) < 0) goto cleanup;

    const AVCodec *decoder = NULL;
    int stream_index = av_find_best_stream(in_fmt, AVMEDIA_TYPE_VIDEO, -1, -1, &decoder, 0);
    if (stream_index < 0) goto cleanup;
    AVStream *in_stream = in_fmt->streams[stream_index];
    if (decoder == NULL) decoder = avcodec_find_decoder(in_stream->codecpar->codec_id);
    if (decoder == NULL) goto cleanup;
    dec_ctx = avcodec_alloc_context3(decoder);
    if (dec_ctx == NULL) goto cleanup;
    if (avcodec_parameters_to_context(dec_ctx, in_stream->codecpar) < 0) goto cleanup;
    if (avcodec_open2(dec_ctx, decoder, NULL) < 0) goto cleanup;

    packet = av_packet_alloc();
    frame = av_frame_alloc();
    if (packet == NULL || frame == NULL) goto cleanup;

    int packets_seen = 0;
    int draining = 0;
    while (rc != 0) {
        if (!draining) {
            if (packets_seen >= max_packets || av_read_frame(in_fmt, packet) < 0) {
                avcodec_send_packet(dec_ctx, NULL);
                draining = 1;
            } else {
                if (packet->stream_index != stream_index) {
                    av_packet_unref(packet);
                    continue;
                }
                packets_seen++;
                int send_rc = avcodec_send_packet(dec_ctx, packet);
                av_packet_unref(packet);
                if (send_rc < 0) continue;
            }
        }
        int recv_rc = avcodec_receive_frame(dec_ctx, frame);
        if (recv_rc == 0) {
            if (ff_emit_frame_thumbnail(frame, dec_ctx, in_fmt, in_stream, suffix, out_buf, out_size) == 0) {
                rc = 0;
            }
            goto cleanup;
        }
        if (recv_rc == AVERROR(EAGAIN) && !draining) {
            continue;
        }
        goto cleanup;
    }

cleanup:
    if (frame != NULL) av_frame_free(&frame);
    if (packet != NULL) av_packet_free(&packet);
    if (dec_ctx != NULL) avcodec_free_context(&dec_ctx);
    if (in_fmt != NULL) avformat_close_input(&in_fmt);
    if (input_avio != NULL) {
        if (input_avio->buffer != NULL) av_freep(&input_avio->buffer);
        avio_context_free(&input_avio);
    } else if (input_avio_buffer != NULL) {
        av_free(input_avio_buffer);
    }
    if (rc != 0 && out_buf != NULL && *out_buf != NULL) {
        g_free(*out_buf);
        *out_buf = NULL;
        *out_size = 0;
    }
    return rc;
}

static int append_ffmpeg_rgba_frame(
    AVFrame *frame,
    AVStream *stream,
    int64_t packet_duration,
    int max_frames,
    size_t max_total_pixels,
    struct SwsContext **sws,
    uint8_t **pixels,
    int **delays,
    int *capacity,
    int *frames,
    int *canvas_w,
    int *canvas_h
) {
    if (frame == NULL || stream == NULL || sws == NULL || pixels == NULL || delays == NULL ||
        capacity == NULL || frames == NULL || canvas_w == NULL || canvas_h == NULL) {
        return -1;
    }
    if (max_frames > 0 && *frames >= max_frames) return 1;

    int width = frame->width > 0 ? frame->width : *canvas_w;
    int height = frame->height > 0 ? frame->height : *canvas_h;
    if (width <= 0 || height <= 0 || width > 16384 || height > 16384) return -1;
    if (*frames == 0) {
        *canvas_w = width;
        *canvas_h = height;
    } else if (width != *canvas_w || height != *canvas_h) {
        return -1;
    }

    size_t frame_pixels = (size_t)width * (size_t)height;
    if (frame_pixels == 0 || frame_pixels > SIZE_MAX / 4) return -1;
    if (max_total_pixels > 0 && frame_pixels > max_total_pixels) return -1;
    if (max_total_pixels > 0 && (size_t)(*frames + 1) > max_total_pixels / frame_pixels) {
        return *frames > 0 ? 1 : -1;
    }
    size_t frame_bytes = frame_pixels * 4;
    if (*capacity <= *frames) {
        int new_capacity = *capacity > 0 ? *capacity * 2 : 8;
        if (max_frames > 0 && new_capacity > max_frames) new_capacity = max_frames;
        if (new_capacity <= *frames) new_capacity = *frames + 1;
        if ((size_t)new_capacity > SIZE_MAX / frame_bytes) return -1;
        uint8_t *new_pixels = (uint8_t *)realloc(*pixels, (size_t)new_capacity * frame_bytes);
        if (new_pixels == NULL) return -1;
        *pixels = new_pixels;
        int *new_delays = (int *)realloc(*delays, (size_t)new_capacity * sizeof(int));
        if (new_delays == NULL) return -1;
        *delays = new_delays;
        *capacity = new_capacity;
    }

    uint8_t *dst = *pixels + (size_t)(*frames) * frame_bytes;
    if (ff_convert_frame_to_rgba(frame, width, height, sws, dst) != 0) return -1;

    int delay = 100;
    int64_t duration = frame->duration > 0 ? frame->duration : packet_duration;
    if (duration > 0 && stream->time_base.num > 0 && stream->time_base.den > 0) {
        int64_t ms = av_rescale_q(duration, stream->time_base, (AVRational){ 1, 1000 });
        if (ms >= 20) {
            delay = ms > INT_MAX ? INT_MAX : (int)ms;
        }
    }
    (*delays)[*frames] = delay;
    *frames += 1;
    return 0;
}

int fluxer_ffmpeg_decode_apng(
    const void *apng_data,
    size_t apng_len,
    VipsImage **out,
    int max_frames,
    size_t max_total_pixels
) {
    if (apng_data == NULL || apng_len == 0 || out == NULL) return -1;
    *out = NULL;

    int rc = -1;
    struct ff_mem_reader reader = { .data = (const uint8_t *)apng_data, .len = apng_len, .offset = 0 };
    unsigned char *input_avio_buffer = NULL;
    AVIOContext *input_avio = NULL;
    AVFormatContext *in_fmt = NULL;
    AVCodecContext *dec_ctx = NULL;
    struct SwsContext *sws = NULL;
    AVPacket *packet = NULL;
    AVFrame *frame = NULL;
    uint8_t *pixels = NULL;
    int *delays = NULL;
    VipsImage *image = NULL;
    int capacity = 0;
    int frames = 0;
    int canvas_w = 0;
    int canvas_h = 0;

    input_avio_buffer = av_malloc(64 * 1024);
    if (input_avio_buffer == NULL) goto cleanup;
    input_avio = avio_alloc_context(input_avio_buffer, 64 * 1024, 0, &reader,
                                    ff_mem_read_packet, NULL, ff_mem_seek);
    if (input_avio == NULL) {
        av_free(input_avio_buffer);
        input_avio_buffer = NULL;
        goto cleanup;
    }
    in_fmt = avformat_alloc_context();
    if (in_fmt == NULL) goto cleanup;
    in_fmt->pb = input_avio;
    in_fmt->flags |= AVFMT_FLAG_CUSTOM_IO;
    in_fmt->probesize = 5 * 1024 * 1024;
    in_fmt->max_analyze_duration = 5 * AV_TIME_BASE;
    const AVInputFormat *apng_format = av_find_input_format("apng");
    if (avformat_open_input(&in_fmt, NULL, apng_format, NULL) < 0) goto cleanup;
    if (avformat_find_stream_info(in_fmt, NULL) < 0) goto cleanup;

    const AVCodec *decoder = NULL;
    int stream_index = av_find_best_stream(in_fmt, AVMEDIA_TYPE_VIDEO, -1, -1, &decoder, 0);
    if (stream_index < 0) goto cleanup;
    AVStream *in_stream = in_fmt->streams[stream_index];
    if (decoder == NULL) decoder = avcodec_find_decoder(in_stream->codecpar->codec_id);
    if (decoder == NULL) goto cleanup;
    dec_ctx = avcodec_alloc_context3(decoder);
    if (dec_ctx == NULL) goto cleanup;
    if (avcodec_parameters_to_context(dec_ctx, in_stream->codecpar) < 0) goto cleanup;
    if (avcodec_open2(dec_ctx, decoder, NULL) < 0) goto cleanup;

    packet = av_packet_alloc();
    frame = av_frame_alloc();
    if (packet == NULL || frame == NULL) goto cleanup;

    int stop = 0;
    while (!stop && av_read_frame(in_fmt, packet) >= 0) {
        if (packet->stream_index != stream_index) {
            av_packet_unref(packet);
            continue;
        }
        int64_t packet_duration = packet->duration;
        int send_rc = avcodec_send_packet(dec_ctx, packet);
        av_packet_unref(packet);
        if (send_rc < 0) continue;
        while (1) {
            int recv_rc = avcodec_receive_frame(dec_ctx, frame);
            if (recv_rc == AVERROR(EAGAIN) || recv_rc == AVERROR_EOF) break;
            if (recv_rc < 0) goto cleanup;
            int append_rc = append_ffmpeg_rgba_frame(frame, in_stream, packet_duration,
                                                     max_frames, max_total_pixels, &sws,
                                                     &pixels, &delays, &capacity, &frames,
                                                     &canvas_w, &canvas_h);
            av_frame_unref(frame);
            if (append_rc < 0) goto cleanup;
            if (append_rc > 0) { stop = 1; break; }
        }
    }

    if (!stop && avcodec_send_packet(dec_ctx, NULL) >= 0) {
        while (1) {
            int recv_rc = avcodec_receive_frame(dec_ctx, frame);
            if (recv_rc == AVERROR(EAGAIN) || recv_rc == AVERROR_EOF) break;
            if (recv_rc < 0) goto cleanup;
            int append_rc = append_ffmpeg_rgba_frame(frame, in_stream, 0,
                                                     max_frames, max_total_pixels, &sws,
                                                     &pixels, &delays, &capacity, &frames,
                                                     &canvas_w, &canvas_h);
            av_frame_unref(frame);
            if (append_rc < 0) goto cleanup;
            if (append_rc > 0) break;
        }
    }

    if (frames <= 0 || canvas_w <= 0 || canvas_h <= 0) goto cleanup;
    size_t frame_bytes = (size_t)canvas_w * (size_t)canvas_h * 4;
    if (frame_bytes == 0 || (size_t)frames > SIZE_MAX / frame_bytes) goto cleanup;
    image = vips_image_new_from_memory_copy(pixels, frame_bytes * (size_t)frames,
                                            canvas_w, canvas_h * frames, 4, VIPS_FORMAT_UCHAR);
    if (image == NULL) goto cleanup;
    vips_image_set_int(image, "page-height", canvas_h);
    if (frames > 1) vips_image_set_int(image, "n-pages", frames);
    vips_image_set_array_int(image, "delay", delays, frames);
    *out = image;
    image = NULL;
    rc = 0;

cleanup:
    if (image != NULL) g_object_unref(image);
    if (pixels != NULL) free(pixels);
    if (delays != NULL) free(delays);
    if (sws != NULL) sws_freeContext(sws);
    if (frame != NULL) av_frame_free(&frame);
    if (packet != NULL) av_packet_free(&packet);
    if (dec_ctx != NULL) avcodec_free_context(&dec_ctx);
    if (in_fmt != NULL) avformat_close_input(&in_fmt);
    if (input_avio != NULL) {
        if (input_avio->buffer != NULL) av_freep(&input_avio->buffer);
        avio_context_free(&input_avio);
    } else if (input_avio_buffer != NULL) {
        av_free(input_avio_buffer);
    }
    if (rc != 0) *out = NULL;
    return rc;
}

static int extract_rgba_strip_into(
    VipsImage *in,
    uint8_t *dst,
    size_t dst_cap,
    uint8_t **out_alloc,
    size_t *out_size
) {
    *out_alloc = NULL;
    *out_size = 0;

    VipsImage *srgb = NULL;
    if (vips_colourspace(in, &srgb, VIPS_INTERPRETATION_sRGB, NULL)) return -1;

    VipsImage *rgba = NULL;
    int bands = vips_image_get_bands(srgb);
    if (bands < 4) {
        if (vips_addalpha(srgb, &rgba, NULL)) { g_object_unref(srgb); return -1; }
        g_object_unref(srgb);
    } else if (bands > 4) {
        if (vips_extract_band(srgb, &rgba, 0, "n", 4, NULL)) { g_object_unref(srgb); return -1; }
        g_object_unref(srgb);
    } else {
        rgba = srgb;
    }

    VipsImage *uchar_img = NULL;
    if (vips_cast_uchar(rgba, &uchar_img, NULL)) { g_object_unref(rgba); return -1; }
    g_object_unref(rgba);

    size_t vips_size = 0;
    void *vips_buf = vips_image_write_to_memory(uchar_img, &vips_size);
    int w = vips_image_get_width(uchar_img);
    int h = vips_image_get_height(uchar_img);
    g_object_unref(uchar_img);
    if (vips_buf == NULL) return -1;

    size_t need = (size_t)w * (size_t)h * 4;
    if (vips_size < need) { g_free(vips_buf); return -1; }

    if (dst != NULL && dst_cap >= need) {
        memcpy(dst, vips_buf, need);
        g_free(vips_buf);
        *out_size = need;
        return 0;
    }

    uint8_t *copy = (uint8_t *)malloc(need);
    if (copy == NULL) { g_free(vips_buf); return -1; }
    memcpy(copy, vips_buf, need);
    g_free(vips_buf);
    *out_alloc = copy;
    *out_size = need;
    return 0;
}

static int *read_delays_ms(VipsImage *image, int n_pages) {
    int *out = (int *)calloc((size_t)n_pages, sizeof(int));
    if (out == NULL) return NULL;
    for (int i = 0; i < n_pages; i++) out[i] = 100;

    if (vips_image_get_typeof(image, "delay") != 0) {
        int *arr = NULL;
        int n = 0;
        if (vips_image_get_array_int(image, "delay", &arr, &n) == 0 && arr != NULL && n > 0) {
            int copy_n = n < n_pages ? n : n_pages;
            for (int i = 0; i < copy_n; i++) {
                int v = arr[i];
                if (v < 20) v = 100;
                out[i] = v;
            }
            return out;
        }
    }
    if (vips_image_get_typeof(image, "gif-delay") != 0) {
        int v = 0;
        if (vips_image_get_int(image, "gif-delay", &v) == 0 && v > 0) {
            int ms = v * 10;
            if (ms < 20) ms = 100;
            for (int i = 0; i < n_pages; i++) out[i] = ms;
        }
    }
    return out;
}

int fluxer_vips_read_delays_ms(VipsImage *image, int n_pages, int **out_delays, int *out_len) {
    if (image == NULL || n_pages <= 0 || out_delays == NULL || out_len == NULL) return -1;
    *out_delays = NULL;
    *out_len = 0;
    int *delays = read_delays_ms(image, n_pages);
    if (delays == NULL) return -1;
    *out_delays = delays;
    *out_len = n_pages;
    return 0;
}

void fluxer_free_int_array(int *values) {
    free(values);
}

static int configure_webp_encoder(
    WebPConfig *config,
    int quality,
    int lossless,
    int effort,
    int alpha_q,
    int smart_subsample
) {
    if (!WebPConfigInit(config)) return -1;
    if (lossless) {
        if (!WebPConfigLosslessPreset(config, effort > 9 ? 9 : (effort < 0 ? 0 : effort))) {
            return -1;
        }
        config->quality = (float)quality;
    } else {
        config->lossless = 0;
        config->quality = (float)quality;
        config->method = effort > 6 ? 6 : (effort < 0 ? 0 : effort);
        config->alpha_quality = alpha_q;
        config->use_sharp_yuv = smart_subsample ? 1 : 0;
    }
    return WebPValidateConfig(config) ? 0 : -1;
}

static int encode_rgba_webp_frame(
    const uint8_t *rgba,
    int width,
    int height,
    const WebPConfig *config,
    WebPMemoryWriter *writer,
    WebPData *bitstream
) {
    WebPPicture pic;
    if (!WebPPictureInit(&pic)) return -1;
    WebPMemoryWriterInit(writer);
    WebPDataInit(bitstream);

    pic.width = width;
    pic.height = height;
    pic.use_argb = 1;
    pic.writer = WebPMemoryWrite;
    pic.custom_ptr = writer;
    if (!WebPPictureImportRGBA(&pic, rgba, width * 4)) {
        WebPPictureFree(&pic);
        WebPMemoryWriterClear(writer);
        return -1;
    }
    if (!WebPEncode(config, &pic)) {
        WebPPictureFree(&pic);
        WebPMemoryWriterClear(writer);
        return -1;
    }
    WebPPictureFree(&pic);
    if (writer->mem == NULL || writer->size == 0) {
        WebPMemoryWriterClear(writer);
        return -1;
    }
    bitstream->bytes = writer->mem;
    bitstream->size = writer->size;
    return 0;
}

static int copy_webp_data_to_malloc(WebPData *webp_data, void **out_buf, size_t *out_size) {
    if (webp_data == NULL || webp_data->bytes == NULL || webp_data->size == 0 ||
        out_buf == NULL || out_size == NULL) {
        return -1;
    }
    uint8_t *out = (uint8_t *)malloc(webp_data->size);
    if (out == NULL) return -1;
    memcpy(out, webp_data->bytes, webp_data->size);
    *out_buf = out;
    *out_size = webp_data->size;
    return 0;
}

static int encode_animated_webp_full_canvas(
    VipsImage *image,
    int width,
    int page_height,
    int n_pages,
    const int *delays,
    const WebPConfig *config,
    int loop_count,
    int max_frames,
    int max_duration_ms,
    long long deadline_unix_ms,
    unsigned char *scratch,
    size_t scratch_cap,
    void **out_buf,
    size_t *out_size
) {
    WebPMux *mux = WebPMuxNew();
    if (mux == NULL) return -1;
    if (WebPMuxSetCanvasSize(mux, width, page_height) != WEBP_MUX_OK) {
        WebPMuxDelete(mux);
        return -1;
    }
    WebPMuxAnimParams anim_params = {
        .bgcolor = 0x00000000,
        .loop_count = loop_count,
    };
    if (WebPMuxSetAnimationParams(mux, &anim_params) != WEBP_MUX_OK) {
        WebPMuxDelete(mux);
        return -1;
    }

    int timestamp = 0;
    int frames_added = 0;
    for (int i = 0; i < n_pages; i++) {
        if (max_frames > 0 && frames_added >= max_frames) break;
        if (max_duration_ms > 0 && timestamp >= max_duration_ms) break;
        if (deadline_unix_ms > 0) {
            struct timespec ts;
            if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
                long long now_ms = (long long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
                if (now_ms >= deadline_unix_ms) break;
            }
        }

        VipsImage *strip = NULL;
        if (vips_extract_area(image, &strip, 0, i * page_height, width, page_height, NULL) != 0) {
            break;
        }
        uint8_t *frame_alloc = NULL;
        size_t rgba_size = 0;
        int erc = extract_rgba_strip_into(strip, scratch, scratch_cap, &frame_alloc, &rgba_size);
        g_object_unref(strip);
        if (erc != 0) {
            break;
        }

        const uint8_t *rgba = (frame_alloc != NULL) ? frame_alloc : scratch;
        WebPMemoryWriter writer;
        WebPData frame_data;
        if (encode_rgba_webp_frame(rgba, width, page_height, config, &writer, &frame_data) != 0) {
            if (frame_alloc) free(frame_alloc);
            break;
        }
        if (frame_alloc) free(frame_alloc);

        WebPMuxFrameInfo frame;
        memset(&frame, 0, sizeof(frame));
        frame.bitstream = frame_data;
        frame.x_offset = 0;
        frame.y_offset = 0;
        frame.duration = delays[i];
        frame.id = WEBP_CHUNK_ANMF;
        frame.dispose_method = WEBP_MUX_DISPOSE_NONE;
        frame.blend_method = WEBP_MUX_NO_BLEND;
        WebPMuxError push_rc = WebPMuxPushFrame(mux, &frame, 1);
        WebPMemoryWriterClear(&writer);
        if (push_rc != WEBP_MUX_OK) {
            break;
        }
        timestamp += delays[i];
        frames_added++;
    }

    if (frames_added == 0) {
        WebPMuxDelete(mux);
        return -1;
    }

    WebPData webp_data;
    WebPDataInit(&webp_data);
    WebPMuxError assemble_rc = WebPMuxAssemble(mux, &webp_data);
    WebPMuxDelete(mux);
    if (assemble_rc != WEBP_MUX_OK || webp_data.bytes == NULL || webp_data.size == 0) {
        WebPDataClear(&webp_data);
        return -1;
    }
    int copy_rc = copy_webp_data_to_malloc(&webp_data, out_buf, out_size);
    WebPDataClear(&webp_data);
    if (copy_rc != 0) return -1;
    return 0;
}

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
) {
    if (image == NULL || out_buf == NULL || out_size == NULL) return -1;
    *out_buf = NULL;
    *out_size = 0;

    int total_h = vips_image_get_height(image);
    int width = vips_image_get_width(image);
    if (width <= 0 || total_h <= 0) return -1;

    int page_height = total_h;
    if (vips_image_get_typeof(image, "page-height") != 0) {
        if (vips_image_get_int(image, "page-height", &page_height) != 0 || page_height <= 0) {
            page_height = total_h;
        }
    }
    if (page_height <= 0 || page_height > total_h || (total_h % page_height) != 0) {
        page_height = total_h;
    }
    int n_pages = total_h / page_height;
    if (n_pages < 1) n_pages = 1;

    int *delays = read_delays_ms(image, n_pages);
    if (delays == NULL) return -1;

    int max_frames = (limits != NULL) ? limits->max_frames : 0;
    int max_duration_ms = (limits != NULL) ? limits->max_duration_ms : 0;
    long long deadline_unix_ms = (limits != NULL) ? limits->deadline_unix_ms : 0;

    WebPConfig config;
    if (configure_webp_encoder(&config, quality, lossless, effort, alpha_q, smart_subsample) != 0) {
        free(delays);
        return -1;
    }

    if (full_canvas_frames) {
        int rc = encode_animated_webp_full_canvas(
            image,
            width,
            page_height,
            n_pages,
            delays,
            &config,
            loop_count,
            max_frames,
            max_duration_ms,
            deadline_unix_ms,
            scratch,
            scratch_cap,
            out_buf,
            out_size
        );
        free(delays);
        return rc;
    }

    WebPAnimEncoderOptions anim_opts;
    if (!WebPAnimEncoderOptionsInit(&anim_opts)) { free(delays); return -1; }
    anim_opts.anim_params.loop_count = loop_count;
    anim_opts.anim_params.bgcolor = 0x00000000;
    anim_opts.kmin = 3;
    anim_opts.kmax = 4;
    anim_opts.allow_mixed = 0;

    WebPAnimEncoder *enc = WebPAnimEncoderNew(width, page_height, &anim_opts);
    if (enc == NULL) { free(delays); return -1; }

    int timestamp = 0;
    int rc = 0;
    int frames_added = 0;
    for (int i = 0; i < n_pages; i++) {
        if (max_frames > 0 && frames_added >= max_frames) break;
        if (max_duration_ms > 0 && timestamp >= max_duration_ms) break;
        if (deadline_unix_ms > 0) {
            struct timespec ts;
            if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
                long long now_ms = (long long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
                if (now_ms >= deadline_unix_ms) break;
            }
        }
        VipsImage *strip = NULL;
        if (vips_extract_area(image, &strip, 0, i * page_height, width, page_height, NULL) != 0) {
            rc = -1; break;
        }
        uint8_t *frame_alloc = NULL;
        size_t rgba_size = 0;
        int erc = extract_rgba_strip_into(strip, scratch, scratch_cap, &frame_alloc, &rgba_size);
        g_object_unref(strip);
        if (erc != 0) { rc = -1; break; }

        const uint8_t *rgba = (frame_alloc != NULL) ? frame_alloc : scratch;

        WebPPicture pic;
        if (!WebPPictureInit(&pic)) { if (frame_alloc) free(frame_alloc); rc = -1; break; }
        pic.width = width;
        pic.height = page_height;
        pic.use_argb = 1;
        if (!WebPPictureImportRGBA(&pic, rgba, width * 4)) {
            if (frame_alloc) {
                free(frame_alloc);
            }
            WebPPictureFree(&pic);
            rc = -1;
            break;
        }
        if (frame_alloc) free(frame_alloc);

        if (!WebPAnimEncoderAdd(enc, &pic, timestamp, &config)) {
            WebPPictureFree(&pic); rc = -1; break;
        }
        WebPPictureFree(&pic);
        timestamp += delays[i];
        frames_added++;
    }
    free(delays);

    if (rc != 0 && frames_added == 0) { WebPAnimEncoderDelete(enc); return -1; }
    if (frames_added == 0) { WebPAnimEncoderDelete(enc); return -1; }

    if (!WebPAnimEncoderAdd(enc, NULL, timestamp, NULL)) {
        WebPAnimEncoderDelete(enc);
        return -1;
    }

    WebPData webp_data;
    WebPDataInit(&webp_data);
    if (!WebPAnimEncoderAssemble(enc, &webp_data)) {
        WebPAnimEncoderDelete(enc);
        return -1;
    }
    WebPAnimEncoderDelete(enc);

    if (copy_webp_data_to_malloc(&webp_data, out_buf, out_size) != 0) {
        WebPDataClear(&webp_data);
        return -1;
    }
    WebPDataClear(&webp_data);
    return 0;
}

static char fluxer_srgb_profile_path[64] = {0};
static pthread_once_t fluxer_srgb_profile_once = PTHREAD_ONCE_INIT;

static void fluxer_init_srgb_profile_path(void) {
    char tmpl[] = "/tmp/fluxer-srgb-XXXXXX.icc";
    int fd = mkstemps(tmpl, 4);
    if (fd < 0) return;
    ssize_t n = write(fd, srgb_profile, srgb_profile_size);
    close(fd);
    if (n != (ssize_t)srgb_profile_size) {
        unlink(tmpl);
        return;
    }
    strncpy(fluxer_srgb_profile_path, tmpl, sizeof(fluxer_srgb_profile_path) - 1);
}

static const char *fluxer_get_srgb_profile_path(void) {
    pthread_once(&fluxer_srgb_profile_once, fluxer_init_srgb_profile_path);
    return fluxer_srgb_profile_path[0] ? fluxer_srgb_profile_path : NULL;
}

int fluxer_vips_image_is_hdr(VipsImage *image) {
    if (image == NULL) return 0;
    VipsInterpretation interp = vips_image_get_interpretation(image);
    if (interp == VIPS_INTERPRETATION_scRGB) return 1;

    if (vips_image_get_typeof(image, VIPS_META_ICC_NAME) == 0) return 0;
    const void *profile_data = NULL;
    size_t profile_len = 0;
    if (vips_image_get_blob(image, VIPS_META_ICC_NAME, &profile_data, &profile_len) != 0) {
        vips_error_clear();
        return 0;
    }
    if (profile_data == NULL || profile_len < 128) return 0;

    const uint8_t *p = profile_data;
    size_t scan_len = profile_len > 4096 ? 4096 : profile_len;
    static const char *markers[] = {
        "PQ", "HLG", "Rec. 2100", "Rec.2100", "ITU-R BT.2100",
        "SMPTE ST 2084", "ST 2084", "Hybrid Log",
    };
    for (size_t m = 0; m < sizeof(markers) / sizeof(markers[0]); m++) {
        const char *needle = markers[m];
        size_t nl = strlen(needle);
        if (nl > scan_len) continue;
        for (size_t i = 0; i + nl <= scan_len; i++) {
            if (memcmp(p + i, needle, nl) == 0) return 1;
        }
        if (nl * 2 + 1 > scan_len) continue;
        for (size_t i = 0; i + nl * 2 <= scan_len; i++) {
            int match = 1;
            for (size_t j = 0; j < nl; j++) {
                if (p[i + j * 2] != 0 || p[i + j * 2 + 1] != (uint8_t)needle[j]) {
                    match = 0;
                    break;
                }
            }
            if (match) return 1;
        }
    }
    return 0;
}

int fluxer_vips_tone_map_hdr_to_sdr(VipsImage *in, VipsImage **out) {
    if (in == NULL || out == NULL) return -1;
    *out = NULL;
    const char *target = fluxer_get_srgb_profile_path();
    if (target == NULL) return -1;

    int has_profile = vips_image_get_typeof(in, VIPS_META_ICC_NAME) != 0;
    if (has_profile) {
        if (vips_icc_transform(in, out, target,
                               "embedded", TRUE,
                               "intent", VIPS_INTENT_PERCEPTUAL,
                               NULL) != 0) {
            vips_error_clear();
            return -1;
        }
        return 0;
    }
    if (vips_icc_transform(in, out, target,
                           "input_profile", target,
                           "intent", VIPS_INTENT_PERCEPTUAL,
                           NULL) != 0) {
        vips_error_clear();
        return -1;
    }
    return 0;
}


#define FLUXER_HDR_PQ_LUT_SIZE 4096
static float fluxer_pq_lut[FLUXER_HDR_PQ_LUT_SIZE];
#define FLUXER_HDR_HLG_LUT_SIZE 4096
static float fluxer_hlg_lut[FLUXER_HDR_HLG_LUT_SIZE];
static pthread_once_t fluxer_hdr_lut_once = PTHREAD_ONCE_INIT;

#define FLUXER_PQ_SDR_TARGET_NORM   0.0203f
#define FLUXER_HLG_SDR_TARGET_NORM  0.075f

static float fluxer_pq_sdr_target_perc;
static float fluxer_hlg_sdr_target_perc;

static inline float fluxer_pq_oetf(float l);

static void fluxer_init_hdr_luts(void) {
    const double m1 = 0.1593017578125;
    const double m2 = 78.84375;
    const double c1 = 0.8359375;
    const double c2 = 18.8515625;
    const double c3 = 18.6875;
    for (int i = 0; i < FLUXER_HDR_PQ_LUT_SIZE; i++) {
        double e = (double)i / (double)(FLUXER_HDR_PQ_LUT_SIZE - 1);
        double ep = pow(e, 1.0 / m2);
        double num = ep - c1;
        if (num < 0.0) num = 0.0;
        double den = c2 - c3 * ep;
        double l = (den > 0.0) ? pow(num / den, 1.0 / m1) : 0.0;
        if (l < 0.0) l = 0.0;
        if (l > 1.0) l = 1.0;
        fluxer_pq_lut[i] = (float)l;
    }
    const double a = 0.17883277;
    const double b = 0.28466892;
    const double c = 0.55991073;
    const double gamma = 1.2;
    for (int i = 0; i < FLUXER_HDR_HLG_LUT_SIZE; i++) {
        double ep = (double)i / (double)(FLUXER_HDR_HLG_LUT_SIZE - 1);
        double e_scene;
        if (ep <= 0.5) {
            e_scene = (ep * ep) / 3.0;
        } else {
            e_scene = (exp((ep - c) / a) + b) / 12.0;
        }
        if (e_scene < 0.0) e_scene = 0.0;
        if (e_scene > 1.0) e_scene = 1.0;
        double e_display = pow(e_scene, gamma);
        if (e_display > 1.0) e_display = 1.0;
        fluxer_hlg_lut[i] = (float)e_display;
    }
    fluxer_pq_sdr_target_perc  = fluxer_pq_oetf(FLUXER_PQ_SDR_TARGET_NORM);
    fluxer_hlg_sdr_target_perc = fluxer_pq_oetf(FLUXER_HLG_SDR_TARGET_NORM);
}

static inline float fluxer_pq_inv_eotf(uint16_t code, int bit_depth) {
    int idx;
    if (bit_depth >= 12) {
        idx = code & 0x0FFF;
    } else {
        idx = ((int)(code & 0x03FF)) << 2;
    }
    if (idx < 0) idx = 0;
    if (idx >= FLUXER_HDR_PQ_LUT_SIZE) idx = FLUXER_HDR_PQ_LUT_SIZE - 1;
    return fluxer_pq_lut[idx];
}

static inline float fluxer_hlg_inv_eotf(uint16_t code, int bit_depth) {
    int idx;
    if (bit_depth >= 12) {
        idx = code & 0x0FFF;
    } else {
        idx = ((int)(code & 0x03FF)) << 2;
    }
    if (idx < 0) idx = 0;
    if (idx >= FLUXER_HDR_HLG_LUT_SIZE) idx = FLUXER_HDR_HLG_LUT_SIZE - 1;
    return fluxer_hlg_lut[idx];
}

static inline float fluxer_bt2390_eetf_perceptual(float x, float max_lum) {
    if (x <= 0.0f) return 0.0f;
    if (max_lum >= 1.0f) return (x > 1.0f) ? 1.0f : x;
    float ks = 1.5f * max_lum - 0.5f;
    if (x < ks) return x;
    if (x >= 1.0f) return max_lum;
    float t = (x - ks) / (1.0f - ks);
    float t2 = t * t;
    float t3 = t2 * t;
    float h00 = 2.0f * t3 - 3.0f * t2 + 1.0f;
    float h10 = t3 - 2.0f * t2 + t;
    float h01 = -2.0f * t3 + 3.0f * t2;
    float p = h00 * ks + h10 * (1.0f - ks) + h01 * max_lum;
    if (p > max_lum) p = max_lum;
    if (p < 0.0f) p = 0.0f;
    return p;
}

static inline float fluxer_pq_oetf(float l) {
    if (l <= 0.0f) return 0.0f;
    if (l >= 1.0f) l = 1.0f;
    const float m1 = 0.1593017578125f;
    const float m2 = 78.84375f;
    const float c1 = 0.8359375f;
    const float c2 = 18.8515625f;
    const float c3 = 18.6875f;
    float lm1 = powf(l, m1);
    float num = c1 + c2 * lm1;
    float den = 1.0f + c3 * lm1;
    return powf(num / den, m2);
}

static inline float fluxer_hlg_oetf_display(float dl) {
    if (dl <= 0.0f) return 0.0f;
    if (dl >= 1.0f) dl = 1.0f;
    float scene = powf(dl, 1.0f / 1.2f);
    if (scene <= 1.0f / 12.0f) return sqrtf(3.0f * scene);
    const float a = 0.17883277f;
    const float b = 0.28466892f;
    const float c = 0.55991073f;
    return a * logf(12.0f * scene - b) + c;
}

static inline float fluxer_srgb_oetf(float e) {
    if (e <= 0.0f) return 0.0f;
    if (e >= 1.0f) return 1.0f;
    if (e <= 0.0031308f) return 12.92f * e;
    return 1.055f * powf(e, 1.0f / 2.4f) - 0.055f;
}

static inline uint8_t fluxer_quantize8(float v) {
    if (v <= 0.0f) return 0;
    if (v >= 1.0f) return 255;
    int q = (int)(v * 255.0f + 0.5f);
    if (q < 0) return 0;
    if (q > 255) return 255;
    return (uint8_t)q;
}

static inline void fluxer_bt2020_to_bt709_linear(float r, float g, float b,
                                                 float *or_, float *og, float *ob) {
    float r_ =  1.6605f * r - 0.5876f * g - 0.0728f * b;
    float g_ = -0.1246f * r + 1.1329f * g - 0.0083f * b;
    float b_ = -0.0182f * r - 0.1006f * g + 1.1187f * b;
    *or_ = r_;
    *og = g_;
    *ob = b_;
}

static inline void fluxer_hdr_pipeline_pixel(float r, float g, float b,
                                             float sdr_target_norm,
                                             float sdr_target_perceptual,
                                             int do_gamut_conv,
                                             uint8_t *out_rgb) {
    float m = r;
    if (g > m) m = g;
    if (b > m) m = b;
    if (m <= 0.0f) {
        out_rgb[0] = out_rgb[1] = out_rgb[2] = 0;
        return;
    }
    float m_perc = fluxer_pq_oetf(m);
    float m_perc_mapped = fluxer_bt2390_eetf_perceptual(m_perc, sdr_target_perceptual);
    float m_mapped;
    {
        const float m1 = 0.1593017578125f;
        const float m2 = 78.84375f;
        const float c1 = 0.8359375f;
        const float c2 = 18.8515625f;
        const float c3 = 18.6875f;
        float ep = powf(m_perc_mapped, 1.0f / m2);
        float num = ep - c1;
        if (num < 0.0f) num = 0.0f;
        float den = c2 - c3 * ep;
        m_mapped = (den > 0.0f) ? powf(num / den, 1.0f / m1) : 0.0f;
        if (m_mapped < 0.0f) m_mapped = 0.0f;
    }
    float scale = (m_mapped / m) / sdr_target_norm;
    (void)sdr_target_perceptual;
    float dr = r * scale;
    float dg = g * scale;
    float db = b * scale;
    if (dr < 0.0f) dr = 0.0f;
    if (dg < 0.0f) dg = 0.0f;
    if (db < 0.0f) db = 0.0f;
    if (dr > 1.0f) dr = 1.0f;
    if (dg > 1.0f) dg = 1.0f;
    if (db > 1.0f) db = 1.0f;

    float lr = dr, lg = dg, lb = db;
    if (do_gamut_conv) {
        fluxer_bt2020_to_bt709_linear(dr, dg, db, &lr, &lg, &lb);
    }
    if (lr < 0.0f) lr = 0.0f;
    if (lg < 0.0f) lg = 0.0f;
    if (lb < 0.0f) lb = 0.0f;
    if (lr > 1.0f) lr = 1.0f;
    if (lg > 1.0f) lg = 1.0f;
    if (lb > 1.0f) lb = 1.0f;

    out_rgb[0] = fluxer_quantize8(fluxer_srgb_oetf(lr));
    out_rgb[1] = fluxer_quantize8(fluxer_srgb_oetf(lg));
    out_rgb[2] = fluxer_quantize8(fluxer_srgb_oetf(lb));
}

int fluxer_hdr_to_sdr_test(uint16_t r, uint16_t g, uint16_t b,
                           int bit_depth, int transfer,
                           uint8_t out_rgb[3]) {
    if (out_rgb == NULL) return -1;
    if (bit_depth != 10 && bit_depth != 12) return -1;
    pthread_once(&fluxer_hdr_lut_once, fluxer_init_hdr_luts);
    float lr, lg, lb, target_norm, target_perc;
    if (transfer == 16) {
        lr = fluxer_pq_inv_eotf(r, bit_depth);
        lg = fluxer_pq_inv_eotf(g, bit_depth);
        lb = fluxer_pq_inv_eotf(b, bit_depth);
        target_norm = FLUXER_PQ_SDR_TARGET_NORM;
        target_perc = fluxer_pq_sdr_target_perc;
    } else if (transfer == 18) {
        lr = fluxer_hlg_inv_eotf(r, bit_depth);
        lg = fluxer_hlg_inv_eotf(g, bit_depth);
        lb = fluxer_hlg_inv_eotf(b, bit_depth);
        target_norm = FLUXER_HLG_SDR_TARGET_NORM;
        target_perc = fluxer_hlg_sdr_target_perc;
    } else {
        return -1;
    }
    fluxer_hdr_pipeline_pixel(lr, lg, lb, target_norm, target_perc,1, out_rgb);
    return 0;
}

static unsigned char fluxer_ascii_lower(unsigned char c) {
    if (c >= 'A' && c <= 'Z') return (unsigned char)(c + ('a' - 'A'));
    return c;
}

static int fluxer_ascii_contains_folded(const char *haystack, const char *needle) {
    if (haystack == NULL || needle == NULL || needle[0] == '\0') return 0;
    size_t needle_len = strlen(needle);
    for (const char *p = haystack; *p != '\0'; p++) {
        size_t i = 0;
        while (i < needle_len && p[i] != '\0' &&
               fluxer_ascii_lower((unsigned char)p[i]) ==
               fluxer_ascii_lower((unsigned char)needle[i])) {
            i++;
        }
        if (i == needle_len) return 1;
    }
    return 0;
}

static int heif_aux_type_is_hdr_gain_map(const char *type) {
    if (type == NULL || type[0] == '\0') return 0;

    if (fluxer_ascii_contains_folded(type, "hdrgainmap") ||
        fluxer_ascii_contains_folded(type, "hdr_gain_map") ||
        fluxer_ascii_contains_folded(type, "hdr-gain-map")) {
        return 1;
    }
    if (fluxer_ascii_contains_folded(type, "gainmap") &&
        (fluxer_ascii_contains_folded(type, "hdr") ||
         fluxer_ascii_contains_folded(type, "21496") ||
         fluxer_ascii_contains_folded(type, "iso"))) {
        return 1;
    }
    return 0;
}

int fluxer_heif_aux_type_is_hdr_gain_map_for_test(const char *type) {
    return heif_aux_type_is_hdr_gain_map(type);
}

static int heif_handle_has_hdr_gain_map(struct heif_image_handle *handle) {
    if (handle == NULL) return 0;
    const int filter = LIBHEIF_AUX_IMAGE_FILTER_OMIT_ALPHA |
                       LIBHEIF_AUX_IMAGE_FILTER_OMIT_DEPTH;
    int count = heif_image_handle_get_number_of_auxiliary_images(handle, filter);
    if (count <= 0) return 0;
    if (count > 4096) return 0;

    heif_item_id *ids = (heif_item_id *)calloc((size_t)count, sizeof(heif_item_id));
    if (ids == NULL) return 0;
    int got = heif_image_handle_get_list_of_auxiliary_image_IDs(handle, filter, ids, count);
    if (got <= 0) {
        free(ids);
        return 0;
    }
    if (got > count) got = count;

    int found = 0;
    for (int i = 0; i < got; i++) {
        struct heif_image_handle *aux_handle = NULL;
        struct heif_error err = heif_image_handle_get_auxiliary_image_handle(handle, ids[i], &aux_handle);
        if (err.code != heif_error_Ok || aux_handle == NULL) continue;

        const char *aux_type = NULL;
        err = heif_image_handle_get_auxiliary_type(aux_handle, &aux_type);
        if (err.code == heif_error_Ok && aux_type != NULL) {
            found = heif_aux_type_is_hdr_gain_map(aux_type);
            heif_image_handle_release_auxiliary_type(aux_handle, &aux_type);
        }
        heif_image_handle_release(aux_handle);
        if (found) break;
    }

    free(ids);
    return found;
}

static int decode_heif_image_to_sdr_rgba8(struct heif_image_handle *handle,
                                          uint8_t *dst,
                                          size_t dst_cap,
                                          int width,
                                          int height,
                                          int *out_was_hdr) {
    if (handle == NULL || dst == NULL || width <= 0 || height <= 0) return -1;
    if (dst_cap < (size_t)width * (size_t)height * 4u) return -1;
    if (out_was_hdr) *out_was_hdr = 0;

    int transfer = 0;
    int primaries = 0;
    {
        struct heif_color_profile_nclx *nclx = NULL;
        if (heif_image_handle_get_nclx_color_profile(handle, &nclx).code == heif_error_Ok && nclx != NULL) {
            transfer = (int)nclx->transfer_characteristics;
            primaries = (int)nclx->color_primaries;
            heif_nclx_color_profile_free(nclx);
        }
    }
    int is_hdr = (transfer == 16 || transfer == 18);

    if (!is_hdr) {
        struct heif_image *img = NULL;
        struct heif_decoding_options *opts = heif_decoding_options_alloc();
        struct heif_error derr = heif_decode_image(handle, &img,
                                                   heif_colorspace_RGB,
                                                   heif_chroma_interleaved_RGBA, opts);
        if (opts) heif_decoding_options_free(opts);
        if (derr.code != heif_error_Ok || img == NULL) return -1;

        int img_w = heif_image_get_primary_width(img);
        int img_h = heif_image_get_primary_height(img);
        int stride = 0;
        const uint8_t *plane = heif_image_get_plane_readonly(img, heif_channel_interleaved, &stride);
        int copied = 0;
        if (plane != NULL && stride > 0 && img_w > 0 && img_h > 0) {
            int copy_w = img_w < width ? img_w : width;
            int copy_h = img_h < height ? img_h : height;
            size_t row_bytes = (size_t)width * 4u;
            for (int y = 0; y < copy_h; y++) {
                memcpy(dst + (size_t)y * row_bytes,
                       plane + (size_t)y * (size_t)stride,
                       (size_t)copy_w * 4u);
            }
            copied = copy_h;
        }
        heif_image_release(img);
        return copied;
    }

    pthread_once(&fluxer_hdr_lut_once, fluxer_init_hdr_luts);
    if (out_was_hdr) *out_was_hdr = 1;

    int luma_bpp = heif_image_handle_get_luma_bits_per_pixel(handle);
    if (luma_bpp != 10 && luma_bpp != 12) {
        if (out_was_hdr) *out_was_hdr = 0;
        struct heif_image *img = NULL;
        struct heif_decoding_options *opts = heif_decoding_options_alloc();
        struct heif_error derr = heif_decode_image(handle, &img,
                                                   heif_colorspace_RGB,
                                                   heif_chroma_interleaved_RGBA, opts);
        if (opts) heif_decoding_options_free(opts);
        if (derr.code != heif_error_Ok || img == NULL) return -1;
        int img_w = heif_image_get_primary_width(img);
        int img_h = heif_image_get_primary_height(img);
        int stride = 0;
        const uint8_t *plane = heif_image_get_plane_readonly(img, heif_channel_interleaved, &stride);
        int copied = 0;
        if (plane != NULL && stride > 0) {
            int copy_w = img_w < width ? img_w : width;
            int copy_h = img_h < height ? img_h : height;
            size_t row_bytes = (size_t)width * 4u;
            for (int y = 0; y < copy_h; y++) {
                memcpy(dst + (size_t)y * row_bytes,
                       plane + (size_t)y * (size_t)stride,
                       (size_t)copy_w * 4u);
            }
            copied = copy_h;
        }
        heif_image_release(img);
        return copied;
    }

    struct heif_image *img = NULL;
    struct heif_decoding_options *opts = heif_decoding_options_alloc();
    struct heif_error derr = heif_decode_image(handle, &img,
                                               heif_colorspace_RGB,
                                               heif_chroma_interleaved_RRGGBBAA_LE, opts);
    if (opts) heif_decoding_options_free(opts);
    if (derr.code != heif_error_Ok || img == NULL) return -1;

    int img_w = heif_image_get_primary_width(img);
    int img_h = heif_image_get_primary_height(img);
    int stride = 0;
    const uint8_t *plane = heif_image_get_plane_readonly(img, heif_channel_interleaved, &stride);
    int copied = 0;
    if (plane != NULL && stride > 0 && img_w > 0 && img_h > 0) {
        int copy_w = img_w < width ? img_w : width;
        int copy_h = img_h < height ? img_h : height;
        size_t row_bytes = (size_t)width * 4u;
        const float target_norm = (transfer == 16)
                                ? FLUXER_PQ_SDR_TARGET_NORM
                                : FLUXER_HLG_SDR_TARGET_NORM;
        const float target_perc = (transfer == 16)
                                ? fluxer_pq_sdr_target_perc
                                : fluxer_hlg_sdr_target_perc;
        const int do_gamut_conv = (primaries == 9);

        const int mask = (1 << luma_bpp) - 1;

        for (int y = 0; y < copy_h; y++) {
            const uint16_t *src_row = (const uint16_t *)(plane + (size_t)y * (size_t)stride);
            uint8_t *dst_row = dst + (size_t)y * row_bytes;
            for (int x = 0; x < copy_w; x++) {
                uint16_t r16 = src_row[(size_t)x * 4 + 0];
                uint16_t g16 = src_row[(size_t)x * 4 + 1];
                uint16_t b16 = src_row[(size_t)x * 4 + 2];
                uint16_t a16 = src_row[(size_t)x * 4 + 3];
                uint16_t rc = (uint16_t)(r16 & mask);
                uint16_t gc = (uint16_t)(g16 & mask);
                uint16_t bc = (uint16_t)(b16 & mask);
                float lr, lg, lb;
                if (transfer == 16) {
                    lr = fluxer_pq_inv_eotf(rc, luma_bpp);
                    lg = fluxer_pq_inv_eotf(gc, luma_bpp);
                    lb = fluxer_pq_inv_eotf(bc, luma_bpp);
                } else {
                    lr = fluxer_hlg_inv_eotf(rc, luma_bpp);
                    lg = fluxer_hlg_inv_eotf(gc, luma_bpp);
                    lb = fluxer_hlg_inv_eotf(bc, luma_bpp);
                }
                uint8_t out3[3];
                fluxer_hdr_pipeline_pixel(lr, lg, lb, target_norm, target_perc,
                                          do_gamut_conv, out3);
                dst_row[(size_t)x * 4 + 0] = out3[0];
                dst_row[(size_t)x * 4 + 1] = out3[1];
                dst_row[(size_t)x * 4 + 2] = out3[2];
                uint16_t ac = (uint16_t)(a16 & mask);
                dst_row[(size_t)x * 4 + 3] = (uint8_t)((ac * 255 + (mask >> 1)) / mask);
            }
        }
        copied = copy_h;
    }
    heif_image_release(img);
    return copied;
}


typedef struct {
    const uint8_t *data;
    size_t         len;
} bmff_buf;

static uint32_t bmff_read_u32(const uint8_t *p) {
    return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
           ((uint32_t)p[2] <<  8) |  (uint32_t)p[3];
}
static uint16_t bmff_read_u16(const uint8_t *p) {
    return (uint16_t)(((uint16_t)p[0] << 8) | (uint16_t)p[1]);
}
static uint64_t bmff_read_u64(const uint8_t *p) {
    return ((uint64_t)bmff_read_u32(p) << 32) | (uint64_t)bmff_read_u32(p + 4);
}

typedef int (*bmff_box_cb)(const uint8_t *payload, size_t payload_len, void *user);

static int bmff_walk(const uint8_t *data, size_t start, size_t end,
                     const char *target, bmff_box_cb cb, void *user,
                     int meta_full_box) {
    size_t off = start;
    if (meta_full_box) {
        if (off + 4 > end) return 0;
        off += 4;
    }
    while (off + 8 <= end) {
        uint32_t size32 = bmff_read_u32(data + off);
        const uint8_t *btype = data + off + 4;
        uint64_t box_size = size32;
        size_t   header_len = 8;
        if (size32 == 1) {
            if (off + 16 > end) return 0;
            box_size = bmff_read_u64(data + off + 8);
            header_len = 16;
        } else if (size32 == 0) {
            box_size = (uint64_t)(end - off);
        }
        if (box_size < header_len || off + box_size > end) return 0;
        size_t child_start = off + header_len;
        size_t child_end   = (size_t)(off + box_size);

        if (memcmp(btype, target, 4) == 0) {
            int r = cb(data + child_start, child_end - child_start, user);
            if (r) return r;
        }
        if (memcmp(btype, "moov", 4) == 0 ||
            memcmp(btype, "trak", 4) == 0 ||
            memcmp(btype, "mdia", 4) == 0 ||
            memcmp(btype, "minf", 4) == 0 ||
            memcmp(btype, "stbl", 4) == 0 ||
            memcmp(btype, "edts", 4) == 0 ||
            memcmp(btype, "dinf", 4) == 0) {
            int r = bmff_walk(data, child_start, child_end, target, cb, user, 0);
            if (r) return r;
        } else if (memcmp(btype, "meta", 4) == 0) {
            int r = bmff_walk(data, child_start, child_end, target, cb, user, 1);
            if (r) return r;
        }

        off = child_end;
    }
    return 0;
}

typedef struct {
    int     have_handler;
    uint8_t handler_type[4];
    int     have_mdhd;
    uint32_t timescale;
    int     have_stts;
    const uint8_t *stts_payload;
    size_t         stts_len;
} trak_info;

static int cb_collect_mdhd(const uint8_t *payload, size_t len, void *user) {
    trak_info *t = (trak_info *)user;
    if (len < 1) return 0;
    uint8_t version = payload[0];
    uint32_t timescale;
    if (version == 0) {
        if (len < 16) return 0;
        timescale = bmff_read_u32(payload + 12);
    } else if (version == 1) {
        if (len < 24) return 0;
        timescale = bmff_read_u32(payload + 20);
    } else {
        return 0;
    }
    t->timescale = timescale;
    t->have_mdhd = timescale > 0;
    return 0;
}

static int cb_collect_hdlr(const uint8_t *payload, size_t len, void *user) {
    trak_info *t = (trak_info *)user;
    if (len < 12) return 0;
    memcpy(t->handler_type, payload + 8, 4);
    t->have_handler = 1;
    return 0;
}

static int cb_collect_stts(const uint8_t *payload, size_t len, void *user) {
    trak_info *t = (trak_info *)user;
    if (len < 8) return 0;
    t->stts_payload = payload;
    t->stts_len = len;
    t->have_stts = 1;
    return 0;
}

typedef struct {
    trak_info *traks;
    size_t     count;
    size_t     cap;
} trak_list;

static int cb_each_trak(const uint8_t *payload, size_t len, void *user) {
    trak_list *list = (trak_list *)user;
    if (list->count >= list->cap) {
        size_t new_cap = list->cap == 0 ? 4 : list->cap * 2;
        trak_info *grown = (trak_info *)realloc(list->traks, new_cap * sizeof(trak_info));
        if (grown == NULL) return -1;
        list->traks = grown;
        list->cap = new_cap;
    }
    trak_info *t = &list->traks[list->count];
    memset(t, 0, sizeof(*t));
    bmff_walk(payload, 0, len, "mdhd", cb_collect_mdhd, t, 0);
    bmff_walk(payload, 0, len, "hdlr", cb_collect_hdlr, t, 0);
    bmff_walk(payload, 0, len, "stts", cb_collect_stts, t, 0);
    list->count++;
    return 0;
}

static int parse_isobmff_track_delays(const void *buf, size_t len,
                                      int **out_delays_ms, int *out_n_samples) {
    if (out_delays_ms) *out_delays_ms = NULL;
    if (out_n_samples) *out_n_samples = 0;
    if (buf == NULL || len < 16 || out_delays_ms == NULL || out_n_samples == NULL) {
        return -1;
    }

    const uint8_t *data = (const uint8_t *)buf;
    trak_list list = { NULL, 0, 0 };
    if (bmff_walk(data, 0, len, "trak", cb_each_trak, &list, 0) != 0) {
        free(list.traks);
        return -1;
    }
    if (list.count == 0) {
        free(list.traks);
        return -1;
    }

    trak_info *picked = NULL;
    for (size_t i = 0; i < list.count; i++) {
        trak_info *t = &list.traks[i];
        if (!t->have_handler || !t->have_mdhd || !t->have_stts) continue;
        int is_pict = memcmp(t->handler_type, "pict", 4) == 0;
        int is_vide = memcmp(t->handler_type, "vide", 4) == 0;
        if (is_pict || is_vide) { picked = t; break; }
    }
    if (picked == NULL) {
        free(list.traks);
        return -1;
    }

    if (picked->stts_len < 8) { free(list.traks); return -1; }
    uint32_t entry_count = bmff_read_u32(picked->stts_payload + 4);
    if (entry_count == 0) { free(list.traks); return -1; }
    if ((size_t)8 + (size_t)entry_count * 8u > picked->stts_len) {
        free(list.traks);
        return -1;
    }

    uint64_t total_samples = 0;
    for (uint32_t i = 0; i < entry_count; i++) {
        uint32_t cnt = bmff_read_u32(picked->stts_payload + 8 + (size_t)i * 8u);
        total_samples += cnt;
        if (total_samples > 16384) {
            free(list.traks);
            return -1;
        }
    }
    if (total_samples == 0) { free(list.traks); return -1; }

    uint32_t timescale = picked->timescale;
    if (timescale == 0) { free(list.traks); return -1; }
    int *delays = (int *)malloc((size_t)total_samples * sizeof(int));
    if (delays == NULL) { free(list.traks); return -1; }

    size_t out_idx = 0;
    for (uint32_t i = 0; i < entry_count; i++) {
        uint32_t cnt   = bmff_read_u32(picked->stts_payload + 8 + (size_t)i * 8u);
        uint32_t delta = bmff_read_u32(picked->stts_payload + 12 + (size_t)i * 8u);
        int delta_ms = (int)(((uint64_t)delta * 1000ULL) / (uint64_t)timescale);
        if (delta_ms < 0) delta_ms = 0;
        for (uint32_t k = 0; k < cnt; k++) {
            delays[out_idx++] = delta_ms;
        }
    }

    free(list.traks);
    *out_delays_ms = delays;
    *out_n_samples = (int)total_samples;
    return 0;
}

static int cb_find_iinf_tmap(const uint8_t *payload, size_t len, void *user) {
    int *found = (int *)user;
    if (found == NULL || len < 6) return 0;

    uint8_t version = payload[0];
    size_t off = 4;
    uint32_t entry_count = 0;
    if (version == 0) {
        if (len < off + 2) return 0;
        entry_count = bmff_read_u16(payload + off);
        off += 2;
    } else {
        if (len < off + 4) return 0;
        entry_count = bmff_read_u32(payload + off);
        off += 4;
    }

    for (uint32_t entry = 0; entry < entry_count && off + 8 <= len; entry++) {
        uint32_t size32 = bmff_read_u32(payload + off);
        const uint8_t *btype = payload + off + 4;
        uint64_t box_size = size32;
        size_t header_len = 8;
        if (size32 == 1) {
            if (off + 16 > len) return 0;
            box_size = bmff_read_u64(payload + off + 8);
            header_len = 16;
        } else if (size32 == 0) {
            box_size = (uint64_t)(len - off);
        }
        if (box_size < header_len || off + box_size > len) return 0;

        size_t child_start = off + header_len;
        size_t child_end = (size_t)(off + box_size);
        if (memcmp(btype, "infe", 4) == 0 && child_end > child_start) {
            const uint8_t *infe = payload + child_start;
            size_t infe_len = child_end - child_start;
            if (infe_len >= 12) {
                uint8_t infe_version = infe[0];
                size_t pos = 4;
                if (infe_version == 2) {
                    if (infe_len < pos + 2 + 2 + 4) return 0;
                    pos += 2;
                } else if (infe_version == 3) {
                    if (infe_len < pos + 4 + 2 + 4) return 0;
                    pos += 4;
                } else {
                    off = child_end;
                    continue;
                }
                pos += 2;
                if (pos + 4 <= infe_len && memcmp(infe + pos, "tmap", 4) == 0) {
                    *found = 1;
                    return 1;
                }
            }
        }
        off = child_end;
    }
    return 0;
}

static int parse_isobmff_has_tmap_item(const void *buf, size_t len) {
    if (buf == NULL || len < 16) return 0;
    int found = 0;
    const uint8_t *data = (const uint8_t *)buf;
    bmff_walk(data, 0, len, "iinf", cb_find_iinf_tmap, &found, 0);
    return found;
}

int fluxer_heif_has_tmap_item_for_test(const void *buf, size_t len) {
    return parse_isobmff_has_tmap_item(buf, len);
}

int fluxer_avif_parse_track_delays_for_test(const void *buf, size_t len,
                                            int **out_delays_ms, int *out_n_samples) {
    return parse_isobmff_track_delays(buf, len, out_delays_ms, out_n_samples);
}

void fluxer_avif_free_delays(int *delays) {
    free(delays);
}

void fluxer_vips_set_anim_metadata_for_test(VipsImage *image, int page_height,
                                            int n_pages, const int *delays_ms) {
    if (image == NULL) return;
    if (page_height > 0) vips_image_set_int(image, "page-height", page_height);
    if (n_pages > 1) vips_image_set_int(image, "n-pages", n_pages);
    if (delays_ms != NULL && n_pages > 0) {
        vips_image_set_array_int(image, "delay", delays_ms, n_pages);
    }
}

int fluxer_heif_decode_animated(const void *buf, size_t len, VipsImage **out,
                                int n_max_pages, size_t max_total_pixels) {
    return fluxer_heif_decode_animated_ex2(buf, len, out, n_max_pages, max_total_pixels, NULL, NULL);
}

int fluxer_heif_decode_animated_ex(const void *buf, size_t len, VipsImage **out,
                                   int n_max_pages, size_t max_total_pixels,
                                   int *was_hdr) {
    return fluxer_heif_decode_animated_ex2(buf, len, out, n_max_pages, max_total_pixels, was_hdr, NULL);
}

int fluxer_heif_decode_animated_ex2(const void *buf, size_t len, VipsImage **out,
                                    int n_max_pages, size_t max_total_pixels,
                                    int *was_hdr, int *had_hdr_gain_map) {
    if (buf == NULL || len == 0 || out == NULL) return -1;
    *out = NULL;
    if (was_hdr) *was_hdr = 0;
    if (had_hdr_gain_map) *had_hdr_gain_map = 0;
    if (had_hdr_gain_map && parse_isobmff_has_tmap_item(buf, len)) {
        *had_hdr_gain_map = 1;
    }

    struct heif_context *ctx = heif_context_alloc();
    if (ctx == NULL) return -1;

    struct heif_error err = heif_context_read_from_memory_without_copy(ctx, buf, len, NULL);
    if (err.code != heif_error_Ok) {
        heif_context_free(ctx);
        return -1;
    }

    int total_imgs = heif_context_get_number_of_top_level_images(ctx);
    if (total_imgs <= 0) {
        heif_context_free(ctx);
        return -1;
    }
    int n_pages = total_imgs;
    if (n_max_pages > 0 && n_max_pages < n_pages) n_pages = n_max_pages;
    if (n_pages > 1024) n_pages = 1024;

    heif_item_id *ids = (heif_item_id *)calloc((size_t)total_imgs, sizeof(heif_item_id));
    if (ids == NULL) {
        heif_context_free(ctx);
        return -1;
    }
    int got_ids = heif_context_get_list_of_top_level_image_IDs(ctx, ids, total_imgs);
    if (got_ids <= 0) {
        free(ids);
        heif_context_free(ctx);
        return -1;
    }

    int canvas_w = 0;
    int canvas_h = 0;
    {
        struct heif_image_handle *handle = NULL;
        if (heif_context_get_image_handle(ctx, ids[0], &handle).code != heif_error_Ok || handle == NULL) {
            free(ids);
            heif_context_free(ctx);
            return -1;
        }
        canvas_w = heif_image_handle_get_width(handle);
        canvas_h = heif_image_handle_get_height(handle);
        heif_image_handle_release(handle);
    }
    if (canvas_w <= 0 || canvas_h <= 0) {
        free(ids);
        heif_context_free(ctx);
        return -1;
    }
    if (max_total_pixels > 0) {
        size_t per_frame = (size_t)canvas_w * (size_t)canvas_h;
        if (per_frame == 0 || per_frame > max_total_pixels / (size_t)n_pages) {
            free(ids);
            heif_context_free(ctx);
            return -1;
        }
    }

    size_t row_bytes = (size_t)canvas_w * 4u;
    size_t per_page_bytes = row_bytes * (size_t)canvas_h;
    size_t total_bytes = per_page_bytes * (size_t)n_pages;
    uint8_t *stacked = (uint8_t *)g_try_malloc0(total_bytes);
    if (stacked == NULL) {
        free(ids);
        heif_context_free(ctx);
        return -1;
    }

    int decoded = 0;
    for (int i = 0; i < n_pages; i++) {
        struct heif_image_handle *handle = NULL;
        if (heif_context_get_image_handle(ctx, ids[i], &handle).code != heif_error_Ok || handle == NULL) {
            continue;
        }
        uint8_t *dst = stacked + (size_t)i * per_page_bytes;
        int page_was_hdr = 0;
        if (had_hdr_gain_map && *had_hdr_gain_map == 0 &&
            heif_handle_has_hdr_gain_map(handle)) {
            *had_hdr_gain_map = 1;
        }
        int copied = decode_heif_image_to_sdr_rgba8(handle, dst, per_page_bytes,
                                                    canvas_w, canvas_h,
                                                    &page_was_hdr);
        if (copied > 0) {
            decoded++;
            if (was_hdr && page_was_hdr) *was_hdr = 1;
        }
        heif_image_handle_release(handle);
    }

    free(ids);
    heif_context_free(ctx);

    if (decoded == 0) {
        g_free(stacked);
        return -1;
    }

    VipsImage *image = vips_image_new_from_memory(stacked, total_bytes,
                                                  canvas_w,
                                                  canvas_h * decoded,
                                                  4,
                                                  VIPS_FORMAT_UCHAR);
    if (image == NULL) {
        g_free(stacked);
        return -1;
    }
    g_signal_connect_swapped(image, "postclose", G_CALLBACK(g_free), stacked);

    vips_image_set_int(image, "page-height", canvas_h);
    if (decoded > 1) vips_image_set_int(image, "n-pages", decoded);

    if (decoded > 1) {
        int *delays = NULL;
        int n_samples = 0;
        if (parse_isobmff_track_delays(buf, len, &delays, &n_samples) == 0 &&
            delays != NULL && n_samples >= decoded) {
            vips_image_set_array_int(image, "delay", delays, decoded);
            free(delays);
        } else {
            if (delays != NULL) free(delays);
            g_warning("fluxer_heif_decode_animated_ex: stts parse failed "
                      "(decoded=%d, n_samples=%d) — falling back to uniform "
                      "100ms delay", decoded, n_samples);
        }
    }

    *out = image;
    return 0;
}

int fluxer_av_probe(
    const void *media_data,
    size_t media_len,
    int *out_has_video,
    int *out_has_audio,
    double *out_duration_seconds
) {
    if (media_data == NULL || media_len == 0 ||
        out_has_video == NULL || out_has_audio == NULL || out_duration_seconds == NULL) {
        return -1;
    }
    *out_has_video = 0;
    *out_has_audio = 0;
    *out_duration_seconds = 0.0;

    int rc = -1;
    struct ff_mem_reader reader = { .data = (const uint8_t *)media_data, .len = media_len, .offset = 0 };
    unsigned char *avio_buffer = NULL;
    AVIOContext *avio = NULL;
    AVFormatContext *in_fmt = NULL;

    avio_buffer = av_malloc(64 * 1024);
    if (avio_buffer == NULL) goto cleanup;
    avio = avio_alloc_context(avio_buffer, 64 * 1024, 0, &reader,
                              ff_mem_read_packet, NULL, ff_mem_seek);
    if (avio == NULL) {
        av_free(avio_buffer);
        avio_buffer = NULL;
        goto cleanup;
    }
    in_fmt = avformat_alloc_context();
    if (in_fmt == NULL) goto cleanup;
    in_fmt->pb = avio;
    in_fmt->flags |= AVFMT_FLAG_CUSTOM_IO;
    in_fmt->probesize = 5 * 1024 * 1024;
    in_fmt->max_analyze_duration = 5 * AV_TIME_BASE;
    if (avformat_open_input(&in_fmt, NULL, NULL, NULL) < 0) goto cleanup;
    if (avformat_find_stream_info(in_fmt, NULL) < 0) goto cleanup;

    for (unsigned i = 0; i < in_fmt->nb_streams; i++) {
        AVStream *st = in_fmt->streams[i];
        if (st == NULL || st->codecpar == NULL) continue;
        int is_attached_picture = 0;
#ifdef AV_DISPOSITION_ATTACHED_PIC
        if ((st->disposition & AV_DISPOSITION_ATTACHED_PIC) != 0) is_attached_picture = 1;
#endif
#ifdef AV_DISPOSITION_TIMED_THUMBNAILS
        if ((st->disposition & AV_DISPOSITION_TIMED_THUMBNAILS) != 0) is_attached_picture = 1;
#endif
#ifdef AV_DISPOSITION_STILL_IMAGE
        if ((st->disposition & AV_DISPOSITION_STILL_IMAGE) != 0) is_attached_picture = 1;
#endif
        if (st->codecpar->codec_type == AVMEDIA_TYPE_VIDEO && !is_attached_picture) *out_has_video = 1;
        if (st->codecpar->codec_type == AVMEDIA_TYPE_AUDIO) *out_has_audio = 1;
    }
    if (in_fmt->duration > 0) {
        *out_duration_seconds = (double)in_fmt->duration / (double)AV_TIME_BASE;
    }
    if (*out_has_video || *out_has_audio) rc = 0;

cleanup:
    if (in_fmt != NULL) avformat_close_input(&in_fmt);
    if (avio != NULL) {
        if (avio->buffer != NULL) {
            av_free(avio->buffer);
            avio->buffer = NULL;
        }
        avio_context_free(&avio);
    } else if (avio_buffer != NULL) {
        av_free(avio_buffer);
    }
    return rc;
}

#define FLUXER_MAX_VIDEO_PACKETS_FOR_NSFW 512

void fluxer_nsfw_frames_free(struct fluxer_nsfw_frame_out *frames, size_t n) {
    if (frames == NULL) return;
    for (size_t i = 0; i < n; i++) {
        if (frames[i].data != NULL) {
            g_free(frames[i].data);
            frames[i].data = NULL;
        }
        frames[i].len = 0;
    }
}

int fluxer_av_extract_frames_for_nsfw(
    const void *media_data,
    size_t media_len,
    const double *timestamps_secs,
    size_t n_timestamps,
    struct fluxer_nsfw_frame_out *out_frames
) {
    if (media_data == NULL || media_len == 0 || timestamps_secs == NULL ||
        out_frames == NULL || n_timestamps == 0) {
        return -1;
    }
    for (size_t i = 0; i < n_timestamps; i++) {
        out_frames[i].data = NULL;
        out_frames[i].len = 0;
    }

    struct ff_mem_reader reader = { .data = (const uint8_t *)media_data, .len = media_len, .offset = 0 };
    unsigned char *avio_buffer = NULL;
    AVIOContext *avio = NULL;
    AVFormatContext *in_fmt = NULL;
    AVCodecContext *dec_ctx = NULL;
    AVPacket *packet = NULL;
    AVFrame *frame = NULL;
    int produced = 0;

    avio_buffer = av_malloc(64 * 1024);
    if (avio_buffer == NULL) goto cleanup;
    avio = avio_alloc_context(avio_buffer, 64 * 1024, 0, &reader,
                              ff_mem_read_packet, NULL, ff_mem_seek);
    if (avio == NULL) {
        av_free(avio_buffer);
        avio_buffer = NULL;
        goto cleanup;
    }
    in_fmt = avformat_alloc_context();
    if (in_fmt == NULL) goto cleanup;
    in_fmt->pb = avio;
    in_fmt->flags |= AVFMT_FLAG_CUSTOM_IO;
    in_fmt->probesize = 5 * 1024 * 1024;
    in_fmt->max_analyze_duration = 5 * AV_TIME_BASE;
    if (avformat_open_input(&in_fmt, NULL, NULL, NULL) < 0) goto cleanup;
    if (avformat_find_stream_info(in_fmt, NULL) < 0) goto cleanup;

    const AVCodec *decoder = NULL;
    int stream_index = av_find_best_stream(in_fmt, AVMEDIA_TYPE_VIDEO, -1, -1, &decoder, 0);
    if (stream_index < 0) goto cleanup;
    AVStream *in_stream = in_fmt->streams[stream_index];
    if (in_stream == NULL || in_stream->codecpar == NULL) goto cleanup;
    int vw = in_stream->codecpar->width;
    int vh = in_stream->codecpar->height;
    if (vw <= 0 || vh <= 0 || vw > 16384 || vh > 16384) goto cleanup;
    if (decoder == NULL) decoder = avcodec_find_decoder(in_stream->codecpar->codec_id);
    if (decoder == NULL) goto cleanup;
    dec_ctx = avcodec_alloc_context3(decoder);
    if (dec_ctx == NULL) goto cleanup;
    if (avcodec_parameters_to_context(dec_ctx, in_stream->codecpar) < 0) goto cleanup;
    if (avcodec_open2(dec_ctx, decoder, NULL) < 0) goto cleanup;

    packet = av_packet_alloc();
    frame = av_frame_alloc();
    if (packet == NULL || frame == NULL) goto cleanup;

    double tb_num = (double)in_stream->time_base.num;
    double tb_den = (double)in_stream->time_base.den;
    int have_tb = (tb_num > 0.0 && tb_den > 0.0);

    for (size_t i = 0; i < n_timestamps; i++) {
        double ts_s = timestamps_secs[i];
        if (!isfinite(ts_s) || ts_s < 0.0) ts_s = 0.0;

        int64_t target_pts;
        if (have_tb) {
            target_pts = (int64_t)(ts_s / tb_num * tb_den);
        } else {
            target_pts = (int64_t)(ts_s * (double)AV_TIME_BASE);
        }
        (void)avformat_seek_file(in_fmt, stream_index, INT64_MIN, target_pts,
                                 target_pts, AVSEEK_FLAG_BACKWARD);
        avcodec_flush_buffers(dec_ctx);

        int packet_count = 0;
        int got_frame = 0;
        while (av_read_frame(in_fmt, packet) >= 0) {
            packet_count++;
            if (packet_count > FLUXER_MAX_VIDEO_PACKETS_FOR_NSFW) {
                av_packet_unref(packet);
                break;
            }
            if (packet->stream_index != stream_index) {
                av_packet_unref(packet);
                continue;
            }
            int send_rc = avcodec_send_packet(dec_ctx, packet);
            av_packet_unref(packet);
            if (send_rc < 0) continue;
            int loop_break = 0;
            while (1) {
                int recv_rc = avcodec_receive_frame(dec_ctx, frame);
                if (recv_rc == AVERROR(EAGAIN) || recv_rc == AVERROR_EOF) break;
                if (recv_rc < 0) { loop_break = 1; break; }

                void *out_buf = NULL;
                size_t out_size = 0;
                int emit_rc = ff_emit_frame_thumbnail(
                    frame, dec_ctx, in_fmt, in_stream, ".jpg[Q=65,strip]",
                    &out_buf, &out_size);
                av_frame_unref(frame);
                if (emit_rc != 0 || out_buf == NULL || out_size == 0) {
                    loop_break = 1;
                    break;
                }
                out_frames[i].data = out_buf;
                out_frames[i].len = out_size;
                produced++;
                got_frame = 1;
                loop_break = 1;
                break;
            }
            if (got_frame || loop_break) break;
        }

        if (!got_frame) {
            (void)avcodec_send_packet(dec_ctx, NULL);
            int recv_rc = avcodec_receive_frame(dec_ctx, frame);
            if (recv_rc == 0) {
                void *out_buf = NULL;
                size_t out_size = 0;
                int emit_rc = ff_emit_frame_thumbnail(
                    frame, dec_ctx, in_fmt, in_stream, ".jpg[Q=65,strip]",
                    &out_buf, &out_size);
                av_frame_unref(frame);
                if (emit_rc == 0 && out_buf != NULL && out_size > 0) {
                    out_frames[i].data = out_buf;
                    out_frames[i].len = out_size;
                    produced++;
                }
            }
            (void)avformat_seek_file(in_fmt, stream_index, INT64_MIN, 0, INT64_MAX,
                                     AVSEEK_FLAG_BACKWARD);
            avcodec_flush_buffers(dec_ctx);
        }
    }

cleanup:
    if (frame != NULL) av_frame_free(&frame);
    if (packet != NULL) av_packet_free(&packet);
    if (dec_ctx != NULL) avcodec_free_context(&dec_ctx);
    if (in_fmt != NULL) avformat_close_input(&in_fmt);
    if (avio != NULL) {
        if (avio->buffer != NULL) {
            av_free(avio->buffer);
            avio->buffer = NULL;
        }
        avio_context_free(&avio);
    } else if (avio_buffer != NULL) {
        av_free(avio_buffer);
    }
    return produced;
}
