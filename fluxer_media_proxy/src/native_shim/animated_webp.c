// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

#define FLUXER_WEBP_MAX_NON_FRAME_CHUNKS ((size_t)64)

int fluxer_configure_webp_encoder(
    WebPConfig *config,
    int quality,
    int lossless,
    int effort,
    int alpha_q,
    int smart_subsample
) {
    if (config == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!WebPConfigInit(config)) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (lossless) {
        int lossless_effort = effort;
        if (lossless_effort < 0) lossless_effort = 0;
        if (lossless_effort > 9) lossless_effort = 9;
        if (!WebPConfigLosslessPreset(config, lossless_effort)) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        config->quality = (float)quality;
    } else {
        int lossy_effort = effort;
        if (lossy_effort < 0) lossy_effort = 0;
        if (lossy_effort > 6) lossy_effort = 6;
        config->lossless = 0;
        config->quality = (float)quality;
        config->method = lossy_effort;
        config->alpha_quality = alpha_q;
        config->use_sharp_yuv = smart_subsample ? 1 : 0;
    }
    return WebPValidateConfig(config)
        ? FLUXER_NATIVE_STATUS_OK
        : FLUXER_NATIVE_STATUS_CODEC_FAILURE;
}

static uint32_t webp_read_le32(const uint8_t *value) {
    return (uint32_t)value[0] |
           ((uint32_t)value[1] << 8) |
           ((uint32_t)value[2] << 16) |
           ((uint32_t)value[3] << 24);
}

static int webp_animation_structure_status(
    const void *webp_data,
    size_t webp_len,
    int max_frames,
    long long deadline_monotonic_ms,
    uint32_t *frame_count
) {
    if (webp_data == NULL || webp_len < 12 || max_frames <= 0 ||
        deadline_monotonic_ms < 0 || frame_count == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *frame_count = 0;
    const uint8_t *data = webp_data;
    if (memcmp(data, "RIFF", 4) != 0 || memcmp(data + 8, "WEBP", 4) != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    uint32_t riff_size = webp_read_le32(data + 4);
    if (riff_size < 4 || (size_t)riff_size != webp_len - 8) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    size_t chunk_limit = (size_t)max_frames +
                         FLUXER_WEBP_MAX_NON_FRAME_CHUNKS;
    size_t chunk_count = 0;
    size_t offset = 12;
    while (offset < webp_len) {
        int deadline_status = fluxer_native_deadline_status(
            deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        if (chunk_count >= chunk_limit) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        chunk_count++;
        size_t remaining = webp_len - offset;
        if (remaining < 8) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        uint32_t chunk_size_u32 = webp_read_le32(data + offset + 4);
        size_t chunk_size = chunk_size_u32;
        size_t padded_size = chunk_size + (chunk_size & 1u);
        if (padded_size < chunk_size || padded_size > remaining - 8) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (memcmp(data + offset, "ANMF", 4) == 0) {
            if (chunk_size < 16) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            if (*frame_count >= (uint32_t)max_frames) {
                return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
            }
            *frame_count += 1;
        }
        offset += 8 + padded_size;
    }
    if (*frame_count < 2) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    return FLUXER_NATIVE_STATUS_OK;
}

static int webp_animation_canvas_status(
    uint32_t canvas_width,
    uint32_t canvas_height,
    size_t max_total_pixels
) {
    if (max_total_pixels == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (canvas_width == 0 || canvas_width > INT_MAX / 4) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (canvas_height == 0 || canvas_height > INT_MAX) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t width = canvas_width;
    size_t height = canvas_height;
    if (height > SIZE_MAX / width) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t frame_pixels = width * height;
    if (frame_pixels > SIZE_MAX / 4u) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (frame_pixels > max_total_pixels) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_webp_animation_preflight(
    const void *webp_data,
    size_t webp_len,
    int max_frames,
    size_t max_total_pixels,
    long long deadline_monotonic_ms,
    struct fluxer_webp_animation_facts *facts
) {
    if (facts == NULL || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    memset(facts, 0, sizeof(*facts));
    int deadline_status = fluxer_native_deadline_status(
        deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
        return deadline_status;
    }
    uint32_t frame_count = 0;
    int structure_status = webp_animation_structure_status(
        webp_data, webp_len, max_frames, deadline_monotonic_ms,
        &frame_count);
    if (structure_status != FLUXER_NATIVE_STATUS_OK) return structure_status;
    deadline_status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
        return deadline_status;
    }
    WebPBitstreamFeatures features;
    VP8StatusCode features_status = WebPGetFeatures(
        webp_data, webp_len, &features);
    deadline_status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
        return deadline_status;
    }
    if (features_status != VP8_STATUS_OK) {
        return features_status == VP8_STATUS_OUT_OF_MEMORY
             ? FLUXER_NATIVE_STATUS_ALLOCATION_FAILED
             : FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!features.has_animation) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    int canvas_status = webp_animation_canvas_status(
        (uint32_t)features.width, (uint32_t)features.height,
        max_total_pixels);
    if (canvas_status != FLUXER_NATIVE_STATUS_OK) return canvas_status;
    size_t frame_pixels =
        (size_t)features.width * (size_t)features.height;
    if (frame_count > max_total_pixels / frame_pixels) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    facts->canvas_width = (uint32_t)features.width;
    facts->canvas_height = (uint32_t)features.height;
    facts->frame_count = frame_count;
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_vips_read_delays_ms(VipsImage *image, int n_pages, int **out_delays, int *out_len) {
    if (out_delays != NULL) *out_delays = NULL;
    if (out_len != NULL) *out_len = 0;
    if (out_delays == NULL || out_len == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    int delays_status = fluxer_vips_read_animation_delays_ms(
        image, n_pages, out_delays);
    if (delays_status != FLUXER_NATIVE_STATUS_OK) return delays_status;
    *out_len = n_pages;
    return FLUXER_NATIVE_STATUS_OK;
}

void fluxer_free_int_array(int *values) {
    free(values);
}

static int fluxer_webp_add_vips_animation_frame(
    struct fluxer_webp_animation_encoder *encoder,
    VipsImage *image,
    int frame_index,
    int width,
    int page_height,
    int delay,
    unsigned char *scratch,
    size_t scratch_cap,
    long long deadline_monotonic_ms
) {
    if (encoder == NULL || image == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (frame_index < 0 || width <= 0 || page_height <= 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (frame_index > INT_MAX / page_height) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    VipsImage *strip = NULL;
    int top = frame_index * page_height;
    if (vips_extract_area(
            image, &strip, 0, top, width, page_height, NULL) != 0 ||
        strip == NULL) {
        if (strip != NULL) g_object_unref(strip);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    size_t rgba_size = 0;
    int extract_status = fluxer_vips_extract_animation_rgba_strip(
        strip, scratch, scratch_cap, deadline_monotonic_ms, &rgba_size);
    g_object_unref(strip);
    if (extract_status != FLUXER_NATIVE_STATUS_OK) return extract_status;
    size_t width_size = (size_t)width;
    size_t page_height_size = (size_t)page_height;
    if (width_size > SIZE_MAX / page_height_size) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t expected_pixels = width_size * page_height_size;
    if (expected_pixels > SIZE_MAX / 4u) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t expected_size = expected_pixels * 4u;
    if (rgba_size != expected_size) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int add_status = fluxer_webp_animation_encoder_add(
        encoder, scratch, (size_t)width * 4u, delay);
    return add_status;
}

static int fluxer_webp_vips_animation_frame_budget(
    const int *delays,
    int n_pages,
    int max_frames,
    int max_duration_ms,
    long long deadline_monotonic_ms,
    int *out_frames
) {
    if (delays == NULL || n_pages <= 0 || max_frames <= 0 ||
        max_duration_ms <= 0 || deadline_monotonic_ms < 0 ||
        out_frames == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *out_frames = 0;
    int64_t timestamp = 0;
    int frames = 0;
    for (int i = 0; i < n_pages; i++) {
        if (fluxer_monotonic_deadline_status(deadline_monotonic_ms) !=
            FLUXER_DEADLINE_PENDING) {
            return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
        }
        if (frames >= max_frames) break;
        if (timestamp >= max_duration_ms) break;
        int delay = delays[i];
        if (delay <= 0) return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        if (delay > FLUXER_WEBP_MAX_FRAME_DURATION_MS) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        if (timestamp > INT_MAX - delay) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        timestamp += delay;
        frames++;
    }
    if (frames <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out_frames = frames;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_webp_encode_vips_animation_frames(
    struct fluxer_webp_animation_encoder *encoder,
    VipsImage *image,
    int width,
    int page_height,
    int n_pages,
    const int *delays,
    long long deadline_monotonic_ms,
    unsigned char *scratch,
    size_t scratch_cap
) {
    assert(encoder != NULL);
    assert(image != NULL);
    assert(width > 0);
    assert(page_height > 0);
    assert(n_pages > 0);
    assert(delays != NULL);
    assert(deadline_monotonic_ms >= 0);
    assert(scratch != NULL);
    assert(scratch_cap > 0);
    for (int i = 0; i < n_pages; i++) {
        if (fluxer_monotonic_deadline_status(deadline_monotonic_ms) !=
            FLUXER_DEADLINE_PENDING) {
            return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
        }
        int delay = delays[i];
        int add_status = fluxer_webp_add_vips_animation_frame(
            encoder, image, i, width, page_height, delay, scratch, scratch_cap,
            deadline_monotonic_ms);
        if (add_status != 0) return add_status;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

struct fluxer_webp_vips_encode_request {
    VipsImage *image;
    int quality;
    int lossless;
    int effort;
    int alpha_q;
    int smart_subsample;
    int thread_level;
    int loop_count;
    int full_canvas_frames;
    const struct fluxer_webp_anim_limits *limits;
    size_t max_output_size;
    void **out_buf;
    size_t *out_size;
};

struct fluxer_webp_vips_encode_context {
    WebPConfig config;
    struct fluxer_webp_animation_encoder *encoder;
    int *delays;
    unsigned char *scratch;
    size_t scratch_capacity;
    int width;
    int page_height;
    int n_pages;
    int encode_frames;
};

static int fluxer_webp_vips_encode_request_status(
    const struct fluxer_webp_vips_encode_request *request
) {
    if (request == NULL || request->image == NULL ||
        request->out_buf == NULL || request->out_size == NULL ||
        request->limits == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->max_output_size == 0) {
        return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    }
    if (request->thread_level < 0 || request->thread_level > 1 ||
        request->limits->max_frames <= 0 ||
        request->limits->max_duration_ms <= 0 ||
        request->limits->deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_webp_vips_encode_geometry(
    struct fluxer_webp_vips_encode_context *context,
    VipsImage *image
) {
    assert(context != NULL);
    assert(image != NULL);
    int total_height = vips_image_get_height(image);
    int width = vips_image_get_width(image);
    if (width <= 0 || total_height <= 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int page_height = total_height;
    if (vips_image_get_typeof(image, "page-height") != 0 &&
        vips_image_get_int(image, "page-height", &page_height) != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (page_height <= 0 || page_height > total_height ||
        total_height % page_height != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t width_size = (size_t)width;
    size_t height_size = (size_t)page_height;
    if (width_size > SIZE_MAX / height_size) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t pixels = width_size * height_size;
    if (pixels > FLUXER_MAX_VIDEO_RGBA_BYTES / 4u) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    context->width = width;
    context->page_height = page_height;
    context->n_pages = total_height / page_height;
    context->scratch_capacity = pixels * 4u;
    return FLUXER_NATIVE_STATUS_OK;
}

static void fluxer_webp_vips_encode_context_clear(
    struct fluxer_webp_vips_encode_context *context
) {
    if (context == NULL) return;
    fluxer_webp_animation_encoder_delete(context->encoder);
    free(context->scratch);
    free(context->delays);
    memset(context, 0, sizeof(*context));
}

static int fluxer_webp_vips_encode_context_open(
    struct fluxer_webp_vips_encode_context *context,
    const struct fluxer_webp_vips_encode_request *request
) {
    assert(context != NULL);
    assert(request != NULL);
    int status = fluxer_webp_vips_encode_geometry(context, request->image);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = fluxer_vips_read_animation_delays_ms(
        request->image, context->n_pages, &context->delays);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = fluxer_webp_vips_animation_frame_budget(
        context->delays, context->n_pages,
        request->limits->max_frames,
        request->limits->max_duration_ms,
        request->limits->deadline_monotonic_ms,
        &context->encode_frames);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = fluxer_configure_webp_encoder(
        &context->config, request->quality, request->lossless,
        request->effort, request->alpha_q, request->smart_subsample);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    context->config.thread_level = request->thread_level;
    if (!WebPValidateConfig(&context->config)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    status = fluxer_native_deadline_status(
        request->limits->deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    context->scratch = malloc(context->scratch_capacity);
    if (context->scratch == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    struct fluxer_webp_animation_encoder_settings settings = {
        .config = &context->config,
        .canvas_width = context->width,
        .canvas_height = context->page_height,
        .loop_count = request->loop_count,
        .full_canvas_frames = request->full_canvas_frames,
        .pixel_layout = FLUXER_WEBP_PIXEL_LAYOUT_RGBA,
        .deadline_monotonic_ms = request->limits->deadline_monotonic_ms,
        .max_output_size = request->max_output_size,
    };
    return fluxer_webp_animation_encoder_create(
        &settings, &context->encoder);
}

static int fluxer_webp_vips_encode_context_run(
    struct fluxer_webp_vips_encode_context *context,
    const struct fluxer_webp_vips_encode_request *request
) {
    assert(context != NULL);
    assert(context->encoder != NULL);
    assert(request != NULL);
    int status = fluxer_webp_encode_vips_animation_frames(
        context->encoder, request->image, context->width,
        context->page_height, context->encode_frames, context->delays,
        request->limits->deadline_monotonic_ms, context->scratch,
        context->scratch_capacity);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return fluxer_webp_animation_encoder_finish(
        context->encoder, request->out_buf, request->out_size);
}

int fluxer_webp_encode_animated(
    VipsImage *image,
    int quality,
    int lossless,
    int effort,
    int alpha_q,
    int smart_subsample,
    int thread_level,
    int loop_count,
    int full_canvas_frames,
    const struct fluxer_webp_anim_limits *limits,
    size_t max_output_size,
    void **out_buf,
    size_t *out_size
) {
    if (out_buf != NULL) *out_buf = NULL;
    if (out_size != NULL) *out_size = 0;
    struct fluxer_webp_vips_encode_request request = {
        .image = image,
        .quality = quality,
        .lossless = lossless,
        .effort = effort,
        .alpha_q = alpha_q,
        .smart_subsample = smart_subsample,
        .thread_level = thread_level,
        .loop_count = loop_count,
        .full_canvas_frames = full_canvas_frames,
        .limits = limits,
        .max_output_size = max_output_size,
        .out_buf = out_buf,
        .out_size = out_size,
    };
    int status = fluxer_webp_vips_encode_request_status(&request);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    struct fluxer_webp_vips_encode_context context = {0};
    status = fluxer_webp_vips_encode_context_open(&context, &request);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_webp_vips_encode_context_run(&context, &request);
    }
    fluxer_webp_vips_encode_context_clear(&context);
    return status;
}
