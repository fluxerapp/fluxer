// SPDX-License-Identifier: AGPL-3.0-or-later

#define _POSIX_C_SOURCE 200809L

#include "vips_shim.h"
#include "webp_animation.h"

#include <assert.h>
#include <libyuv.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <webp/demux.h>
#include <webp/mux.h>

#define FLUXER_WEBP_MAX_FRAME_DURATION_MS ((1 << 24) - 1)
#define FLUXER_WEBP_MAX_LOOP_COUNT (1 << 16)
#define FLUXER_WEBP_ANIMATION_HEADER_BOUND ((size_t)44)
#define FLUXER_WEBP_ANIMATION_FRAME_OVERHEAD_BOUND ((size_t)24)
#define FLUXER_WEBP_WRITER_INITIAL_CAPACITY ((size_t)4096)
#define FLUXER_WEBP_WRITER_MAX_GROWTH_STEPS (sizeof(size_t) * CHAR_BIT)
#define FLUXER_WEBP_KEYFRAME_INTERVAL ((size_t)4)
#define FLUXER_WEBP_FRAME_SCAN_DEADLINE_ROWS 64
#define FLUXER_WEBP_SCALE_DEADLINE_ROWS 64

enum fluxer_webp_writer_failure {
    FLUXER_WEBP_WRITER_OK = 0,
    FLUXER_WEBP_WRITER_LIMIT = 1,
    FLUXER_WEBP_WRITER_ALLOC = 2,
    FLUXER_WEBP_WRITER_INTERNAL = 3
};

enum fluxer_webp_animation_encoder_state {
    FLUXER_WEBP_ANIMATION_ENCODER_OPEN = 0,
    FLUXER_WEBP_ANIMATION_ENCODER_FINISHED = 1
};

struct fluxer_webp_bounded_writer {
    uint8_t *data;
    size_t len;
    size_t capacity;
    size_t max_size;
    size_t allocation_bound;
    enum fluxer_webp_writer_failure failure;
};

struct fluxer_webp_frame_plan {
    const uint32_t *argb;
    int argb_stride;
    int x_offset;
    int y_offset;
    int width;
    int height;
    WebPMuxAnimBlend blend_method;
};

struct fluxer_webp_animation_encoder {
    WebPMux *mux;
    WebPConfig config;
    uint32_t *current_canvas;
    uint32_t *previous_canvas;
    uint32_t transparent_pixel;
    size_t frame_count;
    size_t container_size_bound;
    size_t max_output_size;
    int canvas_width;
    int canvas_height;
    int full_canvas_frames;
    int encode_deadline_exceeded;
    enum fluxer_webp_pixel_layout pixel_layout;
    long long deadline_monotonic_ms;
    enum fluxer_webp_animation_encoder_state state;
    struct fluxer_webp_bounded_writer frame_writer;
};

static int fluxer_webp_animation_settings_status(
    const struct fluxer_webp_animation_encoder_settings *settings
) {
    if (settings == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (settings->config == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!WebPValidateConfig(settings->config)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (settings->canvas_width <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (settings->canvas_width > WEBP_MAX_DIMENSION) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (settings->canvas_height <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (settings->canvas_height > WEBP_MAX_DIMENSION) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (settings->loop_count < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (settings->loop_count >= FLUXER_WEBP_MAX_LOOP_COUNT) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (settings->full_canvas_frames < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (settings->full_canvas_frames > 1) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (settings->deadline_monotonic_ms < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (settings->pixel_layout != FLUXER_WEBP_PIXEL_LAYOUT_RGBA) {
        if (settings->pixel_layout != FLUXER_WEBP_PIXEL_LAYOUT_BGRA) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
    }
    if (settings->max_output_size < FLUXER_WEBP_ANIMATION_HEADER_BOUND) {
        return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_webp_canvas_allocation_size(
    int width, int height, size_t *canvas_bytes
) {
    if (width <= 0) return -1;
    if (height <= 0) return -1;
    if (canvas_bytes == NULL) return -1;
    size_t width_size = (size_t)width;
    size_t height_size = (size_t)height;
    if (height_size > SIZE_MAX / width_size) return -1;
    size_t pixels = width_size * height_size;
    if (pixels > SIZE_MAX / sizeof(uint32_t)) return -1;
    *canvas_bytes = pixels * sizeof(uint32_t);
    return 0;
}

static void fluxer_webp_bounded_writer_init(
    struct fluxer_webp_bounded_writer *writer, size_t allocation_bound
) {
    assert(writer != NULL);
    assert(allocation_bound > 0);
    memset(writer, 0, sizeof(*writer));
    writer->allocation_bound = allocation_bound;
}

static void fluxer_webp_bounded_writer_reset(
    struct fluxer_webp_bounded_writer *writer, size_t max_size
) {
    assert(writer != NULL);
    assert(max_size > 0);
    assert(max_size <= writer->allocation_bound);
    writer->len = 0;
    writer->max_size = max_size;
    writer->failure = FLUXER_WEBP_WRITER_OK;
}

static void fluxer_webp_bounded_writer_clear(
    struct fluxer_webp_bounded_writer *writer
) {
    if (writer == NULL) return;
    WebPFree(writer->data);
    memset(writer, 0, sizeof(*writer));
}

static int fluxer_webp_bounded_writer_next_capacity(
    const struct fluxer_webp_bounded_writer *writer,
    size_t required,
    size_t *next_capacity
) {
    if (writer == NULL) return -1;
    if (next_capacity == NULL) return -1;
    if (required == 0) return -1;
    if (required > writer->max_size) return -1;
    size_t capacity = writer->capacity;
    if (capacity == 0) {
        capacity = FLUXER_WEBP_WRITER_INITIAL_CAPACITY;
        if (capacity > writer->max_size) capacity = writer->max_size;
    }
    for (size_t step = 0; capacity < required; step++) {
        if (step >= FLUXER_WEBP_WRITER_MAX_GROWTH_STEPS) return -1;
        size_t remaining = writer->max_size - capacity;
        size_t growth = capacity;
        if (growth > remaining) growth = remaining;
        if (growth == 0) return -1;
        capacity += growth;
    }
    *next_capacity = capacity;
    return 0;
}

static int fluxer_webp_bounded_writer_reserve(
    struct fluxer_webp_bounded_writer *writer, size_t required
) {
    assert(writer != NULL);
    assert(required > writer->capacity);
    size_t next_capacity = 0;
    if (fluxer_webp_bounded_writer_next_capacity(
            writer, required, &next_capacity) != 0) {
        writer->failure = FLUXER_WEBP_WRITER_INTERNAL;
        return -1;
    }
    uint8_t *next = (uint8_t *)WebPMalloc(next_capacity);
    if (next == NULL) {
        writer->failure = FLUXER_WEBP_WRITER_ALLOC;
        return -1;
    }
    if (writer->len > 0) memcpy(next, writer->data, writer->len);
    WebPFree(writer->data);
    writer->data = next;
    writer->capacity = next_capacity;
    assert(writer->capacity <= writer->allocation_bound);
    return 0;
}

static int fluxer_webp_bounded_write(
    const uint8_t *data, size_t data_size, const WebPPicture *picture
) {
    if (picture == NULL) return 0;
    if (picture->custom_ptr == NULL) return 0;
    struct fluxer_webp_bounded_writer *writer =
        (struct fluxer_webp_bounded_writer *)picture->custom_ptr;
    if (writer->failure != FLUXER_WEBP_WRITER_OK) return 0;
    if (data_size == 0) return 1;
    if (data == NULL) {
        writer->failure = FLUXER_WEBP_WRITER_INTERNAL;
        return 0;
    }
    if (writer->len > writer->max_size) {
        writer->failure = FLUXER_WEBP_WRITER_INTERNAL;
        return 0;
    }
    if (data_size > writer->max_size - writer->len) {
        writer->failure = FLUXER_WEBP_WRITER_LIMIT;
        return 0;
    }
    size_t required = writer->len + data_size;
    if (required > writer->capacity) {
        if (fluxer_webp_bounded_writer_reserve(writer, required) != 0) return 0;
    }
    memcpy(writer->data + writer->len, data, data_size);
    writer->len = required;
    assert(writer->len <= writer->capacity);
    assert(writer->capacity <= writer->allocation_bound);
    return 1;
}

static int fluxer_webp_frame_output_budget(
    const struct fluxer_webp_animation_encoder *encoder,
    size_t *frame_budget
) {
    if (encoder == NULL) return -1;
    if (frame_budget == NULL) return -1;
    if (encoder->container_size_bound > encoder->max_output_size) {
        return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    }
    size_t remaining = encoder->max_output_size - encoder->container_size_bound;
    if (remaining <= FLUXER_WEBP_ANIMATION_FRAME_OVERHEAD_BOUND) {
        return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    }
    *frame_budget = remaining - FLUXER_WEBP_ANIMATION_FRAME_OVERHEAD_BOUND;
    return 0;
}

static int fluxer_webp_next_container_bound(
    const struct fluxer_webp_animation_encoder *encoder,
    size_t frame_size,
    size_t *next_bound
) {
    if (encoder == NULL) return -1;
    if (next_bound == NULL) return -1;
    if (encoder->container_size_bound > encoder->max_output_size) {
        return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    }
    size_t remaining = encoder->max_output_size - encoder->container_size_bound;
    if (FLUXER_WEBP_ANIMATION_FRAME_OVERHEAD_BOUND > remaining) {
        return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    }
    remaining -= FLUXER_WEBP_ANIMATION_FRAME_OVERHEAD_BOUND;
    if (frame_size > remaining) return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    *next_bound = encoder->container_size_bound;
    *next_bound += FLUXER_WEBP_ANIMATION_FRAME_OVERHEAD_BOUND;
    *next_bound += frame_size;
    return 0;
}

static int fluxer_webp_deadline_status_at(long long deadline_monotonic_ms) {
    int status = fluxer_monotonic_deadline_status(deadline_monotonic_ms);
    return status == FLUXER_DEADLINE_PENDING
        ? FLUXER_NATIVE_STATUS_OK
        : FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
}

static int fluxer_webp_deadline_status(
    const struct fluxer_webp_animation_encoder *encoder
) {
    assert(encoder != NULL);
    return fluxer_webp_deadline_status_at(encoder->deadline_monotonic_ms);
}

static int fluxer_webp_encode_progress(
    int percent,
    const WebPPicture *picture
) {
    (void)percent;
    if (picture == NULL || picture->user_data == NULL) return 0;
    struct fluxer_webp_animation_encoder *encoder =
        (struct fluxer_webp_animation_encoder *)picture->user_data;
    if (fluxer_webp_deadline_status(encoder) == FLUXER_NATIVE_STATUS_OK) {
        return 1;
    }
    encoder->encode_deadline_exceeded = 1;
    return 0;
}

static uint32_t fluxer_webp_argb_pixel(
    const uint8_t *pixel, enum fluxer_webp_pixel_layout layout
) {
    assert(pixel != NULL);
    uint32_t red = pixel[0];
    uint32_t green = pixel[1];
    uint32_t blue = pixel[2];
    uint32_t alpha = pixel[3];
    if (layout == FLUXER_WEBP_PIXEL_LAYOUT_BGRA) {
        red = pixel[2];
        blue = pixel[0];
    }
    if (alpha == 0) return 0;
    return (alpha << 24) | (red << 16) | (green << 8) | blue;
}

static int fluxer_webp_frame_is_keyframe(
    const struct fluxer_webp_animation_encoder *encoder
) {
    assert(encoder != NULL);
    if (encoder->frame_count == 0) return 1;
    if (encoder->full_canvas_frames != 0) return 1;
    return encoder->frame_count % FLUXER_WEBP_KEYFRAME_INTERVAL == 0;
}

static int fluxer_webp_prepare_frame_plan(
    struct fluxer_webp_animation_encoder *encoder,
    const uint8_t *pixels,
    size_t stride,
    struct fluxer_webp_frame_plan *plan
) {
    assert(encoder != NULL);
    assert(pixels != NULL);
    assert(plan != NULL);
    memset(plan, 0, sizeof(*plan));
    int keyframe = fluxer_webp_frame_is_keyframe(encoder);
    int left = encoder->canvas_width;
    int top = encoder->canvas_height;
    int right = -1;
    int bottom = -1;
    for (int y = 0; y < encoder->canvas_height; y++) {
        if (y % FLUXER_WEBP_FRAME_SCAN_DEADLINE_ROWS == 0 &&
            fluxer_webp_deadline_status(encoder) !=
                FLUXER_NATIVE_STATUS_OK) {
            return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
        }
        const uint8_t *source_row = pixels + (size_t)y * stride;
        uint32_t *target_row = encoder->current_canvas +
                               (size_t)y * (size_t)encoder->canvas_width;
        const uint32_t *previous_row = NULL;
        if (!keyframe) {
            assert(encoder->previous_canvas != NULL);
            previous_row = encoder->previous_canvas +
                           (size_t)y * (size_t)encoder->canvas_width;
        }
        for (int x = 0; x < encoder->canvas_width; x++) {
            uint32_t pixel = fluxer_webp_argb_pixel(
                source_row + (size_t)x * 4u, encoder->pixel_layout);
            target_row[x] = pixel;
            if (keyframe) continue;
            if (pixel == previous_row[x]) continue;
            if (x < left) left = x;
            if (x > right) right = x;
            if (y < top) top = y;
            if (y > bottom) bottom = y;
        }
    }
    plan->blend_method = WEBP_MUX_NO_BLEND;
    plan->argb_stride = encoder->canvas_width;
    if (keyframe) {
        plan->width = encoder->canvas_width;
        plan->height = encoder->canvas_height;
        plan->argb = encoder->current_canvas;
        return fluxer_webp_deadline_status(encoder);
    }
    if (right >= 0) {
        left &= ~1;
        top &= ~1;
        plan->x_offset = left;
        plan->y_offset = top;
        plan->width = right - left + 1;
        plan->height = bottom - top + 1;
        size_t offset = (size_t)plan->y_offset * (size_t)encoder->canvas_width;
        offset += (size_t)plan->x_offset;
        plan->argb = encoder->current_canvas + offset;
        return fluxer_webp_deadline_status(encoder);
    }
    plan->argb = &encoder->transparent_pixel;
    plan->argb_stride = 1;
    plan->width = 1;
    plan->height = 1;
    plan->blend_method = WEBP_MUX_BLEND;
    return fluxer_webp_deadline_status(encoder);
}

static int fluxer_webp_encode_frame_candidate(
    struct fluxer_webp_animation_encoder *encoder,
    const struct fluxer_webp_frame_plan *plan,
    size_t frame_budget
) {
    assert(encoder != NULL);
    assert(plan != NULL);
    struct fluxer_webp_bounded_writer *writer = &encoder->frame_writer;
    WebPPicture picture;
    if (!WebPPictureInit(&picture)) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    fluxer_webp_bounded_writer_reset(writer, frame_budget);
    picture.width = plan->width;
    picture.height = plan->height;
    picture.use_argb = 1;
    picture.argb = (uint32_t *)(uintptr_t)plan->argb;
    picture.argb_stride = plan->argb_stride;
    picture.writer = fluxer_webp_bounded_write;
    picture.custom_ptr = writer;
    picture.progress_hook = fluxer_webp_encode_progress;
    picture.user_data = encoder;
    encoder->encode_deadline_exceeded = 0;
    int encoded = WebPEncode(&encoder->config, &picture);
    WebPEncodingError encode_error = picture.error_code;
    WebPPictureFree(&picture);
    if (!encoded) {
        int status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        if (writer->failure == FLUXER_WEBP_WRITER_LIMIT) {
            status = FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
        } else if (writer->failure == FLUXER_WEBP_WRITER_ALLOC) {
            status = FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
        } else if (encode_error == VP8_ENC_ERROR_OUT_OF_MEMORY) {
            status = FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
        } else if (encode_error == VP8_ENC_ERROR_USER_ABORT &&
                   encoder->encode_deadline_exceeded != 0) {
            status = FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
        }
        return status;
    }
    if (writer->failure != FLUXER_WEBP_WRITER_OK) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (writer->data == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (writer->len == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_webp_push_frame_candidate(
    struct fluxer_webp_animation_encoder *encoder,
    const struct fluxer_webp_frame_plan *plan,
    int duration_ms,
    const struct fluxer_webp_bounded_writer *writer
) {
    assert(encoder != NULL);
    assert(plan != NULL);
    assert(writer != NULL);
    size_t next_bound = 0;
    int bound_status = fluxer_webp_next_container_bound(
        encoder, writer->len, &next_bound);
    if (bound_status != 0) return bound_status;
    WebPMuxFrameInfo frame;
    memset(&frame, 0, sizeof(frame));
    frame.bitstream.bytes = writer->data;
    frame.bitstream.size = writer->len;
    frame.x_offset = plan->x_offset;
    frame.y_offset = plan->y_offset;
    frame.duration = duration_ms;
    frame.id = WEBP_CHUNK_ANMF;
    frame.dispose_method = WEBP_MUX_DISPOSE_NONE;
    frame.blend_method = plan->blend_method;
    WebPMuxError push_status = WebPMuxPushFrame(encoder->mux, &frame, 1);
    if (push_status == WEBP_MUX_MEMORY_ERROR) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (push_status != WEBP_MUX_OK) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    encoder->container_size_bound = next_bound;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_webp_animation_mux_create(
    struct fluxer_webp_animation_encoder *encoder, int loop_count
) {
    assert(encoder != NULL);
    encoder->mux = WebPMuxNew();
    if (encoder->mux == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    WebPMuxError canvas_status = WebPMuxSetCanvasSize(
        encoder->mux, encoder->canvas_width, encoder->canvas_height);
    if (canvas_status == WEBP_MUX_MEMORY_ERROR) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (canvas_status != WEBP_MUX_OK) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    WebPMuxAnimParams animation = {
        .bgcolor = 0x00000000,
        .loop_count = loop_count,
    };
    WebPMuxError animation_status = WebPMuxSetAnimationParams(
        encoder->mux, &animation);
    if (animation_status == WEBP_MUX_MEMORY_ERROR) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (animation_status != WEBP_MUX_OK) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_webp_animation_encoder_create(
    const struct fluxer_webp_animation_encoder_settings *settings,
    struct fluxer_webp_animation_encoder **out_encoder
) {
    if (out_encoder == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out_encoder = NULL;
    int settings_status = fluxer_webp_animation_settings_status(settings);
    if (settings_status != FLUXER_NATIVE_STATUS_OK) return settings_status;
    if (fluxer_monotonic_deadline_status(settings->deadline_monotonic_ms) !=
        FLUXER_DEADLINE_PENDING) {
        return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
    }
    size_t canvas_bytes = 0;
    if (fluxer_webp_canvas_allocation_size(
            settings->canvas_width, settings->canvas_height,
            &canvas_bytes) != 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    struct fluxer_webp_animation_encoder *encoder =
        (struct fluxer_webp_animation_encoder *)calloc(1, sizeof(*encoder));
    if (encoder == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    encoder->current_canvas = (uint32_t *)malloc(canvas_bytes);
    if (settings->full_canvas_frames == 0) {
        encoder->previous_canvas = (uint32_t *)malloc(canvas_bytes);
    }
    encoder->config = *settings->config;
    encoder->canvas_width = settings->canvas_width;
    encoder->canvas_height = settings->canvas_height;
    encoder->full_canvas_frames = settings->full_canvas_frames;
    encoder->pixel_layout = settings->pixel_layout;
    encoder->deadline_monotonic_ms = settings->deadline_monotonic_ms;
    encoder->max_output_size = settings->max_output_size;
    encoder->container_size_bound = FLUXER_WEBP_ANIMATION_HEADER_BOUND;
    fluxer_webp_bounded_writer_init(
        &encoder->frame_writer, settings->max_output_size);
    if (encoder->current_canvas == NULL ||
        (settings->full_canvas_frames == 0 &&
         encoder->previous_canvas == NULL)) {
        fluxer_webp_animation_encoder_delete(encoder);
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    int mux_status = fluxer_webp_animation_mux_create(
        encoder, settings->loop_count);
    if (mux_status != FLUXER_NATIVE_STATUS_OK) {
        fluxer_webp_animation_encoder_delete(encoder);
        return mux_status;
    }
    if (fluxer_webp_deadline_status(encoder) != FLUXER_NATIVE_STATUS_OK) {
        fluxer_webp_animation_encoder_delete(encoder);
        return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
    }
    *out_encoder = encoder;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_webp_input_stride_valid(
    const struct fluxer_webp_animation_encoder *encoder, size_t stride
) {
    assert(encoder != NULL);
    size_t row_bytes = (size_t)encoder->canvas_width * 4u;
    if (stride < row_bytes) return 0;
    if (encoder->canvas_height == 1) return 1;
    size_t row_count = (size_t)encoder->canvas_height - 1u;
    if (stride > (SIZE_MAX - row_bytes) / row_count) return 0;
    return 1;
}

int fluxer_webp_animation_encoder_add(
    struct fluxer_webp_animation_encoder *encoder,
    const uint8_t *pixels,
    size_t stride,
    int duration_ms
) {
    if (encoder == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (pixels == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (encoder->state != FLUXER_WEBP_ANIMATION_ENCODER_OPEN) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!fluxer_webp_input_stride_valid(encoder, stride)) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (duration_ms <= 0) return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    if (duration_ms > FLUXER_WEBP_MAX_FRAME_DURATION_MS) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (fluxer_webp_deadline_status(encoder) != FLUXER_NATIVE_STATUS_OK) {
        return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
    }
    struct fluxer_webp_frame_plan plan;
    int plan_status = fluxer_webp_prepare_frame_plan(
        encoder, pixels, stride, &plan);
    if (plan_status != FLUXER_NATIVE_STATUS_OK) return plan_status;
    size_t frame_budget = 0;
    int budget_status = fluxer_webp_frame_output_budget(encoder, &frame_budget);
    if (budget_status != 0) return budget_status;
    int encode_status = fluxer_webp_encode_frame_candidate(
        encoder, &plan, frame_budget);
    if (encode_status != 0) return encode_status;
    if (fluxer_webp_deadline_status(encoder) != FLUXER_NATIVE_STATUS_OK) {
        return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
    }
    int push_status = fluxer_webp_push_frame_candidate(
        encoder, &plan, duration_ms, &encoder->frame_writer);
    if (push_status != 0) return push_status;
    if (encoder->full_canvas_frames == 0) {
        assert(encoder->previous_canvas != NULL);
        uint32_t *previous = encoder->previous_canvas;
        encoder->previous_canvas = encoder->current_canvas;
        encoder->current_canvas = previous;
    }
    encoder->frame_count++;
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_webp_animation_encoder_finish(
    struct fluxer_webp_animation_encoder *encoder,
    void **out_buf,
    size_t *out_size
) {
    if (out_buf != NULL) *out_buf = NULL;
    if (out_size != NULL) *out_size = 0;
    if (encoder == NULL || out_buf == NULL || out_size == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (encoder->state != FLUXER_WEBP_ANIMATION_ENCODER_OPEN) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (encoder->frame_count == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (encoder->container_size_bound > encoder->max_output_size) {
        return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    }
    if (fluxer_webp_deadline_status(encoder) != FLUXER_NATIVE_STATUS_OK) {
        return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
    }
    WebPData output;
    WebPDataInit(&output);
    WebPMuxError assemble_status = WebPMuxAssemble(encoder->mux, &output);
    if (assemble_status == WEBP_MUX_MEMORY_ERROR) {
        WebPDataClear(&output);
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (assemble_status != WEBP_MUX_OK) {
        WebPDataClear(&output);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (output.bytes == NULL) {
        WebPDataClear(&output);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (output.size == 0 || output.size > encoder->container_size_bound) {
        WebPDataClear(&output);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (fluxer_webp_deadline_status(encoder) != FLUXER_NATIVE_STATUS_OK) {
        WebPDataClear(&output);
        return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
    }
    *out_buf = (void *)output.bytes;
    *out_size = output.size;
    encoder->state = FLUXER_WEBP_ANIMATION_ENCODER_FINISHED;
    return FLUXER_NATIVE_STATUS_OK;
}

void fluxer_webp_animation_encoder_delete(
    struct fluxer_webp_animation_encoder *encoder
) {
    if (encoder == NULL) return;
    WebPMuxDelete(encoder->mux);
    fluxer_webp_bounded_writer_clear(&encoder->frame_writer);
    free(encoder->current_canvas);
    free(encoder->previous_canvas);
    memset(encoder, 0, sizeof(*encoder));
    free(encoder);
}

struct fluxer_webp_direct_request {
    const void *data;
    size_t len;
    int max_width;
    int max_height;
    int quality;
    int lossless;
    int effort;
    int alpha_q;
    int smart_subsample;
    int thread_level;
    int max_source_frames;
    size_t max_total_pixels;
    const struct fluxer_webp_anim_limits *limits;
    size_t max_output_size;
    void **out_buf;
    size_t *out_size;
};

struct fluxer_webp_direct_state {
    WebPAnimDecoder *decoder;
    struct fluxer_webp_animation_encoder *encoder;
    uint8_t *scaled_bgra;
    int source_width;
    int source_height;
    int target_width;
    int target_height;
    int source_frame_count;
    int max_frames;
    int max_duration_ms;
    long long deadline_monotonic_ms;
};

static int webp_direct_request_valid(const struct fluxer_webp_direct_request *request) {
    if (request == NULL) return 0;
    if (request->data == NULL) return 0;
    if (request->len == 0) return 0;
    if (request->max_width < 0) return 0;
    if (request->max_height < 0) return 0;
    if (request->max_source_frames <= 0) return 0;
    if (request->max_total_pixels == 0) return 0;
    if (request->limits == NULL) return 0;
    if (request->limits->max_frames <= 0) return 0;
    if (request->limits->max_duration_ms <= 0) return 0;
    if (request->limits->deadline_monotonic_ms < 0) return 0;
    if (request->max_output_size == 0) return 0;
    if (request->out_buf == NULL) return 0;
    if (request->out_size == NULL) return 0;
    if (request->thread_level < 0) return 0;
    if (request->thread_level > 1) return 0;
    return 1;
}

static int fit_webp_direct_dimensions(
    const WebPAnimInfo *info,
    const struct fluxer_webp_direct_request *request,
    int *target_width,
    int *target_height
) {
    uint64_t numerator = 1;
    uint64_t denominator = 1;
    uint64_t source_width = info->canvas_width;
    uint64_t source_height = info->canvas_height;
    if (request->max_width > 0 && (uint64_t)request->max_width < source_width) {
        numerator = (uint64_t)request->max_width;
        denominator = source_width;
    }
    if (request->max_height > 0 && (uint64_t)request->max_height < source_height) {
        uint64_t height_limit = (uint64_t)request->max_height;
        if (height_limit * denominator < numerator * source_height) {
            numerator = height_limit;
            denominator = source_height;
        }
    }
    uint64_t width = (source_width * numerator + denominator / 2u) / denominator;
    uint64_t height = (source_height * numerator + denominator / 2u) / denominator;
    if (width == 0) width = 1;
    if (height == 0) height = 1;
    if (width > INT_MAX) return 0;
    if (height > INT_MAX) return 0;
    *target_width = (int)width;
    *target_height = (int)height;
    return 1;
}

static int webp_direct_metadata_supported(const WebPAnimDecoder *decoder) {
    const WebPDemuxer *demuxer = WebPAnimDecoderGetDemuxer(decoder);
    if (demuxer == NULL) return 0;
    uint32_t flags = WebPDemuxGetI(demuxer, WEBP_FF_FORMAT_FLAGS);
    if ((flags & ICCP_FLAG) != 0) return 0;
    if ((flags & EXIF_FLAG) != 0) return 0;
    if ((flags & XMP_FLAG) != 0) return 0;
    return 1;
}

static int scale_webp_bgra_frame(
    const struct fluxer_webp_direct_state *state,
    uint8_t *source_bgra,
    uint8_t **out_bgra
) {
    assert(state != NULL);
    assert(source_bgra != NULL);
    assert(out_bgra != NULL);
    *out_bgra = NULL;
    if (state->source_width == state->target_width) {
        if (state->source_height == state->target_height) {
            *out_bgra = source_bgra;
            return fluxer_webp_deadline_status_at(state->deadline_monotonic_ms);
        }
    }
    if (state->scaled_bgra == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    for (int clip_y = 0; clip_y < state->target_height;
         clip_y += FLUXER_WEBP_SCALE_DEADLINE_ROWS) {
        int deadline_status = fluxer_webp_deadline_status_at(
            state->deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
        int clip_height = state->target_height - clip_y;
        if (clip_height > FLUXER_WEBP_SCALE_DEADLINE_ROWS) {
            clip_height = FLUXER_WEBP_SCALE_DEADLINE_ROWS;
        }
        int rc = ARGBScaleClip(
            source_bgra,
            state->source_width * 4,
            state->source_width,
            state->source_height,
            state->scaled_bgra,
            state->target_width * 4,
            state->target_width,
            state->target_height,
            0,
            clip_y,
            state->target_width,
            clip_height,
            kFilterBox
        );
        if (rc > 0) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
        if (rc < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int deadline_status = fluxer_webp_deadline_status_at(
        state->deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    *out_bgra = state->scaled_bgra;
    return FLUXER_NATIVE_STATUS_OK;
}

static int transform_webp_animation_frame(
    struct fluxer_webp_direct_state *state,
    int *source_timestamp,
    int output_timestamp
) {
    uint8_t *source_bgra = NULL;
    int next_source_timestamp = 0;
    if (!WebPAnimDecoderGetNext(state->decoder, &source_bgra, &next_source_timestamp)) {
        int deadline_status = fluxer_webp_deadline_status_at(
            state->deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (source_bgra == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (next_source_timestamp <= *source_timestamp) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    int delay = next_source_timestamp - *source_timestamp;
    if (delay > FLUXER_WEBP_MAX_FRAME_DURATION_MS) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (output_timestamp > INT_MAX - delay) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    uint8_t *output_bgra = NULL;
    int scale_status = scale_webp_bgra_frame(
        state, source_bgra, &output_bgra);
    if (scale_status != FLUXER_NATIVE_STATUS_OK) return scale_status;
    assert(output_bgra != NULL);
    int add_status = fluxer_webp_animation_encoder_add(
        state->encoder,
        output_bgra,
        (size_t)state->target_width * 4u,
        delay
    );
    if (add_status != 0) return add_status;
    *source_timestamp = next_source_timestamp;
    return delay;
}

static int transform_webp_animation_frames(struct fluxer_webp_direct_state *state) {
    if (state->source_frame_count <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (state->max_frames <= 0 || state->max_duration_ms <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int source_timestamp = 0;
    int output_timestamp = 0;
    int frames_added = 0;
    int truncated = 0;
    while (WebPAnimDecoderHasMoreFrames(state->decoder)) {
        if (frames_added >= state->source_frame_count) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (fluxer_monotonic_deadline_status(state->deadline_monotonic_ms) != FLUXER_DEADLINE_PENDING) {
            return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
        }
        if (frames_added >= state->max_frames) {
            truncated = 1;
            break;
        }
        if (output_timestamp >= state->max_duration_ms) {
            truncated = 1;
            break;
        }
        int delay = transform_webp_animation_frame(state, &source_timestamp, output_timestamp);
        if (delay < 0) return delay;
        output_timestamp += delay;
        frames_added++;
    }
    if (frames_added == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (frames_added > state->source_frame_count) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!truncated && frames_added != state->source_frame_count) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static WebPAnimDecoder *new_webp_direct_decoder(
    const struct fluxer_webp_direct_request *request,
    WebPAnimInfo *info,
    int *status
) {
    WebPData input = { .bytes = request->data, .size = request->len };
    struct fluxer_webp_animation_facts facts;
    int preflight_status = fluxer_webp_animation_preflight(
        request->data, request->len, request->max_source_frames,
        request->max_total_pixels,
        request->limits->deadline_monotonic_ms, &facts);
    if (preflight_status != FLUXER_NATIVE_STATUS_OK) {
        *status = preflight_status;
        return NULL;
    }
    WebPAnimDecoderOptions options;
    if (!WebPAnimDecoderOptionsInit(&options)) return NULL;
    options.color_mode = MODE_BGRA;
    options.use_threads = request->thread_level;
    if (fluxer_monotonic_deadline_status(
            request->limits->deadline_monotonic_ms) !=
        FLUXER_DEADLINE_PENDING) {
        *status = FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
        return NULL;
    }
    WebPAnimDecoder *decoder = WebPAnimDecoderNew(&input, &options);
    if (fluxer_monotonic_deadline_status(
            request->limits->deadline_monotonic_ms) !=
        FLUXER_DEADLINE_PENDING) {
        if (decoder != NULL) WebPAnimDecoderDelete(decoder);
        *status = FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
        return NULL;
    }
    if (decoder == NULL) return NULL;
    if (!WebPAnimDecoderGetInfo(decoder, info)) {
        WebPAnimDecoderDelete(decoder);
        return NULL;
    }
    if (info->canvas_width != facts.canvas_width ||
        info->canvas_height != facts.canvas_height ||
        info->frame_count != facts.frame_count) {
        WebPAnimDecoderDelete(decoder);
        return NULL;
    }
    if (info->loop_count >= (uint32_t)FLUXER_WEBP_MAX_LOOP_COUNT) {
        WebPAnimDecoderDelete(decoder);
        return NULL;
    }
    if (!webp_direct_metadata_supported(decoder)) {
        WebPAnimDecoderDelete(decoder);
        *status = FLUXER_NATIVE_STATUS_UNSUPPORTED;
        return NULL;
    }
    return decoder;
}

static int new_webp_direct_encoder(
    const struct fluxer_webp_direct_request *request,
    int target_width,
    int target_height,
    int loop_count,
    struct fluxer_webp_animation_encoder **out_encoder
) {
    WebPConfig config;
    int config_rc = fluxer_configure_webp_encoder(
        &config,
        request->quality,
        request->lossless,
        request->effort,
        request->alpha_q,
        request->smart_subsample
    );
    if (config_rc != FLUXER_NATIVE_STATUS_OK) return config_rc;
    config.thread_level = request->thread_level;
    if (!WebPValidateConfig(&config)) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    struct fluxer_webp_animation_encoder_settings settings = {
        .config = &config,
        .canvas_width = target_width,
        .canvas_height = target_height,
        .loop_count = loop_count,
        .full_canvas_frames = 0,
        .pixel_layout = FLUXER_WEBP_PIXEL_LAYOUT_BGRA,
        .deadline_monotonic_ms = request->limits->deadline_monotonic_ms,
        .max_output_size = request->max_output_size,
    };
    return fluxer_webp_animation_encoder_create(&settings, out_encoder);
}

static int run_webp_direct_transform(struct fluxer_webp_direct_request *request) {
    if (request == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->out_buf != NULL) *request->out_buf = NULL;
    if (request->out_size != NULL) *request->out_size = 0;
    if (request->out_buf == NULL || request->out_size == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!webp_direct_request_valid(request)) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    WebPAnimInfo info;
    int status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    WebPAnimDecoder *decoder = new_webp_direct_decoder(request, &info, &status);
    if (decoder == NULL) return status;
    int target_width = 0;
    int target_height = 0;
    if (!fit_webp_direct_dimensions(&info, request, &target_width, &target_height)) {
        WebPAnimDecoderDelete(decoder);
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    struct fluxer_webp_animation_encoder *encoder = NULL;
    int encoder_status = new_webp_direct_encoder(
        request, target_width, target_height, (int)info.loop_count, &encoder);
    if (encoder_status != 0) {
        WebPAnimDecoderDelete(decoder);
        return encoder_status;
    }
    uint8_t *scaled_bgra = NULL;
    if ((int)info.canvas_width != target_width || (int)info.canvas_height != target_height) {
        size_t target_bytes = 0;
        if (fluxer_webp_canvas_allocation_size(
                target_width, target_height, &target_bytes) != 0) {
            fluxer_webp_animation_encoder_delete(encoder);
            WebPAnimDecoderDelete(decoder);
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        scaled_bgra = (uint8_t *)malloc(target_bytes);
        if (scaled_bgra == NULL) {
            fluxer_webp_animation_encoder_delete(encoder);
            WebPAnimDecoderDelete(decoder);
            return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
        }
    }
    struct fluxer_webp_direct_state state = {
        .decoder = decoder,
        .encoder = encoder,
        .scaled_bgra = scaled_bgra,
        .source_width = (int)info.canvas_width,
        .source_height = (int)info.canvas_height,
        .target_width = target_width,
        .target_height = target_height,
        .source_frame_count = (int)info.frame_count,
        .max_frames = request->limits->max_frames,
        .max_duration_ms = request->limits->max_duration_ms,
        .deadline_monotonic_ms = request->limits->deadline_monotonic_ms,
    };
    int rc = transform_webp_animation_frames(&state);
    free(scaled_bgra);
    WebPAnimDecoderDelete(decoder);
    if (rc == 0) {
        rc = fluxer_webp_animation_encoder_finish(
            encoder, request->out_buf, request->out_size);
    }
    fluxer_webp_animation_encoder_delete(encoder);
    return rc;
}

int fluxer_webp_transform_animated(
    const void *webp_data,
    size_t webp_len,
    int max_width,
    int max_height,
    int quality,
    int lossless,
    int effort,
    int alpha_q,
    int smart_subsample,
    int thread_level,
    int max_source_frames,
    size_t max_total_pixels,
    const struct fluxer_webp_anim_limits *limits,
    size_t max_output_size,
    void **out_buf,
    size_t *out_size
) {
    struct fluxer_webp_direct_request request = {
        .data = webp_data,
        .len = webp_len,
        .max_width = max_width,
        .max_height = max_height,
        .quality = quality,
        .lossless = lossless,
        .effort = effort,
        .alpha_q = alpha_q,
        .smart_subsample = smart_subsample,
        .thread_level = thread_level,
        .max_source_frames = max_source_frames,
        .max_total_pixels = max_total_pixels,
        .limits = limits,
        .max_output_size = max_output_size,
        .out_buf = out_buf,
        .out_size = out_size,
    };
    return run_webp_direct_transform(&request);
}
