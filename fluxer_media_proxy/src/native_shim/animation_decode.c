// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

#define FLUXER_ANIMATION_AVIO_BUFFER_SIZE (64u * 1024u)
#define FLUXER_ANIMATION_PROBE_SIZE (5u * 1024u * 1024u)
#define FLUXER_ANIMATION_ANALYZE_DURATION (5 * AV_TIME_BASE)
#define FLUXER_VIPS_RGBA_REGION_ROWS 64

enum ff_append_frame_result {
    FF_APPEND_FRAME_LIMIT = INT_MIN
};

static int ff_animation_frame_delay_ms(AVFrame *frame, AVStream *stream,
                                       int64_t packet_duration, int *out_delay) {
    if (frame == NULL || stream == NULL || out_delay == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *out_delay = 0;
    int64_t duration = frame->duration > 0 ? frame->duration : packet_duration;
    if (duration <= 0) {
        *out_delay = FLUXER_DEFAULT_ANIMATION_FRAME_DELAY_MS;
        return FLUXER_NATIVE_STATUS_OK;
    }
    if (stream->time_base.num <= 0 || stream->time_base.den <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    long double milliseconds = (long double)duration *
                               (long double)stream->time_base.num * 1000.0L /
                               (long double)stream->time_base.den;
    if (!isfinite(milliseconds) || milliseconds > (long double)INT_MAX) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    int delay = (int)floorl(milliseconds + 0.5L);
    if (delay < FLUXER_MIN_ANIMATION_FRAME_DELAY_MS) {
        delay = FLUXER_MIN_ANIMATION_FRAME_DELAY_MS;
    }
    *out_delay = delay;
    return FLUXER_NATIVE_STATUS_OK;
}

struct ff_animation_decode_request {
    const uint8_t *data;
    size_t len;
    int decoder_threads;
    long long deadline_monotonic_ms;
    int max_frames;
    size_t max_total_pixels;
    int require_complete;
    const char *allowed_formats;
    const AVInputFormat *forced_format;
    int expected_frames;
    int minimum_frames;
    int decode_pixels;
};

struct ff_animation_decode_context {
    struct ff_animation_decode_request request;
    struct ff_mem_reader reader;
    unsigned char *input_avio_buffer;
    AVIOContext *input_avio;
    AVFormatContext *input_format;
    AVCodecContext *decoder;
    AVStream *input_stream;
    int stream_index;
    struct SwsContext *sws;
    AVPacket *packet;
    AVFrame *frame;
    uint8_t *pixels;
    int *delays;
    VipsImage *image;
    int expected_frames;
    int capacity_hint;
    int capacity;
    int frames;
    int canvas_width;
    int canvas_height;
    int packet_limit;
    int packets_read;
    int stopped;
    int64_t last_packet_duration;
};

static int ff_animation_frame_geometry(
    struct ff_animation_decode_context *context,
    AVFrame *frame,
    int *out_width,
    int *out_height,
    size_t *out_frame_bytes
) {
    assert(context != NULL);
    assert(frame != NULL);
    assert(out_width != NULL);
    assert(out_height != NULL);
    assert(out_frame_bytes != NULL);
    int width = frame->width;
    int height = frame->height;
    if (width <= 0) width = context->canvas_width;
    if (height <= 0) height = context->canvas_height;
    size_t frame_bytes = 0;
    if (ff_validate_rgba_geometry(width, height, &frame_bytes) != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (context->frames == 0) {
        context->canvas_width = width;
        context->canvas_height = height;
    } else {
        if (width != context->canvas_width) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        if (height != context->canvas_height) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
    }
    size_t frame_pixels = frame_bytes / 4u;
    if (frame_pixels > context->request.max_total_pixels) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    *out_width = width;
    *out_height = height;
    *out_frame_bytes = frame_bytes;
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_animation_reserve_frames(
    struct ff_animation_decode_context *context,
    size_t frame_bytes
) {
    assert(context != NULL);
    assert(frame_bytes > 0);
    assert(context->capacity_hint > 0);
    assert(context->frames >= 0);
    if (context->capacity > context->frames) return FLUXER_NATIVE_STATUS_OK;
    size_t frame_pixels = frame_bytes / 4u;
    int capacity_limit = context->request.max_frames;
    size_t pixel_limit = context->request.max_total_pixels / frame_pixels;
    if (pixel_limit < (size_t)capacity_limit) {
        capacity_limit = (int)pixel_limit;
    }

    int new_capacity = context->capacity_hint;
    if (context->capacity > 0) {
        if (context->capacity > INT_MAX / 2) {
            new_capacity = INT_MAX;
        } else {
            new_capacity = context->capacity * 2;
        }
    }
    if (new_capacity > capacity_limit) new_capacity = capacity_limit;
    if (new_capacity <= context->frames) new_capacity = context->frames + 1;
    if ((size_t)new_capacity > SIZE_MAX / frame_bytes) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }

    size_t pixel_bytes = (size_t)new_capacity * frame_bytes;
    uint8_t *new_pixels = realloc(context->pixels, pixel_bytes);
    if (new_pixels == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    context->pixels = new_pixels;
    int *new_delays = realloc(
        context->delays, (size_t)new_capacity * sizeof(int));
    if (new_delays == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    context->delays = new_delays;
    context->capacity = new_capacity;
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_animation_accept_frame(
    struct ff_animation_decode_context *context,
    int64_t packet_duration
) {
    assert(context != NULL);
    assert(context->frame != NULL);
    assert(context->input_stream != NULL);
    int deadline_status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    if (context->frames >= context->request.max_frames) {
        return FF_APPEND_FRAME_LIMIT;
    }
    int width = 0;
    int height = 0;
    size_t frame_bytes = 0;
    int status = ff_animation_frame_geometry(
        context, context->frame, &width, &height, &frame_bytes);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    size_t frame_pixels = frame_bytes / 4u;
    if ((size_t)(context->frames + 1) >
        context->request.max_total_pixels / frame_pixels) {
        return FF_APPEND_FRAME_LIMIT;
    }
    if (context->request.decode_pixels == 0) {
        context->frames++;
        return FLUXER_NATIVE_STATUS_OK;
    }
    status = ff_animation_reserve_frames(context, frame_bytes);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;

    uint8_t *destination =
        context->pixels + (size_t)context->frames * frame_bytes;
    int convert_status = fluxer_av_frame_convert_to_rgba(
        context->frame,
        width,
        height,
        width,
        height,
        &context->sws,
        context->request.deadline_monotonic_ms,
        destination);
    if (convert_status != FLUXER_NATIVE_STATUS_OK) return convert_status;
    int delay = 0;
    status = ff_animation_frame_delay_ms(
        context->frame, context->input_stream, packet_duration, &delay);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    context->delays[context->frames] = delay;
    context->frames++;
    return FLUXER_NATIVE_STATUS_OK;
}
static int ff_animation_decode_request_valid(
    const struct ff_animation_decode_request *request
) {
    assert(request != NULL);
    if (request->data == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->len == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->len > (size_t)INT64_MAX) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (!fluxer_ffmpeg_decoder_threads_valid(request->decoder_threads)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->max_frames <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->max_total_pixels == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->require_complete < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->require_complete > 1) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->allowed_formats == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->allowed_formats[0] == '\0') {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->expected_frames < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->minimum_frames <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->minimum_frames > request->max_frames) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (request->decode_pixels < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->decode_pixels > 1) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static void ff_animation_decode_context_init(
    struct ff_animation_decode_context *context,
    const struct ff_animation_decode_request *request
) {
    assert(context != NULL);
    assert(request != NULL);
    memset(context, 0, sizeof(*context));
    context->request = *request;
    context->reader.data = request->data;
    context->reader.len = request->len;
    context->stream_index = -1;
    context->expected_frames = request->expected_frames;
}

static int ff_animation_open_input(
    struct ff_animation_decode_context *context
) {
    assert(context != NULL);
    assert(context->input_avio == NULL);
    assert(context->input_format == NULL);
    context->input_avio_buffer = av_malloc(
        FLUXER_ANIMATION_AVIO_BUFFER_SIZE);
    if (context->input_avio_buffer == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    context->input_avio = avio_alloc_context(
        context->input_avio_buffer,
        (int)FLUXER_ANIMATION_AVIO_BUFFER_SIZE,
        0,
        &context->reader,
        ff_mem_read_packet,
        NULL,
        ff_mem_seek);
    if (context->input_avio == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    context->input_avio_buffer = NULL;
    context->input_format = avformat_alloc_context();
    if (context->input_format == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    int status = fluxer_restrict_untrusted_av_context(
        context->input_format, context->request.allowed_formats);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    context->input_format->pb = context->input_avio;
    context->input_format->interrupt_callback.callback =
        fluxer_ffmpeg_interrupt_deadline;
    context->input_format->interrupt_callback.opaque =
        &context->request.deadline_monotonic_ms;
    context->input_format->flags |= AVFMT_FLAG_CUSTOM_IO;
    context->input_format->probesize = FLUXER_ANIMATION_PROBE_SIZE;
    context->input_format->max_analyze_duration =
        FLUXER_ANIMATION_ANALYZE_DURATION;
    if (context->request.require_complete != 0) {
        context->input_format->error_recognition = AV_EF_EXPLODE;
    }
    int av_status = avformat_open_input(
        &context->input_format, NULL, context->request.forced_format, NULL);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    status = fluxer_prepare_untrusted_av_input(context->input_format);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    av_status = fluxer_find_stream_info_bounded(context->input_format);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    return fluxer_prepare_untrusted_av_input(context->input_format);
}

static int ff_animation_validate_stream_limits(
    struct ff_animation_decode_context *context
) {
    assert(context != NULL);
    assert(context->input_stream != NULL);
    assert(context->input_stream->codecpar != NULL);
    if (context->expected_frames == 0) {
        int64_t stream_frames = context->input_stream->nb_frames;
        if (stream_frames < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        if (stream_frames > INT_MAX) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        context->expected_frames = (int)stream_frames;
    }
    if (context->expected_frames > 0) {
        if (context->expected_frames < context->request.minimum_frames) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
    }
    if (context->request.require_complete != 0) {
        if (context->expected_frames > context->request.max_frames) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
    }

    int width = context->input_stream->codecpar->width;
    int height = context->input_stream->codecpar->height;
    size_t frame_bytes = 0;
    if (ff_validate_rgba_geometry(width, height, &frame_bytes) != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t frame_pixels = frame_bytes / 4u;
    if (frame_pixels > context->request.max_total_pixels) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (context->request.require_complete != 0) {
        if (context->expected_frames > 0) {
            if ((size_t)context->expected_frames >
                context->request.max_total_pixels / frame_pixels) {
                return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
            }
        }
    }

    context->capacity_hint = 1;
    int packet_frame_limit = context->request.max_frames;
    if (context->request.require_complete != 0) {
        if (context->expected_frames > 0) {
            context->capacity_hint = context->expected_frames;
            packet_frame_limit = context->expected_frames;
        }
    }
    if (packet_frame_limit <= 0) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (packet_frame_limit >
        INT_MAX / FLUXER_ANIMATION_PACKETS_PER_FRAME_LIMIT) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    context->packet_limit =
        packet_frame_limit * FLUXER_ANIMATION_PACKETS_PER_FRAME_LIMIT;
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_animation_open_decoder(
    struct ff_animation_decode_context *context
) {
    assert(context != NULL);
    assert(context->input_format != NULL);
    assert(context->decoder == NULL);
    int deadline_status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    const AVCodec *decoder = NULL;
    int stream_index = ff_find_primary_video_stream(
        context->input_format, &decoder);
    if (stream_index < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if ((unsigned int)stream_index >= context->input_format->nb_streams) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    context->input_stream = context->input_format->streams[stream_index];
    if (context->input_stream == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (context->input_stream->codecpar == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int status = ff_animation_validate_stream_limits(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (decoder == NULL) {
        decoder = avcodec_find_decoder(
            context->input_stream->codecpar->codec_id);
    }
    if (!fluxer_video_decoder_allowed(context->input_stream, decoder)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }

    context->decoder = avcodec_alloc_context3(decoder);
    if (context->decoder == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    int av_status = avcodec_parameters_to_context(
        context->decoder, context->input_stream->codecpar);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    context->decoder->max_pixels = (int64_t)FLUXER_MAX_VIDEO_PIXELS;
    context->decoder->thread_count = context->request.decoder_threads;
    if (context->request.require_complete != 0) {
        context->decoder->err_recognition =
            AV_EF_CRCCHECK | AV_EF_BITSTREAM | AV_EF_BUFFER | AV_EF_EXPLODE;
    }
    av_status = avcodec_open2(context->decoder, decoder, NULL);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    context->stream_index = stream_index;
    context->packet = av_packet_alloc();
    context->frame = av_frame_alloc();
    if (context->packet == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (context->frame == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_animation_receive_frames(
    struct ff_animation_decode_context *context,
    int64_t packet_duration,
    int flushing
) {
    assert(context != NULL);
    assert(context->decoder != NULL);
    assert(context->frame != NULL);
    assert(flushing >= 0);
    assert(flushing <= 1);
    while (1) {
        int deadline_status = fluxer_native_deadline_status(
            context->request.deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
        int receive_status = avcodec_receive_frame(
            context->decoder, context->frame);
        if (receive_status == AVERROR_EOF) return FLUXER_NATIVE_STATUS_OK;
        if (receive_status == AVERROR(EAGAIN)) {
            if (flushing != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            return FLUXER_NATIVE_STATUS_OK;
        }
        if (receive_status < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                receive_status, context->request.deadline_monotonic_ms);
        }
        if (context->request.require_complete != 0) {
            if (context->frame->decode_error_flags != 0) {
                av_frame_unref(context->frame);
                return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            }
        }
        int status = ff_animation_accept_frame(context, packet_duration);
        av_frame_unref(context->frame);
        if (status == FF_APPEND_FRAME_LIMIT) {
            if (context->request.require_complete != 0) {
                return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
            }
            context->stopped = 1;
            return FLUXER_NATIVE_STATUS_OK;
        }
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
    }
}

static int ff_animation_flush_decoder(
    struct ff_animation_decode_context *context
) {
    assert(context != NULL);
    assert(context->decoder != NULL);
    int av_status = avcodec_send_packet(context->decoder, NULL);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    return ff_animation_receive_frames(
        context, context->last_packet_duration, 1);
}

static int ff_animation_decode_packets(
    struct ff_animation_decode_context *context
) {
    assert(context != NULL);
    assert(context->input_format != NULL);
    assert(context->packet != NULL);
    int read_status = 0;
    while (!context->stopped) {
        int deadline_status = fluxer_native_deadline_status(
            context->request.deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
        read_status = av_read_frame(context->input_format, context->packet);
        if (read_status < 0) break;
        context->packets_read++;
        if (context->packets_read > context->packet_limit) {
            av_packet_unref(context->packet);
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        if (context->packet->stream_index != context->stream_index) {
            av_packet_unref(context->packet);
            continue;
        }
        int64_t packet_duration = context->packet->duration;
        context->last_packet_duration = packet_duration;
        int send_status = avcodec_send_packet(
            context->decoder, context->packet);
        av_packet_unref(context->packet);
        if (send_status < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                send_status, context->request.deadline_monotonic_ms);
        }
        int status = ff_animation_receive_frames(
            context, packet_duration, 0);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
    }
    if (context->stopped) return FLUXER_NATIVE_STATUS_OK;
    if (read_status != AVERROR_EOF) {
        return fluxer_native_status_from_av_error_with_deadline(
            read_status, context->request.deadline_monotonic_ms);
    }
    return ff_animation_flush_decoder(context);
}

static int ff_animation_validate_decoded_frames(
    const struct ff_animation_decode_context *context
) {
    assert(context != NULL);
    assert(context->frames >= 0);
    if (context->frames < context->request.minimum_frames) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (context->request.require_complete == 0) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    if (context->expected_frames == 0) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    if (context->frames != context->expected_frames) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_animation_build_image(
    struct ff_animation_decode_context *context,
    VipsImage **out
) {
    assert(context != NULL);
    assert(out != NULL);
    assert(*out == NULL);
    assert(context->request.decode_pixels == 1);
    assert(context->pixels != NULL);
    assert(context->delays != NULL);
    int deadline_status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    assert(context->frames >= context->request.minimum_frames);
    if (context->canvas_width <= 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (context->canvas_height <= 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (context->canvas_height > INT_MAX / context->frames) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t frame_bytes = 0;
    if (ff_validate_rgba_geometry(
            context->canvas_width, context->canvas_height,
            &frame_bytes) != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if ((size_t)context->frames > SIZE_MAX / frame_bytes) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t used_bytes = frame_bytes * (size_t)context->frames;
    context->image = vips_image_new_from_memory(
        context->pixels,
        used_bytes,
        context->canvas_width,
        context->canvas_height * context->frames,
        4,
        VIPS_FORMAT_UCHAR);
    if (context->image == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (g_signal_connect_swapped(
            context->image, "postclose", G_CALLBACK(free),
            context->pixels) == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    context->pixels = NULL;
    vips_image_set_int(
        context->image, "page-height", context->canvas_height);
    if (context->frames > 1) {
        vips_image_set_int(context->image, "n-pages", context->frames);
    }
    vips_image_set_array_int(
        context->image, "delay", context->delays, context->frames);
    *out = context->image;
    context->image = NULL;
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_animation_decode_execute(
    struct ff_animation_decode_context *context
) {
    assert(context != NULL);
    int status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = ff_animation_open_input(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = ff_animation_open_decoder(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = ff_animation_decode_packets(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return ff_animation_validate_decoded_frames(context);
}

static void ff_animation_decode_context_clear(
    struct ff_animation_decode_context *context
) {
    assert(context != NULL);
    if (context->image != NULL) g_object_unref(context->image);
    if (context->pixels != NULL) free(context->pixels);
    if (context->delays != NULL) free(context->delays);
    if (context->sws != NULL) sws_freeContext(context->sws);
    if (context->frame != NULL) av_frame_free(&context->frame);
    if (context->packet != NULL) av_packet_free(&context->packet);
    if (context->decoder != NULL) avcodec_free_context(&context->decoder);
    if (context->input_format != NULL) {
        avformat_close_input(&context->input_format);
    }
    if (context->input_avio != NULL) {
        if (context->input_avio->buffer != NULL) {
            av_freep(&context->input_avio->buffer);
        }
        avio_context_free(&context->input_avio);
    }
    if (context->input_avio_buffer != NULL) {
        av_free(context->input_avio_buffer);
    }
}

static int ffmpeg_decode_animation_stack(
    const void *media_data,
    size_t media_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    VipsImage **out,
    int *out_frame_count,
    int max_frames,
    size_t max_total_pixels,
    int require_complete,
    const char *allowed_formats,
    const AVInputFormat *forced_format,
    int expected_frames,
    int minimum_frames,
    int decode_pixels
) {
    if (out_frame_count == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out_frame_count = 0;
    if (decode_pixels != 0) {
        if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        *out = NULL;
    } else if (out != NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    struct ff_animation_decode_request request = {
        .data = media_data,
        .len = media_len,
        .decoder_threads = decoder_threads,
        .deadline_monotonic_ms = deadline_monotonic_ms,
        .max_frames = max_frames,
        .max_total_pixels = max_total_pixels,
        .require_complete = require_complete,
        .allowed_formats = allowed_formats,
        .forced_format = forced_format,
        .expected_frames = expected_frames,
        .minimum_frames = minimum_frames,
        .decode_pixels = decode_pixels,
    };
    int status = ff_animation_decode_request_valid(&request);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;

    struct ff_animation_decode_context context;
    ff_animation_decode_context_init(&context, &request);
    status = ff_animation_decode_execute(&context);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        if (decode_pixels != 0) {
            status = ff_animation_build_image(&context, out);
        }
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        *out_frame_count = context.frames;
        if (decode_pixels == 0) {
            assert(context.pixels == NULL);
            assert(context.delays == NULL);
        }
    }
    ff_animation_decode_context_clear(&context);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        assert(*out_frame_count == 0);
        if (out != NULL) assert(*out == NULL);
    }
    return status;
}
int fluxer_ffmpeg_decode_apng(
    const void *apng_data,
    size_t apng_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    VipsImage **out,
    int max_frames,
    size_t max_total_pixels,
    int require_complete,
    uint32_t *out_num_plays
) {
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = NULL;
    if (out_num_plays == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out_num_plays = 0;
    int expected_frames = 0;
    uint32_t num_plays = 0;
    int validate_rc = fluxer_validate_complete_apng(
        apng_data, apng_len,
        require_complete ? max_frames : 0,
        require_complete ? max_total_pixels : 0,
        NULL, NULL,
        &expected_frames, &num_plays);
    if (validate_rc != 0) return validate_rc;
    int deadline_status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    int decoded_frames = 0;
    int status = ffmpeg_decode_animation_stack(
        apng_data, apng_len, decoder_threads, deadline_monotonic_ms,
        out, &decoded_frames, max_frames, max_total_pixels,
        require_complete, "apng", av_find_input_format("apng"),
        expected_frames, 1, 1);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        *out_num_plays = num_plays;
    } else {
        assert(*out_num_plays == 0);
    }
    return status;
}

int fluxer_ffmpeg_decode_bmp(
    const void *bmp_data,
    size_t bmp_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    VipsImage **out,
    size_t max_total_pixels
) {
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = NULL;
    const AVInputFormat *bmp_format =
        av_find_input_format(FLUXER_AV_BMP_INPUT_FORMAT);
    if (bmp_format == NULL) return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    int decoded_frames = 0;
    int status = ffmpeg_decode_animation_stack(
        bmp_data, bmp_len, decoder_threads, deadline_monotonic_ms,
        out, &decoded_frames, 1, max_total_pixels, 1,
        FLUXER_AV_BMP_INPUT_FORMAT, bmp_format, 1, 1, 1);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        assert(*out != NULL);
        assert(decoded_frames == 1);
    } else {
        assert(*out == NULL);
    }
    return status;
}

int fluxer_ffmpeg_count_heif_sequence_frames(
    const void *heif_data,
    size_t heif_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    int max_frames,
    size_t max_total_pixels,
    int *out_frame_count
) {
    if (out_frame_count == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out_frame_count = 0;
    int validation_status = fluxer_heif_validate(
        heif_data, heif_len, deadline_monotonic_ms);
    if (validation_status != FLUXER_NATIVE_STATUS_OK) return validation_status;
    int status = ffmpeg_decode_animation_stack(
        heif_data, heif_len, decoder_threads, deadline_monotonic_ms,
        NULL, out_frame_count, max_frames, max_total_pixels,
        1, FLUXER_AV_INPUT_FORMATS, NULL, 0, 2, 0);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        assert(*out_frame_count >= 2);
    } else {
        assert(*out_frame_count == 0);
    }
    return status;
}

int fluxer_ffmpeg_decode_heif_sequence(
    const void *heif_data,
    size_t heif_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    VipsImage **out,
    int max_frames,
    size_t max_total_pixels,
    int *out_frame_count
) {
    if (out_frame_count == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out_frame_count = 0;
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = NULL;
    int validation_status = fluxer_heif_validate(
        heif_data, heif_len, deadline_monotonic_ms);
    if (validation_status != FLUXER_NATIVE_STATUS_OK) return validation_status;
    int status = ffmpeg_decode_animation_stack(
        heif_data, heif_len, decoder_threads, deadline_monotonic_ms,
        out, out_frame_count, max_frames, max_total_pixels,
        1, FLUXER_AV_INPUT_FORMATS, NULL, 0, 2, 1);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        assert(*out != NULL);
        assert(*out_frame_count >= 2);
    } else {
        assert(*out == NULL);
        assert(*out_frame_count == 0);
    }
    return status;
}

static int fluxer_vips_copy_animation_rgba(
    VipsRegion *region,
    uint8_t *destination,
    size_t row_bytes,
    int width,
    int height,
    long long deadline_monotonic_ms
) {
    assert(region != NULL);
    assert(destination != NULL);
    assert(row_bytes > 0);
    assert(width > 0);
    assert(height > 0);
    assert(deadline_monotonic_ms >= 0);
    for (int top = 0; top < height; top += FLUXER_VIPS_RGBA_REGION_ROWS) {
        if (fluxer_monotonic_deadline_status(deadline_monotonic_ms) !=
            FLUXER_DEADLINE_PENDING) {
            return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
        }
        int rows = height - top;
        if (rows > FLUXER_VIPS_RGBA_REGION_ROWS) rows = FLUXER_VIPS_RGBA_REGION_ROWS;
        VipsRect area = { .left = 0, .top = top, .width = width, .height = rows };
        if (vips_region_prepare(region, &area) != 0) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (!vips_rect_includesrect(&region->valid, &area)) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (region->data == NULL || region->bpl <= 0) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if ((size_t)region->bpl < row_bytes) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        for (int y = top; y < top + rows; y++) {
            const uint8_t *source = VIPS_REGION_ADDR(region, 0, y);
            memcpy(destination + (size_t)y * row_bytes, source, row_bytes);
        }
    }
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_vips_extract_animation_rgba_strip(
    VipsImage *input,
    uint8_t *destination,
    size_t destination_capacity,
    long long deadline_monotonic_ms,
    size_t *out_size
) {
    if (out_size != NULL) *out_size = 0;
    if (input == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (destination == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (out_size == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }

    VipsImage *rgba = NULL;
    int status = fluxer_vips_image_to_rgba(input, &rgba);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int width = vips_image_get_width(rgba);
    int height = vips_image_get_height(rgba);
    size_t required = 0;
    if (ff_validate_rgba_geometry(width, height, &required) != 0) {
        g_object_unref(rgba);
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (destination_capacity != required) {
        g_object_unref(rgba);
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }

    VipsRegion *region = vips_region_new(rgba);
    if (region == NULL) {
        g_object_unref(rgba);
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    size_t row_bytes = (size_t)width * 4u;
    status = fluxer_vips_copy_animation_rgba(
        region, destination, row_bytes, width, height,
        deadline_monotonic_ms);
    g_object_unref(region);
    g_object_unref(rgba);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    *out_size = required;
    return FLUXER_NATIVE_STATUS_OK;
}
int fluxer_vips_read_animation_delays_ms(VipsImage *image, int n_pages,
                                         int **out_delays) {
    if (out_delays == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out_delays = NULL;
    if (image == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (n_pages <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if ((size_t)n_pages > SIZE_MAX / sizeof(int)) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (vips_image_get_typeof(image, "delay") != 0) {
        int *arr = NULL;
        int n = 0;
        if (vips_image_get_array_int(image, "delay", &arr, &n) != 0) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (arr == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        if (n != n_pages) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        int *out = (int *)malloc((size_t)n_pages * sizeof(int));
        if (out == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
        for (int i = 0; i < n_pages; i++) {
            if (arr[i] <= 0) {
                out[i] = FLUXER_DEFAULT_ANIMATION_FRAME_DELAY_MS;
            } else if (arr[i] < FLUXER_MIN_ANIMATION_FRAME_DELAY_MS) {
                out[i] = FLUXER_MIN_ANIMATION_FRAME_DELAY_MS;
            } else {
                out[i] = arr[i];
            }
        }
        *out_delays = out;
        return FLUXER_NATIVE_STATUS_OK;
    }
    if (vips_image_get_typeof(image, "gif-delay") == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int gif_delay = 0;
    if (vips_image_get_int(image, "gif-delay", &gif_delay) != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (gif_delay > INT_MAX / 10) return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    int *out = (int *)malloc((size_t)n_pages * sizeof(int));
    if (out == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    int delay_ms = gif_delay > 0 ? gif_delay * 10 : 0;
    if (delay_ms <= 0) {
        delay_ms = FLUXER_DEFAULT_ANIMATION_FRAME_DELAY_MS;
    } else if (delay_ms < FLUXER_MIN_ANIMATION_FRAME_DELAY_MS) {
        delay_ms = FLUXER_MIN_ANIMATION_FRAME_DELAY_MS;
    }
    for (int i = 0; i < n_pages; i++) out[i] = delay_ms;
    *out_delays = out;
    return FLUXER_NATIVE_STATUS_OK;
}
