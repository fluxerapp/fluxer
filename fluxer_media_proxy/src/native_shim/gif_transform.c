// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

#define FLUXER_GIF_AVIO_BUFFER_SIZE (64u * 1024u)
#define FLUXER_GIF_PROBE_SIZE (5u * 1024u * 1024u)
#define FLUXER_GIF_ANALYZE_DURATION (5 * AV_TIME_BASE)

struct gif_output_buffer {
    uint8_t *data;
    size_t len;
    size_t position;
    size_t capacity;
    size_t max_size;
    int error;
};

struct gif_resize_request {
    const uint8_t *data;
    size_t len;
    int decoder_threads;
    int target_width;
    int target_height;
    long long deadline_monotonic_ms;
    int max_source_frames;
    int max_encode_frames;
    int max_encode_duration_ms;
    size_t max_total_pixels;
    size_t max_output_size;
};

struct gif_resize_result {
    void *data;
    size_t size;
    size_t capacity;
};

struct gif_resize_context {
    struct gif_resize_request request;
    struct ff_mem_reader reader;
    unsigned char *input_avio_buffer;
    AVIOContext *input_avio;
    AVFormatContext *input_format;
    AVCodecContext *decoder;
    int stream_index;
    unsigned char *output_avio_buffer;
    AVIOContext *output_avio;
    AVFormatContext *output_format;
    AVCodecContext *encoder;
    AVStream *output_stream;
    AVFilterGraph *filter_graph;
    AVFilterContext *filter_source;
    AVFilterContext *filter_sink;
    AVPacket *input_packet;
    AVPacket *encoded_packet;
    AVFrame *decoded_frame;
    AVFrame *scaled_frame;
    struct gif_output_buffer output;
    int *frame_delays_cs;
    int expected_frames;
    int encode_frames;
    int input_packet_limit;
    int input_packets;
    int decode_complete;
    int decoded_frames;
    int frames_written;
    int packets_written;
    int loop_count;
    int64_t next_pts;
};

static int gif_output_reject(struct gif_output_buffer *output, int error) {
    if (output->error == 0) output->error = error;
    return output->error;
}

static int gif_output_reserve(struct gif_output_buffer *output, size_t required) {
    if (required <= output->capacity) return 0;
    if (required > output->max_size) return gif_output_reject(output, AVERROR(ENOSPC));

    size_t next_capacity = output->capacity > 0 ? output->capacity : required;
    while (next_capacity < required) {
        if (next_capacity > output->max_size / 2u) {
            next_capacity = output->max_size;
        } else {
            next_capacity *= 2u;
        }
    }
    if (next_capacity < required || next_capacity > output->max_size) {
        return gif_output_reject(output, AVERROR(ENOSPC));
    }

    uint8_t *next = av_realloc(output->data, next_capacity);
    if (next == NULL) return gif_output_reject(output, AVERROR(ENOMEM));
    output->data = next;
    output->capacity = next_capacity;
    return 0;
}

static int gif_output_write(void *opaque, const uint8_t *bytes, int byte_count) {
    if (opaque == NULL) return AVERROR(EINVAL);
    struct gif_output_buffer *output = opaque;
    if (output->error != 0) return output->error;
    if (byte_count < 0 || (byte_count > 0 && bytes == NULL)) {
        return gif_output_reject(output, AVERROR(EINVAL));
    }
    if (byte_count == 0) return 0;

    size_t incoming = (size_t)byte_count;
    if (output->position > output->max_size ||
        incoming > output->max_size - output->position) {
        return gif_output_reject(output, AVERROR(ENOSPC));
    }
    size_t required = output->position + incoming;
    int reserve_rc = gif_output_reserve(output, required);
    if (reserve_rc != 0) return reserve_rc;
    if (output->position > output->len) {
        memset(output->data + output->len, 0, output->position - output->len);
    }
    memcpy(output->data + output->position, bytes, incoming);
    output->position = required;
    if (required > output->len) output->len = required;
    return byte_count;
}

static int64_t gif_output_seek(void *opaque, int64_t offset, int whence) {
    if (opaque == NULL) return AVERROR(EINVAL);
    struct gif_output_buffer *output = opaque;
    if (output->error != 0) return output->error;
    if (whence == AVSEEK_SIZE) {
        if (output->len > (size_t)INT64_MAX) {
            return gif_output_reject(output, AVERROR(EOVERFLOW));
        }
        return (int64_t)output->len;
    }

    int mode = whence & ~AVSEEK_FORCE;
    size_t base_size = 0;
    if (mode == SEEK_SET) {
        base_size = 0;
    } else if (mode == SEEK_CUR) {
        base_size = output->position;
    } else if (mode == SEEK_END) {
        base_size = output->len;
    } else {
        return gif_output_reject(output, AVERROR(EINVAL));
    }
    if (base_size > (size_t)INT64_MAX) {
        return gif_output_reject(output, AVERROR(EOVERFLOW));
    }
    int64_t base = (int64_t)base_size;
    if (offset < -base || offset > INT64_MAX - base) {
        return gif_output_reject(output, AVERROR(EINVAL));
    }
    int64_t next = base + offset;
    if (next < 0 || (uint64_t)next > output->max_size) {
        return gif_output_reject(output, AVERROR(ENOSPC));
    }
    output->position = (size_t)next;
    return next;
}

static int gif_output_flush(AVIOContext *avio, struct gif_output_buffer *output) {
    if (avio == NULL || output == NULL) return AVERROR(EINVAL);
    avio_flush(avio);
    if (output->error != 0) return output->error;
    if (avio->error < 0) return avio->error;
    return 0;
}

static int write_encoded_gif_packets(AVFormatContext *out_fmt, AVCodecContext *enc_ctx,
                                     AVStream *out_stream, AVPacket *packet, AVFrame *frame,
                                     struct gif_output_buffer *output, int max_packets,
                                     int *packets_written,
                                     long long deadline_monotonic_ms) {
    if (out_fmt == NULL || enc_ctx == NULL || out_stream == NULL ||
        packet == NULL || output == NULL || max_packets <= 0 ||
        packets_written == NULL || *packets_written < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int deadline_status = fluxer_native_deadline_status(
        deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    int rc = avcodec_send_frame(enc_ctx, frame);
    if (rc < 0) return fluxer_native_status_from_av_error(rc);
    while (1) {
        deadline_status = fluxer_native_deadline_status(
            deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            av_packet_unref(packet);
            return deadline_status;
        }
        rc = avcodec_receive_packet(enc_ctx, packet);
        if (rc == AVERROR(EAGAIN) || rc == AVERROR_EOF) {
            av_packet_unref(packet);
            return FLUXER_NATIVE_STATUS_OK;
        }
        if (rc < 0) {
            av_packet_unref(packet);
            return fluxer_native_status_from_av_error(rc);
        }
        if (*packets_written >= max_packets) {
            av_packet_unref(packet);
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        av_packet_rescale_ts(packet, enc_ctx->time_base, out_stream->time_base);
        packet->stream_index = out_stream->index;
        rc = av_interleaved_write_frame(out_fmt, packet);
        av_packet_unref(packet);
        if (rc < 0) return fluxer_native_status_from_av_error(rc);
        rc = gif_output_flush(out_fmt->pb, output);
        if (rc < 0) return fluxer_native_status_from_av_error(rc);
        (*packets_written)++;
    }
}

static int validate_gif_resize_request(
    const struct gif_resize_request *request,
    int *out_expected_frames
) {
    assert(request != NULL);
    assert(out_expected_frames != NULL);
    *out_expected_frames = 0;
    if (request->data == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->len == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->len > (size_t)INT64_MAX) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (request->target_width <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (request->target_height <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (!fluxer_ffmpeg_decoder_threads_valid(request->decoder_threads)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->max_source_frames <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->max_encode_frames <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->max_encode_duration_ms <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->max_total_pixels == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (request->max_output_size == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;

    int expected_frames = 0;
    int status = fluxer_validate_complete_gif(
        request->data, request->len, request->max_source_frames,
        request->max_total_pixels, &expected_frames);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = fluxer_native_deadline_status(request->deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if ((size_t)expected_frames > SIZE_MAX / sizeof(int)) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (request->target_width > FLUXER_MAX_VIDEO_FRAME_DIMENSION) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (request->target_height > FLUXER_MAX_VIDEO_FRAME_DIMENSION) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    size_t target_height = (size_t)request->target_height;
    if ((size_t)request->target_width > request->max_total_pixels / target_height) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    size_t target_pixels = (size_t)request->target_width * target_height;
    if ((size_t)expected_frames > request->max_total_pixels / target_pixels) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (expected_frames > INT_MAX / FLUXER_ANIMATION_PACKETS_PER_FRAME_LIMIT) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    *out_expected_frames = expected_frames;
    return FLUXER_NATIVE_STATUS_OK;
}

static void gif_resize_context_init(
    struct gif_resize_context *context,
    const struct gif_resize_request *request,
    int expected_frames
) {
    assert(context != NULL);
    assert(request != NULL);
    assert(expected_frames > 0);
    assert(request->max_output_size > 0);
    memset(context, 0, sizeof(*context));
    context->request = *request;
    context->reader.data = request->data;
    context->reader.len = request->len;
    context->stream_index = -1;
    context->output.max_size = request->max_output_size;
    context->expected_frames = expected_frames;
    context->loop_count = -1;
    context->input_packet_limit =
        expected_frames * FLUXER_ANIMATION_PACKETS_PER_FRAME_LIMIT;
}

static int gif_resize_load_frame_delays(struct gif_resize_context *context) {
    assert(context != NULL);
    assert(context->frame_delays_cs == NULL);
    assert(context->expected_frames > 0);
    size_t delays_size = (size_t)context->expected_frames * sizeof(int);
    context->frame_delays_cs = malloc(delays_size);
    if (context->frame_delays_cs == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    int status = fluxer_read_gif_frame_delays(
        context->request.data,
        context->request.len,
        context->frame_delays_cs,
        context->expected_frames,
        0,
        &context->loop_count);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return fluxer_gif_animation_frame_budget(
        context->frame_delays_cs,
        context->expected_frames,
        context->request.max_encode_frames,
        context->request.max_encode_duration_ms,
        &context->encode_frames);
}

static int gif_resize_open_input(struct gif_resize_context *context) {
    assert(context != NULL);
    assert(context->input_avio == NULL);
    assert(context->input_format == NULL);
    context->input_avio_buffer = av_malloc(FLUXER_GIF_AVIO_BUFFER_SIZE);
    if (context->input_avio_buffer == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    context->input_avio = avio_alloc_context(
        context->input_avio_buffer,
        (int)FLUXER_GIF_AVIO_BUFFER_SIZE,
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
    int status = fluxer_restrict_untrusted_av_context(context->input_format, "gif");
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    context->input_format->pb = context->input_avio;
    context->input_format->interrupt_callback.callback =
        fluxer_ffmpeg_interrupt_deadline;
    context->input_format->interrupt_callback.opaque =
        &context->request.deadline_monotonic_ms;
    context->input_format->flags |= AVFMT_FLAG_CUSTOM_IO;
    context->input_format->probesize = FLUXER_GIF_PROBE_SIZE;
    context->input_format->max_analyze_duration = FLUXER_GIF_ANALYZE_DURATION;
    context->input_format->error_recognition = AV_EF_EXPLODE;

    int av_status = avformat_open_input(&context->input_format, NULL, NULL, NULL);
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

static int gif_resize_open_decoder(struct gif_resize_context *context) {
    assert(context != NULL);
    assert(context->input_format != NULL);
    assert(context->decoder == NULL);
    int deadline_status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    const AVCodec *decoder = NULL;
    int stream_index = av_find_best_stream(
        context->input_format, AVMEDIA_TYPE_VIDEO, -1, -1, &decoder, 0);
    if (stream_index < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if ((unsigned int)stream_index >= context->input_format->nb_streams) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    AVStream *stream = context->input_format->streams[stream_index];
    if (stream == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (stream->codecpar == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (decoder == NULL) {
        decoder = avcodec_find_decoder(stream->codecpar->codec_id);
    }
    if (decoder == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!fluxer_video_decoder_allowed(stream, decoder)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }

    context->decoder = avcodec_alloc_context3(decoder);
    if (context->decoder == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    int av_status = avcodec_parameters_to_context(context->decoder, stream->codecpar);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    context->decoder->max_pixels = (int64_t)FLUXER_MAX_VIDEO_PIXELS;
    context->decoder->thread_count = context->request.decoder_threads;
    context->decoder->err_recognition =
        AV_EF_CRCCHECK | AV_EF_BITSTREAM | AV_EF_BUFFER | AV_EF_EXPLODE;
    av_status = avcodec_open2(context->decoder, decoder, NULL);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    context->stream_index = stream_index;
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_resize_open_output_io(struct gif_resize_context *context) {
    assert(context != NULL);
    assert(context->output_format == NULL);
    assert(context->output_avio == NULL);
    int av_status = avformat_alloc_output_context2(
        &context->output_format, NULL, "gif", NULL);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    if (context->output_format == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }

    size_t buffer_size = FLUXER_GIF_AVIO_BUFFER_SIZE;
    if (context->request.max_output_size < buffer_size) {
        buffer_size = context->request.max_output_size;
    }
    context->output_avio_buffer = av_malloc(buffer_size);
    if (context->output_avio_buffer == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    context->output_avio = avio_alloc_context(
        context->output_avio_buffer,
        (int)buffer_size,
        1,
        &context->output,
        NULL,
        gif_output_write,
        gif_output_seek);
    if (context->output_avio == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    context->output_avio_buffer = NULL;
    context->output_format->pb = context->output_avio;
    context->output_format->flags |= AVFMT_FLAG_CUSTOM_IO;
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_resize_open_encoder(struct gif_resize_context *context) {
    assert(context != NULL);
    assert(context->output_format != NULL);
    assert(context->encoder == NULL);
    const AVCodec *encoder = avcodec_find_encoder(AV_CODEC_ID_GIF);
    if (encoder == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    context->output_stream = avformat_new_stream(context->output_format, NULL);
    if (context->output_stream == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    assert(context->output_stream->codecpar != NULL);
    context->encoder = avcodec_alloc_context3(encoder);
    if (context->encoder == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    context->encoder->thread_count = 1;
    context->encoder->width = context->request.target_width;
    context->encoder->height = context->request.target_height;
    context->encoder->pix_fmt = AV_PIX_FMT_PAL8;
    context->encoder->time_base = (AVRational){ 1, 100 };
    context->encoder->framerate = (AVRational){ 100, 1 };
    if (context->output_format->oformat != NULL) {
        if ((context->output_format->oformat->flags & AVFMT_GLOBALHEADER) != 0) {
            context->encoder->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;
        }
    }

    AVDictionary *options = NULL;
    int av_status = av_dict_set(&options, "gifflags", "-offsetting", 0);
    if (av_status >= 0) {
        av_status = avcodec_open2(context->encoder, encoder, &options);
    }
    av_dict_free(&options);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    av_status = avcodec_parameters_from_context(
        context->output_stream->codecpar, context->encoder);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    context->output_stream->time_base = context->encoder->time_base;
    if (context->loop_count < -1 || context->loop_count > UINT16_MAX) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    AVDictionary *mux_options = NULL;
    av_status = av_dict_set_int(
        &mux_options, "loop", context->loop_count, 0);
    if (av_status >= 0) {
        av_status = avformat_write_header(
            context->output_format, &mux_options);
    }
    if (av_status >= 0 && av_dict_count(mux_options) != 0) {
        av_status = AVERROR(EINVAL);
    }
    av_dict_free(&mux_options);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    av_status = gif_output_flush(context->output_avio, &context->output);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_resize_open_pipeline(struct gif_resize_context *context) {
    assert(context != NULL);
    assert(context->decoder != NULL);
    assert(context->encoder != NULL);
    int status = fluxer_gif_setup_filter_graph(
        &context->filter_graph,
        &context->filter_source,
        &context->filter_sink,
        context->decoder->width,
        context->decoder->height,
        context->decoder->pix_fmt,
        context->encoder->time_base,
        context->request.target_width,
        context->request.target_height);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;

    context->input_packet = av_packet_alloc();
    context->encoded_packet = av_packet_alloc();
    context->decoded_frame = av_frame_alloc();
    context->scaled_frame = av_frame_alloc();
    if (context->input_packet == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (context->encoded_packet == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (context->decoded_frame == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (context->scaled_frame == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_resize_drain_filter(
    struct gif_resize_context *context,
    int require_eof
) {
    assert(context != NULL);
    assert(context->filter_sink != NULL);
    assert(context->scaled_frame != NULL);
    assert(context->output_format != NULL);
    assert(context->encoder != NULL);
    assert(context->output_stream != NULL);
    assert(context->encoded_packet != NULL);
    assert(context->encode_frames > 0);
    assert(context->frames_written >= 0);
    assert(context->packets_written >= 0);
    assert(require_eof >= 0);
    assert(require_eof <= 1);
    while (1) {
        int deadline_status = fluxer_native_deadline_status(
            context->request.deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
        int sink_status = av_buffersink_get_frame(
            context->filter_sink, context->scaled_frame);
        if (sink_status == AVERROR_EOF) return FLUXER_NATIVE_STATUS_OK;
        if (sink_status == AVERROR(EAGAIN)) {
            if (require_eof != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            return FLUXER_NATIVE_STATUS_OK;
        }
        if (sink_status < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                sink_status, context->request.deadline_monotonic_ms);
        }
        if (context->frames_written >= context->encode_frames) {
            av_frame_unref(context->scaled_frame);
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        int status = write_encoded_gif_packets(
            context->output_format,
            context->encoder,
            context->output_stream,
            context->encoded_packet,
            context->scaled_frame,
            &context->output,
            context->encode_frames,
            &context->packets_written,
            context->request.deadline_monotonic_ms);
        av_frame_unref(context->scaled_frame);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
        context->frames_written++;
    }
}

static int gif_resize_submit_decoded_frame(
    struct gif_resize_context *context
) {
    assert(context != NULL);
    assert(context->decoded_frame != NULL);
    assert(context->frame_delays_cs != NULL);
    assert(context->filter_source != NULL);
    if (context->decoded_frame->decode_error_flags != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (context->decoded_frames >= context->encode_frames) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    int64_t duration_cs = context->frame_delays_cs[context->decoded_frames];
    if (duration_cs <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (context->next_pts > INT64_MAX - duration_cs) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    context->decoded_frame->pts = context->next_pts;
    context->decoded_frame->duration = duration_cs;
    context->next_pts += duration_cs;
    context->decoded_frames++;

    int av_status = av_buffersrc_add_frame_flags(
        context->filter_source, context->decoded_frame, 0);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    av_frame_unref(context->decoded_frame);
    return gif_resize_drain_filter(context, 0);
}

static int gif_resize_receive_frames(
    struct gif_resize_context *context,
    int flushing
) {
    assert(context != NULL);
    assert(context->decoder != NULL);
    assert(context->decoded_frame != NULL);
    assert(flushing >= 0);
    assert(flushing <= 1);
    while (1) {
        int deadline_status = fluxer_native_deadline_status(
            context->request.deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
        if (context->decoded_frames >= context->encode_frames) {
            context->decode_complete = 1;
            return FLUXER_NATIVE_STATUS_OK;
        }
        int receive_status = avcodec_receive_frame(
            context->decoder, context->decoded_frame);
        if (receive_status == AVERROR_EOF) return FLUXER_NATIVE_STATUS_OK;
        if (receive_status == AVERROR(EAGAIN)) {
            if (flushing != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            return FLUXER_NATIVE_STATUS_OK;
        }
        if (receive_status < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                receive_status, context->request.deadline_monotonic_ms);
        }
        int status = gif_resize_submit_decoded_frame(context);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
    }
}

static int gif_resize_decode_packets(struct gif_resize_context *context) {
    assert(context != NULL);
    assert(context->input_format != NULL);
    assert(context->input_packet != NULL);
    assert(context->decoder != NULL);
    int read_status = 0;
    while (context->decode_complete == 0 &&
           (read_status = av_read_frame(
                context->input_format, context->input_packet)) >= 0) {
        context->input_packets++;
        if (context->input_packets > context->input_packet_limit) {
            av_packet_unref(context->input_packet);
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        if (fluxer_native_deadline_status(
                context->request.deadline_monotonic_ms) !=
            FLUXER_NATIVE_STATUS_OK) {
            av_packet_unref(context->input_packet);
            return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
        }
        if (context->input_packet->stream_index != context->stream_index) {
            av_packet_unref(context->input_packet);
            continue;
        }
        int send_status = avcodec_send_packet(
            context->decoder, context->input_packet);
        av_packet_unref(context->input_packet);
        if (send_status < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                send_status, context->request.deadline_monotonic_ms);
        }
        int status = gif_resize_receive_frames(context, 0);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
    }
    if (context->decode_complete != 0) return FLUXER_NATIVE_STATUS_OK;
    if (read_status != AVERROR_EOF) {
        return fluxer_native_status_from_av_error_with_deadline(
            read_status, context->request.deadline_monotonic_ms);
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_resize_flush_pipeline(struct gif_resize_context *context) {
    assert(context != NULL);
    assert(context->decoder != NULL);
    assert(context->filter_source != NULL);
    assert(context->output_format != NULL);
    assert(context->output_avio != NULL);
    int av_status = avcodec_send_packet(context->decoder, NULL);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    int status = gif_resize_receive_frames(context, 1);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (context->next_pts <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;

    av_status = av_buffersrc_add_frame_flags(context->filter_source, NULL, 0);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    status = gif_resize_drain_filter(context, 1);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = write_encoded_gif_packets(
        context->output_format,
        context->encoder,
        context->output_stream,
        context->encoded_packet,
        NULL,
        &context->output,
        context->encode_frames,
        &context->packets_written,
        context->request.deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;

    av_status = av_write_trailer(context->output_format);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    av_status = gif_output_flush(context->output_avio, &context->output);
    if (av_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            av_status, context->request.deadline_monotonic_ms);
    }
    int deadline_status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    if (context->decoded_frames != context->encode_frames) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (context->frames_written != context->encode_frames) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (context->packets_written != context->encode_frames) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_resize_take_output(
    struct gif_resize_context *context,
    struct gif_resize_result *result
) {
    assert(context != NULL);
    assert(result != NULL);
    assert(result->data == NULL);
    assert(result->size == 0);
    assert(result->capacity == 0);
    int deadline_status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    if (context->output.len == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (context->output.data == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    assert(context->output.len <= context->output.capacity);
    assert(context->output.capacity <= context->output.max_size);

    int output_frames = 0;
    int status = fluxer_validate_complete_gif(
        context->output.data,
        context->output.len,
        context->encode_frames,
        context->request.max_total_pixels,
        &output_frames);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    deadline_status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    if (output_frames != context->encode_frames) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    status = fluxer_patch_gif_frame_delays(
        context->output.data,
        context->output.len,
        context->frame_delays_cs,
        context->encode_frames);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int output_loop_count = -2;
    status = fluxer_read_gif_frame_delays(
        context->output.data,
        context->output.len,
        NULL,
        context->encode_frames,
        0,
        &output_loop_count);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (output_loop_count != context->loop_count) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    deadline_status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;

    result->data = context->output.data;
    result->size = context->output.len;
    result->capacity = context->output.capacity;
    context->output.data = NULL;
    context->output.len = 0;
    context->output.position = 0;
    context->output.capacity = 0;
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_resize_execute(
    struct gif_resize_context *context,
    struct gif_resize_result *result
) {
    assert(context != NULL);
    assert(result != NULL);
    int status = fluxer_native_deadline_status(
        context->request.deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = gif_resize_load_frame_delays(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = gif_resize_open_input(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = gif_resize_open_decoder(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = gif_resize_open_output_io(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = gif_resize_open_encoder(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = gif_resize_open_pipeline(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = gif_resize_decode_packets(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = gif_resize_flush_pipeline(context);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return gif_resize_take_output(context, result);
}

static int gif_resize_resolve_output_error(
    const struct gif_resize_context *context,
    int status
) {
    assert(context != NULL);
    if (status == FLUXER_NATIVE_STATUS_OK) return status;
    if (context->output.error >= 0) return status;
    int output_status = fluxer_native_status_from_av_error_with_deadline(
        context->output.error, context->request.deadline_monotonic_ms);
    if (output_status == FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED) {
        return output_status;
    }
    if (output_status == FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED) {
        return output_status;
    }
    if (output_status == FLUXER_NATIVE_STATUS_ALLOCATION_FAILED) {
        return output_status;
    }
    return status;
}

static void gif_resize_context_clear(struct gif_resize_context *context) {
    assert(context != NULL);
    if (context->output_avio != NULL) {
        if (context->output_format != NULL) {
            context->output_format->pb = NULL;
        }
        av_freep(&context->output_avio->buffer);
        avio_context_free(&context->output_avio);
    }
    if (context->output_avio_buffer != NULL) {
        av_free(context->output_avio_buffer);
    }
    if (context->output.data != NULL) av_free(context->output.data);
    if (context->scaled_frame != NULL) av_frame_free(&context->scaled_frame);
    if (context->decoded_frame != NULL) av_frame_free(&context->decoded_frame);
    if (context->encoded_packet != NULL) av_packet_free(&context->encoded_packet);
    if (context->input_packet != NULL) av_packet_free(&context->input_packet);
    if (context->filter_graph != NULL) avfilter_graph_free(&context->filter_graph);
    if (context->frame_delays_cs != NULL) free(context->frame_delays_cs);
    if (context->encoder != NULL) avcodec_free_context(&context->encoder);
    if (context->output_format != NULL) {
        avformat_free_context(context->output_format);
    }
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

int fluxer_ffmpeg_resize_gif(
    const void *gif_data,
    size_t gif_len,
    int decoder_threads,
    int target_width,
    int target_height,
    long long deadline_monotonic_ms,
    int max_source_frames,
    int max_encode_frames,
    int max_encode_duration_ms,
    size_t max_total_pixels,
    size_t max_output_size,
    void **out_buf,
    size_t *out_size,
    size_t *out_capacity
) {
    if (out_buf != NULL) *out_buf = NULL;
    if (out_size != NULL) *out_size = 0;
    if (out_capacity != NULL) *out_capacity = 0;
    if (out_buf == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (out_size == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (out_capacity == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;

    struct gif_resize_request request = {
        .data = gif_data,
        .len = gif_len,
        .decoder_threads = decoder_threads,
        .target_width = target_width,
        .target_height = target_height,
        .deadline_monotonic_ms = deadline_monotonic_ms,
        .max_source_frames = max_source_frames,
        .max_encode_frames = max_encode_frames,
        .max_encode_duration_ms = max_encode_duration_ms,
        .max_total_pixels = max_total_pixels,
        .max_output_size = max_output_size,
    };
    int expected_frames = 0;
    int status = validate_gif_resize_request(&request, &expected_frames);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;

    struct gif_resize_context context;
    gif_resize_context_init(&context, &request, expected_frames);
    struct gif_resize_result result = {0};
    status = gif_resize_execute(&context, &result);
    status = gif_resize_resolve_output_error(&context, status);
    gif_resize_context_clear(&context);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        assert(result.data == NULL);
        return status;
    }

    assert(result.data != NULL);
    assert(result.size > 0);
    assert(result.size <= result.capacity);
    *out_buf = result.data;
    *out_size = result.size;
    *out_capacity = result.capacity;
    return FLUXER_NATIVE_STATUS_OK;
}
