// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

#define FLUXER_MAX_VIDEO_PACKETS_FOR_NSFW 512
#define FLUXER_MAX_VIDEO_FRAMES_FOR_NSFW_PER_SAMPLE 512
#define FLUXER_NSFW_BYTE_SEEK_REWIND (5u * 1024u * 1024u)

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

void fluxer_nsfw_frames_reset(struct fluxer_nsfw_frame_out *frames, size_t n) {
    assert(n <= FLUXER_MAX_NSFW_SAMPLES);
    if (frames == NULL) return;
    for (size_t i = 0; i < n; i++) {
        frames[i].data = NULL;
        frames[i].len = 0;
    }
}

static int fluxer_seconds_to_pts(
    double seconds,
    AVRational time_base,
    int64_t *out_pts
) {
    if (out_pts == NULL) return -1;
    *out_pts = 0;
    if (!isfinite(seconds) || seconds < 0.0) return -1;
    if (time_base.num <= 0 || time_base.den <= 0) return -1;
    long double scaled = (long double)seconds * (long double)time_base.den /
                         (long double)time_base.num;
    if (!isfinite(scaled) || scaled < 0.0L || scaled > (long double)INT64_MAX) return -1;
    *out_pts = (int64_t)scaled;
    return 0;
}

static int fluxer_emit_nsfw_frame(
    AVFrame *frame,
    AVCodecContext *dec_ctx,
    AVFormatContext *in_fmt,
    AVStream *in_stream,
    long long deadline_monotonic_ms,
    size_t max_frame_output_size,
    struct fluxer_nsfw_frame_out *out
) {
    if (out == NULL || max_frame_output_size == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    void *out_buf = NULL;
    size_t out_size = 0;
    size_t out_capacity = 0;
    int rc = ff_emit_frame_thumbnail(
        frame, dec_ctx, in_fmt, in_stream, ".jpg[Q=65,strip]",
        FLUXER_NSFW_FRAME_MAX_DIMENSION,
        FLUXER_NSFW_FRAME_MAX_DIMENSION,
        deadline_monotonic_ms,
        max_frame_output_size,
        NULL, NULL,
        &out_buf, &out_size, &out_capacity);
    if (rc != FLUXER_NATIVE_STATUS_OK) {
        if (out_buf != NULL) g_free(out_buf);
        return rc;
    }
    if (out_buf == NULL || out_size == 0 || out_capacity < out_size ||
        out_capacity > max_frame_output_size) {
        if (out_buf != NULL) g_free(out_buf);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    out->data = out_buf;
    out->len = out_size;
    return FLUXER_NATIVE_STATUS_OK;
}

enum ff_selected_receive_result {
    FF_SELECTED_RECEIVE_MORE = 100,
    FF_SELECTED_RECEIVE_EOF = 101
};

struct ff_animation_selection {
    const int *indices;
    size_t count;
    size_t next;
    int decoded;
    int expected;
    size_t max_frame_output_size;
    struct fluxer_nsfw_frame_out *outputs;
};

static int ff_receive_selected_animation_frames(
    ff_thumbnail_context *context,
    struct ff_animation_selection *selection
) {
    while (1) {
        int deadline_status = fluxer_native_deadline_status(
            context->deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        int receive_rc = avcodec_receive_frame(context->decoder, context->frame);
        if (receive_rc == AVERROR(EAGAIN)) return FF_SELECTED_RECEIVE_MORE;
        if (receive_rc == AVERROR_EOF) return FF_SELECTED_RECEIVE_EOF;
        if (receive_rc < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                receive_rc, context->deadline_monotonic_ms);
        }
        if (context->frame->decode_error_flags != 0 ||
            selection->decoded >= selection->expected) {
            av_frame_unref(context->frame);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        int frame_index = selection->decoded++;
        if (frame_index > selection->indices[selection->next]) {
            av_frame_unref(context->frame);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (frame_index == selection->indices[selection->next]) {
            int emit_rc = fluxer_emit_nsfw_frame(
                context->frame, context->decoder, context->format, context->stream,
                context->deadline_monotonic_ms,
                selection->max_frame_output_size,
                &selection->outputs[selection->next]);
            if (emit_rc != FLUXER_NATIVE_STATUS_OK) {
                av_frame_unref(context->frame);
                return emit_rc;
            }
            selection->next++;
        }
        av_frame_unref(context->frame);
    }
}

int fluxer_nsfw_animation_selection_valid(
    const int *indices,
    size_t count,
    int expected_frames
) {
    if (indices == NULL || count == 0 || count > FLUXER_MAX_NSFW_SAMPLES) return 0;
    if (expected_frames <= 0 || indices[0] != 0) return 0;
    for (size_t i = 0; i < count; i++) {
        if (indices[i] < 0 || indices[i] >= expected_frames) return 0;
        if (i > 0 && indices[i] <= indices[i - 1]) return 0;
    }
    return indices[count - 1] == expected_frames - 1;
}

static int ff_read_selected_animation_packets(
    ff_thumbnail_context *context,
    struct ff_animation_selection *selection,
    int packet_limit,
    int *out_read_status
) {
    assert(context != NULL);
    assert(selection != NULL);
    assert(packet_limit > 0);
    assert(out_read_status != NULL);
    int packets_read = 0;
    int read_status = 0;
    while (1) {
        int deadline_status = fluxer_native_deadline_status(
            context->deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        read_status = av_read_frame(context->format, context->packet);
        if (read_status < 0) break;
        packets_read++;
        if (packets_read > packet_limit) {
            av_packet_unref(context->packet);
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        if (context->packet->stream_index != context->stream_index) {
            av_packet_unref(context->packet);
            continue;
        }
        int send_status = avcodec_send_packet(
            context->decoder, context->packet);
        av_packet_unref(context->packet);
        if (send_status < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                send_status, context->deadline_monotonic_ms);
        }
        int receive_status = ff_receive_selected_animation_frames(
            context, selection);
        if (receive_status == FF_SELECTED_RECEIVE_EOF) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (receive_status != FF_SELECTED_RECEIVE_MORE) return receive_status;
    }
    *out_read_status = read_status;
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_flush_selected_animation_frames(
    ff_thumbnail_context *context,
    struct ff_animation_selection *selection,
    int read_status
) {
    assert(context != NULL);
    assert(selection != NULL);
    if (read_status != AVERROR_EOF) {
        return fluxer_native_status_from_av_error_with_deadline(
            read_status, context->deadline_monotonic_ms);
    }
    int deadline_status = fluxer_native_deadline_status(
        context->deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    int send_status = avcodec_send_packet(context->decoder, NULL);
    if (send_status < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            send_status, context->deadline_monotonic_ms);
    }
    int receive_status = ff_receive_selected_animation_frames(
        context, selection);
    if (receive_status == FF_SELECTED_RECEIVE_EOF) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    if (receive_status == FF_SELECTED_RECEIVE_MORE) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return receive_status;
}

static int ff_extract_selected_animation_frames(
    const void *media_data,
    size_t media_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    const char *allowed_format,
    const int *frame_indices,
    size_t n_indices,
    int expected_frames,
    size_t max_frame_output_size,
    struct fluxer_nsfw_frame_out *out_frames
) {
    if (deadline_monotonic_ms < 0 ||
        !fluxer_ffmpeg_decoder_threads_valid(decoder_threads)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!fluxer_nsfw_animation_selection_valid(
            frame_indices, n_indices, expected_frames)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (allowed_format == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (out_frames == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (max_frame_output_size == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (expected_frames >
        INT_MAX / FLUXER_ANIMATION_PACKETS_PER_FRAME_LIMIT) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }

    ff_thumbnail_context context;
    int status = ff_thumbnail_context_open(
        &context, media_data, media_len, allowed_format, 1,
        deadline_monotonic_ms);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = ff_thumbnail_decoder_open(&context, decoder_threads);
    }
    struct ff_animation_selection selection = {
        .indices = frame_indices,
        .count = n_indices,
        .expected = expected_frames,
        .max_frame_output_size = max_frame_output_size,
        .outputs = out_frames,
    };
    int read_status = 0;
    if (status == FLUXER_NATIVE_STATUS_OK) {
        int packet_limit =
            expected_frames * FLUXER_ANIMATION_PACKETS_PER_FRAME_LIMIT;
        status = ff_read_selected_animation_packets(
            &context, &selection, packet_limit, &read_status);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = ff_flush_selected_animation_frames(
            &context, &selection, read_status);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        if (selection.next != selection.count) {
            status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        } else if (selection.decoded != expected_frames) {
            status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
    }
    ff_thumbnail_context_clear(&context);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        fluxer_nsfw_frames_free(out_frames, n_indices);
    }
    return status;
}
int fluxer_ffmpeg_extract_apng_frames_for_nsfw(
    const void *apng_data,
    size_t apng_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    const int *frame_indices,
    size_t n_indices,
    int max_frames,
    size_t max_total_pixels,
    size_t max_frame_output_size,
    struct fluxer_nsfw_frame_out *out_frames
) {
    if (n_indices > FLUXER_MAX_NSFW_SAMPLES) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    fluxer_nsfw_frames_reset(out_frames, n_indices);
    if (apng_data == NULL || apng_len == 0 || frame_indices == NULL ||
        out_frames == NULL || n_indices == 0 || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (max_frames <= 0 || max_total_pixels == 0 || max_frame_output_size == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int deadline_status = fluxer_native_deadline_status(
        deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    int expected_frames = 0;
    int rc = fluxer_validate_complete_apng(
        apng_data, apng_len, max_frames, max_total_pixels,
        NULL, NULL, &expected_frames, NULL);
    if (rc != 0) return rc;
    deadline_status = fluxer_native_deadline_status(
        deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    return ff_extract_selected_animation_frames(
        apng_data, apng_len, decoder_threads, deadline_monotonic_ms,
        "apng", frame_indices, n_indices,
        expected_frames, max_frame_output_size, out_frames);
}

int fluxer_ffmpeg_extract_gif_frames_for_nsfw(
    const void *gif_data,
    size_t gif_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    const int *frame_indices,
    size_t n_indices,
    int max_frames,
    size_t max_total_pixels,
    size_t max_frame_output_size,
    struct fluxer_nsfw_frame_out *out_frames
) {
    if (n_indices > FLUXER_MAX_NSFW_SAMPLES) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    fluxer_nsfw_frames_reset(out_frames, n_indices);
    if (gif_data == NULL || gif_len == 0 || frame_indices == NULL ||
        out_frames == NULL || n_indices == 0 || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (max_frames <= 0 || max_total_pixels == 0 || max_frame_output_size == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int deadline_status = fluxer_native_deadline_status(
        deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    int expected_frames = 0;
    int rc = fluxer_validate_complete_gif(
        gif_data, gif_len, max_frames, max_total_pixels, &expected_frames);
    if (rc != 0) return rc;
    deadline_status = fluxer_native_deadline_status(
        deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    return ff_extract_selected_animation_frames(
        gif_data, gif_len, decoder_threads, deadline_monotonic_ms,
        "gif", frame_indices, n_indices,
        expected_frames, max_frame_output_size, out_frames);
}

static int fluxer_nsfw_uses_byte_seek(const AVFormatContext *format) {
    if (format == NULL || format->iformat == NULL || format->iformat->name == NULL) return 0;
    const char *name = format->iformat->name;
    return strcmp(name, "mpegts") == 0 ||
           strcmp(name, "mpeg") == 0 ||
           strcmp(name, "mpegvideo") == 0;
}

static int fluxer_nsfw_seek(
    AVFormatContext *format,
    int stream_index,
    int64_t target_pts,
    double target_seconds,
    size_t media_len,
    long long deadline_monotonic_ms
) {
    int deadline_status = fluxer_native_deadline_status(
        deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    if (fluxer_nsfw_uses_byte_seek(format) &&
        format->duration > 0 && media_len > 0) {
        double duration_seconds =
            (double)format->duration / (double)AV_TIME_BASE;
        if (isfinite(duration_seconds) && duration_seconds > 0.0) {
            double ratio = fmax(
                0.0, fmin(1.0, target_seconds / duration_seconds));
            long double projected_offset =
                floorl((long double)media_len * (long double)ratio);
            size_t offset = media_len;
            if (projected_offset < (long double)media_len) {
                offset = (size_t)projected_offset;
            }
            if (offset > FLUXER_NSFW_BYTE_SEEK_REWIND) {
                offset -= FLUXER_NSFW_BYTE_SEEK_REWIND;
            } else {
                offset = 0;
            }
            int rc = av_seek_frame(
                format, -1, (int64_t)offset, AVSEEK_FLAG_BYTE | AVSEEK_FLAG_BACKWARD);
            deadline_status = fluxer_native_deadline_status(
                deadline_monotonic_ms);
            if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
                return deadline_status;
            }
            if (rc >= 0) return FLUXER_NATIVE_STATUS_OK;
        }
    }
    int rc = av_seek_frame(format, stream_index, target_pts, AVSEEK_FLAG_BACKWARD);
    deadline_status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    if (rc >= 0) return FLUXER_NATIVE_STATUS_OK;
    rc = avformat_seek_file(
        format, stream_index, INT64_MIN, target_pts, target_pts, AVSEEK_FLAG_BACKWARD);
    if (rc >= 0) return fluxer_native_deadline_status(deadline_monotonic_ms);
    return fluxer_native_status_from_av_error_with_deadline(
        rc, deadline_monotonic_ms);
}

struct fluxer_nsfw_target {
    double seconds;
    int64_t pts;
    int64_t keyframe_position;
    int64_t keyframe_pts;
    int has_indexed_keyframe;
};

static int fluxer_nsfw_target_pts(
    AVStream *stream,
    double seconds,
    int64_t *out_pts
) {
    assert(stream != NULL);
    assert(out_pts != NULL);
    int64_t target_pts = 0;
    if (fluxer_seconds_to_pts(seconds, stream->time_base, &target_pts) != 0) return -1;
    if (stream->start_time == AV_NOPTS_VALUE) {
        *out_pts = target_pts;
        return 0;
    }
    if (stream->start_time > 0 && target_pts > INT64_MAX - stream->start_time) {
        return -1;
    }
    if (stream->start_time < 0 && target_pts < INT64_MIN - stream->start_time) {
        return -1;
    }
    *out_pts = target_pts + stream->start_time;
    return 0;
}

static int fluxer_nsfw_targets_init(
    AVFormatContext *format,
    AVStream *stream,
    const double *timestamps,
    size_t count,
    long long deadline_monotonic_ms,
    struct fluxer_nsfw_target *targets
) {
    assert(stream != NULL);
    assert(timestamps != NULL);
    assert(targets != NULL);
    assert(count <= FLUXER_MAX_NSFW_SAMPLES);
    int indexed_seek = !fluxer_nsfw_uses_byte_seek(format);
    for (size_t i = 0; i < count; i++) {
        int deadline_status = fluxer_native_deadline_status(
            deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        double seconds = timestamps[i];
        assert(isfinite(seconds));
        assert(seconds >= 0.0);
        targets[i].seconds = seconds;
        if (fluxer_nsfw_target_pts(stream, seconds, &targets[i].pts) != 0) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        targets[i].keyframe_position = -1;
        targets[i].keyframe_pts = AV_NOPTS_VALUE;
        targets[i].has_indexed_keyframe = 0;
        if (!indexed_seek) continue;
        const AVIndexEntry *entry = avformat_index_get_entry_from_timestamp(
            stream, targets[i].pts, AVSEEK_FLAG_BACKWARD);
        if (entry == NULL || entry->pos < 0 || (entry->flags & AVINDEX_KEYFRAME) == 0) continue;
        targets[i].keyframe_position = entry->pos;
        targets[i].keyframe_pts = entry->timestamp;
        targets[i].has_indexed_keyframe = 1;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static size_t fluxer_nsfw_group_limit(
    const struct fluxer_nsfw_target *targets,
    size_t count,
    size_t first
) {
    assert(targets != NULL);
    assert(first < count);
    assert(count <= FLUXER_MAX_NSFW_SAMPLES);
    const struct fluxer_nsfw_target *origin = &targets[first];
    size_t limit = first + 1;
    while (limit < count) {
        const struct fluxer_nsfw_target *previous = &targets[limit - 1];
        const struct fluxer_nsfw_target *next = &targets[limit];
        if (!origin->has_indexed_keyframe || !next->has_indexed_keyframe) break;
        if (next->pts < previous->pts) break;
        if (next->keyframe_position != origin->keyframe_position) break;
        if (next->keyframe_pts != origin->keyframe_pts) break;
        limit++;
    }
    return limit;
}

static int fluxer_nsfw_decoder_open(ff_thumbnail_context *context, int decoder_threads) {
    assert(context != NULL);
    assert(fluxer_ffmpeg_decoder_threads_valid(decoder_threads));
    int status = fluxer_native_deadline_status(
        context->deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    const AVCodec *decoder = NULL;
    context->stream_index = ff_find_primary_video_stream(context->format, &decoder);
    if (context->stream_index < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    context->stream = context->format->streams[context->stream_index];
    if (context->stream == NULL || context->stream->codecpar == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int width = context->stream->codecpar->width;
    int height = context->stream->codecpar->height;
    if (width <= 0 || height <= 0 ||
        width > FLUXER_MAX_VIDEO_FRAME_DIMENSION ||
        height > FLUXER_MAX_VIDEO_FRAME_DIMENSION ||
        (size_t)width > FLUXER_MAX_VIDEO_PIXELS / (size_t)height) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (decoder == NULL) decoder = avcodec_find_decoder(context->stream->codecpar->codec_id);
    if (!fluxer_video_decoder_allowed(context->stream, decoder)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    context->decoder = avcodec_alloc_context3(decoder);
    if (context->decoder == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    int parameters_rc = avcodec_parameters_to_context(
        context->decoder, context->stream->codecpar);
    if (parameters_rc < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            parameters_rc, context->deadline_monotonic_ms);
    }
    context->decoder->max_pixels = (int64_t)FLUXER_MAX_VIDEO_PIXELS;
    context->decoder->thread_count = decoder_threads;
    int open_rc = avcodec_open2(context->decoder, decoder, NULL);
    if (open_rc < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            open_rc, context->deadline_monotonic_ms);
    }
    status = fluxer_native_deadline_status(context->deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    context->packet = av_packet_alloc();
    context->frame = av_frame_alloc();
    if (context->packet == NULL || context->frame == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

enum fluxer_nsfw_receive_result {
    FLUXER_NSFW_RECEIVE_MORE = 100,
    FLUXER_NSFW_RECEIVE_EOF = 101,
    FLUXER_NSFW_RECEIVE_COMPLETE = 102,
    FLUXER_NSFW_RECEIVE_RESEEK = 103
};

struct fluxer_nsfw_decode_group {
    ff_thumbnail_context *context;
    AVFrame *candidate;
    const struct fluxer_nsfw_target *targets;
    struct fluxer_nsfw_frame_out *outputs;
    size_t next;
    size_t limit;
    int packets;
    int decoded_frames;
    size_t max_frame_output_size;
};

static int fluxer_nsfw_copy_output(
    const struct fluxer_nsfw_frame_out *source,
    long long deadline_monotonic_ms,
    struct fluxer_nsfw_frame_out *destination
) {
    assert(source != NULL);
    assert(destination != NULL);
    if (source->data == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (source->len == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    void *copy = g_try_malloc(source->len);
    if (copy == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    memcpy(copy, source->data, source->len);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        g_free(copy);
        return status;
    }
    destination->data = copy;
    destination->len = source->len;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_nsfw_emit_frame_range(
    struct fluxer_nsfw_decode_group *group,
    AVFrame *frame,
    size_t first,
    size_t limit
) {
    assert(group != NULL);
    assert(frame != NULL);
    assert(first < limit);
    assert(limit <= group->limit);
    int status = fluxer_emit_nsfw_frame(
        frame, group->context->decoder, group->context->format,
        group->context->stream, group->context->deadline_monotonic_ms,
        group->max_frame_output_size,
        &group->outputs[first]);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    for (size_t index = first + 1; index < limit; index++) {
        status = fluxer_nsfw_copy_output(
            &group->outputs[first], group->context->deadline_monotonic_ms,
            &group->outputs[index]);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static void fluxer_nsfw_keep_candidate(struct fluxer_nsfw_decode_group *group) {
    assert(group != NULL);
    assert(group->context != NULL);
    assert(group->context->frame != NULL);
    assert(group->candidate != NULL);
    av_frame_unref(group->candidate);
    av_frame_move_ref(group->candidate, group->context->frame);
}

static int fluxer_nsfw_consume_frame(struct fluxer_nsfw_decode_group *group) {
    assert(group != NULL);
    assert(group->next < group->limit);
    AVFrame *frame = group->context->frame;
    int64_t frame_pts = frame->best_effort_timestamp;
    if (frame_pts == AV_NOPTS_VALUE) {
        int rc = fluxer_emit_nsfw_frame(
            frame, group->context->decoder, group->context->format,
            group->context->stream, group->context->deadline_monotonic_ms,
            group->max_frame_output_size,
            &group->outputs[group->next]);
        av_frame_unref(frame);
        if (rc != FLUXER_NATIVE_STATUS_OK) return rc;
        group->next++;
        return group->next == group->limit
            ? FLUXER_NSFW_RECEIVE_COMPLETE
            : FLUXER_NSFW_RECEIVE_RESEEK;
    }
    if (frame_pts < group->targets[group->next].pts) {
        fluxer_nsfw_keep_candidate(group);
        return FLUXER_NSFW_RECEIVE_MORE;
    }
    size_t first = group->next;
    size_t limit = first;
    while (limit < group->limit && frame_pts >= group->targets[limit].pts) {
        limit++;
    }
    int rc = fluxer_nsfw_emit_frame_range(group, frame, first, limit);
    if (rc != FLUXER_NATIVE_STATUS_OK) {
        av_frame_unref(frame);
        return rc;
    }
    group->next = limit;
    if (group->next == group->limit) {
        av_frame_unref(frame);
        return FLUXER_NSFW_RECEIVE_COMPLETE;
    }
    fluxer_nsfw_keep_candidate(group);
    return FLUXER_NSFW_RECEIVE_MORE;
}

static int fluxer_nsfw_receive_frames(struct fluxer_nsfw_decode_group *group) {
    assert(group != NULL);
    while (1) {
        int deadline_status = fluxer_native_deadline_status(
            group->context->deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        int rc = avcodec_receive_frame(group->context->decoder, group->context->frame);
        if (rc == AVERROR(EAGAIN)) return FLUXER_NSFW_RECEIVE_MORE;
        if (rc == AVERROR_EOF) return FLUXER_NSFW_RECEIVE_EOF;
        if (rc < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                rc, group->context->deadline_monotonic_ms);
        }
        group->decoded_frames++;
        if (group->decoded_frames > FLUXER_MAX_VIDEO_FRAMES_FOR_NSFW_PER_SAMPLE) {
            av_frame_unref(group->context->frame);
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        int consume_rc = fluxer_nsfw_consume_frame(group);
        if (consume_rc != FLUXER_NSFW_RECEIVE_MORE) return consume_rc;
    }
}

static int fluxer_nsfw_emit_candidate(struct fluxer_nsfw_decode_group *group) {
    assert(group != NULL);
    if (group->candidate->data[0] == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    size_t first = group->next;
    int status = fluxer_nsfw_emit_frame_range(
        group, group->candidate, first, group->limit);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    group->next = group->limit;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_nsfw_decode_group(struct fluxer_nsfw_decode_group *group) {
    assert(group != NULL);
    assert(group->next < group->limit);
    int read_rc = 0;
    while (group->packets < FLUXER_MAX_VIDEO_PACKETS_FOR_NSFW) {
        int deadline_status = fluxer_native_deadline_status(
            group->context->deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        read_rc = av_read_frame(group->context->format, group->context->packet);
        if (read_rc < 0) break;
        group->packets++;
        if (group->context->packet->stream_index != group->context->stream_index) {
            av_packet_unref(group->context->packet);
            continue;
        }
        int send_rc = avcodec_send_packet(group->context->decoder, group->context->packet);
        av_packet_unref(group->context->packet);
        if (send_rc < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                send_rc, group->context->deadline_monotonic_ms);
        }
        int receive_rc = fluxer_nsfw_receive_frames(group);
        if (receive_rc == FLUXER_NSFW_RECEIVE_EOF) {
            return fluxer_nsfw_emit_candidate(group);
        }
        if (receive_rc == FLUXER_NSFW_RECEIVE_COMPLETE ||
            receive_rc == FLUXER_NSFW_RECEIVE_RESEEK) {
            return FLUXER_NATIVE_STATUS_OK;
        }
        if (receive_rc != FLUXER_NSFW_RECEIVE_MORE) return receive_rc;
    }
    if (group->packets == FLUXER_MAX_VIDEO_PACKETS_FOR_NSFW && read_rc >= 0) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (read_rc < 0 && read_rc != AVERROR_EOF) {
        return fluxer_native_status_from_av_error_with_deadline(
            read_rc, group->context->deadline_monotonic_ms);
    }
    int send_rc = avcodec_send_packet(group->context->decoder, NULL);
    if (send_rc < 0 && send_rc != AVERROR_EOF) {
        return fluxer_native_status_from_av_error_with_deadline(
            send_rc, group->context->deadline_monotonic_ms);
    }
    int receive_rc = fluxer_nsfw_receive_frames(group);
    if (receive_rc == FLUXER_NSFW_RECEIVE_COMPLETE ||
        receive_rc == FLUXER_NSFW_RECEIVE_RESEEK) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    if (receive_rc == FLUXER_NSFW_RECEIVE_EOF) {
        return fluxer_nsfw_emit_candidate(group);
    }
    if (receive_rc == FLUXER_NSFW_RECEIVE_MORE) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return receive_rc;
}

static int fluxer_av_nsfw_request_valid(
    const void *media_data,
    size_t media_len,
    int decoder_threads,
    const double *timestamps,
    size_t timestamp_count,
    long long deadline_monotonic_ms,
    size_t max_frame_output_size,
    struct fluxer_nsfw_frame_out *outputs
) {
    if (media_data == NULL) return 0;
    if (media_len == 0) return 0;
    if (timestamps == NULL) return 0;
    if (outputs == NULL) return 0;
    if (timestamp_count == 0) return 0;
    if (timestamp_count > FLUXER_MAX_NSFW_SAMPLES) return 0;
    if (deadline_monotonic_ms < 0) return 0;
    if (max_frame_output_size == 0) return 0;
    if (!fluxer_ffmpeg_decoder_threads_valid(decoder_threads)) return 0;
    for (size_t index = 0; index < timestamp_count; index++) {
        if (!isfinite(timestamps[index])) return 0;
        if (timestamps[index] < 0.0) return 0;
    }
    return 1;
}

static int fluxer_nsfw_sample_failure_fatal(int status) {
    return status == FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED ||
           status == FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
}

static size_t fluxer_nsfw_produced_frames(
    const struct fluxer_nsfw_frame_out *outputs,
    size_t count
) {
    assert(outputs != NULL);
    size_t produced = 0;
    for (size_t index = 0; index < count; index++) {
        if (outputs[index].data != NULL) produced++;
    }
    return produced;
}

static int fluxer_nsfw_decode_targets(
    ff_thumbnail_context *context,
    AVFrame *candidate,
    const struct fluxer_nsfw_target *targets,
    size_t target_count,
    size_t media_len,
    long long deadline_monotonic_ms,
    size_t max_frame_output_size,
    struct fluxer_nsfw_frame_out *outputs
) {
    assert(context != NULL);
    assert(candidate != NULL);
    assert(targets != NULL);
    assert(target_count > 0);
    assert(target_count <= FLUXER_MAX_NSFW_SAMPLES);
    assert(outputs != NULL);
    size_t next = 0;
    int first_failure = FLUXER_NATIVE_STATUS_OK;
    while (next < target_count) {
        int deadline_status = fluxer_native_deadline_status(
            deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        size_t limit = fluxer_nsfw_group_limit(
            targets, target_count, next);
        size_t first = next;
        av_frame_unref(candidate);
        struct fluxer_nsfw_decode_group group = {
            .context = context,
            .candidate = candidate,
            .targets = targets,
            .outputs = outputs,
            .next = next,
            .limit = limit,
            .max_frame_output_size = max_frame_output_size,
        };
        int status = fluxer_nsfw_seek(
            context->format,
            context->stream_index,
            targets[next].pts,
            targets[next].seconds,
            media_len,
            deadline_monotonic_ms);
        if (status == FLUXER_NATIVE_STATUS_OK) {
            avcodec_flush_buffers(context->decoder);
            status = fluxer_native_deadline_status(deadline_monotonic_ms);
        }
        if (status == FLUXER_NATIVE_STATUS_OK) {
            status = fluxer_nsfw_decode_group(&group);
        }
        if (status == FLUXER_NATIVE_STATUS_OK && group.next > first) {
            next = group.next;
            continue;
        }
        if (fluxer_nsfw_sample_failure_fatal(status)) return status;
        /* A sample the decoder cannot reach falls back to the last frame it did
           decode, and otherwise leaves its slot empty, so the samples that did
           decode still reach the classifier. */
        if (group.next < limit && outputs[group.next].data == NULL) {
            (void)fluxer_nsfw_emit_candidate(&group);
        }
        if (first_failure == FLUXER_NATIVE_STATUS_OK) first_failure = status;
        next = limit;
    }
    if (fluxer_nsfw_produced_frames(outputs, target_count) == 0) {
        return first_failure != FLUXER_NATIVE_STATUS_OK
            ? first_failure
            : FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_av_extract_frames_for_nsfw(
    const void *media_data,
    size_t media_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    const double *timestamps_secs,
    size_t n_timestamps,
    size_t max_frame_output_size,
    struct fluxer_nsfw_frame_out *out_frames
) {
    if (n_timestamps > FLUXER_MAX_NSFW_SAMPLES) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    fluxer_nsfw_frames_reset(out_frames, n_timestamps);
    if (!fluxer_av_nsfw_request_valid(
            media_data, media_len, decoder_threads, timestamps_secs,
            n_timestamps, deadline_monotonic_ms,
            max_frame_output_size, out_frames)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }

    ff_thumbnail_context context;
    int status = ff_thumbnail_context_open(
        &context, media_data, media_len, FLUXER_AV_INPUT_FORMATS, 0,
        deadline_monotonic_ms);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_nsfw_decoder_open(&context, decoder_threads);
    }
    AVFrame *candidate = NULL;
    if (status == FLUXER_NATIVE_STATUS_OK) {
        candidate = av_frame_alloc();
        if (candidate == NULL) {
            status = FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
        }
    }
    struct fluxer_nsfw_target targets[FLUXER_MAX_NSFW_SAMPLES];
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_nsfw_targets_init(
            context.format, context.stream, timestamps_secs,
            n_timestamps, deadline_monotonic_ms, targets);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_nsfw_decode_targets(
            &context, candidate, targets, n_timestamps, media_len,
            deadline_monotonic_ms,
            max_frame_output_size, out_frames);
    }

    if (candidate != NULL) av_frame_free(&candidate);
    ff_thumbnail_context_clear(&context);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        fluxer_nsfw_frames_free(out_frames, n_timestamps);
    }
    return status;
}
