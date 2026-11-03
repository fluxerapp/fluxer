// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

#define FLUXER_AV_STRIP_AVIO_BUFFER_SIZE (64u * 1024u)
#define FLUXER_AV_STRIP_MAX_MUX_DELAY (7 * AV_TIME_BASE / 10)

typedef struct {
    struct ff_mem_reader reader;
    AVIOContext *avio;
    AVFormatContext *format;
    long long deadline_monotonic_ms;
} fluxer_av_strip_input;

static void fluxer_av_strip_input_close(fluxer_av_strip_input *input) {
    if (input->format != NULL) avformat_close_input(&input->format);
    if (input->avio != NULL) {
        if (input->avio->buffer != NULL) av_freep(&input->avio->buffer);
        avio_context_free(&input->avio);
    }
}

static int fluxer_av_strip_input_open(
    fluxer_av_strip_input *input,
    const void *media_data,
    size_t media_len,
    const char *allowed_formats,
    long long deadline_monotonic_ms
) {
    memset(input, 0, sizeof(*input));
    if (media_data == NULL || media_len == 0 || allowed_formats == NULL ||
        deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (media_len > (size_t)INT64_MAX) return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    input->deadline_monotonic_ms = deadline_monotonic_ms;
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    input->reader.data = (const uint8_t *)media_data;
    input->reader.len = media_len;
    unsigned char *buffer = av_malloc(FLUXER_AV_STRIP_AVIO_BUFFER_SIZE);
    if (buffer == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    input->avio = avio_alloc_context(
        buffer, FLUXER_AV_STRIP_AVIO_BUFFER_SIZE, 0, &input->reader,
        ff_mem_read_packet, NULL, ff_mem_seek);
    if (input->avio == NULL) {
        av_free(buffer);
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    input->format = avformat_alloc_context();
    if (input->format == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    status = fluxer_restrict_untrusted_av_context(input->format, allowed_formats);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    av_freep(&input->format->codec_whitelist);
    input->format->codec_whitelist =
        av_strdup(FLUXER_ALLOWED_VIDEO_DECODERS "," FLUXER_ALLOWED_AUDIO_DECODERS);
    if (input->format->codec_whitelist == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    input->format->pb = input->avio;
    input->format->interrupt_callback.callback = fluxer_ffmpeg_interrupt_deadline;
    input->format->interrupt_callback.opaque = &input->deadline_monotonic_ms;
    input->format->flags |= AVFMT_FLAG_CUSTOM_IO;
    input->format->probesize = 5 * 1024 * 1024;
    input->format->max_analyze_duration = 5 * AV_TIME_BASE;
    int rc = avformat_open_input(&input->format, NULL, NULL, NULL);
    if (rc < 0) {
        return fluxer_native_status_from_av_error_with_deadline(rc, deadline_monotonic_ms);
    }
    rc = input->format->nb_streams > 0
        ? fluxer_find_stream_info_bounded(input->format)
        : avformat_find_stream_info(input->format, NULL);
    if (rc < 0) {
        return fluxer_native_status_from_av_error_with_deadline(rc, deadline_monotonic_ms);
    }
    if (input->format->nb_streams == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (input->format->nb_streams > FLUXER_MAX_AV_STREAMS) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    input->format->max_streams = (int)input->format->nb_streams;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_av_strip_copy_stream(AVFormatContext *output, const AVStream *source) {
    AVStream *stream = avformat_new_stream(output, NULL);
    if (stream == NULL) return AVERROR(ENOMEM);
    int rc = avcodec_parameters_copy(stream->codecpar, source->codecpar);
    if (rc < 0) return rc;
    AVCodecParameters *par = stream->codecpar;
    const struct AVCodecTag *const *tags = output->oformat->codec_tag;
    unsigned int tag = 0;
    if (tags != NULL && av_codec_get_id(tags, par->codec_tag) != par->codec_id &&
        av_codec_get_tag2(tags, par->codec_id, &tag)) {
        par->codec_tag = 0;
    }
    if (par->codec_type == AVMEDIA_TYPE_AUDIO) {
        if (par->codec_id == AV_CODEC_ID_MP3 &&
            (par->block_align == 1 || par->block_align == 1152 || par->block_align == 576)) {
            par->block_align = 0;
        }
        if (par->codec_id == AV_CODEC_ID_AC3) par->block_align = 0;
    }
    if (par->codec_type == AVMEDIA_TYPE_VIDEO) {
        if (source->sample_aspect_ratio.num > 0) {
            par->sample_aspect_ratio = source->sample_aspect_ratio;
        }
        stream->sample_aspect_ratio = par->sample_aspect_ratio;
        stream->avg_frame_rate = source->avg_frame_rate;
        stream->r_frame_rate = source->r_frame_rate;
    }
    stream->time_base = source->time_base;
    stream->disposition = source->disposition;
    return 0;
}

static int fluxer_av_strip_map_streams(AVFormatContext *input, AVFormatContext *output, int *map) {
    static const enum AVMediaType order[] = {
        AVMEDIA_TYPE_VIDEO, AVMEDIA_TYPE_AUDIO, AVMEDIA_TYPE_SUBTITLE,
    };
    for (unsigned int i = 0; i < input->nb_streams; i++) map[i] = -1;
    for (size_t t = 0; t < sizeof(order) / sizeof(order[0]); t++) {
        for (unsigned int i = 0; i < input->nb_streams; i++) {
            const AVStream *stream = input->streams[i];
            if (stream == NULL || stream->codecpar == NULL ||
                stream->codecpar->codec_type != order[t]) {
                continue;
            }
            int rc = fluxer_av_strip_copy_stream(output, stream);
            if (rc < 0) return rc;
            map[i] = (int)output->nb_streams - 1;
        }
    }
    return output->nb_streams > 0 ? 0 : AVERROR_STREAM_NOT_FOUND;
}

typedef struct {
    int64_t dts;
    int64_t next_dts;
    int64_t last_mux_dts;
    int saw_first_ts;
    int started;
} fluxer_av_strip_track;

static int64_t fluxer_av_strip_estimate_dts(
    const AVPacket *packet,
    const AVStream *stream,
    fluxer_av_strip_track *track
) {
    const AVCodecParameters *par = stream->codecpar;
    AVRational frame_rate = stream->avg_frame_rate;
    if (!track->saw_first_ts) {
        track->dts = frame_rate.num > 0 && frame_rate.den > 0
            ? -(int64_t)par->video_delay * AV_TIME_BASE * frame_rate.den / frame_rate.num
            : 0;
        if (packet->pts != AV_NOPTS_VALUE) {
            track->dts += av_rescale_q(packet->pts, stream->time_base, AV_TIME_BASE_Q);
        }
        track->next_dts = track->dts;
        track->saw_first_ts = 1;
    }
    if (packet->dts != AV_NOPTS_VALUE) {
        track->next_dts = av_rescale_q(packet->dts, stream->time_base, AV_TIME_BASE_Q);
    }
    track->dts = track->next_dts;
    if (packet->duration > 0) {
        track->next_dts += av_rescale_q(packet->duration, stream->time_base, AV_TIME_BASE_Q);
    } else if (par->codec_type == AVMEDIA_TYPE_AUDIO && par->sample_rate > 0 && par->frame_size > 0) {
        track->next_dts += (int64_t)AV_TIME_BASE * par->frame_size / par->sample_rate;
    } else if (par->codec_type == AVMEDIA_TYPE_VIDEO && frame_rate.num > 0 && frame_rate.den > 0) {
        track->next_dts += (int64_t)AV_TIME_BASE * frame_rate.den / frame_rate.num;
    }
    return track->dts;
}

static void fluxer_av_strip_retime(
    AVPacket *packet,
    const AVStream *source,
    const AVStream *destination,
    int64_t start_time,
    fluxer_av_strip_track *track,
    int strict_monotonic
) {
    int64_t offset = start_time == AV_NOPTS_VALUE
        ? 0
        : av_rescale_q(start_time, AV_TIME_BASE_Q, source->time_base);
    if (packet->pts != AV_NOPTS_VALUE) packet->pts -= offset;
    if (packet->dts != AV_NOPTS_VALUE) packet->dts -= offset;
    int64_t estimated_dts = fluxer_av_strip_estimate_dts(packet, source, track);
    if (packet->dts == AV_NOPTS_VALUE) {
        packet->dts = av_rescale_q(estimated_dts, AV_TIME_BASE_Q, source->time_base);
    }
    av_packet_rescale_ts(packet, source->time_base, destination->time_base);
    int64_t last = track->last_mux_dts;
    if (packet->pts != AV_NOPTS_VALUE && packet->dts > packet->pts) {
        int64_t next = last == AV_NOPTS_VALUE ? packet->dts : last + 1;
        packet->pts = packet->dts = packet->pts + packet->dts + next -
            FFMIN3(packet->pts, packet->dts, next) - FFMAX3(packet->pts, packet->dts, next);
    }
    if (last != AV_NOPTS_VALUE) {
        int64_t minimum = last + strict_monotonic;
        if (packet->dts < minimum) {
            if (packet->pts != AV_NOPTS_VALUE && packet->pts >= packet->dts) {
                packet->pts = FFMAX(packet->pts, minimum);
            }
            packet->dts = minimum;
        }
    }
    track->last_mux_dts = packet->dts;
}

static int fluxer_av_strip_remux(
    fluxer_av_strip_input *input,
    AVFormatContext *output,
    int faststart
) {
    AVFormatContext *source = input->format;
    unsigned int mapped_streams = source->nb_streams;
    int *map = av_calloc(mapped_streams, sizeof(*map));
    fluxer_av_strip_track *tracks = av_calloc(mapped_streams, sizeof(*tracks));
    AVPacket *packet = av_packet_alloc();
    AVDictionary *options = NULL;
    int rc = map == NULL || tracks == NULL || packet == NULL ? AVERROR(ENOMEM) : 0;
    if (rc == 0) rc = fluxer_av_strip_map_streams(source, output, map);
    if (rc == 0 && faststart) rc = av_dict_set(&options, "movflags", "+faststart", 0);
    if (rc == 0) rc = avio_open2(&output->pb, output->url, AVIO_FLAG_WRITE,
                                 &output->interrupt_callback, NULL);
    if (rc == 0) rc = avformat_write_header(output, &options);
    int header_written = rc >= 0;
    if (rc > 0) rc = 0;
    for (unsigned int i = 0; rc == 0 && i < mapped_streams; i++) {
        tracks[i].last_mux_dts = AV_NOPTS_VALUE;
    }
    int strict_monotonic = !(output->oformat->flags & AVFMT_TS_NONSTRICT);
    while (rc == 0) {
        int read_rc = av_read_frame(source, packet);
        if (read_rc == AVERROR_EOF) break;
        if (read_rc < 0) {
            rc = read_rc;
            break;
        }
        int index = packet->stream_index;
        if (index < 0 || (unsigned int)index >= mapped_streams || map[index] < 0) {
            av_packet_unref(packet);
            continue;
        }
        fluxer_av_strip_track *track = &tracks[index];
        if (!track->started && !(packet->flags & AV_PKT_FLAG_KEY)) {
            av_packet_unref(packet);
            continue;
        }
        track->started = 1;
        const AVStream *destination = output->streams[map[index]];
        fluxer_av_strip_retime(packet, source->streams[index], destination,
                               source->start_time, track, strict_monotonic);
        packet->stream_index = map[index];
        packet->pos = -1;
        rc = av_interleaved_write_frame(output, packet);
        av_packet_unref(packet);
    }
    if (header_written) {
        int trailer_rc = av_write_trailer(output);
        if (rc == 0) rc = trailer_rc;
    }
    if (output->pb != NULL) {
        int close_rc = avio_closep(&output->pb);
        if (rc == 0) rc = close_rc;
    }
    av_dict_free(&options);
    av_packet_free(&packet);
    av_free(tracks);
    av_free(map);
    return rc;
}

int fluxer_av_strip_metadata(
    const void *media_data,
    size_t media_len,
    const char *demuxer,
    const char *muxer,
    int faststart,
    const char *output_path,
    long long deadline_monotonic_ms
) {
    if (demuxer == NULL || muxer == NULL || output_path == NULL ||
        (faststart != 0 && faststart != 1)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    fluxer_av_strip_input input;
    int status = fluxer_av_strip_input_open(
        &input, media_data, media_len, demuxer, deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        fluxer_av_strip_input_close(&input);
        return status;
    }
    AVFormatContext *output = NULL;
    int rc = avformat_alloc_output_context2(&output, NULL, muxer, output_path);
    if (rc >= 0 && output != NULL) {
        output->interrupt_callback = input.format->interrupt_callback;
        output->max_delay = FLUXER_AV_STRIP_MAX_MUX_DELAY;
        output->protocol_whitelist = av_strdup("file");
        rc = output->protocol_whitelist == NULL
            ? AVERROR(ENOMEM)
            : fluxer_av_strip_remux(&input, output, faststart);
    } else if (rc >= 0) {
        rc = AVERROR(ENOMEM);
    }
    if (output != NULL) avformat_free_context(output);
    fluxer_av_strip_input_close(&input);
    return rc < 0
        ? fluxer_native_status_from_av_error_with_deadline(rc, deadline_monotonic_ms)
        : FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_av_audio_decoder_allowed(const AVCodec *decoder) {
    return decoder != NULL &&
           av_match_list(decoder->name, FLUXER_ALLOWED_AUDIO_DECODERS, ',') > 0;
}

static int fluxer_av_sum_audio_frames(
    fluxer_av_strip_input *input,
    int stream_index,
    double *out_seconds,
    int *out_frames
) {
    const AVStream *stream = input->format->streams[stream_index];
    const AVCodec *decoder = avcodec_find_decoder(stream->codecpar->codec_id);
    if (!fluxer_av_audio_decoder_allowed(decoder)) return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    AVCodecContext *context = avcodec_alloc_context3(decoder);
    AVPacket *packet = av_packet_alloc();
    AVFrame *frame = av_frame_alloc();
    int rc = context == NULL || packet == NULL || frame == NULL ? AVERROR(ENOMEM) : 0;
    if (rc == 0) rc = avcodec_parameters_to_context(context, stream->codecpar);
    if (rc == 0) {
        context->pkt_timebase = stream->time_base;
        context->thread_count = 1;
        rc = avcodec_open2(context, decoder, NULL);
    }
    int64_t total = 0;
    int counted = 0;
    int draining = 0;
    while (rc >= 0) {
        int status = fluxer_native_deadline_status(input->deadline_monotonic_ms);
        if (status != FLUXER_NATIVE_STATUS_OK) {
            avcodec_free_context(&context);
            av_packet_free(&packet);
            av_frame_free(&frame);
            return status;
        }
        if (!draining) {
            int read_rc = av_read_frame(input->format, packet);
            if (read_rc == AVERROR_EOF) {
                draining = 1;
                avcodec_send_packet(context, NULL);
            } else if (read_rc < 0) {
                rc = read_rc;
                break;
            } else {
                if (packet->stream_index == stream_index) avcodec_send_packet(context, packet);
                av_packet_unref(packet);
            }
        }
        for (;;) {
            int receive_rc = avcodec_receive_frame(context, frame);
            if (receive_rc < 0) {
                if (draining || receive_rc == AVERROR_EOF) rc = AVERROR_EOF;
                break;
            }
            if (frame->duration > 0) {
                total += frame->duration;
                counted++;
            }
            av_frame_unref(frame);
        }
    }
    avcodec_free_context(&context);
    av_packet_free(&packet);
    av_frame_free(&frame);
    if (rc < 0 && rc != AVERROR_EOF) {
        return fluxer_native_status_from_av_error_with_deadline(rc, input->deadline_monotonic_ms);
    }
    *out_frames = counted;
    *out_seconds = (double)total * av_q2d(stream->time_base);
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_av_audio_duration(
    const void *media_data,
    size_t media_len,
    long long deadline_monotonic_ms,
    struct fluxer_av_audio_duration_out *out
) {
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    memset(out, 0, sizeof(*out));
    fluxer_av_strip_input input;
    int status = fluxer_av_strip_input_open(
        &input, media_data, media_len, FLUXER_AV_INPUT_FORMATS, deadline_monotonic_ms);
    int stream_index = -1;
    if (status == FLUXER_NATIVE_STATUS_OK) {
        for (unsigned int i = 0; i < input.format->nb_streams; i++) {
            const AVStream *stream = input.format->streams[i];
            if (stream != NULL && stream->codecpar != NULL &&
                stream->codecpar->codec_type == AVMEDIA_TYPE_AUDIO) {
                stream_index = (int)i;
                break;
            }
        }
        if (stream_index < 0) status = FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        const AVStream *stream = input.format->streams[stream_index];
        if (stream->duration != AV_NOPTS_VALUE) {
            out->stream_seconds = (double)stream->duration * av_q2d(stream->time_base);
        }
        if (input.format->duration != AV_NOPTS_VALUE) {
            out->format_seconds = (double)input.format->duration / (double)AV_TIME_BASE;
        }
        status = fluxer_av_sum_audio_frames(
            &input, stream_index, &out->frames_seconds, &out->frame_count);
    }
    fluxer_av_strip_input_close(&input);
    return status;
}
