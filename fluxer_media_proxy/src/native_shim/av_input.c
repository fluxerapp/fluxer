// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

static int fluxer_monotonic_ms(long long *out_monotonic_ms) {
    if (out_monotonic_ms == NULL) return -1;
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0 || ts.tv_sec < 0 ||
        ts.tv_nsec < 0 || ts.tv_nsec >= 1000000000L ||
        (uintmax_t)ts.tv_sec > (uintmax_t)(LLONG_MAX / 1000)) return -1;
    long long seconds_ms = (long long)ts.tv_sec * 1000;
    long long nanoseconds_ms = ts.tv_nsec / 1000000;
    if (seconds_ms > LLONG_MAX - nanoseconds_ms) return -1;
    *out_monotonic_ms = seconds_ms + nanoseconds_ms;
    return 0;
}

int fluxer_monotonic_deadline_status(long long deadline_monotonic_ms) {
    assert(deadline_monotonic_ms >= 0);
    if (deadline_monotonic_ms == 0) return FLUXER_DEADLINE_PENDING;
    long long now_monotonic_ms = 0;
    if (fluxer_monotonic_ms(&now_monotonic_ms) != 0) {
        return FLUXER_DEADLINE_CLOCK_FAILED;
    }
    return now_monotonic_ms >= deadline_monotonic_ms
        ? FLUXER_DEADLINE_REACHED
        : FLUXER_DEADLINE_PENDING;
}

int fluxer_native_deadline_status(long long deadline_monotonic_ms) {
    assert(deadline_monotonic_ms >= 0);
    return fluxer_monotonic_deadline_status(deadline_monotonic_ms) ==
                   FLUXER_DEADLINE_PENDING
        ? FLUXER_NATIVE_STATUS_OK
        : FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
}

int fluxer_ffmpeg_interrupt_deadline(void *opaque) {
    if (opaque == NULL) return 1;
    const long long *deadline_monotonic_ms = opaque;
    return fluxer_native_deadline_status(*deadline_monotonic_ms) !=
           FLUXER_NATIVE_STATUS_OK;
}

int fluxer_ffmpeg_decoder_threads_valid(int decoder_threads) {
    return decoder_threads >= 1 && decoder_threads <= FLUXER_MAX_THREADS_PER_PIPELINE;
}

int fluxer_native_status_from_av_error(int error) {
    if (error == AVERROR(ENOMEM)) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    if (error == AVERROR(ENOSPC)) return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    if (error == AVERROR(E2BIG)) return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
}

int fluxer_native_status_from_av_error_with_deadline(
    int error,
    long long deadline_monotonic_ms
) {
    int deadline_status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    return fluxer_native_status_from_av_error(error);
}

int fluxer_native_status_from_heif_error(struct heif_error error) {
    if (error.subcode == heif_suberror_Security_limit_exceeded) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (error.code == heif_error_Memory_allocation_error) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (error.code == heif_error_Unsupported_filetype ||
        error.code == heif_error_Unsupported_feature) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
}

int ff_mem_read_packet(void *opaque, uint8_t *buf, int buf_size) {
    struct ff_mem_reader *r = (struct ff_mem_reader *)opaque;
    if (r == NULL || r->data == NULL || buf == NULL || buf_size < 0) {
        return AVERROR(EINVAL);
    }
    if (buf_size == 0) return 0;
    if (r->offset > r->len) return AVERROR(EINVAL);
    if (r->offset == r->len) return AVERROR_EOF;
    size_t remaining = r->len - r->offset;
    size_t n = remaining < (size_t)buf_size ? remaining : (size_t)buf_size;
    memcpy(buf, r->data + r->offset, n);
    r->offset += n;
    return (int)n;
}

int64_t ff_mem_seek(void *opaque, int64_t offset, int whence) {
    struct ff_mem_reader *r = (struct ff_mem_reader *)opaque;
    if (r == NULL || r->len > (size_t)INT64_MAX) return AVERROR(EINVAL);
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
    if (offset < -base || offset > INT64_MAX - base) return AVERROR(EINVAL);
    int64_t next = base + offset;
    if (next < 0 || (uint64_t)next > r->len) return AVERROR(EINVAL);
    r->offset = (size_t)next;
    return next;
}

static int fluxer_video_codec_id_allowed(enum AVCodecID id) {
    switch (id) {
        case AV_CODEC_ID_GIF:
        case AV_CODEC_ID_APNG:
        case AV_CODEC_ID_H264:
        case AV_CODEC_ID_HEVC:
        case AV_CODEC_ID_VP8:
        case AV_CODEC_ID_VP9:
        case AV_CODEC_ID_AV1:
        case AV_CODEC_ID_MPEG1VIDEO:
        case AV_CODEC_ID_MPEG2VIDEO:
        case AV_CODEC_ID_MPEG4:
        case AV_CODEC_ID_H263:
        case AV_CODEC_ID_H263P:
        case AV_CODEC_ID_H263I:
        case AV_CODEC_ID_THEORA:
        case AV_CODEC_ID_FLV1:
        case AV_CODEC_ID_VP6:
        case AV_CODEC_ID_VP6F:
        case AV_CODEC_ID_VP6A:
        case AV_CODEC_ID_WMV1:
        case AV_CODEC_ID_WMV2:
        case AV_CODEC_ID_WMV3:
        case AV_CODEC_ID_VC1:
        case AV_CODEC_ID_MJPEG:
        case AV_CODEC_ID_BMP:
            return 1;
        default:
            return 0;
    }
}

int fluxer_prepare_untrusted_av_input(AVFormatContext *format) {
    if (format == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (format->nb_streams > FLUXER_MAX_AV_STREAMS) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (format->nb_streams > 0 && format->streams == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    for (unsigned int i = 0; i < format->nb_streams; i++) {
        AVStream *stream = format->streams[i];
        if (stream == NULL || stream->codecpar == NULL) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        enum AVMediaType type = stream->codecpar->codec_type;
        enum AVCodecID id = stream->codecpar->codec_id;
        if (type == AVMEDIA_TYPE_VIDEO && !ff_stream_is_attached_picture(stream)) {
            if (id != AV_CODEC_ID_NONE && !fluxer_video_codec_id_allowed(id)) {
                return FLUXER_NATIVE_STATUS_UNSUPPORTED;
            }
            int width = stream->codecpar->width;
            int height = stream->codecpar->height;
            if (width < 0 || height < 0 ||
                (width > 0 && height > 0 &&
                 (width > FLUXER_MAX_VIDEO_FRAME_DIMENSION ||
                  height > FLUXER_MAX_VIDEO_FRAME_DIMENSION ||
                  (size_t)width > FLUXER_MAX_VIDEO_PIXELS / (size_t)height))) {
                return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
            }
        } else {
            stream->discard = AVDISCARD_ALL;
        }
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_deny_child_av_io(AVFormatContext *format, AVIOContext **io,
                                   const char *url, int flags,
                                   AVDictionary **options) {
    (void)format;
    (void)io;
    (void)url;
    (void)flags;
    (void)options;
    return AVERROR(EACCES);
}

int fluxer_restrict_untrusted_av_context(AVFormatContext *format,
                                        const char *format_whitelist) {
    if (format == NULL || format_whitelist == NULL || format_whitelist[0] == '\0') {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    format->io_open = fluxer_deny_child_av_io;
    format->format_whitelist = av_strdup(format_whitelist);
    format->protocol_whitelist = av_strdup("");
    format->codec_whitelist = av_strdup(FLUXER_ALLOWED_VIDEO_DECODERS);
    format->max_streams = FLUXER_MAX_AV_STREAMS;
    if (format->format_whitelist == NULL ||
        format->protocol_whitelist == NULL ||
        format->codec_whitelist == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_find_stream_info_bounded(AVFormatContext *format) {
    if (format == NULL) return AVERROR(EINVAL);
    unsigned int stream_count = format->nb_streams;
    if (stream_count > FLUXER_MAX_AV_STREAMS) return AVERROR(E2BIG);
    if (stream_count == 0 || format->streams == NULL) return AVERROR_INVALIDDATA;
    format->max_streams = stream_count;

    AVDictionary **options = av_calloc(stream_count, sizeof(*options));
    if (options == NULL) return AVERROR(ENOMEM);
    int rc = 0;
    for (unsigned int i = 0; i < stream_count; i++) {
        const AVStream *stream = format->streams[i];
        if (stream == NULL || stream->codecpar == NULL) continue;
        if (av_dict_set_int(&options[i], "max_pixels",
                            (int64_t)FLUXER_MAX_VIDEO_PIXELS, 0) < 0 ||
            av_dict_set(&options[i], "threads", "1", 0) < 0) {
            rc = AVERROR(ENOMEM);
            break;
        }
        if (stream->codecpar->codec_type == AVMEDIA_TYPE_VIDEO &&
            av_dict_set(&options[i], "codec_whitelist",
                        FLUXER_ALLOWED_VIDEO_DECODERS, 0) < 0) {
            rc = AVERROR(ENOMEM);
            break;
        }
    }
    if (rc == 0) rc = avformat_find_stream_info(format, options);
    for (unsigned int i = 0; i < stream_count; i++) av_dict_free(&options[i]);
    av_free(options);
    return rc;
}

int fluxer_video_decoder_allowed(const AVStream *stream, const AVCodec *decoder) {
    if (stream == NULL || stream->codecpar == NULL || decoder == NULL ||
        stream->codecpar->codec_id != decoder->id) {
        return 0;
    }
    return fluxer_video_codec_id_allowed(decoder->id);
}

int ff_validate_rgba_geometry(int width, int height, size_t *out_size) {
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

int ff_stream_is_attached_picture(const AVStream *stream) {
    if (stream == NULL) return 0;
    int attached = 0;
#ifdef AV_DISPOSITION_ATTACHED_PIC
    if ((stream->disposition & AV_DISPOSITION_ATTACHED_PIC) != 0) attached = 1;
#endif
#ifdef AV_DISPOSITION_TIMED_THUMBNAILS
    if ((stream->disposition & AV_DISPOSITION_TIMED_THUMBNAILS) != 0) attached = 1;
#endif
#ifdef AV_DISPOSITION_STILL_IMAGE
    if ((stream->disposition & AV_DISPOSITION_STILL_IMAGE) != 0) attached = 1;
#endif
    return attached;
}

int ff_find_primary_video_stream(
    AVFormatContext *format,
    const AVCodec **out_codec
) {
    if (format == NULL || out_codec == NULL) return -1;
    *out_codec = NULL;
    int best = av_find_best_stream(
        format, AVMEDIA_TYPE_VIDEO, -1, -1, out_codec, 0);
    AVStream *best_stream = best >= 0 ? format->streams[best] : NULL;
    if (best_stream != NULL && !ff_stream_is_attached_picture(best_stream) &&
        best_stream->nb_frames != 1) return best;
    int fallback = -1;
    const AVCodec *fallback_codec = NULL;
    if (best_stream != NULL && !ff_stream_is_attached_picture(best_stream)) {
        fallback = best;
        fallback_codec = *out_codec;
    }
    for (unsigned int i = 0; i < format->nb_streams; i++) {
        AVStream *stream = format->streams[i];
        if (stream == NULL || stream->codecpar == NULL) continue;
        if (stream->codecpar->codec_type != AVMEDIA_TYPE_VIDEO) continue;
        if (ff_stream_is_attached_picture(stream)) continue;
        const AVCodec *codec = avcodec_find_decoder(stream->codecpar->codec_id);
        if (!fluxer_video_decoder_allowed(stream, codec)) continue;
        if (stream->nb_frames > 1) {
            *out_codec = codec;
            return (int)i;
        }
        if (fallback < 0) {
            fallback = (int)i;
            fallback_codec = codec;
        }
    }
    *out_codec = fallback_codec;
    return fallback;
}
