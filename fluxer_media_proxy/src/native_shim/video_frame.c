// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

static int ff_display_matrix_supported(const int32_t *matrix) {
    assert(matrix != NULL);
    const double unit = 65536.0;
    const double projective_unit = 1073741824.0;
    const double tolerance = 0.001;
    double a = (double)matrix[0] / unit;
    double b = (double)matrix[1] / unit;
    double u = (double)matrix[2] / projective_unit;
    double c = (double)matrix[3] / unit;
    double d = (double)matrix[4] / unit;
    double v = (double)matrix[5] / projective_unit;
    double w = (double)matrix[8] / projective_unit;
    double first_norm = a * a + c * c;
    double second_norm = b * b + d * d;
    double dot = a * b + c * d;
    double determinant = a * d - b * c;
    return isfinite(u) && isfinite(v) && isfinite(w) &&
           fabs(u) <= tolerance && fabs(v) <= tolerance &&
           fabs(w - 1.0) <= tolerance && isfinite(first_norm) &&
           isfinite(second_norm) && isfinite(dot) && isfinite(determinant) &&
           fabs(first_norm - 1.0) <= tolerance &&
           fabs(second_norm - 1.0) <= tolerance && fabs(dot) <= tolerance &&
           fabs(fabs(determinant) - 1.0) <= tolerance;
}

static int ff_display_matrix_angle(
    const AVFrame *frame,
    const AVStream *stream,
    VipsAngle *out_angle,
    int *out_flip_horizontal
) {
    assert(out_angle != NULL);
    assert(out_flip_horizontal != NULL);
    *out_angle = VIPS_ANGLE_D0;
    *out_flip_horizontal = 0;
    int32_t matrix[9];
    int has_matrix = 0;
    const AVFrameSideData *fsd = frame != NULL
        ? av_frame_get_side_data(frame, AV_FRAME_DATA_DISPLAYMATRIX)
        : NULL;
    if (fsd != NULL && fsd->data != NULL && fsd->size >= 9 * sizeof(int32_t)) {
        memcpy(matrix, fsd->data, sizeof(matrix));
        has_matrix = 1;
    } else if (stream != NULL && stream->codecpar != NULL) {
        const AVPacketSideData *psd = av_packet_side_data_get(
            stream->codecpar->coded_side_data, stream->codecpar->nb_coded_side_data,
            AV_PKT_DATA_DISPLAYMATRIX);
        if (psd != NULL && psd->data != NULL && psd->size >= 9 * sizeof(int32_t)) {
            memcpy(matrix, psd->data, sizeof(matrix));
            has_matrix = 1;
        }
    }
    if (!has_matrix) return 0;
    if (!ff_display_matrix_supported(matrix)) return -1;
    double determinant =
        ((double)matrix[0] * (double)matrix[4] -
         (double)matrix[1] * (double)matrix[3]) /
        (65536.0 * 65536.0);
    if (determinant < 0.0) {
        matrix[0] = -matrix[0];
        matrix[3] = -matrix[3];
        *out_flip_horizontal = 1;
    }
    double ccw = av_display_rotation_get(matrix);
    if (!isfinite(ccw)) return -1;
    double turns = -ccw / 90.0;
    long quarter = lround(turns);
    if (fabs(turns - (double)quarter) > 0.0001) return -1;
    quarter %= 4;
    if (quarter < 0) quarter += 4;
    switch (quarter) {
        case 1: *out_angle = VIPS_ANGLE_D90; break;
        case 2: *out_angle = VIPS_ANGLE_D180; break;
        case 3: *out_angle = VIPS_ANGLE_D270; break;
        default: break;
    }
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

static int ff_frame_to_rgba_image(
    AVFrame *frame,
    int fallback_width, int fallback_height,
    int max_width, int max_height,
    long long deadline_monotonic_ms,
    VipsImage **out
) {
    if (out == NULL || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *out = NULL;
    int width = frame != NULL && frame->width > 0 ? frame->width : fallback_width;
    int height = frame != NULL && frame->height > 0 ? frame->height : fallback_height;
    if (ff_validate_rgba_geometry(width, height, NULL) != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int output_width = width;
    int output_height = height;
    if (max_width > 0 && max_height > 0) {
        double scale = fmin((double)max_width / (double)width,
                            (double)max_height / (double)height);
        if (!isfinite(scale) || scale <= 0.0) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        if (scale < 1.0) {
            output_width = (int)fmax(1.0, floor((double)width * scale + 0.5));
            output_height = (int)fmax(1.0, floor((double)height * scale + 0.5));
        }
    }
    size_t rgba_size = 0;
    if (ff_validate_rgba_geometry(output_width, output_height, &rgba_size) != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    uint8_t *rgba = (uint8_t *)g_try_malloc(rgba_size);
    if (rgba == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    struct SwsContext *sws = NULL;
    int convert_status = fluxer_av_frame_convert_to_rgba(
        frame, width, height, output_width, output_height,
        &sws, deadline_monotonic_ms, rgba);
    if (convert_status != FLUXER_NATIVE_STATUS_OK) {
        if (sws != NULL) sws_freeContext(sws);
        g_free(rgba);
        return convert_status;
    }
    if (sws != NULL) sws_freeContext(sws);
    VipsImage *image = vips_image_new_from_memory(
        rgba, rgba_size, output_width, output_height, 4, VIPS_FORMAT_UCHAR);
    if (image == NULL) {
        g_free(rgba);
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    if (g_signal_connect_swapped(
            image, "postclose", G_CALLBACK(g_free), rgba) == 0) {
        g_object_unref(image);
        g_free(rgba);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *out = image;
    return FLUXER_NATIVE_STATUS_OK;
}

struct ff_display_transform {
    double hscale;
    double vscale;
    VipsAngle angle;
    int flip_horizontal;
};

static int ff_display_transform_init(
    AVFormatContext *fmt,
    AVStream *stream,
    AVFrame *frame,
    struct ff_display_transform *transform
) {
    if (transform == NULL) return -1;
    transform->hscale = 1.0;
    transform->vscale = 1.0;
    transform->angle = VIPS_ANGLE_D0;
    transform->flip_horizontal = 0;
    AVRational sar = av_guess_sample_aspect_ratio(fmt, stream, frame);
    if (sar.num > 0 && sar.den > 0 && sar.num != sar.den) {
        transform->hscale = (double)sar.num / (double)sar.den;
    }
    if (!isfinite(transform->hscale) || transform->hscale <= 0.0) return -1;
    if (!isfinite(transform->vscale) || transform->vscale <= 0.0) return -1;
    return ff_display_matrix_angle(
        frame, stream, &transform->angle, &transform->flip_horizontal);
}

static int ff_apply_display_transform(
    VipsImage **image,
    const struct ff_display_transform *transform
) {
    if (image == NULL || *image == NULL || transform == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (transform->hscale != 1.0 || transform->vscale != 1.0) {
        double projected_width =
            (double)vips_image_get_width(*image) * transform->hscale;
        double projected_height =
            (double)vips_image_get_height(*image) * transform->vscale;
        if (!isfinite(projected_width) || !isfinite(projected_height)) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        if (projected_width < 1.0 || projected_height < 1.0) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        if (projected_width > (double)FLUXER_MAX_VIDEO_FRAME_DIMENSION ||
            projected_height > (double)FLUXER_MAX_VIDEO_FRAME_DIMENSION) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        if (ff_validate_rgba_geometry(
                (int)ceil(projected_width), (int)ceil(projected_height), NULL) != 0) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        VipsImage *scaled = NULL;
        if (vips_resize(*image, &scaled, transform->hscale,
                        "vscale", transform->vscale, NULL) != 0 || scaled == NULL) {
            if (scaled != NULL) g_object_unref(scaled);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (ff_validate_vips_image_bounds(scaled) != 0) {
            g_object_unref(scaled);
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        g_object_unref(*image);
        *image = scaled;
    }
    if (transform->angle != VIPS_ANGLE_D0) {
        VipsImage *rotated = NULL;
        if (vips_rot(*image, &rotated, transform->angle, NULL) != 0 || rotated == NULL) {
            if (rotated != NULL) g_object_unref(rotated);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (ff_validate_vips_image_bounds(rotated) != 0) {
            g_object_unref(rotated);
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        g_object_unref(*image);
        *image = rotated;
    }
    if (transform->flip_horizontal) {
        VipsImage *flipped = NULL;
        if (vips_flip(
                *image, &flipped, VIPS_DIRECTION_HORIZONTAL, NULL) != 0 ||
            flipped == NULL) {
            if (flipped != NULL) g_object_unref(flipped);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (ff_validate_vips_image_bounds(flipped) != 0) {
            g_object_unref(flipped);
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        g_object_unref(*image);
        *image = flipped;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_measure_display_geometry(
    const struct ff_display_transform *transform,
    int source_width, int source_height,
    int *out_width, int *out_height
) {
    if (transform == NULL || out_width == NULL || out_height == NULL) return -1;
    if (ff_validate_rgba_geometry(source_width, source_height, NULL) != 0) return -1;
    double projected_width = (double)source_width * transform->hscale;
    double projected_height = (double)source_height * transform->vscale;
    if (!isfinite(projected_width) || !isfinite(projected_height)) return -1;
    if (projected_width < 1.0 || projected_height < 1.0) return -1;
    if (projected_width > (double)FLUXER_MAX_VIDEO_FRAME_DIMENSION ||
        projected_height > (double)FLUXER_MAX_VIDEO_FRAME_DIMENSION) return -1;
    int width = (int)floor(projected_width + 0.5);
    int height = (int)floor(projected_height + 0.5);
    if (transform->angle == VIPS_ANGLE_D90 || transform->angle == VIPS_ANGLE_D270) {
        int swap = width;
        width = height;
        height = swap;
    }
    *out_width = width;
    *out_height = height;
    return ff_validate_rgba_geometry(*out_width, *out_height, NULL);
}

static int ff_prepare_frame_image(
    AVFrame *frame, AVCodecContext *dec_ctx, AVFormatContext *fmt, AVStream *stream,
    int max_width, int max_height,
    long long deadline_monotonic_ms,
    int *out_display_width, int *out_display_height,
    VipsImage **out_image
);

int ff_emit_frame_thumbnail(
    AVFrame *frame, AVCodecContext *dec_ctx, AVFormatContext *fmt, AVStream *stream,
    const char *suffix, int max_width, int max_height,
    long long deadline_monotonic_ms,
    size_t max_output_size,
    int *out_display_width, int *out_display_height,
    void **out_buf, size_t *out_size, size_t *out_capacity
) {
    if (out_display_width != NULL) *out_display_width = 0;
    if (out_display_height != NULL) *out_display_height = 0;
    if (out_buf != NULL) *out_buf = NULL;
    if (out_size != NULL) *out_size = 0;
    if (out_capacity != NULL) *out_capacity = 0;
    if (frame == NULL || dec_ctx == NULL || suffix == NULL ||
        deadline_monotonic_ms < 0 ||
        max_output_size == 0 || out_buf == NULL || out_size == NULL ||
        out_capacity == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    VipsImage *image = NULL;
    status = ff_prepare_frame_image(
        frame, dec_ctx, fmt, stream, max_width, max_height,
        deadline_monotonic_ms,
        out_display_width, out_display_height, &image);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int write_rc = fluxer_vips_image_write_to_buffer_bounded(
        image, suffix, deadline_monotonic_ms, max_output_size,
        out_buf, out_size, out_capacity);
    g_object_unref(image);
    return write_rc;
}

int ff_fit_frame_image(VipsImage **image, int max_width, int max_height) {
    if (image == NULL || *image == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (max_width <= 0 || max_height <= 0) return FLUXER_NATIVE_STATUS_OK;
    int image_width = vips_image_get_width(*image);
    int image_height = vips_image_get_height(*image);
    double scale = fmin((double)max_width / (double)image_width,
                        (double)max_height / (double)image_height);
    if (!isfinite(scale) || scale <= 0.0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (scale >= 1.0) return FLUXER_NATIVE_STATUS_OK;
    VipsImage *resized = NULL;
    if (vips_resize(*image, &resized, scale, NULL) != 0 || resized == NULL) {
        if (resized != NULL) g_object_unref(resized);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    g_object_unref(*image);
    *image = resized;
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_prepare_frame_image(
    AVFrame *frame, AVCodecContext *dec_ctx, AVFormatContext *fmt, AVStream *stream,
    int max_width, int max_height,
    long long deadline_monotonic_ms,
    int *out_display_width, int *out_display_height,
    VipsImage **out_image
) {
    if (frame == NULL || dec_ctx == NULL || out_image == NULL ||
        deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *out_image = NULL;
    int width = frame->width > 0 ? frame->width : dec_ctx->width;
    int height = frame->height > 0 ? frame->height : dec_ctx->height;
    int display_width = width;
    int display_height = height;
    int scaled_width = width;
    int scaled_height = height;
    struct ff_display_transform display_transform;
    if (ff_display_transform_init(fmt, stream, frame, &display_transform) != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (max_width > 0 || (out_display_width != NULL && out_display_height != NULL)) {
        if (ff_measure_display_geometry(&display_transform, width, height,
                                        &display_width, &display_height) != 0) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        if (out_display_width != NULL && out_display_height != NULL) {
            *out_display_width = display_width;
            *out_display_height = display_height;
        }
    }
    if (max_width > 0 && max_height > 0) {
        double scale = fmin((double)max_width / (double)display_width,
                            (double)max_height / (double)display_height);
        if (!isfinite(scale) || scale <= 0.0) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        if (scale < 1.0) {
            scaled_width = (int)fmax(1.0, floor((double)width * scale + 0.5));
            scaled_height = (int)fmax(1.0, floor((double)height * scale + 0.5));
        }
    }
    VipsImage *image = NULL;
    int status = ff_frame_to_rgba_image(
        frame, width, height, scaled_width, scaled_height,
        deadline_monotonic_ms, &image);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = ff_apply_display_transform(&image, &display_transform);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        g_object_unref(image);
        return status;
    }
    status = ff_fit_frame_image(&image, max_width, max_height);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        g_object_unref(image);
        return status;
    }
    *out_image = image;
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_emit_frame_rgba(
    AVFrame *frame, AVCodecContext *dec_ctx, AVFormatContext *fmt, AVStream *stream,
    int max_width, int max_height,
    long long deadline_monotonic_ms,
    struct fluxer_av_metadata_out *out
) {
    if (out == NULL || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    VipsImage *image = NULL;
    int status = ff_prepare_frame_image(
        frame, dec_ctx, fmt, stream, max_width, max_height,
        deadline_monotonic_ms,
        &out->display_width, &out->display_height, &image);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    out->rgba_width = vips_image_get_width(image);
    out->rgba_height = vips_image_get_height(image);
    int bands = vips_image_get_bands(image);
    size_t expected_size = 0;
    if (bands == 4 && out->rgba_width > 0 && out->rgba_height > 0) {
        expected_size = (size_t)out->rgba_width * (size_t)out->rgba_height * 4u;
    }
    if (expected_size == 0) {
        g_object_unref(image);
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    status = fluxer_vips_image_write_to_memory_deadline(
        image, deadline_monotonic_ms, expected_size,
        &out->rgba, &out->rgba_size);
    g_object_unref(image);
    if (status == FLUXER_NATIVE_STATUS_OK && out->rgba != NULL &&
        out->rgba_size == expected_size) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    int allocation_failed = out->rgba == NULL;
    if (out->rgba != NULL) g_free(out->rgba);
    out->rgba = NULL;
    out->rgba_size = 0;
    out->rgba_width = 0;
    out->rgba_height = 0;
    out->display_width = 0;
    out->display_height = 0;
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return allocation_failed ? FLUXER_NATIVE_STATUS_ALLOCATION_FAILED
                             : FLUXER_NATIVE_STATUS_CODEC_FAILURE;
}

void ff_thumbnail_context_clear(ff_thumbnail_context *context) {
    if (context == NULL) return;
    if (context->frame != NULL) av_frame_free(&context->frame);
    if (context->packet != NULL) av_packet_free(&context->packet);
    if (context->decoder != NULL) avcodec_free_context(&context->decoder);
    if (context->format != NULL) avformat_close_input(&context->format);
    if (context->avio != NULL) {
        if (context->avio->buffer != NULL) av_freep(&context->avio->buffer);
        avio_context_free(&context->avio);
    } else if (context->avio_buffer != NULL) {
        av_free(context->avio_buffer);
        context->avio_buffer = NULL;
    }
}

static int ff_container_headers_are_complete(const AVFormatContext *format) {
    if (format == NULL || format->iformat == NULL) return 0;
    if (format->duration <= 0 || format->nb_streams == 0) return 0;
    const char *name = format->iformat->name;
    if (name == NULL) return 0;
    int supported_container = strcmp(name, "matroska,webm") == 0 ||
                              strcmp(name, "mov,mp4,m4a,3gp,3g2,mj2") == 0;
    if (!supported_container) return 0;
    int media_streams = 0;
    for (unsigned int i = 0; i < format->nb_streams; i++) {
        const AVStream *stream = format->streams[i];
        if (stream == NULL || stream->codecpar == NULL) return 0;
        const AVCodecParameters *parameters = stream->codecpar;
        if (parameters->codec_type == AVMEDIA_TYPE_VIDEO) {
            if (parameters->codec_id == AV_CODEC_ID_NONE) return 0;
            if (parameters->width <= 0 || parameters->height <= 0) return 0;
            media_streams++;
        } else if (parameters->codec_type == AVMEDIA_TYPE_AUDIO) {
            if (parameters->codec_id == AV_CODEC_ID_NONE) return 0;
            media_streams++;
        }
    }
    return media_streams > 0;
}

int ff_thumbnail_context_open(
    ff_thumbnail_context *context,
    const void *media_data,
    size_t media_len,
    const char *allowed_formats,
    int strict_decode,
    long long deadline_monotonic_ms
) {
    if (context == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    memset(context, 0, sizeof(*context));
    if (media_data == NULL || media_len == 0 || allowed_formats == NULL ||
        deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (media_len > (size_t)INT64_MAX) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (strict_decode < 0 || strict_decode > 1) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    context->strict_decode = strict_decode;
    context->deadline_monotonic_ms = deadline_monotonic_ms;
    int deadline_status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    context->reader.data = (const uint8_t *)media_data;
    context->reader.len = media_len;
    context->avio_buffer = av_malloc(64 * 1024);
    if (context->avio_buffer == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    context->avio = avio_alloc_context(
        context->avio_buffer, 64 * 1024, 0, &context->reader,
        ff_mem_read_packet, NULL, ff_mem_seek);
    if (context->avio == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    context->avio_buffer = NULL;
    context->format = avformat_alloc_context();
    if (context->format == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    int status = fluxer_restrict_untrusted_av_context(context->format, allowed_formats);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    context->format->pb = context->avio;
    context->format->interrupt_callback.callback = fluxer_ffmpeg_interrupt_deadline;
    context->format->interrupt_callback.opaque = &context->deadline_monotonic_ms;
    context->format->flags |= AVFMT_FLAG_CUSTOM_IO;
    context->format->probesize = 5 * 1024 * 1024;
    context->format->max_analyze_duration = 5 * AV_TIME_BASE;
    if (strict_decode) context->format->error_recognition = AV_EF_EXPLODE;
    int open_rc = avformat_open_input(&context->format, NULL, NULL, NULL);
    if (open_rc < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            open_rc, context->deadline_monotonic_ms);
    }
    status = fluxer_prepare_untrusted_av_input(context->format);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (!ff_container_headers_are_complete(context->format)) {
        int stream_info_rc = fluxer_find_stream_info_bounded(context->format);
        if (stream_info_rc < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                stream_info_rc, context->deadline_monotonic_ms);
        }
    }
    status = fluxer_prepare_untrusted_av_input(context->format);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_thumbnail_context_probe(
    const ff_thumbnail_context *context,
    struct fluxer_av_metadata_out *out
) {
    if (context == NULL || context->format == NULL || out == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    for (unsigned int i = 0; i < context->format->nb_streams; i++) {
        int deadline_status = fluxer_native_deadline_status(
            context->deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
        const AVStream *stream = context->format->streams[i];
        if (stream == NULL || stream->codecpar == NULL) continue;
        enum AVMediaType type = stream->codecpar->codec_type;
        if (type == AVMEDIA_TYPE_VIDEO && !ff_stream_is_attached_picture(stream)) {
            out->has_video = 1;
        }
        if (type == AVMEDIA_TYPE_AUDIO) out->has_audio = 1;
    }
    if (context->format->duration > 0) {
        out->duration_seconds =
            (double)context->format->duration / (double)AV_TIME_BASE;
    }
    return out->has_video || out->has_audio
        ? FLUXER_NATIVE_STATUS_OK
        : FLUXER_NATIVE_STATUS_UNSUPPORTED;
}

int ff_thumbnail_decoder_open(ff_thumbnail_context *context, int decoder_threads) {
    assert(fluxer_ffmpeg_decoder_threads_valid(decoder_threads));
    if (context == NULL || context->format == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int deadline_status = fluxer_native_deadline_status(
        context->deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    const AVCodec *codec = NULL;
    context->stream_index = ff_find_primary_video_stream(context->format, &codec);
    if (context->stream_index < 0) return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    context->stream = context->format->streams[context->stream_index];
    if (context->stream == NULL || context->stream->codecpar == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (codec == NULL) codec = avcodec_find_decoder(context->stream->codecpar->codec_id);
    if (!fluxer_video_decoder_allowed(context->stream, codec)) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    context->decoder = avcodec_alloc_context3(codec);
    if (context->decoder == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    int parameters_rc = avcodec_parameters_to_context(
        context->decoder, context->stream->codecpar);
    if (parameters_rc < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            parameters_rc, context->deadline_monotonic_ms);
    }
    context->decoder->max_pixels = (int64_t)FLUXER_MAX_VIDEO_PIXELS;
    context->decoder->thread_count = decoder_threads;
    if (context->strict_decode) {
        context->decoder->err_recognition = AV_EF_CRCCHECK | AV_EF_BITSTREAM |
                                            AV_EF_BUFFER | AV_EF_EXPLODE;
    }
    int open_rc = avcodec_open2(context->decoder, codec, NULL);
    if (open_rc < 0) {
        return fluxer_native_status_from_av_error_with_deadline(
            open_rc, context->deadline_monotonic_ms);
    }
    context->packet = av_packet_alloc();
    context->frame = av_frame_alloc();
    if (context->packet == NULL || context->frame == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_thumbnail_decode_frame(ff_thumbnail_context *context, int max_packets) {
    if (context == NULL || context->format == NULL || context->decoder == NULL ||
        context->packet == NULL || context->frame == NULL || max_packets <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int packets_seen = 0;
    int packet_pending = 0;
    int input_exhausted = 0;
    int drain_sent = 0;
    int send_blocked = 0;
    for (;;) {
        int deadline_status = fluxer_native_deadline_status(
            context->deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
        int receive_rc = avcodec_receive_frame(context->decoder, context->frame);
        if (receive_rc == 0) return FLUXER_NATIVE_STATUS_OK;
        if (receive_rc != AVERROR(EAGAIN) && receive_rc != AVERROR_EOF) {
            return fluxer_native_status_from_av_error_with_deadline(
                receive_rc, context->deadline_monotonic_ms);
        }
        if (receive_rc == AVERROR_EOF) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        if (send_blocked) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        if (packet_pending) {
            int send_rc = avcodec_send_packet(context->decoder, context->packet);
            if (send_rc == AVERROR(EAGAIN)) {
                send_blocked = 1;
                continue;
            }
            av_packet_unref(context->packet);
            packet_pending = 0;
            if (send_rc < 0) {
                return fluxer_native_status_from_av_error_with_deadline(
                    send_rc, context->deadline_monotonic_ms);
            }
            continue;
        }
        if (input_exhausted) {
            if (drain_sent) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            int send_rc = avcodec_send_packet(context->decoder, NULL);
            if (send_rc == AVERROR(EAGAIN)) {
                send_blocked = 1;
                continue;
            }
            if (send_rc < 0) {
                return fluxer_native_status_from_av_error_with_deadline(
                    send_rc, context->deadline_monotonic_ms);
            }
            drain_sent = 1;
            continue;
        }
        if (packets_seen == max_packets) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        int read_rc = av_read_frame(context->format, context->packet);
        if (read_rc == AVERROR_EOF) {
            input_exhausted = 1;
            continue;
        }
        if (read_rc < 0) {
            return fluxer_native_status_from_av_error_with_deadline(
                read_rc, context->deadline_monotonic_ms);
        }
        packets_seen++;
        if (context->packet->stream_index != context->stream_index) {
            av_packet_unref(context->packet);
            continue;
        }
        packet_pending = 1;
    }
}

static int ff_thumbnail_decode(
    ff_thumbnail_context *context,
    const char *suffix,
    int max_packets,
    int max_width,
    int max_height,
    size_t max_output_size,
    int *out_display_width,
    int *out_display_height,
    void **out_buf,
    size_t *out_size,
    size_t *out_capacity
) {
    int status = ff_thumbnail_decode_frame(context, max_packets);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return ff_emit_frame_thumbnail(
        context->frame, context->decoder, context->format, context->stream,
        suffix, max_width, max_height, context->deadline_monotonic_ms,
        max_output_size,
        out_display_width, out_display_height, out_buf, out_size, out_capacity);
}

static int ff_thumbnail_packet_limit(int requested) {
    if (requested <= 0) return FLUXER_MAX_VIDEO_THUMBNAIL_PACKETS;
    if (requested > FLUXER_MAX_VIDEO_THUMBNAIL_PACKETS) {
        return FLUXER_MAX_VIDEO_THUMBNAIL_PACKETS;
    }
    return requested;
}

int fluxer_ffmpeg_video_thumbnail_ex(
    const void *media_data,
    size_t media_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    const char *suffix,
    int max_packets,
    int max_width,
    int max_height,
    size_t max_output_size,
    int *out_display_width,
    int *out_display_height,
    void **out_buf,
    size_t *out_size,
    size_t *out_capacity
) {
    if (out_display_width != NULL) *out_display_width = 0;
    if (out_display_height != NULL) *out_display_height = 0;
    if (out_buf != NULL) *out_buf = NULL;
    if (out_size != NULL) *out_size = 0;
    if (out_capacity != NULL) *out_capacity = 0;
    if (media_data == NULL || media_len == 0 || suffix == NULL ||
        deadline_monotonic_ms < 0 ||
        max_output_size == 0 || out_buf == NULL || out_size == NULL ||
        out_capacity == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!fluxer_ffmpeg_decoder_threads_valid(decoder_threads)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (max_width < 0 || max_height < 0 ||
        (max_width > 0) != (max_height > 0) ||
        (out_display_width == NULL) != (out_display_height == NULL)) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    max_packets = ff_thumbnail_packet_limit(max_packets);
    ff_thumbnail_context context;
    int rc = ff_thumbnail_context_open(
        &context, media_data, media_len, FLUXER_AV_INPUT_FORMATS, 0,
        deadline_monotonic_ms);
    if (rc == FLUXER_NATIVE_STATUS_OK) {
        rc = ff_thumbnail_decoder_open(&context, decoder_threads);
    }
    if (rc == FLUXER_NATIVE_STATUS_OK) {
        rc = ff_thumbnail_decode(
            &context, suffix, max_packets, max_width, max_height, max_output_size,
            out_display_width, out_display_height, out_buf, out_size, out_capacity);
    }
    ff_thumbnail_context_clear(&context);
    if (rc != FLUXER_NATIVE_STATUS_OK && *out_buf != NULL) {
        g_free(*out_buf);
        *out_buf = NULL;
        *out_size = 0;
        *out_capacity = 0;
    }
    return rc;
}

static void ff_av_metadata_discard_rgba(struct fluxer_av_metadata_out *out) {
    if (out == NULL) return;
    if (out->rgba != NULL) g_free(out->rgba);
    out->rgba = NULL;
    out->rgba_size = 0;
    out->rgba_width = 0;
    out->rgba_height = 0;
    out->display_width = 0;
    out->display_height = 0;
}

int fluxer_av_metadata(
    const void *media_data,
    size_t media_len,
    int decoder_threads,
    long long deadline_monotonic_ms,
    int max_packets,
    int max_width,
    int max_height,
    struct fluxer_av_metadata_out *out
) {
    if (out != NULL) memset(out, 0, sizeof(*out));
    if (media_data == NULL || media_len == 0 || out == NULL ||
        deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!fluxer_ffmpeg_decoder_threads_valid(decoder_threads)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (max_width < 0 || max_height < 0 ||
        (max_width > 0) != (max_height > 0)) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    max_packets = ff_thumbnail_packet_limit(max_packets);
    ff_thumbnail_context context;
    int rc = ff_thumbnail_context_open(
        &context, media_data, media_len, FLUXER_AV_INPUT_FORMATS, 0,
        deadline_monotonic_ms);
    if (rc == FLUXER_NATIVE_STATUS_OK) rc = ff_thumbnail_context_probe(&context, out);
    if (rc == FLUXER_NATIVE_STATUS_OK && out->has_video && max_width > 0) {
        int frame_rc = ff_thumbnail_decoder_open(&context, decoder_threads);
        if (frame_rc == FLUXER_NATIVE_STATUS_OK) {
            int64_t frame_count = context.stream->nb_frames;
            if (frame_count < 0) {
                frame_rc = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            } else if (frame_count > INT_MAX) {
                frame_rc = FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
            } else {
                out->frame_count = (int)frame_count;
            }
        }
        if (frame_rc == FLUXER_NATIVE_STATUS_OK) {
            frame_rc = ff_thumbnail_decode_frame(&context, max_packets);
        }
        if (frame_rc == FLUXER_NATIVE_STATUS_OK) {
            frame_rc = ff_emit_frame_rgba(
                context.frame, context.decoder, context.format, context.stream,
                max_width, max_height, deadline_monotonic_ms, out);
        }
        if (frame_rc != FLUXER_NATIVE_STATUS_OK) {
            ff_av_metadata_discard_rgba(out);
            rc = frame_rc;
        }
    }
    ff_thumbnail_context_clear(&context);
    if (rc != FLUXER_NATIVE_STATUS_OK) {
        ff_av_metadata_discard_rgba(out);
        memset(out, 0, sizeof(*out));
    }
    return rc;
}
