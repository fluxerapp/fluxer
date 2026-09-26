// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

struct ff_frame_color_plan {
    int transfer;
    int gamut;
};

static int ff_frame_color_gamut(const AVFrame *frame, int *out_gamut) {
    assert(frame != NULL);
    assert(out_gamut != NULL);
    switch (frame->color_primaries) {
        case AVCOL_PRI_UNSPECIFIED:
        case AVCOL_PRI_BT709:
        case AVCOL_PRI_BT470M:
        case AVCOL_PRI_BT470BG:
        case AVCOL_PRI_SMPTE170M:
        case AVCOL_PRI_SMPTE240M:
        case AVCOL_PRI_FILM:
            *out_gamut = FLUXER_HDR_GAMUT_SRGB;
            return 0;
        case AVCOL_PRI_BT2020:
            *out_gamut = FLUXER_HDR_GAMUT_BT2020;
            return 0;
        case AVCOL_PRI_SMPTE432:
            *out_gamut = FLUXER_HDR_GAMUT_DISPLAY_P3;
            return 0;
        default:
            return -1;
    }
}

static int ff_frame_color_transfer(const AVFrame *frame, int *out_transfer) {
    assert(frame != NULL);
    assert(out_transfer != NULL);
    switch (frame->color_trc) {
        case AVCOL_TRC_UNSPECIFIED:
        case AVCOL_TRC_IEC61966_2_1:
        case AVCOL_TRC_GAMMA22:
        case AVCOL_TRC_GAMMA28:
            *out_transfer = FLUXER_HDR_TRANSFER_SRGB;
            return 0;
        case AVCOL_TRC_BT709:
        case AVCOL_TRC_SMPTE170M:
        case AVCOL_TRC_SMPTE240M:
        case AVCOL_TRC_BT2020_10:
            *out_transfer = FLUXER_HDR_TRANSFER_BT709;
            return 0;
        case AVCOL_TRC_BT2020_12:
            *out_transfer = FLUXER_HDR_TRANSFER_BT2020_12;
            return 0;
        case AVCOL_TRC_LINEAR:
            *out_transfer = FLUXER_HDR_TRANSFER_LINEAR;
            return 0;
        case AVCOL_TRC_SMPTE2084:
            *out_transfer = FLUXER_HDR_TRANSFER_PQ;
            return 0;
        case AVCOL_TRC_ARIB_STD_B67:
            *out_transfer = FLUXER_HDR_TRANSFER_HLG;
            return 0;
        default:
            return -1;
    }
}

static int ff_frame_color_plan(
    const AVFrame *frame,
    struct ff_frame_color_plan *out_plan
) {
    assert(frame != NULL);
    assert(out_plan != NULL);
    switch (frame->colorspace) {
        case AVCOL_SPC_RGB:
        case AVCOL_SPC_BT709:
        case AVCOL_SPC_UNSPECIFIED:
        case AVCOL_SPC_FCC:
        case AVCOL_SPC_BT470BG:
        case AVCOL_SPC_SMPTE170M:
        case AVCOL_SPC_SMPTE240M:
        case AVCOL_SPC_BT2020_NCL:
            break;
        default:
            return -1;
    }
    if (frame->color_range != AVCOL_RANGE_UNSPECIFIED &&
        frame->color_range != AVCOL_RANGE_MPEG &&
        frame->color_range != AVCOL_RANGE_JPEG) {
        return -1;
    }
    if (ff_frame_color_gamut(frame, &out_plan->gamut) != 0) return -1;
    return ff_frame_color_transfer(frame, &out_plan->transfer);
}

static int ff_frame_plan_is_passthrough(
    const struct ff_frame_color_plan *plan
) {
    assert(plan != NULL);
    return plan->gamut == FLUXER_HDR_GAMUT_SRGB &&
           !fluxer_hdr_transfer_is_hdr(plan->transfer) &&
           plan->transfer != FLUXER_HDR_TRANSFER_LINEAR;
}

typedef int (*ff_i420_to_abgr_fn)(
    const uint8_t *, int,
    const uint8_t *, int,
    const uint8_t *, int,
    uint8_t *, int,
    int, int
);

static int ff_swscale_colorspace(enum AVColorSpace colorspace) {
    switch (colorspace) {
        case AVCOL_SPC_BT709:
            return SWS_CS_ITU709;
        case AVCOL_SPC_FCC:
            return SWS_CS_FCC;
        case AVCOL_SPC_BT470BG:
            return SWS_CS_ITU624;
        case AVCOL_SPC_SMPTE170M:
            return SWS_CS_SMPTE170M;
        case AVCOL_SPC_SMPTE240M:
            return SWS_CS_SMPTE240M;
        case AVCOL_SPC_BT2020_NCL:
            return SWS_CS_BT2020;
        default:
            return SWS_CS_DEFAULT;
    }
}

static int ff_configure_swscale_color(
    struct SwsContext *sws,
    const AVFrame *frame
) {
    assert(sws != NULL);
    assert(frame != NULL);
    int colorspace = ff_swscale_colorspace(frame->colorspace);
    const int *coefficients = sws_getCoefficients(colorspace);
    if (coefficients == NULL) return -1;
    int source_full_range = frame->color_range == AVCOL_RANGE_JPEG;
    const AVPixFmtDescriptor *descriptor = av_pix_fmt_desc_get(frame->format);
    if (descriptor == NULL) return -1;
    if (frame->color_range == AVCOL_RANGE_UNSPECIFIED &&
        (descriptor->flags & AV_PIX_FMT_FLAG_RGB) != 0) {
        source_full_range = 1;
    }
    int rc = sws_setColorspaceDetails(
        sws, coefficients, source_full_range, coefficients, 1,
        0, 1 << 16, 1 << 16);
    return rc < 0 ? -1 : 0;
}

static ff_i420_to_abgr_fn ff_libyuv_converter(const AVFrame *frame) {
    if (frame == NULL ||
        (frame->color_range != AVCOL_RANGE_MPEG &&
         frame->color_range != AVCOL_RANGE_UNSPECIFIED)) {
        return NULL;
    }
    switch (frame->colorspace) {
        case AVCOL_SPC_BT709:
            return H420ToABGR;
        case AVCOL_SPC_UNSPECIFIED:
        case AVCOL_SPC_BT470BG:
        case AVCOL_SPC_SMPTE170M:
            return I420ToABGR;
        default:
            return NULL;
    }
}

static int ff_convert_i420_frame_to_rgba_libyuv(
    AVFrame *frame,
    int source_width,
    int source_height,
    int output_width,
    int output_height,
    long long deadline_monotonic_ms,
    uint8_t *dst,
    int *out_applied
) {
    assert(out_applied != NULL);
    *out_applied = 0;
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    if (frame == NULL || frame->format != AV_PIX_FMT_YUV420P || dst == NULL) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    if (source_width != output_width || source_height != output_height) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    ff_i420_to_abgr_fn convert = ff_libyuv_converter(frame);
    if (convert == NULL || frame->data[0] == NULL ||
        frame->data[1] == NULL || frame->data[2] == NULL) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    int source_chroma_width = (source_width + 1) / 2;
    if (frame->linesize[0] < source_width ||
        frame->linesize[1] < source_chroma_width ||
        frame->linesize[2] < source_chroma_width) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    *out_applied = 1;
    for (int row = 0; row < output_height;) {
        int status = fluxer_native_deadline_status(deadline_monotonic_ms);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
        int rows = output_height - row;
        if (rows > FLUXER_VIDEO_DEADLINE_ROWS) {
            rows = FLUXER_VIDEO_DEADLINE_ROWS;
        }
        const uint8_t *y = frame->data[0] + (size_t)row * frame->linesize[0];
        const uint8_t *u =
            frame->data[1] + (size_t)(row / 2) * frame->linesize[1];
        const uint8_t *v =
            frame->data[2] + (size_t)(row / 2) * frame->linesize[2];
        uint8_t *output =
            dst + (size_t)row * (size_t)output_width * 4u;
        int convert_rc = convert(
            y, frame->linesize[0],
            u, frame->linesize[1],
            v, frame->linesize[2],
            output, output_width * 4,
            output_width, rows);
        if (convert_rc != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        row += rows;
    }
    return fluxer_native_deadline_status(deadline_monotonic_ms);
#else
    (void)frame;
    (void)source_width;
    (void)source_height;
    (void)output_width;
    (void)output_height;
    (void)deadline_monotonic_ms;
    (void)dst;
    return FLUXER_NATIVE_STATUS_OK;
#endif
}

struct ff_swscale_source_layout {
    unsigned int required_planes;
    int chroma_alignment;
    int is_paletted;
    int is_bayer;
};

static int ff_swscale_validate_source_layout(
    const AVFrame *frame,
    const AVPixFmtDescriptor *descriptor,
    struct ff_swscale_source_layout *out
) {
    assert(frame != NULL);
    assert(descriptor != NULL);
    assert(out != NULL);
    if (descriptor->nb_components == 0) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (descriptor->nb_components > 4) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    unsigned int required_planes = 0;
    for (int component = 0;
         component < descriptor->nb_components;
         component++) {
        int plane = descriptor->comp[component].plane;
        if (plane < 0) {
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
        }
        if (plane >= 4) {
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
        }
        required_planes |= 1u << plane;
    }
    for (int plane = 0; plane < 4; plane++) {
        if ((required_planes & (1u << plane)) == 0) continue;
        if (frame->data[plane] == NULL) {
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
        }
        if (frame->linesize[plane] <= 0) {
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
        }
    }
    int is_paletted = (descriptor->flags & AV_PIX_FMT_FLAG_PAL) != 0;
    if (is_paletted) {
        if (frame->data[1] == NULL) {
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
        }
    }
    if (descriptor->log2_chroma_h >= sizeof(int) * CHAR_BIT - 1) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    int chroma_alignment = 1 << descriptor->log2_chroma_h;
    if (chroma_alignment > FLUXER_VIDEO_DEADLINE_ROWS) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    out->required_planes = required_planes;
    out->chroma_alignment = chroma_alignment;
    out->is_paletted = is_paletted;
    out->is_bayer = (descriptor->flags & AV_PIX_FMT_FLAG_BAYER) != 0;
    return FLUXER_NATIVE_STATUS_OK;
}

static int ff_swscale_frame_to_rgba(
    struct SwsContext *sws,
    const AVFrame *frame,
    int source_height,
    int output_width,
    int output_height,
    int destination_pixel_bytes,
    long long deadline_monotonic_ms,
    uint8_t *dst
) {
    assert(sws != NULL);
    assert(frame != NULL);
    assert(source_height > 0);
    assert(output_width > 0);
    assert(output_height > 0);
    assert(destination_pixel_bytes == 4 || destination_pixel_bytes == 8);
    assert(dst != NULL);
    const AVPixFmtDescriptor *descriptor = av_pix_fmt_desc_get(frame->format);
    if (descriptor == NULL) return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    struct ff_swscale_source_layout layout;
    int status = ff_swscale_validate_source_layout(frame, descriptor, &layout);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int slice_alignment = layout.is_bayer ? 2 : layout.chroma_alignment;
    if (layout.is_bayer) {
        if (source_height < 2) return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    uint8_t *dst_data[4] = { dst, NULL, NULL, NULL };
    int dst_linesize[4] = { output_width * destination_pixel_bytes, 0, 0, 0 };
    int output_rows = 0;
    for (int source_row = 0; source_row < source_height;) {
        status = fluxer_native_deadline_status(deadline_monotonic_ms);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
        int source_rows = source_height - source_row;
        if (source_rows > FLUXER_VIDEO_DEADLINE_ROWS) {
            source_rows = FLUXER_VIDEO_DEADLINE_ROWS;
        }
        if (source_row + source_rows < source_height) {
            source_rows -= source_rows % slice_alignment;
            if (layout.is_bayer) {
                if (source_height - source_row - source_rows == 1) {
                    source_rows -= slice_alignment;
                }
            }
        }
        if (source_rows <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        const uint8_t *source_data[4] = { NULL, NULL, NULL, NULL };
        for (int plane = 0; plane < 4; plane++) {
            if ((layout.required_planes & (1u << plane)) == 0) continue;
            int row = source_row;
            if (plane == 1) row >>= descriptor->log2_chroma_h;
            if (plane == 2) row >>= descriptor->log2_chroma_h;
            source_data[plane] =
                frame->data[plane] + (size_t)row * frame->linesize[plane];
        }
        if (layout.is_paletted) source_data[1] = frame->data[1];
        int scaled_rows = sws_scale(
            sws, source_data, frame->linesize,
            source_row, source_rows, dst_data, dst_linesize);
        if (scaled_rows < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        if (scaled_rows > output_height - output_rows) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        output_rows += scaled_rows;
        source_row += source_rows;
    }
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return output_rows == output_height
        ? FLUXER_NATIVE_STATUS_OK
        : FLUXER_NATIVE_STATUS_CODEC_FAILURE;
}

static int ff_convert_hdr_frame_to_rgba(
    AVFrame *frame,
    int source_width,
    int source_height,
    int output_width,
    int output_height,
    struct SwsContext **sws,
    long long deadline_monotonic_ms,
    uint8_t *dst,
    size_t rgba_size,
    const struct ff_frame_color_plan *plan
) {
    assert(sws != NULL);
    assert(dst != NULL);
    assert(plan != NULL);
    if (rgba_size > FLUXER_MAX_VIDEO_RGBA_BYTES / 2u) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    uint8_t *staging = (uint8_t *)g_try_malloc(rgba_size * 2u);
    if (staging == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    *sws = sws_getCachedContext(*sws,
                                source_width, source_height,
                                (enum AVPixelFormat)frame->format,
                                output_width, output_height,
                                AV_PIX_FMT_RGBA64LE,
                                SWS_FAST_BILINEAR, NULL, NULL, NULL);
    int status = FLUXER_NATIVE_STATUS_OK;
    if (*sws == NULL) {
        status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    } else if (ff_configure_swscale_color(*sws, frame) != 0) {
        status = FLUXER_NATIVE_STATUS_UNSUPPORTED;
    } else {
        status = ff_swscale_frame_to_rgba(
            *sws, frame, source_height, output_width, output_height, 8,
            deadline_monotonic_ms, staging);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_hdr_tone_map_rgba16(
            staging, (size_t)output_width * 8u,
            dst, (size_t)output_width * 4u,
            output_width, output_height, 16,
            plan->gamut, plan->transfer, FLUXER_VIDEO_DEADLINE_ROWS,
            deadline_monotonic_ms);
    }
    g_free(staging);
    return status;
}

int fluxer_av_frame_convert_to_rgba(
    AVFrame *frame,
    int source_width,
    int source_height,
    int output_width,
    int output_height,
    struct SwsContext **sws,
    long long deadline_monotonic_ms,
    uint8_t *dst
) {
    if (frame == NULL || sws == NULL || dst == NULL ||
        deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    size_t rgba_size = 0;
    if (ff_validate_rgba_geometry(source_width, source_height, NULL) != 0 ||
        ff_validate_rgba_geometry(output_width, output_height, &rgba_size) != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    struct ff_frame_color_plan plan;
    if (ff_frame_color_plan(frame, &plan) != 0) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (fluxer_hdr_transfer_is_hdr(plan.transfer)) {
        return ff_convert_hdr_frame_to_rgba(
            frame, source_width, source_height, output_width, output_height,
            sws, deadline_monotonic_ms, dst, rgba_size, &plan);
    }
    int libyuv_applied = 0;
    status = ff_convert_i420_frame_to_rgba_libyuv(
        frame, source_width, source_height, output_width, output_height,
        deadline_monotonic_ms, dst, &libyuv_applied);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (!libyuv_applied) {
        *sws = sws_getCachedContext(*sws,
                                    source_width, source_height,
                                    (enum AVPixelFormat)frame->format,
                                    output_width, output_height,
                                    AV_PIX_FMT_RGBA,
                                    SWS_FAST_BILINEAR, NULL, NULL, NULL);
        if (*sws == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        if (ff_configure_swscale_color(*sws, frame) != 0) {
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
        }
        status = ff_swscale_frame_to_rgba(
            *sws, frame, source_height, output_width, output_height, 4,
            deadline_monotonic_ms, dst);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
    }
    if (ff_frame_plan_is_passthrough(&plan)) return FLUXER_NATIVE_STATUS_OK;
    return fluxer_hdr_apply_sdr_gamut(
        dst, output_width, output_height, plan.gamut, plan.transfer,
        FLUXER_VIDEO_DEADLINE_ROWS, deadline_monotonic_ms);
}
