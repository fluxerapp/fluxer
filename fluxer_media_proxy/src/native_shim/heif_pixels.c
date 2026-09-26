// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

#define FLUXER_HEIF_MAX_AUXILIARY_IMAGES 4096
#define FLUXER_HEIF_DEADLINE_ROWS 64
#define FLUXER_HEIF_ICC_PROFILE_BYTES_MAX ((size_t)4 * 1024 * 1024)

static unsigned char fluxer_ascii_lower(unsigned char value) {
    if (value >= 'A' && value <= 'Z') {
        return (unsigned char)(value + ('a' - 'A'));
    }
    return value;
}

static int fluxer_ascii_contains_folded(
    const char *haystack,
    const char *needle
) {
    if (haystack == NULL || needle == NULL || needle[0] == '\0') return 0;
    size_t needle_length = strlen(needle);
    for (const char *position = haystack; *position != '\0'; position++) {
        size_t index = 0;
        while (index < needle_length && position[index] != '\0' &&
               fluxer_ascii_lower((unsigned char)position[index]) ==
               fluxer_ascii_lower((unsigned char)needle[index])) {
            index++;
        }
        if (index == needle_length) return 1;
    }
    return 0;
}

static int fluxer_heif_aux_type_is_hdr_gain_map(const char *type) {
    if (type == NULL || type[0] == '\0') return 0;
    if (fluxer_ascii_contains_folded(type, "hdrgainmap") ||
        fluxer_ascii_contains_folded(type, "hdr_gain_map") ||
        fluxer_ascii_contains_folded(type, "hdr-gain-map")) {
        return 1;
    }
    if (!fluxer_ascii_contains_folded(type, "gainmap")) return 0;
    return fluxer_ascii_contains_folded(type, "hdr") ||
           fluxer_ascii_contains_folded(type, "21496") ||
           fluxer_ascii_contains_folded(type, "iso");
}

static int fluxer_heif_auxiliary_is_gain_map(
    struct heif_image_handle *primary,
    heif_item_id identifier,
    long long deadline_monotonic_ms,
    int *found
) {
    assert(primary != NULL);
    assert(found != NULL);
    *found = 0;
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    struct heif_image_handle *auxiliary = NULL;
    struct heif_error error = heif_image_handle_get_auxiliary_image_handle(
        primary, identifier, &auxiliary);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        if (auxiliary != NULL) heif_image_handle_release(auxiliary);
        return status;
    }
    if (error.code != heif_error_Ok) {
        if (auxiliary != NULL) heif_image_handle_release(auxiliary);
        return fluxer_native_status_from_heif_error(error);
    }
    if (auxiliary == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    const char *type = NULL;
    error = heif_image_handle_get_auxiliary_type(auxiliary, &type);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        if (type != NULL) {
            heif_image_handle_release_auxiliary_type(auxiliary, &type);
        }
        heif_image_handle_release(auxiliary);
        return status;
    }
    if (error.code != heif_error_Ok) {
        if (type != NULL) {
            heif_image_handle_release_auxiliary_type(auxiliary, &type);
        }
        heif_image_handle_release(auxiliary);
        return fluxer_native_status_from_heif_error(error);
    }
    if (type == NULL) {
        heif_image_handle_release(auxiliary);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *found = fluxer_heif_aux_type_is_hdr_gain_map(type);
    heif_image_handle_release_auxiliary_type(auxiliary, &type);
    heif_image_handle_release(auxiliary);
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_heif_detect_hdr_gain_map(
    struct heif_image_handle *handle,
    long long deadline_monotonic_ms,
    int *detected
) {
    if (handle == NULL || deadline_monotonic_ms < 0 || detected == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *detected = 0;
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int filter = LIBHEIF_AUX_IMAGE_FILTER_OMIT_ALPHA |
                 LIBHEIF_AUX_IMAGE_FILTER_OMIT_DEPTH;
    int count = heif_image_handle_get_number_of_auxiliary_images(
        handle, filter);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (count < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (count > FLUXER_HEIF_MAX_AUXILIARY_IMAGES) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (count == 0) return FLUXER_NATIVE_STATUS_OK;
    heif_item_id *identifiers = calloc((size_t)count, sizeof(*identifiers));
    if (identifiers == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    int received = heif_image_handle_get_list_of_auxiliary_image_IDs(
        handle, filter, identifiers, count);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status == FLUXER_NATIVE_STATUS_OK && received != count) {
        status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    for (int index = 0;
         status == FLUXER_NATIVE_STATUS_OK && index < received;
         index++) {
        status = fluxer_native_deadline_status(deadline_monotonic_ms);
        if (status != FLUXER_NATIVE_STATUS_OK) break;
        int found = 0;
        status = fluxer_heif_auxiliary_is_gain_map(
            handle, identifiers[index], deadline_monotonic_ms, &found);
        if (found) {
            *detected = 1;
            break;
        }
    }
    free(identifiers);
    return status;
}

int fluxer_heif_checked_rgba_size(
    int width,
    int height,
    size_t *out_size
) {
    if (width <= 0 || height <= 0 || out_size == NULL) return -1;
    if ((size_t)width > SIZE_MAX / 4u) return -1;
    size_t row_bytes = (size_t)width * 4u;
    if ((size_t)height > SIZE_MAX / row_bytes) return -1;
    *out_size = row_bytes * (size_t)height;
    return 0;
}

struct fluxer_heif_color_profile {
    int transfer;
    int primaries;
    int matrix;
    int nclx_present;
    size_t icc_size;
};

static int fluxer_heif_read_color_profile(
    struct heif_image_handle *handle,
    long long deadline_monotonic_ms,
    struct fluxer_heif_color_profile *profile
) {
    assert(handle != NULL);
    assert(profile != NULL);
    memset(profile, 0, sizeof(*profile));
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    profile->icc_size = heif_image_handle_get_raw_color_profile_size(handle);
    if (profile->icc_size > FLUXER_HEIF_ICC_PROFILE_BYTES_MAX) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    struct heif_color_profile_nclx *nclx = NULL;
    struct heif_error error =
        heif_image_handle_get_nclx_color_profile(handle, &nclx);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        if (nclx != NULL) heif_nclx_color_profile_free(nclx);
        return status;
    }
    if (error.code == heif_error_Color_profile_does_not_exist) {
        if (nclx != NULL) {
            heif_nclx_color_profile_free(nclx);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        return FLUXER_NATIVE_STATUS_OK;
    }
    if (error.code != heif_error_Ok) {
        if (nclx != NULL) heif_nclx_color_profile_free(nclx);
        return fluxer_native_status_from_heif_error(error);
    }
    if (nclx == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    profile->nclx_present = 1;
    profile->transfer = (int)nclx->transfer_characteristics;
    profile->primaries = (int)nclx->color_primaries;
    profile->matrix = (int)nclx->matrix_coefficients;
    heif_nclx_color_profile_free(nclx);
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_heif_apply_icc_profile(
    struct heif_image_handle *handle,
    uint8_t *destination,
    int width,
    int height,
    size_t profile_size,
    long long deadline_monotonic_ms
) {
    assert(handle != NULL);
    assert(destination != NULL);
    assert(profile_size > 0);
    assert(profile_size <= FLUXER_HEIF_ICC_PROFILE_BYTES_MAX);
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    uint8_t *profile_bytes = malloc(profile_size);
    if (profile_bytes == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    struct heif_error error = heif_image_handle_get_raw_color_profile(
        handle, profile_bytes);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status == FLUXER_NATIVE_STATUS_OK && error.code != heif_error_Ok) {
        status = fluxer_native_status_from_heif_error(error);
    }
    cmsHPROFILE input_profile = NULL;
    cmsHPROFILE output_profile = NULL;
    cmsHTRANSFORM transform = NULL;
    uint8_t *source_row = NULL;
    if (status == FLUXER_NATIVE_STATUS_OK) {
        input_profile = cmsOpenProfileFromMem(
            profile_bytes, (cmsUInt32Number)profile_size);
        output_profile = cmsCreate_sRGBProfile();
        if (input_profile == NULL || output_profile == NULL) {
            status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
    }
    if (status == FLUXER_NATIVE_STATUS_OK &&
        (cmsGetColorSpace(input_profile) != cmsSigRgbData ||
         (cmsGetPCS(input_profile) != cmsSigXYZData &&
          cmsGetPCS(input_profile) != cmsSigLabData) ||
         (cmsGetDeviceClass(input_profile) != cmsSigInputClass &&
          cmsGetDeviceClass(input_profile) != cmsSigDisplayClass &&
          cmsGetDeviceClass(input_profile) != cmsSigColorSpaceClass) ||
         !cmsIsMatrixShaper(input_profile))) {
        status = FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_native_deadline_status(deadline_monotonic_ms);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        transform = cmsCreateTransform(
            input_profile, TYPE_RGBA_8,
            output_profile, TYPE_RGBA_8,
            INTENT_RELATIVE_COLORIMETRIC,
            cmsFLAGS_BLACKPOINTCOMPENSATION | cmsFLAGS_COPY_ALPHA);
        if (transform == NULL) status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_native_deadline_status(deadline_monotonic_ms);
    }
    size_t row_bytes = (size_t)width * 4u;
    if (status == FLUXER_NATIVE_STATUS_OK) {
        source_row = malloc(row_bytes);
        if (source_row == NULL) status = FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    for (int row = 0;
         status == FLUXER_NATIVE_STATUS_OK && row < height;
         row++) {
        if (row % FLUXER_HEIF_DEADLINE_ROWS == 0) {
            status = fluxer_native_deadline_status(deadline_monotonic_ms);
            if (status != FLUXER_NATIVE_STATUS_OK) break;
        }
        uint8_t *destination_row = destination + (size_t)row * row_bytes;
        memcpy(source_row, destination_row, row_bytes);
        cmsDoTransform(transform, source_row, destination_row, (cmsUInt32Number)width);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_native_deadline_status(deadline_monotonic_ms);
    }
    free(source_row);
    if (transform != NULL) cmsDeleteTransform(transform);
    if (output_profile != NULL) cmsCloseProfile(output_profile);
    if (input_profile != NULL) cmsCloseProfile(input_profile);
    free(profile_bytes);
    return status;
}

static int fluxer_heif_nclx_gamut(int primaries, int *out_gamut) {
    assert(out_gamut != NULL);
    switch (primaries) {
        case heif_color_primaries_ITU_R_BT_709_5:
            *out_gamut = FLUXER_HDR_GAMUT_SRGB;
            return FLUXER_NATIVE_STATUS_OK;
        case heif_color_primaries_ITU_R_BT_2020_2_and_2100_0:
            *out_gamut = FLUXER_HDR_GAMUT_BT2020;
            return FLUXER_NATIVE_STATUS_OK;
        case heif_color_primaries_SMPTE_EG_432_1:
            *out_gamut = FLUXER_HDR_GAMUT_DISPLAY_P3;
            return FLUXER_NATIVE_STATUS_OK;
        default:
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
}

static int fluxer_heif_nclx_transfer(int transfer, int *out_transfer) {
    assert(out_transfer != NULL);
    switch (transfer) {
        case heif_transfer_characteristic_IEC_61966_2_1:
            *out_transfer = FLUXER_HDR_TRANSFER_SRGB;
            return FLUXER_NATIVE_STATUS_OK;
        case heif_transfer_characteristic_ITU_R_BT_709_5:
        case heif_transfer_characteristic_ITU_R_BT_601_6:
        case heif_transfer_characteristic_ITU_R_BT_2020_2_10bit:
            *out_transfer = FLUXER_HDR_TRANSFER_BT709;
            return FLUXER_NATIVE_STATUS_OK;
        case heif_transfer_characteristic_ITU_R_BT_2020_2_12bit:
            *out_transfer = FLUXER_HDR_TRANSFER_BT2020_12;
            return FLUXER_NATIVE_STATUS_OK;
        case heif_transfer_characteristic_linear:
            *out_transfer = FLUXER_HDR_TRANSFER_LINEAR;
            return FLUXER_NATIVE_STATUS_OK;
        default:
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
}

static int fluxer_heif_apply_sdr_nclx(
    uint8_t *destination,
    int width,
    int height,
    const struct fluxer_heif_color_profile *profile,
    long long deadline_monotonic_ms
) {
    assert(destination != NULL);
    assert(profile != NULL);
    int gamut = FLUXER_HDR_GAMUT_SRGB;
    int status = fluxer_heif_nclx_gamut(profile->primaries, &gamut);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int transfer = FLUXER_HDR_TRANSFER_SRGB;
    status = fluxer_heif_nclx_transfer(profile->transfer, &transfer);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return fluxer_hdr_apply_sdr_gamut(
        destination, width, height, gamut, transfer,
        FLUXER_HEIF_DEADLINE_ROWS, deadline_monotonic_ms);
}

static int fluxer_heif_cancel_decoding(void *opaque) {
    if (opaque == NULL) return 1;
    const long long *deadline_monotonic_ms = opaque;
    return fluxer_native_deadline_status(*deadline_monotonic_ms) !=
           FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_heif_decode_interleaved(
    struct heif_image_handle *handle,
    enum heif_chroma chroma,
    long long deadline_monotonic_ms,
    struct heif_image **out_image
) {
    assert(handle != NULL);
    assert(out_image != NULL);
    *out_image = NULL;
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    struct heif_decoding_options *options = heif_decoding_options_alloc();
    if (options == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    options->progress_user_data = &deadline_monotonic_ms;
    options->cancel_decoding = fluxer_heif_cancel_decoding;
#if LIBHEIF_HAVE_VERSION(1, 21, 0)
    struct heif_color_profile_nclx *source_nclx = NULL;
    size_t raw_profile_size =
        heif_image_handle_get_raw_color_profile_size(handle);
    struct heif_error profile_error =
        heif_image_handle_get_nclx_color_profile(handle, &source_nclx);
    if (profile_error.code == heif_error_Ok && source_nclx == NULL) {
        heif_decoding_options_free(options);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (profile_error.code == heif_error_Ok) {
        options->output_image_nclx_profile = source_nclx;
    } else if (profile_error.code != heif_error_Color_profile_does_not_exist ||
               source_nclx != NULL) {
        if (source_nclx != NULL) heif_nclx_color_profile_free(source_nclx);
        heif_decoding_options_free(options);
        return fluxer_native_status_from_heif_error(profile_error);
    }
#if !LIBHEIF_HAVE_VERSION(1, 23, 0)
    if (source_nclx == NULL && raw_profile_size > 0) {
        heif_decoding_options_free(options);
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
#endif
#endif
#if LIBHEIF_HAVE_VERSION(1, 23, 0)
    options->output_image_nclx_profile_passthrough =
        source_nclx != NULL || raw_profile_size > 0;
#endif
    struct heif_error error = heif_decode_image(
        handle, out_image, heif_colorspace_RGB, chroma, options);
#if LIBHEIF_HAVE_VERSION(1, 21, 0)
    if (source_nclx != NULL) heif_nclx_color_profile_free(source_nclx);
#endif
    heif_decoding_options_free(options);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        if (*out_image != NULL) {
            heif_image_release(*out_image);
            *out_image = NULL;
        }
        return status;
    }
    if (error.code != heif_error_Ok) {
        if (*out_image != NULL) {
            heif_image_release(*out_image);
            *out_image = NULL;
        }
        return fluxer_native_status_from_heif_error(error);
    }
    if (*out_image == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_heif_interleaved_plane(
    struct heif_image *image,
    int width,
    int height,
    enum heif_chroma chroma,
    int storage_bits,
    int value_bits,
    size_t row_bytes,
    const uint8_t **out_plane,
    int *out_stride
) {
    assert(image != NULL);
    assert(out_plane != NULL);
    assert(out_stride != NULL);
    *out_plane = heif_image_get_plane_readonly(
        image, heif_channel_interleaved, out_stride);
    if (heif_image_get_primary_width(image) != width ||
        heif_image_get_primary_height(image) != height) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int actual_storage_bits = heif_image_get_bits_per_pixel(
        image, heif_channel_interleaved);
    int actual_value_bits = heif_image_get_bits_per_pixel_range(
        image, heif_channel_interleaved);
    if (heif_image_get_colorspace(image) != heif_colorspace_RGB ||
        heif_image_get_chroma_format(image) != chroma ||
        actual_storage_bits != storage_bits ||
        actual_value_bits != value_bits || *out_plane == NULL ||
        *out_stride <= 0 || (size_t)*out_stride < row_bytes) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_heif_decode_sdr(
    struct heif_image_handle *handle,
    uint8_t *destination,
    int width,
    int height,
    long long deadline_monotonic_ms,
    const struct fluxer_heif_color_profile *profile
) {
    struct heif_image *image = NULL;
    int status = fluxer_heif_decode_interleaved(
        handle, heif_chroma_interleaved_RGBA,
        deadline_monotonic_ms, &image);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    size_t row_bytes = (size_t)width * 4u;
    const uint8_t *plane = NULL;
    int stride = 0;
    status = fluxer_heif_interleaved_plane(
        image, width, height, heif_chroma_interleaved_RGBA,
        32, 8, row_bytes, &plane, &stride);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        for (int row = 0; row < height; row++) {
            if (row % FLUXER_HEIF_DEADLINE_ROWS == 0) {
                status = fluxer_native_deadline_status(
                    deadline_monotonic_ms);
                if (status != FLUXER_NATIVE_STATUS_OK) break;
            }
            memcpy(destination + (size_t)row * row_bytes,
                   plane + (size_t)row * (size_t)stride, row_bytes);
        }
        if (status == FLUXER_NATIVE_STATUS_OK) {
            status = fluxer_native_deadline_status(deadline_monotonic_ms);
        }
    }
    heif_image_release(image);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (profile->icc_size > 0) {
        return fluxer_heif_apply_icc_profile(
            handle, destination, width, height,
            profile->icc_size, deadline_monotonic_ms);
    }
    if (profile->nclx_present) {
        return fluxer_heif_apply_sdr_nclx(
            destination, width, height, profile, deadline_monotonic_ms);
    }
    return status;
}

static int fluxer_heif_decode_hdr(
    struct heif_image_handle *handle,
    uint8_t *destination,
    int width,
    int height,
    long long deadline_monotonic_ms,
    const struct fluxer_heif_color_profile *profile
) {
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int bit_depth = heif_image_handle_get_luma_bits_per_pixel(handle);
    if (bit_depth != 10 && bit_depth != 12) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (!profile->nclx_present) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    int transfer;
    if (profile->transfer == heif_transfer_characteristic_ITU_R_BT_2100_0_PQ) {
        transfer = FLUXER_HDR_TRANSFER_PQ;
    } else if (profile->transfer ==
               heif_transfer_characteristic_ITU_R_BT_2100_0_HLG) {
        transfer = FLUXER_HDR_TRANSFER_HLG;
    } else {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    int gamut = FLUXER_HDR_GAMUT_SRGB;
    if (fluxer_heif_nclx_gamut(profile->primaries, &gamut) !=
        FLUXER_NATIVE_STATUS_OK) {
        gamut = FLUXER_HDR_GAMUT_SRGB;
    }
    if ((size_t)width > SIZE_MAX / 8u) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    struct heif_image *image = NULL;
    status = fluxer_heif_decode_interleaved(
        handle, heif_chroma_interleaved_RRGGBBAA_LE,
        deadline_monotonic_ms, &image);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    const uint8_t *plane = NULL;
    int stride = 0;
    status = fluxer_heif_interleaved_plane(
        image, width, height, heif_chroma_interleaved_RRGGBBAA_LE,
        64, bit_depth, (size_t)width * 8u, &plane, &stride);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_hdr_tone_map_rgba16(
            plane, (size_t)stride, destination, (size_t)width * 4u,
            width, height, bit_depth, gamut, transfer,
            FLUXER_HEIF_DEADLINE_ROWS, deadline_monotonic_ms);
    }
    heif_image_release(image);
    return status;
}

int fluxer_heif_decode_to_sdr_rgba8(
    struct heif_image_handle *handle,
    uint8_t *destination,
    size_t destination_capacity,
    int width,
    int height,
    long long deadline_monotonic_ms,
    int *out_was_hdr
) {
    if (handle == NULL || destination == NULL || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (out_was_hdr != NULL) *out_was_hdr = 0;
    if (heif_image_handle_is_premultiplied_alpha(handle)) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    size_t expected_size = 0;
    if (fluxer_heif_checked_rgba_size(width, height, &expected_size) != 0 ||
        destination_capacity < expected_size) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    struct fluxer_heif_color_profile profile;
    int status = fluxer_heif_read_color_profile(
        handle, deadline_monotonic_ms, &profile);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (!profile.nclx_present) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    int is_hdr =
        profile.transfer == heif_transfer_characteristic_ITU_R_BT_2100_0_PQ ||
        profile.transfer == heif_transfer_characteristic_ITU_R_BT_2100_0_HLG;
    if (!is_hdr) {
        if (profile.matrix == heif_matrix_coefficients_unspecified ||
            profile.matrix ==
                heif_matrix_coefficients_ITU_R_BT_2020_2_constant_luminance ||
            profile.matrix ==
                heif_matrix_coefficients_chromaticity_derived_constant_luminance) {
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
        }
        return fluxer_heif_decode_sdr(
            handle, destination, width, height,
            deadline_monotonic_ms, &profile);
    }
    status = fluxer_heif_decode_hdr(
        handle, destination, width, height,
        deadline_monotonic_ms, &profile);
    if (status == FLUXER_NATIVE_STATUS_OK && out_was_hdr != NULL) {
        *out_was_hdr = 1;
    }
    return status;
}
