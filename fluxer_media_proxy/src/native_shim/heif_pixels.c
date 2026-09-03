// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

#define FLUXER_HDR_PQ_LUT_SIZE 4096
#define FLUXER_HDR_HLG_LUT_SIZE 4096
#define FLUXER_HDR_SRGB_LUT_SIZE 4096
#define FLUXER_PQ_SDR_TARGET_NORM 0.0203f
#define FLUXER_HLG_REFERENCE_PEAK_NORM 0.1f
#define FLUXER_HEIF_MAX_AUXILIARY_IMAGES 4096
#define FLUXER_HEIF_DEADLINE_ROWS 64
#define FLUXER_HEIF_ICC_PROFILE_BYTES_MAX ((size_t)4 * 1024 * 1024)

enum fluxer_heif_gamut {
    FLUXER_HEIF_GAMUT_SRGB = 0,
    FLUXER_HEIF_GAMUT_BT2020 = 1,
    FLUXER_HEIF_GAMUT_DISPLAY_P3 = 2,
};

static float fluxer_pq_lut[FLUXER_HDR_PQ_LUT_SIZE];
static float fluxer_hlg_lut[FLUXER_HDR_HLG_LUT_SIZE];
static float fluxer_hlg_ootf_scale_lut[FLUXER_HDR_HLG_LUT_SIZE];
static float fluxer_pq_tone_scale_lut[FLUXER_HDR_PQ_LUT_SIZE];
static float fluxer_hlg_tone_scale_lut[FLUXER_HDR_HLG_LUT_SIZE];
static uint8_t fluxer_srgb_lut[FLUXER_HDR_SRGB_LUT_SIZE];
static pthread_once_t fluxer_hdr_lut_once = PTHREAD_ONCE_INIT;
static float fluxer_pq_sdr_target_perceptual;
static float fluxer_hlg_source_peak_perceptual;
static float fluxer_hlg_sdr_target_perceptual;

static inline float fluxer_pq_oetf(float luminance);
static inline float fluxer_hdr_tone_scale(
    float maximum,
    float target_normalized,
    float source_peak_perceptual,
    float target_perceptual
);
static inline float fluxer_srgb_oetf(float value);
static inline uint8_t fluxer_quantize8(float value);

static void fluxer_init_hdr_luts(void) {
    const double m1 = 0.1593017578125;
    const double m2 = 78.84375;
    const double c1 = 0.8359375;
    const double c2 = 18.8515625;
    const double c3 = 18.6875;
    for (int index = 0; index < FLUXER_HDR_PQ_LUT_SIZE; index++) {
        double encoded = (double)index / (FLUXER_HDR_PQ_LUT_SIZE - 1);
        double encoded_power = pow(encoded, 1.0 / m2);
        double numerator = encoded_power - c1;
        if (numerator < 0.0) numerator = 0.0;
        double denominator = c2 - c3 * encoded_power;
        double luminance = denominator > 0.0
                         ? pow(numerator / denominator, 1.0 / m1)
                         : 0.0;
        if (luminance < 0.0) luminance = 0.0;
        if (luminance > 1.0) luminance = 1.0;
        fluxer_pq_lut[index] = (float)luminance;
    }
    const double a = 0.17883277;
    const double b = 0.28466892;
    const double c = 0.55991073;
    for (int index = 0; index < FLUXER_HDR_HLG_LUT_SIZE; index++) {
        double encoded = (double)index / (FLUXER_HDR_HLG_LUT_SIZE - 1);
        double scene = encoded <= 0.5
                     ? (encoded * encoded) / 3.0
                     : (exp((encoded - c) / a) + b) / 12.0;
        if (scene < 0.0) scene = 0.0;
        if (scene > 1.0) scene = 1.0;
        fluxer_hlg_lut[index] = (float)scene;
        double normalized = (double)index / (FLUXER_HDR_HLG_LUT_SIZE - 1);
        fluxer_hlg_ootf_scale_lut[index] = normalized > 0.0
                                         ? (float)pow(normalized, 0.2)
                                         : 0.0f;
    }
    fluxer_pq_sdr_target_perceptual =
        fluxer_pq_oetf(FLUXER_PQ_SDR_TARGET_NORM);
    fluxer_hlg_source_peak_perceptual =
        fluxer_pq_oetf(FLUXER_HLG_REFERENCE_PEAK_NORM);
    fluxer_hlg_sdr_target_perceptual =
        fluxer_pq_sdr_target_perceptual /
        fluxer_hlg_source_peak_perceptual;
    for (int index = 0; index < FLUXER_HDR_PQ_LUT_SIZE; index++) {
        fluxer_pq_tone_scale_lut[index] = fluxer_hdr_tone_scale(
            fluxer_pq_lut[index], FLUXER_PQ_SDR_TARGET_NORM,
            1.0f,
            fluxer_pq_sdr_target_perceptual);
    }
    for (int index = 0; index < FLUXER_HDR_HLG_LUT_SIZE; index++) {
        float maximum = (float)index / (FLUXER_HDR_HLG_LUT_SIZE - 1);
        float absolute_maximum =
            maximum * FLUXER_HLG_REFERENCE_PEAK_NORM;
        fluxer_hlg_tone_scale_lut[index] =
            FLUXER_HLG_REFERENCE_PEAK_NORM * fluxer_hdr_tone_scale(
                absolute_maximum, FLUXER_PQ_SDR_TARGET_NORM,
                fluxer_hlg_source_peak_perceptual,
                fluxer_hlg_sdr_target_perceptual);
    }
    for (int index = 0; index < FLUXER_HDR_SRGB_LUT_SIZE; index++) {
        float linear = (float)index / (FLUXER_HDR_SRGB_LUT_SIZE - 1);
        fluxer_srgb_lut[index] = fluxer_quantize8(
            fluxer_srgb_oetf(linear));
    }
}

static inline uint16_t fluxer_hdr_lut_index(uint16_t code, int bit_depth) {
    assert(bit_depth == 10 || bit_depth == 12);
    if (bit_depth == 12) return code & 0x0fffu;
    uint16_t code10 = code & 0x03ffu;
    return (uint16_t)((code10 << 2) | (code10 >> 8));
}

static inline uint16_t fluxer_unit_lut_index(float value) {
    if (value <= 0.0f) return 0;
    if (value >= 1.0f) return FLUXER_HDR_HLG_LUT_SIZE - 1;
    return (uint16_t)(
        value * (FLUXER_HDR_HLG_LUT_SIZE - 1) + 0.5f);
}

static inline uint16_t fluxer_heif_read_le16(const uint8_t *value) {
    return (uint16_t)((uint16_t)value[0] | ((uint16_t)value[1] << 8));
}

static inline float fluxer_bt2390_eetf_perceptual(
    float encoded,
    float max_luminance
) {
    if (encoded <= 0.0f) return 0.0f;
    if (max_luminance >= 1.0f) {
        return encoded > 1.0f ? 1.0f : encoded;
    }
    float knee = 1.5f * max_luminance - 0.5f;
    if (encoded < knee) return encoded;
    if (encoded >= 1.0f) return max_luminance;
    float position = (encoded - knee) / (1.0f - knee);
    float squared = position * position;
    float cubed = squared * position;
    float start_basis = 2.0f * cubed - 3.0f * squared + 1.0f;
    float tangent_basis = cubed - 2.0f * squared + position;
    float end_basis = -2.0f * cubed + 3.0f * squared;
    float mapped = start_basis * knee + tangent_basis * (1.0f - knee) +
                   end_basis * max_luminance;
    if (mapped > max_luminance) mapped = max_luminance;
    if (mapped < 0.0f) mapped = 0.0f;
    return mapped;
}

static inline float fluxer_pq_oetf(float luminance) {
    if (luminance <= 0.0f) return 0.0f;
    if (luminance >= 1.0f) luminance = 1.0f;
    const float m1 = 0.1593017578125f;
    const float m2 = 78.84375f;
    const float c1 = 0.8359375f;
    const float c2 = 18.8515625f;
    const float c3 = 18.6875f;
    float power = powf(luminance, m1);
    return powf((c1 + c2 * power) / (1.0f + c3 * power), m2);
}

static inline float fluxer_srgb_oetf(float value) {
    if (value <= 0.0f) return 0.0f;
    if (value >= 1.0f) return 1.0f;
    if (value <= 0.0031308f) return 12.92f * value;
    return 1.055f * powf(value, 1.0f / 2.4f) - 0.055f;
}

static inline uint8_t fluxer_quantize8(float value) {
    if (value <= 0.0f) return 0;
    if (value >= 1.0f) return 255;
    int quantized = (int)(value * 255.0f + 0.5f);
    if (quantized < 0) return 0;
    if (quantized > 255) return 255;
    return (uint8_t)quantized;
}

static inline uint8_t fluxer_srgb_lut_quantize(float value) {
    if (value <= 0.0f) return 0;
    if (value >= 1.0f) return 255;
    size_t index = (size_t)(
        value * (FLUXER_HDR_SRGB_LUT_SIZE - 1) + 0.5f);
    assert(index < FLUXER_HDR_SRGB_LUT_SIZE);
    return fluxer_srgb_lut[index];
}

static inline void fluxer_bt2020_to_bt709_linear(
    float red,
    float green,
    float blue,
    float *out_red,
    float *out_green,
    float *out_blue
) {
    *out_red = 1.6605f * red - 0.5876f * green - 0.0728f * blue;
    *out_green = -0.1246f * red + 1.1329f * green - 0.0083f * blue;
    *out_blue = -0.0182f * red - 0.1006f * green + 1.1187f * blue;
}

static inline void fluxer_display_p3_to_srgb_linear(
    float red,
    float green,
    float blue,
    float *out_red,
    float *out_green,
    float *out_blue
) {
    *out_red = 1.2249401f * red - 0.2249404f * green;
    *out_green = -0.0420569f * red + 1.0420571f * green;
    *out_blue = -0.0196376f * red - 0.0786361f * green + 1.0982735f * blue;
}

static inline float fluxer_inverse_srgb(float encoded) {
    if (encoded <= 0.0f) return 0.0f;
    if (encoded >= 1.0f) return 1.0f;
    if (encoded <= 0.04045f) return encoded / 12.92f;
    return powf((encoded + 0.055f) / 1.055f, 2.4f);
}

static inline float fluxer_inverse_bt709(float encoded) {
    if (encoded <= 0.0f) return 0.0f;
    if (encoded >= 1.0f) return 1.0f;
    if (encoded < 0.081f) return encoded / 4.5f;
    return powf((encoded + 0.099f) / 1.099f, 1.0f / 0.45f);
}

static inline float fluxer_inverse_bt2020_12(float encoded) {
    if (encoded <= 0.0f) return 0.0f;
    if (encoded >= 1.0f) return 1.0f;
    if (encoded < 0.08145f) return encoded / 4.5f;
    return powf((encoded + 0.0993f) / 1.0993f, 1.0f / 0.45f);
}

static inline void fluxer_heif_convert_gamut_linear(
    int gamut,
    float red,
    float green,
    float blue,
    float *out_red,
    float *out_green,
    float *out_blue
) {
    if (gamut == FLUXER_HEIF_GAMUT_BT2020) {
        fluxer_bt2020_to_bt709_linear(
            red, green, blue, out_red, out_green, out_blue);
        return;
    }
    if (gamut == FLUXER_HEIF_GAMUT_DISPLAY_P3) {
        fluxer_display_p3_to_srgb_linear(
            red, green, blue, out_red, out_green, out_blue);
        return;
    }
    assert(gamut == FLUXER_HEIF_GAMUT_SRGB);
    *out_red = red;
    *out_green = green;
    *out_blue = blue;
}

static inline float fluxer_heif_linear_luma(
    int gamut,
    float red,
    float green,
    float blue
) {
    if (gamut == FLUXER_HEIF_GAMUT_BT2020) {
        return 0.2627f * red + 0.6780f * green + 0.0593f * blue;
    }
    if (gamut == FLUXER_HEIF_GAMUT_DISPLAY_P3) {
        return 0.2289746f * red + 0.6917385f * green + 0.0792869f * blue;
    }
    assert(gamut == FLUXER_HEIF_GAMUT_SRGB);
    return 0.2126f * red + 0.7152f * green + 0.0722f * blue;
}

static inline float fluxer_inverse_pq(float encoded) {
    const float m1 = 0.1593017578125f;
    const float m2 = 78.84375f;
    const float c1 = 0.8359375f;
    const float c2 = 18.8515625f;
    const float c3 = 18.6875f;
    float power = powf(encoded, 1.0f / m2);
    float numerator = power - c1;
    if (numerator < 0.0f) numerator = 0.0f;
    float denominator = c2 - c3 * power;
    if (denominator <= 0.0f) return 0.0f;
    float luminance = powf(numerator / denominator, 1.0f / m1);
    return luminance < 0.0f ? 0.0f : luminance;
}

static inline float fluxer_hdr_tone_scale(
    float maximum,
    float target_normalized,
    float source_peak_perceptual,
    float target_perceptual
) {
    if (maximum <= 0.0f) return 0.0f;
    assert(source_peak_perceptual > 0.0f);
    assert(source_peak_perceptual <= 1.0f);
    float perceptual = fluxer_pq_oetf(maximum) /
                       source_peak_perceptual;
    float mapped_perceptual = fluxer_bt2390_eetf_perceptual(
        perceptual, target_perceptual);
    float mapped = fluxer_inverse_pq(
        mapped_perceptual * source_peak_perceptual);
    return (mapped / maximum) / target_normalized;
}

static inline void fluxer_hdr_pipeline_pixel(
    float red,
    float green,
    float blue,
    float scale,
    int gamut,
    uint8_t *output
) {
    float display_red = red * scale;
    float display_green = green * scale;
    float display_blue = blue * scale;
    if (display_red < 0.0f) display_red = 0.0f;
    if (display_green < 0.0f) display_green = 0.0f;
    if (display_blue < 0.0f) display_blue = 0.0f;
    if (display_red > 1.0f) display_red = 1.0f;
    if (display_green > 1.0f) display_green = 1.0f;
    if (display_blue > 1.0f) display_blue = 1.0f;
    float linear_red = display_red;
    float linear_green = display_green;
    float linear_blue = display_blue;
    fluxer_heif_convert_gamut_linear(
        gamut, display_red, display_green, display_blue,
        &linear_red, &linear_green, &linear_blue);
    if (linear_red < 0.0f) linear_red = 0.0f;
    if (linear_green < 0.0f) linear_green = 0.0f;
    if (linear_blue < 0.0f) linear_blue = 0.0f;
    if (linear_red > 1.0f) linear_red = 1.0f;
    if (linear_green > 1.0f) linear_green = 1.0f;
    if (linear_blue > 1.0f) linear_blue = 1.0f;
    output[0] = fluxer_srgb_lut_quantize(linear_red);
    output[1] = fluxer_srgb_lut_quantize(linear_green);
    output[2] = fluxer_srgb_lut_quantize(linear_blue);
}

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
            *out_gamut = FLUXER_HEIF_GAMUT_SRGB;
            return FLUXER_NATIVE_STATUS_OK;
        case heif_color_primaries_ITU_R_BT_2020_2_and_2100_0:
            *out_gamut = FLUXER_HEIF_GAMUT_BT2020;
            return FLUXER_NATIVE_STATUS_OK;
        case heif_color_primaries_SMPTE_EG_432_1:
            *out_gamut = FLUXER_HEIF_GAMUT_DISPLAY_P3;
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
    int gamut = FLUXER_HEIF_GAMUT_SRGB;
    int status = fluxer_heif_nclx_gamut(profile->primaries, &gamut);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int transfer = profile->transfer;
    int is_srgb = transfer == heif_transfer_characteristic_IEC_61966_2_1;
    int is_bt709 = transfer == heif_transfer_characteristic_ITU_R_BT_709_5 ||
                   transfer == heif_transfer_characteristic_ITU_R_BT_601_6 ||
                   transfer == heif_transfer_characteristic_ITU_R_BT_2020_2_10bit;
    int is_bt2020_12 =
        transfer == heif_transfer_characteristic_ITU_R_BT_2020_2_12bit;
    int is_linear = transfer == heif_transfer_characteristic_linear;
    if (!is_srgb && !is_bt709 && !is_bt2020_12 && !is_linear) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (is_srgb && gamut == FLUXER_HEIF_GAMUT_SRGB) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    size_t row_bytes = (size_t)width * 4u;
    for (int row = 0; row < height; row++) {
        if (row % FLUXER_HEIF_DEADLINE_ROWS == 0) {
            status = fluxer_native_deadline_status(deadline_monotonic_ms);
            if (status != FLUXER_NATIVE_STATUS_OK) return status;
        }
        uint8_t *row_data = destination + (size_t)row * row_bytes;
        for (int column = 0; column < width; column++) {
            uint8_t *pixel = row_data + (size_t)column * 4u;
            float red = (float)pixel[0] / 255.0f;
            float green = (float)pixel[1] / 255.0f;
            float blue = (float)pixel[2] / 255.0f;
            if (is_srgb) {
                red = fluxer_inverse_srgb(red);
                green = fluxer_inverse_srgb(green);
                blue = fluxer_inverse_srgb(blue);
            } else if (is_bt709) {
                red = fluxer_inverse_bt709(red);
                green = fluxer_inverse_bt709(green);
                blue = fluxer_inverse_bt709(blue);
            } else if (is_bt2020_12) {
                red = fluxer_inverse_bt2020_12(red);
                green = fluxer_inverse_bt2020_12(green);
                blue = fluxer_inverse_bt2020_12(blue);
            }
            float srgb_red = 0.0f;
            float srgb_green = 0.0f;
            float srgb_blue = 0.0f;
            fluxer_heif_convert_gamut_linear(
                gamut, red, green, blue,
                &srgb_red, &srgb_green, &srgb_blue);
            pixel[0] = fluxer_quantize8(fluxer_srgb_oetf(srgb_red));
            pixel[1] = fluxer_quantize8(fluxer_srgb_oetf(srgb_green));
            pixel[2] = fluxer_quantize8(fluxer_srgb_oetf(srgb_blue));
        }
    }
    return fluxer_native_deadline_status(deadline_monotonic_ms);
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

struct fluxer_heif_hdr_transform {
    int bit_depth;
    int mask;
    int gamut;
    int is_hlg;
    const float *linear_lut;
    const float *tone_scale_lut;
};

static void fluxer_heif_transform_hdr_row(
    const uint8_t *source,
    uint8_t *destination,
    int width,
    const struct fluxer_heif_hdr_transform *transform
) {
    assert(source != NULL);
    assert(destination != NULL);
    assert(transform != NULL);
    for (int column = 0; column < width; column++) {
        const uint8_t *source_pixel = source + (size_t)column * 8u;
        uint16_t red_code =
            fluxer_heif_read_le16(source_pixel) & transform->mask;
        uint16_t green_code =
            fluxer_heif_read_le16(source_pixel + 2) & transform->mask;
        uint16_t blue_code =
            fluxer_heif_read_le16(source_pixel + 4) & transform->mask;
        uint16_t red_index = fluxer_hdr_lut_index(
            red_code, transform->bit_depth);
        uint16_t green_index = fluxer_hdr_lut_index(
            green_code, transform->bit_depth);
        uint16_t blue_index = fluxer_hdr_lut_index(
            blue_code, transform->bit_depth);
        float red = transform->linear_lut[red_index];
        float green = transform->linear_lut[green_index];
        float blue = transform->linear_lut[blue_index];
        uint16_t maximum_index = red_index;
        if (green_index > maximum_index) maximum_index = green_index;
        if (blue_index > maximum_index) maximum_index = blue_index;
        if (transform->is_hlg) {
            float luma = fluxer_heif_linear_luma(
                transform->gamut, red, green, blue);
            float ootf_scale =
                fluxer_hlg_ootf_scale_lut[fluxer_unit_lut_index(luma)];
            red *= ootf_scale;
            green *= ootf_scale;
            blue *= ootf_scale;
            float maximum = fmaxf(red, fmaxf(green, blue));
            maximum_index = fluxer_unit_lut_index(maximum);
        }
        float scale = transform->tone_scale_lut[maximum_index];
        uint8_t *destination_pixel = destination + (size_t)column * 4u;
        fluxer_hdr_pipeline_pixel(
            red, green, blue, scale, transform->gamut,
            destination_pixel);
        uint16_t alpha =
            fluxer_heif_read_le16(source_pixel + 6) & transform->mask;
        destination_pixel[3] = (uint8_t)(
            (alpha * 255 + (transform->mask >> 1)) / transform->mask);
    }
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
    if (pthread_once(&fluxer_hdr_lut_once, fluxer_init_hdr_luts) != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int bit_depth = heif_image_handle_get_luma_bits_per_pixel(handle);
    if (bit_depth != 10 && bit_depth != 12) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (!profile->nclx_present) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    int gamut = FLUXER_HEIF_GAMUT_SRGB;
    if (fluxer_heif_nclx_gamut(profile->primaries, &gamut) !=
        FLUXER_NATIVE_STATUS_OK) {
        gamut = FLUXER_HEIF_GAMUT_SRGB;
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
    struct fluxer_heif_hdr_transform transform = {
        .bit_depth = bit_depth,
        .mask = (1 << bit_depth) - 1,
        .gamut = gamut,
        .is_hlg =
            profile->transfer == heif_transfer_characteristic_ITU_R_BT_2100_0_HLG,
        .linear_lut =
            profile->transfer == heif_transfer_characteristic_ITU_R_BT_2100_0_PQ
                    ? fluxer_pq_lut
                    : fluxer_hlg_lut,
        .tone_scale_lut =
            profile->transfer == heif_transfer_characteristic_ITU_R_BT_2100_0_PQ
                        ? fluxer_pq_tone_scale_lut
                        : fluxer_hlg_tone_scale_lut,
    };
    if (status == FLUXER_NATIVE_STATUS_OK) {
        size_t destination_stride = (size_t)width * 4u;
        for (int row = 0; row < height; row++) {
            if (row % FLUXER_HEIF_DEADLINE_ROWS == 0) {
                status = fluxer_native_deadline_status(
                    deadline_monotonic_ms);
                if (status != FLUXER_NATIVE_STATUS_OK) break;
            }
            fluxer_heif_transform_hdr_row(
                plane + (size_t)row * (size_t)stride,
                destination + (size_t)row * destination_stride,
                width, &transform);
        }
        if (status == FLUXER_NATIVE_STATUS_OK) {
            status = fluxer_native_deadline_status(deadline_monotonic_ms);
        }
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
