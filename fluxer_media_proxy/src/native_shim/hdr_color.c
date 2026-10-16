// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

float fluxer_pq_lut[FLUXER_HDR_PQ_LUT_SIZE];
float fluxer_hlg_lut[FLUXER_HDR_HLG_LUT_SIZE];
float fluxer_hlg_ootf_scale_lut[FLUXER_HDR_HLG_LUT_SIZE];
float fluxer_pq_tone_scale_lut[FLUXER_HDR_PQ_LUT_SIZE];
float fluxer_hlg_tone_scale_lut[FLUXER_HDR_HLG_LUT_SIZE];
uint8_t fluxer_srgb_lut[FLUXER_HDR_SRGB_LUT_SIZE];
float fluxer_pq_sdr_target_perceptual;
float fluxer_hlg_source_peak_perceptual;
float fluxer_hlg_sdr_target_perceptual;

static pthread_once_t fluxer_hdr_lut_once = PTHREAD_ONCE_INIT;

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

static inline uint16_t fluxer_hdr_read_le16(const uint8_t *value) {
    return (uint16_t)((uint16_t)value[0] | ((uint16_t)value[1] << 8));
}

int fluxer_hdr_luts_ready(void) {
    if (pthread_once(&fluxer_hdr_lut_once, fluxer_init_hdr_luts) != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_hdr_transfer_is_hdr(int transfer) {
    return transfer == FLUXER_HDR_TRANSFER_PQ ||
           transfer == FLUXER_HDR_TRANSFER_HLG;
}

int fluxer_hdr_apply_sdr_gamut(
    uint8_t *rgba,
    int width,
    int height,
    int gamut,
    int transfer,
    int deadline_rows,
    long long deadline_monotonic_ms
) {
    if (rgba == NULL || width <= 0 || height <= 0 || deadline_rows <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (fluxer_hdr_transfer_is_hdr(transfer)) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (transfer != FLUXER_HDR_TRANSFER_SRGB &&
        transfer != FLUXER_HDR_TRANSFER_BT709 &&
        transfer != FLUXER_HDR_TRANSFER_BT2020_12 &&
        transfer != FLUXER_HDR_TRANSFER_LINEAR) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (transfer == FLUXER_HDR_TRANSFER_SRGB &&
        gamut == FLUXER_HDR_GAMUT_SRGB) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    int status = fluxer_hdr_luts_ready();
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    size_t row_bytes = (size_t)width * 4u;
    for (int row = 0; row < height; row++) {
        if (row % deadline_rows == 0) {
            status = fluxer_native_deadline_status(deadline_monotonic_ms);
            if (status != FLUXER_NATIVE_STATUS_OK) return status;
        }
        uint8_t *row_data = rgba + (size_t)row * row_bytes;
        for (int column = 0; column < width; column++) {
            uint8_t *pixel = row_data + (size_t)column * 4u;
            float red = (float)pixel[0] / 255.0f;
            float green = (float)pixel[1] / 255.0f;
            float blue = (float)pixel[2] / 255.0f;
            if (transfer == FLUXER_HDR_TRANSFER_SRGB) {
                red = fluxer_inverse_srgb(red);
                green = fluxer_inverse_srgb(green);
                blue = fluxer_inverse_srgb(blue);
            } else if (transfer == FLUXER_HDR_TRANSFER_BT709) {
                red = fluxer_inverse_bt709(red);
                green = fluxer_inverse_bt709(green);
                blue = fluxer_inverse_bt709(blue);
            } else if (transfer == FLUXER_HDR_TRANSFER_BT2020_12) {
                red = fluxer_inverse_bt2020_12(red);
                green = fluxer_inverse_bt2020_12(green);
                blue = fluxer_inverse_bt2020_12(blue);
            }
            float srgb_red = 0.0f;
            float srgb_green = 0.0f;
            float srgb_blue = 0.0f;
            fluxer_hdr_convert_gamut_linear(
                gamut, red, green, blue,
                &srgb_red, &srgb_green, &srgb_blue);
            pixel[0] = fluxer_quantize8(fluxer_srgb_oetf(srgb_red));
            pixel[1] = fluxer_quantize8(fluxer_srgb_oetf(srgb_green));
            pixel[2] = fluxer_quantize8(fluxer_srgb_oetf(srgb_blue));
        }
    }
    return fluxer_native_deadline_status(deadline_monotonic_ms);
}

int fluxer_hdr_tone_map_rgba16(
    const uint8_t *source,
    size_t source_stride,
    uint8_t *destination,
    size_t destination_stride,
    int width,
    int height,
    int bit_depth,
    int gamut,
    int transfer,
    int deadline_rows,
    long long deadline_monotonic_ms
) {
    if (source == NULL || destination == NULL || width <= 0 || height <= 0 ||
        deadline_rows <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (bit_depth != 10 && bit_depth != 12 && bit_depth != 16) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if (!fluxer_hdr_transfer_is_hdr(transfer)) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }
    if ((size_t)width > SIZE_MAX / 8u) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (source_stride < (size_t)width * 8u ||
        destination_stride < (size_t)width * 4u) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int status = fluxer_hdr_luts_ready();
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int is_hlg = transfer == FLUXER_HDR_TRANSFER_HLG;
    unsigned int mask = bit_depth == 16
                      ? 0xffffu
                      : (unsigned int)((1u << bit_depth) - 1u);
    const float *linear_lut = is_hlg ? fluxer_hlg_lut : fluxer_pq_lut;
    const float *tone_scale_lut =
        is_hlg ? fluxer_hlg_tone_scale_lut : fluxer_pq_tone_scale_lut;
    for (int row = 0; row < height; row++) {
        if (row % deadline_rows == 0) {
            status = fluxer_native_deadline_status(deadline_monotonic_ms);
            if (status != FLUXER_NATIVE_STATUS_OK) return status;
        }
        const uint8_t *source_row = source + (size_t)row * source_stride;
        uint8_t *destination_row =
            destination + (size_t)row * destination_stride;
        for (int column = 0; column < width; column++) {
            const uint8_t *source_pixel = source_row + (size_t)column * 8u;
            uint16_t red_code =
                (uint16_t)(fluxer_hdr_read_le16(source_pixel) & mask);
            uint16_t green_code =
                (uint16_t)(fluxer_hdr_read_le16(source_pixel + 2) & mask);
            uint16_t blue_code =
                (uint16_t)(fluxer_hdr_read_le16(source_pixel + 4) & mask);
            uint16_t red_index = fluxer_hdr_lut_index(red_code, bit_depth);
            uint16_t green_index = fluxer_hdr_lut_index(green_code, bit_depth);
            uint16_t blue_index = fluxer_hdr_lut_index(blue_code, bit_depth);
            float red = linear_lut[red_index];
            float green = linear_lut[green_index];
            float blue = linear_lut[blue_index];
            uint16_t maximum_index = red_index;
            if (green_index > maximum_index) maximum_index = green_index;
            if (blue_index > maximum_index) maximum_index = blue_index;
            if (is_hlg) {
                float luma = fluxer_hdr_linear_luma(gamut, red, green, blue);
                float ootf_scale =
                    fluxer_hlg_ootf_scale_lut[fluxer_unit_lut_index(luma)];
                red *= ootf_scale;
                green *= ootf_scale;
                blue *= ootf_scale;
                float maximum = fmaxf(red, fmaxf(green, blue));
                maximum_index = fluxer_unit_lut_index(maximum);
            }
            float scale = tone_scale_lut[maximum_index];
            uint8_t *destination_pixel =
                destination_row + (size_t)column * 4u;
            fluxer_hdr_pipeline_pixel(
                red, green, blue, scale, gamut, destination_pixel);
            unsigned int alpha =
                (unsigned int)fluxer_hdr_read_le16(source_pixel + 6) & mask;
            destination_pixel[3] = (uint8_t)(
                (alpha * 255u + (mask >> 1)) / mask);
        }
    }
    return fluxer_native_deadline_status(deadline_monotonic_ms);
}
