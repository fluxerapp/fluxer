// SPDX-License-Identifier: AGPL-3.0-or-later

#pragma once

#include "vips_shim.h"

#include <assert.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>

#define FLUXER_HDR_PQ_LUT_SIZE 4096
#define FLUXER_HDR_HLG_LUT_SIZE 4096
#define FLUXER_HDR_SRGB_LUT_SIZE 4096
#define FLUXER_PQ_SDR_TARGET_NORM 0.0203f
#define FLUXER_HLG_REFERENCE_PEAK_NORM 0.1f

enum fluxer_hdr_gamut {
    FLUXER_HDR_GAMUT_SRGB = 0,
    FLUXER_HDR_GAMUT_BT2020 = 1,
    FLUXER_HDR_GAMUT_DISPLAY_P3 = 2,
};

enum fluxer_hdr_transfer {
    FLUXER_HDR_TRANSFER_SRGB = 0,
    FLUXER_HDR_TRANSFER_BT709 = 1,
    FLUXER_HDR_TRANSFER_BT2020_12 = 2,
    FLUXER_HDR_TRANSFER_LINEAR = 3,
    FLUXER_HDR_TRANSFER_PQ = 4,
    FLUXER_HDR_TRANSFER_HLG = 5,
};

extern float fluxer_pq_lut[FLUXER_HDR_PQ_LUT_SIZE];
extern float fluxer_hlg_lut[FLUXER_HDR_HLG_LUT_SIZE];
extern float fluxer_hlg_ootf_scale_lut[FLUXER_HDR_HLG_LUT_SIZE];
extern float fluxer_pq_tone_scale_lut[FLUXER_HDR_PQ_LUT_SIZE];
extern float fluxer_hlg_tone_scale_lut[FLUXER_HDR_HLG_LUT_SIZE];
extern uint8_t fluxer_srgb_lut[FLUXER_HDR_SRGB_LUT_SIZE];
extern float fluxer_pq_sdr_target_perceptual;
extern float fluxer_hlg_source_peak_perceptual;
extern float fluxer_hlg_sdr_target_perceptual;

int fluxer_hdr_luts_ready(void);
int fluxer_hdr_transfer_is_hdr(int transfer);
int fluxer_hdr_apply_sdr_gamut(uint8_t *rgba, int width, int height, int gamut,
                               int transfer, int deadline_rows,
                               long long deadline_monotonic_ms);
int fluxer_hdr_tone_map_rgba16(const uint8_t *source, size_t source_stride,
                               uint8_t *destination, size_t destination_stride,
                               int width, int height, int bit_depth, int gamut,
                               int transfer, int deadline_rows,
                               long long deadline_monotonic_ms);

static inline uint16_t fluxer_hdr_lut_index(uint16_t code, int bit_depth) {
    assert(bit_depth == 10 || bit_depth == 12 || bit_depth == 16);
    if (bit_depth == 16) return (uint16_t)(code >> 4);
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

static inline void fluxer_hdr_convert_gamut_linear(
    int gamut,
    float red,
    float green,
    float blue,
    float *out_red,
    float *out_green,
    float *out_blue
) {
    if (gamut == FLUXER_HDR_GAMUT_BT2020) {
        fluxer_bt2020_to_bt709_linear(
            red, green, blue, out_red, out_green, out_blue);
        return;
    }
    if (gamut == FLUXER_HDR_GAMUT_DISPLAY_P3) {
        fluxer_display_p3_to_srgb_linear(
            red, green, blue, out_red, out_green, out_blue);
        return;
    }
    assert(gamut == FLUXER_HDR_GAMUT_SRGB);
    *out_red = red;
    *out_green = green;
    *out_blue = blue;
}

static inline float fluxer_hdr_linear_luma(
    int gamut,
    float red,
    float green,
    float blue
) {
    if (gamut == FLUXER_HDR_GAMUT_BT2020) {
        return 0.2627f * red + 0.6780f * green + 0.0593f * blue;
    }
    if (gamut == FLUXER_HDR_GAMUT_DISPLAY_P3) {
        return 0.2289746f * red + 0.6917385f * green + 0.0792869f * blue;
    }
    assert(gamut == FLUXER_HDR_GAMUT_SRGB);
    return 0.2126f * red + 0.7152f * green + 0.0722f * blue;
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
    fluxer_hdr_convert_gamut_linear(
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
