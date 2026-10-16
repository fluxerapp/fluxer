// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

#include <libavutil/crc.h>

enum fluxer_png_idat_state {
    FLUXER_PNG_BEFORE_IDAT = 0,
    FLUXER_PNG_READING_IDAT = 1,
    FLUXER_PNG_AFTER_IDAT = 2
};

enum fluxer_apng_frame_data_kind {
    FLUXER_APNG_NO_FRAME_DATA = 0,
    FLUXER_APNG_IDAT_FRAME_DATA = 1,
    FLUXER_APNG_FDAT_FRAME_DATA = 2
};

struct fluxer_apng_chunk {
    uint32_t length;
    const uint8_t *type;
    const uint8_t *payload;
    size_t next_offset;
};

struct fluxer_apng_validation {
    const uint8_t *data;
    size_t len;
    size_t offset;
    size_t chunk_count;
    int chunk_index;
    int max_frames;
    size_t max_total_pixels;
    uint32_t width;
    uint32_t height;
    uint32_t declared_frames;
    uint32_t declared_plays;
    uint32_t frame_controls;
    uint32_t next_sequence;
    uint8_t color_type;
    int saw_actl;
    int saw_plte;
    int saw_idat;
    int saw_idat_payload;
    int saw_fdat;
    int saw_iend;
    int active_frame;
    int active_frame_has_data;
    enum fluxer_png_idat_state idat_state;
    enum fluxer_apng_frame_data_kind active_frame_data;
    const AVCRC *crc_table;
};

static uint32_t fluxer_read_be32(const uint8_t *value) {
    assert(value != NULL);
    return ((uint32_t)value[0] << 24) |
           ((uint32_t)value[1] << 16) |
           ((uint32_t)value[2] << 8) |
           (uint32_t)value[3];
}

static int fluxer_png_chunk_type_valid(const uint8_t *type) {
    if (type == NULL) return 0;
    for (size_t index = 0; index < 4; index++) {
        uint8_t value = type[index];
        int uppercase = value >= 'A' && value <= 'Z';
        int lowercase = value >= 'a' && value <= 'z';
        if (!uppercase && !lowercase) return 0;
    }
    return (type[2] & 0x20u) == 0;
}

static int fluxer_png_chunk_crc_valid(
    const AVCRC *table,
    const struct fluxer_apng_chunk *chunk
) {
    assert(table != NULL);
    assert(chunk != NULL);
    uint32_t crc = av_crc(table, UINT32_MAX, chunk->type, 4);
    crc = av_crc(table, crc, chunk->payload, chunk->length) ^ UINT32_MAX;
    uint32_t expected = fluxer_read_be32(chunk->payload + chunk->length);
    return crc == expected;
}

static int fluxer_png_ihdr_valid(const uint8_t *payload) {
    assert(payload != NULL);
    uint8_t bit_depth = payload[8];
    uint8_t color_type = payload[9];
    int bit_depth_valid = 0;
    switch (color_type) {
        case 0:
            bit_depth_valid = bit_depth == 1 || bit_depth == 2 ||
                              bit_depth == 4 || bit_depth == 8 ||
                              bit_depth == 16;
            break;
        case 2:
        case 4:
        case 6:
            bit_depth_valid = bit_depth == 8 || bit_depth == 16;
            break;
        case 3:
            bit_depth_valid = bit_depth == 1 || bit_depth == 2 ||
                              bit_depth == 4 || bit_depth == 8;
            break;
        default:
            return 0;
    }
    if (!bit_depth_valid) return 0;
    if (payload[10] != 0) return 0;
    if (payload[11] != 0) return 0;
    return payload[12] <= 1;
}

static int fluxer_apng_sequence_advance(
    const uint8_t *payload,
    uint32_t *next_sequence
) {
    assert(payload != NULL);
    assert(next_sequence != NULL);
    uint32_t sequence = fluxer_read_be32(payload);
    if (sequence > INT_MAX) return -1;
    if (sequence != *next_sequence) return -1;
    if (*next_sequence == UINT32_MAX) return -1;
    *next_sequence += 1;
    return 0;
}

static int fluxer_apng_read_chunk(
    struct fluxer_apng_validation *validation,
    struct fluxer_apng_chunk *chunk
) {
    assert(validation != NULL);
    assert(chunk != NULL);
    if (validation->chunk_count >= FLUXER_MAX_APNG_CHUNKS) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    if (validation->offset > validation->len) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    size_t remaining = validation->len - validation->offset;
    if (remaining < 12) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    uint32_t length = fluxer_read_be32(validation->data + validation->offset);
    if ((size_t)length > remaining - 12) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    chunk->length = length;
    chunk->type = validation->data + validation->offset + 4;
    chunk->payload = validation->data + validation->offset + 8;
    chunk->next_offset = validation->offset + 12 + (size_t)length;
    validation->chunk_count++;
    if (!fluxer_png_chunk_type_valid(chunk->type)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!fluxer_png_chunk_crc_valid(validation->crc_table, chunk)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_apng_accept_ihdr(
    struct fluxer_apng_validation *validation,
    const struct fluxer_apng_chunk *chunk,
    int is_ihdr
) {
    assert(validation != NULL);
    assert(chunk != NULL);
    if (validation->chunk_index != 0) {
        if (is_ihdr) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        return FLUXER_NATIVE_STATUS_OK;
    }
    if (!is_ihdr) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (chunk->length != 13) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!fluxer_png_ihdr_valid(chunk->payload)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    uint32_t width = fluxer_read_be32(chunk->payload);
    uint32_t height = fluxer_read_be32(chunk->payload + 4);
    if (width == 0 || height == 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (width > FLUXER_MAX_VIDEO_FRAME_DIMENSION) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (height > FLUXER_MAX_VIDEO_FRAME_DIMENSION) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if ((size_t)width > FLUXER_MAX_VIDEO_PIXELS / (size_t)height) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    validation->width = width;
    validation->height = height;
    validation->color_type = chunk->payload[9];
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_apng_accept_actl(
    struct fluxer_apng_validation *validation,
    const struct fluxer_apng_chunk *chunk
) {
    assert(validation != NULL);
    assert(chunk != NULL);
    if (validation->saw_actl) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->saw_idat) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->frame_controls != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (chunk->length != 8) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    uint32_t declared_frames = fluxer_read_be32(chunk->payload);
    uint32_t declared_plays = fluxer_read_be32(chunk->payload + 4);
    if (declared_frames == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (declared_frames > INT_MAX) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->max_frames > 0) {
        if (declared_frames > (uint32_t)validation->max_frames) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
    }
    validation->declared_frames = declared_frames;
    validation->declared_plays = declared_plays;
    validation->saw_actl = 1;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_apng_frame_geometry_valid(
    const struct fluxer_apng_validation *validation,
    const uint8_t *payload
) {
    assert(validation != NULL);
    assert(payload != NULL);
    uint32_t frame_width = fluxer_read_be32(payload + 4);
    uint32_t frame_height = fluxer_read_be32(payload + 8);
    uint32_t frame_x = fluxer_read_be32(payload + 12);
    uint32_t frame_y = fluxer_read_be32(payload + 16);
    if (frame_width == 0 || frame_height == 0) return 0;
    if (frame_x > validation->width) return 0;
    if (frame_y > validation->height) return 0;
    if (frame_width > validation->width - frame_x) return 0;
    if (frame_height > validation->height - frame_y) return 0;
    if (validation->frame_controls == 0 && !validation->saw_idat) {
        if (frame_width != validation->width) return 0;
        if (frame_height != validation->height) return 0;
        if (frame_x != 0) return 0;
        if (frame_y != 0) return 0;
    }
    return 1;
}

static int fluxer_apng_accept_fctl(
    struct fluxer_apng_validation *validation,
    const struct fluxer_apng_chunk *chunk
) {
    assert(validation != NULL);
    assert(chunk != NULL);
    if (!validation->saw_actl) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (chunk->length != 26) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->frame_controls >= validation->declared_frames) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (validation->active_frame && !validation->active_frame_has_data) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (fluxer_apng_sequence_advance(
            chunk->payload, &validation->next_sequence) != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!fluxer_apng_frame_geometry_valid(validation, chunk->payload)) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (chunk->payload[24] > 2) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (chunk->payload[25] > 1) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    validation->active_frame = 1;
    validation->active_frame_has_data = 0;
    if (validation->saw_idat) {
        validation->active_frame_data = FLUXER_APNG_FDAT_FRAME_DATA;
    } else {
        validation->active_frame_data = FLUXER_APNG_IDAT_FRAME_DATA;
    }
    validation->frame_controls++;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_apng_accept_plte(
    struct fluxer_apng_validation *validation,
    const struct fluxer_apng_chunk *chunk
) {
    assert(validation != NULL);
    assert(chunk != NULL);
    if (validation->saw_plte) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->saw_idat) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (chunk->length == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (chunk->length > 768) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (chunk->length % 3 != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->color_type == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->color_type == 4) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    validation->saw_plte = 1;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_apng_accept_idat(
    struct fluxer_apng_validation *validation,
    const struct fluxer_apng_chunk *chunk
) {
    assert(validation != NULL);
    assert(chunk != NULL);
    if (!validation->saw_actl) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->idat_state == FLUXER_PNG_AFTER_IDAT) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (validation->saw_fdat) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->color_type == 3 && !validation->saw_plte) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (validation->active_frame) {
        if (validation->active_frame_data == FLUXER_APNG_FDAT_FRAME_DATA) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
    }
    validation->idat_state = FLUXER_PNG_READING_IDAT;
    validation->saw_idat = 1;
    if (chunk->length == 0) return FLUXER_NATIVE_STATUS_OK;
    validation->saw_idat_payload = 1;
    if (validation->active_frame) {
        if (validation->active_frame_data == FLUXER_APNG_IDAT_FRAME_DATA) {
            validation->active_frame_has_data = 1;
        }
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_apng_accept_fdat(
    struct fluxer_apng_validation *validation,
    const struct fluxer_apng_chunk *chunk
) {
    assert(validation != NULL);
    assert(chunk != NULL);
    if (!validation->saw_actl) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (chunk->length <= 4) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!validation->saw_idat) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->idat_state != FLUXER_PNG_AFTER_IDAT) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!validation->active_frame) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->active_frame_data != FLUXER_APNG_FDAT_FRAME_DATA) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (fluxer_apng_sequence_advance(
            chunk->payload, &validation->next_sequence) != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    validation->active_frame_has_data = 1;
    validation->saw_fdat = 1;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_apng_accept_iend(
    struct fluxer_apng_validation *validation,
    const struct fluxer_apng_chunk *chunk
) {
    assert(validation != NULL);
    assert(chunk != NULL);
    if (chunk->length != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (chunk->next_offset != validation->len) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (!validation->active_frame) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!validation->active_frame_has_data) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    validation->saw_iend = 1;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_apng_process_chunk(
    struct fluxer_apng_validation *validation,
    const struct fluxer_apng_chunk *chunk
) {
    assert(validation != NULL);
    assert(chunk != NULL);
    int is_ihdr = memcmp(chunk->type, "IHDR", 4) == 0;
    int is_plte = memcmp(chunk->type, "PLTE", 4) == 0;
    int is_idat = memcmp(chunk->type, "IDAT", 4) == 0;
    int is_iend = memcmp(chunk->type, "IEND", 4) == 0;
    int known_critical = is_ihdr || is_plte || is_idat || is_iend;
    if ((chunk->type[0] & 0x20u) == 0 && !known_critical) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (validation->idat_state == FLUXER_PNG_READING_IDAT && !is_idat) {
        validation->idat_state = FLUXER_PNG_AFTER_IDAT;
    }
    int status = fluxer_apng_accept_ihdr(validation, chunk, is_ihdr);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (memcmp(chunk->type, "acTL", 4) == 0) {
        return fluxer_apng_accept_actl(validation, chunk);
    }
    if (memcmp(chunk->type, "fcTL", 4) == 0) {
        return fluxer_apng_accept_fctl(validation, chunk);
    }
    if (is_plte) return fluxer_apng_accept_plte(validation, chunk);
    if (is_idat) return fluxer_apng_accept_idat(validation, chunk);
    if (memcmp(chunk->type, "fdAT", 4) == 0) {
        return fluxer_apng_accept_fdat(validation, chunk);
    }
    if (is_iend) return fluxer_apng_accept_iend(validation, chunk);
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_apng_finish_validation(
    const struct fluxer_apng_validation *validation,
    int *out_width,
    int *out_height,
    int *out_expected_frames,
    uint32_t *out_num_plays
) {
    assert(validation != NULL);
    assert(out_expected_frames != NULL);
    if (!validation->saw_actl) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!validation->saw_iend) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!validation->saw_idat) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!validation->saw_idat_payload) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (validation->offset != validation->len) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (validation->frame_controls != validation->declared_frames) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (validation->color_type == 3 && !validation->saw_plte) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if ((size_t)validation->width > SIZE_MAX / (size_t)validation->height) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t frame_pixels =
        (size_t)validation->width * (size_t)validation->height;
    if (frame_pixels == 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (frame_pixels > SIZE_MAX / 4u) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (validation->max_total_pixels > 0) {
        if (frame_pixels > validation->max_total_pixels) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        if ((size_t)validation->declared_frames >
            validation->max_total_pixels / frame_pixels) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
    }
    if (out_width != NULL) *out_width = (int)validation->width;
    if (out_height != NULL) *out_height = (int)validation->height;
    *out_expected_frames = (int)validation->declared_frames;
    if (out_num_plays != NULL) *out_num_plays = validation->declared_plays;
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_validate_complete_apng(
    const uint8_t *data,
    size_t len,
    int max_frames,
    size_t max_total_pixels,
    int *out_width,
    int *out_height,
    int *out_expected_frames,
    uint32_t *out_num_plays
) {
    static const uint8_t signature[8] = {
        0x89, 'P', 'N', 'G', '\r', '\n', 0x1a, '\n'
    };
    if (out_width != NULL) *out_width = 0;
    if (out_height != NULL) *out_height = 0;
    if (out_expected_frames == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *out_expected_frames = 0;
    if (out_num_plays != NULL) *out_num_plays = 0;
    if (data == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (len < sizeof(signature)) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (max_frames < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (memcmp(data, signature, sizeof(signature)) != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }

    struct fluxer_apng_validation validation = {
        .data = data,
        .len = len,
        .offset = sizeof(signature),
        .max_frames = max_frames,
        .max_total_pixels = max_total_pixels,
        .idat_state = FLUXER_PNG_BEFORE_IDAT,
        .active_frame_data = FLUXER_APNG_NO_FRAME_DATA,
        .crc_table = av_crc_get_table(AV_CRC_32_IEEE_LE),
    };
    if (validation.crc_table == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    while (validation.offset < validation.len) {
        struct fluxer_apng_chunk chunk;
        int status = fluxer_apng_read_chunk(&validation, &chunk);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
        status = fluxer_apng_process_chunk(&validation, &chunk);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
        validation.offset = chunk.next_offset;
        validation.chunk_index++;
    }
    return fluxer_apng_finish_validation(
        &validation, out_width, out_height, out_expected_frames,
        out_num_plays);
}

int fluxer_apng_probe(
    const void *buffer,
    size_t len,
    int max_frames,
    size_t max_total_pixels,
    int *width,
    int *height,
    int *frames
) {
    if (width != NULL) *width = 0;
    if (height != NULL) *height = 0;
    if (frames != NULL) *frames = 0;
    if (width == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (height == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (frames == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    return fluxer_validate_complete_apng(
        buffer, len, max_frames, max_total_pixels, width, height, frames,
        NULL);
}
