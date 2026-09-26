// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

static int skip_gif_image_block(const uint8_t *bytes, size_t len,
                                size_t *offset, size_t *block_count) {
    if (bytes == NULL || offset == NULL || block_count == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (*offset > len || len - *offset < 9) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    uint8_t packed = bytes[*offset + 8];
    *offset += 9;
    if ((packed & 0x80) != 0) {
        size_t table_bytes = ((size_t)1 << ((packed & 0x07) + 1)) * 3u;
        if (table_bytes > len - *offset) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        *offset += table_bytes;
    }
    if (*offset >= len) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    uint8_t minimum_code_size = bytes[*offset];
    *offset += 1;
    if (minimum_code_size < 2 || minimum_code_size > 8) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return fluxer_skip_gif_sub_blocks_checked(bytes, len, offset, block_count);
}

int fluxer_patch_gif_frame_delays(uint8_t *bytes, size_t len,
                                  const int *delays_cs, int n_delays) {
    if (bytes == NULL || len < 14 || delays_cs == NULL || n_delays <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    size_t offset = 13;
    if ((bytes[10] & 0x80) != 0) {
        size_t table_bytes = ((size_t)1 << ((bytes[10] & 0x07) + 1)) * 3u;
        if (table_bytes > len - offset) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        offset += table_bytes;
    }
    size_t pending_delay_offset = SIZE_MAX;
    size_t block_count = 0;
    int patched = 0;
    while (offset < len) {
        if (block_count >= FLUXER_MAX_GIF_STRUCTURE_BLOCKS) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        block_count++;
        uint8_t introducer = bytes[offset++];
        if (introducer == 0x3b) {
            if (offset != len || pending_delay_offset != SIZE_MAX || patched != n_delays) {
                return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            }
            return FLUXER_NATIVE_STATUS_OK;
        }
        if (introducer == 0x21) {
            if (offset >= len) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            uint8_t label = bytes[offset++];
            if (label == 0xf9) {
                if (pending_delay_offset != SIZE_MAX || len - offset < 6) {
                    return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
                }
                if (bytes[offset] != 4 || bytes[offset + 5] != 0) {
                    return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
                }
                pending_delay_offset = offset + 2;
                offset += 6;
                continue;
            }
            if (label == 0x01 && pending_delay_offset != SIZE_MAX) {
                return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            }
            int skip_status = fluxer_skip_gif_sub_blocks_checked(
                bytes, len, &offset, &block_count);
            if (skip_status != FLUXER_NATIVE_STATUS_OK) return skip_status;
            continue;
        }
        if (introducer != 0x2c || pending_delay_offset == SIZE_MAX || patched >= n_delays) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        int skip_status = skip_gif_image_block(bytes, len, &offset, &block_count);
        if (skip_status != FLUXER_NATIVE_STATUS_OK) return skip_status;
        int delay = delays_cs[patched];
        if (delay < 1 || delay > 65535) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        bytes[pending_delay_offset] = (uint8_t)(delay & 0xff);
        bytes[pending_delay_offset + 1] = (uint8_t)((delay >> 8) & 0xff);
        pending_delay_offset = SIZE_MAX;
        patched++;
    }
    return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
}

static int read_gif_graphics_control_delay(const uint8_t *bytes, size_t len,
                                           size_t *offset, int *delay_cs) {
    if (bytes == NULL || offset == NULL || delay_cs == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (*offset > len || len - *offset < 6) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (bytes[*offset] != 4 || bytes[*offset + 5] != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int delay = (int)fluxer_gif_read_le16(bytes + *offset + 2);
    if (delay <= 0) {
        delay = FLUXER_DEFAULT_ANIMATION_FRAME_DELAY_MS / 10;
    } else if (delay * 10 < FLUXER_MIN_ANIMATION_FRAME_DELAY_MS) {
        delay = FLUXER_MIN_ANIMATION_FRAME_DELAY_MS / 10;
    }
    *delay_cs = delay;
    *offset += 6;
    return FLUXER_NATIVE_STATUS_OK;
}

static int read_gif_application_loop(const uint8_t *bytes, size_t len,
                                     size_t *offset, size_t *block_count,
                                     int *loop_count, int *has_loop) {
    static const uint8_t netscape_id[] = {
        'N', 'E', 'T', 'S', 'C', 'A', 'P', 'E', '2', '.', '0'
    };
    static const uint8_t animexts_id[] = {
        'A', 'N', 'I', 'M', 'E', 'X', 'T', 'S', '1', '.', '0'
    };
    if (bytes == NULL || offset == NULL || block_count == NULL ||
        loop_count == NULL || has_loop == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (*offset >= len) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (*block_count >= FLUXER_MAX_GIF_STRUCTURE_BLOCKS) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    (*block_count)++;
    uint8_t application_len = bytes[(*offset)++];
    if ((size_t)application_len > len - *offset) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int recognized = application_len == sizeof(netscape_id) &&
        (memcmp(bytes + *offset, netscape_id, sizeof(netscape_id)) == 0 ||
         memcmp(bytes + *offset, animexts_id, sizeof(animexts_id)) == 0);
    *offset += application_len;
    if (!recognized) {
        if (application_len == 0) return FLUXER_NATIVE_STATUS_OK;
        return fluxer_skip_gif_sub_blocks_checked(
            bytes, len, offset, block_count);
    }
    if (*has_loop) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (*offset >= len) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (*block_count >= FLUXER_MAX_GIF_STRUCTURE_BLOCKS) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    (*block_count)++;
    uint8_t value_len = bytes[(*offset)++];
    if (value_len != 3 || len - *offset < 3) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (bytes[*offset] != 1) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *loop_count = (int)fluxer_gif_read_le16(bytes + *offset + 1);
    *offset += 3;
    if (*offset >= len) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (*block_count >= FLUXER_MAX_GIF_STRUCTURE_BLOCKS) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    (*block_count)++;
    if (bytes[(*offset)++] != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *has_loop = 1;
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_read_gif_frame_delays(const uint8_t *bytes, size_t len,
                                 int *delays_cs, int expected_frames,
                                 int max_duration_ms,
                                 int *out_loop_count) {
    if (out_loop_count != NULL) *out_loop_count = -1;
    if (bytes == NULL || len < 14 || expected_frames <= 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    size_t offset = 13;
    if ((bytes[10] & 0x80) != 0) {
        size_t table_bytes = ((size_t)1 << ((bytes[10] & 0x07) + 1)) * 3u;
        if (table_bytes > len - offset) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        offset += table_bytes;
    }
    size_t block_count = 0;
    int pending_delay_cs = 0;
    int has_pending_delay = 0;
    int loop_count = -1;
    int has_loop = 0;
    int frame_count = 0;
    int64_t duration_ms = 0;
    while (offset < len) {
        if (block_count >= FLUXER_MAX_GIF_STRUCTURE_BLOCKS) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        block_count++;
        uint8_t introducer = bytes[offset++];
        if (introducer == 0x3b) {
            if (offset != len || has_pending_delay || frame_count != expected_frames) {
                return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            }
            if (out_loop_count != NULL) *out_loop_count = loop_count;
            return FLUXER_NATIVE_STATUS_OK;
        }
        if (introducer == 0x21) {
            if (offset >= len) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            uint8_t label = bytes[offset++];
            if (label == 0xf9) {
                if (has_pending_delay) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
                int status = read_gif_graphics_control_delay(
                    bytes, len, &offset, &pending_delay_cs);
                if (status != FLUXER_NATIVE_STATUS_OK) return status;
                has_pending_delay = 1;
                continue;
            }
            if (label == 0xff) {
                int status = read_gif_application_loop(
                    bytes, len, &offset, &block_count,
                    &loop_count, &has_loop);
                if (status != FLUXER_NATIVE_STATUS_OK) return status;
                continue;
            }
            if (label == 0x01) has_pending_delay = 0;
            int status = fluxer_skip_gif_sub_blocks_checked(
                bytes, len, &offset, &block_count);
            if (status != FLUXER_NATIVE_STATUS_OK) return status;
            continue;
        }
        if (introducer != 0x2c || frame_count >= expected_frames) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        int status = skip_gif_image_block(bytes, len, &offset, &block_count);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
        /* A frame with no Graphics Control Extension declares no delay, which renders at the
         * 100 ms browser default rather than the fast-frame minimum. */
        int delay_cs = has_pending_delay
                     ? pending_delay_cs
                     : FLUXER_DEFAULT_ANIMATION_FRAME_DELAY_MS / 10;
        if (duration_ms > INT64_MAX - (int64_t)delay_cs * 10) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        duration_ms += (int64_t)delay_cs * 10;
        if (max_duration_ms > 0 && duration_ms > max_duration_ms) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        if (delays_cs != NULL) delays_cs[frame_count] = delay_cs;
        frame_count++;
        has_pending_delay = 0;
    }
    return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
}

int fluxer_gif_animation_frame_budget(const int *delays_cs, int n_frames,
                                      int max_frames, int max_duration_ms,
                                      int *out_frames) {
    if (delays_cs == NULL || n_frames <= 0 || max_frames <= 0 ||
        max_duration_ms <= 0 || out_frames == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *out_frames = 0;
    int64_t duration_ms = 0;
    int frames = 0;
    for (int i = 0; i < n_frames; i++) {
        if (frames >= max_frames) break;
        if (duration_ms >= (int64_t)max_duration_ms) break;
        int delay_cs = delays_cs[i];
        if (delay_cs <= 0 || delay_cs > 65535) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        if (duration_ms > INT64_MAX - (int64_t)delay_cs * 10) {
            return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
        }
        duration_ms += (int64_t)delay_cs * 10;
        frames++;
    }
    if (frames <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out_frames = frames;
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_validate_gif_animation(const void *gif_data, size_t gif_len,
                                  int max_frames, int max_duration_ms,
                                  size_t max_total_pixels) {
    if (max_duration_ms <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    int expected_frames = 0;
    int status = fluxer_validate_complete_gif(
        gif_data, gif_len, max_frames, max_total_pixels, &expected_frames);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    return fluxer_read_gif_frame_delays(
        gif_data, gif_len, NULL, expected_frames, max_duration_ms, NULL);
}
