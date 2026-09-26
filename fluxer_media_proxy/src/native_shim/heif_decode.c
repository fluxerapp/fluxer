// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

static void fluxer_clamp_heif_u64(uint64_t *value, uint64_t limit) {
    if (*value == 0 || *value > limit) *value = limit;
}

static void fluxer_clamp_heif_u32(uint32_t *value, uint32_t limit) {
    if (*value == 0 || *value > limit) *value = limit;
}

static int fluxer_set_heif_security_limits(struct heif_context *ctx,
                                           size_t max_total_pixels) {
    if (ctx == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    struct heif_security_limits *limits = heif_context_get_security_limits(ctx);
    if (limits == NULL || limits->version < 1) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    uint64_t pixel_limit = max_total_pixels > 0
                         ? (uint64_t)max_total_pixels
                         : (uint64_t)FLUXER_MAX_VIDEO_PIXELS;
    fluxer_clamp_heif_u64(&limits->max_image_size_pixels, pixel_limit);
    fluxer_clamp_heif_u64(&limits->max_number_of_tiles, 4096);
    fluxer_clamp_heif_u32(&limits->max_bayer_pattern_pixels, 4096);
    fluxer_clamp_heif_u32(&limits->max_items, 4096);
    fluxer_clamp_heif_u32(&limits->max_color_profile_size, 4u * 1024u * 1024u);
    fluxer_clamp_heif_u64(&limits->max_memory_block_size, 512u * 1024u * 1024u);
    fluxer_clamp_heif_u32(&limits->max_components, 64);
    fluxer_clamp_heif_u32(&limits->max_iloc_extents_per_item, 4096);
    fluxer_clamp_heif_u32(&limits->max_size_entity_group, 4096);
    fluxer_clamp_heif_u32(&limits->max_children_per_box, 65536);
    return FLUXER_NATIVE_STATUS_OK;
}

#define FLUXER_BMFF_MAX_DEPTH 32u
#define FLUXER_BMFF_MAX_BOXES 65536u
#define FLUXER_BMFF_MAX_IINF_ENTRIES 16384u
#define FLUXER_BMFF_MAX_ILOC_ITEMS 4096u
#define FLUXER_BMFF_MAX_ILOC_EXTENTS_PER_ITEM 4096u
#define FLUXER_BMFF_MAX_ILOC_EXTENTS_TOTAL 65536u

static uint32_t bmff_read_u32(const uint8_t *p) {
    return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
           ((uint32_t)p[2] <<  8) |  (uint32_t)p[3];
}
static uint16_t bmff_read_u16(const uint8_t *p) {
    return (uint16_t)(((uint16_t)p[0] << 8) | (uint16_t)p[1]);
}
static uint64_t bmff_read_u64(const uint8_t *p) {
    return ((uint64_t)bmff_read_u32(p) << 32) | (uint64_t)bmff_read_u32(p + 4);
}

typedef struct {
    size_t next;
    size_t end;
    uint8_t type[4];
    size_t payload_start;
} bmff_frame;

typedef struct {
    uint8_t type[4];
    size_t payload_start;
    size_t payload_end;
} bmff_box;

typedef struct {
    const uint8_t *data;
    size_t data_len;
    bmff_frame frames[FLUXER_BMFF_MAX_DEPTH];
    size_t depth;
    size_t *box_count;
    uint8_t parent_type[4];
    size_t parent_payload_start;
} bmff_cursor;

typedef enum {
    BMFF_ITEM_BOX_OTHER = 0,
    BMFF_ITEM_BOX_IINF,
    BMFF_ITEM_BOX_ILOC,
    BMFF_ITEM_BOX_IDAT,
} bmff_item_box_kind;
static int bmff_type_is(const uint8_t type[4], const char *expected) {
    return memcmp(type, expected, 4) == 0;
}

static int bmff_container_kind(const uint8_t type[4]) {
    if (bmff_type_is(type, "meta")) return 2;
    if (bmff_type_is(type, "moov") || bmff_type_is(type, "trak") ||
        bmff_type_is(type, "mdia") || bmff_type_is(type, "minf") ||
        bmff_type_is(type, "stbl") || bmff_type_is(type, "edts") ||
        bmff_type_is(type, "dinf")) {
        return 1;
    }
    return 0;
}

static int bmff_parse_box(const uint8_t *data, size_t data_len,
                          size_t start, size_t end, bmff_box *out) {
    if (data == NULL || out == NULL || start > end || end > data_len) return -1;
    size_t remaining = end - start;
    if (remaining < 8) return -1;

    uint32_t size32 = bmff_read_u32(data + start);
    uint64_t box_size = size32;
    size_t header_len = 8;
    if (size32 == 1) {
        if (remaining < 16) return -1;
        box_size = bmff_read_u64(data + start + 8);
        header_len = 16;
    } else if (size32 == 0) {
        box_size = (uint64_t)remaining;
    }
    if (box_size < (uint64_t)header_len || box_size > (uint64_t)remaining) return -1;

    size_t parsed_size = (size_t)box_size;
    memcpy(out->type, data + start + 4, sizeof(out->type));
    out->payload_start = start + header_len;
    out->payload_end = start + parsed_size;
    return 0;
}
static int bmff_cursor_init(bmff_cursor *cursor, const uint8_t *data,
                            size_t data_len, size_t start, size_t end,
                            size_t *box_count) {
    if (cursor == NULL || data == NULL || box_count == NULL) return -1;
    if (start > end || end > data_len) return -1;

    memset(cursor, 0, sizeof(*cursor));
    cursor->data = data;
    cursor->data_len = data_len;
    cursor->frames[0].next = start;
    cursor->frames[0].end = end;
    cursor->depth = 1;
    cursor->box_count = box_count;
    return 0;
}

static int bmff_cursor_parent_is(const bmff_cursor *cursor,
                                 const char *expected) {
    if (cursor == NULL) return 0;
    if (expected == NULL) return 0;
    return bmff_type_is(cursor->parent_type, expected);
}
static bmff_item_box_kind bmff_item_box_kind_from_type(const uint8_t type[4]) {
    if (bmff_type_is(type, "iinf")) return BMFF_ITEM_BOX_IINF;
    if (bmff_type_is(type, "iloc")) return BMFF_ITEM_BOX_ILOC;
    if (bmff_type_is(type, "idat")) return BMFF_ITEM_BOX_IDAT;
    return BMFF_ITEM_BOX_OTHER;
}

static int bmff_cursor_next(bmff_cursor *cursor, bmff_box *out) {
    if (cursor == NULL || out == NULL || cursor->data == NULL ||
        cursor->box_count == NULL) return -1;
    while (cursor->depth > 0) {
        bmff_frame *frame = &cursor->frames[cursor->depth - 1];
        if (frame->next == frame->end) {
            cursor->depth--;
            continue;
        }
        if (frame->next > frame->end) return -1;
        if (*cursor->box_count >= FLUXER_BMFF_MAX_BOXES) return -1;
        if (bmff_parse_box(cursor->data, cursor->data_len,
                           frame->next, frame->end, out) != 0) return -1;
        frame->next = out->payload_end;
        (*cursor->box_count)++;
        memcpy(cursor->parent_type, frame->type, 4);
        cursor->parent_payload_start = frame->payload_start;

        int container_kind = bmff_container_kind(out->type);
        if (container_kind != 0) {
            size_t child_start = out->payload_start;
            if (container_kind == 2) {
                if (out->payload_end - child_start < 4) return -1;
                child_start += 4;
            }
            if (child_start < out->payload_end) {
                if (cursor->depth >= FLUXER_BMFF_MAX_DEPTH) return -1;
                cursor->frames[cursor->depth].next = child_start;
                cursor->frames[cursor->depth].end = out->payload_end;
                memcpy(cursor->frames[cursor->depth].type, out->type, 4);
                cursor->frames[cursor->depth].payload_start = out->payload_start;
                cursor->depth++;
            }
        }
        return 1;
    }
    return 0;
}

static int bmff_infe_has_tmap(const uint8_t *payload, size_t len, int *found) {
    if (payload == NULL || found == NULL || len < 4) return -1;
    *found = 0;
    uint8_t version = payload[0];
    if (version == 0 || version == 1) return 0;
    if (version != 2 && version != 3) return -1;

    size_t pos = 4;
    size_t item_id_len = version == 2 ? 2 : 4;
    if (len - pos < item_id_len) return -1;
    pos += item_id_len;
    if (len - pos < 6) return -1;
    pos += 2;
    if (memcmp(payload + pos, "tmap", 4) == 0) *found = 1;
    return 0;
}

static int bmff_parse_iinf(const uint8_t *payload, size_t len,
                           size_t *box_count, size_t *iinf_entry_count,
                           long long deadline_monotonic_ms,
                           int *found) {
    if (payload == NULL || box_count == NULL || iinf_entry_count == NULL ||
        found == NULL || len < 4) return -1;
    uint8_t version = payload[0];
    size_t off = 4;
    uint32_t entry_count;
    if (version == 0) {
        if (len - off < 2) return -1;
        entry_count = bmff_read_u16(payload + off);
        off += 2;
    } else {
        if (len - off < 4) return -1;
        entry_count = bmff_read_u32(payload + off);
        off += 4;
    }
    if (*iinf_entry_count > FLUXER_BMFF_MAX_IINF_ENTRIES) return -1;
    if ((size_t)entry_count > FLUXER_BMFF_MAX_IINF_ENTRIES - *iinf_entry_count) return -1;
    if ((size_t)entry_count > (len - off) / 8) return -1;

    for (uint32_t index = 0; index < entry_count; index++) {
        int deadline_status = fluxer_native_deadline_status(
            deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        if (*box_count >= FLUXER_BMFF_MAX_BOXES) return -1;
        bmff_box box;
        if (bmff_parse_box(payload, len, off, len, &box) != 0) return -1;
        (*box_count)++;
        (*iinf_entry_count)++;
        if (!bmff_type_is(box.type, "infe")) return -1;
        int entry_found = 0;
        if (bmff_infe_has_tmap(payload + box.payload_start,
                               box.payload_end - box.payload_start,
                               &entry_found) != 0) return -1;
        if (entry_found) *found = 1;
        off = box.payload_end;
    }
    return off == len ? 0 : -1;
}

static int bmff_read_sized_uint(const uint8_t *data, size_t len,
                                size_t *position, uint8_t width,
                                uint64_t *value) {
    if (data == NULL || position == NULL || value == NULL || width > 8) return -1;
    if (*position > len || (size_t)width > len - *position) return -1;
    uint64_t parsed = 0;
    for (uint8_t index = 0; index < width; index++) {
        parsed = (parsed << 8) | data[*position + index];
    }
    *position += width;
    *value = parsed;
    return 0;
}

static int bmff_iloc_extent_is_in_bounds(uint64_t base_offset,
                                         uint64_t extent_offset,
                                         uint64_t extent_length,
                                         size_t data_len) {
    if (base_offset > UINT64_MAX - extent_offset) return 0;
    uint64_t start = base_offset + extent_offset;
    if (start > UINT64_MAX - extent_length) return 0;
    return start + extent_length <= (uint64_t)data_len;
}

static int bmff_iloc_field_width_is_valid(uint8_t width) {
    return width == 0 || width == 4 || width == 8;
}

typedef struct {
    const uint8_t *payload;
    size_t len;
    size_t position;
    size_t data_len;
    size_t idat_len;
    size_t total_extent_count;
    uint8_t version;
    uint8_t offset_size;
    uint8_t length_size;
    uint8_t base_offset_size;
    uint8_t index_size;
    int has_idat;
} bmff_iloc_validation;

static int bmff_iloc_validation_init(
    bmff_iloc_validation *validation,
    const uint8_t *payload,
    size_t len,
    size_t data_len,
    int has_idat,
    size_t idat_len,
    uint64_t *item_count
) {
    if (validation == NULL || payload == NULL || item_count == NULL) return -1;
    if (len < 8 || has_idat < 0 || has_idat > 1) return -1;
    uint8_t version = payload[0];
    if (version > 2) return -1;
    uint8_t offset_size = payload[4] >> 4;
    uint8_t length_size = payload[4] & 0x0f;
    uint8_t base_offset_size = payload[5] >> 4;
    uint8_t encoded_index_size = payload[5] & 0x0f;
    if (!bmff_iloc_field_width_is_valid(offset_size) ||
        !bmff_iloc_field_width_is_valid(length_size) ||
        !bmff_iloc_field_width_is_valid(base_offset_size)) {
        return -1;
    }
    if (version == 0 && encoded_index_size != 0) return -1;
    if (version > 0 &&
        !bmff_iloc_field_width_is_valid(encoded_index_size)) return -1;
    *validation = (bmff_iloc_validation) {
        .payload = payload,
        .len = len,
        .position = 6,
        .data_len = data_len,
        .idat_len = idat_len,
        .version = version,
        .offset_size = offset_size,
        .length_size = length_size,
        .base_offset_size = base_offset_size,
        .index_size = version == 0 ? 0 : encoded_index_size,
        .has_idat = has_idat,
    };
    uint8_t count_size = version == 2 ? 4 : 2;
    if (bmff_read_sized_uint(
            payload, len, &validation->position,
            count_size, item_count) != 0) return -1;
    return *item_count <= FLUXER_BMFF_MAX_ILOC_ITEMS ? 0 : -1;
}

static int bmff_validate_iloc_extents(
    bmff_iloc_validation *validation,
    uint64_t extent_count,
    uint64_t base_offset,
    size_t source_len,
    long long deadline_monotonic_ms
) {
    assert(validation != NULL);
    uint64_t item_extent_bytes = 0;
    for (uint64_t index = 0; index < extent_count; index++) {
        int deadline_status = fluxer_native_deadline_status(
            deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        uint64_t ignored = 0;
        if (bmff_read_sized_uint(
                validation->payload, validation->len,
                &validation->position, validation->index_size,
                &ignored) != 0) return -1;
        uint64_t extent_offset = 0;
        uint64_t extent_length = 0;
        if (bmff_read_sized_uint(
                validation->payload, validation->len,
                &validation->position, validation->offset_size,
                &extent_offset) != 0) return -1;
        if (bmff_read_sized_uint(
                validation->payload, validation->len,
                &validation->position, validation->length_size,
                &extent_length) != 0) return -1;
        if (!bmff_iloc_extent_is_in_bounds(
                base_offset, extent_offset, extent_length, source_len)) {
            return -1;
        }
        if (extent_length > (uint64_t)source_len ||
            item_extent_bytes > (uint64_t)source_len - extent_length) {
            return -1;
        }
        item_extent_bytes += extent_length;
    }
    return 0;
}

static int bmff_validate_iloc_item(
    bmff_iloc_validation *validation,
    long long deadline_monotonic_ms
) {
    assert(validation != NULL);
    uint64_t ignored = 0;
    uint8_t item_id_size = validation->version == 2 ? 4 : 2;
    if (bmff_read_sized_uint(
            validation->payload, validation->len, &validation->position,
            item_id_size, &ignored) != 0) return -1;
    uint64_t construction_method = 0;
    if (validation->version > 0) {
        if (bmff_read_sized_uint(
                validation->payload, validation->len,
                &validation->position, 2, &construction_method) != 0) {
            return -1;
        }
        if ((construction_method & UINT64_C(0xfff0)) != 0 ||
            construction_method > 1) return -1;
    }
    uint64_t data_reference_index = 0;
    if (bmff_read_sized_uint(
            validation->payload, validation->len, &validation->position,
            2, &data_reference_index) != 0 || data_reference_index != 0) {
        return -1;
    }
    uint64_t base_offset = 0;
    if (bmff_read_sized_uint(
            validation->payload, validation->len, &validation->position,
            validation->base_offset_size, &base_offset) != 0) return -1;
    uint64_t extent_count = 0;
    if (bmff_read_sized_uint(
            validation->payload, validation->len, &validation->position,
            2, &extent_count) != 0) return -1;
    if (extent_count > FLUXER_BMFF_MAX_ILOC_EXTENTS_PER_ITEM ||
        (size_t)extent_count > FLUXER_BMFF_MAX_ILOC_EXTENTS_TOTAL -
                               validation->total_extent_count) return -1;
    validation->total_extent_count += (size_t)extent_count;
    size_t source_len = validation->data_len;
    if (construction_method == 1) {
        if (!validation->has_idat) return -1;
        source_len = validation->idat_len;
    }
    return bmff_validate_iloc_extents(
        validation, extent_count, base_offset, source_len,
        deadline_monotonic_ms);
}

static int bmff_validate_iloc_payload(const uint8_t *payload, size_t len,
                                      size_t data_len, int has_idat,
                                      size_t idat_len,
                                      long long deadline_monotonic_ms) {
    bmff_iloc_validation validation;
    uint64_t item_count = 0;
    if (bmff_iloc_validation_init(
            &validation, payload, len, data_len, has_idat, idat_len,
            &item_count) != 0) return -1;
    for (uint64_t index = 0; index < item_count; index++) {
        int status = bmff_validate_iloc_item(
            &validation, deadline_monotonic_ms);
        if (status != 0) return status;
    }
    return validation.position == len ? 0 : -1;
}

static int validate_isobmff_item_metadata(const void *buf, size_t len,
                                          long long deadline_monotonic_ms,
                                          int *has_tmap_item) {
    if (buf == NULL) return -1;
    if (len < 16) return -1;
    if (has_tmap_item == NULL) return -1;
    *has_tmap_item = 0;
    const uint8_t *data = (const uint8_t *)buf;
    size_t box_count = 0;
    size_t iinf_entry_count = 0;
    size_t iinf_count = 0;
    size_t idat_count = 0;
    size_t metadata_parent = SIZE_MAX;
    size_t idat_len = 0;
    const uint8_t *iloc_payload = NULL;
    size_t iloc_payload_len = 0;
    bmff_cursor cursor;
    if (bmff_cursor_init(&cursor, data, len, 0, len, &box_count) != 0) return -1;
    for (;;) {
        int deadline_status = fluxer_native_deadline_status(
            deadline_monotonic_ms);
        if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
            return deadline_status;
        }
        bmff_box box;
        int result = bmff_cursor_next(&cursor, &box);
        if (result < 0) return -1;
        if (result == 0) break;
        bmff_item_box_kind kind = bmff_item_box_kind_from_type(box.type);
        if (kind == BMFF_ITEM_BOX_OTHER) continue;
        if (!bmff_cursor_parent_is(&cursor, "meta")) return -1;
        if (metadata_parent == SIZE_MAX) {
            metadata_parent = cursor.parent_payload_start;
        } else if (metadata_parent != cursor.parent_payload_start) {
            return -1;
        }
        if (kind == BMFF_ITEM_BOX_IINF) {
            iinf_count++;
            if (iinf_count > 1) return -1;
            int status = bmff_parse_iinf(
                data + box.payload_start,
                box.payload_end - box.payload_start,
                &box_count, &iinf_entry_count,
                deadline_monotonic_ms, has_tmap_item);
            if (status != 0) return status;
        } else if (kind == BMFF_ITEM_BOX_ILOC) {
            if (iloc_payload != NULL) return -1;
            iloc_payload = data + box.payload_start;
            iloc_payload_len = box.payload_end - box.payload_start;
        } else {
            idat_count++;
            if (idat_count > 1) return -1;
            idat_len = box.payload_end - box.payload_start;
        }
    }
    if (iinf_count != 1) return -1;
    if (iloc_payload == NULL) return -1;
    return bmff_validate_iloc_payload(
        iloc_payload, iloc_payload_len, len,
        idat_count == 1, idat_len, deadline_monotonic_ms);
}

static int fluxer_heif_validate_with_tmap(const void *buf, size_t len,
                                          long long deadline_monotonic_ms,
                                          int *has_tmap_item) {
    if (has_tmap_item == NULL || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int status = validate_isobmff_item_metadata(
        buf, len, deadline_monotonic_ms, has_tmap_item);
    if (status == 0) return FLUXER_NATIVE_STATUS_OK;
    if (status == FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED) return status;
    return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
}

int fluxer_heif_validate(
    const void *buf,
    size_t len,
    long long deadline_monotonic_ms
) {
    int has_tmap_item = 0;
    return fluxer_heif_validate_with_tmap(
        buf, len, deadline_monotonic_ms, &has_tmap_item);
}

typedef struct {
    uint8_t *pixels;
    size_t pixels_len;
    int width;
    int height;
    int hdr_tone_mapped;
    int auxiliary_gain_map_detected;
} heif_primary_still_pixels;

typedef struct {
    struct heif_context *context;
    struct heif_image_handle *handle;
} heif_primary_still_context;

static void heif_primary_still_context_clear(
    heif_primary_still_context *context
) {
    if (context == NULL) return;
    if (context->handle != NULL) {
        heif_image_handle_release(context->handle);
    }
    if (context->context != NULL) heif_context_free(context->context);
    memset(context, 0, sizeof(*context));
}

static int heif_primary_still_context_open(
    heif_primary_still_context *context,
    const void *data,
    size_t len,
    size_t max_pixels,
    long long deadline_monotonic_ms
) {
    assert(context != NULL);
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    context->context = heif_context_alloc();
    if (context->context == NULL) {
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    status = fluxer_set_heif_security_limits(
        context->context, max_pixels);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    heif_context_set_max_decoding_threads(context->context, 1);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    struct heif_error error = heif_context_read_from_memory_without_copy(
        context->context, data, len, NULL);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (error.code != heif_error_Ok) {
        return fluxer_native_status_from_heif_error(error);
    }
    error = heif_context_get_primary_image_handle(
        context->context, &context->handle);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (error.code != heif_error_Ok) {
        return fluxer_native_status_from_heif_error(error);
    }
    if (context->handle == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    return FLUXER_NATIVE_STATUS_OK;
}

static int heif_primary_still_geometry(
    struct heif_image_handle *handle,
    size_t max_pixels,
    int max_dimension,
    int *out_width,
    int *out_height,
    size_t *out_pixels_len
) {
    assert(handle != NULL);
    assert(out_width != NULL);
    assert(out_height != NULL);
    assert(out_pixels_len != NULL);
    int width = heif_image_handle_get_width(handle);
    int height = heif_image_handle_get_height(handle);
    if (width <= 0 || height <= 0 || width > max_dimension ||
        height > max_dimension) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    size_t pixels_len = 0;
    if (fluxer_heif_checked_rgba_size(width, height, &pixels_len) != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (pixels_len / 4u > max_pixels) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    *out_width = width;
    *out_height = height;
    *out_pixels_len = pixels_len;
    return FLUXER_NATIVE_STATUS_OK;
}

static int decode_heif_primary_still_pixels(
    const void *buf, size_t len, size_t max_pixels, int max_dimension,
    long long deadline_monotonic_ms,
    heif_primary_still_pixels *decoded) {
    if (buf == NULL || len == 0 || max_pixels == 0 || max_dimension <= 0 ||
        deadline_monotonic_ms < 0 || decoded == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    memset(decoded, 0, sizeof(*decoded));
    heif_primary_still_context context = {0};
    uint8_t *pixels = NULL;
    int status = heif_primary_still_context_open(
        &context, buf, len, max_pixels, deadline_monotonic_ms);
    int width = 0;
    int height = 0;
    size_t pixels_len = 0;
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = heif_primary_still_geometry(
            context.handle, max_pixels, max_dimension,
            &width, &height, &pixels_len);
    }
    int gain_map_detected = 0;
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_heif_detect_hdr_gain_map(
            context.handle, deadline_monotonic_ms, &gain_map_detected);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        pixels = g_try_malloc(pixels_len);
        if (pixels == NULL) status = FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    int hdr_tone_mapped = 0;
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_heif_decode_to_sdr_rgba8(
            context.handle, pixels, pixels_len, width, height,
            deadline_monotonic_ms,
            &hdr_tone_mapped);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        *decoded = (heif_primary_still_pixels) {
            .pixels = pixels,
            .pixels_len = pixels_len,
            .width = width,
            .height = height,
            .hdr_tone_mapped = hdr_tone_mapped,
            .auxiliary_gain_map_detected = gain_map_detected,
        };
        pixels = NULL;
    }
    g_free(pixels);
    heif_primary_still_context_clear(&context);
    return status;
}

int fluxer_heif_decode_primary_still(
    const void *buf, size_t len, long long deadline_monotonic_ms,
    VipsImage **out, size_t max_pixels, int max_dimension,
    struct fluxer_heif_primary_still_decode_facts *facts) {
    if (out != NULL) *out = NULL;
    if (facts != NULL) memset(facts, 0, sizeof(*facts));
    if (buf == NULL || len == 0 || out == NULL || max_pixels == 0 ||
        max_dimension <= 0 || deadline_monotonic_ms < 0 || facts == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int has_tmap_item = 0;
    int validate_status = fluxer_heif_validate_with_tmap(
        buf, len, deadline_monotonic_ms, &has_tmap_item);
    if (validate_status != FLUXER_NATIVE_STATUS_OK) return validate_status;

    heif_primary_still_pixels decoded;
    int decode_status = decode_heif_primary_still_pixels(
        buf, len, max_pixels, max_dimension,
        deadline_monotonic_ms, &decoded);
    if (decode_status != FLUXER_NATIVE_STATUS_OK) return decode_status;
    int deadline_status = fluxer_native_deadline_status(
        deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
        g_free(decoded.pixels);
        return deadline_status;
    }
    VipsImage *image = vips_image_new_from_memory(decoded.pixels,
                                                  decoded.pixels_len,
                                                  decoded.width,
                                                  decoded.height,
                                                  4, VIPS_FORMAT_UCHAR);
    if (image == NULL) {
        g_free(decoded.pixels);
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    deadline_status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
        g_object_unref(image);
        g_free(decoded.pixels);
        return deadline_status;
    }
    if (g_signal_connect_swapped(
            image, "postclose", G_CALLBACK(g_free), decoded.pixels) == 0) {
        g_object_unref(image);
        g_free(decoded.pixels);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }

    facts->hdr_tone_mapped = decoded.hdr_tone_mapped;
    facts->hdr_gain_map_detected = has_tmap_item > 0 || decoded.auxiliary_gain_map_detected;
    *out = image;
    return FLUXER_NATIVE_STATUS_OK;
}
