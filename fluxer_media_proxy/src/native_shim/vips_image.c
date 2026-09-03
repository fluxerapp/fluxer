// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

const int fluxer_vips_format_uchar = VIPS_FORMAT_UCHAR;
const int fluxer_vips_format_ushort = VIPS_FORMAT_USHORT;
const int fluxer_vips_format_float = VIPS_FORMAT_FLOAT;

int fluxer_vips_init(const char *argv0) {
    if (argv0 == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    int rc = vips_init(argv0);
    if (rc != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    av_log_set_level(AV_LOG_WARNING);
    vips_block_untrusted_set(TRUE);
    vips_operation_block_set("VipsForeignLoad", TRUE);
    vips_operation_block_set("VipsForeignLoadJpeg", FALSE);
    vips_operation_block_set("VipsForeignLoadPng", FALSE);
    vips_operation_block_set("VipsForeignLoadWebp", FALSE);
    vips_operation_block_set("VipsForeignLoadNsgif", FALSE);
    vips_operation_block_set("VipsForeignLoadSvg", FALSE);
    vips_operation_block_set("VipsForeignLoadHeif", FALSE);
    vips_operation_block_set("VipsForeignLoadJxl", FALSE);
    vips_operation_block_set("VipsForeignLoadTiff", FALSE);
    return FLUXER_NATIVE_STATUS_OK;
}

void fluxer_vips_error_clear(void) {
    vips_error_clear();
}

const char *fluxer_vips_error_buffer(void) {
    return vips_error_buffer();
}

void fluxer_vips_tune_for_server(int per_pipeline_threads) {
    assert(per_pipeline_threads >= 1 && per_pipeline_threads <= FLUXER_MAX_THREADS_PER_PIPELINE);
    vips_concurrency_set(per_pipeline_threads);
    vips_cache_set_max(0);
    vips_cache_set_max_mem(0);
    vips_cache_set_max_files(0);
    vips_leak_set(FALSE);
}

int fluxer_vips_probe_animated(const void *buf, size_t len, int *width, int *height, int *pages) {
    if (width == NULL || height == NULL || pages == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *width = 0;
    *height = 0;
    *pages = 0;
    if (buf == NULL || len == 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    VipsImage *header = vips_image_new_from_buffer(buf, len, "n=1", NULL);
    if (header == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    VipsImage *display_header = NULL;
    if (vips_autorot(header, &display_header, NULL) != 0 || display_header == NULL) {
        if (display_header != NULL) g_object_unref(display_header);
        g_object_unref(header);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int display_width = vips_image_get_width(display_header);
    int display_height = vips_image_get_height(display_header);
    if (display_width <= 0 || display_height <= 0) {
        g_object_unref(display_header);
        g_object_unref(header);
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int n_pages = 1;
    if (vips_image_get_typeof(display_header, "n-pages") != 0) {
        if (vips_image_get_int(display_header, "n-pages", &n_pages) != 0) {
            g_object_unref(display_header);
            g_object_unref(header);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
    }
    if (n_pages <= 0) {
        g_object_unref(display_header);
        g_object_unref(header);
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    *width = display_width;
    *height = display_height;
    *pages = n_pages;
    g_object_unref(display_header);
    g_object_unref(header);
    return FLUXER_NATIVE_STATUS_OK;
}

VipsImage *fluxer_vips_image_new_from_buffer(const void *buf, size_t len, const char *option_string) {
    if (buf == NULL || len == 0 || option_string == NULL) return NULL;
    return vips_image_new_from_buffer(buf, len, option_string, NULL);
}

VipsImage *fluxer_vips_image_new_from_memory(const void *data, size_t size, int width, int height, int bands, int format) {
    if (data == NULL || size == 0 || width <= 0 || height <= 0 || bands <= 0) return NULL;
    return vips_image_new_from_memory(data, size, width, height, bands, format);
}

VipsImage *fluxer_vips_image_new_from_memory_copy(const void *data, size_t size, int width, int height, int bands, int format) {
    if (data == NULL || size == 0 || width <= 0 || height <= 0 || bands <= 0) return NULL;
    return vips_image_new_from_memory_copy(data, size, width, height, bands, format);
}

int fluxer_vips_image_write_to_buffer(VipsImage *image, const char *suffix, void **buf, size_t *size) {
    if (image == NULL || suffix == NULL || buf == NULL || size == NULL) return -1;
    return vips_image_write_to_buffer(image, suffix, buf, size, NULL);
}

int fluxer_vips_image_get_width(VipsImage *image) {
    return image == NULL ? 0 : vips_image_get_width(image);
}

int fluxer_vips_image_get_height(VipsImage *image) {
    return image == NULL ? 0 : vips_image_get_height(image);
}

int fluxer_vips_image_get_orientation_swap(VipsImage *image) {
    return image != NULL && vips_image_get_orientation_swap(image);
}

int fluxer_vips_image_get_bands(VipsImage *image) {
    return image == NULL ? 0 : vips_image_get_bands(image);
}

int fluxer_vips_image_get_format(VipsImage *image) {
    return image == NULL ? VIPS_FORMAT_NOTSET : vips_image_get_format(image);
}

int fluxer_vips_image_has_field(VipsImage *image, const char *field) {
    if (image == NULL) return 0;
    if (field == NULL) return 0;
    return vips_image_get_typeof(image, field) != 0;
}

int fluxer_vips_image_get_int(VipsImage *image, const char *field, int *out) {
    if (out == NULL) return -1;
    *out = 0;
    if (image == NULL || field == NULL) return -1;
    return vips_image_get_int(image, field, out);
}

void fluxer_vips_set_page_height(VipsImage *image, int page_height) {
    if (image != NULL && page_height > 0) {
        vips_image_set_int(image, "page-height", page_height);
    }
}

int fluxer_vips_set_animation_loop_count(VipsImage *image, int loop_count) {
    if (image == NULL || loop_count < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    vips_image_set_int(image, "loop", loop_count);
    int stored_loop_count = -1;
    if (vips_image_get_int(image, "loop", &stored_loop_count) != 0 ||
        stored_loop_count != loop_count) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_vips_operation_status(int rc, VipsImage **out) {
    assert(out != NULL);
    if (rc == 0 && *out != NULL) return FLUXER_NATIVE_STATUS_OK;
    if (*out != NULL) {
        g_object_unref(*out);
        *out = NULL;
    }
    return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
}

struct fluxer_vips_deadline_guard {
    VipsImage *image;
    long long deadline_monotonic_ms;
    gulong eval_handler;
    int reached;
};

static int fluxer_vips_deadline_guard_start(
    struct fluxer_vips_deadline_guard *guard,
    VipsImage *image,
    long long deadline_monotonic_ms);
static int fluxer_vips_deadline_guard_finish(
    struct fluxer_vips_deadline_guard *guard);

int fluxer_vips_image_copy_memory(
    VipsImage *in,
    long long deadline_monotonic_ms,
    VipsImage **out
) {
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = NULL;
    if (in == NULL || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    struct fluxer_vips_deadline_guard deadline_guard;
    int status = fluxer_vips_deadline_guard_start(
        &deadline_guard, in, deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    VipsImage *memory = vips_image_copy_memory(in);
    int deadline_status = fluxer_vips_deadline_guard_finish(&deadline_guard);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
        if (memory != NULL) g_object_unref(memory);
        return deadline_status;
    }
    if (memory == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = memory;
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_vips_autorot(
    VipsImage *in,
    long long deadline_monotonic_ms,
    VipsImage **out
) {
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = NULL;
    if (in == NULL || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int rc = vips_autorot(in, out, NULL);
    status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        if (*out != NULL) {
            g_object_unref(*out);
            *out = NULL;
        }
        return status;
    }
    return fluxer_vips_operation_status(rc, out);
}

int fluxer_vips_extract_area(VipsImage *in, VipsImage **out, int left, int top, int width, int height) {
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = NULL;
    if (in == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (left < 0 || top < 0 || width <= 0 || height <= 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int rc = vips_extract_area(in, out, left, top, width, height, NULL);
    return fluxer_vips_operation_status(rc, out);
}

int fluxer_vips_resize(VipsImage *in, VipsImage **out, double scale) {
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = NULL;
    if (in == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (!isfinite(scale) || scale <= 0.0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int rc = vips_resize(in, out, scale, NULL);
    return fluxer_vips_operation_status(rc, out);
}

static int fluxer_vips_copy_optional_int_metadata(
    VipsImage *source,
    VipsImage *destination,
    const char *name
) {
    assert(source != NULL);
    assert(destination != NULL);
    assert(name != NULL);
    if (vips_image_get_typeof(source, name) == 0) {
        return FLUXER_NATIVE_STATUS_OK;
    }
    int value = 0;
    if (vips_image_get_int(source, name, &value) != 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    vips_image_set_int(destination, name, value);
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_vips_copy_animation_metadata(
    VipsImage *source,
    VipsImage *destination,
    int n_pages,
    int page_height
) {
    assert(source != NULL);
    assert(destination != NULL);
    assert(n_pages > 0);
    assert(page_height > 0);
    if (vips_image_get_typeof(source, "delay") != 0) {
        int *delays = NULL;
        int delay_count = 0;
        if (vips_image_get_array_int(
                source, "delay", &delays, &delay_count) != 0 ||
            delays == NULL || delay_count != n_pages) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
        vips_image_set_array_int(destination, "delay", delays, delay_count);
    }
    const char *int_fields[] = {"loop", "gif-loop", "gif-delay"};
    for (size_t i = 0; i < sizeof(int_fields) / sizeof(int_fields[0]); i++) {
        int status = fluxer_vips_copy_optional_int_metadata(
            source, destination, int_fields[i]);
        if (status != FLUXER_NATIVE_STATUS_OK) return status;
    }
    vips_image_set_int(destination, "page-height", page_height);
    vips_image_set_int(destination, "n-pages", n_pages);
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_vips_join_animation_pages(
    VipsImage *source,
    VipsImage **pages,
    int n_pages,
    int max_pages,
    size_t max_total_pixels,
    VipsImage **out
) {
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = NULL;
    if (source == NULL || pages == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (n_pages <= 1 || max_pages <= 0 || n_pages > max_pages ||
        max_total_pixels == 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (pages[0] == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    int page_width = vips_image_get_width(pages[0]);
    int page_height = vips_image_get_height(pages[0]);
    if (page_width <= 0 || page_height <= 0 || page_height > INT_MAX / n_pages) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int total_height = page_height * n_pages;
    if ((size_t)page_width > max_total_pixels / (size_t)total_height) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    for (int i = 1; i < n_pages; i++) {
        if (pages[i] == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        if (vips_image_get_width(pages[i]) != page_width ||
            vips_image_get_height(pages[i]) != page_height) {
            return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
        }
    }
    VipsImage *joined = NULL;
    int rc = vips_arrayjoin(pages, &joined, n_pages, "across", 1, NULL);
    int status = fluxer_vips_operation_status(rc, &joined);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (vips_image_get_width(joined) != page_width ||
        vips_image_get_height(joined) != total_height) {
        g_object_unref(joined);
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    status = fluxer_vips_copy_animation_metadata(
        source, joined, n_pages, page_height);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        g_object_unref(joined);
        return status;
    }
    *out = joined;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_vips_image_work_bounds_status(
    VipsImage *image,
    int max_pages,
    size_t max_total_pixels
) {
    if (image == NULL || max_pages <= 0 || max_total_pixels == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int width = vips_image_get_width(image);
    int total_height = vips_image_get_height(image);
    if (width <= 0 || total_height <= 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int page_height = total_height;
    if (vips_image_get_typeof(image, "page-height") != 0) {
        if (vips_image_get_int(image, "page-height", &page_height) != 0) {
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
    }
    if (page_height <= 0 || total_height % page_height != 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    int pages = total_height / page_height;
    if (pages <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (pages > max_pages) return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    if ((size_t)width > max_total_pixels / (size_t)total_height) {
        return FLUXER_NATIVE_STATUS_WORK_LIMIT_EXCEEDED;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

struct fluxer_vips_thumbnail_request {
    const void *data;
    size_t len;
    VipsImage **output;
    int width;
    int height;
    int pages;
    int crop_mode;
    int max_pages;
    size_t max_total_pixels;
};

struct fluxer_vips_center_thumbnail {
    VipsImage *image;
    int width;
    int height;
    int has_page_height;
};

static int fluxer_vips_thumbnail_request_status(
    const struct fluxer_vips_thumbnail_request *request
) {
    if (request == NULL || request->data == NULL || request->len == 0 ||
        request->output == NULL || request->max_pages <= 0 ||
        request->max_total_pixels == 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->width < 0 || request->height < 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    if (request->pages != 1 && request->pages != -1) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->crop_mode != FLUXER_THUMB_CROP_NONE &&
        request->crop_mode != FLUXER_THUMB_CROP_CENTRE) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (request->pages == 1 &&
        request->crop_mode == FLUXER_THUMB_CROP_CENTRE &&
        (request->width == 0 || request->height == 0)) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static const char *fluxer_vips_thumbnail_options(
    const struct fluxer_vips_thumbnail_request *request
) {
    assert(request != NULL);
    const uint8_t *bytes = request->data;
    int is_jpeg = request->len >= 3 && bytes[0] == 0xff &&
                  bytes[1] == 0xd8 && bytes[2] == 0xff;
    if (request->pages == -1) {
        return is_jpeg
            ? "n=-1,access=sequential"
            : "n=-1,access=sequential,fail=true";
    }
    return is_jpeg ? "access=sequential" : "access=sequential,fail=true";
}

static int fluxer_vips_uncropped_thumbnail(
    const struct fluxer_vips_thumbnail_request *request,
    const char *options,
    VipsImage *loaded
) {
    assert(request != NULL);
    assert(options != NULL);
    assert(loaded != NULL);
    g_object_unref(loaded);
    int target_width = request->width;
    int target_height = request->height;
    if (target_width == 0) target_width = VIPS_MAX_COORD;
    if (target_height == 0) target_height = VIPS_MAX_COORD;
    int result = vips_thumbnail_buffer(
        (void *)request->data, request->len, request->output, target_width,
        "height", target_height, "size", VIPS_SIZE_DOWN,
        "no_rotate", FALSE, "option_string", options, NULL);
    int status = fluxer_vips_operation_status(result, request->output);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    status = fluxer_vips_image_work_bounds_status(
        *request->output, request->max_pages, request->max_total_pixels);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        g_object_unref(*request->output);
        *request->output = NULL;
    }
    return status;
}

static int fluxer_vips_center_thumbnail_open(
    struct fluxer_vips_center_thumbnail *context,
    const struct fluxer_vips_thumbnail_request *request,
    VipsImage *loaded
) {
    assert(context != NULL);
    assert(request != NULL);
    assert(loaded != NULL);
    VipsImage *oriented = NULL;
    int result = vips_autorot(loaded, &oriented, NULL);
    g_object_unref(loaded);
    if (result != 0 || oriented == NULL) {
        if (oriented != NULL) g_object_unref(oriented);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    int status = fluxer_vips_image_work_bounds_status(
        oriented, request->max_pages, request->max_total_pixels);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        g_object_unref(oriented);
        return status;
    }
    int width = vips_image_get_width(oriented);
    int height = vips_image_get_height(oriented);
    int page_height = height;
    int has_page_height =
        vips_image_get_typeof(oriented, "page-height") != 0;
    if (has_page_height &&
        vips_image_get_int(oriented, "page-height", &page_height) != 0) {
        g_object_unref(oriented);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (width <= 0 || height <= 0 || page_height != height) {
        g_object_unref(oriented);
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    context->image = oriented;
    context->width = width;
    context->height = height;
    context->has_page_height = has_page_height;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_vips_center_thumbnail_resize(
    struct fluxer_vips_center_thumbnail *context,
    const struct fluxer_vips_thumbnail_request *request
) {
    assert(context != NULL);
    assert(context->image != NULL);
    assert(request != NULL);
    double width_scale = (double)request->width / (double)context->width;
    double height_scale = (double)request->height / (double)context->height;
    double scale = width_scale;
    if (height_scale > scale) scale = height_scale;
    if (scale > 1.0) scale = 1.0;
    if (scale < 1.0) {
        VipsImage *resized = NULL;
        int result = vips_resize(context->image, &resized, scale, NULL);
        if (result != 0 || resized == NULL) {
            if (resized != NULL) g_object_unref(resized);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        g_object_unref(context->image);
        context->image = resized;
    }
    context->width = vips_image_get_width(context->image);
    context->height = vips_image_get_height(context->image);
    if (context->width <= 0 || context->height <= 0) {
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_vips_center_thumbnail_publish(
    struct fluxer_vips_center_thumbnail *context,
    const struct fluxer_vips_thumbnail_request *request
) {
    assert(context != NULL);
    assert(context->image != NULL);
    assert(request != NULL);
    int final_width = context->width;
    int final_height = context->height;
    if (final_width > request->width) final_width = request->width;
    if (final_height > request->height) final_height = request->height;
    if (final_width == context->width && final_height == context->height) {
        if (context->has_page_height) {
            vips_image_set_int(context->image, "page-height", final_height);
        }
        *request->output = context->image;
        context->image = NULL;
        return FLUXER_NATIVE_STATUS_OK;
    }
    int left = (context->width - final_width) / 2;
    int top = (context->height - final_height) / 2;
    VipsImage *cropped = NULL;
    int result = vips_extract_area(
        context->image, &cropped, left, top, final_width, final_height, NULL);
    if (result != 0 || cropped == NULL) {
        if (cropped != NULL) g_object_unref(cropped);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (context->has_page_height) {
        vips_image_set_int(cropped, "page-height", final_height);
    }
    *request->output = cropped;
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_vips_center_thumbnail_run(
    const struct fluxer_vips_thumbnail_request *request,
    VipsImage *loaded
) {
    struct fluxer_vips_center_thumbnail context = {0};
    int status = fluxer_vips_center_thumbnail_open(
        &context, request, loaded);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_vips_center_thumbnail_resize(&context, request);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = fluxer_vips_center_thumbnail_publish(&context, request);
    }
    if (context.image != NULL) g_object_unref(context.image);
    return status;
}

int fluxer_vips_thumbnail_buffer_ex(
    const void *buf, size_t len, long long deadline_monotonic_ms,
    VipsImage **out, int width, int height,
    int n, int crop_mode, int max_pages, size_t max_total_pixels
) {
    if (out != NULL) *out = NULL;
    if (deadline_monotonic_ms < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    struct fluxer_vips_thumbnail_request request = {
        .data = buf,
        .len = len,
        .output = out,
        .width = width,
        .height = height,
        .pages = n,
        .crop_mode = crop_mode,
        .max_pages = max_pages,
        .max_total_pixels = max_total_pixels,
    };
    status = fluxer_vips_thumbnail_request_status(&request);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    const char *options = fluxer_vips_thumbnail_options(&request);
    VipsImage *loaded = vips_image_new_from_buffer(buf, len, options, NULL);
    if (loaded == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    status = fluxer_vips_image_work_bounds_status(
        loaded, max_pages, max_total_pixels);
    if (status != FLUXER_NATIVE_STATUS_OK) {
        g_object_unref(loaded);
        return status;
    }
    int use_center_crop = n == 1 && crop_mode == FLUXER_THUMB_CROP_CENTRE;
    if (use_center_crop) {
        status = fluxer_vips_center_thumbnail_run(&request, loaded);
    } else {
        status = fluxer_vips_uncropped_thumbnail(&request, options, loaded);
    }
    int deadline_status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
        if (*out != NULL) {
            g_object_unref(*out);
            *out = NULL;
        }
        return deadline_status;
    }
    return status;
}

struct fluxer_vips_write_ctx {
    fluxer_vips_write_cb cb;
    void *user_data;
    long long deadline_monotonic_ms;
    int status;
};

static void fluxer_vips_deadline_eval(
    VipsImage *image,
    VipsProgress *progress,
    void *user_data
) {
    (void)progress;
    struct fluxer_vips_deadline_guard *guard = user_data;
    if (guard == NULL || guard->reached) return;
    if (fluxer_native_deadline_status(guard->deadline_monotonic_ms) ==
        FLUXER_NATIVE_STATUS_OK) return;
    guard->reached = 1;
    vips_image_set_kill(image, TRUE);
}

static int fluxer_vips_deadline_guard_start(
    struct fluxer_vips_deadline_guard *guard,
    VipsImage *image,
    long long deadline_monotonic_ms
) {
    if (guard == NULL || image == NULL || deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    memset(guard, 0, sizeof(*guard));
    guard->image = image;
    guard->deadline_monotonic_ms = deadline_monotonic_ms;
    int status = fluxer_native_deadline_status(deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (deadline_monotonic_ms == 0) return FLUXER_NATIVE_STATUS_OK;
    vips_image_set_progress(image, TRUE);
    guard->eval_handler = g_signal_connect(
        image, "eval", G_CALLBACK(fluxer_vips_deadline_eval), guard);
    if (guard->eval_handler == 0) {
        vips_image_set_progress(image, FALSE);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    return FLUXER_NATIVE_STATUS_OK;
}

static int fluxer_vips_deadline_guard_finish(
    struct fluxer_vips_deadline_guard *guard
) {
    assert(guard != NULL);
    if (guard->eval_handler != 0) {
        g_signal_handler_disconnect(guard->image, guard->eval_handler);
        vips_image_set_progress(guard->image, FALSE);
        vips_image_set_kill(guard->image, FALSE);
    }
    if (guard->reached) return FLUXER_NATIVE_STATUS_DEADLINE_EXCEEDED;
    return fluxer_native_deadline_status(guard->deadline_monotonic_ms);
}

int fluxer_vips_image_write_to_memory_deadline(
    VipsImage *image,
    long long deadline_monotonic_ms,
    size_t max_output_size,
    void **out_buf,
    size_t *out_size
) {
    if (out_buf != NULL) *out_buf = NULL;
    if (out_size != NULL) *out_size = 0;
    if (image == NULL || deadline_monotonic_ms < 0 || max_output_size == 0 ||
        out_buf == NULL || out_size == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    struct fluxer_vips_deadline_guard deadline_guard;
    int status = fluxer_vips_deadline_guard_start(
        &deadline_guard, image, deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    size_t size = 0;
    void *buffer = vips_image_write_to_memory(image, &size);
    int deadline_status = fluxer_vips_deadline_guard_finish(&deadline_guard);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
        if (buffer != NULL) g_free(buffer);
        return deadline_status;
    }
    if (buffer == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    if (size == 0) {
        g_free(buffer);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (size > max_output_size) {
        g_free(buffer);
        return FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
    }
    *out_buf = buffer;
    *out_size = size;
    return FLUXER_NATIVE_STATUS_OK;
}

static gint64 fluxer_vips_target_write_adapter(VipsTargetCustom *target, const void *bytes, gint64 length, void *gp) {
    (void)target;
    struct fluxer_vips_write_ctx *c = gp;
    int deadline_status = fluxer_native_deadline_status(
        c->deadline_monotonic_ms);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) {
        c->status = deadline_status;
        return -1;
    }
    if (length < 0) {
        c->status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        return -1;
    }
    if (length == 0) return 0;
    size_t callback_length = (size_t)length;
    if ((gint64)callback_length != length) {
        c->status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        return -1;
    }
    if (c->cb(c->user_data, bytes, callback_length) != 0) {
        c->status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        return -1;
    }
    return length;
}

int fluxer_vips_image_write_to_callback(
    VipsImage *image,
    const char *suffix,
    long long deadline_monotonic_ms,
    fluxer_vips_write_cb cb,
    void *user_data
) {
    if (image == NULL || suffix == NULL || cb == NULL ||
        deadline_monotonic_ms < 0) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    struct fluxer_vips_deadline_guard deadline_guard;
    int status = fluxer_vips_deadline_guard_start(
        &deadline_guard, image, deadline_monotonic_ms);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    VipsTargetCustom *target = vips_target_custom_new();
    if (target == NULL) {
        fluxer_vips_deadline_guard_finish(&deadline_guard);
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }

    struct fluxer_vips_write_ctx ctx = {
        .cb = cb,
        .user_data = user_data,
        .deadline_monotonic_ms = deadline_monotonic_ms,
        .status = FLUXER_NATIVE_STATUS_OK,
    };
    g_signal_connect(target, "write", G_CALLBACK(fluxer_vips_target_write_adapter), &ctx);

    int rc = vips_image_write_to_target(image, suffix, (VipsTarget *)target, NULL);
    g_object_unref(target);
    int deadline_status = fluxer_vips_deadline_guard_finish(&deadline_guard);
    if (deadline_status != FLUXER_NATIVE_STATUS_OK) return deadline_status;
    if (ctx.status != FLUXER_NATIVE_STATUS_OK) return ctx.status;
    if (rc != 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    return FLUXER_NATIVE_STATUS_OK;
}

struct fluxer_vips_bounded_buffer {
    uint8_t *data;
    size_t len;
    size_t capacity;
    size_t max_output_size;
    int status;
};

static int fluxer_vips_bounded_buffer_write(void *user_data, const void *bytes, size_t len) {
    if (user_data == NULL) return -1;
    struct fluxer_vips_bounded_buffer *buffer = user_data;
    if (buffer->status != FLUXER_NATIVE_STATUS_OK) return -1;
    if (len == 0) return 0;
    if (bytes == NULL) {
        buffer->status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        return -1;
    }
    if (buffer->len > buffer->max_output_size ||
        len > buffer->max_output_size - buffer->len) {
        buffer->status = FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
        return -1;
    }
    size_t required = buffer->len + len;
    if (required > buffer->capacity) {
        size_t next_capacity = buffer->capacity;
        if (next_capacity == 0) {
            next_capacity = buffer->max_output_size < 16384u
                ? buffer->max_output_size
                : 16384u;
        }
        while (next_capacity < required) {
            if (next_capacity > buffer->max_output_size / 2u) {
                next_capacity = buffer->max_output_size;
            } else {
                next_capacity *= 2u;
            }
        }
        if (next_capacity < required || next_capacity > buffer->max_output_size) {
            buffer->status = FLUXER_NATIVE_STATUS_OUTPUT_LIMIT_EXCEEDED;
            return -1;
        }
        uint8_t *next = g_try_realloc(buffer->data, next_capacity);
        if (next == NULL) {
            buffer->status = FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
            return -1;
        }
        buffer->data = next;
        buffer->capacity = next_capacity;
    }
    memcpy(buffer->data + buffer->len, bytes, len);
    buffer->len = required;
    return 0;
}

int fluxer_vips_image_write_to_buffer_bounded(VipsImage *image,
                                               const char *suffix,
                                               long long deadline_monotonic_ms,
                                               size_t max_output_size,
                                               void **out_buf,
                                               size_t *out_size,
                                               size_t *out_capacity) {
    if (out_buf != NULL) *out_buf = NULL;
    if (out_size != NULL) *out_size = 0;
    if (out_capacity != NULL) *out_capacity = 0;
    if (image == NULL || suffix == NULL || deadline_monotonic_ms < 0 ||
        max_output_size == 0 ||
        out_buf == NULL || out_size == NULL || out_capacity == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    struct fluxer_vips_bounded_buffer buffer = {
        .max_output_size = max_output_size,
        .status = FLUXER_NATIVE_STATUS_OK,
    };
    int status = fluxer_vips_image_write_to_callback(
        image, suffix, deadline_monotonic_ms,
        fluxer_vips_bounded_buffer_write, &buffer);
    if (buffer.status != FLUXER_NATIVE_STATUS_OK) status = buffer.status;
    if (status == FLUXER_NATIVE_STATUS_OK &&
        (buffer.data == NULL || buffer.len == 0 || buffer.capacity < buffer.len)) {
        status = FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (status != FLUXER_NATIVE_STATUS_OK) {
        if (buffer.data != NULL) g_free(buffer.data);
        return status;
    }
    *out_buf = buffer.data;
    *out_size = buffer.len;
    *out_capacity = buffer.capacity;
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_vips_image_to_rgba(VipsImage *in, VipsImage **out) {
    if (out == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out = NULL;
    if (in == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;

    VipsBandFormat format = vips_image_get_format(in);
    VipsInterpretation interpretation = vips_image_guess_interpretation(in);
    VipsImage *scaled = NULL;
    VipsImage *depth_normalized = NULL;
    if (format == VIPS_FORMAT_USHORT) {
        if (interpretation != VIPS_INTERPRETATION_RGB16 &&
            interpretation != VIPS_INTERPRETATION_GREY16) {
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
        }
        if (vips_cast(
                in, &depth_normalized, VIPS_FORMAT_UCHAR,
                "shift", TRUE, NULL) != 0 ||
            depth_normalized == NULL) {
            if (depth_normalized != NULL) g_object_unref(depth_normalized);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
    } else if (format == VIPS_FORMAT_FLOAT) {
        if (interpretation == VIPS_INTERPRETATION_B_W) {
            if (vips_linear1(in, &scaled, 255.0, 0.0, NULL) != 0 ||
                scaled == NULL) {
                if (scaled != NULL) g_object_unref(scaled);
                return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            }
            if (vips_cast_uchar(scaled, &depth_normalized, NULL) != 0 ||
                depth_normalized == NULL) {
                if (depth_normalized != NULL) {
                    g_object_unref(depth_normalized);
                }
                g_object_unref(scaled);
                return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
            }
            g_object_unref(scaled);
        } else if (interpretation != VIPS_INTERPRETATION_scRGB) {
            return FLUXER_NATIVE_STATUS_UNSUPPORTED;
        }
    } else if (format != VIPS_FORMAT_UCHAR) {
        return FLUXER_NATIVE_STATUS_UNSUPPORTED;
    }

    VipsImage *colour_input = depth_normalized != NULL ? depth_normalized : in;
    VipsImage *srgb = NULL;
    if (vips_colourspace(
            colour_input, &srgb, VIPS_INTERPRETATION_sRGB, NULL) != 0 ||
        srgb == NULL) {
        if (srgb != NULL) g_object_unref(srgb);
        if (depth_normalized != NULL) g_object_unref(depth_normalized);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    if (depth_normalized != NULL) g_object_unref(depth_normalized);
    if (vips_image_get_format(srgb) != VIPS_FORMAT_UCHAR) {
        g_object_unref(srgb);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }

    VipsImage *rgba = NULL;
    int bands = vips_image_get_bands(srgb);
    if (bands < 4) {
        if (vips_addalpha(srgb, &rgba, NULL) != 0 || rgba == NULL) {
            if (rgba != NULL) g_object_unref(rgba);
            g_object_unref(srgb);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        g_object_unref(srgb);
    } else if (bands > 4) {
        if (vips_extract_band(srgb, &rgba, 0, "n", 4, NULL) != 0 ||
            rgba == NULL) {
            if (rgba != NULL) g_object_unref(rgba);
            g_object_unref(srgb);
            return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
        }
        g_object_unref(srgb);
    } else {
        rgba = srgb;
    }

    if (rgba == NULL || vips_image_get_format(rgba) != VIPS_FORMAT_UCHAR ||
        vips_image_get_bands(rgba) != 4) {
        if (rgba != NULL) g_object_unref(rgba);
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }

    *out = rgba;
    return FLUXER_NATIVE_STATUS_OK;
}

int fluxer_vips_extract_rgba(
    VipsImage *in,
    long long deadline_monotonic_ms,
    void **out_buf,
    size_t *out_size
) {
    if (out_buf != NULL) *out_buf = NULL;
    if (out_size != NULL) *out_size = 0;
    if (in == NULL || deadline_monotonic_ms < 0 ||
        out_buf == NULL || out_size == NULL) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    VipsImage *rgba = NULL;
    int status = fluxer_vips_image_to_rgba(in, &rgba);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    int width = vips_image_get_width(rgba);
    int height = vips_image_get_height(rgba);
    size_t expected_size = 0;
    if (ff_validate_rgba_geometry(width, height, &expected_size) != 0) {
        g_object_unref(rgba);
        return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    }
    void *buf = NULL;
    status = fluxer_vips_image_write_to_memory_deadline(
        rgba, deadline_monotonic_ms, expected_size, &buf, out_size);
    g_object_unref(rgba);
    if (status != FLUXER_NATIVE_STATUS_OK) return status;
    if (buf == NULL || *out_size != expected_size) {
        if (buf != NULL) g_free(buf);
        *out_size = 0;
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    *out_buf = buf;
    return FLUXER_NATIVE_STATUS_OK;
}

void fluxer_vips_unref(VipsImage *image) {
    if (image != NULL) {
        g_object_unref(image);
    }
}

void fluxer_vips_free(void *mem) {
    g_free(mem);
}

void fluxer_av_free(void *mem) {
    if (mem != NULL) av_free(mem);
}

void fluxer_webp_free(void *mem) {
    if (mem != NULL) WebPFree(mem);
}
