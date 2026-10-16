// SPDX-License-Identifier: AGPL-3.0-or-later

#include "native_shim_internal.h"

static int gif_filter_create_source(
    AVFilterGraph *graph,
    int width,
    int height,
    enum AVPixelFormat pixel_format,
    AVRational time_base,
    AVFilterContext **out_source
) {
    assert(graph != NULL);
    assert(out_source != NULL);
    assert(*out_source == NULL);
    char arguments[512];
    int written = snprintf(
        arguments,
        sizeof(arguments),
        "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d:pixel_aspect=1/1",
        width,
        height,
        (int)pixel_format,
        time_base.num,
        time_base.den);
    if (written < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if ((size_t)written >= sizeof(arguments)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }
    const AVFilter *filter = avfilter_get_by_name("buffer");
    if (filter == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    int av_status = avfilter_graph_create_filter(
        out_source, filter, "in", arguments, NULL, graph);
    if (av_status < 0) return fluxer_native_status_from_av_error(av_status);
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_filter_create_sink(
    AVFilterGraph *graph,
    AVFilterContext **out_sink
) {
    assert(graph != NULL);
    assert(out_sink != NULL);
    assert(*out_sink == NULL);
    const AVFilter *filter = avfilter_get_by_name("buffersink");
    if (filter == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    AVFilterContext *sink = avfilter_graph_alloc_filter(graph, filter, "out");
    if (sink == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    enum AVPixelFormat pixel_formats[] = { AV_PIX_FMT_PAL8 };
    int av_status = av_opt_set_array(
        sink,
        "pixel_formats",
        AV_OPT_SEARCH_CHILDREN,
        0,
        1,
        AV_OPT_TYPE_PIXEL_FMT,
        pixel_formats);
    if (av_status < 0) return fluxer_native_status_from_av_error(av_status);
    av_status = avfilter_init_dict(sink, NULL);
    if (av_status < 0) return fluxer_native_status_from_av_error(av_status);
    *out_sink = sink;
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_filter_create_endpoint(
    const char *name,
    AVFilterContext *filter,
    AVFilterInOut **out_endpoint
) {
    assert(name != NULL);
    assert(filter != NULL);
    assert(out_endpoint != NULL);
    assert(*out_endpoint == NULL);
    AVFilterInOut *endpoint = avfilter_inout_alloc();
    if (endpoint == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    endpoint->name = av_strdup(name);
    if (endpoint->name == NULL) {
        avfilter_inout_free(&endpoint);
        return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    }
    endpoint->filter_ctx = filter;
    endpoint->pad_idx = 0;
    endpoint->next = NULL;
    *out_endpoint = endpoint;
    return FLUXER_NATIVE_STATUS_OK;
}

static int gif_filter_connect(
    AVFilterGraph *graph,
    AVFilterContext *source,
    AVFilterContext *sink,
    int target_width,
    int target_height
) {
    assert(graph != NULL);
    assert(source != NULL);
    assert(sink != NULL);
    char description[256];
    int written = snprintf(
        description,
        sizeof(description),
        "scale=%d:%d:flags=lanczos,format=rgba,"
        "split[a][b];"
        "[a]palettegen=reserve_transparent=1:stats_mode=single[p];"
        "[b][p]paletteuse=alpha_threshold=128:dither=none:new=1",
        target_width,
        target_height);
    if (written < 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if ((size_t)written >= sizeof(description)) {
        return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    }

    AVFilterInOut *outputs = NULL;
    AVFilterInOut *inputs = NULL;
    int status = gif_filter_create_endpoint("in", source, &outputs);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = gif_filter_create_endpoint("out", sink, &inputs);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        int av_status = avfilter_graph_parse_ptr(
            graph, description, &inputs, &outputs, NULL);
        if (av_status < 0) {
            status = fluxer_native_status_from_av_error(av_status);
        }
    }
    avfilter_inout_free(&outputs);
    avfilter_inout_free(&inputs);
    return status;
}

int fluxer_gif_setup_filter_graph(
    AVFilterGraph **out_graph,
    AVFilterContext **out_source,
    AVFilterContext **out_sink,
    int source_width,
    int source_height,
    enum AVPixelFormat source_format,
    AVRational frame_time_base,
    int target_width,
    int target_height
) {
    if (out_graph == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (out_source == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (out_sink == NULL) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (source_width <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (source_height <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (target_width <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (target_height <= 0) return FLUXER_NATIVE_STATUS_INVALID_DIMENSIONS;
    if (frame_time_base.num <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    if (frame_time_base.den <= 0) return FLUXER_NATIVE_STATUS_CODEC_FAILURE;
    *out_graph = NULL;
    *out_source = NULL;
    *out_sink = NULL;

    AVFilterGraph *graph = avfilter_graph_alloc();
    if (graph == NULL) return FLUXER_NATIVE_STATUS_ALLOCATION_FAILED;
    graph->nb_threads = 1;
    AVFilterContext *source = NULL;
    AVFilterContext *sink = NULL;
    int status = gif_filter_create_source(
        graph, source_width, source_height, source_format,
        frame_time_base, &source);
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = gif_filter_create_sink(graph, &sink);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        status = gif_filter_connect(
            graph, source, sink, target_width, target_height);
    }
    if (status == FLUXER_NATIVE_STATUS_OK) {
        int av_status = avfilter_graph_config(graph, NULL);
        if (av_status < 0) {
            status = fluxer_native_status_from_av_error(av_status);
        }
    }
    if (status != FLUXER_NATIVE_STATUS_OK) {
        avfilter_graph_free(&graph);
        return status;
    }
    *out_graph = graph;
    *out_source = source;
    *out_sink = sink;
    return FLUXER_NATIVE_STATUS_OK;
}
