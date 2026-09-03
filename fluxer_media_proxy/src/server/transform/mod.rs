// SPDX-License-Identifier: AGPL-3.0-or-later

pub(in crate::server) mod cache;
pub mod cache_key;
pub(in crate::server) mod execution;
pub mod parameters;
#[cfg(test)]
mod tests;

use crate::{
    byte_budget::ByteBudget,
    byte_cache::ByteCacheSettings,
    coalescer::ByteCoalescer,
    config::Config,
    constants::AssetExtension,
    image_quality::ImageQuality,
    image_transform::{AnimationLimits, AnimationMode, ImageOptions, ResizeMode},
    media_limits::MediaLimits,
    media_process::MediaBytes,
    metrics::{self, transform::TransformMetrics},
    mime,
    output_format::OutputFormat,
    server::{
        format_policy::{
            OriginalImageRequest, content_type_is_trustworthy, default_transform_quality,
            effective_animated_image_output_format, extension_from_mime,
            external_default_output_extension, image_extension_from_filename, is_svg_content_type,
            same_format_loaded_image_request_can_use_original_with_sniff, source_image_format,
            transform_response_content_type, transform_static_quality_default,
        },
        native_task_executor::{NativeTaskExecutor, NativeTaskExecutorSettings},
        response::{
            MediaResponse, content_disposition_header,
            error::{text_with_reason, text_with_source},
            media_response,
        },
        transform::{
            cache::{
                CachedTransformRequest, cached_transform, cached_transform_hit,
                coalescer_failure_response,
            },
            cache_key::{TransformCacheKeyInput, transform_cache_key},
            execution::{
                VideoTransformOptions, coalesced_work_result, deadline_instant, run_transform,
                run_video_transform,
            },
            parameters::{
                TransformRoute, ValidatedTransformParameters, transform_parameter_error_response,
                validate_transform_parameters,
            },
        },
    },
    transform_cache::{TransformCache, TransformCacheSettings},
};
use axum::{
    http::{HeaderMap, Method, StatusCode, header},
    response::Response,
};
use bytes::Bytes;
use std::{collections::HashMap, sync::Arc};

const DECODED_BYTES_PER_PIXEL: usize = 4;
const DECODED_PIXEL_BUFFERS_PER_TRANSFORM: usize = 2;
const CONTENT_TYPE_SNIFF_PREFIX_BYTES: usize = 8192;

pub(in crate::server) struct TransformRuntime {
    limits: MediaLimits,
    animation: AnimationLimits,
    transform_timeout_ms: u64,
    tasks: NativeTaskExecutor,
    cache: TransformCache,
    metrics: Arc<TransformMetrics>,
}

pub(in crate::server) struct ServeBytesRequest<'a> {
    pub(in crate::server) method: Method,
    pub(in crate::server) data: Bytes,
    pub(in crate::server) content_type: String,
    pub(in crate::server) cache_identity: &'a str,
    pub(in crate::server) filename: &'a str,
    pub(in crate::server) route: TransformRoute,
    pub(in crate::server) params: &'a HashMap<String, String>,
    pub(in crate::server) headers: &'a HeaderMap,
}

impl TransformRuntime {
    pub(in crate::server) fn new(
        cfg: &Config,
        metrics: &Arc<metrics::Metrics>,
    ) -> anyhow::Result<Self> {
        let limits = MediaLimits::default_from_config();
        Ok(Self {
            limits,
            animation: AnimationLimits::new(
                cfg.media.max_encode_frames,
                cfg.media.max_encode_duration_ms,
            )?,
            transform_timeout_ms: cfg.media.transform_timeout_ms,
            tasks: NativeTaskExecutor::new(NativeTaskExecutorSettings {
                max_native_transforms: cfg.media.max_native_transforms,
                worker_queue_capacity: cfg.media.worker_queue_capacity,
                decoded_bytes_per_transform: decoded_bytes_per_transform(&limits),
                native_metrics: metrics.native_transform(),
                transform_metrics: metrics.transform(),
            }),
            cache: TransformCache::new(TransformCacheSettings {
                cache: ByteCacheSettings::clamped(
                    cfg.media.transform_cache_capacity_bytes,
                    cfg.media.transform_cache_max_entry_bytes,
                    cfg.media.transform_cache_ttl_ms,
                ),
                budget: ByteBudget::new(usize::MAX),
                max_in_flight: ByteCoalescer::UNBOUNDED_CAPACITY,
                max_waiters: ByteCoalescer::UNBOUNDED_CAPACITY,
                cache_metrics: metrics.transform_cache(),
                coalescer_metrics: metrics.coalescer(),
            }),
            metrics: metrics.transform(),
        })
    }

    pub(in crate::server) fn limits(&self) -> MediaLimits {
        self.limits
    }

    pub(in crate::server) fn animation(&self) -> AnimationLimits {
        self.animation
    }

    pub(in crate::server) fn cache(&self) -> &TransformCache {
        &self.cache
    }

    pub(in crate::server) fn tasks(&self) -> &NativeTaskExecutor {
        &self.tasks
    }

    pub(in crate::server) fn metrics(&self) -> Arc<TransformMetrics> {
        Arc::clone(&self.metrics)
    }

    pub(in crate::server) fn transform_deadline_ms(&self) -> Option<i64> {
        Some(metrics::now_ms() + self.transform_timeout_ms as i64)
    }
}

fn decoded_bytes_per_transform(limits: &MediaLimits) -> usize {
    limits
        .image_pixels()
        .max(limits.animated_total_pixels())
        .checked_mul(DECODED_BYTES_PER_PIXEL * DECODED_PIXEL_BUFFERS_PER_TRANSFORM)
        .expect("the native decoded image budget must not overflow")
}

pub(in crate::server) async fn serve_bytes_or_transform(
    runtime: &TransformRuntime,
    request: ServeBytesRequest<'_>,
) -> Response {
    let ServeBytesRequest {
        method,
        data,
        content_type,
        cache_identity,
        filename,
        route,
        params,
        headers,
    } = request;
    let transform = match validate_transform_parameters(params, &runtime.limits(), route) {
        Ok(transform) => transform,
        Err(error) => return transform_parameter_error_response(error),
    };
    let prefix = &data[..data.len().min(CONTENT_TYPE_SNIFF_PREFIX_BYTES)];
    let content_type = if mime::sniff(prefix).mime == "image/svg+xml" {
        "image/svg+xml".to_owned()
    } else if content_type_is_trustworthy(&content_type) {
        content_type
    } else {
        mime::detect(prefix, filename, Some(&content_type))
    };
    let source_is_svg = is_svg_content_type(&content_type)
        || image_extension_from_filename(filename) == Some(AssetExtension::Svg);
    let range_header = headers.get(header::RANGE).and_then(|v| v.to_str().ok());

    if !source_is_svg && !transform.has_transform_request {
        return original_bytes_response(OriginalBytes {
            method,
            data,
            content_type: &content_type,
            filename,
            range_header,
            requested_download: transform.requested_download,
        });
    }

    match mime::category(&content_type) {
        Some(mime::Category::Video) => {
            serve_video_transform(
                runtime,
                VideoTransformRequest {
                    method,
                    data,
                    content_type: &content_type,
                    cache_identity,
                    filename,
                    route,
                    transform,
                    range_header,
                },
            )
            .await
        }
        Some(mime::Category::Image) => {
            serve_image_transform(
                runtime,
                ImageTransformRequest {
                    method,
                    data,
                    content_type: &content_type,
                    cache_identity,
                    filename,
                    route,
                    transform,
                    range_header,
                },
            )
            .await
        }
        _ => {
            if route == TransformRoute::Attachment && transform.explicit_format.is_some() {
                return text_with_reason(
                    StatusCode::BAD_REQUEST,
                    "Bad Request",
                    "media_transform_unsupported",
                );
            }
            original_bytes_response(OriginalBytes {
                method,
                data,
                content_type: &content_type,
                filename,
                range_header,
                requested_download: transform.requested_download,
            })
        }
    }
}

struct OriginalBytes<'a> {
    method: Method,
    data: Bytes,
    content_type: &'a str,
    filename: &'a str,
    range_header: Option<&'a str>,
    requested_download: bool,
}

fn original_bytes_response(request: OriginalBytes<'_>) -> Response {
    let disposition = Some(content_disposition_header(
        request.content_type,
        request.requested_download,
        Some(request.filename),
    ));
    media_response(MediaResponse {
        method: request.method,
        data: request.data.into(),
        content_type: request.content_type,
        range_header: request.range_header,
        disposition,
    })
}

struct VideoTransformRequest<'a> {
    method: Method,
    data: Bytes,
    content_type: &'a str,
    cache_identity: &'a str,
    filename: &'a str,
    route: TransformRoute,
    transform: ValidatedTransformParameters,
    range_header: Option<&'a str>,
}

async fn serve_video_transform(
    runtime: &TransformRuntime,
    request: VideoTransformRequest<'_>,
) -> Response {
    let VideoTransformRequest {
        method,
        data,
        content_type,
        cache_identity,
        filename,
        route,
        transform,
        range_header,
    } = request;
    let Some(requested_format) = transform.explicit_format else {
        if route == TransformRoute::Attachment {
            return text_with_reason(
                StatusCode::BAD_REQUEST,
                "Bad Request",
                "video_transform_format_required",
            );
        }
        return original_bytes_response(OriginalBytes {
            method,
            data,
            content_type,
            filename,
            range_header,
            requested_download: transform.requested_download,
        });
    };
    let format = OutputFormat::coerce_from_extension(requested_format);
    let quality = transform.quality.unwrap_or(ImageQuality::High);
    let cache_key = transform_cache_key(TransformCacheKeyInput {
        route,
        asset_kind: None,
        cache_identity,
        width: transform.width,
        height: transform.height,
        format,
        quality: Some(quality),
        animated: transform.animated,
        effort: None,
        resize_mode: None,
    });
    if let Some(hit) = cached_transform_hit(runtime, &cache_key) {
        return transformed_response(TransformedBytes {
            method,
            data: hit.data.as_bytes().clone(),
            content_type: hit.format.mime(),
            filename,
            range_header,
            requested_download: transform.requested_download,
        });
    }
    let deadline_ms = runtime.transform_deadline_ms();
    let transformed = match cached_transform(CachedTransformRequest {
        runtime,
        cache_key,
        format,
        deadline: deadline_instant(deadline_ms),
        work: move || async move {
            coalesced_work_result(
                run_video_transform(
                    runtime,
                    data,
                    VideoTransformOptions {
                        format,
                        width: transform.width,
                        height: transform.height,
                        quality,
                        deadline_ms,
                    },
                )
                .await,
            )
            .map(|media| MediaBytes::from(media.bytes))
        },
    })
    .await
    {
        Ok(bytes) => bytes.as_bytes().clone(),
        Err(error) => {
            if let Some(response) = coalescer_failure_response(error, "coalescer_timeout_video") {
                return response;
            }
            return text_with_source(
                StatusCode::BAD_REQUEST,
                "Bad Request",
                "video_transform_failed",
                format!(
                    "fmt={} w={:?} h={:?} q={}",
                    format.extension(),
                    transform.width,
                    transform.height,
                    quality
                ),
            );
        }
    };
    transformed_response(TransformedBytes {
        method,
        data: transformed,
        content_type: format.mime(),
        filename,
        range_header,
        requested_download: transform.requested_download,
    })
}

struct ImageTransformRequest<'a> {
    method: Method,
    data: Bytes,
    content_type: &'a str,
    cache_identity: &'a str,
    filename: &'a str,
    route: TransformRoute,
    transform: ValidatedTransformParameters,
    range_header: Option<&'a str>,
}

async fn serve_image_transform(
    runtime: &TransformRuntime,
    request: ImageTransformRequest<'_>,
) -> Response {
    let ImageTransformRequest {
        method,
        data,
        content_type,
        cache_identity,
        filename,
        route,
        transform,
        range_header,
    } = request;
    let sniffed = mime::sniff(&data);
    let source_format = source_image_format(sniffed.mime, content_type, filename);
    let ImageTransformPlan {
        format,
        quality,
        resize_mode,
        response_content_type,
        cache_key,
    } = plan_image_transform(ImageTransformPlanRequest {
        content_type,
        cache_identity,
        filename,
        route,
        source_format,
        transform,
    });
    if same_format_loaded_image_request_can_use_original_with_sniff(
        sniffed,
        OriginalImageRequest {
            source_ext: source_format,
            explicit_out_ext: transform.explicit_format,
            out_ext: format,
            width: transform.width,
            height: transform.height,
            has_quality: transform.quality.is_some(),
            effort: transform.effort,
            animated: transform.animated,
        },
    ) {
        let serve_content_type = original_image_content_type(content_type, source_format);
        return original_bytes_response(OriginalBytes {
            method,
            data,
            content_type: &serve_content_type,
            filename,
            range_header,
            requested_download: transform.requested_download,
        });
    }
    let options = ImageOptions {
        width: transform.width,
        height: transform.height,
        format,
        quality,
        animation: AnimationMode::new(transform.animated, runtime.animation()),
        effort_override: transform.effort,
        resize_mode,
        deadline_ms: runtime.transform_deadline_ms(),
    };
    if let Some(hit) = cached_transform_hit(runtime, &cache_key) {
        return transformed_response(TransformedBytes {
            method,
            data: hit.data.as_bytes().clone(),
            content_type: response_content_type,
            filename,
            range_header,
            requested_download: transform.requested_download,
        });
    }
    let transformed = match cached_transform(CachedTransformRequest {
        runtime,
        cache_key,
        format: options.format,
        deadline: deadline_instant(options.deadline_ms),
        work: move || async move {
            coalesced_work_result(run_transform(runtime, data, options).await)
                .map(|media| MediaBytes::from(media.bytes))
        },
    })
    .await
    {
        Ok(bytes) => bytes.as_bytes().clone(),
        Err(error) => {
            if let Some(response) = coalescer_failure_response(error, "coalescer_timeout_image") {
                return response;
            }
            return text_with_source(
                StatusCode::BAD_REQUEST,
                "Bad Request",
                "image_transform_failed",
                format!(
                    "route={:?} cache_identity={} fmt={} w={:?} h={:?} q={} animated={}",
                    route,
                    cache_identity,
                    options.format.extension(),
                    options.width,
                    options.height,
                    options.quality,
                    transform.animated,
                ),
            );
        }
    };
    transformed_response(TransformedBytes {
        method,
        data: transformed,
        content_type: response_content_type,
        filename,
        range_header,
        requested_download: transform.requested_download,
    })
}

struct ImageTransformPlanRequest<'a> {
    content_type: &'a str,
    cache_identity: &'a str,
    filename: &'a str,
    route: TransformRoute,
    source_format: Option<AssetExtension>,
    transform: ValidatedTransformParameters,
}

struct ImageTransformPlan<'a> {
    format: OutputFormat,
    quality: ImageQuality,
    resize_mode: ResizeMode,
    response_content_type: &'a str,
    cache_key: String,
}

fn plan_image_transform<'a>(request: ImageTransformPlanRequest<'a>) -> ImageTransformPlan<'a> {
    let ImageTransformPlanRequest {
        content_type,
        cache_identity,
        filename,
        route,
        source_format,
        transform,
    } = request;
    let default_out_ext = match route {
        TransformRoute::External => external_default_output_extension(filename, content_type),
        TransformRoute::Attachment | TransformRoute::Stored | TransformRoute::Asset => {
            image_extension_from_filename(filename).unwrap_or(AssetExtension::Webp)
        }
    };
    let requested_format = transform.explicit_format.unwrap_or(default_out_ext);
    let requested_supported_format = OutputFormat::coerce_from_extension(requested_format);
    let format = effective_animated_image_output_format(
        source_format,
        requested_supported_format,
        transform.animated,
    );
    let quality = transform.quality.unwrap_or_else(|| {
        default_transform_quality(
            format,
            transform.animated,
            transform_static_quality_default(source_format),
        )
    });
    let resize_mode = if transform.wants_cover_crop() {
        ResizeMode::Cover
    } else {
        ResizeMode::Fit
    };
    ImageTransformPlan {
        format,
        quality,
        resize_mode,
        response_content_type: transform_response_content_type(
            transform.explicit_format,
            requested_format,
            format,
            content_type,
        ),
        cache_key: transform_cache_key(TransformCacheKeyInput {
            route,
            asset_kind: None,
            cache_identity,
            width: transform.width,
            height: transform.height,
            format,
            quality: Some(quality),
            animated: transform.animated,
            effort: transform.effort,
            resize_mode: Some(resize_mode),
        }),
    }
}

pub(in crate::server) struct CachedTransformProbe<'a> {
    pub(in crate::server) method: Method,
    pub(in crate::server) content_type: &'a str,
    pub(in crate::server) source_format: Option<AssetExtension>,
    pub(in crate::server) cache_identity: &'a str,
    pub(in crate::server) filename: &'a str,
    pub(in crate::server) route: TransformRoute,
    pub(in crate::server) params: &'a HashMap<String, String>,
    pub(in crate::server) headers: &'a HeaderMap,
}

// Answers a repeat transform from the cache without the caller fetching the source
// bytes again, so a hot remote asset costs one upstream fetch per cache lifetime.
pub(in crate::server) fn cached_transform_response(
    runtime: &TransformRuntime,
    probe: CachedTransformProbe<'_>,
) -> Option<Response> {
    let CachedTransformProbe {
        method,
        content_type,
        source_format,
        cache_identity,
        filename,
        route,
        params,
        headers,
    } = probe;
    let transform = validate_transform_parameters(params, &runtime.limits(), route).ok()?;
    let source_is_svg = is_svg_content_type(content_type)
        || image_extension_from_filename(filename) == Some(AssetExtension::Svg);
    if !source_is_svg && !transform.has_transform_request {
        return None;
    }
    let (cache_key, response_content_type) = match mime::category(content_type)? {
        mime::Category::Image => {
            let plan = plan_image_transform(ImageTransformPlanRequest {
                content_type,
                cache_identity,
                filename,
                route,
                source_format,
                transform,
            });
            (plan.cache_key, plan.response_content_type.to_owned())
        }
        mime::Category::Video => {
            let format = OutputFormat::coerce_from_extension(transform.explicit_format?);
            let quality = transform.quality.unwrap_or(ImageQuality::High);
            let cache_key = transform_cache_key(TransformCacheKeyInput {
                route,
                asset_kind: None,
                cache_identity,
                width: transform.width,
                height: transform.height,
                format,
                quality: Some(quality),
                animated: transform.animated,
                effort: None,
                resize_mode: None,
            });
            (cache_key, format.mime().to_owned())
        }
        mime::Category::Audio => return None,
    };
    let hit = cached_transform_hit(runtime, &cache_key)?;
    Some(transformed_response(TransformedBytes {
        method,
        data: hit.data.as_bytes().clone(),
        content_type: &response_content_type,
        filename,
        range_header: headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
        requested_download: transform.requested_download,
    }))
}

struct TransformedBytes<'a> {
    method: Method,
    data: Bytes,
    content_type: &'a str,
    filename: &'a str,
    range_header: Option<&'a str>,
    requested_download: bool,
}

fn transformed_response(request: TransformedBytes<'_>) -> Response {
    let disposition = Some(content_disposition_header(
        request.content_type,
        request.requested_download,
        Some(request.filename),
    ));
    media_response(MediaResponse {
        method: request.method,
        data: request.data.into(),
        content_type: request.content_type,
        range_header: request.range_header,
        disposition,
    })
}

pub(in crate::server) fn original_image_content_type(
    content_type: &str,
    source_format: Option<AssetExtension>,
) -> String {
    if content_type.is_empty()
        || content_type.eq_ignore_ascii_case("application/octet-stream")
        || extension_from_mime(content_type).is_none()
    {
        return source_format
            .map(|ext| ext.mime().to_owned())
            .unwrap_or_else(|| content_type.to_owned());
    }
    content_type.to_owned()
}
