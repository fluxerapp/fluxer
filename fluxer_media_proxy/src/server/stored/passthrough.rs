// SPDX-License-Identifier: AGPL-3.0-or-later

use super::response::{
    StreamingMediaResponse, passthrough_head_response, streaming_media_response,
};
use crate::{
    config::DeploymentMode,
    constants::{self, AssetExtension},
    http_headers,
    image_quality::ImageQuality,
    image_transform::AnimationMode,
    media_process, mime,
    output_format::OutputFormat,
    range,
    server::{
        download_stream::DownloadStreamPolicy,
        format_policy::{
            content_type_is_trustworthy, image_extension_from_filename, is_svg_content_type,
        },
        response::{
            MediaResponse, content_disposition_header, error::storage_error_response,
            error::text_with_source, media_response,
        },
        state::AppState,
        transform::{
            cache::{
                CachedTransformRequest, cached_transform, cached_transform_hit,
                coalescer_failure_response,
            },
            cache_key::{TransformCacheKeyInput, transform_cache_key},
            execution::{coalesced_work_result, deadline_instant, run_transform},
            parameters::TransformRoute,
        },
    },
    storage::StorageError,
};
use axum::{
    body::Body,
    http::{HeaderMap, HeaderValue, Method, StatusCode, header},
    response::Response,
};
use bytes::Bytes;
use std::sync::Arc;

pub(in crate::server) enum PassthroughDisposition<'a> {
    None,
    Attachment {
        requested_download: bool,
        filename: &'a str,
    },
}

pub(in crate::server) async fn serve_stored_raw(
    app: &Arc<AppState>,
    method: Method,
    bucket: &str,
    key: &str,
    headers: &HeaderMap,
) -> Response {
    serve_stored_passthrough_stream(
        app,
        method,
        bucket,
        key,
        headers,
        PassthroughDisposition::None,
    )
    .await
}

pub(in crate::server) async fn serve_stored_passthrough_stream(
    app: &Arc<AppState>,
    method: Method,
    bucket: &str,
    key: &str,
    headers: &HeaderMap,
    disposition: PassthroughDisposition<'_>,
) -> Response {
    // A GET is answered with a single upstream operation: the client's Range is forwarded
    // verbatim and the object store's own reply supplies the status, the content type, the
    // satisfied range and the total length. Probing with a HEAD first would double the object
    // store requests on the hottest path in the proxy.
    if method == Method::HEAD {
        return serve_stored_passthrough_head(app, bucket, key, headers, &disposition).await;
    }
    if app.cfg.mode == DeploymentMode::Mp
        && image_extension_from_filename(key) == Some(AssetExtension::Svg)
    {
        return serve_stored_passthrough_svg(app, method, bucket, key, headers, &disposition).await;
    }
    let range_header = headers.get(header::RANGE).and_then(|v| v.to_str().ok());
    let forwarded_range = match range::classify_request_range(range_header) {
        range::RequestRange::Absent => None,
        range::RequestRange::Forwardable(value) => Some(value),
        range::RequestRange::Unsatisfiable => {
            return passthrough_unsatisfiable_response(app, bucket, key, None).await;
        }
    };
    let object = match app.store.stream_object(bucket, key, forwarded_range).await {
        Ok(object) => object,
        Err(err) => return storage_error_response(key, err),
    };
    if object.status == StatusCode::RANGE_NOT_SATISFIABLE {
        return passthrough_unsatisfiable_response(app, bucket, key, object.total_length).await;
    }
    let content_type = passthrough_content_type(&object.content_type, key);
    if app.cfg.mode == DeploymentMode::Mp && is_svg_content_type(&content_type) {
        return serve_stored_passthrough_svg(app, method, bucket, key, headers, &disposition).await;
    }
    let total_len = match passthrough_total_len(app, bucket, key, object.total_length).await {
        Ok(value) => value,
        Err(err) => return storage_error_response(key, err),
    };
    if total_len > constants::MAX_MEDIA_PROXY_BYTES {
        return storage_error_response(key, StorageError::StreamTooLong);
    }
    streaming_media_response(StreamingMediaResponse {
        method,
        object,
        total_len,
        content_type: &content_type,
        disposition: passthrough_disposition_header(&disposition, &content_type),
        stream_policy: DownloadStreamPolicy::for_passthrough(app.cfg.socket_io_timeout_ms),
    })
}

async fn serve_stored_passthrough_head(
    app: &Arc<AppState>,
    bucket: &str,
    key: &str,
    headers: &HeaderMap,
    disposition: &PassthroughDisposition<'_>,
) -> Response {
    let head = match app.store.head_object(bucket, key).await {
        Ok(head) => head,
        Err(err) => return storage_error_response(key, err),
    };
    if head.content_length > constants::MAX_MEDIA_PROXY_BYTES as u64 {
        return storage_error_response(key, StorageError::StreamTooLong);
    }
    let content_type = passthrough_content_type(&head.content_type, key);
    if app.cfg.mode == DeploymentMode::Mp
        && (is_svg_content_type(&content_type)
            || image_extension_from_filename(key) == Some(AssetExtension::Svg))
    {
        return serve_stored_passthrough_svg(app, Method::HEAD, bucket, key, headers, disposition)
            .await;
    }
    let total_len = match usize::try_from(head.content_length) {
        Ok(value) => value,
        Err(_) => return storage_error_response(key, StorageError::StreamTooLong),
    };
    let range_header = headers.get(header::RANGE).and_then(|v| v.to_str().ok());
    let byte_range = match range::parse_range(range_header, total_len) {
        range::RangeSelection::Full => None,
        range::RangeSelection::Partial(byte_range) => Some(byte_range),
        range::RangeSelection::Unsatisfiable => return unsatisfiable_response(total_len),
    };
    passthrough_head_response(
        &content_type,
        total_len,
        byte_range,
        passthrough_disposition_header(disposition, &content_type),
    )
}

async fn serve_stored_passthrough_svg(
    app: &Arc<AppState>,
    method: Method,
    bucket: &str,
    key: &str,
    headers: &HeaderMap,
    disposition: &PassthroughDisposition<'_>,
) -> Response {
    let object = match app.store.read_object(bucket, key).await {
        Ok(object) => object,
        Err(err) => return storage_error_response(key, err),
    };
    let cache_identity = format!("{bucket}/{key}");
    serve_stored_svg_rasterized(
        app,
        method,
        object.data,
        &cache_identity,
        headers,
        disposition,
    )
    .await
}

/// Resolves the object's full length, preferring what the store already reported on the streamed
/// reply so the common path never needs a second upstream request.
async fn passthrough_total_len(
    app: &Arc<AppState>,
    bucket: &str,
    key: &str,
    known: Option<u64>,
) -> Result<usize, StorageError> {
    let total = match known {
        Some(value) => value,
        None => app.store.head_object(bucket, key).await?.content_length,
    };
    usize::try_from(total).map_err(|_| StorageError::StreamTooLong)
}

async fn passthrough_unsatisfiable_response(
    app: &Arc<AppState>,
    bucket: &str,
    key: &str,
    known: Option<u64>,
) -> Response {
    match passthrough_total_len(app, bucket, key, known).await {
        Ok(total_len) => unsatisfiable_response(total_len),
        Err(err) => storage_error_response(key, err),
    }
}

fn unsatisfiable_response(total_len: usize) -> Response {
    let mut response = Response::new(Body::empty());
    *response.status_mut() = StatusCode::RANGE_NOT_SATISFIABLE;
    http_headers::add_unsatisfiable_headers(response.headers_mut(), total_len);
    response
}

fn passthrough_content_type(source_content_type: &str, key: &str) -> String {
    let extension_mime = mime::extension_mime(key);
    if extension_mime == Some("audio/mp4")
        && mime::normalize(Some(source_content_type)) == Some("video/mp4")
    {
        return "audio/mp4".to_owned();
    }
    if content_type_is_trustworthy(source_content_type) {
        source_content_type.to_owned()
    } else {
        extension_mime
            .or_else(|| {
                mime::normalize(Some(source_content_type)).filter(|value| {
                    !value.is_empty() && !value.eq_ignore_ascii_case("application/octet-stream")
                })
            })
            .unwrap_or("application/octet-stream")
            .to_owned()
    }
}

fn passthrough_disposition_header(
    disposition: &PassthroughDisposition<'_>,
    content_type: &str,
) -> Option<HeaderValue> {
    match disposition {
        PassthroughDisposition::None => None,
        PassthroughDisposition::Attachment {
            requested_download,
            filename,
        } => Some(content_disposition_header(
            content_type,
            *requested_download,
            Some(filename),
        )),
    }
}

async fn serve_stored_svg_rasterized(
    app: &Arc<AppState>,
    method: Method,
    data: Bytes,
    cache_identity: &str,
    headers: &HeaderMap,
    disposition: &PassthroughDisposition<'_>,
) -> Response {
    let runtime = app.media.transforms();
    let format = OutputFormat::WebP;
    let quality = ImageQuality::Lossless;
    let options = media_process::ImageOptions {
        format,
        quality,
        animation: AnimationMode::Static,
        deadline_ms: runtime.transform_deadline_ms(),
        ..Default::default()
    };
    let cache_key = transform_cache_key(TransformCacheKeyInput {
        route: TransformRoute::Stored,
        asset_kind: None,
        cache_identity,
        width: None,
        height: None,
        format,
        quality: Some(quality),
        animated: false,
        effort: None,
        resize_mode: Some(options.resize_mode),
    });
    if let Some(hit) = cached_transform_hit(runtime, &cache_key) {
        return media_response(MediaResponse {
            method,
            data: hit.data.as_bytes().clone().into(),
            content_type: hit.format.mime(),
            range_header: headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
            disposition: passthrough_disposition_header(disposition, hit.format.mime()),
        });
    }
    let transformed = match cached_transform(CachedTransformRequest {
        runtime,
        cache_key,
        format,
        deadline: deadline_instant(options.deadline_ms),
        work: move || async move {
            coalesced_work_result(run_transform(runtime, data, options).await)
                .map(|media| media_process::MediaBytes::from(media.bytes))
        },
    })
    .await
    {
        Ok(bytes) => bytes.as_bytes().clone(),
        Err(error) => {
            if let Some(response) =
                coalescer_failure_response(error, "coalescer_timeout_svg_rasterize")
            {
                return response;
            }
            return text_with_source(
                StatusCode::BAD_REQUEST,
                "Bad Request",
                "svg_rasterize_failed",
                cache_identity,
            );
        }
    };
    media_response(MediaResponse {
        method,
        data: transformed.into(),
        content_type: format.mime(),
        range_header: headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
        disposition: passthrough_disposition_header(disposition, format.mime()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn passthrough_content_type_preserves_non_media_metadata() {
        assert_eq!(
            "application/zip",
            passthrough_content_type("application/zip", "downloads/app.zip")
        );
    }

    #[test]
    fn passthrough_content_type_prefers_known_extension_over_bad_metadata() {
        assert_eq!(
            "image/png",
            passthrough_content_type("text/plain", "image.png")
        );
    }

    #[test]
    fn passthrough_content_type_prefers_m4a_extension_over_mp4_metadata() {
        assert_eq!(
            "audio/mp4",
            passthrough_content_type("video/mp4", "track.m4a")
        );
    }
}
