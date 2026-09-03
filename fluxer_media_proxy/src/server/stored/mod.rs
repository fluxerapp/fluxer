// SPDX-License-Identifier: AGPL-3.0-or-later

pub(in crate::server) mod passthrough;
pub(in crate::server) mod response;
#[cfg(test)]
mod tests;

use crate::{
    asset_size,
    coalescer::CoalescerError,
    constants::{self, AssetExtension, AssetKind},
    image_quality::ImageQuality,
    image_transform::{AnimationMode, ResizeMode},
    media_process, mime, output_format,
    server::{
        asset_path::{ParsedAssetPath, asset_filename_hint},
        format_policy::{
            OriginalImageRequest, default_transform_quality,
            effective_animated_image_output_format, extension_from_mime, is_svg_content_type,
            same_format_loaded_image_request_can_use_original_with_sniff,
        },
        params::{
            animated_param, asset_manual_format_override, asset_wants_animated, bool_param,
            filename_from_storage_key,
        },
        response::{
            MediaResponse, content_disposition_header,
            error::{storage_error_response, text_with_source},
            media_response,
        },
        state::AppState,
        transform::{
            ServeBytesRequest,
            cache::{
                CachedTransformRequest, cached_transform, cached_transform_hit,
                coalescer_failure_response,
            },
            cache_key::{TransformCacheKeyInput, transform_cache_key},
            execution::{coalesced_work_result, deadline_instant, run_transform},
            original_image_content_type,
            parameters::TransformRoute,
            serve_bytes_or_transform,
        },
    },
    storage::{self, StorageError},
};
use axum::{
    http::{HeaderMap, Method, StatusCode, header},
    response::Response,
};
pub(in crate::server) use passthrough::serve_stored_raw;
use passthrough::{PassthroughDisposition, serve_stored_passthrough_stream};
use std::{collections::HashMap, sync::Arc};

pub(in crate::server) async fn serve_asset_image(
    app: &Arc<AppState>,
    method: Method,
    asset: ParsedAssetPath,
    params: &HashMap<String, String>,
    headers: &HeaderMap,
) -> Response {
    let runtime = app.media.transforms();
    let range_header = headers.get(header::RANGE).and_then(|v| v.to_str().ok());
    let requested_download = bool_param(params, "download", false);
    let asset_filename = asset_filename_hint(&asset);
    let size = asset_size::parse_image_size(params.get("size").map(String::as_str));
    let requested_manual_format = asset_manual_format_override(params, asset.original_ext);
    let manual_format_override = asset.forced_output_format.or(requested_manual_format);
    let selected = output_format::select_url_variant(output_format::Input {
        kind: asset.kind,
        original: asset.original_ext,
        requested_size: Some(size),
        manual_format_override,
    });
    let animated = asset_wants_animated(params, &asset.hash);
    let requested_quality = params
        .get("quality")
        .map(|raw| ImageQuality::parse_lenient(raw));
    let width = selected.size;
    let height = selected.size;
    let resize_mode = if matches!(asset.kind, AssetKind::Emoji | AssetKind::Sticker) {
        ResizeMode::Cover
    } else {
        ResizeMode::Fit
    };
    let CdnSourceObject {
        object,
        content_identity,
    } = match read_cdn_object_with_fallback(app, &asset.storage_key, asset.original_ext).await {
        Ok(source) => source,
        Err(err) => return storage_error_response(&asset.storage_key, err),
    };
    let sniffed = mime::sniff(&object.data);
    let sniffed_source_ext = extension_from_mime(sniffed.mime);
    let source_format = if sniffed_source_ext == Some(AssetExtension::Apng) {
        AssetExtension::Apng
    } else {
        extension_from_mime(&object.content_type)
            .or(sniffed_source_ext)
            .unwrap_or(asset.original_ext)
    };
    let serve_content_type = original_image_content_type(&object.content_type, Some(source_format));
    let out_ext =
        effective_animated_image_output_format(Some(source_format), selected.format, animated);
    let quality = requested_quality
        .unwrap_or_else(|| default_transform_quality(out_ext, animated, ImageQuality::High));
    if same_format_loaded_image_request_can_use_original_with_sniff(
        sniffed,
        OriginalImageRequest {
            source_ext: Some(source_format),
            explicit_out_ext: requested_manual_format,
            out_ext,
            width,
            height,
            has_quality: requested_quality.is_some(),
            effort: None,
            animated,
        },
    ) {
        return media_response(MediaResponse {
            method,
            data: object.data.into(),
            content_type: &serve_content_type,
            range_header,
            disposition: Some(content_disposition_header(
                &serve_content_type,
                requested_download,
                Some(&asset_filename),
            )),
        });
    }
    let cache_key = transform_cache_key(TransformCacheKeyInput {
        route: TransformRoute::Asset,
        asset_kind: Some(asset.kind),
        cache_identity: &content_identity,
        width,
        height,
        format: out_ext,
        quality: Some(quality),
        animated,
        effort: None,
        resize_mode: Some(resize_mode),
    });
    if let Some(hit) = cached_transform_hit(runtime, &cache_key) {
        return media_response(MediaResponse {
            method,
            data: hit.data.as_bytes().clone().into(),
            content_type: hit.format.mime(),
            range_header,
            disposition: Some(content_disposition_header(
                hit.format.mime(),
                requested_download,
                Some(&asset_filename),
            )),
        });
    }
    let options = media_process::ImageOptions {
        width,
        height,
        format: out_ext,
        quality,
        animation: AnimationMode::new(animated, app.media.animation()),
        effort_override: None,
        resize_mode,
        deadline_ms: runtime.transform_deadline_ms(),
    };
    let source_data = object.data.clone();
    let transformed = match cached_transform(CachedTransformRequest {
        runtime,
        cache_key,
        format: out_ext,
        deadline: deadline_instant(options.deadline_ms),
        work: move || async move {
            coalesced_work_result(run_transform(runtime, source_data, options).await)
                .map(|media| media_process::MediaBytes::from(media.bytes))
        },
    })
    .await
    {
        Ok(bytes) => bytes.as_bytes().clone(),
        Err(error) => {
            let detail = format!(
                "asset key={} src_ct={} out={} size={:?} animated={}",
                asset.storage_key,
                object.content_type,
                out_ext.extension(),
                selected.size,
                animated,
            );
            return asset_transform_failure_response(AssetTransformFailure {
                method,
                error,
                object,
                range_header,
                source_format,
                detail,
            });
        }
    };
    media_response(MediaResponse {
        method,
        data: transformed.into(),
        content_type: out_ext.mime(),
        range_header,
        disposition: Some(content_disposition_header(
            out_ext.mime(),
            requested_download,
            Some(&asset_filename),
        )),
    })
}

struct AssetTransformFailure<'a> {
    method: Method,
    error: CoalescerError,
    object: storage::Object,
    range_header: Option<&'a str>,
    source_format: AssetExtension,
    detail: String,
}

fn asset_transform_failure_response(failure: AssetTransformFailure<'_>) -> Response {
    let src_ct = failure.object.content_type.as_str();
    let src_is_displayable = src_ct.starts_with("image/")
        && src_ct != "image/avif"
        && src_ct != "image/heic"
        && src_ct != "image/heif"
        && failure.source_format != AssetExtension::Svg
        && !is_svg_content_type(src_ct);
    if src_is_displayable && failure.error != CoalescerError::RequestTimeout {
        return media_response(MediaResponse {
            method: failure.method,
            data: failure.object.data.into(),
            content_type: &failure.object.content_type,
            range_header: failure.range_header,
            disposition: None,
        });
    }
    if let Some(response) =
        coalescer_failure_response(failure.error, "coalescer_timeout_asset_image")
    {
        return response;
    }
    text_with_source(
        StatusCode::INTERNAL_SERVER_ERROR,
        "Transcode Failed",
        "transcode_failed",
        failure.detail,
    )
}

struct CdnSourceObject {
    object: storage::Object,
    content_identity: String,
}

async fn read_cdn_object_with_fallback(
    app: &Arc<AppState>,
    key: &str,
    original_ext: AssetExtension,
) -> Result<CdnSourceObject, StorageError> {
    let bucket = &app.cfg.storage.bucket_cdn;
    let limit = constants::MAX_MEDIA_PROXY_BYTES;
    let budget = storage::unversioned_read_budget(limit);
    let read = match app
        .store
        .read_object_limited_with_digest(bucket, key, limit, &budget)
        .await
    {
        Err(StorageError::NotFound) => {
            let fallback_key = format!("{key}.{}", original_ext.name());
            app.store
                .read_object_limited_with_digest(bucket, &fallback_key, limit, &budget)
                .await?
        }
        other => other?,
    };
    Ok(CdnSourceObject {
        content_identity: hex::encode(
            read.content_digest
                .expect("a digested read always reports its content digest"),
        ),
        object: storage::Object {
            data: read.data.as_bytes().clone(),
            content_type: read.content_type,
        },
    })
}

pub(in crate::server) async fn serve_attachment(
    app: &Arc<AppState>,
    method: Method,
    key: &str,
    params: &HashMap<String, String>,
    headers: &HeaderMap,
) -> Response {
    let filename = filename_from_storage_key(key);
    let animated = animated_param(params, false);
    let wants_transform = params.contains_key("width")
        || params.contains_key("height")
        || params.contains_key("format")
        || params.contains_key("quality")
        || animated;
    if !wants_transform {
        return serve_stored_passthrough_stream(
            app,
            method,
            &app.cfg.storage.bucket_cdn,
            key,
            headers,
            PassthroughDisposition::Attachment {
                requested_download: bool_param(params, "download", false),
                filename,
            },
        )
        .await;
    }
    let object = match app
        .store
        .read_object(&app.cfg.storage.bucket_cdn, key)
        .await
    {
        Ok(object) => object,
        Err(err) => return storage_error_response(key, err),
    };
    serve_bytes_or_transform(
        app.media.transforms(),
        ServeBytesRequest {
            method,
            data: object.data,
            content_type: object.content_type,
            cache_identity: key,
            filename,
            route: TransformRoute::Attachment,
            params,
            headers,
        },
    )
    .await
}

pub(in crate::server) async fn serve_stored_with_override(
    app: &Arc<AppState>,
    method: Method,
    bucket: &str,
    key: &str,
    content_type: &str,
    headers: &HeaderMap,
) -> Response {
    let object = match app.store.read_object(bucket, key).await {
        Ok(object) => object,
        Err(err) => return storage_error_response(key, err),
    };
    media_response(MediaResponse {
        method,
        data: object.data.into(),
        content_type,
        range_header: headers.get(header::RANGE).and_then(|v| v.to_str().ok()),
        disposition: None,
    })
}
