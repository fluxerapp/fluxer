// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::local_files::{LocalFileResponseOptions, LocalFileStore, serve_binary_file};
use crate::state::AppState;
use crate::static_asset_policy::{
    apply_asset_request_headers, apply_asset_response_policy, asset_cache_control,
    copy_asset_response_headers, guess_mime, is_font_mime,
};
use axum::{
    body::Body,
    extract::{Path, State},
    http::{HeaderMap, HeaderValue, StatusCode},
    response::{IntoResponse, Response},
};
use std::time::Duration;
use tokio::sync::{OwnedSemaphorePermit, TryAcquireError};

const ASSET_REQUEST_TIMEOUT: Duration = Duration::from_secs(15);
const MAX_ASSET_SIZE_BYTES: u64 = 100 * 1024 * 1024;
const BUILD_ASSET_HASH_LEN: usize = 16;

pub async fn proxy_assets(
    State(state): State<AppState>,
    Path(path): Path<String>,
    request: axum::extract::Request,
) -> Response {
    let cdn_endpoint = match &state.config.static_cdn_endpoint {
        Some(endpoint) if !state.bundled_app_assets => endpoint,
        _ => {
            return serve_local_asset(
                &state.local_files,
                &format!("assets/{path}"),
                request.headers(),
                state.csp.asset_header(),
            )
            .await;
        }
    };

    if !is_build_asset_path(&path) {
        return StatusCode::NOT_FOUND.into_response();
    }

    let target_url = format!("{}/assets/{path}", cdn_endpoint.as_str());

    let upstream_slot = match state
        .budgets
        .upstream_asset_slots
        .clone()
        .try_acquire_owned()
    {
        Ok(permit) => permit,
        Err(TryAcquireError::NoPermits) => return super::capacity_refused_response(),
        Err(TryAcquireError::Closed) => {
            panic!("upstream asset slot semaphore closed unexpectedly")
        }
    };

    let request_builder = state
        .http_client
        .get(&target_url)
        .timeout(ASSET_REQUEST_TIMEOUT);
    let request_builder = apply_asset_request_headers(request_builder, request.headers());

    let upstream_response = match request_builder.send().await {
        Ok(resp) => resp,
        Err(err) => {
            tracing::error!(path = %path, target = %target_url, %err, "assets proxy error");
            return StatusCode::BAD_GATEWAY.into_response();
        }
    };

    if let Some(content_length) = upstream_response.content_length()
        && content_length > MAX_ASSET_SIZE_BYTES
    {
        tracing::warn!(
            path = %path,
            content_length,
            "upstream asset exceeds size cap"
        );
        return StatusCode::PAYLOAD_TOO_LARGE.into_response();
    }

    let status = StatusCode::from_u16(upstream_response.status().as_u16())
        .unwrap_or(StatusCode::BAD_GATEWAY);
    let mut response_headers = axum::http::HeaderMap::new();
    copy_asset_response_headers(upstream_response.headers(), &mut response_headers);
    apply_asset_response_policy(
        &mut response_headers,
        &path,
        status,
        state.csp.asset_header(),
    );

    let body = upstream_asset_body(upstream_response, upstream_slot);
    let mut response = Response::new(body);
    *response.status_mut() = status;
    *response.headers_mut() = response_headers;
    response
}

struct UpstreamAssetReadState {
    response: reqwest::Response,
    _permit: OwnedSemaphorePermit,
    remaining_bytes: u64,
}

fn upstream_asset_body(
    upstream_response: reqwest::Response,
    upstream_slot: OwnedSemaphorePermit,
) -> Body {
    let state = UpstreamAssetReadState {
        response: upstream_response,
        _permit: upstream_slot,
        remaining_bytes: MAX_ASSET_SIZE_BYTES,
    };
    Body::from_stream(futures_util::stream::try_unfold(
        state,
        |mut state| async move {
            let Some(chunk) = state
                .response
                .chunk()
                .await
                .inspect_err(|err| {
                    tracing::warn!(%err, "upstream asset body ended early");
                })
                .map_err(axum::Error::new)?
            else {
                return Ok(None);
            };
            let chunk_bytes = chunk.len() as u64;
            if chunk_bytes > state.remaining_bytes {
                tracing::warn!(
                    maximum_bytes = MAX_ASSET_SIZE_BYTES,
                    remaining_bytes = state.remaining_bytes,
                    chunk_bytes,
                    "upstream asset body exceeds size cap"
                );
                return Err(axum::Error::new(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("upstream asset body exceeds {MAX_ASSET_SIZE_BYTES} bytes"),
                )));
            }
            state.remaining_bytes -= chunk_bytes;
            Ok(Some((chunk, state)))
        },
    ))
}

fn is_build_asset_path(path: &str) -> bool {
    let (module, file) = match path.split_once('/') {
        Some((module, file)) => (Some(module), file),
        None => (None, path),
    };
    if module.is_some_and(|module| !is_module_name(module)) {
        return false;
    }
    let mut parts = file.split('.');
    let Some(first) = parts.next() else {
        return false;
    };
    let rest: Vec<&str> = parts.collect();
    let (name, hash, extension) = if is_content_hash(first) {
        ("", first, rest.as_slice())
    } else {
        match rest.split_first() {
            Some((hash, extension)) => (first, *hash, extension),
            None => return false,
        }
    };
    name.bytes()
        .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_'))
        && is_content_hash(hash)
        && !extension.is_empty()
        && extension.iter().all(|part| {
            !part.is_empty()
                && part
                    .bytes()
                    .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit())
        })
}

fn is_module_name(segment: &str) -> bool {
    segment.strip_prefix("fluxer_").is_some_and(|rest| {
        !rest.is_empty()
            && rest
                .bytes()
                .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'_')
    })
}

fn is_content_hash(segment: &str) -> bool {
    segment.len() == BUILD_ASSET_HASH_LEN
        && segment
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

pub fn bundles_app_assets(static_dir: &str) -> bool {
    let Ok(entries) = std::fs::read_dir(std::path::Path::new(static_dir).join("assets")) else {
        return false;
    };
    entries.flatten().any(|entry| {
        entry
            .path()
            .extension()
            .is_some_and(|extension| extension == "js")
    })
}

pub(super) async fn serve_local_asset(
    files: &LocalFileStore,
    relative_path: &str,
    request_headers: &HeaderMap,
    csp_asset_header: HeaderValue,
) -> Response {
    let mime_type = guess_mime(relative_path);
    let mut response = serve_binary_file(
        files,
        relative_path,
        request_headers,
        LocalFileResponseOptions {
            content_type: mime_type,
            cache_control: asset_cache_control(relative_path),
            allow_cross_origin: is_font_mime(mime_type),
            max_bytes: MAX_ASSET_SIZE_BYTES as usize,
        },
    )
    .await;
    let status = response.status();
    apply_asset_response_policy(
        response.headers_mut(),
        relative_path,
        status,
        csp_asset_header,
    );
    response
}
