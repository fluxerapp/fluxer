// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::csp::{RuntimeCspSources, build_asset_csp};
use crate::state::AppState;
use axum::{
    body::Body,
    extract::{Path, State},
    http::{HeaderMap, HeaderValue, StatusCode, header},
    response::{IntoResponse, Response},
};
use std::path::Path as FsPath;
use std::time::Duration;

use super::spa_static::{CORS_ALLOW_ANY_VALUE, guess_mime, is_font_mime, is_hashed_asset};

const ASSET_REQUEST_TIMEOUT: Duration = Duration::from_secs(15);
const MAX_ASSET_SIZE_BYTES: u64 = 100 * 1024 * 1024;

const BLOCKED_REQUEST_HEADERS: &[&str] = &[
    "accept-encoding",
    "authorization",
    "connection",
    "cookie",
    "host",
    "keep-alive",
    "proxy-authenticate",
    "proxy-authorization",
    "te",
    "trailer",
    "trailers",
    "transfer-encoding",
    "upgrade",
];

const BLOCKED_RESPONSE_HEADERS: &[&str] = &[
    "connection",
    "keep-alive",
    "proxy-authenticate",
    "proxy-authorization",
    "te",
    "trailer",
    "trailers",
    "transfer-encoding",
    "upgrade",
];

pub async fn proxy_assets(
    State(state): State<AppState>,
    Path(path): Path<String>,
    request: axum::extract::Request,
) -> Response {
    let Some(cdn_endpoint) = &state.config.static_cdn_endpoint else {
        return serve_local_asset(&state.config.static_dir, &format!("assets/{path}")).await;
    };

    let target_url = format!("{cdn_endpoint}/assets/{path}");

    let upstream_host = cdn_endpoint
        .trim_start_matches("https://")
        .trim_start_matches("http://")
        .split('/')
        .next()
        .unwrap_or("localhost");

    let mut request_builder = state
        .http_client
        .get(&target_url)
        .timeout(ASSET_REQUEST_TIMEOUT);

    for (name, value) in request.headers() {
        let name_str = name.as_str();
        if BLOCKED_REQUEST_HEADERS.contains(&name_str) {
            continue;
        }
        request_builder = request_builder.header(name.clone(), value.clone());
    }
    request_builder = request_builder.header("host", upstream_host);

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

    let status = upstream_response.status();
    let mut response_headers = axum::http::HeaderMap::new();

    for (name, value) in upstream_response.headers() {
        let name_str = name.as_str();
        if BLOCKED_RESPONSE_HEADERS.contains(&name_str) {
            continue;
        }
        if name_str == "content-encoding" || name_str == "content-length" {
            continue;
        }
        response_headers.insert(name.clone(), value.clone());
    }
    set_known_asset_content_type(&mut response_headers, &path);
    set_font_cors(&mut response_headers);

    let asset_csp = build_asset_csp(
        &state.config.csp,
        &RuntimeCspSources {
            static_cdn_endpoint: state.config.static_cdn_endpoint.clone(),
            media_endpoint: None,
            s3_public_endpoint: None,
            s3_uploads_bucket: None,
        },
    );
    if let Ok(value) = HeaderValue::from_str(&asset_csp) {
        response_headers.insert(header::CONTENT_SECURITY_POLICY, value);
    }
    response_headers.remove("content-security-policy-report-only");

    let body = Body::from_stream(upstream_response.bytes_stream());
    let mut response = Response::new(body);
    *response.status_mut() =
        StatusCode::from_u16(status.as_u16()).unwrap_or(StatusCode::BAD_GATEWAY);
    *response.headers_mut() = response_headers;
    response
}

async fn serve_local_asset(static_dir: &str, relative_path: &str) -> Response {
    let file_path = FsPath::new(static_dir).join(relative_path);

    let resolved = match tokio::fs::canonicalize(&file_path).await {
        Ok(path) => path,
        Err(_) => return StatusCode::NOT_FOUND.into_response(),
    };
    let base = match tokio::fs::canonicalize(static_dir).await {
        Ok(path) => path,
        Err(_) => return StatusCode::NOT_FOUND.into_response(),
    };
    if !resolved.starts_with(&base) {
        tracing::warn!(path = relative_path, "directory traversal attempt blocked");
        return StatusCode::NOT_FOUND.into_response();
    }

    let content = match tokio::fs::read(&resolved).await {
        Ok(bytes) => bytes,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return StatusCode::NOT_FOUND.into_response();
        }
        Err(err) => {
            tracing::error!(path = relative_path, %err, "failed to read local asset");
            return (StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error").into_response();
        }
    };

    let mut response = content.into_response();
    let mime_type = guess_mime(relative_path);
    if let Ok(value) = HeaderValue::from_str(mime_type) {
        response.headers_mut().insert(header::CONTENT_TYPE, value);
    }
    if is_font_mime(mime_type) {
        response.headers_mut().insert(
            header::ACCESS_CONTROL_ALLOW_ORIGIN,
            HeaderValue::from_static(CORS_ALLOW_ANY_VALUE),
        );
    }
    let cache_control = if is_hashed_asset(relative_path) {
        "public, max-age=31536000, immutable"
    } else {
        "public, max-age=3600, must-revalidate"
    };
    response.headers_mut().insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static(cache_control),
    );
    response
}

fn set_font_cors(headers: &mut HeaderMap) {
    let is_font = headers
        .get(header::CONTENT_TYPE)
        .and_then(|value| value.to_str().ok())
        .map(|value| value.split(';').next().unwrap_or(value).trim())
        .is_some_and(is_font_mime);
    if is_font {
        headers.insert(
            header::ACCESS_CONTROL_ALLOW_ORIGIN,
            HeaderValue::from_static(CORS_ALLOW_ANY_VALUE),
        );
    }
}

fn set_known_asset_content_type(headers: &mut HeaderMap, path: &str) {
    let mime_type = guess_mime(path);
    if mime_type == "application/octet-stream" {
        return;
    }
    if let Ok(value) = HeaderValue::from_str(mime_type) {
        headers.insert(header::CONTENT_TYPE, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_js_asset_overrides_upstream_octet_stream() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/octet-stream"),
        );

        set_known_asset_content_type(&mut headers, "356aaade04a117b1.js");

        assert_eq!(
            headers
                .get(header::CONTENT_TYPE)
                .and_then(|value| value.to_str().ok()),
            Some("application/javascript; charset=utf-8")
        );
    }

    #[test]
    fn known_wasm_asset_overrides_upstream_octet_stream() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/octet-stream"),
        );

        set_known_asset_content_type(&mut headers, "voice_engine_bg.wasm");

        assert_eq!(
            headers
                .get(header::CONTENT_TYPE)
                .and_then(|value| value.to_str().ok()),
            Some("application/wasm")
        );
    }

    #[test]
    fn proxied_font_gains_cors_when_upstream_omits_it() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/octet-stream"),
        );

        set_known_asset_content_type(&mut headers, "0018072843a46dc4.woff2");
        set_font_cors(&mut headers);

        assert_eq!(
            headers
                .get(header::ACCESS_CONTROL_ALLOW_ORIGIN)
                .and_then(|value| value.to_str().ok()),
            Some("*")
        );
    }

    #[test]
    fn proxied_font_cors_overrides_a_narrower_upstream_value() {
        let mut headers = HeaderMap::new();
        headers.insert(header::CONTENT_TYPE, HeaderValue::from_static("font/woff2"));
        headers.insert(
            header::ACCESS_CONTROL_ALLOW_ORIGIN,
            HeaderValue::from_static("https://example.invalid"),
        );

        set_font_cors(&mut headers);

        assert_eq!(
            headers
                .get(header::ACCESS_CONTROL_ALLOW_ORIGIN)
                .and_then(|value| value.to_str().ok()),
            Some("*")
        );
    }

    #[test]
    fn proxied_font_cors_tolerates_a_content_type_parameter() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("font/woff2; charset=binary"),
        );

        set_font_cors(&mut headers);

        assert_eq!(
            headers
                .get(header::ACCESS_CONTROL_ALLOW_ORIGIN)
                .and_then(|value| value.to_str().ok()),
            Some("*")
        );
    }

    #[test]
    fn proxied_non_font_keeps_upstream_cors_untouched() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/javascript; charset=utf-8"),
        );

        set_font_cors(&mut headers);

        assert!(
            headers.get(header::ACCESS_CONTROL_ALLOW_ORIGIN).is_none(),
            "non-font assets are same-origin and must not gain a wildcard"
        );
    }

    #[test]
    fn unknown_asset_preserves_upstream_content_type() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("application/octet-stream"),
        );

        set_known_asset_content_type(&mut headers, "artifact.unknown-extension");

        assert_eq!(
            headers
                .get(header::CONTENT_TYPE)
                .and_then(|value| value.to_str().ok()),
            Some("application/octet-stream")
        );
    }
}
