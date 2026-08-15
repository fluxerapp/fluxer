// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::bootstrap::{build_bootstrap_script, inject_bootstrap};
use crate::csp::{RuntimeCspSources, build_csp, generate_nonce};
use crate::discovery_cache::DiscoveryResponse;
use crate::geoip::build_geoip_response;
use crate::invite_meta::{
    InviteMetaEndpoints, InvitePageMeta, inject_invite_meta, invite_code_from_path,
};
use crate::state::AppState;
use crate::time_freeze::{
    load_time_freeze_config_for_request, should_serve_frozen, time_freeze_debug_header,
};
use axum::{
    extract::{Request, State},
    http::{HeaderMap, HeaderName, HeaderValue, StatusCode, header},
    response::{IntoResponse, Response},
};
use std::path::Path;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use super::spa_static::{
    CORS_ALLOW_ANY_VALUE, guess_mime, is_font_mime, is_hashed_asset, is_static_asset,
};

const ACCEPT_CH_VALUE: &str = "DPR, Sec-CH-DPR, Sec-CH-Width, Save-Data, ECT, Downlink";
const CRITICAL_CH_VALUE: &str = "Sec-CH-DPR, Sec-CH-Width, Save-Data";
const DEV_NO_STORE_CACHE_CONTROL: &str = "no-store, no-cache, must-revalidate, max-age=0";

pub async fn spa_catch_all(
    State(state): State<AppState>,
    headers: HeaderMap,
    request: Request,
) -> Response {
    let request_path = request.uri().path();

    if is_static_asset(request_path) {
        return serve_static_file(&state.config.static_dir, request_path).await;
    }

    serve_spa_index(&state, &headers, request_path).await
}

async fn serve_static_file(static_dir: &str, request_path: &str) -> Response {
    let file_path = Path::new(static_dir).join(request_path.trim_start_matches('/'));

    let resolved = match tokio::fs::canonicalize(&file_path).await {
        Ok(p) => p,
        Err(_) => return StatusCode::NOT_FOUND.into_response(),
    };
    let base = match tokio::fs::canonicalize(static_dir).await {
        Ok(p) => p,
        Err(_) => return StatusCode::NOT_FOUND.into_response(),
    };
    if !resolved.starts_with(&base) {
        tracing::warn!(path = request_path, "directory traversal attempt blocked");
        return StatusCode::NOT_FOUND.into_response();
    }

    let content = match tokio::fs::read(&resolved).await {
        Ok(bytes) => bytes,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return StatusCode::NOT_FOUND.into_response();
        }
        Err(err) => {
            tracing::error!(path = request_path, %err, "failed to read static file");
            return (StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error").into_response();
        }
    };

    let mime_type = guess_mime(request_path);
    let cache_control = if is_hashed_asset(request_path) {
        "public, max-age=31536000, immutable"
    } else {
        "public, max-age=3600, must-revalidate"
    };

    let mut response = content.into_response();
    if let Ok(ct) = HeaderValue::from_str(mime_type) {
        response.headers_mut().insert(header::CONTENT_TYPE, ct);
    }
    if is_font_mime(mime_type) {
        response.headers_mut().insert(
            header::ACCESS_CONTROL_ALLOW_ORIGIN,
            HeaderValue::from_static(CORS_ALLOW_ANY_VALUE),
        );
    }
    response.headers_mut().insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static(cache_control),
    );
    response
}

async fn serve_spa_index(state: &AppState, headers: &HeaderMap, request_path: &str) -> Response {
    let time_freeze = load_time_freeze_config_for_request(&state.config, headers);
    let debug_header = time_freeze_debug_header(&time_freeze);
    let should_bust_dev_assets = state.config.index_upstream_url.is_some();

    let discovery = match refresh_discovery_for_spa(state).await {
        Some(d) => d,
        None => {
            tracing::error!("discovery cache empty, cannot serve SPA");
            return StatusCode::SERVICE_UNAVAILABLE.into_response();
        }
    };

    let nonce = generate_nonce();
    let runtime_csp_sources = build_runtime_csp_sources(state, &discovery);
    let invite_meta = resolve_invite_meta(state, request_path, &runtime_csp_sources).await;
    let static_cdn_endpoint = runtime_csp_sources
        .static_cdn_endpoint
        .as_deref()
        .unwrap_or("");
    let csp = build_csp(&state.config.csp, &nonce, &runtime_csp_sources);
    let geoip = build_geoip_response(state.geoip.lookup(headers));
    let script_tag = build_bootstrap_script(&state.config, &discovery, &geoip, &nonce);

    if let Some(snapshot) = should_serve_frozen(&time_freeze) {
        let frozen_html = String::from_utf8_lossy(&snapshot.index_html);
        let mut html = inject_bootstrap(&frozen_html, &nonce, &script_tag, static_cdn_endpoint);
        if let Some(meta) = &invite_meta {
            html = inject_invite_meta(&html, meta);
        }
        if should_bust_dev_assets {
            html = append_dev_asset_cache_buster(&html, &current_dev_asset_cache_buster());
        }
        return build_spa_response(html, &csp, debug_header.as_deref(), should_bust_dev_assets);
    }

    let raw_html = match load_spa_index_html(state).await {
        Ok(content) => content,
        Err(response) => return response,
    };

    let mut html = inject_bootstrap(&raw_html, &nonce, &script_tag, static_cdn_endpoint);
    if let Some(meta) = &invite_meta {
        html = inject_invite_meta(&html, meta);
    }
    if should_bust_dev_assets {
        html = append_dev_asset_cache_buster(&html, &current_dev_asset_cache_buster());
    }
    build_spa_response(html, &csp, debug_header.as_deref(), should_bust_dev_assets)
}

async fn refresh_discovery_for_spa(state: &AppState) -> Option<DiscoveryResponse> {
    if let Err(err) = state
        .discovery_cache
        .refresh(&state.http_client, &state.config.discovery_upstream_url)
        .await
    {
        tracing::warn!(
            %err,
            url = %state.config.discovery_upstream_url,
            "failed to refresh discovery before serving SPA; using cached discovery"
        );
    }
    state.discovery_cache.get().await
}

async fn resolve_invite_meta(
    state: &AppState,
    request_path: &str,
    runtime_csp_sources: &RuntimeCspSources,
) -> Option<InvitePageMeta> {
    let code = invite_code_from_path(request_path)?;
    let resolver = state.invite_meta.as_ref()?;
    let endpoints = InviteMetaEndpoints {
        media_endpoint: runtime_csp_sources.media_endpoint.clone(),
        static_cdn_endpoint: runtime_csp_sources.static_cdn_endpoint.clone(),
    };

    match resolver.resolve(code, &endpoints).await {
        Ok(meta) => meta,
        Err(err) => {
            tracing::warn!(%err, code, "failed to resolve invite metadata");
            None
        }
    }
}

fn build_runtime_csp_sources(state: &AppState, discovery: &DiscoveryResponse) -> RuntimeCspSources {
    RuntimeCspSources {
        static_cdn_endpoint: discovery_endpoint(discovery, "static_cdn")
            .or_else(|| state.config.static_cdn_endpoint.clone()),
        media_endpoint: discovery_endpoint(discovery, "media"),
        s3_public_endpoint: state.config.s3_public_endpoint.clone(),
        s3_uploads_bucket: Some(state.config.s3_uploads_bucket.clone()),
    }
}

fn discovery_endpoint(discovery: &DiscoveryResponse, key: &str) -> Option<String> {
    discovery
        .data
        .get("endpoints")
        .and_then(|endpoints| endpoints.get(key))
        .and_then(|value| value.as_str())
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(ToOwned::to_owned)
}

async fn load_spa_index_html(state: &AppState) -> Result<String, Response> {
    if let Some(index_upstream_url) = &state.config.index_upstream_url {
        let response = state
            .http_client
            .get(index_upstream_url)
            .timeout(Duration::from_secs(10))
            .send()
            .await
            .map_err(|err| {
                tracing::error!(url = %index_upstream_url, %err, "failed to fetch upstream index.html");
                StatusCode::BAD_GATEWAY.into_response()
            })?;
        if !response.status().is_success() {
            let status = response.status();
            tracing::error!(url = %index_upstream_url, %status, "upstream index.html returned non-success status");
            return Err(StatusCode::BAD_GATEWAY.into_response());
        }
        return response.text().await.map_err(|err| {
            tracing::error!(url = %index_upstream_url, %err, "failed to read upstream index.html body");
            StatusCode::BAD_GATEWAY.into_response()
        });
    }

    if let Some(cached) = &state.index_html {
        return Ok(cached.to_string());
    }

    let index_path = Path::new(&state.config.static_dir).join("index.html");
    tokio::fs::read_to_string(&index_path).await.map_err(|err| {
        tracing::error!(path = ?index_path, %err, "failed to read index.html");
        StatusCode::INTERNAL_SERVER_ERROR.into_response()
    })
}

fn build_spa_response(
    html: String,
    csp: &str,
    time_freeze_header: Option<&str>,
    dev_no_store: bool,
) -> Response {
    let mut response = html.into_response();
    let headers = response.headers_mut();

    if let Ok(v) = HeaderValue::from_str(csp) {
        headers.insert(header::CONTENT_SECURITY_POLICY, v);
    }
    headers.insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("text/html; charset=utf-8"),
    );
    if dev_no_store {
        headers.insert(
            header::CACHE_CONTROL,
            HeaderValue::from_static(DEV_NO_STORE_CACHE_CONTROL),
        );
        headers.insert(header::PRAGMA, HeaderValue::from_static("no-cache"));
        headers.insert(header::EXPIRES, HeaderValue::from_static("0"));
        headers.insert(
            HeaderName::from_static("cdn-cache-control"),
            HeaderValue::from_static("no-store"),
        );
        headers.insert(
            HeaderName::from_static("cloudflare-cdn-cache-control"),
            HeaderValue::from_static("no-store"),
        );
    } else {
        headers.insert(header::CACHE_CONTROL, HeaderValue::from_static("no-cache"));
    }
    headers.insert(
        header::STRICT_TRANSPORT_SECURITY,
        HeaderValue::from_static("max-age=31536000; includeSubDomains; preload"),
    );
    headers.insert(
        header::X_CONTENT_TYPE_OPTIONS,
        HeaderValue::from_static("nosniff"),
    );
    headers.insert(header::X_FRAME_OPTIONS, HeaderValue::from_static("DENY"));
    headers.insert(
        header::REFERRER_POLICY,
        HeaderValue::from_static("strict-origin-when-cross-origin"),
    );
    headers.insert(
        axum::http::HeaderName::from_static("accept-ch"),
        HeaderValue::from_static(ACCEPT_CH_VALUE),
    );
    headers.insert(
        axum::http::HeaderName::from_static("critical-ch"),
        HeaderValue::from_static(CRITICAL_CH_VALUE),
    );
    headers.insert(
        axum::http::HeaderName::from_static("permissions-policy"),
        HeaderValue::from_static(super::PERMISSIONS_POLICY_VALUE),
    );

    #[cfg(feature = "time-freeze")]
    {
        if let Some(tf) = time_freeze_header
            && let Ok(v) = HeaderValue::from_str(tf)
        {
            headers.insert(axum::http::HeaderName::from_static("x-time-freeze"), v);
        }
    }
    #[cfg(not(feature = "time-freeze"))]
    let _ = time_freeze_header;

    response
}

fn current_dev_asset_cache_buster() -> String {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_millis().to_string())
        .unwrap_or_else(|_| "0".to_owned())
}

fn append_dev_asset_cache_buster(html: &str, buster: &str) -> String {
    let html = append_dev_asset_cache_buster_for_attr(html, "src", '"', buster);
    let html = append_dev_asset_cache_buster_for_attr(&html, "src", '\'', buster);
    let html = append_dev_asset_cache_buster_for_attr(&html, "href", '"', buster);
    append_dev_asset_cache_buster_for_attr(&html, "href", '\'', buster)
}

fn append_dev_asset_cache_buster_for_attr(
    html: &str,
    attr: &str,
    quote: char,
    buster: &str,
) -> String {
    let needle = format!("{attr}={quote}");
    let mut rest = html;
    let mut output = String::with_capacity(html.len() + 128);

    while let Some(index) = rest.find(&needle) {
        output.push_str(&rest[..index + needle.len()]);
        rest = &rest[index + needle.len()..];

        let Some(end_index) = rest.find(quote) else {
            output.push_str(rest);
            return output;
        };

        let value = &rest[..end_index];
        if should_cache_bust_asset_url(value) {
            output.push_str(&append_cache_buster_query(value, buster));
        } else {
            output.push_str(value);
        }
        output.push(quote);
        rest = &rest[end_index + quote.len_utf8()..];
    }

    output.push_str(rest);
    output
}

fn should_cache_bust_asset_url(value: &str) -> bool {
    let value = value.trim();
    if value.is_empty()
        || value.starts_with('#')
        || value.starts_with("data:")
        || value.starts_with("blob:")
        || value.starts_with("javascript:")
    {
        return false;
    }

    let path = value
        .split(['?', '#'])
        .next()
        .unwrap_or(value)
        .to_ascii_lowercase();
    if path.starts_with("/assets/") || path.starts_with("assets/") || path.contains("/assets/") {
        return !has_version_marker(value, &path);
    }
    if path.ends_with("/sw.js")
        || path == "/sw.js"
        || path.ends_with("/manifest.json")
        || path == "/manifest.json"
        || path.ends_with("/browserconfig.xml")
        || path == "/browserconfig.xml"
    {
        return true;
    }

    [
        ".css", ".js", ".mjs", ".wasm", ".png", ".jpg", ".jpeg", ".gif", ".webp", ".ico", ".svg",
        ".woff", ".woff2", ".ttf", ".eot",
    ]
    .iter()
    .any(|extension| path.ends_with(extension))
}

fn has_version_marker(value: &str, path: &str) -> bool {
    let filename = path.rsplit('/').next().unwrap_or(path);
    let stem = filename
        .rsplit_once('.')
        .map(|(stem, _)| stem)
        .unwrap_or(filename);
    if stem.split(['.', '-', '_']).any(is_hex_hash) {
        return true;
    }

    let query = value
        .split_once('#')
        .map(|(before_hash, _)| before_hash)
        .unwrap_or(value)
        .split_once('?')
        .map(|(_, query)| query)
        .unwrap_or("");

    query
        .split('&')
        .map(|part| part.split_once('=').map(|(key, _)| key).unwrap_or(part))
        .any(|key| key != "_" && is_hex_hash(key))
}

fn is_hex_hash(value: &str) -> bool {
    value.len() >= 8 && value.bytes().all(|byte| byte.is_ascii_hexdigit())
}

fn append_cache_buster_query(value: &str, buster: &str) -> String {
    let (before_hash, hash) = value
        .split_once('#')
        .map(|(before_hash, hash)| (before_hash, Some(hash)))
        .unwrap_or((value, None));
    let separator = if before_hash.contains('?') { '&' } else { '?' };
    match hash {
        Some(hash) => format!("{before_hash}{separator}_={buster}#{hash}"),
        None => format!("{before_hash}{separator}_={buster}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dev_asset_cache_buster_rewrites_script_and_link_assets() {
        let html = r#"<link rel="preconnect" href="https://example.test"><link href="/assets/main.css?abcdef1234567890"><script src="https://example.test/assets/main.abcdef1234567890.js"></script><script src="/assets/unversioned.js"></script><link rel="manifest" href="/manifest.json">"#;

        let rewritten = append_dev_asset_cache_buster(html, "123");

        assert!(rewritten.contains(r#"href="https://example.test""#));
        assert!(rewritten.contains(r#"href="/assets/main.css?abcdef1234567890""#));
        assert!(
            rewritten.contains(r#"src="https://example.test/assets/main.abcdef1234567890.js""#)
        );
        assert!(rewritten.contains(r#"src="/assets/unversioned.js?_=123""#));
        assert!(rewritten.contains(r#"href="/manifest.json?_=123""#));
    }

    #[test]
    fn dev_asset_cache_buster_preserves_hash_fragments() {
        assert_eq!(
            append_cache_buster_query("/assets/main.js?hash#module", "123"),
            "/assets/main.js?hash&_=123#module"
        );
    }

    #[test]
    fn dev_asset_cache_buster_skips_non_asset_urls() {
        assert!(!should_cache_bust_asset_url(
            "https://example.test/channels/@me"
        ));
        assert!(!should_cache_bust_asset_url("data:image/png;base64,abc"));
        assert!(should_cache_bust_asset_url(
            "https://example.test/web/favicon-32x32.png"
        ));
    }

    #[test]
    fn font_mime_types_are_cors_enabled() {
        assert!(is_font_mime("font/woff2"));
        assert!(is_font_mime("font/woff"));
        assert!(is_font_mime("font/ttf"));
        assert!(is_font_mime("font/otf"));
        assert!(is_font_mime("application/vnd.ms-fontobject"));
        assert!(!is_font_mime("text/css; charset=utf-8"));
        assert!(!is_font_mime("image/png"));
    }
}
