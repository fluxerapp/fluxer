// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::bootstrap::{build_bootstrap_script, inject_bootstrap};
use crate::csp::{RuntimeCspSources, build_csp, generate_nonce, http_origin};
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

use super::assets_proxy::serve_local_asset;
use super::spa_static::{CORS_ALLOW_ANY_VALUE, guess_mime, is_font_mime};

const ACCEPT_CH_VALUE: &str = "DPR, Sec-CH-DPR, Sec-CH-Width, Save-Data, ECT, Downlink";
const CRITICAL_CH_VALUE: &str = "Sec-CH-DPR, Sec-CH-Width, Save-Data";
const DEV_NO_STORE_CACHE_CONTROL: &str = "no-store, no-cache, must-revalidate, max-age=0";

pub async fn spa_catch_all(
    State(state): State<AppState>,
    headers: HeaderMap,
    request: Request,
) -> Response {
    let request_path = request.uri().path();

    if let Some(cache_control) = static_root_file_cache_control(request_path) {
        return serve_static_file(&state.config.static_dir, request_path, cache_control).await;
    }
    if is_static_asset_path(request_path) {
        return serve_local_asset(
            &state.config.static_dir,
            request_path.trim_start_matches('/'),
            &headers,
        )
        .await;
    }

    serve_spa_index(&state, &headers, request_path).await
}

const CRAWL_CONTROL_CACHE_CONTROL: &str = "public, max-age=300, must-revalidate";

const STATIC_ROOT_FILES: &[(&str, &str)] = &[("/robots.txt", CRAWL_CONTROL_CACHE_CONTROL)];
const STATIC_ASSET_PREFIXES: &[&str] = &[
    "/avatars/",
    "/badges/",
    "/desktop/",
    "/embeds/",
    "/emoji/",
    "/libs/",
    "/marketing/",
    "/web/",
];

fn static_root_file_cache_control(request_path: &str) -> Option<&'static str> {
    STATIC_ROOT_FILES
        .iter()
        .find(|(candidate, _)| request_path.eq_ignore_ascii_case(candidate))
        .map(|(_, cache_control)| *cache_control)
}

fn is_static_asset_path(request_path: &str) -> bool {
    STATIC_ASSET_PREFIXES
        .iter()
        .any(|prefix| request_path.starts_with(prefix))
}

async fn serve_static_file(
    static_dir: &str,
    request_path: &str,
    cache_control: &'static str,
) -> Response {
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
    let media_endpoint = runtime_csp_sources.media_endpoint.as_deref().unwrap_or("");
    let csp = build_csp(&state.config.csp, &nonce, &runtime_csp_sources);
    let geoip = build_geoip_response(state.geoip.lookup(headers));
    let script_tag = build_bootstrap_script(&state.config, &discovery, &geoip, &nonce);

    if let Some(snapshot) = should_serve_frozen(&time_freeze) {
        let frozen_html = String::from_utf8_lossy(&snapshot.index_html);
        let dev_buster = should_bust_dev_assets.then(current_dev_asset_cache_buster);
        let html = render_spa_document(
            &frozen_html,
            &nonce,
            &script_tag,
            static_cdn_endpoint,
            media_endpoint,
            invite_meta.as_ref(),
            dev_buster.as_deref(),
        );
        return build_spa_response(html, &csp, debug_header.as_deref(), should_bust_dev_assets);
    }

    let raw_html = match load_spa_index_html(state).await {
        Ok(content) => content,
        Err(response) => return response,
    };

    let dev_buster = should_bust_dev_assets.then(current_dev_asset_cache_buster);
    let html = render_spa_document(
        &raw_html,
        &nonce,
        &script_tag,
        static_cdn_endpoint,
        media_endpoint,
        invite_meta.as_ref(),
        dev_buster.as_deref(),
    );
    build_spa_response(html, &csp, debug_header.as_deref(), should_bust_dev_assets)
}

fn render_spa_document(
    html: &str,
    nonce: &str,
    script_tag: &str,
    static_cdn_endpoint: &str,
    media_endpoint: &str,
    invite_meta: Option<&InvitePageMeta>,
    dev_asset_cache_buster: Option<&str>,
) -> String {
    let mut document =
        inject_bootstrap(html, nonce, script_tag, static_cdn_endpoint, media_endpoint);
    if let Some(meta) = invite_meta {
        document = inject_invite_meta(&document, meta);
    }
    if let Some(buster) = dev_asset_cache_buster {
        document = append_dev_asset_cache_buster(&document, buster);
    }
    document
}

async fn refresh_discovery_for_spa(state: &AppState) -> Option<DiscoveryResponse> {
    state
        .discovery_cache
        .get_or_cold_start(&state.http_client, &state.config.discovery_upstream_url)
        .await
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
        branding_image_origins: branding_image_origins(discovery),
    }
}

const BRANDING_IMAGE_KEYS: &[&str] = &[
    "icon_url",
    "symbol_url",
    "logo_url",
    "wordmark_url",
    "favicon_url",
];

fn branding_image_origins(discovery: &DiscoveryResponse) -> Vec<String> {
    let Some(branding) = discovery
        .data
        .get("app_public")
        .and_then(|app_public| app_public.get("branding"))
    else {
        return Vec::new();
    };
    let mut origins: Vec<String> = Vec::new();
    for key in BRANDING_IMAGE_KEYS {
        let Some(origin) = branding
            .get(*key)
            .and_then(|value| value.as_str())
            .and_then(http_origin)
        else {
            continue;
        };
        if !origins.contains(&origin) {
            origins.push(origin);
        }
    }
    origins
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

#[allow(clippy::result_large_err)]
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
    use super::super::spa_static::LONG_LIVED_ASSET_CACHE_CONTROL;
    use super::*;

    fn is_static_root_file(request_path: &str) -> bool {
        static_root_file_cache_control(request_path).is_some()
    }

    use crate::config::{AppProxyConfig, ReleaseChannel};
    use crate::discovery_cache::DiscoveryCache;
    use axum::Router;
    use axum::body::Body;
    use fluxer_common::config::GeoipSourceConfig;
    use fluxer_common::geoip::{GeoipConfig, GeoipResolver};
    use std::sync::Arc;

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
    fn only_declared_static_root_files_bypass_the_spa_document() {
        assert!(is_static_root_file("/robots.txt"));
        assert!(!is_static_root_file("/index.html"));
        assert!(!is_static_root_file("/channels/@me"));
    }

    #[test]
    fn spa_routes_containing_a_dot_still_render_the_document() {
        assert!(!is_static_root_file("/theme/my.custom.theme"));
        assert!(!is_static_root_file("/invite/abc.def"));
        assert!(!is_static_root_file("/users/1.2.3"));
    }

    const SHELL_WITH_A_NONCE_HOLE: &str = r#"<!doctype html><html><head><title>Fluxer</title><script nonce="{{CSP_NONCE_PLACEHOLDER}}"></script><script src="/assets/app.js"></script></head><body></body></html>"#;

    fn sample_invite_meta() -> InvitePageMeta {
        InvitePageMeta {
            title: "Join Sample Space".to_owned(),
            description: "A sample invite".to_owned(),
            image_url: None,
        }
    }

    #[test]
    fn the_rendered_document_always_carries_the_bootstrap_and_a_real_nonce() {
        let rendered = render_spa_document(
            SHELL_WITH_A_NONCE_HOLE,
            "reqnonce",
            "<script>booted</script>",
            "https://static.example.test",
            "",
            None,
            None,
        );

        assert!(!rendered.contains("{{CSP_NONCE_PLACEHOLDER}}"));
        assert!(rendered.contains(r#"nonce="reqnonce""#));
        assert!(rendered.contains("<script>booted</script>"));
    }

    #[test]
    fn invite_metadata_reaches_the_rendered_document_only_when_resolved() {
        let meta = sample_invite_meta();
        let with_meta = render_spa_document(
            SHELL_WITH_A_NONCE_HOLE,
            "reqnonce",
            "<script>booted</script>",
            "",
            "",
            Some(&meta),
            None,
        );
        let without_meta = render_spa_document(
            SHELL_WITH_A_NONCE_HOLE,
            "reqnonce",
            "<script>booted</script>",
            "",
            "",
            None,
            None,
        );

        assert!(with_meta.contains("Join Sample Space"));
        assert!(with_meta.contains("og:title"));
        assert!(!without_meta.contains("Join Sample Space"));
        assert!(!without_meta.contains("og:title"));
    }

    #[test]
    fn the_dev_cache_buster_reaches_the_rendered_document_only_when_supplied() {
        let busted = render_spa_document(
            SHELL_WITH_A_NONCE_HOLE,
            "reqnonce",
            "<script>booted</script>",
            "",
            "",
            None,
            Some("9911"),
        );
        let untouched = render_spa_document(
            SHELL_WITH_A_NONCE_HOLE,
            "reqnonce",
            "<script>booted</script>",
            "",
            "",
            None,
            None,
        );

        assert!(busted.contains(r#"src="/assets/app.js?_=9911""#));
        assert!(untouched.contains(r#"src="/assets/app.js""#));
        assert!(!untouched.contains("_=9911"));
    }

    #[cfg(feature = "time-freeze")]
    #[test]
    fn the_frozen_shell_is_never_served_with_an_unfilled_nonce_or_a_missing_bootstrap() {
        let frozen = String::from_utf8_lossy(&crate::frozen_snapshots::STABLE_SNAPSHOT.index_html)
            .into_owned();
        assert!(
            frozen.contains("{{CSP_NONCE_PLACEHOLDER}}"),
            "the captured shell should still carry the hole this test proves we fill"
        );

        let served = render_spa_document(
            &frozen,
            "frozennonce",
            "<script>frozenboot</script>",
            "https://fluxerstatic.com",
            "",
            None,
            None,
        );

        assert!(
            !served.contains("{{CSP_NONCE_PLACEHOLDER}}"),
            "a frozen-served shell shipped a literal nonce placeholder"
        );
        assert!(
            served.contains(r#"nonce="frozennonce""#),
            "a frozen-served shell lost its per-request nonce"
        );
        assert!(
            served.contains("<script>frozenboot</script>"),
            "a frozen-served shell shipped without the bootstrap script"
        );
    }

    const SHELL_WITH_ENDPOINT_HOLES: &str = r#"<!doctype html><html><head><title>Fluxer</title><link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">
<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}" crossorigin>
<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">
<link rel="icon" type="image/png" sizes="32x32" href="{{STATIC_CDN_ENDPOINT}}/web/favicon-32x32.png"><link rel="apple-touch-icon" sizes="180x180" href="{{STATIC_CDN_ENDPOINT}}/web/apple-touch-icon.png"><script nonce="{{CSP_NONCE_PLACEHOLDER}}"></script><script src="/assets/app.js"></script></head><body></body></html>"#;

    #[test]
    fn the_static_cdn_argument_resolves_every_hole_the_shell_carries() {
        let rendered = render_spa_document(
            SHELL_WITH_ENDPOINT_HOLES,
            "reqnonce",
            "<script>booted</script>",
            "https://cdn.example.test/",
            "https://media.example.test",
            None,
            None,
        );

        assert!(
            rendered
                .contains(r#"<link rel="preconnect" href="https://cdn.example.test" crossorigin>"#),
            "the static CDN argument never reached the anonymous preconnect"
        );
        assert!(
            rendered.contains(r#"<link rel="preconnect" href="https://cdn.example.test">"#),
            "the static CDN argument never reached the credentialed preconnect"
        );
        assert!(
            rendered.contains(r#"href="https://cdn.example.test/web/favicon-32x32.png""#),
            "the favicon href was not resolved against the static CDN argument"
        );
        assert!(
            rendered.contains(r#"href="https://cdn.example.test/web/apple-touch-icon.png""#),
            "the touch-icon href was not resolved against the static CDN argument"
        );
        assert!(!rendered.contains("{{STATIC_CDN_ENDPOINT}}"));
        assert_eq!(rendered.matches("preconnect").count(), 3);
    }

    #[test]
    fn the_media_argument_is_resolved_and_weighed_against_the_static_cdn() {
        let distinct = render_spa_document(
            SHELL_WITH_ENDPOINT_HOLES,
            "reqnonce",
            "<script>booted</script>",
            "https://cdn.example.test",
            "https://media.example.test/",
            None,
            None,
        );
        assert!(
            distinct.contains(r#"<link rel="preconnect" href="https://media.example.test">"#),
            "the media argument never reached the media preconnect"
        );
        assert!(!distinct.contains("{{MEDIA_ENDPOINT}}"));
        assert_eq!(distinct.matches("preconnect").count(), 3);

        let shared = render_spa_document(
            SHELL_WITH_ENDPOINT_HOLES,
            "reqnonce",
            "<script>booted</script>",
            "https://cdn.example.test",
            "https://cdn.example.test",
            None,
            None,
        );
        assert!(
            shared.contains(r#"<link rel="preconnect" href="https://cdn.example.test">"#),
            "the static preconnects must survive a media endpoint that collapses onto them"
        );
        assert_eq!(
            shared.matches("preconnect").count(),
            2,
            "a media endpoint equal to the static CDN must not warm a third socket"
        );
    }

    const DISCOVERY_BODY_WITH_BOTH_ENDPOINTS: &str = r#"{"api_code_version":"proxy-test","endpoints":{"static_cdn":"https://cdn.example.test","media":"https://media.example.test"}}"#;

    const DISCOVERY_BODY_WITHOUT_ENDPOINTS: &str = r#"{"api_code_version":"proxy-test"}"#;

    async fn spawn_local_origin(payload: &'static str, content_type: &'static str) -> String {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let router = Router::new().fallback(move || async move {
            let mut response = Response::new(Body::from(payload));
            response
                .headers_mut()
                .insert(header::CONTENT_TYPE, HeaderValue::from_static(content_type));
            response
        });
        tokio::spawn(async move {
            axum::serve(listener, router).await.unwrap();
        });
        format!("http://{addr}/")
    }

    async fn spa_state_serving(channel: ReleaseChannel, cached_shell: Option<&str>) -> AppState {
        assemble_spa_state(
            channel,
            cached_shell,
            DISCOVERY_BODY_WITH_BOTH_ENDPOINTS,
            None,
            None,
        )
        .await
    }

    async fn spa_state_reading_its_shell_from(index_upstream_url: String) -> AppState {
        assemble_spa_state(
            ReleaseChannel::Stable,
            None,
            DISCOVERY_BODY_WITH_BOTH_ENDPOINTS,
            None,
            Some(index_upstream_url),
        )
        .await
    }

    async fn spa_state_without_discovered_endpoints(static_cdn_fallback: Option<&str>) -> AppState {
        assemble_spa_state(
            ReleaseChannel::Canary,
            Some(SHELL_WITH_ENDPOINT_HOLES),
            DISCOVERY_BODY_WITHOUT_ENDPOINTS,
            static_cdn_fallback,
            None,
        )
        .await
    }

    async fn assemble_spa_state(
        channel: ReleaseChannel,
        cached_shell: Option<&str>,
        discovery_body: &'static str,
        static_cdn_fallback: Option<&str>,
        index_upstream_url: Option<String>,
    ) -> AppState {
        let discovery_upstream_url = spawn_local_origin(discovery_body, "application/json").await;
        let mut config = AppProxyConfig::from_env();
        config.release_channel = channel;
        config.time_freeze_enabled = true;
        config.index_upstream_url = index_upstream_url;
        config.static_cdn_endpoint = static_cdn_fallback.map(ToOwned::to_owned);
        config.trust_client_ip_header = false;
        config.discovery_upstream_url = discovery_upstream_url;

        AppState {
            config: Arc::new(config),
            http_client: reqwest::Client::new(),
            discovery_cache: Arc::new(DiscoveryCache::new()),
            geoip: Arc::new(GeoipResolver::from_config(&GeoipConfig {
                geoip_source: GeoipSourceConfig::Filesystem {
                    maxmind_db_path: None,
                },
                geoip_s3_config: None,
                trust_client_ip_header: false,
                client_ip_header_name: "x-forwarded-for".to_owned(),
            })),
            invite_meta: None,
            index_html: cached_shell.map(Arc::from),
        }
    }

    fn nonce_granted_by(response: &Response) -> String {
        let policy = response
            .headers()
            .get(header::CONTENT_SECURITY_POLICY)
            .expect("the document was served without a content security policy")
            .to_str()
            .unwrap();
        let opening = policy
            .find("'nonce-")
            .expect("the content security policy granted no nonce at all");
        let remainder = &policy[opening + "'nonce-".len()..];
        let closing = remainder
            .find('\'')
            .expect("the content security policy left its nonce source unterminated");
        remainder[..closing].to_owned()
    }

    async fn read_document(response: Response) -> String {
        let bytes = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        String::from_utf8(bytes.to_vec()).unwrap()
    }

    #[tokio::test]
    async fn the_live_branch_serves_a_rendered_document_and_not_the_raw_shell() {
        let state =
            spa_state_serving(ReleaseChannel::Canary, Some(SHELL_WITH_ENDPOINT_HOLES)).await;

        let response = serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await;
        assert_eq!(response.status(), StatusCode::OK);
        let granted_nonce = nonce_granted_by(&response);
        let served = read_document(response).await;

        assert!(
            !served.contains("{{CSP_NONCE_PLACEHOLDER}}"),
            "the live branch shipped an unfilled nonce hole"
        );
        assert!(
            served.contains(&format!(
                r#"<script nonce="{granted_nonce}">window.__FLUXER_BOOTSTRAP__"#
            )),
            "the live bootstrap was not granted the nonce its own policy header carries"
        );
        assert_eq!(
            served
                .matches(&format!(r#"nonce="{granted_nonce}""#))
                .count(),
            served.matches(r#"nonce=""#).count(),
            "the live document carries a nonce its own policy header never granted"
        );
        assert!(
            !served.contains("{{STATIC_CDN_ENDPOINT}}"),
            "the live branch shipped an unresolved static CDN hole"
        );
        assert!(
            !served.contains("{{MEDIA_ENDPOINT}}"),
            "the live branch shipped an unresolved media hole"
        );
        assert!(
            served.contains("window.__FLUXER_BOOTSTRAP__"),
            "the live branch shipped without a bootstrap script"
        );
        assert!(
            served
                .contains(r#"<link rel="preconnect" href="https://cdn.example.test" crossorigin>"#),
            "the discovered static CDN never reached the served document"
        );
        assert!(
            served.contains(r#"<link rel="preconnect" href="https://media.example.test">"#),
            "the discovered media endpoint never reached the served document"
        );
    }

    #[cfg(feature = "time-freeze")]
    #[tokio::test]
    async fn the_frozen_document_is_served_with_its_asset_urls_untouched() {
        let captured =
            String::from_utf8_lossy(&crate::frozen_snapshots::STABLE_SNAPSHOT.index_html)
                .into_owned();
        assert!(
            captured.contains(
                r#"<link rel="stylesheet" href="https://fluxerstatic.com/fonts/ibm-plex.css">"#
            ),
            "the captured shell no longer carries the asset URL this test proves we leave alone"
        );

        let state = spa_state_serving(ReleaseChannel::Stable, None).await;

        let response = serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await;
        assert_eq!(response.status(), StatusCode::OK);
        let served = read_document(response).await;

        assert!(
            served.contains(
                r#"<link rel="stylesheet" href="https://fluxerstatic.com/fonts/ibm-plex.css">"#
            ),
            "the frozen document's stylesheet URL was rewritten on the primary hosted path"
        );
        assert!(
            served.contains(r#"<link rel="manifest" href="/manifest.json">"#),
            "the frozen document's manifest URL was rewritten on the primary hosted path"
        );
        assert!(
            !served.contains("?_=") && !served.contains("&_="),
            "the frozen document was served with a development cache-busting query"
        );
    }

    #[cfg(feature = "time-freeze")]
    #[tokio::test]
    async fn the_frozen_branch_serves_a_rendered_document_and_not_the_raw_shell() {
        let captured =
            String::from_utf8_lossy(&crate::frozen_snapshots::STABLE_SNAPSHOT.index_html)
                .into_owned();
        assert!(
            captured.contains("{{CSP_NONCE_PLACEHOLDER}}"),
            "the captured shell no longer carries the nonce hole this test proves we fill"
        );
        assert!(
            !captured.contains("window.__FLUXER_BOOTSTRAP__"),
            "the captured shell already carries a bootstrap, so this test can no longer tell a rendered document from a raw shell"
        );

        let state = spa_state_serving(ReleaseChannel::Stable, None).await;

        let response = serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await;
        assert_eq!(response.status(), StatusCode::OK);
        let granted_nonce = nonce_granted_by(&response);
        let served = read_document(response).await;

        assert!(
            !served.contains("{{CSP_NONCE_PLACEHOLDER}}"),
            "the primary hosted path shipped a literal nonce placeholder"
        );
        assert_eq!(
            served
                .matches(&format!(r#"nonce="{granted_nonce}""#))
                .count(),
            served.matches(r#"nonce=""#).count(),
            "the frozen document carries a nonce its own policy header never granted"
        );
        assert!(
            served.contains(&format!(
                r#"<script nonce="{granted_nonce}">window.__FLUXER_BOOTSTRAP__"#
            )),
            "the primary hosted path shipped without a bootstrap script the browser will run"
        );

        let second = serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await;
        assert_ne!(
            nonce_granted_by(&second),
            granted_nonce,
            "two frozen requests were granted the same nonce"
        );
    }

    #[cfg(feature = "time-freeze")]
    #[tokio::test]
    async fn every_branch_announces_which_snapshot_decision_it_took() {
        let frozen_state = spa_state_serving(ReleaseChannel::Stable, None).await;
        let frozen = serve_spa_index(&frozen_state, &HeaderMap::new(), "/channels/@me").await;
        assert_eq!(
            frozen
                .headers()
                .get("x-time-freeze")
                .expect("the frozen response announced no snapshot decision")
                .to_str()
                .unwrap(),
            format!(
                "frozen; sha={}",
                crate::frozen_snapshots::STABLE_SNAPSHOT.sha
            ),
            "the frozen response named the wrong snapshot"
        );

        let live_state =
            spa_state_serving(ReleaseChannel::Canary, Some(SHELL_WITH_ENDPOINT_HOLES)).await;
        let live = serve_spa_index(&live_state, &HeaderMap::new(), "/channels/@me").await;
        assert_eq!(
            live.headers()
                .get("x-time-freeze")
                .expect("the live response announced no snapshot decision")
                .to_str()
                .unwrap(),
            "no-snapshot",
            "a channel with no snapshot claimed one anyway"
        );
    }

    #[cfg(feature = "time-freeze")]
    #[tokio::test]
    async fn the_frozen_shell_is_never_served_with_the_asset_lifetime() {
        let state = spa_state_serving(ReleaseChannel::Stable, None).await;

        let response = serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await;

        assert_eq!(
            response
                .headers()
                .get("x-time-freeze")
                .expect("the response announced no snapshot decision")
                .to_str()
                .unwrap(),
            format!(
                "frozen; sha={}",
                crate::frozen_snapshots::STABLE_SNAPSHOT.sha
            ),
            "this test never reached the frozen branch, so it proves nothing about it"
        );
        let cache_control = response
            .headers()
            .get(header::CACHE_CONTROL)
            .expect("the frozen shell was served without a cache policy at all")
            .to_str()
            .unwrap();
        assert_eq!(
            cache_control, "no-cache",
            "the frozen document naming the hashed bundle must be revalidated on every load"
        );
        assert_ne!(
            cache_control, LONG_LIVED_ASSET_CACHE_CONTROL,
            "a frozen shell cached for a year pins every returning visitor to the deployed-over bundle"
        );
    }

    #[tokio::test]
    async fn a_crawl_control_document_is_never_served_with_the_asset_lifetime() {
        let root = std::env::temp_dir().join(format!(
            "fluxer-app-proxy-crawl-control-{}",
            std::process::id()
        ));
        tokio::fs::create_dir_all(&root).await.unwrap();
        tokio::fs::write(root.join("robots.txt"), "User-agent: *\nDisallow:\n")
            .await
            .unwrap();
        let static_dir = root.to_str().unwrap();

        let policy = static_root_file_cache_control("/robots.txt")
            .expect("robots.txt is no longer served as a static root file");
        assert!(
            static_root_file_cache_control("/channels/@me").is_none(),
            "an application route was mistaken for a static root file"
        );

        let response = serve_static_file(static_dir, "/robots.txt", policy).await;
        assert_eq!(response.status(), StatusCode::OK);
        let cache_control = response
            .headers()
            .get(header::CACHE_CONTROL)
            .expect("the crawl-control document was served without a cache policy at all")
            .to_str()
            .unwrap();
        assert_eq!(cache_control, CRAWL_CONTROL_CACHE_CONTROL);
        assert_ne!(
            cache_control, LONG_LIVED_ASSET_CACHE_CONTROL,
            "a crawl rule change cannot reach a crawler that already fetched a year-long robots.txt"
        );

        tokio::fs::remove_dir_all(&root).await.unwrap();
    }

    #[tokio::test]
    async fn the_shell_is_never_served_with_the_asset_lifetime() {
        let state =
            spa_state_serving(ReleaseChannel::Canary, Some(SHELL_WITH_ENDPOINT_HOLES)).await;

        let response = serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await;

        let cache_control = response
            .headers()
            .get(header::CACHE_CONTROL)
            .expect("the shell was served without a cache policy at all")
            .to_str()
            .unwrap();
        assert_eq!(
            cache_control, "no-cache",
            "the document naming the hashed bundle must be revalidated on every load"
        );
        assert_ne!(
            cache_control, LONG_LIVED_ASSET_CACHE_CONTROL,
            "a shell cached for a year pins every returning visitor to the deployed-over bundle"
        );
    }

    #[tokio::test]
    async fn an_index_upstream_replaces_the_snapshot_with_an_unstorable_busted_document() {
        let index_upstream_url = spawn_local_origin(SHELL_WITH_ENDPOINT_HOLES, "text/html").await;
        let state = spa_state_reading_its_shell_from(index_upstream_url).await;

        let response = serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await;
        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            response
                .headers()
                .get(header::CACHE_CONTROL)
                .unwrap()
                .to_str()
                .unwrap(),
            DEV_NO_STORE_CACHE_CONTROL,
            "a document fetched from an index upstream was served as cacheable"
        );
        assert_eq!(
            response
                .headers()
                .get("cdn-cache-control")
                .expect("the edge was never told to skip storing this document")
                .to_str()
                .unwrap(),
            "no-store"
        );
        let served = read_document(response).await;

        assert!(
            served.contains(r#"src="/assets/app.js?_="#),
            "an index upstream served its assets without a cache-busting query"
        );
    }

    #[tokio::test]
    async fn the_configured_static_cdn_stands_in_when_discovery_names_none() {
        let state =
            spa_state_without_discovered_endpoints(Some("https://fallbackcdn.example.test")).await;

        let response = serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await;
        assert_eq!(response.status(), StatusCode::OK);
        let served = read_document(response).await;

        assert!(
            served.contains(
                r#"<link rel="preconnect" href="https://fallbackcdn.example.test" crossorigin>"#
            ),
            "the configured static CDN never reached the anonymous preconnect"
        );
        assert!(
            served.contains(r#"<link rel="preconnect" href="https://fallbackcdn.example.test">"#),
            "the configured static CDN never reached the credentialed preconnect"
        );
        assert!(
            served.contains(r#"href="https://fallbackcdn.example.test/web/favicon-32x32.png""#),
            "the favicon was not resolved against the configured static CDN"
        );
        assert!(!served.contains("{{STATIC_CDN_ENDPOINT}}"));
        assert_eq!(
            served.matches("preconnect").count(),
            2,
            "a media endpoint nobody named still warmed a socket"
        );
    }

    #[tokio::test]
    async fn an_endpoint_neither_discovered_nor_configured_warms_no_socket_at_all() {
        let state = spa_state_without_discovered_endpoints(None).await;

        let response = serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await;
        assert_eq!(response.status(), StatusCode::OK);
        let served = read_document(response).await;

        assert_eq!(
            served.matches("preconnect").count(),
            0,
            "an endpoint nobody named still reached the served document"
        );
        assert!(!served.contains("{{STATIC_CDN_ENDPOINT}}"));
        assert!(!served.contains("{{MEDIA_ENDPOINT}}"));
        assert!(
            served.contains(r#"href="/web/favicon-32x32.png""#),
            "an unnamed static CDN left the favicon pointing somewhere other than our own origin"
        );
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
