// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::csp::{RuntimeCspSources, build_asset_csp};
use crate::state::AppState;
use axum::{
    body::Body,
    extract::{Path, State},
    http::{HeaderMap, HeaderValue, StatusCode, header},
    response::{IntoResponse, Response},
};
use std::path::{Path as FsPath, PathBuf};
use std::time::Duration;

use super::file_stream::stream_file;
use super::spa_static::{CORS_ALLOW_ANY_VALUE, asset_cache_control, guess_mime, is_font_mime};

const ASSET_REQUEST_TIMEOUT: Duration = Duration::from_secs(15);
const PRECOMPRESSED_VARIANTS: &[(&str, &str)] = &[("br", "br"), ("gzip", "gz")];
const MAX_ASSET_SIZE_BYTES: u64 = 100 * 1024 * 1024;
const UPSTREAM_FAILURE_CACHE_CONTROL: &str = "no-store";
const UPSTREAM_FAILURE_STRIPPED_HEADERS: &[&str] = &[
    "cdn-cache-control",
    "cloudflare-cdn-cache-control",
    "surrogate-control",
    "expires",
    "age",
];

const BLOCKED_REQUEST_HEADERS: &[&str] = &[
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
        return serve_local_asset(
            &state.config.static_dir,
            &format!("assets/{path}"),
            request.headers(),
        )
        .await;
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

    let status = StatusCode::from_u16(upstream_response.status().as_u16())
        .unwrap_or(StatusCode::BAD_GATEWAY);
    let mut response_headers = axum::http::HeaderMap::new();

    for (name, value) in upstream_response.headers() {
        let name_str = name.as_str();
        if BLOCKED_RESPONSE_HEADERS.contains(&name_str) {
            continue;
        }
        response_headers.insert(name.clone(), value.clone());
    }
    set_known_asset_content_type(&mut response_headers, &path);
    set_font_cors(&mut response_headers);
    set_proxied_cache_control(&mut response_headers, &path, status);
    set_vary_on_accept_encoding(&mut response_headers);

    let asset_csp = build_asset_csp(
        &state.config.csp,
        &RuntimeCspSources {
            static_cdn_endpoint: state.config.static_cdn_endpoint.clone(),
            media_endpoint: None,
            s3_public_endpoint: None,
            s3_uploads_bucket: None,
            branding_image_origins: Vec::new(),
        },
    );
    if let Ok(value) = HeaderValue::from_str(&asset_csp) {
        response_headers.insert(header::CONTENT_SECURITY_POLICY, value);
    }
    response_headers.remove("content-security-policy-report-only");

    let body = Body::from_stream(upstream_response.bytes_stream());
    let mut response = Response::new(body);
    *response.status_mut() = status;
    *response.headers_mut() = response_headers;
    response
}

pub(super) async fn serve_local_asset(
    static_dir: &str,
    relative_path: &str,
    request_headers: &HeaderMap,
) -> Response {
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

    let (served_path, content_encoding) =
        select_precompressed_variant(&resolved, &base, request_headers).await;

    let entity_tag = tokio::fs::metadata(&served_path)
        .await
        .ok()
        .and_then(|metadata| local_asset_entity_tag(&metadata));

    if let Some(entity_tag) = entity_tag.as_deref()
        && if_none_match_matches(request_headers, entity_tag)
    {
        let mut response = StatusCode::NOT_MODIFIED.into_response();
        set_local_asset_headers(response.headers_mut(), relative_path, Some(entity_tag));
        return response;
    }

    let mut response = match stream_file(&served_path, request_headers, entity_tag.as_deref()).await
    {
        Ok(response) => response,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return StatusCode::NOT_FOUND.into_response();
        }
        Err(err) => {
            tracing::error!(path = relative_path, %err, "failed to read local asset");
            return (StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error").into_response();
        }
    };

    let mime_type = guess_mime(relative_path);
    if let Ok(value) = HeaderValue::from_str(mime_type) {
        response.headers_mut().insert(header::CONTENT_TYPE, value);
    }
    if let Some(content_encoding) = content_encoding {
        response.headers_mut().insert(
            header::CONTENT_ENCODING,
            HeaderValue::from_static(content_encoding),
        );
    }
    set_local_asset_headers(response.headers_mut(), relative_path, entity_tag.as_deref());
    response
}

async fn select_precompressed_variant(
    resolved: &FsPath,
    base: &FsPath,
    request_headers: &HeaderMap,
) -> (PathBuf, Option<&'static str>) {
    for &(encoding, extension) in PRECOMPRESSED_VARIANTS {
        if !accepts_encoding(request_headers, encoding) {
            continue;
        }
        let Some(candidate) = usable_sibling(resolved, base, extension).await else {
            continue;
        };
        return (candidate, Some(encoding));
    }
    (resolved.to_path_buf(), None)
}

async fn usable_sibling(resolved: &FsPath, base: &FsPath, extension: &str) -> Option<PathBuf> {
    let mut name = resolved.as_os_str().to_owned();
    name.push(".");
    name.push(extension);

    let candidate = tokio::fs::canonicalize(PathBuf::from(name)).await.ok()?;
    if !candidate.starts_with(base) {
        return None;
    }
    tokio::fs::metadata(&candidate)
        .await
        .ok()
        .filter(std::fs::Metadata::is_file)
        .map(|_| candidate)
}

fn accepts_encoding(headers: &HeaderMap, encoding: &str) -> bool {
    let Some(header_value) = headers
        .get(header::ACCEPT_ENCODING)
        .and_then(|value| value.to_str().ok())
    else {
        return false;
    };
    header_value.split(',').any(|candidate| {
        let mut parts = candidate.split(';').map(str::trim);
        let Some(name) = parts.next() else {
            return false;
        };
        name.eq_ignore_ascii_case(encoding) && !parts.any(is_zero_quality)
    })
}

fn is_zero_quality(parameter: &str) -> bool {
    let Some((key, value)) = parameter.split_once('=') else {
        return false;
    };
    key.trim().eq_ignore_ascii_case("q")
        && value
            .trim()
            .parse::<f32>()
            .is_ok_and(|quality| quality <= 0.0)
}

fn set_local_asset_headers(headers: &mut HeaderMap, relative_path: &str, entity_tag: Option<&str>) {
    headers.insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static(asset_cache_control(relative_path)),
    );
    set_vary_on_accept_encoding(headers);
    if is_font_mime(guess_mime(relative_path)) {
        headers.insert(
            header::ACCESS_CONTROL_ALLOW_ORIGIN,
            HeaderValue::from_static(CORS_ALLOW_ANY_VALUE),
        );
    }
    if let Some(entity_tag) = entity_tag
        && let Ok(value) = HeaderValue::from_str(entity_tag)
    {
        headers.insert(header::ETAG, value);
    }
}

fn local_asset_entity_tag(metadata: &std::fs::Metadata) -> Option<String> {
    let modified = metadata.modified().ok()?;
    let nanos = modified
        .duration_since(std::time::UNIX_EPOCH)
        .ok()?
        .as_nanos();
    Some(format!("\"{nanos:x}-{:x}\"", metadata.len()))
}

fn if_none_match_matches(headers: &HeaderMap, entity_tag: &str) -> bool {
    let Some(header_value) = headers
        .get(header::IF_NONE_MATCH)
        .and_then(|value| value.to_str().ok())
    else {
        return false;
    };
    header_value.split(',').any(|candidate| {
        let candidate = candidate.trim();
        candidate == "*" || candidate.trim_start_matches("W/") == entity_tag
    })
}

fn set_proxied_cache_control(headers: &mut HeaderMap, path: &str, status: StatusCode) {
    if status.is_success() || status == StatusCode::NOT_MODIFIED {
        headers.insert(
            header::CACHE_CONTROL,
            HeaderValue::from_static(asset_cache_control(path)),
        );
        return;
    }
    headers.insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static(UPSTREAM_FAILURE_CACHE_CONTROL),
    );
    for name in UPSTREAM_FAILURE_STRIPPED_HEADERS {
        headers.remove(*name);
    }
}

fn set_vary_on_accept_encoding(headers: &mut HeaderMap) {
    let already_varies = headers.get_all(header::VARY).iter().any(|value| {
        value.to_str().is_ok_and(|value| {
            value.split(',').any(|field| {
                let field = field.trim();
                field == "*" || field.eq_ignore_ascii_case("accept-encoding")
            })
        })
    });
    if !already_varies {
        headers.append(header::VARY, HeaderValue::from_static("accept-encoding"));
    }
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
    use super::super::spa_static::{
        LONG_LIVED_ASSET_CACHE_CONTROL, REVALIDATED_ASSET_CACHE_CONTROL, is_hashed_asset,
    };
    use super::*;
    use crate::config::AppProxyConfig;
    use crate::discovery_cache::DiscoveryCache;
    use crate::state::build_http_client;
    use axum::Router;
    use axum::http::Request as HttpRequest;
    use axum::http::header::HeaderName;
    use fluxer_common::config::GeoipSourceConfig;
    use fluxer_common::geoip::{GeoipConfig, GeoipResolver};
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::{Arc, OnceLock};
    use tower::ServiceExt;

    async fn spawn_upstream(status: StatusCode, cache_control: &'static str) -> String {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let router = Router::new().fallback(move || async move {
            let mut response = Response::new(Body::from("upstream-bytes"));
            *response.status_mut() = status;
            response.headers_mut().insert(
                header::CACHE_CONTROL,
                HeaderValue::from_static(cache_control),
            );
            response
        });
        tokio::spawn(async move {
            axum::serve(listener, router).await.unwrap();
        });
        format!("http://{addr}")
    }

    fn upstream_backed_state(cdn_endpoint: &str) -> AppState {
        let mut config = AppProxyConfig::from_env();
        config.static_cdn_endpoint = Some(cdn_endpoint.to_owned());
        state_from_config(config)
    }

    fn locally_backed_state(static_dir: &str) -> AppState {
        let mut config = AppProxyConfig::from_env();
        config.static_cdn_endpoint = None;
        config.static_dir = static_dir.to_owned();
        state_from_config(config)
    }

    fn state_from_config(config: AppProxyConfig) -> AppState {
        AppState {
            config: Arc::new(config),
            http_client: build_http_client().unwrap(),
            discovery_cache: Arc::new(DiscoveryCache::new()),
            geoip: Arc::new(GeoipResolver::from_config(&GeoipConfig {
                geoip_source: GeoipSourceConfig::Filesystem {
                    maxmind_db_path: None,
                },
                geoip_s3_config: None,
                trust_client_ip_header: false,
                client_ip_header_name: "x-forwarded-for".to_owned(),
            })),
            invite_meta: Arc::new(OnceLock::new()),
            index_html: None,
        }
    }

    async fn proxied_asset(
        status: StatusCode,
        upstream_cache_control: &'static str,
        asset_path: &str,
    ) -> Response {
        let endpoint = spawn_upstream(status, upstream_cache_control).await;
        let state = upstream_backed_state(&endpoint);
        let request = HttpRequest::builder()
            .uri(format!("/assets/{asset_path}"))
            .body(Body::empty())
            .unwrap();
        proxy_assets(State(state), Path(asset_path.to_owned()), request).await
    }

    fn cache_control_of(response: &Response) -> Option<&str> {
        response
            .headers()
            .get(header::CACHE_CONTROL)
            .and_then(|value| value.to_str().ok())
    }

    #[tokio::test]
    async fn a_proxied_asset_overrides_a_shorter_upstream_lifetime() {
        let response = proxied_asset(
            StatusCode::OK,
            "public, max-age=3600, must-revalidate",
            "2d715e4730758083.worker.js",
        )
        .await;

        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            cache_control_of(&response),
            Some(LONG_LIVED_ASSET_CACHE_CONTROL)
        );
    }

    #[tokio::test]
    async fn an_asset_without_a_content_hash_is_never_promised_to_never_change() {
        let response = proxied_asset(
            StatusCode::OK,
            "public, max-age=31536000, immutable",
            "voice_engine_bg.wasm",
        )
        .await;

        assert_eq!(
            cache_control_of(&response),
            Some(REVALIDATED_ASSET_CACHE_CONTROL),
            "a stable filename can be redeployed over, so it must stay revalidatable"
        );
    }

    #[tokio::test]
    async fn revalidated_hashed_asset_keeps_our_lifetime_on_not_modified() {
        let response = proxied_asset(
            StatusCode::NOT_MODIFIED,
            "public, max-age=60",
            "2d715e4730758083.worker.js",
        )
        .await;

        assert_eq!(response.status(), StatusCode::NOT_MODIFIED);
        assert_eq!(
            cache_control_of(&response),
            Some(LONG_LIVED_ASSET_CACHE_CONTROL)
        );
    }

    #[tokio::test]
    async fn an_asset_without_a_content_hash_keeps_our_policy_on_not_modified() {
        let response = proxied_asset(
            StatusCode::NOT_MODIFIED,
            "public, max-age=31536000, immutable",
            "voice_engine_bg.wasm",
        )
        .await;

        assert_eq!(
            cache_control_of(&response),
            Some(REVALIDATED_ASSET_CACHE_CONTROL)
        );
    }

    #[tokio::test]
    async fn upstream_failure_is_never_stamped_with_an_asset_lifetime() {
        let response = proxied_asset(
            StatusCode::NOT_FOUND,
            "no-store",
            "2d715e4730758083.worker.js",
        )
        .await;

        assert_eq!(response.status(), StatusCode::NOT_FOUND);
        assert_eq!(cache_control_of(&response), Some("no-store"));
    }

    #[tokio::test]
    async fn a_not_found_carrying_a_long_upstream_lifetime_is_rewritten_to_no_store() {
        let response = proxied_asset(
            StatusCode::NOT_FOUND,
            "public, max-age=31536000, immutable",
            "2d715e4730758083.worker.js",
        )
        .await;

        assert_eq!(response.status(), StatusCode::NOT_FOUND);
        assert_eq!(
            cache_control_of(&response),
            Some(UPSTREAM_FAILURE_CACHE_CONTROL),
            "a cdn or bucket error page with its own year would pin the miss for a year"
        );
    }

    #[tokio::test]
    async fn a_bad_gateway_carrying_a_long_upstream_lifetime_is_rewritten_to_no_store() {
        let response = proxied_asset(
            StatusCode::BAD_GATEWAY,
            "public, max-age=604800",
            "2d715e4730758083.worker.js",
        )
        .await;

        assert_eq!(response.status(), StatusCode::BAD_GATEWAY);
        assert_eq!(
            cache_control_of(&response),
            Some(UPSTREAM_FAILURE_CACHE_CONTROL)
        );
    }

    #[tokio::test]
    async fn a_server_error_carrying_a_long_upstream_lifetime_is_rewritten_to_no_store() {
        let response = proxied_asset(
            StatusCode::INTERNAL_SERVER_ERROR,
            "public, max-age=86400, immutable",
            "voice_engine_bg.wasm",
        )
        .await;

        assert_eq!(response.status(), StatusCode::INTERNAL_SERVER_ERROR);
        assert_eq!(
            cache_control_of(&response),
            Some(UPSTREAM_FAILURE_CACHE_CONTROL)
        );
    }

    #[test]
    fn a_failure_drops_the_cdn_lifetimes_a_success_keeps() {
        let long_lived = || {
            let mut headers = HeaderMap::new();
            headers.insert(
                header::CACHE_CONTROL,
                HeaderValue::from_static("public, max-age=31536000, immutable"),
            );
            headers.insert(
                HeaderName::from_static("cdn-cache-control"),
                HeaderValue::from_static("public, max-age=31536000"),
            );
            headers.insert(
                header::EXPIRES,
                HeaderValue::from_static("Thu, 31 Dec 2099 23:59:59 GMT"),
            );
            headers
        };

        let mut ok = long_lived();
        set_proxied_cache_control(&mut ok, "2d715e4730758083.worker.js", StatusCode::OK);
        assert!(
            ok.contains_key("cdn-cache-control"),
            "positive control: a real asset still reaches the cdn with its own long lifetime"
        );
        assert!(ok.contains_key(header::EXPIRES));

        let mut failed = long_lived();
        set_proxied_cache_control(
            &mut failed,
            "2d715e4730758083.worker.js",
            StatusCode::NOT_FOUND,
        );
        assert_eq!(
            failed
                .get(header::CACHE_CONTROL)
                .and_then(|value| value.to_str().ok()),
            Some(UPSTREAM_FAILURE_CACHE_CONTROL)
        );
        assert!(
            !failed.contains_key("cdn-cache-control"),
            "a cdn honours cdn-cache-control over cache-control, so the error would still be pinned"
        );
        assert!(!failed.contains_key(header::EXPIRES));
    }

    struct LocalAssetDir {
        root: std::path::PathBuf,
    }

    impl LocalAssetDir {
        fn with_asset(name: &str, bytes: &[u8]) -> Self {
            static NEXT_FIXTURE: AtomicU64 = AtomicU64::new(0);
            let unique = NEXT_FIXTURE.fetch_add(1, Ordering::Relaxed);
            let pid = std::process::id();
            let root =
                std::env::temp_dir().join(format!("fluxer-local-asset-{pid}-{unique}-{name}"));
            std::fs::create_dir_all(root.join("assets")).unwrap();
            std::fs::write(root.join("assets").join(name), bytes).unwrap();
            Self { root }
        }

        fn and_sibling(self, name: &str, bytes: &[u8]) -> Self {
            std::fs::write(self.root.join("assets").join(name), bytes).unwrap();
            self
        }

        fn dir(&self) -> &str {
            self.root.to_str().unwrap()
        }
    }

    impl Drop for LocalAssetDir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.root);
        }
    }

    fn entity_tag_of(response: &Response) -> Option<String> {
        response
            .headers()
            .get(header::ETAG)
            .and_then(|value| value.to_str().ok())
            .map(ToOwned::to_owned)
    }

    fn cors_origin_of(response: &Response) -> Option<&str> {
        response
            .headers()
            .get(header::ACCESS_CONTROL_ALLOW_ORIGIN)
            .and_then(|value| value.to_str().ok())
    }

    #[tokio::test]
    async fn local_font_revalidation_keeps_cross_origin_access() {
        let fixture = LocalAssetDir::with_asset("0018072843a46dc4.woff2", b"wOF2stub");

        let first = serve_local_asset(
            fixture.dir(),
            "assets/0018072843a46dc4.woff2",
            &HeaderMap::new(),
        )
        .await;
        assert_eq!(cors_origin_of(&first), Some(CORS_ALLOW_ANY_VALUE));
        let entity_tag = entity_tag_of(&first).expect("first response carries a validator");

        let mut conditional = HeaderMap::new();
        conditional.insert(
            header::IF_NONE_MATCH,
            HeaderValue::from_str(&entity_tag).unwrap(),
        );
        let second =
            serve_local_asset(fixture.dir(), "assets/0018072843a46dc4.woff2", &conditional).await;

        assert_eq!(second.status(), StatusCode::NOT_MODIFIED);
        assert_eq!(
            cors_origin_of(&second),
            Some(CORS_ALLOW_ANY_VALUE),
            "a 304 without the CORS header fails the cross-origin font fetch the 200 allowed"
        );
    }

    #[tokio::test]
    async fn local_hashed_asset_is_served_with_an_entity_tag() {
        let fixture = LocalAssetDir::with_asset("356aaade04a117b1.js", b"console.log(1)");

        let response = serve_local_asset(
            fixture.dir(),
            "assets/356aaade04a117b1.js",
            &HeaderMap::new(),
        )
        .await;

        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            cache_control_of(&response),
            Some(LONG_LIVED_ASSET_CACHE_CONTROL)
        );
        assert!(
            entity_tag_of(&response).is_some(),
            "a year-long asset with no validator forces a full re-download on any revalidation"
        );
    }

    #[tokio::test]
    async fn a_local_asset_download_can_be_resumed() {
        let fixture = LocalAssetDir::with_asset("fluxer-setup.exe", b"installer-payload");

        let mut resumed = HeaderMap::new();
        resumed.insert(header::RANGE, HeaderValue::from_static("bytes=10-"));
        let response = serve_local_asset(fixture.dir(), "assets/fluxer-setup.exe", &resumed).await;

        assert_eq!(
            response.status(),
            StatusCode::PARTIAL_CONTENT,
            "a resumed installer download that answers 200 re-sends every byte already fetched"
        );
        assert_eq!(
            response
                .headers()
                .get(header::CONTENT_RANGE)
                .and_then(|value| value.to_str().ok()),
            Some("bytes 10-16/17")
        );
        assert_eq!(
            axum::body::to_bytes(response.into_body(), usize::MAX)
                .await
                .unwrap()
                .as_ref(),
            b"payload"
        );
    }

    #[tokio::test]
    async fn local_asset_revalidation_returns_not_modified() {
        let fixture = LocalAssetDir::with_asset("f00dcafe12345678.css", b"body{}");

        let first = serve_local_asset(
            fixture.dir(),
            "assets/f00dcafe12345678.css",
            &HeaderMap::new(),
        )
        .await;
        let entity_tag = entity_tag_of(&first).expect("first response carries a validator");

        let mut conditional = HeaderMap::new();
        conditional.insert(
            header::IF_NONE_MATCH,
            HeaderValue::from_str(&entity_tag).unwrap(),
        );
        let second =
            serve_local_asset(fixture.dir(), "assets/f00dcafe12345678.css", &conditional).await;

        assert_eq!(second.status(), StatusCode::NOT_MODIFIED);
        assert_eq!(entity_tag_of(&second).as_deref(), Some(entity_tag.as_str()));
        assert_eq!(
            cache_control_of(&second),
            Some(LONG_LIVED_ASSET_CACHE_CONTROL)
        );
    }

    #[tokio::test]
    async fn local_asset_with_a_stale_entity_tag_is_resent_in_full() {
        let fixture = LocalAssetDir::with_asset("voice_engine_bg.wasm", b"\0asm");

        let mut conditional = HeaderMap::new();
        conditional.insert(
            header::IF_NONE_MATCH,
            HeaderValue::from_static("\"stale-from-a-previous-build\""),
        );
        let response =
            serve_local_asset(fixture.dir(), "assets/voice_engine_bg.wasm", &conditional).await;

        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            cache_control_of(&response),
            Some(REVALIDATED_ASSET_CACHE_CONTROL)
        );
        assert!(
            entity_tag_of(&response).is_some(),
            "a year-long asset with no validator forces a full re-download on any revalidation"
        );
    }

    #[tokio::test]
    async fn a_local_content_hashed_asset_is_promised_to_never_change() {
        let fixture = LocalAssetDir::with_asset("2d715e4730758083.worker.js", b"self.onmessage=0");

        let response = serve_local_asset(
            fixture.dir(),
            "assets/2d715e4730758083.worker.js",
            &HeaderMap::new(),
        )
        .await;

        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            cache_control_of(&response),
            Some(LONG_LIVED_ASSET_CACHE_CONTROL)
        );
        assert!(is_hashed_asset("assets/2d715e4730758083.worker.js"));
    }

    fn accept_encoding(value: &'static str) -> HeaderMap {
        let mut headers = HeaderMap::new();
        headers.insert(header::ACCEPT_ENCODING, HeaderValue::from_static(value));
        headers
    }

    fn content_encoding_of(response: &Response) -> Option<&str> {
        response
            .headers()
            .get(header::CONTENT_ENCODING)
            .and_then(|value| value.to_str().ok())
    }

    fn varies_on_accept_encoding(response: &Response) -> bool {
        response
            .headers()
            .get_all(header::VARY)
            .iter()
            .any(|value| {
                value
                    .to_str()
                    .is_ok_and(|value| value.eq_ignore_ascii_case("accept-encoding"))
            })
    }

    async fn body_bytes(response: Response) -> Vec<u8> {
        axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap()
            .to_vec()
    }

    #[tokio::test]
    async fn a_local_asset_is_served_from_its_precompressed_brotli_sibling() {
        let fixture = LocalAssetDir::with_asset("356aaade04a117b1.js", b"console.log(1)")
            .and_sibling("356aaade04a117b1.js.br", b"brotli-bytes");

        let response = serve_local_asset(
            fixture.dir(),
            "assets/356aaade04a117b1.js",
            &accept_encoding("gzip, deflate, br, zstd"),
        )
        .await;

        assert_eq!(content_encoding_of(&response), Some("br"));
        assert!(varies_on_accept_encoding(&response));
        assert_eq!(
            response
                .headers()
                .get(header::CONTENT_TYPE)
                .and_then(|value| value.to_str().ok()),
            Some("application/javascript; charset=utf-8"),
            "the encoding must not leak into the media type the browser parses"
        );
        assert_eq!(
            body_bytes(response).await,
            b"brotli-bytes",
            "the sibling produced at build time must reach the wire unmodified"
        );
    }

    #[tokio::test]
    async fn a_local_asset_falls_back_to_the_raw_file_without_a_sibling() {
        let fixture = LocalAssetDir::with_asset("469e0b8f10c496a1.css", b"body{color:red}");

        let response = serve_local_asset(
            fixture.dir(),
            "assets/469e0b8f10c496a1.css",
            &accept_encoding("gzip, deflate, br"),
        )
        .await;

        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(content_encoding_of(&response), None);
        assert!(varies_on_accept_encoding(&response));
        assert_eq!(body_bytes(response).await, b"body{color:red}");
    }

    #[tokio::test]
    async fn a_local_asset_only_uses_an_encoding_the_client_accepted() {
        let fixture = LocalAssetDir::with_asset("488b87159423ca35.js", b"console.log(2)")
            .and_sibling("488b87159423ca35.js.br", b"brotli-bytes")
            .and_sibling("488b87159423ca35.js.gz", b"gzip-bytes");

        let gzip_only = serve_local_asset(
            fixture.dir(),
            "assets/488b87159423ca35.js",
            &accept_encoding("gzip, deflate"),
        )
        .await;
        assert_eq!(content_encoding_of(&gzip_only), Some("gzip"));
        assert_eq!(body_bytes(gzip_only).await, b"gzip-bytes");

        let identity = serve_local_asset(
            fixture.dir(),
            "assets/488b87159423ca35.js",
            &HeaderMap::new(),
        )
        .await;
        assert_eq!(
            content_encoding_of(&identity),
            None,
            "a client that advertised no encoding cannot decode the sibling"
        );
        assert_eq!(body_bytes(identity).await, b"console.log(2)");
    }

    #[tokio::test]
    async fn a_local_asset_refuses_a_sibling_the_client_scored_zero() {
        let fixture = LocalAssetDir::with_asset("2d715e4730758083.worker.js", b"self.onmessage=0")
            .and_sibling("2d715e4730758083.worker.js.br", b"brotli-bytes");

        let response = serve_local_asset(
            fixture.dir(),
            "assets/2d715e4730758083.worker.js",
            &accept_encoding("br;q=0, gzip"),
        )
        .await;

        assert_eq!(content_encoding_of(&response), None);
        assert_eq!(body_bytes(response).await, b"self.onmessage=0");
    }

    #[tokio::test]
    async fn a_precompressed_variant_carries_its_own_validator() {
        let fixture = LocalAssetDir::with_asset("f00dcafe12345678.css", b"body{}")
            .and_sibling("f00dcafe12345678.css.br", b"brotli-bytes-are-longer");

        let brotli = serve_local_asset(
            fixture.dir(),
            "assets/f00dcafe12345678.css",
            &accept_encoding("br"),
        )
        .await;
        let brotli_tag = entity_tag_of(&brotli).expect("the brotli variant carries a validator");

        let identity = serve_local_asset(
            fixture.dir(),
            "assets/f00dcafe12345678.css",
            &HeaderMap::new(),
        )
        .await;
        let identity_tag = entity_tag_of(&identity).expect("the raw file carries a validator");

        assert_ne!(
            brotli_tag, identity_tag,
            "two encodings sharing one validator let a cache hand brotli to a client that asked for identity"
        );

        let mut conditional = accept_encoding("br");
        conditional.insert(
            header::IF_NONE_MATCH,
            HeaderValue::from_str(&brotli_tag).unwrap(),
        );
        let revalidated =
            serve_local_asset(fixture.dir(), "assets/f00dcafe12345678.css", &conditional).await;
        assert_eq!(revalidated.status(), StatusCode::NOT_MODIFIED);
        assert!(varies_on_accept_encoding(&revalidated));
    }

    #[tokio::test]
    async fn a_range_over_a_precompressed_sibling_describes_the_encoded_bytes() {
        let fixture = LocalAssetDir::with_asset("356aaade04a117b1.js", b"console.log(1)")
            .and_sibling("356aaade04a117b1.js.br", b"0123456789");

        let mut ranged = accept_encoding("br");
        ranged.insert(header::RANGE, HeaderValue::from_static("bytes=4-6"));
        let response =
            serve_local_asset(fixture.dir(), "assets/356aaade04a117b1.js", &ranged).await;

        assert_eq!(response.status(), StatusCode::PARTIAL_CONTENT);
        assert_eq!(content_encoding_of(&response), Some("br"));
        assert_eq!(
            response
                .headers()
                .get(header::CONTENT_RANGE)
                .and_then(|value| value.to_str().ok()),
            Some("bytes 4-6/10"),
            "a range counted over the raw file cannot be reassembled from the encoded bytes we sent"
        );
        assert_eq!(
            response
                .headers()
                .get(header::CONTENT_LENGTH)
                .and_then(|value| value.to_str().ok()),
            Some("3")
        );
        assert_eq!(body_bytes(response).await, b"456");
    }

    async fn spawn_encoded_upstream(content_encoding: &'static str, body: &'static str) -> String {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let router = Router::new().fallback(move |request: HttpRequest<Body>| async move {
            let echoed = request
                .headers()
                .get(header::ACCEPT_ENCODING)
                .and_then(|value| value.to_str().ok())
                .unwrap_or("<absent>")
                .to_owned();
            let mut response = Response::new(Body::from(body));
            response.headers_mut().insert(
                header::CONTENT_ENCODING,
                HeaderValue::from_static(content_encoding),
            );
            response.headers_mut().insert(
                HeaderName::from_static("x-echoed-accept-encoding"),
                HeaderValue::from_str(&echoed).unwrap(),
            );
            response
        });
        tokio::spawn(async move {
            axum::serve(listener, router).await.unwrap();
        });
        format!("http://{addr}")
    }

    #[tokio::test]
    async fn a_cdn_backed_asset_streams_the_upstream_encoding_untouched() {
        let endpoint = spawn_encoded_upstream("br", "already-brotli").await;
        let state = upstream_backed_state(&endpoint);
        let request = HttpRequest::builder()
            .uri("/assets/356aaade04a117b1.js")
            .header(header::ACCEPT_ENCODING, "gzip, deflate, br")
            .body(Body::empty())
            .unwrap();

        let response = proxy_assets(
            State(state),
            Path("356aaade04a117b1.js".to_owned()),
            request,
        )
        .await;

        assert_eq!(
            response
                .headers()
                .get("x-echoed-accept-encoding")
                .and_then(|value| value.to_str().ok()),
            Some("gzip, deflate, br"),
            "blocking accept-encoding forces the origin to hand us bytes it already had compressed"
        );
        assert_eq!(
            content_encoding_of(&response),
            Some("br"),
            "dropping content-encoding turns compressed upstream bytes into an undecodable body"
        );
        assert!(varies_on_accept_encoding(&response));
        assert_eq!(body_bytes(response).await, b"already-brotli");
    }

    #[tokio::test]
    async fn a_cdn_backed_asset_keeps_the_upstream_content_length() {
        let endpoint = spawn_encoded_upstream("gzip", "0123456789").await;
        let state = upstream_backed_state(&endpoint);
        let request = HttpRequest::builder()
            .uri("/assets/voice_engine_bg.wasm")
            .header(header::ACCEPT_ENCODING, "gzip")
            .body(Body::empty())
            .unwrap();

        let response = proxy_assets(
            State(state),
            Path("voice_engine_bg.wasm".to_owned()),
            request,
        )
        .await;

        assert_eq!(
            response
                .headers()
                .get(header::CONTENT_LENGTH)
                .and_then(|value| value.to_str().ok()),
            Some("10"),
            "a client that cannot see the encoded length cannot show download progress"
        );
    }

    const COMPRESSIBLE_BODY: &[u8] =
        b"the default compression predicate ignores anything under thirty-two bytes";

    #[tokio::test]
    async fn an_asset_without_a_sibling_is_still_compressed_before_it_leaves() {
        let fixture = LocalAssetDir::with_asset("356aaade04a117b1.js", COMPRESSIBLE_BODY);
        let router = super::super::build_router(locally_backed_state(fixture.dir()));

        let asset = router
            .oneshot(
                HttpRequest::builder()
                    .uri("/assets/356aaade04a117b1.js")
                    .header(header::ACCEPT_ENCODING, "gzip")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(asset.status(), StatusCode::OK);
        assert_eq!(
            content_encoding_of(&asset),
            Some("gzip"),
            "an extension the build-time step does not cover must not fall off a bandwidth cliff"
        );
        assert_ne!(body_bytes(asset).await, COMPRESSIBLE_BODY);
    }

    #[tokio::test]
    async fn a_passed_through_cdn_encoding_is_never_recompressed_by_the_layer() {
        let endpoint = spawn_encoded_upstream(
            "br",
            "already brotli, and long enough to clear the thirty-two byte floor",
        )
        .await;
        let router = super::super::build_router(upstream_backed_state(&endpoint));

        let response = router
            .oneshot(
                HttpRequest::builder()
                    .uri("/assets/356aaade04a117b1.js")
                    .header(header::ACCEPT_ENCODING, "gzip, deflate, br")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(content_encoding_of(&response), Some("br"));
        assert_eq!(
            body_bytes(response).await,
            b"already brotli, and long enough to clear the thirty-two byte floor",
            "re-encoding upstream bytes that already carry an encoding breaks every browser"
        );
    }

    #[tokio::test]
    async fn a_precompressed_sibling_reaches_the_client_through_the_router() {
        let fixture = LocalAssetDir::with_asset("488b87159423ca35.js", COMPRESSIBLE_BODY)
            .and_sibling("488b87159423ca35.js.br", b"brotli-bytes");
        let router = super::super::build_router(locally_backed_state(fixture.dir()));

        let response = router
            .oneshot(
                HttpRequest::builder()
                    .uri("/assets/488b87159423ca35.js")
                    .header(header::ACCEPT_ENCODING, "gzip, deflate, br")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(content_encoding_of(&response), Some("br"));
        assert_eq!(
            body_bytes(response).await,
            b"brotli-bytes",
            "re-encoding the sibling would double-compress it and break every browser"
        );
    }

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
