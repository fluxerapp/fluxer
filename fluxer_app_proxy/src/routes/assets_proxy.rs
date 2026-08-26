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

use super::spa_static::{CORS_ALLOW_ANY_VALUE, asset_cache_control, guess_mime, is_font_mime};

const ASSET_REQUEST_TIMEOUT: Duration = Duration::from_secs(15);
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
        if name_str == "content-encoding" || name_str == "content-length" {
            continue;
        }
        response_headers.insert(name.clone(), value.clone());
    }
    set_known_asset_content_type(&mut response_headers, &path);
    set_font_cors(&mut response_headers);
    set_proxied_cache_control(&mut response_headers, &path, status);

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

    let entity_tag = tokio::fs::metadata(&resolved)
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
    set_local_asset_headers(response.headers_mut(), relative_path, entity_tag.as_deref());
    response
}

fn set_local_asset_headers(headers: &mut HeaderMap, relative_path: &str, entity_tag: Option<&str>) {
    headers.insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static(asset_cache_control(relative_path)),
    );
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
    use axum::Router;
    use axum::http::Request as HttpRequest;
    use axum::http::header::HeaderName;
    use fluxer_common::config::GeoipSourceConfig;
    use fluxer_common::geoip::{GeoipConfig, GeoipResolver};
    use std::sync::Arc;

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
            let unique = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let root = std::env::temp_dir().join(format!("fluxer-local-asset-{unique}-{name}"));
            std::fs::create_dir_all(root.join("assets")).unwrap();
            std::fs::write(root.join("assets").join(name), bytes).unwrap();
            Self { root }
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
