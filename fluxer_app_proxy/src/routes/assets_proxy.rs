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

pub async fn proxy_assets(
    State(state): State<AppState>,
    Path(path): Path<String>,
    request: axum::extract::Request,
) -> Response {
    let Some(cdn_endpoint) = &state.config.static_cdn_endpoint else {
        return serve_local_asset(
            &state.local_files,
            &format!("assets/{path}"),
            request.headers(),
            state.csp.asset_header(),
        )
        .await;
    };

    if path
        .split('/')
        .any(|segment| matches!(segment, "" | "." | ".."))
    {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::AppProxyConfig;
    use crate::state::{AppProxyBudgets, SpaIndexSource, build_http_client};
    use crate::static_asset_policy::{
        CORS_ALLOW_ANY_VALUE, LONG_LIVED_ASSET_CACHE_CONTROL, REVALIDATED_ASSET_CACHE_CONTROL,
        UPSTREAM_FAILURE_CACHE_CONTROL, is_hashed_asset,
    };
    use axum::Router;
    use axum::http::Request as HttpRequest;
    use axum::http::header::{self, HeaderName};
    use std::sync::Arc;
    use std::sync::atomic::{AtomicU64, Ordering};
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
        config.static_dir = ".".to_owned();
        config.static_cdn_endpoint = Some(
            crate::config::HttpEndpoint::parse("TEST_STATIC_CDN_ENDPOINT", cdn_endpoint).unwrap(),
        );
        state_from_config(config)
    }

    fn locally_backed_state(static_dir: &str) -> AppState {
        let mut config = AppProxyConfig::from_env();
        config.static_cdn_endpoint = None;
        config.static_dir = static_dir.to_owned();
        state_from_config(config)
    }

    fn state_from_config(config: AppProxyConfig) -> AppState {
        let csp = Arc::new(
            crate::csp::CompiledCspPolicy::from_config(&config)
                .expect("the test configuration must compile to a valid CSP"),
        );
        let budgets = AppProxyBudgets::default();
        let local_files =
            LocalFileStore::load_blocking(std::path::Path::new(&config.static_dir), &budgets)
                .expect("the test static directory must exist");
        AppState {
            config: Arc::new(config),
            csp,
            http_client: build_http_client().unwrap(),
            spa_index_source: SpaIndexSource::bundled(""),
            local_asset_prefixes: None,
            budgets,
            local_files,
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
    async fn a_not_found_with_a_long_upstream_lifetime_is_rewritten_to_no_store() {
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
    async fn a_bad_gateway_with_a_long_upstream_lifetime_is_rewritten_to_no_store() {
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
    async fn a_server_error_with_a_long_upstream_lifetime_is_rewritten_to_no_store() {
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

    fn apply_test_asset_policy(headers: &mut HeaderMap, path: &str, status: StatusCode) {
        apply_asset_response_policy(
            headers,
            path,
            status,
            HeaderValue::from_static("default-src 'none'"),
        );
    }

    #[test]
    fn upstream_cdn_lifetimes_never_reach_the_client() {
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

        let upstream = long_lived();
        let mut ok = HeaderMap::new();
        copy_asset_response_headers(&upstream, &mut ok);
        apply_test_asset_policy(&mut ok, "2d715e4730758083.worker.js", StatusCode::OK);
        assert!(!ok.contains_key("cdn-cache-control"));
        assert!(!ok.contains_key(header::EXPIRES));

        let mut failed = HeaderMap::new();
        copy_asset_response_headers(&upstream, &mut failed);
        apply_test_asset_policy(
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

        fn files(&self) -> LocalFileStore {
            self.files_with(&budgets())
        }

        fn files_with(&self, budgets: &AppProxyBudgets) -> LocalFileStore {
            LocalFileStore::load_blocking(&self.root, budgets).unwrap()
        }
    }

    impl Drop for LocalAssetDir {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.root);
        }
    }

    fn budgets() -> AppProxyBudgets {
        AppProxyBudgets::default()
    }

    fn test_asset_csp() -> HeaderValue {
        let mut config = AppProxyConfig::from_env();
        config.static_cdn_endpoint = None;
        crate::csp::CompiledCspPolicy::from_config(&config)
            .expect("the test configuration must compile to a valid CSP")
            .asset_header()
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
            &fixture.files(),
            "assets/0018072843a46dc4.woff2",
            &HeaderMap::new(),
            test_asset_csp(),
        )
        .await;
        assert_eq!(cors_origin_of(&first), Some(CORS_ALLOW_ANY_VALUE));
        let entity_tag = entity_tag_of(&first).expect("first response has a validator");

        let mut conditional = HeaderMap::new();
        conditional.insert(
            header::IF_NONE_MATCH,
            HeaderValue::from_str(&entity_tag).unwrap(),
        );
        let second = serve_local_asset(
            &fixture.files(),
            "assets/0018072843a46dc4.woff2",
            &conditional,
            test_asset_csp(),
        )
        .await;

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
            &fixture.files(),
            "assets/356aaade04a117b1.js",
            &HeaderMap::new(),
            test_asset_csp(),
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
        let response = serve_local_asset(
            &fixture.files(),
            "assets/fluxer-setup.exe",
            &resumed,
            test_asset_csp(),
        )
        .await;

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
            &fixture.files(),
            "assets/f00dcafe12345678.css",
            &HeaderMap::new(),
            test_asset_csp(),
        )
        .await;
        let entity_tag = entity_tag_of(&first).expect("first response has a validator");

        let mut conditional = HeaderMap::new();
        conditional.insert(
            header::IF_NONE_MATCH,
            HeaderValue::from_str(&entity_tag).unwrap(),
        );
        let second = serve_local_asset(
            &fixture.files(),
            "assets/f00dcafe12345678.css",
            &conditional,
            test_asset_csp(),
        )
        .await;

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
        let response = serve_local_asset(
            &fixture.files(),
            "assets/voice_engine_bg.wasm",
            &conditional,
            test_asset_csp(),
        )
        .await;

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
            &fixture.files(),
            "assets/2d715e4730758083.worker.js",
            &HeaderMap::new(),
            test_asset_csp(),
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
            &fixture.files(),
            "assets/356aaade04a117b1.js",
            &accept_encoding("gzip, deflate, br, zstd"),
            test_asset_csp(),
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
            &fixture.files(),
            "assets/469e0b8f10c496a1.css",
            &accept_encoding("gzip, deflate, br"),
            test_asset_csp(),
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
            &fixture.files(),
            "assets/488b87159423ca35.js",
            &accept_encoding("gzip, deflate"),
            test_asset_csp(),
        )
        .await;
        assert_eq!(content_encoding_of(&gzip_only), Some("gzip"));
        assert_eq!(body_bytes(gzip_only).await, b"gzip-bytes");

        let identity = serve_local_asset(
            &fixture.files(),
            "assets/488b87159423ca35.js",
            &HeaderMap::new(),
            test_asset_csp(),
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
            &fixture.files(),
            "assets/2d715e4730758083.worker.js",
            &accept_encoding("br;q=0, gzip"),
            test_asset_csp(),
        )
        .await;

        assert_eq!(content_encoding_of(&response), None);
        assert_eq!(body_bytes(response).await, b"self.onmessage=0");
    }

    #[tokio::test]
    async fn a_precompressed_variant_has_its_own_validator() {
        let fixture = LocalAssetDir::with_asset("f00dcafe12345678.css", b"body{}")
            .and_sibling("f00dcafe12345678.css.br", b"brotli-bytes-are-longer");

        let brotli = serve_local_asset(
            &fixture.files(),
            "assets/f00dcafe12345678.css",
            &accept_encoding("br"),
            test_asset_csp(),
        )
        .await;
        let brotli_tag = entity_tag_of(&brotli).expect("the brotli variant has a validator");

        let identity = serve_local_asset(
            &fixture.files(),
            "assets/f00dcafe12345678.css",
            &HeaderMap::new(),
            test_asset_csp(),
        )
        .await;
        let identity_tag = entity_tag_of(&identity).expect("the raw file has a validator");

        assert_ne!(
            brotli_tag, identity_tag,
            "two encodings sharing one validator let a cache hand brotli to a client that asked for identity"
        );

        let mut conditional = accept_encoding("br");
        conditional.insert(
            header::IF_NONE_MATCH,
            HeaderValue::from_str(&brotli_tag).unwrap(),
        );
        let revalidated = serve_local_asset(
            &fixture.files(),
            "assets/f00dcafe12345678.css",
            &conditional,
            test_asset_csp(),
        )
        .await;
        assert_eq!(revalidated.status(), StatusCode::NOT_MODIFIED);
        assert!(varies_on_accept_encoding(&revalidated));
    }

    #[tokio::test]
    async fn a_range_over_a_precompressed_sibling_describes_the_encoded_bytes() {
        let fixture = LocalAssetDir::with_asset("356aaade04a117b1.js", b"console.log(1)")
            .and_sibling("356aaade04a117b1.js.br", b"0123456789");

        let mut ranged = accept_encoding("br");
        ranged.insert(header::RANGE, HeaderValue::from_static("bytes=4-6"));
        let response = serve_local_asset(
            &fixture.files(),
            "assets/356aaade04a117b1.js",
            &ranged,
            test_asset_csp(),
        )
        .await;

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

    async fn spawn_encoded_upstream(
        content_encoding: &'static str,
        body: &'static str,
    ) -> (String, Arc<std::sync::Mutex<Option<String>>>) {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let observed_accept_encoding: Arc<std::sync::Mutex<Option<String>>> =
            Arc::new(std::sync::Mutex::new(None));
        let recorder = Arc::clone(&observed_accept_encoding);
        let router = Router::new().fallback(move |request: HttpRequest<Body>| {
            let recorder = Arc::clone(&recorder);
            async move {
                let received = request
                    .headers()
                    .get(header::ACCEPT_ENCODING)
                    .and_then(|value| value.to_str().ok())
                    .map(str::to_owned);
                *recorder.lock().unwrap() = received;
                let mut response = Response::new(Body::from(body));
                response.headers_mut().insert(
                    header::CONTENT_ENCODING,
                    HeaderValue::from_static(content_encoding),
                );
                response
            }
        });
        tokio::spawn(async move {
            axum::serve(listener, router).await.unwrap();
        });
        (format!("http://{addr}"), observed_accept_encoding)
    }

    #[tokio::test]
    async fn a_cdn_backed_asset_streams_the_upstream_encoding_untouched() {
        let (endpoint, observed_accept_encoding) =
            spawn_encoded_upstream("br", "already-brotli").await;
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
            observed_accept_encoding.lock().unwrap().as_deref(),
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
        let (endpoint, _observed_accept_encoding) =
            spawn_encoded_upstream("gzip", "0123456789").await;
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
        let (endpoint, _observed_accept_encoding) = spawn_encoded_upstream(
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
            "re-encoding upstream bytes that already have an encoding breaks every browser"
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

        apply_test_asset_policy(&mut headers, "356aaade04a117b1.js", StatusCode::OK);

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

        apply_test_asset_policy(&mut headers, "voice_engine_bg.wasm", StatusCode::OK);

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

        apply_test_asset_policy(&mut headers, "0018072843a46dc4.woff2", StatusCode::OK);

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

        apply_test_asset_policy(&mut headers, "0018072843a46dc4.woff2", StatusCode::OK);

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

        apply_test_asset_policy(&mut headers, "font-download", StatusCode::OK);

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

        apply_test_asset_policy(&mut headers, "356aaade04a117b1.js", StatusCode::OK);

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

        apply_test_asset_policy(&mut headers, "artifact.unknown-extension", StatusCode::OK);

        assert_eq!(
            headers
                .get(header::CONTENT_TYPE)
                .and_then(|value| value.to_str().ok()),
            Some("application/octet-stream")
        );
    }

    #[tokio::test]
    async fn a_local_asset_is_refused_once_the_read_slots_are_gone() {
        let fixture = LocalAssetDir::with_asset("356aaade04a117b1.js", b"console.log(1)");
        let budgets = AppProxyBudgets::default();
        let files = fixture.files_with(&budgets);
        let held = budgets
            .local_read_slots
            .clone()
            .try_acquire_many_owned(
                u32::try_from(crate::state::LOCAL_FILE_READS_IN_FLIGHT_MAX).unwrap(),
            )
            .expect("a fresh budget holds every local read slot");

        let refused = serve_local_asset(
            &files,
            "assets/356aaade04a117b1.js",
            &HeaderMap::new(),
            test_asset_csp(),
        )
        .await;
        assert_eq!(refused.status(), StatusCode::SERVICE_UNAVAILABLE);
        assert_eq!(
            refused
                .headers()
                .get(header::CACHE_CONTROL)
                .and_then(|value| value.to_str().ok()),
            Some("no-store"),
            "a cached refusal would pin the outage for every later reader"
        );

        drop(held);
        let served = serve_local_asset(
            &files,
            "assets/356aaade04a117b1.js",
            &HeaderMap::new(),
            test_asset_csp(),
        )
        .await;
        assert_eq!(served.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn an_open_local_asset_response_never_holds_a_read_slot() {
        let fixture = LocalAssetDir::with_asset("356aaade04a117b1.js", b"console.log(1)");
        let budgets = AppProxyBudgets::default();
        let files = fixture.files_with(&budgets);

        let mut open = Vec::with_capacity(crate::state::LOCAL_FILE_READS_IN_FLIGHT_MAX + 1);
        for _ in 0..=crate::state::LOCAL_FILE_READS_IN_FLIGHT_MAX {
            open.push(
                serve_local_asset(
                    &files,
                    "assets/356aaade04a117b1.js",
                    &HeaderMap::new(),
                    test_asset_csp(),
                )
                .await,
            );
        }

        let refused = open
            .iter()
            .filter(|response| response.status() != StatusCode::OK)
            .count();
        assert_eq!(
            refused, 0,
            "{refused} readers were turned away while earlier responses were still open"
        );

        let body = axum::body::to_bytes(open.pop().unwrap().into_body(), usize::MAX)
            .await
            .unwrap();
        assert_eq!(
            body.as_ref(),
            b"console.log(1)",
            "a response served past the read slot count had the wrong bytes"
        );
    }
}
