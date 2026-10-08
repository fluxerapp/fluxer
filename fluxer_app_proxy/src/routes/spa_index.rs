// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::HttpEndpoint;
use crate::csp::{CspHeaderValues, inline_script_hashes};
use crate::state::{
    AppProxyBudgets, AppState, MAX_RENDERED_SPA_INDEX_BYTES, MAX_SPA_INDEX_BYTES, SpaIndexSource,
};
use crate::static_asset_policy::{CORS_ALLOW_ANY_VALUE, guess_mime, is_font_mime};
use axum::{
    body::{Body, Bytes},
    extract::{Request, State},
    http::{HeaderMap, HeaderName, HeaderValue, StatusCode, header},
    response::{IntoResponse, Response},
};
use std::path::Path;
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use super::assets_proxy::serve_local_asset;
use super::file_stream::stream_file;

const ACCEPT_CH_VALUE: &str = "DPR, Sec-CH-DPR, Sec-CH-Width, Save-Data, ECT, Downlink";
const CRITICAL_CH_VALUE: &str = "Sec-CH-DPR, Sec-CH-Width, Save-Data";
const DEV_NO_STORE_CACHE_CONTROL: &str = "no-store, no-cache, must-revalidate, max-age=0";
const SHARED_SHELL_CACHE_CONTROL: &str = "public, max-age=0, s-maxage=1";
const CSP_NONCE_PLACEHOLDER_ATTRIBUTE: &str = r#" nonce="{{CSP_NONCE_PLACEHOLDER}}""#;
const MEDIA_PRECONNECT_TAG: &str = r#"<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">"#;
const SAME_ORIGIN_API_META_TAG: &str = r#"<meta name="fluxer-api-origin" content="self">"#;
const HEAD_CLOSE_TAG: &str = "</head>";
const STATIC_PRECONNECT_TAGS: [&str; 2] = [
    r#"<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">"#,
    r#"<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}" crossorigin>"#,
];

pub async fn spa_catch_all(
    State(state): State<AppState>,
    headers: HeaderMap,
    request: Request,
) -> Response {
    let request_path = request.uri().path();

    if let Some(cache_control) = static_root_file_cache_control(request_path) {
        return serve_static_file(
            &state.budgets,
            &state.config.static_dir,
            request_path,
            cache_control,
            &headers,
        )
        .await;
    }
    if let Some(prefix) = static_asset_prefix(request_path) {
        if state
            .local_asset_prefixes
            .as_ref()
            .is_some_and(|present| !present.contains(&prefix))
        {
            return StatusCode::NOT_FOUND.into_response();
        }
        return serve_local_asset(
            &state.local_files,
            request_path.trim_start_matches('/'),
            &headers,
            state.csp.asset_header(),
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

fn static_asset_prefix(request_path: &str) -> Option<&'static str> {
    STATIC_ASSET_PREFIXES
        .iter()
        .copied()
        .find(|prefix| request_path.starts_with(prefix))
}

pub fn present_local_asset_prefixes(static_dir: &str) -> Arc<[&'static str]> {
    STATIC_ASSET_PREFIXES
        .iter()
        .copied()
        .filter(|prefix| {
            Path::new(static_dir)
                .join(prefix.trim_matches('/'))
                .is_dir()
        })
        .collect()
}

async fn serve_static_file(
    budgets: &AppProxyBudgets,
    static_dir: &str,
    request_path: &str,
    cache_control: &'static str,
    request_headers: &HeaderMap,
) -> Response {
    let Ok(_read_slot) = budgets.local_read_slots.try_acquire() else {
        return super::capacity_refused_response();
    };

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

    let mut response = match stream_file(&resolved, request_headers, None).await {
        Ok(response) => response,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return StatusCode::NOT_FOUND.into_response();
        }
        Err(err) => {
            tracing::error!(path = request_path, %err, "failed to read static file");
            return (StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error").into_response();
        }
    };

    let mime_type = guess_mime(request_path);
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
    let should_bust_dev_assets = state.spa_index_source.is_upstream();

    let static_cdn_endpoint = state
        .config
        .static_cdn_endpoint
        .as_ref()
        .map_or("", HttpEndpoint::as_str);
    let media_endpoint = state
        .config
        .media_endpoint
        .as_ref()
        .map_or("", HttpEndpoint::as_str);

    let raw_html = match load_spa_index_html(state, request_path).await {
        Ok(content) => content,
        Err(response) => return response,
    };
    let raw_html = if state.config.self_hosted {
        strip_link_preview_metadata(&raw_html)
    } else {
        raw_html
    };
    let raw_html = if serves_same_origin_host(&state.config.same_origin_hosts, headers) {
        mark_same_origin_api(&raw_html)
    } else {
        raw_html
    };

    let dev_buster = should_bust_dev_assets.then(current_dev_asset_cache_buster);
    let html = match render_spa_document(
        &raw_html,
        static_cdn_endpoint,
        media_endpoint,
        dev_buster.as_deref(),
    ) {
        Ok(html) => html,
        Err(error) => {
            tracing::error!(%error, "failed to render SPA document within its size limit");
            return StatusCode::INTERNAL_SERVER_ERROR.into_response();
        }
    };
    let csp = state.csp.spa_headers(&inline_script_hashes(&html));
    build_spa_response(html.into_boxed_str(), &csp, should_bust_dev_assets)
}

fn serves_same_origin_host(same_origin_hosts: &[String], headers: &HeaderMap) -> bool {
    if same_origin_hosts.is_empty() {
        return false;
    }
    let Some(host) = headers
        .get(header::HOST)
        .and_then(|value| value.to_str().ok())
        .and_then(request_hostname)
    else {
        return false;
    };
    same_origin_hosts
        .iter()
        .any(|candidate| candidate.eq_ignore_ascii_case(host))
}

fn request_hostname(authority: &str) -> Option<&str> {
    let authority = authority.trim();
    let hostname = if authority.starts_with('[') {
        &authority[..=authority.find(']')?]
    } else {
        authority.split(':').next()?
    };
    let hostname = hostname.trim_end_matches('.');
    (!hostname.is_empty()).then_some(hostname)
}

fn mark_same_origin_api(html: &str) -> String {
    html.replacen(
        HEAD_CLOSE_TAG,
        &format!("{SAME_ORIGIN_API_META_TAG}\n{HEAD_CLOSE_TAG}"),
        1,
    )
}

#[derive(Debug)]
struct SpaDocumentSizeLimitError {
    attempted_bytes: usize,
}

impl std::fmt::Display for SpaDocumentSizeLimitError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "rendered SPA document would be {} bytes, exceeding the {MAX_RENDERED_SPA_INDEX_BYTES} byte limit",
            self.attempted_bytes
        )
    }
}

impl std::error::Error for SpaDocumentSizeLimitError {}

fn bounded_document(document: String) -> Result<String, SpaDocumentSizeLimitError> {
    if document.len() > MAX_RENDERED_SPA_INDEX_BYTES {
        return Err(SpaDocumentSizeLimitError {
            attempted_bytes: document.len(),
        });
    }
    Ok(document)
}

fn render_spa_document(
    html: &str,
    static_cdn_endpoint: &str,
    media_endpoint: &str,
    dev_asset_cache_buster: Option<&str>,
) -> Result<String, SpaDocumentSizeLimitError> {
    let mut document =
        bounded_document(render_spa_shell(html, static_cdn_endpoint, media_endpoint))?;
    if let Some(buster) = dev_asset_cache_buster {
        document = bounded_document(append_dev_asset_cache_buster(&document, buster))?;
    }
    Ok(document)
}

fn render_spa_shell(html: &str, static_cdn_endpoint: &str, media_endpoint: &str) -> String {
    let static_cdn = static_cdn_endpoint.trim_end_matches('/');
    let media = media_endpoint.trim_end_matches('/');

    let document = html.replace(CSP_NONCE_PLACEHOLDER_ATTRIBUTE, "");
    let document = apply_static_preconnect(document, static_cdn);
    let document = document.replace("{{STATIC_CDN_ENDPOINT}}", static_cdn);
    apply_media_preconnect(&document, media, static_cdn)
}

fn apply_static_preconnect(mut html: String, static_cdn: &str) -> String {
    if !static_cdn.is_empty() {
        return html;
    }
    for tag in STATIC_PRECONNECT_TAGS {
        html = html.replace(&format!("{tag}\n"), "").replace(tag, "");
    }
    html
}

fn apply_media_preconnect(html: &str, media: &str, static_cdn: &str) -> String {
    if media.is_empty() || media == static_cdn {
        return html
            .replace(&format!("{MEDIA_PRECONNECT_TAG}\n"), "")
            .replace(MEDIA_PRECONNECT_TAG, "")
            .replace("{{MEDIA_ENDPOINT}}", "");
    }
    html.replace("{{MEDIA_ENDPOINT}}", media)
}

fn strip_link_preview_metadata(html: &str) -> String {
    let html = remove_elements(html, "<title", "</title>");
    remove_elements(&html, r#"<meta name="description""#, ">")
}

fn remove_elements(html: &str, start: &str, end: &str) -> String {
    let mut rest = html;
    let mut output = String::with_capacity(html.len());

    while let Some(index) = rest.find(start) {
        let Some(length) = rest[index..].find(end) else {
            break;
        };
        output.push_str(&rest[..index]);
        rest = rest[index + length + end.len()..].trim_start_matches('\n');
    }

    output.push_str(rest);
    output
}

const AUTH_ENTRY_ROOTS: &[&str] = &[
    "/login",
    "/register",
    "/forgot",
    "/reset",
    "/verify",
    "/authorize-ip",
    "/wasntme",
];
const AUTH_ENTRY_CODE_ROOTS: &[&str] = &["/invite/", "/gift/", "/theme/"];

fn is_auth_entry_path(path: &str) -> bool {
    let path = path
        .strip_suffix('/')
        .filter(|p| !p.is_empty())
        .unwrap_or(path);
    if AUTH_ENTRY_ROOTS.iter().any(|root| {
        path.strip_prefix(root)
            .is_some_and(|rest| rest.is_empty() || rest.starts_with('/'))
    }) {
        return true;
    }
    AUTH_ENTRY_CODE_ROOTS.iter().any(|root| {
        path.strip_prefix(root).is_some_and(|rest| {
            let (code, tail) = rest.split_once('/').unwrap_or((rest, ""));
            !code.is_empty() && (tail.is_empty() || tail == "login")
        })
    })
}

#[allow(clippy::result_large_err)]
async fn load_spa_index_html(state: &AppState, request_path: &str) -> Result<String, Response> {
    let index_upstream_url = match &state.spa_index_source {
        SpaIndexSource::Bundled {
            shell,
            auth_entry_shell,
        } => {
            let shell = auth_entry_shell
                .as_ref()
                .filter(|_| is_auth_entry_path(request_path))
                .unwrap_or(shell);
            return Ok(shell.to_string());
        }
        SpaIndexSource::Upstream(index_upstream_url) => index_upstream_url,
    };
    let response = state
        .http_client
        .get(index_upstream_url.as_url().clone())
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
    if response
        .content_length()
        .is_some_and(|length| length > MAX_SPA_INDEX_BYTES as u64)
    {
        tracing::error!(url = %index_upstream_url, "upstream index.html exceeds the size limit");
        return Err(StatusCode::BAD_GATEWAY.into_response());
    }
    let mut response = response;
    let mut bytes: Vec<u8> = Vec::new();
    loop {
        let chunk = response.chunk().await.map_err(|err| {
            tracing::error!(url = %index_upstream_url, %err, "failed to read upstream index.html body");
            StatusCode::BAD_GATEWAY.into_response()
        })?;
        let Some(chunk) = chunk else {
            break;
        };
        if chunk.len() > MAX_SPA_INDEX_BYTES - bytes.len() {
            tracing::error!(url = %index_upstream_url, "upstream index.html exceeds the size limit");
            return Err(StatusCode::BAD_GATEWAY.into_response());
        }
        bytes.extend_from_slice(&chunk);
    }
    String::from_utf8(bytes).map_err(|err| {
        tracing::error!(url = %index_upstream_url, %err, "upstream index.html is not valid UTF-8");
        StatusCode::BAD_GATEWAY.into_response()
    })
}

struct SpaDocumentBody {
    html: Box<str>,
}

impl AsRef<[u8]> for SpaDocumentBody {
    fn as_ref(&self) -> &[u8] {
        self.html.as_bytes()
    }
}

fn build_spa_response(html: Box<str>, csp: &CspHeaderValues, dev_no_store: bool) -> Response {
    let body = Bytes::from_owner(SpaDocumentBody { html });
    let mut response = Response::new(Body::from(body));
    let headers = response.headers_mut();

    headers.insert(header::CONTENT_SECURITY_POLICY, csp.enforced.clone());
    if let Some(report_only) = &csp.report_only {
        headers.insert(
            header::CONTENT_SECURITY_POLICY_REPORT_ONLY,
            report_only.clone(),
        );
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
        headers.insert(
            header::CACHE_CONTROL,
            HeaderValue::from_static(SHARED_SHELL_CACHE_CONTROL),
        );
    }
    super::set_security_headers(headers);
    headers.insert(
        HeaderName::from_static("x-fluxer-app-shell"),
        HeaderValue::from_static("1"),
    );
    headers.insert(
        axum::http::HeaderName::from_static("accept-ch"),
        HeaderValue::from_static(ACCEPT_CH_VALUE),
    );
    headers.insert(
        axum::http::HeaderName::from_static("critical-ch"),
        HeaderValue::from_static(CRITICAL_CH_VALUE),
    );
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
    use crate::static_asset_policy::LONG_LIVED_ASSET_CACHE_CONTROL;

    fn is_static_root_file(request_path: &str) -> bool {
        static_root_file_cache_control(request_path).is_some()
    }

    use crate::config::{AppProxyConfig, ReleaseChannel};
    use crate::state::LOCAL_FILE_READS_IN_FLIGHT_MAX;
    use axum::Router;
    use axum::body::Body;

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

    const SHELL_WITH_A_NONCE_HOLE: &str = r#"<!doctype html><html><head><title>Fluxer</title><script nonce="{{CSP_NONCE_PLACEHOLDER}}">inline()</script><script src="/assets/app.js"></script></head><body></body></html>"#;

    #[test]
    fn the_rendered_document_drops_the_nonce_hole_and_injects_no_script() {
        let rendered = render_spa_document(
            SHELL_WITH_A_NONCE_HOLE,
            "https://static.example.test",
            "",
            None,
        )
        .expect("test SPA document must render within its size limit");

        assert!(!rendered.contains("{{CSP_NONCE_PLACEHOLDER}}"));
        assert!(!rendered.contains("nonce"));
        assert!(rendered.contains("<script>inline()</script>"));
        assert_eq!(rendered.matches("<script").count(), 2);
    }

    #[test]
    fn the_dev_cache_buster_reaches_the_rendered_document_only_when_supplied() {
        let busted = render_spa_document(SHELL_WITH_A_NONCE_HOLE, "", "", Some("9911"))
            .expect("test SPA document must render within its size limit");
        let untouched = render_spa_document(SHELL_WITH_A_NONCE_HOLE, "", "", None)
            .expect("test SPA document must render within its size limit");

        assert!(busted.contains(r#"src="/assets/app.js?_=9911""#));
        assert!(untouched.contains(r#"src="/assets/app.js""#));
        assert!(!untouched.contains("_=9911"));
    }

    const SHELL_WITH_ENDPOINT_HOLES: &str = r#"<!doctype html><html><head><title>Fluxer</title><link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}">
<link rel="preconnect" href="{{STATIC_CDN_ENDPOINT}}" crossorigin>
<link rel="preconnect" href="{{MEDIA_ENDPOINT}}">
<link rel="icon" type="image/png" sizes="32x32" href="{{STATIC_CDN_ENDPOINT}}/web/favicon-32x32.png"><link rel="apple-touch-icon" sizes="180x180" href="{{STATIC_CDN_ENDPOINT}}/web/apple-touch-icon.png"><script>inline()</script><script src="/assets/app.js"></script></head><body></body></html>"#;

    #[test]
    fn the_static_cdn_argument_resolves_every_hole_the_shell_has() {
        let rendered = render_spa_document(
            SHELL_WITH_ENDPOINT_HOLES,
            "https://cdn.example.test/",
            "https://media.example.test",
            None,
        )
        .expect("test SPA document must render within its size limit");

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
            "https://cdn.example.test",
            "https://media.example.test/",
            None,
        )
        .expect("test SPA document must render within its size limit");
        assert!(
            distinct.contains(r#"<link rel="preconnect" href="https://media.example.test">"#),
            "the media argument never reached the media preconnect"
        );
        assert!(!distinct.contains("{{MEDIA_ENDPOINT}}"));
        assert_eq!(distinct.matches("preconnect").count(), 3);

        let shared = render_spa_document(
            SHELL_WITH_ENDPOINT_HOLES,
            "https://cdn.example.test",
            "https://cdn.example.test",
            None,
        )
        .expect("test SPA document must render within its size limit");
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

    const SHIPPED_APP_SHELL: &str = include_str!("../../../fluxer_app/index.html");
    const AUTH_ENTRY_TEST_SHELL: &str = r#"<!doctype html><html><head><style>.auth-entry-shell{}</style></head><body><div id="root"></div></body></html>"#;

    #[test]
    fn the_shipped_shell_loses_every_link_preview_field_when_stripped() {
        assert!(SHIPPED_APP_SHELL.contains("<title>"));
        assert!(SHIPPED_APP_SHELL.contains(r#"<meta name="description""#));

        let stripped = strip_link_preview_metadata(SHIPPED_APP_SHELL);

        assert!(!stripped.contains("<title"));
        assert!(!stripped.contains(r#"name="description""#));
        assert!(!stripped.contains("og:"));
        assert!(!stripped.contains("twitter:"));
        assert!(stripped.contains(r#"<meta name="viewport""#));
        assert!(stripped.contains(r#"nonce="{{CSP_NONCE_PLACEHOLDER}}""#));
    }

    #[test]
    fn the_shipped_shell_renders_without_a_bootstrap_payload() {
        let rendered = render_spa_document(SHIPPED_APP_SHELL, "https://cdn.example.test", "", None)
            .expect("the shipped shell must render within its size limit");

        assert!(!rendered.contains("FLUXER_BOOTSTRAP"));
        assert!(!rendered.contains("{{"));
        assert!(!rendered.contains("nonce"));
    }

    fn static_dir_with(prefix_dirs: &[&str]) -> std::path::PathBuf {
        static NEXT_DIR: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let unique = NEXT_DIR.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!(
            "fluxer-static-prefixes-{}-{unique}",
            std::process::id()
        ));
        std::fs::create_dir_all(&root).unwrap();
        for dir in prefix_dirs {
            std::fs::create_dir_all(root.join(dir)).unwrap();
        }
        root
    }

    #[test]
    fn only_prefixes_with_a_directory_on_disk_are_present() {
        let root = static_dir_with(&["emoji", "web"]);
        std::fs::write(root.join("badges"), b"a file, not a directory").unwrap();

        let present = present_local_asset_prefixes(root.to_str().unwrap());

        assert_eq!(&*present, &["/emoji/", "/web/"]);
        std::fs::remove_dir_all(root).unwrap();
    }

    #[tokio::test]
    async fn a_prefix_missing_at_startup_is_refused_without_a_file_read_slot() {
        let mut state = spa_state_serving(SHIPPED_APP_SHELL);
        state.local_asset_prefixes = Some(Arc::from([] as [&str; 0]));
        let _every_slot = state
            .budgets
            .local_read_slots
            .clone()
            .try_acquire_many_owned(LOCAL_FILE_READS_IN_FLIGHT_MAX as u32)
            .unwrap();

        let response = spa_catch_all(
            State(state),
            HeaderMap::new(),
            Request::builder()
                .uri("/emoji/1f600.svg")
                .body(Body::empty())
                .unwrap(),
        )
        .await;

        assert_eq!(response.status(), StatusCode::NOT_FOUND);
    }

    #[tokio::test]
    async fn a_prefix_present_at_startup_is_still_served_from_disk() {
        let root = static_dir_with(&["emoji"]);
        std::fs::write(root.join("emoji").join("1f600.svg"), b"<svg/>").unwrap();
        let mut state = spa_state_serving(SHIPPED_APP_SHELL);
        let mut config = (*state.config).clone();
        config.static_dir = root.to_str().unwrap().to_owned();
        state.local_asset_prefixes = Some(present_local_asset_prefixes(&config.static_dir));
        state.local_files =
            crate::local_files::LocalFileStore::load_blocking(&root, &state.budgets)
                .expect("the test static directory must load");
        state.config = Arc::new(config);

        let response = spa_catch_all(
            State(state),
            HeaderMap::new(),
            Request::builder()
                .uri("/emoji/1f600.svg")
                .body(Body::empty())
                .unwrap(),
        )
        .await;

        assert_eq!(response.status(), StatusCode::OK);
        std::fs::remove_dir_all(root).unwrap();
    }

    #[tokio::test]
    async fn a_self_hosted_instance_serves_no_link_preview_metadata() {
        let state = assemble_spa_state(SpaIndexSource::bundled(SHIPPED_APP_SHELL), None, true);

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;
        assert_eq!(response.status(), StatusCode::OK);
        let served = read_document(response).await;

        assert!(!served.contains("<title"));
        assert!(!served.contains(r#"name="description""#));
        assert!(served.contains(r#"<meta name="viewport""#));
    }

    #[tokio::test]
    async fn the_official_instance_keeps_its_link_preview_metadata() {
        let state = spa_state_serving(SHIPPED_APP_SHELL);

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;
        assert_eq!(response.status(), StatusCode::OK);
        let served = read_document(response).await;

        assert!(served.contains("<title>Fluxer</title>"));
        assert!(served.contains(r#"<meta name="description""#));
    }

    fn request_for_host(host: &'static str) -> HeaderMap {
        let mut headers = HeaderMap::new();
        headers.insert(header::HOST, HeaderValue::from_static(host));
        headers
    }

    #[tokio::test]
    async fn a_same_origin_host_marks_the_document_as_serving_its_own_api() {
        let mut state = spa_state_serving(SHIPPED_APP_SHELL);
        Arc::make_mut(&mut state.config).same_origin_hosts =
            vec!["fluxer.com".to_owned(), "canary.fluxer.com".to_owned()];

        for host in ["fluxer.com", "FLUXER.com:443", "canary.fluxer.com."] {
            let response = serve_spa_index(&state, &request_for_host(host), "/").await;
            assert_eq!(response.status(), StatusCode::OK);
            let policy = policy_of(&response);
            let served = read_document(response).await;
            assert_eq!(
                served.matches(SAME_ORIGIN_API_META_TAG).count(),
                1,
                "{host} was not marked as serving its own API"
            );
            assert!(
                served.find(SAME_ORIGIN_API_META_TAG).unwrap() < served.find("</head>").unwrap()
            );
            assert_every_inline_script_is_granted(&served, &policy);
        }

        for host in ["web.fluxer.app", "fluxer.com.evil.test", "notfluxer.com"] {
            let served =
                read_document(serve_spa_index(&state, &request_for_host(host), "/").await).await;
            assert!(
                !served.contains(SAME_ORIGIN_API_META_TAG),
                "{host} was marked although it is not a same-origin host"
            );
        }
        let served = read_document(serve_spa_index(&state, &HeaderMap::new(), "/").await).await;
        assert!(!served.contains(SAME_ORIGIN_API_META_TAG));
    }

    #[tokio::test]
    async fn the_configured_media_endpoint_is_preconnected() {
        let mut state = assemble_spa_state(
            SpaIndexSource::bundled(SHELL_WITH_ENDPOINT_HOLES),
            Some("https://cdn.example.test"),
            false,
        );
        Arc::make_mut(&mut state.config).media_endpoint = Some(
            HttpEndpoint::parse("TEST_MEDIA_ENDPOINT", "https://media.example.test")
                .expect("test media endpoint must be a valid HTTP endpoint"),
        );

        let served = read_document(serve_spa_index(&state, &HeaderMap::new(), "/").await).await;

        assert!(served.contains(r#"<link rel="preconnect" href="https://media.example.test">"#));
        assert!(!served.contains("{{MEDIA_ENDPOINT}}"));
    }

    #[test]
    fn auth_entry_paths_are_the_signed_out_entry_routes() {
        for path in [
            "/login",
            "/login/",
            "/register",
            "/forgot",
            "/reset/abc",
            "/verify",
            "/authorize-ip",
            "/wasntme/token",
            "/invite/abc",
            "/invite/abc/login",
            "/gift/abc/",
            "/theme/123/login",
        ] {
            assert!(is_auth_entry_path(path), "{path} is an auth entry route");
        }
        for path in [
            "/",
            "/app",
            "/channels/@me",
            "/loginx",
            "/invite/",
            "/invite/abc/other",
            "/theme-studio",
            "/oauth2/authorize",
        ] {
            assert!(
                !is_auth_entry_path(path),
                "{path} is not an auth entry route"
            );
        }
    }

    #[tokio::test]
    async fn auth_entry_routes_are_served_the_auth_entry_shell() {
        let mut state = spa_state_serving(SHIPPED_APP_SHELL);
        state.spa_index_source = SpaIndexSource::Bundled {
            shell: Arc::from(SHIPPED_APP_SHELL),
            auth_entry_shell: Some(Arc::from(AUTH_ENTRY_TEST_SHELL)),
        };

        let auth = read_document(serve_spa_index(&state, &HeaderMap::new(), "/login").await).await;
        let app =
            read_document(serve_spa_index(&state, &HeaderMap::new(), "/channels/@me").await).await;

        assert!(auth.contains("auth-entry-shell"));
        assert!(!app.contains("auth-entry-shell"));
    }

    #[tokio::test]
    async fn auth_entry_routes_fall_back_to_the_app_shell_without_an_auth_entry_shell() {
        let state = spa_state_serving(SHIPPED_APP_SHELL);

        let auth = read_document(serve_spa_index(&state, &HeaderMap::new(), "/login").await).await;

        assert!(auth.contains(r#"<meta name="viewport""#));
        assert!(!auth.contains("auth-entry-shell"));
    }

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

    fn spa_state_serving(shell: &str) -> AppState {
        assemble_spa_state(
            SpaIndexSource::bundled(shell),
            Some("https://cdn.example.test"),
            false,
        )
    }

    fn spa_state_reading_its_shell_from(index_upstream_url: String) -> AppState {
        let url = crate::config::HttpUrl::parse("TEST_INDEX_UPSTREAM_URL", &index_upstream_url)
            .expect("test index upstream URL must be a valid HTTP URL");
        assemble_spa_state(
            SpaIndexSource::Upstream(Arc::new(url)),
            Some("https://cdn.example.test"),
            false,
        )
    }

    fn assemble_spa_state(
        spa_index_source: SpaIndexSource,
        static_cdn_endpoint: Option<&str>,
        self_hosted: bool,
    ) -> AppState {
        let mut config = AppProxyConfig::from_env();
        config.release_channel = ReleaseChannel::Stable;
        config.index_upstream_url = match &spa_index_source {
            SpaIndexSource::Bundled { .. } => None,
            SpaIndexSource::Upstream(url) => Some((**url).clone()),
        };
        config.static_cdn_endpoint = static_cdn_endpoint.map(|endpoint| {
            HttpEndpoint::parse("TEST_STATIC_CDN_ENDPOINT", endpoint)
                .expect("test static CDN endpoint must be a valid HTTP endpoint")
        });
        config.self_hosted = self_hosted;

        let csp = Arc::new(
            crate::csp::CompiledCspPolicy::from_config(&config)
                .expect("the test configuration must compile to a valid CSP"),
        );
        let budgets = crate::state::AppProxyBudgets::default();
        let local_files =
            crate::local_files::LocalFileStore::load_blocking(std::path::Path::new("."), &budgets)
                .expect("the test static directory must exist");
        AppState {
            config: Arc::new(config),
            csp,
            http_client: reqwest::Client::new(),
            spa_index_source,
            local_asset_prefixes: None,
            budgets,
            local_files,
        }
    }

    fn policy_of(response: &Response) -> String {
        response
            .headers()
            .get(header::CONTENT_SECURITY_POLICY)
            .expect("the document was served without a content security policy")
            .to_str()
            .unwrap()
            .to_owned()
    }

    fn script_hashes_granted_by(policy: &str) -> Vec<String> {
        policy
            .split("; ")
            .find(|directive| directive.starts_with("script-src "))
            .expect("the content security policy has no script-src directive")
            .split(' ')
            .filter(|source| source.starts_with("'sha256-"))
            .map(str::to_owned)
            .collect()
    }

    fn bare_inline_scripts_in(document: &str) -> Vec<&str> {
        document
            .split("<script>")
            .skip(1)
            .map(|rest| {
                &rest[..rest
                    .find("</script>")
                    .expect("an inline script in the served document is never closed")]
            })
            .collect()
    }

    fn sha256_source(text: &str) -> String {
        use base64::Engine as _;
        use sha2::Digest as _;
        format!(
            "'sha256-{}'",
            base64::engine::general_purpose::STANDARD.encode(sha2::Sha256::digest(text))
        )
    }

    fn assert_every_inline_script_is_granted(document: &str, policy: &str) {
        for tag in document.split("<script").skip(1) {
            let tag = &tag[..tag.find('>').unwrap()];
            assert!(
                tag.is_empty() || tag.contains(" src="),
                "the served document has a script tag the test cannot classify: <script{tag}>"
            );
        }
        let inline = bare_inline_scripts_in(document);
        assert!(
            !inline.is_empty(),
            "the served document has no inline script at all"
        );
        let mut expected: Vec<String> = inline.iter().map(|script| sha256_source(script)).collect();
        expected.sort();
        expected.dedup();
        let mut granted = script_hashes_granted_by(policy);
        granted.sort();
        assert_eq!(
            granted, expected,
            "the policy must grant exactly the inline scripts the document contains"
        );
        assert!(!document.contains("nonce"));
        assert!(!policy.contains("nonce"));
    }

    async fn read_document(response: Response) -> String {
        let bytes = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        String::from_utf8(bytes.to_vec()).unwrap()
    }

    #[tokio::test]
    async fn the_spa_document_marks_itself_as_the_app_shell() {
        let state = spa_state_serving(SHELL_WITH_ENDPOINT_HOLES);

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;
        assert_eq!(response.status(), StatusCode::OK);
        assert_eq!(
            response
                .headers()
                .get("x-fluxer-app-shell")
                .and_then(|value| value.to_str().ok()),
            Some("1")
        );
    }

    #[tokio::test]
    async fn the_live_branch_serves_a_rendered_document_and_not_the_raw_shell() {
        let state = spa_state_serving(SHELL_WITH_ENDPOINT_HOLES);

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;
        assert_eq!(response.status(), StatusCode::OK);
        let policy = policy_of(&response);
        let served = read_document(response).await;

        assert_every_inline_script_is_granted(&served, &policy);
        assert!(
            !served.contains("{{STATIC_CDN_ENDPOINT}}"),
            "the live branch shipped an unresolved static CDN hole"
        );
        assert!(
            !served.contains("{{MEDIA_ENDPOINT}}"),
            "the live branch shipped an unresolved media hole"
        );
        assert!(
            !served.contains("__FLUXER_BOOTSTRAP__"),
            "the live branch injected a bootstrap payload"
        );
        assert!(
            served
                .contains(r#"<link rel="preconnect" href="https://cdn.example.test" crossorigin>"#),
            "the configured static CDN never reached the served document"
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

        let response = serve_static_file(
            &AppProxyBudgets::default(),
            static_dir,
            "/robots.txt",
            policy,
            &HeaderMap::new(),
        )
        .await;
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
        let state = spa_state_serving(SHELL_WITH_ENDPOINT_HOLES);

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;

        let cache_control = response
            .headers()
            .get(header::CACHE_CONTROL)
            .expect("the shell was served without a cache policy at all")
            .to_str()
            .unwrap();
        assert_eq!(
            cache_control, SHARED_SHELL_CACHE_CONTROL,
            "the document naming the hashed bundle must be revalidated on every load and held \
             by a shared cache for one second at most"
        );
        assert!(response.headers().get(header::SET_COOKIE).is_none());
        assert_ne!(
            cache_control, LONG_LIVED_ASSET_CACHE_CONTROL,
            "a shell cached for a year pins every returning visitor to the deployed-over bundle"
        );
    }

    #[tokio::test]
    async fn an_index_upstream_replaces_the_snapshot_with_an_unstorable_busted_document() {
        let index_upstream_url = spawn_local_origin(SHELL_WITH_ENDPOINT_HOLES, "text/html").await;
        let state = spa_state_reading_its_shell_from(index_upstream_url);

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;
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
    async fn the_configured_static_cdn_reaches_the_served_document() {
        let state = assemble_spa_state(
            SpaIndexSource::bundled(SHELL_WITH_ENDPOINT_HOLES),
            Some("https://fallbackcdn.example.test"),
            false,
        );

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;
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
    async fn an_unconfigured_endpoint_warms_no_socket_at_all() {
        let state = assemble_spa_state(
            SpaIndexSource::bundled(SHELL_WITH_ENDPOINT_HOLES),
            None,
            false,
        );

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;
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

    #[tokio::test]
    async fn every_request_gets_a_byte_identical_shell_and_policy() {
        let state = spa_state_serving(SHIPPED_APP_SHELL);

        let first = serve_spa_index(&state, &HeaderMap::new(), "/").await;
        let second = serve_spa_index(&state, &HeaderMap::new(), "/").await;
        assert_eq!(first.status(), StatusCode::OK);
        assert_eq!(second.status(), StatusCode::OK);

        let first_policy = policy_of(&first);
        assert_eq!(first_policy, policy_of(&second));
        assert!(first.headers().get(header::SET_COOKIE).is_none());

        let first_body = read_document(first).await;
        assert_eq!(first_body, read_document(second).await);
        assert!(!first_body.contains("geoip"));
        assert!(!first_body.contains("countryCode"));
        assert_every_inline_script_is_granted(&first_body, &first_policy);
    }

    #[tokio::test]
    async fn the_shipped_shell_runs_every_inline_script_it_contains_under_its_policy() {
        let state = spa_state_serving(SHIPPED_APP_SHELL);

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;
        assert_eq!(response.status(), StatusCode::OK);
        let policy = policy_of(&response);
        let served = read_document(response).await;

        assert_eq!(
            served.matches("<script").count(),
            SHIPPED_APP_SHELL.matches("<script").count(),
            "every inline script of fluxer_app/index.html must reach the document"
        );
        assert_every_inline_script_is_granted(&served, &policy);
    }

    #[tokio::test]
    async fn an_index_upstream_document_is_granted_after_its_dev_cache_buster() {
        let index_upstream_url = spawn_local_origin(SHIPPED_APP_SHELL, "text/html").await;
        let state = spa_state_reading_its_shell_from(index_upstream_url);

        let response = serve_spa_index(&state, &HeaderMap::new(), "/").await;
        assert_eq!(response.status(), StatusCode::OK);
        let policy = policy_of(&response);
        let served = read_document(response).await;

        assert_every_inline_script_is_granted(&served, &policy);
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
