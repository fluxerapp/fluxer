// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::branding_images::BrandingSnapshot;
use crate::config::HttpEndpoint;
use crate::csp::{CspHeaderValues, inline_script_hashes};
use crate::instance_brand_colors::{THEMES, instance_brand_variables};
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
const RELEASE_CHANNEL_META_NAME: &str = "fluxer-release-channel";
const HEAD_CLOSE_TAG: &str = "</head>";
const TITLE_START: &str = "<title";
const TITLE_END: &str = "</title>";
const FAVICON_LINK_START: &str = r#"<link rel="icon""#;
const LIGHT_SCHEME_MEDIA: &str = "(prefers-color-scheme: light)";
const DARK_SCHEME_MEDIA: &str = "(prefers-color-scheme: dark)";
const APPLE_TOUCH_ICON_LINK_START: &str = r#"<link rel="apple-touch-icon""#;
const APPLE_TITLE_META_START: &str = r#"<meta name="apple-mobile-web-app-title""#;
const THEME_COLOR_META_STARTS: [&str; 3] = [
    r#"<meta name="theme-color""#,
    r#"<meta name="msapplication-navbutton-color""#,
    r#"<meta name="msapplication-TileColor""#,
];
const THEME_COLOR_STYLE_ID: &str = "fluxer-instance-theme-color";
const CUSTOM_CSS_STYLE_ID: &str = "fluxer-instance-custom-css";
const INSTANCE_BRAND_PREBOOT_SCRIPT: &str = include_str!("instance_brand_preboot.js");
const SATURATION_PREBOOT_SCRIPT: &str = include_str!("saturation_preboot.js");
const NARROW_AUTH_CARD_META_TAG: &str = r#"<meta name="fluxer-auth-card" content="narrow">"#;
const INLINE_CUSTOM_CSS_BYTES_MAX: usize = 64 * 1024;
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
    let branding = state.branding_snapshot();
    let raw_html = if state.config.self_hosted {
        apply_instance_branding(strip_link_preview_metadata(&raw_html), &branding)
    } else {
        raw_html
    };
    let mut metadata = String::new();
    if serves_same_origin_host(&state.config.same_origin_hosts, headers) {
        metadata.push_str(SAME_ORIGIN_API_META_TAG);
        metadata.push('\n');
    }
    if let Some(channel) = state.config.release_channel {
        metadata.push_str(&format!(
            r#"<meta name="{RELEASE_CHANNEL_META_NAME}" content="{channel}">
"#
        ));
    }
    let raw_html = if metadata.is_empty() {
        raw_html
    } else {
        metadata.push_str(HEAD_CLOSE_TAG);
        raw_html.replacen(HEAD_CLOSE_TAG, &metadata, 1)
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
    let csp = state.csp.spa_headers(
        &inline_script_hashes(&html),
        &branding.image_origins,
        &branding.instance_sources,
    );
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

#[derive(Debug, thiserror::Error)]
#[error(
    "rendered SPA document would be {attempted_bytes} bytes, exceeding the {MAX_RENDERED_SPA_INDEX_BYTES} byte limit"
)]
struct SpaDocumentSizeLimitError {
    attempted_bytes: usize,
}

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
    replace_elements(html, r#"<meta name="description""#, ">", "")
}

fn link_preview_tags(product_name: &str, image_url: Option<&str>) -> String {
    let mut tags = format!(
        r#"<meta property="og:site_name" content="{product_name}"><meta property="og:title" content="{product_name}"><meta property="og:type" content="website">"#
    );
    if let Some(image_url) = image_url {
        tags.push_str(&format!(
            r#"<meta property="og:image" content="{}">"#,
            escape_html(image_url)
        ));
    }
    tags.push_str(r#"<meta name="twitter:card" content="summary">"#);
    tags
}

fn apply_instance_branding(mut html: String, branding: &BrandingSnapshot) -> String {
    if let Some(product_name) = &branding.product_name {
        let product_name = escape_html(product_name);
        let preview = link_preview_tags(&product_name, branding.icon_url.as_deref());
        html = html.replacen(HEAD_CLOSE_TAG, &format!("{preview}{HEAD_CLOSE_TAG}"), 1);
        html = replace_elements(
            &html,
            TITLE_START,
            TITLE_END,
            &format!("{TITLE_START}>{product_name}{TITLE_END}"),
        );
        html = replace_elements(
            &html,
            APPLE_TITLE_META_START,
            ">",
            &format!(r#"{APPLE_TITLE_META_START} content="{product_name}">"#),
        );
    }
    if let Some(favicon_url) = &branding.favicon_url {
        html = replace_elements(
            &html,
            FAVICON_LINK_START,
            ">",
            &format!(
                r#"{FAVICON_LINK_START} href="{}">"#,
                escape_html(favicon_url)
            ),
        );
    }
    if let Some(favicon_dark_url) = &branding.favicon_dark_url {
        let light_link_start = format!(r#"{FAVICON_LINK_START} media="{LIGHT_SCHEME_MEDIA}""#);
        html = html.replace(FAVICON_LINK_START, &light_link_start);
        html = html.replacen(
            &light_link_start,
            &format!(
                r#"{FAVICON_LINK_START} href="{}" media="{DARK_SCHEME_MEDIA}">{light_link_start}"#,
                escape_html(favicon_dark_url)
            ),
            1,
        );
    }
    if let Some(touch_icon_url) = branding.icon_url.as_ref().or(branding.favicon_url.as_ref()) {
        html = replace_elements(
            &html,
            APPLE_TOUCH_ICON_LINK_START,
            ">",
            &format!(
                r#"{APPLE_TOUCH_ICON_LINK_START} href="{}">"#,
                escape_html(touch_icon_url)
            ),
        );
    }
    let mut first_paint_styles = format!(
        r#"<script data-cfasync="false">{}</script>"#,
        SATURATION_PREBOOT_SCRIPT.trim_end()
    );
    if branding.auth_card_narrow {
        first_paint_styles.push_str(NARROW_AUTH_CARD_META_TAG);
    }
    if let Some(theme_color) = &branding.theme_color {
        let theme_color = escape_html(theme_color);
        for start in THEME_COLOR_META_STARTS {
            html = replace_elements(
                &html,
                start,
                ">",
                &format!(r#"{start} content="{theme_color}">"#),
            );
        }
        if let Some(rules) = instance_brand_rules(&theme_color) {
            first_paint_styles.push_str(&format!(
                r#"<style id="{THEME_COLOR_STYLE_ID}" data-color="{theme_color}">{rules}</style><script data-cfasync="false">{}</script>"#,
                INSTANCE_BRAND_PREBOOT_SCRIPT.trim_end()
            ));
        }
    }
    if let Some(css) = branding.custom_css.as_deref().and_then(inline_custom_css) {
        first_paint_styles.push_str(&format!(
            r#"<style id="{CUSTOM_CSS_STYLE_ID}">{css}</style>"#
        ));
    }
    html.replacen(
        HEAD_CLOSE_TAG,
        &format!("{first_paint_styles}{HEAD_CLOSE_TAG}"),
        1,
    )
}

fn inline_custom_css(css: &str) -> Option<String> {
    let css = css.replace('<', "\\3c ");
    (css.len() <= INLINE_CUSTOM_CSS_BYTES_MAX).then_some(css)
}

fn instance_brand_rules(theme_color: &str) -> Option<String> {
    let dark = instance_brand_variables(theme_color, THEMES[0])?;
    let declarations = |variables: &[(&str, String)]| {
        variables
            .iter()
            .map(|(name, value)| format!("{name}:{value}"))
            .collect::<Vec<_>>()
            .join(";")
    };
    let mut rules = format!(":root{{{}}}", declarations(&dark));
    for theme in &THEMES[1..] {
        let differing: Vec<(&str, String)> = instance_brand_variables(theme_color, *theme)?
            .into_iter()
            .zip(&dark)
            .filter(|((_, value), (_, dark_value))| value != dark_value)
            .map(|(variable, _)| variable)
            .collect();
        if !differing.is_empty() {
            rules.push_str(&format!(
                ":root.theme-{theme}{{{}}}",
                declarations(&differing)
            ));
        }
    }
    Some(rules)
}

fn escape_html(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('"', "&quot;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

fn replace_elements(html: &str, start: &str, end: &str, replacement: &str) -> String {
    let mut rest = html;
    let mut output = String::with_capacity(html.len() + replacement.len());

    while let Some(index) = rest.find(start) {
        let Some(length) = rest[index..].find(end) else {
            break;
        };
        output.push_str(&rest[..index]);
        output.push_str(replacement);
        rest = &rest[index + length + end.len()..];
        if replacement.is_empty() {
            rest = rest.trim_start_matches('\n');
        }
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

    #[test]
    fn inlined_custom_css_cannot_close_its_style_element() {
        let branding = BrandingSnapshot {
            custom_css: Some(Arc::from(
                r#"a::after { content: "</STYLE><script>x()</script>"; }"#,
            )),
            ..BrandingSnapshot::default()
        };

        assert_eq!(
            apply_instance_branding("<head></head>".to_owned(), &branding),
            format!(
                r#"<head>{}<style id="fluxer-instance-custom-css">a::after {{ content: "\3c /STYLE>\3c script>x()\3c /script>"; }}</style></head>"#,
                saturation_preboot_tag()
            )
        );
    }

    #[test]
    fn branded_head_values_cannot_break_out_of_their_markup() {
        let html = r#"<head><title>Fluxer</title><meta name="apple-mobile-web-app-title" content="Fluxer"><link rel="icon" type="image/png" href="/web/favicon-32x32.png"><link rel="apple-touch-icon" href="/web/apple-touch-icon.png"></head>"#;
        let branding = BrandingSnapshot {
            product_name: Some(Arc::from(r#"A & B</title><script>x()</script>""#)),
            favicon_url: Some(Arc::from(r#"https://example.test/"><script>x()</script>"#)),
            ..BrandingSnapshot::default()
        };

        assert_eq!(
            apply_instance_branding(html.to_owned(), &branding),
            format!(
                r#"<head><title>A &amp; B&lt;/title&gt;&lt;script&gt;x()&lt;/script&gt;&quot;</title><meta name="apple-mobile-web-app-title" content="A &amp; B&lt;/title&gt;&lt;script&gt;x()&lt;/script&gt;&quot;"><link rel="icon" href="https://example.test/&quot;&gt;&lt;script&gt;x()&lt;/script&gt;"><link rel="apple-touch-icon" href="https://example.test/&quot;&gt;&lt;script&gt;x()&lt;/script&gt;"><meta property="og:site_name" content="A &amp; B&lt;/title&gt;&lt;script&gt;x()&lt;/script&gt;&quot;"><meta property="og:title" content="A &amp; B&lt;/title&gt;&lt;script&gt;x()&lt;/script&gt;&quot;"><meta property="og:type" content="website"><meta name="twitter:card" content="summary">{}</head>"#,
                saturation_preboot_tag()
            )
        );
    }

    fn saturation_preboot_tag() -> String {
        format!(
            r#"<script data-cfasync="false">{}</script>"#,
            include_str!("saturation_preboot.js").trim_end()
        )
    }
}
