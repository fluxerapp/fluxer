// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::discovery_cache::DiscoveryResponse;
use crate::state::AppState;
use crate::time_freeze::{
    TimeFreezeConfig, describe_decision, load_time_freeze_config_for_request,
    time_freeze_debug_header,
};
use axum::{
    extract::State,
    http::{HeaderMap, HeaderValue, StatusCode, header},
    response::{IntoResponse, Response},
};
use std::path::Path;

fn serve_frozen_file(
    config: &TimeFreezeConfig,
    pick: impl FnOnce(&crate::time_freeze::FrozenSnapshot) -> Option<(&[u8], &str)>,
) -> (Option<Response>, Option<String>) {
    let debug = describe_decision(config);
    let debug_header = time_freeze_debug_header(config);

    if debug.decision != crate::time_freeze::TimeFreezeDecision::Frozen {
        return (None, debug_header);
    }

    if let Some(snapshot) = &config.snapshot
        && let Some((bytes, content_type)) = pick(snapshot)
    {
        let mut response = bytes.to_vec().into_response();
        if let Ok(ct) = HeaderValue::from_str(content_type) {
            response.headers_mut().insert(header::CONTENT_TYPE, ct);
        }
        response
            .headers_mut()
            .insert(header::CACHE_CONTROL, HeaderValue::from_static("no-cache"));
        set_time_freeze_header(&mut response, debug_header.as_deref());
        return (Some(response), None);
    }

    (None, debug_header)
}

pub async fn version_json(State(state): State<AppState>, headers: HeaderMap) -> Response {
    let tf = load_time_freeze_config_for_request(&state.config, &headers);
    let (frozen, debug_header) = serve_frozen_file(&tf, |snap| {
        Some((snap.version_json.as_slice(), "application/json"))
    });
    if let Some(resp) = frozen {
        return resp;
    }

    let mut result =
        serve_static_text_file(&state.config.static_dir, "version.json", "application/json");

    if result.status() == StatusCode::NOT_FOUND && !state.config.build_version.is_empty() {
        let body = serde_json::json!({ "version": state.config.build_version });
        result = axum::Json(body).into_response();
        result
            .headers_mut()
            .insert(header::CACHE_CONTROL, HeaderValue::from_static("no-cache"));
    }

    set_time_freeze_header(&mut result, debug_header.as_deref());
    result
}

pub async fn manifest_json(State(state): State<AppState>) -> Response {
    let static_cdn_endpoint = runtime_static_cdn_endpoint(&state).await;
    serve_static_text_file_with_cdn(
        &state.config.static_dir,
        "manifest.json",
        "application/manifest+json",
        static_cdn_endpoint.as_deref(),
    )
}

pub async fn browserconfig_xml(State(state): State<AppState>) -> Response {
    let static_cdn_endpoint = runtime_static_cdn_endpoint(&state).await;
    serve_static_text_file_with_cdn(
        &state.config.static_dir,
        "browserconfig.xml",
        "application/xml; charset=utf-8",
        static_cdn_endpoint.as_deref(),
    )
}

pub async fn service_worker(State(state): State<AppState>, headers: HeaderMap) -> Response {
    let tf = load_time_freeze_config_for_request(&state.config, &headers);
    let (frozen, debug_header) = serve_frozen_file(&tf, |snap| {
        Some((
            snap.sw_js.as_slice(),
            "application/javascript; charset=utf-8",
        ))
    });
    if let Some(resp) = frozen {
        return resp;
    }
    let mut result = serve_static_text_file(
        &state.config.static_dir,
        "sw.js",
        "application/javascript; charset=utf-8",
    );
    set_time_freeze_header(&mut result, debug_header.as_deref());
    result
}

pub async fn service_worker_map(State(state): State<AppState>) -> Response {
    serve_static_text_file(&state.config.static_dir, "sw.js.map", "application/json")
}

fn set_time_freeze_header(response: &mut Response, value: Option<&str>) {
    #[cfg(feature = "time-freeze")]
    {
        if let Some(v) = value
            && let Ok(hv) = HeaderValue::from_str(v)
        {
            response
                .headers_mut()
                .insert(axum::http::HeaderName::from_static("x-time-freeze"), hv);
        }
    }

    #[cfg(not(feature = "time-freeze"))]
    let _ = (response, value);
}

async fn runtime_static_cdn_endpoint(state: &AppState) -> Option<String> {
    if let Some(discovery) = state.discovery_cache.get().await
        && let Some(endpoint) = discovery_endpoint(&discovery, "static_cdn")
    {
        return Some(endpoint);
    }

    state.config.static_cdn_endpoint.clone()
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

fn serve_static_text_file(static_dir: &str, filename: &str, content_type: &str) -> Response {
    serve_static_text_file_with_cdn(static_dir, filename, content_type, None)
}

fn serve_static_text_file_with_cdn(
    static_dir: &str,
    filename: &str,
    content_type: &str,
    static_cdn_endpoint: Option<&str>,
) -> Response {
    let file_path = Path::new(static_dir).join(filename);

    let resolved = match file_path.canonicalize() {
        Ok(p) => p,
        Err(_) => return StatusCode::NOT_FOUND.into_response(),
    };
    let base = match Path::new(static_dir).canonicalize() {
        Ok(p) => p,
        Err(_) => return StatusCode::NOT_FOUND.into_response(),
    };
    if !resolved.starts_with(&base) {
        return StatusCode::NOT_FOUND.into_response();
    }

    let content = match std::fs::read(&resolved) {
        Ok(bytes) => bytes,
        Err(_) => return StatusCode::NOT_FOUND.into_response(),
    };

    let replacement = static_cdn_endpoint.unwrap_or("").trim_end_matches('/');
    let body: axum::body::Body = match std::str::from_utf8(&content) {
        Ok(text) => text
            .replace("{{STATIC_CDN_ENDPOINT}}", replacement)
            .into_bytes()
            .into(),
        Err(_) => content.into(),
    };

    let mut response = body.into_response();
    if let Ok(ct) = HeaderValue::from_str(content_type) {
        response.headers_mut().insert(header::CONTENT_TYPE, ct);
    }
    response
        .headers_mut()
        .insert(header::CACHE_CONTROL, HeaderValue::from_static("no-cache"));
    response
}

pub fn guess_mime(path: &str) -> &'static str {
    let ext = match path.rfind('.') {
        Some(i) => &path[i..],
        None => return "application/octet-stream",
    };
    match ext.to_ascii_lowercase().as_str() {
        ".html" | ".htm" => "text/html; charset=utf-8",
        ".js" | ".mjs" => "application/javascript; charset=utf-8",
        ".css" => "text/css; charset=utf-8",
        ".json" => "application/json; charset=utf-8",
        ".png" => "image/png",
        ".jpg" | ".jpeg" => "image/jpeg",
        ".gif" => "image/gif",
        ".webp" => "image/webp",
        ".avif" => "image/avif",
        ".svg" => "image/svg+xml",
        ".ico" => "image/x-icon",
        ".woff" => "font/woff",
        ".woff2" => "font/woff2",
        ".ttf" => "font/ttf",
        ".otf" => "font/otf",
        ".eot" => "application/vnd.ms-fontobject",
        ".mp3" => "audio/mpeg",
        ".mp4" => "video/mp4",
        ".webm" => "video/webm",
        ".ogg" => "audio/ogg",
        ".wav" => "audio/wav",
        ".pdf" => "application/pdf",
        ".txt" => "text/plain; charset=utf-8",
        ".xml" => "application/xml; charset=utf-8",
        ".webmanifest" => "application/manifest+json",
        ".map" => "application/json",
        ".wasm" => "application/wasm",
        _ => "application/octet-stream",
    }
}

pub const CORS_ALLOW_ANY_VALUE: &str = "*";

pub fn is_font_mime(mime_type: &str) -> bool {
    matches!(
        mime_type,
        "font/woff" | "font/woff2" | "font/ttf" | "font/otf" | "application/vnd.ms-fontobject"
    )
}

pub fn is_static_asset(path: &str) -> bool {
    let filename = path.rsplit('/').next().unwrap_or(path);
    filename.contains('.')
}

pub fn is_hashed_asset(path: &str) -> bool {
    let filename = path.rsplit('/').next().unwrap_or(path);
    let Some(last_dot) = filename.rfind('.') else {
        return false;
    };
    let stem = &filename[..last_dot];
    if is_content_hash(stem) {
        return true;
    }
    ['.', '-'].iter().any(|sep| {
        stem.rfind(*sep)
            .is_some_and(|sep_pos| is_content_hash(&stem[sep_pos + 1..]))
    })
}

fn is_content_hash(value: &str) -> bool {
    value.len() >= 8 && value.chars().all(|c| c.is_ascii_hexdigit())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mime_html() {
        assert_eq!(guess_mime("i.html"), "text/html; charset=utf-8");
    }
    #[test]
    fn mime_js() {
        assert_eq!(guess_mime("a.js"), "application/javascript; charset=utf-8");
    }
    #[test]
    fn mime_css() {
        assert_eq!(guess_mime("s.css"), "text/css; charset=utf-8");
    }
    #[test]
    fn mime_json() {
        assert_eq!(guess_mime("d.json"), "application/json; charset=utf-8");
    }
    #[test]
    fn mime_wasm() {
        assert_eq!(guess_mime("m.wasm"), "application/wasm");
    }
    #[test]
    fn mime_svg() {
        assert_eq!(guess_mime("i.svg"), "image/svg+xml");
    }
    #[test]
    fn mime_png() {
        assert_eq!(guess_mime("p.png"), "image/png");
    }
    #[test]
    fn mime_jpg() {
        assert_eq!(guess_mime("p.jpg"), "image/jpeg");
    }
    #[test]
    fn mime_webp() {
        assert_eq!(guess_mime("p.webp"), "image/webp");
    }
    #[test]
    fn mime_avif() {
        assert_eq!(guess_mime("p.avif"), "image/avif");
    }
    #[test]
    fn mime_ico() {
        assert_eq!(guess_mime("f.ico"), "image/x-icon");
    }
    #[test]
    fn mime_woff2() {
        assert_eq!(guess_mime("f.woff2"), "font/woff2");
    }
    #[test]
    fn mime_mp4() {
        assert_eq!(guess_mime("c.mp4"), "video/mp4");
    }

    #[test]
    fn mime_unknown() {
        assert_eq!(guess_mime("f.xyz"), "application/octet-stream");
    }

    #[test]
    fn mime_no_ext() {
        assert_eq!(guess_mime("LICENSE"), "application/octet-stream");
    }

    #[test]
    fn mime_case_insensitive() {
        assert_eq!(guess_mime("F.HTML"), "text/html; charset=utf-8");
        assert_eq!(guess_mime("F.JS"), "application/javascript; charset=utf-8");
    }

    #[test]
    fn static_asset_with_ext() {
        assert!(is_static_asset("/assets/app.js"));
        assert!(is_static_asset("style.css"));
    }

    #[test]
    fn static_asset_without_ext() {
        assert!(!is_static_asset("/channels/me"));
        assert!(!is_static_asset("/login"));
    }

    #[test]
    fn hashed_asset_positive() {
        assert!(is_hashed_asset("app.a1b2c3d4.js"));
        assert!(is_hashed_asset("style-abcdef01.css"));
    }

    #[test]
    fn hashed_asset_accepts_bare_contenthash_filenames() {
        assert!(is_hashed_asset("assets/469e0b8f10c496a1.css"));
        assert!(is_hashed_asset("assets/a79f1c3119cd700d.woff2"));
        assert!(is_hashed_asset("/assets/488b87159423ca35.js"));
    }

    #[test]
    fn hashed_asset_negative() {
        assert!(!is_hashed_asset("app.js"));
        assert!(!is_hashed_asset("style.css"));
        assert!(!is_hashed_asset("a79f1c3119cd700d/app.js"));
    }
}
