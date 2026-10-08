// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::http::{HeaderMap, HeaderName, HeaderValue, StatusCode, header};

pub(crate) const CORS_ALLOW_ANY_VALUE: &str = "*";
pub(crate) const LONG_LIVED_ASSET_CACHE_CONTROL: &str = "public, max-age=31536000, immutable";
pub(crate) const REVALIDATED_ASSET_CACHE_CONTROL: &str = "public, max-age=3600, must-revalidate";
pub(crate) const UPSTREAM_FAILURE_CACHE_CONTROL: &str = "no-store";

const FORWARDED_ASSET_REQUEST_HEADERS: &[&str] = &[
    "accept",
    "accept-encoding",
    "accept-language",
    "cache-control",
    "if-match",
    "if-modified-since",
    "if-none-match",
    "if-range",
    "if-unmodified-since",
    "range",
];

const FORWARDED_ASSET_RESPONSE_HEADERS: &[&str] = &[
    "accept-ranges",
    "access-control-allow-origin",
    "access-control-expose-headers",
    "content-disposition",
    "content-encoding",
    "content-language",
    "content-length",
    "content-range",
    "content-type",
    "cross-origin-resource-policy",
    "etag",
    "last-modified",
    "timing-allow-origin",
    "vary",
];

pub(crate) fn apply_asset_request_headers(
    mut target: reqwest::RequestBuilder,
    source: &HeaderMap,
) -> reqwest::RequestBuilder {
    for name in FORWARDED_ASSET_REQUEST_HEADERS {
        let header_name = HeaderName::from_static(name);
        for value in source.get_all(&header_name) {
            target = target.header(header_name.clone(), value.clone());
        }
    }
    target
}

pub(crate) fn copy_asset_response_headers(source: &HeaderMap, target: &mut HeaderMap) {
    for name in FORWARDED_ASSET_RESPONSE_HEADERS {
        let header_name = HeaderName::from_static(name);
        for value in source.get_all(&header_name) {
            target.append(header_name.clone(), value.clone());
        }
    }
}

pub(crate) fn apply_asset_response_policy(
    headers: &mut HeaderMap,
    path: &str,
    status: StatusCode,
    csp: HeaderValue,
) {
    set_known_asset_content_type(headers, path);
    set_font_cors(headers);
    set_vary_on_accept_encoding(headers);
    weaken_entity_tag(headers);
    let cache_control = if status.is_success() || status == StatusCode::NOT_MODIFIED {
        asset_cache_control(path)
    } else {
        UPSTREAM_FAILURE_CACHE_CONTROL
    };
    headers.insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static(cache_control),
    );
    apply_static_resource_security_policy(headers, csp);
}

fn weaken_entity_tag(headers: &mut HeaderMap) {
    let Some(value) = headers.get(header::ETAG) else {
        return;
    };
    let Some(value) = value.to_str().ok().map(str::trim) else {
        headers.remove(header::ETAG);
        return;
    };
    let (already_weak, opaque) = match value.strip_prefix("W/") {
        Some(opaque) => (true, opaque),
        None => (false, value),
    };
    if opaque.len() < 2
        || !opaque.starts_with('"')
        || !opaque.ends_with('"')
        || opaque[1..opaque.len() - 1].contains('"')
    {
        headers.remove(header::ETAG);
        return;
    }
    if already_weak {
        return;
    }
    let weak = HeaderValue::from_str(&format!("W/{opaque}"))
        .expect("a validated ASCII entity tag must remain a valid header when weakened");
    headers.insert(header::ETAG, weak);
}

pub(crate) fn set_vary_on_accept_encoding(headers: &mut HeaderMap) {
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

pub(crate) fn accepts_content_encoding(headers: &HeaderMap, encoding: &str) -> bool {
    let Some(accepted) = headers
        .get(header::ACCEPT_ENCODING)
        .and_then(|value| value.to_str().ok())
    else {
        return false;
    };
    accepted.split(',').any(|candidate| {
        let mut parameters = candidate.split(';').map(str::trim);
        let Some(name) = parameters.next() else {
            return false;
        };
        name.eq_ignore_ascii_case(encoding) && !parameters.any(is_zero_quality)
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

pub(crate) fn apply_static_resource_security_policy(headers: &mut HeaderMap, csp: HeaderValue) {
    headers.insert(header::CONTENT_SECURITY_POLICY, csp);
    headers.remove(header::CONTENT_SECURITY_POLICY_REPORT_ONLY);
}

pub(crate) fn guess_mime(path: &str) -> &'static str {
    let extension = match path.rfind('.') {
        Some(index) => &path[index..],
        None => return "application/octet-stream",
    };
    match extension.to_ascii_lowercase().as_str() {
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

pub(crate) fn is_font_mime(mime_type: &str) -> bool {
    matches!(
        mime_type,
        "font/woff" | "font/woff2" | "font/ttf" | "font/otf" | "application/vnd.ms-fontobject"
    )
}

pub(crate) fn is_hashed_asset(path: &str) -> bool {
    let filename = path.rsplit('/').next().unwrap_or(path);
    let Some(last_dot) = filename.rfind('.') else {
        return false;
    };
    let stem = &filename[..last_dot];
    if stem.split('.').next().is_some_and(is_content_hash) {
        return true;
    }
    ['.', '-'].iter().any(|separator| {
        stem.rfind(*separator)
            .is_some_and(|position| is_content_hash(&stem[position + 1..]))
    })
}

pub(crate) fn asset_cache_control(path: &str) -> &'static str {
    if is_hashed_asset(path) {
        LONG_LIVED_ASSET_CACHE_CONTROL
    } else {
        REVALIDATED_ASSET_CACHE_CONTROL
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
    headers.insert(header::CONTENT_TYPE, HeaderValue::from_static(mime_type));
}

pub(crate) fn is_content_hash(value: &str) -> bool {
    value.len() >= 8 && value.chars().all(|character| character.is_ascii_hexdigit())
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
    fn hashed_asset_accepts_the_contenthash_worker_bundle_name() {
        assert!(
            is_hashed_asset("assets/2d715e4730758083.worker.js"),
            "rspack emits workers as assets/[contenthash:16].worker.js"
        );
    }

    #[test]
    fn hashed_asset_negative() {
        assert!(!is_hashed_asset("app.js"));
        assert!(!is_hashed_asset("style.css"));
        assert!(!is_hashed_asset("a79f1c3119cd700d/app.js"));
    }

    #[test]
    fn the_bundled_font_licences_are_not_treated_as_content_hashed() {
        assert!(!is_hashed_asset("assets/fonts-NOTICE.txt"));
        assert!(!is_hashed_asset("assets/fonts-LICENSE-IBM-PLEX.txt"));
    }

    #[test]
    fn only_a_content_hashed_asset_is_promised_to_never_change() {
        assert_eq!(
            asset_cache_control("assets/469e0b8f10c496a1.css"),
            LONG_LIVED_ASSET_CACHE_CONTROL
        );
        assert_eq!(
            asset_cache_control("assets/fonts-NOTICE.txt"),
            REVALIDATED_ASSET_CACHE_CONTROL
        );
    }
}
