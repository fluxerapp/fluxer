// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::range::ByteRange;
#[cfg(test)]
pub use fluxer_common::server::STRICT_TRANSPORT_SECURITY;
use http::{HeaderMap, HeaderValue, StatusCode, header};

pub const ROBOTS: &str = "noindex, nofollow, nosnippet, noimageindex, notranslate, max-snippet:0, max-image-preview:none, max-video-preview:0";
pub const MEDIA_CSP: &str = "default-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'; object-src 'none'; script-src 'none'; script-src-attr 'none'; script-src-elem 'none'; style-src 'unsafe-inline'; img-src 'self' blob: data:; media-src 'self' blob:; sandbox allow-same-origin";
pub const PERMISSIONS_POLICY: &str = "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()";

pub fn parse_content_length(headers: &HeaderMap) -> Option<u64> {
    let content_lengths = headers.get_all(header::CONTENT_LENGTH);
    let mut values = content_lengths.iter();
    let raw = values.next()?.to_str().ok()?;
    if values.next().is_some() || raw.is_empty() || !raw.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    raw.parse().ok()
}

pub fn add_security_headers(headers: &mut HeaderMap) {
    fluxer_common::server::set_baseline_security_headers(headers, PERMISSIONS_POLICY);
    headers
        .entry(header::CONTENT_SECURITY_POLICY)
        .or_insert(HeaderValue::from_static(MEDIA_CSP));
}

pub fn neutralize_script_content_type(status: StatusCode, headers: &mut HeaderMap) {
    let values = headers.get_all(header::CONTENT_TYPE);
    let scriptable = values.iter().any(|value| {
        value
            .to_str()
            .map_or(true, crate::mime::is_javascript_content_type)
    });
    let untyped = values.iter().all(|value| {
        value
            .as_bytes()
            .iter()
            .all(|byte| matches!(byte, b' ' | b'\t'))
    });
    let serves_body = matches!(status, StatusCode::OK | StatusCode::PARTIAL_CONTENT);
    if scriptable || (untyped && serves_body) {
        headers.insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static(crate::mime::INERT_CONTENT_TYPE),
        );
    }
}

pub fn add_media_headers(
    headers: &mut HeaderMap,
    size: usize,
    content_type: &str,
    byte_range: Option<ByteRange>,
) {
    add_security_headers(headers);
    let streamable = content_type.starts_with("video/") || content_type.starts_with("audio/");
    headers.insert(header::ACCEPT_RANGES, HeaderValue::from_static("bytes"));
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_ORIGIN,
        HeaderValue::from_static("*"),
    );
    headers.insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static(if streamable {
            "public, max-age=31536000, no-transform"
        } else {
            "public, max-age=31536000"
        }),
    );
    headers.insert(
        "CDN-Cache-Control",
        HeaderValue::from_static("public, max-age=31536000"),
    );
    headers.insert(
        header::CONTENT_SECURITY_POLICY,
        HeaderValue::from_static(MEDIA_CSP),
    );
    headers.insert(
        header::CONTENT_TYPE,
        HeaderValue::from_str(content_type)
            .unwrap_or_else(|_| HeaderValue::from_static("application/octet-stream")),
    );
    headers.insert(header::VARY, HeaderValue::from_static("Accept-Encoding"));
    headers.insert(
        header::X_CONTENT_TYPE_OPTIONS,
        HeaderValue::from_static("nosniff"),
    );
    headers.insert("X-Robots-Tag", HeaderValue::from_static(ROBOTS));
    if let Some(r) = byte_range {
        headers.insert(
            header::CONTENT_RANGE,
            HeaderValue::from_str(&format!("bytes {}-{}/{}", r.start, r.end, size))
                .expect("content-range is ASCII"),
        );
    }
}

pub fn add_unsatisfiable_headers(headers: &mut HeaderMap, size: usize) {
    add_security_headers(headers);
    headers.insert(header::ACCEPT_RANGES, HeaderValue::from_static("bytes"));
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_ORIGIN,
        HeaderValue::from_static("*"),
    );
    headers.insert(
        header::CONTENT_RANGE,
        HeaderValue::from_str(&format!("bytes */{size}")).expect("content-range is ASCII"),
    );
    headers.insert(header::VARY, HeaderValue::from_static("Accept-Encoding"));
    headers.insert("X-Robots-Tag", HeaderValue::from_static(ROBOTS));
}
