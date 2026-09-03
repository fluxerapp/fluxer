// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::range::ByteRange;
use http::{HeaderMap, HeaderName, HeaderValue, header};

pub const ROBOTS: &str = "noindex, nofollow, nosnippet, noimageindex, notranslate, max-snippet:0, max-image-preview:none, max-video-preview:0";
pub const MEDIA_CSP: &str = "default-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors 'none'; object-src 'none'; script-src 'none'; script-src-attr 'none'; script-src-elem 'none'; style-src 'unsafe-inline'; img-src 'self' blob: data:; media-src 'self' blob:; sandbox allow-same-origin";
pub const STRICT_TRANSPORT_SECURITY: &str = "max-age=31536000; includeSubDomains; preload";
pub const REFERRER_POLICY: &str = "strict-origin-when-cross-origin";
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
    set_static_header(
        headers,
        HeaderName::from_static("strict-transport-security"),
        STRICT_TRANSPORT_SECURITY,
    );
    set_static_header(headers, header::X_CONTENT_TYPE_OPTIONS, "nosniff");
    set_static_header(
        headers,
        HeaderName::from_static("referrer-policy"),
        REFERRER_POLICY,
    );
    set_static_header(headers, HeaderName::from_static("x-frame-options"), "DENY");
    set_static_header(
        headers,
        HeaderName::from_static("permissions-policy"),
        PERMISSIONS_POLICY,
    );
    set_static_header(headers, header::CONTENT_SECURITY_POLICY, MEDIA_CSP);
}

fn set_static_header(headers: &mut HeaderMap, name: HeaderName, value: &'static str) {
    headers
        .entry(name)
        .or_insert(HeaderValue::from_static(value));
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

#[cfg(test)]
mod tests {
    use super::*;

    fn header_names(headers: &HeaderMap) -> Vec<String> {
        let mut names: Vec<String> = headers
            .keys()
            .map(|name| name.as_str().to_owned())
            .collect();
        names.sort();
        names
    }

    fn value(headers: &HeaderMap, name: &str) -> String {
        headers
            .get(name)
            .unwrap_or_else(|| panic!("missing {name}"))
            .to_str()
            .expect("header value is ASCII")
            .to_owned()
    }

    const MEDIA_HEADER_NAMES: [&str; 13] = [
        "accept-ranges",
        "access-control-allow-origin",
        "cache-control",
        "cdn-cache-control",
        "content-security-policy",
        "content-type",
        "permissions-policy",
        "referrer-policy",
        "strict-transport-security",
        "vary",
        "x-content-type-options",
        "x-frame-options",
        "x-robots-tag",
    ];

    #[test]
    fn media_headers_distinguish_streamable_content_and_ranges() {
        let mut image_headers = HeaderMap::new();
        add_media_headers(&mut image_headers, 100, "image/png", None);
        assert_eq!(header_names(&image_headers), MEDIA_HEADER_NAMES.to_vec());
        assert_eq!(
            value(&image_headers, "cache-control"),
            "public, max-age=31536000"
        );
        assert_eq!(value(&image_headers, "content-type"), "image/png");
        assert!(image_headers.get(header::CONTENT_RANGE).is_none());

        let mut video_headers = HeaderMap::new();
        add_media_headers(
            &mut video_headers,
            100,
            "video/mp4",
            Some(ByteRange { start: 10, end: 19 }),
        );
        let mut ranged_names = MEDIA_HEADER_NAMES.to_vec();
        ranged_names.push("content-range");
        ranged_names.sort();
        assert_eq!(header_names(&video_headers), ranged_names);
        assert_eq!(
            value(&video_headers, "cache-control"),
            "public, max-age=31536000, no-transform"
        );
        assert_eq!(value(&video_headers, "content-range"), "bytes 10-19/100");

        let mut audio_headers = HeaderMap::new();
        add_media_headers(&mut audio_headers, 100, "audio/mpeg", None);
        assert_eq!(
            value(&audio_headers, "cache-control"),
            "public, max-age=31536000, no-transform"
        );
    }

    #[test]
    fn media_headers_carry_the_frozen_policy_values_and_no_entity_tag() {
        let mut headers = HeaderMap::new();
        add_media_headers(&mut headers, 100, "image/png", None);
        assert_eq!(value(&headers, "accept-ranges"), "bytes");
        assert_eq!(value(&headers, "access-control-allow-origin"), "*");
        assert_eq!(
            value(&headers, "cdn-cache-control"),
            "public, max-age=31536000"
        );
        assert_eq!(value(&headers, "content-security-policy"), MEDIA_CSP);
        assert!(headers.get(header::EXPIRES).is_none());
        assert!(headers.get(header::LAST_MODIFIED).is_none());
        assert_eq!(
            value(&headers, "strict-transport-security"),
            STRICT_TRANSPORT_SECURITY
        );
        assert_eq!(value(&headers, "referrer-policy"), REFERRER_POLICY);
        assert_eq!(value(&headers, "permissions-policy"), PERMISSIONS_POLICY);
        assert_eq!(value(&headers, "x-frame-options"), "DENY");
        assert_eq!(value(&headers, "x-content-type-options"), "nosniff");
        assert_eq!(value(&headers, "x-robots-tag"), ROBOTS);
        assert_eq!(value(&headers, "vary"), "Accept-Encoding");
        assert!(headers.get(header::ETAG).is_none());
    }

    #[test]
    fn media_headers_overwrite_inherited_security_values_and_fall_back_on_invalid_content_types() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::CONTENT_SECURITY_POLICY,
            HeaderValue::from_static("default-src *"),
        );
        headers.insert(
            header::X_CONTENT_TYPE_OPTIONS,
            HeaderValue::from_static("sniff-everything"),
        );
        add_media_headers(&mut headers, 100, "image/\u{7f}png", None);
        assert_eq!(value(&headers, "content-security-policy"), MEDIA_CSP);
        assert_eq!(value(&headers, "x-content-type-options"), "nosniff");
        assert_eq!(value(&headers, "content-type"), "application/octet-stream");
        assert_eq!(
            headers
                .get_all(header::CONTENT_SECURITY_POLICY)
                .iter()
                .count(),
            1
        );
    }

    #[test]
    fn shorter_cache_policies_replace_the_stored_media_policy_completely() {
        for policy in [
            "public, max-age=300",
            "public, max-age=86400",
            "no-store",
            "private, no-store",
        ] {
            let mut headers = HeaderMap::new();
            add_media_headers(&mut headers, 100, "image/png", None);
            headers.insert(header::CACHE_CONTROL, HeaderValue::from_static(policy));
            headers.insert("CDN-Cache-Control", HeaderValue::from_static(policy));
            assert_eq!(headers.get_all(header::CACHE_CONTROL).iter().count(), 1);
            assert_eq!(value(&headers, "cache-control"), policy);
            assert_eq!(value(&headers, "cdn-cache-control"), policy);
        }
    }

    #[test]
    fn unsatisfiable_headers_state_the_full_size_without_a_body_representation() {
        let mut headers = HeaderMap::new();
        add_unsatisfiable_headers(&mut headers, 4096);
        assert_eq!(
            header_names(&headers),
            vec![
                "accept-ranges",
                "access-control-allow-origin",
                "content-range",
                "content-security-policy",
                "permissions-policy",
                "referrer-policy",
                "strict-transport-security",
                "vary",
                "x-content-type-options",
                "x-frame-options",
                "x-robots-tag",
            ]
        );
        assert_eq!(value(&headers, "content-range"), "bytes */4096");
        assert_eq!(value(&headers, "vary"), "Accept-Encoding");
        assert_eq!(value(&headers, "x-robots-tag"), ROBOTS);
        assert!(headers.get(header::CONTENT_TYPE).is_none());
        assert!(headers.get(header::CACHE_CONTROL).is_none());
        assert!(headers.get("CDN-Cache-Control").is_none());
    }

    #[test]
    fn content_length_parser_requires_one_canonical_decimal_value() {
        let mut headers = HeaderMap::new();
        assert_eq!(parse_content_length(&headers), None);
        headers.insert(header::CONTENT_LENGTH, HeaderValue::from_static("123"));
        assert_eq!(parse_content_length(&headers), Some(123));
        headers.append(header::CONTENT_LENGTH, HeaderValue::from_static("123"));
        assert_eq!(parse_content_length(&headers), None);
        headers.clear();
        headers.insert(header::CONTENT_LENGTH, HeaderValue::from_static("+1"));
        assert_eq!(parse_content_length(&headers), None);
        headers.insert(
            header::CONTENT_LENGTH,
            HeaderValue::from_static("18446744073709551616"),
        );
        assert_eq!(parse_content_length(&headers), None);
        for raw in ["", " 1", "1 ", "0x10", "1_0", "12.0", "-1", "1,1"] {
            headers.clear();
            let header_value = HeaderValue::from_str(raw).expect("visible ascii header value");
            headers.insert(header::CONTENT_LENGTH, header_value);
            assert_eq!(parse_content_length(&headers), None, "accepted {raw:?}");
        }
        headers.clear();
        headers.insert(
            header::CONTENT_LENGTH,
            HeaderValue::from_bytes(&[0xC3, 0x28]).expect("opaque header value"),
        );
        assert_eq!(parse_content_length(&headers), None);
        headers.clear();
        headers.insert(
            header::CONTENT_LENGTH,
            HeaderValue::from_static("18446744073709551615"),
        );
        assert_eq!(parse_content_length(&headers), Some(u64::MAX));
        headers.clear();
        headers.insert(header::CONTENT_LENGTH, HeaderValue::from_static("00"));
        assert_eq!(parse_content_length(&headers), Some(0));
    }

    #[test]
    fn media_headers_always_cache_forever() {
        let mut headers = HeaderMap::new();
        add_media_headers(&mut headers, 10, "image/png", None);
        assert_eq!(
            "public, max-age=31536000",
            headers.get(header::CACHE_CONTROL).unwrap()
        );

        let mut streamable = HeaderMap::new();
        add_media_headers(&mut streamable, 10, "video/mp4", None);
        assert_eq!(
            "public, max-age=31536000, no-transform",
            streamable.get(header::CACHE_CONTROL).unwrap()
        );

        let mut audio = HeaderMap::new();
        add_media_headers(&mut audio, 10, "audio/ogg", None);
        assert_eq!(
            "public, max-age=31536000, no-transform",
            audio.get(header::CACHE_CONTROL).unwrap()
        );
    }

    #[test]
    fn media_headers_pair_the_policy_with_cdn_cache_control_and_omit_expires() {
        let mut headers = HeaderMap::new();
        add_media_headers(&mut headers, 10, "image/png", None);
        assert_eq!(
            "public, max-age=31536000",
            headers.get("CDN-Cache-Control").unwrap()
        );
        assert!(
            headers.get(header::EXPIRES).is_none(),
            "Cache-Control is authoritative; a fixed Expires date only contradicts it"
        );
    }
}
