// SPDX-License-Identifier: AGPL-3.0-or-later

use base64::prelude::*;
use thiserror::Error;

const OPAQUE_PREFIX: &str = "v2/";

#[derive(Debug, Error, Eq, PartialEq)]
pub enum ExternalPathError {
    #[error("invalid external path")]
    InvalidExternalPath,
    #[error("invalid base64")]
    InvalidBase64,
    #[error("invalid utf-8")]
    InvalidUtf8,
}

pub fn build_opaque_external_media_proxy_path(input_url: &str) -> String {
    format!(
        "{OPAQUE_PREFIX}{}",
        BASE64_URL_SAFE_NO_PAD.encode(input_url.as_bytes())
    )
}

fn percent_encode_component(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for byte in value.bytes() {
        match byte {
            b'A'..=b'Z'
            | b'a'..=b'z'
            | b'0'..=b'9'
            | b'-'
            | b'_'
            | b'.'
            | b'!'
            | b'~'
            | b'*'
            | b'\''
            | b'('
            | b')' => out.push(byte as char),
            _ => out.push_str(&format!("%{byte:02X}")),
        }
    }
    out
}

pub fn build_external_media_proxy_path(input_url: &str) -> Result<String, ExternalPathError> {
    let (scheme, remainder) = input_url
        .split_once("://")
        .ok_or(ExternalPathError::InvalidExternalPath)?;
    if scheme.is_empty() || remainder.is_empty() {
        return Err(ExternalPathError::InvalidExternalPath);
    }
    let fetchable = match remainder.split_once('#') {
        Some((head, _)) => head,
        None => remainder,
    };
    let (authority_and_path, query) = match fetchable.split_once('?') {
        Some((head, tail)) => (head, (!tail.is_empty()).then_some(tail)),
        None => (fetchable, None),
    };
    let (authority, path) = match authority_and_path.split_once('/') {
        Some((authority, path)) => (authority, path),
        None => (authority_and_path, ""),
    };
    let host = match authority.rsplit_once('@') {
        Some((_, after_credentials)) => after_credentials,
        None => authority,
    };
    if host.is_empty() {
        return Err(ExternalPathError::InvalidExternalPath);
    }
    let mut segments: Vec<String> = Vec::new();
    if let Some(query) = query {
        segments.push(percent_encode_component(&format!("?{query}")));
    }
    segments.push(scheme.to_owned());
    segments.push(host.to_owned());
    if !path.is_empty() {
        segments.push(
            path.split('/')
                .map(percent_encode_component)
                .collect::<Vec<_>>()
                .join("/"),
        );
    }
    Ok(segments.join("/"))
}

fn decode_opaque(proxy_path: &str) -> Result<String, ExternalPathError> {
    let encoded = proxy_path
        .strip_prefix(OPAQUE_PREFIX)
        .ok_or(ExternalPathError::InvalidExternalPath)?;
    if encoded.is_empty() {
        return Err(ExternalPathError::InvalidExternalPath);
    }
    let bytes = BASE64_URL_SAFE_NO_PAD
        .decode(encoded)
        .map_err(|_| ExternalPathError::InvalidBase64)?;
    String::from_utf8(bytes).map_err(|_| ExternalPathError::InvalidUtf8)
}

fn hex_val(c: u8) -> Option<u8> {
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'a'..=b'f' => Some(c - b'a' + 10),
        b'A'..=b'F' => Some(c - b'A' + 10),
        _ => None,
    }
}

pub fn percent_decode(input: &str, plus_as_space: bool) -> Vec<u8> {
    let bytes = input.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        let ch = bytes[i];
        if ch == b'%'
            && i + 2 < bytes.len()
            && let (Some(hi), Some(lo)) = (hex_val(bytes[i + 1]), hex_val(bytes[i + 2]))
        {
            out.push((hi << 4) | lo);
            i += 3;
            continue;
        }
        out.push(if plus_as_space && ch == b'+' {
            b' '
        } else {
            ch
        });
        i += 1;
    }
    out
}

pub fn percent_decode_string(input: &str, plus_as_space: bool) -> String {
    String::from_utf8_lossy(&percent_decode(input, plus_as_space)).into_owned()
}

fn segmented_protocol_index(parts: &[&str]) -> Option<usize> {
    for (index, part) in parts.iter().enumerate() {
        if part.is_empty() {
            continue;
        }
        if index == 0 && part.contains("%3D") {
            continue;
        }
        let mut chars = part.bytes();
        let first = chars.next()?;
        if !first.is_ascii_alphabetic() {
            continue;
        }
        if chars.all(|c| c.is_ascii_alphanumeric() || matches!(c, b'+' | b'.' | b'-')) {
            return Some(index);
        }
    }
    None
}

fn reconstruct_segmented(proxy_path: &str) -> Result<String, ExternalPathError> {
    let parts: Vec<&str> = proxy_path.split('/').collect();
    let protocol_index =
        segmented_protocol_index(&parts).ok_or(ExternalPathError::InvalidExternalPath)?;
    if protocol_index + 1 >= parts.len() {
        return Err(ExternalPathError::InvalidExternalPath);
    }
    let protocol = parts[protocol_index];
    let host_port = percent_decode_string(parts[protocol_index + 1], false);
    let query_raw = parts[..protocol_index].join("/");
    let path_raw = parts[protocol_index + 2..].join("/");
    let query = percent_decode_string(&query_raw, false);
    let path = percent_decode_string(&path_raw, false);
    let normalized_query = query.strip_prefix('?').unwrap_or(&query);
    Ok(format!(
        "{protocol}://{host_port}/{path}{}{normalized_query}",
        if normalized_query.is_empty() { "" } else { "?" }
    ))
}

pub fn reconstruct_original_url(proxy_path: &str) -> Result<String, ExternalPathError> {
    if proxy_path.starts_with(OPAQUE_PREFIX) {
        decode_opaque(proxy_path)
    } else {
        reconstruct_segmented(proxy_path)
    }
}

type HmacSha256 = hmac::Hmac<sha2::Sha256>;

pub fn create_signature(input: &str, secret: &[u8]) -> String {
    use hmac::{KeyInit, Mac};
    let mut mac = HmacSha256::new_from_slice(secret).expect("HMAC accepts any key length");
    mac.update(input.as_bytes());
    BASE64_URL_SAFE_NO_PAD.encode(mac.finalize().into_bytes())
}

pub fn build_external_media_proxy_url(
    public_endpoint: &str,
    input_url: &str,
    secret: &[u8],
) -> Option<String> {
    let path = build_external_media_proxy_path(input_url).ok()?;
    let signature = create_signature(&path, secret);
    Some(format!("{public_endpoint}/external/{signature}/{path}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_the_plain_path_shape() {
        assert_eq!(
            "https/static.klipy.com/ii/c8/28/HkAKKCzZ.webp",
            build_external_media_proxy_path("https://static.klipy.com/ii/c8/28/HkAKKCzZ.webp")
                .unwrap()
        );
    }

    #[test]
    fn builds_an_encoded_query_ahead_of_the_scheme() {
        assert_eq!(
            "%3Fv%3Dquery_param%26goes%3Dhere/https/static.klipy.com/ii/HkAKKCzZ.webp",
            build_external_media_proxy_path(
                "https://static.klipy.com/ii/HkAKKCzZ.webp?v=query_param&goes=here"
            )
            .unwrap()
        );
    }

    #[test]
    fn leaves_the_fragment_out_of_the_signed_path() {
        assert_eq!(
            build_external_media_proxy_path("https://example.com/a.gif").unwrap(),
            build_external_media_proxy_path("https://example.com/a.gif#anchor").unwrap()
        );
        assert_eq!(
            build_external_media_proxy_path("https://example.com/a.gif?v=1").unwrap(),
            build_external_media_proxy_path("https://example.com/a.gif?v=1#anchor").unwrap()
        );
    }

    #[test]
    fn leaves_credentials_out_of_the_host_segment() {
        assert_eq!(
            "https/cdn.example.com/a.gif",
            build_external_media_proxy_path("https://reader:secret@cdn.example.com/a.gif").unwrap()
        );
        assert_eq!(
            "https/cdn.example.com/a.gif",
            build_external_media_proxy_path("https://reader@cdn.example.com/a.gif").unwrap()
        );
    }

    #[test]
    fn leaves_out_a_query_that_reconstructs_to_nothing() {
        assert_eq!(
            build_external_media_proxy_path("https://example.com/a.gif").unwrap(),
            build_external_media_proxy_path("https://example.com/a.gif?").unwrap()
        );
    }

    #[test]
    fn matches_the_typescript_builder_on_the_shared_vectors() {
        let raw = include_str!("testdata/external_media_proxy_path_vectors.json");
        let vectors: serde_json::Value = serde_json::from_str(raw).expect("vectors parse as json");
        let vectors = vectors.as_array().expect("vectors are an array");
        assert!(!vectors.is_empty());
        for vector in vectors {
            let original = vector["url"].as_str().expect("vector carries a url");
            let expected = vector["path"].as_str().expect("vector carries a path");
            let normalized = url::Url::parse(original).expect("vector url parses");
            assert_eq!(
                expected,
                build_external_media_proxy_path(normalized.as_str()).expect("path builds"),
                "vector {original}"
            );
        }
    }

    #[test]
    fn keeps_a_non_default_port() {
        assert_eq!(
            "https/example.com:8443/a.png",
            build_external_media_proxy_path("https://example.com:8443/a.png").unwrap()
        );
    }

    #[test]
    fn round_trips_every_shape() {
        for url in [
            "https://static.klipy.com/ii/c8/28/HkAKKCzZ.webp",
            "https://static.klipy.com/ii/HkAKKCzZ.webp?v=query_param&goes=here",
            "https://example.com:8443/a.png",
            "https://avatars.githubusercontent.com/u/241303489?v=4",
            "http://example.com/plain.gif",
            "https://example.com/file.png?a=1&b=2&c=3",
        ] {
            let path = build_external_media_proxy_path(url).unwrap();
            assert_eq!(
                url,
                reconstruct_original_url(&path).unwrap(),
                "round trip {url}"
            );
        }
    }

    #[test]
    fn does_not_double_the_question_mark() {
        let decoded = reconstruct_original_url("%3Fa%3D1/https/example.com/x.png").unwrap();
        assert_eq!("https://example.com/x.png?a=1", decoded);
        assert!(!decoded.contains("??"));
    }

    #[test]
    fn still_accepts_a_query_without_a_leading_question_mark() {
        assert_eq!(
            "https://example.com/x.png?a=1",
            reconstruct_original_url("a%3D1/https/example.com/x.png").unwrap()
        );
    }

    #[test]
    fn rejects_a_url_without_a_scheme() {
        assert!(build_external_media_proxy_path("example.com/a.png").is_err());
    }

    #[test]
    fn v2_path_roundtrip() {
        let path = build_opaque_external_media_proxy_path("https://example.com/a b.png?x=1");
        let decoded = reconstruct_original_url(&path).unwrap();
        assert_eq!("https://example.com/a b.png?x=1", decoded);
    }
}
