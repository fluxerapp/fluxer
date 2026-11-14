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
    #[error("invalid signature")]
    InvalidSignature,
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

fn is_scheme_segment(part: &str) -> bool {
    let mut bytes = part.bytes();
    bytes
        .next()
        .is_some_and(|first| first.is_ascii_alphabetic())
        && bytes.all(|c| c.is_ascii_alphanumeric() || matches!(c, b'+' | b'.' | b'-'))
}

fn segmented_protocol_index(parts: &[&str]) -> Option<usize> {
    parts.iter().enumerate().find_map(|(index, part)| {
        let legacy_query = index == 0 && part.contains("%3D");
        (!legacy_query && is_scheme_segment(part)).then_some(index)
    })
}

fn reconstruct_segmented(proxy_path: &str) -> Result<String, ExternalPathError> {
    let parts: Vec<&str> = proxy_path.split('/').collect();
    let protocol_index =
        segmented_protocol_index(&parts).ok_or(ExternalPathError::InvalidExternalPath)?;
    reconstruct_segmented_at(&parts, protocol_index)
}

fn reconstruct_segmented_at(
    parts: &[&str],
    protocol_index: usize,
) -> Result<String, ExternalPathError> {
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

use url::Url;

pub fn is_own_media_url(media_endpoint: &str, target: &Url) -> bool {
    let Ok(base) = Url::parse(media_endpoint) else {
        return false;
    };
    let same_host = match (base.host_str(), target.host_str()) {
        (Some(base_host), Some(target_host)) => base_host.eq_ignore_ascii_case(target_host),
        _ => false,
    };
    same_host
        && base.scheme().eq_ignore_ascii_case(target.scheme())
        && base.port_or_known_default() == target.port_or_known_default()
        && target
            .path()
            .strip_prefix(base.path().trim_end_matches('/'))
            .is_some_and(|rest| rest.is_empty() || rest.starts_with('/'))
}

pub fn verify_signature(input: &str, provided: &str, secret: &[u8]) -> bool {
    let expected = create_signature(input, secret);
    if expected.len() != provided.len() {
        return false;
    }
    expected
        .bytes()
        .zip(provided.bytes())
        .fold(0u8, |diff, (a, b)| diff | (a ^ b))
        == 0
}

const MAX_QUERY_SEGMENTS: usize = 32;

fn protocol_index_candidates(parts: &[&str]) -> Vec<usize> {
    let default = segmented_protocol_index(parts);
    let mut candidates: Vec<usize> = default.into_iter().collect();
    let starts_with_query = parts[0]
        .get(..3)
        .is_some_and(|prefix| prefix.eq_ignore_ascii_case("%3F"));
    if starts_with_query {
        let last = parts.len().saturating_sub(1).min(MAX_QUERY_SEGMENTS);
        candidates.extend(
            (1..last).filter(|&index| Some(index) != default && is_scheme_segment(parts[index])),
        );
    }
    candidates
}

fn verify_re_encoded_proxy_path(
    parts: &[&str],
    protocol_index: usize,
    signature: &str,
    secret: &[u8],
) -> Option<String> {
    let url = reconstruct_segmented_at(parts, protocol_index).ok()?;
    let canonical = build_external_media_proxy_path(&url).ok()?;
    if !verify_signature(&canonical, signature, secret) {
        return None;
    }
    reconstruct_original_url(&canonical)
        .ok()
        .filter(|signed_url| *signed_url == url)
}

pub fn verify_signed_proxy_path(
    proxy_path: &str,
    signature: &str,
    secret: &[u8],
) -> Result<String, ExternalPathError> {
    if verify_signature(proxy_path, signature, secret) {
        return reconstruct_original_url(proxy_path);
    }
    if proxy_path.starts_with(OPAQUE_PREFIX) {
        return Err(ExternalPathError::InvalidSignature);
    }
    let parts: Vec<&str> = proxy_path.split('/').collect();
    protocol_index_candidates(&parts)
        .into_iter()
        .find_map(|index| verify_re_encoded_proxy_path(&parts, index, signature, secret))
        .ok_or(ExternalPathError::InvalidSignature)
}

#[cfg(test)]
mod signature_tests {
    use super::*;
    #[test]
    fn signature_matches_node_hmac_base64url_behavior() {
        let sig = create_signature("v2/aHR0cHM6Ly9leGFtcGxlLmNvbS8", b"secret");
        assert_eq!("jkGeYYCiqJ67eS5T6lWJ4eQ77pwIaf6yoXXF5LncsHY", sig);
        assert!(verify_signature(
            "v2/aHR0cHM6Ly9leGFtcGxlLmNvbS8",
            &sig,
            b"secret"
        ));
    }

    #[test]
    fn verify_rejects_tampered_signature() {
        let sig = create_signature("input", b"secret");
        let mut tampered = sig.clone().into_bytes();
        tampered[0] = if tampered[0] == b'A' { b'B' } else { b'A' };
        assert!(!verify_signature(
            "input",
            std::str::from_utf8(&tampered).unwrap(),
            b"secret"
        ));
    }

    #[test]
    fn verify_rejects_wrong_secret() {
        let sig = create_signature("input", b"secret");
        assert!(!verify_signature("input", &sig, b"different"));
    }

    #[test]
    fn verify_rejects_wrong_input() {
        let sig = create_signature("input-a", b"secret");
        assert!(!verify_signature("input-b", &sig, b"secret"));
    }

    #[test]
    fn verify_rejects_different_length_signature_without_panic() {
        assert!(!verify_signature("input", "short", b"secret"));
        assert!(!verify_signature(
            "input",
            "definitely-too-long-of-a-signature-for-hmac-sha256-base64url",
            b"secret"
        ));
    }

    const SIGNED_QUERY_PATH: &str =
        "%3Fsize%3D48/https/cdn.discordapp.com/emojis/1005624491637997568.png";

    fn signed(path: &str) -> String {
        create_signature(path, b"secret")
    }

    #[test]
    fn signed_path_verifies_on_the_fast_path() {
        assert_eq!(
            Ok("https://cdn.discordapp.com/emojis/1005624491637997568.png?size=48".to_owned()),
            verify_signed_proxy_path(SIGNED_QUERY_PATH, &signed(SIGNED_QUERY_PATH), b"secret")
        );
    }

    #[test]
    fn re_encoded_signed_path_verifies_and_fetches_the_signed_url() {
        let sig = signed(SIGNED_QUERY_PATH);
        for received in [
            "%3Fsize=48/https/cdn.discordapp.com/emojis/1005624491637997568.png",
            "%3fsize%3d48/https/cdn.discordapp.com/emojis/1005624491637997568.png",
            "%3Fsize%3D48/https/cdn.discordapp.com/emojis/%31005624491637997568.png",
        ] {
            assert_eq!(
                Ok("https://cdn.discordapp.com/emojis/1005624491637997568.png?size=48".to_owned()),
                verify_signed_proxy_path(received, &sig, b"secret"),
                "{received}"
            );
        }
        let dashed = "https/example.com/a-b%20c.png";
        assert_eq!(
            Ok("https://example.com/a-b c.png".to_owned()),
            verify_signed_proxy_path(
                "https/example.com/a%2Db%20c.png",
                &signed(dashed),
                b"secret"
            )
        );
    }

    #[test]
    fn re_encoded_path_for_a_different_url_is_rejected() {
        let sig = signed(SIGNED_QUERY_PATH);
        for received in [
            "%3Fsize=96/https/cdn.discordapp.com/emojis/1005624491637997568.png",
            "%3Fsize=48/https/evil.example/emojis/1005624491637997568.png",
            "%3Fsize=48/http/cdn.discordapp.com/emojis/1005624491637997568.png",
            "%3Fsize=48/https/cdn.discordapp.com/emojis/1.png",
            "https/cdn.discordapp.com/emojis/1005624491637997568.png",
            "%3Fsize=48/https/cdn.discordapp.com/emojis/1005624491637997568.png/",
        ] {
            assert_eq!(
                Err(ExternalPathError::InvalidSignature),
                verify_signed_proxy_path(received, &sig, b"secret"),
                "{received}"
            );
        }
        assert_eq!(
            Err(ExternalPathError::InvalidSignature),
            verify_signed_proxy_path(
                "%3Fsize=48/https/cdn.discordapp.com/emojis/1005624491637997568.png",
                &sig,
                b"other"
            )
        );
    }

    #[test]
    fn decoded_slash_in_a_path_segment_is_rejected() {
        let encoded_slash = "https/example.com/a%2Fb.png";
        assert_eq!(
            Err(ExternalPathError::InvalidSignature),
            verify_signed_proxy_path(
                "https/example.com/a/b.png",
                &signed(encoded_slash),
                b"secret"
            )
        );
    }

    fn re_encode(path: &str, decodes: impl Fn(u8) -> bool) -> String {
        let bytes = path.as_bytes();
        let mut out = Vec::with_capacity(bytes.len());
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'%'
                && i + 2 < bytes.len()
                && let (Some(hi), Some(lo)) = (hex_val(bytes[i + 1]), hex_val(bytes[i + 2]))
                && decodes((hi << 4) | lo)
            {
                out.push((hi << 4) | lo);
                i += 3;
                continue;
            }
            out.push(bytes[i]);
            i += 1;
        }
        String::from_utf8(out).expect("re-encoded path is utf-8")
    }

    fn apache_decodes(byte: u8) -> bool {
        byte.is_ascii_alphanumeric() || b"!$&'()*+,-.:;=@_~".contains(&byte)
    }

    fn nginx_decodes(byte: u8) -> bool {
        apache_decodes(byte) || b"[]".contains(&byte)
    }

    fn caddy_decodes(byte: u8) -> bool {
        byte.is_ascii_alphanumeric() || b"$&+,-.:;=@_~".contains(&byte)
    }

    fn slash_decoding_nginx_decodes(byte: u8) -> bool {
        nginx_decodes(byte) || byte == b'/'
    }

    pub(super) fn proxy_rewrites(path: &str) -> Vec<String> {
        let lowercase_hex = {
            let mut out = String::with_capacity(path.len());
            let mut rest = path;
            while let Some(at) = rest.find('%') {
                out.push_str(&rest[..at]);
                let escape = rest.get(at..at + 3).unwrap_or(&rest[at..]);
                out.push_str(&escape.to_ascii_lowercase());
                rest = &rest[at + escape.len()..];
            }
            out.push_str(rest);
            out
        };
        let go_escaped = path
            .replace('!', "%21")
            .replace('\'', "%27")
            .replace('(', "%28")
            .replace(')', "%29")
            .replace('*', "%2A");
        vec![
            re_encode(path, apache_decodes),
            re_encode(path, nginx_decodes),
            re_encode(path, caddy_decodes),
            re_encode(path, slash_decoding_nginx_decodes),
            lowercase_hex,
            go_escaped,
        ]
    }

    #[test]
    fn every_observed_proxy_rewrite_verifies_to_the_signed_url() {
        let printable: String = (0x20u8..0x7f)
            .map(char::from)
            .filter(|c| !matches!(c, '/' | '#' | '?'))
            .chain("åäö€😀".chars())
            .collect();
        let query_url = format!("https://example.com/a.png?{printable}");
        let path_url = format!("https://example.com/{printable}/b.png?x=1");
        for url in [
            "https://cdn.discordapp.com/emojis/1005624491637997568.png?size=48",
            "https://example.com/a.png?x=y/b",
            "https://example.com/a.png?p=/c/d",
            "https://example.com/a.png?redirect=https://cdn.example/x.png",
            "https://example.com/a.png?u=https/x/y",
            "https://example.com/o/x%2Fy.png?alt=media",
            query_url.as_str(),
            path_url.as_str(),
        ] {
            let path = build_external_media_proxy_path(url).expect("path builds");
            let sig = signed(&path);
            let expected = reconstruct_original_url(&path);
            for received in proxy_rewrites(&path) {
                assert_eq!(
                    expected,
                    verify_signed_proxy_path(&received, &sig, b"secret"),
                    "{url} received as {received}"
                );
                assert_eq!(
                    Err(ExternalPathError::InvalidSignature),
                    verify_signed_proxy_path(&received, &sig, b"other"),
                    "{url} received as {received}"
                );
            }
        }
    }

    #[test]
    fn decoded_slashes_in_the_query_never_verify_a_different_host() {
        let sig = signed("%3Fu%3Dhttps%2Fx%2Fy/https/example.com/a.png");
        for received in [
            "%3Fu=https/x/y/https/evil.example/a.png",
            "%3Fu=https/x/https/example.com/a.png",
            "%3Fu=https/evil.example/y/https/example.com/a.png",
        ] {
            assert_eq!(
                Err(ExternalPathError::InvalidSignature),
                verify_signed_proxy_path(received, &sig, b"secret"),
                "{received}"
            );
        }
    }

    #[test]
    fn userinfo_smuggled_into_the_host_segment_is_rejected() {
        let sig = signed("https/cdn.discordapp.com/x.png");
        for received in [
            "https/evil.example%5C@cdn.discordapp.com/x.png",
            "https/evil.example%5C%40cdn.discordapp.com/x.png",
            "https/evil.example%40cdn.discordapp.com/x.png",
            "https/evil.example@cdn.discordapp.com/x.png",
            "https/cdn.discordapp.com%23@evil.example/x.png",
        ] {
            assert_eq!(
                Err(ExternalPathError::InvalidSignature),
                verify_signed_proxy_path(received, &sig, b"secret"),
                "{received}"
            );
        }
    }

    #[test]
    fn opaque_paths_stay_strict() {
        let opaque = build_opaque_external_media_proxy_path("https://example.com/a.png");
        let sig = signed(&opaque);
        assert_eq!(
            Ok("https://example.com/a.png".to_owned()),
            verify_signed_proxy_path(&opaque, &sig, b"secret")
        );
        assert_eq!(
            Err(ExternalPathError::InvalidSignature),
            verify_signed_proxy_path(&format!("{opaque}A"), &sig, b"secret")
        );
        let segmented = build_external_media_proxy_path("https://example.com/a.png").unwrap();
        assert_eq!(
            Err(ExternalPathError::InvalidSignature),
            verify_signed_proxy_path(&opaque, &signed(&segmented), b"secret")
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn own_media_urls_require_the_same_origin_and_path_boundary() {
        let endpoint = "https://chat.example.test/media/";
        for target in [
            "https://chat.example.test/media",
            "https://CHAT.example.test:443/media/attachments/1/2/cat.gif",
        ] {
            assert!(is_own_media_url(endpoint, &Url::parse(target).unwrap()));
        }
        for target in [
            "https://chat.example.test/mediafiles/1.gif",
            "https://static.klipy.com/a.webp",
            "http://chat.example.test/media/a.gif",
            "https://chat.example.test:444/media/a.gif",
        ] {
            assert!(!is_own_media_url(endpoint, &Url::parse(target).unwrap()));
        }
        assert!(!is_own_media_url(
            "invalid endpoint",
            &Url::parse("https://chat.example.test/media").unwrap()
        ));
    }

    #[test]
    fn matches_the_typescript_builder_on_the_shared_vectors() {
        let raw = include_str!("testdata/external_media_proxy_path_vectors.json");
        let vectors: serde_json::Value = serde_json::from_str(raw).expect("vectors parse as json");
        let vectors = vectors.as_array().expect("vectors are an array");
        assert!(!vectors.is_empty());
        for vector in vectors {
            let original = vector["url"].as_str().expect("vector has a url");
            let expected = vector["path"].as_str().expect("vector has a path");
            let normalized = url::Url::parse(original).expect("vector url parses");
            assert_eq!(
                expected,
                build_external_media_proxy_path(normalized.as_str()).expect("path builds"),
                "vector {original}"
            );
        }
    }

    #[test]
    fn shared_vectors_verify_after_proxy_re_encoding() {
        let raw = include_str!("testdata/external_media_proxy_path_vectors.json");
        let vectors: serde_json::Value = serde_json::from_str(raw).expect("vectors parse as json");
        for vector in vectors.as_array().expect("vectors are an array") {
            let path = vector["path"].as_str().expect("vector has a path");
            let signature = create_signature(path, b"secret");
            let expected = reconstruct_original_url(path);
            for received in super::signature_tests::proxy_rewrites(path) {
                assert_eq!(
                    expected,
                    verify_signed_proxy_path(&received, &signature, b"secret"),
                    "{received}"
                );
            }
        }
    }
}
