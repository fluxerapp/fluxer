// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::external_media_path::percent_decode;
use hmac::{KeyInit, Mac};

pub const ATTACHMENT_URL_TTL_SECS: u64 = 86_400;
pub const ATTACHMENT_URL_BUCKET_SECS: u64 = 43_200;
pub const SIGNATURE_PARAMETER_NAMES: [&str; 4] = ["ex", "is", "hm", "uc"];

const DOMAIN: &str = "fluxer-attachment-url-v1";
const DATA_PACKAGE_USAGE: &str = "dp";
const DATA_PACKAGE_EXPIRES: &str = "0";
const WINDOW_HEX_LEN: usize = 8;
const WINDOW_MAX_SECS: u64 = 0xffff_ffff;
const SIGNATURE_HEX_LEN: usize = 64;

type HmacSha256 = hmac::Hmac<sha2::Sha256>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Verdict {
    Valid,
    Missing,
    Malformed,
    Mismatch,
    Expired,
}

impl Verdict {
    pub fn label(self) -> &'static str {
        match self {
            Self::Valid => "valid",
            Self::Missing => "missing",
            Self::Malformed => "malformed",
            Self::Mismatch => "mismatch",
            Self::Expired => "expired",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Verification {
    pub verdict: Verdict,
    pub remaining_secs: Option<u64>,
    pub expired_secs: Option<u64>,
}

impl Verification {
    fn without_expiry(verdict: Verdict) -> Self {
        Self {
            verdict,
            remaining_secs: None,
            expired_secs: None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UrlKind {
    Ordinary,
    DataPackage,
}

impl UrlKind {
    pub fn usage(self) -> &'static str {
        match self {
            Self::Ordinary => "",
            Self::DataPackage => DATA_PACKAGE_USAGE,
        }
    }
}

pub fn issue_window(anchor_secs: u64, now_secs: u64) -> (u64, u64) {
    let elapsed = now_secs.saturating_sub(anchor_secs);
    let issued = anchor_secs
        .saturating_add((elapsed / ATTACHMENT_URL_BUCKET_SECS) * ATTACHMENT_URL_BUCKET_SECS);
    (issued, issued.saturating_add(ATTACHMENT_URL_TTL_SECS))
}

pub fn canonical_input(storage_key: &str, ex: u64, is: u64, kind: UrlKind) -> String {
    format!(
        "{DOMAIN}\n{ex:08x}\n{is:08x}\n{}\n{storage_key}",
        kind.usage()
    )
}

pub fn sign(storage_key: &str, ex: u64, is: u64, kind: UrlKind, secret: &[u8]) -> String {
    let input = canonical_input(storage_key, ex, is, kind);
    hex::encode(keyed_mac(secret, &input).finalize().into_bytes())
}

pub fn is_signature_parameter_name(name: &str) -> bool {
    let decoded = percent_decode(name, true);
    SIGNATURE_PARAMETER_NAMES
        .iter()
        .any(|candidate| candidate.as_bytes() == decoded.as_slice())
}

pub fn with_signature(
    url: &str,
    storage_key: &str,
    anchor_secs: u64,
    now_secs: u64,
    secret: &[u8],
) -> String {
    let (issued, expires) = issue_window(anchor_secs, now_secs);
    if expires > WINDOW_MAX_SECS {
        return url.to_owned();
    }
    let signature = sign(storage_key, expires, issued, UrlKind::Ordinary, secret);
    replace_signature(
        url,
        &format!("ex={expires:08x}&is={issued:08x}&hm={signature}"),
    )
}

pub fn with_data_package_signature(
    url: &str,
    storage_key: &str,
    anchor_secs: u64,
    now_secs: u64,
    secret: &[u8],
) -> String {
    let (issued, expires) = issue_window(anchor_secs, now_secs);
    if expires > WINDOW_MAX_SECS {
        return url.to_owned();
    }
    let signature = sign(storage_key, 0, issued, UrlKind::DataPackage, secret);
    replace_signature(
        url,
        &format!(
            "ex={DATA_PACKAGE_EXPIRES}&is={issued:08x}&hm={signature}&uc={DATA_PACKAGE_USAGE}"
        ),
    )
}

pub fn decode_key(path: &str) -> Option<String> {
    let decoded = percent_decode(path.trim_start_matches('/'), false);
    std::str::from_utf8(&decoded).ok().map(ToOwned::to_owned)
}

pub fn verify(
    storage_key: &str,
    raw_query: Option<&str>,
    secrets: &[&[u8]],
    now_secs: u64,
) -> Verification {
    let Some(raw_query) = raw_query else {
        return Verification::without_expiry(Verdict::Missing);
    };
    let Some(fields) = signature_fields(raw_query) else {
        return Verification::without_expiry(Verdict::Malformed);
    };
    let (expires, issued, signature) = match (
        fields.expires,
        fields.issued,
        fields.signature,
        fields.usage,
    ) {
        (None, None, None, None) => return Verification::without_expiry(Verdict::Missing),
        (Some(expires), Some(issued), Some(signature), _) => (expires, issued, signature),
        _ => return Verification::without_expiry(Verdict::Malformed),
    };
    let kind = match fields.usage {
        None => UrlKind::Ordinary,
        Some(DATA_PACKAGE_USAGE) => UrlKind::DataPackage,
        Some(_) => return Verification::without_expiry(Verdict::Malformed),
    };
    let expires = match kind {
        UrlKind::Ordinary => parse_window(expires),
        UrlKind::DataPackage => (expires == DATA_PACKAGE_EXPIRES).then_some(0),
    };
    let (Some(expires), Some(issued)) = (expires, parse_window(issued)) else {
        return Verification::without_expiry(Verdict::Malformed);
    };
    if !is_lowercase_hex(signature, SIGNATURE_HEX_LEN) {
        return Verification::without_expiry(Verdict::Malformed);
    }
    let Ok(provided) = hex::decode(signature) else {
        return Verification::without_expiry(Verdict::Malformed);
    };
    if kind == UrlKind::Ordinary && issued > expires {
        return Verification::without_expiry(Verdict::Malformed);
    }
    let input = canonical_input(storage_key, expires, issued, kind);
    let matched = secrets
        .iter()
        .any(|secret| keyed_mac(secret, &input).verify_slice(&provided).is_ok());
    if !matched {
        return Verification::without_expiry(Verdict::Mismatch);
    }
    if kind == UrlKind::DataPackage {
        return Verification::without_expiry(Verdict::Valid);
    }
    if now_secs >= expires {
        return Verification {
            verdict: Verdict::Expired,
            remaining_secs: None,
            expired_secs: Some(now_secs - expires),
        };
    }
    Verification {
        verdict: Verdict::Valid,
        remaining_secs: Some(expires - now_secs),
        expired_secs: None,
    }
}

#[derive(Default)]
struct SignatureFields<'a> {
    expires: Option<&'a str>,
    issued: Option<&'a str>,
    signature: Option<&'a str>,
    usage: Option<&'a str>,
}

fn signature_fields(raw_query: &str) -> Option<SignatureFields<'_>> {
    let mut fields = SignatureFields::default();
    for field in raw_query.split('&').filter(|field| !field.is_empty()) {
        let (name, value) = field.split_once('=').unwrap_or((field, ""));
        let slot = match percent_decode(name, true).as_slice() {
            b"ex" => &mut fields.expires,
            b"is" => &mut fields.issued,
            b"hm" => &mut fields.signature,
            b"uc" => &mut fields.usage,
            _ => continue,
        };
        if slot.replace(value).is_some() {
            return None;
        }
    }
    Some(fields)
}

fn keyed_mac(secret: &[u8], input: &str) -> HmacSha256 {
    let mut mac = HmacSha256::new_from_slice(secret).expect("hmac accepts any key length");
    mac.update(input.as_bytes());
    mac
}

fn parse_window(value: &str) -> Option<u64> {
    if !is_lowercase_hex(value, WINDOW_HEX_LEN) {
        return None;
    }
    u64::from_str_radix(value, 16).ok()
}

fn is_lowercase_hex(value: &str, len: usize) -> bool {
    value.len() == len
        && value
            .bytes()
            .all(|byte| matches!(byte, b'0'..=b'9' | b'a'..=b'f'))
}

fn replace_signature(url: &str, signature_fields: &str) -> String {
    let (head, fragment) = split_fragment(url);
    let (base, query) = split_query(head);
    let preserved = preserved_fields(query);
    if preserved.is_empty() {
        return format!("{base}?{signature_fields}{fragment}");
    }
    format!(
        "{base}?{signature_fields}&{}{fragment}",
        preserved.join("&")
    )
}

fn split_fragment(url: &str) -> (&str, &str) {
    match url.find('#') {
        Some(index) => (&url[..index], &url[index..]),
        None => (url, ""),
    }
}

fn split_query(head: &str) -> (&str, &str) {
    head.split_once('?').unwrap_or((head, ""))
}

fn preserved_fields(query: &str) -> Vec<&str> {
    query
        .split('&')
        .filter(|field| {
            if field.is_empty() || *field == "=" {
                return false;
            }
            let (name, _) = field.split_once('=').unwrap_or((*field, ""));
            !is_signature_parameter_name(name)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use base64::prelude::*;
    use serde_json::Value;

    const VECTORS: &str = include_str!("testdata/attachment_url_signature_vectors.json");
    const KEY: &str = "attachments/1544725486800732163/1544971349200470016/cat.gif";
    const ANCHOR: u64 = 1_788_420_273;

    type Signer = fn(&str, &str, u64, u64, &[u8]) -> String;

    fn fixture() -> Value {
        serde_json::from_str(VECTORS).expect("the signature vectors parse as json")
    }

    fn fixture_secrets(fixture: &Value) -> Vec<Vec<u8>> {
        fixture["secrets_base64"]
            .as_array()
            .expect("the fixture has a secret list")
            .iter()
            .map(|entry| {
                BASE64_STANDARD
                    .decode(entry.as_str().expect("a secret is a string"))
                    .expect("a secret is standard base64")
            })
            .collect()
    }

    fn cases<'a>(fixture: &'a Value, name: &str) -> &'a [Value] {
        fixture[name]
            .as_array()
            .expect("the fixture has the case list")
            .as_slice()
    }

    fn text<'a>(case: &'a Value, field: &str) -> &'a str {
        case[field]
            .as_str()
            .unwrap_or_else(|| panic!("case has {field}"))
    }

    fn number(case: &Value, field: &str) -> u64 {
        case[field]
            .as_u64()
            .unwrap_or_else(|| panic!("case has {field}"))
    }

    fn secret_bytes() -> Vec<u8> {
        (0u8..32).collect()
    }

    fn other_secret_bytes() -> Vec<u8> {
        (32u8..64).collect()
    }

    fn verdict_of(
        storage_key: &str,
        raw_query: Option<&str>,
        secrets: &[&[u8]],
        now_secs: u64,
    ) -> Verdict {
        verify(storage_key, raw_query, secrets, now_secs).verdict
    }

    fn ordinary_query(key: &str, issued: u64, expires: u64, secret: &[u8]) -> String {
        format!(
            "ex={expires:08x}&is={issued:08x}&hm={}",
            sign(key, expires, issued, UrlKind::Ordinary, secret)
        )
    }

    fn data_package_query(key: &str, issued: u64, secret: &[u8]) -> String {
        format!(
            "ex=0&is={issued:08x}&hm={}&uc=dp",
            sign(key, 0, issued, UrlKind::DataPackage, secret)
        )
    }

    #[test]
    fn the_fixture_declares_the_constants_the_code_uses() {
        let fixture = fixture();
        assert_eq!(3, fixture["version"].as_u64().expect("version"));
        assert_eq!(
            ATTACHMENT_URL_TTL_SECS,
            fixture["ttl_secs"].as_u64().expect("ttl")
        );
        assert_eq!(
            ATTACHMENT_URL_BUCKET_SECS,
            fixture["bucket_secs"].as_u64().expect("bucket")
        );
        assert_eq!(
            1_420_070_400_000,
            fixture["fluxer_epoch_ms"].as_u64().expect("epoch")
        );
    }

    #[test]
    fn every_sign_vector_reproduces_byte_for_byte() {
        let fixture = fixture();
        let secrets = fixture_secrets(&fixture);
        let parsed = cases(&fixture, "sign");
        let mut run = 0;
        let mut data_packages = 0;
        for case in parsed {
            let name = text(case, "name");
            let storage_key = text(case, "storage_key");
            let anchor = number(case, "anchor");
            let now = number(case, "now");
            let (issued, expires) = issue_window(anchor, now);
            assert_eq!(format!("{issued:08x}"), text(case, "is"), "{name} issued");
            let (kind, signed_expires, signer): (_, _, Signer) = match text(case, "uc") {
                "" => {
                    assert_eq!(format!("{expires:08x}"), text(case, "ex"), "{name} expires");
                    (UrlKind::Ordinary, expires, with_signature)
                }
                "dp" => {
                    assert_eq!("0", text(case, "ex"), "{name} expires");
                    data_packages += 1;
                    (UrlKind::DataPackage, 0, with_data_package_signature)
                }
                other => panic!("{name} has an unknown uc {other}"),
            };
            assert_eq!(
                text(case, "signature_input"),
                canonical_input(storage_key, signed_expires, issued, kind),
                "{name} input"
            );
            assert_eq!(
                text(case, "hm"),
                sign(storage_key, signed_expires, issued, kind, &secrets[0]),
                "{name} signature"
            );
            assert_eq!(
                text(case, "signed"),
                signer(text(case, "url"), storage_key, anchor, now, &secrets[0]),
                "{name} signed url"
            );
            let signed = text(case, "signed");
            assert!(!signed.ends_with('&'), "{name} trailing separator");
            assert!(!signed.contains("&&"), "{name} doubled separator");
            run += 1;
        }
        assert_eq!(parsed.len(), run);
        assert!(run >= 8);
        assert!(data_packages >= 3);
    }

    #[test]
    fn every_verify_vector_reaches_its_declared_verdict() {
        let fixture = fixture();
        let secrets = fixture_secrets(&fixture);
        let parsed = cases(&fixture, "verify");
        let mut run = 0;
        let mut rotations = 0;
        let mut trailing_separators = 0;
        let mut doubled_separators = 0;
        for case in parsed {
            let name = text(case, "name");
            let held: Vec<&[u8]> = case["verifier_secret_indices"]
                .as_array()
                .expect("case names the secrets the verifier holds")
                .iter()
                .map(|index| {
                    let index = index.as_u64().expect("a secret index is a number") as usize;
                    secrets[index].as_slice()
                })
                .collect();
            assert!(!held.is_empty(), "{name}");
            if held.len() > 1 {
                rotations += 1;
            }
            let now = number(case, "now");
            let query = case["query"].as_str();
            let verdict = match decode_key(text(case, "path")) {
                Some(key) => verdict_of(&key, query, &held, now),
                None => Verdict::Malformed,
            };
            assert_eq!(text(case, "verdict"), verdict.label(), "{name}");
            if verdict == Verdict::Valid {
                if query.is_some_and(|query| query.ends_with('&')) {
                    trailing_separators += 1;
                }
                if query.is_some_and(|query| query.contains("&&")) {
                    doubled_separators += 1;
                }
            }
            run += 1;
        }
        assert_eq!(parsed.len(), run);
        assert!(run >= 50);
        assert!(rotations >= 3);
        assert!(trailing_separators >= 2);
        assert!(doubled_separators >= 2);
    }

    #[test]
    fn verification_tries_every_configured_secret() {
        let first = secret_bytes();
        let second = other_secret_bytes();
        let now = ANCHOR;
        let (issued, expires) = issue_window(ANCHOR, now);
        for query in [
            ordinary_query(KEY, issued, expires, &second),
            data_package_query(KEY, issued, &second),
        ] {
            assert_eq!(
                Verdict::Mismatch,
                verdict_of(KEY, Some(&query), &[&first], now)
            );
            assert_eq!(
                Verdict::Valid,
                verdict_of(KEY, Some(&query), &[&first, &second], now)
            );
            assert_eq!(
                Verdict::Valid,
                verdict_of(KEY, Some(&query), &[&second, &first], now)
            );
            assert_eq!(Verdict::Mismatch, verdict_of(KEY, Some(&query), &[], now));
        }
    }

    #[test]
    fn a_signature_never_validates_across_keys_or_windows() {
        let secret = secret_bytes();
        let now = ANCHOR;
        let (issued, expires) = issue_window(ANCHOR, now);
        let signature = sign(KEY, expires, issued, UrlKind::Ordinary, &secret);
        let query = ordinary_query(KEY, issued, expires, &secret);
        let package = data_package_query(KEY, issued, &secret);
        assert_eq!(
            Verdict::Valid,
            verdict_of(KEY, Some(&query), &[&secret], now)
        );
        assert_eq!(
            Verdict::Valid,
            verdict_of(KEY, Some(&package), &[&secret], now)
        );
        for other in [
            "attachments/1544725486800732163/1544971349200470016/cat.gi",
            "attachments/1544725486800732163/1544971349200470017/cat.gif",
            "attachments/1544725486800732163/1544971349200470016/cat.gif ",
            "attachments/1544725486800732163/1544971349200470016/sub/cat.gif",
        ] {
            for query in [&query, &package] {
                assert_eq!(
                    Verdict::Mismatch,
                    verdict_of(other, Some(query), &[&secret], now),
                    "{other} {query}"
                );
            }
        }
        let shifted = format!(
            "ex={:08x}&is={issued:08x}&hm={signature}",
            expires + ATTACHMENT_URL_BUCKET_SECS
        );
        assert_eq!(
            Verdict::Mismatch,
            verdict_of(KEY, Some(&shifted), &[&secret], now)
        );
        let shifted_package = format!(
            "ex=0&is={:08x}&hm={}&uc=dp",
            issued + 1,
            sign(KEY, 0, issued, UrlKind::DataPackage, &secret)
        );
        assert_eq!(
            Verdict::Mismatch,
            verdict_of(KEY, Some(&shifted_package), &[&secret], now)
        );
    }

    #[test]
    fn a_signature_is_bound_to_its_usage() {
        let secret = secret_bytes();
        let now = ANCHOR;
        let (issued, expires) = issue_window(ANCHOR, now);
        assert_ne!(
            canonical_input(KEY, 0, issued, UrlKind::Ordinary),
            canonical_input(KEY, 0, issued, UrlKind::DataPackage)
        );
        let ordinary = sign(KEY, expires, issued, UrlKind::Ordinary, &secret);
        let package = sign(KEY, 0, issued, UrlKind::DataPackage, &secret);
        for (label, query) in [
            (
                "ordinary signature relabelled as a data package",
                format!("ex=0&is={issued:08x}&hm={ordinary}&uc=dp"),
            ),
            (
                "data package signature relabelled as ordinary",
                format!("ex={expires:08x}&is={issued:08x}&hm={package}"),
            ),
            (
                "ordinary input with a zero expiry signed under the data package",
                format!(
                    "ex=0&is={issued:08x}&hm={}&uc=dp",
                    sign(KEY, 0, issued, UrlKind::Ordinary, &secret)
                ),
            ),
        ] {
            assert_eq!(
                Verdict::Mismatch,
                verdict_of(KEY, Some(&query), &[&secret], now),
                "{label}"
            );
        }
    }

    #[test]
    fn every_parameter_shape_outside_the_grammar_is_refused() {
        let secret = secret_bytes();
        let now = ANCHOR;
        let (issued, expires) = issue_window(ANCHOR, now);
        let signature = sign(KEY, expires, issued, UrlKind::Ordinary, &secret);
        let package = sign(KEY, 0, issued, UrlKind::DataPackage, &secret);
        let valid = ordinary_query(KEY, issued, expires, &secret);
        let valid_package = data_package_query(KEY, issued, &secret);
        assert_eq!(
            Verdict::Valid,
            verdict_of(KEY, Some(&valid), &[&secret], now)
        );
        assert_eq!(
            Verdict::Valid,
            verdict_of(KEY, Some(&valid_package), &[&secret], now)
        );
        for (label, query, expected) in [
            ("no query", None, Verdict::Missing),
            ("empty query", Some(String::new()), Verdict::Missing),
            (
                "only transform parameters",
                Some("width=64&format=webp".to_owned()),
                Verdict::Missing,
            ),
            (
                "only the signature",
                Some(format!("hm={signature}")),
                Verdict::Malformed,
            ),
            (
                "only the window",
                Some(format!("ex={expires:08x}&is={issued:08x}")),
                Verdict::Malformed,
            ),
            (
                "repeated signature",
                Some(format!("{valid}&hm={signature}")),
                Verdict::Malformed,
            ),
            (
                "percent escaped second name",
                Some(format!("%65x={expires:08x}&{valid}")),
                Verdict::Malformed,
            ),
            (
                "uppercase signature",
                Some(format!(
                    "ex={expires:08x}&is={issued:08x}&hm={}",
                    signature.to_ascii_uppercase()
                )),
                Verdict::Malformed,
            ),
            (
                "percent escaped value",
                Some(format!(
                    "ex={expires:08x}&is={issued:08x}&hm=%{}",
                    &signature[2..]
                )),
                Verdict::Malformed,
            ),
            (
                "short window field",
                Some(format!("ex={:07x}&is={issued:08x}&hm={signature}", 1)),
                Verdict::Malformed,
            ),
            (
                "issued after expiry",
                Some(format!("ex={issued:08x}&is={expires:08x}&hm={signature}")),
                Verdict::Malformed,
            ),
            (
                "name without a value",
                Some(format!("ex&is={issued:08x}&hm={signature}")),
                Verdict::Malformed,
            ),
            (
                "zero expiry without uc",
                Some(format!("ex=0&is={issued:08x}&hm={package}")),
                Verdict::Malformed,
            ),
            (
                "padded zero expiry with uc",
                Some(format!("ex=00000000&is={issued:08x}&hm={package}&uc=dp")),
                Verdict::Malformed,
            ),
            (
                "eight digit expiry with uc",
                Some(format!("{valid}&uc=dp")),
                Verdict::Malformed,
            ),
            (
                "double zero expiry with uc",
                Some(format!("ex=00&is={issued:08x}&hm={package}&uc=dp")),
                Verdict::Malformed,
            ),
            (
                "another uc value",
                Some(format!("ex=0&is={issued:08x}&hm={package}&uc=dq")),
                Verdict::Malformed,
            ),
            (
                "uppercase uc",
                Some(format!("ex=0&is={issued:08x}&hm={package}&uc=DP")),
                Verdict::Malformed,
            ),
            (
                "empty uc",
                Some(format!("ex=0&is={issued:08x}&hm={package}&uc=")),
                Verdict::Malformed,
            ),
            (
                "uc without a value",
                Some(format!("ex=0&is={issued:08x}&hm={package}&uc")),
                Verdict::Malformed,
            ),
            (
                "percent escaped uc value",
                Some(format!("ex=0&is={issued:08x}&hm={package}&uc=%64p")),
                Verdict::Malformed,
            ),
            ("uc alone", Some("uc=dp".to_owned()), Verdict::Malformed),
            (
                "uc among transform parameters",
                Some("width=64&uc=dp&format=webp".to_owned()),
                Verdict::Malformed,
            ),
            (
                "data package without its signature",
                Some(format!("ex=0&is={issued:08x}&uc=dp")),
                Verdict::Malformed,
            ),
            (
                "data package without its issued field",
                Some(format!("ex=0&hm={package}&uc=dp")),
                Verdict::Malformed,
            ),
            (
                "repeated uc",
                Some(format!("{valid_package}&uc=dp")),
                Verdict::Malformed,
            ),
            (
                "percent escaped repeated uc",
                Some(format!("{valid_package}&%75c=dp")),
                Verdict::Malformed,
            ),
            (
                "repeated zero expiry",
                Some(format!("ex=0&{valid_package}")),
                Verdict::Malformed,
            ),
            (
                "short data package issued field",
                Some(format!("ex=0&is={:07x}&hm={package}&uc=dp", 1)),
                Verdict::Malformed,
            ),
            (
                "uppercase data package signature",
                Some(format!(
                    "ex=0&is={issued:08x}&hm={}&uc=dp",
                    package.to_ascii_uppercase()
                )),
                Verdict::Malformed,
            ),
        ] {
            assert_eq!(
                expected,
                verdict_of(KEY, query.as_deref(), &[&secret], now),
                "{label}"
            );
        }
    }
}
