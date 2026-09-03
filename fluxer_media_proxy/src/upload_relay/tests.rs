// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::upload_relay::{
    RelayError, release_buffer_budget,
    target::{RelayRequest, query_part_number, valid_content_type, validate_relay_request},
    token::{TokenError, TokenMethod, TokenPayload, decode_token, encode_token, token_from_query},
    try_reserve_buffer_budget,
};
use http::Method;

const BASE64_URL_ALPHABET: &[u8] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

fn token() -> TokenPayload {
    TokenPayload {
        b: "uploads".to_owned(),
        k: "guild/file.bin".to_owned(),
        m: TokenMethod::Put,
        u: Some("upload-id".to_owned()),
        p: Some(7),
        ct: Some("application/octet-stream".to_owned()),
        mb: 100,
        e: 2_000,
    }
}

fn with_noncanonical_tail(encoded: &str) -> String {
    let mut bytes = encoded.as_bytes().to_vec();
    let last = bytes.last_mut().expect("encoded token is not empty");
    let index = BASE64_URL_ALPHABET
        .iter()
        .position(|candidate| candidate == last)
        .expect("encoded token ends with a base64url character");
    *last = BASE64_URL_ALPHABET[index ^ 1];
    String::from_utf8(bytes).expect("base64url alphabet is ascii")
}

#[test]
fn token_roundtrip_and_tamper_detection() {
    let secret = [3u8; 32];
    let encoded = encode_token(&token(), &secret).unwrap();
    assert_eq!(token(), decode_token(&encoded, &secret, 1_000).unwrap());
    let mut tampered = encoded.clone();
    tampered.push('x');
    assert_eq!(
        Err(TokenError::BadSignature),
        decode_token(&tampered, &secret, 1_000)
    );
}

#[test]
fn expired_token_rejected() {
    let secret = [3u8; 32];
    let encoded = encode_token(&token(), &secret).unwrap();
    assert_eq!(
        Err(TokenError::Expired),
        decode_token(&encoded, &secret, 2_000)
    );
}

#[test]
fn validates_matching_relay_request() {
    validate_relay_request(
        &token(),
        RelayRequest {
            uploads_bucket: "uploads",
            request_key: "guild/file.bin",
            request_method: &Method::PUT,
            query_upload_id: Some("upload-id"),
            query_part_number: Some(7),
            content_length: Some(99),
            max_body_bytes: 100,
        },
    )
    .unwrap();
}

#[test]
fn validates_relay_mismatches() {
    assert_eq!(
        Err(RelayError::WrongBucket),
        validate_relay_request(
            &token(),
            RelayRequest {
                uploads_bucket: "cdn",
                request_key: "guild/file.bin",
                request_method: &Method::PUT,
                query_upload_id: Some("upload-id"),
                query_part_number: Some(7),
                content_length: Some(99),
                max_body_bytes: 100,
            }
        )
    );
    assert_eq!(
        Err(RelayError::PayloadTooLarge),
        validate_relay_request(
            &token(),
            RelayRequest {
                uploads_bucket: "uploads",
                request_key: "guild/file.bin",
                request_method: &Method::PUT,
                query_upload_id: Some("upload-id"),
                query_part_number: Some(7),
                content_length: Some(101),
                max_body_bytes: 100,
            }
        )
    );
    validate_relay_request(
        &token(),
        RelayRequest {
            uploads_bucket: "uploads",
            request_key: "guild/file.bin",
            request_method: &Method::PUT,
            query_upload_id: Some("upload-id"),
            query_part_number: Some(7),
            content_length: None,
            max_body_bytes: 100,
        },
    )
    .unwrap();
}

#[test]
fn buffer_budget_is_bounded() {
    assert!(try_reserve_buffer_budget(4, 8));
    assert!(!try_reserve_buffer_budget(5, 8));
    release_buffer_budget(4);
    assert!(try_reserve_buffer_budget(8, 8));
    release_buffer_budget(8);
}

#[test]
fn token_decoder_rejects_noncanonical_wrong_secret_and_oversized_tokens() {
    let secret = [3u8; 32];
    let encoded = encode_token(&token(), &secret).unwrap();
    assert_eq!(
        Err(TokenError::BadEncoding),
        decode_token(&with_noncanonical_tail(&encoded), &secret, 1_000)
    );
    assert_eq!(
        Err(TokenError::BadSignature),
        decode_token(&encoded, &[9u8; 32], 1_000)
    );
    assert_eq!(
        Err(TokenError::Malformed),
        decode_token(&"x".repeat(16 * 1024 + 1), &secret, 1_000)
    );
    assert_eq!(
        Err(TokenError::Malformed),
        decode_token("no-separator", &secret, 1_000)
    );
}

#[test]
fn token_query_requires_a_present_and_bounded_token() {
    assert_eq!(Err(RelayError::MissingToken), token_from_query(None));
    assert_eq!(Err(RelayError::InvalidToken), token_from_query(Some("")));
    assert_eq!(
        Err(RelayError::InvalidToken),
        token_from_query(Some(&"x".repeat(16 * 1024 + 1)))
    );
    assert_eq!(Ok("value"), token_from_query(Some("value")));
}

#[test]
fn relay_request_requires_the_exact_signed_key() {
    for request_key in [
        "guild/../file.bin",
        "guild/file.bin/",
        "/guild/file.bin",
        "guild//file.bin",
    ] {
        assert_eq!(
            Err(RelayError::KeyMismatch),
            validate_relay_request(
                &token(),
                RelayRequest {
                    uploads_bucket: "uploads",
                    request_key,
                    request_method: &Method::PUT,
                    query_upload_id: Some("upload-id"),
                    query_part_number: Some(7),
                    content_length: Some(99),
                    max_body_bytes: 100,
                }
            )
        );
    }
}

#[test]
fn part_number_query_is_optional_but_never_empty() {
    assert_eq!(Ok(None), query_part_number(None));
    assert_eq!(Ok(Some(3)), query_part_number(Some("3")));
    assert_eq!(Err(RelayError::BadQuery), query_part_number(Some("")));
    assert_eq!(Err(RelayError::BadQuery), query_part_number(Some("two")));
}

#[test]
fn content_type_gate_rejects_header_injection_and_malformed_media_types() {
    for accepted in [
        "image/png",
        "application/octet-stream; charset=binary",
        "text/plain;charset=utf-8",
        "text/plain; filename=\"a b\"",
        "video/mp4; codecs=\"avc1.42E01E, mp4a.40.2\"",
    ] {
        assert!(valid_content_type(accepted), "rejected {accepted:?}");
    }
    for rejected in [
        "",
        "image",
        "image/(png)",
        "image/png\r\nInjected: yes",
        "image/png\nInjected: yes",
        "*/*",
        "image/png; charset=a; charset=b",
        "image/png; charset",
        "image/png/extra",
        "image /png",
    ] {
        assert!(!valid_content_type(rejected), "accepted {rejected:?}");
    }
    assert!(!valid_content_type(&format!("image/{}", "p".repeat(255))));
}
