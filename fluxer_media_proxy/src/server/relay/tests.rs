// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::Config,
    server::{
        routes::relay::{relay_error, relay_put},
        state::AppState,
    },
    upload_relay::{
        RelayError,
        token::{TokenMethod, TokenPayload, encode_token, now_unix},
    },
};
use axum::{
    body::Body,
    extract::{Path, Query, State},
    http::{HeaderMap, HeaderValue, Method, Request, StatusCode, header},
};
use base64::engine::general_purpose::STANDARD;
use bytes::Bytes;
use std::{collections::HashMap, sync::Arc};

fn upload_relay_test_config(
    mode: &str,
    storage_root: &std::path::Path,
    spool_dir: &std::path::Path,
    relay_secret: &[u8],
) -> Config {
    Config::load_from_iter([
        (
            "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
            "secret".to_owned(),
        ),
        ("FLUXER_MEDIA_PROXY_MODE".to_owned(), mode.to_owned()),
        (
            "FLUXER_MEDIA_PROXY_STORAGE_BACKEND".to_owned(),
            "local".to_owned(),
        ),
        (
            "FLUXER_MEDIA_PROXY_STORAGE_ROOT".to_owned(),
            storage_root.display().to_string(),
        ),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64".to_owned(),
            base64::Engine::encode(&STANDARD, relay_secret),
        ),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES".to_owned(),
            "4096".to_owned(),
        ),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_DIR".to_owned(),
            spool_dir.display().to_string(),
        ),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES".to_owned(),
            (1u64 << 30).to_string(),
        ),
    ])
    .unwrap()
}

fn test_app_state(cfg: Config) -> Arc<AppState> {
    Arc::new(AppState::for_tests(cfg))
}

fn relay_test_token(key: &str, relay_secret: &[u8]) -> String {
    encode_token(
        &TokenPayload {
            b: "uploads".to_owned(),
            k: key.to_owned(),
            m: TokenMethod::Put,
            u: None,
            p: None,
            ct: Some("application/octet-stream".to_owned()),
            mb: 4096,
            e: now_unix() + 60,
        },
        relay_secret,
    )
    .unwrap()
}

#[test]
fn relay_capability_mismatches_are_forbidden() {
    for err in [
        RelayError::WrongBucket,
        RelayError::KeyMismatch,
        RelayError::MethodMismatch,
        RelayError::PartNumberMismatch,
        RelayError::UploadIdMismatch,
    ] {
        assert_eq!(
            StatusCode::FORBIDDEN,
            relay_error(err).status(),
            "{err} must answer 403"
        );
    }

    assert_eq!(
        StatusCode::BAD_REQUEST,
        relay_error(RelayError::BadQuery).status()
    );
    assert_eq!(
        StatusCode::UNAUTHORIZED,
        relay_error(RelayError::InvalidToken).status()
    );
    assert_eq!(
        StatusCode::INTERNAL_SERVER_ERROR,
        relay_error(RelayError::InternalError).status()
    );
}

#[tokio::test]
async fn relay_put_rejects_a_capability_issued_for_another_key() {
    let tmp = tempfile::tempdir().unwrap();
    let tmp_root = tmp.path().canonicalize().unwrap();
    let storage_root = tmp_root.join("storage");
    let spool_dir = tmp_root.join("spool");
    tokio::fs::create_dir_all(&spool_dir).await.unwrap();
    let relay_secret = [7u8; 32];
    let cfg = upload_relay_test_config("upload", &storage_root, &spool_dir, &relay_secret);
    let token = relay_test_token("guild/a.bin", &relay_secret);
    let requested_key = "guild/b.bin";

    let response = relay_put(
        State(test_app_state(cfg)),
        Path(requested_key.to_owned()),
        Query(HashMap::from([("t".to_owned(), token)])),
        HeaderMap::new(),
        Request::builder()
            .method(Method::PUT)
            .body(Body::from(Bytes::from_static(b"not mine to write")))
            .unwrap(),
    )
    .await;

    assert_eq!(StatusCode::FORBIDDEN, response.status());
    assert!(
        response
            .headers()
            .contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN)
    );
    assert!(
        !tokio::fs::try_exists(storage_root.join("uploads").join(requested_key))
            .await
            .unwrap_or(false)
    );
}

fn content_length_headers(declared: u64) -> HeaderMap {
    let mut headers = HeaderMap::new();
    headers.insert(
        header::CONTENT_LENGTH,
        HeaderValue::from_str(&declared.to_string()).unwrap(),
    );
    headers
}

#[tokio::test]
async fn relay_put_never_forwards_an_injected_content_type_upstream() {
    let fake = crate::storage::tests::fake_s3().await;
    let tmp = tempfile::tempdir().unwrap();
    let relay_secret = [7u8; 32];
    let mut cfg = fake.config(tmp.path());
    cfg.mode = crate::config::DeploymentMode::Upload;
    cfg.socket_io_timeout_ms = 30_000;
    cfg.upload_relay.secret = SecretBytes::new(relay_secret.to_vec());
    cfg.upload_relay.max_body_bytes = 4096;
    let key = "guild/injected.bin";
    let token = encode_token(
        &TokenPayload {
            b: "uploads".to_owned(),
            k: key.to_owned(),
            m: TokenMethod::Put,
            u: None,
            p: None,
            ct: Some("image/png\r\nInjected: yes".to_owned()),
            mb: 4096,
            e: now_unix() + 60,
        },
        &relay_secret,
    )
    .unwrap();
    let body = Bytes::from_static(b"payload");

    let response = relay_put(
        State(test_app_state(cfg)),
        Path(key.to_owned()),
        Query(HashMap::from([("t".to_owned(), token)])),
        content_length_headers(body.len() as u64),
        Request::builder()
            .method(Method::PUT)
            .body(Body::from(body.clone()))
            .unwrap(),
    )
    .await;

    assert_eq!(StatusCode::OK, response.status());
    let (method, uri, headers, sent) = fake.last_request();
    assert_eq!(Method::PUT, method);
    assert_eq!("/uploads/guild/injected.bin", uri.path());
    assert_eq!(body.as_ref(), sent.as_ref());
    assert_eq!(
        "application/octet-stream",
        headers.get(header::CONTENT_TYPE).unwrap()
    );
    assert!(headers.get("injected").is_none());
    for value in headers.values() {
        assert!(!value.as_bytes().windows(8).any(|w| w == b"Injected"));
    }
    assert!(response.headers().get(header::ETAG).is_none());
}

use fluxer_common::secret::SecretBytes;
