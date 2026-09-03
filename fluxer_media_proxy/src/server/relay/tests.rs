// SPDX-License-Identifier: AGPL-3.0-or-later

use super::body::{
    RelayBodyStream, RelayBodyStreamRequest, relay_etag, validate_completed_relay_body,
};
use crate::{
    config::Config,
    secret::SecretBytes,
    server::{
        routes::relay::{relay_cors, relay_put},
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
use std::{
    collections::HashMap,
    sync::{Arc, OnceLock},
    time::Duration,
};

fn body_stream(body: Body, declared_length: u64) -> (RelayBodyStream, Arc<OnceLock<RelayError>>) {
    let failure = Arc::new(OnceLock::new());
    let stream = RelayBodyStream::new(RelayBodyStreamRequest {
        body,
        declared_length,
        deadline: tokio::time::Instant::now() + Duration::from_secs(5),
        failure: Arc::clone(&failure),
    });
    (stream, failure)
}

#[tokio::test]
async fn relay_body_stream_splits_frames_and_counts_every_relayed_byte() {
    let input = Bytes::from(vec![7; 70 * 1024]);
    let declared = input.len() as u64;
    let (mut stream, failure) = body_stream(Body::from(input.clone()), declared);
    let progress = stream.progress();
    let mut output = Vec::new();
    while let Some((frame, next)) = stream.next().await {
        let frame = frame.expect("relay frame");
        assert!(frame.len() <= 64 * 1024);
        output.extend_from_slice(&frame);
        stream = next;
    }

    assert_eq!(output, input);
    validate_completed_relay_body(&failure, &progress, declared).expect("completed body");
    assert_eq!(
        validate_completed_relay_body(&failure, &progress, declared + 1),
        Err(RelayError::ClientUploadFailed)
    );
}

#[tokio::test]
async fn relay_body_stream_rejects_short_and_long_bodies() {
    let (stream, failure) = body_stream(Body::from("abc"), 4);
    let progress = stream.progress();
    let Some((first, stream)) = stream.next().await else {
        panic!("short body omitted its data frame")
    };
    assert_eq!(first.expect("first frame"), Bytes::from_static(b"abc"));
    let Some((error, terminal)) = stream.next().await else {
        panic!("short body did not report an error")
    };
    assert_eq!(
        error.expect_err("short body error").kind(),
        std::io::ErrorKind::UnexpectedEof
    );
    assert_eq!(failure.get(), Some(&RelayError::ClientUploadFailed));
    assert_eq!(
        validate_completed_relay_body(&failure, &progress, 4),
        Err(RelayError::ClientUploadFailed)
    );
    assert!(terminal.next().await.is_none());

    let (stream, failure) = body_stream(Body::from("abc"), 2);
    let progress = stream.progress();
    let Some((error, terminal)) = stream.next().await else {
        panic!("long body did not report an error")
    };
    assert_eq!(
        error.expect_err("long body error").kind(),
        std::io::ErrorKind::InvalidData
    );
    assert_eq!(failure.get(), Some(&RelayError::PayloadTooLarge));
    assert_eq!(
        validate_completed_relay_body(&failure, &progress, 2),
        Err(RelayError::PayloadTooLarge)
    );
    assert!(terminal.next().await.is_none());
}

#[tokio::test]
async fn relay_body_stream_handles_empty_body_and_elapsed_total_deadline() {
    let (stream, failure) = body_stream(Body::empty(), 0);
    let progress = stream.progress();
    assert!(stream.next().await.is_none());
    validate_completed_relay_body(&failure, &progress, 0).expect("empty body");

    let failure = Arc::new(OnceLock::new());
    let stream = RelayBodyStream::new(RelayBodyStreamRequest {
        body: Body::from("a"),
        declared_length: 1,
        deadline: tokio::time::Instant::now(),
        failure: Arc::clone(&failure),
    });
    let Some((error, terminal)) = stream.next().await else {
        panic!("elapsed deadline did not report an error")
    };
    assert_eq!(
        error.expect_err("deadline error").kind(),
        std::io::ErrorKind::TimedOut
    );
    assert_eq!(failure.get(), Some(&RelayError::ClientUploadFailed));
    assert!(terminal.next().await.is_none());
}

#[tokio::test]
async fn relay_body_stream_waits_out_a_client_stall_inside_the_total_budget() {
    let payload = Bytes::from_static(b"resumed after a stall");
    let declared = payload.len() as u64;
    let stalled = payload.clone();
    let (mut stream, failure) = body_stream(
        Body::from_stream(futures_util::stream::once(async move {
            tokio::time::sleep(Duration::from_millis(1_500)).await;
            Ok::<Bytes, std::io::Error>(stalled)
        })),
        declared,
    );
    let progress = stream.progress();
    let mut output = Vec::new();
    while let Some((frame, next)) = stream.next().await {
        output.extend_from_slice(&frame.expect("stalled client frame"));
        stream = next;
    }

    assert_eq!(output, payload);
    assert!(failure.get().is_none());
    validate_completed_relay_body(&failure, &progress, declared).expect("stalled body");
}

fn upload_relay_test_config(
    storage_root: &std::path::Path,
    spool_dir: &std::path::Path,
    relay_secret: &[u8],
) -> Config {
    Config::load_from_iter([
        (
            "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
            "secret".to_owned(),
        ),
        ("FLUXER_MEDIA_PROXY_MODE".to_owned(), "upload".to_owned()),
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

#[test]
fn relay_cors_allows_client_context_headers() {
    let mut headers = HeaderMap::new();
    relay_cors(&mut headers);
    let allow_headers = headers
        .get(header::ACCESS_CONTROL_ALLOW_HEADERS)
        .unwrap()
        .to_str()
        .unwrap();

    assert!(
        allow_headers
            .split(',')
            .any(|name| name.trim().eq_ignore_ascii_case("x-fluxer-features"))
    );
    assert!(
        allow_headers
            .split(',')
            .any(|name| name.trim().eq_ignore_ascii_case("x-client-context"))
    );
}

#[tokio::test]
async fn relay_put_accepts_unknown_content_length_body() {
    let tmp = tempfile::tempdir().unwrap();
    let tmp_root = tmp.path().canonicalize().unwrap();
    let storage_root = tmp_root.join("storage");
    let spool_dir = tmp_root.join("spool");
    tokio::fs::create_dir_all(&spool_dir).await.unwrap();
    let relay_secret = [7u8; 32];
    let cfg = upload_relay_test_config(&storage_root, &spool_dir, &relay_secret);
    let key = "guild/diagnostics.txt";
    let token = encode_token(
        &TokenPayload {
            b: "uploads".to_owned(),
            k: key.to_owned(),
            m: TokenMethod::Put,
            u: None,
            p: None,
            ct: Some("text/plain".to_owned()),
            mb: 4096,
            e: now_unix() + 60,
        },
        &relay_secret,
    )
    .unwrap();
    let body = Bytes::from_static(b"diagnostics bundle");
    let request = Request::builder()
        .method(Method::PUT)
        .body(Body::from(body.clone()))
        .unwrap();
    assert!(request.headers().get(header::CONTENT_LENGTH).is_none());

    let response = relay_put(
        State(test_app_state(cfg)),
        Path(key.to_owned()),
        Query(HashMap::from([("t".to_owned(), token)])),
        HeaderMap::new(),
        request,
    )
    .await;

    assert_eq!(StatusCode::OK, response.status());
    let stored = tokio::fs::read(storage_root.join("uploads").join(key))
        .await
        .unwrap();
    assert_eq!(body.as_ref(), stored.as_slice());
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

fn content_length_headers(declared: u64) -> HeaderMap {
    let mut headers = HeaderMap::new();
    headers.insert(
        header::CONTENT_LENGTH,
        HeaderValue::from_str(&declared.to_string()).unwrap(),
    );
    headers
}

#[tokio::test]
async fn relay_put_streams_known_length_body_without_spooling() {
    let tmp = tempfile::tempdir().unwrap();
    let tmp_root = tmp.path().canonicalize().unwrap();
    let storage_root = tmp_root.join("storage");
    let spool_dir = tmp_root.join("spool");
    tokio::fs::create_dir_all(&spool_dir).await.unwrap();
    let relay_secret = [7u8; 32];
    let cfg = upload_relay_test_config(&storage_root, &spool_dir, &relay_secret);
    let key = "guild/streamed.bin";
    let token = relay_test_token(key, &relay_secret);
    let body = Bytes::from_static(b"streamed straight through");

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
    let stored = tokio::fs::read(storage_root.join("uploads").join(key))
        .await
        .unwrap();
    assert_eq!(body.as_ref(), stored.as_slice());
    let mut spool_entries = tokio::fs::read_dir(&spool_dir).await.unwrap();
    assert!(spool_entries.next_entry().await.unwrap().is_none());
}

#[tokio::test]
async fn relay_put_rejects_streaming_body_longer_than_declared() {
    let tmp = tempfile::tempdir().unwrap();
    let tmp_root = tmp.path().canonicalize().unwrap();
    let storage_root = tmp_root.join("storage");
    let spool_dir = tmp_root.join("spool");
    tokio::fs::create_dir_all(&spool_dir).await.unwrap();
    let relay_secret = [7u8; 32];
    let cfg = upload_relay_test_config(&storage_root, &spool_dir, &relay_secret);
    let key = "guild/overrun.bin";
    let token = relay_test_token(key, &relay_secret);

    let response = relay_put(
        State(test_app_state(cfg)),
        Path(key.to_owned()),
        Query(HashMap::from([("t".to_owned(), token)])),
        content_length_headers(4),
        Request::builder()
            .method(Method::PUT)
            .body(Body::from(Bytes::from_static(b"way past four bytes")))
            .unwrap(),
    )
    .await;

    assert_eq!(StatusCode::PAYLOAD_TOO_LARGE, response.status());
    assert!(
        tokio::fs::try_exists(storage_root.join("uploads").join(key))
            .await
            .ok()
            != Some(true)
    );
}

#[tokio::test]
async fn relay_put_rejects_streaming_body_shorter_than_declared() {
    let tmp = tempfile::tempdir().unwrap();
    let tmp_root = tmp.path().canonicalize().unwrap();
    let storage_root = tmp_root.join("storage");
    let spool_dir = tmp_root.join("spool");
    tokio::fs::create_dir_all(&spool_dir).await.unwrap();
    let relay_secret = [7u8; 32];
    let cfg = upload_relay_test_config(&storage_root, &spool_dir, &relay_secret);
    let key = "guild/short.bin";
    let token = relay_test_token(key, &relay_secret);

    let response = relay_put(
        State(test_app_state(cfg)),
        Path(key.to_owned()),
        Query(HashMap::from([("t".to_owned(), token)])),
        content_length_headers(32),
        Request::builder()
            .method(Method::PUT)
            .body(Body::from(Bytes::from_static(b"tiny")))
            .unwrap(),
    )
    .await;

    assert_eq!(StatusCode::BAD_REQUEST, response.status());
    assert!(
        tokio::fs::try_exists(storage_root.join("uploads").join(key))
            .await
            .ok()
            != Some(true)
    );
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

#[tokio::test]
async fn relay_put_returns_ok_for_a_malformed_upstream_etag() {
    for upstream_etag in [
        "unquoted-etag-123".to_owned(),
        format!("\"{}\"", "a".repeat(300)),
    ] {
        let fake = crate::storage::tests::fake_s3().await;
        fake.set_put_etag(&upstream_etag);
        let tmp = tempfile::tempdir().unwrap();
        let relay_secret = [7u8; 32];
        let mut cfg = fake.config(tmp.path());
        cfg.mode = crate::config::DeploymentMode::Upload;
        cfg.socket_io_timeout_ms = 30_000;
        cfg.upload_relay.secret = SecretBytes::new(relay_secret.to_vec());
        cfg.upload_relay.max_body_bytes = 4096;
        let key = "guild/malformed-etag.bin";
        let token = relay_test_token(key, &relay_secret);
        let body = Bytes::from_static(b"committed upload bytes");

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
        assert_eq!(body.as_ref(), fake.last_request().3.as_ref());
        assert_eq!(
            upstream_etag.as_str(),
            response
                .headers()
                .get(header::ETAG)
                .unwrap()
                .to_str()
                .unwrap()
        );
    }
}

#[test]
fn relay_etag_degrades_an_unrepresentable_upstream_value_to_an_empty_header() {
    assert_eq!("", relay_etag("\"broken\u{7f}tag\"").to_str().unwrap());
    assert_eq!("", relay_etag("\"broken\ntag\"").to_str().unwrap());
    assert_eq!("W/\"weak\"", relay_etag("W/\"weak\"").to_str().unwrap());
    assert_eq!("\"etag-123\"", relay_etag("\"etag-123\"").to_str().unwrap());
}

#[tokio::test]
async fn relay_put_omits_the_etag_when_the_store_returns_none() {
    let tmp = tempfile::tempdir().unwrap();
    let tmp_root = tmp.path().canonicalize().unwrap();
    let storage_root = tmp_root.join("storage");
    let spool_dir = tmp_root.join("spool");
    tokio::fs::create_dir_all(&spool_dir).await.unwrap();
    let relay_secret = [7u8; 32];
    let cfg = upload_relay_test_config(&storage_root, &spool_dir, &relay_secret);
    let key = "guild/no-upstream-etag.bin";
    let token = relay_test_token(key, &relay_secret);
    let body = Bytes::from_static(b"local backend bytes");

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
    assert!(response.headers().get(header::ETAG).is_none());
    let stored = tokio::fs::read(storage_root.join("uploads").join(key))
        .await
        .unwrap();
    assert_eq!(body.as_ref(), stored.as_slice());
}
