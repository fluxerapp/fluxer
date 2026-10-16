// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{FakeObject, FakeS3, fake_s3, store};
use crate::{
    config::{BucketStyle, Config},
    storage::{RelayBody, RelayBodyChunks, RelayPutOptions, StorageError, Store},
};
use bytes::Bytes;
use http::{Method, header};
use std::path::Path;

const PAYLOAD: &[u8] = b"hello world, this is a payload.";

fn payload_object() -> FakeObject {
    FakeObject {
        body: PAYLOAD.to_vec(),
        content_type: Some("image/png".to_owned()),
        ..FakeObject::default()
    }
}

fn denied_object(status: u16) -> FakeObject {
    FakeObject {
        status: Some(status),
        ..FakeObject::default()
    }
}

fn fronted_config(origin: &FakeS3, cdn: &FakeS3, root: &Path) -> Config {
    let mut cfg = origin.config(root);
    cfg.storage.s3_read_endpoint = Some(cdn.endpoint().to_owned());
    cfg
}

fn only_request(fake: &FakeS3) -> super::CapturedRequest {
    let mut requests = fake.requests();
    assert_eq!(1, requests.len(), "expected exactly one captured request");
    requests.remove(0)
}

fn assert_unsigned(headers: &http::HeaderMap) {
    assert!(headers.get(header::AUTHORIZATION).is_none());
    assert!(headers.get("x-amz-date").is_none());
    assert!(headers.get("x-amz-content-sha256").is_none());
}

fn assert_signed(headers: &http::HeaderMap) {
    assert!(
        headers
            .get(header::AUTHORIZATION)
            .unwrap()
            .to_str()
            .unwrap()
            .starts_with("AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/")
    );
    assert!(headers.get("x-amz-date").is_some());
    assert!(headers.get("x-amz-content-sha256").is_some());
}

fn url_store(root: &Path, read_endpoint: &str, style: BucketStyle) -> Store {
    let mut cfg = super::test_config(root);
    cfg.storage.backend = crate::config::StorageBackend::S3;
    cfg.storage.s3_endpoint = "https://s3.example.test".to_owned();
    cfg.storage.s3_read_endpoint = Some(read_endpoint.to_owned());
    cfg.storage.s3_read_bucket_style = style;
    store(cfg)
}

#[tokio::test]
async fn read_without_read_endpoint_hits_s3_endpoint_signed() {
    let origin = fake_s3().await;
    origin.put_object("cdn/attachments/1/2/a.png", payload_object());
    let tmp = tempfile::tempdir().unwrap();
    let store = store(origin.config(tmp.path()));

    let object = store
        .read_object("cdn", "attachments/1/2/a.png")
        .await
        .unwrap();

    assert_eq!(PAYLOAD, object.data.as_ref());
    let (method, uri, headers, _) = only_request(&origin);
    assert_eq!(Method::GET, method);
    assert_eq!("/cdn/attachments/1/2/a.png", uri.path());
    assert_signed(&headers);
}

#[tokio::test]
async fn read_endpoint_routes_fronted_bucket_to_cdn_unsigned() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    cdn.put_object("cdn/attachments/1/2/a.png", payload_object());
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fronted_config(&origin, &cdn, tmp.path()));

    let object = store
        .read_object("cdn", "attachments/1/2/a.png")
        .await
        .unwrap();

    assert_eq!(PAYLOAD, object.data.as_ref());
    assert!(
        origin.requests().is_empty(),
        "S3 endpoint must not be touched"
    );
    let (method, uri, headers, _) = only_request(&cdn);
    assert_eq!(Method::GET, method);
    assert_eq!("/cdn/attachments/1/2/a.png", uri.path());
    assert_unsigned(&headers);
}

#[tokio::test]
async fn read_endpoint_rooted_style_omits_bucket_segment() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    cdn.put_object("attachments/1/2/a.png", payload_object());
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fronted_config(&origin, &cdn, tmp.path());
    cfg.storage.s3_read_bucket_style = BucketStyle::Rooted;
    let store = store(cfg);

    store
        .read_object("cdn", "attachments/1/2/a.png")
        .await
        .unwrap();

    let (_, uri, ..) = only_request(&cdn);
    assert_eq!("/attachments/1/2/a.png", uri.path());
}

#[test]
fn read_endpoint_virtual_style_uses_bucket_subdomain() {
    let tmp = tempfile::tempdir().unwrap();
    let store = url_store(
        tmp.path(),
        "https://cdn.example.net",
        BucketStyle::VirtualHosted,
    );

    assert_eq!(
        "https://cdn.cdn.example.net/attachments/1/2/a.png",
        store.s3_read_url("cdn", "attachments/1/2/a.png").unwrap()
    );
}

#[tokio::test]
async fn read_endpoint_never_redirects_other_buckets() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    origin.put_object("uploads/fresh-upload-key", payload_object());
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fronted_config(&origin, &cdn, tmp.path());
    cfg.storage.s3_read_bucket_style = BucketStyle::Rooted;
    let store = store(cfg);

    store
        .read_object("uploads", "fresh-upload-key")
        .await
        .unwrap();

    assert!(cdn.requests().is_empty(), "uploads must not hit the CDN");
    let (_, uri, headers, _) = only_request(&origin);
    assert_eq!("/uploads/fresh-upload-key", uri.path());
    assert_signed(&headers);
}

#[tokio::test]
async fn read_endpoint_never_affects_writes() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fronted_config(&origin, &cdn, tmp.path());
    cfg.storage.s3_read_bucket = "uploads".to_owned();
    let store = store(cfg);

    let body: RelayBodyChunks = Box::pin(futures_util::stream::iter(vec![Ok(Bytes::from_static(
        b"body",
    ))]));
    store
        .relay_put_object(
            "uploads",
            "guild/x.bin",
            RelayPutOptions {
                body: RelayBody::Streamed(body),
                content_length: 4,
                content_type: Some("application/octet-stream".to_owned()),
                upload_id: None,
                part_number: None,
                timeout_ms: 5_000,
            },
        )
        .await
        .unwrap();

    assert!(cdn.requests().is_empty(), "writes must not hit the CDN");
    let (method, uri, headers, _) = only_request(&origin);
    assert_eq!(Method::PUT, method);
    assert_eq!("/uploads/guild/x.bin", uri.path());
    assert_signed(&headers);
}

#[tokio::test]
async fn read_endpoint_signs_when_read_signed_enabled() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    cdn.put_object("cdn/a.png", payload_object());
    let cdn_host = cdn.endpoint().trim_start_matches("http://").to_owned();
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fronted_config(&origin, &cdn, tmp.path());
    cfg.storage.s3_read_signed = true;
    let store = store(cfg);

    store.read_object("cdn", "a.png").await.unwrap();

    let (_, _, headers, _) = only_request(&cdn);
    assert_signed(&headers);
    assert_eq!(
        cdn_host,
        headers.get(header::HOST).unwrap().to_str().unwrap()
    );
}

#[tokio::test]
async fn unsigned_read_still_sends_range_header() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    cdn.put_object("cdn/video.mp4", payload_object());
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fronted_config(&origin, &cdn, tmp.path()));

    store
        .stream_object("cdn", "video.mp4", Some("bytes=10-19"))
        .await
        .unwrap();

    let (_, _, headers, _) = only_request(&cdn);
    assert_eq!("bytes=10-19", headers.get(header::RANGE).unwrap());
    assert_unsigned(&headers);
}

#[tokio::test]
async fn head_object_uses_the_read_endpoint_like_every_other_read() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    cdn.put_object("a.png", payload_object());
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fronted_config(&origin, &cdn, tmp.path());
    cfg.storage.s3_read_bucket_style = BucketStyle::Rooted;
    let store = store(cfg);

    store.head_object("cdn", "a.png").await.unwrap();

    // A HEAD is a read: routing it to the write origin would bypass the configured read
    // endpoint and attach credentials to a read the operator configured as unsigned.
    assert!(
        origin.requests().is_empty(),
        "HEAD must not hit the write origin"
    );
    let (method, uri, headers, _) = only_request(&cdn);
    assert_eq!(Method::HEAD, method);
    assert_eq!("/a.png", uri.path());
    assert_unsigned(&headers);
}

#[tokio::test]
async fn head_and_body_reads_both_use_the_read_endpoint() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    cdn.put_object("video.mp4", payload_object());
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fronted_config(&origin, &cdn, tmp.path());
    cfg.storage.s3_read_bucket_style = BucketStyle::Rooted;
    let store = store(cfg);

    store.head_object("cdn", "video.mp4").await.unwrap();
    store
        .stream_object("cdn", "video.mp4", Some("bytes=0-3"))
        .await
        .unwrap();

    assert!(
        origin.requests().is_empty(),
        "neither read may reach the write origin"
    );
    let cdn_requests = cdn.requests();
    assert_eq!(2, cdn_requests.len());
    assert_eq!(Method::HEAD, cdn_requests[0].0);
    assert_eq!(Method::GET, cdn_requests[1].0);
    assert_eq!("bytes=0-3", cdn_requests[1].2.get(header::RANGE).unwrap());
}

#[tokio::test]
async fn unsigned_read_works_without_credentials() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    cdn.put_object("cdn/a.png", payload_object());
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fronted_config(&origin, &cdn, tmp.path());
    cfg.storage.s3_access_key_id = String::new();
    cfg.storage.s3_secret_access_key = String::new();
    let store = store(cfg);

    store.read_object("cdn", "a.png").await.unwrap();

    let (_, _, headers, _) = only_request(&cdn);
    assert_unsigned(&headers);
}

#[test]
fn read_url_matches_write_url_shape_for_encoding_and_trailing_slash() {
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = super::test_config(tmp.path());
    cfg.storage.backend = crate::config::StorageBackend::S3;
    cfg.storage.s3_endpoint = "https://s3.example.test/".to_owned();
    cfg.storage.s3_read_endpoint = Some("https://cdn.example.net/".to_owned());
    let store = store(cfg);
    let key = "attachments/1/2/na me+ü.png";

    assert_eq!(
        "https://s3.example.test/uploads/attachments/1/2/na%20me%2B%C3%BC.png",
        store.s3_url("uploads", key).unwrap()
    );
    assert_eq!(
        "https://cdn.example.net/cdn/attachments/1/2/na%20me%2B%C3%BC.png",
        store.s3_read_url("cdn", key).unwrap()
    );
}

#[test]
fn read_url_rejects_unsafe_keys_and_buckets() {
    let tmp = tempfile::tempdir().unwrap();
    let store = url_store(tmp.path(), "https://cdn.example.net", BucketStyle::Rooted);

    assert!(store.s3_read_url("cdn", "../escape").is_err());
    assert!(store.s3_read_url("cdn", "/leading").is_err());
}

#[test]
fn read_url_validates_bucket_inside_the_read_endpoint_branch() {
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = super::test_config(tmp.path());
    cfg.storage.backend = crate::config::StorageBackend::S3;
    cfg.storage.s3_endpoint = "https://s3.example.test".to_owned();
    cfg.storage.s3_read_endpoint = Some("https://cdn.example.net".to_owned());
    cfg.storage.s3_read_bucket = "..".to_owned();
    cfg.storage.s3_read_bucket_style = BucketStyle::Rooted;
    let store = store(cfg);

    assert!(store.read_endpoint_for("..").is_some());
    assert!(store.s3_read_url("..", "a.png").is_err());
}

#[tokio::test]
async fn unsigned_cdn_read_treats_403_as_not_found() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    cdn.put_object("avatars/1/hash", denied_object(403));
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fronted_config(&origin, &cdn, tmp.path());
    cfg.storage.s3_read_bucket_style = BucketStyle::Rooted;
    let store = store(cfg);

    let err = store
        .read_object("cdn", "avatars/1/hash")
        .await
        .unwrap_err();
    assert!(matches!(err, StorageError::NotFound), "got {err:?}");
    assert_eq!(1, cdn.requests().len());

    let stream = store.stream_object("cdn", "avatars/1/hash", None).await;
    assert!(
        matches!(stream, Err(StorageError::NotFound)),
        "stream_object should map 403 to NotFound too"
    );
}

#[tokio::test]
async fn signed_cdn_read_keeps_403_as_an_error() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    cdn.put_object("cdn/avatars/1/hash", denied_object(403));
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fronted_config(&origin, &cdn, tmp.path());
    cfg.storage.s3_read_signed = true;
    let store = store(cfg);

    let err = store
        .read_object("cdn", "avatars/1/hash")
        .await
        .unwrap_err();
    assert!(!matches!(err, StorageError::NotFound), "got {err:?}");
}

#[tokio::test]
async fn origin_read_keeps_403_as_an_error() {
    let origin = fake_s3().await;
    origin.put_object("cdn/avatars/1/hash", denied_object(403));
    let tmp = tempfile::tempdir().unwrap();
    let store = store(origin.config(tmp.path()));

    let err = store
        .read_object("cdn", "avatars/1/hash")
        .await
        .unwrap_err();
    assert!(!matches!(err, StorageError::NotFound), "got {err:?}");
}

#[tokio::test]
async fn unfronted_bucket_keeps_403_as_an_error() {
    let origin = fake_s3().await;
    let cdn = fake_s3().await;
    origin.put_object("uploads/fresh", denied_object(403));
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fronted_config(&origin, &cdn, tmp.path()));

    let err = store.read_object("uploads", "fresh").await.unwrap_err();
    assert!(!matches!(err, StorageError::NotFound), "got {err:?}");
    assert!(cdn.requests().is_empty());
}
