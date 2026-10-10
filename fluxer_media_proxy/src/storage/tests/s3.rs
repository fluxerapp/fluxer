// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{FakeObject, FakeS3, connection_dropping_front, fake_s3, store};
use crate::storage::StorageError;
use http::{Method, StatusCode, header};
use std::sync::atomic::Ordering;

const LAST_MODIFIED: &str = "Wed, 21 Oct 2015 07:28:00 GMT";

fn stored_object() -> FakeObject {
    FakeObject {
        body: b"hello world".to_vec(),
        etag: Some("\"v1\"".to_owned()),
        content_type: Some("text/plain".to_owned()),
        last_modified: Some(LAST_MODIFIED.to_owned()),
        ..FakeObject::default()
    }
}

#[tokio::test]
async fn head_and_ranged_get_match_the_stored_object() {
    let fake = fake_s3().await;
    fake.put_object("cdn/a/b.txt", stored_object());
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));

    let head = store.head_object("cdn", "a/b.txt").await.unwrap();
    assert_eq!(11, head.content_length);
    assert_eq!("text/plain", head.content_type);

    let ranged = store
        .stream_object("cdn", "a/b.txt", Some("bytes=6-10"))
        .await
        .unwrap();
    assert_eq!(StatusCode::PARTIAL_CONTENT, ranged.status);
    assert_eq!(Some(5), ranged.content_length);
    let body = axum::body::to_bytes(ranged.body, 16).await.unwrap();
    assert_eq!(b"world", &body[..]);
}

#[tokio::test]
async fn missing_and_failing_objects_map_to_storage_errors() {
    let fake = fake_s3().await;
    fake.put_object(
        "cdn/broken.txt",
        FakeObject {
            status: Some(500),
            ..FakeObject::default()
        },
    );
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));

    assert!(matches!(
        store.read_object("cdn", "gone.txt").await,
        Err(StorageError::NotFound)
    ));
    assert!(matches!(
        store.head_object("cdn", "gone.txt").await,
        Err(StorageError::NotFound)
    ));
    let failure = store.read_object("cdn", "broken.txt").await;
    assert!(matches!(failure, Err(StorageError::S3(_))));
}

#[tokio::test]
async fn a_transient_origin_status_is_retried_while_a_missing_object_is_not() {
    let fake = fake_s3().await;
    fake.put_object(
        "cdn/broken.txt",
        FakeObject {
            status: Some(500),
            ..FakeObject::default()
        },
    );
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));

    assert!(matches!(
        store.read_object("cdn", "broken.txt").await,
        Err(StorageError::S3(_))
    ));
    assert_eq!(3, fake_gets(&fake, "/cdn/broken.txt"));

    assert!(matches!(
        store.read_object("cdn", "gone.txt").await,
        Err(StorageError::NotFound)
    ));
    assert_eq!(1, fake_gets(&fake, "/cdn/gone.txt"));

    assert!(matches!(
        store.head_object("cdn", "broken.txt").await,
        Err(StorageError::S3(_))
    ));
    assert_eq!(
        3,
        fake.requests()
            .iter()
            .filter(|(method, uri, ..)| *method == Method::HEAD && uri.path() == "/cdn/broken.txt")
            .count()
    );
}

#[tokio::test]
async fn a_ranged_read_asks_the_origin_for_exactly_the_requested_bytes() {
    let fake = fake_s3().await;
    fake.put_object("cdn/a/b.txt", stored_object());
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));

    let suffix = store
        .stream_object("cdn", "a/b.txt", Some("bytes=6-10"))
        .await
        .unwrap();
    assert_eq!(Some(5), suffix.content_length);
    assert_eq!(
        "bytes=6-10",
        fake.last_request().2.get(header::RANGE).unwrap()
    );
}

#[tokio::test]
async fn stream_s3_surfaces_an_upstream_416_instead_of_a_storage_error() {
    // A range past the end of the object is a routine thing for a video player to ask for while
    // seeking. Mapping the upstream 416 onto a storage error would answer the client 502.
    let fake = fake_s3().await;
    fake.put_object(
        "cdn/video.mp4",
        FakeObject {
            read_status: Some(416),
            ..stored_object()
        },
    );
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));

    let object = store
        .stream_object("cdn", "video.mp4", Some("bytes=900-999"))
        .await
        .expect("an upstream 416 is an answer about the range, not a storage failure");

    assert_eq!(StatusCode::RANGE_NOT_SATISFIABLE, object.status);
    assert_eq!(None, object.byte_range);
    assert_eq!(Some(0), object.content_length);
}

#[tokio::test]
async fn a_read_on_a_dropped_connection_is_retried_on_a_new_one() {
    let fake = fake_s3().await;
    fake.put_object("cdn/a/b.txt", stored_object());
    let (endpoint, accepted) = connection_dropping_front(fake.endpoint(), 1).await;
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fake.config(tmp.path());
    cfg.storage.s3_endpoint = endpoint;
    let store = store(cfg);

    let object = store.read_object("cdn", "a/b.txt").await.unwrap();

    assert_eq!(b"hello world", &object.data[..]);
    assert_eq!(2, accepted.load(Ordering::SeqCst));
    assert_eq!(1, fake_gets(&fake, "/cdn/a/b.txt"));
}

#[tokio::test]
async fn a_read_that_keeps_losing_its_connection_reports_the_cause() {
    let fake = fake_s3().await;
    fake.put_object("cdn/a/b.txt", stored_object());
    let (endpoint, accepted) = connection_dropping_front(fake.endpoint(), usize::MAX).await;
    let tmp = tempfile::tempdir().unwrap();
    let mut cfg = fake.config(tmp.path());
    cfg.storage.s3_endpoint = endpoint;
    let store = store(cfg);

    let error = store
        .head_object("cdn", "a/b.txt")
        .await
        .expect_err("every connection to the origin is dropped");

    assert!(
        error
            .to_string()
            .contains("connection closed before message completed"),
        "{error}"
    );
    assert_eq!(3, accepted.load(Ordering::SeqCst));
    assert!(fake.requests().is_empty());
}

fn fake_gets(fake: &FakeS3, path: &str) -> usize {
    fake.requests()
        .iter()
        .filter(|(method, uri, ..)| *method == Method::GET && uri.path() == path)
        .count()
}
