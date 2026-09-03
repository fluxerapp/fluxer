// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{FakeObject, FakeS3, fake_s3, store};
use crate::{
    byte_budget::ByteBudget,
    range::ByteRange,
    response_body_limit,
    storage::{ObjectReadRequest, ObjectStreamRequest, StorageError},
};
use http::{Method, StatusCode, header};
use std::time::Duration;

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
    assert_eq!(64, head.identity.cache_identity().len());

    let ranged = store
        .stream_object("cdn", "a/b.txt", Some("bytes=6-10"))
        .await
        .unwrap();
    assert_eq!(StatusCode::PARTIAL_CONTENT, ranged.status);
    assert_eq!(Some(5), ranged.content_length);
    let body = axum::body::to_bytes(ranged.body, 16).await.unwrap();
    assert_eq!(b"world", &body[..]);

    let versioned = store
        .stream_object_limited(ObjectStreamRequest {
            bucket: "cdn",
            key: "a/b.txt",
            max_bytes: 1 << 20,
            byte_range: Some(ByteRange { start: 6, end: 10 }),
            expected_identity: &head.identity,
        })
        .await
        .unwrap();
    assert_eq!(StatusCode::PARTIAL_CONTENT, versioned.status);
    assert_eq!(Some(5), versioned.content_length);
    let body = axum::body::to_bytes(versioned.body, 16).await.unwrap();
    assert_eq!(b"world", &body[..]);
    assert_eq!(
        "\"v1\"",
        fake.last_request().2.get(header::IF_MATCH).unwrap()
    );
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
async fn a_truncated_get_body_fails_a_versioned_stream() {
    let fake = fake_s3().await;
    fake.put_object(
        "cdn/a/b.txt",
        FakeObject {
            body: b"hello world".to_vec(),
            content_type: Some("text/plain".to_owned()),
            ..FakeObject::default()
        },
    );
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));
    let head = store.head_object("cdn", "a/b.txt").await.unwrap();
    assert_eq!(11, head.content_length);

    fake.put_object(
        "cdn/a/b.txt",
        FakeObject {
            body: b"hell".to_vec(),
            head_length: Some(11),
            content_type: Some("text/plain".to_owned()),
            ..FakeObject::default()
        },
    );
    let truncated = store
        .stream_object_limited(ObjectStreamRequest {
            bucket: "cdn",
            key: "a/b.txt",
            max_bytes: 1 << 20,
            byte_range: None,
            expected_identity: &head.identity,
        })
        .await;
    assert!(matches!(truncated, Err(StorageError::ObjectChanged)));
}

#[tokio::test]
async fn a_mutated_object_between_head_and_get_yields_object_changed() {
    let fake = fake_s3().await;
    fake.put_object("cdn/a/b.txt", stored_object());
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));
    let head = store.head_object("cdn", "a/b.txt").await.unwrap();

    fake.put_object(
        "cdn/a/b.txt",
        FakeObject {
            body: b"replaced!!!".to_vec(),
            etag: Some("\"v2\"".to_owned()),
            content_type: Some("text/plain".to_owned()),
            last_modified: Some(LAST_MODIFIED.to_owned()),
            ..FakeObject::default()
        },
    );
    let budget = ByteBudget::new(1 << 20);
    let changed = store
        .read_object_versioned(ObjectReadRequest {
            bucket: "cdn",
            key: "a/b.txt",
            max_bytes: 1 << 20,
            budget: &budget,
            expected_identity: &head.identity,
        })
        .await;
    assert!(matches!(changed, Err(StorageError::ObjectChanged)));
    assert_eq!(
        "\"v1\"",
        fake.last_request().2.get(header::IF_MATCH).unwrap()
    );
}

#[tokio::test]
async fn concurrent_versioned_reads_share_one_upstream_get() {
    let fake = fake_s3().await;
    fake.put_object(
        "cdn/a/b.txt",
        FakeObject {
            delay: Some(Duration::from_millis(150)),
            ..stored_object()
        },
    );
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));
    let head = store.head_object("cdn", "a/b.txt").await.unwrap();

    let budget = ByteBudget::new(1 << 20);
    let request = || {
        store.read_object_versioned(ObjectReadRequest {
            bucket: "cdn",
            key: "a/b.txt",
            max_bytes: 1 << 20,
            budget: &budget,
            expected_identity: &head.identity,
        })
    };
    let (first, second) = tokio::join!(request(), request());
    assert_eq!(b"hello world", first.unwrap().data.as_ref());
    assert_eq!(b"hello world", second.unwrap().data.as_ref());
    let gets = fake
        .requests()
        .into_iter()
        .filter(|(method, ..)| *method == Method::GET)
        .count();
    assert_eq!(1, gets);
}

#[tokio::test]
async fn coalesced_versioned_readers_charge_the_byte_budget_for_one_buffer() {
    const PAYLOAD_BYTES: usize = 256 * 1024;
    const TRANSPORT_BYTES: usize = response_body_limit::RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX;
    let fake = fake_s3().await;
    fake.put_object(
        "cdn/a/big.bin",
        FakeObject {
            body: vec![7u8; PAYLOAD_BYTES],
            delay: Some(Duration::from_millis(150)),
            ..stored_object()
        },
    );
    let tmp = tempfile::tempdir().unwrap();
    let store = store(fake.config(tmp.path()));
    let head = store.head_object("cdn", "a/big.bin").await.unwrap();

    let budget = ByteBudget::new(TRANSPORT_BYTES + PAYLOAD_BYTES);
    let request = || {
        store.read_object_versioned(ObjectReadRequest {
            bucket: "cdn",
            key: "a/big.bin",
            max_bytes: 1 << 20,
            budget: &budget,
            expected_identity: &head.identity,
        })
    };
    let (first, second, third, fourth) = tokio::join!(request(), request(), request(), request());
    let objects = [
        first.expect("first reader"),
        second.expect("second reader"),
        third.expect("third reader"),
        fourth.expect("fourth reader"),
    ];
    for object in &objects {
        assert_eq!(PAYLOAD_BYTES, object.data.len());
        assert_eq!(objects[0].data.as_ptr(), object.data.as_ptr());
    }
    assert!(budget.try_reserve(TRANSPORT_BYTES).is_some());
    assert!(budget.try_reserve(TRANSPORT_BYTES + 1).is_none());
    drop(objects);
    assert!(
        budget
            .try_reserve(TRANSPORT_BYTES + PAYLOAD_BYTES)
            .is_some()
    );
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
    let head = store.head_object("cdn", "a/b.txt").await.unwrap();

    let ranged = store
        .stream_object_limited(ObjectStreamRequest {
            bucket: "cdn",
            key: "a/b.txt",
            max_bytes: 1 << 20,
            byte_range: Some(ByteRange { start: 0, end: 4 }),
            expected_identity: &head.identity,
        })
        .await
        .unwrap();
    assert_eq!(StatusCode::PARTIAL_CONTENT, ranged.status);
    assert_eq!(Some(5), ranged.content_length);
    let body = axum::body::to_bytes(ranged.body, 16).await.unwrap();
    assert_eq!(b"hello", &body[..]);
    assert_eq!(
        "bytes=0-4",
        fake.last_request().2.get(header::RANGE).unwrap()
    );

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

fn fake_gets(fake: &FakeS3, path: &str) -> usize {
    fake.requests()
        .iter()
        .filter(|(method, uri, ..)| *method == Method::GET && uri.path() == path)
        .count()
}
