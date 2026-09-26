// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{rendered_counter, store, store_with_storage_metrics, test_config};
use crate::{
    metrics::Metrics,
    storage::{ObjectStreamRequest, StorageError},
};
use http::StatusCode;

#[tokio::test]
async fn local_write_read_head_roundtrip() {
    let tmp = tempfile::tempdir().unwrap();
    let store = store(test_config(&tmp.path().canonicalize().unwrap()));
    store
        .write_object("cdn", "a/b.txt", b"hello", "text/plain")
        .await
        .unwrap();
    let head = store.head_object("cdn", "a/b.txt").await.unwrap();
    assert_eq!(5, head.content_length);
    let object = store.read_object("cdn", "a/b.txt").await.unwrap();
    assert_eq!(b"hello", &object.data[..]);
}

#[tokio::test]
async fn local_stream_honors_range_without_buffered_read() {
    let tmp = tempfile::tempdir().unwrap();
    let store = store(test_config(&tmp.path().canonicalize().unwrap()));
    store
        .write_object("cdn", "a/b.txt", b"hello world", "text/plain")
        .await
        .unwrap();
    let object = store
        .stream_object("cdn", "a/b.txt", Some("bytes=6-10"))
        .await
        .unwrap();
    assert_eq!(StatusCode::PARTIAL_CONTENT, object.status);
    assert_eq!(Some(5), object.content_length);
    let body = axum::body::to_bytes(object.body, 16).await.unwrap();
    assert_eq!(b"world", &body[..]);
}

#[tokio::test]
async fn local_versioned_reads_reject_an_object_rewritten_after_its_head() {
    let tmp = tempfile::tempdir().unwrap();
    let store = store(test_config(&tmp.path().canonicalize().unwrap()));
    store
        .write_object("cdn", "a/b.txt", b"hello world", "text/plain")
        .await
        .unwrap();
    let head = store.head_object("cdn", "a/b.txt").await.unwrap();
    let streamed = store
        .stream_object_limited(ObjectStreamRequest {
            bucket: "cdn",
            key: "a/b.txt",
            max_bytes: 1 << 20,
            byte_range: None,
            expected_identity: &head.identity,
        })
        .await
        .unwrap();
    assert_eq!(Some(11), streamed.content_length);

    store
        .write_object("cdn", "a/b.txt", b"goodbye world", "text/plain")
        .await
        .unwrap();
    let changed = store
        .stream_object_limited(ObjectStreamRequest {
            bucket: "cdn",
            key: "a/b.txt",
            max_bytes: 1 << 20,
            byte_range: None,
            expected_identity: &head.identity,
        })
        .await;
    assert!(matches!(changed, Err(StorageError::ObjectChanged)));
}

#[tokio::test]
async fn local_reads_refuse_to_follow_a_symlinked_key() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().canonicalize().unwrap();
    let store = store(test_config(&root));
    store
        .write_object("cdn", "a/b.txt", b"hello", "text/plain")
        .await
        .unwrap();
    std::os::unix::fs::symlink(root.join("cdn/a/b.txt"), root.join("cdn/a/link.txt")).unwrap();
    assert!(matches!(
        store.read_object("cdn", "a/link.txt").await,
        Err(StorageError::InvalidKey)
    ));
}

#[tokio::test]
async fn head_object_leaves_the_storage_counters_untouched() {
    let tmp = tempfile::tempdir().unwrap();
    let metrics = Metrics::new();
    let store = store_with_storage_metrics(
        test_config(&tmp.path().canonicalize().unwrap()),
        metrics.storage(),
    );
    store
        .write_object("cdn", "a/b.txt", b"hello", "text/plain")
        .await
        .unwrap();

    store.head_object("cdn", "a/b.txt").await.unwrap();
    assert!(store.head_object("cdn", "missing.txt").await.is_err());
    assert_eq!(
        0,
        rendered_counter(&metrics, "fluxer_media_proxy_storage_hits_total")
    );
    assert_eq!(
        0,
        rendered_counter(&metrics, "fluxer_media_proxy_storage_misses_total")
    );
    assert_eq!(
        0,
        rendered_counter(&metrics, "fluxer_media_proxy_storage_errors_total")
    );

    store.read_object("cdn", "a/b.txt").await.unwrap();
    assert_eq!(
        1,
        rendered_counter(&metrics, "fluxer_media_proxy_storage_hits_total")
    );
    assert!(matches!(
        store.read_object("cdn", "missing.txt").await,
        Err(StorageError::NotFound)
    ));
    assert_eq!(
        1,
        rendered_counter(&metrics, "fluxer_media_proxy_storage_misses_total")
    );
    assert_eq!(
        0,
        rendered_counter(&metrics, "fluxer_media_proxy_storage_errors_total")
    );
}
