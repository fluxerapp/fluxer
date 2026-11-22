// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{FakeObject, FakeS3, fake_s3};
use crate::{
    config::{Config, DualStoreConfig, StoreSide},
    metrics::{http_client::HTTPClientMetrics, storage::StorageMetrics},
    storage::{StorageError, Store},
};
use std::{path::Path, sync::Arc};

fn object(body: &[u8]) -> FakeObject {
    FakeObject {
        body: body.to_vec(),
        content_type: Some("image/png".to_owned()),
        ..FakeObject::default()
    }
}

fn dual_config(primary: &FakeS3, fallback: &FakeS3, root: &Path, reads_enabled: bool) -> Config {
    let mut cfg = primary.config(root);
    cfg.storage.store_id = "store-b".to_owned();
    cfg.storage.bucket_cdn = "media-new".to_owned();
    cfg.storage.s3_read_bucket = "media-new".to_owned();
    cfg.storage.bucket_uploads = "uploads-new".to_owned();
    let fallback_cfg = fallback.config(root);
    cfg.storage.dual_store = Some(DualStoreConfig {
        store_id: "store-a".to_owned(),
        endpoint: fallback_cfg.storage.s3_endpoint,
        region: "region-a".to_owned(),
        access_key_id: fallback_cfg.storage.s3_access_key_id,
        secret_access_key: fallback_cfg.storage.s3_secret_access_key,
        force_path_style: true,
        reads_enabled,
        bucket_cdn: "media".to_owned(),
        bucket_uploads: "uploads".to_owned(),
        bucket_static: "static".to_owned(),
    });
    cfg
}

fn store(cfg: Config, metrics: &Arc<StorageMetrics>) -> Store {
    Store::try_new(
        cfg,
        Arc::clone(metrics),
        Arc::new(HTTPClientMetrics::default()),
    )
    .unwrap()
}

#[tokio::test]
async fn primary_hit_never_asks_the_fallback() {
    let dir = tempfile::tempdir().unwrap();
    let (primary, fallback) = (fake_s3().await, fake_s3().await);
    primary.put_object("media-new/a.png", object(b"new"));
    fallback.put_object("media/a.png", object(b"old"));
    let metrics = Arc::new(StorageMetrics::default());
    let store = store(dual_config(&primary, &fallback, dir.path(), true), &metrics);
    let read = store.read_object("media-new", "a.png").await.unwrap();
    assert_eq!(&read.data[..], b"new");
    assert_eq!(0, metrics.fallback_hits.get());
}

#[tokio::test]
async fn primary_miss_reads_the_fallback_under_its_own_bucket_name() {
    let dir = tempfile::tempdir().unwrap();
    let (primary, fallback) = (fake_s3().await, fake_s3().await);
    fallback.put_object("media/old.png", object(b"old"));
    let metrics = Arc::new(StorageMetrics::default());
    let store = store(dual_config(&primary, &fallback, dir.path(), true), &metrics);
    let read = store.read_object("media-new", "old.png").await.unwrap();
    assert_eq!(&read.data[..], b"old");
    let head = store.head_object("media-new", "old.png").await.unwrap();
    assert_eq!(3, head.content_length);
    let streamed = store
        .stream_object("media-new", "old.png", None)
        .await
        .unwrap();
    let body = axum::body::to_bytes(streamed.body, 1024).await.unwrap();
    assert_eq!(&body[..], b"old");
    assert_eq!(3, metrics.fallback_hits.get());
}

#[tokio::test]
async fn not_found_only_when_both_stores_miss() {
    let dir = tempfile::tempdir().unwrap();
    let (primary, fallback) = (fake_s3().await, fake_s3().await);
    let metrics = Arc::new(StorageMetrics::default());
    let store = store(dual_config(&primary, &fallback, dir.path(), true), &metrics);
    assert!(matches!(
        store.read_object("media-new", "missing.png").await,
        Err(StorageError::NotFound)
    ));
    assert_eq!(1, metrics.fallback_double_misses.get());
}

#[tokio::test]
async fn primary_server_error_is_not_turned_into_not_found() {
    let dir = tempfile::tempdir().unwrap();
    let (primary, fallback) = (fake_s3().await, fake_s3().await);
    primary.put_object(
        "media-new/broken.png",
        FakeObject {
            status: Some(503),
            read_status: Some(503),
            ..FakeObject::default()
        },
    );
    let metrics = Arc::new(StorageMetrics::default());
    let store = store(dual_config(&primary, &fallback, dir.path(), true), &metrics);
    let result = store.read_object("media-new", "broken.png").await;
    assert!(
        !matches!(result, Err(StorageError::NotFound) | Ok(_)),
        "a 503 on the primary plus a 404 on the fallback must stay a retryable error"
    );
}

#[tokio::test]
async fn fallback_reads_off_keeps_today_behaviour() {
    let dir = tempfile::tempdir().unwrap();
    let (primary, fallback) = (fake_s3().await, fake_s3().await);
    fallback.put_object("media/old.png", object(b"old"));
    let metrics = Arc::new(StorageMetrics::default());
    let store = store(
        dual_config(&primary, &fallback, dir.path(), false),
        &metrics,
    );
    assert!(matches!(
        store.read_object("media-new", "old.png").await,
        Err(StorageError::NotFound)
    ));
}

#[tokio::test]
async fn store_sides_follow_the_absolute_store_id() {
    let dir = tempfile::tempdir().unwrap();
    let (primary, fallback) = (fake_s3().await, fake_s3().await);
    let cfg = dual_config(&primary, &fallback, dir.path(), true);
    let storage = &cfg.storage;
    assert_eq!(
        Some(StoreSide::Primary),
        storage.relay_side(Some("store-b"), "uploads")
    );
    assert_eq!(
        Some(StoreSide::Fallback),
        storage.relay_side(Some("store-a"), "uploads-new")
    );
    assert_eq!(None, storage.relay_side(Some("store-c"), "uploads-new"));
}

#[tokio::test]
async fn token_without_store_id_follows_its_uploads_bucket() {
    let dir = tempfile::tempdir().unwrap();
    let (primary, fallback) = (fake_s3().await, fake_s3().await);
    let mut cfg = dual_config(&primary, &fallback, dir.path(), true);
    let storage = &cfg.storage;
    assert_eq!(
        Some(StoreSide::Primary),
        storage.relay_side(None, "uploads-new")
    );
    assert_eq!(
        Some(StoreSide::Fallback),
        storage.relay_side(None, "uploads")
    );
    assert_eq!(None, storage.relay_side(None, "media-new"));
    cfg.storage.dual_store = None;
    assert_eq!(
        Some(StoreSide::Primary),
        cfg.storage.relay_side(None, "uploads")
    );
    assert_eq!(
        Some(StoreSide::Primary),
        cfg.storage.relay_side(Some("store-c"), "uploads")
    );
}
