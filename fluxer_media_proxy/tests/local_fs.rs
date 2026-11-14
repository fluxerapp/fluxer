// SPDX-License-Identifier: AGPL-3.0-or-later

use bytes::Bytes;
use fluxer_media_proxy::{
    config::Config,
    metrics::Metrics,
    storage::{RelayBody, RelayPutOptions, StorageError, Store},
};
use serde_json::Value;
use std::{
    os::unix::fs::PermissionsExt as _,
    path::{Path, PathBuf},
};

const VECTORS: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../fluxer_common/src/testdata/filesystem_storage_vectors.json"
));

struct Root {
    _dir: tempfile::TempDir,
    path: PathBuf,
    store: Store,
}

fn vectors() -> Value {
    serde_json::from_str(VECTORS).expect("vectors are valid JSON")
}

fn store_at(path: &Path) -> anyhow::Result<Store> {
    let cfg = Config::load_from_iter([
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret".to_owned()),
        ("FLUXER_MEDIA_PROXY_STORAGE_BACKEND", "local".to_owned()),
        (
            "FLUXER_MEDIA_PROXY_STORAGE_ROOT",
            path.display().to_string(),
        ),
    ])?;
    let metrics = Metrics::new();
    Store::try_new(cfg, metrics.storage(), metrics.http_client())
}

fn prepared_root() -> Root {
    let dir = tempfile::tempdir().expect("temp root");
    let path = dir.path().canonicalize().expect("canonical root");
    let layout = &vectors()["layout"];
    std::fs::write(
        path.join(layout["version_file"].as_str().unwrap()),
        layout["version_contents"].as_str().unwrap(),
    )
    .expect("write layout");
    let store = store_at(&path).expect("prepared root opens");
    Root {
        _dir: dir,
        path,
        store,
    }
}

fn put_options(data: &[u8], content_type: Option<&str>) -> RelayPutOptions {
    RelayPutOptions {
        body: RelayBody::Buffered(Bytes::copy_from_slice(data)),
        content_length: data.len() as u64,
        content_type: content_type.map(ToOwned::to_owned),
        upload_id: None,
        part_number: None,
        timeout_ms: 0,
    }
}

fn part_options(data: &[u8], upload_id: &str, part_number: u32) -> RelayPutOptions {
    RelayPutOptions {
        upload_id: Some(upload_id.to_owned()),
        part_number: Some(part_number),
        ..put_options(data, None)
    }
}

async fn put(root: &Root, bucket: &str, key: &str, data: &[u8], content_type: Option<&str>) {
    root.store
        .relay_put_object(bucket, key, put_options(data, content_type))
        .await
        .unwrap_or_else(|error| panic!("put {bucket}/{key}: {error}"));
}

fn staging_entries(root: &Root, sub: &str) -> Vec<String> {
    let mut names: Vec<String> = std::fs::read_dir(root.path.join(".staging").join(sub))
        .expect("staging dir")
        .map(|entry| entry.unwrap().file_name().into_string().unwrap())
        .collect();
    names.sort();
    names
}

#[test]
fn root_needs_the_layout_file_and_gets_its_staging_dirs() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().canonicalize().unwrap();
    assert!(
        store_at(&path).is_err(),
        "a root without .layout is refused"
    );
    std::fs::write(path.join(".layout"), r#"{"v":2}"#).unwrap();
    assert!(
        store_at(&path).is_err(),
        "an unknown layout version is refused"
    );
    std::fs::write(path.join(".layout"), r#"{"v":1}"#).unwrap();
    store_at(&path).expect("a version 1 root opens");
    let layout = &vectors()["layout"];
    for sub in layout["staging_subdirs"].as_array().unwrap() {
        let staged = path.join(".staging").join(sub.as_str().unwrap());
        assert!(staged.is_dir(), "{} is created", staged.display());
    }
    assert!(store_at(&path.join("missing")).is_err());
}

#[tokio::test]
async fn shared_vectors_hold() {
    let root = prepared_root();
    let vectors = vectors();
    let mapping = &vectors["segment_mapping"];

    for (index, case) in mapping["segments"].as_array().unwrap().iter().enumerate() {
        let bucket = format!("seg{index}");
        let segment = case["segment"].as_str().unwrap();
        let mapped = case["mapped"].as_str().unwrap();
        put(&root, &bucket, segment, b"x", None).await;
        assert!(
            root.path.join(&bucket).join(mapped).is_file(),
            "{}: stored under the mapped name",
            case["description"]
        );
        let sidecar = root.path.join(".meta").join(&bucket).join(mapped);
        assert_eq!(case["changed"].as_bool().unwrap(), sidecar.is_file());
        if sidecar.is_file() {
            let body: Value = serde_json::from_slice(&std::fs::read(&sidecar).unwrap()).unwrap();
            assert_eq!(segment, body["key"].as_str().unwrap());
            assert_eq!(1, body["v"].as_u64().unwrap());
        }
        let head = root.store.head_object(&bucket, segment).await.unwrap();
        assert_eq!(1, head.content_length);
    }

    for (index, case) in mapping["keys"].as_array().unwrap().iter().enumerate() {
        let bucket = format!("key{index}");
        let key = case["key"].as_str().unwrap();
        put(&root, &bucket, key, b"key", None).await;
        assert!(
            root.path
                .join(&bucket)
                .join(case["path"].as_str().unwrap())
                .is_file()
        );
        let read = root.store.read_object(&bucket, key).await.unwrap();
        assert_eq!(b"key".as_slice(), read.data.as_ref());
    }

    for case in vectors["invalid_keys"].as_array().unwrap() {
        let key = case["key"].as_str().unwrap();
        let put = root
            .store
            .relay_put_object("cdn", key, put_options(b"x", None))
            .await;
        assert!(
            matches!(put, Err(StorageError::InvalidKey)),
            "put {key:?} ({}) is refused",
            case["reason"]
        );
        assert!(matches!(
            root.store.head_object("cdn", key).await,
            Err(StorageError::InvalidKey)
        ));
    }

    for (index, key) in vectors["valid_keys"].as_array().unwrap().iter().enumerate() {
        let bucket = format!("valid{index}");
        let key = key.as_str().unwrap();
        put(&root, &bucket, key, b"valid", None).await;
        assert_eq!(
            5,
            root.store
                .head_object(&bucket, key)
                .await
                .unwrap()
                .content_length
        );
    }

    for case in vectors["invalid_buckets"].as_array().unwrap() {
        let bucket = case["bucket"].as_str().unwrap();
        let put = root
            .store
            .relay_put_object(bucket, "a.png", put_options(b"x", None))
            .await;
        assert!(
            matches!(put, Err(StorageError::InvalidBucket)),
            "bucket {bucket:?} ({}) is refused",
            case["reason"]
        );
    }

    let table = &vectors["extension_table"];
    let fallback = table["fallback"].as_str().unwrap();
    for (extension, content_type) in table["types"].as_object().unwrap() {
        let key = format!("types/file.{extension}");
        put(&root, "ext", &key, b"x", None).await;
        let head = root.store.head_object("ext", &key).await.unwrap();
        assert_eq!(content_type.as_str().unwrap(), head.content_type, "{key}");
    }
    for case in table["cases"].as_array().unwrap() {
        let key = case["key"].as_str().unwrap();
        put(&root, "cases", key, b"x", None).await;
        let head = root.store.head_object("cases", key).await.unwrap();
        assert_eq!(
            case["content_type"].as_str().unwrap_or(fallback),
            head.content_type,
            "{key}"
        );
    }

    for (index, case) in vectors["sidecar"].as_array().unwrap().iter().enumerate() {
        let bucket = format!("side{index}");
        let key = case["key"].as_str().unwrap();
        let stored = case["stored_content_type"].as_str();
        put(&root, &bucket, key, b"x", stored).await;
        let mapped: Vec<String> = key
            .split('/')
            .map(|segment| mapped_segment(mapping, segment))
            .collect();
        let sidecar = root.path.join(".meta").join(&bucket).join(mapped.join("/"));
        assert_eq!(
            case["sidecar"].as_bool().unwrap(),
            sidecar.is_file(),
            "{key} {stored:?}"
        );
        if let Some(stored) = stored {
            let head = root.store.head_object(&bucket, key).await.unwrap();
            assert_eq!(stored, head.content_type, "{key}");
        }
    }

    assert!(staging_entries(&root, "tmp").is_empty());
}

fn mapped_segment(mapping: &Value, segment: &str) -> String {
    if segment.len() <= 255 {
        return segment.to_owned();
    }
    for case in mapping["segments"].as_array().unwrap() {
        if case["segment"].as_str() == Some(segment) {
            return case["mapped"].as_str().unwrap().to_owned();
        }
    }
    for case in mapping["keys"].as_array().unwrap() {
        let key = case["key"].as_str().unwrap();
        let path = case["path"].as_str().unwrap();
        for (original, mapped) in key.split('/').zip(path.split('/')) {
            if original == segment {
                return mapped.to_owned();
            }
        }
    }
    panic!("no vector maps the segment {segment}");
}

#[tokio::test]
async fn relay_part_needs_a_matching_upload_manifest() {
    let root = prepared_root();
    let upload_id = "0123456789abcdef0123456789abcdef";
    let key = "attachments/1/2/big.mp4";
    let refused = root
        .store
        .relay_put_object("uploads", key, part_options(b"one", upload_id, 1))
        .await;
    assert!(matches!(refused, Err(StorageError::NoSuchUpload)));

    let upload_dir = root.path.join(".staging/multipart").join(upload_id);
    std::fs::create_dir(&upload_dir).unwrap();
    let refused = root
        .store
        .relay_put_object("uploads", key, part_options(b"one", upload_id, 1))
        .await;
    assert!(matches!(refused, Err(StorageError::NoSuchUpload)));

    let manifest = |key: &str| {
        format!(
            r#"{{"v":1,"bucket":"uploads","key":"{key}","contentType":"video/mp4","created":1}}"#
        )
    };
    std::fs::write(
        upload_dir.join("upload.json"),
        manifest("attachments/1/2/other.mp4"),
    )
    .unwrap();
    let refused = root
        .store
        .relay_put_object("uploads", key, part_options(b"one", upload_id, 1))
        .await;
    assert!(matches!(refused, Err(StorageError::NoSuchUpload)));

    std::fs::write(upload_dir.join("upload.json"), manifest(key)).unwrap();
    let etag = root
        .store
        .relay_put_object("uploads", key, part_options(b"one", upload_id, 1))
        .await
        .expect("part accepted")
        .expect("local parts answer an ETag");
    assert!(etag.starts_with("\"3-"), "{etag}");
    assert_eq!(
        b"one".as_slice(),
        std::fs::read(upload_dir.join("part-00001")).unwrap()
    );
    root.store
        .relay_put_object("uploads", key, part_options(b"last", upload_id, 10_000))
        .await
        .expect("highest part number accepted");
    assert!(upload_dir.join("part-10000").is_file());
    for (bad_id, part_number) in [
        ("0123456789ABCDEF0123456789abcdef", 1),
        (upload_id, 0),
        (upload_id, 10_001),
    ] {
        let refused = root
            .store
            .relay_put_object("uploads", key, part_options(b"x", bad_id, part_number))
            .await;
        assert!(matches!(refused, Err(StorageError::NoSuchUpload)));
    }

    let trash = root
        .path
        .join(".staging/trash")
        .join(format!("{upload_id}-a1b2"));
    std::fs::rename(&upload_dir, &trash).unwrap();
    let late = root
        .store
        .relay_put_object("uploads", key, part_options(b"late", upload_id, 2))
        .await;
    assert!(matches!(late, Err(StorageError::NoSuchUpload)));
    assert!(!trash.join("part-00002").exists());
    assert!(
        !root.path.join("uploads").exists(),
        "parts never land in the bucket"
    );
    assert!(staging_entries(&root, "tmp").is_empty());
}

#[tokio::test]
async fn replacing_an_object_keeps_open_readers_on_the_old_bytes() {
    let root = prepared_root();
    let key = "attachments/1/2/a.bin";
    let old = vec![1u8; 600_000];
    put(&root, "cdn", key, &old, None).await;
    let open = root.store.stream_object("cdn", key, None).await.unwrap();
    put(&root, "cdn", key, b"new", None).await;
    let read = axum::body::to_bytes(open.body, usize::MAX).await.unwrap();
    assert_eq!(old.as_slice(), read.as_ref());
    let fresh = root.store.read_object("cdn", key).await.unwrap();
    assert_eq!(b"new".as_slice(), fresh.data.as_ref());
    assert!(staging_entries(&root, "tmp").is_empty());
}

#[tokio::test]
async fn sidecar_type_wins_over_the_extension_and_goes_away_when_it_matches() {
    let root = prepared_root();
    let key = "attachments/1/2/picture.png";
    let png = b"\x89PNG\r\n\x1a\n\0\0\0\rIHDR\0\0\0\x01\0\0\0\x01\x08\x06\0\0\0\x1f\x15\xc4\x89";
    put(&root, "cdn", key, png, Some("image/jpeg")).await;
    let sidecar = root.path.join(".meta/cdn").join(key);
    assert!(sidecar.is_file());
    assert_eq!(
        "image/jpeg",
        root.store
            .head_object("cdn", key)
            .await
            .unwrap()
            .content_type
    );
    assert_eq!(
        "image/jpeg",
        root.store
            .stream_object("cdn", key, None)
            .await
            .unwrap()
            .content_type
    );
    assert_eq!(
        "image/jpeg",
        root.store
            .read_object("cdn", key)
            .await
            .unwrap()
            .content_type
    );

    put(&root, "cdn", key, png, Some("image/png")).await;
    assert!(
        !sidecar.exists(),
        "a sidecar that matches the extension is removed"
    );
    assert_eq!(
        "image/png",
        root.store
            .head_object("cdn", key)
            .await
            .unwrap()
            .content_type
    );
}

#[tokio::test]
async fn writes_recreate_staging_and_leave_no_temp_files() {
    let root = prepared_root();
    std::fs::remove_dir_all(root.path.join(".staging")).unwrap();
    put(&root, "cdn", "a/b/c.txt", b"abc", None).await;
    assert!(root.path.join(".staging/tmp").is_dir());

    let short = root
        .store
        .relay_put_object(
            "cdn",
            "a/b/short.txt",
            RelayPutOptions {
                content_length: 10,
                ..put_options(b"abc", None)
            },
        )
        .await;
    assert!(short.is_err(), "a body shorter than declared is refused");
    assert!(!root.path.join("cdn/a/b/short.txt").exists());
    assert!(staging_entries(&root, "tmp").is_empty());

    let mode = |path: &Path| std::fs::metadata(path).unwrap().permissions().mode() & 0o777;
    assert_eq!(0o770, mode(&root.path.join("cdn/a/b")));
    assert_eq!(0o660, mode(&root.path.join("cdn/a/b/c.txt")));
}

#[tokio::test]
async fn symlinked_last_component_is_refused() {
    let root = prepared_root();
    put(&root, "cdn", "real.txt", b"real", None).await;
    std::os::unix::fs::symlink(
        root.path.join("cdn/real.txt"),
        root.path.join("cdn/link.txt"),
    )
    .unwrap();
    assert!(matches!(
        root.store.read_object("cdn", "link.txt").await,
        Err(StorageError::InvalidKey)
    ));
}
