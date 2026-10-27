// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{env_bool, env_string};
use anyhow::{Context, Result, bail, ensure};
use aws_config::{BehaviorVersion, Region, retry::RetryConfig};
use aws_sdk_s3::Client as S3Client;
use aws_sdk_s3::error::ProvideErrorMetadata;
use aws_sdk_s3::primitives::ByteStream;
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use md5::{Digest as _, Md5};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::fs::{self};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Semaphore;
use tokio::task::JoinSet;

const DEFAULT_S3_WRITE_CONCURRENCY: usize = 8;
const DEFAULT_S3_RETRY_ATTEMPTS: u32 = 8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct S3UploadPlanItem {
    pub(crate) path: PathBuf,
    pub(crate) key: String,
    pub(crate) content_type: Option<String>,
    pub(crate) cache_control: Option<String>,
    pub(crate) repair_existing_metadata: bool,
}

impl S3UploadPlanItem {
    pub(crate) fn new(path: PathBuf, key: String) -> Self {
        Self {
            path,
            key,
            content_type: None,
            cache_control: None,
            repair_existing_metadata: false,
        }
    }

    pub(crate) fn with_content_type(mut self, content_type: impl Into<String>) -> Self {
        self.content_type = Some(content_type.into());
        self
    }

    pub(crate) fn with_cache_control(mut self, cache_control: impl Into<String>) -> Self {
        self.cache_control = Some(cache_control.into());
        self
    }

    pub(crate) fn with_detected_content_type(self) -> Self {
        let Some(content_type) = s3_content_type_for_key(&self.key) else {
            return self;
        };
        self.with_content_type(content_type)
    }

    pub(crate) fn repair_existing_metadata(mut self) -> Self {
        self.repair_existing_metadata = true;
        self
    }

    fn has_upload_metadata(&self) -> bool {
        self.content_type.is_some() || self.cache_control.is_some()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct S3UploadStats {
    pub(crate) uploaded: usize,
    pub(crate) skipped_existing: usize,
    pub(crate) metadata_repaired: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum S3UploadDisposition {
    Uploaded,
    SkippedExisting,
    MetadataRepaired,
}

pub(crate) async fn s3_client(default_endpoint: Option<&str>) -> Result<S3Client> {
    let region = env::var("AWS_REGION")
        .or_else(|_| env::var("AWS_DEFAULT_REGION"))
        .unwrap_or_else(|_| "us-east-1".to_string());
    let mut loader = aws_config::defaults(BehaviorVersion::latest()).region(Region::new(region));
    let endpoint = env_string("S3_ENDPOINT").or_else(|| default_endpoint.map(ToOwned::to_owned));
    if let Some(endpoint) = endpoint.as_deref() {
        loader = loader.endpoint_url(endpoint);
    }
    loader = loader.retry_config(s3_retry_config());
    let shared_config = loader.load().await;
    let mut s3_config = aws_sdk_s3::config::Builder::from(&shared_config);
    if endpoint.is_some() || env_bool("S3_FORCE_PATH_STYLE") {
        s3_config = s3_config.force_path_style(true);
    }
    Ok(S3Client::from_conf(s3_config.build()))
}

pub(crate) async fn upload_s3_plan_append_only(
    client: &S3Client,
    bucket: &str,
    plan: Vec<S3UploadPlanItem>,
) -> Result<S3UploadStats> {
    ensure_unique_s3_keys(&plan)?;
    let existing_objects = existing_s3_objects_for_plan(client, bucket, &plan).await?;
    let mut stats = S3UploadStats::default();
    let (pending, skipped_items) = split_append_only_upload_plan(bucket, plan, &existing_objects)?;

    let concurrency = s3_write_concurrency();
    let semaphore = Arc::new(Semaphore::new(concurrency));
    let bucket = bucket.to_string();
    let mut tasks = JoinSet::new();
    for item in pending {
        let permit = semaphore
            .clone()
            .acquire_owned()
            .await
            .context("S3 upload semaphore closed")?;
        let client = client.clone();
        let bucket = bucket.clone();
        tasks.spawn(async move {
            let _permit = permit;
            let disposition = put_file_to_s3(&client, &bucket, &item, true)
                .await
                .with_context(|| {
                    format!("Failed append-only upload for s3://{}/{}", bucket, item.key)
                })?;
            Ok::<_, anyhow::Error>(disposition)
        });
    }
    for item in skipped_items {
        if item.repair_existing_metadata && item.has_upload_metadata() {
            let permit = semaphore
                .clone()
                .acquire_owned()
                .await
                .context("S3 upload semaphore closed")?;
            let client = client.clone();
            let bucket = bucket.clone();
            tasks.spawn(async move {
                let _permit = permit;
                put_file_to_s3(&client, &bucket, &item, false)
                    .await
                    .with_context(|| {
                        format!("Failed to repair metadata for s3://{}/{}", bucket, item.key)
                    })
            });
        } else {
            println!("Skipping existing s3://{bucket}/{}", item.key);
            stats.skipped_existing += 1;
        }
    }

    while let Some(result) = tasks.join_next().await {
        match result.context("S3 upload task failed")?? {
            S3UploadDisposition::Uploaded => stats.uploaded += 1,
            S3UploadDisposition::SkippedExisting => stats.skipped_existing += 1,
            S3UploadDisposition::MetadataRepaired => stats.metadata_repaired += 1,
        }
    }

    Ok(stats)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct S3ObjectMetadata {
    pub(crate) e_tag: Option<String>,
    pub(crate) size: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct S3ListedObject {
    key: String,
    metadata: S3ObjectMetadata,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct S3FileIdentity {
    size: u64,
    md5_hex: String,
    md5_base64: String,
}

async fn existing_s3_objects_for_plan(
    client: &S3Client,
    bucket: &str,
    plan: &[S3UploadPlanItem],
) -> Result<BTreeMap<String, S3ObjectMetadata>> {
    let planned_keys = plan
        .iter()
        .map(|item| item.key.clone())
        .collect::<BTreeSet<_>>();
    let prefixes = s3_list_prefixes_for_plan(plan);
    let mut existing = BTreeMap::new();

    for prefix in prefixes {
        println!("Scanning existing S3 objects under s3://{bucket}/{prefix}");
        for object in list_s3_objects(client, bucket, &prefix).await? {
            if planned_keys.contains(&object.key) {
                existing.insert(object.key, object.metadata);
            }
        }
    }

    Ok(existing)
}

pub(crate) fn split_append_only_upload_plan(
    bucket: &str,
    plan: Vec<S3UploadPlanItem>,
    existing_objects: &BTreeMap<String, S3ObjectMetadata>,
) -> Result<(Vec<S3UploadPlanItem>, Vec<S3UploadPlanItem>)> {
    ensure_unique_s3_keys(&plan)?;
    let mut pending = Vec::new();
    let mut skipped = Vec::new();
    for item in plan {
        if let Some(remote) = existing_objects.get(&item.key) {
            if let Err(error) = ensure_existing_s3_object_matches_local(bucket, &item, remote) {
                if !item.key.ends_with(".map") {
                    return Err(error);
                }
                println!("Keeping the published source map: {error}");
            }
            skipped.push(item);
        } else {
            pending.push(item);
        }
    }
    Ok((pending, skipped))
}

fn ensure_existing_s3_object_matches_local(
    bucket: &str,
    item: &S3UploadPlanItem,
    remote: &S3ObjectMetadata,
) -> Result<()> {
    let local = s3_file_identity(&item.path)?;
    if let Some(remote_size) = remote.size {
        ensure!(
            remote_size >= 0,
            "Existing S3 object has invalid negative size: s3://{bucket}/{}",
            item.key
        );
        ensure!(
            remote_size as u64 == local.size,
            "Existing S3 object differs from local file: s3://{bucket}/{} has {} byte(s), local {} has {} byte(s)",
            item.key,
            remote_size,
            item.path.display(),
            local.size
        );
    }

    let Some(e_tag) = remote.e_tag.as_deref() else {
        bail!(
            "Existing S3 object cannot be verified because ListObjectsV2 did not return an ETag: s3://{bucket}/{}",
            item.key
        );
    };
    let Some(remote_md5) = s3_etag_md5(e_tag) else {
        bail!(
            "Existing S3 object cannot be verified because its ETag is not a single-part MD5: s3://{bucket}/{} (ETag {e_tag})",
            item.key
        );
    };
    ensure!(
        remote_md5 == local.md5_hex,
        "Existing S3 object differs from local file: s3://{bucket}/{} has ETag {e_tag}, local {} has MD5 {}",
        item.key,
        item.path.display(),
        local.md5_hex
    );
    Ok(())
}

fn s3_file_identity(path: &Path) -> Result<S3FileIdentity> {
    let mut file =
        fs::File::open(path).with_context(|| format!("Failed to read {}", path.display()))?;
    let size = file
        .metadata()
        .with_context(|| format!("Failed to stat {}", path.display()))?
        .len();
    let mut hasher = Md5::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let read = file
            .read(&mut buffer)
            .with_context(|| format!("Failed to read {}", path.display()))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    let digest = hasher.finalize();
    Ok(S3FileIdentity {
        size,
        md5_hex: hex::encode(digest),
        md5_base64: BASE64.encode(digest),
    })
}

fn s3_etag_md5(e_tag: &str) -> Option<String> {
    let value = e_tag.trim().trim_matches('"');
    if value.len() == 32 && value.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        Some(value.to_ascii_lowercase())
    } else {
        None
    }
}

fn s3_list_prefixes_for_plan(plan: &[S3UploadPlanItem]) -> BTreeSet<String> {
    plan.iter()
        .map(|item| s3_list_prefix_for_key(&item.key))
        .collect()
}

fn s3_list_prefix_for_key(key: &str) -> String {
    key.find('/')
        .map(|index| key[..=index].to_string())
        .unwrap_or_else(|| key.to_string())
}

fn ensure_unique_s3_keys(plan: &[S3UploadPlanItem]) -> Result<()> {
    let mut keys = BTreeSet::new();
    for item in plan {
        ensure!(
            keys.insert(item.key.clone()),
            "Duplicate S3 upload key in local plan: {}",
            item.key
        );
    }
    Ok(())
}

async fn put_file_to_s3(
    client: &S3Client,
    bucket: &str,
    item: &S3UploadPlanItem,
    if_absent: bool,
) -> Result<S3UploadDisposition> {
    let (action, operation, failure, done) = if if_absent {
        (
            "Uploading",
            "upload",
            "upload",
            S3UploadDisposition::Uploaded,
        )
    } else {
        (
            "Repairing metadata for",
            "metadata repair",
            "repair metadata for",
            S3UploadDisposition::MetadataRepaired,
        )
    };
    println!(
        "{action} {} -> s3://{bucket}/{}",
        item.path.display(),
        item.key
    );
    let identity = s3_file_identity(&item.path)?;
    let attempts = s3_retry_attempts();
    for attempt in 1..=attempts {
        let body = ByteStream::from_path(&item.path)
            .await
            .with_context(|| format!("Failed to read {}", item.path.display()))?;
        let mut request = client
            .put_object()
            .bucket(bucket)
            .key(&item.key)
            .set_if_none_match(if_absent.then(|| "*".to_string()))
            .content_md5(identity.md5_base64.clone())
            .body(body);
        if let Some(content_type) = &item.content_type {
            request = request.content_type(content_type);
        }
        if let Some(cache_control) = &item.cache_control {
            request = request.cache_control(cache_control);
        }
        match request.send().await {
            Ok(_) => return Ok(done),
            Err(error) => {
                let code = error
                    .as_service_error()
                    .and_then(|error| error.code())
                    .map(ToOwned::to_owned);
                let status = error
                    .raw_response()
                    .map(|response| response.status().as_u16());
                if if_absent && is_existing_object_error(code.as_deref(), status) {
                    println!("Skipping existing s3://{bucket}/{}", item.key);
                    return Ok(S3UploadDisposition::SkippedExisting);
                }
                if is_retryable_s3_error(code.as_deref(), status) && attempt < attempts {
                    sleep_before_s3_retry(
                        operation,
                        &format!("s3://{bucket}/{}", item.key),
                        attempt,
                        attempts,
                        code.as_deref(),
                        status,
                    )
                    .await;
                    continue;
                }
                let summary = s3_error_summary(code.as_deref(), status);
                return Err(error).with_context(|| {
                    format!("Failed to {failure} s3://{bucket}/{}{summary}", item.key)
                });
            }
        }
    }
    unreachable!("S3 retry attempts are always greater than zero")
}

fn is_existing_object_error(code: Option<&str>, status: Option<u16>) -> bool {
    matches!(
        code,
        Some("PreconditionFailed" | "ConditionalRequestConflict")
    ) || matches!(status, Some(409 | 412))
}

fn is_retryable_s3_error(code: Option<&str>, status: Option<u16>) -> bool {
    status == Some(429) || matches!(code, Some("RateLimitExceeded" | "TooManyRequests"))
}

async fn sleep_before_s3_retry(
    operation: &str,
    target: &str,
    attempt: u32,
    attempts: u32,
    code: Option<&str>,
    status: Option<u16>,
) {
    let delay = s3_retry_delay(attempt);
    let summary = s3_error_summary(code, status);
    println!(
        "Retrying S3 {operation} {target}{summary} in {:.1}s (attempt {}/{attempts})",
        delay.as_secs_f32(),
        attempt + 1
    );
    tokio::time::sleep(delay).await;
}

fn s3_retry_delay(attempt: u32) -> Duration {
    let shift = attempt.saturating_sub(1).min(5);
    Duration::from_millis(500 * (1_u64 << shift))
}

fn s3_error_summary(code: Option<&str>, status: Option<u16>) -> String {
    match (code, status) {
        (Some(code), Some(status)) => format!(" (S3 code {code}, HTTP status {status})"),
        (Some(code), None) => format!(" (S3 code {code})"),
        (None, Some(status)) => format!(" (HTTP status {status})"),
        (None, None) => String::new(),
    }
}

fn s3_retry_config() -> RetryConfig {
    RetryConfig::standard()
        .with_max_attempts(s3_retry_attempts())
        .with_initial_backoff(Duration::from_millis(500))
        .with_max_backoff(Duration::from_secs(30))
}

fn s3_retry_attempts() -> u32 {
    env::var("S3_RETRY_ATTEMPTS")
        .ok()
        .and_then(|value| value.parse::<u32>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(DEFAULT_S3_RETRY_ATTEMPTS)
}

fn s3_write_concurrency() -> usize {
    env::var("S3_WRITE_CONCURRENCY")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(DEFAULT_S3_WRITE_CONCURRENCY)
}

pub(crate) fn s3_content_type_for_key(key: &str) -> Option<&'static str> {
    let ext = key.rsplit('.').next()?.to_ascii_lowercase();
    match ext.as_str() {
        "html" | "htm" => Some("text/html; charset=utf-8"),
        "js" | "mjs" => Some("application/javascript; charset=utf-8"),
        "css" => Some("text/css; charset=utf-8"),
        "json" | "map" => Some("application/json; charset=utf-8"),
        "wasm" => Some("application/wasm"),
        "png" => Some("image/png"),
        "jpg" | "jpeg" => Some("image/jpeg"),
        "gif" => Some("image/gif"),
        "webp" => Some("image/webp"),
        "avif" => Some("image/avif"),
        "svg" => Some("image/svg+xml"),
        "ico" => Some("image/x-icon"),
        "woff" => Some("font/woff"),
        "woff2" => Some("font/woff2"),
        "ttf" => Some("font/ttf"),
        "otf" => Some("font/otf"),
        "eot" => Some("application/vnd.ms-fontobject"),
        "mp3" => Some("audio/mpeg"),
        "mp4" => Some("video/mp4"),
        "webm" => Some("video/webm"),
        "ogg" => Some("audio/ogg"),
        "wav" => Some("audio/wav"),
        "pdf" => Some("application/pdf"),
        "txt" => Some("text/plain; charset=utf-8"),
        "xml" => Some("application/xml; charset=utf-8"),
        "webmanifest" => Some("application/manifest+json"),
        _ => None,
    }
}

async fn list_s3_objects(
    client: &S3Client,
    bucket: &str,
    prefix: &str,
) -> Result<Vec<S3ListedObject>> {
    let mut objects = Vec::new();
    let mut token: Option<String> = None;
    loop {
        let response =
            send_s3_list_objects_v2_page(client, bucket, prefix, token.as_deref()).await?;
        objects.extend(response.contents().iter().filter_map(|object| {
            object.key().map(|key| S3ListedObject {
                key: key.to_string(),
                metadata: S3ObjectMetadata {
                    e_tag: object.e_tag().map(ToOwned::to_owned),
                    size: object.size(),
                },
            })
        }));
        token = response.next_continuation_token().map(ToOwned::to_owned);
        if token.is_none() {
            break;
        }
    }
    Ok(objects)
}

async fn send_s3_list_objects_v2_page(
    client: &S3Client,
    bucket: &str,
    prefix: &str,
    token: Option<&str>,
) -> Result<aws_sdk_s3::operation::list_objects_v2::ListObjectsV2Output> {
    let attempts = s3_retry_attempts();
    for attempt in 1..=attempts {
        let mut request = client.list_objects_v2().bucket(bucket).prefix(prefix);
        if let Some(value) = token {
            request = request.continuation_token(value);
        }
        match request.send().await {
            Ok(response) => return Ok(response),
            Err(error) => {
                let code = error
                    .as_service_error()
                    .and_then(|error| error.code())
                    .map(ToOwned::to_owned);
                let status = error
                    .raw_response()
                    .map(|response| response.status().as_u16());
                if is_retryable_s3_error(code.as_deref(), status) && attempt < attempts {
                    sleep_before_s3_retry(
                        "list",
                        &format!("s3://{bucket}/{prefix}"),
                        attempt,
                        attempts,
                        code.as_deref(),
                        status,
                    )
                    .await;
                    continue;
                }
                let summary = s3_error_summary(code.as_deref(), status);
                return Err(error)
                    .with_context(|| format!("Failed to list s3://{bucket}/{prefix}{summary}"));
            }
        }
    }
    unreachable!("S3 retry attempts are always greater than zero")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn append_only_plan_skips_existing_keys_without_reordering_pending_uploads() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();
        fs::write(root.join("a"), "a").unwrap();
        fs::write(root.join("b"), "b").unwrap();
        fs::write(root.join("c"), "c").unwrap();
        let plan = vec![
            S3UploadPlanItem::new(root.join("a"), "desktop/a".to_string()),
            S3UploadPlanItem::new(root.join("b"), "desktop/b".to_string())
                .with_content_type("text/plain; charset=utf-8")
                .repair_existing_metadata(),
            S3UploadPlanItem::new(root.join("c"), "desktop/c".to_string()),
        ];
        let b_identity = s3_file_identity(&root.join("b")).unwrap();
        let existing = BTreeMap::from([(
            "desktop/b".to_string(),
            S3ObjectMetadata {
                e_tag: Some(format!("\"{}\"", b_identity.md5_hex)),
                size: Some(b_identity.size as i64),
            },
        )]);

        let (pending, skipped) = split_append_only_upload_plan("bucket", plan, &existing).unwrap();

        assert_eq!(
            pending.into_iter().map(|item| item.key).collect::<Vec<_>>(),
            vec!["desktop/a", "desktop/c"]
        );
        assert_eq!(
            skipped
                .iter()
                .map(|item| item.key.as_str())
                .collect::<Vec<_>>(),
            vec!["desktop/b"]
        );
        assert_eq!(
            skipped[0].content_type.as_deref(),
            Some("text/plain; charset=utf-8")
        );
        assert!(skipped[0].repair_existing_metadata);
    }

    #[test]
    fn append_only_plan_rejects_existing_key_with_different_checksum() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("asset.svg");
        fs::write(&path, "local").unwrap();
        let plan = vec![S3UploadPlanItem::new(path, "emoji/asset.svg".to_string())];
        let existing = BTreeMap::from([(
            "emoji/asset.svg".to_string(),
            S3ObjectMetadata {
                e_tag: Some("\"00000000000000000000000000000000\"".to_string()),
                size: Some(5),
            },
        )]);

        let error = split_append_only_upload_plan("bucket", plan, &existing).unwrap_err();

        assert!(error.to_string().contains("differs from local file"));
    }

    #[test]
    fn append_only_plan_keeps_a_published_source_map_that_differs() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("abc.worker.js.map");
        fs::write(&path, "local").unwrap();
        let plan = vec![S3UploadPlanItem::new(
            path,
            "assets/abc.worker.js.map".to_string(),
        )];
        let existing = BTreeMap::from([(
            "assets/abc.worker.js.map".to_string(),
            S3ObjectMetadata {
                e_tag: Some("\"00000000000000000000000000000000\"".to_string()),
                size: Some(9),
            },
        )]);

        let (pending, skipped) = split_append_only_upload_plan("bucket", plan, &existing).unwrap();

        assert!(pending.is_empty());
        assert_eq!(
            skipped
                .iter()
                .map(|item| item.key.as_str())
                .collect::<Vec<_>>(),
            vec!["assets/abc.worker.js.map"]
        );
    }

    #[test]
    fn append_only_plan_rejects_existing_key_without_comparable_etag() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("asset.bin");
        fs::write(&path, "local").unwrap();
        let plan = vec![S3UploadPlanItem::new(path, "assets/asset.bin".to_string())];
        let existing = BTreeMap::from([(
            "assets/asset.bin".to_string(),
            S3ObjectMetadata {
                e_tag: Some("\"multipart-etag-2\"".to_string()),
                size: Some(5),
            },
        )]);

        let error = split_append_only_upload_plan("bucket", plan, &existing).unwrap_err();

        assert!(error.to_string().contains("not a single-part MD5"));
    }

    #[test]
    fn append_only_plan_rejects_duplicate_local_keys() {
        let plan = vec![
            S3UploadPlanItem::new(PathBuf::from("a"), "same/key".to_string()),
            S3UploadPlanItem::new(PathBuf::from("b"), "same/key".to_string()),
        ];

        assert_eq!(
            split_append_only_upload_plan("bucket", plan, &BTreeMap::new())
                .unwrap_err()
                .to_string(),
            "Duplicate S3 upload key in local plan: same/key"
        );
    }
}
