// SPDX-License-Identifier: AGPL-3.0-or-later

mod keys;
mod local;
mod object_read;
mod object_stream;
mod relay_body;
mod response_body;
mod s3;
mod s3_endpoint;

#[cfg(test)]
pub(crate) mod tests;

pub(crate) use keys::safe_key;
pub use relay_body::{RelayBody, RelayBodyChunks, RelayPutOptions};

use crate::{
    aws_sigv4,
    byte_budget::{BudgetedBytes, ByteBudget},
    config::{Config, StorageBackend, StoreSide},
    http_client::{self, HTTPClientOptions},
    metrics::{http_client::HTTPClientMetrics, storage::StorageMetrics},
    response_body_limit,
};
use axum::body::Body;
use bytes::Bytes;
use http::StatusCode;
use response_body::LocalStreamBufferPool;
use std::sync::Arc;
use thiserror::Error;

const STORAGE_STREAM_CHUNK_BYTES: usize = 256 * 1024;
const LOCAL_STREAM_BUFFER_COUNT: usize = 256;

#[derive(Clone, Debug)]
pub struct Object {
    pub data: Bytes,
    pub content_type: String,
}

#[derive(Clone, Debug)]
pub struct BufferedStorageObject {
    pub data: BudgetedBytes,
    pub content_type: String,
    pub content_digest: Option<[u8; 32]>,
}

pub struct StreamObject {
    pub body: Body,
    pub status: StatusCode,
    pub content_length: Option<u64>,
    pub content_type: String,
    pub byte_range: Option<crate::range::ByteRange>,
    pub total_length: Option<u64>,
}

#[derive(Clone, Debug)]
pub struct HeadResult {
    pub content_length: u64,
    pub content_type: String,
}

#[derive(Clone, Copy)]
struct BufferedObjectReadRequest<'a> {
    bucket: &'a str,
    key: &'a str,
    limit: usize,
    budget: &'a ByteBudget,
    content_digest: ContentDigestRequest,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ContentDigestRequest {
    Omit,
    Include,
}

#[derive(Clone, Copy)]
struct ObjectStreamPlan<'a> {
    bucket: &'a str,
    key: &'a str,
    range: StreamRange<'a>,
}

#[derive(Clone, Copy)]
enum StreamRange<'a> {
    Full,
    Header(&'a str),
}

#[derive(Debug, Error)]
pub enum StorageError {
    #[error("object not found")]
    NotFound,
    #[error("invalid key")]
    InvalidKey,
    #[error("invalid bucket")]
    InvalidBucket,
    #[error("no such multipart upload")]
    NoSuchUpload,
    #[error("read-only storage")]
    ReadOnlyStorage,
    #[error("stream too long")]
    StreamTooLong,
    #[error("invalid S3 endpoint")]
    InvalidS3Endpoint,
    #[error("object changed between identity resolution and content read")]
    ObjectChanged,
    #[error("buffered input byte budget exhausted")]
    BufferBudgetExhausted,
    #[error("buffered input allocation failed")]
    BufferAllocationFailed,
    #[error("object storage operation failed: {0}")]
    ObjectStorage(#[source] anyhow::Error),
    #[error("S3 request failed: {0}")]
    S3(String),
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("{}", error_with_causes(.0))]
    Http(#[from] reqwest::Error),
    #[error("{}", error_with_causes(.0))]
    HttpMiddleware(#[from] reqwest_middleware::Error),
    #[error(transparent)]
    Sign(#[from] aws_sigv4::Error),
}

#[derive(Clone)]
pub struct Store {
    pub cfg: Config,
    client: http_client::HttpClient,
    raw_client: reqwest::Client,
    metrics: Arc<StorageMetrics>,
    local_stream_buffers: LocalStreamBufferPool,
    fallback: Option<Arc<Store>>,
}

impl Store {
    #[cfg(test)]
    pub fn new(
        cfg: Config,
        metrics: Arc<StorageMetrics>,
        http_client_metrics: Arc<HTTPClientMetrics>,
    ) -> Self {
        Self {
            cfg,
            client: http_client::build_default(http_client_metrics),
            raw_client: http_client::build_raw_default(),
            metrics,
            local_stream_buffers: local_stream_buffer_pool(),
            fallback: None,
        }
    }

    pub fn try_new(
        cfg: Config,
        metrics: Arc<StorageMetrics>,
        http_client_metrics: Arc<HTTPClientMetrics>,
    ) -> anyhow::Result<Self> {
        if matches!(cfg.storage.backend, StorageBackend::Local) {
            local::prepare_local_root(&cfg.storage.root)?;
        }
        let socket_io_timeout = cfg.socket_io_timeout_ms;
        let options = HTTPClientOptions::streaming(socket_io_timeout, socket_io_timeout);
        let client = http_client::build(options, Arc::clone(&http_client_metrics))?;
        let raw_client = http_client::build_raw(options)?;
        let fallback = match (&cfg.storage.backend, fallback_config(&cfg)) {
            (StorageBackend::S3, Some(fallback_cfg)) => Some(Arc::new(Self::try_new(
                fallback_cfg,
                Arc::clone(&metrics),
                http_client_metrics,
            )?)),
            _ => None,
        };
        Ok(Self {
            cfg,
            client,
            raw_client,
            metrics,
            local_stream_buffers: local_stream_buffer_pool(),
            fallback,
        })
    }

    pub(super) fn reads_fallback(&self) -> Option<&Store> {
        let reads_enabled = self
            .cfg
            .storage
            .dual_store
            .as_ref()
            .is_some_and(|dual| dual.reads_enabled);
        if reads_enabled {
            self.fallback.as_deref()
        } else {
            None
        }
    }

    pub(super) fn fallback_bucket(&self, bucket: &str) -> String {
        let storage = &self.cfg.storage;
        let Some(dual) = storage.dual_store.as_ref() else {
            return bucket.to_owned();
        };
        if bucket == storage.bucket_cdn {
            dual.bucket_cdn.clone()
        } else if bucket == storage.bucket_uploads {
            dual.bucket_uploads.clone()
        } else if bucket == storage.bucket_static {
            dual.bucket_static.clone()
        } else {
            bucket.to_owned()
        }
    }

    pub fn side(&self, side: StoreSide) -> Option<&Store> {
        match side {
            StoreSide::Primary => Some(self),
            StoreSide::Fallback => self.fallback.as_deref(),
        }
    }

    pub(super) fn record_fallback<T>(
        &self,
        primary: &Result<T, StorageError>,
        fallback: &Result<T, StorageError>,
    ) {
        match (primary, fallback) {
            (_, Ok(_)) => self.metrics.fallback_hits.inc(),
            (Err(StorageError::NotFound), Err(StorageError::NotFound)) => {
                self.metrics.fallback_double_misses.inc();
            }
            (_, Err(StorageError::NotFound)) => {}
            (_, Err(_)) => self.metrics.fallback_errors.inc(),
        }
    }

    #[cfg(test)]
    pub async fn write_object(
        &self,
        bucket: &str,
        key: &str,
        data: &[u8],
        content_type: &str,
    ) -> Result<(), StorageError> {
        assert!(
            matches!(self.cfg.storage.backend, StorageBackend::Local),
            "write_object is a local-only test fixture"
        );
        self.relay_put_object(
            bucket,
            key,
            RelayPutOptions {
                body: RelayBody::Buffered(Bytes::copy_from_slice(data)),
                content_length: data.len() as u64,
                content_type: Some(content_type.to_owned()),
                upload_id: None,
                part_number: None,
                timeout_ms: 0,
            },
        )
        .await
        .map(|_| ())
    }

    pub async fn relay_put_object(
        &self,
        bucket: &str,
        key: &str,
        options: RelayPutOptions,
    ) -> Result<Option<String>, StorageError> {
        if self.cfg.read_only {
            return Err(StorageError::ReadOnlyStorage);
        }
        match self.cfg.storage.backend {
            StorageBackend::Local => self.write_local_relay(bucket, key, options).await,
            StorageBackend::S3 => self.relay_put_s3(bucket, key, options).await,
        }
    }
}

fn fallback_config(cfg: &Config) -> Option<Config> {
    let dual = cfg.storage.dual_store.as_ref()?;
    let mut fallback = cfg.clone();
    let storage = &mut fallback.storage;
    storage.s3_endpoint = dual.endpoint.clone();
    storage.s3_region = dual.region.clone();
    storage.s3_access_key_id = dual.access_key_id.clone();
    storage.s3_secret_access_key = dual.secret_access_key.clone();
    storage.s3_session_token = String::new();
    storage.s3_force_path_style = dual.force_path_style;
    storage.s3_read_endpoint = None;
    storage.s3_read_bucket = dual.bucket_cdn.clone();
    storage.bucket_cdn = dual.bucket_cdn.clone();
    storage.bucket_uploads = dual.bucket_uploads.clone();
    storage.bucket_static = dual.bucket_static.clone();
    storage.store_id = dual.store_id.clone();
    storage.dual_store = None;
    Some(fallback)
}

pub(super) fn merge_fallback<T>(
    primary: Result<T, StorageError>,
    fallback: Result<T, StorageError>,
) -> Result<T, StorageError> {
    match (primary, fallback) {
        (Ok(value), _) | (_, Ok(value)) => Ok(value),
        (Err(StorageError::NotFound), Err(fallback_error)) => Err(fallback_error),
        (Err(primary_error), Err(_)) => Err(primary_error),
    }
}

pub(super) fn wants_fallback<T>(result: &Result<T, StorageError>) -> bool {
    match result {
        Ok(_) => false,
        Err(
            StorageError::InvalidKey
            | StorageError::InvalidBucket
            | StorageError::StreamTooLong
            | StorageError::BufferBudgetExhausted
            | StorageError::BufferAllocationFailed
            | StorageError::ObjectChanged
            | StorageError::ReadOnlyStorage,
        ) => false,
        Err(_) => true,
    }
}

fn local_stream_buffer_pool() -> LocalStreamBufferPool {
    LocalStreamBufferPool::new(STORAGE_STREAM_CHUNK_BYTES, LOCAL_STREAM_BUFFER_COUNT)
        .expect("local stream buffer pool bounds are constant and nonzero")
}

pub(crate) fn unversioned_read_budget(limit: usize) -> ByteBudget {
    ByteBudget::new(
        limit.saturating_add(response_body_limit::RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX),
    )
}

fn record_storage_outcome<T>(metrics: &StorageMetrics, result: &Result<T, StorageError>) {
    match result {
        Ok(_) => metrics.storage_hits.inc(),
        Err(StorageError::NotFound) => metrics.storage_misses.inc(),
        Err(_) => metrics.storage_errors.inc(),
    }
}

fn error_with_causes(error: &(dyn std::error::Error + 'static)) -> String {
    let mut message = error.to_string();
    let mut cause = error.source();
    while let Some(error) = cause {
        message.push_str(": ");
        message.push_str(&error.to_string());
        cause = error.source();
    }
    message
}
