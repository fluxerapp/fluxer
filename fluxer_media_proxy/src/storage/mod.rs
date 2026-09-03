// SPDX-License-Identifier: AGPL-3.0-or-later

mod identity;
mod keys;
mod local;
mod object_read;
mod object_stream;
mod relay_body;
mod response_body;
mod s3;
mod s3_endpoint;
mod source_read;

#[cfg(test)]
pub(crate) mod tests;

pub use identity::SourceObjectIdentity;
pub(crate) use keys::safe_key;
pub use relay_body::{RelayBody, RelayBodyChunks, RelayPutOptions};

use crate::{
    aws_sigv4,
    byte_budget::{BudgetedBytes, ByteBudget},
    config::{Config, StorageBackend},
    http_client::{self, HTTPClientOptions},
    metrics::{http_client::HTTPClientMetrics, storage::StorageMetrics},
    range::ByteRange,
    response_body_limit,
};
use axum::body::Body;
use bytes::Bytes;
use http::StatusCode;
use response_body::LocalStreamBufferPool;
use source_read::SourceReadCoordinator;
use std::{num::NonZeroU64, sync::Arc};
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
    pub identity: SourceObjectIdentity,
}

pub struct ObjectReadRequest<'a> {
    pub bucket: &'a str,
    pub key: &'a str,
    pub max_bytes: usize,
    pub budget: &'a ByteBudget,
    pub expected_identity: &'a SourceObjectIdentity,
}

#[derive(Clone, Copy)]
pub struct ObjectStreamRequest<'a> {
    pub bucket: &'a str,
    pub key: &'a str,
    pub max_bytes: usize,
    pub byte_range: Option<ByteRange>,
    pub expected_identity: &'a SourceObjectIdentity,
}

#[derive(Clone, Copy)]
struct BufferedObjectReadRequest<'a> {
    bucket: &'a str,
    key: &'a str,
    limit: usize,
    budget: &'a ByteBudget,
    expected_identity: Option<&'a SourceObjectIdentity>,
    content_digest: ContentDigestRequest,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ContentDigestRequest {
    Omit,
    Include,
}

impl ContentDigestRequest {
    const fn cache_key(self) -> &'static str {
        match self {
            Self::Omit => "omit-digest",
            Self::Include => "include-digest",
        }
    }
}

#[derive(Clone, Copy)]
struct ObjectStreamPlan<'a> {
    bucket: &'a str,
    key: &'a str,
    max_bytes: usize,
    range: StreamRange<'a>,
    expected_identity: Option<&'a SourceObjectIdentity>,
}

#[derive(Clone, Copy)]
enum StreamRange<'a> {
    Full,
    Header(&'a str),
    Bytes(ByteRange),
}

#[derive(Debug, Error)]
pub enum StorageError {
    #[error("object not found")]
    NotFound,
    #[error("invalid key")]
    InvalidKey,
    #[error("invalid bucket")]
    InvalidBucket,
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
    #[error("source read capacity exhausted")]
    SourceReadCapacityExhausted,
    #[error("source read waiter capacity exhausted")]
    SourceReadWaiterCapacityExhausted,
    #[error("source read leader ended without publishing a result")]
    SourceReadLeaderEnded,
    #[error("coalesced source read failed: {0}")]
    CoalescedSourceReadFailed(String),
    #[error("object storage operation failed: {0}")]
    ObjectStorage(#[source] anyhow::Error),
    #[error("S3 request failed: {0}")]
    S3(String),
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Http(#[from] reqwest::Error),
    #[error(transparent)]
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
    source_reads: SourceReadCoordinator,
    local_stream_buffers: LocalStreamBufferPool,
}

impl Store {
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
            source_reads: SourceReadCoordinator::new(),
            local_stream_buffers: local_stream_buffer_pool(),
        }
    }

    pub fn try_new(
        cfg: Config,
        metrics: Arc<StorageMetrics>,
        http_client_metrics: Arc<HTTPClientMetrics>,
    ) -> Result<Self, reqwest::Error> {
        let socket_io_timeout =
            NonZeroU64::new(cfg.socket_io_timeout_ms).unwrap_or(NonZeroU64::MIN);
        let options = HTTPClientOptions::new(socket_io_timeout, socket_io_timeout);
        let client = http_client::build(options, http_client_metrics)?;
        let raw_client = http_client::build_raw(options)?;
        Ok(Self {
            cfg,
            client,
            raw_client,
            metrics,
            source_reads: SourceReadCoordinator::new(),
            local_stream_buffers: local_stream_buffer_pool(),
        })
    }

    pub async fn write_object(
        &self,
        bucket: &str,
        key: &str,
        data: &[u8],
        content_type: &str,
    ) -> Result<(), StorageError> {
        if self.cfg.read_only {
            return Err(StorageError::ReadOnlyStorage);
        }
        match self.cfg.storage.backend {
            StorageBackend::Local => self.write_local(bucket, key, data).await,
            StorageBackend::S3 => self.write_s3(bucket, key, data, content_type).await,
        }
    }

    pub async fn ensure_bucket(&self, bucket: &str) -> Result<(), StorageError> {
        match self.cfg.storage.backend {
            StorageBackend::Local => self.ensure_bucket_local(bucket).await,
            StorageBackend::S3 => self.ensure_bucket_s3(bucket).await,
        }
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
            StorageBackend::Local => {
                self.write_local_relay(bucket, key, options).await?;
                Ok(None)
            }
            StorageBackend::S3 => self.relay_put_s3(bucket, key, options).await,
        }
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
        Ok(_) => metrics.record_hit(),
        Err(StorageError::NotFound) => metrics.record_miss(),
        Err(_) => metrics.record_error(),
    }
}

fn map_not_found(err: std::io::Error) -> StorageError {
    if err.kind() == std::io::ErrorKind::NotFound {
        StorageError::NotFound
    } else {
        StorageError::Io(err)
    }
}
