// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    BufferedObjectReadRequest, BufferedStorageObject, ContentDigestRequest, HeadResult,
    ObjectStreamPlan, STORAGE_STREAM_CHUNK_BYTES, StorageError, Store, StreamObject, StreamRange,
    identity::{LocalSourceObject, SourceObjectIdentity, local_source_object_identity},
    keys::{safe_bucket, safe_key},
    map_not_found,
    relay_body::{RelayBody, RelayPutOptions},
    response_body::{exact_byte_stream, local_reader_stream, read_exact_bytes},
};
use crate::{mime, range};
use axum::body::Body;
use futures_util::StreamExt as _;
use http::StatusCode;
use sha2::{Digest as _, Sha256};
use std::{
    fs::Metadata,
    os::unix::fs::MetadataExt as _,
    path::{Path, PathBuf},
};
use tokio::io::{AsyncReadExt as _, AsyncSeekExt as _, AsyncWriteExt as _};

const LOCAL_OPEN_FLAGS: i32 = libc::O_CLOEXEC | libc::O_NOFOLLOW | libc::O_NONBLOCK;

impl Store {
    pub(super) async fn local_path(
        &self,
        bucket: &str,
        key: &str,
    ) -> Result<PathBuf, StorageError> {
        safe_bucket(bucket)?;
        safe_key(key)?;
        let root = Path::new(&self.cfg.storage.root);
        let path = root.join(bucket).join(key);
        reject_symlink_chain(&path)?;
        Ok(path)
    }

    pub(super) async fn ensure_bucket_local(&self, bucket: &str) -> Result<(), StorageError> {
        safe_bucket(bucket)?;
        tokio::fs::create_dir_all(Path::new(&self.cfg.storage.root).join(bucket)).await?;
        Ok(())
    }

    pub(super) async fn read_local(
        &self,
        request: BufferedObjectReadRequest<'_>,
    ) -> Result<BufferedStorageObject, StorageError> {
        let path = self.local_path(request.bucket, request.key).await?;
        let (file, metadata) = open_local_read_file(&path).await?;
        if metadata.len() > request.limit as u64 {
            return Err(StorageError::StreamTooLong);
        }
        let content_length =
            usize::try_from(metadata.len()).map_err(|_| StorageError::StreamTooLong)?;
        if let Some(expected) = request.expected_identity {
            let identity = local_identity(request.bucket, request.key, &metadata);
            if identity != *expected {
                return Err(StorageError::ObjectChanged);
            }
        }
        let data = read_exact_bytes(file, content_length, request.budget).await?;
        let content_type = mime::detect(&data[..data.len().min(8192)], request.key, None);
        Ok(BufferedStorageObject {
            content_digest: match request.content_digest {
                ContentDigestRequest::Omit => None,
                ContentDigestRequest::Include => Some(Sha256::digest(data.as_ref()).into()),
            },
            data,
            content_type,
        })
    }

    pub(super) async fn head_local(
        &self,
        bucket: &str,
        key: &str,
        max_bytes: usize,
    ) -> Result<HeadResult, StorageError> {
        let path = self.local_path(bucket, key).await?;
        let metadata = tokio::fs::symlink_metadata(&path)
            .await
            .map_err(map_not_found)?;
        if !metadata.is_file() {
            return Err(StorageError::NotFound);
        }
        if metadata.len() > max_bytes as u64 {
            return Err(StorageError::StreamTooLong);
        }
        Ok(HeadResult {
            content_length: metadata.len(),
            content_type: local_content_type(key),
            identity: local_identity(bucket, key, &metadata),
        })
    }

    pub(super) async fn stream_local(
        &self,
        plan: ObjectStreamPlan<'_>,
    ) -> Result<StreamObject, StorageError> {
        let path = self.local_path(plan.bucket, plan.key).await?;
        let (mut file, metadata) = open_local_read_file(&path).await?;
        if metadata.len() > plan.max_bytes as u64 {
            return Err(StorageError::StreamTooLong);
        }
        if let Some(expected) = plan.expected_identity {
            let identity = local_identity(plan.bucket, plan.key, &metadata);
            if identity != *expected {
                return Err(StorageError::ObjectChanged);
            }
        }
        let total_len = usize::try_from(metadata.len()).map_err(|_| StorageError::StreamTooLong)?;
        let byte_range = match plan.range {
            StreamRange::Full => None,
            StreamRange::Header(header) => match range::parse_range(Some(header), total_len) {
                range::RangeSelection::Partial(byte_range) => Some(byte_range),
                range::RangeSelection::Full => None,
                // The caller decides how to render 416; reporting it as a plain 200 here would
                // serve the whole object for a range the client can never use.
                range::RangeSelection::Unsatisfiable => {
                    return Ok(StreamObject {
                        body: Body::empty(),
                        status: StatusCode::RANGE_NOT_SATISFIABLE,
                        content_length: Some(0),
                        content_type: local_content_type(plan.key),
                        byte_range: None,
                        total_length: Some(metadata.len()),
                    });
                }
            },
            StreamRange::Bytes(byte_range) => {
                if byte_range.start > byte_range.end || byte_range.end >= total_len {
                    return Err(StorageError::ObjectChanged);
                }
                Some(byte_range)
            }
        };
        let (status, body_len, start) = match byte_range {
            Some(byte_range) => (
                StatusCode::PARTIAL_CONTENT,
                (byte_range.end - byte_range.start + 1) as u64,
                byte_range.start as u64,
            ),
            None => (StatusCode::OK, total_len as u64, 0),
        };
        if start > 0 {
            file.seek(std::io::SeekFrom::Start(start)).await?;
        }
        let reader = file.take(body_len);
        let body = if body_len == 0 {
            Body::empty()
        } else {
            let capacity = body_len.min(STORAGE_STREAM_CHUNK_BYTES as u64) as usize;
            let stream = local_reader_stream(reader, self.local_stream_buffers.clone(), capacity)?;
            Body::from_stream(exact_byte_stream(stream, body_len))
        };
        Ok(StreamObject {
            body,
            status,
            content_length: Some(body_len),
            content_type: local_content_type(plan.key),
            byte_range,
            total_length: Some(metadata.len()),
        })
    }

    pub(super) async fn write_local(
        &self,
        bucket: &str,
        key: &str,
        data: &[u8],
    ) -> Result<(), StorageError> {
        let path = self.local_path(bucket, key).await?;
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }
        tokio::fs::write(path, data).await?;
        Ok(())
    }

    pub(super) async fn write_local_relay(
        &self,
        bucket: &str,
        key: &str,
        options: RelayPutOptions,
    ) -> Result<(), StorageError> {
        let path = self.local_path(bucket, key).await?;
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }
        let mut dest = tokio::fs::File::create(&path).await?;
        let content_length = options.content_length;
        let result = async {
            match options.body {
                RelayBody::Spooled(mut source) => {
                    source.seek(std::io::SeekFrom::Start(0)).await?;
                    tokio::io::copy(&mut source, &mut dest).await?;
                }
                RelayBody::Streamed(mut chunks) => {
                    while let Some(chunk) = chunks.next().await {
                        dest.write_all(&chunk?).await?;
                    }
                }
            }
            dest.flush().await?;
            dest.sync_all().await?;
            if dest.metadata().await?.len() != content_length {
                return Err(StorageError::Io(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "upload relay body did not match its declared content length",
                )));
            }
            Ok(())
        }
        .await;
        if result.is_err() {
            let _ = tokio::fs::remove_file(&path).await;
        }
        result
    }
}

fn local_content_type(key: &str) -> String {
    mime::extension_mime(key)
        .unwrap_or("application/octet-stream")
        .to_owned()
}

fn local_identity(bucket: &str, key: &str, metadata: &Metadata) -> SourceObjectIdentity {
    local_source_object_identity(LocalSourceObject {
        bucket,
        key,
        content_length: metadata.len(),
        content_type: &local_content_type(key),
        modified_nanos: i128::from(metadata.mtime()) * 1_000_000_000
            + i128::from(metadata.mtime_nsec()),
        inode: metadata.ino(),
    })
}

async fn open_local_read_file(path: &Path) -> Result<(tokio::fs::File, Metadata), StorageError> {
    let mut options = tokio::fs::OpenOptions::new();
    options.read(true).custom_flags(LOCAL_OPEN_FLAGS);
    let file = options.open(path).await.map_err(map_local_open_error)?;
    let metadata = file.metadata().await?;
    if !metadata.is_file() {
        return Err(StorageError::NotFound);
    }
    Ok((file, metadata))
}

fn map_local_open_error(error: std::io::Error) -> StorageError {
    if error.raw_os_error() == Some(libc::ELOOP) {
        return StorageError::InvalidKey;
    }
    map_not_found(error)
}

/// Walks the resolved path and refuses any component that is a symlink.
///
/// This is deliberately synchronous. It runs once per storage operation and touches one dentry
/// per path component, which the kernel serves from cache in well under a microsecond. Issuing
/// each `symlink_metadata` through `tokio::fs` instead turns a handful of cheap syscalls into one
/// blocking-threadpool round-trip *per component*, which measured as a 2-3x throughput loss on
/// every storage-backed request.
fn reject_symlink_chain(path: &Path) -> Result<(), StorageError> {
    let mut current = PathBuf::new();
    for component in path.components() {
        current.push(component.as_os_str());
        match std::fs::symlink_metadata(&current) {
            Ok(metadata) if metadata.file_type().is_symlink() => {
                return Err(StorageError::InvalidKey);
            }
            Ok(_) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(StorageError::Io(error)),
        }
    }
    Ok(())
}
