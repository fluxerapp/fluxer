// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    BufferedObjectReadRequest, BufferedStorageObject, ContentDigestRequest, HeadResult,
    ObjectStreamPlan, STORAGE_STREAM_CHUNK_BYTES, StorageError, Store, StreamObject, StreamRange,
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
    ffi::{CStr, CString},
    fs::Metadata,
    os::{
        fd::{AsRawFd as _, FromRawFd as _, OwnedFd},
        unix::fs::OpenOptionsExt as _,
    },
};
use tokio::io::{AsyncReadExt as _, AsyncSeekExt as _, AsyncWriteExt as _};

const LOCAL_OPEN_FLAGS: i32 = libc::O_CLOEXEC | libc::O_NOFOLLOW | libc::O_NONBLOCK;
const LOCAL_DIR_FLAGS: i32 =
    libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
const LOCAL_CREATE_FLAGS: i32 =
    libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC | libc::O_NOFOLLOW | libc::O_CLOEXEC;

struct LocalEntry {
    parent: OwnedFd,
    name: CString,
}

impl Store {
    fn local_entry(&self, bucket: &str, key: &str) -> Result<LocalEntry, StorageError> {
        safe_bucket(bucket)?;
        safe_key(key)?;
        open_local_entry(&self.cfg.storage.root, bucket, key, false)
    }

    async fn create_local_file(
        &self,
        bucket: &str,
        key: &str,
    ) -> Result<(LocalEntry, tokio::fs::File), StorageError> {
        safe_bucket(bucket)?;
        safe_key(key)?;
        let root = self.cfg.storage.root.clone();
        let bucket = bucket.to_owned();
        let key = key.to_owned();
        tokio::task::spawn_blocking(move || {
            let entry = open_local_entry(&root, &bucket, &key, true)?;
            let file = open_at(&entry.parent, &entry.name, LOCAL_CREATE_FLAGS, 0o666)
                .map_err(map_local_open_error)?;
            Ok((entry, tokio::fs::File::from_std(std::fs::File::from(file))))
        })
        .await
        .map_err(std::io::Error::other)?
    }

    pub(super) async fn read_local(
        &self,
        request: BufferedObjectReadRequest<'_>,
    ) -> Result<BufferedStorageObject, StorageError> {
        let entry = self.local_entry(request.bucket, request.key)?;
        let (file, metadata) = open_local_read_file(entry).await?;
        if metadata.len() > request.limit as u64 {
            return Err(StorageError::StreamTooLong);
        }
        let content_length =
            usize::try_from(metadata.len()).map_err(|_| StorageError::StreamTooLong)?;
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
    ) -> Result<HeadResult, StorageError> {
        let entry = self.local_entry(bucket, key)?;
        let stat = stat_at(&entry.parent, &entry.name).map_err(map_not_found)?;
        match stat.st_mode & libc::S_IFMT {
            libc::S_IFREG => {}
            libc::S_IFLNK => return Err(StorageError::InvalidKey),
            _ => return Err(StorageError::NotFound),
        }
        Ok(HeadResult {
            content_length: u64::try_from(stat.st_size).unwrap_or(0),
            content_type: local_content_type(key),
        })
    }

    pub(super) async fn stream_local(
        &self,
        plan: ObjectStreamPlan<'_>,
    ) -> Result<StreamObject, StorageError> {
        let entry = self.local_entry(plan.bucket, plan.key)?;
        let (mut file, metadata) = open_local_read_file(entry).await?;
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

    #[cfg(test)]
    pub(super) async fn write_local(
        &self,
        bucket: &str,
        key: &str,
        data: &[u8],
    ) -> Result<(), StorageError> {
        let (_entry, mut file) = self.create_local_file(bucket, key).await?;
        file.write_all(data).await?;
        file.flush().await?;
        Ok(())
    }

    pub(super) async fn write_local_relay(
        &self,
        bucket: &str,
        key: &str,
        options: RelayPutOptions,
    ) -> Result<(), StorageError> {
        let (entry, mut dest) = self.create_local_file(bucket, key).await?;
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
                RelayBody::Buffered(bytes) => {
                    dest.write_all(&bytes).await?;
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
            drop(dest);
            let _ =
                tokio::task::spawn_blocking(move || unlink_at(&entry.parent, &entry.name)).await;
        }
        result
    }
}

fn local_content_type(key: &str) -> String {
    mime::extension_mime(key)
        .unwrap_or("application/octet-stream")
        .to_owned()
}

async fn open_local_read_file(
    entry: LocalEntry,
) -> Result<(tokio::fs::File, Metadata), StorageError> {
    let (file, metadata) = tokio::task::spawn_blocking(move || {
        let file = open_at(
            &entry.parent,
            &entry.name,
            libc::O_RDONLY | LOCAL_OPEN_FLAGS,
            0,
        )
        .map(std::fs::File::from)
        .map_err(map_local_open_error)?;
        let metadata = file.metadata()?;
        Ok::<_, StorageError>((file, metadata))
    })
    .await
    .map_err(std::io::Error::other)??;
    if !metadata.is_file() {
        return Err(StorageError::NotFound);
    }
    Ok((tokio::fs::File::from_std(file), metadata))
}

fn map_local_open_error(error: std::io::Error) -> StorageError {
    if error.raw_os_error() == Some(libc::ELOOP) {
        return StorageError::InvalidKey;
    }
    map_not_found(error)
}

fn open_local_entry(
    root: &str,
    bucket: &str,
    key: &str,
    create: bool,
) -> Result<LocalEntry, StorageError> {
    if create {
        std::fs::create_dir_all(root)?;
    }
    let mut dir = OwnedFd::from(
        std::fs::OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_DIRECTORY)
            .open(root)
            .map_err(map_not_found)?,
    );
    let mut components = std::iter::once(bucket).chain(key.split('/')).peekable();
    while let Some(component) = components.next() {
        let name = CString::new(component).map_err(|_| StorageError::InvalidKey)?;
        if components.peek().is_none() {
            return Ok(LocalEntry { parent: dir, name });
        }
        if create {
            mkdir_at(&dir, &name)?;
        }
        dir = match open_at(&dir, &name, LOCAL_DIR_FLAGS, 0) {
            Ok(next) => next,
            Err(error)
                if error.raw_os_error() == Some(libc::ENOTDIR) && is_symlink_at(&dir, &name) =>
            {
                return Err(StorageError::InvalidKey);
            }
            Err(error) => return Err(map_local_open_error(error)),
        };
    }
    Err(StorageError::InvalidKey)
}

fn open_at(dir: &OwnedFd, name: &CStr, flags: i32, mode: libc::c_uint) -> std::io::Result<OwnedFd> {
    let fd = unsafe { libc::openat(dir.as_raw_fd(), name.as_ptr(), flags, mode) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

fn mkdir_at(dir: &OwnedFd, name: &CStr) -> std::io::Result<()> {
    if unsafe { libc::mkdirat(dir.as_raw_fd(), name.as_ptr(), 0o777) } == 0 {
        return Ok(());
    }
    let error = std::io::Error::last_os_error();
    if error.raw_os_error() == Some(libc::EEXIST) {
        return Ok(());
    }
    Err(error)
}

fn stat_at(dir: &OwnedFd, name: &CStr) -> std::io::Result<libc::stat> {
    let mut stat = std::mem::MaybeUninit::<libc::stat>::uninit();
    let rc = unsafe {
        libc::fstatat(
            dir.as_raw_fd(),
            name.as_ptr(),
            stat.as_mut_ptr(),
            libc::AT_SYMLINK_NOFOLLOW,
        )
    };
    if rc != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(unsafe { stat.assume_init() })
}

fn is_symlink_at(dir: &OwnedFd, name: &CStr) -> bool {
    stat_at(dir, name).is_ok_and(|stat| stat.st_mode & libc::S_IFMT == libc::S_IFLNK)
}

fn unlink_at(dir: &OwnedFd, name: &CStr) -> std::io::Result<()> {
    if unsafe { libc::unlinkat(dir.as_raw_fd(), name.as_ptr(), 0) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}
