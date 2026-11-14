// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    BufferedObjectReadRequest, BufferedStorageObject, ContentDigestRequest, HeadResult,
    ObjectStreamPlan, STORAGE_STREAM_CHUNK_BYTES, StorageError, Store, StreamObject, StreamRange,
    keys::{map_segment, safe_bucket, safe_key, valid_upload_id},
    relay_body::{RelayBody, RelayPutOptions},
    response_body::{exact_byte_stream, local_reader_stream, read_exact_bytes},
};
use crate::{mime, range};
use anyhow::{Context as _, bail};
use axum::body::Body;
use futures_util::StreamExt as _;
use http::{HeaderValue, StatusCode};
use rand::RngExt as _;
use serde::{Deserialize, Serialize};
use sha2::{Digest as _, Sha256};
use std::{
    borrow::Cow,
    ffi::{CStr, CString},
    fs::Metadata,
    io::Read as _,
    os::{
        fd::{AsRawFd as _, FromRawFd as _, OwnedFd},
        unix::fs::{MetadataExt as _, OpenOptionsExt as _},
    },
};
use tokio::io::{AsyncReadExt as _, AsyncSeekExt as _, AsyncWriteExt as _};
use tracing::warn;

const LOCAL_OPEN_FLAGS: i32 = libc::O_CLOEXEC | libc::O_NOFOLLOW | libc::O_NONBLOCK;
const LOCAL_DIR_FLAGS: i32 =
    libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
const LOCAL_SMALL_READ_FLAGS: i32 = libc::O_RDONLY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
const LOCAL_TEMP_FLAGS: i32 =
    libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL | libc::O_NOFOLLOW | libc::O_CLOEXEC;
const LOCAL_DIR_MODE: libc::mode_t = 0o2770;
const LOCAL_FILE_MODE: libc::mode_t = 0o660;
const LOCAL_FILE_CREATE_MODE: libc::c_uint = 0o660;

const LAYOUT_FILE: &str = ".layout";
const LAYOUT_VERSION: u64 = 1;
const META_DIR: &str = ".meta";
const STAGING_DIR: &str = ".staging";
const STAGING_TMP: &str = "tmp";
const STAGING_MULTIPART: &str = "multipart";
const STAGING_SUBDIRS: [&str; 3] = [STAGING_TMP, STAGING_MULTIPART, "trash"];
const MULTIPART_MANIFEST: &str = "upload.json";
const MULTIPART_MAX_PART: u32 = 10_000;
const SMALL_FILE_MAX_BYTES: u64 = 256 * 1024;

#[derive(Deserialize)]
struct LayoutFile {
    v: u64,
}

#[derive(Deserialize, Serialize)]
struct Sidecar {
    v: u64,
    #[serde(
        rename = "contentType",
        default,
        skip_serializing_if = "Option::is_none"
    )]
    content_type: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    key: Option<String>,
}

#[derive(Deserialize)]
struct MultipartManifest {
    v: u64,
    bucket: String,
    key: String,
}

struct ObjectPath {
    data: Vec<CString>,
    mapped: bool,
}

impl ObjectPath {
    fn new(bucket: &str, key: &str) -> Result<Self, StorageError> {
        safe_bucket(bucket)?;
        safe_key(key)?;
        let mut data = vec![CString::new(bucket).map_err(|_| StorageError::InvalidBucket)?];
        let mut mapped = false;
        for segment in key.split('/') {
            let name = map_segment(segment);
            mapped |= matches!(name, Cow::Owned(_));
            data.push(CString::new(name.as_bytes()).map_err(|_| StorageError::InvalidKey)?);
        }
        Ok(Self { data, mapped })
    }

    fn meta(&self) -> Vec<CString> {
        std::iter::once(c_name(META_DIR))
            .chain(self.data.iter().cloned())
            .collect()
    }
}

struct PartTarget {
    bucket: String,
    key: String,
    upload_id: String,
    part_number: u32,
}

impl PartTarget {
    fn new(
        bucket: &str,
        key: &str,
        upload_id: &str,
        part_number: u32,
    ) -> Result<Self, StorageError> {
        safe_bucket(bucket)?;
        safe_key(key)?;
        if !valid_upload_id(upload_id) || !(1..=MULTIPART_MAX_PART).contains(&part_number) {
            return Err(StorageError::NoSuchUpload);
        }
        Ok(Self {
            bucket: bucket.to_owned(),
            key: key.to_owned(),
            upload_id: upload_id.to_owned(),
            part_number,
        })
    }

    fn part_path(&self) -> CString {
        c_name(&format!("{}/part-{:05}", self.upload_id, self.part_number))
    }
}

enum WriteTarget {
    Object {
        path: ObjectPath,
        key: String,
        content_type: Option<String>,
    },
    Part(PartTarget),
}

struct OpenUpload {
    multipart: OwnedFd,
    upload: OwnedFd,
}

struct StagedFile {
    tmp: OwnedFd,
    name: CString,
}

impl StagedFile {
    fn create(root: &OwnedFd) -> Result<(Self, std::fs::File), StorageError> {
        let tmp = open_staging_tmp(root)?;
        let name = c_name(&random_hex());
        let fd = open_at(&tmp, &name, LOCAL_TEMP_FLAGS, LOCAL_FILE_CREATE_MODE)?;
        let staged = Self { tmp, name };
        if let Err(error) = fchmod(&fd, LOCAL_FILE_MODE) {
            staged.discard();
            return Err(error.into());
        }
        Ok((staged, std::fs::File::from(fd)))
    }

    fn discard(self) {
        let _ = unlink_at(&self.tmp, &self.name);
    }
}

impl Store {
    pub(super) async fn read_local(
        &self,
        request: BufferedObjectReadRequest<'_>,
    ) -> Result<BufferedStorageObject, StorageError> {
        let (file, metadata, stored_type) =
            self.open_local_object(request.bucket, request.key).await?;
        if metadata.len() > request.limit as u64 {
            return Err(StorageError::StreamTooLong);
        }
        let content_length =
            usize::try_from(metadata.len()).map_err(|_| StorageError::StreamTooLong)?;
        let data = read_exact_bytes(file, content_length, request.budget).await?;
        let content_type = stored_type
            .unwrap_or_else(|| mime::detect(&data[..data.len().min(8192)], request.key, None));
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
        let path = ObjectPath::new(bucket, key)?;
        let root = self.cfg.storage.root.clone();
        let (content_length, stored_type) = blocking(move || {
            let root = open_root(&root)?;
            let (parent, name) = walk(&root, &path.data, false)?;
            let stat = stat_at(&parent, name).map_err(map_local_error)?;
            match stat.st_mode & libc::S_IFMT {
                libc::S_IFREG => {}
                libc::S_IFLNK => return Err(StorageError::InvalidKey),
                _ => return Err(StorageError::NotFound),
            }
            let stored_type = read_sidecar(&root, &path)?.and_then(sidecar_content_type);
            Ok((u64::try_from(stat.st_size).unwrap_or(0), stored_type))
        })
        .await?;
        Ok(HeadResult {
            content_length,
            content_type: stored_type.unwrap_or_else(|| inferred_content_type(key)),
        })
    }

    pub(super) async fn stream_local(
        &self,
        plan: ObjectStreamPlan<'_>,
    ) -> Result<StreamObject, StorageError> {
        let (mut file, metadata, stored_type) =
            self.open_local_object(plan.bucket, plan.key).await?;
        let content_type = stored_type.unwrap_or_else(|| inferred_content_type(plan.key));
        let total_len = usize::try_from(metadata.len()).map_err(|_| StorageError::StreamTooLong)?;
        let byte_range = match plan.range {
            StreamRange::Full => None,
            StreamRange::Header(header) => match range::parse_range(Some(header), total_len) {
                range::RangeSelection::Partial(byte_range) => Some(byte_range),
                range::RangeSelection::Full => None,
                range::RangeSelection::Unsatisfiable => {
                    return Ok(StreamObject {
                        body: Body::empty(),
                        status: StatusCode::RANGE_NOT_SATISFIABLE,
                        content_length: Some(0),
                        content_type,
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
            content_type,
            byte_range,
            total_length: Some(metadata.len()),
        })
    }

    pub(super) async fn write_local_relay(
        &self,
        bucket: &str,
        key: &str,
        options: RelayPutOptions,
    ) -> Result<Option<String>, StorageError> {
        let target = match (options.upload_id.as_deref(), options.part_number) {
            (None, None) => WriteTarget::Object {
                path: ObjectPath::new(bucket, key)?,
                key: key.to_owned(),
                content_type: options.content_type,
            },
            (Some(upload_id), Some(part_number)) => {
                WriteTarget::Part(PartTarget::new(bucket, key, upload_id, part_number)?)
            }
            _ => return Err(StorageError::NoSuchUpload),
        };
        let root_path = self.cfg.storage.root.clone();
        let (root, target, upload, staged, file) = blocking(move || {
            let root = open_root(&root_path)?;
            let upload = match &target {
                WriteTarget::Part(part) => Some(open_upload(&root, part)?),
                WriteTarget::Object { .. } => None,
            };
            let (staged, file) = StagedFile::create(&root)?;
            Ok((root, target, upload, staged, file))
        })
        .await?;
        let mut dest = tokio::fs::File::from_std(file);
        let content_length = options.content_length;
        let written = async {
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
            let metadata = dest.metadata().await?;
            if metadata.len() != content_length {
                return Err(StorageError::Io(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "upload relay body did not match its declared content length",
                )));
            }
            Ok(metadata)
        }
        .await;
        drop(dest);
        let metadata = match written {
            Ok(metadata) => metadata,
            Err(error) => {
                let _ = blocking(move || {
                    staged.discard();
                    Ok(())
                })
                .await;
                return Err(error);
            }
        };
        let etag = local_etag(&metadata);
        blocking(move || commit(&root, staged, target, upload)).await?;
        Ok(Some(etag))
    }

    async fn open_local_object(
        &self,
        bucket: &str,
        key: &str,
    ) -> Result<(tokio::fs::File, Metadata, Option<String>), StorageError> {
        let path = ObjectPath::new(bucket, key)?;
        let root = self.cfg.storage.root.clone();
        let (file, metadata, stored_type) = blocking(move || {
            let root = open_root(&root)?;
            let (parent, name) = walk(&root, &path.data, false)?;
            let file = open_at(&parent, name, libc::O_RDONLY | LOCAL_OPEN_FLAGS, 0)
                .map(std::fs::File::from)
                .map_err(map_local_error)?;
            let metadata = file.metadata()?;
            if !metadata.is_file() {
                return Err(StorageError::NotFound);
            }
            let stored_type = read_sidecar(&root, &path)?.and_then(sidecar_content_type);
            Ok((file, metadata, stored_type))
        })
        .await?;
        Ok((tokio::fs::File::from_std(file), metadata, stored_type))
    }
}

pub(super) fn prepare_local_root(root: &str) -> anyhow::Result<()> {
    let root_fd = open_root(root)
        .map_err(anyhow::Error::from)
        .with_context(|| format!("can't open the storage root {root}"))?;
    let layout = read_small_file(&root_fd, &c_name(LAYOUT_FILE))
        .map_err(anyhow::Error::from)
        .with_context(|| format!("can't read {root}/{LAYOUT_FILE}"))?;
    let Some(layout) = layout else {
        bail!("{root}/{LAYOUT_FILE} is missing, so {root} is not a prepared storage root");
    };
    let version = serde_json::from_slice::<LayoutFile>(&layout)
        .with_context(|| format!("{root}/{LAYOUT_FILE} is not valid"))?
        .v;
    if version != LAYOUT_VERSION {
        bail!(
            "{root}/{LAYOUT_FILE} has version {version}, this media proxy reads version {LAYOUT_VERSION}"
        );
    }
    let root_dev = fstat(&root_fd)?.st_dev;
    let staging = ensure_dir(&root_fd, &c_name(STAGING_DIR))?;
    let mut dirs = vec![(STAGING_DIR.to_owned(), staging.try_clone()?)];
    for sub in STAGING_SUBDIRS {
        dirs.push((
            format!("{STAGING_DIR}/{sub}"),
            ensure_dir(&staging, &c_name(sub))?,
        ));
    }
    for (name, dir) in dirs {
        if fstat(&dir)?.st_dev != root_dev {
            bail!("{root}/{name} is on a different filesystem than {root}");
        }
    }
    Ok(())
}

fn commit(
    root: &OwnedFd,
    staged: StagedFile,
    target: WriteTarget,
    upload: Option<OpenUpload>,
) -> Result<(), StorageError> {
    let result = match (&target, upload) {
        (WriteTarget::Part(part), Some(upload)) => commit_part(&staged, part, &upload),
        (
            WriteTarget::Object {
                path,
                key,
                content_type,
            },
            _,
        ) => commit_object(root, &staged, path, key, content_type.as_deref()),
        (WriteTarget::Part(_), None) => Err(StorageError::NoSuchUpload),
    };
    if result.is_err() {
        staged.discard();
    }
    result
}

fn commit_part(
    staged: &StagedFile,
    part: &PartTarget,
    upload: &OpenUpload,
) -> Result<(), StorageError> {
    rename_at(
        &staged.tmp,
        &staged.name,
        &upload.multipart,
        &part.part_path(),
    )
    .map_err(|error| match error.raw_os_error() {
        Some(libc::ENOENT | libc::ENOTDIR) => StorageError::NoSuchUpload,
        _ => StorageError::Io(error),
    })?;
    fsync(&upload.upload)?;
    Ok(())
}

fn commit_object(
    root: &OwnedFd,
    staged: &StagedFile,
    path: &ObjectPath,
    key: &str,
    content_type: Option<&str>,
) -> Result<(), StorageError> {
    let needs_sidecar =
        path.mapped || content_type.is_some_and(|stored| Some(stored) != mime::extension_mime(key));
    if needs_sidecar {
        let sidecar = Sidecar {
            v: 1,
            content_type: content_type.map(ToOwned::to_owned),
            key: path.mapped.then(|| key.to_owned()),
        };
        write_sidecar(root, &path.meta(), &sidecar)?;
    } else {
        remove_sidecar(root, &path.meta())?;
    }
    let (parent, name) = walk(root, &path.data, true)?;
    rename_at(&staged.tmp, &staged.name, &parent, name).map_err(map_local_error)?;
    fsync(&parent)?;
    Ok(())
}

fn write_sidecar(root: &OwnedFd, meta: &[CString], sidecar: &Sidecar) -> Result<(), StorageError> {
    let body = serde_json::to_vec(sidecar).map_err(std::io::Error::other)?;
    let (staged, mut file) = StagedFile::create(root)?;
    let result = (|| {
        std::io::Write::write_all(&mut file, &body)?;
        file.sync_all()?;
        drop(file);
        let (parent, name) = walk(root, meta, true)?;
        rename_at(&staged.tmp, &staged.name, &parent, name).map_err(map_local_error)?;
        fsync(&parent)?;
        Ok(())
    })();
    if result.is_err() {
        staged.discard();
    }
    result
}

fn remove_sidecar(root: &OwnedFd, meta: &[CString]) -> Result<(), StorageError> {
    let (parent, name) = match walk(root, meta, false) {
        Ok(found) => found,
        Err(StorageError::NotFound) => return Ok(()),
        Err(error) => return Err(error),
    };
    match unlink_at(&parent, name) {
        Ok(()) => fsync(&parent).map_err(StorageError::from),
        Err(error) if is_missing(&error) => Ok(()),
        Err(error) => Err(error.into()),
    }
}

fn read_sidecar(root: &OwnedFd, path: &ObjectPath) -> Result<Option<Sidecar>, StorageError> {
    let meta = path.meta();
    let (parent, name) = match walk(root, &meta, false) {
        Ok(found) => found,
        Err(StorageError::NotFound) => return Ok(None),
        Err(error) => return Err(error),
    };
    let Some(body) = read_small_file(&parent, name)? else {
        return Ok(None);
    };
    match serde_json::from_slice::<Sidecar>(&body) {
        Ok(sidecar) if sidecar.v == 1 => Ok(Some(sidecar)),
        Ok(sidecar) => {
            warn!(
                version = sidecar.v,
                "ignoring a storage sidecar with an unknown version"
            );
            Ok(None)
        }
        Err(error) => {
            warn!(error = %error, "ignoring a storage sidecar that is not valid JSON");
            Ok(None)
        }
    }
}

fn sidecar_content_type(sidecar: Sidecar) -> Option<String> {
    sidecar
        .content_type
        .filter(|value| !value.is_empty() && HeaderValue::from_str(value).is_ok())
}

fn open_upload(root: &OwnedFd, part: &PartTarget) -> Result<OpenUpload, StorageError> {
    let components = [
        c_name(STAGING_DIR),
        c_name(STAGING_MULTIPART),
        c_name(&part.upload_id),
    ];
    let (staging, multipart_name) = walk(root, &components[..2], false).map_err(no_such_upload)?;
    let multipart = open_at(&staging, multipart_name, LOCAL_DIR_FLAGS, 0)
        .map_err(map_local_error)
        .map_err(no_such_upload)?;
    let upload = open_at(&multipart, &components[2], LOCAL_DIR_FLAGS, 0)
        .map_err(map_local_error)
        .map_err(no_such_upload)?;
    let manifest =
        read_small_file(&upload, &c_name(MULTIPART_MANIFEST))?.ok_or(StorageError::NoSuchUpload)?;
    let manifest = serde_json::from_slice::<MultipartManifest>(&manifest)
        .map_err(|_| StorageError::NoSuchUpload)?;
    if manifest.v != 1 || manifest.bucket != part.bucket || manifest.key != part.key {
        return Err(StorageError::NoSuchUpload);
    }
    Ok(OpenUpload { multipart, upload })
}

fn no_such_upload(error: StorageError) -> StorageError {
    match error {
        StorageError::NotFound | StorageError::InvalidKey => StorageError::NoSuchUpload,
        other => other,
    }
}

fn open_staging_tmp(root: &OwnedFd) -> Result<OwnedFd, StorageError> {
    let components = [c_name(STAGING_DIR), c_name(STAGING_TMP)];
    let (staging, tmp) = walk(root, &components, true)?;
    ensure_dir(&staging, tmp).map_err(map_local_error)
}

fn ensure_dir(parent: &OwnedFd, name: &CStr) -> std::io::Result<OwnedFd> {
    let created = mkdir_at(parent, name)?;
    let dir = open_at(parent, name, LOCAL_DIR_FLAGS, 0)?;
    if created {
        fchmod(&dir, LOCAL_DIR_MODE)?;
        fsync(parent)?;
    }
    Ok(dir)
}

fn walk<'a>(
    root: &OwnedFd,
    components: &'a [CString],
    create: bool,
) -> Result<(OwnedFd, &'a CStr), StorageError> {
    let Some((last, parents)) = components.split_last() else {
        return Err(StorageError::InvalidKey);
    };
    let mut dir = root.try_clone()?;
    for name in parents {
        let next = if create {
            ensure_dir(&dir, name)
        } else {
            open_at(&dir, name, LOCAL_DIR_FLAGS, 0)
        };
        dir = match next {
            Ok(next) => next,
            Err(error)
                if error.raw_os_error() == Some(libc::ENOTDIR) && is_symlink_at(&dir, name) =>
            {
                return Err(StorageError::InvalidKey);
            }
            Err(error) => return Err(map_local_error(error)),
        };
    }
    Ok((dir, last.as_c_str()))
}

fn open_root(root: &str) -> Result<OwnedFd, StorageError> {
    std::fs::OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_DIRECTORY | libc::O_CLOEXEC)
        .open(root)
        .map(OwnedFd::from)
        .map_err(map_local_error)
}

fn read_small_file(dir: &OwnedFd, name: &CStr) -> Result<Option<Vec<u8>>, StorageError> {
    let file = match open_at(dir, name, LOCAL_SMALL_READ_FLAGS, 0) {
        Ok(fd) => std::fs::File::from(fd),
        Err(error) if is_missing(&error) => return Ok(None),
        Err(error) => return Err(map_local_error(error)),
    };
    let mut body = Vec::new();
    file.take(SMALL_FILE_MAX_BYTES).read_to_end(&mut body)?;
    Ok(Some(body))
}

fn inferred_content_type(key: &str) -> String {
    mime::extension_mime(key)
        .unwrap_or("application/octet-stream")
        .to_owned()
}

fn local_etag(metadata: &Metadata) -> String {
    let mtime_ns = i128::from(metadata.mtime()) * 1_000_000_000 + i128::from(metadata.mtime_nsec());
    format!("\"{:x}-{:x}\"", metadata.len(), mtime_ns.max(0))
}

fn random_hex() -> String {
    format!("{:032x}", rand::rng().random::<u128>())
}

fn c_name(name: &str) -> CString {
    CString::new(name).expect("storage layout names have no NUL byte")
}

async fn blocking<T, F>(work: F) -> Result<T, StorageError>
where
    T: Send + 'static,
    F: FnOnce() -> Result<T, StorageError> + Send + 'static,
{
    tokio::task::spawn_blocking(work)
        .await
        .map_err(std::io::Error::other)?
}

fn is_missing(error: &std::io::Error) -> bool {
    matches!(
        error.raw_os_error(),
        Some(libc::ENOENT | libc::ENOTDIR | libc::ENAMETOOLONG | libc::EISDIR)
    )
}

fn map_local_error(error: std::io::Error) -> StorageError {
    if error.raw_os_error() == Some(libc::ELOOP) {
        return StorageError::InvalidKey;
    }
    if is_missing(&error) {
        return StorageError::NotFound;
    }
    StorageError::Io(error)
}

fn open_at(dir: &OwnedFd, name: &CStr, flags: i32, mode: libc::c_uint) -> std::io::Result<OwnedFd> {
    let fd = unsafe { libc::openat(dir.as_raw_fd(), name.as_ptr(), flags, mode) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

fn mkdir_at(dir: &OwnedFd, name: &CStr) -> std::io::Result<bool> {
    if unsafe { libc::mkdirat(dir.as_raw_fd(), name.as_ptr(), LOCAL_DIR_MODE) } == 0 {
        return Ok(true);
    }
    let error = std::io::Error::last_os_error();
    if error.raw_os_error() == Some(libc::EEXIST) {
        return Ok(false);
    }
    Err(error)
}

fn rename_at(from_dir: &OwnedFd, from: &CStr, to_dir: &OwnedFd, to: &CStr) -> std::io::Result<()> {
    let rc = unsafe {
        libc::renameat(
            from_dir.as_raw_fd(),
            from.as_ptr(),
            to_dir.as_raw_fd(),
            to.as_ptr(),
        )
    };
    if rc != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}

fn fsync(fd: &OwnedFd) -> std::io::Result<()> {
    if unsafe { libc::fsync(fd.as_raw_fd()) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}

fn fchmod(fd: &OwnedFd, mode: libc::mode_t) -> std::io::Result<()> {
    if unsafe { libc::fchmod(fd.as_raw_fd(), mode) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}

fn fstat(fd: &OwnedFd) -> std::io::Result<libc::stat> {
    let mut stat = std::mem::MaybeUninit::<libc::stat>::uninit();
    if unsafe { libc::fstat(fd.as_raw_fd(), stat.as_mut_ptr()) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(unsafe { stat.assume_init() })
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
