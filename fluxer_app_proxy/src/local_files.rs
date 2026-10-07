// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::routes::capacity_refused_response;
use crate::routes::file_stream::stream_file;
use crate::state::AppProxyBudgets;
use crate::static_asset_policy::{accepts_content_encoding, set_vary_on_accept_encoding};
use anyhow::Context;
use axum::{
    body::{Body, BodyDataStream, Bytes},
    http::{HeaderMap, HeaderValue, StatusCode, header},
    response::{IntoResponse, Response},
};
use futures_util::Stream;
use sha2::{Digest, Sha256};
use std::fmt;
use std::path::{Component, Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context as TaskContext, Poll};
use tokio::io::AsyncReadExt;
use tokio::sync::{OwnedSemaphorePermit, Semaphore, TryAcquireError};

const LOCAL_FILE_BUFFER_BUDGET_BYTES: usize = 128 * 1024 * 1024;
const LOCAL_FILE_STREAM_BUFFER_BYTES: usize = 256 * 1024;
const PRECOMPRESSED_SIBLINGS: &[(&str, &str)] = &[("br", "br"), ("gzip", "gz")];

#[derive(Clone)]
pub(crate) struct LocalFileStore {
    root: Arc<PathBuf>,
    read_slots: Arc<Semaphore>,
    buffer_budget: Arc<Semaphore>,
}

impl LocalFileStore {
    pub(crate) async fn load(root: &Path, budgets: &AppProxyBudgets) -> anyhow::Result<Self> {
        let root = tokio::fs::canonicalize(root)
            .await
            .with_context(|| format!("failed to resolve static directory {}", root.display()))?;
        let metadata = tokio::fs::metadata(&root)
            .await
            .with_context(|| format!("failed to inspect static directory {}", root.display()))?;
        Self::from_resolved_root(root, metadata, budgets)
    }

    fn from_resolved_root(
        root: PathBuf,
        metadata: std::fs::Metadata,
        budgets: &AppProxyBudgets,
    ) -> anyhow::Result<Self> {
        anyhow::ensure!(
            root.is_absolute(),
            "static directory must resolve absolutely"
        );
        anyhow::ensure!(metadata.is_dir(), "{} is not a directory", root.display());
        Ok(Self {
            root: Arc::new(root),
            read_slots: Arc::clone(&budgets.local_read_slots),
            buffer_budget: Arc::new(Semaphore::new(LOCAL_FILE_BUFFER_BUDGET_BYTES)),
        })
    }

    #[cfg(test)]
    pub(crate) fn load_blocking(root: &Path, budgets: &AppProxyBudgets) -> anyhow::Result<Self> {
        let root = std::fs::canonicalize(root)
            .with_context(|| format!("failed to resolve static directory {}", root.display()))?;
        let metadata = std::fs::metadata(&root)
            .with_context(|| format!("failed to inspect static directory {}", root.display()))?;
        Self::from_resolved_root(root, metadata, budgets)
    }

    async fn resolve_checked(
        &self,
        relative_path: &str,
        max_bytes: usize,
    ) -> Result<ResolvedLocalFile, LocalFileReadError> {
        let relative = Path::new(relative_path);
        if relative.as_os_str().is_empty()
            || relative
                .components()
                .any(|component| !matches!(component, Component::Normal(_)))
        {
            return Err(LocalFileReadError::InvalidPath(relative_path.to_owned()));
        }

        let read_slot = match self.read_slots.clone().try_acquire_owned() {
            Ok(permit) => permit,
            Err(TryAcquireError::NoPermits) => {
                return Err(LocalFileReadError::Busy(relative_path.to_owned()));
            }
            Err(TryAcquireError::Closed) => {
                panic!("local file read capacity semaphore closed unexpectedly")
            }
        };

        let requested = self.root.join(relative);
        let resolved = match tokio::fs::canonicalize(&requested).await {
            Ok(path) => path,
            Err(source) if source.kind() == std::io::ErrorKind::NotFound => {
                return Err(LocalFileReadError::NotFound(relative_path.to_owned()));
            }
            Err(source) => {
                return Err(LocalFileReadError::Io {
                    path: requested,
                    source,
                });
            }
        };
        if !resolved.starts_with(self.root.as_ref()) {
            return Err(LocalFileReadError::InvalidPath(relative_path.to_owned()));
        }

        let metadata =
            tokio::fs::metadata(&resolved)
                .await
                .map_err(|source| LocalFileReadError::Io {
                    path: resolved.clone(),
                    source,
                })?;
        if !metadata.is_file() {
            return Err(LocalFileReadError::NotFound(relative_path.to_owned()));
        }
        if metadata.len() > max_bytes as u64 {
            return Err(LocalFileReadError::TooLarge {
                path: relative_path.to_owned(),
                actual: metadata.len(),
                maximum: max_bytes,
            });
        }
        Ok(ResolvedLocalFile {
            read_slot,
            metadata,
            path: resolved,
        })
    }

    async fn read(
        &self,
        relative_path: &str,
        max_bytes: usize,
    ) -> Result<LocalFileContents, LocalFileReadError> {
        assert!(max_bytes <= LOCAL_FILE_BUFFER_BUDGET_BYTES);
        let ResolvedLocalFile {
            read_slot: _read_slot,
            metadata,
            path,
        } = self.resolve_checked(relative_path, max_bytes).await?;

        let expected_bytes = usize::try_from(metadata.len())
            .expect("file length within a usize byte limit must fit usize");
        let buffer_budget = self.reserve_buffer(expected_bytes, relative_path)?;
        let file = tokio::fs::File::open(&path)
            .await
            .map_err(|source| LocalFileReadError::Io {
                path: path.clone(),
                source,
            })?;
        let mut bytes = Vec::with_capacity(expected_bytes);
        file.take(metadata.len() + 1)
            .read_to_end(&mut bytes)
            .await
            .map_err(|source| LocalFileReadError::Io { path, source })?;
        if bytes.len() > expected_bytes {
            return Err(LocalFileReadError::ChangedDuringRead {
                path: relative_path.to_owned(),
                expected: expected_bytes,
            });
        }
        Ok(LocalFileContents {
            bytes,
            buffer_budget,
        })
    }

    async fn negotiate_encoding(
        &self,
        relative_path: &str,
        request_headers: &HeaderMap,
        max_bytes: usize,
    ) -> Result<NegotiatedLocalFile, LocalFileReadError> {
        for &(encoding, extension) in PRECOMPRESSED_SIBLINGS {
            if !accepts_content_encoding(request_headers, encoding) {
                continue;
            }
            let sibling_path = format!("{relative_path}.{extension}");
            match self.resolve_checked(&sibling_path, max_bytes).await {
                Ok(resolved) => {
                    return self.with_stream_budget(resolved, Some(encoding), &sibling_path);
                }
                Err(
                    LocalFileReadError::NotFound(_)
                    | LocalFileReadError::InvalidPath(_)
                    | LocalFileReadError::TooLarge { .. }
                    | LocalFileReadError::Io { .. },
                ) => continue,
                Err(error) => return Err(error),
            }
        }
        let resolved = self.resolve_checked(relative_path, max_bytes).await?;
        self.with_stream_budget(resolved, None, relative_path)
    }

    fn with_stream_budget(
        &self,
        resolved: ResolvedLocalFile,
        content_encoding: Option<&'static str>,
        relative_path: &str,
    ) -> Result<NegotiatedLocalFile, LocalFileReadError> {
        let buffer_budget = self.reserve_buffer(LOCAL_FILE_STREAM_BUFFER_BYTES, relative_path)?;
        Ok(NegotiatedLocalFile {
            entity_tag: modification_entity_tag(&resolved.metadata),
            path: resolved.path,
            content_encoding,
            buffer_budget,
        })
    }

    fn reserve_buffer(
        &self,
        bytes: usize,
        relative_path: &str,
    ) -> Result<OwnedSemaphorePermit, LocalFileReadError> {
        let permits = u32::try_from(bytes.max(1))
            .expect("local file buffer budget must fit the semaphore permit type");
        match self.buffer_budget.clone().try_acquire_many_owned(permits) {
            Ok(permit) => Ok(permit),
            Err(TryAcquireError::NoPermits) => {
                Err(LocalFileReadError::Busy(relative_path.to_owned()))
            }
            Err(TryAcquireError::Closed) => {
                panic!("local file buffer budget semaphore closed unexpectedly")
            }
        }
    }
}

struct ResolvedLocalFile {
    read_slot: OwnedSemaphorePermit,
    metadata: std::fs::Metadata,
    path: PathBuf,
}

struct NegotiatedLocalFile {
    entity_tag: Option<HeaderValue>,
    path: PathBuf,
    content_encoding: Option<&'static str>,
    buffer_budget: OwnedSemaphorePermit,
}

struct LocalFileContents {
    bytes: Vec<u8>,
    buffer_budget: OwnedSemaphorePermit,
}

struct LocalFileBody {
    bytes: Vec<u8>,
    _buffer_budget: OwnedSemaphorePermit,
}

impl AsRef<[u8]> for LocalFileBody {
    fn as_ref(&self) -> &[u8] {
        &self.bytes
    }
}

struct BudgetedLocalFileBody {
    chunks: BodyDataStream,
    _buffer_budget: OwnedSemaphorePermit,
}

impl Stream for BudgetedLocalFileBody {
    type Item = Result<Bytes, axum::Error>;

    fn poll_next(
        mut self: Pin<&mut Self>,
        context: &mut TaskContext<'_>,
    ) -> Poll<Option<Self::Item>> {
        Pin::new(&mut self.chunks).poll_next(context)
    }
}

#[derive(Clone, Copy)]
pub(crate) struct LocalFileResponseOptions {
    pub content_type: &'static str,
    pub cache_control: &'static str,
    pub allow_cross_origin: bool,
    pub max_bytes: usize,
}

pub(crate) async fn serve_binary_file(
    store: &LocalFileStore,
    relative_path: &str,
    request_headers: &HeaderMap,
    options: LocalFileResponseOptions,
) -> Response {
    let negotiated = match store
        .negotiate_encoding(relative_path, request_headers, options.max_bytes)
        .await
    {
        Ok(negotiated) => negotiated,
        Err(error) => return local_file_error_response(error),
    };
    local_file_stream_response(negotiated, relative_path, request_headers, options).await
}

pub(crate) async fn serve_text_file(
    store: &LocalFileStore,
    relative_path: &str,
    request_headers: &HeaderMap,
    options: LocalFileResponseOptions,
    replacements: &[(&str, &str)],
    finish: impl FnOnce(String) -> String,
) -> Response {
    let LocalFileContents {
        bytes,
        mut buffer_budget,
    } = match store.read(relative_path, options.max_bytes).await {
        Ok(contents) => contents,
        Err(error) => return local_file_error_response(error),
    };
    let mut text = match String::from_utf8(bytes) {
        Ok(text) => text,
        Err(_) => {
            return local_file_error_response(LocalFileReadError::InvalidUtf8(
                relative_path.to_owned(),
            ));
        }
    };
    for (pattern, replacement) in replacements {
        text = match replace_text_bounded(
            store,
            &mut buffer_budget,
            text,
            pattern,
            replacement,
            options.max_bytes,
            relative_path,
        ) {
            Ok(text) => text,
            Err(error) => return local_file_error_response(error),
        };
    }
    let text = finish(text);
    if text.len() > options.max_bytes {
        return local_file_error_response(LocalFileReadError::TooLarge {
            path: relative_path.to_owned(),
            actual: text.len() as u64,
            maximum: options.max_bytes,
        });
    }
    if text.len() > buffer_budget.num_permits() {
        buffer_budget = match store.reserve_buffer(text.len(), relative_path) {
            Ok(output_budget) => output_budget,
            Err(error) => return local_file_error_response(error),
        };
    }
    local_file_response(
        LocalFileContents {
            bytes: text.into_bytes(),
            buffer_budget,
        },
        request_headers,
        options,
    )
}

fn replace_text_bounded(
    store: &LocalFileStore,
    buffer_budget: &mut OwnedSemaphorePermit,
    text: String,
    pattern: &str,
    replacement: &str,
    max_bytes: usize,
    relative_path: &str,
) -> Result<String, LocalFileReadError> {
    assert!(!pattern.is_empty());
    let matches = text.match_indices(pattern).count();
    if matches == 0 {
        return Ok(text);
    }
    let removed = matches
        .checked_mul(pattern.len())
        .expect("replacement removal length must fit usize");
    let inserted =
        matches
            .checked_mul(replacement.len())
            .ok_or_else(|| LocalFileReadError::TooLarge {
                path: relative_path.to_owned(),
                actual: u64::MAX,
                maximum: max_bytes,
            })?;
    let output_bytes = text
        .len()
        .checked_sub(removed)
        .and_then(|length| length.checked_add(inserted))
        .ok_or_else(|| LocalFileReadError::TooLarge {
            path: relative_path.to_owned(),
            actual: u64::MAX,
            maximum: max_bytes,
        })?;
    if output_bytes > max_bytes {
        return Err(LocalFileReadError::TooLarge {
            path: relative_path.to_owned(),
            actual: output_bytes as u64,
            maximum: max_bytes,
        });
    }
    let output_budget = store.reserve_buffer(output_bytes, relative_path)?;
    let output = text.replace(pattern, replacement);
    drop(text);
    *buffer_budget = output_budget;
    Ok(output)
}

fn local_file_response(
    contents: LocalFileContents,
    request_headers: &HeaderMap,
    options: LocalFileResponseOptions,
) -> Response {
    let LocalFileContents {
        bytes,
        buffer_budget,
    } = contents;
    let digest = Sha256::digest(&bytes);
    let entity_tag = HeaderValue::from_str(&format!("W/\"{}\"", hex::encode(digest)))
        .expect("SHA-256 entity tag must be a valid HTTP header value");

    let mut response = if entity_tag_matches(request_headers, &entity_tag) {
        StatusCode::NOT_MODIFIED.into_response()
    } else {
        let body = Bytes::from_owner(LocalFileBody {
            bytes,
            _buffer_budget: buffer_budget,
        });
        Response::new(Body::from(body))
    };
    apply_local_file_headers(response.headers_mut(), Some(entity_tag), None, options);
    response
}

async fn local_file_stream_response(
    negotiated: NegotiatedLocalFile,
    relative_path: &str,
    request_headers: &HeaderMap,
    options: LocalFileResponseOptions,
) -> Response {
    let NegotiatedLocalFile {
        entity_tag,
        path,
        content_encoding,
        buffer_budget,
    } = negotiated;

    if entity_tag
        .as_ref()
        .is_some_and(|entity_tag| entity_tag_matches(request_headers, entity_tag))
    {
        let mut response = StatusCode::NOT_MODIFIED.into_response();
        apply_local_file_headers(
            response.headers_mut(),
            entity_tag,
            content_encoding,
            options,
        );
        return response;
    }

    let validator = entity_tag.as_ref().map(|entity_tag| {
        entity_tag
            .to_str()
            .expect("a local file entity tag must contain only visible ASCII")
    });
    let streamed = stream_file(&path, request_headers, validator).await;
    let response = match streamed {
        Ok(response) => response,
        Err(source) if source.kind() == std::io::ErrorKind::NotFound => {
            return local_file_error_response(LocalFileReadError::NotFound(
                relative_path.to_owned(),
            ));
        }
        Err(source) => return local_file_error_response(LocalFileReadError::Io { path, source }),
    };

    let (mut parts, body) = response.into_parts();
    apply_local_file_headers(&mut parts.headers, entity_tag, content_encoding, options);
    Response::from_parts(
        parts,
        Body::from_stream(BudgetedLocalFileBody {
            chunks: body.into_data_stream(),
            _buffer_budget: buffer_budget,
        }),
    )
}

fn modification_entity_tag(metadata: &std::fs::Metadata) -> Option<HeaderValue> {
    let modified = metadata.modified().ok()?;
    let since_epoch = modified
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .ok()?;
    HeaderValue::from_str(&format!(
        "W/\"{:x}-{:x}\"",
        metadata.len(),
        since_epoch.as_nanos()
    ))
    .ok()
}

fn entity_tag_matches(request_headers: &HeaderMap, entity_tag: &HeaderValue) -> bool {
    let entity_tag_text = entity_tag
        .to_str()
        .expect("a local file entity tag must contain only visible ASCII");
    let entity_tag_opaque = entity_tag_text
        .strip_prefix("W/")
        .unwrap_or(entity_tag_text);
    request_headers
        .get(header::IF_NONE_MATCH)
        .and_then(|value| value.to_str().ok())
        .is_some_and(|value| {
            value.split(',').any(|candidate| {
                let candidate = candidate.trim();
                candidate == "*"
                    || candidate.strip_prefix("W/").unwrap_or(candidate) == entity_tag_opaque
            })
        })
}

fn apply_local_file_headers(
    headers: &mut HeaderMap,
    entity_tag: Option<HeaderValue>,
    content_encoding: Option<&'static str>,
    options: LocalFileResponseOptions,
) {
    headers.insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static(options.content_type),
    );
    headers.insert(
        header::CACHE_CONTROL,
        HeaderValue::from_static(options.cache_control),
    );
    if let Some(entity_tag) = entity_tag {
        headers.insert(header::ETAG, entity_tag);
    }
    if let Some(content_encoding) = content_encoding {
        headers.insert(
            header::CONTENT_ENCODING,
            HeaderValue::from_static(content_encoding),
        );
    }
    set_vary_on_accept_encoding(headers);
    if options.allow_cross_origin {
        headers.insert(
            header::ACCESS_CONTROL_ALLOW_ORIGIN,
            HeaderValue::from_static("*"),
        );
    }
}

#[derive(Debug)]
pub(crate) enum LocalFileReadError {
    NotFound(String),
    InvalidPath(String),
    Busy(String),
    ChangedDuringRead {
        path: String,
        expected: usize,
    },
    TooLarge {
        path: String,
        actual: u64,
        maximum: usize,
    },
    InvalidUtf8(String),
    Io {
        path: PathBuf,
        source: std::io::Error,
    },
}

impl fmt::Display for LocalFileReadError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotFound(path) => write!(formatter, "local file {path:?} was not found"),
            Self::InvalidPath(path) => write!(formatter, "local file path {path:?} is invalid"),
            Self::Busy(path) => {
                write!(formatter, "local file capacity is unavailable for {path:?}")
            }
            Self::ChangedDuringRead { path, expected } => write!(
                formatter,
                "local file {path:?} grew after its {expected} byte size was inspected"
            ),
            Self::TooLarge {
                path,
                actual,
                maximum,
            } => write!(
                formatter,
                "local file {path:?} is {actual} bytes and exceeds the {maximum} byte limit"
            ),
            Self::InvalidUtf8(path) => write!(formatter, "local text file {path:?} is not UTF-8"),
            Self::Io { path, source } => {
                write!(
                    formatter,
                    "failed to read local file {}: {source}",
                    path.display()
                )
            }
        }
    }
}

impl std::error::Error for LocalFileReadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io { source, .. } => Some(source),
            _ => None,
        }
    }
}

fn local_file_error_response(error: LocalFileReadError) -> Response {
    match error {
        LocalFileReadError::NotFound(_) | LocalFileReadError::InvalidPath(_) => {
            StatusCode::NOT_FOUND.into_response()
        }
        LocalFileReadError::Busy(_) => capacity_refused_response(),
        error => {
            tracing::error!(%error, "failed to serve local file");
            (StatusCode::INTERNAL_SERVER_ERROR, "Internal Server Error").into_response()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::LOCAL_FILE_READS_IN_FLIGHT_MAX;

    fn temp_root(name: &str) -> PathBuf {
        let root = std::env::temp_dir().join(format!(
            "fluxer-app-proxy-local-files-{}-{name}",
            std::process::id()
        ));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).unwrap();
        root
    }

    fn text_options(max_bytes: usize) -> LocalFileResponseOptions {
        LocalFileResponseOptions {
            content_type: "text/plain; charset=utf-8",
            cache_control: "no-cache",
            allow_cross_origin: false,
            max_bytes,
        }
    }

    async fn body_of(response: Response) -> Vec<u8> {
        axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap()
            .to_vec()
    }

    #[tokio::test]
    async fn an_open_response_never_holds_a_read_slot_against_the_next_reader() {
        let root = temp_root("in-flight");
        std::fs::write(root.join("robots.txt"), "User-agent: *\n").unwrap();
        let store = LocalFileStore::load(&root, &AppProxyBudgets::new())
            .await
            .unwrap();

        let mut open = Vec::with_capacity(LOCAL_FILE_READS_IN_FLIGHT_MAX + 1);
        for _ in 0..=LOCAL_FILE_READS_IN_FLIGHT_MAX {
            open.push(
                serve_binary_file(&store, "robots.txt", &HeaderMap::new(), text_options(1024))
                    .await,
            );
        }

        let refused = open
            .iter()
            .filter(|response| response.status() != StatusCode::OK)
            .count();
        assert_eq!(
            refused, 0,
            "{refused} readers were turned away while earlier responses were still open"
        );
        assert_eq!(
            body_of(open.pop().unwrap()).await,
            b"User-agent: *\n",
            "a response served past the read slot count carried the wrong bytes"
        );

        std::fs::remove_dir_all(&root).unwrap();
    }

    #[tokio::test]
    async fn concurrent_bundle_responses_are_not_charged_their_whole_file_size() {
        let root = temp_root("bundle-budget");
        let bundle_bytes = 8 * 1024 * 1024;
        std::fs::write(root.join("bundle.js"), vec![b'x'; bundle_bytes]).unwrap();
        let store = LocalFileStore::load(&root, &AppProxyBudgets::new())
            .await
            .unwrap();

        let mut open = Vec::with_capacity(32);
        for _ in 0..32 {
            open.push(
                serve_binary_file(
                    &store,
                    "bundle.js",
                    &HeaderMap::new(),
                    text_options(100 * 1024 * 1024),
                )
                .await,
            );
        }

        let refused = open
            .iter()
            .filter(|response| response.status() != StatusCode::OK)
            .count();
        assert_eq!(
            refused, 0,
            "{refused} of 32 concurrent bundle responses were refused for buffer capacity"
        );
        let last = open.pop().unwrap();
        assert_eq!(
            last.headers().get(header::CONTENT_LENGTH).unwrap(),
            bundle_bytes.to_string().as_str(),
            "a streamed bundle response did not declare its length"
        );
        let body = body_of(last).await;
        assert_eq!(body.len(), bundle_bytes);
        assert!(
            body.iter().all(|byte| *byte == b'x'),
            "a streamed bundle response carried bytes the file does not hold"
        );

        std::fs::remove_dir_all(&root).unwrap();
    }

    #[tokio::test]
    async fn a_path_that_leaves_the_static_root_is_not_served() {
        let root = temp_root("escape");
        let outside = root.parent().unwrap().join("outside.txt");
        std::fs::write(root.join("robots.txt"), "User-agent: *\n").unwrap();
        std::fs::write(&outside, "secret").unwrap();
        let store = LocalFileStore::load(&root, &AppProxyBudgets::new())
            .await
            .unwrap();

        for path in [
            "../outside.txt",
            "/etc/hosts",
            "nested/../../outside.txt",
            "",
        ] {
            let response =
                serve_binary_file(&store, path, &HeaderMap::new(), text_options(1024)).await;
            assert_eq!(
                response.status(),
                StatusCode::NOT_FOUND,
                "{path} was served from outside the static root"
            );
        }

        std::fs::remove_file(&outside).unwrap();
        std::fs::remove_dir_all(&root).unwrap();
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn a_symlink_pointing_out_of_the_static_root_is_not_served() {
        let root = temp_root("symlink");
        let outside = root.parent().unwrap().join("symlink-target.txt");
        std::fs::write(&outside, "secret").unwrap();
        std::os::unix::fs::symlink(&outside, root.join("leak.txt")).unwrap();
        let store = LocalFileStore::load(&root, &AppProxyBudgets::new())
            .await
            .unwrap();

        let response =
            serve_binary_file(&store, "leak.txt", &HeaderMap::new(), text_options(1024)).await;

        assert_eq!(
            response.status(),
            StatusCode::NOT_FOUND,
            "a symlink resolved outside the static root and was served anyway"
        );

        std::fs::remove_file(&outside).unwrap();
        std::fs::remove_dir_all(&root).unwrap();
    }

    #[tokio::test]
    async fn a_revalidating_client_is_answered_from_its_entity_tag() {
        let root = temp_root("revalidate");
        std::fs::write(root.join("robots.txt"), "User-agent: *\n").unwrap();
        let store = LocalFileStore::load(&root, &AppProxyBudgets::new())
            .await
            .unwrap();

        let first =
            serve_binary_file(&store, "robots.txt", &HeaderMap::new(), text_options(1024)).await;
        assert_eq!(first.status(), StatusCode::OK);
        let entity_tag = first.headers().get(header::ETAG).unwrap().clone();

        let mut revalidation = HeaderMap::new();
        revalidation.insert(header::IF_NONE_MATCH, entity_tag.clone());
        let second =
            serve_binary_file(&store, "robots.txt", &revalidation, text_options(1024)).await;
        assert_eq!(second.status(), StatusCode::NOT_MODIFIED);
        assert_eq!(second.headers().get(header::ETAG), Some(&entity_tag));
        assert!(body_of(second).await.is_empty());

        let mut stale = HeaderMap::new();
        stale.insert(header::IF_NONE_MATCH, HeaderValue::from_static("\"other\""));
        let third = serve_binary_file(&store, "robots.txt", &stale, text_options(1024)).await;
        assert_eq!(
            third.status(),
            StatusCode::OK,
            "a client holding a different entity tag was told its copy was current"
        );

        std::fs::remove_dir_all(&root).unwrap();
    }

    #[tokio::test]
    async fn a_file_over_its_response_limit_is_refused_before_it_is_buffered() {
        let root = temp_root("too-large");
        std::fs::write(root.join("version.json"), "x".repeat(4096)).unwrap();
        let store = LocalFileStore::load(&root, &AppProxyBudgets::new())
            .await
            .unwrap();

        let response = serve_binary_file(
            &store,
            "version.json",
            &HeaderMap::new(),
            text_options(1024),
        )
        .await;

        assert_eq!(response.status(), StatusCode::INTERNAL_SERVER_ERROR);
        assert_eq!(
            store.buffer_budget.available_permits(),
            LOCAL_FILE_BUFFER_BUDGET_BYTES,
            "a refused file still holds its buffer budget"
        );

        std::fs::remove_dir_all(&root).unwrap();
    }

    #[tokio::test]
    async fn a_replacement_that_grows_past_the_response_limit_is_refused() {
        let root = temp_root("replacement");
        std::fs::write(root.join("sw.js"), "const endpoint = '{{MEDIA_ENDPOINT}}';").unwrap();
        let store = LocalFileStore::load(&root, &AppProxyBudgets::new())
            .await
            .unwrap();

        let served = serve_text_file(
            &store,
            "sw.js",
            &HeaderMap::new(),
            text_options(1024),
            &[("{{MEDIA_ENDPOINT}}", "https://media.example.test")],
            std::convert::identity,
        )
        .await;
        assert_eq!(served.status(), StatusCode::OK);
        assert_eq!(
            String::from_utf8(body_of(served).await).unwrap(),
            "const endpoint = 'https://media.example.test';"
        );

        let overflowed = serve_text_file(
            &store,
            "sw.js",
            &HeaderMap::new(),
            text_options(64),
            &[("{{MEDIA_ENDPOINT}}", &"x".repeat(64))],
            std::convert::identity,
        )
        .await;
        assert_eq!(overflowed.status(), StatusCode::INTERNAL_SERVER_ERROR);
        assert_eq!(
            store.buffer_budget.available_permits(),
            LOCAL_FILE_BUFFER_BUDGET_BYTES,
            "a refused replacement still holds its buffer budget"
        );

        std::fs::remove_dir_all(&root).unwrap();
    }

    #[tokio::test]
    async fn an_exhausted_buffer_budget_asks_the_client_to_retry() {
        let root = temp_root("busy");
        std::fs::write(root.join("robots.txt"), "User-agent: *\n").unwrap();
        let store = LocalFileStore::load(&root, &AppProxyBudgets::new())
            .await
            .unwrap();
        let held = store
            .buffer_budget
            .clone()
            .try_acquire_many_owned(u32::try_from(LOCAL_FILE_BUFFER_BUDGET_BYTES).unwrap())
            .unwrap();

        let response =
            serve_binary_file(&store, "robots.txt", &HeaderMap::new(), text_options(1024)).await;

        assert_eq!(response.status(), StatusCode::SERVICE_UNAVAILABLE);
        assert_eq!(
            response.headers().get(header::RETRY_AFTER).unwrap(),
            "1",
            "a client refused for capacity was not told when to come back"
        );
        assert_eq!(
            response.headers().get(header::CACHE_CONTROL).unwrap(),
            "no-store"
        );
        drop(held);

        std::fs::remove_dir_all(&root).unwrap();
    }
}
