// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    aws_sigv4,
    config::{BucketStyle, Config, StorageBackend},
    constants, http_client, mime,
};
use axum::body::Body;
use bytes::Bytes;
use http::{StatusCode, header};
use http_body::{Frame, SizeHint};
use percent_encoding::{AsciiSet, CONTROLS, percent_encode};
use reqwest::Method;
use std::{
    path::{Path, PathBuf},
    pin::Pin,
    task::{Context, Poll},
    time::Duration,
};
use thiserror::Error;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncSeekExt, AsyncWriteExt, ReadBuf};
use tokio_util::io::ReaderStream;

const UNSIGNED_PAYLOAD: &str = "UNSIGNED-PAYLOAD";

const PATH_ENCODE_SET: &AsciiSet = &percent_encoding::NON_ALPHANUMERIC
    .remove(b'-')
    .remove(b'.')
    .remove(b'_')
    .remove(b'~')
    .remove(b'/');

const QUERY_ENCODE_SET: &AsciiSet = &CONTROLS
    .add(b' ')
    .add(b'"')
    .add(b'#')
    .add(b'%')
    .add(b'&')
    .add(b'+')
    .add(b'<')
    .add(b'=')
    .add(b'>')
    .add(b'?')
    .add(b'`')
    .add(b'{')
    .add(b'}');

#[derive(Clone, Debug)]
pub struct Object {
    pub data: Bytes,
    pub content_type: String,
}

pub struct StreamObject {
    pub body: Body,
    pub status: StatusCode,
    pub content_length: Option<u64>,
    pub content_type: String,
}

#[derive(Clone, Debug)]
pub struct HeadResult {
    pub content_length: u64,
    pub content_type: String,
}

pub enum RelayBody {
    Spooled(tokio::fs::File),
    Streamed(tokio::sync::mpsc::Receiver<Result<Bytes, std::io::Error>>),
}

pub struct RelayPutOptions {
    pub body: RelayBody,
    pub content_length: u64,
    pub content_type: Option<String>,
    pub upload_id: Option<String>,
    pub part_number: Option<u32>,
    pub timeout_ms: u64,
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
}

impl Store {
    pub fn new(cfg: Config) -> Self {
        Self {
            cfg,
            client: http_client::build_default(),
            raw_client: http_client::build_raw_default(),
        }
    }

    pub fn try_new(cfg: Config) -> Result<Self, reqwest::Error> {
        let options = http_client::Options {
            connect_timeout_ms: cfg.socket_io_timeout_ms.max(1),
            timeout_ms: cfg.socket_io_timeout_ms.max(1),
            ..http_client::Options::default()
        };
        let client = http_client::build(options)?;
        let raw_client = http_client::build_raw(options)?;
        Ok(Self {
            cfg,
            client,
            raw_client,
        })
    }

    pub async fn read_object(&self, bucket: &str, key: &str) -> Result<Object, StorageError> {
        let result = match self.cfg.storage_backend {
            StorageBackend::Local => self.read_local(bucket, key).await,
            StorageBackend::S3 => self.read_s3(bucket, key).await,
        };
        match &result {
            Ok(_) => crate::metrics::GLOBAL
                .storage_hits
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            Err(StorageError::NotFound) => crate::metrics::GLOBAL
                .storage_misses
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            Err(_) => crate::metrics::GLOBAL
                .storage_errors
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        };
        result
    }

    pub async fn head_object(&self, bucket: &str, key: &str) -> Result<HeadResult, StorageError> {
        match self.cfg.storage_backend {
            StorageBackend::Local => self.head_local(bucket, key).await,
            StorageBackend::S3 => self.head_s3(bucket, key).await,
        }
    }

    pub async fn stream_object(
        &self,
        bucket: &str,
        key: &str,
        range_header: Option<&str>,
    ) -> Result<StreamObject, StorageError> {
        let result = match self.cfg.storage_backend {
            StorageBackend::Local => self.stream_local(bucket, key, range_header).await,
            StorageBackend::S3 => self.stream_s3(bucket, key, range_header).await,
        };
        match &result {
            Ok(_) => crate::metrics::GLOBAL
                .storage_hits
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            Err(StorageError::NotFound) => crate::metrics::GLOBAL
                .storage_misses
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            Err(_) => crate::metrics::GLOBAL
                .storage_errors
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        };
        result
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
        match self.cfg.storage_backend {
            StorageBackend::Local => self.write_local(bucket, key, data).await,
            StorageBackend::S3 => self.write_s3(bucket, key, data, content_type).await,
        }
    }

    pub async fn ensure_bucket(&self, bucket: &str) -> Result<(), StorageError> {
        safe_bucket(bucket)?;
        match self.cfg.storage_backend {
            StorageBackend::Local => {
                tokio::fs::create_dir_all(Path::new(&self.cfg.storage_root).join(bucket)).await?;
                Ok(())
            }
            StorageBackend::S3 => {
                let url = self.s3_bucket_url(bucket)?;
                let signed = self.sign(Method::PUT, &url, &[], None, &[])?;
                let response = self
                    .client
                    .put(&url)
                    .headers(signed_headers(&signed, &self.cfg))
                    .send()
                    .await?;
                if response.status().is_success()
                    || response.status() == reqwest::StatusCode::CONFLICT
                {
                    Ok(())
                } else {
                    Err(StorageError::S3(response.status().to_string()))
                }
            }
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
        match self.cfg.storage_backend {
            StorageBackend::Local => {
                self.write_local_relay(bucket, key, options).await?;
                Ok(None)
            }
            StorageBackend::S3 => self.relay_put_s3(bucket, key, options).await,
        }
    }

    async fn write_local_relay(
        &self,
        bucket: &str,
        key: &str,
        options: RelayPutOptions,
    ) -> Result<(), StorageError> {
        let path = self.local_path(bucket, key)?;
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }
        let mut dest = tokio::fs::File::create(&path).await?;
        let result = async {
            match options.body {
                RelayBody::Spooled(mut source) => {
                    source.seek(std::io::SeekFrom::Start(0)).await?;
                    tokio::io::copy(&mut source, &mut dest).await?;
                }
                RelayBody::Streamed(mut rx) => {
                    while let Some(chunk) = rx.recv().await {
                        dest.write_all(&chunk?).await?;
                    }
                }
            }
            dest.flush().await?;
            Ok(())
        }
        .await;
        if result.is_err() {
            let _ = tokio::fs::remove_file(&path).await;
        }
        result
    }

    fn local_path(&self, bucket: &str, key: &str) -> Result<PathBuf, StorageError> {
        safe_bucket(bucket)?;
        safe_key(key)?;
        let root = Path::new(&self.cfg.storage_root);
        let path = root.join(bucket).join(key);
        reject_symlink_chain(&path)?;
        Ok(path)
    }

    async fn read_local(&self, bucket: &str, key: &str) -> Result<Object, StorageError> {
        let path = self.local_path(bucket, key)?;
        let data = tokio::fs::read(&path).await.map_err(map_not_found)?;
        if data.len() > constants::MAX_MEDIA_PROXY_BYTES {
            return Err(StorageError::StreamTooLong);
        }
        let content_type = mime::detect(&data[..data.len().min(8192)], key, None);
        Ok(Object {
            data: Bytes::from(data),
            content_type,
        })
    }

    async fn head_local(&self, bucket: &str, key: &str) -> Result<HeadResult, StorageError> {
        let path = self.local_path(bucket, key)?;
        let meta = tokio::fs::metadata(&path).await.map_err(map_not_found)?;
        if !meta.is_file() {
            return Err(StorageError::NotFound);
        }
        if meta.len() > constants::MAX_MEDIA_PROXY_BYTES as u64 {
            return Err(StorageError::StreamTooLong);
        }
        Ok(HeadResult {
            content_length: meta.len(),
            content_type: mime::extension_mime(key)
                .unwrap_or("application/octet-stream")
                .to_owned(),
        })
    }

    async fn stream_local(
        &self,
        bucket: &str,
        key: &str,
        range_header: Option<&str>,
    ) -> Result<StreamObject, StorageError> {
        let path = self.local_path(bucket, key)?;
        let meta = tokio::fs::metadata(&path).await.map_err(map_not_found)?;
        if !meta.is_file() {
            return Err(StorageError::NotFound);
        }
        if meta.len() > constants::MAX_MEDIA_PROXY_BYTES as u64 {
            return Err(StorageError::StreamTooLong);
        }
        let total_len = meta.len() as usize;
        let parsed_range = crate::range::parse_range(range_header, total_len);
        let (status, body_len, start) = if let Some(r) = parsed_range.range {
            (
                StatusCode::PARTIAL_CONTENT,
                (r.end - r.start + 1) as u64,
                r.start as u64,
            )
        } else {
            (StatusCode::OK, total_len as u64, 0)
        };
        let mut file = tokio::fs::File::open(&path).await.map_err(map_not_found)?;
        if start > 0 {
            file.seek(std::io::SeekFrom::Start(start)).await?;
        }
        let reader = file.take(body_len);
        Ok(StreamObject {
            content_length: Some(body_len),
            body: Body::from_stream(ReaderStream::new(reader)),
            status,
            content_type: mime::extension_mime(key)
                .unwrap_or("application/octet-stream")
                .to_owned(),
        })
    }

    async fn write_local(&self, bucket: &str, key: &str, data: &[u8]) -> Result<(), StorageError> {
        let path = self.local_path(bucket, key)?;
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }
        tokio::fs::write(path, data).await?;
        Ok(())
    }

    fn s3_url(&self, bucket: &str, key: &str) -> Result<String, StorageError> {
        safe_bucket(bucket)?;
        safe_key(key)?;
        object_url(
            &self.cfg.s3_endpoint,
            write_bucket_style(&self.cfg),
            bucket,
            key,
        )
    }

    fn read_endpoint_for(&self, bucket: &str) -> Option<&str> {
        self.cfg
            .s3_read_endpoint
            .as_deref()
            .filter(|_| bucket == self.cfg.s3_read_bucket)
    }

    fn s3_read_url(&self, bucket: &str, key: &str) -> Result<String, StorageError> {
        let Some(endpoint) = self.read_endpoint_for(bucket) else {
            return self.s3_url(bucket, key);
        };
        safe_bucket(bucket)?;
        safe_key(key)?;
        object_url(endpoint, self.cfg.s3_read_bucket_style, bucket, key)
    }

    fn read_status_is_miss(&self, bucket: &str, status: reqwest::StatusCode) -> bool {
        if status == reqwest::StatusCode::NOT_FOUND {
            return true;
        }
        status == reqwest::StatusCode::FORBIDDEN
            && self.read_endpoint_for(bucket).is_some()
            && !self.cfg.s3_read_signed
    }

    fn read_headers(
        &self,
        bucket: &str,
        method: Method,
        url: &str,
        extra_signed_headers: &[aws_sigv4::Header<'_>],
    ) -> Result<reqwest::header::HeaderMap, StorageError> {
        if self.read_endpoint_for(bucket).is_some() && !self.cfg.s3_read_signed {
            return Ok(reqwest::header::HeaderMap::new());
        }
        let signed = self.sign(method, url, &[], None, extra_signed_headers)?;
        Ok(signed_headers(&signed, &self.cfg))
    }

    fn s3_bucket_url(&self, bucket: &str) -> Result<String, StorageError> {
        safe_bucket(bucket)?;
        if self.cfg.s3_endpoint.is_empty() {
            return Err(StorageError::InvalidS3Endpoint);
        }
        let endpoint = self.cfg.s3_endpoint.trim_end_matches('/');
        if self.cfg.s3_force_path_style {
            return Ok(format!("{endpoint}/{bucket}"));
        }
        validate_virtual_hosted_bucket(bucket)?;
        let parsed = url::Url::parse(endpoint).map_err(|_| StorageError::InvalidS3Endpoint)?;
        let scheme = parsed.scheme();
        let host = parsed.host_str().ok_or(StorageError::InvalidS3Endpoint)?;
        let port = parsed.port().map(|p| format!(":{p}")).unwrap_or_default();
        let base_path = parsed.path().trim_end_matches('/');
        Ok(format!("{scheme}://{bucket}.{host}{port}{base_path}"))
    }

    async fn read_s3(&self, bucket: &str, key: &str) -> Result<Object, StorageError> {
        let url = self.s3_read_url(bucket, key)?;
        let headers = self.read_headers(bucket, Method::GET, &url, &[])?;
        let response = self.client.get(&url).headers(headers).send().await?;
        if self.read_status_is_miss(bucket, response.status()) {
            return Err(StorageError::NotFound);
        }
        if !response.status().is_success() {
            return Err(StorageError::S3(s3_error_summary(response).await));
        }
        let content_length: Option<u64> = response
            .headers()
            .get(header::CONTENT_LENGTH)
            .and_then(|v| v.to_str().ok())
            .and_then(|v| v.parse().ok());
        if let Some(content_length) = content_length
            && content_length > constants::MAX_MEDIA_PROXY_BYTES as u64
        {
            return Err(StorageError::StreamTooLong);
        }
        let content_type = response
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("application/octet-stream")
            .to_owned();
        let data = response.bytes().await?;
        if data.len() > constants::MAX_MEDIA_PROXY_BYTES {
            return Err(StorageError::StreamTooLong);
        }
        Ok(Object { data, content_type })
    }

    async fn head_s3(&self, bucket: &str, key: &str) -> Result<HeadResult, StorageError> {
        let url = self.s3_url(bucket, key)?;
        let signed = self.sign(Method::HEAD, &url, &[], None, &[])?;
        let response = self
            .client
            .head(&url)
            .headers(signed_headers(&signed, &self.cfg))
            .send()
            .await?;
        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(StorageError::NotFound);
        }
        if !response.status().is_success() {
            return Err(StorageError::S3(s3_error_summary(response).await));
        }
        let content_length: u64 = response
            .headers()
            .get(header::CONTENT_LENGTH)
            .and_then(|v| v.to_str().ok())
            .and_then(|v| v.parse().ok())
            .unwrap_or(0);
        let content_type = response
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("application/octet-stream")
            .to_owned();
        Ok(HeadResult {
            content_length,
            content_type,
        })
    }

    async fn stream_s3(
        &self,
        bucket: &str,
        key: &str,
        range_header: Option<&str>,
    ) -> Result<StreamObject, StorageError> {
        let url = self.s3_read_url(bucket, key)?;
        let range_extra = range_header.map(|value| aws_sigv4::Header {
            name: "Range",
            value,
        });
        let extra = range_extra.as_slice();
        let mut headers = self.read_headers(bucket, Method::GET, &url, extra)?;
        if let Some(range_value) = range_header {
            headers.insert(
                header::RANGE,
                range_value
                    .parse()
                    .map_err(|_| StorageError::S3("invalid Range header".to_owned()))?,
            );
        }
        let response = self.client.get(&url).headers(headers).send().await?;
        if self.read_status_is_miss(bucket, response.status()) {
            return Err(StorageError::NotFound);
        }
        if !response.status().is_success() {
            return Err(StorageError::S3(s3_error_summary(response).await));
        }
        let status = response.status();
        let content_length: Option<u64> = response
            .headers()
            .get(header::CONTENT_LENGTH)
            .and_then(|v| v.to_str().ok())
            .and_then(|v| v.parse().ok());
        if let Some(content_length) = content_length
            && range_header.is_none()
            && content_length > constants::MAX_MEDIA_PROXY_BYTES as u64
        {
            return Err(StorageError::StreamTooLong);
        }
        let content_type = response
            .headers()
            .get(header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("application/octet-stream")
            .to_owned();
        Ok(StreamObject {
            body: Body::from_stream(response.bytes_stream()),
            status,
            content_length,
            content_type,
        })
    }

    async fn write_s3(
        &self,
        bucket: &str,
        key: &str,
        data: &[u8],
        content_type: &str,
    ) -> Result<(), StorageError> {
        let url = self.s3_url(bucket, key)?;
        let extra = [aws_sigv4::Header {
            name: "Content-Type",
            value: content_type,
        }];
        let signed = self.sign(Method::PUT, &url, data, None, &extra)?;
        let mut headers = signed_headers(&signed, &self.cfg);
        headers.insert(
            header::CONTENT_TYPE,
            content_type
                .parse()
                .unwrap_or_else(|_| "application/octet-stream".parse().unwrap()),
        );
        let response = self
            .client
            .put(&url)
            .headers(headers)
            .body(data.to_vec())
            .send()
            .await?;
        if !response.status().is_success() {
            return Err(StorageError::S3(response.status().to_string()));
        }
        Ok(())
    }

    async fn relay_put_s3(
        &self,
        bucket: &str,
        key: &str,
        options: RelayPutOptions,
    ) -> Result<Option<String>, StorageError> {
        let mut url = self.s3_url(bucket, key)?;
        if let (Some(upload_id), Some(part_number)) = (&options.upload_id, options.part_number) {
            url.push_str(if url.contains('?') { "&" } else { "?" });
            url.push_str("partNumber=");
            url.push_str(
                &percent_encode(part_number.to_string().as_bytes(), QUERY_ENCODE_SET).to_string(),
            );
            url.push_str("&uploadId=");
            url.push_str(&percent_encode(upload_id.as_bytes(), QUERY_ENCODE_SET).to_string());
        }
        let content_type = options
            .content_type
            .as_deref()
            .unwrap_or("application/octet-stream");
        let extra = [aws_sigv4::Header {
            name: "Content-Type",
            value: content_type,
        }];
        let signed = self.sign(Method::PUT, &url, &[], Some(UNSIGNED_PAYLOAD), &extra)?;
        let mut headers = signed_headers(&signed, &self.cfg);
        headers.insert(
            header::CONTENT_TYPE,
            content_type
                .parse()
                .unwrap_or_else(|_| "application/octet-stream".parse().unwrap()),
        );
        headers.insert(
            header::CONTENT_LENGTH,
            header::HeaderValue::from(options.content_length),
        );
        let body = match options.body {
            RelayBody::Spooled(mut file) => {
                file.seek(std::io::SeekFrom::Start(0)).await?;
                reqwest::Body::wrap(SizedFileBody::new(file, options.content_length))
            }
            RelayBody::Streamed(rx) => {
                reqwest::Body::wrap(ChannelBody::new(rx, options.content_length))
            }
        };
        let response = self
            .raw_client
            .put(&url)
            .headers(headers)
            .timeout(Duration::from_millis(options.timeout_ms.max(1)))
            .body(body)
            .send()
            .await?;
        if !response.status().is_success() {
            return Err(StorageError::S3(s3_error_summary(response).await));
        }
        Ok(response
            .headers()
            .get(header::ETAG)
            .and_then(|v| v.to_str().ok())
            .map(ToOwned::to_owned))
    }

    fn sign(
        &self,
        method: Method,
        url: &str,
        payload: &[u8],
        payload_hash_override: Option<&str>,
        extra_signed_headers: &[aws_sigv4::Header<'_>],
    ) -> Result<aws_sigv4::SignedRequest, StorageError> {
        let mut options = aws_sigv4::Options::new(
            method.as_str(),
            url,
            &self.cfg.s3_region,
            &self.cfg.s3_access_key_id,
            &self.cfg.s3_secret_access_key,
        );
        options.payload = payload;
        options.payload_hash_override = payload_hash_override;
        options.extra_signed_headers = extra_signed_headers;
        options.session_token = &self.cfg.s3_session_token;
        Ok(aws_sigv4::sign(options)?)
    }
}

struct SizedFileBody {
    file: tokio::fs::File,
    remaining: u64,
}

impl SizedFileBody {
    fn new(file: tokio::fs::File, len: u64) -> Self {
        Self {
            file,
            remaining: len,
        }
    }
}

impl http_body::Body for SizedFileBody {
    type Data = Bytes;
    type Error = std::io::Error;

    fn poll_frame(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Frame<Self::Data>, Self::Error>>> {
        if self.remaining == 0 {
            return Poll::Ready(None);
        }
        let chunk_len = self.remaining.min(256 * 1024) as usize;
        let mut buffer = vec![0u8; chunk_len];
        let read = {
            let mut read_buf = ReadBuf::new(&mut buffer);
            match Pin::new(&mut self.file).poll_read(cx, &mut read_buf) {
                Poll::Ready(Ok(())) => read_buf.filled().len(),
                Poll::Ready(Err(err)) => return Poll::Ready(Some(Err(err))),
                Poll::Pending => return Poll::Pending,
            }
        };
        if read == 0 {
            return Poll::Ready(Some(Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "upload relay body ended before declared content length",
            ))));
        }
        buffer.truncate(read);
        self.remaining = self.remaining.saturating_sub(read as u64);
        Poll::Ready(Some(Ok(Frame::data(Bytes::from(buffer)))))
    }

    fn size_hint(&self) -> SizeHint {
        SizeHint::with_exact(self.remaining)
    }
}

struct ChannelBody {
    rx: tokio::sync::mpsc::Receiver<Result<Bytes, std::io::Error>>,
    remaining: u64,
}

impl ChannelBody {
    fn new(rx: tokio::sync::mpsc::Receiver<Result<Bytes, std::io::Error>>, len: u64) -> Self {
        Self { rx, remaining: len }
    }
}

impl http_body::Body for ChannelBody {
    type Data = Bytes;
    type Error = std::io::Error;

    fn poll_frame(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Frame<Self::Data>, Self::Error>>> {
        if self.remaining == 0 {
            return Poll::Ready(None);
        }
        match self.rx.poll_recv(cx) {
            Poll::Ready(Some(Ok(chunk))) => {
                if chunk.len() as u64 > self.remaining {
                    return Poll::Ready(Some(Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "upload relay body exceeded declared content length",
                    ))));
                }
                self.remaining -= chunk.len() as u64;
                Poll::Ready(Some(Ok(Frame::data(chunk))))
            }
            Poll::Ready(Some(Err(err))) => Poll::Ready(Some(Err(err))),
            Poll::Ready(None) => Poll::Ready(Some(Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "upload relay body ended before declared content length",
            )))),
            Poll::Pending => Poll::Pending,
        }
    }

    fn size_hint(&self) -> SizeHint {
        SizeHint::with_exact(self.remaining)
    }
}

fn write_bucket_style(cfg: &Config) -> BucketStyle {
    if cfg.s3_force_path_style {
        BucketStyle::Path
    } else {
        BucketStyle::VirtualHosted
    }
}

fn object_url(
    endpoint: &str,
    style: BucketStyle,
    bucket: &str,
    key: &str,
) -> Result<String, StorageError> {
    if endpoint.is_empty() {
        return Err(StorageError::InvalidS3Endpoint);
    }
    let endpoint = endpoint.trim_end_matches('/');
    let encoded_key = percent_encode(key.as_bytes(), PATH_ENCODE_SET).to_string();
    match style {
        BucketStyle::Path => Ok(format!("{endpoint}/{bucket}/{encoded_key}")),
        BucketStyle::Rooted => Ok(format!("{endpoint}/{encoded_key}")),
        BucketStyle::VirtualHosted => {
            validate_virtual_hosted_bucket(bucket)?;
            let parsed = url::Url::parse(endpoint).map_err(|_| StorageError::InvalidS3Endpoint)?;
            let scheme = parsed.scheme();
            let host = parsed.host_str().ok_or(StorageError::InvalidS3Endpoint)?;
            let port = parsed.port().map(|p| format!(":{p}")).unwrap_or_default();
            let base_path = parsed.path().trim_end_matches('/');
            Ok(format!(
                "{scheme}://{bucket}.{host}{port}{base_path}/{encoded_key}"
            ))
        }
    }
}

fn signed_headers(signed: &aws_sigv4::SignedRequest, cfg: &Config) -> reqwest::header::HeaderMap {
    let mut headers = reqwest::header::HeaderMap::new();
    headers.insert(
        header::HOST,
        signed.host.parse().expect("signed host is a valid header"),
    );
    headers.insert(
        "x-amz-content-sha256",
        signed.payload_hash.parse().expect("payload hash is ASCII"),
    );
    headers.insert(
        "x-amz-date",
        signed.amz_date.parse().expect("date is ASCII"),
    );
    headers.insert(
        header::AUTHORIZATION,
        signed
            .authorization
            .parse()
            .expect("authorization is ASCII"),
    );
    if !cfg.s3_session_token.is_empty() {
        headers.insert(
            "x-amz-security-token",
            cfg.s3_session_token
                .parse()
                .expect("session token is ASCII"),
        );
    }
    headers
}

async fn s3_error_summary(response: reqwest::Response) -> String {
    let status = response.status();
    let body = response
        .bytes()
        .await
        .map(|b| b.to_vec())
        .unwrap_or_default();
    let snippet: String = String::from_utf8_lossy(&body)
        .chars()
        .filter(|c| !c.is_control() || *c == ' ')
        .take(512)
        .collect();
    if snippet.is_empty() {
        status.to_string()
    } else {
        format!("{status}: {snippet}")
    }
}

fn map_not_found(err: std::io::Error) -> StorageError {
    if err.kind() == std::io::ErrorKind::NotFound {
        StorageError::NotFound
    } else {
        StorageError::Io(err)
    }
}

fn safe_bucket(bucket: &str) -> Result<(), StorageError> {
    if bucket.is_empty() || bucket.contains('/') || bucket == "." || bucket == ".." {
        return Err(StorageError::InvalidBucket);
    }
    Ok(())
}

pub fn safe_key(key: &str) -> Result<(), StorageError> {
    if key.is_empty() || key.starts_with('/') {
        return Err(StorageError::InvalidKey);
    }
    for part in key.split('/') {
        if part.is_empty() || part == "." || part == ".." {
            return Err(StorageError::InvalidKey);
        }
    }
    Ok(())
}

fn validate_virtual_hosted_bucket(bucket: &str) -> Result<(), StorageError> {
    if bucket.len() < 3 || bucket.len() > 63 {
        return Err(StorageError::InvalidBucket);
    }
    if !bucket
        .bytes()
        .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'-' || b == b'.')
    {
        return Err(StorageError::InvalidBucket);
    }
    Ok(())
}

fn reject_symlink_chain(path: &Path) -> Result<(), StorageError> {
    let mut cur = PathBuf::new();
    for component in path.components() {
        cur.push(component.as_os_str());
        match std::fs::symlink_metadata(&cur) {
            Ok(meta) if meta.file_type().is_symlink() => return Err(StorageError::InvalidKey),
            Ok(_) => {}
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
            Err(err) => return Err(StorageError::Io(err)),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{BucketStyle, DeploymentMode, StorageBackend};

    fn test_config(root: &Path) -> Config {
        Config {
            node_env: "test".to_owned(),
            bind_host: "127.0.0.1".to_owned(),
            port: 0,
            secret_key: "secret".to_owned(),
            mode: DeploymentMode::Mp,
            read_only: false,
            storage_backend: StorageBackend::Local,
            storage_root: root.display().to_string(),
            s3_endpoint: String::new(),
            s3_region: "us-east-1".to_owned(),
            s3_access_key_id: String::new(),
            s3_secret_access_key: String::new(),
            s3_session_token: String::new(),
            s3_force_path_style: true,
            s3_read_endpoint: None,
            s3_read_bucket: "cdn".to_owned(),
            s3_read_bucket_style: BucketStyle::Path,
            s3_read_signed: false,
            bucket_cdn: "cdn".to_owned(),
            bucket_uploads: "uploads".to_owned(),
            bucket_static: "static".to_owned(),
            upload_relay_secret: Vec::new(),
            upload_relay_max_body_bytes: 1024,
            upload_relay_token_ttl_secs: 3600,
            upload_relay_s3_timeout_ms: 1000,
            upload_relay_buffered_retry_max_bytes: 0,
            upload_relay_buffered_retry_total_bytes: 0,
            upload_relay_spool_dir: std::env::temp_dir(),
            upload_relay_spool_chunk_bytes: 64 * 1024,
            upload_relay_spool_max_total_bytes: 1 << 30,
            max_native_transforms: 2,
            worker_queue_capacity: 16,
            nsfw_service_endpoint: String::new(),
            nsfw_threshold: 0.85,
            transform_cache_capacity_bytes: 0,
            transform_cache_max_entry_bytes: 0,
            transform_cache_ttl_ms: 0,
            shutdown_grace_ms: 0,
            socket_io_timeout_ms: 0,
            transform_timeout_ms: 1000,
            max_encode_frames: constants::MAX_ANIMATED_FRAMES_DEFAULT,
            max_encode_duration_ms: 30_000,
            bunny_ip_gate_enabled: false,
            bunny_ip_gate_trusted_proxies: Vec::new(),
            bunny_ip_gate_refresh_secs: 3_600,
        }
    }

    #[tokio::test]
    async fn local_write_read_head_roundtrip() {
        let tmp = tempfile::tempdir().unwrap();
        let store = Store::new(test_config(&tmp.path().canonicalize().unwrap()));
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
        let store = Store::new(test_config(&tmp.path().canonicalize().unwrap()));
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

    #[test]
    fn safe_key_rejects_traversal() {
        assert!(safe_key("a/b").is_ok());
        assert!(safe_key("../x").is_err());
        assert!(safe_key("a//b").is_err());
        assert!(safe_key("/a").is_err());
    }

    #[tokio::test]
    async fn relay_put_s3_streams_body_with_unsigned_payload() {
        type CapturedRequest = (http::Uri, http::HeaderMap, Bytes);
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let captured: std::sync::Arc<tokio::sync::Mutex<Option<CapturedRequest>>> =
            std::sync::Arc::new(tokio::sync::Mutex::new(None));
        let captured_handler = std::sync::Arc::clone(&captured);
        let app = axum::Router::new().fallback(axum::routing::any(
            move |request: axum::extract::Request| {
                let captured = std::sync::Arc::clone(&captured_handler);
                async move {
                    let (parts, body) = request.into_parts();
                    let bytes = axum::body::to_bytes(body, 1 << 20).await.unwrap();
                    *captured.lock().await = Some((parts.uri, parts.headers, bytes));
                    ([(header::ETAG, "\"etag-123\"")], "")
                }
            },
        ));
        tokio::spawn(async move {
            axum::serve(listener, app).await.unwrap();
        });

        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = test_config(tmp.path());
        cfg.storage_backend = StorageBackend::S3;
        cfg.s3_endpoint = format!("http://{addr}");
        cfg.s3_access_key_id = "AKIAIOSFODNN7EXAMPLE".to_owned();
        cfg.s3_secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY".to_owned();
        let store = Store::new(cfg);

        let (tx, rx) = tokio::sync::mpsc::channel(4);
        tokio::spawn(async move {
            tx.send(Ok(Bytes::from_static(b"hello "))).await.unwrap();
            tx.send(Ok(Bytes::from_static(b"world"))).await.unwrap();
        });
        let etag = store
            .relay_put_object(
                "uploads",
                "guild/streamed.bin",
                RelayPutOptions {
                    body: RelayBody::Streamed(rx),
                    content_length: 11,
                    content_type: Some("application/octet-stream".to_owned()),
                    upload_id: Some("upload-1".to_owned()),
                    part_number: Some(2),
                    timeout_ms: 5_000,
                },
            )
            .await
            .unwrap();

        assert_eq!(Some("\"etag-123\"".to_owned()), etag);
        let (uri, headers, body) = captured.lock().await.take().unwrap();
        assert_eq!("/uploads/guild/streamed.bin", uri.path());
        assert_eq!(Some("partNumber=2&uploadId=upload-1"), uri.query());
        assert_eq!(
            UNSIGNED_PAYLOAD,
            headers.get("x-amz-content-sha256").unwrap()
        );
        assert_eq!("11", headers.get(header::CONTENT_LENGTH).unwrap());
        assert!(
            headers
                .get(header::AUTHORIZATION)
                .unwrap()
                .to_str()
                .unwrap()
                .starts_with("AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/")
        );
        assert_eq!(b"hello world", body.as_ref());
    }

    #[tokio::test]
    async fn relay_put_s3_fails_when_stream_ends_short() {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let app = axum::Router::new().fallback(axum::routing::any(
            move |request: axum::extract::Request| async move {
                let _ = axum::body::to_bytes(request.into_body(), 1 << 20).await;
                ""
            },
        ));
        tokio::spawn(async move {
            axum::serve(listener, app).await.unwrap();
        });

        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = test_config(tmp.path());
        cfg.storage_backend = StorageBackend::S3;
        cfg.s3_endpoint = format!("http://{addr}");
        cfg.s3_access_key_id = "AKIAIOSFODNN7EXAMPLE".to_owned();
        cfg.s3_secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY".to_owned();
        let store = Store::new(cfg);

        let (tx, rx) = tokio::sync::mpsc::channel(4);
        tokio::spawn(async move {
            tx.send(Ok(Bytes::from_static(b"only"))).await.unwrap();
        });
        let result = store
            .relay_put_object(
                "uploads",
                "guild/short.bin",
                RelayPutOptions {
                    body: RelayBody::Streamed(rx),
                    content_length: 32,
                    content_type: None,
                    upload_id: None,
                    part_number: None,
                    timeout_ms: 5_000,
                },
            )
            .await;
        assert!(result.is_err());
    }

    type CapturedRequests =
        std::sync::Arc<tokio::sync::Mutex<Vec<(http::Method, http::Uri, http::HeaderMap)>>>;

    async fn capture_server() -> (String, CapturedRequests) {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let captured: CapturedRequests = std::sync::Arc::new(tokio::sync::Mutex::new(Vec::new()));
        let handler = std::sync::Arc::clone(&captured);
        let app = axum::Router::new().fallback(axum::routing::any(
            move |request: axum::extract::Request| {
                let captured = std::sync::Arc::clone(&handler);
                async move {
                    let (parts, _body) = request.into_parts();
                    captured
                        .lock()
                        .await
                        .push((parts.method, parts.uri, parts.headers));
                    ([(header::CONTENT_TYPE, "image/png")], "payload")
                }
            },
        ));
        tokio::spawn(async move {
            axum::serve(listener, app).await.unwrap();
        });
        (format!("http://{addr}"), captured)
    }

    async fn status_server(status: u16) -> (String, CapturedRequests) {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let captured: CapturedRequests = std::sync::Arc::new(tokio::sync::Mutex::new(Vec::new()));
        let handler = std::sync::Arc::clone(&captured);
        let app = axum::Router::new().fallback(axum::routing::any(
            move |request: axum::extract::Request| {
                let captured = std::sync::Arc::clone(&handler);
                async move {
                    let (parts, _body) = request.into_parts();
                    captured
                        .lock()
                        .await
                        .push((parts.method, parts.uri, parts.headers));
                    (StatusCode::from_u16(status).unwrap(), "denied")
                }
            },
        ));
        tokio::spawn(async move {
            axum::serve(listener, app).await.unwrap();
        });
        (format!("http://{addr}"), captured)
    }

    fn s3_test_config(root: &Path, s3_endpoint: &str) -> Config {
        let mut cfg = test_config(root);
        cfg.storage_backend = StorageBackend::S3;
        cfg.s3_endpoint = s3_endpoint.to_owned();
        cfg.s3_access_key_id = "AKIAIOSFODNN7EXAMPLE".to_owned();
        cfg.s3_secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY".to_owned();
        cfg
    }

    async fn only_request(
        captured: &CapturedRequests,
    ) -> (http::Method, http::Uri, http::HeaderMap) {
        let mut guard = captured.lock().await;
        assert_eq!(1, guard.len(), "expected exactly one captured request");
        guard.remove(0)
    }

    fn assert_unsigned(headers: &http::HeaderMap) {
        assert!(headers.get(header::AUTHORIZATION).is_none());
        assert!(headers.get("x-amz-date").is_none());
        assert!(headers.get("x-amz-content-sha256").is_none());
    }

    fn assert_signed(headers: &http::HeaderMap) {
        assert!(
            headers
                .get(header::AUTHORIZATION)
                .unwrap()
                .to_str()
                .unwrap()
                .starts_with("AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/")
        );
        assert!(headers.get("x-amz-date").is_some());
        assert!(headers.get("x-amz-content-sha256").is_some());
    }

    #[tokio::test]
    async fn read_without_read_endpoint_hits_s3_endpoint_signed() {
        let (s3, s3_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let store = Store::new(s3_test_config(tmp.path(), &s3));

        let object = store
            .read_object("cdn", "attachments/1/2/a.png")
            .await
            .unwrap();

        assert_eq!("payload", object.data);
        let (method, uri, headers) = only_request(&s3_seen).await;
        assert_eq!(http::Method::GET, method);
        assert_eq!("/cdn/attachments/1/2/a.png", uri.path());
        assert_signed(&headers);
    }

    #[tokio::test]
    async fn read_endpoint_routes_fronted_bucket_to_cdn_unsigned() {
        let (s3, s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        let store = Store::new(cfg);

        let object = store
            .read_object("cdn", "attachments/1/2/a.png")
            .await
            .unwrap();

        assert_eq!("payload", object.data);
        assert!(
            s3_seen.lock().await.is_empty(),
            "S3 endpoint must not be touched"
        );
        let (method, uri, headers) = only_request(&cdn_seen).await;
        assert_eq!(http::Method::GET, method);
        assert_eq!("/cdn/attachments/1/2/a.png", uri.path());
        assert_unsigned(&headers);
    }

    #[tokio::test]
    async fn read_endpoint_rooted_style_omits_bucket_segment() {
        let (s3, _s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        cfg.s3_read_bucket_style = BucketStyle::Rooted;
        let store = Store::new(cfg);

        store
            .read_object("cdn", "attachments/1/2/a.png")
            .await
            .unwrap();

        let (_, uri, _) = only_request(&cdn_seen).await;
        assert_eq!("/attachments/1/2/a.png", uri.path());
    }

    #[test]
    fn read_endpoint_virtual_style_uses_bucket_subdomain() {
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), "https://s3.example.test");
        cfg.s3_read_endpoint = Some("https://cdn.example.net".to_owned());
        cfg.s3_read_bucket_style = BucketStyle::VirtualHosted;
        let store = Store::new(cfg);

        assert_eq!(
            "https://cdn.cdn.example.net/attachments/1/2/a.png",
            store.s3_read_url("cdn", "attachments/1/2/a.png").unwrap()
        );
    }

    #[tokio::test]
    async fn read_endpoint_never_redirects_other_buckets() {
        let (s3, s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        cfg.s3_read_bucket_style = BucketStyle::Rooted;
        let store = Store::new(cfg);

        store
            .read_object("uploads", "fresh-upload-key")
            .await
            .unwrap();

        assert!(
            cdn_seen.lock().await.is_empty(),
            "uploads must not hit the CDN"
        );
        let (_, uri, headers) = only_request(&s3_seen).await;
        assert_eq!("/uploads/fresh-upload-key", uri.path());
        assert_signed(&headers);
    }

    #[tokio::test]
    async fn read_endpoint_never_affects_writes() {
        let (s3, s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        cfg.s3_read_bucket = "uploads".to_owned();
        let store = Store::new(cfg);

        let (tx, rx) = tokio::sync::mpsc::channel(2);
        tokio::spawn(async move {
            tx.send(Ok(Bytes::from_static(b"body"))).await.unwrap();
        });
        store
            .relay_put_object(
                "uploads",
                "guild/x.bin",
                RelayPutOptions {
                    body: RelayBody::Streamed(rx),
                    content_length: 4,
                    content_type: Some("application/octet-stream".to_owned()),
                    upload_id: None,
                    part_number: None,
                    timeout_ms: 5_000,
                },
            )
            .await
            .unwrap();

        assert!(
            cdn_seen.lock().await.is_empty(),
            "writes must not hit the CDN"
        );
        let (method, uri, headers) = only_request(&s3_seen).await;
        assert_eq!(http::Method::PUT, method);
        assert_eq!("/uploads/guild/x.bin", uri.path());
        assert_signed(&headers);
    }

    #[tokio::test]
    async fn read_endpoint_signs_when_read_signed_enabled() {
        let (s3, _s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = capture_server().await;
        let cdn_host = cdn.trim_start_matches("http://").to_owned();
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        cfg.s3_read_signed = true;
        let store = Store::new(cfg);

        store.read_object("cdn", "a.png").await.unwrap();

        let (_, _, headers) = only_request(&cdn_seen).await;
        assert_signed(&headers);
        assert_eq!(
            cdn_host,
            headers.get(header::HOST).unwrap().to_str().unwrap()
        );
    }

    #[tokio::test]
    async fn unsigned_read_still_sends_range_header() {
        let (s3, _s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        let store = Store::new(cfg);

        store
            .stream_object("cdn", "video.mp4", Some("bytes=10-19"))
            .await
            .unwrap();

        let (_, _, headers) = only_request(&cdn_seen).await;
        assert_eq!("bytes=10-19", headers.get(header::RANGE).unwrap());
        assert_unsigned(&headers);
    }

    #[tokio::test]
    async fn head_object_always_uses_origin_for_authoritative_length() {
        let (s3, s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        cfg.s3_read_bucket_style = BucketStyle::Rooted;
        let store = Store::new(cfg);

        store.head_object("cdn", "a.png").await.unwrap();

        assert!(
            cdn_seen.lock().await.is_empty(),
            "HEAD must not hit the CDN"
        );
        let (method, uri, headers) = only_request(&s3_seen).await;
        assert_eq!(http::Method::HEAD, method);
        assert_eq!("/cdn/a.png", uri.path());
        assert_signed(&headers);
    }

    #[tokio::test]
    async fn body_reads_use_cdn_while_head_uses_origin() {
        let (s3, s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        cfg.s3_read_bucket_style = BucketStyle::Rooted;
        let store = Store::new(cfg);

        store.head_object("cdn", "video.mp4").await.unwrap();
        store
            .stream_object("cdn", "video.mp4", Some("bytes=0-3"))
            .await
            .unwrap();

        let s3_reqs = s3_seen.lock().await.clone();
        let cdn_reqs = cdn_seen.lock().await.clone();
        assert_eq!(1, s3_reqs.len());
        assert_eq!(http::Method::HEAD, s3_reqs[0].0);
        assert_eq!(1, cdn_reqs.len());
        assert_eq!(http::Method::GET, cdn_reqs[0].0);
        assert_eq!("bytes=0-3", cdn_reqs[0].2.get(header::RANGE).unwrap());
    }

    #[tokio::test]
    async fn unsigned_read_works_without_credentials() {
        let (s3, _s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        cfg.s3_access_key_id = String::new();
        cfg.s3_secret_access_key = String::new();
        let store = Store::new(cfg);

        store.read_object("cdn", "a.png").await.unwrap();

        let (_, _, headers) = only_request(&cdn_seen).await;
        assert_unsigned(&headers);
    }

    #[test]
    fn read_url_matches_write_url_shape_for_encoding_and_trailing_slash() {
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), "https://s3.example.test/");
        cfg.s3_read_endpoint = Some("https://cdn.example.net/".to_owned());
        let store = Store::new(cfg);
        let key = "attachments/1/2/na me+ü.png";

        assert_eq!(
            "https://s3.example.test/uploads/attachments/1/2/na%20me%2B%C3%BC.png",
            store.s3_url("uploads", key).unwrap()
        );
        assert_eq!(
            "https://cdn.example.net/cdn/attachments/1/2/na%20me%2B%C3%BC.png",
            store.s3_read_url("cdn", key).unwrap()
        );
    }

    #[test]
    fn read_url_rejects_unsafe_keys_and_buckets() {
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), "https://s3.example.test");
        cfg.s3_read_endpoint = Some("https://cdn.example.net".to_owned());
        cfg.s3_read_bucket_style = BucketStyle::Rooted;
        let store = Store::new(cfg);

        assert!(store.s3_read_url("cdn", "../escape").is_err());
        assert!(store.s3_read_url("cdn", "/leading").is_err());
    }

    #[test]
    fn read_url_validates_bucket_inside_the_read_endpoint_branch() {
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), "https://s3.example.test");
        cfg.s3_read_endpoint = Some("https://cdn.example.net".to_owned());
        cfg.s3_read_bucket = "..".to_owned();
        cfg.s3_read_bucket_style = BucketStyle::Rooted;
        let store = Store::new(cfg);

        assert!(store.read_endpoint_for("..").is_some());
        assert!(store.s3_read_url("..", "a.png").is_err());
    }

    #[tokio::test]
    async fn unsigned_cdn_read_treats_403_as_not_found() {
        let (s3, _s3_seen) = capture_server().await;
        let (cdn, cdn_seen) = status_server(403).await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        cfg.s3_read_bucket_style = BucketStyle::Rooted;
        let store = Store::new(cfg);

        let err = store
            .read_object("cdn", "avatars/1/hash")
            .await
            .unwrap_err();
        assert!(matches!(err, StorageError::NotFound), "got {err:?}");
        assert_eq!(1, cdn_seen.lock().await.len());

        let stream = store.stream_object("cdn", "avatars/1/hash", None).await;
        assert!(
            matches!(stream, Err(StorageError::NotFound)),
            "stream_object should map 403 to NotFound too"
        );
    }

    #[tokio::test]
    async fn signed_cdn_read_keeps_403_as_an_error() {
        let (s3, _s3_seen) = capture_server().await;
        let (cdn, _cdn_seen) = status_server(403).await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        cfg.s3_read_signed = true;
        let store = Store::new(cfg);

        let err = store
            .read_object("cdn", "avatars/1/hash")
            .await
            .unwrap_err();
        assert!(!matches!(err, StorageError::NotFound), "got {err:?}");
    }

    #[tokio::test]
    async fn origin_read_keeps_403_as_an_error() {
        let (s3, _s3_seen) = status_server(403).await;
        let tmp = tempfile::tempdir().unwrap();
        let store = Store::new(s3_test_config(tmp.path(), &s3));

        let err = store
            .read_object("cdn", "avatars/1/hash")
            .await
            .unwrap_err();
        assert!(!matches!(err, StorageError::NotFound), "got {err:?}");
    }

    #[tokio::test]
    async fn unfronted_bucket_keeps_403_as_an_error() {
        let (s3, _s3_seen) = status_server(403).await;
        let (cdn, cdn_seen) = capture_server().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = s3_test_config(tmp.path(), &s3);
        cfg.s3_read_endpoint = Some(cdn.clone());
        let store = Store::new(cfg);

        let err = store.read_object("uploads", "fresh").await.unwrap_err();
        assert!(!matches!(err, StorageError::NotFound), "got {err:?}");
        assert!(cdn_seen.lock().await.is_empty());
    }
}
