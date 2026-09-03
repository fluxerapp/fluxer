// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    BufferedObjectReadRequest, BufferedStorageObject, ContentDigestRequest, HeadResult,
    ObjectStreamPlan, StorageError, Store, StreamObject, StreamRange,
    identity::{RemoteSourceObject, SourceObjectIdentity, remote_source_object_identity},
    relay_body::{RelayBody, RelayPutOptions, SizedFileBody},
    response_body::{
        StreamResponseValidation, exact_response_stream, read_response_bytes,
        validate_stream_response,
    },
};
use crate::{aws_sigv4, byte_budget::BudgetedBytes, config::Config, http_headers, range};
use axum::body::Body;
use http::{HeaderMap, HeaderName, StatusCode, header};
use percent_encoding::{AsciiSet, CONTROLS, percent_encode};
use reqwest::Method;
use sha2::{Digest as _, Sha256};
use std::time::Duration;
use tokio::io::AsyncSeekExt as _;

pub(super) const UNSIGNED_PAYLOAD: &str = "UNSIGNED-PAYLOAD";

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

impl Store {
    pub(super) async fn ensure_bucket_s3(&self, bucket: &str) -> Result<(), StorageError> {
        let url = self.s3_bucket_url(bucket)?;
        let signed = self.sign(Method::PUT, &url, &[], None, &[])?;
        let response = self
            .client
            .put(&url)
            .headers(signed_headers(&signed, &self.cfg))
            .send()
            .await?;
        if response.status().is_success() || response.status() == StatusCode::CONFLICT {
            Ok(())
        } else {
            Err(StorageError::S3(response.status().to_string()))
        }
    }

    pub(super) async fn read_s3(
        &self,
        request: BufferedObjectReadRequest<'_>,
    ) -> Result<BufferedStorageObject, StorageError> {
        let url = self.s3_read_url(request.bucket, request.key)?;
        let if_match = request
            .expected_identity
            .and_then(SourceObjectIdentity::etag);
        let extra = if_match.map(|value| aws_sigv4::Header {
            name: "If-Match",
            value,
        });
        let mut headers = self.read_headers(request.bucket, Method::GET, &url, extra.as_slice())?;
        if let Some(value) = if_match {
            headers.insert(
                header::IF_MATCH,
                value.parse().map_err(|_| StorageError::ObjectChanged)?,
            );
        }
        let response = self.client.get(&url).headers(headers).send().await?;
        let status = response.status();
        if let Some(error) =
            self.read_status_error(request.bucket, status, request.expected_identity.is_some())
        {
            return Err(error);
        }
        if !status.is_success() {
            return Err(StorageError::S3(s3_error_summary(response).await));
        }
        let content_length = http_headers::parse_content_length(response.headers());
        if let Some(content_length) = content_length
            && content_length > request.limit as u64
        {
            return Err(StorageError::StreamTooLong);
        }
        let content_type = response_content_type(response.headers());
        if let Some(expected) = request.expected_identity
            && expected.etag().is_none()
        {
            let Some(content_length) = content_length else {
                return Err(StorageError::ObjectChanged);
            };
            let identity = remote_source_object_identity(RemoteSourceObject {
                bucket: request.bucket,
                key: request.key,
                content_length,
                content_type: &content_type,
                etag: header_str(response.headers(), header::ETAG).as_deref(),
                last_modified: header_str(response.headers(), header::LAST_MODIFIED).as_deref(),
            });
            if identity != *expected {
                return Err(StorageError::ObjectChanged);
            }
        }
        let data = match content_length {
            Some(content_length) => {
                let expected_length =
                    usize::try_from(content_length).map_err(|_| StorageError::StreamTooLong)?;
                read_response_bytes(response, expected_length, request.budget).await?
            }
            None => {
                let data = response.bytes().await?;
                if data.len() > request.limit {
                    return Err(StorageError::StreamTooLong);
                }
                BudgetedBytes::unbudgeted(data)
            }
        };
        Ok(BufferedStorageObject {
            content_digest: match request.content_digest {
                ContentDigestRequest::Omit => None,
                ContentDigestRequest::Include => Some(Sha256::digest(data.as_ref()).into()),
            },
            data,
            content_type,
        })
    }

    pub(super) async fn head_s3(
        &self,
        bucket: &str,
        key: &str,
        max_bytes: usize,
    ) -> Result<HeadResult, StorageError> {
        // A HEAD is a read, so it follows the same endpoint and signing policy as a body read.
        // Sending it to the write origin instead would bypass the configured read endpoint and
        // attach credentials to a read the operator configured as unsigned.
        let url = self.s3_read_url(bucket, key)?;
        let headers = self.read_headers(bucket, Method::HEAD, &url, &[])?;
        let response = self.client.head(&url).headers(headers).send().await?;
        let status = response.status();
        if let Some(error) = self.read_status_error(bucket, status, false) {
            return Err(error);
        }
        if !status.is_success() {
            return Err(StorageError::S3(s3_error_summary(response).await));
        }
        let content_length = http_headers::parse_content_length(response.headers()).unwrap_or(0);
        if content_length > max_bytes as u64 {
            return Err(StorageError::StreamTooLong);
        }
        let content_type = response_content_type(response.headers());
        let identity = remote_source_object_identity(RemoteSourceObject {
            bucket,
            key,
            content_length,
            content_type: &content_type,
            etag: header_str(response.headers(), header::ETAG).as_deref(),
            last_modified: header_str(response.headers(), header::LAST_MODIFIED).as_deref(),
        });
        Ok(HeadResult {
            content_length,
            content_type,
            identity,
        })
    }

    pub(super) async fn stream_s3(
        &self,
        plan: ObjectStreamPlan<'_>,
    ) -> Result<StreamObject, StorageError> {
        let url = self.s3_read_url(plan.bucket, plan.key)?;
        let range_value = match plan.range {
            StreamRange::Full => None,
            StreamRange::Header(header) => Some(header.to_owned()),
            StreamRange::Bytes(byte_range) => {
                Some(format!("bytes={}-{}", byte_range.start, byte_range.end))
            }
        };
        let if_match = plan.expected_identity.and_then(SourceObjectIdentity::etag);
        let mut extra = Vec::new();
        if let Some(value) = range_value.as_deref() {
            extra.push(aws_sigv4::Header {
                name: "Range",
                value,
            });
        }
        if let Some(value) = if_match {
            extra.push(aws_sigv4::Header {
                name: "If-Match",
                value,
            });
        }
        let mut headers = self.read_headers(plan.bucket, Method::GET, &url, &extra)?;
        if let Some(value) = range_value.as_deref() {
            headers.insert(
                header::RANGE,
                value
                    .parse()
                    .map_err(|_| StorageError::S3("invalid Range header".to_owned()))?,
            );
        }
        if let Some(value) = if_match {
            headers.insert(
                header::IF_MATCH,
                value.parse().map_err(|_| StorageError::ObjectChanged)?,
            );
        }
        let response = self.client.get(&url).headers(headers).send().await?;
        let status = response.status();
        if let Some(error) =
            self.read_status_error(plan.bucket, status, plan.expected_identity.is_some())
        {
            return Err(error);
        }
        // An upstream 416 is an answer about the range, not a transport failure. Reporting it as
        // a storage error would surface 502 to a client that merely asked for a span past the end
        // of the object, which is a routine thing for a video player to do while seeking.
        if status == StatusCode::RANGE_NOT_SATISFIABLE {
            let total_length = header_str(response.headers(), header::CONTENT_RANGE)
                .and_then(|value| range::parse_unsatisfiable_content_range(Some(&value)))
                .map(|total| total as u64);
            return Ok(StreamObject {
                body: Body::empty(),
                status,
                content_length: Some(0),
                content_type: String::new(),
                byte_range: None,
                total_length,
            });
        }
        if !status.is_success() {
            return Err(StorageError::S3(s3_error_summary(response).await));
        }
        if let Some(expected) = plan.expected_identity {
            return versioned_stream_object(response, plan, expected);
        }
        let content_length = http_headers::parse_content_length(response.headers());
        if let Some(content_length) = content_length
            && matches!(plan.range, StreamRange::Full)
            && content_length > plan.max_bytes as u64
        {
            return Err(StorageError::StreamTooLong);
        }
        let content_type = response_content_type(response.headers());
        let content_range = header_str(response.headers(), header::CONTENT_RANGE)
            .and_then(|value| range::parse_content_range(Some(&value)));
        let byte_range = content_range.map(|cr| range::ByteRange {
            start: cr.start,
            end: cr.end,
        });
        let total_length = content_range
            .and_then(|cr| cr.size)
            .or(content_length.map(|len| len as usize))
            .map(|len| len as u64);
        let body = match content_length {
            Some(content_length) => {
                Body::from_stream(exact_response_stream(response, content_length))
            }
            None => Body::from_stream(response.bytes_stream()),
        };
        Ok(StreamObject {
            body,
            status,
            content_length,
            content_type,
            byte_range,
            total_length,
        })
    }

    pub(super) async fn write_s3(
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
        headers.insert(header::CONTENT_TYPE, content_type_header(content_type));
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

    pub(super) async fn relay_put_s3(
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
        headers.insert(header::CONTENT_TYPE, content_type_header(content_type));
        headers.insert(
            header::CONTENT_LENGTH,
            header::HeaderValue::from(options.content_length),
        );
        let body = match options.body {
            RelayBody::Spooled(mut file) => {
                file.seek(std::io::SeekFrom::Start(0)).await?;
                reqwest::Body::wrap(SizedFileBody::new(file, options.content_length))
            }
            RelayBody::Streamed(chunks) => reqwest::Body::wrap_stream(chunks),
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
        Ok(header_str(response.headers(), header::ETAG))
    }

    pub(super) fn sign(
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
            &self.cfg.storage.s3_region,
            &self.cfg.storage.s3_access_key_id,
            &self.cfg.storage.s3_secret_access_key,
        );
        options.payload = payload;
        options.payload_hash_override = payload_hash_override;
        options.extra_signed_headers = extra_signed_headers;
        options.session_token = &self.cfg.storage.s3_session_token;
        Ok(aws_sigv4::sign(options)?)
    }
}

fn versioned_stream_object(
    response: reqwest::Response,
    plan: ObjectStreamPlan<'_>,
    expected: &SourceObjectIdentity,
) -> Result<StreamObject, StorageError> {
    let status = response.status();
    let total_length = expected.content_length();
    let byte_range = match plan.range {
        StreamRange::Bytes(byte_range) => Some(byte_range),
        StreamRange::Full | StreamRange::Header(_) => None,
    };
    let expected_length = byte_range.map_or(total_length, |byte_range| {
        (byte_range.end - byte_range.start + 1) as u64
    });
    validate_stream_response(StreamResponseValidation {
        status,
        headers: response.headers(),
        total_length,
        expected_length,
        byte_range,
    })?;
    if expected.etag().is_none() {
        let identity = remote_source_object_identity(RemoteSourceObject {
            bucket: plan.bucket,
            key: plan.key,
            content_length: total_length,
            content_type: &response_content_type(response.headers()),
            etag: header_str(response.headers(), header::ETAG).as_deref(),
            last_modified: header_str(response.headers(), header::LAST_MODIFIED).as_deref(),
        });
        if identity != *expected {
            return Err(StorageError::ObjectChanged);
        }
    }
    Ok(StreamObject {
        body: Body::from_stream(exact_response_stream(response, expected_length)),
        status: if byte_range.is_some() {
            StatusCode::PARTIAL_CONTENT
        } else {
            StatusCode::OK
        },
        content_length: Some(expected_length),
        content_type: expected.content_type().to_owned(),
        byte_range,
        total_length: Some(total_length),
    })
}

fn content_type_header(content_type: &str) -> header::HeaderValue {
    content_type
        .parse()
        .unwrap_or_else(|_| header::HeaderValue::from_static("application/octet-stream"))
}

fn response_content_type(headers: &HeaderMap) -> String {
    header_str(headers, header::CONTENT_TYPE)
        .unwrap_or_else(|| "application/octet-stream".to_owned())
}

fn header_str(headers: &HeaderMap, name: HeaderName) -> Option<String> {
    headers
        .get(name)
        .and_then(|value| value.to_str().ok())
        .map(ToOwned::to_owned)
}

pub(super) fn signed_headers(
    signed: &aws_sigv4::SignedRequest,
    cfg: &Config,
) -> reqwest::header::HeaderMap {
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
    if !cfg.storage.s3_session_token.is_empty() {
        headers.insert(
            "x-amz-security-token",
            cfg.storage
                .s3_session_token
                .parse()
                .expect("session token is ASCII"),
        );
    }
    headers
}

pub(super) async fn s3_error_summary(response: reqwest::Response) -> String {
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
