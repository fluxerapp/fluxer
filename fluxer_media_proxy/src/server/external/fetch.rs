// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    ExternalBody, ExternalFetchError, ExternalSuccessStatus, FetchedExternal,
    byte_range::{ExternalRangeSelection, validate_external_partial},
    is_redirect_status,
    response::{ExternalStreamDecision, external_stream_decision},
};
use crate::{
    byte_budget::{BudgetedBytes, ByteBudget, ByteReservation},
    constants::{self, AssetExtension},
    disposition::PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES,
    http_headers, metrics, mime, public_net_policy,
    request_log::{self, Stage},
    response_body_limit,
    server::{format_policy::image_extension_from_filename, params::url_filename, state::AppState},
};
use axum::http::{HeaderMap, HeaderName, Method, StatusCode, header};
use bytes::Bytes;
use tracing::warn;

const EXTERNAL_FETCH_REDIRECT_LIMIT: usize = 5;
const DEFAULT_EXTERNAL_CONTENT_TYPE: &str = "application/octet-stream";
const EXTERNAL_SNIFF_PREFIX_BYTES: usize = 8192;

#[derive(Clone, Copy, Eq, PartialEq)]
pub(super) enum ExternalFetchMode {
    Buffered,
    Streaming,
}

impl ExternalFetchMode {
    fn allows_streaming(self) -> bool {
        self == Self::Streaming
    }
}

pub(super) struct ExternalFetchRequest<'a> {
    pub(super) app: &'a AppState,
    pub(super) url: &'a str,
    pub(super) range: Option<ExternalRangeSelection<'a>>,
    pub(super) mode: ExternalFetchMode,
}

pub(super) async fn fetch_external_with_range(
    request: ExternalFetchRequest<'_>,
) -> Result<FetchedExternal, ExternalFetchError> {
    let start_ms = metrics::now_ms();
    let result = fetch_external_inner(request).await;
    request_log::record_stage(Stage::Fetch, (metrics::now_ms() - start_ms).max(0) as u64);
    result
}

struct ExternalUpstream {
    url: String,
    response: reqwest::Response,
}

async fn send_external_request(
    app: &AppState,
    url: &str,
    method: Method,
    range: Option<ExternalRangeSelection<'_>>,
) -> Result<ExternalUpstream, ExternalFetchError> {
    let external_metrics = app.metrics.external();
    let mut current_url = url.to_owned();
    let mut visited: Vec<String> = Vec::new();
    visited
        .try_reserve_exact(EXTERNAL_FETCH_REDIRECT_LIMIT + 1)
        .map_err(|_| ExternalFetchError::BufferAllocationFailed)?;
    for _ in 0..=EXTERNAL_FETCH_REDIRECT_LIMIT {
        if visited.iter().any(|seen| seen == &current_url) {
            warn!(url = %current_url, "redirect loop detected");
            return Err(ExternalFetchError::TooManyRedirects);
        }
        visited.push(current_url.clone());
        if let Err(err) = public_net_policy::validate_url(&current_url) {
            warn!(?err, url = %current_url, "blocked external fetch");
            external_metrics.record_blocked_url();
            return Err(ExternalFetchError::BlockedUrl);
        }
        let mut outbound = app
            .media
            .external_client()
            .request(method.clone(), &current_url);
        if let Some(range) = range {
            outbound = outbound.header(header::RANGE, range.header_value());
        }
        let response = outbound.send().await.map_err(|err| {
            if public_net_policy::is_pinned_dns_failure(&err) {
                warn!(?err, url = %current_url, "blocked external fetch");
                external_metrics.record_blocked_url();
                return ExternalFetchError::BlockedUrl;
            }
            warn!(url = %current_url, %err, "external send failed");
            external_metrics.record_fetch_failure();
            ExternalFetchError::FetchFailed
        })?;
        let status = response.status();
        if is_redirect_status(status) {
            let location = match bounded_single_visible_header(
                response.headers(),
                &header::LOCATION,
            ) {
                Some(location) if !location.is_empty() => location,
                Some(_) | None => {
                    warn!(url = %current_url, status = status.as_u16(), "redirect has an invalid Location");
                    external_metrics.record_fetch_failure();
                    return Err(ExternalFetchError::FetchFailed);
                }
            };
            current_url =
                public_net_policy::resolve_redirect(&current_url, location).map_err(|err| {
                    warn!(url = %current_url, %location, ?err, "redirect target blocked");
                    external_metrics.record_blocked_url();
                    ExternalFetchError::BlockedUrl
                })?;
            continue;
        }
        if !status.is_success() {
            return Err(ExternalFetchError::UpstreamFailure(status));
        }
        return Ok(ExternalUpstream {
            url: current_url,
            response,
        });
    }
    Err(ExternalFetchError::TooManyRedirects)
}

pub(super) struct ExternalHead {
    pub(super) url: String,
    pub(super) status: StatusCode,
    pub(super) content_type: String,
    pub(super) content_length: Option<u64>,
}

pub(super) async fn fetch_external_head(
    app: &AppState,
    url: &str,
) -> Result<ExternalHead, ExternalFetchError> {
    let start_ms = metrics::now_ms();
    let result = send_external_request(app, url, Method::HEAD, None).await;
    request_log::record_stage(Stage::Fetch, (metrics::now_ms() - start_ms).max(0) as u64);
    let ExternalUpstream { url, response } = result?;
    Ok(ExternalHead {
        url,
        status: response.status(),
        content_type: external_content_type(response.headers()),
        content_length: validated_content_length(response.headers()),
    })
}

async fn fetch_external_inner(
    request: ExternalFetchRequest<'_>,
) -> Result<FetchedExternal, ExternalFetchError> {
    let ExternalFetchRequest {
        app,
        url,
        range,
        mode,
    } = request;
    let external_metrics = app.metrics.external();
    let ExternalUpstream {
        url: current_url,
        mut response,
    } = send_external_request(app, url, Method::GET, range).await?;
    let content_type = external_content_type(response.headers());
    let declared_length = validated_content_length(response.headers());
    if let Some(len) = declared_length
        && len > constants::MAX_MEDIA_PROXY_BYTES as u64
    {
        warn!(url = %current_url, len, "external payload too large");
        return Err(ExternalFetchError::PayloadTooLarge);
    }
    let status = if response.status() == StatusCode::PARTIAL_CONTENT {
        let content_range =
            bounded_single_visible_header(response.headers(), &header::CONTENT_RANGE);
        let Some(partial) = validate_external_partial(
            range,
            content_range,
            declared_length,
            constants::MAX_MEDIA_PROXY_BYTES,
        ) else {
            warn!(url = %current_url, "invalid upstream partial response");
            external_metrics.record_fetch_failure();
            return Err(ExternalFetchError::FetchFailed);
        };
        ExternalSuccessStatus::Partial(partial)
    } else {
        ExternalSuccessStatus::Complete
    };
    let mut prefix = Bytes::new();
    if let ExternalStreamDecision::Stream(content_length) = external_stream_decision(
        mode.allows_streaming(),
        response.content_length(),
        &content_type,
    ) {
        prefix = external_body_prefix(&mut response, &current_url, &external_metrics).await?;
        if !external_streamed_source_is_svg(&prefix, &url_filename(&current_url)) {
            return Ok(FetchedExternal {
                url: current_url,
                status,
                body: ExternalBody::Streaming {
                    response,
                    prefix,
                    content_length,
                },
                content_type,
            });
        }
    }
    let data = buffer_external_response(ExternalBufferRequest {
        response,
        prefix,
        url: &current_url,
        budget: app.media.external_buffer_bytes(),
        metrics: &external_metrics,
        content_length: declared_length,
        limit: constants::MAX_MEDIA_PROXY_BYTES,
    })
    .await?;
    if status
        .partial()
        .and_then(|partial| partial.content_length())
        .is_some_and(|expected| data.len() as u64 != expected)
    {
        warn!(url = %current_url, "upstream partial response body length mismatch");
        external_metrics.record_fetch_failure();
        return Err(ExternalFetchError::FetchFailed);
    }
    Ok(FetchedExternal {
        url: current_url,
        status,
        body: ExternalBody::Buffered(data),
        content_type,
    })
}

fn external_content_type(headers: &HeaderMap) -> String {
    bounded_single_visible_header(headers, &header::CONTENT_TYPE)
        .filter(|content_type| !content_type.is_empty())
        .unwrap_or(DEFAULT_EXTERNAL_CONTENT_TYPE)
        .to_owned()
}

async fn external_body_prefix(
    response: &mut reqwest::Response,
    url: &str,
    metrics: &metrics::external::ExternalMetrics,
) -> Result<Bytes, ExternalFetchError> {
    let mut prefix: Vec<u8> = Vec::new();
    prefix
        .try_reserve_exact(EXTERNAL_SNIFF_PREFIX_BYTES)
        .map_err(|_| ExternalFetchError::BufferAllocationFailed)?;
    let mut chunks_read = 0_u64;
    let chunks_max =
        response_body_limit::response_body_chunk_limit(constants::MAX_MEDIA_PROXY_BYTES as u64);
    while prefix.len() < EXTERNAL_SNIFF_PREFIX_BYTES {
        let Some(chunk) = response.chunk().await.map_err(|err| {
            warn!(url = %url, %err, "external body read failed");
            metrics.record_fetch_failure();
            ExternalFetchError::FetchFailed
        })?
        else {
            break;
        };
        if chunk.len() > response_body_limit::RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX {
            warn!(url = %url, "external response transport chunk exceeded its byte bound");
            return Err(ExternalFetchError::PayloadTooLarge);
        }
        chunks_read = chunks_read
            .checked_add(1)
            .filter(|chunks| *chunks <= chunks_max)
            .ok_or_else(|| {
                warn!(url = %url, chunks_max, "external sniff prefix exceeded its chunk limit");
                ExternalFetchError::PayloadTooLarge
            })?;
        prefix.extend_from_slice(&chunk);
    }
    Ok(Bytes::from(prefix))
}

pub(super) fn external_streamed_source_is_svg(prefix: &[u8], filename: &str) -> bool {
    image_extension_from_filename(filename) == Some(AssetExtension::Svg)
        || mime::sniff(&prefix[..prefix.len().min(EXTERNAL_SNIFF_PREFIX_BYTES)]).mime
            == "image/svg+xml"
}

pub(super) async fn buffer_external_body(
    app: &AppState,
    url: &str,
    response: reqwest::Response,
    prefix: Bytes,
    content_length: Option<u64>,
) -> Result<BudgetedBytes, ExternalFetchError> {
    let external_metrics = app.metrics.external();
    buffer_external_response(ExternalBufferRequest {
        response,
        prefix,
        url,
        budget: app.media.external_buffer_bytes(),
        metrics: &external_metrics,
        content_length,
        limit: constants::MAX_MEDIA_PROXY_BYTES,
    })
    .await
}

fn bounded_single_visible_header<'a>(headers: &'a HeaderMap, name: &HeaderName) -> Option<&'a str> {
    let mut values = headers.get_all(name).iter();
    let value = values.next()?;
    if values.next().is_some() || value.as_bytes().len() > PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES {
        return None;
    }
    value.to_str().ok()
}

fn validated_content_length(headers: &HeaderMap) -> Option<u64> {
    let mut values = headers.get_all(header::CONTENT_LENGTH).iter();
    let value = values.next()?;
    if values.next().is_some() || value.as_bytes().len() > PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES {
        return None;
    }
    http_headers::parse_content_length(headers)
}

pub(super) struct ExternalBufferRequest<'a> {
    pub(super) response: reqwest::Response,
    pub(super) prefix: Bytes,
    pub(super) url: &'a str,
    pub(super) budget: &'a ByteBudget,
    pub(super) metrics: &'a metrics::external::ExternalMetrics,
    pub(super) content_length: Option<u64>,
    pub(super) limit: usize,
}

pub(super) async fn buffer_external_response(
    request: ExternalBufferRequest<'_>,
) -> Result<BudgetedBytes, ExternalFetchError> {
    let ExternalBufferRequest {
        mut response,
        prefix,
        url,
        budget,
        metrics,
        content_length,
        limit,
    } = request;
    let declared_length = match content_length {
        Some(length) if length > limit as u64 => {
            warn!(url = %url, length, "external payload too large");
            return Err(ExternalFetchError::PayloadTooLarge);
        }
        Some(length) => {
            Some(usize::try_from(length).expect("a bounded external content length fits usize"))
        }
        None => None,
    };
    let initial_capacity = declared_length.unwrap_or(0).max(prefix.len());
    let _transport_chunk_reservation = reserve(
        budget,
        metrics,
        response_body_limit::RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX,
    )?;
    let mut reservation = reserve(budget, metrics, initial_capacity)?;
    let mut buf: Vec<u8> = Vec::new();
    buf.try_reserve_exact(initial_capacity)
        .map_err(|_| ExternalFetchError::BufferAllocationFailed)?;
    let mut reserved_bytes =
        grow_to_capacity(&mut reservation, metrics, buf.capacity(), initial_capacity)?;
    buf.extend_from_slice(&prefix);
    let mut chunks_read = 0_u64;
    let chunks_max = response_body_limit::response_body_chunk_limit(limit as u64);
    while let Some(chunk) = response.chunk().await.map_err(|err| {
        warn!(url = %url, %err, "external body read failed");
        metrics.record_fetch_failure();
        ExternalFetchError::FetchFailed
    })? {
        if chunk.len() > response_body_limit::RESPONSE_BODY_TRANSPORT_CHUNK_BYTES_MAX {
            warn!(url = %url, "external response transport chunk exceeded its byte bound");
            return Err(ExternalFetchError::PayloadTooLarge);
        }
        chunks_read = chunks_read
            .checked_add(1)
            .filter(|chunks| *chunks <= chunks_max)
            .ok_or_else(|| {
                warn!(url = %url, chunks_max, "external payload exceeded its chunk limit");
                ExternalFetchError::PayloadTooLarge
            })?;
        let Some(next_len) = buf
            .len()
            .checked_add(chunk.len())
            .filter(|len| *len <= limit)
        else {
            warn!(url = %url, "external payload too large");
            return Err(ExternalFetchError::PayloadTooLarge);
        };
        if next_len > buf.capacity() {
            let target_capacity = buf
                .capacity()
                .checked_mul(2)
                .expect("a bounded external buffer capacity doubling fits usize")
                .max(next_len)
                .min(limit);
            let additional = target_capacity
                .checked_sub(reserved_bytes)
                .expect("an external buffer target capacity covers its reservation");
            if !reservation.try_grow(additional) {
                metrics.record_buffer_rejected();
                return Err(ExternalFetchError::BufferBudgetExhausted);
            }
            reserved_bytes = target_capacity;
            buf.try_reserve_exact(target_capacity - buf.len())
                .map_err(|_| ExternalFetchError::BufferAllocationFailed)?;
            reserved_bytes =
                grow_to_capacity(&mut reservation, metrics, buf.capacity(), reserved_bytes)?;
        }
        buf.extend_from_slice(&chunk);
    }
    if let Some(expected_length) = declared_length
        && buf.len() != expected_length
    {
        warn!(
            url = %url,
            expected_length,
            actual_length = buf.len(),
            "external buffered body length did not match Content-Length"
        );
        metrics.record_fetch_failure();
        return Err(ExternalFetchError::FetchFailed);
    }
    reservation.shrink_to(buf.capacity());
    Ok(BudgetedBytes::budgeted(Bytes::from(buf), reservation))
}

fn reserve(
    budget: &ByteBudget,
    metrics: &metrics::external::ExternalMetrics,
    amount: usize,
) -> Result<ByteReservation, ExternalFetchError> {
    budget.try_reserve(amount).ok_or_else(|| {
        metrics.record_buffer_rejected();
        ExternalFetchError::BufferBudgetExhausted
    })
}

fn grow_to_capacity(
    reservation: &mut ByteReservation,
    metrics: &metrics::external::ExternalMetrics,
    capacity: usize,
    reserved_bytes: usize,
) -> Result<usize, ExternalFetchError> {
    if capacity <= reserved_bytes {
        return Ok(reserved_bytes);
    }
    if !reservation.try_grow(capacity - reserved_bytes) {
        metrics.record_buffer_rejected();
        return Err(ExternalFetchError::BufferBudgetExhausted);
    }
    Ok(capacity)
}
