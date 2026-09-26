// SPDX-License-Identifier: AGPL-3.0-or-later

mod byte_range;
mod fetch;
mod response;

use crate::{
    byte_budget::BudgetedBytes,
    config::Config,
    constants::{self, AssetExtension},
    external_path, mime,
    server::{
        download_stream::DownloadStreamPolicy,
        external::{
            byte_range::{ExternalPartial, ExternalRangeSelection, parse_external_requested_range},
            fetch::{
                ExternalFetchMode, ExternalFetchRequest, ExternalHead, buffer_external_body,
                fetch_external_head, fetch_external_with_range,
            },
            response::{
                ExternalPartialResponse, ExternalStreamingResponse, external_partial_response,
                external_streaming_response,
            },
        },
        format_policy::{
            content_type_is_trustworthy, image_extension_from_filename, is_svg_content_type,
            source_image_format,
        },
        params::{animated_param, bool_param, url_filename},
        response::{
            content_disposition_header,
            error::{text, text_with_source},
        },
        state::AppState,
        stored::response::passthrough_head_response,
        transform::{
            CachedTransformProbe, ServeBytesRequest, cached_transform_response,
            parameters::TransformRoute, serve_bytes_or_transform,
        },
    },
    signing,
};
use axum::{
    http::{HeaderMap, Method, StatusCode, header},
    response::Response,
};
use bytes::Bytes;
use std::{collections::HashMap, sync::Arc, time::Duration};

const EXTERNAL_SNIFF_PREFIX_BYTES: usize = 8192;
const EXTERNAL_HINT_CACHE_ENTRIES: u64 = 4096;

pub(in crate::server) type ExternalHintCache = moka::sync::Cache<String, ExternalHint>;

// What the origin last served for a url, so a repeat transform of a hot embed can be
// answered from the transform cache instead of downloading the origin asset again.
#[derive(Clone)]
pub(in crate::server) struct ExternalHint {
    url: String,
    content_type: String,
    source_format: Option<AssetExtension>,
}

pub(in crate::server) fn new_external_hint_cache(cfg: &Config) -> ExternalHintCache {
    moka::sync::Cache::builder()
        .max_capacity(EXTERNAL_HINT_CACHE_ENTRIES)
        .time_to_live(Duration::from_millis(
            cfg.media.transform_cache_ttl_ms.max(1),
        ))
        .build()
}

fn external_sniffed_content_type(data: &[u8], filename: &str, content_type: String) -> String {
    let prefix = &data[..data.len().min(EXTERNAL_SNIFF_PREFIX_BYTES)];
    if mime::sniff(prefix).mime == "image/svg+xml" {
        return "image/svg+xml".to_owned();
    }
    if content_type_is_trustworthy(&content_type) {
        return content_type;
    }
    mime::detect(prefix, filename, Some(&content_type))
}

fn external_hint(url: &str, filename: &str, content_type: &str, data: &[u8]) -> ExternalHint {
    let content_type = external_sniffed_content_type(data, filename, content_type.to_owned());
    let source_format = source_image_format(mime::sniff(data).mime, &content_type, filename);
    ExternalHint {
        url: url.to_owned(),
        content_type,
        source_format,
    }
}

fn external_cached_transform(
    app: &Arc<AppState>,
    url: &str,
    method: &Method,
    params: &HashMap<String, String>,
    headers: &HeaderMap,
) -> Option<Response> {
    let hint = app.media.external_hints().get(url)?;
    let filename = url_filename(&hint.url);
    cached_transform_response(
        app.media.transforms(),
        CachedTransformProbe {
            method: method.clone(),
            content_type: &hint.content_type,
            source_format: hint.source_format,
            cache_identity: &hint.url,
            filename: &filename,
            route: TransformRoute::External,
            params,
            headers,
        },
    )
}

pub(in crate::server) struct FetchedExternal {
    url: String,
    status: ExternalSuccessStatus,
    body: ExternalBody,
    content_type: String,
}

#[derive(Clone)]
enum ExternalSuccessStatus {
    Complete,
    Partial(ExternalPartial),
}

impl ExternalSuccessStatus {
    fn partial(&self) -> Option<ExternalPartial> {
        match self {
            Self::Partial(partial) => Some(partial.clone()),
            Self::Complete => None,
        }
    }

    fn is_partial(&self) -> bool {
        matches!(self, Self::Partial(_))
    }
}

enum ExternalBody {
    Buffered(BudgetedBytes),
    Streaming {
        response: reqwest::Response,
        prefix: Bytes,
        content_length: Option<u64>,
    },
}

impl ExternalBody {
    async fn into_buffered(
        self,
        app: &AppState,
        url: &str,
    ) -> Result<BudgetedBytes, ExternalFetchError> {
        match self {
            Self::Buffered(data) => Ok(data),
            Self::Streaming {
                response,
                prefix,
                content_length,
            } => buffer_external_body(app, url, response, prefix, content_length).await,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(in crate::server) enum ExternalFetchError {
    BlockedUrl,
    PayloadTooLarge,
    BufferBudgetExhausted,
    BufferAllocationFailed,
    UpstreamFailure(StatusCode),
    TooManyRedirects,
    FetchFailed,
}

struct PreparedExternalRequest<'a> {
    app: &'a Arc<AppState>,
    method: Method,
    params: &'a HashMap<String, String>,
    headers: &'a HeaderMap,
    url: String,
    wants_transform: bool,
    forward_range: Option<ExternalRangeSelection<'a>>,
}

impl<'a> PreparedExternalRequest<'a> {
    fn new(
        app: &'a Arc<AppState>,
        method: Method,
        rest: &str,
        params: &'a HashMap<String, String>,
        headers: &'a HeaderMap,
    ) -> Result<Self, Box<Response>> {
        let Some((sig, proxy_path)) = rest.split_once('/') else {
            return Err(Box::new(text(StatusCode::BAD_REQUEST, "Bad Request")));
        };
        if !signing::verify_signature(proxy_path, sig, app.cfg.secret_key.as_bytes()) {
            return Err(Box::new(text(StatusCode::UNAUTHORIZED, "Unauthorized")));
        }
        let Ok(url) = external_path::reconstruct_original_url(proxy_path) else {
            return Err(Box::new(text(StatusCode::BAD_REQUEST, "Bad Request")));
        };
        let url_ext_is_svg =
            image_extension_from_filename(&url_filename(&url)) == Some(AssetExtension::Svg);
        let wants_transform = url_ext_is_svg
            || params.contains_key("width")
            || params.contains_key("height")
            || params.contains_key("format")
            || params.contains_key("quality")
            || animated_param(params, false);
        let client_range = headers
            .get(header::RANGE)
            .and_then(|value| value.to_str().ok())
            .and_then(parse_external_requested_range);
        let forward_range = if wants_transform { None } else { client_range };
        Ok(Self {
            app,
            method,
            params,
            headers,
            url,
            wants_transform,
            forward_range,
        })
    }

    fn initial_fetch_mode(&self) -> ExternalFetchMode {
        if self.wants_transform {
            ExternalFetchMode::Buffered
        } else {
            ExternalFetchMode::Streaming
        }
    }

    async fn fetch(&self) -> Result<FetchedExternal, Box<Response>> {
        let fetched = self
            .fetch_with(self.forward_range, self.initial_fetch_mode())
            .await?;
        if self.forward_range.is_some()
            && fetched.status.is_partial()
            && is_svg_content_type(&fetched.content_type)
        {
            drop(fetched);
            return self.fetch_with(None, ExternalFetchMode::Buffered).await;
        }
        Ok(fetched)
    }

    async fn fetch_with(
        &self,
        range: Option<ExternalRangeSelection<'_>>,
        mode: ExternalFetchMode,
    ) -> Result<FetchedExternal, Box<Response>> {
        fetch_external_with_range(ExternalFetchRequest {
            app: self.app,
            url: &self.url,
            range,
            mode,
        })
        .await
        .map_err(|err| Box::new(external_fetch_error_response(&self.url, err)))
    }

    fn stream_policy(&self) -> DownloadStreamPolicy {
        DownloadStreamPolicy::for_external(
            self.app.cfg.socket_io_timeout_ms,
            self.app.metrics.external(),
        )
    }

    fn wants_head_passthrough(&self) -> bool {
        self.method == Method::HEAD && !self.wants_transform && self.forward_range.is_none()
    }
}

pub(in crate::server) async fn serve_external(
    app: &Arc<AppState>,
    method: Method,
    rest: &str,
    params: &HashMap<String, String>,
    headers: &HeaderMap,
) -> Response {
    let prepared = match PreparedExternalRequest::new(app, method, rest, params, headers) {
        Ok(prepared) => prepared,
        Err(response) => return *response,
    };
    if prepared.wants_transform
        && let Some(cached) =
            external_cached_transform(app, &prepared.url, &prepared.method, params, headers)
    {
        return cached;
    }
    if prepared.wants_head_passthrough()
        && let Some(response) = external_head_passthrough(app, &prepared.url, params).await
    {
        return response;
    }
    let fetched = match prepared.fetch().await {
        Ok(fetched) => fetched,
        Err(response) => return *response,
    };
    serve_fetched_external(prepared, fetched).await
}

// Unlike the object store, a third-party origin answers a GET by sending a body the proxy
// then discards, and an untrusted content type makes the proxy buffer that body in full.
// A metadata-only probe is worth its own upstream HEAD here.
async fn external_head_passthrough(
    app: &AppState,
    url: &str,
    params: &HashMap<String, String>,
) -> Option<Response> {
    let head = fetch_external_head(app, url).await.ok()?;
    external_head_response(head, bool_param(params, "download", false))
}

fn external_head_response(head: ExternalHead, requested_download: bool) -> Option<Response> {
    if head.status != StatusCode::OK || is_svg_content_type(&head.content_type) {
        return None;
    }
    let total_len = usize::try_from(head.content_length?).ok()?;
    if total_len > constants::MAX_MEDIA_PROXY_BYTES {
        return None;
    }
    let filename = url_filename(&head.url);
    let content_type = if content_type_is_trustworthy(&head.content_type) {
        head.content_type
    } else {
        mime::detect(&[], &filename, Some(&head.content_type))
    };
    let disposition =
        content_disposition_header(&content_type, requested_download, Some(&filename));
    Some(passthrough_head_response(
        &content_type,
        total_len,
        None,
        Some(disposition),
    ))
}

async fn serve_fetched_external(
    prepared: PreparedExternalRequest<'_>,
    fetched: FetchedExternal,
) -> Response {
    let stream_policy = prepared.stream_policy();
    let PreparedExternalRequest {
        app,
        method,
        params,
        headers,
        url: requested_url,
        forward_range,
        ..
    } = prepared;
    let filename = url_filename(&fetched.url);
    let requested_download = bool_param(params, "download", false);
    if forward_range.is_some()
        && let Some(partial) = fetched.status.partial()
    {
        let disposition = Some(content_disposition_header(
            &fetched.content_type,
            requested_download,
            Some(&filename),
        ));
        return external_partial_response(ExternalPartialResponse {
            method,
            fetched,
            partial,
            disposition,
            stream_policy,
        });
    }
    let FetchedExternal {
        url: fetched_url,
        body,
        content_type,
        ..
    } = fetched;
    let data = match body {
        ExternalBody::Streaming {
            response,
            prefix,
            content_length,
        } => {
            let disposition = Some(content_disposition_header(
                &content_type,
                requested_download,
                Some(&filename),
            ));
            return external_streaming_response(ExternalStreamingResponse {
                method,
                response,
                prefix,
                content_length,
                content_type: &content_type,
                disposition,
                stream_policy,
            });
        }
        body => match body.into_buffered(app, &fetched_url).await {
            Ok(data) => data,
            Err(err) => return external_fetch_error_response(&fetched_url, err),
        },
    };
    app.media.external_hints().insert(
        requested_url,
        external_hint(&fetched_url, &filename, &content_type, data.as_bytes()),
    );
    serve_bytes_or_transform(
        app.media.transforms(),
        ServeBytesRequest {
            method,
            data: data.as_bytes().clone(),
            content_type,
            cache_identity: &fetched_url,
            filename: &filename,
            route: TransformRoute::External,
            params,
            headers,
        },
    )
    .await
}

pub(in crate::server) async fn fetch_external(
    app: &AppState,
    url: &str,
) -> Result<(String, BudgetedBytes), ExternalFetchError> {
    let fetched = fetch_external_with_range(ExternalFetchRequest {
        app,
        url,
        range: None,
        mode: ExternalFetchMode::Buffered,
    })
    .await?;
    let FetchedExternal { url, body, .. } = fetched;
    let data = body.into_buffered(app, &url).await?;
    Ok((url, data))
}

fn external_fetch_error_response(url: &str, err: ExternalFetchError) -> Response {
    match err {
        ExternalFetchError::BlockedUrl => text_with_source(
            StatusCode::BAD_REQUEST,
            "Bad Request",
            "external_blocked_url",
            url,
        ),
        ExternalFetchError::PayloadTooLarge => text_with_source(
            StatusCode::PAYLOAD_TOO_LARGE,
            "Payload Too Large",
            "external_payload_too_large",
            url,
        ),
        ExternalFetchError::BufferBudgetExhausted => text_with_source(
            StatusCode::SERVICE_UNAVAILABLE,
            "Service Unavailable",
            "external_buffer_budget_exhausted",
            url,
        ),
        ExternalFetchError::BufferAllocationFailed => text_with_source(
            StatusCode::SERVICE_UNAVAILABLE,
            "Service Unavailable",
            "external_buffer_allocation_failed",
            url,
        ),
        ExternalFetchError::UpstreamFailure(status) => text_with_source(
            map_upstream_status(status),
            "Upstream fetch failed",
            "external_upstream_status",
            format!("url={url} upstream_status={}", status.as_u16()),
        ),
        ExternalFetchError::TooManyRedirects | ExternalFetchError::FetchFailed => text_with_source(
            StatusCode::BAD_GATEWAY,
            "Bad Gateway",
            "external_fetch_failed",
            format!("url={url} err={err:?}"),
        ),
    }
}

pub(in crate::server) fn map_upstream_status(status: StatusCode) -> StatusCode {
    match status.as_u16() {
        400 => StatusCode::BAD_REQUEST,
        401 => StatusCode::UNAUTHORIZED,
        403 => StatusCode::FORBIDDEN,
        404 => StatusCode::NOT_FOUND,
        405 => StatusCode::METHOD_NOT_ALLOWED,
        406 => StatusCode::NOT_ACCEPTABLE,
        408 => StatusCode::REQUEST_TIMEOUT,
        409 => StatusCode::CONFLICT,
        410 => StatusCode::GONE,
        411 => StatusCode::LENGTH_REQUIRED,
        412 => StatusCode::PRECONDITION_FAILED,
        413 => StatusCode::PAYLOAD_TOO_LARGE,
        414 => StatusCode::URI_TOO_LONG,
        415 => StatusCode::UNSUPPORTED_MEDIA_TYPE,
        416 => StatusCode::RANGE_NOT_SATISFIABLE,
        428 => StatusCode::from_u16(428).expect("428 is a valid status code"),
        429 => StatusCode::TOO_MANY_REQUESTS,
        _ => StatusCode::BAD_GATEWAY,
    }
}

pub(in crate::server) fn map_internal_metadata_upstream_status(status: StatusCode) -> StatusCode {
    match status.as_u16() {
        429 => StatusCode::SERVICE_UNAVAILABLE,
        _ => map_upstream_status(status),
    }
}

fn is_redirect_status(status: StatusCode) -> bool {
    matches!(
        status,
        StatusCode::MOVED_PERMANENTLY
            | StatusCode::FOUND
            | StatusCode::SEE_OTHER
            | StatusCode::TEMPORARY_REDIRECT
            | StatusCode::PERMANENT_REDIRECT
    )
}

#[cfg(test)]
mod tests;
