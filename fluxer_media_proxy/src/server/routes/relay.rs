// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::DeploymentMode,
    external_path, http_headers,
    server::{
        relay::body::{
            RelayBodyProgress, RelayBodyStream, RelayBodyStreamRequest, relay_body_chunks,
            relay_etag, validate_completed_relay_body,
        },
        response::error::text,
        state::AppState,
    },
    spool::{SpoolError, spool_to_temp},
    storage::{RelayBody, RelayPutOptions},
    upload_relay::{RelayError, target, token},
};
use axum::{
    body::Body,
    extract::{Path, Query, State},
    http::{HeaderMap, HeaderValue, Request, StatusCode, header},
    response::Response,
};
use std::{
    collections::HashMap,
    sync::{Arc, OnceLock},
    time::Duration,
};
use tracing::warn;

const RELAY_STREAM_MIN_CLIENT_BYTES_PER_SEC: u64 = 16 * 1024;

struct StreamedRelayBody {
    failure: Arc<OnceLock<RelayError>>,
    progress: RelayBodyProgress,
}

pub(in crate::server) async fn relay_options() -> Response {
    let mut response = Response::new(Body::empty());
    *response.status_mut() = StatusCode::NO_CONTENT;
    http_headers::add_security_headers(response.headers_mut());
    relay_cors(response.headers_mut());
    response.headers_mut().insert(
        header::ACCESS_CONTROL_MAX_AGE,
        HeaderValue::from_static("600"),
    );
    response
}

pub(in crate::server) async fn relay_put(
    State(app): State<Arc<AppState>>,
    Path(key): Path<String>,
    Query(params): Query<HashMap<String, String>>,
    headers: HeaderMap,
    request: Request<Body>,
) -> Response {
    if app.cfg.mode != DeploymentMode::Upload {
        return text(StatusCode::NOT_FOUND, "Not Found");
    }
    let key = external_path::percent_decode_string(&key, false);
    let token_raw = match token::token_from_query(params.get("t").map(String::as_str)) {
        Ok(token_raw) => token_raw,
        Err(err) => return relay_error(err),
    };
    let token = match token::decode_token(
        token_raw,
        app.cfg.upload_relay.secret.expose(),
        token::now_unix(),
    ) {
        Ok(token) => token,
        Err(err) => return relay_error(token::map_token_error(err)),
    };
    let part_number = match target::query_part_number(params.get("partNumber").map(String::as_str))
    {
        Ok(part_number) => part_number,
        Err(err) => return relay_error(err),
    };
    let content_length = http_headers::parse_content_length(&headers);
    if let Err(err) = target::validate_relay_request(
        &token,
        target::RelayRequest {
            uploads_bucket: &app.cfg.storage.bucket_uploads,
            request_key: &key,
            request_method: request.method(),
            query_upload_id: params.get("uploadId").map(String::as_str),
            query_part_number: part_number,
            content_length,
            max_body_bytes: app.cfg.upload_relay.max_body_bytes,
        },
    ) {
        return relay_error(err);
    }
    let timeout_ms = relay_upstream_timeout_ms(&app, content_length);
    let (body, body_length, streamed) = match content_length {
        Some(declared) => {
            let Some(deadline) =
                tokio::time::Instant::now().checked_add(Duration::from_millis(timeout_ms))
            else {
                return relay_error(RelayError::InternalError);
            };
            let failure = Arc::new(OnceLock::new());
            let stream = RelayBodyStream::new(RelayBodyStreamRequest {
                body: request.into_body(),
                declared_length: declared,
                deadline,
                failure: Arc::clone(&failure),
            });
            let progress = stream.progress();
            (
                RelayBody::Streamed(relay_body_chunks(stream)),
                declared,
                Some(StreamedRelayBody { failure, progress }),
            )
        }
        None => {
            let body_length_limit = token.mb.min(app.cfg.upload_relay.max_body_bytes);
            let spooled = match spool_to_temp(
                request.into_body(),
                content_length,
                body_length_limit,
                &app.cfg.upload_relay.spool_dir,
                app.cfg.upload_relay.spool_chunk_bytes,
                app.cfg.upload_relay.spool_max_total_bytes,
            )
            .await
            {
                Ok(spooled) => spooled,
                Err(SpoolError::PayloadTooLarge) => {
                    return relay_error(RelayError::PayloadTooLarge);
                }
                Err(SpoolError::PayloadShortRead) | Err(SpoolError::Body(_)) => {
                    return relay_error(RelayError::ClientUploadFailed);
                }
                Err(SpoolError::BudgetExhausted) => {
                    app.metrics.relay().record_retryable_failure();
                    return relay_error(RelayError::UpstreamRetryable);
                }
                Err(SpoolError::Io(_)) => {
                    return relay_error(RelayError::InternalError);
                }
            };
            let (file, spooled_length) = spooled.into_parts();
            (RelayBody::Spooled(file), spooled_length, None)
        }
    };
    let content_type = resolve_relay_content_type(
        token.ct.as_deref(),
        headers
            .get(header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok()),
    );
    let options = RelayPutOptions {
        body,
        content_length: body_length,
        content_type,
        upload_id: params.get("uploadId").cloned(),
        part_number,
        timeout_ms,
    };
    match app
        .store
        .relay_put_object(&app.cfg.storage.bucket_uploads, &key, options)
        .await
    {
        Ok(etag) => match relay_success_etag(etag, streamed.as_ref(), body_length) {
            Ok(etag) => {
                app.metrics.relay().record_success();
                relay_success_response(etag)
            }
            Err(err) => relay_error(err),
        },
        Err(err) => {
            if let Some(client_err) = streamed
                .as_ref()
                .and_then(|streamed| streamed.failure.get())
            {
                return relay_error(*client_err);
            }
            warn!(error = %err, "upload relay upstream S3 PUT failed");
            app.metrics.relay().record_hard_failure();
            relay_error(RelayError::UpstreamS3Error)
        }
    }
}

fn relay_upstream_timeout_ms(app: &AppState, content_length: Option<u64>) -> u64 {
    let Some(declared) = content_length else {
        return app.cfg.upload_relay.s3_timeout_ms;
    };
    app.cfg.upload_relay.s3_timeout_ms.saturating_add(
        declared
            .div_ceil(RELAY_STREAM_MIN_CLIENT_BYTES_PER_SEC)
            .saturating_mul(1000),
    )
}

fn relay_success_etag(
    upstream_etag: Option<String>,
    streamed: Option<&StreamedRelayBody>,
    body_length: u64,
) -> Result<Option<HeaderValue>, RelayError> {
    if let Some(streamed) = streamed {
        validate_completed_relay_body(&streamed.failure, &streamed.progress, body_length)?;
    }
    // Only the store's own entity tag goes back to the client, because that is the value S3 wants
    // when the client completes a multipart upload. A tag invented here would be rejected there,
    // so a store that returns none leaves the response without an ETag.
    Ok(upstream_etag.as_deref().map(relay_etag))
}

fn relay_success_response(etag: Option<HeaderValue>) -> Response {
    let mut response = Response::new(Body::empty());
    *response.status_mut() = StatusCode::OK;
    http_headers::add_security_headers(response.headers_mut());
    relay_cors(response.headers_mut());
    if let Some(etag) = etag {
        response.headers_mut().insert(header::ETAG, etag);
    }
    response
}

fn resolve_relay_content_type(
    token_content_type: Option<&str>,
    header_content_type: Option<&str>,
) -> Option<String> {
    token_content_type
        .or(header_content_type)
        .filter(|value| target::valid_content_type(value))
        .map(ToOwned::to_owned)
}

pub(in crate::server) fn relay_error(err: RelayError) -> Response {
    let status = match err {
        RelayError::MissingToken | RelayError::InvalidToken | RelayError::RelayTokenExpired => {
            StatusCode::UNAUTHORIZED
        }
        RelayError::WrongBucket
        | RelayError::KeyMismatch
        | RelayError::MethodMismatch
        | RelayError::PartNumberMismatch
        | RelayError::UploadIdMismatch => StatusCode::FORBIDDEN,
        RelayError::PayloadTooLarge => StatusCode::PAYLOAD_TOO_LARGE,
        RelayError::UpstreamRetryable => StatusCode::SERVICE_UNAVAILABLE,
        RelayError::UpstreamS3Error => StatusCode::BAD_GATEWAY,
        RelayError::InternalError => StatusCode::INTERNAL_SERVER_ERROR,
        _ => StatusCode::BAD_REQUEST,
    };
    let mut response = text(status, status.canonical_reason().unwrap_or("Bad Request"));
    relay_cors(response.headers_mut());
    response
}

pub(in crate::server) fn relay_cors(headers: &mut HeaderMap) {
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_ORIGIN,
        HeaderValue::from_static("*"),
    );
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_METHODS,
        HeaderValue::from_static("PUT, OPTIONS"),
    );
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_HEADERS,
        HeaderValue::from_static(
            "Content-Type, Content-Length, Authorization, X-Fluxer-Features, X-Client-Context",
        ),
    );
    headers.insert(
        header::ACCESS_CONTROL_EXPOSE_HEADERS,
        HeaderValue::from_static("ETag, X-Fluxer-Version"),
    );
}
