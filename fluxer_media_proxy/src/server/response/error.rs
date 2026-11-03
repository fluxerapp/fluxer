// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{http_headers, request_log::ErrorReason, storage::StorageError};
use axum::{
    body::Body,
    http::{HeaderValue, StatusCode, header},
    response::Response,
};

pub(in crate::server) fn storage_status(err: &StorageError) -> StatusCode {
    match err {
        StorageError::NotFound => StatusCode::NOT_FOUND,
        StorageError::ReadOnlyStorage => StatusCode::FORBIDDEN,
        StorageError::InvalidBucket | StorageError::InvalidKey => StatusCode::BAD_REQUEST,
        StorageError::StreamTooLong => StatusCode::PAYLOAD_TOO_LARGE,
        _ => StatusCode::BAD_GATEWAY,
    }
}

pub(in crate::server) fn storage_error_response(key: &str, err: StorageError) -> Response {
    let status = storage_status(&err);
    let body = if status == StatusCode::NOT_FOUND {
        "Not Found"
    } else {
        canonical_reason_str(status)
    };
    text_with_source(
        status,
        body,
        "storage_error",
        format!("key={key} err={err}"),
    )
}

pub(in crate::server) fn text(status: StatusCode, body: &str) -> Response {
    text_inner(status, body, None)
}

pub(in crate::server) fn text_with_source(
    status: StatusCode,
    body: &str,
    code: &'static str,
    source: impl std::fmt::Debug,
) -> Response {
    text_inner(status, body, Some(ErrorReason::with_source(code, source)))
}

pub(in crate::server) fn text_with_reason(
    status: StatusCode,
    body: &str,
    code: &'static str,
) -> Response {
    text_inner(status, body, Some(ErrorReason::new(code)))
}

const ERROR_CACHE_CONTROL: &str = "no-store";

fn text_inner(status: StatusCode, body: &str, reason: Option<ErrorReason>) -> Response {
    let mut response = Response::new(Body::from(body.to_owned()));
    *response.status_mut() = status;
    http_headers::add_security_headers(response.headers_mut());
    response.headers_mut().insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("text/plain; charset=utf-8"),
    );
    response.headers_mut().insert(
        header::X_CONTENT_TYPE_OPTIONS,
        HeaderValue::from_static("nosniff"),
    );
    if status.is_client_error() || status.is_server_error() {
        response.headers_mut().insert(
            header::CACHE_CONTROL,
            HeaderValue::from_static(ERROR_CACHE_CONTROL),
        );
        response
            .extensions_mut()
            .insert(reason.unwrap_or_else(|| ErrorReason::new(canonical_reason_str(status))));
    }
    response
}

pub(in crate::server) fn canonical_reason_str(status: StatusCode) -> &'static str {
    status.canonical_reason().unwrap_or("error")
}

pub(in crate::server) fn json_response(status: StatusCode, body: String) -> Response {
    let mut response = Response::new(Body::from(body));
    *response.status_mut() = status;
    http_headers::add_security_headers(response.headers_mut());
    response.headers_mut().insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("application/json"),
    );
    if status.is_client_error() || status.is_server_error() {
        response.headers_mut().insert(
            header::CACHE_CONTROL,
            HeaderValue::from_static(ERROR_CACHE_CONTROL),
        );
    }
    response
}
