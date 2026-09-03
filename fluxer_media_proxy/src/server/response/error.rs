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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        metrics::Metrics,
        request_log::{RequestId, trace_public_request},
    };
    use axum::http::{HeaderMap, Method};
    use std::sync::{Arc, Mutex};
    use tracing_subscriber::fmt::MakeWriter;

    #[derive(Clone, Default)]
    struct CapturedLog(Arc<Mutex<Vec<u8>>>);

    impl CapturedLog {
        fn text(&self) -> String {
            String::from_utf8(self.0.lock().expect("captured log is not poisoned").clone())
                .expect("captured log is utf-8")
        }
    }

    impl std::io::Write for CapturedLog {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0
                .lock()
                .expect("captured log is not poisoned")
                .extend_from_slice(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    impl<'a> MakeWriter<'a> for CapturedLog {
        type Writer = Self;

        fn make_writer(&'a self) -> Self::Writer {
            self.clone()
        }
    }

    #[test]
    fn text_responses_set_nosniff_header() {
        let response = text(StatusCode::NOT_FOUND, "Not Found");
        assert_eq!(
            "nosniff",
            response
                .headers()
                .get(header::X_CONTENT_TYPE_OPTIONS)
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert_eq!(
            http_headers::STRICT_TRANSPORT_SECURITY,
            response
                .headers()
                .get("strict-transport-security")
                .unwrap()
                .to_str()
                .unwrap()
        );
        assert!(
            response
                .headers()
                .contains_key(header::CONTENT_SECURITY_POLICY)
        );
        assert!(response.headers().contains_key("permissions-policy"));
    }

    #[test]
    fn error_responses_declare_an_explicit_no_store_policy() {
        for status in [
            StatusCode::NOT_FOUND,
            StatusCode::BAD_GATEWAY,
            StatusCode::INTERNAL_SERVER_ERROR,
        ] {
            for response in [text(status, "nope"), json_response(status, "{}".to_owned())] {
                assert_eq!(
                    ERROR_CACHE_CONTROL,
                    response
                        .headers()
                        .get(header::CACHE_CONTROL)
                        .expect("an error response declares a cache policy"),
                    "status {status} must not be cacheable"
                );
            }
        }
    }

    #[test]
    fn successful_text_responses_are_left_to_the_media_cache_policy() {
        assert!(
            text(StatusCode::OK, "fine")
                .headers()
                .get(header::CACHE_CONTROL)
                .is_none()
        );
        assert!(
            json_response(StatusCode::OK, "{}".to_owned())
                .headers()
                .get(header::CACHE_CONTROL)
                .is_none()
        );
    }

    #[tokio::test]
    async fn a_very_long_storage_key_cannot_bloat_the_logged_source() {
        let key = format!("attachments/1/2/{}.png", "k".repeat(4096));
        let captured = CapturedLog::default();
        let subscriber = tracing_subscriber::fmt()
            .with_writer(captured.clone())
            .with_ansi(false)
            .with_max_level(tracing::Level::TRACE)
            .finish();
        let response = {
            let _guard = tracing::subscriber::set_default(subscriber);
            let metrics = Metrics::new();
            trace_public_request(
                metrics.request().as_ref(),
                RequestId::generate(),
                Method::GET,
                "/attachments/1/2/missing.png",
                &HeaderMap::new(),
                async { storage_error_response(&key, StorageError::NotFound) },
            )
            .await
        };
        assert_eq!(StatusCode::NOT_FOUND, response.status());
        let line = captured.text();
        assert!(line.contains("reason=\"storage_error\""), "{line}");
        assert!(!line.contains(&key), "{line}");
        let source = line
            .split("source=")
            .nth(1)
            .expect("a failure log line carries a source field")
            .trim_end();
        assert_eq!(513, source.len(), "{source}");
        assert!(source.ends_with('~'), "{source}");
        assert!(source.contains("key=attachments/1/2/kkk"), "{source}");
    }
}
