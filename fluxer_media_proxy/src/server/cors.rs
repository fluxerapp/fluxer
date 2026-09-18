// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{request_log, server::response::error::text_with_source};
use axum::{
    http::{HeaderMap, HeaderValue, Method, StatusCode, header},
    response::Response,
};

const VARY_ON_ORIGIN: &str = "Accept-Encoding, Origin";
const ORIGIN_LOG_BYTES_MAX: usize = 256;

#[derive(Debug, Eq, PartialEq)]
pub(in crate::server) enum OriginCheck {
    Absent,
    Allowed(HeaderValue),
    Refused,
}

pub(in crate::server) fn check_origin(allowed: &[HeaderValue], request: &HeaderMap) -> OriginCheck {
    let mut origins = request.get_all(header::ORIGIN).iter();
    let Some(origin) = origins.next() else {
        return OriginCheck::Absent;
    };
    if origins.next().is_some() {
        return OriginCheck::Refused;
    }
    allowed
        .iter()
        .find(|candidate| candidate.as_bytes() == origin.as_bytes())
        .map_or(OriginCheck::Refused, |candidate| {
            OriginCheck::Allowed(candidate.clone())
        })
}

pub(in crate::server) fn refused_response(request: &HeaderMap) -> Response {
    let mut response = text_with_source(
        StatusCode::FORBIDDEN,
        "Forbidden",
        "cors_origin_denied",
        format!("origin={}", origin_for_log(request)),
    );
    response
        .headers_mut()
        .insert(header::VARY, HeaderValue::from_static(VARY_ON_ORIGIN));
    response
}

pub(in crate::server) fn log_would_refuse(method: &Method, path: &str, request: &HeaderMap) {
    tracing::warn!(
        reason = "cors_origin_would_deny",
        kind = request_log::classify_route(path).label(),
        method = %method,
        origin = %origin_for_log(request),
        "public read origin would be refused"
    );
}

pub(in crate::server) fn narrow_response(check: &OriginCheck, headers: &mut HeaderMap) {
    headers.insert(header::VARY, HeaderValue::from_static(VARY_ON_ORIGIN));
    if !headers.contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN) {
        return;
    }
    match check {
        OriginCheck::Allowed(origin) => {
            headers.insert(header::ACCESS_CONTROL_ALLOW_ORIGIN, origin.clone());
        }
        OriginCheck::Absent | OriginCheck::Refused => {
            headers.remove(header::ACCESS_CONTROL_ALLOW_ORIGIN);
        }
    }
}

fn origin_for_log(request: &HeaderMap) -> &str {
    let Some(Ok(origin)) = request.get(header::ORIGIN).map(HeaderValue::to_str) else {
        return "";
    };
    &origin[..origin.len().min(ORIGIN_LOG_BYTES_MAX)]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        http_headers::{self, add_media_headers, add_unsatisfiable_headers},
        request_log::ErrorReason,
        server::response::error::text,
        test_fixtures::ADVERSARIAL_TEXT_INPUTS,
    };
    use axum::body::to_bytes;
    use std::sync::{Arc, Mutex};
    use tracing_subscriber::fmt::MakeWriter;

    const WEB: &str = "https://web.fluxer.app";
    const CANARY: &str = "https://web.canary.fluxer.app";

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

    fn allowlist() -> Vec<HeaderValue> {
        vec![
            HeaderValue::from_static(WEB),
            HeaderValue::from_static(CANARY),
        ]
    }

    fn origin_headers(origins: &[HeaderValue]) -> HeaderMap {
        let mut headers = HeaderMap::new();
        for origin in origins {
            headers.append(header::ORIGIN, origin.clone());
        }
        headers
    }

    fn check(origins: &[HeaderValue]) -> OriginCheck {
        check_origin(&allowlist(), &origin_headers(origins))
    }

    fn header_text(headers: &HeaderMap, name: header::HeaderName) -> Option<&str> {
        headers
            .get(name)
            .map(|value| value.to_str().expect("header is ascii"))
    }

    fn vary_values(headers: &HeaderMap) -> Vec<&str> {
        headers
            .get_all(header::VARY)
            .iter()
            .map(|value| value.to_str().expect("vary is ascii"))
            .collect()
    }

    #[test]
    fn origin_check_classifies_every_origin_shape() {
        assert_eq!(OriginCheck::Absent, check(&[]));
        for allowed in [WEB, CANARY] {
            assert_eq!(
                OriginCheck::Allowed(HeaderValue::from_static(allowed)),
                check(&[HeaderValue::from_static(allowed)]),
                "{allowed}"
            );
        }
        for refused in [
            "null",
            "https://evil.example",
            "https://web.fluxer.app/",
            "HTTPS://WEB.FLUXER.APP",
            "https://web.fluxer.app:443",
            "http://web.fluxer.app",
            "https://web.fluxer.app.evil.example",
            "",
        ] {
            assert_eq!(
                OriginCheck::Refused,
                check(&[HeaderValue::from_static(refused)]),
                "{refused:?}"
            );
        }
        assert_eq!(
            OriginCheck::Refused,
            check(&[HeaderValue::from_bytes(&[0xC3, 0x28]).expect("opaque origin")])
        );
        assert_eq!(
            OriginCheck::Refused,
            check(&[HeaderValue::from_static(WEB), HeaderValue::from_static(WEB)])
        );
        assert_eq!(
            OriginCheck::Refused,
            check(&[
                HeaderValue::from_static(WEB),
                HeaderValue::from_static(CANARY)
            ])
        );
    }

    #[test]
    fn adversarial_text_is_never_an_allowed_origin() {
        let mut checked = 0;
        for text in ADVERSARIAL_TEXT_INPUTS {
            let Ok(origin) = HeaderValue::from_str(text) else {
                continue;
            };
            assert_eq!(OriginCheck::Refused, check(&[origin]), "{text:?}");
            checked += 1;
        }
        assert!(
            checked > 0,
            "at least one adversarial input is a header value"
        );
    }

    #[test]
    fn narrowing_rewrites_only_an_allow_origin_a_builder_set() {
        let allowed = OriginCheck::Allowed(HeaderValue::from_static(WEB));

        let mut media_allowed = HeaderMap::new();
        add_media_headers(&mut media_allowed, 16, "image/png", None);
        narrow_response(&allowed, &mut media_allowed);
        assert_eq!(
            Some(WEB),
            header_text(&media_allowed, header::ACCESS_CONTROL_ALLOW_ORIGIN)
        );

        let mut media_absent = HeaderMap::new();
        add_media_headers(&mut media_absent, 16, "image/png", None);
        narrow_response(&OriginCheck::Absent, &mut media_absent);
        assert!(!media_absent.contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN));

        let mut media_refused = HeaderMap::new();
        add_media_headers(&mut media_refused, 16, "image/png", None);
        narrow_response(&OriginCheck::Refused, &mut media_refused);
        assert!(!media_refused.contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN));

        let mut unsatisfiable = HeaderMap::new();
        add_unsatisfiable_headers(&mut unsatisfiable, 16);
        narrow_response(&allowed, &mut unsatisfiable);
        assert_eq!(
            Some(WEB),
            header_text(&unsatisfiable, header::ACCESS_CONTROL_ALLOW_ORIGIN)
        );

        let mut not_found = text(StatusCode::NOT_FOUND, "Not Found").headers().clone();
        narrow_response(&allowed, &mut not_found);
        assert!(!not_found.contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN));

        for headers in [
            &media_allowed,
            &media_absent,
            &media_refused,
            &unsatisfiable,
            &not_found,
        ] {
            assert_eq!(vec!["Accept-Encoding, Origin"], vary_values(headers));
        }
    }

    #[tokio::test]
    async fn refused_response_is_a_no_store_forbidden_without_cors() {
        let response = refused_response(&origin_headers(&[HeaderValue::from_static(
            "https://evil.example",
        )]));
        assert_eq!(StatusCode::FORBIDDEN, response.status());
        let headers = response.headers();
        assert_eq!(
            Some("text/plain; charset=utf-8"),
            header_text(headers, header::CONTENT_TYPE)
        );
        assert_eq!(
            Some("no-store"),
            header_text(headers, header::CACHE_CONTROL)
        );
        assert_eq!(vec!["Accept-Encoding, Origin"], vary_values(headers));
        assert!(!headers.contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN));
        assert_eq!(
            Some(http_headers::MEDIA_CSP),
            header_text(headers, header::CONTENT_SECURITY_POLICY)
        );
        assert_eq!(
            Some(http_headers::STRICT_TRANSPORT_SECURITY),
            header_text(headers, header::STRICT_TRANSPORT_SECURITY)
        );
        let reason = response
            .extensions()
            .get::<ErrorReason>()
            .expect("a refusal carries an error reason")
            .clone();
        assert_eq!("cors_origin_denied", reason.code);
        assert_eq!(
            Some(format!("{:?}", "origin=https://evil.example")),
            reason.source
        );
        assert_eq!(
            b"Forbidden".as_slice(),
            to_bytes(response.into_body(), usize::MAX)
                .await
                .expect("refusal body")
        );
    }

    #[test]
    fn a_long_origin_is_clipped_for_logging() {
        let long = "a".repeat(4096);
        let headers = origin_headers(&[HeaderValue::from_str(&long).expect("ascii origin")]);
        let clipped = "a".repeat(ORIGIN_LOG_BYTES_MAX);
        assert_eq!(clipped, origin_for_log(&headers));

        let captured = CapturedLog::default();
        let subscriber = tracing_subscriber::fmt()
            .with_writer(captured.clone())
            .with_ansi(false)
            .finish();
        {
            let _guard = tracing::subscriber::set_default(subscriber);
            log_would_refuse(&Method::GET, "/avatars/1/hash.png", &headers);
        }
        let line = captured.text();
        assert!(line.contains("reason=\"cors_origin_would_deny\""), "{line}");
        assert!(line.contains("kind=\"asset_image\""), "{line}");
        assert!(line.contains("method=GET"), "{line}");
        assert!(
            line.trim_end().ends_with(&format!("origin={clipped}")),
            "{line}"
        );
        assert!(
            !line.contains(&"a".repeat(ORIGIN_LOG_BYTES_MAX + 1)),
            "{line}"
        );
    }
}
