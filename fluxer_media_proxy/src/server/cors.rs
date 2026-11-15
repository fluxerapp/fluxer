// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    constants::DESKTOP_APP_ORIGIN, http_headers, request_log,
    server::response::error::text_with_source,
};
use axum::{
    body::Body,
    http::{HeaderMap, HeaderValue, Method, StatusCode, header},
    response::Response,
};

const VARY_ON_ORIGIN: &str = "Accept-Encoding, Origin";
const VARY_ON_PREFLIGHT: &str =
    "Origin, Access-Control-Request-Method, Access-Control-Request-Headers";
const PREFLIGHT_ALLOW_METHODS: &str = "GET, HEAD, OPTIONS";
const PREFLIGHT_ALLOW_HEADERS: &str = "Range";
const PREFLIGHT_MAX_AGE_SECONDS: &str = "600";
const ANY_ORIGIN: &str = "*";
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
    if origin.as_bytes() == DESKTOP_APP_ORIGIN.as_bytes() {
        return OriginCheck::Allowed(HeaderValue::from_static(DESKTOP_APP_ORIGIN));
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

pub(in crate::server) fn is_preflight(method: &Method, request: &HeaderMap) -> bool {
    method == Method::OPTIONS
        && request.contains_key(header::ORIGIN)
        && request.contains_key(header::ACCESS_CONTROL_REQUEST_METHOD)
}

pub(in crate::server) fn preflight_response(check: &OriginCheck) -> Response {
    let mut response = Response::new(Body::empty());
    *response.status_mut() = StatusCode::NO_CONTENT;
    let headers = response.headers_mut();
    http_headers::add_security_headers(headers);
    let allow_origin = match check {
        OriginCheck::Allowed(origin) => origin.clone(),
        OriginCheck::Absent | OriginCheck::Refused => HeaderValue::from_static(ANY_ORIGIN),
    };
    headers.insert(header::ACCESS_CONTROL_ALLOW_ORIGIN, allow_origin);
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_METHODS,
        HeaderValue::from_static(PREFLIGHT_ALLOW_METHODS),
    );
    headers.insert(
        header::ACCESS_CONTROL_ALLOW_HEADERS,
        HeaderValue::from_static(PREFLIGHT_ALLOW_HEADERS),
    );
    headers.insert(
        header::ACCESS_CONTROL_MAX_AGE,
        HeaderValue::from_static(PREFLIGHT_MAX_AGE_SECONDS),
    );
    headers.insert(header::VARY, HeaderValue::from_static(VARY_ON_PREFLIGHT));
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
    use crate::test_fixtures::ADVERSARIAL_TEXT_INPUTS;

    const WEB: &str = "https://web.fluxer.app";
    const CANARY: &str = "https://web.canary.fluxer.app";

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
}
