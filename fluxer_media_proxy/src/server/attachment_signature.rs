// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::AttachmentSignatureConfig,
    metrics::now_ms,
    request_log::{self, HEADER_LOG_BYTES_MAX, RequestId, clip},
    server::response::error::text_with_source,
};
use axum::{
    http::{HeaderMap, HeaderValue, Method, StatusCode, header},
    response::Response,
};
use fluxer_common::attachment_url_signature::{
    ATTACHMENT_URL_TTL_SECS, Verdict, Verification, decode_key, verify,
};
use std::{
    sync::atomic::{AtomicI64, AtomicU64, Ordering},
    time::{SystemTime, UNIX_EPOCH},
};

const UNAVAILABLE_BODY: &str = "This content is no longer available.";
const ATTACHMENT_PATH: &str = "/attachments/";
const WOULD_DENY_LOG_WINDOW_MS: i64 = 1_000;
const WOULD_DENY_LOG_LINES_PER_WINDOW: u64 = 20;

pub(in crate::server) struct WouldDenyLog {
    window_start_ms: AtomicI64,
    logged_in_window: AtomicU64,
    suppressed: AtomicU64,
}

impl WouldDenyLog {
    pub(in crate::server) fn new() -> Self {
        Self {
            window_start_ms: AtomicI64::new(i64::MIN),
            logged_in_window: AtomicU64::new(0),
            suppressed: AtomicU64::new(0),
        }
    }

    fn admit(&self, now_ms: i64) -> Option<u64> {
        if now_ms.saturating_sub(self.window_start_ms.load(Ordering::Relaxed))
            >= WOULD_DENY_LOG_WINDOW_MS
        {
            self.window_start_ms.store(now_ms, Ordering::Relaxed);
            self.logged_in_window.store(0, Ordering::Relaxed);
        }
        if self.logged_in_window.fetch_add(1, Ordering::Relaxed) >= WOULD_DENY_LOG_LINES_PER_WINDOW
        {
            self.suppressed.fetch_add(1, Ordering::Relaxed);
            return None;
        }
        Some(self.suppressed.swap(0, Ordering::Relaxed))
    }
}

pub(in crate::server) fn check(
    cfg: &AttachmentSignatureConfig,
    path: &str,
    raw_query: Option<&str>,
) -> Verification {
    let Some(key) = decode_key(path) else {
        return Verification {
            verdict: Verdict::Malformed,
            remaining_secs: None,
        };
    };
    verify(&key, raw_query, &cfg.secrets(), now_secs())
}

pub(in crate::server) fn limit_cache_lifetime(response: &mut Response, verification: Verification) {
    if verification.verdict != Verdict::Valid
        || !matches!(
            response.status(),
            StatusCode::OK | StatusCode::PARTIAL_CONTENT
        )
    {
        return;
    }
    let headers = response.headers_mut();
    if headers
        .get(header::CACHE_CONTROL)
        .is_some_and(|value| value == "no-store")
    {
        return;
    }
    let max_age = verification
        .remaining_secs
        .unwrap_or(ATTACHMENT_URL_TTL_SECS);
    let no_transform = headers
        .get(header::CACHE_CONTROL)
        .and_then(|value| value.to_str().ok())
        .is_some_and(|value| value.contains("no-transform"));
    let shared = format!("public, max-age={max_age}");
    let browser = if no_transform {
        format!("{shared}, no-transform")
    } else {
        shared.clone()
    };
    if let (Ok(browser), Ok(shared)) = (
        HeaderValue::from_str(&browser),
        HeaderValue::from_str(&shared),
    ) {
        headers.insert(header::CACHE_CONTROL, browser);
        headers.insert("CDN-Cache-Control", shared);
    }
}

pub(in crate::server) fn denied_response(verdict: Verdict) -> Response {
    text_with_source(
        StatusCode::NOT_FOUND,
        UNAVAILABLE_BODY,
        refusal_code(verdict),
        verdict,
    )
}

pub(in crate::server) fn masked_not_found_response(key: &str) -> Response {
    text_with_source(
        StatusCode::NOT_FOUND,
        UNAVAILABLE_BODY,
        "attachment_not_found",
        format!("key={key}"),
    )
}

pub(in crate::server) fn log_would_deny(
    budget: &WouldDenyLog,
    method: &Method,
    verdict: Verdict,
    headers: &HeaderMap,
    request_id: Option<&RequestId>,
) {
    let Some(suppressed) = budget.admit(now_ms()) else {
        return;
    };
    tracing::warn!(
        reason = "attachment_signature_would_deny",
        kind = request_log::classify_route(ATTACHMENT_PATH).label(),
        method = %method,
        verdict = verdict.label(),
        req = request_id.map(RequestId::as_str).unwrap_or(""),
        suppressed,
        user_agent = %user_agent_for_log(headers),
        "attachment read would be refused"
    );
}

fn refusal_code(verdict: Verdict) -> &'static str {
    match verdict {
        Verdict::Missing => "attachment_signature_missing",
        Verdict::Malformed => "attachment_signature_malformed",
        Verdict::Mismatch => "attachment_signature_mismatch",
        Verdict::Expired => "attachment_signature_expired",
        Verdict::Valid => "attachment_signature_valid",
    }
}

fn user_agent_for_log(headers: &HeaderMap) -> String {
    headers
        .get(header::USER_AGENT)
        .and_then(|value| value.to_str().ok())
        .map(|value| clip(value, HEADER_LOG_BYTES_MAX))
        .unwrap_or_default()
}

fn now_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{config::PolicyMode, http_headers, request_log::ErrorReason};
    use axum::body::to_bytes;
    use base64::{Engine as _, engine::general_purpose};
    use fluxer_common::attachment_url_signature::{UrlKind, issue_window, sign};

    const SECRET: [u8; 32] = [3u8; 32];
    const KEY: &str = "attachments/1/2/cat.gif";
    const PATH: &str = "/attachments/1/2/cat.gif";

    fn enforcing_config() -> AttachmentSignatureConfig {
        crate::config::Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE", "enforce"),
            (
                "FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64",
                general_purpose::STANDARD.encode(SECRET).as_str(),
            ),
        ])
        .expect("signature config")
        .attachment_signature
    }

    fn check_verdict(
        cfg: &AttachmentSignatureConfig,
        path: &str,
        raw_query: Option<&str>,
    ) -> Verdict {
        check(cfg, path, raw_query).verdict
    }

    fn signed_query(key: &str, now: u64) -> String {
        let (issued, expires) = issue_window(now, now);
        format!(
            "ex={expires:08x}&is={issued:08x}&hm={}",
            sign(key, expires, issued, UrlKind::Ordinary, &SECRET)
        )
    }

    #[test]
    fn the_check_reads_the_raw_query_against_the_decoded_key() {
        let cfg = enforcing_config();
        assert_eq!(PolicyMode::Enforce, cfg.mode);
        let now = now_secs();
        let query = signed_query(KEY, now);
        assert_eq!(Verdict::Valid, check_verdict(&cfg, PATH, Some(&query)));
        assert_eq!(Verdict::Missing, check_verdict(&cfg, PATH, None));
        assert_eq!(
            Verdict::Mismatch,
            check_verdict(&cfg, "/attachments/1/2/dog.gif", Some(&query))
        );
        assert_eq!(
            Verdict::Malformed,
            check_verdict(&cfg, "/attachments/1/2/%FF.gif", Some(&query))
        );
        assert_eq!(
            Verdict::Valid,
            check_verdict(&cfg, PATH, Some(&format!("{query}&")))
        );
        let (issued, _) = issue_window(1, 1);
        let package = format!(
            "ex=0&is={issued:08x}&hm={}&uc=dp",
            sign(KEY, 0, issued, UrlKind::DataPackage, &SECRET)
        );
        assert_eq!(Verdict::Valid, check_verdict(&cfg, PATH, Some(&package)));
        assert_eq!(
            Verdict::Valid,
            check_verdict(&cfg, PATH, Some(&format!("{package}&")))
        );
        assert_eq!(
            Verdict::Malformed,
            check_verdict(&cfg, PATH, Some(&package.replace("&uc=dp", "")))
        );
        assert_eq!(
            Verdict::Mismatch,
            check_verdict(&cfg, "/attachments/1/2/dog.gif", Some(&package))
        );
    }

    #[tokio::test]
    async fn every_refusal_is_the_same_thirty_six_byte_not_found() {
        let mut bodies = Vec::new();
        for verdict in [
            Verdict::Missing,
            Verdict::Malformed,
            Verdict::Mismatch,
            Verdict::Expired,
        ] {
            let response = denied_response(verdict);
            assert_eq!(StatusCode::NOT_FOUND, response.status());
            let headers = response.headers().clone();
            assert_eq!(
                "text/plain; charset=utf-8",
                headers.get(header::CONTENT_TYPE).expect("content type")
            );
            assert_eq!(
                "no-store",
                headers.get(header::CACHE_CONTROL).expect("cache control")
            );
            assert_eq!(
                "nosniff",
                headers
                    .get(header::X_CONTENT_TYPE_OPTIONS)
                    .expect("nosniff")
            );
            assert_eq!(
                http_headers::MEDIA_CSP,
                headers.get(header::CONTENT_SECURITY_POLICY).expect("csp")
            );
            assert_eq!(
                http_headers::STRICT_TRANSPORT_SECURITY,
                headers
                    .get(header::STRICT_TRANSPORT_SECURITY)
                    .expect("hsts")
            );
            assert!(!headers.contains_key(header::ACCESS_CONTROL_ALLOW_ORIGIN));
            let reason = response
                .extensions()
                .get::<ErrorReason>()
                .expect("a refusal has an error reason")
                .clone();
            assert_eq!(refusal_code(verdict), reason.code);
            assert!(reason.code.starts_with("attachment_signature_"));
            bodies.push(
                to_bytes(response.into_body(), usize::MAX)
                    .await
                    .expect("body"),
            );
        }
        bodies.push(
            to_bytes(masked_not_found_response(KEY).into_body(), usize::MAX)
                .await
                .expect("body"),
        );
        for body in &bodies {
            assert_eq!(36, body.len());
            assert_eq!(UNAVAILABLE_BODY.as_bytes(), body.as_ref());
            assert_eq!(bodies[0], *body);
        }
    }
}
