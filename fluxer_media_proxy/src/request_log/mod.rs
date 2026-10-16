// SPDX-License-Identifier: AGPL-3.0-or-later

mod failure;
mod stage;

pub use failure::ErrorReason;
pub use stage::{Stage, record_stage, timed_stage};

use crate::{
    metrics::{
        self,
        request::{RequestKind, RequestMetrics},
    },
    public_net_policy::external_url_for_log,
};
use axum::{
    extract::{Request, State},
    http::{HeaderMap, Method, StatusCode, header},
    middleware::Next,
    response::Response,
};
use fluxer_common::attachment_url_signature::is_signature_parameter_name;
use rand::RngExt;
use stage::{StageTimingSnapshot, StageTimings};
use std::{future::Future, sync::Arc, time::Instant};
use tracing::{Instrument, Level, event};

const ID_ALPHABET: &[u8] = b"0123456789ABCDEFGHJKMNPQRSTVWXYZ";
const ID_LEN: usize = 12;
const TARGET_LOG_BYTES_MAX: usize = 512;
pub(crate) const HEADER_LOG_BYTES_MAX: usize = 256;
const REDACTED_VALUE: &str = "[redacted]";

#[derive(Clone, Debug)]
pub struct RequestId(pub String);

impl RequestId {
    pub fn generate() -> Self {
        let mut raw: u64 = rand::rng().random();
        let mut id = [0u8; ID_LEN];
        for slot in id.iter_mut().rev() {
            *slot = ID_ALPHABET[(raw & 0x1f) as usize];
            raw >>= 5;
        }
        Self(String::from_utf8(id.to_vec()).expect("alphabet is ASCII"))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

struct RequestObservation {
    id: RequestId,
    method: Method,
    kind: RequestKind,
    path: String,
    query: String,
    referer: Option<String>,
    user_agent: Option<String>,
}

impl RequestObservation {
    fn new(
        id: RequestId,
        method: Method,
        path: &str,
        query: Option<&str>,
        headers: &HeaderMap,
    ) -> Self {
        Self {
            kind: classify_route(path),
            path: clip(path, TARGET_LOG_BYTES_MAX),
            query: query
                .map(|query| clip(&redact_signature_values(query), TARGET_LOG_BYTES_MAX))
                .unwrap_or_default(),
            referer: header_str(headers, header::REFERER)
                .map(|value| clip(&external_url_for_log(value), HEADER_LOG_BYTES_MAX)),
            user_agent: header_str(headers, header::USER_AGENT)
                .map(|value| clip(value, HEADER_LOG_BYTES_MAX)),
            id,
            method,
        }
    }
}

pub fn classify_route(path: &str) -> RequestKind {
    if let Some(kind) = match path {
        "/_health" => Some(RequestKind::Health),
        "/_metrics" => Some(RequestKind::Other),
        "/_metadata" => Some(RequestKind::Metadata),
        "/_sniff" => Some(RequestKind::Sniff),
        "/_thumbnail" => Some(RequestKind::Thumbnail),
        "/_frames" => Some(RequestKind::Frames),
        _ => None,
    } {
        return kind;
    }
    if path.starts_with("/v1/relay/") {
        return RequestKind::Upload;
    }
    if path.starts_with("/external/") {
        return RequestKind::External;
    }
    if path.starts_with("/attachments/") {
        return RequestKind::Attachment;
    }
    if path.starts_with("/themes/") {
        return RequestKind::Themes;
    }
    if path.starts_with("/guilds/") {
        return RequestKind::GuildMemberImage;
    }
    if path.starts_with("/avatars/")
        || path.starts_with("/icons/")
        || path.starts_with("/banners/")
        || path.starts_with("/splashes/")
        || path.starts_with("/embed-splashes/")
        || path.starts_with("/emojis/")
        || path.starts_with("/stickers/")
    {
        return RequestKind::AssetImage;
    }
    RequestKind::Other
}

pub async fn trace(
    State(metrics): State<Arc<RequestMetrics>>,
    mut req: Request,
    next: Next,
) -> Response {
    let id = RequestId::generate();
    let observation = RequestObservation::new(
        id.clone(),
        req.method().clone(),
        req.uri().path(),
        req.uri().query(),
        req.headers(),
    );
    req.extensions_mut().insert(id);
    observe(metrics.as_ref(), observation, next.run(req)).await
}

pub async fn trace_public_request<F>(
    metrics: &RequestMetrics,
    id: RequestId,
    method: Method,
    path_and_query: &str,
    headers: &HeaderMap,
    future: F,
) -> Response
where
    F: Future<Output = Response>,
{
    let (path, query) = match path_and_query.split_once('?') {
        Some((path, query)) => (path, Some(query)),
        None => (path_and_query, None),
    };
    let observation = RequestObservation::new(id, method, path, query, headers);
    observe(metrics, observation, future).await
}

async fn observe<F>(
    metrics: &RequestMetrics,
    observation: RequestObservation,
    future: F,
) -> Response
where
    F: Future<Output = Response>,
{
    let RequestObservation {
        id,
        method,
        kind,
        path,
        query,
        referer,
        user_agent,
    } = observation;
    let stages = Arc::new(StageTimings::default());
    let started = Instant::now();
    let span = tracing::info_span!("request", req = %id.as_str(), path = %path);
    let response = stage::scope(Arc::clone(&stages), future)
        .instrument(span)
        .await;
    let elapsed_ms = metrics::duration_millis(started.elapsed());
    let StageTimingSnapshot {
        fetch_ms,
        transform_ms,
        nsfw_ms,
    } = stages.snapshot();
    let status = response.status();
    let reason = response.extensions().get::<ErrorReason>().cloned();

    metrics.record_request_with_duration(kind, status.as_u16(), elapsed_ms);

    if status.is_success() || status.is_redirection() {
        if !matches!(kind, RequestKind::Health | RequestKind::Other) {
            event!(
                Level::INFO,
                req = %id.as_str(),
                kind = kind.label(),
                method = %method,
                path = %path,
                query = %query,
                status = status.as_u16(),
                duration_ms = elapsed_ms,
                fetch_ms,
                transform_ms,
                nsfw_ms,
                "request"
            );
        }
        return response;
    }

    let level = if status.is_server_error() {
        Level::ERROR
    } else {
        Level::WARN
    };
    let (code, source) = log_reason(reason, status);

    match level {
        Level::ERROR => event!(
            Level::ERROR,
            req = %id.as_str(),
            kind = kind.label(),
            method = %method,
            path = %path,
            query = %query,
            status = status.as_u16(),
            duration_ms = elapsed_ms,
            fetch_ms,
            transform_ms,
            nsfw_ms,
            reason = code,
            source = %source,
            referer = referer.as_deref().unwrap_or(""),
            user_agent = user_agent.as_deref().unwrap_or(""),
            "request failed"
        ),
        _ => event!(
            Level::WARN,
            req = %id.as_str(),
            kind = kind.label(),
            method = %method,
            path = %path,
            query = %query,
            status = status.as_u16(),
            duration_ms = elapsed_ms,
            fetch_ms,
            transform_ms,
            nsfw_ms,
            reason = code,
            source = %source,
            "request rejected"
        ),
    }
    response
}

fn header_str(headers: &HeaderMap, name: header::HeaderName) -> Option<&str> {
    headers.get(name).and_then(|value| value.to_str().ok())
}

fn log_reason(reason: Option<ErrorReason>, status: StatusCode) -> (&'static str, String) {
    match reason {
        Some(reason) => (
            reason.code,
            reason
                .source
                .map(|source| clip(&source, TARGET_LOG_BYTES_MAX))
                .unwrap_or_default(),
        ),
        None => (default_reason(status), String::new()),
    }
}

fn redact_signature_values(query: &str) -> String {
    if !query.contains('=') {
        return query.to_owned();
    }
    let mut out = String::with_capacity(query.len());
    for (index, field) in query.split('&').enumerate() {
        if index > 0 {
            out.push('&');
        }
        match field.split_once('=') {
            Some((name, _)) if is_signature_parameter_name(name) => {
                out.push_str(name);
                out.push('=');
                out.push_str(REDACTED_VALUE);
            }
            _ => out.push_str(field),
        }
    }
    out
}

pub(crate) fn clip(value: &str, max: usize) -> String {
    if value.len() <= max {
        return value.to_owned();
    }
    let mut end = max;
    while !value.is_char_boundary(end) {
        end -= 1;
    }
    let mut out = value[..end].to_owned();
    out.push('~');
    out
}

fn default_reason(status: StatusCode) -> &'static str {
    status.canonical_reason().unwrap_or("error")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn observation_with_headers(headers: HeaderMap) -> RequestObservation {
        RequestObservation::new(
            RequestId::generate(),
            Method::GET,
            "/attachments/1/2/a.png",
            Some("size=128"),
            &headers,
        )
    }

    #[test]
    fn clip_never_splits_a_multibyte_character() {
        let key = "\u{597d}".repeat(8);
        assert_eq!(24, key.len());
        assert_eq!("\u{597d}\u{597d}\u{597d}~", clip(&key, 10));
        assert_eq!("\u{597d}\u{597d}\u{597d}\u{597d}~", clip(&key, 12));
        assert_eq!(key, clip(&key, 24));
    }

    #[test]
    fn a_long_error_source_is_clipped_to_the_target_bound() {
        let long = ErrorReason::with_message("storage_error", "k".repeat(4096));
        let (code, source) = log_reason(Some(long), StatusCode::NOT_FOUND);
        assert_eq!("storage_error", code);
        assert_eq!(TARGET_LOG_BYTES_MAX + 1, source.len());
        assert!(source.ends_with('~'));
        assert_eq!(
            ("Not Found", String::new()),
            log_reason(None, StatusCode::NOT_FOUND)
        );
        assert_eq!(
            ("storage_error", "key=a".to_owned()),
            log_reason(
                Some(ErrorReason::with_message("storage_error", "key=a")),
                StatusCode::NOT_FOUND
            )
        );
    }

    #[test]
    fn a_live_signature_never_reaches_the_log() {
        let signature = "f84dea2dbe168641c69753d838f270806583fedfe5c86f3f7a9a80f93af7ad28";
        for query in [
            format!("ex=6a9a7231&is=6a9920b1&hm={signature}"),
            format!("ex=6a9a7231&is=6a9920b1&hm={signature}&"),
            format!("width=64&ex=6a9a7231&is=6a9920b1&hm={signature}"),
            format!("%65x=6a9a7231&is=6a9920b1&hm={signature}&"),
            format!("hm={signature}&hm={signature}"),
            format!("ex=0&is=6a9920b1&hm={signature}&uc=dp"),
            format!("ex=0&is=6a9920b1&hm={signature}&uc=dp&"),
            format!("width=64&%75c=dp&hm={signature}&ex=6a9a7231&is=6a9920b1"),
        ] {
            let observation = RequestObservation::new(
                RequestId::generate(),
                Method::GET,
                "/attachments/1/2/a.png",
                Some(&query),
                &HeaderMap::new(),
            );
            assert!(!observation.query.contains(signature), "{query}");
            assert!(!observation.query.contains("6a9a7231"), "{query}");
            assert!(!observation.query.contains("6a9920b1"), "{query}");
            assert_eq!(
                query.matches('&').count(),
                observation.query.matches('&').count(),
                "{query}"
            );
        }
    }

    #[test]
    fn redaction_leaves_every_other_parameter_byte_for_byte() {
        for query in [
            "",
            "size=128",
            "width=64&height=64&format=webp",
            "download=1",
            "hm",
            "uc",
            "a=b&&c=d",
            "ucx=dp&dp=1",
        ] {
            assert_eq!(query, redact_signature_values(query), "{query}");
        }
        assert_eq!(
            "width=64&ex=[redacted]&is=[redacted]&hm=[redacted]&download=1",
            redact_signature_values("width=64&ex=6a9a7231&is=6a9920b1&hm=abc&download=1")
        );
        assert_eq!(
            "ex=[redacted]&is=[redacted]&hm=[redacted]&uc=[redacted]&&width=64",
            redact_signature_values("ex=0&is=6a9920b1&hm=abc&uc=dp&&width=64")
        );
        assert_eq!(
            "ex=[redacted]&is=[redacted]&hm=[redacted]",
            redact_signature_values("ex=6a9a7231&is=6a9920b1&hm=abc")
        );
        assert_eq!(
            "ex=[redacted]&is=[redacted]&hm=[redacted]&uc=[redacted]",
            redact_signature_values("ex=0&is=6a9920b1&hm=abc&uc=dp")
        );
    }

    #[test]
    fn credentialed_referer_is_redacted_before_it_reaches_the_log() {
        let mut headers = HeaderMap::new();
        headers.insert(
            header::REFERER,
            "https://agent:hunter2@cdn.example.com/rooms/private?token=abc#fragment"
                .parse()
                .unwrap(),
        );
        headers.insert(header::USER_AGENT, "curl/8.0".parse().unwrap());
        let observation = observation_with_headers(headers);
        assert_eq!(
            Some("https://cdn.example.com/[redacted]".to_owned()),
            observation.referer
        );
        assert_eq!(Some("curl/8.0".to_owned()), observation.user_agent);
    }

    #[test]
    fn unparsable_referer_collapses_to_a_marker_and_paths_stay_raw() {
        let mut headers = HeaderMap::new();
        headers.insert(header::REFERER, "not a url".parse().unwrap());
        let observation = observation_with_headers(headers);
        assert_eq!(Some("[invalid-url]".to_owned()), observation.referer);
        assert_eq!("/attachments/1/2/a.png", observation.path);
        assert_eq!("size=128", observation.query);
        assert_eq!(RequestKind::Attachment, observation.kind);
    }
}
