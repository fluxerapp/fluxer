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
use rand::RngExt;
use stage::{StageTimingSnapshot, StageTimings};
use std::{future::Future, sync::Arc, time::Instant};
use tracing::{Level, event};

const ID_ALPHABET: &[u8] = b"0123456789ABCDEFGHJKMNPQRSTVWXYZ";
const ID_LEN: usize = 12;
const TARGET_LOG_BYTES_MAX: usize = 512;
const HEADER_LOG_BYTES_MAX: usize = 256;

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
                .map(|query| clip(query, TARGET_LOG_BYTES_MAX))
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
    let response = stage::scope(Arc::clone(&stages), future).await;
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

fn clip(value: &str, max: usize) -> String {
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
    use crate::metrics::Metrics;
    use axum::{Router, body::Body, middleware, response::IntoResponse, routing::get};
    use tower::ServiceExt;

    fn req_id_is_alphabet_only(id: &str) -> bool {
        id.len() == ID_LEN && id.bytes().all(|c| ID_ALPHABET.contains(&c))
    }

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
    fn request_id_is_stable_length_and_alphabet() {
        let id = RequestId::generate();
        assert!(req_id_is_alphabet_only(id.as_str()));
    }

    #[test]
    fn two_back_to_back_ids_differ() {
        assert_ne!(RequestId::generate().0, RequestId::generate().0);
    }

    #[test]
    fn classify_route_buckets_known_prefixes() {
        assert_eq!(RequestKind::Health, classify_route("/_health"));
        assert_eq!(RequestKind::Metadata, classify_route("/_metadata"));
        assert_eq!(RequestKind::Upload, classify_route("/v1/relay/abc"));
        assert_eq!(RequestKind::External, classify_route("/external/x/y"));
        assert_eq!(RequestKind::Attachment, classify_route("/attachments/a/b"));
        assert_eq!(RequestKind::Themes, classify_route("/themes/x.css"));
        assert_eq!(
            RequestKind::GuildMemberImage,
            classify_route("/guilds/1/users/2/avatars/h.png")
        );
        assert_eq!(RequestKind::AssetImage, classify_route("/emojis/1.png"));
        assert_eq!(RequestKind::AssetImage, classify_route("/icons/1/h.png"));
        assert_eq!(RequestKind::Other, classify_route("/unknown"));
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
    fn clip_truncates_with_marker() {
        assert_eq!("abc", clip("abc", 8));
        assert_eq!("abcdefgh~", clip("abcdefghIJ", 8));
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

    #[tokio::test]
    async fn middleware_inserts_request_id_into_extensions() {
        async fn handler(req: Request) -> impl IntoResponse {
            let id = req
                .extensions()
                .get::<RequestId>()
                .expect("middleware must inject RequestId")
                .clone();
            id.0
        }
        let app = Router::new()
            .route("/", get(handler))
            .layer(middleware::from_fn_with_state(
                Arc::new(RequestMetrics::new()),
                trace,
            ));
        let resp = app
            .oneshot(
                axum::http::Request::builder()
                    .uri("/")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(StatusCode::OK, resp.status());
        let bytes = axum::body::to_bytes(resp.into_body(), 64).await.unwrap();
        let body = std::str::from_utf8(&bytes).unwrap();
        assert!(req_id_is_alphabet_only(body));
    }

    #[tokio::test]
    async fn middleware_records_metrics_and_reads_error_reason() {
        async fn handler() -> Response {
            let mut resp = Response::new(Body::from("boom"));
            *resp.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
            resp.extensions_mut()
                .insert(ErrorReason::with_message("transcode_failed", "vips OOM"));
            resp
        }
        let metrics = Metrics::new();
        let app = Router::new()
            .route("/", get(handler))
            .layer(middleware::from_fn_with_state(metrics.request(), trace));
        let resp = app
            .oneshot(
                axum::http::Request::builder()
                    .uri("/")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(StatusCode::INTERNAL_SERVER_ERROR, resp.status());
        let reason = resp.extensions().get::<ErrorReason>().unwrap();
        assert_eq!("transcode_failed", reason.code);
        assert_eq!(Some("vips OOM".to_owned()), reason.source);
        let rendered = metrics.render();
        assert!(rendered.contains("fluxer_media_proxy_requests_5xx_total{kind=\"other\"} 1\n"));
        assert!(rendered.contains("fluxer_media_proxy_request_duration_ms_count 1\n"));
    }

    #[tokio::test]
    async fn trace_public_request_observes_a_non_axum_request() {
        let metrics = Metrics::new();
        let response = trace_public_request(
            metrics.request().as_ref(),
            RequestId::generate(),
            Method::GET,
            "/attachments/1/2/a.png?size=128",
            &HeaderMap::new(),
            async {
                record_stage(Stage::Fetch, 3);
                record_stage(Stage::Transform, 5);
                record_stage(Stage::Nsfw, 7);
                Response::new(Body::empty())
            },
        )
        .await;
        assert_eq!(StatusCode::OK, response.status());
        let rendered = metrics.render();
        assert!(
            rendered.contains("fluxer_media_proxy_requests_2xx_total{kind=\"attachment\"} 1\n")
        );
    }
}
