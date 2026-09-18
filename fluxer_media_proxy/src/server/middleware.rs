// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{config::DeploymentMode, http_headers};
use axum::{
    body::Body,
    extract::State,
    http::{HeaderValue, Request},
    response::Response,
};
use bytes::Bytes;
use http_body::{Body as HttpBody, Frame, SizeHint};
use std::{
    pin::Pin,
    sync::{
        Arc, OnceLock,
        atomic::{AtomicU64, Ordering},
    },
    task::{Context, Poll},
};
use tokio::sync::Notify;

#[derive(Clone, Default)]
pub(in crate::server) struct HttpRequestDrain {
    inner: Arc<HttpRequestDrainInner>,
}

#[derive(Default)]
struct HttpRequestDrainInner {
    active_requests: AtomicU64,
    drained: Notify,
}

struct ActiveHttpRequest {
    drain: HttpRequestDrain,
}

struct DrainedBody {
    body: Pin<Box<Body>>,
    _active: ActiveHttpRequest,
}

impl HttpRequestDrain {
    pub(in crate::server) fn new() -> Self {
        Self::default()
    }

    pub(in crate::server) fn active_requests(&self) -> u64 {
        self.inner.active_requests.load(Ordering::Relaxed)
    }

    pub(in crate::server) async fn wait_for_requests_drained(&self) {
        loop {
            let drained = self.inner.drained.notified();
            tokio::pin!(drained);
            drained.as_mut().enable();
            if self.inner.active_requests.load(Ordering::Acquire) == 0 {
                return;
            }
            drained.await;
        }
    }

    fn begin_request(&self) -> ActiveHttpRequest {
        self.inner
            .active_requests
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |active| {
                active.checked_add(1)
            })
            .expect("http active request count must not overflow");
        ActiveHttpRequest {
            drain: self.clone(),
        }
    }
}

impl Drop for ActiveHttpRequest {
    fn drop(&mut self) {
        let previous = self
            .drain
            .inner
            .active_requests
            .fetch_sub(1, Ordering::AcqRel);
        assert!(previous > 0, "http active request count must stay positive");
        if previous == 1 {
            self.drain.inner.drained.notify_waiters();
        }
    }
}

impl HttpBody for DrainedBody {
    type Data = Bytes;
    type Error = axum::Error;

    fn poll_frame(
        mut self: Pin<&mut Self>,
        context: &mut Context<'_>,
    ) -> Poll<Option<Result<Frame<Self::Data>, Self::Error>>> {
        self.body.as_mut().poll_frame(context)
    }

    fn is_end_stream(&self) -> bool {
        self.body.is_end_stream()
    }

    fn size_hint(&self) -> SizeHint {
        self.body.size_hint()
    }
}

pub(in crate::server) async fn track_active_request(
    State(drain): State<HttpRequestDrain>,
    request: Request<Body>,
    next: axum::middleware::Next,
) -> Response {
    let active = drain.begin_request();
    next.run(request).await.map(|body| {
        Body::new(DrainedBody {
            body: Box::pin(body),
            _active: active,
        })
    })
}

pub(in crate::server) fn build_version() -> &'static str {
    static BUILD_VERSION: OnceLock<String> = OnceLock::new();
    BUILD_VERSION
        .get_or_init(|| {
            std::env::var("BUILD_VERSION")
                .ok()
                .filter(|value| !value.trim().is_empty())
                .unwrap_or_else(|| "dev".to_owned())
        })
        .as_str()
}

pub(in crate::server) async fn add_version_header(
    request: Request<Body>,
    next: axum::middleware::Next,
) -> Response {
    let mut response = next.run(request).await;
    if let Ok(value) = HeaderValue::from_str(build_version()) {
        response.headers_mut().insert("x-fluxer-version", value);
    }
    response
}

pub(in crate::server) async fn add_security_header_middleware(
    State(mode): State<DeploymentMode>,
    request: Request<Body>,
    next: axum::middleware::Next,
) -> Response {
    let mut response = next.run(request).await;
    let status = response.status();
    let headers = response.headers_mut();
    http_headers::add_security_headers(headers);
    if mode == DeploymentMode::Static {
        headers.remove("X-Robots-Tag");
    } else {
        http_headers::neutralize_script_content_type(status, headers);
    }
    response
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        server::{routes::dispatch::catch_all, state::AppState},
        storage::tests::{FakeObject, fake_s3},
    };
    use axum::{
        Router,
        http::{HeaderMap, HeaderName, Method, StatusCode, header},
        routing::get,
    };

    const INERT: &str = "text/plain; charset=utf-8";

    const BROWSER_PARSED_JAVASCRIPT: [&str; 7] = [
        "text/javascript x",
        "text/javascript\tx",
        "text/javascript(x)",
        "TEXT/JAVASCRIPT (x)",
        "video/mp4, text/javascript x",
        " text/javascript ;charset=x",
        "image/png,text/javascript(x)",
    ];

    async fn headers_for(mode: DeploymentMode, content_type: Option<&'static str>) -> HeaderMap {
        headers_with_status(mode, StatusCode::OK, content_type).await
    }

    async fn headers_with_status(
        mode: DeploymentMode,
        status: StatusCode,
        content_type: Option<&'static str>,
    ) -> HeaderMap {
        let router = Router::new()
            .route(
                "/probe",
                get(move || async move {
                    let mut response = Response::new(Body::empty());
                    *response.status_mut() = status;
                    if let Some(content_type) = content_type {
                        response
                            .headers_mut()
                            .insert(header::CONTENT_TYPE, HeaderValue::from_static(content_type));
                    }
                    response
                }),
            )
            .layer(axum::middleware::from_fn_with_state(
                mode,
                add_security_header_middleware,
            ));
        let response = tower::ServiceExt::oneshot(
            router,
            Request::builder()
                .uri("/probe")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
        response.headers().clone()
    }

    fn header_text(headers: &HeaderMap, name: HeaderName) -> Option<&str> {
        headers.get(name).map(|value| value.to_str().unwrap())
    }

    #[tokio::test]
    async fn mp_mode_serves_javascript_as_plain_text() {
        for content_type in [
            "text/javascript",
            "application/javascript; charset=utf-8",
            "image/png, text/javascript",
        ] {
            let headers = headers_for(DeploymentMode::Mp, Some(content_type)).await;
            assert_eq!(
                Some(INERT),
                header_text(&headers, header::CONTENT_TYPE),
                "{content_type}"
            );
            assert_eq!(
                Some("nosniff"),
                header_text(&headers, header::X_CONTENT_TYPE_OPTIONS),
                "{content_type}"
            );
        }
    }

    #[tokio::test]
    async fn mp_mode_serves_javascript_that_browsers_parse_past_junk_as_plain_text() {
        for content_type in BROWSER_PARSED_JAVASCRIPT {
            for status in [StatusCode::OK, StatusCode::PARTIAL_CONTENT] {
                let headers =
                    headers_with_status(DeploymentMode::Mp, status, Some(content_type)).await;
                assert_eq!(
                    Some(INERT),
                    header_text(&headers, header::CONTENT_TYPE),
                    "{status} {content_type:?}"
                );
                assert_eq!(1, headers.get_all(header::CONTENT_TYPE).iter().count());
            }
        }
    }

    #[tokio::test]
    async fn static_mode_serves_javascript_unchanged() {
        for content_type in ["text/javascript", "application/javascript; charset=utf-8"]
            .into_iter()
            .chain(BROWSER_PARSED_JAVASCRIPT)
        {
            let headers = headers_for(DeploymentMode::Static, Some(content_type)).await;
            assert_eq!(
                Some(content_type),
                header_text(&headers, header::CONTENT_TYPE),
                "{content_type:?}"
            );
        }
    }

    #[tokio::test]
    async fn every_mode_that_serves_stored_bytes_neutralizes_javascript() {
        for mode in [
            DeploymentMode::Mp,
            DeploymentMode::Upload,
            DeploymentMode::Relay,
        ] {
            for content_type in ["text/javascript", "application/javascript; charset=utf-8"]
                .into_iter()
                .chain(BROWSER_PARSED_JAVASCRIPT)
            {
                let headers = headers_for(mode, Some(content_type)).await;
                assert_eq!(
                    Some(INERT),
                    header_text(&headers, header::CONTENT_TYPE),
                    "{mode:?} {content_type:?}"
                );
            }
        }
    }

    #[tokio::test]
    async fn non_javascript_content_types_pass_through_every_mode() {
        for mode in [
            DeploymentMode::Mp,
            DeploymentMode::Static,
            DeploymentMode::Upload,
            DeploymentMode::Relay,
        ] {
            for content_type in [
                "text/css; charset=utf-8",
                "text/css",
                "video/mp2t",
                "video/mp4",
                "image/png",
                "image/svg+xml",
                "application/json",
                "text/plain",
                "text/javascriptx",
                "text/javascript1.6",
            ] {
                let headers = headers_for(mode, Some(content_type)).await;
                assert_eq!(
                    Some(content_type),
                    header_text(&headers, header::CONTENT_TYPE),
                    "{mode:?} {content_type}"
                );
            }
        }
    }

    #[tokio::test]
    async fn mp_mode_types_an_untyped_full_or_partial_response_as_plain_text() {
        for status in [StatusCode::OK, StatusCode::PARTIAL_CONTENT] {
            for content_type in [None, Some(""), Some("  ")] {
                let headers = headers_with_status(DeploymentMode::Mp, status, content_type).await;
                assert_eq!(
                    Some(INERT),
                    header_text(&headers, header::CONTENT_TYPE),
                    "{status} {content_type:?}"
                );
                assert_eq!(
                    Some("nosniff"),
                    header_text(&headers, header::X_CONTENT_TYPE_OPTIONS),
                    "{status} {content_type:?}"
                );
            }
        }
    }

    #[tokio::test]
    async fn untyped_responses_of_other_statuses_or_modes_stay_untyped() {
        for status in [
            StatusCode::NO_CONTENT,
            StatusCode::NOT_MODIFIED,
            StatusCode::NOT_FOUND,
            StatusCode::METHOD_NOT_ALLOWED,
            StatusCode::RANGE_NOT_SATISFIABLE,
        ] {
            let headers = headers_with_status(DeploymentMode::Mp, status, None).await;
            assert_eq!(None, headers.get(header::CONTENT_TYPE), "{status}");
        }
        for status in [
            StatusCode::OK,
            StatusCode::PARTIAL_CONTENT,
            StatusCode::RANGE_NOT_SATISFIABLE,
        ] {
            let headers = headers_with_status(DeploymentMode::Static, status, None).await;
            assert_eq!(None, headers.get(header::CONTENT_TYPE), "{status}");
        }
        for mode in [DeploymentMode::Upload, DeploymentMode::Relay] {
            let headers = headers_with_status(mode, StatusCode::RANGE_NOT_SATISFIABLE, None).await;
            assert_eq!(None, headers.get(header::CONTENT_TYPE), "{mode:?}");
            for status in [StatusCode::OK, StatusCode::PARTIAL_CONTENT] {
                let headers = headers_with_status(mode, status, None).await;
                assert_eq!(
                    Some(INERT),
                    header_text(&headers, header::CONTENT_TYPE),
                    "{mode:?} {status}"
                );
            }
        }
    }

    fn transport_stream_object(content_type: &str) -> FakeObject {
        let mut body = vec![0_u8; 376];
        body[0] = 0x47;
        body[188] = 0x47;
        FakeObject {
            body,
            head_length: Some(376),
            content_type: Some(content_type.to_owned()),
            ..FakeObject::default()
        }
    }

    fn security_layered_router(app: &Arc<AppState>, mode: DeploymentMode) -> Router {
        Router::new()
            .fallback(axum::routing::any(catch_all))
            .layer(axum::middleware::from_fn_with_state(
                mode,
                add_security_header_middleware,
            ))
            .with_state(Arc::clone(app))
    }

    async fn send(
        router: &Router,
        method: Method,
        uri: &str,
        range: Option<&'static str>,
    ) -> Response {
        let mut request = Request::builder().method(method).uri(uri);
        if let Some(range) = range {
            request = request.header(header::RANGE, range);
        }
        tower::ServiceExt::oneshot(router.clone(), request.body(Body::empty()).unwrap())
            .await
            .unwrap()
    }

    fn assert_attachment_named(headers: &HeaderMap, filename: &str) {
        let disposition =
            header_text(headers, header::CONTENT_DISPOSITION).expect("content disposition");
        assert!(
            disposition.starts_with("attachment") && disposition.contains(filename),
            "{disposition}"
        );
    }

    #[tokio::test]
    async fn a_stored_javascript_attachment_is_plain_text_through_the_security_layer() {
        let fake = fake_s3().await;
        let tmp = tempfile::tempdir().unwrap();
        let mut cfg = fake.config(tmp.path());
        cfg.mode = DeploymentMode::Mp;
        let app = Arc::new(AppState::for_tests(cfg));
        let bucket = app.cfg.storage.bucket_cdn.clone();
        let segment = transport_stream_object("text/javascript");
        fake.put_object(
            &format!("{bucket}/attachments/1/2/segment.js"),
            segment.clone(),
        );
        fake.put_object(
            &format!("{bucket}/attachments/1/2/seg.ts"),
            transport_stream_object("video/mp2t"),
        );
        fake.put_object(
            &format!("{bucket}/themes/abc123.css"),
            FakeObject {
                body: b"body{color:#fff}".to_vec(),
                content_type: Some("text/css".to_owned()),
                ..FakeObject::default()
            },
        );
        let mp = security_layered_router(&app, DeploymentMode::Mp);

        let response = send(&mp, Method::GET, "/attachments/1/2/segment.js", None).await;
        assert_eq!(StatusCode::OK, response.status());
        assert_eq!(
            Some(INERT),
            header_text(response.headers(), header::CONTENT_TYPE)
        );
        assert_eq!(
            Some("nosniff"),
            header_text(response.headers(), header::X_CONTENT_TYPE_OPTIONS)
        );
        assert_attachment_named(response.headers(), "segment.js");
        let body = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        assert_eq!(segment.body.as_slice(), &body[..]);

        let head = send(&mp, Method::HEAD, "/attachments/1/2/segment.js", None).await;
        assert_eq!(StatusCode::OK, head.status());
        assert_eq!(
            Some(INERT),
            header_text(head.headers(), header::CONTENT_TYPE)
        );

        let ranged = send(
            &mp,
            Method::GET,
            "/attachments/1/2/segment.js",
            Some("bytes=0-0"),
        )
        .await;
        assert_eq!(StatusCode::PARTIAL_CONTENT, ranged.status());
        assert_eq!(
            Some(INERT),
            header_text(ranged.headers(), header::CONTENT_TYPE)
        );

        let unsatisfiable = send(
            &mp,
            Method::HEAD,
            "/attachments/1/2/segment.js",
            Some("bytes=999-"),
        )
        .await;
        assert_eq!(StatusCode::RANGE_NOT_SATISFIABLE, unsatisfiable.status());
        assert_eq!(None, unsatisfiable.headers().get(header::CONTENT_TYPE));
        assert_eq!(
            Some("nosniff"),
            header_text(unsatisfiable.headers(), header::X_CONTENT_TYPE_OPTIONS)
        );

        let download = send(
            &mp,
            Method::GET,
            "/attachments/1/2/segment.js?download=1",
            None,
        )
        .await;
        assert_eq!(StatusCode::OK, download.status());
        assert_eq!(
            Some(INERT),
            header_text(download.headers(), header::CONTENT_TYPE)
        );
        assert_attachment_named(download.headers(), "segment.js");

        let upload = security_layered_router(&app, DeploymentMode::Upload);
        let neutralized = send(&upload, Method::GET, "/attachments/1/2/segment.js", None).await;
        assert_eq!(StatusCode::OK, neutralized.status());
        assert_eq!(
            Some(INERT),
            header_text(neutralized.headers(), header::CONTENT_TYPE)
        );

        let transport_stream = send(&mp, Method::GET, "/attachments/1/2/seg.ts", None).await;
        assert_eq!(StatusCode::OK, transport_stream.status());
        assert_eq!(
            Some("video/mp2t"),
            header_text(transport_stream.headers(), header::CONTENT_TYPE)
        );

        let theme = send(&mp, Method::GET, "/themes/abc123.css", None).await;
        assert_eq!(StatusCode::OK, theme.status());
        assert_eq!(
            Some("text/css; charset=utf-8"),
            header_text(theme.headers(), header::CONTENT_TYPE)
        );
    }

    async fn robots_header_for(mode: DeploymentMode) -> Option<String> {
        let router = Router::new()
            .route(
                "/probe",
                get(|| async {
                    let mut response = Response::new(Body::empty());
                    http_headers::add_media_headers(response.headers_mut(), 0, "text/plain", None);
                    response
                }),
            )
            .layer(axum::middleware::from_fn_with_state(
                mode,
                add_security_header_middleware,
            ));
        let response = tower::ServiceExt::oneshot(
            router,
            Request::builder()
                .uri("/probe")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
        response
            .headers()
            .get("X-Robots-Tag")
            .map(|v| v.to_str().unwrap().to_owned())
    }

    #[tokio::test]
    async fn static_mode_does_not_set_robots_tag() {
        assert_eq!(robots_header_for(DeploymentMode::Static).await, None);
    }

    #[tokio::test]
    async fn media_and_upload_modes_still_set_robots_tag() {
        assert_eq!(
            robots_header_for(DeploymentMode::Mp).await.as_deref(),
            Some(http_headers::ROBOTS)
        );
        assert_eq!(
            robots_header_for(DeploymentMode::Upload).await.as_deref(),
            Some(http_headers::ROBOTS)
        );
    }

    #[tokio::test]
    async fn request_drain_waits_until_every_response_body_is_dropped() {
        let drain = HttpRequestDrain::new();
        let router = Router::new().route("/probe", get(|| async { "OK" })).layer(
            axum::middleware::from_fn_with_state(drain.clone(), track_active_request),
        );
        let response = tower::ServiceExt::oneshot(
            router,
            Request::builder()
                .uri("/probe")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();
        assert_eq!(drain.active_requests(), 1);
        drop(response);
        drain.wait_for_requests_drained().await;
        assert_eq!(drain.active_requests(), 0);
    }
}
