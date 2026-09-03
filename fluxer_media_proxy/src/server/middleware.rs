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
    let headers = response.headers_mut();
    http_headers::add_security_headers(headers);
    if mode == DeploymentMode::Static {
        headers.remove("X-Robots-Tag");
    }
    response
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::{Router, routing::get};

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
