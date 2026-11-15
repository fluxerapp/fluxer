// SPDX-License-Identifier: AGPL-3.0-or-later

use std::task::{Context, Poll};
use tracing::Span;
use tracing::instrument::{Instrument, Instrumented};

pub fn span(svc: &'static str) -> Span {
    tracing::info_span!("svc", svc)
}

#[derive(Clone, Debug)]
pub struct CurrentSpanLayer {
    span: Span,
}

impl CurrentSpanLayer {
    pub fn new() -> Self {
        Self {
            span: Span::current(),
        }
    }
}

impl Default for CurrentSpanLayer {
    fn default() -> Self {
        Self::new()
    }
}

impl<S> tower_layer::Layer<S> for CurrentSpanLayer {
    type Service = CurrentSpanService<S>;

    fn layer(&self, inner: S) -> Self::Service {
        CurrentSpanService {
            inner,
            span: self.span.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CurrentSpanService<S> {
    inner: S,
    span: Span,
}

impl<S, R> tower_service::Service<R> for CurrentSpanService<S>
where
    S: tower_service::Service<R>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = Instrumented<S::Future>;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(&mut self, request: R) -> Self::Future {
        let _entered = self.span.enter();
        self.inner.call(request).instrument(self.span.clone())
    }
}
