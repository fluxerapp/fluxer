// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::metrics::http_client::HTTPClientMetrics;
use reqwest::StatusCode;
use reqwest_middleware::Error as MiddlewareError;
use reqwest_retry::{
    RetryDecision, RetryPolicy, Retryable, RetryableStrategy, policies::ExponentialBackoff,
};
use std::io::ErrorKind;
use std::sync::Arc;
use std::time::{Duration, SystemTime};

pub(super) struct ObservableRetryPolicy {
    inner: ExponentialBackoff,
    retry_window: Option<Duration>,
    metrics: Arc<HTTPClientMetrics>,
}

impl ObservableRetryPolicy {
    pub(super) fn new(
        inner: ExponentialBackoff,
        retry_window: Option<Duration>,
        metrics: Arc<HTTPClientMetrics>,
    ) -> Self {
        Self {
            inner,
            retry_window,
            metrics,
        }
    }
}

impl RetryPolicy for ObservableRetryPolicy {
    fn should_retry(&self, request_start_time: SystemTime, n_past_retries: u32) -> RetryDecision {
        let window_spent = self.retry_window.is_some_and(|window| {
            request_start_time
                .elapsed()
                .is_ok_and(|elapsed| elapsed >= window)
        });
        let decision = if window_spent {
            RetryDecision::DoNotRetry
        } else {
            self.inner.should_retry(request_start_time, n_past_retries)
        };
        match decision {
            RetryDecision::Retry { .. } => self.metrics.http_retries.inc(),
            RetryDecision::DoNotRetry => self.metrics.http_retries_exhausted.inc(),
        }
        decision
    }
}

pub(super) struct MediaProxyRetryStrategy {
    metrics: Arc<HTTPClientMetrics>,
}

impl MediaProxyRetryStrategy {
    pub(super) fn new(metrics: Arc<HTTPClientMetrics>) -> Self {
        Self { metrics }
    }
}

impl RetryableStrategy for MediaProxyRetryStrategy {
    fn handle(&self, result: &Result<reqwest::Response, MiddlewareError>) -> Option<Retryable> {
        match result {
            Ok(response) => retryable_status(response.status()).map(|retryable| {
                if retryable == Retryable::Transient {
                    self.metrics.http_retryable_status.inc();
                }
                retryable
            }),
            Err(error) => retryable_error(error).map(|retryable| {
                if retryable == Retryable::Transient {
                    self.metrics.http_retryable_error.inc();
                }
                retryable
            }),
        }
    }
}

fn retryable_status(status: StatusCode) -> Option<Retryable> {
    match status {
        StatusCode::REQUEST_TIMEOUT
        | StatusCode::TOO_MANY_REQUESTS
        | StatusCode::INTERNAL_SERVER_ERROR
        | StatusCode::BAD_GATEWAY
        | StatusCode::SERVICE_UNAVAILABLE
        | StatusCode::GATEWAY_TIMEOUT => Some(Retryable::Transient),
        status if status.is_client_error() || status.is_server_error() => Some(Retryable::Fatal),
        _ => None,
    }
}

fn retryable_error(error: &MiddlewareError) -> Option<Retryable> {
    // A pinned-resolver rejection is a policy decision, not a transient fault, and retrying it only
    // repeats the same lookup against a name the caller supplied.
    if crate::public_net_policy::is_pinned_dns_failure(error) {
        return Some(Retryable::Fatal);
    }
    match error {
        MiddlewareError::Middleware(_) => Some(Retryable::Fatal),
        MiddlewareError::Reqwest(error) => {
            #[cfg(not(target_arch = "wasm32"))]
            let is_connect = error.is_connect();
            #[cfg(target_arch = "wasm32")]
            let is_connect = false;

            if error.is_timeout() || is_connect || is_dropped_connection(error) {
                Some(Retryable::Transient)
            } else if error.is_body()
                || error.is_decode()
                || error.is_builder()
                || error.is_redirect()
                || error.is_status()
            {
                Some(Retryable::Fatal)
            } else {
                None
            }
        }
    }
}

pub(crate) fn is_dropped_connection(error: &(dyn std::error::Error + 'static)) -> bool {
    let mut current = Some(error);
    while let Some(error) = current {
        if error
            .downcast_ref::<hyper::Error>()
            .is_some_and(hyper::Error::is_incomplete_message)
        {
            return true;
        }
        if error.downcast_ref::<std::io::Error>().is_some_and(|error| {
            matches!(
                error.kind(),
                ErrorKind::ConnectionReset
                    | ErrorKind::ConnectionAborted
                    | ErrorKind::BrokenPipe
                    | ErrorKind::UnexpectedEof
            )
        }) {
            return true;
        }
        current = error.source();
    }
    false
}
