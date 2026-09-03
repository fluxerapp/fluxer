// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::metrics::http_client::HTTPClientMetrics;
use reqwest::StatusCode;
use reqwest_middleware::Error as MiddlewareError;
use reqwest_retry::{
    RetryDecision, RetryPolicy, Retryable, RetryableStrategy, policies::ExponentialBackoff,
};
use std::sync::Arc;
use std::time::SystemTime;

pub(super) struct ObservableRetryPolicy {
    inner: ExponentialBackoff,
    metrics: Arc<HTTPClientMetrics>,
}

impl ObservableRetryPolicy {
    pub(super) fn new(inner: ExponentialBackoff, metrics: Arc<HTTPClientMetrics>) -> Self {
        Self { inner, metrics }
    }
}

impl RetryPolicy for ObservableRetryPolicy {
    fn should_retry(&self, request_start_time: SystemTime, n_past_retries: u32) -> RetryDecision {
        let decision = self.inner.should_retry(request_start_time, n_past_retries);
        match decision {
            RetryDecision::Retry { .. } => self.metrics.record_retry(),
            RetryDecision::DoNotRetry => self.metrics.record_retries_exhausted(),
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
                    self.metrics.record_retryable_status();
                }
                retryable
            }),
            Err(error) => retryable_error(error).map(|retryable| {
                if retryable == Retryable::Transient {
                    self.metrics.record_retryable_error();
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

            if error.is_timeout() || is_connect {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metrics::Metrics;
    use std::time::Duration;

    fn response_with_status(status: StatusCode) -> Result<reqwest::Response, MiddlewareError> {
        let response = http::Response::builder()
            .status(status)
            .body("")
            .expect("building a synthetic response cannot fail");
        Ok(reqwest::Response::from(response))
    }

    fn counter_line(rendered: &str, name: &str) -> String {
        rendered
            .lines()
            .find(|line| line.starts_with(name) && !line.starts_with('#'))
            .unwrap_or_else(|| panic!("{name} is missing from the rendered metrics"))
            .to_owned()
    }

    #[test]
    fn retry_strategy_retries_only_explicit_transient_statuses() {
        assert!(matches!(
            retryable_status(StatusCode::REQUEST_TIMEOUT),
            Some(Retryable::Transient)
        ));
        assert!(matches!(
            retryable_status(StatusCode::TOO_MANY_REQUESTS),
            Some(Retryable::Transient)
        ));
        assert!(matches!(
            retryable_status(StatusCode::INTERNAL_SERVER_ERROR),
            Some(Retryable::Transient)
        ));
        assert!(matches!(
            retryable_status(StatusCode::BAD_GATEWAY),
            Some(Retryable::Transient)
        ));
        assert!(matches!(
            retryable_status(StatusCode::SERVICE_UNAVAILABLE),
            Some(Retryable::Transient)
        ));
        assert!(matches!(
            retryable_status(StatusCode::GATEWAY_TIMEOUT),
            Some(Retryable::Transient)
        ));
        assert!(matches!(
            retryable_status(StatusCode::NOT_FOUND),
            Some(Retryable::Fatal)
        ));
        assert!(matches!(
            retryable_status(StatusCode::NOT_IMPLEMENTED),
            Some(Retryable::Fatal)
        ));
        assert!(retryable_status(StatusCode::OK).is_none());
        assert!(retryable_status(StatusCode::FOUND).is_none());
    }

    #[test]
    fn retryable_status_ignores_success_and_redirect_statuses() {
        assert!(retryable_status(StatusCode::OK).is_none());
        assert!(retryable_status(StatusCode::FOUND).is_none());
    }

    #[test]
    fn server_errors_outside_the_whitelist_stay_fatal() {
        for status in [
            StatusCode::NOT_IMPLEMENTED,
            StatusCode::HTTP_VERSION_NOT_SUPPORTED,
            StatusCode::INSUFFICIENT_STORAGE,
            StatusCode::LOOP_DETECTED,
            StatusCode::NETWORK_AUTHENTICATION_REQUIRED,
        ] {
            assert!(
                matches!(retryable_status(status), Some(Retryable::Fatal)),
                "{status} must not be retried"
            );
        }
    }

    #[test]
    fn middleware_failures_are_fatal_and_are_not_counted_as_transient() {
        let metrics = Metrics::new();
        let strategy = MediaProxyRetryStrategy::new(metrics.http_client());
        let error = Err(MiddlewareError::Middleware(anyhow::anyhow!(
            "middleware refused the request"
        )));
        assert!(matches!(strategy.handle(&error), Some(Retryable::Fatal)));
        let rendered = metrics.render();
        assert_eq!(
            counter_line(
                &rendered,
                "fluxer_media_proxy_http_retryable_classifications_total{reason=\"error\"}"
            ),
            "fluxer_media_proxy_http_retryable_classifications_total{reason=\"error\"} 0"
        );
    }

    #[test]
    fn transient_status_classifications_are_counted_once_and_fatal_ones_are_not() {
        let metrics = Metrics::new();
        let strategy = MediaProxyRetryStrategy::new(metrics.http_client());
        assert!(matches!(
            strategy.handle(&response_with_status(StatusCode::SERVICE_UNAVAILABLE)),
            Some(Retryable::Transient)
        ));
        assert!(matches!(
            strategy.handle(&response_with_status(StatusCode::NOT_FOUND)),
            Some(Retryable::Fatal)
        ));
        assert!(
            strategy
                .handle(&response_with_status(StatusCode::OK))
                .is_none()
        );
        let rendered = metrics.render();
        assert_eq!(
            counter_line(
                &rendered,
                "fluxer_media_proxy_http_retryable_classifications_total{reason=\"status\"}"
            ),
            "fluxer_media_proxy_http_retryable_classifications_total{reason=\"status\"} 1"
        );
    }

    #[test]
    fn every_retry_decision_reaches_the_injected_metrics_handle() {
        let metrics = Metrics::new();
        let backoff = ExponentialBackoff::builder()
            .retry_bounds(Duration::from_millis(1), Duration::from_millis(2))
            .build_with_max_retries(1);
        let policy = ObservableRetryPolicy::new(backoff, metrics.http_client());
        let started = SystemTime::now();
        assert!(matches!(
            policy.should_retry(started, 0),
            RetryDecision::Retry { .. }
        ));
        assert!(matches!(
            policy.should_retry(started, 1),
            RetryDecision::DoNotRetry
        ));
        let rendered = metrics.render();
        assert_eq!(
            counter_line(&rendered, "fluxer_media_proxy_http_retries_total"),
            "fluxer_media_proxy_http_retries_total 1"
        );
        assert_eq!(
            counter_line(&rendered, "fluxer_media_proxy_http_retries_exhausted_total"),
            "fluxer_media_proxy_http_retries_exhausted_total 1"
        );
    }
}
