// SPDX-License-Identifier: AGPL-3.0-or-later

mod retry;

use crate::metrics::http_client::HTTPClientMetrics;
use crate::public_net_policy::PinnedDnsResolver;
use reqwest_middleware::{ClientBuilder, ClientWithMiddleware};
use reqwest_retry::{RetryTransientMiddleware, policies::ExponentialBackoff};
use retry::{MediaProxyRetryStrategy, ObservableRetryPolicy};
use std::num::{NonZeroU32, NonZeroU64};
use std::sync::Arc;
use std::time::Duration;

pub type HttpClient = ClientWithMiddleware;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct HTTPClientOptions {
    connect_timeout_ms: NonZeroU64,
    request_timeout: HTTPRequestTimeout,
    retries: HTTPRetryPolicy,
    address_policy: HTTPAddressPolicy,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum HTTPRequestTimeout {
    Bounded(NonZeroU64),
    Disabled,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum HTTPRetryPolicy {
    Disabled,
    Enabled {
        max_retries: NonZeroU32,
        min_delay_ms: NonZeroU64,
        max_delay_ms: NonZeroU64,
    },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum HTTPAddressPolicy {
    Any,
    PublicOnly,
}

impl HTTPClientOptions {
    pub fn new(connect_timeout_ms: NonZeroU64, timeout_ms: NonZeroU64) -> Self {
        Self {
            connect_timeout_ms,
            request_timeout: HTTPRequestTimeout::Bounded(timeout_ms),
            ..Self::default()
        }
    }

    pub fn without_request_timeout(mut self) -> Self {
        self.request_timeout = HTTPRequestTimeout::Disabled;
        self
    }

    pub fn restrict_to_public(mut self) -> Self {
        self.address_policy = HTTPAddressPolicy::PublicOnly;
        self
    }

    pub fn without_retries(mut self) -> Self {
        self.retries = HTTPRetryPolicy::Disabled;
        self
    }
}

impl Default for HTTPClientOptions {
    fn default() -> Self {
        Self {
            connect_timeout_ms: NonZeroU64::new(1_500)
                .expect("default connection timeout must be nonzero"),
            request_timeout: HTTPRequestTimeout::Bounded(
                NonZeroU64::new(30_000).expect("default request timeout must be nonzero"),
            ),
            retries: HTTPRetryPolicy::Enabled {
                max_retries: NonZeroU32::new(2)
                    .expect("default maximum retry count must be nonzero"),
                min_delay_ms: NonZeroU64::new(25)
                    .expect("default minimum retry delay must be nonzero"),
                max_delay_ms: NonZeroU64::new(500)
                    .expect("default maximum retry delay must be nonzero"),
            },
            address_policy: HTTPAddressPolicy::Any,
        }
    }
}

pub fn build_raw(options: HTTPClientOptions) -> Result<reqwest::Client, reqwest::Error> {
    // System proxy discovery stays on. HTTP_PROXY, HTTPS_PROXY, ALL_PROXY and NO_PROXY are part
    // of the deployment's egress configuration, so an operator who funnels egress through a
    // gateway gets storage reads and external fetches routed through it. The healthcheck probe
    // is the one client that opts out, because it only ever dials this process on loopback.
    let mut builder = reqwest::Client::builder()
        .connect_timeout(Duration::from_millis(options.connect_timeout_ms.get()))
        .redirect(reqwest::redirect::Policy::none())
        .user_agent(crate::constants::OUTBOUND_USER_AGENT);
    if let HTTPRequestTimeout::Bounded(timeout_ms) = options.request_timeout {
        builder = builder.timeout(Duration::from_millis(timeout_ms.get()));
    }
    if options.address_policy == HTTPAddressPolicy::PublicOnly {
        builder = builder.dns_resolver(Arc::new(PinnedDnsResolver));
    }
    builder.build()
}

pub fn build(
    options: HTTPClientOptions,
    metrics: Arc<HTTPClientMetrics>,
) -> Result<HttpClient, reqwest::Error> {
    let client = build_raw(options)?;
    let builder = ClientBuilder::new(client);
    let HTTPRetryPolicy::Enabled {
        max_retries,
        min_delay_ms,
        max_delay_ms,
    } = options.retries
    else {
        return Ok(builder.build());
    };
    let retry_policy = ExponentialBackoff::builder()
        .retry_bounds(
            Duration::from_millis(min_delay_ms.get()),
            Duration::from_millis(max_delay_ms.max(min_delay_ms).get()),
        )
        .build_with_max_retries(max_retries.get());
    Ok(builder
        .with(RetryTransientMiddleware::new_with_policy_and_strategy(
            ObservableRetryPolicy::new(retry_policy, Arc::clone(&metrics)),
            MediaProxyRetryStrategy::new(metrics),
        ))
        .build())
}

pub fn build_default(metrics: Arc<HTTPClientMetrics>) -> HttpClient {
    build(HTTPClientOptions::default(), metrics)
        .expect("default HTTP client configuration is valid")
}

pub fn build_raw_default() -> reqwest::Client {
    build_raw(HTTPClientOptions::default()).expect("default HTTP client configuration is valid")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metrics::Metrics;

    fn millis(value: u64) -> NonZeroU64 {
        NonZeroU64::new(value).expect("test timeout must be nonzero")
    }

    #[test]
    fn builds_retrying_client() {
        let client = build(
            HTTPClientOptions::new(millis(1), millis(1)),
            Metrics::new().http_client(),
        );
        assert!(client.is_ok());
    }

    #[test]
    fn builds_non_retrying_client() {
        let client = build(
            HTTPClientOptions::default().without_retries(),
            Metrics::new().http_client(),
        );
        assert!(client.is_ok());
    }

    #[test]
    fn default_options_carry_the_frozen_timeout_and_retry_budget() {
        let options = HTTPClientOptions::default();
        assert_eq!(options.connect_timeout_ms, millis(1_500));
        assert_eq!(
            options.request_timeout,
            HTTPRequestTimeout::Bounded(millis(30_000))
        );
        assert_eq!(
            options.retries,
            HTTPRetryPolicy::Enabled {
                max_retries: NonZeroU32::new(2).expect("nonzero"),
                min_delay_ms: millis(25),
                max_delay_ms: millis(500),
            }
        );
        assert_eq!(options.address_policy, HTTPAddressPolicy::Any);
    }

    #[test]
    fn new_overrides_only_the_two_timeouts_and_keeps_the_default_retry_budget() {
        let options = HTTPClientOptions::new(millis(250), millis(4_000));
        assert_eq!(options.connect_timeout_ms, millis(250));
        assert_eq!(
            options.request_timeout,
            HTTPRequestTimeout::Bounded(millis(4_000))
        );
        assert_eq!(options.retries, HTTPClientOptions::default().retries);
        assert_eq!(options.address_policy, HTTPAddressPolicy::Any);
    }

    #[test]
    fn each_modifier_changes_exactly_one_facet_and_composes_with_the_others() {
        let base = HTTPClientOptions::new(millis(250), millis(4_000));
        let stripped = base
            .without_request_timeout()
            .restrict_to_public()
            .without_retries();
        assert_eq!(stripped.connect_timeout_ms, millis(250));
        assert_eq!(stripped.request_timeout, HTTPRequestTimeout::Disabled);
        assert_eq!(stripped.retries, HTTPRetryPolicy::Disabled);
        assert_eq!(stripped.address_policy, HTTPAddressPolicy::PublicOnly);
        assert_eq!(
            base.without_request_timeout(),
            HTTPClientOptions {
                request_timeout: HTTPRequestTimeout::Disabled,
                ..base
            }
        );
        assert_eq!(
            base.restrict_to_public(),
            HTTPClientOptions {
                address_policy: HTTPAddressPolicy::PublicOnly,
                ..base
            }
        );
        assert_eq!(
            base.without_retries(),
            HTTPClientOptions {
                retries: HTTPRetryPolicy::Disabled,
                ..base
            }
        );
    }

    #[test]
    fn every_option_shape_produces_a_usable_transport_client() {
        let base = HTTPClientOptions::new(millis(250), millis(4_000));
        assert!(build_raw(base).is_ok());
        assert!(build_raw(base.without_request_timeout()).is_ok());
        assert!(build_raw(base.restrict_to_public()).is_ok());
        assert!(build_raw(HTTPClientOptions::default()).is_ok());
    }

    #[test]
    fn every_transport_client_keeps_system_proxy_discovery() {
        // reqwest hands out no accessor for a built client's proxy matchers and renders the
        // field only while one survives, so Debug is the only way to catch a stray no_proxy().
        for options in [
            HTTPClientOptions::default(),
            HTTPClientOptions::default().restrict_to_public(),
            HTTPClientOptions::new(millis(250), millis(4_000)),
        ] {
            let client = build_raw(options).expect("transport client builds");
            assert!(
                format!("{client:?}").contains("proxies"),
                "outbound clients must honour HTTP_PROXY/HTTPS_PROXY/ALL_PROXY: {client:?}"
            );
        }
    }

    #[tokio::test]
    async fn a_pinned_resolver_rejection_survives_the_transport_error_chain() {
        let client =
            build_raw(HTTPClientOptions::new(millis(250), millis(1_000)).restrict_to_public())
                .expect("the public-only client configuration is valid");
        let error = client
            .get("http://localhost/")
            .send()
            .await
            .expect_err("the pinned resolver rejects a loopback host");
        assert!(crate::public_net_policy::is_pinned_dns_failure(&error));
    }
}
