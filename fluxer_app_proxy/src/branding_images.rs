// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{AppProxyConfig, HttpEndpoint};
use axum::http::header;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::watch;
use tokio::task::JoinHandle;

const BRANDING_IMAGES: [(&str, &str); 5] = [
    ("icon_url", "FLUXER_APP_ICON_URL"),
    ("symbol_url", "FLUXER_APP_SYMBOL_URL"),
    ("logo_url", "FLUXER_APP_LOGO_URL"),
    ("wordmark_url", "FLUXER_APP_WORDMARK_URL"),
    ("favicon_url", "FLUXER_APP_FAVICON_URL"),
];
const DISCOVERY_FETCH_TIMEOUT: Duration = Duration::from_secs(5);
const DISCOVERY_BODY_BYTES_MAX: usize = 1024 * 1024;
const REFRESH_INTERVAL_MIN: Duration = Duration::from_secs(1);
const REFRESH_BACKOFF_MAX: Duration = Duration::from_secs(300);

pub fn origins_from_environment<F>(mut read_var: F) -> Vec<HttpEndpoint>
where
    F: FnMut(&str) -> Option<String>,
{
    distinct_origins(
        BRANDING_IMAGES
            .iter()
            .filter_map(|(_, name)| Some((*name, read_var(name)?))),
    )
}

pub fn origins_from_discovery(discovery: &serde_json::Value) -> Option<Vec<HttpEndpoint>> {
    let branding = discovery.pointer("/app_public/branding")?.as_object()?;
    Some(distinct_origins(BRANDING_IMAGES.iter().filter_map(
        |(key, _)| Some((*key, branding.get(*key)?.as_str()?.to_owned())),
    )))
}

fn distinct_origins(urls: impl Iterator<Item = (&'static str, String)>) -> Vec<HttpEndpoint> {
    let mut origins: Vec<HttpEndpoint> = Vec::new();
    for (name, url) in urls {
        if url.trim().is_empty() {
            continue;
        }
        match HttpEndpoint::origin_of(name, &url) {
            Ok(origin) => {
                if !origins.contains(&origin) {
                    origins.push(origin);
                }
            }
            Err(error) => {
                tracing::debug!(%error, "branding image has no origin to allow");
            }
        }
    }
    origins
}

async fn fetch_discovery(
    client: &reqwest::Client,
    upstream_url: &str,
) -> anyhow::Result<serde_json::Value> {
    let mut response = client
        .get(upstream_url)
        .header(header::ACCEPT, "application/json")
        .timeout(DISCOVERY_FETCH_TIMEOUT)
        .send()
        .await?;
    if !response.status().is_success() {
        anyhow::bail!("upstream discovery returned {}", response.status());
    }
    let mut body: Vec<u8> = Vec::new();
    while let Some(chunk) = response.chunk().await? {
        if body.len() + chunk.len() > DISCOVERY_BODY_BYTES_MAX {
            anyhow::bail!("upstream discovery exceeds {DISCOVERY_BODY_BYTES_MAX} bytes");
        }
        body.extend_from_slice(&chunk);
    }
    Ok(serde_json::from_slice(&body)?)
}

#[derive(Clone, Debug)]
pub struct BrandingImageOrigins(watch::Sender<Arc<[HttpEndpoint]>>);

impl BrandingImageOrigins {
    pub fn new(origins: Vec<HttpEndpoint>) -> Self {
        Self(watch::Sender::new(origins.into()))
    }

    pub fn from_config(config: &AppProxyConfig) -> Self {
        if config.self_hosted {
            Self::new(config.branding_image_origins.clone())
        } else {
            Self::new(Vec::new())
        }
    }

    pub fn current(&self) -> Arc<[HttpEndpoint]> {
        self.0.borrow().clone()
    }

    pub async fn refresh(
        &self,
        client: &reqwest::Client,
        upstream_url: &str,
    ) -> anyhow::Result<()> {
        let discovery = fetch_discovery(client, upstream_url).await?;
        let Some(origins) = origins_from_discovery(&discovery) else {
            anyhow::bail!("upstream discovery has no app_public.branding object");
        };
        let changed = self.0.send_if_modified(|current| {
            if **current == *origins {
                return false;
            }
            *current = origins.as_slice().into();
            true
        });
        if changed {
            let origins: Vec<&str> = origins.iter().map(HttpEndpoint::csp_origin).collect();
            tracing::info!(?origins, "branding image origins updated");
        }
        Ok(())
    }

    pub fn start_background_refresh(
        &self,
        client: reqwest::Client,
        upstream_url: String,
        interval: Duration,
    ) -> JoinHandle<()> {
        self.spawn_refresh_loop(client, upstream_url, interval.max(REFRESH_INTERVAL_MIN))
    }

    fn spawn_refresh_loop(
        &self,
        client: reqwest::Client,
        upstream_url: String,
        interval: Duration,
    ) -> JoinHandle<()> {
        let origins = self.clone();
        tokio::spawn(async move {
            let mut consecutive_failures: u32 = 0;
            loop {
                let wait = match origins.refresh(&client, &upstream_url).await {
                    Ok(()) => {
                        consecutive_failures = 0;
                        interval
                    }
                    Err(error) => {
                        let backoff = interval
                            .saturating_mul(1 << consecutive_failures.min(5))
                            .min(REFRESH_BACKOFF_MAX);
                        consecutive_failures = consecutive_failures.saturating_add(1);
                        tracing::warn!(
                            %error,
                            url = %upstream_url,
                            consecutive_failures,
                            backoff_ms = backoff.as_millis() as u64,
                            "branding image origin refresh failed, keeping the previous origins"
                        );
                        backoff
                    }
                };
                tokio::time::sleep(wait).await;
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::http::StatusCode;
    use std::sync::atomic::{AtomicUsize, Ordering};

    fn sources(origins: &[HttpEndpoint]) -> Vec<&str> {
        origins.iter().map(HttpEndpoint::csp_origin).collect()
    }

    fn endpoint(value: &str) -> HttpEndpoint {
        HttpEndpoint::parse("TEST_ENDPOINT", value).unwrap()
    }

    fn fast_client() -> reqwest::Client {
        reqwest::Client::builder()
            .timeout(Duration::from_secs(2))
            .build()
            .unwrap()
    }

    async fn serve_discovery(responses: Vec<(StatusCode, String)>) -> String {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let served = Arc::new(AtomicUsize::new(0));
        let responses = Arc::new(responses);
        let router = axum::Router::new().fallback(move || {
            let index = served.fetch_add(1, Ordering::SeqCst);
            let responses = Arc::clone(&responses);
            async move { responses[index.min(responses.len() - 1)].clone() }
        });
        tokio::spawn(async move {
            axum::serve(listener, router).await.unwrap();
        });
        format!("http://{addr}/.well-known/fluxer")
    }

    fn discovery_with_branding(branding: serde_json::Value) -> String {
        serde_json::json!({"api_code_version": "v1", "app_public": {"branding": branding}})
            .to_string()
    }

    #[test]
    fn discovery_branding_images_yield_their_distinct_origins() {
        let discovery = serde_json::json!({
            "app_public": {
                "branding": {
                    "product_name": "Example",
                    "icon_url": "https://example.test/logo.png",
                    "symbol_url": " https://example.test/symbol.svg?v=2#dark ",
                    "logo_url": "http://assets.example.test:8443/brand/logo.png",
                    "wordmark_url": null,
                    "favicon_url": "https://EXAMPLE.test:443/favicon.png",
                    "status_page_url": "https://status.example.test"
                }
            }
        });

        assert_eq!(
            sources(&origins_from_discovery(&discovery).unwrap()),
            ["https://example.test", "http://assets.example.test:8443"]
        );
    }

    #[test]
    fn branding_images_without_a_network_origin_are_skipped() {
        let discovery = serde_json::json!({
            "app_public": {
                "branding": {
                    "icon_url": "/branding/icon.png",
                    "symbol_url": "data:image/png;base64,AAAA",
                    "logo_url": "   ",
                    "wordmark_url": 7,
                    "favicon_url": "ftp://example.test/favicon.png"
                }
            }
        });

        assert!(origins_from_discovery(&discovery).unwrap().is_empty());
    }

    #[test]
    fn a_document_without_branding_is_not_a_discovery_snapshot() {
        assert!(origins_from_discovery(&serde_json::json!({"api_code_version": "v1"})).is_none());
        assert!(origins_from_discovery(&serde_json::json!({"app_public": {}})).is_none());
        assert!(
            origins_from_discovery(&serde_json::json!({"app_public": {"branding": null}}))
                .is_none()
        );
        assert!(
            origins_from_discovery(&serde_json::json!({"app_public": {"branding": {}}}))
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn environment_branding_images_yield_their_distinct_origins() {
        let origins = origins_from_environment(|name| match name {
            "FLUXER_APP_ICON_URL" => Some("https://example.test/logo.png".to_owned()),
            "FLUXER_APP_FAVICON_URL" => Some("https://example.test/favicon.png".to_owned()),
            "FLUXER_APP_WORDMARK_URL" => Some("https://cdn.example.test/wordmark.svg".to_owned()),
            "FLUXER_APP_LOGO_URL" => Some("not a url".to_owned()),
            "FLUXER_APP_STATUS_PAGE_URL" => Some("https://status.example.test".to_owned()),
            _ => None,
        });

        assert_eq!(
            sources(&origins),
            ["https://example.test", "https://cdn.example.test"]
        );
    }

    #[test]
    fn only_a_self_hosted_instance_seeds_origins_from_its_environment() {
        let mut config = AppProxyConfig::from_env();
        config.branding_image_origins = vec![endpoint("https://example.test")];

        config.self_hosted = false;
        assert!(
            BrandingImageOrigins::from_config(&config)
                .current()
                .is_empty()
        );

        config.self_hosted = true;
        assert_eq!(
            sources(&BrandingImageOrigins::from_config(&config).current()),
            ["https://example.test"]
        );
    }

    #[tokio::test]
    async fn a_discovery_snapshot_replaces_the_environment_seed() {
        let upstream = serve_discovery(vec![(
            StatusCode::OK,
            discovery_with_branding(serde_json::json!({
                "icon_url": "https://dashboard.example.test/icon.png",
                "favicon_url": null
            })),
        )])
        .await;
        let origins = BrandingImageOrigins::new(vec![endpoint("https://env.example.test")]);
        assert_eq!(sources(&origins.current()), ["https://env.example.test"]);

        origins.refresh(&fast_client(), &upstream).await.unwrap();

        assert_eq!(
            sources(&origins.current()),
            ["https://dashboard.example.test"]
        );
    }

    #[tokio::test]
    async fn a_failed_refresh_keeps_the_previous_origins() {
        let seeded = || BrandingImageOrigins::new(vec![endpoint("https://env.example.test")]);

        let origins = seeded();
        let unavailable =
            serve_discovery(vec![(StatusCode::SERVICE_UNAVAILABLE, String::new())]).await;
        assert!(origins.refresh(&fast_client(), &unavailable).await.is_err());
        assert_eq!(sources(&origins.current()), ["https://env.example.test"]);

        let origins = seeded();
        let not_json = serve_discovery(vec![(StatusCode::OK, "<html>".to_owned())]).await;
        assert!(origins.refresh(&fast_client(), &not_json).await.is_err());
        assert_eq!(sources(&origins.current()), ["https://env.example.test"]);

        let origins = seeded();
        let no_branding = serve_discovery(vec![(StatusCode::OK, "{}".to_owned())]).await;
        assert!(origins.refresh(&fast_client(), &no_branding).await.is_err());
        assert_eq!(sources(&origins.current()), ["https://env.example.test"]);

        let origins = seeded();
        let oversized = serve_discovery(vec![(
            StatusCode::OK,
            discovery_with_branding(serde_json::json!({
                "icon_url": format!(
                    "https://dashboard.example.test/{}",
                    "a".repeat(DISCOVERY_BODY_BYTES_MAX)
                )
            })),
        )])
        .await;
        assert!(origins.refresh(&fast_client(), &oversized).await.is_err());
        assert_eq!(sources(&origins.current()), ["https://env.example.test"]);

        let origins = seeded();
        assert!(
            origins
                .refresh(&fast_client(), "http://127.0.0.1:1/.well-known/fluxer")
                .await
                .is_err()
        );
        assert_eq!(sources(&origins.current()), ["https://env.example.test"]);
    }

    #[tokio::test]
    async fn the_background_refresh_follows_discovery_and_survives_a_failure() {
        let upstream = serve_discovery(vec![
            (
                StatusCode::OK,
                discovery_with_branding(serde_json::json!({
                    "icon_url": "https://first.example.test/icon.png"
                })),
            ),
            (StatusCode::BAD_GATEWAY, String::new()),
            (
                StatusCode::OK,
                discovery_with_branding(serde_json::json!({
                    "icon_url": "https://second.example.test/icon.png"
                })),
            ),
        ])
        .await;
        let origins = BrandingImageOrigins::new(Vec::new());
        let mut changes = origins.0.subscribe();

        let refresh =
            origins.spawn_refresh_loop(fast_client(), upstream, Duration::from_millis(10));

        changes.changed().await.unwrap();
        assert_eq!(sources(&origins.current()), ["https://first.example.test"]);
        changes.changed().await.unwrap();
        assert_eq!(sources(&origins.current()), ["https://second.example.test"]);
        refresh.abort();
    }
}
