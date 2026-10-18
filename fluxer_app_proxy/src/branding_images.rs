// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{AppProxyConfig, HttpEndpoint};
use crate::csp::InstanceCspSources;
use axum::http::header;
use reqwest::Url;
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
const CONNECT_ENDPOINTS: [&str; 7] = [
    "api",
    "api_client",
    "api_public",
    "gateway",
    "media",
    "upload_relay",
    "static_cdn",
];
const PRODUCT_NAME: (&str, &str) = ("product_name", "FLUXER_APP_PRODUCT_NAME");
const ICON_URL: (&str, &str) = ("icon_url", "FLUXER_APP_ICON_URL");
const FAVICON_URL: (&str, &str) = ("favicon_url", "FLUXER_APP_FAVICON_URL");
const THEME_COLOR: (&str, &str) = ("theme_color", "FLUXER_APP_THEME_COLOR");
const DISCOVERY_FETCH_TIMEOUT: Duration = Duration::from_secs(5);
const DISCOVERY_BODY_BYTES_MAX: usize = 1024 * 1024;
const REFRESH_INTERVAL_MIN: Duration = Duration::from_secs(1);
const REFRESH_BACKOFF_MAX: Duration = Duration::from_secs(300);

pub fn snapshot_from_environment<F>(mut read_var: F) -> BrandingSnapshot
where
    F: FnMut(&str) -> Option<String>,
{
    snapshot_from_fields(|(_, name)| read_var(name))
}

pub fn snapshot_from_discovery(discovery: &serde_json::Value) -> Option<BrandingSnapshot> {
    let branding = discovery.pointer("/app_public/branding")?.as_object()?;
    Some(BrandingSnapshot {
        instance_sources: instance_sources_from_discovery(discovery),
        ..snapshot_from_fields(|(key, _)| branding.get(key)?.as_str().map(str::to_owned))
    })
}

fn instance_sources_from_discovery(discovery: &serde_json::Value) -> InstanceCspSources {
    let endpoint = |key: &str| {
        discovery
            .pointer(&format!("/endpoints/{key}"))
            .and_then(serde_json::Value::as_str)
            .map(str::trim)
            .filter(|value| !value.is_empty())
    };
    let http_origin = |key: &'static str| {
        endpoint(key).and_then(|value| HttpEndpoint::origin_of(key, value).ok())
    };
    let mut connect_origins: Vec<String> = Vec::new();
    for key in CONNECT_ENDPOINTS {
        let Some(origin) = endpoint(key).and_then(connect_origin) else {
            continue;
        };
        if !connect_origins.contains(&origin) {
            connect_origins.push(origin);
        }
    }
    InstanceCspSources {
        static_cdn_endpoint: http_origin("static_cdn"),
        media_endpoint: http_origin("media"),
        connect_origins,
    }
}

fn connect_origin(value: &str) -> Option<String> {
    let url = Url::parse(value).ok()?;
    if !matches!(url.scheme(), "http" | "https" | "ws" | "wss") || url.host_str().is_none() {
        return None;
    }
    Some(url.origin().ascii_serialization())
}

fn snapshot_from_fields(
    mut field: impl FnMut((&'static str, &'static str)) -> Option<String>,
) -> BrandingSnapshot {
    let image_origins = distinct_origins(
        BRANDING_IMAGES
            .iter()
            .filter_map(|&(key, name)| Some((name, field((key, name))?))),
    );
    let icon_url = non_empty(field(ICON_URL));
    let favicon_url = non_empty(field(FAVICON_URL)).or_else(|| icon_url.clone());
    BrandingSnapshot {
        image_origins: image_origins.into(),
        instance_sources: InstanceCspSources::default(),
        product_name: non_empty(field(PRODUCT_NAME)).map(Into::into),
        icon_url: icon_url.as_deref().and_then(absolute_url).map(Into::into),
        favicon_url: favicon_url
            .as_deref()
            .and_then(absolute_url)
            .map(Into::into),
        theme_color: field(THEME_COLOR)
            .as_deref()
            .and_then(theme_color)
            .map(Into::into),
    }
}

fn non_empty(value: Option<String>) -> Option<String> {
    value
        .map(|value| value.trim().to_owned())
        .filter(|value| !value.is_empty())
}

fn absolute_url(value: &str) -> Option<String> {
    Url::parse(value)
        .ok()
        .filter(|url| matches!(url.scheme(), "http" | "https") && url.host_str().is_some())
        .map(String::from)
}

fn theme_color(value: &str) -> Option<String> {
    let value = value.trim();
    let digits = value.strip_prefix('#').unwrap_or(value);
    if !matches!(digits.len(), 3 | 6) || !digits.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        return None;
    }
    let digits = digits.to_ascii_lowercase();
    let color = if digits.len() == 3 {
        digits.chars().flat_map(|digit| [digit, digit]).collect()
    } else {
        digits
    };
    (color != "000000").then(|| format!("#{color}"))
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

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct BrandingSnapshot {
    pub image_origins: Arc<[HttpEndpoint]>,
    pub instance_sources: InstanceCspSources,
    pub product_name: Option<Arc<str>>,
    pub icon_url: Option<Arc<str>>,
    pub favicon_url: Option<Arc<str>>,
    pub theme_color: Option<Arc<str>>,
}

#[derive(Clone, Debug)]
pub struct InstanceBranding(watch::Sender<Arc<BrandingSnapshot>>);

impl InstanceBranding {
    pub fn new(snapshot: BrandingSnapshot) -> Self {
        Self(watch::Sender::new(Arc::new(snapshot)))
    }

    pub fn from_config(config: &AppProxyConfig) -> Self {
        if config.self_hosted {
            Self::new(config.branding.clone())
        } else {
            Self::new(BrandingSnapshot::default())
        }
    }

    pub fn image_origins(&self) -> Arc<[HttpEndpoint]> {
        self.0.borrow().image_origins.clone()
    }

    pub fn instance_sources(&self) -> InstanceCspSources {
        self.0.borrow().instance_sources.clone()
    }

    pub fn snapshot(&self) -> Arc<BrandingSnapshot> {
        self.0.borrow().clone()
    }

    pub async fn refresh(
        &self,
        client: &reqwest::Client,
        upstream_url: &str,
    ) -> anyhow::Result<()> {
        let discovery = fetch_discovery(client, upstream_url).await?;
        let Some(snapshot) = snapshot_from_discovery(&discovery) else {
            anyhow::bail!("upstream discovery has no app_public.branding object");
        };
        let changed = self.0.send_if_modified(|current| {
            if **current == snapshot {
                return false;
            }
            *current = Arc::new(snapshot.clone());
            true
        });
        if changed {
            let origins: Vec<&str> = snapshot
                .image_origins
                .iter()
                .map(HttpEndpoint::csp_origin)
                .collect();
            tracing::info!(
                ?origins,
                instance_origins = ?snapshot.instance_sources.connect_origins,
                product_name = snapshot.product_name.as_deref(),
                icon_url = snapshot.icon_url.as_deref(),
                favicon_url = snapshot.favicon_url.as_deref(),
                theme_color = snapshot.theme_color.as_deref(),
                "instance branding updated"
            );
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
        let branding = self.clone();
        tokio::spawn(async move {
            let mut consecutive_failures: u32 = 0;
            loop {
                let wait = match branding.refresh(&client, &upstream_url).await {
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
                            "instance branding refresh failed, keeping the previous branding"
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

    fn seeded_with(origins: &[&str]) -> BrandingSnapshot {
        BrandingSnapshot {
            image_origins: origins.iter().map(|origin| endpoint(origin)).collect(),
            ..BrandingSnapshot::default()
        }
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
            sources(&snapshot_from_discovery(&discovery).unwrap().image_origins),
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

        assert!(
            snapshot_from_discovery(&discovery)
                .unwrap()
                .image_origins
                .is_empty()
        );
    }

    #[test]
    fn discovery_endpoints_yield_the_instance_origins_the_page_must_reach() {
        let discovery = serde_json::json!({
            "endpoints": {
                "api": "https://chat.example.com/api",
                "api_client": "https://chat.example.com/api",
                "gateway": "wss://chat.example.com/gateway",
                "media": "https://chat.example.com/media",
                "static_cdn": "https://static.example.com",
                "marketing": "https://www.example.com",
            },
            "app_public": {"branding": {}},
        });
        let sources = snapshot_from_discovery(&discovery)
            .unwrap()
            .instance_sources;
        assert_eq!(
            sources.connect_origins,
            vec![
                "https://chat.example.com",
                "wss://chat.example.com",
                "https://static.example.com",
            ]
        );
        assert_eq!(
            sources
                .media_endpoint
                .as_ref()
                .map(HttpEndpoint::csp_origin),
            Some("https://chat.example.com")
        );
        assert_eq!(
            sources
                .static_cdn_endpoint
                .as_ref()
                .map(HttpEndpoint::csp_origin),
            Some("https://static.example.com")
        );
    }

    #[test]
    fn a_document_without_branding_is_not_a_discovery_snapshot() {
        assert!(snapshot_from_discovery(&serde_json::json!({"api_code_version": "v1"})).is_none());
        assert!(snapshot_from_discovery(&serde_json::json!({"app_public": {}})).is_none());
        assert!(
            snapshot_from_discovery(&serde_json::json!({"app_public": {"branding": null}}))
                .is_none()
        );
        assert_eq!(
            snapshot_from_discovery(&serde_json::json!({"app_public": {"branding": {}}})),
            Some(BrandingSnapshot::default())
        );
    }

    #[test]
    fn the_favicon_falls_back_to_the_icon_like_the_app() {
        let favicon = |branding: serde_json::Value| {
            snapshot_from_discovery(&serde_json::json!({"app_public": {"branding": branding}}))
                .unwrap()
                .favicon_url
                .map(|url| url.to_string())
        };

        assert_eq!(
            favicon(serde_json::json!({
                "icon_url": "https://example.test/icon.png",
                "favicon_url": " https://example.test/favicon.png "
            })),
            Some("https://example.test/favicon.png".to_owned())
        );
        assert_eq!(
            favicon(serde_json::json!({
                "icon_url": "https://example.test/icon.png",
                "favicon_url": null
            })),
            Some("https://example.test/icon.png".to_owned())
        );
        assert_eq!(
            favicon(serde_json::json!({"icon_url": null, "favicon_url": null})),
            None
        );
        assert_eq!(
            favicon(serde_json::json!({"favicon_url": "javascript:alert(1)"})),
            None
        );
    }

    #[test]
    fn environment_branding_images_yield_their_distinct_origins() {
        let snapshot = snapshot_from_environment(|name| match name {
            "FLUXER_APP_ICON_URL" => Some("https://example.test/logo.png".to_owned()),
            "FLUXER_APP_FAVICON_URL" => Some("https://example.test/favicon.png".to_owned()),
            "FLUXER_APP_WORDMARK_URL" => Some("https://cdn.example.test/wordmark.svg".to_owned()),
            "FLUXER_APP_LOGO_URL" => Some("not a url".to_owned()),
            "FLUXER_APP_STATUS_PAGE_URL" => Some("https://status.example.test".to_owned()),
            _ => None,
        });

        assert_eq!(
            sources(&snapshot.image_origins),
            ["https://example.test", "https://cdn.example.test"]
        );
    }

    #[test]
    fn a_theme_colour_is_normalised_like_the_api() {
        assert_eq!(theme_color("#7B2CE4").as_deref(), Some("#7b2ce4"));
        assert_eq!(theme_color(" abc ").as_deref(), Some("#aabbcc"));
        assert_eq!(theme_color("#000001").as_deref(), Some("#000001"));
        for unset in [
            "#000000",
            "#000",
            "",
            "#12345",
            "##abc",
            "rgb(1, 2, 3)",
            "rebeccapurple",
            "#abcdeg",
            "\"><script>",
        ] {
            assert_eq!(theme_color(unset), None, "{unset} is not a theme colour");
        }
    }

    #[test]
    fn an_icon_without_a_network_origin_is_not_served() {
        let snapshot = snapshot_from_environment(|name| match name {
            "FLUXER_APP_ICON_URL" => Some("javascript:alert(1)".to_owned()),
            "FLUXER_APP_FAVICON_URL" => Some("/branding/favicon.png".to_owned()),
            _ => None,
        });

        assert_eq!(snapshot.icon_url, None);
        assert_eq!(snapshot.favicon_url, None);
    }

    #[test]
    fn only_a_self_hosted_instance_seeds_origins_from_its_environment() {
        let mut config = AppProxyConfig::from_env();
        config.branding = seeded_with(&["https://example.test"]);

        config.self_hosted = false;
        assert!(
            InstanceBranding::from_config(&config)
                .image_origins()
                .is_empty()
        );

        config.self_hosted = true;
        assert_eq!(
            sources(&InstanceBranding::from_config(&config).image_origins()),
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
        let origins = InstanceBranding::new(seeded_with(&["https://env.example.test"]));
        assert_eq!(
            sources(&origins.image_origins()),
            ["https://env.example.test"]
        );

        origins.refresh(&fast_client(), &upstream).await.unwrap();

        assert_eq!(
            sources(&origins.image_origins()),
            ["https://dashboard.example.test"]
        );
        assert_eq!(
            origins.snapshot().favicon_url.as_deref(),
            Some("https://dashboard.example.test/icon.png")
        );
    }

    #[tokio::test]
    async fn a_failed_refresh_keeps_the_previous_origins() {
        let seeded = || InstanceBranding::new(seeded_with(&["https://env.example.test"]));

        let origins = seeded();
        let unavailable =
            serve_discovery(vec![(StatusCode::SERVICE_UNAVAILABLE, String::new())]).await;
        assert!(origins.refresh(&fast_client(), &unavailable).await.is_err());
        assert_eq!(
            sources(&origins.image_origins()),
            ["https://env.example.test"]
        );

        let origins = seeded();
        let not_json = serve_discovery(vec![(StatusCode::OK, "<html>".to_owned())]).await;
        assert!(origins.refresh(&fast_client(), &not_json).await.is_err());
        assert_eq!(
            sources(&origins.image_origins()),
            ["https://env.example.test"]
        );

        let origins = seeded();
        let no_branding = serve_discovery(vec![(StatusCode::OK, "{}".to_owned())]).await;
        assert!(origins.refresh(&fast_client(), &no_branding).await.is_err());
        assert_eq!(
            sources(&origins.image_origins()),
            ["https://env.example.test"]
        );

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
        assert_eq!(
            sources(&origins.image_origins()),
            ["https://env.example.test"]
        );

        let origins = seeded();
        assert!(
            origins
                .refresh(&fast_client(), "http://127.0.0.1:1/.well-known/fluxer")
                .await
                .is_err()
        );
        assert_eq!(
            sources(&origins.image_origins()),
            ["https://env.example.test"]
        );
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
        let origins = InstanceBranding::new(BrandingSnapshot::default());
        let mut changes = origins.0.subscribe();

        let refresh =
            origins.spawn_refresh_loop(fast_client(), upstream, Duration::from_millis(10));

        changes.changed().await.unwrap();
        assert_eq!(
            sources(&origins.image_origins()),
            ["https://first.example.test"]
        );
        changes.changed().await.unwrap();
        assert_eq!(
            sources(&origins.image_origins()),
            ["https://second.example.test"]
        );
        refresh.abort();
    }
}
