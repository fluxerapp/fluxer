// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{AppProxyConfig, HttpEndpoint};
use crate::csp::InstanceCspSources;
use axum::http::header;
use reqwest::Url;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{Mutex, watch};
use tokio::time::Instant;

const BRANDING_IMAGES: [(&str, &str); 7] = [
    ("icon_url", "FLUXER_APP_ICON_URL"),
    ("symbol_url", "FLUXER_APP_SYMBOL_URL"),
    ("logo_url", "FLUXER_APP_LOGO_URL"),
    ("wordmark_url", "FLUXER_APP_WORDMARK_URL"),
    ("favicon_url", "FLUXER_APP_FAVICON_URL"),
    FAVICON_DARK_URL,
    ("sign_in_backdrop_url", "FLUXER_APP_SIGN_IN_BACKDROP_URL"),
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
const SIGN_IN_LOGOS: [(&str, &str); 3] = [
    ("logo_url", "FLUXER_APP_LOGO_URL"),
    ICON_URL,
    ("symbol_url", "FLUXER_APP_SYMBOL_URL"),
];
const DEFAULT_PRODUCT_NAME: &str = "Fluxer";
const FAVICON_URL: (&str, &str) = ("favicon_url", "FLUXER_APP_FAVICON_URL");
const FAVICON_DARK_URL: (&str, &str) = ("favicon_dark_url", "FLUXER_APP_FAVICON_DARK_URL");
const THEME_COLOR: (&str, &str) = ("theme_color", "FLUXER_APP_THEME_COLOR");
const STATUS_PAGE_URL: &str = "FLUXER_APP_STATUS_PAGE_URL";
const DISCOVERY_FETCH_TIMEOUT: Duration = Duration::from_secs(2);
const DISCOVERY_BODY_BYTES_MAX: usize = 1024 * 1024;
const REFRESH_INTERVAL_MIN: Duration = Duration::from_secs(1);
const REFRESH_BACKOFF_MAX: Duration = Duration::from_secs(300);

pub fn snapshot_from_environment<F>(mut read_var: F) -> BrandingSnapshot
where
    F: FnMut(&str) -> Option<String>,
{
    let status_page_origin = read_var(STATUS_PAGE_URL)
        .as_deref()
        .map(str::trim)
        .and_then(connect_origin);
    BrandingSnapshot {
        instance_sources: InstanceCspSources {
            connect_origins: status_page_origin.into_iter().collect(),
            ..InstanceCspSources::default()
        },
        ..snapshot_from_fields(|(_, name)| read_var(name))
    }
}

pub fn snapshot_from_discovery(discovery: &serde_json::Value) -> Option<BrandingSnapshot> {
    let branding = discovery.pointer("/app_public/branding")?.as_object()?;
    Some(BrandingSnapshot {
        instance_sources: instance_sources_from_discovery(discovery),
        custom_css: non_empty(
            branding
                .get("custom_css")
                .and_then(serde_json::Value::as_str)
                .map(str::to_owned),
        )
        .map(Into::into),
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
    if let Some(origin) = discovery
        .pointer("/app_public/branding/status_page_url")
        .and_then(serde_json::Value::as_str)
        .map(str::trim)
        .and_then(connect_origin)
        && !connect_origins.contains(&origin)
    {
        connect_origins.push(origin);
    }
    for origin in voice_origins(discovery) {
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

fn voice_origins(discovery: &serde_json::Value) -> Vec<String> {
    let Some(values) = discovery
        .pointer("/voice_origins")
        .and_then(serde_json::Value::as_array)
    else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for origin in values
        .iter()
        .filter_map(serde_json::Value::as_str)
        .filter_map(|value| connect_origin(value.trim()))
    {
        let twin = origin
            .strip_prefix("wss://")
            .map(|rest| format!("https://{rest}"))
            .or_else(|| {
                origin
                    .strip_prefix("ws://")
                    .map(|rest| format!("http://{rest}"))
            });
        out.push(origin);
        out.extend(twin);
    }
    out
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
    let product_name = non_empty(field(PRODUCT_NAME));
    let has_sign_in_logo = SIGN_IN_LOGOS
        .iter()
        .any(|&logo| non_empty(field(logo)).is_some());
    BrandingSnapshot {
        image_origins: image_origins.into(),
        instance_sources: InstanceCspSources::default(),
        auth_card_narrow: !has_sign_in_logo
            && product_name
                .as_deref()
                .is_some_and(|name| name != DEFAULT_PRODUCT_NAME),
        product_name: product_name.map(Into::into),
        icon_url: icon_url.as_deref().and_then(absolute_url).map(Into::into),
        favicon_url: favicon_url
            .as_deref()
            .and_then(absolute_url)
            .map(Into::into),
        favicon_dark_url: non_empty(field(FAVICON_DARK_URL))
            .as_deref()
            .and_then(absolute_url)
            .map(Into::into),
        theme_color: field(THEME_COLOR)
            .as_deref()
            .and_then(theme_color)
            .map(Into::into),
        custom_css: None,
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
    pub auth_card_narrow: bool,
    pub product_name: Option<Arc<str>>,
    pub icon_url: Option<Arc<str>>,
    pub favicon_url: Option<Arc<str>>,
    pub favicon_dark_url: Option<Arc<str>>,
    pub theme_color: Option<Arc<str>>,
    pub custom_css: Option<Arc<str>>,
}

#[derive(Debug, Default)]
struct RefreshGate {
    next_attempt: Option<Instant>,
    consecutive_failures: u32,
}

#[derive(Clone, Debug)]
pub struct InstanceBranding {
    current: watch::Sender<Arc<BrandingSnapshot>>,
    gate: Arc<Mutex<RefreshGate>>,
}

impl InstanceBranding {
    pub fn new(snapshot: BrandingSnapshot) -> Self {
        Self {
            current: watch::Sender::new(Arc::new(snapshot)),
            gate: Arc::default(),
        }
    }

    pub fn from_config(config: &AppProxyConfig) -> Self {
        if config.self_hosted {
            Self::new(config.branding.clone())
        } else {
            Self::new(BrandingSnapshot::default())
        }
    }

    pub fn snapshot(&self) -> Arc<BrandingSnapshot> {
        self.current.borrow().clone()
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
        let changed = self.current.send_if_modified(|current| {
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
                favicon_dark_url = snapshot.favicon_dark_url.as_deref(),
                theme_color = snapshot.theme_color.as_deref(),
                custom_css_bytes = snapshot.custom_css.as_deref().map_or(0, str::len),
                "instance branding updated"
            );
        }
        Ok(())
    }

    pub fn refresh_if_due(&self, client: &reqwest::Client, upstream_url: &str, interval: Duration) {
        let Ok(mut gate) = self.gate.clone().try_lock_owned() else {
            return;
        };
        if gate
            .next_attempt
            .is_some_and(|next_attempt| Instant::now() < next_attempt)
        {
            return;
        }
        let branding = self.clone();
        let client = client.clone();
        let upstream_url = upstream_url.to_owned();
        let interval = interval.max(REFRESH_INTERVAL_MIN);
        tokio::spawn(async move {
            let wait = match branding.refresh(&client, &upstream_url).await {
                Ok(()) => {
                    gate.consecutive_failures = 0;
                    interval
                }
                Err(error) => {
                    let backoff = interval
                        .saturating_mul(1 << gate.consecutive_failures.min(5))
                        .min(REFRESH_BACKOFF_MAX);
                    gate.consecutive_failures = gate.consecutive_failures.saturating_add(1);
                    tracing::warn!(
                        %error,
                        url = %upstream_url,
                        consecutive_failures = gate.consecutive_failures,
                        backoff_ms = backoff.as_millis() as u64,
                        "instance branding refresh failed, keeping the previous branding"
                    );
                    backoff
                }
            };
            gate.next_attempt = Some(Instant::now() + wait);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn voice_origins_from_discovery_join_connect_sources_with_their_https_twins() {
        let discovery = serde_json::json!({
            "endpoints": {"api": "https://chat.example.com/api"},
            "voice_origins": [
                "wss://voice-fra.chat.example.com",
                "wss://203.0.113.7:8443",
                "not a url",
                42
            ]
        });
        let sources = instance_sources_from_discovery(&discovery);
        for origin in [
            "wss://voice-fra.chat.example.com",
            "https://voice-fra.chat.example.com",
            "wss://203.0.113.7:8443",
            "https://203.0.113.7:8443",
        ] {
            assert!(
                sources.connect_origins.iter().any(|o| o == origin),
                "{origin} missing from {:?}",
                sources.connect_origins
            );
        }
        assert!(
            !sources
                .connect_origins
                .iter()
                .any(|o| o.contains("not a url"))
        );
    }

    #[test]
    fn a_server_that_leaves_discovery_leaves_the_connect_sources() {
        let with = serde_json::json!({"voice_origins": ["wss://voice-old.example.com"]});
        let without = serde_json::json!({"voice_origins": []});
        assert!(
            instance_sources_from_discovery(&with)
                .connect_origins
                .contains(&"wss://voice-old.example.com".to_owned())
        );
        assert!(
            instance_sources_from_discovery(&without)
                .connect_origins
                .is_empty()
        );
    }
}
