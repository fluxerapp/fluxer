// SPDX-License-Identifier: AGPL-3.0-or-later

use moka::future::Cache;
use serde::Deserialize;
use std::time::Duration;

const FETCH_TIMEOUT: Duration = Duration::from_millis(1500);
const CACHE_TTL: Duration = Duration::from_secs(120);

#[derive(Clone, Debug, Default)]
pub struct LatestDesktopVersions {
    pub windows: Option<LatestVersionInfo>,
    pub macos: Option<LatestVersionInfo>,
    pub linux: Option<LatestVersionInfo>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct LatestVersionInfo {
    pub version: String,
    #[serde(default)]
    pub minimum_system_version: Option<String>,
}

#[derive(Clone)]
pub struct LatestVersionsCache {
    entries: Cache<String, LatestDesktopVersions>,
}

impl LatestVersionsCache {
    pub fn new() -> Self {
        Self {
            entries: Cache::builder()
                .max_capacity(8)
                .time_to_live(CACHE_TTL)
                .build(),
        }
    }
}

impl Default for LatestVersionsCache {
    fn default() -> Self {
        Self::new()
    }
}

pub async fn fetch_latest_desktop_versions_cached(
    cache: &LatestVersionsCache,
    client: &reqwest::Client,
    api_endpoint: &str,
    channel: &str,
) -> LatestDesktopVersions {
    if let Some(cached) = cache.entries.get(channel).await {
        return cached;
    }
    let fresh = fetch_latest_desktop_versions(client, api_endpoint, channel).await;
    if fresh.windows.is_some() || fresh.macos.is_some() || fresh.linux.is_some() {
        cache
            .entries
            .insert(channel.to_owned(), fresh.clone())
            .await;
    }
    fresh
}

pub async fn fetch_latest_desktop_versions(
    client: &reqwest::Client,
    api_endpoint: &str,
    channel: &str,
) -> LatestDesktopVersions {
    let windows = fetch_latest_desktop_version(client, api_endpoint, channel, "win32", "x64");
    let macos = fetch_latest_desktop_version(client, api_endpoint, channel, "darwin", "arm64");
    let linux = fetch_latest_desktop_version(client, api_endpoint, channel, "linux", "x64");
    let (windows, macos, linux) = tokio::join!(windows, macos, linux);
    LatestDesktopVersions {
        windows,
        macos,
        linux,
    }
}

pub fn format_latest_version_line(info: &LatestVersionInfo) -> String {
    format!("v{}", info.version)
}

async fn fetch_latest_desktop_version(
    client: &reqwest::Client,
    api_endpoint: &str,
    channel: &str,
    platform: &str,
    arch: &str,
) -> Option<LatestVersionInfo> {
    let url = format!(
        "{}/dl/desktop/{channel}/{platform}/{arch}/latest",
        api_endpoint.trim_end_matches('/')
    );
    let response = client
        .get(url)
        .timeout(FETCH_TIMEOUT)
        .header(reqwest::header::ACCEPT, "application/json")
        .send()
        .await
        .ok()?;
    if !response.status().is_success() {
        return None;
    }
    let body = response.bytes().await.ok()?;
    if body.is_empty() {
        return None;
    }
    let info = serde_json::from_slice::<LatestVersionInfo>(&body).ok()?;
    if info.version.is_empty() {
        None
    } else {
        Some(info)
    }
}
