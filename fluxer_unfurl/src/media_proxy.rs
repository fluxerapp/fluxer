// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};
use std::time::Duration;
use url::Url;

const METADATA_TIMEOUT: Duration = Duration::from_secs(5);

pub struct MediaProxyClient {
    http_client: reqwest::Client,
    endpoint: String,
    public_endpoint: String,
    secret_key: String,
}

#[derive(Debug, Clone, Serialize)]
struct MetadataRequest<'a> {
    #[serde(rename = "type")]
    req_type: &'a str,
    url: &'a str,
    version: u8,
    nsfw: &'a str,
}

#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)]
pub struct MediaMetadata {
    pub format: String,
    pub content_type: String,
    pub content_hash: String,
    pub size: u64,
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub duration: Option<f64>,
    pub placeholder: Option<String>,
    pub animated: Option<bool>,
    pub nsfw: bool,
    pub nsfw_probability: Option<f64>,
}

pub fn embed_media_flags(meta: &MediaMetadata) -> u32 {
    let mut flags = 0;
    if meta.nsfw {
        flags |= 1 << 4;
    }
    if meta.animated.unwrap_or(false) {
        flags |= 1 << 5;
    }
    flags
}

impl MediaProxyClient {
    pub fn new_with_public_endpoint(
        endpoint: &str,
        secret_key: &str,
        public_endpoint: Option<&str>,
        http_client: reqwest::Client,
    ) -> Self {
        Self {
            http_client,
            endpoint: endpoint.trim_end_matches('/').to_owned(),
            public_endpoint: public_endpoint
                .unwrap_or(endpoint)
                .trim_end_matches('/')
                .to_owned(),
            secret_key: secret_key.to_owned(),
        }
    }

    pub async fn get_metadata(&self, url: &str, nsfw_mode: &str) -> anyhow::Result<MediaMetadata> {
        let body = MetadataRequest {
            req_type: "external",
            url,
            version: 2,
            nsfw: nsfw_mode,
        };

        let resp = self
            .http_client
            .post(format!("{}/_metadata", self.endpoint))
            .header("content-type", "application/json")
            .bearer_auth(&self.secret_key)
            .timeout(METADATA_TIMEOUT)
            .json(&body)
            .send()
            .await;

        let resp =
            resp.map_err(|err| anyhow::anyhow!("media proxy request failed for {url}: {err}"))?;

        if !resp.status().is_success() {
            let status = resp.status();
            let body = resp.text().await.unwrap_or_default();
            let snippet: String = body.chars().take(256).collect();
            return Err(anyhow::anyhow!(
                "media proxy metadata request failed for {url}: status={} body={}",
                status.as_u16(),
                snippet
            ));
        }

        resp.json::<MediaMetadata>()
            .await
            .map_err(|err| anyhow::anyhow!("failed to parse media proxy metadata for {url}: {err}"))
    }

    pub fn nsfw_mode_str(mode: crate::types::NsfwMode) -> &'static str {
        match mode {
            crate::types::NsfwMode::Block => "block",
            crate::types::NsfwMode::Flag => "flag",
            crate::types::NsfwMode::Allow => "allow",
        }
    }

    pub fn external_proxy_url(&self, input_url: &str) -> Option<String> {
        if input_url == self.public_endpoint
            || input_url.starts_with(&format!("{}/", self.public_endpoint))
        {
            return Some(input_url.to_owned());
        }
        let parsed = Url::parse(input_url).ok()?;
        fluxer_common::external_media_path::build_external_media_proxy_url(
            &self.public_endpoint,
            parsed.as_str(),
            self.secret_key.as_bytes(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn endpoint_trailing_slash_stripped() {
        let client = reqwest::Client::new();
        let mp = MediaProxyClient::new_with_public_endpoint(
            "http://localhost:8000/",
            "secret",
            None,
            client,
        );
        assert_eq!(mp.endpoint, "http://localhost:8000");
        assert_eq!(mp.public_endpoint, "http://localhost:8000");
    }

    #[test]
    fn nsfw_mode_strings() {
        assert_eq!(
            MediaProxyClient::nsfw_mode_str(crate::types::NsfwMode::Block),
            "block"
        );
        assert_eq!(
            MediaProxyClient::nsfw_mode_str(crate::types::NsfwMode::Allow),
            "allow"
        );
        assert_eq!(
            MediaProxyClient::nsfw_mode_str(crate::types::NsfwMode::Flag),
            "flag"
        );
    }

    #[test]
    fn external_proxy_url_uses_public_endpoint_and_plain_path() {
        let client = reqwest::Client::new();
        let mp = MediaProxyClient::new_with_public_endpoint(
            "http://media-proxy:8080/",
            "secret",
            Some("https://media.example.test/"),
            client,
        );
        let proxy = mp
            .external_proxy_url("https://pbs.twimg.com/media/a.jpg?name=orig")
            .expect("proxy url");

        assert!(proxy.starts_with("https://media.example.test/external/"));
        assert!(proxy.contains("/https/"));

        let path = proxy
            .split_once("/external/")
            .expect("external segment")
            .1
            .split_once('/')
            .expect("signature segment")
            .1;
        assert_eq!(
            "https://pbs.twimg.com/media/a.jpg?name=orig",
            fluxer_common::external_media_path::reconstruct_original_url(path).expect("decodes")
        );
        assert_eq!(
            mp.external_proxy_url("https://media.example.test/external/already"),
            Some("https://media.example.test/external/already".to_owned())
        );
    }
}
