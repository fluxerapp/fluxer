// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_svc::config::optional_env;
use url::Url;

#[derive(Clone)]
pub struct MediaProxyUrlBuilder {
    endpoint: String,
    endpoint_host: Option<String>,
    secret_key: String,
}

impl MediaProxyUrlBuilder {
    #[cfg(test)]
    pub(crate) fn for_test(endpoint: &str, secret_key: &str) -> Self {
        let endpoint = endpoint.trim_end_matches('/').to_owned();
        let endpoint_host = Url::parse(&endpoint)
            .ok()
            .and_then(|parsed| parsed.host_str().map(ToOwned::to_owned));

        Self {
            endpoint,
            endpoint_host,
            secret_key: secret_key.to_owned(),
        }
    }

    pub fn from_env() -> anyhow::Result<Self> {
        let Some(endpoint) = optional_env("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT")
            .or_else(|| optional_env("FLUXER_MEDIA_ENDPOINT"))
        else {
            anyhow::bail!(
                "gifs shard requires FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT or FLUXER_MEDIA_ENDPOINT"
            );
        };

        let Some(secret_key) = optional_env("FLUXER_MEDIA_PROXY_SECRET_KEY") else {
            anyhow::bail!("gifs shard requires FLUXER_MEDIA_PROXY_SECRET_KEY");
        };

        let endpoint = fluxer_common::config::normalize_public_endpoint_from_env(
            endpoint.trim_end_matches('/'),
        );
        let endpoint_host = Url::parse(&endpoint)
            .ok()
            .and_then(|parsed| parsed.host_str().map(ToOwned::to_owned));

        Ok(Self {
            endpoint,
            endpoint_host,
            secret_key,
        })
    }

    pub fn external_proxy_url(&self, input_url: &str) -> Option<String> {
        let parsed = Url::parse(input_url).ok()?;
        if self
            .endpoint_host
            .as_deref()
            .is_some_and(|host| parsed.host_str() == Some(host))
        {
            return Some(input_url.to_owned());
        }

        fluxer_common::external_media_path::build_external_media_proxy_url(
            &self.endpoint,
            parsed.as_str(),
            self.secret_key.as_bytes(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn external_proxy_url_builds_v2_signed_url() {
        let builder = MediaProxyUrlBuilder {
            endpoint: "https://media.example.test".to_owned(),
            endpoint_host: Some("media.example.test".to_owned()),
            secret_key: "secret".to_owned(),
        };

        let url = builder
            .external_proxy_url("https://img.klipy.com/a.webp?x=1")
            .expect("proxy url");

        assert!(url.starts_with("https://media.example.test/external/"));
        assert!(url.contains("/https/"));
        assert_eq!(
            builder.external_proxy_url("https://media.example.test/external/existing"),
            Some("https://media.example.test/external/existing".to_owned())
        );
    }
}
