// SPDX-License-Identifier: AGPL-3.0-or-later

use base64::prelude::*;
use hmac::{Hmac, KeyInit, Mac};
use sha2::Sha256;
use url::Url;

const V2_PATH_PREFIX: &str = "v2/";

#[derive(Clone)]
pub struct MediaProxyUrlBuilder {
    endpoint: String,
    endpoint_host: Option<String>,
    secret_key: String,
}

impl MediaProxyUrlBuilder {
    pub fn from_env() -> anyhow::Result<Self> {
        let endpoint = std::env::var("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT")
            .or_else(|_| std::env::var("FLUXER_MEDIA_ENDPOINT"))
            .unwrap_or_default();
        if endpoint.trim().is_empty() {
            anyhow::bail!(
                "gifs shard requires FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT or FLUXER_MEDIA_ENDPOINT"
            );
        }

        let secret_key = std::env::var("FLUXER_MEDIA_PROXY_SECRET_KEY").unwrap_or_default();
        if secret_key.trim().is_empty() {
            anyhow::bail!("gifs shard requires FLUXER_MEDIA_PROXY_SECRET_KEY");
        }

        let endpoint = endpoint.trim_end_matches('/').to_owned();
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

        let proxy_path = build_external_media_proxy_path(parsed.as_str());
        let signature = create_signature(&proxy_path, &self.secret_key);
        Some(format!(
            "{}/external/{signature}/{proxy_path}",
            self.endpoint
        ))
    }
}

fn build_external_media_proxy_path(input_url: &str) -> String {
    format!(
        "{V2_PATH_PREFIX}{}",
        BASE64_URL_SAFE_NO_PAD.encode(input_url)
    )
}

fn create_signature(input: &str, secret: &str) -> String {
    let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes()).expect("HMAC accepts any key");
    mac.update(input.as_bytes());
    BASE64_URL_SAFE_NO_PAD.encode(mac.finalize().into_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    static ENV_LOCK: Mutex<()> = Mutex::new(());

    fn with_media_proxy_env(
        vars: &[(&str, Option<&str>)],
        test: impl FnOnce() -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let _guard = ENV_LOCK.lock().unwrap();
        let keys = [
            "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT",
            "FLUXER_MEDIA_ENDPOINT",
            "FLUXER_MEDIA_PROXY_ENDPOINT",
            "FLUXER_MEDIA_PROXY_SECRET_KEY",
        ];
        let saved = keys
            .iter()
            .map(|key| (*key, std::env::var(key).ok()))
            .collect::<Vec<_>>();

        for key in keys {
            unsafe {
                std::env::remove_var(key);
            }
        }
        for (key, value) in vars {
            if let Some(value) = value {
                unsafe {
                    std::env::set_var(key, value);
                }
            }
        }

        let result = test();

        for (key, value) in saved {
            match value {
                Some(value) => unsafe {
                    std::env::set_var(key, value);
                },
                None => unsafe {
                    std::env::remove_var(key);
                },
            }
        }

        result
    }

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
        assert!(url.contains("/v2/"));
        assert_eq!(
            builder.external_proxy_url("https://media.example.test/external/existing"),
            Some("https://media.example.test/external/existing".to_owned())
        );
    }

    #[test]
    fn from_env_uses_public_endpoint_when_internal_proxy_endpoint_is_set() -> anyhow::Result<()> {
        with_media_proxy_env(
            &[
                (
                    "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT",
                    Some("https://media.example.test/"),
                ),
                (
                    "FLUXER_MEDIA_PROXY_ENDPOINT",
                    Some("http://media-proxy:8080"),
                ),
                ("FLUXER_MEDIA_PROXY_SECRET_KEY", Some("secret")),
            ],
            || {
                let builder = MediaProxyUrlBuilder::from_env()?;

                assert_eq!(builder.endpoint, "https://media.example.test");
                Ok(())
            },
        )
    }

    #[test]
    fn from_env_accepts_legacy_public_media_endpoint() -> anyhow::Result<()> {
        with_media_proxy_env(
            &[
                (
                    "FLUXER_MEDIA_ENDPOINT",
                    Some("https://media.example.test/media"),
                ),
                (
                    "FLUXER_MEDIA_PROXY_ENDPOINT",
                    Some("http://media-proxy:8080"),
                ),
                ("FLUXER_MEDIA_PROXY_SECRET_KEY", Some("secret")),
            ],
            || {
                let builder = MediaProxyUrlBuilder::from_env()?;

                assert_eq!(builder.endpoint, "https://media.example.test/media");
                Ok(())
            },
        )
    }

    #[test]
    fn from_env_rejects_internal_proxy_endpoint_without_public_endpoint() -> anyhow::Result<()> {
        with_media_proxy_env(
            &[
                (
                    "FLUXER_MEDIA_PROXY_ENDPOINT",
                    Some("http://media-proxy:8080"),
                ),
                ("FLUXER_MEDIA_PROXY_SECRET_KEY", Some("secret")),
            ],
            || {
                let err = MediaProxyUrlBuilder::from_env()
                    .err()
                    .expect("internal endpoint must not be accepted as public endpoint")
                    .to_string();

                assert!(err.contains("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT"));
                Ok(())
            },
        )
    }
}
