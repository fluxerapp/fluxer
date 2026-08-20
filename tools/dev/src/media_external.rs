// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};
use clap::Args;

#[derive(Debug, Clone, Args)]
pub struct SignExternalUrlArgs {
    #[arg(long)]
    pub secret_key: String,
    #[arg(long)]
    pub server_url: String,
    pub upstream: String,
}

pub fn sign_external_url(secret_key: &str, server_url: &str, upstream: &str) -> Result<String> {
    fluxer_common::external_media_path::build_external_media_proxy_url(
        server_url.trim_end_matches('/'),
        upstream,
        secret_key.as_bytes(),
    )
    .context("failed to build the external media proxy url")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn signs_external_urls_with_urlsafe_components() {
        let signed = sign_external_url(
            "benchmark-secret",
            "http://127.0.0.1:19110/",
            "https://example.test/a b.jpg",
        )
        .unwrap();
        assert!(signed.starts_with("http://127.0.0.1:19110/external/"));
        assert!(signed.ends_with("/https/example.test/a%20b.jpg"));
        assert_eq!(
            "https://example.test/a b.jpg",
            fluxer_common::external_media_path::reconstruct_original_url(
                signed
                    .split_once("/external/")
                    .unwrap()
                    .1
                    .split_once('/')
                    .unwrap()
                    .1
            )
            .unwrap()
        );
    }
}
