// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};
use base64::{Engine, engine::general_purpose::URL_SAFE_NO_PAD};
use clap::Args;
use hmac::{Hmac, KeyInit, Mac};
use sha2::Sha256;

#[derive(Debug, Clone, Args)]
pub struct SignExternalUrlArgs {
    #[arg(long)]
    pub secret_key: String,
    #[arg(long)]
    pub server_url: String,
    pub upstream: String,
}

pub fn sign_external_url(secret_key: &str, server_url: &str, upstream: &str) -> Result<String> {
    let path = format!("v2/{}", URL_SAFE_NO_PAD.encode(upstream.as_bytes()));
    let mut mac = Hmac::<Sha256>::new_from_slice(secret_key.as_bytes())
        .context("failed to create HMAC signer")?;
    mac.update(path.as_bytes());
    let signature = URL_SAFE_NO_PAD.encode(mac.finalize().into_bytes());
    Ok(format!(
        "{}/external/{}/{}",
        server_url.trim_end_matches('/'),
        signature,
        path
    ))
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
        let parts = signed.split('/').collect::<Vec<_>>();
        assert!(signed.starts_with("http://127.0.0.1:19110/external/"));
        assert_eq!(parts[5], "v2");
        assert_eq!(
            URL_SAFE_NO_PAD.decode(parts[6]).unwrap(),
            b"https://example.test/a b.jpg"
        );
        assert_eq!(URL_SAFE_NO_PAD.decode(parts[4]).unwrap().len(), 32);
    }
}
