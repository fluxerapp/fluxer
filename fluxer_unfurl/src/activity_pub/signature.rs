// SPDX-License-Identifier: AGPL-3.0-or-later

use base64::prelude::*;
use chrono::{DateTime, Utc};
use reqwest::header::{DATE, HOST, HeaderMap, HeaderName, HeaderValue};
use ring::rand::SystemRandom;
use ring::signature::{RSA_PKCS1_SHA256, RsaKeyPair};
use std::sync::Arc;
use std::time::SystemTime;
use url::Url;

const SIGNATURE: HeaderName = HeaderName::from_static("signature");

pub fn parse_signing_key(encoded: &str) -> anyhow::Result<RsaKeyPair> {
    let compact: String = encoded
        .chars()
        .filter(|c| !c.is_ascii_whitespace())
        .collect();
    let der = BASE64_STANDARD.decode(compact)?;
    RsaKeyPair::from_pkcs8(&der)
        .or_else(|_| RsaKeyPair::from_der(&der))
        .map_err(|err| anyhow::anyhow!("invalid RSA private key: {err}"))
}

pub fn public_key_base64(key: &RsaKeyPair) -> String {
    BASE64_STANDARD.encode(key.public().as_ref())
}

#[derive(Clone)]
pub struct HostKey {
    host: String,
    key: Arc<RsaKeyPair>,
    key_id: String,
}

impl HostKey {
    pub fn new(key: RsaKeyPair, key_id: String) -> anyhow::Result<Self> {
        let host = Url::parse(&key_id)?
            .host_str()
            .ok_or_else(|| anyhow::anyhow!("key id has no host"))?
            .to_owned();
        Ok(Self {
            host,
            key: Arc::new(key),
            key_id,
        })
    }
}

#[derive(Clone)]
pub struct RequestSigner {
    key: Arc<RsaKeyPair>,
    key_id: String,
    host_key: Option<HostKey>,
}

impl RequestSigner {
    pub fn new(key: Arc<RsaKeyPair>, key_id: String, host_key: Option<HostKey>) -> Self {
        Self {
            key,
            key_id,
            host_key,
        }
    }

    pub fn headers(&self, url: &Url, at: SystemTime) -> anyhow::Result<HeaderMap> {
        let host_name = url
            .host_str()
            .ok_or_else(|| anyhow::anyhow!("signed request URL has no host"))?;
        let host = match url.port() {
            Some(port) => format!("{host_name}:{port}"),
            None => host_name.to_owned(),
        };
        let date = DateTime::<Utc>::from(at)
            .format("%a, %d %b %Y %H:%M:%S GMT")
            .to_string();
        let target = match url.query() {
            Some(query) => format!("{}?{query}", url.path()),
            None => url.path().to_owned(),
        };
        let (key, key_id) = match &self.host_key {
            Some(host_key) if host_key.host == host_name => (&host_key.key, &host_key.key_id),
            _ => (&self.key, &self.key_id),
        };
        let signing_string = format!("(request-target): get {target}\nhost: {host}\ndate: {date}");
        let mut signature = vec![0; key.public().modulus_len()];
        key.sign(
            &RSA_PKCS1_SHA256,
            &SystemRandom::new(),
            signing_string.as_bytes(),
            &mut signature,
        )
        .map_err(|_| anyhow::anyhow!("failed to sign ActivityPub request"))?;
        let signature_header = format!(
            "keyId=\"{}\",algorithm=\"rsa-sha256\",headers=\"(request-target) host date\",signature=\"{}\"",
            key_id,
            BASE64_STANDARD.encode(signature)
        );

        let mut headers = HeaderMap::new();
        headers.insert(HOST, HeaderValue::from_str(&host)?);
        headers.insert(DATE, HeaderValue::from_str(&date)?);
        headers.insert(SIGNATURE, HeaderValue::from_str(&signature_header)?);
        Ok(headers)
    }
}
