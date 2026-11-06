// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod google;
pub mod service;
pub mod verifier;

use anyhow::Context;
use base64::Engine;
use base64::engine::general_purpose::{GeneralPurpose, GeneralPurposeConfig};
use base64::engine::{DecodePaddingMode, general_purpose};
use fluxer_svc::config::optional_env;
use serde::{Deserialize, Serialize};

const PACKAGE_NAMES_ENV: &str = "FLUXER_PLAY_INTEGRITY_PACKAGE_NAMES";
const CERT_SHA256_ENV: &str = "FLUXER_PLAY_INTEGRITY_CERT_SHA256";
const SERVICE_ACCOUNT_ENV: &str = "FLUXER_PLAY_INTEGRITY_SERVICE_ACCOUNT_JSON_PATH";
const DEVICE_INTEGRITY_ENV: &str = "FLUXER_PLAY_INTEGRITY_DEVICE_INTEGRITY";
const DEFAULT_TOKEN_URI: &str = "https://oauth2.googleapis.com/token";
const MAX_PACKAGE_NAME_LEN: usize = 255;
const SHA256_LEN: usize = 32;

pub const URL_SAFE_ANY_PADDING: GeneralPurpose = GeneralPurpose::new(
    &base64::alphabet::URL_SAFE,
    GeneralPurposeConfig::new().with_decode_padding_mode(DecodePaddingMode::Indifferent),
);

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum DeviceIntegrity {
    #[default]
    Device,
    Strong,
}

#[derive(Clone, Default, PartialEq, Eq)]
pub struct ServiceAccount {
    pub client_email: String,
    pub private_key: String,
    pub token_uri: String,
}

impl std::fmt::Debug for ServiceAccount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ServiceAccount")
            .field("client_email", &self.client_email)
            .field("token_uri", &self.token_uri)
            .finish_non_exhaustive()
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PlayIntegrityConfig {
    pub package_names: Vec<String>,
    pub cert_digests: Vec<[u8; SHA256_LEN]>,
    pub device_integrity: DeviceIntegrity,
    pub service_account: Option<ServiceAccount>,
}

#[derive(Deserialize)]
struct ServiceAccountFile {
    client_email: Option<String>,
    private_key: Option<String>,
    token_uri: Option<String>,
}

impl PlayIntegrityConfig {
    pub fn from_env() -> anyhow::Result<Self> {
        let service_account = optional_env(SERVICE_ACCOUNT_ENV)
            .filter(|path| !path.trim().is_empty())
            .map(|path| {
                std::fs::read_to_string(path.trim())
                    .with_context(|| format!("{SERVICE_ACCOUNT_ENV} could not be read: {path}"))
            })
            .transpose()?;
        Self::parse(
            optional_env(PACKAGE_NAMES_ENV).as_deref(),
            optional_env(CERT_SHA256_ENV).as_deref(),
            optional_env(DEVICE_INTEGRITY_ENV).as_deref(),
            service_account.as_deref(),
        )
    }

    pub fn parse(
        package_names: Option<&str>,
        cert_digests: Option<&str>,
        device_integrity: Option<&str>,
        service_account_json: Option<&str>,
    ) -> anyhow::Result<Self> {
        let package_names = split_list(package_names)
            .map(|name| {
                if is_valid_package_name(name) {
                    Ok(name.to_owned())
                } else {
                    Err(anyhow::anyhow!(
                        "{PACKAGE_NAMES_ENV} entry {name:?} is not an Android package name"
                    ))
                }
            })
            .collect::<anyhow::Result<Vec<_>>>()?;
        let cert_digests = split_list(cert_digests)
            .map(|digest| {
                parse_cert_digest(digest).ok_or_else(|| {
                    anyhow::anyhow!(
                        "{CERT_SHA256_ENV} entry {digest:?} is not a SHA-256 digest in base64url or hex"
                    )
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;
        let device_integrity = match device_integrity
            .map(|value| value.trim().to_ascii_lowercase())
            .as_deref()
        {
            None | Some("" | "device") => DeviceIntegrity::Device,
            Some("strong") => DeviceIntegrity::Strong,
            Some(other) => anyhow::bail!("{DEVICE_INTEGRITY_ENV} has an invalid value {other:?}"),
        };
        if package_names.is_empty() {
            return Ok(Self {
                device_integrity,
                ..Self::default()
            });
        }
        if cert_digests.is_empty() {
            anyhow::bail!("{CERT_SHA256_ENV} is required when {PACKAGE_NAMES_ENV} is set");
        }
        let service_account = parse_service_account(service_account_json.ok_or_else(|| {
            anyhow::anyhow!("{SERVICE_ACCOUNT_ENV} is required when {PACKAGE_NAMES_ENV} is set")
        })?)?;
        Ok(Self {
            package_names,
            cert_digests,
            device_integrity,
            service_account: Some(service_account),
        })
    }

    pub fn is_enabled(&self) -> bool {
        !self.package_names.is_empty() && self.service_account.is_some()
    }

    pub fn allows_package(&self, package_name: &str) -> bool {
        self.package_names.iter().any(|name| name == package_name)
    }
}

fn split_list(value: Option<&str>) -> impl Iterator<Item = &str> {
    value
        .unwrap_or_default()
        .split(',')
        .map(str::trim)
        .filter(|entry| !entry.is_empty())
}

fn parse_service_account(json: &str) -> anyhow::Result<ServiceAccount> {
    let file: ServiceAccountFile = serde_json::from_str(json)
        .map_err(|_| anyhow::anyhow!("{SERVICE_ACCOUNT_ENV} is not a service account JSON file"))?;
    let client_email = file
        .client_email
        .filter(|email| !email.is_empty())
        .ok_or_else(|| anyhow::anyhow!("{SERVICE_ACCOUNT_ENV} has no client_email"))?;
    let private_key = file
        .private_key
        .filter(|key| !key.is_empty())
        .ok_or_else(|| anyhow::anyhow!("{SERVICE_ACCOUNT_ENV} has no private_key"))?;
    google::parse_rsa_pkcs8_pem(&private_key)
        .ok_or_else(|| anyhow::anyhow!("{SERVICE_ACCOUNT_ENV} private_key is not an RSA key"))?;
    let token_uri = file
        .token_uri
        .filter(|uri| uri.starts_with("https://"))
        .unwrap_or_else(|| DEFAULT_TOKEN_URI.to_owned());
    Ok(ServiceAccount {
        client_email,
        private_key,
        token_uri,
    })
}

pub fn is_valid_package_name(name: &str) -> bool {
    name.len() <= MAX_PACKAGE_NAME_LEN
        && name.split('.').count() >= 2
        && name.split('.').all(|segment| {
            let mut bytes = segment.bytes();
            bytes
                .next()
                .is_some_and(|first| first.is_ascii_alphabetic())
                && bytes.all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
        })
}

pub fn parse_cert_digest(value: &str) -> Option<[u8; SHA256_LEN]> {
    let hex = value.replace(':', "");
    if hex.len() == SHA256_LEN * 2 && hex.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        let mut digest = [0u8; SHA256_LEN];
        for (index, byte) in digest.iter_mut().enumerate() {
            *byte = u8::from_str_radix(&hex[index * 2..index * 2 + 2], 16).ok()?;
        }
        return Some(digest);
    }
    URL_SAFE_ANY_PADDING
        .decode(value)
        .or_else(|_| general_purpose::STANDARD.decode(value))
        .ok()?
        .try_into()
        .ok()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlayIntegrityRejection {
    NotConfigured,
    InvalidRequest,
    InvalidChallenge,
    InvalidToken,
    AppNotRecognized,
    DeviceNotTrusted,
    Unavailable,
}
