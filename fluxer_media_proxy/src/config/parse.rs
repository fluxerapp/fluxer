// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{BucketStyle, DeploymentMode, StorageBackend};
use crate::secret::SecretBytes;
use base64::{Engine as _, engine::general_purpose};
use std::net::IpAddr;

#[derive(Debug, Default)]
pub(super) struct EnvMap(Vec<(String, String)>);

impl EnvMap {
    pub(super) fn from_iter<I, K, V>(vars: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<String>,
        V: Into<String>,
    {
        Self(
            vars.into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        )
    }

    pub(super) fn get(&self, key: &str) -> Option<&str> {
        self.0
            .iter()
            .find_map(|(k, v)| (k == key).then_some(v.as_str()))
    }
}

fn parse_mode(raw: &str) -> Option<DeploymentMode> {
    match raw.to_ascii_lowercase().as_str() {
        "mp" => Some(DeploymentMode::Mp),
        "static" => Some(DeploymentMode::Static),
        "upload" => Some(DeploymentMode::Upload),
        _ => None,
    }
}

pub(super) fn parse_mode_env(raw: Option<&str>) -> anyhow::Result<Option<DeploymentMode>> {
    let Some(raw) = raw else {
        return Ok(None);
    };
    let raw = raw.trim();
    parse_mode(raw).map(Some).ok_or_else(|| {
        anyhow::anyhow!("FLUXER_MEDIA_PROXY_MODE must be one of: mp, static, upload")
    })
}

pub(super) fn non_empty(raw: Option<&str>) -> Option<String> {
    raw.map(str::trim)
        .filter(|value| !value.is_empty())
        .map(ToOwned::to_owned)
}

pub(super) fn parse_bucket_style(raw: Option<&str>) -> anyhow::Result<Option<BucketStyle>> {
    let Some(raw) = non_empty(raw) else {
        return Ok(None);
    };
    match raw.to_ascii_lowercase().as_str() {
        "path" => Ok(Some(BucketStyle::Path)),
        "virtual" => Ok(Some(BucketStyle::VirtualHosted)),
        "root" => Ok(Some(BucketStyle::Rooted)),
        _ => Err(anyhow::anyhow!(
            "FLUXER_S3_READ_BUCKET_STYLE must be one of: path, virtual, root"
        )),
    }
}

pub(super) fn validate_read_endpoint(endpoint: &str) -> anyhow::Result<()> {
    let parsed = url::Url::parse(endpoint)
        .map_err(|err| anyhow::anyhow!("FLUXER_S3_READ_ENDPOINT is not a valid URL: {err}"))?;
    anyhow::ensure!(
        matches!(parsed.scheme(), "http" | "https"),
        "FLUXER_S3_READ_ENDPOINT must be an http or https URL"
    );
    anyhow::ensure!(
        parsed.host_str().is_some_and(|host| !host.is_empty()),
        "FLUXER_S3_READ_ENDPOINT must include a host"
    );
    anyhow::ensure!(
        parsed.username().is_empty() && parsed.password().is_none(),
        "FLUXER_S3_READ_ENDPOINT must not contain credentials"
    );
    anyhow::ensure!(
        parsed.query().is_none() && parsed.fragment().is_none(),
        "FLUXER_S3_READ_ENDPOINT must not contain a query string or fragment"
    );
    Ok(())
}

pub(super) fn parse_storage_backend(raw: Option<&str>) -> anyhow::Result<Option<StorageBackend>> {
    let Some(raw) = raw else {
        return Ok(None);
    };
    let raw = raw.trim();
    match raw.to_ascii_lowercase().as_str() {
        "local" => Ok(Some(StorageBackend::Local)),
        "s3" => Ok(Some(StorageBackend::S3)),
        _ => Err(anyhow::anyhow!(
            "FLUXER_MEDIA_PROXY_STORAGE_BACKEND must be one of: local, s3"
        )),
    }
}

pub(super) fn decode_upload_relay_secret(
    raw: Option<&str>,
    mode: DeploymentMode,
) -> anyhow::Result<SecretBytes> {
    let Some(raw) = raw.map(str::trim).filter(|s| !s.is_empty()) else {
        anyhow::ensure!(
            mode != DeploymentMode::Upload,
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 is required in upload mode"
        );
        return Ok(SecretBytes::new(Vec::new()));
    };
    let decoded = general_purpose::STANDARD.decode(raw).map_err(|_| {
        anyhow::anyhow!("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must be base64")
    })?;
    anyhow::ensure!(
        decoded.len() >= 32,
        "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must decode to at least 32 bytes"
    );
    Ok(SecretBytes::new(decoded))
}

pub(super) fn parse_bool(var_name: &str, raw: Option<&str>) -> anyhow::Result<Option<bool>> {
    let Some(raw) = raw else {
        return Ok(None);
    };
    let raw = raw.trim();
    match raw.to_ascii_lowercase().as_str() {
        "true" | "1" | "yes" => Ok(Some(true)),
        "false" | "0" | "no" => Ok(Some(false)),
        _ => Err(anyhow::anyhow!(
            "{var_name} must be a boolean: true, false, 1, 0, yes, or no"
        )),
    }
}

pub(super) fn parse_u16(
    var_name: &str,
    raw: Option<&str>,
    default_value: u16,
) -> anyhow::Result<u16> {
    parse_number(var_name, raw, default_value, u16::MIN, u16::MAX)
}

pub(super) fn parse_u64(
    var_name: &str,
    raw: Option<&str>,
    default_value: u64,
    min_value: u64,
    max_value: u64,
) -> anyhow::Result<u64> {
    parse_number(var_name, raw, default_value, min_value, max_value)
}

pub(super) fn parse_usize(
    var_name: &str,
    raw: Option<&str>,
    default_value: usize,
    min_value: usize,
    max_value: usize,
) -> anyhow::Result<usize> {
    parse_number(var_name, raw, default_value, min_value, max_value)
}

pub(super) fn parse_f32(
    var_name: &str,
    raw: Option<&str>,
    default_value: f32,
    min_value: f32,
    max_value: f32,
) -> anyhow::Result<f32> {
    let Some(raw) = raw else {
        return Ok(default_value);
    };
    let parsed = raw
        .trim()
        .parse::<f32>()
        .map_err(|_| anyhow::anyhow!("{var_name} must be a number"))?;
    anyhow::ensure!(parsed.is_finite(), "{var_name} must be a finite number");
    anyhow::ensure!(
        (min_value..=max_value).contains(&parsed),
        "{var_name} must be between {min_value} and {max_value}"
    );
    Ok(parsed)
}

fn parse_number<T>(
    var_name: &str,
    raw: Option<&str>,
    default_value: T,
    min_value: T,
    max_value: T,
) -> anyhow::Result<T>
where
    T: std::str::FromStr + PartialOrd + std::fmt::Display + Copy,
{
    let Some(raw) = raw else {
        return Ok(default_value);
    };
    let parsed = raw
        .trim()
        .parse::<T>()
        .map_err(|_| anyhow::anyhow!("{var_name} must be a number"))?;
    anyhow::ensure!(
        parsed >= min_value && parsed <= max_value,
        "{var_name} must be between {min_value} and {max_value}"
    );
    Ok(parsed)
}

pub(super) fn parse_ip_list_env(var_name: &str, raw: Option<&str>) -> anyhow::Result<Vec<IpAddr>> {
    let Some(raw) = raw.map(str::trim).filter(|s| !s.is_empty()) else {
        return Ok(Vec::new());
    };
    let mut out = Vec::new();
    for entry in raw.split(',') {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        let ip = entry
            .parse::<IpAddr>()
            .map_err(|_| anyhow::anyhow!("{var_name} contains invalid IP: {entry}"))?;
        out.push(ip);
    }
    Ok(out)
}

pub(super) fn default_native_transform_concurrency() -> usize {
    std::thread::available_parallelism()
        .map(usize::from)
        .unwrap_or(4)
        .clamp(2, 8)
}
