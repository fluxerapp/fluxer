// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::constants;
use base64::{Engine as _, engine::general_purpose};
use std::{env, net::IpAddr};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StorageBackend {
    Local,
    S3,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DeploymentMode {
    Mp,
    Static,
    Upload,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BucketStyle {
    Path,
    VirtualHosted,
    Rooted,
}

#[derive(Clone, Debug)]
pub struct Config {
    pub node_env: String,
    pub bind_host: String,
    pub port: u16,
    pub secret_key: String,
    pub mode: DeploymentMode,
    pub read_only: bool,
    pub storage_backend: StorageBackend,
    pub storage_root: String,
    pub s3_endpoint: String,
    pub s3_region: String,
    pub s3_access_key_id: String,
    pub s3_secret_access_key: String,
    pub s3_session_token: String,
    pub s3_force_path_style: bool,
    pub s3_read_endpoint: Option<String>,
    pub s3_read_bucket: String,
    pub s3_read_bucket_style: BucketStyle,
    pub s3_read_signed: bool,
    pub bucket_cdn: String,
    pub bucket_uploads: String,
    pub bucket_static: String,
    pub upload_relay_secret: Vec<u8>,
    pub upload_relay_max_body_bytes: u64,
    pub upload_relay_token_ttl_secs: u64,
    pub upload_relay_s3_timeout_ms: u64,
    pub upload_relay_buffered_retry_max_bytes: u64,
    pub upload_relay_buffered_retry_total_bytes: u64,
    pub upload_relay_spool_dir: std::path::PathBuf,
    pub upload_relay_spool_chunk_bytes: usize,
    pub upload_relay_spool_max_total_bytes: u64,
    pub max_native_transforms: usize,
    pub worker_queue_capacity: usize,
    pub nsfw_service_endpoint: String,
    pub nsfw_threshold: f32,
    pub transform_cache_capacity_bytes: usize,
    pub transform_cache_max_entry_bytes: usize,
    pub transform_cache_ttl_ms: u64,
    pub shutdown_grace_ms: u64,
    pub socket_io_timeout_ms: u64,
    pub transform_timeout_ms: u64,
    pub max_encode_frames: u32,
    pub max_encode_duration_ms: u32,
    pub bunny_ip_gate_enabled: bool,
    pub bunny_ip_gate_trusted_proxies: Vec<IpAddr>,
    pub bunny_ip_gate_refresh_secs: u64,
}

impl Config {
    pub fn load_from_env() -> anyhow::Result<Self> {
        Self::load_from_iter(env::vars())
    }

    pub fn load_from_iter<I, K, V>(vars: I) -> anyhow::Result<Self>
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<String>,
        V: Into<String>,
    {
        let env = EnvMap::from_iter(vars);

        let mode =
            parse_mode_env(env.get("FLUXER_MEDIA_PROXY_MODE"))?.unwrap_or(DeploymentMode::Mp);
        let secret_key = env
            .get("FLUXER_MEDIA_PROXY_SECRET_KEY")
            .map(ToOwned::to_owned)
            .unwrap_or_default();
        anyhow::ensure!(
            !secret_key.is_empty(),
            "FLUXER_MEDIA_PROXY_SECRET_KEY is required"
        );

        let max_native_transforms = parse_usize(
            "FLUXER_MEDIA_PROXY_MAX_NATIVE_TRANSFORMS",
            env.get("FLUXER_MEDIA_PROXY_MAX_NATIVE_TRANSFORMS"),
            default_native_transform_concurrency(),
            1,
            128,
        )?;
        let upload_relay_secret = decode_upload_relay_secret(
            env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64"),
            mode,
        )?;

        let s3_force_path_style = parse_bool(
            "FLUXER_S3_FORCE_PATH_STYLE",
            env.get("FLUXER_S3_FORCE_PATH_STYLE"),
        )?
        .unwrap_or(true);
        let bucket_cdn = env
            .get("FLUXER_S3_BUCKET_CDN")
            .map(ToOwned::to_owned)
            .unwrap_or_else(|| "cdn".to_owned());
        let s3_read_endpoint = non_empty(env.get("FLUXER_S3_READ_ENDPOINT"));
        if let Some(endpoint) = s3_read_endpoint.as_deref() {
            validate_read_endpoint(endpoint)?;
        }
        let s3_read_bucket =
            non_empty(env.get("FLUXER_S3_READ_BUCKET")).unwrap_or_else(|| bucket_cdn.clone());
        let s3_read_bucket_style = parse_bucket_style(env.get("FLUXER_S3_READ_BUCKET_STYLE"))?
            .unwrap_or(if s3_force_path_style {
                BucketStyle::Path
            } else {
                BucketStyle::VirtualHosted
            });
        let s3_read_signed = parse_bool(
            "FLUXER_S3_READ_SIGNED",
            non_empty(env.get("FLUXER_S3_READ_SIGNED")).as_deref(),
        )?
        .unwrap_or(false);

        Ok(Self {
            node_env: env.get("NODE_ENV").unwrap_or("development").to_owned(),
            bind_host: env
                .get("FLUXER_MEDIA_PROXY_HOST")
                .unwrap_or("0.0.0.0")
                .to_owned(),
            port: parse_u16(
                "FLUXER_MEDIA_PROXY_PORT",
                env.get("FLUXER_MEDIA_PROXY_PORT"),
                8080,
            )?,
            secret_key,
            mode,
            read_only: parse_bool(
                "FLUXER_MEDIA_PROXY_READ_ONLY",
                env.get("FLUXER_MEDIA_PROXY_READ_ONLY"),
            )?
            .unwrap_or(false),
            storage_backend: parse_storage_backend(env.get("FLUXER_MEDIA_PROXY_STORAGE_BACKEND"))?
                .unwrap_or(StorageBackend::Local),
            storage_root: env
                .get("FLUXER_MEDIA_PROXY_STORAGE_ROOT")
                .unwrap_or("./media_proxy_storage")
                .to_owned(),
            s3_endpoint: env
                .get("FLUXER_S3_ENDPOINT")
                .map(ToOwned::to_owned)
                .unwrap_or_default(),
            s3_region: env
                .get("FLUXER_S3_REGION")
                .map(ToOwned::to_owned)
                .unwrap_or_else(|| "us-east-1".to_owned()),
            s3_access_key_id: env
                .get("FLUXER_S3_ACCESS_KEY_ID")
                .map(ToOwned::to_owned)
                .unwrap_or_default(),
            s3_secret_access_key: env
                .get("FLUXER_S3_SECRET_ACCESS_KEY")
                .map(ToOwned::to_owned)
                .unwrap_or_default(),
            s3_session_token: env.get("FLUXER_S3_SESSION_TOKEN").unwrap_or("").to_owned(),
            s3_force_path_style,
            s3_read_endpoint,
            s3_read_bucket,
            s3_read_bucket_style,
            s3_read_signed,
            bucket_cdn,
            bucket_uploads: env
                .get("FLUXER_S3_BUCKET_UPLOADS")
                .map(ToOwned::to_owned)
                .unwrap_or_else(|| "uploads".to_owned()),
            bucket_static: env
                .get("FLUXER_S3_BUCKET_STATIC")
                .map(ToOwned::to_owned)
                .unwrap_or_else(|| "static".to_owned()),
            upload_relay_secret,
            upload_relay_max_body_bytes: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES"),
                500 * 1024 * 1024,
                1,
                5 * 1024 * 1024 * 1024,
            )?,
            upload_relay_token_ttl_secs: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_TOKEN_TTL_SECS",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_TOKEN_TTL_SECS"),
                3_600,
                1,
                7 * 24 * 60 * 60,
            )?,
            upload_relay_s3_timeout_ms: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_S3_TIMEOUT_MS",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_S3_TIMEOUT_MS"),
                900_000,
                1_000,
                60 * 60 * 1000,
            )?,
            upload_relay_buffered_retry_max_bytes: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_BYTES"),
                32 * 1024 * 1024,
                0,
                256 * 1024 * 1024,
            )?,
            upload_relay_buffered_retry_total_bytes: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_TOTAL_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_TOTAL_BYTES"),
                512 * 1024 * 1024,
                0,
                8 * 1024 * 1024 * 1024,
            )?,
            upload_relay_spool_dir: env
                .get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_DIR")
                .map(std::path::PathBuf::from)
                .unwrap_or_else(std::env::temp_dir),
            upload_relay_spool_chunk_bytes: parse_usize(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_CHUNK_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_CHUNK_BYTES"),
                1024 * 1024,
                64 * 1024,
                64 * 1024 * 1024,
            )?,
            upload_relay_spool_max_total_bytes: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES"),
                8 * 1024 * 1024 * 1024,
                0,
                256 * 1024 * 1024 * 1024,
            )?,
            max_native_transforms,
            worker_queue_capacity: parse_usize(
                "FLUXER_MEDIA_PROXY_WORKER_QUEUE_CAPACITY",
                env.get("FLUXER_MEDIA_PROXY_WORKER_QUEUE_CAPACITY"),
                max_native_transforms * 8,
                1,
                8192,
            )?,
            nsfw_service_endpoint: env
                .get("FLUXER_NSFW_SERVICE_ENDPOINT")
                .unwrap_or("")
                .to_owned(),
            nsfw_threshold: parse_f32(
                "FLUXER_MEDIA_PROXY_NSFW_THRESHOLD",
                env.get("FLUXER_MEDIA_PROXY_NSFW_THRESHOLD"),
                0.85,
                0.0,
                1.0,
            )?,
            transform_cache_capacity_bytes: parse_usize(
                "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES",
                env.get("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES"),
                256 * 1024 * 1024,
                0,
                4 * 1024 * 1024 * 1024,
            )?,
            transform_cache_max_entry_bytes: parse_usize(
                "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_MAX_ENTRY_BYTES",
                env.get("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_MAX_ENTRY_BYTES"),
                64 * 1024 * 1024,
                0,
                512 * 1024 * 1024,
            )?,
            transform_cache_ttl_ms: parse_u64(
                "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_TTL_MS",
                env.get("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_TTL_MS"),
                120_000,
                0,
                60 * 60 * 1000,
            )?,
            shutdown_grace_ms: parse_u64(
                "FLUXER_MEDIA_PROXY_SHUTDOWN_GRACE_MS",
                env.get("FLUXER_MEDIA_PROXY_SHUTDOWN_GRACE_MS"),
                30_000,
                0,
                5 * 60 * 1000,
            )?,
            socket_io_timeout_ms: parse_u64(
                "FLUXER_MEDIA_PROXY_SOCKET_IO_TIMEOUT_MS",
                env.get("FLUXER_MEDIA_PROXY_SOCKET_IO_TIMEOUT_MS"),
                30_000,
                0,
                5 * 60 * 1000,
            )?,
            transform_timeout_ms: parse_u64(
                "FLUXER_MEDIA_PROXY_TRANSFORM_TIMEOUT_MS",
                env.get("FLUXER_MEDIA_PROXY_TRANSFORM_TIMEOUT_MS"),
                15_000,
                1_000,
                120_000,
            )?,
            max_encode_frames: parse_usize(
                "FLUXER_MEDIA_PROXY_MAX_ENCODE_FRAMES",
                env.get("FLUXER_MEDIA_PROXY_MAX_ENCODE_FRAMES"),
                constants::MAX_ANIMATED_FRAMES_DEFAULT as usize,
                1,
                100_000,
            )? as u32,
            max_encode_duration_ms: parse_usize(
                "FLUXER_MEDIA_PROXY_MAX_ENCODE_DURATION_MS",
                env.get("FLUXER_MEDIA_PROXY_MAX_ENCODE_DURATION_MS"),
                30_000,
                100,
                10 * 60 * 1000,
            )? as u32,
            bunny_ip_gate_enabled: parse_bool(
                "FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_ENABLED",
                env.get("FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_ENABLED"),
            )?
            .unwrap_or(false),
            bunny_ip_gate_trusted_proxies: parse_ip_list_env(
                "FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_TRUSTED_PROXIES",
                env.get("FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_TRUSTED_PROXIES"),
            )?,
            bunny_ip_gate_refresh_secs: parse_u64(
                "FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_REFRESH_SECS",
                env.get("FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_REFRESH_SECS"),
                3_600,
                60,
                24 * 60 * 60,
            )?,
        })
    }
}

#[derive(Debug, Default)]
struct EnvMap(Vec<(String, String)>);

impl EnvMap {
    fn from_iter<I, K, V>(vars: I) -> Self
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

    fn get(&self, key: &str) -> Option<&str> {
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

fn parse_mode_env(raw: Option<&str>) -> anyhow::Result<Option<DeploymentMode>> {
    let Some(raw) = raw else {
        return Ok(None);
    };
    let raw = raw.trim();
    parse_mode(raw).map(Some).ok_or_else(|| {
        anyhow::anyhow!("FLUXER_MEDIA_PROXY_MODE must be one of: mp, static, upload")
    })
}

fn non_empty(raw: Option<&str>) -> Option<String> {
    raw.map(str::trim)
        .filter(|value| !value.is_empty())
        .map(ToOwned::to_owned)
}

fn parse_bucket_style(raw: Option<&str>) -> anyhow::Result<Option<BucketStyle>> {
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

fn validate_read_endpoint(endpoint: &str) -> anyhow::Result<()> {
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

fn parse_storage_backend(raw: Option<&str>) -> anyhow::Result<Option<StorageBackend>> {
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

fn decode_upload_relay_secret(raw: Option<&str>, mode: DeploymentMode) -> anyhow::Result<Vec<u8>> {
    let Some(raw) = raw.map(str::trim).filter(|s| !s.is_empty()) else {
        anyhow::ensure!(
            mode != DeploymentMode::Upload,
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 is required in upload mode"
        );
        return Ok(Vec::new());
    };
    let decoded = general_purpose::STANDARD.decode(raw).map_err(|_| {
        anyhow::anyhow!("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must be base64")
    })?;
    anyhow::ensure!(
        decoded.len() >= 32,
        "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must decode to at least 32 bytes"
    );
    Ok(decoded)
}

fn parse_bool(var_name: &str, raw: Option<&str>) -> anyhow::Result<Option<bool>> {
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

fn parse_u16(var_name: &str, raw: Option<&str>, default_value: u16) -> anyhow::Result<u16> {
    parse_number(var_name, raw, default_value, u16::MIN, u16::MAX)
}

fn parse_u64(
    var_name: &str,
    raw: Option<&str>,
    default_value: u64,
    min_value: u64,
    max_value: u64,
) -> anyhow::Result<u64> {
    parse_number(var_name, raw, default_value, min_value, max_value)
}

fn parse_usize(
    var_name: &str,
    raw: Option<&str>,
    default_value: usize,
    min_value: usize,
    max_value: usize,
) -> anyhow::Result<usize> {
    parse_number(var_name, raw, default_value, min_value, max_value)
}

fn parse_f32(
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

fn parse_ip_list_env(var_name: &str, raw: Option<&str>) -> anyhow::Result<Vec<IpAddr>> {
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

fn default_native_transform_concurrency() -> usize {
    std::thread::available_parallelism()
        .map(usize::from)
        .unwrap_or(4)
        .clamp(2, 8)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn base_env() -> Vec<(&'static str, &'static str)> {
        vec![("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")]
    }

    #[test]
    fn requires_secret_key() {
        let err = Config::load_from_iter(std::iter::empty::<(&str, &str)>()).unwrap_err();
        assert!(err.to_string().contains("FLUXER_MEDIA_PROXY_SECRET_KEY"));
    }

    #[test]
    fn default_config_matches_media_service() {
        let cfg = Config::load_from_iter(base_env()).unwrap();
        assert_eq!("0.0.0.0", cfg.bind_host);
        assert_eq!(8080, cfg.port);
        assert_eq!(StorageBackend::Local, cfg.storage_backend);
        assert_eq!(DeploymentMode::Mp, cfg.mode);
        assert_eq!("cdn", cfg.bucket_cdn);
        assert_eq!("uploads", cfg.bucket_uploads);
        assert_eq!("static", cfg.bucket_static);
        assert!(cfg.max_native_transforms >= 2);
        assert_eq!(cfg.max_native_transforms * 8, cfg.worker_queue_capacity);
    }

    #[test]
    fn canonical_media_proxy_env_overrides_apply() {
        let cfg = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_STORAGE_BACKEND", "s3"),
            ("FLUXER_MEDIA_PROXY_STORAGE_ROOT", "/srv/fluxer-media"),
            ("FLUXER_MEDIA_PROXY_READ_ONLY", "true"),
            ("FLUXER_S3_FORCE_PATH_STYLE", "false"),
            ("FLUXER_S3_SESSION_TOKEN", "token"),
            ("FLUXER_MEDIA_PROXY_MAX_NATIVE_TRANSFORMS", "3"),
            ("FLUXER_MEDIA_PROXY_WORKER_QUEUE_CAPACITY", "24"),
            ("FLUXER_MEDIA_PROXY_TRANSFORM_TIMEOUT_MS", "2000"),
            ("FLUXER_NSFW_SERVICE_ENDPOINT", "http://nsfw:8000"),
            ("FLUXER_MEDIA_PROXY_NSFW_THRESHOLD", "0.7"),
        ])
        .unwrap();

        assert_eq!(StorageBackend::S3, cfg.storage_backend);
        assert_eq!("/srv/fluxer-media", cfg.storage_root);
        assert!(cfg.read_only);
        assert!(!cfg.s3_force_path_style);
        assert_eq!("token", cfg.s3_session_token);
        assert_eq!(3, cfg.max_native_transforms);
        assert_eq!(24, cfg.worker_queue_capacity);
        assert_eq!(2_000, cfg.transform_timeout_ms);
        assert_eq!("http://nsfw:8000", cfg.nsfw_service_endpoint);
        assert!((cfg.nsfw_threshold - 0.7).abs() < f32::EPSILON);
    }

    #[test]
    fn upload_mode_requires_relay_secret() {
        let err = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_MODE", "upload"),
        ])
        .unwrap_err();
        assert!(
            err.to_string()
                .contains("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64")
        );
    }

    #[test]
    fn parses_upload_relay_secret() {
        let secret = general_purpose::STANDARD.encode([7u8; 32]);
        let cfg = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_MODE", "upload"),
            (
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64",
                secret.as_str(),
            ),
        ])
        .unwrap();
        assert_eq!(vec![7u8; 32], cfg.upload_relay_secret);
    }

    #[test]
    fn rejects_invalid_mode_env() {
        let err = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_MODE", "worker"),
        ])
        .unwrap_err();
        assert!(err.to_string().contains("FLUXER_MEDIA_PROXY_MODE"));
    }

    #[test]
    fn rejects_invalid_storage_backend_env() {
        let err = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_STORAGE_BACKEND", "filesystem"),
        ])
        .unwrap_err();
        assert!(
            err.to_string()
                .contains("FLUXER_MEDIA_PROXY_STORAGE_BACKEND")
        );
    }

    #[test]
    fn rejects_invalid_bool_env() {
        let err = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_READ_ONLY", "maybe"),
        ])
        .unwrap_err();
        assert!(err.to_string().contains("FLUXER_MEDIA_PROXY_READ_ONLY"));
    }

    #[test]
    fn rejects_invalid_number_env() {
        let err = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_WORKER_QUEUE_CAPACITY", "many"),
        ])
        .unwrap_err();
        assert!(
            err.to_string()
                .contains("FLUXER_MEDIA_PROXY_WORKER_QUEUE_CAPACITY")
        );
    }

    #[test]
    fn rejects_out_of_range_number_env() {
        let err = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_TRANSFORM_TIMEOUT_MS", "999999"),
        ])
        .unwrap_err();
        assert!(
            err.to_string()
                .contains("FLUXER_MEDIA_PROXY_TRANSFORM_TIMEOUT_MS")
        );
    }

    fn env_with(extra: &[(&'static str, &'static str)]) -> Vec<(&'static str, &'static str)> {
        let mut env = base_env();
        env.extend_from_slice(extra);
        env
    }

    #[test]
    fn read_endpoint_defaults_to_disabled() {
        let cfg = Config::load_from_iter(base_env()).unwrap();
        assert_eq!(None, cfg.s3_read_endpoint);
        assert_eq!("cdn", cfg.s3_read_bucket);
        assert_eq!(BucketStyle::Path, cfg.s3_read_bucket_style);
        assert!(!cfg.s3_read_signed);
    }

    #[test]
    fn empty_read_vars_are_treated_as_unset() {
        let cfg = Config::load_from_iter(env_with(&[
            ("FLUXER_S3_READ_ENDPOINT", "   "),
            ("FLUXER_S3_READ_BUCKET", ""),
            ("FLUXER_S3_READ_BUCKET_STYLE", ""),
            ("FLUXER_S3_READ_SIGNED", ""),
        ]))
        .unwrap();
        assert_eq!(None, cfg.s3_read_endpoint);
        assert_eq!("cdn", cfg.s3_read_bucket);
        assert_eq!(BucketStyle::Path, cfg.s3_read_bucket_style);
        assert!(!cfg.s3_read_signed);
    }

    #[test]
    fn every_new_var_tolerates_a_blank_value_individually() {
        for var in [
            "FLUXER_S3_READ_ENDPOINT",
            "FLUXER_S3_READ_BUCKET",
            "FLUXER_S3_READ_BUCKET_STYLE",
            "FLUXER_S3_READ_SIGNED",
        ] {
            for blank in ["", "  "] {
                let cfg = Config::load_from_iter(env_with(&[(var, blank)])).unwrap_or_else(|err| {
                    panic!("{var}={blank:?} must be treated as unset: {err}")
                });
                assert_eq!(None, cfg.s3_read_endpoint);
                assert_eq!("cdn", cfg.s3_read_bucket);
                assert_eq!(BucketStyle::Path, cfg.s3_read_bucket_style);
                assert!(!cfg.s3_read_signed);
            }
        }
    }

    #[test]
    fn read_bucket_defaults_to_cdn_bucket_and_can_be_overridden() {
        let cfg = Config::load_from_iter(env_with(&[("FLUXER_S3_BUCKET_CDN", "fluxer")])).unwrap();
        assert_eq!("fluxer", cfg.s3_read_bucket);

        let cfg = Config::load_from_iter(env_with(&[
            ("FLUXER_S3_BUCKET_CDN", "fluxer"),
            ("FLUXER_S3_READ_BUCKET", "fluxer-static"),
        ]))
        .unwrap();
        assert_eq!("fluxer-static", cfg.s3_read_bucket);
    }

    #[test]
    fn read_bucket_style_inherits_force_path_style() {
        let cfg =
            Config::load_from_iter(env_with(&[("FLUXER_S3_FORCE_PATH_STYLE", "false")])).unwrap();
        assert_eq!(BucketStyle::VirtualHosted, cfg.s3_read_bucket_style);

        let cfg = Config::load_from_iter(env_with(&[
            ("FLUXER_S3_FORCE_PATH_STYLE", "false"),
            ("FLUXER_S3_READ_BUCKET_STYLE", "root"),
        ]))
        .unwrap();
        assert_eq!(BucketStyle::Rooted, cfg.s3_read_bucket_style);
        assert!(!cfg.s3_force_path_style);
    }

    #[test]
    fn read_bucket_style_parses_all_values_case_insensitively() {
        for (raw, expected) in [
            ("path", BucketStyle::Path),
            ("PATH", BucketStyle::Path),
            ("virtual", BucketStyle::VirtualHosted),
            (" Virtual ", BucketStyle::VirtualHosted),
            ("root", BucketStyle::Rooted),
            ("ROOT", BucketStyle::Rooted),
        ] {
            let cfg =
                Config::load_from_iter(env_with(&[("FLUXER_S3_READ_BUCKET_STYLE", raw)])).unwrap();
            assert_eq!(expected, cfg.s3_read_bucket_style, "raw={raw}");
        }
    }

    #[test]
    fn read_bucket_style_rejects_unknown_value() {
        let err = Config::load_from_iter(env_with(&[("FLUXER_S3_READ_BUCKET_STYLE", "cdn")]))
            .unwrap_err();
        assert!(err.to_string().contains("FLUXER_S3_READ_BUCKET_STYLE"));
    }

    #[test]
    fn read_signed_parses_boolean() {
        let cfg = Config::load_from_iter(env_with(&[("FLUXER_S3_READ_SIGNED", "true")])).unwrap();
        assert!(cfg.s3_read_signed);

        let err =
            Config::load_from_iter(env_with(&[("FLUXER_S3_READ_SIGNED", "maybe")])).unwrap_err();
        assert!(err.to_string().contains("FLUXER_S3_READ_SIGNED"));
    }

    #[test]
    fn read_endpoint_is_validated_at_startup() {
        let cfg = Config::load_from_iter(env_with(&[(
            "FLUXER_S3_READ_ENDPOINT",
            "https://cdn.example.net",
        )]))
        .unwrap();
        assert_eq!(
            Some("https://cdn.example.net".to_owned()),
            cfg.s3_read_endpoint
        );

        for (bad, expected) in [
            ("cdn.example.net", "not a valid URL"),
            ("ftp://cdn.example.net", "must be an http or https URL"),
            ("https://", "not a valid URL"),
            (
                "https://user:pw@cdn.example.net",
                "must not contain credentials",
            ),
            (
                "https://cdn.example.net/?token=abc",
                "must not contain a query string or fragment",
            ),
            (
                "https://cdn.example.net/#frag",
                "must not contain a query string or fragment",
            ),
        ] {
            let err =
                Config::load_from_iter(env_with(&[("FLUXER_S3_READ_ENDPOINT", bad)])).unwrap_err();
            let message = err.to_string();
            assert!(
                message.contains("FLUXER_S3_READ_ENDPOINT") && message.contains(expected),
                "bad={bad} err={message}"
            );
        }
    }
}
