// SPDX-License-Identifier: AGPL-3.0-or-later

mod parse;
#[cfg(test)]
mod tests;

use crate::constants;
use crate::secret::{SecretBytes, SecretString};
use parse::{
    EnvMap, decode_upload_relay_secret, default_native_transform_concurrency, non_empty,
    parse_bool, parse_bucket_style, parse_f32, parse_ip_list_env, parse_mode_env,
    parse_storage_backend, parse_u16, parse_u64, parse_usize, validate_read_endpoint,
};
use std::{env, net::IpAddr, path::PathBuf};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StorageBackend {
    Local,
    S3,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BucketStyle {
    Path,
    VirtualHosted,
    Rooted,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DeploymentMode {
    Mp,
    Static,
    Upload,
}

#[derive(Clone, Debug)]
pub struct StorageConfig {
    pub backend: StorageBackend,
    pub root: String,
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
}

#[derive(Clone, Debug)]
pub struct MediaServingConfig {
    pub max_native_transforms: usize,
    pub worker_queue_capacity: usize,
    pub nsfw_service_endpoint: String,
    pub nsfw_threshold: f32,
    pub transform_cache_capacity_bytes: usize,
    pub transform_cache_max_entry_bytes: usize,
    pub transform_cache_ttl_ms: u64,
    pub transform_timeout_ms: u64,
    pub max_encode_frames: u32,
    pub max_encode_duration_ms: u32,
}

#[derive(Clone, Debug)]
pub struct UploadRelayConfig {
    pub(crate) secret: SecretBytes,
    pub max_body_bytes: u64,
    pub token_ttl_secs: u64,
    pub s3_timeout_ms: u64,
    pub buffered_retry_max_bytes: u64,
    pub buffered_retry_total_bytes: u64,
    pub spool_dir: PathBuf,
    pub spool_chunk_bytes: usize,
    pub spool_max_total_bytes: u64,
}

#[derive(Clone, Debug)]
pub struct Config {
    pub node_env: String,
    pub bind_host: String,
    pub port: u16,
    pub(crate) secret_key: SecretString,
    pub mode: DeploymentMode,
    pub read_only: bool,
    pub shutdown_grace_ms: u64,
    pub socket_io_timeout_ms: u64,
    pub storage: StorageConfig,
    pub media: MediaServingConfig,
    pub upload_relay: UploadRelayConfig,
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
        let secret_key = SecretString::new(
            env.get("FLUXER_MEDIA_PROXY_SECRET_KEY")
                .unwrap_or_default()
                .to_owned(),
        );
        anyhow::ensure!(
            !secret_key.is_empty(),
            "FLUXER_MEDIA_PROXY_SECRET_KEY is required"
        );

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
            storage: StorageConfig::load(&env)?,
            media: MediaServingConfig::load(&env)?,
            upload_relay: UploadRelayConfig::load(&env, mode)?,
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

impl StorageConfig {
    fn load(env: &EnvMap) -> anyhow::Result<Self> {
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
            backend: parse_storage_backend(env.get("FLUXER_MEDIA_PROXY_STORAGE_BACKEND"))?
                .unwrap_or(StorageBackend::Local),
            root: env
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
        })
    }
}

impl MediaServingConfig {
    fn load(env: &EnvMap) -> anyhow::Result<Self> {
        let max_native_transforms = parse_usize(
            "FLUXER_MEDIA_PROXY_MAX_NATIVE_TRANSFORMS",
            env.get("FLUXER_MEDIA_PROXY_MAX_NATIVE_TRANSFORMS"),
            default_native_transform_concurrency(),
            1,
            128,
        )?;
        let transform_cache_capacity_bytes = parse_usize(
            "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES",
            env.get("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES"),
            256 * 1024 * 1024,
            0,
            4 * 1024 * 1024 * 1024,
        )?;
        let transform_cache_max_entry_bytes = parse_usize(
            "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_MAX_ENTRY_BYTES",
            env.get("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_MAX_ENTRY_BYTES"),
            64 * 1024 * 1024,
            0,
            512 * 1024 * 1024,
        )?;
        Ok(Self {
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
            transform_cache_capacity_bytes,
            transform_cache_max_entry_bytes,
            transform_cache_ttl_ms: parse_u64(
                "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_TTL_MS",
                env.get("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_TTL_MS"),
                120_000,
                0,
                60 * 60 * 1000,
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
        })
    }
}

impl UploadRelayConfig {
    fn load(env: &EnvMap, mode: DeploymentMode) -> anyhow::Result<Self> {
        Ok(Self {
            secret: decode_upload_relay_secret(
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64"),
                mode,
            )?,
            max_body_bytes: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES"),
                500 * 1024 * 1024,
                1,
                5 * 1024 * 1024 * 1024,
            )?,
            token_ttl_secs: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_TOKEN_TTL_SECS",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_TOKEN_TTL_SECS"),
                3_600,
                1,
                7 * 24 * 60 * 60,
            )?,
            s3_timeout_ms: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_S3_TIMEOUT_MS",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_S3_TIMEOUT_MS"),
                900_000,
                1_000,
                60 * 60 * 1000,
            )?,
            buffered_retry_max_bytes: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_BYTES"),
                32 * 1024 * 1024,
                0,
                256 * 1024 * 1024,
            )?,
            buffered_retry_total_bytes: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_TOTAL_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_TOTAL_BYTES"),
                512 * 1024 * 1024,
                0,
                8 * 1024 * 1024 * 1024,
            )?,
            spool_dir: env
                .get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_DIR")
                .map(PathBuf::from)
                .unwrap_or_else(std::env::temp_dir),
            spool_chunk_bytes: parse_usize(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_CHUNK_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_CHUNK_BYTES"),
                1024 * 1024,
                64 * 1024,
                64 * 1024 * 1024,
            )?,
            spool_max_total_bytes: parse_u64(
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES",
                env.get("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES"),
                8 * 1024 * 1024 * 1024,
                0,
                256 * 1024 * 1024 * 1024,
            )?,
        })
    }
}
