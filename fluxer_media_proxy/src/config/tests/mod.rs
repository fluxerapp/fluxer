// SPDX-License-Identifier: AGPL-3.0-or-later

mod s3_read;

use super::*;
use base64::{Engine as _, engine::general_purpose};

fn base_env() -> Vec<(&'static str, &'static str)> {
    vec![("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")]
}

fn env_with(extra: &[(&'static str, &'static str)]) -> Vec<(&'static str, &'static str)> {
    let mut env = base_env();
    env.extend_from_slice(extra);
    env
}

fn with_shared_runtime_env(release: &[(&str, &str)]) -> Vec<(String, String)> {
    [
        ("NODE_ENV", "production"),
        ("FLUXER_ENV", "production"),
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "shared-runtime-secret"),
        ("FLUXER_S3_ENDPOINT", "https://ewr1.vultrobjects.com"),
        ("FLUXER_S3_REGION", "ewr1"),
        ("FLUXER_S3_ACCESS_KEY_ID", "AKIAIOSFODNN7EXAMPLE"),
        (
            "FLUXER_S3_SECRET_ACCESS_KEY",
            "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
        ),
    ]
    .iter()
    .chain(release.iter())
    .map(|(key, value)| ((*key).to_owned(), (*value).to_owned()))
    .collect()
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
    assert_eq!(StorageBackend::Local, cfg.storage.backend);
    assert_eq!(DeploymentMode::Mp, cfg.mode);
    assert_eq!("cdn", cfg.storage.bucket_cdn);
    assert_eq!("uploads", cfg.storage.bucket_uploads);
    assert_eq!("static", cfg.storage.bucket_static);
    assert!(cfg.media.max_native_transforms >= 2);
    assert_eq!(
        cfg.media.max_native_transforms * 8,
        cfg.media.worker_queue_capacity
    );
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

    assert_eq!(StorageBackend::S3, cfg.storage.backend);
    assert_eq!("/srv/fluxer-media", cfg.storage.root);
    assert!(cfg.read_only);
    assert!(!cfg.storage.s3_force_path_style);
    assert_eq!("token", cfg.storage.s3_session_token);
    assert_eq!(3, cfg.media.max_native_transforms);
    assert_eq!(24, cfg.media.worker_queue_capacity);
    assert_eq!(2_000, cfg.media.transform_timeout_ms);
    assert_eq!("http://nsfw:8000", cfg.media.nsfw_service_endpoint);
    assert!((cfg.media.nsfw_threshold - 0.7).abs() < f32::EPSILON);
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
    assert_eq!(&[7u8; 32][..], cfg.upload_relay.secret.expose());
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

#[test]
fn production_media_proxy_release_env_loads() {
    let cfg = Config::load_from_iter(with_shared_runtime_env(&[
        ("RELEASE_CHANNEL", "canary"),
        (
            "FLUXER_NSFW_SERVICE_ENDPOINT",
            "http://int.flx-nyc-misc1.srv.fluxer.dev:8000",
        ),
        ("FLUXER_MEDIA_PROXY_MODE", "mp"),
        ("FLUXER_MEDIA_PROXY_NSFW_THRESHOLD", "0.95"),
        ("FLUXER_MEDIA_PROXY_STORAGE_BACKEND", "s3"),
        ("FLUXER_MEDIA_PROXY_READ_ONLY", "true"),
        ("FLUXER_MEDIA_PROXY_MAX_NATIVE_TRANSFORMS", "4"),
        ("FLUXER_MEDIA_PROXY_WORKER_QUEUE_CAPACITY", "128"),
        ("FLUXER_MEDIA_PROXY_TRANSFORM_TIMEOUT_MS", "30000"),
        ("FLUXER_MEDIA_PROXY_MAX_ENCODE_FRAMES", "4096"),
        ("FLUXER_MEDIA_PROXY_MAX_ENCODE_DURATION_MS", "30000"),
        ("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES", "1073741824"),
        (
            "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_MAX_ENTRY_BYTES",
            "134217728",
        ),
        ("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_TTL_MS", "1800000"),
        ("FLUXER_MEDIA_PROXY_SOCKET_IO_TIMEOUT_MS", "30000"),
    ]))
    .unwrap();

    assert_eq!("production", cfg.node_env);
    assert_eq!(DeploymentMode::Mp, cfg.mode);
    assert!(cfg.read_only);
    assert_eq!(StorageBackend::S3, cfg.storage.backend);
    assert_eq!("ewr1", cfg.storage.s3_region);
    assert_eq!(4, cfg.media.max_native_transforms);
    assert_eq!(128, cfg.media.worker_queue_capacity);
    assert_eq!(30_000, cfg.media.transform_timeout_ms);
    assert_eq!(4_096, cfg.media.max_encode_frames);
    assert_eq!(30_000, cfg.media.max_encode_duration_ms);
    assert_eq!(1 << 30, cfg.media.transform_cache_capacity_bytes);
    assert_eq!(128 << 20, cfg.media.transform_cache_max_entry_bytes);
    assert_eq!(1_800_000, cfg.media.transform_cache_ttl_ms);
    assert_eq!(30_000, cfg.socket_io_timeout_ms);
    assert!((cfg.media.nsfw_threshold - 0.95).abs() < f32::EPSILON);
    assert_eq!(
        "http://int.flx-nyc-misc1.srv.fluxer.dev:8000",
        cfg.media.nsfw_service_endpoint
    );
}

#[test]
fn production_static_proxy_release_env_loads() {
    let cfg = Config::load_from_iter(with_shared_runtime_env(&[
        ("RELEASE_CHANNEL", "canary"),
        ("FLUXER_MEDIA_PROXY_MODE", "static"),
        ("FLUXER_MEDIA_PROXY_STORAGE_BACKEND", "s3"),
        ("FLUXER_MEDIA_PROXY_READ_ONLY", "true"),
        ("FLUXER_MEDIA_PROXY_SOCKET_IO_TIMEOUT_MS", "30000"),
    ]))
    .unwrap();

    assert_eq!(DeploymentMode::Static, cfg.mode);
    assert!(cfg.read_only);
    assert_eq!(StorageBackend::S3, cfg.storage.backend);
    assert_eq!("static", cfg.storage.bucket_static);
    assert_eq!(30_000, cfg.socket_io_timeout_ms);
    assert!(cfg.upload_relay.secret.expose().is_empty());
}

#[test]
fn production_uploads_release_env_loads() {
    let relay_secret = general_purpose::STANDARD.encode([9u8; 48]);
    let cfg = Config::load_from_iter(with_shared_runtime_env(&[
        ("RELEASE_CHANNEL", "stable"),
        ("FLUXER_MEDIA_PROXY_MODE", "upload"),
        ("FLUXER_MEDIA_PROXY_STORAGE_BACKEND", "s3"),
        ("FLUXER_MEDIA_PROXY_READ_ONLY", "false"),
        ("FLUXER_MEDIA_PROXY_SOCKET_IO_TIMEOUT_MS", "300000"),
        ("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_S3_TIMEOUT_MS", "900000"),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_BYTES",
            "33554432",
        ),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_BUFFERED_RETRY_TOTAL_BYTES",
            "536870912",
        ),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64",
            relay_secret.as_str(),
        ),
    ]))
    .unwrap();

    assert_eq!(DeploymentMode::Upload, cfg.mode);
    assert!(!cfg.read_only);
    assert_eq!(300_000, cfg.socket_io_timeout_ms);
    assert_eq!(900_000, cfg.upload_relay.s3_timeout_ms);
    assert_eq!(32 << 20, cfg.upload_relay.buffered_retry_max_bytes);
    assert_eq!(512 << 20, cfg.upload_relay.buffered_retry_total_bytes);
    assert_eq!(500 * 1024 * 1024, cfg.upload_relay.max_body_bytes);
    assert_eq!(&[9u8; 48][..], cfg.upload_relay.secret.expose());
}

#[test]
fn a_transform_cache_entry_ceiling_above_the_capacity_still_boots() {
    let cfg = Config::load_from_iter([
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
        ("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES", "1048576"),
        (
            "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_MAX_ENTRY_BYTES",
            "2097152",
        ),
    ])
    .unwrap();
    assert_eq!(1024 * 1024, cfg.media.transform_cache_capacity_bytes);
    assert_eq!(2 * 1024 * 1024, cfg.media.transform_cache_max_entry_bytes);
}

#[test]
fn both_transform_cache_env_keys_accept_their_whole_range() {
    let cfg = Config::load_from_iter([
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
        ("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES", "4294967296"),
        (
            "FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_MAX_ENTRY_BYTES",
            "536870912",
        ),
    ])
    .unwrap();
    assert_eq!(
        4 * 1024 * 1024 * 1024,
        cfg.media.transform_cache_capacity_bytes
    );
    assert_eq!(512 * 1024 * 1024, cfg.media.transform_cache_max_entry_bytes);

    let cfg = Config::load_from_iter([
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
        ("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES", "0"),
        ("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_MAX_ENTRY_BYTES", "0"),
    ])
    .unwrap();
    assert_eq!(0, cfg.media.transform_cache_capacity_bytes);
    assert_eq!(0, cfg.media.transform_cache_max_entry_bytes);
}

#[test]
fn a_disabled_transform_cache_ignores_the_entry_ceiling() {
    let cfg = Config::load_from_iter([
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
        ("FLUXER_MEDIA_PROXY_TRANSFORM_CACHE_BYTES", "0"),
    ])
    .unwrap();
    assert_eq!(0, cfg.media.transform_cache_capacity_bytes);
    assert_eq!(64 * 1024 * 1024, cfg.media.transform_cache_max_entry_bytes);
}

#[test]
fn every_deployment_mode_and_storage_backend_variant_parses() {
    let relay_secret = general_purpose::STANDARD.encode([3u8; 32]);
    for (raw, expected) in [
        ("mp", DeploymentMode::Mp),
        ("MP", DeploymentMode::Mp),
        ("static", DeploymentMode::Static),
        ("Static", DeploymentMode::Static),
        ("upload", DeploymentMode::Upload),
        (" UPLOAD ", DeploymentMode::Upload),
    ] {
        let cfg = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_MODE", raw),
            (
                "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64",
                relay_secret.as_str(),
            ),
        ])
        .unwrap();
        assert_eq!(expected, cfg.mode);
    }

    for (raw, expected) in [
        ("local", StorageBackend::Local),
        ("Local", StorageBackend::Local),
        ("s3", StorageBackend::S3),
        (" S3 ", StorageBackend::S3),
    ] {
        let cfg = Config::load_from_iter([
            ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
            ("FLUXER_MEDIA_PROXY_STORAGE_BACKEND", raw),
        ])
        .unwrap();
        assert_eq!(expected, cfg.storage.backend);
    }
}

#[test]
fn upload_relay_spool_and_bunny_ip_gate_keys_apply() {
    let cfg = Config::load_from_iter([
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_DIR",
            "/var/spool/flx",
        ),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_CHUNK_BYTES",
            "2097152",
        ),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_MAX_TOTAL_BYTES",
            "1073741824",
        ),
        ("FLUXER_MEDIA_PROXY_UPLOAD_RELAY_TOKEN_TTL_SECS", "600"),
        ("FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_ENABLED", "yes"),
        (
            "FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_TRUSTED_PROXIES",
            "10.0.0.1, 2001:db8::1 ,",
        ),
        ("FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_REFRESH_SECS", "900"),
    ])
    .unwrap();

    assert_eq!(
        std::path::Path::new("/var/spool/flx"),
        cfg.upload_relay.spool_dir
    );
    assert_eq!(2 << 20, cfg.upload_relay.spool_chunk_bytes);
    assert_eq!(1 << 30, cfg.upload_relay.spool_max_total_bytes);
    assert_eq!(600, cfg.upload_relay.token_ttl_secs);
    assert!(cfg.bunny_ip_gate_enabled);
    assert_eq!(
        vec![
            "10.0.0.1".parse::<IpAddr>().unwrap(),
            "2001:db8::1".parse::<IpAddr>().unwrap(),
        ],
        cfg.bunny_ip_gate_trusted_proxies
    );
    assert_eq!(900, cfg.bunny_ip_gate_refresh_secs);
}

#[test]
fn rejects_invalid_bunny_ip_gate_trusted_proxies() {
    let err = Config::load_from_iter([
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
        (
            "FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_TRUSTED_PROXIES",
            "10.0.0.1,not-an-ip",
        ),
    ])
    .unwrap_err();
    assert!(
        err.to_string()
            .contains("FLUXER_MEDIA_PROXY_BUNNY_IP_GATE_TRUSTED_PROXIES")
    );
}

#[test]
fn debug_output_never_reveals_a_secret() {
    let relay_secret = general_purpose::STANDARD.encode(b"relay-secret-material-0123456789");
    let cfg = Config::load_from_iter([
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "signing-secret-material"),
        ("FLUXER_MEDIA_PROXY_MODE", "upload"),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64",
            relay_secret.as_str(),
        ),
    ])
    .unwrap();

    let rendered = format!("{cfg:?}");
    assert!(!rendered.contains("signing-secret-material"));
    assert!(!rendered.contains("relay-secret-material"));
    assert_eq!(2, rendered.matches("[REDACTED]").count());
    assert_eq!("signing-secret-material", cfg.secret_key.expose());
}
