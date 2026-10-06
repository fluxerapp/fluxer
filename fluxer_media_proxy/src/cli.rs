// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{Config, StorageBackend};
use clap::{ArgAction, Parser, Subcommand, ValueEnum};

#[derive(Debug, Parser)]
#[command(name = "fluxer-media-proxy", disable_help_subcommand = true)]
pub struct Args {
    #[arg(long = "bind-host", value_name = "HOST")]
    pub bind_host: Option<String>,

    #[arg(long = "port", value_name = "PORT")]
    pub port: Option<u16>,

    #[arg(long = "mode", value_enum)]
    pub mode: Option<ModeArg>,

    #[arg(long = "storage-backend", value_enum)]
    pub storage_backend: Option<StorageBackendArg>,

    #[arg(long = "storage-root", value_name = "PATH")]
    pub storage_root: Option<String>,

    #[arg(long = "read-only", action = ArgAction::SetTrue)]
    pub read_only: bool,

    #[command(subcommand)]
    pub command: Option<Command>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Subcommand)]
pub enum Command {
    Healthcheck,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum ModeArg {
    Mp,
    Static,
    Upload,
    Relay,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum StorageBackendArg {
    Local,
    S3,
}

pub fn load_config(args: &Args) -> anyhow::Result<Config> {
    load_config_from_iter(args, std::env::vars())
}

pub fn load_config_from_iter<I, K, V>(args: &Args, vars: I) -> anyhow::Result<Config>
where
    I: IntoIterator<Item = (K, V)>,
    K: Into<String>,
    V: Into<String>,
{
    let mode_override = args.mode.map(|mode| {
        (
            "FLUXER_MEDIA_PROXY_MODE".to_owned(),
            mode.env_value().to_owned(),
        )
    });
    let vars = mode_override.into_iter().chain(
        vars.into_iter()
            .map(|(key, value)| (key.into(), value.into())),
    );
    let mut cfg = Config::load_from_iter(vars)?;
    apply_overrides(args, &mut cfg)?;
    Ok(cfg)
}

fn apply_overrides(args: &Args, cfg: &mut Config) -> anyhow::Result<()> {
    if let Some(bind_host) = args.bind_host.as_deref() {
        anyhow::ensure!(!bind_host.trim().is_empty(), "--bind-host cannot be empty");
        cfg.bind_host = bind_host.to_owned();
    }
    if let Some(port) = args.port {
        cfg.port = port;
    }
    if let Some(storage_backend) = args.storage_backend {
        cfg.storage.backend = storage_backend.into();
    }
    if let Some(storage_root) = args.storage_root.as_deref() {
        anyhow::ensure!(
            !storage_root.trim().is_empty(),
            "--storage-root cannot be empty"
        );
        cfg.storage.root = storage_root.to_owned();
    }
    if args.read_only {
        cfg.read_only = true;
    }
    Ok(())
}

impl ModeArg {
    fn env_value(self) -> &'static str {
        match self {
            Self::Mp => "mp",
            Self::Static => "static",
            Self::Upload => "upload",
            Self::Relay => "relay",
        }
    }
}

impl From<StorageBackendArg> for StorageBackend {
    fn from(value: StorageBackendArg) -> Self {
        match value {
            StorageBackendArg::Local => Self::Local,
            StorageBackendArg::S3 => Self::S3,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{DeploymentMode, PolicyMode};

    const RELAY_SECRET: &str = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";

    #[test]
    fn cli_overrides_env_config() {
        let args = Args::try_parse_from([
            "fluxer-media-proxy",
            "--bind-host",
            "127.0.0.1",
            "--port",
            "18080",
            "--mode",
            "static",
            "--storage-backend",
            "s3",
            "--storage-root",
            "/srv/media",
            "--read-only",
        ])
        .unwrap();
        let cfg = load_config_from_iter(
            &args,
            [
                ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
                ("FLUXER_MEDIA_PROXY_HOST", "0.0.0.0"),
                ("FLUXER_MEDIA_PROXY_PORT", "8080"),
            ],
        )
        .unwrap();
        assert_eq!("127.0.0.1", cfg.bind_host);
        assert_eq!(18080, cfg.port);
        assert_eq!(DeploymentMode::Static, cfg.mode);
        assert_eq!(StorageBackend::S3, cfg.storage.backend);
        assert_eq!("/srv/media", cfg.storage.root);
        assert!(cfg.read_only);
    }

    #[test]
    fn cli_mode_relay_selects_relay_mode() {
        let args = Args::try_parse_from(["fluxer-media-proxy", "--mode", "relay"]).unwrap();
        let cfg = load_config_from_iter(
            &args,
            [
                ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
                (
                    "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64",
                    RELAY_SECRET,
                ),
            ],
        )
        .unwrap();
        assert_eq!(DeploymentMode::Relay, cfg.mode);
    }

    #[test]
    fn cli_mode_decides_the_mode_dependent_config() {
        let relay = Args::try_parse_from(["fluxer-media-proxy", "--mode", "relay"]).unwrap();
        let cfg = load_config_from_iter(
            &relay,
            [
                ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
                ("FLUXER_MEDIA_PROXY_MODE", "mp"),
                ("FLUXER_MEDIA_PROXY_CORS_MODE", "enforce"),
                ("FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS", "not an origin"),
                (
                    "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64",
                    RELAY_SECRET,
                ),
            ],
        )
        .unwrap();
        assert_eq!(DeploymentMode::Relay, cfg.mode);
        assert_eq!(PolicyMode::Off, cfg.cors.mode);
        assert!(cfg.cors.allowed_origins.is_empty());

        let upload = Args::try_parse_from(["fluxer-media-proxy", "--mode", "upload"]).unwrap();
        let err = load_config_from_iter(
            &upload,
            [
                ("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret"),
                ("FLUXER_MEDIA_PROXY_MODE", "static"),
            ],
        )
        .unwrap_err();
        assert_eq!(
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 is required in upload and relay modes",
            err.to_string()
        );
    }

    #[test]
    fn cli_parses_healthcheck_subcommand() {
        assert_eq!(
            Some(Command::Healthcheck),
            Args::try_parse_from(["fluxer-media-proxy", "healthcheck"])
                .unwrap()
                .command
        );
        assert!(
            Args::try_parse_from(["fluxer-media-proxy"])
                .unwrap()
                .command
                .is_none()
        );
    }

    #[test]
    fn cli_rejects_empty_bind_host() {
        let args = Args::try_parse_from(["fluxer-media-proxy", "--bind-host", ""]).unwrap();
        let err = load_config_from_iter(&args, [("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")])
            .unwrap_err();
        assert!(err.to_string().contains("--bind-host"));
    }
}
