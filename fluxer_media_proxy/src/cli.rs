// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{Config, DeploymentMode, StorageBackend};
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
    if let Some(mode) = args.mode {
        cfg.mode = mode.into();
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

impl From<ModeArg> for DeploymentMode {
    fn from(value: ModeArg) -> Self {
        match value {
            ModeArg::Mp => Self::Mp,
            ModeArg::Static => Self::Static,
            ModeArg::Upload => Self::Upload,
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
