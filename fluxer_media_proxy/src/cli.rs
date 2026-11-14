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
    let vars =
        fluxer_common::config::resolve_env_files(std::env::vars()).map_err(anyhow::Error::msg)?;
    load_config_from_iter(args, vars)
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
