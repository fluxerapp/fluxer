// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{Config, Mode};
use anyhow::Context as _;
use clap::{Parser, Subcommand};
use std::net::IpAddr;

#[derive(Debug, Parser)]
#[command(name = "fluxer-push", disable_help_subcommand = true)]
pub struct Args {
    #[arg(long = "mode", value_name = "MODE", value_enum, default_value_t = Mode::Delivery, global = true)]
    pub mode: Mode,

    #[arg(long = "bind-host", value_name = "HOST")]
    pub bind_host: Option<String>,

    #[arg(long = "port", value_name = "PORT")]
    pub port: Option<u16>,

    #[command(subcommand)]
    pub command: Option<Command>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Subcommand)]
pub enum Command {
    Healthcheck,
}

pub fn load_config(args: &Args) -> anyhow::Result<Config> {
    load_config_from_iter(args, std::env::vars())
}

fn load_config_from_iter<I, K, V>(args: &Args, vars: I) -> anyhow::Result<Config>
where
    I: IntoIterator<Item = (K, V)>,
    K: Into<String>,
    V: Into<String>,
{
    let mut cfg = Config::load_from_iter(args.mode, vars)?;
    apply_overrides(args, &mut cfg)?;
    Ok(cfg)
}

fn apply_overrides(args: &Args, cfg: &mut Config) -> anyhow::Result<()> {
    let bind_addr = cfg.bind_addr_mut();
    if let Some(bind_host) = args.bind_host.as_deref() {
        let bind_host = bind_host.trim();
        anyhow::ensure!(!bind_host.is_empty(), "--bind-host cannot be empty");
        bind_addr.set_ip(
            bind_host
                .parse::<IpAddr>()
                .with_context(|| format!("--bind-host is not an IP address: {bind_host}"))?,
        );
    }
    if let Some(port) = args.port {
        bind_addr.set_port(port);
    }
    Ok(())
}
