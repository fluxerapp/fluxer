// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::{Config, Mode};
use anyhow::Context as _;
use clap::{Parser, Subcommand};
use fluxer_common::config::EnvMap;
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

pub fn load_config(args: &Args, env: &EnvMap) -> anyhow::Result<Config> {
    let mut cfg = Config::load_from_iter(args.mode, env.iter())?;
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
    let service_port = bind_addr.port();
    crate::config::ensure_metrics_port_differs(cfg.metrics_port(), service_port)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn env(vars: &[(&str, &str)]) -> EnvMap {
        EnvMap::from_pairs(vars.iter().copied())
    }

    #[test]
    fn a_port_override_cannot_land_on_the_metrics_port() {
        let args = Args {
            mode: Mode::Relay,
            bind_host: None,
            port: Some(8082),
            command: None,
        };
        assert!(load_config(&args, &env(&[("FLUXER_PUSH_METRICS_PORT", "8082")])).is_err());
        let args = Args {
            port: Some(9000),
            ..args
        };
        let cfg =
            load_config(&args, &env(&[("FLUXER_PUSH_METRICS_PORT", "8082")])).expect("config");
        assert_eq!(Some(8082), cfg.metrics_port());
    }
}
