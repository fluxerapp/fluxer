// SPDX-License-Identifier: AGPL-3.0-or-later

mod klipy;
mod media_proxy;
mod router_impl;
mod shard_impl;
mod types;

use fluxer_common::config::EnvMap;
use fluxer_svc::config::{Mode, ServiceConfig};
use fluxer_svc::transport::NatsTransport;
use router_impl::GifsRouter;
use shard_impl::{GifsShard, ShardConfig};
use tokio_util::sync::CancellationToken;

pub struct Config {
    service: ServiceConfig,
    role: Role,
}

enum Role {
    Router,
    Shard(ShardConfig),
    Combined(ShardConfig),
}

impl Config {
    pub fn from_env(env: &EnvMap) -> anyhow::Result<Self> {
        let service = ServiceConfig::from_env_map(env)?;
        let role = match service.mode {
            Mode::Router => Role::Router,
            Mode::Shard => Role::Shard(ShardConfig::from_env(env)?),
            Mode::Combined => Role::Combined(ShardConfig::from_env(env)?),
        };
        Ok(Self { service, role })
    }
}

pub async fn run(config: Config, shutdown: CancellationToken) -> anyhow::Result<()> {
    let Config {
        service: config,
        role,
    } = config;
    let transport =
        NatsTransport::connect(&config.nats_url, config.nats_auth_token.as_deref()).await?;

    tracing::info!(
        service = config.service_name,
        mode = ?config.mode,
        shard_id = config.shard_id,
        shard_count = config.shard_count,
        listen_addr = %config.listen_addr,
        "starting gifs service"
    );

    match role {
        Role::Router => {
            let router = GifsRouter::new(config.cache_max_entries, config.cache_ttl);
            fluxer_svc::router::run_router(&config, router, transport, shutdown).await
        }
        Role::Shard(shard_config) => {
            let shard = GifsShard::new(shard_config)?;
            fluxer_svc::shard::run_shard(&config, shard, transport, shutdown).await
        }
        Role::Combined(shard_config) => {
            let router = GifsRouter::new(config.cache_max_entries, config.cache_ttl);
            let shard = GifsShard::new(shard_config)?;
            fluxer_svc::combined::run_combined(&config, router, shard, transport, shutdown).await
        }
    }
}
