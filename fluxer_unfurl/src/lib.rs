// SPDX-License-Identifier: AGPL-3.0-or-later

mod activity_pub;
mod cache_policy;
mod charset;
mod direct_media;
mod dns_txt;
mod domain_verification;
mod embed_normalizer;
mod external_fetch;
mod html_markdown;
mod html_parser;
mod http_fetch;
mod media_proxy;
mod network_policy;
mod oembed;
mod resolvers;
mod router_impl;
mod sanitizer;
mod shard_impl;
mod text_limits;
mod types;

use fluxer_common::config::EnvMap;
use fluxer_svc::config::{Mode, ServiceConfig};
use fluxer_svc::transport::NatsTransport;
use router_impl::UnfurlRouter;
use shard_impl::{ShardConfig, UnfurlShard};
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
        "starting unfurl service"
    );

    match role {
        Role::Router => {
            let router = UnfurlRouter::new(config.cache_max_entries, config.cache_ttl);
            fluxer_svc::router::run_router(&config, router, transport, shutdown).await
        }
        Role::Shard(shard_config) => {
            let shard = UnfurlShard::new(config.cache_ttl, shard_config)?;
            fluxer_svc::shard::run_shard(&config, shard, transport, shutdown).await
        }
        Role::Combined(shard_config) => {
            let router = UnfurlRouter::new(config.cache_max_entries, config.cache_ttl);
            let shard = UnfurlShard::new(config.cache_ttl, shard_config)?;
            fluxer_svc::combined::run_combined(&config, router, shard, transport, shutdown).await
        }
    }
}
