// SPDX-License-Identifier: AGPL-3.0-or-later

mod generator;
mod router_impl;
mod shard_impl;
mod types;
mod worker_guard;

use fluxer_common::config::EnvMap;
use fluxer_svc::config::{Mode, ServiceConfig};
use fluxer_svc::transport::NatsTransport;
use router_impl::SnowflakesRouter;
use shard_impl::SnowflakesShard;
use tokio_util::sync::CancellationToken;
use types::SERVICE_NAME;

const WORKER_GUARD_ENV: &str = "FLUXER_SNOWFLAKES_WORKER_GUARD";

pub struct Config {
    service: ServiceConfig,
    worker_guard: bool,
}

impl Config {
    pub fn from_env(env: &EnvMap) -> anyhow::Result<Self> {
        Ok(Self {
            service: ServiceConfig::from_env_map(env)?,
            worker_guard: env.read_bool(WORKER_GUARD_ENV, false),
        })
    }
}

pub async fn run(config: Config, shutdown: CancellationToken) -> anyhow::Result<()> {
    let Config {
        service: config,
        worker_guard,
    } = config;
    let transport =
        NatsTransport::connect(&config.nats_url, config.nats_auth_token.as_deref()).await?;
    tracing::info!(
        service = SERVICE_NAME,
        mode = ?config.mode,
        shard_id = config.shard_id,
        shard_count = config.shard_count,
        listen_addr = %config.listen_addr,
        worker_guard,
        "starting snowflakes service"
    );
    match config.mode {
        Mode::Router => {
            let router = SnowflakesRouter::new(config.shard_count);
            fluxer_svc::router::run_router(&config, router, transport, shutdown).await
        }
        Mode::Shard => {
            let shard = SnowflakesShard::new(config.shard_id)?;
            let serve = fluxer_svc::shard::run_shard(&config, shard, transport.clone(), shutdown);
            worker_guard::run(worker_guard, &transport, config.shard_id, serve).await
        }
        Mode::Combined => {
            let router = SnowflakesRouter::new(config.shard_count);
            let shard = SnowflakesShard::new(config.shard_id)?;
            let serve = fluxer_svc::combined::run_combined(
                &config,
                router,
                shard,
                transport.clone(),
                shutdown,
            );
            worker_guard::run(worker_guard, &transport, config.shard_id, serve).await
        }
    }
}
