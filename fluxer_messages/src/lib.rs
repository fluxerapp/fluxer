// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod mention_extractor;
pub mod router_impl;
pub mod shard_impl;
pub mod types;
pub mod udt;

use fluxer_common::config::EnvMap;
use fluxer_svc::config::{DatabaseBackend, Mode, ServiceConfig};
use fluxer_svc::transport::NatsTransport;
use router_impl::MessagesRouter;
use shard_impl::MessagesShard;
use tokio_util::sync::CancellationToken;

pub struct Config {
    service: ServiceConfig,
}

impl Config {
    pub fn from_env(env: &EnvMap) -> anyhow::Result<Self> {
        Ok(Self {
            service: ServiceConfig::from_env_map(env)?,
        })
    }
}

pub async fn run(config: Config, shutdown: CancellationToken) -> anyhow::Result<()> {
    let config = config.service;
    let transport =
        NatsTransport::connect(&config.nats_url, config.nats_auth_token.as_deref()).await?;

    tracing::info!(
        service = config.service_name,
        mode = ?config.mode,
        shard_id = config.shard_id,
        shard_count = config.shard_count,
        listen_addr = %config.listen_addr,
        "starting messages service"
    );

    match config.mode {
        Mode::Router => {
            let router = MessagesRouter::new();
            fluxer_svc::router::run_router(&config, router, transport, shutdown).await
        }
        Mode::Shard => {
            let shard = build_shard(&config, &transport).await?;
            fluxer_svc::shard::run_shard(&config, shard, transport, shutdown).await
        }
        Mode::Combined => {
            let shard = build_shard(&config, &transport).await?;
            let router = MessagesRouter::new();
            fluxer_svc::combined::run_combined(&config, router, shard, transport, shutdown).await
        }
    }
}

async fn build_shard(
    config: &ServiceConfig,
    transport: &NatsTransport,
) -> anyhow::Result<MessagesShard<NatsTransport>> {
    match config.database_backend {
        DatabaseBackend::Postgres => {
            let postgres_config = fluxer_svc::postgres::PostgresConfig::from_service_config(config);
            let pool = fluxer_svc::postgres::connect(&postgres_config).await?;
            let kv = fluxer_svc::postgres::KvClient::new(pool, &postgres_config)?;
            MessagesShard::new_postgres(kv, transport.clone())
        }
        DatabaseBackend::Cassandra => {
            #[cfg(feature = "scylla")]
            {
                let scylla_config = fluxer_svc::scylla::ScyllaConfig::from_service_config(config);
                let db = fluxer_svc::scylla::connect(&scylla_config).await?;
                MessagesShard::new_scylla(db, transport.clone()).await
            }
            #[cfg(not(feature = "scylla"))]
            {
                anyhow::bail!("FLUXER_DATABASE_BACKEND=cassandra requires the scylla feature");
            }
        }
    }
}
