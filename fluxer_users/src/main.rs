// SPDX-License-Identifier: AGPL-3.0-or-later

mod router_impl;
mod shard_impl;
mod types;

use fluxer_svc::config::{DatabaseBackend, Mode, ServiceConfig};
use fluxer_svc::transport::NatsTransport;
use router_impl::UsersRouter;
use shard_impl::UsersShard;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    fluxer_svc::init_tracing();
    let config = ServiceConfig::from_env()?;
    let transport = NatsTransport::connect(&config.nats_url).await?;

    tracing::info!(
        service = config.service_name,
        mode = ?config.mode,
        shard_id = config.shard_id,
        shard_count = config.shard_count,
        listen_addr = %config.listen_addr,
        "starting users service"
    );

    match config.mode {
        Mode::Router => {
            let router = UsersRouter::new(config.cache_max_entries, config.cache_ttl);
            fluxer_svc::router::run_router(&config, router, transport).await
        }
        Mode::Shard => {
            let shard = match config.database_backend {
                DatabaseBackend::Postgres => {
                    let postgres_config =
                        fluxer_svc::postgres::PostgresConfig::from_service_config(&config);
                    let pool = fluxer_svc::postgres::connect(&postgres_config).await?;
                    let kv = fluxer_svc::postgres::KvClient::new(pool, &postgres_config)?;
                    UsersShard::new_postgres(
                        kv,
                        transport.clone(),
                        config.cache_max_entries,
                        config.cache_ttl,
                    )?
                }
                DatabaseBackend::Cassandra => {
                    #[cfg(feature = "scylla")]
                    {
                        let scylla_config =
                            fluxer_svc::scylla::ScyllaConfig::from_service_config(&config);
                        let db = fluxer_svc::scylla::connect(&scylla_config).await?;
                        UsersShard::new_scylla(
                            db,
                            transport.clone(),
                            config.cache_max_entries,
                            config.cache_ttl,
                        )
                        .await?
                    }
                    #[cfg(not(feature = "scylla"))]
                    {
                        anyhow::bail!(
                            "FLUXER_DATABASE_BACKEND=cassandra requires the scylla feature"
                        );
                    }
                }
            };
            fluxer_svc::shard::run_shard(&config, shard, transport).await
        }
    }
}
