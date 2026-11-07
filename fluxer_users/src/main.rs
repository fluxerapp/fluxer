// SPDX-License-Identifier: AGPL-3.0-or-later

mod app_attest;
mod play_integrity;
mod pseudonym;
mod router_impl;
mod shard_impl;
mod types;

use fluxer_svc::config::{DatabaseBackend, Mode, ServiceConfig, optional_env};
use fluxer_svc::transport::NatsTransport;
use router_impl::UsersRouter;
use shard_impl::UsersShard;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    fluxer_svc::init_tracing();
    let config = ServiceConfig::from_env()?;
    let transport =
        NatsTransport::connect(&config.nats_url, config.nats_auth_token.as_deref()).await?;

    tracing::info!(
        service = config.service_name,
        mode = ?config.mode,
        shard_id = config.shard_id,
        shard_count = config.shard_count,
        listen_addr = %config.listen_addr,
        "starting users service"
    );

    match config.mode {
        Mode::Router => fluxer_svc::router::run_router(&config, UsersRouter, transport).await,
        Mode::Shard => {
            let shard = build_shard(&config).await?;
            fluxer_svc::shard::run_shard(&config, shard, transport).await
        }
        Mode::Combined => {
            let shard = build_shard(&config).await?;
            fluxer_svc::combined::run_combined(&config, UsersRouter, shard, transport).await
        }
    }
}

fn is_self_hosted() -> bool {
    matches!(
        optional_env("FLUXER_SELF_HOSTED")
            .map(|value| value.trim().to_ascii_lowercase())
            .as_deref(),
        Some("1" | "true" | "yes" | "y" | "on")
    )
}

async fn build_shard(config: &ServiceConfig) -> anyhow::Result<UsersShard> {
    pseudonym::configure_from_env()?;
    let self_hosted = is_self_hosted();
    let app_attest = if self_hosted {
        app_attest::AppAttestConfig::default()
    } else {
        app_attest::AppAttestConfig::from_env()?
    };
    tracing::info!(
        app_ids = app_attest.app_ids.len(),
        allow_development = app_attest.allow_development,
        "app attest configuration"
    );
    let play_integrity = if self_hosted {
        play_integrity::PlayIntegrityConfig::default()
    } else {
        play_integrity::PlayIntegrityConfig::from_env()?
    };
    tracing::info!(
        package_names = play_integrity.package_names.len(),
        cert_digests = play_integrity.cert_digests.len(),
        device_integrity = ?play_integrity.device_integrity,
        "play integrity configuration"
    );
    match config.database_backend {
        DatabaseBackend::Postgres => {
            let postgres_config = fluxer_svc::postgres::PostgresConfig::from_service_config(config);
            let pool = fluxer_svc::postgres::connect(&postgres_config).await?;
            let kv = fluxer_svc::postgres::KvClient::new(pool, &postgres_config)?;
            UsersShard::new_postgres(
                kv,
                config.cache_max_entries,
                config.cache_ttl,
                app_attest,
                play_integrity,
            )
        }
        DatabaseBackend::Cassandra => {
            #[cfg(feature = "scylla")]
            {
                let scylla_config = fluxer_svc::scylla::ScyllaConfig::from_service_config(config);
                let db = fluxer_svc::scylla::connect(&scylla_config).await?;
                UsersShard::new_scylla(
                    db,
                    config.cache_max_entries,
                    config.cache_ttl,
                    app_attest,
                    play_integrity,
                )
                .await
            }
            #[cfg(not(feature = "scylla"))]
            {
                anyhow::bail!("FLUXER_DATABASE_BACKEND=cassandra requires the scylla feature");
            }
        }
    }
}
