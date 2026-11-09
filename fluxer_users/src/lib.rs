// SPDX-License-Identifier: AGPL-3.0-or-later

mod app_attest;
mod play_integrity;
mod pseudonym;
mod router_impl;
mod shard_impl;
mod types;

use app_attest::AppAttestConfig;
use fluxer_common::config::EnvMap;
use fluxer_svc::config::{DatabaseBackend, Mode, ServiceConfig};
use fluxer_svc::transport::NatsTransport;
use play_integrity::PlayIntegrityConfig;
use router_impl::UsersRouter;
use shard_impl::UsersShard;
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

struct ShardConfig {
    pseudonym_secret: Vec<u8>,
    app_attest: AppAttestConfig,
    play_integrity: PlayIntegrityConfig,
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

impl ShardConfig {
    fn from_env(env: &EnvMap) -> anyhow::Result<Self> {
        let pseudonym_secret = pseudonym::secret_from_env(env)?;
        let self_hosted = is_self_hosted(env);
        let app_attest = if self_hosted {
            AppAttestConfig::default()
        } else {
            AppAttestConfig::from_env(env)?
        };
        let play_integrity = if self_hosted {
            PlayIntegrityConfig::default()
        } else {
            PlayIntegrityConfig::from_env(env)?
        };
        Ok(Self {
            pseudonym_secret,
            app_attest,
            play_integrity,
        })
    }
}

fn is_self_hosted(env: &EnvMap) -> bool {
    matches!(
        env.get("FLUXER_SELF_HOSTED")
            .map(|value| value.trim().to_ascii_lowercase())
            .as_deref(),
        Some("1" | "true" | "yes" | "y" | "on")
    )
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
        "starting users service"
    );

    match role {
        Role::Router => {
            fluxer_svc::router::run_router(&config, UsersRouter, transport, shutdown).await
        }
        Role::Shard(shard_config) => {
            let shard = build_shard(&config, shard_config).await?;
            fluxer_svc::shard::run_shard(&config, shard, transport, shutdown).await
        }
        Role::Combined(shard_config) => {
            let shard = build_shard(&config, shard_config).await?;
            fluxer_svc::combined::run_combined(&config, UsersRouter, shard, transport, shutdown)
                .await
        }
    }
}

async fn build_shard(config: &ServiceConfig, shard: ShardConfig) -> anyhow::Result<UsersShard> {
    let ShardConfig {
        pseudonym_secret,
        app_attest,
        play_integrity,
    } = shard;
    pseudonym::configure(pseudonym_secret)?;
    tracing::info!(
        app_ids = app_attest.app_ids.len(),
        allow_development = app_attest.allow_development,
        "app attest configuration"
    );
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
