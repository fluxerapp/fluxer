// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod cache;
pub mod config;
pub mod hash_ring;
pub mod metrics;
pub mod postgres;
pub mod router;
pub mod server;
pub mod shard;
pub mod shutdown;
pub mod transport;

#[cfg(feature = "scylla")]
pub mod scylla;

use tracing_subscriber::EnvFilter;

pub fn init_tracing() {
    tracing_subscriber::fmt()
        .json()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .try_init()
        .ok();
}

pub async fn run<S>(service: S) -> anyhow::Result<()>
where
    S: router::RouterService,
    S::Response: Clone,
{
    init_tracing();
    let config = config::ServiceConfig::from_env()?;
    let transport =
        transport::NatsTransport::connect(&config.nats_url, config.nats_auth_token.as_deref())
            .await?;
    tracing::info!(
        service = config.service_name,
        mode = ?config.mode,
        shard_id = config.shard_id,
        shard_count = config.shard_count,
        listen_addr = %config.listen_addr,
        "starting service"
    );
    match config.mode {
        config::Mode::Router => router::run_router(&config, service, transport).await,
        config::Mode::Shard => {
            anyhow::bail!(
                "run() with RouterService cannot be used in shard mode; use run_shard() directly"
            )
        }
    }
}
