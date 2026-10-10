// SPDX-License-Identifier: AGPL-3.0-or-later

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
            config::optional_env("RUST_LOG")
                .and_then(|filter| EnvFilter::try_new(filter).ok())
                .unwrap_or_else(|| EnvFilter::new("info")),
        )
        .try_init()
        .ok();
}
