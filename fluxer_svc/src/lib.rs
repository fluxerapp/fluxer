// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod combined;
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

pub fn init_tracing() {
    tracing_subscriber::fmt()
        .json()
        .with_env_filter(fluxer_common::config::env_filter("info"))
        .try_init()
        .ok();
}
