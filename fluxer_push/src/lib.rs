// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod cli;
pub mod config;
mod crypto;
mod dedupe;
mod delivery;
mod job;
mod metrics;
mod payload;
mod providers;
mod relay;
mod relay_consent;
mod resolver;
mod retry;
mod rpc;
pub mod server;
mod subscription;
mod tokens;
mod vendor;

pub use server::run;

fn unix_seconds() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |since| since.as_secs() as i64)
}
