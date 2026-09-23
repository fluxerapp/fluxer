// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod cli;
pub mod config;
mod crypto;
mod dedupe;
mod delivery;
pub mod healthcheck;
mod job;
mod metrics;
mod payload;
mod providers;
mod relay;
mod resolver;
mod retry;
mod rollout;
mod rpc;
mod secret;
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

fn unix_millis() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |since| since.as_millis() as i64)
}
