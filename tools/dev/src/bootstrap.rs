// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::gateway::setup_gateway_config;
use crate::object_store::bootstrap_schema;
use crate::paths::{ensure_state_dirs, ensure_writable_dev_paths};
use crate::proc::{PNPM_INSTALL_ENV, RunOptions, run_command, wait_http_success, wait_tcp};
use anyhow::Result;

pub async fn bootstrap(skip_install: bool) -> Result<()> {
    ensure_state_dirs()?;
    ensure_writable_dev_paths()?;
    if !skip_install {
        run_command(
            &["pnpm", "install", "--frozen-lockfile"],
            RunOptions {
                env: PNPM_INSTALL_ENV
                    .iter()
                    .map(|(key, value)| ((*key).to_owned(), Some((*value).to_owned())))
                    .collect(),
                ..RunOptions::default()
            },
        )?;
    }
    setup_gateway_config()?;
    wait_core_infra().await?;
    bootstrap_schema().await?;
    println!("Fluxer dev bootstrap complete.");
    Ok(())
}

pub async fn post_start() -> Result<()> {
    ensure_state_dirs()?;
    ensure_writable_dev_paths()?;
    setup_gateway_config()?;
    crate::media_proxy::ensure_dev_object_store(true, 120).await?;
    Ok(())
}

pub async fn wait_core_infra() -> Result<()> {
    wait_tcp("Postgres", "postgres", 5432, 120).await?;
    wait_http_success("Meilisearch", "http://meilisearch:7700/health", 120).await?;
    wait_tcp("Valkey", "valkey", 6379, 120).await?;
    wait_tcp("NATS", "nats", 4222, 120).await?;
    wait_tcp("LiveKit", "livekit", 7880, 120).await?;
    crate::media_proxy::ensure_dev_object_store(true, 120).await?;
    Ok(())
}
