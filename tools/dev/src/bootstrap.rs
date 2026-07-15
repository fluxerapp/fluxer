// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::desktop::install_desktop;
use crate::gateway::setup_gateway_config;
use crate::paths::{ensure_state_dirs, ensure_writable_dev_paths};
use crate::proc::{PNPM_INSTALL_ENV, RunOptions, run_command, wait_http, wait_tcp};
use crate::smoke::{bootstrap_schema_and_object_store, run_smoke, wait_s3_api};
use anyhow::Result;

pub async fn bootstrap(skip_install: bool, skip_desktop_install: bool) -> Result<()> {
    ensure_state_dirs()?;
    ensure_writable_dev_paths()?;
    if !skip_install {
        crate::proc::run(&["corepack", "enable"])?;
        crate::proc::run(&["corepack", "prepare", "pnpm@10.29.3", "--activate"])?;
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
        if !skip_desktop_install {
            install_desktop()?;
        }
    }
    setup_gateway_config()?;
    wait_core_infra().await?;
    bootstrap_schema_and_object_store().await?;
    run_smoke(false, false).await?;
    println!("Fluxer dev bootstrap complete.");
    Ok(())
}

pub async fn post_start() -> Result<()> {
    ensure_state_dirs()?;
    ensure_writable_dev_paths()?;
    setup_gateway_config()?;
    crate::media_proxy::ensure_dev_object_store(true, 120).await?;
    run_smoke(true, false).await
}

pub async fn wait_core_infra() -> Result<()> {
    wait_tcp("Valkey", "valkey", 6379, 120).await?;
    wait_tcp("NATS", "nats", 4222, 120).await?;
    wait_tcp("LiveKit", "livekit", 7880, 120).await?;
    crate::media_proxy::ensure_dev_object_store(true, 120).await?;
    wait_tcp("SeaweedFS S3", "127.0.0.1", 8333, 120).await?;
    wait_http(
        "SeaweedFS master",
        "http://127.0.0.1:9333/cluster/status",
        120,
    )
    .await?;
    wait_s3_api(120).await
}
