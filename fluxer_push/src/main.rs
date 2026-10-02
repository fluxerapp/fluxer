// SPDX-License-Identifier: AGPL-3.0-or-later

use clap::Parser;
use fluxer_push::{cli, healthcheck, run};

#[tokio::main(flavor = "multi_thread")]
async fn main() -> anyhow::Result<()> {
    let args = cli::Args::parse();
    if matches!(args.command, Some(cli::Command::Healthcheck)) {
        return healthcheck::run(args.mode).await;
    }

    fluxer_svc::init_tracing();

    let cfg = cli::load_config(&args)?;
    run(cfg).await
}
