// SPDX-License-Identifier: AGPL-3.0-or-later

use clap::Parser;
use fluxer_common::config::EnvMap;
use fluxer_push::{cli, run};

#[tokio::main(flavor = "multi_thread")]
async fn main() -> anyhow::Result<()> {
    let args = cli::Args::parse();
    let env = EnvMap::from_process()?;
    if matches!(args.command, Some(cli::Command::Healthcheck)) {
        return fluxer_common::healthcheck::run(fluxer_common::healthcheck::address(
            &env,
            "FLUXER_PUSH_SERVICE_HOST",
            "FLUXER_PUSH_SERVICE_PORT",
            args.mode.default_port(),
        )?)
        .await;
    }

    fluxer_svc::init_tracing(&env);

    let cfg = cli::load_config(&args, &env)?;
    run(cfg, fluxer_common::shutdown::signal_token()).await
}
