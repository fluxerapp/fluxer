// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::config::EnvMap;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let env = EnvMap::from_process()?;
    fluxer_svc::init_tracing(&env);
    let config = fluxer_messages::Config::from_env(&env)?;
    fluxer_messages::run(config, fluxer_common::shutdown::signal_token()).await
}
