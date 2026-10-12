// SPDX-License-Identifier: AGPL-3.0-or-later

pub async fn run(mode: crate::config::Mode) -> anyhow::Result<()> {
    fluxer_common::healthcheck::run(
        fluxer_common::config::env_value("FLUXER_PUSH_SERVICE_HOST").as_deref(),
        fluxer_common::config::env_value("FLUXER_PUSH_SERVICE_PORT").as_deref(),
        "FLUXER_PUSH_SERVICE_HOST",
        "FLUXER_PUSH_SERVICE_PORT",
        mode.default_port(),
    )
    .await
}
