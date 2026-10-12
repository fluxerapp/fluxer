// SPDX-License-Identifier: AGPL-3.0-or-later

pub async fn run() -> anyhow::Result<()> {
    fluxer_common::healthcheck::run(
        fluxer_common::config::env_value("FLUXER_MEDIA_PROXY_HOST").as_deref(),
        fluxer_common::config::env_value("FLUXER_MEDIA_PROXY_PORT").as_deref(),
        "FLUXER_MEDIA_PROXY_HOST",
        "FLUXER_MEDIA_PROXY_PORT",
        8080,
    )
    .await
}
