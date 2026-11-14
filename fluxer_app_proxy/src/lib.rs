// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod branding_images;
pub mod config;
pub mod csp;
mod instance_brand_colors;
mod local_files;
pub mod routes;
pub mod state;
mod static_asset_policy;

use anyhow::Context as _;
use config::AppProxyConfig;
use csp::CompiledCspPolicy;
use fluxer_common::config::EnvMap;
use fluxer_common::svc_span::CurrentSpanLayer;
use state::{AppState, build_http_client};
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio_util::sync::CancellationToken;

pub struct Config {
    pub app: AppProxyConfig,
    pub csp: CompiledCspPolicy,
}

impl Config {
    pub fn from_env(env: &EnvMap) -> anyhow::Result<Self> {
        let app = AppProxyConfig::from_env(env);
        let csp = CompiledCspPolicy::from_config(&app)
            .context("failed to compile the Fluxer app proxy content security policy")?;
        Ok(Self { app, csp })
    }
}

pub async fn run(config: Config, shutdown: CancellationToken) -> anyhow::Result<()> {
    let addr = format!("{}:{}", config.app.host, config.app.port);
    let http_client =
        build_http_client().context("failed to build Fluxer app proxy HTTP client")?;
    let state = AppState::load(Arc::new(config.app), Arc::new(config.csp), http_client)
        .await
        .context("failed to load the Fluxer app proxy static directory")?;

    let router = routes::build_router(state).layer(CurrentSpanLayer::new());
    let listener = TcpListener::bind(&addr)
        .await
        .with_context(|| format!("failed to bind Fluxer app proxy on {addr}"))?;
    tracing::info!(%addr, "starting Fluxer app proxy");

    axum::serve(listener, router)
        .with_graceful_shutdown(shutdown.cancelled_owned())
        .await
        .context("app proxy server exited unexpectedly")
}
