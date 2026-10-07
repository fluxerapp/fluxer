// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Context;
use fluxer_app_proxy::{
    config::AppProxyConfig,
    csp::CompiledCspPolicy,
    routes::build_router,
    state::{AppState, build_http_client},
};
use std::sync::Arc;
use tokio::{net::TcpListener, runtime::Builder};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

fn main() -> anyhow::Result<()> {
    tracing_subscriber::registry()
        .with(fluxer_common::config::env_filter("info"))
        .with(tracing_subscriber::fmt::layer())
        .init();

    let config = Arc::new(AppProxyConfig::from_env());
    let addr = format!("{}:{}", config.host, config.port);

    let csp = Arc::new(
        CompiledCspPolicy::from_config(&config)
            .context("failed to compile the Fluxer app proxy content security policy")?,
    );

    let runtime = Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("failed to create Fluxer app proxy async runtime")?;

    runtime.block_on(async move {
        let http_client =
            build_http_client().context("failed to build Fluxer app proxy HTTP client")?;
        let state = AppState::load(config, csp, http_client)
            .await
            .context("failed to load the Fluxer app proxy static directory")?;

        let router = build_router(state);
        let listener = TcpListener::bind(&addr)
            .await
            .with_context(|| format!("failed to bind Fluxer app proxy on {addr}"))?;
        tracing::info!(%addr, "starting Fluxer app proxy");

        axum::serve(listener, router)
            .with_graceful_shutdown(shutdown_signal())
            .await
            .context("app proxy server exited unexpectedly")?;

        Ok(())
    })
}

async fn shutdown_signal() {
    let ctrl_c = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .expect("failed to install SIGTERM handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = ctrl_c => {},
        _ = terminate => {},
    }
}
