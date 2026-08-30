// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Context;
use fluxer_app_proxy::{
    config::AppProxyConfig,
    discovery_cache::DiscoveryCache,
    geoip,
    invite_meta::InviteMetaResolver,
    routes::build_router,
    state::{AppState, build_http_client},
};
use std::sync::Arc;
use tokio::{net::TcpListener, runtime::Builder};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

fn main() -> anyhow::Result<()> {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let config = AppProxyConfig::from_env();
    let addr = format!("{}:{}", config.host, config.port);

    let geoip = Arc::new(geoip::resolver_from_app_config(&config));

    let runtime = Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("failed to create Fluxer app proxy async runtime")?;

    runtime.block_on(async move {
        let http_client =
            build_http_client().context("failed to build Fluxer app proxy HTTP client")?;
        let discovery_cache = Arc::new(DiscoveryCache::new());

        if let Err(err) = discovery_cache
            .refresh(&http_client, &config.discovery_upstream_url)
            .await
        {
            tracing::warn!(%err, url = %config.discovery_upstream_url, "initial discovery fetch failed; will retry in background");
        }

        let cancel = discovery_cache.start_background_refresh(
            http_client.clone(),
            config.discovery_upstream_url.clone(),
            config.discovery_refresh_interval_ms,
        );

        let invite_meta = if config.invite_meta_enabled {
            match InviteMetaResolver::connect(&config).await {
                Ok(resolver) => Some(Arc::new(resolver)),
                Err(err) => {
                    tracing::warn!(%err, "invite metadata resolver disabled; failed to connect to database");
                    None
                }
            }
        } else {
            None
        };

        let index_html = if config.index_upstream_url.is_none() {
            let index_path = std::path::Path::new(&config.static_dir).join("index.html");
            match tokio::fs::read_to_string(&index_path).await {
                Ok(contents) => Some(Arc::<str>::from(contents)),
                Err(err) => {
                    tracing::warn!(path = ?index_path, %err, "failed to preload index.html; will read per request");
                    None
                }
            }
        } else {
            None
        };

        let state = AppState {
            config: Arc::new(config),
            http_client,
            discovery_cache,
            geoip,
            invite_meta,
            index_html,
        };

        let router = build_router(state);
        let listener = TcpListener::bind(&addr)
            .await
            .with_context(|| format!("failed to bind Fluxer app proxy on {addr}"))?;
        tracing::info!(%addr, "starting Fluxer app proxy");

        axum::serve(listener, router)
            .with_graceful_shutdown(shutdown_signal())
            .await
            .context("app proxy server exited unexpectedly")?;

        cancel.abort();
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
