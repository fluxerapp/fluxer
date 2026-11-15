// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod acl;
pub mod admin_flags;
pub mod admin_hints;
pub mod api;
pub mod config;
pub mod fonts;
pub mod middleware;
pub mod oauth2;
pub mod routes;
pub mod session;
pub mod sso;
pub mod state;
pub mod templates;
pub mod utils;

pub use config::AdminConfig as Config;
pub use routes::build_router;

use anyhow::Context as _;
use fluxer_common::svc_span::CurrentSpanLayer;
use tokio::net::TcpListener;
use tokio_util::sync::CancellationToken;

pub async fn run(config: Config, shutdown: CancellationToken) -> anyhow::Result<()> {
    let addr = format!("{}:{}", config.host, config.port);
    let router = build_router(config)?.layer(CurrentSpanLayer::new());

    let listener = TcpListener::bind(&addr)
        .await
        .with_context(|| format!("failed to bind Fluxer admin service on {addr}"))?;
    tracing::info!(%addr, "starting Fluxer admin service");

    axum::serve(listener, router)
        .with_graceful_shutdown(shutdown.cancelled_owned())
        .await
        .context("admin server exited unexpectedly")
}
