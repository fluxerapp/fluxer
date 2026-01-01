/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

mod alerts;
mod api;
mod config;
mod db;

use std::net::SocketAddr;
use std::sync::Arc;

use axum::{
    Router,
    routing::{get, post},
};
use tower_http::cors::{Any, CorsLayer};
use tower_http::trace::TraceLayer;
use tracing::info;
use tracing_subscriber::{EnvFilter, fmt, prelude::*};

use api::ingest::{
    AppState, ingest_batch, ingest_counter, ingest_crash, ingest_gauge, ingest_histogram,
};
use api::query::{
    health_check, query_aggregate, query_crashes, query_metrics, query_percentiles, query_top,
};
use config::{Config, MetricsMode};
use db::{ClickHouseStorage, NoOpStorage, Storage};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let _ = dotenvy::dotenv();

    tracing_subscriber::registry()
        .with(fmt::layer().with_target(true))
        .with(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info,tonbo=warn")),
        )
        .init();

    info!("Starting fluxer_metrics service");

    let config = Config::from_env().unwrap_or_else(|e| {
        tracing::error!("Configuration error: {}", e);
        std::process::exit(1);
    });

    let mode_str = match config.metrics_mode {
        MetricsMode::ClickHouse => "clickhouse",
        MetricsMode::NoOp => "noop",
    };

    info!(
        "Configuration loaded: port={}, mode={}, alert_webhook={}",
        config.port,
        mode_str,
        config.alert_webhook_url.is_some()
    );

    if config.metrics_mode == MetricsMode::ClickHouse {
        info!(
            "ClickHouse config: url={}, database={}",
            config.clickhouse_url, config.clickhouse_database
        );
    }

    let storage: Box<dyn Storage> = match config.metrics_mode {
        MetricsMode::ClickHouse => Box::new(ClickHouseStorage::new(&config).await?),
        MetricsMode::NoOp => Box::new(NoOpStorage::new()),
    };

    let state = Arc::new(AppState {
        storage,
        config: config.clone(),
    });

    let app = Router::new()
        .route("/metrics/counter", post(ingest_counter))
        .route("/metrics/gauge", post(ingest_gauge))
        .route("/metrics/histogram", post(ingest_histogram))
        .route("/metrics/crash", post(ingest_crash))
        .route("/metrics/batch", post(ingest_batch))
        .route("/query", get(query_metrics))
        .route("/query/aggregate", get(query_aggregate))
        .route("/query/percentiles", get(query_percentiles))
        .route("/query/top", get(query_top))
        .route("/query/crashes", get(query_crashes))
        .route("/_health", get(health_check))
        .layer(
            CorsLayer::new()
                .allow_origin(Any)
                .allow_methods(Any)
                .allow_headers(Any),
        )
        .layer(TraceLayer::new_for_http())
        .with_state(state);

    let addr = SocketAddr::from(([0, 0, 0, 0], config.port));
    info!("Listening on {}", addr);

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
