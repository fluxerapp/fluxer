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

use std::sync::Arc;

use axum::{
    Json,
    extract::State,
    http::StatusCode,
    response::{IntoResponse, Response},
};
use tracing::{error, info, warn};

use crate::alerts::send_discord_crash_alert;
use crate::config::Config;
use crate::db::{CounterRequest, CrashRequest, GaugeRequest, HistogramRequest, Storage};

pub struct AppState {
    pub storage: Box<dyn Storage>,
    pub config: Config,
}

pub async fn ingest_counter(
    State(state): State<Arc<AppState>>,
    Json(req): Json<CounterRequest>,
) -> StatusCode {
    match state.storage.insert_counter(req).await {
        Ok(()) => StatusCode::ACCEPTED,
        Err(e) => {
            error!("Failed to insert counter: {}", e);
            StatusCode::INTERNAL_SERVER_ERROR
        }
    }
}

pub async fn ingest_gauge(
    State(state): State<Arc<AppState>>,
    Json(req): Json<GaugeRequest>,
) -> StatusCode {
    match state.storage.insert_gauge(req).await {
        Ok(()) => StatusCode::ACCEPTED,
        Err(e) => {
            error!("Failed to insert gauge: {}", e);
            StatusCode::INTERNAL_SERVER_ERROR
        }
    }
}

pub async fn ingest_histogram(
    State(state): State<Arc<AppState>>,
    Json(req): Json<HistogramRequest>,
) -> StatusCode {
    match state.storage.insert_histogram(req).await {
        Ok(()) => StatusCode::ACCEPTED,
        Err(e) => {
            error!("Failed to insert histogram: {}", e);
            StatusCode::INTERNAL_SERVER_ERROR
        }
    }
}

#[allow(clippy::cognitive_complexity)]
pub async fn ingest_crash(
    State(state): State<Arc<AppState>>,
    Json(req): Json<CrashRequest>,
) -> Response {
    let guild_id = req.guild_id.clone();

    match state.storage.insert_crash(req).await {
        Ok(event) => {
            info!("Recorded crash for guild {}", guild_id);

            if let Some(webhook_url) = &state.config.alert_webhook_url {
                let admin_endpoint = state.config.admin_endpoint.as_deref();
                match send_discord_crash_alert(webhook_url, &event, admin_endpoint).await {
                    Ok(()) => {
                        if let Err(e) = state.storage.mark_crash_notified(&event.id).await {
                            warn!("Failed to mark crash as notified: {}", e);
                        }
                    }
                    Err(e) => {
                        error!("Failed to send Discord alert: {}", e);
                    }
                }
            }

            StatusCode::ACCEPTED.into_response()
        }
        Err(e) => {
            error!("Failed to insert crash: {}", e);
            StatusCode::INTERNAL_SERVER_ERROR.into_response()
        }
    }
}

#[derive(serde::Deserialize)]
pub struct BatchRequest {
    #[serde(default)]
    pub counters: Vec<CounterRequest>,
    #[serde(default)]
    pub gauges: Vec<GaugeRequest>,
    #[serde(default)]
    pub histograms: Vec<HistogramRequest>,
}

#[allow(clippy::cognitive_complexity)]
pub async fn ingest_batch(
    State(state): State<Arc<AppState>>,
    Json(req): Json<BatchRequest>,
) -> StatusCode {
    let mut had_error = false;

    for counter in req.counters {
        if let Err(e) = state.storage.insert_counter(counter).await {
            error!("Failed to insert counter in batch: {}", e);
            had_error = true;
        }
    }

    for gauge in req.gauges {
        if let Err(e) = state.storage.insert_gauge(gauge).await {
            error!("Failed to insert gauge in batch: {}", e);
            had_error = true;
        }
    }

    for histogram in req.histograms {
        if let Err(e) = state.storage.insert_histogram(histogram).await {
            error!("Failed to insert histogram in batch: {}", e);
            had_error = true;
        }
    }

    if had_error {
        StatusCode::PARTIAL_CONTENT
    } else {
        StatusCode::ACCEPTED
    }
}
