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
    extract::{Query, State},
    http::StatusCode,
    response::{IntoResponse, Response},
};
use serde::{Deserialize, Serialize};
use time::{
    Duration, OffsetDateTime, format_description::well_known::Rfc3339, macros::format_description,
};
use tracing::error;

use crate::api::ingest::AppState;
use crate::db::{
    QueryParams, QueryResponse, Resolution, TopEntry, TopQueryParams, TopQueryResponse,
};

const GAUGE_METRICS: &[(&str, &str)] = &[
    ("guild.member_count", "guild_id"),
    ("user.guild_membership_count", "user_id"),
];

const METRIC_TYPE_COUNTER: &str = "counter";
const METRIC_TYPE_GAUGE: &str = "gauge";
const METRIC_TYPE_HISTOGRAM: &str = "histogram";

fn parse_datetime(s: &str) -> Option<OffsetDateTime> {
    if let Ok(dt) = OffsetDateTime::parse(s, &Rfc3339) {
        return Some(dt);
    }
    let date_format = format_description!("[year]-[month]-[day]");
    if let Ok(date) = time::Date::parse(s, &date_format) {
        return Some(date.midnight().assume_utc());
    }
    None
}

fn to_millis(dt: OffsetDateTime) -> i64 {
    (dt.unix_timestamp_nanos() / 1_000_000) as i64
}

fn infer_metric_type(metric: &str) -> &'static str {
    let is_percentile_metric = metric.ends_with(".p50")
        || metric.ends_with(".p95")
        || metric.ends_with(".p99")
        || metric.ends_with(".avg")
        || metric.ends_with(".min")
        || metric.ends_with(".max")
        || metric.ends_with(".count");

    if metric.starts_with("gateway.") || is_percentile_metric {
        return METRIC_TYPE_GAUGE;
    }

    if metric.contains("latency") || metric.ends_with(".histogram") {
        return METRIC_TYPE_HISTOGRAM;
    }

    METRIC_TYPE_COUNTER
}

#[allow(clippy::cognitive_complexity)]
pub async fn query_metrics(
    State(state): State<Arc<AppState>>,
    Query(params): Query<QueryParams>,
) -> Response {
    let now = OffsetDateTime::now_utc();
    let default_start = now - Duration::days(7);

    let start_ms = params
        .start
        .as_ref()
        .and_then(|s| parse_datetime(s))
        .map_or_else(|| to_millis(default_start), to_millis);

    let end_ms = params
        .end
        .as_ref()
        .and_then(|s| parse_datetime(s))
        .map_or_else(|| to_millis(now), to_millis);

    let metric_type = params
        .metric_type
        .as_deref()
        .unwrap_or_else(|| infer_metric_type(&params.metric));

    let data = match metric_type {
        METRIC_TYPE_GAUGE => {
            match state
                .storage
                .query_gauges(&params.metric, start_ms, end_ms)
                .await
            {
                Ok(d) => d,
                Err(e) => {
                    error!("Query failed for {}: {}", params.metric, e);
                    return (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(serde_json::json!({"error": format!("Query failed: {}", e)})),
                    )
                        .into_response();
                }
            }
        }
        METRIC_TYPE_HISTOGRAM => {
            match state
                .storage
                .query_histograms(&params.metric, start_ms, end_ms)
                .await
            {
                Ok(d) => d,
                Err(e) => {
                    error!("Query failed for {}: {}", params.metric, e);
                    return (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(serde_json::json!({"error": format!("Query failed: {}", e)})),
                    )
                        .into_response();
                }
            }
        }
        _ => {
            let resolution = Resolution::from_str(params.resolution.as_deref());
            match state
                .storage
                .query_counters(
                    &params.metric,
                    start_ms,
                    end_ms,
                    params.group_by.as_deref(),
                    resolution,
                )
                .await
            {
                Ok(d) => d,
                Err(e) => {
                    error!("Query failed for {}: {}", params.metric, e);
                    return (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(serde_json::json!({"error": format!("Query failed: {}", e)})),
                    )
                        .into_response();
                }
            }
        }
    };

    Json(QueryResponse {
        metric: params.metric,
        data,
    })
    .into_response()
}

/// GET /query/aggregate - Get aggregated totals
#[derive(Deserialize)]
pub struct AggregateParams {
    pub metric: String,
    pub start: Option<String>,
    pub end: Option<String>,
    pub group_by: Option<String>,
}

#[derive(Serialize)]
pub struct AggregateResponse {
    pub metric: String,
    pub total: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub breakdown: Option<Vec<TopEntry>>,
}

pub async fn query_aggregate(
    State(state): State<Arc<AppState>>,
    Query(params): Query<AggregateParams>,
) -> Response {
    let now = OffsetDateTime::now_utc();
    let default_start = now - Duration::days(365);

    let start_ms = params
        .start
        .as_ref()
        .and_then(|s| parse_datetime(s))
        .map_or_else(|| to_millis(default_start), to_millis);

    let end_ms = params
        .end
        .as_ref()
        .and_then(|s| parse_datetime(s))
        .map_or_else(|| to_millis(now), to_millis);

    let data = match state
        .storage
        .query_counters(
            &params.metric,
            start_ms,
            end_ms,
            params.group_by.as_deref(),
            Resolution::Raw,
        )
        .await
    {
        Ok(d) => d,
        Err(e) => {
            error!("Aggregate query failed for {}: {}", params.metric, e);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": format!("Query failed: {}", e)})),
            )
                .into_response();
        }
    };

    if params.group_by.is_some() {
        let mut groups: std::collections::HashMap<String, f64> = std::collections::HashMap::new();
        for point in &data {
            if let Some(dims) = &point.dimensions {
                let key = dims
                    .values()
                    .filter_map(|v| v.as_str())
                    .collect::<Vec<_>>()
                    .join(",");
                *groups.entry(key).or_insert(0.0) += point.value;
            }
        }

        let mut breakdown: Vec<TopEntry> = groups
            .into_iter()
            .map(|(label, value)| TopEntry { label, value })
            .collect();
        breakdown.sort_by(|a, b| {
            b.value
                .partial_cmp(&a.value)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        let total = breakdown.iter().map(|e| e.value).sum();

        Json(AggregateResponse {
            metric: params.metric,
            total,
            breakdown: Some(breakdown),
        })
        .into_response()
    } else {
        let total = data.iter().map(|d| d.value).sum();
        Json(AggregateResponse {
            metric: params.metric,
            total,
            breakdown: None,
        })
        .into_response()
    }
}

#[allow(clippy::too_many_lines)]
pub async fn query_top(
    State(state): State<Arc<AppState>>,
    Query(params): Query<TopQueryParams>,
) -> Response {
    let limit = params.limit.unwrap_or(10);
    let now = OffsetDateTime::now_utc();
    let start_ms = to_millis(now - Duration::days(365));
    let end_ms = to_millis(now);

    let gauge_metric = GAUGE_METRICS
        .iter()
        .find(|(name, _)| *name == params.metric);

    let mut entries: Vec<TopEntry> = if let Some((_, dimension_key)) = gauge_metric {
        match state
            .storage
            .query_latest_gauges(&params.metric, Some(dimension_key))
            .await
        {
            Ok(summaries) => summaries
                .into_iter()
                .map(|summary| {
                    let mut label = summary.label.clone();
                    if params.metric == "guild.member_count" {
                        if let Some(name) = summary
                            .dimensions
                            .get("guild_name")
                            .and_then(|v| v.as_str())
                        {
                            label = format!("{} ({})", name, summary.label);
                        }
                    } else if params.metric == "user.guild_membership_count"
                        && let Some(user_id) =
                            summary.dimensions.get("user_id").and_then(|v| v.as_str())
                    {
                        label = user_id.to_string();
                    }
                    TopEntry {
                        label,
                        value: summary.value,
                    }
                })
                .collect(),
            Err(e) => {
                error!("Top gauge query failed for {}: {}", params.metric, e);
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(serde_json::json!({"error": format!("Query failed: {}", e)})),
                )
                    .into_response();
            }
        }
    } else {
        let group_by = if params.metric.starts_with("guild.") {
            Some("guild_id")
        } else if params.metric.starts_with("user.") {
            Some("user_id")
        } else if params.metric.contains("referrer") {
            Some("referrer")
        } else {
            None
        };

        let data = match state
            .storage
            .query_counters(&params.metric, start_ms, end_ms, group_by, Resolution::Raw)
            .await
        {
            Ok(d) => d,
            Err(e) => {
                error!("Query failed for {}: {}", params.metric, e);
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(serde_json::json!({"error": format!("Query failed: {}", e)})),
                )
                    .into_response();
            }
        };

        let mut groups: std::collections::HashMap<String, f64> = std::collections::HashMap::new();
        for point in &data {
            if let Some(dims) = &point.dimensions {
                let key = dims
                    .values()
                    .filter_map(|v| v.as_str())
                    .collect::<Vec<_>>()
                    .join(",");
                if !key.is_empty() {
                    *groups.entry(key).or_insert(0.0) += point.value;
                }
            }
        }

        let mut counter_entries: Vec<TopEntry> = groups
            .into_iter()
            .map(|(label, value)| TopEntry { label, value })
            .collect();
        counter_entries.sort_by(|a, b| {
            b.value
                .partial_cmp(&a.value)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        counter_entries
    };

    entries.sort_by(|a, b| {
        b.value
            .partial_cmp(&a.value)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    entries.truncate(limit);

    Json(TopQueryResponse {
        metric: params.metric,
        entries,
    })
    .into_response()
}

/// GET /query/crashes - Get recent crashes
#[derive(Deserialize)]
pub struct CrashesParams {
    pub limit: Option<usize>,
}

#[derive(Serialize)]
pub struct CrashesResponse {
    pub crashes: Vec<CrashEventResponse>,
}

#[derive(Serialize)]
pub struct CrashEventResponse {
    pub id: String,
    pub timestamp: i64,
    pub guild_id: String,
    pub stacktrace: String,
    pub notified: bool,
}

pub async fn query_crashes(
    State(state): State<Arc<AppState>>,
    Query(params): Query<CrashesParams>,
) -> Response {
    let limit = params.limit.unwrap_or(50);

    let crashes = match state.storage.get_recent_crashes(limit).await {
        Ok(c) => c,
        Err(e) => {
            error!("Crashes query failed: {}", e);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": format!("Query failed: {}", e)})),
            )
                .into_response();
        }
    };

    let response: Vec<CrashEventResponse> = crashes
        .into_iter()
        .map(|c| CrashEventResponse {
            id: c.id.clone(),
            timestamp: c.timestamp,
            guild_id: c.guild_id.clone(),
            stacktrace: c.stacktrace.clone(),
            notified: c.notified,
        })
        .collect();

    Json(CrashesResponse { crashes: response }).into_response()
}

#[derive(Deserialize)]
pub struct PercentilesParams {
    pub metric: String,
    pub start: Option<String>,
    pub end: Option<String>,
}

#[derive(Serialize)]
pub struct PercentilesResponse {
    pub metric: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub percentiles: Option<PercentilesData>,
}

#[derive(Serialize)]
pub struct PercentilesData {
    pub count: u64,
    pub avg: f64,
    pub min: f64,
    pub max: f64,
    pub p50: f64,
    pub p95: f64,
    pub p99: f64,
}

pub async fn query_percentiles(
    State(state): State<Arc<AppState>>,
    Query(params): Query<PercentilesParams>,
) -> Response {
    let now = OffsetDateTime::now_utc();
    let default_start = now - Duration::days(7);

    let start_ms = params
        .start
        .as_ref()
        .and_then(|s| parse_datetime(s))
        .map_or_else(|| to_millis(default_start), to_millis);

    let end_ms = params
        .end
        .as_ref()
        .and_then(|s| parse_datetime(s))
        .map_or_else(|| to_millis(now), to_millis);

    match state
        .storage
        .query_histogram_percentiles(&params.metric, start_ms, end_ms)
        .await
    {
        Ok(Some(p)) => Json(PercentilesResponse {
            metric: params.metric,
            percentiles: Some(PercentilesData {
                count: p.count,
                avg: p.avg,
                min: p.min,
                max: p.max,
                p50: p.p50,
                p95: p.p95,
                p99: p.p99,
            }),
        })
        .into_response(),
        Ok(None) => Json(PercentilesResponse {
            metric: params.metric,
            percentiles: None,
        })
        .into_response(),
        Err(e) => {
            error!("Percentiles query failed for {}: {}", params.metric, e);
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": format!("Query failed: {}", e)})),
            )
                .into_response()
        }
    }
}

pub async fn health_check(State(state): State<Arc<AppState>>) -> Response {
    match state.storage.check_health().await {
        Ok(()) => StatusCode::OK.into_response(),
        Err(e) => (
            StatusCode::SERVICE_UNAVAILABLE,
            Json(serde_json::json!({"error": e.to_string()})),
        )
            .into_response(),
    }
}
