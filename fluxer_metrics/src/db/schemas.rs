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

use clickhouse::Row;
use serde::{Deserialize, Serialize};
use time::OffsetDateTime;

#[derive(Row, Serialize, Deserialize, Debug, Clone)]
pub struct CounterMetric {
    pub metric_name: String,
    #[serde(with = "clickhouse::serde::time::datetime64::millis")]
    pub timestamp: OffsetDateTime,
    pub dimensions_hash: String,
    pub dimensions: Vec<(String, String)>,
    pub value: i64,
}

#[derive(Row, Serialize, Deserialize, Debug, Clone)]
pub struct GaugeMetric {
    pub id: String,
    pub metric_name: String,
    #[serde(with = "clickhouse::serde::time::datetime64::millis")]
    pub timestamp: OffsetDateTime,
    pub dimensions_hash: String,
    pub dimensions: Vec<(String, String)>,
    pub value: f64,
}

#[derive(Row, Serialize, Deserialize, Debug, Clone)]
pub struct HistogramRaw {
    pub id: String,
    pub metric_name: String,
    #[serde(with = "clickhouse::serde::time::datetime64::millis")]
    pub timestamp: OffsetDateTime,
    pub dimensions_hash: String,
    pub dimensions: Vec<(String, String)>,
    pub value_ms: f64,
}

#[derive(Row, Serialize, Deserialize, Debug, Clone)]
pub struct CrashEvent {
    pub id: String,
    #[serde(with = "clickhouse::serde::time::datetime64::millis")]
    pub timestamp: OffsetDateTime,
    pub guild_id: String,
    pub stacktrace: String,
    pub notified: u8,
    #[serde(with = "clickhouse::serde::time::datetime64::millis")]
    pub updated_at: OffsetDateTime,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CounterRequest {
    pub name: String,
    #[serde(default)]
    pub dimensions: serde_json::Map<String, serde_json::Value>,
    #[serde(default = "default_counter_value")]
    pub value: i64,
}

const fn default_counter_value() -> i64 {
    1
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GaugeRequest {
    pub name: String,
    #[serde(default)]
    pub dimensions: serde_json::Map<String, serde_json::Value>,
    pub value: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HistogramRequest {
    pub name: String,
    #[serde(default)]
    pub dimensions: serde_json::Map<String, serde_json::Value>,
    pub value_ms: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrashRequest {
    pub guild_id: String,
    pub stacktrace: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryParams {
    pub metric: String,
    pub start: Option<String>,
    pub end: Option<String>,
    pub group_by: Option<String>,
    pub resolution: Option<String>,
    #[serde(rename = "type")]
    pub metric_type: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TopQueryParams {
    pub metric: String,
    pub limit: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataPoint {
    pub timestamp: i64,
    pub value: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dimensions: Option<serde_json::Map<String, serde_json::Value>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryResponse {
    pub metric: String,
    pub data: Vec<DataPoint>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TopEntry {
    pub label: String,
    pub value: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TopQueryResponse {
    pub metric: String,
    pub entries: Vec<TopEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HistogramPercentiles {
    pub count: u64,
    pub avg: f64,
    pub min: f64,
    pub max: f64,
    pub p50: f64,
    pub p95: f64,
    pub p99: f64,
}

pub fn convert_dimensions(
    dimensions: &serde_json::Map<String, serde_json::Value>,
) -> Vec<(String, String)> {
    dimensions
        .iter()
        .map(|(k, v)| {
            let value = match v {
                serde_json::Value::String(s) => s.clone(),
                other => other.to_string(),
            };
            (k.clone(), value)
        })
        .collect()
}

pub fn dimensions_to_json(
    dimensions: &[(String, String)],
) -> serde_json::Map<String, serde_json::Value> {
    dimensions
        .iter()
        .map(|(k, v)| (k.clone(), serde_json::Value::String(v.clone())))
        .collect()
}
