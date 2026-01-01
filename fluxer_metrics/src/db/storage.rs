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

use anyhow::{Result, ensure};
use async_trait::async_trait;
use clickhouse::{Client, Row};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use time::OffsetDateTime;
use tracing::info;
use ulid::Ulid;

use super::Storage;
use super::migrations::run_migrations;
use super::schemas::{
    CounterMetric, CounterRequest, CrashEvent, CrashRequest, DataPoint, GaugeMetric, GaugeRequest,
    HistogramPercentiles, HistogramRaw, HistogramRequest, convert_dimensions, dimensions_to_json,
};
use crate::config::Config;

pub fn hash_dimensions(dimensions: &serde_json::Map<String, serde_json::Value>) -> String {
    if dimensions.is_empty() {
        return String::new();
    }
    let json = serde_json::to_string(dimensions).unwrap_or_default();
    let mut hasher = Sha256::new();
    hasher.update(json.as_bytes());
    format!("{:x}", hasher.finalize())[..16].to_string()
}

fn sanitize_dimension_key(key: &str) -> Option<String> {
    if key.is_empty() || key.len() > 64 {
        return None;
    }
    let sanitized: String = key
        .chars()
        .filter(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '.' || *c == '-')
        .collect();
    if sanitized.is_empty() || sanitized != key {
        None
    } else {
        Some(sanitized)
    }
}

fn validate_identifier(name: &str) -> Result<()> {
    ensure!(
        name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
        "Invalid ClickHouse identifier: {name}"
    );
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Resolution {
    Raw,
    Hourly,
    Daily,
}

impl Resolution {
    pub fn from_str(s: Option<&str>) -> Self {
        match s {
            Some("hourly") => Self::Hourly,
            Some("daily") => Self::Daily,
            _ => Self::Raw,
        }
    }
}

#[derive(Clone)]
pub struct ClickHouseStorage {
    client: Client,
    database: String,
}

#[derive(Debug, Clone)]
pub struct LatestGaugeSummary {
    pub dimensions: serde_json::Map<String, serde_json::Value>,
    pub value: f64,
    pub label: String,
}

#[derive(Debug, Clone)]
pub struct CrashEventData {
    pub id: String,
    pub timestamp: i64,
    pub guild_id: String,
    pub stacktrace: String,
    pub notified: bool,
}

#[derive(Row, Serialize, Deserialize)]
struct CounterQueryRow {
    timestamp_bucket: i64,
    group_key: String,
    total: i64,
}

#[derive(Row, Serialize, Deserialize)]
struct AggregatedCounterQueryRow {
    period_start: i64,
    group_key: String,
    total: i64,
}

#[derive(Row, Serialize, Deserialize)]
struct GaugeQueryRow {
    timestamp: i64,
    value: f64,
    dimensions: Vec<(String, String)>,
}

#[derive(Row, Serialize, Deserialize)]
struct HistogramQueryRow {
    timestamp_bucket: i64,
    avg_value: f64,
}

#[derive(Row, Serialize, Deserialize)]
struct LatestGaugeRow {
    dimensions_hash: String,
    timestamp: i64,
    value: f64,
    dimensions: Vec<(String, String)>,
    label: String,
}

#[derive(Row, Serialize, Deserialize)]
struct PercentilesRow {
    count: u64,
    avg: f64,
    min: f64,
    max: f64,
    p50: f64,
    p95: f64,
    p99: f64,
}

#[async_trait]
impl Storage for ClickHouseStorage {
    async fn check_health(&self) -> Result<()> {
        self.client.query("SELECT 1").execute().await?;
        Ok(())
    }

    async fn insert_counter(&self, req: CounterRequest) -> Result<()> {
        self.insert_counter_impl(req).await
    }

    async fn insert_gauge(&self, req: GaugeRequest) -> Result<()> {
        self.insert_gauge_impl(req).await
    }

    async fn insert_histogram(&self, req: HistogramRequest) -> Result<()> {
        self.insert_histogram_impl(req).await
    }

    async fn insert_crash(&self, req: CrashRequest) -> Result<CrashEventData> {
        self.insert_crash_impl(req).await
    }

    async fn mark_crash_notified(&self, id: &str) -> Result<()> {
        self.mark_crash_notified_impl(id).await
    }

    async fn query_counters(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
        group_by: Option<&str>,
        resolution: Resolution,
    ) -> Result<Vec<DataPoint>> {
        self.query_counters_impl(metric_name, start_ms, end_ms, group_by, resolution)
            .await
    }

    async fn query_gauges(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
    ) -> Result<Vec<DataPoint>> {
        self.query_gauges_impl(metric_name, start_ms, end_ms).await
    }

    async fn query_histograms(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
    ) -> Result<Vec<DataPoint>> {
        self.query_histograms_impl(metric_name, start_ms, end_ms)
            .await
    }

    async fn query_histogram_percentiles(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
    ) -> Result<Option<HistogramPercentiles>> {
        self.query_histogram_percentiles_impl(metric_name, start_ms, end_ms)
            .await
    }

    async fn get_recent_crashes(&self, limit: usize) -> Result<Vec<CrashEventData>> {
        self.get_recent_crashes_impl(limit).await
    }

    async fn query_latest_gauges(
        &self,
        metric_name: &str,
        group_by: Option<&str>,
    ) -> Result<Vec<LatestGaugeSummary>> {
        self.query_latest_gauges_impl(metric_name, group_by).await
    }
}

impl ClickHouseStorage {
    pub async fn new(config: &Config) -> Result<Self> {
        info!(
            "Initializing ClickHouse storage at {}",
            config.clickhouse_url
        );
        validate_identifier(&config.clickhouse_database)?;

        let migration_client = Client::default()
            .with_url(&config.clickhouse_url)
            .with_user(&config.clickhouse_user)
            .with_password(&config.clickhouse_password);

        run_migrations(&migration_client, &config.clickhouse_database).await?;

        let client = migration_client.with_database(&config.clickhouse_database);

        info!("ClickHouse storage initialized successfully");

        Ok(Self {
            client,
            database: config.clickhouse_database.clone(),
        })
    }

    async fn insert_counter_impl(&self, req: CounterRequest) -> Result<()> {
        let now = OffsetDateTime::now_utc();
        let dimensions_hash = hash_dimensions(&req.dimensions);
        let dimensions = convert_dimensions(&req.dimensions);

        let metric = CounterMetric {
            metric_name: req.name,
            timestamp: now,
            dimensions_hash,
            dimensions,
            value: req.value,
        };

        let mut insert = self.client.insert("counters")?;
        insert.write(&metric).await?;
        insert.end().await?;

        Ok(())
    }

    async fn insert_gauge_impl(&self, req: GaugeRequest) -> Result<()> {
        let now = OffsetDateTime::now_utc();
        let dimensions_hash = hash_dimensions(&req.dimensions);
        let dimensions = convert_dimensions(&req.dimensions);

        let metric = GaugeMetric {
            id: Ulid::new().to_string(),
            metric_name: req.name,
            timestamp: now,
            dimensions_hash,
            dimensions,
            value: req.value,
        };

        let mut insert = self.client.insert("gauges")?;
        insert.write(&metric).await?;
        insert.end().await?;

        Ok(())
    }

    async fn insert_histogram_impl(&self, req: HistogramRequest) -> Result<()> {
        let now = OffsetDateTime::now_utc();
        let dimensions_hash = hash_dimensions(&req.dimensions);
        let dimensions = convert_dimensions(&req.dimensions);

        let raw = HistogramRaw {
            id: Ulid::new().to_string(),
            metric_name: req.name,
            timestamp: now,
            dimensions_hash,
            dimensions,
            value_ms: req.value_ms,
        };

        let mut insert = self.client.insert("histogram_raw")?;
        insert.write(&raw).await?;
        insert.end().await?;

        Ok(())
    }

    async fn insert_crash_impl(&self, req: CrashRequest) -> Result<CrashEventData> {
        let now = OffsetDateTime::now_utc();
        let id = Ulid::new().to_string();

        let event = CrashEvent {
            id: id.clone(),
            timestamp: now,
            guild_id: req.guild_id.clone(),
            stacktrace: req.stacktrace.clone(),
            notified: 0,
            updated_at: now,
        };

        let mut insert = self.client.insert("crashes")?;
        insert.write(&event).await?;
        insert.end().await?;

        Ok(CrashEventData {
            id,
            timestamp: (now.unix_timestamp_nanos() / 1_000_000) as i64,
            guild_id: req.guild_id,
            stacktrace: req.stacktrace,
            notified: false,
        })
    }

    async fn mark_crash_notified_impl(&self, id: &str) -> Result<()> {
        let query = format!(
            r#"
            INSERT INTO {}.crashes
            SELECT
                id,
                timestamp,
                guild_id,
                stacktrace,
                1 AS notified,
                now64(3) AS updated_at
            FROM {}.crashes
            WHERE id = ?
            ORDER BY updated_at DESC
            LIMIT 1
            "#,
            self.database, self.database
        );

        self.client.query(&query).bind(id).execute().await?;
        Ok(())
    }

    async fn query_counters_impl(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
        group_by: Option<&str>,
        resolution: Resolution,
    ) -> Result<Vec<DataPoint>> {
        match resolution {
            Resolution::Raw => {
                self.query_counters_raw(metric_name, start_ms, end_ms, group_by)
                    .await
            }
            Resolution::Hourly => {
                self.query_counters_aggregated(
                    metric_name,
                    start_ms,
                    end_ms,
                    group_by,
                    "counters_hourly",
                )
                .await
            }
            Resolution::Daily => {
                self.query_counters_aggregated(
                    metric_name,
                    start_ms,
                    end_ms,
                    group_by,
                    "counters_daily",
                )
                .await
            }
        }
    }

    async fn query_counters_raw(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
        group_by: Option<&str>,
    ) -> Result<Vec<DataPoint>> {
        let group_expr = group_by
            .and_then(sanitize_dimension_key)
            .map_or("''".to_string(), |g| format!("dimensions['{g}']"));

        let query = format!(
            r#"
            SELECT
                toUnixTimestamp64Milli(timestamp_bucket) AS timestamp_bucket,
                {group_expr} AS group_key,
                sum(value) AS total
            FROM {}.counters
            WHERE metric_name = ?
              AND timestamp_bucket >= fromUnixTimestamp64Milli(?)
              AND timestamp_bucket <= fromUnixTimestamp64Milli(?)
            GROUP BY timestamp_bucket, group_key
            ORDER BY timestamp_bucket
            "#,
            self.database
        );

        let rows: Vec<CounterQueryRow> = self
            .client
            .query(&query)
            .bind(metric_name)
            .bind(start_ms)
            .bind(end_ms)
            .fetch_all()
            .await?;

        let data: Vec<DataPoint> = rows
            .into_iter()
            .map(|r| {
                let dims = if r.group_key.is_empty() {
                    None
                } else {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        group_by.unwrap_or("group").to_string(),
                        serde_json::Value::String(r.group_key),
                    );
                    Some(map)
                };
                DataPoint {
                    timestamp: r.timestamp_bucket,
                    value: r.total as f64,
                    dimensions: dims,
                }
            })
            .collect();

        Ok(data)
    }

    async fn query_counters_aggregated(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
        group_by: Option<&str>,
        table_name: &str,
    ) -> Result<Vec<DataPoint>> {
        let group_expr = group_by
            .and_then(sanitize_dimension_key)
            .map_or("''".to_string(), |g| format!("dimensions['{g}']"));

        let query = format!(
            r#"
            SELECT
                toUnixTimestamp64Milli(period_start) AS period_start,
                {group_expr} AS group_key,
                sum(total_value) AS total
            FROM {}.{table_name}
            WHERE metric_name = ?
              AND period_start >= fromUnixTimestamp64Milli(?)
              AND period_start <= fromUnixTimestamp64Milli(?)
            GROUP BY period_start, group_key
            ORDER BY period_start
            "#,
            self.database
        );

        let rows: Vec<AggregatedCounterQueryRow> = self
            .client
            .query(&query)
            .bind(metric_name)
            .bind(start_ms)
            .bind(end_ms)
            .fetch_all()
            .await?;

        let data: Vec<DataPoint> = rows
            .into_iter()
            .map(|r| {
                let dims = if r.group_key.is_empty() {
                    None
                } else {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        group_by.unwrap_or("group").to_string(),
                        serde_json::Value::String(r.group_key),
                    );
                    Some(map)
                };
                DataPoint {
                    timestamp: r.period_start,
                    value: r.total as f64,
                    dimensions: dims,
                }
            })
            .collect();

        Ok(data)
    }

    async fn query_gauges_impl(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
    ) -> Result<Vec<DataPoint>> {
        let query = format!(
            r#"
            SELECT
                toUnixTimestamp64Milli(timestamp) AS timestamp,
                value,
                dimensions
            FROM {}.gauges
            WHERE metric_name = ?
              AND timestamp >= fromUnixTimestamp64Milli(?)
              AND timestamp <= fromUnixTimestamp64Milli(?)
            ORDER BY timestamp
            "#,
            self.database
        );

        let rows: Vec<GaugeQueryRow> = self
            .client
            .query(&query)
            .bind(metric_name)
            .bind(start_ms)
            .bind(end_ms)
            .fetch_all()
            .await?;

        let data: Vec<DataPoint> = rows
            .into_iter()
            .map(|r| {
                let dims = if r.dimensions.is_empty() {
                    None
                } else {
                    Some(dimensions_to_json(&r.dimensions))
                };
                DataPoint {
                    timestamp: r.timestamp,
                    value: r.value,
                    dimensions: dims,
                }
            })
            .collect();

        Ok(data)
    }

    async fn query_histograms_impl(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
    ) -> Result<Vec<DataPoint>> {
        let query = format!(
            r#"
            SELECT
                toUnixTimestamp64Milli(timestamp_bucket) AS timestamp_bucket,
                avg(value_ms) AS avg_value
            FROM {}.histogram_raw
            WHERE metric_name = ?
              AND timestamp_bucket >= fromUnixTimestamp64Milli(?)
              AND timestamp_bucket <= fromUnixTimestamp64Milli(?)
            GROUP BY timestamp_bucket
            ORDER BY timestamp_bucket
            "#,
            self.database
        );

        let rows: Vec<HistogramQueryRow> = self
            .client
            .query(&query)
            .bind(metric_name)
            .bind(start_ms)
            .bind(end_ms)
            .fetch_all()
            .await?;

        let data: Vec<DataPoint> = rows
            .into_iter()
            .map(|r| DataPoint {
                timestamp: r.timestamp_bucket,
                value: r.avg_value,
                dimensions: None,
            })
            .collect();

        Ok(data)
    }

    async fn query_histogram_percentiles_impl(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
    ) -> Result<Option<HistogramPercentiles>> {
        let query = format!(
            r#"
            SELECT
                count() AS count,
                avg(value_ms) AS avg,
                min(value_ms) AS min,
                max(value_ms) AS max,
                quantile(0.50)(value_ms) AS p50,
                quantile(0.95)(value_ms) AS p95,
                quantile(0.99)(value_ms) AS p99
            FROM {}.histogram_raw
            WHERE metric_name = ?
              AND timestamp_bucket >= fromUnixTimestamp64Milli(?)
              AND timestamp_bucket <= fromUnixTimestamp64Milli(?)
            "#,
            self.database
        );

        let result: Option<PercentilesRow> = self
            .client
            .query(&query)
            .bind(metric_name)
            .bind(start_ms)
            .bind(end_ms)
            .fetch_optional()
            .await?;

        match result {
            Some(r) if r.count > 0 => Ok(Some(HistogramPercentiles {
                count: r.count,
                avg: r.avg,
                min: r.min,
                max: r.max,
                p50: r.p50,
                p95: r.p95,
                p99: r.p99,
            })),
            _ => Ok(None),
        }
    }

    async fn get_recent_crashes_impl(&self, limit: usize) -> Result<Vec<CrashEventData>> {
        let query = format!(
            r#"
            SELECT
                id,
                argMax(crashes.timestamp, crashes.updated_at) AS timestamp,
                argMax(guild_id, crashes.updated_at) AS guild_id,
                argMax(stacktrace, crashes.updated_at) AS stacktrace,
                argMax(notified, crashes.updated_at) AS notified,
                max(crashes.updated_at) AS updated_at
            FROM {}.crashes
            GROUP BY id
            ORDER BY timestamp DESC
            LIMIT ?
            "#,
            self.database
        );

        let rows: Vec<CrashEvent> = self.client.query(&query).bind(limit).fetch_all().await?;

        let crashes: Vec<CrashEventData> = rows
            .into_iter()
            .map(|r| CrashEventData {
                id: r.id,
                timestamp: (r.timestamp.unix_timestamp_nanos() / 1_000_000) as i64,
                guild_id: r.guild_id,
                stacktrace: r.stacktrace,
                notified: r.notified != 0,
            })
            .collect();

        Ok(crashes)
    }

    async fn query_latest_gauges_impl(
        &self,
        metric_name: &str,
        group_by: Option<&str>,
    ) -> Result<Vec<LatestGaugeSummary>> {
        let label_expr = group_by
            .and_then(sanitize_dimension_key)
            .map_or("dimensions_hash".to_string(), |g| {
                format!("argMax(gauges.dimensions['{g}'], gauges.timestamp)")
            });

        let query = format!(
            r#"
            SELECT
                dimensions_hash,
                max(gauges.timestamp) AS timestamp,
                argMax(gauges.value, gauges.timestamp) AS value,
                argMax(gauges.dimensions, gauges.timestamp) AS dimensions,
                {label_expr} AS label
            FROM {}.gauges
            WHERE metric_name = ?
            GROUP BY dimensions_hash
            ORDER BY value DESC
            "#,
            self.database
        );

        let rows: Vec<LatestGaugeRow> = self
            .client
            .query(&query)
            .bind(metric_name)
            .fetch_all()
            .await?;

        let summaries: Vec<LatestGaugeSummary> = rows
            .into_iter()
            .map(|r| LatestGaugeSummary {
                dimensions: dimensions_to_json(&r.dimensions),
                value: r.value,
                label: r.label,
            })
            .collect();

        Ok(summaries)
    }
}

#[derive(Clone)]
pub struct NoOpStorage;

impl NoOpStorage {
    pub fn new() -> Self {
        info!("Initializing NoOp storage (metrics will be discarded)");
        Self
    }
}

impl Default for NoOpStorage {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Storage for NoOpStorage {
    async fn check_health(&self) -> Result<()> {
        Ok(())
    }

    async fn insert_counter(&self, _req: CounterRequest) -> Result<()> {
        Ok(())
    }

    async fn insert_gauge(&self, _req: GaugeRequest) -> Result<()> {
        Ok(())
    }

    async fn insert_histogram(&self, _req: HistogramRequest) -> Result<()> {
        Ok(())
    }

    async fn insert_crash(&self, req: CrashRequest) -> Result<CrashEventData> {
        let now = OffsetDateTime::now_utc();
        let id = Ulid::new().to_string();
        Ok(CrashEventData {
            id,
            timestamp: (now.unix_timestamp_nanos() / 1_000_000) as i64,
            guild_id: req.guild_id,
            stacktrace: req.stacktrace,
            notified: false,
        })
    }

    async fn mark_crash_notified(&self, _id: &str) -> Result<()> {
        Ok(())
    }

    async fn query_counters(
        &self,
        _metric_name: &str,
        _start_ms: i64,
        _end_ms: i64,
        _group_by: Option<&str>,
        _resolution: Resolution,
    ) -> Result<Vec<DataPoint>> {
        Ok(Vec::new())
    }

    async fn query_gauges(
        &self,
        _metric_name: &str,
        _start_ms: i64,
        _end_ms: i64,
    ) -> Result<Vec<DataPoint>> {
        Ok(Vec::new())
    }

    async fn query_histograms(
        &self,
        _metric_name: &str,
        _start_ms: i64,
        _end_ms: i64,
    ) -> Result<Vec<DataPoint>> {
        Ok(Vec::new())
    }

    async fn query_histogram_percentiles(
        &self,
        _metric_name: &str,
        _start_ms: i64,
        _end_ms: i64,
    ) -> Result<Option<HistogramPercentiles>> {
        Ok(None)
    }

    async fn get_recent_crashes(&self, _limit: usize) -> Result<Vec<CrashEventData>> {
        Ok(Vec::new())
    }

    async fn query_latest_gauges(
        &self,
        _metric_name: &str,
        _group_by: Option<&str>,
    ) -> Result<Vec<LatestGaugeSummary>> {
        Ok(Vec::new())
    }
}
