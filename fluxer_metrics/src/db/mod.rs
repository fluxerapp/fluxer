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

mod migrations;
pub mod schemas;
pub mod storage;

use anyhow::Result;
use async_trait::async_trait;

pub use schemas::*;
pub use storage::{ClickHouseStorage, CrashEventData, LatestGaugeSummary, NoOpStorage, Resolution};

#[async_trait]
pub trait Storage: Send + Sync {
    async fn check_health(&self) -> Result<()>;
    async fn insert_counter(&self, req: CounterRequest) -> Result<()>;
    async fn insert_gauge(&self, req: GaugeRequest) -> Result<()>;
    async fn insert_histogram(&self, req: HistogramRequest) -> Result<()>;
    async fn insert_crash(&self, req: CrashRequest) -> Result<CrashEventData>;
    async fn mark_crash_notified(&self, id: &str) -> Result<()>;
    async fn query_counters(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
        group_by: Option<&str>,
        resolution: Resolution,
    ) -> Result<Vec<DataPoint>>;
    async fn query_gauges(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
    ) -> Result<Vec<DataPoint>>;
    async fn query_histograms(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
    ) -> Result<Vec<DataPoint>>;
    async fn query_histogram_percentiles(
        &self,
        metric_name: &str,
        start_ms: i64,
        end_ms: i64,
    ) -> Result<Option<HistogramPercentiles>>;
    async fn get_recent_crashes(&self, limit: usize) -> Result<Vec<CrashEventData>>;
    async fn query_latest_gauges(
        &self,
        metric_name: &str,
        group_by: Option<&str>,
    ) -> Result<Vec<LatestGaugeSummary>>;
}
