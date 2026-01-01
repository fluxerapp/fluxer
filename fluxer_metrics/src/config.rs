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

use std::env;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MetricsMode {
    #[default]
    ClickHouse,
    NoOp,
}

impl FromStr for MetricsMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "noop" | "no-op" | "no_op" | "none" | "disabled" => Ok(Self::NoOp),
            "clickhouse" | "ch" | "" => Ok(Self::ClickHouse),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Config {
    pub port: u16,
    pub metrics_mode: MetricsMode,
    pub clickhouse_url: String,
    pub clickhouse_database: String,
    pub clickhouse_user: String,
    pub clickhouse_password: String,
    pub alert_webhook_url: Option<String>,
    pub admin_endpoint: Option<String>,
}

impl Config {
    pub fn from_env() -> Result<Self, ConfigError> {
        let metrics_mode = env::var("METRICS_MODE")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or_default();

        let clickhouse_url =
            env::var("CLICKHOUSE_URL").unwrap_or_else(|_| "http://localhost:8123".into());
        let clickhouse_database =
            env::var("CLICKHOUSE_DATABASE").unwrap_or_else(|_| "fluxer_metrics".into());
        let clickhouse_user = env::var("CLICKHOUSE_USER").unwrap_or_else(|_| "default".into());
        let clickhouse_password = env::var("CLICKHOUSE_PASSWORD").unwrap_or_default();

        if metrics_mode == MetricsMode::ClickHouse && clickhouse_url.is_empty() {
            return Err(ConfigError::EmptyEnvVar("CLICKHOUSE_URL"));
        }

        Ok(Self {
            port: env::var("METRICS_PORT")
                .ok()
                .and_then(|p| p.parse().ok())
                .unwrap_or(8080),
            metrics_mode,
            clickhouse_url,
            clickhouse_database,
            clickhouse_user,
            clickhouse_password,
            alert_webhook_url: env::var("ALERT_WEBHOOK_URL").ok().filter(|s| !s.is_empty()),
            admin_endpoint: env::var("FLUXER_ADMIN_ENDPOINT")
                .ok()
                .filter(|s| !s.is_empty()),
        })
    }
}

#[derive(Debug)]
pub enum ConfigError {
    EmptyEnvVar(&'static str),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyEnvVar(var) => {
                write!(f, "required environment variable {var} is empty")
            }
        }
    }
}

impl std::error::Error for ConfigError {}
