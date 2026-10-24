// SPDX-License-Identifier: AGPL-3.0-or-later

pub use fluxer_common::config::env_value as optional_env;

use std::net::{IpAddr, SocketAddr};
use std::time::Duration;

const DEFAULT_MAX_CONCURRENT_REQUESTS: usize = 64;
const MESSAGES_MAX_CONCURRENT_REQUESTS: usize = 192;
const SNOWFLAKES_MAX_CONCURRENT_REQUESTS: usize = 320;

#[derive(Clone, Debug)]
pub struct ServiceConfig {
    pub service_name: String,
    pub mode: Mode,
    pub database_backend: DatabaseBackend,
    pub shard_id: u32,
    pub shard_count: u32,
    pub listen_addr: SocketAddr,
    pub nats_url: String,
    pub nats_auth_token: Option<String>,
    pub cache_max_entries: u64,
    pub cache_ttl: Duration,
    pub max_concurrent_requests: usize,
    pub scylla_hosts: Vec<String>,
    pub scylla_keyspace: String,
    pub scylla_username: Option<String>,
    pub scylla_password: Option<String>,
    pub postgres_url: Option<String>,
    pub postgres_host: String,
    pub postgres_port: u16,
    pub postgres_database: String,
    pub postgres_username: String,
    pub postgres_password: Option<String>,
    pub postgres_ssl: bool,
    pub postgres_ssl_ca: Option<String>,
    pub postgres_max_connections: usize,
    pub postgres_kv_table: String,
    pub postgres_prepared_statements: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    Router,
    Shard,
    Combined,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DatabaseBackend {
    Postgres,
    Cassandra,
}

impl ServiceConfig {
    #[cfg(test)]
    pub(crate) fn for_test(mode: Mode, shard_id: u32, shard_count: u32) -> Self {
        ServiceConfig {
            service_name: "test".to_owned(),
            mode,
            database_backend: DatabaseBackend::Postgres,
            shard_id,
            shard_count,
            listen_addr: "127.0.0.1:0".parse().unwrap(),
            nats_url: "memory".to_owned(),
            nats_auth_token: None,
            cache_max_entries: 100,
            cache_ttl: Duration::from_secs(30),
            max_concurrent_requests: 16,
            scylla_hosts: Vec::new(),
            scylla_keyspace: "fluxer".to_owned(),
            scylla_username: None,
            scylla_password: None,
            postgres_url: None,
            postgres_host: "127.0.0.1".to_owned(),
            postgres_port: 5432,
            postgres_database: "fluxer".to_owned(),
            postgres_username: "fluxer".to_owned(),
            postgres_password: Some("fluxer".to_owned()),
            postgres_ssl: false,
            postgres_ssl_ca: None,
            postgres_max_connections: 1,
            postgres_kv_table: "fluxer_kv".to_owned(),
            postgres_prepared_statements: true,
        }
    }

    pub fn from_env() -> anyhow::Result<Self> {
        Self::from_env_reader(fluxer_common::config::env_value)
    }

    fn from_env_reader<F>(get: F) -> anyhow::Result<Self>
    where
        F: Fn(&str) -> Option<String>,
    {
        let service_name =
            optional_from(&get, "FLUXER_SVC_NAME").unwrap_or_else(|| "default".to_owned());

        let database_backend = match optional_from(&get, "FLUXER_DATABASE_BACKEND")
            .unwrap_or_else(|| "postgres".to_owned())
            .trim()
            .to_ascii_lowercase()
            .as_str()
        {
            "postgres" | "postgresql" | "pg" => DatabaseBackend::Postgres,
            "cassandra" | "scylla" | "scylladb" => DatabaseBackend::Cassandra,
            other => anyhow::bail!("unsupported FLUXER_DATABASE_BACKEND: {other}"),
        };

        let mode = match optional_from(&get, "FLUXER_SVC_MODE").as_deref() {
            None | Some("router") => Mode::Router,
            Some("shard") => Mode::Shard,
            Some("combined") => Mode::Combined,
            Some(other) => anyhow::bail!("unsupported FLUXER_SVC_MODE: {other}"),
        };

        let shard_count = optional_from(&get, "FLUXER_SVC_SHARD_COUNT")
            .map(|v| v.parse::<u32>())
            .transpose()?
            .unwrap_or(1);

        let shard_id = match optional_from(&get, "FLUXER_SVC_SHARD_ID") {
            Some(v) => v.parse::<u32>()?,
            None => shard_id_from_pod_name(&optional_from(&get, "POD_NAME").unwrap_or_default())
                .unwrap_or(0),
        };

        if shard_id >= shard_count {
            anyhow::bail!(
                "FLUXER_SVC_SHARD_ID ({shard_id}) must be less than FLUXER_SVC_SHARD_COUNT ({shard_count})"
            );
        }

        let listen_host =
            optional_from(&get, "FLUXER_SVC_LISTEN_HOST").unwrap_or_else(|| "0.0.0.0".to_owned());
        let listen_port = optional_from(&get, "FLUXER_SVC_PORT")
            .map(|v| v.parse::<u16>())
            .transpose()?
            .unwrap_or(8090);

        let nats_url = optional_from(&get, "FLUXER_SVC_NATS_URL")
            .unwrap_or_else(|| "nats://127.0.0.1:4222".to_owned());

        let nats_auth_token = optional_from(&get, "FLUXER_NATS_AUTH_TOKEN");

        let cache_ttl_ms = optional_from(&get, "FLUXER_SVC_CACHE_TTL_MS")
            .map(|v| v.parse::<u64>())
            .transpose()?
            .unwrap_or(30_000);

        let cassandra_port = optional_from(&get, "FLUXER_CASSANDRA_PORT")
            .map(|v| v.parse::<u16>())
            .transpose()?
            .unwrap_or(9042);

        let default_scylla_hosts = || vec![normalize_host("127.0.0.1", cassandra_port)];
        let scylla_hosts = optional_from(&get, "FLUXER_CASSANDRA_HOSTS")
            .map(|hosts| {
                parse_hosts(&hosts)
                    .into_iter()
                    .map(|host| normalize_host(&host, cassandra_port))
                    .collect::<Vec<_>>()
            })
            .filter(|hosts| !hosts.is_empty())
            .unwrap_or_else(default_scylla_hosts);

        let postgres_port = optional_from(&get, "FLUXER_POSTGRES_PORT")
            .map(|v| v.parse::<u16>())
            .transpose()?
            .unwrap_or(5432);
        let postgres_ssl = optional_from(&get, "FLUXER_POSTGRES_SSL")
            .map(|v| parse_bool(&v))
            .transpose()?
            .unwrap_or(false);
        let postgres_max_connections = optional_from(&get, "FLUXER_POSTGRES_MAX_CONNECTIONS")
            .map(|v| v.parse::<usize>())
            .transpose()?
            .unwrap_or(20)
            .max(1);
        let postgres_prepared_statements =
            optional_from(&get, "FLUXER_POSTGRES_PREPARED_STATEMENTS")
                .map(|v| parse_bool(&v))
                .transpose()?
                .unwrap_or(true);

        let max_concurrent_requests = optional_from(&get, "FLUXER_SVC_MAX_CONCURRENT_REQUESTS")
            .map(|v| v.parse::<usize>())
            .transpose()?
            .unwrap_or_else(|| default_max_concurrent_requests(&service_name))
            .max(1);

        Ok(Self {
            service_name,
            mode,
            database_backend,
            shard_id,
            shard_count,
            listen_addr: format!("{listen_host}:{listen_port}").parse()?,
            nats_url,
            nats_auth_token,
            cache_max_entries: optional_from(&get, "FLUXER_SVC_CACHE_MAX_ENTRIES")
                .map(|v| v.parse::<u64>())
                .transpose()?
                .unwrap_or(100_000),
            cache_ttl: Duration::from_millis(cache_ttl_ms),
            max_concurrent_requests,
            scylla_hosts,
            scylla_keyspace: optional_from(&get, "FLUXER_CASSANDRA_KEYSPACE")
                .unwrap_or_else(|| "fluxer".to_owned()),
            scylla_username: optional_from(&get, "FLUXER_CASSANDRA_USERNAME"),
            scylla_password: optional_from(&get, "FLUXER_CASSANDRA_PASSWORD"),
            postgres_url: optional_from(&get, "FLUXER_POSTGRES_URL"),
            postgres_host: optional_from(&get, "FLUXER_POSTGRES_HOST")
                .unwrap_or_else(|| "127.0.0.1".to_owned()),
            postgres_port,
            postgres_database: optional_from(&get, "FLUXER_POSTGRES_DATABASE")
                .unwrap_or_else(|| "fluxer".to_owned()),
            postgres_username: optional_from(&get, "FLUXER_POSTGRES_USERNAME")
                .unwrap_or_else(|| "fluxer".to_owned()),
            postgres_password: optional_from(&get, "FLUXER_POSTGRES_PASSWORD")
                .or_else(|| Some("fluxer".to_owned())),
            postgres_ssl,
            postgres_ssl_ca: optional_from(&get, "FLUXER_POSTGRES_SSL_CA"),
            postgres_max_connections,
            postgres_kv_table: optional_from(&get, "FLUXER_POSTGRES_KV_TABLE")
                .unwrap_or_else(|| "fluxer_kv".to_owned()),
            postgres_prepared_statements,
        })
    }
}

fn default_max_concurrent_requests(service_name: &str) -> usize {
    match service_name {
        "messages" => MESSAGES_MAX_CONCURRENT_REQUESTS,
        "snowflakes" => SNOWFLAKES_MAX_CONCURRENT_REQUESTS,
        _ => DEFAULT_MAX_CONCURRENT_REQUESTS,
    }
}

fn optional_from<F>(get: &F, name: &str) -> Option<String>
where
    F: Fn(&str) -> Option<String>,
{
    get(name).filter(|v| !v.trim().is_empty())
}

pub fn parse_hosts(hosts: &str) -> Vec<String> {
    hosts
        .split(',')
        .map(str::trim)
        .filter(|h| !h.is_empty())
        .map(ToOwned::to_owned)
        .collect()
}

pub fn normalize_host(host: &str, port: u16) -> String {
    let host = host.trim();
    if let Some(rest) = host.strip_prefix('[') {
        if let Some((_, suffix)) = rest.rsplit_once("]:")
            && suffix.parse::<u16>().is_ok()
        {
            return host.to_owned();
        }
        if host.ends_with(']') {
            return format!("{host}:{port}");
        }
    }
    if host.matches(':').count() == 1
        && host
            .rsplit_once(':')
            .is_some_and(|(_, suffix)| suffix.parse::<u16>().is_ok())
    {
        return host.to_owned();
    }
    if let Ok(addr) = host.parse::<IpAddr>() {
        return match addr {
            IpAddr::V4(addr) => format!("{addr}:{port}"),
            IpAddr::V6(addr) => format!("[{addr}]:{port}"),
        };
    }
    format!("{host}:{port}")
}

pub fn shard_id_from_pod_name(pod_name: &str) -> Option<u32> {
    let (_, suffix) = pod_name.rsplit_once('-')?;
    suffix.parse::<u32>().ok()
}

fn parse_bool(value: &str) -> anyhow::Result<bool> {
    match value.trim().to_ascii_lowercase().as_str() {
        "1" | "true" | "yes" | "y" | "on" => Ok(true),
        "0" | "false" | "no" | "n" | "off" => Ok(false),
        other => anyhow::bail!("invalid boolean value: {other}"),
    }
}
