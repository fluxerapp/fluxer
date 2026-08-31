// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env;
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
    pub cache_max_entries: u64,
    pub cache_ttl: Duration,
    pub cache_hard_ttl: Duration,
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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DatabaseBackend {
    Postgres,
    Cassandra,
}

impl ServiceConfig {
    pub fn from_env() -> anyhow::Result<Self> {
        Self::from_env_reader(|name| env::var(name).ok())
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
            Some("shard") => Mode::Shard,
            _ => Mode::Router,
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

        let cache_ttl_ms = optional_from(&get, "FLUXER_SVC_CACHE_TTL_MS")
            .map(|v| v.parse::<u64>())
            .transpose()?
            .unwrap_or(30_000);

        let cache_hard_ttl_ms = optional_from(&get, "FLUXER_SVC_CACHE_HARD_TTL_MS")
            .map(|v| v.parse::<u64>())
            .transpose()?
            .unwrap_or(600_000)
            .max(cache_ttl_ms);

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
            cache_max_entries: optional_from(&get, "FLUXER_SVC_CACHE_MAX_ENTRIES")
                .map(|v| v.parse::<u64>())
                .transpose()?
                .unwrap_or(100_000),
            cache_ttl: Duration::from_millis(cache_ttl_ms),
            cache_hard_ttl: Duration::from_millis(cache_hard_ttl_ms),
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

pub fn optional_env(name: &str) -> Option<String> {
    optional_from(&|key| env::var(key).ok(), name)
}

fn optional_from<F>(get: &F, name: &str) -> Option<String>
where
    F: Fn(&str) -> Option<String>,
{
    get(name).filter(|v| !v.is_empty())
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

#[cfg(test)]
mod tests {
    use super::*;

    fn config_from_pairs(pairs: &[(&str, &str)]) -> ServiceConfig {
        ServiceConfig::from_env_reader(|name| {
            pairs
                .iter()
                .find_map(|(key, value)| (*key == name).then(|| (*value).to_owned()))
        })
        .unwrap()
    }

    #[test]
    fn extracts_statefulset_ordinal() {
        assert_eq!(shard_id_from_pod_name("my-service-0"), Some(0));
        assert_eq!(shard_id_from_pod_name("fluxer-user-svc-12"), Some(12));
        assert_eq!(shard_id_from_pod_name("api"), None);
    }

    #[test]
    fn appends_port_when_missing() {
        assert_eq!(normalize_host("scylla", 9042), "scylla:9042");
        assert_eq!(normalize_host("scylla:9041", 9042), "scylla:9041");
        assert_eq!(normalize_host("127.0.0.1", 9042), "127.0.0.1:9042");
    }

    #[test]
    fn normalizes_ipv6_hosts() {
        assert_eq!(normalize_host("::1", 9042), "[::1]:9042");
        assert_eq!(normalize_host("[::1]", 9042), "[::1]:9042");
        assert_eq!(normalize_host("[::1]:9041", 9042), "[::1]:9041");
    }

    #[test]
    fn parses_comma_separated_hosts() {
        let hosts = parse_hosts("a:9042, b:9042, c:9042");
        assert_eq!(hosts, vec!["a:9042", "b:9042", "c:9042"]);
    }

    #[test]
    fn skips_empty_host_segments() {
        let hosts = parse_hosts(",a,,b,");
        assert_eq!(hosts, vec!["a", "b"]);
    }

    #[test]
    fn reads_canonical_cassandra_env_for_shards() {
        let cfg = config_from_pairs(&[
            ("FLUXER_SVC_NAME", "messages"),
            ("FLUXER_SVC_MODE", "shard"),
            ("FLUXER_SVC_SHARD_COUNT", "2"),
            ("FLUXER_SVC_SHARD_ID", "1"),
            ("FLUXER_CASSANDRA_HOSTS", "cass-a,cass-b:9142"),
            ("FLUXER_CASSANDRA_PORT", "9042"),
            ("FLUXER_CASSANDRA_KEYSPACE", "fluxer_dev"),
            ("FLUXER_CASSANDRA_USERNAME", "fluxer"),
            ("FLUXER_CASSANDRA_PASSWORD", "secret"),
        ]);

        assert_eq!("messages", cfg.service_name);
        assert_eq!(Mode::Shard, cfg.mode);
        assert_eq!(1, cfg.shard_id);
        assert_eq!(2, cfg.shard_count);
        assert_eq!(vec!["cass-a:9042", "cass-b:9142"], cfg.scylla_hosts);
        assert_eq!("fluxer_dev", cfg.scylla_keyspace);
        assert_eq!(Some("fluxer".to_owned()), cfg.scylla_username);
        assert_eq!(Some("secret".to_owned()), cfg.scylla_password);
    }

    #[test]
    fn defaults_database_backend_to_postgres() {
        let cfg = config_from_pairs(&[]);

        assert_eq!(DatabaseBackend::Postgres, cfg.database_backend);
        assert_eq!(None, cfg.postgres_url);
        assert_eq!("127.0.0.1", cfg.postgres_host);
        assert_eq!(5432, cfg.postgres_port);
        assert_eq!("fluxer", cfg.postgres_database);
        assert_eq!("fluxer", cfg.postgres_username);
        assert_eq!(Some("fluxer".to_owned()), cfg.postgres_password);
        assert!(!cfg.postgres_ssl);
        assert_eq!(None, cfg.postgres_ssl_ca);
        assert_eq!(20, cfg.postgres_max_connections);
        assert_eq!("fluxer_kv", cfg.postgres_kv_table);
        assert!(cfg.postgres_prepared_statements);
    }

    #[test]
    fn reads_postgres_database_env() {
        let cfg = config_from_pairs(&[
            ("FLUXER_DATABASE_BACKEND", "postgresql"),
            ("FLUXER_POSTGRES_URL", "postgres://user:pass@db/fluxer"),
            ("FLUXER_POSTGRES_HOST", "db"),
            ("FLUXER_POSTGRES_PORT", "6432"),
            ("FLUXER_POSTGRES_DATABASE", "fluxer_dev"),
            ("FLUXER_POSTGRES_USERNAME", "app"),
            ("FLUXER_POSTGRES_PASSWORD", "secret"),
            ("FLUXER_POSTGRES_SSL", "true"),
            ("FLUXER_POSTGRES_SSL_CA", "ca-pem"),
            ("FLUXER_POSTGRES_MAX_CONNECTIONS", "7"),
            ("FLUXER_POSTGRES_KV_TABLE", "fluxer_kv_dev"),
            ("FLUXER_POSTGRES_PREPARED_STATEMENTS", "false"),
        ]);

        assert_eq!(DatabaseBackend::Postgres, cfg.database_backend);
        assert_eq!(
            Some("postgres://user:pass@db/fluxer".to_owned()),
            cfg.postgres_url
        );
        assert_eq!("db", cfg.postgres_host);
        assert_eq!(6432, cfg.postgres_port);
        assert_eq!("fluxer_dev", cfg.postgres_database);
        assert_eq!("app", cfg.postgres_username);
        assert_eq!(Some("secret".to_owned()), cfg.postgres_password);
        assert!(cfg.postgres_ssl);
        assert_eq!(Some("ca-pem".to_owned()), cfg.postgres_ssl_ca);
        assert_eq!(7, cfg.postgres_max_connections);
        assert_eq!("fluxer_kv_dev", cfg.postgres_kv_table);
        assert!(!cfg.postgres_prepared_statements);
    }

    #[test]
    fn rejects_a_non_boolean_prepared_statements_value() {
        let result = ServiceConfig::from_env_reader(|name| {
            (name == "FLUXER_POSTGRES_PREPARED_STATEMENTS").then(|| "maybe".to_owned())
        });

        assert!(result.is_err());
    }

    #[test]
    fn raises_concurrency_defaults_for_hot_path_services() {
        assert_eq!(
            MESSAGES_MAX_CONCURRENT_REQUESTS,
            config_from_pairs(&[("FLUXER_SVC_NAME", "messages")]).max_concurrent_requests
        );
        assert_eq!(
            SNOWFLAKES_MAX_CONCURRENT_REQUESTS,
            config_from_pairs(&[("FLUXER_SVC_NAME", "snowflakes")]).max_concurrent_requests
        );
        assert_eq!(
            DEFAULT_MAX_CONCURRENT_REQUESTS,
            config_from_pairs(&[("FLUXER_SVC_NAME", "users")]).max_concurrent_requests
        );
    }

    #[test]
    fn concurrency_env_override_wins_over_service_default() {
        let cfg = config_from_pairs(&[
            ("FLUXER_SVC_NAME", "messages"),
            ("FLUXER_SVC_MAX_CONCURRENT_REQUESTS", "32"),
        ]);

        assert_eq!(32, cfg.max_concurrent_requests);
    }

    #[test]
    fn reads_legacy_cassandra_backend_aliases() {
        let cfg = config_from_pairs(&[("FLUXER_DATABASE_BACKEND", "scylla")]);

        assert_eq!(DatabaseBackend::Cassandra, cfg.database_backend);
    }
}
