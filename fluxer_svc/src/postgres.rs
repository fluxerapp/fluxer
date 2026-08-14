// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ServiceConfig;
use anyhow::Context;
use chrono::{DateTime, Utc};
use deadpool_postgres::{Manager, Pool, Runtime};
use rustls::RootCertStore;
use serde_json::{Map, Number, Value};
use std::io::Cursor;
use std::str::FromStr;
use tokio_postgres::{Config as PgConfig, Row, config::SslMode, types::ToSql};
use tokio_postgres_rustls::MakeRustlsConnect;

#[derive(Clone, Debug)]
pub struct PostgresConfig {
    pub url: Option<String>,
    pub host: String,
    pub port: u16,
    pub database: String,
    pub username: String,
    pub password: Option<String>,
    pub ssl: bool,
    pub ssl_ca: Option<String>,
    pub max_connections: usize,
    pub kv_table: String,
}

impl PostgresConfig {
    pub fn from_service_config(config: &ServiceConfig) -> Self {
        Self {
            url: config.postgres_url.clone(),
            host: config.postgres_host.clone(),
            port: config.postgres_port,
            database: config.postgres_database.clone(),
            username: config.postgres_username.clone(),
            password: config.postgres_password.clone(),
            ssl: config.postgres_ssl,
            ssl_ca: config.postgres_ssl_ca.clone(),
            max_connections: config.postgres_max_connections,
            kv_table: config.postgres_kv_table.clone(),
        }
    }
}

pub async fn connect(config: &PostgresConfig) -> anyhow::Result<Pool> {
    let has_url = config.url.is_some();
    let mut pg = if let Some(url) = &config.url {
        PgConfig::from_str(url).context("failed to parse FLUXER_POSTGRES_URL")?
    } else {
        let mut pg = PgConfig::new();
        pg.host(&config.host);
        pg.port(config.port);
        pg.dbname(&config.database);
        pg.user(&config.username);
        if let Some(password) = &config.password {
            pg.password(password);
        }
        pg
    };

    if config.ssl {
        pg.ssl_mode(SslMode::Require);
    } else if !has_url {
        pg.ssl_mode(SslMode::Disable);
    }

    let tls = if pg.get_ssl_mode() == SslMode::Disable {
        build_disabled_tls_connector()
    } else {
        build_tls_connector(config.ssl_ca.as_deref())?
    };
    let manager = Manager::new(pg, tls);
    let pool = Pool::builder(manager)
        .max_size(config.max_connections)
        .runtime(Runtime::Tokio1)
        .build()
        .context("failed to build Postgres pool")?;

    let client = pool.get().await.context("failed to connect to Postgres")?;
    client.simple_query("SELECT 1").await?;
    drop(client);
    ensure_kv_schema(&pool, &config.kv_table).await?;
    tracing::info!(
        host = config.host,
        port = config.port,
        database = config.database,
        max_connections = config.max_connections,
        kv_table = config.kv_table,
        "connected to Postgres"
    );
    Ok(pool)
}

fn build_tls_connector(ca_pem: Option<&str>) -> anyhow::Result<MakeRustlsConnect> {
    let _ = rustls::crypto::ring::default_provider().install_default();
    if let Some(ca_pem) = ca_pem.filter(|value| !value.trim().is_empty()) {
        let normalized = ca_pem.replace("\\n", "\n");
        let mut reader = Cursor::new(normalized.as_bytes());
        let mut roots = RootCertStore::empty();
        for cert in rustls_pemfile::certs(&mut reader) {
            roots.add(cert.context("failed to parse FLUXER_POSTGRES_SSL_CA certificate")?)?;
        }
        if roots.is_empty() {
            anyhow::bail!("FLUXER_POSTGRES_SSL_CA did not contain any certificates");
        }
        let tls_config = rustls::ClientConfig::builder()
            .with_root_certificates(roots)
            .with_no_client_auth();
        return Ok(MakeRustlsConnect::new(tls_config));
    }
    let (connector, errors) = MakeRustlsConnect::with_native_certs().map_err(|errors| {
        anyhow::anyhow!("failed to load native TLS roots for Postgres: {errors:?}")
    })?;
    if !errors.is_empty() {
        tracing::warn!(errors = ?errors, "loaded Postgres TLS roots with native certificate store warnings");
    }
    Ok(connector)
}

fn build_disabled_tls_connector() -> MakeRustlsConnect {
    let _ = rustls::crypto::ring::default_provider().install_default();
    let tls_config = rustls::ClientConfig::builder()
        .with_root_certificates(RootCertStore::empty())
        .with_no_client_auth();
    MakeRustlsConnect::new(tls_config)
}

pub async fn ensure_kv_schema(pool: &Pool, kv_table: &str) -> anyhow::Result<()> {
    let table = quote_identifier(kv_table)?;
    let old_partition_index = quote_identifier(&format!("{kv_table}_partition_idx"))?;
    let partition_row_index = quote_identifier(&format!("{kv_table}_partition_row_idx"))?;
    let row_key_c_index = quote_identifier(&format!("{kv_table}_row_key_c_idx"))?;
    let expires_index = quote_identifier(&format!("{kv_table}_expires_idx"))?;
    let messages_message_index = quote_identifier(&format!("{kv_table}_messages_message_idx"))?;
    let message_reactions_message_index =
        quote_identifier(&format!("{kv_table}_message_reactions_message_idx"))?;
    let client = pool.get().await?;
    client
        .batch_execute(&format!(
            r#"
CREATE TABLE IF NOT EXISTS {table} (
    table_name text NOT NULL,
    partition_key text NOT NULL,
    row_key text NOT NULL,
    row_data jsonb NOT NULL,
    expires_at timestamptz,
    updated_at timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (table_name, row_key)
);
CREATE INDEX IF NOT EXISTS {partition_row_index} ON {table} (table_name, partition_key, row_key);
CREATE INDEX IF NOT EXISTS {row_key_c_index} ON {table} (table_name, row_key COLLATE "C");
CREATE INDEX IF NOT EXISTS {expires_index} ON {table} (expires_at) WHERE expires_at IS NOT NULL;
CREATE INDEX IF NOT EXISTS {messages_message_index} ON {table} (partition_key, ((CASE WHEN row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN (row_data -> 'message_id' ->> 'value')::bigint END))) WHERE table_name = 'messages';
CREATE INDEX IF NOT EXISTS {message_reactions_message_index} ON {table} (partition_key, ((CASE WHEN row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN (row_data -> 'message_id' ->> 'value')::bigint END))) WHERE table_name = 'message_reactions';
UPDATE {table}
SET partition_key = split_part(row_key, chr(31), 1) || chr(31) || split_part(row_key, chr(31), 2)
WHERE table_name = 'messages'
    AND partition_key = row_key
    AND split_part(row_key, chr(31), 3) <> '';
DROP INDEX IF EXISTS {old_partition_index};
"#
        ))
        .await
        .context("failed to ensure Postgres KV schema")?;
    Ok(())
}

pub fn quote_identifier(identifier: &str) -> anyhow::Result<String> {
    if !is_safe_identifier(identifier) {
        anyhow::bail!("unsafe Postgres identifier: {identifier:?}");
    }
    Ok(format!("\"{identifier}\""))
}

fn is_safe_identifier(identifier: &str) -> bool {
    let mut chars = identifier.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}

#[derive(Clone)]
pub struct KvClient {
    pool: Pool,
    table: String,
}

impl KvClient {
    pub fn new(pool: Pool, kv_table: &str) -> anyhow::Result<Self> {
        Ok(Self {
            pool,
            table: quote_identifier(kv_table)?,
        })
    }

    pub async fn get_row(&self, table_name: &str, row_key: &str) -> anyhow::Result<Option<Value>> {
        let client = self.pool.get().await?;
        let sql = format!(
            "SELECT row_data FROM {} WHERE table_name = $1 AND row_key = $2 AND (expires_at IS NULL OR expires_at > now()) LIMIT 1",
            self.table
        );
        let row = client.query_opt(&sql, &[&table_name, &row_key]).await?;
        Ok(row.map(|row| row.get::<_, Value>("row_data")))
    }

    pub async fn get_rows(
        &self,
        table_name: &str,
        row_keys: &[String],
    ) -> anyhow::Result<Vec<(String, Value)>> {
        if row_keys.is_empty() {
            return Ok(Vec::new());
        }
        let client = self.pool.get().await?;
        let sql = format!(
            "SELECT row_key, row_data FROM {} WHERE table_name = $1 AND row_key = ANY($2::text[]) AND (expires_at IS NULL OR expires_at > now())",
            self.table
        );
        let rows = client.query(&sql, &[&table_name, &row_keys]).await?;
        Ok(rows.into_iter().map(row_key_and_data).collect())
    }

    pub async fn get_partition_rows(
        &self,
        table_name: &str,
        partition_key: &str,
    ) -> anyhow::Result<Vec<(String, Value)>> {
        let client = self.pool.get().await?;
        let sql = format!(
            "SELECT row_key, row_data FROM {} WHERE table_name = $1 AND partition_key = $2 AND (expires_at IS NULL OR expires_at > now())",
            self.table
        );
        let rows = client.query(&sql, &[&table_name, &partition_key]).await?;
        Ok(rows.into_iter().map(row_key_and_data).collect())
    }

    pub async fn get_row_key_prefix_rows(
        &self,
        table_name: &str,
        row_key_prefix: &str,
    ) -> anyhow::Result<Vec<(String, Value)>> {
        let client = self.pool.get().await?;
        let upper = format!("{row_key_prefix}\u{10ffff}");
        let sql = format!(
            "SELECT row_key, row_data FROM {} WHERE table_name = $1 AND row_key COLLATE \"C\" >= $2 AND row_key COLLATE \"C\" < $3 AND (expires_at IS NULL OR expires_at > now())",
            self.table
        );
        let rows = client
            .query(&sql, &[&table_name, &row_key_prefix, &upper])
            .await?;
        Ok(rows.into_iter().map(row_key_and_data).collect())
    }

    pub async fn get_partition_rows_by_bigint_field(
        &self,
        table_name: &str,
        partition_key: &str,
        field_name: &str,
        bound: Option<BigIntBound>,
        desc: bool,
        limit: i64,
    ) -> anyhow::Result<Vec<(String, Value)>> {
        if limit <= 0 {
            return Ok(Vec::new());
        }
        let client = self.pool.get().await?;
        let field_expr = json_field_expr(field_name)?;
        let direction = if desc { "DESC" } else { "ASC" };
        let base = format!(
            "SELECT row_key, row_data FROM {} WHERE table_name = $1 AND partition_key = $2 AND (expires_at IS NULL OR expires_at > now())",
            self.table
        );
        let rows = match bound {
            Some(BigIntBound::LessThan(value)) => {
                let sql = format!(
                    "{base} AND {field_expr} < $3 ORDER BY {field_expr} {direction} LIMIT $4"
                );
                client
                    .query(&sql, &[&table_name, &partition_key, &value, &limit])
                    .await?
            }
            Some(BigIntBound::GreaterThan(value)) => {
                let sql = format!(
                    "{base} AND {field_expr} > $3 ORDER BY {field_expr} {direction} LIMIT $4"
                );
                client
                    .query(&sql, &[&table_name, &partition_key, &value, &limit])
                    .await?
            }
            None => {
                let sql = format!("{base} ORDER BY {field_expr} {direction} LIMIT $3");
                client
                    .query(&sql, &[&table_name, &partition_key, &limit])
                    .await?
            }
        };
        Ok(rows.into_iter().map(row_key_and_data).collect())
    }

    pub async fn get_partition_rows_by_bigint_field_values(
        &self,
        table_name: &str,
        partition_key: &str,
        field_name: &str,
        values: &[i64],
    ) -> anyhow::Result<Vec<(String, Value)>> {
        if values.is_empty() {
            return Ok(Vec::new());
        }
        let client = self.pool.get().await?;
        let field_expr = json_field_expr(field_name)?;
        let sql = format!(
            "SELECT row_key, row_data FROM {} WHERE table_name = $1 AND partition_key = $2 AND {field_expr} = ANY($3::bigint[]) AND (expires_at IS NULL OR expires_at > now())",
            self.table
        );
        let rows = client
            .query(&sql, &[&table_name, &partition_key, &values])
            .await?;
        Ok(rows.into_iter().map(row_key_and_data).collect())
    }

    pub async fn delete_row(&self, table_name: &str, row_key: &str) -> anyhow::Result<()> {
        let client = self.pool.get().await?;
        let sql = format!(
            "DELETE FROM {} WHERE table_name = $1 AND row_key = $2",
            self.table
        );
        client.execute(&sql, &[&table_name, &row_key]).await?;
        Ok(())
    }

    pub async fn query(
        &self,
        sql: &str,
        params: &[&(dyn ToSql + Sync)],
    ) -> anyhow::Result<Vec<Row>> {
        let client = self.pool.get().await?;
        Ok(client.query(sql, params).await?)
    }
}

fn row_key_and_data(row: Row) -> (String, Value) {
    (
        row.get::<_, String>("row_key"),
        row.get::<_, Value>("row_data"),
    )
}

#[derive(Clone, Copy, Debug)]
pub enum KeyPart<'a> {
    BigInt(i64),
    Number(i64),
    Bool(bool),
    String(&'a str),
}

#[derive(Clone, Copy, Debug)]
pub enum BigIntBound {
    LessThan(i64),
    GreaterThan(i64),
}

fn json_field_expr(field_name: &str) -> anyhow::Result<String> {
    if !is_safe_identifier(field_name) {
        anyhow::bail!("unsafe Postgres JSON field name: {field_name:?}");
    }
    Ok(format!(
        "(CASE WHEN row_data -> '{field_name}' ->> 'value' ~ '^-?[0-9]+$' THEN (row_data -> '{field_name}' ->> 'value')::bigint END)"
    ))
}

pub fn kv_key(parts: &[KeyPart<'_>]) -> anyhow::Result<String> {
    parts
        .iter()
        .map(encoded_key_part)
        .collect::<anyhow::Result<Vec<_>>>()
        .map(|parts| parts.join("\u{001f}"))
}

pub fn decode_row(value: Value) -> anyhow::Result<Value> {
    decode_value(value, DecodeDateMode::String)
}

pub fn decode_row_dates_as_millis(value: Value) -> anyhow::Result<Value> {
    decode_value(value, DecodeDateMode::Millis)
}

#[derive(Clone, Copy)]
enum DecodeDateMode {
    String,
    Millis,
}

fn encoded_key_part(part: &KeyPart<'_>) -> anyhow::Result<String> {
    let value = match part {
        KeyPart::BigInt(value) => {
            let mut object = Map::new();
            object.insert(
                "__fluxer_type".to_owned(),
                Value::String("bigint".to_owned()),
            );
            object.insert("value".to_owned(), Value::String(value.to_string()));
            Value::Object(object)
        }
        KeyPart::Number(value) => Value::Number(Number::from(*value)),
        KeyPart::Bool(value) => Value::Bool(*value),
        KeyPart::String(value) => Value::String((*value).to_owned()),
    };
    Ok(serde_json::to_string(&value)?)
}

fn decode_value(value: Value, date_mode: DecodeDateMode) -> anyhow::Result<Value> {
    match value {
        Value::Array(values) => values
            .into_iter()
            .map(|value| decode_value(value, date_mode))
            .collect::<anyhow::Result<Vec<_>>>()
            .map(Value::Array),
        Value::Object(mut object) => match object.get("__fluxer_type").and_then(Value::as_str) {
            Some("bigint") => {
                let value = object
                    .remove("value")
                    .and_then(|value| value.as_str().map(ToOwned::to_owned))
                    .unwrap_or_default();
                Ok(value
                    .parse::<i64>()
                    .ok()
                    .map(|value| Value::Number(Number::from(value)))
                    .unwrap_or(Value::String(value)))
            }
            Some("date") => {
                let value = object
                    .remove("value")
                    .and_then(|value| value.as_str().map(ToOwned::to_owned))
                    .unwrap_or_default();
                match date_mode {
                    DecodeDateMode::String => Ok(Value::String(value)),
                    DecodeDateMode::Millis => Ok(DateTime::parse_from_rfc3339(&value)
                        .map(|dt| {
                            Value::Number(Number::from(dt.with_timezone(&Utc).timestamp_millis()))
                        })
                        .unwrap_or(Value::String(value))),
                }
            }
            Some("buffer" | "local_date") => Ok(object.remove("value").unwrap_or(Value::Null)),
            Some("set") => match object.remove("value").unwrap_or(Value::Null) {
                Value::Array(values) => values
                    .into_iter()
                    .map(|value| decode_value(value, date_mode))
                    .collect::<anyhow::Result<Vec<_>>>()
                    .map(Value::Array),
                _ => Ok(Value::Array(Vec::new())),
            },
            Some("map") => match object.remove("value").unwrap_or(Value::Null) {
                Value::Array(entries) => entries
                    .into_iter()
                    .map(|entry| decode_value(entry, date_mode))
                    .collect::<anyhow::Result<Vec<_>>>()
                    .map(Value::Array),
                _ => Ok(Value::Array(Vec::new())),
            },
            _ => object
                .into_iter()
                .map(|(key, value)| decode_value(value, date_mode).map(|value| (key, value)))
                .collect::<anyhow::Result<Map<_, _>>>()
                .map(Value::Object),
        },
        value => Ok(value),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn encodes_row_keys_like_postgres_kv_executor() {
        assert_eq!(
            kv_key(&[KeyPart::BigInt(42)]).unwrap(),
            r#"{"__fluxer_type":"bigint","value":"42"}"#
        );
        assert_eq!(
            kv_key(&[
                KeyPart::BigInt(10),
                KeyPart::Number(416),
                KeyPart::String("wave")
            ])
            .unwrap(),
            "{\"__fluxer_type\":\"bigint\",\"value\":\"10\"}\u{001f}416\u{001f}\"wave\""
        );
        assert_eq!(
            kv_key(&[KeyPart::BigInt(5), KeyPart::Bool(false)]).unwrap(),
            "{\"__fluxer_type\":\"bigint\",\"value\":\"5\"}\u{001f}false"
        );
    }

    #[test]
    fn decodes_tagged_json_values() {
        let decoded = decode_row_dates_as_millis(json!({
            "id": {"__fluxer_type": "bigint", "value": "1509197195776110592"},
            "when": {"__fluxer_type": "date", "value": "2026-06-15T12:34:56.789Z"},
            "birth": {"__fluxer_type": "local_date", "value": "1999-01-02"},
            "bytes": {"__fluxer_type": "buffer", "value": "YWJj"},
            "ids": {"__fluxer_type": "set", "value": [
                {"__fluxer_type": "bigint", "value": "1"},
                {"__fluxer_type": "bigint", "value": "2"}
            ]},
            "metadata": {"__fluxer_type": "map", "value": [
                ["kind", {"__fluxer_type": "bigint", "value": "9"}]
            ]}
        }))
        .unwrap();

        assert_eq!(decoded["id"], json!(1_509_197_195_776_110_592_i64));
        assert_eq!(decoded["when"], json!(1_781_526_896_789_i64));
        assert_eq!(decoded["birth"], json!("1999-01-02"));
        assert_eq!(decoded["bytes"], json!("YWJj"));
        assert_eq!(decoded["ids"], json!([1, 2]));
        assert_eq!(decoded["metadata"], json!([["kind", 9]]));
    }

    #[test]
    fn rejects_unsafe_identifiers() {
        assert!(quote_identifier("fluxer_kv").is_ok());
        assert!(quote_identifier("fluxer-kv").is_err());
        assert!(quote_identifier("1kv").is_err());
    }
}
