// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::ServiceConfig;
use anyhow::Context;
use chrono::{DateTime, Utc};
use deadpool_postgres::{Client, Manager, Pool, Runtime, Transaction};
use rustls::{
    RootCertStore,
    pki_types::{CertificateDer, pem::PemObject},
};
use serde_json::{Map, Number, Value};
use std::str::FromStr;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::{Duration, Instant};
use tokio_postgres::{
    Config as PgConfig, Row,
    config::SslMode,
    error::SqlState,
    types::{ToSql, Type},
};
use tokio_postgres_rustls::MakeRustlsConnect;

const POSTGRES_KV_SCHEMA_LOCK_NAMESPACE: i32 = 0x4658_4b56;
const POSTGRES_KV_SCHEMA_LOCK_TIMEOUT: &str = "120s";
const POSTGRES_KV_SCHEMA_MIGRATION_TIMEOUT: &str = "30min";
const POSTGRES_KV_MIGRATION_TABLE: &str = "__fluxer_schema_migrations";
const POSTGRES_KV_MESSAGES_PARTITION_MIGRATION: &str = "messages_partition_key_v1";
const POSTGRES_KV_SORT_ID_MIGRATION: &str = "sort_id_v1";
const POSTGRES_KV_SORT_ID_TABLES: &[&str] = &["messages", "message_reactions"];
const POSTGRES_KV_SORT_ID_RECHECK: Duration = Duration::from_secs(30);
const POSTGRES_KV_STORAGE_OPTIONS: &[&str] = &[
    "fillfactor=85",
    "autovacuum_vacuum_scale_factor=0",
    "autovacuum_vacuum_threshold=20000",
    "autovacuum_vacuum_insert_scale_factor=0.05",
    "autovacuum_analyze_scale_factor=0.01",
];
const POSTGRES_KV_ALTER_LOCK_TIMEOUT: &str = "10s";
const POSTGRES_KV_SCHEMA_ATTEMPTS: u32 = 3;
const POSTGRES_KV_SCHEMA_RETRY_DELAY: Duration = Duration::from_millis(250);
const CACHED_JSON_FIELDS: &[&str] = &["message_id"];

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
    pub prepared_statements: bool,
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
            prepared_statements: config.postgres_prepared_statements,
        }
    }
}

pub async fn connect(config: &PostgresConfig) -> anyhow::Result<Pool> {
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

    apply_ssl_mode(&mut pg, config.ssl);

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
        prepared_statements = config.prepared_statements,
        "connected to Postgres"
    );
    Ok(pool)
}

fn apply_ssl_mode(pg: &mut PgConfig, ssl: bool) {
    if ssl {
        pg.ssl_mode(SslMode::Require);
    } else if pg.get_ssl_mode() == SslMode::Prefer {
        pg.ssl_mode(SslMode::Disable);
    }
}

fn build_tls_connector(ca_pem: Option<&str>) -> anyhow::Result<MakeRustlsConnect> {
    let _ = rustls::crypto::ring::default_provider().install_default();
    if let Some(ca_pem) = ca_pem.filter(|value| !value.trim().is_empty()) {
        let normalized = ca_pem.replace("\\n", "\n");
        let mut roots = RootCertStore::empty();
        for cert in CertificateDer::pem_slice_iter(normalized.as_bytes()) {
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

async fn row_key_is_c_collated(
    transaction: &Transaction<'_>,
    kv_table: &str,
) -> anyhow::Result<bool> {
    let row = transaction
        .query_opt(
            r#"SELECT col.collname = 'C' AND col.collnamespace = 'pg_catalog'::regnamespace AS c_collated
FROM pg_attribute att
JOIN pg_collation col ON col.oid = att.attcollation
WHERE att.attrelid = to_regclass($1)
    AND att.attname = 'row_key'
    AND NOT att.attisdropped"#,
            &[&kv_table],
        )
        .await
        .context("failed to inspect Postgres KV row_key collation")?;
    Ok(row.and_then(|row| row.get::<_, Option<bool>>("c_collated")) == Some(true))
}

async fn has_sort_id(transaction: &Transaction<'_>, kv_table: &str) -> anyhow::Result<bool> {
    let row = transaction
        .query_opt(
            "SELECT 1 FROM pg_attribute WHERE attrelid = to_regclass($1) AND attname = 'sort_id' AND NOT attisdropped",
            &[&kv_table],
        )
        .await
        .context("failed to inspect the Postgres KV sort_id column")?;
    Ok(row.is_some())
}

async fn has_sort_id_fill(transaction: &Transaction<'_>, kv_table: &str) -> anyhow::Result<bool> {
    let row = transaction
        .query_opt(
            "SELECT 1 FROM pg_trigger WHERE tgrelid = to_regclass($1) AND tgname = $2",
            &[&kv_table, &format!("{kv_table}_sort_id_fill")],
        )
        .await
        .context("failed to inspect the Postgres KV sort_id fill trigger")?;
    Ok(row.is_some())
}

async fn create_sort_id_fill(transaction: &Transaction<'_>, kv_table: &str) -> anyhow::Result<()> {
    let table = quote_identifier(kv_table)?;
    let fill = quote_identifier(&format!("{kv_table}_sort_id_fill"))?;
    transaction
        .batch_execute(&format!(
            r#"CREATE OR REPLACE FUNCTION {fill}() RETURNS trigger LANGUAGE plpgsql AS $fill$
BEGIN
    IF NEW.row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN
        NEW.sort_id := (NEW.row_data -> 'message_id' ->> 'value')::bigint;
    END IF;
    RETURN NEW;
END
$fill$;
CREATE TRIGGER {fill} BEFORE INSERT OR UPDATE ON {table} FOR EACH ROW WHEN (NEW.sort_id IS NULL AND NEW.table_name IN ('messages', 'message_reactions')) EXECUTE FUNCTION {fill}();"#
        ))
        .await
        .context("failed to create the Postgres KV sort_id fill trigger")?;
    Ok(())
}

async fn needs_lz4(transaction: &Transaction<'_>, kv_table: &str) -> anyhow::Result<bool> {
    let row = transaction
        .query_opt(
            r#"SELECT att.attcompression <> 'l' AND EXISTS (SELECT 1 FROM pg_settings WHERE name = 'default_toast_compression' AND 'lz4' = ANY(enumvals)) AS needs_lz4
FROM pg_attribute att
WHERE att.attrelid = to_regclass($1) AND att.attname = 'row_data' AND NOT att.attisdropped"#,
            &[&kv_table],
        )
        .await
        .context("failed to inspect Postgres KV row_data compression")?;
    Ok(row.and_then(|row| row.get::<_, Option<bool>>("needs_lz4")) == Some(true))
}

async fn missing_storage_options(
    transaction: &Transaction<'_>,
    kv_table: &str,
) -> anyhow::Result<Vec<&'static str>> {
    let row = transaction
        .query_opt(
            "SELECT reloptions AS options FROM pg_class WHERE oid = to_regclass($1)",
            &[&kv_table],
        )
        .await
        .context("failed to inspect Postgres KV storage options")?;
    let current = row
        .and_then(|row| row.get::<_, Option<Vec<String>>>("options"))
        .unwrap_or_default();
    Ok(POSTGRES_KV_STORAGE_OPTIONS
        .iter()
        .copied()
        .filter(|option| !current.iter().any(|value| value == option))
        .collect())
}

fn is_concurrent_ddl_conflict(error: &anyhow::Error) -> bool {
    error
        .chain()
        .filter_map(|cause| cause.downcast_ref::<tokio_postgres::Error>())
        .filter_map(tokio_postgres::Error::code)
        .any(|code| {
            *code == SqlState::UNIQUE_VIOLATION
                || *code == SqlState::DUPLICATE_TABLE
                || *code == SqlState::DUPLICATE_OBJECT
                || *code == SqlState::LOCK_NOT_AVAILABLE
        })
}

pub async fn ensure_kv_schema(pool: &Pool, kv_table: &str) -> anyhow::Result<()> {
    let mut attempt = 1;
    loop {
        match ensure_kv_schema_once(pool, kv_table).await {
            Ok(()) => return Ok(()),
            Err(err) => {
                if attempt >= POSTGRES_KV_SCHEMA_ATTEMPTS || !is_concurrent_ddl_conflict(&err) {
                    return Err(err);
                }
                tracing::warn!(
                    error = ?err,
                    kv_table,
                    attempt,
                    "retrying Postgres KV schema after a concurrent DDL conflict"
                );
                attempt += 1;
                tokio::time::sleep(POSTGRES_KV_SCHEMA_RETRY_DELAY).await;
            }
        }
    }
}

async fn ensure_kv_schema_once(pool: &Pool, kv_table: &str) -> anyhow::Result<()> {
    let table = quote_identifier(kv_table)?;
    let old_partition_index = quote_identifier(&format!("{kv_table}_partition_idx"))?;
    let row_key_c_index = quote_identifier(&format!("{kv_table}_row_key_c_idx"))?;
    let expires_index = quote_identifier(&format!("{kv_table}_expires_idx"))?;
    let mut client = pool.get().await?;
    let transaction = client
        .transaction()
        .await
        .context("failed to begin Postgres KV schema transaction")?;
    transaction
        .query_one(
            "SELECT set_config('statement_timeout', $1, true)",
            &[&POSTGRES_KV_SCHEMA_LOCK_TIMEOUT],
        )
        .await
        .context("failed to configure Postgres KV schema lock timeout")?;
    transaction
        .query_one(
            "SELECT pg_advisory_xact_lock($1, hashtext($2))",
            &[&POSTGRES_KV_SCHEMA_LOCK_NAMESPACE, &kv_table],
        )
        .await
        .context("failed to acquire Postgres KV schema lock")?;
    transaction
        .query_one(
            "SELECT set_config('statement_timeout', $1, true)",
            &[&POSTGRES_KV_SCHEMA_MIGRATION_TIMEOUT],
        )
        .await
        .context("failed to configure Postgres KV schema migration timeout")?;
    transaction
        .batch_execute(&format!(
            r#"
CREATE TABLE IF NOT EXISTS {table} (
    table_name text NOT NULL,
    partition_key text COLLATE "C" NOT NULL,
    row_key text COLLATE "C" NOT NULL,
    row_data jsonb NOT NULL,
    expires_at timestamptz,
    updated_at timestamptz NOT NULL DEFAULT now(),
    sort_id bigint,
    PRIMARY KEY (table_name, row_key)
);
"#
        ))
        .await
        .context("failed to ensure Postgres KV schema")?;
    let add_sort_id = !has_sort_id(&transaction, kv_table).await?;
    let add_sort_id_fill = !has_sort_id_fill(&transaction, kv_table).await?;
    let missing_options = missing_storage_options(&transaction, kv_table).await?;
    let set_lz4 = needs_lz4(&transaction, kv_table).await?;
    if add_sort_id || add_sort_id_fill || !missing_options.is_empty() || set_lz4 {
        transaction
            .query_one(
                "SELECT set_config('lock_timeout', $1, true)",
                &[&POSTGRES_KV_ALTER_LOCK_TIMEOUT],
            )
            .await
            .context("failed to configure the Postgres KV lock timeout")?;
        if add_sort_id {
            transaction
                .batch_execute(&format!("ALTER TABLE {table} ADD COLUMN sort_id bigint"))
                .await
                .context("failed to add the Postgres KV sort_id column")?;
        }
        if add_sort_id_fill {
            create_sort_id_fill(&transaction, kv_table).await?;
        }
        if !missing_options.is_empty() {
            transaction
                .batch_execute(&format!(
                    "ALTER TABLE {table} SET ({})",
                    missing_options.join(", ")
                ))
                .await
                .context("failed to set Postgres KV storage options")?;
        }
        if set_lz4 {
            transaction
                .batch_execute(&format!(
                    "ALTER TABLE {table} ALTER COLUMN row_data SET COMPRESSION lz4"
                ))
                .await
                .context("failed to set Postgres KV row_data compression")?;
        }
        transaction
            .query_one("SELECT set_config('lock_timeout', '0', true)", &[])
            .await
            .context("failed to reset the Postgres KV lock timeout")?;
    }
    if !row_key_is_c_collated(&transaction, kv_table).await? {
        transaction
            .batch_execute(&format!(
                r#"CREATE INDEX IF NOT EXISTS {row_key_c_index} ON {table} (table_name, row_key COLLATE "C");"#
            ))
            .await
            .context("failed to ensure Postgres KV schema")?;
    }
    transaction
        .batch_execute(&format!(
            r#"
CREATE INDEX IF NOT EXISTS {expires_index} ON {table} (expires_at) WHERE expires_at IS NOT NULL;
"#
        ))
        .await
        .context("failed to ensure Postgres KV schema")?;
    let migrated = transaction
        .query_typed_opt(
            &format!("SELECT 1 FROM {table} WHERE table_name = $1 AND row_key = $2 LIMIT 1"),
            &[
                (&POSTGRES_KV_MIGRATION_TABLE, Type::TEXT),
                (&POSTGRES_KV_MESSAGES_PARTITION_MIGRATION, Type::TEXT),
            ],
        )
        .await
        .context("failed to read the Postgres KV migration marker")?;
    if migrated.is_none() {
        let pending = transaction
            .query_typed_opt(
                &format!(
                    r#"
SELECT 1
FROM {table}
WHERE table_name = 'messages'
    AND partition_key = row_key
    AND split_part(row_key, chr(31), 3) <> ''
LIMIT 1"#
                ),
                &[],
            )
            .await
            .context("failed to probe the Postgres KV messages partition backfill")?;
        if pending.is_some() {
            transaction
                .batch_execute(&format!(
                    r#"
UPDATE {table}
SET partition_key = split_part(row_key, chr(31), 1) || chr(31) || split_part(row_key, chr(31), 2)
WHERE table_name = 'messages'
    AND partition_key = row_key
    AND split_part(row_key, chr(31), 3) <> '';
"#
                ))
                .await
                .context("failed to backfill Postgres KV message partition keys")?;
        }
        transaction
            .execute_typed(
                &format!(
                    r#"INSERT INTO {table} (table_name, partition_key, row_key, row_data)
VALUES ($1, $2, $2, jsonb_build_object('applied_at', now()))
ON CONFLICT (table_name, row_key) DO NOTHING"#
                ),
                &[
                    (&POSTGRES_KV_MIGRATION_TABLE, Type::TEXT),
                    (&POSTGRES_KV_MESSAGES_PARTITION_MIGRATION, Type::TEXT),
                ],
            )
            .await
            .context("failed to record the Postgres KV migration marker")?;
    }
    transaction
        .batch_execute(&format!("DROP INDEX IF EXISTS {old_partition_index};"))
        .await
        .context("failed to ensure Postgres KV schema")?;
    transaction
        .commit()
        .await
        .context("failed to commit Postgres KV schema transaction")?;
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

struct SortIdState {
    ready: AtomicBool,
    started: Instant,
    checked_at_ms: AtomicU64,
}

#[derive(Clone)]
pub struct KvClient {
    pool: Pool,
    prepared_statements: bool,
    get_row_sql: String,
    get_rows_sql: String,
    get_partition_rows_sql: String,
    get_ordered_rows_sql: String,
    delete_row_sql: String,
    sort_ids: Arc<SortIdState>,
}

impl KvClient {
    pub fn new(pool: Pool, config: &PostgresConfig) -> anyhow::Result<Self> {
        let table = quote_identifier(&config.kv_table)?;
        Ok(Self {
            pool,
            prepared_statements: config.prepared_statements,
            get_row_sql: format!(
                "SELECT row_data FROM {table} WHERE table_name = $1 AND row_key = $2 AND (expires_at IS NULL OR expires_at > now()) LIMIT 1"
            ),
            get_rows_sql: format!(
                "SELECT row_key, row_data FROM {table} WHERE table_name = $1 AND row_key = ANY($2::text[]) AND (expires_at IS NULL OR expires_at > now())"
            ),
            get_partition_rows_sql: format!(
                r#"SELECT row_key, row_data FROM {table} WHERE table_name = $1 AND (row_key = $2 OR (row_key COLLATE "C" >= $2 || chr(31) AND row_key COLLATE "C" < $2 || ' ')) AND (expires_at IS NULL OR expires_at > now())"#
            ),
            get_ordered_rows_sql: format!(
                "SELECT row_key, row_data FROM {table} WHERE table_name = $1 AND partition_key = $2 AND (expires_at IS NULL OR expires_at > now())"
            ),
            delete_row_sql: format!("DELETE FROM {table} WHERE table_name = $1 AND row_key = $2"),
            sort_ids: Arc::new(SortIdState {
                ready: AtomicBool::new(false),
                started: Instant::now(),
                checked_at_ms: AtomicU64::new(0),
            }),
        })
    }

    async fn sort_ids_ready(&self, client: &Client) -> bool {
        if self.sort_ids.ready.load(Ordering::Relaxed) {
            return true;
        }
        let now_ms = (self.sort_ids.started.elapsed().as_millis() as u64).max(1);
        let checked_at_ms = self.sort_ids.checked_at_ms.load(Ordering::Relaxed);
        if checked_at_ms != 0
            && now_ms.saturating_sub(checked_at_ms) < POSTGRES_KV_SORT_ID_RECHECK.as_millis() as u64
        {
            return false;
        }
        self.sort_ids.checked_at_ms.store(now_ms, Ordering::Relaxed);
        let marked = self
            .query_row(
                client,
                &self.get_row_sql,
                &[
                    (&POSTGRES_KV_MIGRATION_TABLE, Type::TEXT),
                    (&POSTGRES_KV_SORT_ID_MIGRATION, Type::TEXT),
                ],
            )
            .await;
        match marked {
            Ok(Some(_)) => {
                self.sort_ids.ready.store(true, Ordering::Relaxed);
                true
            }
            Ok(None) => false,
            Err(error) => {
                tracing::warn!(error = ?error, "failed to read the Postgres KV sort_id marker");
                false
            }
        }
    }

    async fn ordered_rows_sql(
        &self,
        client: &Client,
        table_name: &str,
        field_name: &str,
    ) -> anyhow::Result<(String, String)> {
        let base = &self.get_ordered_rows_sql;
        if field_name == "message_id"
            && let Some(sorted) = POSTGRES_KV_SORT_ID_TABLES
                .iter()
                .find(|sorted| **sorted == table_name)
            && self.sort_ids_ready(client).await
        {
            return Ok((
                format!("{base} AND table_name = '{sorted}'"),
                "sort_id".to_owned(),
            ));
        }
        Ok((base.clone(), json_field_expr(field_name)?))
    }

    async fn query_rows(
        &self,
        client: &Client,
        sql: &str,
        params: &[(&(dyn ToSql + Sync), Type)],
    ) -> anyhow::Result<Vec<Row>> {
        if !self.prepared_statements {
            return Ok(client.query_typed(sql, params).await?);
        }
        let statement = client.prepare_cached(sql).await?;
        Ok(client.query(&statement, &statement_params(params)).await?)
    }

    async fn query_row(
        &self,
        client: &Client,
        sql: &str,
        params: &[(&(dyn ToSql + Sync), Type)],
    ) -> anyhow::Result<Option<Row>> {
        if !self.prepared_statements {
            return Ok(client.query_typed_opt(sql, params).await?);
        }
        let statement = client.prepare_cached(sql).await?;
        Ok(client
            .query_opt(&statement, &statement_params(params))
            .await?)
    }

    async fn execute(
        &self,
        client: &Client,
        sql: &str,
        params: &[(&(dyn ToSql + Sync), Type)],
    ) -> anyhow::Result<u64> {
        if !self.prepared_statements {
            return Ok(client.execute_typed(sql, params).await?);
        }
        let statement = client.prepare_cached(sql).await?;
        Ok(client
            .execute(&statement, &statement_params(params))
            .await?)
    }

    async fn query_dynamic_rows(
        &self,
        client: &Client,
        sql: &str,
        field_name: &str,
        params: &[(&(dyn ToSql + Sync), Type)],
    ) -> anyhow::Result<Vec<Row>> {
        if !self.prepared_statements {
            return Ok(client.query_typed(sql, params).await?);
        }
        let statement = if is_cached_json_field(field_name) {
            client.prepare_cached(sql).await?
        } else {
            client.prepare(sql).await?
        };
        Ok(client.query(&statement, &statement_params(params)).await?)
    }

    pub async fn get_row(&self, table_name: &str, row_key: &str) -> anyhow::Result<Option<Value>> {
        let client = self.pool.get().await?;
        let row = self
            .query_row(
                &client,
                &self.get_row_sql,
                &[(&table_name, Type::TEXT), (&row_key, Type::TEXT)],
            )
            .await?;
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
        let rows = self
            .query_rows(
                &client,
                &self.get_rows_sql,
                &[(&table_name, Type::TEXT), (&row_keys, Type::TEXT_ARRAY)],
            )
            .await?;
        Ok(rows.into_iter().map(row_key_and_data).collect())
    }

    pub async fn get_partition_rows(
        &self,
        table_name: &str,
        partition_key: &str,
    ) -> anyhow::Result<Vec<(String, Value)>> {
        let client = self.pool.get().await?;
        let rows = self
            .query_rows(
                &client,
                &self.get_partition_rows_sql,
                &[(&table_name, Type::TEXT), (&partition_key, Type::TEXT)],
            )
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
        let (base, field_expr) = self
            .ordered_rows_sql(&client, table_name, field_name)
            .await?;
        let direction = if desc { "DESC" } else { "ASC" };
        let rows = match bound {
            Some(BigIntBound::LessThan(value)) => {
                let sql = format!(
                    "{base} AND {field_expr} < $3 ORDER BY {field_expr} {direction} LIMIT $4"
                );
                self.query_dynamic_rows(
                    &client,
                    &sql,
                    field_name,
                    &[
                        (&table_name, Type::TEXT),
                        (&partition_key, Type::TEXT),
                        (&value, Type::INT8),
                        (&limit, Type::INT8),
                    ],
                )
                .await?
            }
            Some(BigIntBound::GreaterThan(value)) => {
                let sql = format!(
                    "{base} AND {field_expr} > $3 ORDER BY {field_expr} {direction} LIMIT $4"
                );
                self.query_dynamic_rows(
                    &client,
                    &sql,
                    field_name,
                    &[
                        (&table_name, Type::TEXT),
                        (&partition_key, Type::TEXT),
                        (&value, Type::INT8),
                        (&limit, Type::INT8),
                    ],
                )
                .await?
            }
            None => {
                let sql = format!("{base} ORDER BY {field_expr} {direction} LIMIT $3");
                self.query_dynamic_rows(
                    &client,
                    &sql,
                    field_name,
                    &[
                        (&table_name, Type::TEXT),
                        (&partition_key, Type::TEXT),
                        (&limit, Type::INT8),
                    ],
                )
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
        let (base, field_expr) = self
            .ordered_rows_sql(&client, table_name, field_name)
            .await?;
        let sql = format!("{base} AND {field_expr} = ANY($3::bigint[])");
        let rows = self
            .query_dynamic_rows(
                &client,
                &sql,
                field_name,
                &[
                    (&table_name, Type::TEXT),
                    (&partition_key, Type::TEXT),
                    (&values, Type::INT8_ARRAY),
                ],
            )
            .await?;
        Ok(rows.into_iter().map(row_key_and_data).collect())
    }

    pub async fn delete_row(&self, table_name: &str, row_key: &str) -> anyhow::Result<()> {
        let client = self.pool.get().await?;
        self.execute(
            &client,
            &self.delete_row_sql,
            &[(&table_name, Type::TEXT), (&row_key, Type::TEXT)],
        )
        .await?;
        Ok(())
    }

    pub async fn query(
        &self,
        sql: &str,
        params: &[(&(dyn ToSql + Sync), Type)],
    ) -> anyhow::Result<Vec<Row>> {
        let client = self.pool.get().await?;
        if !self.prepared_statements {
            return Ok(client.query_typed(sql, params).await?);
        }
        Ok(client.query(sql, &statement_params(params)).await?)
    }
}

fn statement_params<'a>(
    params: &'a [(&'a (dyn ToSql + Sync), Type)],
) -> Vec<&'a (dyn ToSql + Sync)> {
    params.iter().map(|(value, _)| *value).collect()
}

fn is_cached_json_field(field_name: &str) -> bool {
    CACHED_JSON_FIELDS.contains(&field_name)
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

pub fn decode_row_dates_as_millis(value: Value) -> anyhow::Result<Value> {
    decode_value(value)
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

fn decode_value(value: Value) -> anyhow::Result<Value> {
    match value {
        Value::Array(values) => values
            .into_iter()
            .map(decode_value)
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
                Ok(DateTime::parse_from_rfc3339(&value)
                    .map(|dt| {
                        Value::Number(Number::from(dt.with_timezone(&Utc).timestamp_millis()))
                    })
                    .unwrap_or(Value::String(value)))
            }
            Some("buffer" | "local_date") => Ok(object.remove("value").unwrap_or(Value::Null)),
            Some("set") => match object.remove("value").unwrap_or(Value::Null) {
                Value::Array(values) => values
                    .into_iter()
                    .map(decode_value)
                    .collect::<anyhow::Result<Vec<_>>>()
                    .map(Value::Array),
                _ => Ok(Value::Array(Vec::new())),
            },
            Some("map") => match object.remove("value").unwrap_or(Value::Null) {
                Value::Array(entries) => entries
                    .into_iter()
                    .map(decode_value)
                    .collect::<anyhow::Result<Vec<_>>>()
                    .map(Value::Array),
                _ => Ok(Value::Array(Vec::new())),
            },
            _ => object
                .into_iter()
                .map(|(key, value)| decode_value(value).map(|value| (key, value)))
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
