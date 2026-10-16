// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::paths::DEV_CASSANDRA_DIR;
use anyhow::{Context, Result, bail};
use regex::Regex;
use scylla::DeserializeRow;
use scylla::client::session::Session;
use scylla::client::session_builder::SessionBuilder;
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::path::{Path, PathBuf};
use std::sync::LazyLock;
use std::time::{Duration, Instant};
use tokio::time::sleep;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CassandraConfig {
    pub host: String,
    pub port: u16,
    pub keyspace: String,
    pub local_dc: String,
    pub username: Option<String>,
    pub password: Option<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ExistingTable {
    pub columns: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ExistingSchema {
    pub keyspace_exists: bool,
    pub types: BTreeMap<String, BTreeMap<String, String>>,
    pub tables: BTreeMap<String, ExistingTable>,
    pub indexes: BTreeSet<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchemaDiff {
    pub statements: Vec<String>,
    pub warnings: Vec<String>,
    pub errors: Vec<String>,
    pub target_sha256: String,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    #[serde(rename = "type")]
    pub field_type: String,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct UserType {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct Table {
    pub name: String,
    pub columns: Vec<Field>,
    pub primary_key: String,
    pub options: String,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct Index {
    pub name: String,
    pub table: String,
    pub expression: String,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct TargetSchema {
    pub user_types: Vec<UserType>,
    pub tables: Vec<Table>,
    pub indexes: Vec<Index>,
}

static TARGET_SCHEMA: LazyLock<TargetSchema> = LazyLock::new(|| {
    serde_json::from_str(include_str!("../cassandra_target_schema.json"))
        .expect("embedded Cassandra target schema JSON is valid")
});

static TYPE_TOKEN_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"[a-zA-Z_][a-zA-Z0-9_]*").expect("valid type token regex"));
static WHITESPACE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\s+").expect("valid whitespace regex"));

pub fn target_schema() -> &'static TargetSchema {
    &TARGET_SCHEMA
}

pub fn config_from_env() -> Result<CassandraConfig> {
    let hosts = env::var("FLUXER_CASSANDRA_HOSTS").unwrap_or_else(|_| "cassandra".to_owned());
    let host = hosts
        .split_once(',')
        .map(|(first, _)| first)
        .unwrap_or(&hosts)
        .trim();
    Ok(CassandraConfig {
        host: if host.is_empty() {
            "cassandra".to_owned()
        } else {
            host.to_owned()
        },
        port: env::var("FLUXER_CASSANDRA_PORT")
            .unwrap_or_else(|_| "9042".to_owned())
            .parse()
            .context("invalid FLUXER_CASSANDRA_PORT")?,
        keyspace: env::var("FLUXER_CASSANDRA_KEYSPACE").unwrap_or_else(|_| "fluxer".to_owned()),
        local_dc: env::var("FLUXER_CASSANDRA_LOCAL_DC")
            .unwrap_or_else(|_| "datacenter1".to_owned()),
        username: env::var("FLUXER_CASSANDRA_USERNAME")
            .ok()
            .filter(|value| !value.is_empty()),
        password: env::var("FLUXER_CASSANDRA_PASSWORD")
            .ok()
            .filter(|value| !value.is_empty()),
    })
}

async fn connect(config: &CassandraConfig) -> Result<Session> {
    let contact = format!("{}:{}", config.host, config.port);
    let mut builder = SessionBuilder::new().known_node(contact);
    if let Some(username) = &config.username {
        builder = builder.user(username, config.password.as_deref().unwrap_or_default());
    }
    Ok(builder.build().await?)
}

pub async fn wait_for_cassandra(config: &CassandraConfig, timeout_secs: u64) -> Result<()> {
    let deadline = Instant::now() + Duration::from_secs(timeout_secs);
    let mut last_error = None;
    while Instant::now() < deadline {
        match connect(config).await {
            Ok(session) => match session
                .query_unpaged("SELECT release_version FROM system.local", ())
                .await
            {
                Ok(_) => {
                    println!("Cassandra is reachable at {}:{}", config.host, config.port);
                    return Ok(());
                }
                Err(error) => last_error = Some(error.to_string()),
            },
            Err(error) => last_error = Some(error.to_string()),
        }
        sleep(Duration::from_secs(3)).await;
    }
    bail!(
        "Timed out waiting for Cassandra at {}:{}: {}",
        config.host,
        config.port,
        last_error.unwrap_or_else(|| "unknown error".to_owned())
    );
}

pub fn normalize_type(value: &str) -> String {
    let mut normalized = WHITESPACE_RE.replace_all(value, "").to_ascii_lowercase();
    let user_type_names = target_schema()
        .user_types
        .iter()
        .map(|user_type| user_type.name.as_str())
        .collect::<Vec<_>>();
    let mut previous = String::new();
    while previous != normalized {
        previous = normalized.clone();
        for name in &user_type_names {
            normalized = normalized.replace(&format!("frozen<{name}>"), name);
        }
    }
    normalized
}

fn normalize_identifier(value: &str) -> String {
    value.trim_matches('"').to_ascii_lowercase()
}

#[derive(Debug, DeserializeRow)]
#[allow(dead_code)]
struct KeyspaceRow {
    keyspace_name: String,
}

#[derive(Debug, DeserializeRow)]
struct TypeRow {
    type_name: String,
    field_names: Option<Vec<String>>,
    field_types: Option<Vec<String>>,
}

#[derive(Debug, DeserializeRow)]
struct ColumnRow {
    table_name: String,
    column_name: String,
    #[scylla(rename = "type")]
    column_type: String,
}

#[derive(Debug, DeserializeRow)]
struct IndexRow {
    index_name: Option<String>,
}

pub async fn fetch_existing_schema(session: &Session, keyspace: &str) -> Result<ExistingSchema> {
    let keyspace_rows = session
        .query_unpaged(
            "SELECT keyspace_name FROM system_schema.keyspaces WHERE keyspace_name = ?",
            (keyspace,),
        )
        .await?
        .into_rows_result()?
        .rows::<KeyspaceRow>()?
        .collect::<std::result::Result<Vec<_>, _>>()?;

    let type_rows = session
        .query_unpaged(
            "SELECT type_name, field_names, field_types FROM system_schema.types WHERE keyspace_name = ?",
            (keyspace,),
        )
        .await?
        .into_rows_result()?
        .rows::<TypeRow>()?
        .collect::<std::result::Result<Vec<_>, _>>()?;
    let mut types = BTreeMap::new();
    for row in type_rows {
        let mut fields = BTreeMap::new();
        for (name, field_type) in row
            .field_names
            .unwrap_or_default()
            .into_iter()
            .zip(row.field_types.unwrap_or_default())
        {
            fields.insert(normalize_identifier(&name), field_type);
        }
        types.insert(normalize_identifier(&row.type_name), fields);
    }

    let column_rows = session
        .query_unpaged(
            "SELECT table_name, column_name, type FROM system_schema.columns WHERE keyspace_name = ?",
            (keyspace,),
        )
        .await?
        .into_rows_result()?
        .rows::<ColumnRow>()?
        .collect::<std::result::Result<Vec<_>, _>>()?;
    let mut tables: BTreeMap<String, ExistingTable> = BTreeMap::new();
    for row in column_rows {
        tables
            .entry(normalize_identifier(&row.table_name))
            .or_default()
            .columns
            .insert(normalize_identifier(&row.column_name), row.column_type);
    }

    let index_rows = session
        .query_unpaged(
            "SELECT index_name FROM system_schema.indexes WHERE keyspace_name = ?",
            (keyspace,),
        )
        .await?
        .into_rows_result()?
        .rows::<IndexRow>()?
        .collect::<std::result::Result<Vec<_>, _>>()?;
    let indexes = index_rows
        .into_iter()
        .filter_map(|row| row.index_name)
        .map(|name| normalize_identifier(&name))
        .collect();

    Ok(ExistingSchema {
        keyspace_exists: !keyspace_rows.is_empty(),
        types,
        tables,
        indexes,
    })
}

pub fn render_user_type(user_type: &UserType, keyspace: &str) -> String {
    let fields = user_type
        .fields
        .iter()
        .map(|field| format!("{} {}", field.name, field.field_type))
        .collect::<Vec<_>>()
        .join(",\n    ");
    format!(
        "CREATE TYPE IF NOT EXISTS {keyspace}.{} (\n    {fields}\n)",
        user_type.name
    )
}

pub fn render_table(table: &Table, keyspace: &str) -> String {
    let mut fields = table
        .columns
        .iter()
        .map(|field| format!("{} {}", field.name, field.field_type))
        .collect::<Vec<_>>();
    fields.push(format!("PRIMARY KEY {}", table.primary_key));
    let body = fields.join(",\n    ");
    let mut statement = format!(
        "CREATE TABLE IF NOT EXISTS {keyspace}.{} (\n    {body}\n)",
        table.name
    );
    if !table.options.is_empty() {
        statement.push_str(" WITH ");
        statement.push_str(&table.options);
    }
    statement
}

pub fn render_index(index: &Index, keyspace: &str) -> String {
    format!(
        "CREATE INDEX IF NOT EXISTS {} ON {keyspace}.{} ({})",
        index.name, index.table, index.expression
    )
}

pub fn render_target_schema(keyspace: &str) -> String {
    let schema = target_schema();
    let mut statements = Vec::new();
    for user_type in sorted_user_types() {
        statements.push(statement_with_semicolon(&render_user_type(
            &user_type, keyspace,
        )));
    }
    let mut tables = schema.tables.clone();
    tables.sort_by(|a, b| a.name.cmp(&b.name));
    for table in tables {
        statements.push(statement_with_semicolon(&render_table(&table, keyspace)));
    }
    let mut indexes = schema.indexes.clone();
    indexes.sort_by(|a, b| a.name.cmp(&b.name));
    for index in indexes {
        statements.push(statement_with_semicolon(&render_index(&index, keyspace)));
    }
    format!("{}\n", statements.join("\n\n"))
}

pub fn target_schema_sha256(keyspace: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(render_target_schema(keyspace).as_bytes());
    hex::encode(hasher.finalize())
}

pub fn sorted_user_types() -> Vec<UserType> {
    let schema = target_schema();
    let by_name = schema
        .user_types
        .iter()
        .map(|user_type| (user_type.name.clone(), user_type.clone()))
        .collect::<BTreeMap<_, _>>();
    let mut done = BTreeSet::new();
    let mut result = Vec::new();
    let mut remaining = by_name.clone();
    while !remaining.is_empty() {
        let mut progressed = false;
        for (name, user_type) in remaining.clone() {
            let deps = type_dependencies(&user_type, &by_name.keys().cloned().collect());
            if deps.is_subset(&done) {
                result.push(user_type);
                done.insert(name.clone());
                remaining.remove(&name);
                progressed = true;
            }
        }
        if !progressed {
            result.extend(remaining.into_values());
            break;
        }
    }
    result
}

fn type_dependencies(user_type: &UserType, user_type_names: &BTreeSet<String>) -> BTreeSet<String> {
    let mut deps = BTreeSet::new();
    for field in &user_type.fields {
        for token in TYPE_TOKEN_RE.find_iter(&field.field_type) {
            let token = token.as_str().to_ascii_lowercase();
            if user_type_names.contains(&token) && token != user_type.name {
                deps.insert(token);
            }
        }
    }
    deps
}

pub fn create_diff(existing: &ExistingSchema, keyspace: &str) -> SchemaDiff {
    let schema = target_schema();
    let mut statements = Vec::new();
    let mut warnings = Vec::new();
    let mut errors = Vec::new();
    if !existing.keyspace_exists {
        statements.push(format!(
            "CREATE KEYSPACE IF NOT EXISTS {keyspace} WITH replication = {{'class': 'SimpleStrategy', 'replication_factor': 1}}"
        ));
    }
    for user_type in sorted_user_types() {
        let Some(current_fields) = existing.types.get(&user_type.name) else {
            statements.push(render_user_type(&user_type, keyspace));
            continue;
        };
        for field in &user_type.fields {
            match current_fields.get(&field.name) {
                None => statements.push(format!(
                    "ALTER TYPE {keyspace}.{} ADD IF NOT EXISTS {} {}",
                    user_type.name, field.name, field.field_type
                )),
                Some(current_type)
                    if normalize_type(current_type) != normalize_type(&field.field_type) =>
                {
                    errors.push(format!(
                        "type {}.{} has {current_type:?}, expected {:?}",
                        user_type.name, field.name, field.field_type
                    ));
                }
                Some(_) => {}
            }
        }
    }
    let mut tables = schema.tables.clone();
    tables.sort_by(|a, b| a.name.cmp(&b.name));
    for table in tables {
        let Some(current_table) = existing.tables.get(&table.name) else {
            statements.push(render_table(&table, keyspace));
            continue;
        };
        for field in &table.columns {
            match current_table.columns.get(&field.name) {
                None => statements.push(format!(
                    "ALTER TABLE {keyspace}.{} ADD IF NOT EXISTS {} {}",
                    table.name, field.name, field.field_type
                )),
                Some(current_type)
                    if normalize_type(current_type) != normalize_type(&field.field_type) =>
                {
                    errors.push(format!(
                        "table {}.{} has {current_type:?}, expected {:?}",
                        table.name, field.name, field.field_type
                    ));
                }
                Some(_) => {}
            }
        }
    }
    let mut indexes = schema.indexes.clone();
    indexes.sort_by(|a, b| a.name.cmp(&b.name));
    for index in indexes {
        if !existing.indexes.contains(&index.name.to_ascii_lowercase()) {
            statements.push(render_index(&index, keyspace));
        }
    }
    let target_tables = schema
        .tables
        .iter()
        .map(|table| table.name.clone())
        .collect::<BTreeSet<_>>();
    for table_name in existing.tables.keys() {
        if !target_tables.contains(table_name) && !table_name.starts_with("system_") {
            warnings.push(format!(
                "extra Cassandra table exists outside target schema: {table_name}"
            ));
        }
    }
    SchemaDiff {
        statements,
        warnings,
        errors,
        target_sha256: target_schema_sha256(keyspace),
    }
}

fn statement_with_semicolon(statement: &str) -> String {
    format!("{};", statement.trim().trim_end_matches(';'))
}

pub fn render_diff(diff: &SchemaDiff) -> String {
    let mut lines = vec![
        "-- Generated by fluxer-dev cassandra diff".to_owned(),
        format!("-- Target schema sha256: {}", diff.target_sha256),
        String::new(),
    ];
    if !diff.errors.is_empty() {
        lines.push("-- Diff contains validation errors and should not be applied:".to_owned());
        lines.extend(diff.errors.iter().map(|error| format!("--   {error}")));
        lines.push(String::new());
    }
    if !diff.warnings.is_empty() {
        lines.push("-- Warnings:".to_owned());
        lines.extend(
            diff.warnings
                .iter()
                .map(|warning| format!("--   {warning}")),
        );
        lines.push(String::new());
    }
    if diff.statements.is_empty() {
        lines.push("-- No schema changes required.".to_owned());
    } else {
        lines.extend(
            diff.statements
                .iter()
                .map(|statement| statement_with_semicolon(statement)),
        );
    }
    format!("{}\n", lines.join("\n"))
}

pub fn write_diff_file(diff: &SchemaDiff, path: Option<&Path>) -> Result<PathBuf> {
    std::fs::create_dir_all(DEV_CASSANDRA_DIR.as_path())?;
    let output = path
        .map(Path::to_path_buf)
        .unwrap_or_else(|| DEV_CASSANDRA_DIR.join("last-diff.cql"));
    std::fs::write(&output, render_diff(diff))?;
    std::fs::write(
        DEV_CASSANDRA_DIR.join("target-schema.sha256"),
        format!("{}\n", diff.target_sha256),
    )?;
    Ok(output)
}

pub async fn compute_diff(config: Option<CassandraConfig>) -> Result<SchemaDiff> {
    let config = config.unwrap_or(config_from_env()?);
    wait_for_cassandra(&config, 180).await?;
    let session = connect(&config).await?;
    create_live_diff(&session, &config.keyspace).await
}

async fn create_live_diff(session: &Session, keyspace: &str) -> Result<SchemaDiff> {
    Ok(create_diff(
        &fetch_existing_schema(session, keyspace).await?,
        keyspace,
    ))
}

pub async fn apply_schema(config: Option<CassandraConfig>) -> Result<SchemaDiff> {
    let config = config.unwrap_or(config_from_env()?);
    wait_for_cassandra(&config, 180).await?;
    let session = connect(&config).await?;
    let diff = create_live_diff(&session, &config.keyspace).await?;
    let diff_path = write_diff_file(&diff, None)?;
    if !diff.errors.is_empty() {
        bail!(
            "Cassandra schema has validation errors; wrote {}",
            diff_path.display()
        );
    }
    for warning in &diff.warnings {
        println!("Cassandra schema warning: {warning}");
    }
    if diff.statements.is_empty() {
        println!(
            "Cassandra schema is already up to date; wrote {}",
            diff_path.display()
        );
    } else {
        println!(
            "Applying {} Cassandra schema statement(s); wrote {}",
            diff.statements.len(),
            diff_path.display()
        );
        for statement in &diff.statements {
            session.query_unpaged(statement.as_str(), ()).await?;
        }
    }
    verify_schema_with_session(&session, &config.keyspace).await?;
    Ok(diff)
}

pub async fn verify_schema(
    config: Option<CassandraConfig>,
    keyspace: Option<String>,
) -> Result<()> {
    let config = config.unwrap_or(config_from_env()?);
    let keyspace = keyspace.unwrap_or_else(|| config.keyspace.clone());
    wait_for_cassandra(&config, 180).await?;
    let session = connect(&config).await?;
    verify_schema_with_session(&session, &keyspace).await
}

async fn verify_schema_with_session(session: &Session, keyspace: &str) -> Result<()> {
    let existing = fetch_existing_schema(session, keyspace).await?;
    let diff = create_diff(&existing, keyspace);
    let unapplied = diff.statements.clone();
    if !diff.errors.is_empty() || !unapplied.is_empty() {
        write_diff_file(&diff, None)?;
        let mut problems = diff.errors;
        problems.extend(
            unapplied
                .into_iter()
                .take(10)
                .map(|statement| format!("still needs: {statement}")),
        );
        bail!(
            "Cassandra schema verification failed:\n{}",
            problems
                .into_iter()
                .map(|item| format!("  - {item}"))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
    println!(
        "Cassandra schema verified: {} types, {} tables, {} indexes, target {}",
        target_schema().user_types.len(),
        target_schema().tables.len(),
        target_schema().indexes.len(),
        &diff.target_sha256[..12],
    );
    Ok(())
}
