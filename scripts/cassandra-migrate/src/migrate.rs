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

use anyhow::{Result, anyhow};
use chrono::{DateTime, Utc};
use regex::Regex;
use scylla::Session;
use scylla::SessionBuilder;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::time::{Duration, Instant};

const MIGRATION_TABLE: &str = "schema_migrations";

lazy_static::lazy_static! {
    static ref MIGRATION_KEYSPACE: String = env::var("CASSANDRA_KEYSPACE").unwrap_or_else(|_| "fluxer".to_string());
}

fn migration_keyspace() -> &'static str {
    MIGRATION_KEYSPACE.as_str()
}

const MIGRATION_TEMPLATE: &str = "";

struct ForbiddenPattern {
    pattern: Regex,
    message: &'static str,
}

lazy_static::lazy_static! {
    static ref FORBIDDEN_PATTERNS: Vec<ForbiddenPattern> = vec![
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bCREATE\s+INDEX\b").unwrap(),
            message: "Secondary indexes are forbidden (CREATE INDEX)",
        },
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bCREATE\s+CUSTOM\s+INDEX\b").unwrap(),
            message: "Custom indexes are forbidden (CREATE CUSTOM INDEX)",
        },
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bCREATE\s+MATERIALIZED\s+VIEW\b").unwrap(),
            message: "Materialized views are forbidden (CREATE MATERIALIZED VIEW)",
        },
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bDROP\s+TABLE\b").unwrap(),
            message: "DROP TABLE is forbidden",
        },
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bDROP\s+KEYSPACE\b").unwrap(),
            message: "DROP KEYSPACE is forbidden",
        },
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bDROP\s+TYPE\b").unwrap(),
            message: "DROP TYPE is forbidden",
        },
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bDROP\s+INDEX\b").unwrap(),
            message: "DROP INDEX is forbidden",
        },
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bDROP\s+MATERIALIZED\s+VIEW\b").unwrap(),
            message: "DROP MATERIALIZED VIEW is forbidden",
        },
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bDROP\s+COLUMN\b").unwrap(),
            message: "DROP COLUMN is forbidden (use ALTER TABLE ... DROP ...)",
        },
        ForbiddenPattern {
            pattern: Regex::new(r"(?i)\bTRUNCATE\b").unwrap(),
            message: "TRUNCATE is forbidden",
        },
    ];
}

pub async fn test_connection(host: &str, port: u16, username: &str, password: &str) -> Result<()> {
    println!("Testing Cassandra connection to {host}:{port}...");

    let session = create_session(host, port, username, password).await?;

    let result = session
        .query("SELECT release_version FROM system.local", &[])
        .await?;

    if let Some(rows) = result.rows
        && let Some(row) = rows.into_iter().next()
        && let Some(cql_value) = &row.columns[0]
        && let Some(version) = cql_value.as_text()
    {
        println!("✓ Connection successful - Cassandra version: {version}");
        return Ok(());
    }

    println!("✓ Connection successful");

    Ok(())
}

pub fn create_migration(name: &str) -> Result<()> {
    let sanitized = sanitize_name(name);
    if sanitized.is_empty() {
        return Err(anyhow!("Invalid migration name: {name}"));
    }

    let timestamp = Utc::now().format("%Y%m%d%H%M%S");
    let filename = format!("{timestamp}_{sanitized}.cql");

    let filepath = get_migration_path(&filename);

    if filepath.exists() {
        return Err(anyhow!("Migration file already exists: {filename}"));
    }

    fs::write(&filepath, MIGRATION_TEMPLATE)?;

    println!("✓ Created migration: {filename}");
    println!("  Path: {}", filepath.display());
    Ok(())
}

pub fn check_migrations() -> Result<()> {
    let migrations = get_migration_files()?;

    if migrations.is_empty() {
        println!("No migration files found");
        return Ok(());
    }

    println!("Checking {} migration file(s)...\n", migrations.len());

    let mut errors = Vec::new();
    let mut valid_count = 0;

    for migration in &migrations {
        let content = fs::read_to_string(get_migration_path(migration))?;
        let file_errors = validate_migration_content(migration, &content);

        if file_errors.is_empty() {
            valid_count += 1;
            println!("✓ {migration}");
        } else {
            errors.extend(file_errors);
        }
    }

    if !errors.is_empty() {
        println!("\nValidation errors:");
        for error in &errors {
            println!("✗ {error}");
        }
        return Err(anyhow!("Validation failed with {} error(s)", errors.len()));
    }

    println!("\n✓ All {valid_count} migration(s) are valid!");
    Ok(())
}

pub async fn run_migrations(host: &str, port: u16, username: &str, password: &str) -> Result<()> {
    println!("Starting Cassandra migration process...");
    println!("Host: {host}, Port: {port}");

    let session = create_session(host, port, username, password).await?;

    setup_migration_infrastructure(&session).await?;

    let migrations = get_migration_files()?;
    let applied = get_applied_migrations(&session).await?;

    if migrations.is_empty() {
        println!("No migration files found");
        return Ok(());
    }

    let mut pending = Vec::new();
    let mut skipped = Vec::new();

    for migration in migrations {
        if !applied.contains_key(&migration) {
            if has_skip_ci(&migration)? {
                skipped.push(migration);
            } else {
                pending.push(migration);
            }
        }
    }

    if !skipped.is_empty() {
        println!(
            "Found {} migration(s) with '-- skip ci' annotation:",
            skipped.len()
        );
        for migration in &skipped {
            println!("  - {migration}");
        }
        println!("\nAuto-acknowledging skipped migrations...");

        for migration in &skipped {
            auto_acknowledge_migration(&session, migration).await?;
            println!("  ✓ Acknowledged: {migration}");
        }
        println!();
    }

    if pending.is_empty() {
        println!("✓ No pending migrations");
        return Ok(());
    }

    println!("Found {} pending migration(s) to apply:", pending.len());
    for migration in &pending {
        println!("  - {migration}");
    }
    println!();

    let pending_count = pending.len();
    for migration in pending {
        apply_migration(&session, &migration).await?;
    }

    println!("✓ Successfully applied {pending_count} migration(s)");
    Ok(())
}

pub async fn show_status(host: &str, port: u16, username: &str, password: &str) -> Result<()> {
    let session = create_session(host, port, username, password).await?;

    let migrations = get_migration_files()?;
    let applied = get_applied_migrations(&session).await?;

    println!("Migration Status");
    println!("================\n");
    println!("Total migrations: {}", migrations.len());
    println!("Applied: {}", applied.len());
    println!("Pending: {}\n", migrations.len() - applied.len());

    if !migrations.is_empty() {
        println!("Migrations:");
        for migration in migrations {
            let status = if applied.contains_key(&migration) {
                "[✓]"
            } else {
                "[ ]"
            };

            let suffix = if has_skip_ci(&migration)? {
                " (skip ci)"
            } else {
                ""
            };
            println!("  {status} {migration}{suffix}");
        }
    }

    Ok(())
}

pub async fn acknowledge_migration(
    host: &str,
    port: u16,
    username: &str,
    password: &str,
    filename: &str,
) -> Result<()> {
    let session = create_session(host, port, username, password).await?;

    let applied = get_applied_migrations(&session).await?;
    if applied.contains_key(filename) {
        return Err(anyhow!("Migration {filename} is already applied"));
    }

    let content = fs::read_to_string(get_migration_path(filename))?;
    let checksum = calculate_checksum(&content);

    session
        .query(
            format!(
                "INSERT INTO {}.{} (filename, applied_at, checksum) VALUES (?, ?, ?)",
                migration_keyspace(),
                MIGRATION_TABLE
            ),
            (filename, Utc::now(), checksum),
        )
        .await?;

    println!("✓ Migration acknowledged: {filename}");
    Ok(())
}

async fn create_session(host: &str, port: u16, username: &str, password: &str) -> Result<Session> {
    let max_retries = 5;
    let retry_delay = Duration::from_secs(10);

    let mut last_error = None;

    for attempt in 1..=max_retries {
        if attempt > 1 {
            println!("Retrying connection (attempt {attempt}/{max_retries})...");
        }

        let result = SessionBuilder::new()
            .known_node(format!("{host}:{port}"))
            .user(username, password)
            .connection_timeout(Duration::from_secs(60))
            .build()
            .await;

        match result {
            Ok(session) => {
                let _ = session
                    .query(
                        format!(
                            "CREATE KEYSPACE IF NOT EXISTS {} WITH REPLICATION = {{'class': 'SimpleStrategy', 'replication_factor': 1}}",
                            migration_keyspace()
                        ),
                        &[],
                    )
                    .await;

                return Ok(session);
            }
            Err(e) => {
                last_error = Some(e);

                if attempt < max_retries {
                    tokio::time::sleep(retry_delay).await;
                }
            }
        }
    }

    Err(anyhow!(
        "Failed to connect to Cassandra after {} attempts: {}",
        max_retries,
        last_error.unwrap()
    ))
}

async fn setup_migration_infrastructure(session: &Session) -> Result<()> {
    session
        .query(
            format!(
                "CREATE TABLE IF NOT EXISTS {}.{} (filename text PRIMARY KEY, applied_at timestamp, checksum text)",
                migration_keyspace(),
                MIGRATION_TABLE
            ),
            &[],
        )
        .await
        .map_err(|e| anyhow!("Failed to create migrations table: {e}"))?;

    Ok(())
}

async fn apply_migration(session: &Session, filename: &str) -> Result<()> {
    println!("Applying migration: {filename}");

    let content = fs::read_to_string(get_migration_path(filename))?;
    let statements = parse_statements(&content);

    if statements.is_empty() {
        return Err(anyhow!("No valid statements found in migration"));
    }

    println!("  Executing {} statement(s)...", statements.len());

    for (i, statement) in statements.iter().enumerate() {
        println!("    [{}/{}] Executing...", i + 1, statements.len());

        session
            .query(statement.as_str(), &[])
            .await
            .map_err(|e| anyhow!("Statement {} failed: {}\n{}", i + 1, e, statement))?;
    }

    let checksum = calculate_checksum(&content);
    session
        .query(
            format!(
                "INSERT INTO {}.{} (filename, applied_at, checksum) VALUES (?, ?, ?)",
                migration_keyspace(),
                MIGRATION_TABLE
            ),
            (filename, Utc::now(), checksum),
        )
        .await
        .map_err(|e| anyhow!("Failed to record migration: {e}"))?;

    println!("  ✓ Migration applied successfully");
    Ok(())
}

async fn auto_acknowledge_migration(session: &Session, filename: &str) -> Result<()> {
    let content = fs::read_to_string(get_migration_path(filename))?;
    let checksum = calculate_checksum(&content);

    session
        .query(
            format!(
                "INSERT INTO {}.{} (filename, applied_at, checksum) VALUES (?, ?, ?)",
                migration_keyspace(),
                MIGRATION_TABLE
            ),
            (filename, Utc::now(), checksum),
        )
        .await
        .map_err(|e| anyhow!("Failed to record migration: {e}"))?;

    Ok(())
}

async fn get_applied_migrations(session: &Session) -> Result<HashMap<String, DateTime<Utc>>> {
    let mut applied = HashMap::new();

    let result = session
        .query(
            format!(
                "SELECT filename, applied_at FROM {}.{}",
                migration_keyspace(),
                MIGRATION_TABLE
            ),
            &[],
        )
        .await?;

    if let Some(rows) = result.rows {
        for row in rows {
            if let Some(filename_cql) = &row.columns[0]
                && let Some(filename) = filename_cql.as_text()
                && let Some(applied_at_cql) = &row.columns[1]
            {
                use scylla::frame::response::result::CqlValue;
                if let CqlValue::Timestamp(duration) = applied_at_cql {
                    let millis = duration.num_milliseconds();
                    let applied_at: DateTime<Utc> = DateTime::from_timestamp(
                        millis / 1000,
                        ((millis % 1000) * 1_000_000) as u32,
                    )
                    .unwrap_or(Utc::now());
                    applied.insert(filename.to_string(), applied_at);
                }
            }
        }
    }

    Ok(applied)
}

fn get_migration_files() -> Result<Vec<String>> {
    let migrations_dir = get_migrations_dir();
    let mut migrations = Vec::new();

    for entry in fs::read_dir(migrations_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_file()
            && path.extension().is_some_and(|ext| ext == "cql")
            && let Some(filename) = path.file_name().and_then(|n| n.to_str())
        {
            migrations.push(filename.to_string());
        }
    }

    migrations.sort();
    Ok(migrations)
}

fn get_migrations_dir() -> PathBuf {
    PathBuf::from("fluxer_devops/cassandra/migrations")
}

fn get_migration_path(filename: &str) -> PathBuf {
    get_migrations_dir().join(filename)
}

fn has_skip_ci(filename: &str) -> Result<bool> {
    let content = fs::read_to_string(get_migration_path(filename))?;
    let lines: Vec<&str> = content.lines().take(10).collect();

    for line in lines {
        let line = line.trim().to_lowercase();
        if line.contains("-- skip ci") || line.contains("--skip ci") {
            return Ok(true);
        }
    }

    Ok(false)
}

fn parse_statements(content: &str) -> Vec<String> {
    let mut statements = Vec::new();
    let mut current_statement = String::new();

    for line in content.lines() {
        let line = if let Some(idx) = line.find("--") {
            &line[..idx]
        } else {
            line
        };

        let trimmed = line.trim();

        if trimmed.is_empty() {
            continue;
        }

        current_statement.push_str(trimmed);
        current_statement.push(' ');

        if trimmed.ends_with(';') {
            let statement = current_statement.trim().to_string();
            if !statement.is_empty() {
                statements.push(statement);
            }
            current_statement.clear();
        }
    }

    if !current_statement.trim().is_empty() {
        statements.push(current_statement.trim().to_string());
    }

    statements
}

fn calculate_checksum(content: &str) -> String {
    format!("{:x}", md5::compute(content.as_bytes()))
}

fn sanitize_name(name: &str) -> String {
    let mut name = name.replace(' ', "_");
    name = name.replace('-', "_");
    name = name.to_lowercase();

    let result: String = name
        .chars()
        .filter(|c| c.is_ascii_alphanumeric() || *c == '_')
        .collect();

    let mut result = result.replace("__", "_");
    while result.contains("__") {
        result = result.replace("__", "_");
    }

    result.trim_matches('_').to_string()
}

fn validate_migration_content(filename: &str, content: &str) -> Vec<String> {
    let mut errors = Vec::new();

    let clean_content = remove_comments(content);

    for forbidden in FORBIDDEN_PATTERNS.iter() {
        if forbidden.pattern.is_match(&clean_content) {
            errors.push(format!("  {}: {}", filename, forbidden.message));
        }
    }

    if clean_content.trim().is_empty() {
        errors.push(format!("  {filename}: migration file is empty"));
    }

    errors
}

fn remove_comments(content: &str) -> String {
    content
        .lines()
        .map(|line| {
            if let Some(idx) = line.find("--") {
                &line[..idx]
            } else {
                line
            }
        })
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

pub async fn debug_connection(host: &str, port: u16, username: &str, password: &str) -> Result<()> {
    println!("=== Cassandra Connection Debug ===");
    println!("Host: {host}:{port}");
    println!("Username: {username}");

    println!("\n[1/3] Testing TCP connectivity...");
    let start = Instant::now();
    match tokio::time::timeout(
        Duration::from_secs(5),
        tokio::net::TcpStream::connect(format!("{host}:{port}")),
    )
    .await
    {
        Ok(Ok(_)) => {
            println!(
                "  ✓ TCP connection successful ({:.2}s)",
                start.elapsed().as_secs_f64()
            );
        }
        Ok(Err(e)) => {
            println!("  ✗ TCP connection failed: {e}");
            return Err(anyhow!("TCP connection failed: {e}"));
        }
        Err(_) => {
            println!("  ✗ TCP connection timed out");
            return Err(anyhow!("TCP connection timed out"));
        }
    }

    println!("\n[2/3] Creating Cassandra session...");
    let start = Instant::now();
    let session = match tokio::time::timeout(
        Duration::from_secs(30),
        SessionBuilder::new()
            .known_node(format!("{host}:{port}"))
            .user(username, password)
            .connection_timeout(Duration::from_secs(20))
            .build(),
    )
    .await
    {
        Ok(Ok(session)) => {
            println!(
                "  ✓ Session created ({:.2}s)",
                start.elapsed().as_secs_f64()
            );
            session
        }
        Ok(Err(e)) => {
            println!("  ✗ Session creation failed: {e}");
            return Err(anyhow!("Failed to create session: {e}"));
        }
        Err(_) => {
            println!("  ✗ Session creation timed out");
            return Err(anyhow!("Session creation timed out"));
        }
    };

    println!("\n[3/3] Testing queries...");
    let start = Instant::now();
    let result = session
        .query("SELECT release_version FROM system.local", &[])
        .await?;

    if let Some(rows) = result.rows
        && let Some(row) = rows.into_iter().next()
        && let Some(version_col) = &row.columns[0]
        && let Some(version) = version_col.as_text()
    {
        println!(
            "  ✓ Cassandra version: {} ({:.2}s)",
            version,
            start.elapsed().as_secs_f64()
        );
    } else {
        println!(
            "  ✓ Query successful ({:.2}s)",
            start.elapsed().as_secs_f64()
        );
    }

    println!("\n✓ All debug checks passed");
    Ok(())
}
