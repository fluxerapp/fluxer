// SPDX-License-Identifier: AGPL-3.0-or-later

use deadpool_postgres::Pool;
use fluxer_svc::postgres::{PostgresConfig, connect, ensure_kv_schema};
use std::net::TcpListener;
use std::process::{Command, Stdio};
use std::time::Duration;

const KV_TABLE_WARMUP: &str = "svc_schema_warmup";
const KV_TABLE_BOOT: &str = "svc_schema_boot";
const KV_TABLE_HELPER: &str = "svc_schema_helper";
const KV_TABLE_RACE: &str = "svc_schema_race";
const KV_TABLE_BACKFILL: &str = "svc_schema_backfill";
const SEPARATOR: char = '\u{1f}';

fn docker_available() -> bool {
    Command::new("docker")
        .arg("version")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

fn docker(args: &[&str]) -> anyhow::Result<()> {
    let output = Command::new("docker").args(args).output()?;
    if output.status.success() {
        return Ok(());
    }
    anyhow::bail!(
        "docker {} failed: {}",
        args.join(" "),
        String::from_utf8_lossy(&output.stderr).trim().to_owned()
    )
}

fn free_port() -> anyhow::Result<u16> {
    let listener = TcpListener::bind("127.0.0.1:0")?;
    Ok(listener.local_addr()?.port())
}

fn postgres_config(port: u16, kv_table: &str, max_connections: usize) -> PostgresConfig {
    PostgresConfig {
        url: None,
        host: "127.0.0.1".to_owned(),
        port,
        database: "fluxer".to_owned(),
        username: "fluxer".to_owned(),
        password: Some("fluxer".to_owned()),
        ssl: false,
        ssl_ca: None,
        max_connections,
        kv_table: kv_table.to_owned(),
        prepared_statements: true,
    }
}

fn start_postgres(container: &str, port: u16) -> anyhow::Result<()> {
    docker(&[
        "run",
        "-d",
        "--name",
        container,
        "-e",
        "POSTGRES_USER=fluxer",
        "-e",
        "POSTGRES_PASSWORD=fluxer",
        "-e",
        "POSTGRES_DB=fluxer",
        "-p",
        &format!("127.0.0.1:{port}:5432"),
        "postgres:16-alpine",
        "-c",
        "fsync=off",
        "-c",
        "synchronous_commit=off",
    ])
}

async fn wait_for_postgres(container: &str, port: u16) -> anyhow::Result<()> {
    for _ in 0..180 {
        tokio::time::sleep(Duration::from_millis(500)).await;
        if docker(&[
            "exec",
            container,
            "pg_isready",
            "-U",
            "fluxer",
            "-d",
            "fluxer",
        ])
        .is_err()
        {
            continue;
        }
        if let Ok(pool) = connect(&postgres_config(port, KV_TABLE_WARMUP, 1)).await {
            pool.close();
            return Ok(());
        }
    }
    anyhow::bail!("postgres never came up")
}

fn unlocked_create_table_sql(kv_table: &str) -> String {
    format!(
        r#"
CREATE TABLE IF NOT EXISTS "{kv_table}" (
    table_name text NOT NULL,
    partition_key text COLLATE "C" NOT NULL,
    row_key text COLLATE "C" NOT NULL,
    row_data jsonb NOT NULL,
    expires_at timestamptz,
    updated_at timestamptz NOT NULL DEFAULT now(),
    PRIMARY KEY (table_name, row_key)
);
"#
    )
}

fn legacy_row_key(id: &str) -> String {
    format!("\"c\"{SEPARATOR}\"b\"{SEPARATOR}\"{id}\"")
}

fn migrated_partition_key() -> String {
    format!("\"c\"{SEPARATOR}\"b\"")
}

async fn insert_legacy_message(pool: &Pool, kv_table: &str, id: &str) -> anyhow::Result<()> {
    let client = pool.get().await?;
    client
        .execute(
            &format!(
                "INSERT INTO \"{kv_table}\" (table_name, partition_key, row_key, row_data) VALUES ('messages', $1, $1, '{{}}'::jsonb)"
            ),
            &[&legacy_row_key(id)],
        )
        .await?;
    Ok(())
}

async fn partition_key_of(pool: &Pool, kv_table: &str, id: &str) -> anyhow::Result<Option<String>> {
    let client = pool.get().await?;
    let row = client
        .query_opt(
            &format!(
                "SELECT partition_key FROM \"{kv_table}\" WHERE table_name = 'messages' AND row_key = $1"
            ),
            &[&legacy_row_key(id)],
        )
        .await?;
    Ok(row.map(|row| row.get::<_, String>("partition_key")))
}

async fn migration_marker_count(pool: &Pool, kv_table: &str) -> anyhow::Result<i64> {
    let client = pool.get().await?;
    let row = client
        .query_one(
            &format!(
                "SELECT count(*) AS n FROM \"{kv_table}\" WHERE table_name = '__fluxer_schema_migrations' AND row_key = 'messages_partition_key_v1'"
            ),
            &[],
        )
        .await?;
    Ok(row.get::<_, i64>("n"))
}

#[tokio::test(flavor = "multi_thread")]
async fn concurrent_boots_all_ensure_the_kv_schema() -> anyhow::Result<()> {
    if !docker_available() {
        eprintln!("skipping: docker is not available");
        return Ok(());
    }
    let port = free_port()?;
    let container = format!("fluxer-kvschema-boot-{}-{port}", std::process::id());
    start_postgres(&container, port)?;
    let result = async {
        wait_for_postgres(&container, port).await?;
        let mut handles = Vec::new();
        for _ in 0..4 {
            handles.push(tokio::spawn(async move {
                connect(&postgres_config(port, KV_TABLE_BOOT, 2))
                    .await
                    .map(|pool| pool.close())
            }));
        }
        let mut outcomes = Vec::new();
        for handle in handles {
            outcomes.push(match handle.await? {
                Ok(()) => "ok".to_owned(),
                Err(err) => format!("{err:#}"),
            });
        }
        anyhow::Ok(outcomes)
    }
    .await;
    let _ = docker(&["rm", "-f", &container]);
    let outcomes = result?;
    assert_eq!(outcomes, vec!["ok".to_owned(); 4], "{outcomes:#?}");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn boot_survives_a_peer_that_creates_the_table_without_the_schema_lock() -> anyhow::Result<()>
{
    if !docker_available() {
        eprintln!("skipping: docker is not available");
        return Ok(());
    }
    let port = free_port()?;
    let container = format!("fluxer-kvschema-race-{}-{port}", std::process::id());
    start_postgres(&container, port)?;
    let result = async {
        wait_for_postgres(&container, port).await?;
        let helper = connect(&postgres_config(port, KV_TABLE_HELPER, 4)).await?;
        let mut peer = helper.get().await?;
        let peer_transaction = peer.transaction().await?;
        peer_transaction
            .batch_execute(&unlocked_create_table_sql(KV_TABLE_RACE))
            .await?;
        let booting = tokio::spawn(async move {
            connect(&postgres_config(port, KV_TABLE_RACE, 2))
                .await
                .map(|pool| pool.close())
        });
        tokio::time::sleep(Duration::from_millis(500)).await;
        peer_transaction.commit().await?;
        let booted = match booting.await? {
            Ok(()) => "ok".to_owned(),
            Err(err) => format!("{err:#}"),
        };
        drop(peer);
        helper.close();
        anyhow::Ok(booted)
    }
    .await;
    let _ = docker(&["rm", "-f", &container]);
    let booted = result?;
    assert_eq!(booted, "ok");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn backfills_message_partition_keys_once_and_never_scans_again() -> anyhow::Result<()> {
    if !docker_available() {
        eprintln!("skipping: docker is not available");
        return Ok(());
    }
    let port = free_port()?;
    let container = format!("fluxer-kvschema-backfill-{}-{port}", std::process::id());
    start_postgres(&container, port)?;
    let result = async {
        wait_for_postgres(&container, port).await?;
        let helper = connect(&postgres_config(port, KV_TABLE_HELPER, 4)).await?;
        helper
            .get()
            .await?
            .batch_execute(&unlocked_create_table_sql(KV_TABLE_BACKFILL))
            .await?;
        insert_legacy_message(&helper, KV_TABLE_BACKFILL, "m1").await?;
        ensure_kv_schema(&helper, KV_TABLE_BACKFILL).await?;
        let first = partition_key_of(&helper, KV_TABLE_BACKFILL, "m1").await?;
        let markers = migration_marker_count(&helper, KV_TABLE_BACKFILL).await?;
        insert_legacy_message(&helper, KV_TABLE_BACKFILL, "m2").await?;
        ensure_kv_schema(&helper, KV_TABLE_BACKFILL).await?;
        let untouched = partition_key_of(&helper, KV_TABLE_BACKFILL, "m2").await?;
        let still_migrated = partition_key_of(&helper, KV_TABLE_BACKFILL, "m1").await?;
        helper.close();
        anyhow::Ok((first, markers, untouched, still_migrated))
    }
    .await;
    let _ = docker(&["rm", "-f", &container]);
    let (first, markers, untouched, still_migrated) = result?;
    assert_eq!(first, Some(migrated_partition_key()));
    assert_eq!(markers, 1);
    assert_eq!(untouched, Some(legacy_row_key("m2")));
    assert_eq!(still_migrated, Some(migrated_partition_key()));
    Ok(())
}
