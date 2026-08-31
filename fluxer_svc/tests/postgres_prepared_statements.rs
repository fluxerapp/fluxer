// SPDX-License-Identifier: AGPL-3.0-or-later

use deadpool_postgres::Pool;
use fluxer_svc::postgres::{BigIntBound, KvClient, PostgresConfig, connect};
use serde_json::{Value, json};
use std::net::TcpListener;
use std::process::{Command, Stdio};
use std::time::Duration;
use tokio_postgres::types::Type;

const KV_TABLE: &str = "svc_stmt_names";
const KV_TABLE_POOLED: &str = "svc_pooled_stmt_names";
const TABLE_NAME: &str = "messages";

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

fn executed_statements(container: &str, kv_table: &str) -> anyhow::Result<Vec<String>> {
    let output = Command::new("docker").args(["logs", container]).output()?;
    let needle = format!("\"{kv_table}\"");
    Ok(String::from_utf8_lossy(&output.stderr)
        .lines()
        .chain(String::from_utf8_lossy(&output.stdout).lines())
        .filter(|line| line.contains("execute ") && line.contains(&needle))
        .map(str::to_owned)
        .collect())
}

fn free_port() -> anyhow::Result<u16> {
    let listener = TcpListener::bind("127.0.0.1:0")?;
    Ok(listener.local_addr()?.port())
}

fn postgres_config(port: u16, kv_table: &str, prepared_statements: bool) -> PostgresConfig {
    PostgresConfig {
        url: None,
        host: "127.0.0.1".to_owned(),
        port,
        database: "fluxer".to_owned(),
        username: "fluxer".to_owned(),
        password: Some("fluxer".to_owned()),
        ssl: false,
        ssl_ca: None,
        max_connections: 1,
        kv_table: kv_table.to_owned(),
        prepared_statements,
    }
}

fn tagged_bigint(value: i64) -> Value {
    json!({"__fluxer_type": "bigint", "value": value.to_string()})
}

fn seed_rows() -> Vec<(&'static str, &'static str, Value)> {
    vec![
        (
            "p0",
            "r1",
            json!({"message_id": tagged_bigint(10), "user_id": tagged_bigint(1010), "payload": "one"}),
        ),
        (
            "p0",
            "r2",
            json!({"message_id": tagged_bigint(20), "user_id": tagged_bigint(1020), "payload": "two"}),
        ),
        (
            "p0",
            "r3",
            json!({"message_id": tagged_bigint(30), "user_id": tagged_bigint(1030), "payload": "three"}),
        ),
        (
            "p1",
            "z9",
            json!({"message_id": tagged_bigint(40), "user_id": tagged_bigint(1040), "payload": "four"}),
        ),
    ]
}

async fn seed(kv: &KvClient, kv_table: &str) -> anyhow::Result<()> {
    let sql = format!(
        "INSERT INTO \"{kv_table}\" (table_name, partition_key, row_key, row_data) VALUES ($1, $2, $3, $4)"
    );
    for (partition_key, row_key, row_data) in seed_rows() {
        kv.query(
            &sql,
            &[
                (&TABLE_NAME, Type::TEXT),
                (&partition_key, Type::TEXT),
                (&row_key, Type::TEXT),
                (&row_data, Type::JSONB),
            ],
        )
        .await?;
    }
    Ok(())
}

fn render(rows: Vec<(String, Value)>) -> String {
    rows.into_iter()
        .map(|(row_key, row_data)| format!("{row_key}={row_data}"))
        .collect::<Vec<_>>()
        .join("|")
}

fn render_sorted(mut rows: Vec<(String, Value)>) -> String {
    rows.sort_by(|left, right| left.0.cmp(&right.0));
    render(rows)
}

async fn exercise_kv_shapes(kv: &KvClient) -> anyhow::Result<Vec<String>> {
    let mut observations = Vec::new();
    observations.push(format!("{:?}", kv.get_row(TABLE_NAME, "r1").await?));
    observations.push(format!("{:?}", kv.get_row(TABLE_NAME, "missing").await?));
    observations.push(render_sorted(
        kv.get_rows(TABLE_NAME, &["r1".to_owned(), "r3".to_owned()])
            .await?,
    ));
    observations.push(render_sorted(kv.get_rows(TABLE_NAME, &[]).await?));
    observations.push(render_sorted(
        kv.get_partition_rows(TABLE_NAME, "p0").await?,
    ));
    observations.push(render_sorted(
        kv.get_row_key_prefix_rows(TABLE_NAME, "r").await?,
    ));
    observations.push(render(
        kv.get_partition_rows_by_bigint_field(
            TABLE_NAME,
            "p0",
            "message_id",
            Some(BigIntBound::LessThan(30)),
            true,
            10,
        )
        .await?,
    ));
    observations.push(render(
        kv.get_partition_rows_by_bigint_field(
            TABLE_NAME,
            "p0",
            "message_id",
            Some(BigIntBound::GreaterThan(10)),
            false,
            10,
        )
        .await?,
    ));
    observations.push(render(
        kv.get_partition_rows_by_bigint_field(TABLE_NAME, "p0", "message_id", None, true, 2)
            .await?,
    ));
    observations.push(render(
        kv.get_partition_rows_by_bigint_field(TABLE_NAME, "p0", "user_id", None, false, 10)
            .await?,
    ));
    observations.push(render(
        kv.get_partition_rows_by_bigint_field(TABLE_NAME, "p0", "message_id", None, true, 0)
            .await?,
    ));
    observations.push(render_sorted(
        kv.get_partition_rows_by_bigint_field_values(TABLE_NAME, "p0", "message_id", &[10, 30])
            .await?,
    ));
    observations.push(render_sorted(
        kv.get_partition_rows_by_bigint_field_values(TABLE_NAME, "p0", "message_id", &[])
            .await?,
    ));
    kv.delete_row(TABLE_NAME, "r2").await?;
    observations.push(render_sorted(
        kv.get_partition_rows(TABLE_NAME, "p0").await?,
    ));
    Ok(observations)
}

struct KvRun {
    observations: Vec<String>,
    prepared: Vec<String>,
    after_session_reset: String,
}

async fn prepared_statement_texts(pool: &Pool) -> anyhow::Result<Vec<String>> {
    let client = pool.get().await?;
    let rows = client
        .query_typed(
            "SELECT statement FROM pg_prepared_statements ORDER BY statement",
            &[],
        )
        .await?;
    Ok(rows
        .iter()
        .map(|row| row.get::<_, String>("statement"))
        .collect())
}

async fn discard_session_state(pool: &Pool) -> anyhow::Result<()> {
    let client = pool.get().await?;
    client.simple_query("DISCARD ALL").await?;
    Ok(())
}

async fn run_kv_shapes(
    port: u16,
    kv_table: &str,
    prepared_statements: bool,
) -> anyhow::Result<KvRun> {
    let config = postgres_config(port, kv_table, prepared_statements);
    let pool = connect(&config).await?;
    let kv = KvClient::new(pool.clone(), &config)?;
    seed(&kv, kv_table).await?;
    let observations = exercise_kv_shapes(&kv).await?;
    let prepared = prepared_statement_texts(&pool).await?;
    discard_session_state(&pool).await?;
    let after_session_reset = match kv.get_row(TABLE_NAME, "r1").await {
        Ok(row) => format!("ok:{row:?}"),
        Err(err) => format!("err:{err:#}"),
    };
    pool.close();
    Ok(KvRun {
        observations,
        prepared,
        after_session_reset,
    })
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
        let config = postgres_config(port, KV_TABLE, true);
        if let Ok(pool) = connect(&config).await {
            pool.close();
            return Ok(());
        }
    }
    anyhow::bail!("postgres never came up")
}

#[tokio::test(flavor = "multi_thread")]
async fn drops_server_side_prepared_statements_when_disabled() -> anyhow::Result<()> {
    if !docker_available() {
        eprintln!("skipping: docker is not available");
        return Ok(());
    }
    let port = free_port()?;
    let container = format!("fluxer-svcstmt-{}-{port}", std::process::id());
    docker(&[
        "run",
        "-d",
        "--name",
        &container,
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
        "log_statement=all",
    ])?;
    let result = async {
        wait_for_postgres(&container, port).await?;
        let named = run_kv_shapes(port, KV_TABLE, true).await?;
        let unnamed = run_kv_shapes(port, KV_TABLE_POOLED, false).await?;
        let named_executions = executed_statements(&container, KV_TABLE)?;
        let unnamed_executions = executed_statements(&container, KV_TABLE_POOLED)?;
        anyhow::Ok((named, unnamed, named_executions, unnamed_executions))
    }
    .await;
    let _ = docker(&["rm", "-f", &container]);
    let (named, unnamed, named_executions, unnamed_executions) = result?;

    assert_eq!(9, named.prepared.len(), "{:#?}", named.prepared);
    assert!(named.prepared.iter().all(|sql| sql.contains(KV_TABLE)));
    assert!(
        named.after_session_reset.contains("prepared statement"),
        "{}",
        named.after_session_reset
    );
    assert!(
        named_executions
            .iter()
            .any(|line| !line.contains("execute <unnamed>:")),
        "{named_executions:#?}"
    );

    assert!(unnamed.prepared.is_empty(), "{:#?}", unnamed.prepared);
    assert_eq!(
        format!("ok:{}", unnamed.observations[0]),
        unnamed.after_session_reset
    );
    assert!(!unnamed_executions.is_empty());
    assert!(
        unnamed_executions
            .iter()
            .all(|line| line.contains("execute <unnamed>:")),
        "{unnamed_executions:#?}"
    );

    assert_eq!(named.observations, unnamed.observations);
    Ok(())
}
