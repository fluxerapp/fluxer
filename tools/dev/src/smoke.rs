// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::cassandra::{apply_schema, config_from_env, verify_schema};
use crate::manifest::{API_PORT, DEV_PROXY_PORT, LOOPBACK_HOST};
use crate::proc::{RunOptions, merged_env, run_command, wait_http, wait_tcp};
use anyhow::{Context, Result, bail};
use futures_util::StreamExt;
use reqwest::header::{CONTENT_TYPE, HeaderMap, HeaderValue};
use serde_json::json;
use std::env;
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::{Duration, Instant};
use tokio::time::sleep;
use tokio_tungstenite::{connect_async, tungstenite::Message};
use url::Url;

pub const S3_BUCKETS: &[&str] = &[
    "fluxer",
    "fluxer-uploads",
    "fluxer-downloads",
    "fluxer-reports",
    "fluxer-harvests",
    "fluxer-static",
];

pub async fn run_smoke(quick: bool, public: bool) -> Result<()> {
    let timeout = if quick { 5 } else { 120 };
    wait_tcp(
        "Valkey",
        &env::var("VALKEY_HOST").unwrap_or_else(|_| "valkey".to_owned()),
        env::var("VALKEY_PORT")
            .unwrap_or_else(|_| "6379".to_owned())
            .parse()?,
        timeout,
    )
    .await?;
    wait_tcp(
        "NATS",
        &env::var("NATS_HOST").unwrap_or_else(|_| "nats".to_owned()),
        env::var("NATS_PORT")
            .unwrap_or_else(|_| "4222".to_owned())
            .parse()?,
        timeout,
    )
    .await?;
    wait_tcp(
        "LiveKit",
        &env::var("LIVEKIT_HOST").unwrap_or_else(|_| "livekit".to_owned()),
        env::var("LIVEKIT_PORT")
            .unwrap_or_else(|_| "7880".to_owned())
            .parse()?,
        timeout,
    )
    .await?;
    wait_tcp(
        "Mailpit SMTP",
        &env::var("MAILPIT_HOST").unwrap_or_else(|_| "mailpit".to_owned()),
        env::var("MAILPIT_SMTP_PORT")
            .unwrap_or_else(|_| "1025".to_owned())
            .parse()?,
        timeout,
    )
    .await?;
    wait_tcp(
        "SeaweedFS S3",
        &env::var("S3_HOST").unwrap_or_else(|_| "127.0.0.1".to_owned()),
        env::var("S3_PORT")
            .unwrap_or_else(|_| "8333".to_owned())
            .parse()?,
        timeout,
    )
    .await?;
    wait_http(
        "SeaweedFS master",
        &env::var("SEAWEEDFS_MASTER_URL")
            .unwrap_or_else(|_| "http://127.0.0.1:9333/cluster/status".to_owned()),
        timeout,
    )
    .await?;
    wait_s3_api(timeout).await?;
    if !quick {
        if cassandra_backend_enabled() {
            verify_schema(None, None).await?;
        }
        check_s3_buckets()?;
        check_config_load()?;
    }
    if public {
        check_gateway_internal_rpc().await?;
        check_public_routes(timeout).await?;
    }
    println!("Fluxer smoke checks passed.");
    Ok(())
}

fn check_config_load() -> Result<()> {
    let script = concat!(
        "void import('./packages/config/src/ConfigLoader.ts')",
        ".then(async (module) => { const config = await module.loadConfig(); ",
        "if (!config.integrations.voice.enabled) throw new Error('voice disabled'); ",
        "console.log('config: ok'); })",
        ".catch((error) => { console.error(error); process.exit(1); });",
    );
    crate::proc::run(&["pnpm", "exec", "tsx", "-e", script])
}

pub fn s3_endpoint() -> String {
    env::var("FLUXER_S3_ENDPOINT").unwrap_or_else(|_| "http://127.0.0.1:8333".to_owned())
}

pub fn s3_env() -> Vec<(String, Option<String>)> {
    vec![
        (
            "AWS_ACCESS_KEY_ID".to_owned(),
            Some(env::var("FLUXER_S3_ACCESS_KEY_ID").unwrap_or_else(|_| "fluxer".to_owned())),
        ),
        (
            "AWS_SECRET_ACCESS_KEY".to_owned(),
            Some(
                env::var("FLUXER_S3_SECRET_ACCESS_KEY")
                    .unwrap_or_else(|_| "fluxer-secret".to_owned()),
            ),
        ),
        (
            "AWS_DEFAULT_REGION".to_owned(),
            Some(env::var("FLUXER_S3_REGION").unwrap_or_else(|_| "us-east-1".to_owned())),
        ),
    ]
}

pub async fn wait_s3_api(timeout_secs: u64) -> Result<()> {
    let endpoint = s3_endpoint();
    let deadline = Instant::now() + Duration::from_secs(timeout_secs);
    let mut last_output = String::new();
    while Instant::now() < deadline {
        let env = merged_env(Some(&s3_env()), true)?;
        let output = Command::new("aws")
            .args(["--endpoint-url", &endpoint, "s3api", "list-buckets"])
            .env_clear()
            .envs(env)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .context("failed to run aws s3api list-buckets")?;
        if output.status.success() {
            println!("SeaweedFS S3 API is reachable at {endpoint}");
            return Ok(());
        }
        let mut combined = output.stdout;
        combined.extend(output.stderr);
        last_output = String::from_utf8_lossy(&combined).trim().to_owned();
        sleep(Duration::from_secs(2)).await;
    }
    bail!("Timed out waiting for SeaweedFS S3 API at {endpoint}: {last_output}");
}

pub fn ensure_s3_buckets() -> Result<()> {
    let endpoint = s3_endpoint();
    let env = s3_env();
    for bucket in S3_BUCKETS {
        ensure_s3_bucket(&endpoint, &env, bucket)?;
    }
    Ok(())
}

fn ensure_s3_bucket(endpoint: &str, env: &[(String, Option<String>)], bucket: &str) -> Result<()> {
    let deadline = Instant::now() + Duration::from_secs(30);

    loop {
        if s3_bucket_exists(endpoint, env, bucket)? {
            println!("S3 bucket exists: {bucket}");
            return Ok(());
        }

        let output = create_s3_bucket(endpoint, env, bucket)?;
        if output.status.success() {
            println!("Created S3 bucket: {bucket}");
            return Ok(());
        }

        let text = command_output_text(&output);
        if !s3_bucket_already_exists_output(&text) {
            let code = output.status.code().unwrap_or(-1);
            bail!(
                "Command failed with exit code {code}: aws --endpoint-url {endpoint} s3api create-bucket --bucket {bucket}"
            );
        }

        if Instant::now() >= deadline {
            bail!(
                "Timed out waiting for S3 bucket {bucket} to become readable after create-bucket reported it already exists: {text}"
            );
        }
        thread::sleep(Duration::from_secs(1));
    }
}

fn s3_bucket_exists(
    endpoint: &str,
    env: &[(String, Option<String>)],
    bucket: &str,
) -> Result<bool> {
    let output = run_command(
        &[
            "aws",
            "--endpoint-url",
            endpoint,
            "s3api",
            "head-bucket",
            "--bucket",
            bucket,
        ],
        RunOptions {
            env: env.to_vec(),
            check: false,
            capture: true,
            ..RunOptions::default()
        },
    )?;
    Ok(output.status.success())
}

fn create_s3_bucket(
    endpoint: &str,
    env: &[(String, Option<String>)],
    bucket: &str,
) -> Result<Output> {
    run_command(
        &[
            "aws",
            "--endpoint-url",
            endpoint,
            "s3api",
            "create-bucket",
            "--bucket",
            bucket,
        ],
        RunOptions {
            env: env.to_vec(),
            check: false,
            capture: true,
            ..RunOptions::default()
        },
    )
}

fn command_output_text(output: &Output) -> String {
    let mut combined = output.stdout.clone();
    combined.extend_from_slice(&output.stderr);
    String::from_utf8_lossy(&combined).trim().to_owned()
}

fn s3_bucket_already_exists_output(output: &str) -> bool {
    output.contains("BucketAlreadyExists") || output.contains("BucketAlreadyOwnedByYou")
}

fn check_s3_buckets() -> Result<()> {
    let endpoint = s3_endpoint();
    let env = s3_env();
    for bucket in S3_BUCKETS {
        run_command(
            &[
                "aws",
                "--endpoint-url",
                &endpoint,
                "s3api",
                "head-bucket",
                "--bucket",
                bucket,
            ],
            RunOptions {
                env: env.clone(),
                capture: true,
                ..RunOptions::default()
            },
        )?;
    }
    println!("S3 buckets: ok");
    Ok(())
}

async fn check_public_routes(timeout_secs: u64) -> Result<()> {
    let base_url = public_smoke_base_url();
    wait_http(
        "dev proxy api",
        &format!("{base_url}/api/_health"),
        timeout_secs,
    )
    .await?;
    wait_http(
        "dev proxy media",
        &format!("{base_url}/media/_health"),
        timeout_secs,
    )
    .await?;
    wait_http(
        "dev proxy gateway",
        &format!("{base_url}/gateway/_health"),
        timeout_secs,
    )
    .await?;
    check_gateway_websocket(&base_url, timeout_secs).await?;
    wait_http("dev proxy app", &format!("{base_url}/"), timeout_secs).await?;
    wait_http("devmail", &format!("{base_url}/devmail/"), timeout_secs).await
}

async fn check_gateway_websocket(base_url: &str, timeout_secs: u64) -> Result<()> {
    let url = gateway_websocket_url(base_url)?;
    let connect_timeout = Duration::from_secs(timeout_secs.clamp(5, 30));
    let (mut socket, _) = tokio::time::timeout(connect_timeout, connect_async(url.as_str()))
        .await
        .with_context(|| format!("timed out connecting to gateway websocket at {url}"))?
        .with_context(|| format!("failed to connect to gateway websocket at {url}"))?;
    let frame = tokio::time::timeout(Duration::from_secs(10), socket.next())
        .await
        .with_context(|| format!("timed out waiting for gateway websocket hello at {url}"))?
        .ok_or_else(|| anyhow::anyhow!("gateway websocket ended before hello at {url}"))?
        .with_context(|| format!("gateway websocket receive failed at {url}"))?;
    if matches!(frame, Message::Close(_)) {
        bail!("gateway websocket closed before hello at {url}");
    }
    println!("gateway websocket is reachable at {url}");
    Ok(())
}

fn gateway_websocket_url(base_url: &str) -> Result<String> {
    let mut url =
        Url::parse(base_url).with_context(|| format!("invalid public URL: {base_url}"))?;
    let scheme = match url.scheme() {
        "https" => "wss",
        "http" => "ws",
        scheme => bail!("public URL must use http or https for websocket smoke: {scheme}"),
    };
    url.set_scheme(scheme)
        .map_err(|_| anyhow::anyhow!("failed to set websocket scheme for {base_url}"))?;
    url.set_path("/gateway");
    url.set_query(Some("v=1&encoding=json&compress=zstd-stream&stream=1"));
    url.set_fragment(None);
    Ok(url.to_string())
}

fn public_smoke_base_url() -> String {
    if let Ok(url) = env::var("FLUXER_DEV_PUBLIC_SMOKE_URL") {
        let url = url.trim();
        if !url.is_empty() {
            return url.trim_end_matches('/').to_owned();
        }
    }
    if let Ok(url) = env::var("FLUXER_PUBLIC_URL") {
        let url = url.trim().trim_end_matches('/');
        if !url.is_empty() && !matches!(url, "http://localhost:8088" | "http://127.0.0.1:8088") {
            return url.to_owned();
        }
    }
    if let Ok(url) = crate::tunnel::resolve_cloudflare_public_url(None) {
        return url.trim_end_matches('/').to_owned();
    }
    format!("http://{LOOPBACK_HOST}:{DEV_PROXY_PORT}")
}

async fn check_gateway_internal_rpc() -> Result<()> {
    let base_endpoint = env::var("FLUXER_INTERNAL_API_ENDPOINT")
        .unwrap_or_else(|_| format!("http://{LOOPBACK_HOST}:{API_PORT}"));
    let endpoint = env::var("FLUXER_GATEWAY_API_RPC_ENDPOINT")
        .unwrap_or_else(|_| format!("{}/internal/rpc", base_endpoint.trim_end_matches('/')));
    let token = env::var("FLUXER_GATEWAY_RPC_AUTH_TOKEN").unwrap_or_default();
    let mut headers = HeaderMap::new();
    headers.insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
    headers.insert("x-forwarded-for", HeaderValue::from_static(LOOPBACK_HOST));
    headers.insert("x-fluxer-rpc-auth", HeaderValue::from_str(&token)?);
    let payload: serde_json::Value = reqwest::Client::new()
        .post(endpoint)
        .headers(headers)
        .json(&json!({"type": "get_gateway_rollout_config"}))
        .send()
        .await?
        .json()
        .await?;
    if payload.get("type").and_then(|value| value.as_str()) != Some("get_gateway_rollout_config")
        || payload.pointer("/data/config").is_none()
    {
        bail!("Gateway internal RPC returned unexpected payload: {payload}");
    }
    println!("Gateway internal RPC: ok");
    Ok(())
}

pub async fn bootstrap_schema_and_object_store() -> Result<()> {
    if cassandra_backend_enabled() {
        apply_schema(Some(config_from_env()?)).await?;
    }
    ensure_s3_buckets()
}

fn cassandra_backend_enabled() -> bool {
    env::var("FLUXER_DATABASE_BACKEND").as_deref() == Ok("cassandra")
}

pub async fn http_ok(url: &str) -> bool {
    reqwest::Client::new()
        .get(url)
        .timeout(Duration::from_secs(5))
        .send()
        .await
        .map(|response| response.status().as_u16() < 500)
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn s3_env_uses_fluxer_defaults() {
        let env = s3_env();
        assert!(env.iter().any(
            |(key, value)| key == "AWS_DEFAULT_REGION" && value.as_deref() == Some("us-east-1")
        ));
    }

    #[test]
    fn bucket_already_exists_output_matches_aws_errors() {
        assert!(s3_bucket_already_exists_output(
            "An error occurred (BucketAlreadyExists) when calling the CreateBucket operation"
        ));
        assert!(s3_bucket_already_exists_output(
            "An error occurred (BucketAlreadyOwnedByYou) when calling the CreateBucket operation"
        ));
        assert!(!s3_bucket_already_exists_output(
            "An error occurred (AccessDenied) when calling the CreateBucket operation"
        ));
    }

    #[test]
    fn gateway_websocket_url_uses_public_scheme() {
        assert_eq!(
            gateway_websocket_url("https://dev.example.test").unwrap(),
            "wss://dev.example.test/gateway?v=1&encoding=json&compress=zstd-stream&stream=1"
        );
        assert_eq!(
            gateway_websocket_url("http://localhost:8088").unwrap(),
            "ws://localhost:8088/gateway?v=1&encoding=json&compress=zstd-stream&stream=1"
        );
    }
}
