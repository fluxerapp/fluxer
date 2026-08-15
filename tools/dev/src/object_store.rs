// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::cassandra::{apply_schema, config_from_env};
use crate::proc::{RunOptions, merged_env, run_command};
use anyhow::{Context, Result, bail};
use std::env;
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::{Duration, Instant};
use tokio::time::sleep;

pub const S3_BUCKETS: &[&str] = &[
    "fluxer",
    "fluxer-uploads",
    "fluxer-downloads",
    "fluxer-reports",
    "fluxer-harvests",
    "fluxer-static",
];

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

pub async fn bootstrap_schema_and_object_store() -> Result<()> {
    if cassandra_backend_enabled() {
        apply_schema(Some(config_from_env()?)).await?;
    }
    ensure_s3_buckets()
}

fn cassandra_backend_enabled() -> bool {
    env::var("FLUXER_DATABASE_BACKEND").as_deref() == Ok("cassandra")
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
}
