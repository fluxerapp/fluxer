// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result, anyhow, bail, ensure};
use aws_config::{BehaviorVersion, Region, retry::RetryConfig};
use aws_sdk_s3::Client as S3Client;
use aws_sdk_s3::error::ProvideErrorMetadata;
use aws_sdk_s3::primitives::ByteStream;
use aws_sdk_s3::types::MetadataDirective;
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use chrono::{DateTime, Datelike, NaiveDate, NaiveDateTime, TimeZone, Timelike, Utc};
use md5::{Digest as _, Md5};
use reqwest::Client;
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::{self, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Semaphore;
use tokio::task::JoinSet;
use walkdir::WalkDir;

pub(crate) use crate::functions::{remove_dir_if_exists, remove_file_if_exists};

const DEFAULT_S3_WRITE_CONCURRENCY: usize = 8;
const DEFAULT_S3_RETRY_ATTEMPTS: u32 = 8;

pub(crate) const CALVER_SCHEME: &str = "YYYY.MDD.MICRO";

#[derive(Default)]
pub(crate) struct CalverEnv {
    pub(crate) build_version: Option<String>,
    pub(crate) fluxer_build_version: Option<String>,
    pub(crate) fluxer_build_date: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CommandSpec {
    pub(crate) program: OsString,
    pub(crate) args: Vec<OsString>,
    pub(crate) cwd: Option<PathBuf>,
    pub(crate) env: Vec<(OsString, OsString)>,
    pub(crate) env_remove: Vec<OsString>,
}

impl CommandSpec {
    pub(crate) fn new(program: impl Into<OsString>) -> Self {
        Self {
            program: program.into(),
            args: Vec::new(),
            cwd: None,
            env: Vec::new(),
            env_remove: Vec::new(),
        }
    }

    pub(crate) fn arg(mut self, arg: impl Into<OsString>) -> Self {
        self.args.push(arg.into());
        self
    }

    pub(crate) fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<OsString>,
    {
        self.args.extend(args.into_iter().map(Into::into));
        self
    }

    pub(crate) fn env(mut self, key: impl Into<OsString>, value: impl Into<OsString>) -> Self {
        self.env.push((key.into(), value.into()));
        self
    }

    pub(crate) fn env_remove(mut self, key: impl Into<OsString>) -> Self {
        self.env_remove.push(key.into());
        self
    }

    pub(crate) fn current_dir(mut self, cwd: impl Into<PathBuf>) -> Self {
        self.cwd = Some(cwd.into());
        self
    }

    pub(crate) fn to_command(&self) -> Command {
        let mut command = Command::new(&self.program);
        command.args(&self.args);
        if let Some(cwd) = &self.cwd {
            command.current_dir(cwd);
        }
        for key in &self.env_remove {
            command.env_remove(key);
        }
        for (key, value) in &self.env {
            command.env(key, value);
        }
        command
    }
}

#[derive(Debug)]
pub(crate) struct CapturedOutput {
    pub(crate) status: i32,
    pub(crate) stdout: Vec<u8>,
    pub(crate) stderr: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct S3UploadPlanItem {
    pub(crate) path: PathBuf,
    pub(crate) key: String,
    pub(crate) content_type: Option<String>,
    pub(crate) cache_control: Option<String>,
    pub(crate) repair_existing_metadata: bool,
}

impl S3UploadPlanItem {
    pub(crate) fn new(path: PathBuf, key: String) -> Self {
        Self {
            path,
            key,
            content_type: None,
            cache_control: None,
            repair_existing_metadata: false,
        }
    }

    pub(crate) fn with_content_type(mut self, content_type: impl Into<String>) -> Self {
        self.content_type = Some(content_type.into());
        self
    }

    pub(crate) fn with_cache_control(mut self, cache_control: impl Into<String>) -> Self {
        self.cache_control = Some(cache_control.into());
        self
    }

    pub(crate) fn with_detected_content_type(self) -> Self {
        let Some(content_type) = s3_content_type_for_key(&self.key) else {
            return self;
        };
        self.with_content_type(content_type)
    }

    pub(crate) fn repair_existing_metadata(mut self) -> Self {
        self.repair_existing_metadata = true;
        self
    }

    fn has_upload_metadata(&self) -> bool {
        self.content_type.is_some() || self.cache_control.is_some()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct S3UploadStats {
    pub(crate) uploaded: usize,
    pub(crate) skipped_existing: usize,
    pub(crate) metadata_repaired: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum S3UploadDisposition {
    Uploaded,
    SkippedExisting,
    MetadataRepaired,
}

pub(crate) fn env_string(name: &str) -> Option<String> {
    env::var(name).ok().filter(|value| !value.is_empty())
}

pub(crate) fn require_env(name: &str) -> Result<String> {
    env_string(name).ok_or_else(|| anyhow!("Missing required environment variable: {name}"))
}

pub(crate) fn require_any_env(names: &[&str]) -> Result<String> {
    for name in names {
        if let Some(value) = env_string(name) {
            return Ok(value);
        }
    }
    bail!(
        "Missing required environment variable: {}",
        names.join(" or ")
    )
}

pub(crate) fn parse_bool(value: &str) -> bool {
    matches!(
        value.to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

pub(crate) fn env_bool(name: &str) -> bool {
    env::var(name).ok().is_some_and(|value| parse_bool(&value))
}

pub(crate) fn trim_option(value: Option<String>) -> Option<String> {
    value
        .map(|value| value.trim().to_string())
        .filter(|value| !value.is_empty())
}

pub(crate) fn append_github_output(pairs: &[(&str, &str)]) -> Result<()> {
    append_key_values("GITHUB_OUTPUT", pairs)
}

pub(crate) fn append_github_env(pairs: &[(&str, &str)]) -> Result<()> {
    append_key_values("GITHUB_ENV", pairs)
}

pub(crate) fn append_github_path(path: &Path) -> Result<()> {
    let github_path = require_env("GITHUB_PATH")?;
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&github_path)
        .with_context(|| format!("Failed to open {github_path}"))?;
    writeln!(file, "{}", path.display()).with_context(|| format!("Failed to write {github_path}"))
}

fn append_key_values(path_env_name: &str, pairs: &[(&str, &str)]) -> Result<()> {
    let path = require_env(path_env_name)?;
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .with_context(|| format!("Failed to open {path}"))?;
    for (key, value) in pairs {
        writeln!(file, "{key}={value}").with_context(|| format!("Failed to write {path}"))?;
    }
    Ok(())
}

pub(crate) fn run_command(spec: CommandSpec) -> Result<()> {
    println!("+ {}", display_command(&spec));
    let status = spec
        .to_command()
        .status()
        .with_context(|| format!("Failed to run {}", spec.program.to_string_lossy()))?;
    ensure!(
        status.success(),
        "Command failed with exit code {}: {}",
        status.code().unwrap_or(1),
        display_command(&spec)
    );
    Ok(())
}

pub(crate) fn output_text(spec: CommandSpec) -> Result<String> {
    let output = output(spec)?;
    String::from_utf8(output.stdout)
        .map(|value| value.trim().to_string())
        .context("Command output was not valid UTF-8")
}

pub(crate) fn output_bytes(spec: CommandSpec) -> Result<Vec<u8>> {
    Ok(output(spec)?.stdout)
}

fn output(spec: CommandSpec) -> Result<CapturedOutput> {
    let captured = capture(spec.clone())?;
    if captured.status == 0 {
        Ok(captured)
    } else {
        let stdout = String::from_utf8_lossy(&captured.stdout);
        let stderr = String::from_utf8_lossy(&captured.stderr);
        bail!(
            "Command failed with exit code {}: {}\n{}{}",
            captured.status,
            display_command(&spec),
            stdout,
            stderr
        );
    }
}

pub(crate) fn capture(spec: CommandSpec) -> Result<CapturedOutput> {
    println!("+ {}", display_command(&spec));
    let output = spec
        .to_command()
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| format!("Failed to run {}", spec.program.to_string_lossy()))?;
    print!("{}", String::from_utf8_lossy(&output.stdout));
    eprint!("{}", String::from_utf8_lossy(&output.stderr));
    Ok(CapturedOutput {
        status: output.status.code().unwrap_or(1),
        stdout: output.stdout,
        stderr: output.stderr,
    })
}

pub(crate) fn command_succeeds(spec: CommandSpec) -> bool {
    spec.to_command()
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

fn display_command(spec: &CommandSpec) -> String {
    std::iter::once(spec.program.to_string_lossy().to_string())
        .chain(spec.args.iter().map(|arg| quote_arg(arg)))
        .collect::<Vec<_>>()
        .join(" ")
}

fn quote_arg(arg: &OsStr) -> String {
    let value = arg.to_string_lossy();
    if value.chars().all(|ch| {
        ch.is_ascii_alphanumeric() || matches!(ch, '-' | '_' | '.' | '/' | ':' | '=' | '\\')
    }) {
        value.to_string()
    } else {
        format!("{value:?}")
    }
}

pub(crate) fn resolve_calver(calver_env: &CalverEnv, now: DateTime<Utc>) -> Result<String> {
    if let Some(version) = calver_env
        .build_version
        .as_deref()
        .or(calver_env.fluxer_build_version.as_deref())
    {
        parse_version_instant(version)?;
        return Ok(version.to_string());
    }

    Ok(format_calver(parse_instant(calver_env, now)?))
}

fn parse_instant(calver_env: &CalverEnv, now: DateTime<Utc>) -> Result<DateTime<Utc>> {
    let Some(override_value) = calver_env.fluxer_build_date.as_deref() else {
        return Ok(now);
    };

    if let Ok(date) = NaiveDate::parse_from_str(override_value, "%Y-%m-%d") {
        return date
            .and_hms_opt(0, 0, 0)
            .map(|instant| Utc.from_utc_datetime(&instant))
            .ok_or_else(|| anyhow!("Invalid FLUXER_BUILD_DATE: {override_value}"));
    }

    if let Ok(instant) = NaiveDateTime::parse_from_str(override_value, "%Y-%m-%dT%H:%M:%SZ") {
        return Ok(Utc.from_utc_datetime(&instant));
    }

    bail!("Invalid FLUXER_BUILD_DATE: {override_value}")
}

fn format_calver(instant: DateTime<Utc>) -> String {
    format!(
        "{}.{}.{}",
        instant.year(),
        month_day_segment(instant),
        micro_segment(instant)
    )
}

fn month_day_segment(instant: DateTime<Utc>) -> String {
    format!("{}{:02}", instant.month(), instant.day())
}

fn micro_segment(instant: DateTime<Utc>) -> String {
    format!(
        "{:02}{:02}{:02}",
        instant.hour(),
        instant.minute(),
        instant.second()
    )
    .parse::<u32>()
    .expect("HHMMSS time segment should parse")
    .to_string()
}

pub(crate) fn parse_version_instant(version: &str) -> Result<DateTime<Utc>> {
    let mut parts = version.split('.');
    let year = parts.next().unwrap_or_default();
    let month_day = parts.next().unwrap_or_default();
    let micro = parts.next().unwrap_or_default();
    if parts.next().is_some()
        || year.len() != 4
        || !valid_nonzero_digits(year, 4)
        || !(month_day.len() == 3 || month_day.len() == 4)
        || !valid_nonzero_digits(month_day, month_day.len())
        || !valid_micro(micro)
    {
        bail!("Invalid build version: {version} (expected {CALVER_SCHEME}, e.g. 2026.520.0)");
    }

    let year: i32 = year.parse().map_err(|_| invalid_version_date(version))?;
    let (month, day) = if month_day.len() == 3 {
        (&month_day[0..1], &month_day[1..3])
    } else {
        (&month_day[0..2], &month_day[2..4])
    };
    let month: u32 = month.parse().map_err(|_| invalid_version_date(version))?;
    let day: u32 = day.parse().map_err(|_| invalid_version_date(version))?;
    let micro: u32 = micro.parse().map_err(|_| invalid_version_date(version))?;
    let hour = micro / 10_000;
    let minute = (micro / 100) % 100;
    let second = micro % 100;
    if hour > 23 || minute > 59 || second > 59 {
        return Err(invalid_version_date(version));
    }

    Utc.with_ymd_and_hms(year, month, day, hour, minute, second)
        .single()
        .ok_or_else(|| invalid_version_date(version))
}

fn valid_nonzero_digits(value: &str, max_len: usize) -> bool {
    !value.is_empty()
        && value.len() <= max_len
        && !value.starts_with('0')
        && value.bytes().all(|byte| byte.is_ascii_digit())
}

fn valid_micro(value: &str) -> bool {
    value == "0" || valid_nonzero_digits(value, 6)
}

fn invalid_version_date(version: &str) -> anyhow::Error {
    anyhow!("Invalid build version date/time: {version}")
}

pub(crate) async fn s3_client(default_endpoint: Option<&str>) -> Result<S3Client> {
    let region = env::var("AWS_REGION")
        .or_else(|_| env::var("AWS_DEFAULT_REGION"))
        .unwrap_or_else(|_| "us-east-1".to_string());
    let mut loader = aws_config::defaults(BehaviorVersion::latest()).region(Region::new(region));
    let endpoint = env_string("S3_ENDPOINT").or_else(|| default_endpoint.map(ToOwned::to_owned));
    if let Some(endpoint) = endpoint.as_deref() {
        loader = loader.endpoint_url(endpoint);
    }
    loader = loader.retry_config(s3_retry_config());
    let shared_config = loader.load().await;
    let mut s3_config = aws_sdk_s3::config::Builder::from(&shared_config);
    if endpoint.is_some() || env_bool("S3_FORCE_PATH_STYLE") {
        s3_config = s3_config.force_path_style(true);
    }
    Ok(S3Client::from_conf(s3_config.build()))
}

pub(crate) async fn upload_directory_to_s3<F>(
    client: &S3Client,
    bucket: &str,
    prefix: &str,
    root: &Path,
    include: F,
) -> Result<()>
where
    F: Fn(&Path) -> bool,
{
    let plan = directory_upload_plan(prefix, root, include)?;
    let stats = upload_s3_plan_append_only(client, bucket, plan).await?;
    println!(
        "Append-only upload complete for s3://{bucket}/{prefix}: uploaded {}, skipped existing {}",
        stats.uploaded, stats.skipped_existing
    );
    Ok(())
}

pub(crate) async fn upload_directory_to_s3_overwrite<F>(
    client: &S3Client,
    bucket: &str,
    prefix: &str,
    root: &Path,
    include: F,
) -> Result<()>
where
    F: Fn(&Path) -> bool,
{
    let plan = directory_upload_plan(prefix, root, include)?;
    let stats = upload_s3_plan_overwrite(client, bucket, plan).await?;
    println!(
        "Overwrite upload complete for s3://{bucket}/{prefix}: uploaded {}",
        stats.uploaded
    );
    Ok(())
}

pub(crate) async fn upload_s3_plan_append_only(
    client: &S3Client,
    bucket: &str,
    plan: Vec<S3UploadPlanItem>,
) -> Result<S3UploadStats> {
    ensure_unique_s3_keys(&plan)?;
    let existing_objects = existing_s3_objects_for_plan(client, bucket, &plan).await?;
    let mut stats = S3UploadStats::default();
    let (pending, skipped_items) = split_append_only_upload_plan(bucket, plan, &existing_objects)?;

    let concurrency = s3_write_concurrency();
    let semaphore = Arc::new(Semaphore::new(concurrency));
    let bucket = bucket.to_string();
    let mut tasks = JoinSet::new();
    for item in pending {
        let permit = semaphore
            .clone()
            .acquire_owned()
            .await
            .context("S3 upload semaphore closed")?;
        let client = client.clone();
        let bucket = bucket.clone();
        tasks.spawn(async move {
            let _permit = permit;
            let disposition = put_file_to_s3_if_absent(&client, &bucket, &item)
                .await
                .with_context(|| {
                    format!("Failed append-only upload for s3://{}/{}", bucket, item.key)
                })?;
            Ok::<_, anyhow::Error>(disposition)
        });
    }
    for item in skipped_items {
        if item.repair_existing_metadata && item.has_upload_metadata() {
            let permit = semaphore
                .clone()
                .acquire_owned()
                .await
                .context("S3 upload semaphore closed")?;
            let client = client.clone();
            let bucket = bucket.clone();
            tasks.spawn(async move {
                let _permit = permit;
                repair_existing_s3_object_metadata(&client, &bucket, &item)
                    .await
                    .with_context(|| {
                        format!("Failed to repair metadata for s3://{}/{}", bucket, item.key)
                    })?;
                Ok::<_, anyhow::Error>(S3UploadDisposition::MetadataRepaired)
            });
        } else {
            println!("Skipping existing s3://{bucket}/{}", item.key);
            stats.skipped_existing += 1;
        }
    }

    while let Some(result) = tasks.join_next().await {
        match result.context("S3 upload task failed")?? {
            S3UploadDisposition::Uploaded => stats.uploaded += 1,
            S3UploadDisposition::SkippedExisting => stats.skipped_existing += 1,
            S3UploadDisposition::MetadataRepaired => stats.metadata_repaired += 1,
        }
    }

    Ok(stats)
}

pub(crate) async fn upload_s3_plan_overwrite(
    client: &S3Client,
    bucket: &str,
    plan: Vec<S3UploadPlanItem>,
) -> Result<S3UploadStats> {
    ensure_unique_s3_keys(&plan)?;
    let concurrency = s3_write_concurrency();
    let semaphore = Arc::new(Semaphore::new(concurrency));
    let bucket = bucket.to_string();
    let mut tasks = JoinSet::new();
    for item in plan {
        let permit = semaphore
            .clone()
            .acquire_owned()
            .await
            .context("S3 upload semaphore closed")?;
        let client = client.clone();
        let bucket = bucket.clone();
        tasks.spawn(async move {
            let _permit = permit;
            put_file_to_s3_overwrite(&client, &bucket, &item)
                .await
                .with_context(|| {
                    format!("Failed overwrite upload for s3://{}/{}", bucket, item.key)
                })?;
            Ok::<_, anyhow::Error>(S3UploadDisposition::Uploaded)
        });
    }

    let mut stats = S3UploadStats::default();
    while let Some(result) = tasks.join_next().await {
        match result.context("S3 upload task failed")?? {
            S3UploadDisposition::Uploaded => stats.uploaded += 1,
            S3UploadDisposition::SkippedExisting => stats.skipped_existing += 1,
            S3UploadDisposition::MetadataRepaired => stats.metadata_repaired += 1,
        }
    }

    Ok(stats)
}

pub(crate) async fn upload_s3_plan_sync(
    client: &S3Client,
    bucket: &str,
    plan: Vec<S3UploadPlanItem>,
) -> Result<S3UploadStats> {
    ensure_unique_s3_keys(&plan)?;
    let existing_objects = existing_s3_objects_for_plan(client, bucket, &plan).await?;
    let mut to_upload = Vec::new();
    let mut skipped_existing = 0_usize;
    for item in plan {
        match existing_objects.get(&item.key) {
            Some(remote) if s3_object_matches_local(&item, remote)? => skipped_existing += 1,
            _ => to_upload.push(item),
        }
    }

    let concurrency = s3_write_concurrency();
    let semaphore = Arc::new(Semaphore::new(concurrency));
    let bucket = bucket.to_string();
    let mut tasks = JoinSet::new();
    for item in to_upload {
        let permit = semaphore
            .clone()
            .acquire_owned()
            .await
            .context("S3 upload semaphore closed")?;
        let client = client.clone();
        let bucket = bucket.clone();
        tasks.spawn(async move {
            let _permit = permit;
            put_file_to_s3_overwrite(&client, &bucket, &item)
                .await
                .with_context(|| format!("Failed sync upload for s3://{}/{}", bucket, item.key))?;
            Ok::<_, anyhow::Error>(())
        });
    }

    let mut stats = S3UploadStats {
        skipped_existing,
        ..Default::default()
    };
    while let Some(result) = tasks.join_next().await {
        result.context("S3 upload task failed")??;
        stats.uploaded += 1;
    }

    Ok(stats)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct S3ObjectMetadata {
    pub(crate) e_tag: Option<String>,
    pub(crate) size: Option<i64>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct S3ListedObject {
    key: String,
    metadata: S3ObjectMetadata,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct S3FileIdentity {
    size: u64,
    md5_hex: String,
    md5_base64: String,
}

async fn existing_s3_objects_for_plan(
    client: &S3Client,
    bucket: &str,
    plan: &[S3UploadPlanItem],
) -> Result<BTreeMap<String, S3ObjectMetadata>> {
    let planned_keys = plan
        .iter()
        .map(|item| item.key.clone())
        .collect::<BTreeSet<_>>();
    let prefixes = s3_list_prefixes_for_plan(plan);
    let mut existing = BTreeMap::new();

    for prefix in prefixes {
        println!("Scanning existing S3 objects under s3://{bucket}/{prefix}");
        for object in list_s3_objects(client, bucket, &prefix).await? {
            if planned_keys.contains(&object.key) {
                existing.insert(object.key, object.metadata);
            }
        }
    }

    Ok(existing)
}

pub(crate) fn split_append_only_upload_plan(
    bucket: &str,
    plan: Vec<S3UploadPlanItem>,
    existing_objects: &BTreeMap<String, S3ObjectMetadata>,
) -> Result<(Vec<S3UploadPlanItem>, Vec<S3UploadPlanItem>)> {
    ensure_unique_s3_keys(&plan)?;
    let mut pending = Vec::new();
    let mut skipped = Vec::new();
    for item in plan {
        if let Some(remote) = existing_objects.get(&item.key) {
            ensure_existing_s3_object_matches_local(bucket, &item, remote)?;
            skipped.push(item);
        } else {
            pending.push(item);
        }
    }
    Ok((pending, skipped))
}

fn ensure_existing_s3_object_matches_local(
    bucket: &str,
    item: &S3UploadPlanItem,
    remote: &S3ObjectMetadata,
) -> Result<()> {
    let local = s3_file_identity(&item.path)?;
    if let Some(remote_size) = remote.size {
        ensure!(
            remote_size >= 0,
            "Existing S3 object has invalid negative size: s3://{bucket}/{}",
            item.key
        );
        ensure!(
            remote_size as u64 == local.size,
            "Existing S3 object differs from local file: s3://{bucket}/{} has {} byte(s), local {} has {} byte(s)",
            item.key,
            remote_size,
            item.path.display(),
            local.size
        );
    }

    let Some(e_tag) = remote.e_tag.as_deref() else {
        bail!(
            "Existing S3 object cannot be verified because ListObjectsV2 did not return an ETag: s3://{bucket}/{}",
            item.key
        );
    };
    let Some(remote_md5) = s3_etag_md5(e_tag) else {
        bail!(
            "Existing S3 object cannot be verified because its ETag is not a single-part MD5: s3://{bucket}/{} (ETag {e_tag})",
            item.key
        );
    };
    ensure!(
        remote_md5 == local.md5_hex,
        "Existing S3 object differs from local file: s3://{bucket}/{} has ETag {e_tag}, local {} has MD5 {}",
        item.key,
        item.path.display(),
        local.md5_hex
    );
    Ok(())
}

fn s3_object_matches_local(item: &S3UploadPlanItem, remote: &S3ObjectMetadata) -> Result<bool> {
    let local = s3_file_identity(&item.path)?;
    if let Some(remote_size) = remote.size
        && (remote_size < 0 || remote_size as u64 != local.size)
    {
        return Ok(false);
    }
    let Some(e_tag) = remote.e_tag.as_deref() else {
        return Ok(false);
    };
    let Some(remote_md5) = s3_etag_md5(e_tag) else {
        return Ok(false);
    };
    Ok(remote_md5 == local.md5_hex)
}

fn s3_file_identity(path: &Path) -> Result<S3FileIdentity> {
    let mut file =
        fs::File::open(path).with_context(|| format!("Failed to read {}", path.display()))?;
    let size = file
        .metadata()
        .with_context(|| format!("Failed to stat {}", path.display()))?
        .len();
    let mut hasher = Md5::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let read = file
            .read(&mut buffer)
            .with_context(|| format!("Failed to read {}", path.display()))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    let digest = hasher.finalize();
    Ok(S3FileIdentity {
        size,
        md5_hex: hex::encode(digest),
        md5_base64: BASE64.encode(digest),
    })
}

fn s3_etag_md5(e_tag: &str) -> Option<String> {
    let value = e_tag.trim().trim_matches('"');
    if value.len() == 32 && value.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        Some(value.to_ascii_lowercase())
    } else {
        None
    }
}

fn s3_list_prefixes_for_plan(plan: &[S3UploadPlanItem]) -> BTreeSet<String> {
    plan.iter()
        .map(|item| s3_list_prefix_for_key(&item.key))
        .collect()
}

fn s3_list_prefix_for_key(key: &str) -> String {
    key.find('/')
        .map(|index| key[..=index].to_string())
        .unwrap_or_else(|| key.to_string())
}

fn ensure_unique_s3_keys(plan: &[S3UploadPlanItem]) -> Result<()> {
    let mut keys = BTreeSet::new();
    for item in plan {
        ensure!(
            keys.insert(item.key.clone()),
            "Duplicate S3 upload key in local plan: {}",
            item.key
        );
    }
    Ok(())
}

async fn put_file_to_s3_if_absent(
    client: &S3Client,
    bucket: &str,
    item: &S3UploadPlanItem,
) -> Result<S3UploadDisposition> {
    println!(
        "Uploading {} -> s3://{bucket}/{}",
        item.path.display(),
        item.key
    );
    let identity = s3_file_identity(&item.path)?;
    let attempts = s3_retry_attempts();
    for attempt in 1..=attempts {
        let body = ByteStream::from_path(&item.path)
            .await
            .with_context(|| format!("Failed to read {}", item.path.display()))?;
        let mut request = client
            .put_object()
            .bucket(bucket)
            .key(&item.key)
            .if_none_match("*")
            .content_md5(identity.md5_base64.clone())
            .body(body);
        if let Some(content_type) = &item.content_type {
            request = request.content_type(content_type);
        }
        if let Some(cache_control) = &item.cache_control {
            request = request.cache_control(cache_control);
        }
        match request.send().await {
            Ok(_) => return Ok(S3UploadDisposition::Uploaded),
            Err(error) => {
                let code = error
                    .as_service_error()
                    .and_then(|error| error.code())
                    .map(ToOwned::to_owned);
                let status = error
                    .raw_response()
                    .map(|response| response.status().as_u16());
                if is_existing_object_error(code.as_deref(), status) {
                    println!("Skipping existing s3://{bucket}/{}", item.key);
                    return Ok(S3UploadDisposition::SkippedExisting);
                }
                if is_retryable_s3_error(code.as_deref(), status) && attempt < attempts {
                    sleep_before_s3_retry(
                        "upload",
                        &format!("s3://{bucket}/{}", item.key),
                        attempt,
                        attempts,
                        code.as_deref(),
                        status,
                    )
                    .await;
                    continue;
                }
                let summary = s3_error_summary(code.as_deref(), status);
                return Err(error).with_context(|| {
                    format!("Failed to upload s3://{bucket}/{}{summary}", item.key)
                });
            }
        }
    }
    unreachable!("S3 retry attempts are always greater than zero")
}

async fn repair_existing_s3_object_metadata(
    client: &S3Client,
    bucket: &str,
    item: &S3UploadPlanItem,
) -> Result<()> {
    println!(
        "Repairing metadata for {} -> s3://{bucket}/{}",
        item.path.display(),
        item.key
    );
    let identity = s3_file_identity(&item.path)?;
    let attempts = s3_retry_attempts();
    for attempt in 1..=attempts {
        let body = ByteStream::from_path(&item.path)
            .await
            .with_context(|| format!("Failed to read {}", item.path.display()))?;
        let mut request = client
            .put_object()
            .bucket(bucket)
            .key(&item.key)
            .content_md5(identity.md5_base64.clone())
            .body(body);
        if let Some(content_type) = &item.content_type {
            request = request.content_type(content_type);
        }
        if let Some(cache_control) = &item.cache_control {
            request = request.cache_control(cache_control);
        }
        match request.send().await {
            Ok(_) => return Ok(()),
            Err(error) => {
                let code = error
                    .as_service_error()
                    .and_then(|error| error.code())
                    .map(ToOwned::to_owned);
                let status = error
                    .raw_response()
                    .map(|response| response.status().as_u16());
                if is_retryable_s3_error(code.as_deref(), status) && attempt < attempts {
                    sleep_before_s3_retry(
                        "metadata repair",
                        &format!("s3://{bucket}/{}", item.key),
                        attempt,
                        attempts,
                        code.as_deref(),
                        status,
                    )
                    .await;
                    continue;
                }
                let summary = s3_error_summary(code.as_deref(), status);
                return Err(error).with_context(|| {
                    format!(
                        "Failed to repair metadata for s3://{bucket}/{}{summary}",
                        item.key
                    )
                });
            }
        }
    }
    unreachable!("S3 retry attempts are always greater than zero")
}

async fn put_file_to_s3_overwrite(
    client: &S3Client,
    bucket: &str,
    item: &S3UploadPlanItem,
) -> Result<()> {
    println!(
        "Overwriting {} -> s3://{bucket}/{}",
        item.path.display(),
        item.key
    );
    let identity = s3_file_identity(&item.path)?;
    let attempts = s3_retry_attempts();
    for attempt in 1..=attempts {
        let body = ByteStream::from_path(&item.path)
            .await
            .with_context(|| format!("Failed to read {}", item.path.display()))?;
        let mut request = client
            .put_object()
            .bucket(bucket)
            .key(&item.key)
            .content_md5(identity.md5_base64.clone())
            .body(body);
        if let Some(content_type) = &item.content_type {
            request = request.content_type(content_type);
        }
        if let Some(cache_control) = &item.cache_control {
            request = request.cache_control(cache_control);
        }
        match request.send().await {
            Ok(_) => return Ok(()),
            Err(error) => {
                let code = error
                    .as_service_error()
                    .and_then(|error| error.code())
                    .map(ToOwned::to_owned);
                let status = error
                    .raw_response()
                    .map(|response| response.status().as_u16());
                if is_retryable_s3_error(code.as_deref(), status) && attempt < attempts {
                    sleep_before_s3_retry(
                        "overwrite upload",
                        &format!("s3://{bucket}/{}", item.key),
                        attempt,
                        attempts,
                        code.as_deref(),
                        status,
                    )
                    .await;
                    continue;
                }
                let summary = s3_error_summary(code.as_deref(), status);
                return Err(error).with_context(|| {
                    format!(
                        "Failed to overwrite upload s3://{bucket}/{}{summary}",
                        item.key
                    )
                });
            }
        }
    }
    unreachable!("S3 retry attempts are always greater than zero")
}

fn is_existing_object_error(code: Option<&str>, status: Option<u16>) -> bool {
    matches!(
        code,
        Some("PreconditionFailed" | "ConditionalRequestConflict")
    ) || matches!(status, Some(409 | 412))
}

fn is_retryable_s3_error(code: Option<&str>, status: Option<u16>) -> bool {
    status == Some(429) || matches!(code, Some("RateLimitExceeded" | "TooManyRequests"))
}

async fn sleep_before_s3_retry(
    operation: &str,
    target: &str,
    attempt: u32,
    attempts: u32,
    code: Option<&str>,
    status: Option<u16>,
) {
    let delay = s3_retry_delay(attempt);
    let summary = s3_error_summary(code, status);
    println!(
        "Retrying S3 {operation} {target}{summary} in {:.1}s (attempt {}/{attempts})",
        delay.as_secs_f32(),
        attempt + 1
    );
    tokio::time::sleep(delay).await;
}

fn s3_retry_delay(attempt: u32) -> Duration {
    let shift = attempt.saturating_sub(1).min(5);
    Duration::from_millis(500 * (1_u64 << shift))
}

fn s3_error_summary(code: Option<&str>, status: Option<u16>) -> String {
    match (code, status) {
        (Some(code), Some(status)) => format!(" (S3 code {code}, HTTP status {status})"),
        (Some(code), None) => format!(" (S3 code {code})"),
        (None, Some(status)) => format!(" (HTTP status {status})"),
        (None, None) => String::new(),
    }
}

fn s3_retry_config() -> RetryConfig {
    RetryConfig::standard()
        .with_max_attempts(s3_retry_attempts())
        .with_initial_backoff(Duration::from_millis(500))
        .with_max_backoff(Duration::from_secs(30))
}

fn s3_retry_attempts() -> u32 {
    env::var("S3_RETRY_ATTEMPTS")
        .ok()
        .and_then(|value| value.parse::<u32>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(DEFAULT_S3_RETRY_ATTEMPTS)
}

fn s3_write_concurrency() -> usize {
    env::var("S3_WRITE_CONCURRENCY")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(DEFAULT_S3_WRITE_CONCURRENCY)
}

pub(crate) fn directory_upload_plan<F>(
    prefix: &str,
    root: &Path,
    include: F,
) -> Result<Vec<S3UploadPlanItem>>
where
    F: Fn(&Path) -> bool,
{
    Ok(collect_files(root)?
        .into_iter()
        .filter_map(|file| {
            let relative = file.strip_prefix(root).ok()?;
            if !include(relative) {
                return None;
            }
            let key = join_s3_key(prefix, &path_to_s3_key(relative));
            Some(S3UploadPlanItem::new(file, key).with_detected_content_type())
        })
        .collect::<Vec<_>>())
}

pub(crate) fn s3_content_type_for_key(key: &str) -> Option<&'static str> {
    let ext = key.rsplit('.').next()?.to_ascii_lowercase();
    match ext.as_str() {
        "html" | "htm" => Some("text/html; charset=utf-8"),
        "js" | "mjs" => Some("application/javascript; charset=utf-8"),
        "css" => Some("text/css; charset=utf-8"),
        "json" | "map" => Some("application/json; charset=utf-8"),
        "wasm" => Some("application/wasm"),
        "png" => Some("image/png"),
        "jpg" | "jpeg" => Some("image/jpeg"),
        "gif" => Some("image/gif"),
        "webp" => Some("image/webp"),
        "avif" => Some("image/avif"),
        "svg" => Some("image/svg+xml"),
        "ico" => Some("image/x-icon"),
        "woff" => Some("font/woff"),
        "woff2" => Some("font/woff2"),
        "ttf" => Some("font/ttf"),
        "otf" => Some("font/otf"),
        "eot" => Some("application/vnd.ms-fontobject"),
        "mp3" => Some("audio/mpeg"),
        "mp4" => Some("video/mp4"),
        "webm" => Some("video/webm"),
        "ogg" => Some("audio/ogg"),
        "wav" => Some("audio/wav"),
        "pdf" => Some("application/pdf"),
        "txt" => Some("text/plain; charset=utf-8"),
        "xml" => Some("application/xml; charset=utf-8"),
        "webmanifest" => Some("application/manifest+json"),
        _ => None,
    }
}

pub(crate) async fn download_s3_prefix(
    client: &S3Client,
    bucket: &str,
    prefix: &str,
    target: &Path,
) -> Result<()> {
    let list_prefix = s3_directory_prefix(prefix);
    let keys = list_s3_keys(client, bucket, &list_prefix).await?;
    for key in keys {
        let relative = key
            .strip_prefix(&list_prefix)
            .unwrap_or(&key)
            .trim_start_matches('/');
        if relative.is_empty() {
            continue;
        }
        let output = target.join(relative);
        if let Some(parent) = output.parent() {
            tokio::fs::create_dir_all(parent)
                .await
                .with_context(|| format!("Failed to create {}", parent.display()))?;
        }
        let bytes = get_s3_object_bytes(client, bucket, &key).await?;
        tokio::fs::write(&output, bytes)
            .await
            .with_context(|| format!("Failed to write {}", output.display()))?;
        println!("Downloaded s3://{bucket}/{key} -> {}", output.display());
    }
    Ok(())
}

pub(crate) async fn list_s3_keys(
    client: &S3Client,
    bucket: &str,
    prefix: &str,
) -> Result<Vec<String>> {
    Ok(list_s3_objects(client, bucket, prefix)
        .await?
        .into_iter()
        .map(|object| object.key)
        .collect())
}

pub(crate) async fn delete_s3_objects(
    client: &S3Client,
    bucket: &str,
    keys: &[String],
) -> Result<usize> {
    let mut deleted = 0_usize;
    for key in keys {
        delete_s3_object(client, bucket, key).await?;
        deleted += 1;
    }
    Ok(deleted)
}

async fn delete_s3_object(client: &S3Client, bucket: &str, key: &str) -> Result<()> {
    println!("Deleting s3://{bucket}/{key}");
    let attempts = s3_retry_attempts();
    for attempt in 1..=attempts {
        match client.delete_object().bucket(bucket).key(key).send().await {
            Ok(_) => return Ok(()),
            Err(error) => {
                let code = error
                    .as_service_error()
                    .and_then(|error| error.code())
                    .map(ToOwned::to_owned);
                let status = error
                    .raw_response()
                    .map(|response| response.status().as_u16());
                if is_retryable_s3_error(code.as_deref(), status) && attempt < attempts {
                    sleep_before_s3_retry(
                        "delete",
                        &format!("s3://{bucket}/{key}"),
                        attempt,
                        attempts,
                        code.as_deref(),
                        status,
                    )
                    .await;
                    continue;
                }
                let summary = s3_error_summary(code.as_deref(), status);
                return Err(error)
                    .with_context(|| format!("Failed to delete s3://{bucket}/{key}{summary}"));
            }
        }
    }
    unreachable!("S3 retry attempts are always greater than zero")
}

pub(crate) async fn replace_s3_object_metadata(
    client: &S3Client,
    bucket: &str,
    key: &str,
    content_type: Option<&str>,
    cache_control: Option<&str>,
) -> Result<()> {
    println!("Replacing metadata for s3://{bucket}/{key}");
    let attempts = s3_retry_attempts();
    for attempt in 1..=attempts {
        let mut request = client
            .copy_object()
            .bucket(bucket)
            .key(key)
            .copy_source(s3_copy_source(bucket, key))
            .metadata_directive(MetadataDirective::Replace);
        if let Some(content_type) = content_type {
            request = request.content_type(content_type);
        }
        if let Some(cache_control) = cache_control {
            request = request.cache_control(cache_control);
        }
        match request.send().await {
            Ok(_) => return Ok(()),
            Err(error) => {
                let code = error
                    .as_service_error()
                    .and_then(|error| error.code())
                    .map(ToOwned::to_owned);
                let status = error
                    .raw_response()
                    .map(|response| response.status().as_u16());
                if is_retryable_s3_error(code.as_deref(), status) && attempt < attempts {
                    sleep_before_s3_retry(
                        "metadata replace",
                        &format!("s3://{bucket}/{key}"),
                        attempt,
                        attempts,
                        code.as_deref(),
                        status,
                    )
                    .await;
                    continue;
                }
                let summary = s3_error_summary(code.as_deref(), status);
                return Err(error).with_context(|| {
                    format!("Failed to replace metadata for s3://{bucket}/{key}{summary}")
                });
            }
        }
    }
    unreachable!("S3 retry attempts are always greater than zero")
}

fn s3_copy_source(bucket: &str, key: &str) -> String {
    format!("{bucket}/{}", percent_encode_s3_copy_source_key(key))
}

fn percent_encode_s3_copy_source_key(key: &str) -> String {
    let mut encoded = String::with_capacity(key.len());
    for byte in key.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' | b'/' => {
                encoded.push(byte as char)
            }
            _ => encoded.push_str(&format!("%{byte:02X}")),
        }
    }
    encoded
}

async fn list_s3_objects(
    client: &S3Client,
    bucket: &str,
    prefix: &str,
) -> Result<Vec<S3ListedObject>> {
    let mut objects = Vec::new();
    let mut token: Option<String> = None;
    loop {
        let response =
            send_s3_list_objects_v2_page(client, bucket, prefix, token.as_deref()).await?;
        objects.extend(response.contents().iter().filter_map(|object| {
            object.key().map(|key| S3ListedObject {
                key: key.to_string(),
                metadata: S3ObjectMetadata {
                    e_tag: object.e_tag().map(ToOwned::to_owned),
                    size: object.size(),
                },
            })
        }));
        token = response.next_continuation_token().map(ToOwned::to_owned);
        if token.is_none() {
            break;
        }
    }
    Ok(objects)
}

async fn send_s3_list_objects_v2_page(
    client: &S3Client,
    bucket: &str,
    prefix: &str,
    token: Option<&str>,
) -> Result<aws_sdk_s3::operation::list_objects_v2::ListObjectsV2Output> {
    let attempts = s3_retry_attempts();
    for attempt in 1..=attempts {
        let mut request = client.list_objects_v2().bucket(bucket).prefix(prefix);
        if let Some(value) = token {
            request = request.continuation_token(value);
        }
        match request.send().await {
            Ok(response) => return Ok(response),
            Err(error) => {
                let code = error
                    .as_service_error()
                    .and_then(|error| error.code())
                    .map(ToOwned::to_owned);
                let status = error
                    .raw_response()
                    .map(|response| response.status().as_u16());
                if is_retryable_s3_error(code.as_deref(), status) && attempt < attempts {
                    sleep_before_s3_retry(
                        "list",
                        &format!("s3://{bucket}/{prefix}"),
                        attempt,
                        attempts,
                        code.as_deref(),
                        status,
                    )
                    .await;
                    continue;
                }
                let summary = s3_error_summary(code.as_deref(), status);
                return Err(error)
                    .with_context(|| format!("Failed to list s3://{bucket}/{prefix}{summary}"));
            }
        }
    }
    unreachable!("S3 retry attempts are always greater than zero")
}

pub(crate) async fn get_s3_object_bytes(
    client: &S3Client,
    bucket: &str,
    key: &str,
) -> Result<bytes::Bytes> {
    let object = send_s3_get_object(client, bucket, key).await?;
    Ok(object
        .body
        .collect()
        .await
        .with_context(|| format!("Failed to collect s3://{bucket}/{key} body"))?
        .into_bytes())
}

async fn send_s3_get_object(
    client: &S3Client,
    bucket: &str,
    key: &str,
) -> Result<aws_sdk_s3::operation::get_object::GetObjectOutput> {
    let attempts = s3_retry_attempts();
    for attempt in 1..=attempts {
        match client.get_object().bucket(bucket).key(key).send().await {
            Ok(response) => return Ok(response),
            Err(error) => {
                let code = error
                    .as_service_error()
                    .and_then(|error| error.code())
                    .map(ToOwned::to_owned);
                let status = error
                    .raw_response()
                    .map(|response| response.status().as_u16());
                if is_retryable_s3_error(code.as_deref(), status) && attempt < attempts {
                    sleep_before_s3_retry(
                        "read",
                        &format!("s3://{bucket}/{key}"),
                        attempt,
                        attempts,
                        code.as_deref(),
                        status,
                    )
                    .await;
                    continue;
                }
                let summary = s3_error_summary(code.as_deref(), status);
                return Err(error)
                    .with_context(|| format!("Failed to read s3://{bucket}/{key}{summary}"));
            }
        }
    }
    unreachable!("S3 retry attempts are always greater than zero")
}

pub(crate) fn join_s3_key(prefix: &str, child: &str) -> String {
    let prefix = prefix.trim_matches('/');
    let child = child.trim_matches('/');
    match (prefix.is_empty(), child.is_empty()) {
        (true, true) => String::new(),
        (true, false) => child.to_string(),
        (false, true) => prefix.to_string(),
        (false, false) => format!("{prefix}/{child}"),
    }
}

pub(crate) fn s3_directory_prefix(prefix: &str) -> String {
    let prefix = prefix.trim_matches('/');
    if prefix.is_empty() {
        String::new()
    } else {
        format!("{prefix}/")
    }
}

pub(crate) fn path_to_s3_key(path: &Path) -> String {
    path.components()
        .filter_map(|component| match component {
            std::path::Component::Normal(value) => Some(value.to_string_lossy().to_string()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("/")
}

pub(crate) async fn download_file(url: &str, path: &Path) -> Result<()> {
    let bytes = Client::new()
        .get(url)
        .send()
        .await
        .with_context(|| format!("Failed to download {url}"))?
        .error_for_status()
        .with_context(|| format!("Failed to download {url}"))?
        .bytes()
        .await
        .with_context(|| format!("Failed to read response body for {url}"))?;
    if let Some(parent) = path.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    tokio::fs::write(path, bytes)
        .await
        .with_context(|| format!("Failed to write {}", path.display()))
}

pub(crate) fn runner_temp() -> PathBuf {
    env::var("RUNNER_TEMP")
        .map(PathBuf::from)
        .unwrap_or_else(|_| env::temp_dir())
}

pub(crate) fn require_home() -> Result<PathBuf> {
    env::var("HOME")
        .or_else(|_| env::var("USERPROFILE"))
        .map(PathBuf::from)
        .context("HOME or USERPROFILE is required")
}

pub(crate) fn copy_dir_contents(source: &Path, dest: &Path) -> Result<()> {
    for file in collect_files(source)? {
        let relative = file.strip_prefix(source)?;
        let target = dest.join(relative);
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create {}", parent.display()))?;
        }
        fs::copy(&file, &target).with_context(|| {
            format!("Failed to copy {} to {}", file.display(), target.display())
        })?;
    }
    Ok(())
}

pub(crate) fn collect_files(root: &Path) -> Result<Vec<PathBuf>> {
    if !root.exists() {
        return Ok(Vec::new());
    }
    let mut files = WalkDir::new(root)
        .into_iter()
        .collect::<std::result::Result<Vec<_>, _>>()?
        .into_iter()
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| entry.into_path())
        .collect::<Vec<_>>();
    files.sort();
    Ok(files)
}

pub(crate) fn count_files(root: &Path) -> Result<usize> {
    Ok(collect_files(root)?.len())
}

pub(crate) fn count_files_min_depth(root: &Path, min_depth: usize) -> Result<usize> {
    Ok(WalkDir::new(root)
        .min_depth(min_depth)
        .into_iter()
        .collect::<std::result::Result<Vec<_>, _>>()?
        .into_iter()
        .filter(|entry| entry.file_type().is_file())
        .count())
}

pub(crate) fn first_word(value: &str) -> String {
    value
        .split_whitespace()
        .next()
        .unwrap_or_default()
        .to_string()
}

pub(crate) fn title_case(value: &str) -> String {
    let mut chars = value.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dt(year: i32, month: u32, day: u32, hour: u32, minute: u32, second: u32) -> DateTime<Utc> {
        Utc.with_ymd_and_hms(year, month, day, hour, minute, second)
            .single()
            .unwrap()
    }

    #[test]
    fn resolves_generated_calver_from_date_override() {
        let calver_env = CalverEnv {
            fluxer_build_date: Some("2026-05-20T01:02:03Z".to_string()),
            ..CalverEnv::default()
        };
        assert_eq!(
            resolve_calver(&calver_env, dt(2026, 1, 1, 0, 0, 0)).unwrap(),
            "2026.520.10203"
        );
    }

    #[test]
    fn rejects_invalid_explicit_time() {
        assert_eq!(
            parse_version_instant("2026.520.246000")
                .unwrap_err()
                .to_string(),
            "Invalid build version date/time: 2026.520.246000"
        );
    }

    #[test]
    fn s3_key_helpers_are_platform_neutral() {
        assert_eq!(
            join_s3_key("/desktop/", "/canary/linux/"),
            "desktop/canary/linux"
        );
        assert_eq!(
            s3_directory_prefix("/_handoff/desktop/build/"),
            "_handoff/desktop/build/"
        );
        assert_eq!(
            path_to_s3_key(Path::new("assets").join("chunks").join("a.js").as_path()),
            "assets/chunks/a.js"
        );
    }

    #[test]
    fn append_only_plan_skips_existing_keys_without_reordering_pending_uploads() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();
        fs::write(root.join("a"), "a").unwrap();
        fs::write(root.join("b"), "b").unwrap();
        fs::write(root.join("c"), "c").unwrap();
        let plan = vec![
            S3UploadPlanItem::new(root.join("a"), "desktop/a".to_string()),
            S3UploadPlanItem::new(root.join("b"), "desktop/b".to_string())
                .with_content_type("text/plain; charset=utf-8")
                .repair_existing_metadata(),
            S3UploadPlanItem::new(root.join("c"), "desktop/c".to_string()),
        ];
        let b_identity = s3_file_identity(&root.join("b")).unwrap();
        let existing = BTreeMap::from([(
            "desktop/b".to_string(),
            S3ObjectMetadata {
                e_tag: Some(format!("\"{}\"", b_identity.md5_hex)),
                size: Some(b_identity.size as i64),
            },
        )]);

        let (pending, skipped) = split_append_only_upload_plan("bucket", plan, &existing).unwrap();

        assert_eq!(
            pending.into_iter().map(|item| item.key).collect::<Vec<_>>(),
            vec!["desktop/a", "desktop/c"]
        );
        assert_eq!(
            skipped
                .iter()
                .map(|item| item.key.as_str())
                .collect::<Vec<_>>(),
            vec!["desktop/b"]
        );
        assert_eq!(
            skipped[0].content_type.as_deref(),
            Some("text/plain; charset=utf-8")
        );
        assert!(skipped[0].repair_existing_metadata);
    }

    #[test]
    fn append_only_plan_rejects_existing_key_with_different_checksum() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("asset.svg");
        fs::write(&path, "local").unwrap();
        let plan = vec![S3UploadPlanItem::new(path, "emoji/asset.svg".to_string())];
        let existing = BTreeMap::from([(
            "emoji/asset.svg".to_string(),
            S3ObjectMetadata {
                e_tag: Some("\"00000000000000000000000000000000\"".to_string()),
                size: Some(5),
            },
        )]);

        let error = split_append_only_upload_plan("bucket", plan, &existing).unwrap_err();

        assert!(error.to_string().contains("differs from local file"));
    }

    #[test]
    fn append_only_plan_rejects_existing_key_without_comparable_etag() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("asset.bin");
        fs::write(&path, "local").unwrap();
        let plan = vec![S3UploadPlanItem::new(path, "assets/asset.bin".to_string())];
        let existing = BTreeMap::from([(
            "assets/asset.bin".to_string(),
            S3ObjectMetadata {
                e_tag: Some("\"multipart-etag-2\"".to_string()),
                size: Some(5),
            },
        )]);

        let error = split_append_only_upload_plan("bucket", plan, &existing).unwrap_err();

        assert!(error.to_string().contains("not a single-part MD5"));
    }

    #[test]
    fn append_only_plan_rejects_duplicate_local_keys() {
        let plan = vec![
            S3UploadPlanItem::new(PathBuf::from("a"), "same/key".to_string()),
            S3UploadPlanItem::new(PathBuf::from("b"), "same/key".to_string()),
        ];

        assert_eq!(
            split_append_only_upload_plan("bucket", plan, &BTreeMap::new())
                .unwrap_err()
                .to_string(),
            "Duplicate S3 upload key in local plan: same/key"
        );
    }

    #[test]
    fn s3_conditional_put_errors_are_treated_as_existing_objects() {
        assert!(is_existing_object_error(Some("PreconditionFailed"), None));
        assert!(is_existing_object_error(
            Some("ConditionalRequestConflict"),
            None
        ));
        assert!(is_existing_object_error(Some("Error"), Some(412)));
        assert!(is_existing_object_error(None, Some(409)));
        assert!(!is_existing_object_error(Some("AccessDenied"), Some(403)));
        assert!(!is_existing_object_error(None, None));
    }

    #[test]
    fn s3_retryable_errors_include_unmodeled_429s() {
        assert!(is_retryable_s3_error(Some("Error"), Some(429)));
        assert!(is_retryable_s3_error(Some("TooManyRequests"), None));
        assert!(is_retryable_s3_error(Some("RateLimitExceeded"), None));
        assert!(!is_retryable_s3_error(None, Some(503)));
        assert!(!is_retryable_s3_error(Some("AccessDenied"), Some(403)));
        assert!(!is_retryable_s3_error(None, None));
    }

    #[test]
    fn s3_etag_md5_accepts_only_single_part_md5_values() {
        assert_eq!(
            s3_etag_md5("\"D41D8CD98F00B204E9800998ECF8427E\"").as_deref(),
            Some("d41d8cd98f00b204e9800998ecf8427e")
        );
        assert!(s3_etag_md5("\"d41d8cd98f00b204e9800998ecf8427e-2\"").is_none());
        assert!(s3_etag_md5("\"not-md5\"").is_none());
    }

    #[test]
    fn s3_list_prefixes_group_planned_keys_by_top_level_prefix() {
        let plan = vec![
            S3UploadPlanItem::new(PathBuf::from("index.html"), "index.html".to_string()),
            S3UploadPlanItem::new(PathBuf::from("emoji/a.svg"), "emoji/a.svg".to_string()),
            S3UploadPlanItem::new(
                PathBuf::from("emoji/nested/b.svg"),
                "emoji/nested/b.svg".to_string(),
            ),
            S3UploadPlanItem::new(PathBuf::from("assets/app.js"), "assets/app.js".to_string()),
        ];

        assert_eq!(
            s3_list_prefixes_for_plan(&plan),
            BTreeSet::from([
                "assets/".to_string(),
                "emoji/".to_string(),
                "index.html".to_string()
            ])
        );
    }

    #[test]
    fn s3_error_summary_includes_available_metadata() {
        assert_eq!(
            s3_error_summary(Some("Error"), Some(404)),
            " (S3 code Error, HTTP status 404)"
        );
        assert_eq!(s3_error_summary(None, Some(500)), " (HTTP status 500)");
    }

    #[test]
    fn directory_upload_plan_filters_and_prefixes_keys() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();
        fs::create_dir_all(root.join("nested")).unwrap();
        fs::write(root.join("keep.txt"), "keep").unwrap();
        fs::write(root.join("nested").join("skip.map"), "skip").unwrap();
        fs::write(root.join("nested").join("keep.js"), "keep").unwrap();

        let plan = directory_upload_plan("static", root, |relative| {
            relative.extension().and_then(OsStr::to_str) != Some("map")
        })
        .unwrap();

        assert_eq!(
            plan.iter()
                .map(|item| item.key.as_str())
                .collect::<Vec<_>>(),
            vec!["static/keep.txt", "static/nested/keep.js"]
        );
        assert_eq!(
            plan[0].content_type.as_deref(),
            Some("text/plain; charset=utf-8")
        );
        assert_eq!(
            plan[1].content_type.as_deref(),
            Some("application/javascript; charset=utf-8")
        );
    }

    #[test]
    fn s3_content_type_for_key_covers_browser_module_assets() {
        assert_eq!(
            s3_content_type_for_key("assets/app.js"),
            Some("application/javascript; charset=utf-8")
        );
        assert_eq!(
            s3_content_type_for_key("assets/app.mjs"),
            Some("application/javascript; charset=utf-8")
        );
        assert_eq!(
            s3_content_type_for_key("assets/voice_bg.wasm"),
            Some("application/wasm")
        );
        assert_eq!(
            s3_content_type_for_key("assets/app.css"),
            Some("text/css; charset=utf-8")
        );
        assert_eq!(s3_content_type_for_key("assets/blob.unknown"), None);
    }

    #[test]
    fn s3_copy_source_percent_encodes_key_bytes_but_keeps_slashes() {
        assert_eq!(
            s3_copy_source("bucket", "assets/chunk name+1.js"),
            "bucket/assets/chunk%20name%2B1.js"
        );
    }

    #[test]
    fn collect_and_copy_dir_contents_are_sorted_and_recursive() {
        let temp = tempfile::tempdir().unwrap();
        let source = temp.path().join("source");
        let dest = temp.path().join("dest");
        fs::create_dir_all(source.join("b")).unwrap();
        fs::write(source.join("b").join("two.txt"), "2").unwrap();
        fs::write(source.join("one.txt"), "1").unwrap();

        let relative_files = collect_files(&source)
            .unwrap()
            .into_iter()
            .map(|path| path_to_s3_key(path.strip_prefix(&source).unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(relative_files, vec!["b/two.txt", "one.txt"]);

        copy_dir_contents(&source, &dest).unwrap();
        assert_eq!(fs::read_to_string(dest.join("one.txt")).unwrap(), "1");
        assert_eq!(
            fs::read_to_string(dest.join("b").join("two.txt")).unwrap(),
            "2"
        );
    }
}
