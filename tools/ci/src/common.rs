// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result, anyhow, bail, ensure};
use chrono::{DateTime, Datelike, NaiveDate, NaiveDateTime, TimeZone, Timelike, Utc};
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use walkdir::WalkDir;

pub(crate) use crate::functions::{remove_dir_if_exists, remove_file_if_exists};

pub(crate) const CALVER_SCHEME: &str = "YYYY.MDD.MICRO";

#[derive(Default)]
pub(crate) struct CalverEnv {
    pub(crate) build_version: Option<String>,
    pub(crate) fluxer_build_date: Option<String>,
}

impl CalverEnv {
    pub(crate) fn from_process() -> Self {
        Self {
            build_version: trim_option(env::var("BUILD_VERSION").ok()),
            fluxer_build_date: trim_option(env::var("FLUXER_BUILD_DATE").ok()),
        }
    }
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
    println!("+ {}", display_command(&spec.program, &spec.args));
    let status = spec
        .to_command()
        .status()
        .with_context(|| format!("Failed to run {}", spec.program.to_string_lossy()))?;
    ensure!(
        status.success(),
        "Command failed with exit code {}: {}",
        status.code().unwrap_or(1),
        display_command(&spec.program, &spec.args)
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
            display_command(&spec.program, &spec.args),
            stdout,
            stderr
        );
    }
}

pub(crate) fn capture(spec: CommandSpec) -> Result<CapturedOutput> {
    println!("+ {}", display_command(&spec.program, &spec.args));
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

pub(crate) fn display_command(program: impl AsRef<OsStr>, args: &[impl AsRef<OsStr>]) -> String {
    std::iter::once(program.as_ref().to_string_lossy().to_string())
        .chain(args.iter().map(|arg| quote_arg(arg.as_ref())))
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
    if let Some(version) = calver_env.build_version.as_deref() {
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

pub(crate) fn month_day_segment(instant: DateTime<Utc>) -> String {
    format!("{}{:02}", instant.month(), instant.day())
}

pub(crate) fn micro_segment(instant: DateTime<Utc>) -> String {
    (instant.hour() * 10_000 + instant.minute() * 100 + instant.second()).to_string()
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
    #[cfg(not(feature = "http"))]
    {
        _ = path;
        bail!("Downloading {url} needs the http feature");
    }

    #[cfg(feature = "http")]
    {
        let bytes = reqwest::Client::new()
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
    let mut files = Vec::new();
    for entry in WalkDir::new(root) {
        let entry = entry?;
        if entry.file_type().is_file() {
            files.push(entry.into_path());
        }
    }
    files.sort();
    Ok(files)
}

pub(crate) fn count_files(root: &Path) -> Result<usize> {
    Ok(collect_files(root)?.len())
}

pub(crate) fn remove_empty_dirs_below(root: &Path) -> Result<usize> {
    if !root.exists() {
        return Ok(0);
    }
    let mut dirs = WalkDir::new(root)
        .min_depth(1)
        .into_iter()
        .collect::<std::result::Result<Vec<_>, _>>()?
        .into_iter()
        .filter(|entry| entry.file_type().is_dir())
        .map(|entry| entry.into_path())
        .collect::<Vec<_>>();
    dirs.sort();
    let mut removed = 0usize;
    for dir in dirs.into_iter().rev() {
        let is_empty = fs::read_dir(&dir)
            .with_context(|| format!("Failed to read {}", dir.display()))?
            .next()
            .is_none();
        if is_empty {
            fs::remove_dir(&dir).with_context(|| format!("Failed to remove {}", dir.display()))?;
            removed += 1;
        }
    }
    Ok(removed)
}

pub(crate) fn title_case(value: &str) -> String {
    let mut chars = value.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}
