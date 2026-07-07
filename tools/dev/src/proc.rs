// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::env::merge_default_env_with_current;
use crate::paths::{DEV_ENV_FILE, DEV_LOCAL_ENV_FILE, ROOT, ROOT_LOCAL_ENV_FILE};
use anyhow::{Context, Result, bail};
use std::collections::{BTreeMap, VecDeque};
use std::net::{TcpStream, ToSocketAddrs};
use std::path::Path;
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant};
use tokio::time::sleep;

pub const PNPM_INSTALL_ENV: &[(&str, &str)] = &[
    ("CI", "true"),
    ("npm_config_child_concurrency", "2"),
    ("npm_config_network_concurrency", "8"),
];

pub fn format_command(args: &[impl AsRef<str>]) -> String {
    args.iter()
        .map(|arg| quote_posix(arg.as_ref()))
        .collect::<Vec<_>>()
        .join(" ")
}

fn quote_posix(value: &str) -> String {
    if value.is_empty() {
        return "''".to_owned();
    }
    if value
        .bytes()
        .all(|ch| ch.is_ascii_alphanumeric() || b"._+-/:=@%".contains(&ch))
    {
        return value.to_owned();
    }
    format!("'{}'", value.replace('\'', "'\"'\"'"))
}

pub fn merged_env(
    extra: Option<&[(String, Option<String>)]>,
    load_default_env: bool,
) -> Result<BTreeMap<String, String>> {
    let mut current: BTreeMap<String, String> = std::env::vars().collect();
    if load_default_env {
        current = merge_default_env_with_current(
            DEV_ENV_FILE.as_path(),
            DEV_LOCAL_ENV_FILE.as_path(),
            ROOT_LOCAL_ENV_FILE.as_path(),
            current,
        )?;
    }
    if let Some(extra) = extra {
        for (key, value) in extra {
            match value {
                Some(value) => {
                    current.insert(key.clone(), value.clone());
                }
                None => {
                    current.remove(key);
                }
            }
        }
    }
    if load_default_env {
        current.insert("FLUXER_SELF_HOSTED".to_owned(), "true".to_owned());
    }
    Ok(current)
}

pub fn run(args: &[&str]) -> Result<()> {
    run_command(args, RunOptions::default()).map(drop)
}

pub fn run_with_env(args: &[&str], env: Vec<(String, Option<String>)>) -> Result<()> {
    run_command(
        args,
        RunOptions {
            env,
            ..RunOptions::default()
        },
    )
    .map(drop)
}

pub fn run_capture(
    args: &[&str],
    env: Vec<(String, Option<String>)>,
    check: bool,
) -> Result<Output> {
    run_command(
        args,
        RunOptions {
            env,
            check,
            capture: true,
            ..RunOptions::default()
        },
    )
}

#[derive(Debug)]
pub struct RunOptions<'a> {
    pub cwd: &'a Path,
    pub env: Vec<(String, Option<String>)>,
    pub check: bool,
    pub capture: bool,
    pub load_default_env: bool,
}

impl Default for RunOptions<'_> {
    fn default() -> Self {
        Self {
            cwd: ROOT.as_path(),
            env: Vec::new(),
            check: true,
            capture: false,
            load_default_env: true,
        }
    }
}

pub fn run_command(args: &[&str], options: RunOptions<'_>) -> Result<Output> {
    println!("$ {}", format_command(args));
    let env = merged_env(Some(&options.env), options.load_default_env)?;
    let program = args[0];
    let mut command = if cfg!(windows) && (program == "pnpm" || program == "npm" || program == "corepack") {
        let mut cmd = Command::new("cmd");
        cmd.arg("/c").arg(program);
        cmd
    } else {
        Command::new(program)
    };
    command
        .args(&args[1..])
        .current_dir(options.cwd)
        .env_clear()
        .envs(env);
    if options.capture {
        command.stdout(Stdio::piped()).stderr(Stdio::piped());
        let output = command
            .output()
            .with_context(|| format!("failed to run {}", format_command(args)))?;
        let mut printed = Vec::new();
        printed.extend_from_slice(&output.stdout);
        printed.extend_from_slice(&output.stderr);
        let text = String::from_utf8_lossy(&printed);
        if !text.trim_end().is_empty() {
            println!("{}", text.trim_end());
        }
        if options.check && !output.status.success() {
            let code = output.status.code().unwrap_or(-1);
            bail!(
                "Command failed with exit code {code}: {}",
                format_command(args)
            );
        }
        return Ok(output);
    }

    command
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());
    let status = command
        .status()
        .with_context(|| format!("failed to run {}", format_command(args)))?;
    let output = Output {
        status,
        stdout: Vec::new(),
        stderr: Vec::new(),
    };
    if options.check && !output.status.success() {
        let code = output.status.code().unwrap_or(-1);
        bail!(
            "Command failed with exit code {code}: {}",
            format_command(args)
        );
    }
    Ok(output)
}

pub async fn wait_tcp(name: &str, host: &str, port: u16, timeout_secs: u64) -> Result<()> {
    let deadline = Instant::now() + Duration::from_secs(timeout_secs);
    let mut last_error = None;
    while Instant::now() < deadline {
        let address = format!("{host}:{port}");
        match address
            .to_socket_addrs()
            .ok()
            .and_then(|mut addrs| addrs.next())
            .map(|addr| TcpStream::connect_timeout(&addr, Duration::from_secs(2)))
        {
            Some(Ok(_)) => {
                println!("{name} is reachable at {host}:{port}");
                return Ok(());
            }
            Some(Err(error)) => last_error = Some(error.to_string()),
            None => last_error = Some(format!("failed to resolve {address}")),
        }
        sleep(Duration::from_secs(2)).await;
    }
    bail!(
        "Timed out waiting for {name} at {host}:{port}: {}",
        last_error.unwrap_or_else(|| "unknown error".to_owned())
    );
}

pub async fn wait_http(name: &str, url: &str, timeout_secs: u64) -> Result<()> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(5))
        .build()?;
    let deadline = Instant::now() + Duration::from_secs(timeout_secs);
    let mut last_error = None;
    while Instant::now() < deadline {
        match client.get(url).send().await {
            Ok(response) if response.status().as_u16() < 500 => {
                println!("{name} is reachable at {url}");
                return Ok(());
            }
            Ok(response) => last_error = Some(format!("HTTP {}", response.status())),
            Err(error) => last_error = Some(error.to_string()),
        }
        sleep(Duration::from_secs(2)).await;
    }
    bail!(
        "Timed out waiting for {name} at {url}: {}",
        last_error.unwrap_or_else(|| "unknown error".to_owned())
    );
}

pub const RESTART_WINDOW: Duration = Duration::from_secs(60);
pub const RESTART_LIMIT: usize = 5;

pub fn restart_budget_exceeded(restarts: &mut VecDeque<Instant>, now: Instant) -> bool {
    while restarts
        .front()
        .is_some_and(|at| now.duration_since(*at) > RESTART_WINDOW)
    {
        restarts.pop_front();
    }
    if restarts.len() >= RESTART_LIMIT {
        return true;
    }
    restarts.push_back(now);
    false
}

#[cfg(unix)]
pub struct ShutdownSignal {
    interrupt: tokio::signal::unix::Signal,
    terminate: tokio::signal::unix::Signal,
}

#[cfg(unix)]
impl ShutdownSignal {
    pub fn new() -> Result<Self> {
        use tokio::signal::unix::{SignalKind, signal};
        Ok(Self {
            interrupt: signal(SignalKind::interrupt())?,
            terminate: signal(SignalKind::terminate())?,
        })
    }

    pub async fn recv(&mut self) -> &'static str {
        tokio::select! {
            _ = self.interrupt.recv() => "SIGINT",
            _ = self.terminate.recv() => "SIGTERM",
        }
    }
}

#[cfg(not(unix))]
pub struct ShutdownSignal;

#[cfg(not(unix))]
impl ShutdownSignal {
    pub fn new() -> Result<Self> {
        Ok(Self)
    }

    pub async fn recv(&mut self) -> &'static str {
        let _ = tokio::signal::ctrl_c().await;
        "signal"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_posix_commands_for_logs() {
        assert_eq!(format_command(&["pnpm", "build"]), "pnpm build");
        assert_eq!(
            format_command(&["", "two words", "a'b"]),
            "'' 'two words' 'a'\"'\"'b'"
        );
    }

    #[test]
    fn restart_budget_allows_limited_restarts_within_window() {
        let mut restarts = VecDeque::new();
        let base = Instant::now();
        for offset in 0..RESTART_LIMIT {
            assert!(!restart_budget_exceeded(
                &mut restarts,
                base + Duration::from_secs(offset as u64)
            ));
        }
        assert!(restart_budget_exceeded(
            &mut restarts,
            base + Duration::from_secs(RESTART_LIMIT as u64)
        ));
    }

    #[test]
    fn restart_budget_resets_after_window_elapses() {
        let mut restarts = VecDeque::new();
        let base = Instant::now();
        for _ in 0..RESTART_LIMIT {
            assert!(!restart_budget_exceeded(&mut restarts, base));
        }
        assert!(!restart_budget_exceeded(
            &mut restarts,
            base + RESTART_WINDOW + Duration::from_secs(1)
        ));
    }
}
