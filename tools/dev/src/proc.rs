// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::env::merge_default_env_with_current;
use crate::paths::{DEV_ENV_FILE, DEV_LOCAL_ENV_FILE, ROOT, ROOT_LOCAL_ENV_FILE};
use anyhow::{Context, Result, bail};
use std::collections::{BTreeMap, VecDeque};
use std::future::Future;
use std::io::{BufRead, BufReader, Read};
use std::net::{TcpStream, ToSocketAddrs};
use std::path::Path;
use std::process::{Child, Command, Output, Stdio};
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
    let mut command = Command::new(args[0]);
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

pub async fn run_command_interruptible(
    args: &[&str],
    options: RunOptions<'_>,
    shutdown: &mut ShutdownSignal,
) -> Result<AwaitOutcome<Output>> {
    if options.capture {
        bail!("interruptible commands do not support captured output");
    }
    println!("$ {}", format_command(args));
    let env = merged_env(Some(&options.env), options.load_default_env)?;
    let mut command = Command::new(args[0]);
    command
        .args(&args[1..])
        .current_dir(options.cwd)
        .env_clear()
        .envs(env)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());
    configure_process_group(&mut command);
    let mut child = command
        .spawn()
        .with_context(|| format!("failed to run {}", format_command(args)))?;
    loop {
        let status = match child.try_wait() {
            Ok(status) => status,
            Err(error) => {
                stop_process_group(&mut child, Duration::from_secs(5)).await;
                return Err(error)
                    .with_context(|| format!("failed to wait for {}", format_command(args)));
            }
        };
        if let Some(status) = status {
            stop_process_group(&mut child, Duration::from_secs(5)).await;
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
            return Ok(AwaitOutcome::Completed(output));
        }
        tokio::select! {
            signal = shutdown.recv() => {
                stop_process_group(&mut child, Duration::from_secs(5)).await;
                return Ok(AwaitOutcome::Shutdown(signal));
            }
            _ = sleep(Duration::from_millis(100)) => {}
        }
    }
}

pub fn configure_process_group(command: &mut Command) {
    #[cfg(unix)]
    unsafe {
        use std::os::unix::process::CommandExt;
        command.pre_exec(|| {
            if libc::setsid() == -1 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
}

#[cfg(unix)]
pub fn terminate_process_group(process: &mut Child) -> std::io::Result<()> {
    signal_process_group(process, libc::SIGTERM)
}

#[cfg(not(unix))]
pub fn terminate_process_group(process: &mut Child) -> std::io::Result<()> {
    process.kill()
}

#[cfg(unix)]
pub fn force_kill_process_group(process: &mut Child) {
    if let Err(error) = signal_process_group(process, libc::SIGKILL) {
        eprintln!(
            "Failed to send SIGKILL to process group {}: {error}",
            process.id()
        );
    }
    let _ = process.kill();
    let _ = process.wait();
}

#[cfg(unix)]
pub fn process_group_running(process: &mut Child) -> bool {
    if unsafe { libc::kill(-(process.id() as i32), 0) } == 0 {
        return true;
    }
    let error = std::io::Error::last_os_error();
    error.raw_os_error() != Some(libc::ESRCH)
}

#[cfg(not(unix))]
pub fn process_group_running(process: &mut Child) -> bool {
    process.try_wait().ok().flatten().is_none()
}

#[cfg(not(unix))]
pub fn force_kill_process_group(process: &mut Child) {
    let _ = process.kill();
    let _ = process.wait();
}

#[cfg(unix)]
fn signal_process_group(process: &Child, signal: libc::c_int) -> std::io::Result<()> {
    if unsafe { libc::kill(-(process.id() as i32), signal) } == 0 {
        return Ok(());
    }
    let error = std::io::Error::last_os_error();
    if error.raw_os_error() == Some(libc::ESRCH) {
        return Ok(());
    }
    Err(error)
}

pub async fn stop_process_group(process: &mut Child, grace_period: Duration) {
    if let Err(error) = terminate_process_group(process) {
        eprintln!(
            "Failed to terminate process group {}: {error}",
            process.id()
        );
    }
    let deadline = Instant::now() + grace_period;
    loop {
        let leader_exited = process.try_wait().ok().flatten().is_some();
        if leader_exited && !process_group_running(process) {
            return;
        }
        if Instant::now() >= deadline {
            force_kill_process_group(process);
            return;
        }
        sleep(Duration::from_millis(100)).await;
    }
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
    wait_http_status(name, url, timeout_secs, |status| status.as_u16() < 500).await
}

pub async fn wait_http_success(name: &str, url: &str, timeout_secs: u64) -> Result<()> {
    wait_http_status(name, url, timeout_secs, |status| status.is_success()).await
}

async fn wait_http_status(
    name: &str,
    url: &str,
    timeout_secs: u64,
    accepts: impl Fn(reqwest::StatusCode) -> bool,
) -> Result<()> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(5))
        .build()?;
    let deadline = Instant::now() + Duration::from_secs(timeout_secs);
    let mut last_error = None;
    while Instant::now() < deadline {
        match client.get(url).send().await {
            Ok(response) if accepts(response.status()) => {
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

pub fn prefix_output(label: &str, reader: impl Read) {
    let mut reader = BufReader::new(reader);
    let mut line = Vec::new();
    loop {
        line.clear();
        match reader.read_until(b'\n', &mut line) {
            Ok(0) => return,
            Ok(_) => {
                if line.last() == Some(&b'\n') {
                    line.pop();
                }
                if line.last() == Some(&b'\r') {
                    line.pop();
                }
                println!("[{label}] {}", String::from_utf8_lossy(&line));
            }
            Err(error) => {
                eprintln!("[{label}] output reader failed: {error}");
                return;
            }
        }
    }
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

pub enum AwaitOutcome<T> {
    Completed(T),
    Shutdown(&'static str),
}

pub async fn await_or_shutdown<T, F>(shutdown: &mut ShutdownSignal, future: F) -> AwaitOutcome<T>
where
    F: Future<Output = T>,
{
    tokio::select! {
        biased;
        signal = shutdown.recv() => AwaitOutcome::Shutdown(signal),
        output = future => AwaitOutcome::Completed(output),
    }
}

pub async fn pending_shutdown(shutdown: &mut ShutdownSignal) -> Option<&'static str> {
    match await_or_shutdown(shutdown, std::future::ready(())).await {
        AwaitOutcome::Completed(()) => None,
        AwaitOutcome::Shutdown(signal) => Some(signal),
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
