// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::env::merge_default_env_with_current;
use crate::paths::{DEV_ENV_FILE, DEV_LOCAL_ENV_FILE, ROOT, ROOT_LOCAL_ENV_FILE};
use anyhow::{Context, Result, bail};
#[cfg(target_os = "linux")]
use std::collections::BTreeSet;
use std::collections::{BTreeMap, VecDeque};
#[cfg(target_os = "linux")]
use std::fs;
use std::future::Future;
use std::io::{BufRead, BufReader, Read};
use std::net::{SocketAddr, TcpListener, TcpStream, ToSocketAddrs};
use std::path::Path;
use std::process::{Child, Command, Output, Stdio};
use std::time::{Duration, Instant};
use tokio::time::sleep;

pub const PNPM_INSTALL_ENV: &[(&str, &str)] = &[
    ("CI", "true"),
    ("pnpm_config_child_concurrency", "2"),
    ("pnpm_config_network_concurrency", "8"),
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
    let output = if options.capture {
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
        output
    } else {
        command
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit());
        let status = command
            .status()
            .with_context(|| format!("failed to run {}", format_command(args)))?;
        Output {
            status,
            stdout: Vec::new(),
            stderr: Vec::new(),
        }
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

pub fn can_bind_tcp_port(port: u16) -> bool {
    TcpListener::bind(SocketAddr::from(([0, 0, 0, 0], port))).is_ok()
}

#[cfg(target_os = "linux")]
#[derive(Clone, Debug, Eq, PartialEq)]
struct ProcessGroupLeader {
    pid: i32,
    members: Vec<ProcessIdentity>,
}

#[cfg(target_os = "linux")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ProcessIdentity {
    pid: i32,
    starttime: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProcProcess {
    pub pid: i32,
    pub ppid: i32,
    pub pgid: i32,
    pub state: char,
    pub starttime: u64,
}

#[cfg(target_os = "linux")]
pub async fn cleanup_orphaned_process_groups(
    label: &str,
    is_orphan_root: impl Fn(&ProcProcess, &[String]) -> bool,
    is_supervisor: impl Fn(&[String], &Path) -> bool,
) -> Result<()> {
    let leaders = orphaned_process_leaders(is_orphan_root, is_supervisor)?;
    if leaders.is_empty() {
        return Ok(());
    }
    let pids = leaders.iter().map(|leader| leader.pid).collect::<Vec<_>>();

    println!(
        "Stopping orphaned {label} process group(s): {}",
        format_pids(&pids)
    );
    let term_failed = signal_process_groups(&leaders, libc::SIGTERM);
    if !term_failed.is_empty() {
        println!(
            "SIGTERM delivery failed for orphaned {label} pid(s): {}",
            format_pids(&term_failed)
        );
    }

    let deadline = Instant::now() + Duration::from_secs(5);
    loop {
        let remaining = surviving_group_pids(&leaders)?;
        if remaining.is_empty() {
            return Ok(());
        }
        if Instant::now() >= deadline {
            let remaining_leaders = leaders
                .iter()
                .filter(|leader| remaining.contains(&leader.pid))
                .cloned()
                .collect::<Vec<_>>();
            let kill_failed = signal_process_groups(&remaining_leaders, libc::SIGKILL);
            sleep(Duration::from_millis(200)).await;
            let survivors = surviving_group_pids(&leaders)?;
            if survivors.is_empty() {
                return Ok(());
            }
            let delivery_failed = kill_failed
                .into_iter()
                .filter(|pid| survivors.contains(pid))
                .collect::<Vec<_>>();
            if delivery_failed.is_empty() {
                bail!(
                    "orphaned {label} process(es) survived SIGKILL: {}",
                    format_pids(&survivors)
                );
            }
            bail!(
                "orphaned {label} process(es) survived SIGKILL: {} (signal delivery failed for: {})",
                format_pids(&survivors),
                format_pids(&delivery_failed)
            );
        }
        sleep(Duration::from_millis(200)).await;
    }
}

#[cfg(not(target_os = "linux"))]
pub async fn cleanup_orphaned_process_groups(
    _label: &str,
    _is_orphan_root: impl Fn(&ProcProcess, &[String]) -> bool,
    _is_supervisor: impl Fn(&[String], &Path) -> bool,
) -> Result<()> {
    Ok(())
}

#[cfg(target_os = "linux")]
fn orphaned_process_leaders(
    is_orphan_root: impl Fn(&ProcProcess, &[String]) -> bool,
    is_supervisor: impl Fn(&[String], &Path) -> bool,
) -> Result<Vec<ProcessGroupLeader>> {
    let current_exe = std::env::current_exe().context("failed to resolve fluxer-dev executable")?;
    let processes = process_snapshot()?;
    let mut owned_pids = BTreeSet::new();
    for process in &processes {
        if process.ppid != 1 || proc_stat_state_is_dead(process.state) {
            continue;
        }
        let args = proc_cmdline(process.pid);
        if is_supervisor(&args, &current_exe) || is_orphan_root(process, &args) {
            owned_pids.insert(process.pid);
        }
    }
    loop {
        let previous_count = owned_pids.len();
        for process in &processes {
            if owned_pids.contains(&process.ppid) {
                owned_pids.insert(process.pid);
            }
        }
        if owned_pids.len() == previous_count {
            break;
        }
    }
    let group_pids = processes
        .iter()
        .filter(|process| owned_pids.contains(&process.pid) && process.pgid > 1)
        .map(|process| process.pgid)
        .collect::<BTreeSet<_>>();
    let leaders = group_pids
        .into_iter()
        .map(|pid| ProcessGroupLeader {
            pid,
            members: processes
                .iter()
                .filter(|process| owned_pids.contains(&process.pid) && process.pgid == pid)
                .map(|process| ProcessIdentity {
                    pid: process.pid,
                    starttime: process.starttime,
                })
                .collect(),
        })
        .collect::<Vec<_>>();
    Ok(leaders)
}

#[cfg(target_os = "linux")]
fn format_pids(pids: &[i32]) -> String {
    if pids.is_empty() {
        return "none".to_owned();
    }
    pids.iter()
        .map(i32::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(target_os = "linux")]
fn process_snapshot() -> Result<Vec<ProcProcess>> {
    let mut processes = Vec::new();
    for entry in fs::read_dir("/proc").context("failed to read /proc")? {
        let Ok(entry) = entry else {
            continue;
        };
        let Some(pid) = entry
            .file_name()
            .to_str()
            .and_then(|name| name.parse::<i32>().ok())
        else {
            continue;
        };
        let Ok(stat) = fs::read_to_string(format!("/proc/{pid}/stat")) else {
            continue;
        };
        let Some((state, ppid, pgid, starttime)) = parse_proc_stat_process(&stat) else {
            continue;
        };
        processes.push(ProcProcess {
            pid,
            ppid,
            pgid,
            state,
            starttime,
        });
    }
    Ok(processes)
}

#[cfg(target_os = "linux")]
fn proc_cmdline(pid: i32) -> Vec<String> {
    fs::read(format!("/proc/{pid}/cmdline"))
        .unwrap_or_default()
        .split(|byte| *byte == 0)
        .filter(|arg| !arg.is_empty())
        .map(|arg| String::from_utf8_lossy(arg).into_owned())
        .collect()
}

#[cfg(target_os = "linux")]
fn parse_proc_stat_state_and_pgid(stat: &str) -> Option<(char, i32)> {
    let (_, after_comm) = stat.rsplit_once(')')?;
    let mut fields = after_comm.split_ascii_whitespace();
    let state = fields.next()?.chars().next()?;
    let _ppid: i32 = fields.next()?.parse().ok()?;
    let pgid: i32 = fields.next()?.parse().ok()?;
    Some((state, pgid))
}

#[cfg(target_os = "linux")]
fn parse_proc_stat_process(stat: &str) -> Option<(char, i32, i32, u64)> {
    let (_, after_comm) = stat.rsplit_once(')')?;
    let mut fields = after_comm.split_ascii_whitespace();
    let state = fields.next()?.chars().next()?;
    let ppid = fields.next()?.parse().ok()?;
    let pgid = fields.next()?.parse().ok()?;
    Some((state, ppid, pgid, parse_proc_stat_starttime(stat)?))
}

#[cfg(target_os = "linux")]
fn parse_proc_stat_starttime(stat: &str) -> Option<u64> {
    let (_, after_comm) = stat.rsplit_once(')')?;
    after_comm.split_ascii_whitespace().nth(19)?.parse().ok()
}

#[cfg(target_os = "linux")]
fn proc_stat_state_is_dead(state: char) -> bool {
    state == 'Z' || state == 'X' || state == 'x'
}

#[cfg(target_os = "linux")]
fn surviving_group_pids(leaders: &[ProcessGroupLeader]) -> Result<Vec<i32>> {
    Ok(leaders
        .iter()
        .filter(|leader| group_has_owned_member(leader))
        .map(|leader| leader.pid)
        .collect())
}

#[cfg(target_os = "linux")]
fn group_has_owned_member(leader: &ProcessGroupLeader) -> bool {
    leader.members.iter().any(|member| {
        let Ok(stat) = fs::read_to_string(format!("/proc/{}/stat", member.pid)) else {
            return false;
        };
        let Some((state, pgid)) = parse_proc_stat_state_and_pgid(&stat) else {
            return false;
        };
        !proc_stat_state_is_dead(state)
            && pgid == leader.pid
            && parse_proc_stat_starttime(&stat) == Some(member.starttime)
    })
}

#[cfg(target_os = "linux")]
fn signal_process_groups(leaders: &[ProcessGroupLeader], signal: i32) -> Vec<i32> {
    assert!(signal == libc::SIGTERM || signal == libc::SIGKILL);
    let mut failed = Vec::with_capacity(leaders.len());
    for leader in leaders {
        assert!(leader.pid > 1);
        if !group_has_owned_member(leader) {
            continue;
        }
        let group_result = unsafe { libc::kill(-leader.pid, signal) };
        if group_result == 0 {
            continue;
        }
        if group_has_owned_member(leader) {
            failed.push(leader.pid);
        }
    }
    failed
}
