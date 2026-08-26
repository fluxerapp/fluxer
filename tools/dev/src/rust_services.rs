// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::manifest::{DEV_PROXY_PORT, MEDIA_PROXY_PORT, RustServiceSpec, rust_services};
use crate::paths::{ROOT, TARGET_DIR};
use crate::proc::{
    AwaitOutcome, RESTART_LIMIT, RESTART_WINDOW, RunOptions, ShutdownSignal, await_or_shutdown,
    configure_process_group, format_command, merged_env, pending_shutdown, prefix_output,
    restart_budget_exceeded, run_command_interruptible,
};
use anyhow::{Result, bail};
use std::collections::{BTreeSet, VecDeque};
use std::env;
#[cfg(target_os = "linux")]
use std::fs;
use std::net::{SocketAddr, TcpListener};
#[cfg(target_os = "linux")]
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};
use tokio::time::sleep;

const SERVICE_RESTART_PORT_WAIT: Duration = Duration::from_secs(75);

struct SupervisedService {
    spec: RustServiceSpec,
    mode: &'static str,
    port: u16,
    child: Child,
    restarts: VecDeque<Instant>,
}

impl SupervisedService {
    fn label(&self) -> String {
        format!("{}:{}", self.spec.name, self.mode)
    }
}

pub async fn run_rust_services(service_names: &[String]) -> Result<i32> {
    let selected = select_services(service_names)?;
    let mut shutdown = ShutdownSignal::new()?;
    cleanup_orphaned_service_processes(&selected).await?;
    if let Some(signal) = pending_shutdown(&mut shutdown).await {
        println!("Received {signal}; stopping Rust service startup...");
        return Ok(0);
    }
    wait_for_service_ports_available(&selected)?;
    match build_services(&selected, &mut shutdown).await? {
        AwaitOutcome::Completed(_) => {}
        AwaitOutcome::Shutdown(signal) => {
            println!("Received {signal}; stopping Rust service startup...");
            return Ok(0);
        }
    }
    let mut supervised = Vec::new();
    for spec in &selected {
        for (mode, port) in [("router", spec.port_base), ("shard", spec.port_base + 1)] {
            let child = match start_service(spec, mode, port) {
                Ok(child) => child,
                Err(error) => {
                    stop_supervised_services(&mut supervised);
                    return Err(error);
                }
            };
            supervised.push(SupervisedService {
                spec: spec.clone(),
                mode,
                port,
                child,
                restarts: VecDeque::new(),
            });
        }
    }
    loop {
        match await_or_shutdown(&mut shutdown, restart_exited_services(&mut supervised)).await {
            AwaitOutcome::Completed(Ok(())) => {}
            AwaitOutcome::Completed(Err(error)) => {
                stop_supervised_services(&mut supervised);
                return Err(error);
            }
            AwaitOutcome::Shutdown(signal) => {
                println!("Received {signal}; stopping Rust service tasks...");
                stop_supervised_services(&mut supervised);
                return Ok(0);
            }
        }
        tokio::select! {
            signal = shutdown.recv() => {
                println!("Received {signal}; stopping Rust service tasks...");
                stop_supervised_services(&mut supervised);
                return Ok(0);
            }
            _ = sleep(Duration::from_millis(500)) => {}
        }
    }
}

async fn restart_exited_services(supervised: &mut [SupervisedService]) -> Result<()> {
    for entry in supervised.iter_mut() {
        let Some(status) = entry.child.try_wait()? else {
            continue;
        };
        crate::gateway::stop_child_processes(&mut [&mut entry.child]);
        if restart_budget_exceeded(&mut entry.restarts, Instant::now()) {
            bail!(
                "Rust service {} exited with {status} after {RESTART_LIMIT} restarts within {}s; giving up",
                entry.label(),
                RESTART_WINDOW.as_secs()
            );
        }
        println!(
            "[{}] exited with {status}; restarting service",
            entry.label()
        );
        wait_for_port_available(&entry.label(), entry.port).await?;
        entry.child = start_service(&entry.spec, entry.mode, entry.port)?;
    }
    Ok(())
}

async fn wait_for_port_available(label: &str, port: u16) -> Result<()> {
    let deadline = Instant::now() + SERVICE_RESTART_PORT_WAIT;
    loop {
        if can_bind_tcp_port(port) {
            return Ok(());
        }
        if Instant::now() >= deadline {
            bail!("port {port} for Rust service {label} did not become available");
        }
        sleep(Duration::from_millis(500)).await;
    }
}

fn stop_supervised_services(supervised: &mut [SupervisedService]) {
    let mut children = supervised
        .iter_mut()
        .map(|entry| &mut entry.child)
        .collect::<Vec<_>>();
    let mut processes = Vec::new();
    for child in children.drain(..) {
        processes.push(child);
    }
    crate::gateway::stop_child_processes(&mut processes);
}

pub fn select_services(service_names: &[String]) -> Result<Vec<RustServiceSpec>> {
    let services = rust_services();
    if service_names.is_empty() {
        return Ok(services);
    }
    let unknown = service_names
        .iter()
        .filter(|name| !services.iter().any(|spec| spec.name == name.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    if !unknown.is_empty() {
        let available = services
            .iter()
            .map(|spec| spec.name)
            .collect::<Vec<_>>()
            .join(", ");
        bail!(
            "Unknown Rust service(s): {}. Available: {available}",
            unknown.join(", ")
        );
    }
    Ok(service_names
        .iter()
        .filter_map(|name| {
            services
                .iter()
                .find(|spec| spec.name == name.as_str())
                .cloned()
        })
        .collect())
}

async fn build_services(
    services: &[RustServiceSpec],
    shutdown: &mut ShutdownSignal,
) -> Result<AwaitOutcome<std::process::Output>> {
    let mut packages = Vec::new();
    let mut seen = BTreeSet::new();
    for spec in services {
        if seen.insert(spec.package) {
            packages.extend(["-p".to_owned(), spec.package.to_owned()]);
        }
    }
    let mut args = vec!["cargo".to_owned(), "build".to_owned()];
    args.extend(packages);
    let refs = args.iter().map(String::as_str).collect::<Vec<_>>();
    let env = if env::var_os("CARGO_BUILD_JOBS").is_none() {
        vec![(
            "CARGO_BUILD_JOBS".to_owned(),
            Some(env::var("FLUXER_DEV_CARGO_JOBS").unwrap_or_else(|_| "2".to_owned())),
        )]
    } else {
        Vec::new()
    };
    run_command_interruptible(
        &refs,
        RunOptions {
            env,
            ..RunOptions::default()
        },
        shutdown,
    )
    .await
}

fn start_service(spec: &RustServiceSpec, mode: &str, port: u16) -> Result<Child> {
    let args = service_command(spec);
    let env = merged_env(Some(&service_env(spec, mode, port)), true)?;
    let label = format!("{}:{mode}", spec.name);
    println!("[{label}] $ {}", format_command(&args));
    let mut command = Command::new(&args[0]);
    command
        .args(&args[1..])
        .current_dir(ROOT.as_path())
        .env_clear()
        .envs(env)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    configure_process_group(&mut command);
    let mut child = command.spawn()?;
    if let Some(stdout) = child.stdout.take() {
        let stdout_label = label.clone();
        std::thread::spawn(move || prefix_output(&stdout_label, stdout));
    }
    if let Some(stderr) = child.stderr.take() {
        std::thread::spawn(move || prefix_output(&label, stderr));
    }
    Ok(child)
}

pub fn service_command(spec: &RustServiceSpec) -> Vec<String> {
    if env::var("FLUXER_DEV_RUST_HOT_RELOAD")
        .map(|value| matches!(value.to_ascii_lowercase().as_str(), "1" | "true" | "yes"))
        .unwrap_or(false)
    {
        let rel = spec.path.strip_prefix(ROOT.as_path()).unwrap_or(&spec.path);
        return vec![
            "cargo".to_owned(),
            "watch".to_owned(),
            "-w".to_owned(),
            rel.join("src").display().to_string(),
            "-w".to_owned(),
            rel.join("Cargo.toml").display().to_string(),
            "-w".to_owned(),
            "fluxer_svc/src".to_owned(),
            "-w".to_owned(),
            "fluxer_svc/Cargo.toml".to_owned(),
            "-x".to_owned(),
            format!("run -p {}", spec.package),
        ];
    }
    let target_dir = env::var_os("CARGO_TARGET_DIR")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| TARGET_DIR.clone());
    let target_dir = if target_dir.is_absolute() {
        target_dir
    } else {
        ROOT.join(target_dir)
    };
    vec![
        target_dir
            .join("debug")
            .join(format!("{}{}", spec.package, std::env::consts::EXE_SUFFIX))
            .display()
            .to_string(),
    ]
}

pub fn service_env(spec: &RustServiceSpec, mode: &str, port: u16) -> Vec<(String, Option<String>)> {
    let mut envs = vec![
        ("FLUXER_SVC_NAME".to_owned(), Some(spec.name.to_owned())),
        ("FLUXER_SVC_MODE".to_owned(), Some(mode.to_owned())),
        ("FLUXER_SVC_PORT".to_owned(), Some(port.to_string())),
        (
            "FLUXER_SVC_LISTEN_HOST".to_owned(),
            Some("0.0.0.0".to_owned()),
        ),
        ("FLUXER_SVC_SHARD_COUNT".to_owned(), Some("1".to_owned())),
        (
            "FLUXER_SVC_NATS_URL".to_owned(),
            Some(
                env::var("FLUXER_SVC_NATS_URL")
                    .or_else(|_| env::var("FLUXER_NATS_URL"))
                    .unwrap_or_else(|_| "nats://nats:4222".to_owned()),
            ),
        ),
    ];
    if mode == "shard" {
        envs.push(("FLUXER_SVC_SHARD_ID".to_owned(), Some("0".to_owned())));
    }
    if spec.name == "unfurl" {
        envs.extend([
            (
                "FLUXER_MEDIA_PROXY_ENDPOINT".to_owned(),
                Some(
                    env::var("FLUXER_MEDIA_PROXY_ENDPOINT")
                        .unwrap_or_else(|_| format!("http://127.0.0.1:{MEDIA_PROXY_PORT}")),
                ),
            ),
            (
                "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT".to_owned(),
                Some(
                    env::var("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT")
                        .or_else(|_| env::var("FLUXER_MEDIA_ENDPOINT"))
                        .unwrap_or_else(|_| format!("http://localhost:{DEV_PROXY_PORT}/media")),
                ),
            ),
            (
                "FLUXER_STATIC_CDN_ENDPOINT".to_owned(),
                Some(
                    env::var("FLUXER_STATIC_CDN_ENDPOINT")
                        .or_else(|_| env::var("FLUXER_PUBLIC_URL"))
                        .unwrap_or_else(|_| format!("http://localhost:{DEV_PROXY_PORT}")),
                ),
            ),
        ]);
    }
    envs
}

fn wait_for_service_ports_available(services: &[RustServiceSpec]) -> Result<()> {
    let conflicts = services
        .iter()
        .flat_map(|spec| {
            [
                (
                    !can_bind_tcp_port(spec.port_base),
                    format!("{}:router={}", spec.name, spec.port_base),
                ),
                (
                    !can_bind_tcp_port(spec.port_base + 1),
                    format!("{}:shard={}", spec.name, spec.port_base + 1),
                ),
            ]
        })
        .filter_map(|(conflict, label)| conflict.then_some(label))
        .collect::<Vec<_>>();
    if !conflicts.is_empty() {
        bail!(
            "Rust service port(s) already in use: {}. Stop the conflicting process before starting rust-services.",
            conflicts.join(", ")
        );
    }
    Ok(())
}

fn can_bind_tcp_port(port: u16) -> bool {
    TcpListener::bind(SocketAddr::from(([0, 0, 0, 0], port))).is_ok()
}

#[cfg(target_os = "linux")]
async fn cleanup_orphaned_service_processes(services: &[RustServiceSpec]) -> Result<()> {
    let leaders = orphaned_service_leaders(services)?;
    if leaders.is_empty() {
        return Ok(());
    }
    let pids = leaders.iter().map(|leader| leader.pid).collect::<Vec<_>>();

    println!(
        "Stopping orphaned Rust service process group(s): {}",
        format_pids(&pids)
    );
    let term_failed = signal_process_groups(&leaders, libc::SIGTERM);
    if !term_failed.is_empty() {
        println!(
            "SIGTERM delivery failed for orphaned Rust service pid(s): {}",
            format_pids(&term_failed)
        );
    }

    let deadline = Instant::now() + Duration::from_secs(5);
    loop {
        let remaining = surviving_service_group_pids(&leaders)?;
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
            let survivors = surviving_service_group_pids(&leaders)?;
            if survivors.is_empty() {
                return Ok(());
            }
            let delivery_failed = kill_failed
                .into_iter()
                .filter(|pid| survivors.contains(pid))
                .collect::<Vec<_>>();
            if delivery_failed.is_empty() {
                bail!(
                    "orphaned Rust service process(es) survived SIGKILL: {}",
                    format_pids(&survivors)
                );
            }
            bail!(
                "orphaned Rust service process(es) survived SIGKILL: {} (signal delivery failed for: {})",
                format_pids(&survivors),
                format_pids(&delivery_failed)
            );
        }
        sleep(Duration::from_millis(200)).await;
    }
}

#[cfg(not(target_os = "linux"))]
async fn cleanup_orphaned_service_processes(_services: &[RustServiceSpec]) -> Result<()> {
    Ok(())
}

#[cfg(target_os = "linux")]
#[derive(Clone, Debug, Eq, PartialEq)]
struct RustServiceLeader {
    pid: i32,
    members: Vec<RustServiceProcessIdentity>,
}

#[cfg(target_os = "linux")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct RustServiceProcessIdentity {
    pid: i32,
    starttime: u64,
}

#[cfg(target_os = "linux")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct RustServiceProcess {
    pid: i32,
    ppid: i32,
    pgid: i32,
    state: char,
    starttime: u64,
}

#[cfg(target_os = "linux")]
fn orphaned_service_leaders(services: &[RustServiceSpec]) -> Result<Vec<RustServiceLeader>> {
    let current_exe = std::env::current_exe()?;
    let processes = service_process_snapshot()?;
    let mut owned_pids = BTreeSet::new();
    for process in &processes {
        if process.ppid != 1 || proc_stat_state_is_dead(process.state) {
            continue;
        }
        let args = proc_cmdline(process.pid);
        let orphaned_supervisor = cmdline_is_service_supervisor(&args, &current_exe);
        let orphaned_service = proc_has_managed_service_environment(process.pid, services)
            && fs::read_link(format!("/proc/{}/cwd", process.pid))
                .ok()
                .as_deref()
                == Some(ROOT.as_path());
        if orphaned_supervisor || orphaned_service {
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
        .map(|pid| RustServiceLeader {
            pid,
            members: processes
                .iter()
                .filter(|process| owned_pids.contains(&process.pid) && process.pgid == pid)
                .map(|process| RustServiceProcessIdentity {
                    pid: process.pid,
                    starttime: process.starttime,
                })
                .collect(),
        })
        .collect::<Vec<_>>();
    Ok(leaders)
}

#[cfg(target_os = "linux")]
fn service_process_snapshot() -> Result<Vec<RustServiceProcess>> {
    let mut processes = Vec::new();
    for entry in fs::read_dir("/proc")? {
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
        processes.push(RustServiceProcess {
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
fn cmdline_is_service_supervisor(args: &[String], current_exe: &Path) -> bool {
    args.first().map(Path::new) == Some(current_exe)
        && args.get(1).map(String::as_str) == Some("rust-services")
}

#[cfg(target_os = "linux")]
fn proc_has_managed_service_environment(pid: i32, services: &[RustServiceSpec]) -> bool {
    let environment_bytes = fs::read(format!("/proc/{pid}/environ")).unwrap_or_default();
    let environment = environment_bytes
        .split(|byte| *byte == 0)
        .filter_map(|entry| {
            let separator = entry.iter().position(|byte| *byte == b'=')?;
            let (key, value) = entry.split_at(separator);
            Some((key, &value[1..]))
        })
        .filter_map(|(key, value)| {
            Some((
                std::str::from_utf8(key).ok()?,
                std::str::from_utf8(value).ok()?,
            ))
        })
        .collect::<std::collections::HashMap<_, _>>();
    let Some(name) = environment.get("FLUXER_SVC_NAME") else {
        return false;
    };
    let Some(mode) = environment.get("FLUXER_SVC_MODE") else {
        return false;
    };
    let Some(port) = environment
        .get("FLUXER_SVC_PORT")
        .and_then(|port| port.parse::<u16>().ok())
    else {
        return false;
    };
    if environment.get("FLUXER_SVC_LISTEN_HOST") != Some(&"0.0.0.0") {
        return false;
    }
    services.iter().any(|service| {
        service.name == *name
            && match *mode {
                "router" => port == service.port_base,
                "shard" => port == service.port_base + 1,
                _ => false,
            }
    })
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

#[cfg(test)]
fn cmdline_has_service_binary(args: &[String], binaries: &BTreeSet<String>) -> bool {
    args.first()
        .map(|arg| binaries.iter().any(|binary| arg.ends_with(binary)))
        .unwrap_or(false)
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

#[cfg(any(target_os = "linux", test))]
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

#[cfg(any(target_os = "linux", test))]
fn parse_proc_stat_starttime(stat: &str) -> Option<u64> {
    let (_, after_comm) = stat.rsplit_once(')')?;
    after_comm.split_ascii_whitespace().nth(19)?.parse().ok()
}

#[cfg(any(target_os = "linux", test))]
fn proc_stat_state_is_dead(state: char) -> bool {
    state == 'Z' || state == 'X' || state == 'x'
}

#[cfg(target_os = "linux")]
fn surviving_service_group_pids(leaders: &[RustServiceLeader]) -> Result<Vec<i32>> {
    Ok(leaders
        .iter()
        .filter(|leader| service_group_has_owned_member(leader))
        .map(|leader| leader.pid)
        .collect())
}

#[cfg(target_os = "linux")]
fn service_group_has_owned_member(leader: &RustServiceLeader) -> bool {
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
fn signal_process_groups(leaders: &[RustServiceLeader], signal: i32) -> Vec<i32> {
    assert!(signal == libc::SIGTERM || signal == libc::SIGKILL);
    let mut failed = Vec::with_capacity(leaders.len());
    for leader in leaders {
        assert!(leader.pid > 1);
        if !service_group_has_owned_member(leader) {
            continue;
        }
        let group_result = unsafe { libc::kill(-leader.pid, signal) };
        if group_result == 0 {
            continue;
        }
        if service_group_has_owned_member(leader) {
            failed.push(leader.pid);
        }
    }
    failed
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    static ENV_LOCK: Mutex<()> = Mutex::new(());

    fn with_public_env(
        overrides: &[(&str, Option<&str>)],
        assertions: impl FnOnce(Vec<(String, Option<String>)>),
    ) {
        let _guard = ENV_LOCK.lock().unwrap();
        let keys = [
            "FLUXER_MEDIA_PROXY_ENDPOINT",
            "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT",
            "FLUXER_MEDIA_ENDPOINT",
            "FLUXER_STATIC_CDN_ENDPOINT",
            "FLUXER_PUBLIC_URL",
        ];
        let saved = keys
            .iter()
            .map(|key| (*key, env::var(key).ok()))
            .collect::<Vec<_>>();
        for key in keys {
            unsafe {
                env::remove_var(key);
            }
        }
        for (key, value) in overrides {
            if let Some(value) = value {
                unsafe {
                    env::set_var(key, value);
                }
            }
        }

        let spec = rust_services()
            .into_iter()
            .find(|spec| spec.name == "unfurl")
            .unwrap();
        let env = service_env(&spec, "router", spec.port_base);

        for (key, value) in saved {
            match value {
                Some(value) => unsafe {
                    env::set_var(key, value);
                },
                None => unsafe {
                    env::remove_var(key);
                },
            }
        }

        assertions(env);
    }

    #[test]
    fn rejects_unknown_services() {
        let err = select_services(&["bogus".to_owned()])
            .unwrap_err()
            .to_string();
        assert!(err.contains("Unknown Rust service"));
    }

    #[test]
    fn builds_unfurl_env_with_media_endpoints() {
        with_public_env(&[], |env| {
            assert!(
                env.iter()
                    .any(|(key, value)| key == "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT"
                        && value.as_deref() == Some("http://localhost:8088/media"))
            );
            assert!(
                env.iter()
                    .any(|(key, value)| key == "FLUXER_STATIC_CDN_ENDPOINT"
                        && value.as_deref() == Some("http://localhost:8088"))
            );
        });
    }

    #[test]
    fn unfurl_env_uses_public_url_overrides() {
        with_public_env(
            &[
                (
                    "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT",
                    Some("https://dev.example.com/media"),
                ),
                (
                    "FLUXER_STATIC_CDN_ENDPOINT",
                    Some("https://dev.example.com"),
                ),
            ],
            |env| {
                assert!(
                    env.iter()
                        .any(|(key, value)| key == "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT"
                            && value.as_deref() == Some("https://dev.example.com/media"))
                );
                assert!(
                    env.iter()
                        .any(|(key, value)| key == "FLUXER_STATIC_CDN_ENDPOINT"
                            && value.as_deref() == Some("https://dev.example.com"))
                );
            },
        );
    }

    #[test]
    fn cmdline_binary_match_uses_selected_service_binary_suffix() {
        let binaries = BTreeSet::from(["target/debug/fluxer_messages".to_owned()]);
        assert!(cmdline_has_service_binary(
            &["/workspaces/fluxer/target/debug/fluxer_messages".to_owned()],
            &binaries
        ));
        assert!(!cmdline_has_service_binary(
            &["/workspaces/fluxer/target/debug/fluxer_users".to_owned()],
            &binaries
        ));
    }

    #[test]
    fn proc_stat_parsing_handles_parentheses_and_spaces_in_comm() {
        assert_eq!(
            parse_proc_stat_state_and_pgid("99 (spaced comm) T 1 99 99 0 -1"),
            Some(('T', 99))
        );
        assert_eq!(
            parse_proc_stat_state_and_pgid("99 (weird) comm (name) Z 1 42 42 0 -1"),
            Some(('Z', 42))
        );
    }

    #[test]
    fn proc_stat_parsing_rejects_malformed_lines() {
        assert_eq!(parse_proc_stat_state_and_pgid(""), None);
        assert_eq!(parse_proc_stat_state_and_pgid("1234 (svc)"), None);
        assert_eq!(parse_proc_stat_state_and_pgid("1234 (svc) S"), None);
        assert_eq!(parse_proc_stat_state_and_pgid("1234 (svc) S 1"), None);
        assert_eq!(
            parse_proc_stat_state_and_pgid("1234 (svc) S 1 not-a-pgid"),
            None
        );
    }

    #[test]
    fn proc_stat_parsing_extracts_starttime() {
        let stat = "1234 (svc) S 1 1234 1234 0 -1 4194560 0 0 0 0 5 3 0 0 20 0 30 0 12345678 4096";
        assert_eq!(parse_proc_stat_starttime(stat), Some(12_345_678));
    }

    #[test]
    fn proc_stat_starttime_parsing_rejects_truncated_lines() {
        assert_eq!(parse_proc_stat_starttime(""), None);
        assert_eq!(parse_proc_stat_starttime("1234 (svc)"), None);
        assert_eq!(
            parse_proc_stat_starttime("1234 (svc) S 1 1234 1234 0 -1 4194560 0"),
            None
        );
    }

    #[test]
    fn zombie_and_reaped_states_count_as_dead() {
        assert!(proc_stat_state_is_dead('Z'));
        assert!(proc_stat_state_is_dead('X'));
        assert!(proc_stat_state_is_dead('x'));
        assert!(!proc_stat_state_is_dead('S'));
    }
}
