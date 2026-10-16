// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::manifest::{DEV_PROXY_PORT, MEDIA_PROXY_PORT, RustServiceSpec, rust_services};
use crate::paths::{ROOT, TARGET_DIR};
use crate::proc::{
    AwaitOutcome, RESTART_LIMIT, RESTART_WINDOW, RunOptions, ShutdownSignal, await_or_shutdown,
    can_bind_tcp_port, cleanup_orphaned_process_groups, configure_process_group, format_command,
    merged_env, pending_shutdown, prefix_output, restart_budget_exceeded,
    run_command_interruptible,
};
use anyhow::{Result, bail};
use std::collections::{BTreeSet, VecDeque};
use std::env;
use std::fs;
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

async fn cleanup_orphaned_service_processes(services: &[RustServiceSpec]) -> Result<()> {
    cleanup_orphaned_process_groups(
        "Rust service",
        |process, _args| {
            proc_has_managed_service_environment(process.pid, services)
                && fs::read_link(format!("/proc/{}/cwd", process.pid))
                    .ok()
                    .as_deref()
                    == Some(ROOT.as_path())
        },
        cmdline_is_service_supervisor,
    )
    .await
}

fn cmdline_is_service_supervisor(args: &[String], current_exe: &Path) -> bool {
    args.first().map(Path::new) == Some(current_exe)
        && args.get(1).map(String::as_str) == Some("rust-services")
}

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
