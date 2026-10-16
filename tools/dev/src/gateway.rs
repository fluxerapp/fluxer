// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::paths::{DEV_GATEWAY_DIR, GATEWAY_CONFIG_DIR, ROOT};
use crate::proc::{
    AwaitOutcome, RESTART_LIMIT, RESTART_WINDOW, RunOptions, ShutdownSignal, await_or_shutdown,
    can_bind_tcp_port, cleanup_orphaned_process_groups, configure_process_group,
    force_kill_process_group, format_command, merged_env, pending_shutdown, prefix_output,
    process_group_running, restart_budget_exceeded, run_command_interruptible, stop_process_group,
    terminate_process_group,
};
use anyhow::{Context, Result, bail};
use sha2::{Digest, Sha256};
use std::collections::VecDeque;
use std::env;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};
use tokio::time::sleep;

const GATEWAY_FOREGROUND_EVAL: &str = concat!(
    "case application:ensure_all_started(fluxer_gateway) of ",
    "{ok, _Apps} -> io:format(\"gateway started~n\"), receive after infinity -> ok end; ",
    "Error -> io:format(\"gateway failed: ~p~n\", [Error]), halt(1) end."
);
const GATEWAY_CLUSTER_ROLES: &[&str] = &[
    "websocket",
    "sessions",
    "presence",
    "guilds",
    "calls",
    "push",
];
const GATEWAY_CLUSTER_DEFAULT_REPLICAS: u16 = 3;
const GATEWAY_CLUSTER_DIST_PORT_BASE: u16 = 9001;
const GATEWAY_CLUSTER_COOKIE: &str = "fluxer-dev";
const GATEWAY_COMPILE_COMMAND: &[&str] = &[
    "cargo",
    "run",
    "--locked",
    "--quiet",
    "--manifest-path",
    "tools/ci/Cargo.toml",
    "--",
    "gateway",
    "--step",
    "compile",
];
const GATEWAY_DEPENDENCY_INPUTS: &[&str] = &["rebar.config", "rebar.config.script", "rebar.lock"];
const GATEWAY_DEPENDENCY_INPUT_MAX_BYTES: u64 = 4 * 1024 * 1024;
const GATEWAY_DEPENDENCY_STAMP_FILE: &str = "rebar-dependencies.sha256";
const GATEWAY_DEPENDENCY_STAMP_MAX_BYTES: u64 = 128;
const NODE_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(5);
const NODE_RESTART_PORT_WAIT: Duration = Duration::from_secs(75);
const NESTED_STOP_GRACE_PERIOD: Duration = Duration::from_secs(5);
const ROOT_STOP_GRACE_PERIOD: Duration = Duration::from_secs(15);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GatewayNode {
    pub role: String,
    pub ordinal: u16,
    pub http_port: u16,
    pub dist_port: u16,
}

impl GatewayNode {
    pub fn name(&self) -> String {
        format!("{}-{}", self.role, self.ordinal)
    }

    pub fn erlang_name(&self) -> String {
        let prefix = env::var("FLUXER_DEV_GATEWAY_CLUSTER_NODE_PREFIX")
            .unwrap_or_else(|_| "fluxer_gateway".to_owned());
        format!("{prefix}_{}_{}@127.0.0.1", self.role, self.ordinal)
    }

    pub fn config_dir(&self) -> PathBuf {
        DEV_GATEWAY_DIR.join("cluster").join(self.name())
    }
}

pub fn gateway_dir() -> PathBuf {
    ROOT.join("fluxer_gateway")
}

fn gateway_ebin_root() -> PathBuf {
    gateway_dir().join("_build/default/lib")
}

pub fn setup_gateway_config() -> Result<()> {
    write_gateway_config(
        DEV_GATEWAY_DIR.as_path(),
        &env::var("FLUXER_ERLANG_NODE_NAME")
            .unwrap_or_else(|_| "fluxer_gateway@127.0.0.1".to_owned()),
        &env::var("FLUXER_ERLANG_COOKIE").unwrap_or_else(|_| GATEWAY_CLUSTER_COOKIE.to_owned()),
        &env::var("FLUXER_ERLANG_DIST_PORT").unwrap_or_else(|_| "8081".to_owned()),
    )?;
    remove_stale_gateway_config()
}

pub fn write_gateway_config(
    config_dir: &Path,
    node_name: &str,
    cookie: &str,
    dist_port: &str,
) -> Result<()> {
    let sys_template = GATEWAY_CONFIG_DIR.join("sys.config.template");
    let vm_template = GATEWAY_CONFIG_DIR.join("vm.args.template");
    fs::create_dir_all(config_dir)?;
    fs::write(
        config_dir.join("sys.config"),
        fs::read_to_string(sys_template)?,
    )?;
    let vm_text = fs::read_to_string(vm_template)?
        .replace("${FLUXER_ERLANG_NODE_NAME}", node_name)
        .replace("${FLUXER_ERLANG_COOKIE}", cookie)
        .replace("${FLUXER_ERLANG_DIST_PORT}", dist_port);
    fs::write(config_dir.join("vm.args"), vm_text)?;
    Ok(())
}

fn remove_stale_gateway_config() -> Result<()> {
    for path in [
        GATEWAY_CONFIG_DIR.join("sys.config"),
        GATEWAY_CONFIG_DIR.join("vm.args"),
    ] {
        if path.exists() {
            fs::remove_file(&path)
                .with_context(|| format!("failed to remove {}", path.display()))?;
        }
    }
    Ok(())
}

pub async fn run_gateway() -> Result<i32> {
    let mut shutdown = ShutdownSignal::new()?;
    let node_names = std::collections::HashSet::from([env::var("FLUXER_ERLANG_NODE_NAME")
        .unwrap_or_else(|_| "fluxer_gateway@127.0.0.1".to_owned())]);
    cleanup_orphaned_gateway_processes(&node_names).await?;
    if let Some(signal) = pending_shutdown(&mut shutdown).await {
        println!("Received {signal}; stopping gateway startup...");
        return Ok(0);
    }
    setup_gateway_config()?;
    match compile_gateway_interruptible(&mut shutdown).await? {
        AwaitOutcome::Completed(_) => {}
        AwaitOutcome::Shutdown(signal) => {
            println!("Received {signal}; stopping gateway startup...");
            return Ok(0);
        }
    }
    let command = build_gateway_command(DEV_GATEWAY_DIR.as_path())?;
    let command_refs = command.iter().map(String::as_str).collect::<Vec<_>>();
    let working_dir = gateway_dir();
    let outcome = run_command_interruptible(
        &command_refs,
        RunOptions {
            cwd: &working_dir,
            ..RunOptions::default()
        },
        &mut shutdown,
    )
    .await?;
    stop_idle_epmd();
    match outcome {
        AwaitOutcome::Completed(output) => Ok(output.status.code().unwrap_or(1)),
        AwaitOutcome::Shutdown(signal) => {
            println!("Received {signal}; stopping gateway...");
            Ok(0)
        }
    }
}

struct SupervisedNode {
    node: GatewayNode,
    child: Child,
    restarts: VecDeque<Instant>,
}

pub async fn run_gateway_cluster() -> Result<i32> {
    let nodes = build_gateway_cluster_nodes()?;
    let mut shutdown = ShutdownSignal::new()?;
    let node_names = nodes
        .iter()
        .map(GatewayNode::erlang_name)
        .collect::<std::collections::HashSet<_>>();
    cleanup_orphaned_gateway_processes(&node_names).await?;
    if let Some(signal) = pending_shutdown(&mut shutdown).await {
        println!("Received {signal}; stopping gateway startup...");
        return Ok(0);
    }
    match await_or_shutdown(&mut shutdown, wait_for_cluster_ports_available(&nodes)).await {
        AwaitOutcome::Completed(result) => result?,
        AwaitOutcome::Shutdown(signal) => {
            println!("Received {signal}; stopping gateway startup...");
            return Ok(0);
        }
    }
    setup_gateway_cluster_config(&nodes)?;
    match compile_gateway_interruptible(&mut shutdown).await? {
        AwaitOutcome::Completed(_) => {}
        AwaitOutcome::Shutdown(signal) => {
            println!("Received {signal}; stopping gateway startup...");
            return Ok(0);
        }
    }
    let static_peers = nodes
        .iter()
        .map(GatewayNode::erlang_name)
        .collect::<Vec<_>>()
        .join(",");
    let mut supervised = Vec::new();
    print_gateway_cluster_topology(&nodes);
    for node in &nodes {
        let child = match start_node(node, &static_peers) {
            Ok(child) => child,
            Err(error) => {
                stop_supervised(&mut supervised);
                return Err(error);
            }
        };
        supervised.push(SupervisedNode {
            node: node.clone(),
            child,
            restarts: VecDeque::new(),
        });
    }
    loop {
        match await_or_shutdown(
            &mut shutdown,
            restart_exited_nodes(&mut supervised, &static_peers),
        )
        .await
        {
            AwaitOutcome::Completed(Ok(())) => {}
            AwaitOutcome::Completed(Err(error)) => {
                stop_supervised(&mut supervised);
                return Err(error);
            }
            AwaitOutcome::Shutdown(signal) => {
                println!("Received {signal}; stopping gateway cluster...");
                stop_supervised(&mut supervised);
                return Ok(0);
            }
        }
        tokio::select! {
            signal = shutdown.recv() => {
                println!("Received {signal}; stopping gateway cluster...");
                stop_supervised(&mut supervised);
                return Ok(0);
            }
            _ = sleep(Duration::from_millis(500)) => {}
        }
    }
}

async fn compile_gateway_interruptible(
    shutdown: &mut ShutdownSignal,
) -> Result<AwaitOutcome<std::process::Output>> {
    let dependency_stamp = prepare_gateway_compile()?;
    let outcome =
        run_command_interruptible(GATEWAY_COMPILE_COMMAND, RunOptions::default(), shutdown).await?;
    if matches!(&outcome, AwaitOutcome::Completed(_)) {
        write_gateway_dependency_stamp(&dependency_stamp)?;
    }
    Ok(outcome)
}

fn prepare_gateway_compile() -> Result<String> {
    let dependency_stamp = gateway_dependency_stamp()?;
    if gateway_dependency_stamp_matches(&dependency_stamp)? {
        return Ok(dependency_stamp);
    }
    let build_dir = gateway_dir().join("_build/default");
    println!(
        "Gateway dependency inputs changed; removing stale {}",
        build_dir.display()
    );
    match fs::remove_dir_all(&build_dir) {
        Ok(()) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => {
            return Err(error).with_context(|| format!("failed to remove {}", build_dir.display()));
        }
    }
    Ok(dependency_stamp)
}

fn gateway_dependency_stamp() -> Result<String> {
    let mut hasher = Sha256::new();
    let mut total_bytes = 0_u64;
    for input in GATEWAY_DEPENDENCY_INPUTS {
        let path = gateway_dir().join(input);
        let metadata = fs::metadata(&path)
            .with_context(|| format!("failed to read metadata for {}", path.display()))?;
        if !metadata.is_file() {
            bail!("gateway dependency input is not a file: {}", path.display());
        }
        total_bytes = total_bytes
            .checked_add(metadata.len())
            .context("gateway dependency input byte count overflowed")?;
        if total_bytes > GATEWAY_DEPENDENCY_INPUT_MAX_BYTES {
            bail!(
                "gateway dependency inputs exceed {} bytes",
                GATEWAY_DEPENDENCY_INPUT_MAX_BYTES
            );
        }
        let mut bytes = Vec::with_capacity(metadata.len() as usize);
        fs::File::open(&path)
            .with_context(|| format!("failed to open {}", path.display()))?
            .take(metadata.len() + 1)
            .read_to_end(&mut bytes)
            .with_context(|| format!("failed to read {}", path.display()))?;
        if bytes.len() as u64 != metadata.len() {
            bail!(
                "gateway dependency input changed while reading: {}",
                path.display()
            );
        }
        hasher.update((input.len() as u64).to_le_bytes());
        hasher.update(input.as_bytes());
        hasher.update(metadata.len().to_le_bytes());
        hasher.update(&bytes);
    }
    Ok(hex::encode(hasher.finalize()))
}

fn gateway_dependency_stamp_matches(expected: &str) -> Result<bool> {
    let path = DEV_GATEWAY_DIR.join(GATEWAY_DEPENDENCY_STAMP_FILE);
    let file = match fs::File::open(&path) {
        Ok(file) => file,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(error) => {
            return Err(error).with_context(|| format!("failed to open {}", path.display()));
        }
    };
    let mut bytes = Vec::new();
    file.take(GATEWAY_DEPENDENCY_STAMP_MAX_BYTES + 1)
        .read_to_end(&mut bytes)
        .with_context(|| format!("failed to read {}", path.display()))?;
    if bytes.len() as u64 > GATEWAY_DEPENDENCY_STAMP_MAX_BYTES {
        bail!(
            "gateway dependency stamp exceeds {} bytes: {}",
            GATEWAY_DEPENDENCY_STAMP_MAX_BYTES,
            path.display()
        );
    }
    Ok(bytes == format!("{expected}\n").as_bytes())
}

fn write_gateway_dependency_stamp(dependency_stamp: &str) -> Result<()> {
    fs::create_dir_all(DEV_GATEWAY_DIR.as_path())
        .with_context(|| format!("failed to create {}", DEV_GATEWAY_DIR.display()))?;
    let path = DEV_GATEWAY_DIR.join(GATEWAY_DEPENDENCY_STAMP_FILE);
    fs::write(&path, format!("{dependency_stamp}\n"))
        .with_context(|| format!("failed to write {}", path.display()))
}

async fn restart_exited_nodes(supervised: &mut [SupervisedNode], static_peers: &str) -> Result<()> {
    for entry in supervised.iter_mut() {
        let Some(status) = entry.child.try_wait()? else {
            continue;
        };
        if restart_budget_exceeded(&mut entry.restarts, Instant::now()) {
            bail!(
                "gateway node {} exited with {status} after {RESTART_LIMIT} restarts within {}s; giving up",
                entry.node.name(),
                RESTART_WINDOW.as_secs()
            );
        }
        println!(
            "[gateway:{}] exited with {status}; restarting node",
            entry.node.name()
        );
        restart_node(entry, static_peers).await?;
    }
    Ok(())
}

async fn restart_node(entry: &mut SupervisedNode, static_peers: &str) -> Result<()> {
    stop_process_group(&mut entry.child, NODE_SHUTDOWN_TIMEOUT).await;
    wait_for_ports_available_until(
        std::slice::from_ref(&entry.node),
        Instant::now() + NODE_RESTART_PORT_WAIT,
    )
    .await?;
    entry.child = start_node(&entry.node, static_peers)?;
    Ok(())
}

fn stop_supervised(supervised: &mut [SupervisedNode]) {
    let mut children = supervised
        .iter_mut()
        .map(|entry| &mut entry.child)
        .collect::<Vec<_>>();
    stop_child_processes(&mut children);
    stop_idle_epmd();
}

pub fn build_gateway_cluster_nodes() -> Result<Vec<GatewayNode>> {
    let replicas = positive_int_env(
        "FLUXER_DEV_GATEWAY_CLUSTER_REPLICAS",
        GATEWAY_CLUSTER_DEFAULT_REPLICAS,
    );
    if replicas > GATEWAY_CLUSTER_DEFAULT_REPLICAS {
        bail!(
            "FLUXER_DEV_GATEWAY_CLUSTER_REPLICAS={replicas} exceeds the configured local port table ({GATEWAY_CLUSTER_DEFAULT_REPLICAS})"
        );
    }
    let mut nodes = Vec::new();
    let mut dist_port = positive_int_env(
        "FLUXER_DEV_GATEWAY_CLUSTER_DIST_PORT_BASE",
        GATEWAY_CLUSTER_DIST_PORT_BASE,
    );
    let http_port_offset = non_negative_int_env("FLUXER_DEV_GATEWAY_CLUSTER_HTTP_PORT_OFFSET", 0);
    for role in GATEWAY_CLUSTER_ROLES {
        let ports = gateway_cluster_http_ports(role);
        for ordinal in 1..=replicas {
            nodes.push(GatewayNode {
                role: (*role).to_owned(),
                ordinal,
                http_port: ports[(ordinal - 1) as usize] + http_port_offset,
                dist_port,
            });
            dist_port += 1;
        }
    }
    Ok(nodes)
}

fn gateway_cluster_http_ports(role: &str) -> [u16; 3] {
    match role {
        "websocket" => [8771, 8772, 8774],
        "sessions" => [8780, 8781, 8782],
        "presence" => [8790, 8791, 8792],
        "guilds" => [8800, 8801, 8802],
        "calls" => [8810, 8811, 8812],
        "push" => [8820, 8821, 8822],
        _ => unreachable!("known gateway role"),
    }
}

fn positive_int_env(name: &str, default: u16) -> u16 {
    let parsed = int_env(name, default);
    if parsed > 0 { parsed } else { default }
}

fn non_negative_int_env(name: &str, default: u16) -> u16 {
    int_env(name, default)
}

fn int_env(name: &str, default: u16) -> u16 {
    env::var(name)
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(default)
}

fn wait_for_cluster_ports_available_sync(nodes: &[GatewayNode]) -> Result<()> {
    let conflicts = nodes
        .iter()
        .flat_map(|node| {
            [
                (
                    !can_bind_tcp_port(node.http_port),
                    format!("{} http={}", node.name(), node.http_port),
                ),
                (
                    !can_bind_tcp_port(node.dist_port),
                    format!("{} dist={}", node.name(), node.dist_port),
                ),
            ]
        })
        .filter_map(|(conflict, label)| conflict.then_some(label))
        .collect::<Vec<_>>();
    if !conflicts.is_empty() {
        bail!(
            "Gateway cluster port(s) already in use: {}. Stop the conflicting process or set FLUXER_DEV_GATEWAY_CLUSTER_HTTP_PORT_OFFSET/FLUXER_DEV_GATEWAY_CLUSTER_DIST_PORT_BASE for an isolated run.",
            conflicts.join(", ")
        );
    }
    Ok(())
}

async fn wait_for_cluster_ports_available(nodes: &[GatewayNode]) -> Result<()> {
    let timeout = env::var("FLUXER_DEV_GATEWAY_CLUSTER_PORT_WAIT_SECONDS")
        .ok()
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(10.0);
    wait_for_ports_available_until(nodes, Instant::now() + Duration::from_secs_f64(timeout)).await
}

async fn wait_for_ports_available_until(nodes: &[GatewayNode], deadline: Instant) -> Result<()> {
    loop {
        match wait_for_cluster_ports_available_sync(nodes) {
            Ok(()) => return Ok(()),
            Err(error) if Instant::now() < deadline => {
                let _ = error;
                sleep(Duration::from_millis(500)).await;
            }
            Err(error) => return Err(error),
        }
    }
}

async fn cleanup_orphaned_gateway_processes(
    node_names: &std::collections::HashSet<String>,
) -> Result<()> {
    cleanup_orphaned_process_groups(
        "gateway node",
        |process, args| {
            (cmdline_has_gateway_node(args, node_names) || cmdline_has_managed_gateway_config(args))
                && fs::read_link(format!("/proc/{}/cwd", process.pid))
                    .ok()
                    .as_deref()
                    == Some(gateway_dir().as_path())
        },
        cmdline_is_gateway_supervisor,
    )
    .await
}

fn cmdline_is_gateway_supervisor(args: &[String], current_exe: &Path) -> bool {
    args.first().map(Path::new) == Some(current_exe)
        && args.get(1).map(String::as_str) == Some("gateway")
        && match args.get(2).map(String::as_str) {
            None | Some("cluster" | "single") => args.len() <= 3,
            Some(_) => false,
        }
}

fn cmdline_has_managed_gateway_config(args: &[String]) -> bool {
    args.windows(2).any(|pair| {
        matches!(pair[0].as_str(), "-config" | "-args_file")
            && Path::new(&pair[1]).starts_with(DEV_GATEWAY_DIR.as_path())
    })
}

fn cmdline_has_gateway_node(
    args: &[String],
    node_names: &std::collections::HashSet<String>,
) -> bool {
    args.windows(2)
        .any(|pair| pair[0] == "-name" && node_names.contains(&pair[1]))
}

fn setup_gateway_cluster_config(nodes: &[GatewayNode]) -> Result<()> {
    let cookie =
        env::var("FLUXER_ERLANG_COOKIE").unwrap_or_else(|_| GATEWAY_CLUSTER_COOKIE.to_owned());
    for node in nodes {
        write_gateway_config(
            &node.config_dir(),
            &node.erlang_name(),
            &cookie,
            &node.dist_port.to_string(),
        )?;
    }
    remove_stale_gateway_config()
}

fn gateway_node_env(node: &GatewayNode, static_peers: &str) -> Vec<(String, Option<String>)> {
    vec![
        (
            "FLUXER_GATEWAY_CLUSTER_ENABLED".to_owned(),
            Some("true".to_owned()),
        ),
        (
            "FLUXER_GATEWAY_CLUSTER_STATIC_PEERS".to_owned(),
            Some(static_peers.to_owned()),
        ),
        ("FLUXER_GATEWAY_ROLE".to_owned(), Some(node.role.clone())),
        (
            "FLUXER_GATEWAY_PORT".to_owned(),
            Some(node.http_port.to_string()),
        ),
        (
            "FLUXER_ERLANG_NODE_NAME".to_owned(),
            Some(node.erlang_name()),
        ),
        (
            "FLUXER_ERLANG_DIST_PORT".to_owned(),
            Some(node.dist_port.to_string()),
        ),
        (
            "FLUXER_ERLANG_COOKIE".to_owned(),
            Some(
                env::var("FLUXER_ERLANG_COOKIE")
                    .unwrap_or_else(|_| GATEWAY_CLUSTER_COOKIE.to_owned()),
            ),
        ),
    ]
}

fn print_gateway_cluster_topology(nodes: &[GatewayNode]) {
    println!("Gateway clustered dev topology:");
    for role in GATEWAY_CLUSTER_ROLES {
        let ports = nodes
            .iter()
            .filter(|node| node.role == *role)
            .map(|node| {
                format!(
                    "{}:http={},dist={}",
                    node.name(),
                    node.http_port,
                    node.dist_port
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        println!("  {role}: {ports}");
    }
}

fn start_node(node: &GatewayNode, static_peers: &str) -> Result<Child> {
    let command = build_gateway_command(&node.config_dir())?;
    let env = merged_env(Some(&gateway_node_env(node, static_peers)), true)?;
    println!("[gateway:{}] $ {}", node.name(), format_command(&command));
    let mut child_command = Command::new(&command[0]);
    child_command
        .args(&command[1..])
        .current_dir(gateway_dir())
        .env_clear()
        .envs(env)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    configure_process_group(&mut child_command);
    let mut child = child_command.spawn()?;
    if let Some(stdout) = child.stdout.take() {
        let label = node.name();
        std::thread::spawn(move || prefix_output(&format!("gateway:{label}"), stdout));
    }
    if let Some(stderr) = child.stderr.take() {
        let label = node.name();
        std::thread::spawn(move || prefix_output(&format!("gateway:{label}"), stderr));
    }
    Ok(child)
}

pub fn stop_processes(processes: &mut [Child]) {
    let mut children = processes.iter_mut().collect::<Vec<_>>();
    stop_child_processes_with_grace(&mut children, ROOT_STOP_GRACE_PERIOD);
    stop_idle_epmd();
}

pub fn stop_child_processes(processes: &mut [&mut Child]) {
    stop_child_processes_with_grace(processes, NESTED_STOP_GRACE_PERIOD);
}

fn stop_child_processes_with_grace(processes: &mut [&mut Child], grace_period: Duration) {
    for process in processes.iter_mut() {
        if let Err(error) = terminate_process_group(process) {
            eprintln!(
                "Failed to terminate process group {}: {error}",
                process.id()
            );
        }
    }
    let deadline = Instant::now() + grace_period;
    loop {
        let all_exited = processes.iter_mut().all(|process| {
            process.try_wait().ok().flatten().is_some() && !process_group_running(process)
        });
        if all_exited {
            return;
        }
        if Instant::now() >= deadline {
            break;
        }
        std::thread::sleep(Duration::from_millis(100));
    }
    for process in processes {
        if process.try_wait().ok().flatten().is_none() || process_group_running(process) {
            force_kill_process_group(process);
        }
    }
}

fn stop_idle_epmd() {
    let output = match Command::new("epmd").arg("-kill").output() {
        Ok(output) => output,
        Err(error) => {
            eprintln!("Failed to stop Erlang port mapper: {error}");
            return;
        }
    };
    if output.status.success() {
        return;
    }
    let mut message = output.stdout;
    message.extend(output.stderr);
    let message = String::from_utf8_lossy(&message);
    if message.contains("Cannot connect to local epmd") {
        return;
    }
    eprintln!("Failed to stop Erlang port mapper: {}", message.trim());
}

pub fn build_gateway_command(config_dir: &Path) -> Result<Vec<String>> {
    let mut ebin_paths = Vec::new();
    if gateway_ebin_root().exists() {
        for entry in fs::read_dir(gateway_ebin_root())? {
            let path = entry?.path().join("ebin");
            if path.is_dir() {
                ebin_paths.push(path);
            }
        }
    }
    ebin_paths.sort();
    if ebin_paths.is_empty() {
        bail!(
            "No gateway ebin paths found under {}",
            gateway_ebin_root().display()
        );
    }
    let mut args = vec!["erl".to_owned(), "-noshell".to_owned()];
    for path in ebin_paths {
        args.push("-pa".to_owned());
        args.push(path.display().to_string());
    }
    args.extend([
        "-config".to_owned(),
        config_dir.join("sys.config").display().to_string(),
        "-args_file".to_owned(),
        config_dir.join("vm.args").display().to_string(),
        "-eval".to_owned(),
        GATEWAY_FOREGROUND_EVAL.to_owned(),
    ]);
    Ok(args)
}
