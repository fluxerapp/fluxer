// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::manifest::GATEWAY_PORT;
use crate::paths::{DEV_GATEWAY_DIR, GATEWAY_CONFIG_DIR, ROOT, write_private_file};
use crate::proc::{
    AwaitOutcome, RESTART_LIMIT, RESTART_WINDOW, RunOptions, ShutdownSignal, await_or_shutdown,
    can_bind_tcp_port, cleanup_orphaned_process_groups, configure_process_group,
    force_kill_process_group, format_command, merged_env, pending_shutdown, prefix_output,
    process_group_running, restart_budget_exceeded, run_command_interruptible, stop_process_group,
    terminate_process_group,
};
use anyhow::{Context, Result, bail};
use std::collections::VecDeque;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};
use tokio::time::sleep;

const GATEWAY_MAIN_EVAL: &str = "fluxer_gateway@@main:run(fluxer_gateway)";
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
const GATEWAY_DEFAULT_NODE_NAME: &str = "fluxer_gateway@127.0.0.1";
const GATEWAY_DEFAULT_DIST_PORT: &str = "8081";
const GATEWAY_STATUS_PORT_OFFSET: u16 = 1000;
const GATEWAY_COMPILE_COMMAND: &[&str] = &["scripts/gleam.sh", "build"];
const GATEWAY_EPMDLESS_VM_ARGS: &[&str] = &["-start_epmd", "-erl_epmd_port"];
const NODE_SHUTDOWN_TIMEOUT: Duration = Duration::from_secs(5);
const NODE_RESTART_PORT_WAIT: Duration = Duration::from_secs(75);
const NESTED_STOP_GRACE_PERIOD: Duration = Duration::from_secs(5);
const ROOT_STOP_GRACE_PERIOD: Duration = Duration::from_secs(15);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GatewayNode {
    pub role: String,
    pub ordinal: u16,
    pub http_port: u16,
    pub status_port: u16,
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
    gateway_dir().join("build/dev/erlang")
}

fn gateway_cookie() -> String {
    env::var("FLUXER_ERLANG_COOKIE").unwrap_or_else(|_| GATEWAY_CLUSTER_COOKIE.to_owned())
}

fn gateway_node_name() -> String {
    env::var("FLUXER_ERLANG_NODE_NAME").unwrap_or_else(|_| GATEWAY_DEFAULT_NODE_NAME.to_owned())
}

pub fn setup_gateway_config() -> Result<()> {
    write_gateway_config(
        DEV_GATEWAY_DIR.as_path(),
        &gateway_node_name(),
        &gateway_cookie(),
        &env::var("FLUXER_ERLANG_DIST_PORT")
            .unwrap_or_else(|_| GATEWAY_DEFAULT_DIST_PORT.to_owned()),
    )
}

pub fn write_gateway_config(
    config_dir: &Path,
    node_name: &str,
    cookie: &str,
    dist_port: &str,
) -> Result<()> {
    let sys_config_source = GATEWAY_CONFIG_DIR.join("sys.config");
    let vm_args_source = GATEWAY_CONFIG_DIR.join("vm.args.src");
    fs::create_dir_all(config_dir)
        .with_context(|| format!("failed to create {}", config_dir.display()))?;
    let sys_config = config_dir.join("sys.config");
    fs::write(
        &sys_config,
        fs::read_to_string(&sys_config_source)
            .with_context(|| format!("failed to read {}", sys_config_source.display()))?,
    )
    .with_context(|| format!("failed to write {}", sys_config.display()))?;
    let template = fs::read_to_string(&vm_args_source)
        .with_context(|| format!("failed to read {}", vm_args_source.display()))?;
    let sys_config_path = sys_config.display().to_string();
    let vm_text = render_dev_vm_args(&template, |name| match name {
        "FLUXER_ERLANG_NODE_NAME" => Some(node_name.to_owned()),
        "FLUXER_ERLANG_COOKIE" => Some(cookie.to_owned()),
        "FLUXER_ERLANG_DIST_PORT" => Some(dist_port.to_owned()),
        "FLUXER_GATEWAY_SYS_CONFIG" => Some(sys_config_path.clone()),
        _ => env::var(name).ok(),
    })
    .with_context(|| format!("failed to render {}", vm_args_source.display()))?;
    write_private_file(&config_dir.join("vm.args"), &vm_text)?;
    Ok(())
}

fn render_dev_vm_args(template: &str, lookup: impl Fn(&str) -> Option<String>) -> Result<String> {
    let mut rendered = String::with_capacity(template.len());
    for line in template.lines() {
        if GATEWAY_EPMDLESS_VM_ARGS
            .iter()
            .any(|flag| line.split_whitespace().next() == Some(*flag))
        {
            continue;
        }
        rendered.push_str(&substitute_env_line(line, &lookup)?);
        rendered.push('\n');
    }
    Ok(rendered)
}

fn substitute_env_line(line: &str, lookup: &impl Fn(&str) -> Option<String>) -> Result<String> {
    let mut out = String::with_capacity(line.len());
    let mut rest = line;
    while let Some(start) = rest.find("${") {
        out.push_str(&rest[..start]);
        let after = &rest[start + 2..];
        let end = after
            .find('}')
            .with_context(|| format!("unterminated variable in vm.args line: {line}"))?;
        let token = &after[..end];
        let (name, fallback) = match token.split_once(":-") {
            Some((name, fallback)) => (name, Some(fallback)),
            None => (token, None),
        };
        if name.is_empty()
            || !name
                .chars()
                .all(|character| character.is_ascii_alphanumeric() || character == '_')
        {
            bail!("invalid variable name in vm.args line: {line}");
        }
        let value = lookup(name).filter(|value| !value.is_empty());
        match (value, fallback) {
            (Some(value), _) => out.push_str(&value),
            (None, Some(fallback)) => out.push_str(fallback),
            (None, None) => bail!("{name} is not set and has no default in vm.args"),
        }
        rest = &after[end + 1..];
    }
    out.push_str(rest);
    Ok(out)
}

fn single_status_port(env: &std::collections::BTreeMap<String, String>) -> Result<String> {
    if let Some(port) = env
        .get("FLUXER_GATEWAY_STATUS_PORT")
        .filter(|port| !port.is_empty())
    {
        return Ok(port.clone());
    }
    let http_port = match env.get("FLUXER_GATEWAY_PORT") {
        Some(port) => port
            .parse::<u16>()
            .with_context(|| format!("FLUXER_GATEWAY_PORT is not a port: {port}"))?,
        None => GATEWAY_PORT,
    };
    status_port_for(http_port).map(|port| port.to_string())
}

fn status_port_for(http_port: u16) -> Result<u16> {
    http_port
        .checked_add(GATEWAY_STATUS_PORT_OFFSET)
        .with_context(|| format!("no status port fits above gateway port {http_port}"))
}

pub async fn run_gateway() -> Result<i32> {
    let mut shutdown = ShutdownSignal::new()?;
    let node_names = std::collections::HashSet::from([gateway_node_name()]);
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
    let status_port = single_status_port(&merged_env(None, true)?)?;
    let outcome = run_command_interruptible(
        &command_refs,
        RunOptions {
            cwd: &working_dir,
            env: vec![("FLUXER_GATEWAY_STATUS_PORT".to_owned(), Some(status_port))],
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
    let working_dir = gateway_dir();
    run_command_interruptible(
        GATEWAY_COMPILE_COMMAND,
        RunOptions {
            cwd: &working_dir,
            ..RunOptions::default()
        },
        shutdown,
    )
    .await
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
            let http_port = ports[(ordinal - 1) as usize]
                .checked_add(http_port_offset)
                .context("FLUXER_DEV_GATEWAY_CLUSTER_HTTP_PORT_OFFSET is too large")?;
            nodes.push(GatewayNode {
                role: (*role).to_owned(),
                ordinal,
                http_port,
                status_port: status_port_for(http_port)?,
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
                    !can_bind_tcp_port(node.status_port),
                    format!("{} status={}", node.name(), node.status_port),
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
    let cookie = gateway_cookie();
    for node in nodes {
        write_gateway_config(
            &node.config_dir(),
            &node.erlang_name(),
            &cookie,
            &node.dist_port.to_string(),
        )?;
    }
    Ok(())
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
            "FLUXER_GATEWAY_STATUS_PORT".to_owned(),
            Some(node.status_port.to_string()),
        ),
        (
            "FLUXER_ERLANG_NODE_NAME".to_owned(),
            Some(node.erlang_name()),
        ),
        (
            "FLUXER_ERLANG_DIST_PORT".to_owned(),
            Some(node.dist_port.to_string()),
        ),
        ("FLUXER_ERLANG_COOKIE".to_owned(), Some(gateway_cookie())),
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
                    "{}:http={},status={},dist={}",
                    node.name(),
                    node.http_port,
                    node.status_port,
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
    Ok(erl_command(&ebin_paths, config_dir))
}

fn erl_command(ebin_paths: &[PathBuf], config_dir: &Path) -> Vec<String> {
    let mut args = vec!["erl".to_owned(), "-noshell".to_owned()];
    for path in ebin_paths {
        args.push("-pa".to_owned());
        args.push(path.display().to_string());
    }
    args.extend([
        "-args_file".to_owned(),
        config_dir.join("vm.args").display().to_string(),
        "-eval".to_owned(),
        GATEWAY_MAIN_EVAL.to_owned(),
        "-extra".to_owned(),
    ]);
    args
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::{BTreeMap, HashSet};

    fn lookup(values: &[(&str, &str)]) -> impl Fn(&str) -> Option<String> {
        let values = values
            .iter()
            .map(|(name, value)| ((*name).to_owned(), (*value).to_owned()))
            .collect::<BTreeMap<_, _>>();
        move |name| values.get(name).cloned()
    }

    #[test]
    fn substitutes_set_values_and_defaults() {
        let rendered = render_dev_vm_args(
            "-name ${NODE}\n+S ${SCHEDULERS:-2}:${SCHEDULERS:-2}\n+SDio ${EMPTY:-3}\n",
            lookup(&[("NODE", "gw@127.0.0.1"), ("EMPTY", "")]),
        )
        .unwrap();
        assert_eq!(rendered, "-name gw@127.0.0.1\n+S 2:2\n+SDio 3\n");
    }

    #[test]
    fn rejects_unset_variable_without_default() {
        let error = render_dev_vm_args("-setcookie ${COOKIE}\n", lookup(&[])).unwrap_err();
        assert!(error.to_string().contains("COOKIE is not set"));
    }

    #[test]
    fn rejects_unterminated_variable() {
        assert!(render_dev_vm_args("-name ${NODE\n", lookup(&[("NODE", "x")])).is_err());
    }

    #[test]
    fn drops_epmdless_dist_flags_so_local_nodes_share_epmd() {
        let rendered = render_dev_vm_args(
            "-name a@127.0.0.1\n-start_epmd false\n-erl_epmd_port ${PORT}\n-kernel inet_dist_listen_min ${PORT}\n",
            lookup(&[("PORT", "9001")]),
        )
        .unwrap();
        assert_eq!(
            rendered,
            "-name a@127.0.0.1\n-kernel inet_dist_listen_min 9001\n"
        );
    }

    #[test]
    fn renders_the_gateway_vm_args_template_completely() {
        let template = fs::read_to_string(GATEWAY_CONFIG_DIR.join("vm.args.src")).unwrap();
        let rendered = render_dev_vm_args(
            &template,
            lookup(&[
                ("FLUXER_ERLANG_NODE_NAME", "fluxer_gateway@127.0.0.1"),
                ("FLUXER_ERLANG_COOKIE", "cookie"),
                ("FLUXER_ERLANG_DIST_PORT", "9001"),
                ("FLUXER_GATEWAY_SYS_CONFIG", "/dev/sys.config"),
            ]),
        )
        .unwrap();
        assert!(!rendered.contains("${"));
        assert!(!rendered.contains("start_epmd"));
        assert!(!rendered.contains("erl_epmd_port"));
        assert!(rendered.contains("-config /dev/sys.config\n"));
        assert!(rendered.contains("-kernel inet_dist_listen_min 9001\n"));
        assert!(rendered.contains("-kernel inet_dist_listen_max 9001\n"));
        assert!(rendered.contains("-kernel connect_all true\n"));
    }

    #[test]
    fn single_status_port_follows_the_gateway_port() {
        let env = BTreeMap::from([("FLUXER_GATEWAY_PORT".to_owned(), "8771".to_owned())]);
        assert_eq!(single_status_port(&env).unwrap(), "9771");
        assert_eq!(single_status_port(&BTreeMap::new()).unwrap(), "9771");
    }

    #[test]
    fn single_status_port_keeps_an_explicit_value() {
        let env = BTreeMap::from([
            ("FLUXER_GATEWAY_PORT".to_owned(), "8771".to_owned()),
            ("FLUXER_GATEWAY_STATUS_PORT".to_owned(), "9999".to_owned()),
        ]);
        assert_eq!(single_status_port(&env).unwrap(), "9999");
    }

    #[test]
    fn single_status_port_avoids_the_media_proxy_port() {
        let env = BTreeMap::from([(
            "FLUXER_GATEWAY_PORT".to_owned(),
            crate::manifest::GATEWAY_PORT.to_string(),
        )]);
        assert_ne!(
            single_status_port(&env).unwrap(),
            crate::manifest::MEDIA_PROXY_PORT.to_string()
        );
    }

    #[test]
    fn cluster_ports_are_unique_and_clear_of_dev_services() {
        let nodes = build_gateway_cluster_nodes().unwrap();
        assert_eq!(
            nodes.len(),
            GATEWAY_CLUSTER_ROLES.len() * GATEWAY_CLUSTER_DEFAULT_REPLICAS as usize
        );
        let ports = nodes
            .iter()
            .flat_map(|node| [node.http_port, node.status_port, node.dist_port])
            .collect::<Vec<_>>();
        assert_eq!(ports.iter().collect::<HashSet<_>>().len(), ports.len());
        for reserved in [
            crate::manifest::API_PORT,
            crate::manifest::MEDIA_PROXY_PORT,
            crate::manifest::APP_PROXY_PORT,
            crate::manifest::DEV_PROXY_PORT,
        ] {
            assert!(!ports.contains(&reserved), "port {reserved} is taken");
        }
    }

    #[test]
    fn cluster_node_env_sets_role_ports_and_peers() {
        let node = GatewayNode {
            role: "guilds".to_owned(),
            ordinal: 2,
            http_port: 8801,
            status_port: 9801,
            dist_port: 9011,
        };
        let env = gateway_node_env(&node, "a@127.0.0.1,b@127.0.0.1")
            .into_iter()
            .collect::<BTreeMap<_, _>>();
        let value = |name: &str| env.get(name).cloned().flatten();
        assert_eq!(value("FLUXER_GATEWAY_ROLE").as_deref(), Some("guilds"));
        assert_eq!(value("FLUXER_GATEWAY_PORT").as_deref(), Some("8801"));
        assert_eq!(value("FLUXER_GATEWAY_STATUS_PORT").as_deref(), Some("9801"));
        assert_eq!(value("FLUXER_ERLANG_DIST_PORT").as_deref(), Some("9011"));
        assert_eq!(
            value("FLUXER_GATEWAY_CLUSTER_STATIC_PEERS").as_deref(),
            Some("a@127.0.0.1,b@127.0.0.1")
        );
    }

    #[test]
    fn erl_command_runs_the_gleam_main_with_the_rendered_args() {
        let ebin = PathBuf::from("/gw/build/dev/erlang/fluxer_gateway/ebin");
        let command = erl_command(std::slice::from_ref(&ebin), Path::new("/dev-config"));
        assert_eq!(
            command,
            [
                "erl",
                "-noshell",
                "-pa",
                "/gw/build/dev/erlang/fluxer_gateway/ebin",
                "-args_file",
                "/dev-config/vm.args",
                "-eval",
                "fluxer_gateway@@main:run(fluxer_gateway)",
                "-extra",
            ]
        );
    }
}
