// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::gateway::setup_gateway_config;
use crate::manifest::{
    ADMIN_PORT, ANY_HOST, API_PORT, APP_PORT, APP_PROXY_PORT, DEV_PROXY_PORT, LOOPBACK_HOST,
    MEDIA_PROXY_PORT, rust_services,
};
use crate::paths::{DESKTOP_DIR, ROOT};
use crate::proc::{
    PNPM_INSTALL_ENV, RESTART_LIMIT, RESTART_WINDOW, RunOptions, ShutdownSignal, format_command,
    merged_env, restart_budget_exceeded, run_command, wait_http,
};
use crate::smoke::s3_endpoint;
use anyhow::{Result, bail};
use std::collections::{BTreeMap, VecDeque};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};
use tokio::time::sleep;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DevTask {
    pub name: &'static str,
    pub args: Vec<String>,
    pub cwd: PathBuf,
    pub env: Vec<(String, Option<String>)>,
}

const DEFAULT_TASKS: &[&str] = &[
    "proxy",
    "services",
    "media",
    "gateway",
    "marketing",
    "admin",
    "api",
    "worker",
    "app",
    "app-proxy",
];

const JS_DEPENDENCY_TASKS: &[&str] = &["api", "worker", "app", "admin"];
const OBJECT_STORE_TASKS: &[&str] = &["api", "worker", "media"];
const CLOUDFLARE_TUNNEL_TASK: &str = "cloudflare-tunnel";
const OBJECT_STORE_STARTUP_REPAIR_TIMEOUT_SECS: u64 = 60;
const DEFAULT_OBJECT_STORE_MONITOR_INTERVAL_SECS: u64 = 15;

pub async fn run_dev(task_names: &[String], cloudflare_tunnel: bool) -> Result<i32> {
    let tasks = task_table()?;
    let selected = if task_names.is_empty() {
        DEFAULT_TASKS
            .iter()
            .map(|name| (*name).to_owned())
            .collect::<Vec<_>>()
    } else {
        task_names.to_owned()
    };
    let selected = normalize_selected_tasks(selected, cloudflare_tunnel);
    let unknown = selected
        .iter()
        .filter(|name| !tasks.contains_key(name.as_str()))
        .cloned()
        .collect::<Vec<_>>();
    if !unknown.is_empty() {
        let available = tasks.keys().copied().collect::<Vec<_>>().join(", ");
        bail!(
            "Unknown dev task(s): {}. Available: {available}",
            unknown.join(", ")
        );
    }
    ensure_js_dependencies_if_needed(&selected)?;
    ensure_object_store_if_needed(&selected).await?;
    wait_for_search_backend_if_needed(&selected).await?;
    setup_gateway_config()?;
    let mut shutdown = ShutdownSignal::new()?;
    let mut processes = Vec::new();
    let mut task_names = Vec::new();
    let mut task_specs = Vec::new();
    let mut task_restarts = Vec::new();
    let monitor_object_store = selected_needs_object_store(&selected);
    let object_store_monitor_interval = object_store_monitor_interval();
    let mut next_object_store_check = Instant::now() + object_store_monitor_interval;
    let selected_for_readiness = selected.clone();
    for name in selected {
        if name == CLOUDFLARE_TUNNEL_TASK
            && let Err(error) =
                wait_for_cloudflare_tunnel_routes(&mut processes, &selected_for_readiness).await
        {
            crate::gateway::stop_processes(&mut processes);
            return Err(error);
        }
        let process = start_task(tasks.get(name.as_str()).expect("validated task"))?;
        processes.push(process);
        task_names.push(name.clone());
        task_specs.push(tasks.get(name.as_str()).expect("validated task").clone());
        task_restarts.push(VecDeque::new());
        if name == "services"
            && let Err(error) = wait_for_rust_services(&mut processes).await
        {
            crate::gateway::stop_processes(&mut processes);
            return Err(error);
        }
        if name == "api"
            && let Err(error) = wait_for_api(&mut processes).await
        {
            crate::gateway::stop_processes(&mut processes);
            return Err(error);
        }
    }
    loop {
        if let Err(error) =
            restart_exited_tasks(&mut processes, &task_names, &task_specs, &mut task_restarts)
        {
            crate::gateway::stop_processes(&mut processes);
            return Err(error);
        }
        if monitor_object_store && Instant::now() >= next_object_store_check {
            if let Err(error) = monitor_object_store_dependency().await {
                crate::gateway::stop_processes(&mut processes);
                return Err(error);
            }
            next_object_store_check = Instant::now() + object_store_monitor_interval;
        }
        tokio::select! {
            signal = shutdown.recv() => {
                println!("Received {signal}; stopping dev tasks...");
                crate::gateway::stop_processes(&mut processes);
                return Ok(0);
            }
            _ = sleep(Duration::from_millis(500)) => {}
        }
    }
}

async fn ensure_object_store_if_needed(selected: &[String]) -> Result<()> {
    if !selected_needs_object_store(selected) {
        return Ok(());
    }
    crate::media_proxy::ensure_dev_object_store(true, OBJECT_STORE_STARTUP_REPAIR_TIMEOUT_SECS)
        .await
}

async fn monitor_object_store_dependency() -> Result<()> {
    if check_object_store_endpoint().await.is_ok() {
        return Ok(());
    }
    eprintln!("Dev object store became unreachable; attempting SeaweedFS repair.");
    if let Err(error) =
        crate::media_proxy::ensure_dev_object_store(true, OBJECT_STORE_STARTUP_REPAIR_TIMEOUT_SECS)
            .await
    {
        bail!(
            "Dev object store is unreachable and automatic repair failed: {error}\nRun `fluxer-dev media-proxy doctor --repair` inside the devcontainer, then restart `fluxer-dev dev`."
        );
    }
    println!("Dev object store recovered.");
    Ok(())
}

async fn check_object_store_endpoint() -> Result<()> {
    let endpoint = s3_endpoint();
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(3))
        .build()?;
    match client.get(&endpoint).send().await {
        Ok(response) if response.status().as_u16() < 500 => Ok(()),
        Ok(response) => bail!(
            "SeaweedFS S3 endpoint {endpoint} returned HTTP {}",
            response.status()
        ),
        Err(error) => bail!("SeaweedFS S3 endpoint {endpoint} is unreachable: {error}"),
    }
}

fn restart_exited_tasks(
    processes: &mut [Child],
    task_names: &[String],
    task_specs: &[DevTask],
    task_restarts: &mut [VecDeque<Instant>],
) -> Result<()> {
    for (index, process) in processes.iter_mut().enumerate() {
        let Some(status) = process.try_wait()? else {
            continue;
        };
        if restart_budget_exceeded(&mut task_restarts[index], Instant::now()) {
            bail!(
                "Dev task {} exited with {status} after {RESTART_LIMIT} restarts within {}s; giving up",
                task_names[index],
                RESTART_WINDOW.as_secs()
            );
        }
        println!(
            "Dev task {} exited with {status}; restarting task",
            task_names[index]
        );
        *process = start_task(&task_specs[index])?;
    }
    Ok(())
}

fn normalize_selected_tasks(mut selected: Vec<String>, cloudflare_tunnel: bool) -> Vec<String> {
    let wants_cloudflare_tunnel =
        cloudflare_tunnel || selected.iter().any(|name| name == CLOUDFLARE_TUNNEL_TASK);
    if !wants_cloudflare_tunnel {
        return selected;
    }
    selected.retain(|name| name != CLOUDFLARE_TUNNEL_TASK);
    selected.push(CLOUDFLARE_TUNNEL_TASK.to_owned());
    selected
}

pub fn task_table() -> Result<BTreeMap<&'static str, DevTask>> {
    let self_tool = self_tool_command()?;
    let public_url = public_url();
    let marketing_endpoint = std::env::var("FLUXER_MARKETING_ENDPOINT")
        .unwrap_or_else(|_| format!("{public_url}/marketing"));
    let admin_endpoint =
        std::env::var("FLUXER_ADMIN_ENDPOINT").unwrap_or_else(|_| format!("{public_url}/admin"));
    let mut tasks = BTreeMap::new();
    let mut insert = |task: DevTask| {
        tasks.insert(task.name, task);
    };
    insert(DevTask {
        name: "proxy",
        args: tool_args(&self_tool, &["proxy"]),
        cwd: ROOT.clone(),
        env: Vec::new(),
    });
    insert(DevTask {
        name: CLOUDFLARE_TUNNEL_TASK,
        args: tool_args(&self_tool, &["tunnel", "run"]),
        cwd: ROOT.clone(),
        env: Vec::new(),
    });
    insert(DevTask {
        name: "api",
        args: strings(&["pnpm", "--filter", "fluxer_api", "dev"]),
        cwd: ROOT.clone(),
        env: vec![
            (
                "FLUXER_S3_PUBLIC_ENDPOINT".to_owned(),
                Some(public_url.clone()),
            ),
            (
                "FLUXER_S3_FORCE_PATH_STYLE".to_owned(),
                Some(
                    std::env::var("FLUXER_S3_FORCE_PATH_STYLE")
                        .unwrap_or_else(|_| "true".to_owned()),
                ),
            ),
            (
                "FLUXER_DISABLE_RATE_LIMITS".to_owned(),
                Some("true".to_owned()),
            ),
            (
                "FLUXER_RELAX_REGISTRATION_RATE_LIMITS".to_owned(),
                Some("true".to_owned()),
            ),
        ],
    });
    insert(DevTask {
        name: "worker",
        args: strings(&[
            "pnpm",
            "--filter",
            "fluxer_api",
            "exec",
            "tsx",
            "watch",
            "--clear-screen=false",
            "src/WorkerEntrypoint.ts",
        ]),
        cwd: ROOT.clone(),
        env: Vec::new(),
    });
    insert(DevTask {
        name: "app",
        args: strings(&["pnpm", "--filter", "fluxer_app", "dev"]),
        cwd: ROOT.clone(),
        env: vec![
            ("FLUXER_APP_DEV_PORT".to_owned(), Some(APP_PORT.to_string())),
            (
                "FLUXER_APP_SKIP_I18N_COMPILE".to_owned(),
                Some("true".to_owned()),
            ),
            (
                "FLUXER_STATIC_CDN_ENDPOINT".to_owned(),
                Some(public_url.clone()),
            ),
            (
                "PUBLIC_STATIC_CDN_ENDPOINT".to_owned(),
                Some(public_url.clone()),
            ),
        ],
    });
    insert(DevTask {
        name: "app-proxy",
        args: strings(&["cargo", "run", "-p", "fluxer_app_proxy"]),
        cwd: ROOT.clone(),
        env: vec![
            (
                "DISCOVERY_UPSTREAM_URL".to_owned(),
                Some(format!(
                    "http://{LOOPBACK_HOST}:{DEV_PROXY_PORT}/api/.well-known/fluxer"
                )),
            ),
            (
                "FLUXER_APP_PROXY_INDEX_UPSTREAM_URL".to_owned(),
                Some(format!("http://{LOOPBACK_HOST}:{APP_PORT}/")),
            ),
            (
                "FLUXER_APP_PROXY_PORT".to_owned(),
                Some(APP_PROXY_PORT.to_string()),
            ),
            (
                "FLUXER_STATIC_CDN_ENDPOINT".to_owned(),
                Some(public_url.clone()),
            ),
            (
                "FLUXER_STATIC_DIR".to_owned(),
                Some("fluxer_static".to_owned()),
            ),
            ("RELEASE_CHANNEL".to_owned(), Some("canary".to_owned())),
        ],
    });
    insert(DevTask {
        name: "media",
        args: strings(&[
            "cargo",
            "run",
            "-p",
            "fluxer-media-proxy",
            "--bin",
            "fluxer-media-proxy",
            "--",
            "--bind-host",
            ANY_HOST,
            "--port",
            &MEDIA_PROXY_PORT.to_string(),
            "--mode",
            "upload",
            "--storage-backend",
            "s3",
        ]),
        cwd: ROOT.clone(),
        env: Vec::new(),
    });
    insert(DevTask {
        name: "services",
        args: tool_args(&self_tool, &["rust-services"]),
        cwd: ROOT.clone(),
        env: Vec::new(),
    });
    insert(DevTask {
        name: "gateway",
        args: tool_args(&self_tool, &["gateway"]),
        cwd: ROOT.clone(),
        env: vec![(
            "FLUXER_DISABLE_RATE_LIMITS".to_owned(),
            Some("true".to_owned()),
        )],
    });
    insert(DevTask {
        name: "gateway-single",
        args: tool_args(&self_tool, &["gateway", "single"]),
        cwd: ROOT.clone(),
        env: vec![(
            "FLUXER_DISABLE_RATE_LIMITS".to_owned(),
            Some("true".to_owned()),
        )],
    });
    insert(DevTask {
        name: "marketing",
        args: strings(&[
            "cargo",
            "watch",
            "--no-dot-ignores",
            "-w",
            "fluxer_marketing/src",
            "-w",
            "fluxer_marketing/Cargo.toml",
            "-w",
            "fluxer_marketing/build.rs",
            "-w",
            "fluxer_marketing/locales",
            "-w",
            "fluxer_marketing/content",
            "-x",
            "run -p fluxer_marketing",
        ]),
        cwd: ROOT.clone(),
        env: vec![
            ("FLUXER_APP_ENDPOINT".to_owned(), Some(public_url.clone())),
            (
                "FLUXER_MARKETING_BASE_PATH".to_owned(),
                Some("/marketing".to_owned()),
            ),
            (
                "FLUXER_MARKETING_ENDPOINT".to_owned(),
                Some(marketing_endpoint),
            ),
            (
                "FLUXER_STATIC_CDN_ENDPOINT".to_owned(),
                Some(public_url.clone()),
            ),
        ],
    });
    insert(DevTask {
        name: "admin",
        args: strings(&[
            "cargo",
            "watch",
            "--no-dot-ignores",
            "-w",
            "fluxer_admin/src",
            "-w",
            "fluxer_admin/Cargo.toml",
            "-w",
            "fluxer_admin/build.rs",
            "-w",
            "fluxer_admin/openapi-admin.json",
            "-w",
            "fluxer_admin/static",
            "-x",
            "run -p fluxer_admin",
        ]),
        cwd: ROOT.clone(),
        env: vec![
            (
                "FLUXER_ADMIN_BASE_PATH".to_owned(),
                Some("/admin".to_owned()),
            ),
            ("FLUXER_ADMIN_ENDPOINT".to_owned(), Some(admin_endpoint)),
            ("FLUXER_ADMIN_PORT".to_owned(), Some(ADMIN_PORT.to_string())),
            (
                "FLUXER_API_ENDPOINT".to_owned(),
                Some(format!("{public_url}/api")),
            ),
            ("FLUXER_APP_ENDPOINT".to_owned(), Some(public_url.clone())),
            (
                "FLUXER_MEDIA_ENDPOINT".to_owned(),
                Some(format!("{public_url}/media")),
            ),
            ("FLUXER_STATIC_CDN_ENDPOINT".to_owned(), Some(public_url)),
            ("RELEASE_CHANNEL".to_owned(), Some("canary".to_owned())),
        ],
    });
    insert(DevTask {
        name: "desktop",
        args: tool_args(&self_tool, &["desktop", "run"]),
        cwd: DESKTOP_DIR.clone(),
        env: Vec::new(),
    });
    Ok(tasks)
}

async fn wait_for_search_backend_if_needed(selected: &[String]) -> Result<()> {
    if !selected
        .iter()
        .any(|name| name == "api" || name == "worker")
    {
        return Ok(());
    }
    let env = merged_env(None, true)?;
    let search_url = env
        .get("FLUXER_SEARCH_URL")
        .cloned()
        .unwrap_or_else(|| default_search_url(&env));
    wait_http(search_backend_label(&env), &search_url, 120).await
}

fn default_search_url(env: &BTreeMap<String, String>) -> String {
    if env
        .get("FLUXER_SEARCH_ENGINE")
        .is_some_and(|engine| engine == "meilisearch")
    {
        return "http://127.0.0.1:7700".to_owned();
    }
    "http://127.0.0.1:9200".to_owned()
}

fn search_backend_label(env: &BTreeMap<String, String>) -> &'static str {
    match env.get("FLUXER_SEARCH_ENGINE").map(String::as_str) {
        Some("meilisearch") => "Meilisearch",
        _ => "Elasticsearch",
    }
}

fn self_tool_command() -> Result<Vec<String>> {
    Ok(vec![std::env::current_exe()?.display().to_string()])
}

fn tool_args(self_tool: &[String], args: &[&str]) -> Vec<String> {
    let mut command = self_tool.to_vec();
    command.extend(args.iter().map(|arg| (*arg).to_owned()));
    command
}

fn strings(args: &[&str]) -> Vec<String> {
    args.iter().map(|arg| (*arg).to_owned()).collect()
}

fn public_url() -> String {
    std::env::var("FLUXER_PUBLIC_URL")
        .unwrap_or_else(|_| format!("http://localhost:{DEV_PROXY_PORT}"))
}

fn ensure_js_dependencies_if_needed(selected: &[String]) -> Result<()> {
    if !selected_needs_js_dependency_preflight(selected) {
        return Ok(());
    }
    run_command(
        &["pnpm", "install", "--frozen-lockfile"],
        RunOptions {
            env: PNPM_INSTALL_ENV
                .iter()
                .map(|(key, value)| ((*key).to_owned(), Some((*value).to_owned())))
                .collect(),
            ..RunOptions::default()
        },
    )?;
    Ok(())
}

fn selected_needs_js_dependency_preflight(selected: &[String]) -> bool {
    selected
        .iter()
        .any(|name| JS_DEPENDENCY_TASKS.contains(&name.as_str()))
}

fn selected_needs_object_store(selected: &[String]) -> bool {
    selected
        .iter()
        .any(|name| OBJECT_STORE_TASKS.contains(&name.as_str()))
}

fn object_store_monitor_interval() -> Duration {
    let seconds = std::env::var("FLUXER_DEV_OBJECT_STORE_MONITOR_INTERVAL_SECS")
        .ok()
        .and_then(|value| value.parse::<u64>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(DEFAULT_OBJECT_STORE_MONITOR_INTERVAL_SECS);
    Duration::from_secs(seconds)
}

fn start_task(task: &DevTask) -> Result<Child> {
    let env = merged_env(Some(&task.env), true)?;
    println!("[{}] $ {}", task.name, format_command(&task.args));
    let mut command = Command::new(&task.args[0]);
    command
        .args(&task.args[1..])
        .current_dir(&task.cwd)
        .env_clear()
        .envs(env)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    #[cfg(unix)]
    unsafe {
        use std::os::unix::process::CommandExt;
        command.pre_exec(|| {
            libc::setsid();
            Ok(())
        });
    }
    let mut child = command.spawn()?;
    if let Some(stdout) = child.stdout.take() {
        let label = task.name.to_owned();
        std::thread::spawn(move || prefix_output(&label, stdout));
    }
    if let Some(stderr) = child.stderr.take() {
        let label = task.name.to_owned();
        std::thread::spawn(move || prefix_output(&label, stderr));
    }
    Ok(child)
}

async fn wait_for_rust_services(processes: &mut [Child]) -> Result<()> {
    let timeout = std::env::var("FLUXER_DEV_RUST_SERVICE_READY_TIMEOUT")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(240);
    let services_index = processes
        .len()
        .checked_sub(1)
        .expect("services process was just pushed");
    for spec in rust_services() {
        for (mode, port) in [("router", spec.port_base), ("shard", spec.port_base + 1)] {
            check_startup_processes(processes, services_index, spec.name, mode)?;
            wait_http(
                &format!("Rust service {}:{mode}", spec.name),
                &format!("http://{LOOPBACK_HOST}:{port}/_health"),
                timeout,
            )
            .await?;
        }
    }
    Ok(())
}

async fn wait_for_api(processes: &mut [Child]) -> Result<()> {
    let timeout = std::env::var("FLUXER_DEV_API_READY_TIMEOUT")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(180);
    let api_index = processes
        .len()
        .checked_sub(1)
        .expect("api process was just pushed");
    wait_http_with_startup_checks(
        processes,
        api_index,
        "API",
        &format!("http://{LOOPBACK_HOST}:{API_PORT}/_health"),
        timeout,
    )
    .await
}

async fn wait_for_cloudflare_tunnel_routes(
    processes: &mut [Child],
    selected: &[String],
) -> Result<()> {
    let timeout = std::env::var("FLUXER_CLOUDFLARE_TUNNEL_ROUTE_READY_TIMEOUT")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(300);
    let base_url = format!("http://{LOOPBACK_HOST}:{DEV_PROXY_PORT}");
    let has_task = |task: &str| selected.iter().any(|name| name == task);
    if has_task("api") {
        wait_http_for_dev_tasks(
            processes,
            "dev proxy api",
            &format!("{base_url}/api/_health"),
            timeout,
        )
        .await?;
    }
    if has_task("media") {
        wait_http_for_dev_tasks(
            processes,
            "dev proxy media",
            &format!("{base_url}/media/_health"),
            timeout,
        )
        .await?;
    }
    if has_task("gateway") || has_task("gateway-single") {
        wait_http_for_dev_tasks(
            processes,
            "dev proxy gateway",
            &format!("{base_url}/gateway/_health"),
            timeout,
        )
        .await?;
    }
    if has_task("marketing") {
        wait_http_for_dev_tasks(
            processes,
            "dev proxy marketing",
            &format!("{base_url}/marketing/"),
            timeout,
        )
        .await?;
    }
    if has_task("admin") {
        wait_http_for_dev_tasks(
            processes,
            "dev proxy admin",
            &format!("{base_url}/admin/"),
            timeout,
        )
        .await?;
    }
    if has_task("app") && has_task("app-proxy") {
        wait_http_for_dev_tasks(processes, "dev proxy app", &format!("{base_url}/"), timeout)
            .await?;
    }
    Ok(())
}

async fn wait_http_for_dev_tasks(
    processes: &mut [Child],
    name: &str,
    url: &str,
    timeout_secs: u64,
) -> Result<()> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(5))
        .build()?;
    let deadline = Instant::now() + Duration::from_secs(timeout_secs);
    let mut last_error = None;
    while Instant::now() < deadline {
        check_dev_startup_processes(processes, name)?;
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
    check_dev_startup_processes(processes, name)?;
    bail!(
        "Timed out waiting for {name} at {url}: {}",
        last_error.unwrap_or_else(|| "unknown error".to_owned())
    );
}

async fn wait_http_with_startup_checks(
    processes: &mut [Child],
    watched_index: usize,
    name: &str,
    url: &str,
    timeout_secs: u64,
) -> Result<()> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(5))
        .build()?;
    let deadline = Instant::now() + Duration::from_secs(timeout_secs);
    let mut last_error = None;
    while Instant::now() < deadline {
        check_startup_task_processes(processes, watched_index, name)?;
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
    check_startup_task_processes(processes, watched_index, name)?;
    bail!(
        "Timed out waiting for {name} at {url}: {}",
        last_error.unwrap_or_else(|| "unknown error".to_owned())
    );
}

fn check_dev_startup_processes(processes: &mut [Child], waiting_for: &str) -> Result<()> {
    for process in processes {
        if let Some(status) = process.try_wait()? {
            let code = status.code().unwrap_or(1);
            bail!("Dev task exited with status {code} before {waiting_for} became ready");
        }
    }
    Ok(())
}

fn check_startup_task_processes(
    processes: &mut [Child],
    watched_index: usize,
    waiting_for: &str,
) -> Result<()> {
    for (index, process) in processes.iter_mut().enumerate() {
        if let Some(status) = process.try_wait()? {
            let code = status.code().unwrap_or(1);
            if index == watched_index {
                bail!("{waiting_for} task exited with status {code} before it became ready");
            }
            bail!("Dev task exited with status {code} before {waiting_for} became ready");
        }
    }
    Ok(())
}

fn check_startup_processes(
    processes: &mut [Child],
    services_index: usize,
    service_name: &str,
    mode: &str,
) -> Result<()> {
    for (index, process) in processes.iter_mut().enumerate() {
        if let Some(status) = process.try_wait()? {
            let code = status.code().unwrap_or(1);
            if index == services_index {
                bail!(
                    "Rust service supervisor exited with status {code} before {service_name}:{mode} became ready"
                );
            }
            bail!(
                "Dev task exited with status {code} before Rust service {service_name}:{mode} became ready"
            );
        }
    }
    Ok(())
}

fn prefix_output(label: &str, reader: impl std::io::Read) {
    use std::io::{BufRead, BufReader};
    for line in BufReader::new(reader).lines().map_while(|line| line.ok()) {
        println!("[{label}] {line}");
    }
}

#[allow(dead_code)]
fn _cwd_is_root(path: &Path) -> bool {
    path == ROOT.as_path()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_tasks_keep_legacy_order() {
        assert_eq!(
            DEFAULT_TASKS,
            &[
                "proxy",
                "services",
                "media",
                "gateway",
                "marketing",
                "admin",
                "api",
                "worker",
                "app",
                "app-proxy"
            ]
        );
    }

    #[test]
    fn cloudflare_tunnel_task_is_deferred_until_after_stack_tasks() {
        let selected = normalize_selected_tasks(
            DEFAULT_TASKS
                .iter()
                .map(|name| (*name).to_owned())
                .collect(),
            true,
        );
        assert_eq!(
            selected.last().map(String::as_str),
            Some(CLOUDFLARE_TUNNEL_TASK)
        );
        assert_eq!(selected[..DEFAULT_TASKS.len()], *DEFAULT_TASKS);

        let selected = normalize_selected_tasks(
            vec![
                CLOUDFLARE_TUNNEL_TASK.to_owned(),
                "proxy".to_owned(),
                "media".to_owned(),
            ],
            false,
        );
        assert_eq!(
            selected,
            vec![
                "proxy".to_owned(),
                "media".to_owned(),
                CLOUDFLARE_TUNNEL_TASK.to_owned()
            ]
        );
    }

    #[test]
    fn js_dependency_preflight_only_runs_for_js_tasks() {
        assert!(selected_needs_js_dependency_preflight(&["api".to_owned()]));
        assert!(selected_needs_js_dependency_preflight(&[
            "worker".to_owned()
        ]));
        assert!(selected_needs_js_dependency_preflight(&["app".to_owned()]));
        assert!(selected_needs_js_dependency_preflight(
            &["admin".to_owned()]
        ));
        assert!(!selected_needs_js_dependency_preflight(&[
            "proxy".to_owned(),
            "gateway".to_owned(),
            "media".to_owned()
        ]));
    }

    #[test]
    fn object_store_preflight_runs_for_upload_capable_tasks() {
        assert!(selected_needs_object_store(&["api".to_owned()]));
        assert!(selected_needs_object_store(&["worker".to_owned()]));
        assert!(selected_needs_object_store(&["media".to_owned()]));
        assert!(!selected_needs_object_store(&[
            "proxy".to_owned(),
            "gateway".to_owned(),
            "app".to_owned()
        ]));
    }

    #[test]
    fn task_table_contains_recursive_binary_tasks() {
        let tasks = task_table().unwrap();
        let expected_public_url = public_url();
        let expected_app_port = APP_PORT.to_string();
        let expected_admin_port = ADMIN_PORT.to_string();
        let expected_app_upstream_url = format!("http://{LOOPBACK_HOST}:{APP_PORT}/");
        assert!(tasks["proxy"].args.ends_with(&["proxy".to_owned()]));
        assert!(
            tasks["gateway-single"]
                .args
                .ends_with(&["gateway".to_owned(), "single".to_owned()])
        );
        assert_eq!(
            tasks["gateway"]
                .env
                .iter()
                .find(|(key, _)| key == "FLUXER_DISABLE_RATE_LIMITS")
                .unwrap()
                .1
                .as_deref(),
            Some("true")
        );
        assert_eq!(
            tasks["gateway-single"]
                .env
                .iter()
                .find(|(key, _)| key == "FLUXER_DISABLE_RATE_LIMITS")
                .unwrap()
                .1
                .as_deref(),
            Some("true")
        );
        assert_eq!(
            tasks["api"]
                .env
                .iter()
                .find(|(key, _)| key == "FLUXER_S3_PUBLIC_ENDPOINT")
                .unwrap()
                .1
                .as_deref(),
            Some(expected_public_url.as_str())
        );
        assert_eq!(
            tasks["api"]
                .env
                .iter()
                .find(|(key, _)| key == "FLUXER_S3_FORCE_PATH_STYLE")
                .unwrap()
                .1
                .as_deref(),
            Some("true")
        );
        assert!(crate::manifest::PROXY_ROUTES.iter().any(|route| {
            route.prefix == "/fluxer-uploads" && route.host == LOOPBACK_HOST && route.port == 8333
        }));
        assert!(crate::manifest::PROXY_ROUTES.iter().any(|route| {
            route.prefix == "/admin"
                && route.host == LOOPBACK_HOST
                && route.port == ADMIN_PORT
                && route.strip_prefix
        }));
        assert_eq!(
            tasks["app"]
                .env
                .iter()
                .find(|(key, _)| key == "FLUXER_APP_DEV_PORT")
                .unwrap()
                .1
                .as_deref(),
            Some(expected_app_port.as_str())
        );
        assert_eq!(
            tasks["app"]
                .env
                .iter()
                .find(|(key, _)| key == "FLUXER_STATIC_CDN_ENDPOINT")
                .unwrap()
                .1
                .as_deref(),
            Some(expected_public_url.as_str())
        );
        assert_eq!(
            tasks["app-proxy"]
                .env
                .iter()
                .find(|(key, _)| key == "FLUXER_APP_PROXY_INDEX_UPSTREAM_URL")
                .unwrap()
                .1
                .as_deref(),
            Some(expected_app_upstream_url.as_str())
        );
        assert_eq!(
            tasks["app"]
                .env
                .iter()
                .find(|(key, _)| key == "PUBLIC_STATIC_CDN_ENDPOINT")
                .unwrap()
                .1
                .as_deref(),
            Some(expected_public_url.as_str())
        );
        assert_eq!(
            tasks["app-proxy"]
                .env
                .iter()
                .find(|(key, _)| key == "RELEASE_CHANNEL")
                .unwrap()
                .1
                .as_deref(),
            Some("canary")
        );
        assert!(tasks["media"].args.starts_with(&[
            "cargo".to_owned(),
            "run".to_owned(),
            "-p".to_owned(),
            "fluxer-media-proxy".to_owned()
        ]));
        assert_eq!(
            tasks["marketing"].args,
            vec![
                "cargo".to_owned(),
                "watch".to_owned(),
                "--no-dot-ignores".to_owned(),
                "-w".to_owned(),
                "fluxer_marketing/src".to_owned(),
                "-w".to_owned(),
                "fluxer_marketing/Cargo.toml".to_owned(),
                "-w".to_owned(),
                "fluxer_marketing/build.rs".to_owned(),
                "-w".to_owned(),
                "fluxer_marketing/locales".to_owned(),
                "-w".to_owned(),
                "fluxer_marketing/content".to_owned(),
                "-x".to_owned(),
                "run -p fluxer_marketing".to_owned()
            ]
        );
        assert_eq!(
            tasks["admin"].args,
            vec![
                "cargo".to_owned(),
                "watch".to_owned(),
                "--no-dot-ignores".to_owned(),
                "-w".to_owned(),
                "fluxer_admin/src".to_owned(),
                "-w".to_owned(),
                "fluxer_admin/Cargo.toml".to_owned(),
                "-w".to_owned(),
                "fluxer_admin/build.rs".to_owned(),
                "-w".to_owned(),
                "fluxer_admin/openapi-admin.json".to_owned(),
                "-w".to_owned(),
                "fluxer_admin/static".to_owned(),
                "-x".to_owned(),
                "run -p fluxer_admin".to_owned()
            ]
        );
        assert_eq!(
            tasks["admin"]
                .env
                .iter()
                .find(|(key, _)| key == "FLUXER_ADMIN_BASE_PATH")
                .unwrap()
                .1
                .as_deref(),
            Some("/admin")
        );
        assert_eq!(
            tasks["admin"]
                .env
                .iter()
                .find(|(key, _)| key == "FLUXER_ADMIN_PORT")
                .unwrap()
                .1
                .as_deref(),
            Some(expected_admin_port.as_str())
        );
        assert!(tasks.contains_key(CLOUDFLARE_TUNNEL_TASK));
    }
}
