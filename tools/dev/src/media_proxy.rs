// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::object_store::{ensure_s3_buckets, wait_s3_api};
use crate::paths::{
    DEV_LOG_DIR, DEV_SEAWEEDFS_DIR, DEV_SEAWEEDFS_PID_FILE, ensure_writable_dev_paths, which,
};
use anyhow::{Context, Result, bail};
use std::fs::{self, OpenOptions};
use std::net::{SocketAddr, TcpStream, ToSocketAddrs};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};
use tokio::time::sleep;

const DEV_S3_BUCKETS: &str =
    "fluxer,fluxer-uploads,fluxer-downloads,fluxer-reports,fluxer-harvests,fluxer-static";
const DEV_S3_ACCESS_KEY_ID: &str = "fluxer";
const DEV_S3_SECRET_ACCESS_KEY: &str = "fluxer-secret";
const DEV_S3_HOST: &str = "127.0.0.1";
const DEV_S3_PORT: u16 = 8333;

pub async fn run_dev_media_doctor(
    repair: bool,
    base_url: &str,
    media_path: Option<&str>,
) -> Result<()> {
    let media_health_url = format!("{}/media/_health", base_url.trim_end_matches('/'));
    ensure_dev_object_store(repair, 60).await?;
    wait_for_success(&media_health_url, 30).await?;
    println!("Media proxy health: ok ({media_health_url})");
    if let Some(path) = media_path {
        check_dev_media_path(base_url, path).await?;
    }
    println!("Fluxer media proxy doctor passed.");
    Ok(())
}

pub async fn ensure_dev_object_store(repair: bool, repair_timeout_secs: u64) -> Result<()> {
    match wait_s3_api(5).await {
        Ok(()) => {}
        Err(error) if repair => {
            println!("SeaweedFS S3 check failed: {error}");
            start_dev_seaweedfs()?;
            wait_s3_api(repair_timeout_secs).await?;
        }
        Err(error) => {
            bail!(
                "SeaweedFS S3 is unreachable: {error}\nRun `fluxer-dev media-proxy doctor --repair` to start the local dev object store."
            );
        }
    }
    ensure_s3_buckets()?;
    Ok(())
}

pub fn run_rust_stress_smoke() -> Result<()> {
    crate::proc::run(&["cargo", "test", "-p", "fluxer-media-proxy"])?;
    crate::proc::run(&[
        "cargo",
        "bench",
        "-p",
        "fluxer-media-proxy",
        "--bench",
        "core",
        "--",
        "--sample-size",
        "10",
        "--warm-up-time",
        "1",
        "--measurement-time",
        "1",
    ])?;

    if which("cargo-fuzz").is_none() {
        eprintln!("cargo-fuzz not installed; skipping fuzz smoke");
        return Ok(());
    }
    let use_nightly = rustup_has_nightly();
    for target in ["parsers", "signing_external_path", "thumbhash"] {
        run_fuzz_smoke(target, use_nightly)?;
    }
    Ok(())
}

fn rustup_has_nightly() -> bool {
    let Ok(output) = Command::new("rustup").args(["toolchain", "list"]).output() else {
        return false;
    };
    if !output.status.success() {
        return false;
    }
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .any(|line| line.starts_with("nightly"))
}

fn run_fuzz_smoke(target: &str, use_nightly: bool) -> Result<()> {
    if use_nightly {
        crate::proc::run(&[
            "cargo",
            "+nightly",
            "fuzz",
            "run",
            target,
            "--",
            "-runs=1000",
        ])
    } else {
        crate::proc::run(&["cargo", "fuzz", "run", target, "--", "-runs=1000"])
    }
}

fn start_dev_seaweedfs() -> Result<()> {
    ensure_writable_dev_paths()?;
    if tcp_reachable(DEV_S3_HOST, DEV_S3_PORT) {
        println!("SeaweedFS S3 is already reachable at {DEV_S3_HOST}:{DEV_S3_PORT}");
        return Ok(());
    }

    fs::create_dir_all(DEV_SEAWEEDFS_DIR.as_path()).with_context(|| {
        format!(
            "failed to create SeaweedFS data dir {}",
            DEV_SEAWEEDFS_DIR.display()
        )
    })?;
    fs::create_dir_all(DEV_LOG_DIR.as_path())
        .with_context(|| format!("failed to create log dir {}", DEV_LOG_DIR.display()))?;

    if let Some(pid) = read_dev_seaweedfs_pid()? {
        if managed_seaweedfs_process_running(pid) {
            eprintln!("Stopping unresponsive managed SeaweedFS process {pid}.");
            stop_managed_seaweedfs_process(pid);
        }
        let _ = fs::remove_file(DEV_SEAWEEDFS_PID_FILE.as_path());
    }

    let weed = which("weed").ok_or_else(|| {
        anyhow::anyhow!(
            "missing `weed` binary. Rebuild the devcontainer so SeaweedFS can run inside it."
        )
    })?;
    let log_path = DEV_LOG_DIR.join("seaweedfs.log");
    let log = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_path)
        .with_context(|| format!("failed to open SeaweedFS log {}", log_path.display()))?;
    let stderr = log
        .try_clone()
        .context("failed to clone SeaweedFS log handle")?;
    let data_dir_arg = format!("-dir={}", DEV_SEAWEEDFS_DIR.display());
    let child = Command::new(weed)
        .args(["-logtostderr=true", "mini", &data_dir_arg])
        .env("AWS_ACCESS_KEY_ID", DEV_S3_ACCESS_KEY_ID)
        .env("AWS_SECRET_ACCESS_KEY", DEV_S3_SECRET_ACCESS_KEY)
        .env("S3_BUCKET", DEV_S3_BUCKETS)
        .stdin(Stdio::null())
        .stdout(Stdio::from(log))
        .stderr(Stdio::from(stderr))
        .spawn()
        .context("failed to start SeaweedFS")?;
    let pid = child.id();
    fs::write(DEV_SEAWEEDFS_PID_FILE.as_path(), pid.to_string()).with_context(|| {
        format!(
            "failed to write SeaweedFS pid file {}",
            DEV_SEAWEEDFS_PID_FILE.display()
        )
    })?;
    println!(
        "Started SeaweedFS dev object store with pid {pid}; logs: {}",
        log_path.display()
    );
    Ok(())
}

fn read_dev_seaweedfs_pid() -> Result<Option<u32>> {
    let text = match fs::read_to_string(DEV_SEAWEEDFS_PID_FILE.as_path()) {
        Ok(text) => text,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => return Err(error).context("failed to read SeaweedFS pid file"),
    };
    let pid = text.trim().parse().with_context(|| {
        format!(
            "invalid SeaweedFS pid file: {}",
            DEV_SEAWEEDFS_PID_FILE.display()
        )
    })?;
    Ok(Some(pid))
}

fn managed_seaweedfs_process_running(pid: u32) -> bool {
    let cmdline = fs::read(format!("/proc/{pid}/cmdline")).unwrap_or_default();
    cmdline
        .split(|byte| *byte == 0)
        .any(|arg| arg.ends_with(b"/weed") || arg == b"weed")
}

#[cfg(unix)]
fn stop_managed_seaweedfs_process(pid: u32) {
    unsafe {
        libc::kill(pid as i32, libc::SIGTERM);
    }
    std::thread::sleep(Duration::from_secs(2));
    if managed_seaweedfs_process_running(pid) {
        unsafe {
            libc::kill(pid as i32, libc::SIGKILL);
        }
    }
}

#[cfg(not(unix))]
fn stop_managed_seaweedfs_process(_pid: u32) {}

async fn check_dev_media_path(base_url: &str, path: &str) -> Result<()> {
    let path = if path.starts_with('/') {
        path.to_owned()
    } else {
        format!("/{path}")
    };
    let url = format!("{}{}", base_url.trim_end_matches('/'), path);
    let status = request_status(&url).await?;
    if status == 502 {
        bail!("Media proxy returned 502 for {url}; storage is still unreachable from the proxy.");
    }
    if status >= 500 {
        bail!("Media proxy returned {status} for {url}");
    }
    println!("Media path check: {status} ({url})");
    Ok(())
}
async fn request_status(url: &str) -> Result<u16> {
    let client = reqwest::Client::builder()
        .timeout(Duration::from_secs(20))
        .build()?;
    Ok(client.get(url).send().await?.status().as_u16())
}

async fn wait_for_success(url: &str, timeout_secs: u64) -> Result<()> {
    let deadline = Instant::now() + Duration::from_secs(timeout_secs);
    let mut last_error = None;
    while Instant::now() < deadline {
        match request_status(url).await {
            Ok(status) if (200..300).contains(&status) => return Ok(()),
            Ok(status) => last_error = Some(format!("HTTP {status}")),
            Err(error) => last_error = Some(error.to_string()),
        }
        sleep(Duration::from_millis(250)).await;
    }
    bail!(
        "Timed out waiting for {url}: {}",
        last_error.unwrap_or_else(|| "unknown error".to_owned())
    );
}

fn tcp_reachable(host: &str, port: u16) -> bool {
    let address = format!("{host}:{port}");
    TcpStream::connect_timeout(
        &address
            .to_socket_addrs()
            .ok()
            .and_then(|mut addrs| addrs.next())
            .unwrap_or_else(|| SocketAddr::from(([127, 0, 0, 1], port))),
        Duration::from_secs(2),
    )
    .is_ok()
}
