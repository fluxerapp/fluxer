// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result, bail};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::LazyLock;

pub static ROOT: LazyLock<PathBuf> = LazyLock::new(|| {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("tools/dev has a repository grandparent")
        .to_path_buf()
});

pub static DEV_STATE_DIR: LazyLock<PathBuf> = LazyLock::new(|| ROOT.join(".fluxer/dev"));
pub static DEV_CASSANDRA_DIR: LazyLock<PathBuf> = LazyLock::new(|| DEV_STATE_DIR.join("cassandra"));
pub static DEV_GATEWAY_DIR: LazyLock<PathBuf> = LazyLock::new(|| DEV_STATE_DIR.join("gateway"));
pub static DEV_LOG_DIR: LazyLock<PathBuf> = LazyLock::new(|| DEV_STATE_DIR.join("logs"));
pub static DEV_SEAWEEDFS_DIR: LazyLock<PathBuf> = LazyLock::new(|| DEV_STATE_DIR.join("seaweedfs"));
pub static DEV_SEAWEEDFS_PID_FILE: LazyLock<PathBuf> =
    LazyLock::new(|| DEV_SEAWEEDFS_DIR.join("seaweedfs.pid"));
pub static DEV_ENV_FILE: LazyLock<PathBuf> =
    LazyLock::new(|| ROOT.join("config/env/development.env"));
pub static DEV_LOCAL_ENV_FILE: LazyLock<PathBuf> =
    LazyLock::new(|| ROOT.join("config/env/local.env"));
pub static ROOT_LOCAL_ENV_FILE: LazyLock<PathBuf> = LazyLock::new(|| ROOT.join(".env.local"));
pub static TARGET_DIR: LazyLock<PathBuf> = LazyLock::new(|| ROOT.join("target"));
pub static DESKTOP_DIR: LazyLock<PathBuf> = LazyLock::new(|| ROOT.join("fluxer_desktop"));
pub static GATEWAY_CONFIG_DIR: LazyLock<PathBuf> =
    LazyLock::new(|| ROOT.join("fluxer_gateway/config"));

pub fn ensure_state_dirs() -> Result<()> {
    for path in [
        DEV_STATE_DIR.as_path(),
        DEV_CASSANDRA_DIR.as_path(),
        DEV_GATEWAY_DIR.as_path(),
        DEV_LOG_DIR.as_path(),
        DEV_SEAWEEDFS_DIR.as_path(),
    ] {
        std::fs::create_dir_all(path)
            .with_context(|| format!("failed to create {}", path.display()))?;
    }
    Ok(())
}

pub fn ensure_writable_dev_paths() -> Result<()> {
    for path in [DEV_STATE_DIR.as_path(), TARGET_DIR.as_path()] {
        std::fs::create_dir_all(path)
            .with_context(|| format!("failed to create {}", path.display()))?;
        if is_writable(path) {
            continue;
        }
        let sudo = which("sudo").ok_or_else(|| {
            anyhow::anyhow!("{} is not writable and sudo is unavailable", path.display())
        })?;
        let owner = format!("{}:{}", current_uid(), current_gid());
        let status = Command::new(sudo)
            .args(["chown", "-R", &owner])
            .arg(path)
            .status()
            .with_context(|| format!("failed to chown {}", path.display()))?;
        if !status.success() {
            bail!("sudo chown failed for {}", path.display());
        }
    }
    Ok(())
}

pub fn which(name: &str) -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let candidate = dir.join(name);
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

fn is_writable(path: &Path) -> bool {
    tempfile::NamedTempFile::new_in(path).is_ok()
}

#[cfg(unix)]
fn current_uid() -> u32 {
    unsafe { libc::geteuid() }
}

#[cfg(unix)]
fn current_gid() -> u32 {
    unsafe { libc::getegid() }
}

#[cfg(not(unix))]
fn current_uid() -> u32 {
    0
}

#[cfg(not(unix))]
fn current_gid() -> u32 {
    0
}
