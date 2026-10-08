// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::desktop::{copy_tree, remove_path};
use crate::paths::{DEV_STATE_DIR, ROOT};
use crate::proc::{RunOptions, run_command};
use anyhow::{Context, Result, bail, ensure};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

pub const DEVELOPMENT_PACKAGE_ORIGIN: &str = "http://localhost:48780";
const PKGS_ROOT_ENV: &str = "FLUXER_DEV_PKGS_ROOT";
const PKGS_CONTAINER: &str = "fluxer-dev-pkgs";
const PKGS_IMAGE: &str = "nginx:1.28";
const PKGS_PUBLISH: &str = "127.0.0.1:48780:80";
const PKGS_PROBE_ORIGIN: &str = "http://127.0.0.1:48780";
const PKGS_CONTAINER_ROOT: &str = "/srv/pkgs";
const PKGS_CONTAINER_CONFIG: &str = "/etc/nginx/conf.d/default.conf";
const PKGS_NGINX_CONFIG: &str = "server {
    listen 80;
    root /srv/pkgs;
    etag on;
    location ~ /modules\\.json$ {
        add_header Cache-Control \"max-age=0, must-revalidate\" always;
    }
    location ~ /package\\.br$ {
        add_header Cache-Control \"public, max-age=31536000, immutable\" always;
    }
}
";
const MODULES_KEY_SEGMENT: &str = "modules";
const MODULE_PACKAGE_NAME: &str = "package.br";
const CHANNEL_MANIFEST_NAME: &str = "modules.json";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleCoordinate {
    pub channel: String,
    pub platform: String,
    pub arch: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PackedModulesStamp {
    fingerprint: String,
    version: String,
    pub_date: String,
}

impl PackedModulesStamp {
    fn parse(raw: &str) -> Option<Self> {
        let mut parts = raw.split_whitespace();
        let stamp = Self {
            fingerprint: parts.next()?.to_owned(),
            version: parts.next()?.to_owned(),
            pub_date: parts.next()?.to_owned(),
        };
        parts.next().is_none().then_some(stamp)
    }

    fn render(&self) -> String {
        format!("{} {} {}\n", self.fingerprint, self.version, self.pub_date)
    }
}

fn modules_state_dir() -> PathBuf {
    DEV_STATE_DIR.join("desktop-modules")
}

fn modules_workdir() -> PathBuf {
    modules_state_dir().join("work")
}

fn modules_stamp_path() -> PathBuf {
    modules_state_dir().join("modules.stamp")
}

fn publish_staging_dir() -> PathBuf {
    modules_state_dir().join("publish")
}

fn pkgs_state_dir() -> PathBuf {
    DEV_STATE_DIR.join("desktop-pkgs")
}

pub fn pkgs_root() -> PathBuf {
    match env::var(PKGS_ROOT_ENV) {
        Ok(value) if !value.trim().is_empty() => PathBuf::from(value.trim()),
        _ => pkgs_state_dir().join("root"),
    }
}

pub fn host_module_platform() -> Result<&'static str> {
    match env::consts::OS {
        "macos" => Ok("darwin"),
        "linux" => Ok("linux"),
        "windows" => Ok("win32"),
        other => bail!("no desktop module platform for host OS {other}"),
    }
}

pub fn rfc3339_utc(now: SystemTime) -> String {
    let seconds = now
        .duration_since(UNIX_EPOCH)
        .map(|elapsed| elapsed.as_secs())
        .unwrap_or(0);
    let (year, month, day) = crate::desktop::civil_from_days((seconds / 86_400) as i64);
    let second_of_day = seconds % 86_400;
    format!(
        "{year:04}-{month:02}-{day:02}T{:02}:{:02}:{:02}Z",
        second_of_day / 3600,
        (second_of_day % 3600) / 60,
        second_of_day % 60
    )
}

fn run_ci_step(step: &str, cwd: &Path, env: Vec<(String, Option<String>)>) -> Result<()> {
    let manifest = ROOT.join("tools/ci/Cargo.toml");
    let manifest = manifest.display().to_string();
    run_command(
        &[
            "cargo",
            "run",
            "--quiet",
            "--manifest-path",
            &manifest,
            "--",
            "build-desktop",
            "--step",
            step,
        ],
        RunOptions {
            cwd,
            env,
            load_default_env: false,
            ..RunOptions::default()
        },
    )
    .map(drop)
}

fn source_sha() -> String {
    Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(ROOT.as_path())
        .stderr(Stdio::null())
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_owned())
        .unwrap_or_default()
}

fn pack_renderer_modules(
    renderer: &Path,
    fingerprint: &str,
    version: &str,
    channel: &str,
) -> Result<PackedModulesStamp> {
    let workdir = modules_workdir();
    let stamp_path = modules_stamp_path();
    if let Some(stamp) = fs::read_to_string(&stamp_path)
        .ok()
        .as_deref()
        .and_then(PackedModulesStamp::parse)
        && stamp.fingerprint == fingerprint
        && workdir.join("desktop-modules").is_dir()
    {
        println!(
            "Reusing the renderer modules packed at {} from unchanged sources",
            stamp.version
        );
        return Ok(stamp);
    }
    println!("Packing the renderer into desktop modules...");
    remove_path(&stamp_path)?;
    remove_path(&workdir)?;
    let renderer_copy = workdir.join("fluxer_desktop/dist/renderer");
    copy_tree(renderer, &renderer_copy, &|_| false)?;
    let static_link = workdir.join("fluxer_static");
    #[cfg(unix)]
    std::os::unix::fs::symlink(ROOT.join("fluxer_static"), &static_link)
        .with_context(|| format!("failed to link {}", static_link.display()))?;
    #[cfg(not(unix))]
    copy_tree(&ROOT.join("fluxer_static"), &static_link, &|_| false)?;
    let env = vec![
        ("WORKDIR".to_owned(), Some(workdir.display().to_string())),
        ("BUILD_VERSION".to_owned(), Some(version.to_owned())),
        ("BUILD_CHANNEL".to_owned(), Some(channel.to_owned())),
        ("SOURCE_SHA".to_owned(), Some(source_sha())),
    ];
    run_ci_step("split_modules", &workdir, env.clone())?;
    run_ci_step("pack_modules", &workdir, env)?;
    remove_path(&workdir.join("fluxer_desktop"))?;
    let stamp = PackedModulesStamp {
        fingerprint: fingerprint.to_owned(),
        version: version.to_owned(),
        pub_date: rfc3339_utc(SystemTime::now()),
    };
    fs::write(&stamp_path, stamp.render())
        .with_context(|| format!("failed to write {}", stamp_path.display()))?;
    Ok(stamp)
}

fn stage_channel_manifest(
    stamp: &PackedModulesStamp,
    target: &ModuleCoordinate,
) -> Result<PathBuf> {
    let staging = publish_staging_dir();
    remove_path(&staging)?;
    let payload_root = staging.join("payload_tree/desktop");
    fs::create_dir_all(&payload_root)
        .with_context(|| format!("failed to create {}", payload_root.display()))?;
    run_ci_step(
        "build_module_manifest",
        &staging,
        vec![
            (
                "WORKDIR".to_owned(),
                Some(modules_workdir().display().to_string()),
            ),
            ("CHANNEL".to_owned(), Some(target.channel.clone())),
            ("VERSION".to_owned(), Some(stamp.version.clone())),
            ("PUB_DATE".to_owned(), Some(stamp.pub_date.clone())),
            ("DESKTOP_MODULE_ONLY".to_owned(), Some("1".to_owned())),
            ("PLATFORM".to_owned(), Some(target.platform.clone())),
            ("ARCH".to_owned(), Some(target.arch.clone())),
            (
                "PKGS_DOWNLOAD_BASE_URL".to_owned(),
                Some(DEVELOPMENT_PACKAGE_ORIGIN.to_owned()),
            ),
        ],
    )?;
    Ok(payload_root)
}

fn sha256_file(path: &Path) -> Result<String> {
    let mut file =
        fs::File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let mut hasher = Sha256::new();
    let mut buffer = vec![0u8; 1 << 20];
    loop {
        let read = io::Read::read(&mut file, &mut buffer)
            .with_context(|| format!("failed to read {}", path.display()))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    Ok(hasher
        .finalize()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect())
}

fn write_atomically(path: &Path, bytes: &[u8]) -> Result<()> {
    let parent = path
        .parent()
        .with_context(|| format!("{} has no parent directory", path.display()))?;
    fs::create_dir_all(parent).with_context(|| format!("failed to create {}", parent.display()))?;
    let next = parent.join(format!(
        ".{}.next",
        path.file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("file")
    ));
    fs::write(&next, bytes).with_context(|| format!("failed to write {}", next.display()))?;
    fs::rename(&next, path).with_context(|| format!("failed to move {} into place", path.display()))
}

fn copy_package(source: &Path, destination: &Path) -> Result<()> {
    if destination.is_file() {
        return Ok(());
    }
    let parent = destination
        .parent()
        .with_context(|| format!("{} has no parent directory", destination.display()))?;
    fs::create_dir_all(parent).with_context(|| format!("failed to create {}", parent.display()))?;
    let next = parent.join(".package.br.next");
    fs::copy(source, &next)
        .with_context(|| format!("failed to copy {} to {}", source.display(), next.display()))?;
    fs::rename(&next, destination)
        .with_context(|| format!("failed to move {} into place", destination.display()))
}

fn manifest_without_metadata_version(manifest: &Value) -> Value {
    let mut stripped = manifest.clone();
    if let Some(object) = stripped.as_object_mut() {
        object.remove("metadata_version");
    }
    stripped
}

fn metadata_version_of(manifest: &Value, path: &Path) -> Result<u64> {
    manifest
        .get("metadata_version")
        .and_then(Value::as_u64)
        .with_context(|| format!("{} declares no valid metadata_version", path.display()))
}

pub fn next_metadata_version(now: u64, own: u64, live: Option<u64>) -> u64 {
    let stamp = now.max(own);
    match live {
        Some(live) if stamp <= live => live + 1,
        _ => stamp,
    }
}

fn verify_manifest_packages(
    root: &Path,
    channel: &str,
    manifest: &Value,
    path: &Path,
) -> Result<()> {
    let modules = manifest
        .get("modules")
        .and_then(Value::as_object)
        .with_context(|| format!("{} lists no modules", path.display()))?;
    ensure!(
        modules.contains_key("fluxer_renderer"),
        "{} lists no fluxer_renderer",
        path.display()
    );
    for (module, entry) in modules {
        let sha256 = entry
            .get("sha256")
            .and_then(Value::as_str)
            .with_context(|| format!("{} declares no sha256 for {module}", path.display()))?;
        let bytes = entry
            .get("bytes")
            .and_then(Value::as_u64)
            .with_context(|| format!("{} declares no size for {module}", path.display()))?;
        let url = entry
            .get("url")
            .and_then(Value::as_str)
            .with_context(|| format!("{} declares no url for {module}", path.display()))?;
        let address = format!(
            "/desktop/{channel}/{MODULES_KEY_SEGMENT}/{module}/{sha256}/{MODULE_PACKAGE_NAME}"
        );
        ensure!(
            url == format!("{DEVELOPMENT_PACKAGE_ORIGIN}{address}"),
            "{} points {module} at {url}, which is not its content address",
            path.display()
        );
        let package = root.join(address.trim_start_matches('/'));
        let size = fs::metadata(&package)
            .with_context(|| {
                format!(
                    "{} is missing, {} references it",
                    package.display(),
                    path.display()
                )
            })?
            .len();
        ensure!(
            size == bytes,
            "{} is {size} bytes, {} declares {bytes}",
            package.display(),
            path.display()
        );
        let actual = sha256_file(&package)?;
        ensure!(
            actual == sha256,
            "{} hashes to {actual}, {} declares {sha256}",
            package.display(),
            path.display()
        );
    }
    Ok(())
}

fn publish_modules(
    payload_root: &Path,
    root: &Path,
    stamp: &PackedModulesStamp,
    target: &ModuleCoordinate,
) -> Result<u64> {
    let staged_modules = payload_root.join(&target.channel).join(MODULES_KEY_SEGMENT);
    for module_dir in fs::read_dir(&staged_modules)
        .with_context(|| format!("failed to read {}", staged_modules.display()))?
    {
        let module_dir = module_dir?.path();
        for digest_dir in fs::read_dir(&module_dir)
            .with_context(|| format!("failed to read {}", module_dir.display()))?
        {
            let digest_dir = digest_dir?.path();
            let relative = digest_dir.strip_prefix(payload_root)?;
            copy_package(
                &digest_dir.join(MODULE_PACKAGE_NAME),
                &root
                    .join("desktop")
                    .join(relative)
                    .join(MODULE_PACKAGE_NAME),
            )?;
        }
    }
    let coordinate_dir = root
        .join("desktop")
        .join(&target.channel)
        .join(&target.platform)
        .join(&target.arch);
    let staged_manifest_path = payload_root
        .join(&target.channel)
        .join(&target.platform)
        .join(&target.arch)
        .join(CHANNEL_MANIFEST_NAME);
    let staged_bytes = fs::read(&staged_manifest_path)
        .with_context(|| format!("failed to read {}", staged_manifest_path.display()))?;
    let version_manifest_path = coordinate_dir
        .join(&stamp.version)
        .join(CHANNEL_MANIFEST_NAME);
    write_atomically(&version_manifest_path, &staged_bytes)?;
    let manifest: Value = serde_json::from_slice(&staged_bytes)
        .with_context(|| format!("failed to parse {}", staged_manifest_path.display()))?;
    verify_manifest_packages(root, &target.channel, &manifest, &version_manifest_path)?;
    let own = metadata_version_of(&manifest, &version_manifest_path)?;
    let live_path = coordinate_dir.join(CHANNEL_MANIFEST_NAME);
    let live = match fs::read(&live_path) {
        Ok(bytes) => Some(
            serde_json::from_slice::<Value>(&bytes)
                .with_context(|| format!("failed to parse {}", live_path.display()))?,
        ),
        Err(error) if error.kind() == io::ErrorKind::NotFound => None,
        Err(error) => {
            return Err(error).with_context(|| format!("failed to read {}", live_path.display()));
        }
    };
    let live_version = live
        .as_ref()
        .map(|live| metadata_version_of(live, &live_path))
        .transpose()?;
    if let (Some(live), Some(live_version)) = (&live, live_version)
        && live_version >= own
        && manifest_without_metadata_version(live) == manifest_without_metadata_version(&manifest)
    {
        println!(
            "The local package server already serves modules {} at metadata_version {live_version}",
            stamp.version
        );
        return Ok(live_version);
    }
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|elapsed| elapsed.as_secs())
        .unwrap_or(0);
    let next = next_metadata_version(now, own, live_version);
    let mut restamped = manifest;
    restamped["metadata_version"] = Value::from(next);
    let mut bytes = serde_json::to_vec(&restamped)?;
    bytes.push(b'\n');
    write_atomically(&live_path, &bytes)?;
    println!(
        "Published modules {} to {} at metadata_version {next}",
        stamp.version,
        live_path.display()
    );
    Ok(next)
}

fn docker_output(args: &[&str]) -> Result<std::process::Output> {
    Command::new("docker")
        .args(args)
        .stdin(Stdio::null())
        .output()
        .with_context(|| format!("failed to run docker {}", args.join(" ")))
}

fn container_matches(root: &Path, config: &Path) -> Result<bool> {
    let output = docker_output(&[
        "inspect",
        "--format",
        "{{.State.Running}} {{range .Mounts}}{{.Source}}={{.Destination}} {{end}}",
        PKGS_CONTAINER,
    ])?;
    if !output.status.success() {
        return Ok(false);
    }
    let described = String::from_utf8_lossy(&output.stdout);
    let mut fields = described.split_whitespace();
    if fields.next() != Some("true") {
        return Ok(false);
    }
    let mounts: Vec<&str> = fields.collect();
    Ok(
        mounts.contains(&format!("{}={PKGS_CONTAINER_ROOT}", root.display()).as_str())
            && mounts.contains(&format!("{}={PKGS_CONTAINER_CONFIG}", config.display()).as_str()),
    )
}

fn port_holder() -> String {
    docker_output(&["ps", "--filter", "publish=48780", "--format", "{{.Names}}"])
        .ok()
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_owned())
        .filter(|names| !names.is_empty())
        .map_or_else(
            || "another process".to_owned(),
            |names| format!("the container {names}"),
        )
}

pub fn ensure_pkgs_server() -> Result<()> {
    let root = pkgs_root();
    fs::create_dir_all(&root).with_context(|| format!("failed to create {}", root.display()))?;
    let root = root
        .canonicalize()
        .with_context(|| format!("failed to resolve {}", root.display()))?;
    let config = pkgs_state_dir().join("nginx.conf");
    if fs::read_to_string(&config).ok().as_deref() != Some(PKGS_NGINX_CONFIG) {
        write_atomically(&config, PKGS_NGINX_CONFIG.as_bytes())?;
    }
    let config = config
        .canonicalize()
        .with_context(|| format!("failed to resolve {}", config.display()))?;
    if container_matches(&root, &config)? {
        println!(
            "The local package server {PKGS_CONTAINER} serves {} on {DEVELOPMENT_PACKAGE_ORIGIN}",
            root.display()
        );
        return Ok(());
    }
    let _ = docker_output(&["rm", "-f", PKGS_CONTAINER]);
    let root_mount = format!("{}:{PKGS_CONTAINER_ROOT}:ro", root.display());
    let config_mount = format!("{}:{PKGS_CONTAINER_CONFIG}:ro", config.display());
    let output = docker_output(&[
        "run",
        "--detach",
        "--name",
        PKGS_CONTAINER,
        "--restart",
        "unless-stopped",
        "--publish",
        PKGS_PUBLISH,
        "--volume",
        &root_mount,
        "--volume",
        &config_mount,
        PKGS_IMAGE,
    ])?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        if stderr.contains("address already in use") || stderr.contains("port is already allocated")
        {
            let _ = docker_output(&["rm", "-f", PKGS_CONTAINER]);
            bail!(
                "{DEVELOPMENT_PACKAGE_ORIGIN} is taken by {}, stop it so the development app can reach its module server",
                port_holder()
            );
        }
        bail!(
            "failed to start the local package server: {}",
            stderr.trim()
        );
    }
    println!(
        "Started the local package server {PKGS_CONTAINER} for {} on {DEVELOPMENT_PACKAGE_ORIGIN}",
        root.display()
    );
    Ok(())
}

fn verify_served_manifest(target: &ModuleCoordinate, metadata_version: u64) -> Result<()> {
    let url = format!(
        "{PKGS_PROBE_ORIGIN}/desktop/{}/{}/{}/{CHANNEL_MANIFEST_NAME}",
        target.channel, target.platform, target.arch
    );
    let mut last = String::new();
    for _ in 0..20 {
        let output = Command::new("curl")
            .args(["--silent", "--fail", "--max-time", "5", &url])
            .stdin(Stdio::null())
            .output()
            .context("failed to run curl")?;
        if output.status.success() {
            let served: Value = serde_json::from_slice(&output.stdout)
                .with_context(|| format!("{url} served invalid JSON"))?;
            if served.get("metadata_version").and_then(Value::as_u64) == Some(metadata_version) {
                return Ok(());
            }
            last = format!(
                "it serves metadata_version {}",
                served.get("metadata_version").unwrap_or(&Value::Null)
            );
        } else {
            last = String::from_utf8_lossy(&output.stderr).trim().to_owned();
        }
        std::thread::sleep(std::time::Duration::from_millis(250));
    }
    bail!("{url} does not serve metadata_version {metadata_version}: {last}")
}

pub fn publish_renderer_modules(
    renderer: &Path,
    fingerprint: &str,
    version: &str,
    target: &ModuleCoordinate,
) -> Result<()> {
    let stamp = pack_renderer_modules(renderer, fingerprint, version, &target.channel)?;
    let payload_root = stage_channel_manifest(&stamp, target)?;
    let root = pkgs_root();
    let metadata_version = publish_modules(&payload_root, &root, &stamp, target)?;
    remove_path(&publish_staging_dir())?;
    ensure_pkgs_server()?;
    verify_served_manifest(target, metadata_version)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn metadata_version_always_rises_above_the_live_feed() {
        assert_eq!(next_metadata_version(100, 50, None), 100);
        assert_eq!(next_metadata_version(100, 150, None), 150);
        assert_eq!(next_metadata_version(100, 50, Some(120)), 121);
        assert_eq!(next_metadata_version(100, 50, Some(99)), 100);
    }

    #[test]
    fn packed_module_stamps_round_trip() {
        let stamp = PackedModulesStamp {
            fingerprint: "abc".to_owned(),
            version: "2026.1005.171400".to_owned(),
            pub_date: "2026-10-05T17:14:00Z".to_owned(),
        };
        assert_eq!(PackedModulesStamp::parse(&stamp.render()), Some(stamp));
        assert_eq!(PackedModulesStamp::parse("abc 2026.1005.1"), None);
    }

    #[test]
    fn formats_pub_dates_as_rfc3339_utc() {
        let at = UNIX_EPOCH + std::time::Duration::from_secs(1_791_220_440);
        assert_eq!(rfc3339_utc(at), "2026-10-05T17:14:00Z");
    }

    #[test]
    fn publishes_one_coordinate_and_restamps_only_on_change() {
        let temp = tempfile::tempdir().unwrap();
        let payload = temp.path().join("payload");
        let root = temp.path().join("root");
        let package_dir = payload.join("development/modules/fluxer_renderer");
        let package_bytes = b"renderer";
        let digest = Sha256::digest(package_bytes)
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect::<String>();
        fs::create_dir_all(package_dir.join(&digest)).unwrap();
        fs::write(
            package_dir.join(&digest).join(MODULE_PACKAGE_NAME),
            package_bytes,
        )
        .unwrap();
        let manifest = serde_json::json!({
            "release_channel": "development",
            "metadata_version": 10,
            "modules": {
                "fluxer_renderer": {
                    "sha256": digest,
                    "bytes": package_bytes.len(),
                    "url": format!("{DEVELOPMENT_PACKAGE_ORIGIN}/desktop/development/modules/fluxer_renderer/{digest}/package.br"),
                }
            }
        });
        let manifest_dir = payload.join("development/darwin/arm64");
        fs::create_dir_all(&manifest_dir).unwrap();
        fs::write(
            manifest_dir.join(CHANNEL_MANIFEST_NAME),
            manifest.to_string(),
        )
        .unwrap();
        let stamp = PackedModulesStamp {
            fingerprint: "f".to_owned(),
            version: "2026.1005.1".to_owned(),
            pub_date: "2026-10-05T00:00:00Z".to_owned(),
        };
        let target = ModuleCoordinate {
            channel: "development".to_owned(),
            platform: "darwin".to_owned(),
            arch: "arm64".to_owned(),
        };
        let first = publish_modules(&payload, &root, &stamp, &target).unwrap();
        assert!(first > 10);
        assert!(
            root.join(format!(
                "desktop/development/modules/fluxer_renderer/{digest}/package.br"
            ))
            .is_file()
        );
        assert!(
            root.join("desktop/development/darwin/arm64/2026.1005.1/modules.json")
                .is_file()
        );
        assert_eq!(
            publish_modules(&payload, &root, &stamp, &target).unwrap(),
            first
        );
    }

    #[test]
    fn refuses_a_manifest_whose_package_is_not_at_its_content_address() {
        let temp = tempfile::tempdir().unwrap();
        let manifest = serde_json::json!({
            "modules": {
                "fluxer_renderer": {
                    "sha256": "00".repeat(32),
                    "bytes": 1,
                    "url": "https://pkgs.fluxer.com/desktop/development/modules/fluxer_renderer/x/package.br",
                }
            }
        });
        let error = verify_manifest_packages(temp.path(), "development", &manifest, Path::new("m"))
            .unwrap_err();
        assert!(error.to_string().contains("not its content address"));
    }
}
