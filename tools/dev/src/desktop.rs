// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::desktop_modules::{ModuleCoordinate, host_module_platform, publish_renderer_modules};
use crate::paths::{DESKTOP_DIR, DEV_STATE_DIR, ROOT};
use crate::proc::{PNPM_INSTALL_ENV, RunOptions, run_command};
use anyhow::{Context, Result, bail, ensure};
use sha2::{Digest, Sha256};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread::sleep;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

pub const DEVELOPMENT_APP_NAME: &str = "Fluxer Development";
pub const DEFAULT_INSTALL_DIR: &str = "/Applications";
const DEVELOPMENT_CHANNEL: &str = "development";
const DEVELOPMENT_BUNDLE_ID: &str = "app.fluxer.development";
const DEVELOPMENT_PROTOCOL_SCHEME: &str = "fluxer-development";
const PRODUCTION_PROTOCOL_SCHEME: &str = "fluxer";
const RENDERER_BUILD_VERSION: &str = "dev";
const RENDERER_SOURCE_PATHS: &[&str] = &["fluxer_app", "packages", "pnpm-lock.yaml"];
const RENDERER_FORBIDDEN_ENTRIES: &[&str] = &["sw.js", "sw.js.map"];
const DEVELOPER_ID_PREFIX: &str = "Developer ID Application: ";
const APP_QUIT_TIMEOUT: Duration = Duration::from_secs(15);
const LSREGISTER: &str = "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister";
const MACOS_DEV_ELECTRON_USAGE_DESCRIPTIONS: &[(&str, &str)] = &[
    (
        "NSMicrophoneUsageDescription",
        "Fluxer needs access to your microphone to enable voice chat features.",
    ),
    (
        "NSCameraUsageDescription",
        "Fluxer needs access to your camera to enable video chat features.",
    ),
    (
        "NSAppleEventsUsageDescription",
        "Fluxer needs access to Apple Events for automation features.",
    ),
    (
        "NSAudioCaptureUsageDescription",
        "Fluxer captures audio from the screen or window you choose to share.",
    ),
    (
        "NSScreenCaptureUsageDescription",
        "Fluxer captures the screen or window you choose to share.",
    ),
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacSigning {
    DeveloperId(String),
    AdHoc,
}

#[derive(Debug, Clone)]
pub struct DesktopAppOptions {
    pub rebuild_renderer: bool,
    pub ad_hoc: bool,
    pub install_dir: PathBuf,
    pub install: bool,
    pub launch: bool,
}

fn desktop_app_state_dir() -> PathBuf {
    DEV_STATE_DIR.join("desktop-app")
}

fn renderer_cache_dir() -> PathBuf {
    desktop_app_state_dir().join("renderer")
}

fn renderer_stamp_path() -> PathBuf {
    desktop_app_state_dir().join("renderer.stamp")
}

fn package_output_dir() -> PathBuf {
    desktop_app_state_dir().join("package")
}

fn installed_app_path(install_dir: &Path) -> PathBuf {
    install_dir.join(format!("{DEVELOPMENT_APP_NAME}.app"))
}

fn app_executable_path(app: &Path) -> PathBuf {
    app.join("Contents/MacOS").join(DEVELOPMENT_APP_NAME)
}

pub fn install_desktop_dependencies() -> Result<()> {
    run_command(
        &["pnpm", "install", "--frozen-lockfile"],
        RunOptions {
            cwd: ROOT.as_path(),
            env: PNPM_INSTALL_ENV
                .iter()
                .map(|(k, v)| ((*k).to_owned(), Some((*v).to_owned())))
                .collect(),
            ..RunOptions::default()
        },
    )
    .map(drop)
}

fn ensure_desktop_dependencies() -> Result<()> {
    if DESKTOP_DIR
        .join("node_modules/.bin/electron-builder")
        .exists()
    {
        return Ok(());
    }
    install_desktop_dependencies()
}

pub fn typecheck_desktop() -> Result<()> {
    run_command(
        &["pnpm", "typecheck"],
        RunOptions {
            cwd: DESKTOP_DIR.as_path(),
            ..RunOptions::default()
        },
    )
    .map(drop)
}

pub fn development_version(now: SystemTime) -> String {
    let seconds = now
        .duration_since(UNIX_EPOCH)
        .map(|elapsed| elapsed.as_secs())
        .unwrap_or(0);
    let days = (seconds / 86_400) as i64;
    let second_of_day = seconds % 86_400;
    let (year, month, day) = civil_from_days(days);
    let micro =
        (second_of_day / 3600) * 10_000 + ((second_of_day % 3600) / 60) * 100 + second_of_day % 60;
    format!("{year}.{month}{day:02}.{micro}")
}

pub(crate) fn civil_from_days(days: i64) -> (i64, u32, u32) {
    let shifted = days + 719_468;
    let era = shifted.div_euclid(146_097);
    let day_of_era = shifted.rem_euclid(146_097);
    let year_of_era =
        (day_of_era - day_of_era / 1460 + day_of_era / 36_524 - day_of_era / 146_096) / 365;
    let day_of_year = day_of_era - (365 * year_of_era + year_of_era / 4 - year_of_era / 100);
    let month_index = (5 * day_of_year + 2) / 153;
    let day = (day_of_year - (153 * month_index + 2) / 5 + 1) as u32;
    let month = if month_index < 10 {
        month_index + 3
    } else {
        month_index - 9
    } as u32;
    let year = year_of_era + era * 400 + i64::from(month <= 2);
    (year, month, day)
}

fn target_electron_arch() -> Result<String> {
    if let Ok(arch) = env::var("ELECTRON_ARCH")
        && !arch.trim().is_empty()
    {
        return Ok(arch.trim().to_owned());
    }
    match env::consts::ARCH {
        "x86_64" => Ok("x64".to_owned()),
        "aarch64" => Ok("arm64".to_owned()),
        other => bail!(
            "cannot build the desktop app on host architecture {other}. Set ELECTRON_ARCH to x64 or arm64"
        ),
    }
}

fn git_output(args: &[&str]) -> Result<Vec<u8>> {
    let output = Command::new("git")
        .args(args)
        .current_dir(ROOT.as_path())
        .env("GIT_OPTIONAL_LOCKS", "0")
        .stdin(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| format!("failed to run git {}", args.join(" ")))?;
    if !output.status.success() {
        bail!(
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr).trim_end()
        );
    }
    Ok(output.stdout)
}

fn renderer_source_fingerprint() -> Result<String> {
    let mut hasher = Sha256::new();
    hasher.update(format!("{DEVELOPMENT_CHANNEL}\n{RENDERER_BUILD_VERSION}\n"));
    let mut tree_args = vec!["rev-parse".to_owned()];
    tree_args.extend(
        RENDERER_SOURCE_PATHS
            .iter()
            .map(|path| format!("HEAD:{path}")),
    );
    hasher.update(git_output(
        &tree_args.iter().map(String::as_str).collect::<Vec<_>>(),
    )?);
    let mut diff_args = vec!["diff", "--no-ext-diff", "--binary", "HEAD", "--"];
    diff_args.extend(RENDERER_SOURCE_PATHS);
    hasher.update(git_output(&diff_args)?);
    let mut untracked_args = vec!["ls-files", "--others", "--exclude-standard", "-z", "--"];
    untracked_args.extend(RENDERER_SOURCE_PATHS);
    for path in git_output(&untracked_args)?
        .split(|byte| *byte == 0)
        .filter(|path| !path.is_empty())
    {
        hasher.update(path);
        if let Ok(metadata) = fs::metadata(ROOT.join(String::from_utf8_lossy(path).as_ref())) {
            hasher.update(metadata.len().to_le_bytes());
            if let Ok(modified) = metadata.modified()
                && let Ok(elapsed) = modified.duration_since(UNIX_EPOCH)
            {
                hasher.update(elapsed.as_nanos().to_le_bytes());
            }
        }
    }
    Ok(hasher
        .finalize()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect())
}

pub fn verify_renderer_tree(root: &Path) -> Result<()> {
    ensure!(
        root.join("index.html").is_file(),
        "renderer at {} has no index.html",
        root.display()
    );
    ensure!(
        root.join("assets").is_dir(),
        "renderer at {} has no assets directory",
        root.display()
    );
    for entry in RENDERER_FORBIDDEN_ENTRIES {
        ensure!(
            !root.join(entry).exists(),
            "the desktop renderer must not ship a service worker, found {} in {}",
            entry,
            root.display()
        );
    }
    Ok(())
}

pub(crate) fn copy_tree(
    source: &Path,
    destination: &Path,
    skip: &dyn Fn(&Path) -> bool,
) -> Result<()> {
    fs::create_dir_all(destination)
        .with_context(|| format!("failed to create {}", destination.display()))?;
    for entry in
        fs::read_dir(source).with_context(|| format!("failed to read {}", source.display()))?
    {
        let entry = entry?;
        let path = entry.path();
        if skip(&path) {
            continue;
        }
        let target = destination.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_tree(&path, &target, skip)?;
        } else {
            fs::copy(&path, &target).with_context(|| {
                format!("failed to copy {} to {}", path.display(), target.display())
            })?;
        }
    }
    Ok(())
}

pub(crate) fn remove_path(path: &Path) -> Result<()> {
    match fs::symlink_metadata(path) {
        Ok(metadata) if metadata.is_dir() => {
            fs::remove_dir_all(path).with_context(|| format!("failed to remove {}", path.display()))
        }
        Ok(_) => {
            fs::remove_file(path).with_context(|| format!("failed to remove {}", path.display()))
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error).with_context(|| format!("failed to inspect {}", path.display())),
    }
}

fn is_source_map(path: &Path) -> bool {
    path.extension().is_some_and(|extension| extension == "map")
}

fn ensure_renderer(rebuild: bool) -> Result<(PathBuf, String)> {
    let cache = renderer_cache_dir();
    let stamp_path = renderer_stamp_path();
    let fingerprint = renderer_source_fingerprint()?;
    let cached = fs::read_to_string(&stamp_path).unwrap_or_default();
    if !rebuild && cached.trim() == fingerprint && verify_renderer_tree(&cache).is_ok() {
        println!(
            "Reusing the renderer built from unchanged sources ({})",
            &fingerprint[..12]
        );
        return Ok((cache, fingerprint));
    }
    println!("Building the renderer for the development app...");
    run_command(
        &["pnpm", "--filter", "fluxer_app", "build:desktop"],
        RunOptions {
            cwd: ROOT.as_path(),
            env: vec![
                ("NODE_ENV".to_owned(), Some("production".to_owned())),
                (
                    "PUBLIC_BUILD_VERSION".to_owned(),
                    Some(RENDERER_BUILD_VERSION.to_owned()),
                ),
                (
                    "PUBLIC_RELEASE_CHANNEL".to_owned(),
                    Some(DEVELOPMENT_CHANNEL.to_owned()),
                ),
            ],
            load_default_env: false,
            ..RunOptions::default()
        },
    )?;
    let app_dist = ROOT.join("fluxer_app/dist");
    verify_renderer_tree(&app_dist)?;
    remove_path(&stamp_path)?;
    remove_path(&cache)?;
    copy_tree(&app_dist, &cache, &is_source_map)?;
    verify_renderer_tree(&cache)?;
    fs::write(&stamp_path, format!("{fingerprint}\n"))
        .with_context(|| format!("failed to write {}", stamp_path.display()))?;
    Ok((cache, fingerprint))
}

struct BuildChannelFileGuard {
    path: PathBuf,
    original: Option<String>,
}

impl BuildChannelFileGuard {
    fn capture() -> Self {
        let path = DESKTOP_DIR.join("src/common/BuildChannel.ts");
        let original = fs::read_to_string(&path).ok();
        Self { path, original }
    }
}

impl Drop for BuildChannelFileGuard {
    fn drop(&mut self) {
        if let Some(original) = &self.original
            && fs::read_to_string(&self.path).ok().as_deref() != Some(original.as_str())
        {
            let _ = fs::write(&self.path, original);
        }
    }
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct DesktopBuildInfo {
    build_version: String,
    build_channel: String,
    offline_build: bool,
    modules_enabled: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RendererDelivery {
    Offline,
    Modules,
}

fn verify_development_build_info(
    raw: &str,
    version: &str,
    delivery: RendererDelivery,
) -> Result<()> {
    let info: DesktopBuildInfo =
        serde_json::from_str(raw).context("failed to parse dist/build-info.json")?;
    ensure!(
        info.build_channel == DEVELOPMENT_CHANNEL,
        "the desktop build compiled the {} channel instead of {DEVELOPMENT_CHANNEL}",
        info.build_channel
    );
    match delivery {
        RendererDelivery::Offline => {
            ensure!(
                info.offline_build && !info.modules_enabled,
                "the unpackaged development shell must carry its renderer offline"
            );
        }
        RendererDelivery::Modules => {
            ensure!(
                info.modules_enabled && !info.offline_build,
                "the development app must load its renderer from the module feed like a release"
            );
        }
    }
    ensure!(
        info.build_version == version,
        "dist/build-info.json records version {} instead of {version}",
        info.build_version
    );
    Ok(())
}

pub fn build_desktop_shell() -> Result<()> {
    let env = vec![
        (
            "BUILD_CHANNEL".to_owned(),
            Some(env::var("BUILD_CHANNEL").unwrap_or_else(|_| "canary".to_owned())),
        ),
        (
            "PUBLIC_BUILD_VERSION".to_owned(),
            Some(env::var("PUBLIC_BUILD_VERSION").unwrap_or_else(|_| "dev".to_owned())),
        ),
        (
            "PUBLIC_RELEASE_CHANNEL".to_owned(),
            Some(env::var("PUBLIC_RELEASE_CHANNEL").unwrap_or_else(|_| "canary".to_owned())),
        ),
    ];
    run_command(
        &["pnpm", "build"],
        RunOptions {
            cwd: DESKTOP_DIR.as_path(),
            env,
            ..RunOptions::default()
        },
    )
    .map(drop)
}

pub fn build_desktop(
    rebuild_renderer: bool,
    skip_native: bool,
    delivery: RendererDelivery,
) -> Result<String> {
    ensure_desktop_dependencies()?;
    let (renderer, fingerprint) = ensure_renderer(rebuild_renderer)?;
    let version = development_version(SystemTime::now());
    let arch = target_electron_arch()?;
    if delivery == RendererDelivery::Modules {
        publish_renderer_modules(
            &renderer,
            &fingerprint,
            &version,
            &ModuleCoordinate {
                channel: DEVELOPMENT_CHANNEL.to_owned(),
                platform: host_module_platform()?.to_owned(),
                arch: arch.clone(),
            },
        )?;
    }
    let dist_renderer = DESKTOP_DIR.join("dist/renderer");
    remove_path(&dist_renderer)?;
    copy_tree(&renderer, &dist_renderer, &|_| false)?;
    {
        let _guard = BuildChannelFileGuard::capture();
        run_command(
            &[
                "pnpm",
                "exec",
                "node",
                "scripts/build.mjs",
                "--use-shared-renderer",
            ],
            RunOptions {
                cwd: DESKTOP_DIR.as_path(),
                env: vec![
                    (
                        "BUILD_CHANNEL".to_owned(),
                        Some(DEVELOPMENT_CHANNEL.to_owned()),
                    ),
                    (
                        "FLUXER_OFFLINE".to_owned(),
                        (delivery == RendererDelivery::Offline).then(|| "1".to_owned()),
                    ),
                    (
                        "FLUXER_MODULES".to_owned(),
                        (delivery == RendererDelivery::Modules).then(|| "1".to_owned()),
                    ),
                    (
                        "FLUXER_SKIP_NATIVE".to_owned(),
                        skip_native.then(|| "true".to_owned()),
                    ),
                    ("NODE_ENV".to_owned(), Some("production".to_owned())),
                    (
                        "FLUXER_DESKTOP_PRODUCTION".to_owned(),
                        Some("true".to_owned()),
                    ),
                    ("PUBLIC_BUILD_VERSION".to_owned(), Some(version.clone())),
                    ("BUILD_VERSION".to_owned(), Some(version.clone())),
                    (
                        "PUBLIC_RELEASE_CHANNEL".to_owned(),
                        Some(DEVELOPMENT_CHANNEL.to_owned()),
                    ),
                    (
                        "RELEASE_CHANNEL".to_owned(),
                        Some(DEVELOPMENT_CHANNEL.to_owned()),
                    ),
                    ("ELECTRON_ARCH".to_owned(), Some(arch)),
                    ("WORKDIR".to_owned(), Some(ROOT.display().to_string())),
                ],
                load_default_env: false,
                ..RunOptions::default()
            },
        )?;
    }
    let build_info_path = DESKTOP_DIR.join("dist/build-info.json");
    let build_info = fs::read_to_string(&build_info_path)
        .with_context(|| format!("failed to read {}", build_info_path.display()))?;
    verify_development_build_info(&build_info, &version, delivery)?;
    Ok(version)
}

pub fn parse_developer_id_identity(find_identity_output: &str) -> Option<String> {
    find_identity_output.lines().find_map(|line| {
        let start = line.find(&format!("\"{DEVELOPER_ID_PREFIX}"))? + 1;
        let rest = &line[start..];
        let name = &rest[..rest.find('"')?];
        Some(name.trim_start_matches(DEVELOPER_ID_PREFIX).to_owned())
    })
}

pub fn resolve_mac_signing(force_ad_hoc: bool) -> MacSigning {
    if force_ad_hoc {
        return MacSigning::AdHoc;
    }
    if let Ok(name) = env::var("CSC_NAME")
        && !name.trim().is_empty()
    {
        return MacSigning::DeveloperId(
            name.trim()
                .trim_start_matches(DEVELOPER_ID_PREFIX)
                .to_owned(),
        );
    }
    let output = Command::new("security")
        .args(["find-identity", "-v", "-p", "codesigning"])
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .output();
    if let Ok(output) = output
        && let Some(name) = parse_developer_id_identity(&String::from_utf8_lossy(&output.stdout))
    {
        return MacSigning::DeveloperId(name);
    }
    MacSigning::AdHoc
}

fn builder_target_args(arch: &str) -> Vec<String> {
    if cfg!(target_os = "macos") {
        vec!["--mac".to_owned(), format!("--{arch}")]
    } else {
        vec![format!("--{arch}")]
    }
}

pub fn electron_builder_args(
    output_dir: &Path,
    arch: &str,
    signing: &MacSigning,
    extra_args: &[String],
) -> Vec<String> {
    let mut args = vec![
        "pnpm".to_owned(),
        "exec".to_owned(),
        "electron-builder".to_owned(),
        "--config".to_owned(),
        "electron-builder.config.cjs".to_owned(),
        "--dir".to_owned(),
        format!("-c.directories.output={}", output_dir.display()),
        "-c.mac.notarize=false".to_owned(),
    ];
    if *signing == MacSigning::AdHoc {
        args.push("-c.mac.identity=-".to_owned());
    }
    if !extra_args
        .iter()
        .any(|arg| arg == "--mac" || arg == "--win" || arg == "--linux")
    {
        args.extend(builder_target_args(arch));
    }
    args.extend(extra_args.iter().cloned());
    args
}

pub fn package_desktop(
    rebuild_renderer: bool,
    ad_hoc: bool,
    extra_args: &[String],
) -> Result<PathBuf> {
    let version = build_desktop(rebuild_renderer, false, RendererDelivery::Modules)?;
    let arch = target_electron_arch()?;
    let signing = if cfg!(target_os = "macos") {
        resolve_mac_signing(ad_hoc)
    } else {
        MacSigning::AdHoc
    };
    if cfg!(target_os = "macos") {
        match &signing {
            MacSigning::DeveloperId(name) => {
                println!("Signing {DEVELOPMENT_APP_NAME} with Developer ID Application: {name}")
            }
            MacSigning::AdHoc => println!(
                "No Developer ID Application identity found in the keychain, signing {DEVELOPMENT_APP_NAME} ad hoc. macOS will ask for camera, microphone and screen access again after every rebuild. Set CSC_NAME to a Developer ID identity to keep the grants."
            ),
        }
    }
    let output_dir = package_output_dir();
    remove_path(&output_dir)?;
    let mut env = vec![
        (
            "BUILD_CHANNEL".to_owned(),
            Some(DEVELOPMENT_CHANNEL.to_owned()),
        ),
        ("VERSION".to_owned(), Some(version.clone())),
        ("ELECTRON_ARCH".to_owned(), Some(arch.clone())),
        ("NODE_ENV".to_owned(), Some("production".to_owned())),
    ];
    match &signing {
        MacSigning::DeveloperId(name) => env.push(("CSC_NAME".to_owned(), Some(name.clone()))),
        MacSigning::AdHoc => env.push((
            "CSC_IDENTITY_AUTO_DISCOVERY".to_owned(),
            Some("false".to_owned()),
        )),
    }
    let args = electron_builder_args(&output_dir, &arch, &signing, extra_args);
    let refs: Vec<_> = args.iter().map(String::as_str).collect();
    run_command(
        &refs,
        RunOptions {
            cwd: DESKTOP_DIR.as_path(),
            env,
            load_default_env: false,
            ..RunOptions::default()
        },
    )?;
    if !cfg!(target_os = "macos") {
        println!(
            "Packaged {DEVELOPMENT_APP_NAME} {version} into {}",
            output_dir.display()
        );
        return Ok(output_dir);
    }
    let app = find_packaged_app(&output_dir)?;
    verify_packaged_app(&app)?;
    println!(
        "Packaged {DEVELOPMENT_APP_NAME} {version} at {}",
        app.display()
    );
    Ok(app)
}

fn find_packaged_app(output_dir: &Path) -> Result<PathBuf> {
    let bundle_name = format!("{DEVELOPMENT_APP_NAME}.app");
    if output_dir.is_dir() {
        for entry in fs::read_dir(output_dir)? {
            let candidate = entry?.path().join(&bundle_name);
            if candidate.is_dir() {
                return Ok(candidate);
            }
        }
    }
    bail!(
        "no packaged {bundle_name} under {}. Run `pnpm dev:desktop:package` first",
        output_dir.display()
    )
}

fn plist_value(info_plist: &Path, key: &str, format: &str) -> Result<String> {
    let output = Command::new("plutil")
        .args(["-extract", key, format, "-o", "-"])
        .arg(info_plist)
        .stdin(Stdio::null())
        .output()
        .with_context(|| format!("failed to read {key} from {}", info_plist.display()))?;
    if !output.status.success() {
        bail!(
            "{} has no {key}: {}",
            info_plist.display(),
            String::from_utf8_lossy(&output.stderr).trim_end()
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_owned())
}

pub fn verify_url_schemes(url_types_json: &str) -> Result<()> {
    let url_types: Vec<serde_json::Value> =
        serde_json::from_str(url_types_json).context("CFBundleURLTypes is not a JSON array")?;
    let schemes: Vec<&str> = url_types
        .iter()
        .filter_map(|entry| entry.get("CFBundleURLSchemes")?.as_array())
        .flatten()
        .filter_map(serde_json::Value::as_str)
        .collect();
    ensure!(
        !schemes
            .iter()
            .any(|scheme| scheme.eq_ignore_ascii_case(PRODUCTION_PROTOCOL_SCHEME)),
        "{DEVELOPMENT_APP_NAME} must not register {PRODUCTION_PROTOCOL_SCHEME}://, it registers {schemes:?}"
    );
    ensure!(
        schemes.contains(&DEVELOPMENT_PROTOCOL_SCHEME),
        "{DEVELOPMENT_APP_NAME} must register {DEVELOPMENT_PROTOCOL_SCHEME}://, it registers {schemes:?}"
    );
    Ok(())
}

fn verify_packaged_app(app: &Path) -> Result<()> {
    let info_plist = app.join("Contents/Info.plist");
    let bundle_id = plist_value(&info_plist, "CFBundleIdentifier", "raw")?;
    ensure!(
        bundle_id == DEVELOPMENT_BUNDLE_ID,
        "{} has bundle id {bundle_id}, expected {DEVELOPMENT_BUNDLE_ID}",
        app.display()
    );
    verify_url_schemes(&plist_value(&info_plist, "CFBundleURLTypes", "json")?)?;
    ensure!(
        app_executable_path(app).is_file(),
        "{} has no {DEVELOPMENT_APP_NAME} executable",
        app.display()
    );
    let status = Command::new("codesign")
        .args(["--verify", "--deep", "--strict"])
        .arg(app)
        .status()
        .context("failed to run codesign --verify")?;
    ensure!(
        status.success(),
        "{} failed code signature verification",
        app.display()
    );
    Ok(())
}

pub fn parse_pids_for_executable(ps_output: &str, executable: &str) -> Vec<i32> {
    ps_output
        .lines()
        .filter_map(|line| {
            let line = line.trim_start();
            let (pid, command) = line.split_once(char::is_whitespace)?;
            (command.trim() == executable)
                .then(|| pid.parse().ok())
                .flatten()
        })
        .collect()
}

fn running_pids(executable: &Path) -> Result<Vec<i32>> {
    let output = Command::new("ps")
        .args(["-axww", "-o", "pid=,comm="])
        .stdin(Stdio::null())
        .output()
        .context("failed to list running processes")?;
    Ok(parse_pids_for_executable(
        &String::from_utf8_lossy(&output.stdout),
        &executable.to_string_lossy(),
    ))
}

#[cfg(unix)]
fn process_alive(pid: i32) -> bool {
    unsafe { libc::kill(pid, 0) == 0 }
}

#[cfg(unix)]
fn signal_process(pid: i32, signal: libc::c_int) {
    unsafe {
        libc::kill(pid, signal);
    }
}

#[cfg(unix)]
fn stop_running_app(app: &Path) -> Result<bool> {
    let executable = app_executable_path(app);
    let pids = running_pids(&executable)?;
    if pids.is_empty() {
        return Ok(false);
    }
    for pid in &pids {
        println!("Asking the running {DEVELOPMENT_APP_NAME} (pid {pid}) to quit...");
        signal_process(*pid, libc::SIGTERM);
    }
    let deadline = Instant::now() + APP_QUIT_TIMEOUT;
    while pids.iter().any(|pid| process_alive(*pid)) && Instant::now() < deadline {
        sleep(Duration::from_millis(200));
    }
    for pid in pids.iter().filter(|pid| process_alive(**pid)) {
        println!("{DEVELOPMENT_APP_NAME} (pid {pid}) did not quit in time, stopping it");
        signal_process(*pid, libc::SIGKILL);
    }
    Ok(true)
}

#[cfg(not(unix))]
fn stop_running_app(_app: &Path) -> Result<bool> {
    Ok(false)
}

#[cfg(target_os = "macos")]
fn swap_paths(first: &Path, second: &Path) -> Result<()> {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;
    let first_c = CString::new(first.as_os_str().as_bytes())?;
    let second_c = CString::new(second.as_os_str().as_bytes())?;
    let rc = unsafe { libc::renamex_np(first_c.as_ptr(), second_c.as_ptr(), libc::RENAME_SWAP) };
    if rc != 0 {
        return Err(std::io::Error::last_os_error()).with_context(|| {
            format!(
                "failed to swap {} with {}",
                first.display(),
                second.display()
            )
        });
    }
    Ok(())
}

#[cfg(not(target_os = "macos"))]
fn swap_paths(first: &Path, second: &Path) -> Result<()> {
    let parked = second.with_extension("app.previous");
    remove_path(&parked)?;
    fs::rename(second, &parked)?;
    fs::rename(first, second)?;
    fs::rename(&parked, first)?;
    Ok(())
}

fn ensure_host_macos(command: &str) -> Result<()> {
    if !cfg!(target_os = "macos") || Path::new("/.dockerenv").exists() {
        bail!(
            "`desktop {command}` installs a macOS app. Run it on the Mac host, not inside the devcontainer."
        );
    }
    Ok(())
}

pub fn install_desktop_app(
    source: Option<&Path>,
    install_dir: &Path,
    launch: bool,
) -> Result<PathBuf> {
    ensure_host_macos("install")?;
    let source = match source {
        Some(source) => source.to_path_buf(),
        None => find_packaged_app(&package_output_dir())?,
    };
    verify_packaged_app(&source)?;
    let destination = installed_app_path(install_dir);
    let staging = install_dir.join(format!(".{DEVELOPMENT_APP_NAME}.app.installing"));
    remove_path(&staging)?;
    let status = Command::new("ditto")
        .arg(&source)
        .arg(&staging)
        .status()
        .context("failed to run ditto")?;
    ensure!(
        status.success(),
        "failed to copy {} to {}",
        source.display(),
        staging.display()
    );
    stop_running_app(&destination)?;
    if destination.exists() {
        swap_paths(&staging, &destination)?;
        remove_path(&staging)?;
    } else {
        fs::rename(&staging, &destination)
            .with_context(|| format!("failed to move {} into place", staging.display()))?;
    }
    let _ = Command::new(LSREGISTER)
        .args(["-f"])
        .arg(&destination)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
    println!(
        "Installed {DEVELOPMENT_APP_NAME} at {}",
        destination.display()
    );
    if launch {
        let status = Command::new("open")
            .arg(&destination)
            .status()
            .context("failed to open the installed app")?;
        ensure!(status.success(), "failed to open {}", destination.display());
    }
    Ok(destination)
}

pub fn desktop_app(options: &DesktopAppOptions) -> Result<()> {
    if options.install {
        ensure_host_macos("app")?;
    }
    let app = package_desktop(options.rebuild_renderer, options.ad_hoc, &[])?;
    if options.install {
        install_desktop_app(Some(&app), &options.install_dir, options.launch)?;
    }
    Ok(())
}

pub fn electron_args(args: &[String]) -> Vec<String> {
    let mut runtime_args = Vec::new();
    if cfg!(target_os = "linux")
        && Path::new("/.dockerenv").exists()
        && env::var("FLUXER_ELECTRON_NO_SANDBOX").as_deref() != Ok("0")
    {
        runtime_args.push("--no-sandbox".to_owned());
    }
    if linux_wayland_session() && !args.iter().any(|arg| arg.starts_with("--ozone-platform")) {
        runtime_args.push("--ozone-platform=wayland".to_owned());
    }
    runtime_args.extend(args.iter().cloned());
    runtime_args
}

fn linux_wayland_session() -> bool {
    if !cfg!(target_os = "linux") {
        return false;
    }
    let Some(display) = env::var_os("WAYLAND_DISPLAY") else {
        return false;
    };
    if display.is_empty() {
        return false;
    }
    let display_path = PathBuf::from(&display);
    let socket = if display_path.is_absolute() {
        display_path
    } else {
        let Some(runtime_dir) = env::var_os("XDG_RUNTIME_DIR") else {
            return false;
        };
        Path::new(&runtime_dir).join(display_path)
    };
    socket.exists()
}

pub fn electron_command(args: &[String]) -> Vec<String> {
    let mut command = base_electron_command();
    command.extend(electron_args(args));
    command
}

fn base_electron_command() -> Vec<String> {
    if cfg!(target_os = "macos") && !Path::new("/.dockerenv").exists() {
        let electron_binary = dev_electron_binary_path();
        if electron_binary.is_file()
            && let Ok(launcher) = env::current_exe()
        {
            return disclaimed_electron_command(&launcher, &electron_binary);
        }
    }
    vec![
        "pnpm".to_owned(),
        "exec".to_owned(),
        "electron".to_owned(),
        ".".to_owned(),
    ]
}

fn disclaimed_electron_command(launcher: &Path, electron_binary: &Path) -> Vec<String> {
    vec![
        launcher.to_string_lossy().into_owned(),
        "desktop".to_owned(),
        "exec-disclaimed".to_owned(),
        electron_binary.to_string_lossy().into_owned(),
        ".".to_owned(),
    ]
}

pub fn run_desktop(
    extra_args: &[String],
    build: bool,
    rebuild_renderer: bool,
    skip_native: bool,
) -> Result<()> {
    if build {
        build_desktop(rebuild_renderer, skip_native, RendererDelivery::Offline)?;
    } else {
        ensure_desktop_dependencies()?;
    }
    patch_macos_dev_electron_info_plist()?;
    let mut args = vec!["--fluxer-log-renderer-console".to_owned()];
    args.extend(extra_args.iter().cloned());
    let command = electron_command(&args);
    let refs: Vec<_> = command.iter().map(String::as_str).collect();
    println!(
        "Starting the unpackaged {DEVELOPMENT_APP_NAME} shell. It shares the installed app's profile, so only one of them runs at a time."
    );
    run_command(
        &refs,
        RunOptions {
            cwd: DESKTOP_DIR.as_path(),
            load_default_env: false,
            ..RunOptions::default()
        },
    )
    .map(drop)
}

fn patch_macos_dev_electron_info_plist() -> Result<()> {
    if !cfg!(target_os = "macos") || Path::new("/.dockerenv").exists() {
        return Ok(());
    }
    let app_bundle = dev_electron_app_bundle_path();
    let info_plist = dev_electron_info_plist_path();
    if !info_plist.is_file() {
        bail!(
            "missing dev Electron Info.plist at {}. Run `pnpm dev:desktop:deps` first",
            info_plist.display()
        );
    }

    let mut changed = false;
    for (key, value) in MACOS_DEV_ELECTRON_USAGE_DESCRIPTIONS {
        changed |= set_or_add_plist_string(&info_plist, key, value)?;
    }

    if changed {
        println!(
            "Patched dev Electron Info.plist for macOS capture permissions: {}",
            info_plist.display()
        );
    }
    if changed || !codesign_verify(&app_bundle) {
        codesign_ad_hoc(&app_bundle)?;
    }
    Ok(())
}

fn dev_electron_app_bundle_path() -> PathBuf {
    DESKTOP_DIR.join("node_modules/electron/dist/Electron.app")
}

fn dev_electron_info_plist_path() -> PathBuf {
    dev_electron_app_bundle_path().join("Contents/Info.plist")
}

fn dev_electron_binary_path() -> PathBuf {
    dev_electron_app_bundle_path().join("Contents/MacOS/Electron")
}

fn set_or_add_plist_string(info_plist: &Path, key: &str, value: &str) -> Result<bool> {
    if read_plist_string(info_plist, key)?.as_deref() == Some(value) {
        return Ok(false);
    }
    if plist_key_exists(info_plist, key)? {
        run_plist_buddy(info_plist, &format!("Set :{key} {value}"))?;
    } else {
        run_plist_buddy(info_plist, &format!("Add :{key} string {value}"))?;
    }
    Ok(true)
}

fn plist_key_exists(info_plist: &Path, key: &str) -> Result<bool> {
    Ok(read_plist_string(info_plist, key)?.is_some())
}

fn read_plist_string(info_plist: &Path, key: &str) -> Result<Option<String>> {
    let output = Command::new("/usr/libexec/PlistBuddy")
        .args(["-c", &format!("Print :{key}")])
        .arg(info_plist)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| format!("failed to read {key} from {}", info_plist.display()))?;
    if !output.status.success() {
        return Ok(None);
    }
    Ok(Some(
        String::from_utf8_lossy(&output.stdout)
            .trim_end()
            .to_owned(),
    ))
}

fn run_plist_buddy(info_plist: &Path, command: &str) -> Result<()> {
    let output = Command::new("/usr/libexec/PlistBuddy")
        .args(["-c", command])
        .arg(info_plist)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| format!("failed to update {}", info_plist.display()))?;
    if output.status.success() {
        return Ok(());
    }
    bail!(
        "failed to update {}: {}",
        info_plist.display(),
        String::from_utf8_lossy(&output.stderr).trim_end()
    )
}

fn codesign_verify(app_bundle: &Path) -> bool {
    Command::new("codesign")
        .args(["--verify", "--deep", "--strict"])
        .arg(app_bundle)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|status| status.success())
        .unwrap_or(false)
}

fn codesign_ad_hoc(app_bundle: &Path) -> Result<()> {
    println!("Re-signing dev Electron.app after macOS permission plist patch...");
    let output = Command::new("codesign")
        .args(["--force", "--deep", "--sign", "-"])
        .arg(app_bundle)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .with_context(|| format!("failed to re-sign {}", app_bundle.display()))?;
    if output.status.success() {
        return Ok(());
    }
    bail!(
        "failed to re-sign {}: {}",
        app_bundle.display(),
        String::from_utf8_lossy(&output.stderr).trim_end()
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strips_no_sandbox_only_by_container_policy() {
        let args = electron_args(&["--flag".to_owned()]);
        if cfg!(target_os = "linux")
            && Path::new("/.dockerenv").exists()
            && env::var("FLUXER_ELECTRON_NO_SANDBOX").as_deref() != Ok("0")
        {
            assert_eq!(args[0], "--no-sandbox");
        }
        assert!(args.contains(&"--flag".to_owned()));
    }

    #[test]
    fn disclaimed_electron_command_re_execs_through_the_launcher() {
        let command = disclaimed_electron_command(
            Path::new("/tmp/fluxer-dev"),
            Path::new("/tmp/Electron.app/Contents/MacOS/Electron"),
        );
        assert_eq!(
            command,
            vec![
                "/tmp/fluxer-dev",
                "desktop",
                "exec-disclaimed",
                "/tmp/Electron.app/Contents/MacOS/Electron",
                "."
            ]
        );
    }

    #[test]
    fn dev_electron_paths_point_inside_the_dev_bundle() {
        assert!(dev_electron_binary_path().ends_with(
            "fluxer_desktop/node_modules/electron/dist/Electron.app/Contents/MacOS/Electron"
        ));
        assert!(dev_electron_info_plist_path().ends_with(
            "fluxer_desktop/node_modules/electron/dist/Electron.app/Contents/Info.plist"
        ));
    }

    #[test]
    fn dev_electron_usage_descriptions_include_audio_capture() {
        assert!(
            MACOS_DEV_ELECTRON_USAGE_DESCRIPTIONS
                .iter()
                .any(|(key, _)| *key == "NSAudioCaptureUsageDescription")
        );
    }

    #[test]
    fn development_version_is_a_calver_electron_builder_accepts() {
        let at = |seconds: u64| development_version(UNIX_EPOCH + Duration::from_secs(seconds));
        assert_eq!(at(1_791_039_600), "2026.1003.150000");
        assert_eq!(at(1_767_225_605), "2026.101.5");
        assert_eq!(at(1_798_761_599), "2026.1231.235959");
        assert_eq!(at(1_709_208_000), "2024.229.120000");
    }

    #[test]
    fn finds_the_developer_id_identity_and_skips_other_certificates() {
        let output = r#"  1) D9E17CA1546F517C814DFA8719A4FED295D7B019 "Apple Development: Someone (TT9JK7QNP2)"
  2) AA2A51260E57959F00CF6B117CEA2757748756FA "Developer ID Application: Fluxer Platform AB (3G5837T29K)"
     2 valid identities found"#;
        assert_eq!(
            parse_developer_id_identity(output).as_deref(),
            Some("Fluxer Platform AB (3G5837T29K)")
        );
        assert_eq!(
            parse_developer_id_identity("     0 valid identities found"),
            None
        );
    }

    #[test]
    fn electron_builder_packages_a_development_dir_for_the_host_without_notarizing() {
        let args = electron_builder_args(
            Path::new("/tmp/out"),
            "arm64",
            &MacSigning::DeveloperId("Fluxer Platform AB (3G5837T29K)".to_owned()),
            &[],
        );
        assert!(args.contains(&"--dir".to_owned()));
        assert!(args.contains(&"-c.mac.notarize=false".to_owned()));
        assert!(args.contains(&"-c.directories.output=/tmp/out".to_owned()));
        assert!(args.contains(&"--arm64".to_owned()));
        assert!(!args.iter().any(|arg| arg.starts_with("-c.mac.identity")));
        let ad_hoc = electron_builder_args(Path::new("/tmp/out"), "x64", &MacSigning::AdHoc, &[]);
        assert!(ad_hoc.contains(&"-c.mac.identity=-".to_owned()));
        assert!(ad_hoc.contains(&"--x64".to_owned()));
        let linux = electron_builder_args(
            Path::new("/tmp/out"),
            "x64",
            &MacSigning::AdHoc,
            &["--linux".to_owned()],
        );
        assert_eq!(linux.iter().filter(|arg| *arg == "--linux").count(), 1);
        assert!(!linux.contains(&"--mac".to_owned()));
    }

    #[test]
    fn rejects_a_development_app_that_claims_the_production_scheme() {
        assert!(
            verify_url_schemes(
                r#"[{"CFBundleURLName":"app.fluxer.development","CFBundleURLSchemes":["fluxer-development"]}]"#
            )
            .is_ok()
        );
        assert!(
            verify_url_schemes(r#"[{"CFBundleURLSchemes":["fluxer-development","Fluxer"]}]"#)
                .is_err()
        );
        assert!(verify_url_schemes(r#"[{"CFBundleURLSchemes":["fluxer"]}]"#).is_err());
        assert!(verify_url_schemes("[]").is_err());
    }

    #[test]
    fn build_info_must_match_the_renderer_delivery() {
        let info = |channel: &str, offline: bool, modules: bool| {
            format!(
                r#"{{"buildVersion":"2026.1003.150000","releaseChannel":"development","buildChannel":"{channel}","offlineBuild":{offline},"modulesEnabled":{modules}}}"#
            )
        };
        let check = |raw: String, version: &str, delivery| {
            verify_development_build_info(&raw, version, delivery).is_ok()
        };
        let version = "2026.1003.150000";
        assert!(check(
            info("development", true, false),
            version,
            RendererDelivery::Offline
        ));
        assert!(check(
            info("development", false, true),
            version,
            RendererDelivery::Modules
        ));
        assert!(!check(
            info("canary", false, true),
            version,
            RendererDelivery::Modules
        ));
        assert!(!check(
            info("development", true, false),
            version,
            RendererDelivery::Modules
        ));
        assert!(!check(
            info("development", false, true),
            version,
            RendererDelivery::Offline
        ));
        assert!(!check(
            info("development", true, true),
            version,
            RendererDelivery::Modules
        ));
        assert!(!check(
            info("development", false, true),
            "2026.1003.150001",
            RendererDelivery::Modules
        ));
    }

    #[test]
    fn matches_only_the_exact_main_executable_of_the_installed_app() {
        let ps = "  501 /Applications/Fluxer Development.app/Contents/MacOS/Fluxer Development\n\
                  502 /Applications/Fluxer Development.app/Contents/Frameworks/Fluxer Development Helper.app/Contents/MacOS/Fluxer Development Helper\n\
                  503 /Applications/Fluxer Canary.app/Contents/MacOS/Fluxer Canary\n\
                  504 /tmp/copy/Fluxer Development.app/Contents/MacOS/Fluxer Development\n";
        assert_eq!(
            parse_pids_for_executable(
                ps,
                "/Applications/Fluxer Development.app/Contents/MacOS/Fluxer Development"
            ),
            vec![501]
        );
        assert!(
            parse_pids_for_executable(ps, "/Applications/Fluxer.app/Contents/MacOS/Fluxer")
                .is_empty()
        );
    }

    #[test]
    fn installs_under_the_development_bundle_name_only() {
        let app = installed_app_path(Path::new(DEFAULT_INSTALL_DIR));
        assert_eq!(app, Path::new("/Applications/Fluxer Development.app"));
        assert_eq!(
            app_executable_path(&app),
            Path::new("/Applications/Fluxer Development.app/Contents/MacOS/Fluxer Development")
        );
    }

    #[test]
    fn renderer_tree_must_be_complete_and_free_of_service_workers() {
        let dir = tempfile::tempdir().unwrap();
        assert!(verify_renderer_tree(dir.path()).is_err());
        fs::write(dir.path().join("index.html"), "<!doctype html>").unwrap();
        fs::create_dir(dir.path().join("assets")).unwrap();
        assert!(verify_renderer_tree(dir.path()).is_ok());
        fs::write(dir.path().join("sw.js"), "").unwrap();
        assert!(verify_renderer_tree(dir.path()).is_err());
    }
}
