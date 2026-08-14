// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{
    CalverEnv, CommandSpec, append_github_env, append_github_output, append_github_path, capture,
    collect_files, command_succeeds, copy_dir_contents, count_files, count_files_min_depth,
    download_file, download_s3_prefix, env_bool, env_string, join_s3_key, output_bytes,
    output_text, parse_bool, path_to_s3_key, remove_dir_if_exists, remove_file_if_exists,
    require_any_env, require_env, require_home, resolve_calver, run_command, runner_temp,
    s3_client, title_case, trim_option, upload_directory_to_s3, upload_directory_to_s3_overwrite,
};
use crate::functions::write_json_pretty;
use anyhow::{Context, Result, anyhow, bail, ensure};
use aws_sdk_s3::Client as S3Client;
use chrono::Utc;
use clap::{Args, ValueEnum};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::{self, File, OpenOptions};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant};
use tempfile::TempDir;
use walkdir::WalkDir;
use zip::write::SimpleFileOptions;

const PUBLIC_DL_BASE: &str = "https://api.fluxer.app/dl";
const PNPM_VERSION: &str = "10.29.3";
const RUST_TOOLCHAIN: &str = "1.93.0";
const DEFAULT_DESKTOP_VARIANT: &str = "default";
const WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT: &str = "windows-game-capture";

#[derive(Debug, Args, Clone)]
pub struct BuildDesktopArgs {
    #[arg(long, value_enum)]
    step: DesktopStep,
    #[arg(long)]
    channel: Option<String>,
    #[arg(long)]
    test_build: Option<String>,
    #[arg(long)]
    skip_targets: Option<String>,
    #[arg(long)]
    skip_windows: Option<String>,
    #[arg(long)]
    skip_windows_x64: Option<String>,
    #[arg(long)]
    skip_windows_arm64: Option<String>,
    #[arg(long)]
    skip_macos: Option<String>,
    #[arg(long)]
    skip_macos_x64: Option<String>,
    #[arg(long)]
    skip_macos_arm64: Option<String>,
    #[arg(long)]
    skip_linux: Option<String>,
    #[arg(long)]
    skip_linux_x64: Option<String>,
    #[arg(long)]
    skip_linux_arm64: Option<String>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
#[clap(rename_all = "snake_case")]
enum DesktopStep {
    SetMetadata,
    SetMatrix,
    WindowsPaths,
    SetWorkdirUnix,
    EnsurePython3Windows,
    SetupPnpmCorepack,
    ResolvePnpmStoreWindows,
    ResolvePnpmStoreUnix,
    InstallSetuptoolsWindowsArm64,
    InstallSetuptoolsMacos,
    InstallLinuxDeps,
    InstallMsvcArm64Tools,
    InstallRustWindowsTargets,
    InstallDependencies,
    UpdateVersion,
    SetBuildChannel,
    BuildElectronMain,
    InstallVelopackCli,
    BuildAppMacos,
    VerifyBundleId,
    BuildAppWindows,
    ValidateWindowsSigningInputs,
    WriteWindowsSigningMetadata,
    ResolveWindowsUnpackedDir,
    VerifyWindowsUnpackedSignatures,
    PackageAppWindowsVelopack,
    AnalyseVelopackPaths,
    BuildAppLinux,
    CreatePortableZipWindows,
    VerifyWindowsSignedArtifacts,
    PrepareArtifactsWindows,
    PrepareArtifactsUnix,
    NormaliseUpdaterYaml,
    GenerateChecksumsUnix,
    GenerateChecksumsWindows,
    UploadHandoff,
    DownloadHandoff,
    CleanupHandoff,
    BuildPayload,
    UploadPayload,
    BuildSummary,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Platform {
    platform: &'static str,
    arch: &'static str,
    desktop_variant: &'static str,
    os: &'static str,
    electron_arch: &'static str,
}

const PLATFORMS: &[Platform] = &[
    Platform {
        platform: "windows",
        arch: "x64",
        desktop_variant: DEFAULT_DESKTOP_VARIANT,
        os: "blacksmith-32vcpu-windows-2025",
        electron_arch: "x64",
    },
    Platform {
        platform: "windows",
        arch: "arm64",
        desktop_variant: DEFAULT_DESKTOP_VARIANT,
        os: "blacksmith-32vcpu-windows-2025",
        electron_arch: "arm64",
    },
    Platform {
        platform: "macos",
        arch: "x64",
        desktop_variant: DEFAULT_DESKTOP_VARIANT,
        os: "fluxer-desktop-macos-arm64",
        electron_arch: "x64",
    },
    Platform {
        platform: "macos",
        arch: "arm64",
        desktop_variant: DEFAULT_DESKTOP_VARIANT,
        os: "fluxer-desktop-macos-arm64",
        electron_arch: "arm64",
    },
    Platform {
        platform: "linux",
        arch: "x64",
        desktop_variant: DEFAULT_DESKTOP_VARIANT,
        os: "blacksmith-32vcpu-ubuntu-2404",
        electron_arch: "x64",
    },
    Platform {
        platform: "linux",
        arch: "arm64",
        desktop_variant: DEFAULT_DESKTOP_VARIANT,
        os: "blacksmith-32vcpu-ubuntu-2404-arm",
        electron_arch: "arm64",
    },
];

pub async fn run(args: BuildDesktopArgs) -> Result<()> {
    match args.step {
        DesktopStep::SetMetadata => {
            let channel = args
                .channel
                .clone()
                .filter(|value| !value.is_empty())
                .or_else(|| env_string("CHANNEL"))
                .unwrap_or_else(|| "stable".to_string());
            let test_build = args
                .test_build
                .as_deref()
                .map(parse_bool)
                .unwrap_or_else(|| env_bool("TEST_BUILD"));
            set_metadata_step(&channel, test_build)
        }
        DesktopStep::SetMatrix => set_matrix_step(&args),
        DesktopStep::WindowsPaths => windows_paths_step().await,
        DesktopStep::SetWorkdirUnix => set_workdir_unix_step(),
        DesktopStep::EnsurePython3Windows => ensure_python3_windows_step(),
        DesktopStep::SetupPnpmCorepack => setup_pnpm_corepack_step(),
        DesktopStep::ResolvePnpmStoreWindows | DesktopStep::ResolvePnpmStoreUnix => {
            resolve_pnpm_store_step()
        }
        DesktopStep::InstallSetuptoolsWindowsArm64 => install_setuptools_windows_arm64_step(),
        DesktopStep::InstallSetuptoolsMacos => install_setuptools_macos_step(),
        DesktopStep::InstallLinuxDeps => install_linux_deps_step(),
        DesktopStep::InstallMsvcArm64Tools => install_msvc_arm64_tools_step(),
        DesktopStep::InstallRustWindowsTargets => install_rust_windows_targets_step(),
        DesktopStep::InstallDependencies => {
            run_command(pnpm_command()?.args(["install", "--frozen-lockfile"]))
        }
        DesktopStep::UpdateVersion => run_command(pnpm_command()?.args([
            "version",
            &require_env("VERSION")?,
            "--no-git-tag-version",
            "--allow-same-version",
        ])),
        DesktopStep::SetBuildChannel => set_build_channel_step(),
        DesktopStep::BuildElectronMain => build_electron_main_step(),
        DesktopStep::InstallVelopackCli => install_velopack_cli_step(),
        DesktopStep::BuildAppMacos => build_app_step(DesktopBuildPlatform::Macos),
        DesktopStep::VerifyBundleId => verify_bundle_id_step(),
        DesktopStep::BuildAppWindows => build_app_step(DesktopBuildPlatform::Windows),
        DesktopStep::ValidateWindowsSigningInputs => validate_windows_signing_inputs_step(),
        DesktopStep::WriteWindowsSigningMetadata => write_windows_signing_metadata_step(),
        DesktopStep::ResolveWindowsUnpackedDir => resolve_windows_unpacked_dir_step(),
        DesktopStep::VerifyWindowsUnpackedSignatures => verify_windows_unpacked_signatures_step(),
        DesktopStep::PackageAppWindowsVelopack => package_app_windows_velopack_step(),
        DesktopStep::AnalyseVelopackPaths => analyse_velopack_paths_step(),
        DesktopStep::BuildAppLinux => build_app_step(DesktopBuildPlatform::Linux),
        DesktopStep::CreatePortableZipWindows => create_portable_zip_windows_step(),
        DesktopStep::VerifyWindowsSignedArtifacts => verify_windows_signed_artifacts_step(),
        DesktopStep::PrepareArtifactsWindows => prepare_artifacts_windows_step(),
        DesktopStep::PrepareArtifactsUnix => prepare_artifacts_unix_step(),
        DesktopStep::NormaliseUpdaterYaml => normalise_updater_yaml_step(),
        DesktopStep::GenerateChecksumsUnix => generate_checksums_step(&[
            ArtifactChecksumKind::Extension("exe"),
            ArtifactChecksumKind::Extension("dmg"),
            ArtifactChecksumKind::Extension("zip"),
            ArtifactChecksumKind::Extension("AppImage"),
            ArtifactChecksumKind::Extension("deb"),
            ArtifactChecksumKind::Extension("rpm"),
            ArtifactChecksumKind::Suffix(".tar.gz"),
        ]),
        DesktopStep::GenerateChecksumsWindows => generate_checksums_step(&[
            ArtifactChecksumKind::Extension("exe"),
            ArtifactChecksumKind::Extension("nupkg"),
            ArtifactChecksumKind::Extension("zip"),
        ]),
        DesktopStep::UploadHandoff => upload_handoff_step(false).await,
        DesktopStep::DownloadHandoff => download_handoff_step().await,
        DesktopStep::CleanupHandoff => cleanup_handoff_step().await,
        DesktopStep::BuildPayload => build_payload_step(),
        DesktopStep::UploadPayload => upload_payload_step().await,
        DesktopStep::BuildSummary => build_summary_step(),
    }
}

fn calver_env_from_process() -> CalverEnv {
    CalverEnv {
        build_version: trim_option(env::var("BUILD_VERSION").ok()),
        fluxer_build_version: trim_option(env::var("FLUXER_BUILD_VERSION").ok()),
        fluxer_build_date: trim_option(env::var("FLUXER_BUILD_DATE").ok()),
    }
}

fn set_metadata_step(channel: &str, test_build: bool) -> Result<()> {
    let version = resolve_calver(&calver_env_from_process(), Utc::now())?;
    let pub_date = Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string();
    let build_channel = if channel == "canary" {
        "canary"
    } else {
        "stable"
    };
    let s3_prefix = if test_build {
        "desktop-test"
    } else {
        "desktop"
    };
    let source_sha = resolve_source_sha()?;

    append_github_output(&[
        ("version", version.as_str()),
        ("pub_date", pub_date.as_str()),
        ("channel", channel),
        ("build_channel", build_channel),
        ("test_build", if test_build { "true" } else { "false" }),
        ("s3_prefix", s3_prefix),
        ("source_sha", source_sha.as_str()),
    ])
}

fn set_build_channel_step() -> Result<()> {
    let channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    write_build_channel_file(&resolve_desktop_dir()?, &channel)
}

fn resolve_desktop_dir() -> Result<PathBuf> {
    let cwd = env::current_dir().context("Failed to resolve current directory")?;
    if cwd.file_name().and_then(|value| value.to_str()) == Some("fluxer_desktop") {
        return Ok(cwd);
    }
    if cwd.join("fluxer_desktop").is_dir() {
        return Ok(cwd.join("fluxer_desktop"));
    }
    Err(anyhow!(
        "Could not resolve fluxer_desktop directory from {}",
        cwd.display()
    ))
}

pub(crate) fn write_build_channel_file(root: &Path, channel: &str) -> Result<()> {
    ensure!(
        matches!(channel, "stable" | "canary"),
        "Invalid BUILD_CHANNEL: {channel}. Must be 'stable' or 'canary'."
    );
    let path = root.join("src/common/BuildChannel.ts");
    let content = build_channel_content(channel);
    if path
        .exists()
        .then(|| fs::read_to_string(&path))
        .transpose()
        .with_context(|| format!("Failed to read {}", path.display()))?
        .as_deref()
        == Some(content.as_str())
    {
        println!("Build channel already set to: {channel}");
        return Ok(());
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    fs::write(&path, content).with_context(|| format!("Failed to write {}", path.display()))?;
    println!("Set build channel to: {channel}");
    Ok(())
}

fn build_channel_content(channel: &str) -> String {
    format!(
        "// SPDX-License-Identifier: AGPL-3.0-or-later\n\n\
export type BuildChannel = 'stable' | 'canary';\n\n\
export const BUILD_CHANNEL = '{channel}' as BuildChannel;\n\
export const IS_CANARY = BUILD_CHANNEL === 'canary';\n\
export const CHANNEL_DISPLAY_NAME = BUILD_CHANNEL;\n"
    )
}

fn resolve_source_sha() -> Result<String> {
    let workspace = env::var("GITHUB_WORKSPACE").unwrap_or_else(|_| ".".to_string());
    let workspace = PathBuf::from(workspace);
    if workspace.join(".git").exists() {
        output_text(CommandSpec::new("git").args([
            "-C",
            workspace.to_string_lossy().as_ref(),
            "rev-parse",
            "HEAD",
        ]))
    } else {
        output_text(CommandSpec::new("git").args(["rev-parse", "HEAD"]))
    }
}

fn set_matrix_step(args: &BuildDesktopArgs) -> Result<()> {
    let platforms = selected_platforms(args)?;
    let include = platforms
        .iter()
        .copied()
        .map(platform_json)
        .collect::<Vec<_>>()
        .join(",");
    let matrix = format!("{{\"include\":[{include}]}}");
    append_github_output(&[("matrix", matrix.as_str())])
}

fn selected_platforms(args: &BuildDesktopArgs) -> Result<Vec<Platform>> {
    let skip_targets = skip_target_set(args)?;
    Ok(PLATFORMS
        .iter()
        .copied()
        .filter(|platform| !skip_platform(*platform, args, &skip_targets))
        .collect())
}

fn skip_target_set(args: &BuildDesktopArgs) -> Result<BTreeSet<String>> {
    let raw = args
        .skip_targets
        .clone()
        .filter(|value| !value.is_empty())
        .or_else(|| env_string("SKIP_TARGETS"))
        .unwrap_or_default();
    let valid = BTreeSet::from([
        "windows",
        "windows-x64",
        "windows-arm64",
        "macos",
        "macos-x64",
        "macos-arm64",
        "linux",
        "linux-x64",
        "linux-arm64",
    ]);
    let mut targets = BTreeSet::new();
    for token in raw.split(|character: char| character == ',' || character.is_whitespace()) {
        let target = token.trim().to_ascii_lowercase().replace('_', "-");
        if target.is_empty() {
            continue;
        }
        ensure!(
            valid.contains(target.as_str()),
            "Unknown desktop skip target: {target}. Expected one of: {}",
            valid.iter().copied().collect::<Vec<_>>().join(", ")
        );
        targets.insert(target);
    }
    Ok(targets)
}

fn skip_platform(
    platform: Platform,
    args: &BuildDesktopArgs,
    skip_targets: &BTreeSet<String>,
) -> bool {
    let flag = |arg: &Option<String>, env_name: &str| {
        arg.as_deref()
            .map(parse_bool)
            .unwrap_or_else(|| env_bool(env_name))
    };
    let platform_arch = format!("{}-{}", platform.platform, platform.arch);
    if skip_targets.contains(platform.platform) || skip_targets.contains(platform_arch.as_str()) {
        return true;
    }
    match platform.platform {
        "windows" => {
            flag(&args.skip_windows, "SKIP_WINDOWS")
                || (platform.arch == "x64" && flag(&args.skip_windows_x64, "SKIP_WINDOWS_X64"))
                || (platform.arch == "arm64"
                    && flag(&args.skip_windows_arm64, "SKIP_WINDOWS_ARM64"))
        }
        "macos" => {
            flag(&args.skip_macos, "SKIP_MACOS")
                || (platform.arch == "x64" && flag(&args.skip_macos_x64, "SKIP_MACOS_X64"))
                || (platform.arch == "arm64" && flag(&args.skip_macos_arm64, "SKIP_MACOS_ARM64"))
        }
        "linux" => {
            flag(&args.skip_linux, "SKIP_LINUX")
                || (platform.arch == "x64" && flag(&args.skip_linux_x64, "SKIP_LINUX_X64"))
                || (platform.arch == "arm64" && flag(&args.skip_linux_arm64, "SKIP_LINUX_ARM64"))
        }
        _ => false,
    }
}

fn platform_json(platform: Platform) -> String {
    format!(
        "{{\"platform\":\"{}\",\"arch\":\"{}\",\"desktop_variant\":\"{}\",\"os\":\"{}\",\"electron_arch\":\"{}\"}}",
        platform.platform,
        platform.arch,
        platform.desktop_variant,
        platform.os,
        platform.electron_arch
    )
}

fn desktop_variant_from_env() -> Result<String> {
    let variant = env::var("DESKTOP_VARIANT")
        .ok()
        .filter(|value| !value.is_empty())
        .unwrap_or_else(|| DEFAULT_DESKTOP_VARIANT.to_string());
    ensure_valid_desktop_variant(&variant)?;
    Ok(variant)
}

fn ensure_valid_desktop_variant(variant: &str) -> Result<()> {
    ensure!(
        matches!(
            variant,
            DEFAULT_DESKTOP_VARIANT | WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT
        ),
        "Unknown desktop variant: {variant}"
    );
    Ok(())
}

fn ensure_platform_supports_desktop_variant(platform: &str, variant: &str) -> Result<()> {
    ensure_valid_desktop_variant(variant)?;
    ensure!(
        variant == DEFAULT_DESKTOP_VARIANT || platform == "windows",
        "Desktop variant {variant} is only supported for Windows artifacts."
    );
    Ok(())
}

fn desktop_variant_path_segment(variant: &str) -> Option<&str> {
    if variant == DEFAULT_DESKTOP_VARIANT {
        None
    } else {
        Some(variant)
    }
}

fn workspace_dir() -> PathBuf {
    env::var("GITHUB_WORKSPACE")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
}

fn workdir() -> PathBuf {
    env::var("WORKDIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| workspace_dir())
}

fn desktop_dist_dir() -> PathBuf {
    workdir().join("fluxer_desktop").join("dist-electron")
}

async fn windows_paths_step() -> Result<()> {
    let github_workspace = require_env("GITHUB_WORKSPACE")?;
    let target = env::var("SUBST_TARGET").unwrap_or(github_workspace.clone());
    run_command(CommandSpec::new("subst").args(["W:", target.as_str()]))?;

    let temp = Path::new(r"C:\t");
    let eb_cache = Path::new(r"C:\ebcache");
    fs::create_dir_all(temp).context("Failed to create C:\\t")?;
    fs::create_dir_all(eb_cache).context("Failed to create C:\\ebcache")?;

    let arch = require_env("ARCH")?;
    let store_dir = PathBuf::from(&github_workspace).join(format!("pnpm-store-{arch}"));
    fs::create_dir_all(&store_dir)
        .with_context(|| format!("Failed to create {}", store_dir.display()))?;
    fs::write(
        Path::new(r"W:\.npmrc"),
        format!("store-dir={}\n", store_dir.display()),
    )
    .context("Failed to write W:\\.npmrc")?;

    append_github_env(&[
        ("WORKDIR", "W:"),
        ("TEMP", r"C:\t"),
        ("TMP", r"C:\t"),
        ("ELECTRON_BUILDER_CACHE", r"C:\ebcache"),
        ("NPM_CONFIG_STORE_DIR", store_dir.to_string_lossy().as_ref()),
        ("npm_config_store_dir", store_dir.to_string_lossy().as_ref()),
    ])?;

    run_command(CommandSpec::new("git").args(["config", "--global", "core.longpaths", "true"]))?;

    let git_link = Path::new(r"C:\Program Files\Git\usr\bin\link.exe");
    if git_link.exists() {
        let disabled = git_link.with_file_name("link.exe.disabled");
        remove_file_if_exists(&disabled)?;
        fs::rename(git_link, &disabled)
            .with_context(|| format!("Failed to rename {}", git_link.display()))?;
    }

    let llvm_bin = Path::new(r"C:\Program Files\LLVM\bin");
    let clang = llvm_bin.join("clang.exe");
    if !clang.exists() {
        println!("Installing LLVM...");
        let installer = runner_temp().join("LLVM-win64.exe");
        download_file(
            "https://github.com/llvm/llvm-project/releases/download/llvmorg-19.1.5/LLVM-19.1.5-win64.exe",
            &installer,
        ).await?;
        run_command(CommandSpec::new(&installer).arg("/S"))?;
    }
    ensure!(
        clang.exists(),
        "clang.exe not available at {}",
        clang.display()
    );
    append_github_path(llvm_bin)?;
    println!("Clang: {}", llvm_bin.display());
    Ok(())
}

fn set_workdir_unix_step() -> Result<()> {
    let workspace = env::var("SUBST_TARGET")
        .or_else(|_| env::var("GITHUB_WORKSPACE"))
        .unwrap_or_else(|_| ".".to_string());
    let mut env_pairs = vec![("WORKDIR", workspace.clone())];

    if env::consts::OS == "macos" {
        let arch = require_env("ARCH")?;
        let home = require_home()?;
        let store_dir = home
            .join("Library")
            .join("pnpm")
            .join(format!("store-{arch}"));
        fs::create_dir_all(&store_dir)
            .with_context(|| format!("Failed to create {}", store_dir.display()))?;
        env_pairs.push((
            "NPM_CONFIG_STORE_DIR",
            store_dir.to_string_lossy().to_string(),
        ));
        env_pairs.push((
            "npm_config_store_dir",
            store_dir.to_string_lossy().to_string(),
        ));
    }

    let pairs = env_pairs
        .iter()
        .map(|(key, value)| (*key, value.as_str()))
        .collect::<Vec<_>>();
    append_github_env(&pairs)
}

fn ensure_python3_windows_step() -> Result<()> {
    let python =
        output_text(CommandSpec::new("python").args(["-c", "import sys; print(sys.executable)"]))?;
    let python = PathBuf::from(python);
    let target = python
        .parent()
        .ok_or_else(|| anyhow!("python executable has no parent: {}", python.display()))?
        .join("python3.exe");
    if !target.exists() {
        fs::copy(&python, &target).with_context(|| {
            format!(
                "Failed to copy {} to {}",
                python.display(),
                target.display()
            )
        })?;
    }
    Ok(())
}

fn setup_pnpm_corepack_step() -> Result<()> {
    let corepack = corepack_program()?;
    run_command(CommandSpec::new(corepack.clone()).arg("enable"))?;
    run_command(CommandSpec::new(corepack).args([
        "prepare",
        &format!("pnpm@{PNPM_VERSION}"),
        "--activate",
    ]))?;
    ensure_pnpm_available()
}

fn corepack_program() -> Result<OsString> {
    if command_succeeds(CommandSpec::new("corepack").arg("--version")) {
        return Ok(OsString::from("corepack"));
    }

    if cfg!(windows) {
        let node_dir =
            node_executable_dir().context("Failed to locate Node.js while resolving corepack")?;

        for file_name in ["corepack.cmd", "corepack.exe", "corepack"] {
            let candidate = node_dir.join(file_name);
            if candidate.exists() {
                return Ok(candidate.into_os_string());
            }
        }

        bail!(
            "corepack not found on PATH or next to Node.js at {}",
            node_dir.display()
        );
    }

    bail!("corepack not found on PATH")
}

fn ensure_pnpm_available() -> Result<()> {
    if let Ok(pnpm) = pnpm_program()
        && command_succeeds(CommandSpec::new(pnpm).arg("--version"))
    {
        return Ok(());
    }

    if cfg!(windows) {
        let npm = npm_program()?;
        let pnpm_package = format!("pnpm@{PNPM_VERSION}");
        run_command(CommandSpec::new(npm.clone()).args(["install", "--global", &pnpm_package]))?;

        let npm_prefix = output_text(CommandSpec::new(npm).args(["prefix", "--global"]))
            .context("Failed to resolve global npm prefix after installing pnpm")?;
        let npm_prefix = PathBuf::from(npm_prefix);
        append_github_path(&npm_prefix)?;

        for file_name in ["pnpm.cmd", "pnpm.exe", "pnpm"] {
            let candidate = npm_prefix.join(file_name);
            if candidate.exists() {
                return run_command(CommandSpec::new(candidate.into_os_string()).arg("--version"));
            }
        }
    }

    run_command(pnpm_command()?.arg("--version"))
        .context("Failed to verify pnpm after Corepack setup")
}

fn pnpm_command() -> Result<CommandSpec> {
    Ok(CommandSpec::new(pnpm_program()?))
}

fn pnpm_program() -> Result<OsString> {
    if command_succeeds(CommandSpec::new("pnpm").arg("--version")) {
        return Ok(OsString::from("pnpm"));
    }

    if cfg!(windows) {
        for candidate in pnpm_windows_candidates() {
            if candidate.exists() {
                return Ok(candidate.into_os_string());
            }
        }
    }

    bail!("pnpm not found on PATH")
}

fn pnpm_windows_candidates() -> Vec<PathBuf> {
    let mut candidates = Vec::new();

    if let Ok(npm) = npm_program()
        && let Ok(prefix) = output_text(CommandSpec::new(npm).args(["prefix", "--global"]))
    {
        push_windows_command_candidates(&mut candidates, Path::new(&prefix), "pnpm");
    }

    if let Ok(node_dir) = node_executable_dir() {
        push_windows_command_candidates(&mut candidates, &node_dir, "pnpm");
    }

    candidates
}

fn push_windows_command_candidates(candidates: &mut Vec<PathBuf>, dir: &Path, command: &str) {
    for extension in ["cmd", "exe", ""] {
        let file_name = if extension.is_empty() {
            command.to_string()
        } else {
            format!("{command}.{extension}")
        };
        candidates.push(dir.join(file_name));
    }
}

fn npm_program() -> Result<OsString> {
    if command_succeeds(CommandSpec::new("npm").arg("--version")) {
        return Ok(OsString::from("npm"));
    }

    if cfg!(windows) {
        let node_dir =
            node_executable_dir().context("Failed to locate Node.js while resolving npm")?;

        for file_name in ["npm.cmd", "npm.exe", "npm"] {
            let candidate = node_dir.join(file_name);
            if candidate.exists() {
                return Ok(candidate.into_os_string());
            }
        }

        bail!(
            "npm not found on PATH or next to Node.js at {}",
            node_dir.display()
        );
    }

    bail!("npm not found on PATH")
}

fn node_executable_dir() -> Result<PathBuf> {
    let node = output_text(CommandSpec::new("node").args(["-p", "process.execPath"]))?;
    let node = PathBuf::from(node);
    node.parent()
        .map(Path::to_path_buf)
        .ok_or_else(|| anyhow!("Node.js executable has no parent: {}", node.display()))
}

fn resolve_pnpm_store_step() -> Result<()> {
    let store = output_text(pnpm_command()?.args(["store", "path", "--silent"]))?;
    fs::create_dir_all(&store).with_context(|| format!("Failed to create pnpm store {store}"))?;
    append_github_env(&[("PNPM_STORE_PATH", store.as_str())])
}

fn install_setuptools_windows_arm64_step() -> Result<()> {
    run_command(CommandSpec::new("python").args(["-m", "pip", "install", "--upgrade", "pip"]))?;
    run_command(CommandSpec::new("python").args([
        "-m",
        "pip",
        "install",
        "setuptools>=69",
        "wheel",
    ]))
}

fn install_setuptools_macos_step() -> Result<()> {
    let brew = if command_succeeds(CommandSpec::new("brew").arg("--version")) {
        PathBuf::from("brew")
    } else if Path::new("/opt/homebrew/bin/brew").exists() {
        PathBuf::from("/opt/homebrew/bin/brew")
    } else {
        PathBuf::from("/usr/local/bin/brew")
    };
    run_command(CommandSpec::new(brew).args(["install", "python-setuptools"]))
}

fn install_linux_deps_step() -> Result<()> {
    let apt_conf = runner_temp().join("99fluxer-ci-network");
    fs::write(
        &apt_conf,
        r#"Acquire::Retries "6";
Acquire::ForceIPv4 "true";
Acquire::http::Timeout "30";
Acquire::https::Timeout "30";
DPkg::Lock::Timeout "120";
"#,
    )
    .with_context(|| format!("Failed to write {}", apt_conf.display()))?;
    run_command(CommandSpec::new("sudo").args([
        "cp",
        apt_conf.to_string_lossy().as_ref(),
        "/etc/apt/apt.conf.d/99fluxer-ci-network",
    ]))?;

    rewrite_ubuntu_ports_sources()?;
    apt_get(&["update"])?;
    let _ = apt_get(&["remove", "-y", "--purge", "liboss4-salsa-asound2"]);
    apt_get(&[
        "install",
        "-y",
        "--no-install-recommends",
        "libx11-dev",
        "libxtst-dev",
        "libxt-dev",
        "libxinerama-dev",
        "libxkbcommon-dev",
        "libxrandr-dev",
        "ruby",
        "ruby-dev",
        "build-essential",
        "binutils",
        "nasm",
        "rpm",
        "desktop-file-utils",
        "appstream",
        "libpixman-1-dev",
        "libcairo2-dev",
        "libpango1.0-dev",
        "libjpeg-dev",
        "libgif-dev",
        "librsvg2-dev",
        "libpipewire-0.3-dev",
        "libspa-0.2-dev",
        "libdbus-1-dev",
        "libudev-dev",
        "libhunspell-dev",
        "libfido2-dev",
        "libcbor-dev",
        "libssl-dev",
        "pkg-config",
        "libegl-dev",
        "libclang-dev",
        "clang",
        "libpulse-dev",
    ])?;
    run_command(CommandSpec::new("sudo").args(["gem", "install", "--no-document", "fpm"]))
}

fn rewrite_ubuntu_ports_sources() -> Result<()> {
    let apt = Path::new("/etc/apt");
    if !apt.exists() {
        return Ok(());
    }

    for entry in WalkDir::new(apt)
        .into_iter()
        .filter_map(std::result::Result::ok)
        .filter(|entry| entry.file_type().is_file())
    {
        let path = entry.path();
        let extension = path.extension().and_then(OsStr::to_str);
        if !matches!(extension, Some("list" | "sources")) {
            continue;
        }
        run_command(CommandSpec::new("sudo").args([
            "sed",
            "-i",
            "s|http://ports.ubuntu.com/ubuntu-ports|https://ports.ubuntu.com/ubuntu-ports|g",
            path.to_string_lossy().as_ref(),
        ]))?;
    }
    Ok(())
}

fn apt_get(args: &[&str]) -> Result<()> {
    let mut last_error: Option<anyhow::Error> = None;
    for attempt in 1..=4 {
        let mut full_args = vec![
            "env",
            "DEBIAN_FRONTEND=noninteractive",
            "NEEDRESTART_MODE=a",
            "timeout",
            "--kill-after=30s",
            "600s",
            "apt-get",
            "-o",
            "Dpkg::Use-Pty=0",
            "-o",
            "Acquire::Retries=6",
            "-o",
            "Acquire::ForceIPv4=true",
            "-o",
            "Acquire::http::Timeout=30",
            "-o",
            "Acquire::https::Timeout=30",
        ];
        full_args.extend_from_slice(args);
        match run_command(CommandSpec::new("sudo").args(full_args)) {
            Ok(()) => return Ok(()),
            Err(error) if attempt < 4 => {
                last_error = Some(error);
                thread::sleep(Duration::from_secs(attempt * 20));
            }
            Err(error) => return Err(error),
        }
    }
    Err(last_error.unwrap_or_else(|| anyhow!("apt-get failed")))
}

fn install_msvc_arm64_tools_step() -> Result<()> {
    let installer =
        Path::new(r"C:\Program Files (x86)\Microsoft Visual Studio\Installer\setup.exe");
    let install_path = Path::new(r"C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools");
    run_command(CommandSpec::new(installer).args([
        "modify",
        "--installPath",
        install_path.to_string_lossy().as_ref(),
        "--add",
        "Microsoft.VisualStudio.Component.VC.Tools.ARM64",
        "--quiet",
        "--norestart",
        "--nocache",
    ]))?;

    let deadline = Instant::now() + Duration::from_secs(20 * 60);
    thread::sleep(Duration::from_secs(10));
    while Instant::now() < deadline {
        if !windows_installer_process_running()? {
            break;
        }
        thread::sleep(Duration::from_secs(10));
    }
    ensure!(
        Instant::now() < deadline,
        "VS installer did not finish within the timeout."
    );

    let mut found = false;
    let msvc_root = install_path.join("VC").join("Tools").join("MSVC");
    if msvc_root.exists() {
        for entry in fs::read_dir(&msvc_root)
            .with_context(|| format!("Failed to read {}", msvc_root.display()))?
        {
            let candidate = entry?
                .path()
                .join("bin")
                .join("HostX64")
                .join("arm64")
                .join("link.exe");
            if candidate.exists() {
                println!("ARM64 cross link.exe: {}", candidate.display());
                found = true;
            }
        }
    }
    ensure!(
        found,
        "ARM64 cross-build tools were not installed under {}\\*\\bin\\HostX64\\arm64.",
        msvc_root.display()
    );
    Ok(())
}

fn windows_installer_process_running() -> Result<bool> {
    let output = output_text(CommandSpec::new("tasklist").args(["/FO", "CSV", "/NH"]))?;
    let names = [
        "setup.exe",
        "vs_installer.exe",
        "vs_installershell.exe",
        "vs_installerservice.exe",
        "vctip.exe",
    ];
    Ok(output.lines().any(|line| {
        let first = line
            .trim_start_matches('"')
            .split('"')
            .next()
            .unwrap_or_default()
            .to_ascii_lowercase();
        names.contains(&first.as_str())
    }))
}

fn install_rust_windows_targets_step() -> Result<()> {
    let arch = require_env("ARCH")?;
    let target = if arch == "arm64" {
        "aarch64-pc-windows-msvc"
    } else {
        "x86_64-pc-windows-msvc"
    };
    run_command(CommandSpec::new("rustup").args([
        "toolchain",
        "install",
        RUST_TOOLCHAIN,
        "--profile",
        "minimal",
    ]))?;
    run_command(CommandSpec::new("rustup").args([
        "target",
        "add",
        "--toolchain",
        RUST_TOOLCHAIN,
        target,
    ]))?;
    if arch == "x64" {
        run_command(CommandSpec::new("rustup").args([
            "target",
            "add",
            "--toolchain",
            RUST_TOOLCHAIN,
            "i686-pc-windows-msvc",
        ]))?;
        run_command(CommandSpec::new("rustup").args([
            "target",
            "add",
            "--toolchain",
            RUST_TOOLCHAIN,
            "aarch64-pc-windows-msvc",
        ]))?;
    }
    if let Ok(user_profile) = env::var("USERPROFILE") {
        let cargo_bin = PathBuf::from(user_profile).join(".cargo").join("bin");
        if cargo_bin.exists() && env::var("GITHUB_PATH").is_ok() {
            append_github_path(&cargo_bin)?;
        }
    }
    run_command(CommandSpec::new("cargo").arg("--version"))
}

fn build_electron_main_step() -> Result<()> {
    run_command(
        pnpm_command()?
            .arg("build")
            .env("NODE_ENV", "production")
            .env("FLUXER_DESKTOP_PRODUCTION", "true"),
    )
}

fn install_velopack_cli_step() -> Result<()> {
    let tool_dir = env::current_dir()
        .context("Failed to resolve current directory")?
        .join(".velopack");
    fs::create_dir_all(&tool_dir)
        .with_context(|| format!("Failed to create {}", tool_dir.display()))?;
    run_command(CommandSpec::new("dotnet").args([
        "tool",
        "install",
        "--tool-path",
        tool_dir.to_string_lossy().as_ref(),
        "vpk",
        "--version",
        "0.0.1298",
    ]))
}

#[derive(Debug, Clone, Copy)]
enum DesktopBuildPlatform {
    Macos,
    Windows,
    Linux,
}

impl DesktopBuildPlatform {
    fn electron_builder_target(self) -> &'static str {
        match self {
            Self::Macos => "--mac",
            Self::Windows => "--win",
            Self::Linux => "--linux",
        }
    }

    fn transient_patterns(self) -> &'static [&'static str] {
        match self {
            Self::Windows => &[
                "RCX",
                ".tmp",
                "EOF",
                "status code 5",
                "cannot resolve",
                "i/o timeout",
                "connection reset",
                "TLS handshake",
            ],
            Self::Macos | Self::Linux => &[
                "EOF",
                "status code 5",
                "cannot resolve",
                "i/o timeout",
                "connection reset",
                "TLS handshake",
            ],
        }
    }

    fn retry_sleep(self) -> Duration {
        match self {
            Self::Windows => Duration::from_secs(5),
            Self::Macos | Self::Linux => Duration::from_secs(10),
        }
    }
}

fn build_app_step(platform: DesktopBuildPlatform) -> Result<()> {
    let macos_keychain = if matches!(platform, DesktopBuildPlatform::Macos) {
        Some(validate_macos_signing_env()?)
    } else {
        None
    };

    if let Some(keychain) = &macos_keychain {
        println!(
            "Using macOS signing keychain for electron-builder: {}",
            keychain.display()
        );
    }

    let electron_arch = require_env("ELECTRON_ARCH")?;
    for attempt in 1..=3 {
        println!(
            "::group::electron-builder {:?} attempt {attempt}/3",
            platform
        );
        let mut command = pnpm_command()?
            .args([
                "exec",
                "electron-builder",
                "--config",
                "electron-builder.config.cjs",
                platform.electron_builder_target(),
                &format!("--{electron_arch}"),
            ])
            .env("ELECTRON_ARCH", &electron_arch);
        if let Some(keychain) = &macos_keychain {
            command = command
                .env("CSC_KEYCHAIN", keychain.as_os_str())
                .env_remove("CSC_LINK")
                .env_remove("CSC_KEY_PASSWORD");
        }
        let result = capture(command);
        println!("::endgroup::");

        match result {
            Ok(output) if output.status == 0 => return Ok(()),
            Ok(output) => {
                let log = format!(
                    "{}{}",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr)
                );
                if attempt < 3 && is_transient_failure(&log, platform.transient_patterns()) {
                    println!("Detected transient build failure; cleaning and retrying.");
                    clean_electron_builder_outputs(platform)?;
                    thread::sleep(platform.retry_sleep());
                    continue;
                }
                bail!("electron-builder failed with exit code {}", output.status);
            }
            Err(error) if attempt < 3 => {
                println!("electron-builder failed to start: {error:?}; retrying.");
                clean_electron_builder_outputs(platform)?;
                thread::sleep(platform.retry_sleep());
            }
            Err(error) => return Err(error),
        }
    }
    bail!("electron-builder failed after retries")
}

fn validate_macos_signing_env() -> Result<PathBuf> {
    let missing = ["APPLE_ID", "APPLE_APP_SPECIFIC_PASSWORD", "APPLE_TEAM_ID"]
        .into_iter()
        .filter(|name| env_string(name).is_none())
        .collect::<Vec<_>>();
    ensure!(
        missing.is_empty(),
        "Missing macOS notarization environment variables: {}. APPLE_ID maps to repo secret APPLE_ID; APPLE_APP_SPECIFIC_PASSWORD maps to APPLE_PASSWORD; APPLE_TEAM_ID maps to APPLE_TEAM_ID.",
        missing.join(" ")
    );

    let keychain = require_home()?.join("Library/Keychains/fluxer-build.keychain-db");
    ensure!(
        keychain.exists(),
        "Signing keychain {} not found on runner host. Run the runner's keychain bootstrap to import the Developer ID cert.",
        keychain.display()
    );
    run_command(CommandSpec::new("security").args([
        "unlock-keychain",
        "-p",
        "",
        keychain.to_string_lossy().as_ref(),
    ]))?;
    let identities = output_text(CommandSpec::new("security").args([
        "find-identity",
        "-v",
        "-p",
        "codesigning",
        keychain.to_string_lossy().as_ref(),
    ]))?;
    ensure!(
        identities.contains("Developer ID Application"),
        "No valid Developer ID Application identity in {}.",
        keychain.display()
    );
    Ok(keychain)
}

fn is_transient_failure(log: &str, patterns: &[&str]) -> bool {
    patterns.iter().any(|pattern| log.contains(pattern))
}

fn clean_electron_builder_outputs(platform: DesktopBuildPlatform) -> Result<()> {
    let dist = Path::new("dist-electron");
    if !dist.exists() {
        return Ok(());
    }
    match platform {
        DesktopBuildPlatform::Macos => {
            remove_dir_if_exists(&dist.join("mac"))?;
            remove_dir_if_exists(&dist.join("mac-arm64"))?;
        }
        DesktopBuildPlatform::Windows => {
            remove_dir_if_exists(&dist.join("win-unpacked"))?;
        }
        DesktopBuildPlatform::Linux => {
            remove_dir_if_exists(&dist.join("linux-unpacked"))?;
        }
    }
    for entry in fs::read_dir(dist).with_context(|| format!("Failed to read {}", dist.display()))? {
        let path = entry?.path();
        if path.is_dir()
            && path
                .file_name()
                .and_then(OsStr::to_str)
                .is_some_and(|name| name.ends_with("-unpacked"))
        {
            remove_dir_if_exists(&path)?;
        }
    }
    if matches!(platform, DesktopBuildPlatform::Windows) {
        for entry in WalkDir::new(dist)
            .into_iter()
            .filter_map(std::result::Result::ok)
            .filter(|entry| entry.file_type().is_file())
        {
            let name = entry.file_name().to_string_lossy();
            if name.starts_with("RCX") && name.ends_with(".tmp") {
                remove_file_if_exists(entry.path())?;
            }
        }
    }
    Ok(())
}

fn verify_bundle_id_step() -> Result<()> {
    let electron_arch = require_env("ELECTRON_ARCH")?;
    let build_channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    let zip_path = find_dist_file(Path::new("dist-electron"), |name| {
        name.ends_with(".zip") && name.contains(&electron_arch)
    })
    .or_else(|| find_dist_file(Path::new("dist-electron"), |name| name.ends_with(".zip")))
    .ok_or_else(|| anyhow!("No macOS zip artifact found in dist-electron"))?;

    let temp = TempDir::new().context("Failed to create temp directory")?;
    run_command(CommandSpec::new("ditto").args([
        "-xk",
        zip_path.to_string_lossy().as_ref(),
        temp.path().to_string_lossy().as_ref(),
    ]))?;

    let app = find_first(temp.path(), |path| {
        path.extension().and_then(OsStr::to_str) == Some("app")
    })
    .ok_or_else(|| {
        anyhow!(
            "No .app bundle found after extracting {}",
            zip_path.display()
        )
    })?;
    let info_plist = app.join("Contents").join("Info.plist");
    let profile = app.join("Contents").join("embedded.provisionprofile");
    let bid = output_text(CommandSpec::new("/usr/libexec/PlistBuddy").args([
        "-c",
        "Print :CFBundleIdentifier",
        info_plist.to_string_lossy().as_ref(),
    ]))?;

    let expected = if build_channel == "canary" {
        "app.fluxer.canary"
    } else {
        "app.fluxer"
    };
    let expected_profile = if build_channel == "canary" {
        "3G5837T29K.app.fluxer.canary"
    } else {
        "3G5837T29K.app.fluxer"
    };
    println!("Bundle id in zip: {bid} (expected: {expected})");
    ensure!(bid == expected, "Unexpected bundle id: {bid}");
    ensure!(
        profile.exists(),
        "Missing provisioning profile: {}",
        profile.display()
    );

    let decoded_profile = temp.path().join("embedded.provisionprofile.plist");
    let decoded = output_bytes(CommandSpec::new("security").args([
        "cms",
        "-D",
        "-i",
        profile.to_string_lossy().as_ref(),
    ]))?;
    fs::write(&decoded_profile, decoded)
        .with_context(|| format!("Failed to write {}", decoded_profile.display()))?;
    let profile_app_id = output_text(CommandSpec::new("/usr/libexec/PlistBuddy").args([
        "-c",
        "Print :Entitlements:com.apple.application-identifier",
        decoded_profile.to_string_lossy().as_ref(),
    ]))?;
    println!("Provisioning profile app id: {profile_app_id} (expected: {expected_profile})");
    ensure!(
        profile_app_id == expected_profile,
        "Unexpected provisioning profile app id: {profile_app_id}"
    );

    let expected_macho_arch = if electron_arch == "arm64" {
        "arm64"
    } else {
        "x86_64"
    };
    let native_rels = macos_native_runtime_rels(&electron_arch);
    for rel in native_rels {
        let native_file = app
            .join("Contents")
            .join("Resources")
            .join("app.asar.unpacked")
            .join("node_modules")
            .join(rel);
        ensure!(
            native_file.exists(),
            "Missing native runtime artifact: {}",
            native_file.display()
        );
        println!("Found native runtime artifact: {}", native_file.display());
        check_macho_arch(&native_file, expected_macho_arch, &electron_arch)?;
    }

    run_command(CommandSpec::new("codesign").args([
        "--verify",
        "--deep",
        "--strict",
        "--verbose=4",
        app.to_string_lossy().as_ref(),
    ]))?;
    run_command(CommandSpec::new("xcrun").args([
        "stapler",
        "validate",
        app.to_string_lossy().as_ref(),
    ]))?;
    run_command(CommandSpec::new("spctl").args([
        "--assess",
        "--type",
        "execute",
        "--verbose=4",
        app.to_string_lossy().as_ref(),
    ]))
}

fn macos_native_runtime_rels(electron_arch: &str) -> Vec<String> {
    [
        "@fluxer/webauthn/webauthn",
        "@fluxer/mac-app-audio/mac-app-audio",
        "@fluxer/mac-clipboard/mac-clipboard",
        "@fluxer/mac-sysctl/mac-sysctl",
        "@fluxer/mac-tcc/mac-tcc",
        "@fluxer/macos-input-hook/macos-input-hook",
        "@fluxer/platform-info/platform-info",
    ]
    .into_iter()
    .map(|prefix| format!("{prefix}.darwin-{electron_arch}.node"))
    .collect()
}

fn check_macho_arch(file: &Path, expected: &str, electron_arch: &str) -> Result<()> {
    let archs =
        output_text(CommandSpec::new("lipo").args(["-archs", file.to_string_lossy().as_ref()]))?;
    println!("Mach-O archs for {}: {archs}", file.display());
    let arch_list = archs.split_whitespace().collect::<Vec<_>>();
    ensure!(
        arch_list.contains(&expected),
        "{} has Mach-O archs '{archs}', expected '{expected}'",
        file.display()
    );
    ensure!(
        !(electron_arch == "x64"
            && arch_list.contains(&"x86_64h")
            && !arch_list.contains(&"x86_64")),
        "{} is x86_64h-only; x64 desktop artifacts must use baseline x86_64",
        file.display()
    );
    Ok(())
}

const WINDOWS_SIGNING_ENV: &[&str] = &[
    "AZURE_CLIENT_ID",
    "AZURE_TENANT_ID",
    "AZURE_SUBSCRIPTION_ID",
    "AZURE_ARTIFACT_SIGNING_ENDPOINT",
    "AZURE_ARTIFACT_SIGNING_ACCOUNT_NAME",
    "AZURE_ARTIFACT_SIGNING_CERTIFICATE_PROFILE_NAME",
];
const VELOPACK_TRUSTED_SIGN_FILE_ENV: &str = "VELOPACK_TRUSTED_SIGN_FILE";
const TRUSTED_SIGNING_EXCLUDED_CREDENTIALS: &[&str] = &[
    "ManagedIdentityCredential",
    "WorkloadIdentityCredential",
    "SharedTokenCacheCredential",
    "VisualStudioCredential",
    "VisualStudioCodeCredential",
    "AzurePowerShellCredential",
    "AzureDeveloperCliCredential",
    "InteractiveBrowserCredential",
];

fn validate_windows_signing_inputs_step() -> Result<()> {
    let missing = WINDOWS_SIGNING_ENV
        .iter()
        .copied()
        .filter(|name| env_string(name).is_none())
        .collect::<Vec<_>>();
    ensure!(
        missing.is_empty(),
        "Missing Windows code signing environment variables: {}. Windows releases are always signed; every Azure Trusted Signing input is mandatory and there is no unsigned fallback.",
        missing.join(" ")
    );
    println!(
        "Windows code signing inputs present: {}",
        WINDOWS_SIGNING_ENV.join(" ")
    );
    Ok(())
}

#[derive(Debug, Serialize)]
struct TrustedSigningMetadata {
    #[serde(rename = "Endpoint")]
    endpoint: String,
    #[serde(rename = "CodeSigningAccountName")]
    code_signing_account_name: String,
    #[serde(rename = "CertificateProfileName")]
    certificate_profile_name: String,
    #[serde(rename = "ExcludeCredentials")]
    exclude_credentials: Vec<&'static str>,
}

fn windows_trusted_signing_metadata_path() -> PathBuf {
    runner_temp().join("velopack-trusted-signing.json")
}

fn write_windows_signing_metadata_step() -> Result<()> {
    validate_windows_signing_inputs_step()?;
    let metadata = TrustedSigningMetadata {
        endpoint: require_env("AZURE_ARTIFACT_SIGNING_ENDPOINT")?,
        code_signing_account_name: require_env("AZURE_ARTIFACT_SIGNING_ACCOUNT_NAME")?,
        certificate_profile_name: require_env("AZURE_ARTIFACT_SIGNING_CERTIFICATE_PROFILE_NAME")?,
        exclude_credentials: TRUSTED_SIGNING_EXCLUDED_CREDENTIALS.to_vec(),
    };
    let path = windows_trusted_signing_metadata_path();
    write_json_pretty(&path, &metadata)?;
    println!(
        "Wrote Velopack Trusted Signing metadata to {} (never staged for upload).",
        path.display()
    );
    append_github_env(&[(
        VELOPACK_TRUSTED_SIGN_FILE_ENV,
        path.to_string_lossy().as_ref(),
    )])
}

fn resolve_windows_unpacked_dir_step() -> Result<()> {
    let build_channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    let arch = require_env("ARCH")?;
    let config = windows_package_config(&build_channel, &arch);
    let pack_dir = resolve_windows_unpacked_dir(&arch, &config.main_exe)?;
    println!(
        "Resolved unpacked Windows app directory: {}",
        pack_dir.display()
    );
    append_github_output(&[("unpacked_dir", pack_dir.to_string_lossy().as_ref())])
}

fn resolve_windows_unpacked_dir(arch: &str, main_exe: &str) -> Result<PathBuf> {
    let pack_dir = find_windows_unpacked_app(arch, main_exe)
        .ok_or_else(|| anyhow!("Unable to find unpacked Windows app containing {main_exe}"))?;
    let absolute = env::current_dir()
        .context("Failed to resolve current directory")?
        .join(pack_dir);
    ensure!(
        absolute.is_dir(),
        "Unpacked Windows app directory does not exist: {}",
        absolute.display()
    );
    Ok(absolute)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct WindowsPackageConfig {
    pack_id: &'static str,
    pack_title: &'static str,
    icon_dir: &'static str,
    runtime: &'static str,
    main_exe: String,
    output_dir: PathBuf,
}

fn windows_package_config(build_channel: &str, arch: &str) -> WindowsPackageConfig {
    let canary = build_channel == "canary";
    let pack_title = if canary { "Fluxer Canary" } else { "Fluxer" };
    WindowsPackageConfig {
        pack_id: if canary {
            "fluxer_desktop_canary"
        } else {
            "fluxer_desktop"
        },
        pack_title,
        icon_dir: if canary {
            "icons-canary"
        } else {
            "icons-stable"
        },
        runtime: if arch == "arm64" {
            "win-arm64"
        } else {
            "win-x64"
        },
        main_exe: format!("{pack_title}.exe"),
        output_dir: PathBuf::from("dist-electron").join(format!("velopack-windows-{arch}")),
    }
}

fn package_app_windows_velopack_step() -> Result<()> {
    let build_channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    let arch = require_env("ARCH")?;
    let version = require_env("VERSION")?;
    let config = windows_package_config(&build_channel, &arch);
    remove_dir_if_exists(&config.output_dir)?;

    let pack_dir = find_windows_unpacked_app(&arch, &config.main_exe).ok_or_else(|| {
        anyhow!(
            "Unable to find unpacked Windows app containing {}",
            config.main_exe
        )
    })?;
    let vpk = find_velopack_cli()?;
    let trusted_sign_file = PathBuf::from(require_env(VELOPACK_TRUSTED_SIGN_FILE_ENV).context(
        "Velopack packaging requires the Trusted Signing metadata written by the write_windows_signing_metadata step. Windows packages are never produced unsigned.",
    )?);
    ensure!(
        trusted_sign_file.is_file(),
        "Velopack Trusted Signing metadata file is missing: {}",
        trusted_sign_file.display()
    );
    let packaged = pack_and_validate_windows_velopack(
        &vpk,
        &config,
        &version,
        &arch,
        &pack_dir,
        &trusted_sign_file,
    );
    let metadata_removed = remove_file_if_exists(&trusted_sign_file);
    packaged?;
    metadata_removed?;
    print_directory(&config.output_dir)
}

fn pack_and_validate_windows_velopack(
    vpk: &Path,
    config: &WindowsPackageConfig,
    version: &str,
    arch: &str,
    pack_dir: &Path,
    trusted_sign_file: &Path,
) -> Result<()> {
    ensure_velopack_pack_supports(vpk, &["--azureTrustedSignFile"])?;

    run_command(CommandSpec::new(vpk).args([
        "--yes",
        "pack",
        "--packId",
        config.pack_id,
        "--packVersion",
        version,
        "--packDir",
        pack_dir.to_string_lossy().as_ref(),
        "--mainExe",
        config.main_exe.as_str(),
        "--packTitle",
        config.pack_title,
        "--packAuthors",
        "Fluxer Platform AB",
        "--shortcuts",
        "Desktop,StartMenu",
        "--runtime",
        config.runtime,
        "--icon",
        &format!("build_resources/{}/icon.ico", config.icon_dir),
        "--outputDir",
        config.output_dir.to_string_lossy().as_ref(),
        "--delta",
        "BestSpeed",
        "--azureTrustedSignFile",
        trusted_sign_file.to_string_lossy().as_ref(),
    ]))?;

    validate_velopack_output(config, version, arch)?;
    remove_velopack_portable_archives(&config.output_dir)
}

fn remove_velopack_portable_archives(output_dir: &Path) -> Result<()> {
    for path in collect_files(output_dir)? {
        if !extension_is(&path, "zip") {
            continue;
        }
        fs::remove_file(&path).with_context(|| format!("Failed to remove {}", path.display()))?;
        println!(
            "Removed Velopack portable archive {}. Fluxer publishes its own portable ZIP built from the signed application tree.",
            path.display()
        );
    }
    Ok(())
}

fn ensure_velopack_pack_supports(vpk: &Path, options: &[&str]) -> Result<()> {
    let help = capture(CommandSpec::new(vpk).args(["pack", "--help"]))
        .context("Failed to read `vpk pack --help` from the pinned Velopack CLI")?;
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&help.stdout),
        String::from_utf8_lossy(&help.stderr)
    );
    let missing = options
        .iter()
        .filter(|option| !text.contains(**option))
        .copied()
        .collect::<Vec<_>>();
    ensure!(
        missing.is_empty(),
        "The pinned Velopack CLI does not support {}. Update the pin or rework the packaging step before releasing.",
        missing.join(", ")
    );
    Ok(())
}

fn validate_velopack_output(
    config: &WindowsPackageConfig,
    version: &str,
    arch: &str,
) -> Result<()> {
    let legacy_releases = config.output_dir.join("RELEASES");
    let velopack_releases = config.output_dir.join("releases.win.json");
    let full_nupkg = first_file_matching(&config.output_dir, |name| name.ends_with("-full.nupkg"));

    ensure!(
        legacy_releases.exists(),
        "Velopack did not produce the legacy Squirrel RELEASES file. Do not pass --channel to vpk pack for Windows, or old Squirrel clients cannot migrate."
    );
    ensure!(
        velopack_releases.exists(),
        "Velopack did not produce releases.win.json for Windows updates."
    );
    let full_nupkg = full_nupkg.ok_or_else(|| {
        anyhow!("Velopack did not produce a full nupkg payload for Windows updates.")
    })?;
    let release_feed = fs::read_to_string(&legacy_releases)
        .with_context(|| format!("Failed to read {}", legacy_releases.display()))?;
    let nupkg_name = file_name_string(&full_nupkg)?;
    ensure!(
        release_feed.contains(&nupkg_name),
        "The legacy Squirrel RELEASES file does not reference {nupkg_name}."
    );

    let setup_exe = first_file_matching(&config.output_dir, |name| name.ends_with("-Setup.exe"))
        .ok_or_else(|| {
            anyhow!(
                "Velopack did not produce a Setup.exe in {}.",
                config.output_dir.display()
            )
        })?;
    let desired_setup_name = format!("{}-{version}-win-{arch}.exe", config.pack_title);
    if file_name_string(&setup_exe)? != desired_setup_name {
        fs::rename(&setup_exe, config.output_dir.join(desired_setup_name))
            .with_context(|| format!("Failed to rename {}", setup_exe.display()))?;
    }
    Ok(())
}

fn find_windows_unpacked_app(arch: &str, main_exe: &str) -> Option<PathBuf> {
    windows_unpacked_candidates(arch)
        .into_iter()
        .find(|candidate| candidate.join(main_exe).exists())
}

fn windows_unpacked_candidates(arch: &str) -> Vec<PathBuf> {
    let mut candidates = Vec::new();
    if arch == "arm64" {
        candidates.push(PathBuf::from("dist-electron/win-arm64-unpacked"));
    }
    candidates.push(PathBuf::from("dist-electron/win-unpacked"));
    candidates
}

fn find_velopack_cli() -> Result<PathBuf> {
    let candidates = [".velopack/vpk.exe", ".velopack/vpk"];
    candidates
        .into_iter()
        .map(PathBuf::from)
        .find(|path| path.exists())
        .ok_or_else(|| anyhow!("Velopack CLI was not installed under .velopack"))
}

fn analyse_velopack_paths_step() -> Result<()> {
    let arch = require_env("ARCH")?;
    let build_channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    let config = windows_package_config(&build_channel, &arch);
    let nupkg = first_file_matching(&config.output_dir, |name| name.ends_with("-full.nupkg"))
        .ok_or_else(|| {
            anyhow!(
                "No Velopack full nupkg found in: {}",
                config.output_dir.display()
            )
        })?;

    println!("Analyzing Velopack package {}", nupkg.display());
    let local_app_data = require_env("LOCALAPPDATA")?;
    let prefix = PathBuf::from(local_app_data)
        .join(config.pack_id)
        .join("current")
        .join("resources")
        .join("app.asar.unpacked");
    let max_len = env::var("MAX_WINDOWS_PATH_LEN")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(260);
    let headroom = env::var("PATH_HEADROOM")
        .ok()
        .and_then(|value| value.parse::<usize>().ok())
        .unwrap_or(10);
    let limit = max_len.saturating_sub(headroom);
    let entries = velopack_path_lengths(&nupkg, &prefix)?;

    ensure!(!entries.is_empty(), "nupkg archive contains no entries");
    println!(
        "Assumed install prefix: {} ({} chars). Maximum allowed path length: {limit} (total reserve {max_len}, headroom {headroom}).",
        prefix.display(),
        prefix.to_string_lossy().len()
    );
    println!("Top 20 longest archived paths (length includes prefix):");
    for entry in entries.iter().take(20) {
        println!("{:4} {}", entry.length, entry.name);
    }
    let longest = entries.first().expect("entries not empty");
    ensure!(
        longest.length <= limit,
        "Longest path {} for {} exceeds limit {limit}",
        longest.length,
        longest.name
    );
    println!(
        "Longest archived path {} is within the limit of {limit}.",
        longest.length
    );
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ArchivePathLength {
    length: usize,
    name: String,
}

fn velopack_path_lengths(nupkg: &Path, prefix: &Path) -> Result<Vec<ArchivePathLength>> {
    let file = File::open(nupkg).with_context(|| format!("Failed to open {}", nupkg.display()))?;
    let mut archive = zip::ZipArchive::new(file)
        .with_context(|| format!("Failed to read zip {}", nupkg.display()))?;
    let mut entries = Vec::new();
    for index in 0..archive.len() {
        let entry = archive.by_index(index)?;
        let normalized = entry
            .name()
            .trim_start_matches(['/', '\\'])
            .replace('\\', "/");
        let full = if normalized.is_empty() {
            prefix.to_path_buf()
        } else {
            prefix.join(&normalized)
        };
        entries.push(ArchivePathLength {
            length: full.to_string_lossy().len(),
            name: entry.name().to_string(),
        });
    }
    entries.sort_by(|a, b| b.length.cmp(&a.length).then_with(|| a.name.cmp(&b.name)));
    Ok(entries)
}

fn create_portable_zip_windows_step() -> Result<()> {
    let build_channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    let arch = require_env("ARCH")?;
    let version = require_env("VERSION")?;
    let config = windows_package_config(&build_channel, &arch);
    let Some(pack_dir) = find_windows_unpacked_app(&arch, &config.main_exe) else {
        println!("No unpacked Windows app found; skipping portable ZIP.");
        return Ok(());
    };
    let portable_marker = pack_dir.join(".portable");
    fs::write(&portable_marker, "")
        .with_context(|| format!("Failed to write {}", portable_marker.display()))?;
    let zip_name = format!("{}-{version}-portable-win-{arch}.zip", config.pack_title);
    let zip_path = PathBuf::from("dist-electron").join(zip_name);
    create_zip_from_dir(&pack_dir, &zip_path)?;
    remove_file_if_exists(&portable_marker)?;
    let size_mb = fs::metadata(&zip_path)?.len() as f64 / 1024.0 / 1024.0;
    println!(
        "Created portable ZIP: {} ({size_mb:.1} MB); removed {} so installed builds are not marked portable.",
        zip_path.display(),
        portable_marker.display()
    );
    Ok(())
}

const FLUXER_WINDOWS_SIGNER_COMMON_NAME: &str = "Fluxer Platform AB";
const THIRD_PARTY_PUBLISHER_ALLOWLIST: &[&str] = &[];
const KNOWN_OPTIONAL_WINDOWS_PE_INVENTORY: &[&str] = &["fluxer-vulkan-layer.win32-ia32-msvc.dll"];
const WINDOWS_NATIVE_ADDON_STEMS: &[&str] = &[
    "webauthn",
    "webrtc-sender",
    "win-process-loopback",
    "win-clipboard",
    "win-shell",
    "win-toast",
    "windows-input-hook",
    "platform-info",
];

fn expected_windows_pe_inventory(arch: &str, main_exe: &str) -> Vec<String> {
    let tag = format!("win32-{arch}-msvc");
    let mut names = vec![
        main_exe.to_string(),
        format!("velopack_nodeffi_win_{arch}_msvc.node"),
        format!("win-game-capture.{tag}.node"),
        format!("fluxer-game-hook.{tag}.dll"),
        format!("fluxer-inject-helper.{tag}.exe"),
        format!("fluxer-vulkan-layer.{tag}.dll"),
    ];
    names.extend(
        WINDOWS_NATIVE_ADDON_STEMS
            .iter()
            .map(|stem| format!("{stem}.{tag}.node")),
    );
    if arch == "x64" {
        names.push("fluxer-game-hook.win32-ia32-msvc.dll".to_string());
        names.push("fluxer-inject-helper.win32-ia32-msvc.exe".to_string());
    }
    names.sort();
    names.dedup();
    names
}

fn is_pe_file(path: &Path) -> Result<bool> {
    let mut file =
        File::open(path).with_context(|| format!("Failed to open {}", path.display()))?;
    let mut dos_header = [0u8; 0x40];
    if !read_exact_or_eof(&mut file, &mut dos_header, path)? {
        return Ok(false);
    }
    if &dos_header[0..2] != b"MZ" {
        return Ok(false);
    }
    let e_lfanew = u32::from_le_bytes([
        dos_header[0x3c],
        dos_header[0x3d],
        dos_header[0x3e],
        dos_header[0x3f],
    ]);
    file.seek(SeekFrom::Start(u64::from(e_lfanew)))
        .with_context(|| format!("Failed to seek in {}", path.display()))?;
    let mut signature = [0u8; 4];
    if !read_exact_or_eof(&mut file, &mut signature, path)? {
        return Ok(false);
    }
    Ok(&signature == b"PE\0\0")
}

fn read_exact_or_eof(file: &mut File, buffer: &mut [u8], path: &Path) -> Result<bool> {
    match file.read_exact(buffer) {
        Ok(()) => Ok(true),
        Err(error) if error.kind() == io::ErrorKind::UnexpectedEof => Ok(false),
        Err(error) => Err(error).with_context(|| format!("Failed to read {}", path.display())),
    }
}

fn collect_pe_files(root: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for path in collect_files(root)? {
        if is_pe_file(&path)? {
            files.push(path);
        }
    }
    Ok(files)
}

fn absolute_path(path: &Path) -> Result<PathBuf> {
    if path.is_absolute() {
        return Ok(path.to_path_buf());
    }
    Ok(env::current_dir()
        .context("Failed to resolve current directory")?
        .join(path))
}

fn relative_display(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

fn percent_decode_archive_name(name: &str) -> String {
    if !name.contains('%') {
        return name.to_string();
    }
    let bytes = name.as_bytes();
    let mut decoded = Vec::with_capacity(bytes.len());
    let mut index = 0;
    while index < bytes.len() {
        if bytes[index] == b'%' && index + 2 < bytes.len() {
            let high = (bytes[index + 1] as char).to_digit(16);
            let low = (bytes[index + 2] as char).to_digit(16);
            if let (Some(high), Some(low)) = (high, low) {
                decoded.push((high * 16 + low) as u8);
                index += 3;
                continue;
            }
        }
        decoded.push(bytes[index]);
        index += 1;
    }
    String::from_utf8(decoded).unwrap_or_else(|_| name.to_string())
}

fn assert_expected_windows_pe_inventory(
    root: &Path,
    files: &[PathBuf],
    arch: &str,
    main_exe: &str,
) -> Result<()> {
    let present = files
        .iter()
        .filter_map(|path| path.file_name().and_then(OsStr::to_str))
        .map(percent_decode_archive_name)
        .collect::<BTreeSet<_>>();
    let expected = expected_windows_pe_inventory(arch, main_exe);
    let missing = expected
        .iter()
        .filter(|name| !present.contains(*name))
        .cloned()
        .collect::<Vec<_>>();
    ensure!(
        missing.is_empty(),
        "{} is missing {} expected Windows binaries:\n{}",
        root.display(),
        missing.len(),
        missing.join("\n")
    );
    let contradictory = contradictory_optional_windows_pe_inventory(arch, main_exe);
    ensure!(
        contradictory.is_empty(),
        "KNOWN_OPTIONAL_WINDOWS_PE_INVENTORY lists {} binary/binaries that {arch} also requires, so the inventory contradicts itself:\n{}",
        contradictory.len(),
        contradictory.join("\n")
    );
    for name in KNOWN_OPTIONAL_WINDOWS_PE_INVENTORY {
        println!(
            "Known-optional Windows binary {name}: {}",
            if present.contains(*name) {
                "present"
            } else {
                "absent"
            }
        );
    }
    let unlisted = present
        .iter()
        .filter(|name| {
            !expected.iter().any(|value| value == *name)
                && !KNOWN_OPTIONAL_WINDOWS_PE_INVENTORY.contains(&name.as_str())
        })
        .cloned()
        .collect::<Vec<_>>();
    println!(
        "{}: {} expected, {} unlisted PE(s) shipped by glob (Electron runtime and cross-architecture native artifacts). Every one of them is signature-classified below; none may be unsigned or signed by an unknown publisher.",
        root.display(),
        expected.len(),
        unlisted.len()
    );
    for name in &unlisted {
        println!("Unlisted Windows PE pending signature classification: {name}");
    }
    Ok(())
}

fn contradictory_optional_windows_pe_inventory(arch: &str, main_exe: &str) -> Vec<String> {
    let expected = expected_windows_pe_inventory(arch, main_exe);
    KNOWN_OPTIONAL_WINDOWS_PE_INVENTORY
        .iter()
        .filter(|name| expected.iter().any(|value| value == *name))
        .map(|name| (*name).to_string())
        .collect()
}

#[derive(Debug, Clone, Deserialize)]
struct SignatureRow {
    #[serde(rename = "Path")]
    path: String,
    #[serde(rename = "Status")]
    status: String,
    #[serde(rename = "Subject")]
    subject: Option<String>,
    #[serde(rename = "Thumbprint")]
    thumbprint: Option<String>,
    #[serde(rename = "TsSubject")]
    ts_subject: Option<String>,
}

fn authenticode_report(files: &[PathBuf]) -> Result<Vec<SignatureRow>> {
    let temp = TempDir::new().context("Failed to create Authenticode report temp directory")?;
    let list_path = temp.path().join("paths.txt");
    let mut list = String::new();
    for file in files {
        list.push_str(file.to_string_lossy().as_ref());
        list.push('\n');
    }
    fs::write(&list_path, list)
        .with_context(|| format!("Failed to write {}", list_path.display()))?;

    let script_path = temp.path().join("authenticode-report.ps1");
    fs::write(&script_path, authenticode_report_script(&list_path))
        .with_context(|| format!("Failed to write {}", script_path.display()))?;

    let output = capture(
        CommandSpec::new(powershell_host())
            .args([
                "-NoProfile",
                "-NonInteractive",
                "-ExecutionPolicy",
                "Bypass",
                "-File",
                script_path.to_string_lossy().as_ref(),
            ])
            .env_remove("PSModulePath"),
    )?;
    ensure!(
        output.status == 0,
        "Get-AuthenticodeSignature failed with exit code {}",
        output.status
    );
    let stdout = String::from_utf8(output.stdout)
        .context("Get-AuthenticodeSignature output was not UTF-8")?;
    parse_authenticode_report(stdout.trim())
}

fn powershell_host() -> &'static str {
    if which_in_path("pwsh.exe").is_some() || which_in_path("pwsh").is_some() {
        return "pwsh";
    }
    "powershell"
}

fn which_in_path(program: &str) -> Option<PathBuf> {
    let path = env::var_os("PATH")?;
    env::split_paths(&path)
        .map(|directory| directory.join(program))
        .find(|candidate| candidate.is_file())
}

fn authenticode_report_script(list_path: &Path) -> String {
    format!(
        "$ErrorActionPreference = 'Stop'\n\
[Console]::OutputEncoding = New-Object System.Text.UTF8Encoding $false\n\
$paths = @(Get-Content -LiteralPath '{}' -Encoding UTF8 | Where-Object {{ $_ -ne '' }})\n\
$rows = @(Get-AuthenticodeSignature -LiteralPath $paths | Select-Object \
@{{n='Path';e={{[string]$_.Path}}}}, \
@{{n='Status';e={{[string]$_.Status}}}}, \
@{{n='Subject';e={{if ($_.SignerCertificate) {{ [string]$_.SignerCertificate.Subject }} else {{ $null }}}}}}, \
@{{n='Thumbprint';e={{if ($_.SignerCertificate) {{ [string]$_.SignerCertificate.Thumbprint }} else {{ $null }}}}}}, \
@{{n='TsSubject';e={{if ($_.TimeStamperCertificate) {{ [string]$_.TimeStamperCertificate.Subject }} else {{ $null }}}}}})\n\
ConvertTo-Json -InputObject $rows -Depth 3 -Compress\n",
        list_path.display()
    )
}

fn parse_authenticode_report(json: &str) -> Result<Vec<SignatureRow>> {
    let json = json.trim_start_matches('\u{feff}').trim();
    ensure!(
        !json.is_empty(),
        "Get-AuthenticodeSignature produced no output."
    );
    let value: Value =
        serde_json::from_str(json).context("Failed to parse Get-AuthenticodeSignature JSON")?;
    let rows = match value {
        Value::Array(items) => items,
        single => vec![single],
    };
    rows.into_iter()
        .map(|row| {
            serde_json::from_value::<SignatureRow>(row)
                .context("Failed to parse Get-AuthenticodeSignature row")
        })
        .collect()
}

fn certificate_common_name(subject: &str) -> Option<&str> {
    subject
        .split(", ")
        .find_map(|component| component.strip_prefix("CN="))
}

fn assert_fluxer_signed(row: &SignatureRow) -> Result<()> {
    ensure!(
        row.status == "Valid",
        "Authenticode status is {} (expected Valid)",
        row.status
    );
    ensure!(
        row.ts_subject.is_some(),
        "Authenticode signature carries no RFC3161 timestamp"
    );
    let subject = row
        .subject
        .as_deref()
        .ok_or_else(|| anyhow!("Authenticode signature has no signer certificate subject"))?;
    let common_name = certificate_common_name(subject)
        .ok_or_else(|| anyhow!("Signer subject has no CN= component: {subject}"))?;
    ensure!(
        common_name == FLUXER_WINDOWS_SIGNER_COMMON_NAME,
        "Signer CN is '{common_name}', expected '{}' (thumbprint {})",
        FLUXER_WINDOWS_SIGNER_COMMON_NAME,
        row.thumbprint.as_deref().unwrap_or("unknown")
    );
    Ok(())
}

fn assert_third_party_signed(row: &SignatureRow) -> Result<()> {
    ensure!(
        row.status == "Valid",
        "Authenticode status is {} (expected Valid)",
        row.status
    );
    let subject = row
        .subject
        .as_deref()
        .ok_or_else(|| anyhow!("Authenticode signature has no signer certificate subject"))?;
    let common_name = certificate_common_name(subject)
        .ok_or_else(|| anyhow!("Signer subject has no CN= component: {subject}"))?;
    ensure!(
        THIRD_PARTY_PUBLISHER_ALLOWLIST.contains(&common_name),
        "Signer CN '{common_name}' is not an allowlisted third-party publisher"
    );
    ensure!(
        row.ts_subject.is_some(),
        "Authenticode signature has no RFC3161 timestamp"
    );
    Ok(())
}

fn assert_signed_by_known_publisher(row: &SignatureRow) -> Result<()> {
    match assert_fluxer_signed(row) {
        Ok(()) => Ok(()),
        Err(fluxer_error) => assert_third_party_signed(row)
            .map_err(|third_party_error| anyhow!("{fluxer_error}; {third_party_error}")),
    }
}

fn same_windows_path(reported: &str, expected: &Path) -> bool {
    fn normalise(value: &str) -> String {
        let replaced = value.replace('/', "\\");
        let trimmed = replaced.trim_start_matches(r"\\?\");
        trimmed.to_ascii_lowercase()
    }
    normalise(reported) == normalise(expected.to_string_lossy().as_ref())
}

fn find_signtool() -> Result<PathBuf> {
    if let Some(path) = env_string("SIGNTOOL_PATH")
        .map(PathBuf::from)
        .filter(|path| path.exists())
    {
        return Ok(path);
    }
    let roots = [
        PathBuf::from(r"C:\Program Files (x86)\Windows Kits\10\bin"),
        PathBuf::from(r"C:\Program Files\Windows Kits\10\bin"),
    ];
    let host_leaf = signtool_host_arch_dir();
    let mut best: Option<((u8, [u32; 4]), PathBuf)> = None;
    for root in &roots {
        if !root.exists() {
            continue;
        }
        for entry in WalkDir::new(root)
            .into_iter()
            .filter_map(std::result::Result::ok)
            .filter(|entry| entry.file_type().is_file())
        {
            let path = entry.into_path();
            if !path
                .file_name()
                .and_then(OsStr::to_str)
                .is_some_and(|name| name.eq_ignore_ascii_case("signtool.exe"))
            {
                continue;
            }
            let leaf_matches_host = path
                .parent()
                .and_then(Path::file_name)
                .and_then(OsStr::to_str)
                .is_some_and(|leaf| leaf.eq_ignore_ascii_case(host_leaf));
            let rank = (
                u8::from(leaf_matches_host),
                windows_sdk_version_from_path(&path),
            );
            if best.as_ref().is_none_or(|(current, _)| rank > *current) {
                best = Some((rank, path));
            }
        }
    }
    let (rank, path) = best.ok_or_else(|| {
        anyhow!(
            "Could not find signtool.exe under {} or {}. Install the Windows SDK Signing Tools on the runner, or set SIGNTOOL_PATH to an explicit signtool.exe.",
            roots[0].display(),
            roots[1].display()
        )
    })?;
    let (host_arch_match, sdk_version) = rank;
    println!(
        "Using signtool {} (SDK {}.{}.{}.{}, host architecture match: {})",
        path.display(),
        sdk_version[0],
        sdk_version[1],
        sdk_version[2],
        sdk_version[3],
        host_arch_match == 1
    );
    Ok(path)
}

fn signtool_host_arch_dir() -> &'static str {
    match env::consts::ARCH {
        "aarch64" => "arm64",
        "x86" => "x86",
        _ => "x64",
    }
}

fn windows_sdk_version_from_path(path: &Path) -> [u32; 4] {
    let mut best = [0u32; 4];
    for component in path.components() {
        let Some(text) = component.as_os_str().to_str() else {
            continue;
        };
        let parts = text.split('.').collect::<Vec<_>>();
        if parts.len() < 2 || parts.len() > 4 {
            continue;
        }
        let mut version = [0u32; 4];
        let mut parsed = true;
        for (index, part) in parts.iter().enumerate() {
            match part.parse::<u32>() {
                Ok(value) => version[index] = value,
                Err(_) => {
                    parsed = false;
                    break;
                }
            }
        }
        if parsed && version > best {
            best = version;
        }
    }
    best
}

fn verify_pe_signature(signtool: &Path, file: &Path) -> Result<()> {
    let output = capture(CommandSpec::new(signtool).args([
        "verify",
        "/pa",
        "/all",
        "/tw",
        file.to_string_lossy().as_ref(),
    ]))?;
    ensure!(
        output.status == 0,
        "signtool verify /pa /all /tw failed with exit code {}",
        output.status
    );
    Ok(())
}

fn verify_windows_pe_signatures(
    signtool: &Path,
    label: &str,
    root: &Path,
    files: &[PathBuf],
) -> Result<()> {
    ensure!(
        !files.is_empty(),
        "{label}: no Windows PE files found under {}. Refusing to publish an unverified inventory.",
        root.display()
    );
    let root = absolute_path(root)?;
    let files = files
        .iter()
        .map(|file| absolute_path(file))
        .collect::<Result<Vec<_>>>()?;
    let rows = authenticode_report(&files)?;
    let mut failures = Vec::new();
    for file in &files {
        let relative = relative_display(&root, file);
        if let Err(error) = verify_pe_signature(signtool, file) {
            failures.push(format!("{relative}: {error}"));
            continue;
        }
        let Some(row) = rows
            .iter()
            .find(|row| same_windows_path(&row.path, file.as_path()))
        else {
            failures.push(format!(
                "{relative}: Get-AuthenticodeSignature reported no row for this file"
            ));
            continue;
        };
        if let Err(error) = assert_signed_by_known_publisher(row) {
            failures.push(format!("{relative}: {error}"));
        }
    }
    ensure!(
        failures.is_empty(),
        "{label}: {} of {} Windows binaries are not signed by '{}':\n{}",
        failures.len(),
        files.len(),
        FLUXER_WINDOWS_SIGNER_COMMON_NAME,
        failures.join("\n")
    );
    println!(
        "{label}: verified {} Windows binaries signed by '{}'.",
        files.len(),
        FLUXER_WINDOWS_SIGNER_COMMON_NAME
    );
    Ok(())
}

fn verify_windows_unpacked_signatures_step() -> Result<()> {
    let build_channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    let arch = require_env("ARCH")?;
    let config = windows_package_config(&build_channel, &arch);
    let pack_dir = resolve_windows_unpacked_dir(&arch, &config.main_exe)?;
    let files = collect_pe_files(&pack_dir)?;
    assert_expected_windows_pe_inventory(&pack_dir, &files, &arch, &config.main_exe)?;
    ensure!(
        files.iter().any(|file| extension_is(file, "node")),
        "No .node addon was detected as a PE file under {}; the exe,dll,node signing filter would have been a silent no-op.",
        pack_dir.display()
    );
    ensure!(
        files.iter().any(|file| extension_is(file, "dll")),
        "No .dll was detected as a PE file under {}; the exe,dll,node signing filter would have been a silent no-op.",
        pack_dir.display()
    );
    let signtool = find_signtool()?;
    verify_windows_pe_signatures(&signtool, "win-unpacked", &pack_dir, &files)
}

fn short_extraction_root(key: &str) -> PathBuf {
    let drive = env::var("SystemDrive").unwrap_or_else(|_| "C:".to_string());
    let base = PathBuf::from(format!("{}\\fxv", drive.trim_end_matches(['\\', '/'])));
    let digest = hex::encode(Sha256::digest(key.as_bytes()));
    base.join(&digest[..12])
}

fn extract_zip_safely(archive_path: &Path, destination: &Path) -> Result<()> {
    let file = File::open(archive_path)
        .with_context(|| format!("Failed to open {}", archive_path.display()))?;
    let mut archive = zip::ZipArchive::new(file)
        .with_context(|| format!("Failed to read zip {}", archive_path.display()))?;
    fs::create_dir_all(destination)
        .with_context(|| format!("Failed to create {}", destination.display()))?;
    for index in 0..archive.len() {
        let mut entry = archive.by_index(index)?;
        let is_dir = entry.is_dir();
        let relative = entry.enclosed_name().ok_or_else(|| {
            anyhow!(
                "Refusing to extract unsafe archive path '{}' from {}",
                entry.name(),
                archive_path.display()
            )
        })?;
        let target = destination.join(relative);
        if is_dir {
            fs::create_dir_all(&target)
                .with_context(|| format!("Failed to create {}", target.display()))?;
            continue;
        }
        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create {}", parent.display()))?;
        }
        let mut output = File::create(&target)
            .with_context(|| format!("Failed to create {}", target.display()))?;
        io::copy(&mut entry, &mut output)
            .with_context(|| format!("Failed to extract {}", target.display()))?;
    }
    Ok(())
}

fn verify_windows_signed_artifacts_step() -> Result<()> {
    let build_channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    let arch = require_env("ARCH")?;
    let version = require_env("VERSION")?;
    let config = windows_package_config(&build_channel, &arch);

    let nupkg = first_file_matching(&config.output_dir, |name| name.ends_with("-full.nupkg"))
        .ok_or_else(|| {
            anyhow!(
                "No Velopack full nupkg found in {}",
                config.output_dir.display()
            )
        })?;
    let setup_exe = config
        .output_dir
        .join(format!("{}-{version}-win-{arch}.exe", config.pack_title));
    ensure!(
        setup_exe.is_file(),
        "Velopack Setup.exe not found: {}",
        setup_exe.display()
    );
    let portable_zip = PathBuf::from("dist-electron").join(format!(
        "{}-{version}-portable-win-{arch}.zip",
        config.pack_title
    ));
    ensure!(
        portable_zip.is_file(),
        "Portable ZIP not found: {}",
        portable_zip.display()
    );

    let staged_nupkgs = collect_files(&config.output_dir)?
        .into_iter()
        .filter(|path| extension_is(path, "nupkg"))
        .collect::<Vec<_>>();
    let delta_nupkgs = staged_nupkgs
        .iter()
        .filter(|path| {
            path.file_name()
                .and_then(OsStr::to_str)
                .is_some_and(|name| name.ends_with("-delta.nupkg"))
        })
        .cloned()
        .collect::<Vec<_>>();
    let unclassified_nupkgs = staged_nupkgs
        .iter()
        .filter(|path| **path != nupkg && !delta_nupkgs.contains(path))
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>();
    ensure!(
        unclassified_nupkgs.is_empty(),
        "{} stages {} nupkg(s) that are neither the verified full package nor a delta package, so they would be published unverified:\n{}",
        config.output_dir.display(),
        unclassified_nupkgs.len(),
        unclassified_nupkgs.join("\n")
    );

    let unverified_zips = collect_files(&config.output_dir)?
        .into_iter()
        .filter(|path| extension_is(path, "zip") && *path != portable_zip)
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>();
    ensure!(
        unverified_zips.is_empty(),
        "{} stages {} zip(s) that are not the verified portable archive {}, so they would be published unverified:\n{}",
        config.output_dir.display(),
        unverified_zips.len(),
        portable_zip.display(),
        unverified_zips.join("\n")
    );

    let signtool = find_signtool()?;
    let root = short_extraction_root(&format!("{}-{version}-{arch}", config.pack_id));
    remove_dir_if_exists(&root)?;

    let nupkg_root = root.join("n");
    extract_zip_safely(&nupkg, &nupkg_root)?;
    let lib_app = nupkg_root.join("lib").join("app");
    ensure!(
        lib_app.is_dir(),
        "{} contains no lib/app tree.",
        nupkg.display()
    );
    ensure!(
        lib_app.join("Squirrel.exe").is_file(),
        "{} contains no lib/app/Squirrel.exe.",
        nupkg.display()
    );
    let execution_stub = format!("{}_ExecutionStub.exe", config.pack_title);
    ensure!(
        lib_app.join(&execution_stub).is_file(),
        "{} contains no lib/app/{execution_stub}.",
        nupkg.display()
    );
    let nupkg_files = collect_pe_files(&lib_app)?;
    assert_expected_windows_pe_inventory(&lib_app, &nupkg_files, &arch, &config.main_exe)?;
    verify_windows_pe_signatures(&signtool, "nupkg lib/app", &lib_app, &nupkg_files)?;

    for (index, delta_nupkg) in delta_nupkgs.iter().enumerate() {
        let delta_root = root.join(format!("d{index}"));
        extract_zip_safely(delta_nupkg, &delta_root)?;
        let delta_files = collect_pe_files(&delta_root)?;
        let label = format!("delta nupkg {}", file_name_string(delta_nupkg)?);
        if delta_files.is_empty() {
            println!(
                "{label}: contains no whole PE entries, only Velopack diffs; nothing to verify."
            );
            continue;
        }
        verify_windows_pe_signatures(&signtool, &label, &delta_root, &delta_files)?;
    }

    let portable_root = root.join("p");
    extract_zip_safely(&portable_zip, &portable_root)?;
    let portable_files = collect_pe_files(&portable_root)?;
    assert_expected_windows_pe_inventory(&portable_root, &portable_files, &arch, &config.main_exe)?;
    verify_windows_pe_signatures(&signtool, "portable zip", &portable_root, &portable_files)?;

    let staged_installers = collect_pe_files(&config.output_dir)?;
    ensure!(
        staged_installers.contains(&setup_exe),
        "Velopack output directory does not contain the renamed Setup executable {}",
        setup_exe.display()
    );
    verify_windows_pe_signatures(&signtool, "setup", &config.output_dir, &staged_installers)?;

    remove_dir_if_exists(&root)
}

fn prepare_artifacts_windows_step() -> Result<()> {
    let arch = require_env("ARCH")?;
    let staging = Path::new("upload_staging");
    remove_dir_if_exists(staging)?;
    fs::create_dir_all(staging).context("Failed to create upload_staging")?;

    let dist = desktop_dist_dir();
    let release_dir = dist.join(format!("velopack-windows-{arch}"));
    ensure!(
        release_dir.exists(),
        "Velopack release directory not found: {}",
        release_dir.display()
    );

    copy_matching_files(&release_dir, staging, |name| {
        name.ends_with(".exe")
            || name.ends_with(".zip")
            || name.ends_with(".nupkg")
            || name.starts_with("RELEASES")
            || (name.starts_with("releases") && name.ends_with(".json"))
            || (name.starts_with("assets") && name.ends_with(".json"))
    })?;
    let portable_suffix = format!("-portable-win-{arch}.zip");
    copy_matching_files(&dist, staging, |name| name.ends_with(&portable_suffix))?;

    ensure!(
        any_file_matching(staging, |name| name.ends_with(".exe"))?,
        "No installer .exe staged."
    );
    ensure!(
        staging.join("RELEASES").exists(),
        "Legacy Squirrel RELEASES file was not staged."
    );
    ensure!(
        staging.join("releases.win.json").exists(),
        "Velopack releases.win.json was not staged."
    );
    ensure!(
        any_file_matching(staging, |name| name.ends_with("-full.nupkg"))?,
        "No Velopack full nupkg staged."
    );
    print_directory(staging)
}

fn prepare_artifacts_unix_step() -> Result<()> {
    let staging = Path::new("upload_staging");
    remove_dir_if_exists(staging)?;
    fs::create_dir_all(staging).context("Failed to create upload_staging")?;
    let dist = desktop_dist_dir();
    copy_matching_files(&dist, staging, is_unix_upload_artifact)?;
    print_directory(staging)
}

fn is_unix_upload_artifact(name: &str) -> bool {
    name.ends_with(".dmg")
        || name.ends_with(".zip")
        || name.ends_with(".zip.blockmap")
        || name.ends_with(".yml")
        || name.ends_with(".AppImage")
        || name.ends_with(".deb")
        || name.ends_with(".rpm")
        || name.ends_with(".tar.gz")
}

fn normalise_updater_yaml_step() -> Result<()> {
    if env::var("PLATFORM").unwrap_or_default() == "macos"
        && env::var("ARCH").unwrap_or_default() == "arm64"
    {
        let source = Path::new("upload_staging/latest-mac.yml");
        let target = Path::new("upload_staging/latest-mac-arm64.yml");
        if source.exists() && !target.exists() {
            fs::rename(source, target).with_context(|| {
                format!(
                    "Failed to rename {} to {}",
                    source.display(),
                    target.display()
                )
            })?;
        }
    }
    Ok(())
}

#[derive(Debug, Clone, Copy)]
enum ArtifactChecksumKind {
    Extension(&'static str),
    Suffix(&'static str),
}

fn generate_checksums_step(kinds: &[ArtifactChecksumKind]) -> Result<()> {
    let staging = Path::new("upload_staging");
    let mut generated = Vec::new();
    for entry in
        fs::read_dir(staging).with_context(|| format!("Failed to read {}", staging.display()))?
    {
        let path = entry?.path();
        if !path.is_file() {
            continue;
        }
        let name = file_name_string(&path)?;
        if !kinds.iter().any(|kind| checksum_kind_matches(*kind, &name)) {
            continue;
        }
        let hash = sha256_file(&path)?;
        let output = path.with_file_name(format!("{name}.sha256"));
        fs::write(&output, &hash)
            .with_context(|| format!("Failed to write {}", output.display()))?;
        println!("Generated checksum for {name}");
        generated.push(output);
    }
    if generated.is_empty() {
        println!("No checksum files generated");
    } else {
        for path in generated {
            println!("{}", path.display());
        }
    }
    Ok(())
}

fn checksum_kind_matches(kind: ArtifactChecksumKind, name: &str) -> bool {
    match kind {
        ArtifactChecksumKind::Extension(extension) => name
            .rsplit_once('.')
            .is_some_and(|(_, ext)| ext == extension),
        ArtifactChecksumKind::Suffix(suffix) => name.ends_with(suffix),
    }
}

async fn upload_handoff_step(signed_windows_artifacts: bool) -> Result<()> {
    let client = s3_client(None).await?;
    let bucket = require_env("S3_BUCKET")?;
    let prefix = require_env("DESKTOP_HANDOFF_PREFIX")?;
    let build_channel = require_env("BUILD_CHANNEL")?;
    let platform = require_any_env(&["DESKTOP_PLATFORM", "PLATFORM"])?;
    let arch = require_any_env(&["DESKTOP_ARCH", "ARCH"])?;
    let desktop_variant = desktop_variant_from_env()?;
    ensure_platform_supports_desktop_variant(&platform, &desktop_variant)?;
    let staging = Path::new("upload_staging");
    ensure!(staging.exists(), "upload_staging is missing.");
    let artifact_count = count_files(staging)?;
    ensure!(artifact_count > 0, "upload_staging is empty.");

    let artifact_name = handoff_artifact_name(
        &build_channel,
        &platform,
        &arch,
        &desktop_variant,
        signed_windows_artifacts,
    );
    let artifact_prefix = join_s3_key(&prefix, &artifact_name);
    println!("Uploading {artifact_count} desktop artifact file(s) to {artifact_prefix}");
    upload_directory_to_s3(&client, &bucket, &artifact_prefix, staging, |_| true).await?;
    Ok(())
}

fn handoff_artifact_name(
    build_channel: &str,
    platform: &str,
    arch: &str,
    desktop_variant: &str,
    signed_windows_artifacts: bool,
) -> String {
    let variant_suffix = desktop_variant_path_segment(desktop_variant)
        .map(|variant| format!("-{variant}"))
        .unwrap_or_default();
    let signed_suffix = if signed_windows_artifacts && platform == "windows" {
        "-signed"
    } else {
        ""
    };
    format!("fluxer-desktop-{build_channel}-{platform}-{arch}{variant_suffix}{signed_suffix}")
}

async fn download_handoff_step() -> Result<()> {
    let client = s3_client(None).await?;
    let bucket = require_env("S3_BUCKET")?;
    let prefix = require_env("DESKTOP_HANDOFF_PREFIX")?;
    let artifacts = Path::new("artifacts");
    remove_dir_if_exists(artifacts)?;
    fs::create_dir_all(artifacts)?;
    println!("Downloading desktop handoff artifacts from {prefix}");
    download_s3_prefix(&client, &bucket, &prefix, artifacts).await?;
    ensure!(
        count_files_min_depth(artifacts, 2)? > 0,
        "No desktop handoff files were downloaded."
    );
    println!("Downloaded handoff artifact tree:");
    print_tree(artifacts, 3)
}

async fn cleanup_handoff_step() -> Result<()> {
    println!("S3 handoff cleanup skipped: CI S3 writes are append-only and never delete objects.");
    Ok(())
}

fn build_payload_step() -> Result<()> {
    let s3_prefix = require_env("S3_DESKTOP_PREFIX")?;
    let payload_root = Path::new("s3_payload").join(&s3_prefix);
    remove_dir_if_exists(&payload_root)?;
    fs::create_dir_all(&payload_root)?;

    let channel = require_env("CHANNEL")?;
    let version = require_env("VERSION")?;
    let pub_date = require_env("PUB_DATE")?;
    let artifacts = Path::new("artifacts");
    for (dir, identity) in payload_artifact_dirs(artifacts, &channel)? {
        let platform = match identity.platform.as_str() {
            "windows" => "win32",
            "macos" => "darwin",
            "linux" => "linux",
            other => {
                println!("Unknown platform: {other}");
                continue;
            }
        };
        let mut dest = payload_root
            .join(&channel)
            .join(platform)
            .join(&identity.arch);
        if let Some(segment) = desktop_variant_path_segment(&identity.desktop_variant) {
            dest = dest.join(segment);
        }
        fs::create_dir_all(&dest)?;
        copy_dir_contents(&dir, &dest)?;
        let manifest = build_desktop_manifest(
            &dest,
            &PayloadManifestInput {
                channel: channel.clone(),
                platform: platform.to_string(),
                arch: identity.arch.clone(),
                desktop_variant: identity.desktop_variant.clone(),
                version: version.clone(),
                pub_date: pub_date.clone(),
            },
        )?;
        if platform == "darwin" {
            write_macos_releases(&dest, &s3_prefix, &channel, &manifest)?;
        }
        write_json_pretty(&dest.join("manifest.json"), &manifest)?;
    }

    println!("Payload tree:");
    print_tree(&payload_root, 6)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ArtifactIdentity {
    platform: String,
    arch: String,
    desktop_variant: String,
    signed: bool,
}

fn parse_artifact_dir_name(base: &str, channel: &str) -> Option<ArtifactIdentity> {
    let prefix = format!("fluxer-desktop-{channel}-");
    let rest = base.strip_prefix(&prefix)?;
    let (rest, signed) = rest
        .strip_suffix("-signed")
        .map(|value| (value, true))
        .unwrap_or((rest, false));
    let (rest, desktop_variant) = rest
        .strip_suffix(&format!("-{WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT}"))
        .map(|value| (value, WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT))
        .unwrap_or((rest, DEFAULT_DESKTOP_VARIANT));
    let (platform, arch) = rest.rsplit_once('-')?;
    Some(ArtifactIdentity {
        platform: platform.to_string(),
        arch: arch.to_string(),
        desktop_variant: desktop_variant.to_string(),
        signed,
    })
}

fn payload_artifact_dirs(
    artifacts: &Path,
    channel: &str,
) -> Result<Vec<(PathBuf, ArtifactIdentity)>> {
    let mut selected = BTreeMap::<(String, String, String), (PathBuf, ArtifactIdentity)>::new();
    if !artifacts.exists() {
        return Ok(Vec::new());
    }

    let mut dirs = fs::read_dir(artifacts)
        .with_context(|| format!("Failed to read {}", artifacts.display()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::result::Result<Vec<_>, _>>()?;
    dirs.sort();

    for dir in dirs {
        if !dir.is_dir() {
            continue;
        }
        let base = file_name_string(&dir)?;
        let Some(identity) = parse_artifact_dir_name(&base, channel) else {
            println!("Skipping unrecognised artifact dir: {base}");
            continue;
        };

        let key = (
            identity.platform.clone(),
            identity.arch.clone(),
            identity.desktop_variant.clone(),
        );
        match selected.get(&key) {
            Some((_, current)) if current.signed && !identity.signed => {}
            Some((_, current)) if !current.signed && identity.signed => {
                selected.insert(key, (dir, identity));
            }
            Some(_) => {}
            None => {
                selected.insert(key, (dir, identity));
            }
        }
    }

    Ok(selected.into_values().collect())
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PayloadManifestInput {
    channel: String,
    platform: String,
    arch: String,
    desktop_variant: String,
    version: String,
    pub_date: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopManifest {
    channel: String,
    platform: String,
    arch: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    variant: Option<String>,
    version: String,
    pub_date: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    minimum_system_version: Option<String>,
    files: BTreeMap<String, DesktopManifestFile>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(untagged)]
enum DesktopManifestFile {
    Name(String),
    Detail { filename: String, sha256: String },
}

impl DesktopManifestFile {
    fn filename(&self) -> &str {
        match self {
            Self::Name(filename) => filename,
            Self::Detail { filename, .. } => filename,
        }
    }
}

fn build_desktop_manifest(dest: &Path, input: &PayloadManifestInput) -> Result<DesktopManifest> {
    let candidates = manifest_candidates(dest, &input.platform, &input.arch)?;
    let files = candidates
        .into_iter()
        .map(|(kind, path)| manifest_file_entry(&kind, &path).map(|entry| (kind, entry)))
        .collect::<Result<BTreeMap<_, _>>>()?;
    Ok(DesktopManifest {
        channel: input.channel.clone(),
        platform: input.platform.clone(),
        arch: input.arch.clone(),
        variant: desktop_variant_path_segment(&input.desktop_variant).map(ToOwned::to_owned),
        version: input.version.clone(),
        pub_date: input.pub_date.clone(),
        minimum_system_version: if input.platform == "darwin" {
            Some("12.0".to_string())
        } else {
            None
        },
        files,
    })
}

fn manifest_candidates(dest: &Path, platform: &str, arch: &str) -> Result<Vec<(String, PathBuf)>> {
    let mut files = collect_files(dest)?;
    files.sort();
    let mut candidates = Vec::new();
    match platform {
        "win32" => {
            if let Some(path) = first_matching_path(&files, |name| {
                name.ends_with(".exe") && name.to_ascii_lowercase().contains("setup")
            })
            .or_else(|| first_matching_path(&files, |name| name.ends_with(".exe")))
            {
                candidates.push(("setup".to_string(), path));
            }
            if let Some(path) = first_matching_path(&files, |name| {
                name.to_ascii_lowercase().contains("portable") && name.ends_with(".zip")
            }) {
                candidates.push(("portable".to_string(), path));
            }
        }
        "darwin" => {
            if let Some(path) = first_matching_path(&files, |name| {
                name.ends_with(&format!("-{arch}.dmg")) || name.ends_with(".dmg")
            }) {
                candidates.push(("dmg".to_string(), path));
            }
            if let Some(path) = first_matching_path(&files, |name| {
                name.ends_with(&format!("-{arch}.zip")) || name.ends_with(".zip")
            }) {
                candidates.push(("zip".to_string(), path));
            }
        }
        "linux" => {
            for (kind, suffix) in [
                ("appimage", ".AppImage"),
                ("deb", ".deb"),
                ("rpm", ".rpm"),
                ("tar_gz", ".tar.gz"),
            ] {
                if let Some(path) = first_matching_path(&files, |name| name.ends_with(suffix)) {
                    candidates.push((kind.to_string(), path));
                }
            }
        }
        _ => {}
    }
    Ok(candidates)
}

fn manifest_file_entry(kind: &str, file: &Path) -> Result<DesktopManifestFile> {
    let filename = file_name_string(file)?;
    let checksum_path = file.with_file_name(format!("{filename}.sha256"));
    if checksum_path.exists() {
        let sha256 = fs::read_to_string(&checksum_path)
            .with_context(|| format!("Failed to read {}", checksum_path.display()))?
            .split_whitespace()
            .next()
            .unwrap_or_default()
            .to_string();
        ensure!(
            !sha256.is_empty(),
            "{} checksum file is empty",
            checksum_path.display()
        );
        Ok(DesktopManifestFile::Detail { filename, sha256 })
    } else {
        println!("No checksum file found for {kind}: {}", file.display());
        Ok(DesktopManifestFile::Name(filename))
    }
}

fn write_macos_releases(
    dest: &Path,
    s3_prefix: &str,
    channel: &str,
    manifest: &DesktopManifest,
) -> Result<()> {
    let Some(zip) = manifest.files.get("zip") else {
        println!(
            "No .zip found for macOS {} in {} (auto-update requires zip artifacts).",
            manifest.arch,
            dest.display()
        );
        return Ok(());
    };
    let url = format!(
        "{PUBLIC_DL_BASE}/{s3_prefix}/{channel}/{}/{}/{}/{}",
        manifest.platform,
        manifest.arch,
        zip.filename(),
        ""
    );
    let url = url.trim_end_matches('/').to_string();
    let releases = json!({
        "currentRelease": manifest.version,
        "releases": [{
            "version": manifest.version,
            "updateTo": {
                "version": manifest.version,
                "pub_date": manifest.pub_date,
                "notes": "",
                "name": manifest.version,
                "url": url,
            },
        }],
    });
    write_json_pretty(&dest.join("RELEASES.json"), &releases)?;
    write_json_pretty(&dest.join("releases.json"), &releases)?;
    Ok(())
}

async fn upload_payload_step() -> Result<()> {
    let client = s3_client(None).await?;
    let s3_prefix = require_env("S3_DESKTOP_PREFIX")?;
    let bucket = require_env("S3_BUCKET")?;
    let payload_root = Path::new("s3_payload").join(&s3_prefix);
    let overwrite_binaries = should_overwrite_payload(&s3_prefix, env_bool("TEST_BUILD"));

    println!("Uploading desktop binaries and checksums first (prefix: {s3_prefix})...");
    upload_payload_directory(
        &client,
        &bucket,
        &s3_prefix,
        &payload_root,
        overwrite_binaries,
        |relative| !is_payload_metadata_key(relative),
    )
    .await?;
    println!(
        "Uploading manifests and updater metadata last, overwriting the previous release feed..."
    );
    upload_payload_directory(
        &client,
        &bucket,
        &s3_prefix,
        &payload_root,
        true,
        is_payload_metadata_key,
    )
    .await
}

async fn upload_payload_directory<F>(
    client: &S3Client,
    bucket: &str,
    s3_prefix: &str,
    payload_root: &Path,
    overwrite_existing: bool,
    include: F,
) -> Result<()>
where
    F: Fn(&Path) -> bool,
{
    if overwrite_existing {
        upload_directory_to_s3_overwrite(client, bucket, s3_prefix, payload_root, include).await
    } else {
        upload_directory_to_s3(client, bucket, s3_prefix, payload_root, include).await
    }
}

fn should_overwrite_payload(s3_prefix: &str, test_build: bool) -> bool {
    test_build && s3_prefix == "desktop-test"
}

fn is_payload_metadata_key(relative: &Path) -> bool {
    let name = relative
        .file_name()
        .and_then(OsStr::to_str)
        .unwrap_or_default();
    name == "manifest.json"
        || name.ends_with(".yml")
        || name.starts_with("RELEASES")
        || (name.starts_with("releases") && name.ends_with(".json"))
        || (name.starts_with("assets") && name.ends_with(".json"))
}

fn build_summary_step() -> Result<()> {
    let summary = require_env("GITHUB_STEP_SUMMARY")?;
    let test_build = env_bool("TEST_BUILD");
    let display_channel = env::var("DISPLAY_CHANNEL").unwrap_or_default();
    let version = require_env("VERSION")?;
    let s3_prefix = require_env("S3_DESKTOP_PREFIX")?;
    let channel = require_env("CHANNEL")?;
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&summary)
        .with_context(|| format!("Failed to open {summary}"))?;
    if test_build {
        writeln!(
            file,
            "## Desktop {} Test Upload Complete",
            title_case(&display_channel)
        )?;
        writeln!(
            file,
            "\n_This is a **test build**. Artifacts were stashed under `{s3_prefix}/` so the API will not promote them as a release._"
        )?;
    } else {
        writeln!(
            file,
            "## Desktop {} Upload Complete",
            title_case(&display_channel)
        )?;
    }
    writeln!(
        file,
        "\n**Version:** {version}\n\n**S3 prefix:** {s3_prefix}/{channel}/\n\n**Redirect endpoint shape:** /dl/{s3_prefix}/{channel}/{{plat}}/{{arch}}[/{{variant}}]/{{format}}"
    )?;
    Ok(())
}

fn find_dist_file<F>(dist: &Path, predicate: F) -> Option<PathBuf>
where
    F: Fn(&str) -> bool,
{
    fs::read_dir(dist)
        .ok()?
        .filter_map(std::result::Result::ok)
        .find_map(|entry| {
            let path = entry.path();
            let name = path.file_name()?.to_string_lossy();
            (path.is_file() && predicate(&name)).then_some(path)
        })
}

fn find_first<F>(root: &Path, predicate: F) -> Option<PathBuf>
where
    F: Fn(&Path) -> bool,
{
    WalkDir::new(root)
        .into_iter()
        .filter_map(std::result::Result::ok)
        .map(|entry| entry.into_path())
        .find(|path| predicate(path))
}

fn first_file_matching<F>(dir: &Path, predicate: F) -> Option<PathBuf>
where
    F: Fn(&str) -> bool,
{
    fs::read_dir(dir)
        .ok()?
        .filter_map(std::result::Result::ok)
        .find_map(|entry| {
            let path = entry.path();
            let name = path.file_name()?.to_string_lossy();
            (path.is_file() && predicate(&name)).then_some(path)
        })
}

fn any_file_matching<F>(dir: &Path, predicate: F) -> Result<bool>
where
    F: Fn(&str) -> bool,
{
    Ok(fs::read_dir(dir)
        .with_context(|| format!("Failed to read {}", dir.display()))?
        .filter_map(std::result::Result::ok)
        .any(|entry| {
            let path = entry.path();
            path.is_file()
                && path
                    .file_name()
                    .and_then(OsStr::to_str)
                    .is_some_and(&predicate)
        }))
}

fn copy_matching_files<F>(source: &Path, dest: &Path, predicate: F) -> Result<()>
where
    F: Fn(&str) -> bool,
{
    if !source.exists() {
        return Ok(());
    }
    for entry in
        fs::read_dir(source).with_context(|| format!("Failed to read {}", source.display()))?
    {
        let path = entry?.path();
        if !path.is_file() {
            continue;
        }
        let name = file_name_string(&path)?;
        if predicate(&name) {
            fs::copy(&path, dest.join(&name))
                .with_context(|| format!("Failed to copy {}", path.display()))?;
        }
    }
    Ok(())
}

fn create_zip_from_dir(source: &Path, output: &Path) -> Result<()> {
    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    let file =
        File::create(output).with_context(|| format!("Failed to create {}", output.display()))?;
    let mut zip = zip::ZipWriter::new(file);
    let options = SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);
    for path in collect_files(source)? {
        let relative = path.strip_prefix(source)?;
        let name = path_to_s3_key(relative);
        zip.start_file(name, options)?;
        let mut file = File::open(&path)?;
        io::copy(&mut file, &mut zip)?;
    }
    zip.finish()?;
    Ok(())
}

fn first_matching_path<F>(paths: &[PathBuf], predicate: F) -> Option<PathBuf>
where
    F: Fn(&str) -> bool,
{
    paths.iter().find_map(|path| {
        let name = path.file_name()?.to_string_lossy();
        predicate(&name).then(|| path.clone())
    })
}

fn extension_is(path: &Path, extension: &str) -> bool {
    path.extension().and_then(OsStr::to_str) == Some(extension)
}

fn file_name_string(path: &Path) -> Result<String> {
    path.file_name()
        .and_then(OsStr::to_str)
        .map(ToOwned::to_owned)
        .ok_or_else(|| anyhow!("Path has no UTF-8 file name: {}", path.display()))
}

fn sha256_file(path: &Path) -> Result<String> {
    let mut file =
        File::open(path).with_context(|| format!("Failed to open {}", path.display()))?;
    let mut hasher = Sha256::new();
    let mut buffer = [0u8; 64 * 1024];
    loop {
        let read = file
            .read(&mut buffer)
            .with_context(|| format!("Failed to read {}", path.display()))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    Ok(hex::encode(hasher.finalize()))
}

fn print_directory(dir: &Path) -> Result<()> {
    if !dir.exists() {
        println!("{} does not exist", dir.display());
        return Ok(());
    }
    for entry in fs::read_dir(dir).with_context(|| format!("Failed to read {}", dir.display()))? {
        let path = entry?.path();
        let metadata = fs::metadata(&path)?;
        println!("{:>12} {}", metadata.len(), path.display());
    }
    Ok(())
}

fn print_tree(root: &Path, max_depth: usize) -> Result<()> {
    if !root.exists() {
        return Ok(());
    }
    for entry in WalkDir::new(root)
        .max_depth(max_depth)
        .into_iter()
        .collect::<std::result::Result<Vec<_>, _>>()?
        .into_iter()
        .filter(|entry| entry.file_type().is_file())
    {
        println!("{}", entry.path().display());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{directory_upload_plan, parse_version_instant, s3_directory_prefix};
    use chrono::{DateTime, TimeZone, Utc};

    fn dt(year: i32, month: u32, day: u32, hour: u32, minute: u32, second: u32) -> DateTime<Utc> {
        Utc.with_ymd_and_hms(year, month, day, hour, minute, second)
            .single()
            .unwrap()
    }

    fn matrix_args() -> BuildDesktopArgs {
        BuildDesktopArgs {
            step: DesktopStep::SetMatrix,
            channel: None,
            test_build: None,
            skip_targets: None,
            skip_windows: Some("false".to_string()),
            skip_windows_x64: Some("false".to_string()),
            skip_windows_arm64: Some("false".to_string()),
            skip_macos: Some("false".to_string()),
            skip_macos_x64: Some("false".to_string()),
            skip_macos_arm64: Some("false".to_string()),
            skip_linux: Some("false".to_string()),
            skip_linux_x64: Some("false".to_string()),
            skip_linux_arm64: Some("false".to_string()),
        }
    }

    fn write_file(path: &Path, contents: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, contents).unwrap();
    }

    #[test]
    fn only_desktop_test_payloads_overwrite_existing_s3_objects() {
        assert!(should_overwrite_payload("desktop-test", true));
        assert!(!should_overwrite_payload("desktop", true));
        assert!(!should_overwrite_payload("desktop-test", false));
    }

    #[test]
    fn resolves_explicit_calver_with_precedence() {
        let calver_env = CalverEnv {
            build_version: Some("2026.520.1".to_string()),
            fluxer_build_version: Some("2026.521.2".to_string()),
            fluxer_build_date: Some("2026-05-22T03:04:05Z".to_string()),
        };
        assert_eq!(
            resolve_calver(&calver_env, dt(2026, 5, 1, 0, 0, 0)).unwrap(),
            "2026.520.1"
        );
    }

    #[test]
    fn resolves_generated_calver_from_date_override() {
        let calver_env = CalverEnv {
            fluxer_build_date: Some("2026-05-20T01:02:03Z".to_string()),
            ..CalverEnv::default()
        };
        assert_eq!(
            resolve_calver(&calver_env, dt(2026, 1, 1, 0, 0, 0)).unwrap(),
            "2026.520.10203"
        );
    }

    #[test]
    fn rejects_invalid_explicit_time() {
        let error = parse_version_instant("2026.520.246000").unwrap_err();
        assert_eq!(
            error.to_string(),
            "Invalid build version date/time: 2026.520.246000"
        );
    }

    #[test]
    fn matrix_skip_flags_filter_individual_arches() {
        let mut args = matrix_args();
        args.skip_windows_x64 = Some("true".to_string());
        args.skip_macos = Some("true".to_string());

        let selected = selected_platforms(&args)
            .unwrap()
            .into_iter()
            .map(platform_json)
            .collect::<Vec<_>>();

        assert_eq!(
            selected,
            vec![
                "{\"platform\":\"windows\",\"arch\":\"arm64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-windows-2025\",\"electron_arch\":\"arm64\"}",
                "{\"platform\":\"linux\",\"arch\":\"x64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-ubuntu-2404\",\"electron_arch\":\"x64\"}",
                "{\"platform\":\"linux\",\"arch\":\"arm64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-ubuntu-2404-arm\",\"electron_arch\":\"arm64\"}",
            ]
        );
    }

    #[test]
    fn matrix_selects_one_row_per_platform_arch_by_default() {
        let selected = selected_platforms(&matrix_args()).unwrap();

        assert_eq!(selected.len(), 6);
        assert_eq!(
            selected
                .iter()
                .filter(|platform| platform.platform == "windows")
                .count(),
            2
        );
        assert!(
            selected
                .iter()
                .all(|platform| platform.desktop_variant == DEFAULT_DESKTOP_VARIANT)
        );
    }

    #[test]
    fn matrix_skip_targets_filter_platforms_and_arches() {
        let mut args = matrix_args();
        args.skip_targets = Some("windows-x64, macos".to_string());

        let selected = selected_platforms(&args)
            .unwrap()
            .into_iter()
            .map(platform_json)
            .collect::<Vec<_>>();

        assert_eq!(
            selected,
            vec![
                "{\"platform\":\"windows\",\"arch\":\"arm64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-windows-2025\",\"electron_arch\":\"arm64\"}",
                "{\"platform\":\"linux\",\"arch\":\"x64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-ubuntu-2404\",\"electron_arch\":\"x64\"}",
                "{\"platform\":\"linux\",\"arch\":\"arm64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-ubuntu-2404-arm\",\"electron_arch\":\"arm64\"}",
            ]
        );
    }

    #[test]
    fn matrix_skip_targets_drop_every_windows_row() {
        let mut args = matrix_args();
        args.skip_targets = Some("windows".to_string());

        let selected = selected_platforms(&args).unwrap();

        assert!(
            selected
                .iter()
                .all(|platform| platform.platform != "windows")
        );
        assert_eq!(selected.len(), 4);
    }

    #[test]
    fn matrix_skip_targets_reject_unknown_values() {
        let mut args = matrix_args();
        args.skip_targets = Some("windows-riscv".to_string());

        let error = selected_platforms(&args).unwrap_err();

        assert!(error.to_string().contains("Unknown desktop skip target"));
    }

    #[test]
    fn matrix_skip_targets_reject_retired_windows_game_capture_variant() {
        for target in [
            WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT,
            "windows-game-capture-x64",
            "windows-game-capture-arm64",
        ] {
            let mut args = matrix_args();
            args.skip_targets = Some(target.to_string());

            let error = selected_platforms(&args).unwrap_err();

            assert!(
                error.to_string().contains("Unknown desktop skip target"),
                "{target} should no longer be a recognised skip target"
            );
        }
    }

    #[test]
    fn s3_key_join_and_path_conversion_are_platform_neutral() {
        assert_eq!(
            join_s3_key("/desktop/", "/canary/linux/"),
            "desktop/canary/linux"
        );
        assert_eq!(
            s3_directory_prefix("/_handoff/desktop/build/"),
            "_handoff/desktop/build/"
        );
        assert_eq!(join_s3_key("", "manifest.json"), "manifest.json");
        assert_eq!(
            path_to_s3_key(Path::new("canary").join("linux").join("x64").as_path()),
            "canary/linux/x64"
        );
    }

    #[test]
    fn upload_plan_splits_payload_metadata_without_s3() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();
        write_file(&root.join("canary/linux/x64/Fluxer.AppImage"), "app");
        write_file(&root.join("canary/linux/x64/manifest.json"), "{}");
        write_file(&root.join("canary/darwin/x64/releases.json"), "{}");

        let binaries = directory_upload_plan("desktop", root, |relative| {
            !is_payload_metadata_key(relative)
        })
        .unwrap()
        .into_iter()
        .map(|item| item.key)
        .collect::<Vec<_>>();
        let metadata = directory_upload_plan("desktop", root, is_payload_metadata_key)
            .unwrap()
            .into_iter()
            .map(|item| item.key)
            .collect::<Vec<_>>();

        assert_eq!(binaries, vec!["desktop/canary/linux/x64/Fluxer.AppImage"]);
        assert_eq!(
            metadata,
            vec![
                "desktop/canary/darwin/x64/releases.json",
                "desktop/canary/linux/x64/manifest.json",
            ]
        );
    }

    #[test]
    fn parses_handoff_artifact_dir_names() {
        assert_eq!(
            parse_artifact_dir_name("fluxer-desktop-canary-windows-arm64", "canary").unwrap(),
            ArtifactIdentity {
                platform: "windows".to_string(),
                arch: "arm64".to_string(),
                desktop_variant: DEFAULT_DESKTOP_VARIANT.to_string(),
                signed: false,
            }
        );
        assert_eq!(
            parse_artifact_dir_name(
                "fluxer-desktop-canary-windows-x64-windows-game-capture-signed",
                "canary",
            )
            .unwrap(),
            ArtifactIdentity {
                platform: "windows".to_string(),
                arch: "x64".to_string(),
                desktop_variant: WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT.to_string(),
                signed: true,
            }
        );
        assert!(parse_artifact_dir_name("fluxer-desktop-stable-linux-x64", "canary").is_none());
        assert_eq!(
            parse_artifact_dir_name("fluxer-desktop-canary-windows-x64-signed", "canary").unwrap(),
            ArtifactIdentity {
                platform: "windows".to_string(),
                arch: "x64".to_string(),
                desktop_variant: DEFAULT_DESKTOP_VARIANT.to_string(),
                signed: true,
            }
        );
    }

    #[test]
    fn handoff_artifact_name_only_marks_signed_windows_uploads() {
        assert_eq!(
            handoff_artifact_name("canary", "windows", "x64", DEFAULT_DESKTOP_VARIANT, true),
            "fluxer-desktop-canary-windows-x64-signed"
        );
        assert_eq!(
            handoff_artifact_name("canary", "linux", "x64", DEFAULT_DESKTOP_VARIANT, true),
            "fluxer-desktop-canary-linux-x64"
        );
        assert_eq!(
            handoff_artifact_name(
                "stable",
                "windows",
                "arm64",
                WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT,
                false,
            ),
            "fluxer-desktop-stable-windows-arm64-windows-game-capture"
        );
        assert_eq!(
            handoff_artifact_name("stable", "windows", "arm64", DEFAULT_DESKTOP_VARIANT, false),
            "fluxer-desktop-stable-windows-arm64"
        );
    }

    #[test]
    fn build_channel_content_matches_expected_typescript() {
        assert_eq!(
            build_channel_content("canary"),
            "// SPDX-License-Identifier: AGPL-3.0-or-later\n\n\
export type BuildChannel = 'stable' | 'canary';\n\n\
export const BUILD_CHANNEL = 'canary' as BuildChannel;\n\
export const IS_CANARY = BUILD_CHANNEL === 'canary';\n\
export const CHANNEL_DISPLAY_NAME = BUILD_CHANNEL;\n"
        );
    }

    #[test]
    fn write_build_channel_file_rejects_invalid_channels() {
        let temp = tempfile::tempdir().unwrap();
        assert_eq!(
            write_build_channel_file(temp.path(), "nightly")
                .unwrap_err()
                .to_string(),
            "Invalid BUILD_CHANNEL: nightly. Must be 'stable' or 'canary'."
        );
    }

    #[test]
    fn write_build_channel_file_creates_and_updates_file() {
        let temp = tempfile::tempdir().unwrap();
        write_build_channel_file(temp.path(), "stable").unwrap();
        let path = temp.path().join("src/common/BuildChannel.ts");
        assert_eq!(
            fs::read_to_string(&path).unwrap(),
            build_channel_content("stable")
        );

        write_build_channel_file(temp.path(), "canary").unwrap();
        assert_eq!(
            fs::read_to_string(&path).unwrap(),
            build_channel_content("canary")
        );
    }

    #[test]
    fn payload_artifact_dirs_prefer_signed_windows_artifacts() {
        let temp = tempfile::tempdir().unwrap();
        let artifacts = temp.path();
        fs::create_dir_all(artifacts.join("fluxer-desktop-canary-windows-x64")).unwrap();
        fs::create_dir_all(artifacts.join("fluxer-desktop-canary-windows-x64-signed")).unwrap();
        fs::create_dir_all(
            artifacts.join("fluxer-desktop-canary-windows-x64-windows-game-capture"),
        )
        .unwrap();
        fs::create_dir_all(
            artifacts.join("fluxer-desktop-canary-windows-x64-windows-game-capture-signed"),
        )
        .unwrap();
        fs::create_dir_all(artifacts.join("fluxer-desktop-canary-linux-x64")).unwrap();
        fs::create_dir_all(artifacts.join("unrelated")).unwrap();

        let selected = payload_artifact_dirs(artifacts, "canary")
            .unwrap()
            .into_iter()
            .map(|(path, identity)| {
                (
                    path.file_name().unwrap().to_string_lossy().to_string(),
                    identity,
                )
            })
            .collect::<Vec<_>>();

        assert_eq!(
            selected,
            vec![
                (
                    "fluxer-desktop-canary-linux-x64".to_string(),
                    ArtifactIdentity {
                        platform: "linux".to_string(),
                        arch: "x64".to_string(),
                        desktop_variant: DEFAULT_DESKTOP_VARIANT.to_string(),
                        signed: false,
                    },
                ),
                (
                    "fluxer-desktop-canary-windows-x64-signed".to_string(),
                    ArtifactIdentity {
                        platform: "windows".to_string(),
                        arch: "x64".to_string(),
                        desktop_variant: DEFAULT_DESKTOP_VARIANT.to_string(),
                        signed: true,
                    },
                ),
                (
                    "fluxer-desktop-canary-windows-x64-windows-game-capture-signed".to_string(),
                    ArtifactIdentity {
                        platform: "windows".to_string(),
                        arch: "x64".to_string(),
                        desktop_variant: WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT.to_string(),
                        signed: true,
                    },
                ),
            ]
        );
    }

    #[test]
    fn desktop_manifest_uses_checksum_detail_when_present() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();
        write_file(&root.join("Fluxer-2026.520.1-x64.AppImage"), "app");
        write_file(
            &root.join("Fluxer-2026.520.1-x64.AppImage.sha256"),
            "abc123\n",
        );
        write_file(&root.join("Fluxer-2026.520.1-x64.deb"), "deb");

        let manifest = build_desktop_manifest(
            root,
            &PayloadManifestInput {
                channel: "canary".to_string(),
                platform: "linux".to_string(),
                arch: "x64".to_string(),
                desktop_variant: DEFAULT_DESKTOP_VARIANT.to_string(),
                version: "2026.520.1".to_string(),
                pub_date: "2026-05-20T01:02:03Z".to_string(),
            },
        )
        .unwrap();

        assert_eq!(
            manifest.files.get("appimage"),
            Some(&DesktopManifestFile::Detail {
                filename: "Fluxer-2026.520.1-x64.AppImage".to_string(),
                sha256: "abc123".to_string(),
            })
        );
        assert_eq!(
            manifest.files.get("deb"),
            Some(&DesktopManifestFile::Name(
                "Fluxer-2026.520.1-x64.deb".to_string()
            ))
        );
    }

    #[test]
    fn macos_releases_json_points_at_zip_filename() {
        let temp = tempfile::tempdir().unwrap();
        let manifest = DesktopManifest {
            channel: "canary".to_string(),
            platform: "darwin".to_string(),
            arch: "arm64".to_string(),
            variant: None,
            version: "2026.520.1".to_string(),
            pub_date: "2026-05-20T01:02:03Z".to_string(),
            minimum_system_version: Some("12.0".to_string()),
            files: BTreeMap::from([(
                "zip".to_string(),
                DesktopManifestFile::Name("Fluxer-2026.520.1-arm64.zip".to_string()),
            )]),
        };

        write_macos_releases(temp.path(), "desktop-test", "canary", &manifest).unwrap();
        let releases: Value =
            serde_json::from_str(&fs::read_to_string(temp.path().join("RELEASES.json")).unwrap())
                .unwrap();

        assert_eq!(
            releases["releases"][0]["updateTo"]["url"],
            "https://api.fluxer.app/dl/desktop-test/canary/darwin/arm64/Fluxer-2026.520.1-arm64.zip"
        );
        assert!(temp.path().join("releases.json").exists());
    }

    #[test]
    fn velopack_path_lengths_include_install_prefix_and_sort_descending() {
        let temp = tempfile::tempdir().unwrap();
        let archive_path = temp.path().join("test.nupkg");
        {
            let file = File::create(&archive_path).unwrap();
            let mut zip = zip::ZipWriter::new(file);
            let options = SimpleFileOptions::default();
            zip.start_file("short.txt", options).unwrap();
            zip.write_all(b"short").unwrap();
            zip.start_file("deep/path/with/long/file.txt", options)
                .unwrap();
            zip.write_all(b"long").unwrap();
            zip.finish().unwrap();
        }

        let entries =
            velopack_path_lengths(&archive_path, Path::new(r"C:\Users\a\AppData\Local\Fluxer"))
                .unwrap();

        assert_eq!(entries[0].name, "deep/path/with/long/file.txt");
        assert!(entries[0].length > entries[1].length);
    }

    #[test]
    fn windows_package_config_tracks_channel_and_arch() {
        let stable = windows_package_config("stable", "x64");
        assert_eq!(stable.pack_id, "fluxer_desktop");
        assert_eq!(stable.runtime, "win-x64");
        assert_eq!(stable.main_exe, "Fluxer.exe");

        let canary = windows_package_config("canary", "arm64");
        assert_eq!(canary.pack_id, "fluxer_desktop_canary");
        assert_eq!(canary.runtime, "win-arm64");
        assert_eq!(canary.main_exe, "Fluxer Canary.exe");
    }

    #[test]
    fn create_zip_from_dir_preserves_relative_paths() {
        let temp = tempfile::tempdir().unwrap();
        let source = temp.path().join("source");
        write_file(&source.join(".portable"), "");
        write_file(&source.join("resources/app.asar"), "asar");
        let zip_path = temp.path().join("portable.zip");

        create_zip_from_dir(&source, &zip_path).unwrap();

        let file = File::open(zip_path).unwrap();
        let mut zip = zip::ZipArchive::new(file).unwrap();
        assert!(zip.by_name(".portable").is_ok());
        assert!(zip.by_name("resources/app.asar").is_ok());
    }

    #[test]
    fn velopack_portable_archives_are_removed_from_the_release_directory() {
        let temp = tempfile::tempdir().unwrap();
        let output_dir = temp.path();
        write_file(
            &output_dir.join("fluxer_desktop_canary-2026.810.1-Portable.zip"),
            "velopack",
        );
        write_file(
            &output_dir.join("fluxer_desktop_canary-2026.810.1-full.nupkg"),
            "payload",
        );
        write_file(
            &output_dir.join("Fluxer Canary-2026.810.1-win-arm64.exe"),
            "setup",
        );
        write_file(&output_dir.join("RELEASES"), "feed");

        remove_velopack_portable_archives(output_dir).unwrap();

        let remaining = collect_files(output_dir)
            .unwrap()
            .into_iter()
            .filter_map(|path| file_name_string(&path).ok())
            .collect::<BTreeSet<_>>();
        assert!(!remaining.iter().any(|name| name.ends_with(".zip")));
        assert!(remaining.contains("fluxer_desktop_canary-2026.810.1-full.nupkg"));
        assert!(remaining.contains("Fluxer Canary-2026.810.1-win-arm64.exe"));
        assert!(remaining.contains("RELEASES"));
    }

    #[test]
    fn percent_encoded_archive_names_match_their_decoded_inventory_entry() {
        assert_eq!(
            percent_decode_archive_name("Fluxer%20Canary.exe"),
            "Fluxer Canary.exe"
        );
        assert_eq!(percent_decode_archive_name("Fluxer.exe"), "Fluxer.exe");
        assert_eq!(
            percent_decode_archive_name("win-game-capture.win32-arm64-msvc.node"),
            "win-game-capture.win32-arm64-msvc.node"
        );
        assert_eq!(percent_decode_archive_name("100%.dll"), "100%.dll");
        assert_eq!(percent_decode_archive_name("a%zz.dll"), "a%zz.dll");
    }

    #[test]
    fn canary_nupkg_inventory_accepts_percent_encoded_main_executable() {
        let root = Path::new("lib").join("app");
        let files = expected_windows_pe_inventory("arm64", "Fluxer Canary.exe")
            .into_iter()
            .map(|name| {
                if name == "Fluxer Canary.exe" {
                    return root.join("Fluxer%20Canary.exe");
                }
                root.join(name)
            })
            .collect::<Vec<_>>();
        assert_expected_windows_pe_inventory(&root, &files, "arm64", "Fluxer Canary.exe").unwrap();
    }

    #[test]
    fn known_optional_windows_pe_inventory_never_repeats_a_required_binary() {
        for arch in ["x64", "arm64"] {
            for main_exe in ["Fluxer.exe", "Fluxer Canary.exe"] {
                assert_eq!(
                    contradictory_optional_windows_pe_inventory(arch, main_exe),
                    Vec::<String>::new(),
                    "{arch}/{main_exe} declares a binary as both required and known-optional"
                );
            }
        }
    }
}
