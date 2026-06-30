// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{
    CalverEnv, CommandSpec, append_github_env, append_github_output, append_github_path, capture,
    collect_files, command_succeeds, copy_dir_contents, count_files, count_files_min_depth,
    download_file, download_s3_prefix, env_bool, env_string, first_word, get_s3_object_bytes,
    join_s3_key, output_bytes, output_text, parse_bool, path_to_s3_key, remove_dir_if_exists,
    remove_file_if_exists, require_any_env, require_env, require_home, resolve_calver, run_command,
    runner_temp, s3_client, title_case, trim_option, upload_directory_to_s3,
    upload_directory_to_s3_overwrite,
};
use crate::functions::write_json_pretty;
use anyhow::{Context, Result, anyhow, bail, ensure};
use aws_sdk_s3::Client as S3Client;
use chrono::{DateTime, Utc};
use clap::{Args, ValueEnum};
use flate2::{Compression, GzBuilder, read::GzDecoder};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::{self, File, OpenOptions};
use std::io::{self, Cursor, Read, Write};
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

type DesktopFormat = (&'static str, &'static str);
type DesktopArchFormats = (&'static str, &'static [DesktopFormat]);
type DesktopDownloadSection = (
    &'static str,
    &'static str,
    &'static str,
    &'static [DesktopArchFormats],
);

const WINDOWS_DESKTOP_FORMATS: &[DesktopFormat] =
    &[("setup", "Setup.exe"), ("portable", "Portable ZIP")];
const MACOS_DESKTOP_FORMATS: &[DesktopFormat] = &[("dmg", "DMG"), ("zip", "ZIP")];
const DESKTOP_WEBHOOK_CONTENT_LIMIT: usize = 2_000;
const LINUX_DESKTOP_FORMATS: &[DesktopFormat] = &[
    ("appimage", "AppImage"),
    ("deb", "DEB"),
    ("rpm", "RPM"),
    ("tar_gz", "tar.gz"),
];
const WINDOWS_DESKTOP_ARCHES: &[DesktopArchFormats] = &[
    ("x64", WINDOWS_DESKTOP_FORMATS),
    ("arm64", WINDOWS_DESKTOP_FORMATS),
];
const MACOS_DESKTOP_ARCHES: &[DesktopArchFormats] = &[
    ("x64", MACOS_DESKTOP_FORMATS),
    ("arm64", MACOS_DESKTOP_FORMATS),
];
const LINUX_DESKTOP_ARCHES: &[DesktopArchFormats] = &[
    ("x64", LINUX_DESKTOP_FORMATS),
    ("arm64", LINUX_DESKTOP_FORMATS),
];
const DESKTOP_DOWNLOAD_SECTIONS: &[DesktopDownloadSection] = &[
    (
        "win32",
        DEFAULT_DESKTOP_VARIANT,
        "Windows (`win32`)",
        WINDOWS_DESKTOP_ARCHES,
    ),
    (
        "win32",
        WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT,
        "Windows Game Capture (`win32`)",
        WINDOWS_DESKTOP_ARCHES,
    ),
    (
        "darwin",
        DEFAULT_DESKTOP_VARIANT,
        "macOS (`darwin`)",
        MACOS_DESKTOP_ARCHES,
    ),
    (
        "linux",
        DEFAULT_DESKTOP_VARIANT,
        "Linux (`linux`)",
        LINUX_DESKTOP_ARCHES,
    ),
];

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
    PackageAppWindowsVelopack,
    AnalyseVelopackPaths,
    BuildAppLinux,
    CreatePortableZipWindows,
    PrepareArtifactsWindows,
    PrepareArtifactsUnix,
    NormaliseUpdaterYaml,
    GenerateChecksumsUnix,
    GenerateChecksumsWindows,
    BuildSourceTarball,
    UploadHandoff,
    CheckSigningSecrets,
    DownloadWindowsHandoff,
    CheckWindowsArtifacts,
    VerifyAuthenticode,
    RegenerateSignedChecksums,
    StageSignedWindowsArtifacts,
    DownloadHandoff,
    CleanupHandoff,
    BuildPayload,
    UploadPayload,
    VerifySourceTarball,
    BuildSummary,
    NotifyWebhook,
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
        arch: "x64",
        desktop_variant: WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT,
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
        platform: "windows",
        arch: "arm64",
        desktop_variant: WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT,
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
        DesktopStep::PackageAppWindowsVelopack => package_app_windows_velopack_step(),
        DesktopStep::AnalyseVelopackPaths => analyse_velopack_paths_step(),
        DesktopStep::BuildAppLinux => build_app_step(DesktopBuildPlatform::Linux),
        DesktopStep::CreatePortableZipWindows => create_portable_zip_windows_step(),
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
        DesktopStep::BuildSourceTarball => build_source_tarball_step(),
        DesktopStep::UploadHandoff => upload_handoff_step(false).await,
        DesktopStep::CheckSigningSecrets => check_signing_secrets_step(),
        DesktopStep::DownloadWindowsHandoff => download_windows_handoff_step().await,
        DesktopStep::CheckWindowsArtifacts => check_windows_artifacts_step(),
        DesktopStep::VerifyAuthenticode => verify_authenticode_step(),
        DesktopStep::RegenerateSignedChecksums => regenerate_signed_checksums_step(),
        DesktopStep::StageSignedWindowsArtifacts => stage_signed_windows_artifacts_step().await,
        DesktopStep::DownloadHandoff => download_handoff_step().await,
        DesktopStep::CleanupHandoff => cleanup_handoff_step().await,
        DesktopStep::BuildPayload => build_payload_step(),
        DesktopStep::UploadPayload => upload_payload_step().await,
        DesktopStep::VerifySourceTarball => verify_source_tarball_step().await,
        DesktopStep::BuildSummary => build_summary_step(),
        DesktopStep::NotifyWebhook => notify_webhook_step().await,
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
    let windows_x64 = selected_platform(&platforms, "windows", "x64").to_string();
    let windows_arm64 = selected_platform(&platforms, "windows", "arm64").to_string();
    let windows_x64_default =
        selected_platform_variant(&platforms, "windows", "x64", DEFAULT_DESKTOP_VARIANT)
            .to_string();
    let windows_arm64_default =
        selected_platform_variant(&platforms, "windows", "arm64", DEFAULT_DESKTOP_VARIANT)
            .to_string();
    let windows_game_capture_x64 = selected_platform_variant(
        &platforms,
        "windows",
        "x64",
        WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT,
    )
    .to_string();
    let windows_game_capture_arm64 = selected_platform_variant(
        &platforms,
        "windows",
        "arm64",
        WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT,
    )
    .to_string();
    let matrix = format!("{{\"include\":[{include}]}}");
    append_github_output(&[
        ("matrix", matrix.as_str()),
        ("windows_x64", windows_x64.as_str()),
        ("windows_arm64", windows_arm64.as_str()),
        ("windows_x64_default", windows_x64_default.as_str()),
        ("windows_arm64_default", windows_arm64_default.as_str()),
        (
            "windows_game_capture_x64",
            windows_game_capture_x64.as_str(),
        ),
        (
            "windows_game_capture_arm64",
            windows_game_capture_arm64.as_str(),
        ),
    ])
}

fn selected_platforms(args: &BuildDesktopArgs) -> Result<Vec<Platform>> {
    let skip_targets = skip_target_set(args)?;
    Ok(PLATFORMS
        .iter()
        .copied()
        .filter(|platform| !skip_platform(*platform, args, &skip_targets))
        .collect())
}

fn selected_platform(platforms: &[Platform], platform: &str, arch: &str) -> bool {
    platforms
        .iter()
        .any(|item| item.platform == platform && item.arch == arch)
}

fn selected_platform_variant(
    platforms: &[Platform],
    platform: &str,
    arch: &str,
    desktop_variant: &str,
) -> bool {
    platforms.iter().any(|item| {
        item.platform == platform && item.arch == arch && item.desktop_variant == desktop_variant
    })
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
        "windows-game-capture",
        "windows-game-capture-x64",
        "windows-game-capture-arm64",
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
    if platform.desktop_variant != DEFAULT_DESKTOP_VARIANT {
        let variant_arch = format!("{}-{}", platform.desktop_variant, platform.arch);
        if skip_targets.contains(platform.desktop_variant)
            || skip_targets.contains(variant_arch.as_str())
        {
            return true;
        }
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

fn windows_artifact_dir(arch: &str, variant: &str) -> PathBuf {
    let mut name = format!("windows-{arch}");
    if let Some(segment) = desktop_variant_path_segment(variant) {
        name.push('-');
        name.push_str(segment);
    }
    Path::new("artifacts").join(name)
}

fn expected_windows_artifacts(arch: &str, variant: &str) -> bool {
    if env_string("EXPECT_WINDOWS_ARTIFACTS").is_some() {
        return env_bool("EXPECT_WINDOWS_ARTIFACTS");
    }
    let env_name = match (arch, variant) {
        ("x64", DEFAULT_DESKTOP_VARIANT) => "EXPECT_WINDOWS_X64_DEFAULT",
        ("arm64", DEFAULT_DESKTOP_VARIANT) => "EXPECT_WINDOWS_ARM64_DEFAULT",
        ("x64", WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT) => "EXPECT_WINDOWS_GAME_CAPTURE_X64",
        ("arm64", WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT) => "EXPECT_WINDOWS_GAME_CAPTURE_ARM64",
        ("x64", _) => "EXPECT_WINDOWS_X64",
        _ => "EXPECT_WINDOWS_ARM64",
    };
    if env_string(env_name).is_some() {
        return env_bool(env_name);
    }
    let fallback_env = if arch == "x64" {
        "EXPECT_WINDOWS_X64"
    } else {
        "EXPECT_WINDOWS_ARM64"
    };
    env_bool(fallback_env)
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

    run_command(CommandSpec::new(vpk).args([
        "--yes",
        "pack",
        "--packId",
        config.pack_id,
        "--packVersion",
        version.as_str(),
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
    ]))?;

    validate_velopack_output(&config, &version, &arch)?;
    print_directory(&config.output_dir)
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
    fs::write(pack_dir.join(".portable"), "")
        .with_context(|| format!("Failed to write {}", pack_dir.join(".portable").display()))?;
    let zip_name = format!("{}-{version}-portable-win-{arch}.zip", config.pack_title);
    let zip_path = PathBuf::from("dist-electron").join(zip_name);
    create_zip_from_dir(&pack_dir, &zip_path)?;
    let size_mb = fs::metadata(&zip_path)?.len() as f64 / 1024.0 / 1024.0;
    println!(
        "Created portable ZIP: {} ({size_mb:.1} MB)",
        zip_path.display()
    );
    Ok(())
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

fn build_source_tarball_step() -> Result<()> {
    let workdir = require_env("WORKDIR")?;
    let workdir = PathBuf::from(workdir);
    let commit = require_env("SOURCE_SHA")?;
    let short_commit = commit.chars().take(12).collect::<String>();
    let desktop_version = require_env("VERSION")?;
    let published_at = require_env("PUB_DATE")?;
    let build_channel = require_env("BUILD_CHANNEL")?;
    let s3_prefix = require_env("S3_DESKTOP_PREFIX")?;
    let filename = format!("fluxer_desktop-source-{desktop_version}-{short_commit}.tar.gz");
    let archive_dir = Path::new("source_staging").join("by-commit").join(&commit);
    let required_linux_packaging = [
        "packaging/linux/app.fluxer.Fluxer.desktop",
        "packaging/linux/app.fluxer.Fluxer.metainfo.xml",
        "packaging/linux/app.fluxer.Fluxer.svg",
        "packaging/linux/app.fluxer.FluxerCanary.desktop",
        "packaging/linux/app.fluxer.FluxerCanary.metainfo.xml",
        "packaging/linux/app.fluxer.FluxerCanary.svg",
    ];

    ensure!(
        workdir.join("fluxer_desktop/LICENSE").exists(),
        "Missing fluxer_desktop/LICENSE"
    );
    for file in required_linux_packaging {
        let path = workdir.join("fluxer_desktop").join(file);
        ensure!(
            path.exists(),
            "Missing required packaging file {}",
            path.display()
        );
    }
    run_command(
        CommandSpec::new("desktop-file-validate").arg(
            workdir
                .join("fluxer_desktop/packaging/linux/app.fluxer.Fluxer.desktop")
                .to_string_lossy()
                .as_ref(),
        ),
    )?;
    run_command(
        CommandSpec::new("desktop-file-validate").arg(
            workdir
                .join("fluxer_desktop/packaging/linux/app.fluxer.FluxerCanary.desktop")
                .to_string_lossy()
                .as_ref(),
        ),
    )?;
    for file in [
        "app.fluxer.Fluxer.metainfo.xml",
        "app.fluxer.FluxerCanary.metainfo.xml",
    ] {
        run_command(
            CommandSpec::new("appstreamcli").args([
                "validate",
                "--no-net",
                workdir
                    .join("fluxer_desktop/packaging/linux")
                    .join(file)
                    .to_string_lossy()
                    .as_ref(),
            ]),
        )?;
    }

    remove_dir_if_exists(Path::new("source_staging"))?;
    fs::create_dir_all(&archive_dir)
        .with_context(|| format!("Failed to create {}", archive_dir.display()))?;

    let source_dir = TempDir::new().context("Failed to create source temp directory")?;
    let prefix = format!("fluxer_desktop-{desktop_version}-{commit}/");
    let archive_bytes = output_bytes(CommandSpec::new("git").args([
        "-C",
        workdir.to_string_lossy().as_ref(),
        "archive",
        "--format=tar",
        &format!("--prefix={prefix}"),
        "HEAD:fluxer_desktop",
    ]))?;
    tar::Archive::new(Cursor::new(archive_bytes))
        .unpack(source_dir.path())
        .context("Failed to unpack git archive")?;

    let source_root = source_dir
        .path()
        .join(format!("fluxer_desktop-{desktop_version}-{commit}"));
    rewrite_package_version(&source_root.join("package.json"), &desktop_version)?;

    let tar_gz_path = archive_dir.join(&filename);
    create_deterministic_tar_gz(&source_root, &tar_gz_path, &published_at)?;
    let archived_package_version = package_version_from_tar_gz(
        &tar_gz_path,
        &format!("fluxer_desktop-{desktop_version}-{commit}/package.json"),
    )?;
    ensure!(
        archived_package_version == desktop_version,
        "Archived package version {archived_package_version} did not match {desktop_version}"
    );

    let sha256 = sha256_file(&tar_gz_path)?;
    let size = fs::metadata(&tar_gz_path)?.len();
    fs::write(
        archive_dir.join(format!("{filename}.sha256")),
        format!("{sha256}  {filename}\n"),
    )?;
    fs::copy(
        &tar_gz_path,
        Path::new("source_staging").join("latest.tar.gz"),
    )?;
    fs::write(
        Path::new("source_staging").join("latest.tar.gz.sha256"),
        format!("{sha256}  {filename}\n"),
    )?;

    let latest = SourceManifest {
        filename: filename.clone(),
        key: format!("{s3_prefix}/source/by-commit/{commit}/{filename}"),
        sha256,
        commit,
        published_at: published_at.clone(),
        size,
        desktop_version: desktop_version.clone(),
        desktop_version_source: DesktopVersionSource {
            channel: build_channel.clone(),
            platform: "linux".to_string(),
            arch: "x64".to_string(),
            key: format!("{s3_prefix}/{build_channel}/linux/x64/manifest.json"),
            pub_date: published_at,
        },
    };
    write_json_pretty(Path::new("source_staging/latest.json"), &latest)?;

    println!("Desktop source payload:");
    print_tree(Path::new("source_staging"), 4)
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct SourceManifest {
    filename: String,
    key: String,
    sha256: String,
    commit: String,
    published_at: String,
    size: u64,
    desktop_version: String,
    desktop_version_source: DesktopVersionSource,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopVersionSource {
    channel: String,
    platform: String,
    arch: String,
    key: String,
    pub_date: String,
}

fn rewrite_package_version(package_json: &Path, version: &str) -> Result<()> {
    let mut package: Value = serde_json::from_str(
        &fs::read_to_string(package_json)
            .with_context(|| format!("Failed to read {}", package_json.display()))?,
    )
    .with_context(|| format!("Failed to parse {}", package_json.display()))?;
    package["version"] = Value::String(version.to_string());
    write_json_pretty(package_json, &package)
}

fn create_deterministic_tar_gz(
    source_root: &Path,
    output: &Path,
    published_at: &str,
) -> Result<()> {
    let mtime = DateTime::parse_from_rfc3339(published_at)
        .with_context(|| format!("Invalid PUB_DATE: {published_at}"))?
        .timestamp()
        .try_into()
        .context("PUB_DATE timestamp did not fit u64")?;
    let file =
        File::create(output).with_context(|| format!("Failed to create {}", output.display()))?;
    let encoder = GzBuilder::new()
        .mtime(0)
        .write(file, Compression::default());
    let mut builder = tar::Builder::new(encoder);
    let parent = source_root
        .parent()
        .ok_or_else(|| anyhow!("source root has no parent: {}", source_root.display()))?;
    let mut paths = WalkDir::new(source_root)
        .follow_links(false)
        .into_iter()
        .collect::<std::result::Result<Vec<_>, _>>()?;
    paths.sort_by(|a, b| a.path().cmp(b.path()));

    for entry in paths {
        let path = entry.path();
        let archive_path = path.strip_prefix(parent)?;
        let metadata = fs::symlink_metadata(path)?;
        let mut header = tar::Header::new_gnu();
        header.set_mtime(mtime);
        header.set_uid(0);
        header.set_gid(0);
        header.set_mode(if metadata.is_dir() { 0o755 } else { 0o644 });
        if metadata.is_dir() {
            header.set_entry_type(tar::EntryType::Directory);
            header.set_size(0);
            header.set_cksum();
            builder.append_data(&mut header, archive_path, io::empty())?;
        } else if metadata.file_type().is_symlink() {
            let target = fs::read_link(path)?;
            header.set_entry_type(tar::EntryType::Symlink);
            header.set_size(0);
            header.set_cksum();
            builder.append_link(&mut header, archive_path, target)?;
        } else if metadata.is_file() {
            header.set_entry_type(tar::EntryType::Regular);
            header.set_size(metadata.len());
            header.set_cksum();
            let mut file = File::open(path)?;
            builder.append_data(&mut header, archive_path, &mut file)?;
        }
    }
    let encoder = builder.into_inner()?;
    encoder.finish()?;
    Ok(())
}

fn package_version_from_tar_gz(tar_gz: &Path, package_json_path: &str) -> Result<String> {
    let file =
        File::open(tar_gz).with_context(|| format!("Failed to open {}", tar_gz.display()))?;
    let decoder = GzDecoder::new(file);
    let mut archive = tar::Archive::new(decoder);
    for entry in archive.entries()? {
        let mut entry = entry?;
        if entry.path()?.to_string_lossy() == package_json_path {
            let mut text = String::new();
            entry.read_to_string(&mut text)?;
            let package: Value = serde_json::from_str(&text)?;
            return package
                .get("version")
                .and_then(Value::as_str)
                .map(ToOwned::to_owned)
                .ok_or_else(|| anyhow!("package.json in archive has no version"));
        }
    }
    bail!("{package_json_path} not found in {}", tar_gz.display())
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

    let source_staging = Path::new("source_staging");
    if !signed_windows_artifacts && source_staging.exists() {
        let source_count = count_files(source_staging)?;
        if source_count > 0 {
            let source_name = format!("fluxer-desktop-{build_channel}-source-linux-x64");
            let source_prefix = join_s3_key(&prefix, &source_name);
            println!("Uploading {source_count} desktop source file(s) to {source_prefix}");
            upload_directory_to_s3(&client, &bucket, &source_prefix, source_staging, |_| true)
                .await?;
        }
    }
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

fn check_signing_secrets_step() -> Result<()> {
    if env_string("AZURE_CLIENT_ID").is_some() {
        append_github_output(&[("enabled", "true")])
    } else {
        println!("::notice::Windows code signing secrets not configured - skipping signing.");
        append_github_output(&[("enabled", "false")])
    }
}

async fn download_windows_handoff_step() -> Result<()> {
    let client = s3_client(None).await?;
    let bucket = require_env("S3_BUCKET")?;
    let prefix = require_env("DESKTOP_HANDOFF_PREFIX")?;
    let build_channel = require_env("BUILD_CHANNEL")?;
    let arch = require_any_env(&["DESKTOP_ARCH", "ARCH"])?;
    let desktop_variant = desktop_variant_from_env()?;
    let artifact_name =
        handoff_artifact_name(&build_channel, "windows", &arch, &desktop_variant, false);
    let target = windows_artifact_dir(&arch, &desktop_variant);
    remove_dir_if_exists(&target)?;
    fs::create_dir_all(&target)?;
    let artifact_prefix = join_s3_key(&prefix, &artifact_name);
    download_s3_prefix(&client, &bucket, &artifact_prefix, &target).await
}

fn check_windows_artifacts_step() -> Result<()> {
    let arch = require_any_env(&["DESKTOP_ARCH", "ARCH"])?;
    let desktop_variant = desktop_variant_from_env()?;
    let artifact_path = windows_artifact_dir(&arch, &desktop_variant);
    let found = artifact_path.exists() && count_files(&artifact_path)? > 0;
    if !found && expected_windows_artifacts(&arch, &desktop_variant) {
        bail!("Expected Windows artifacts for {arch} ({desktop_variant}), but none were found.");
    }
    append_github_output(&[("found", if found { "true" } else { "false" })])?;
    if found {
        println!("Found Windows artifacts for {arch} ({desktop_variant}).");
    } else {
        println!(
            "No Windows artifacts found for {arch} ({desktop_variant}). Skipping signing for this variant."
        );
    }
    Ok(())
}

fn verify_authenticode_step() -> Result<()> {
    let arch = require_any_env(&["DESKTOP_ARCH", "ARCH"])?;
    let desktop_variant = desktop_variant_from_env()?;
    let artifact_path = windows_artifact_dir(&arch, &desktop_variant);
    let files = collect_files(&artifact_path)?
        .into_iter()
        .filter(|path| extension_is(path, "exe"))
        .collect::<Vec<_>>();
    ensure!(
        !files.is_empty(),
        "No executable files found to verify for {arch}."
    );
    let signtool = find_signtool()?;
    for file in files {
        run_command(CommandSpec::new(&signtool).args([
            "verify",
            "/pa",
            "/all",
            file.to_string_lossy().as_ref(),
        ]))?;
    }
    Ok(())
}

fn regenerate_signed_checksums_step() -> Result<()> {
    let arch = require_any_env(&["DESKTOP_ARCH", "ARCH"])?;
    let desktop_variant = desktop_variant_from_env()?;
    let artifact_path = windows_artifact_dir(&arch, &desktop_variant);
    for file in collect_files(&artifact_path)?
        .into_iter()
        .filter(|path| extension_is(path, "exe"))
    {
        let hash = sha256_file(&file)?;
        let name = file_name_string(&file)?;
        fs::write(file.with_file_name(format!("{name}.sha256")), &hash)?;
        println!("Regenerated checksum for {}", file.display());
    }
    Ok(())
}

async fn stage_signed_windows_artifacts_step() -> Result<()> {
    let arch = require_any_env(&["DESKTOP_ARCH", "ARCH"])?;
    let desktop_variant = desktop_variant_from_env()?;
    let source = windows_artifact_dir(&arch, &desktop_variant);
    let staging = Path::new("upload_staging");
    remove_dir_if_exists(staging)?;
    fs::create_dir_all(staging)?;
    copy_dir_contents(&source, staging)?;
    upload_handoff_step(true).await
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

    let source_artifact_root = artifacts.join(format!("fluxer-desktop-{channel}-source-linux-x64"));
    let source_artifact = if source_artifact_root.join("latest.json").exists() {
        source_artifact_root.clone()
    } else {
        source_artifact_root.join("source_staging")
    };
    if source_artifact.join("latest.json").exists() {
        let source_dest = payload_root.join("source");
        fs::create_dir_all(&source_dest)?;
        copy_dir_contents(&source_artifact, &source_dest)?;
    }

    println!("Payload tree:");
    print_tree(&payload_root, 6)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ArtifactIdentity {
    platform: String,
    arch: String,
    desktop_variant: String,
    source: bool,
    signed: bool,
}

fn parse_artifact_dir_name(base: &str, channel: &str) -> Option<ArtifactIdentity> {
    let prefix = format!("fluxer-desktop-{channel}-");
    let rest = base.strip_prefix(&prefix)?;
    let (rest, signed) = rest
        .strip_suffix("-signed")
        .map(|value| (value, true))
        .unwrap_or((rest, false));
    if rest == "source-linux-x64" {
        return Some(ArtifactIdentity {
            platform: "linux".to_string(),
            arch: "x64".to_string(),
            desktop_variant: DEFAULT_DESKTOP_VARIANT.to_string(),
            source: true,
            signed: false,
        });
    }
    let (rest, desktop_variant) = rest
        .strip_suffix(&format!("-{WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT}"))
        .map(|value| (value, WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT))
        .unwrap_or((rest, DEFAULT_DESKTOP_VARIANT));
    let (platform, arch) = rest.rsplit_once('-')?;
    Some(ArtifactIdentity {
        platform: platform.to_string(),
        arch: arch.to_string(),
        desktop_variant: desktop_variant.to_string(),
        source: false,
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
        if identity.source {
            continue;
        }

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
    let overwrite_existing = should_overwrite_payload(&s3_prefix, env_bool("TEST_BUILD"));

    println!("Uploading desktop binaries and checksums first (prefix: {s3_prefix})...");
    upload_payload_directory(
        &client,
        &bucket,
        &s3_prefix,
        &payload_root,
        overwrite_existing,
        |relative| !is_payload_metadata_key(relative),
    )
    .await?;
    println!("Uploading manifests and updater metadata last...");
    upload_payload_directory(
        &client,
        &bucket,
        &s3_prefix,
        &payload_root,
        overwrite_existing,
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
        || relative
            .to_string_lossy()
            .replace('\\', "/")
            .ends_with("source/latest.json")
}

async fn verify_source_tarball_step() -> Result<()> {
    let client = s3_client(None).await?;
    let s3_prefix = require_env("S3_DESKTOP_PREFIX")?;
    let manifest_path = Path::new("s3_payload")
        .join(&s3_prefix)
        .join("source")
        .join("latest.json");
    if !manifest_path.exists() {
        println!("No desktop source tarball payload present; skipping source verification.");
        return Ok(());
    }

    let manifest: SourceManifest = serde_json::from_str(
        &fs::read_to_string(&manifest_path)
            .with_context(|| format!("Failed to read {}", manifest_path.display()))?,
    )?;
    ensure!(
        !manifest.filename.is_empty()
            && !manifest.sha256.is_empty()
            && !manifest.desktop_version.is_empty()
            && !manifest.commit.is_empty(),
        "Desktop source manifest is missing filename, sha256, desktop_version, or commit."
    );

    let bucket = require_env("S3_BUCKET")?;
    let remote_base = join_s3_key(&s3_prefix, "source");
    let remote_sha256 = first_word(&String::from_utf8(
        get_s3_object_bytes(
            &client,
            &bucket,
            &join_s3_key(
                &remote_base,
                &format!("by-commit/{}/{}.sha256", manifest.commit, manifest.filename),
            ),
        )
        .await?
        .to_vec(),
    )?);
    let latest_sha256 = first_word(&String::from_utf8(
        get_s3_object_bytes(
            &client,
            &bucket,
            &join_s3_key(&remote_base, "latest.tar.gz.sha256"),
        )
        .await?
        .to_vec(),
    )?);
    let remote_manifest: SourceManifest = serde_json::from_slice(
        &get_s3_object_bytes(&client, &bucket, &join_s3_key(&remote_base, "latest.json")).await?,
    )?;
    ensure!(
        remote_sha256 == manifest.sha256,
        "Remote by-commit checksum mismatch"
    );
    ensure!(
        latest_sha256 == manifest.sha256,
        "Remote latest checksum mismatch"
    );
    ensure!(
        remote_manifest.sha256 == manifest.sha256,
        "Remote manifest checksum mismatch"
    );
    ensure!(
        remote_manifest.desktop_version == manifest.desktop_version,
        "Remote manifest desktop version mismatch"
    );

    let tar_bytes = get_s3_object_bytes(
        &client,
        &bucket,
        &join_s3_key(
            &remote_base,
            &format!("by-commit/{}/{}", manifest.commit, manifest.filename),
        ),
    )
    .await?;
    let package_version = package_version_from_tar_gz_bytes(
        &tar_bytes,
        &format!(
            "fluxer_desktop-{}-{}/package.json",
            manifest.desktop_version, manifest.commit
        ),
    )?;
    ensure!(
        package_version == manifest.desktop_version,
        "Remote source tarball package version mismatch"
    );
    Ok(())
}

fn package_version_from_tar_gz_bytes(bytes: &[u8], package_json_path: &str) -> Result<String> {
    let decoder = GzDecoder::new(Cursor::new(bytes));
    let mut archive = tar::Archive::new(decoder);
    for entry in archive.entries()? {
        let mut entry = entry?;
        if entry.path()?.to_string_lossy() == package_json_path {
            let mut text = String::new();
            entry.read_to_string(&mut text)?;
            let package: Value = serde_json::from_str(&text)?;
            return package
                .get("version")
                .and_then(Value::as_str)
                .map(ToOwned::to_owned)
                .ok_or_else(|| anyhow!("package.json in archive has no version"));
        }
    }
    bail!("{package_json_path} not found in source tarball")
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

async fn notify_webhook_step() -> Result<()> {
    let version = require_env("VERSION")?;
    let channel = env::var("DISPLAY_CHANNEL")
        .ok()
        .filter(|value| !value.is_empty())
        .or_else(|| env::var("CHANNEL").ok())
        .unwrap_or_default();
    let test_build = env_bool("TEST_BUILD");

    if !should_notify_desktop_webhook(&channel) {
        println!("Skipping desktop notification for channel={channel}, test_build={test_build}.");
        return Ok(());
    }

    let webhook_url = env::var("FLUXER_WEBHOOK_URL")
        .unwrap_or_default()
        .trim()
        .to_string();
    if webhook_url.is_empty() {
        println!("FLUXER_WEBHOOK_URL is not set; skipping desktop canary notification.");
        return Ok(());
    }

    let messages = desktop_webhook_messages(&version, test_build, None)?;
    let message_count = messages.len();
    for (index, message) in messages.into_iter().enumerate() {
        let response = Client::new()
            .post(&webhook_url)
            .header("User-Agent", "fluxer-ci-desktop")
            .json(&json!({
                "content": message,
                "allowed_mentions": {"parse": []},
            }))
            .send()
            .await
            .context("Failed to send desktop webhook")?;
        let status = response.status();
        if !status.is_success() {
            let body = response.text().await.unwrap_or_default();
            bail!(
                "Desktop webhook part {}/{} returned {status}: {body}",
                index + 1,
                message_count
            );
        }
        println!(
            "Desktop canary notification part {}/{} sent ({status}).",
            index + 1,
            message_count
        );
    }
    Ok(())
}

fn should_notify_desktop_webhook(channel: &str) -> bool {
    channel == "canary"
}

fn desktop_payload_root() -> PathBuf {
    let s3_prefix = env::var("S3_DESKTOP_PREFIX").unwrap_or_else(|_| "desktop".to_string());
    let channel = env::var("CHANNEL").unwrap_or_else(|_| "canary".to_string());
    PathBuf::from("s3_payload").join(s3_prefix).join(channel)
}

fn desktop_manifest_formats(
    payload_root: &Path,
    platform: &str,
    arch: &str,
    desktop_variant: &str,
) -> Result<BTreeSet<String>> {
    let mut manifest_path = payload_root.join(platform).join(arch);
    if let Some(segment) = desktop_variant_path_segment(desktop_variant) {
        manifest_path = manifest_path.join(segment);
    }
    manifest_path = manifest_path.join("manifest.json");
    if !manifest_path.exists() {
        return Ok(BTreeSet::new());
    }

    let manifest: Value = serde_json::from_str(
        &fs::read_to_string(&manifest_path)
            .with_context(|| format!("Failed to read {}", manifest_path.display()))?,
    )
    .with_context(|| format!("Failed to parse {}", manifest_path.display()))?;
    let files = manifest
        .get("files")
        .and_then(Value::as_object)
        .ok_or_else(|| {
            anyhow!(
                "Invalid desktop manifest files object: {}",
                manifest_path.display()
            )
        })?;
    Ok(files.keys().cloned().collect())
}

fn desktop_download_url(
    platform: &str,
    arch: &str,
    desktop_variant: &str,
    version: &str,
    format_name: &str,
    test_build: bool,
) -> String {
    let test_query = if test_build { "?test=1" } else { "" };
    let variant_segment = desktop_variant_path_segment(desktop_variant)
        .map(|variant| format!("/{variant}"))
        .unwrap_or_default();
    format!(
        "{PUBLIC_DL_BASE}/desktop/canary/{platform}/{arch}{variant_segment}/{version}/{format_name}{test_query}"
    )
}

#[cfg(test)]
fn desktop_download_table(
    version: &str,
    test_build: bool,
    payload_root: Option<&Path>,
) -> Result<String> {
    Ok(desktop_download_sections(version, test_build, payload_root)?.join("\n\n"))
}

fn desktop_webhook_messages(
    version: &str,
    test_build: bool,
    payload_root: Option<&Path>,
) -> Result<Vec<String>> {
    let title = if test_build {
        "Canary Desktop Test Build Ready"
    } else {
        "Canary Desktop Build Ready"
    };
    let mut messages = Vec::new();
    let mut current = format!("## {title}\n\nDesktop app version: `{version}`");
    for section in desktop_download_sections(version, test_build, payload_root)? {
        append_desktop_webhook_section(&mut messages, &mut current, &section)?;
    }
    if !current.is_empty() {
        messages.push(current);
    }
    Ok(messages)
}

fn append_desktop_webhook_section(
    messages: &mut Vec<String>,
    current: &mut String,
    section: &str,
) -> Result<()> {
    let separator = if current.is_empty() { "" } else { "\n\n" };
    let candidate = format!("{current}{separator}{section}");
    if candidate.chars().count() <= DESKTOP_WEBHOOK_CONTENT_LIMIT {
        *current = candidate;
        return Ok(());
    }

    if !current.is_empty() {
        messages.push(std::mem::take(current));
    }
    ensure!(
        section.chars().count() <= DESKTOP_WEBHOOK_CONTENT_LIMIT,
        "Desktop webhook section exceeds {DESKTOP_WEBHOOK_CONTENT_LIMIT} characters."
    );
    current.push_str(section);
    Ok(())
}

fn desktop_download_sections(
    version: &str,
    test_build: bool,
    payload_root: Option<&Path>,
) -> Result<Vec<String>> {
    let root;
    let payload_root = match payload_root {
        Some(path) => path,
        None => {
            root = desktop_payload_root();
            root.as_path()
        }
    };
    let mut rendered_sections = Vec::new();
    for (platform, desktop_variant, heading, arch_groups) in DESKTOP_DOWNLOAD_SECTIONS {
        let mut rows = Vec::new();
        for (arch, formats) in *arch_groups {
            let available_formats =
                desktop_manifest_formats(payload_root, platform, arch, desktop_variant)?;
            for (format_name, label) in *formats {
                if available_formats.contains(*format_name) {
                    rows.push(format!(
                        "| {arch} | {label} | {} |",
                        desktop_download_url(
                            platform,
                            arch,
                            desktop_variant,
                            version,
                            format_name,
                            test_build
                        )
                    ));
                }
            }
        }

        if !rows.is_empty() {
            let mut table = vec![
                "| Arch | Format | URL |".to_string(),
                "|---|---|---|".to_string(),
            ];
            table.extend(rows);
            rendered_sections.push(format!("## {heading}\n\n{}", table.join("\n")));
        }
    }

    if rendered_sections.is_empty() {
        bail!(
            "No desktop manifests found under {}; refusing to send an empty notification.",
            payload_root.display()
        );
    }

    Ok(rendered_sections)
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
    for root in roots {
        if !root.exists() {
            continue;
        }
        if let Some(path) = find_first(&root, |path| {
            path.file_name()
                .and_then(OsStr::to_str)
                .is_some_and(|name| name.eq_ignore_ascii_case("signtool.exe"))
                && path.to_string_lossy().contains("x64")
        }) {
            return Ok(path);
        }
    }
    Ok(PathBuf::from("signtool.exe"))
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
    fn desktop_webhook_notifies_canary_uploads_and_tests() {
        assert!(should_notify_desktop_webhook("canary"));
        assert!(!should_notify_desktop_webhook("stable"));
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
                "{\"platform\":\"windows\",\"arch\":\"arm64\",\"desktop_variant\":\"windows-game-capture\",\"os\":\"blacksmith-32vcpu-windows-2025\",\"electron_arch\":\"arm64\"}",
                "{\"platform\":\"linux\",\"arch\":\"x64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-ubuntu-2404\",\"electron_arch\":\"x64\"}",
                "{\"platform\":\"linux\",\"arch\":\"arm64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-ubuntu-2404-arm\",\"electron_arch\":\"arm64\"}",
            ]
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
                "{\"platform\":\"windows\",\"arch\":\"arm64\",\"desktop_variant\":\"windows-game-capture\",\"os\":\"blacksmith-32vcpu-windows-2025\",\"electron_arch\":\"arm64\"}",
                "{\"platform\":\"linux\",\"arch\":\"x64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-ubuntu-2404\",\"electron_arch\":\"x64\"}",
                "{\"platform\":\"linux\",\"arch\":\"arm64\",\"desktop_variant\":\"default\",\"os\":\"blacksmith-32vcpu-ubuntu-2404-arm\",\"electron_arch\":\"arm64\"}",
            ]
        );
    }

    #[test]
    fn matrix_skip_targets_reject_unknown_values() {
        let mut args = matrix_args();
        args.skip_targets = Some("windows-riscv".to_string());

        let error = selected_platforms(&args).unwrap_err();

        assert!(error.to_string().contains("Unknown desktop skip target"));
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
        write_file(&root.join("source/latest.json"), "{}");
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
                "desktop/source/latest.json",
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
                source: false,
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
                source: false,
                signed: true,
            }
        );
        assert!(parse_artifact_dir_name("fluxer-desktop-stable-linux-x64", "canary").is_none());
        assert!(
            parse_artifact_dir_name("fluxer-desktop-canary-source-linux-x64", "canary")
                .unwrap()
                .source
        );
        assert_eq!(
            parse_artifact_dir_name("fluxer-desktop-canary-windows-x64-signed", "canary").unwrap(),
            ArtifactIdentity {
                platform: "windows".to_string(),
                arch: "x64".to_string(),
                desktop_variant: DEFAULT_DESKTOP_VARIANT.to_string(),
                source: false,
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
        fs::create_dir_all(artifacts.join("fluxer-desktop-canary-source-linux-x64")).unwrap();
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
                        source: false,
                        signed: false,
                    },
                ),
                (
                    "fluxer-desktop-canary-windows-x64-signed".to_string(),
                    ArtifactIdentity {
                        platform: "windows".to_string(),
                        arch: "x64".to_string(),
                        desktop_variant: DEFAULT_DESKTOP_VARIANT.to_string(),
                        source: false,
                        signed: true,
                    },
                ),
                (
                    "fluxer-desktop-canary-windows-x64-windows-game-capture-signed".to_string(),
                    ArtifactIdentity {
                        platform: "windows".to_string(),
                        arch: "x64".to_string(),
                        desktop_variant: WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT.to_string(),
                        source: false,
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
    fn deterministic_tarball_rewrites_and_reads_package_version() {
        let temp = tempfile::tempdir().unwrap();
        let source_root = temp.path().join("fluxer_desktop-2026.520.1-abcdef");
        write_file(
            &source_root.join("package.json"),
            r#"{"name":"fluxer","version":"0.0.0"}"#,
        );
        rewrite_package_version(&source_root.join("package.json"), "2026.520.1").unwrap();
        let archive = temp.path().join("source.tar.gz");

        create_deterministic_tar_gz(&source_root, &archive, "2026-05-20T01:02:03Z").unwrap();

        assert_eq!(
            package_version_from_tar_gz(&archive, "fluxer_desktop-2026.520.1-abcdef/package.json")
                .unwrap(),
            "2026.520.1"
        );
        assert_eq!(sha256_file(&archive).unwrap().len(), 64);
    }

    #[test]
    fn desktop_download_table_uses_manifest_files() {
        let temp = tempfile::tempdir().unwrap();
        let manifest_dir = temp.path().join("win32").join("x64");
        fs::create_dir_all(&manifest_dir).unwrap();
        fs::write(
            manifest_dir.join("manifest.json"),
            r#"{"files":{"setup":"Fluxer.exe","portable":"Fluxer.zip"}}"#,
        )
        .unwrap();
        let variant_manifest_dir = temp
            .path()
            .join("win32")
            .join("x64")
            .join(WINDOWS_GAME_CAPTURE_DESKTOP_VARIANT);
        fs::create_dir_all(&variant_manifest_dir).unwrap();
        fs::write(
            variant_manifest_dir.join("manifest.json"),
            r#"{"files":{"setup":"Fluxer Game Capture.exe"}}"#,
        )
        .unwrap();

        let table = desktop_download_table("2026.520.1", true, Some(temp.path())).unwrap();
        assert!(table.contains("## Windows (`win32`)"));
        assert!(table.contains("## Windows Game Capture (`win32`)"));
        assert!(table.contains("| Arch | Format | URL |"));
        assert!(table.contains("|---|---|---|"));
        assert!(table.contains("| x64 | Setup.exe | https://api.fluxer.app/dl/desktop/canary/win32/x64/2026.520.1/setup?test=1 |"));
        assert!(table.contains("| x64 | Portable ZIP | https://api.fluxer.app/dl/desktop/canary/win32/x64/2026.520.1/portable?test=1 |"));
        assert!(table.contains("| x64 | Setup.exe | https://api.fluxer.app/dl/desktop/canary/win32/x64/windows-game-capture/2026.520.1/setup?test=1 |"));
        assert!(!table.contains("SHA-256"));
    }

    #[test]
    fn desktop_webhook_messages_split_under_receiver_limit() {
        let temp = tempfile::tempdir().unwrap();
        let manifest = r#"{"files":{"setup":"Fluxer.exe","portable":"Fluxer.zip","dmg":"Fluxer.dmg","zip":"Fluxer.zip","appimage":"Fluxer.AppImage","deb":"Fluxer.deb","rpm":"Fluxer.rpm","tar_gz":"Fluxer.tar.gz"}}"#;
        for (platform, desktop_variant, _, arch_groups) in DESKTOP_DOWNLOAD_SECTIONS {
            for (arch, _) in *arch_groups {
                let mut manifest_dir = temp.path().join(platform).join(arch);
                if let Some(segment) = desktop_variant_path_segment(desktop_variant) {
                    manifest_dir = manifest_dir.join(segment);
                }
                write_file(&manifest_dir.join("manifest.json"), manifest);
            }
        }

        let messages =
            desktop_webhook_messages("2026.614.181201", true, Some(temp.path())).unwrap();
        assert!(messages.len() > 1);
        assert!(
            messages
                .iter()
                .all(|message| message.chars().count() <= DESKTOP_WEBHOOK_CONTENT_LIMIT)
        );

        let combined = messages.join("\n\n");
        assert!(combined.starts_with("## Canary Desktop Test Build Ready"));
        assert!(combined.contains("Desktop app version: `2026.614.181201`"));
        assert!(combined.contains("## Windows (`win32`)"));
        assert!(combined.contains("## Windows Game Capture (`win32`)"));
        assert!(combined.contains("## macOS (`darwin`)"));
        assert!(combined.contains("## Linux (`linux`)"));
        assert!(combined.contains("| Arch | Format | URL |"));
        assert!(!combined.contains("SHA-256"));
    }
}
