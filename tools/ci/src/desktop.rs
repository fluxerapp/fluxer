// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{
    CalverEnv, CommandSpec, append_github_env, append_github_output, append_github_path, capture,
    collect_files, command_succeeds, copy_dir_contents, count_files, download_file, env_bool,
    env_string, output_bytes, output_text, parse_bool, path_to_s3_key, remove_dir_if_exists,
    remove_empty_dirs_below, remove_file_if_exists, require_any_env, require_env, require_home,
    resolve_calver, run_command, runner_temp, title_case, trim_option,
};
use crate::functions::{sha256_file, write_json_pretty};
use crate::release::{
    DESKTOP_RELEASE_DESCRIPTOR_SCHEMA_VERSION, DesktopReleaseAsset, DesktopReleaseDescriptor,
    DesktopReleaseKind, RELEASE_REPOSITORY, desktop_module_manifest_release_asset_name,
    desktop_module_manifest_storage_key, desktop_module_package_release_asset_name,
    desktop_module_package_storage_key, desktop_release_asset_name, desktop_release_coordinates,
    desktop_release_descriptor_filename, desktop_release_product, desktop_release_shipped_formats,
    desktop_release_update_payload_suffix, desktop_release_updater_feeds,
    validate_desktop_release_descriptor, validate_desktop_release_module_files,
};
use anyhow::{Context, Result, anyhow, bail, ensure};
use chrono::{DateTime, Utc};
use clap::{Args, ValueEnum};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs::{self, File, OpenOptions};
use std::io::{self, BufReader, BufWriter, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::time::{Duration, Instant};
use tempfile::TempDir;
use walkdir::WalkDir;
use zip::write::SimpleFileOptions;

const PACKAGE_ORIGIN_BASE: &str = "https://pkgs.fluxer.com";
const PACKAGE_ORIGIN_BASE_ENV: &str = "PKGS_DOWNLOAD_BASE_URL";
const PNPM_VERSION: &str = "12.4.2";
const RUST_TOOLCHAIN: &str = "1.98.1";
const LINUX_PIPEWIRE_VERSION: &str = "0.3.65";
const LINUX_PIPEWIRE_HEADER_DIR: &str = "pipewire-0.3";
const LINUX_SPA_HEADER_DIR: &str = "spa-0.2";
const LINUX_PIPEWIRE_SOURCE_SHA256: &str =
    "bb76f938136d0ce8c35bffa99e002dc2dbaeab5e14c6c34154e7f750013d1d6b";
const LINUX_LIBFIDO2_VERSION: &str = "1.16.0";
const LINUX_LIBFIDO2_SOURCE_SHA256: &str =
    "8c2b6fb279b5b42e9ac92ade71832e485852647b53607c43baaafbbcecea04e4";
pub(crate) const MACOS_UNIVERSAL_ARCH: &str = "universal";
const MACOS_MINIMUM_SYSTEM_VERSION: &str = "13.0";
const DESKTOP_PAYLOAD_PREFIX: &str = "desktop";
const DESKTOP_SHARED_ASSETS_DIR_NAME: &str = "desktop-shared-assets";
const DESKTOP_SHARED_ASSETS_PAYLOAD_DIR_NAME: &str = "renderer";
const DESKTOP_SHARED_ASSETS_MANIFEST_NAME: &str = "manifest.json";
const DESKTOP_SHARED_ASSET_FILE_LIMIT: usize = 100_000;
const DESKTOP_MODULES_DIR_NAME: &str = "desktop-modules";
const DESKTOP_MODULE_FILES_DIR_NAME: &str = "files";
const DESKTOP_MODULE_FILE_LIST_NAME: &str = "module.json";
const DESKTOP_MODULE_CLASSIFICATION_NAME: &str = "classification.json";
pub(crate) const DESKTOP_MODULE_PACKAGE_NAME: &str = "package.br";
const DESKTOP_MODULE_PACKAGE_CHECKSUM_NAME: &str = "package.br.sha256";
pub(crate) const DESKTOP_RENDERER_MODULE: &str = "fluxer_renderer";
const DESKTOP_SOURCEMAP_MODULE: &str = "fluxer_sourcemaps";
const DESKTOP_SOURCEMAP_EXTENSION: &str = "map";
const DESKTOP_MODULE_ASSETS_DIR_NAME: &str = "assets";
const DESKTOP_MODULE_NAME_MAX_LENGTH: usize = 64;
const DESKTOP_PRECOMPRESSED_EXTENSIONS: &[&str] = &[
    "avif", "br", "gif", "gz", "ico", "jpeg", "jpg", "mp3", "mp4", "ogg", "onnx", "png", "webm",
    "webp", "woff2", "zip",
];
const DESKTOP_MODULE_BROTLI_QUALITY: u32 = 9;
const DESKTOP_PRECOMPRESSED_MODULE_BROTLI_QUALITY: u32 = 0;
const DESKTOP_MODULE_BROTLI_WINDOW: u32 = 24;
const DESKTOP_MODULE_BROTLI_BUFFER_BYTES: usize = 1024 * 1024;
const DESKTOP_MODULE_TAR_MODE: u32 = 0o644;
const DESKTOP_MODULE_PACK_MAX_THREADS: usize = 8;
const DESKTOP_PAYLOAD_MANIFEST_NAME: &str = "manifest.json";
pub(crate) const DESKTOP_CHANNEL_MANIFEST_NAME: &str = "modules.json";
pub(crate) const DESKTOP_MODULES_KEY_SEGMENT: &str = "modules";
const DESKTOP_CHANNEL_MANIFEST_VERSION: u64 = 1;
const DESKTOP_CHANNEL_MANIFEST_MAX_BYTES: usize = 1024 * 1024;
const DESKTOP_MODULE_ASSET_SEGMENT_RULE: &str = "module_asset_segment";
const DESKTOP_RENDERER_REMAINDER_RULE: &str = "renderer_remainder";
const DESKTOP_SOURCEMAP_RULE: &str = "source_map";
const DESKTOP_SPELLCHECK_DICTIONARY_RULE: &str = "spellcheck_dictionary";
const DESKTOP_DICTIONARY_MODULE_PREFIX: &str = "fluxer_dict_";
const DESKTOP_DICTIONARY_PACKAGE_PREFIX: &str = "dictionary-";
const DESKTOP_DICTIONARY_FILE_NAMES: &[&str] = &["index.aff", "index.dic"];
const DESKTOP_MODULE_MINIMUM_SHELL_VERSION: &str = "2026.1008.42615";
const DESKTOP_DEVELOPMENT_MINIMUM_SHELL_VERSION: &str = "0.0.0";
const DESKTOP_REQUIRED_MODULES: &[&str] = &[DESKTOP_RENDERER_MODULE];
const DESKTOP_MODULES_ENV: &str = "FLUXER_MODULES";

#[derive(Debug, Args, Clone)]
pub struct BuildDesktopArgs {
    #[arg(long, value_enum)]
    step: DesktopStep,
    #[arg(long)]
    channel: Option<String>,
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
    SetupPnpm,
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
    BuildSharedAssets,
    PrepareSharedAssets,
    RestoreSharedAssets,
    SplitModules,
    PackModules,
    StripShellRenderer,
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
    BuildAppimageUpdateFeed,
    CreatePortableZipWindows,
    VerifyWindowsSignedArtifacts,
    PrepareArtifactsWindows,
    PrepareArtifactsUnix,
    NormaliseUpdaterYaml,
    GenerateChecksumsUnix,
    GenerateChecksumsWindows,
    StageHandoff,
    BuildPayload,
    BuildModuleManifest,
    PrepareReleaseAssets,
    BuildSummary,
    CheckShellDrift,
    BuildModulesOnlyManifest,
    PrepareModulesReleaseAssets,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Platform {
    platform: &'static str,
    arch: &'static str,
    os: &'static str,
    electron_arch: &'static str,
}

const PLATFORMS: &[Platform] = &[
    Platform {
        platform: "windows",
        arch: "x64",
        os: "windows-2025",
        electron_arch: "x64",
    },
    Platform {
        platform: "windows",
        arch: "arm64",
        os: "windows-2025",
        electron_arch: "arm64",
    },
    Platform {
        platform: "macos",
        arch: MACOS_UNIVERSAL_ARCH,
        os: "fluxer-desktop-macos-arm64",
        electron_arch: MACOS_UNIVERSAL_ARCH,
    },
    Platform {
        platform: "linux",
        arch: "x64",
        os: "ubuntu-22.04",
        electron_arch: "x64",
    },
    Platform {
        platform: "linux",
        arch: "arm64",
        os: "ubuntu-22.04-arm",
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
            set_metadata_step(&channel)
        }
        DesktopStep::SetMatrix => set_matrix_step(&args),
        DesktopStep::WindowsPaths => windows_paths_step().await,
        DesktopStep::SetWorkdirUnix => set_workdir_unix_step(),
        DesktopStep::EnsurePython3Windows => ensure_python3_windows_step(),
        DesktopStep::SetupPnpm => setup_pnpm_step(),
        DesktopStep::ResolvePnpmStoreWindows | DesktopStep::ResolvePnpmStoreUnix => {
            resolve_pnpm_store_step()
        }
        DesktopStep::InstallSetuptoolsWindowsArm64 => install_setuptools_windows_arm64_step(),
        DesktopStep::InstallSetuptoolsMacos => install_setuptools_macos_step(),
        DesktopStep::InstallLinuxDeps => install_linux_deps_step().await,
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
        DesktopStep::BuildSharedAssets => build_shared_assets_step(),
        DesktopStep::PrepareSharedAssets => prepare_shared_assets_step(),
        DesktopStep::RestoreSharedAssets => restore_shared_assets_step(),
        DesktopStep::SplitModules => split_modules_step(),
        DesktopStep::PackModules => pack_modules_step(),
        DesktopStep::StripShellRenderer => strip_shell_renderer_step(),
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
        DesktopStep::BuildAppimageUpdateFeed => crate::appimage::build_update_feed_step(),
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
        DesktopStep::StageHandoff => stage_handoff_step(),
        DesktopStep::BuildPayload => build_payload_step(),
        DesktopStep::BuildModuleManifest => build_module_manifest_step(),
        DesktopStep::PrepareReleaseAssets => prepare_release_assets_step(),
        DesktopStep::BuildSummary => build_summary_step(),
        DesktopStep::CheckShellDrift => check_shell_drift_step().await,
        DesktopStep::BuildModulesOnlyManifest => build_modules_only_manifest_step(),
        DesktopStep::PrepareModulesReleaseAssets => prepare_modules_release_assets_step(),
    }
}

fn calver_env_from_process() -> CalverEnv {
    CalverEnv {
        build_version: trim_option(env::var("BUILD_VERSION").ok()),
        fluxer_build_version: trim_option(env::var("FLUXER_BUILD_VERSION").ok()),
        fluxer_build_date: trim_option(env::var("FLUXER_BUILD_DATE").ok()),
    }
}

fn set_metadata_step(channel: &str) -> Result<()> {
    let version = resolve_calver(&calver_env_from_process(), Utc::now())?;
    let pub_date = Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string();
    let build_channel = if channel == "canary" {
        "canary"
    } else {
        "stable"
    };
    let source_sha = resolve_source_sha()?;

    append_github_output(&[
        ("version", version.as_str()),
        ("pub_date", pub_date.as_str()),
        ("channel", channel),
        ("build_channel", build_channel),
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

const BUILD_CHANNELS: &[&str] = &["stable", "canary", "development"];

pub(crate) fn write_build_channel_file(root: &Path, channel: &str) -> Result<()> {
    ensure!(
        BUILD_CHANNELS.contains(&channel),
        "Invalid BUILD_CHANNEL: {channel}. Must be one of: {}.",
        BUILD_CHANNELS.join(", ")
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
    let union = BUILD_CHANNELS
        .iter()
        .map(|value| format!("'{value}'"))
        .collect::<Vec<_>>()
        .join(" | ");
    format!(
        "// SPDX-License-Identifier: AGPL-3.0-or-later\n\n\
export type BuildChannel = {union};\n\n\
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
        "macos-universal",
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
        "macos" => flag(&args.skip_macos, "SKIP_MACOS"),
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
        "{{\"platform\":\"{}\",\"arch\":\"{}\",\"os\":\"{}\",\"electron_arch\":\"{}\"}}",
        platform.platform, platform.arch, platform.os, platform.electron_arch
    )
}

fn workspace_dir() -> PathBuf {
    env::var("GITHUB_WORKSPACE")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("."))
}

fn workdir() -> PathBuf {
    env::var("WORKDIR")
        .map(|value| workdir_path(&value))
        .unwrap_or_else(|_| workspace_dir())
}

fn workdir_path(value: &str) -> PathBuf {
    let bytes = value.as_bytes();
    if bytes.len() == 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':' {
        return PathBuf::from(format!("{value}\\"));
    }
    PathBuf::from(value)
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

    append_github_env(&[
        ("WORKDIR", "W:"),
        ("TEMP", r"C:\t"),
        ("TMP", r"C:\t"),
        ("ELECTRON_BUILDER_CACHE", r"C:\ebcache"),
        (
            "PNPM_CONFIG_STORE_DIR",
            store_dir.to_string_lossy().as_ref(),
        ),
        (
            "pnpm_config_store_dir",
            store_dir.to_string_lossy().as_ref(),
        ),
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
            "PNPM_CONFIG_STORE_DIR",
            store_dir.to_string_lossy().to_string(),
        ));
        env_pairs.push((
            "pnpm_config_store_dir",
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

fn setup_pnpm_step() -> Result<()> {
    let npm = npm_program()?;
    let pnpm_package = format!("pnpm@{PNPM_VERSION}");
    run_command(CommandSpec::new(npm.clone()).args(["install", "--global", &pnpm_package]))?;

    let npm_prefix = output_text(CommandSpec::new(npm).args(["prefix", "--global"]))
        .context("Failed to resolve global npm prefix after installing pnpm")?;
    let npm_bin = if cfg!(windows) {
        PathBuf::from(npm_prefix)
    } else {
        PathBuf::from(npm_prefix).join("bin")
    };
    append_github_path(&npm_bin)?;

    for file_name in ["pnpm.cmd", "pnpm.exe", "pnpm"] {
        let candidate = npm_bin.join(file_name);
        if candidate.exists() {
            return run_command(CommandSpec::new(candidate.into_os_string()).arg("--version"));
        }
    }

    bail!(
        "pnpm not found in {} after installing {pnpm_package}",
        npm_bin.display()
    )
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

async fn install_linux_deps_step() -> Result<()> {
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
        "cmake",
        "meson",
        "ninja-build",
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
        "libcbor-dev",
        "libssl-dev",
        "zlib1g-dev",
        "pkg-config",
        "libegl-dev",
        "libclang-dev",
        "clang",
        "libpulse-dev",
    ])?;
    install_linux_pipewire_headers().await?;
    install_linux_libfido2().await?;
    run_command(CommandSpec::new("sudo").args(["gem", "install", "--no-document", "fpm"]))
}

async fn install_linux_pipewire_headers() -> Result<()> {
    let temp = TempDir::new().context("Failed to create PipeWire header build directory")?;
    let archive_name = format!("pipewire-{LINUX_PIPEWIRE_VERSION}.tar.gz");
    let archive_path = temp.path().join(&archive_name);
    let source_dir = temp.path().join("source");
    let build_dir = temp.path().join("build");
    let staging_dir = temp.path().join("stage");
    let source_url = format!(
        "https://gitlab.freedesktop.org/pipewire/pipewire/-/archive/{LINUX_PIPEWIRE_VERSION}/{archive_name}"
    );
    let multiarch = linux_multiarch()?;
    let install_library_dir = Path::new("/usr/local/lib").join(&multiarch);
    let install_pkgconfig_dir = install_library_dir.join("pkgconfig");
    let libdir_arg = format!("--libdir=lib/{multiarch}");
    let system_pipewire_library = fs::canonicalize(linux_loader_path("libpipewire-0.3.so.0")?)
        .context("Failed to resolve the system PipeWire library")?;
    ensure!(
        !system_pipewire_library.starts_with("/usr/local"),
        "Expected the system PipeWire runtime, got {}",
        system_pipewire_library.display()
    );
    let install_include_dir = Path::new("/usr/local/include");
    if linux_pipewire_header_overlay_present(install_include_dir)? {
        verify_linux_pipewire_header_overlay(
            install_include_dir,
            &install_library_dir,
            &install_pkgconfig_dir,
            &system_pipewire_library,
        )
        .with_context(|| {
            format!(
                "The PipeWire header overlay under {} is not the expected {LINUX_PIPEWIRE_VERSION} install",
                install_include_dir.display()
            )
        })?;
        println!(
            "PipeWire {LINUX_PIPEWIRE_VERSION} header overlay already installed for {}, skipping.",
            system_pipewire_library.display()
        );
        return Ok(());
    }

    download_file(&source_url, &archive_path).await?;
    let archive_sha256 = sha256_file(&archive_path)?;
    ensure!(
        archive_sha256 == LINUX_PIPEWIRE_SOURCE_SHA256,
        "PipeWire source checksum mismatch: expected {}, got {}",
        LINUX_PIPEWIRE_SOURCE_SHA256,
        archive_sha256
    );

    fs::create_dir_all(&source_dir)
        .with_context(|| format!("Failed to create {}", source_dir.display()))?;
    run_command(CommandSpec::new("tar").args([
        "-xzf",
        archive_path.to_string_lossy().as_ref(),
        "-C",
        source_dir.to_string_lossy().as_ref(),
        "--strip-components=1",
    ]))?;
    run_command(CommandSpec::new("meson").args([
        "setup",
        build_dir.to_string_lossy().as_ref(),
        source_dir.to_string_lossy().as_ref(),
        "--buildtype=release",
        "--default-library=shared",
        "--prefix=/usr/local",
        &libdir_arg,
        "--auto-features=disabled",
        "-Dexamples=disabled",
        "-Dtests=disabled",
        "-Dinstalled_tests=disabled",
        "-Ddocs=disabled",
        "-Dman=disabled",
        "-Dspa-plugins=enabled",
        "-Dpipewire-alsa=disabled",
        "-Dpipewire-jack=disabled",
        "-Dpipewire-v4l2=disabled",
        "-Dsystemd=disabled",
        "-Ddbus=disabled",
        "-Dflatpak=disabled",
        "-Dsession-managers=[]",
        "-Dlegacy-rtkit=false",
    ]))?;
    run_command(CommandSpec::new("meson").args([
        "compile",
        "-C",
        build_dir.to_string_lossy().as_ref(),
    ]))?;
    run_command(
        CommandSpec::new("meson")
            .args([
                "install",
                "-C",
                build_dir.to_string_lossy().as_ref(),
                "--no-rebuild",
            ])
            .env("DESTDIR", staging_dir.as_os_str()),
    )?;

    let staged_prefix = staging_dir.join("usr/local");
    let staged_pipewire_headers = staged_prefix
        .join("include")
        .join(LINUX_PIPEWIRE_HEADER_DIR);
    let staged_spa_headers = staged_prefix.join("include").join(LINUX_SPA_HEADER_DIR);
    let staged_pkgconfig_dir = staged_prefix.join(format!("lib/{multiarch}/pkgconfig"));
    run_command(CommandSpec::new("sudo").args([
        "install",
        "-d",
        install_include_dir.to_string_lossy().as_ref(),
        install_pkgconfig_dir.to_string_lossy().as_ref(),
    ]))?;
    run_command(CommandSpec::new("sudo").args([
        "cp",
        "-a",
        staged_pipewire_headers.to_string_lossy().as_ref(),
        staged_spa_headers.to_string_lossy().as_ref(),
        install_include_dir.to_string_lossy().as_ref(),
    ]))?;
    for pkgconfig_name in ["libpipewire-0.3.pc", "libspa-0.2.pc"] {
        run_command(
            CommandSpec::new("sudo").args([
                "install",
                "-m",
                "0644",
                staged_pkgconfig_dir
                    .join(pkgconfig_name)
                    .to_string_lossy()
                    .as_ref(),
                install_pkgconfig_dir
                    .join(pkgconfig_name)
                    .to_string_lossy()
                    .as_ref(),
            ]),
        )?;
    }

    verify_linux_pipewire_header_overlay(
        install_include_dir,
        &install_library_dir,
        &install_pkgconfig_dir,
        &system_pipewire_library,
    )?;

    println!(
        "Installed PipeWire {LINUX_PIPEWIRE_VERSION} headers for {}.",
        system_pipewire_library.display()
    );
    Ok(())
}

fn linux_pipewire_header_overlay_present(install_include_dir: &Path) -> Result<bool> {
    let pipewire_headers = install_include_dir.join(LINUX_PIPEWIRE_HEADER_DIR);
    let spa_headers = install_include_dir.join(LINUX_SPA_HEADER_DIR);
    match (pipewire_headers.exists(), spa_headers.exists()) {
        (false, false) => Ok(false),
        (true, true) => Ok(true),
        (has_pipewire, _) => {
            let (present, missing) = if has_pipewire {
                (pipewire_headers, spa_headers)
            } else {
                (spa_headers, pipewire_headers)
            };
            bail!(
                "PipeWire header overlay is partially installed: {} exists but {} does not",
                present.display(),
                missing.display()
            )
        }
    }
}

fn verify_linux_pipewire_header_overlay(
    install_include_dir: &Path,
    install_library_dir: &Path,
    install_pkgconfig_dir: &Path,
    system_pipewire_library: &Path,
) -> Result<()> {
    let installed_version =
        output_text(CommandSpec::new("pkg-config").args(["--modversion", "libpipewire-0.3"]))?;
    ensure!(
        installed_version == LINUX_PIPEWIRE_VERSION,
        "Expected PipeWire headers {}, got {}",
        LINUX_PIPEWIRE_VERSION,
        installed_version
    );
    for (package, expected_include_flag) in [
        ("libpipewire-0.3", "-I/usr/local/include/pipewire-0.3"),
        ("libspa-0.2", "-I/usr/local/include/spa-0.2"),
    ] {
        let pkgconfig_dir =
            output_text(CommandSpec::new("pkg-config").args(["--variable=pcfiledir", package]))?;
        ensure!(
            Path::new(&pkgconfig_dir) == install_pkgconfig_dir,
            "Expected {package} metadata in {}, got {}",
            install_pkgconfig_dir.display(),
            pkgconfig_dir
        );
        let include_dir =
            output_text(CommandSpec::new("pkg-config").args(["--variable=includedir", package]))?;
        ensure!(
            Path::new(&include_dir) == install_include_dir,
            "Expected {package} headers in {}, got {}",
            install_include_dir.display(),
            include_dir
        );
        let include_flags =
            output_text(CommandSpec::new("pkg-config").args(["--cflags-only-I", package]))?;
        ensure!(
            include_flags
                .split_whitespace()
                .any(|flag| flag == expected_include_flag),
            "Expected {package} include flag {expected_include_flag}, got {include_flags}"
        );
    }
    let install_pipewire_headers = install_include_dir.join(LINUX_PIPEWIRE_HEADER_DIR);
    let install_spa_headers = install_include_dir.join(LINUX_SPA_HEADER_DIR);
    ensure!(
        install_pipewire_headers
            .join("pipewire/pipewire.h")
            .is_file()
            && install_spa_headers.join("spa/buffer/meta.h").is_file()
            && install_spa_headers.join("spa/param/video/raw.h").is_file(),
        "PipeWire {} header overlay is incomplete",
        LINUX_PIPEWIRE_VERSION
    );
    let mut local_pipewire_library = None;
    for entry in fs::read_dir(install_library_dir)
        .with_context(|| format!("Failed to read {}", install_library_dir.display()))?
    {
        let entry = entry
            .with_context(|| format!("Failed to inspect {}", install_library_dir.display()))?;
        if entry
            .file_name()
            .to_string_lossy()
            .starts_with("libpipewire-0.3.")
        {
            local_pipewire_library = Some(entry.path());
            break;
        }
    }
    if let Some(path) = local_pipewire_library {
        bail!(
            "PipeWire header overlay found unexpected library {}",
            path.display()
        );
    }
    let loaded_pipewire_library = fs::canonicalize(linux_loader_path("libpipewire-0.3.so.0")?)
        .context("Failed to resolve the selected PipeWire library")?;
    ensure!(
        loaded_pipewire_library == system_pipewire_library,
        "PipeWire header overlay changed the runtime from {} to {}",
        system_pipewire_library.display(),
        loaded_pipewire_library.display()
    );
    Ok(())
}

async fn install_linux_libfido2() -> Result<()> {
    let temp = TempDir::new().context("Failed to create libfido2 build directory")?;
    let archive_name = format!("libfido2-{LINUX_LIBFIDO2_VERSION}.tar.gz");
    let archive_path = temp.path().join(&archive_name);
    let source_dir = temp.path().join("source");
    let build_dir = temp.path().join("build");
    let source_url = format!("https://developers.yubico.com/libfido2/Releases/{archive_name}");
    let multiarch = linux_multiarch()?;
    let install_library_dir = Path::new("/usr/local/lib").join(&multiarch);
    let install_library_arg = format!("-DCMAKE_INSTALL_LIBDIR=lib/{multiarch}");

    download_file(&source_url, &archive_path).await?;
    let archive_sha256 = sha256_file(&archive_path)?;
    ensure!(
        archive_sha256 == LINUX_LIBFIDO2_SOURCE_SHA256,
        "libfido2 source checksum mismatch: expected {}, got {}",
        LINUX_LIBFIDO2_SOURCE_SHA256,
        archive_sha256
    );

    fs::create_dir_all(&source_dir)
        .with_context(|| format!("Failed to create {}", source_dir.display()))?;
    run_command(CommandSpec::new("tar").args([
        "-xzf",
        archive_path.to_string_lossy().as_ref(),
        "-C",
        source_dir.to_string_lossy().as_ref(),
        "--strip-components=1",
    ]))?;
    run_command(CommandSpec::new("cmake").args([
        "-S",
        source_dir.to_string_lossy().as_ref(),
        "-B",
        build_dir.to_string_lossy().as_ref(),
        "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=/usr/local",
        &install_library_arg,
        "-DBUILD_SHARED_LIBS=ON",
        "-DBUILD_STATIC_LIBS=OFF",
        "-DBUILD_MANPAGES=OFF",
        "-DBUILD_EXAMPLES=OFF",
        "-DBUILD_TOOLS=OFF",
        "-DBUILD_TESTS=OFF",
        "-DFUZZ=OFF",
        "-DNFC_LINUX=OFF",
        "-DUSE_PCSC=OFF",
        "-DUSE_HIDAPI=OFF",
        "-DUSE_WINHELLO=OFF",
    ]))?;
    run_command(CommandSpec::new("cmake").args([
        "--build",
        build_dir.to_string_lossy().as_ref(),
        "--config",
        "Release",
        "--parallel",
    ]))?;
    run_command(CommandSpec::new("sudo").args([
        "cmake",
        "--install",
        build_dir.to_string_lossy().as_ref(),
        "--config",
        "Release",
    ]))?;
    run_command(CommandSpec::new("sudo").arg("ldconfig"))?;

    let installed_version =
        output_text(CommandSpec::new("pkg-config").args(["--modversion", "libfido2"]))?;
    ensure!(
        installed_version.trim() == LINUX_LIBFIDO2_VERSION,
        "Expected libfido2 {}, got {}",
        LINUX_LIBFIDO2_VERSION,
        installed_version.trim()
    );
    let include_dir =
        output_text(CommandSpec::new("pkg-config").args(["--variable=includedir", "libfido2"]))?;
    ensure!(
        Path::new(include_dir.trim()).join("fido/es384.h").is_file(),
        "libfido2 {} did not install fido/es384.h",
        LINUX_LIBFIDO2_VERSION
    );
    let library_dir =
        output_text(CommandSpec::new("pkg-config").args(["--variable=libdir", "libfido2"]))?;
    ensure!(
        Path::new(library_dir.trim()) == install_library_dir,
        "Expected libfido2 library directory {}, got {}",
        install_library_dir.display(),
        library_dir.trim()
    );
    let installed_soname = install_library_dir.join("libfido2.so.1");
    ensure!(
        installed_soname.exists(),
        "libfido2 {} did not install {}",
        LINUX_LIBFIDO2_VERSION,
        installed_soname.display()
    );
    let installed_library = fs::canonicalize(&installed_soname)
        .with_context(|| format!("Failed to resolve {}", installed_soname.display()))?;
    let loader_path = linux_loader_path("libfido2.so.1")?;
    let loaded_library = fs::canonicalize(&loader_path)
        .with_context(|| format!("Failed to resolve {}", loader_path.display()))?;
    ensure!(
        loaded_library == installed_library,
        "Expected the loader to select {}, got {}",
        installed_library.display(),
        loaded_library.display()
    );

    println!("Installed libfido2 {LINUX_LIBFIDO2_VERSION} from verified source.");
    Ok(())
}

fn linux_multiarch() -> Result<String> {
    let multiarch = output_text(CommandSpec::new("gcc").arg("-print-multiarch"))?;
    ensure!(
        !multiarch.is_empty()
            && multiarch
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-' || byte == b'_'),
        "gcc returned invalid multiarch tuple: {multiarch:?}"
    );
    Ok(multiarch)
}

fn linux_loader_path(library_name: &str) -> Result<PathBuf> {
    let loader_cache = output_text(CommandSpec::new("ldconfig").arg("-p"))?;
    loader_cache
        .lines()
        .find_map(|line| {
            let (library, path) = line.trim().split_once("=>")?;
            (library.split_whitespace().next() == Some(library_name))
                .then(|| PathBuf::from(path.trim()))
        })
        .with_context(|| format!("ldconfig did not resolve {library_name}"))
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
    let program_files_x86 = env::var_os("ProgramFiles(x86)")
        .map(PathBuf::from)
        .ok_or_else(|| anyhow!("ProgramFiles(x86) is not set on the Windows runner"))?;
    let installer_dir = program_files_x86
        .join("Microsoft Visual Studio")
        .join("Installer");
    let installer = installer_dir.join("setup.exe");
    let vswhere = installer_dir.join("vswhere.exe");
    ensure!(
        vswhere.is_file(),
        "Visual Studio locator not found: {}",
        vswhere.display()
    );

    let install_path = resolve_visual_studio_install_path(&vswhere)?;
    if let Some(linker) = find_msvc_arm64_linker(&install_path)? {
        println!("ARM64 cross link.exe: {}", linker.display());
        return Ok(());
    }
    ensure!(
        installer.is_file(),
        "Visual Studio installer not found: {}",
        installer.display()
    );

    run_command(CommandSpec::new(&installer).args([
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

    let linker = find_msvc_arm64_linker(&install_path)?.ok_or_else(|| {
        anyhow!(
            "ARM64 cross-build tools were not installed under {}\\VC\\Tools\\MSVC\\*\\bin\\HostX64\\arm64",
            install_path.display()
        )
    })?;
    println!("ARM64 cross link.exe: {}", linker.display());
    Ok(())
}

fn resolve_visual_studio_install_path(vswhere: &Path) -> Result<PathBuf> {
    let output = output_text(CommandSpec::new(vswhere).args([
        "-products",
        "*",
        "-latest",
        "-prerelease",
        "-property",
        "installationPath",
    ]))?;
    let paths = output
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>();
    ensure!(
        paths.len() == 1,
        "vswhere returned {} Visual Studio installation paths; expected exactly one",
        paths.len()
    );
    let install_path = PathBuf::from(paths[0]);
    ensure!(
        install_path.is_dir(),
        "vswhere returned a missing Visual Studio installation: {}",
        install_path.display()
    );
    println!("Visual Studio installation: {}", install_path.display());
    Ok(install_path)
}

fn find_msvc_arm64_linker(install_path: &Path) -> Result<Option<PathBuf>> {
    let msvc_root = install_path.join("VC").join("Tools").join("MSVC");
    if !msvc_root.is_dir() {
        return Ok(None);
    }
    let mut linkers = Vec::new();
    for entry in fs::read_dir(&msvc_root)
        .with_context(|| format!("Failed to read {}", msvc_root.display()))?
    {
        let candidate = entry?
            .path()
            .join("bin")
            .join("HostX64")
            .join("arm64")
            .join("link.exe");
        if candidate.is_file() {
            linkers.push(candidate);
        }
    }
    linkers.sort();
    Ok(linkers.pop())
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
            .args(["build", "--use-shared-renderer"])
            .env("NODE_ENV", "production")
            .env("FLUXER_DESKTOP_PRODUCTION", "true")
            .env(DESKTOP_MODULES_ENV, "1"),
    )
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopSharedAssetFile {
    path: String,
    sha256: String,
    bytes: u64,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopSharedAssetManifest {
    build_version: String,
    release_channel: String,
    source_sha: String,
    files: Vec<DesktopSharedAssetFile>,
}

fn desktop_renderer_dir() -> PathBuf {
    workdir()
        .join("fluxer_desktop")
        .join("dist")
        .join("renderer")
}

fn desktop_shared_assets_dir() -> PathBuf {
    workdir().join(DESKTOP_SHARED_ASSETS_DIR_NAME)
}

fn build_shared_assets_step() -> Result<()> {
    run_command(
        pnpm_command()?
            .args(["exec", "node", "scripts/build.mjs", "--shared-assets"])
            .env("NODE_ENV", "production")
            .env("FLUXER_DESKTOP_PRODUCTION", "true"),
    )
}

fn prepare_shared_assets_step() -> Result<()> {
    let renderer_dir = desktop_renderer_dir();
    ensure_renderer_assets_ready(&renderer_dir)?;
    let staging_dir = desktop_shared_assets_dir();
    remove_dir_if_exists(&staging_dir)?;
    let payload_dir = staging_dir.join(DESKTOP_SHARED_ASSETS_PAYLOAD_DIR_NAME);
    copy_dir_contents(&renderer_dir, &payload_dir)?;
    let manifest = build_shared_asset_manifest(&payload_dir)?;
    write_json_pretty(
        &staging_dir.join(DESKTOP_SHARED_ASSETS_MANIFEST_NAME),
        &manifest,
    )?;
    println!(
        "Staged {} shared renderer file(s) into {}",
        manifest.files.len(),
        staging_dir.display()
    );
    Ok(())
}

fn restore_shared_assets_step() -> Result<()> {
    let staging_dir = desktop_shared_assets_dir();
    let manifest_path = staging_dir.join(DESKTOP_SHARED_ASSETS_MANIFEST_NAME);
    let manifest_bytes = fs::read(&manifest_path)
        .with_context(|| format!("Failed to read {}", manifest_path.display()))?;
    let manifest: DesktopSharedAssetManifest = serde_json::from_slice(&manifest_bytes)
        .with_context(|| format!("Failed to parse {}", manifest_path.display()))?;
    let build_version = require_env("BUILD_VERSION")?;
    let release_channel = require_env("BUILD_CHANNEL")?;
    let source_sha = require_env("SOURCE_SHA")?;
    ensure!(
        manifest.build_version == build_version,
        "Shared renderer assets were built for version {}, expected {build_version}",
        manifest.build_version
    );
    ensure!(
        manifest.release_channel == release_channel,
        "Shared renderer assets were built for channel {}, expected {release_channel}",
        manifest.release_channel
    );
    ensure!(
        manifest.source_sha == source_sha,
        "Shared renderer assets were built from {}, expected {source_sha}",
        manifest.source_sha
    );
    let payload_dir = staging_dir.join(DESKTOP_SHARED_ASSETS_PAYLOAD_DIR_NAME);
    verify_shared_asset_payload(&payload_dir, &manifest)?;
    let renderer_dir = desktop_renderer_dir();
    remove_dir_if_exists(&renderer_dir)?;
    copy_dir_contents(&payload_dir, &renderer_dir)?;
    ensure_renderer_assets_ready(&renderer_dir)?;
    println!(
        "Restored {} shared renderer file(s) into {}",
        manifest.files.len(),
        renderer_dir.display()
    );
    Ok(())
}

fn build_shared_asset_manifest(root: &Path) -> Result<DesktopSharedAssetManifest> {
    let files = collect_files(root)?;
    ensure!(
        files.len() <= DESKTOP_SHARED_ASSET_FILE_LIMIT,
        "Shared renderer payload has {} files, above the {DESKTOP_SHARED_ASSET_FILE_LIMIT} file limit",
        files.len()
    );
    let mut entries = Vec::with_capacity(files.len());
    for file in files {
        let relative = file.strip_prefix(root)?;
        let metadata =
            fs::metadata(&file).with_context(|| format!("Failed to stat {}", file.display()))?;
        entries.push(DesktopSharedAssetFile {
            path: shared_asset_relative_path(relative),
            sha256: sha256_file(&file)?,
            bytes: metadata.len(),
        });
    }
    Ok(DesktopSharedAssetManifest {
        build_version: require_env("BUILD_VERSION")?,
        release_channel: require_env("BUILD_CHANNEL")?,
        source_sha: require_env("SOURCE_SHA")?,
        files: entries,
    })
}

fn shared_asset_relative_path(relative: &Path) -> String {
    relative
        .components()
        .map(|component| component.as_os_str().to_string_lossy().into_owned())
        .collect::<Vec<_>>()
        .join("/")
}

fn resolve_shared_asset_path(root: &Path, relative: &str) -> Result<PathBuf> {
    let mut path = root.to_path_buf();
    for segment in relative.split('/') {
        ensure!(
            !segment.is_empty() && segment != "." && segment != "..",
            "Shared renderer manifest contains an unsafe path: {relative}"
        );
        path.push(segment);
    }
    Ok(path)
}

fn verify_shared_asset_payload(root: &Path, manifest: &DesktopSharedAssetManifest) -> Result<()> {
    let present = collect_files(root)?;
    ensure!(
        present.len() == manifest.files.len(),
        "Shared renderer payload has {} files, the manifest lists {}",
        present.len(),
        manifest.files.len()
    );
    for entry in &manifest.files {
        let path = resolve_shared_asset_path(root, &entry.path)?;
        let metadata = fs::metadata(&path)
            .with_context(|| format!("Missing shared renderer file {}", path.display()))?;
        ensure!(
            metadata.len() == entry.bytes,
            "Shared renderer file {} is {} bytes, the manifest lists {}",
            entry.path,
            metadata.len(),
            entry.bytes
        );
        let digest = sha256_file(&path)?;
        ensure!(
            digest == entry.sha256,
            "Shared renderer file {} hashes to {digest}, the manifest lists {}",
            entry.path,
            entry.sha256
        );
    }
    Ok(())
}

fn ensure_renderer_assets_ready(root: &Path) -> Result<()> {
    ensure!(
        root.join("index.html").is_file(),
        "Missing renderer index.html in {}",
        root.display()
    );
    ensure!(
        root.join("assets").is_dir(),
        "Missing renderer assets directory in {}",
        root.display()
    );
    ensure!(
        !root.join("sw.js").exists(),
        "The desktop renderer bundle must not contain sw.js ({})",
        root.display()
    );
    Ok(())
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopModuleManifest {
    module: String,
    build_version: String,
    release_channel: String,
    source_sha: String,
    files: Vec<DesktopSharedAssetFile>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopModuleClassificationFile {
    path: String,
    module: String,
    rule: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopModuleClassificationSummary {
    module: String,
    files: usize,
    bytes: u64,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopModuleClassification {
    build_version: String,
    release_channel: String,
    source_sha: String,
    modules: Vec<DesktopModuleClassificationSummary>,
    files: Vec<DesktopModuleClassificationFile>,
}

fn desktop_modules_dir() -> PathBuf {
    workdir().join(DESKTOP_MODULES_DIR_NAME)
}

pub(crate) fn is_desktop_module_name(value: &str) -> bool {
    if value.is_empty() || value.len() > DESKTOP_MODULE_NAME_MAX_LENGTH {
        return false;
    }
    let mut characters = value.chars();
    let Some(first) = characters.next() else {
        return false;
    };
    first.is_ascii_lowercase()
        && characters
            .all(|value| value.is_ascii_lowercase() || value.is_ascii_digit() || value == '_')
}

fn is_desktop_source_map(relative: &str) -> bool {
    Path::new(relative)
        .extension()
        .and_then(|value| value.to_str())
        .is_some_and(|value| value.eq_ignore_ascii_case(DESKTOP_SOURCEMAP_EXTENSION))
}

fn desktop_module_for_relative_path(relative: &str) -> Result<Option<&str>> {
    let Some(rest) = relative.strip_prefix(&format!("{DESKTOP_MODULE_ASSETS_DIR_NAME}/")) else {
        return Ok(None);
    };
    let Some((segment, name)) = rest.split_once('/') else {
        return Ok(None);
    };
    ensure!(
        !name.contains('/'),
        "Renderer file {relative} nests more than one directory under {DESKTOP_MODULE_ASSETS_DIR_NAME}/, the owning module segment must be the only one"
    );
    ensure!(
        segment != DESKTOP_MODULE_ASSETS_DIR_NAME,
        "Renderer file {relative} names its module {DESKTOP_MODULE_ASSETS_DIR_NAME}, which nests a second {DESKTOP_MODULE_ASSETS_DIR_NAME} segment and collapses every proxied asset path"
    );
    ensure!(
        segment != DESKTOP_RENDERER_MODULE,
        "Renderer file {relative} names {DESKTOP_RENDERER_MODULE} explicitly, a file with no module segment already belongs to it"
    );
    ensure!(
        is_desktop_module_name(segment),
        "Renderer file {relative} sits under {segment}, which is not a usable desktop module name"
    );
    Ok(Some(segment))
}

fn expected_desktop_modules(modules_dir: &Path) -> Result<BTreeSet<String>> {
    let path = modules_dir.join(DESKTOP_MODULE_CLASSIFICATION_NAME);
    let bytes = fs::read(&path).with_context(|| {
        format!(
            "Failed to read {}, run the split_modules step first",
            path.display()
        )
    })?;
    let classification: DesktopModuleClassification = serde_json::from_slice(&bytes)
        .with_context(|| format!("Failed to parse {}", path.display()))?;
    let modules = classification
        .modules
        .iter()
        .map(|summary| summary.module.clone())
        .collect::<BTreeSet<_>>();
    ensure!(
        modules.contains(DESKTOP_RENDERER_MODULE),
        "{} lists no {DESKTOP_RENDERER_MODULE}",
        path.display()
    );
    Ok(modules)
}

fn desktop_module_brotli_quality(manifest: &DesktopModuleManifest) -> u32 {
    if manifest.files.is_empty() {
        return DESKTOP_MODULE_BROTLI_QUALITY;
    }
    let precompressed = manifest.files.iter().all(|entry| {
        Path::new(&entry.path)
            .extension()
            .and_then(OsStr::to_str)
            .is_some_and(|extension| {
                DESKTOP_PRECOMPRESSED_EXTENSIONS.contains(&extension.to_ascii_lowercase().as_str())
            })
    });
    if precompressed {
        DESKTOP_PRECOMPRESSED_MODULE_BROTLI_QUALITY
    } else {
        DESKTOP_MODULE_BROTLI_QUALITY
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DesktopClassifiedRendererFile {
    source: PathBuf,
    module: String,
    rule: &'static str,
    entry: DesktopSharedAssetFile,
}

fn classify_renderer_files(renderer_dir: &Path) -> Result<Vec<DesktopClassifiedRendererFile>> {
    let files = collect_files(renderer_dir)?;
    ensure!(
        files.len() <= DESKTOP_SHARED_ASSET_FILE_LIMIT,
        "Renderer payload has {} files, above the {DESKTOP_SHARED_ASSET_FILE_LIMIT} file limit",
        files.len()
    );
    let mut classified = Vec::with_capacity(files.len());
    for file in files {
        let relative = shared_asset_relative_path(file.strip_prefix(renderer_dir)?);
        let metadata =
            fs::metadata(&file).with_context(|| format!("Failed to stat {}", file.display()))?;
        let sha256 = sha256_file(&file)?;
        let (module, rule) = if is_desktop_source_map(&relative) {
            (DESKTOP_SOURCEMAP_MODULE.to_string(), DESKTOP_SOURCEMAP_RULE)
        } else {
            match desktop_module_for_relative_path(&relative)? {
                Some(segment) => (segment.to_string(), DESKTOP_MODULE_ASSET_SEGMENT_RULE),
                None => (
                    DESKTOP_RENDERER_MODULE.to_string(),
                    DESKTOP_RENDERER_REMAINDER_RULE,
                ),
            }
        };
        classified.push(DesktopClassifiedRendererFile {
            source: file,
            module,
            rule,
            entry: DesktopSharedAssetFile {
                path: relative,
                sha256,
                bytes: metadata.len(),
            },
        });
    }
    Ok(classified)
}

fn strip_renderer_tree_owned_by_modules(
    renderer_dir: &Path,
) -> Result<BTreeMap<String, Vec<DesktopSharedAssetFile>>> {
    ensure_renderer_assets_ready(renderer_dir)?;
    let mut removed: BTreeMap<String, Vec<DesktopSharedAssetFile>> = BTreeMap::new();
    for file in classify_renderer_files(renderer_dir)? {
        fs::remove_file(&file.source)
            .with_context(|| format!("Failed to remove {}", file.source.display()))?;
        removed.entry(file.module).or_default().push(file.entry);
    }
    ensure!(
        removed
            .get(DESKTOP_RENDERER_MODULE)
            .is_some_and(|entries| !entries.is_empty()),
        "{DESKTOP_RENDERER_MODULE} claimed no file in {}, refusing to publish a shell whose renderer module is empty",
        renderer_dir.display()
    );
    let left = count_files(renderer_dir)?;
    ensure!(
        left == 0,
        "{left} renderer file(s) survived the strip in {}, every packed file must belong to a module",
        renderer_dir.display()
    );
    remove_empty_dirs_below(renderer_dir)?;
    let surviving_entries = fs::read_dir(renderer_dir)
        .with_context(|| format!("Failed to read {}", renderer_dir.display()))?
        .count();
    ensure!(
        surviving_entries == 0,
        "{surviving_entries} entries survived the strip in {}, the packed renderer directory must be left empty",
        renderer_dir.display()
    );
    Ok(removed)
}

fn strip_shell_renderer_step() -> Result<()> {
    let renderer_dir = desktop_renderer_dir();
    let removed = strip_renderer_tree_owned_by_modules(&renderer_dir)?;
    let mut total_files = 0usize;
    let mut total_bytes = 0u64;
    for (module, entries) in &removed {
        let bytes = entries.iter().map(|entry| entry.bytes).sum::<u64>();
        total_files += entries.len();
        total_bytes += bytes;
        println!(
            "Removed {} file(s), {bytes} bytes owned by {module}",
            entries.len()
        );
    }
    println!(
        "Stripped {total_files} module file(s), {total_bytes} bytes from {}, the shell now ships no renderer",
        renderer_dir.display()
    );
    Ok(())
}

fn split_modules_step() -> Result<()> {
    let renderer_dir = desktop_renderer_dir();
    ensure_renderer_assets_ready(&renderer_dir)?;
    let build_version = require_env("BUILD_VERSION")?;
    let release_channel = require_env("BUILD_CHANNEL")?;
    let source_sha = require_env("SOURCE_SHA")?;
    let classified = classify_renderer_files(&renderer_dir)?;
    let file_count = classified.len();
    let mut classification = Vec::with_capacity(file_count);
    let mut modules: BTreeMap<String, Vec<(PathBuf, DesktopSharedAssetFile)>> = BTreeMap::new();
    for file in classified {
        classification.push(DesktopModuleClassificationFile {
            path: file.entry.path.clone(),
            module: file.module.clone(),
            rule: file.rule.to_string(),
        });
        modules
            .entry(file.module)
            .or_default()
            .push((file.source, file.entry));
    }
    ensure!(
        modules
            .get(DESKTOP_RENDERER_MODULE)
            .is_some_and(|entries| !entries.is_empty()),
        "{DESKTOP_RENDERER_MODULE} classified no renderer files"
    );
    let modules_dir = desktop_modules_dir();
    remove_dir_if_exists(&modules_dir)?;
    let mut summaries: Vec<DesktopModuleClassificationSummary> = Vec::with_capacity(modules.len());
    for (module, entries) in &modules {
        let module_dir = modules_dir.join(module);
        let files_dir = module_dir.join(DESKTOP_MODULE_FILES_DIR_NAME);
        for (source, entry) in entries {
            let target = resolve_shared_asset_path(&files_dir, &entry.path)?;
            if let Some(parent) = target.parent() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("Failed to create {}", parent.display()))?;
            }
            fs::copy(source, &target).with_context(|| {
                format!(
                    "Failed to copy {} to {}",
                    source.display(),
                    target.display()
                )
            })?;
        }
        write_json_pretty(
            &module_dir.join(DESKTOP_MODULE_FILE_LIST_NAME),
            &DesktopModuleManifest {
                module: module.clone(),
                build_version: build_version.clone(),
                release_channel: release_channel.clone(),
                source_sha: source_sha.clone(),
                files: entries.iter().map(|(_, entry)| entry.clone()).collect(),
            },
        )?;
        summaries.push(DesktopModuleClassificationSummary {
            module: module.clone(),
            files: entries.len(),
            bytes: entries.iter().map(|(_, entry)| entry.bytes).sum(),
        });
    }
    verify_desktop_module_split(&modules_dir, &renderer_dir)?;
    let dictionaries = write_desktop_dictionary_modules(
        &modules_dir,
        &build_version,
        &release_channel,
        &source_sha,
    )?;
    for dictionary in &dictionaries {
        for entry in &dictionary.files {
            classification.push(DesktopModuleClassificationFile {
                path: entry.path.clone(),
                module: dictionary.module.clone(),
                rule: DESKTOP_SPELLCHECK_DICTIONARY_RULE.to_string(),
            });
        }
        summaries.push(DesktopModuleClassificationSummary {
            module: dictionary.module.clone(),
            files: dictionary.files.len(),
            bytes: dictionary.files.iter().map(|entry| entry.bytes).sum(),
        });
    }
    write_json_pretty(
        &modules_dir.join(DESKTOP_MODULE_CLASSIFICATION_NAME),
        &DesktopModuleClassification {
            build_version,
            release_channel,
            source_sha,
            modules: summaries.clone(),
            files: classification,
        },
    )?;
    for summary in &summaries {
        println!(
            "{} carries {} file(s), {} bytes",
            summary.module, summary.files, summary.bytes
        );
    }
    println!(
        "Split {file_count} renderer file(s) into {} module(s) under {}",
        summaries.len(),
        modules_dir.display()
    );
    Ok(())
}

fn desktop_dictionary_module_name(directory_name: &str) -> Option<String> {
    let (package, version) = directory_name.split_once('@')?;
    if version.is_empty() {
        return None;
    }
    let tag = package.strip_prefix(DESKTOP_DICTIONARY_PACKAGE_PREFIX)?;
    let module = format!(
        "{DESKTOP_DICTIONARY_MODULE_PREFIX}{}",
        tag.replace('-', "_")
    );
    is_desktop_module_name(&module).then_some(module)
}

fn desktop_dictionary_source_root() -> PathBuf {
    workdir()
        .join("fluxer_static")
        .join("desktop")
        .join("spellcheck")
        .join("dictionaries")
}

fn desktop_dictionary_sources() -> Result<Vec<(String, PathBuf)>> {
    let source_root = desktop_dictionary_source_root();
    ensure!(
        source_root.is_dir(),
        "Missing {}, the shipped client resolves every spellcheck dictionary from its own modules",
        source_root.display()
    );
    let mut sources = Vec::new();
    let mut seen = BTreeSet::new();
    for entry in sorted_child_directories(&source_root)? {
        let directory_name = file_name_string(&entry)?;
        let Some(module) = desktop_dictionary_module_name(&directory_name) else {
            continue;
        };
        ensure!(
            seen.insert(module.clone()),
            "Two dictionary directories under {} both resolve to {module}",
            source_root.display()
        );
        for name in DESKTOP_DICTIONARY_FILE_NAMES {
            ensure!(
                entry.join(name).is_file(),
                "Dictionary {directory_name} is missing {name}"
            );
        }
        sources.push((module, entry));
    }
    ensure!(
        !sources.is_empty(),
        "{} holds no spellcheck dictionary, refusing to publish a client whose spellcheck can never load",
        source_root.display()
    );
    Ok(sources)
}

fn write_desktop_dictionary_modules(
    modules_dir: &Path,
    build_version: &str,
    release_channel: &str,
    source_sha: &str,
) -> Result<Vec<DesktopModuleManifest>> {
    let mut manifests = Vec::new();
    for (module, entry) in desktop_dictionary_sources()? {
        let module_dir = modules_dir.join(&module);
        let files_dir = module_dir
            .join(DESKTOP_MODULE_FILES_DIR_NAME)
            .join(DESKTOP_MODULE_ASSETS_DIR_NAME)
            .join(&module);
        fs::create_dir_all(&files_dir)
            .with_context(|| format!("Failed to create {}", files_dir.display()))?;
        let mut files = Vec::with_capacity(DESKTOP_DICTIONARY_FILE_NAMES.len());
        for name in DESKTOP_DICTIONARY_FILE_NAMES {
            let source = entry.join(name);
            let target = files_dir.join(name);
            fs::copy(&source, &target).with_context(|| {
                format!(
                    "Failed to copy {} to {}",
                    source.display(),
                    target.display()
                )
            })?;
            files.push(DesktopSharedAssetFile {
                path: format!("{DESKTOP_MODULE_ASSETS_DIR_NAME}/{module}/{name}"),
                sha256: sha256_file(&source)?,
                bytes: fs::metadata(&source)
                    .with_context(|| format!("Failed to stat {}", source.display()))?
                    .len(),
            });
        }
        let manifest = DesktopModuleManifest {
            module,
            build_version: build_version.to_string(),
            release_channel: release_channel.to_string(),
            source_sha: source_sha.to_string(),
            files,
        };
        write_json_pretty(&module_dir.join(DESKTOP_MODULE_FILE_LIST_NAME), &manifest)?;
        manifests.push(manifest);
    }
    println!("Wrote {} spellcheck dictionary module(s)", manifests.len());
    Ok(manifests)
}

fn read_desktop_module_manifest(module_dir: &Path) -> Result<DesktopModuleManifest> {
    let path = module_dir.join(DESKTOP_MODULE_FILE_LIST_NAME);
    let bytes = fs::read(&path).with_context(|| format!("Failed to read {}", path.display()))?;
    let manifest: DesktopModuleManifest = serde_json::from_slice(&bytes)
        .with_context(|| format!("Failed to parse {}", path.display()))?;
    Ok(manifest)
}

fn desktop_module_dirs(modules_dir: &Path) -> Result<Vec<PathBuf>> {
    let mut dirs = fs::read_dir(modules_dir)
        .with_context(|| format!("Failed to read {}", modules_dir.display()))?
        .collect::<std::result::Result<Vec<_>, _>>()?
        .into_iter()
        .map(|entry| entry.path())
        .filter(|path| path.join(DESKTOP_MODULE_FILE_LIST_NAME).is_file())
        .collect::<Vec<_>>();
    dirs.sort();
    Ok(dirs)
}

fn verify_desktop_module_split(modules_dir: &Path, renderer_dir: &Path) -> Result<()> {
    let mut renderer_files = BTreeSet::new();
    for file in collect_files(renderer_dir)? {
        renderer_files.insert(shared_asset_relative_path(file.strip_prefix(renderer_dir)?));
    }
    let mut owners: BTreeMap<String, String> = BTreeMap::new();
    for module_dir in desktop_module_dirs(modules_dir)? {
        let manifest = read_desktop_module_manifest(&module_dir)?;
        let files_dir = module_dir.join(DESKTOP_MODULE_FILES_DIR_NAME);
        let present = collect_files(&files_dir)?;
        ensure!(
            present.len() == manifest.files.len(),
            "Desktop module {} holds {} file(s), its manifest lists {}",
            manifest.module,
            present.len(),
            manifest.files.len()
        );
        for entry in &manifest.files {
            let path = resolve_shared_asset_path(&files_dir, &entry.path)?;
            let metadata = fs::metadata(&path)
                .with_context(|| format!("Missing desktop module file {}", path.display()))?;
            ensure!(
                metadata.len() == entry.bytes,
                "Desktop module file {} is {} bytes, its manifest lists {}",
                entry.path,
                metadata.len(),
                entry.bytes
            );
            if let Some(previous) = owners.insert(entry.path.clone(), manifest.module.clone()) {
                bail!(
                    "Renderer file {} landed in both {previous} and {}",
                    entry.path,
                    manifest.module
                );
            }
        }
    }
    for relative in &renderer_files {
        ensure!(
            owners.contains_key(relative),
            "Renderer file {relative} landed in no desktop module"
        );
    }
    ensure!(
        owners.len() == renderer_files.len(),
        "The desktop modules hold {} file(s), the renderer has {}",
        owners.len(),
        renderer_files.len()
    );
    let renderer_files_dir = modules_dir
        .join(DESKTOP_RENDERER_MODULE)
        .join(DESKTOP_MODULE_FILES_DIR_NAME);
    ensure_renderer_assets_ready(&renderer_files_dir)?;
    ensure!(
        count_files(&renderer_files_dir.join("assets"))? > 0,
        "{DESKTOP_RENDERER_MODULE} must keep a non-empty assets directory"
    );
    Ok(())
}

fn pack_modules_step() -> Result<()> {
    let modules_dir = desktop_modules_dir();
    ensure!(
        modules_dir.is_dir(),
        "Missing {}, run the split_modules step first",
        modules_dir.display()
    );
    let module_dirs = desktop_module_dirs(&modules_dir)?;
    ensure!(
        !module_dirs.is_empty(),
        "No desktop modules found in {}",
        modules_dir.display()
    );
    let mut jobs = Vec::with_capacity(module_dirs.len());
    for module_dir in module_dirs {
        let manifest = read_desktop_module_manifest(&module_dir)?;
        ensure!(
            module_dir.file_name().and_then(OsStr::to_str) == Some(manifest.module.as_str()),
            "Desktop module directory {} declares module {}",
            module_dir.display(),
            manifest.module
        );
        jobs.push((module_dir, manifest));
    }
    let mut schedule: Vec<usize> = (0..jobs.len()).collect();
    schedule.sort_by_key(|index| {
        std::cmp::Reverse(
            jobs[*index]
                .1
                .files
                .iter()
                .map(|entry| entry.bytes)
                .sum::<u64>(),
        )
    });
    let jobs = &jobs;
    let schedule = &schedule;
    let cursor = AtomicUsize::new(0);
    let workers = thread::available_parallelism()
        .map_or(1, |value| value.get())
        .min(DESKTOP_MODULE_PACK_MAX_THREADS)
        .min(schedule.len());
    let mut packed = thread::scope(|scope| -> Result<Vec<(usize, String)>> {
        let handles = (0..workers)
            .map(|_| {
                scope.spawn(|| -> Result<Vec<(usize, String)>> {
                    let mut lines = Vec::new();
                    loop {
                        let slot = cursor.fetch_add(1, Ordering::Relaxed);
                        let Some(index) = schedule.get(slot).copied() else {
                            return Ok(lines);
                        };
                        let (module_dir, manifest) = &jobs[index];
                        lines.push((index, pack_one_desktop_module(module_dir, manifest)?));
                    }
                })
            })
            .collect::<Vec<_>>();
        let mut packed = Vec::with_capacity(jobs.len());
        for handle in handles {
            let lines = handle
                .join()
                .map_err(|_| anyhow!("A desktop module packing thread panicked"))??;
            packed.extend(lines);
        }
        Ok(packed)
    })?;
    packed.sort_by_key(|(index, _)| *index);
    for (_, line) in packed {
        println!("{line}");
    }
    Ok(())
}

fn pack_one_desktop_module(module_dir: &Path, manifest: &DesktopModuleManifest) -> Result<String> {
    let package_path = module_dir.join(DESKTOP_MODULE_PACKAGE_NAME);
    pack_desktop_module(module_dir, manifest, &package_path)?;
    verify_desktop_module_package(&package_path, manifest)?;
    let digest = sha256_file(&package_path)?;
    let checksum_path = module_dir.join(DESKTOP_MODULE_PACKAGE_CHECKSUM_NAME);
    fs::write(&checksum_path, &digest)
        .with_context(|| format!("Failed to write {}", checksum_path.display()))?;
    let packed = fs::metadata(&package_path)
        .with_context(|| format!("Failed to stat {}", package_path.display()))?
        .len();
    Ok(format!(
        "Packed {} into {packed} bytes ({} file(s), sha256 {digest})",
        manifest.module,
        manifest.files.len()
    ))
}

fn desktop_module_tar_header(path: &str, bytes: u64) -> Result<tar::Header> {
    let mut header = tar::Header::new_ustar();
    header.set_path(path)?;
    header.set_entry_type(tar::EntryType::Regular);
    header.set_size(bytes);
    header.set_mode(DESKTOP_MODULE_TAR_MODE);
    header.set_uid(0);
    header.set_gid(0);
    header.set_mtime(0);
    header.set_cksum();
    Ok(header)
}

fn pack_desktop_module(
    module_dir: &Path,
    manifest: &DesktopModuleManifest,
    package_path: &Path,
) -> Result<()> {
    let manifest_bytes = fs::read(module_dir.join(DESKTOP_MODULE_FILE_LIST_NAME))?;
    let package = File::create(package_path)
        .with_context(|| format!("Failed to create {}", package_path.display()))?;
    let mut archive = tar::Builder::new(brotli::CompressorWriter::new(
        BufWriter::new(package),
        DESKTOP_MODULE_BROTLI_BUFFER_BYTES,
        desktop_module_brotli_quality(manifest),
        DESKTOP_MODULE_BROTLI_WINDOW,
    ));
    let header = desktop_module_tar_header(
        DESKTOP_MODULE_FILE_LIST_NAME,
        u64::try_from(manifest_bytes.len())?,
    )?;
    archive.append(&header, manifest_bytes.as_slice())?;
    let files_dir = module_dir.join(DESKTOP_MODULE_FILES_DIR_NAME);
    for entry in &manifest.files {
        let path = resolve_shared_asset_path(&files_dir, &entry.path)?;
        let file =
            File::open(&path).with_context(|| format!("Failed to open {}", path.display()))?;
        let header = desktop_module_tar_header(
            &format!("{DESKTOP_MODULE_FILES_DIR_NAME}/{}", entry.path),
            entry.bytes,
        )?;
        archive
            .append(&header, file)
            .with_context(|| format!("Failed to pack {}", path.display()))?;
    }
    let mut package = archive.into_inner()?.into_inner();
    package.flush()?;
    Ok(())
}

fn verify_desktop_module_package(
    package_path: &Path,
    manifest: &DesktopModuleManifest,
) -> Result<()> {
    let package = File::open(package_path)
        .with_context(|| format!("Failed to open {}", package_path.display()))?;
    let mut archive = tar::Archive::new(brotli::Decompressor::new(
        BufReader::new(package),
        DESKTOP_MODULE_BROTLI_BUFFER_BYTES,
    ));
    let mut expected = vec![DESKTOP_MODULE_FILE_LIST_NAME.to_string()];
    expected.extend(
        manifest
            .files
            .iter()
            .map(|entry| format!("{DESKTOP_MODULE_FILES_DIR_NAME}/{}", entry.path)),
    );
    let mut found = Vec::with_capacity(expected.len());
    for entry in archive
        .entries()
        .with_context(|| format!("Failed to read {}", package_path.display()))?
    {
        let entry = entry.with_context(|| format!("Failed to read {}", package_path.display()))?;
        found.push(entry.path()?.to_string_lossy().into_owned());
    }
    if found != expected {
        let mismatch = found
            .iter()
            .zip(&expected)
            .find(|(packed, wanted)| packed != wanted)
            .map_or_else(
                || {
                    format!(
                        "{} entrie(s) where {} were expected",
                        found.len(),
                        expected.len()
                    )
                },
                |(packed, wanted)| format!("{packed} where {wanted} was expected"),
            );
        bail!("{} packs {mismatch}", package_path.display());
    }
    Ok(())
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
        "1.2.0",
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

    for (rel, expected_macho_arch) in macos_native_runtime_targets(&electron_arch) {
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
        check_macho_arch(&native_file, expected_macho_arch)?;
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

fn macos_native_runtime_targets(electron_arch: &str) -> Vec<(String, &'static str)> {
    if electron_arch == MACOS_UNIVERSAL_ARCH {
        let mut targets = macos_native_runtime_targets("arm64");
        targets.extend(macos_native_runtime_targets("x64"));
        return targets;
    }
    let expected_macho_arch = if electron_arch == "arm64" {
        "arm64"
    } else {
        "x86_64"
    };
    [
        "@fluxer/webauthn/webauthn",
        "@fluxer/mac-app-audio/mac-app-audio",
        "@fluxer/mac-clipboard/mac-clipboard",
        "@fluxer/mac-sysctl/mac-sysctl",
        "@fluxer/mac-tcc/mac-tcc",
        "@fluxer/macos-input-hook/macos-input-hook",
        "@fluxer/platform-info/platform-info",
        "@fluxer/app-store/app-store",
        "@fluxer/gateway-socket/gateway-socket",
    ]
    .into_iter()
    .map(|prefix| {
        (
            format!("{prefix}.darwin-{electron_arch}.node"),
            expected_macho_arch,
        )
    })
    .collect()
}

fn check_macho_arch(file: &Path, expected: &str) -> Result<()> {
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
        !(expected == "x86_64" && arch_list.contains(&"x86_64h") && !arch_list.contains(&"x86_64")),
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
    let config = windows_package_config(&build_channel, &arch)?;
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
    artifact_prefix: &'static str,
    icon_dir: &'static str,
    runtime: &'static str,
    main_exe: String,
    output_dir: PathBuf,
}

#[derive(Debug, Serialize, Deserialize)]
struct VelopackAssetIndexEntry {
    #[serde(rename = "RelativeFileName")]
    relative_file_name: String,
    #[serde(rename = "Type")]
    asset_type: String,
    #[serde(flatten)]
    extra: BTreeMap<String, Value>,
}

fn windows_package_config(build_channel: &str, arch: &str) -> Result<WindowsPackageConfig> {
    let (pack_id, pack_title, artifact_prefix, icon_dir) = match build_channel {
        "stable" => ("fluxer_desktop", "Fluxer", "Fluxer", "icons-stable"),
        "canary" => (
            "fluxer_desktop_canary",
            "Fluxer Canary",
            "Fluxer-Canary",
            "icons-canary",
        ),
        "development" => (
            "fluxer_desktop_development",
            "Fluxer Development",
            "Fluxer-Development",
            "icons-development",
        ),
        other => bail!(
            "Unsupported BUILD_CHANNEL for Windows packaging: {other}. Expected stable, canary or development"
        ),
    };
    Ok(WindowsPackageConfig {
        pack_id,
        pack_title,
        artifact_prefix,
        icon_dir,
        runtime: if arch == "arm64" {
            "win-arm64"
        } else {
            "win-x64"
        },
        main_exe: format!("{pack_title}.exe"),
        output_dir: PathBuf::from("dist-electron").join(format!("velopack-windows-{arch}")),
    })
}

fn package_app_windows_velopack_step() -> Result<()> {
    let build_channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    let arch = require_env("ARCH")?;
    let version = require_env("VERSION")?;
    let config = windows_package_config(&build_channel, &arch)?;
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
        "None",
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
    let delta_nupkgs = collect_files(&config.output_dir)?
        .into_iter()
        .filter(|path| {
            path.file_name()
                .and_then(OsStr::to_str)
                .is_some_and(|name| name.ends_with("-delta.nupkg"))
        })
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>();

    ensure!(
        legacy_releases.exists(),
        "Velopack did not produce the legacy Squirrel RELEASES file. Do not pass --channel to vpk pack for Windows, or old Squirrel clients cannot migrate."
    );
    ensure!(
        velopack_releases.exists(),
        "Velopack did not produce releases.win.json for Windows updates."
    );
    ensure!(
        delta_nupkgs.is_empty(),
        "Velopack produced {} disabled delta package(s), which cannot be independently verified against the signed full package:\n{}",
        delta_nupkgs.len(),
        delta_nupkgs.join("\n")
    );
    let full_nupkg = full_nupkg.ok_or_else(|| {
        anyhow!("Velopack did not produce a full nupkg payload for Windows updates.")
    })?;
    let full_nupkg = rename_windows_update_package(config, version, arch, &full_nupkg)?;
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
    let desired_setup_name = format!("{}-{version}-win-{arch}.exe", config.artifact_prefix);
    if file_name_string(&setup_exe)? != desired_setup_name {
        fs::rename(&setup_exe, config.output_dir.join(desired_setup_name))
            .with_context(|| format!("Failed to rename {}", setup_exe.display()))?;
    }
    Ok(())
}

fn rename_windows_update_package(
    config: &WindowsPackageConfig,
    version: &str,
    arch: &str,
    source: &Path,
) -> Result<PathBuf> {
    let source_name = file_name_string(source)?;
    let target_name = format!("{}-{version}-win-{arch}-full.nupkg", config.artifact_prefix);
    let setup_name = format!("{}-{version}-win-{arch}.exe", config.artifact_prefix);
    let portable_name = format!(
        "{}-{version}-portable-win-{arch}.zip",
        config.artifact_prefix
    );
    ensure!(
        source_name != target_name,
        "Velopack unexpectedly emitted the canonical package name {target_name:?} before feed normalization"
    );
    let legacy_path = config.output_dir.join("RELEASES");
    let legacy = fs::read_to_string(&legacy_path)
        .with_context(|| format!("Failed to read {}", legacy_path.display()))?;
    ensure!(
        legacy.matches(&source_name).count() == 1,
        "{} must reference Velopack package {source_name:?} exactly once",
        legacy_path.display()
    );
    fs::write(&legacy_path, legacy.replace(&source_name, &target_name))
        .with_context(|| format!("Failed to rewrite {}", legacy_path.display()))?;

    let releases_path = config.output_dir.join("releases.win.json");
    let mut releases: Value = serde_json::from_slice(
        &fs::read(&releases_path)
            .with_context(|| format!("Failed to read {}", releases_path.display()))?,
    )
    .with_context(|| format!("Failed to parse {}", releases_path.display()))?;
    let replacements = replace_json_string(&mut releases, &source_name, &target_name);
    ensure!(
        replacements == 1,
        "{} must reference Velopack package {source_name:?} exactly once, found {replacements}",
        releases_path.display()
    );
    write_json_pretty(&releases_path, &releases)?;

    let assets_path = config.output_dir.join("assets.win.json");
    let mut assets: Vec<VelopackAssetIndexEntry> = serde_json::from_slice(
        &fs::read(&assets_path)
            .with_context(|| format!("Failed to read {}", assets_path.display()))?,
    )
    .with_context(|| format!("Failed to parse {}", assets_path.display()))?;
    ensure!(
        assets.len() == 3,
        "{} must contain exactly three Velopack assets, found {}",
        assets_path.display(),
        assets.len()
    );
    let mut asset_types = BTreeSet::new();
    for asset in &mut assets {
        ensure!(
            asset_types.insert(asset.asset_type.as_str()),
            "{} contains duplicate asset type {:?}",
            assets_path.display(),
            asset.asset_type
        );
        asset.relative_file_name = match asset.asset_type.as_str() {
            "Installer" => setup_name.clone(),
            "Portable" => portable_name.clone(),
            "Full" => {
                ensure!(
                    asset.relative_file_name == source_name,
                    "{} Full asset references {:?}, expected {source_name:?}",
                    assets_path.display(),
                    asset.relative_file_name
                );
                target_name.clone()
            }
            other => bail!(
                "{} contains unsupported Velopack asset type {other:?}",
                assets_path.display()
            ),
        };
    }
    ensure!(
        asset_types == BTreeSet::from(["Full", "Installer", "Portable"]),
        "{} contains an incomplete Velopack asset inventory",
        assets_path.display()
    );
    write_json_pretty(&assets_path, &assets)?;

    let target = config.output_dir.join(&target_name);
    fs::rename(source, &target).with_context(|| {
        format!(
            "Failed to rename {} to {}",
            source.display(),
            target.display()
        )
    })?;
    Ok(target)
}

fn replace_json_string(value: &mut Value, source: &str, target: &str) -> usize {
    match value {
        Value::String(current) if current == source => {
            *current = target.to_string();
            1
        }
        Value::Array(values) => values
            .iter_mut()
            .map(|value| replace_json_string(value, source, target))
            .sum(),
        Value::Object(values) => values
            .values_mut()
            .map(|value| replace_json_string(value, source, target))
            .sum(),
        _ => 0,
    }
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
    let config = windows_package_config(&build_channel, &arch)?;
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
    let config = windows_package_config(&build_channel, &arch)?;
    let Some(pack_dir) = find_windows_unpacked_app(&arch, &config.main_exe) else {
        println!("No unpacked Windows app found; skipping portable ZIP.");
        return Ok(());
    };
    let portable_marker = pack_dir.join(".portable");
    fs::write(&portable_marker, "")
        .with_context(|| format!("Failed to write {}", portable_marker.display()))?;
    let zip_name = format!(
        "{}-{version}-portable-win-{arch}.zip",
        config.artifact_prefix
    );
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
const THIRD_PARTY_WINDOWS_SIGNATURE_ALLOWLIST: &[(&str, &str)] = &[
    (
        "d3dcompiler_47.dll",
        "CN=Microsoft Windows, O=Microsoft Corporation, L=Redmond, S=Washington, C=US",
    ),
    (
        "dxil.dll",
        "CN=Microsoft Windows, O=Microsoft Corporation, L=Redmond, S=Washington, C=US",
    ),
];
const KNOWN_OPTIONAL_WINDOWS_PE_INVENTORY: &[&str] = &[];
const FORBIDDEN_WINDOWS_GAME_CAPTURE_ARTIFACT_PREFIXES: &[&str] = &[
    "fluxer-game-hook.",
    "fluxer-inject-helper.",
    "fluxer-vulkan-layer.",
    "fluxer_game_hook.",
    "fluxer_inject_helper.",
    "fluxer_vulkan_layer.",
];
const WINDOWS_NATIVE_ADDON_STEMS: &[&str] = &[
    "app-store",
    "gateway-socket",
    "hardware-encoder",
    "webauthn",
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
    ];
    names.extend(
        WINDOWS_NATIVE_ADDON_STEMS
            .iter()
            .map(|stem| format!("{stem}.{tag}.node")),
    );
    names.sort();
    names.dedup();
    names
}

fn read_pe_machine(path: &Path) -> Result<Option<u16>> {
    let mut file =
        File::open(path).with_context(|| format!("Failed to open {}", path.display()))?;
    let mut dos_header = [0u8; 0x40];
    if !read_exact_or_eof(&mut file, &mut dos_header, path)? {
        return Ok(None);
    }
    if &dos_header[0..2] != b"MZ" {
        return Ok(None);
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
        return Ok(None);
    }
    if &signature != b"PE\0\0" {
        return Ok(None);
    }
    let mut machine = [0u8; 2];
    if !read_exact_or_eof(&mut file, &mut machine, path)? {
        return Ok(None);
    }
    Ok(Some(u16::from_le_bytes(machine)))
}

fn is_pe_file(path: &Path) -> Result<bool> {
    Ok(read_pe_machine(path)?.is_some())
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

fn windows_pe_machine_arch(machine: u16) -> Option<&'static str> {
    match machine {
        0x014c => Some("ia32"),
        0x8664 => Some("x64"),
        0xaa64 => Some("arm64"),
        _ => None,
    }
}

fn assert_windows_native_pe_machines(
    root: &Path,
    files: &[PathBuf],
    expected_arch: &str,
    main_exe: &str,
) -> Result<()> {
    let mut violations = Vec::new();
    let normalized_main_exe = main_exe.to_ascii_lowercase();
    for path in files {
        let Some(file_name) = path.file_name().and_then(OsStr::to_str) else {
            continue;
        };
        let normalized_name = percent_decode_archive_name(file_name).to_ascii_lowercase();
        let file_arch = if normalized_name == normalized_main_exe {
            Some(expected_arch)
        } else {
            windows_native_pe_arch(&normalized_name)
        };
        let Some(file_arch) = file_arch else {
            continue;
        };
        let machine = read_pe_machine(path)?.ok_or_else(|| {
            anyhow!(
                "{} was collected as a PE file but no COFF Machine field was readable",
                path.display()
            )
        })?;
        let actual_arch = windows_pe_machine_arch(machine);
        if actual_arch != Some(file_arch) || actual_arch != Some(expected_arch) {
            violations.push(format!(
                "{}: file policy expects {file_arch}, COFF Machine is 0x{machine:04x} ({}) and package architecture is {expected_arch}",
                relative_display(root, path),
                actual_arch.unwrap_or("unknown")
            ));
        }
    }
    ensure!(
        violations.is_empty(),
        "{} contains {} Windows native binary/binaries with invalid machine architecture:\n{}",
        root.display(),
        violations.len(),
        violations.join("\n")
    );
    Ok(())
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
    let forbidden = present
        .iter()
        .filter_map(|name| windows_pe_inventory_violation(name, arch))
        .collect::<Vec<_>>();
    ensure!(
        forbidden.is_empty(),
        "{} contains {} forbidden Windows native binaries:\n{}",
        root.display(),
        forbidden.len(),
        forbidden.join("\n")
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
        "{}: {} expected, {} unlisted PE(s) shipped by glob (Electron runtime and third-party binaries). Every one of them is signature-classified below; none may be unsigned or signed by an unknown publisher.",
        root.display(),
        expected.len(),
        unlisted.len()
    );
    for name in &unlisted {
        println!("Unlisted Windows PE pending signature classification: {name}");
    }
    Ok(())
}

const ASAR_HEADER_LIMIT: usize = 64 * 1024 * 1024;
const ASAR_ENTRY_LIMIT: usize = 1_000_000;
const ASAR_NESTING_LIMIT: usize = 256;

fn windows_package_file_policy_violation(relative: &str, expected_arch: &str) -> bool {
    let normalized_relative = relative
        .replace('\\', "/")
        .split('/')
        .map(percent_decode_archive_name)
        .collect::<Vec<_>>()
        .join("/")
        .replace('\\', "/")
        .to_ascii_lowercase();
    let normalized_name = normalized_relative
        .rsplit('/')
        .next()
        .unwrap_or_default()
        .to_string();
    if FORBIDDEN_WINDOWS_GAME_CAPTURE_ARTIFACT_PREFIXES
        .iter()
        .any(|prefix| normalized_name.starts_with(prefix))
    {
        return true;
    }
    if normalized_name == "compatibility.json"
        && normalized_relative
            .split('/')
            .any(|component| component == "win-game-capture")
    {
        return true;
    }
    windows_native_pe_arch(&normalized_name).is_some_and(|arch| arch != expected_arch)
}

fn read_u32_le(bytes: &[u8], offset: usize) -> Result<u32> {
    let end = offset
        .checked_add(4)
        .ok_or_else(|| anyhow!("ASAR header offset overflow"))?;
    let value = bytes
        .get(offset..end)
        .ok_or_else(|| anyhow!("ASAR header is truncated at byte {offset}"))?;
    let value = <[u8; 4]>::try_from(value)
        .map_err(|_| anyhow!("ASAR header field at byte {offset} is not four bytes"))?;
    Ok(u32::from_le_bytes(value))
}

fn collect_asar_policy_violations(
    node: &Value,
    expected_arch: &str,
    violations: &mut Vec<String>,
) -> Result<()> {
    let mut stack = vec![(node, String::new(), 0usize)];
    let mut entry_count = 0usize;
    while let Some((current, prefix, depth)) = stack.pop() {
        ensure!(
            depth <= ASAR_NESTING_LIMIT,
            "ASAR header nesting exceeds {ASAR_NESTING_LIMIT} levels under {prefix:?}"
        );
        let files = current
            .get("files")
            .and_then(Value::as_object)
            .ok_or_else(|| anyhow!("ASAR header node {prefix:?} has no files object"))?;
        for (name, entry) in files {
            entry_count = entry_count
                .checked_add(1)
                .ok_or_else(|| anyhow!("ASAR entry count overflow"))?;
            ensure!(
                entry_count <= ASAR_ENTRY_LIMIT,
                "ASAR header contains more than {ASAR_ENTRY_LIMIT} entries"
            );
            let decoded_name = percent_decode_archive_name(name);
            ensure!(
                !name.is_empty()
                    && name != "."
                    && name != ".."
                    && !name.contains('/')
                    && !name.contains('\\')
                    && decoded_name != "."
                    && decoded_name != ".."
                    && !decoded_name.contains('/')
                    && !decoded_name.contains('\\'),
                "ASAR header contains invalid entry name {name:?} under {prefix:?}"
            );
            let relative = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{prefix}/{name}")
            };
            if windows_package_file_policy_violation(&relative, expected_arch) {
                violations.push(relative.clone());
            }
            if entry.get("files").is_some() {
                stack.push((entry, relative, depth + 1));
            }
        }
    }
    Ok(())
}

fn asar_policy_violations(path: &Path, expected_arch: &str) -> Result<Vec<String>> {
    let mut file =
        File::open(path).with_context(|| format!("Failed to open {}", path.display()))?;
    let file_size = file
        .metadata()
        .with_context(|| format!("Failed to stat {}", path.display()))?
        .len();
    let mut prefix = [0u8; 16];
    file.read_exact(&mut prefix)
        .with_context(|| format!("Failed to read ASAR header prefix from {}", path.display()))?;
    let size_pickle_payload = read_u32_le(&prefix, 0)?;
    let header_pickle_size = read_u32_le(&prefix, 4)?;
    let header_pickle_payload = read_u32_le(&prefix, 8)?;
    let header_json_size = read_u32_le(&prefix, 12)?;
    let padded_header_json_size = header_json_size
        .checked_add(3)
        .map(|size| size & !3)
        .ok_or_else(|| anyhow!("{} ASAR JSON header size overflow", path.display()))?;
    let expected_header_pickle_payload = padded_header_json_size
        .checked_add(4)
        .ok_or_else(|| anyhow!("{} ASAR pickle payload size overflow", path.display()))?;
    ensure!(
        size_pickle_payload == 4
            && header_pickle_payload.checked_add(4) == Some(header_pickle_size)
            && header_pickle_payload == expected_header_pickle_payload,
        "{} has an invalid ASAR pickle header",
        path.display()
    );
    let header_json_size = usize::try_from(header_json_size).context("ASAR header is too large")?;
    ensure!(
        header_json_size > 0 && header_json_size <= ASAR_HEADER_LIMIT,
        "{} ASAR JSON header size {} is outside 1..={ASAR_HEADER_LIMIT}",
        path.display(),
        header_json_size
    );
    let header_pickle_size =
        usize::try_from(header_pickle_size).context("ASAR pickle header is too large")?;
    let archive_payload_offset = 8usize
        .checked_add(header_pickle_size)
        .ok_or_else(|| anyhow!("{} ASAR header size overflow", path.display()))?;
    ensure!(
        u64::try_from(archive_payload_offset).unwrap_or(u64::MAX) <= file_size,
        "{} ASAR header extends beyond the {}-byte archive",
        path.display(),
        file_size
    );
    let mut header_json = vec![0u8; header_json_size];
    file.read_exact(&mut header_json)
        .with_context(|| format!("Failed to read ASAR JSON header from {}", path.display()))?;
    let header: Value = serde_json::from_slice(&header_json)
        .with_context(|| format!("Failed to parse ASAR JSON header from {}", path.display()))?;
    let mut violations = Vec::new();
    collect_asar_policy_violations(&header, expected_arch, &mut violations)?;
    Ok(violations)
}

fn assert_windows_package_file_policy(root: &Path, expected_arch: &str) -> Result<()> {
    let mut forbidden = Vec::new();
    for path in collect_files(root)? {
        let relative = relative_display(root, &path);
        if windows_package_file_policy_violation(&relative, expected_arch) {
            forbidden.push(relative.clone());
        }
        if path
            .file_name()
            .and_then(OsStr::to_str)
            .is_some_and(|name| {
                percent_decode_archive_name(name)
                    .to_ascii_lowercase()
                    .ends_with(".asar")
            })
        {
            forbidden.extend(
                asar_policy_violations(&path, expected_arch)?
                    .into_iter()
                    .map(|entry| format!("{relative}!/{entry}")),
            );
        }
    }
    forbidden.sort();
    forbidden.dedup();
    ensure!(
        forbidden.is_empty(),
        "{} contains {} forbidden Windows package file(s):\n{}",
        root.display(),
        forbidden.len(),
        forbidden.join("\n")
    );
    Ok(())
}

fn windows_pe_inventory_violation(name: &str, expected_arch: &str) -> Option<String> {
    let normalized = name.to_ascii_lowercase();
    if FORBIDDEN_WINDOWS_GAME_CAPTURE_ARTIFACT_PREFIXES
        .iter()
        .any(|prefix| normalized.starts_with(prefix))
    {
        return Some(format!(
            "{name}: disabled game-capture hook sidecars must not ship"
        ));
    }
    let packaged_arch = windows_native_pe_arch(&normalized)?;
    if packaged_arch == expected_arch {
        return None;
    }
    Some(format!(
        "{name}: native architecture is {packaged_arch}, expected {expected_arch}"
    ))
}

fn windows_native_pe_arch(name: &str) -> Option<&'static str> {
    for (marker, arch) in [
        (".win32-x64-msvc.", "x64"),
        (".win32-arm64-msvc.", "arm64"),
        (".win32-ia32-msvc.", "ia32"),
        ("_win_x64_msvc.", "x64"),
        ("_win_arm64_msvc.", "arm64"),
        ("_win_x86_msvc.", "ia32"),
    ] {
        if name.contains(marker) {
            return Some(arch);
        }
    }
    None
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
        "Authenticode signature has no RFC3161 timestamp"
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

fn assert_third_party_signed(row: &SignatureRow, relative: &str) -> Result<()> {
    ensure!(
        row.status == "Valid",
        "Authenticode status is {} (expected Valid)",
        row.status
    );
    let subject = row
        .subject
        .as_deref()
        .ok_or_else(|| anyhow!("Authenticode signature has no signer certificate subject"))?;
    ensure!(
        THIRD_PARTY_WINDOWS_SIGNATURE_ALLOWLIST
            .iter()
            .any(|(allowed_path, allowed_subject)| {
                relative.eq_ignore_ascii_case(allowed_path) && subject == *allowed_subject
            }),
        "Signer subject '{subject}' is not allowlisted for {relative}"
    );
    ensure!(
        row.ts_subject.is_some(),
        "Authenticode signature has no RFC3161 timestamp"
    );
    Ok(())
}

fn assert_signed_by_known_publisher(row: &SignatureRow, relative: &str) -> Result<()> {
    match assert_fluxer_signed(row) {
        Ok(()) => Ok(()),
        Err(fluxer_error) => assert_third_party_signed(row, relative)
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
        if let Err(error) = assert_signed_by_known_publisher(row, &relative) {
            failures.push(format!("{relative}: {error}"));
        }
    }
    ensure!(
        failures.is_empty(),
        "{label}: {} of {} Windows binaries do not have an approved signature:\n{}",
        failures.len(),
        files.len(),
        failures.join("\n")
    );
    println!(
        "{label}: verified {} Windows binary signatures.",
        files.len()
    );
    Ok(())
}

fn verify_windows_unpacked_signatures_step() -> Result<()> {
    let build_channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    let arch = require_env("ARCH")?;
    let config = windows_package_config(&build_channel, &arch)?;
    let pack_dir = resolve_windows_unpacked_dir(&arch, &config.main_exe)?;
    let files = collect_pe_files(&pack_dir)?;
    assert_windows_package_file_policy(&pack_dir, &arch)?;
    assert_windows_native_pe_machines(&pack_dir, &files, &arch, &config.main_exe)?;
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
    let config = windows_package_config(&build_channel, &arch)?;

    let nupkg = first_file_matching(&config.output_dir, |name| name.ends_with("-full.nupkg"))
        .ok_or_else(|| {
            anyhow!(
                "No Velopack full nupkg found in {}",
                config.output_dir.display()
            )
        })?;
    let setup_exe = config.output_dir.join(format!(
        "{}-{version}-win-{arch}.exe",
        config.artifact_prefix
    ));
    ensure!(
        setup_exe.is_file(),
        "Velopack Setup.exe not found: {}",
        setup_exe.display()
    );
    let portable_zip = PathBuf::from("dist-electron").join(format!(
        "{}-{version}-portable-win-{arch}.zip",
        config.artifact_prefix
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
    let unclassified_nupkgs = staged_nupkgs
        .iter()
        .filter(|path| **path != nupkg)
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>();
    ensure!(
        unclassified_nupkgs.is_empty(),
        "{} stages {} nupkg(s) other than the verified full package, so they would be published unverified:\n{}",
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
    assert_windows_package_file_policy(&lib_app, &arch)?;
    assert_windows_native_pe_machines(&lib_app, &nupkg_files, &arch, &config.main_exe)?;
    assert_expected_windows_pe_inventory(&lib_app, &nupkg_files, &arch, &config.main_exe)?;
    verify_windows_pe_signatures(&signtool, "nupkg lib/app", &lib_app, &nupkg_files)?;

    let portable_root = root.join("p");
    extract_zip_safely(&portable_zip, &portable_root)?;
    let portable_files = collect_pe_files(&portable_root)?;
    assert_windows_package_file_policy(&portable_root, &arch)?;
    assert_windows_native_pe_machines(&portable_root, &portable_files, &arch, &config.main_exe)?;
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
        && env::var("ARCH").unwrap_or_default() == MACOS_UNIVERSAL_ARCH
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

fn stage_handoff_step() -> Result<()> {
    let build_channel = require_env("BUILD_CHANNEL")?;
    let platform = require_any_env(&["DESKTOP_PLATFORM", "PLATFORM"])?;
    let arch = require_any_env(&["DESKTOP_ARCH", "ARCH"])?;
    let staging = Path::new("upload_staging");
    ensure!(staging.exists(), "upload_staging is missing.");
    let artifact_count = count_files(staging)?;
    ensure!(artifact_count > 0, "upload_staging is empty.");

    let artifact_name = handoff_artifact_name(&build_channel, &platform, &arch, false);
    println!("Staging {artifact_count} desktop artifact file(s) as {artifact_name}");
    append_github_output(&[("artifact_name", artifact_name.as_str())])
}

fn handoff_artifact_name(
    build_channel: &str,
    platform: &str,
    arch: &str,
    signed_windows_artifacts: bool,
) -> String {
    let signed_suffix = if signed_windows_artifacts && platform == "windows" {
        "-signed"
    } else {
        ""
    };
    format!("fluxer-desktop-{build_channel}-{platform}-{arch}{signed_suffix}")
}

fn build_payload_step() -> Result<()> {
    let payload_root = Path::new("payload_tree").join(DESKTOP_PAYLOAD_PREFIX);
    remove_dir_if_exists(&payload_root)?;
    fs::create_dir_all(&payload_root)?;

    let channel = require_env("CHANNEL")?;
    let version = require_env("VERSION")?;
    let pub_date = require_env("PUB_DATE")?;
    let artifacts = Path::new("artifacts");
    let artifact_dirs = payload_artifact_dirs(artifacts, &channel)?;
    ensure!(
        !artifact_dirs.is_empty(),
        "No desktop build artifacts were downloaded into {}",
        artifacts.display()
    );
    for (dir, identity) in artifact_dirs {
        let platform = match identity.platform.as_str() {
            "windows" => "win32",
            "macos" => "darwin",
            "linux" => "linux",
            other => {
                println!("Unknown platform: {other}");
                continue;
            }
        };
        for published_arch in published_arches(platform, &identity.arch) {
            let dest = payload_root
                .join(&channel)
                .join(platform)
                .join(published_arch);
            fs::create_dir_all(&dest)?;
            copy_dir_contents(&dir, &dest)?;
            let manifest = build_desktop_manifest(
                &dest,
                &PayloadManifestInput {
                    channel: channel.clone(),
                    platform: platform.to_string(),
                    arch: published_arch.to_string(),
                    version: version.clone(),
                    pub_date: pub_date.clone(),
                },
            )?;
            if platform == "darwin" {
                write_macos_releases(&dest, &channel, &manifest)?;
            }
            write_json_pretty(&dest.join("manifest.json"), &manifest)?;
        }
    }

    println!("Payload tree:");
    print_tree(&payload_root, 6)
}

fn prepare_release_assets_step() -> Result<()> {
    let channel = require_env("CHANNEL")?;
    let version = require_env("VERSION")?;
    let source_sha = require_env("SOURCE_SHA")?;
    let product = desktop_release_product(&channel)?;
    let payload_root = Path::new("payload_tree")
        .join(DESKTOP_PAYLOAD_PREFIX)
        .join(&channel);
    let release_assets = Path::new("release_assets");
    remove_dir_if_exists(release_assets)?;
    fs::create_dir_all(release_assets)?;

    let mut release_builder =
        DesktopReleaseAssetBuilder::new(&channel, &version, product, release_assets);
    for (platform, arch) in desktop_release_coordinates() {
        let dir = payload_root.join(platform).join(arch);
        ensure!(
            dir.is_dir(),
            "Desktop release payload directory is missing: {}",
            dir.display()
        );
        let manifest_path = dir.join("manifest.json");
        let manifest: DesktopManifest = serde_json::from_slice(
            &fs::read(&manifest_path)
                .with_context(|| format!("Failed to read {}", manifest_path.display()))?,
        )
        .with_context(|| format!("Failed to parse {}", manifest_path.display()))?;
        ensure!(
            manifest.channel == channel
                && manifest.platform == platform
                && manifest.arch == arch
                && manifest.version == version,
            "Desktop release manifest identity mismatch in {}",
            manifest_path.display()
        );
        let expected_kinds = desktop_release_shipped_formats(platform)?
            .iter()
            .copied()
            .collect::<BTreeSet<&str>>();
        let actual_kinds = manifest
            .files
            .keys()
            .map(String::as_str)
            .collect::<BTreeSet<_>>();
        ensure!(
            actual_kinds == expected_kinds,
            "Incomplete shipped artifact set for {platform}/{arch}: expected {:?}, found {:?}",
            expected_kinds,
            actual_kinds
        );
        for entry in manifest.files.values() {
            release_builder.add(platform, arch, &dir.join(entry.filename()), false)?;
        }
        let updater_files = desktop_updater_release_files(&dir, platform)?;
        for updater_file in updater_files {
            release_builder.add(platform, arch, &updater_file, true)?;
        }
    }
    add_desktop_module_release_assets(&mut release_builder, &payload_root)?;
    let (mut descriptor_assets, mut descriptor_modules) = release_builder.finish();
    descriptor_assets.sort_by(|left, right| left.storage_key.cmp(&right.storage_key));
    descriptor_modules.sort_by(|left, right| left.storage_key.cmp(&right.storage_key));
    let descriptor = DesktopReleaseDescriptor {
        schema_version: DESKTOP_RELEASE_DESCRIPTOR_SCHEMA_VERSION,
        kind: DesktopReleaseKind::Full,
        channel: channel.clone(),
        version: version.clone(),
        release_tag: format!("fluxer-desktop-{channel}@{version}"),
        source_sha,
        assets: descriptor_assets,
        modules: descriptor_modules,
    };
    validate_desktop_release_descriptor(&descriptor, &channel, &version, &descriptor.source_sha)?;
    validate_desktop_release_module_files(&descriptor, release_assets)?;
    let descriptor_path =
        release_assets.join(desktop_release_descriptor_filename(&channel, &version)?);
    write_json_pretty(&descriptor_path, &descriptor)?;
    println!("GitHub release asset tree:");
    print_tree(release_assets, 2)
}

fn add_desktop_module_release_assets(
    release_builder: &mut DesktopReleaseAssetBuilder,
    channel_payload: &Path,
) -> Result<()> {
    let mut packages = BTreeMap::<String, String>::new();
    for (platform, arch) in desktop_release_coordinates() {
        let manifest_path = channel_payload
            .join(platform)
            .join(arch)
            .join(DESKTOP_CHANNEL_MANIFEST_NAME);
        ensure!(
            manifest_path.is_file(),
            "Desktop module manifest is missing: {}, run the build_module_manifest step first",
            manifest_path.display()
        );
        let manifest: DesktopChannelManifest = serde_json::from_slice(
            &fs::read(&manifest_path)
                .with_context(|| format!("Failed to read {}", manifest_path.display()))?,
        )
        .with_context(|| format!("Failed to parse {}", manifest_path.display()))?;
        for (module, entry) in manifest.modules {
            if let Some(existing) = packages.insert(module.clone(), entry.sha256.clone()) {
                ensure!(
                    existing == entry.sha256,
                    "Desktop module {module} resolves to {existing} and {} across coordinates",
                    entry.sha256
                );
            }
        }
        release_builder.add_module_manifest(platform, arch, &manifest_path)?;
    }
    for (module, sha256) in &packages {
        let package_path = channel_payload
            .join(DESKTOP_MODULES_KEY_SEGMENT)
            .join(module)
            .join(sha256)
            .join(DESKTOP_MODULE_PACKAGE_NAME);
        release_builder.add_module_package(module, sha256, &package_path)?;
    }
    Ok(())
}

fn desktop_updater_release_files(dir: &Path, platform: &str) -> Result<Vec<PathBuf>> {
    let mut files = desktop_release_updater_feeds(platform)?
        .iter()
        .map(|name| dir.join(name))
        .collect::<Vec<_>>();
    if let Some(suffix) = desktop_release_update_payload_suffix(platform)? {
        let payloads = collect_files(dir)?
            .into_iter()
            .filter(|path| {
                path.file_name()
                    .and_then(OsStr::to_str)
                    .is_some_and(|name| name.ends_with(suffix))
            })
            .collect::<Vec<_>>();
        ensure!(
            payloads.len() == 1,
            "Expected one {suffix} desktop update payload in {}, found {}",
            dir.display(),
            payloads.len()
        );
        files.push(payloads[0].clone());
    }
    for path in &files {
        ensure!(
            path.is_file(),
            "Desktop updater release file is missing: {}",
            path.display()
        );
    }
    Ok(files)
}

struct DesktopReleaseAssetBuilder<'a> {
    channel: &'a str,
    version: &'a str,
    product: &'a str,
    release_assets: &'a Path,
    descriptor_assets: Vec<DesktopReleaseAsset>,
    descriptor_modules: Vec<DesktopReleaseAsset>,
    storage_keys: BTreeSet<String>,
    release_asset_content: BTreeMap<String, (String, u64)>,
    release_asset_names: BTreeMap<String, String>,
}

impl<'a> DesktopReleaseAssetBuilder<'a> {
    fn new(channel: &'a str, version: &'a str, product: &'a str, release_assets: &'a Path) -> Self {
        Self {
            channel,
            version,
            product,
            release_assets,
            descriptor_assets: Vec::new(),
            descriptor_modules: Vec::new(),
            storage_keys: BTreeSet::new(),
            release_asset_content: BTreeMap::new(),
            release_asset_names: BTreeMap::new(),
        }
    }

    fn add(&mut self, platform: &str, arch: &str, source: &Path, qualify_name: bool) -> Result<()> {
        ensure!(
            source.is_file(),
            "Release source is missing: {}",
            source.display()
        );
        let source_name = file_name_string(source)?;
        let canonical_prefix = format!("{}-{}-", self.product, self.version);
        ensure!(
            qualify_name || source_name.starts_with(&canonical_prefix),
            "Shipped desktop artifact name is not canonical: {source_name:?}"
        );
        let release_asset =
            desktop_release_asset_name(self.channel, self.version, platform, arch, &source_name)?;
        ensure!(
            release_asset.starts_with(&canonical_prefix)
                && release_asset.bytes().all(|byte| {
                    byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'-' | b'_')
                }),
            "Desktop release asset name is not canonical and URL-safe: {release_asset:?}"
        );
        let storage_key = format!(
            "{DESKTOP_PAYLOAD_PREFIX}/{}/{platform}/{arch}/{source_name}",
            self.channel
        );
        let asset = self.copy(storage_key, release_asset, source)?;
        self.descriptor_assets.push(asset);
        Ok(())
    }

    fn add_module_manifest(&mut self, platform: &str, arch: &str, source: &Path) -> Result<()> {
        let asset = self.copy(
            desktop_module_manifest_storage_key(self.channel, platform, arch),
            desktop_module_manifest_release_asset_name(self.channel, self.version, platform, arch)?,
            source,
        )?;
        self.descriptor_modules.push(asset);
        Ok(())
    }

    fn add_module_package(&mut self, module: &str, sha256: &str, source: &Path) -> Result<()> {
        let asset = self.copy(
            desktop_module_package_storage_key(self.channel, module, sha256),
            desktop_module_package_release_asset_name(self.channel, self.version, module, sha256)?,
            source,
        )?;
        ensure!(
            asset.sha256 == sha256,
            "Desktop module package {} hashes to {}, its manifest records {sha256}",
            source.display(),
            asset.sha256
        );
        self.descriptor_modules.push(asset);
        Ok(())
    }

    fn copy(
        &mut self,
        storage_key: String,
        release_asset: String,
        source: &Path,
    ) -> Result<DesktopReleaseAsset> {
        ensure!(
            source.is_file(),
            "Release source is missing: {}",
            source.display()
        );
        if let Some(existing) = self
            .release_asset_names
            .insert(release_asset.to_ascii_lowercase(), release_asset.clone())
        {
            ensure!(
                existing == release_asset,
                "Desktop release asset names differ only by case: {existing:?} and {release_asset:?}"
            );
        }
        ensure!(
            self.storage_keys.insert(storage_key.clone()),
            "Duplicate desktop release storage key {storage_key:?}"
        );
        let size = fs::metadata(source)
            .with_context(|| format!("Failed to inspect {}", source.display()))?
            .len();
        ensure!(
            size > 0,
            "Desktop release source is empty: {}",
            source.display()
        );
        let sha256 = sha256_file(source)?;
        if let Some((existing_sha256, existing_size)) =
            self.release_asset_content.get(&release_asset)
        {
            ensure!(
                existing_sha256 == &sha256 && *existing_size == size,
                "Desktop release asset {release_asset:?} has conflicting source content"
            );
        } else {
            let destination = self.release_assets.join(&release_asset);
            let copied = fs::copy(source, &destination).with_context(|| {
                format!(
                    "Failed to copy desktop release asset {} to {}",
                    source.display(),
                    destination.display()
                )
            })?;
            ensure!(
                copied == size,
                "Desktop release asset copy size mismatch for {}",
                destination.display()
            );
            self.release_asset_content
                .insert(release_asset.clone(), (sha256.clone(), size));
        }
        Ok(DesktopReleaseAsset {
            storage_key,
            release_asset,
            sha256,
            size,
        })
    }

    fn finish(self) -> (Vec<DesktopReleaseAsset>, Vec<DesktopReleaseAsset>) {
        (self.descriptor_assets, self.descriptor_modules)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ArtifactIdentity {
    platform: String,
    arch: String,
    signed: bool,
}

fn parse_artifact_dir_name(base: &str, channel: &str) -> Option<ArtifactIdentity> {
    let prefix = format!("fluxer-desktop-{channel}-");
    let rest = base.strip_prefix(&prefix)?;
    let (rest, signed) = rest
        .strip_suffix("-signed")
        .map(|value| (value, true))
        .unwrap_or((rest, false));
    let (platform, arch) = rest.rsplit_once('-')?;
    Some(ArtifactIdentity {
        platform: platform.to_string(),
        arch: arch.to_string(),
        signed,
    })
}

fn payload_artifact_dirs(
    artifacts: &Path,
    channel: &str,
) -> Result<Vec<(PathBuf, ArtifactIdentity)>> {
    let mut selected = BTreeMap::<(String, String), (PathBuf, ArtifactIdentity)>::new();
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

        let key = (identity.platform.clone(), identity.arch.clone());
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
    version: String,
    pub_date: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopManifest {
    channel: String,
    platform: String,
    arch: String,
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
        version: input.version.clone(),
        pub_date: input.pub_date.clone(),
        minimum_system_version: if input.platform == "darwin" {
            Some(MACOS_MINIMUM_SYSTEM_VERSION.to_string())
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

fn published_arches(platform: &str, arch: &str) -> Vec<&'static str> {
    if platform == "darwin" && arch == MACOS_UNIVERSAL_ARCH {
        return vec!["x64", "arm64"];
    }
    match arch {
        "x64" => vec!["x64"],
        "arm64" => vec!["arm64"],
        other => panic!("Unsupported desktop arch: {other}"),
    }
}

fn write_macos_releases(dest: &Path, channel: &str, manifest: &DesktopManifest) -> Result<()> {
    let Some(zip) = manifest.files.get("zip") else {
        println!(
            "No .zip found for macOS {} in {} (auto-update requires zip artifacts).",
            manifest.arch,
            dest.display()
        );
        return Ok(());
    };
    let url = format!(
        "{}/{DESKTOP_PAYLOAD_PREFIX}/{channel}/{}/{}/{}",
        package_origin_base(),
        manifest.platform,
        manifest.arch,
        zip.filename()
    );
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

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub(crate) struct DesktopChannelManifestShell {
    pub(crate) latest_version: String,
    pub(crate) minimum_version: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub(crate) struct DesktopChannelManifestEntry {
    pub(crate) sha256: String,
    pub(crate) bytes: u64,
    pub(crate) url: String,
    pub(crate) minimum_shell_version: String,
    pub(crate) maximum_shell_version: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub(crate) struct DesktopChannelManifest {
    pub(crate) manifest_version: u64,
    pub(crate) release_channel: String,
    pub(crate) platform: String,
    pub(crate) arch: String,
    pub(crate) build_version: String,
    pub(crate) pub_date: String,
    pub(crate) metadata_version: u64,
    pub(crate) shell: DesktopChannelManifestShell,
    pub(crate) modules: BTreeMap<String, DesktopChannelManifestEntry>,
    pub(crate) required_modules: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DesktopPackedModule {
    module: String,
    sha256: String,
    bytes: u64,
    directory: PathBuf,
}

fn package_origin_base() -> String {
    env::var(PACKAGE_ORIGIN_BASE_ENV)
        .ok()
        .map(|value| value.trim().trim_end_matches('/').to_string())
        .filter(|value| !value.is_empty())
        .unwrap_or_else(|| PACKAGE_ORIGIN_BASE.to_string())
}

pub(crate) fn desktop_module_package_url(channel: &str, module: &str, sha256: &str) -> String {
    format!(
        "{}/{DESKTOP_PAYLOAD_PREFIX}/{channel}/{DESKTOP_MODULES_KEY_SEGMENT}/{module}/{sha256}/{DESKTOP_MODULE_PACKAGE_NAME}",
        package_origin_base()
    )
}

fn read_packed_desktop_modules(
    modules_dir: &Path,
    release_channel: &str,
    build_version: &str,
) -> Result<Vec<DesktopPackedModule>> {
    ensure!(
        modules_dir.is_dir(),
        "Missing {}, run the split_modules and pack_modules steps first",
        modules_dir.display()
    );
    let mut packed = Vec::new();
    for module_dir in desktop_module_dirs(modules_dir)? {
        let manifest = read_desktop_module_manifest(&module_dir)?;
        ensure!(
            manifest.release_channel == release_channel,
            "Desktop module {} was packed for channel {}, expected {release_channel}",
            manifest.module,
            manifest.release_channel
        );
        ensure!(
            manifest.build_version == build_version,
            "Desktop module {} was packed for version {}, expected {build_version}",
            manifest.module,
            manifest.build_version
        );
        let package_path = module_dir.join(DESKTOP_MODULE_PACKAGE_NAME);
        let checksum_path = module_dir.join(DESKTOP_MODULE_PACKAGE_CHECKSUM_NAME);
        let recorded = fs::read_to_string(&checksum_path)
            .with_context(|| format!("Failed to read {}", checksum_path.display()))?;
        let recorded = recorded.trim();
        let sha256 = sha256_file(&package_path)?;
        ensure!(
            sha256 == recorded,
            "Desktop module package {} hashes to {sha256}, {} records {recorded}",
            package_path.display(),
            checksum_path.display()
        );
        let bytes = fs::metadata(&package_path)
            .with_context(|| format!("Failed to stat {}", package_path.display()))?
            .len();
        packed.push(DesktopPackedModule {
            module: manifest.module,
            sha256,
            bytes,
            directory: module_dir,
        });
    }
    let present = packed
        .iter()
        .map(|module| module.module.clone())
        .collect::<BTreeSet<_>>();
    let expected = expected_desktop_modules(modules_dir)?;
    ensure!(
        present == expected,
        "The packed desktop modules are [{}], expected [{}]",
        present.into_iter().collect::<Vec<_>>().join(", "),
        expected.into_iter().collect::<Vec<_>>().join(", ")
    );
    Ok(packed)
}

fn stage_desktop_module_packages(
    payload_root: &Path,
    channel: &str,
    packed: &[DesktopPackedModule],
) -> Result<()> {
    for module in packed {
        let dest = payload_root
            .join(channel)
            .join(DESKTOP_MODULES_KEY_SEGMENT)
            .join(&module.module)
            .join(&module.sha256);
        fs::create_dir_all(&dest)
            .with_context(|| format!("Failed to create {}", dest.display()))?;
        for name in [
            DESKTOP_MODULE_PACKAGE_NAME,
            DESKTOP_MODULE_PACKAGE_CHECKSUM_NAME,
            DESKTOP_MODULE_FILE_LIST_NAME,
        ] {
            let source = module.directory.join(name);
            let target = dest.join(name);
            fs::copy(&source, &target).with_context(|| {
                format!(
                    "Failed to copy {} to {}",
                    source.display(),
                    target.display()
                )
            })?;
        }
    }
    Ok(())
}

fn desktop_module_minimum_shell_version(channel: &str) -> &'static str {
    if channel == "development" {
        DESKTOP_DEVELOPMENT_MINIMUM_SHELL_VERSION
    } else {
        DESKTOP_MODULE_MINIMUM_SHELL_VERSION
    }
}

fn desktop_channel_manifest_entries(
    channel: &str,
    packed: &[DesktopPackedModule],
) -> BTreeMap<String, DesktopChannelManifestEntry> {
    packed
        .iter()
        .map(|module| {
            (
                module.module.clone(),
                DesktopChannelManifestEntry {
                    sha256: module.sha256.clone(),
                    bytes: module.bytes,
                    url: desktop_module_package_url(channel, &module.module, &module.sha256),
                    minimum_shell_version: desktop_module_minimum_shell_version(channel).to_string(),
                    maximum_shell_version: None,
                },
            )
        })
        .collect()
}

fn desktop_module_metadata_version(pub_date: &str) -> Result<u64> {
    let published = DateTime::parse_from_rfc3339(pub_date)
        .with_context(|| format!("Failed to parse PUB_DATE {pub_date}"))?;
    u64::try_from(published.timestamp())
        .with_context(|| format!("PUB_DATE {pub_date} predates the epoch"))
}

fn sorted_child_directories(root: &Path) -> Result<Vec<PathBuf>> {
    let mut dirs = fs::read_dir(root)
        .with_context(|| format!("Failed to read {}", root.display()))?
        .collect::<std::result::Result<Vec<_>, _>>()?
        .into_iter()
        .map(|entry| entry.path())
        .filter(|path| path.is_dir())
        .collect::<Vec<_>>();
    dirs.sort();
    Ok(dirs)
}

fn desktop_channel_manifest_targets(
    payload_root: &Path,
    channel: &str,
) -> Result<Vec<(String, String)>> {
    let channel_dir = payload_root.join(channel);
    let mut targets = Vec::new();
    for platform_dir in sorted_child_directories(&channel_dir)? {
        let platform = file_name_string(&platform_dir)?;
        if platform == DESKTOP_MODULES_KEY_SEGMENT {
            continue;
        }
        for arch_dir in sorted_child_directories(&platform_dir)? {
            if !arch_dir.join(DESKTOP_PAYLOAD_MANIFEST_NAME).is_file() {
                continue;
            }
            targets.push((platform.clone(), file_name_string(&arch_dir)?));
        }
    }
    Ok(targets)
}

fn desktop_module_manifest_targets(
    payload_root: &Path,
    channel: &str,
) -> Result<Vec<(String, String)>> {
    if !env_bool("DESKTOP_MODULE_ONLY") {
        return desktop_channel_manifest_targets(payload_root, channel);
    }
    let platform = require_env("PLATFORM")?;
    let arch = require_env("ARCH")?;
    ensure!(
        matches!(platform.as_str(), "darwin" | "win32" | "linux"),
        "Invalid module-only PLATFORM: {platform}"
    );
    ensure!(
        matches!(arch.as_str(), "x64" | "arm64"),
        "Invalid module-only ARCH: {arch}"
    );
    Ok(vec![(platform, arch)])
}

fn desktop_channel_manifest_path(
    payload_root: &Path,
    channel: &str,
    platform: &str,
    arch: &str,
) -> PathBuf {
    payload_root
        .join(channel)
        .join(platform)
        .join(arch)
        .join(DESKTOP_CHANNEL_MANIFEST_NAME)
}

fn write_desktop_channel_manifest(path: &Path, manifest: &DesktopChannelManifest) -> Result<()> {
    let bytes = serde_json::to_vec(manifest)?;
    write_desktop_channel_manifest_bytes(path, &manifest.platform, &manifest.arch, bytes)
}

fn write_desktop_channel_manifest_bytes(
    path: &Path,
    platform: &str,
    arch: &str,
    mut bytes: Vec<u8>,
) -> Result<()> {
    bytes.push(b'\n');
    ensure!(
        bytes.len() <= DESKTOP_CHANNEL_MANIFEST_MAX_BYTES,
        "The module manifest for {platform} {arch} is {} bytes, above the {DESKTOP_CHANNEL_MANIFEST_MAX_BYTES} byte client limit",
        bytes.len()
    );
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    fs::write(path, bytes).with_context(|| format!("Failed to write {}", path.display()))
}

fn write_desktop_channel_manifests(
    payload_root: &Path,
    channel: &str,
    build_version: &str,
    pub_date: &str,
    packed: &[DesktopPackedModule],
    targets: &[(String, String)],
) -> Result<()> {
    ensure!(
        !targets.is_empty(),
        "The payload tree under {} has no platform and arch to publish a module manifest for",
        payload_root.display()
    );
    let metadata_version = desktop_module_metadata_version(pub_date)?;
    stage_desktop_module_packages(payload_root, channel, packed)?;
    let modules = desktop_channel_manifest_entries(channel, packed);
    for (platform, arch) in targets {
        let manifest = DesktopChannelManifest {
            manifest_version: DESKTOP_CHANNEL_MANIFEST_VERSION,
            release_channel: channel.to_string(),
            platform: platform.clone(),
            arch: arch.clone(),
            build_version: build_version.to_string(),
            pub_date: pub_date.to_string(),
            metadata_version,
            shell: DesktopChannelManifestShell {
                latest_version: build_version.to_string(),
                minimum_version: desktop_module_minimum_shell_version(channel).to_string(),
            },
            modules: modules.clone(),
            required_modules: DESKTOP_REQUIRED_MODULES
                .iter()
                .map(|module| (*module).to_string())
                .collect(),
        };
        let manifest_path = desktop_channel_manifest_path(payload_root, channel, platform, arch);
        write_desktop_channel_manifest(&manifest_path, &manifest)?;
        println!(
            "Wrote the {platform} {arch} module manifest to {}",
            manifest_path.display()
        );
    }
    Ok(())
}

fn build_module_manifest_step() -> Result<()> {
    let payload_root = Path::new("payload_tree").join(DESKTOP_PAYLOAD_PREFIX);
    ensure!(
        payload_root.is_dir(),
        "Missing {}, run the build_payload step first",
        payload_root.display()
    );
    let channel = require_env("CHANNEL")?;
    let build_version = require_env("VERSION")?;
    let pub_date = require_env("PUB_DATE")?;
    let packed = read_packed_desktop_modules(&desktop_modules_dir(), &channel, &build_version)?;
    let targets = desktop_module_manifest_targets(&payload_root, &channel)?;
    write_desktop_channel_manifests(
        &payload_root,
        &channel,
        &build_version,
        &pub_date,
        &packed,
        &targets,
    )?;
    for module in &packed {
        println!(
            "{} is {} byte(s) at sha256 {}",
            module.module, module.bytes, module.sha256
        );
    }
    println!(
        "Staged {} module(s) across {} module manifest(s) for {build_version}",
        packed.len(),
        targets.len()
    );
    Ok(())
}

const DESKTOP_ALLOW_SHELL_DRIFT_ENV: &str = "ALLOW_SHELL_DRIFT";
const DESKTOP_LIVE_SHELL_FILE: &str = "live-shell.json";
const DESKTOP_LIVE_MANIFEST_TIMEOUT: Duration = Duration::from_secs(30);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
struct DesktopLiveShell {
    channel: String,
    shell_version: String,
    shell_sha: String,
    manifests: BTreeMap<String, Value>,
}

fn desktop_coordinate_key(platform: &str, arch: &str) -> String {
    format!("{platform}/{arch}")
}

fn desktop_live_manifest_url(channel: &str, platform: &str, arch: &str) -> String {
    format!(
        "{}/{DESKTOP_PAYLOAD_PREFIX}/{channel}/{platform}/{arch}/{DESKTOP_CHANNEL_MANIFEST_NAME}",
        package_origin_base()
    )
}

async fn fetch_live_channel_manifests(channel: &str) -> Result<BTreeMap<String, Value>> {
    let client = reqwest::Client::builder()
        .timeout(DESKTOP_LIVE_MANIFEST_TIMEOUT)
        .build()
        .context("Failed to build the HTTP client")?;
    let mut manifests = BTreeMap::new();
    for (platform, arch) in desktop_release_coordinates() {
        let url = desktop_live_manifest_url(channel, platform, arch);
        let manifest: Value = client
            .get(&url)
            .send()
            .await
            .with_context(|| format!("Failed to fetch {url}"))?
            .error_for_status()
            .with_context(|| format!("{url} did not serve a module manifest"))?
            .json()
            .await
            .with_context(|| format!("{url} is not JSON"))?;
        ensure!(
            manifest["release_channel"] == channel
                && manifest["platform"] == platform
                && manifest["arch"] == arch,
            "{url} describes another channel or coordinate"
        );
        manifests.insert(desktop_coordinate_key(platform, arch), manifest);
    }
    Ok(manifests)
}

fn live_shell_version(manifest: &Value) -> Option<&str> {
    manifest["shell"]["latest_version"].as_str()
}

fn common_live_shell_version(manifests: &BTreeMap<String, Value>) -> Result<String> {
    let versions = manifests
        .iter()
        .map(|(coordinate, manifest)| {
            live_shell_version(manifest)
                .map(str::to_string)
                .with_context(|| format!("The live {coordinate} manifest names no shell version"))
        })
        .collect::<Result<BTreeSet<_>>>()?;
    let mut iter = versions.iter();
    let first = iter
        .next()
        .context("No live module manifest was fetched")?
        .clone();
    ensure!(
        iter.next().is_none(),
        "The live feeds disagree on the shell version ({}), finish publishing the last full desktop release before a modules-only release",
        versions.iter().cloned().collect::<Vec<_>>().join(", ")
    );
    crate::common::parse_version_instant(&first)
        .with_context(|| format!("The live shell version {first:?} is not a CalVer"))?;
    Ok(first)
}

fn desktop_shell_source_roots(repo_root: &Path) -> Result<Vec<String>> {
    let package_path = repo_root.join("fluxer_desktop").join("package.json");
    let package: Value = serde_json::from_slice(
        &fs::read(&package_path)
            .with_context(|| format!("Failed to read {}", package_path.display()))?,
    )
    .with_context(|| format!("Failed to parse {}", package_path.display()))?;
    let workspace_dependencies = package["dependencies"]
        .as_object()
        .into_iter()
        .flatten()
        .filter(|(_, spec)| {
            spec.as_str()
                .is_some_and(|spec| spec.starts_with("workspace:"))
        })
        .map(|(name, _)| name.clone())
        .collect::<BTreeSet<_>>();
    let mut roots = vec!["fluxer_desktop/".to_string()];
    for package_dir in sorted_child_directories(&repo_root.join("packages"))? {
        let manifest_path = package_dir.join("package.json");
        if !manifest_path.is_file() {
            continue;
        }
        let manifest: Value = serde_json::from_slice(&fs::read(&manifest_path)?)
            .with_context(|| format!("Failed to parse {}", manifest_path.display()))?;
        if manifest["name"]
            .as_str()
            .is_some_and(|name| workspace_dependencies.contains(name))
        {
            roots.push(format!("packages/{}/", file_name_string(&package_dir)?));
        }
    }
    Ok(roots)
}

fn is_shell_irrelevant_path(path: &str) -> bool {
    path.contains(".test.")
        || path.contains("/__tests__/")
        || path.contains("/__fixtures__/")
        || path.ends_with(".md")
}

fn desktop_shell_drift(changed: &[String], roots: &[String]) -> Vec<String> {
    changed
        .iter()
        .filter(|path| roots.iter().any(|root| path.starts_with(root.as_str())))
        .filter(|path| !is_shell_irrelevant_path(path))
        .cloned()
        .collect()
}

async fn check_shell_drift_step() -> Result<()> {
    let channel = require_env("CHANNEL")?;
    let source_sha = require_env("SOURCE_SHA")?;
    let manifests = fetch_live_channel_manifests(&channel).await?;
    let shell_version = common_live_shell_version(&manifests)?;
    let tag = format!("fluxer-desktop-{channel}@{shell_version}");
    let shell_sha = output_text(CommandSpec::new("gh").args([
        "api",
        &format!("repos/{RELEASE_REPOSITORY}/commits/{tag}"),
        "--jq",
        ".sha",
    ]))
    .with_context(|| format!("Failed to resolve the live shell release {tag}"))?
    .trim()
    .to_string();
    ensure!(
        shell_sha.len() == 40 && shell_sha.bytes().all(|byte| byte.is_ascii_hexdigit()),
        "{tag} resolved to an invalid commit {shell_sha:?}"
    );
    let repo_root = env::current_dir().context("Failed to resolve the repository root")?;
    let roots = desktop_shell_source_roots(&repo_root)?;
    let changed =
        output_text(CommandSpec::new("git").args(["diff", "--name-only", &shell_sha, &source_sha]))
            .with_context(|| format!("Failed to diff {shell_sha} against {source_sha}"))?
            .lines()
            .map(str::to_string)
            .collect::<Vec<_>>();
    let drift = desktop_shell_drift(&changed, &roots);
    if !drift.is_empty() {
        let listed = drift.join("\n  ");
        ensure!(
            env_bool(DESKTOP_ALLOW_SHELL_DRIFT_ENV),
            "The live {channel} shell {shell_version} was built from {shell_sha}, and these shell sources changed since then:\n  {listed}\nShip a full desktop release, or set {DESKTOP_ALLOW_SHELL_DRIFT_ENV}=true if the renderer does not depend on these changes"
        );
        println!(
            "{DESKTOP_ALLOW_SHELL_DRIFT_ENV} is set, publishing modules although these shell sources changed since {shell_sha}:\n  {listed}"
        );
    }
    let live = DesktopLiveShell {
        channel: channel.clone(),
        shell_version: shell_version.clone(),
        shell_sha: shell_sha.clone(),
        manifests,
    };
    let modules_dir = desktop_modules_dir();
    fs::create_dir_all(&modules_dir)
        .with_context(|| format!("Failed to create {}", modules_dir.display()))?;
    write_json_pretty(&modules_dir.join(DESKTOP_LIVE_SHELL_FILE), &live)?;
    println!(
        "The live {channel} shell is {shell_version} from {shell_sha}, the modules from {source_sha} run on it"
    );
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn modules_only_channel_manifest(
    live: &Value,
    channel: &str,
    platform: &str,
    arch: &str,
    build_version: &str,
    pub_date: &str,
    metadata_version: u64,
    packed: &[DesktopPackedModule],
) -> Result<Value> {
    let mut manifest = live
        .as_object()
        .cloned()
        .with_context(|| format!("The live {platform} {arch} manifest is not an object"))?;
    let shell_version = live_shell_version(live)
        .with_context(|| format!("The live {platform} {arch} manifest names no shell version"))?
        .to_string();
    let modules = desktop_channel_manifest_entries(channel, packed)
        .into_iter()
        .map(|(module, mut entry)| {
            entry.minimum_shell_version = shell_version.clone();
            (module, entry)
        })
        .collect::<BTreeMap<_, _>>();
    manifest.insert(
        "manifest_version".to_string(),
        json!(DESKTOP_CHANNEL_MANIFEST_VERSION),
    );
    manifest.insert("release_channel".to_string(), json!(channel));
    manifest.insert("platform".to_string(), json!(platform));
    manifest.insert("arch".to_string(), json!(arch));
    manifest.insert("build_version".to_string(), json!(build_version));
    manifest.insert("pub_date".to_string(), json!(pub_date));
    manifest.insert("metadata_version".to_string(), json!(metadata_version));
    manifest.insert("modules".to_string(), serde_json::to_value(modules)?);
    manifest.insert(
        "required_modules".to_string(),
        json!(DESKTOP_REQUIRED_MODULES),
    );
    Ok(Value::Object(manifest))
}

fn build_modules_only_manifest_step() -> Result<()> {
    let channel = require_env("CHANNEL")?;
    let build_version = require_env("VERSION")?;
    let pub_date = require_env("PUB_DATE")?;
    let modules_dir = desktop_modules_dir();
    let live_path = modules_dir.join(DESKTOP_LIVE_SHELL_FILE);
    let live: DesktopLiveShell =
        serde_json::from_slice(&fs::read(&live_path).with_context(|| {
            format!(
                "Failed to read {}, run the check_shell_drift step first",
                live_path.display()
            )
        })?)
        .with_context(|| format!("Failed to parse {}", live_path.display()))?;
    ensure!(
        live.channel == channel,
        "{} describes the {} channel, not {channel}",
        live_path.display(),
        live.channel
    );
    let payload_root = Path::new("payload_tree").join(DESKTOP_PAYLOAD_PREFIX);
    remove_dir_if_exists(Path::new("payload_tree"))?;
    fs::create_dir_all(&payload_root)
        .with_context(|| format!("Failed to create {}", payload_root.display()))?;
    let packed = read_packed_desktop_modules(&modules_dir, &channel, &build_version)?;
    let metadata_version = desktop_module_metadata_version(&pub_date)?;
    stage_desktop_module_packages(&payload_root, &channel, &packed)?;
    for (platform, arch) in desktop_release_coordinates() {
        let key = desktop_coordinate_key(platform, arch);
        let live_manifest = live
            .manifests
            .get(&key)
            .with_context(|| format!("{} holds no live {key} manifest", live_path.display()))?;
        let manifest = modules_only_channel_manifest(
            live_manifest,
            &channel,
            platform,
            arch,
            &build_version,
            &pub_date,
            metadata_version,
            &packed,
        )?;
        let manifest_path = desktop_channel_manifest_path(&payload_root, &channel, platform, arch);
        if let Some(parent) = manifest_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create {}", parent.display()))?;
        }
        write_desktop_channel_manifest_bytes(
            &manifest_path,
            platform,
            arch,
            serde_json::to_vec(&manifest)?,
        )?;
        println!(
            "Wrote the {platform} {arch} modules-only manifest for shell {}",
            live.shell_version
        );
    }
    println!(
        "Staged {} module(s) for {build_version} on the live {channel} shell {}",
        packed.len(),
        live.shell_version
    );
    Ok(())
}

fn prepare_modules_release_assets_step() -> Result<()> {
    let channel = require_env("CHANNEL")?;
    let version = require_env("VERSION")?;
    let source_sha = require_env("SOURCE_SHA")?;
    let product = desktop_release_product(&channel)?;
    let payload_root = Path::new("payload_tree")
        .join(DESKTOP_PAYLOAD_PREFIX)
        .join(&channel);
    let release_assets = Path::new("release_assets");
    remove_dir_if_exists(release_assets)?;
    fs::create_dir_all(release_assets)?;
    let mut release_builder =
        DesktopReleaseAssetBuilder::new(&channel, &version, product, release_assets);
    add_desktop_module_release_assets(&mut release_builder, &payload_root)?;
    let (descriptor_assets, mut descriptor_modules) = release_builder.finish();
    ensure!(
        descriptor_assets.is_empty(),
        "A modules-only release staged shell assets"
    );
    descriptor_modules.sort_by(|left, right| left.storage_key.cmp(&right.storage_key));
    let descriptor = DesktopReleaseDescriptor {
        schema_version: DESKTOP_RELEASE_DESCRIPTOR_SCHEMA_VERSION,
        kind: DesktopReleaseKind::Modules,
        channel: channel.clone(),
        version: version.clone(),
        release_tag: format!("fluxer-desktop-{channel}@{version}"),
        source_sha,
        assets: Vec::new(),
        modules: descriptor_modules,
    };
    validate_desktop_release_descriptor(&descriptor, &channel, &version, &descriptor.source_sha)?;
    validate_desktop_release_module_files(&descriptor, release_assets)?;
    let descriptor_path =
        release_assets.join(desktop_release_descriptor_filename(&channel, &version)?);
    write_json_pretty(&descriptor_path, &descriptor)?;
    println!("GitHub release asset tree:");
    print_tree(release_assets, 2)
}

fn build_summary_step() -> Result<()> {
    let summary = require_env("GITHUB_STEP_SUMMARY")?;
    let display_channel = env::var("DISPLAY_CHANNEL").unwrap_or_default();
    let version = require_env("VERSION")?;
    let channel = require_env("CHANNEL")?;
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&summary)
        .with_context(|| format!("Failed to open {summary}"))?;
    writeln!(
        file,
        "## Desktop {} Release Assets Ready",
        title_case(&display_channel)
    )?;
    writeln!(
        file,
        "\n**Version:** {version}\n\n**Download prefix:** {DESKTOP_PAYLOAD_PREFIX}/{channel}/\n\n**Redirect endpoint shape:** /dl/{DESKTOP_PAYLOAD_PREFIX}/{channel}/{{plat}}/{{arch}}/{{format}}"
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

pub(crate) fn file_name_string(path: &Path) -> Result<String> {
    path.file_name()
        .and_then(OsStr::to_str)
        .map(ToOwned::to_owned)
        .ok_or_else(|| anyhow!("Path has no UTF-8 file name: {}", path.display()))
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
    use crate::common::parse_version_instant;
    use chrono::{DateTime, TimeZone, Utc};

    fn dt(year: i32, month: u32, day: u32, hour: u32, minute: u32, second: u32) -> DateTime<Utc> {
        Utc.with_ymd_and_hms(year, month, day, hour, minute, second)
            .single()
            .unwrap()
    }

    fn live_manifest(platform: &str, arch: &str, shell: &str) -> Value {
        json!({
            "manifest_version": 1,
            "release_channel": "canary",
            "platform": platform,
            "arch": arch,
            "build_version": shell,
            "pub_date": "2026-10-08T03:23:23Z",
            "metadata_version": 1791433225,
            "shell": {"latest_version": shell, "minimum_version": "0.0.0"},
            "linux_security_minimum": {"version": "2026.1001.1", "required_modules": ["fluxer_renderer"]},
            "modules": {},
            "required_modules": ["fluxer_renderer"],
        })
    }

    #[test]
    fn live_shell_version_must_agree_across_coordinates() {
        let mut manifests = BTreeMap::new();
        for (platform, arch) in desktop_release_coordinates() {
            manifests.insert(
                desktop_coordinate_key(platform, arch),
                live_manifest(platform, arch, "2026.1008.32323"),
            );
        }
        assert_eq!(
            common_live_shell_version(&manifests).unwrap(),
            "2026.1008.32323"
        );
        manifests.insert(
            "linux/arm64".to_string(),
            live_manifest("linux", "arm64", "2026.1008.12411"),
        );
        let error = common_live_shell_version(&manifests)
            .unwrap_err()
            .to_string();
        assert!(error.contains("disagree"), "{error}");
        manifests.insert("linux/arm64".to_string(), json!({"shell": {}}));
        assert!(common_live_shell_version(&manifests).is_err());
    }

    #[test]
    fn shell_drift_counts_only_shell_sources() {
        let roots = vec![
            "fluxer_desktop/".to_string(),
            "packages/desktop_ipc/".to_string(),
        ];
        let changed = [
            "fluxer_app/src/App.tsx",
            "fluxer_desktop/src/main/Updater.ts",
            "fluxer_desktop/src/main/Updater.test.mjs",
            "packages/desktop_ipc/src/Channels.ts",
            "packages/desktop_ipc/README.md",
            "packages/schema/src/Thing.ts",
        ]
        .map(str::to_string);
        assert_eq!(
            desktop_shell_drift(&changed, &roots),
            vec![
                "fluxer_desktop/src/main/Updater.ts".to_string(),
                "packages/desktop_ipc/src/Channels.ts".to_string(),
            ]
        );
    }

    #[test]
    fn shell_source_roots_follow_the_desktop_workspace_dependencies() {
        let repo = tempfile::tempdir().unwrap();
        let root = repo.path();
        fs::create_dir_all(root.join("fluxer_desktop")).unwrap();
        fs::write(
            root.join("fluxer_desktop/package.json"),
            r#"{"dependencies":{"@fluxer/desktop_ipc":"workspace:*","electron-log":"5.0.0"}}"#,
        )
        .unwrap();
        for (dir, name) in [
            ("desktop_ipc", "@fluxer/desktop_ipc"),
            ("schema", "@fluxer/schema"),
        ] {
            fs::create_dir_all(root.join("packages").join(dir)).unwrap();
            fs::write(
                root.join("packages").join(dir).join("package.json"),
                format!(r#"{{"name":"{name}"}}"#),
            )
            .unwrap();
        }
        assert_eq!(
            desktop_shell_source_roots(root).unwrap(),
            vec![
                "fluxer_desktop/".to_string(),
                "packages/desktop_ipc/".to_string()
            ]
        );
    }

    #[test]
    fn modules_only_manifest_keeps_the_live_shell_and_pins_modules_to_it() {
        let live = live_manifest("darwin", "arm64", "2026.1008.32323");
        let packed = vec![DesktopPackedModule {
            module: "fluxer_renderer".to_string(),
            sha256: "a".repeat(64),
            bytes: 42,
            directory: PathBuf::from("unused"),
        }];
        let manifest = modules_only_channel_manifest(
            &live,
            "canary",
            "darwin",
            "arm64",
            "2026.1008.90000",
            "2026-10-08T09:00:00Z",
            1791450000,
            &packed,
        )
        .unwrap();
        assert_eq!(manifest["shell"], live["shell"]);
        assert_eq!(
            manifest["linux_security_minimum"],
            live["linux_security_minimum"]
        );
        assert_eq!(manifest["build_version"], "2026.1008.90000");
        assert_eq!(manifest["metadata_version"], 1791450000);
        let renderer = &manifest["modules"]["fluxer_renderer"];
        assert_eq!(renderer["sha256"], "a".repeat(64));
        assert_eq!(renderer["minimum_shell_version"], "2026.1008.32323");
        assert!(renderer["maximum_shell_version"].is_null());
        let parsed: DesktopChannelManifest = serde_json::from_value(manifest).unwrap();
        assert_eq!(parsed.shell.latest_version, "2026.1008.32323");
    }

    #[test]
    fn released_channels_require_a_shell_with_the_in_app_update_start() {
        assert_eq!(desktop_module_minimum_shell_version("canary"), "2026.1008.42615");
        assert_eq!(desktop_module_minimum_shell_version("stable"), "2026.1008.42615");
        assert_eq!(desktop_module_minimum_shell_version("development"), "0.0.0");
    }

    #[test]
    fn bare_drive_workdir_resolves_to_the_drive_root() {
        assert_eq!(workdir_path("W:"), PathBuf::from("W:\\"));
        assert_eq!(workdir_path("w:"), PathBuf::from("w:\\"));
        assert_eq!(workdir_path("W:\\src"), PathBuf::from("W:\\src"));
        assert_eq!(
            workdir_path("/home/runner/work"),
            PathBuf::from("/home/runner/work")
        );
    }

    fn matrix_args() -> BuildDesktopArgs {
        BuildDesktopArgs {
            step: DesktopStep::SetMatrix,
            channel: None,
            skip_targets: None,
            skip_windows: Some("false".to_string()),
            skip_windows_x64: Some("false".to_string()),
            skip_windows_arm64: Some("false".to_string()),
            skip_macos: Some("false".to_string()),
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
                "{\"platform\":\"windows\",\"arch\":\"arm64\",\"os\":\"windows-2025\",\"electron_arch\":\"arm64\"}",
                "{\"platform\":\"linux\",\"arch\":\"x64\",\"os\":\"ubuntu-22.04\",\"electron_arch\":\"x64\"}",
                "{\"platform\":\"linux\",\"arch\":\"arm64\",\"os\":\"ubuntu-22.04-arm\",\"electron_arch\":\"arm64\"}",
            ]
        );
    }

    #[test]
    fn matrix_selects_one_row_per_platform_arch_by_default() {
        let selected = selected_platforms(&matrix_args()).unwrap();

        assert_eq!(selected.len(), 5);
        assert_eq!(
            selected
                .iter()
                .filter(|platform| platform.platform == "windows")
                .count(),
            2
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
                "{\"platform\":\"windows\",\"arch\":\"arm64\",\"os\":\"windows-2025\",\"electron_arch\":\"arm64\"}",
                "{\"platform\":\"linux\",\"arch\":\"x64\",\"os\":\"ubuntu-22.04\",\"electron_arch\":\"x64\"}",
                "{\"platform\":\"linux\",\"arch\":\"arm64\",\"os\":\"ubuntu-22.04-arm\",\"electron_arch\":\"arm64\"}",
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
        assert_eq!(selected.len(), 3);
    }

    #[test]
    fn matrix_skip_targets_reject_unknown_values() {
        let mut args = matrix_args();
        args.skip_targets = Some("windows-riscv".to_string());

        let error = selected_platforms(&args).unwrap_err();

        assert!(error.to_string().contains("Unknown desktop skip target"));
    }

    const BUILD_DESKTOP_WORKFLOW: &str =
        include_str!("../../../.github/workflows/build-desktop.yaml");

    fn workflow_job(job: &str) -> &'static str {
        let header = format!("\n  {job}:\n");
        let start = BUILD_DESKTOP_WORKFLOW
            .find(&header)
            .unwrap_or_else(|| panic!("build-desktop.yaml has no {job} job"))
            + header.len();
        let body = &BUILD_DESKTOP_WORKFLOW[start..];
        let end = body
            .match_indices('\n')
            .map(|(index, _)| index + 1)
            .find(|&index| body[index..].starts_with("  ") && !body[index..].starts_with("   "))
            .unwrap_or(body.len());
        &body[..end]
    }

    fn workflow_step_names(job: &str) -> Vec<&str> {
        job.lines()
            .filter_map(|line| line.strip_prefix("      - name: "))
            .collect()
    }

    #[test]
    fn every_build_desktop_workflow_step_dispatches_to_a_desktop_step() {
        let steps = BUILD_DESKTOP_WORKFLOW
            .lines()
            .filter_map(|line| line.trim().strip_prefix("--step "))
            .collect::<Vec<_>>();

        assert!(steps.contains(&"stage_handoff"));
        for step in steps {
            assert!(
                <DesktopStep as ValueEnum>::from_str(step, false).is_ok(),
                "build-desktop.yaml dispatches unknown desktop step {step}"
            );
        }
        assert!(matches!(
            <DesktopStep as ValueEnum>::from_str("stage_handoff", false),
            Ok(DesktopStep::StageHandoff)
        ));
    }

    #[test]
    fn the_github_release_is_the_only_destination_for_built_artifacts() {
        for job in ["build", "upload", "publish_release"] {
            let body = workflow_job(job);
            for forbidden in [
                "S3_BUCKET",
                "S3_ENDPOINT",
                "AWS_ACCESS_KEY_ID",
                "AWS_SECRET_ACCESS_KEY",
                "DOWNLOADS_S3",
                "_handoff/",
            ] {
                assert!(
                    !body.contains(forbidden),
                    "{job} must not reference {forbidden} now that the downloads bucket is gone"
                );
            }
        }

        assert_eq!(
            workflow_step_names(workflow_job("publish_release")),
            vec![
                "Checkout source",
                "Set up Rust toolchain (CI helpers)",
                "Download GitHub release assets",
                "Create token",
                "Publish GitHub desktop release",
            ]
        );
    }

    #[test]
    fn the_job_handoff_travels_as_github_actions_artifacts() {
        let build = workflow_job("build");
        assert!(build.contains("--step stage_handoff"));
        assert!(build.contains("name: ${{ steps.handoff.outputs.artifact_name }}"));
        assert!(build.contains("path: upload_staging"));

        let upload = workflow_job("upload");
        assert!(
            upload.contains("pattern: fluxer-desktop-${{ needs.meta.outputs.build_channel }}-*"),
            "the upload job must collect every build leg for this channel"
        );
        assert!(upload.contains("path: artifacts"));
        assert!(upload.contains("name: fluxer-desktop-release-assets"));

        let publish = workflow_job("publish_release");
        assert!(publish.contains("name: fluxer-desktop-release-assets"));
        assert!(publish.contains("path: release_assets"));

        for job in ["build", "upload", "publish_release"] {
            let body = workflow_job(job);
            for action in ["actions/upload-artifact@", "actions/download-artifact@"] {
                for line in body.lines().filter(|line| line.contains(action)) {
                    let pin = line.rsplit('@').next().unwrap_or_default();
                    assert!(
                        pin.len() == 40 && pin.bytes().all(|byte| byte.is_ascii_hexdigit()),
                        "{job} must pin {action} to a full commit sha, found {line:?}"
                    );
                }
            }
            for line in body
                .lines()
                .filter_map(|line| line.trim().strip_prefix("retention-days: "))
            {
                let days = line
                    .parse::<u32>()
                    .expect("retention-days must be a number");
                assert!(
                    days <= 7,
                    "{job} keeps a job relay artifact for {days} days, which is how staging piled up before"
                );
            }
        }
    }

    #[test]
    fn parses_handoff_artifact_dir_names() {
        assert_eq!(
            parse_artifact_dir_name("fluxer-desktop-canary-windows-arm64", "canary").unwrap(),
            ArtifactIdentity {
                platform: "windows".to_string(),
                arch: "arm64".to_string(),
                signed: false,
            }
        );
        assert!(parse_artifact_dir_name("fluxer-desktop-stable-linux-x64", "canary").is_none());
        assert_eq!(
            parse_artifact_dir_name("fluxer-desktop-canary-windows-x64-signed", "canary").unwrap(),
            ArtifactIdentity {
                platform: "windows".to_string(),
                arch: "x64".to_string(),
                signed: true,
            }
        );
    }

    #[test]
    fn handoff_artifact_name_only_marks_signed_windows_uploads() {
        assert_eq!(
            handoff_artifact_name("canary", "windows", "x64", true),
            "fluxer-desktop-canary-windows-x64-signed"
        );
        assert_eq!(
            handoff_artifact_name("canary", "linux", "x64", true),
            "fluxer-desktop-canary-linux-x64"
        );
        assert_eq!(
            handoff_artifact_name("stable", "windows", "arm64", false),
            "fluxer-desktop-stable-windows-arm64"
        );
    }

    #[test]
    fn build_channel_content_matches_expected_typescript() {
        assert_eq!(
            build_channel_content("canary"),
            "// SPDX-License-Identifier: AGPL-3.0-or-later\n\n\
export type BuildChannel = 'stable' | 'canary' | 'development';\n\n\
export const BUILD_CHANNEL = 'canary' as BuildChannel;\n\
export const IS_CANARY = BUILD_CHANNEL === 'canary';\n\
export const CHANNEL_DISPLAY_NAME = BUILD_CHANNEL;\n"
        );
        assert!(
            build_channel_content("development")
                .contains("export const BUILD_CHANNEL = 'development' as BuildChannel;")
        );
    }

    #[test]
    fn write_build_channel_file_rejects_invalid_channels() {
        let temp = tempfile::tempdir().unwrap();
        assert_eq!(
            write_build_channel_file(temp.path(), "nightly")
                .unwrap_err()
                .to_string(),
            "Invalid BUILD_CHANNEL: nightly. Must be one of: stable, canary, development."
        );
        write_build_channel_file(temp.path(), "development").unwrap();
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
                        signed: false,
                    },
                ),
                (
                    "fluxer-desktop-canary-windows-x64-signed".to_string(),
                    ArtifactIdentity {
                        platform: "windows".to_string(),
                        arch: "x64".to_string(),
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
            version: "2026.520.1".to_string(),
            pub_date: "2026-05-20T01:02:03Z".to_string(),
            minimum_system_version: Some(MACOS_MINIMUM_SYSTEM_VERSION.to_string()),
            files: BTreeMap::from([(
                "zip".to_string(),
                DesktopManifestFile::Name("Fluxer-2026.520.1-arm64.zip".to_string()),
            )]),
        };

        write_macos_releases(temp.path(), "canary", &manifest).unwrap();
        let releases: Value =
            serde_json::from_str(&fs::read_to_string(temp.path().join("RELEASES.json")).unwrap())
                .unwrap();

        assert_eq!(
            releases["releases"][0]["updateTo"]["url"],
            "https://pkgs.fluxer.com/desktop/canary/darwin/arm64/Fluxer-2026.520.1-arm64.zip"
        );
        assert!(temp.path().join("releases.json").exists());
    }

    #[test]
    fn desktop_manifest_publishes_macos_minimum_only_for_darwin() {
        let temp = tempfile::tempdir().unwrap();

        let darwin_root = temp.path().join("darwin");
        write_file(&darwin_root.join("Fluxer-2026.520.1-arm64.zip"), "zip");
        let darwin_manifest = build_desktop_manifest(
            &darwin_root,
            &PayloadManifestInput {
                channel: "stable".to_string(),
                platform: "darwin".to_string(),
                arch: "arm64".to_string(),
                version: "2026.520.1".to_string(),
                pub_date: "2026-05-20T01:02:03Z".to_string(),
            },
        )
        .unwrap();
        assert_eq!(
            darwin_manifest.minimum_system_version.as_deref(),
            Some(MACOS_MINIMUM_SYSTEM_VERSION)
        );

        let linux_root = temp.path().join("linux");
        write_file(&linux_root.join("Fluxer-2026.520.1-x64.deb"), "deb");
        let linux_manifest = build_desktop_manifest(
            &linux_root,
            &PayloadManifestInput {
                channel: "stable".to_string(),
                platform: "linux".to_string(),
                arch: "x64".to_string(),
                version: "2026.520.1".to_string(),
                pub_date: "2026-05-20T01:02:03Z".to_string(),
            },
        )
        .unwrap();
        assert_eq!(linux_manifest.minimum_system_version, None);

        let windows_root = temp.path().join("win32");
        write_file(&windows_root.join("Fluxer-Setup-2026.520.1-x64.exe"), "exe");
        let windows_manifest = build_desktop_manifest(
            &windows_root,
            &PayloadManifestInput {
                channel: "stable".to_string(),
                platform: "win32".to_string(),
                arch: "x64".to_string(),
                version: "2026.520.1".to_string(),
                pub_date: "2026-05-20T01:02:03Z".to_string(),
            },
        )
        .unwrap();
        assert_eq!(windows_manifest.minimum_system_version, None);
    }

    #[test]
    fn desktop_macos_minimum_matches_electron_builder_config() {
        let config_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../fluxer_desktop/electron-builder.config.cjs");
        let config = fs::read_to_string(&config_path)
            .expect("the electron-builder config should be readable");
        let declared = config
            .split_once("const macOSMinimumSystemVersion = '")
            .and_then(|(_, rest)| rest.split_once('\''))
            .map(|(value, _)| value)
            .expect("the electron-builder config should declare macOSMinimumSystemVersion");
        assert_eq!(declared, MACOS_MINIMUM_SYSTEM_VERSION);
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
        let stable = windows_package_config("stable", "x64").unwrap();
        assert_eq!(stable.pack_id, "fluxer_desktop");
        assert_eq!(stable.runtime, "win-x64");
        assert_eq!(stable.main_exe, "Fluxer.exe");

        let canary = windows_package_config("canary", "arm64").unwrap();
        assert_eq!(canary.pack_id, "fluxer_desktop_canary");
        assert_eq!(canary.runtime, "win-arm64");
        assert_eq!(canary.main_exe, "Fluxer Canary.exe");

        let development = windows_package_config("development", "x64").unwrap();
        assert_eq!(development.pack_id, "fluxer_desktop_development");
        assert_eq!(development.pack_title, "Fluxer Development");
        assert_eq!(development.artifact_prefix, "Fluxer-Development");
        assert_eq!(development.icon_dir, "icons-development");
        assert_eq!(development.main_exe, "Fluxer Development.exe");

        assert!(windows_package_config("nightly", "x64").is_err());
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
    fn shipped_account_switching_addons_are_required_windows_binaries() {
        for arch in ["x64", "arm64"] {
            let inventory = expected_windows_pe_inventory(arch, "Fluxer.exe");
            for stem in ["app-store", "gateway-socket"] {
                let expected = format!("{stem}.win32-{arch}-msvc.node");
                assert!(
                    inventory.contains(&expected),
                    "{expected} must be a required Windows binary, not an unlisted PE"
                );
            }
        }
    }

    #[test]
    fn shipped_account_switching_addons_are_architecture_verified_on_macos() {
        for arch in ["x64", "arm64"] {
            let targets = macos_native_runtime_targets(arch);
            for stem in ["app-store", "gateway-socket"] {
                let expected = format!("@fluxer/{stem}/{stem}.darwin-{arch}.node");
                assert!(
                    targets.iter().any(|(relative, _)| relative == &expected),
                    "{expected} must be Mach-O architecture verified"
                );
            }
        }
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

    #[test]
    fn module_package_urls_point_at_the_content_addressed_package() {
        assert_eq!(
            desktop_module_package_url("canary", "fluxer_renderer", "5c1e"),
            "https://pkgs.fluxer.com/desktop/canary/modules/fluxer_renderer/5c1e/package.br"
        );
    }

    #[test]
    fn metadata_version_is_the_publication_instant() {
        assert_eq!(
            desktop_module_metadata_version("2026-05-20T01:02:03Z").unwrap(),
            1_779_238_923
        );
        assert!(desktop_module_metadata_version("not-a-date").is_err());
    }

    #[test]
    fn module_manifest_targets_skip_the_module_tree() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();
        write_file(&root.join("canary/darwin/arm64/manifest.json"), "{}");
        write_file(&root.join("canary/darwin/x64/manifest.json"), "{}");
        write_file(&root.join("canary/win32/x64/manifest.json"), "{}");
        write_file(&root.join("canary/win32/arm64/Fluxer.exe"), "exe");
        write_file(
            &root.join("canary/modules/fluxer_renderer/5c1e/package.br"),
            "package",
        );

        assert_eq!(
            desktop_channel_manifest_targets(root, "canary").unwrap(),
            vec![
                ("darwin".to_string(), "arm64".to_string()),
                ("darwin".to_string(), "x64".to_string()),
                ("win32".to_string(), "x64".to_string()),
            ]
        );
    }

    const LAYOUT_VERSION: &str = "2026.1003.120000";
    const LAYOUT_PUB_DATE: &str = "2026-10-03T12:00:00Z";

    fn fake_packed_module(root: &Path, module: &str) -> DesktopPackedModule {
        let directory = root.join(module);
        write_file(
            &directory.join(DESKTOP_MODULE_PACKAGE_NAME),
            &format!("{module} brotli tar"),
        );
        let package = directory.join(DESKTOP_MODULE_PACKAGE_NAME);
        let sha256 = sha256_file(&package).unwrap();
        write_file(
            &directory.join(DESKTOP_MODULE_PACKAGE_CHECKSUM_NAME),
            &sha256,
        );
        write_file(&directory.join(DESKTOP_MODULE_FILE_LIST_NAME), "{}");
        DesktopPackedModule {
            module: module.to_string(),
            sha256,
            bytes: fs::metadata(&package).unwrap().len(),
            directory,
        }
    }

    fn write_fake_module_payload(root: &Path) -> (PathBuf, Vec<DesktopPackedModule>) {
        let packed = ["fluxer_renderer", "fluxer_sourcemaps"]
            .into_iter()
            .map(|module| fake_packed_module(&root.join("desktop-modules"), module))
            .collect::<Vec<_>>();
        let payload_root = root.join("payload_tree/desktop");
        let targets = desktop_release_coordinates()
            .into_iter()
            .map(|(platform, arch)| (platform.to_string(), arch.to_string()))
            .collect::<Vec<_>>();
        write_desktop_channel_manifests(
            &payload_root,
            "canary",
            LAYOUT_VERSION,
            LAYOUT_PUB_DATE,
            &packed,
            &targets,
        )
        .unwrap();
        (payload_root, packed)
    }

    #[test]
    fn module_manifests_are_one_modules_json_per_coordinate() {
        let temp = tempfile::tempdir().unwrap();
        let (payload_root, packed) = write_fake_module_payload(temp.path());
        for (platform, arch) in desktop_release_coordinates() {
            let coordinate = payload_root.join("canary").join(platform).join(arch);
            assert!(!coordinate.join("modules").exists());
            assert!(!coordinate.join(LAYOUT_VERSION).exists());
            let manifest: DesktopChannelManifest =
                serde_json::from_slice(&fs::read(coordinate.join("modules.json")).unwrap())
                    .unwrap();
            assert_eq!(manifest.platform, platform);
            assert_eq!(manifest.arch, arch);
            assert_eq!(manifest.build_version, LAYOUT_VERSION);
            assert_eq!(manifest.required_modules, vec!["fluxer_renderer"]);
            assert_eq!(
                manifest.modules.keys().collect::<Vec<_>>(),
                vec!["fluxer_renderer", "fluxer_sourcemaps"]
            );
            for module in &packed {
                let entry = &manifest.modules[&module.module];
                assert_eq!(entry.sha256, module.sha256);
                assert_eq!(entry.bytes, module.bytes);
                assert_eq!(
                    entry.url,
                    format!(
                        "https://pkgs.fluxer.com/desktop/canary/modules/{}/{}/package.br",
                        module.module, module.sha256
                    )
                );
            }
        }
        for module in &packed {
            let staged = payload_root
                .join("canary/modules")
                .join(&module.module)
                .join(&module.sha256);
            assert_eq!(
                sha256_file(&staged.join("package.br")).unwrap(),
                module.sha256
            );
            assert!(staged.join("package.br.sha256").is_file());
            assert!(staged.join("module.json").is_file());
        }
    }

    #[test]
    fn module_manifests_and_packages_become_release_assets() {
        let temp = tempfile::tempdir().unwrap();
        let (payload_root, packed) = write_fake_module_payload(temp.path());
        let release_assets = temp.path().join("release_assets");
        fs::create_dir_all(&release_assets).unwrap();
        let mut builder = DesktopReleaseAssetBuilder::new(
            "canary",
            LAYOUT_VERSION,
            "Fluxer-Canary",
            &release_assets,
        );
        add_desktop_module_release_assets(&mut builder, &payload_root.join("canary")).unwrap();
        let (assets, modules) = builder.finish();
        assert!(assets.is_empty());
        let mut expected = desktop_release_coordinates()
            .into_iter()
            .map(|(platform, arch)| {
                let token = match platform {
                    "win32" => "win",
                    "darwin" => "mac",
                    _ => "linux",
                };
                (
                    format!("desktop/canary/{platform}/{arch}/modules.json"),
                    format!("Fluxer-Canary-{LAYOUT_VERSION}-{token}-{arch}-modules.json"),
                )
            })
            .collect::<Vec<_>>();
        for module in &packed {
            expected.push((
                format!(
                    "desktop/canary/modules/{}/{}/package.br",
                    module.module, module.sha256
                ),
                format!(
                    "Fluxer-Canary-{LAYOUT_VERSION}-module-{}-{}.br",
                    module.module, module.sha256
                ),
            ));
        }
        assert_eq!(
            modules
                .iter()
                .map(|entry| (entry.storage_key.clone(), entry.release_asset.clone()))
                .collect::<Vec<_>>(),
            expected
        );
        let mut on_disk = fs::read_dir(&release_assets)
            .unwrap()
            .map(|entry| entry.unwrap().file_name().into_string().unwrap())
            .collect::<Vec<_>>();
        on_disk.sort();
        let mut names = expected
            .iter()
            .map(|(_, name)| name.clone())
            .collect::<Vec<_>>();
        names.sort();
        assert_eq!(on_disk, names);
        let descriptor = DesktopReleaseDescriptor {
            schema_version: DESKTOP_RELEASE_DESCRIPTOR_SCHEMA_VERSION,
            kind: DesktopReleaseKind::Full,
            channel: "canary".to_string(),
            version: LAYOUT_VERSION.to_string(),
            release_tag: format!("fluxer-desktop-canary@{LAYOUT_VERSION}"),
            source_sha: "0".repeat(40),
            assets,
            modules,
        };
        validate_desktop_release_module_files(&descriptor, &release_assets).unwrap();
    }

    #[test]
    fn a_missing_staged_package_stops_the_release_assets() {
        let temp = tempfile::tempdir().unwrap();
        let (payload_root, packed) = write_fake_module_payload(temp.path());
        fs::remove_file(
            payload_root
                .join("canary/modules")
                .join(&packed[0].module)
                .join(&packed[0].sha256)
                .join("package.br"),
        )
        .unwrap();
        let release_assets = temp.path().join("release_assets");
        fs::create_dir_all(&release_assets).unwrap();
        let mut builder = DesktopReleaseAssetBuilder::new(
            "canary",
            LAYOUT_VERSION,
            "Fluxer-Canary",
            &release_assets,
        );
        assert!(
            add_desktop_module_release_assets(&mut builder, &payload_root.join("canary"))
                .unwrap_err()
                .to_string()
                .starts_with("Release source is missing")
        );
    }

    #[test]
    fn coordinates_that_disagree_on_a_module_stop_the_release_assets() {
        let temp = tempfile::tempdir().unwrap();
        let (payload_root, _) = write_fake_module_payload(temp.path());
        let path = payload_root.join("canary/linux/arm64/modules.json");
        let mut manifest: DesktopChannelManifest =
            serde_json::from_slice(&fs::read(&path).unwrap()).unwrap();
        manifest.modules.get_mut("fluxer_renderer").unwrap().sha256 = "f".repeat(64);
        fs::write(&path, serde_json::to_vec(&manifest).unwrap()).unwrap();
        let release_assets = temp.path().join("release_assets");
        fs::create_dir_all(&release_assets).unwrap();
        let mut builder = DesktopReleaseAssetBuilder::new(
            "canary",
            LAYOUT_VERSION,
            "Fluxer-Canary",
            &release_assets,
        );
        assert!(
            add_desktop_module_release_assets(&mut builder, &payload_root.join("canary"))
                .unwrap_err()
                .to_string()
                .contains("across coordinates")
        );
    }

    #[test]
    fn the_module_payload_ships_only_through_the_github_release() {
        let upload = workflow_job("upload");
        assert!(!upload.contains("fluxer-desktop-module-payload"));
        assert!(!upload.contains("payload_tree/desktop/*/modules/"));
        let steps = workflow_step_names(upload);
        let manifest = steps
            .iter()
            .position(|step| *step == "Build desktop module manifest")
            .unwrap();
        assert_eq!(steps[manifest + 1], "Prepare GitHub release assets");
    }

    #[test]
    fn renderer_files_split_by_module_segment_and_source_map() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("renderer");
        write_file(&root.join("index.html"), "<html></html>");
        write_file(&root.join("assets/app.js"), "app");
        write_file(&root.join("assets/app.js.map"), "{}");
        write_file(&root.join("assets/fluxer_grammars/en.json"), "{}");

        let classified = classify_renderer_files(&root)
            .unwrap()
            .into_iter()
            .map(|file| (file.entry.path, file.module))
            .collect::<Vec<_>>();
        assert_eq!(
            classified,
            vec![
                ("assets/app.js".to_string(), "fluxer_renderer".to_string()),
                (
                    "assets/app.js.map".to_string(),
                    "fluxer_sourcemaps".to_string()
                ),
                (
                    "assets/fluxer_grammars/en.json".to_string(),
                    "fluxer_grammars".to_string()
                ),
                ("index.html".to_string(), "fluxer_renderer".to_string()),
            ]
        );
        assert!(desktop_module_for_relative_path("assets/fluxer_renderer/app.js").is_err());
        assert!(desktop_module_for_relative_path("assets/Bad/app.js").is_err());
        assert!(desktop_module_for_relative_path("assets/a/b/c.js").is_err());
    }

    #[test]
    fn dictionary_directories_map_to_module_names() {
        assert_eq!(
            desktop_dictionary_module_name("dictionary-en-gb@3.0.0").as_deref(),
            Some("fluxer_dict_en_gb")
        );
        assert_eq!(desktop_dictionary_module_name("dictionary-en@"), None);
        assert_eq!(desktop_dictionary_module_name("NOTICE.md"), None);
    }

    #[test]
    fn packed_modules_round_trip_through_brotli_tar() {
        let temp = tempfile::tempdir().unwrap();
        let module_dir = temp.path().join("fluxer_renderer");
        write_file(&module_dir.join("files/assets/app.js"), "console.log(1)");
        write_file(&module_dir.join("files/index.html"), "<html></html>");
        let files = ["assets/app.js", "index.html"]
            .into_iter()
            .map(|path| {
                let file = module_dir.join("files").join(path);
                DesktopSharedAssetFile {
                    path: path.to_string(),
                    sha256: sha256_file(&file).unwrap(),
                    bytes: fs::metadata(&file).unwrap().len(),
                }
            })
            .collect::<Vec<_>>();
        let manifest = DesktopModuleManifest {
            module: "fluxer_renderer".to_string(),
            build_version: "2026.820.1".to_string(),
            release_channel: "canary".to_string(),
            source_sha: "0".repeat(40),
            files,
        };
        write_json_pretty(&module_dir.join("module.json"), &manifest).unwrap();

        pack_one_desktop_module(&module_dir, &manifest).unwrap();

        let package = module_dir.join("package.br");
        verify_desktop_module_package(&package, &manifest).unwrap();
        assert_eq!(
            fs::read_to_string(module_dir.join("package.br.sha256")).unwrap(),
            sha256_file(&package).unwrap()
        );
        let mut missing = manifest.clone();
        missing.files.pop();
        assert!(verify_desktop_module_package(&package, &missing).is_err());
    }

    #[test]
    fn shared_asset_payload_verification_detects_tampering() {
        let temp = tempfile::tempdir().unwrap();
        let payload = temp.path().join("renderer");
        write_file(&payload.join("index.html"), "<html></html>");
        write_file(
            &payload.join("assets/0123456789abcdef.js"),
            "console.log(1)",
        );

        let files = collect_files(&payload)
            .unwrap()
            .into_iter()
            .map(|file| {
                let relative = file.strip_prefix(&payload).unwrap();
                DesktopSharedAssetFile {
                    path: shared_asset_relative_path(relative),
                    sha256: sha256_file(&file).unwrap(),
                    bytes: fs::metadata(&file).unwrap().len(),
                }
            })
            .collect::<Vec<_>>();
        let manifest = DesktopSharedAssetManifest {
            build_version: "2026.820.1".to_string(),
            release_channel: "canary".to_string(),
            source_sha: "0".repeat(40),
            files,
        };

        verify_shared_asset_payload(&payload, &manifest).unwrap();

        write_file(
            &payload.join("assets/0123456789abcdef.js"),
            "console.log(2)",
        );
        assert!(verify_shared_asset_payload(&payload, &manifest).is_err());
    }

    #[test]
    fn shared_asset_paths_never_escape_the_payload_root() {
        let root = Path::new("/tmp/desktop-shared-assets/renderer");
        assert_eq!(
            resolve_shared_asset_path(root, "assets/app.js").unwrap(),
            root.join("assets").join("app.js")
        );
        assert!(resolve_shared_asset_path(root, "../secrets").is_err());
        assert!(resolve_shared_asset_path(root, "assets//app.js").is_err());
    }

    #[test]
    fn renderer_assets_readiness_rejects_a_bundled_service_worker() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("renderer");
        write_file(&root.join("index.html"), "<html></html>");
        write_file(&root.join("assets/0123456789abcdef.js"), "console.log(1)");

        ensure_renderer_assets_ready(&root).unwrap();

        write_file(&root.join("sw.js"), "self.addEventListener");
        assert!(ensure_renderer_assets_ready(&root).is_err());
    }

    #[test]
    fn pipewire_header_overlay_presence_skips_only_a_complete_install() {
        let temp = TempDir::new().unwrap();
        let include_dir = temp.path();
        assert!(!linux_pipewire_header_overlay_present(include_dir).unwrap());

        fs::create_dir_all(include_dir.join(LINUX_PIPEWIRE_HEADER_DIR)).unwrap();
        let partial = linux_pipewire_header_overlay_present(include_dir).unwrap_err();
        assert!(partial.to_string().contains("partially installed"));

        fs::create_dir_all(include_dir.join(LINUX_SPA_HEADER_DIR)).unwrap();
        assert!(linux_pipewire_header_overlay_present(include_dir).unwrap());

        fs::remove_dir(include_dir.join(LINUX_PIPEWIRE_HEADER_DIR)).unwrap();
        let partial = linux_pipewire_header_overlay_present(include_dir).unwrap_err();
        assert!(partial.to_string().contains("partially installed"));
    }
}
