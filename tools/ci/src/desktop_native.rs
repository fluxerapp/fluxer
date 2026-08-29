// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, command_succeeds, output_text, run_command};
use crate::desktop::MACOS_UNIVERSAL_ARCH;
use anyhow::{Context, Result, anyhow, bail, ensure};
use clap::Args;
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Args, Clone)]
pub struct BuildDesktopNativeAddonArgs {
    #[arg(long)]
    addon_root: Option<PathBuf>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DesktopNativeSpecialBuild {
    None,
    Webauthn,
    WinGameCapture,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PkgConfigRequirement {
    package: &'static str,
    message: &'static str,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct DesktopNativeAddon {
    package_dir: &'static str,
    package_name: &'static str,
    crate_name: &'static str,
    node_file_stem: &'static str,
    required_platform: Option<&'static str>,
    features: &'static [&'static str],
    pkg_config: &'static [PkgConfigRequirement],
    special: DesktopNativeSpecialBuild,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BuiltNodeAddon {
    out_file: PathBuf,
    source: PathBuf,
    target: String,
}

const LINUX_AUDIO_CAPTURE_PKG_CONFIG: &[PkgConfigRequirement] = &[PkgConfigRequirement {
    package: "libpipewire-0.3",
    message: "@fluxer/linux-audio-capture requires libpipewire-0.3-dev (or your distro's equivalent: `apt install libpipewire-0.3-dev`, `dnf install pipewire-devel`, `apk add pipewire-dev`) to build. Install it on the build host, then re-run `pnpm build`.",
}];

const DESKTOP_NATIVE_ADDONS: &[DesktopNativeAddon] = &[
    DesktopNativeAddon {
        package_dir: "hardware-encoder",
        package_name: "@fluxer/hardware-encoder",
        crate_name: "fluxer_hardware_encoder",
        node_file_stem: "hardware-encoder",
        required_platform: None,
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "linux-audio-capture",
        package_name: "@fluxer/linux-audio-capture",
        crate_name: "fluxer_linux_audio_capture",
        node_file_stem: "linux-audio-capture",
        required_platform: Some("linux"),
        features: &[],
        pkg_config: LINUX_AUDIO_CAPTURE_PKG_CONFIG,
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "linux-evdev",
        package_name: "@fluxer/linux-evdev",
        crate_name: "fluxer_linux_evdev",
        node_file_stem: "linux-evdev",
        required_platform: Some("linux"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "linux-input-hook",
        package_name: "@fluxer/linux-input-hook",
        crate_name: "fluxer_linux_input_hook",
        node_file_stem: "linux-input-hook",
        required_platform: Some("linux"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "linux-notifications",
        package_name: "@fluxer/linux-notifications",
        crate_name: "fluxer_linux_notifications",
        node_file_stem: "linux-notifications",
        required_platform: Some("linux"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "linux-portals",
        package_name: "@fluxer/linux-portals",
        crate_name: "fluxer_linux_portals",
        node_file_stem: "linux-portals",
        required_platform: Some("linux"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "linux-screen-capture",
        package_name: "@fluxer/linux-screen-capture",
        crate_name: "fluxer_linux_screen_capture",
        node_file_stem: "linux-screen-capture",
        required_platform: Some("linux"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "mac-app-audio",
        package_name: "@fluxer/mac-app-audio",
        crate_name: "fluxer_mac_app_audio",
        node_file_stem: "mac-app-audio",
        required_platform: Some("darwin"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "mac-clipboard",
        package_name: "@fluxer/mac-clipboard",
        crate_name: "fluxer_mac_clipboard",
        node_file_stem: "mac-clipboard",
        required_platform: Some("darwin"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "mac-screen-capture",
        package_name: "@fluxer/mac-screen-capture",
        crate_name: "fluxer_mac_screen_capture",
        node_file_stem: "mac-screen-capture",
        required_platform: Some("darwin"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "mac-sysctl",
        package_name: "@fluxer/mac-sysctl",
        crate_name: "fluxer_mac_sysctl",
        node_file_stem: "mac-sysctl",
        required_platform: Some("darwin"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "mac-tcc",
        package_name: "@fluxer/mac-tcc",
        crate_name: "fluxer_mac_tcc",
        node_file_stem: "mac-tcc",
        required_platform: Some("darwin"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "macos-input-hook",
        package_name: "@fluxer/macos-input-hook",
        crate_name: "fluxer_macos_input_hook",
        node_file_stem: "macos-input-hook",
        required_platform: Some("darwin"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "platform-info",
        package_name: "@fluxer/platform-info",
        crate_name: "fluxer_platform_info",
        node_file_stem: "platform-info",
        required_platform: None,
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "system-hunspell",
        package_name: "@fluxer/system-hunspell",
        crate_name: "fluxer_system_hunspell",
        node_file_stem: "system-hunspell",
        required_platform: Some("linux"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "webauthn",
        package_name: "@fluxer/webauthn",
        crate_name: "fluxer_webauthn",
        node_file_stem: "webauthn",
        required_platform: None,
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::Webauthn,
    },
    DesktopNativeAddon {
        package_dir: "win-clipboard",
        package_name: "@fluxer/win-clipboard",
        crate_name: "fluxer_win_clipboard",
        node_file_stem: "win-clipboard",
        required_platform: Some("win32"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "win-game-capture",
        package_name: "@fluxer/win-game-capture",
        crate_name: "fluxer_win_game_capture",
        node_file_stem: "win-game-capture",
        required_platform: Some("win32"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::WinGameCapture,
    },
    DesktopNativeAddon {
        package_dir: "win-process-loopback",
        package_name: "@fluxer/win-process-loopback",
        crate_name: "fluxer_win_process_loopback",
        node_file_stem: "win-process-loopback",
        required_platform: Some("win32"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "win-shell",
        package_name: "@fluxer/win-shell",
        crate_name: "fluxer_win_shell",
        node_file_stem: "win-shell",
        required_platform: Some("win32"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "win-toast",
        package_name: "@fluxer/win-toast",
        crate_name: "fluxer_win_toast",
        node_file_stem: "win-toast",
        required_platform: Some("win32"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
    DesktopNativeAddon {
        package_dir: "windows-input-hook",
        package_name: "@fluxer/windows-input-hook",
        crate_name: "fluxer_windows_input_hook",
        node_file_stem: "windows-input-hook",
        required_platform: Some("win32"),
        features: &[],
        pkg_config: &[],
        special: DesktopNativeSpecialBuild::None,
    },
];

pub fn run_build_desktop_native_addon(args: BuildDesktopNativeAddonArgs) -> Result<()> {
    let addon_root = args
        .addon_root
        .map(Ok)
        .unwrap_or_else(|| env::current_dir().context("Failed to resolve current directory"))?;
    let addon = resolve_addon(&addon_root)?;
    build_desktop_native_addon(&addon_root, addon)
}

fn build_desktop_native_addon(addon_root: &Path, addon: &DesktopNativeAddon) -> Result<()> {
    let platform = current_platform();
    if let Some(required) = addon.required_platform {
        ensure!(
            platform == required,
            "{} can only be built on {}, got {}",
            addon.package_name,
            platform_display_name(required),
            platform
        );
    }
    for requirement in addon.pkg_config {
        ensure_pkg_config(requirement)?;
    }

    let built = build_rust_node_addon(addon_root, addon)?;
    match addon.special {
        DesktopNativeSpecialBuild::None => {}
        DesktopNativeSpecialBuild::Webauthn => {
            copy_webauthn_linux_shared_libraries(&built.out_file, addon_root)?
        }
        DesktopNativeSpecialBuild::WinGameCapture => {
            remove_stale_win_game_capture_outputs(addon_root, &built.out_file)?
        }
    }
    Ok(())
}

fn resolve_addon(addon_root: &Path) -> Result<&'static DesktopNativeAddon> {
    let package_dir = addon_root
        .file_name()
        .and_then(OsStr::to_str)
        .ok_or_else(|| {
            anyhow!(
                "Could not infer desktop native addon from {}",
                addon_root.display()
            )
        })?;
    DESKTOP_NATIVE_ADDONS
        .iter()
        .find(|addon| addon.package_dir == package_dir)
        .ok_or_else(|| anyhow!("Unsupported desktop native addon: {package_dir}"))
}

fn ensure_pkg_config(requirement: &PkgConfigRequirement) -> Result<()> {
    if command_succeeds(CommandSpec::new("pkg-config").args(["--exists", requirement.package])) {
        return Ok(());
    }
    bail!("{}", requirement.message)
}

fn build_rust_node_addon(addon_root: &Path, addon: &DesktopNativeAddon) -> Result<BuiltNodeAddon> {
    let platform = current_platform();
    let arch = electron_arch();
    if platform == "darwin" && arch == MACOS_UNIVERSAL_ARCH {
        let arm64 = build_rust_node_addon_for_arch(addon_root, addon, &platform, "arm64")?;
        build_rust_node_addon_for_arch(addon_root, addon, &platform, "x64")?;
        return Ok(arm64);
    }
    build_rust_node_addon_for_arch(addon_root, addon, &platform, &arch)
}

fn build_rust_node_addon_for_arch(
    addon_root: &Path,
    addon: &DesktopNativeAddon,
    platform: &str,
    arch: &str,
) -> Result<BuiltNodeAddon> {
    let tag = platform_tag(platform, arch)?;
    let target = rust_target_for_platform(platform, arch)?;
    let target_root = cargo_target_root_for_build(addon_root, platform)?;
    let mut args = vec![
        OsString::from("build"),
        OsString::from("--release"),
        OsString::from("--target"),
        OsString::from(&target),
        OsString::from("--manifest-path"),
        OsString::from("Cargo.toml"),
    ];
    if !addon.features.is_empty() {
        args.push(OsString::from("--features"));
        args.push(OsString::from(addon.features.join(",")));
    }

    let mut command = CommandSpec::new(resolve_cargo_bin())
        .args(args)
        .current_dir(addon_root);
    if env::var_os("CARGO_TARGET_DIR").is_none() && platform == "win32" {
        command = command.env("CARGO_TARGET_DIR", target_root.as_os_str());
    }
    if platform == "win32" && arch == "arm64" {
        command = configure_windows_arm64_msvc_cc(command);
    }

    println!("[buildRustNodeAddon] cwd={}", addon_root.display());
    if env::var_os("CARGO_TARGET_DIR").is_some() || platform == "win32" {
        println!(
            "[buildRustNodeAddon] CARGO_TARGET_DIR={}",
            target_root.display()
        );
    }
    run_command(command)?;

    let source = target_root
        .join(&target)
        .join("release")
        .join(cargo_dynamic_library_file_name(addon.crate_name, platform)?);
    let out_file = addon_root.join(format!("{}.{}.node", addon.node_file_stem, tag));
    ensure!(
        source.exists(),
        "expected {} to exist after cargo build",
        source.display()
    );
    fs::copy(&source, &out_file).with_context(|| {
        format!(
            "Failed to copy {} to {}",
            source.display(),
            out_file.display()
        )
    })?;
    ensure!(
        out_file.exists(),
        "expected {} to exist after copy",
        out_file.display()
    );
    sign_macos_node_addon(&out_file, platform)?;
    assert_no_redistributable_runtime_imports(&out_file, platform)?;
    assert_system32_dependent_load_flag(&out_file, platform)?;
    assert_no_disabled_win_game_capture_capabilities(&out_file, addon, platform)?;
    Ok(BuiltNodeAddon {
        out_file,
        source,
        target,
    })
}

fn configure_windows_arm64_msvc_cc(command: CommandSpec) -> CommandSpec {
    let clang = windows_llvm_tool("clang.exe");
    command
        .env("CC_aarch64_pc_windows_msvc", clang.as_os_str())
        .env("CC_aarch64-pc-windows-msvc", clang.as_os_str())
        .env("CRATE_CC_NO_DEFAULTS", "1")
        .env(
            "CFLAGS_aarch64_pc_windows_msvc",
            "--target=aarch64-pc-windows-msvc -DHAVE_BUILTIN_CTZL=1",
        )
}

fn windows_llvm_tool(file_name: &str) -> PathBuf {
    let candidate = Path::new(r"C:\Program Files\LLVM\bin").join(file_name);
    if candidate.exists() {
        candidate
    } else {
        PathBuf::from(file_name)
    }
}

fn sign_macos_node_addon(out_file: &Path, platform: &str) -> Result<()> {
    if platform != "darwin" {
        return Ok(());
    }

    run_command(CommandSpec::new("codesign").args([
        "--force",
        "--sign",
        "-",
        out_file.to_string_lossy().as_ref(),
    ]))
    .with_context(|| format!("Failed to ad-hoc sign {}", out_file.display()))
}

fn current_platform() -> String {
    match env::consts::OS {
        "macos" => "darwin",
        "windows" => "win32",
        other => other,
    }
    .to_string()
}

fn platform_display_name(platform: &str) -> &'static str {
    match platform {
        "darwin" => "macOS",
        "win32" => "Windows",
        "linux" => "Linux",
        _ => "the required platform",
    }
}

fn electron_arch() -> String {
    env::var("ELECTRON_ARCH")
        .ok()
        .filter(|value| !value.is_empty())
        .or_else(|| {
            env::var("npm_config_arch")
                .ok()
                .filter(|value| !value.is_empty())
        })
        .unwrap_or_else(|| match env::consts::ARCH {
            "x86_64" => "x64".to_string(),
            "aarch64" => "arm64".to_string(),
            other => other.to_string(),
        })
}

fn platform_tag(platform: &str, arch: &str) -> Result<String> {
    match platform {
        "darwin" => Ok(format!("darwin-{arch}")),
        "win32" => Ok(format!("win32-{arch}-msvc")),
        "linux" => Ok(format!("linux-{arch}-gnu")),
        _ => bail!("Unsupported platform: {platform}"),
    }
}

fn rust_target_for_platform(platform: &str, arch: &str) -> Result<String> {
    ensure!(
        arch == "x64" || arch == "arm64",
        "Unsupported architecture: {arch}"
    );
    match (platform, arch) {
        ("darwin", "arm64") => Ok("aarch64-apple-darwin".to_string()),
        ("darwin", "x64") => Ok("x86_64-apple-darwin".to_string()),
        ("win32", "arm64") => Ok("aarch64-pc-windows-msvc".to_string()),
        ("win32", "x64") => Ok("x86_64-pc-windows-msvc".to_string()),
        ("linux", "arm64") => Ok("aarch64-unknown-linux-gnu".to_string()),
        ("linux", "x64") => Ok("x86_64-unknown-linux-gnu".to_string()),
        _ => bail!("Unsupported platform: {platform}"),
    }
}

fn cargo_dynamic_library_file_name(crate_name: &str, platform: &str) -> Result<String> {
    let normalized = crate_name.replace('-', "_");
    match platform {
        "win32" => Ok(format!("{normalized}.dll")),
        "darwin" => Ok(format!("lib{normalized}.dylib")),
        "linux" => Ok(format!("lib{normalized}.so")),
        _ => bail!("Unsupported platform: {platform}"),
    }
}

fn resolve_cargo_bin() -> OsString {
    if let Some(cargo) = env::var_os("CARGO").filter(|value| !value.is_empty()) {
        return cargo;
    }
    if current_platform() != "win32" {
        return OsString::from("cargo");
    }
    let user_profile = env::var_os("USERPROFILE")
        .filter(|value| !value.is_empty())
        .or_else(|| {
            let drive = env::var_os("HOMEDRIVE")?;
            let path = env::var_os("HOMEPATH").unwrap_or_default();
            let mut combined = PathBuf::from(drive);
            combined.push(path);
            Some(combined.into_os_string())
        });
    if let Some(profile) = user_profile {
        let candidate = PathBuf::from(profile).join(".cargo/bin/cargo.exe");
        if candidate.exists() {
            return candidate.into_os_string();
        }
    }
    if let Some(path) = env::var_os("PATH") {
        for entry in env::split_paths(&path) {
            let candidate = entry.join("cargo.exe");
            if candidate.exists() {
                return candidate.into_os_string();
            }
        }
    }
    OsString::from("cargo.exe")
}

fn cargo_target_root_for_build(addon_root: &Path, platform: &str) -> Result<PathBuf> {
    if let Some(explicit) = env::var_os("CARGO_TARGET_DIR").filter(|value| !value.is_empty()) {
        let explicit = PathBuf::from(explicit);
        return Ok(if explicit.is_absolute() {
            explicit
        } else {
            addon_root.join(explicit)
        });
    }
    if platform == "win32" {
        return default_windows_cargo_target_root(addon_root);
    }
    Ok(addon_root.join("target"))
}

fn default_windows_cargo_target_root(addon_root: &Path) -> Result<PathBuf> {
    let base = env::var_os("FLUXER_CARGO_TARGET_BASE")
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            let drive = env::var("SystemDrive").unwrap_or_else(|_| "C:".to_string());
            PathBuf::from(format!("{}\\fcargo", drive.trim_end_matches(['\\', '/'])))
        });
    let resolved = fs::canonicalize(addon_root).unwrap_or_else(|_| addon_root.to_path_buf());
    let key = hex::encode(Sha256::digest(resolved.to_string_lossy().as_bytes()));
    Ok(base.join(&key[..12]))
}

fn copy_webauthn_linux_shared_libraries(native_path: &Path, root: &Path) -> Result<()> {
    if current_platform() != "linux" {
        return Ok(());
    }
    for dependency in collect_linux_shared_library_dependencies(native_path)? {
        let target_path = root.join(&dependency.library_name);
        if target_path
            .exists()
            .then(|| fs::canonicalize(&target_path))
            .transpose()
            .with_context(|| format!("Failed to resolve {}", target_path.display()))?
            .as_deref()
            == Some(dependency.source_path.as_path())
        {
            continue;
        }
        fs::copy(&dependency.source_path, &target_path).with_context(|| {
            format!(
                "Failed to copy {} to {}",
                dependency.source_path.display(),
                target_path.display()
            )
        })?;
        println!(
            "[webauthn] bundled Linux runtime library {}",
            dependency.library_name
        );
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LinuxSharedLibraryDependency {
    library_name: String,
    source_path: PathBuf,
}

fn collect_linux_shared_library_dependencies(
    native_path: &Path,
) -> Result<Vec<LinuxSharedLibraryDependency>> {
    let output = output_text(CommandSpec::new("ldd").arg(native_path.as_os_str()))?;
    let mut dependencies = BTreeMap::new();
    for line in output.lines() {
        let Some((name_part, target_part)) = line.trim().split_once("=>") else {
            continue;
        };
        let library_name = name_part.split_whitespace().next().unwrap_or("");
        let library_path = target_part.split_whitespace().next().unwrap_or("");
        if library_name.is_empty()
            || library_path.is_empty()
            || !library_path.starts_with('/')
            || !should_bundle_linux_library(library_name)
        {
            continue;
        }
        dependencies.insert(
            library_name.to_string(),
            LinuxSharedLibraryDependency {
                library_name: library_name.to_string(),
                source_path: fs::canonicalize(library_path)
                    .with_context(|| format!("Failed to resolve {library_path}"))?,
            },
        );
    }
    Ok(dependencies.into_values().collect())
}

fn should_bundle_linux_library(library_name: &str) -> bool {
    ![
        "ld-linux",
        "libc.so",
        "libdl.so",
        "libgcc_s.so",
        "libm.so",
        "libpthread.so",
        "libresolv.so",
        "librt.so",
        "linux-vdso.so",
    ]
    .iter()
    .any(|prefix| library_name.starts_with(prefix))
}

fn remove_stale_win_game_capture_outputs(root: &Path, primary_node: &Path) -> Result<()> {
    let primary_node_name = primary_node
        .file_name()
        .and_then(OsStr::to_str)
        .ok_or_else(|| {
            anyhow!(
                "Invalid win-game-capture output path: {}",
                primary_node.display()
            )
        })?;
    let mut stale = fs::read_dir(root)
        .with_context(|| format!("Failed to read {}", root.display()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::result::Result<Vec<_>, _>>()?;
    stale.retain(|path| {
        if !path.is_file() {
            return false;
        }
        let Some(file_name) = path.file_name().and_then(OsStr::to_str) else {
            return false;
        };
        file_name.starts_with("fluxer-game-hook.")
            || file_name.starts_with("fluxer-inject-helper.")
            || file_name.starts_with("fluxer-vulkan-layer.")
            || (file_name.starts_with("win-game-capture.")
                && file_name.ends_with(".node")
                && file_name != primary_node_name)
    });
    stale.sort();
    for path in stale {
        fs::remove_file(&path).with_context(|| format!("Failed to remove {}", path.display()))?;
        println!("[win-game-capture] removed stale output {}", path.display());
    }
    Ok(())
}

fn assert_no_redistributable_runtime_imports(node_file_path: &Path, platform: &str) -> Result<()> {
    if platform != "win32" {
        return Ok(());
    }
    let offenders = find_redistributable_runtime_imports(node_file_path);
    if offenders.is_empty() {
        return Ok(());
    }
    bail!(
        "{} imports redistributable Microsoft runtime DLL(s) that are not on a clean Windows install:\n{}\nEnsure the crate is built with +crt-static so the C/C++ runtime is statically linked into the addon.\nSee fluxer_desktop/native/.cargo/config.toml for the cargo configuration that enables this for MSVC targets.",
        node_file_path.display(),
        offenders
            .iter()
            .map(|dll| format!("  - {dll}"))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

const DISABLED_WIN_GAME_CAPTURE_BINARY_MARKERS: &[&[u8]] = &[
    b"CreateRemoteThread",
    b"VirtualAllocEx",
    b"VirtualFreeEx",
    b"WriteProcessMemory",
    b"SetWindowsHookExW",
    b"fluxer-inject-helper.",
];

const LOAD_LIBRARY_SEARCH_SYSTEM32: u16 = 0x0800;

fn read_dependent_load_flags(file_path: &Path) -> Option<u16> {
    let buffer = fs::read(file_path).ok()?;
    let pe = PeFile { buffer };
    let header = pe.parse_header()?;
    if header.load_config_directory_rva == 0 {
        return None;
    }
    let offset = pe.rva_to_offset(&header.sections, header.load_config_directory_rva)?;
    if offset + 4 > pe.buffer.len() {
        return None;
    }
    let size = pe.u32(offset) as usize;
    let flags_offset = if header.is_pe32_plus { 0x4e } else { 0x36 };
    if size <= flags_offset + 2 || offset + flags_offset + 2 > pe.buffer.len() {
        return None;
    }
    Some(pe.u16(offset + flags_offset))
}

fn assert_system32_dependent_load_flag(node_file_path: &Path, platform: &str) -> Result<()> {
    if platform != "win32" {
        return Ok(());
    }
    let flags = read_dependent_load_flags(node_file_path);
    ensure!(
        flags.is_some_and(|value| value & LOAD_LIBRARY_SEARCH_SYSTEM32 != 0),
        "{} does not set LOAD_LIBRARY_SEARCH_SYSTEM32 in its load config DependentLoadFlags (read {}).\nEvery import of a shipped addon must resolve from System32 so a planted DLL cannot be loaded in its place.\nSee fluxer_desktop/native/.cargo/config.toml for the /DEPENDENTLOADFLAG:0x800 link argument that sets it.",
        node_file_path.display(),
        flags.map_or_else(
            || "no load config".to_string(),
            |value| format!("{value:#06x}")
        )
    );
    Ok(())
}

fn assert_no_disabled_win_game_capture_capabilities(
    node_file_path: &Path,
    addon: &DesktopNativeAddon,
    platform: &str,
) -> Result<()> {
    if platform != "win32" || addon.special != DesktopNativeSpecialBuild::WinGameCapture {
        return Ok(());
    }
    let binary = fs::read(node_file_path)
        .with_context(|| format!("Failed to read {}", node_file_path.display()))?;
    let present = DISABLED_WIN_GAME_CAPTURE_BINARY_MARKERS
        .iter()
        .copied()
        .filter(|marker| binary.windows(marker.len()).any(|window| window == *marker))
        .map(|marker| String::from_utf8_lossy(marker).into_owned())
        .collect::<Vec<_>>();
    ensure!(
        present.is_empty(),
        "{} contains disabled game-capture injection capabilities:\n{}",
        node_file_path.display(),
        present.join("\n")
    );
    Ok(())
}

fn find_redistributable_runtime_imports(file_path: &Path) -> Vec<String> {
    read_pe_imports(file_path)
        .into_iter()
        .filter(|dll| is_redistributable_runtime_import(dll))
        .collect()
}

fn is_redistributable_runtime_import(dll_name: &str) -> bool {
    let lower = dll_name.to_ascii_lowercase();
    matches_numbered_runtime_dll(&lower, "vcruntime", true)
        || matches_numbered_runtime_dll(&lower, "msvcp", true)
        || matches_numbered_runtime_dll(&lower, "msvcr", true)
        || matches_numbered_runtime_dll(&lower, "concrt", false)
        || matches_numbered_runtime_dll(&lower, "vcamp", false)
        || matches_numbered_runtime_dll(&lower, "vcomp", false)
}

fn matches_numbered_runtime_dll(value: &str, prefix: &str, allow_suffix: bool) -> bool {
    let Some(rest) = value.strip_prefix(prefix) else {
        return false;
    };
    let Some(rest) = rest.strip_suffix(".dll") else {
        return false;
    };
    let (digits, suffix) = rest
        .char_indices()
        .find(|(_, ch)| !ch.is_ascii_digit())
        .map(|(index, _)| rest.split_at(index))
        .unwrap_or((rest, ""));
    !digits.is_empty()
        && suffix.strip_prefix('_').is_none_or(|tail| {
            allow_suffix && !tail.is_empty() && tail.chars().all(|ch| ch.is_ascii_digit())
        })
}

#[derive(Debug)]
struct PeFile {
    buffer: Vec<u8>,
}

#[derive(Debug)]
struct PeHeader {
    sections: Vec<PeSection>,
    import_directory_rva: u32,
    load_config_directory_rva: u32,
    is_pe32_plus: bool,
}

#[derive(Debug)]
struct PeSection {
    virtual_size: u32,
    virtual_address: u32,
    raw_size: u32,
    raw_pointer: u32,
}

fn read_pe_imports(file_path: &Path) -> Vec<String> {
    let Ok(buffer) = fs::read(file_path) else {
        return Vec::new();
    };
    let pe = PeFile { buffer };
    let Some(header) = pe.parse_header() else {
        return Vec::new();
    };
    if header.import_directory_rva == 0 {
        return Vec::new();
    }
    let Some(import_table_offset) = pe.rva_to_offset(&header.sections, header.import_directory_rva)
    else {
        return Vec::new();
    };
    let mut imports = Vec::new();
    for index in 0..1024 {
        let base = import_table_offset + index * 20;
        if base + 20 > pe.buffer.len() {
            break;
        }
        let lookup_rva = pe.u32(base);
        let name_rva = pe.u32(base + 12);
        let iat_rva = pe.u32(base + 16);
        if lookup_rva == 0 && name_rva == 0 && iat_rva == 0 {
            break;
        }
        let Some(name_offset) = pe.rva_to_offset(&header.sections, name_rva) else {
            continue;
        };
        let name = pe.read_c_string(name_offset);
        if !name.is_empty() && !imports.contains(&name) {
            imports.push(name);
        }
    }
    imports
}

impl PeFile {
    fn parse_header(&self) -> Option<PeHeader> {
        if self.buffer.len() < 0x40 {
            return None;
        }
        let pe_offset = self.u32(0x3c) as usize;
        if pe_offset == 0 || pe_offset + 24 >= self.buffer.len() || self.u32(pe_offset) != 0x4550 {
            return None;
        }
        let coff_offset = pe_offset + 4;
        let number_of_sections = self.u16(coff_offset + 2) as usize;
        let size_of_optional_header = self.u16(coff_offset + 16) as usize;
        let optional_header_offset = coff_offset + 20;
        if optional_header_offset + size_of_optional_header > self.buffer.len() {
            return None;
        }
        let magic = self.u16(optional_header_offset);
        let is_pe32_plus = magic == 0x20b;
        if magic != 0x10b && !is_pe32_plus {
            return None;
        }
        let data_directories_offset = optional_header_offset + if is_pe32_plus { 112 } else { 96 };
        let import_directory_entry_offset = data_directories_offset + 8;
        if import_directory_entry_offset + 8 > self.buffer.len() {
            return None;
        }
        let import_directory_rva = self.u32(import_directory_entry_offset);
        let load_config_directory_entry_offset = data_directories_offset + 10 * 8;
        if load_config_directory_entry_offset + 8 > self.buffer.len() {
            return None;
        }
        let load_config_directory_rva = self.u32(load_config_directory_entry_offset);
        let section_table_offset = optional_header_offset + size_of_optional_header;
        let mut sections = Vec::new();
        for index in 0..number_of_sections {
            let base = section_table_offset + index * 40;
            if base + 40 > self.buffer.len() {
                return None;
            }
            sections.push(PeSection {
                virtual_size: self.u32(base + 8),
                virtual_address: self.u32(base + 12),
                raw_size: self.u32(base + 16),
                raw_pointer: self.u32(base + 20),
            });
        }
        Some(PeHeader {
            sections,
            import_directory_rva,
            load_config_directory_rva,
            is_pe32_plus,
        })
    }

    fn rva_to_offset(&self, sections: &[PeSection], rva: u32) -> Option<usize> {
        for section in sections {
            let span = section.virtual_size.max(section.raw_size);
            if rva >= section.virtual_address && rva < section.virtual_address + span {
                return Some((rva - section.virtual_address + section.raw_pointer) as usize);
            }
        }
        None
    }

    fn read_c_string(&self, offset: usize) -> String {
        let mut end = offset;
        while end < self.buffer.len() && self.buffer[end] != 0 {
            end += 1;
        }
        String::from_utf8_lossy(&self.buffer[offset..end]).to_string()
    }

    fn u16(&self, offset: usize) -> u16 {
        u16::from_le_bytes([self.buffer[offset], self.buffer[offset + 1]])
    }

    fn u32(&self, offset: usize) -> u32 {
        u32::from_le_bytes([
            self.buffer[offset],
            self.buffer[offset + 1],
            self.buffer[offset + 2],
            self.buffer[offset + 3],
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn addon_mapping_covers_expected_packages() {
        let names = DESKTOP_NATIVE_ADDONS
            .iter()
            .map(|addon| addon.package_dir)
            .collect::<Vec<_>>();

        assert!(names.contains(&"linux-audio-capture"));
        assert!(names.contains(&"webauthn"));
        assert!(names.contains(&"win-game-capture"));
        assert_eq!(DESKTOP_NATIVE_ADDONS.len(), 22);
    }

    #[test]
    fn platform_tags_match_legacy_node_helper() {
        assert_eq!(platform_tag("linux", "x64").unwrap(), "linux-x64-gnu");
        assert_eq!(platform_tag("darwin", "arm64").unwrap(), "darwin-arm64");
        assert_eq!(platform_tag("win32", "x64").unwrap(), "win32-x64-msvc");
    }

    #[test]
    fn rust_targets_match_legacy_node_helper() {
        assert_eq!(
            rust_target_for_platform("linux", "arm64").unwrap(),
            "aarch64-unknown-linux-gnu"
        );
        assert_eq!(
            rust_target_for_platform("darwin", "x64").unwrap(),
            "x86_64-apple-darwin"
        );
        assert_eq!(
            rust_target_for_platform("win32", "arm64").unwrap(),
            "aarch64-pc-windows-msvc"
        );
    }

    #[test]
    fn dynamic_library_names_match_legacy_node_helper() {
        assert_eq!(
            cargo_dynamic_library_file_name("fluxer_webauthn", "linux").unwrap(),
            "libfluxer_webauthn.so"
        );
        assert_eq!(
            cargo_dynamic_library_file_name("fluxer-webauthn", "darwin").unwrap(),
            "libfluxer_webauthn.dylib"
        );
        assert_eq!(
            cargo_dynamic_library_file_name("fluxer_webauthn", "win32").unwrap(),
            "fluxer_webauthn.dll"
        );
    }

    #[test]
    fn redistributable_runtime_import_detection_matches_legacy_patterns() {
        assert!(is_redistributable_runtime_import("vcruntime140.dll"));
        assert!(is_redistributable_runtime_import("vcruntime140_1.dll"));
        assert!(is_redistributable_runtime_import("msvcp140.dll"));
        assert!(is_redistributable_runtime_import("concrt140.dll"));
        assert!(!is_redistributable_runtime_import("kernel32.dll"));
        assert!(!is_redistributable_runtime_import("vcruntime.dll"));
    }

    fn synthetic_pe(pe32_plus: bool, dependent_load_flags: u16) -> Vec<u8> {
        let opt_size: usize = if pe32_plus { 240 } else { 224 };
        let load_config_size: usize = if pe32_plus { 0x140 } else { 0xc0 };
        let flags_offset: usize = if pe32_plus { 0x4e } else { 0x36 };
        let pe_offset: usize = 0x80;
        let opt_offset = pe_offset + 4 + 20;
        let section_offset = opt_offset + opt_size;
        let raw_pointer = section_offset + 40;
        let virtual_address: u32 = 0x1000;

        let mut buffer = vec![0u8; raw_pointer + load_config_size];
        buffer[0..2].copy_from_slice(b"MZ");
        buffer[0x3c..0x40].copy_from_slice(&(pe_offset as u32).to_le_bytes());
        buffer[pe_offset..pe_offset + 4].copy_from_slice(&0x4550u32.to_le_bytes());
        buffer[pe_offset + 6..pe_offset + 8].copy_from_slice(&1u16.to_le_bytes());
        buffer[pe_offset + 20..pe_offset + 22].copy_from_slice(&(opt_size as u16).to_le_bytes());
        let magic: u16 = if pe32_plus { 0x20b } else { 0x10b };
        buffer[opt_offset..opt_offset + 2].copy_from_slice(&magic.to_le_bytes());

        let data_dirs = opt_offset + if pe32_plus { 112 } else { 96 };
        let load_config_entry = data_dirs + 10 * 8;
        buffer[load_config_entry..load_config_entry + 4]
            .copy_from_slice(&virtual_address.to_le_bytes());
        buffer[load_config_entry + 4..load_config_entry + 8]
            .copy_from_slice(&(load_config_size as u32).to_le_bytes());

        buffer[section_offset + 8..section_offset + 12]
            .copy_from_slice(&(load_config_size as u32).to_le_bytes());
        buffer[section_offset + 12..section_offset + 16]
            .copy_from_slice(&virtual_address.to_le_bytes());
        buffer[section_offset + 16..section_offset + 20]
            .copy_from_slice(&(load_config_size as u32).to_le_bytes());
        buffer[section_offset + 20..section_offset + 24]
            .copy_from_slice(&(raw_pointer as u32).to_le_bytes());

        buffer[raw_pointer..raw_pointer + 4]
            .copy_from_slice(&(load_config_size as u32).to_le_bytes());
        let flags_at = raw_pointer + flags_offset;
        buffer[flags_at..flags_at + 2].copy_from_slice(&dependent_load_flags.to_le_bytes());
        buffer
    }

    #[test]
    fn dependent_load_flags_are_read_from_both_pe_widths() {
        for pe32_plus in [false, true] {
            let dir = tempfile::tempdir().expect("tempdir");
            let set = dir.path().join("set.bin");
            let unset = dir.path().join("unset.bin");
            fs::write(&set, synthetic_pe(pe32_plus, LOAD_LIBRARY_SEARCH_SYSTEM32)).expect("write");
            fs::write(&unset, synthetic_pe(pe32_plus, 0)).expect("write");

            assert_eq!(
                read_dependent_load_flags(&set),
                Some(LOAD_LIBRARY_SEARCH_SYSTEM32),
                "pe32_plus={pe32_plus}"
            );
            assert_eq!(
                read_dependent_load_flags(&unset),
                Some(0),
                "pe32_plus={pe32_plus}"
            );
            assert!(assert_system32_dependent_load_flag(&set, "win32").is_ok());
            assert!(assert_system32_dependent_load_flag(&unset, "win32").is_err());
            assert!(assert_system32_dependent_load_flag(&unset, "linux").is_ok());
        }
    }

    #[test]
    fn linux_library_bundling_denylist_matches_legacy_prefixes() {
        assert!(!should_bundle_linux_library("libc.so.6"));
        assert!(!should_bundle_linux_library("ld-linux-x86-64.so.2"));
        assert!(should_bundle_linux_library("libfido2.so.1"));
    }
}
