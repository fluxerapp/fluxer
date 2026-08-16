// SPDX-License-Identifier: AGPL-3.0-or-later
use std::env;

fn main() {
    println!("cargo:rustc-check-cfg=cfg(fluxer_linux_nvenc)");
    println!("cargo:rustc-check-cfg=cfg(fluxer_windows_nvenc)");
    println!("cargo:rustc-check-cfg=cfg(fluxer_windows_nvenc_encoder)");
    println!("cargo:rustc-check-cfg=cfg(fluxer_macos_videotoolbox)");

    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    let is_macos = target_os == "macos";
    let has_linux_nvenc = target_os == "linux" && supports_nvenc_arch();
    let has_windows_nvenc = target_os == "windows" && supports_nvenc_arch();
    let has_windows_nvenc_encoder = has_windows_nvenc;

    println!(
        "cargo:rustc-env=FLUXER_LINUX_NVENC_COMPILED={}",
        if has_linux_nvenc { "1" } else { "0" }
    );
    println!(
        "cargo:rustc-env=FLUXER_WINDOWS_NVENC_COMPILED={}",
        if has_windows_nvenc { "1" } else { "0" }
    );
    println!(
        "cargo:rustc-env=FLUXER_WINDOWS_NVENC_ENCODER_COMPILED={}",
        if has_windows_nvenc_encoder { "1" } else { "0" }
    );
    if has_linux_nvenc {
        println!("cargo:rustc-cfg=fluxer_linux_nvenc");
    }
    if has_windows_nvenc {
        println!("cargo:rustc-cfg=fluxer_windows_nvenc");
    }
    if has_windows_nvenc_encoder {
        println!("cargo:rustc-cfg=fluxer_windows_nvenc_encoder");
    }
    println!(
        "cargo:rustc-env=FLUXER_MACOS_VIDEOTOOLBOX_COMPILED={}",
        if is_macos { "1" } else { "0" }
    );
    if is_macos {
        println!("cargo:rustc-cfg=fluxer_macos_videotoolbox");
        println!("cargo:rustc-link-lib=framework=CoreFoundation");
        println!("cargo:rustc-link-lib=framework=CoreMedia");
        println!("cargo:rustc-link-lib=framework=VideoToolbox");
    }

    napi_build::setup();
}

fn supports_nvenc_arch() -> bool {
    let Ok(arch) = env::var("CARGO_CFG_TARGET_ARCH") else {
        return false;
    };
    matches!(arch.as_str(), "x86_64" | "i686" | "aarch64") || arch.contains("arm")
}
