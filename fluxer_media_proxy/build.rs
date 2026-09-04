// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{collections::HashSet, path::Path, path::PathBuf, process::Command};

fn is_distribution_lib_dir(path: &Path) -> bool {
    let Some(path) = path.to_str() else {
        return false;
    };
    path.starts_with("/usr/lib") || path.starts_with("/lib")
}

const NATIVE_SHIM_SOURCES: [&str; 16] = [
    "src/webp_animation.c",
    "src/native_shim/av_input.c",
    "src/native_shim/av_frame_rgba.c",
    "src/native_shim/vips_image.c",
    "src/native_shim/gif_validation.c",
    "src/native_shim/gif_timing.c",
    "src/native_shim/gif_filter.c",
    "src/native_shim/gif_transform.c",
    "src/native_shim/apng_validation.c",
    "src/native_shim/animation_decode.c",
    "src/native_shim/heif_pixels.c",
    "src/native_shim/heif_decode.c",
    "src/native_shim/animated_webp.c",
    "src/native_shim/video_frame.c",
    "src/native_shim/nsfw_frame.c",
    "src/native_shim/nsfw_webp.c",
];

const NATIVE_SHIM_HEADERS: [&str; 3] = [
    "src/vips_shim.h",
    "src/webp_animation.h",
    "src/native_shim/native_shim_internal.h",
];

const NATIVE_BUILD_ENVIRONMENT: [&str; 6] = [
    "CARGO_CFG_TARGET_OS",
    "HOMEBREW_PREFIX",
    "PKG_CONFIG",
    "PKG_CONFIG_LIBDIR",
    "PKG_CONFIG_PATH",
    "PKG_CONFIG_SYSROOT_DIR",
];

fn homebrew_prefix() -> PathBuf {
    if let Ok(prefix) = std::env::var("HOMEBREW_PREFIX") {
        return PathBuf::from(prefix);
    }
    let output = Command::new("brew")
        .arg("--prefix")
        .output()
        .expect("libyuv needs a Homebrew prefix: set HOMEBREW_PREFIX or install Homebrew");
    assert!(output.status.success(), "`brew --prefix` failed");
    PathBuf::from(
        String::from_utf8(output.stdout)
            .expect("`brew --prefix` printed invalid UTF-8")
            .trim(),
    )
}

fn main() {
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    for variable in NATIVE_BUILD_ENVIRONMENT {
        println!("cargo:rerun-if-env-changed={variable}");
    }
    for header in NATIVE_SHIM_HEADERS {
        println!("cargo:rerun-if-changed={header}");
    }
    for source in NATIVE_SHIM_SOURCES {
        println!("cargo:rerun-if-changed={source}");
    }

    let mut shim = cc::Build::new();
    shim.include("src")
        .flag("-std=c11")
        .warnings_into_errors(true);
    for source in NATIVE_SHIM_SOURCES {
        shim.file(source);
    }

    let mut link_paths: Vec<PathBuf> = Vec::new();
    let mut link_files: Vec<PathBuf> = Vec::new();
    let mut framework_paths: Vec<PathBuf> = Vec::new();
    let mut frameworks: Vec<String> = Vec::new();
    let mut libs: Vec<String> = Vec::new();
    let mut ld_args: Vec<Vec<String>> = Vec::new();

    for lib in [
        "vips",
        "libheif",
        "libavformat",
        "libavcodec",
        "libavfilter",
        "libavutil",
        "libswscale",
        "lcms2",
        "libwebpdemux",
        "libwebpmux",
        "libwebp",
    ] {
        let probed = pkg_config::Config::new()
            .cargo_metadata(false)
            .probe(lib)
            .unwrap_or_else(|err| panic!("pkg-config could not find {lib}: {err}"));
        for include in probed.include_paths {
            shim.include(include);
        }
        link_paths.extend(probed.link_paths);
        link_files.extend(probed.link_files);
        framework_paths.extend(probed.framework_paths);
        frameworks.extend(probed.frameworks);
        libs.extend(probed.libs);
        ld_args.extend(probed.ld_args);
    }

    libs.push("yuv".into());

    if target_os == "macos" {
        let prefix = homebrew_prefix();
        let header = prefix.join("include/libyuv.h");
        assert!(
            header.is_file(),
            "libyuv header missing at {}: run `brew install libyuv`",
            header.display()
        );
        let has_library = ["lib/libyuv.dylib", "lib/libyuv.a"]
            .iter()
            .any(|candidate| prefix.join(candidate).is_file());
        assert!(
            has_library,
            "libyuv library missing under {}: run `brew install libyuv`",
            prefix.join("lib").display()
        );
        shim.include(prefix.join("include"));
        link_paths.push(prefix.join("lib"));
    }

    shim.compile("fluxer_vips_shim");

    let (source_built_link_paths, distribution_link_paths): (Vec<PathBuf>, Vec<PathBuf>) =
        link_paths
            .into_iter()
            .partition(|path| !is_distribution_lib_dir(path));

    let mut emitted_link_paths: HashSet<PathBuf> = HashSet::new();
    for path in source_built_link_paths
        .into_iter()
        .chain(distribution_link_paths)
    {
        if !emitted_link_paths.insert(path.clone()) {
            continue;
        }
        println!("cargo:rustc-link-search=native={}", path.display());
        if target_os == "macos" {
            println!("cargo:rustc-link-arg=-Wl,-rpath,{}", path.display());
        }
    }
    for path in framework_paths {
        println!("cargo:rustc-link-search=framework={}", path.display());
    }
    for file in link_files {
        println!("cargo:rustc-link-arg={}", file.display());
    }
    for args in ld_args {
        if !args.is_empty() {
            println!("cargo:rustc-link-arg=-Wl,{}", args.join(","));
        }
    }
    for framework in frameworks {
        println!("cargo:rustc-link-lib=framework={framework}");
    }
    let mut emitted_libs: HashSet<String> = HashSet::new();
    for lib in libs {
        if !emitted_libs.insert(lib.clone()) {
            continue;
        }
        println!("cargo:rustc-link-lib={lib}");
    }
}
