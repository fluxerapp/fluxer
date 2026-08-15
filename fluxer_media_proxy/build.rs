// SPDX-License-Identifier: AGPL-3.0-or-later

use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=src/vips_shim.c");
    println!("cargo:rerun-if-changed=src/vips_shim.h");
    println!("cargo:rerun-if-changed=src/srgb_profile.h");

    let mut build = cc::Build::new();
    build
        .file("src/vips_shim.c")
        .include("src")
        .flag_if_supported("-std=gnu11");

    let mut link_paths: Vec<PathBuf> = Vec::new();
    let mut link_files: Vec<PathBuf> = Vec::new();
    let mut framework_paths: Vec<PathBuf> = Vec::new();
    let mut frameworks: Vec<String> = Vec::new();
    let mut libs: Vec<String> = Vec::new();
    let mut ld_args: Vec<Vec<String>> = Vec::new();

    for lib in [
        "libcurl",
        "vips",
        "libheif",
        "libavformat",
        "libavcodec",
        "libavfilter",
        "libavutil",
        "libswscale",
        "libswresample",
        "libwebpmux",
        "libwebp",
    ] {
        let probed = pkg_config::Config::new()
            .cargo_metadata(false)
            .probe(lib)
            .unwrap_or_else(|err| panic!("pkg-config could not find {lib}: {err}"));
        for include in probed.include_paths {
            build.include(include);
        }
        link_paths.extend(probed.link_paths);
        link_files.extend(probed.link_files);
        framework_paths.extend(probed.framework_paths);
        frameworks.extend(probed.frameworks);
        libs.extend(probed.libs);
        ld_args.extend(probed.ld_args);
    }

    build.compile("fluxer_vips_shim");

    for path in link_paths {
        println!("cargo:rustc-link-search=native={}", path.display());
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
    for lib in libs {
        println!("cargo:rustc-link-lib={lib}");
    }
}
