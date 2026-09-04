// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{collections::HashSet, fs, path::PathBuf};

use super::source_hygiene::sources_under;

const NATIVE_INSTALLER_HASH: &str = "hashFiles('fluxer_media_proxy/tools/install-native-deps.sh')";

const FFMPEG_LIBRARY_PACKAGES: [(&str, &str, &str, &str); 5] = [
    ("libaom", "aom", "libaom-dev", "libaom3"),
    ("libdav1d", "dav1d", "libdav1d-dev", "libdav1d7"),
    ("libde265", "libde265", "libde265-dev", "libde265-0"),
    ("libvpx", "vpx", "libvpx-dev", "libvpx9"),
    ("libwebp", "libwebp", "libwebp-dev", "libwebp7"),
];

pub(super) fn repository_file(relative: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(relative);
    fs::read_to_string(&path).unwrap_or_else(|_| panic!("{} is readable", path.display()))
}

pub(super) fn ci_workflow() -> Option<String> {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../.github/workflows/tests.yaml");
    fs::read_to_string(&path).ok()
}

pub(super) fn dockerfile_stage(dockerfile: &str, stage: &str) -> String {
    let header = format!("AS {stage}");
    let mut collecting = false;
    let mut collected = String::new();
    for line in dockerfile.lines() {
        if line.starts_with("FROM ") {
            collecting = line.trim_end().ends_with(&header);
            continue;
        }
        if collecting {
            collected.push_str(line);
            collected.push('\n');
        }
    }
    assert!(!collected.is_empty(), "the {stage} stage exists");
    collected
}

fn apt_packages(stage: &str) -> HashSet<String> {
    let mut packages = HashSet::new();
    let mut collecting = false;
    for line in stage.lines() {
        let trimmed = line.trim();
        if trimmed.contains("apt-get install") {
            collecting = true;
            continue;
        }
        if !collecting {
            continue;
        }
        if trimmed.starts_with("&&") {
            collecting = false;
            continue;
        }
        for token in trimmed.split_whitespace() {
            if token != "\\" {
                packages.insert(token.to_owned());
            }
        }
    }
    assert!(!packages.is_empty(), "the stage installs packages");
    packages
}

fn ffmpeg_library_flags(script: &str) -> Vec<String> {
    script
        .split_whitespace()
        .filter_map(|token| token.strip_prefix("--enable-lib"))
        .map(|suffix| format!("lib{suffix}"))
        .collect()
}

fn ffmpeg_build_modules(script: &str) -> Vec<String> {
    script
        .lines()
        .find_map(|line| line.strip_prefix("FFMPEG_BUILD_MODULES="))
        .expect("the installer declares FFMPEG_BUILD_MODULES")
        .trim_matches('"')
        .split_whitespace()
        .map(str::to_owned)
        .collect()
}

fn library_entry(flag: &str) -> (&'static str, &'static str, &'static str, &'static str) {
    *FFMPEG_LIBRARY_PACKAGES
        .iter()
        .find(|entry| entry.0 == flag)
        .unwrap_or_else(|| panic!("--enable-{flag} needs an entry in FFMPEG_LIBRARY_PACKAGES"))
}

#[test]
fn every_enabled_ffmpeg_library_is_packaged_in_all_three_image_stages() {
    let script = repository_file("tools/install-native-deps.sh");
    let dockerfile = repository_file("Dockerfile");
    let native = apt_packages(&dockerfile_stage(&dockerfile, "native"));
    let builder = apt_packages(&dockerfile_stage(&dockerfile, "builder"));
    let runtime = apt_packages(&dockerfile_stage(&dockerfile, "runtime"));
    for flag in ffmpeg_library_flags(&script) {
        let (_, _, development, shared) = library_entry(&flag);
        assert!(
            native.contains(development),
            "the native stage cannot build --enable-{flag} without {development}"
        );
        assert!(
            builder.contains(development),
            "the builder stage cannot link --enable-{flag} without {development}"
        );
        assert!(
            runtime.contains(shared),
            "the runtime stage cannot load --enable-{flag} without {shared}"
        );
    }
}

#[test]
fn the_ffmpeg_build_modules_match_the_enabled_ffmpeg_libraries() {
    let script = repository_file("tools/install-native-deps.sh");
    let flags = ffmpeg_library_flags(&script);
    let modules = ffmpeg_build_modules(&script);
    for flag in &flags {
        let (_, module, ..) = library_entry(flag);
        assert!(
            modules.iter().any(|declared| declared == module),
            "FFMPEG_BUILD_MODULES is missing {module} for --enable-{flag}"
        );
    }
    for module in modules.iter().filter(|module| *module != "zlib") {
        let entry = FFMPEG_LIBRARY_PACKAGES
            .iter()
            .find(|entry| entry.1 == module)
            .unwrap_or_else(|| panic!("{module} needs an entry in FFMPEG_LIBRARY_PACKAGES"));
        assert!(
            flags.iter().any(|flag| flag == entry.0),
            "FFMPEG_BUILD_MODULES probes {module} but FFmpeg is not configured with --enable-{}",
            entry.0
        );
    }
}

pub(super) fn shell_variable(script: &str, name: &str) -> String {
    let prefix = format!("{name}=");
    script
        .lines()
        .find_map(|line| line.strip_prefix(prefix.as_str()))
        .unwrap_or_else(|| panic!("the installer declares {name}"))
        .trim_matches('"')
        .to_owned()
}

fn declared_floors(script: &str) -> Vec<String> {
    script
        .lines()
        .filter_map(|line| line.split('=').next())
        .filter(|name| name.ends_with("_FLOOR"))
        .map(str::to_owned)
        .collect()
}

fn shell_function_names(script: &str) -> Vec<String> {
    script
        .lines()
        .filter_map(|line| line.strip_suffix("() {"))
        .filter(|name| !name.starts_with(' '))
        .map(str::to_owned)
        .collect()
}

pub(super) fn shell_function(script: &str, name: &str) -> String {
    let header = format!("\n{name}() {{");
    let start = script
        .find(&header)
        .unwrap_or_else(|| panic!("the installer declares {name}"));
    let body = &script[start + header.len()..];
    let end = body
        .find("\n}")
        .unwrap_or_else(|| panic!("{name} is a closed shell function"));
    body[..end].to_owned()
}

fn reachable_region(script: &str, seed: &str) -> String {
    let names = shell_function_names(script);
    let mut region = seed.to_owned();
    let mut expanded: HashSet<String> = HashSet::new();
    loop {
        let pending: Vec<String> = names
            .iter()
            .filter(|name| region.contains(name.as_str()) && !expanded.contains(*name))
            .cloned()
            .collect();
        if pending.is_empty() {
            return region;
        }
        for name in pending {
            region.push_str(&shell_function(script, &name));
            expanded.insert(name);
        }
    }
}

pub(super) fn linux_region(script: &str) -> String {
    let branch = script
        .find("if [ \"$(uname -s)\" = \"Darwin\" ]; then")
        .expect("the installer branches on the platform");
    let tail = &script[branch..];
    let closing = tail
        .find("\nfi\n")
        .expect("the platform branch is closed before the Linux path");
    reachable_region(script, &tail[closing..])
}

fn enforces_floor(region: &str, floor: &str) -> bool {
    region.contains(&format!("\"${floor}\"")) || region.contains(&format!("${{{floor}}}"))
}

fn version_parts(version: &str) -> Vec<u64> {
    let mut parts: Vec<u64> = version
        .split('.')
        .map(|part| part.parse().expect("a numeric version component"))
        .collect();
    parts.resize(3, 0);
    parts
}

fn libheif_guard_versions(source: &str) -> Vec<Vec<u64>> {
    source
        .match_indices("LIBHEIF_HAVE_VERSION(")
        .filter_map(|(index, needle)| source[index + needle.len()..].split(')').next())
        .map(|arguments| {
            arguments
                .split(',')
                .map(|part| part.trim().parse().expect("a numeric guard component"))
                .collect()
        })
        .collect()
}

#[test]
fn every_native_floor_is_enforced_on_linux_and_on_macos() {
    let script = repository_file("tools/install-native-deps.sh");
    let floors = declared_floors(&script);
    assert!(
        floors.len() >= 5,
        "the installer declares the native floors"
    );
    let macos = reachable_region(&script, &shell_function(&script, "verify_macos_floors"));
    let linux = linux_region(&script);
    for floor in floors {
        assert!(
            enforces_floor(&macos, &floor),
            "the macOS path stopped enforcing {floor}"
        );
        if floor == "LIBHEIF_FLOOR" {
            continue;
        }
        assert!(
            enforces_floor(&linux, &floor),
            "the Linux path enforces {floor} by nothing"
        );
    }
    assert!(
        linux.contains("pc --exact-version=\"$LIBHEIF_VERSION\" libheif"),
        "the Linux path stopped pinning libheif to LIBHEIF_VERSION"
    );
}

fn required_development_packages(script: &str) -> Vec<String> {
    let mut packages: Vec<String> = linux_region(script)
        .split(|character: char| !character.is_alphanumeric() && character != '-')
        .filter(|token| token.ends_with("-dev"))
        .map(str::to_owned)
        .collect();
    packages.sort();
    packages.dedup();
    packages
}

pub(super) fn installs_package(text: &str, package: &str) -> bool {
    text.split_whitespace().any(|token| token == package)
}

#[test]
fn every_package_the_linux_gate_needs_is_installed_before_the_installer_runs() {
    let script = repository_file("tools/install-native-deps.sh");
    let packages = required_development_packages(&script);
    assert!(
        packages.len() >= 8,
        "the Linux gate names the development packages it needs: {packages:?}"
    );
    let native = dockerfile_stage(&repository_file("Dockerfile"), "native");
    let Some(workflow) = ci_workflow() else {
        eprintln!("skipping: the CI workflow is outside this build context");
        return;
    };
    let step = workflow_step(&workflow, "Install native dependencies");
    let devcontainer = repository_file("../.devcontainer/Dockerfile");
    for package in packages {
        assert!(
            installs_package(&native, &package),
            "the image native stage runs the installer without {package}"
        );
        assert!(
            installs_package(&step, &package),
            "the CI native dependency step runs the installer without {package}"
        );
        assert!(
            installs_package(&devcontainer, &package),
            "the devcontainer runs the installer without {package}"
        );
    }
}

#[test]
fn the_libheif_floor_compiles_the_shim_guards_the_same_way_everywhere() {
    let script = repository_file("tools/install-native-deps.sh");
    let floor = version_parts(&shell_variable(&script, "LIBHEIF_FLOOR"));
    let pinned = version_parts(&shell_variable(&script, "LIBHEIF_VERSION"));
    let guards = libheif_guard_versions(&repository_file("src/native_shim/heif_pixels.c"));
    assert!(
        !guards.is_empty(),
        "the HEIF shim guards on libheif versions"
    );
    for guard in guards {
        assert!(
            floor >= guard,
            "libheif {floor:?} is below the {guard:?} guard in heif_pixels.c, so a developer machine compiles a different HEIF decode path than CI"
        );
    }
    assert!(
        pinned >= floor,
        "the source-built libheif {pinned:?} is below the floor {floor:?} every other environment must clear"
    );
}

fn native_shim_sources() -> Vec<PathBuf> {
    sources_under(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/native_shim"),
        "c",
    )
}

fn shim_string_macro(header: &str, name: &str) -> String {
    let declaration = format!("#define {name}");
    let mut body = String::new();
    let mut found = false;
    for line in header.lines() {
        let trimmed = line.trim();
        if !found {
            if !trimmed.starts_with(&declaration) {
                continue;
            }
            found = true;
        }
        let fragment = trimmed
            .strip_prefix(declaration.as_str())
            .unwrap_or(trimmed);
        let continues = fragment.ends_with('\\');
        body.push_str(fragment.trim_end_matches('\\'));
        if !continues {
            break;
        }
    }
    assert!(found, "the shim defines {name}");
    body.split('"').skip(1).step_by(2).collect()
}

fn forced_input_formats(header: &str) -> Vec<String> {
    let mut formats = Vec::new();
    for path in native_shim_sources() {
        let source = fs::read_to_string(&path).expect("a shim source file is utf8");
        for fragment in source.split("av_find_input_format(").skip(1) {
            let argument = fragment
                .split(')')
                .next()
                .expect("the call is closed")
                .trim();
            let name = match argument.strip_prefix('"') {
                Some(literal) => literal
                    .split('"')
                    .next()
                    .expect("the literal is closed")
                    .to_owned(),
                None => shim_string_macro(header, argument),
            };
            assert!(
                !name.is_empty(),
                "{} forces an empty demuxer name",
                path.display()
            );
            formats.push(name);
        }
    }
    formats.sort();
    formats.dedup();
    formats
}

fn shell_words(script: &str, name: &str) -> Vec<String> {
    shell_variable(script, name)
        .split_whitespace()
        .map(str::to_owned)
        .collect()
}

#[test]
fn the_installer_requires_the_bmp_decoder_libvips_can_no_longer_provide() {
    let vips = repository_file("src/native_shim/vips_image.c");
    assert!(
        vips.contains("vips_block_untrusted_set"),
        "the vips shim stopped blocking untrusted loaders"
    );
    assert!(
        !vips.contains("Magick"),
        "unblocking an ImageMagick loader contradicts the decision that FFmpeg decodes BMP"
    );
    let header = repository_file("src/native_shim/native_shim_internal.h");
    let allowed = shim_string_macro(&header, "FLUXER_ALLOWED_VIDEO_DECODERS");
    assert!(
        allowed.split(',').any(|name| name == "bmp"),
        "the shim stopped whitelisting the bmp decoder"
    );
    let script = repository_file("tools/install-native-deps.sh");
    assert!(
        shell_words(&script, "REQUIRED_DECODERS")
            .iter()
            .any(|name| name == "bmp"),
        "libvips blocks every ImageMagick loader, so a build without FFmpeg's bmp decoder serves no BMP at all"
    );
}

#[test]
fn the_installer_requires_every_demuxer_the_shim_forces_by_name() {
    let script = repository_file("tools/install-native-deps.sh");
    let header = repository_file("src/native_shim/native_shim_internal.h");
    let forced = forced_input_formats(&header);
    assert!(
        forced.iter().any(|name| name == "bmp_pipe"),
        "the shim stopped forcing the bmp_pipe demuxer"
    );
    let required = shell_words(&script, "REQUIRED_DEMUXERS");
    for name in &forced {
        assert!(
            required.contains(name),
            "the shim forces the {name} demuxer by name but REQUIRED_DEMUXERS does not require it"
        );
    }
    let verify = shell_function(&script, "verify_ffmpeg_codecs");
    assert!(
        verify.contains("-demuxers") && verify.contains("$REQUIRED_DEMUXERS"),
        "verify_ffmpeg_codecs declares REQUIRED_DEMUXERS without checking the built ffmpeg for them"
    );
}

pub(super) fn workflow_step(workflow: &str, name: &str) -> String {
    let header = format!("- name: {name}");
    let mut collecting = false;
    let mut collected = String::new();
    for line in workflow.lines() {
        if line.trim_start().starts_with("- name: ") {
            collecting = line.trim_start() == header;
            continue;
        }
        if collecting {
            collected.push_str(line);
            collected.push('\n');
        }
    }
    assert!(!collected.is_empty(), "the {name} step exists");
    collected
}

fn cache_keys(step: &str) -> (String, Vec<String>) {
    let mut key = None;
    let mut restore_keys = Vec::new();
    let mut collecting = false;
    for line in step.lines() {
        let trimmed = line.trim();
        if let Some(value) = trimmed.strip_prefix("key: ") {
            key = Some(value.to_owned());
            collecting = false;
        } else if trimmed == "restore-keys: |" {
            collecting = true;
        } else if collecting {
            if trimmed.is_empty() {
                collecting = false;
            } else {
                restore_keys.push(trimmed.to_owned());
            }
        }
    }
    (key.expect("the cache step declares a key"), restore_keys)
}

#[test]
fn the_cargo_cache_is_keyed_on_the_native_dependency_installer() {
    let Some(workflow) = ci_workflow() else {
        eprintln!("skipping: the CI workflow is outside this build context");
        return;
    };
    let (native_key, _) = cache_keys(&workflow_step(&workflow, "Cache native media dependencies"));
    assert!(
        native_key.contains(NATIVE_INSTALLER_HASH),
        "the native dependency cache stopped keying on the installer: {native_key}"
    );
    let (cargo_key, restore_keys) = cache_keys(&workflow_step(&workflow, "Cache cargo"));
    assert!(
        cargo_key.contains(NATIVE_INSTALLER_HASH),
        "target/ carries the native shim archive built against the installed headers, so the cargo cache key must move with {NATIVE_INSTALLER_HASH}: {cargo_key}"
    );
    assert!(
        !restore_keys.is_empty(),
        "the cargo cache declares fallbacks"
    );
    for restore_key in restore_keys {
        assert!(
            restore_key.contains(NATIVE_INSTALLER_HASH),
            "restore-key {restore_key} resurrects a target/ built against different native headers"
        );
    }
}
