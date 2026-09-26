// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::HashSet;

use super::provisioning::{
    ci_workflow, dockerfile_stage, installs_package, linux_region, repository_file, shell_function,
    shell_variable, workflow_step,
};

const SHIPPED_MALLOC_CONF: &str = "ENV MALLOC_CONF=\"background_thread:true,metadata_thp:auto,dirty_decay_ms:5000,muzzy_decay_ms:5000\"";

fn copy_sources(stage: &str, from: &str) -> Vec<String> {
    let prefix = format!("COPY --from={from} ");
    stage
        .lines()
        .filter_map(|line| line.strip_prefix(prefix.as_str()))
        .filter_map(|rest| rest.split_whitespace().next())
        .map(str::to_owned)
        .collect()
}

#[test]
fn the_runtime_image_takes_only_shared_objects_from_the_native_prefix() {
    let dockerfile = repository_file("Dockerfile");
    let native = dockerfile_stage(&dockerfile, "native");
    let runtime = dockerfile_stage(&dockerfile, "runtime");
    let sources = copy_sources(&runtime, "native");
    assert!(
        !sources.is_empty(),
        "the runtime image never receives the source-built libraries"
    );
    for source in &sources {
        assert!(
            source.as_str() != "/usr/local",
            "the runtime image ships the whole native prefix, so the ffmpeg CLI, the headers and the pkgconfig files land in production"
        );
        assert!(
            native.contains(source.as_str()),
            "the native stage never assembles {source}"
        );
    }
    assert!(
        native.contains("/usr/local/lib/*.so.*"),
        "the native stage stopped selecting the shared objects on their own"
    );
}

#[test]
fn the_runtime_image_resolves_exactly_one_libheif_and_asserts_which_one() {
    let runtime = dockerfile_stage(&repository_file("Dockerfile"), "runtime");
    assert!(
        runtime.contains("rm -f /usr/lib/*/libheif.so.1*"),
        "trixie's libheif still sits beside the source-built one, so which copy libvips binds to is left to ld.so cache order"
    );
    assert!(
        runtime.contains("ldconfig -p | grep -c 'libheif\\.so\\.1 '"),
        "nothing counts the resolvable libheif copies at build time"
    );
    assert!(
        runtime.contains("/usr/local/lib/libheif\\.so\\.1"),
        "nothing pins the surviving libheif to the source-built one"
    );
}

#[test]
fn the_devcontainer_keeps_one_libheif_header_set_and_proves_the_compiler_picks_it() {
    let devcontainer = repository_file("../.devcontainer/Dockerfile");
    assert!(
        !installs_package(&devcontainer, "libheif-dev"),
        "the devcontainer apt-installs libheif headers beside the source-built ones"
    );
    assert!(
        devcontainer.contains("LIBHEIF_HAVE_VERSION"),
        "nothing proves the source-built libheif headers win the include search"
    );
    assert!(
        devcontainer.contains("heif_get_version()"),
        "nothing proves the libheif the loader picks matches the headers it was compiled against"
    );
}

#[test]
fn the_runtime_image_ships_the_allocator_tuning_the_service_was_deployed_with() {
    let runtime = dockerfile_stage(&repository_file("Dockerfile"), "runtime");
    assert!(
        runtime.contains(SHIPPED_MALLOC_CONF),
        "the jemalloc tuning drifted from the one production runs"
    );
}

const FFMPEG_GPL_LIBRARIES: [&str; 13] = [
    "avisynth",
    "frei0r",
    "libcdio",
    "libdavs2",
    "libdvdnav",
    "libdvdread",
    "librubberband",
    "libvidstab",
    "libx264",
    "libx265",
    "libxavs",
    "libxavs2",
    "libxvid",
];

const FFMPEG_RELICENSING_FLAGS: [&str; 3] = ["gpl", "nonfree", "version3"];

const DEBIAN_ESSENTIAL: &str = "";

const LINUX_INSTALLER_COMMAND_PACKAGES: [(&str, &str); 6] = [
    ("cc", "build-essential"),
    ("cmake", "cmake"),
    ("curl", "curl"),
    ("make", "build-essential"),
    ("pkg-config", "pkg-config"),
    ("tar", DEBIAN_ESSENTIAL),
];

const LIBHEIF_CMAKE_OPTIONS: [&str; 38] = [
    "BUILD_DEVELOPMENT_TOOLS",
    "BUILD_DOCUMENTATION",
    "BUILD_SHARED_LIBS",
    "BUILD_TESTING",
    "ENABLE_COVERAGE",
    "ENABLE_MULTITHREADING_SUPPORT",
    "ENABLE_PARALLEL_TILE_DECODING",
    "ENABLE_PLUGIN_LOADING",
    "WITH_AOM_DECODER",
    "WITH_AOM_ENCODER",
    "WITH_DAV1D",
    "WITH_EXAMPLES",
    "WITH_EXAMPLE_HEIF_THUMB",
    "WITH_EXAMPLE_HEIF_VIEW",
    "WITH_FFMPEG_DECODER",
    "WITH_FUZZERS",
    "WITH_GDK_PIXBUF",
    "WITH_HEADER_COMPRESSION",
    "WITH_JPEG_DECODER",
    "WITH_JPEG_ENCODER",
    "WITH_KVAZAAR",
    "WITH_LIBDE265",
    "WITH_LIBSHARPYUV",
    "WITH_LIBSHARPYUV_INTERNAL",
    "WITH_OPENJPH_ENCODER",
    "WITH_OpenH264_DECODER",
    "WITH_OpenJPEG_DECODER",
    "WITH_OpenJPEG_ENCODER",
    "WITH_RAV1E",
    "WITH_REDUCED_VISIBILITY",
    "WITH_SvtEnc",
    "WITH_UNCOMPRESSED_CODEC",
    "WITH_UVG266",
    "WITH_VVDEC",
    "WITH_VVENC",
    "WITH_WEBCODECS",
    "WITH_X264",
    "WITH_X265",
];

fn ffmpeg_configure_enables(script: &str) -> Vec<String> {
    shell_function(script, "build_ffmpeg")
        .split_whitespace()
        .filter_map(|token| token.strip_prefix("--enable-"))
        .map(str::to_owned)
        .collect()
}

fn need_commands(script: &str) -> Vec<String> {
    let mut commands: Vec<String> = linux_region(script)
        .lines()
        .filter_map(|line| line.trim().strip_prefix("need_command "))
        .map(|name| name.trim().to_owned())
        .collect();
    commands.sort();
    commands.dedup();
    commands
}

fn cmake_feature_definitions(invocation: &str) -> HashSet<String> {
    invocation
        .split_whitespace()
        .filter_map(|token| token.strip_prefix("-D"))
        .filter_map(|definition| definition.split('=').next())
        .filter(|name| {
            name.starts_with("WITH_") || name.starts_with("ENABLE_") || name.starts_with("BUILD_")
        })
        .map(str::to_owned)
        .collect()
}

#[test]
fn the_source_built_ffmpeg_keeps_the_lgpl_licence_it_is_shipped_under() {
    let script = repository_file("tools/install-native-deps.sh");
    let enabled = ffmpeg_configure_enables(&script);
    assert!(!enabled.is_empty(), "the installer configures FFmpeg");
    for flag in FFMPEG_RELICENSING_FLAGS {
        assert!(
            !enabled.iter().any(|enable| enable == flag),
            "--enable-{flag} relicenses every shipped FFmpeg library away from LGPL without enabling anything the media proxy asks for"
        );
    }
    for enable in &enabled {
        assert!(
            !FFMPEG_GPL_LIBRARIES.contains(&enable.as_str()),
            "--enable-{enable} is on FFmpeg's EXTERNAL_LIBRARY_GPL_LIST, so the build can no longer stay LGPL"
        );
    }
}

#[test]
fn every_command_the_linux_installer_runs_is_installed_before_it_runs() {
    let script = repository_file("tools/install-native-deps.sh");
    let commands = need_commands(&script);
    assert!(
        commands.iter().any(|command| command == "curl"),
        "fetch_source downloads every pinned tarball with curl"
    );
    let native = dockerfile_stage(&repository_file("Dockerfile"), "native");
    let Some(workflow) = ci_workflow() else {
        eprintln!("skipping: the CI workflow is outside this build context");
        return;
    };
    let step = workflow_step(&workflow, "Install native dependencies");
    let devcontainer = repository_file("../.devcontainer/Dockerfile");
    for command in commands {
        let package = LINUX_INSTALLER_COMMAND_PACKAGES
            .iter()
            .find(|entry| entry.0 == command)
            .unwrap_or_else(|| {
                panic!("{command} needs an entry in LINUX_INSTALLER_COMMAND_PACKAGES")
            })
            .1;
        if package == DEBIAN_ESSENTIAL {
            continue;
        }
        assert!(
            installs_package(&native, package),
            "the image native stage runs the installer without {package}, which provides {command}"
        );
        assert!(
            installs_package(&step, package),
            "the CI native dependency step runs the installer without {package}, which provides {command}"
        );
        assert!(
            installs_package(&devcontainer, package),
            "the devcontainer runs the installer without {package}, which provides {command}"
        );
    }
}

#[test]
fn the_libheif_build_pins_every_feature_flag_instead_of_autodetecting_it() {
    let script = repository_file("tools/install-native-deps.sh");
    let version = shell_variable(&script, "LIBHEIF_VERSION");
    let build = shell_function(&script, "build_libheif");
    let pinned = cmake_feature_definitions(&build);
    for option in LIBHEIF_CMAKE_OPTIONS {
        assert!(
            pinned.contains(option),
            "libheif {option} is left to autodetection, so CI, the devcontainer and production build differently featured libraries from the same source"
        );
    }
    for option in &pinned {
        assert!(
            LIBHEIF_CMAKE_OPTIONS.contains(&option.as_str()),
            "-D{option} is not an option libheif {version} declares"
        );
    }
    assert!(
        build.contains("-DENABLE_PLUGIN_LOADING=OFF"),
        "every WITH_*_PLUGIN variant only changes the build while plugin loading is on"
    );
}
