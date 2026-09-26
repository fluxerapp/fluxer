// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

use super::provisioning::repository_file;

const REQUIRED_PKG_CONFIG_ENVIRONMENT: [&str; 4] = [
    "PKG_CONFIG",
    "PKG_CONFIG_LIBDIR",
    "PKG_CONFIG_PATH",
    "PKG_CONFIG_SYSROOT_DIR",
];

pub(super) fn sources_under(root: PathBuf, extension: &str) -> Vec<PathBuf> {
    let mut pending = vec![root];
    let mut sources = Vec::new();
    while let Some(dir) = pending.pop() {
        for entry in fs::read_dir(&dir).expect("the source tree is readable") {
            let path = entry.expect("a source entry is readable").path();
            if path.is_dir() {
                pending.push(path);
            } else if path.extension().is_some_and(|ext| ext == extension) {
                sources.push(path);
            }
        }
    }
    sources
}

fn server_sources() -> Vec<PathBuf> {
    sources_under(
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/server"),
        "rs",
    )
}

fn crate_sources() -> Vec<PathBuf> {
    sources_under(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src"), "rs")
}

#[test]
fn the_server_module_tree_suppresses_no_dead_code() {
    let sources = server_sources();
    assert!(!sources.is_empty());
    for path in sources {
        let source = fs::read_to_string(&path).expect("a server source file is utf8");
        assert!(
            !source.contains("dead_code"),
            "{} suppresses dead code instead of deleting it",
            path.display()
        );
    }
}

#[test]
fn no_crate_source_file_suppresses_dead_code() {
    let sources = crate_sources();
    assert!(!sources.is_empty());
    for path in sources {
        let source = fs::read_to_string(&path).expect("a crate source file is utf8");
        for line in source.lines() {
            let attribute = line.trim_start();
            let is_attribute = attribute.starts_with("#[") || attribute.starts_with("#![");
            assert!(
                !(is_attribute && attribute.contains("dead_code")),
                "{} suppresses dead code instead of deleting it",
                path.display()
            );
        }
    }
}

fn string_array(build_script: &str, name: &str) -> HashSet<String> {
    let declaration = format!("const {name}");
    let start = build_script
        .find(&declaration)
        .unwrap_or_else(|| panic!("build.rs declares {name}"));
    let body = &build_script[start..];
    let end = body
        .find("];")
        .unwrap_or_else(|| panic!("{name} is a closed array"));
    body[..end]
        .lines()
        .filter_map(|line| line.trim().strip_prefix('"'))
        .filter_map(|entry| entry.split('"').next())
        .map(str::to_owned)
        .collect()
}

fn crate_relative(manifest: &Path, path: &Path) -> String {
    path.strip_prefix(manifest)
        .expect("the file lives inside the crate")
        .to_str()
        .expect("the path is utf8")
        .to_owned()
}

fn local_includes(path: &Path) -> Vec<String> {
    fs::read_to_string(path)
        .unwrap_or_else(|_| panic!("{} is readable", path.display()))
        .lines()
        .filter_map(|line| line.trim().strip_prefix("#include \""))
        .filter_map(|rest| rest.split('"').next())
        .map(str::to_owned)
        .collect()
}

fn resolve_include(manifest: &Path, includer: &Path, include: &str) -> PathBuf {
    let sibling = includer
        .parent()
        .expect("a compiled source has a parent directory")
        .join(include);
    if sibling.is_file() {
        return sibling;
    }
    manifest.join("src").join(include)
}

fn direct_environment_reads(build_script: &str) -> Vec<String> {
    build_script
        .match_indices("env::var(\"")
        .filter_map(|(index, needle)| build_script[index + needle.len()..].split('"').next())
        .map(str::to_owned)
        .collect()
}

#[test]
fn build_rs_reruns_for_every_compiled_source_and_header() {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let build_script = repository_file("build.rs");
    assert!(
        build_script.contains("println!(\"cargo:rerun-if-changed={source}\");"),
        "build.rs stopped emitting a rerun line for every compiled source"
    );
    assert!(
        build_script.contains("println!(\"cargo:rerun-if-changed={header}\");"),
        "build.rs stopped emitting a rerun line for every tracked header"
    );
    let compiled: HashSet<String> = sources_under(manifest.join("src"), "c")
        .iter()
        .map(|path| crate_relative(&manifest, path))
        .collect();
    assert_eq!(
        string_array(&build_script, "NATIVE_SHIM_SOURCES"),
        compiled,
        "NATIVE_SHIM_SOURCES no longer matches the C sources cc compiles"
    );
    let mut headers: HashSet<String> = HashSet::new();
    let mut pending: Vec<PathBuf> = compiled.iter().map(|path| manifest.join(path)).collect();
    while let Some(path) = pending.pop() {
        for include in local_includes(&path) {
            let resolved = resolve_include(&manifest, &path, &include);
            assert!(
                resolved.is_file(),
                "{} includes {include}, which is not in the tree",
                path.display()
            );
            if headers.insert(crate_relative(&manifest, &resolved)) {
                pending.push(resolved);
            }
        }
    }
    assert_eq!(
        string_array(&build_script, "NATIVE_SHIM_HEADERS"),
        headers,
        "NATIVE_SHIM_HEADERS no longer matches the headers the compiled sources include"
    );
}

#[test]
fn build_rs_reruns_when_the_native_toolchain_environment_changes() {
    let build_script = repository_file("build.rs");
    assert!(
        build_script.contains("println!(\"cargo:rerun-if-env-changed={variable}\");"),
        "build.rs stopped emitting a rerun line for every tracked variable"
    );
    let declared = string_array(&build_script, "NATIVE_BUILD_ENVIRONMENT");
    for variable in REQUIRED_PKG_CONFIG_ENVIRONMENT {
        assert!(
            declared.contains(variable),
            "build.rs resolves every native library through {variable} without tracking it"
        );
    }
    for variable in direct_environment_reads(&build_script) {
        assert!(
            declared.contains(&variable),
            "build.rs reads {variable} without tracking it"
        );
    }
}
