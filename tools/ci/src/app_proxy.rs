// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{
    CALVER_SCHEME, CalverEnv, CommandSpec, append_github_env, append_github_output, collect_files,
    env_string, output_text, path_to_s3_key, remove_dir_if_exists, require_env, resolve_calver,
    run_command, runner_temp, trim_option,
};
use crate::functions::sha256_file;
#[cfg(feature = "s3")]
use crate::s3::{S3UploadPlanItem, s3_client, upload_s3_plan_append_only};
use anyhow::{Context, Result, anyhow, ensure};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use chrono::Utc;
use clap::{Args, ValueEnum};
use serde_json::{Map, Value, json};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

const DEFAULT_PUBLIC_ASSET_BASE_URL: &str = "https://fluxerstatic.com";
const DEFAULT_APP_PROXY_BUNDLE_LOCAL_ASSETS: &str = "false";
#[cfg(feature = "s3")]
const DEFAULT_STATIC_BUCKET: &str = "fluxer-static";
#[cfg(feature = "s3")]
const DEFAULT_S3_ENDPOINT: &str = "https://ewr1.vultrobjects.com";
#[cfg(feature = "s3")]
const IMMUTABLE_ASSET_CACHE_CONTROL: &str = "public, max-age=31536000, immutable";
const RUNTIME_STATIC_DIR: &str = "/srv/app/static";
const CANONICAL_ASSETS_DIR: &str = "/assets";
const AMD64_PLATFORM: &str = "linux/amd64";
const ARM64_PLATFORM: &str = "linux/arm64";
const PARITY_DIFF_LIMIT: usize = 20;

#[derive(Debug, Args, Clone)]
pub struct BuildAppProxyArgs {
    #[arg(long, value_enum)]
    step: AppProxyStep,
    #[arg(long)]
    build_version: Option<String>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
#[clap(rename_all = "snake_case")]
enum AppProxyStep {
    SetMetadata,
    PrepareDockerConfig,
    ConfigureGhcrAuth,
    BuildDist,
    BuildImage,
    GenerateAssetManifest,
    UploadAssets,
    VerifyPublishedAssets,
    VerifyAssetParity,
}

pub async fn run(args: BuildAppProxyArgs) -> Result<()> {
    match args.step {
        AppProxyStep::SetMetadata => set_metadata_step(args.build_version.as_deref()),
        AppProxyStep::PrepareDockerConfig => prepare_docker_config_step(),
        AppProxyStep::ConfigureGhcrAuth => configure_ghcr_auth_step(),
        AppProxyStep::BuildDist => build_dist_step(),
        AppProxyStep::BuildImage => build_image_step(),
        AppProxyStep::GenerateAssetManifest => generate_asset_manifest_step(),
        AppProxyStep::UploadAssets => upload_assets_step().await,
        AppProxyStep::VerifyPublishedAssets => verify_published_assets_step().await,
        AppProxyStep::VerifyAssetParity => verify_asset_parity_step(),
    }
}

fn set_metadata_step(build_version_arg: Option<&str>) -> Result<()> {
    let mut calver_env = CalverEnv::from_process();
    calver_env.build_version =
        trim_option(build_version_arg.map(ToOwned::to_owned)).or(calver_env.build_version);
    let version = resolve_calver(&calver_env, Utc::now())?;
    append_github_output(&[
        ("build_version", version.as_str()),
        ("version", version.as_str()),
        ("calver_scheme", CALVER_SCHEME),
    ])
}

fn prepare_docker_config_step() -> Result<()> {
    let docker_config = runner_temp().join("docker-config");
    fs::create_dir_all(&docker_config)
        .with_context(|| format!("Failed to create {}", docker_config.display()))?;
    append_github_env(&[("DOCKER_CONFIG", docker_config.to_string_lossy().as_ref())])
}

fn configure_ghcr_auth_step() -> Result<()> {
    let docker_config = require_env("DOCKER_CONFIG")?;
    let username = require_env("GHCR_USERNAME")?;
    let token = require_env("GHCR_TOKEN")?;
    let path = PathBuf::from(docker_config).join("config.json");
    write_ghcr_auth_config(&path, &username, &token)
}

fn write_ghcr_auth_config(path: &Path, username: &str, token: &str) -> Result<()> {
    let mut config = if path.exists() {
        serde_json::from_str::<Value>(
            &fs::read_to_string(path)
                .with_context(|| format!("Failed to read {}", path.display()))?,
        )
        .with_context(|| format!("Failed to parse {}", path.display()))?
    } else {
        Value::Object(Map::new())
    };

    let root = config
        .as_object_mut()
        .ok_or_else(|| anyhow!("Docker config root must be a JSON object"))?;
    let auths = root
        .entry("auths")
        .or_insert_with(|| Value::Object(Map::new()))
        .as_object_mut()
        .ok_or_else(|| anyhow!("Docker config auths must be a JSON object"))?;
    auths.insert(
        "ghcr.io".to_string(),
        json!({ "auth": ghcr_auth_value(username, token) }),
    );

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    fs::write(path, format!("{}\n", serde_json::to_string(&config)?))
        .with_context(|| format!("Failed to write {}", path.display()))
}

fn ghcr_auth_value(username: &str, token: &str) -> String {
    BASE64.encode(format!("{username}:{token}"))
}

fn build_dist_step() -> Result<()> {
    run_command(bake_command(&["app-dist", "app-assets-image"])?)
}

fn build_image_step() -> Result<()> {
    let command = bake_command(&["app-proxy"])?
        .env("APP_ASSETS_REF", assets_ref()?)
        .env("APP_ASSETS_PLATFORM", canonical_assets_platform());
    run_command(command)
}

fn canonical_assets_platform() -> String {
    env_string("APP_ASSETS_PLATFORM").unwrap_or_else(|| AMD64_PLATFORM.to_string())
}

fn assets_ref() -> Result<String> {
    match env_string("APP_ASSETS_REF") {
        Some(reference) => Ok(reference),
        None => Ok(assets_image_ref(
            &image_repo()?,
            &require_env("BUILD_VERSION")?,
        )),
    }
}

fn assets_image_ref(image_repo: &str, build_version: &str) -> String {
    format!("{image_repo}:{build_version}-assets")
}

fn image_repo() -> Result<String> {
    match env::var("IMAGE_REPO") {
        Ok(value) => Ok(value),
        Err(_) => Ok(format!("ghcr.io/{}/fluxer-app-proxy", ghcr_owner()?)),
    }
}

fn bake_command(targets: &[&str]) -> Result<CommandSpec> {
    let build_version = require_env("BUILD_VERSION")?;
    let public_asset_base_url = env::var("PUBLIC_ASSET_BASE_URL")
        .unwrap_or_else(|_| DEFAULT_PUBLIC_ASSET_BASE_URL.to_string());
    Ok(CommandSpec::new("docker")
        .args(["buildx", "bake", "-f", "fluxer_app_proxy/docker-bake.hcl"])
        .args(targets.iter().copied())
        .env("IMAGE_REPO", image_repo()?)
        .env("BUILD_VERSION", build_version)
        .env("PUBLIC_ASSET_BASE_URL", public_asset_base_url)
        .env(
            "BUNDLE_LOCAL_ASSETS",
            env::var("BUNDLE_LOCAL_ASSETS")
                .unwrap_or_else(|_| DEFAULT_APP_PROXY_BUNDLE_LOCAL_ASSETS.to_string()),
        )
        .env(
            "CACHE_FROM",
            env::var("CACHE_FROM").unwrap_or_else(|_| default_app_proxy_cache_ref()),
        )
        .env(
            "CACHE_TO",
            env::var("CACHE_TO").unwrap_or_else(|_| {
                format!(
                    "{},mode=max,image-manifest=true,oci-mediatypes=true,ignore-error=true",
                    default_app_proxy_cache_ref()
                )
            }),
        )
        .env(
            "DOCKER_BUILD_SUMMARY",
            env::var("DOCKER_BUILD_SUMMARY").unwrap_or_else(|_| "false".to_string()),
        )
        .env(
            "DOCKER_BUILD_RECORD_UPLOAD",
            env::var("DOCKER_BUILD_RECORD_UPLOAD").unwrap_or_else(|_| "false".to_string()),
        ))
}

fn default_app_proxy_cache_ref() -> String {
    let owner = ghcr_owner().unwrap_or_else(|_| "fluxerapp".to_string());
    format!("type=registry,ref=ghcr.io/{owner}/fluxer-app-proxy:buildcache-amd64")
}

fn ghcr_owner() -> Result<String> {
    for key in ["GHCR_OWNER", "GITHUB_REPOSITORY_OWNER", "OWNER"] {
        if let Ok(value) = env::var(key) {
            let value = value.trim();
            if !value.is_empty() {
                return Ok(value.to_string());
            }
        }
    }

    if let Ok(repository) = env::var("GITHUB_REPOSITORY")
        && let Some((owner, _)) = repository.split_once('/')
    {
        let owner = owner.trim();
        if !owner.is_empty() {
            return Ok(owner.to_string());
        }
    }

    Err(anyhow!(
        "GHCR owner must be set with GHCR_OWNER, GITHUB_REPOSITORY_OWNER, OWNER, or GITHUB_REPOSITORY"
    ))
}

fn generate_asset_manifest_step() -> Result<()> {
    let dist = app_dist_dir();
    let manifest_path = dist.join("assets-manifest.txt");
    let assets = asset_manifest_entries(&dist)?;
    fs::write(&manifest_path, format!("{}\n", assets.join("\n")))
        .with_context(|| format!("Failed to write {}", manifest_path.display()))?;
    println!("=== asset manifest ===");
    for asset in &assets {
        println!("{asset}");
    }
    println!("total assets: {}", assets.len());
    Ok(())
}

fn app_dist_dir() -> PathBuf {
    env::var("APP_DIST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("app-dist-output/dist"))
}

fn asset_manifest_entries(dist: &Path) -> Result<Vec<String>> {
    let assets_dir = dist.join("assets");
    ensure!(
        assets_dir.exists(),
        "App proxy assets directory is missing: {}",
        assets_dir.display()
    );
    let mut entries = collect_files(&assets_dir)?
        .into_iter()
        .map(|path| {
            path.strip_prefix(dist)
                .with_context(|| format!("Failed to relativize {}", path.display()))
                .map(path_to_s3_key)
        })
        .collect::<Result<Vec<_>>>()?;
    entries.sort();
    Ok(entries)
}

#[cfg(not(feature = "s3"))]
async fn upload_assets_step() -> Result<()> {
    anyhow::bail!("Uploading assets needs the s3 feature")
}

#[cfg(feature = "s3")]
async fn upload_assets_step() -> Result<()> {
    let client = s3_client(Some(DEFAULT_S3_ENDPOINT)).await?;
    let bucket = env::var("STATIC_BUCKET").unwrap_or_else(|_| DEFAULT_STATIC_BUCKET.to_string());
    let dist = app_dist_dir();
    let manifest_path = dist.join("assets-manifest.txt");
    let assets = read_asset_manifest(&manifest_path)?;
    ensure!(!assets.is_empty(), "{} is empty", manifest_path.display());

    let plan = asset_upload_plan(&dist, &assets)?;
    let stats = upload_s3_plan_append_only(&client, &bucket, plan).await?;

    println!("upload complete - {} assets", assets.len());
    println!(
        "append-only result - uploaded {}, skipped existing {}, repaired metadata {}",
        stats.uploaded, stats.skipped_existing, stats.metadata_repaired
    );
    Ok(())
}

#[cfg(feature = "s3")]
fn asset_upload_plan(dist: &Path, assets: &[String]) -> Result<Vec<S3UploadPlanItem>> {
    assets
        .iter()
        .map(|asset| {
            let path = dist.join(asset);
            ensure!(
                path.is_file(),
                "Manifest asset is missing: {}",
                path.display()
            );
            Ok(S3UploadPlanItem::new(path, asset.clone())
                .with_detected_content_type()
                .with_cache_control(IMMUTABLE_ASSET_CACHE_CONTROL)
                .repair_existing_metadata())
        })
        .collect()
}

fn read_asset_manifest(path: &Path) -> Result<Vec<String>> {
    let manifest =
        fs::read_to_string(path).with_context(|| format!("Failed to read {}", path.display()))?;
    manifest
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(validate_manifest_asset)
        .collect()
}

fn validate_manifest_asset(asset: &str) -> Result<String> {
    ensure!(
        asset.starts_with("assets/"),
        "Asset manifest entry must be under assets/: {asset}"
    );
    ensure!(
        !asset.contains("..") && !asset.starts_with('/') && !asset.contains('\\'),
        "Invalid asset manifest path: {asset}"
    );
    Ok(asset.to_string())
}

async fn verify_published_assets_step() -> Result<()> {
    let dist = app_dist_dir();
    let manifest_path = dist.join("assets-manifest.txt");
    let assets = read_asset_manifest(&manifest_path)?;
    ensure!(!assets.is_empty(), "{} is empty", manifest_path.display());

    let index_path = dist.join("index.html");
    let index = fs::read_to_string(&index_path)
        .with_context(|| format!("Failed to read {}", index_path.display()))?;
    let unproduced = unproduced_references(&referenced_assets(&index), &assets);
    ensure!(
        unproduced.is_empty(),
        "index.html references assets this build did not produce: {}",
        unproduced.join(", ")
    );

    verify_local_assets(&dist, &assets)
}

fn verify_local_assets(dist: &Path, assets: &[String]) -> Result<()> {
    let missing = assets
        .iter()
        .filter(|asset| !dist.join(asset).is_file())
        .cloned()
        .collect::<Vec<_>>();
    ensure!(
        missing.is_empty(),
        "Manifest assets are missing from {}: {}",
        dist.display(),
        missing.join(", ")
    );
    println!(
        "published asset verification passed - {} assets present in {}",
        assets.len(),
        dist.display()
    );
    Ok(())
}

fn referenced_assets(source: &str) -> Vec<String> {
    const PREFIX: &str = "assets/";
    let bytes = source.as_bytes();
    let mut names = BTreeSet::new();
    let mut cursor = 0;
    while let Some(offset) = source[cursor..].find(PREFIX) {
        let start = cursor + offset;
        cursor = start + PREFIX.len();
        if start > 0
            && !matches!(
                bytes[start - 1],
                b'/' | b'"' | b'\'' | b'=' | b'(' | b' ' | b'>'
            )
        {
            continue;
        }
        let name = source[cursor..]
            .chars()
            .take_while(|value| {
                value.is_ascii_alphanumeric() || matches!(value, '.' | '_' | '-' | '/')
            })
            .collect::<String>();
        if !name.is_empty() && !name.ends_with('/') {
            names.insert(format!("{PREFIX}{name}"));
        }
    }
    names.into_iter().collect()
}

fn unproduced_references(referenced: &[String], produced: &[String]) -> Vec<String> {
    let produced = produced.iter().map(String::as_str).collect::<BTreeSet<_>>();
    referenced
        .iter()
        .filter(|asset| !produced.contains(asset.as_str()))
        .cloned()
        .collect()
}

fn verify_asset_parity_step() -> Result<()> {
    let root = runner_temp().join("app-proxy-asset-parity");
    remove_dir_if_exists(&root)?;

    let canonical_dir = extract_image_dir(
        &require_env("APP_PROXY_ASSETS_REF")?,
        &canonical_assets_platform(),
        CANONICAL_ASSETS_DIR,
        &root.join("canonical"),
    )?;
    let canonical = asset_tree_digests(&canonical_dir)?;
    ensure!(
        !canonical.is_empty(),
        "Canonical asset tree is empty: {}",
        canonical_dir.display()
    );

    for (label, reference_env, platform) in [
        ("amd64", "APP_PROXY_AMD64_REF", AMD64_PLATFORM),
        ("arm64", "APP_PROXY_ARM64_REF", ARM64_PLATFORM),
    ] {
        let runtime_dir = extract_image_dir(
            &require_env(reference_env)?,
            platform,
            RUNTIME_STATIC_DIR,
            &root.join(label),
        )?;
        let differences = tree_differences(&canonical, &asset_tree_digests(&runtime_dir)?);
        ensure!(
            differences.is_empty(),
            "{label} app-proxy asset tree does not match the canonical dist:\n{}",
            differences.join("\n")
        );
    }

    println!(
        "asset parity verified - {} files in every architecture",
        canonical.len()
    );
    Ok(())
}

fn extract_image_dir(reference: &str, platform: &str, path: &str, dest: &Path) -> Result<PathBuf> {
    run_command(pull_command(reference, platform))?;
    let container = output_text(create_command(reference, platform))?;
    ensure!(
        !container.is_empty(),
        "docker create returned no container id for {reference}"
    );
    if let Some(parent) = dest.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    let copied = run_command(copy_command(&container, path, dest));
    let removed = run_command(CommandSpec::new("docker").args(["rm", "-f", container.as_str()]));
    copied?;
    removed?;
    Ok(dest.to_path_buf())
}

fn pull_command(reference: &str, platform: &str) -> CommandSpec {
    CommandSpec::new("docker").args(["pull", "--platform", platform, reference])
}

fn create_command(reference: &str, platform: &str) -> CommandSpec {
    CommandSpec::new("docker").args(["create", "--platform", platform, reference])
}

fn copy_command(container: &str, path: &str, dest: &Path) -> CommandSpec {
    CommandSpec::new("docker")
        .arg("cp")
        .arg(format!("{container}:{path}"))
        .arg(dest.as_os_str())
}

fn asset_tree_digests(root: &Path) -> Result<BTreeMap<String, String>> {
    collect_files(root)?
        .into_iter()
        .map(|path| {
            let relative = path
                .strip_prefix(root)
                .with_context(|| format!("Failed to relativize {}", path.display()))?;
            Ok((path_to_s3_key(relative), sha256_file(&path)?))
        })
        .collect()
}

fn tree_differences(
    canonical: &BTreeMap<String, String>,
    other: &BTreeMap<String, String>,
) -> Vec<String> {
    let mut differences = Vec::new();
    for (path, digest) in canonical {
        match other.get(path) {
            None => differences.push(format!("missing: {path}")),
            Some(actual) if actual != digest => {
                differences.push(format!("content differs: {path}"))
            }
            Some(_) => {}
        }
    }
    for path in other.keys() {
        if !canonical.contains_key(path) {
            differences.push(format!("unexpected: {path}"));
        }
    }
    if differences.len() > PARITY_DIFF_LIMIT {
        let remaining = differences.len() - PARITY_DIFF_LIMIT;
        differences.truncate(PARITY_DIFF_LIMIT);
        differences.push(format!("...and {remaining} more differences"));
    }
    differences
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{DateTime, TimeZone, Utc};

    fn dt(year: i32, month: u32, day: u32, hour: u32, minute: u32, second: u32) -> DateTime<Utc> {
        Utc.with_ymd_and_hms(year, month, day, hour, minute, second)
            .single()
            .unwrap()
    }

    #[test]
    fn resolves_calver_from_explicit_or_date_override() {
        let explicit = CalverEnv {
            build_version: Some("2026.520.1".to_string()),
            fluxer_build_date: Some("2026-05-22T03:04:05Z".to_string()),
        };
        assert_eq!(
            resolve_calver(&explicit, dt(2026, 1, 1, 0, 0, 0)).unwrap(),
            "2026.520.1"
        );

        let generated = CalverEnv {
            fluxer_build_date: Some("2026-05-20T01:02:03Z".to_string()),
            ..CalverEnv::default()
        };
        assert_eq!(
            resolve_calver(&generated, dt(2026, 1, 1, 0, 0, 0)).unwrap(),
            "2026.520.10203"
        );
    }

    #[test]
    fn dockerfile_guards_the_trim_on_an_absolute_asset_base_url() {
        let dockerfile = include_str!("../../../fluxer_app_proxy/Dockerfile");
        let trim = dockerfile
            .split("FROM alpine:3.24 AS app-assets")
            .nth(1)
            .expect("app-assets stage");
        assert!(
            trim.contains("ARG PUBLIC_ASSET_BASE_URL"),
            "the app-assets stage must redeclare PUBLIC_ASSET_BASE_URL to be able to check it"
        );
        assert!(
            trim.contains("http://* | https://*"),
            "the trim must assert an absolute PUBLIC_ASSET_BASE_URL before deleting anything"
        );
    }

    #[test]
    fn dockerfile_workspace_manifest_keeps_the_release_profile() {
        let dockerfile = include_str!("../../../fluxer_app_proxy/Dockerfile");
        let manifest = dockerfile
            .split("'[workspace]'")
            .nth(1)
            .expect("synthesized workspace manifest")
            .split("> Cargo.toml")
            .next()
            .expect("synthesized workspace manifest body");
        for entry in [
            "'[profile.release]'",
            "'lto = \"fat\"'",
            "'codegen-units = 1'",
            "'strip = \"symbols\"'",
        ] {
            assert!(
                manifest.contains(entry),
                "the rust-builder workspace manifest replaces the repository root one, so it must contain {entry}"
            );
        }
        assert!(
            !manifest.contains("panic"),
            "fluxer_svc runs every request on its own tokio task, so a panicking handler must unwind instead of aborting the pod"
        );
    }

    #[test]
    fn manifest_reader_rejects_absolute_parent_and_backslash_paths() {
        for asset in [
            "../secret",
            "/assets/a.js",
            "assets/../secret",
            r"assets\app.js",
        ] {
            assert!(validate_manifest_asset(asset).is_err(), "{asset}");
        }
    }

    #[test]
    fn referenced_assets_collects_absolute_and_root_relative_urls() {
        let index = concat!(
            "<script src=\"https://fluxerstatic.com/assets/a.js\"></script>",
            "<link rel=\"stylesheet\" href=\"/assets/b.css\">",
            "<a href=\"assets/fonts-NOTICE.txt\">notice</a>",
            "<script src=\"https://fluxerstatic.com/assets/a.js\"></script>",
        );

        assert_eq!(
            referenced_assets(index),
            vec!["assets/a.js", "assets/b.css", "assets/fonts-NOTICE.txt"]
        );
    }

    #[test]
    fn referenced_assets_ignores_lookalike_paths() {
        assert!(
            referenced_assets(
                "<script src=\"/myassets/x.js\"></script><img src=\"/other/notassets/y.js\">"
            )
            .is_empty()
        );
    }

    #[test]
    fn referenced_assets_keeps_nested_asset_paths() {
        assert_eq!(
            referenced_assets("<script src=\"/assets/chunks/a.js\"></script>"),
            vec!["assets/chunks/a.js"]
        );
    }

    #[test]
    fn unproduced_references_flags_assets_the_build_did_not_produce() {
        let referenced = vec!["assets/a.js".to_string(), "assets/gone.js".to_string()];
        let produced = vec!["assets/a.js".to_string()];

        assert_eq!(
            unproduced_references(&referenced, &produced),
            vec!["assets/gone.js"]
        );
    }

    #[test]
    fn tree_differences_reports_missing_unexpected_and_changed() {
        let canonical = BTreeMap::from([
            ("assets/a.js".to_string(), "aa".to_string()),
            ("assets/b.js".to_string(), "bb".to_string()),
        ]);
        let other = BTreeMap::from([
            ("assets/a.js".to_string(), "changed".to_string()),
            ("assets/c.js".to_string(), "cc".to_string()),
        ]);

        assert_eq!(
            tree_differences(&canonical, &other),
            vec![
                "content differs: assets/a.js",
                "missing: assets/b.js",
                "unexpected: assets/c.js",
            ]
        );
    }

    #[test]
    fn self_hosted_workflow_keeps_assets_same_origin() {
        let workflow = include_str!("../../../.github/workflows/build-app-proxy-self-hosted.yaml");
        for entry in [
            "PUBLIC_ASSET_BASE_URL: \"\"",
            "BUNDLE_LOCAL_ASSETS: \"true\"",
        ] {
            assert!(
                workflow.contains(entry),
                "a self-hosted dist that inherits the hosted CDN default would ship index.html pointing at fluxerstatic.com, so the workflow must set {entry}"
            );
        }
    }
}
