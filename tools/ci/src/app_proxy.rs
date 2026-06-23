// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{
    CALVER_SCHEME, CalverEnv, CommandSpec, S3UploadPlanItem, append_github_env,
    append_github_output, collect_files, path_to_s3_key, require_env, resolve_calver, run_command,
    runner_temp, s3_client, trim_option, upload_s3_plan_append_only,
};
use anyhow::{Context, Result, anyhow, ensure};
use base64::Engine;
use base64::engine::general_purpose::STANDARD as BASE64;
use chrono::Utc;
use clap::{Args, ValueEnum};
use serde_json::{Map, Value, json};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

const DEFAULT_PUBLIC_ASSET_BASE_URL: &str = "https://fluxerstatic.com";
const DEFAULT_APP_PROXY_TIME_FREEZE_ENABLED: &str = "true";
const DEFAULT_STATIC_BUCKET: &str = "fluxer-static";
const DEFAULT_S3_ENDPOINT: &str = "https://ewr1.vultrobjects.com";
const IMMUTABLE_ASSET_CACHE_CONTROL: &str = "public, max-age=31536000, immutable";

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
    BuildAndExtract,
    GenerateAssetManifest,
    UploadAssets,
}

pub async fn run(args: BuildAppProxyArgs) -> Result<()> {
    match args.step {
        AppProxyStep::SetMetadata => set_metadata_step(args.build_version.as_deref()),
        AppProxyStep::PrepareDockerConfig => prepare_docker_config_step(),
        AppProxyStep::ConfigureGhcrAuth => configure_ghcr_auth_step(),
        AppProxyStep::BuildAndExtract => build_and_extract_step(),
        AppProxyStep::GenerateAssetManifest => generate_asset_manifest_step(),
        AppProxyStep::UploadAssets => upload_assets_step().await,
    }
}

fn set_metadata_step(build_version_arg: Option<&str>) -> Result<()> {
    let calver_env = CalverEnv {
        build_version: trim_option(build_version_arg.map(ToOwned::to_owned))
            .or_else(|| trim_option(env::var("BUILD_VERSION").ok())),
        fluxer_build_version: trim_option(env::var("FLUXER_BUILD_VERSION").ok()),
        fluxer_build_date: trim_option(env::var("FLUXER_BUILD_DATE").ok()),
    };
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

fn build_and_extract_step() -> Result<()> {
    run_command(build_and_extract_command()?)
}

fn build_and_extract_command() -> Result<CommandSpec> {
    let build_version = require_env("BUILD_VERSION")?;
    let public_asset_base_url = env::var("PUBLIC_ASSET_BASE_URL")
        .unwrap_or_else(|_| DEFAULT_PUBLIC_ASSET_BASE_URL.to_string());
    let image_repo = match env::var("IMAGE_REPO") {
        Ok(value) => value,
        Err(_) => format!("ghcr.io/{}/fluxer-app-proxy", ghcr_owner()?),
    };
    Ok(CommandSpec::new("docker")
        .args(["buildx", "bake", "-f", "fluxer_app_proxy/docker-bake.hcl"])
        .env("IMAGE_REPO", image_repo)
        .env("BUILD_VERSION", build_version)
        .env("PUBLIC_ASSET_BASE_URL", public_asset_base_url)
        .env(
            "FLUXER_APP_PROXY_TIME_FREEZE_ENABLED",
            env::var("FLUXER_APP_PROXY_TIME_FREEZE_ENABLED")
                .unwrap_or_else(|_| DEFAULT_APP_PROXY_TIME_FREEZE_ENABLED.to_string()),
        )
        .env(
            "CACHE_FROM",
            env::var("CACHE_FROM")
                .unwrap_or_else(|_| "type=gha,scope=fluxer-app-proxy".to_string()),
        )
        .env(
            "CACHE_TO",
            env::var("CACHE_TO")
                .unwrap_or_else(|_| "type=gha,scope=fluxer-app-proxy,mode=max".to_string()),
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
        .filter(|path| !is_source_map_asset(path))
        .map(|path| {
            path.strip_prefix(dist)
                .with_context(|| format!("Failed to relativize {}", path.display()))
                .map(path_to_s3_key)
        })
        .collect::<Result<Vec<_>>>()?;
    entries.sort();
    Ok(entries)
}

fn is_source_map_asset(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("map"))
}

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::parse_version_instant;
    use chrono::{DateTime, TimeZone, Utc};
    use std::ffi::OsString;

    fn dt(year: i32, month: u32, day: u32, hour: u32, minute: u32, second: u32) -> DateTime<Utc> {
        Utc.with_ymd_and_hms(year, month, day, hour, minute, second)
            .single()
            .unwrap()
    }

    fn write_file(path: &Path, contents: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, contents).unwrap();
    }

    #[test]
    fn resolves_calver_from_explicit_or_date_override() {
        let explicit = CalverEnv {
            build_version: Some("2026.520.1".to_string()),
            fluxer_build_version: Some("2026.521.2".to_string()),
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
    fn rejects_invalid_calver_time() {
        assert_eq!(
            parse_version_instant("2026.520.246000")
                .unwrap_err()
                .to_string(),
            "Invalid build version date/time: 2026.520.246000"
        );
    }

    #[test]
    fn ghcr_auth_config_merges_existing_auths() {
        let temp = tempfile::tempdir().unwrap();
        let config_path = temp.path().join("config.json");
        fs::write(
            &config_path,
            r#"{"auths":{"example.com":{"auth":"old"}},"currentContext":"builder"}"#,
        )
        .unwrap();

        write_ghcr_auth_config(&config_path, "octo", "secret").unwrap();

        let config: Value =
            serde_json::from_str(&fs::read_to_string(config_path).unwrap()).unwrap();
        assert_eq!(config["auths"]["example.com"]["auth"], "old");
        assert_eq!(
            config["auths"]["ghcr.io"]["auth"],
            BASE64.encode("octo:secret")
        );
        assert_eq!(config["currentContext"], "builder");
    }

    #[test]
    fn ghcr_auth_config_rejects_non_object_roots_and_auths() {
        let temp = tempfile::tempdir().unwrap();
        let config_path = temp.path().join("config.json");
        fs::write(&config_path, "[]").unwrap();
        assert_eq!(
            write_ghcr_auth_config(&config_path, "octo", "secret")
                .unwrap_err()
                .to_string(),
            "Docker config root must be a JSON object"
        );

        fs::write(&config_path, r#"{"auths":[]}"#).unwrap();
        assert_eq!(
            write_ghcr_auth_config(&config_path, "octo", "secret")
                .unwrap_err()
                .to_string(),
            "Docker config auths must be a JSON object"
        );
    }

    #[test]
    fn build_command_sets_bake_environment() {
        let command = CommandSpec::new("docker")
            .args(["buildx", "bake", "-f", "fluxer_app_proxy/docker-bake.hcl"])
            .env("IMAGE_REPO", "ghcr.io/example/fluxer-app-proxy")
            .env("BUILD_VERSION", "2026.520.1")
            .env("PUBLIC_ASSET_BASE_URL", DEFAULT_PUBLIC_ASSET_BASE_URL)
            .env(
                "FLUXER_APP_PROXY_TIME_FREEZE_ENABLED",
                DEFAULT_APP_PROXY_TIME_FREEZE_ENABLED,
            );

        assert_eq!(command.program, OsString::from("docker"));
        assert_eq!(
            command.args,
            vec![
                OsString::from("buildx"),
                OsString::from("bake"),
                OsString::from("-f"),
                OsString::from("fluxer_app_proxy/docker-bake.hcl"),
            ]
        );
        assert!(command.env.contains(&(
            OsString::from("BUILD_VERSION"),
            OsString::from("2026.520.1")
        )));
        assert!(command.env.contains(&(
            OsString::from("FLUXER_APP_PROXY_TIME_FREEZE_ENABLED"),
            OsString::from(DEFAULT_APP_PROXY_TIME_FREEZE_ENABLED)
        )));
    }

    #[test]
    fn asset_manifest_entries_are_sorted_and_relative_to_dist() {
        let temp = tempfile::tempdir().unwrap();
        let dist = temp.path().join("dist");
        write_file(&dist.join("assets/z.js"), "z");
        write_file(&dist.join("assets/z.js.map"), "{}");
        write_file(&dist.join("assets/chunks/a.js"), "a");
        write_file(&dist.join("assets/chunks/a.js.map"), "{}");
        write_file(&dist.join("index.html"), "ignored");

        assert_eq!(
            asset_manifest_entries(&dist).unwrap(),
            vec!["assets/chunks/a.js", "assets/z.js"]
        );
    }

    #[test]
    fn asset_manifest_entries_require_assets_directory() {
        let temp = tempfile::tempdir().unwrap();
        let dist = temp.path().join("dist");
        fs::create_dir_all(&dist).unwrap();

        assert!(
            asset_manifest_entries(&dist)
                .unwrap_err()
                .to_string()
                .contains("App proxy assets directory is missing")
        );
    }

    #[test]
    fn manifest_reader_trims_blank_lines_and_keeps_order() {
        let temp = tempfile::tempdir().unwrap();
        let manifest = temp.path().join("assets-manifest.txt");
        fs::write(&manifest, "\n assets/b.js \n\nassets/a.js\n").unwrap();

        assert_eq!(
            read_asset_manifest(&manifest).unwrap(),
            vec!["assets/b.js", "assets/a.js"]
        );
    }

    #[test]
    fn asset_upload_plan_preserves_manifest_keys() {
        let temp = tempfile::tempdir().unwrap();
        let dist = temp.path().join("dist");
        write_file(&dist.join("assets/a.js"), "a");
        write_file(&dist.join("assets/chunks/b.js"), "b");
        let assets = vec!["assets/a.js".to_string(), "assets/chunks/b.js".to_string()];

        let plan = asset_upload_plan(&dist, &assets).unwrap();

        assert_eq!(
            plan.iter()
                .map(|item| item.key.as_str())
                .collect::<Vec<_>>(),
            vec!["assets/a.js", "assets/chunks/b.js"]
        );
        assert_eq!(plan[0].path, dist.join("assets/a.js"));
        assert_eq!(plan[1].path, dist.join("assets/chunks/b.js"));
        assert_eq!(
            plan[0].content_type.as_deref(),
            Some("application/javascript; charset=utf-8")
        );
        assert_eq!(
            plan[0].cache_control.as_deref(),
            Some(IMMUTABLE_ASSET_CACHE_CONTROL)
        );
        assert!(plan[0].repair_existing_metadata);
    }

    #[test]
    fn asset_upload_plan_rejects_manifest_entries_missing_on_disk() {
        let temp = tempfile::tempdir().unwrap();
        let dist = temp.path().join("dist");
        fs::create_dir_all(&dist).unwrap();
        let assets = vec!["assets/missing.js".to_string()];

        assert!(
            asset_upload_plan(&dist, &assets)
                .unwrap_err()
                .to_string()
                .contains("Manifest asset is missing")
        );
    }

    #[test]
    fn manifest_reader_rejects_paths_outside_assets() {
        let temp = tempfile::tempdir().unwrap();
        let manifest = temp.path().join("assets-manifest.txt");
        fs::write(&manifest, "assets/a.js\n../secret\n").unwrap();

        assert!(read_asset_manifest(&manifest).is_err());
    }

    #[test]
    fn manifest_reader_rejects_absolute_parent_and_backslash_paths() {
        for asset in ["/assets/a.js", "assets/../secret", r"assets\app.js"] {
            assert!(validate_manifest_asset(asset).is_err(), "{asset}");
        }
    }

    #[test]
    fn path_to_s3_key_uses_forward_slashes() {
        assert_eq!(
            path_to_s3_key(Path::new("assets").join("chunks").join("a.js").as_path()),
            "assets/chunks/a.js"
        );
    }
}
