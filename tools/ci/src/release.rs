// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, output_text, parse_version_instant, run_command};
use crate::functions::{remove_dir_if_exists, write_json_pretty};
use anyhow::{Context, Result, anyhow, ensure};
use chrono::Utc;
use clap::{ArgAction, Args, Subcommand};
use flate2::Compression;
use flate2::write::GzEncoder;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use sha2::{Digest as _, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::env;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use tar::{Builder, Header};
use tempfile::tempdir;
use walkdir::WalkDir;

const RELEASE_SCHEMA_VERSION: u32 = 1;
const RELEASE_MANIFEST_FILENAME: &str = "fluxer-release-manifest.json";
const RELEASE_FRAGMENT_FILENAME_PREFIX: &str = "fluxer-release-fragment-";
const DEFAULT_RELEASE_OUTPUT_DIR: &str = "release-out";

const SELF_HOSTED_IMAGE_COMPONENTS: &[&str] = &[
    "fluxer-admin",
    "fluxer-api",
    "fluxer-app-proxy-self-hosted",
    "fluxer-gateway",
    "fluxer-gifs",
    "fluxer-media-proxy",
    "fluxer-messages",
    "fluxer-snowflakes",
    "fluxer-static",
    "fluxer-unfurl",
    "fluxer-users",
];

#[derive(Debug, Args, Clone)]
pub struct ReleaseArgs {
    #[command(subcommand)]
    command: ReleaseCommand,
}

#[derive(Debug, Subcommand, Clone)]
#[clap(rename_all = "kebab_case")]
enum ReleaseCommand {
    PublishImage(PublishImageArgs),
    PublishAppProxy(PublishAppProxyArgs),
    PublishDesktop(PublishDesktopArgs),
    PublishHelm(PublishHelmArgs),
    PublishSelfHosting(PublishSelfHostingArgs),
    Finalise(FinaliseArgs),
}

#[derive(Debug, Args, Clone)]
pub struct PublishImageArgs {
    #[arg(long)]
    build_version: String,
    #[arg(long)]
    image: String,
    #[arg(long)]
    image_ref: Option<String>,
    #[arg(long)]
    digest: Option<String>,
    #[arg(long, default_value = "v1,latest")]
    moving_tags: String,
    #[arg(long)]
    source_sha: Option<String>,
}

#[derive(Debug, Args, Clone)]
pub struct PublishAppProxyArgs {
    #[command(flatten)]
    image: PublishImageArgs,
    #[arg(long)]
    asset_manifest: Option<PathBuf>,
}

#[derive(Debug, Args, Clone)]
pub struct PublishDesktopArgs {
    #[arg(long)]
    build_version: String,
    #[arg(long)]
    channel: String,
    #[arg(long, action = ArgAction::Set)]
    test_build: bool,
    #[arg(long)]
    s3_prefix: String,
    #[arg(long, default_value = "s3_payload")]
    payload_root: PathBuf,
    #[arg(long)]
    source_sha: Option<String>,
}

#[derive(Debug, Args, Clone)]
pub struct PublishHelmArgs {
    #[arg(long)]
    build_version: String,
    #[arg(long, default_value = DEFAULT_RELEASE_OUTPUT_DIR)]
    output_dir: PathBuf,
    #[arg(long)]
    source_sha: Option<String>,
}

#[derive(Debug, Args, Clone)]
pub struct PublishSelfHostingArgs {
    #[arg(long)]
    build_version: String,
    #[arg(long, default_value = DEFAULT_RELEASE_OUTPUT_DIR)]
    output_dir: PathBuf,
    #[arg(long)]
    source_sha: Option<String>,
}

#[derive(Debug, Args, Clone)]
pub struct FinaliseArgs {
    #[arg(long)]
    build_version: String,
    #[arg(long, default_value = DEFAULT_RELEASE_OUTPUT_DIR)]
    output_dir: PathBuf,
    #[arg(long)]
    source_sha: Option<String>,
}

pub async fn run(args: ReleaseArgs) -> Result<()> {
    match args.command {
        ReleaseCommand::PublishImage(args) => publish_image(args).await,
        ReleaseCommand::PublishAppProxy(args) => publish_app_proxy(args).await,
        ReleaseCommand::PublishDesktop(args) => publish_desktop(args).await,
        ReleaseCommand::PublishHelm(args) => publish_helm(args).await,
        ReleaseCommand::PublishSelfHosting(args) => publish_self_hosting(args).await,
        ReleaseCommand::Finalise(args) => finalise(args).await,
    }
}

async fn publish_image(args: PublishImageArgs) -> Result<()> {
    let version = validate_build_version(&args.build_version)?;
    let image_ref = args
        .image_ref
        .clone()
        .unwrap_or_else(|| format!("{}:{version}", args.image));
    let digest = match args
        .digest
        .as_deref()
        .map(str::trim)
        .filter(|v| !v.is_empty())
    {
        Some(digest) => digest.to_string(),
        None => inspect_image_digest(&image_ref)?,
    };
    let source_sha = resolve_source_sha(args.source_sha.as_deref())?;
    let moving_tags = parse_csv(&args.moving_tags);
    let fragment = image_fragment(
        &version,
        &source_sha,
        &args.image,
        &image_ref,
        &digest,
        moving_tags,
    );
    let fragment_path = write_fragment(
        &fragment_filename(&format!("image-{}", sanitize_asset_segment(&args.image))),
        &fragment,
    )?;
    publish_fragment(&version, &source_sha, false, true, &[])?;
    println!("Wrote release fragment: {}", fragment_path.display());
    Ok(())
}

async fn publish_app_proxy(args: PublishAppProxyArgs) -> Result<()> {
    let version = validate_build_version(&args.image.build_version)?;
    let image_ref = args
        .image
        .image_ref
        .clone()
        .unwrap_or_else(|| format!("{}:{version}", args.image.image));
    let digest = match args
        .image
        .digest
        .as_deref()
        .map(str::trim)
        .filter(|v| !v.is_empty())
    {
        Some(digest) => digest.to_string(),
        None => inspect_image_digest(&image_ref)?,
    };
    let source_sha = resolve_source_sha(args.image.source_sha.as_deref())?;
    let moving_tags = parse_csv(&args.image.moving_tags);
    let static_assets = match args.asset_manifest.as_deref() {
        Some(path) => Some(static_asset_manifest(path)?),
        None => None,
    };
    let fragment = json!({
        "schemaVersion": RELEASE_SCHEMA_VERSION,
        "kind": "app-proxy",
        "version": version,
        "sourceSha": source_sha,
        "image": image_payload(&args.image.image, &image_ref, &digest, moving_tags),
        "staticAssets": static_assets,
        "workflow": workflow_payload(),
    });
    let fragment_path = write_fragment(&fragment_filename("app-proxy"), &fragment)?;
    publish_fragment(&version, &source_sha, false, true, &[])?;
    println!("Wrote release fragment: {}", fragment_path.display());
    Ok(())
}

async fn publish_desktop(args: PublishDesktopArgs) -> Result<()> {
    let version = validate_build_version(&args.build_version)?;
    let source_sha = resolve_source_sha(args.source_sha.as_deref())?;
    let payload_dir = args.payload_root.join(&args.s3_prefix);
    ensure!(
        payload_dir.is_dir(),
        "Desktop payload directory does not exist: {}",
        payload_dir.display()
    );
    let output_dir = release_output_dir().join("desktop");
    fs::create_dir_all(&output_dir)
        .with_context(|| format!("Failed to create {}", output_dir.display()))?;
    let bundle_name = format!(
        "fluxer-desktop-{}-{version}.tar.gz",
        sanitize_asset_segment(&args.channel)
    );
    let bundle_path = output_dir.join(&bundle_name);
    create_tar_gz(
        &bundle_path,
        &payload_dir,
        &format!("fluxer-desktop-{version}"),
    )?;
    let manifests = collect_payload_manifests(&payload_dir)?;
    let fragment = json!({
        "schemaVersion": RELEASE_SCHEMA_VERSION,
        "kind": "desktop",
        "version": version,
        "sourceSha": source_sha,
        "channel": args.channel,
        "testBuild": args.test_build,
        "s3Prefix": args.s3_prefix,
        "payloadRoot": path_to_slash_string(&payload_dir),
        "bundle": file_payload(&bundle_path)?,
        "manifests": manifests,
        "workflow": workflow_payload(),
    });
    let fragment_path = write_fragment(
        &fragment_filename(&format!(
            "desktop-{}",
            sanitize_asset_segment(&args.channel)
        )),
        &fragment,
    )?;
    publish_fragment(
        &version,
        &source_sha,
        args.channel == "canary" || args.test_build,
        true,
        &[bundle_path],
    )?;
    println!("Wrote release fragment: {}", fragment_path.display());
    Ok(())
}

async fn publish_helm(args: PublishHelmArgs) -> Result<()> {
    let version = validate_build_version(&args.build_version)?;
    let source_sha = resolve_source_sha(args.source_sha.as_deref())?;
    let output_dir = args.output_dir.join("helm");
    remove_dir_if_exists(&output_dir)?;
    fs::create_dir_all(&output_dir)
        .with_context(|| format!("Failed to create {}", output_dir.display()))?;

    let chart_dirs = helm_chart_dirs(Path::new("deploy/helm"))?;
    for chart_dir in &chart_dirs {
        run_command(
            CommandSpec::new("helm")
                .args(["dependency", "update"])
                .arg(chart_dir),
        )?;
        run_command(
            CommandSpec::new("helm")
                .arg("package")
                .arg(chart_dir)
                .args(["--version", &version])
                .args(["--app-version", &version])
                .arg("--destination")
                .arg(&output_dir),
        )?;
    }

    let mut charts = Vec::new();
    let mut assets = Vec::new();
    for path in sorted_files(&output_dir)? {
        if path.extension().and_then(|ext| ext.to_str()) != Some("tgz") {
            continue;
        }
        charts.push(file_payload(&path)?);
        assets.push(path);
    }
    ensure!(!charts.is_empty(), "No Helm chart packages were generated");

    let fragment = json!({
        "schemaVersion": RELEASE_SCHEMA_VERSION,
        "kind": "helm",
        "version": version,
        "sourceSha": source_sha,
        "charts": charts,
        "workflow": workflow_payload(),
    });
    let fragment_path = write_fragment(&fragment_filename("helm"), &fragment)?;
    publish_fragment(&version, &source_sha, false, true, &assets)?;
    println!("Wrote release fragment: {}", fragment_path.display());
    Ok(())
}

async fn publish_self_hosting(args: PublishSelfHostingArgs) -> Result<()> {
    let version = validate_build_version(&args.build_version)?;
    let source_sha = resolve_source_sha(args.source_sha.as_deref())?;
    let output_dir = args.output_dir.join("self-hosting");
    fs::create_dir_all(&output_dir)
        .with_context(|| format!("Failed to create {}", output_dir.display()))?;
    let bundle_path = output_dir.join(format!("fluxer-self-hosting-{version}.tar.gz"));
    create_self_hosting_bundle(&bundle_path, &version)?;
    let fragment = json!({
        "schemaVersion": RELEASE_SCHEMA_VERSION,
        "kind": "self-hosting",
        "version": version,
        "sourceSha": source_sha,
        "bundle": file_payload(&bundle_path)?,
        "images": self_hosting_images(&version),
        "workflow": workflow_payload(),
    });
    let fragment_path = write_fragment(&fragment_filename("self-hosting"), &fragment)?;
    publish_fragment(&version, &source_sha, true, true, &[bundle_path])?;
    println!("Wrote release fragment: {}", fragment_path.display());
    Ok(())
}

async fn finalise(args: FinaliseArgs) -> Result<()> {
    let version = validate_build_version(&args.build_version)?;
    let source_sha = resolve_source_sha(args.source_sha.as_deref())?;
    ensure_release(&version, &source_sha, false, true)?;

    let fragments_dir = args.output_dir.join("fragments");
    let fragments = read_fragments(&fragments_dir)?;
    ensure!(
        !fragments.is_empty(),
        "No release fragments found in {} for {}",
        fragments_dir.display(),
        release_tag(&version)
    );
    let manifest = build_manifest(&version, &source_sha, fragments);
    let manifest_path = args.output_dir.join(RELEASE_MANIFEST_FILENAME);
    write_json_pretty(&manifest_path, &manifest)?;
    let notes_path = args.output_dir.join("fluxer-release-notes.md");
    fs::write(&notes_path, release_notes(&manifest))
        .with_context(|| format!("Failed to write {}", notes_path.display()))?;

    upload_release_assets(&version, &[manifest_path])?;
    publish_release(&version, &notes_path)
}

fn image_fragment(
    version: &str,
    source_sha: &str,
    image: &str,
    image_ref: &str,
    digest: &str,
    moving_tags: Vec<String>,
) -> Value {
    json!({
        "schemaVersion": RELEASE_SCHEMA_VERSION,
        "kind": "image",
        "version": version,
        "sourceSha": source_sha,
        "image": image_payload(image, image_ref, digest, moving_tags),
        "workflow": workflow_payload(),
    })
}

fn image_payload(image: &str, image_ref: &str, digest: &str, moving_tags: Vec<String>) -> Value {
    json!({
        "name": image,
        "ref": image_ref,
        "digest": digest,
        "digestRef": digest_ref(image_ref, digest),
        "movingTags": moving_tags,
    })
}

fn digest_ref(image_ref: &str, digest: &str) -> String {
    let image = image_ref
        .split_once(':')
        .map(|(image, _)| image)
        .unwrap_or(image_ref);
    format!("{image}@{digest}")
}

fn static_asset_manifest(path: &Path) -> Result<Value> {
    let text =
        fs::read_to_string(path).with_context(|| format!("Failed to read {}", path.display()))?;
    let assets = text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();
    Ok(json!({
        "filename": file_name(path)?,
        "assetCount": assets.len(),
        "sha256": sha256_file(path)?,
    }))
}

fn inspect_image_digest(image_ref: &str) -> Result<String> {
    let manifest = output_text(
        CommandSpec::new("docker")
            .args(["buildx", "imagetools", "inspect", image_ref])
            .args(["--format", "{{json .Manifest}}"]),
    )?;
    let value: Value = serde_json::from_str(&manifest)
        .with_context(|| format!("Failed to parse docker manifest for {image_ref}"))?;
    value
        .get("digest")
        .and_then(Value::as_str)
        .map(ToOwned::to_owned)
        .ok_or_else(|| anyhow!("Docker manifest for {image_ref} did not include a digest"))
}

fn publish_fragment(
    version: &str,
    source_sha: &str,
    prerelease: bool,
    draft: bool,
    assets: &[PathBuf],
) -> Result<()> {
    ensure_release(version, source_sha, prerelease, draft)?;
    upload_release_assets(version, assets)
}

fn ensure_release(version: &str, source_sha: &str, prerelease: bool, draft: bool) -> Result<()> {
    let tag = release_tag(version);
    if release_exists(&tag) {
        return Ok(());
    }

    let notes = format!(
        "Source: `{source_sha}`\n\nRelease manifest will be attached after finalisation.\n"
    );
    let notes_dir = tempdir()?;
    let notes_file = notes_dir.path().join("notes.md");
    fs::write(&notes_file, notes)
        .with_context(|| format!("Failed to write {}", notes_file.display()))?;
    let mut command = CommandSpec::new("gh")
        .args(["release", "create", &tag])
        .args(["--title", &release_title(version)])
        .args(["--latest=false"])
        .arg("--notes-file")
        .arg(&notes_file)
        .args(["--target", source_sha]);
    if prerelease {
        command = command.arg("--prerelease");
    }
    if draft {
        command = command.arg("--draft");
    }
    match run_command(command) {
        Ok(()) => Ok(()),
        Err(error) if release_exists(&tag) => {
            eprintln!(
                "Release {tag} was created concurrently; continuing. Original error: {error:#}"
            );
            Ok(())
        }
        Err(error) => Err(error),
    }
}

fn release_exists(tag: &str) -> bool {
    crate::common::command_succeeds(CommandSpec::new("gh").args(["release", "view", tag]))
}

fn upload_release_assets(version: &str, assets: &[PathBuf]) -> Result<()> {
    if assets.is_empty() {
        return Ok(());
    }
    let mut command = CommandSpec::new("gh")
        .args(["release", "upload", &release_tag(version)])
        .args(["--clobber"]);
    for asset in assets {
        ensure!(
            asset.is_file(),
            "Release asset is missing: {}",
            asset.display()
        );
        command = command.arg(asset);
    }
    run_command(command)
}

fn publish_release(version: &str, notes_path: &Path) -> Result<()> {
    let tag = release_tag(version);
    let release_id = release_database_id(&tag)?;
    let payload_dir = tempdir()?;
    let payload_path = payload_dir.path().join("release-update.json");
    write_json_pretty(
        &payload_path,
        &json!({
            "body": fs::read_to_string(notes_path)
                .with_context(|| format!("Failed to read {}", notes_path.display()))?,
            "draft": false,
            "make_latest": "false",
        }),
    )?;
    run_command(
        CommandSpec::new("gh")
            .args(["api", "-X", "PATCH"])
            .arg(format!("repos/{{owner}}/{{repo}}/releases/{release_id}"))
            .arg("--input")
            .arg(&payload_path),
    )
}

fn release_database_id(tag: &str) -> Result<String> {
    let release_id = output_text(release_database_id_command(tag))?;
    ensure!(
        !release_id.trim().is_empty(),
        "GitHub release {tag} did not include a database id"
    );
    Ok(release_id)
}

fn release_database_id_command(tag: &str) -> CommandSpec {
    CommandSpec::new("gh")
        .args(["release", "view", tag])
        .args(["--json", "databaseId"])
        .args(["--jq", ".databaseId"])
}

fn write_fragment(name: &str, value: &Value) -> Result<PathBuf> {
    let dir = release_output_dir().join("fragments");
    fs::create_dir_all(&dir).with_context(|| format!("Failed to create {}", dir.display()))?;
    let path = dir.join(name);
    write_json_pretty(&path, value)?;
    Ok(path)
}

fn fragment_filename(name: &str) -> String {
    format!("{RELEASE_FRAGMENT_FILENAME_PREFIX}{name}.json")
}

fn release_output_dir() -> PathBuf {
    env::var("RELEASE_OUTPUT_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from(DEFAULT_RELEASE_OUTPUT_DIR))
}

fn validate_build_version(version: &str) -> Result<String> {
    parse_version_instant(version)?;
    Ok(version.to_string())
}

fn release_tag(version: &str) -> String {
    version.to_string()
}

fn release_title(version: &str) -> String {
    release_tag(version)
}

fn resolve_source_sha(value: Option<&str>) -> Result<String> {
    value
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(ToOwned::to_owned)
        .or_else(|| env::var("GITHUB_SHA").ok())
        .or_else(|| output_text(CommandSpec::new("git").args(["rev-parse", "HEAD"])).ok())
        .ok_or_else(|| anyhow!("Unable to resolve source SHA"))
}

fn workflow_payload() -> Value {
    json!({
        "repository": env::var("GITHUB_REPOSITORY").ok(),
        "workflow": env::var("GITHUB_WORKFLOW").ok(),
        "runId": env::var("GITHUB_RUN_ID").ok(),
        "runAttempt": env::var("GITHUB_RUN_ATTEMPT").ok(),
        "serverUrl": env::var("GITHUB_SERVER_URL").ok(),
    })
}

fn parse_csv(value: &str) -> Vec<String> {
    value
        .split(',')
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(ToOwned::to_owned)
        .collect()
}

fn file_payload(path: &Path) -> Result<Value> {
    Ok(json!({
        "filename": file_name(path)?,
        "sha256": sha256_file(path)?,
        "bytes": fs::metadata(path)
            .with_context(|| format!("Failed to stat {}", path.display()))?
            .len(),
    }))
}

fn file_name(path: &Path) -> Result<String> {
    path.file_name()
        .and_then(|value| value.to_str())
        .map(ToOwned::to_owned)
        .ok_or_else(|| anyhow!("Invalid file name: {}", path.display()))
}

fn sha256_file(path: &Path) -> Result<String> {
    let mut file =
        File::open(path).with_context(|| format!("Failed to open {}", path.display()))?;
    let mut hasher = Sha256::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let read = file
            .read(&mut buffer)
            .with_context(|| format!("Failed to read {}", path.display()))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    Ok(hex::encode(hasher.finalize()))
}

fn collect_payload_manifests(payload_dir: &Path) -> Result<Vec<Value>> {
    let mut manifests = Vec::new();
    for path in sorted_files(payload_dir)? {
        if path.file_name().and_then(|value| value.to_str()) != Some("manifest.json") {
            continue;
        }
        let relative = path
            .strip_prefix(payload_dir)
            .with_context(|| format!("Failed to relativize {}", path.display()))?;
        manifests.push(json!({
            "path": path_to_slash_string(relative),
            "sha256": sha256_file(&path)?,
        }));
    }
    Ok(manifests)
}

fn sorted_files(root: &Path) -> Result<Vec<PathBuf>> {
    if !root.exists() {
        return Ok(Vec::new());
    }
    let mut files = WalkDir::new(root)
        .into_iter()
        .collect::<std::result::Result<Vec<_>, _>>()
        .with_context(|| format!("Failed to walk {}", root.display()))?
        .into_iter()
        .map(|entry| entry.path().to_path_buf())
        .filter(|path| path.is_file())
        .collect::<Vec<_>>();
    files.sort();
    Ok(files)
}

fn create_tar_gz(output: &Path, root: &Path, archive_root: &str) -> Result<()> {
    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    let file =
        File::create(output).with_context(|| format!("Failed to create {}", output.display()))?;
    let encoder = GzEncoder::new(file, Compression::default());
    let mut builder = Builder::new(encoder);
    for path in sorted_files(root)? {
        let relative = path
            .strip_prefix(root)
            .with_context(|| format!("Failed to relativize {}", path.display()))?;
        builder
            .append_path_with_name(&path, Path::new(archive_root).join(relative))
            .with_context(|| format!("Failed to add {} to {}", path.display(), output.display()))?;
    }
    builder
        .finish()
        .with_context(|| format!("Failed to finish {}", output.display()))?;
    Ok(())
}

fn create_self_hosting_bundle(output: &Path, version: &str) -> Result<()> {
    let root = Path::new("deploy/self-hosting");
    create_self_hosting_bundle_from(root, output, version)
}

fn create_self_hosting_bundle_from(root: &Path, output: &Path, version: &str) -> Result<()> {
    ensure!(root.is_dir(), "{} does not exist", root.display());
    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    let archive_root = format!("fluxer-self-hosting-{version}");
    let file =
        File::create(output).with_context(|| format!("Failed to create {}", output.display()))?;
    let encoder = GzEncoder::new(file, Compression::default());
    let mut builder = Builder::new(encoder);
    for path in sorted_files(root)? {
        let relative = path
            .strip_prefix(root)
            .with_context(|| format!("Failed to relativize {}", path.display()))?;
        builder
            .append_path_with_name(&path, Path::new(&archive_root).join(relative))
            .with_context(|| format!("Failed to add {} to {}", path.display(), output.display()))?;
    }
    append_generated_tar_file(
        &mut builder,
        &Path::new(&archive_root).join("release.env"),
        format!("FLUXER_IMAGE_TAG={version}\n").as_bytes(),
    )?;
    builder
        .finish()
        .with_context(|| format!("Failed to finish {}", output.display()))?;
    Ok(())
}

fn append_generated_tar_file<W: io::Write>(
    builder: &mut Builder<W>,
    path: &Path,
    contents: &[u8],
) -> Result<()> {
    let mut header = Header::new_gnu();
    header.set_size(contents.len() as u64);
    header.set_mode(0o644);
    header.set_cksum();
    builder
        .append_data(&mut header, path, contents)
        .with_context(|| format!("Failed to add generated {}", path.display()))
}

fn self_hosting_images(version: &str) -> Vec<Value> {
    SELF_HOSTED_IMAGE_COMPONENTS
        .iter()
        .map(|image| {
            json!({
                "name": image,
                "ref": format!("ghcr.io/fluxerapp/{image}:{version}"),
            })
        })
        .collect()
}

fn helm_chart_dirs(root: &Path) -> Result<Vec<PathBuf>> {
    let mut dirs = fs::read_dir(root)
        .with_context(|| format!("Failed to read {}", root.display()))?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::result::Result<Vec<_>, _>>()?;
    dirs.retain(|path| path.join("Chart.yaml").is_file());
    dirs.sort();
    Ok(dirs)
}

fn read_fragments(dir: &Path) -> Result<Vec<ReleaseFragment>> {
    let mut fragments = Vec::new();
    for path in sorted_files(dir)? {
        if !file_name(&path)?.starts_with(RELEASE_FRAGMENT_FILENAME_PREFIX) {
            continue;
        }
        let value: Value = serde_json::from_str(
            &fs::read_to_string(&path)
                .with_context(|| format!("Failed to read {}", path.display()))?,
        )
        .with_context(|| format!("Failed to parse {}", path.display()))?;
        fragments.push(ReleaseFragment {
            filename: file_name(&path)?,
            kind: value
                .get("kind")
                .and_then(Value::as_str)
                .unwrap_or("unknown")
                .to_string(),
            value,
        });
    }
    fragments.sort_by(|a, b| a.filename.cmp(&b.filename));
    Ok(fragments)
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct ReleaseFragment {
    filename: String,
    kind: String,
    value: Value,
}

fn build_manifest(version: &str, source_sha: &str, fragments: Vec<ReleaseFragment>) -> Value {
    let mut images = BTreeMap::new();
    let mut other = Vec::new();
    for fragment in &fragments {
        match fragment.kind.as_str() {
            "image" | "app-proxy" => {
                if let Some(image) = fragment.value.get("image")
                    && let Some(name) = image.get("name").and_then(Value::as_str)
                {
                    images.insert(name.to_string(), image.clone());
                }
            }
            _ => other.push(fragment.value.clone()),
        }
    }
    json!({
        "schemaVersion": RELEASE_SCHEMA_VERSION,
        "version": version,
        "tag": release_tag(version),
        "sourceSha": source_sha,
        "generatedAt": Utc::now().to_rfc3339(),
        "repository": env::var("GITHUB_REPOSITORY").ok(),
        "images": images,
        "artifacts": other,
        "fragments": fragments,
    })
}

fn release_notes(manifest: &Value) -> String {
    let source_sha = manifest
        .get("sourceSha")
        .and_then(Value::as_str)
        .unwrap_or("unknown");
    let images = manifest
        .get("images")
        .and_then(Value::as_object)
        .map(|images| images.keys().cloned().collect::<BTreeSet<_>>())
        .unwrap_or_default();
    let artifacts = manifest
        .get("artifacts")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();

    let mut notes =
        format!("Source: `{source_sha}`\n\nRelease manifest: `{RELEASE_MANIFEST_FILENAME}`\n");
    if !images.is_empty() {
        notes.push_str("\n## Images\n\n");
        for image in images {
            notes.push_str(&format!("- `{image}`\n"));
        }
    }
    if !artifacts.is_empty() {
        notes.push_str("\n## Artifacts\n\n");
        for artifact in artifacts {
            let kind = artifact
                .get("kind")
                .and_then(Value::as_str)
                .unwrap_or("artifact");
            notes.push_str(&format!("- `{kind}`\n"));
        }
    }
    notes
}

fn path_to_slash_string(path: &Path) -> String {
    path.components()
        .map(|component| component.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

fn sanitize_asset_segment(value: &str) -> String {
    value
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                ch
            } else {
                '-'
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_file(path: &Path, contents: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, contents).unwrap();
    }

    #[test]
    fn release_tag_uses_build_version_without_namespace() {
        assert_eq!(release_tag("2026.520.1"), "2026.520.1");
    }

    #[test]
    fn validate_build_version_rejects_invalid_calver() {
        assert!(validate_build_version("v1").is_err());
    }

    #[test]
    fn image_fragment_contains_digest_ref() {
        let fragment = image_fragment(
            "2026.520.1",
            "abc",
            "ghcr.io/fluxerapp/fluxer-api",
            "ghcr.io/fluxerapp/fluxer-api:2026.520.1",
            "sha256:123",
            vec!["latest".to_string()],
        );
        assert_eq!(fragment["kind"], "image");
        assert_eq!(
            fragment["image"]["digestRef"],
            "ghcr.io/fluxerapp/fluxer-api@sha256:123"
        );
    }

    #[test]
    fn static_asset_manifest_counts_non_empty_lines_and_hashes_file() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("assets-manifest.txt");
        fs::write(&path, "assets/a.js\n\nassets/b.css\n").unwrap();

        let manifest = static_asset_manifest(&path).unwrap();

        assert_eq!(manifest["assetCount"], 2);
        assert_eq!(manifest["filename"], "assets-manifest.txt");
        assert_eq!(manifest["sha256"].as_str().unwrap().len(), 64);
    }

    #[test]
    fn collect_payload_manifests_records_relative_paths() {
        let temp = tempdir().unwrap();
        write_file(
            &temp.path().join("desktop/canary/linux/x64/manifest.json"),
            "{}",
        );
        write_file(
            &temp.path().join("desktop/canary/linux/x64/file.txt"),
            "ignored",
        );

        let manifests = collect_payload_manifests(&temp.path().join("desktop")).unwrap();

        assert_eq!(manifests.len(), 1);
        assert_eq!(manifests[0]["path"], "canary/linux/x64/manifest.json");
    }

    #[test]
    fn self_hosting_bundle_includes_release_env() {
        let temp = tempdir().unwrap();
        let root = temp.path().join("self-hosting");
        fs::create_dir_all(&root).unwrap();
        fs::write(root.join("docker-compose.yml"), "name: fluxer\n").unwrap();
        let output = temp.path().join("bundle.tar.gz");

        create_self_hosting_bundle_from(&root, &output, "2026.520.1").unwrap();

        assert!(output.is_file());
        assert_eq!(sha256_file(&output).unwrap().len(), 64);
    }

    #[test]
    fn build_manifest_promotes_image_fragments() {
        let fragments = vec![
            ReleaseFragment {
                filename: "fluxer-release-fragment-image-fluxer-api.json".to_string(),
                kind: "image".to_string(),
                value: image_fragment(
                    "2026.520.1",
                    "abc",
                    "fluxer-api",
                    "ghcr.io/fluxerapp/fluxer-api:2026.520.1",
                    "sha256:123",
                    Vec::new(),
                ),
            },
            ReleaseFragment {
                filename: "fluxer-release-fragment-helm.json".to_string(),
                kind: "helm".to_string(),
                value: json!({"kind": "helm"}),
            },
        ];

        let manifest = build_manifest("2026.520.1", "abc", fragments);

        assert_eq!(manifest["images"]["fluxer-api"]["digest"], "sha256:123");
        assert_eq!(manifest["artifacts"][0]["kind"], "helm");
    }

    #[test]
    fn release_notes_include_images_and_artifact_kinds() {
        let manifest = json!({
            "version": "2026.520.1",
            "sourceSha": "abc",
            "images": {
                "fluxer-api": {"digest": "sha256:123"},
                "fluxer-users": {"digest": "sha256:456"},
            },
            "artifacts": [
                {"kind": "helm"},
                {"kind": "self-hosting"},
            ],
        });

        let notes = release_notes(&manifest);

        assert!(notes.contains("Source: `abc`"));
        assert!(notes.contains("Release manifest: `fluxer-release-manifest.json`"));
        assert!(notes.contains("- `fluxer-api`"));
        assert!(notes.contains("- `fluxer-users`"));
        assert!(notes.contains("- `helm`"));
        assert!(notes.contains("- `self-hosting`"));
    }

    #[test]
    fn release_database_id_command_uses_gh_release_view_for_drafts() {
        assert_eq!(
            release_database_id_command("2026.520.1"),
            CommandSpec::new("gh")
                .args(["release", "view", "2026.520.1"])
                .args(["--json", "databaseId"])
                .args(["--jq", ".databaseId"])
        );
    }

    #[test]
    fn self_hosting_images_match_compose_release_surface() {
        let images = self_hosting_images("2026.520.1")
            .into_iter()
            .map(|image| image["name"].as_str().unwrap().to_string())
            .collect::<Vec<_>>();

        assert_eq!(
            images,
            vec![
                "fluxer-admin",
                "fluxer-api",
                "fluxer-app-proxy-self-hosted",
                "fluxer-gateway",
                "fluxer-gifs",
                "fluxer-media-proxy",
                "fluxer-messages",
                "fluxer-snowflakes",
                "fluxer-static",
                "fluxer-unfurl",
                "fluxer-users",
            ]
        );
    }

    #[test]
    fn helm_chart_dirs_are_sorted_and_require_chart_yaml() {
        let temp = tempdir().unwrap();
        fs::create_dir_all(temp.path().join("b")).unwrap();
        fs::create_dir_all(temp.path().join("a")).unwrap();
        fs::create_dir_all(temp.path().join("ignored")).unwrap();
        fs::write(temp.path().join("b/Chart.yaml"), "name: b\n").unwrap();
        fs::write(temp.path().join("a/Chart.yaml"), "name: a\n").unwrap();

        let dirs = helm_chart_dirs(temp.path())
            .unwrap()
            .into_iter()
            .map(|path| file_name(&path).unwrap())
            .collect::<Vec<_>>();

        assert_eq!(dirs, vec!["a", "b"]);
    }

    #[test]
    fn digest_ref_replaces_tag_with_digest() {
        assert_eq!(
            digest_ref("ghcr.io/fluxerapp/fluxer-api:2026.520.1", "sha256:abc"),
            "ghcr.io/fluxerapp/fluxer-api@sha256:abc"
        );
    }

    #[test]
    fn parse_csv_trims_empty_entries() {
        assert_eq!(parse_csv("v1, latest,,"), vec!["v1", "latest"]);
    }
}
