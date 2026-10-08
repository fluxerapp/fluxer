// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, output_text, parse_version_instant, run_command};
use crate::desktop::{
    DESKTOP_CHANNEL_MANIFEST_NAME, DESKTOP_MODULE_PACKAGE_NAME, DESKTOP_MODULES_KEY_SEGMENT,
    DESKTOP_RENDERER_MODULE, DesktopChannelManifest, desktop_module_package_url,
    is_desktop_module_name,
};
use crate::functions::sha256_reader;
use anyhow::{Context, Result, anyhow, bail, ensure};
use chrono::{DateTime, Utc};
use clap::{Args, Subcommand};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::thread;
use std::time::Duration;

pub(crate) const RELEASE_REPOSITORY: &str = "fluxerapp/fluxer";
const RELEASE_COMPARE_URL: &str = "https://github.com/fluxerapp/fluxer/compare";
pub(crate) const DESKTOP_RELEASE_DESCRIPTOR_SCHEMA_VERSION: u8 = 2;
const DESKTOP_RELEASE_ARCHES: [&str; 2] = ["x64", "arm64"];

struct DesktopReleasePlatform {
    platform: &'static str,
    shipped_formats: &'static [&'static str],
    updater_feeds: &'static [&'static str],
    update_payload_suffix: Option<&'static str>,
    one_build_serves_every_arch: bool,
}

const DESKTOP_RELEASE_PLATFORMS: [DesktopReleasePlatform; 3] = [
    DesktopReleasePlatform {
        platform: "win32",
        shipped_formats: &["portable", "setup"],
        updater_feeds: &["RELEASES", "releases.win.json", "assets.win.json"],
        update_payload_suffix: Some("-full.nupkg"),
        one_build_serves_every_arch: false,
    },
    DesktopReleasePlatform {
        platform: "darwin",
        shipped_formats: &["dmg", "zip"],
        updater_feeds: &["RELEASES.json", "releases.json"],
        update_payload_suffix: None,
        one_build_serves_every_arch: true,
    },
    DesktopReleasePlatform {
        platform: "linux",
        shipped_formats: &["appimage", "deb", "rpm", "tar_gz"],
        updater_feeds: &[],
        update_payload_suffix: Some(".AppImage.zsync"),
        one_build_serves_every_arch: false,
    },
];

fn desktop_release_platform(platform: &str) -> Result<&'static DesktopReleasePlatform> {
    DESKTOP_RELEASE_PLATFORMS
        .iter()
        .find(|entry| entry.platform == platform)
        .ok_or_else(|| anyhow!("Unsupported desktop release platform {platform:?}"))
}

pub(crate) fn desktop_release_coordinates() -> Vec<(&'static str, &'static str)> {
    DESKTOP_RELEASE_PLATFORMS
        .iter()
        .flat_map(|entry| {
            DESKTOP_RELEASE_ARCHES
                .iter()
                .map(move |arch| (entry.platform, *arch))
        })
        .collect()
}

pub(crate) fn desktop_release_shipped_formats(platform: &str) -> Result<&'static [&'static str]> {
    Ok(desktop_release_platform(platform)?.shipped_formats)
}

pub(crate) fn desktop_release_updater_feeds(platform: &str) -> Result<&'static [&'static str]> {
    Ok(desktop_release_platform(platform)?.updater_feeds)
}

pub(crate) fn desktop_release_update_payload_suffix(
    platform: &str,
) -> Result<Option<&'static str>> {
    Ok(desktop_release_platform(platform)?.update_payload_suffix)
}

fn desktop_release_coordinate_routes(entry: &DesktopReleasePlatform) -> usize {
    entry.shipped_formats.len()
        + entry.updater_feeds.len()
        + usize::from(entry.update_payload_suffix.is_some())
}

fn desktop_release_route_inventory() -> BTreeMap<String, usize> {
    DESKTOP_RELEASE_PLATFORMS
        .iter()
        .flat_map(|entry| {
            DESKTOP_RELEASE_ARCHES.iter().map(move |arch| {
                (
                    format!("{}/{arch}", entry.platform),
                    desktop_release_coordinate_routes(entry),
                )
            })
        })
        .collect()
}

fn desktop_release_route_count() -> usize {
    desktop_release_route_inventory().values().sum()
}

fn desktop_release_asset_count() -> usize {
    DESKTOP_RELEASE_PLATFORMS
        .iter()
        .map(|entry| {
            let builds = if entry.one_build_serves_every_arch {
                1
            } else {
                DESKTOP_RELEASE_ARCHES.len()
            };
            let feeds = entry
                .updater_feeds
                .iter()
                .map(|name| desktop_release_asset_basename(entry.platform, name))
                .collect::<BTreeSet<_>>()
                .len();
            entry.shipped_formats.len() * builds
                + (feeds + usize::from(entry.update_payload_suffix.is_some()))
                    * DESKTOP_RELEASE_ARCHES.len()
        })
        .sum()
}

fn desktop_release_asset_basename<'a>(platform: &str, storage_filename: &'a str) -> &'a str {
    if platform == "darwin" && storage_filename.eq_ignore_ascii_case("releases.json") {
        "releases.json"
    } else {
        storage_filename
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub(crate) struct DesktopReleaseAsset {
    pub(crate) storage_key: String,
    pub(crate) release_asset: String,
    pub(crate) sha256: String,
    pub(crate) size: u64,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy, Default, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub(crate) enum DesktopReleaseKind {
    #[default]
    Full,
    Modules,
}

impl DesktopReleaseKind {
    pub(crate) fn is_full(&self) -> bool {
        *self == Self::Full
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub(crate) struct DesktopReleaseDescriptor {
    pub(crate) schema_version: u8,
    #[serde(default, skip_serializing_if = "DesktopReleaseKind::is_full")]
    pub(crate) kind: DesktopReleaseKind,
    pub(crate) channel: String,
    pub(crate) version: String,
    pub(crate) release_tag: String,
    pub(crate) source_sha: String,
    pub(crate) assets: Vec<DesktopReleaseAsset>,
    #[serde(default)]
    pub(crate) modules: Vec<DesktopReleaseAsset>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DesktopReleaseModuleKey<'a> {
    Manifest { platform: &'a str, arch: &'a str },
    Package { module: &'a str, sha256: &'a str },
}

fn is_lowercase_sha256(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
}

fn parse_desktop_release_module_key<'a>(
    storage_key: &'a str,
    channel: &str,
) -> Option<DesktopReleaseModuleKey<'a>> {
    match storage_key.split('/').collect::<Vec<_>>().as_slice() {
        ["desktop", key_channel, platform, arch, name]
            if *key_channel == channel
                && matches!(*platform, "win32" | "darwin" | "linux")
                && matches!(*arch, "x64" | "arm64")
                && *name == DESKTOP_CHANNEL_MANIFEST_NAME =>
        {
            Some(DesktopReleaseModuleKey::Manifest { platform, arch })
        }
        ["desktop", key_channel, segment, module, sha256, name]
            if *key_channel == channel
                && *segment == DESKTOP_MODULES_KEY_SEGMENT
                && is_desktop_module_name(module)
                && is_lowercase_sha256(sha256)
                && *name == DESKTOP_MODULE_PACKAGE_NAME =>
        {
            Some(DesktopReleaseModuleKey::Package { module, sha256 })
        }
        _ => None,
    }
}

pub(crate) fn desktop_module_manifest_storage_key(
    channel: &str,
    platform: &str,
    arch: &str,
) -> String {
    format!("desktop/{channel}/{platform}/{arch}/{DESKTOP_CHANNEL_MANIFEST_NAME}")
}

pub(crate) fn desktop_module_package_storage_key(
    channel: &str,
    module: &str,
    sha256: &str,
) -> String {
    format!(
        "desktop/{channel}/{DESKTOP_MODULES_KEY_SEGMENT}/{module}/{sha256}/{DESKTOP_MODULE_PACKAGE_NAME}"
    )
}

pub(crate) fn desktop_module_manifest_release_asset_name(
    channel: &str,
    version: &str,
    platform: &str,
    arch: &str,
) -> Result<String> {
    desktop_release_asset_name(
        channel,
        version,
        platform,
        arch,
        DESKTOP_CHANNEL_MANIFEST_NAME,
    )
}

pub(crate) fn desktop_module_package_release_asset_name(
    channel: &str,
    version: &str,
    module: &str,
    sha256: &str,
) -> Result<String> {
    ensure!(
        is_desktop_module_name(module),
        "Invalid desktop module name {module:?}"
    );
    ensure!(
        is_lowercase_sha256(sha256),
        "Invalid desktop module package SHA-256 {sha256:?}"
    );
    Ok(format!(
        "{}-{version}-module-{module}-{sha256}.br",
        desktop_release_product(channel)?
    ))
}

pub(crate) fn desktop_release_product(channel: &str) -> Result<&'static str> {
    match channel {
        "stable" => Ok("Fluxer"),
        "canary" => Ok("Fluxer-Canary"),
        other => bail!("Unsupported desktop release channel {other:?}"),
    }
}

pub(crate) fn desktop_release_descriptor_filename(channel: &str, version: &str) -> Result<String> {
    Ok(format!(
        "{}-{version}-release-manifest.json",
        desktop_release_product(channel)?
    ))
}

pub(crate) fn desktop_release_asset_name(
    channel: &str,
    version: &str,
    platform: &str,
    arch: &str,
    storage_filename: &str,
) -> Result<String> {
    let release_prefix = format!("{}-{version}-", desktop_release_product(channel)?);
    let platform_token = match platform {
        "win32" => "win",
        "darwin" => "mac",
        "linux" => "linux",
        other => bail!("Unsupported desktop release platform {other:?}"),
    };
    ensure!(
        matches!(arch, "x64" | "arm64"),
        "Unsupported desktop release architecture {arch:?}"
    );
    if storage_filename.starts_with(&release_prefix) {
        return Ok(storage_filename.to_string());
    }
    let release_filename = desktop_release_asset_basename(platform, storage_filename);
    Ok(format!(
        "{release_prefix}{platform_token}-{arch}-{release_filename}"
    ))
}

pub(crate) fn validate_desktop_release_descriptor(
    descriptor: &DesktopReleaseDescriptor,
    channel: &str,
    version: &str,
    source_sha: &str,
) -> Result<()> {
    ensure!(
        descriptor.schema_version == DESKTOP_RELEASE_DESCRIPTOR_SCHEMA_VERSION,
        "Unsupported desktop release descriptor schema version {}",
        descriptor.schema_version
    );
    ensure!(
        descriptor.channel == channel,
        "Desktop release descriptor channel {:?} does not match {channel:?}",
        descriptor.channel
    );
    ensure!(
        descriptor.version == version,
        "Desktop release descriptor version {:?} does not match {version:?}",
        descriptor.version
    );
    ensure!(
        descriptor.release_tag == format!("fluxer-desktop-{channel}@{version}"),
        "Desktop release descriptor tag {:?} is invalid",
        descriptor.release_tag
    );
    ensure!(
        descriptor.source_sha == source_sha,
        "Desktop release descriptor source SHA {:?} does not match {source_sha:?}",
        descriptor.source_sha
    );
    ensure!(
        source_sha.len() == 40
            && source_sha
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase()),
        "Invalid desktop release source SHA {source_sha:?}"
    );
    parse_version_instant(version)
        .with_context(|| format!("Invalid desktop release descriptor version {version:?}"))?;
    if descriptor.kind == DesktopReleaseKind::Modules {
        ensure!(
            descriptor.assets.is_empty(),
            "A modules-only desktop release descriptor must not list shell assets, found {}",
            descriptor.assets.len()
        );
        let descriptor_name = desktop_release_descriptor_filename(channel, version)?;
        let mut release_asset_names = BTreeMap::from([(
            descriptor_name.to_ascii_lowercase(),
            descriptor_name.as_str(),
        )]);
        return validate_desktop_release_modules(
            descriptor,
            channel,
            version,
            &mut release_asset_names,
        );
    }
    let route_count = desktop_release_route_count();
    ensure!(
        descriptor.assets.len() == route_count,
        "Desktop release descriptor must contain {route_count} routes, found {}",
        descriptor.assets.len()
    );
    let storage_prefix = format!("desktop/{channel}/");
    let release_prefix = format!("{}-{version}-", desktop_release_product(channel)?);
    let descriptor_name = desktop_release_descriptor_filename(channel, version)?;
    let mut storage_keys = BTreeSet::new();
    let mut route_counts = BTreeMap::<String, usize>::new();
    let mut release_assets = BTreeMap::<&str, (&str, u64)>::new();
    let mut release_asset_names = BTreeMap::from([(
        descriptor_name.to_ascii_lowercase(),
        descriptor_name.as_str(),
    )]);
    for asset in &descriptor.assets {
        ensure!(
            storage_keys.insert(asset.storage_key.as_str()),
            "Desktop release descriptor contains duplicate storage key {:?}",
            asset.storage_key
        );
        let key_segments = asset.storage_key.split('/').collect::<Vec<_>>();
        ensure!(
            key_segments.len() == 5
                && key_segments[0] == "desktop"
                && key_segments[1] == channel
                && matches!(key_segments[2], "win32" | "darwin" | "linux")
                && matches!(key_segments[3], "x64" | "arm64")
                && !key_segments[4].is_empty()
                && key_segments[4].bytes().all(|byte| {
                    byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'-' | b'_')
                })
                && asset.storage_key.starts_with(&storage_prefix),
            "Desktop release descriptor contains invalid storage key {:?}",
            asset.storage_key
        );
        *route_counts
            .entry(format!("{}/{}", key_segments[2], key_segments[3]))
            .or_default() += 1;
        let expected_release_asset = desktop_release_asset_name(
            channel,
            version,
            key_segments[2],
            key_segments[3],
            key_segments[4],
        )?;
        ensure!(
            asset.release_asset.starts_with(&release_prefix)
                && asset.release_asset != descriptor_name
                && asset.release_asset == expected_release_asset
                && asset.release_asset.bytes().all(|byte| {
                    byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'-' | b'_')
                }),
            "Desktop release descriptor contains invalid release asset {:?}",
            asset.release_asset
        );
        if let Some(existing) = release_asset_names.insert(
            asset.release_asset.to_ascii_lowercase(),
            asset.release_asset.as_str(),
        ) {
            ensure!(
                existing == asset.release_asset,
                "Desktop release asset names differ only by case: {existing:?} and {:?}",
                asset.release_asset
            );
        }
        ensure!(
            is_lowercase_sha256(&asset.sha256),
            "Desktop release descriptor contains invalid SHA-256 for {:?}",
            asset.release_asset
        );
        ensure!(
            asset.size > 0,
            "Desktop release descriptor contains an empty asset {:?}",
            asset.release_asset
        );
        if let Some((sha256, size)) = release_assets.get(asset.release_asset.as_str()) {
            ensure!(
                *sha256 == asset.sha256 && *size == asset.size,
                "Desktop release descriptor maps conflicting content to {:?}",
                asset.release_asset
            );
        } else {
            release_assets.insert(
                asset.release_asset.as_str(),
                (asset.sha256.as_str(), asset.size),
            );
        }
    }
    let asset_count = desktop_release_asset_count();
    ensure!(
        release_assets.len() == asset_count,
        "Desktop release descriptor must contain {asset_count} unique release assets, found {}",
        release_assets.len()
    );
    let expected_route_counts = desktop_release_route_inventory();
    ensure!(
        route_counts == expected_route_counts,
        "Desktop release descriptor route inventory mismatch: expected {expected_route_counts:?}, found {route_counts:?}"
    );
    validate_desktop_release_modules(descriptor, channel, version, &mut release_asset_names)
}

fn validate_desktop_release_modules<'a>(
    descriptor: &'a DesktopReleaseDescriptor,
    channel: &str,
    version: &str,
    release_asset_names: &mut BTreeMap<String, &'a str>,
) -> Result<()> {
    let mut storage_keys = BTreeSet::new();
    let mut manifests = BTreeSet::new();
    let mut packages = BTreeMap::<&str, &str>::new();
    for entry in &descriptor.modules {
        ensure!(
            storage_keys.insert(entry.storage_key.as_str()),
            "Desktop release descriptor contains duplicate module storage key {:?}",
            entry.storage_key
        );
        let key =
            parse_desktop_release_module_key(&entry.storage_key, channel).with_context(|| {
                format!(
                    "Desktop release descriptor contains invalid module storage key {:?}",
                    entry.storage_key
                )
            })?;
        let expected_release_asset = match key {
            DesktopReleaseModuleKey::Manifest { platform, arch } => {
                manifests.insert((platform, arch));
                desktop_module_manifest_release_asset_name(channel, version, platform, arch)?
            }
            DesktopReleaseModuleKey::Package { module, sha256 } => {
                ensure!(
                    entry.sha256 == sha256,
                    "Desktop module package {:?} hashes to {}, its storage key names {sha256}",
                    entry.release_asset,
                    entry.sha256
                );
                ensure!(
                    packages.insert(module, sha256).is_none(),
                    "Desktop release descriptor carries more than one package for module {module:?}"
                );
                desktop_module_package_release_asset_name(channel, version, module, sha256)?
            }
        };
        ensure!(
            entry.release_asset == expected_release_asset,
            "Desktop release descriptor contains invalid module release asset {:?}, expected {expected_release_asset:?}",
            entry.release_asset
        );
        if let Some(existing) = release_asset_names.insert(
            entry.release_asset.to_ascii_lowercase(),
            entry.release_asset.as_str(),
        ) {
            bail!(
                "Desktop release asset name {:?} collides with {existing:?}",
                entry.release_asset
            );
        }
        ensure!(
            is_lowercase_sha256(&entry.sha256),
            "Desktop release descriptor contains invalid SHA-256 for {:?}",
            entry.release_asset
        );
        ensure!(
            entry.size > 0,
            "Desktop release descriptor contains an empty asset {:?}",
            entry.release_asset
        );
    }
    let expected_manifests = desktop_release_coordinates()
        .into_iter()
        .collect::<BTreeSet<_>>();
    ensure!(
        manifests == expected_manifests,
        "Desktop release descriptor module manifests mismatch: expected {expected_manifests:?}, found {manifests:?}"
    );
    ensure!(
        packages.contains_key(DESKTOP_RENDERER_MODULE),
        "Desktop release descriptor carries no {DESKTOP_RENDERER_MODULE} package"
    );
    Ok(())
}

pub(crate) fn validate_desktop_release_module_files(
    descriptor: &DesktopReleaseDescriptor,
    asset_dir: &Path,
) -> Result<()> {
    let channel = descriptor.channel.as_str();
    let mut packages = BTreeMap::<String, (String, u64)>::new();
    let mut manifests = Vec::new();
    for entry in &descriptor.modules {
        let path = asset_dir.join(&entry.release_asset);
        let size = fs::metadata(&path)
            .with_context(|| format!("Failed to inspect {}", path.display()))?
            .len();
        ensure!(
            sha256_file(&path)? == entry.sha256 && size == entry.size,
            "Desktop release descriptor metadata does not match {:?}",
            entry.release_asset
        );
        match parse_desktop_release_module_key(&entry.storage_key, channel) {
            Some(DesktopReleaseModuleKey::Package { module, sha256 }) => {
                packages.insert(module.to_string(), (sha256.to_string(), size));
            }
            Some(DesktopReleaseModuleKey::Manifest { platform, arch }) => {
                manifests.push((platform, arch, entry, path));
            }
            None => bail!(
                "Desktop release descriptor contains invalid module storage key {:?}",
                entry.storage_key
            ),
        }
    }
    for (platform, arch, entry, path) in manifests {
        let manifest: DesktopChannelManifest = serde_json::from_slice(
            &fs::read(&path).with_context(|| format!("Failed to read {}", path.display()))?,
        )
        .with_context(|| format!("Failed to parse {}", path.display()))?;
        ensure!(
            manifest.release_channel == channel
                && manifest.platform == platform
                && manifest.arch == arch
                && manifest.build_version == descriptor.version,
            "Desktop module manifest {:?} does not describe {channel} {platform} {arch} {}",
            entry.release_asset,
            descriptor.version
        );
        let listed = manifest
            .modules
            .iter()
            .map(|(module, listed)| (module.clone(), (listed.sha256.clone(), listed.bytes)))
            .collect::<BTreeMap<_, _>>();
        ensure!(
            listed == packages,
            "Desktop module manifest {:?} lists {listed:?}, the release carries packages {packages:?}",
            entry.release_asset
        );
        for (module, listed) in &manifest.modules {
            ensure!(
                listed.url == desktop_module_package_url(channel, module, &listed.sha256),
                "Desktop module manifest {:?} points {module} at {:?}",
                entry.release_asset,
                listed.url
            );
        }
        for required in &manifest.required_modules {
            ensure!(
                manifest.modules.contains_key(required),
                "Desktop module manifest {:?} requires missing module {required}",
                entry.release_asset
            );
        }
    }
    Ok(())
}

#[derive(Debug, Args, Clone)]
pub struct ReleaseArgs {
    #[command(subcommand)]
    command: ReleaseCommand,
}

#[derive(Debug, Subcommand, Clone)]
#[clap(rename_all = "kebab_case")]
enum ReleaseCommand {
    Publish(PublishArgs),
}

#[derive(Debug, Args, Clone)]
struct PublishArgs {
    #[arg(long)]
    component: String,
    #[arg(long)]
    build_version: String,
    #[arg(long)]
    source_sha: String,
    #[arg(long)]
    previous_sha: Option<String>,
    #[arg(long)]
    prerelease: bool,
    #[arg(long)]
    asset_dir: Option<PathBuf>,
}

#[derive(Debug, Deserialize)]
struct ReleaseSummary {
    id: u64,
    tag_name: String,
    #[serde(rename = "draft")]
    is_draft: bool,
    published_at: Option<String>,
}

#[derive(Debug)]
struct QualifiedRelease {
    id: u64,
    tag: String,
    version_instant: DateTime<Utc>,
    published_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Copy)]
struct ReleaseHandle<'a> {
    id: u64,
    tag: &'a str,
}

#[derive(Debug, Deserialize)]
struct ReleaseDetail {
    id: u64,
    tag_name: String,
    target_commitish: String,
    name: Option<String>,
    body: Option<String>,
    draft: bool,
    prerelease: bool,
    assets: Vec<PublishedReleaseAsset>,
}

#[derive(Debug, Deserialize)]
struct PublishedReleaseAsset {
    name: String,
    label: Option<String>,
    size: u64,
    digest: Option<String>,
    state: String,
}

#[derive(Debug)]
struct LocalReleaseAsset {
    path: PathBuf,
    name: String,
    size: u64,
    digest: String,
}

#[derive(Debug, Deserialize)]
struct GitRef {
    #[serde(rename = "ref")]
    name: String,
}

const PUBLISH_ATTEMPTS: u64 = 3;

pub async fn run(args: ReleaseArgs) -> Result<()> {
    match args.command {
        ReleaseCommand::Publish(args) => retry_publish(
            PUBLISH_ATTEMPTS,
            |attempt| thread::sleep(Duration::from_secs(attempt * 15)),
            || publish(args.clone()),
        ),
    }
}

fn retry_publish(
    attempts: u64,
    mut wait: impl FnMut(u64),
    mut publish: impl FnMut() -> Result<()>,
) -> Result<()> {
    let mut attempt = 1;
    loop {
        match publish() {
            Ok(()) => return Ok(()),
            Err(error) if attempt < attempts => {
                eprintln!("Release publish attempt {attempt} of {attempts} failed: {error:#}");
                wait(attempt);
                attempt += 1;
            }
            Err(error) => return Err(error),
        }
    }
}

fn publish(args: PublishArgs) -> Result<()> {
    validate_component(&args.component)?;
    let version_instant = parse_version_instant(&args.build_version)?;
    let source_sha = validate_full_sha("source SHA", &args.source_sha)?;
    let resolved_source_sha = resolve_commit_sha(&source_sha).with_context(|| {
        format!("Source SHA {source_sha} is not a resolvable repository commit")
    })?;
    ensure!(
        resolved_source_sha == source_sha,
        "Source SHA {source_sha} resolved to unexpected commit {resolved_source_sha}"
    );

    let tag = release_tag(&args.component, &args.build_version);
    let title = release_title(&args.component, &args.build_version);
    let summaries = release_summaries()?;
    let qualified = qualified_releases(&summaries, &args.component)?;
    let existing_release = qualified.iter().find(|release| release.tag == tag);
    let existing_summary = summaries.iter().find(|release| release.tag_name == tag);

    if existing_release.is_none()
        && let Some(newer) = qualified
            .iter()
            .filter(|release| release.version_instant > version_instant)
            .max_by_key(|release| release.version_instant)
    {
        bail!(
            "Refusing to publish {tag}: newer component release {} already exists",
            newer.tag
        );
    }

    let previous_sha = match qualified
        .iter()
        .filter(|release| release.tag != tag)
        .filter(|release| {
            existing_release.is_none_or(|existing| {
                (release.published_at, release.id) < (existing.published_at, existing.id)
            })
        })
        .max_by_key(|release| (release.published_at, release.id))
    {
        Some(previous) => {
            let previous_sha = resolve_commit_sha(&previous.tag).with_context(|| {
                format!(
                    "Previous component release tag {} is not a resolvable repository commit",
                    previous.tag
                )
            })?;
            ensure!(
                previous_sha != source_sha,
                "Component {component} already has a prior qualified release at source SHA {source_sha}",
                component = args.component
            );
            ensure_ancestor(&previous_sha, &source_sha, false)?;
            previous_sha
        }
        None => {
            let baseline = args
                .previous_sha
                .as_deref()
                .map(str::trim)
                .filter(|value| !value.is_empty())
                .context("--previous-sha is required for the first qualified component release")?;
            let baseline = validate_full_sha("previous SHA", baseline)?;
            let resolved_baseline = resolve_commit_sha(&baseline).with_context(|| {
                format!("Previous SHA {baseline} is not a resolvable repository commit")
            })?;
            ensure!(
                resolved_baseline == baseline,
                "Previous SHA {baseline} resolved to unexpected commit {resolved_baseline}"
            );
            ensure_ancestor(&baseline, &source_sha, true)?;
            baseline
        }
    };

    let body = release_body(&previous_sha, &source_sha);
    let assets = local_release_assets(
        &args.component,
        &args.build_version,
        &source_sha,
        args.asset_dir.as_deref(),
    )?;
    if let Some(existing) = existing_summary.filter(|release| !release.is_draft) {
        ensure!(
            existing.published_at.is_some(),
            "Published release {tag} is missing its publication timestamp"
        );
        verify_release(
            ReleaseHandle {
                id: existing.id,
                tag: &tag,
            },
            &title,
            &body,
            &source_sha,
            args.prerelease,
            false,
            &assets,
        )?;
        println!("Release {tag} already exists with the expected state.");
        return Ok(());
    }

    let release_id = if let Some(existing) = existing_summary {
        ensure!(
            existing.published_at.is_none(),
            "Draft release {tag} unexpectedly has a publication timestamp"
        );
        existing.id
    } else {
        ensure!(
            !tag_exists(&tag)?,
            "Refusing to publish {tag}: the tag already exists without a matching GitHub Release"
        );
        create_draft_release(&tag, &title, &body, &source_sha, args.prerelease)?
    };
    let release = ReleaseHandle {
        id: release_id,
        tag: &tag,
    };
    upload_draft_release_assets(
        release,
        &title,
        &body,
        &source_sha,
        args.prerelease,
        &assets,
    )?;
    verify_release(
        release,
        &title,
        &body,
        &source_sha,
        args.prerelease,
        true,
        &assets,
    )?;
    run_command(
        CommandSpec::new("gh")
            .args(["release", "edit", &tag])
            .args(["--repo", RELEASE_REPOSITORY])
            .arg("--draft=false")
            .arg(format!("--prerelease={}", args.prerelease))
            .arg("--latest=false"),
    )?;
    verify_release(
        release,
        &title,
        &body,
        &source_sha,
        args.prerelease,
        false,
        &assets,
    )
}

fn validate_component(component: &str) -> Result<()> {
    ensure!(!component.is_empty(), "Release component must not be empty");
    ensure!(
        component.split('-').all(|segment| {
            !segment.is_empty()
                && segment
                    .bytes()
                    .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit())
        }),
        "Invalid release component {component:?}: expected lowercase letters, digits, and single hyphen separators"
    );
    Ok(())
}

pub(crate) fn validate_full_sha(label: &str, value: &str) -> Result<String> {
    let value = value.trim();
    ensure!(
        value.len() == 40 && value.bytes().all(|byte| byte.is_ascii_hexdigit()),
        "Invalid {label} {value:?}: expected a full 40-character commit SHA"
    );
    Ok(value.to_ascii_lowercase())
}

fn release_summaries() -> Result<Vec<ReleaseSummary>> {
    let output = output_text(
        CommandSpec::new("gh")
            .args(["api", "--paginate", "--slurp"])
            .arg(format!("repos/{RELEASE_REPOSITORY}/releases?per_page=100")),
    )?;
    let pages: Vec<Vec<ReleaseSummary>> =
        serde_json::from_str(&output).context("Failed to parse GitHub Release history")?;
    Ok(pages.into_iter().flatten().collect())
}

fn qualified_releases(
    summaries: &[ReleaseSummary],
    component: &str,
) -> Result<Vec<QualifiedRelease>> {
    let prefix = format!("{component}@");
    let mut qualified = Vec::new();
    for release in summaries.iter().filter(|release| !release.is_draft) {
        let Some(version) = release.tag_name.strip_prefix(&prefix) else {
            continue;
        };
        let Ok(version_instant) = parse_version_instant(version) else {
            continue;
        };
        let published_at = release.published_at.as_deref().with_context(|| {
            format!(
                "Published component release {} is missing its publication timestamp",
                release.tag_name
            )
        })?;
        let published_at = DateTime::parse_from_rfc3339(published_at)
            .with_context(|| {
                format!(
                    "Release {} has invalid published timestamp {published_at:?}",
                    release.tag_name
                )
            })?
            .with_timezone(&Utc);
        qualified.push(QualifiedRelease {
            id: release.id,
            tag: release.tag_name.clone(),
            version_instant,
            published_at,
        });
    }
    Ok(qualified)
}

pub(crate) fn resolve_commit_sha(reference: &str) -> Result<String> {
    let sha = output_text(
        CommandSpec::new("gh")
            .arg("api")
            .arg(format!("repos/{RELEASE_REPOSITORY}/commits/{reference}"))
            .args(["--jq", ".sha"]),
    )?;
    validate_full_sha("resolved commit SHA", &sha)
}

fn ensure_ancestor(previous_sha: &str, source_sha: &str, allow_identical: bool) -> Result<()> {
    let status = output_text(
        CommandSpec::new("gh")
            .arg("api")
            .arg(format!(
                "repos/{RELEASE_REPOSITORY}/compare/{previous_sha}...{source_sha}"
            ))
            .args(["--jq", ".status"]),
    )?;
    if status == "identical" {
        ensure!(
            allow_identical,
            "Identical compare range {previous_sha}..{source_sha} is allowed only for a component's first qualified release"
        );
        return Ok(());
    }
    ensure!(
        status == "ahead",
        "Previous SHA {previous_sha} is not an ancestor of source SHA {source_sha}; GitHub compare status is {status:?}"
    );
    Ok(())
}

fn tag_exists(tag: &str) -> Result<bool> {
    let output = output_text(
        CommandSpec::new("gh")
            .arg("api")
            .arg(format!(
                "repos/{RELEASE_REPOSITORY}/git/matching-refs/tags/{tag}"
            ))
            .args(["--jq", "map({ref: .ref})"]),
    )?;
    let refs: Vec<GitRef> =
        serde_json::from_str(&output).context("Failed to parse matching Git tag references")?;
    let expected = format!("refs/tags/{tag}");
    Ok(refs.iter().any(|git_ref| git_ref.name == expected))
}

fn local_release_assets(
    component: &str,
    version: &str,
    source_sha: &str,
    asset_dir: Option<&Path>,
) -> Result<Vec<LocalReleaseAsset>> {
    let Some(channel) = desktop_channel(component) else {
        ensure!(
            asset_dir.is_none(),
            "Release assets are supported only for desktop components"
        );
        return Ok(Vec::new());
    };
    let asset_dir = asset_dir.context("Desktop releases require --asset-dir")?;
    ensure!(
        asset_dir.is_dir(),
        "Desktop release asset directory does not exist: {}",
        asset_dir.display()
    );
    let product = desktop_release_product(channel)?;
    let prefix = format!("{product}-{version}-");
    let descriptor_name = desktop_release_descriptor_filename(channel, version)?;
    let descriptor_path = asset_dir.join(&descriptor_name);
    let descriptor: DesktopReleaseDescriptor = serde_json::from_slice(
        &fs::read(&descriptor_path)
            .with_context(|| format!("Failed to read {}", descriptor_path.display()))?,
    )
    .with_context(|| format!("Failed to parse {}", descriptor_path.display()))?;
    validate_desktop_release_descriptor(&descriptor, channel, version, source_sha)?;
    let mut entries = fs::read_dir(asset_dir)
        .with_context(|| {
            format!(
                "Failed to read desktop release assets in {}",
                asset_dir.display()
            )
        })?
        .collect::<std::result::Result<Vec<_>, _>>()?;
    entries.sort_by_key(std::fs::DirEntry::file_name);
    ensure!(
        !entries.is_empty(),
        "Desktop release asset directory is empty: {}",
        asset_dir.display()
    );

    let mut assets = Vec::with_capacity(entries.len());
    let mut case_folded_names = BTreeMap::<String, String>::new();
    for entry in entries {
        let path = entry.path();
        let metadata = fs::symlink_metadata(&path)
            .with_context(|| format!("Failed to inspect release asset {}", path.display()))?;
        ensure!(
            metadata.file_type().is_file(),
            "Release asset must be a regular file: {}",
            path.display()
        );
        ensure!(
            metadata.len() > 0,
            "Release asset is empty: {}",
            path.display()
        );
        let name = entry
            .file_name()
            .into_string()
            .map_err(|name| anyhow::anyhow!("Release asset name is not valid UTF-8: {name:?}"))?;
        ensure!(
            name.bytes()
                .all(|byte| { byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'-' | b'_') }),
            "Release asset name is not clean and URL-safe: {name:?}"
        );
        ensure!(
            name.starts_with(&prefix),
            "Release asset {name:?} must start with {prefix:?}"
        );
        if let Some(existing) = case_folded_names.insert(name.to_ascii_lowercase(), name.clone()) {
            ensure!(
                existing == name,
                "Release asset names differ only by case: {existing:?} and {name:?}"
            );
        }
        assets.push(LocalReleaseAsset {
            digest: sha256_file(&path)?,
            path,
            name,
            size: metadata.len(),
        });
    }
    let expected_names = descriptor
        .assets
        .iter()
        .chain(&descriptor.modules)
        .map(|asset| asset.release_asset.clone())
        .chain(std::iter::once(descriptor_name))
        .collect::<BTreeSet<_>>();
    let actual_names = assets
        .iter()
        .map(|asset| asset.name.clone())
        .collect::<BTreeSet<_>>();
    ensure!(
        actual_names == expected_names,
        "Desktop release asset inventory mismatch: expected {expected_names:?}, found {actual_names:?}"
    );
    let local_by_name = assets
        .iter()
        .map(|asset| (asset.name.as_str(), asset))
        .collect::<BTreeMap<_, _>>();
    for descriptor_asset in descriptor.assets.iter().chain(&descriptor.modules) {
        let local = local_by_name
            .get(descriptor_asset.release_asset.as_str())
            .with_context(|| {
                format!(
                    "Desktop release descriptor references missing asset {:?}",
                    descriptor_asset.release_asset
                )
            })?;
        ensure!(
            local.digest == descriptor_asset.sha256 && local.size == descriptor_asset.size,
            "Desktop release descriptor metadata does not match {:?}",
            descriptor_asset.release_asset
        );
    }
    validate_desktop_release_module_files(&descriptor, asset_dir)?;
    Ok(assets)
}

fn sha256_file(path: &Path) -> Result<String> {
    let file = File::open(path)
        .with_context(|| format!("Failed to open release asset {}", path.display()))?;
    sha256_reader(file).with_context(|| format!("Failed to read release asset {}", path.display()))
}

fn create_draft_release(
    tag: &str,
    title: &str,
    body: &str,
    source_sha: &str,
    prerelease: bool,
) -> Result<u64> {
    let output = output_text(
        CommandSpec::new("gh")
            .args(["api", "--method", "POST"])
            .arg(format!("repos/{RELEASE_REPOSITORY}/releases"))
            .arg("-f")
            .arg(format!("tag_name={tag}"))
            .arg("-f")
            .arg(format!("target_commitish={source_sha}"))
            .arg("-f")
            .arg(format!("name={title}"))
            .arg("-f")
            .arg(format!("body={body}"))
            .arg("-F")
            .arg("draft=true")
            .arg("-F")
            .arg(format!("prerelease={prerelease}"))
            .arg("-f")
            .arg("make_latest=false"),
    )?;
    let release: ReleaseDetail = serde_json::from_str(&output)
        .with_context(|| format!("Failed to parse created draft release {tag}"))?;
    ensure!(release.id > 0, "Draft release {tag} has an invalid ID");
    Ok(release.id)
}

fn release_detail(release_id: u64) -> Result<ReleaseDetail> {
    let output = output_text(
        CommandSpec::new("gh")
            .arg("api")
            .arg(format!("repos/{RELEASE_REPOSITORY}/releases/{release_id}")),
    )?;
    serde_json::from_str(&output)
        .with_context(|| format!("Failed to parse release ID {release_id}"))
}

fn upload_draft_release_assets(
    release: ReleaseHandle<'_>,
    title: &str,
    body: &str,
    source_sha: &str,
    prerelease: bool,
    expected_assets: &[LocalReleaseAsset],
) -> Result<()> {
    let detail = release_detail(release.id)?;
    verify_release_metadata(
        release.tag,
        &detail,
        title,
        body,
        source_sha,
        prerelease,
        true,
    )?;
    let expected_by_name = expected_assets
        .iter()
        .map(|asset| (asset.name.as_str(), asset))
        .collect::<BTreeMap<_, _>>();
    let mut published_by_name = BTreeMap::new();
    for asset in &detail.assets {
        ensure!(
            expected_by_name.contains_key(asset.name.as_str()),
            "Draft release {} contains unexpected asset {:?}",
            release.tag,
            asset.name
        );
        ensure!(
            published_by_name
                .insert(asset.name.as_str(), asset)
                .is_none(),
            "Draft release {} contains duplicate asset name {:?}",
            release.tag,
            asset.name
        );
    }
    let pending = expected_assets
        .iter()
        .filter(|expected| {
            published_by_name
                .get(expected.name.as_str())
                .is_none_or(|published| !release_asset_matches(published, expected))
        })
        .collect::<Vec<_>>();
    if pending.is_empty() {
        return Ok(());
    }
    let mut command = CommandSpec::new("gh")
        .args(["release", "upload", release.tag])
        .args(["--repo", RELEASE_REPOSITORY])
        .arg("--clobber");
    for asset in pending {
        command = command.arg(&asset.path);
    }
    run_command(command)
}

fn verify_release(
    release: ReleaseHandle<'_>,
    title: &str,
    body: &str,
    source_sha: &str,
    prerelease: bool,
    draft: bool,
    expected_assets: &[LocalReleaseAsset],
) -> Result<()> {
    let detail = release_detail(release.id)?;
    verify_release_metadata(
        release.tag,
        &detail,
        title,
        body,
        source_sha,
        prerelease,
        draft,
    )?;
    verify_release_assets(release.tag, &detail.assets, expected_assets)?;
    if draft {
        return Ok(());
    }
    let tag_sha = resolve_commit_sha(release.tag)?;
    ensure!(
        tag_sha == source_sha,
        "Release tag {} targets {tag_sha}, expected {source_sha}",
        release.tag
    );
    Ok(())
}

fn verify_release_metadata(
    tag: &str,
    release: &ReleaseDetail,
    title: &str,
    body: &str,
    source_sha: &str,
    prerelease: bool,
    draft: bool,
) -> Result<()> {
    ensure!(
        release.tag_name == tag,
        "Release {tag} has a mismatched tag"
    );
    ensure!(
        release.name.as_deref().unwrap_or_default() == title,
        "Release {tag} has a mismatched title"
    );
    ensure!(
        release.body.as_deref().unwrap_or_default() == body,
        "Release {tag} has a mismatched body"
    );
    ensure!(
        release.draft == draft,
        "Release {tag} has draft state {}, expected {draft}",
        release.draft
    );
    ensure!(
        release.prerelease == prerelease,
        "Release {tag} has a mismatched prerelease state"
    );
    let target_sha = resolve_commit_sha(&release.target_commitish)?;
    ensure!(
        target_sha == source_sha,
        "Release {tag} target resolves to {target_sha}, expected {source_sha}"
    );
    if draft && tag_exists(tag)? {
        let tag_sha = resolve_commit_sha(tag)?;
        ensure!(
            tag_sha == source_sha,
            "Draft release tag {tag} targets {tag_sha}, expected {source_sha}"
        );
    }
    Ok(())
}

fn release_asset_matches(published: &PublishedReleaseAsset, expected: &LocalReleaseAsset) -> bool {
    let expected_digest = format!("sha256:{}", expected.digest);
    published.label.as_deref().unwrap_or_default().is_empty()
        && published.state == "uploaded"
        && published.size == expected.size
        && published.digest.as_deref() == Some(expected_digest.as_str())
}

fn verify_release_assets(
    tag: &str,
    published_assets: &[PublishedReleaseAsset],
    expected_assets: &[LocalReleaseAsset],
) -> Result<()> {
    let mut published_by_name = BTreeMap::new();
    for asset in published_assets {
        ensure!(
            published_by_name
                .insert(asset.name.as_str(), asset)
                .is_none(),
            "Release {tag} contains duplicate asset name {:?}",
            asset.name
        );
    }
    let expected_names = expected_assets
        .iter()
        .map(|asset| asset.name.as_str())
        .collect::<Vec<_>>();
    let published_names = published_by_name.keys().copied().collect::<Vec<_>>();
    ensure!(
        published_names == expected_names,
        "Release {tag} asset inventory mismatch: expected {expected_names:?}, published {published_names:?}"
    );
    for expected in expected_assets {
        let published = published_by_name
            .get(expected.name.as_str())
            .with_context(|| format!("Release {tag} is missing asset {:?}", expected.name))?;
        ensure!(
            published.label.as_deref().unwrap_or_default().is_empty(),
            "Release {tag} asset {:?} has unexpected label {:?}",
            expected.name,
            published.label
        );
        ensure!(
            published.state == "uploaded",
            "Release {tag} asset {:?} is in unexpected state {:?}",
            expected.name,
            published.state
        );
        ensure!(
            published.size == expected.size,
            "Release {tag} asset {:?} has size {}, expected {}",
            expected.name,
            published.size,
            expected.size
        );
        let expected_digest = format!("sha256:{}", expected.digest);
        ensure!(
            published.digest.as_deref() == Some(expected_digest.as_str()),
            "Release {tag} asset {:?} has digest {:?}, expected {expected_digest}",
            expected.name,
            published.digest
        );
    }
    Ok(())
}

pub(crate) fn release_tag(component: &str, version: &str) -> String {
    format!("{component}@{version}")
}

fn release_title(component: &str, version: &str) -> String {
    format!("{component} {version}")
}

fn release_body(previous_sha: &str, source_sha: &str) -> String {
    format!(
        "Changes: [`{}..{}`]({RELEASE_COMPARE_URL}/{previous_sha}..{source_sha})",
        &previous_sha[..7],
        &source_sha[..7]
    )
}

fn desktop_channel(component: &str) -> Option<&str> {
    component.strip_prefix("fluxer-desktop-")
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::anyhow;

    const SAMPLE_CHANNEL: &str = "canary";
    const SAMPLE_VERSION: &str = "2026.913.210037";
    const SAMPLE_SOURCE_SHA: &str = "0123456789abcdef0123456789abcdef01234567";

    fn sample_storage_filenames(platform: &str, arch: &str, product: &str) -> Vec<String> {
        let prefix = format!("{product}-{SAMPLE_VERSION}");
        match platform {
            "win32" => vec![
                format!("{prefix}-portable-win-{arch}.zip"),
                format!("{prefix}-win-{arch}.exe"),
                "RELEASES".to_string(),
                "releases.win.json".to_string(),
                "assets.win.json".to_string(),
                format!("{prefix}-win-{arch}-full.nupkg"),
            ],
            "darwin" => vec![
                format!("{prefix}-mac-universal.dmg"),
                format!("{prefix}-mac-universal.zip"),
                "RELEASES.json".to_string(),
                "releases.json".to_string(),
            ],
            "linux" => vec![
                format!("{prefix}-linux-{arch}.AppImage"),
                format!("{prefix}-linux-{arch}.AppImage.zsync"),
                format!("{prefix}-linux-{arch}.deb"),
                format!("{prefix}-linux-{arch}.rpm"),
                format!("{prefix}-linux-{arch}.tar.gz"),
            ],
            other => panic!("unsupported desktop release platform {other:?}"),
        }
    }

    fn sample_descriptor() -> DesktopReleaseDescriptor {
        let product = desktop_release_product(SAMPLE_CHANNEL).unwrap();
        let mut contents = BTreeMap::<String, (String, u64)>::new();
        let mut assets = Vec::new();
        for (platform, arch) in desktop_release_coordinates() {
            for filename in sample_storage_filenames(platform, arch, product) {
                let release_asset = desktop_release_asset_name(
                    SAMPLE_CHANNEL,
                    SAMPLE_VERSION,
                    platform,
                    arch,
                    &filename,
                )
                .unwrap();
                let ordinal = contents.len() as u64 + 1;
                let (sha256, size) = contents
                    .entry(release_asset.clone())
                    .or_insert_with(|| (format!("{ordinal:064x}"), ordinal * 1024))
                    .clone();
                assets.push(DesktopReleaseAsset {
                    storage_key: format!("desktop/{SAMPLE_CHANNEL}/{platform}/{arch}/{filename}"),
                    release_asset,
                    sha256,
                    size,
                });
            }
        }
        let mut modules = desktop_release_coordinates()
            .into_iter()
            .enumerate()
            .map(|(index, (platform, arch))| DesktopReleaseAsset {
                storage_key: desktop_module_manifest_storage_key(SAMPLE_CHANNEL, platform, arch),
                release_asset: desktop_module_manifest_release_asset_name(
                    SAMPLE_CHANNEL,
                    SAMPLE_VERSION,
                    platform,
                    arch,
                )
                .unwrap(),
                sha256: format!("{:064x}", 500 + index),
                size: 700,
            })
            .collect::<Vec<_>>();
        for (index, module) in SAMPLE_MODULES.iter().enumerate() {
            let sha256 = format!("{:064x}", 900 + index);
            modules.push(sample_package_entry(module, &sha256, 4096));
        }
        DesktopReleaseDescriptor {
            schema_version: DESKTOP_RELEASE_DESCRIPTOR_SCHEMA_VERSION,
            kind: DesktopReleaseKind::Full,
            channel: SAMPLE_CHANNEL.to_string(),
            version: SAMPLE_VERSION.to_string(),
            release_tag: format!("fluxer-desktop-{SAMPLE_CHANNEL}@{SAMPLE_VERSION}"),
            source_sha: SAMPLE_SOURCE_SHA.to_string(),
            assets,
            modules,
        }
    }

    const SAMPLE_MODULES: [&str; 2] = ["fluxer_renderer", "fluxer_sourcemaps"];

    fn sample_package_entry(module: &str, sha256: &str, size: u64) -> DesktopReleaseAsset {
        DesktopReleaseAsset {
            storage_key: desktop_module_package_storage_key(SAMPLE_CHANNEL, module, sha256),
            release_asset: desktop_module_package_release_asset_name(
                SAMPLE_CHANNEL,
                SAMPLE_VERSION,
                module,
                sha256,
            )
            .unwrap(),
            sha256: sha256.to_string(),
            size,
        }
    }

    fn write_sample_module_release(
        dir: &Path,
        manifest_modules: &[&str],
    ) -> DesktopReleaseDescriptor {
        let mut descriptor = sample_descriptor();
        descriptor.modules.clear();
        let mut entries = BTreeMap::new();
        for module in SAMPLE_MODULES {
            let staged = dir.join(format!("{module}.staged"));
            fs::write(&staged, format!("{module} package bytes")).unwrap();
            let sha256 = sha256_file(&staged).unwrap();
            let size = fs::metadata(&staged).unwrap().len();
            let entry = sample_package_entry(module, &sha256, size);
            fs::rename(&staged, dir.join(&entry.release_asset)).unwrap();
            entries.insert(
                module.to_string(),
                crate::desktop::DesktopChannelManifestEntry {
                    url: desktop_module_package_url(SAMPLE_CHANNEL, module, &sha256),
                    sha256,
                    bytes: size,
                    minimum_shell_version: "0.0.0".to_string(),
                    maximum_shell_version: None,
                },
            );
            descriptor.modules.push(entry);
        }
        for (platform, arch) in desktop_release_coordinates() {
            let manifest = DesktopChannelManifest {
                manifest_version: 1,
                release_channel: SAMPLE_CHANNEL.to_string(),
                platform: platform.to_string(),
                arch: arch.to_string(),
                build_version: SAMPLE_VERSION.to_string(),
                pub_date: "2026-09-13T21:00:37Z".to_string(),
                metadata_version: 1,
                shell: crate::desktop::DesktopChannelManifestShell {
                    latest_version: SAMPLE_VERSION.to_string(),
                    minimum_version: "0.0.0".to_string(),
                },
                modules: entries
                    .iter()
                    .filter(|(module, _)| manifest_modules.contains(&module.as_str()))
                    .map(|(module, entry)| (module.clone(), entry.clone()))
                    .collect(),
                required_modules: vec![DESKTOP_RENDERER_MODULE.to_string()],
            };
            let release_asset = desktop_module_manifest_release_asset_name(
                SAMPLE_CHANNEL,
                SAMPLE_VERSION,
                platform,
                arch,
            )
            .unwrap();
            let path = dir.join(&release_asset);
            fs::write(&path, serde_json::to_vec(&manifest).unwrap()).unwrap();
            descriptor.modules.push(DesktopReleaseAsset {
                storage_key: desktop_module_manifest_storage_key(SAMPLE_CHANNEL, platform, arch),
                release_asset,
                sha256: sha256_file(&path).unwrap(),
                size: fs::metadata(&path).unwrap().len(),
            });
        }
        descriptor
    }

    fn sample_modules_only_descriptor() -> DesktopReleaseDescriptor {
        let mut descriptor = sample_descriptor();
        descriptor.kind = DesktopReleaseKind::Modules;
        descriptor.assets.clear();
        descriptor
    }

    #[test]
    fn modules_only_descriptor_validates_without_shell_assets() {
        validate_sample(&sample_modules_only_descriptor()).unwrap();
    }

    #[test]
    fn modules_only_descriptor_rejects_shell_assets() {
        let mut descriptor = sample_modules_only_descriptor();
        descriptor.assets = sample_descriptor().assets;
        let error = validate_sample(&descriptor).unwrap_err().to_string();
        assert!(error.contains("must not list shell assets"), "{error}");
    }

    #[test]
    fn modules_only_descriptor_still_requires_every_coordinate_and_the_renderer() {
        let mut descriptor = sample_modules_only_descriptor();
        descriptor
            .modules
            .retain(|entry| !entry.storage_key.contains("/fluxer_renderer/"));
        assert!(validate_sample(&descriptor).is_err());
        let mut descriptor = sample_modules_only_descriptor();
        descriptor
            .modules
            .retain(|entry| !entry.storage_key.starts_with("desktop/canary/linux/arm64/"));
        assert!(validate_sample(&descriptor).is_err());
    }

    #[test]
    fn full_descriptor_omits_the_kind_and_modules_only_carries_it() {
        let full = serde_json::to_value(sample_descriptor()).unwrap();
        assert!(full.get("kind").is_none());
        let modules_only = serde_json::to_value(sample_modules_only_descriptor()).unwrap();
        assert_eq!(modules_only["kind"], "modules");
        let parsed: DesktopReleaseDescriptor = serde_json::from_value(full).unwrap();
        assert_eq!(parsed.kind, DesktopReleaseKind::Full);
    }

    fn validate_sample(descriptor: &DesktopReleaseDescriptor) -> Result<()> {
        validate_desktop_release_descriptor(
            descriptor,
            SAMPLE_CHANNEL,
            SAMPLE_VERSION,
            SAMPLE_SOURCE_SHA,
        )
    }

    #[test]
    fn the_release_inventory_is_the_one_the_publisher_stages() {
        assert_eq!(
            desktop_release_route_inventory(),
            BTreeMap::from([
                ("darwin/arm64".to_string(), 4usize),
                ("darwin/x64".to_string(), 4usize),
                ("linux/arm64".to_string(), 5usize),
                ("linux/x64".to_string(), 5usize),
                ("win32/arm64".to_string(), 6usize),
                ("win32/x64".to_string(), 6usize),
            ])
        );
        assert_eq!(desktop_release_route_count(), 30);
        assert_eq!(desktop_release_asset_count(), 26);
    }

    #[test]
    fn a_complete_desktop_release_validates() {
        let descriptor = sample_descriptor();
        assert_eq!(descriptor.assets.len(), desktop_release_route_count());
        assert_eq!(
            descriptor
                .assets
                .iter()
                .map(|asset| asset.release_asset.as_str())
                .collect::<BTreeSet<_>>()
                .len(),
            desktop_release_asset_count()
        );
        validate_sample(&descriptor).unwrap();
    }

    #[test]
    fn the_two_macos_feed_names_and_the_universal_build_share_one_release_asset() {
        let descriptor = sample_descriptor();
        let asset_for = |storage_key_suffix: &str| {
            descriptor
                .assets
                .iter()
                .find(|asset| asset.storage_key.ends_with(storage_key_suffix))
                .map(|asset| asset.release_asset.clone())
                .unwrap()
        };
        assert_eq!(
            asset_for("darwin/x64/RELEASES.json"),
            asset_for("darwin/x64/releases.json")
        );
        assert_eq!(
            asset_for("darwin/x64/Fluxer-Canary-2026.913.210037-mac-universal.dmg"),
            asset_for("darwin/arm64/Fluxer-Canary-2026.913.210037-mac-universal.dmg")
        );
        assert_ne!(
            asset_for("darwin/x64/RELEASES.json"),
            asset_for("darwin/arm64/RELEASES.json")
        );
    }

    #[test]
    fn a_release_missing_a_route_is_refused() {
        let mut descriptor = sample_descriptor();
        descriptor.assets.pop().unwrap();
        assert_eq!(
            validate_sample(&descriptor).unwrap_err().to_string(),
            "Desktop release descriptor must contain 30 routes, found 29"
        );
    }

    #[test]
    fn a_release_with_an_extra_route_is_refused() {
        let mut descriptor = sample_descriptor();
        let extra = DesktopReleaseAsset {
            storage_key: format!("desktop/{SAMPLE_CHANNEL}/linux/x64/latest-linux.yml"),
            release_asset: format!("Fluxer-Canary-{SAMPLE_VERSION}-linux-x64-latest-linux.yml"),
            sha256: format!("{:064x}", 99u64),
            size: 4096,
        };
        descriptor.assets.push(extra);
        assert_eq!(
            validate_sample(&descriptor).unwrap_err().to_string(),
            "Desktop release descriptor must contain 30 routes, found 31"
        );
    }

    #[test]
    fn the_release_publishes_no_per_coordinate_manifest() {
        for (platform, _) in desktop_release_coordinates() {
            assert!(
                !desktop_release_updater_feeds(platform)
                    .unwrap()
                    .contains(&"manifest.json")
            );
        }
        for asset in sample_descriptor().assets {
            assert!(!asset.storage_key.ends_with("/manifest.json"));
        }
    }

    #[test]
    fn module_release_assets_follow_the_contract_names() {
        let sha256 = "ab".repeat(32);
        assert_eq!(
            desktop_module_manifest_release_asset_name("canary", SAMPLE_VERSION, "win32", "x64")
                .unwrap(),
            "Fluxer-Canary-2026.913.210037-win-x64-modules.json"
        );
        assert_eq!(
            desktop_module_manifest_release_asset_name("stable", SAMPLE_VERSION, "darwin", "arm64")
                .unwrap(),
            "Fluxer-2026.913.210037-mac-arm64-modules.json"
        );
        assert_eq!(
            desktop_module_manifest_storage_key("canary", "linux", "arm64"),
            "desktop/canary/linux/arm64/modules.json"
        );
        assert_eq!(
            desktop_module_package_release_asset_name(
                "canary",
                SAMPLE_VERSION,
                "fluxer_renderer",
                &sha256
            )
            .unwrap(),
            format!("Fluxer-Canary-2026.913.210037-module-fluxer_renderer-{sha256}.br")
        );
        assert_eq!(
            desktop_module_package_storage_key("stable", "fluxer_renderer", &sha256),
            format!("desktop/stable/modules/fluxer_renderer/{sha256}/package.br")
        );
        assert!(
            desktop_module_package_release_asset_name("canary", SAMPLE_VERSION, "Bad", &sha256)
                .is_err()
        );
        assert!(
            desktop_module_package_release_asset_name(
                "canary",
                SAMPLE_VERSION,
                "fluxer_renderer",
                "ABCD"
            )
            .is_err()
        );
    }

    #[test]
    fn a_schema_two_descriptor_round_trips_with_its_modules() {
        let descriptor = sample_descriptor();
        assert_eq!(descriptor.schema_version, 2);
        let json = serde_json::to_value(&descriptor).unwrap();
        assert_eq!(json["modules"].as_array().unwrap().len(), 8);
        let parsed: DesktopReleaseDescriptor = serde_json::from_value(json).unwrap();
        assert_eq!(parsed, descriptor);

        let mut empty = sample_descriptor();
        empty.modules.clear();
        assert!(
            serde_json::to_value(&empty)
                .unwrap()
                .as_object()
                .unwrap()
                .contains_key("modules")
        );
    }

    #[test]
    fn a_schema_one_descriptor_parses_but_is_refused() {
        let mut json = serde_json::to_value(sample_descriptor()).unwrap();
        let object = json.as_object_mut().unwrap();
        object.remove("modules");
        object.insert("schema_version".to_string(), 1.into());
        let parsed: DesktopReleaseDescriptor = serde_json::from_value(json).unwrap();
        assert!(parsed.modules.is_empty());
        assert_eq!(
            validate_sample(&parsed).unwrap_err().to_string(),
            "Unsupported desktop release descriptor schema version 1"
        );
    }

    #[test]
    fn a_release_without_a_renderer_package_is_refused() {
        let mut descriptor = sample_descriptor();
        descriptor
            .modules
            .retain(|entry| !entry.storage_key.contains("/fluxer_renderer/"));
        assert_eq!(
            validate_sample(&descriptor).unwrap_err().to_string(),
            "Desktop release descriptor carries no fluxer_renderer package"
        );
    }

    #[test]
    fn a_package_whose_hash_differs_from_its_path_is_refused() {
        let mut descriptor = sample_descriptor();
        let package = descriptor
            .modules
            .iter_mut()
            .find(|entry| entry.storage_key.ends_with("/package.br"))
            .unwrap();
        package.sha256 = "f".repeat(64);
        assert!(
            validate_sample(&descriptor)
                .unwrap_err()
                .to_string()
                .contains("its storage key names")
        );
    }

    #[test]
    fn a_release_missing_a_coordinate_module_manifest_is_refused() {
        let mut descriptor = sample_descriptor();
        descriptor
            .modules
            .retain(|entry| entry.storage_key != "desktop/canary/linux/arm64/modules.json");
        assert!(
            validate_sample(&descriptor)
                .unwrap_err()
                .to_string()
                .starts_with("Desktop release descriptor module manifests mismatch")
        );
    }

    #[test]
    fn module_entries_with_wrong_keys_or_names_are_refused() {
        let mut descriptor = sample_descriptor();
        descriptor.modules[0].storage_key = "desktop/canary/win32/x64/modules/manifest.json".into();
        assert!(
            validate_sample(&descriptor)
                .unwrap_err()
                .to_string()
                .contains("invalid module storage key")
        );

        let mut descriptor = sample_descriptor();
        descriptor.modules[0].release_asset = "Fluxer-Canary-2026.913.210037-modules.json".into();
        assert!(
            validate_sample(&descriptor)
                .unwrap_err()
                .to_string()
                .contains("invalid module release asset")
        );

        let mut descriptor = sample_descriptor();
        let duplicate = descriptor.modules.last().unwrap().clone();
        let sha256 = "e".repeat(64);
        descriptor.modules.push(DesktopReleaseAsset {
            storage_key: desktop_module_package_storage_key(
                SAMPLE_CHANNEL,
                "fluxer_sourcemaps",
                &sha256,
            ),
            release_asset: desktop_module_package_release_asset_name(
                SAMPLE_CHANNEL,
                SAMPLE_VERSION,
                "fluxer_sourcemaps",
                &sha256,
            )
            .unwrap(),
            sha256,
            size: duplicate.size,
        });
        assert_eq!(
            validate_sample(&descriptor).unwrap_err().to_string(),
            "Desktop release descriptor carries more than one package for module \"fluxer_sourcemaps\""
        );
    }

    #[test]
    fn module_release_asset_names_are_unique_across_the_whole_release() {
        let descriptor = sample_descriptor();
        let shell_names = descriptor
            .assets
            .iter()
            .map(|asset| asset.release_asset.to_ascii_lowercase())
            .collect::<BTreeSet<_>>();
        let module_names = descriptor
            .modules
            .iter()
            .map(|entry| entry.release_asset.to_ascii_lowercase())
            .collect::<BTreeSet<_>>();
        assert_eq!(module_names.len(), descriptor.modules.len());
        assert!(shell_names.is_disjoint(&module_names));

        let mut descriptor = sample_descriptor();
        let repeated = descriptor.modules[0].clone();
        descriptor.modules.push(repeated);
        assert_eq!(
            validate_sample(&descriptor).unwrap_err().to_string(),
            "Desktop release descriptor contains duplicate module storage key \"desktop/canary/win32/x64/modules.json\""
        );
    }

    #[test]
    fn module_files_whose_manifests_match_the_packages_validate() {
        let temp = tempfile::tempdir().unwrap();
        let descriptor = write_sample_module_release(temp.path(), &SAMPLE_MODULES);
        validate_sample(&descriptor).unwrap();
        validate_desktop_release_module_files(&descriptor, temp.path()).unwrap();
    }

    #[test]
    fn a_manifest_that_omits_a_shipped_package_is_refused() {
        let temp = tempfile::tempdir().unwrap();
        let descriptor = write_sample_module_release(temp.path(), &["fluxer_renderer"]);
        validate_sample(&descriptor).unwrap();
        let error = validate_desktop_release_module_files(&descriptor, temp.path())
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("the release carries packages"),
            "unexpected error {error}"
        );
    }

    #[test]
    fn a_manifest_that_references_an_unshipped_package_is_refused() {
        let temp = tempfile::tempdir().unwrap();
        let mut descriptor = write_sample_module_release(temp.path(), &SAMPLE_MODULES);
        descriptor
            .modules
            .retain(|entry| !entry.storage_key.contains("/fluxer_sourcemaps/"));
        validate_sample(&descriptor).unwrap();
        assert!(
            validate_desktop_release_module_files(&descriptor, temp.path())
                .unwrap_err()
                .to_string()
                .contains("the release carries packages")
        );
    }

    #[test]
    fn a_tampered_module_file_is_refused() {
        let temp = tempfile::tempdir().unwrap();
        let descriptor = write_sample_module_release(temp.path(), &SAMPLE_MODULES);
        let package = descriptor
            .modules
            .iter()
            .find(|entry| entry.storage_key.ends_with("/package.br"))
            .unwrap();
        fs::write(temp.path().join(&package.release_asset), "tampered").unwrap();
        assert!(
            validate_desktop_release_module_files(&descriptor, temp.path())
                .unwrap_err()
                .to_string()
                .starts_with("Desktop release descriptor metadata does not match")
        );
    }

    fn write_sample_release_dir(dir: &Path) -> DesktopReleaseDescriptor {
        let mut descriptor = write_sample_module_release(dir, &SAMPLE_MODULES);
        for asset in &mut descriptor.assets {
            let path = dir.join(&asset.release_asset);
            if !path.exists() {
                fs::write(&path, format!("{} bytes", asset.release_asset)).unwrap();
            }
            asset.sha256 = sha256_file(&path).unwrap();
            asset.size = fs::metadata(&path).unwrap().len();
        }
        fs::write(
            dir.join(desktop_release_descriptor_filename(SAMPLE_CHANNEL, SAMPLE_VERSION).unwrap()),
            serde_json::to_vec(&descriptor).unwrap(),
        )
        .unwrap();
        descriptor
    }

    #[test]
    fn the_publisher_uploads_every_module_release_asset() {
        let temp = tempfile::tempdir().unwrap();
        let descriptor = write_sample_release_dir(temp.path());
        let uploaded = local_release_assets(
            "fluxer-desktop-canary",
            SAMPLE_VERSION,
            SAMPLE_SOURCE_SHA,
            Some(temp.path()),
        )
        .unwrap()
        .into_iter()
        .map(|asset| asset.name)
        .collect::<BTreeSet<_>>();
        for entry in &descriptor.modules {
            assert!(uploaded.contains(&entry.release_asset));
        }
        assert_eq!(
            uploaded.len(),
            desktop_release_asset_count() + descriptor.modules.len() + 1
        );
    }

    #[test]
    fn the_publisher_refuses_a_release_missing_a_module_file() {
        let temp = tempfile::tempdir().unwrap();
        let descriptor = write_sample_release_dir(temp.path());
        let package = descriptor
            .modules
            .iter()
            .find(|entry| entry.storage_key.ends_with("/package.br"))
            .unwrap();
        fs::remove_file(temp.path().join(&package.release_asset)).unwrap();
        assert!(
            local_release_assets(
                "fluxer-desktop-canary",
                SAMPLE_VERSION,
                SAMPLE_SOURCE_SHA,
                Some(temp.path()),
            )
            .unwrap_err()
            .to_string()
            .starts_with("Desktop release asset inventory mismatch")
        );
    }

    #[test]
    fn retry_publish_retries_until_a_publish_succeeds() {
        let mut calls = 0;
        let mut waits = Vec::new();
        let result = retry_publish(
            3,
            |attempt| waits.push(attempt),
            || {
                calls += 1;
                if calls < 3 {
                    Err(anyhow!("unexpected end of JSON input"))
                } else {
                    Ok(())
                }
            },
        );
        assert!(result.is_ok());
        assert_eq!(calls, 3);
        assert_eq!(waits, vec![1, 2]);
    }

    #[test]
    fn retry_publish_returns_the_last_error_without_waiting_after_it() {
        let mut calls = 0;
        let mut waits = Vec::new();
        let result = retry_publish(
            3,
            |attempt| waits.push(attempt),
            || {
                calls += 1;
                Err(anyhow!("attempt {calls} failed"))
            },
        );
        assert_eq!(result.unwrap_err().to_string(), "attempt 3 failed");
        assert_eq!(calls, 3);
        assert_eq!(waits, vec![1, 2]);
    }

    #[test]
    fn retry_publish_does_not_retry_a_successful_publish() {
        let mut calls = 0;
        let result = retry_publish(
            3,
            |_| panic!("a successful publish must not wait"),
            || {
                calls += 1;
                Ok(())
            },
        );
        assert!(result.is_ok());
        assert_eq!(calls, 1);
    }
}
