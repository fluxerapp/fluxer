// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, output_text, parse_version_instant, run_command};
use anyhow::{Context, Result, bail, ensure};
use chrono::{DateTime, Utc};
use clap::{Args, Subcommand};
use serde::Deserialize;
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::io::Read;
use std::path::{Path, PathBuf};

const RELEASE_REPOSITORY: &str = "fluxerapp/fluxer";
const RELEASE_COMPARE_URL: &str = "https://github.com/fluxerapp/fluxer/compare";
pub(crate) const DESKTOP_RELEASE_ASSET_SUFFIXES: &[&str] = &[
    "linux-aarch64.rpm",
    "linux-amd64.deb",
    "linux-arm64.AppImage",
    "linux-arm64.deb",
    "linux-arm64.tar.gz",
    "linux-x64.tar.gz",
    "linux-x86_64.AppImage",
    "linux-x86_64.rpm",
    "mac-universal.dmg",
    "mac-universal.zip",
    "portable-win-arm64.zip",
    "portable-win-x64.zip",
    "win-arm64.exe",
    "win-x64.exe",
];

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

pub async fn run(args: ReleaseArgs) -> Result<()> {
    match args.command {
        ReleaseCommand::Publish(args) => publish(args),
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
    ensure!(
        component != "fluxer-marketing" && component != "marketing",
        "Marketing must not publish a public GitHub Release"
    );
    Ok(())
}

fn validate_full_sha(label: &str, value: &str) -> Result<String> {
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

fn resolve_commit_sha(reference: &str) -> Result<String> {
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
    let product = match channel {
        "stable" => "Fluxer",
        "canary" => "Fluxer.Canary",
        other => bail!("Unsupported desktop release channel {other:?}"),
    };
    let prefix = format!("{product}-{version}-");
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
        let suffix = name
            .strip_prefix(&prefix)
            .with_context(|| format!("Release asset {name:?} must start with {prefix:?}"))?;
        ensure!(
            DESKTOP_RELEASE_ASSET_SUFFIXES.contains(&suffix),
            "Release asset {name:?} is not a supported shipped desktop artifact"
        );
        assets.push(LocalReleaseAsset {
            digest: sha256_file(&path)?,
            path,
            name,
            size: metadata.len(),
        });
    }
    let expected_names = DESKTOP_RELEASE_ASSET_SUFFIXES
        .iter()
        .map(|suffix| format!("{prefix}{suffix}"))
        .collect::<BTreeSet<_>>();
    let actual_names = assets
        .iter()
        .map(|asset| asset.name.clone())
        .collect::<BTreeSet<_>>();
    ensure!(
        actual_names == expected_names,
        "Desktop release asset inventory mismatch: expected {expected_names:?}, found {actual_names:?}"
    );
    Ok(assets)
}

fn sha256_file(path: &Path) -> Result<String> {
    let mut file = File::open(path)
        .with_context(|| format!("Failed to open release asset {}", path.display()))?;
    let mut hasher = Sha256::new();
    let mut buffer = [0u8; 64 * 1024];
    loop {
        let read = file
            .read(&mut buffer)
            .with_context(|| format!("Failed to read release asset {}", path.display()))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    Ok(hex::encode(hasher.finalize()))
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

fn release_tag(component: &str, version: &str) -> String {
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
