// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, output_text, parse_version_instant, run_command};
use anyhow::{Context, Result, bail, ensure};
use chrono::{DateTime, Utc};
use clap::{Args, Subcommand};
use serde::Deserialize;
use serde_json::Value;

const RELEASE_REPOSITORY: &str = "fluxerapp/fluxer";
const RELEASE_COMPARE_URL: &str = "https://github.com/fluxerapp/fluxer/compare";
const DESKTOP_DOWNLOAD_URL: &str = "https://api.fluxer.app/dl/desktop";

struct DesktopDownload {
    arch: &'static str,
    label: &'static str,
    format: &'static str,
}

struct DesktopPlatform {
    name: &'static str,
    slug: &'static str,
    downloads: &'static [DesktopDownload],
}

const DESKTOP_PLATFORMS: &[DesktopPlatform] = &[
    DesktopPlatform {
        name: "Windows",
        slug: "win32",
        downloads: &[
            DesktopDownload {
                arch: "x64",
                label: "Setup.exe",
                format: "setup",
            },
            DesktopDownload {
                arch: "x64",
                label: "Portable ZIP",
                format: "portable",
            },
            DesktopDownload {
                arch: "arm64",
                label: "Setup.exe",
                format: "setup",
            },
            DesktopDownload {
                arch: "arm64",
                label: "Portable ZIP",
                format: "portable",
            },
        ],
    },
    DesktopPlatform {
        name: "macOS",
        slug: "darwin",
        downloads: &[
            DesktopDownload {
                arch: "x64",
                label: "DMG",
                format: "dmg",
            },
            DesktopDownload {
                arch: "x64",
                label: "ZIP",
                format: "zip",
            },
            DesktopDownload {
                arch: "arm64",
                label: "DMG",
                format: "dmg",
            },
            DesktopDownload {
                arch: "arm64",
                label: "ZIP",
                format: "zip",
            },
        ],
    },
    DesktopPlatform {
        name: "Linux",
        slug: "linux",
        downloads: &[
            DesktopDownload {
                arch: "x64",
                label: "AppImage",
                format: "appimage",
            },
            DesktopDownload {
                arch: "x64",
                label: "DEB",
                format: "deb",
            },
            DesktopDownload {
                arch: "x64",
                label: "RPM",
                format: "rpm",
            },
            DesktopDownload {
                arch: "x64",
                label: "tar.gz",
                format: "tar_gz",
            },
            DesktopDownload {
                arch: "arm64",
                label: "AppImage",
                format: "appimage",
            },
            DesktopDownload {
                arch: "arm64",
                label: "DEB",
                format: "deb",
            },
            DesktopDownload {
                arch: "arm64",
                label: "RPM",
                format: "rpm",
            },
            DesktopDownload {
                arch: "arm64",
                label: "tar.gz",
                format: "tar_gz",
            },
        ],
    },
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

#[derive(Debug, Deserialize)]
struct ReleaseDetail {
    tag_name: String,
    target_commitish: String,
    name: Option<String>,
    body: Option<String>,
    draft: bool,
    prerelease: bool,
    assets: Vec<Value>,
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
    if let Some(existing) = summaries.iter().find(|release| release.tag_name == tag) {
        ensure!(
            !existing.is_draft && existing.published_at.is_some(),
            "Release {tag} exists but is not a published, non-draft component release"
        );
    }

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

    let body = release_body(
        &args.component,
        &args.build_version,
        &previous_sha,
        &source_sha,
    );
    if summaries.iter().any(|release| release.tag_name == tag) {
        verify_existing_release(&tag, &title, &body, &source_sha, args.prerelease)?;
        println!("Release {tag} already exists with the expected state.");
        return Ok(());
    }

    ensure!(
        !tag_exists(&tag)?,
        "Refusing to publish {tag}: the tag already exists without a matching GitHub Release"
    );

    let mut command = CommandSpec::new("gh")
        .args(["release", "create", &tag])
        .args(["--repo", RELEASE_REPOSITORY])
        .args(["--title", &title])
        .args(["--notes", &body])
        .args(["--target", &source_sha])
        .args(["--latest=false"]);
    if args.prerelease {
        command = command.arg("--prerelease");
    }
    run_command(command)?;
    verify_existing_release(&tag, &title, &body, &source_sha, args.prerelease)
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

fn verify_existing_release(
    tag: &str,
    title: &str,
    body: &str,
    source_sha: &str,
    prerelease: bool,
) -> Result<()> {
    let output = output_text(
        CommandSpec::new("gh")
            .arg("api")
            .arg(format!("repos/{RELEASE_REPOSITORY}/releases/tags/{tag}")),
    )?;
    let release: ReleaseDetail =
        serde_json::from_str(&output).with_context(|| format!("Failed to parse release {tag}"))?;
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
    ensure!(!release.draft, "Release {tag} is unexpectedly a draft");
    ensure!(
        release.prerelease == prerelease,
        "Release {tag} has a mismatched prerelease state"
    );
    ensure!(
        release.assets.is_empty(),
        "Release {tag} has assets; asset-free publication is required"
    );

    let tag_sha = resolve_commit_sha(tag)?;
    ensure!(
        tag_sha == source_sha,
        "Release tag {tag} targets {tag_sha}, expected {source_sha}"
    );
    let target_sha = resolve_commit_sha(&release.target_commitish)?;
    ensure!(
        target_sha == source_sha,
        "Release {tag} target resolves to {target_sha}, expected {source_sha}"
    );
    Ok(())
}

fn release_tag(component: &str, version: &str) -> String {
    format!("{component}@{version}")
}

fn release_title(component: &str, version: &str) -> String {
    format!("{component} {version}")
}

fn release_body(component: &str, version: &str, previous_sha: &str, source_sha: &str) -> String {
    let changes = format!(
        "Changes: [`{}..{}`]({RELEASE_COMPARE_URL}/{previous_sha}..{source_sha})",
        &previous_sha[..7],
        &source_sha[..7]
    );
    match desktop_channel(component) {
        Some(channel) => format!(
            "{changes}\n\n{}",
            desktop_download_sections(channel, version)
        ),
        None => changes,
    }
}

fn desktop_channel(component: &str) -> Option<&str> {
    component.strip_prefix("fluxer-desktop-")
}

fn desktop_download_sections(channel: &str, version: &str) -> String {
    DESKTOP_PLATFORMS
        .iter()
        .map(|platform| desktop_download_section(channel, version, platform))
        .collect::<Vec<_>>()
        .join("\n\n")
}

fn desktop_download_section(channel: &str, version: &str, platform: &DesktopPlatform) -> String {
    let rows = platform
        .downloads
        .iter()
        .map(|download| {
            format!(
                "| {arch} | {label} | {DESKTOP_DOWNLOAD_URL}/{channel}/{slug}/{arch}/{version}/{format} |",
                arch = download.arch,
                label = download.label,
                slug = platform.slug,
                format = download.format,
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        "## {name} (`{slug}`)\n\n| Arch | Format | URL |\n|---|---|---|\n{rows}",
        name = platform.name,
        slug = platform.slug,
    )
}
