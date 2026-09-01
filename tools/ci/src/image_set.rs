// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{
    CommandSpec, append_github_output, env_string, output_text, parse_version_instant, run_command,
};
use crate::release::{RELEASE_REPOSITORY, release_tag, resolve_commit_sha, validate_full_sha};
use anyhow::{Context, Result, bail, ensure};
use clap::{Args, Subcommand};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

const SCHEMA_VERSION: u8 = 1;
const DEFAULT_REGISTRY: &str = "ghcr.io/fluxerapp";
const DEFAULT_MOVING_TAGS: &str = "v1,latest";
const DEFAULT_FROM_TAG: &str = "v1";
const DEFAULT_OUT_DIR: &str = "release-out";
const RELEASE_COMPONENT: &str = "fluxer-release";
const COMPOSE_IMAGE_PREFIX: &str =
    "${FLUXER_REGISTRY:-ghcr.io/${FLUXER_REGISTRY_OWNER:-fluxerapp}}";
const OCI_INDEX_MEDIA_TYPE: &str = "application/vnd.oci.image.index.v1+json";
const DOCKER_MANIFEST_LIST_MEDIA_TYPE: &str =
    "application/vnd.docker.distribution.manifest.list.v2+json";
const REQUIRED_PLATFORMS: &[(&str, &str)] = &[("linux", "amd64"), ("linux", "arm64")];
const ARCH_TAG_SUFFIXES: &[&str] = &["-amd64", "-arm64"];

struct Component {
    image: &'static str,
    services: &'static [&'static str],
}

const COMPONENTS: &[Component] = &[
    Component {
        image: "fluxer-admin",
        services: &["admin"],
    },
    Component {
        image: "fluxer-api",
        services: &["api", "worker"],
    },
    Component {
        image: "fluxer-app-proxy",
        services: &[],
    },
    Component {
        image: "fluxer-app-proxy-self-hosted",
        services: &["app-proxy"],
    },
    Component {
        image: "fluxer-docs",
        services: &[],
    },
    Component {
        image: "fluxer-gateway",
        services: &["gateway"],
    },
    Component {
        image: "fluxer-gifs",
        services: &["gifs", "gifs-shard"],
    },
    Component {
        image: "fluxer-media-proxy",
        services: &["media-proxy"],
    },
    Component {
        image: "fluxer-messages",
        services: &["messages", "messages-shard"],
    },
    Component {
        image: "fluxer-snowflakes",
        services: &["snowflakes", "snowflakes-shard"],
    },
    Component {
        image: "fluxer-static",
        services: &["static-proxy"],
    },
    Component {
        image: "fluxer-unfurl",
        services: &["unfurl", "unfurl-shard"],
    },
    Component {
        image: "fluxer-users",
        services: &["users", "users-shard"],
    },
];

#[derive(Debug, Args, Clone)]
pub struct ImageSetArgs {
    #[command(subcommand)]
    command: ImageSetCommand,
}

#[derive(Debug, Subcommand, Clone)]
#[clap(rename_all = "kebab_case")]
enum ImageSetCommand {
    Resolve(ResolveArgs),
    Verify(VerifyArgs),
    Promote(PromoteArgs),
}

#[derive(Debug, Args, Clone)]
struct ResolveArgs {
    #[arg(long)]
    version: String,
    #[arg(long, default_value = DEFAULT_FROM_TAG)]
    from_tag: String,
    #[arg(long)]
    component_version: Vec<String>,
    #[arg(long, default_value = DEFAULT_REGISTRY)]
    registry: String,
    #[arg(long, default_value = DEFAULT_OUT_DIR)]
    out_dir: PathBuf,
    #[arg(long)]
    allow_unreleased: bool,
    #[arg(long)]
    github_output: bool,
}

#[derive(Debug, Args, Clone)]
struct VerifyArgs {
    #[arg(long)]
    manifest: PathBuf,
    #[arg(long)]
    allow_unreleased: bool,
}

#[derive(Debug, Args, Clone)]
struct PromoteArgs {
    #[arg(long)]
    component: String,
    #[arg(long)]
    build_version: String,
    #[arg(long, default_value = DEFAULT_REGISTRY)]
    registry: String,
    #[arg(long, default_value = DEFAULT_MOVING_TAGS)]
    moving_tags: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub(crate) struct ImageSetManifest {
    pub(crate) schema_version: u8,
    pub(crate) version: String,
    pub(crate) release_tag: String,
    pub(crate) registry: String,
    pub(crate) components: Vec<ImageSetComponent>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub(crate) struct ImageSetComponent {
    pub(crate) component: String,
    pub(crate) image: String,
    pub(crate) tag: String,
    pub(crate) digest: String,
    pub(crate) build_version: String,
    pub(crate) source_sha: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CommitCompare {
    Identical,
    Ahead,
    Behind,
    Diverged,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ResolvedComponent {
    component: String,
    tag: String,
    digest: String,
    source_sha: Option<String>,
}

#[derive(Debug, Deserialize)]
struct RegistryManifest {
    #[serde(rename = "mediaType")]
    media_type: String,
    digest: String,
    #[serde(default)]
    manifests: Vec<RegistryManifestEntry>,
}

#[derive(Debug, Deserialize)]
struct RegistryManifestEntry {
    #[serde(default)]
    platform: Option<RegistryPlatform>,
}

#[derive(Debug, Deserialize)]
struct RegistryPlatform {
    os: String,
    architecture: String,
}

#[derive(Debug, Deserialize)]
struct PackageVersion {
    name: String,
    #[serde(default)]
    metadata: Option<PackageVersionMetadata>,
}

#[derive(Debug, Deserialize)]
struct PackageVersionMetadata {
    #[serde(default)]
    container: Option<PackageContainerMetadata>,
}

#[derive(Debug, Deserialize)]
struct PackageContainerMetadata {
    #[serde(default)]
    tags: Vec<String>,
}

pub fn run(args: ImageSetArgs) -> Result<()> {
    match args.command {
        ImageSetCommand::Resolve(args) => run_resolve(args),
        ImageSetCommand::Verify(args) => run_verify(args),
        ImageSetCommand::Promote(args) => run_promote(args),
    }
}

fn run_resolve(args: ResolveArgs) -> Result<()> {
    parse_version_instant(&args.version)?;
    validate_registry(&args.registry)?;
    let pins = parse_component_versions(&args.component_version)?;
    let from_tag_is_calver = parse_version_instant(&args.from_tag).is_ok();

    let mut resolved = Vec::with_capacity(COMPONENTS.len());
    for component in COMPONENTS {
        let image = format!("{}/{}", args.registry, component.image);
        let tag = match pins.get(component.image) {
            Some(pinned) => pinned.clone(),
            None if from_tag_is_calver => args.from_tag.clone(),
            None => {
                let snapshot =
                    inspect_digest(&format!("{image}:{}", args.from_tag)).with_context(|| {
                        format!(
                            "Component {} has no published {} image",
                            component.image, args.from_tag
                        )
                    })?;
                let tags = package_tags(&args.registry, component.image, &snapshot)?;
                calver_tag_for_digest(&tags).with_context(|| {
                    format!(
                        "Component {} tag {} does not resolve to a single CalVer tag",
                        component.image, args.from_tag
                    )
                })?
            }
        };
        let digest = inspect_digest(&format!("{image}:{tag}")).with_context(|| {
            format!("Component {} has no published {tag} image", component.image)
        })?;
        let source_sha = component_source_sha(component.image, &tag);
        if source_sha.is_none() {
            println!(
                "warning: {} has no {} release in {RELEASE_REPOSITORY}",
                component.image,
                release_tag(component.image, &tag)
            );
        }
        resolved.push(ResolvedComponent {
            component: component.image.to_string(),
            tag,
            digest,
            source_sha,
        });
    }

    let manifest = build_manifest(
        &args.version,
        &args.registry,
        &resolved,
        args.allow_unreleased,
    )?;
    let bundle_commit = resolve_bundle_commit(&resolved)?;
    write_release_files(&args.out_dir, &manifest)?;
    if args.github_output {
        append_github_output(&[("bundle_commit", bundle_commit.as_str())])?;
    }
    Ok(())
}

fn resolve_bundle_commit(resolved: &[ResolvedComponent]) -> Result<String> {
    let unreleased: Vec<&str> = resolved
        .iter()
        .filter(|entry| entry.source_sha.is_none())
        .map(|entry| entry.component.as_str())
        .collect();
    if !unreleased.is_empty() {
        println!(
            "warning: no source commit for {}, so the bundle commit is not proven to be in every image",
            unreleased.join(", ")
        );
    }

    let mut seen = BTreeSet::new();
    let mut candidates: Vec<(&str, &str)> = Vec::new();
    for entry in resolved {
        let Some(sha) = entry.source_sha.as_deref() else {
            continue;
        };
        if seen.insert(sha) {
            candidates.push((entry.component.as_str(), sha));
        }
    }
    ensure!(
        !candidates.is_empty(),
        "Refusing to release: no component records a source commit, so no bundle commit can be proven"
    );

    let (component, commit) = oldest_common_commit(&candidates, compare_commits)?;
    println!("bundle commit {commit} pinned by {component}");
    if let Some(head) = env_string("GITHUB_SHA")
        && head != commit
    {
        println!(
            "warning: workflow ref {head} is not the bundle commit, so the release tag points at {commit}"
        );
    }
    Ok(commit.to_string())
}

fn oldest_common_commit<'a, F>(
    candidates: &[(&'a str, &'a str)],
    mut compare: F,
) -> Result<(&'a str, &'a str)>
where
    F: FnMut(&str, &str) -> Result<CommitCompare>,
{
    let mut oldest = *candidates
        .first()
        .context("The image set records no source commits")?;
    for &(component, commit) in candidates.iter().skip(1) {
        match compare(oldest.1, commit)? {
            CommitCompare::Identical | CommitCompare::Ahead => {}
            CommitCompare::Behind => oldest = (component, commit),
            CommitCompare::Diverged => bail!(
                "Refusing to release: {component} was built from {commit} and {} was built from {}, which are on divergent branches",
                oldest.0,
                oldest.1
            ),
        }
    }
    Ok(oldest)
}

fn compare_commits(base: &str, head: &str) -> Result<CommitCompare> {
    let status = output_text(
        CommandSpec::new("gh")
            .arg("api")
            .arg(format!(
                "repos/{RELEASE_REPOSITORY}/compare/{base}...{head}"
            ))
            .args(["--jq", ".status"]),
    )?;
    parse_commit_compare(&status)
}

fn parse_commit_compare(status: &str) -> Result<CommitCompare> {
    match status.trim() {
        "identical" => Ok(CommitCompare::Identical),
        "ahead" => Ok(CommitCompare::Ahead),
        "behind" => Ok(CommitCompare::Behind),
        "diverged" => Ok(CommitCompare::Diverged),
        other => bail!("Unexpected GitHub compare status {other:?}"),
    }
}

fn run_verify(args: VerifyArgs) -> Result<()> {
    let raw = fs::read_to_string(&args.manifest)
        .with_context(|| format!("Failed to read {}", args.manifest.display()))?;
    let manifest: ImageSetManifest = serde_json::from_str(&raw)
        .with_context(|| format!("Failed to parse {}", args.manifest.display()))?;
    ensure!(
        manifest.schema_version == SCHEMA_VERSION,
        "Image set {} has schema version {}, expected {SCHEMA_VERSION}",
        args.manifest.display(),
        manifest.schema_version
    );

    let recorded: Vec<ResolvedComponent> = manifest
        .components
        .iter()
        .map(|entry| ResolvedComponent {
            component: entry.component.clone(),
            tag: entry.tag.clone(),
            digest: entry.digest.clone(),
            source_sha: entry.source_sha.clone(),
        })
        .collect();
    let rebuilt = build_manifest(
        &manifest.version,
        &manifest.registry,
        &recorded,
        args.allow_unreleased,
    )?;
    ensure!(
        rebuilt == manifest,
        "Image set {} does not match the manifest rebuilt from its own contents",
        args.manifest.display()
    );

    let mut observed = Vec::with_capacity(manifest.components.len());
    for entry in &manifest.components {
        let digest = inspect_digest(&format!("{}:{}", entry.image, entry.tag))?;
        inspect_digest(&format!("{}@{}", entry.image, entry.digest)).with_context(|| {
            format!(
                "Component {} digest {} is no longer published",
                entry.component, entry.digest
            )
        })?;
        observed.push(ResolvedComponent {
            component: entry.component.clone(),
            tag: entry.tag.clone(),
            digest,
            source_sha: entry.source_sha.clone(),
        });
    }
    verify_resolved(&manifest, &observed)?;

    println!(
        "{} verified: {} components pinned by digest",
        manifest.release_tag,
        manifest.components.len()
    );
    Ok(())
}

fn run_promote(args: PromoteArgs) -> Result<()> {
    let component = component(&args.component)?;
    parse_version_instant(&args.build_version)?;
    validate_registry(&args.registry)?;
    let tags = moving_tags(&args.moving_tags)?;

    let image = format!("{}/{}", args.registry, component.image);
    let digest = inspect_digest(&format!("{image}:{}", args.build_version))?;
    if tags.is_empty() {
        println!("No moving tags requested for {image}.");
        return Ok(());
    }

    for other in COMPONENTS
        .iter()
        .filter(|other| other.image != component.image)
    {
        for tag in &tags {
            inspect_digest(&format!("{}/{}:{tag}", args.registry, other.image)).with_context(
                || {
                    format!(
                        "Refusing to advance {tag} for {}: {} has no published {tag} image; run the build-{} workflow first",
                        component.image,
                        other.image,
                        workflow_suffix(other.image)
                    )
                },
            )?;
        }
    }

    run_command(promote_command(&image, &digest, &tags))
}

fn write_release_files(out_dir: &Path, manifest: &ImageSetManifest) -> Result<()> {
    fs::create_dir_all(out_dir)
        .with_context(|| format!("Failed to create {}", out_dir.display()))?;
    let manifest_path = out_dir.join(manifest_filename(&manifest.version));
    let compose_path = out_dir.join(compose_filename(&manifest.version));
    let json = format!(
        "{}\n",
        serde_json::to_string_pretty(manifest).context("Failed to serialise the image set")?
    );
    fs::write(&manifest_path, &json)
        .with_context(|| format!("Failed to write {}", manifest_path.display()))?;
    fs::write(&compose_path, compose_override(manifest))
        .with_context(|| format!("Failed to write {}", compose_path.display()))?;
    println!("{}", manifest_path.display());
    println!("{}", compose_path.display());
    println!("sha256:{}", hex::encode(Sha256::digest(json.as_bytes())));
    Ok(())
}

fn build_manifest(
    version: &str,
    registry: &str,
    resolved: &[ResolvedComponent],
    allow_unreleased: bool,
) -> Result<ImageSetManifest> {
    let version_instant = parse_version_instant(version)?;
    validate_registry(registry)?;

    let mut seen = BTreeSet::new();
    for entry in resolved {
        component(&entry.component)?;
        ensure!(
            seen.insert(entry.component.as_str()),
            "Component {} appears more than once in the release set",
            entry.component
        );
    }
    let missing: Vec<&str> = COMPONENTS
        .iter()
        .map(|component| component.image)
        .filter(|image| !seen.contains(image))
        .collect();
    ensure!(
        missing.is_empty(),
        "Refusing to build the release set: missing components {missing:?}"
    );

    let mut components = Vec::with_capacity(resolved.len());
    for entry in resolved {
        let digest = validate_digest(&entry.digest).with_context(|| {
            format!("Component {} has an unusable image digest", entry.component)
        })?;
        let tag_instant = parse_version_instant(&entry.tag)
            .with_context(|| format!("Component {} has an unusable tag", entry.component))?;
        ensure!(
            tag_instant <= version_instant,
            "Component {} tag {} is newer than release version {version}",
            entry.component,
            entry.tag
        );
        let source_sha = match entry.source_sha.as_deref() {
            Some(sha) => Some(validate_full_sha(
                &format!("{} source SHA", entry.component),
                sha,
            )?),
            None => None,
        };
        ensure!(
            allow_unreleased || source_sha.is_some(),
            "Component {} has no {} release in {RELEASE_REPOSITORY}; rebuild it or pass --allow-unreleased",
            entry.component,
            release_tag(&entry.component, &entry.tag)
        );
        components.push(ImageSetComponent {
            component: entry.component.clone(),
            image: format!("{registry}/{}", entry.component),
            tag: entry.tag.clone(),
            digest,
            build_version: entry.tag.clone(),
            source_sha,
        });
    }
    components.sort_by(|left, right| left.component.cmp(&right.component));

    Ok(ImageSetManifest {
        schema_version: SCHEMA_VERSION,
        version: version.to_string(),
        release_tag: release_tag(RELEASE_COMPONENT, version),
        registry: registry.to_string(),
        components,
    })
}

fn compose_override(manifest: &ImageSetManifest) -> String {
    let mut services: BTreeMap<&str, String> = BTreeMap::new();
    for entry in &manifest.components {
        let Ok(component) = component(&entry.component) else {
            continue;
        };
        for service in component.services {
            services.insert(
                service,
                format!(
                    "{COMPOSE_IMAGE_PREFIX}/{}@${{{}:-{}}}",
                    entry.component,
                    digest_env_name(&entry.component),
                    entry.digest
                ),
            );
        }
    }

    let mut rendered = String::new();
    rendered.push_str(&format!("# {} image set\n", manifest.release_tag));
    rendered.push_str(&format!(
        "# docker compose -f docker-compose.yml -f {} up -d\n",
        compose_filename(&manifest.version)
    ));
    rendered.push_str("services:\n");
    for (service, image) in &services {
        rendered.push_str(&format!("  {service}:\n    image: {image}\n"));
    }
    rendered
}

fn verify_resolved(manifest: &ImageSetManifest, resolved: &[ResolvedComponent]) -> Result<()> {
    let observed: BTreeMap<&str, &ResolvedComponent> = resolved
        .iter()
        .map(|entry| (entry.component.as_str(), entry))
        .collect();
    for entry in &manifest.components {
        let found = observed
            .get(entry.component.as_str())
            .with_context(|| format!("Component {} was not resolved", entry.component))?;
        ensure!(
            found.tag == entry.tag,
            "Component {} records tag {} but {} resolved {}",
            entry.component,
            entry.tag,
            entry.image,
            found.tag
        );
        ensure!(
            found.digest == entry.digest,
            "Component {} drifted: {}:{} records {} but now resolves to {}",
            entry.component,
            entry.image,
            entry.tag,
            entry.digest,
            found.digest
        );
    }
    Ok(())
}

fn promote_command(image: &str, digest: &str, tags: &[String]) -> CommandSpec {
    let mut spec = CommandSpec::new("docker").args(["buildx", "imagetools", "create"]);
    for tag in tags {
        spec = spec.arg("-t").arg(format!("{image}:{tag}"));
    }
    spec.arg(format!("{image}@{digest}"))
}

fn component(image: &str) -> Result<&'static Component> {
    COMPONENTS
        .iter()
        .find(|candidate| candidate.image == image)
        .with_context(|| format!("Unknown release component {image:?}"))
}

fn workflow_suffix(image: &str) -> &str {
    image.strip_prefix("fluxer-").unwrap_or(image)
}

fn digest_env_name(component: &str) -> String {
    format!(
        "{}_IMAGE_DIGEST",
        component.to_ascii_uppercase().replace('-', "_")
    )
}

fn manifest_filename(version: &str) -> String {
    format!("{RELEASE_COMPONENT}-{version}.json")
}

fn compose_filename(version: &str) -> String {
    format!("{RELEASE_COMPONENT}-{version}.yml")
}

fn validate_digest(digest: &str) -> Result<String> {
    let value = digest.trim();
    let hex = value
        .strip_prefix("sha256:")
        .with_context(|| format!("Invalid image digest {value:?}: expected sha256:<64 hex>"))?;
    ensure!(
        hex.len() == 64
            && hex
                .bytes()
                .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)),
        "Invalid image digest {value:?}: expected sha256:<64 hex>"
    );
    Ok(value.to_string())
}

fn validate_registry(registry: &str) -> Result<()> {
    ensure!(!registry.is_empty(), "Registry must not be empty");
    ensure!(
        !registry.chars().any(char::is_whitespace),
        "Invalid registry {registry:?}: whitespace is not allowed"
    );
    ensure!(
        !registry.ends_with('/'),
        "Invalid registry {registry:?}: a trailing slash is not allowed"
    );
    let namespace = registry.rsplit('/').next().unwrap_or_default();
    ensure!(
        !namespace.is_empty() && !namespace.contains(':'),
        "Invalid registry {registry:?}: expected a namespace, not an image reference"
    );
    Ok(())
}

fn moving_tags(raw: &str) -> Result<Vec<String>> {
    let mut tags = Vec::new();
    for entry in raw.split(',') {
        let tag = entry.trim();
        if tag.is_empty() {
            continue;
        }
        ensure!(
            tag.bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'_' | b'-')),
            "Invalid moving tag {tag:?}: expected letters, digits, dots, underscores and hyphens"
        );
        tags.push(tag.to_string());
    }
    Ok(tags)
}

fn calver_tag_for_digest(tags: &[String]) -> Result<String> {
    let mut calver: Vec<&str> = tags
        .iter()
        .map(String::as_str)
        .filter(|tag| !ARCH_TAG_SUFFIXES.iter().any(|suffix| tag.ends_with(suffix)))
        .filter(|tag| parse_version_instant(tag).is_ok())
        .collect();
    calver.sort_unstable();
    calver.dedup();
    match calver.as_slice() {
        [tag] => Ok((*tag).to_string()),
        found => bail!(
            "Expected exactly one CalVer tag but found {}: {tags:?}",
            found.len()
        ),
    }
}

fn parse_component_versions(entries: &[String]) -> Result<BTreeMap<String, String>> {
    let mut pins = BTreeMap::new();
    for raw in entries {
        let entry = raw.trim();
        let (image, version) = entry.split_once('=').with_context(|| {
            format!("Invalid component version {entry:?}: expected <image>=<version>")
        })?;
        let image = image.trim();
        let version = version.trim();
        component(image)?;
        parse_version_instant(version)
            .with_context(|| format!("Component {image} is pinned to an unusable version"))?;
        ensure!(
            pins.insert(image.to_string(), version.to_string())
                .is_none(),
            "Component {image} is pinned more than once"
        );
    }
    Ok(pins)
}

fn inspect_digest(reference: &str) -> Result<String> {
    let output = output_text(
        CommandSpec::new("docker")
            .args(["buildx", "imagetools", "inspect", reference])
            .args(["--format", "{{json .Manifest}}"]),
    )?;
    let manifest: RegistryManifest = serde_json::from_str(&output)
        .with_context(|| format!("Failed to parse the registry manifest for {reference}"))?;
    ensure!(
        manifest.media_type == OCI_INDEX_MEDIA_TYPE
            || manifest.media_type == DOCKER_MANIFEST_LIST_MEDIA_TYPE,
        "{reference} is {} and not a multi-architecture index",
        manifest.media_type
    );
    let platforms: BTreeSet<(&str, &str)> = manifest
        .manifests
        .iter()
        .filter_map(|entry| entry.platform.as_ref())
        .map(|platform| (platform.os.as_str(), platform.architecture.as_str()))
        .collect();
    for (os, architecture) in REQUIRED_PLATFORMS {
        ensure!(
            platforms.contains(&(*os, *architecture)),
            "{reference} does not publish the {os}/{architecture} platform"
        );
    }
    validate_digest(&manifest.digest)
}

fn package_tags(registry: &str, image: &str, digest: &str) -> Result<Vec<String>> {
    let owner = registry
        .rsplit('/')
        .next()
        .filter(|owner| !owner.is_empty())
        .with_context(|| format!("Cannot derive a package owner from registry {registry:?}"))?;
    let output = output_text(
        CommandSpec::new("gh")
            .args(["api", "--paginate", "--slurp"])
            .arg(format!(
                "/orgs/{owner}/packages/container/{image}/versions?per_page=100"
            )),
    )?;
    let pages: Vec<Vec<PackageVersion>> = serde_json::from_str(&output)
        .with_context(|| format!("Failed to parse the package versions of {image}"))?;
    pages
        .into_iter()
        .flatten()
        .find(|version| version.name == digest)
        .map(|version| {
            version
                .metadata
                .and_then(|metadata| metadata.container)
                .map(|container| container.tags)
                .unwrap_or_default()
        })
        .with_context(|| format!("No {image} package version matches {digest}"))
}

fn component_source_sha(image: &str, version: &str) -> Option<String> {
    resolve_commit_sha(&release_tag(image, version)).ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cmp::Ordering;

    const COMPOSE_IMAGE_TAG: &str = "${FLUXER_IMAGE_TAG:-v1}";
    const RELEASE_VERSION: &str = "2026.901.120000";
    const COMPONENT_TAG: &str = "2026.830.191141";

    fn resolved_set() -> Vec<ResolvedComponent> {
        COMPONENTS
            .iter()
            .enumerate()
            .map(|(index, component)| ResolvedComponent {
                component: component.image.to_string(),
                tag: COMPONENT_TAG.to_string(),
                digest: format!("sha256:{}", format!("{index:02x}").repeat(32)),
                source_sha: Some("a".repeat(40)),
            })
            .collect()
    }

    fn manifest() -> ImageSetManifest {
        build_manifest(RELEASE_VERSION, DEFAULT_REGISTRY, &resolved_set(), true).unwrap()
    }

    fn compose_component(reference: &str) -> Option<&str> {
        reference
            .strip_prefix(COMPOSE_IMAGE_PREFIX)?
            .strip_prefix('/')?
            .strip_suffix(COMPOSE_IMAGE_TAG)?
            .strip_suffix(':')
    }

    #[test]
    fn components_cover_every_self_hosting_service() {
        let compose = include_str!("../../../deploy/self-hosting/docker-compose.yml");
        let expected: BTreeSet<(&str, &str)> = COMPONENTS
            .iter()
            .flat_map(|component| {
                component
                    .services
                    .iter()
                    .map(move |service| (component.image, *service))
            })
            .collect();

        let mut observed: BTreeSet<(&str, &str)> = BTreeSet::new();
        let mut in_services = false;
        let mut service = None;
        for line in compose.lines() {
            if !line.is_empty() && !line.starts_with(' ') {
                in_services = line == "services:";
                service = None;
                continue;
            }
            if !in_services {
                continue;
            }
            if let Some(name) = line
                .strip_prefix("  ")
                .and_then(|rest| rest.strip_suffix(':'))
                && !name.starts_with(' ')
            {
                service = Some(name);
                continue;
            }
            let trimmed = line.trim();
            let Some(reference) = trimmed.strip_prefix("image: ") else {
                continue;
            };
            if !reference.starts_with("${FLUXER_REGISTRY") {
                continue;
            }
            let image = compose_component(reference)
                .unwrap_or_else(|| panic!("Unexpected Fluxer image line: {trimmed}"));
            let service = service.expect("a Fluxer image line must follow a service header");
            observed.insert((image, service));
        }

        assert_eq!(observed, expected);
    }

    #[test]
    fn components_are_sorted_and_unique() {
        let images: Vec<&str> = COMPONENTS.iter().map(|component| component.image).collect();
        let mut sorted = images.clone();
        sorted.sort_unstable();
        sorted.dedup();
        assert_eq!(images, sorted);

        let services: Vec<&str> = COMPONENTS
            .iter()
            .flat_map(|component| component.services.iter().copied())
            .collect();
        let unique: BTreeSet<&str> = services.iter().copied().collect();
        assert_eq!(services.len(), unique.len());
        assert_eq!(services.len(), 17);
    }

    #[test]
    fn every_component_has_a_build_workflow() {
        let directory = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../.github/workflows");
        let mut workflows = String::new();
        for entry in fs::read_dir(&directory).expect("the workflow directory should be readable") {
            let path = entry.expect("the workflow entry should be readable").path();
            if path
                .extension()
                .is_some_and(|extension| extension == "yaml")
            {
                workflows
                    .push_str(&fs::read_to_string(&path).expect("the workflow should be readable"));
            }
        }
        for component in COMPONENTS {
            assert!(
                workflows.contains(component.image),
                "{} has no build workflow",
                component.image
            );
        }
    }

    #[test]
    fn manifest_requires_every_component() {
        let mut resolved = resolved_set();
        resolved.retain(|entry| entry.component != "fluxer-api");
        let error = build_manifest(RELEASE_VERSION, DEFAULT_REGISTRY, &resolved, true)
            .unwrap_err()
            .to_string();
        assert!(error.contains("missing components"), "{error}");
        assert!(error.contains("fluxer-api"), "{error}");
    }

    #[test]
    fn manifest_rejects_unknown_and_duplicate_components() {
        let mut unknown = resolved_set();
        unknown[0].component = "fluxer-unknown".to_string();
        let error = build_manifest(RELEASE_VERSION, DEFAULT_REGISTRY, &unknown, true)
            .unwrap_err()
            .to_string();
        assert!(error.contains("Unknown release component"), "{error}");

        let mut duplicate = resolved_set();
        duplicate.push(duplicate[0].clone());
        let error = build_manifest(RELEASE_VERSION, DEFAULT_REGISTRY, &duplicate, true)
            .unwrap_err()
            .to_string();
        assert!(error.contains("appears more than once"), "{error}");
    }

    #[test]
    fn manifest_rejects_bad_digest() {
        let mut resolved = resolved_set();
        resolved[0].digest = "sha256:notahexdigest".to_string();
        let error = format!(
            "{:?}",
            build_manifest(RELEASE_VERSION, DEFAULT_REGISTRY, &resolved, true).unwrap_err()
        );
        assert!(error.contains("fluxer-admin"), "{error}");
        assert!(error.contains("Invalid image digest"), "{error}");
    }

    #[test]
    fn manifest_rejects_component_tag_newer_than_release() {
        let mut resolved = resolved_set();
        resolved[0].tag = "2026.902.0".to_string();
        let error = build_manifest(RELEASE_VERSION, DEFAULT_REGISTRY, &resolved, true)
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("is newer than release version 2026.901.120000"),
            "{error}"
        );
    }

    #[test]
    fn manifest_requires_source_sha_unless_allowed() {
        let mut resolved = resolved_set();
        resolved[0].source_sha = None;
        let error = build_manifest(RELEASE_VERSION, DEFAULT_REGISTRY, &resolved, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("fluxer-admin@2026.830.191141"), "{error}");
        assert!(error.contains("--allow-unreleased"), "{error}");

        let manifest = build_manifest(RELEASE_VERSION, DEFAULT_REGISTRY, &resolved, true).unwrap();
        assert_eq!(manifest.components[0].source_sha, None);
        assert_eq!(manifest.release_tag, "fluxer-release@2026.901.120000");
        assert_eq!(manifest.components.len(), COMPONENTS.len());
    }

    #[test]
    fn compose_override_pins_every_service_by_digest() {
        let manifest = manifest();
        let rendered = compose_override(&manifest);
        assert_eq!(
            rendered
                .lines()
                .filter(|line| line.starts_with("    image: "))
                .count(),
            17
        );

        let api = manifest
            .components
            .iter()
            .find(|entry| entry.component == "fluxer-api")
            .unwrap();
        for service in ["api", "worker"] {
            assert!(
                rendered.contains(&format!(
                    "  {service}:\n    image: {COMPOSE_IMAGE_PREFIX}/fluxer-api@${{FLUXER_API_IMAGE_DIGEST:-{}}}\n",
                    api.digest
                )),
                "{rendered}"
            );
        }

        let app_proxy = manifest
            .components
            .iter()
            .find(|entry| entry.component == "fluxer-app-proxy-self-hosted")
            .unwrap();
        assert!(
            rendered.contains(&format!(
                "  app-proxy:\n    image: {COMPOSE_IMAGE_PREFIX}/fluxer-app-proxy-self-hosted@${{FLUXER_APP_PROXY_SELF_HOSTED_IMAGE_DIGEST:-{}}}\n",
                app_proxy.digest
            )),
            "{rendered}"
        );
        assert!(!rendered.contains("fluxer-docs"), "{rendered}");
        assert!(
            rendered.starts_with(
                "# fluxer-release@2026.901.120000 image set\n# docker compose -f docker-compose.yml -f fluxer-release-2026.901.120000.yml up -d\nservices:\n"
            ),
            "{rendered}"
        );
    }

    #[test]
    fn compose_override_is_sorted_and_stable() {
        let manifest = manifest();
        let rendered = compose_override(&manifest);
        assert_eq!(rendered, compose_override(&manifest));

        let services: Vec<&str> = rendered
            .lines()
            .filter_map(|line| line.strip_prefix("  ")?.strip_suffix(':'))
            .collect();
        let mut sorted = services.clone();
        sorted.sort_unstable();
        assert_eq!(services, sorted);
        assert_eq!(services.len(), 17);
    }

    #[test]
    fn moving_tags_parses_and_rejects_junk() {
        assert_eq!(moving_tags("").unwrap(), Vec::<String>::new());
        assert_eq!(moving_tags(" , ").unwrap(), Vec::<String>::new());
        assert_eq!(moving_tags(" v1, latest ").unwrap(), ["v1", "latest"]);
        assert_eq!(moving_tags(DEFAULT_MOVING_TAGS).unwrap(), ["v1", "latest"]);
        assert!(moving_tags("v1@sha256:abc").is_err());
        assert!(moving_tags("v1 latest").is_err());
    }

    #[test]
    fn promote_command_targets_the_digest() {
        let digest = format!("sha256:{}", "0".repeat(64));
        let spec = promote_command(
            "ghcr.io/fluxerapp/fluxer-api",
            &digest,
            &["v1".to_string(), "latest".to_string()],
        );
        assert_eq!(spec.program, "docker");
        assert_eq!(
            spec.args,
            [
                "buildx",
                "imagetools",
                "create",
                "-t",
                "ghcr.io/fluxerapp/fluxer-api:v1",
                "-t",
                "ghcr.io/fluxerapp/fluxer-api:latest",
                &format!("ghcr.io/fluxerapp/fluxer-api@{digest}"),
            ]
        );
    }

    #[test]
    fn calver_tag_for_digest_picks_the_single_calver_tag() {
        let tags = ["2026.630.20736", "latest", "v1"].map(str::to_string);
        assert_eq!(calver_tag_for_digest(&tags).unwrap(), "2026.630.20736");

        let arch = [
            "2026.630.20736",
            "2026.630.20736-amd64",
            "2026.630.20736-arm64",
        ]
        .map(str::to_string);
        assert_eq!(calver_tag_for_digest(&arch).unwrap(), "2026.630.20736");

        let none = ["v1", "latest"].map(str::to_string);
        assert!(calver_tag_for_digest(&none).is_err());

        let many = ["2026.630.20736", "2026.830.191141"].map(str::to_string);
        assert!(calver_tag_for_digest(&many).is_err());
    }

    #[test]
    fn parse_component_versions_rejects_junk() {
        let pins = parse_component_versions(&["fluxer-api=2026.830.191141".to_string()]).unwrap();
        assert_eq!(pins["fluxer-api"], "2026.830.191141");

        assert!(parse_component_versions(&["fluxer-api".to_string()]).is_err());
        assert!(parse_component_versions(&["fluxer-nope=2026.830.191141".to_string()]).is_err());
        assert!(parse_component_versions(&["fluxer-api=v1".to_string()]).is_err());
        assert!(
            parse_component_versions(&[
                "fluxer-api=2026.830.191141".to_string(),
                "fluxer-api=2026.830.191142".to_string(),
            ])
            .is_err()
        );
    }

    #[test]
    fn verify_resolved_detects_digest_drift() {
        let manifest = manifest();
        let mut resolved = resolved_set();
        verify_resolved(&manifest, &resolved).unwrap();

        resolved[1].digest = format!("sha256:{}", "f".repeat(64));
        let error = verify_resolved(&manifest, &resolved)
            .unwrap_err()
            .to_string();
        assert!(error.contains("fluxer-api"), "{error}");
        assert!(error.contains("drifted"), "{error}");

        let mut retagged = resolved_set();
        retagged[1].tag = "2026.830.191140".to_string();
        let error = verify_resolved(&manifest, &retagged)
            .unwrap_err()
            .to_string();
        assert!(error.contains("records tag"), "{error}");
    }

    #[test]
    fn digest_env_name_matches_component() {
        assert_eq!(digest_env_name("fluxer-api"), "FLUXER_API_IMAGE_DIGEST");
        assert_eq!(
            digest_env_name("fluxer-app-proxy-self-hosted"),
            "FLUXER_APP_PROXY_SELF_HOSTED_IMAGE_DIGEST"
        );
    }

    #[test]
    fn validate_registry_rejects_image_references() {
        validate_registry(DEFAULT_REGISTRY).unwrap();
        validate_registry("registry.example.com:5000/fluxer").unwrap();
        assert!(validate_registry("").is_err());
        assert!(validate_registry("ghcr.io/fluxerapp/").is_err());
        assert!(validate_registry("ghcr.io/fluxerapp/fluxer-api:v1").is_err());
        assert!(validate_registry("ghcr.io/ fluxerapp").is_err());
    }

    fn linear_compare<'a>(
        history: &'a [&'a str; 3],
    ) -> impl FnMut(&str, &str) -> Result<CommitCompare> + 'a {
        move |base: &str, head: &str| {
            let position = |commit: &str| {
                history
                    .iter()
                    .position(|entry| *entry == commit)
                    .with_context(|| format!("{commit} is not in the test history"))
            };
            Ok(match position(base)?.cmp(&position(head)?) {
                Ordering::Equal => CommitCompare::Identical,
                Ordering::Less => CommitCompare::Ahead,
                Ordering::Greater => CommitCompare::Behind,
            })
        }
    }

    #[test]
    fn oldest_common_commit_picks_the_commit_every_image_contains() {
        let history = ["a".repeat(40), "b".repeat(40), "c".repeat(40)];
        let history = [
            history[0].as_str(),
            history[1].as_str(),
            history[2].as_str(),
        ];
        let candidates = [
            ("fluxer-api", history[2]),
            ("fluxer-gateway", history[0]),
            ("fluxer-users", history[1]),
        ];
        assert_eq!(
            oldest_common_commit(&candidates, linear_compare(&history)).unwrap(),
            ("fluxer-gateway", history[0])
        );

        let single = [("fluxer-api", history[2])];
        assert_eq!(
            oldest_common_commit(&single, linear_compare(&history)).unwrap(),
            ("fluxer-api", history[2])
        );
    }

    #[test]
    fn oldest_common_commit_refuses_divergent_component_builds() {
        let candidates = [("fluxer-api", "aa"), ("fluxer-gateway", "bb")];
        let error =
            oldest_common_commit(&candidates, |_: &str, _: &str| Ok(CommitCompare::Diverged))
                .unwrap_err()
                .to_string();
        assert!(error.contains("fluxer-api"), "{error}");
        assert!(error.contains("fluxer-gateway"), "{error}");
        assert!(error.contains("divergent branches"), "{error}");
    }

    #[test]
    fn parse_commit_compare_maps_every_github_status() {
        assert_eq!(
            parse_commit_compare("identical").unwrap(),
            CommitCompare::Identical
        );
        assert_eq!(
            parse_commit_compare(" ahead ").unwrap(),
            CommitCompare::Ahead
        );
        assert_eq!(
            parse_commit_compare("behind").unwrap(),
            CommitCompare::Behind
        );
        assert_eq!(
            parse_commit_compare("diverged").unwrap(),
            CommitCompare::Diverged
        );
        assert!(parse_commit_compare("").is_err());
        assert!(parse_commit_compare("null").is_err());
    }

    #[test]
    fn resolve_bundle_commit_refuses_a_set_with_no_source_commits() {
        let mut resolved = resolved_set();
        for entry in &mut resolved {
            entry.source_sha = None;
        }
        let error = resolve_bundle_commit(&resolved).unwrap_err().to_string();
        assert!(error.contains("no bundle commit can be proven"), "{error}");
    }

    #[test]
    fn release_workflow_tags_the_bundle_commit_and_marks_it_latest() {
        let workflow = include_str!("../../../.github/workflows/release-image-set.yaml");
        for entry in [
            "--github-output",
            "BUNDLE_COMMIT: ${{ steps.resolve.outputs.bundle_commit }}",
            "--target \"${BUNDLE_COMMIT}\"",
            "--latest=true",
        ] {
            assert!(
                workflow.contains(entry),
                "release-image-set.yaml must carry {entry}"
            );
        }
        assert!(
            !workflow.contains("${GITHUB_SHA}"),
            "the release tag must be cut at the bundle commit, not at the workflow ref"
        );
    }

    #[test]
    fn release_filenames_are_stable() {
        assert_eq!(
            manifest_filename(RELEASE_VERSION),
            "fluxer-release-2026.901.120000.json"
        );
        assert_eq!(
            compose_filename(RELEASE_VERSION),
            "fluxer-release-2026.901.120000.yml"
        );
    }
}
