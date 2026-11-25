// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, output_text, parse_version_instant, run_command};
use anyhow::{Context, Result, ensure};
use clap::{Args, Subcommand};
use serde::Deserialize;
use std::collections::BTreeSet;

const DEFAULT_REGISTRY: &str = "ghcr.io/fluxerapp";
const DEFAULT_MOVING_TAGS: &str = "v1,latest";
const OCI_INDEX_MEDIA_TYPE: &str = "application/vnd.oci.image.index.v1+json";
const DOCKER_MANIFEST_LIST_MEDIA_TYPE: &str =
    "application/vnd.docker.distribution.manifest.list.v2+json";
const REQUIRED_PLATFORMS: &[(&str, &str)] = &[("linux", "amd64"), ("linux", "arm64")];

const COMPONENTS: &[&str] = &[
    "fluxer-admin",
    "fluxer-api",
    "fluxer-app-proxy",
    "fluxer-docs",
    "fluxer-gateway",
    "fluxer-gifs",
    "fluxer-media-proxy",
    "fluxer-messages",
    "fluxer-push",
    "fluxer-snowflakes",
    "fluxer-static",
    "fluxer-unfurl",
    "fluxer-users",
];

#[derive(Debug, Args, Clone)]
pub struct ImageSetArgs {
    #[command(subcommand)]
    command: ImageSetCommand,
}

#[derive(Debug, Subcommand, Clone)]
#[clap(rename_all = "kebab_case")]
enum ImageSetCommand {
    Promote(PromoteArgs),
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

pub fn run(args: ImageSetArgs) -> Result<()> {
    match args.command {
        ImageSetCommand::Promote(args) => run_promote(args),
    }
}

fn run_promote(args: PromoteArgs) -> Result<()> {
    let component = component(&args.component)?;
    parse_version_instant(&args.build_version)?;
    validate_registry(&args.registry)?;
    let tags = moving_tags(&args.moving_tags)?;

    let image = format!("{}/{component}", args.registry);
    let digest = inspect_digest(&format!("{image}:{}", args.build_version))?;
    if tags.is_empty() {
        println!("No moving tags requested for {image}.");
        return Ok(());
    }

    for warning in unpublished_moving_tags(&args.registry, component, &tags, |reference| {
        inspect_digest(reference).is_ok()
    }) {
        println!("warning: {warning}");
    }

    run_command(promote_command(&image, &digest, &tags))
}

fn unpublished_moving_tags<F>(
    registry: &str,
    component: &str,
    tags: &[String],
    mut published: F,
) -> Vec<String>
where
    F: FnMut(&str) -> bool,
{
    let mut warnings = Vec::new();
    for other in COMPONENTS.iter().filter(|other| **other != component) {
        for tag in tags {
            if !published(&format!("{registry}/{other}:{tag}")) {
                warnings.push(format!(
                    "{other} has no published {tag} image, so {tag} stays an incomplete set until the build-{} workflow runs",
                    workflow_suffix(other)
                ));
            }
        }
    }
    warnings
}

fn promote_command(image: &str, digest: &str, tags: &[String]) -> CommandSpec {
    let mut spec = CommandSpec::new("docker").args(["buildx", "imagetools", "create"]);
    for tag in tags {
        spec = spec.arg("-t").arg(format!("{image}:{tag}"));
    }
    spec.arg(format!("{image}@{digest}"))
}

fn component(image: &str) -> Result<&'static str> {
    COMPONENTS
        .iter()
        .copied()
        .find(|candidate| *candidate == image)
        .with_context(|| format!("Unknown release component {image:?}"))
}

fn workflow_suffix(image: &str) -> &str {
    image.strip_prefix("fluxer-").unwrap_or(image)
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
