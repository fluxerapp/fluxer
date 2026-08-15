// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::manifest::{DEV_PROXY_PORT, LOCAL_APP_URL, LOOPBACK_HOST};
use crate::paths::{DEV_STATE_DIR, ROOT, which};
use crate::proc::wait_tcp;
use anyhow::{Context, Result, bail};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use url::Url;

const LOCAL_ENV_START: &str = "# BEGIN fluxer-dev public URL";
const LOCAL_ENV_END: &str = "# END fluxer-dev public URL";
const DEFAULT_TOKEN_FILE_NAME: &str = "cloudflare-tunnel-token";
const DEFAULT_PUBLIC_URL_FILE_NAME: &str = "cloudflare-tunnel-public-url";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PublicUrl {
    pub base_url: String,
    pub host: String,
    pub scheme: String,
    pub public_port: u16,
    pub websocket_scheme: String,
}

pub fn parse_public_url(raw: &str) -> Result<PublicUrl> {
    let url = Url::parse(raw).with_context(|| format!("invalid public URL: {raw}"))?;
    let scheme = url.scheme().to_owned();
    if scheme != "http" && scheme != "https" {
        bail!("Public URL must use http or https: {raw}");
    }
    let host = url
        .host_str()
        .ok_or_else(|| anyhow::anyhow!("Public URL must include a host: {raw}"))?
        .to_owned();
    let path = url.path();
    if path != "/" && !path.is_empty() {
        bail!("Public URL must not include a path: {raw}");
    }
    if url.query().is_some() || url.fragment().is_some() {
        bail!("Public URL must not include a query string or fragment: {raw}");
    }
    let public_port = url
        .port_or_known_default()
        .ok_or_else(|| anyhow::anyhow!("Public URL has no known port: {raw}"))?;
    let mut normalized = url;
    normalized.set_path("");
    normalized.set_query(None);
    normalized.set_fragment(None);
    Ok(PublicUrl {
        base_url: normalized.as_str().trim_end_matches('/').to_owned(),
        host,
        scheme: scheme.clone(),
        public_port,
        websocket_scheme: if scheme == "https" { "wss" } else { "ws" }.to_owned(),
    })
}

pub fn public_url_env(public_url: &str) -> Result<Vec<(String, String)>> {
    let parsed = parse_public_url(public_url)?;
    let base = parsed.base_url.as_str();
    let gateway_base = format!("{}://{}", parsed.websocket_scheme, parsed.host);
    let localhost_origins = "http://localhost,http://localhost:8088";
    Ok(vec![
        ("FLUXER_BASE_DOMAIN".to_owned(), parsed.host.clone()),
        ("FLUXER_PUBLIC_SCHEME".to_owned(), parsed.scheme.clone()),
        (
            "FLUXER_PUBLIC_PORT".to_owned(),
            parsed.public_port.to_string(),
        ),
        ("FLUXER_STATIC_CDN_DOMAIN".to_owned(), parsed.host.clone()),
        ("FLUXER_PUBLIC_URL".to_owned(), base.to_owned()),
        ("FLUXER_API_ENDPOINT".to_owned(), format!("{base}/api")),
        (
            "FLUXER_API_CLIENT_ENDPOINT".to_owned(),
            format!("{base}/api"),
        ),
        ("FLUXER_APP_ENDPOINT".to_owned(), base.to_owned()),
        (
            "FLUXER_GATEWAY_ENDPOINT".to_owned(),
            format!("{gateway_base}/gateway"),
        ),
        ("FLUXER_MEDIA_ENDPOINT".to_owned(), format!("{base}/media")),
        ("FLUXER_S3_PUBLIC_ENDPOINT".to_owned(), base.to_owned()),
        ("FLUXER_S3_FORCE_PATH_STYLE".to_owned(), "true".to_owned()),
        ("FLUXER_STATIC_CDN_ENDPOINT".to_owned(), base.to_owned()),
        ("FLUXER_ADMIN_ENDPOINT".to_owned(), format!("{base}/admin")),
        (
            "FLUXER_MARKETING_ENDPOINT".to_owned(),
            std::env::var("FLUXER_MARKETING_ENDPOINT")
                .unwrap_or_else(|_| "https://fluxer.app".to_owned()),
        ),
        (
            "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT".to_owned(),
            format!("{base}/media"),
        ),
        (
            "FLUXER_LIVEKIT_URL".to_owned(),
            format!("{gateway_base}/livekit"),
        ),
        (
            "FLUXER_LIVEKIT_WEBHOOK_URL".to_owned(),
            format!("{base}/api/webhooks/livekit"),
        ),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT".to_owned(),
            format!("{base}/media"),
        ),
        (
            "FLUXER_GATEWAY_MEDIA_PROXY_ENDPOINT".to_owned(),
            format!("{base}/media"),
        ),
        (
            "FLUXER_GATEWAY_STATIC_CDN_ENDPOINT".to_owned(),
            base.to_owned(),
        ),
        (
            "FLUXER_ADMIN_OAUTH_REDIRECT_URI".to_owned(),
            format!("{base}/admin/oauth2_callback"),
        ),
        ("FLUXER_PASSKEY_RP_ID".to_owned(), parsed.host.clone()),
        (
            "FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS".to_owned(),
            format!("{localhost_origins},{base}"),
        ),
        (
            "PUBLIC_BOOTSTRAP_API_PUBLIC_ENDPOINT".to_owned(),
            format!("{base}/api"),
        ),
    ])
}

pub fn apply_public_url_env(public_url: &str) -> Result<()> {
    for (key, value) in public_url_env(public_url)? {
        unsafe {
            std::env::set_var(key, value);
        }
    }
    Ok(())
}

pub fn public_url_env_text(public_url: &str) -> Result<String> {
    let mut text = String::new();
    text.push_str(LOCAL_ENV_START);
    text.push('\n');
    text.push_str("# Generated by `fluxer-dev tunnel configure`; safe to delete.\n");
    for (key, value) in public_url_env(public_url)? {
        text.push_str(&key);
        text.push('=');
        text.push_str(&value);
        text.push('\n');
    }
    text.push_str(LOCAL_ENV_END);
    text.push('\n');
    Ok(text)
}

pub fn write_public_url_local_env(path: &Path, public_url: &str) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    let generated = public_url_env_text(public_url)?;
    let existing = fs::read_to_string(path).unwrap_or_default();
    let next = replace_marked_block(&existing, &generated);
    fs::write(path, next).with_context(|| format!("failed to write {}", path.display()))?;
    println!("Wrote public URL overrides to {}", path.display());
    Ok(())
}

pub fn default_token_file() -> PathBuf {
    DEV_STATE_DIR.join(DEFAULT_TOKEN_FILE_NAME)
}

pub fn default_public_url_file() -> PathBuf {
    DEV_STATE_DIR.join(DEFAULT_PUBLIC_URL_FILE_NAME)
}

pub fn write_cloudflare_public_url_file(public_url: &str) -> Result<PathBuf> {
    fs::create_dir_all(DEV_STATE_DIR.as_path())
        .with_context(|| format!("failed to create {}", DEV_STATE_DIR.display()))?;
    let parsed = parse_public_url(public_url)?;
    let path = default_public_url_file();
    fs::write(&path, format!("{}\n", parsed.base_url))
        .with_context(|| format!("failed to write {}", path.display()))?;
    println!("Saved Cloudflare tunnel public URL to {}", path.display());
    Ok(path)
}

pub fn write_cloudflare_token_file(token: &str) -> Result<PathBuf> {
    fs::create_dir_all(DEV_STATE_DIR.as_path())
        .with_context(|| format!("failed to create {}", DEV_STATE_DIR.display()))?;
    let path = default_token_file();
    fs::write(&path, format!("{}\n", token.trim()))
        .with_context(|| format!("failed to write {}", path.display()))?;
    set_private_file_mode(&path)?;
    println!("Saved Cloudflare tunnel token to {}", path.display());
    Ok(path)
}

pub fn resolve_cloudflare_public_url(public_url_arg: Option<&str>) -> Result<String> {
    if let Some(public_url) = public_url_arg {
        return Ok(parse_public_url(public_url)?.base_url);
    }
    for key in [
        "FLUXER_CLOUDFLARE_TUNNEL_PUBLIC_URL",
        "CLOUDFLARE_TUNNEL_PUBLIC_URL",
    ] {
        if let Ok(public_url) = std::env::var(key) {
            let public_url = public_url.trim();
            if !public_url.is_empty() {
                return Ok(parse_public_url(public_url)?.base_url);
            }
        }
    }
    if let Ok(public_url) = std::env::var("FLUXER_PUBLIC_URL") {
        let public_url = public_url.trim();
        if !public_url.is_empty() && public_url != LOCAL_APP_URL {
            return Ok(parse_public_url(public_url)?.base_url);
        }
    }
    let path = default_public_url_file();
    if let Ok(public_url) = fs::read_to_string(&path) {
        let public_url = public_url.trim();
        if !public_url.is_empty() {
            return Ok(parse_public_url(public_url)?.base_url);
        }
    }
    bail!(
        "Missing Cloudflare tunnel public URL. Run `pnpm dev:tunnel:configure -- --public-url https://...` or pass `pnpm dev -- --cloudflare-tunnel --public-url https://...`."
    );
}

pub fn apply_cloudflare_public_url_env(public_url_arg: Option<&str>) -> Result<String> {
    let public_url = resolve_cloudflare_public_url(public_url_arg)?;
    apply_public_url_env(&public_url)?;
    Ok(public_url)
}

pub async fn run_cloudflare_tunnel(
    token_arg: Option<String>,
    token_file: Option<PathBuf>,
) -> Result<i32> {
    let token = resolve_cloudflare_token(token_arg, token_file.as_deref())?;
    wait_cloudflare_tunnel_origin().await?;
    let public_url = display_public_url();
    println!("Starting Cloudflare Tunnel for {public_url} -> http://127.0.0.1:8088");
    if let Some(binary) = resolve_cloudflared_binary() {
        let status = Command::new(binary)
            .args(["tunnel", "run", "--token", token.as_str()])
            .status()
            .context("failed to start cloudflared")?;
        return Ok(status.code().unwrap_or(1));
    }
    if running_inside_devcontainer() {
        let docker = docker_command();
        let mut command = Command::new(&docker[0]);
        command.args(&docker[1..]);
        let status = command
            .args([
                "run",
                "--rm",
                "--network",
                &format!(
                    "container:{}",
                    std::env::var("HOSTNAME")
                        .unwrap_or_else(|_| "fluxer-dev-workspace-1".to_owned())
                ),
                "cloudflare/cloudflared:latest",
                "tunnel",
                "run",
                "--token",
                token.as_str(),
            ])
            .status()
            .context("failed to start cloudflare/cloudflared Docker image")?;
        return Ok(status.code().unwrap_or(1));
    }
    bail!(
        "cloudflared is not installed. Install cloudflared or run from the devcontainer with Docker available."
    );
}

async fn wait_cloudflare_tunnel_origin() -> Result<()> {
    let timeout = std::env::var("FLUXER_CLOUDFLARE_TUNNEL_ORIGIN_READY_TIMEOUT")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(30);
    if let Err(error) = wait_tcp("Fluxer dev proxy", LOOPBACK_HOST, DEV_PROXY_PORT, timeout).await {
        bail!(
            "Cloudflare Tunnel origin is not reachable at http://{LOOPBACK_HOST}:{DEV_PROXY_PORT}: {error}\nStart the full stack with `pnpm dev:tunnel`; `pnpm dev:tunnel:run` only starts cloudflared and expects the dev proxy to already be running."
        );
    }
    Ok(())
}

fn resolve_cloudflare_token(
    token_arg: Option<String>,
    token_file: Option<&Path>,
) -> Result<String> {
    if let Some(token) = token_arg {
        let token = token.trim();
        if !token.is_empty() {
            return Ok(token.to_owned());
        }
    }
    for key in [
        "FLUXER_CLOUDFLARE_TUNNEL_TOKEN",
        "CLOUDFLARE_TUNNEL_TOKEN",
        "TUNNEL_TOKEN",
    ] {
        if let Ok(token) = std::env::var(key) {
            let token = token.trim();
            if !token.is_empty() {
                return Ok(token.to_owned());
            }
        }
    }
    let path = token_file
        .map(Path::to_path_buf)
        .unwrap_or_else(default_token_file);
    if let Ok(token) = fs::read_to_string(&path) {
        let token = token.trim();
        if !token.is_empty() {
            return Ok(token.to_owned());
        }
    }
    bail!(
        "Missing Cloudflare tunnel token. Set FLUXER_CLOUDFLARE_TUNNEL_TOKEN or write {}.",
        path.display()
    );
}

fn display_public_url() -> String {
    if let Ok(public_url) = std::env::var("FLUXER_PUBLIC_URL") {
        let public_url = public_url.trim();
        if !public_url.is_empty() && public_url != LOCAL_APP_URL {
            return public_url.to_owned();
        }
    }
    if let Ok(public_url) = fs::read_to_string(default_public_url_file()) {
        let public_url = public_url.trim();
        if !public_url.is_empty() {
            return public_url.to_owned();
        }
    }
    LOCAL_APP_URL.to_owned()
}

fn resolve_cloudflared_binary() -> Option<PathBuf> {
    if let Ok(binary) = std::env::var("FLUXER_CLOUDFLARED_BIN")
        && !binary.trim().is_empty()
    {
        return Some(PathBuf::from(binary));
    }
    which("cloudflared")
}

fn running_inside_devcontainer() -> bool {
    ROOT.starts_with("/workspaces")
        && Path::new("/var/run/docker.sock").exists()
        && which("docker").is_some()
}

fn docker_command() -> Vec<PathBuf> {
    if docker_socket_is_writable() || which("sudo").is_none() {
        return vec![PathBuf::from("docker")];
    }
    vec![PathBuf::from("sudo"), PathBuf::from("docker")]
}

fn docker_socket_is_writable() -> bool {
    std::os::unix::net::UnixStream::connect("/var/run/docker.sock").is_ok()
}

fn replace_marked_block(existing: &str, generated: &str) -> String {
    let Some(start) = existing.find(LOCAL_ENV_START) else {
        return append_block(existing, generated);
    };
    let Some(relative_end) = existing[start..].find(LOCAL_ENV_END) else {
        return append_block(existing, generated);
    };
    let end = start + relative_end + LOCAL_ENV_END.len();
    let mut next = String::new();
    next.push_str(existing[..start].trim_end());
    if !next.is_empty() {
        next.push_str("\n\n");
    }
    next.push_str(generated.trim_end());
    let suffix = existing[end..].trim_start_matches(['\r', '\n']);
    if !suffix.is_empty() {
        next.push_str("\n\n");
        next.push_str(suffix);
    } else {
        next.push('\n');
    }
    next
}

fn append_block(existing: &str, generated: &str) -> String {
    let mut next = existing.trim_end().to_owned();
    if !next.is_empty() {
        next.push_str("\n\n");
    }
    next.push_str(generated.trim_end());
    next.push('\n');
    next
}

#[cfg(unix)]
fn set_private_file_mode(path: &Path) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut permissions = fs::metadata(path)?.permissions();
    permissions.set_mode(0o600);
    fs::set_permissions(path, permissions)?;
    Ok(())
}

#[cfg(not(unix))]
fn set_private_file_mode(_path: &Path) -> Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn derives_https_public_url_env() {
        let env = public_url_env("https://dev.example.test").unwrap();
        assert!(env.iter().any(|(key, value)| {
            key == "FLUXER_GATEWAY_ENDPOINT" && value == "wss://dev.example.test/gateway"
        }));
        assert!(env.iter().any(|(key, value)| {
            key == "FLUXER_LIVEKIT_URL" && value == "wss://dev.example.test/livekit"
        }));
        assert!(env.iter().any(|(key, value)| {
            key == "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT"
                && value == "https://dev.example.test/media"
        }));
        assert!(env.iter().any(|(key, value)| {
            key == "FLUXER_S3_PUBLIC_ENDPOINT" && value == "https://dev.example.test"
        }));
        assert!(
            env.iter()
                .any(|(key, value)| { key == "FLUXER_PUBLIC_PORT" && value == "443" })
        );
    }

    #[test]
    fn rejects_public_url_with_path() {
        assert!(parse_public_url("https://example.com/app").is_err());
    }

    #[test]
    fn resolves_public_url_arg_normalized() {
        assert_eq!(
            resolve_cloudflare_public_url(Some("https://example.com/")).unwrap(),
            "https://example.com"
        );
    }

    #[test]
    fn replaces_existing_generated_block() {
        let existing =
            "A=1\n\n# BEGIN fluxer-dev public URL\nOLD=1\n# END fluxer-dev public URL\n\nB=2\n";
        let next = replace_marked_block(existing, "NEW=1\n");
        assert_eq!(next, "A=1\n\nNEW=1\n\nB=2\n");
    }
}
