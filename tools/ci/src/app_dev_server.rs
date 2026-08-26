// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::app_wasm::resolve_app_dir;
use anyhow::{Context, Result, bail, ensure};
use clap::Args;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::process::{Child, Command};
use tokio::sync::watch;
use tokio::time::timeout;
use walkdir::WalkDir;

const DEFAULT_SKIP_DIRS: &[&str] = &[".git", "node_modules", "dist", "target", "pkg", "pkgs"];

#[derive(Debug, Args, Clone)]
pub struct AppDevServerArgs {
    #[arg(long)]
    app_dir: Option<PathBuf>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
enum InputFingerprint {
    Sha256 {
        sha256: String,
        #[serde(skip)]
        legacy_timestamp_ms: Option<f64>,
    },
    LegacyTimestamp(f64),
}

impl InputFingerprint {
    fn matches(&self, current: &Self) -> bool {
        match (self, current) {
            (
                Self::Sha256 { sha256, .. },
                Self::Sha256 {
                    sha256: current, ..
                },
            ) => sha256 == current,
            (
                Self::LegacyTimestamp(timestamp),
                Self::Sha256 {
                    legacy_timestamp_ms: Some(current),
                    ..
                },
            ) => timestamp == current,
            (Self::LegacyTimestamp(timestamp), Self::LegacyTimestamp(current)) => {
                timestamp == current
            }
            _ => false,
        }
    }

    fn is_legacy(&self) -> bool {
        matches!(self, Self::LegacyTimestamp(_))
    }
}

type StepInputs = BTreeMap<String, InputFingerprint>;

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
struct StepMetadata {
    inputs: StepInputs,
}

type Metadata = BTreeMap<String, StepMetadata>;

pub async fn run(args: AppDevServerArgs) -> Result<()> {
    let project_root = args.app_dir.unwrap_or(resolve_app_dir()?);
    let mut server = AppDevServer::new(project_root);
    server.run().await
}

struct AppDevServer {
    project_root: PathBuf,
    metadata_file: PathBuf,
    metadata: Metadata,
}

impl AppDevServer {
    fn new(project_root: PathBuf) -> Self {
        Self {
            metadata_file: project_root.join(".devserver-cache.json"),
            project_root,
            metadata: Metadata::default(),
        }
    }

    async fn run(&mut self) -> Result<()> {
        self.load_metadata();
        let (shutdown_tx, mut shutdown_rx) = watch::channel(false);
        tokio::spawn(listen_for_shutdown(shutdown_tx));

        self.run_cached_step(
            "wasm",
            gather_wasm_inputs,
            "pnpm wasm:codegen",
            |server, shutdown| Box::pin(server.run_command("pnpm", &["wasm:codegen"], shutdown)),
            &mut shutdown_rx,
        )
        .await?;
        self.run_cached_step(
            "colors",
            gather_color_inputs,
            "pnpm generate:colors",
            |server, shutdown| Box::pin(server.run_command("pnpm", &["generate:colors"], shutdown)),
            &mut shutdown_rx,
        )
        .await?;
        self.run_cached_step(
            "messageLayout",
            gather_message_layout_inputs,
            "pnpm generate:message-layout",
            |server, shutdown| {
                Box::pin(server.run_command("pnpm", &["generate:message-layout"], shutdown))
            },
            &mut shutdown_rx,
        )
        .await?;
        self.run_cached_step(
            "masks",
            gather_mask_inputs,
            "pnpm generate:masks",
            |server, shutdown| Box::pin(server.run_command("pnpm", &["generate:masks"], shutdown)),
            &mut shutdown_rx,
        )
        .await?;
        self.run_cached_step(
            "cssTypes",
            gather_css_module_inputs,
            "pnpm generate:css-types",
            |server, shutdown| {
                Box::pin(server.run_command("pnpm", &["generate:css-types"], shutdown))
            },
            &mut shutdown_rx,
        )
        .await?;

        self.run_cached_step(
            "lingui",
            gather_lingui_inputs,
            "pnpm lingui:compile",
            |server, shutdown| Box::pin(server.run_command("pnpm", &["lingui:compile"], shutdown)),
            &mut shutdown_rx,
        )
        .await?;

        if *shutdown_rx.borrow() {
            return Ok(());
        }

        self.clean_dist()?;
        let mut css_type_watcher = self.start_css_type_watcher()?;
        let rspack_result = self.run_rspack(&mut shutdown_rx).await;
        terminate_child(&mut css_type_watcher).await;
        rspack_result
    }

    fn load_metadata(&mut self) {
        match fs::read_to_string(&self.metadata_file) {
            Ok(raw) => match serde_json::from_str::<Metadata>(&raw) {
                Ok(metadata) => {
                    self.metadata = metadata;
                }
                Err(error) => {
                    eprintln!(
                        "Failed to parse dev server metadata cache, falling back to full rebuild: {error}"
                    );
                    self.metadata = Metadata::default();
                }
            },
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                self.metadata = Metadata::default();
            }
            Err(error) => {
                eprintln!(
                    "Failed to read dev server metadata cache, falling back to full rebuild: {error}"
                );
                self.metadata = Metadata::default();
            }
        }
    }

    fn save_metadata(&self) -> Result<()> {
        if let Some(parent) = self.metadata_file.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create {}", parent.display()))?;
        }
        fs::write(
            &self.metadata_file,
            serde_json::to_string_pretty(&self.metadata)?,
        )
        .with_context(|| format!("Failed to write {}", self.metadata_file.display()))
    }

    async fn run_cached_step<G, E>(
        &mut self,
        step_name: &'static str,
        gather_inputs: G,
        label: &'static str,
        execute: E,
        shutdown: &mut watch::Receiver<bool>,
    ) -> Result<()>
    where
        G: Fn(&Path) -> Result<StepInputs>,
        E: for<'a> FnOnce(
            &'a AppDevServer,
            &'a mut watch::Receiver<bool>,
        )
            -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + 'a>>,
    {
        if *shutdown.borrow() {
            return Ok(());
        }
        let inputs = gather_inputs(&self.project_root)?;
        if !self.should_run_step(step_name, &inputs) {
            let needs_upgrade = self
                .metadata
                .get(step_name)
                .is_some_and(|entry| entry.inputs.values().any(InputFingerprint::is_legacy));
            if needs_upgrade {
                self.metadata
                    .insert(step_name.to_string(), StepMetadata { inputs });
                self.save_metadata()?;
            }
            println!("Skipping {label} (no changes detected)");
            return Ok(());
        }

        execute(self, shutdown).await?;
        if *shutdown.borrow() {
            return Ok(());
        }
        self.metadata
            .insert(step_name.to_string(), StepMetadata { inputs });
        self.save_metadata()
    }

    fn should_run_step(&self, step_name: &str, inputs: &StepInputs) -> bool {
        let Some(entry) = self.metadata.get(step_name) else {
            return true;
        };
        entry.inputs.len() != inputs.len()
            || entry.inputs.iter().any(|(path, cached)| {
                inputs
                    .get(path)
                    .is_none_or(|current| !cached.matches(current))
            })
    }

    async fn run_command(
        &self,
        command: &str,
        args: &[&str],
        shutdown: &mut watch::Receiver<bool>,
    ) -> Result<()> {
        if *shutdown.borrow() {
            return Ok(());
        }

        let mut child = spawn_child(command, args, &self.project_root)?;
        let status = wait_for_child(command, args, &mut child, shutdown).await?;
        if *shutdown.borrow() {
            return Ok(());
        }
        ensure!(
            status.success(),
            "{} exited with status {}",
            display_command(command, args),
            status.code().unwrap_or(1)
        );
        Ok(())
    }

    fn clean_dist(&self) -> Result<()> {
        let dist_path = self.project_root.join("dist");
        match fs::remove_dir_all(&dist_path) {
            Ok(()) => Ok(()),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(error) => {
                Err(error).with_context(|| format!("Failed to remove {}", dist_path.display()))
            }
        }
    }

    fn start_css_type_watcher(&self) -> Result<Child> {
        let tcm = self.project_root.join("node_modules/.bin/tcm");
        println!(
            "+ {} src --pattern '**/*.module.css' --watch --silent",
            tcm.display()
        );
        Command::new(tcm)
            .args(["src", "--pattern", "**/*.module.css", "--watch", "--silent"])
            .current_dir(&self.project_root)
            .stdin(Stdio::null())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .spawn()
            .context("Failed to start CSS type watcher")
    }

    async fn run_rspack(&self, shutdown: &mut watch::Receiver<bool>) -> Result<()> {
        let rspack = self.project_root.join("node_modules/.bin/rspack");
        let rspack_string = rspack.to_string_lossy().to_string();
        let mut child = spawn_child(
            &rspack_string,
            &["serve", "--mode", "development"],
            &self.project_root,
        )?;
        let status = wait_for_child(
            &rspack_string,
            &["serve", "--mode", "development"],
            &mut child,
            shutdown,
        )
        .await?;
        if *shutdown.borrow() {
            return Ok(());
        }
        if status.success() {
            Ok(())
        } else {
            bail!(
                "rspack serve exited with status {}",
                status.code().unwrap_or(1)
            )
        }
    }
}

fn collect_file_digests(project_root: &Path, paths: &[PathBuf]) -> Result<StepInputs> {
    let mut result = BTreeMap::new();
    for rel_path in paths {
        let absolute_path = project_root.join(rel_path);
        let metadata = fs::metadata(&absolute_path)
            .with_context(|| format!("Failed to stat {}", absolute_path.display()))?;
        ensure!(
            metadata.is_file(),
            "Expected {} to be a file when collecting dev server cache inputs.",
            rel_path.display()
        );
        result.insert(rel_path_key(rel_path), fingerprint_file(&absolute_path)?);
    }
    Ok(result)
}

fn collect_directory_digests<P>(
    project_root: &Path,
    root_rel: &Path,
    predicate: P,
) -> Result<StepInputs>
where
    P: Fn(&str) -> bool,
{
    let skip_dirs: BTreeSet<&str> = DEFAULT_SKIP_DIRS.iter().copied().collect();
    let root = project_root.join(root_rel);
    let mut result = BTreeMap::new();
    if !root.exists() {
        return Ok(result);
    }

    for entry in WalkDir::new(&root)
        .into_iter()
        .filter_entry(|entry| should_walk_entry(entry.path(), &skip_dirs))
    {
        let entry = entry.with_context(|| format!("Failed to read {}", root.display()))?;
        if !entry.file_type().is_file() {
            continue;
        }
        let rel_from_root = entry
            .path()
            .strip_prefix(&root)
            .with_context(|| format!("Failed to relativize {}", entry.path().display()))?
            .to_path_buf();
        let rel_path = root_rel.join(rel_from_root);
        let key = rel_path_key(&rel_path);
        if !predicate(&key) {
            continue;
        }
        result.insert(key, fingerprint_file(entry.path())?);
    }
    Ok(result)
}

fn hash_file(path: &Path) -> Result<String> {
    let mut file = fs::File::open(path)
        .with_context(|| format!("Failed to open {} for hashing", path.display()))?;
    let mut hasher = Sha256::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let bytes_read = file
            .read(&mut buffer)
            .with_context(|| format!("Failed to read {} for hashing", path.display()))?;
        if bytes_read == 0 {
            break;
        }
        hasher.update(&buffer[..bytes_read]);
    }
    Ok(hex::encode(hasher.finalize()))
}

fn fingerprint_file(path: &Path) -> Result<InputFingerprint> {
    let sha256 = hash_file(path)?;
    let metadata = fs::metadata(path)
        .with_context(|| format!("Failed to stat {} after hashing", path.display()))?;
    Ok(InputFingerprint::Sha256 {
        sha256,
        legacy_timestamp_ms: Some(timestamp_ms(metadata.modified()?)?),
    })
}

fn should_walk_entry(path: &Path, skip_dirs: &BTreeSet<&str>) -> bool {
    path.file_name()
        .and_then(OsStr::to_str)
        .is_none_or(|name| !skip_dirs.contains(name))
}

fn gather_wasm_inputs(project_root: &Path) -> Result<StepInputs> {
    let markdown_parser_rust_dir = PathBuf::from("../packages/markdown_parser/rust");
    let mut inputs = collect_file_digests(
        project_root,
        &[
            PathBuf::from("../tools/ci/Cargo.toml"),
            PathBuf::from("../tools/ci/src/app_dev_server.rs"),
            PathBuf::from("../tools/ci/src/app_wasm.rs"),
            PathBuf::from("../tools/ci/src/common.rs"),
            PathBuf::from("../tools/ci/src/lib.rs"),
            PathBuf::from("../tools/ci/templates/libfluxcore_wrapper.js"),
            PathBuf::from("../tools/ci/templates/libfluxcore_wrapper.d.ts"),
            markdown_parser_rust_dir.join("Cargo.toml"),
        ],
    )?;
    inputs.extend(collect_directory_digests(
        project_root,
        Path::new("rust/libfluxcore"),
        |path| !path.contains("/target/"),
    )?);
    inputs.extend(collect_directory_digests(
        project_root,
        &markdown_parser_rust_dir,
        |path| !path.contains("/target/"),
    )?);
    Ok(inputs)
}

fn gather_color_inputs(project_root: &Path) -> Result<StepInputs> {
    collect_file_digests(
        project_root,
        &[PathBuf::from("scripts/GenerateColorSystem.ts")],
    )
}

fn gather_message_layout_inputs(project_root: &Path) -> Result<StepInputs> {
    collect_file_digests(
        project_root,
        &[
            PathBuf::from("scripts/GenerateMessageLayoutCss.ts"),
            PathBuf::from("src/features/theme/layout/MessageLayoutSpec.ts"),
        ],
    )
}

fn gather_mask_inputs(project_root: &Path) -> Result<StepInputs> {
    collect_file_digests(
        project_root,
        &[
            PathBuf::from("scripts/GenerateAvatarMasks.ts"),
            PathBuf::from("src/features/ui/constants/TypingConstants.ts"),
        ],
    )
}

fn gather_css_module_inputs(project_root: &Path) -> Result<StepInputs> {
    collect_directory_digests(project_root, Path::new("src"), |path| {
        path.ends_with(".module.css")
    })
}

fn gather_lingui_inputs(project_root: &Path) -> Result<StepInputs> {
    collect_directory_digests(
        project_root,
        Path::new("src/features/i18n/locales"),
        |path| path.ends_with(".po"),
    )
}

fn spawn_child(command: &str, args: &[&str], cwd: &Path) -> Result<Child> {
    println!("+ {}", display_command(command, args));
    Command::new(command)
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .with_context(|| format!("Failed to run {}", display_command(command, args)))
}

async fn wait_for_child(
    command: &str,
    args: &[&str],
    child: &mut Child,
    shutdown: &mut watch::Receiver<bool>,
) -> Result<std::process::ExitStatus> {
    tokio::select! {
        status = child.wait() => {
            status.with_context(|| format!("Failed to wait for {}", display_command(command, args)))
        }
        changed = shutdown.changed() => {
            let _ = changed;
            terminate_child(child).await;
            child.wait().await.with_context(|| format!("Failed to wait for {}", display_command(command, args)))
        }
    }
}

async fn terminate_child(child: &mut Child) {
    if child.id().is_none() {
        return;
    }
    let _ = child.start_kill();
    let _ = timeout(Duration::from_secs(5), child.wait()).await;
}

async fn listen_for_shutdown(shutdown_tx: watch::Sender<bool>) {
    let signal = wait_for_shutdown_signal().await;
    println!("\nReceived {signal}, shutting down fluxer app dev server...");
    let _ = shutdown_tx.send(true);
}

#[cfg(unix)]
async fn wait_for_shutdown_signal() -> &'static str {
    let mut sigterm = match tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
    {
        Ok(signal) => signal,
        Err(_) => {
            let _ = tokio::signal::ctrl_c().await;
            return "SIGINT";
        }
    };
    tokio::select! {
        _ = tokio::signal::ctrl_c() => "SIGINT",
        _ = sigterm.recv() => "SIGTERM",
    }
}

#[cfg(not(unix))]
async fn wait_for_shutdown_signal() -> &'static str {
    let _ = tokio::signal::ctrl_c().await;
    "SIGINT"
}

fn rel_path_key(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}

fn timestamp_ms(timestamp: SystemTime) -> Result<f64> {
    Ok(timestamp
        .duration_since(UNIX_EPOCH)
        .context("File timestamp predates UNIX epoch")?
        .as_secs_f64()
        * 1000.0)
}

fn display_command(command: &str, args: &[&str]) -> String {
    std::iter::once(command.to_string())
        .chain(args.iter().map(|arg| quote_arg(arg)))
        .collect::<Vec<_>>()
        .join(" ")
}

fn quote_arg(arg: &str) -> String {
    if arg
        .chars()
        .all(|ch| ch.is_ascii_alphanumeric() || matches!(ch, '-' | '_' | '.' | '/' | ':' | '='))
    {
        arg.to_string()
    } else {
        format!("{arg:?}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rel_path_keys_are_posix_like() {
        assert_eq!(
            rel_path_key(Path::new("scripts/GenerateColorSystem.ts")),
            "scripts/GenerateColorSystem.ts"
        );
    }

    #[test]
    fn display_command_quotes_globs() {
        assert_eq!(
            display_command("tcm", &["src", "--pattern", "**/*.module.css"]),
            "tcm src --pattern \"**/*.module.css\""
        );
    }
}
