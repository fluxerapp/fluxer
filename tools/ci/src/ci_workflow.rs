// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, run_command};
use crate::desktop::write_build_channel_file;
use crate::gateway::{GatewayStep, run_gateway_step};
use anyhow::{Context, Result};
use clap::{Args, ValueEnum};
use std::env;
use std::ffi::OsString;
use std::path::{Path, PathBuf};

#[derive(Debug, Args, Clone)]
pub struct CiArgs {
    #[arg(long, value_enum)]
    step: CiStep,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
#[clap(rename_all = "snake_case")]
enum CiStep {
    InstallDependencies,
    Typecheck,
    Test,
    Knip,
    GatewayFmt,
    GatewayCompile,
    GatewayDialyzer,
    GatewayEunit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AppWasm {
    Build,
    ReuseIfPresent,
}

pub async fn run_ci(args: CiArgs) -> Result<()> {
    let root = repo_root()?;
    match args.step {
        CiStep::InstallDependencies => run_command(
            CommandSpec::new("pnpm")
                .args(["install", "--frozen-lockfile"])
                .current_dir(root),
        ),
        CiStep::Typecheck => {
            ensure_desktop_build_channel_file(&root)?;
            run_generators(&root, true)?;
            run_app_test_artifact_generators(&root, AppWasm::Build)?;
            run_command(
                CommandSpec::new("pnpm")
                    .args(["-r", "--if-present", "typecheck"])
                    .current_dir(root),
            )
        }
        CiStep::Test => {
            run_generators(&root, false)?;
            run_app_test_artifact_generators(&root, AppWasm::ReuseIfPresent)?;
            run_workspace_tests(&root)?;
            run_command(with_test_env(
                CommandSpec::new("pnpm")
                    .args(["--filter", "fluxer_api", "test"])
                    .current_dir(root),
            ))
        }
        CiStep::Knip => {
            run_app_test_artifact_generators(&root, AppWasm::ReuseIfPresent)?;
            ensure_desktop_build_channel_file(&root)?;
            run_fluxer_app_script(&root, "i18n:compile")?;
            run_command(
                CommandSpec::new("pnpm")
                    .args(["exec", "knip"])
                    .current_dir(root),
            )
        }
        CiStep::GatewayFmt => {
            run_gateway_step(&root.join("fluxer_gateway"), GatewayStep::FmtCheck, "test")
        }
        CiStep::GatewayCompile => {
            run_gateway_step(&root.join("fluxer_gateway"), GatewayStep::Compile, "test")
        }
        CiStep::GatewayDialyzer => {
            run_gateway_step(&root.join("fluxer_gateway"), GatewayStep::Dialyzer, "test")
        }
        CiStep::GatewayEunit => {
            run_gateway_step(&root.join("fluxer_gateway"), GatewayStep::Eunit, "test")
        }
    }
}

fn ensure_desktop_build_channel_file(root: &Path) -> Result<()> {
    let channel = env::var("BUILD_CHANNEL").unwrap_or_else(|_| "stable".to_string());
    write_build_channel_file(&root.join("fluxer_desktop"), &channel)
}

fn run_app_test_artifact_generators(root: &Path, wasm: AppWasm) -> Result<()> {
    match (wasm, missing_app_wasm_artifact(root)) {
        (AppWasm::ReuseIfPresent, None) => {
            println!("Reusing restored fluxer_app wasm artifacts");
        }
        (AppWasm::ReuseIfPresent, Some(missing)) => {
            println!(
                "Rebuilding fluxer_app wasm artifacts: {} is missing",
                missing.display()
            );
            run_fluxer_app_script(root, "wasm:codegen")?;
        }
        (AppWasm::Build, _) => run_fluxer_app_script(root, "wasm:codegen")?,
    }
    run_fluxer_app_script(root, "generate:masks")
}

fn app_wasm_artifacts(root: &Path) -> Vec<PathBuf> {
    let app_dir = root.join("fluxer_app");
    vec![
        app_dir.join("pkgs/libfluxcore/libfluxcore.js"),
        app_dir.join("pkgs/libfluxcore/libfluxcore.d.ts"),
        app_dir.join("pkgs/libfluxcore/libfluxcore_bindgen.js"),
        app_dir.join("pkgs/libfluxcore/libfluxcore_bindgen.d.ts"),
        app_dir.join("pkgs/libfluxcore/libfluxcore_bg.wasm"),
        app_dir.join("pkgs/libfluxcore/libfluxcore_bg.wasm.d.ts"),
        app_dir.join("pkgs/libfluxcore/package.json"),
        app_dir.join("src/features/messaging/utils/markdown/parser/MarkdownParserWasmBytes.ts"),
    ]
}

fn missing_app_wasm_artifact(root: &Path) -> Option<PathBuf> {
    app_wasm_artifacts(root)
        .into_iter()
        .find(|path| !path.exists())
}

fn run_fluxer_app_script(root: &Path, script: &str) -> Result<()> {
    run_command(
        CommandSpec::new("pnpm")
            .args(["--filter", "fluxer_app", script])
            .current_dir(root),
    )
}

fn run_generators(root: &Path, for_typecheck: bool) -> Result<()> {
    for command in generator_commands(for_typecheck) {
        run_command(command.current_dir(root))?;
    }
    Ok(())
}

fn generator_commands(for_typecheck: bool) -> Vec<CommandSpec> {
    let mut commands = vec![
        CommandSpec::new("pnpm").args(["--filter", "@fluxer/config", "generate"]),
        CommandSpec::new("pnpm").args(["--filter", "@fluxer/schema", "generate"]),
    ];
    if for_typecheck {
        commands.push(CommandSpec::new("pnpm").args([
            "--filter",
            "@fluxer/i18n",
            "generate:types",
        ]));
    }
    commands.push(CommandSpec::new("pnpm").args(["--filter", "fluxer_app", "i18n:compile"]));
    commands
}

fn workspace_test_args(concurrency: Option<&str>) -> Vec<OsString> {
    let mut args = vec![OsString::from("-r")];
    if let Some(concurrency) = concurrency {
        args.push(OsString::from(format!(
            "--workspace-concurrency={concurrency}"
        )));
    }
    args.extend(
        [
            "--filter",
            "!fluxer_api",
            "--filter",
            "!fluxer",
            "--filter",
            "!fluxer_desktop",
            "--if-present",
            "test",
        ]
        .into_iter()
        .map(OsString::from),
    );
    args
}

fn run_workspace_tests(root: &Path) -> Result<()> {
    let concurrency = env::var("PNPM_TEST_WORKSPACE_CONCURRENCY").ok();
    run_command(with_test_env(
        CommandSpec::new("pnpm")
            .args(workspace_test_args(concurrency.as_deref()))
            .current_dir(root),
    ))
}

fn with_test_env(spec: CommandSpec) -> CommandSpec {
    let nats_url = env::var("FLUXER_NATS_URL").unwrap_or_else(|_| default_test_nats_url());
    spec.env("FLUXER_NATS_URL", &nats_url)
        .env(
            "FLUXER_NATS_CORE_URL",
            env::var("FLUXER_NATS_CORE_URL").unwrap_or_else(|_| nats_url.clone()),
        )
        .env(
            "FLUXER_NATS_JETSTREAM_URL",
            env::var("FLUXER_NATS_JETSTREAM_URL").unwrap_or_else(|_| nats_url.clone()),
        )
}

fn default_test_nats_url() -> String {
    if Path::new("/.dockerenv").exists() {
        "nats://nats:4222".to_string()
    } else {
        "nats://127.0.0.1:4222".to_string()
    }
}

fn repo_root() -> Result<PathBuf> {
    env::var("GITHUB_WORKSPACE")
        .map(PathBuf::from)
        .or_else(|_| env::current_dir())
        .context("Failed to resolve repository root")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generator_commands_include_i18n_types_only_for_typecheck() {
        let typecheck = generator_commands(true)
            .into_iter()
            .map(|command| command.args)
            .collect::<Vec<_>>();
        let test = generator_commands(false)
            .into_iter()
            .map(|command| command.args)
            .collect::<Vec<_>>();

        assert!(typecheck.contains(&vec![
            OsString::from("--filter"),
            OsString::from("@fluxer/i18n"),
            OsString::from("generate:types"),
        ]));
        assert!(!test.contains(&vec![
            OsString::from("--filter"),
            OsString::from("@fluxer/i18n"),
            OsString::from("generate:types"),
        ]));
    }

    #[test]
    fn with_test_env_sets_all_nats_urls_and_leaves_worker_counts_to_vitest() {
        let spec = with_test_env(CommandSpec::new("pnpm"));
        let env = spec
            .env
            .into_iter()
            .collect::<std::collections::BTreeMap<_, _>>();
        let default_nats_url = OsString::from(default_test_nats_url());

        assert_eq!(
            env.get(&OsString::from("FLUXER_NATS_URL")),
            Some(&default_nats_url)
        );
        assert_eq!(
            env.get(&OsString::from("FLUXER_NATS_CORE_URL")),
            Some(&default_nats_url)
        );
        assert_eq!(
            env.get(&OsString::from("FLUXER_NATS_JETSTREAM_URL")),
            Some(&default_nats_url)
        );
        assert!(!env.contains_key(&OsString::from("API_TEST_MAX_WORKERS")));
        assert!(!env.contains_key(&OsString::from("API_TEST_MAX_CONCURRENCY")));
    }

    #[test]
    fn workspace_tests_exclude_desktop_and_leave_concurrency_to_pnpm() {
        let args = workspace_test_args(None);

        assert_eq!(args[0], OsString::from("-r"));
        assert!(
            !args
                .iter()
                .any(|arg| arg.to_string_lossy().starts_with("--workspace-concurrency"))
        );
        assert!(args.windows(2).any(|pair| pair
            == [
                OsString::from("--filter"),
                OsString::from("!fluxer_desktop")
            ]));
    }

    #[test]
    fn workspace_tests_forward_an_explicit_concurrency_override() {
        let args = workspace_test_args(Some("4"));

        assert_eq!(args[1], OsString::from("--workspace-concurrency=4"));
    }

    #[test]
    fn missing_app_wasm_artifact_reports_the_first_absent_output() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();

        assert_eq!(
            missing_app_wasm_artifact(root),
            Some(root.join("fluxer_app/pkgs/libfluxcore/libfluxcore.js"))
        );

        for path in app_wasm_artifacts(root) {
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(&path, "").unwrap();
        }

        assert_eq!(missing_app_wasm_artifact(root), None);
    }
}
