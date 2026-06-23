// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, run_command};
use crate::desktop::write_build_channel_file;
use crate::gateway::{GatewayStep, run_gateway_step};
use anyhow::{Context, Result};
use clap::{Args, ValueEnum};
use std::env;
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

#[derive(Debug, Args, Clone)]
pub struct CiScriptsArgs {
    #[arg(long, value_enum)]
    step: CiScriptsStep,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
#[clap(rename_all = "snake_case")]
enum CiScriptsStep {
    Sync,
    Test,
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
            run_app_test_artifact_generators(&root)?;
            run_command(
                CommandSpec::new("pnpm")
                    .args(["-r", "--if-present", "typecheck"])
                    .current_dir(root),
            )
        }
        CiStep::Test => {
            run_generators(&root, false)?;
            run_app_test_artifact_generators(&root)?;
            run_workspace_tests(&root)?;
            run_command(with_test_env(
                CommandSpec::new("pnpm")
                    .args(["--filter", "fluxer_api", "test"])
                    .current_dir(root),
            ))
        }
        CiStep::Knip => {
            run_app_test_artifact_generators(&root)?;
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

fn run_app_test_artifact_generators(root: &Path) -> Result<()> {
    run_fluxer_app_script(root, "wasm:codegen")?;
    run_fluxer_app_script(root, "generate:masks")
}

fn run_fluxer_app_script(root: &Path, script: &str) -> Result<()> {
    run_command(
        CommandSpec::new("pnpm")
            .args(["--filter", "fluxer_app", script])
            .current_dir(root),
    )
}

pub async fn run_ci_scripts(args: CiScriptsArgs) -> Result<()> {
    let root = repo_root()?;
    match args.step {
        CiScriptsStep::Sync => run_command(
            CommandSpec::new("cargo")
                .args([
                    "fetch",
                    "--locked",
                    "--manifest-path",
                    "tools/ci/Cargo.toml",
                ])
                .current_dir(root),
        ),
        CiScriptsStep::Test => run_command(
            CommandSpec::new("cargo")
                .args(["test", "--locked", "--manifest-path", "tools/ci/Cargo.toml"])
                .current_dir(root),
        ),
    }
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

fn run_workspace_tests(root: &Path) -> Result<()> {
    let workspace_concurrency =
        env::var("PNPM_TEST_WORKSPACE_CONCURRENCY").unwrap_or_else(|_| "2".to_string());
    run_command(with_test_env(
        CommandSpec::new("pnpm")
            .args([
                "-r",
                &format!("--workspace-concurrency={workspace_concurrency}"),
                "--filter",
                "!fluxer_api",
                "--filter",
                "!fluxer",
                "--if-present",
                "test",
            ])
            .current_dir(root),
    ))
}

fn with_test_env(spec: CommandSpec) -> CommandSpec {
    let nats_url = env::var("FLUXER_NATS_URL").unwrap_or_else(|_| default_test_nats_url());
    let api_workers = env::var("API_TEST_MAX_WORKERS").unwrap_or_else(|_| "2".to_string());
    spec.env("FLUXER_NATS_URL", &nats_url)
        .env(
            "FLUXER_NATS_CORE_URL",
            env::var("FLUXER_NATS_CORE_URL").unwrap_or_else(|_| nats_url.clone()),
        )
        .env(
            "FLUXER_NATS_JETSTREAM_URL",
            env::var("FLUXER_NATS_JETSTREAM_URL").unwrap_or_else(|_| nats_url.clone()),
        )
        .env("API_TEST_MAX_WORKERS", &api_workers)
        .env(
            "API_TEST_MAX_CONCURRENCY",
            env::var("API_TEST_MAX_CONCURRENCY").unwrap_or(api_workers),
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
    use std::ffi::OsString;

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
    fn with_test_env_sets_all_nats_urls_and_concurrency() {
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
        assert_eq!(
            env.get(&OsString::from("API_TEST_MAX_WORKERS")),
            Some(&OsString::from("2"))
        );
        assert_eq!(
            env.get(&OsString::from("API_TEST_MAX_CONCURRENCY")),
            Some(&OsString::from("2"))
        );
    }
}
