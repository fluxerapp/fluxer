// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, remove_dir_if_exists, remove_file_if_exists, run_command};
use anyhow::{Context, Result, anyhow};
use clap::{Args, ValueEnum};
use std::env;
use std::path::{Path, PathBuf};

const GLEAM_SOURCE_DIRS: &[&str] = &[
    "src",
    "test",
    "bouncer/src",
    "bouncer/test",
    "bouncer_proto/src",
    "bouncer_proto/test",
];
const BOUNCER_PACKAGES: &[&str] = &["bouncer_proto", "bouncer"];
const GLEAM_BUILD_DIRS: &[&str] = &["build", "bouncer/build", "bouncer_proto/build"];
const GATEWAY_NIF: &str = "priv/gateway_nif.so";

#[derive(Debug, Args, Clone)]
pub struct GatewayArgs {
    #[arg(long, value_enum)]
    step: GatewayStep,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
#[clap(rename_all = "snake_case")]
pub(crate) enum GatewayStep {
    Fmt,
    FmtCheck,
    Compile,
    Test,
    BouncerTest,
    RustFmtCheck,
    Clippy,
    RustTest,
    AllChecks,
    Clean,
}

const ALL_CHECKS: &[(&str, GatewayStep)] = &[
    ("Format check (gleam)", GatewayStep::FmtCheck),
    ("Format check (rustfmt)", GatewayStep::RustFmtCheck),
    ("Clippy", GatewayStep::Clippy),
    ("Compile", GatewayStep::Compile),
    ("Rust tests", GatewayStep::RustTest),
    ("Gateway tests", GatewayStep::Test),
    ("Bouncer tests", GatewayStep::BouncerTest),
];

pub fn run_gateway(args: GatewayArgs) -> Result<()> {
    let gateway_dir = resolve_gateway_dir()?;
    run_gateway_step(&gateway_dir, args.step)
}

pub(crate) fn run_gateway_step(gateway_dir: &Path, step: GatewayStep) -> Result<()> {
    match step {
        GatewayStep::AllChecks => {
            for (index, (label, step)) in ALL_CHECKS.iter().enumerate() {
                println!("========================================");
                println!("  Step {}/{}: {label}", index + 1, ALL_CHECKS.len());
                println!("========================================");
                run_gateway_step(gateway_dir, *step)?;
                println!();
            }
            println!("All gateway checks passed.");
            Ok(())
        }
        GatewayStep::Clean => {
            for dir in GLEAM_BUILD_DIRS {
                remove_dir_if_exists(&gateway_dir.join(dir))?;
            }
            remove_dir_if_exists(&gateway_dir.join("rust/target"))?;
            remove_file_if_exists(&gateway_dir.join(GATEWAY_NIF))?;
            println!("Cleaned gateway build outputs.");
            Ok(())
        }
        step => {
            for command in gateway_commands(gateway_dir, step) {
                run_command(command)?;
            }
            Ok(())
        }
    }
}

fn gateway_commands(gateway_dir: &Path, step: GatewayStep) -> Vec<CommandSpec> {
    let rust_dir = gateway_dir.join("rust");
    let gleam_sh = gateway_dir.join("scripts/gleam.sh");
    match step {
        GatewayStep::Fmt => vec![
            CommandSpec::new("gleam")
                .arg("format")
                .args(GLEAM_SOURCE_DIRS.iter().copied())
                .current_dir(gateway_dir),
            CommandSpec::new("cargo").arg("fmt").current_dir(rust_dir),
        ],
        GatewayStep::FmtCheck => vec![
            CommandSpec::new("gleam")
                .args(["format", "--check"])
                .args(GLEAM_SOURCE_DIRS.iter().copied())
                .current_dir(gateway_dir),
        ],
        GatewayStep::Compile => {
            let mut commands = vec![
                CommandSpec::new(&gleam_sh)
                    .args(["build", "--warnings-as-errors"])
                    .current_dir(gateway_dir),
            ];
            commands.extend(BOUNCER_PACKAGES.iter().map(|package| {
                CommandSpec::new("gleam")
                    .args(["build", "--warnings-as-errors"])
                    .current_dir(gateway_dir.join(package))
            }));
            commands
        }
        GatewayStep::Test => vec![
            CommandSpec::new(&gleam_sh)
                .arg("test")
                .current_dir(gateway_dir),
        ],
        GatewayStep::BouncerTest => BOUNCER_PACKAGES
            .iter()
            .map(|package| {
                CommandSpec::new("gleam")
                    .arg("test")
                    .current_dir(gateway_dir.join(package))
            })
            .collect(),
        GatewayStep::RustFmtCheck => vec![
            CommandSpec::new("cargo")
                .args(["fmt", "--check"])
                .current_dir(rust_dir),
        ],
        GatewayStep::Clippy => vec![
            CommandSpec::new("cargo")
                .args([
                    "clippy",
                    "--locked",
                    "--all-targets",
                    "--",
                    "-D",
                    "warnings",
                ])
                .current_dir(rust_dir),
        ],
        GatewayStep::RustTest => vec![
            CommandSpec::new("cargo")
                .args(["test", "--locked"])
                .current_dir(rust_dir),
        ],
        GatewayStep::AllChecks | GatewayStep::Clean => Vec::new(),
    }
}

fn resolve_gateway_dir() -> Result<PathBuf> {
    let cwd = env::current_dir().context("Failed to resolve current directory")?;
    if cwd.join("gleam.toml").is_file()
        && cwd.file_name().and_then(|value| value.to_str()) == Some("fluxer_gateway")
    {
        return Ok(cwd);
    }
    if cwd.join("fluxer_gateway/gleam.toml").is_file() {
        return Ok(cwd.join("fluxer_gateway"));
    }
    Err(anyhow!(
        "Could not resolve fluxer_gateway directory from {}",
        cwd.display()
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsString;

    fn command_lines(step: GatewayStep) -> Vec<(String, Vec<String>, PathBuf)> {
        gateway_commands(Path::new("/repo/fluxer_gateway"), step)
            .into_iter()
            .map(|command| {
                (
                    command.program.to_string_lossy().into_owned(),
                    command
                        .args
                        .iter()
                        .map(|arg| arg.to_string_lossy().into_owned())
                        .collect(),
                    command
                        .cwd
                        .expect("gateway commands set a working directory"),
                )
            })
            .collect()
    }

    #[test]
    fn format_check_covers_the_gateway_and_both_bouncer_packages() {
        let commands = command_lines(GatewayStep::FmtCheck);
        assert_eq!(commands.len(), 1);
        let (program, args, cwd) = &commands[0];
        assert_eq!(program, "gleam");
        assert_eq!(&args[..2], ["format", "--check"]);
        assert_eq!(&args[2..], GLEAM_SOURCE_DIRS);
        assert_eq!(cwd, Path::new("/repo/fluxer_gateway"));
    }

    #[test]
    fn compile_treats_warnings_as_errors_in_every_package() {
        let commands = command_lines(GatewayStep::Compile);
        let cwds = commands
            .iter()
            .map(|(_, _, cwd)| cwd.clone())
            .collect::<Vec<_>>();
        assert_eq!(
            cwds,
            [
                PathBuf::from("/repo/fluxer_gateway"),
                PathBuf::from("/repo/fluxer_gateway/bouncer_proto"),
                PathBuf::from("/repo/fluxer_gateway/bouncer"),
            ]
        );
        for (_, args, _) in &commands {
            assert_eq!(args, &["build", "--warnings-as-errors"]);
        }
        assert_eq!(commands[0].0, "/repo/fluxer_gateway/scripts/gleam.sh");
    }

    #[test]
    fn gateway_tests_run_through_the_wrapper_that_checks_the_summary_line() {
        let commands = command_lines(GatewayStep::Test);
        assert_eq!(
            commands,
            [(
                "/repo/fluxer_gateway/scripts/gleam.sh".to_string(),
                vec!["test".to_string()],
                PathBuf::from("/repo/fluxer_gateway"),
            )]
        );
    }

    #[test]
    fn rust_steps_run_inside_the_gateway_workspace() {
        for step in [
            GatewayStep::RustFmtCheck,
            GatewayStep::Clippy,
            GatewayStep::RustTest,
        ] {
            for (program, _, cwd) in command_lines(step) {
                assert_eq!(program, "cargo");
                assert_eq!(cwd, Path::new("/repo/fluxer_gateway/rust"));
            }
        }
        let clippy = gateway_commands(Path::new("/repo/fluxer_gateway"), GatewayStep::Clippy);
        assert!(
            clippy[0]
                .args
                .ends_with(&[OsString::from("-D"), OsString::from("warnings")])
        );
    }

    #[test]
    fn all_checks_runs_every_check_step() {
        let steps = ALL_CHECKS.iter().map(|(_, step)| *step).collect::<Vec<_>>();
        for step in GatewayStep::value_variants() {
            let is_check = !matches!(
                step,
                GatewayStep::Fmt | GatewayStep::AllChecks | GatewayStep::Clean
            );
            assert_eq!(steps.contains(step), is_check, "{step:?}");
        }
    }
}
