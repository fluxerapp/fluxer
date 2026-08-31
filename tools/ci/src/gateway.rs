// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, command_succeeds, output_text, run_command};
use anyhow::{Context, Result, anyhow, ensure};
use clap::{Args, ValueEnum};
use std::env;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::path::{Path, PathBuf};

const GATEWAY_NIF_CRATES: &[&str] = &["push_markdown_plaintext_nif", "guild_member_list_oset_nif"];
const GNU_LD_FATAL_WARNINGS: &str = "-Wl,--fatal-warnings";

#[derive(Debug, Args, Clone)]
pub struct BuildGatewayNifsArgs {
    #[arg(long)]
    gateway_dir: Option<PathBuf>,
}

#[derive(Debug, Args, Clone)]
pub struct GatewayArgs {
    #[arg(long, value_enum)]
    step: GatewayStep,
    #[arg(long, default_value = "test")]
    eqwalizer_profile: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
#[clap(rename_all = "snake_case")]
pub(crate) enum GatewayStep {
    Fmt,
    FmtCheck,
    Lint,
    Compile,
    ProdCompile,
    Dialyzer,
    Eqwalizer,
    Typecheck,
    Eunit,
    Bench,
    AllChecks,
    Clean,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NifBuildProfile {
    Debug,
    Release,
}

impl NifBuildProfile {
    fn from_env_value(value: Option<&str>) -> Self {
        match value {
            Some("release") | None => Self::Release,
            Some(_) => Self::Debug,
        }
    }

    fn target_dir_name(self) -> &'static str {
        match self {
            Self::Debug => "debug",
            Self::Release => "release",
        }
    }

    fn cargo_args(self) -> Vec<OsString> {
        let mut args = vec![OsString::from("build")];
        if self == Self::Release {
            args.push(OsString::from("--release"));
        }
        args
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct GatewayNifBuild {
    crate_name: String,
    native_dir: PathBuf,
    cargo_args: Vec<OsString>,
    artifact_path: PathBuf,
    output_path: PathBuf,
}

pub fn run_build_gateway_nifs(args: BuildGatewayNifsArgs) -> Result<()> {
    let gateway_dir = args
        .gateway_dir
        .map(Ok)
        .unwrap_or_else(resolve_gateway_dir)?;
    build_gateway_nifs(&gateway_dir)
}

pub fn run_gateway(args: GatewayArgs) -> Result<()> {
    let gateway_dir = resolve_gateway_dir()?;
    run_gateway_step(&gateway_dir, args.step, &args.eqwalizer_profile)
}

pub(crate) fn run_gateway_step(
    gateway_dir: &Path,
    step: GatewayStep,
    eqwalizer_profile: &str,
) -> Result<()> {
    match step {
        GatewayStep::Fmt => run_rebar(gateway_dir, ["fmt"]),
        GatewayStep::FmtCheck => run_gateway_fmt_check(gateway_dir),
        GatewayStep::Lint => run_rebar(gateway_dir, ["lint"]),
        GatewayStep::Compile => run_rebar(gateway_dir, ["compile"]),
        GatewayStep::ProdCompile => {
            run_rebar(gateway_dir, ["as", "prod", "clean", "-a"])?;
            run_rebar(gateway_dir, ["as", "prod", "compile"])
        }
        GatewayStep::Dialyzer => run_rebar(gateway_dir, ["dialyzer"]),
        GatewayStep::Eqwalizer => run_eqwalizer(gateway_dir, eqwalizer_profile),
        GatewayStep::Typecheck => run_eqwalizer(gateway_dir, eqwalizer_profile),
        GatewayStep::Eunit => run_rebar(gateway_dir, ["as", "test", "eunit"]),
        GatewayStep::Bench => run_rebar(gateway_dir, ["eunit", "--module=guild_member_list_bench"]),
        GatewayStep::AllChecks => {
            run_gateway_check_step("Step 1/5: Format check (erlfmt)", || {
                run_gateway_step(gateway_dir, GatewayStep::FmtCheck, eqwalizer_profile)
            })?;
            run_gateway_check_step("Step 2/5: Lint (elvis)", || {
                run_gateway_step(gateway_dir, GatewayStep::Lint, eqwalizer_profile)
            })?;
            run_gateway_check_step("Step 3/5: Compile", || {
                run_gateway_step(gateway_dir, GatewayStep::Compile, eqwalizer_profile)
            })?;
            run_gateway_check_step("Step 4/5: Type check", || {
                run_gateway_step(gateway_dir, GatewayStep::Typecheck, eqwalizer_profile)
            })?;
            run_gateway_check_step("Step 5/5: Unit tests (eunit)", || {
                run_gateway_step(gateway_dir, GatewayStep::Eunit, eqwalizer_profile)
            })?;
            println!("All gateway checks passed.");
            Ok(())
        }
        GatewayStep::Clean => {
            run_rebar(gateway_dir, ["clean", "--all"])?;
            let plugins_dir = gateway_dir.join("_build/default/plugins");
            if plugins_dir.exists() {
                fs::remove_dir_all(&plugins_dir)
                    .with_context(|| format!("Failed to remove {}", plugins_dir.display()))?;
            }
            println!("Cleaned gateway build outputs.");
            Ok(())
        }
    }
}

fn build_gateway_nifs(gateway_dir: &Path) -> Result<()> {
    let profile =
        NifBuildProfile::from_env_value(env::var("FLUXER_GATEWAY_NIF_PROFILE").ok().as_deref());
    let builds = gateway_nif_builds(
        gateway_dir,
        profile,
        env::consts::DLL_PREFIX,
        env::consts::DLL_EXTENSION,
    );
    let priv_dir = gateway_dir.join("priv");
    fs::create_dir_all(&priv_dir)
        .with_context(|| format!("Failed to create {}", priv_dir.display()))?;

    for build in builds {
        let mut cargo_args = build.cargo_args.clone();
        cargo_args.push(OsString::from("--manifest-path"));
        cargo_args.push(build.native_dir.join("Cargo.toml").into_os_string());
        run_command(CommandSpec::new("cargo").args(cargo_args))?;

        ensure!(
            build.artifact_path.is_file(),
            "Expected NIF artifact was not produced: {}",
            build.artifact_path.display()
        );
        install_gateway_nif(&build.artifact_path, &build.output_path)?;
        println!(
            "Installed gateway NIF {} -> {}",
            build.crate_name,
            build.output_path.display()
        );
    }

    Ok(())
}

fn install_gateway_nif(artifact_path: &Path, output_path: &Path) -> Result<()> {
    let output_dir = output_path.parent().ok_or_else(|| {
        anyhow!(
            "Gateway NIF output path has no parent: {}",
            output_path.display()
        )
    })?;
    let staged_artifact = tempfile::NamedTempFile::new_in(output_dir).with_context(|| {
        format!(
            "Failed to create staged gateway NIF beside {}",
            output_path.display()
        )
    })?;
    fs::copy(artifact_path, staged_artifact.path()).with_context(|| {
        format!(
            "Failed to stage gateway NIF {} for {}",
            artifact_path.display(),
            output_path.display()
        )
    })?;
    staged_artifact
        .persist(output_path)
        .map_err(|error| error.error)
        .with_context(|| {
            format!(
                "Failed to atomically install gateway NIF {}",
                output_path.display()
            )
        })?;
    Ok(())
}

fn run_gateway_check_step(label: &str, run: impl FnOnce() -> Result<()>) -> Result<()> {
    println!("========================================");
    println!("  {label}");
    println!("========================================");
    run()?;
    println!();
    Ok(())
}

fn run_rebar(
    gateway_dir: &Path,
    args: impl IntoIterator<Item = impl Into<OsString>>,
) -> Result<()> {
    run_command(rebar_command(gateway_dir, args))
}

fn run_gateway_fmt_check(gateway_dir: &Path) -> Result<()> {
    ensure!(
        command_succeeds(with_asdf_shims(CommandSpec::new("rebar3").arg("--version"))),
        "rebar3 is required for gateway formatting"
    );
    let output = crate::common::capture(rebar_command(gateway_dir, ["fmt", "--check"]))?;
    ensure!(
        output.status == 0,
        "gateway formatting failed with exit code {}",
        output.status
    );
    Ok(())
}

fn run_eqwalizer(gateway_dir: &Path, profile: &str) -> Result<()> {
    ensure!(
        !profile.is_empty(),
        "--eqwalizer-profile requires a non-empty profile name"
    );
    ensure!(
        command_succeeds(with_asdf_shims(CommandSpec::new("elp").arg("version"))),
        "elp not found in PATH. Install ELP from https://github.com/WhatsApp/erlang-language-platform/releases"
    );
    ensure!(
        command_succeeds(with_asdf_shims(CommandSpec::new("erl").arg("-version"))),
        "erl not found in PATH"
    );
    let erlang_source = find_erlang_source()?;
    let elp_version = output_text(with_asdf_shims(CommandSpec::new("elp").arg("version")))?;
    println!("==> Running eqWAlizer with {elp_version}");
    println!("==> Using Erlang source: {erlang_source}");
    println!("==> Rebar profile: {profile}");
    run_command(
        with_asdf_shims(
            CommandSpec::new("elp")
                .args([
                    "eqwalize-all",
                    "--rebar",
                    "--as",
                    profile,
                    "--stats",
                    "--bail-on-error",
                ])
                .current_dir(gateway_dir),
        )
        .env("REBAR_SKIP_PROJECT_PLUGINS", "1"),
    )
}

fn find_erlang_source() -> Result<String> {
    output_text(with_asdf_shims(CommandSpec::new("erl").args([
        "-noshell",
        "-eval",
        concat!(
            "Root = code:root_dir(), ",
            "Matches = filelib:wildcard(filename:join([Root, \"lib\", \"erts-*\", \"src\", \"erlang.erl\"])), ",
            "case Matches of ",
            "[Path | _] -> io:format(\"~s~n\", [Path]), halt(0); ",
            "[] -> halt(2) ",
            "end."
        ),
    ])))
    .context(
        "Erlang/OTP source files are required for Eqwalizer. On Debian/Ubuntu, install erlang-src",
    )
}

fn rebar_command(
    gateway_dir: &Path,
    args: impl IntoIterator<Item = impl Into<OsString>>,
) -> CommandSpec {
    let args = args.into_iter().map(Into::into).collect::<Vec<_>>();
    let should_skip_plugins = should_skip_rebar_project_plugins(&args);
    let mut spec = with_asdf_shims(
        CommandSpec::new("rebar3")
            .args(args)
            .current_dir(gateway_dir),
    );
    if should_skip_plugins {
        spec = spec.env("REBAR_SKIP_PROJECT_PLUGINS", "1");
    }
    let current_ldflags = env::var_os("LDFLAGS");
    if let Some(ldflags) = zstd_probe_ldflags(
        env::consts::OS,
        env::consts::ARCH,
        current_ldflags.as_deref(),
    ) {
        spec = spec.env("LDFLAGS", ldflags);
    }
    spec
}

fn zstd_probe_ldflags(os: &str, arch: &str, current: Option<&OsStr>) -> Option<OsString> {
    if os != "linux" || arch != "aarch64" {
        return None;
    }
    let mut ldflags = current.unwrap_or_default().to_os_string();
    if !ldflags.is_empty() {
        ldflags.push(" ");
    }
    ldflags.push(GNU_LD_FATAL_WARNINGS);
    Some(ldflags)
}

fn should_skip_rebar_project_plugins(args: &[OsString]) -> bool {
    !args
        .iter()
        .any(|arg| matches!(arg.to_string_lossy().as_ref(), "fmt" | "lint" | "plugins"))
}

fn with_asdf_shims(spec: CommandSpec) -> CommandSpec {
    let Some(shims_path) = asdf_shims_path() else {
        return spec;
    };
    if !shims_path.is_dir() {
        return spec;
    }
    let path = env::var_os("PATH").unwrap_or_default();
    let mut paths = std::iter::once(shims_path)
        .chain(env::split_paths(&path))
        .collect::<Vec<_>>();
    let joined = env::join_paths(paths.drain(..)).unwrap_or(path);
    spec.env("PATH", joined)
}

fn asdf_shims_path() -> Option<PathBuf> {
    if let Some(asdf_data_dir) = env::var_os("ASDF_DATA_DIR") {
        return Some(PathBuf::from(asdf_data_dir).join("shims"));
    }
    env::var_os("HOME")
        .filter(|home| !home.is_empty())
        .map(|home| PathBuf::from(home).join(".asdf/shims"))
}

fn gateway_nif_builds(
    gateway_dir: &Path,
    profile: NifBuildProfile,
    dll_prefix: &str,
    dll_extension: &str,
) -> Vec<GatewayNifBuild> {
    GATEWAY_NIF_CRATES
        .iter()
        .map(|crate_name| {
            gateway_nif_build(gateway_dir, profile, dll_prefix, dll_extension, crate_name)
        })
        .collect()
}

fn gateway_nif_build(
    gateway_dir: &Path,
    profile: NifBuildProfile,
    dll_prefix: &str,
    dll_extension: &str,
    crate_name: &str,
) -> GatewayNifBuild {
    let native_dir = gateway_dir.join("native").join(crate_name);
    GatewayNifBuild {
        crate_name: crate_name.to_string(),
        cargo_args: profile.cargo_args(),
        artifact_path: native_dir
            .join("target")
            .join(profile.target_dir_name())
            .join(format!("{dll_prefix}{crate_name}.{dll_extension}")),
        output_path: gateway_dir.join("priv").join(format!("{crate_name}.so")),
        native_dir,
    }
}

fn resolve_gateway_dir() -> Result<PathBuf> {
    let cwd = env::current_dir().context("Failed to resolve current directory")?;
    if cwd.join("rebar.config").is_file()
        && cwd.file_name().and_then(|value| value.to_str()) == Some("fluxer_gateway")
    {
        return Ok(cwd);
    }
    if cwd.join("fluxer_gateway/rebar.config").is_file() {
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

    #[test]
    fn nif_profile_defaults_to_release_and_uses_debug_for_other_values() {
        assert_eq!(
            NifBuildProfile::from_env_value(None),
            NifBuildProfile::Release
        );
        assert_eq!(
            NifBuildProfile::from_env_value(Some("release")),
            NifBuildProfile::Release
        );
        assert_eq!(
            NifBuildProfile::from_env_value(Some("debug")),
            NifBuildProfile::Debug
        );
        assert_eq!(
            NifBuildProfile::from_env_value(Some("dev")),
            NifBuildProfile::Debug
        );
    }

    #[test]
    fn gateway_nif_build_plan_matches_legacy_artifact_layout() {
        let gateway_dir = Path::new("/repo/fluxer_gateway");
        let builds = gateway_nif_builds(gateway_dir, NifBuildProfile::Release, "lib", "so");

        assert_eq!(builds.len(), 2);
        assert_eq!(
            builds[0],
            GatewayNifBuild {
                crate_name: "push_markdown_plaintext_nif".to_string(),
                native_dir: PathBuf::from(
                    "/repo/fluxer_gateway/native/push_markdown_plaintext_nif"
                ),
                cargo_args: vec![OsString::from("build"), OsString::from("--release")],
                artifact_path: PathBuf::from(
                    "/repo/fluxer_gateway/native/push_markdown_plaintext_nif/target/release/libpush_markdown_plaintext_nif.so"
                ),
                output_path: PathBuf::from(
                    "/repo/fluxer_gateway/priv/push_markdown_plaintext_nif.so"
                ),
            }
        );
    }

    #[test]
    fn debug_build_plan_omits_release_arg_and_uses_debug_target_dir() {
        let build = gateway_nif_build(
            Path::new("/repo/fluxer_gateway"),
            NifBuildProfile::Debug,
            "lib",
            "dylib",
            "guild_member_list_oset_nif",
        );

        assert_eq!(build.cargo_args, vec![OsString::from("build")]);
        assert_eq!(
            build.artifact_path,
            PathBuf::from(
                "/repo/fluxer_gateway/native/guild_member_list_oset_nif/target/debug/libguild_member_list_oset_nif.dylib"
            )
        );
        assert_eq!(
            build.output_path,
            PathBuf::from("/repo/fluxer_gateway/priv/guild_member_list_oset_nif.so")
        );
    }

    #[test]
    fn rebar_project_plugins_are_skipped_except_for_plugin_commands() {
        assert!(should_skip_rebar_project_plugins(&[OsString::from(
            "compile"
        )]));
        assert!(should_skip_rebar_project_plugins(&[
            OsString::from("as"),
            OsString::from("test"),
            OsString::from("eunit"),
        ]));
        assert!(!should_skip_rebar_project_plugins(&[OsString::from("fmt")]));
        assert!(!should_skip_rebar_project_plugins(&[OsString::from(
            "lint"
        )]));
        assert!(!should_skip_rebar_project_plugins(&[OsString::from(
            "plugins"
        )]));
    }

    #[test]
    fn rebar_command_runs_in_gateway_dir_and_sets_skip_env_for_compile() {
        let command = rebar_command(Path::new("/repo/fluxer_gateway"), ["compile"]);

        assert_eq!(command.program, OsString::from("rebar3"));
        assert_eq!(command.args, vec![OsString::from("compile")]);
        assert_eq!(command.cwd, Some(PathBuf::from("/repo/fluxer_gateway")));
        assert!(command.env.contains(&(
            OsString::from("REBAR_SKIP_PROJECT_PLUGINS"),
            OsString::from("1")
        )));
    }

    #[test]
    fn rebar_command_keeps_project_plugins_for_fmt() {
        let command = rebar_command(Path::new("/repo/fluxer_gateway"), ["fmt", "--check"]);

        assert_eq!(
            command.args,
            vec![OsString::from("fmt"), OsString::from("--check")]
        );
        assert!(
            !command
                .env
                .iter()
                .any(|(key, _)| key == &OsString::from("REBAR_SKIP_PROJECT_PLUGINS"))
        );
    }

    #[test]
    fn zstd_probe_linker_warnings_are_fatal_only_on_aarch64_linux() {
        assert_eq!(
            zstd_probe_ldflags("linux", "aarch64", None),
            Some(OsString::from(GNU_LD_FATAL_WARNINGS))
        );
        assert_eq!(
            zstd_probe_ldflags("linux", "aarch64", Some(OsStr::new("-Wl,--as-needed"))),
            Some(OsString::from("-Wl,--as-needed -Wl,--fatal-warnings"))
        );
        assert_eq!(zstd_probe_ldflags("linux", "x86_64", None), None);
        assert_eq!(zstd_probe_ldflags("macos", "aarch64", None), None);
    }
}
