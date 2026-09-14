// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{
    CALVER_SCHEME, CalverEnv, append_github_env, append_github_output, micro_segment,
    month_day_segment, parse_version_instant, resolve_calver, trim_option,
};
use anyhow::Result;
use chrono::{Datelike, Timelike, Utc};
use clap::Args;
use std::env;

#[derive(Debug, Args, Clone)]
pub struct ResolveCalverArgs {
    #[arg(long)]
    github_output: bool,
    #[arg(long)]
    github_env: bool,
    #[arg(long, default_value = "BUILD_VERSION")]
    env_name: String,
}

pub fn run(args: ResolveCalverArgs) -> Result<()> {
    let resolved = resolve_calver_from_env()?;
    if args.github_output {
        let output = calver_outputs(&resolved)?;
        append_github_output(&[
            ("version", output.version.as_str()),
            ("build_version", output.version.as_str()),
            ("time", output.time.as_str()),
            ("micro", output.micro.as_str()),
            ("patch", output.micro.as_str()),
            ("date", output.date.as_str()),
            ("year", output.year.as_str()),
            ("month", output.month.as_str()),
            ("day", output.day.as_str()),
            ("month_day", output.month_day.as_str()),
            ("calver_scheme", CALVER_SCHEME),
        ])?;
    }
    if args.github_env {
        append_github_env(&[(args.env_name.as_str(), resolved.as_str())])?;
    }
    println!("{resolved}");
    Ok(())
}

fn resolve_calver_from_env() -> Result<String> {
    resolve_calver(
        &CalverEnv {
            build_version: trim_option(env::var("BUILD_VERSION").ok()),
            fluxer_build_version: trim_option(env::var("FLUXER_BUILD_VERSION").ok()),
            fluxer_build_date: trim_option(env::var("FLUXER_BUILD_DATE").ok()),
        },
        Utc::now(),
    )
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CalverOutputs {
    version: String,
    time: String,
    micro: String,
    date: String,
    year: String,
    month: String,
    day: String,
    month_day: String,
}

fn calver_outputs(version: &str) -> Result<CalverOutputs> {
    let instant = parse_version_instant(version)?;
    let time = format!(
        "{:02}{:02}{:02}",
        instant.hour(),
        instant.minute(),
        instant.second()
    );
    Ok(CalverOutputs {
        version: version.to_string(),
        time,
        micro: micro_segment(instant),
        date: format!(
            "{:04}{:02}{:02}",
            instant.year(),
            instant.month(),
            instant.day()
        ),
        year: instant.year().to_string(),
        month: instant.month().to_string(),
        day: format!("{:02}", instant.day()),
        month_day: month_day_segment(instant),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::CalverEnv;
    use chrono::{TimeZone, Utc};

    #[test]
    fn calver_outputs_match_legacy_shell_fields() {
        assert_eq!(
            calver_outputs("2026.520.10203").unwrap(),
            CalverOutputs {
                version: "2026.520.10203".to_string(),
                time: "010203".to_string(),
                micro: "10203".to_string(),
                date: "20260520".to_string(),
                year: "2026".to_string(),
                month: "5".to_string(),
                day: "20".to_string(),
                month_day: "520".to_string(),
            }
        );
    }

    #[test]
    fn calver_date_only_override_matches_legacy_shell() {
        let version = resolve_calver(
            &CalverEnv {
                fluxer_build_date: Some("2026-01-09".to_string()),
                ..CalverEnv::default()
            },
            Utc.with_ymd_and_hms(2026, 5, 20, 1, 2, 3).single().unwrap(),
        )
        .unwrap();

        assert_eq!(version, "2026.109.0");
    }

    #[test]
    fn calver_rejects_invalid_time() {
        assert_eq!(
            calver_outputs("2026.520.246000").unwrap_err().to_string(),
            "Invalid build version date/time: 2026.520.246000"
        );
    }
}
