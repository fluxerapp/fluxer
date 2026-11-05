// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod service;
pub mod store;
pub mod verifier;

use fluxer_svc::config::optional_env;
use serde::{Deserialize, Serialize};

const APP_IDS_ENV: &str = "FLUXER_APP_ATTEST_APP_IDS";
const ALLOW_DEVELOPMENT_ENV: &str = "FLUXER_APP_ATTEST_ALLOW_DEVELOPMENT";
const TEAM_ID_LEN: usize = 10;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct AppAttestConfig {
    pub app_ids: Vec<String>,
    pub allow_development: bool,
}

impl AppAttestConfig {
    pub fn from_env() -> anyhow::Result<Self> {
        Self::parse(
            optional_env(APP_IDS_ENV).as_deref(),
            optional_env(ALLOW_DEVELOPMENT_ENV).as_deref(),
        )
    }

    pub fn parse(app_ids: Option<&str>, allow_development: Option<&str>) -> anyhow::Result<Self> {
        let app_ids = app_ids
            .unwrap_or_default()
            .split(',')
            .map(str::trim)
            .filter(|app_id| !app_id.is_empty())
            .map(|app_id| {
                if is_valid_app_id(app_id) {
                    Ok(app_id.to_owned())
                } else {
                    Err(anyhow::anyhow!(
                        "{APP_IDS_ENV} entry {app_id:?} is not TEAMID.bundle.id"
                    ))
                }
            })
            .collect::<anyhow::Result<Vec<_>>>()?;
        let allow_development = match allow_development
            .map(|value| value.trim().to_ascii_lowercase())
            .as_deref()
        {
            None | Some("" | "0" | "false" | "no" | "off") => false,
            Some("1" | "true" | "yes" | "on") => true,
            Some(other) => anyhow::bail!("{ALLOW_DEVELOPMENT_ENV} has an invalid value {other:?}"),
        };
        Ok(Self {
            app_ids,
            allow_development,
        })
    }

    pub fn is_enabled(&self) -> bool {
        !self.app_ids.is_empty()
    }
}

fn is_valid_app_id(app_id: &str) -> bool {
    let Some((team_id, bundle_id)) = app_id.split_once('.') else {
        return false;
    };
    team_id.len() == TEAM_ID_LEN
        && team_id
            .bytes()
            .all(|byte| byte.is_ascii_uppercase() || byte.is_ascii_digit())
        && !bundle_id.is_empty()
        && bundle_id
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'.' || byte == b'-')
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Environment {
    Development,
    Production,
}

impl Environment {
    pub fn as_str(self) -> &'static str {
        match self {
            Environment::Development => "development",
            Environment::Production => "production",
        }
    }

    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "development" => Some(Environment::Development),
            "production" => Some(Environment::Production),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ChallengePurpose {
    Attest,
    Assert,
}

impl ChallengePurpose {
    pub fn as_str(self) -> &'static str {
        match self {
            ChallengePurpose::Attest => "attest",
            ChallengePurpose::Assert => "assert",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AgeFloor {
    Adult,
    Minor,
    UnderMinimum,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AppAttestRejection {
    NotConfigured,
    InvalidRequest,
    InvalidChallenge,
    InvalidAttestation,
    KeyInUse,
    UnknownKey,
    InvalidAssertion,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn configuration_is_off_when_no_app_id_is_set_and_rejects_typos() {
        assert!(!AppAttestConfig::parse(None, None).unwrap().is_enabled());
        assert!(
            !AppAttestConfig::parse(Some(" , "), None)
                .unwrap()
                .is_enabled()
        );
        let config = AppAttestConfig::parse(
            Some("3G5837T29K.com.fluxer, 3G5837T29K.com.fluxer.canary"),
            Some("true"),
        )
        .unwrap();
        assert_eq!(
            config.app_ids,
            vec!["3G5837T29K.com.fluxer", "3G5837T29K.com.fluxer.canary"]
        );
        assert!(config.allow_development);
        assert!(
            !AppAttestConfig::parse(Some("3G5837T29K.com.fluxer"), Some(""))
                .unwrap()
                .allow_development
        );
        assert!(AppAttestConfig::parse(Some("com.fluxer"), None).is_err());
        assert!(AppAttestConfig::parse(Some("3g5837t29k.com.fluxer"), None).is_err());
        assert!(AppAttestConfig::parse(Some("3G5837T29K.com.fluxer"), Some("maybe")).is_err());
    }
}
