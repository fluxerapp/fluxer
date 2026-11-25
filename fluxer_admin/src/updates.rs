// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{acl, config::AdminConfig};
use serde::{Deserialize, Deserializer};
use time::{OffsetDateTime, format_description::well_known::Rfc3339};

pub const DOCS_URL: &str = "https://docs.fluxer.com/operator/upgrades/";
pub const REQUIRED_ACLS: &[&str] = &[acl::INSTANCE_CONFIG_VIEW];

#[derive(Clone, Debug, Default, Deserialize)]
#[serde(default)]
pub struct Release {
    pub version: String,
    pub date: String,
}

#[derive(Clone, Debug, Default, Deserialize)]
#[serde(default)]
pub struct Report {
    pub running: String,
    pub method: String,
    pub outdated: bool,
    pub enabled: bool,
    pub url: String,
    pub checked_at: String,
    pub failed_at: String,
    pub failures: u32,
    pub error: String,
    pub latest: String,
    #[serde(deserialize_with = "null_as_empty")]
    pub newer: Vec<Release>,
    pub command: String,
    pub then: String,
    pub backup: String,
}

fn null_as_empty<'de, D, T>(deserializer: D) -> Result<Vec<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    Ok(Option::<Vec<T>>::deserialize(deserializer)?.unwrap_or_default())
}

impl Report {
    pub fn newest(&self) -> Option<&Release> {
        self.newer.first()
    }

    pub fn available(&self) -> bool {
        self.outdated || !self.newer.is_empty()
    }
}

pub fn parse(data: &str) -> Option<Report> {
    serde_json::from_str(data).ok()
}

pub fn visible(config: &AdminConfig, acls: &[String]) -> Option<Report> {
    if !config.self_hosted || !acl::has_any_permission(acls, REQUIRED_ACLS) {
        return None;
    }
    let path = config.update_state_path.as_deref()?;
    parse(&std::fs::read_to_string(path).ok()?)
}

fn timestamp(value: &str) -> Option<OffsetDateTime> {
    OffsetDateTime::parse(value, &Rfc3339).ok()
}

pub fn day(value: &str) -> Option<String> {
    let t = timestamp(value)?.to_offset(time::UtcOffset::UTC);
    Some(format!("{} {} {}", t.day(), t.month(), t.year()))
}

pub fn span(value: &str, now: OffsetDateTime) -> Option<String> {
    let minutes = (now - timestamp(value)?).whole_minutes().max(1);
    let (n, unit) = match minutes {
        1..60 => (minutes, "minute"),
        60..2880 => (minutes / 60, "hour"),
        _ => (minutes / 1440, "day"),
    };
    Some(if n == 1 {
        format!("1 {unit}")
    } else {
        format!("{n} {unit}s")
    })
}

pub fn ago(value: &str, now: OffsetDateTime) -> Option<String> {
    if (now - timestamp(value)?).whole_minutes() < 1 {
        return Some("just now".to_owned());
    }
    span(value, now).map(|s| format!("{s} ago"))
}
