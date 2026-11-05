// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

pub const BADGE_IMAGE_MAX_BYTES: usize = 10 * 1024 * 1024;

pub const BADGE_IMAGE_TYPES: &[&str] = &[
    "image/png",
    "image/jpeg",
    "image/gif",
    "image/webp",
    "image/svg+xml",
];

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum CustomBadgeType {
    #[default]
    User,
    Guild,
}

impl CustomBadgeType {
    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "user" => Some(Self::User),
            "guild" => Some(Self::Guild),
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::User => "user",
            Self::Guild => "guild",
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AdminCustomBadge {
    pub id: String,
    #[serde(rename = "type")]
    pub badge_type: CustomBadgeType,
    #[serde(default)]
    pub position: String,
    pub name: String,
    pub description: Option<String>,
    pub asset_url: String,
    pub href: Option<String>,
}

impl AdminCustomBadge {
    pub fn image_url(&self) -> String {
        format!("{}?size=96", self.asset_url)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BuiltInBadgeImage {
    pub kind: String,
    pub asset_url: Option<String>,
    #[serde(default)]
    pub built_in_asset: Option<String>,
    #[serde(default)]
    pub hidden: bool,
}

impl BuiltInBadgeImage {
    pub fn image_url(&self) -> Option<String> {
        self.asset_url.as_ref().map(|url| format!("{url}?size=96"))
    }

    pub fn badge_type(&self) -> CustomBadgeType {
        if self.kind.starts_with("guild_") {
            CustomBadgeType::Guild
        } else {
            CustomBadgeType::User
        }
    }

    pub fn label(&self) -> &str {
        match self.kind.as_str() {
            "staff" => "Staff",
            "premium" => "Premium",
            "guild_verified" => "Verified",
            "guild_partnered" => "Partnered",
            "guild_discoverable" => "Discoverable",
            other => other,
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct BadgeSettings {
    pub max_per_user: u32,
    pub max_per_guild: u32,
}

impl Default for BadgeSettings {
    fn default() -> Self {
        Self {
            max_per_user: 100,
            max_per_guild: 50,
        }
    }
}

pub const BADGES_PER_USER_CEILING: u32 = 1000;
pub const BADGES_PER_GUILD_CEILING: u32 = 200;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AdminCustomBadgeList {
    pub badges: Vec<AdminCustomBadge>,
    #[serde(default)]
    pub next: Option<String>,
    #[serde(default)]
    pub settings: BadgeSettings,
    #[serde(default)]
    pub built_in: Vec<BuiltInBadgeImage>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BadgeMove {
    Up,
    Down,
    First,
    Last,
}

impl BadgeMove {
    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "up" => Some(Self::Up),
            "down" => Some(Self::Down),
            "first" => Some(Self::First),
            "last" => Some(Self::Last),
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Up => "up",
            Self::Down => "down",
            Self::First => "first",
            Self::Last => "last",
        }
    }

    pub fn label(self) -> &'static str {
        match self {
            Self::Up => "Move up",
            Self::Down => "Move down",
            Self::First => "Move to top",
            Self::Last => "Move to bottom",
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct BadgeListQuery {
    pub badge_type: Option<CustomBadgeType>,
    pub query: Option<String>,
    pub after: Option<String>,
    pub limit: Option<u32>,
}

impl AdminCustomBadgeList {
    pub fn built_in_of_type(
        &self,
        badge_type: CustomBadgeType,
    ) -> impl Iterator<Item = &BuiltInBadgeImage> {
        self.built_in
            .iter()
            .filter(move |badge| badge.badge_type() == badge_type)
    }
}

#[derive(Clone, Debug, Default)]
pub struct BadgeAssignment {
    pub assigned: Vec<AdminCustomBadge>,
    pub picked: Vec<AdminCustomBadge>,
    pub candidates: Vec<AdminCustomBadge>,
    pub more_candidates: bool,
    pub search_incomplete: bool,
    pub query: Option<String>,
    pub limit: u32,
}

pub struct CustomBadgeFields {
    pub name: String,
    pub description: Option<String>,
    pub href: Option<String>,
    pub image: Option<String>,
}
