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

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum CustomBadgeType {
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
pub struct AdminCustomBadgeList {
    pub badges: Vec<AdminCustomBadge>,
}

impl AdminCustomBadgeList {
    pub fn of_type(&self, badge_type: CustomBadgeType) -> impl Iterator<Item = &AdminCustomBadge> {
        self.badges
            .iter()
            .filter(move |badge| badge.badge_type == badge_type)
    }
}

pub struct CustomBadgeFields {
    pub name: String,
    pub description: Option<String>,
    pub href: Option<String>,
    pub image: Option<String>,
}
