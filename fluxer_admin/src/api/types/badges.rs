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
pub struct BuiltInBadgeImage {
    pub kind: String,
    pub asset_url: Option<String>,
    #[serde(default)]
    pub built_in_asset: Option<String>,
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

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AdminCustomBadgeList {
    pub badges: Vec<AdminCustomBadge>,
    #[serde(default)]
    pub built_in: Vec<BuiltInBadgeImage>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BadgeMove {
    Up,
    Down,
}

impl BadgeMove {
    pub fn parse(value: &str) -> Option<Self> {
        match value {
            "up" => Some(Self::Up),
            "down" => Some(Self::Down),
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Up => "up",
            Self::Down => "down",
        }
    }
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

    pub fn of_type(&self, badge_type: CustomBadgeType) -> impl Iterator<Item = &AdminCustomBadge> {
        self.badges
            .iter()
            .filter(move |badge| badge.badge_type == badge_type)
    }

    pub fn moved_order(&self, badge_id: &str, direction: BadgeMove) -> Option<Vec<String>> {
        let index = self.badges.iter().position(|badge| badge.id == badge_id)?;
        let badge_type = self.badges[index].badge_type;
        let same_type = |candidate: &AdminCustomBadge| candidate.badge_type == badge_type;
        let neighbour = match direction {
            BadgeMove::Up => self.badges[..index].iter().rposition(same_type)?,
            BadgeMove::Down => index + 1 + self.badges[index + 1..].iter().position(same_type)?,
        };
        let mut order: Vec<String> = self.badges.iter().map(|badge| badge.id.clone()).collect();
        order.swap(index, neighbour);
        Some(order)
    }
}

pub struct CustomBadgeFields {
    pub name: String,
    pub description: Option<String>,
    pub href: Option<String>,
    pub image: Option<String>,
}
