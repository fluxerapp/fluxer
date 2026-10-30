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

#[cfg(test)]
mod tests {
    use super::*;

    fn badge(id: &str, badge_type: CustomBadgeType) -> AdminCustomBadge {
        AdminCustomBadge {
            id: id.to_owned(),
            badge_type,
            name: id.to_owned(),
            description: None,
            asset_url: String::new(),
            href: None,
        }
    }

    fn ids(values: &[&str]) -> Option<Vec<String>> {
        Some(values.iter().map(|value| (*value).to_owned()).collect())
    }

    fn list() -> AdminCustomBadgeList {
        AdminCustomBadgeList {
            badges: vec![
                badge("1", CustomBadgeType::User),
                badge("2", CustomBadgeType::Guild),
                badge("3", CustomBadgeType::User),
                badge("4", CustomBadgeType::Guild),
                badge("5", CustomBadgeType::User),
            ],
        }
    }

    #[test]
    fn moved_order_swaps_with_the_nearest_badge_of_the_same_type() {
        let badges = list();
        assert_eq!(
            ids(&["3", "2", "1", "4", "5"]),
            badges.moved_order("3", BadgeMove::Up)
        );
        assert_eq!(
            ids(&["1", "4", "3", "2", "5"]),
            badges.moved_order("2", BadgeMove::Down)
        );
        assert_eq!(
            ids(&["1", "2", "5", "4", "3"]),
            badges.moved_order("3", BadgeMove::Down)
        );
    }

    #[test]
    fn moved_order_refuses_the_ends_and_unknown_badges() {
        let badges = list();
        assert_eq!(None, badges.moved_order("1", BadgeMove::Up));
        assert_eq!(None, badges.moved_order("2", BadgeMove::Up));
        assert_eq!(None, badges.moved_order("5", BadgeMove::Down));
        assert_eq!(None, badges.moved_order("4", BadgeMove::Down));
        assert_eq!(None, badges.moved_order("9", BadgeMove::Up));
    }
}
