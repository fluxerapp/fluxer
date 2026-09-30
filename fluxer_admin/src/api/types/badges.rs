// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Badge {
    pub id: String,
    #[serde(rename = "type")]
    pub badge_type: String,
    pub name: String,
    pub tooltip: String,
    pub icon: String,
    pub url: Option<String>,
    pub position: i64,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct BuiltinBadgeIcons {
    pub staff: Option<String>,
    pub premium: Option<String>,
    pub verified: Option<String>,
    pub partnered: Option<String>,
    pub discoverable: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListBadgesResponse {
    pub version: i64,
    #[serde(default)]
    pub badges: Vec<Badge>,
    #[serde(default)]
    pub builtin_icons: BuiltinBadgeIcons,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BadgeMutationResponse {
    pub badge: Badge,
}
