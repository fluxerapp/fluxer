// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::components::tooltip::{Hint, HintLink};

pub fn limit_key_hint(key: &str) -> Option<Hint<'static>> {
    match key {
        "feature_guild_create" => Some(Hint {
            name: Some("Community Creation Access"),
            body: "Admins with the wildcard ACL can always create communities.",
            link: Some(HintLink::new(
                "/instance-config#community-creation",
                "Community creation policy",
            )),
        }),
        "max_screen_share_bitrate" => Some(Hint {
            name: Some("Max Screen Share Bitrate"),
            body: "In bits per second, so 9000000 is 9 Mbps. Raising it also raises the automatic bitrate of every screen share quality. Accounts with Higher Video Quality can pick their own bitrate up to this value.",
            link: None,
        }),
        _ => None,
    }
}
