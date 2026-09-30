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
        _ => None,
    }
}
