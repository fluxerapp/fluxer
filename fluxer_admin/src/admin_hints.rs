// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::components::tooltip::{Hint, HintLink};

/// Hints for the User Flags
pub fn u64_flag_hint(_value: u64) -> Option<Hint<'static>> {
    None
}

/// Hints for the limit fields on the limit configuration page
pub fn limit_key_hint(key: &str) -> Option<Hint<'static>> {
    match key {
        "feature_guild_create" => Some(Hint {
            name: Some("Community Creation Access"),
            body: "Only relevant while the Community Creation policy is disabled and for non-admins. \
						Add a rule that grants this to let users of your choosing create communities.",
            link: Some(HintLink::new(
                "/instance-config#community-creation",
                " Community Creation policy",
            )),
        }),
        _ => None,
    }
}
