// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{api::types::AdminResolvedUser, utils::bigint::format_discriminator};

pub mod account;
pub mod applications;
pub mod archives;
pub mod dm_history;
pub mod group_dm;
pub mod guilds;
pub mod moderation;
pub mod overview;
pub mod relationships;
pub mod reports;
pub mod settings;

pub(super) fn resolved_user_display(user: &AdminResolvedUser) -> String {
    let disc = format_discriminator(&user.discriminator);
    let tag = format!("{}#{}", user.username, disc);
    match &user.global_name {
        Some(gn) if !gn.trim().is_empty() => format!("{} ({})", gn, tag),
        _ => tag,
    }
}
