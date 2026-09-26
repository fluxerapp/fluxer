// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod applications;
pub mod archives;
pub mod audit_log;
pub mod emojis;
pub mod features;
pub mod members;
pub mod moderation;
pub mod overview;
pub mod reports;
pub mod settings;
pub mod stickers;

use crate::api::types::GuildDetailInfo;

pub(crate) fn owner_display(guild: &GuildDetailInfo) -> String {
    let Some(username) = guild.owner_username.as_deref() else {
        return guild.owner_id.clone();
    };
    let Some(discriminator) = guild.owner_discriminator.as_deref() else {
        return guild.owner_id.clone();
    };
    let tag = format!("{username}#{discriminator}");
    if let Some(global_name) = guild
        .owner_global_name
        .as_deref()
        .filter(|name| !name.trim().is_empty())
    {
        format!("{global_name} ({tag})")
    } else {
        tag
    }
}
