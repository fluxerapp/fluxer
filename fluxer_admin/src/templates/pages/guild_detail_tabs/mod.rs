// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod archives;
pub mod audit_log;
pub mod emojis;
pub mod features;
pub mod members;
pub mod moderation;
pub mod overview;
pub mod reports;
pub mod settings;
pub mod threads;

use crate::{api::types::GuildDetailInfo, utils::user_tag::display_with_tag};

pub(crate) fn owner_display(guild: &GuildDetailInfo) -> String {
    let (Some(username), Some(discriminator)) = (
        guild.owner_username.as_deref(),
        guild.owner_discriminator.as_deref(),
    ) else {
        return guild.owner_id.clone();
    };
    display_with_tag(
        guild.owner_global_name.as_deref(),
        username,
        discriminator,
        false,
    )
}
