// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::flags::Permissions;
use crate::ids::{ChannelId, UserId};
use crate::shared::{PermsError, View};
use crate::views::Scope;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ThreadAccess {
    pub private: bool,
    pub member: bool,
    pub timed_out: bool,
}

impl View<'_> {
    pub fn thread_permissions(
        &self,
        user: UserId,
        parent: ChannelId,
        access: ThreadAccess,
    ) -> Result<Permissions, PermsError> {
        if self.model.channels.resolve(parent).is_none() {
            return Ok(Permissions::empty());
        }
        let parent_bits = self.permissions(user, Scope::Channel(parent))?;
        let bits = if parent_bits == Permissions::all() {
            parent_bits
        } else if parent_bits.contains(Permissions::SEND_MESSAGES_IN_THREADS) {
            parent_bits | Permissions::SEND_MESSAGES
        } else {
            parent_bits & !Permissions::SEND_MESSAGES
        };
        let moderator = bits == Permissions::all()
            || bits.contains(Permissions::ADMINISTRATOR)
            || (bits.contains(Permissions::MANAGE_THREADS) && !access.timed_out);
        let keeps_view = bits.contains(Permissions::VIEW_CHANNEL)
            && (!access.private || moderator || access.member);
        if keeps_view {
            Ok(bits)
        } else {
            Ok(bits & !Permissions::VIEW_CHANNEL)
        }
    }
}
