// SPDX-License-Identifier: AGPL-3.0-or-later

use bitflags::bitflags;

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
    pub struct Permissions: u64 {
        const ADMINISTRATOR = 1 << 3;
        const MANAGE_CHANNELS = 1 << 4;
        const VIEW_AUDIT_LOG = 1 << 7;
        const STREAM = 1 << 9;
        const VIEW_CHANNEL = 1 << 10;
        const SEND_MESSAGES = 1 << 11;
        const READ_MESSAGE_HISTORY = 1 << 16;
        const CONNECT = 1 << 20;
        const SPEAK = 1 << 21;
        const MANAGE_ROLES = 1 << 28;
        const MANAGE_THREADS = 1 << 34;
        const SEND_MESSAGES_IN_THREADS = 1 << 38;
        const VIEW_CHANNEL_MEMBERS = 1 << 54;
        const _ = !0;
    }
}

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
    pub struct Visible: u8 {
        const VIEW = 1;
        const VIEW_MEMBERS = 2;
        const HISTORY = 4;
    }
}

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
    pub struct MemberFlags: u8 {
        const QUARANTINED = 1;
        const EXCEPTION = 2;
    }
}

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
    pub struct ChannelFlags: u8 {
        const NAMED = 1;
        const DENIES_VIEW = 2;
    }
}

impl Visible {
    pub fn from_permissions(permissions: Permissions) -> Visible {
        let mut visible = Visible::empty();
        visible.set(
            Visible::VIEW,
            permissions.contains(Permissions::VIEW_CHANNEL),
        );
        visible.set(
            Visible::VIEW_MEMBERS,
            permissions.contains(Permissions::VIEW_CHANNEL_MEMBERS),
        );
        visible.set(
            Visible::HISTORY,
            permissions.contains(Permissions::READ_MESSAGE_HISTORY),
        );
        visible
    }
}
