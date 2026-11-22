// SPDX-License-Identifier: AGPL-3.0-or-later

use bitflags::bitflags;

pub use perm_core::Visible;

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
    pub struct MemberFlags: u8 {
        const LIVE = 1;
        const CONNECTED = 2;
        const ONLINE = 4;
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum Status {
    Online,
    Idle,
    Dnd,
    Invisible,
    #[default]
    Offline,
    Other,
}

impl Status {
    pub const fn counts_as_online(self) -> bool {
        !matches!(self, Status::Invisible | Status::Offline)
    }

    pub fn shown_online(self, connected: bool) -> bool {
        connected && self.counts_as_online()
    }
}
