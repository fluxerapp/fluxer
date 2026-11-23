// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::flags::Status;
use crate::ids::{ChannelId, SessionKey, UserId};
use crate::ranges::Ranges;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Change {
    Changed,
    Unchanged,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Timing {
    Immediate,
    Deferred,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Connection {
    Connected { presence: Change },
    Disconnected,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Updates {
    UpdatesOn,
    UpdatesOff,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum SyncScope {
    AllLists,
    Channel(ChannelId),
    NoLists,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Event<P> {
    MemberUpsert {
        user: UserId,
        display_name: Box<[u8]>,
        change: Change,
    },
    MemberRemove(UserId),
    Presence {
        user: UserId,
        status: Status,
        change: Change,
        timing: Timing,
    },
    PresenceCleared(UserId),
    Connection(UserId, Connection),
    Permissions(SyncScope),
    Subscribe {
        session: SessionKey,
        pid: P,
        user: UserId,
        channel: ChannelId,
        ranges: Ranges,
        sync: Ranges,
    },
    Unsubscribe {
        session: SessionKey,
        channel: ChannelId,
    },
    DropSession(SessionKey),
    ViewGained {
        session: SessionKey,
        channel: ChannelId,
    },
    Flush,
    Updates(Updates),
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Snapshot {
    pub members: Vec<(UserId, Box<[u8]>)>,
    pub statuses: Vec<(UserId, Status)>,
    pub connected: Vec<UserId>,
    pub updates_off: bool,
}
