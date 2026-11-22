// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::channels::ChannelSpec;
use crate::ids::{ChannelId, GuildId, RoleId, UserId};
use crate::roles::RoleSpec;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Cutoff {
    Missing,
    AtMillis(i64),
    Malformed,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ChannelCause {
    Created,
    Updated,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum RoleList {
    Roles(Vec<RoleId>),
    Malformed,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Event {
    Roles(Vec<RoleSpec>),
    Owner(Option<UserId>),
    ChannelUpsert(ChannelSpec, ChannelCause),
    ChannelDelete(ChannelId),
    ChannelOrder(Vec<ChannelId>),
    MemberUpsert(UserId, RoleList),
    MemberRemove(UserId),
    VirtualAccess(UserId, Vec<ChannelId>),
    HistoryCutoff(Cutoff),
    GuildNsfw(Option<bool>, u8),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Snapshot {
    pub guild: GuildId,
    pub owner: Option<UserId>,
    pub roles: Vec<RoleSpec>,
    pub channels: Vec<ChannelSpec>,
    pub members: Vec<(UserId, RoleList)>,
    pub virtual_access: Vec<(UserId, Vec<ChannelId>)>,
    pub cutoff: Cutoff,
    pub nsfw: Option<bool>,
    pub nsfw_level: u8,
}

impl Snapshot {
    pub fn new(guild: GuildId, owner: Option<UserId>) -> Snapshot {
        Snapshot {
            guild,
            owner,
            roles: Vec::new(),
            channels: Vec::new(),
            members: Vec::new(),
            virtual_access: Vec::new(),
            cutoff: Cutoff::Missing,
            nsfw: None,
            nsfw_level: 0,
        }
    }
}
