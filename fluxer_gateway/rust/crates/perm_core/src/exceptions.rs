// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;

use smallvec::SmallVec;

use crate::channels::ChannelTable;
use crate::ids::{ChannelId, UserId};

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct Exceptions {
    users: Vec<UserId>,
    member_targets: Vec<UserId>,
}

impl Exceptions {
    pub(crate) fn build(
        owner: Option<UserId>,
        channels: &ChannelTable,
        virtual_view: &VirtualView,
        quarantined: impl IntoIterator<Item = UserId>,
    ) -> Exceptions {
        let mut member_targets: Vec<UserId> = channels
            .live()
            .flat_map(|(_, c)| c.rules().members().iter().map(|(u, _)| *u))
            .collect();
        member_targets.sort_unstable();
        member_targets.dedup();
        let mut users = member_targets.clone();
        users.extend(owner);
        users.extend(virtual_view.users());
        users.extend(quarantined);
        users.sort_unstable();
        users.dedup();
        Exceptions {
            users,
            member_targets,
        }
    }

    pub fn users(&self) -> &[UserId] {
        &self.users
    }

    pub fn member_targets(&self) -> &[UserId] {
        &self.member_targets
    }

    pub fn contains(&self, user: UserId) -> bool {
        self.users.binary_search(&user).is_ok()
    }

    pub fn len(&self) -> usize {
        self.users.len()
    }

    pub fn is_empty(&self) -> bool {
        self.users.is_empty()
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct VirtualView {
    by_user: BTreeMap<UserId, SmallVec<[ChannelId; 2]>>,
}

impl VirtualView {
    pub fn get(&self, user: UserId) -> &[ChannelId] {
        self.by_user.get(&user).map_or(&[], |c| c.as_slice())
    }

    pub fn has(&self, user: UserId, channel: ChannelId) -> bool {
        self.get(user).binary_search(&channel).is_ok()
    }

    pub fn users(&self) -> impl Iterator<Item = UserId> + '_ {
        self.by_user.keys().copied()
    }

    pub fn len(&self) -> usize {
        self.by_user.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_user.is_empty()
    }

    pub(crate) fn set(&mut self, user: UserId, channels: &[ChannelId]) {
        if channels.is_empty() {
            self.by_user.remove(&user);
            return;
        }
        let mut list: SmallVec<[ChannelId; 2]> = SmallVec::from_slice(channels);
        list.sort_unstable();
        list.dedup();
        self.by_user.insert(user, list);
    }
}
