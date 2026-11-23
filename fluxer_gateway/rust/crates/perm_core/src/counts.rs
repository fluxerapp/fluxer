// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;

use crate::bits;
use crate::budget::Meter;
use crate::diff::Group;
use crate::everyone::{everyone_views, thread_only};
use crate::flags::{Permissions, Visible};
use crate::ids::{ChannelIdx, RoleSetId, UserId};
use crate::shared::View;
use crate::views::Scope;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PublicOnline {
    Open,
    Count(u64),
    Empty,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum MutualOnline {
    All,
    Count(u64),
}

fn count(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

fn unique(users: &[UserId]) -> Vec<UserId> {
    let mut list = users.to_vec();
    list.sort_unstable();
    list.dedup();
    list
}

impl View<'_> {
    fn shares(
        &self,
        user: UserId,
        mask: &[u64],
        memo: &mut BTreeMap<Group, bool>,
        meter: &mut Meter,
    ) -> bool {
        let Some(group) = self.group_of(user) else {
            return false;
        };
        if let Some(known) = memo.get(&group) {
            return *known;
        }
        let hit = self
            .viewable_idx(user, meter)
            .iter()
            .any(|idx| bits::test(mask, idx.index()));
        if let Group::RoleSet(_) = group {
            memo.insert(group, hit);
        }
        hit
    }

    fn mask_of(&self, list: impl Iterator<Item = ChannelIdx>) -> Vec<u64> {
        let mut mask = vec![0u64; bits::words_for(self.model.channels.slots())];
        for idx in list {
            bits::set(&mut mask, idx.index());
        }
        mask
    }

    fn target_views_any(
        &self,
        target: UserId,
        own: &[ChannelIdx],
        memo: &mut BTreeMap<RoleSetId, bool>,
        meter: &mut Meter,
    ) -> bool {
        if own.is_empty() || !self.members.contains(target) {
            return false;
        }
        let model = &self.model;
        match self.group_of(target) {
            Some(Group::RoleSet(set)) => {
                if let Some(known) = memo.get(&set) {
                    return *known;
                }
                meter.charge(count(own.len()));
                let hit = own.iter().any(|idx| {
                    model
                        .classes
                        .class_of_channel(*idx)
                        .is_some_and(|class| bits::test(class.planes().view(), set.index()))
                });
                memo.insert(set, hit);
                hit
            }
            Some(Group::User(_)) => {
                meter.charge(count(own.len()).saturating_mul(8));
                own.iter().any(|idx| {
                    model
                        .channels
                        .get(*idx)
                        .is_some_and(|c| self.visible(target, c.id()).contains(Visible::VIEW))
                })
            }
            None => false,
        }
    }

    fn shared_channels(&self, subscriber: UserId, meter: &mut Meter) -> Vec<ChannelIdx> {
        let channels = &self.model.channels;
        let mut own = self.viewable_idx(subscriber, meter);
        own.retain(|idx| channels.get(*idx).is_some_and(|c| !c.kind().is_category()));
        own
    }

    pub fn shares_view(&self, subscriber: UserId, target: UserId, meter: &mut Meter) -> bool {
        let own = self.shared_channels(subscriber, meter);
        meter.charge(8);
        self.target_views_any(target, &own, &mut BTreeMap::new(), meter)
    }

    pub fn shares_view_many(
        &self,
        subscriber: UserId,
        targets: &[UserId],
        meter: &mut Meter,
    ) -> Vec<bool> {
        let own = self.shared_channels(subscriber, meter);
        let mut memo = BTreeMap::new();
        targets
            .iter()
            .map(|target| {
                meter.charge(8);
                self.target_views_any(*target, &own, &mut memo, meter)
            })
            .collect()
    }

    pub fn public_online(&self, online: &[UserId], meter: &mut Meter) -> PublicOnline {
        let model = &self.model;
        let mut open = false;
        let mut restricted = Vec::new();
        for (idx, channel) in model.channels.in_order() {
            meter.charge(4);
            if thread_only(channel.kind()) || !everyone_views(model, idx) {
                continue;
            }
            if channel.denies_view() {
                restricted.push(idx);
            } else {
                open = true;
            }
        }
        if open {
            return PublicOnline::Open;
        }
        if restricted.is_empty() {
            return PublicOnline::Empty;
        }
        let mask = self.mask_of(restricted.into_iter());
        let mut memo = BTreeMap::new();
        let mut total = 0u64;
        for user in unique(online) {
            meter.charge(8);
            if self.shares(user, &mask, &mut memo, meter) {
                total = total.saturating_add(1);
            }
        }
        PublicOnline::Count(total)
    }

    pub fn mutual_online(
        &self,
        requester: UserId,
        online: &[UserId],
        meter: &mut Meter,
    ) -> MutualOnline {
        let sees_all = self
            .permissions(requester, Scope::Guild)
            .is_ok_and(|p| p.contains(Permissions::ADMINISTRATOR));
        if sees_all {
            return MutualOnline::All;
        }
        let users = unique(online);
        let own = self.viewable_idx(requester, meter);
        if own.is_empty() {
            let present = users.binary_search(&requester).is_ok();
            return MutualOnline::Count(u64::from(present));
        }
        let channels = &self.model.channels;
        let mask = self.mask_of(own.into_iter().filter(|idx| {
            channels
                .get(*idx)
                .is_some_and(|c| !c.kind().is_category() && !thread_only(c.kind()))
        }));
        let mut memo = BTreeMap::new();
        let mut total = 0u64;
        for user in users {
            meter.charge(8);
            if user == requester || self.shares(user, &mask, &mut memo, meter) {
                total = total.saturating_add(1);
            }
        }
        MutualOnline::Count(total)
    }
}
