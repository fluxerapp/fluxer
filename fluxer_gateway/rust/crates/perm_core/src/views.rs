// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::bits;
use crate::budget::{Meter, Units};
use crate::eval::Who;
use crate::flags::{Permissions, Visible};
use crate::ids::{ChannelId, ChannelIdx, Epoch, RoleId, RoleIdx, UserId};
use crate::shared::{PermsError, View};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Scope {
    Guild,
    Channel(ChannelId),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Read {
    Point,
    Viewable,
    Audience,
    AudienceBits,
    Viewers,
    FilterViewers(usize),
    MembersWithRole,
    Counts(usize),
    Shares(usize),
    FilterSharing(usize),
    Everyone,
    EveryoneList(usize),
}

const LOOKUP_BATCH: usize = 16;

fn count(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

impl View<'_> {
    pub(crate) fn who(&self, user: UserId) -> Who<'_> {
        Who::of(
            self.model.owner,
            &self.model.rolesets,
            user,
            self.members.get(user),
        )
    }

    pub fn estimate(&self, read: Read) -> Units {
        let channels = count(self.model.channels.slots());
        let sets = count(self.model.rolesets.len());
        let words = count(self.model.rolesets.words());
        let exceptions = count(self.model.exceptions.len());
        let members = count(self.members.len());
        let units = match read {
            Read::Point => 8,
            Read::Viewable => channels.saturating_mul(4).saturating_add(8),
            Read::Audience => words
                .saturating_add(channels.saturating_mul(2))
                .max(sets.saturating_mul(8))
                .saturating_add(exceptions.saturating_mul(8)),
            Read::AudienceBits => sets
                .saturating_mul(8)
                .saturating_add(exceptions.saturating_mul(16)),
            Read::Viewers => members.saturating_add(channels.saturating_mul(8)),
            Read::FilterViewers(n) => count(n).saturating_mul(16),
            Read::MembersWithRole => members
                .saturating_mul(3)
                .saturating_add(sets.saturating_mul(4)),
            Read::Counts(n) => count(n)
                .saturating_mul(8)
                .saturating_add(channels.saturating_mul(4))
                .saturating_add(
                    count(n)
                        .min(sets.saturating_add(exceptions))
                        .saturating_mul(channels.saturating_mul(4)),
                ),
            Read::Shares(n) => channels.saturating_mul(4).saturating_add(8).saturating_add(
                count(n).saturating_mul(8u64.saturating_add(channels.saturating_mul(8))),
            ),
            Read::FilterSharing(n) => {
                let words = count(bits::words_for(self.model.channels.len()));
                let build = channels
                    .saturating_mul(9)
                    .saturating_add(words)
                    .saturating_add(16);
                let builds = count(n)
                    .min(sets.saturating_add(exceptions).saturating_add(1))
                    .saturating_add(1);
                channels
                    .saturating_add(builds.saturating_mul(build))
                    .saturating_add(count(n).saturating_mul(words.saturating_add(8)))
            }
            Read::Everyone => channels.saturating_mul(8),
            Read::EveryoneList(n) => count(n).saturating_mul(8),
        };
        Units::new(units)
    }

    pub fn permissions(&self, user: UserId, scope: Scope) -> Result<Permissions, PermsError> {
        match self.who(user) {
            Who::Owner { .. } => Ok(Permissions::all()),
            Who::Stranger => Ok(Permissions::empty()),
            Who::Quarantined => Err(PermsError::MalformedMember(user)),
            who @ Who::Member { base, .. } => Ok(match scope {
                Scope::Guild => base.permissions(),
                Scope::Channel(id) => match self.model.channels.by_id(id) {
                    Some(channel) => who.channel_bits(user, channel),
                    None => match base {
                        crate::rolesets::Base::All => Permissions::all(),
                        crate::rolesets::Base::Bits(_) => Permissions::empty(),
                    },
                },
            }),
        }
    }

    pub fn check(&self, user: UserId, scope: Scope, bits: Permissions) -> Result<bool, PermsError> {
        if bits.is_empty() {
            return Err(PermsError::EmptyRequirement);
        }
        Ok(self.permissions(user, scope)?.contains(bits))
    }

    pub fn visible(&self, user: UserId, channel: ChannelId) -> Visible {
        match self.who(user) {
            Who::Quarantined => Visible::empty(),
            Who::Owner { .. } => Visible::all(),
            Who::Stranger => {
                if self.model.virtual_view.has(user, channel) {
                    Visible::VIEW
                } else {
                    Visible::empty()
                }
            }
            who @ Who::Member { set, base, .. } => {
                if let Some(idx) = self.model.channels.resolve(channel) {
                    return match self.model.classes.class_of_channel(idx) {
                        Some(class) => class.visible(set, user),
                        None => crate::eval::session_bits(
                            who,
                            user,
                            self.model.virtual_view.get(user),
                            &self.model.channels,
                            idx,
                            &mut Meter::new(),
                        ),
                    };
                }
                let mut visible = match base {
                    crate::rolesets::Base::All => Visible::all(),
                    crate::rolesets::Base::Bits(_) => Visible::empty(),
                };
                if self.model.virtual_view.has(user, channel) {
                    visible.insert(Visible::VIEW);
                }
                visible
            }
        }
    }

    pub fn message_access(&self, user: UserId, channel: ChannelId, message_ms: i64) -> bool {
        match self.permissions(user, Scope::Channel(channel)) {
            Ok(p) => {
                p.contains(Permissions::READ_MESSAGE_HISTORY)
                    || self.model.flags.history_open(message_ms)
            }
            Err(_) => false,
        }
    }

    pub(crate) fn viewable_idx(&self, user: UserId, meter: &mut Meter) -> Vec<ChannelIdx> {
        let Some(entry) = self.members.get(user) else {
            return Vec::new();
        };
        if entry.is_quarantined() && self.model.owner != Some(user) {
            return Vec::new();
        }
        let set = entry.roleset();
        if self.model.exceptions.contains(user) {
            self.model.viewable_by(meter, |class| {
                class.visible(set, user).contains(Visible::VIEW)
            })
        } else {
            self.model.set_viewable(set, meter).to_vec()
        }
    }

    pub fn viewable(&self, user: UserId, meter: &mut Meter) -> Vec<ChannelId> {
        let list = self.viewable_idx(user, meter);
        self.model.ids(&list)
    }

    pub fn view_set(&self, user: UserId, meter: &mut Meter) -> (Epoch, Vec<u64>) {
        let list = self.viewable_idx(user, meter);
        let channels = &self.model.channels;
        let mut position = vec![usize::MAX; channels.slots()];
        for (i, idx) in channels.order().iter().enumerate() {
            if let Some(slot) = position.get_mut(idx.index()) {
                *slot = i;
            }
        }
        let mut words = vec![0u64; bits::words_for(channels.len())];
        for idx in list {
            if let Some(p) = position.get(idx.index()).copied()
                && p != usize::MAX
            {
                bits::set(&mut words, p);
            }
        }
        (self.model.epoch, words)
    }

    pub fn viewers(&self, channel: ChannelId, meter: &mut Meter) -> Vec<UserId> {
        let mut out = Vec::new();
        meter.charge(count(self.members.len()));
        self.viewers_range(channel, 0, self.members.len(), &mut out);
        out
    }

    pub(crate) fn viewers_range(
        &self,
        channel: ChannelId,
        start: usize,
        end: usize,
        out: &mut Vec<UserId>,
    ) {
        let entries = self.members.entries().get(start..end).unwrap_or(&[]);
        let class = self
            .model
            .channels
            .resolve(channel)
            .and_then(|idx| self.model.classes.class_of_channel(idx));
        let Some(class) = class else {
            out.extend(
                entries
                    .iter()
                    .filter(|e| self.visible(e.user(), channel).contains(Visible::VIEW))
                    .map(|e| e.user()),
            );
            return;
        };
        let plane = class.planes().view();
        out.reserve(entries.len());
        for chunk in entries.chunks(64) {
            let mut mask = 0u64;
            let mut bit = 1u64;
            for entry in chunk {
                let visible = if entry.is_exception() {
                    class
                        .visible(entry.roleset(), entry.user())
                        .contains(Visible::VIEW)
                } else {
                    bits::test(plane, entry.roleset().index())
                };
                if visible {
                    mask |= bit;
                }
                bit = bit.wrapping_shl(1);
            }
            if usize::try_from(mask.count_ones()).is_ok_and(|n| n == chunk.len()) {
                out.extend(chunk.iter().map(|e| e.user()));
                continue;
            }
            while mask != 0 {
                let i = usize::try_from(mask.trailing_zeros()).unwrap_or(usize::MAX);
                if let Some(entry) = chunk.get(i) {
                    out.push(entry.user());
                }
                mask &= mask.wrapping_sub(1);
            }
        }
    }

    pub fn filter_viewers(
        &self,
        channel: ChannelId,
        users: &[UserId],
        meter: &mut Meter,
    ) -> Vec<UserId> {
        meter.charge(count(users.len()).saturating_mul(16));
        self.only_viewers(channel, users)
    }

    pub(crate) fn only_viewers(&self, channel: ChannelId, users: &[UserId]) -> Vec<UserId> {
        let class = self
            .model
            .channels
            .resolve(channel)
            .and_then(|idx| self.model.classes.class_of_channel(idx));
        let Some(class) = class else {
            return users
                .iter()
                .copied()
                .filter(|u| self.visible(*u, channel).contains(Visible::VIEW))
                .collect();
        };
        let plane = class.planes().view();
        let owner = self.model.owner;
        let sees = |user: UserId, hash: u64| {
            if owner == Some(user) {
                return true;
            }
            match self.members.get_hashed(user, hash) {
                None => self.model.virtual_view.has(user, channel),
                Some(entry) if entry.is_quarantined() => false,
                Some(entry) if self.model.rolesets.get(entry.roleset()).is_none() => false,
                Some(entry) => match class.override_of(user) {
                    Some(visible) => visible.contains(Visible::VIEW),
                    None => bits::test(plane, entry.roleset().index()),
                },
            }
        };
        let mut out = Vec::with_capacity(users.len());
        let mut hashes = [0u64; LOOKUP_BATCH];
        for chunk in users.chunks(LOOKUP_BATCH) {
            for (hash, user) in hashes.iter_mut().zip(chunk) {
                *hash = self.members.hash_of(*user);
            }
            out.extend(
                chunk
                    .iter()
                    .zip(hashes)
                    .filter(|(user, hash)| sees(**user, *hash))
                    .map(|(user, _)| *user),
            );
        }
        out
    }

    pub fn has_member(&self, user: UserId) -> bool {
        self.members.contains(user)
    }

    pub fn members_with_role(&self, role: RoleId, meter: &mut Meter) -> Vec<UserId> {
        self.role_set_members(&[role], meter)
    }

    pub(crate) fn holding_sets(&self, roles: &[RoleId]) -> Vec<crate::ids::RoleSetId> {
        let mut wanted: Vec<RoleIdx> = roles
            .iter()
            .filter_map(|role| self.model.roles.resolve(*role))
            .collect();
        if wanted.is_empty() {
            return Vec::new();
        }
        wanted.sort_unstable();
        wanted.dedup();
        self.model.rolesets.holding_any(&wanted)
    }

    pub fn role_set_members_estimate(&self, roles: &[RoleId]) -> Units {
        let sets = count(self.model.rolesets.len());
        let found = self
            .holding_sets(roles)
            .into_iter()
            .map(|set| count(self.members.set_len(set)))
            .fold(0u64, u64::saturating_add);
        Units::new(
            sets.saturating_mul(8)
                .saturating_add(count(roles.len()).saturating_mul(8))
                .saturating_add(found.saturating_mul(16)),
        )
    }

    pub fn role_set_members(&self, roles: &[RoleId], meter: &mut Meter) -> Vec<UserId> {
        meter.charge(count(self.model.rolesets.len()).saturating_mul(4));
        let mut out = Vec::new();
        for set in self.holding_sets(roles) {
            meter.charge(count(self.members.set_len(set)));
            out.extend(self.members.in_set(set));
        }
        meter.charge(count(out.len()).saturating_mul(2));
        out.sort_unstable();
        out
    }

    pub fn virtual_view(&self, user: UserId) -> Vec<ChannelId> {
        self.model.virtual_view.get(user).to_vec()
    }

    pub fn has_virtual_view(&self, user: UserId, channel: ChannelId) -> bool {
        self.model.virtual_view.has(user, channel)
    }
}
