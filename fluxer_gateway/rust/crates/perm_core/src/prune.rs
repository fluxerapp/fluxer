// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::bits;
use crate::budget::{Meter, Units};
use crate::flags::Visible;
use crate::ids::{ChannelId, RoleId, UserId};
use crate::members::MemberEntry;
use crate::shared::View;
use crate::views::Read;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PushPrune<'a> {
    pub channel: ChannelId,
    pub mentioned: &'a [UserId],
    pub roles: &'a [RoleId],
    pub everyone: bool,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Pruned {
    pub immediate: Vec<UserId>,
    pub held: Vec<UserId>,
}

fn count(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

impl View<'_> {
    pub fn push_prune_estimate(&self, prune: &PushPrune<'_>) -> Units {
        let explicit = self
            .role_set_members_estimate(prune.roles)
            .plus(Units::new(count(prune.mentioned.len()).saturating_mul(24)));
        if prune.everyone {
            explicit
                .plus(self.estimate(Read::Viewers))
                .plus(Units::new(count(self.members().len()).saturating_mul(16)))
        } else {
            explicit
        }
    }

    pub fn push_prune(&self, prune: &PushPrune<'_>, meter: &mut Meter) -> Pruned {
        if prune.mentioned.is_empty() && prune.roles.is_empty() && !prune.everyone {
            return Pruned::default();
        }
        let mut immediate = self.role_viewers(prune.channel, prune.roles, meter);
        meter.charge(
            count(immediate.len())
                .saturating_add(count(prune.mentioned.len()))
                .saturating_mul(16),
        );
        immediate.extend(self.only_viewers(prune.channel, prune.mentioned));
        immediate.sort_unstable();
        immediate.dedup();
        if !prune.everyone {
            return Pruned {
                immediate,
                held: Vec::new(),
            };
        }
        let mut held = self.viewers(prune.channel, meter);
        meter.charge(count(held.len()).saturating_mul(16));
        held.sort_unstable();
        held.retain(|user| immediate.binary_search(user).is_err());
        Pruned { immediate, held }
    }

    fn role_viewers(&self, channel: ChannelId, roles: &[RoleId], meter: &mut Meter) -> Vec<UserId> {
        meter.charge(count(self.model.rolesets.len()).saturating_mul(4));
        let class = self
            .model
            .channels
            .resolve(channel)
            .and_then(|idx| self.model.classes.class_of_channel(idx));
        let mut found = 0usize;
        let mut out = Vec::new();
        for set in self.holding_sets(roles) {
            let len = self.members.set_len(set);
            meter.charge(count(len));
            found = found.saturating_add(len);
            let entries = self.members.set_entries(set);
            let Some(class) = class else {
                out.extend(
                    entries
                        .map(MemberEntry::user)
                        .filter(|user| self.visible(*user, channel).contains(Visible::VIEW)),
                );
                continue;
            };
            let set_visible = bits::test(class.planes().view(), set.index());
            if class.overrides().is_empty() {
                if set_visible {
                    out.extend(entries.map(MemberEntry::user));
                }
                continue;
            }
            out.extend(entries.map(MemberEntry::user).filter(|user| {
                class
                    .override_of(*user)
                    .map_or(set_visible, |visible| visible.contains(Visible::VIEW))
            }));
        }
        meter.charge(count(found).saturating_mul(2));
        out
    }
}
