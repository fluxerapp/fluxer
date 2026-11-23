// SPDX-License-Identifier: AGPL-3.0-or-later

use core::cmp::Ordering;

use crate::budget::Meter;
use crate::channels::{Channel, ChannelTable, Rules};
use crate::flags::{Permissions, Visible};
use crate::ids::{ChannelId, ChannelIdx, RoleIdx, RoleSetId, UserId};
use crate::members::MemberEntry;
use crate::rolesets::{Base, RoleSets};
use crate::stage::{Grant, Stage};

pub(crate) fn grant_of(grants: &[(RoleIdx, Grant)], set: &[RoleIdx]) -> Grant {
    let mut grant = Grant::NONE;
    let mut grants = grants.iter().peekable();
    let mut roles = set.iter().peekable();
    while let (Some((role, g)), Some(member_role)) = (grants.peek(), roles.peek()) {
        match role.cmp(member_role) {
            Ordering::Less => {
                grants.next();
            }
            Ordering::Greater => {
                roles.next();
            }
            Ordering::Equal => {
                grant = grant.merge(*g);
                grants.next();
                roles.next();
            }
        }
    }
    grant
}

pub(crate) fn evaluate(
    base: Base,
    set: &[RoleIdx],
    everyone: Stage,
    grants: &[(RoleIdx, Grant)],
    member: Stage,
) -> Permissions {
    match base {
        Base::All => Permissions::all(),
        Base::Bits(bits) => {
            let after_everyone = everyone.apply(bits);
            let after_roles = grant_of(grants, set).as_stage().apply(after_everyone);
            member.apply(after_roles)
        }
    }
}

pub(crate) fn channel_permissions(
    base: Base,
    set: &[RoleIdx],
    rules: &Rules,
    member: Stage,
) -> Permissions {
    evaluate(base, set, rules.everyone(), rules.roles(), member)
}

pub(crate) fn role_set_visible(
    base: Base,
    set: &[RoleIdx],
    everyone: Stage,
    grants: &[(RoleIdx, Grant)],
) -> Visible {
    Visible::from_permissions(evaluate(base, set, everyone, grants, Stage::IDENTITY))
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Who<'a> {
    Owner {
        member: bool,
    },
    Member {
        set: RoleSetId,
        base: Base,
        roles: &'a [RoleIdx],
    },
    Quarantined,
    Stranger,
}

impl<'a> Who<'a> {
    pub(crate) fn of(
        owner: Option<UserId>,
        rolesets: &'a RoleSets,
        user: UserId,
        entry: Option<&MemberEntry>,
    ) -> Who<'a> {
        if owner == Some(user) {
            return Who::Owner {
                member: entry.is_some(),
            };
        }
        match entry {
            None => Who::Stranger,
            Some(e) if e.is_quarantined() => Who::Quarantined,
            Some(e) => match rolesets.get(e.roleset()) {
                Some(set) => Who::Member {
                    set: e.roleset(),
                    base: set.base(),
                    roles: set.roles(),
                },
                None => Who::Quarantined,
            },
        }
    }

    pub(crate) fn is_member(self) -> bool {
        match self {
            Who::Owner { member } => member,
            Who::Member { .. } | Who::Quarantined => true,
            Who::Stranger => false,
        }
    }

    pub(crate) fn channel_bits(self, user: UserId, channel: &Channel) -> Permissions {
        match self {
            Who::Owner { .. } => Permissions::all(),
            Who::Member { base, roles, .. } => channel_permissions(
                base,
                roles,
                channel.rules(),
                channel.rules().member_stage(user),
            ),
            Who::Quarantined | Who::Stranger => Permissions::empty(),
        }
    }
}

pub(crate) fn session_bits(
    who: Who<'_>,
    user: UserId,
    virtual_list: &[ChannelId],
    channels: &ChannelTable,
    idx: ChannelIdx,
    meter: &mut Meter,
) -> Visible {
    let Some(channel) = channels.get(idx) else {
        return Visible::empty();
    };
    meter.charge(8);
    let has_virtual = virtual_list.binary_search(&channel.id()).is_ok();
    match who {
        Who::Owner { member: true } => Visible::all(),
        Who::Quarantined => Visible::empty(),
        Who::Owner { member: false } | Who::Stranger => {
            if has_virtual {
                Visible::VIEW
            } else {
                Visible::empty()
            }
        }
        Who::Member { .. } => {
            let mut visible = Visible::from_permissions(who.channel_bits(user, channel));
            if has_virtual {
                visible.insert(Visible::VIEW);
            }
            if channel.kind().is_category() && !visible.contains(Visible::VIEW) {
                for child in channels.children(idx) {
                    meter.charge(8);
                    if channels.get(*child).is_some_and(|c| {
                        who.channel_bits(user, c)
                            .contains(Permissions::VIEW_CHANNEL)
                    }) {
                        visible.insert(Visible::VIEW);
                        break;
                    }
                }
            }
            visible
        }
    }
}

pub(crate) fn session_row(
    who: Who<'_>,
    user: UserId,
    virtual_list: &[ChannelId],
    channels: &ChannelTable,
    out: &mut Vec<Visible>,
    meter: &mut Meter,
) {
    out.clear();
    out.resize(channels.slots(), Visible::empty());
    let mut rule2 = vec![false; channels.slots()];
    for (idx, channel) in channels.live() {
        meter.charge(8);
        let has_virtual = virtual_list.binary_search(&channel.id()).is_ok();
        let visible = match who {
            Who::Owner { member: true } => Visible::all(),
            Who::Quarantined => Visible::empty(),
            Who::Owner { member: false } | Who::Stranger => {
                if has_virtual {
                    Visible::VIEW
                } else {
                    Visible::empty()
                }
            }
            Who::Member { .. } => {
                let mut visible = Visible::from_permissions(who.channel_bits(user, channel));
                if let Some(flag) = rule2.get_mut(idx.index()) {
                    *flag = visible.contains(Visible::VIEW);
                }
                if has_virtual {
                    visible.insert(Visible::VIEW);
                }
                visible
            }
        };
        if let Some(slot) = out.get_mut(idx.index()) {
            *slot = visible;
        }
    }
    if !matches!(who, Who::Member { .. }) {
        return;
    }
    for (idx, channel) in channels.live() {
        if !channel.kind().is_category() {
            continue;
        }
        let children = channels.children(idx);
        meter.charge(u64::try_from(children.len()).unwrap_or(u64::MAX));
        if children
            .iter()
            .any(|c| rule2.get(c.index()).copied().unwrap_or(false))
            && let Some(slot) = out.get_mut(idx.index())
        {
            slot.insert(Visible::VIEW);
        }
    }
}
