// SPDX-License-Identifier: AGPL-3.0-or-later

use smallvec::SmallVec;

use crate::bits;
use crate::budget::Meter;
use crate::diff::{self, Group};
use crate::eval::{self, Who};
use crate::flags::{Permissions, Visible};
use crate::ids::{ChannelId, Epoch, RoleSetId, UserId};
use crate::rolesets::Base;
use crate::shared::{PermsError, View};
use crate::views::Scope;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Need {
    View,
    ViewMembers,
    MessageAccess(i64),
    History(i64),
    ChannelBits(Permissions),
    GuildBits(Permissions),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Audience {
    epoch: Epoch,
    rolesets: Box<[u64]>,
    users: SmallVec<[UserId; 4]>,
}

impl Audience {
    pub fn epoch(&self) -> Epoch {
        self.epoch
    }

    pub fn rolesets(&self) -> &[u64] {
        &self.rolesets
    }

    pub fn users(&self) -> &[UserId] {
        &self.users
    }

    pub fn has_set(&self, set: RoleSetId) -> bool {
        bits::test(&self.rolesets, set.index())
    }

    pub fn contains(&self, group: Group) -> bool {
        match group {
            Group::RoleSet(set) => self.has_set(set),
            Group::User(user) => self.users.binary_search(&user).is_ok(),
        }
    }
}

fn meets(visible: Visible, need: Need, open: impl Fn(i64) -> bool) -> bool {
    match need {
        Need::View => visible.contains(Visible::VIEW),
        Need::ViewMembers => visible.contains(Visible::VIEW | Visible::VIEW_MEMBERS),
        Need::MessageAccess(ms) => {
            visible.contains(Visible::VIEW) && (visible.contains(Visible::HISTORY) || open(ms))
        }
        Need::History(ms) => visible.contains(Visible::HISTORY) || open(ms),
        Need::ChannelBits(_) | Need::GuildBits(_) => false,
    }
}

impl View<'_> {
    pub fn audience(
        &self,
        channel: ChannelId,
        need: Need,
        meter: &mut Meter,
    ) -> Result<Audience, PermsError> {
        if let Need::ChannelBits(p) | Need::GuildBits(p) = need
            && p.is_empty()
        {
            return Err(PermsError::EmptyRequirement);
        }
        let model = &self.model;
        let sets = model.rolesets.len();
        let words = model.rolesets.words();
        let flags = model.flags;
        let open = |ms: i64| flags.history_open(ms);
        let idx = model.channels.resolve(channel);
        let class = idx.and_then(|i| model.classes.class_of_channel(i));
        let mut out = vec![0u64; words];
        match (need, class) {
            (
                Need::View | Need::ViewMembers | Need::MessageAccess(_) | Need::History(_),
                Some(class),
            ) => {
                meter.charge(u64::try_from(words).unwrap_or(u64::MAX));
                let planes = class.planes();
                for (w, slot) in out.iter_mut().enumerate() {
                    let get = |p: &[u64]| p.get(w).copied().unwrap_or(0);
                    *slot = match need {
                        Need::ViewMembers => get(planes.view()) & get(planes.view_members()),
                        Need::MessageAccess(ms) if open(ms) => get(planes.view()),
                        Need::MessageAccess(_) => get(planes.view()) & get(planes.history()),
                        Need::History(ms) if open(ms) => u64::MAX,
                        Need::History(_) => get(planes.history()),
                        _ => get(planes.view()),
                    };
                }
            }
            (Need::ChannelBits(p), _) => {
                let channel = idx.and_then(|i| model.channels.get(i));
                for (id, set) in model.rolesets.iter() {
                    meter.charge(8);
                    let bits = match (channel, set.base()) {
                        (Some(c), base) => eval::channel_permissions(
                            base,
                            set.roles(),
                            c.rules(),
                            crate::stage::Stage::IDENTITY,
                        ),
                        (None, Base::All) => Permissions::all(),
                        (None, Base::Bits(_)) => Permissions::empty(),
                    };
                    bits::assign(&mut out, id.index(), bits.contains(p));
                }
            }
            (Need::GuildBits(p), _) => {
                for (id, set) in model.rolesets.iter() {
                    meter.charge(8);
                    bits::assign(&mut out, id.index(), set.base().permissions().contains(p));
                }
            }
            (_, None) => {
                let channels = &model.channels;
                let resolved = idx.and_then(|i| channels.get(i).map(|c| (i, c)));
                for (id, set) in model.rolesets.iter() {
                    meter.charge(8);
                    let bits_of = |c: &crate::channels::Channel| {
                        eval::channel_permissions(
                            set.base(),
                            set.roles(),
                            c.rules(),
                            crate::stage::Stage::IDENTITY,
                        )
                    };
                    let visible = match resolved {
                        Some((i, c)) => {
                            let mut visible = Visible::from_permissions(bits_of(c));
                            if c.kind().is_category() && !visible.contains(Visible::VIEW) {
                                let children = channels.children(i);
                                meter.charge(u64::try_from(children.len()).unwrap_or(u64::MAX));
                                if children.iter().any(|child| {
                                    channels.get(*child).is_some_and(|ch| {
                                        bits_of(ch).contains(Permissions::VIEW_CHANNEL)
                                    })
                                }) {
                                    visible.insert(Visible::VIEW);
                                }
                            }
                            visible
                        }
                        None => match set.base() {
                            Base::All => Visible::all(),
                            Base::Bits(_) => Visible::empty(),
                        },
                    };
                    bits::assign(&mut out, id.index(), meets(visible, need, open));
                }
            }
        }
        for (w, slot) in out.iter_mut().enumerate() {
            *slot &= bits::tail_mask(sets, w);
        }
        let mut users: SmallVec<[UserId; 4]> = SmallVec::new();
        for user in model.exceptions.users() {
            meter.charge(8);
            let entry = self.members.get(*user);
            if diff::group_of(model, *user, entry) != Some(Group::User(*user)) {
                continue;
            }
            let who = Who::of(model.owner, &model.rolesets, *user, entry);
            let ok = match need {
                Need::ChannelBits(p) => match self.permissions(*user, Scope::Channel(channel)) {
                    Ok(bits) => bits.contains(p),
                    Err(_) => false,
                },
                Need::GuildBits(p) => match self.permissions(*user, Scope::Guild) {
                    Ok(bits) => bits.contains(p),
                    Err(_) => false,
                },
                Need::History(ms) => self.message_access(*user, channel, ms),
                _ => {
                    let visible = match (class, who) {
                        (Some(class), Who::Member { set, .. }) => class.visible(set, *user),
                        (Some(class), Who::Owner { member: true }) => {
                            class.visible(entry.map_or(RoleSetId::EVERYONE, |e| e.roleset()), *user)
                        }
                        (Some(class), Who::Owner { member: false } | Who::Stranger) => {
                            class.override_of(*user).unwrap_or(Visible::empty())
                        }
                        (Some(_), Who::Quarantined) => Visible::empty(),
                        (None, Who::Owner { member: false }) => {
                            if model.virtual_view.has(*user, channel) {
                                Visible::VIEW
                            } else {
                                Visible::empty()
                            }
                        }
                        (None, _) => self.visible(*user, channel),
                    };
                    meets(visible, need, open)
                }
            };
            if ok {
                users.push(*user);
            }
        }
        Ok(Audience {
            epoch: model.epoch,
            rolesets: out.into_boxed_slice(),
            users,
        })
    }
}
