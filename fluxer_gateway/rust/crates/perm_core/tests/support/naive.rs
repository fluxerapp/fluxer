// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;

use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Cutoff, Event, GuildId, Hoist, Nsfw,
    OverwriteSpec, OverwriteTarget, Permissions, RankPos, RoleId, RoleList, RoleSpec, Snapshot,
    UserId, Visible,
};

use super::GUILD;

pub struct Rng(u64);

impl Rng {
    pub fn new(seed: u64) -> Rng {
        Rng(seed.wrapping_mul(0x9E37_79B9_7F4A_7C15).wrapping_add(1))
    }

    pub fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }

    pub fn below(&mut self, n: u64) -> u64 {
        if n == 0 { 0 } else { self.next() % n }
    }

    pub fn chance(&mut self, percent: u64) -> bool {
        self.below(100) < percent
    }

    pub fn pick<T: Copy>(&mut self, items: &[T]) -> T {
        items[self.below(items.len() as u64) as usize]
    }
}

const BITS: [Permissions; 7] = [
    Permissions::VIEW_CHANNEL,
    Permissions::VIEW_CHANNEL_MEMBERS,
    Permissions::READ_MESSAGE_HISTORY,
    Permissions::SEND_MESSAGES,
    Permissions::MANAGE_ROLES,
    Permissions::CONNECT,
    Permissions::ADMINISTRATOR,
];

pub fn bits(rng: &mut Rng, admin_percent: u64) -> Permissions {
    let mut p = Permissions::empty();
    for bit in BITS {
        let chance = if bit == Permissions::ADMINISTRATOR {
            admin_percent
        } else {
            45
        };
        if rng.chance(chance) {
            p |= bit;
        }
    }
    p
}

#[derive(Clone, Debug)]
pub struct World {
    pub owner: Option<UserId>,
    pub roles: Vec<RoleSpec>,
    pub channels: Vec<ChannelSpec>,
    pub members: BTreeMap<UserId, RoleList>,
    pub virtual_access: BTreeMap<UserId, Vec<ChannelId>>,
    pub role_pool: u64,
    pub user_pool: u64,
    pub next_channel: u64,
}

impl World {
    pub fn guild() -> GuildId {
        GuildId::new(GUILD)
    }

    pub fn random(rng: &mut Rng, roles: u64, channels: u64, members: u64) -> World {
        let mut world = World {
            owner: None,
            roles: Vec::new(),
            channels: Vec::new(),
            members: BTreeMap::new(),
            virtual_access: BTreeMap::new(),
            role_pool: roles,
            user_pool: members + 5,
            next_channel: 5000,
        };
        world.roles = world.random_roles(rng);
        for _ in 0..channels {
            let spec = world.random_channel(rng, None);
            world.channels.push(spec);
        }
        for u in 1..=members {
            let list = world.random_role_list(rng);
            world.members.insert(UserId::new(u), list);
        }
        world.owner = Some(UserId::new(1 + rng.below(world.user_pool)));
        world
    }

    pub fn snapshot(&self) -> Snapshot {
        let mut snapshot = Snapshot::new(World::guild(), self.owner);
        snapshot.roles = self.roles.clone();
        snapshot.channels = self.channels.clone();
        snapshot.members = self.members.iter().map(|(u, l)| (*u, l.clone())).collect();
        snapshot.virtual_access = self
            .virtual_access
            .iter()
            .map(|(u, c)| (*u, c.clone()))
            .collect();
        snapshot.cutoff = Cutoff::Missing;
        snapshot
    }

    fn random_roles(&self, rng: &mut Rng) -> Vec<RoleSpec> {
        let mut roles = vec![RoleSpec {
            id: RoleId::new(GUILD),
            permissions: bits(rng, 3) | Permissions::VIEW_CHANNEL,
            position: RankPos::Int(0),
            hoist: Hoist::NotHoisted,
        }];
        for r in 1..=self.role_pool {
            if rng.chance(10) {
                continue;
            }
            roles.push(RoleSpec {
                id: RoleId::new(r),
                permissions: bits(rng, 8),
                position: if rng.chance(5) {
                    RankPos::Undefined
                } else {
                    RankPos::Int(rng.below(20) as i64)
                },
                hoist: if rng.chance(30) {
                    Hoist::Hoisted(Some(rng.below(10) as i64))
                } else {
                    Hoist::NotHoisted
                },
            });
        }
        let len = roles.len() as u64;
        for i in 0..roles.len() {
            let j = rng.below(len) as usize;
            roles.swap(i, j);
        }
        roles
    }

    pub fn random_role_list(&self, rng: &mut Rng) -> RoleList {
        if rng.chance(2) {
            return RoleList::Malformed;
        }
        let count = rng.below(4);
        let mut ids = Vec::new();
        for _ in 0..count {
            let id = match rng.below(20) {
                0 => GUILD,
                1 => 900 + rng.below(3),
                _ => 1 + rng.below(self.role_pool.max(1)),
            };
            ids.push(RoleId::new(id));
        }
        RoleList::Roles(ids)
    }

    pub fn random_channel(&mut self, rng: &mut Rng, id: Option<ChannelId>) -> ChannelSpec {
        let id = id.unwrap_or_else(|| {
            self.next_channel += 1;
            ChannelId::new(self.next_channel)
        });
        let kind = match rng.below(10) {
            0 | 1 => ChannelKind::Category,
            2 => ChannelKind::Voice,
            3 => ChannelKind::Announcement,
            4 => ChannelKind::Forum,
            _ => ChannelKind::Text,
        };
        let categories: Vec<ChannelId> = self
            .channels
            .iter()
            .filter(|c| c.kind == ChannelKind::Category && c.id != id)
            .map(|c| c.id)
            .collect();
        let parent = if kind != ChannelKind::Category && !categories.is_empty() && rng.chance(60) {
            Some(rng.pick(&categories))
        } else if rng.chance(3) {
            Some(ChannelId::new(99_999))
        } else {
            None
        };
        let mut overwrites = Vec::new();
        for _ in 0..rng.below(5) {
            let target = match rng.below(10) {
                0..=2 => OverwriteTarget::Role(RoleId::new(GUILD)),
                3 => OverwriteTarget::Member(UserId::new(1 + rng.below(self.user_pool))),
                4 => OverwriteTarget::Role(RoleId::new(900 + rng.below(3))),
                _ => OverwriteTarget::Role(RoleId::new(1 + rng.below(self.role_pool.max(1)))),
            };
            overwrites.push(OverwriteSpec {
                target,
                allow: bits(rng, 2),
                deny: bits(rng, 2),
            });
        }
        ChannelSpec {
            id,
            kind,
            parent,
            overwrites,
            nsfw: Nsfw::Inherit,
            named: true,
        }
    }

    pub fn random_event(&mut self, rng: &mut Rng) -> Event {
        match rng.below(100) {
            0..=9 => {
                let roles = self.random_roles(rng);
                self.roles = roles.clone();
                Event::Roles(roles)
            }
            10..=14 => {
                let owner = if rng.chance(20) {
                    None
                } else {
                    Some(UserId::new(1 + rng.below(self.user_pool)))
                };
                self.owner = owner;
                Event::Owner(owner)
            }
            15..=29 => {
                let existing = !self.channels.is_empty() && rng.chance(60);
                if existing {
                    let i = rng.below(self.channels.len() as u64) as usize;
                    let id = self.channels[i].id;
                    let spec = self.random_channel(rng, Some(id));
                    self.channels[i] = spec.clone();
                    Event::ChannelUpsert(spec, ChannelCause::Updated)
                } else {
                    let spec = self.random_channel(rng, None);
                    self.channels.push(spec.clone());
                    Event::ChannelUpsert(spec, ChannelCause::Created)
                }
            }
            30..=34 => {
                if self.channels.is_empty() {
                    return Event::ChannelOrder(Vec::new());
                }
                let i = rng.below(self.channels.len() as u64) as usize;
                let spec = self.channels.remove(i);
                Event::ChannelDelete(spec.id)
            }
            35..=37 => {
                let len = self.channels.len() as u64;
                for i in 0..self.channels.len() {
                    let j = rng.below(len) as usize;
                    self.channels.swap(i, j);
                }
                Event::ChannelOrder(self.channels.iter().map(|c| c.id).collect())
            }
            38..=74 => {
                let user = UserId::new(1 + rng.below(self.user_pool));
                let list = self.random_role_list(rng);
                self.members.insert(user, list.clone());
                Event::MemberUpsert(user, list)
            }
            75..=84 => {
                let user = UserId::new(1 + rng.below(self.user_pool));
                self.members.remove(&user);
                Event::MemberRemove(user)
            }
            85..=94 => {
                let user = UserId::new(1 + rng.below(self.user_pool));
                let mut list = Vec::new();
                if !rng.chance(30) {
                    for _ in 0..(1 + rng.below(2)) {
                        if !self.channels.is_empty() {
                            list.push(
                                self.channels[rng.below(self.channels.len() as u64) as usize].id,
                            );
                        }
                    }
                }
                list.sort();
                list.dedup();
                if list.is_empty() {
                    self.virtual_access.remove(&user);
                } else {
                    self.virtual_access.insert(user, list.clone());
                }
                Event::VirtualAccess(user, list)
            }
            _ => Event::HistoryCutoff(Cutoff::AtMillis(rng.below(1000) as i64)),
        }
    }

    fn role_perms(&self, id: RoleId) -> Permissions {
        self.roles
            .iter()
            .rev()
            .find(|r| r.id == id)
            .map_or(Permissions::empty(), |r| r.permissions)
    }

    pub fn member_roles(&self, user: UserId) -> Option<Option<Vec<RoleId>>> {
        self.members.get(&user).map(|l| match l {
            RoleList::Roles(ids) => Some(ids.clone()),
            RoleList::Malformed => None,
        })
    }

    pub fn permissions(&self, user: UserId, channel: &ChannelSpec) -> Option<Permissions> {
        if self.owner == Some(user) {
            return Some(Permissions::all());
        }
        let roles = self.member_roles(user)??;
        let mut base = self.role_perms(RoleId::new(GUILD));
        for r in &roles {
            base |= self.role_perms(*r);
        }
        if base.contains(Permissions::ADMINISTRATOR) {
            return Some(Permissions::all());
        }
        let mut p = base;
        for o in &channel.overwrites {
            if o.target == OverwriteTarget::Role(RoleId::new(GUILD)) {
                p = (p & !o.deny) | o.allow;
            }
        }
        let (mut allow, mut deny) = (Permissions::empty(), Permissions::empty());
        for o in &channel.overwrites {
            if let OverwriteTarget::Role(id) = o.target
                && roles.contains(&id)
            {
                allow |= o.allow;
                deny |= o.deny;
            }
        }
        p = (p & !deny) | allow;
        for o in &channel.overwrites {
            if o.target == OverwriteTarget::Member(user) {
                p = (p & !o.deny) | o.allow;
            }
        }
        Some(p)
    }

    pub fn visible(&self, user: UserId, channel: &ChannelSpec) -> Visible {
        let is_member = self.members.contains_key(&user);
        let virtual_ok = self
            .virtual_access
            .get(&user)
            .is_some_and(|c| c.contains(&channel.id));
        if !is_member {
            return if virtual_ok {
                Visible::VIEW
            } else {
                Visible::empty()
            };
        }
        let Some(perms) = self.permissions(user, channel) else {
            return Visible::empty();
        };
        let mut v = Visible::from_permissions(perms);
        if virtual_ok {
            v |= Visible::VIEW;
        }
        if channel.kind == ChannelKind::Category
            && self.channels.iter().any(|c| {
                c.parent == Some(channel.id)
                    && self
                        .permissions(user, c)
                        .is_some_and(|p| p.contains(Permissions::VIEW_CHANNEL))
            })
        {
            v |= Visible::VIEW;
        }
        v
    }
}
