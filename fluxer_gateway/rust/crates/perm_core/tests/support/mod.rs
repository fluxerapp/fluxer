// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use perm_core::audit::{audit, audit_class_ids};
use perm_core::reference::{Malformed, RefGroup, RefNeed, Reference, row_diff};
use perm_core::rolesets::DEAD;
use perm_core::{
    ChannelCause, ChannelId, ChannelIdx, ChannelKind, ChannelSpec, ClassId, Config, Cutoff, Delta,
    Effects, Event, Group, GuildId, Hoist, MAX_CANDIDATES, Meter, Model, Need, Nsfw, OverwriteSpec,
    OverwriteTarget, PAIR_MEMO_LIMIT, Permissions, PermsError, PermsShared, PushPrune, RankPos,
    Read, RoleId, RoleList, RoleSetId, RoleSpec, Scope, Snapshot, Subject, Target, ThreadAccess,
    UserId, ViewKey, ViewSetId, VisibilityChange, Visible, diff,
};
use proptest::prelude::*;

pub mod bytes;
pub mod naive;

pub fn cases(default_cases: u32) -> u32 {
    std::env::var("PROPTEST_CASES")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(default_cases)
}

pub fn rng_seed() -> u64 {
    std::env::var("PROPTEST_RNG_SEED")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1)
}

pub fn runner_config(cases: u32) -> proptest::test_runner::Config {
    proptest::test_runner::Config {
        cases,
        failure_persistence: None,
        rng_seed: proptest::test_runner::RngSeed::Fixed(rng_seed()),
        max_shrink_iters: 4096,
        ..proptest::test_runner::Config::default()
    }
}

pub const GUILD: u64 = 1000;
pub const UNKNOWN_CHANNEL: u64 = 424_242;
pub const UNKNOWN_USER: u64 = 99_999;
pub const UNKNOWN_ROLE: u64 = 77_777;

pub fn guild() -> GuildId {
    GuildId::new(GUILD)
}

pub struct Rng(u64);

impl Rng {
    pub fn new(seed: u64) -> Rng {
        Rng(seed
            .wrapping_mul(0x9E37_79B9_7F4A_7C15)
            .wrapping_add(0x2545_F491_4F6C_DD1D))
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
}

const BITS: [Permissions; 11] = [
    Permissions::VIEW_CHANNEL,
    Permissions::VIEW_CHANNEL_MEMBERS,
    Permissions::READ_MESSAGE_HISTORY,
    Permissions::SEND_MESSAGES,
    Permissions::SEND_MESSAGES_IN_THREADS,
    Permissions::MANAGE_ROLES,
    Permissions::MANAGE_CHANNELS,
    Permissions::MANAGE_THREADS,
    Permissions::VIEW_AUDIT_LOG,
    Permissions::CONNECT,
    Permissions::ADMINISTRATOR,
];

fn bits(rng: &mut Rng, admin: u64, density: u64) -> Permissions {
    let mut p = Permissions::empty();
    for bit in BITS {
        let chance = if bit == Permissions::ADMINISTRATOR {
            admin
        } else {
            density
        };
        if rng.chance(chance) {
            p |= bit;
        }
    }
    if rng.chance(3) {
        p |= Permissions::from_bits_retain(1u64 << 63);
    }
    p
}

#[derive(Clone, Debug)]
pub struct Params {
    pub roles: u64,
    pub channels: u64,
    pub members: u64,
    pub seed: u64,
    pub budget: u64,
    pub floor: u32,
    pub cache: u64,
}

impl Params {
    pub fn user_pool(&self) -> u64 {
        self.members + 12
    }

    pub fn config(&self) -> Config {
        Config {
            work_budget_units: self.budget,
            viewable_cache_bytes: self.cache,
            dead_role_set_floor: self.floor,
            scan_chunk: 7,
        }
    }
}

fn skewed(small: u64, mid: u64, max: u64) -> impl Strategy<Value = u64> {
    prop_oneof![
        6 => 1..=small,
        3 => (small + 1)..=mid,
        1 => (mid + 1)..=max,
    ]
}

pub fn params() -> impl Strategy<Value = Params> {
    (
        skewed(6, 14, 30),
        skewed(8, 20, 40),
        skewed(12, 50, 200),
        any::<u64>(),
        prop_oneof![4 => Just(250_000u64), 1 => Just(4_000u64)],
        prop_oneof![3 => Just(4096u32), 1 => Just(2u32)],
        prop_oneof![3 => Just(1u64 << 20), 1 => Just(0u64), 1 => Just(96u64)],
    )
        .prop_map(
            |(roles, channels, members, seed, budget, floor, cache)| Params {
                roles,
                channels,
                members,
                seed,
                budget,
                floor,
                cache,
            },
        )
}

#[derive(Clone, Debug)]
pub enum Op {
    Roles(u64),
    RoleEdit(u8, u64),
    EveryoneEdit(u64),
    RolePosition(u8, i8),
    Owner(Option<u16>),
    ChannelCreate(u64),
    ChannelEdit(u16, u64),
    Reparent(u16, u16),
    ChannelDelete(u16),
    ChannelOrder(u64),
    MemberUpsert(u16, u64),
    MemberRemove(u16),
    VirtualAccess(u16, Vec<u16>),
    Cutoff(u8),
    Nsfw(u8),
    Category(u8, u16),
    Raw(Event),
}

pub fn op() -> impl Strategy<Value = Op> {
    prop_oneof![
        3 => any::<u64>().prop_map(Op::Roles),
        6 => (any::<u8>(), any::<u64>()).prop_map(|(r, s)| Op::RoleEdit(r, s)),
        3 => any::<u64>().prop_map(Op::EveryoneEdit),
        2 => (any::<u8>(), -2i8..20).prop_map(|(r, p)| Op::RolePosition(r, p)),
        2 => proptest::option::of(any::<u16>()).prop_map(Op::Owner),
        4 => any::<u64>().prop_map(Op::ChannelCreate),
        8 => (any::<u16>(), any::<u64>()).prop_map(|(c, s)| Op::ChannelEdit(c, s)),
        3 => (any::<u16>(), any::<u16>()).prop_map(|(c, p)| Op::Reparent(c, p)),
        3 => any::<u16>().prop_map(Op::ChannelDelete),
        2 => any::<u64>().prop_map(Op::ChannelOrder),
        18 => (any::<u16>(), any::<u64>()).prop_map(|(u, s)| Op::MemberUpsert(u, s)),
        5 => any::<u16>().prop_map(Op::MemberRemove),
        5 => (any::<u16>(), proptest::collection::vec(any::<u16>(), 0..3))
            .prop_map(|(u, c)| Op::VirtualAccess(u, c)),
        2 => any::<u8>().prop_map(Op::Cutoff),
        1 => any::<u8>().prop_map(Op::Nsfw),
        3 => (any::<u8>(), any::<u16>()).prop_map(|(k, u)| Op::Category(k, u)),
    ]
}

fn step() -> impl Strategy<Value = Vec<Op>> {
    proptest::collection::vec(op(), 1..=3)
}

pub fn steps() -> impl Strategy<Value = Vec<Vec<Op>>> {
    prop_oneof![
        6 => proptest::collection::vec(step(), 1..=10),
        3 => proptest::collection::vec(step(), 11..=40),
        1 => proptest::collection::vec(step(), 41..=150),
    ]
}

pub struct World {
    pub params: Params,
    pub rng: Rng,
    pub next_channel: u64,
}

impl World {
    pub fn new(params: &Params) -> World {
        World {
            params: params.clone(),
            rng: Rng::new(params.seed),
            next_channel: 5000,
        }
    }

    pub fn user(&self, raw: u16) -> UserId {
        UserId::new(1 + u64::from(raw) % self.params.user_pool())
    }

    fn role_id(&mut self) -> RoleId {
        match self.rng.below(24) {
            0 => RoleId::new(GUILD),
            1 => RoleId::new(900 + self.rng.below(3)),
            _ => RoleId::new(1 + self.rng.below(self.params.roles.max(1))),
        }
    }

    pub fn roles(&mut self) -> Vec<RoleSpec> {
        let mut roles = Vec::new();
        if !self.rng.chance(4) {
            roles.push(RoleSpec {
                id: RoleId::new(GUILD),
                permissions: bits(&mut self.rng, 3, 50) | Permissions::VIEW_CHANNEL,
                position: RankPos::Int(0),
                hoist: Hoist::NotHoisted,
            });
        }
        for r in 1..=self.params.roles {
            if self.rng.chance(8) {
                continue;
            }
            roles.push(self.role_spec(RoleId::new(r)));
        }
        if self.rng.chance(5) && !roles.is_empty() {
            let pick = roles[self.rng.below(roles.len() as u64) as usize].clone();
            let mut dup = self.role_spec(pick.id);
            dup.position = pick.position;
            roles.push(dup);
        }
        let len = roles.len() as u64;
        for i in 0..roles.len() {
            let j = self.rng.below(len) as usize;
            roles.swap(i, j);
        }
        roles
    }

    fn role_spec(&mut self, id: RoleId) -> RoleSpec {
        RoleSpec {
            id,
            permissions: bits(&mut self.rng, 6, 35),
            position: if self.rng.chance(4) {
                RankPos::Undefined
            } else {
                RankPos::Int(self.rng.below(12) as i64)
            },
            hoist: if self.rng.chance(30) {
                Hoist::Hoisted(Some(self.rng.below(10) as i64))
            } else {
                Hoist::NotHoisted
            },
        }
    }

    pub fn role_list(&mut self) -> RoleList {
        if self.rng.chance(3) {
            return RoleList::Malformed;
        }
        let count = self.rng.below(5);
        RoleList::Roles((0..count).map(|_| self.role_id()).collect())
    }

    fn overwrites(&mut self, users: u64) -> Vec<OverwriteSpec> {
        let mut out = Vec::new();
        let n = match self.rng.below(10) {
            0..=2 => 0,
            3..=7 => 1 + self.rng.below(3),
            _ => 3 + self.rng.below(5),
        };
        for _ in 0..n {
            let target = match self.rng.below(12) {
                0..=3 => OverwriteTarget::Role(RoleId::new(GUILD)),
                4 | 5 => OverwriteTarget::Member(UserId::new(1 + self.rng.below(users))),
                6 => OverwriteTarget::Role(RoleId::new(900 + self.rng.below(3))),
                _ => {
                    OverwriteTarget::Role(RoleId::new(1 + self.rng.below(self.params.roles.max(1))))
                }
            };
            out.push(OverwriteSpec {
                target,
                allow: bits(&mut self.rng, 2, 20),
                deny: bits(&mut self.rng, 2, 25),
            });
        }
        out
    }

    pub fn channel(&mut self, existing: &[ChannelSpec], id: Option<ChannelId>) -> ChannelSpec {
        let id = id.unwrap_or_else(|| {
            self.next_channel += 1;
            ChannelId::new(self.next_channel)
        });
        let kind = match self.rng.below(14) {
            0..=2 => ChannelKind::Category,
            3 => ChannelKind::Voice,
            4 => ChannelKind::Announcement,
            5 => ChannelKind::Forum,
            6 => ChannelKind::Media,
            7 => ChannelKind::Other(13),
            _ => ChannelKind::Text,
        };
        let categories: Vec<ChannelId> = existing
            .iter()
            .filter(|c| c.kind == ChannelKind::Category && c.id != id)
            .map(|c| c.id)
            .collect();
        let parent =
            if kind != ChannelKind::Category && !categories.is_empty() && self.rng.chance(65) {
                Some(categories[self.rng.below(categories.len() as u64) as usize])
            } else if self.rng.chance(4) && !existing.is_empty() {
                Some(existing[self.rng.below(existing.len() as u64) as usize].id)
            } else if self.rng.chance(3) {
                Some(ChannelId::new(99_999))
            } else {
                None
            };
        let nsfw = match self.rng.below(10) {
            0 => Nsfw::Adult,
            1 => Nsfw::Safe,
            _ => Nsfw::Inherit,
        };
        ChannelSpec {
            id,
            kind,
            parent,
            overwrites: self.overwrites(self.params.user_pool()),
            nsfw,
            named: !self.rng.chance(5),
        }
    }

    pub fn snapshot(&mut self) -> Snapshot {
        let mut snapshot = Snapshot::new(guild(), None);
        snapshot.roles = self.roles();
        for _ in 0..self.params.channels {
            let spec = self.channel(&snapshot.channels, None);
            snapshot.channels.push(spec);
        }
        for u in 1..=self.params.members {
            let list = self.role_list();
            snapshot.members.push((UserId::new(u), list));
        }
        snapshot.owner = match self.rng.below(10) {
            0 => None,
            1 | 2 => Some(UserId::new(self.params.members + 1 + self.rng.below(5))),
            _ => Some(UserId::new(1 + self.rng.below(self.params.members))),
        };
        for _ in 0..self.rng.below(3) {
            if snapshot.channels.is_empty() {
                break;
            }
            let user = UserId::new(1 + self.rng.below(self.params.user_pool()));
            let channel =
                snapshot.channels[self.rng.below(snapshot.channels.len() as u64) as usize].id;
            snapshot.virtual_access.push((user, vec![channel]));
        }
        snapshot.cutoff = match self.rng.below(4) {
            0 => Cutoff::Missing,
            1 => Cutoff::Malformed,
            _ => Cutoff::AtMillis(self.rng.below(1000) as i64),
        };
        snapshot.nsfw = match self.rng.below(4) {
            0 => Some(true),
            1 => Some(false),
            _ => None,
        };
        snapshot.nsfw_level = self.rng.below(4) as u8;
        snapshot
    }

    pub fn events(&mut self, op: &Op, world: &Reference) -> Vec<Event> {
        let channels = &world.channels;
        let pick = |raw: u16| -> Option<&ChannelSpec> {
            if channels.is_empty() {
                None
            } else {
                channels.get(raw as usize % channels.len())
            }
        };
        match op {
            Op::Roles(seed) => {
                self.rng = Rng::new(*seed ^ self.params.seed);
                vec![Event::Roles(self.roles())]
            }
            Op::RoleEdit(r, seed) => {
                let mut roles = world.roles.clone();
                if roles.is_empty() {
                    return vec![];
                }
                let i = *r as usize % roles.len();
                self.rng = Rng::new(*seed);
                roles[i].permissions = bits(&mut self.rng, 6, 35);
                vec![Event::Roles(roles)]
            }
            Op::EveryoneEdit(seed) => {
                let mut roles = world.roles.clone();
                self.rng = Rng::new(*seed);
                let quiet = self.rng.chance(40);
                match roles.iter_mut().rev().find(|r| r.id.get() == GUILD) {
                    Some(role) if quiet => role.permissions ^= Permissions::CONNECT,
                    Some(role) => role.permissions = bits(&mut self.rng, 4, 50),
                    None => roles.push(RoleSpec {
                        id: RoleId::new(GUILD),
                        permissions: bits(&mut self.rng, 4, 50),
                        position: RankPos::Int(0),
                        hoist: Hoist::NotHoisted,
                    }),
                }
                vec![Event::Roles(roles)]
            }
            Op::RolePosition(r, p) => {
                let mut roles = world.roles.clone();
                if roles.is_empty() {
                    return vec![];
                }
                let i = *r as usize % roles.len();
                roles[i].position = if *p < 0 {
                    RankPos::Undefined
                } else {
                    RankPos::Int(i64::from(*p))
                };
                roles[i].hoist = if *p % 3 == 0 {
                    Hoist::Hoisted(None)
                } else {
                    Hoist::NotHoisted
                };
                vec![Event::Roles(roles)]
            }
            Op::Owner(o) => vec![Event::Owner(o.map(|u| self.user(u)))],
            Op::ChannelCreate(seed) => {
                self.rng = Rng::new(*seed);
                let spec = self.channel(channels, None);
                vec![Event::ChannelUpsert(spec, ChannelCause::Created)]
            }
            Op::ChannelEdit(c, seed) => {
                let Some(old) = pick(*c).cloned() else {
                    return vec![];
                };
                self.rng = Rng::new(*seed);
                let mut spec = self.channel(channels, Some(old.id));
                match self.rng.below(4) {
                    0 => {
                        spec.kind = old.kind;
                        spec.parent = old.parent;
                    }
                    1 => {
                        spec.overwrites = old.overwrites.clone();
                        spec.kind = old.kind;
                        spec.parent = old.parent;
                    }
                    _ => {}
                }
                vec![Event::ChannelUpsert(spec, ChannelCause::Updated)]
            }
            Op::Reparent(c, p) => {
                let Some(mut spec) = pick(*c).cloned() else {
                    return vec![];
                };
                spec.parent = match *p % 7 {
                    0 => None,
                    1 => Some(ChannelId::new(99_999)),
                    _ => pick(*p).map(|x| x.id),
                };
                vec![Event::ChannelUpsert(spec, ChannelCause::Updated)]
            }
            Op::ChannelDelete(c) => match pick(*c) {
                Some(spec) => vec![Event::ChannelDelete(spec.id)],
                None => vec![Event::ChannelDelete(ChannelId::new(UNKNOWN_CHANNEL))],
            },
            Op::ChannelOrder(seed) => {
                self.rng = Rng::new(*seed);
                let mut ids: Vec<ChannelId> = channels.iter().map(|c| c.id).collect();
                let len = ids.len() as u64;
                for i in 0..ids.len() {
                    let j = self.rng.below(len) as usize;
                    ids.swap(i, j);
                }
                if self.rng.chance(30) {
                    ids.truncate(self.rng.below(len + 1) as usize);
                    ids.push(ChannelId::new(UNKNOWN_CHANNEL));
                    if let Some(first) = ids.first().copied() {
                        ids.push(first);
                    }
                }
                vec![Event::ChannelOrder(ids)]
            }
            Op::MemberUpsert(u, seed) => {
                self.rng = Rng::new(*seed);
                vec![Event::MemberUpsert(self.user(*u), self.role_list())]
            }
            Op::MemberRemove(u) => vec![Event::MemberRemove(self.user(*u))],
            Op::VirtualAccess(u, picks) => {
                let mut list: Vec<ChannelId> = picks
                    .iter()
                    .filter_map(|p| pick(*p).map(|c| c.id))
                    .collect();
                if picks.len() == 2 && picks[0] % 5 == 0 {
                    list.push(ChannelId::new(UNKNOWN_CHANNEL));
                }
                vec![Event::VirtualAccess(self.user(*u), list)]
            }
            Op::Cutoff(x) => vec![Event::HistoryCutoff(match x % 8 {
                0 => Cutoff::Missing,
                1 => Cutoff::Malformed,
                _ => Cutoff::AtMillis(i64::from(*x) * 4),
            })],
            Op::Nsfw(x) => vec![Event::GuildNsfw(
                match x % 3 {
                    0 => None,
                    1 => Some(false),
                    _ => Some(true),
                },
                x % 4,
            )],
            Op::Category(k, u) => self.category(*k, self.user(*u)),
            Op::Raw(event) => vec![event.clone()],
        }
    }

    pub fn category(&mut self, shape: u8, user: UserId) -> Vec<Event> {
        let everyone = |allow: Permissions, deny: Permissions| OverwriteSpec {
            target: OverwriteTarget::Role(RoleId::new(GUILD)),
            allow,
            deny,
        };
        let view = Permissions::VIEW_CHANNEL;
        let none = Permissions::empty();
        let (own, child) = match shape % 3 {
            0 => (everyone(none, view), everyone(view, none)),
            1 => (everyone(view, none), everyone(none, view)),
            _ => (everyone(none, view), everyone(none, view)),
        };
        self.next_channel += 2;
        let category = ChannelId::new(self.next_channel - 1);
        let text = ChannelId::new(self.next_channel);
        let mut events = vec![
            Event::ChannelUpsert(
                ChannelSpec {
                    id: category,
                    kind: ChannelKind::Category,
                    parent: None,
                    overwrites: vec![own],
                    nsfw: Nsfw::Inherit,
                    named: true,
                },
                ChannelCause::Created,
            ),
            Event::ChannelUpsert(
                ChannelSpec {
                    id: text,
                    kind: ChannelKind::Text,
                    parent: Some(category),
                    overwrites: vec![child],
                    nsfw: Nsfw::Inherit,
                    named: true,
                },
                ChannelCause::Created,
            ),
        ];
        if shape % 3 == 2 {
            events.push(Event::VirtualAccess(user, vec![text]));
        }
        events
    }
}

pub struct Before {
    pub reference: Reference,
    pub model: Arc<Model>,
    pub refcounts: Vec<u32>,
}

pub struct Driver {
    pub shared: Arc<PermsShared>,
    pub reference: Reference,
    pub world: World,
    pub compactions: u64,
    pub slot_compactions: u64,
    pub dirty: u64,
    pub seen: BTreeSet<UserId>,
}

pub fn named_users(r: &Reference) -> BTreeSet<UserId> {
    let mut users: BTreeSet<UserId> = r.members.keys().copied().collect();
    users.extend(r.virtual_access.keys());
    users.extend(r.owner);
    for channel in &r.channels {
        for o in &channel.overwrites {
            if let OverwriteTarget::Member(u) = o.target {
                users.insert(u);
            }
        }
    }
    users
}

pub fn role_key(model: &Model, set: RoleSetId) -> Vec<RoleId> {
    let mut ids: Vec<RoleId> = model
        .rolesets()
        .get(set)
        .map(|s| {
            s.roles()
                .iter()
                .filter_map(|i| model.roles().get(*i))
                .map(|r| r.id())
                .collect()
        })
        .unwrap_or_default();
    ids.sort();
    ids
}

pub fn core_group_key(model: &Model, group: Option<Group>) -> Option<RefGroup> {
    match group? {
        Group::RoleSet(set) => Some(RefGroup::Roles(role_key(model, set))),
        Group::User(u) => Some(RefGroup::User(u)),
    }
}

fn split(old: &[ChannelId], new: &[ChannelId]) -> (Vec<ChannelId>, Vec<ChannelId>) {
    (
        old.iter().filter(|c| !new.contains(c)).copied().collect(),
        new.iter().filter(|c| !old.contains(c)).copied().collect(),
    )
}

impl Driver {
    pub fn new(params: &Params) -> Driver {
        Driver::start(params, None)
    }

    pub fn start(params: &Params, snapshot: Option<&Snapshot>) -> Driver {
        let mut world = World::new(params);
        let snapshot = match snapshot {
            Some(given) => Snapshot {
                guild: guild(),
                ..given.clone()
            },
            None => world.snapshot(),
        };
        let shared = PermsShared::new(guild(), params.config());
        let estimate = PermsShared::load_estimate(&snapshot);
        let cap = params.config().budget().runaway_cap(estimate);
        shared.fault_extra_units(cap.get() - estimate.get());
        let mut dirty = 0;
        match shared.load(&snapshot) {
            Ok(_) => {}
            Err(PermsError::NeedsDirty) => {
                dirty += 1;
                shared.load_dirty(&snapshot).unwrap();
            }
            Err(e) => panic!("load spent more than its estimate {estimate:?}: {e:?}"),
        }
        shared.fault_extra_units(0);
        let reference = Reference::new(&snapshot);
        Driver {
            shared,
            seen: named_users(&reference),
            reference,
            world,
            compactions: 0,
            slot_compactions: 0,
            dirty,
        }
    }

    pub fn users(&self) -> Vec<UserId> {
        let mut users: BTreeSet<UserId> = (1..=self.world.params.user_pool())
            .map(UserId::new)
            .collect();
        users.insert(UserId::new(UNKNOWN_USER));
        users.extend(self.reference.owner);
        users.extend(&self.seen);
        users.into_iter().collect()
    }

    pub fn channel_ids(&self) -> Vec<ChannelId> {
        let mut ids: Vec<ChannelId> = self.reference.channels.iter().map(|c| c.id).collect();
        ids.push(ChannelId::new(UNKNOWN_CHANNEL));
        ids
    }

    pub fn before(&self) -> Before {
        let view = self.shared.view().unwrap();
        Before {
            reference: self.reference.clone(),
            model: view.model().clone(),
            refcounts: view.members().refcounts().to_vec(),
        }
    }
}

pub fn apply_step(driver: &mut Driver, ops: &[Op]) -> (Before, Effects) {
    let before = driver.before();
    let mut events = Vec::new();
    for op in ops {
        let produced = driver.world.events(op, &driver.reference);
        for e in &produced {
            driver.reference.apply(e);
        }
        events.extend(produced);
    }
    let named = named_users(&driver.reference);
    driver.seen.extend(named);
    let effects = match driver.shared.apply(&events) {
        Ok(effects) => {
            assert!(effects.estimate().get() <= driver.world.params.budget);
            effects
        }
        Err(PermsError::NeedsDirty) => {
            assert_eq!(driver.shared.epoch(), before.model.epoch());
            driver.dirty += 1;
            driver.shared.apply_dirty(&events).unwrap()
        }
        Err(e) => panic!("apply failed: {e:?} on {events:?}"),
    };
    assert!(
        effects.spent() <= effects.estimate(),
        "spent {:?} above estimate {:?} for {events:?}",
        effects.spent(),
        effects.estimate()
    );
    (before, effects)
}

pub fn maybe_compact(driver: &mut Driver, effects: &Effects) -> Option<Effects> {
    if !effects.compaction_due() {
        return None;
    }
    let done = match driver.shared.compact() {
        Ok(done) => done,
        Err(PermsError::NeedsDirty) => driver.shared.compact_dirty().unwrap(),
        Err(e) => panic!("compact failed: {e:?}"),
    };
    let done = done.expect("compaction was due");
    assert!(done.spent() <= done.estimate());
    assert!(done.roleset_remap().is_some() || done.channel_remap().is_some());
    assert!(done.changes().is_empty() && done.regroups().is_empty());
    let after = done.after();
    if let Some(remap) = done.channel_remap() {
        assert_eq!(remap.len(), done.before().channels().slots());
        assert_eq!(after.channels().dead_slots(), 0);
        driver.slot_compactions += 1;
    }
    for channel in &driver.reference.channels {
        let old = done.before().class_of_id(channel.id).map(|c| {
            let class = done.before().class(c).unwrap();
            (class.planes().clone(), class.overrides().to_vec())
        });
        let new = after.class_of_id(channel.id).map(|c| {
            let class = after.class(c).unwrap();
            (class.planes().clone(), class.overrides().to_vec())
        });
        match (old, new, done.roleset_remap()) {
            (Some(a), Some(b), None) => assert_eq!(a, b, "{:?}", channel.id),
            (Some(_), Some(_), Some(_)) => {}
            (a, b, _) => panic!("{:?} class {} -> {}", channel.id, a.is_some(), b.is_some()),
        }
    }
    driver.compactions += 1;
    Some(done)
}

fn meets_need(need: Need) -> RefNeed {
    match need {
        Need::View => RefNeed::View,
        Need::ViewMembers => RefNeed::ViewMembers,
        Need::MessageAccess(ms) => RefNeed::MessageAccess(ms),
        Need::History(ms) => RefNeed::History(ms),
        Need::ChannelBits(p) => RefNeed::ChannelBits(p),
        Need::GuildBits(p) => RefNeed::GuildBits(p),
    }
}

pub fn check_effects(driver: &Driver, before: &Before, effects: &Effects) {
    let after = effects.after();
    let users = driver.users();
    let r0 = &before.reference;
    let r1 = &driver.reference;
    let mut regroups = Vec::new();
    for user in &users {
        let a = r0.group(*user);
        let b = r1.group(*user);
        if a != b {
            regroups.push((*user, a, b));
        }
    }
    let got: Vec<(UserId, Option<RefGroup>, Option<RefGroup>)> = effects
        .regroups()
        .iter()
        .map(|r| {
            (
                r.user,
                core_group_key(&before.model, r.old),
                core_group_key(after, r.new),
            )
        })
        .collect();
    assert_eq!(got, regroups, "regroups");

    let mut expected: Vec<VisibilityChange> = Vec::new();
    for (set, _) in before.model.rolesets().iter() {
        if before.refcounts.get(set.index()).copied().unwrap_or(0) == 0 {
            continue;
        }
        let key = role_key(&before.model, set);
        let (lost, gained) = split(&r0.roles_view(&key), &r1.roles_view(&key));
        if !lost.is_empty() || !gained.is_empty() {
            expected.push(VisibilityChange {
                target: Target::Group(Group::RoleSet(set)),
                lost,
                gained,
            });
        }
    }
    let regrouped: BTreeSet<UserId> = regroups.iter().map(|r| r.0).collect();
    for user in &users {
        if regrouped.contains(user) || r0.group(*user) != Some(RefGroup::User(*user)) {
            continue;
        }
        let (lost, gained) = split(&r0.session_view(*user), &r1.session_view(*user));
        if !lost.is_empty() || !gained.is_empty() {
            expected.push(VisibilityChange {
                target: Target::Group(Group::User(*user)),
                lost,
                gained,
            });
        }
    }
    for (user, a, b) in &regroups {
        let (lost, gained) = split(&r0.group_view(a.as_ref()), &r1.group_view(b.as_ref()));
        if !lost.is_empty() || !gained.is_empty() {
            expected.push(VisibilityChange {
                target: Target::OneUser(*user),
                lost,
                gained,
            });
        }
    }
    expected.sort_by_key(|a| a.target);
    assert_eq!(effects.changes(), expected.as_slice(), "visibility changes");
    let delta = effects.delta();
    check_class_rows(&before.model, after, &delta);
    check_rows_against_reference(driver, before, effects, &delta);
}

pub fn check_class_rows(old: &Model, new: &Model, delta: &Delta) {
    if delta.resync {
        return;
    }
    let rows = new.rolesets().len();
    let mut listed = BTreeSet::new();
    for change in &delta.classes {
        assert!(
            listed.insert(change.class),
            "class {:?} twice",
            change.class
        );
        assert!(
            change.flipped.windows(2).all(|w| w[0].0 < w[1].0),
            "rows of {:?} not ascending",
            change.class
        );
        for (set, a, b) in &change.flipped {
            if set.index() < rows {
                assert_ne!(*a ^ *b, change.toggled, "{set:?} listed for its default");
            } else {
                assert_ne!(a, b, "{set:?} listed without a change");
            }
        }
    }
    let ids: BTreeSet<ClassId> = old
        .classes()
        .iter()
        .map(|(id, _)| id)
        .chain(new.classes().iter().map(|(id, _)| id))
        .collect();
    for id in ids {
        if old.class(id).is_none() || new.class(id).is_none() {
            assert!(!listed.contains(&id), "{id:?} listed without both sides");
            continue;
        }
        let got = delta
            .classes
            .iter()
            .find(|c| c.class == id)
            .map(|c| c.expanded(new))
            .unwrap_or_default();
        assert_eq!(got, row_diff(old, new, id), "rows of {id:?}");
    }
}

fn check_rows_against_reference(
    driver: &Driver,
    before: &Before,
    effects: &Effects,
    delta: &Delta,
) {
    let (old, new) = (&before.model, effects.after());
    if old.rolesets().generation() != new.rolesets().generation() {
        return;
    }
    let view = driver.shared.view().unwrap();
    let live: Vec<RoleSetId> = new
        .rolesets()
        .iter()
        .map(|(set, _)| set)
        .filter(|set| {
            before.refcounts.get(set.index()).copied().unwrap_or(0) > 0
                && view.members().refcount(*set) > 0
        })
        .collect();
    for channel in &driver.reference.channels {
        let Some(class) = new.class_of_id(channel.id) else {
            continue;
        };
        if old.class_of_id(channel.id) != Some(class) {
            continue;
        }
        let rows: BTreeMap<RoleSetId, (Visible, Visible)> = delta
            .classes
            .iter()
            .find(|c| c.class == class)
            .map(|c| c.expanded(new))
            .unwrap_or_default()
            .into_iter()
            .map(|(set, a, b)| (set, (a, b)))
            .collect();
        for set in &live {
            let a = before
                .reference
                .roles_visible(&role_key(old, *set), channel.id);
            let b = driver
                .reference
                .roles_visible(&role_key(new, *set), channel.id);
            let want = (a != b).then_some((a, b));
            assert_eq!(
                rows.get(set).copied(),
                want,
                "row {set:?} of {class:?} on {:?}",
                channel.id
            );
        }
    }
}

pub fn sweep<K: Ord>(keys: &[K], extra: usize, salt: u64) -> Vec<usize> {
    let stride = keys.len().div_ceil(extra.max(1)).max(1);
    let offset = usize::try_from(salt % stride as u64).unwrap_or(0);
    let mut seen = BTreeSet::new();
    let mut picked = Vec::new();
    for (i, key) in keys.iter().enumerate() {
        if seen.insert(key) || i % stride == offset {
            picked.push(i);
        }
    }
    picked
}

pub fn check_view(driver: &Driver) {
    check_view_over(driver, u64::MAX, usize::MAX);
}

pub fn check_view_over(driver: &Driver, work: u64, extra: usize) {
    let view = driver.shared.view().unwrap();
    let r = &driver.reference;
    let model = view.model().clone();
    let users = driver.users();
    let channels = driver.channel_ids();
    let mut meter = Meter::new();
    let mut sees: BTreeMap<UserId, BTreeSet<ChannelId>> = BTreeMap::new();
    let per_user = u64::try_from(channels.len())
        .unwrap_or(u64::MAX)
        .saturating_add(1);
    let room = usize::try_from(work / per_user).unwrap_or(usize::MAX);
    let detailed: BTreeSet<usize> = sweep(&vec![(); users.len()], room, model.epoch().get())
        .into_iter()
        .collect();

    for (i, user) in users.iter().enumerate() {
        let group = core_group_key(&model, view.group_of(*user));
        assert_eq!(group, r.group(*user), "group of {user:?}");
        assert_eq!(view.has_member(*user), r.members.contains_key(user));
        for channel in &channels {
            let visible = r.can_view(*user, *channel);
            assert_eq!(
                view.visible(*user, *channel),
                visible,
                "visible {user:?} {channel:?}"
            );
            if visible.contains(Visible::VIEW) {
                sees.entry(*user).or_default().insert(*channel);
            }
        }
        if !detailed.contains(&i) {
            continue;
        }
        assert_eq!(
            view.viewable(*user, &mut meter),
            r.viewable(*user),
            "viewable of {user:?}"
        );
        let (_, set) = view.view_set(*user, &mut meter);
        let order: Vec<ChannelId> = r.channels.iter().map(|c| c.id).collect();
        let from_bits: BTreeSet<ChannelId> = order
            .iter()
            .enumerate()
            .filter(|(i, _)| set.get(i / 64).is_some_and(|w| w >> (i % 64) & 1 == 1))
            .map(|(_, c)| *c)
            .collect();
        let want: BTreeSet<ChannelId> = r.viewable(*user).into_iter().collect();
        assert_eq!(from_bits, want, "view set of {user:?}");
        assert_eq!(
            view.viewable_channels_reply(*user, &mut meter),
            r.viewable_reply(*user).into_bytes()
        );
        for scope in
            std::iter::once(Scope::Guild).chain(channels.iter().map(|c| Scope::Channel(*c)))
        {
            let got = view.permissions(*user, scope);
            let want = r.permissions(*user, scope);
            match (got, want) {
                (Ok(a), Ok(b)) => assert_eq!(a, b, "permissions {user:?} {scope:?}"),
                (Err(PermsError::MalformedMember(u)), Err(Malformed)) => assert_eq!(u, *user),
                (a, b) => panic!("permissions {user:?} {scope:?}: {a:?} vs {b:?}"),
            }
            assert_eq!(
                view.user_permissions_reply(*user, scope).ok(),
                r.permissions_reply(*user, scope).map(String::into_bytes)
            );
            for bits in [
                Permissions::VIEW_CHANNEL,
                Permissions::MANAGE_ROLES | Permissions::CONNECT,
                Permissions::empty(),
            ] {
                let got = view.check_permission_reply(*user, scope, bits).ok();
                assert_eq!(
                    got,
                    r.check_reply(*user, scope, bits).map(String::into_bytes)
                );
            }
        }
        for channel in &channels {
            for ms in [0i64, 500, 5000] {
                assert_eq!(
                    view.message_access(*user, *channel, ms),
                    r.message_access(*user, *channel, ms),
                    "message access {user:?} {channel:?} {ms}"
                );
            }
        }
    }

    let exception_users: Vec<UserId> = users
        .iter()
        .copied()
        .filter(|u| r.group(*u) == Some(RefGroup::User(*u)))
        .collect();
    let mut needs = vec![
        Need::View,
        Need::ViewMembers,
        Need::MessageAccess(400),
        Need::History(0),
        Need::History(400),
        Need::History(5000),
        Need::ChannelBits(Permissions::MANAGE_CHANNELS),
        Need::GuildBits(Permissions::VIEW_AUDIT_LOG),
    ];
    if let Cutoff::AtMillis(c) = r.cutoff {
        needs.extend([Need::History(c - 1), Need::History(c)]);
    }
    for channel in &channels {
        assert_eq!(
            view.visitor_permissions(*channel),
            r.visitor_permissions(*channel),
            "visitor permissions {channel:?}"
        );
        for need in needs.iter().copied() {
            let audience = view.audience(*channel, need, &mut meter).unwrap();
            for (set, _) in model.rolesets().iter() {
                let key = role_key(&model, set);
                assert_eq!(
                    audience.has_set(set),
                    r.roles_in_audience(&key, *channel, meets_need(need)),
                    "audience {need:?} {channel:?} set {set:?}"
                );
            }
            let mut want_users = Vec::new();
            for user in &exception_users {
                if r.in_audience(*user, *channel, meets_need(need)) {
                    want_users.push(*user);
                }
            }
            assert_eq!(
                audience.users(),
                want_users.as_slice(),
                "audience users {need:?} {channel:?}"
            );
        }
        let viewers: BTreeSet<UserId> = view.viewers(*channel, &mut meter).into_iter().collect();
        assert_eq!(viewers, r.viewers(*channel), "viewers {channel:?}");
        let filtered = view.filter_viewers(*channel, &users, &mut meter);
        let want: Vec<UserId> = users
            .iter()
            .copied()
            .filter(|u| r.can_view(*u, *channel).contains(Visible::VIEW))
            .collect();
        assert_eq!(filtered, want);
        let mentioned: Vec<UserId> = users.iter().copied().step_by(3).collect();
        for role in r.roles.iter().map(|spec| spec.id) {
            let pruned = view.push_prune(
                &PushPrune {
                    channel: *channel,
                    mentioned: &mentioned,
                    roles: &[role],
                    everyone: false,
                },
                &mut meter,
            );
            let mut want = view.role_set_members(&[role], &mut meter);
            want.extend_from_slice(&mentioned);
            want.sort_unstable();
            want.dedup();
            want.retain(|u| r.can_view(*u, *channel).contains(Visible::VIEW));
            assert_eq!(pruned.immediate, want, "push prune {channel:?} {role:?}");
            assert!(pruned.held.is_empty());
        }
    }
    let shareable: BTreeSet<ChannelId> = r
        .channels
        .iter()
        .filter(|c| c.kind != ChannelKind::Category)
        .map(|c| c.id)
        .collect();
    let none = BTreeSet::new();
    let owns: Vec<Vec<ChannelId>> = users
        .iter()
        .map(|u| {
            r.viewable(*u)
                .into_iter()
                .filter(|c| shareable.contains(c))
                .collect()
        })
        .collect();
    let picked = sweep(&owns, extra, model.epoch().get());
    for &i in &picked {
        let (subscriber, own) = (&users[i], &owns[i]);
        let want: Vec<bool> = users
            .iter()
            .map(|t| {
                let seen = sees.get(t).unwrap_or(&none);
                r.members.contains_key(t) && own.iter().any(|c| seen.contains(c))
            })
            .collect();
        let mut metered = Meter::new();
        let got = view.shares_view_many(*subscriber, &users, &mut metered);
        assert!(metered.spent() <= view.estimate(Read::Shares(users.len())));
        assert_eq!(got, want, "shares of {subscriber:?}");
    }
    let pace = users.len().div_ceil(extra.max(1)).max(3);
    for subscriber in picked.iter().step_by(5).map(|&i| &users[i]) {
        for target in users.iter().step_by(pace) {
            assert_eq!(
                view.shares_view(*subscriber, *target, &mut meter),
                r.shares_view(*subscriber, *target),
                "share {subscriber:?} {target:?}"
            );
        }
    }

    for channel in channels.iter().take(3) {
        let chunked: BTreeSet<UserId> = driver
            .shared
            .viewers_chunked(*channel)
            .unwrap()
            .into_iter()
            .collect();
        assert_eq!(chunked, r.viewers(*channel));
    }

    let mut roles: Vec<RoleId> = (1..=driver.world.params.roles).map(RoleId::new).collect();
    roles.extend([
        RoleId::new(GUILD),
        RoleId::new(900),
        RoleId::new(UNKNOWN_ROLE),
    ]);
    for role in &roles {
        assert_eq!(
            view.members_with_role(*role, &mut meter),
            r.members_with_role(*role),
            "members with {role:?}"
        );
        for user in &users {
            match (
                view.can_manage_roles(*user, *role),
                r.can_manage_roles(*user, *role),
            ) {
                (Ok(a), Ok(b)) => assert_eq!(a, b, "can_manage_roles {user:?} {role:?}"),
                (Err(_), Err(Malformed)) => {}
                (a, b) => panic!("can_manage_roles {user:?} {role:?}: {a:?} vs {b:?}"),
            }
            assert_eq!(
                view.can_manage_role(*user, *role),
                r.can_manage_role(*user, *role),
                "can_manage_role {user:?} {role:?}"
            );
        }
    }
    for actor in users.iter().take(16) {
        for target in &users {
            assert_eq!(
                view.can_manage_member(*actor, *target),
                r.can_manage_member(*actor, *target)
            );
        }
    }

    for adult in [false, true] {
        assert_eq!(
            view.visitor_channels(adult, &mut meter),
            r.visitor_channels(adult),
            "visitors {adult}"
        );
    }
    let mut asked = channels.clone();
    asked.extend(channels.iter().rev().take(2));
    assert_eq!(
        view.everyone_mentions(&asked, &mut meter),
        r.everyone_mentions(&asked)
    );
    assert_eq!(
        view.first_everyone_viewable(&asked, &mut meter),
        r.first_everyone_viewable(&asked)
    );
    let rev: Vec<ChannelId> = asked.iter().rev().copied().collect();
    assert_eq!(
        view.first_everyone_viewable(&rev, &mut meter),
        r.first_everyone_viewable(&rev)
    );

    let online: Vec<UserId> = users.iter().copied().step_by(2).collect();
    let public = view.public_online(&online, &mut meter);
    assert_eq!(public, r.public_online(&online), "public online");
    for requester in users.iter().step_by(7).take(4) {
        let got = view.mutual_online(*requester, &online, &mut meter);
        assert_eq!(
            got,
            r.mutual_online(*requester, &online),
            "mutual {requester:?}"
        );
    }

    for user in users.iter().step_by(3) {
        for parent in channels.iter().step_by(2) {
            for mask in 0u8..8 {
                let access = ThreadAccess {
                    private: mask & 1 == 1,
                    member: mask & 2 == 2,
                    timed_out: mask & 4 == 4,
                };
                match (
                    view.thread_permissions(*user, *parent, access),
                    r.thread(*user, *parent, access),
                ) {
                    (Ok(a), Ok(b)) => assert_eq!(a, b, "thread {user:?} {parent:?} {access:?}"),
                    (Err(_), Err(Malformed)) => {}
                    (a, b) => panic!("thread {user:?} {parent:?}: {a:?} vs {b:?}"),
                }
            }
        }
    }
    for user in &users {
        assert_eq!(
            view.virtual_view(*user),
            r.virtual_access.get(user).cloned().unwrap_or_default()
        );
    }
}

pub fn subjects(driver: &Driver) -> Vec<Subject> {
    let users = driver.users();
    let mut list: Vec<Subject> = users.iter().map(|u| Subject::Member(*u)).collect();
    list.extend(users.iter().map(|u| Subject::NonMember(*u)));
    list.push(Subject::Visitor);
    list
}

pub fn ref_bits(r: &Reference, ids: &[ChannelId]) -> Vec<u64> {
    let ids: BTreeSet<ChannelId> = ids.iter().copied().collect();
    let mut words = vec![0u64; r.channels.len().div_ceil(64)];
    for (i, c) in r.channels.iter().enumerate() {
        if ids.contains(&c.id) {
            words[i / 64] |= 1u64 << (i % 64);
        }
    }
    words
}

pub fn overlaps(a: &[u64], b: &[u64]) -> bool {
    a.iter().zip(b).any(|(x, y)| x & y != 0)
}

pub fn check_viewsets(driver: &Driver) {
    check_viewsets_over(driver, usize::MAX);
}

pub fn check_viewsets_over(driver: &Driver, extra: usize) {
    let view = driver.shared.view().unwrap();
    let r = &driver.reference;
    let subjects = subjects(driver);
    let sets: Vec<Vec<u64>> = subjects.iter().map(|s| r.view_set(s)).collect();
    for (subject, want) in subjects.iter().zip(&sets) {
        let (epoch, got) = view.subject_view_set(subject, &mut Meter::new());
        assert_eq!(epoch, view.model().epoch());
        assert_eq!(got.to_vec(), *want, "view set of {subject:?}");
    }
    let keys: Vec<(bool, &[u64])> = subjects
        .iter()
        .zip(&sets)
        .map(|(s, set)| {
            let member = matches!(s, Subject::Member(u) if r.members.contains_key(u));
            (member, &set[..])
        })
        .collect();
    for (n, t) in sweep(&keys, extra, view.model().epoch().get())
        .into_iter()
        .enumerate()
    {
        let target = &subjects[t];
        let user = match target {
            Subject::Member(u) if r.members.contains_key(u) => Some(*u),
            _ => None,
        };
        let want: Vec<bool> = subjects
            .iter()
            .zip(&sets)
            .map(|(c, set)| user.is_some_and(|u| c.user() != Some(u) && overlaps(set, &sets[t])))
            .collect();
        if n % 23 == 0 {
            assert_eq!(
                r.filter_sharing(target, &subjects),
                want,
                "reference filter_sharing {target:?}"
            );
        }
        let mut got = Vec::with_capacity(subjects.len());
        for chunk in subjects.chunks(MAX_CANDIDATES) {
            let estimate = view.estimate(Read::FilterSharing(chunk.len()));
            let mut meter = Meter::new();
            got.extend(view.filter_sharing(target, chunk, &mut meter).unwrap());
            assert!(
                meter.spent() <= estimate,
                "filter_sharing spent {:?} above {estimate:?}",
                meter.spent()
            );
        }
        assert_eq!(got, want, "filter_sharing {target:?}");
    }
}

pub fn check_viewsets_audit(driver: &Driver) {
    let view = driver.shared.view().unwrap();
    let model = view.model();
    let r = &driver.reference;
    let audit = view.view_sets_audit();
    let words = r.channels.len().div_ceil(64);
    assert_eq!(audit.sets.len(), audit.refs.len());
    let distinct: BTreeSet<&[u64]> = audit.sets.iter().map(|s| &s[..]).collect();
    assert_eq!(distinct.len(), audit.sets.len(), "a set interned twice");
    let mut refs = vec![0u32; audit.sets.len()];
    for (key, stamp, id) in &audit.keys {
        refs[id.index()] += 1;
        let set = audit.sets[id.index()].to_vec();
        assert_eq!(set.len(), words, "width of {key:?}");
        let want = match key {
            ViewKey::Visitor => {
                assert_eq!(*stamp, None);
                r.view_set(&Subject::Visitor)
            }
            ViewKey::RoleSet(roleset) => {
                assert_eq!(*stamp, None);
                ref_bits(r, &r.roles_view(&role_key(model, *roleset)))
            }
            ViewKey::User(user) => {
                let current = view
                    .members()
                    .get(*user)
                    .map(|e| (e.roleset(), e.is_quarantined()));
                assert!(stamp.is_some(), "per-user key {user:?} without a stamp");
                if current != *stamp {
                    continue;
                }
                r.view_set(&Subject::Member(*user))
            }
        };
        assert_eq!(set, want, "interned set of {key:?}");
    }
    assert_eq!(refs, audit.refs, "interned refcounts");
    assert!(audit.memo.len() <= PAIR_MEMO_LIMIT);
    for ((a, b), hit) in &audit.memo {
        assert!(a <= b, "memo pair {a:?} {b:?} not ordered");
        let (x, y) = (&audit.sets[a.index()], &audit.sets[b.index()]);
        assert_eq!(*hit, overlaps(x, y), "memo {a:?} {b:?}");
    }
}

pub fn audited(driver: &Driver) {
    let view = driver.shared.view().unwrap();
    if let Err(e) = audit(view.model(), view.members()) {
        panic!("audit: {e}");
    }
}

pub fn fresh_epoch(driver: &Driver, last: &Model) {
    let view = driver.shared.view().unwrap();
    if view.model().epoch() != last.epoch() {
        assert_eq!(
            view.view_sets_audit(),
            Default::default(),
            "view sets survived into {:?}",
            view.model().epoch()
        );
    }
}

pub fn unchanged_view_sets(driver: &Driver, old: &Reference) {
    let new = &driver.reference;
    for subject in subjects(driver) {
        if let Some(user) = subject.user()
            && old.members.get(&user) != new.members.get(&user)
        {
            continue;
        }
        let a: BTreeSet<ChannelId> = old.view_ids(&subject).into_iter().collect();
        let b: BTreeSet<ChannelId> = new.view_ids(&subject).into_iter().collect();
        assert_eq!(
            a, b,
            "view set of {subject:?} moved without viewsets_changed"
        );
    }
}

pub fn view_set_ids(driver: &Driver) -> Vec<Option<ViewSetId>> {
    let view = driver.shared.view().unwrap();
    let pool = subjects(driver);
    for target in pool.iter().step_by(5) {
        for chunk in pool.chunks(MAX_CANDIDATES) {
            view.filter_sharing(target, chunk, &mut Meter::new())
                .unwrap();
        }
    }
    check_viewsets_audit(driver);
    let ids = pool
        .iter()
        .map(|s| view.view_set_id(s, &mut Meter::new()))
        .collect();
    check_viewsets_audit(driver);
    ids
}

pub fn slot_classes(model: &Model) -> Vec<Option<ClassId>> {
    (0..model.channels().slots())
        .map(|i| model.class_of(ChannelIdx::from_index(i).unwrap()))
        .collect()
}

pub fn replay(last: &Model, delta: &Delta, current: &Model) {
    let mut slots = slot_classes(last);
    for (idx, class) in &delta.dropped {
        assert_eq!(slots[idx.index()].take(), Some(*class), "dropped {idx:?}");
    }
    if let Some(map) = &delta.channel_remap {
        let mut moved = vec![None; current.channels().slots()];
        for (i, class) in slots.iter().enumerate() {
            if let Some(class) = class {
                let target = map[i];
                assert_ne!(target, DEAD, "slot {i} lost without a drop");
                moved[target as usize] = Some(*class);
            }
        }
        slots = moved;
    } else {
        assert!(delta.dropped.is_empty());
        slots.resize(current.channels().slots(), None);
    }
    for (idx, a, b) in &delta.moves {
        assert_eq!(slots[idx.index()], *a, "move {idx:?}");
        slots[idx.index()] = *b;
    }
    assert_eq!(slots, slot_classes(current));
}

pub type Trace = Vec<(
    Vec<(ChannelId, Option<ClassId>)>,
    Delta,
    Vec<Option<ViewSetId>>,
)>;

pub fn trace(mut driver: Driver, steps: &[Vec<Op>]) -> Trace {
    audited(&driver);
    view_set_ids(&driver);
    let mut last = driver.shared.model().unwrap();
    let mut older = last.clone();
    let mut trace = Vec::new();
    for ops in steps {
        let (before, effects) = apply_step(&mut driver, ops);
        assert!(effects.spent() <= effects.estimate());
        fresh_epoch(&driver, &before.model);
        if !effects.delta().viewsets_changed {
            unchanged_view_sets(&driver, &before.reference);
        }
        check_viewsets_audit(&driver);
        view_set_ids(&driver);
        audited(&driver);
        if let Err(e) = audit_class_ids(&before.model, effects.after()) {
            panic!("{e}");
        }
        assert_eq!(effects.delta(), diff(effects.before(), effects.after()));
        if let Some(done) = maybe_compact(&mut driver, &effects) {
            fresh_epoch(&driver, done.before());
            audited(&driver);
            let delta = done.delta();
            assert!(delta.remap.is_some() || delta.channel_remap.is_some());
            if delta.remap.is_none() {
                assert!(delta.moves.is_empty() && delta.created.is_empty());
                assert!(delta.freed.is_empty() && delta.dropped.is_empty());
            }
            assert!(
                delta.classes.iter().all(|c| c.toggled.is_empty()
                    && c.flipped.is_empty()
                    && c.overrides.is_empty())
            );
            check_class_rows(done.before(), done.after(), &delta);
        }
        let current = driver.shared.model().unwrap();
        let delta = diff(&last, &current);
        assert!(!delta.resync);
        replay(&last, &delta, &current);
        check_class_rows(&last, &current, &delta);
        let lagging = diff(&older, &current);
        if !lagging.resync {
            replay(&older, &lagging, &current);
            check_class_rows(&older, &current, &lagging);
        }
        let ids = driver
            .reference
            .channels
            .iter()
            .map(|c| (c.id, current.class_of_id(c.id)))
            .collect();
        trace.push((ids, delta, view_set_ids(&driver)));
        older = core::mem::replace(&mut last, current);
    }
    trace
}
