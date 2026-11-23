// SPDX-License-Identifier: AGPL-3.0-or-later

use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Cutoff, Event, GuildId, Hoist, Nsfw,
    OverwriteSpec, OverwriteTarget, Permissions, RankPos, RoleId, RoleList, RoleSpec, Snapshot,
    UserId,
};

use super::{Driver, Op, Params};

pub const MAX_STEPS: usize = 150;
pub const MAX_STEP_OPS: u8 = 16;
pub const MAX_MEMBERS: u16 = 2048;
pub const MAX_LIST: u8 = 64;
pub const MAX_OVERWRITES: u8 = 32;
pub const MAX_IDS: u8 = 16;

const BUDGETS: [u64; 2] = [250_000, 4_000];
const FLOORS: [u32; 2] = [4096, 2];
const CACHES: [u64; 3] = [1 << 20, 0, 96];

#[derive(Clone, Debug)]
pub struct Case {
    pub params: Params,
    pub snapshot: Option<Snapshot>,
    pub steps: Vec<Vec<Op>>,
}

impl Case {
    pub fn driver(&self) -> Driver {
        Driver::start(&self.params, self.snapshot.as_ref())
    }

    pub fn step_cost(&self) -> u64 {
        let channels = self.params.channels
            + self
                .snapshot
                .as_ref()
                .map_or(0, |s| s.channels.len() as u64);
        let members = self.snapshot.as_ref().map_or(0, |s| s.members.len() as u64);
        (self.params.user_pool() + members) * (channels + 1)
    }

    pub fn bounded(mut self, work: u64) -> Case {
        let steps = (work / self.step_cost().max(1)).max(1);
        self.steps
            .truncate(usize::try_from(steps).unwrap_or(usize::MAX));
        self
    }
}

pub struct Reader<'a> {
    data: &'a [u8],
    at: usize,
}

impl<'a> Reader<'a> {
    pub fn new(data: &'a [u8]) -> Reader<'a> {
        Reader { data, at: 0 }
    }

    pub fn done(&self) -> bool {
        self.at >= self.data.len()
    }

    pub fn byte(&mut self) -> u8 {
        let b = self.data.get(self.at).copied().unwrap_or(0);
        self.at += 1;
        b
    }

    pub fn below(&mut self, n: u8) -> u8 {
        self.byte() % n
    }

    pub fn flag(&mut self) -> bool {
        self.byte() & 1 == 1
    }

    pub fn u16(&mut self) -> u16 {
        u16::from_le_bytes([self.byte(), self.byte()])
    }

    pub fn u64(&mut self) -> u64 {
        let mut b = [0u8; 8];
        for x in &mut b {
            *x = self.byte();
        }
        u64::from_le_bytes(b)
    }

    pub fn varint(&mut self) -> u64 {
        let mut value = 0u64;
        for i in 0..10 {
            let b = self.byte();
            value |= u64::from(b & 0x7f).wrapping_shl(7 * i);
            if b & 0x80 == 0 {
                break;
            }
        }
        value
    }

    pub fn ivarint(&mut self) -> i64 {
        let v = self.varint();
        ((v >> 1) as i64) ^ -((v & 1) as i64)
    }

    pub fn len(&mut self, max: u8) -> usize {
        usize::from(self.byte() % (max + 1))
    }
}

#[derive(Default)]
pub struct Writer {
    pub out: Vec<u8>,
}

impl Writer {
    pub fn byte(&mut self, b: u8) {
        self.out.push(b);
    }

    pub fn flag(&mut self, f: bool) {
        self.out.push(u8::from(f));
    }

    pub fn u16(&mut self, v: u16) {
        self.out.extend_from_slice(&v.to_le_bytes());
    }

    pub fn u64(&mut self, v: u64) {
        self.out.extend_from_slice(&v.to_le_bytes());
    }

    pub fn varint(&mut self, mut v: u64) {
        loop {
            let b = (v & 0x7f) as u8;
            v >>= 7;
            if v == 0 {
                self.out.push(b);
                return;
            }
            self.out.push(b | 0x80);
        }
    }

    pub fn ivarint(&mut self, v: i64) {
        self.varint(((v << 1) ^ (v >> 63)) as u64);
    }

    pub fn len(&mut self, n: usize, max: u8) {
        assert!(n <= usize::from(max), "list of {n} above {max}");
        self.out.push(n as u8);
    }
}

fn index_of<T: PartialEq>(items: &[T], value: &T) -> u8 {
    items
        .iter()
        .position(|x| x == value)
        .unwrap_or_else(|| panic!("value outside the encodable set")) as u8
}

pub fn params(r: &mut Reader<'_>) -> Params {
    let roles = 1 + u64::from(r.below(30));
    let channels = 1 + u64::from(r.below(40));
    let members = 1 + u64::from(r.u16() % MAX_MEMBERS);
    let seed = r.u64();
    let config = r.byte();
    Params {
        roles,
        channels,
        members,
        seed,
        budget: BUDGETS[usize::from(config & 1)],
        floor: FLOORS[usize::from((config >> 1) & 1)],
        cache: CACHES[usize::from((config >> 2) % 3)],
    }
}

pub fn write_params(w: &mut Writer, p: &Params) {
    assert!((1..=30).contains(&p.roles) && (1..=40).contains(&p.channels));
    assert!((1..=u64::from(MAX_MEMBERS)).contains(&p.members));
    w.byte((p.roles - 1) as u8);
    w.byte((p.channels - 1) as u8);
    w.u16((p.members - 1) as u16);
    w.u64(p.seed);
    w.byte(
        index_of(&BUDGETS, &p.budget)
            | (index_of(&FLOORS, &p.floor) << 1)
            | (index_of(&CACHES, &p.cache) << 2),
    );
}

fn option<T>(r: &mut Reader<'_>, f: impl FnOnce(&mut Reader<'_>) -> T) -> Option<T> {
    if r.flag() { Some(f(r)) } else { None }
}

fn write_option<T>(w: &mut Writer, v: Option<T>, f: impl FnOnce(&mut Writer, T)) {
    w.flag(v.is_some());
    if let Some(v) = v {
        f(w, v);
    }
}

fn ids(r: &mut Reader<'_>, max: u8) -> Vec<u64> {
    let n = r.len(max);
    (0..n).map(|_| r.varint()).collect()
}

fn write_ids(w: &mut Writer, ids: impl ExactSizeIterator<Item = u64>, max: u8) {
    w.len(ids.len(), max);
    for id in ids {
        w.varint(id);
    }
}

fn channel_ids(r: &mut Reader<'_>, max: u8) -> Vec<ChannelId> {
    ids(r, max).into_iter().map(ChannelId::new).collect()
}

fn write_channel_ids(w: &mut Writer, ids: &[ChannelId], max: u8) {
    write_ids(w, ids.iter().map(|c| c.get()), max);
}

pub fn role_spec(r: &mut Reader<'_>) -> RoleSpec {
    let id = RoleId::new(r.varint());
    let permissions = Permissions::from_bits_retain(r.varint());
    let position = if r.flag() {
        RankPos::Int(r.ivarint())
    } else {
        RankPos::Undefined
    };
    let hoist = match r.below(3) {
        0 => Hoist::NotHoisted,
        1 => Hoist::Hoisted(None),
        _ => Hoist::Hoisted(Some(r.ivarint())),
    };
    RoleSpec {
        id,
        permissions,
        position,
        hoist,
    }
}

pub fn write_role_spec(w: &mut Writer, s: &RoleSpec) {
    w.varint(s.id.get());
    w.varint(s.permissions.bits());
    match s.position {
        RankPos::Int(at) => {
            w.flag(true);
            w.ivarint(at);
        }
        RankPos::Undefined => w.flag(false),
    }
    match s.hoist {
        Hoist::NotHoisted => w.byte(0),
        Hoist::Hoisted(None) => w.byte(1),
        Hoist::Hoisted(Some(at)) => {
            w.byte(2);
            w.ivarint(at);
        }
    }
}

fn kind(r: &mut Reader<'_>) -> ChannelKind {
    match r.below(8) {
        0 => ChannelKind::Text,
        1 => ChannelKind::Voice,
        2 => ChannelKind::Category,
        3 => ChannelKind::Announcement,
        4 => ChannelKind::Forum,
        5 => ChannelKind::Media,
        6 => ChannelKind::Thread(r.byte()),
        _ => ChannelKind::Other(r.varint() as u32),
    }
}

fn write_kind(w: &mut Writer, k: ChannelKind) {
    match k {
        ChannelKind::Text => w.byte(0),
        ChannelKind::Voice => w.byte(1),
        ChannelKind::Category => w.byte(2),
        ChannelKind::Announcement => w.byte(3),
        ChannelKind::Forum => w.byte(4),
        ChannelKind::Media => w.byte(5),
        ChannelKind::Thread(code) => {
            w.byte(6);
            w.byte(code);
        }
        ChannelKind::Other(code) => {
            w.byte(7);
            w.varint(u64::from(code));
        }
    }
}

const NSFW: [Nsfw; 3] = [Nsfw::Inherit, Nsfw::Safe, Nsfw::Adult];

pub fn channel_spec(r: &mut Reader<'_>) -> ChannelSpec {
    let id = ChannelId::new(r.varint());
    let kind = kind(r);
    let parent = option(r, |r| ChannelId::new(r.varint()));
    let n = r.len(MAX_OVERWRITES);
    let overwrites = (0..n)
        .map(|_| {
            let target = if r.flag() {
                OverwriteTarget::Member(UserId::new(r.varint()))
            } else {
                OverwriteTarget::Role(RoleId::new(r.varint()))
            };
            OverwriteSpec {
                target,
                allow: Permissions::from_bits_retain(r.varint()),
                deny: Permissions::from_bits_retain(r.varint()),
            }
        })
        .collect();
    let nsfw = NSFW[usize::from(r.below(3))];
    let named = r.flag();
    ChannelSpec {
        id,
        kind,
        parent,
        overwrites,
        nsfw,
        named,
    }
}

pub fn write_channel_spec(w: &mut Writer, s: &ChannelSpec) {
    w.varint(s.id.get());
    write_kind(w, s.kind);
    write_option(w, s.parent, |w, p| w.varint(p.get()));
    w.len(s.overwrites.len(), MAX_OVERWRITES);
    for o in &s.overwrites {
        match o.target {
            OverwriteTarget::Member(u) => {
                w.flag(true);
                w.varint(u.get());
            }
            OverwriteTarget::Role(role) => {
                w.flag(false);
                w.varint(role.get());
            }
        }
        w.varint(o.allow.bits());
        w.varint(o.deny.bits());
    }
    w.byte(index_of(&NSFW, &s.nsfw));
    w.flag(s.named);
}

fn role_list(r: &mut Reader<'_>) -> RoleList {
    let b = r.byte();
    if b == 255 {
        return RoleList::Malformed;
    }
    let n = b % (MAX_IDS + 1);
    RoleList::Roles((0..n).map(|_| RoleId::new(r.varint())).collect())
}

fn write_role_list(w: &mut Writer, list: &RoleList) {
    match list {
        RoleList::Malformed => w.byte(255),
        RoleList::Roles(ids) => write_ids(w, ids.iter().map(|x| x.get()), MAX_IDS),
    }
}

fn cutoff(r: &mut Reader<'_>) -> Cutoff {
    match r.below(3) {
        0 => Cutoff::Missing,
        1 => Cutoff::Malformed,
        _ => Cutoff::AtMillis(r.ivarint()),
    }
}

fn write_cutoff(w: &mut Writer, c: Cutoff) {
    match c {
        Cutoff::Missing => w.byte(0),
        Cutoff::Malformed => w.byte(1),
        Cutoff::AtMillis(ms) => {
            w.byte(2);
            w.ivarint(ms);
        }
    }
}

const FLAGS: [Option<bool>; 3] = [None, Some(false), Some(true)];

pub fn event(r: &mut Reader<'_>) -> Event {
    match r.below(11) {
        0 => {
            let n = r.len(MAX_LIST);
            Event::Roles((0..n).map(|_| role_spec(r)).collect())
        }
        1 => Event::Owner(option(r, |r| UserId::new(r.varint()))),
        2 => {
            let spec = channel_spec(r);
            let cause = if r.flag() {
                ChannelCause::Created
            } else {
                ChannelCause::Updated
            };
            Event::ChannelUpsert(spec, cause)
        }
        3 => Event::ChannelDelete(ChannelId::new(r.varint())),
        4 => Event::ChannelOrder(channel_ids(r, MAX_LIST)),
        5 => {
            let user = UserId::new(r.varint());
            Event::MemberUpsert(user, role_list(r))
        }
        6 => Event::MemberRemove(UserId::new(r.varint())),
        7 => {
            let user = UserId::new(r.varint());
            Event::VirtualAccess(user, channel_ids(r, MAX_IDS))
        }
        8 => Event::HistoryCutoff(cutoff(r)),
        9 => {
            let nsfw = FLAGS[usize::from(r.below(3))];
            Event::GuildNsfw(nsfw, r.byte())
        }
        _ => {
            r.flag();
            Event::ChannelOrder(Vec::new())
        }
    }
}

pub fn write_event(w: &mut Writer, e: &Event) {
    match e {
        Event::Roles(roles) => {
            w.byte(0);
            w.len(roles.len(), MAX_LIST);
            for s in roles {
                write_role_spec(w, s);
            }
        }
        Event::Owner(owner) => {
            w.byte(1);
            write_option(w, *owner, |w, u| w.varint(u.get()));
        }
        Event::ChannelUpsert(spec, cause) => {
            w.byte(2);
            write_channel_spec(w, spec);
            w.flag(*cause == ChannelCause::Created);
        }
        Event::ChannelDelete(id) => {
            w.byte(3);
            w.varint(id.get());
        }
        Event::ChannelOrder(ids) if ids.is_empty() => {
            w.byte(10);
            w.flag(false);
        }
        Event::ChannelOrder(ids) => {
            w.byte(4);
            write_channel_ids(w, ids, MAX_LIST);
        }
        Event::MemberUpsert(user, list) => {
            w.byte(5);
            w.varint(user.get());
            write_role_list(w, list);
        }
        Event::MemberRemove(user) => {
            w.byte(6);
            w.varint(user.get());
        }
        Event::VirtualAccess(user, ids) => {
            w.byte(7);
            w.varint(user.get());
            write_channel_ids(w, ids, MAX_IDS);
        }
        Event::HistoryCutoff(c) => {
            w.byte(8);
            write_cutoff(w, *c);
        }
        Event::GuildNsfw(nsfw, level) => {
            w.byte(9);
            w.byte(index_of(&FLAGS, nsfw));
            w.byte(*level);
        }
    }
}

pub fn snapshot(r: &mut Reader<'_>) -> Snapshot {
    let guild = GuildId::new(r.varint());
    let owner = option(r, |r| UserId::new(r.varint()));
    let mut s = Snapshot::new(guild, owner);
    let n = r.len(MAX_LIST);
    s.roles = (0..n).map(|_| role_spec(r)).collect();
    let n = r.len(MAX_LIST);
    s.channels = (0..n).map(|_| channel_spec(r)).collect();
    let n = r.u16() % (MAX_MEMBERS + 1);
    s.members = (0..n)
        .map(|_| {
            let user = UserId::new(r.varint());
            (user, role_list(r))
        })
        .collect();
    let n = r.len(MAX_IDS);
    s.virtual_access = (0..n)
        .map(|_| {
            let user = UserId::new(r.varint());
            (user, channel_ids(r, MAX_IDS))
        })
        .collect();
    s.cutoff = cutoff(r);
    s.nsfw = FLAGS[usize::from(r.below(3))];
    s.nsfw_level = r.byte();
    r.flag();
    s
}

pub fn write_snapshot(w: &mut Writer, s: &Snapshot) {
    w.varint(s.guild.get());
    write_option(w, s.owner, |w, u| w.varint(u.get()));
    w.len(s.roles.len(), MAX_LIST);
    for role in &s.roles {
        write_role_spec(w, role);
    }
    w.len(s.channels.len(), MAX_LIST);
    for channel in &s.channels {
        write_channel_spec(w, channel);
    }
    assert!(s.members.len() <= usize::from(MAX_MEMBERS));
    w.u16(s.members.len() as u16);
    for (user, list) in &s.members {
        w.varint(user.get());
        write_role_list(w, list);
    }
    w.len(s.virtual_access.len(), MAX_IDS);
    for (user, ids) in &s.virtual_access {
        w.varint(user.get());
        write_channel_ids(w, ids, MAX_IDS);
    }
    write_cutoff(w, s.cutoff);
    w.byte(index_of(&FLAGS, &s.nsfw));
    w.byte(s.nsfw_level);
    w.flag(false);
}

pub fn op(r: &mut Reader<'_>) -> Op {
    match r.below(18) {
        0 => Op::Roles(r.u64()),
        1 => Op::RoleEdit(r.byte(), r.u64()),
        2 => Op::EveryoneEdit(r.u64()),
        3 => Op::RolePosition(r.byte(), r.byte() as i8),
        4 => Op::Owner(option(r, |r| r.u16())),
        5 => Op::ChannelCreate(r.u64()),
        6 => Op::ChannelEdit(r.u16(), r.u64()),
        7 => Op::Reparent(r.u16(), r.u16()),
        8 => Op::ChannelDelete(r.u16()),
        9 => Op::ChannelOrder(r.u64()),
        10 => Op::MemberUpsert(r.u16(), r.u64()),
        11 => Op::MemberRemove(r.u16()),
        12 => {
            let user = r.u16();
            let n = r.below(3);
            Op::VirtualAccess(user, (0..n).map(|_| r.u16()).collect())
        }
        13 => Op::Cutoff(r.byte()),
        14 => Op::Nsfw(r.byte()),
        15 => {
            r.flag();
            Op::Raw(Event::ChannelOrder(Vec::new()))
        }
        16 => Op::Category(r.byte(), r.u16()),
        _ => Op::Raw(event(r)),
    }
}

pub fn write_op(w: &mut Writer, op: &Op) {
    match op {
        Op::Roles(seed) => {
            w.byte(0);
            w.u64(*seed);
        }
        Op::RoleEdit(role, seed) => {
            w.byte(1);
            w.byte(*role);
            w.u64(*seed);
        }
        Op::EveryoneEdit(seed) => {
            w.byte(2);
            w.u64(*seed);
        }
        Op::RolePosition(role, at) => {
            w.byte(3);
            w.byte(*role);
            w.byte(*at as u8);
        }
        Op::Owner(owner) => {
            w.byte(4);
            write_option(w, *owner, Writer::u16);
        }
        Op::ChannelCreate(seed) => {
            w.byte(5);
            w.u64(*seed);
        }
        Op::ChannelEdit(c, seed) => {
            w.byte(6);
            w.u16(*c);
            w.u64(*seed);
        }
        Op::Reparent(c, p) => {
            w.byte(7);
            w.u16(*c);
            w.u16(*p);
        }
        Op::ChannelDelete(c) => {
            w.byte(8);
            w.u16(*c);
        }
        Op::ChannelOrder(seed) => {
            w.byte(9);
            w.u64(*seed);
        }
        Op::MemberUpsert(u, seed) => {
            w.byte(10);
            w.u16(*u);
            w.u64(*seed);
        }
        Op::MemberRemove(u) => {
            w.byte(11);
            w.u16(*u);
        }
        Op::VirtualAccess(u, picks) => {
            w.byte(12);
            w.u16(*u);
            w.len(picks.len(), 2);
            for p in picks {
                w.u16(*p);
            }
        }
        Op::Cutoff(x) => {
            w.byte(13);
            w.byte(*x);
        }
        Op::Nsfw(x) => {
            w.byte(14);
            w.byte(*x);
        }
        Op::Category(k, u) => {
            w.byte(16);
            w.byte(*k);
            w.u16(*u);
        }
        Op::Raw(Event::ChannelOrder(ids)) if ids.is_empty() => {
            w.byte(15);
            w.flag(false);
        }
        Op::Raw(e) => {
            w.byte(17);
            write_event(w, e);
        }
    }
}

pub fn decode(data: &[u8]) -> Case {
    let mut r = Reader::new(data);
    let explicit = r.flag();
    let params = params(&mut r);
    let snapshot = explicit.then(|| snapshot(&mut r));
    let mut steps = Vec::new();
    while !r.done() && steps.len() < MAX_STEPS {
        let n = 1 + r.below(MAX_STEP_OPS);
        steps.push((0..n).map(|_| op(&mut r)).collect());
    }
    Case {
        params,
        snapshot,
        steps,
    }
}

pub fn encode(case: &Case) -> Vec<u8> {
    let mut w = Writer::default();
    w.flag(case.snapshot.is_some());
    write_params(&mut w, &case.params);
    if let Some(s) = &case.snapshot {
        write_snapshot(&mut w, s);
    }
    assert!(case.steps.len() <= MAX_STEPS);
    for step in &case.steps {
        assert!((1..=usize::from(MAX_STEP_OPS)).contains(&step.len()));
        w.byte((step.len() - 1) as u8);
        for op in step {
            write_op(&mut w, op);
        }
    }
    w.out
}

pub fn stages(data: &[u8]) -> (u64, usize, Vec<(u64, u64)>) {
    let mut r = Reader::new(data);
    let x = r.u64();
    let split = usize::from(r.byte());
    let mut stages = Vec::new();
    while !r.done() && stages.len() < 32 {
        stages.push((r.u64(), r.u64()));
    }
    (x, split, stages)
}

pub fn encode_stages(x: u64, split: usize, stages: &[(u64, u64)]) -> Vec<u8> {
    assert!(split <= 255 && stages.len() <= 32);
    let mut w = Writer::default();
    w.u64(x);
    w.byte(split as u8);
    for (allow, deny) in stages {
        w.u64(*allow);
        w.u64(*deny);
    }
    w.out
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Reply {
    Permissions(u64),
    Check(bool),
    Channels(Vec<ChannelId>),
}

pub fn replies(data: &[u8]) -> (Snapshot, Vec<Reply>) {
    let mut r = Reader::new(data);
    let snapshot = snapshot(&mut r);
    let mut values = Vec::new();
    while !r.done() && values.len() < usize::from(MAX_LIST) {
        values.push(match r.below(3) {
            0 => Reply::Permissions(r.u64()),
            1 => Reply::Check(r.flag()),
            _ => Reply::Channels(channel_ids(&mut r, MAX_IDS)),
        });
    }
    (snapshot, values)
}

pub fn encode_replies(snapshot: &Snapshot, values: &[Reply]) -> Vec<u8> {
    assert!(values.len() <= usize::from(MAX_LIST));
    let mut w = Writer::default();
    write_snapshot(&mut w, snapshot);
    for v in values {
        match v {
            Reply::Permissions(bits) => {
                w.byte(0);
                w.u64(*bits);
            }
            Reply::Check(granted) => {
                w.byte(1);
                w.flag(*granted);
            }
            Reply::Channels(ids) => {
                w.byte(2);
                write_channel_ids(&mut w, ids, MAX_IDS);
            }
        }
    }
    w.out
}

pub const BUDGET_MEMBERS: [u64; 3] = [500, 3_000, 10_000];
pub const BUDGET_CHANNELS: [u64; 3] = [50, 200, 400];
pub const BUDGET_ROLES: [u64; 3] = [10, 50, 150];
pub const BUDGET_CALLS: usize = 32;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BudgetCase {
    pub members: u64,
    pub channels: u64,
    pub roles: u64,
    pub seed: u64,
    pub calls: Vec<(u8, u16, u64)>,
}

pub fn budget(data: &[u8]) -> BudgetCase {
    let mut r = Reader::new(data);
    let shape = r.byte();
    let seed = r.u64();
    let mut calls = Vec::new();
    while !r.done() && calls.len() < BUDGET_CALLS {
        calls.push((r.byte(), r.u16(), r.u64()));
    }
    BudgetCase {
        members: BUDGET_MEMBERS[usize::from(shape % 3)],
        channels: BUDGET_CHANNELS[usize::from((shape / 3) % 3)],
        roles: BUDGET_ROLES[usize::from((shape / 9) % 3)],
        seed,
        calls,
    }
}

pub fn encode_budget(case: &BudgetCase) -> Vec<u8> {
    assert!(case.calls.len() <= BUDGET_CALLS);
    let mut w = Writer::default();
    w.byte(
        index_of(&BUDGET_MEMBERS, &case.members)
            + 3 * index_of(&BUDGET_CHANNELS, &case.channels)
            + 9 * index_of(&BUDGET_ROLES, &case.roles),
    );
    w.u64(case.seed);
    for (kind, n, arg) in &case.calls {
        w.byte(*kind);
        w.u16(*n);
        w.u64(*arg);
    }
    w.out
}
