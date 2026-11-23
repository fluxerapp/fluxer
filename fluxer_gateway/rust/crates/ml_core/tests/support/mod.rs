// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use ml_core::reference::Reference;
use ml_core::{
    Change, Config, Connection, Core, CoreError, Done, EncodedItem, Event, Mode, PageQuery, Ranges,
    Sent, SessionKey, Snapshot, Status, Step, SyncScope, Timing, Updates,
};
use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Config as PermConfig, Event as PermEvent,
    GuildId, Hoist, Nsfw, OverwriteSpec, OverwriteTarget, Permissions, PermsError, PermsShared,
    RankPos, RoleId, RoleList, RoleSpec, Snapshot as PermSnapshot, UserId,
};

pub const GUILD: u64 = 1_000_000;
pub const CHANNELS: [u64; 8] = [9, 10, 100, 11, 2000, 99, 1000, 5];

pub const NAMES: &[&[u8]] = &[
    b"alice",
    b"Alice",
    b"ALICE",
    b"alicea",
    b"alice b",
    b"bob",
    b"",
    "ß".as_bytes(),
    b"ss",
    b"SS",
    "İ".as_bytes(),
    "i\u{307}".as_bytes(),
    "e\u{301}".as_bytes(),
    "é".as_bytes(),
    "Ärla".as_bytes(),
    "ärla".as_bytes(),
    b"abcdefgh",
    b"abcdefghX",
    b"abcdefghx",
    b"abcdefgh\x00",
    b"zz",
    b"user 10",
    b"user 2",
    &[0xff, 0x41],
    "Σ".as_bytes(),
    "σ".as_bytes(),
    "ς".as_bytes(),
    "\u{1F600}x".as_bytes(),
    "ﬃ".as_bytes(),
    "ＡＢＣ".as_bytes(),
    b"long",
    b"longer",
];

pub fn name(i: u8) -> Vec<u8> {
    match NAMES.get(i as usize).copied() {
        Some(b"long") => vec![b'a'; 600],
        Some(b"longer") => {
            let mut v = vec![b'a'; 599];
            v.push(b'B');
            v
        }
        Some(n) => n.to_vec(),
        None => b"x".to_vec(),
    }
}

pub type ChannelSetup = (u8, Option<u8>, Vec<(bool, u8, u8, u8)>);

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Setup {
    pub members: Vec<(u8, u8, Vec<u8>, u8, bool)>,
    pub roles: Vec<(u8, u8, Option<u8>)>,
    pub channels: Vec<ChannelSetup>,
    pub threshold: u8,
    pub budget: u8,
    pub fragments: u8,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Op {
    Upsert {
        user: u8,
        name: u8,
        roles: Vec<u8>,
        unchanged: bool,
    },
    Remove {
        user: u8,
    },
    Presence {
        user: u8,
        status: u8,
        unchanged: bool,
        deferred: bool,
    },
    Cleared {
        user: u8,
    },
    Connect {
        user: u8,
        changed: bool,
    },
    Disconnect {
        user: u8,
    },
    Role {
        role: u8,
        perms: u8,
        position: u8,
        hoist: Option<u8>,
        delete: bool,
    },
    Channel {
        channel: u8,
        parent: Option<u8>,
        overwrites: Vec<(bool, u8, u8, u8)>,
        delete: bool,
    },
    Owner {
        user: u8,
    },
    Virtual {
        user: u8,
        channels: Vec<u8>,
    },
    Subscribe {
        session: u8,
        user: u8,
        channel: u8,
        ranges: Vec<(u16, u8)>,
    },
    Widen {
        session: u8,
        user: u8,
        channel: u8,
        first: u16,
        gap: u16,
    },
    Unsubscribe {
        session: u8,
        channel: u8,
    },
    Drop {
        session: u8,
    },
    ViewGained {
        session: u8,
        channel: u8,
    },
    Flush,
    Updates {
        on: bool,
    },
    Join,
}

pub fn user_id(u: u8) -> UserId {
    UserId::new(500 + u64::from(u) * 7 % 24 * 3 + u64::from(u) / 24)
}

fn role_id(r: u8) -> RoleId {
    if r >= 8 {
        RoleId::new(GUILD)
    } else {
        RoleId::new(10 + u64::from(r))
    }
}

fn channel_id(c: u8) -> ChannelId {
    ChannelId::new(CHANNELS[(c % 8) as usize])
}

fn status(s: u8) -> Status {
    match s % 6 {
        0 => Status::Online,
        1 => Status::Idle,
        2 => Status::Dnd,
        3 => Status::Invisible,
        4 => Status::Offline,
        _ => Status::Other,
    }
}

fn bits(b: u8) -> Permissions {
    let mut p = Permissions::empty();
    if b & 1 != 0 {
        p |= Permissions::VIEW_CHANNEL;
    }
    if b & 2 != 0 {
        p |= Permissions::VIEW_CHANNEL_MEMBERS;
    }
    if b == 7 {
        p |= Permissions::ADMINISTRATOR;
    }
    p
}

fn role_spec(r: u8, perms: u8, position: u8, hoist: Option<u8>) -> RoleSpec {
    RoleSpec {
        id: role_id(r),
        permissions: bits(perms),
        position: if position == 5 {
            RankPos::Undefined
        } else {
            RankPos::Int(i64::from(position % 3))
        },
        hoist: match hoist {
            None => Hoist::NotHoisted,
            Some(5) => Hoist::Hoisted(None),
            Some(h) => Hoist::Hoisted(Some(i64::from(h % 3))),
        },
    }
}

fn channel_spec(c: u8, parent: Option<u8>, overwrites: &[(bool, u8, u8, u8)]) -> ChannelSpec {
    let category = c % 8 < 2;
    ChannelSpec {
        id: channel_id(c),
        kind: if category {
            ChannelKind::Category
        } else {
            ChannelKind::Text
        },
        parent: if category {
            None
        } else {
            parent.map(|p| channel_id(p % 2))
        },
        overwrites: overwrites
            .iter()
            .map(|(member, target, allow, deny)| OverwriteSpec {
                target: if *member {
                    OverwriteTarget::Member(user_id(*target))
                } else {
                    OverwriteTarget::Role(role_id(target % 9))
                },
                allow: bits(*allow & 3),
                deny: bits(*deny & 3),
            })
            .collect(),
        nsfw: Nsfw::Inherit,
        named: true,
    }
}

#[derive(Clone, Copy, Debug)]
enum Bump {
    Member(u8, Option<u8>),
    Presence(u8),
}

pub struct Harness {
    pub perms: Arc<PermsShared>,
    pub core: Core<u64>,
    pub reference: Reference<u64>,
    roles: Vec<RoleSpec>,
    channels: BTreeMap<u8, ChannelSpec>,
    members: BTreeMap<u8, (u8, Vec<u8>)>,
    statuses: BTreeMap<u8, Status>,
    connected: BTreeSet<u8>,
    sessions: BTreeMap<u8, u8>,
    held: Vec<Event<u64>>,
    hold: bool,
    member_ver: BTreeMap<u8, (u32, Option<u8>)>,
    presence_ver: BTreeMap<u8, u32>,
    fresh_bumps: Vec<Bump>,
    held_bumps: Vec<Bump>,
    by_id: BTreeMap<u64, u8>,
    pub fills: u64,
    pub encoded_fills: u64,
    pub partials: u64,
    pub dirty_retries: u64,
    pub widest_sync: usize,
    pub widest_ops: usize,
    pub most_quarantined: usize,
}

pub const MALFORMED: u8 = u8::MAX;

fn role_list(roles: &[u8]) -> RoleList {
    if roles.contains(&MALFORMED) {
        return RoleList::Malformed;
    }
    RoleList::Roles(roles.iter().map(|r| role_id(*r)).collect())
}

fn perms_apply(perms: &PermsShared, events: &[PermEvent]) -> Result<(), String> {
    if events.is_empty() {
        return Ok(());
    }
    match perms.apply(events) {
        Ok(_) => Ok(()),
        Err(PermsError::NeedsDirty) => perms
            .apply_dirty(events)
            .map(|_| ())
            .map_err(|e| format!("{e:?}")),
        Err(e) => Err(format!("{e:?}")),
    }
}

pub fn session_key(s: u8) -> SessionKey {
    SessionKey::new(format!("session-{s}").as_bytes())
}

fn normalize(items: &[(u32, u32)]) -> Vec<(u32, u32)> {
    let mut valid: Vec<(u32, u32)> = items
        .iter()
        .copied()
        .filter(|(s, e)| s <= e && *e <= 100_000 && e - s <= 99)
        .collect();
    valid.sort();
    let mut merged: Vec<(u32, u32)> = Vec::new();
    for (s, e) in valid {
        match merged.last_mut() {
            Some(last) if s <= last.1 + 1 => last.1 = last.1.max(e),
            _ => merged.push((s, e)),
        }
    }
    let mut out = Vec::new();
    for (s, e) in merged {
        let mut start = s;
        while e - start > 99 {
            out.push((start, start + 99));
            start += 100;
        }
        out.push((start, e));
    }
    out
}

fn subtract(ranges: Vec<(u32, u32)>, sub: (u32, u32)) -> Vec<(u32, u32)> {
    ranges
        .into_iter()
        .flat_map(|(rs, re)| {
            if re < sub.0 || rs > sub.1 {
                vec![(rs, re)]
            } else {
                let mut v = Vec::new();
                if rs < sub.0 {
                    v.push((rs, sub.0 - 1));
                }
                if re > sub.1 {
                    v.push((sub.1 + 1, re));
                }
                v
            }
        })
        .collect()
}

fn to_sync(new: &[(u32, u32)], old: &[(u32, u32)]) -> Vec<(u32, u32)> {
    let should = !new.is_empty() && new != old;
    if !should {
        return new.to_vec();
    }
    let subset = new
        .iter()
        .all(|(s, e)| old.iter().any(|(os, oe)| os <= s && oe >= e));
    if subset && !old.is_empty() {
        return Vec::new();
    }
    let delta = normalize(&old.iter().fold(new.to_vec(), |acc, r| subtract(acc, *r)));
    if delta.is_empty() {
        new.to_vec()
    } else {
        delta
    }
}

impl Harness {
    pub fn new(setup: &Setup) -> Result<Harness, String> {
        let threshold = [1000u32, 2, 4, 8][(setup.threshold % 4) as usize];
        let budget = [250_000u64, 40_000, 4_000, 64][(setup.budget % 4) as usize];
        let fragment_budget = [2u64 << 20, 1_500, 0][(setup.fragments % 3) as usize];
        let payload_budget = [2u64 << 20, 6_000, 0][(setup.fragments / 3 % 3) as usize];
        let config = Config {
            offline_threshold: threshold,
            offline_drop: threshold + threshold / 2,
            work_budget_units: budget,
            fragment_budget_bytes: fragment_budget,
            payload_budget_bytes: payload_budget,
            ..Config::default()
        };
        let perms = PermsShared::new(GuildId::new(GUILD), PermConfig::default());
        let mut roles = vec![RoleSpec {
            id: RoleId::new(GUILD),
            permissions: Permissions::VIEW_CHANNEL | Permissions::VIEW_CHANNEL_MEMBERS,
            position: RankPos::Int(0),
            hoist: Hoist::NotHoisted,
        }];
        for (r, perms_bits, hoist) in &setup.roles {
            if roles.iter().any(|s| s.id == role_id(*r % 8)) {
                continue;
            }
            roles.push(role_spec(*r % 8, *perms_bits, *r % 4, *hoist));
        }
        let mut channels = BTreeMap::new();
        for (c, parent, overwrites) in &setup.channels {
            channels.insert(*c % 8, channel_spec(*c % 8, *parent, overwrites));
        }
        let mut members = BTreeMap::new();
        let mut statuses = BTreeMap::new();
        let mut connected = BTreeSet::new();
        for (u, n, r, s, c) in &setup.members {
            members.insert(*u, (*n, r.clone()));
            statuses.insert(*u, status(*s));
            if *c {
                connected.insert(*u);
            }
        }
        let mut snapshot = PermSnapshot::new(GuildId::new(GUILD), None);
        snapshot.roles = roles.clone();
        snapshot.channels = channels.values().cloned().collect();
        snapshot.members = members
            .iter()
            .map(|(u, (_, r))| (user_id(*u), role_list(r)))
            .collect();
        match perms.load(&snapshot) {
            Ok(_) => {}
            Err(PermsError::NeedsDirty) => {
                perms
                    .load_dirty(&snapshot)
                    .map_err(|e| format!("perms load {e:?}"))?;
            }
            Err(e) => return Err(format!("perms load {e:?}")),
        }
        let ml = Snapshot {
            members: members
                .iter()
                .map(|(u, (n, _))| (user_id(*u), name(*n).into_boxed_slice()))
                .collect(),
            statuses: statuses.iter().map(|(u, s)| (user_id(*u), *s)).collect(),
            connected: connected.iter().map(|u| user_id(*u)).collect(),
            updates_off: false,
        };
        let mut core = Core::new(GuildId::new(GUILD), config, Arc::clone(&perms))
            .map_err(|e| format!("{e:?}"))?;
        match core.load(&ml, Mode::Normal) {
            Ok(()) => {}
            Err(CoreError::NeedsDirty) => {
                core.load(&ml, Mode::Dirty).map_err(|e| format!("{e:?}"))?
            }
            Err(e) => return Err(format!("{e:?}")),
        }
        let mut reference = Reference::new(Arc::clone(&perms), threshold);
        reference.load(&ml);
        let member_ver = members
            .iter()
            .map(|(u, (n, _))| (*u, (0, Some(*n))))
            .collect();
        Ok(Harness {
            perms,
            core,
            reference,
            roles,
            channels,
            members,
            statuses,
            connected,
            sessions: BTreeMap::new(),
            held: Vec::new(),
            hold: false,
            member_ver,
            presence_ver: BTreeMap::new(),
            fresh_bumps: Vec::new(),
            held_bumps: Vec::new(),
            by_id: (0..=u8::MAX).rev().map(|u| (user_id(u).get(), u)).collect(),
            fills: 0,
            encoded_fills: 0,
            partials: 0,
            dirty_retries: 0,
            widest_sync: 0,
            widest_ops: 0,
            most_quarantined: 0,
        })
    }

    fn translate(&mut self, op: &Op) -> Result<(Vec<PermEvent>, Vec<Event<u64>>), String> {
        let mut pe = Vec::new();
        let mut ml = Vec::new();
        match op {
            Op::Upsert {
                user,
                name: n,
                roles,
                unchanged,
            } => {
                let same = self
                    .members
                    .get(user)
                    .is_some_and(|(on, or)| on == n && or == roles);
                if let (true, Some((stored, _))) = (*unchanged, self.members.get(user)) {
                    ml.push(Event::MemberUpsert {
                        user: user_id(*user),
                        display_name: name(*stored).into_boxed_slice(),
                        change: Change::Unchanged,
                    });
                } else if !same {
                    let roles_changed = self.members.get(user).map(|(_, r)| r) != Some(roles);
                    if roles_changed {
                        pe.push(PermEvent::MemberUpsert(user_id(*user), role_list(roles)));
                    }
                    self.members.insert(*user, (*n, roles.clone()));
                    self.fresh_bumps.push(Bump::Member(*user, Some(*n)));
                    ml.push(Event::MemberUpsert {
                        user: user_id(*user),
                        display_name: name(*n).into_boxed_slice(),
                        change: Change::Changed,
                    });
                }
            }
            Op::Remove { user } => {
                self.members.remove(user);
                self.fresh_bumps.push(Bump::Member(*user, None));
                pe.push(PermEvent::MemberRemove(user_id(*user)));
                ml.push(Event::MemberRemove(user_id(*user)));
            }
            Op::Presence {
                user,
                status: s,
                unchanged,
                deferred,
            } => {
                let member = self.members.contains_key(user);
                let current = self.statuses.get(user).copied().unwrap_or(Status::Offline);
                let (next, change) = if *unchanged && member {
                    (current, Change::Unchanged)
                } else {
                    (status(*s), Change::Changed)
                };
                self.statuses.insert(*user, next);
                self.fresh_bumps.push(Bump::Presence(*user));
                ml.push(Event::Presence {
                    user: user_id(*user),
                    status: next,
                    change,
                    timing: if *deferred {
                        Timing::Deferred
                    } else {
                        Timing::Immediate
                    },
                });
            }
            Op::Cleared { user } => {
                self.statuses.insert(*user, Status::Offline);
                self.fresh_bumps.push(Bump::Presence(*user));
                ml.push(Event::PresenceCleared(user_id(*user)));
            }
            Op::Connect { user, changed } => {
                if self.connected.insert(*user) {
                    ml.push(Event::Connection(
                        user_id(*user),
                        Connection::Connected {
                            presence: if *changed {
                                Change::Changed
                            } else {
                                Change::Unchanged
                            },
                        },
                    ));
                }
            }
            Op::Disconnect { user } => {
                if self.connected.remove(user) {
                    ml.push(Event::Connection(user_id(*user), Connection::Disconnected));
                }
            }
            Op::Role {
                role,
                perms,
                position,
                hoist,
                delete,
            } => {
                let id = role_id(*role % 8);
                let at = self.roles.iter().position(|r| r.id == id);
                match (at, delete) {
                    (Some(i), true) => {
                        self.roles.remove(i);
                    }
                    (Some(i), false) => {
                        self.roles[i] = role_spec(*role % 8, *perms, *position, *hoist)
                    }
                    (None, _) => self
                        .roles
                        .push(role_spec(*role % 8, *perms, *position, *hoist)),
                }
                pe.push(PermEvent::Roles(self.roles.clone()));
                ml.push(Event::Permissions(SyncScope::AllLists));
            }
            Op::Channel {
                channel,
                parent,
                overwrites,
                delete,
            } => {
                let c = *channel % 8;
                let exists = self.channels.contains_key(&c);
                if *delete && exists {
                    self.channels.remove(&c);
                    pe.push(PermEvent::ChannelDelete(channel_id(c)));
                    ml.push(Event::Permissions(SyncScope::AllLists));
                } else if !*delete {
                    let spec = channel_spec(c, *parent, overwrites);
                    self.channels.insert(c, spec.clone());
                    if exists {
                        pe.push(PermEvent::ChannelUpsert(spec, ChannelCause::Updated));
                        ml.push(Event::Permissions(SyncScope::Channel(channel_id(c))));
                    } else {
                        pe.push(PermEvent::ChannelUpsert(spec, ChannelCause::Created));
                        ml.push(Event::Permissions(SyncScope::AllLists));
                    }
                }
            }
            Op::Owner { user } => {
                pe.push(PermEvent::Owner(Some(user_id(*user))));
                ml.push(Event::Permissions(SyncScope::NoLists));
            }
            Op::Virtual { user, channels } => {
                pe.push(PermEvent::VirtualAccess(
                    user_id(*user),
                    channels.iter().map(|c| channel_id(*c)).collect(),
                ));
                ml.push(Event::Permissions(SyncScope::NoLists));
            }
            Op::Subscribe {
                session,
                user,
                channel,
                ranges,
            } => {
                let owner = *self.sessions.entry(*session).or_insert(*user);
                let raw: Vec<(u32, u32)> = ranges
                    .iter()
                    .map(|(s, span)| (u32::from(*s), u32::from(*s) + u32::from(*span)))
                    .filter(|(s, e)| e - s <= 99)
                    .take(10)
                    .collect();
                let new = normalize(&raw);
                let key = session_key(*session);
                let old = match self.core.session_ranges(&key) {
                    Some((c, r)) if c == channel_id(*channel) => r.to_pairs(),
                    _ => Vec::new(),
                };
                let sync = to_sync(&new, &old);
                self.widest_sync = self.widest_sync.max(sync.len());
                ml.push(Event::Subscribe {
                    session: key,
                    pid: u64::from(*session),
                    user: user_id(owner),
                    channel: channel_id(*channel),
                    ranges: Ranges::normalized(&new)
                        .map_err(|e| format!("ranges {new:?} refused: {e:?}"))?,
                    sync: Ranges::sync(&sync)
                        .map_err(|e| format!("sync {sync:?} refused: {e:?}"))?,
                });
            }
            Op::Unsubscribe { session, channel } => ml.push(Event::Unsubscribe {
                session: session_key(*session),
                channel: channel_id(*channel),
            }),
            Op::Drop { session } => {
                self.sessions.remove(session);
                ml.push(Event::DropSession(session_key(*session)));
            }
            Op::ViewGained { session, channel } => ml.push(Event::ViewGained {
                session: session_key(*session),
                channel: channel_id(*channel),
            }),
            Op::Flush => ml.push(Event::Flush),
            Op::Updates { on } => ml.push(Event::Updates(if *on {
                Updates::UpdatesOn
            } else {
                Updates::UpdatesOff
            })),
            Op::Join | Op::Widen { .. } => {}
        }
        Ok((pe, ml))
    }

    pub fn step(&mut self, op: &Op) -> Result<(), String> {
        if let Op::Widen {
            session,
            user,
            channel,
            first,
            gap,
        } = op
        {
            let rows = (0..10u16).map(|i| (first + i * gap, 0)).collect();
            for ranges in [rows, vec![(0, 99)]] {
                self.step(&Op::Subscribe {
                    session: *session,
                    user: *user,
                    channel: *channel,
                    ranges,
                })?;
            }
            return Ok(());
        }
        if matches!(op, Op::Join) {
            self.hold = true;
            return Ok(());
        }
        let (pe, ml) = self.translate(op)?;
        let bumps = core::mem::take(&mut self.fresh_bumps);
        if !pe.is_empty() && !self.held.is_empty() {
            let held = core::mem::take(&mut self.held);
            let held_bumps = core::mem::take(&mut self.held_bumps);
            self.bump(held_bumps);
            self.run(held)?;
        }
        perms_apply(&self.perms, &pe)?;
        let quarantined = self
            .perms
            .view()
            .map_err(|e| format!("{e:?}"))?
            .members()
            .quarantined_len();
        self.most_quarantined = self.most_quarantined.max(quarantined);
        if self.hold && pe.is_empty() {
            self.hold = false;
            self.held.extend(ml);
            self.held_bumps.extend(bumps);
            return Ok(());
        }
        self.hold = false;
        let mut events = core::mem::take(&mut self.held);
        events.extend(ml);
        let mut all = core::mem::take(&mut self.held_bumps);
        all.extend(bumps);
        self.bump(all);
        self.run(events)
    }

    fn bump(&mut self, bumps: Vec<Bump>) {
        for b in bumps {
            match b {
                Bump::Member(u, n) => {
                    let entry = self.member_ver.entry(u).or_insert((0, n));
                    *entry = (entry.0 + 1, n);
                }
                Bump::Presence(u) => *self.presence_ver.entry(u).or_default() += 1,
            }
        }
    }

    fn run(&mut self, events: Vec<Event<u64>>) -> Result<(), String> {
        if events.is_empty() {
            return Ok(());
        }
        let before = self.fingerprint();
        let got = match self.core.apply(&events, None, Mode::Normal) {
            Err(CoreError::NeedsDirty) => {
                self.dirty_retries += 1;
                if self.fingerprint() != before {
                    return Err(format!("a refused call changed state for {events:?}"));
                }
                self.core.apply(&events, None, Mode::Dirty)
            }
            other => other,
        }
        .map_err(|e| format!("core apply {e:?}"))?;
        let want = self.reference.apply(&events);
        if got.spent > got.estimate {
            return Err(format!(
                "metered work {} above estimate {} for {events:?}",
                got.spent.get(),
                got.estimate.get()
            ));
        }
        for dispatch in &got.dispatches {
            self.widest_ops = self.widest_ops.max(dispatch.body.ops.len());
        }
        if got.dispatches != want.dispatches {
            return Err(format!(
                "dispatches differ for {events:?}\ncore {:#?}\nreference {:#?}",
                got.dispatches, want.dispatches
            ));
        }
        if got.timer != want.timer {
            return Err(format!(
                "timer {:?} but reference {:?} for {events:?}",
                got.timer, want.timer
            ));
        }
        if got.skipped_absent != want.skipped_absent {
            return Err(format!(
                "skipped {} but reference {} for {events:?}",
                got.skipped_absent, want.skipped_absent
            ));
        }
        let (timer, skipped) = (got.timer, got.skipped_absent);
        let step = self.core.render(got).map_err(|e| format!("render {e:?}"))?;
        let (sent, done) = self.drive(step)?;
        let Done::Sync(rendered) = done else {
            return Err("a sync render finished as a page".to_string());
        };
        if rendered.timer != timer || rendered.skipped_absent != skipped {
            return Err("rendered effects changed the timer or the skipped count".to_string());
        }
        let sent: Vec<Sent<u64>> = sent.into_iter().chain(rendered.sent).collect();
        if sent.len() != want.dispatches.len() {
            return Err(format!(
                "rendered {} payloads for {} dispatches",
                sent.len(),
                want.dispatches.len()
            ));
        }
        for (got, want) in sent.iter().zip(&want.dispatches) {
            if got.target != want.target {
                return Err(format!(
                    "rendered target {:?} vs {:?}",
                    got.target, want.target
                ));
            }
            let expected =
                ml_core::render::write_sync(GuildId::new(GUILD), &want.body, &mut |out, u| {
                    match self.list_bytes(u.get()) {
                        Some(bytes) => {
                            out.extend_from_slice(&bytes);
                            true
                        }
                        None => false,
                    }
                });
            if got.payload.as_bytes() != expected.as_slice() {
                return Err(format!(
                    "payload bytes differ for {events:?}\ncore      {}\nreference {}",
                    String::from_utf8_lossy(got.payload.as_bytes()),
                    String::from_utf8_lossy(&expected)
                ));
            }
        }
        Ok(())
    }

    fn harness_user(&self, user: u64) -> Option<u8> {
        self.by_id.get(&user).copied()
    }

    fn shown(&self, u: u8) -> bool {
        self.statuses
            .get(&u)
            .copied()
            .unwrap_or(Status::Offline)
            .shown_online(self.connected.contains(&u))
    }

    fn member_bytes(&self, user: u64) -> Option<Vec<u8>> {
        let u = self.harness_user(user)?;
        let (version, n) = self.member_ver.get(&u).copied()?;
        n?;
        Some(format!("{{\"user\":{{\"id\":\"{user}\"}},\"v\":{version}}}").into_bytes())
    }

    fn presence_bytes(&self, user: u64) -> Vec<u8> {
        match self.harness_user(user) {
            Some(u) if self.shown(u) => format!(
                "{{\"status\":{},\"v\":{}}}",
                self.statuses.get(&u).map_or(9, |s| *s as u8),
                self.presence_ver.get(&u).copied().unwrap_or(0)
            )
            .into_bytes(),
            _ => b"{\"afk\":false,\"mobile\":false,\"status\":\"offline\"}".to_vec(),
        }
    }

    fn list_bytes(&self, user: u64) -> Option<Vec<u8>> {
        let member = self.member_bytes(user)?;
        let mut out = b"{\"member\":".to_vec();
        out.extend_from_slice(&member);
        out.extend_from_slice(b",\"presence\":");
        out.extend_from_slice(&self.presence_bytes(user));
        out.push(b'}');
        Some(out)
    }

    fn drive(&mut self, mut step: Step<u64>) -> Result<(Vec<Sent<u64>>, Done<u64>), String> {
        let mut sent = Vec::new();
        for _ in 0..10_000 {
            step = match step {
                Step::Done(done) => return Ok((sent, done)),
                Step::Partial(more) => {
                    self.partials += 1;
                    sent.extend(more);
                    self.core.resume()
                }
                Step::NeedItems(users) => {
                    self.fills += 1;
                    let mut items = Vec::new();
                    for u in users {
                        let (Some(list), Some(page)) =
                            (self.list_bytes(u.get()), self.member_bytes(u.get()))
                        else {
                            continue;
                        };
                        self.encoded_fills += 1;
                        items.push(EncodedItem {
                            user: u,
                            list: list.into_boxed_slice(),
                            page: page.into_boxed_slice(),
                        });
                    }
                    self.core.fill(items)
                }
            }
            .map_err(|e| format!("render step {e:?}"))?;
        }
        Err("render did not finish in 10,000 steps".to_string())
    }

    fn page_compare(&mut self, at: &str, query: PageQuery, ids: &[UserId]) -> Result<(), String> {
        let step = self
            .core
            .page(query)
            .map_err(|e| format!("{at}: page {e:?}"))?;
        let (sent, done) = self.drive(step)?;
        let Done::Page(page) = done else {
            return Err(format!("{at}: a page render finished as a sync"));
        };
        if !sent.is_empty() {
            return Err(format!("{at}: a page render sent payloads"));
        }
        let total = self.reference.member_ids().len() as u32;
        let want = ml_core::render::write_page(ids, total, &mut |out, u| match self
            .member_bytes(u.get())
        {
            Some(bytes) => {
                out.extend_from_slice(&bytes);
                true
            }
            None => false,
        });
        if page.total != total || page.payload.as_bytes() != want.as_slice() {
            return Err(format!(
                "{at}: page {query:?}\ncore      {}\nreference {}",
                String::from_utf8_lossy(page.payload.as_bytes()),
                String::from_utf8_lossy(&want)
            ));
        }
        Ok(())
    }

    fn fingerprint(&self) -> String {
        let sessions: Vec<_> = (0..7u8)
            .map(|s| self.core.session_ranges(&session_key(s)))
            .collect();
        format!(
            "{:?} {:?} {:?} {:?} {} {:?}",
            self.core.page_ids(0, 1000),
            self.core.guild_counts(),
            self.core.pending_marks(),
            sessions,
            self.core.timer_armed(),
            self.core.stats()
        )
    }

    pub fn compare(&mut self, at: &str) -> Result<(), String> {
        self.core
            .check()
            .map_err(|e| format!("{at}: invariant: {e}"))?;
        for s in 0..7u8 {
            let key = session_key(s);
            if self.core.session_ranges(&key) != self.reference.session_ranges(&key) {
                return Err(format!(
                    "{at}: session {s} ranges {:?} vs {:?}",
                    self.core.session_ranges(&key),
                    self.reference.session_ranges(&key)
                ));
            }
            let got = self.core.render_session(&key);
            let want = self.reference.render_session(&key);
            if got != want {
                return Err(format!(
                    "{at}: session {s} view\ncore {got:#?}\nreference {want:#?}"
                ));
            }
        }
        let channels: Vec<ChannelId> = CHANNELS.iter().map(|c| ChannelId::new(*c)).collect();
        let counts = self
            .core
            .channel_counts(&channels, Mode::Dirty)
            .map_err(|e| format!("{e:?}"))?;
        for (c, total, online) in counts {
            if (total, online) != self.reference.counts(c) {
                return Err(format!(
                    "{at}: channel {} counts {:?} vs {:?}",
                    c.get(),
                    (total, online),
                    self.reference.counts(c)
                ));
            }
            for u in 0..24u8 {
                let row = self.core.is_row_of(c, user_id(u));
                let view = self.perms.view().map_err(|e| format!("{e:?}"))?;
                let want = self.reference.member_ids().contains(&user_id(u))
                    && view.model().class_of_id(c).is_some_and(|class| {
                        view.roleset(user_id(u)).is_some_and(|set| {
                            view.model()
                                .visible(class, set, user_id(u))
                                .contains(perm_core::Visible::VIEW)
                        })
                    });
                if row != want {
                    return Err(format!("{at}: is_row {} {} {row}", c.get(), u));
                }
            }
        }
        if self.core.guild_counts() != self.reference.guild_counts() {
            return Err(format!("{at}: guild counts"));
        }
        if self.core.pending_marks() != self.reference.pending_marks() {
            return Err(format!(
                "{at}: pending {:?} vs {:?}",
                self.core.pending_marks(),
                self.reference.pending_marks()
            ));
        }
        let ids = self.reference.member_ids();
        if self.core.page_ids(0, 1000) != ids {
            return Err(format!("{at}: page ids"));
        }
        self.page_compare(
            at,
            PageQuery::Offset {
                offset: 0,
                limit: 1000,
            },
            &ids,
        )?;
        let tail: Vec<UserId> = ids.iter().skip(2).take(3).copied().collect();
        self.page_compare(
            at,
            PageQuery::Offset {
                offset: 2,
                limit: 3,
            },
            &tail,
        )?;
        let after: Vec<UserId> = ids
            .iter()
            .filter(|u| u.get() > 520)
            .take(4)
            .copied()
            .collect();
        self.page_compare(
            at,
            PageQuery::After {
                after: Some(UserId::new(520)),
                limit: 4,
            },
            &after,
        )?;
        for offset in [0u32, 1, 3, 7] {
            for limit in [0u32, 1, 2, 5, 100] {
                let want: Vec<UserId> = ids
                    .iter()
                    .skip(offset as usize)
                    .take(limit as usize)
                    .copied()
                    .collect();
                if self.core.page_ids(offset, limit) != want {
                    return Err(format!("{at}: page {offset} {limit}"));
                }
            }
        }
        for after in [
            None,
            Some(UserId::new(0)),
            Some(UserId::new(520)),
            Some(UserId::new(9999)),
        ]
        .into_iter()
        .chain(ids.iter().map(|u| Some(*u)))
        {
            let want: Vec<UserId> = ids
                .iter()
                .filter(|u| after.is_none_or(|a| u.get() > a.get()))
                .take(3)
                .copied()
                .collect();
            if self.core.page_after(after, 3) != want {
                return Err(format!("{at}: page after {after:?}"));
            }
        }
        Ok(())
    }
}
