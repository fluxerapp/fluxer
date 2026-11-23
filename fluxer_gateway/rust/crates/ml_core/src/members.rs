// SPDX-License-Identifier: AGPL-3.0-or-later

use core::cell::Cell;
use core::cmp::Ordering;
use core::hash::BuildHasher;

use foldhash::fast::FixedState;
use hashbrown::{HashMap, HashTable};

use crate::budget::Meter;
use crate::casefold;
use crate::flags::{MemberFlags, Status};
use crate::ids::{RoleSetId, Slot, UserId};
use crate::names::{ArenaFull, MAX_KEY, NameRef, Names};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Presence {
    pub status: Status,
    pub connected: bool,
}

impl Presence {
    pub fn is_default(self) -> bool {
        self == Presence::default()
    }
}

#[derive(Debug)]
pub struct Members {
    user: Vec<UserId>,
    name_off: Vec<u32>,
    name_len: Vec<u16>,
    roleset: Vec<RoleSetId>,
    flags: Vec<MemberFlags>,
    status: Vec<Status>,
    member_rev: Vec<u32>,
    presence_rev: Vec<u32>,
    names: Names,
    free: Vec<Slot>,
    index: HashTable<Slot>,
    hasher: FixedState,
    outsiders: HashMap<UserId, Presence, FixedState>,
    live: u32,
    online: u32,
    prefix_ties: Cell<u64>,
    scratch: Vec<u8>,
}

impl Members {
    pub fn new(seed: u64) -> Members {
        Members::with_capacity(seed, 0, 0)
    }

    pub fn with_capacity(seed: u64, slots: usize, name_bytes: usize) -> Members {
        Members {
            user: Vec::with_capacity(slots),
            name_off: Vec::with_capacity(slots),
            name_len: Vec::with_capacity(slots),
            roleset: Vec::with_capacity(slots),
            flags: Vec::with_capacity(slots),
            status: Vec::with_capacity(slots),
            member_rev: Vec::with_capacity(slots),
            presence_rev: Vec::with_capacity(slots),
            names: Names::with_capacity(name_bytes),
            free: Vec::new(),
            index: HashTable::with_capacity(slots),
            hasher: FixedState::with_seed(seed),
            outsiders: HashMap::with_hasher(FixedState::with_seed(seed.rotate_left(17))),
            live: 0,
            online: 0,
            prefix_ties: Cell::new(0),
            scratch: Vec::new(),
        }
    }

    pub fn len(&self) -> u32 {
        self.live
    }

    pub fn is_empty(&self) -> bool {
        self.live == 0
    }

    pub fn online(&self) -> u32 {
        self.online
    }

    pub fn capacity_slots(&self) -> usize {
        self.user.len()
    }

    pub fn prefix_ties(&self) -> u64 {
        self.prefix_ties.get()
    }

    pub fn find(&self, user: UserId) -> Option<Slot> {
        let hash = self.hasher.hash_one(user.get());
        let users = &self.user;
        self.index
            .find(hash, |slot| users.get(slot.index()) == Some(&user))
            .copied()
    }

    pub fn contains(&self, user: UserId) -> bool {
        self.find(user).is_some()
    }

    pub fn user(&self, slot: Slot) -> UserId {
        self.user
            .get(slot.index())
            .copied()
            .unwrap_or(UserId::new(0))
    }

    pub fn roleset(&self, slot: Slot) -> RoleSetId {
        self.roleset
            .get(slot.index())
            .copied()
            .unwrap_or(RoleSetId::EVERYONE)
    }

    pub fn flags(&self, slot: Slot) -> MemberFlags {
        self.flags.get(slot.index()).copied().unwrap_or_default()
    }

    pub fn is_live(&self, slot: Slot) -> bool {
        self.flags(slot).contains(MemberFlags::LIVE)
    }

    pub fn is_online(&self, slot: Slot) -> bool {
        self.flags(slot).contains(MemberFlags::ONLINE)
    }

    pub fn is_connected(&self, slot: Slot) -> bool {
        self.flags(slot).contains(MemberFlags::CONNECTED)
    }

    pub fn status(&self, slot: Slot) -> Status {
        self.status.get(slot.index()).copied().unwrap_or_default()
    }

    pub fn member_rev(&self, slot: Slot) -> u32 {
        self.member_rev.get(slot.index()).copied().unwrap_or(0)
    }

    pub fn presence_rev(&self, slot: Slot) -> u32 {
        self.presence_rev.get(slot.index()).copied().unwrap_or(0)
    }

    pub fn prefix(&self, slot: Slot) -> u64 {
        casefold::prefix(self.key(slot))
    }

    fn name(&self, slot: Slot) -> NameRef {
        let i = slot.index();
        match (self.name_off.get(i), self.name_len.get(i)) {
            (Some(off), Some(len)) => NameRef::new(*off, *len),
            _ => NameRef::EMPTY,
        }
    }

    pub fn key(&self, slot: Slot) -> &[u8] {
        self.names.get(self.name(slot))
    }

    pub fn slots(&self) -> impl Iterator<Item = Slot> + '_ {
        self.flags
            .iter()
            .enumerate()
            .filter(|(_, f)| f.contains(MemberFlags::LIVE))
            .filter_map(|(i, _)| Slot::from_index(i))
    }

    pub fn prefixed(&self, slot: Slot) -> (u64, Slot) {
        (self.prefix(slot), slot)
    }

    pub fn sort_prefixed(&self, entries: &mut [(u64, Slot)]) {
        entries.sort_by(|(pa, a), (pb, b)| self.cmp_prefixed(*pa, *a, *pb, *b));
    }

    pub fn cmp(&self, a: Slot, b: Slot) -> Ordering {
        self.cmp_prefixed(self.prefix(a), a, self.prefix(b), b)
    }

    pub fn cmp_prefixed(&self, pa: u64, a: Slot, pb: u64, b: Slot) -> Ordering {
        pa.cmp(&pb).then_with(|| {
            if a == b {
                return Ordering::Equal;
            }
            self.prefix_ties.set(self.prefix_ties.get().wrapping_add(1));
            self.key(a)
                .cmp(self.key(b))
                .then_with(|| (self.user(a).get() as i64).cmp(&(self.user(b).get() as i64)))
        })
    }

    pub fn outsider(&self, user: UserId) -> Presence {
        self.outsiders.get(&user).copied().unwrap_or_default()
    }

    pub fn set_outsider(&mut self, user: UserId, presence: Presence) {
        if presence.is_default() {
            self.outsiders.remove(&user);
        } else {
            self.outsiders.insert(user, presence);
        }
    }

    fn fold(&mut self, display_name: &[u8]) -> Result<NameRef, ArenaFull> {
        let mut scratch = core::mem::take(&mut self.scratch);
        casefold::fold_into(display_name, &mut scratch);
        let pushed = self.names.push(&scratch);
        self.scratch = scratch;
        pushed
    }

    pub fn insert(
        &mut self,
        user: UserId,
        display_name: &[u8],
        roleset: RoleSetId,
    ) -> Result<Slot, ArenaFull> {
        let name = self.fold(display_name)?;
        let presence = self.outsiders.remove(&user).unwrap_or_default();
        let mut flags = MemberFlags::LIVE;
        if presence.connected {
            flags |= MemberFlags::CONNECTED;
        }
        if presence.status.shown_online(presence.connected) {
            flags |= MemberFlags::ONLINE;
            self.online = self.online.saturating_add(1);
        }
        let slot = match self.free.pop() {
            Some(slot) => {
                let i = slot.index();
                set(&mut self.user, i, user);
                set(&mut self.name_off, i, name.off());
                set(&mut self.name_len, i, name.len());
                set(&mut self.roleset, i, roleset);
                set(&mut self.flags, i, flags);
                set(&mut self.status, i, presence.status);
                bump(&mut self.member_rev, i);
                bump(&mut self.presence_rev, i);
                slot
            }
            None => {
                let slot = Slot::from_index(self.user.len()).ok_or(ArenaFull)?;
                self.user.push(user);
                self.name_off.push(name.off());
                self.name_len.push(name.len());
                self.roleset.push(roleset);
                self.flags.push(flags);
                self.status.push(presence.status);
                self.member_rev.push(0);
                self.presence_rev.push(0);
                slot
            }
        };
        let hasher = self.hasher.clone();
        let users = &self.user;
        self.index
            .insert_unique(hasher.hash_one(user.get()), slot, |s| {
                hasher.hash_one(users.get(s.index()).map_or(0, |u| u.get()))
            });
        self.live = self.live.saturating_add(1);
        Ok(slot)
    }

    pub fn remove(&mut self, slot: Slot) {
        let user = self.user(slot);
        let flags = self.flags(slot);
        if !flags.contains(MemberFlags::LIVE) {
            return;
        }
        let hash = self.hasher.hash_one(user.get());
        if let Ok(entry) = self.index.find_entry(hash, |s| *s == slot) {
            entry.remove();
        }
        let presence = Presence {
            status: self.status(slot),
            connected: flags.contains(MemberFlags::CONNECTED),
        };
        self.set_outsider(user, presence);
        if flags.contains(MemberFlags::ONLINE) {
            self.online = self.online.saturating_sub(1);
        }
        self.names.release(self.name(slot));
        set(&mut self.name_off, slot.index(), 0);
        set(&mut self.name_len, slot.index(), 0);
        set(&mut self.flags, slot.index(), MemberFlags::empty());
        self.free.push(slot);
        self.live = self.live.saturating_sub(1);
    }

    pub fn rename(&mut self, slot: Slot, display_name: &[u8]) -> Result<bool, ArenaFull> {
        let mut scratch = core::mem::take(&mut self.scratch);
        casefold::fold_into(display_name, &mut scratch);
        let clipped = scratch
            .get(..MAX_KEY.min(scratch.len()))
            .unwrap_or_default();
        let same = self.key(slot) == clipped;
        let result = if same {
            Ok(false)
        } else {
            match self.names.push(&scratch) {
                Ok(name) => {
                    self.names.release(self.name(slot));
                    set(&mut self.name_off, slot.index(), name.off());
                    set(&mut self.name_len, slot.index(), name.len());
                    Ok(true)
                }
                Err(e) => Err(e),
            }
        };
        self.scratch = scratch;
        result
    }

    pub fn set_roleset(&mut self, slot: Slot, roleset: RoleSetId) {
        set(&mut self.roleset, slot.index(), roleset);
    }

    pub fn bump_member(&mut self, slot: Slot) {
        bump(&mut self.member_rev, slot.index());
    }

    pub fn bump_presence(&mut self, slot: Slot) {
        bump(&mut self.presence_rev, slot.index());
    }

    pub fn set_presence(&mut self, slot: Slot, presence: Presence) -> bool {
        let before = self.is_online(slot);
        let online = presence.status.shown_online(presence.connected);
        let mut flags = self.flags(slot);
        flags.set(MemberFlags::CONNECTED, presence.connected);
        flags.set(MemberFlags::ONLINE, online);
        set(&mut self.flags, slot.index(), flags);
        set(&mut self.status, slot.index(), presence.status);
        match (before, online) {
            (false, true) => self.online = self.online.saturating_add(1),
            (true, false) => self.online = self.online.saturating_sub(1),
            _ => {}
        }
        before != online
    }

    pub fn presence(&self, slot: Slot) -> Presence {
        Presence {
            status: self.status(slot),
            connected: self.is_connected(slot),
        }
    }

    pub fn compaction_cost(&self) -> Option<u64> {
        self.names.needs_compaction().then(|| {
            (self.name_off.len() as u64)
                .saturating_mul(2)
                .saturating_add(self.names.live_bytes() as u64 / 16)
                .saturating_add(64)
        })
    }

    pub fn compact(&mut self, meter: &mut Meter) {
        meter.charge(64);
        let flags = &self.flags;
        let lens = &self.name_len;
        let names = self
            .name_off
            .iter_mut()
            .zip(lens.iter())
            .zip(flags.iter())
            .filter(|(_, f)| {
                meter.charge(2);
                f.contains(MemberFlags::LIVE)
            })
            .map(|((off, len), _)| (off, *len));
        let copied = self.names.compact(names);
        meter.charge(copied as u64 / 16);
    }

    pub fn bytes(&self) -> usize {
        const PER_SLOT: usize = core::mem::size_of::<UserId>()
            + core::mem::size_of::<u32>()
            + core::mem::size_of::<u16>()
            + core::mem::size_of::<RoleSetId>()
            + core::mem::size_of::<MemberFlags>()
            + core::mem::size_of::<Status>()
            + 2 * core::mem::size_of::<u32>();
        let per_slot = PER_SLOT;
        self.user
            .capacity()
            .saturating_mul(per_slot)
            .saturating_add(self.names.bytes())
            .saturating_add(
                self.index
                    .capacity()
                    .saturating_mul(8)
                    .div_ceil(7)
                    .next_power_of_two()
                    .saturating_mul(5),
            )
            .saturating_add(self.free.capacity().saturating_mul(4))
            .saturating_add(self.outsiders.capacity().saturating_mul(24))
    }
}

fn set<T>(vec: &mut [T], i: usize, value: T) {
    if let Some(cell) = vec.get_mut(i) {
        *cell = value;
    }
}

fn bump(vec: &mut [u32], i: usize) {
    if let Some(cell) = vec.get_mut(i) {
        *cell = cell.wrapping_add(1);
    }
}
