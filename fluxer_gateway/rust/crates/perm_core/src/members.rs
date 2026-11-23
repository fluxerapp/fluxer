// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeSet;

use hashbrown::HashTable;

use crate::flags::MemberFlags;
use crate::hashing::Seeded;
use crate::ids::{RoleSetId, UserId};
use crate::rolesets::DEAD;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct MemberEntry {
    user: UserId,
    roleset: RoleSetId,
    flags: MemberFlags,
}

impl MemberEntry {
    pub const fn new(user: UserId, roleset: RoleSetId, flags: MemberFlags) -> MemberEntry {
        MemberEntry {
            user,
            roleset,
            flags,
        }
    }

    pub fn user(&self) -> UserId {
        self.user
    }

    pub fn roleset(&self) -> RoleSetId {
        self.roleset
    }

    pub fn flags(&self) -> MemberFlags {
        self.flags
    }

    pub fn is_quarantined(&self) -> bool {
        self.flags.contains(MemberFlags::QUARANTINED)
    }

    pub fn is_exception(&self) -> bool {
        self.flags.contains(MemberFlags::EXCEPTION)
    }
}

#[derive(Clone, Debug, Default)]
pub struct MemberIndex {
    entries: Vec<MemberEntry>,
    slots: HashTable<u32>,
    refcount: Vec<u32>,
    live_sets: u32,
    quarantined: BTreeSet<UserId>,
    by_set: Vec<Vec<u32>>,
    set_pos: Vec<u32>,
    generation: u32,
    version: u64,
    writes: u64,
    seeded: Seeded,
}

impl MemberIndex {
    pub fn new() -> MemberIndex {
        MemberIndex::default()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn get(&self, user: UserId) -> Option<&MemberEntry> {
        self.get_hashed(user, self.hash_of(user))
    }

    pub(crate) fn hash_of(&self, user: UserId) -> u64 {
        self.seeded.hash(&user)
    }

    pub(crate) fn get_hashed(&self, user: UserId, hash: u64) -> Option<&MemberEntry> {
        let entries = &self.entries;
        self.slots
            .find(hash, |p| {
                entries.get(slot(*p)).is_some_and(|e| e.user == user)
            })
            .and_then(|p| entries.get(slot(*p)))
    }

    pub fn contains(&self, user: UserId) -> bool {
        self.get(user).is_some()
    }

    pub fn roleset(&self, user: UserId) -> Option<RoleSetId> {
        self.get(user).map(MemberEntry::roleset)
    }

    pub fn refcount(&self, set: RoleSetId) -> u32 {
        self.refcount.get(set.index()).copied().unwrap_or(0)
    }

    pub fn refcounts(&self) -> &[u32] {
        &self.refcount
    }

    pub fn live_sets(&self) -> u32 {
        self.live_sets
    }

    pub fn generation(&self) -> u32 {
        self.generation
    }

    pub fn version(&self) -> u64 {
        self.version
    }

    pub fn writes(&self) -> u64 {
        self.writes
    }

    pub fn quarantined(&self) -> impl Iterator<Item = UserId> + '_ {
        self.quarantined.iter().copied()
    }

    pub fn quarantined_len(&self) -> usize {
        self.quarantined.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &MemberEntry> {
        self.entries.iter()
    }

    pub fn entries(&self) -> &[MemberEntry] {
        &self.entries
    }

    pub fn set_len(&self, set: RoleSetId) -> usize {
        self.set_positions(set).len()
    }

    pub fn in_set(&self, set: RoleSetId) -> impl Iterator<Item = UserId> + '_ {
        self.set_entries(set).map(MemberEntry::user)
    }

    pub fn set_entries(&self, set: RoleSetId) -> impl Iterator<Item = &MemberEntry> + '_ {
        self.set_positions(set)
            .iter()
            .filter_map(|p| self.entries.get(slot(*p)))
    }

    fn set_positions(&self, set: RoleSetId) -> &[u32] {
        self.by_set.get(set.index()).map_or(&[], Vec::as_slice)
    }

    pub fn bytes(&self) -> usize {
        let lists = self
            .by_set
            .iter()
            .map(|list| list.capacity().saturating_mul(size_of::<u32>()))
            .fold(0usize, usize::saturating_add);
        self.entries
            .capacity()
            .saturating_mul(size_of::<MemberEntry>())
            .saturating_add(
                self.slots
                    .capacity()
                    .saturating_mul(size_of::<u32>().saturating_add(1)),
            )
            .saturating_add(self.set_pos.capacity().saturating_mul(size_of::<u32>()))
            .saturating_add(self.refcount.capacity().saturating_mul(size_of::<u32>()))
            .saturating_add(self.by_set.capacity().saturating_mul(size_of::<Vec<u32>>()))
            .saturating_add(lists)
            .saturating_add(self.quarantined.len().saturating_mul(40))
    }

    fn place(&mut self, position: usize) {
        let Some(entry) = self.entries.get(position).copied() else {
            return;
        };
        let at = match u32::try_from(position) {
            Ok(member) if !entry.is_quarantined() => {
                let set = entry.roleset.index();
                if self.by_set.len() <= set {
                    self.by_set.resize_with(set.saturating_add(1), Vec::new);
                }
                self.by_set.get_mut(set).and_then(|list| {
                    let at = u32::try_from(list.len()).ok()?;
                    list.push(member);
                    Some(at)
                })
            }
            _ => None,
        };
        if self.set_pos.len() <= position {
            self.set_pos.resize(position.saturating_add(1), NOT_PLACED);
        }
        if let Some(slot) = self.set_pos.get_mut(position) {
            *slot = at.unwrap_or(NOT_PLACED);
        }
    }

    fn unplace(&mut self, position: usize) {
        let Some(entry) = self.entries.get(position).copied() else {
            return;
        };
        let Some(at) = self.set_pos.get_mut(position).map(|slot| {
            let at = *slot;
            *slot = NOT_PLACED;
            at
        }) else {
            return;
        };
        if at == NOT_PLACED {
            return;
        }
        let at = slot(at);
        let Some(list) = self.by_set.get_mut(entry.roleset.index()) else {
            return;
        };
        if at >= list.len() {
            return;
        }
        list.swap_remove(at);
        let Some(moved) = list.get(at).copied() else {
            return;
        };
        if let Some(pos) = self.set_pos.get_mut(slot(moved)) {
            *pos = u32::try_from(at).unwrap_or(NOT_PLACED);
        }
    }

    fn rebuild_sets(&mut self) {
        self.by_set = self
            .refcount
            .iter()
            .map(|count| Vec::with_capacity(slot(*count)))
            .collect();
        self.set_pos = Vec::with_capacity(self.entries.len());
        for position in 0..self.entries.len() {
            self.place(position);
        }
    }

    pub(crate) fn fit_sets(&mut self) {
        self.by_set.shrink_to_fit();
        for list in &mut self.by_set {
            list.shrink_to_fit();
        }
    }

    pub(crate) fn upsert(&mut self, entry: MemberEntry) -> Option<MemberEntry> {
        self.writes = self.writes.wrapping_add(1);
        self.retain(entry.roleset);
        if entry.is_quarantined() {
            self.quarantined.insert(entry.user);
        } else {
            self.quarantined.remove(&entry.user);
        }
        let hash = self.seeded.hash(&entry.user);
        let entries = &self.entries;
        let found = self
            .slots
            .find(hash, |p| {
                entries.get(slot(*p)).is_some_and(|e| e.user == entry.user)
            })
            .copied();
        let previous = match found.map(slot).filter(|p| *p < self.entries.len()) {
            Some(position) => {
                self.unplace(position);
                let old = self.entries.get_mut(position).map(|existing| {
                    let old = *existing;
                    *existing = entry;
                    old
                });
                self.place(position);
                old
            }
            None => {
                let Ok(position) = u32::try_from(self.entries.len()) else {
                    self.release(entry.roleset);
                    return None;
                };
                self.entries.push(entry);
                let seeded = &self.seeded;
                let entries = &self.entries;
                self.slots.insert_unique(hash, position, |p| {
                    entries.get(slot(*p)).map_or(0, |e| seeded.hash(&e.user))
                });
                self.place(slot(position));
                self.version = self.version.wrapping_add(1);
                None
            }
        };
        if let Some(old) = previous {
            self.release(old.roleset);
        }
        previous
    }

    pub(crate) fn set_flags(&mut self, user: UserId, flags: MemberFlags) {
        self.writes = self.writes.wrapping_add(1);
        let hash = self.seeded.hash(&user);
        let entries = &self.entries;
        let found = self
            .slots
            .find(hash, |p| {
                entries.get(slot(*p)).is_some_and(|e| e.user == user)
            })
            .copied();
        let Some(position) = found.map(slot).filter(|p| *p < self.entries.len()) else {
            return;
        };
        self.unplace(position);
        if let Some(entry) = self.entries.get_mut(position) {
            entry.flags = flags;
            if flags.contains(MemberFlags::QUARANTINED) {
                self.quarantined.insert(entry.user);
            } else {
                self.quarantined.remove(&entry.user);
            }
        }
        self.place(position);
    }

    pub(crate) fn remove(&mut self, user: UserId) -> Option<MemberEntry> {
        self.writes = self.writes.wrapping_add(1);
        let hash = self.seeded.hash(&user);
        let entries = &self.entries;
        let position = match self.slots.find_entry(hash, |p| {
            entries.get(slot(*p)).is_some_and(|e| e.user == user)
        }) {
            Ok(occupied) => slot(occupied.remove().0),
            Err(_) => return None,
        };
        if position >= self.entries.len() {
            return None;
        }
        self.unplace(position);
        let removed = self.entries.swap_remove(position);
        let last = self.entries.len();
        if position < self.set_pos.len() {
            self.set_pos.swap_remove(position);
        }
        if let Some(moved) = self.entries.get(position).copied() {
            let moved_hash = self.seeded.hash(&moved.user);
            let last = u32::try_from(last).unwrap_or(u32::MAX);
            let now = u32::try_from(position).unwrap_or(u32::MAX);
            if let Some(p) = self.slots.find_mut(moved_hash, |p| *p == last) {
                *p = now;
            }
            let at = self.set_pos.get(position).copied().unwrap_or(NOT_PLACED);
            if at != NOT_PLACED
                && let Some(member) = self
                    .by_set
                    .get_mut(moved.roleset.index())
                    .and_then(|list| list.get_mut(slot(at)))
            {
                *member = now;
            }
        }
        self.version = self.version.wrapping_add(1);
        self.release(removed.roleset);
        self.quarantined.remove(&removed.user);
        Some(removed)
    }

    pub(crate) fn remapped(&self, remap: &[u32]) -> MemberIndex {
        let mut next = self.clone();
        let mut refcount = vec![0u32; remap.iter().filter(|r| **r != DEAD).count()];
        for entry in &mut next.entries {
            let target = remap.get(entry.roleset.index()).copied().unwrap_or(0);
            let target = if target == DEAD { 0 } else { target };
            entry.roleset = RoleSetId::new(target);
            if let Some(count) = refcount.get_mut(entry.roleset.index()) {
                *count = count.saturating_add(1);
            }
        }
        next.live_sets = u32::try_from(refcount.iter().filter(|c| **c > 0).count()).unwrap_or(0);
        next.refcount = refcount;
        next.rebuild_sets();
        next.generation = next.generation.wrapping_add(1);
        next.version = next.version.wrapping_add(1);
        next.writes = next.writes.wrapping_add(1);
        next
    }

    fn retain(&mut self, set: RoleSetId) {
        let index = set.index();
        if self.refcount.len() <= index {
            self.refcount.resize(index.saturating_add(1), 0);
        }
        if let Some(count) = self.refcount.get_mut(index) {
            if *count == 0 {
                self.live_sets = self.live_sets.saturating_add(1);
            }
            *count = count.saturating_add(1);
        }
    }

    fn release(&mut self, set: RoleSetId) {
        if let Some(count) = self.refcount.get_mut(set.index()) {
            *count = count.saturating_sub(1);
            if *count == 0 {
                self.live_sets = self.live_sets.saturating_sub(1);
            }
        }
    }
}

const NOT_PLACED: u32 = u32::MAX;

fn slot(position: u32) -> usize {
    usize::try_from(position).unwrap_or(usize::MAX)
}
