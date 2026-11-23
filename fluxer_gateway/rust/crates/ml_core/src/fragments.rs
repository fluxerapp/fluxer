// SPDX-License-Identifier: AGPL-3.0-or-later

use foldhash::fast::FixedState;
use hashbrown::HashMap;

use crate::ids::{Slot, UserId};

pub const ENTRY_OVERHEAD: usize = 64;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Revs {
    pub user: UserId,
    pub member: u32,
    pub presence: u32,
    pub shown: bool,
}

#[derive(Debug)]
struct Entry {
    user: UserId,
    member_rev: u32,
    presence_rev: u32,
    shown: bool,
    list: Box<[u8]>,
    page: Box<[u8]>,
    referenced: bool,
}

impl Entry {
    fn cost(&self) -> usize {
        self.list
            .len()
            .saturating_add(self.page.len())
            .saturating_add(ENTRY_OVERHEAD)
    }

    fn member_valid(&self, revs: Revs) -> bool {
        self.user == revs.user && self.member_rev == revs.member
    }

    fn list_valid(&self, revs: Revs) -> bool {
        self.member_valid(revs)
            && self.shown == revs.shown
            && (!revs.shown || self.presence_rev == revs.presence)
    }
}

#[derive(Debug)]
pub struct Fragments {
    entries: HashMap<Slot, Entry, FixedState>,
    ring: Vec<Slot>,
    hand: usize,
    bytes: usize,
    budget: usize,
}

impl Fragments {
    pub fn new(budget: usize, seed: u64) -> Fragments {
        Fragments {
            entries: HashMap::with_hasher(FixedState::with_seed(seed.rotate_left(29))),
            ring: Vec::new(),
            hand: 0,
            bytes: 0,
            budget,
        }
    }

    pub fn bytes(&self) -> usize {
        self.bytes
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn touch_list(&mut self, slot: Slot, revs: Revs) -> Option<usize> {
        match self.entries.get_mut(&slot) {
            Some(entry) if entry.list_valid(revs) => {
                entry.referenced = true;
                Some(entry.list.len())
            }
            _ => None,
        }
    }

    pub fn touch_page(&mut self, slot: Slot, revs: Revs) -> bool {
        match self.entries.get_mut(&slot) {
            Some(entry) if entry.member_valid(revs) => {
                entry.referenced = true;
                true
            }
            _ => false,
        }
    }

    pub fn list_bytes(&self, slot: Slot) -> Option<&[u8]> {
        self.entries.get(&slot).map(|e| e.list.as_ref())
    }

    pub fn page_bytes(&self, slot: Slot) -> Option<&[u8]> {
        self.entries.get(&slot).map(|e| e.page.as_ref())
    }

    pub fn put(&mut self, slot: Slot, revs: Revs, list: Box<[u8]>, page: Box<[u8]>) {
        let entry = Entry {
            user: revs.user,
            member_rev: revs.member,
            presence_rev: revs.presence,
            shown: revs.shown,
            list,
            page,
            referenced: true,
        };
        self.bytes = self.bytes.saturating_add(entry.cost());
        match self.entries.insert(slot, entry) {
            Some(old) => self.bytes = self.bytes.saturating_sub(old.cost()),
            None => self.ring.push(slot),
        }
    }

    pub fn trim(&mut self) {
        let mut steps = self.ring.len().saturating_mul(2);
        while self.bytes > self.budget && steps > 0 && !self.ring.is_empty() {
            steps = steps.saturating_sub(1);
            if self.hand >= self.ring.len() {
                self.hand = 0;
            }
            let Some(slot) = self.ring.get(self.hand).copied() else {
                break;
            };
            let evicted = match self.entries.get_mut(&slot) {
                Some(e) if e.referenced => {
                    e.referenced = false;
                    None
                }
                Some(_) => self.entries.remove(&slot).map(|e| e.cost()),
                None => Some(0),
            };
            match evicted {
                Some(cost) => {
                    self.bytes = self.bytes.saturating_sub(cost);
                    self.ring.swap_remove(self.hand);
                }
                None => self.hand = self.hand.saturating_add(1),
            }
        }
    }

    pub fn check(&self, idle: bool) -> Result<(), String> {
        let sum = self
            .entries
            .values()
            .map(Entry::cost)
            .fold(0usize, usize::saturating_add);
        if sum != self.bytes {
            return Err(format!(
                "fragment bytes {} but entries sum to {sum}",
                self.bytes
            ));
        }
        if self.ring.len() != self.len() {
            return Err(format!(
                "fragment ring holds {} entries for {} fragments",
                self.ring.len(),
                self.len()
            ));
        }
        if idle && self.bytes > self.budget {
            return Err(format!(
                "fragment bytes {} above budget {} with no render held",
                self.bytes, self.budget
            ));
        }
        Ok(())
    }
}
