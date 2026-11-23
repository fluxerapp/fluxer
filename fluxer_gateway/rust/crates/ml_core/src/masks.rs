// SPDX-License-Identifier: AGPL-3.0-or-later

use push_core::{MaskKind, PushMasks};

use crate::ids::{Slot, UserId};
use crate::members::Members;

pub const MAX_UPDATES: usize = 4_096;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct MaskUpdate {
    pub user: UserId,
    pub all_messages: bool,
    pub uncertain: bool,
    pub muted: bool,
    pub active: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum MaskReply {
    Masks {
        immediate: Vec<UserId>,
        held: Vec<UserId>,
    },
    Stale,
}

#[derive(Debug, Default)]
pub struct Masks {
    masks: Option<PushMasks>,
}

fn slot_count(members: &Members) -> u32 {
    u32::try_from(members.capacity_slots()).unwrap_or(u32::MAX)
}

impl Masks {
    pub fn new() -> Masks {
        Masks::default()
    }

    pub fn reset(&mut self) {
        self.masks = None;
    }

    fn seed(members: &Members) -> PushMasks {
        let mut masks = PushMasks::with_capacity(slot_count(members));
        for slot in members.slots() {
            masks.set(slot.get(), MaskKind::Uncertain, true);
        }
        masks
    }

    pub fn joined(&mut self, members: &Members, slot: Slot) {
        if let Some(masks) = self.masks.as_mut() {
            masks.grow(slot_count(members));
            masks.clear_slot(slot.get());
            masks.set(slot.get(), MaskKind::Uncertain, true);
        }
    }

    pub fn left(&mut self, slot: Slot) {
        if let Some(masks) = self.masks.as_mut() {
            masks.clear_slot(slot.get());
        }
    }

    pub fn set(&mut self, members: &Members, updates: &[MaskUpdate]) {
        let masks = self.masks.get_or_insert_with(|| Masks::seed(members));
        masks.grow(slot_count(members));
        for update in updates {
            let Some(slot) = members.find(update.user) else {
                continue;
            };
            let at = slot.get();
            masks.set(at, MaskKind::AllMessages, update.all_messages);
            masks.set(at, MaskKind::Uncertain, update.uncertain);
            masks.set(at, MaskKind::Muted, update.muted);
            masks.set(at, MaskKind::Active, update.active);
        }
    }

    pub fn query(&self, members: &Members) -> MaskReply {
        let Some(masks) = self.masks.as_ref() else {
            return MaskReply::Stale;
        };
        let mut slots = Vec::new();
        masks.immediate(&mut slots);
        let immediate = users_of(members, &slots);
        slots.clear();
        masks.held(&mut slots);
        let held = users_of(members, &slots);
        MaskReply::Masks { immediate, held }
    }

    pub fn bytes(&self) -> usize {
        self.masks.as_ref().map_or(0, PushMasks::bytes)
    }
}

fn users_of(members: &Members, slots: &[u32]) -> Vec<UserId> {
    slots
        .iter()
        .map(|s| Slot::new(*s))
        .filter(|s| members.is_live(*s))
        .map(|s| members.user(s))
        .collect()
}
