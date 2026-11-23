// SPDX-License-Identifier: AGPL-3.0-or-later

#![forbid(unsafe_code)]

pub mod job;
#[cfg(feature = "nif")]
pub mod nifs;

const WORD_BITS: usize = 64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MaskKind {
    AllMessages,
    Uncertain,
    Muted,
    Active,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PushMasks {
    all_messages: Vec<u64>,
    uncertain: Vec<u64>,
    muted: Vec<u64>,
    active: Vec<u64>,
    slots: u32,
}

fn words_for(slots: u32) -> usize {
    (slots as usize).div_ceil(WORD_BITS)
}

fn locate(slot: u32) -> (usize, u64) {
    let slot = slot as usize;
    (slot / WORD_BITS, 1u64 << (slot % WORD_BITS))
}

fn collect(words: impl Iterator<Item = u64>, out: &mut Vec<u32>) {
    out.clear();
    let mut base: u32 = 0;
    for word in words {
        let mut bits = word;
        while bits != 0 {
            out.push(base | bits.trailing_zeros());
            bits &= bits - 1;
        }
        base = base.wrapping_add(WORD_BITS as u32);
    }
}

impl PushMasks {
    pub fn new() -> PushMasks {
        PushMasks::default()
    }

    pub fn with_capacity(slots: u32) -> PushMasks {
        let mut masks = PushMasks::default();
        masks.grow(slots);
        masks
    }

    pub fn grow(&mut self, slots: u32) {
        if slots <= self.slots {
            return;
        }
        let words = words_for(slots);
        self.all_messages.resize(words, 0);
        self.uncertain.resize(words, 0);
        self.muted.resize(words, 0);
        self.active.resize(words, 0);
        self.slots = slots;
    }

    pub fn slots(&self) -> u32 {
        self.slots
    }

    fn mask(&self, kind: MaskKind) -> &Vec<u64> {
        match kind {
            MaskKind::AllMessages => &self.all_messages,
            MaskKind::Uncertain => &self.uncertain,
            MaskKind::Muted => &self.muted,
            MaskKind::Active => &self.active,
        }
    }

    fn mask_mut(&mut self, kind: MaskKind) -> &mut Vec<u64> {
        match kind {
            MaskKind::AllMessages => &mut self.all_messages,
            MaskKind::Uncertain => &mut self.uncertain,
            MaskKind::Muted => &mut self.muted,
            MaskKind::Active => &mut self.active,
        }
    }

    pub fn set(&mut self, slot: u32, mask: MaskKind, on: bool) {
        if slot >= self.slots {
            if !on {
                return;
            }
            self.grow(slot.saturating_add(1));
        }
        let (index, bit) = locate(slot);
        if let Some(word) = self.mask_mut(mask).get_mut(index) {
            if on {
                *word |= bit;
            } else {
                *word &= !bit;
            }
        }
    }

    pub fn get(&self, slot: u32, mask: MaskKind) -> bool {
        let (index, bit) = locate(slot);
        self.mask(mask)
            .get(index)
            .is_some_and(|word| word & bit != 0)
    }

    pub fn clear_slot(&mut self, slot: u32) {
        let (index, bit) = locate(slot);
        for mask in [
            &mut self.all_messages,
            &mut self.uncertain,
            &mut self.muted,
            &mut self.active,
        ] {
            if let Some(word) = mask.get_mut(index) {
                *word &= !bit;
            }
        }
    }

    pub fn immediate(&self, out: &mut Vec<u32>) {
        let words = self
            .all_messages
            .iter()
            .zip(&self.uncertain)
            .zip(&self.active)
            .zip(&self.muted)
            .map(|(((all, uncertain), active), muted)| (all | uncertain) & !active & !muted);
        collect(words, out);
    }

    pub fn held(&self, out: &mut Vec<u32>) {
        let words = self
            .all_messages
            .iter()
            .zip(&self.uncertain)
            .zip(&self.active)
            .zip(&self.muted)
            .map(|(((all, uncertain), active), muted)| (all | uncertain) & active & !muted);
        collect(words, out);
    }

    pub fn bytes(&self) -> usize {
        let words = self.all_messages.capacity()
            + self.uncertain.capacity()
            + self.muted.capacity()
            + self.active.capacity();
        std::mem::size_of::<PushMasks>() + words * std::mem::size_of::<u64>()
    }
}

#[cfg(test)]
mod tests {
    use super::{MaskKind, PushMasks};
    use proptest::prelude::*;

    #[derive(Clone, Copy, Debug, Default)]
    struct Row {
        all: bool,
        uncertain: bool,
        muted: bool,
        active: bool,
    }

    fn masks_of(rows: &[Row]) -> PushMasks {
        let mut masks = PushMasks::new();
        for (slot, row) in rows.iter().enumerate() {
            let slot = slot as u32;
            masks.set(slot, MaskKind::AllMessages, row.all);
            masks.set(slot, MaskKind::Uncertain, row.uncertain);
            masks.set(slot, MaskKind::Muted, row.muted);
            masks.set(slot, MaskKind::Active, row.active);
        }
        masks
    }

    fn row() -> impl Strategy<Value = Row> {
        (any::<bool>(), any::<bool>(), any::<bool>(), any::<bool>()).prop_map(
            |(all, uncertain, muted, active)| Row {
                all,
                uncertain,
                muted,
                active,
            },
        )
    }

    proptest! {
        #[test]
        fn queries_match_the_row_formula(rows in proptest::collection::vec(row(), 0..400)) {
            let masks = masks_of(&rows);
            let mut immediate = Vec::new();
            let mut held = Vec::new();
            masks.immediate(&mut immediate);
            masks.held(&mut held);
            let want_immediate: Vec<u32> = rows
                .iter()
                .enumerate()
                .filter(|(_, r)| (r.all || r.uncertain) && !r.active && !r.muted)
                .map(|(slot, _)| slot as u32)
                .collect();
            let want_held: Vec<u32> = rows
                .iter()
                .enumerate()
                .filter(|(_, r)| (r.all || r.uncertain) && r.active && !r.muted)
                .map(|(slot, _)| slot as u32)
                .collect();
            prop_assert_eq!(immediate, want_immediate);
            prop_assert_eq!(held, want_held);
        }
    }

    #[test]
    fn clear_slot_drops_every_bit() {
        let mut masks = PushMasks::with_capacity(130);
        masks.set(129, MaskKind::AllMessages, true);
        masks.set(129, MaskKind::Active, true);
        masks.clear_slot(129);
        let mut out = vec![7];
        masks.held(&mut out);
        assert!(out.is_empty());
        assert!(!masks.get(129, MaskKind::AllMessages));
    }

    #[test]
    fn set_past_capacity_grows() {
        let mut masks = PushMasks::new();
        masks.set(1_000, MaskKind::Uncertain, true);
        assert_eq!(masks.slots(), 1_001);
        let mut out = Vec::new();
        masks.immediate(&mut out);
        assert_eq!(out, vec![1_000]);
    }

    #[test]
    fn clearing_past_capacity_is_a_no_op() {
        let mut masks = PushMasks::new();
        masks.set(5, MaskKind::Muted, false);
        assert_eq!(masks.slots(), 0);
    }
}
