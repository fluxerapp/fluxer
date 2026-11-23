// SPDX-License-Identifier: AGPL-3.0-or-later

use core::cmp::Ordering;

use crate::budget::Meter;
use crate::ids::Slot;
use crate::members::Members;

pub const BLOCK_MAX: usize = 256;
pub const BLOCK_MIN: usize = 64;
const BUILD_FILL: usize = 192;

pub type Mask = [u64; 4];

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ColId(u16);

impl ColId {
    pub const fn new(value: u16) -> ColId {
        ColId(value)
    }

    pub const fn index(self) -> usize {
        self.0 as usize
    }

    pub fn from_index(index: usize) -> Option<ColId> {
        u16::try_from(index).ok().map(ColId)
    }
}

#[derive(Clone, Debug, Default)]
struct Block {
    prefix: Vec<u64>,
    slot: Vec<Slot>,
}

impl Block {
    fn len(&self) -> usize {
        self.slot.len()
    }

    fn with(entries: &[(u64, Slot)]) -> Block {
        let mut prefix = Vec::with_capacity(BLOCK_MAX);
        let mut slot = Vec::with_capacity(BLOCK_MAX);
        prefix.extend(entries.iter().map(|(p, _)| *p));
        slot.extend(entries.iter().map(|(_, s)| *s));
        Block { prefix, slot }
    }
}

#[derive(Clone, Debug, Default)]
struct Col {
    counts: Vec<u16>,
    masks: Vec<Mask>,
    total: u32,
}

#[derive(Clone, Debug, Default)]
pub struct BlockedList {
    blocks: Vec<Block>,
    cols: Vec<Option<Col>>,
    len: u32,
}

pub trait Columns {
    fn live(&self) -> &[ColId];
    fn visible(&self, col: ColId, slot: Slot) -> bool;
}

pub fn split_sizes(n: usize, pieces: usize) -> impl Iterator<Item = usize> {
    let pieces = pieces.max(1);
    let base = n.checked_div(pieces).unwrap_or(0);
    let extra = n.checked_rem(pieces).unwrap_or(0);
    (0..pieces).map(move |p| {
        if p < extra {
            base.saturating_add(1)
        } else {
            base
        }
    })
}

fn bit_mask(entries: &[Slot], col: ColId, columns: &dyn Columns, meter: &mut Meter) -> Mask {
    let mut mask = [0u64; 4];
    for (i, slot) in entries.iter().enumerate() {
        meter.charge(1);
        if columns.visible(col, *slot) {
            set_bit(&mut mask, i);
        }
    }
    mask
}

fn set_bit(mask: &mut Mask, i: usize) {
    if let Some(word) = mask.get_mut(i / 64) {
        *word |= 1u64 << (i % 64);
    }
}

pub fn get_bit(mask: &Mask, i: usize) -> bool {
    mask.get(i / 64)
        .is_some_and(|word| word & (1u64 << (i % 64)) != 0)
}

fn popcount(mask: &Mask) -> u16 {
    mask.iter()
        .map(|w| w.count_ones() as u16)
        .fold(0u16, u16::saturating_add)
}

fn insert_bit(mask: &mut Mask, i: usize, value: bool) {
    let at = i / 64;
    let bit = (i % 64) as u32;
    let mut carry = 0u64;
    for (j, word) in mask.iter_mut().enumerate() {
        if j < at {
            continue;
        }
        let old = *word;
        if j == at {
            let low_mask = (1u64 << bit).wrapping_sub(1);
            *word = (old & low_mask) | ((old & !low_mask) << 1) | (u64::from(value) << bit);
        } else {
            *word = (old << 1) | carry;
        }
        carry = old >> 63;
    }
}

fn remove_bit(mask: &mut Mask, i: usize) {
    let at = i / 64;
    let bit = (i % 64) as u32;
    let snapshot = *mask;
    for (j, word) in mask.iter_mut().enumerate() {
        if j < at {
            continue;
        }
        let old = *word;
        let next_low = snapshot.get(j.wrapping_add(1)).map_or(0, |n| n & 1);
        if j == at {
            let low_mask = (1u64 << bit).wrapping_sub(1);
            *word = (old & low_mask) | ((old >> 1) & !low_mask) | (next_low << 63);
        } else {
            *word = (old >> 1) | (next_low << 63);
        }
    }
}

fn nth_set(mask: &Mask, mut n: u32) -> Option<usize> {
    for (j, word) in mask.iter().enumerate() {
        let ones = word.count_ones();
        if n < ones {
            let mut w = *word;
            for _ in 0..n {
                w &= w.wrapping_sub(1);
            }
            return Some(
                j.saturating_mul(64)
                    .saturating_add(w.trailing_zeros() as usize),
            );
        }
        n = n.saturating_sub(ones);
    }
    None
}

impl BlockedList {
    pub fn new() -> BlockedList {
        BlockedList::default()
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn from_prefixed(
        entries: &[(u64, Slot)],
        columns: &dyn Columns,
        meter: &mut Meter,
    ) -> BlockedList {
        let mut list = BlockedList::new();
        list.fill_from(entries, columns, meter);
        list
    }

    fn fill_from(&mut self, entries: &[(u64, Slot)], columns: &dyn Columns, meter: &mut Meter) {
        let n = entries.len();
        self.blocks.clear();
        if n > 0 {
            let count = n.div_ceil(BUILD_FILL).max(1);
            let mut at = 0usize;
            for size in split_sizes(n, count) {
                let end = at.saturating_add(size).min(n);
                let chunk = entries.get(at..end).unwrap_or_default();
                meter.charge(chunk.len() as u64);
                self.blocks.push(Block::with(chunk));
                at = end;
            }
        }
        self.len = u32::try_from(n).unwrap_or(u32::MAX);
        let live: Vec<ColId> = self
            .cols
            .iter()
            .enumerate()
            .filter(|(_, c)| c.is_some())
            .filter_map(|(i, _)| ColId::from_index(i))
            .collect();
        for col in live {
            self.fill_column(col, columns, meter);
        }
    }

    pub fn rebuild(
        &mut self,
        entries: &[Slot],
        members: &Members,
        columns: &dyn Columns,
        meter: &mut Meter,
    ) {
        let prefixed: Vec<(u64, Slot)> = entries.iter().map(|s| members.prefixed(*s)).collect();
        self.fill_from(&prefixed, columns, meter);
    }

    pub fn slots(&self) -> impl Iterator<Item = Slot> + '_ {
        self.blocks.iter().flat_map(|b| b.slot.iter().copied())
    }

    pub fn has_column(&self, col: ColId) -> bool {
        self.cols.get(col.index()).is_some_and(Option::is_some)
    }

    pub fn fill_column(&mut self, col: ColId, columns: &dyn Columns, meter: &mut Meter) {
        let mut data = Col {
            counts: Vec::with_capacity(self.blocks.len()),
            masks: Vec::with_capacity(self.blocks.len()),
            total: 0,
        };
        for block in &self.blocks {
            let mask = bit_mask(&block.slot, col, columns, meter);
            let count = popcount(&mask);
            data.total = data.total.saturating_add(u32::from(count));
            data.counts.push(count);
            data.masks.push(mask);
        }
        if self.cols.len() <= col.index() {
            self.cols
                .resize_with(col.index().saturating_add(1), || None);
        }
        if let Some(cell) = self.cols.get_mut(col.index()) {
            *cell = Some(data);
        }
    }

    pub fn drop_column(&mut self, col: ColId) {
        if let Some(cell) = self.cols.get_mut(col.index()) {
            *cell = None;
        }
    }

    pub fn count(&self, col: ColId) -> u32 {
        self.cols
            .get(col.index())
            .and_then(Option::as_ref)
            .map_or(0, |c| c.total)
    }

    fn find_block(&self, prefix: u64, slot: Slot, members: &Members, meter: &mut Meter) -> usize {
        let last = self.blocks.len().saturating_sub(1);
        let mut lo = 0usize;
        let mut hi = last;
        while lo < hi {
            let mid = lo.saturating_add(hi.saturating_sub(lo) / 2);
            meter.charge(1);
            let tail = self
                .blocks
                .get(mid)
                .and_then(|b| b.prefix.last().copied().zip(b.slot.last().copied()));
            let below = tail.is_some_and(|(tp, ts)| {
                members.cmp_prefixed(tp, ts, prefix, slot) == Ordering::Less
            });
            if below {
                lo = mid.saturating_add(1);
            } else {
                hi = mid;
            }
        }
        lo
    }

    fn search_in(
        block: &Block,
        prefix: u64,
        slot: Slot,
        members: &Members,
        meter: &mut Meter,
    ) -> Result<usize, usize> {
        let mut lo = 0usize;
        let mut hi = block.len();
        while lo < hi {
            let mid = lo.saturating_add(hi.saturating_sub(lo) / 2);
            meter.charge(1);
            let (Some(p), Some(s)) = (block.prefix.get(mid), block.slot.get(mid)) else {
                return Err(lo);
            };
            match members.cmp_prefixed(*p, *s, prefix, slot) {
                Ordering::Less => lo = mid.saturating_add(1),
                Ordering::Greater => hi = mid,
                Ordering::Equal => return Ok(mid),
            }
        }
        Err(lo)
    }

    pub fn position(
        &self,
        slot: Slot,
        members: &Members,
        meter: &mut Meter,
    ) -> Option<(usize, usize)> {
        if self.blocks.is_empty() {
            return None;
        }
        let prefix = members.prefix(slot);
        let b = self.find_block(prefix, slot, members, meter);
        let block = self.blocks.get(b)?;
        Self::search_in(block, prefix, slot, members, meter)
            .ok()
            .map(|i| (b, i))
    }

    pub fn insert(
        &mut self,
        slot: Slot,
        members: &Members,
        columns: &dyn Columns,
        meter: &mut Meter,
    ) -> bool {
        if self.blocks.is_empty() {
            self.blocks.push(Block::default());
            for col in self.cols.iter_mut().flatten() {
                col.counts.push(0);
                col.masks.push([0; 4]);
            }
        }
        let prefix = members.prefix(slot);
        let b = self.find_block(prefix, slot, members, meter);
        let Some(block) = self.blocks.get_mut(b) else {
            return false;
        };
        let at = match Self::search_in(block, prefix, slot, members, meter) {
            Ok(_) => return false,
            Err(at) => at,
        };
        block.prefix.insert(at, prefix);
        block.slot.insert(at, slot);
        meter.charge(block.len() as u64 / 64);
        let full = block.len() > BLOCK_MAX;
        self.len = self.len.saturating_add(1);
        for (i, cell) in self.cols.iter_mut().enumerate() {
            let Some(col) = cell.as_mut() else {
                continue;
            };
            let Some(id) = ColId::from_index(i) else {
                continue;
            };
            meter.charge(2);
            if full {
                continue;
            }
            let visible = columns.visible(id, slot);
            if visible {
                col.total = col.total.saturating_add(1);
            }
            if let Some(mask) = col.masks.get_mut(b) {
                insert_bit(mask, at, visible);
            }
            if visible && let Some(count) = col.counts.get_mut(b) {
                *count = count.saturating_add(1);
            }
        }
        if full {
            self.repack(b, b.saturating_add(1), columns, meter);
        }
        true
    }

    pub fn remove(
        &mut self,
        slot: Slot,
        members: &Members,
        columns: &dyn Columns,
        meter: &mut Meter,
    ) -> bool {
        let Some((b, at)) = self.position(slot, members, meter) else {
            return false;
        };
        let Some(block) = self.blocks.get_mut(b) else {
            return false;
        };
        block.prefix.remove(at);
        block.slot.remove(at);
        meter.charge(block.len() as u64 / 64);
        let len = block.len();
        self.len = self.len.saturating_sub(1);
        for col in self.cols.iter_mut().flatten() {
            meter.charge(2);
            let Some(mask) = col.masks.get_mut(b) else {
                continue;
            };
            if get_bit(mask, at) {
                col.total = col.total.saturating_sub(1);
                if let Some(count) = col.counts.get_mut(b) {
                    *count = count.saturating_sub(1);
                }
            }
            remove_bit(mask, at);
        }
        if len == 0 {
            self.blocks.remove(b);
            for col in self.cols.iter_mut().flatten() {
                if b < col.counts.len() {
                    col.counts.remove(b);
                    col.masks.remove(b);
                }
            }
        } else if len < BLOCK_MIN && self.blocks.len() > 1 {
            let (lo, hi) = if b.saturating_add(1) < self.blocks.len() {
                (b, b.saturating_add(2))
            } else {
                (b.saturating_sub(1), b.saturating_add(1))
            };
            self.repack(lo, hi, columns, meter);
        }
        true
    }

    fn repack(&mut self, lo: usize, hi: usize, columns: &dyn Columns, meter: &mut Meter) {
        let Some(range) = self.blocks.get(lo..hi) else {
            return;
        };
        let entries: Vec<(u64, Slot)> = range
            .iter()
            .flat_map(|b| b.prefix.iter().copied().zip(b.slot.iter().copied()))
            .collect();
        let n = entries.len();
        let pieces = n.div_ceil(BLOCK_MAX.saturating_sub(BLOCK_MAX / 4)).max(1);
        let mut fresh = Vec::with_capacity(pieces);
        let mut at = 0usize;
        for size in split_sizes(n, pieces) {
            let end = at.saturating_add(size).min(n);
            fresh.push(Block::with(entries.get(at..end).unwrap_or_default()));
            at = end;
        }
        meter.charge(n as u64);
        let mut masks: Vec<(usize, Vec<Mask>)> = Vec::new();
        for (i, cell) in self.cols.iter().enumerate() {
            if cell.is_none() {
                continue;
            }
            let Some(id) = ColId::from_index(i) else {
                continue;
            };
            let built = fresh
                .iter()
                .map(|b| bit_mask(&b.slot, id, columns, meter))
                .collect();
            masks.push((i, built));
        }
        self.blocks.splice(lo..hi, fresh);
        for (i, built) in masks {
            let Some(Some(col)) = self.cols.get_mut(i) else {
                continue;
            };
            let removed: u32 = col
                .counts
                .get(lo..hi)
                .unwrap_or_default()
                .iter()
                .map(|c| u32::from(*c))
                .fold(0, u32::saturating_add);
            let counts: Vec<u16> = built.iter().map(popcount).collect();
            let added: u32 = counts
                .iter()
                .map(|c| u32::from(*c))
                .fold(0, u32::saturating_add);
            col.total = col.total.saturating_sub(removed).saturating_add(added);
            if hi <= col.counts.len() {
                col.counts.splice(lo..hi, counts);
                col.masks.splice(lo..hi, built);
            }
        }
    }

    pub fn walk(
        &self,
        col: Option<ColId>,
        mut skip: u32,
        mut take: u32,
        out: &mut Vec<Slot>,
        meter: &mut Meter,
    ) {
        if take == 0 {
            return;
        }
        let data = col.and_then(|c| self.cols.get(c.index()).and_then(Option::as_ref));
        for (b, block) in self.blocks.iter().enumerate() {
            meter.charge(1);
            let (count, mask) = match (col, data) {
                (None, _) => (block.len() as u32, None),
                (Some(_), Some(d)) => (
                    d.counts.get(b).copied().map_or(0, u32::from),
                    d.masks.get(b),
                ),
                (Some(_), None) => return,
            };
            if skip >= count {
                skip = skip.saturating_sub(count);
                continue;
            }
            match mask {
                None => {
                    let start = skip as usize;
                    let end = (start.saturating_add(take as usize)).min(block.len());
                    for s in block.slot.get(start..end).unwrap_or_default() {
                        out.push(*s);
                        meter.charge(3);
                        take = take.saturating_sub(1);
                    }
                }
                Some(mask) => {
                    let mut i = nth_set(mask, skip);
                    while let Some(at) = i {
                        if take == 0 {
                            break;
                        }
                        if let Some(s) = block.slot.get(at) {
                            out.push(*s);
                        }
                        meter.charge(3);
                        take = take.saturating_sub(1);
                        i = next_set(mask, at.saturating_add(1));
                    }
                }
            }
            skip = 0;
            if take == 0 {
                return;
            }
        }
    }

    pub fn bytes(&self) -> usize {
        let blocks: usize = self
            .blocks
            .iter()
            .map(|b| {
                b.prefix
                    .capacity()
                    .saturating_mul(8)
                    .saturating_add(b.slot.capacity().saturating_mul(4))
            })
            .fold(0usize, usize::saturating_add);
        let cols: usize = self
            .cols
            .iter()
            .flatten()
            .map(|c| {
                c.counts
                    .capacity()
                    .saturating_mul(2)
                    .saturating_add(c.masks.capacity().saturating_mul(32))
            })
            .fold(0usize, usize::saturating_add);
        blocks.saturating_add(cols)
    }

    pub fn check(&self, members: &Members, columns: &dyn Columns) -> Result<(), String> {
        let mut total = 0usize;
        let single = self.blocks.len() == 1;
        let mut previous: Option<Slot> = None;
        for (b, block) in self.blocks.iter().enumerate() {
            let len = block.len();
            if len == 0 || len > BLOCK_MAX || (!single && len < BLOCK_MIN) {
                return Err(format!("block {b} has {len} entries"));
            }
            if block.prefix.len() != len {
                return Err(format!("block {b} prefix column length"));
            }
            for (p, s) in block.prefix.iter().zip(block.slot.iter()) {
                if *p != members.prefix(*s) || !members.is_live(*s) {
                    return Err(format!("block {b} stale entry for slot {}", s.get()));
                }
                if let Some(prev) = previous
                    && members.cmp(prev, *s) != Ordering::Less
                {
                    return Err(format!("block {b} out of order at slot {}", s.get()));
                }
                previous = Some(*s);
            }
            total = total.saturating_add(len);
        }
        if total != self.len as usize {
            return Err(format!("len {} but {} entries", self.len, total));
        }
        for (i, cell) in self.cols.iter().enumerate() {
            let (Some(col), Some(id)) = (cell, ColId::from_index(i)) else {
                continue;
            };
            if !columns.live().contains(&id) {
                return Err(format!("column {i} kept but not live"));
            }
            if col.counts.len() != self.blocks.len() || col.masks.len() != self.blocks.len() {
                return Err(format!("column {i} has wrong block count"));
            }
            let mut sum = 0u32;
            for (b, block) in self.blocks.iter().enumerate() {
                let mask = col.masks.get(b).copied().unwrap_or_default();
                let count = col.counts.get(b).copied().unwrap_or(0);
                if popcount(&mask) != count {
                    return Err(format!("column {i} block {b} popcount differs from count"));
                }
                for bit in block.len()..256 {
                    if get_bit(&mask, bit) {
                        return Err(format!("column {i} block {b} bit {bit} set past the end"));
                    }
                }
                for (at, s) in block.slot.iter().enumerate() {
                    if get_bit(&mask, at) != columns.visible(id, *s) {
                        return Err(format!("column {i} block {b} slot {} mask stale", s.get()));
                    }
                }
                sum = sum.saturating_add(u32::from(count));
            }
            if sum != col.total {
                return Err(format!(
                    "column {i} total {} but counts sum {sum}",
                    col.total
                ));
            }
        }
        for live in columns.live() {
            if !self.has_column(*live) {
                return Err(format!("live column {} missing", live.index()));
            }
        }
        Ok(())
    }
}

fn next_set(mask: &Mask, from: usize) -> Option<usize> {
    let mut at = from;
    while at < 256 {
        let word = mask.get(at / 64).copied().unwrap_or(0);
        let rest = word >> (at % 64);
        if rest != 0 {
            return Some(at.saturating_add(rest.trailing_zeros() as usize));
        }
        at = (at / 64).saturating_add(1).saturating_mul(64);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bit_insert_and_remove_match_a_vector() {
        let mut state = 0x9e37_79b9_7f4a_7c15u64;
        let mut next = || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            state
        };
        for _ in 0..2000 {
            let mut mask = [0u64; 4];
            let mut model: Vec<bool> = Vec::new();
            for _ in 0..600 {
                let op = next() % 3;
                if op < 2 && model.len() < 256 {
                    let at = (next() % (model.len() as u64 + 1)) as usize;
                    let v = next() % 2 == 0;
                    insert_bit(&mut mask, at, v);
                    model.insert(at, v);
                } else if !model.is_empty() {
                    let at = (next() % model.len() as u64) as usize;
                    remove_bit(&mut mask, at);
                    model.remove(at);
                }
                for (i, v) in model.iter().enumerate() {
                    assert_eq!(get_bit(&mask, i), *v);
                }
                for i in model.len()..256 {
                    assert!(!get_bit(&mask, i));
                }
                let ones = model.iter().filter(|v| **v).count();
                for n in 0..ones {
                    let expect = model
                        .iter()
                        .enumerate()
                        .filter(|(_, v)| **v)
                        .nth(n)
                        .map(|(i, _)| i);
                    assert_eq!(nth_set(&mask, n as u32), expect);
                }
                assert_eq!(nth_set(&mask, ones as u32), None);
            }
        }
    }
}
