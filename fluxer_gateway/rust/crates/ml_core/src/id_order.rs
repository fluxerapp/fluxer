// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::budget::Meter;
use crate::ids::UserId;

const MAX: usize = 512;
const MIN: usize = 128;
const FILL: usize = 384;

#[derive(Clone, Debug, Default)]
pub struct IdOrder {
    blocks: Vec<Vec<u64>>,
    first: Vec<u64>,
    len: u32,
}

impl IdOrder {
    pub fn new() -> IdOrder {
        IdOrder::default()
    }

    pub fn from_sorted(ids: &[u64]) -> IdOrder {
        let mut order = IdOrder::new();
        if !ids.is_empty() {
            let count = ids.len().div_ceil(FILL).max(1);
            let mut at = 0usize;
            for size in crate::blocked::split_sizes(ids.len(), count) {
                let end = at.saturating_add(size).min(ids.len());
                order
                    .blocks
                    .push(ids.get(at..end).unwrap_or_default().to_vec());
                at = end;
            }
        }
        order.len = u32::try_from(ids.len()).unwrap_or(u32::MAX);
        order.refresh_first();
        order
    }

    fn refresh_first(&mut self) {
        self.first = self
            .blocks
            .iter()
            .map(|b| b.first().copied().unwrap_or(0))
            .collect();
    }

    fn block_for(&self, id: u64) -> usize {
        self.first.partition_point(|f| *f <= id).saturating_sub(1)
    }

    pub fn insert(&mut self, id: UserId, meter: &mut Meter) -> bool {
        let id = id.get();
        if self.blocks.is_empty() {
            self.blocks.push(Vec::with_capacity(MAX));
            self.first.push(id);
        }
        let b = self.block_for(id);
        let Some(block) = self.blocks.get_mut(b) else {
            return false;
        };
        let Err(at) = block.binary_search(&id) else {
            return false;
        };
        block.insert(at, id);
        meter.charge(16u64.saturating_add(block.len() as u64 / 64));
        self.len = self.len.saturating_add(1);
        if block.len() > MAX {
            let tail = block.split_off(block.len() / 2);
            self.blocks.insert(b.saturating_add(1), tail);
        }
        self.refresh_block_first(b);
        true
    }

    fn refresh_block_first(&mut self, around: usize) {
        if self.first.len() != self.blocks.len() {
            self.refresh_first();
            return;
        }
        for i in around.saturating_sub(1)..around.saturating_add(2) {
            if let (Some(f), Some(b)) = (self.first.get_mut(i), self.blocks.get(i)) {
                *f = b.first().copied().unwrap_or(0);
            }
        }
    }

    pub fn remove(&mut self, id: UserId, meter: &mut Meter) -> bool {
        let id = id.get();
        let b = self.block_for(id);
        let Some(block) = self.blocks.get_mut(b) else {
            return false;
        };
        let Ok(at) = block.binary_search(&id) else {
            return false;
        };
        block.remove(at);
        meter.charge(16u64.saturating_add(block.len() as u64 / 64));
        self.len = self.len.saturating_sub(1);
        let len = block.len();
        if len == 0 {
            self.blocks.remove(b);
            self.refresh_first();
        } else if len < MIN && self.blocks.len() > 1 {
            let (lo, hi) = if b.saturating_add(1) < self.blocks.len() {
                (b, b.saturating_add(1))
            } else {
                (b.saturating_sub(1), b)
            };
            let right = self
                .blocks
                .get_mut(hi)
                .map(core::mem::take)
                .unwrap_or_default();
            if let Some(left) = self.blocks.get_mut(lo) {
                left.extend_from_slice(&right);
                meter.charge(right.len() as u64 / 64);
                if left.len() > MAX {
                    let tail = left.split_off(left.len() / 2);
                    if let Some(cell) = self.blocks.get_mut(hi) {
                        *cell = tail;
                    }
                } else {
                    self.blocks.remove(hi);
                }
            }
            self.refresh_first();
        } else {
            self.refresh_block_first(b);
        }
        true
    }

    pub fn page(&self, offset: u32, limit: u32, out: &mut Vec<UserId>, meter: &mut Meter) {
        let mut skip = offset as usize;
        let mut take = limit as usize;
        for block in &self.blocks {
            meter.charge(1);
            if take == 0 {
                return;
            }
            if skip >= block.len() {
                skip = skip.saturating_sub(block.len());
                continue;
            }
            let end = skip.saturating_add(take).min(block.len());
            for id in block.get(skip..end).unwrap_or_default() {
                out.push(UserId::new(*id));
                meter.charge(1);
            }
            take = take.saturating_sub(end.saturating_sub(skip));
            skip = 0;
        }
    }

    pub fn page_after(
        &self,
        after: Option<UserId>,
        limit: u32,
        out: &mut Vec<UserId>,
        meter: &mut Meter,
    ) {
        let Some(after) = after else {
            self.page(0, limit, out, meter);
            return;
        };
        let mut take = limit as usize;
        let start = self.block_for(after.get());
        meter.charge(16);
        for (i, block) in self.blocks.iter().enumerate().skip(start) {
            if take == 0 {
                return;
            }
            let from = if i == start {
                block.partition_point(|id| *id <= after.get())
            } else {
                0
            };
            let end = from.saturating_add(take).min(block.len());
            for id in block.get(from..end).unwrap_or_default() {
                out.push(UserId::new(*id));
                meter.charge(1);
            }
            take = take.saturating_sub(end.saturating_sub(from));
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = u64> + '_ {
        self.blocks.iter().flatten().copied()
    }

    pub fn bytes(&self) -> usize {
        self.blocks
            .iter()
            .map(|b| b.capacity().saturating_mul(8))
            .fold(
                self.first.capacity().saturating_mul(8),
                usize::saturating_add,
            )
    }

    pub fn check(&self) -> Result<(), String> {
        let ids: Vec<u64> = self.iter().collect();
        if ids.windows(2).any(|w| matches!(w, [a, b] if a >= b)) {
            return Err("id order not strictly ascending".into());
        }
        if ids.len() != self.len as usize {
            return Err("id order length".into());
        }
        if self.first.len() != self.blocks.len() {
            return Err("id order first vector length".into());
        }
        let single = self.blocks.len() == 1;
        for (b, block) in self.blocks.iter().enumerate() {
            if block.is_empty() || block.len() > MAX || (!single && block.len() < MIN / 2) {
                return Err(format!("id block {b} has {} entries", block.len()));
            }
            if self.first.get(b) != block.first() {
                return Err(format!("id block {b} first stale"));
            }
        }
        Ok(())
    }
}
