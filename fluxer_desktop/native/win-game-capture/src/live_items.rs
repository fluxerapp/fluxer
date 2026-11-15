// SPDX-License-Identifier: AGPL-3.0-or-later

pub const LIVE_ITEMS_MAX: usize = 8;

pub trait WeakItem {
    type Strong;

    fn upgrade(&self) -> Option<Self::Strong>;
}

struct LiveItem<W> {
    window: isize,
    weak: W,
    used_at: u64,
}

pub struct LiveItems<W> {
    slots: [Option<LiveItem<W>>; LIVE_ITEMS_MAX],
    uses: u64,
}

impl<W: WeakItem> LiveItems<W> {
    pub const fn new() -> Self {
        Self {
            slots: [const { None }; LIVE_ITEMS_MAX],
            uses: 0,
        }
    }

    pub fn lookup(&mut self, window: isize) -> Option<W::Strong> {
        let used_at = self.next_use();
        for slot in &mut self.slots {
            let Some(item) = slot else {
                continue;
            };
            if item.window != window {
                continue;
            }
            match item.weak.upgrade() {
                Some(strong) => {
                    item.used_at = used_at;
                    return Some(strong);
                }
                None => *slot = None,
            }
        }
        None
    }

    pub fn remember(&mut self, window: isize, weak: W) {
        let used_at = self.next_use();
        let index = self.vacant_index();
        assert!(index < LIVE_ITEMS_MAX);
        self.slots[index] = Some(LiveItem {
            window,
            weak,
            used_at,
        });
    }

    fn next_use(&mut self) -> u64 {
        assert!(self.uses < u64::MAX);
        self.uses += 1;
        self.uses
    }

    fn vacant_index(&self) -> usize {
        let mut least_recently_used = 0;
        let mut least_recent_use = u64::MAX;
        for (index, slot) in self.slots.iter().enumerate() {
            let Some(item) = slot else {
                return index;
            };
            if item.weak.upgrade().is_none() {
                return index;
            }
            if item.used_at < least_recent_use {
                least_recent_use = item.used_at;
                least_recently_used = index;
            }
        }
        least_recently_used
    }
}
