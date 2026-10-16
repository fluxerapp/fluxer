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

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::{Rc, Weak};

    const FRAMES_IN_HALF_AN_HOUR_AT_30_FPS: u32 = 30 * 60 * 30;

    impl WeakItem for Weak<isize> {
        type Strong = Rc<isize>;

        fn upgrade(&self) -> Option<Rc<isize>> {
            Weak::upgrade(self)
        }
    }

    struct CaptureService {
        items_created: u32,
        live: LiveItems<Weak<isize>>,
    }

    impl CaptureService {
        fn new() -> Self {
            Self {
                items_created: 0,
                live: LiveItems::new(),
            }
        }

        fn create_for_window(&mut self, window: isize) -> Rc<isize> {
            if let Some(item) = self.live.lookup(window) {
                return item;
            }
            self.items_created += 1;
            let item = Rc::new(window);
            self.live.remember(window, Rc::downgrade(&item));
            item
        }

        fn occupied_count(&self) -> usize {
            self.live.slots.iter().flatten().count()
        }
    }

    #[test]
    fn per_frame_probes_reuse_the_item_the_share_holds() {
        let mut service = CaptureService::new();
        drop(service.create_for_window(0x1234));
        let share_item = service.create_for_window(0x1234);
        assert_eq!(service.items_created, 2);
        for _ in 0..FRAMES_IN_HALF_AN_HOUR_AT_30_FPS {
            let probe = service.create_for_window(0x1234);
            assert!(Rc::ptr_eq(&probe, &share_item));
        }
        assert_eq!(service.items_created, 2);
        assert_eq!(Rc::strong_count(&share_item), 1);
    }

    #[test]
    fn probes_without_a_held_item_each_create_one() {
        let mut service = CaptureService::new();
        for _ in 0..100 {
            drop(service.create_for_window(7));
        }
        assert_eq!(service.items_created, 100);
        assert_eq!(service.occupied_count(), 1);
    }

    #[test]
    fn a_new_share_of_the_same_window_creates_a_new_item() {
        let mut service = CaptureService::new();
        drop(service.create_for_window(9));
        let second_share_item = service.create_for_window(9);
        assert_eq!(service.items_created, 2);
        drop(second_share_item);
        assert!(service.live.lookup(9).is_none());
        assert_eq!(service.occupied_count(), 0);
    }

    #[test]
    fn windows_never_share_an_item() {
        let mut service = CaptureService::new();
        let first = service.create_for_window(1);
        let second = service.create_for_window(2);
        assert!(!Rc::ptr_eq(&first, &second));
        assert!(Rc::ptr_eq(&service.create_for_window(1), &first));
        assert!(Rc::ptr_eq(&service.create_for_window(2), &second));
        assert_eq!(service.items_created, 2);
    }

    #[test]
    fn a_dead_slot_is_reused_before_a_live_one_is_evicted() {
        let mut service = CaptureService::new();
        let held: Vec<_> = (0..LIVE_ITEMS_MAX as isize)
            .map(|window| service.create_for_window(window))
            .collect();
        drop(service.create_for_window(100));
        let newest = service.create_for_window(101);
        assert_eq!(service.occupied_count(), LIVE_ITEMS_MAX);
        assert!(Rc::ptr_eq(&service.create_for_window(101), &newest));
        for (window, item) in held.iter().enumerate().skip(1) {
            assert!(Rc::ptr_eq(
                &service.create_for_window(window as isize),
                item
            ));
        }
    }

    #[test]
    fn a_window_probed_every_frame_outlives_more_held_items_than_slots() {
        let mut service = CaptureService::new();
        drop(service.create_for_window(1));
        let editor_item = service.create_for_window(1);
        let mut slide_show_items = Vec::new();
        for slide_show in 0..(LIVE_ITEMS_MAX as isize * 3) {
            let slide_show_window = 100 + slide_show;
            drop(service.create_for_window(slide_show_window));
            let slide_show_item = service.create_for_window(slide_show_window);
            for _ in 0..30 {
                let probe = service.create_for_window(slide_show_window);
                assert!(Rc::ptr_eq(&probe, &slide_show_item));
            }
            slide_show_items.push(slide_show_item);
            let items_created = service.items_created;
            for _ in 0..30 {
                let probe = service.create_for_window(1);
                assert!(Rc::ptr_eq(&probe, &editor_item));
            }
            assert_eq!(service.items_created, items_created);
        }
        assert_eq!(service.items_created, 2 + 2 * LIVE_ITEMS_MAX as u32 * 3);
        assert_eq!(service.occupied_count(), LIVE_ITEMS_MAX);
    }

    #[test]
    fn the_least_recently_used_held_item_is_the_one_evicted() {
        for unused in 0..LIVE_ITEMS_MAX as isize {
            let mut service = CaptureService::new();
            let held: Vec<_> = (0..LIVE_ITEMS_MAX as isize)
                .map(|window| service.create_for_window(window))
                .collect();
            for window in (0..LIVE_ITEMS_MAX as isize).filter(|window| *window != unused) {
                drop(service.create_for_window(window));
            }
            let newest = service.create_for_window(100);
            assert!(service.live.lookup(unused).is_none());
            assert!(Rc::ptr_eq(&service.create_for_window(100), &newest));
            for window in (0..LIVE_ITEMS_MAX as isize).filter(|window| *window != unused) {
                assert!(Rc::ptr_eq(
                    &service.create_for_window(window),
                    &held[window as usize]
                ));
            }
        }
    }

    #[test]
    fn a_newly_held_item_is_evicted_after_the_older_ones() {
        let mut service = CaptureService::new();
        let mut held: Vec<_> = (0..LIVE_ITEMS_MAX as isize - 1)
            .map(|window| service.create_for_window(window))
            .collect();
        for window in 0..LIVE_ITEMS_MAX as isize - 1 {
            drop(service.create_for_window(window));
        }
        let newly_held_window = LIVE_ITEMS_MAX as isize - 1;
        held.push(service.create_for_window(newly_held_window));
        let over_the_bound = service.create_for_window(100);
        assert!(service.live.lookup(0).is_none());
        assert!(Rc::ptr_eq(
            &service.create_for_window(newly_held_window),
            &held[newly_held_window as usize]
        ));
        assert!(Rc::ptr_eq(&service.create_for_window(100), &over_the_bound));
    }

    #[test]
    fn random_share_sequences_match_a_reference_model() {
        for seed in 1..=32u64 {
            let mut state = seed.wrapping_mul(0x9E37_79B9_7F4A_7C15);
            let mut service = CaptureService::new();
            let mut held: [Option<Rc<isize>>; LIVE_ITEMS_MAX] = [const { None }; LIVE_ITEMS_MAX];
            let mut items_expected = 0;
            for _ in 0..5_000 {
                state ^= state << 13;
                state ^= state >> 7;
                state ^= state << 17;
                let window = (state % LIVE_ITEMS_MAX as u64) as usize;
                let action = (state >> 32) % 8;
                if action == 0 {
                    held[window] = None;
                } else if let Some(share_item) = &held[window] {
                    let probe = service.create_for_window(window as isize);
                    assert!(Rc::ptr_eq(&probe, share_item), "seed {seed}");
                } else {
                    items_expected += 1;
                    let item = service.create_for_window(window as isize);
                    assert_eq!(*item, window as isize, "seed {seed}");
                    if action == 1 {
                        held[window] = Some(item);
                    }
                }
                assert_eq!(service.items_created, items_expected, "seed {seed}");
                for share_item in held.iter().flatten() {
                    assert_eq!(Rc::strong_count(share_item), 1, "seed {seed}");
                }
            }
        }
    }

    #[test]
    fn the_table_never_grows_past_its_bound() {
        let mut service = CaptureService::new();
        let held: Vec<_> = (0..(LIVE_ITEMS_MAX as isize * 4))
            .map(|window| service.create_for_window(window))
            .collect();
        assert_eq!(service.occupied_count(), LIVE_ITEMS_MAX);
        drop(held);
        assert!(service.live.lookup(0).is_none());
    }
}
