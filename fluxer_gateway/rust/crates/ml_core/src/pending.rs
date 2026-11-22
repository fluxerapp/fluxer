// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeMap;

use crate::ids::ListKey;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Mark {
    Sync,
    Synced,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum TimerAction {
    KeepTimer,
    ArmTimer,
    CancelTimer,
}

#[derive(Clone, Debug, Default)]
pub struct Pending {
    marks: BTreeMap<ListKey, Mark>,
    armed: bool,
    started_armed: bool,
    fresh: bool,
    cancelled: bool,
}

impl Pending {
    pub fn new() -> Pending {
        Pending::default()
    }

    pub fn begin(&mut self) {
        self.started_armed = self.armed;
        self.fresh = false;
        self.cancelled = false;
    }

    pub fn finish(&self) -> TimerAction {
        if self.armed && self.fresh {
            TimerAction::ArmTimer
        } else if !self.armed && self.started_armed && self.cancelled {
            TimerAction::CancelTimer
        } else {
            TimerAction::KeepTimer
        }
    }

    pub fn is_armed(&self) -> bool {
        self.armed
    }

    pub fn marks(&self) -> impl Iterator<Item = (ListKey, Mark)> + '_ {
        self.marks.iter().map(|(k, m)| (*k, *m))
    }

    pub fn mark(&mut self, list: ListKey, mark: Mark) {
        if !self.armed {
            self.armed = true;
            self.fresh = true;
            self.marks.clear();
        }
        let entry = self.marks.entry(list).or_insert(mark);
        if *entry == Mark::Synced {
            *entry = mark;
        }
    }

    pub fn take_immediate(&mut self) -> Vec<ListKey> {
        if !self.armed {
            return Vec::new();
        }
        let lists = self
            .marks
            .iter()
            .filter(|(_, m)| **m == Mark::Sync)
            .map(|(k, _)| *k)
            .collect();
        self.marks.clear();
        self.armed = false;
        if self.fresh {
            self.fresh = false;
        } else {
            self.cancelled = true;
        }
        lists
    }

    pub fn take_flush(&mut self) -> Vec<ListKey> {
        let lists = self
            .marks
            .iter()
            .filter(|(_, m)| **m == Mark::Sync)
            .map(|(k, _)| *k)
            .collect();
        self.marks.clear();
        if self.armed {
            if self.fresh {
                self.fresh = false;
            } else {
                self.started_armed = false;
            }
        }
        self.armed = false;
        lists
    }

    pub fn invalidate(&mut self, mut is_row: impl FnMut(ListKey) -> bool) {
        for (list, mark) in self.marks.iter_mut() {
            if *mark == Mark::Synced && is_row(*list) {
                *mark = Mark::Sync;
            }
        }
    }
}
