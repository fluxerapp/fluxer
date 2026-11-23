// SPDX-License-Identifier: AGPL-3.0-or-later

use core::cmp::Ordering;

use perm_core::{Hoist, Model, RankPos};
use std::collections::BTreeMap;

use crate::blocked::{BlockedList, Columns};
use crate::budget::Meter;
use crate::ids::{RoleId, RoleIdx, RoleSetId, Slot};
use crate::members::Members;

pub const SINGLE_MOVE_LIMIT: usize = 256;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum SectionKey {
    Hoisted(RoleIdx),
    Online,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Label {
    Role(RoleId),
    Online,
    Offline,
}

pub fn effective_position(model: &Model, role: RoleIdx) -> Option<(RankPos, u64)> {
    let entry = model.roles().get(role)?;
    if !entry.is_present() || entry.id().get() == model.guild().get() || entry.id().get() == 0 {
        return None;
    }
    match entry.hoist() {
        Hoist::NotHoisted => None,
        Hoist::Hoisted(Some(at)) => Some((RankPos::Int(at), entry.id().get())),
        Hoist::Hoisted(None) => Some((entry.position(), entry.id().get())),
    }
}

pub fn hoist_order(model: &Model) -> Vec<RoleIdx> {
    let mut hoisted: Vec<(RoleIdx, RankPos, u64)> = model
        .roles()
        .present_in_order()
        .iter()
        .filter_map(|idx| effective_position(model, *idx).map(|(pos, id)| (*idx, pos, id)))
        .collect();
    hoisted.sort_by(|a, b| b.1.cmp(&a.1).then(a.2.cmp(&b.2)));
    hoisted.into_iter().map(|(idx, _, _)| idx).collect()
}

#[derive(Debug, Default)]
pub struct Sections {
    order: Vec<SectionKey>,
    labels: Vec<Label>,
    lists: Vec<BlockedList>,
    rank: BTreeMap<RoleIdx, usize>,
    top: Vec<Option<usize>>,
}

impl Sections {
    pub fn new() -> Sections {
        let mut sections = Sections::default();
        sections.order.push(SectionKey::Online);
        sections.labels.push(Label::Online);
        sections.lists.push(BlockedList::new());
        sections
    }

    pub fn len(&self) -> usize {
        self.order.len()
    }

    pub fn is_empty(&self) -> bool {
        self.order.is_empty()
    }

    pub fn labels(&self) -> &[Label] {
        &self.labels
    }

    pub fn list(&self, section: usize) -> Option<&BlockedList> {
        self.lists.get(section)
    }

    pub fn lists(&self) -> &[BlockedList] {
        &self.lists
    }

    pub fn lists_mut(&mut self) -> &mut [BlockedList] {
        &mut self.lists
    }

    pub fn online_index(&self) -> usize {
        self.order.len().saturating_sub(1)
    }

    pub fn hoisted(&self) -> Vec<RoleIdx> {
        self.order
            .iter()
            .filter_map(|k| match k {
                SectionKey::Hoisted(r) => Some(*r),
                SectionKey::Online => None,
            })
            .collect()
    }

    fn compute_top(&self, model: &Model, set: RoleSetId) -> Option<usize> {
        model
            .rolesets()
            .get(set)?
            .roles()
            .iter()
            .filter_map(|r| self.rank.get(r).copied())
            .min()
    }

    pub fn section_of_set(&mut self, model: &Model, set: RoleSetId) -> usize {
        let i = set.index();
        if self.top.len() <= i {
            self.top.resize(i.saturating_add(1), None);
            let top = self.compute_top(model, set);
            if let Some(cell) = self.top.get_mut(i) {
                *cell = Some(top.unwrap_or(usize::MAX));
            }
        } else if self.top.get(i).copied().flatten().is_none() {
            let top = self.compute_top(model, set);
            if let Some(cell) = self.top.get_mut(i) {
                *cell = Some(top.unwrap_or(usize::MAX));
            }
        }
        match self.top.get(i).copied().flatten() {
            Some(usize::MAX) | None => self.online_index(),
            Some(at) => at,
        }
    }

    pub fn known_section(&self, set: RoleSetId) -> usize {
        match self.top.get(set.index()).copied().flatten() {
            Some(usize::MAX) | None => self.online_index(),
            Some(at) => at,
        }
    }

    pub fn set_labels(&mut self, model: &Model) {
        self.labels = self
            .order
            .iter()
            .map(|k| match k {
                SectionKey::Hoisted(r) => model
                    .roles()
                    .get(*r)
                    .map_or(Label::Online, |role| Label::Role(role.id())),
                SectionKey::Online => Label::Online,
            })
            .collect();
    }

    pub fn reset_order(&mut self, model: &Model, hoisted: &[RoleIdx]) {
        self.order = hoisted
            .iter()
            .map(|r| SectionKey::Hoisted(*r))
            .chain(core::iter::once(SectionKey::Online))
            .collect();
        self.rank = hoisted.iter().enumerate().map(|(i, r)| (*r, i)).collect();
        self.top.clear();
        self.lists = self.order.iter().map(|_| BlockedList::new()).collect();
        self.set_labels(model);
    }

    pub fn rehoist(
        &mut self,
        model: &Model,
        hoisted: &[RoleIdx],
        members: &Members,
        columns: &dyn Columns,
        meter: &mut Meter,
    ) -> usize {
        let old_order = core::mem::take(&mut self.order);
        let mut old_lists = core::mem::take(&mut self.lists);
        self.order = hoisted
            .iter()
            .map(|r| SectionKey::Hoisted(*r))
            .chain(core::iter::once(SectionKey::Online))
            .collect();
        self.rank = hoisted.iter().enumerate().map(|(i, r)| (*r, i)).collect();
        self.top.clear();
        self.set_labels(model);
        let mut moves: Vec<(usize, usize, Slot)> = Vec::new();
        for (from, list) in old_lists.iter().enumerate() {
            let key = old_order.get(from).copied().unwrap_or(SectionKey::Online);
            let kept = self.order.iter().position(|k| *k == key);
            for slot in list.slots() {
                meter.charge(1);
                let to = self.section_of_set(model, members.roleset(slot));
                if kept != Some(to) {
                    moves.push((from, to, slot));
                }
            }
        }
        let moved = moves.len();
        let mut lists: Vec<BlockedList> = Vec::with_capacity(self.order.len());
        for key in &self.order {
            let reuse = old_order
                .iter()
                .position(|k| k == key)
                .and_then(|i| old_lists.get_mut(i))
                .map(core::mem::take);
            lists.push(reuse.unwrap_or_default());
        }
        for list in &mut lists {
            for col in columns.live() {
                if !list.has_column(*col) {
                    list.fill_column(*col, columns, meter);
                }
            }
        }
        if moved < SINGLE_MOVE_LIMIT {
            self.lists = lists;
            for (from, to, slot) in moves {
                let still = old_order
                    .get(from)
                    .and_then(|k| self.order.iter().position(|n| n == k));
                if let Some(at) = still
                    && let Some(list) = self.lists.get_mut(at)
                {
                    list.remove(slot, members, columns, meter);
                }
                if let Some(list) = self.lists.get_mut(to) {
                    list.insert(slot, members, columns, meter);
                }
            }
            return moved;
        }
        let mut leaving: Vec<Vec<Slot>> = vec![Vec::new(); self.order.len()];
        let mut arriving: Vec<Vec<Vec<Slot>>> = vec![Vec::new(); self.order.len()];
        let mut run_from: Vec<Option<usize>> = vec![None; self.order.len()];
        for (from, to, slot) in &moves {
            let still = old_order
                .get(*from)
                .and_then(|k| self.order.iter().position(|n| n == k));
            if let Some(at) = still
                && let Some(out) = leaving.get_mut(at)
            {
                out.push(*slot);
            }
            if let (Some(runs), Some(last)) = (arriving.get_mut(*to), run_from.get_mut(*to)) {
                if *last != Some(*from) || runs.is_empty() {
                    runs.push(Vec::new());
                    *last = Some(*from);
                }
                if let Some(run) = runs.last_mut() {
                    run.push(*slot);
                }
            }
        }
        for (at, list) in lists.iter_mut().enumerate() {
            let out = leaving.get(at).map(Vec::as_slice).unwrap_or_default();
            let runs = arriving
                .get_mut(at)
                .map(core::mem::take)
                .unwrap_or_default();
            if out.is_empty() && runs.is_empty() {
                continue;
            }
            let staying: Vec<Slot> = if out.is_empty() {
                list.slots().collect()
            } else {
                let mut out_iter = out.iter().peekable();
                list.slots()
                    .filter(|s| {
                        if out_iter.peek() == Some(&s) {
                            out_iter.next();
                            false
                        } else {
                            true
                        }
                    })
                    .collect()
            };
            let mut merged = staying;
            for run in runs {
                meter.charge(merged.len().saturating_add(run.len()) as u64);
                merged = merge(&merged, &run, members);
            }
            list.rebuild(&merged, members, columns, meter);
        }
        self.lists = lists;
        moved
    }

    pub fn bytes(&self) -> usize {
        self.lists.iter().map(BlockedList::bytes).fold(
            self.top.capacity().saturating_mul(16),
            usize::saturating_add,
        )
    }
}

pub fn merge(a: &[Slot], b: &[Slot], members: &Members) -> Vec<Slot> {
    let mut out = Vec::with_capacity(a.len().saturating_add(b.len()));
    let mut x = a.iter().peekable();
    let mut y = b.iter().peekable();
    loop {
        match (x.peek(), y.peek()) {
            (Some(p), Some(q)) => {
                if members.cmp(**p, **q) == Ordering::Greater {
                    out.push(**q);
                    y.next();
                } else {
                    out.push(**p);
                    x.next();
                }
            }
            (Some(p), None) => {
                out.push(**p);
                x.next();
            }
            (None, Some(q)) => {
                out.push(**q);
                y.next();
            }
            (None, None) => return out,
        }
    }
}
