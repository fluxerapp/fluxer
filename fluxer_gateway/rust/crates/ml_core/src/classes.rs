// SPDX-License-Identifier: AGPL-3.0-or-later

use core::cmp::Ordering;
use std::collections::BTreeMap;

use perm_core::Model;
use smallvec::SmallVec;

use crate::blocked::{ColId, Columns};
use crate::budget::Meter;
use crate::flags::Visible;
use crate::ids::{ClassId, RoleSetId, Slot};
use crate::members::Members;
use crate::sections::Sections;

#[derive(Clone, Debug)]
pub struct Column {
    class: ClassId,
    readers: u32,
    offline: Option<Vec<Slot>>,
    offline_count: u32,
}

impl Column {
    pub fn class(&self) -> ClassId {
        self.class
    }

    pub fn readers(&self) -> u32 {
        self.readers
    }

    pub fn offline(&self) -> Option<&[Slot]> {
        self.offline.as_deref()
    }

    pub fn offline_count(&self) -> u32 {
        self.offline_count
    }
}

#[derive(Clone, Debug, Default)]
pub struct ColumnTable {
    cols: Vec<Option<Column>>,
    by_class: BTreeMap<ClassId, ColId>,
    live: Vec<ColId>,
}

impl ColumnTable {
    pub fn new() -> ColumnTable {
        ColumnTable::default()
    }

    pub fn live(&self) -> &[ColId] {
        &self.live
    }

    pub fn len(&self) -> usize {
        self.live.len()
    }

    pub fn is_empty(&self) -> bool {
        self.live.is_empty()
    }

    pub fn of_class(&self, class: ClassId) -> Option<ColId> {
        self.by_class.get(&class).copied()
    }

    pub fn get(&self, col: ColId) -> Option<&Column> {
        self.cols.get(col.index()).and_then(Option::as_ref)
    }

    pub fn get_mut(&mut self, col: ColId) -> Option<&mut Column> {
        self.cols.get_mut(col.index()).and_then(Option::as_mut)
    }

    pub fn iter(&self) -> impl Iterator<Item = (ColId, &Column)> {
        self.live
            .iter()
            .filter_map(|c| self.get(*c).map(|col| (*c, col)))
    }

    pub fn class_of(&self, col: ColId) -> Option<ClassId> {
        self.get(col).map(|c| c.class)
    }

    pub fn create(&mut self, class: ClassId) -> ColId {
        if let Some(col) = self.of_class(class) {
            return col;
        }
        let free = self.cols.iter().position(Option::is_none);
        let index = free.unwrap_or(self.cols.len());
        let col = ColId::from_index(index).unwrap_or(ColId::new(u16::MAX));
        let column = Column {
            class,
            readers: 0,
            offline: None,
            offline_count: 0,
        };
        match self.cols.get_mut(index) {
            Some(cell) => *cell = Some(column),
            None => self.cols.push(Some(column)),
        }
        self.by_class.insert(class, col);
        self.live.push(col);
        self.live.sort();
        col
    }

    pub fn remove(&mut self, col: ColId) -> Option<Column> {
        let column = self.cols.get_mut(col.index()).and_then(Option::take)?;
        self.by_class.remove(&column.class);
        self.live.retain(|c| *c != col);
        Some(column)
    }

    pub fn set_readers(&mut self, col: ColId, readers: u32) {
        if let Some(column) = self.get_mut(col) {
            column.readers = readers;
        }
    }

    pub fn set_offline(&mut self, col: ColId, offline: Option<Vec<Slot>>) {
        if let Some(column) = self.get_mut(col) {
            column.offline = offline;
        }
    }

    pub fn set_offline_count(&mut self, col: ColId, count: u32) {
        if let Some(column) = self.get_mut(col) {
            column.offline_count = count;
        }
    }

    pub fn add_offline(&mut self, col: ColId) {
        if let Some(column) = self.get_mut(col) {
            column.offline_count = column.offline_count.saturating_add(1);
        }
    }

    pub fn sub_offline(&mut self, col: ColId) {
        if let Some(column) = self.get_mut(col) {
            column.offline_count = column.offline_count.saturating_sub(1);
        }
    }

    pub fn offline_mut(&mut self, col: ColId) -> Option<&mut Vec<Slot>> {
        self.get_mut(col).and_then(|c| c.offline.as_mut())
    }

    pub fn bytes(&self) -> usize {
        self.cols
            .iter()
            .flatten()
            .map(|c| {
                c.offline
                    .as_ref()
                    .map_or(0, |v| v.capacity().saturating_mul(4))
                    .saturating_add(48)
            })
            .fold(
                self.cols.capacity().saturating_mul(8),
                usize::saturating_add,
            )
    }
}

pub struct Vis<'a> {
    pub model: &'a Model,
    pub members: &'a Members,
    pub table: &'a ColumnTable,
}

impl Vis<'_> {
    pub fn class_sees(&self, class: ClassId, slot: Slot) -> bool {
        self.model
            .visible(class, self.members.roleset(slot), self.members.user(slot))
            .contains(Visible::VIEW)
    }
}

impl Columns for Vis<'_> {
    fn live(&self) -> &[ColId] {
        self.table.live()
    }

    fn visible(&self, col: ColId, slot: Slot) -> bool {
        self.table
            .class_of(col)
            .is_some_and(|class| self.class_sees(class, slot))
    }
}

#[derive(Clone, Debug, Default)]
pub struct SetCounts {
    total: Vec<u32>,
    online: Vec<u32>,
}

impl SetCounts {
    pub fn new() -> SetCounts {
        SetCounts::default()
    }

    fn grow(&mut self, set: RoleSetId) {
        let need = set.index().saturating_add(1);
        if self.total.len() < need {
            self.total.resize(need, 0);
            self.online.resize(need, 0);
        }
    }

    pub fn add(&mut self, set: RoleSetId, online: bool) {
        self.grow(set);
        if let Some(t) = self.total.get_mut(set.index()) {
            *t = t.saturating_add(1);
        }
        if online && let Some(o) = self.online.get_mut(set.index()) {
            *o = o.saturating_add(1);
        }
    }

    pub fn sub(&mut self, set: RoleSetId, online: bool) {
        if let Some(t) = self.total.get_mut(set.index()) {
            *t = t.saturating_sub(1);
        }
        if online && let Some(o) = self.online.get_mut(set.index()) {
            *o = o.saturating_sub(1);
        }
    }

    pub fn set_online(&mut self, set: RoleSetId, online: bool) {
        self.grow(set);
        if let Some(o) = self.online.get_mut(set.index()) {
            *o = if online {
                o.saturating_add(1)
            } else {
                o.saturating_sub(1)
            };
        }
    }

    pub fn total(&self, set: RoleSetId) -> u32 {
        self.total.get(set.index()).copied().unwrap_or(0)
    }

    pub fn online(&self, set: RoleSetId) -> u32 {
        self.online.get(set.index()).copied().unwrap_or(0)
    }

    pub fn live(&self) -> impl Iterator<Item = (RoleSetId, u32, u32)> + '_ {
        self.total
            .iter()
            .zip(self.online.iter())
            .enumerate()
            .filter(|(_, (t, _))| **t > 0)
            .filter_map(|(i, (t, o))| RoleSetId::from_index(i).map(|s| (s, *t, *o)))
    }

    pub fn live_sets(&self) -> u64 {
        self.total.iter().filter(|t| **t > 0).count() as u64
    }

    pub fn bytes(&self) -> usize {
        self.total
            .capacity()
            .saturating_add(self.online.capacity())
            .saturating_mul(4)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct ClassCounts {
    pub total: u32,
    pub online: u32,
    pub sections: SmallVec<[u32; 8]>,
}

impl ClassCounts {
    pub fn offline(&self) -> u32 {
        self.total.saturating_sub(self.online)
    }
}

pub fn class_counts(
    model: &Model,
    class: ClassId,
    counts: &SetCounts,
    members: &Members,
    sections: &Sections,
    meter: &mut Meter,
) -> ClassCounts {
    let mut out = ClassCounts {
        total: 0,
        online: 0,
        sections: SmallVec::from_elem(0, sections.len()),
    };
    let Some(entry) = model.class(class) else {
        return out;
    };
    let planes = entry.planes();
    for (set, total, online) in counts.live() {
        meter.charge(1);
        if !planes.get(set).contains(Visible::VIEW) {
            continue;
        }
        out.total = out.total.saturating_add(total);
        out.online = out.online.saturating_add(online);
        if online > 0
            && let Some(cell) = out.sections.get_mut(sections.known_section(set))
        {
            *cell = cell.saturating_add(online);
        }
    }
    for (user, visible) in entry.overrides() {
        meter.charge(2);
        let Some(slot) = members.find(*user) else {
            continue;
        };
        let set = members.roleset(slot);
        let plane = planes.get(set).contains(Visible::VIEW);
        let own = visible.contains(Visible::VIEW);
        let online = members.is_online(slot);
        let section = sections.known_section(set);
        match (plane, own) {
            (true, false) => {
                out.total = out.total.saturating_sub(1);
                if online {
                    out.online = out.online.saturating_sub(1);
                    if let Some(cell) = out.sections.get_mut(section) {
                        *cell = cell.saturating_sub(1);
                    }
                }
            }
            (false, true) => {
                out.total = out.total.saturating_add(1);
                if online {
                    out.online = out.online.saturating_add(1);
                    if let Some(cell) = out.sections.get_mut(section) {
                        *cell = cell.saturating_add(1);
                    }
                }
            }
            _ => {}
        }
    }
    out
}

pub fn offline_insert(vec: &mut Vec<Slot>, slot: Slot, members: &Members, meter: &mut Meter) {
    let at = vec.binary_search_by(|probe| {
        meter.charge(1);
        members.cmp(*probe, slot)
    });
    if let Err(at) = at {
        vec.insert(at, slot);
        meter.charge(vec.len() as u64 / 64);
    }
}

pub fn offline_remove(vec: &mut Vec<Slot>, slot: Slot, members: &Members, meter: &mut Meter) {
    let at = vec.binary_search_by(|probe| {
        meter.charge(1);
        members.cmp(*probe, slot)
    });
    if let Ok(at) = at {
        vec.remove(at);
        meter.charge(vec.len() as u64 / 64);
    }
}

pub fn build_offline(vis: &Vis<'_>, class: ClassId, meter: &mut Meter) -> Vec<Slot> {
    let mut out: Vec<(u64, Slot)> = vis
        .members
        .slots()
        .filter(|s| {
            meter.charge(1);
            !vis.members.is_online(*s) && vis.class_sees(class, *s)
        })
        .map(|s| vis.members.prefixed(s))
        .collect();
    meter.charge((out.len() as u64).saturating_mul(14));
    vis.members.sort_prefixed(&mut out);
    out.into_iter().map(|(_, s)| s).collect()
}

pub fn check_sorted(vec: &[Slot], members: &Members) -> bool {
    vec.windows(2).all(|w| match w {
        [a, b] => members.cmp(*a, *b) == Ordering::Less,
        _ => true,
    })
}
