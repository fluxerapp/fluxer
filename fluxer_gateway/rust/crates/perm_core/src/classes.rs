// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, HashMap};

use hashbrown::HashTable;
use smallvec::SmallVec;

use crate::budget::Meter;
use crate::flags::Visible;
use crate::hashing::Seeded;
use crate::ids::{ChannelIdx, ClassId, RoleSetId, UserId};
use crate::rolesets::DEAD;
use crate::signatures::Planes;

pub(crate) type Overrides = SmallVec<[(UserId, Visible); 4]>;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) struct Content {
    pub(crate) planes: Planes,
    pub(crate) overrides: Overrides,
}

impl Content {
    fn same_as(&self, planes: &Planes, overrides: &[(UserId, Visible)]) -> bool {
        self.overrides.as_slice() == overrides && self.planes.same_as(planes)
    }

    fn reuse_cost(&self) -> u64 {
        u64::try_from(self.overrides.len())
            .unwrap_or(u64::MAX)
            .saturating_add(4)
    }

    fn cost(&self) -> u64 {
        let words = u64::try_from(self.planes.words()).unwrap_or(u64::MAX);
        let users = u64::try_from(self.overrides.len()).unwrap_or(u64::MAX);
        words
            .saturating_mul(3)
            .saturating_add(users)
            .saturating_add(4)
    }
}

fn content_hash(seeded: &Seeded, planes: u64, overrides: &[(UserId, Visible)]) -> u64 {
    seeded.hash(&(planes, overrides))
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Class {
    planes: Planes,
    overrides: Overrides,
    channels: SmallVec<[ChannelIdx; 2]>,
    hash: u64,
}

impl Class {
    pub fn planes(&self) -> &Planes {
        &self.planes
    }

    pub fn overrides(&self) -> &[(UserId, Visible)] {
        &self.overrides
    }

    pub fn channels(&self) -> &[ChannelIdx] {
        &self.channels
    }

    pub fn override_of(&self, user: UserId) -> Option<Visible> {
        if self.overrides.is_empty() {
            return None;
        }
        self.overrides
            .binary_search_by_key(&user, |(u, _)| *u)
            .ok()
            .and_then(|i| self.overrides.get(i))
            .map(|(_, v)| *v)
    }

    pub fn visible(&self, set: RoleSetId, user: UserId) -> Visible {
        match self.override_of(user) {
            Some(visible) => visible,
            None => self.planes.get(set),
        }
    }

    pub fn same_content(&self, other: &Class) -> bool {
        self.hash == other.hash
            && self.overrides == other.overrides
            && self.planes.same_as(&other.planes)
    }

    #[cfg(feature = "reference")]
    pub(crate) fn content(&self) -> Content {
        Content {
            planes: self.planes.clone(),
            overrides: self.overrides.clone(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Classes {
    list: Vec<Option<Class>>,
    lookup: HashTable<ClassId>,
    by_channel: Vec<Option<ClassId>>,
    seeded: Seeded,
}

impl PartialEq for Classes {
    fn eq(&self, other: &Classes) -> bool {
        self.list == other.list && self.by_channel == other.by_channel
    }
}

impl Eq for Classes {}

struct Group {
    content: Content,
    hash: u64,
    channels: SmallVec<[ChannelIdx; 2]>,
}

impl Classes {
    pub fn len(&self) -> usize {
        self.list.iter().filter(|c| c.is_some()).count()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn capacity(&self) -> usize {
        self.list.len()
    }

    pub fn get(&self, id: ClassId) -> Option<&Class> {
        self.list.get(id.index()).and_then(Option::as_ref)
    }

    pub fn of_channel(&self, channel: ChannelIdx) -> Option<ClassId> {
        self.by_channel.get(channel.index()).copied().flatten()
    }

    pub fn class_of_channel(&self, channel: ChannelIdx) -> Option<&Class> {
        self.of_channel(channel).and_then(|id| self.get(id))
    }

    pub fn iter(&self) -> impl Iterator<Item = (ClassId, &Class)> {
        self.list.iter().enumerate().filter_map(|(i, c)| {
            let class = c.as_ref()?;
            ClassId::from_index(i).map(|id| (id, class))
        })
    }

    #[cfg(feature = "reference")]
    pub(crate) fn content_of(&self, channel: ChannelIdx) -> Option<Content> {
        self.class_of_channel(channel).map(Class::content)
    }

    fn find(&self, hash: u64, content: &Content) -> Option<ClassId> {
        let list = &self.list;
        self.lookup
            .find(hash, |id| {
                list.get(id.index())
                    .and_then(Option::as_ref)
                    .is_some_and(|c| c.hash == hash && content.same_as(&c.planes, &c.overrides))
            })
            .copied()
    }

    fn group(
        seeded: &Seeded,
        contents: impl IntoIterator<Item = (ChannelIdx, Content)>,
        meter: &mut Meter,
    ) -> Vec<Group> {
        let mut groups: Vec<Group> = Vec::new();
        let mut group_planes: Vec<usize> = Vec::new();
        let mut grouping: HashTable<usize> = HashTable::new();
        let mut known: HashMap<[*const u64; 3], (usize, u64)> = HashMap::new();
        let mut distinct: Vec<Planes> = Vec::new();
        let mut by_hash: HashTable<usize> = HashTable::new();
        for (channel, content) in contents {
            meter.charge(content.reuse_cost());
            let identity = content.planes.identity();
            let (planes_id, planes_hash) = match known.get(&identity) {
                Some(seen) => *seen,
                None => {
                    let words = u64::try_from(content.planes.words())
                        .unwrap_or(u64::MAX)
                        .saturating_mul(3);
                    meter.charge(words);
                    let hash = seeded.hash(&content.planes);
                    let found = by_hash
                        .find(hash, |i| {
                            meter.charge(words);
                            distinct.get(*i).is_some_and(|p| p.same_as(&content.planes))
                        })
                        .copied();
                    let planes_id = found.unwrap_or_else(|| {
                        let position = distinct.len();
                        distinct.push(content.planes.clone());
                        by_hash.insert_unique(hash, position, |i| {
                            distinct.get(*i).map_or(0, |p| seeded.hash(p))
                        });
                        position
                    });
                    known.insert(identity, (planes_id, hash));
                    (planes_id, hash)
                }
            };
            let hash = content_hash(seeded, planes_hash, &content.overrides);
            let found = grouping
                .find(hash, |g| {
                    group_planes.get(*g) == Some(&planes_id)
                        && groups
                            .get(*g)
                            .is_some_and(|x| x.content.overrides == content.overrides)
                })
                .copied();
            match found.and_then(|g| groups.get_mut(g)) {
                Some(group) => group.channels.push(channel),
                None => {
                    let position = groups.len();
                    groups.push(Group {
                        content,
                        hash,
                        channels: SmallVec::from_slice(&[channel]),
                    });
                    group_planes.push(planes_id);
                    grouping
                        .insert_unique(hash, position, |g| groups.get(*g).map_or(0, |x| x.hash));
                }
            }
        }
        groups
    }

    #[cfg(feature = "reference")]
    pub(crate) fn assign(
        prev: &Classes,
        contents: Vec<(ChannelIdx, Content)>,
        slots: usize,
        meter: &mut Meter,
    ) -> Classes {
        let remaining = vec![0u32; prev.list.len()];
        let groups = Classes::group(&prev.seeded, contents, meter);
        let ids = prev.choose_ids(&groups, &remaining);
        let mut next = Classes {
            list: Vec::new(),
            lookup: HashTable::new(),
            by_channel: vec![None; slots],
            seeded: prev.seeded.clone(),
        };
        for (id, group) in ids.into_iter().zip(groups) {
            next.place(id, group);
        }
        next.rebuild_lookup();
        next
    }

    fn choose_ids(&self, groups: &[Group], remaining: &[u32]) -> Vec<ClassId> {
        let mut taken: Vec<bool> = remaining.iter().map(|r| *r > 0).collect();
        let mut ids: Vec<Option<ClassId>> = vec![None; groups.len()];
        for (slot, group) in ids.iter_mut().zip(groups) {
            if let Some(id) = self.find(group.hash, &group.content) {
                if let Some(flag) = taken.get_mut(id.index()) {
                    *flag = true;
                }
                *slot = Some(id);
            }
        }
        for (slot, group) in ids.iter_mut().zip(groups) {
            if slot.is_some() {
                continue;
            }
            let mut origin = group.channels.iter().map(|c| self.of_channel(*c));
            let Some(Some(first)) = origin.next() else {
                continue;
            };
            if origin.all(|o| o == Some(first))
                && let Some(flag) = taken.get_mut(first.index())
                && !*flag
            {
                *flag = true;
                *slot = Some(first);
            }
        }
        let mut next_free = 0usize;
        ids.into_iter()
            .map(|slot| match slot {
                Some(id) => id,
                None => {
                    while taken.get(next_free).copied().unwrap_or(false) {
                        next_free = next_free.saturating_add(1);
                    }
                    if taken.len() <= next_free {
                        taken.resize(next_free.saturating_add(1), false);
                    }
                    if let Some(flag) = taken.get_mut(next_free) {
                        *flag = true;
                    }
                    ClassId::from_index(next_free).unwrap_or(ClassId::new(u32::MAX))
                }
            })
            .collect()
    }

    fn place(&mut self, id: ClassId, group: Group) {
        if self.list.len() <= id.index() {
            self.list.resize(id.index().saturating_add(1), None);
        }
        for channel in &group.channels {
            if self.by_channel.len() <= channel.index() {
                self.by_channel
                    .resize(channel.index().saturating_add(1), None);
            }
            if let Some(entry) = self.by_channel.get_mut(channel.index()) {
                *entry = Some(id);
            }
        }
        if let Some(slot) = self.list.get_mut(id.index()) {
            match slot {
                Some(class) => {
                    class.channels.extend(group.channels);
                    class.channels.sort_unstable();
                }
                None => {
                    let mut channels = group.channels;
                    channels.sort_unstable();
                    *slot = Some(Class {
                        planes: group.content.planes,
                        overrides: group.content.overrides,
                        channels,
                        hash: group.hash,
                    });
                }
            }
        }
    }

    fn rebuild_lookup(&mut self) {
        while self.list.last().is_some_and(Option::is_none) {
            self.list.pop();
        }
        let mut lookup = HashTable::new();
        for (i, class) in self.list.iter().enumerate() {
            if let (Some(class), Some(id)) = (class, ClassId::from_index(i)) {
                let list = &self.list;
                lookup.insert_unique(class.hash, id, |j: &ClassId| {
                    list.get(j.index())
                        .and_then(Option::as_ref)
                        .map_or(0, |c| c.hash)
                });
            }
        }
        self.lookup = lookup;
    }

    pub(crate) fn reassign(
        prev: &Classes,
        changes: BTreeMap<ChannelIdx, Option<Content>>,
        slots: usize,
        meter: &mut Meter,
    ) -> Classes {
        meter.charge(u64::try_from(prev.list.len().saturating_add(slots)).unwrap_or(u64::MAX));
        let mut next = Classes {
            list: prev.list.clone(),
            lookup: HashTable::new(),
            by_channel: prev.by_channel.clone(),
            seeded: prev.seeded.clone(),
        };
        next.by_channel
            .resize(slots.max(next.by_channel.len()), None);
        let mut remaining: Vec<u32> = next
            .list
            .iter()
            .map(|c| {
                c.as_ref()
                    .map_or(0, |c| u32::try_from(c.channels.len()).unwrap_or(u32::MAX))
            })
            .collect();
        for channel in changes.keys() {
            if let Some(entry) = next.by_channel.get_mut(channel.index())
                && let Some(old) = entry.take()
            {
                if let Some(count) = remaining.get_mut(old.index()) {
                    *count = count.saturating_sub(1);
                }
                if let Some(Some(class)) = next.list.get_mut(old.index()) {
                    class.channels.retain(|c| c != channel);
                }
            }
        }
        let live = changes
            .into_iter()
            .filter_map(|(channel, content)| content.map(|c| (channel, c)));
        let groups = Classes::group(&prev.seeded, live, meter);
        let ids = prev.choose_ids(&groups, &remaining);
        for (i, slot) in next.list.iter_mut().enumerate() {
            if remaining.get(i).copied().unwrap_or(0) == 0 {
                *slot = None;
            }
        }
        for (id, group) in ids.into_iter().zip(groups) {
            next.place(id, group);
        }
        next.rebuild_lookup();
        next
    }

    pub(crate) fn remapped(&self, remap: &[u32], words: usize, meter: &mut Meter) -> Classes {
        let mut kept: Vec<(ClassId, Group)> = Vec::new();
        let mut grouping: HashTable<usize> = HashTable::new();
        for (id, class) in self.iter() {
            let content = Content {
                planes: class.planes.remapped(remap, words),
                overrides: class.overrides.clone(),
            };
            meter.charge(content.cost().saturating_mul(16));
            let hash = content_hash(
                &self.seeded,
                self.seeded.hash(&content.planes),
                &content.overrides,
            );
            let found = grouping
                .find(hash, |g| {
                    kept.get(*g).is_some_and(|(_, x)| {
                        x.hash == hash && x.content.same_as(&content.planes, &content.overrides)
                    })
                })
                .copied();
            match found.and_then(|g| kept.get_mut(g)) {
                Some((_, group)) => group.channels.extend(class.channels.iter().copied()),
                None => {
                    let position = kept.len();
                    kept.push((
                        id,
                        Group {
                            content,
                            hash,
                            channels: class.channels.clone(),
                        },
                    ));
                    grouping
                        .insert_unique(hash, position, |g| kept.get(*g).map_or(0, |x| x.1.hash));
                }
            }
        }
        let mut next = Classes {
            list: Vec::new(),
            lookup: HashTable::new(),
            by_channel: vec![None; self.by_channel.len()],
            seeded: self.seeded.clone(),
        };
        for (id, group) in kept {
            next.place(id, group);
        }
        next.rebuild_lookup();
        next
    }

    pub(crate) fn channels_remapped(&mut self, remap: &[u32], slots: usize) {
        let map = |idx: &ChannelIdx| {
            remap
                .get(idx.index())
                .copied()
                .filter(|n| *n != DEAD)
                .map(ChannelIdx::new)
        };
        for class in self.list.iter_mut().flatten() {
            class.channels = class.channels.iter().filter_map(map).collect();
        }
        let mut by_channel = vec![None; slots];
        for (old, class) in self.by_channel.iter().enumerate() {
            if let Some(new) = ChannelIdx::from_index(old).as_ref().and_then(map)
                && let Some(entry) = by_channel.get_mut(new.index())
            {
                *entry = *class;
            }
        }
        self.by_channel = by_channel;
    }
}
