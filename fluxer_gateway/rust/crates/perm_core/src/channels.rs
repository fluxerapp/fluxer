// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;

use hashbrown::HashTable;
use smallvec::SmallVec;

use crate::flags::{ChannelFlags, Permissions};
use crate::hashing::Seeded;
use crate::ids::{ChannelId, ChannelIdx, GuildId, RoleId, RoleIdx, UserId};
use crate::roles::RoleTable;
use crate::rolesets::DEAD;
use crate::stage::{Grant, Stage};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ChannelKind {
    Text,
    Voice,
    Category,
    Announcement,
    Forum,
    Media,
    Thread(u8),
    Other(u32),
}

impl ChannelKind {
    pub fn from_code(code: u32) -> ChannelKind {
        match code {
            0 => ChannelKind::Text,
            2 => ChannelKind::Voice,
            4 => ChannelKind::Category,
            5 => ChannelKind::Announcement,
            10 => ChannelKind::Thread(10),
            11 => ChannelKind::Thread(11),
            12 => ChannelKind::Thread(12),
            15 => ChannelKind::Forum,
            16 => ChannelKind::Media,
            other => ChannelKind::Other(other),
        }
    }

    pub fn code(self) -> u32 {
        match self {
            ChannelKind::Text => 0,
            ChannelKind::Voice => 2,
            ChannelKind::Category => 4,
            ChannelKind::Announcement => 5,
            ChannelKind::Forum => 15,
            ChannelKind::Media => 16,
            ChannelKind::Thread(code) => u32::from(code),
            ChannelKind::Other(code) => code,
        }
    }

    pub fn is_category(self) -> bool {
        self == ChannelKind::Category
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Nsfw {
    Inherit,
    Safe,
    Adult,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum OverwriteTarget {
    Role(RoleId),
    Member(UserId),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct OverwriteSpec {
    pub target: OverwriteTarget,
    pub allow: Permissions,
    pub deny: Permissions,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ChannelSpec {
    pub id: ChannelId,
    pub kind: ChannelKind,
    pub parent: Option<ChannelId>,
    pub overwrites: Vec<OverwriteSpec>,
    pub nsfw: Nsfw,
    pub named: bool,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub struct Rules {
    everyone: Stage,
    roles: SmallVec<[(RoleIdx, Grant); 4]>,
    members: SmallVec<[(UserId, Stage); 1]>,
}

impl Rules {
    pub fn everyone(&self) -> Stage {
        self.everyone
    }

    pub fn roles(&self) -> &[(RoleIdx, Grant)] {
        &self.roles
    }

    pub fn members(&self) -> &[(UserId, Stage)] {
        &self.members
    }

    pub fn grant_count(&self) -> u64 {
        u64::try_from(self.roles.len()).unwrap_or(u64::MAX)
    }

    pub fn member_stage(&self, user: UserId) -> Stage {
        self.members
            .binary_search_by_key(&user, |(u, _)| *u)
            .ok()
            .and_then(|i| self.members.get(i))
            .map_or(Stage::IDENTITY, |(_, stage)| *stage)
    }

    pub(crate) fn compile(
        guild: GuildId,
        overwrites: &[OverwriteSpec],
        roles: &mut RoleTable,
    ) -> Rules {
        let everyone_id = RoleId::new(guild.get());
        let mut everyone = Stage::IDENTITY;
        let mut grants: SmallVec<[(RoleIdx, Grant); 4]> = SmallVec::new();
        let mut members: SmallVec<[(UserId, Stage); 1]> = SmallVec::new();
        for overwrite in overwrites {
            match overwrite.target {
                OverwriteTarget::Role(id) => {
                    let grant = Grant::new(overwrite.allow, overwrite.deny);
                    if id == everyone_id {
                        everyone = everyone.then(grant.as_stage());
                    }
                    let idx = roles.intern(id);
                    match grants.binary_search_by_key(&idx, |(r, _)| *r) {
                        Ok(i) => {
                            if let Some(entry) = grants.get_mut(i) {
                                entry.1 = entry.1.merge(grant);
                            }
                        }
                        Err(i) => grants.insert(i, (idx, grant)),
                    }
                }
                OverwriteTarget::Member(user) => {
                    let stage = Stage::new(overwrite.allow, overwrite.deny);
                    match members.binary_search_by_key(&user, |(u, _)| *u) {
                        Ok(i) => {
                            if let Some(entry) = members.get_mut(i) {
                                entry.1 = entry.1.then(stage);
                            }
                        }
                        Err(i) => members.insert(i, (user, stage)),
                    }
                }
            }
        }
        Rules {
            everyone,
            roles: grants,
            members,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Channel {
    id: ChannelId,
    kind: ChannelKind,
    parent_id: Option<ChannelId>,
    parent: Option<ChannelIdx>,
    flags: ChannelFlags,
    nsfw: Nsfw,
    rules: Rules,
}

impl Channel {
    pub fn id(&self) -> ChannelId {
        self.id
    }

    pub fn kind(&self) -> ChannelKind {
        self.kind
    }

    pub fn parent_id(&self) -> Option<ChannelId> {
        self.parent_id
    }

    pub fn parent(&self) -> Option<ChannelIdx> {
        self.parent
    }

    pub fn flags(&self) -> ChannelFlags {
        self.flags
    }

    pub fn nsfw(&self) -> Nsfw {
        self.nsfw
    }

    pub fn rules(&self) -> &Rules {
        &self.rules
    }

    pub fn is_named(&self) -> bool {
        self.flags.contains(ChannelFlags::NAMED)
    }

    pub fn denies_view(&self) -> bool {
        self.flags.contains(ChannelFlags::DENIES_VIEW)
    }

    pub fn nsfw_override(&self) -> Option<bool> {
        match self.nsfw {
            Nsfw::Inherit => None,
            Nsfw::Safe => Some(false),
            Nsfw::Adult => Some(true),
        }
    }

    pub(crate) fn compile(guild: GuildId, spec: &ChannelSpec, roles: &mut RoleTable) -> Channel {
        let mut flags = ChannelFlags::empty();
        flags.set(ChannelFlags::NAMED, spec.named);
        flags.set(
            ChannelFlags::DENIES_VIEW,
            spec.overwrites
                .iter()
                .any(|o| o.deny.contains(Permissions::VIEW_CHANNEL)),
        );
        Channel {
            id: spec.id,
            kind: spec.kind,
            parent_id: spec.parent,
            parent: None,
            flags,
            nsfw: spec.nsfw,
            rules: Rules::compile(guild, &spec.overwrites, roles),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ChannelTable {
    slots: Vec<Option<Channel>>,
    order: Vec<ChannelIdx>,
    index: HashTable<ChannelIdx>,
    children: Vec<SmallVec<[ChannelIdx; 8]>>,
    irregular: bool,
    seeded: Seeded,
    generation: u32,
    remap: Option<Arc<[u32]>>,
}

impl PartialEq for ChannelTable {
    fn eq(&self, other: &Self) -> bool {
        self.slots == other.slots && self.order == other.order
    }
}

impl Eq for ChannelTable {}

impl ChannelTable {
    pub fn slots(&self) -> usize {
        self.slots.len()
    }

    pub fn len(&self) -> usize {
        self.order.len()
    }

    pub fn is_empty(&self) -> bool {
        self.order.is_empty()
    }

    pub fn get(&self, idx: ChannelIdx) -> Option<&Channel> {
        self.slots.get(idx.index()).and_then(Option::as_ref)
    }

    pub fn resolve(&self, id: ChannelId) -> Option<ChannelIdx> {
        let hash = self.seeded.hash(&id);
        let slots = &self.slots;
        self.index
            .find(hash, |idx| {
                slots
                    .get(idx.index())
                    .and_then(Option::as_ref)
                    .is_some_and(|c| c.id == id)
            })
            .copied()
    }

    pub fn by_id(&self, id: ChannelId) -> Option<&Channel> {
        self.resolve(id).and_then(|idx| self.get(idx))
    }

    pub fn order(&self) -> &[ChannelIdx] {
        &self.order
    }

    pub fn in_order(&self) -> impl Iterator<Item = (ChannelIdx, &Channel)> {
        self.order
            .iter()
            .filter_map(|idx| self.get(*idx).map(|c| (*idx, c)))
    }

    pub fn live(&self) -> impl Iterator<Item = (ChannelIdx, &Channel)> {
        self.slots.iter().enumerate().filter_map(|(i, slot)| {
            let channel = slot.as_ref()?;
            ChannelIdx::from_index(i).map(|idx| (idx, channel))
        })
    }

    pub fn children(&self, idx: ChannelIdx) -> &[ChannelIdx] {
        self.children.get(idx.index()).map_or(&[], |c| c.as_slice())
    }

    pub fn has_irregular_parents(&self) -> bool {
        self.irregular
    }

    pub fn dead_slots(&self) -> usize {
        self.slots.len().saturating_sub(self.order.len())
    }

    pub fn generation(&self) -> u32 {
        self.generation
    }

    pub fn remap(&self) -> Option<&Arc<[u32]>> {
        self.remap.as_ref()
    }

    pub(crate) fn compacted(&self) -> ChannelTable {
        let mut remap = vec![DEAD; self.slots.len()];
        let mut slots: Vec<Option<Channel>> = Vec::with_capacity(self.order.len());
        for (i, slot) in self.slots.iter().enumerate() {
            let Some(channel) = slot else { continue };
            if let (Some(entry), Ok(next)) = (remap.get_mut(i), u32::try_from(slots.len())) {
                *entry = next;
            }
            slots.push(Some(channel.clone()));
        }
        let map = |idx: ChannelIdx| {
            remap
                .get(idx.index())
                .copied()
                .filter(|n| *n != DEAD)
                .map(ChannelIdx::new)
        };
        for channel in slots.iter_mut().flatten() {
            channel.parent = channel.parent.and_then(map);
        }
        let order: Vec<ChannelIdx> = self.order.iter().filter_map(|idx| map(*idx)).collect();
        let mut children: Vec<SmallVec<[ChannelIdx; 8]>> = vec![SmallVec::new(); slots.len()];
        for (i, list) in self.children.iter().enumerate() {
            let Some(at) = ChannelIdx::from_index(i).and_then(map) else {
                continue;
            };
            if let Some(entry) = children.get_mut(at.index()) {
                *entry = list.iter().filter_map(|c| map(*c)).collect();
            }
        }
        let seeded = self.seeded.clone();
        let mut index = HashTable::new();
        for (i, slot) in slots.iter().enumerate() {
            if let (Some(channel), Some(idx)) = (slot, ChannelIdx::from_index(i)) {
                index.insert_unique(seeded.hash(&channel.id), idx, |j: &ChannelIdx| {
                    slots
                        .get(j.index())
                        .and_then(Option::as_ref)
                        .map_or(0, |c| seeded.hash(&c.id))
                });
            }
        }
        ChannelTable {
            slots,
            order,
            index,
            children,
            irregular: self.irregular,
            seeded,
            generation: self.generation.wrapping_add(1),
            remap: Some(remap.into()),
        }
    }

    pub(crate) fn upsert(&mut self, mut channel: Channel) -> ChannelIdx {
        if let Some(idx) = self.resolve(channel.id) {
            if let Some(slot) = self.slots.get_mut(idx.index()) {
                if let Some(old) = slot.as_ref()
                    && old.parent_id == channel.parent_id
                {
                    channel.parent = old.parent;
                }
                *slot = Some(channel);
            }
            return idx;
        }
        let Some(idx) = ChannelIdx::from_index(self.slots.len()) else {
            return ChannelIdx::new(u32::MAX);
        };
        let hash = self.seeded.hash(&channel.id);
        self.slots.push(Some(channel));
        let seeded = &self.seeded;
        let slots = &self.slots;
        self.index.insert_unique(hash, idx, |i| {
            slots
                .get(i.index())
                .and_then(Option::as_ref)
                .map_or(0, |c| seeded.hash(&c.id))
        });
        self.order.push(idx);
        idx
    }

    pub(crate) fn remove(&mut self, id: ChannelId) -> Option<ChannelIdx> {
        let idx = self.resolve(id)?;
        let hash = self.seeded.hash(&id);
        if let Ok(entry) = self.index.find_entry(hash, |i| *i == idx) {
            entry.remove();
        }
        if let Some(slot) = self.slots.get_mut(idx.index()) {
            *slot = None;
        }
        self.order.retain(|i| *i != idx);
        Some(idx)
    }

    pub(crate) fn reorder(&mut self, ids: &[ChannelId]) {
        let mut seen = vec![false; self.slots.len()];
        let mut order = Vec::with_capacity(self.order.len());
        for id in ids {
            if let Some(idx) = self.resolve(*id)
                && let Some(flag) = seen.get_mut(idx.index())
                && !*flag
            {
                *flag = true;
                order.push(idx);
            }
        }
        for idx in &self.order {
            if let Some(flag) = seen.get_mut(idx.index())
                && !*flag
            {
                *flag = true;
                order.push(*idx);
            }
        }
        self.order = order;
    }

    pub(crate) fn relink(&mut self) {
        let parents: Vec<Option<ChannelIdx>> = self
            .slots
            .iter()
            .map(|slot| {
                slot.as_ref()
                    .and_then(|c| c.parent_id)
                    .and_then(|pid| self.resolve(pid))
            })
            .collect();
        let mut children: Vec<SmallVec<[ChannelIdx; 8]>> = vec![SmallVec::new(); self.slots.len()];
        for idx in &self.order {
            if let Some(Some(parent)) = parents.get(idx.index())
                && let Some(list) = children.get_mut(parent.index())
            {
                list.push(*idx);
            }
        }
        let mut irregular = false;
        for (slot, parent) in self.slots.iter_mut().zip(&parents) {
            if let Some(channel) = slot {
                channel.parent = *parent;
                if parent.is_some() && channel.kind.is_category() {
                    irregular = true;
                }
            }
        }
        for (slot, parent) in self.slots.iter().zip(&parents) {
            if slot.is_some()
                && let Some(parent) = parent
                && self
                    .slots
                    .get(parent.index())
                    .and_then(Option::as_ref)
                    .is_some_and(|p| !p.kind.is_category())
            {
                irregular = true;
            }
        }
        self.children = children;
        self.irregular = irregular;
    }
}
