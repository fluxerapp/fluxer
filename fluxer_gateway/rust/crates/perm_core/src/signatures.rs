// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeSet;
use std::sync::Arc;

use hashbrown::HashTable;
use smallvec::SmallVec;

use crate::bits;
use crate::budget::Meter;
use crate::channels::Rules;
use crate::eval;
use crate::flags::{Permissions, Visible};
use crate::hashing::Seeded;
use crate::ids::{ChannelIdx, RoleIdx, RoleSetId, SigId};
use crate::rolesets::{Base, DEAD, RoleSets};
use crate::stage::{Grant, Stage};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Planes {
    view: Arc<[u64]>,
    view_members: Arc<[u64]>,
    history: Arc<[u64]>,
}

impl Planes {
    pub fn empty(words: usize) -> Planes {
        let zero: Arc<[u64]> = vec![0u64; words].into();
        Planes {
            view: zero.clone(),
            view_members: zero.clone(),
            history: zero,
        }
    }

    pub(crate) fn from_vecs(view: Vec<u64>, view_members: Vec<u64>, history: Vec<u64>) -> Planes {
        Planes {
            view: view.into(),
            view_members: view_members.into(),
            history: history.into(),
        }
    }

    pub fn view(&self) -> &[u64] {
        &self.view
    }

    pub fn view_members(&self) -> &[u64] {
        &self.view_members
    }

    pub fn history(&self) -> &[u64] {
        &self.history
    }

    pub fn words(&self) -> usize {
        self.view.len()
    }

    pub fn get(&self, set: RoleSetId) -> Visible {
        let index = set.index();
        let mut visible = Visible::empty();
        visible.set(Visible::VIEW, bits::test(&self.view, index));
        visible.set(Visible::VIEW_MEMBERS, bits::test(&self.view_members, index));
        visible.set(Visible::HISTORY, bits::test(&self.history, index));
        visible
    }

    pub fn same_as(&self, other: &Planes) -> bool {
        (Arc::ptr_eq(&self.view, &other.view) || self.view == other.view)
            && (Arc::ptr_eq(&self.view_members, &other.view_members)
                || self.view_members == other.view_members)
            && (Arc::ptr_eq(&self.history, &other.history) || self.history == other.history)
    }

    pub(crate) fn identity(&self) -> [*const u64; 3] {
        [
            Arc::as_ptr(&self.view).cast::<u64>(),
            Arc::as_ptr(&self.view_members).cast::<u64>(),
            Arc::as_ptr(&self.history).cast::<u64>(),
        ]
    }

    pub(crate) fn with_view(&self, view: Vec<u64>) -> Planes {
        Planes {
            view: view.into(),
            view_members: self.view_members.clone(),
            history: self.history.clone(),
        }
    }

    pub(crate) fn keep_equal(self, old: &Planes) -> Planes {
        let pick = |new: Arc<[u64]>, old: &Arc<[u64]>| if new == *old { old.clone() } else { new };
        Planes {
            view: pick(self.view, &old.view),
            view_members: pick(self.view_members, &old.view_members),
            history: pick(self.history, &old.history),
        }
    }

    pub(crate) fn remapped(&self, remap: &[u32], words: usize) -> Planes {
        let map = |plane: &[u64]| {
            let mut out = vec![0u64; words];
            for old in bits::ones(plane) {
                if let Some(new) = remap.get(old).copied()
                    && new != DEAD
                {
                    bits::set(&mut out, usize::try_from(new).unwrap_or(usize::MAX));
                }
            }
            out
        };
        Planes::from_vecs(map(&self.view), map(&self.view_members), map(&self.history))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Signature {
    everyone: Stage,
    roles: SmallVec<[(RoleIdx, Grant); 4]>,
    planes: Planes,
    refcount: u32,
}

impl Signature {
    pub fn everyone(&self) -> Stage {
        self.everyone
    }

    pub fn roles(&self) -> &[(RoleIdx, Grant)] {
        &self.roles
    }

    pub fn planes(&self) -> &Planes {
        &self.planes
    }

    pub fn refcount(&self) -> u32 {
        self.refcount
    }

    fn grants(&self) -> u64 {
        u64::try_from(self.roles.len()).unwrap_or(u64::MAX)
    }
}

pub(crate) fn slice_cost(words: usize, grants: u64) -> u64 {
    u64::try_from(words)
        .unwrap_or(u64::MAX)
        .saturating_mul(grants.saturating_add(4))
}

pub(crate) const SCALAR_COST: u64 = 8;

pub(crate) struct Slicer {
    words: usize,
    len: usize,
    view: Vec<u64>,
    view_members: Vec<u64>,
    history: Vec<u64>,
    admin: Vec<u64>,
    holders: Vec<Vec<u64>>,
}

impl Slicer {
    pub(crate) fn cost(rolesets: &RoleSets, roles: usize) -> u64 {
        let sets = u64::try_from(rolesets.len()).unwrap_or(u64::MAX);
        let words = u64::try_from(rolesets.words()).unwrap_or(u64::MAX);
        let roles = u64::try_from(roles).unwrap_or(u64::MAX);
        sets.saturating_mul(8)
            .saturating_add(roles.saturating_mul(words))
    }

    pub(crate) fn new(rolesets: &RoleSets, roles: usize, meter: &mut Meter) -> Slicer {
        meter.charge(Slicer::cost(rolesets, roles));
        let words = rolesets.words();
        let mut slicer = Slicer {
            words,
            len: rolesets.len(),
            view: vec![0; words],
            view_members: vec![0; words],
            history: vec![0; words],
            admin: vec![0; words],
            holders: vec![Vec::new(); roles],
        };
        for (id, set) in rolesets.iter() {
            let index = id.index();
            match set.base() {
                Base::All => bits::set(&mut slicer.admin, index),
                Base::Bits(p) => {
                    bits::assign(
                        &mut slicer.view,
                        index,
                        p.contains(Permissions::VIEW_CHANNEL),
                    );
                    bits::assign(
                        &mut slicer.view_members,
                        index,
                        p.contains(Permissions::VIEW_CHANNEL_MEMBERS),
                    );
                    bits::assign(
                        &mut slicer.history,
                        index,
                        p.contains(Permissions::READ_MESSAGE_HISTORY),
                    );
                }
            }
            for role in set.roles() {
                if let Some(plane) = slicer.holders.get_mut(role.index()) {
                    if plane.is_empty() {
                        plane.resize(words, 0);
                    }
                    bits::set(plane, index);
                }
            }
        }
        slicer
    }

    pub(crate) fn evaluate(
        &self,
        everyone: Stage,
        roles: &[(RoleIdx, Grant)],
        meter: &mut Meter,
    ) -> Planes {
        meter.charge(slice_cost(
            self.words,
            u64::try_from(roles.len()).unwrap_or(u64::MAX),
        ));
        let mut view = vec![0u64; self.words];
        let mut view_members = vec![0u64; self.words];
        let mut history = vec![0u64; self.words];
        let lanes = [
            (Permissions::VIEW_CHANNEL, &self.view),
            (Permissions::VIEW_CHANNEL_MEMBERS, &self.view_members),
            (Permissions::READ_MESSAGE_HISTORY, &self.history),
        ];
        for word in 0..self.words {
            let tail = bits::tail_mask(self.len, word);
            let admin = self.admin.get(word).copied().unwrap_or(0);
            let mut allow = [0u64; 3];
            let mut deny = [0u64; 3];
            for (role, grant) in roles {
                let held = self
                    .holders
                    .get(role.index())
                    .and_then(|p| p.get(word))
                    .copied()
                    .unwrap_or(0);
                if held == 0 {
                    continue;
                }
                for (lane, (bit, _)) in lanes.iter().enumerate() {
                    if grant.allow().contains(*bit)
                        && let Some(a) = allow.get_mut(lane)
                    {
                        *a |= held;
                    }
                    if grant.deny().contains(*bit)
                        && let Some(d) = deny.get_mut(lane)
                    {
                        *d |= held;
                    }
                }
            }
            let outs = [&mut view, &mut view_members, &mut history];
            for (lane, ((bit, base), out)) in lanes.iter().zip(outs).enumerate() {
                let mut x = base.get(word).copied().unwrap_or(0);
                if everyone.deny().contains(*bit) {
                    x = 0;
                }
                if everyone.allow().contains(*bit) {
                    x = u64::MAX;
                }
                let d = deny.get(lane).copied().unwrap_or(0);
                let a = allow.get(lane).copied().unwrap_or(0);
                x = (x & !d) | a;
                if let Some(slot) = out.get_mut(word) {
                    *slot = (x | admin) & tail;
                }
            }
        }
        Planes::from_vecs(view, view_members, history)
    }
}

#[derive(Clone, Debug, Default)]
pub struct Signatures {
    list: Vec<Option<Signature>>,
    lookup: HashTable<SigId>,
    free: BTreeSet<SigId>,
    by_channel: Vec<Option<SigId>>,
    grants: u64,
    seeded: Seeded,
}

impl Signatures {
    pub fn len(&self) -> usize {
        self.list.len().saturating_sub(self.free.len())
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn grant_total(&self) -> u64 {
        self.grants
    }

    pub fn get(&self, id: SigId) -> Option<&Signature> {
        self.list.get(id.index()).and_then(Option::as_ref)
    }

    pub fn of_channel(&self, channel: ChannelIdx) -> Option<SigId> {
        self.by_channel.get(channel.index()).copied().flatten()
    }

    pub fn planes_of(&self, channel: ChannelIdx) -> Option<&Planes> {
        self.of_channel(channel)
            .and_then(|id| self.get(id))
            .map(Signature::planes)
    }

    pub fn iter(&self) -> impl Iterator<Item = (SigId, &Signature)> {
        self.list.iter().enumerate().filter_map(|(i, s)| {
            let sig = s.as_ref()?;
            SigId::from_index(i).map(|id| (id, sig))
        })
    }

    pub(crate) fn grow_channels(&mut self, slots: usize) {
        if self.by_channel.len() < slots {
            self.by_channel.resize(slots, None);
        }
    }

    pub(crate) fn detach(&mut self, channel: ChannelIdx) {
        let Some(id) = self
            .by_channel
            .get_mut(channel.index())
            .and_then(Option::take)
        else {
            return;
        };
        let mut freed = None;
        if let Some(Some(sig)) = self.list.get_mut(id.index()) {
            sig.refcount = sig.refcount.saturating_sub(1);
            if sig.refcount == 0 {
                freed = Some(self.seeded.hash(&(sig.everyone, sig.roles.as_slice())));
            }
        }
        if let Some(hash) = freed {
            if let Ok(entry) = self.lookup.find_entry(hash, |i| *i == id) {
                entry.remove();
            }
            if let Some(slot) = self.list.get_mut(id.index())
                && let Some(sig) = slot.take()
            {
                self.grants = self.grants.saturating_sub(sig.grants());
            }
            self.free.insert(id);
        }
    }

    pub(crate) fn keeps(&self, channel: ChannelIdx, rules: &Rules) -> bool {
        self.of_channel(channel)
            .and_then(|id| self.get(id))
            .is_some_and(|s| s.everyone == rules.everyone() && s.roles.as_slice() == rules.roles())
    }

    pub(crate) fn attach(
        &mut self,
        channel: ChannelIdx,
        rules: &Rules,
        slicer: &mut Option<Slicer>,
        make: &dyn Fn(&mut Meter) -> Slicer,
        meter: &mut Meter,
    ) -> Option<(SigId, bool)> {
        let key = (rules.everyone(), rules.roles());
        let hash = self.seeded.hash(&key);
        let list = &self.list;
        let found = self
            .lookup
            .find(hash, |id| {
                list.get(id.index())
                    .and_then(Option::as_ref)
                    .is_some_and(|s| s.everyone == key.0 && s.roles.as_slice() == key.1)
            })
            .copied();
        let (id, fresh) = match found {
            Some(id) => {
                if let Some(Some(sig)) = self.list.get_mut(id.index()) {
                    sig.refcount = sig.refcount.saturating_add(1);
                }
                (id, false)
            }
            None => {
                let slicer = slicer.get_or_insert_with(|| make(meter));
                let planes = slicer.evaluate(rules.everyone(), rules.roles(), meter);
                let id = match self.free.pop_first() {
                    Some(id) => id,
                    None => SigId::from_index(self.list.len())?,
                };
                let sig = Signature {
                    everyone: rules.everyone(),
                    roles: SmallVec::from_slice(rules.roles()),
                    planes,
                    refcount: 1,
                };
                self.grants = self.grants.saturating_add(sig.grants());
                if id.index() >= self.list.len() {
                    self.list.resize(id.index().saturating_add(1), None);
                }
                if let Some(slot) = self.list.get_mut(id.index()) {
                    *slot = Some(sig);
                }
                let seeded = &self.seeded;
                let list = &self.list;
                self.lookup.insert_unique(hash, id, |i| {
                    list.get(i.index())
                        .and_then(Option::as_ref)
                        .map_or(0, |s| seeded.hash(&(s.everyone, s.roles.as_slice())))
                });
                (id, true)
            }
        };
        if let Some(slot) = self.by_channel.get_mut(channel.index()) {
            *slot = Some(id);
        }
        Some((id, fresh))
    }

    pub(crate) fn reslice(
        &mut self,
        skip: &BTreeSet<SigId>,
        slicer: &Slicer,
        meter: &mut Meter,
    ) -> BTreeSet<SigId> {
        let mut changed = BTreeSet::new();
        for (i, slot) in self.list.iter_mut().enumerate() {
            let (Some(sig), Some(id)) = (slot.as_mut(), SigId::from_index(i)) else {
                continue;
            };
            if skip.contains(&id) {
                continue;
            }
            let planes = slicer.evaluate(sig.everyone, &sig.roles, meter);
            if !planes.same_as(&sig.planes) {
                sig.planes = planes.keep_equal(&sig.planes);
                changed.insert(id);
            }
        }
        changed
    }

    pub(crate) fn update_sets(
        &mut self,
        sets: &[RoleSetId],
        rolesets: &RoleSets,
        skip: &BTreeSet<SigId>,
        meter: &mut Meter,
    ) -> BTreeSet<SigId> {
        let words = rolesets.words();
        let mut changed = BTreeSet::new();
        for (i, slot) in self.list.iter_mut().enumerate() {
            let (Some(sig), Some(id)) = (slot.as_mut(), SigId::from_index(i)) else {
                continue;
            };
            if skip.contains(&id) {
                continue;
            }
            meter.charge(u64::try_from(words).unwrap_or(u64::MAX).saturating_mul(3));
            let resized = sig.planes.words() != words;
            let grow = |plane: &[u64]| {
                let mut out = plane.to_vec();
                out.resize(words, 0);
                out
            };
            let mut view = grow(sig.planes.view());
            let mut view_members = grow(sig.planes.view_members());
            let mut history = grow(sig.planes.history());
            for set in sets {
                meter.charge(SCALAR_COST);
                let Some(roleset) = rolesets.get(*set) else {
                    continue;
                };
                let visible = eval::role_set_visible(
                    roleset.base(),
                    roleset.roles(),
                    sig.everyone,
                    &sig.roles,
                );
                let index = set.index();
                bits::assign(&mut view, index, visible.contains(Visible::VIEW));
                bits::assign(
                    &mut view_members,
                    index,
                    visible.contains(Visible::VIEW_MEMBERS),
                );
                bits::assign(&mut history, index, visible.contains(Visible::HISTORY));
            }
            let planes = Planes::from_vecs(view, view_members, history);
            if resized || !planes.same_as(&sig.planes) {
                sig.planes = planes.keep_equal(&sig.planes);
                changed.insert(id);
            }
        }
        changed
    }

    pub(crate) fn remapped(&self, remap: &[u32], words: usize) -> Signatures {
        let mut next = self.clone();
        for sig in next.list.iter_mut().flatten() {
            sig.planes = sig.planes.remapped(remap, words);
        }
        next
    }

    pub(crate) fn channels_remapped(&mut self, remap: &[u32], slots: usize) {
        let mut by_channel = vec![None; slots];
        for (old, sig) in self.by_channel.iter().enumerate() {
            if let Some(new) = remap.get(old).copied().filter(|n| *n != DEAD)
                && let Some(entry) = usize::try_from(new)
                    .ok()
                    .and_then(|n| by_channel.get_mut(n))
            {
                *entry = *sig;
            }
        }
        self.by_channel = by_channel;
    }
}
