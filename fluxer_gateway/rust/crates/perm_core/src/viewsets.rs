// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex, OnceLock};

use foldhash::fast::RandomState;

use crate::bits;
use crate::budget::Meter;
use crate::ids::{ChannelIdx, Epoch, RoleSetId, UserId};
use crate::model::Model;
use crate::shared::{PermsError, View};

pub const MAX_CANDIDATES: usize = 4096;
pub const PAIR_MEMO_LIMIT: usize = 4096;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Subject {
    Member(UserId),
    NonMember(UserId),
    Visitor,
}

impl Subject {
    pub fn user(self) -> Option<UserId> {
        match self {
            Subject::Member(user) | Subject::NonMember(user) => Some(user),
            Subject::Visitor => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ViewSetId(u32);

impl ViewSetId {
    pub const fn get(self) -> u32 {
        self.0
    }

    pub fn index(self) -> usize {
        usize::try_from(self.0).unwrap_or(usize::MAX)
    }

    fn from_index(index: usize) -> Option<ViewSetId> {
        u32::try_from(index).ok().map(ViewSetId)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum ViewKey {
    RoleSet(RoleSetId),
    User(UserId),
    Visitor,
}

pub type Stamp = Option<(RoleSetId, bool)>;

#[derive(Clone, Debug)]
pub(crate) struct Handle {
    id: Option<ViewSetId>,
    words: Arc<[u64]>,
}

#[derive(Debug, Default)]
struct Table {
    sets: Vec<Arc<[u64]>>,
    refs: Vec<u32>,
    ids: HashMap<Arc<[u64]>, ViewSetId, RandomState>,
    keys: BTreeMap<ViewKey, (Stamp, ViewSetId)>,
    memo: HashMap<(ViewSetId, ViewSetId), bool, RandomState>,
    bytes: usize,
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct ViewSetsAudit {
    pub sets: Vec<Arc<[u64]>>,
    pub refs: Vec<u32>,
    pub keys: Vec<(ViewKey, Stamp, ViewSetId)>,
    pub memo: Vec<((ViewSetId, ViewSetId), bool)>,
}

#[derive(Debug)]
pub(crate) struct ViewSets {
    positions: OnceLock<Arc<[u32]>>,
    table: Mutex<Table>,
    limit: usize,
}

impl ViewSets {
    pub(crate) fn new(limit: usize) -> ViewSets {
        ViewSets {
            positions: OnceLock::new(),
            table: Mutex::new(Table::default()),
            limit,
        }
    }

    fn positions(&self, model: &Model, meter: &mut Meter) -> Arc<[u32]> {
        if let Some(known) = self.positions.get() {
            return known.clone();
        }
        let channels = &model.channels;
        meter.charge(u64::try_from(channels.slots()).unwrap_or(u64::MAX));
        let mut list = vec![u32::MAX; channels.slots()];
        for (i, idx) in channels.order().iter().enumerate() {
            if let Some(slot) = list.get_mut(idx.index()) {
                *slot = u32::try_from(i).unwrap_or(u32::MAX);
            }
        }
        let list: Arc<[u32]> = list.into();
        if self.positions.set(list.clone()).is_err() {
            return self.positions.get().cloned().unwrap_or(list);
        }
        list
    }

    fn lookup(&self, key: ViewKey, stamp: Stamp) -> Option<Handle> {
        let table = self.table.lock().ok()?;
        let (known, id) = table.keys.get(&key)?;
        if *known != stamp {
            return None;
        }
        let words = table.sets.get(id.index())?.clone();
        Some(Handle {
            id: Some(*id),
            words,
        })
    }

    fn intern(&self, key: ViewKey, stamp: Stamp, words: Arc<[u64]>) -> Handle {
        let uninterned = |words: Arc<[u64]>| Handle { id: None, words };
        let Ok(mut table) = self.table.lock() else {
            return uninterned(words);
        };
        let id = match table.ids.get(&words) {
            Some(id) => *id,
            None => {
                let size = words.len().saturating_mul(16).saturating_add(64);
                let Some(id) = ViewSetId::from_index(table.sets.len()) else {
                    return uninterned(words);
                };
                if table.bytes.saturating_add(size) > self.limit {
                    return uninterned(words);
                }
                table.sets.push(words.clone());
                table.refs.push(0);
                table.ids.insert(words.clone(), id);
                table.bytes = table.bytes.saturating_add(size);
                id
            }
        };
        if let Some((_, old)) = table.keys.insert(key, (stamp, id))
            && let Some(count) = table.refs.get_mut(old.index())
        {
            *count = count.saturating_sub(1);
        }
        if let Some(count) = table.refs.get_mut(id.index()) {
            *count = count.saturating_add(1);
        }
        let words = table.sets.get(id.index()).cloned().unwrap_or(words);
        Handle {
            id: Some(id),
            words,
        }
    }

    fn memo(&self, pair: (ViewSetId, ViewSetId)) -> Option<bool> {
        self.table.lock().ok()?.memo.get(&pair).copied()
    }

    fn remember(&self, pair: (ViewSetId, ViewSetId), hit: bool) {
        if let Ok(mut table) = self.table.lock()
            && table.memo.len() < PAIR_MEMO_LIMIT
        {
            table.memo.insert(pair, hit);
        }
    }

    pub(crate) fn audit(&self) -> ViewSetsAudit {
        let Ok(table) = self.table.lock() else {
            return ViewSetsAudit::default();
        };
        let mut memo: Vec<((ViewSetId, ViewSetId), bool)> =
            table.memo.iter().map(|(k, v)| (*k, *v)).collect();
        memo.sort_unstable();
        ViewSetsAudit {
            sets: table.sets.clone(),
            refs: table.refs.clone(),
            keys: table
                .keys
                .iter()
                .map(|(key, (stamp, id))| (*key, *stamp, *id))
                .collect(),
            memo,
        }
    }
}

impl View<'_> {
    fn key_of(&self, subject: Subject) -> Option<(ViewKey, Stamp)> {
        match subject {
            Subject::NonMember(_) => None,
            Subject::Visitor => Some((ViewKey::Visitor, None)),
            Subject::Member(user) => {
                let entry = self.members.get(user)?;
                if entry.is_quarantined() && self.model.owner != Some(user) {
                    return None;
                }
                if self.model.exceptions.contains(user) {
                    Some((
                        ViewKey::User(user),
                        Some((entry.roleset(), entry.is_quarantined())),
                    ))
                } else {
                    Some((ViewKey::RoleSet(entry.roleset()), None))
                }
            }
        }
    }

    fn words_of(&self, list: impl Iterator<Item = ChannelIdx>, meter: &mut Meter) -> Arc<[u64]> {
        let model = &self.model;
        let positions = model.viewsets.positions(model, meter);
        let mut words = vec![0u64; bits::words_for(model.channels.len())];
        let mut seen = 0u64;
        for idx in list {
            seen = seen.saturating_add(1);
            if let Some(p) = positions.get(idx.index()).copied()
                && p != u32::MAX
            {
                bits::set(&mut words, usize::try_from(p).unwrap_or(usize::MAX));
            }
        }
        meter.charge(seen.saturating_add(u64::try_from(words.len()).unwrap_or(u64::MAX)));
        words.into()
    }

    fn build(&self, key: ViewKey, meter: &mut Meter) -> Arc<[u64]> {
        match key {
            ViewKey::RoleSet(set) => {
                let list = self.model.set_viewable(set, meter);
                self.words_of(list.iter().copied(), meter)
            }
            ViewKey::User(user) => {
                let list = self.viewable_idx(user, meter);
                self.words_of(list.into_iter(), meter)
            }
            ViewKey::Visitor => {
                let list = self.visitor_channels(false, meter);
                let channels = &self.model.channels;
                self.words_of(
                    list.into_iter().filter_map(|(id, _)| channels.resolve(id)),
                    meter,
                )
            }
        }
    }

    fn handle(
        &self,
        subject: Subject,
        local: &mut BTreeMap<ViewKey, Handle>,
        meter: &mut Meter,
    ) -> Option<Handle> {
        let (key, stamp) = self.key_of(subject)?;
        if let Some(known) = local.get(&key) {
            return Some(known.clone());
        }
        let sets = &self.model.viewsets;
        let handle = match sets.lookup(key, stamp) {
            Some(known) => known,
            None => {
                let words = self.build(key, meter);
                sets.intern(key, stamp, words)
            }
        };
        local.insert(key, handle.clone());
        Some(handle)
    }

    fn intersects(&self, a: &Handle, b: &Handle, meter: &mut Meter) -> bool {
        let and = |meter: &mut Meter| {
            meter.charge(u64::try_from(a.words.len().min(b.words.len())).unwrap_or(u64::MAX));
            a.words.iter().zip(b.words.iter()).any(|(x, y)| x & y != 0)
        };
        let (Some(x), Some(y)) = (a.id, b.id) else {
            return and(meter);
        };
        let pair = (x.min(y), x.max(y));
        let sets = &self.model.viewsets;
        if let Some(hit) = sets.memo(pair) {
            return hit;
        }
        let hit = and(meter);
        sets.remember(pair, hit);
        hit
    }

    pub fn subject_view_set(&self, subject: &Subject, meter: &mut Meter) -> (Epoch, Arc<[u64]>) {
        let epoch = self.model.epoch;
        match self.handle(*subject, &mut BTreeMap::new(), meter) {
            Some(handle) => (epoch, handle.words),
            None => (
                epoch,
                vec![0u64; bits::words_for(self.model.channels.len())].into(),
            ),
        }
    }

    pub fn view_set_id(&self, subject: &Subject, meter: &mut Meter) -> Option<ViewSetId> {
        self.handle(*subject, &mut BTreeMap::new(), meter)
            .and_then(|handle| handle.id)
    }

    pub fn view_sets_audit(&self) -> ViewSetsAudit {
        self.model.viewsets.audit()
    }

    pub fn filter_sharing(
        &self,
        target: &Subject,
        candidates: &[Subject],
        meter: &mut Meter,
    ) -> Result<Vec<bool>, PermsError> {
        if candidates.len() > MAX_CANDIDATES {
            return Err(PermsError::BadInput("too many candidates".to_owned()));
        }
        let mut out = vec![false; candidates.len()];
        let Subject::Member(user) = *target else {
            return Ok(out);
        };
        if !self.members.contains(user) {
            return Ok(out);
        }
        let mut local = BTreeMap::new();
        let Some(theirs) = self.handle(*target, &mut local, meter) else {
            return Ok(out);
        };
        for (slot, candidate) in out.iter_mut().zip(candidates) {
            meter.charge(8);
            if candidate.user() == Some(user) {
                continue;
            }
            if let Some(own) = self.handle(*candidate, &mut local, meter) {
                *slot = self.intersects(&own, &theirs, meter);
            }
        }
        Ok(out)
    }
}
