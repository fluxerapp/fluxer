// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU8, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, TryLockError};

use arc_swap::{ArcSwap, Guard};

use crate::apply::{self, MEMBER_WRITE};
use crate::budget::{Budget, Meter, Units};
use crate::diff::{self, Delta, Group, Regroup, VisibilityChange};
use crate::event::{Cutoff, Event, Snapshot};
use crate::ids::{ChannelId, Epoch, GuildId, RoleSetId, UserId};
use crate::members::MemberIndex;
use crate::metrics::{Metrics, MetricsSink};
use crate::model::Model;
use crate::sharded::{ShardRead, Sharded, Wait};

pub const MAX_EVENTS: usize = 64;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Config {
    pub work_budget_units: u64,
    pub viewable_cache_bytes: u64,
    pub dead_role_set_floor: u32,
    pub scan_chunk: u32,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            work_budget_units: Budget::DEFAULT_UNITS,
            viewable_cache_bytes: 1 << 20,
            dead_role_set_floor: 4096,
            scan_chunk: 4096,
        }
    }
}

impl Config {
    pub fn budget(&self) -> Budget {
        Budget::new(Units::new(self.work_budget_units))
    }

    fn viewable_limit(&self) -> usize {
        usize::try_from(self.viewable_cache_bytes).unwrap_or(usize::MAX)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum PermsError {
    NeedsDirty,
    Broken,
    Gone,
    NotOwner,
    MalformedMember(UserId),
    EmptyRequirement,
    BadInput(String),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Status {
    Ready,
    Broken,
    Released,
}

const READY: u8 = 0;
const BROKEN: u8 = 1;
const RELEASED: u8 = 2;

#[derive(Clone, Debug)]
pub struct Effects {
    epoch: Epoch,
    before: Arc<Model>,
    after: Arc<Model>,
    regroups: Vec<Regroup>,
    changes: Vec<VisibilityChange>,
    spent: Units,
    estimate: Units,
    compaction_due: bool,
}

impl Effects {
    pub fn epoch(&self) -> Epoch {
        self.epoch
    }

    pub fn before(&self) -> &Arc<Model> {
        &self.before
    }

    pub fn after(&self) -> &Arc<Model> {
        &self.after
    }

    pub fn regroups(&self) -> &[Regroup] {
        &self.regroups
    }

    pub fn changes(&self) -> &[VisibilityChange] {
        &self.changes
    }

    pub fn spent(&self) -> Units {
        self.spent
    }

    pub fn estimate(&self) -> Units {
        self.estimate
    }

    pub fn compaction_due(&self) -> bool {
        self.compaction_due
    }

    pub fn roleset_remap(&self) -> Option<&Arc<[u32]>> {
        if self.before.rolesets.generation() == self.after.rolesets.generation() {
            return None;
        }
        self.after.rolesets.remap()
    }

    pub fn channel_remap(&self) -> Option<&Arc<[u32]>> {
        if self.before.channels.generation() == self.after.channels.generation() {
            return None;
        }
        self.after.channels.remap()
    }

    pub fn delta(&self) -> Delta {
        diff::diff(&self.before, &self.after)
    }

    pub fn is_empty(&self) -> bool {
        self.regroups.is_empty()
            && self.changes.is_empty()
            && Arc::ptr_eq(&self.before, &self.after)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Stats {
    pub bytes: u64,
    pub members: u64,
    pub role_sets: u64,
    pub dead_role_sets: u64,
    pub signatures: u64,
    pub classes: u64,
    pub exceptions: u64,
    pub epoch: u64,
    pub rebuilds: u64,
    pub quarantined: u64,
    pub masked: u64,
    pub dirty_calls: u64,
    pub unknown_channel: u64,
}

#[derive(Debug, Default)]
struct Counters {
    rebuilds: AtomicU64,
    masked: AtomicU64,
    dirty_calls: AtomicU64,
    unknown_channel: AtomicU64,
}

#[derive(Debug)]
pub struct PermsShared {
    guild: GuildId,
    model: ArcSwap<Model>,
    members: Sharded<MemberIndex>,
    writer: Mutex<()>,
    status: AtomicU8,
    revision: AtomicU64,
    counters: Counters,
    metrics: Metrics,
    config: Config,
    #[cfg(feature = "fault")]
    faults: Faults,
}

#[derive(Debug)]
pub struct Retired {
    model: Arc<Model>,
    members: MemberIndex,
}

impl Retired {
    pub fn members(&self) -> usize {
        self.members.len()
    }

    pub fn model(&self) -> &Arc<Model> {
        &self.model
    }
}

pub struct View<'a> {
    pub(crate) model: Guard<Arc<Model>>,
    pub(crate) members: ShardRead<'a, MemberIndex>,
}

impl<'a> View<'a> {
    pub fn model(&self) -> &Arc<Model> {
        &self.model
    }

    pub fn members(&self) -> &MemberIndex {
        &self.members
    }

    pub fn roleset(&self, user: UserId) -> Option<RoleSetId> {
        self.members.roleset(user)
    }

    pub fn group_of(&self, user: UserId) -> Option<Group> {
        diff::group_of(&self.model, user, self.members.get(user))
    }
}

struct BreakOnUnwind<'a>(&'a AtomicU8);

impl Drop for BreakOnUnwind<'_> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            self.0.store(BROKEN, Ordering::Release);
        }
    }
}

#[cfg(feature = "fault")]
#[derive(Debug, Default)]
struct Faults {
    extra_units: AtomicU64,
    before_swap: Mutex<Option<fn(&PermsShared)>>,
}

const COMPACT_RETRIES: u32 = 3;

impl PermsShared {
    pub fn new(guild: GuildId, config: Config) -> Arc<PermsShared> {
        PermsShared::build(guild, config, None)
    }

    pub fn with_sink(
        guild: GuildId,
        config: Config,
        sink: Arc<dyn MetricsSink>,
    ) -> Arc<PermsShared> {
        PermsShared::build(guild, config, Some(sink))
    }

    fn build(
        guild: GuildId,
        config: Config,
        sink: Option<Arc<dyn MetricsSink>>,
    ) -> Arc<PermsShared> {
        Arc::new(PermsShared {
            guild,
            model: ArcSwap::from_pointee(Model::empty(guild)),
            members: Sharded::new(MemberIndex::new()),
            writer: Mutex::new(()),
            status: AtomicU8::new(READY),
            revision: AtomicU64::new(0),
            counters: Counters::default(),
            metrics: Metrics::new(sink),
            config,
            #[cfg(feature = "fault")]
            faults: Faults::default(),
        })
    }

    pub fn guild(&self) -> GuildId {
        self.guild
    }

    pub fn config(&self) -> Config {
        self.config
    }

    pub fn status(&self) -> Status {
        match self.status.load(Ordering::Acquire) {
            READY => Status::Ready,
            RELEASED => Status::Released,
            _ => Status::Broken,
        }
    }

    pub fn epoch(&self) -> Epoch {
        self.model.load().epoch
    }

    pub fn revision(&self) -> u64 {
        self.revision.load(Ordering::Acquire)
    }

    fn advance(&self) {
        self.revision.fetch_add(1, Ordering::AcqRel);
    }

    pub fn model(&self) -> Result<Arc<Model>, PermsError> {
        self.ready()?;
        Ok(self.model.load_full())
    }

    pub fn latest_model(&self) -> Arc<Model> {
        self.model.load_full()
    }

    pub fn view(&self) -> Result<View<'_>, PermsError> {
        self.ready()?;
        let members = self.members.read().map_err(|_| PermsError::Broken)?;
        let model = self.model.load();
        self.ready()?;
        Ok(View { model, members })
    }

    pub fn try_view(&self) -> Result<View<'_>, PermsError> {
        self.ready()?;
        let members = self.members.try_read()?;
        let model = self.model.load();
        self.ready()?;
        Ok(View { model, members })
    }

    pub fn view_at(&self, model: Arc<Model>) -> Result<View<'_>, PermsError> {
        self.ready()?;
        let members = self.members.read().map_err(|_| PermsError::Broken)?;
        self.ready()?;
        if model.rolesets.generation() != members.generation() {
            return Err(PermsError::BadInput("model generation".to_owned()));
        }
        Ok(View {
            model: Guard::from_inner(model),
            members,
        })
    }

    pub fn try_view_at(&self, model: Arc<Model>) -> Result<View<'_>, PermsError> {
        self.ready()?;
        let members = self.members.try_read()?;
        self.ready()?;
        if model.rolesets.generation() != members.generation() {
            return Err(PermsError::BadInput("model generation".to_owned()));
        }
        Ok(View {
            model: Guard::from_inner(model),
            members,
        })
    }

    pub fn note_dirty_read(&self) {
        self.counters.dirty_calls.fetch_add(1, Ordering::Relaxed);
        self.metrics.dirty_call();
    }

    pub fn note_channel(&self, model: &Model, channel: ChannelId) {
        if model.channel_idx(channel).is_none() {
            self.counters
                .unknown_channel
                .fetch_add(1, Ordering::Relaxed);
        }
    }

    fn note_masked(&self, cutoffs: usize) {
        if cutoffs > 0 {
            self.counters
                .masked
                .fetch_add(to_u64(cutoffs), Ordering::Relaxed);
        }
    }

    fn ready(&self) -> Result<(), PermsError> {
        match self.status() {
            Status::Ready => Ok(()),
            Status::Broken => Err(PermsError::Broken),
            Status::Released => Err(PermsError::Gone),
        }
    }

    pub fn load_estimate(snapshot: &Snapshot) -> Units {
        apply::load_estimate(snapshot)
    }

    pub fn load(&self, snapshot: &Snapshot) -> Result<Epoch, PermsError> {
        self.load_with(snapshot, false)
    }

    pub fn load_dirty(&self, snapshot: &Snapshot) -> Result<Epoch, PermsError> {
        self.load_with(snapshot, true)
    }

    fn load_with(&self, snapshot: &Snapshot, dirty: bool) -> Result<Epoch, PermsError> {
        if snapshot.guild != self.guild {
            return Err(PermsError::BadInput("snapshot guild".to_owned()));
        }
        let estimate = apply::load_estimate(snapshot);
        if !dirty && !self.config.budget().fits(estimate) {
            return Err(PermsError::NeedsDirty);
        }
        let _writer = self.writer(dirty)?;
        self.ready()?;
        let _guard = BreakOnUnwind(&self.status);
        let current = self.model.load_full();
        if current.epoch != Epoch::ZERO {
            return Err(PermsError::BadInput("already loaded".to_owned()));
        }
        if dirty {
            self.counters.dirty_calls.fetch_add(1, Ordering::Relaxed);
            self.metrics.dirty_call();
        }
        let mut meter = Meter::capped(self.config.budget().runaway_cap(estimate));
        let empty = Model::empty(self.guild);
        let fresh = MemberIndex::new();
        let events = apply::snapshot_events(snapshot);
        let plan = apply::plan(&empty, &fresh, &events);
        meter.charge(plan.planned().get());
        let epoch = current.epoch.next();
        let next = apply::build(
            &empty,
            &plan,
            &fresh,
            epoch,
            self.config.viewable_limit(),
            &mut meter,
        );
        let mut index = MemberIndex::new();
        for (user, edit) in &plan.edits {
            if let Some(membership) = edit {
                meter.charge(MEMBER_WRITE);
                index.upsert(membership.entry(*user, &next.exceptions));
            }
        }
        index.fit_sets();
        self.fault_charge(&mut meter);
        if meter.exhausted() {
            self.status.store(BROKEN, Ordering::Release);
            return Err(PermsError::Broken);
        }
        let next = Arc::new(next);
        let bytes = footprint(&next, &index);
        let mut members = self.members.write_with(Wait::of(dirty))?;
        *members = index;
        self.model.store(next);
        drop(members);
        self.advance();
        self.metrics.bytes(bytes);
        self.note_masked(usize::from(snapshot.cutoff == Cutoff::Malformed));
        Ok(epoch)
    }

    pub fn apply(&self, events: &[Event]) -> Result<Effects, PermsError> {
        self.apply_with(events, false)
    }

    pub fn apply_dirty(&self, events: &[Event]) -> Result<Effects, PermsError> {
        self.apply_with(events, true)
    }

    fn apply_with(&self, events: &[Event], dirty: bool) -> Result<Effects, PermsError> {
        if events.len() > MAX_EVENTS {
            return Err(PermsError::BadInput("too many events".to_owned()));
        }
        let _writer = self.writer(dirty)?;
        self.ready()?;
        let _guard = BreakOnUnwind(&self.status);
        let before = self.model.load_full();
        let read = self.members.read().map_err(|_| PermsError::Broken)?;
        let plan = apply::plan(&before, &read, events);
        let estimate = plan.estimate();
        if !dirty && !self.config.budget().fits(estimate) {
            return Err(PermsError::NeedsDirty);
        }
        if dirty {
            self.counters.dirty_calls.fetch_add(1, Ordering::Relaxed);
            self.metrics.dirty_call();
        }
        let mut meter = Meter::capped(self.config.budget().runaway_cap(estimate));
        meter.charge(plan.planned().get());
        let edits = u64::try_from(plan.edits.len()).unwrap_or(u64::MAX);
        meter.charge(edits.saturating_mul(MEMBER_WRITE));
        let rebuilt = plan.changed && apply::needs_build(&before, &plan);
        let after = if !plan.changed {
            before.clone()
        } else if rebuilt {
            Arc::new(apply::build(
                &before,
                &plan,
                &read,
                before.epoch.next(),
                self.config.viewable_limit(),
                &mut meter,
            ))
        } else {
            Arc::new(apply::republish(
                &before,
                &plan,
                before.epoch.next(),
                self.config.viewable_limit(),
            ))
        };
        let regroups = apply::regroups(&before, &after, &read, &plan);
        let changes = apply::visibility(&before, &after, &read, &plan, &regroups, &mut meter);
        let flagged = apply::flag_changes(&before, &read, &plan);
        drop(read);
        self.fault_charge(&mut meter);
        if meter.exhausted() {
            self.status.store(BROKEN, Ordering::Release);
            return Err(PermsError::Broken);
        }
        let edited = !plan.edits.is_empty() || !flagged.is_empty();
        let due = if !edited {
            if !Arc::ptr_eq(&before, &after) {
                self.model.store(after.clone());
            }
            let members = self.members.read().map_err(|_| PermsError::Broken)?;
            self.metrics.bytes(footprint(&after, &members));
            apply::compaction(&after, &members, self.config.dead_role_set_floor).due()
        } else {
            let mut members = self.members.write_with(Wait::of(dirty))?;
            for (user, edit) in &plan.edits {
                match edit {
                    Some(membership) => {
                        members.upsert(membership.entry(*user, &after.exceptions));
                    }
                    None => {
                        members.remove(*user);
                    }
                }
            }
            for (user, flags) in flagged {
                members.set_flags(user, flags);
            }
            if !Arc::ptr_eq(&before, &after) {
                self.model.store(after.clone());
            }
            self.metrics.bytes(footprint(&after, &members));
            apply::compaction(&after, &members, self.config.dead_role_set_floor).due()
        };
        if edited || !Arc::ptr_eq(&before, &after) {
            self.advance();
        }
        if rebuilt {
            self.counters.rebuilds.fetch_add(1, Ordering::Relaxed);
        }
        self.note_masked(
            events
                .iter()
                .filter(|event| matches!(event, Event::HistoryCutoff(Cutoff::Malformed)))
                .count(),
        );
        Ok(Effects {
            epoch: after.epoch,
            before,
            after,
            regroups,
            changes,
            spent: meter.spent(),
            estimate,
            compaction_due: due,
        })
    }

    pub fn compaction_due(&self) -> bool {
        let Ok(members) = self.members.read() else {
            return false;
        };
        let model = self.model.load();
        apply::compaction(&model, &members, self.config.dead_role_set_floor).due()
    }

    pub fn compaction_estimate(&self) -> Result<Units, PermsError> {
        let view = self.view()?;
        Ok(apply::compaction_estimate(&view.model, &view.members))
    }

    pub fn compact(&self) -> Result<Option<Effects>, PermsError> {
        self.compact_with(false)
    }

    pub fn compact_dirty(&self) -> Result<Option<Effects>, PermsError> {
        self.compact_with(true)
    }

    fn compact_with(&self, dirty: bool) -> Result<Option<Effects>, PermsError> {
        let _guard = BreakOnUnwind(&self.status);
        let mut counted = false;
        for attempt in 0..=COMPACT_RETRIES {
            let early = if attempt == COMPACT_RETRIES {
                Some(self.writer(dirty)?)
            } else {
                None
            };
            self.ready()?;
            let (before, index) = {
                let _held = if early.is_none() {
                    Some(self.writer(dirty)?)
                } else {
                    None
                };
                let read = self.members.read().map_err(|_| PermsError::Broken)?;
                (self.model.load_full(), MemberIndex::clone(&read))
            };
            let parts = apply::compaction(&before, &index, self.config.dead_role_set_floor);
            if !parts.due() {
                return Ok(None);
            }
            let estimate = apply::compaction_estimate(&before, &index);
            if !dirty && !self.config.budget().fits(estimate) {
                return Err(PermsError::NeedsDirty);
            }
            if dirty && !counted {
                counted = true;
                self.counters.dirty_calls.fetch_add(1, Ordering::Relaxed);
                self.metrics.dirty_call();
            }
            let mut meter = Meter::capped(self.config.budget().runaway_cap(estimate));
            let (next, next_index) = apply::compact(
                &before,
                &index,
                parts,
                before.epoch.next(),
                self.config.viewable_limit(),
                &mut meter,
            );
            self.fault_charge(&mut meter);
            if meter.exhausted() {
                self.status.store(BROKEN, Ordering::Release);
                return Err(PermsError::Broken);
            }
            let (writes, generation) = (index.writes(), index.generation());
            drop(index);
            if early.is_none() {
                self.fault_before_swap();
            }
            let _writer = match early {
                Some(writer) => writer,
                None => self.writer(dirty)?,
            };
            self.ready()?;
            let next = Arc::new(next);
            let mut members = self.members.lock_with(Wait::of(dirty))?;
            let current = self.model.load();
            if current.epoch != before.epoch
                || members.writes() != writes
                || members.generation() != generation
            {
                continue;
            }
            drop(current);
            let old = next_index.map(|remapped| members.replace(remapped));
            self.model.store(next.clone());
            let bytes = footprint(&next, &members);
            drop(members);
            drop(old);
            self.advance();
            self.metrics.bytes(bytes);
            self.counters.rebuilds.fetch_add(1, Ordering::Relaxed);
            return Ok(Some(Effects {
                epoch: next.epoch,
                before,
                after: next,
                regroups: Vec::new(),
                changes: Vec::new(),
                spent: meter.spent(),
                estimate,
                compaction_due: false,
            }));
        }
        self.status.store(BROKEN, Ordering::Release);
        Err(PermsError::Broken)
    }

    #[cfg(feature = "fault")]
    pub fn fault_extra_units(&self, units: u64) {
        self.faults.extra_units.store(units, Ordering::Relaxed);
    }

    #[cfg(feature = "fault")]
    pub fn fault_on_compaction_swap(&self, hook: Option<fn(&PermsShared)>) {
        if let Ok(mut slot) = self.faults.before_swap.lock() {
            *slot = hook;
        }
    }

    #[cfg(feature = "fault")]
    pub fn fault_member_index_clones(&self) -> u64 {
        self.members.clones_under_lock()
    }

    #[cfg(feature = "fault")]
    fn fault_charge(&self, meter: &mut Meter) {
        meter.charge(self.faults.extra_units.load(Ordering::Relaxed));
    }

    #[cfg(not(feature = "fault"))]
    fn fault_charge(&self, _meter: &mut Meter) {}

    #[cfg(feature = "fault")]
    fn fault_before_swap(&self) {
        let hook = self.faults.before_swap.lock().ok().and_then(|slot| *slot);
        if let Some(hook) = hook {
            hook(self);
        }
    }

    #[cfg(not(feature = "fault"))]
    fn fault_before_swap(&self) {}

    pub fn viewers_chunked(
        &self,
        channel: ChannelId,
    ) -> Result<Vec<crate::ids::UserId>, PermsError> {
        let chunk = usize::try_from(self.config.scan_chunk.max(1)).unwrap_or(4096);
        let mut restarts = 0u32;
        'scan: loop {
            let mut out = Vec::new();
            let mut start = 0usize;
            let mut version = None;
            loop {
                let view = self.view()?;
                let current = (view.members.version(), view.model.epoch);
                if version.is_some_and(|v| v != current) {
                    restarts = restarts.saturating_add(1);
                    if restarts < 3 {
                        continue 'scan;
                    }
                    let mut meter = Meter::new();
                    return Ok(view.viewers(channel, &mut meter));
                }
                version = Some(current);
                let len = view.members.len();
                let end = start.saturating_add(chunk).min(len);
                view.viewers_range(channel, start, end, &mut out);
                if end >= len {
                    return Ok(out);
                }
                start = end;
            }
        }
    }

    pub fn release(&self) -> Option<Retired> {
        let _writer = self.writer.lock();
        if self.status.swap(RELEASED, Ordering::AcqRel) == RELEASED {
            return None;
        }
        let members = match self.members.write() {
            Ok(mut members) => std::mem::take(&mut *members),
            Err(_) => MemberIndex::new(),
        };
        Some(self.retire(members))
    }

    pub fn try_release(&self) -> Result<Option<Retired>, PermsError> {
        let _writer = self.writer(false)?;
        if self.status() == Status::Released {
            return Ok(None);
        }
        let mut members = match self.members.write_with(Wait::Try) {
            Ok(members) => Some(members),
            Err(PermsError::NeedsDirty) => return Err(PermsError::NeedsDirty),
            Err(_) => None,
        };
        if self.status.swap(RELEASED, Ordering::AcqRel) == RELEASED {
            return Ok(None);
        }
        let members = members
            .as_mut()
            .map(|members| std::mem::take(&mut **members))
            .unwrap_or_default();
        Ok(Some(self.retire(members)))
    }

    fn retire(&self, members: MemberIndex) -> Retired {
        let model = self.model.swap(Arc::new(Model::empty(self.guild)));
        self.advance();
        self.metrics.bytes(0);
        Retired { model, members }
    }

    fn writer(&self, dirty: bool) -> Result<MutexGuard<'_, ()>, PermsError> {
        if dirty {
            return self.writer.lock().map_err(|_| PermsError::Broken);
        }
        match self.writer.try_lock() {
            Ok(guard) => Ok(guard),
            Err(TryLockError::WouldBlock) => Err(PermsError::NeedsDirty),
            Err(TryLockError::Poisoned(_)) => Err(PermsError::Broken),
        }
    }

    pub fn stats(&self) -> Stats {
        let model = self.model.load_full();
        let (members, dead, quarantined, bytes) = match self.members.read() {
            Ok(index) => (
                to_u64(index.len()),
                u64::from(apply::dead_sets(&model, &index)),
                to_u64(index.quarantined_len()),
                footprint(&model, &index),
            ),
            Err(_) => (0, 0, 0, model_bytes(&model)),
        };
        let role_sets = to_u64(model.rolesets.len());
        Stats {
            bytes,
            members,
            role_sets,
            dead_role_sets: dead,
            signatures: to_u64(model.signatures.len()),
            classes: to_u64(model.classes.len()),
            exceptions: to_u64(model.exceptions.len()),
            epoch: model.epoch.get(),
            rebuilds: self.counters.rebuilds.load(Ordering::Relaxed),
            quarantined,
            masked: self.counters.masked.load(Ordering::Relaxed),
            dirty_calls: self.counters.dirty_calls.load(Ordering::Relaxed),
            unknown_channel: self.counters.unknown_channel.load(Ordering::Relaxed),
        }
    }
}

fn to_u64(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

fn model_bytes(model: &Model) -> u64 {
    let words = to_u64(model.rolesets.words());
    let planes = to_u64(model.signatures.len())
        .saturating_add(to_u64(model.classes.len()))
        .saturating_mul(words)
        .saturating_mul(24);
    let overrides = model
        .classes
        .iter()
        .map(|(_, c)| to_u64(c.overrides().len()))
        .fold(0u64, u64::saturating_add)
        .saturating_mul(16);
    to_u64(model.rolesets.len())
        .saturating_mul(48)
        .saturating_add(to_u64(model.channels.slots()).saturating_mul(200))
        .saturating_add(planes)
        .saturating_add(overrides)
        .saturating_add(to_u64(model.exceptions.len()).saturating_mul(16))
        .saturating_add(to_u64(model.viewable_cache_bytes()))
        .saturating_add(to_u64(model.roles.len()).saturating_mul(64))
}

fn footprint(model: &Model, members: &MemberIndex) -> u64 {
    model_bytes(model).saturating_add(to_u64(members.bytes()))
}
