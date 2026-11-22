// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use perm_core::{Delta, Model, PermsError, PermsShared, RoleSetId, View, diff};

use crate::blocked::{BlockedList, ColId};
use crate::budget::{Budget, Meter, Shape, Units};
use crate::classes::{
    ClassCounts, ColumnTable, SetCounts, Vis, build_offline, check_sorted, class_counts,
    offline_insert, offline_remove,
};
use crate::event::{Change, Connection, Event, Snapshot, SyncScope, Timing, Updates};
use crate::flags::{Status, Visible};
use crate::fragments::Fragments;
use crate::id_order::IdOrder;
use crate::ids::{ChannelId, ClassId, GuildId, ListKey, SessionKey, Slot, UserId};
use crate::masks::{MaskReply, MaskUpdate, Masks};
use crate::members::{Members, Presence};
use crate::payloads::PayloadCache;
use crate::pending::{Mark, Pending, TimerAction};
use crate::ranges::{Range, Ranges};
use crate::render::{self, Ctx, EncodedItem, FillError, Job, Step};
use crate::sections::{Label, Sections, hoist_order};
use crate::sessions::{Sessions, Subscription};

pub const MAX_EVENTS: usize = 64;
const SKIPPED_ABSENT_CAP: u64 = 1_000_000_000;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Config {
    pub offline_threshold: u32,
    pub offline_drop: u32,
    pub work_budget_units: u64,
    pub fragment_budget_bytes: u64,
    pub payload_budget_bytes: u64,
    pub seed: u64,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            offline_threshold: 1000,
            offline_drop: 1500,
            work_budget_units: Budget::DEFAULT_UNITS,
            fragment_budget_bytes: 2 << 20,
            payload_budget_bytes: 1 << 20,
            seed: 0x6d6c_5f63_6f72_6501,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CoreError {
    NeedsDirty,
    BadInput(String),
    Perms(PermsError),
}

impl From<PermsError> for CoreError {
    fn from(e: PermsError) -> CoreError {
        match e {
            PermsError::NeedsDirty => CoreError::NeedsDirty,
            other => CoreError::Perms(other),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Mode {
    Normal,
    Dirty,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Group {
    pub label: Label,
    pub count: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Row {
    Group(Group),
    Member(UserId),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SyncOp {
    pub range: Range,
    pub rows: Vec<Row>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SyncBody {
    pub channel: ChannelId,
    pub member_count: u32,
    pub online_count: u32,
    pub groups: Vec<Group>,
    pub ops: Vec<SyncOp>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Target<P> {
    Sessions(Vec<P>),
    Session(P),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Dispatch<P> {
    pub target: Target<P>,
    pub body: SyncBody,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Effects<P> {
    pub dispatches: Vec<Dispatch<P>>,
    pub timer: TimerAction,
    pub skipped_absent: u64,
    pub spent: Units,
    pub estimate: Units,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Stats {
    pub bytes: u64,
    pub members: u64,
    pub online: u64,
    pub classes: u64,
    pub columns: u64,
    pub fragment_bytes: u64,
    pub skipped_absent: u64,
    pub gate_drops: u64,
    pub rebuilds: u64,
    pub prefix_ties: u64,
    pub offline_rebuilds: u64,
    pub rehoists: u64,
    pub bulk_rehoists: u64,
    pub dirty_calls: u64,
    pub compactions: u64,
    pub resyncs: u64,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Accounting {
    pub core: u64,
    pub fragments: u64,
    pub payloads: u64,
}

fn needs_resync(last: &Model, model: &Model) -> bool {
    last.rolesets().generation() != model.rolesets().generation()
        || last.channels().generation() != model.channels().generation()
}

#[derive(Debug)]
pub struct Core<P> {
    guild: GuildId,
    config: Config,
    perms: Arc<PermsShared>,
    last: Arc<Model>,
    members: Members,
    sets: SetCounts,
    sections: Sections,
    id_order: IdOrder,
    columns: ColumnTable,
    sessions: Sessions<P>,
    pending: Pending,
    updates: Updates,
    stats: Stats,
    fragments: Fragments,
    payloads: PayloadCache,
    masks: Masks,
    job: Option<Job<P>>,
    render_spent: Units,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PageQuery {
    Offset { offset: u32, limit: u32 },
    After { after: Option<UserId>, limit: u32 },
}

fn budget_bytes(bytes: u64) -> usize {
    usize::try_from(bytes).unwrap_or(usize::MAX)
}

struct Batch<P> {
    dispatches: Vec<Dispatch<P>>,
}

fn recipient_bits() -> Visible {
    Visible::VIEW | Visible::VIEW_MEMBERS
}

fn bad(msg: &str) -> CoreError {
    CoreError::BadInput(msg.to_string())
}

impl<P: Copy + Eq + core::fmt::Debug> Core<P> {
    pub fn new(
        guild: GuildId,
        config: Config,
        perms: Arc<PermsShared>,
    ) -> Result<Core<P>, CoreError> {
        let last = perms.model()?;
        let mut sections = Sections::new();
        sections.reset_order(&last, &hoist_order(&last));
        Ok(Core {
            guild,
            config,
            perms,
            last,
            members: Members::new(config.seed),
            sets: SetCounts::new(),
            sections,
            id_order: IdOrder::new(),
            columns: ColumnTable::new(),
            sessions: Sessions::new(),
            pending: Pending::new(),
            updates: Updates::UpdatesOn,
            stats: Stats::default(),
            fragments: Fragments::new(budget_bytes(config.fragment_budget_bytes), config.seed),
            payloads: PayloadCache::new(budget_bytes(config.payload_budget_bytes), config.seed),
            masks: Masks::new(),
            job: None,
            render_spent: Units::ZERO,
        })
    }

    pub fn guild(&self) -> GuildId {
        self.guild
    }

    pub fn config(&self) -> Config {
        self.config
    }

    fn budget(&self) -> Budget {
        Budget::new(Units::new(self.config.work_budget_units))
    }

    fn shape(&self, model: &Model, margin: usize) -> Shape {
        let overrides = self
            .columns
            .iter()
            .filter_map(|(_, c)| model.class(c.class()))
            .map(|c| c.overrides().len() as u64)
            .max()
            .unwrap_or(0);
        let reach = self
            .config
            .offline_threshold
            .saturating_add(u32::try_from(margin).unwrap_or(u32::MAX));
        let needs_build = self
            .columns
            .iter()
            .any(|(_, c)| c.offline().is_none() && c.offline_count() <= reach);
        let max_offline_build = if needs_build {
            u64::from(self.members.len())
                .saturating_mul(2)
                .saturating_add(u64::from(reach).saturating_mul(16))
                .saturating_add(64)
        } else {
            0
        };
        Shape {
            members: u64::from(self.members.len()),
            online: u64::from(self.members.online()),
            role_sets: self.sets.live_sets(),
            overrides,
            columns: self.columns.len() as u64,
            sections: self.sections.len() as u64,
            lists: self.sessions.lists().count() as u64,
            max_offline_build,
        }
    }

    fn view_for(perms: &PermsShared, mode: Mode) -> Result<View<'_>, CoreError> {
        let view = match mode {
            Mode::Normal => perms.try_view(),
            Mode::Dirty => perms.view(),
        };
        Ok(view?)
    }

    pub fn load_estimate(members: usize) -> Units {
        Shape {
            members: members as u64,
            ..Shape::default()
        }
        .load()
    }

    pub fn load(&mut self, snapshot: &Snapshot, mode: Mode) -> Result<(), CoreError> {
        self.idle()?;
        let estimate = Self::load_estimate(snapshot.members.len());
        if mode == Mode::Normal && !self.budget().fits(estimate) {
            return Err(CoreError::NeedsDirty);
        }
        let perms = Arc::clone(&self.perms);
        let view = Self::view_for(&perms, mode)?;
        let model = Arc::clone(view.model());
        let mut latest: Vec<(UserId, &[u8], RoleSetId)> =
            Vec::with_capacity(snapshot.members.len());
        for (user, name) in &snapshot.members {
            let Some(set) = view.roleset(*user) else {
                return Err(bad("member missing from the permission index"));
            };
            latest.push((*user, name, set));
        }
        keep_last_by_key(&mut latest, |(user, _, _)| *user);
        let name_bytes = latest
            .iter()
            .map(|(_, n, _)| n.len())
            .fold(0usize, usize::saturating_add);
        let mut members = Members::with_capacity(self.config.seed, latest.len(), name_bytes);
        let mut connected: Vec<UserId> = snapshot.connected.clone();
        connected.sort_unstable();
        connected.dedup();
        let mut statuses: Vec<(UserId, Status)> = snapshot.statuses.clone();
        keep_last_by_key(&mut statuses, |(user, _)| *user);
        let status_of = |user: &UserId| {
            statuses
                .binary_search_by_key(user, |(u, _)| *u)
                .ok()
                .and_then(|at| statuses.get(at))
                .map(|(_, status)| *status)
        };
        for user in connected
            .iter()
            .chain(statuses.iter().map(|(user, _)| user))
        {
            members.set_outsider(
                *user,
                Presence {
                    status: status_of(user).unwrap_or_default(),
                    connected: connected.binary_search(user).is_ok(),
                },
            );
        }
        let mut sets = SetCounts::new();
        let mut ids = Vec::with_capacity(latest.len());
        for (user, name, set) in &latest {
            let slot = members
                .insert(*user, name, *set)
                .map_err(|_| bad("name arena full"))?;
            sets.add(*set, members.is_online(slot));
            ids.push(user.get());
        }
        let mut sections = Sections::new();
        sections.reset_order(&model, &hoist_order(&model));
        let mut per_section: Vec<Vec<(u64, Slot)>> = vec![Vec::new(); sections.len()];
        for slot in members.slots() {
            if members.is_online(slot) {
                let at = sections.section_of_set(&model, members.roleset(slot));
                if let Some(bucket) = per_section.get_mut(at) {
                    bucket.push(members.prefixed(slot));
                }
            }
        }
        let mut meter = Meter::new();
        let table = ColumnTable::new();
        for (at, mut bucket) in per_section.into_iter().enumerate() {
            members.sort_prefixed(&mut bucket);
            let vis = Vis {
                model: &model,
                members: &members,
                table: &table,
            };
            let built = BlockedList::from_prefixed(&bucket, &vis, &mut meter);
            if let Some(list) = sections.lists_mut().get_mut(at) {
                *list = built;
            }
        }
        self.members = members;
        self.sets = sets;
        self.sections = sections;
        self.id_order = IdOrder::from_sorted(&ids);
        self.columns = table;
        self.sessions = Sessions::new();
        self.pending = Pending::new();
        self.fragments = Fragments::new(
            budget_bytes(self.config.fragment_budget_bytes),
            self.config.seed,
        );
        self.payloads = PayloadCache::new(
            budget_bytes(self.config.payload_budget_bytes),
            self.config.seed,
        );
        self.masks.reset();
        self.updates = if snapshot.updates_off {
            Updates::UpdatesOff
        } else {
            Updates::UpdatesOn
        };
        self.last = model;
        Ok(())
    }

    fn validate(&self, view: &View<'_>, events: &[Event<P>]) -> Result<(), CoreError> {
        if events.len() > MAX_EVENTS {
            return Err(bad("more than 64 events"));
        }
        for event in events {
            if let Event::MemberUpsert { user, .. } = event
                && view.roleset(*user).is_none()
            {
                return Err(bad(
                    "member upsert for a user missing from the permission index",
                ));
            }
        }
        Ok(())
    }

    fn sync_cost(&self, shape: Shape, list: Option<ListKey>) -> Units {
        match list {
            Some(l) => self.list_sync_cost(shape, l),
            None => self.sessions.lists().fold(Units::ZERO, |total, l| {
                total.plus(self.list_sync_cost(shape, l))
            }),
        }
    }

    fn list_sync_cost(&self, shape: Shape, list: ListKey) -> Units {
        let mut groups: Vec<&Ranges> = self
            .sessions
            .subscribers(list)
            .map(|(_, s)| s.ranges())
            .collect();
        let total = Units::new((groups.len() as u64).saturating_mul(32).saturating_add(16));
        groups.sort_unstable();
        groups.dedup();
        groups
            .into_iter()
            .fold(total, |total, ranges| total.plus(shape.render(ranges)))
    }

    fn marked_cost(&self, shape: Shape) -> (Units, Units) {
        if !self.pending.is_armed() {
            return (Units::ZERO, Units::ZERO);
        }
        self.pending
            .marks()
            .fold((Units::ZERO, Units::ZERO), |(sync, any), (list, mark)| {
                let cost = self.sync_cost(shape, Some(list));
                let sync = if mark == Mark::Sync {
                    sync.plus(cost)
                } else {
                    sync
                };
                (sync, any.plus(cost))
            })
    }

    fn estimate(&self, model: &Arc<Model>, events: &[Event<P>]) -> Units {
        let shape = self.shape(model, events.len());
        let mut total = Units::new(64)
            .plus(Units::new(self.members.compaction_cost().unwrap_or(0)))
            .plus(Units::new(self.columns.len() as u64));
        if !Arc::ptr_eq(&self.last, model) {
            if needs_resync(&self.last, model) {
                total = total.plus(Shape::load(shape));
            }
            let columns = (self.sessions.lists().count() as u64).max(self.columns.len() as u64);
            let reach =
                u64::from(self.config.offline_threshold).saturating_add(events.len() as u64);
            total = total
                .plus(shape.column_fill().times(columns.saturating_add(1)))
                .plus(
                    shape
                        .offline_build(reach)
                        .times(self.offline_builds(model, reach)),
                )
                .plus(shape.rehoist())
                .plus(Units::new((self.sessions.len() as u64).saturating_mul(8)));
        }
        let all = self.sync_cost(shape, None);
        let (mut drain, marked) = self.marked_cost(shape);
        let remarked = all.plus(marked);
        for event in events {
            let cost = match event {
                Event::MemberUpsert { .. } | Event::MemberRemove(_) => shape
                    .point_write()
                    .times(2)
                    .plus(all)
                    .plus(Units::new(64))
                    .plus(std::mem::take(&mut drain)),
                Event::Presence { timing, change, .. } => {
                    let syncs = match timing {
                        Timing::Immediate => all.plus(std::mem::take(&mut drain)),
                        Timing::Deferred => Units::new(shape.lists.saturating_mul(32)),
                    };
                    if *timing == Timing::Deferred || *change == Change::Unchanged {
                        drain = remarked;
                    }
                    shape.point_write().times(2).plus(syncs)
                }
                Event::PresenceCleared(_) => shape.point_write().times(2),
                Event::Connection(..) => {
                    drain = remarked;
                    shape
                        .point_write()
                        .times(2)
                        .plus(Units::new(shape.lists.saturating_mul(32)))
                }
                Event::Permissions(scope) => match scope {
                    SyncScope::AllLists => all,
                    SyncScope::Channel(c) => {
                        self.sync_cost(shape, Some(ListKey::new(*c))).plus(all)
                    }
                    SyncScope::NoLists => Units::new(8),
                },
                Event::Subscribe { channel, sync, .. } => {
                    let fill = match model.class_of_id(*channel) {
                        Some(class) if self.columns.of_class(class).is_none() => {
                            let reach = u64::from(self.config.offline_threshold)
                                .saturating_add(events.len() as u64);
                            let offline = self.counts_of(model, class, &mut Meter::new()).offline();
                            let build = if u64::from(offline) <= reach {
                                shape.offline_build(reach)
                            } else {
                                Units::ZERO
                            };
                            shape.column_fill().plus(build)
                        }
                        _ => Units::ZERO,
                    };
                    fill.plus(shape.render(sync)).plus(Units::new(64))
                }
                Event::Unsubscribe { .. } | Event::DropSession(_) => Units::new(64),
                Event::ViewGained { session, .. } => {
                    let ranges = self
                        .sessions
                        .get(session)
                        .map(|s| s.ranges().clone())
                        .unwrap_or_default();
                    shape.render(&ranges).plus(Units::new(32))
                }
                Event::Flush => all,
                Event::Updates(_) => Units::new(4),
            };
            total = total.plus(cost);
        }
        total
    }

    pub fn apply(
        &mut self,
        events: &[Event<P>],
        delta: Option<&Delta>,
        mode: Mode,
    ) -> Result<Effects<P>, CoreError> {
        let perms = Arc::clone(&self.perms);
        let view = Self::view_for(&perms, mode)?;
        let model = Arc::clone(view.model());
        self.idle()?;
        self.validate(&view, events)?;
        let estimate = self.estimate(&model, events);
        if mode == Mode::Normal && !self.budget().fits(estimate) {
            return Err(CoreError::NeedsDirty);
        }
        if mode == Mode::Dirty {
            self.stats.dirty_calls = self.stats.dirty_calls.saturating_add(1);
        }
        let mut meter = Meter::new();
        let mut batch = Batch {
            dispatches: Vec::new(),
        };
        self.pending.begin();
        if self.members.compaction_cost().is_some() {
            self.members.compact(&mut meter);
            self.stats.compactions = self.stats.compactions.saturating_add(1);
        }
        if !Arc::ptr_eq(&self.last, &model) {
            self.observe(&view, &model, delta, &mut meter);
        }
        for event in events {
            self.event(&view, &model, event, &mut batch, &mut meter);
        }
        self.trim_offline(&mut meter);
        Ok(Effects {
            dispatches: batch.dispatches,
            timer: self.pending.finish(),
            skipped_absent: self.stats.skipped_absent,
            spent: meter.spent(),
            estimate,
        })
    }

    fn observe(
        &mut self,
        view: &View<'_>,
        model: &Arc<Model>,
        given: Option<&Delta>,
        meter: &mut Meter,
    ) {
        if needs_resync(&self.last, model) {
            self.resync(view, model, meter);
            return;
        }
        let computed;
        let delta = match given {
            Some(d) if d.from == self.last.epoch() && d.to == model.epoch() => d,
            _ => {
                computed = diff(&self.last, model);
                &computed
            }
        };
        if delta.remap.is_some() || delta.channel_remap.is_some() || delta.resync {
            self.resync(view, model, meter);
            return;
        }
        self.last = Arc::clone(model);
        let changed: BTreeSet<ClassId> = delta
            .classes
            .iter()
            .map(|c| c.class)
            .chain(delta.created.iter().copied())
            .collect();
        let stale: Vec<ColId> = self
            .columns
            .iter()
            .filter(|(_, c)| changed.contains(&c.class()) || delta.freed.contains(&c.class()))
            .map(|(id, _)| id)
            .collect();
        for col in stale {
            self.drop_column(col);
        }
        self.reconcile_columns(model, meter);
        let hoisted = hoist_order(model);
        if hoisted != self.sections.hoisted() {
            let vis = Vis {
                model,
                members: &self.members,
                table: &self.columns,
            };
            let moved = self
                .sections
                .rehoist(model, &hoisted, &self.members, &vis, meter);
            self.stats.rehoists = self.stats.rehoists.saturating_add(1);
            if moved >= crate::sections::SINGLE_MOVE_LIMIT {
                self.stats.bulk_rehoists = self.stats.bulk_rehoists.saturating_add(1);
            }
        } else {
            self.sections.set_labels(model);
        }
    }

    fn resync(&mut self, view: &View<'_>, model: &Arc<Model>, meter: &mut Meter) {
        self.last = Arc::clone(model);
        let live: Vec<Slot> = self.members.slots().collect();
        let mut sets = SetCounts::new();
        for slot in &live {
            meter.charge(4);
            let user = self.members.user(*slot);
            let set = view.roleset(user).unwrap_or(RoleSetId::EVERYONE);
            self.members.set_roleset(*slot, set);
            sets.add(set, self.members.is_online(*slot));
        }
        self.sets = sets;
        self.columns = ColumnTable::new();
        let mut sections = Sections::new();
        sections.reset_order(model, &hoist_order(model));
        let mut per_section: Vec<Vec<(u64, Slot)>> = vec![Vec::new(); sections.len()];
        for slot in live {
            if self.members.is_online(slot) {
                let at = sections.section_of_set(model, self.members.roleset(slot));
                if let Some(bucket) = per_section.get_mut(at) {
                    bucket.push(self.members.prefixed(slot));
                }
            }
        }
        for (at, mut bucket) in per_section.into_iter().enumerate() {
            meter.charge((bucket.len() as u64).saturating_mul(4));
            self.members.sort_prefixed(&mut bucket);
            let vis = Vis {
                model,
                members: &self.members,
                table: &self.columns,
            };
            let built = BlockedList::from_prefixed(&bucket, &vis, meter);
            if let Some(list) = sections.lists_mut().get_mut(at) {
                *list = built;
            }
        }
        self.sections = sections;
        self.stats.resyncs = self.stats.resyncs.saturating_add(1);
        self.reconcile_columns(model, meter);
    }

    fn drop_column(&mut self, col: ColId) {
        for list in self.sections.lists_mut() {
            list.drop_column(col);
        }
        self.columns.remove(col);
    }

    fn reconcile_columns(&mut self, model: &Model, meter: &mut Meter) {
        let mut readers: BTreeMap<ClassId, u32> = BTreeMap::new();
        for (channel, count) in self.sessions.readers() {
            meter.charge(2);
            if let Some(class) = model.class_of_id(channel) {
                let entry = readers.entry(class).or_insert(0);
                *entry = entry.saturating_add(u32::try_from(count).unwrap_or(u32::MAX));
            }
        }
        let unused: Vec<ColId> = self
            .columns
            .iter()
            .filter(|(_, c)| !readers.contains_key(&c.class()))
            .map(|(id, _)| id)
            .collect();
        for col in unused {
            self.drop_column(col);
        }
        for (class, count) in readers {
            let col = self.ensure_column(model, class, meter);
            self.columns.set_readers(col, count);
        }
    }

    fn ensure_column(&mut self, model: &Model, class: ClassId, meter: &mut Meter) -> ColId {
        if let Some(col) = self.columns.of_class(class) {
            return col;
        }
        let col = self.columns.create(class);
        let Core {
            members,
            sections,
            columns,
            ..
        } = self;
        let vis = Vis {
            model,
            members,
            table: columns,
        };
        for list in sections.lists_mut() {
            list.fill_column(col, &vis, meter);
        }
        let counts = class_counts(
            model,
            class,
            &self.sets,
            &self.members,
            &self.sections,
            meter,
        );
        self.columns.set_offline_count(col, counts.offline());
        if counts.offline() <= self.config.offline_threshold {
            let vis = Vis {
                model,
                members: &self.members,
                table: &self.columns,
            };
            let built = build_offline(&vis, class, meter);
            self.columns.set_offline(col, Some(built));
            self.stats.offline_rebuilds = self.stats.offline_rebuilds.saturating_add(1);
        }
        col
    }

    fn trim_offline(&mut self, meter: &mut Meter) {
        let limit = usize::try_from(self.config.offline_drop).unwrap_or(usize::MAX);
        let drop: Vec<ColId> = self
            .columns
            .iter()
            .filter(|(_, c)| {
                meter.charge(1);
                c.offline().is_some_and(|v| v.len() > limit)
            })
            .map(|(id, _)| id)
            .collect();
        for col in drop {
            self.columns.set_offline(col, None);
        }
    }

    fn attach(&mut self, model: &Model, slot: Slot, meter: &mut Meter) {
        let set = self.members.roleset(slot);
        let online = self.members.is_online(slot);
        self.sets.add(set, online);
        if online {
            self.insert_online(model, slot, meter);
        } else {
            self.insert_offline(model, slot, meter);
        }
    }

    fn detach(&mut self, model: &Model, slot: Slot, meter: &mut Meter) {
        let set = self.members.roleset(slot);
        let online = self.members.is_online(slot);
        self.sets.sub(set, online);
        if online {
            self.remove_online(model, slot, meter);
        } else {
            self.remove_offline(model, slot, meter);
        }
    }

    fn insert_online(&mut self, model: &Model, slot: Slot, meter: &mut Meter) {
        let at = self
            .sections
            .section_of_set(model, self.members.roleset(slot));
        let Core {
            members,
            sections,
            columns,
            ..
        } = self;
        let vis = Vis {
            model,
            members,
            table: columns,
        };
        if let Some(list) = sections.lists_mut().get_mut(at) {
            list.insert(slot, members, &vis, meter);
        }
    }

    fn remove_online(&mut self, model: &Model, slot: Slot, meter: &mut Meter) {
        let at = self.sections.known_section(self.members.roleset(slot));
        let Core {
            members,
            sections,
            columns,
            ..
        } = self;
        let vis = Vis {
            model,
            members,
            table: columns,
        };
        if let Some(list) = sections.lists_mut().get_mut(at) {
            list.remove(slot, members, &vis, meter);
        }
    }

    fn insert_offline(&mut self, model: &Model, slot: Slot, meter: &mut Meter) {
        let live: Vec<(ColId, ClassId)> =
            self.columns.iter().map(|(id, c)| (id, c.class())).collect();
        for (col, class) in live {
            meter.charge(2);
            let sees = model
                .visible(class, self.members.roleset(slot), self.members.user(slot))
                .contains(Visible::VIEW);
            if !sees {
                continue;
            }
            self.columns.add_offline(col);
            if let Some(vec) = self.columns.offline_mut(col) {
                offline_insert(vec, slot, &self.members, meter);
            }
        }
    }

    fn remove_offline(&mut self, model: &Model, slot: Slot, meter: &mut Meter) {
        let live: Vec<(ColId, ClassId)> =
            self.columns.iter().map(|(id, c)| (id, c.class())).collect();
        for (col, class) in live {
            meter.charge(2);
            let sees = model
                .visible(class, self.members.roleset(slot), self.members.user(slot))
                .contains(Visible::VIEW);
            if !sees {
                continue;
            }
            self.columns.sub_offline(col);
            if let Some(vec) = self.columns.offline_mut(col) {
                offline_remove(vec, slot, &self.members, meter);
            }
        }
    }

    fn set_presence(&mut self, model: &Model, slot: Slot, presence: Presence, meter: &mut Meter) {
        let was = self.members.is_online(slot);
        let now = presence.status.shown_online(presence.connected);
        if was == now {
            self.members.set_presence(slot, presence);
            return;
        }
        let set = self.members.roleset(slot);
        if was {
            self.remove_online(model, slot, meter);
        } else {
            self.remove_offline(model, slot, meter);
        }
        self.members.set_presence(slot, presence);
        self.sets.set_online(set, now);
        if now {
            self.insert_online(model, slot, meter);
        } else {
            self.insert_offline(model, slot, meter);
        }
    }

    fn is_row(&self, model: &Model, list: ListKey, user: UserId) -> bool {
        let Some(slot) = self.members.find(user) else {
            return false;
        };
        let Some(class) = model.class_of_id(list.channel()) else {
            return false;
        };
        model
            .visible(class, self.members.roleset(slot), user)
            .contains(Visible::VIEW)
    }

    fn note_skipped(&mut self) {
        if self.stats.skipped_absent < SKIPPED_ABSENT_CAP {
            self.stats.skipped_absent = self.stats.skipped_absent.saturating_add(1);
        }
    }

    fn event(
        &mut self,
        view: &View<'_>,
        model: &Arc<Model>,
        event: &Event<P>,
        batch: &mut Batch<P>,
        meter: &mut Meter,
    ) {
        match event {
            Event::MemberUpsert {
                user,
                display_name,
                change,
            } => self.member_upsert(view, model, *user, display_name, *change, batch, meter),
            Event::MemberRemove(user) => self.member_remove(model, *user, batch, meter),
            Event::Presence {
                user,
                status,
                change,
                timing,
            } => self.presence(model, *user, *status, *change, *timing, batch, meter),
            Event::PresenceCleared(user) => {
                let current = self.presence_of(*user);
                self.write_presence(
                    model,
                    *user,
                    Presence {
                        status: Status::Offline,
                        connected: current.connected,
                    },
                    meter,
                );
            }
            Event::Connection(user, connection) => {
                self.connection(model, *user, *connection, meter)
            }
            Event::Permissions(scope) => {
                if self.updates == Updates::UpdatesOff {
                    return;
                }
                match scope {
                    SyncScope::AllLists => self.sync_all(model, batch, meter),
                    SyncScope::Channel(channel) => {
                        let list = ListKey::new(*channel);
                        if self.sessions.has_subscribers(list) {
                            self.immediate(model, list, batch, meter);
                        }
                    }
                    SyncScope::NoLists => {}
                }
            }
            Event::Subscribe {
                session,
                pid,
                user,
                channel,
                ranges,
                sync,
            } => self.subscribe(
                model, session, *pid, *user, *channel, ranges, sync, batch, meter,
            ),
            Event::Unsubscribe { session, channel } => {
                if self.sessions.unsubscribe(session, *channel) {
                    self.reconcile_columns(model, meter);
                }
            }
            Event::DropSession(session) => {
                if self.sessions.drop_session(session).is_some() {
                    self.reconcile_columns(model, meter);
                }
            }
            Event::ViewGained { session, channel } => {
                self.view_gained(model, session, *channel, batch, meter)
            }
            Event::Flush => {
                let lists = self.pending.take_flush();
                for list in lists {
                    self.dispatch_list(model, list, batch, meter);
                }
            }
            Event::Updates(updates) => self.updates = *updates,
        }
    }

    fn presence_of(&self, user: UserId) -> Presence {
        match self.members.find(user) {
            Some(slot) => self.members.presence(slot),
            None => self.members.outsider(user),
        }
    }

    fn write_presence(
        &mut self,
        model: &Model,
        user: UserId,
        presence: Presence,
        meter: &mut Meter,
    ) {
        match self.members.find(user) {
            Some(slot) => {
                self.set_presence(model, slot, presence, meter);
                self.members.bump_presence(slot);
            }
            None => self.members.set_outsider(user, presence),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn member_upsert(
        &mut self,
        view: &View<'_>,
        model: &Model,
        user: UserId,
        display_name: &[u8],
        change: Change,
        batch: &mut Batch<P>,
        meter: &mut Meter,
    ) {
        let set = view.roleset(user).unwrap_or(perm_core::RoleSetId::EVERYONE);
        let lists: Vec<ListKey> = self.sessions.lists().collect();
        let rows_before: Vec<bool> = match change {
            Change::Unchanged => lists.iter().map(|l| self.is_row(model, *l, user)).collect(),
            Change::Changed => Vec::new(),
        };
        match self.members.find(user) {
            Some(slot) => {
                let renamed = casefold_differs(&self.members, slot, display_name);
                if renamed || self.members.roleset(slot) != set {
                    self.detach(model, slot, meter);
                    if renamed && self.members.rename(slot, display_name).is_err() {
                        meter.charge(1);
                    }
                    self.members.set_roleset(slot, set);
                    self.attach(model, slot, meter);
                }
                self.members.bump_member(slot);
            }
            None => {
                if let Ok(slot) = self.members.insert(user, display_name, set) {
                    self.id_order.insert(user, meter);
                    self.masks.joined(&self.members, slot);
                    self.attach(model, slot, meter);
                }
            }
        }
        if self.updates == Updates::UpdatesOff {
            return;
        }
        for (i, list) in lists.iter().enumerate() {
            if change == Change::Unchanged
                && !rows_before.get(i).copied().unwrap_or(true)
                && !self.is_row(model, *list, user)
            {
                self.note_skipped();
                continue;
            }
            self.immediate(model, *list, batch, meter);
        }
    }

    fn member_remove(
        &mut self,
        model: &Model,
        user: UserId,
        batch: &mut Batch<P>,
        meter: &mut Meter,
    ) {
        if let Some(slot) = self.members.find(user) {
            self.detach(model, slot, meter);
            self.id_order.remove(user, meter);
            self.masks.left(slot);
            self.members.remove(slot);
        }
        if self.updates == Updates::UpdatesOff {
            return;
        }
        let lists: Vec<ListKey> = self.sessions.lists().collect();
        for list in lists {
            self.immediate(model, list, batch, meter);
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn presence(
        &mut self,
        model: &Model,
        user: UserId,
        status: Status,
        change: Change,
        timing: Timing,
        batch: &mut Batch<P>,
        meter: &mut Meter,
    ) {
        let current = self.presence_of(user);
        self.write_presence(
            model,
            user,
            Presence {
                status,
                connected: current.connected,
            },
            meter,
        );
        let member = self.members.contains(user);
        if change == Change::Unchanged && member {
            let lists: Vec<ListKey> = self
                .pending
                .marks()
                .filter(|(_, m)| *m == Mark::Synced)
                .map(|(l, _)| l)
                .collect();
            let rows: BTreeSet<ListKey> = lists
                .into_iter()
                .filter(|l| self.is_row(model, *l, user))
                .collect();
            self.pending.invalidate(|l| rows.contains(&l));
            return;
        }
        let lists: Vec<ListKey> = self.sessions.lists().collect();
        for list in lists {
            if member && !self.is_row(model, list, user) {
                self.note_skipped();
                continue;
            }
            match timing {
                Timing::Immediate => self.immediate(model, list, batch, meter),
                Timing::Deferred => self.pending.mark(list, Mark::Sync),
            }
        }
    }

    fn connection(
        &mut self,
        model: &Model,
        user: UserId,
        connection: Connection,
        meter: &mut Meter,
    ) {
        let current = self.presence_of(user);
        let connected = matches!(connection, Connection::Connected { .. });
        let next = Presence {
            status: current.status,
            connected,
        };
        match self.members.find(user) {
            Some(slot) => self.set_presence(model, slot, next, meter),
            None => self.members.set_outsider(user, next),
        }
        let mark = match connection {
            Connection::Connected {
                presence: Change::Changed,
            } => Mark::Synced,
            _ => Mark::Sync,
        };
        let lists: Vec<ListKey> = self.sessions.lists().collect();
        for list in lists {
            if self.is_row(model, list, user) {
                self.pending.mark(list, mark);
            } else {
                self.note_skipped();
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn subscribe(
        &mut self,
        model: &Model,
        session: &SessionKey,
        pid: P,
        user: UserId,
        channel: ChannelId,
        ranges: &Ranges,
        sync: &Ranges,
        batch: &mut Batch<P>,
        meter: &mut Meter,
    ) {
        let allowed = self.members.find(user).is_some_and(|slot| {
            model.class_of_id(channel).is_some_and(|class| {
                model
                    .visible(class, self.members.roleset(slot), user)
                    .contains(recipient_bits())
            })
        });
        if !allowed {
            self.stats.gate_drops = self.stats.gate_drops.saturating_add(1);
            return;
        }
        if ranges.is_empty() {
            if self.sessions.unsubscribe(session, channel) {
                self.reconcile_columns(model, meter);
            }
            return;
        }
        self.sessions
            .subscribe(session, pid, user, channel, ranges.clone());
        self.reconcile_columns(model, meter);
        if self.updates == Updates::UpdatesOn && !sync.is_empty() {
            let body = self.body(model, channel, sync, meter);
            batch.dispatches.push(Dispatch {
                target: Target::Session(pid),
                body,
            });
        }
    }

    fn view_gained(
        &mut self,
        model: &Model,
        session: &SessionKey,
        channel: ChannelId,
        batch: &mut Batch<P>,
        meter: &mut Meter,
    ) {
        let Some(found) = self.sessions.get(session) else {
            return;
        };
        if found.list() != Some(channel) || found.ranges().is_empty() {
            return;
        }
        let (pid, user, ranges) = (found.pid(), found.user(), found.ranges().clone());
        let sees = self.members.find(user).is_some_and(|slot| {
            model.class_of_id(channel).is_some_and(|class| {
                model
                    .visible(class, self.members.roleset(slot), user)
                    .contains(recipient_bits())
            })
        });
        if !sees {
            return;
        }
        let body = self.body(model, channel, &ranges, meter);
        batch.dispatches.push(Dispatch {
            target: Target::Session(pid),
            body,
        });
    }

    fn immediate(&mut self, model: &Model, list: ListKey, batch: &mut Batch<P>, meter: &mut Meter) {
        let mut lists = self.pending.take_immediate();
        lists.push(list);
        lists.sort();
        lists.dedup();
        for l in lists {
            self.dispatch_list(model, l, batch, meter);
        }
    }

    fn sync_all(&mut self, model: &Model, batch: &mut Batch<P>, meter: &mut Meter) {
        let mut lists: Vec<ListKey> = self.sessions.lists().collect();
        if lists.is_empty() {
            return;
        }
        lists.extend(self.pending.take_immediate());
        lists.sort();
        lists.dedup();
        for l in lists {
            self.dispatch_list(model, l, batch, meter);
        }
    }

    fn dispatch_list(
        &mut self,
        model: &Model,
        list: ListKey,
        batch: &mut Batch<P>,
        meter: &mut Meter,
    ) {
        let channel = list.channel();
        let class = model.class_of_id(channel);
        let mut receivers: Vec<(&Ranges, P)> = Vec::new();
        for (_, session) in self.sessions.subscribers(list) {
            meter.charge(25);
            let receives = class.is_some_and(|class| {
                self.members.find(session.user()).is_some_and(|slot| {
                    model
                        .visible(class, self.members.roleset(slot), session.user())
                        .contains(recipient_bits())
                })
            });
            if receives {
                receivers.push((session.ranges(), session.pid()));
            }
        }
        receivers.sort_by(|a, b| a.0.cmp(b.0));
        let groups: Vec<(Ranges, Vec<P>)> = receivers
            .chunk_by(|a, b| a.0 == b.0)
            .filter_map(|run| {
                let (ranges, _) = run.first()?;
                Some(((*ranges).clone(), run.iter().map(|(_, pid)| *pid).collect()))
            })
            .collect();
        for (ranges, pids) in groups {
            let body = self.body(model, channel, &ranges, meter);
            batch.dispatches.push(Dispatch {
                target: Target::Sessions(pids),
                body,
            });
        }
    }

    fn offline_builds(&self, model: &Model, reach: u64) -> u64 {
        let classes: BTreeSet<ClassId> = self
            .sessions
            .lists()
            .filter_map(|list| model.class_of_id(list.channel()))
            .collect();
        classes
            .into_iter()
            .filter(|class| {
                u64::from(self.counts_of(model, *class, &mut Meter::new()).offline()) <= reach
            })
            .count() as u64
    }

    fn counts_of(&self, model: &Model, class: ClassId, meter: &mut Meter) -> ClassCounts {
        class_counts(
            model,
            class,
            &self.sets,
            &self.members,
            &self.sections,
            meter,
        )
    }

    fn body(
        &mut self,
        model: &Model,
        channel: ChannelId,
        ranges: &Ranges,
        meter: &mut Meter,
    ) -> SyncBody {
        let class = model.class_of_id(channel);
        let counts = class
            .map(|c| self.counts_of(model, c, meter))
            .unwrap_or_default();
        let col = class.map(|c| self.ensure_column(model, c, meter));
        let threshold = self.config.offline_threshold;
        let offline = counts.offline();
        let hidden = offline > threshold;
        if let (Some(col), Some(class), false) = (col, class, hidden)
            && self.columns.get(col).is_some_and(|c| c.offline().is_none())
        {
            let vis = Vis {
                model,
                members: &self.members,
                table: &self.columns,
            };
            let built = build_offline(&vis, class, meter);
            self.columns.set_offline(col, Some(built));
            self.stats.offline_rebuilds = self.stats.offline_rebuilds.saturating_add(1);
        }
        let labels = self.sections.labels();
        let mut groups: Vec<Group> = labels
            .iter()
            .zip(counts.sections.iter())
            .filter(|(_, c)| **c > 0)
            .map(|(label, count)| Group {
                label: *label,
                count: *count,
            })
            .collect();
        if offline > 0 && !hidden {
            groups.push(Group {
                label: Label::Offline,
                count: offline,
            });
        }
        let ops = ranges
            .iter()
            .map(|range| SyncOp {
                range,
                rows: match col {
                    Some(col) => self.rows(col, &counts, hidden, range, meter),
                    None => Vec::new(),
                },
            })
            .collect();
        SyncBody {
            channel,
            member_count: counts.total,
            online_count: counts.online,
            groups,
            ops,
        }
    }

    fn rows(
        &self,
        col: ColId,
        counts: &ClassCounts,
        hidden: bool,
        range: Range,
        meter: &mut Meter,
    ) -> Vec<Row> {
        let (start, end) = (range.start(), range.end());
        let span = end.saturating_sub(start).saturating_add(1) as usize;
        let mut rows = Vec::with_capacity(span);
        let mut pos = 0u32;
        let mut slots = Vec::with_capacity(span);
        let labels = self.sections.labels();
        let mut sections: Vec<(Label, u32, Option<&BlockedList>)> = labels
            .iter()
            .zip(counts.sections.iter())
            .enumerate()
            .map(|(i, (l, c))| (*l, *c, self.sections.list(i)))
            .collect();
        if !hidden {
            sections.push((Label::Offline, counts.offline(), None));
        }
        for (label, count, list) in sections {
            meter.charge(2);
            if count == 0 {
                continue;
            }
            if pos > end {
                break;
            }
            let last = pos.saturating_add(count);
            if last < start {
                pos = last.saturating_add(1);
                continue;
            }
            if pos >= start {
                rows.push(Row::Group(Group { label, count }));
            }
            let first_member = pos.saturating_add(1);
            let from = start.max(first_member);
            let to = end.min(last);
            if from <= to {
                let skip = from.saturating_sub(first_member);
                let take = to.saturating_sub(from).saturating_add(1);
                slots.clear();
                match list {
                    Some(list) => list.walk(Some(col), skip, take, &mut slots, meter),
                    None => {
                        let offline = self
                            .columns
                            .get(col)
                            .and_then(|c| c.offline())
                            .unwrap_or_default();
                        let s = skip as usize;
                        let e = s.saturating_add(take as usize).min(offline.len());
                        slots.extend_from_slice(offline.get(s..e).unwrap_or_default());
                        meter.charge(u64::from(take).saturating_mul(3));
                    }
                }
                rows.extend(slots.iter().map(|s| Row::Member(self.members.user(*s))));
            }
            pos = last.saturating_add(1);
        }
        rows
    }

    fn idle(&self) -> Result<(), CoreError> {
        match self.job {
            Some(_) => Err(bad("a render is held")),
            None => Ok(()),
        }
    }

    fn step(
        &mut self,
        job: Job<P>,
        run: impl FnOnce(&mut Ctx<'_>, &mut Job<P>) -> Result<Step<P>, FillError>,
    ) -> Result<Step<P>, CoreError> {
        let mut job = job;
        let mut ctx = Ctx {
            guild: self.guild,
            members: &self.members,
            fragments: &mut self.fragments,
            payloads: &mut self.payloads,
            budget: Budget::new(Units::new(self.config.work_budget_units)),
            meter: Meter::new(),
        };
        let ran = run(&mut ctx, &mut job);
        self.render_spent = ctx.meter.spent();
        match ran {
            Ok(step) => {
                if !matches!(step, Step::Done(_)) {
                    self.job = Some(job);
                }
                Ok(step)
            }
            Err(e) => {
                self.fragments.trim();
                Err(CoreError::BadInput(match e {
                    FillError::TooMany => "items: more than 1000".to_string(),
                    FillError::Busy => "items: no render waits for items".to_string(),
                }))
            }
        }
    }

    fn held(&mut self) -> Result<Job<P>, CoreError> {
        self.job.take().ok_or_else(|| bad("no render is held"))
    }

    pub fn render(&mut self, effects: Effects<P>) -> Result<Step<P>, CoreError> {
        self.idle()?;
        let job = Job::sync(effects.dispatches, effects.timer, effects.skipped_absent);
        self.step(job, |ctx, job| Ok(render::drive(ctx, job)))
    }

    pub fn page(&mut self, query: PageQuery) -> Result<Step<P>, CoreError> {
        self.idle()?;
        let ids = match query {
            PageQuery::Offset { offset, limit } => self.page_ids(offset, limit),
            PageQuery::After { after, limit } => self.page_after(after, limit),
        };
        let job = Job::page(ids, self.members.len());
        self.step(job, |ctx, job| Ok(render::drive(ctx, job)))
    }

    pub fn render_spent(&self) -> Units {
        self.render_spent
    }

    pub fn fill(&mut self, items: Vec<EncodedItem>) -> Result<Step<P>, CoreError> {
        let job = self.held()?;
        self.step(job, |ctx, job| render::fill(ctx, job, items))
    }

    pub fn masks_set(&mut self, updates: &[MaskUpdate]) -> Result<(), CoreError> {
        if updates.len() > crate::masks::MAX_UPDATES {
            return Err(bad("more than 4096 mask updates"));
        }
        self.masks.set(&self.members, updates);
        Ok(())
    }

    pub fn masks_query(&self, epoch: u64) -> MaskReply {
        if epoch != self.last.epoch().get() {
            return MaskReply::Stale;
        }
        self.masks.query(&self.members)
    }

    pub fn accounting(&self) -> Accounting {
        Accounting {
            fragments: self.fragments.bytes() as u64,
            payloads: self.payloads.bytes() as u64,
            core: self
                .stats()
                .bytes
                .saturating_sub(self.fragments.bytes() as u64)
                .saturating_sub(self.payloads.bytes() as u64),
        }
    }

    pub fn resume(&mut self) -> Result<Step<P>, CoreError> {
        let job = self.held()?;
        if job.is_waiting() {
            self.job = Some(job);
            return Err(bad("the held render waits for items"));
        }
        self.step(job, |ctx, job| Ok(render::drive(ctx, job)))
    }

    pub fn session_ranges(&self, session: &SessionKey) -> Option<(ChannelId, Ranges)> {
        let found = self.sessions.get(session)?;
        found.list().map(|c| (c, found.ranges().clone()))
    }

    pub fn channel_counts(
        &self,
        channels: &[ChannelId],
        mode: Mode,
    ) -> Result<Vec<(ChannelId, u32, u32)>, CoreError> {
        let model = Arc::clone(&self.last);
        let per = self.shape(&model, 0).counts();
        let estimate = per.times(channels.len() as u64);
        if mode == Mode::Normal && !self.budget().fits(estimate) {
            return Err(CoreError::NeedsDirty);
        }
        let mut meter = Meter::new();
        Ok(channels
            .iter()
            .map(|c| {
                let counts = model
                    .class_of_id(*c)
                    .map(|class| self.counts_of(&model, class, &mut meter))
                    .unwrap_or_default();
                (*c, counts.total, counts.online)
            })
            .collect())
    }

    pub fn guild_counts(&self) -> (u32, u32) {
        (self.members.len(), self.members.online())
    }

    pub fn is_row_of(&self, channel: ChannelId, user: UserId) -> bool {
        self.is_row(&self.last, ListKey::new(channel), user)
    }

    pub fn page_ids(&self, offset: u32, limit: u32) -> Vec<UserId> {
        let mut out = Vec::with_capacity(limit.min(self.members.len()).min(100_000) as usize);
        self.id_order
            .page(offset, limit, &mut out, &mut Meter::new());
        out
    }

    pub fn page_after(&self, after: Option<UserId>, limit: u32) -> Vec<UserId> {
        let mut out = Vec::with_capacity(limit.min(1000) as usize);
        self.id_order
            .page_after(after, limit, &mut out, &mut Meter::new());
        out
    }

    pub fn subscription_count(&self) -> usize {
        self.sessions.len()
    }

    pub fn subscriptions(&self) -> Vec<Subscription<P>> {
        self.sessions.subscriptions()
    }

    pub fn pending_marks(&self) -> Vec<(ListKey, Mark)> {
        self.pending.marks().collect()
    }

    pub fn timer_armed(&self) -> bool {
        self.pending.is_armed()
    }

    pub fn render_session(&mut self, session: &SessionKey) -> Option<SyncBody> {
        let found = self.sessions.get(session)?;
        let channel = found.list()?;
        let ranges = found.ranges().clone();
        let model = Arc::clone(&self.last);
        Some(self.body(&model, channel, &ranges, &mut Meter::new()))
    }

    pub fn stats(&self) -> Stats {
        let bytes = self
            .members
            .bytes()
            .saturating_add(self.sets.bytes())
            .saturating_add(self.sections.bytes())
            .saturating_add(self.id_order.bytes())
            .saturating_add(self.columns.bytes())
            .saturating_add(self.sessions.bytes())
            .saturating_add(self.fragments.bytes())
            .saturating_add(self.payloads.bytes())
            .saturating_add(self.masks.bytes())
            .saturating_add(core::mem::size_of::<Core<P>>());
        Stats {
            bytes: bytes as u64,
            members: u64::from(self.members.len()),
            online: u64::from(self.members.online()),
            classes: self.last.classes().len() as u64,
            columns: self.columns.len() as u64,
            fragment_bytes: self.fragments.bytes() as u64,
            prefix_ties: self.members.prefix_ties(),
            ..self.stats
        }
    }

    pub fn check(&self) -> Result<(), String> {
        self.fragments.check(self.job.is_none())?;
        self.payloads.check()?;
        let perms = Arc::clone(&self.perms);
        let view = perms.view().map_err(|e| format!("{e:?}"))?;
        let model = &self.last;
        let mut ids: Vec<u64> = Vec::new();
        let mut online = 0u32;
        let mut recount = SetCounts::new();
        for slot in self.members.slots() {
            let user = self.members.user(slot);
            if self.members.find(user) != Some(slot) {
                return Err(format!(
                    "index does not map user {} to slot {}",
                    user.get(),
                    slot.get()
                ));
            }
            ids.push(user.get());
            let is_online = self.members.is_online(slot);
            let presence = self.members.presence(slot);
            if is_online != presence.status.shown_online(presence.connected) {
                return Err(format!(
                    "online flag of user {} disagrees with presence",
                    user.get()
                ));
            }
            if is_online {
                online = online.saturating_add(1);
            }
            recount.add(self.members.roleset(slot), is_online);
            if view.roleset(user) != Some(self.members.roleset(slot)) {
                return Err(format!(
                    "role set of user {} differs from the permission index",
                    user.get()
                ));
            }
        }
        if ids.len() != self.members.len() as usize || online != self.members.online() {
            return Err("member or online totals differ from a recount".into());
        }
        if view.members().len() != ids.len() {
            return Err(format!(
                "member set size {} differs from the permission index {}",
                ids.len(),
                view.members().len()
            ));
        }
        ids.sort_unstable();
        if self.id_order.iter().collect::<Vec<u64>>() != ids {
            return Err("id order differs from the sorted member ids".into());
        }
        self.id_order.check()?;
        for (set, total, on) in recount.live() {
            if self.sets.total(set) != total || self.sets.online(set) != on {
                return Err(format!(
                    "role set {} counts differ from a recount",
                    set.get()
                ));
            }
        }
        for (set, total, _) in self.sets.live() {
            if recount.total(set) != total {
                return Err(format!("role set {} counted but has no members", set.get()));
            }
            if view.members().refcount(set) != total {
                return Err(format!(
                    "role set {} total differs from the permission refcount",
                    set.get()
                ));
            }
        }
        if self.sections.hoisted() != hoist_order(model) {
            return Err("hoist order stale".into());
        }
        let vis = Vis {
            model,
            members: &self.members,
            table: &self.columns,
        };
        let mut placed = 0u32;
        for (at, list) in self.sections.lists().iter().enumerate() {
            list.check(&self.members, &vis)
                .map_err(|e| format!("section {at}: {e}"))?;
            for slot in list.slots() {
                if !self.members.is_online(slot) {
                    return Err(format!("offline slot {} in an online section", slot.get()));
                }
                if self.sections.known_section(self.members.roleset(slot)) != at {
                    return Err(format!(
                        "slot {} in section {at} but belongs elsewhere",
                        slot.get()
                    ));
                }
            }
            placed = placed.saturating_add(list.len());
        }
        if placed != online {
            return Err(format!("{placed} slots in sections but {online} online"));
        }
        let mut readers: BTreeMap<ClassId, u32> = BTreeMap::new();
        for (channel, count) in self.sessions.readers() {
            if let Some(class) = model.class_of_id(channel) {
                let e = readers.entry(class).or_insert(0);
                *e = e.saturating_add(count as u32);
            }
        }
        for (col, column) in self.columns.iter() {
            if readers.get(&column.class()) != Some(&column.readers()) {
                return Err(format!("column {} readers stale", col.index()));
            }
            let class = column.class();
            let counts = class_counts(
                model,
                class,
                &self.sets,
                &self.members,
                &self.sections,
                &mut Meter::new(),
            );
            let mut total = 0u32;
            let mut on = 0u32;
            let mut sec = vec![0u32; self.sections.len()];
            let mut off: Vec<Slot> = Vec::new();
            for slot in self.members.slots() {
                if vis.class_sees(class, slot) {
                    total = total.saturating_add(1);
                    if self.members.is_online(slot) {
                        on = on.saturating_add(1);
                        let s = self.sections.known_section(self.members.roleset(slot));
                        if let Some(c) = sec.get_mut(s) {
                            *c = c.saturating_add(1);
                        }
                    } else {
                        off.push(slot);
                    }
                }
            }
            if counts.total != total
                || counts.online != on
                || counts.sections.as_slice() != sec.as_slice()
            {
                return Err(format!(
                    "class {} sums {:?} differ from a recount {total} {on} {sec:?}",
                    class.get(),
                    counts
                ));
            }
            for (at, list) in self.sections.lists().iter().enumerate() {
                if Some(&list.count(col)) != sec.get(at) {
                    return Err(format!("column {} section {at} count differs", col.index()));
                }
            }
            if column.offline_count() as usize != off.len() {
                return Err(format!(
                    "column {} offline count {} differs from a recount {}",
                    col.index(),
                    column.offline_count(),
                    off.len()
                ));
            }
            if let Some(vec) = column.offline() {
                if vec.len() > self.config.offline_drop as usize {
                    return Err(format!(
                        "offline vector of column {} holds {} above offline_drop",
                        col.index(),
                        vec.len()
                    ));
                }
                off.sort_by(|a, b| self.members.cmp(*a, *b));
                if !check_sorted(vec, &self.members) || vec != off.as_slice() {
                    return Err(format!("offline vector of column {} stale", col.index()));
                }
            }
        }
        for class in readers.keys() {
            if self.columns.of_class(*class).is_none() {
                return Err(format!("class {} has readers but no column", class.get()));
            }
        }
        Ok(())
    }
}

fn keep_last_by_key<T: Copy, K: Ord>(items: &mut Vec<T>, key: impl Fn(&T) -> K) {
    items.sort_by_key(&key);
    items.dedup_by(|later, kept| {
        let same = key(later) == key(kept);
        if same {
            *kept = *later;
        }
        same
    });
}

fn casefold_differs(members: &Members, slot: Slot, display_name: &[u8]) -> bool {
    let folded = crate::casefold::fold(display_name);
    let clipped = folded
        .get(..crate::names::MAX_KEY.min(folded.len()))
        .unwrap_or_default();
    clipped != members.key(slot)
}

#[cfg(test)]
mod tests {
    use super::keep_last_by_key;
    use std::collections::BTreeMap;

    #[test]
    fn keep_last_by_key_matches_map_inserts() {
        let mut state = 0x2545_f491_4f6c_dd1du64;
        for _ in 0..500 {
            let mut items: Vec<(u8, u32)> = Vec::new();
            let mut map: BTreeMap<u8, u32> = BTreeMap::new();
            for value in 0..(state % 40) as u32 {
                state ^= state << 13;
                state ^= state >> 7;
                state ^= state << 17;
                let key = (state % 12) as u8;
                items.push((key, value));
                map.insert(key, value);
            }
            keep_last_by_key(&mut items, |(key, _)| *key);
            assert_eq!(items, map.into_iter().collect::<Vec<_>>());
        }
    }
}
