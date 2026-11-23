// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use smallvec::SmallVec;

use crate::budget::{Meter, Units};
use crate::channels::{Channel, ChannelTable, OverwriteTarget};
use crate::classes::{Classes, Content, Overrides};
use crate::diff::{self, Regroup, Side, VisibilityChange};
use crate::eval::{self, Who};
use crate::event::{ChannelCause, Event, RoleList, Snapshot};
use crate::exceptions::{Exceptions, VirtualView};
use crate::flags::{MemberFlags, Visible};
use crate::ids::{ChannelId, ChannelIdx, Epoch, RoleId, RoleIdx, RoleSetId, SigId, UserId};
use crate::members::{MemberEntry, MemberIndex};
use crate::model::{GuildFlags, Model, ViewableCache};
use crate::roles::RoleTable;
use crate::rolesets::{self, RoleSets};
use crate::signatures::{Planes, SCALAR_COST, Slicer};
use crate::viewsets::ViewSets;

pub(crate) const MEMBER_WRITE: u64 = 64;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Membership {
    pub(crate) roleset: RoleSetId,
    pub(crate) quarantined: bool,
}

impl Membership {
    pub(crate) fn of(entry: &MemberEntry) -> Membership {
        Membership {
            roleset: entry.roleset(),
            quarantined: entry.is_quarantined(),
        }
    }

    pub(crate) fn entry(self, user: UserId, exceptions: &Exceptions) -> MemberEntry {
        let mut flags = MemberFlags::empty();
        flags.set(MemberFlags::QUARANTINED, self.quarantined);
        flags.set(MemberFlags::EXCEPTION, exceptions.contains(user));
        MemberEntry::new(user, self.roleset, flags)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Rebase {
    None,
    Sets(Vec<RoleSetId>),
    All,
}

pub(crate) struct Plan {
    pub(crate) roles: Arc<RoleTable>,
    pub(crate) rolesets: Arc<RoleSets>,
    pub(crate) channels: Arc<ChannelTable>,
    pub(crate) owner: Option<UserId>,
    pub(crate) virtual_view: Arc<VirtualView>,
    pub(crate) flags: GuildFlags,
    pub(crate) edits: BTreeMap<UserId, Option<Membership>>,
    pub(crate) touched: BTreeSet<UserId>,
    refreshed: BTreeSet<UserId>,
    pub(crate) changed: bool,
    changed_roles: BTreeSet<RoleIdx>,
    everyone_changed: bool,
    dirty: BTreeSet<ChannelIdx>,
    parent_ids: BTreeSet<ChannelId>,
    relink: bool,
    exception_inputs: bool,
    prev_sets: usize,
    rebase: Rebase,
    reslice: bool,
    exceptions: Arc<Exceptions>,
    dirty_users: BTreeSet<UserId>,
    parents: BTreeSet<ChannelIdx>,
    planned: Units,
    estimate: Units,
}

impl Plan {
    fn from_model(model: &Model) -> Plan {
        Plan {
            roles: model.roles.clone(),
            rolesets: model.rolesets.clone(),
            channels: model.channels.clone(),
            owner: model.owner,
            virtual_view: model.virtual_view.clone(),
            flags: model.flags,
            edits: BTreeMap::new(),
            touched: BTreeSet::new(),
            refreshed: BTreeSet::new(),
            changed: false,
            changed_roles: BTreeSet::new(),
            everyone_changed: false,
            dirty: BTreeSet::new(),
            parent_ids: BTreeSet::new(),
            relink: false,
            exception_inputs: false,
            prev_sets: model.rolesets.len(),
            rebase: Rebase::None,
            reslice: false,
            exceptions: model.exceptions.clone(),
            dirty_users: BTreeSet::new(),
            parents: BTreeSet::new(),
            planned: Units::ZERO,
            estimate: Units::ZERO,
        }
    }

    pub(crate) fn membership(&self, index: &MemberIndex, user: UserId) -> Option<Membership> {
        match self.edits.get(&user) {
            Some(edit) => *edit,
            None => index.get(user).map(Membership::of),
        }
    }

    pub(crate) fn entry_after(&self, index: &MemberIndex, user: UserId) -> Option<MemberEntry> {
        self.membership(index, user)
            .map(|m| m.entry(user, &self.exceptions))
    }

    pub(crate) fn estimate(&self) -> Units {
        self.estimate
    }

    pub(crate) fn planned(&self) -> Units {
        self.planned
    }

    fn charge(&mut self, units: u64) {
        self.planned = self.planned.plus(Units::new(units));
    }
}

fn count(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

fn sorted(channels: &[ChannelId]) -> Vec<ChannelId> {
    let mut list = channels.to_vec();
    list.sort_unstable();
    list.dedup();
    list
}

fn member_targets(channel: &Channel) -> impl Iterator<Item = UserId> + '_ {
    channel.rules().members().iter().map(|(u, _)| *u)
}

pub(crate) fn plan(model: &Model, index: &MemberIndex, events: &[Event]) -> Plan {
    let mut plan = Plan::from_model(model);
    for event in events {
        plan_event(&mut plan, model, event);
    }
    finish(&mut plan, model, index);
    plan
}

fn plan_event(plan: &mut Plan, model: &Model, event: &Event) {
    match event {
        Event::Roles(specs) => {
            plan.charge(count(specs.len().saturating_add(plan.roles.len())).saturating_mul(4));
            let before = plan.roles.clone();
            Arc::make_mut(&mut plan.roles).replace(specs);
            if *plan.roles != *before {
                plan.changed = true;
            }
            for (idx, role) in plan.roles.iter() {
                if before.permissions(idx) != role.permissions() {
                    plan.changed_roles.insert(idx);
                    if idx == RoleIdx::EVERYONE {
                        plan.everyone_changed = true;
                    }
                }
            }
        }
        Event::Owner(owner) => {
            if plan.owner != *owner {
                plan.touched.extend(plan.owner);
                plan.touched.extend(*owner);
                plan.refreshed.extend(plan.owner);
                plan.refreshed.extend(*owner);
                plan.owner = *owner;
                plan.changed = true;
                plan.exception_inputs = true;
            }
        }
        Event::ChannelUpsert(spec, _) => {
            plan.charge(
                count(spec.overwrites.len())
                    .saturating_mul(4)
                    .saturating_add(16),
            );
            let roles_before = plan.roles.len();
            let channel = Channel::compile(model.guild, spec, Arc::make_mut(&mut plan.roles));
            if plan.roles.len() != roles_before {
                plan.changed = true;
            }
            let old = plan.channels.by_id(spec.id).cloned();
            if old.as_ref() == Some(&channel) {
                return;
            }
            plan.changed = true;
            if let Some(old) = &old {
                plan.parent_ids.extend(old.parent_id());
                if member_targets(old).next().is_some() {
                    plan.exception_inputs = true;
                }
            }
            plan.parent_ids.extend(channel.parent_id());
            if member_targets(&channel).next().is_some() {
                plan.exception_inputs = true;
            }
            let structural = old.as_ref().is_none_or(|o| {
                o.rules() != channel.rules()
                    || o.kind() != channel.kind()
                    || o.parent_id() != channel.parent_id()
            });
            let idx = Arc::make_mut(&mut plan.channels).upsert(channel);
            if structural {
                plan.dirty.insert(idx);
                plan.relink = true;
            }
        }
        Event::ChannelDelete(id) => {
            let old = plan.channels.by_id(*id).cloned();
            if let Some(idx) = Arc::make_mut(&mut plan.channels).remove(*id) {
                plan.changed = true;
                plan.dirty.insert(idx);
                plan.relink = true;
                if let Some(old) = old {
                    plan.parent_ids.extend(old.parent_id());
                    if member_targets(&old).next().is_some() {
                        plan.exception_inputs = true;
                    }
                }
            }
        }
        Event::ChannelOrder(ids) => {
            plan.charge(count(ids.len().saturating_add(plan.channels.slots())).saturating_mul(2));
            let before: Vec<ChannelIdx> = plan.channels.order().to_vec();
            let mut next = (*plan.channels).clone();
            next.reorder(ids);
            if next.order() != before.as_slice() {
                plan.channels = Arc::new(next);
                plan.changed = true;
                plan.relink = true;
            }
        }
        Event::MemberUpsert(user, list) => plan_member(plan, *user, list),
        Event::MemberRemove(user) => {
            plan.edits.insert(*user, None);
            plan.touched.insert(*user);
        }
        Event::VirtualAccess(user, channels) => {
            let list = sorted(channels);
            if plan.virtual_view.get(*user) != list.as_slice() {
                Arc::make_mut(&mut plan.virtual_view).set(*user, &list);
                plan.touched.insert(*user);
                plan.refreshed.insert(*user);
                plan.changed = true;
                plan.exception_inputs = true;
            }
        }
        Event::HistoryCutoff(cutoff) => {
            if plan.flags.cutoff != *cutoff {
                plan.flags.cutoff = *cutoff;
                plan.changed = true;
            }
        }
        Event::GuildNsfw(nsfw, level) => {
            if plan.flags.nsfw != *nsfw || plan.flags.nsfw_level != *level {
                plan.flags.nsfw = *nsfw;
                plan.flags.nsfw_level = *level;
                plan.changed = true;
            }
        }
    }
}

fn plan_member(plan: &mut Plan, user: UserId, list: &RoleList) {
    plan.touched.insert(user);
    let next = match list {
        RoleList::Malformed => Membership {
            roleset: RoleSetId::EVERYONE,
            quarantined: true,
        },
        RoleList::Roles(ids) => {
            plan.charge(count(ids.len()).saturating_mul(8).saturating_add(16));
            let mut idxs: SmallVec<[RoleIdx; 4]> = SmallVec::with_capacity(ids.len());
            for id in ids {
                let idx = match plan.roles.resolve(*id) {
                    Some(idx) => idx,
                    None => {
                        plan.changed = true;
                        Arc::make_mut(&mut plan.roles).intern(*id)
                    }
                };
                idxs.push(idx);
            }
            let set = rolesets::normalize(idxs);
            let roleset = match plan.rolesets.find(&set) {
                Some(id) => id,
                None => {
                    plan.changed = true;
                    let roles = plan.roles.clone();
                    Arc::make_mut(&mut plan.rolesets)
                        .intern(&set, &roles)
                        .unwrap_or(RoleSetId::EVERYONE)
                }
            };
            Membership {
                roleset,
                quarantined: false,
            }
        }
    };
    plan.edits.insert(user, Some(next));
}

fn finish(plan: &mut Plan, model: &Model, index: &MemberIndex) {
    if plan.relink {
        plan.charge(count(plan.channels.slots()).saturating_mul(16));
        Arc::make_mut(&mut plan.channels).relink();
    }
    for id in &plan.parent_ids {
        if let Some(idx) = plan
            .channels
            .resolve(*id)
            .or_else(|| model.channels.resolve(*id))
        {
            plan.parents.insert(idx);
        }
    }
    for idx in &plan.dirty {
        plan.parents.insert(*idx);
    }

    let new_sets: Vec<RoleSetId> = (plan.prev_sets..plan.rolesets.len())
        .filter_map(RoleSetId::from_index)
        .collect();
    plan.rebase = if plan.everyone_changed {
        Rebase::All
    } else if plan.changed_roles.is_empty() {
        if new_sets.is_empty() {
            Rebase::None
        } else {
            Rebase::Sets(new_sets)
        }
    } else {
        let roles: Vec<RoleIdx> = plan.changed_roles.iter().copied().collect();
        plan.charge(count(plan.rolesets.len()).saturating_mul(8));
        let mut sets = plan.rolesets.holding_any(&roles);
        sets.extend(new_sets);
        sets.sort_unstable();
        sets.dedup();
        Rebase::Sets(sets)
    };
    let roles = plan.roles.clone();
    match plan.rebase.clone() {
        Rebase::All => {
            plan.charge(count(plan.rolesets.len()).saturating_mul(8));
            Arc::make_mut(&mut plan.rolesets).rebase_all(&roles);
        }
        Rebase::Sets(sets) if !plan.changed_roles.is_empty() => {
            plan.charge(count(sets.len()).saturating_mul(8));
            Arc::make_mut(&mut plan.rolesets).rebase(&sets, &roles);
        }
        Rebase::Sets(_) | Rebase::None => {}
    }

    let quarantine_moved = plan.edits.iter().any(|(user, edit)| {
        edit.is_some_and(|m| m.quarantined)
            || index.get(*user).is_some_and(MemberEntry::is_quarantined)
    });
    if plan.exception_inputs || quarantine_moved {
        let mut quarantined: BTreeSet<UserId> = index.quarantined().collect();
        for (user, edit) in &plan.edits {
            match edit {
                Some(m) if m.quarantined => {
                    quarantined.insert(*user);
                }
                _ => {
                    quarantined.remove(user);
                }
            }
        }
        let exceptions =
            Exceptions::build(plan.owner, &plan.channels, &plan.virtual_view, quarantined);
        plan.charge(
            count(exceptions.len().saturating_add(model.exceptions.len())).saturating_mul(4),
        );
        if exceptions != *model.exceptions {
            plan.exceptions = Arc::new(exceptions);
            plan.changed = true;
        }
    }

    let old_users = model.exceptions.users();
    let new_users = plan.exceptions.users();
    let mut dirty: BTreeSet<UserId> = BTreeSet::new();
    for user in old_users.iter().chain(new_users) {
        if old_users.binary_search(user).is_err() || new_users.binary_search(user).is_err() {
            dirty.insert(*user);
        }
    }
    for user in &plan.touched {
        if (old_users.binary_search(user).is_ok() || new_users.binary_search(user).is_ok())
            && (plan.edits.get(user).copied() != Some(index.get(*user).map(Membership::of))
                || plan.refreshed.contains(user))
        {
            dirty.insert(*user);
        }
    }
    match &plan.rebase {
        Rebase::All => dirty.extend(new_users.iter().copied()),
        Rebase::Sets(sets) => {
            for user in new_users {
                if plan
                    .membership(index, *user)
                    .is_some_and(|m| sets.binary_search(&m.roleset).is_ok())
                {
                    dirty.insert(*user);
                }
            }
        }
        Rebase::None => {}
    }
    if !dirty.is_empty() {
        plan.changed = true;
    }
    plan.dirty_users = dirty;
    plan.estimate = estimate(plan, model);
}

struct Counts {
    sets: u64,
    words: u64,
    slots: u64,
    live: u64,
    kids: u64,
    roles: u64,
    categories: u64,
}

impl Counts {
    fn of(plan: &Plan) -> Counts {
        let kids = plan
            .channels
            .live()
            .map(|(idx, _)| count(plan.channels.children(idx).len()))
            .fold(0u64, u64::saturating_add);
        Counts {
            sets: count(plan.rolesets.len()),
            words: count(plan.rolesets.words()),
            slots: count(plan.channels.slots()),
            live: count(plan.channels.len()),
            kids,
            roles: count(plan.roles.len()),
            categories: count(
                plan.channels
                    .live()
                    .filter(|(_, ch)| ch.kind().is_category())
                    .count(),
            ),
        }
    }
}

fn reslice_wins(targets: u64, sigs: u64, grants: u64, c: &Counts) -> bool {
    let scalar = targets
        .saturating_mul(SCALAR_COST)
        .saturating_mul(sigs)
        .saturating_add(sigs.saturating_mul(c.words).saturating_mul(3));
    let slice = c
        .sets
        .saturating_mul(8)
        .saturating_add(c.roles.saturating_mul(c.words))
        .saturating_add(
            c.words
                .saturating_mul(grants.saturating_add(sigs.saturating_mul(4))),
        );
    slice < scalar
}

fn estimate(plan: &mut Plan, model: &Model) -> Units {
    let edits = count(plan.edits.len()).saturating_mul(MEMBER_WRITE);
    let regrouped = count(plan.touched.union(&plan.dirty_users).count());
    let c = Counts::of(plan);
    let pair_scan = count(model.channels.len())
        .saturating_add(c.live)
        .saturating_mul(2);
    let va_total = |view: &VirtualView| {
        view.users()
            .map(|u| count(view.get(u).len()))
            .fold(0u64, u64::saturating_add)
    };
    let va_pair = va_total(&model.virtual_view).saturating_add(va_total(&plan.virtual_view));
    let regroups = regrouped
        .saturating_mul(pair_scan.saturating_add(8))
        .saturating_add(va_pair);
    let base = plan
        .planned
        .get()
        .saturating_add(edits)
        .saturating_add(regroups);
    if !plan.changed {
        return Units::new(base);
    }

    let dirty_live: Vec<&Channel> = plan
        .dirty
        .iter()
        .filter_map(|idx| plan.channels.get(*idx))
        .collect();
    let dirty_grants = dirty_live
        .iter()
        .map(|ch| ch.rules().grant_count())
        .fold(0u64, u64::saturating_add);
    let sigs = count(model.signatures.len()).saturating_add(count(dirty_live.len()));
    let grants = model.signatures.grant_total().saturating_add(dirty_grants);
    let words_grew = model.rolesets.words() != plan.rolesets.words();
    let targets = match &plan.rebase {
        Rebase::All => c.sets,
        Rebase::Sets(s) => count(s.len()),
        Rebase::None => 0,
    };
    plan.reslice = matches!(plan.rebase, Rebase::All)
        || (targets > 0 && reslice_wins(targets, sigs, grants, &c));
    let wide = !matches!(plan.rebase, Rebase::None) || words_grew;
    let slicer = c
        .sets
        .saturating_mul(8)
        .saturating_add(c.roles.saturating_mul(c.words));
    let mut sig_work = c
        .words
        .saturating_mul(dirty_grants.saturating_add(count(dirty_live.len()).saturating_mul(4)));
    if !dirty_live.is_empty() || plan.reslice {
        sig_work = sig_work.saturating_add(slicer);
    }
    if plan.reslice {
        sig_work = sig_work.saturating_add(
            c.words
                .saturating_mul(grants.saturating_add(sigs.saturating_mul(4))),
        );
    } else if wide {
        sig_work = sig_work.saturating_add(
            sigs.saturating_mul(
                targets
                    .saturating_mul(SCALAR_COST)
                    .saturating_add(c.words.saturating_mul(3)),
            ),
        );
    }

    let (needed, needed_kids) = if wide {
        (c.live, c.kids)
    } else {
        let kids = plan
            .parents
            .iter()
            .map(|idx| count(plan.channels.children(*idx).len()))
            .fold(0u64, u64::saturating_add);
        (count(plan.parents.len()), kids)
    };
    let planes = needed
        .saturating_mul(4)
        .saturating_add(c.words.saturating_mul(needed.saturating_add(needed_kids)));

    let e_new = count(plan.exceptions.len());
    let e_old = count(model.exceptions.len());
    let users = e_new.saturating_add(e_old);
    let overrides = users
        .saturating_mul(8)
        .saturating_mul(needed.saturating_add(needed_kids))
        .saturating_add(
            count(plan.dirty_users.len()).saturating_mul(
                c.live
                    .saturating_mul(10)
                    .saturating_add(c.kids)
                    .saturating_add(c.slots),
            ),
        );

    let distinct = sigs
        .saturating_add(c.categories)
        .saturating_add(count(model.classes.capacity()))
        .min(c.slots);
    let classes = count(model.classes.capacity())
        .saturating_add(c.slots)
        .saturating_add(c.slots.saturating_mul(users.saturating_add(4)))
        .saturating_add(distinct.saturating_mul(c.words.saturating_mul(6)));

    let regular = !model.channels.has_irregular_parents() && !plan.channels.has_irregular_parents();
    let prev_sets = count(model.rolesets.len());
    let role_changes = if regular {
        let rebased = match &plan.rebase {
            Rebase::All => prev_sets,
            Rebase::Sets(s) => count(s.iter().filter(|set| set.index() < plan.prev_sets).count()),
            Rebase::None => 0,
        };
        let moved = count(plan.parents.len()).saturating_add(1);
        c.slots
            .saturating_mul(c.words)
            .saturating_add(count(plan.dirty.len()))
            .saturating_add(rebased.saturating_mul(c.slots))
            .saturating_add(prev_sets.saturating_mul(moved))
    } else {
        prev_sets.saturating_mul(pair_scan.saturating_add(4))
    };
    let views = e_old
        .saturating_mul(pair_scan.saturating_add(4))
        .saturating_add(va_pair);

    Units::new(
        base.saturating_add(sig_work)
            .saturating_add(planes)
            .saturating_add(overrides)
            .saturating_add(classes)
            .saturating_add(role_changes)
            .saturating_add(views),
    )
}

pub(crate) fn build(
    prev: &Model,
    plan: &Plan,
    index: &MemberIndex,
    epoch: Epoch,
    viewable_limit: usize,
    meter: &mut Meter,
) -> Model {
    let rolesets = plan.rolesets.clone();
    let channels = plan.channels.clone();
    let words = rolesets.words();
    let words_grew = prev.rolesets.words() != words;
    let wide = !matches!(plan.rebase, Rebase::None) || words_grew;

    let mut signatures = (*prev.signatures).clone();
    signatures.grow_channels(channels.slots());
    let make = |m: &mut Meter| Slicer::new(&rolesets, plan.roles.len(), m);
    let mut slicer = plan.reslice.then(|| make(meter));
    let mut fresh: BTreeSet<SigId> = BTreeSet::new();
    for idx in &plan.dirty {
        let channel = channels.get(*idx);
        if channel.is_some_and(|ch| signatures.keeps(*idx, ch.rules())) {
            continue;
        }
        signatures.detach(*idx);
        if let Some(channel) = channel
            && let Some((id, is_fresh)) =
                signatures.attach(*idx, channel.rules(), &mut slicer, &make, meter)
            && is_fresh
        {
            fresh.insert(id);
        }
    }
    let changed_sigs: BTreeSet<SigId> = match (&plan.rebase, slicer.as_ref()) {
        (_, Some(slicer)) if plan.reslice => signatures.reslice(&fresh, slicer, meter),
        (Rebase::All, _) => BTreeSet::new(),
        (Rebase::Sets(sets), _) => signatures.update_sets(sets, &rolesets, &fresh, meter),
        (Rebase::None, _) if words_grew => signatures.update_sets(&[], &rolesets, &fresh, meter),
        (Rebase::None, _) => BTreeSet::new(),
    };

    let sig_moved = |idx: ChannelIdx| {
        let now = signatures.of_channel(idx);
        now != prev.signatures.of_channel(idx) || now.is_some_and(|s| changed_sigs.contains(&s))
    };
    let mut planes: BTreeMap<ChannelIdx, Planes> = BTreeMap::new();
    for (idx, channel) in channels.live() {
        let needed = wide
            || plan.parents.contains(&idx)
            || sig_moved(idx)
            || (channel.kind().is_category()
                && channels.children(idx).iter().any(|c| sig_moved(*c)));
        if !needed {
            continue;
        }
        let children = channels.children(idx);
        meter.charge(4u64.saturating_add(
            count(words).saturating_mul(1u64.saturating_add(count(children.len()))),
        ));
        let own = signatures
            .planes_of(idx)
            .cloned()
            .unwrap_or_else(|| Planes::empty(words));
        let fresh_planes = if channel.kind().is_category() && !children.is_empty() {
            let mut view = own.view().to_vec();
            for child in children {
                if let Some(child_planes) = signatures.planes_of(*child) {
                    for (word, child_word) in view.iter_mut().zip(child_planes.view()) {
                        *word |= *child_word;
                    }
                }
            }
            if view.as_slice() == own.view() {
                own
            } else {
                own.with_view(view)
            }
        } else {
            own
        };
        let unchanged = prev
            .classes
            .class_of_channel(idx)
            .filter(|_| prev.channels.get(idx).is_some())
            .is_some_and(|class| class.planes().same_as(&fresh_planes));
        if !unchanged || plan.parents.contains(&idx) {
            planes.insert(idx, fresh_planes);
        }
    }

    let planes_at = |idx: ChannelIdx| -> Option<Planes> {
        match planes.get(&idx) {
            Some(p) => Some(p.clone()),
            None => prev
                .classes
                .class_of_channel(idx)
                .map(|c| c.planes().clone()),
        }
    };
    let full: BTreeSet<ChannelIdx> = planes.keys().copied().collect();
    let mut lists: BTreeMap<ChannelIdx, Overrides> =
        full.iter().map(|idx| (*idx, Overrides::new())).collect();

    let entry_of = |user: UserId| plan.entry_after(index, user);
    let mut full_planes: Vec<(ChannelIdx, Planes)> = Vec::with_capacity(full.len());
    for idx in &full {
        if let Some(p) = planes_at(*idx) {
            full_planes.push((*idx, p));
        }
    }
    if !full.is_empty() {
        for user in plan.exceptions.users() {
            let entry = entry_of(*user);
            let who = Who::of(plan.owner, &rolesets, *user, entry.as_ref());
            let virtual_list = plan.virtual_view.get(*user);
            if !who.is_member() && virtual_list.is_empty() {
                continue;
            }
            for (idx, p) in &full_planes {
                let bits = eval::session_bits(who, *user, virtual_list, &channels, *idx, meter);
                let reference = entry.map_or(Visible::empty(), |e| p.get(e.roleset()));
                if bits != reference
                    && let Some(list) = lists.get_mut(idx)
                {
                    list.push((*user, bits));
                }
            }
        }
    }

    let mut row: Vec<Visible> = Vec::new();
    for user in &plan.dirty_users {
        let entry = entry_of(*user);
        let who = Who::of(plan.owner, &rolesets, *user, entry.as_ref());
        let virtual_list = plan.virtual_view.get(*user);
        let active =
            plan.exceptions.contains(*user) && (who.is_member() || !virtual_list.is_empty());
        if active {
            eval::session_row(who, *user, virtual_list, &channels, &mut row, meter);
        }
        for (idx, _) in channels.live() {
            if full.contains(&idx) {
                continue;
            }
            meter.charge(1);
            let desired = if active {
                let bits = row.get(idx.index()).copied().unwrap_or(Visible::empty());
                let reference = match (entry, planes_at(idx)) {
                    (Some(e), Some(p)) => p.get(e.roleset()),
                    _ => Visible::empty(),
                };
                (bits != reference).then_some(bits)
            } else {
                None
            };
            let current = match lists.get(&idx) {
                Some(list) => lookup(list, *user),
                None => prev
                    .classes
                    .class_of_channel(idx)
                    .and_then(|c| c.override_of(*user)),
            };
            if current == desired {
                continue;
            }
            let list = lists.entry(idx).or_insert_with(|| {
                prev.classes
                    .class_of_channel(idx)
                    .map(|c| SmallVec::from_slice(c.overrides()))
                    .unwrap_or_default()
            });
            match list.binary_search_by_key(user, |(u, _)| *u) {
                Ok(i) => match desired {
                    Some(bits) => {
                        if let Some(slot) = list.get_mut(i) {
                            slot.1 = bits;
                        }
                    }
                    None => {
                        list.remove(i);
                    }
                },
                Err(i) => {
                    if let Some(bits) = desired {
                        list.insert(i, (*user, bits));
                    }
                }
            }
        }
    }

    let mut changes: BTreeMap<ChannelIdx, Option<Content>> = BTreeMap::new();
    for idx in &plan.dirty {
        if channels.get(*idx).is_none() {
            changes.insert(*idx, None);
        }
    }
    for (idx, list) in lists {
        let Some(p) = planes_at(idx) else { continue };
        changes.insert(
            idx,
            Some(Content {
                planes: p,
                overrides: list,
            }),
        );
    }
    let classes = if changes.is_empty() && channels.slots() == prev.channels.slots() {
        prev.classes.clone()
    } else {
        Arc::new(Classes::reassign(
            &prev.classes,
            changes,
            channels.slots(),
            meter,
        ))
    };

    Model {
        epoch,
        guild: prev.guild,
        owner: plan.owner,
        roles: plan.roles.clone(),
        rolesets: rolesets.clone(),
        channels,
        signatures: Arc::new(signatures),
        classes,
        exceptions: plan.exceptions.clone(),
        virtual_view: plan.virtual_view.clone(),
        flags: plan.flags,
        viewable: ViewableCache::new(rolesets.len(), viewable_limit),
        viewsets: ViewSets::new(viewable_limit),
    }
}

fn lookup(list: &[(UserId, Visible)], user: UserId) -> Option<Visible> {
    list.binary_search_by_key(&user, |(u, _)| *u)
        .ok()
        .and_then(|i| list.get(i))
        .map(|(_, v)| *v)
}

pub(crate) fn republish(prev: &Model, plan: &Plan, epoch: Epoch, viewable_limit: usize) -> Model {
    Model {
        epoch,
        guild: prev.guild,
        owner: plan.owner,
        roles: plan.roles.clone(),
        rolesets: plan.rolesets.clone(),
        channels: plan.channels.clone(),
        signatures: prev.signatures.clone(),
        classes: prev.classes.clone(),
        exceptions: plan.exceptions.clone(),
        virtual_view: plan.virtual_view.clone(),
        flags: plan.flags,
        viewable: ViewableCache::new(plan.rolesets.len(), viewable_limit),
        viewsets: ViewSets::new(viewable_limit),
    }
}

pub(crate) fn needs_build(prev: &Model, plan: &Plan) -> bool {
    !plan.dirty.is_empty()
        || !plan.dirty_users.is_empty()
        || !matches!(plan.rebase, Rebase::None)
        || prev.rolesets.words() != plan.rolesets.words()
}

pub(crate) fn regroups(
    before: &Model,
    after: &Model,
    index: &MemberIndex,
    plan: &Plan,
) -> Vec<Regroup> {
    let mut users: BTreeSet<UserId> = plan.touched.clone();
    users.extend(plan.dirty_users.iter().copied());
    users
        .into_iter()
        .filter_map(|user| {
            let old = diff::group_of(before, user, index.get(user));
            let new = diff::group_of(after, user, plan.entry_after(index, user).as_ref());
            (old != new).then_some(Regroup { user, old, new })
        })
        .collect()
}

pub(crate) fn visibility(
    before: &Model,
    after: &Model,
    index: &MemberIndex,
    plan: &Plan,
    regroups: &[Regroup],
    meter: &mut Meter,
) -> Vec<VisibilityChange> {
    let old_entry = |user: UserId| index.get(user).copied();
    let new_entry = |user: UserId| plan.entry_after(index, user);
    let old = Side {
        model: before,
        entry: &old_entry,
    };
    let new = Side {
        model: after,
        entry: &new_entry,
    };
    diff::visibility_changes(&old, &new, &|set| index.refcount(set), regroups, meter)
}

pub(crate) fn flag_changes(
    before: &Model,
    index: &MemberIndex,
    plan: &Plan,
) -> Vec<(UserId, MemberFlags)> {
    if Arc::ptr_eq(&before.exceptions, &plan.exceptions) {
        return Vec::new();
    }
    plan.dirty_users
        .iter()
        .filter(|user| !plan.edits.contains_key(user))
        .filter_map(|user| {
            index.get(*user).map(|entry| {
                (
                    *user,
                    Membership::of(entry).entry(*user, &plan.exceptions).flags(),
                )
            })
        })
        .collect()
}

pub(crate) fn snapshot_events(snapshot: &Snapshot) -> Vec<Event> {
    let mut events = Vec::with_capacity(
        snapshot
            .channels
            .len()
            .saturating_add(snapshot.members.len())
            .saturating_add(snapshot.virtual_access.len())
            .saturating_add(5),
    );
    events.push(Event::Roles(snapshot.roles.clone()));
    events.push(Event::Owner(snapshot.owner));
    for channel in &snapshot.channels {
        events.push(Event::ChannelUpsert(channel.clone(), ChannelCause::Created));
    }
    events.push(Event::ChannelOrder(
        snapshot.channels.iter().map(|c| c.id).collect(),
    ));
    for (user, roles) in &snapshot.members {
        events.push(Event::MemberUpsert(*user, roles.clone()));
    }
    for (user, channels) in &snapshot.virtual_access {
        events.push(Event::VirtualAccess(*user, channels.clone()));
    }
    events.push(Event::HistoryCutoff(snapshot.cutoff));
    events.push(Event::GuildNsfw(snapshot.nsfw, snapshot.nsfw_level));
    events
}

const DISTINCT_SCAN: usize = 4_096;

fn distinct(snapshot: &Snapshot) -> Option<(u64, u64)> {
    if snapshot.members.len() > DISTINCT_SCAN {
        return None;
    }
    let mut roles: Vec<RoleId> = snapshot.roles.iter().map(|r| r.id).collect();
    roles.extend(
        snapshot
            .channels
            .iter()
            .flat_map(|c| &c.overwrites)
            .filter_map(|o| match o.target {
                OverwriteTarget::Role(id) => Some(id),
                OverwriteTarget::Member(_) => None,
            }),
    );
    let mut lists: Vec<Vec<RoleId>> = Vec::with_capacity(snapshot.members.len());
    for (_, list) in &snapshot.members {
        if let RoleList::Roles(ids) = list {
            let mut ids = ids.clone();
            ids.sort_unstable();
            ids.dedup();
            roles.extend(ids.iter().copied());
            lists.push(ids);
        }
    }
    lists.sort_unstable();
    lists.dedup();
    roles.sort_unstable();
    roles.dedup();
    Some((count(lists.len()), count(roles.len())))
}

pub fn load_estimate(snapshot: &Snapshot) -> Units {
    let members = count(snapshot.members.len());
    let member_roles = snapshot
        .members
        .iter()
        .map(|(_, list)| match list {
            RoleList::Roles(ids) => count(ids.len()),
            RoleList::Malformed => 0,
        })
        .fold(0u64, u64::saturating_add);
    let quarantined = count(
        snapshot
            .members
            .iter()
            .filter(|(_, l)| *l == RoleList::Malformed)
            .count(),
    );
    let channels = count(snapshot.channels.len());
    let overwrites = snapshot
        .channels
        .iter()
        .map(|c| count(c.overwrites.len()))
        .fold(0u64, u64::saturating_add);
    let targets = snapshot
        .channels
        .iter()
        .flat_map(|c| &c.overwrites)
        .filter(|o| matches!(o.target, OverwriteTarget::Member(_)))
        .count();
    let virtual_users = count(snapshot.virtual_access.len());
    let virtual_total = snapshot
        .virtual_access
        .iter()
        .map(|(_, l)| count(l.len()))
        .fold(0u64, u64::saturating_add);
    let (sets, roles) = distinct(snapshot).unwrap_or((
        members,
        count(snapshot.roles.len())
            .saturating_add(overwrites)
            .saturating_add(member_roles),
    ));
    let roles = roles.saturating_add(1);
    let sets = sets.saturating_add(1);
    let words = sets.div_ceil(64);
    let exceptions = count(targets)
        .saturating_add(virtual_users)
        .saturating_add(quarantined)
        .saturating_add(1);
    let planning = members
        .saturating_mul(MEMBER_WRITE.saturating_add(24))
        .saturating_add(member_roles.saturating_mul(16))
        .saturating_add(overwrites.saturating_mul(8))
        .saturating_add(channels.saturating_mul(64))
        .saturating_add(roles.saturating_mul(8))
        .saturating_add(count(snapshot.roles.len()).saturating_mul(8))
        .saturating_add(sets.saturating_mul(16))
        .saturating_add(exceptions.saturating_mul(8));
    let slicer = sets
        .saturating_mul(8)
        .saturating_add(roles.saturating_mul(words));
    let sig_work = words.saturating_mul(overwrites.saturating_add(channels.saturating_mul(4)));
    let planes = channels
        .saturating_mul(4)
        .saturating_add(words.saturating_mul(channels.saturating_mul(2)));
    let overrides = exceptions
        .saturating_mul(16)
        .saturating_mul(channels.saturating_mul(2));
    let content = words
        .saturating_mul(3)
        .saturating_add(exceptions)
        .saturating_add(4);
    let classes = channels.saturating_mul(content.saturating_add(2));
    Units::new(
        planning
            .saturating_add(slicer)
            .saturating_add(sig_work)
            .saturating_add(planes)
            .saturating_add(overrides)
            .saturating_add(classes)
            .saturating_add(virtual_total.saturating_mul(2)),
    )
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Compaction {
    pub(crate) sets: bool,
    pub(crate) slots: bool,
}

impl Compaction {
    pub(crate) fn due(self) -> bool {
        self.sets || self.slots
    }
}

pub(crate) fn dead_sets(model: &Model, index: &MemberIndex) -> u32 {
    let total = u32::try_from(model.rolesets.len()).unwrap_or(u32::MAX);
    let live_without_zero = index
        .live_sets()
        .saturating_sub(u32::from(index.refcount(RoleSetId::EVERYONE) > 0));
    total.saturating_sub(1).saturating_sub(live_without_zero)
}

pub(crate) fn compaction(model: &Model, index: &MemberIndex, floor: u32) -> Compaction {
    let to_u32 = |n: usize| u32::try_from(n).unwrap_or(u32::MAX);
    let live_channels = to_u32(model.channels.len());
    Compaction {
        sets: dead_sets(model, index) > floor.max(index.live_sets()),
        slots: to_u32(model.channels.dead_slots()) > floor.max(live_channels),
    }
}

fn slot_compaction_cost(model: &Model) -> u64 {
    count(model.channels.slots())
        .saturating_mul(16)
        .saturating_add(count(model.classes.capacity()).saturating_mul(4))
}

pub(crate) fn compaction_estimate(model: &Model, index: &MemberIndex) -> Units {
    let sets = count(model.rolesets.len());
    let words = count(model.rolesets.words());
    let planes = count(model.signatures.len().saturating_add(model.classes.len()));
    let per_plane = sets.saturating_add(words).saturating_mul(3);
    let users = count(model.exceptions.len());
    Units::new(
        count(index.len())
            .saturating_mul(5)
            .saturating_add(sets.saturating_mul(8))
            .saturating_add(
                planes.saturating_mul(per_plane.saturating_add(users).saturating_add(8)),
            )
            .saturating_add(slot_compaction_cost(model)),
    )
}

pub(crate) fn compact(
    model: &Model,
    index: &MemberIndex,
    parts: Compaction,
    epoch: Epoch,
    viewable_limit: usize,
    meter: &mut Meter,
) -> (Model, Option<MemberIndex>) {
    let mut rolesets = model.rolesets.clone();
    let mut signatures = model.signatures.clone();
    let mut classes = model.classes.clone();
    let mut next_index = None;
    if parts.sets {
        let compacted = model.rolesets.compacted(|set| index.refcount(set) > 0);
        let remap: Arc<[u32]> = compacted
            .remap()
            .cloned()
            .unwrap_or_else(|| Arc::from(Vec::new()));
        let words = compacted.words();
        let per_plane =
            count(model.rolesets.len().saturating_add(model.rolesets.words())).saturating_mul(3);
        meter.charge(count(model.rolesets.len()).saturating_mul(8));
        meter.charge(count(model.signatures.len()).saturating_mul(per_plane.saturating_add(8)));
        signatures = Arc::new(model.signatures.remapped(&remap, words));
        meter.charge(
            count(model.classes.len()).saturating_mul(
                per_plane
                    .saturating_add(count(model.exceptions.len()))
                    .saturating_add(8),
            ),
        );
        classes = Arc::new(model.classes.remapped(&remap, words, &mut Meter::new()));
        meter.charge(count(index.len()).saturating_mul(4));
        next_index = Some(index.remapped(&remap));
        rolesets = Arc::new(compacted);
    }
    let mut channels = model.channels.clone();
    if parts.slots {
        meter.charge(slot_compaction_cost(model));
        let compacted = model.channels.compacted();
        if let Some(remap) = compacted.remap().cloned() {
            let slots = compacted.slots();
            Arc::make_mut(&mut signatures).channels_remapped(&remap, slots);
            Arc::make_mut(&mut classes).channels_remapped(&remap, slots);
        }
        channels = Arc::new(compacted);
    }
    let next = Model {
        epoch,
        guild: model.guild,
        owner: model.owner,
        roles: model.roles.clone(),
        rolesets: rolesets.clone(),
        channels,
        signatures,
        classes,
        exceptions: model.exceptions.clone(),
        virtual_view: model.virtual_view.clone(),
        flags: model.flags,
        viewable: ViewableCache::new(rolesets.len(), viewable_limit),
        viewsets: ViewSets::new(viewable_limit),
    };
    (next, next_index)
}
