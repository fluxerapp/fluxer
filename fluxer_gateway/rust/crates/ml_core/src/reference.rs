// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use perm_core::{Hoist, Model, PermsShared, RankPos, View};

use crate::budget::Units;
use crate::casefold;
use crate::core::{Dispatch, Effects, Group, Row, SyncBody, SyncOp, Target};
use crate::event::{Change, Connection, Event, Snapshot, SyncScope, Timing, Updates};
use crate::flags::{Status, Visible};
use crate::ids::{ChannelId, ListKey, RoleIdx, SessionKey, UserId};
use crate::pending::{Mark, TimerAction};
use crate::ranges::Ranges;
use crate::sections::Label;

const ONLINE_IDX: u64 = 0xF000_0000;
const OFFLINE_IDX: u64 = 0xF000_0001;
const SKIPPED_CAP: u64 = 1_000_000_000;

#[derive(Debug)]
pub struct Reference<P> {
    perms: Arc<PermsShared>,
    threshold: u32,
    names: BTreeMap<UserId, Vec<u8>>,
    presence: BTreeMap<UserId, (Status, bool)>,
    subs: BTreeMap<(ListKey, SessionKey), Ranges>,
    session_lists: BTreeMap<SessionKey, Vec<ListKey>>,
    session_info: BTreeMap<SessionKey, (P, UserId)>,
    batch: Option<(BTreeMap<ListKey, Mark>, u64)>,
    next_timer: u64,
    updates_on: bool,
    skipped: u64,
}

struct Entry {
    section: u64,
    key: Vec<u8>,
    user: UserId,
    online: bool,
}

struct ListView {
    total: u32,
    online: u32,
    groups: Vec<(Label, u32)>,
    rows: Vec<Row>,
    hidden: bool,
}

fn hoisted(model: &Model) -> Vec<RoleIdx> {
    let mut roles: Vec<(RoleIdx, RankPos, u64)> = Vec::new();
    for idx in model.roles().present_in_order() {
        let Some(role) = model.roles().get(*idx) else {
            continue;
        };
        if role.id().get() == model.guild().get() || role.id().get() == 0 {
            continue;
        }
        let pos = match role.hoist() {
            Hoist::NotHoisted => continue,
            Hoist::Hoisted(Some(at)) => RankPos::Int(at),
            Hoist::Hoisted(None) => role.position(),
        };
        roles.push((*idx, pos, role.id().get()));
    }
    roles.sort_by(|a, b| b.1.cmp(&a.1).then(a.2.cmp(&b.2)));
    roles.into_iter().map(|(idx, _, _)| idx).collect()
}

impl<P: Copy + Eq + core::fmt::Debug> Reference<P> {
    pub fn new(perms: Arc<PermsShared>, threshold: u32) -> Reference<P> {
        Reference {
            perms,
            threshold,
            names: BTreeMap::new(),
            presence: BTreeMap::new(),
            subs: BTreeMap::new(),
            session_lists: BTreeMap::new(),
            session_info: BTreeMap::new(),
            batch: None,
            next_timer: 0,
            updates_on: true,
            skipped: 0,
        }
    }

    pub fn load(&mut self, snapshot: &Snapshot) {
        self.names = snapshot
            .members
            .iter()
            .map(|(u, n)| (*u, n.to_vec()))
            .collect();
        self.presence.clear();
        for (user, status) in &snapshot.statuses {
            self.presence
                .entry(*user)
                .or_insert((Status::Offline, false))
                .0 = *status;
        }
        for user in &snapshot.connected {
            self.presence
                .entry(*user)
                .or_insert((Status::Offline, false))
                .1 = true;
        }
        self.updates_on = !snapshot.updates_off;
    }

    fn online(&self, user: UserId) -> bool {
        self.presence
            .get(&user)
            .is_some_and(|(s, c)| *c && s.counts_as_online())
    }

    fn sees(view: &View<'_>, channel: ChannelId, user: UserId, bits: Visible) -> bool {
        let model = view.model();
        match (model.class_of_id(channel), view.roleset(user)) {
            (Some(class), Some(set)) => model.visible(class, set, user).contains(bits),
            _ => false,
        }
    }

    fn is_row(&self, view: &View<'_>, list: ListKey, user: UserId) -> bool {
        self.names.contains_key(&user) && Self::sees(view, list.channel(), user, Visible::VIEW)
    }

    fn list_view(&self, view: &View<'_>, channel: ChannelId) -> ListView {
        let model = view.model();
        let order = hoisted(model);
        let mut entries: Vec<Entry> = Vec::new();
        for (user, name) in &self.names {
            if !Self::sees(view, channel, *user, Visible::VIEW) {
                continue;
            }
            let online = self.online(*user);
            let section = if online {
                view.roleset(*user)
                    .and_then(|set| model.rolesets().get(set))
                    .and_then(|set| {
                        set.roles()
                            .iter()
                            .filter_map(|r| order.iter().position(|h| h == r))
                            .min()
                    })
                    .map_or(ONLINE_IDX, |i| i as u64)
            } else {
                OFFLINE_IDX
            };
            entries.push(Entry {
                section,
                key: casefold::fold(name),
                user: *user,
                online,
            });
        }
        entries.sort_by(|a, b| {
            (a.section, &a.key, a.user.get() as i64).cmp(&(b.section, &b.key, b.user.get() as i64))
        });
        let mut sections: Vec<(u64, Label)> = order
            .iter()
            .enumerate()
            .map(|(i, r)| {
                let id = model.roles().get(*r).map(|role| role.id());
                (i as u64, id.map_or(Label::Online, Label::Role))
            })
            .collect();
        sections.push((ONLINE_IDX, Label::Online));
        sections.push((OFFLINE_IDX, Label::Offline));
        let mut all_groups = Vec::new();
        let mut rows = Vec::new();
        for (idx, label) in &sections {
            let members: Vec<&Entry> = entries.iter().filter(|e| e.section == *idx).collect();
            let count = members.len() as u32;
            all_groups.push((*label, count));
            if count == 0 {
                continue;
            }
            rows.push(Row::Group(Group {
                label: *label,
                count,
            }));
            rows.extend(members.iter().map(|e| Row::Member(e.user)));
        }
        let offline = all_groups
            .iter()
            .find(|(l, _)| *l == Label::Offline)
            .map_or(0, |(_, c)| *c);
        let hidden = offline > self.threshold;
        let groups = all_groups
            .into_iter()
            .filter(|(l, c)| *c > 0 && (*l != Label::Offline || *c <= self.threshold))
            .collect();
        ListView {
            total: entries.len() as u32,
            online: entries.iter().filter(|e| e.online).count() as u32,
            groups,
            rows,
            hidden,
        }
    }

    fn body(&self, view: &View<'_>, channel: ChannelId, ranges: &Ranges) -> SyncBody {
        let list = self.list_view(view, channel);
        let visible_end: i64 = list
            .groups
            .iter()
            .filter(|(l, _)| *l != Label::Offline)
            .map(|(_, c)| i64::from(*c).saturating_add(1))
            .fold(0i64, i64::saturating_add)
            .saturating_sub(1);
        let ops = ranges
            .iter()
            .map(|range| {
                let start = i64::from(range.start());
                let mut end = i64::from(range.end());
                if list.hidden {
                    end = end.min(visible_end);
                }
                let rows = if start > end {
                    Vec::new()
                } else {
                    list.rows
                        .iter()
                        .enumerate()
                        .filter(|(i, _)| (*i as i64) >= start && (*i as i64) <= end)
                        .map(|(_, r)| *r)
                        .collect()
                };
                SyncOp { range, rows }
            })
            .collect();
        SyncBody {
            channel,
            member_count: list.total,
            online_count: list.online,
            groups: list
                .groups
                .iter()
                .map(|(label, count)| Group {
                    label: *label,
                    count: *count,
                })
                .collect(),
            ops,
        }
    }

    fn list_ids(&self) -> Vec<ListKey> {
        let set: BTreeSet<ListKey> = self.subs.keys().map(|(l, _)| *l).collect();
        set.into_iter().collect()
    }

    fn dispatch_list(&self, view: &View<'_>, list: ListKey, out: &mut Vec<Dispatch<P>>) {
        let mut groups: BTreeMap<Ranges, Vec<P>> = BTreeMap::new();
        for ((l, session), ranges) in &self.subs {
            if *l != list {
                continue;
            }
            let Some((pid, user)) = self.session_info.get(session) else {
                continue;
            };
            if Self::sees(
                view,
                list.channel(),
                *user,
                Visible::VIEW | Visible::VIEW_MEMBERS,
            ) && self.names.contains_key(user)
            {
                groups.entry(ranges.clone()).or_default().push(*pid);
            }
        }
        for (ranges, pids) in groups {
            out.push(Dispatch {
                target: Target::Sessions(pids),
                body: self.body(view, list.channel(), &ranges),
            });
        }
    }

    fn take_pending(&mut self, lists: Vec<ListKey>, timers: &mut Timers) -> Vec<ListKey> {
        let mut out = lists;
        if let Some((marks, timer)) = self.batch.take() {
            timers.cancelled.insert(timer);
            out.extend(
                marks
                    .iter()
                    .filter(|(_, m)| **m != Mark::Synced)
                    .map(|(l, _)| *l),
            );
        }
        let set: BTreeSet<ListKey> = out.into_iter().collect();
        set.into_iter().collect()
    }

    fn queue_list_syncs(
        &mut self,
        view: &View<'_>,
        lists: Vec<ListKey>,
        timers: &mut Timers,
        out: &mut Vec<Dispatch<P>>,
    ) {
        if lists.is_empty() {
            return;
        }
        let lists = self.take_pending(lists, timers);
        for list in lists {
            self.dispatch_list(view, list, out);
        }
    }

    fn queue_connection_sync(&mut self, list: ListKey, mark: Mark) {
        match &mut self.batch {
            Some((marks, _)) => {
                let entry = marks.entry(list).or_insert(mark);
                if *entry != Mark::Sync {
                    *entry = mark;
                }
            }
            None => {
                self.next_timer = self.next_timer.saturating_add(1);
                self.batch = Some((BTreeMap::from([(list, mark)]), self.next_timer));
            }
        }
    }

    fn skip(&mut self) {
        if self.skipped < SKIPPED_CAP {
            self.skipped = self.skipped.saturating_add(1);
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn user_change(
        &mut self,
        view: &View<'_>,
        user: UserId,
        member_unchanged: bool,
        before: &BTreeMap<ListKey, bool>,
        timing: Timing,
        timers: &mut Timers,
        out: &mut Vec<Dispatch<P>>,
    ) {
        for list in self.list_ids() {
            let absent_before = member_unchanged && !before.get(&list).copied().unwrap_or(true);
            if absent_before && !self.is_row(view, list, user) {
                self.skip();
                continue;
            }
            match timing {
                Timing::Immediate => self.queue_list_syncs(view, vec![list], timers, out),
                Timing::Deferred => self.queue_connection_sync(list, Mark::Sync),
            }
        }
    }

    fn rows_before(&self, view: &View<'_>, user: UserId) -> BTreeMap<ListKey, bool> {
        self.list_ids()
            .into_iter()
            .map(|l| (l, self.is_row(view, l, user)))
            .collect()
    }

    pub fn apply(&mut self, events: &[Event<P>]) -> Effects<P> {
        let perms = Arc::clone(&self.perms);
        let Ok(view) = perms.view() else {
            return Effects {
                dispatches: Vec::new(),
                timer: TimerAction::KeepTimer,
                skipped_absent: self.skipped,
                spent: Units::ZERO,
                estimate: Units::ZERO,
            };
        };
        let start = self.batch.as_ref().map(|(_, t)| *t);
        let mut timers = Timers::default();
        let mut out = Vec::new();
        for event in events {
            self.event(&view, event, &mut timers, &mut out);
        }
        let end = self.batch.as_ref().map(|(_, t)| *t);
        let timer = match (start, end) {
            (_, Some(e)) if Some(e) != start => TimerAction::ArmTimer,
            (Some(s), None) if timers.cancelled.contains(&s) && !timers.fired.contains(&s) => {
                TimerAction::CancelTimer
            }
            _ => TimerAction::KeepTimer,
        };
        Effects {
            dispatches: out,
            timer,
            skipped_absent: self.skipped,
            spent: Units::ZERO,
            estimate: Units::ZERO,
        }
    }

    fn event(
        &mut self,
        view: &View<'_>,
        event: &Event<P>,
        timers: &mut Timers,
        out: &mut Vec<Dispatch<P>>,
    ) {
        match event {
            Event::MemberUpsert {
                user,
                display_name,
                change,
            } => {
                let before = self.rows_before(view, *user);
                self.names.insert(*user, display_name.to_vec());
                if self.updates_on {
                    let unchanged = *change == Change::Unchanged;
                    self.user_change(
                        view,
                        *user,
                        unchanged,
                        &before,
                        Timing::Immediate,
                        timers,
                        out,
                    );
                }
            }
            Event::MemberRemove(user) => {
                self.names.remove(user);
                if self.updates_on {
                    let lists = self.list_ids();
                    for list in lists {
                        self.queue_list_syncs(view, vec![list], timers, out);
                    }
                }
            }
            Event::Presence {
                user,
                status,
                change,
                timing,
            } => {
                let before = self.rows_before(view, *user);
                self.presence
                    .entry(*user)
                    .or_insert((Status::Offline, false))
                    .0 = *status;
                let member = self.names.contains_key(user);
                if *change == Change::Unchanged && member {
                    let lists: Vec<ListKey> = match &self.batch {
                        Some((marks, _)) => marks.keys().copied().collect(),
                        None => Vec::new(),
                    };
                    let present: Vec<bool> =
                        lists.iter().map(|l| self.is_row(view, *l, *user)).collect();
                    if let Some((marks, _)) = &mut self.batch {
                        for (list, row) in lists.iter().zip(present) {
                            if row && marks.get(list) == Some(&Mark::Synced) {
                                marks.insert(*list, Mark::Sync);
                            }
                        }
                    }
                } else {
                    self.user_change(view, *user, member, &before, *timing, timers, out);
                }
            }
            Event::PresenceCleared(user) => {
                self.presence
                    .entry(*user)
                    .or_insert((Status::Offline, false))
                    .0 = Status::Offline;
            }
            Event::Connection(user, connection) => {
                let connected = matches!(connection, Connection::Connected { .. });
                self.presence
                    .entry(*user)
                    .or_insert((Status::Offline, false))
                    .1 = connected;
                let mark = match connection {
                    Connection::Connected {
                        presence: Change::Changed,
                    } => Mark::Synced,
                    _ => Mark::Sync,
                };
                for list in self.list_ids() {
                    if self.is_row(view, list, *user) {
                        self.queue_connection_sync(list, mark);
                    } else {
                        self.skip();
                    }
                }
            }
            Event::Permissions(scope) => {
                if !self.updates_on {
                    return;
                }
                match scope {
                    SyncScope::AllLists => {
                        let lists = self.list_ids();
                        self.queue_list_syncs(view, lists, timers, out);
                    }
                    SyncScope::Channel(channel) => {
                        let list = ListKey::new(*channel);
                        if self.subs.keys().any(|(l, _)| *l == list) {
                            self.queue_list_syncs(view, vec![list], timers, out);
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
            } => {
                let allowed = self.names.contains_key(user)
                    && Self::sees(view, *channel, *user, Visible::VIEW | Visible::VIEW_MEMBERS);
                if !allowed {
                    return;
                }
                let list = ListKey::new(*channel);
                let key = (list, session.clone());
                if ranges.is_empty() {
                    self.subs.remove(&key);
                    if let Some(lists) = self.session_lists.get_mut(session) {
                        lists.retain(|l| *l != list);
                        if lists.is_empty() {
                            self.session_lists.remove(session);
                            self.session_info.remove(session);
                        }
                    }
                    return;
                }
                self.subs.insert(key, ranges.clone());
                let lists = self.session_lists.entry(session.clone()).or_default();
                for other in lists.iter().filter(|l| **l != list) {
                    self.subs.remove(&(*other, session.clone()));
                }
                *lists = vec![list];
                self.session_info.insert(session.clone(), (*pid, *user));
                if self.updates_on && !sync.is_empty() {
                    out.push(Dispatch {
                        target: Target::Session(*pid),
                        body: self.body(view, *channel, sync),
                    });
                }
            }
            Event::Unsubscribe { session, channel } => {
                let list = ListKey::new(*channel);
                if self.subs.remove(&(list, session.clone())).is_some() {
                    self.session_lists.remove(session);
                    self.session_info.remove(session);
                }
            }
            Event::DropSession(session) => {
                for list in self.session_lists.remove(session).unwrap_or_default() {
                    self.subs.remove(&(list, session.clone()));
                }
                self.session_info.remove(session);
            }
            Event::ViewGained { session, channel } => {
                let list = ListKey::new(*channel);
                let Some(ranges) = self.subs.get(&(list, session.clone())).cloned() else {
                    return;
                };
                let Some((pid, user)) = self.session_info.get(session).copied() else {
                    return;
                };
                if !ranges.is_empty()
                    && self.names.contains_key(&user)
                    && Self::sees(view, *channel, user, Visible::VIEW | Visible::VIEW_MEMBERS)
                {
                    out.push(Dispatch {
                        target: Target::Session(pid),
                        body: self.body(view, *channel, &ranges),
                    });
                }
            }
            Event::Flush => {
                if let Some((_, timer)) = &self.batch {
                    timers.fired.insert(*timer);
                }
                let lists = self.take_pending(Vec::new(), timers);
                for list in lists {
                    self.dispatch_list(view, list, out);
                }
            }
            Event::Updates(updates) => self.updates_on = *updates == Updates::UpdatesOn,
        }
    }

    pub fn render_session(&self, session: &SessionKey) -> Option<SyncBody> {
        let lists = self.session_lists.get(session)?;
        let list = *lists.first()?;
        let ranges = self.subs.get(&(list, session.clone()))?;
        let perms = Arc::clone(&self.perms);
        let view = perms.view().ok()?;
        Some(self.body(&view, list.channel(), ranges))
    }

    pub fn counts(&self, channel: ChannelId) -> (u32, u32) {
        let perms = Arc::clone(&self.perms);
        let Ok(view) = perms.view() else {
            return (0, 0);
        };
        if view.model().class_of_id(channel).is_none() {
            return (0, 0);
        }
        let list = self.list_view(&view, channel);
        (list.total, list.online)
    }

    pub fn guild_counts(&self) -> (u32, u32) {
        let online = self.names.keys().filter(|u| self.online(**u)).count() as u32;
        (self.names.len() as u32, online)
    }

    pub fn member_ids(&self) -> Vec<UserId> {
        self.names.keys().copied().collect()
    }

    pub fn pending_marks(&self) -> Vec<(ListKey, Mark)> {
        self.batch
            .as_ref()
            .map(|(m, _)| m.iter().map(|(l, k)| (*l, *k)).collect())
            .unwrap_or_default()
    }

    pub fn session_ranges(&self, session: &SessionKey) -> Option<(ChannelId, Ranges)> {
        let lists = self.session_lists.get(session)?;
        let list = *lists.first()?;
        self.subs
            .get(&(list, session.clone()))
            .map(|r| (list.channel(), r.clone()))
    }
}

#[derive(Default)]
struct Timers {
    cancelled: BTreeSet<u64>,
    fired: BTreeSet<u64>,
}
