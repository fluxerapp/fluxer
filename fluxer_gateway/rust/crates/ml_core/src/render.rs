// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeSet, VecDeque};

use crate::budget::{Budget, Meter};
use crate::core::{Dispatch, Group, Row, SyncBody, Target};
use crate::digits;
use crate::fragments::{Fragments, Revs};
use crate::ids::{GuildId, Slot, UserId};
use crate::members::Members;
use crate::payloads::{Payload, PayloadCache, PayloadWriter};
use crate::pending::TimerAction;
use crate::ranges::Range;
use crate::sections::Label;

pub const MAX_FILL: usize = 1_000;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct EncodedItem {
    pub user: UserId,
    pub list: Box<[u8]>,
    pub page: Box<[u8]>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Sent<P> {
    pub target: Target<P>,
    pub payload: Payload,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Rendered<P> {
    pub sent: Vec<Sent<P>>,
    pub timer: TimerAction,
    pub skipped_absent: u64,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Page {
    pub total: u32,
    pub payload: Payload,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Done<P> {
    Sync(Rendered<P>),
    Page(Page),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Step<P> {
    Done(Done<P>),
    Partial(Vec<Sent<P>>),
    NeedItems(Vec<UserId>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum FillError {
    TooMany,
    Busy,
}

#[derive(Debug)]
enum Work<P> {
    Sync {
        queue: VecDeque<Dispatch<P>>,
        sent: Vec<Sent<P>>,
        timer: TimerAction,
        skipped_absent: u64,
    },
    Page {
        ids: Vec<UserId>,
        total: u32,
    },
}

#[derive(Debug)]
pub struct Job<P> {
    work: Work<P>,
    asked: Option<BTreeSet<UserId>>,
    missing: BTreeSet<UserId>,
}

impl<P> Job<P> {
    pub fn sync(dispatches: Vec<Dispatch<P>>, timer: TimerAction, skipped_absent: u64) -> Job<P> {
        Job {
            work: Work::Sync {
                queue: dispatches.into(),
                sent: Vec::new(),
                timer,
                skipped_absent,
            },
            asked: None,
            missing: BTreeSet::new(),
        }
    }

    pub fn page(ids: Vec<UserId>, total: u32) -> Job<P> {
        Job {
            work: Work::Page { ids, total },
            asked: None,
            missing: BTreeSet::new(),
        }
    }

    pub fn is_waiting(&self) -> bool {
        self.asked.is_some()
    }
}

pub struct Ctx<'a> {
    pub guild: GuildId,
    pub members: &'a Members,
    pub fragments: &'a mut Fragments,
    pub payloads: &'a mut PayloadCache,
    pub budget: Budget,
    pub meter: Meter,
}

pub fn revs(members: &Members, slot: Slot) -> Revs {
    let presence = members.presence(slot);
    let shown = presence.status.shown_online(presence.connected);
    Revs {
        user: members.user(slot),
        member: members.member_rev(slot),
        presence: if shown { members.presence_rev(slot) } else { 0 },
        shown,
    }
}

fn label_code(label: Label) -> u64 {
    match label {
        Label::Role(id) => id.get(),
        Label::Online => u64::MAX,
        Label::Offline => u64::MAX.saturating_sub(1),
    }
}

fn stamp(members: &Members, body: &SyncBody, row_slots: &[Option<Slot>]) -> Vec<u64> {
    let words = body
        .ops
        .iter()
        .map(|op| op.rows.len().saturating_mul(6).saturating_add(3))
        .fold(
            body.groups.len().saturating_mul(2).saturating_add(3),
            usize::saturating_add,
        );
    let mut out = Vec::with_capacity(words);
    out.extend([
        u64::from(body.member_count),
        u64::from(body.online_count),
        body.groups.len() as u64,
    ]);
    for group in &body.groups {
        out.extend([label_code(group.label), u64::from(group.count)]);
    }
    let mut slots = row_slots.iter();
    for op in &body.ops {
        out.extend([1, u64::from(op.range.start()), u64::from(op.range.end())]);
        for row in &op.rows {
            match row {
                Row::Group(group) => {
                    out.extend([2, label_code(group.label), u64::from(group.count)])
                }
                Row::Member(user) => match slots.next().copied().flatten() {
                    Some(slot) => {
                        let r = revs(members, slot);
                        out.extend([
                            3,
                            u64::from(slot.get()),
                            user.get(),
                            u64::from(r.member),
                            u64::from(r.presence),
                            u64::from(r.shown),
                        ]);
                    }
                    None => out.extend([4, user.get()]),
                },
            }
        }
    }
    out
}

fn ranges_of(body: &SyncBody) -> Vec<Range> {
    body.ops.iter().map(|op| op.range).collect()
}

fn row_users(body: &SyncBody) -> impl Iterator<Item = UserId> + '_ {
    body.ops.iter().flat_map(|op| {
        op.rows.iter().filter_map(|row| match row {
            Row::Member(user) => Some(*user),
            Row::Group(_) => None,
        })
    })
}

fn list_needs(
    ctx: &mut Ctx<'_>,
    missing: &BTreeSet<UserId>,
    body: &SyncBody,
    row_slots: &[Option<Slot>],
) -> (BTreeSet<UserId>, usize) {
    let mut needs = BTreeSet::new();
    let mut bytes = 0usize;
    for (user, slot) in row_users(body).zip(row_slots) {
        if missing.contains(&user) {
            continue;
        }
        let Some(slot) = *slot else {
            continue;
        };
        match ctx.fragments.touch_list(slot, revs(ctx.members, slot)) {
            Some(len) => bytes = bytes.saturating_add(len).saturating_add(1),
            None => {
                needs.insert(user);
            }
        }
    }
    (needs, bytes)
}

fn sync_capacity(body: &SyncBody, items: usize) -> usize {
    const FRAME: usize = 192;
    const GROUP: usize = 64;
    const OP: usize = 64;
    let groups = body.groups.len().saturating_add(
        body.ops
            .iter()
            .flat_map(|op| op.rows.iter())
            .filter(|row| matches!(row, Row::Group(_)))
            .count(),
    );
    items
        .saturating_add(FRAME)
        .saturating_add(groups.saturating_mul(GROUP))
        .saturating_add(body.ops.len().saturating_mul(OP))
}

fn page_needs(ctx: &mut Ctx<'_>, missing: &BTreeSet<UserId>, ids: &[UserId]) -> BTreeSet<UserId> {
    let mut needs = BTreeSet::new();
    for user in ids {
        if missing.contains(user) {
            continue;
        }
        let Some(slot) = ctx.members.find(*user) else {
            continue;
        };
        if !ctx.fragments.touch_page(slot, revs(ctx.members, slot)) {
            needs.insert(*user);
        }
    }
    needs
}

fn group_id(out: &mut Vec<u8>, label: Label) {
    match label {
        Label::Role(id) => digits::quoted_int(out, id.get()),
        Label::Online => out.extend_from_slice(b"\"online\""),
        Label::Offline => out.extend_from_slice(b"\"offline\""),
    }
}

fn group_object(out: &mut Vec<u8>, group: &Group) {
    out.extend_from_slice(b"{\"count\":");
    digits::int(out, u64::from(group.count));
    out.extend_from_slice(b",\"id\":");
    group_id(out, group.label);
    out.push(b'}');
}

fn list_item(
    out: &mut Vec<u8>,
    ctx: &Ctx<'_>,
    missing: &BTreeSet<UserId>,
    user: UserId,
    slot: Option<Slot>,
) -> bool {
    if missing.contains(&user) {
        return false;
    }
    let Some(slot) = slot else {
        return false;
    };
    match ctx.fragments.list_bytes(slot) {
        Some(bytes) => {
            out.extend_from_slice(bytes);
            true
        }
        None => false,
    }
}

fn page_item(out: &mut Vec<u8>, ctx: &Ctx<'_>, missing: &BTreeSet<UserId>, user: UserId) -> bool {
    if missing.contains(&user) {
        return false;
    }
    let Some(slot) = ctx.members.find(user) else {
        return false;
    };
    match ctx.fragments.page_bytes(slot) {
        Some(bytes) => {
            out.extend_from_slice(bytes);
            true
        }
        None => false,
    }
}

pub type ItemWriter<'w> = dyn FnMut(&mut Vec<u8>, UserId) -> bool + 'w;

pub fn write_sync(guild: GuildId, body: &SyncBody, item: &mut ItemWriter<'_>) -> Vec<u8> {
    let mut out = Vec::with_capacity(256);
    write_sync_into(&mut out, guild, body, item);
    out
}

fn write_sync_into(out: &mut Vec<u8>, guild: GuildId, body: &SyncBody, item: &mut ItemWriter<'_>) {
    let channel = body.channel.get();
    out.extend_from_slice(b"{\"channel_id\":");
    digits::quoted_int(out, channel);
    out.extend_from_slice(b",\"groups\":[");
    for (i, group) in body.groups.iter().enumerate() {
        if i > 0 {
            out.push(b',');
        }
        group_object(out, group);
    }
    out.extend_from_slice(b"],\"guild_id\":");
    digits::quoted_int(out, guild.get());
    out.extend_from_slice(b",\"id\":");
    digits::quoted_int(out, channel);
    out.extend_from_slice(b",\"member_count\":");
    digits::int(out, u64::from(body.member_count));
    out.extend_from_slice(b",\"online_count\":");
    digits::int(out, u64::from(body.online_count));
    out.extend_from_slice(b",\"ops\":[");
    for (i, op) in body.ops.iter().enumerate() {
        if i > 0 {
            out.push(b',');
        }
        out.extend_from_slice(b"{\"items\":[");
        let mut first = true;
        for row in &op.rows {
            let mark = out.len();
            if !first {
                out.push(b',');
            }
            let wrote = match row {
                Row::Group(group) => {
                    out.extend_from_slice(b"{\"group\":");
                    group_object(out, group);
                    out.push(b'}');
                    true
                }
                Row::Member(user) => item(out, *user),
            };
            if wrote {
                first = false;
            } else {
                out.truncate(mark);
            }
        }
        out.extend_from_slice(b"],\"op\":\"SYNC\",\"range\":[");
        digits::int(out, u64::from(op.range.start()));
        out.push(b',');
        digits::int(out, u64::from(op.range.end()));
        out.extend_from_slice(b"]}");
    }
    out.extend_from_slice(b"]}");
}

pub fn write_page(ids: &[UserId], total: u32, item: &mut ItemWriter<'_>) -> Vec<u8> {
    let mut out = Vec::with_capacity(page_capacity(ids));
    write_page_into(&mut out, ids, total, item);
    out
}

fn page_capacity(ids: &[UserId]) -> usize {
    ids.len().saturating_mul(128).saturating_add(32)
}

fn write_page_into(out: &mut Vec<u8>, ids: &[UserId], total: u32, item: &mut ItemWriter<'_>) {
    out.extend_from_slice(b"{\"members\":[");
    let mut first = true;
    for user in ids {
        let mark = out.len();
        if !first {
            out.push(b',');
        }
        if item(out, *user) {
            first = false;
        } else {
            out.truncate(mark);
        }
    }
    out.extend_from_slice(b"],\"total\":");
    digits::int(out, u64::from(total));
    out.push(b'}');
}

fn ask<P>(job: &mut Job<P>, needs: BTreeSet<UserId>) -> Step<P> {
    let users: Vec<UserId> = needs.iter().copied().take(MAX_FILL).collect();
    job.asked = Some(users.iter().copied().collect());
    Step::NeedItems(users)
}

pub fn drive<P>(ctx: &mut Ctx<'_>, job: &mut Job<P>) -> Step<P> {
    let missing = core::mem::take(&mut job.missing);
    let step = match &mut job.work {
        Work::Page { ids, total } => {
            let needs = page_needs(ctx, &missing, ids);
            if !needs.is_empty() {
                job.missing = missing;
                return ask(job, needs);
            }
            let mut writer = PayloadWriter::with_capacity(page_capacity(ids));
            write_page_into(writer.out(), ids, *total, &mut |out, user| {
                page_item(out, ctx, &missing, user)
            });
            Step::Done(Done::Page(Page {
                total: *total,
                payload: writer.finish(),
            }))
        }
        Work::Sync {
            queue,
            sent,
            timer,
            skipped_absent,
        } => loop {
            let Some(dispatch) = queue.pop_front() else {
                break Step::Done(Done::Sync(Rendered {
                    sent: core::mem::take(sent),
                    timer: *timer,
                    skipped_absent: *skipped_absent,
                }));
            };
            let body = &dispatch.body;
            let ranges = ranges_of(body);
            let rows = body.ops.iter().map(|op| op.rows.len() as u64).sum::<u64>();
            let row_slots: Vec<Option<Slot>> =
                row_users(body).map(|user| ctx.members.find(user)).collect();
            let stamp = stamp(ctx.members, body, &row_slots);
            let payload = match ctx.payloads.get(body.channel, &ranges, &stamp) {
                Some(payload) => {
                    ctx.meter.charge(rows.saturating_add(16));
                    payload
                }
                None => {
                    let (needs, items) = list_needs(ctx, &missing, body, &row_slots);
                    if !needs.is_empty() {
                        queue.push_front(dispatch);
                        job.missing = missing;
                        return ask(job, needs);
                    }
                    let mut writer = PayloadWriter::with_capacity(sync_capacity(body, items));
                    let mut slots = row_slots.iter();
                    write_sync_into(writer.out(), ctx.guild, body, &mut |out, user| {
                        list_item(out, ctx, &missing, user, slots.next().copied().flatten())
                    });
                    let payload = writer.finish();
                    ctx.meter.charge(
                        rows.saturating_mul(4)
                            .saturating_add(payload.len() as u64 / 64)
                            .saturating_add(16),
                    );
                    if !row_users(body).any(|u| missing.contains(&u)) {
                        ctx.payloads
                            .put(body.channel, &ranges, stamp, payload.clone());
                    }
                    payload
                }
            };
            sent.push(Sent {
                target: dispatch.target,
                payload,
            });
            if !queue.is_empty() && !ctx.budget.fits(ctx.meter.spent()) {
                job.missing = missing;
                return Step::Partial(core::mem::take(sent));
            }
        },
    };
    ctx.fragments.trim();
    step
}

pub fn fill_units(items: &[EncodedItem]) -> u64 {
    items.iter().fold(0u64, |units, item| {
        let bytes = item.list.len().saturating_add(item.page.len()) as u64;
        units.saturating_add(bytes / 64).saturating_add(16)
    })
}

pub fn fill<P>(
    ctx: &mut Ctx<'_>,
    job: &mut Job<P>,
    items: Vec<EncodedItem>,
) -> Result<Step<P>, FillError> {
    if items.len() > MAX_FILL {
        return Err(FillError::TooMany);
    }
    let Some(mut asked) = job.asked.take() else {
        return Err(FillError::Busy);
    };
    ctx.meter.charge(fill_units(&items));
    for item in items {
        if !asked.remove(&item.user) {
            continue;
        }
        let slot = ctx.members.find(item.user);
        match slot {
            Some(slot) if !item.list.is_empty() => {
                ctx.fragments
                    .put(slot, revs(ctx.members, slot), item.list, item.page);
            }
            _ => {
                job.missing.insert(item.user);
            }
        }
    }
    job.missing.extend(asked);
    Ok(drive(ctx, job))
}
