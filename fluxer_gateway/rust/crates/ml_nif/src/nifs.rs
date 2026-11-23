// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;

use guild_rt::Scheduler;
use ml_core::budget::{Budget, Units};
use ml_core::{Core, Delta, MaskReply, Mode, render};
use perm_core::PermsShared;
use perm_nif::DeltaResource;
use rustler::{Encoder, Env, ResourceArc, Term};

use crate::atoms;
use crate::decode::{self, Decoded, Query};
use crate::encode;
use crate::resource::{Counts, Failure, MemberList, Subscriber};

pub const PAGE_IDS_NORMAL: u32 = 10_000;

const ITEMS_PER_PERCENT: usize = 256;

const SUBSCRIPTIONS_NORMAL: usize = 2_000;

fn decoded<T>(value: Decoded<T>) -> Result<T, Failure> {
    value.map_err(Failure::BadInput)
}

fn handle(term: Term<'_>) -> Result<ResourceArc<MemberList>, Failure> {
    term.decode::<ResourceArc<MemberList>>()
        .map_err(|_| Failure::BadInput("handle: expected a member list handle".to_owned()))
}

fn charge(env: Env<'_>, mode: Mode, spent: Units, units: u64) {
    match mode {
        Mode::Normal => {
            let percent = spent.fraction_percent(Budget::new(Units::new(units)));
            rustler::schedule::consume_timeslice(env, i32::from(percent.max(1)));
        }
        Mode::Dirty => guild_rt::dirty_call(),
    }
}

fn mode_of(on: Scheduler) -> Mode {
    match on {
        Scheduler::Normal => Mode::Normal,
        Scheduler::Dirty => Mode::Dirty,
    }
}

fn on_core<R>(
    list: &MemberList,
    mode: Mode,
    work: impl FnOnce(&mut Core<Subscriber>) -> Result<R, Failure>,
) -> Result<R, Failure> {
    match mode {
        Mode::Normal => list.try_with_core(work),
        Mode::Dirty => list.with_core(work),
    }
}

fn normal_only<'a>(
    env: Env<'a>,
    mode: Mode,
    result: Result<Term<'a>, Failure>,
) -> Option<Term<'a>> {
    match (mode, result) {
        (Mode::Normal, Err(Failure::NeedsDirty)) => None,
        (_, result) => Some(encode::reply(env, result)),
    }
}

fn perms_of(term: Term<'_>) -> Option<Arc<PermsShared>> {
    perm_nif::shared_from_term(term).ok()
}

fn pair<'a>(env: Env<'a>, list: ResourceArc<MemberList>, counts: ResourceArc<Counts>) -> Term<'a> {
    (list.encode(env), counts.encode(env)).encode(env)
}

pub fn new<'a>(
    env: Env<'a>,
    guild: Term<'a>,
    config: Term<'a>,
    owner: Term<'a>,
    perms: Term<'a>,
) -> Term<'a> {
    let counts = ResourceArc::new(Counts::new());
    let inputs = decode::guild(guild, "guild")
        .and_then(|g| decode::config(config).map(|c| (g, c)))
        .and_then(|(g, c)| decode::owner(owner, "owner").map(|o| (g, c, o)));
    let core = match (inputs.as_ref(), perms_of(perms)) {
        (Ok((guild, config, _)), Some(shared)) => {
            Core::<Subscriber>::new(*guild, *config, shared).ok()
        }
        _ => None,
    };
    let list = ResourceArc::new(MemberList::new(core, counts.clone()));
    if let Ok((_, _, owner)) = inputs
        && env.monitor(&list, &owner).is_none()
    {
        list.shut_now();
    }
    pair(env, list, counts)
}

const LOAD_UNITS_PER_ROW: u64 = 24;

fn load_fits(list: &MemberList, snapshot: Term<'_>) -> Result<(), Failure> {
    let Some(units) = list.try_read_even_broken(|core| core.config().work_budget_units)? else {
        return Ok(());
    };
    let most = usize::try_from(units / LOAD_UNITS_PER_ROW).unwrap_or(usize::MAX);
    let rows = decode::snapshot_rows(snapshot, most);
    if Budget::new(Units::new(units)).fits(Core::<Subscriber>::load_estimate(rows)) {
        Ok(())
    } else {
        Err(Failure::NeedsDirty)
    }
}

fn load_with<'a>(env: Env<'a>, list: Term<'a>, snapshot: Term<'a>, mode: Mode) -> Term<'a> {
    let result = handle(list).and_then(|list| {
        if mode == Mode::Normal {
            load_fits(&list, snapshot)?;
        }
        let snapshot = decoded(decode::snapshot(snapshot))?;
        on_core(&list, mode, |core| {
            let units = core.config().work_budget_units;
            let estimate = Core::<Subscriber>::load_estimate(snapshot.members.len());
            core.load(&snapshot, mode)?;
            charge(env, mode, estimate, units);
            Ok(encode::nil(env))
        })
    });
    encode::reply(env, result)
}

pub fn load<'a>(env: Env<'a>, list: Term<'a>, snapshot: Term<'a>) -> Term<'a> {
    load_with(env, list, snapshot, Mode::Normal)
}

pub fn load_dirty<'a>(env: Env<'a>, list: Term<'a>, snapshot: Term<'a>) -> Term<'a> {
    load_with(env, list, snapshot, Mode::Dirty)
}

fn delta_resource(term: Term<'_>) -> Result<Option<ResourceArc<DeltaResource>>, Failure> {
    decoded(decode::option(term, "delta"))?
        .map(|inner| {
            perm_nif::delta_from_term(inner)
                .map_err(|_| Failure::BadInput("delta: expected a permission delta".to_owned()))
        })
        .transpose()
}

fn delta_for(
    resource: Option<&DeltaResource>,
    mode: Mode,
    budget: Budget,
) -> Result<(Option<Delta>, Units), Failure> {
    match (resource, mode) {
        (None, _) => Ok((None, Units::ZERO)),
        (Some(resource), Mode::Dirty) => Ok((Some(resource.delta().clone()), Units::ZERO)),
        (Some(resource), Mode::Normal) => resource
            .within(budget)
            .map(|(delta, cost)| (Some(delta.clone()), cost))
            .ok_or(Failure::NeedsDirty),
    }
}

fn apply_with<'a>(
    env: Env<'a>,
    list: Term<'a>,
    events: Term<'a>,
    delta: Term<'a>,
    mode: Mode,
) -> Term<'a> {
    let result = handle(list).and_then(|list| {
        let events = decoded(decode::events(events))?;
        let delta = delta_resource(delta)?;
        on_core(&list, mode, |core| {
            let units = core.config().work_budget_units;
            let diff_budget = Budget::new(Units::new(units / 2));
            let (delta, diffed) = delta_for(delta.as_deref(), mode, diff_budget)?;
            let effects = core.apply(&events, delta.as_ref(), mode)?;
            let spent = effects.spent;
            let step = core.render(effects)?;
            charge(
                env,
                mode,
                spent.plus(core.render_spent()).plus(diffed),
                units,
            );
            Ok(encode::step(env, &step))
        })
    });
    encode::reply(env, result)
}

pub fn apply<'a>(env: Env<'a>, list: Term<'a>, events: Term<'a>, delta: Term<'a>) -> Term<'a> {
    apply_with(env, list, events, delta, Mode::Normal)
}

pub fn apply_dirty<'a>(
    env: Env<'a>,
    list: Term<'a>,
    events: Term<'a>,
    delta: Term<'a>,
) -> Term<'a> {
    apply_with(env, list, events, delta, Mode::Dirty)
}

pub fn fill<'a>(env: Env<'a>, list: Term<'a>, items: Term<'a>, on: Scheduler) -> Option<Term<'a>> {
    let mode = mode_of(on);
    let result = handle(list).and_then(|list| {
        let items = decoded(decode::items(items))?;
        on_core(&list, mode, |core| {
            let units = core.config().work_budget_units;
            let estimate = Units::new(render::fill_units(&items));
            if mode == Mode::Normal && !Budget::new(Units::new(units)).fits(estimate) {
                return Err(Failure::NeedsDirty);
            }
            let step = core.fill(items)?;
            charge(env, mode, core.render_spent(), units);
            Ok(encode::step(env, &step))
        })
    });
    normal_only(env, mode, result)
}

pub fn resume<'a>(env: Env<'a>, list: Term<'a>, on: Scheduler) -> Option<Term<'a>> {
    let mode = mode_of(on);
    let result = handle(list).and_then(|list| {
        on_core(&list, mode, |core| {
            let units = core.config().work_budget_units;
            let step = core.resume()?;
            charge(env, mode, core.render_spent(), units);
            Ok(encode::step(env, &step))
        })
    });
    normal_only(env, mode, result)
}

fn subscriptions<'a>(env: Env<'a>, list: &MemberList, mode: Mode) -> Result<Term<'a>, Failure> {
    let found = match mode {
        Mode::Normal => list
            .try_read_even_broken(|core| {
                (core.subscription_count() <= SUBSCRIPTIONS_NORMAL).then(|| core.subscriptions())
            })?
            .map(|fits| fits.ok_or(Failure::NeedsDirty))
            .transpose()?,
        Mode::Dirty => {
            guild_rt::dirty_call();
            list.read_even_broken(|core| core.subscriptions())
        }
    };
    if mode == Mode::Normal {
        charge_items(env, found.as_ref().map_or(0, Vec::len));
    }
    let items: Vec<Term<'a>> = found
        .unwrap_or_default()
        .iter()
        .map(|s| {
            let fields = [
                atoms::subscription().encode(env),
                encode::binary(env, s.session.as_bytes()),
                s.pid.0.encode(env),
                s.user.get().encode(env),
                s.channel.get().encode(env),
                encode::ranges(env, &s.ranges),
            ];
            rustler::types::tuple::make_tuple(env, &fields)
        })
        .collect();
    Ok(items.encode(env))
}

fn page_fits(core: &Core<Subscriber>, limit: u32, mode: Mode) -> Result<(), Failure> {
    let rows = limit.min(core.guild_counts().0);
    if mode == Mode::Normal && rows > PAGE_IDS_NORMAL {
        Err(Failure::NeedsDirty)
    } else {
        Ok(())
    }
}

fn tagged<'a>(env: Env<'a>, tag: rustler::Atom, value: Term<'a>) -> Term<'a> {
    (tag, value).encode(env)
}

fn query_with<'a>(env: Env<'a>, list: Term<'a>, query: Term<'a>, mode: Mode) -> Term<'a> {
    let result = handle(list).and_then(|list| {
        let query = decoded(decode::query(query))?;
        if query == Query::Subscriptions {
            return Ok(tagged(
                env,
                atoms::subscriptions_reply(),
                subscriptions(env, &list, mode)?,
            ));
        }
        let run = |core: &mut Core<Subscriber>| {
            if mode == Mode::Dirty {
                guild_rt::dirty_call();
            }
            match query {
                Query::Subscriptions => Ok(tagged(
                    env,
                    atoms::subscriptions_reply(),
                    Vec::<Term<'a>>::new().encode(env),
                )),
                Query::SessionRanges(session) => Ok(tagged(
                    env,
                    atoms::ranges_reply(),
                    encode::session_ranges(env, core.session_ranges(&session)),
                )),
                Query::ChannelCounts(channels) => {
                    let counts = core.channel_counts(&channels, mode)?;
                    if mode == Mode::Normal {
                        charge_items(env, counts.len());
                    }
                    Ok(tagged(
                        env,
                        atoms::counts_reply(),
                        encode::channel_counts(env, &counts),
                    ))
                }
                Query::PageIds { offset, limit } => {
                    page_fits(core, limit, mode)?;
                    let ids = core.page_ids(offset, limit);
                    if mode == Mode::Normal {
                        charge_items(env, ids.len());
                    }
                    Ok(tagged(
                        env,
                        atoms::ids_reply(),
                        encode::packed_ids(env, &ids),
                    ))
                }
            }
        };
        match mode {
            Mode::Normal => list.try_with_core(run),
            Mode::Dirty => list.with_core(run),
        }
    });
    encode::reply(env, result)
}

pub fn query<'a>(env: Env<'a>, list: Term<'a>, query: Term<'a>) -> Term<'a> {
    query_with(env, list, query, Mode::Normal)
}

pub fn query_dirty<'a>(env: Env<'a>, list: Term<'a>, query: Term<'a>) -> Term<'a> {
    query_with(env, list, query, Mode::Dirty)
}

pub fn counts<'a>(env: Env<'a>, counts: Term<'a>) -> Term<'a> {
    let (total, online) = counts
        .decode::<ResourceArc<Counts>>()
        .map(|cell| cell.read())
        .unwrap_or((0, 0));
    (total, online).encode(env)
}

pub fn release<'a>(env: Env<'a>, list: Term<'a>) -> Term<'a> {
    if let Ok(list) = handle(list) {
        list.shut();
    }
    encode::nil(env)
}

pub fn masks_set<'a>(env: Env<'a>, list: Term<'a>, updates: Term<'a>) -> Term<'a> {
    let result = handle(list).and_then(|list| {
        let updates = decoded(decode::mask_updates(updates))?;
        list.with_core(|core| {
            guild_rt::dirty_call();
            core.masks_set(&updates)?;
            Ok(encode::nil(env))
        })
    });
    encode::reply(env, result)
}

pub fn masks_query<'a>(env: Env<'a>, list: Term<'a>, query: Term<'a>) -> Term<'a> {
    let result = handle(list).and_then(|list| {
        let epoch = decoded(decode::mask_query(query))?;
        list.try_with_core(|core| {
            let reply = core.masks_query(epoch);
            charge_items(env, mask_items(&reply));
            Ok(encode::mask_reply(env, &reply))
        })
    });
    encode::reply(env, result)
}

fn mask_items(reply: &MaskReply) -> usize {
    match reply {
        MaskReply::Stale => 0,
        MaskReply::Masks { immediate, held } => immediate.len().saturating_add(held.len()),
    }
}

fn charge_items(env: Env<'_>, items: usize) {
    let percent = items
        .checked_div(ITEMS_PER_PERCENT)
        .unwrap_or(0)
        .saturating_add(1)
        .min(100);
    rustler::schedule::consume_timeslice(env, i32::try_from(percent).unwrap_or(100));
}
