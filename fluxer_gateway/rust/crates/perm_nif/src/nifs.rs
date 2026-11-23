// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;

use guild_rt::Scheduler;
use perm_core::rolesets::DEAD;
use perm_core::{
    ChannelId, Effects, GuildId, MAX_CANDIDATES, MAX_EVENTS, Meter, Model, MutualOnline, Need,
    PermsError, PermsShared, PushPrune, Read, Scope, Status, Subject, ThreadAccess, Units, UserId,
    View, Visible,
};
use rustler::{Encoder, Env, LocalPid, NifResult, ResourceArc, Term};

use crate::decode::{self, BadInput, Decoder, Query, ReplyQuery};
use crate::encode::{self, atoms};
use crate::resource::{self, DeltaResource, ModelResource, PermsResource, ReaderResource};

pub const MAX_FILTER: usize = 1_000;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Mode {
    Normal,
    Dirty,
}

fn bad(input: BadInput) -> PermsError {
    PermsError::BadInput(input.0)
}

struct Target {
    shared: Arc<PermsShared>,
    pinned: Option<Arc<Model>>,
}

impl Target {
    fn of(term: Term<'_>) -> Result<Target, PermsError> {
        if let Ok(reader) = term.decode::<ResourceArc<ReaderResource>>() {
            return Ok(Target {
                shared: reader.shared().clone(),
                pinned: None,
            });
        }
        if let Ok(model) = term.decode::<ResourceArc<ModelResource>>() {
            return Ok(Target {
                shared: model.shared().clone(),
                pinned: Some(model.model().clone()),
            });
        }
        if let Ok(perms) = term.decode::<ResourceArc<PermsResource>>() {
            return Ok(Target {
                shared: perms.shared().clone(),
                pinned: None,
            });
        }
        Err(PermsError::BadInput(
            "reader: expected a permission reader or model".to_owned(),
        ))
    }

    fn view(&self, mode: Mode) -> Result<View<'_>, PermsError> {
        match (&self.pinned, mode) {
            (None, Mode::Normal) => self.shared.try_view(),
            (None, Mode::Dirty) => self.shared.view(),
            (Some(model), Mode::Normal) => self.shared.try_view_at(model.clone()),
            (Some(model), Mode::Dirty) => self.shared.view_at(model.clone()),
        }
    }

    fn point_view(&self, on: Scheduler) -> Result<View<'_>, PermsError> {
        match on {
            Scheduler::Normal => self.view(Mode::Normal),
            Scheduler::Dirty => self.view(Mode::Dirty),
        }
    }

    fn note_channel(&self, view: &View<'_>, channel: ChannelId) {
        self.shared.note_channel(view.model(), channel);
    }

    fn note_scope(&self, view: &View<'_>, scope: Scope) {
        if let Scope::Channel(channel) = scope {
            self.note_channel(view, channel);
        }
    }
}

fn perms_of(term: Term<'_>) -> Result<ResourceArc<PermsResource>, PermsError> {
    term.decode::<ResourceArc<PermsResource>>()
        .map_err(|_| PermsError::BadInput("perms: expected a permission handle".to_owned()))
}

fn metered<T>(
    env: Env<'_>,
    shared: &PermsShared,
    mode: Mode,
    estimate: Units,
    work: impl FnOnce(&mut Meter) -> Result<T, PermsError>,
) -> Result<T, PermsError> {
    let budget = shared.config().budget();
    if mode == Mode::Normal && !budget.fits(estimate) {
        return Err(PermsError::NeedsDirty);
    }
    if mode == Mode::Dirty {
        shared.note_dirty_read();
    }
    let mut meter = Meter::capped(budget.runaway_cap(estimate));
    let out = work(&mut meter)?;
    if meter.exhausted() {
        return Err(PermsError::Broken);
    }
    if mode != Mode::Dirty {
        let percent = meter.spent().fraction_percent(budget);
        rustler::schedule::consume_timeslice(env, i32::from(percent.max(1)));
    }
    Ok(out)
}

fn point_metered<T>(
    env: Env<'_>,
    shared: &PermsShared,
    on: Scheduler,
    estimate: Units,
    work: impl FnOnce(&mut Meter) -> Result<T, PermsError>,
) -> Result<T, PermsError> {
    let budget = shared.config().budget();
    if on == Scheduler::Normal && !budget.fits(estimate) {
        return Err(PermsError::NeedsDirty);
    }
    let mut meter = Meter::new();
    let out = work(&mut meter)?;
    match on {
        Scheduler::Normal => {
            let percent = meter.spent().fraction_percent(budget);
            rustler::schedule::consume_timeslice(env, i32::from(percent.max(1)));
        }
        Scheduler::Dirty => shared.note_dirty_read(),
    }
    Ok(out)
}

fn point<'a>(
    env: Env<'a>,
    on: Scheduler,
    result: Result<Term<'a>, PermsError>,
) -> Option<Term<'a>> {
    match (on, result) {
        (Scheduler::Normal, Err(PermsError::NeedsDirty)) => None,
        (_, result) => Some(encode::result(env, result)),
    }
}

fn users_of(ids: &[UserId]) -> Vec<Subject> {
    ids.iter().map(|user| Subject::Member(*user)).collect()
}

pub fn new<'a>(
    env: Env<'a>,
    guild: Term<'a>,
    config: Term<'a>,
    owner: Term<'a>,
) -> NifResult<Term<'a>> {
    let guild = GuildId::new(decode::id(guild, "guild").map_err(|_| rustler::Error::BadArg)?);
    let config = Decoder::new()
        .config(config)
        .map_err(|_| rustler::Error::BadArg)?;
    let owner = owner.decode::<LocalPid>()?;
    let sink: Arc<dyn perm_core::MetricsSink> =
        Arc::new(guild_rt::Sink(guild_rt::ResourceKind::PermIndex));
    let shared = PermsShared::with_sink(guild, config, sink);
    let perms = ResourceArc::new(PermsResource {
        shared: shared.clone(),
    });
    if perms.monitor(Some(env), &owner).is_none() {
        resource::release_now(&shared);
    }
    let reader = ResourceArc::new(ReaderResource { shared });
    Ok((perms, reader).encode(env))
}

const MIN_LOAD_UNITS_PER_ITEM: u64 = 8;

fn load_with<'a>(env: Env<'a>, perms: Term<'a>, snapshot: Term<'a>, mode: Mode) -> Term<'a> {
    let run = || -> Result<Term<'a>, PermsError> {
        let perms = perms_of(perms)?;
        if mode != Mode::Dirty {
            let limit = usize::try_from(
                perms.shared().config().budget().limit().get() / MIN_LOAD_UNITS_PER_ITEM,
            )
            .unwrap_or(usize::MAX);
            if decode::snapshot_items(snapshot, limit) > limit {
                return Err(PermsError::NeedsDirty);
            }
        }
        let snapshot = Decoder::new().snapshot(snapshot).map_err(bad)?;
        let epoch = match mode {
            Mode::Dirty => perms.shared().load_dirty(&snapshot)?,
            Mode::Normal => perms.shared().load(&snapshot)?,
        };
        if mode == Mode::Normal {
            let budget = perms.shared().config().budget();
            let percent = perm_core::load_estimate(&snapshot).fraction_percent(budget);
            rustler::schedule::consume_timeslice(env, i32::from(percent));
        }
        Ok(epoch.get().encode(env))
    };
    encode::result(env, run())
}

pub fn load<'a>(env: Env<'a>, perms: Term<'a>, snapshot: Term<'a>) -> Term<'a> {
    load_with(env, perms, snapshot, Mode::Normal)
}

pub fn load_dirty<'a>(env: Env<'a>, perms: Term<'a>, snapshot: Term<'a>) -> Term<'a> {
    load_with(env, perms, snapshot, Mode::Dirty)
}

fn effects<'a>(env: Env<'a>, shared: &Arc<PermsShared>, effects: &Effects) -> Term<'a> {
    let before = ResourceArc::new(ModelResource {
        shared: shared.clone(),
        model: effects.before().clone(),
    });
    let delta = ResourceArc::new(DeltaResource::new(
        effects.before().clone(),
        effects.after().clone(),
    ));
    let regroups: Vec<Term<'a>> = effects
        .regroups()
        .iter()
        .map(|r| encode::regroup(env, r))
        .collect();
    let changes: Vec<Term<'a>> = effects
        .changes()
        .iter()
        .map(|c| encode::visibility_change(env, c))
        .collect();
    let fields = [
        atoms::effects().encode(env),
        effects.epoch().get().encode(env),
        before.encode(env),
        regroups.encode(env),
        changes.encode(env),
        delta.encode(env),
        effects.compaction_due().encode(env),
        renumbered(env, effects),
    ];
    rustler::types::tuple::make_tuple(env, &fields)
}

fn renumbered<'a>(env: Env<'a>, effects: &Effects) -> Term<'a> {
    let Some(remap) = effects.roleset_remap() else {
        return encode::binary(env, &[]);
    };
    let live = remap.iter().filter(|new| **new != DEAD).count();
    let mut inverse = vec![0u32; live];
    for (old, new) in remap.iter().enumerate() {
        if let (Some(slot), Ok(old)) = (
            usize::try_from(*new)
                .ok()
                .and_then(|new| inverse.get_mut(new)),
            u32::try_from(old),
        ) {
            *slot = old;
        }
    }
    let bytes: Vec<u8> = inverse.iter().flat_map(|old| old.to_be_bytes()).collect();
    encode::binary(env, &bytes)
}

fn apply_with<'a>(env: Env<'a>, perms: Term<'a>, events: Term<'a>, mode: Mode) -> Term<'a> {
    let run = || -> Result<Term<'a>, PermsError> {
        let perms = perms_of(perms)?;
        let events = Decoder::new().events(events, MAX_EVENTS).map_err(bad)?;
        let shared = perms.shared();
        let out = match mode {
            Mode::Dirty => shared.apply_dirty(&events)?,
            Mode::Normal => shared.apply(&events)?,
        };
        if mode != Mode::Dirty {
            let percent = out.spent().fraction_percent(shared.config().budget());
            rustler::schedule::consume_timeslice(env, i32::from(percent.max(1)));
        }
        Ok(effects(env, shared, &out))
    };
    encode::result(env, run())
}

pub fn apply<'a>(env: Env<'a>, perms: Term<'a>, events: Term<'a>) -> Term<'a> {
    apply_with(env, perms, events, Mode::Normal)
}

pub fn apply_dirty<'a>(env: Env<'a>, perms: Term<'a>, events: Term<'a>) -> Term<'a> {
    apply_with(env, perms, events, Mode::Dirty)
}

fn compact_with<'a>(env: Env<'a>, perms: Term<'a>, mode: Mode) -> Term<'a> {
    let run = || -> Result<Term<'a>, PermsError> {
        let perms = perms_of(perms)?;
        let shared = perms.shared();
        let out = match mode {
            Mode::Dirty => shared.compact_dirty()?,
            Mode::Normal => shared.compact()?,
        };
        if mode != Mode::Dirty
            && let Some(found) = &out
        {
            let percent = found.spent().fraction_percent(shared.config().budget());
            rustler::schedule::consume_timeslice(env, i32::from(percent.max(1)));
        }
        let compacted = out.as_ref().map(|found| effects(env, shared, found));
        Ok(encode::option(env, compacted))
    };
    encode::result(env, run())
}

pub fn compact<'a>(env: Env<'a>, perms: Term<'a>) -> Term<'a> {
    compact_with(env, perms, Mode::Normal)
}

pub fn compact_dirty<'a>(env: Env<'a>, perms: Term<'a>) -> Term<'a> {
    compact_with(env, perms, Mode::Dirty)
}

pub fn model<'a>(env: Env<'a>, reader: Term<'a>) -> NifResult<Term<'a>> {
    let target = Target::of(reader).map_err(|_| rustler::Error::BadArg)?;
    let model = match target.pinned {
        Some(model) => model,
        None => target.shared.latest_model(),
    };
    Ok(ResourceArc::new(ModelResource {
        shared: target.shared,
        model,
    })
    .encode(env))
}

pub fn permissions<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    scope: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let user = decode::user(user, "user").map_err(bad)?;
        let scope = Decoder::new().scope(scope).map_err(bad)?;
        let view = target.point_view(on)?;
        target.note_scope(&view, scope);
        Ok(encode::permissions(env, view.permissions(user, scope)?))
    };
    point(env, on, run())
}

pub fn check<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    scope: Term<'a>,
    bits: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let user = decode::user(user, "user").map_err(bad)?;
        let scope = Decoder::new().scope(scope).map_err(bad)?;
        let bits = decode::permissions(bits, "bits").map_err(bad)?;
        let view = target.point_view(on)?;
        target.note_scope(&view, scope);
        Ok(view.check(user, scope, bits)?.encode(env))
    };
    point(env, on, run())
}

pub fn visible<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    channel: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let user = decode::user(user, "user").map_err(bad)?;
        let channel = decode::channel(channel, "channel").map_err(bad)?;
        let view = target.point_view(on)?;
        target.note_channel(&view, channel);
        let visible = view.visible(user, channel);
        Ok((
            visible.contains(Visible::VIEW),
            visible.contains(Visible::VIEW | Visible::VIEW_MEMBERS),
        )
            .encode(env))
    };
    point(env, on, run())
}

pub fn has_virtual_view<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    channel: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let user = decode::user(user, "user").map_err(bad)?;
        let channel = decode::channel(channel, "channel").map_err(bad)?;
        let view = target.point_view(on)?;
        Ok(view.has_virtual_view(user, channel).encode(env))
    };
    point(env, on, run())
}

pub fn viewable<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let user = decode::user(user, "user").map_err(bad)?;
        let view = target.point_view(on)?;
        let estimate = view.estimate(Read::Viewable);
        let list = point_metered(env, &target.shared, on, estimate, |meter| {
            Ok(view.viewable(user, meter))
        })?;
        Ok(encode::packed(env, list.iter().map(|c| c.get())))
    };
    point(env, on, run())
}

pub fn shares_view_many<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    candidates: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let user = decode::user(user, "user").map_err(bad)?;
        let candidates = Decoder::new()
            .users(candidates, MAX_CANDIDATES, "candidates")
            .map_err(bad)?;
        let view = target.point_view(on)?;
        let subjects = users_of(&candidates);
        let estimate = view.estimate(Read::FilterSharing(subjects.len()));
        let answers = point_metered(env, &target.shared, on, estimate, |meter| {
            view.filter_sharing(&Subject::Member(user), &subjects, meter)
        })?;
        let kept: Vec<u64> = candidates
            .iter()
            .zip(answers)
            .filter(|(_, keep)| *keep)
            .map(|(u, _)| u.get())
            .collect();
        Ok(encode::packed(env, kept.into_iter()))
    };
    point(env, on, run())
}

pub fn audience<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    channel: Term<'a>,
    need: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let channel = decode::channel(channel, "channel").map_err(bad)?;
        let need = Decoder::new().need(need).map_err(bad)?;
        let view = target.point_view(on)?;
        target.note_channel(&view, channel);
        let read = match need {
            Need::ChannelBits(_) | Need::GuildBits(_) => Read::AudienceBits,
            _ => Read::Audience,
        };
        let estimate = view.estimate(read);
        let audience = point_metered(env, &target.shared, on, estimate, |meter| {
            view.audience(channel, need, meter)
        })?;
        Ok(encode::audience(env, &audience))
    };
    point(env, on, run())
}

fn viewers_with<'a>(env: Env<'a>, reader: Term<'a>, channel: Term<'a>, mode: Mode) -> Term<'a> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let channel = decode::channel(channel, "channel").map_err(bad)?;
        if mode == Mode::Dirty && target.pinned.is_none() {
            target.shared.note_dirty_read();
            target
                .shared
                .note_channel(&target.shared.latest_model(), channel);
            let users = target.shared.viewers_chunked(channel)?;
            return Ok(encode::packed_users(env, &users));
        }
        let view = target.view(mode)?;
        target.note_channel(&view, channel);
        let estimate = view.estimate(Read::Viewers);
        let users = metered(env, &target.shared, mode, estimate, |meter| {
            Ok(view.viewers(channel, meter))
        })?;
        Ok(encode::packed_users(env, &users))
    };
    encode::result(env, run())
}

pub fn viewers<'a>(env: Env<'a>, reader: Term<'a>, channel: Term<'a>) -> Term<'a> {
    viewers_with(env, reader, channel, Mode::Normal)
}

pub fn viewers_dirty<'a>(env: Env<'a>, reader: Term<'a>, channel: Term<'a>) -> Term<'a> {
    viewers_with(env, reader, channel, Mode::Dirty)
}

pub fn filter_viewers<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    channel: Term<'a>,
    users: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let channel = decode::channel(channel, "channel").map_err(bad)?;
        let users = Decoder::new()
            .users(users, MAX_FILTER, "users")
            .map_err(bad)?;
        let view = target.point_view(on)?;
        target.note_channel(&view, channel);
        let estimate = view.estimate(Read::FilterViewers(users.len()));
        let kept = point_metered(env, &target.shared, on, estimate, |meter| {
            Ok(view.filter_viewers(channel, &users, meter))
        })?;
        Ok(encode::packed_users(env, &kept))
    };
    point(env, on, run())
}

pub fn thread_permissions<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    user: Term<'a>,
    parent: Term<'a>,
    (private, member, timed_out): (Term<'a>, Term<'a>, Term<'a>),
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let user = decode::user(user, "user").map_err(bad)?;
        let parent = decode::channel(parent, "parent").map_err(bad)?;
        let access = ThreadAccess {
            private: decode::boolean(private, "private").map_err(bad)?,
            member: decode::boolean(member, "member").map_err(bad)?,
            timed_out: decode::boolean(timed_out, "timed_out").map_err(bad)?,
        };
        let view = target.point_view(on)?;
        target.note_channel(&view, parent);
        Ok(encode::permissions(
            env,
            view.thread_permissions(user, parent, access)?,
        ))
    };
    point(env, on, run())
}

fn query_estimate(view: &View<'_>, query: &Query) -> Units {
    match query {
        Query::GroupOf(_) => view.estimate(Read::Point),
        Query::VisitorChannels(_) => view.estimate(Read::Everyone),
        Query::PublicOnline(users) | Query::MutualOnline(_, users) => {
            view.estimate(Read::Counts(users.len()))
        }
        Query::PushPrune(prune) => view.push_prune_estimate(&PushPrune {
            channel: prune.channel,
            mentioned: &prune.mentioned,
            roles: &prune.roles,
            everyone: prune.everyone,
        }),
    }
}

fn run_query<'a>(
    env: Env<'a>,
    view: &View<'_>,
    query: &Query,
    meter: &mut Meter,
) -> Result<Term<'a>, PermsError> {
    Ok(match query {
        Query::VisitorChannels(adult) => {
            let rows: Vec<(u64, u64)> = view
                .visitor_channels(*adult, meter)
                .into_iter()
                .map(|(c, p)| (c.get(), p.bits()))
                .collect();
            (atoms::channel_grants(), rows).encode(env)
        }
        Query::PublicOnline(users) => encode::public_online(env, view.public_online(users, meter)),
        Query::MutualOnline(requester, users) => {
            let value: MutualOnline = view.mutual_online(*requester, users, meter);
            encode::mutual_online(env, value)
        }
        Query::PushPrune(prune) => {
            let pruned = view.push_prune(
                &PushPrune {
                    channel: prune.channel,
                    mentioned: &prune.mentioned,
                    roles: &prune.roles,
                    everyone: prune.everyone,
                },
                meter,
            );
            (
                atoms::pruned(),
                encode::packed_users(env, &pruned.immediate),
                encode::packed_users(env, &pruned.held),
            )
                .encode(env)
        }
        Query::GroupOf(user) => {
            let group = view.group_of(*user).map(|g| encode::group(env, g));
            (atoms::group_reply(), encode::option(env, group)).encode(env)
        }
    })
}

fn query_with<'a>(env: Env<'a>, reader: Term<'a>, query: Term<'a>, mode: Mode) -> Term<'a> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let query = Decoder::new().query(query).map_err(bad)?;
        let view = target.view(mode)?;
        if let Query::PushPrune(prune) = &query {
            target.note_channel(&view, prune.channel);
        }
        let estimate = query_estimate(&view, &query);
        metered(env, &target.shared, mode, estimate, |meter| {
            run_query(env, &view, &query, meter)
        })
    };
    encode::result(env, run())
}

pub fn query<'a>(env: Env<'a>, reader: Term<'a>, query: Term<'a>) -> Term<'a> {
    query_with(env, reader, query, Mode::Normal)
}

pub fn query_dirty<'a>(env: Env<'a>, reader: Term<'a>, query: Term<'a>) -> Term<'a> {
    query_with(env, reader, query, Mode::Dirty)
}

pub fn reply<'a>(
    env: Env<'a>,
    reader: Term<'a>,
    query: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let run = || -> Result<Term<'a>, PermsError> {
        let target = Target::of(reader)?;
        let query = Decoder::new().reply_query(query).map_err(bad)?;
        let view = target.point_view(on)?;
        let bytes = match query {
            ReplyQuery::UserPermissions(user, scope) => {
                target.note_scope(&view, scope);
                view.user_permissions_reply(user, scope)?
            }
            ReplyQuery::CheckPermission(user, scope, bits) => {
                target.note_scope(&view, scope);
                view.check_permission_reply(user, scope, bits)?
            }
            ReplyQuery::ViewableChannels(user) => {
                let estimate = view.estimate(Read::Viewable);
                point_metered(env, &target.shared, on, estimate, |meter| {
                    Ok(view.viewable_channels_reply(user, meter))
                })?
            }
        };
        Ok(encode::binary(env, &bytes))
    };
    point(env, on, run())
}

pub fn epoch<'a>(env: Env<'a>, reader: Term<'a>) -> NifResult<Term<'a>> {
    match Target::of(reader).map_err(|_| rustler::Error::BadArg)? {
        Target {
            pinned: Some(model),
            ..
        } => Ok(model.epoch().get().encode(env)),
        target => Ok(target.shared.epoch().get().encode(env)),
    }
}

pub fn revision<'a>(env: Env<'a>, reader: Term<'a>) -> NifResult<Term<'a>> {
    let target = Target::of(reader).map_err(|_| rustler::Error::BadArg)?;
    Ok(target.shared.revision().encode(env))
}

pub fn status<'a>(env: Env<'a>, perms: Term<'a>) -> Term<'a> {
    let found = perms_of(perms).and_then(|perms| match perms.shared().status() {
        Status::Ready => Ok(atoms::nil().encode(env)),
        Status::Broken => Err(PermsError::Broken),
        Status::Released => Err(PermsError::Gone),
    });
    encode::result(env, found)
}

pub fn release<'a>(env: Env<'a>, perms: Term<'a>) -> Term<'a> {
    if let Ok(perms) = perms_of(perms) {
        resource::release(perms.shared());
    }
    atoms::nil().encode(env)
}
