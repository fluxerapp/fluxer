// SPDX-License-Identifier: AGPL-3.0-or-later

use ml_core::render::Sent;
use ml_core::{ChannelId, Done, MaskReply, Payload, Ranges, Step, Target, TimerAction, UserId};
use rustler::{Binary, Encoder, Env, NewBinary, ResourceArc, Term};

use crate::atoms;
use crate::resource::{Failure, PayloadRes, Subscriber};

pub fn ok<'a>(env: Env<'a>, value: Term<'a>) -> Term<'a> {
    (atoms::ok(), value).encode(env)
}

pub fn nil<'a>(env: Env<'a>) -> Term<'a> {
    atoms::nil().encode(env)
}

pub fn failure<'a>(env: Env<'a>, failure: &Failure) -> Term<'a> {
    let reason = match failure {
        Failure::NeedsDirty => atoms::needs_dirty().encode(env),
        Failure::Broken => atoms::broken().encode(env),
        Failure::Gone => atoms::gone().encode(env),
        Failure::BadInput(path) => (atoms::bad_input(), binary(env, path.as_bytes())).encode(env),
    };
    (atoms::error(), reason).encode(env)
}

pub fn reply<'a>(env: Env<'a>, result: Result<Term<'a>, Failure>) -> Term<'a> {
    match result {
        Ok(value) => ok(env, value),
        Err(error) => failure(env, &error),
    }
}

pub fn binary<'a>(env: Env<'a>, bytes: &[u8]) -> Term<'a> {
    let mut out = NewBinary::new(env, bytes.len());
    out.as_mut_slice().copy_from_slice(bytes);
    Binary::from(out).to_term(env)
}

pub fn payload<'a>(env: Env<'a>, payload: &Payload) -> Term<'a> {
    let held = ResourceArc::new(PayloadRes::new(payload.clone()));
    held.make_binary(env, PayloadRes::vm_bytes).to_term(env)
}

pub fn packed_ids<'a>(env: Env<'a>, users: &[UserId]) -> Term<'a> {
    let mut out = NewBinary::new(env, users.len().saturating_mul(8));
    for (chunk, user) in out
        .as_mut_slice()
        .as_chunks_mut::<8>()
        .0
        .iter_mut()
        .zip(users)
    {
        *chunk = user.get().to_be_bytes();
    }
    (atoms::packed_ids(), Binary::from(out).to_term(env)).encode(env)
}

pub fn ranges<'a>(env: Env<'a>, ranges: &Ranges) -> Term<'a> {
    ranges.to_pairs().encode(env)
}

fn timer<'a>(env: Env<'a>, action: TimerAction) -> Term<'a> {
    match action {
        TimerAction::KeepTimer => atoms::keep_timer().encode(env),
        TimerAction::ArmTimer => atoms::arm_timer().encode(env),
        TimerAction::CancelTimer => atoms::cancel_timer().encode(env),
    }
}

fn target<'a>(env: Env<'a>, target: &Target<Subscriber>) -> Term<'a> {
    match target {
        Target::Sessions(subscribers) => {
            let raw: Vec<u64> = subscribers.iter().map(|s| s.0).collect();
            (atoms::to_sessions(), raw).encode(env)
        }
        Target::Session(subscriber) => (atoms::to_session(), subscriber.0).encode(env),
    }
}

fn dispatches<'a>(env: Env<'a>, sent: &[Sent<Subscriber>]) -> Term<'a> {
    let items: Vec<Term<'a>> = sent
        .iter()
        .map(|s| {
            (
                atoms::dispatch(),
                target(env, &s.target),
                payload(env, &s.payload),
            )
                .encode(env)
        })
        .collect();
    items.encode(env)
}

fn effects<'a>(
    env: Env<'a>,
    sent: &[Sent<Subscriber>],
    action: TimerAction,
    skipped_absent: u64,
) -> Term<'a> {
    (
        atoms::effects(),
        dispatches(env, sent),
        timer(env, action),
        skipped_absent,
    )
        .encode(env)
}

pub fn step<'a>(env: Env<'a>, step: &Step<Subscriber>) -> Term<'a> {
    match step {
        Step::Done(Done::Sync(rendered)) => (
            atoms::done(),
            effects(env, &rendered.sent, rendered.timer, rendered.skipped_absent),
        )
            .encode(env),
        Step::Done(Done::Page(page)) => (
            atoms::page_done(),
            (atoms::page(), page.total, payload(env, &page.payload)).encode(env),
        )
            .encode(env),
        Step::Partial(sent) => (
            atoms::partial(),
            effects(env, sent, TimerAction::KeepTimer, 0),
        )
            .encode(env),
        Step::NeedItems(users) => {
            let raw: Vec<u64> = users.iter().map(|u| u.get()).collect();
            (atoms::need_items(), raw).encode(env)
        }
    }
}

pub fn session_ranges<'a>(env: Env<'a>, found: Option<(ChannelId, Ranges)>) -> Term<'a> {
    match found {
        Some((channel, list)) => (
            atoms::some(),
            (channel.get(), ranges(env, &list)).encode(env),
        )
            .encode(env),
        None => atoms::none().encode(env),
    }
}

pub fn channel_counts<'a>(env: Env<'a>, counts: &[(ChannelId, u32, u32)]) -> Term<'a> {
    let items: Vec<Term<'a>> = counts
        .iter()
        .map(|(channel, total, online)| (channel.get(), *total, *online).encode(env))
        .collect();
    items.encode(env)
}

pub fn mask_reply<'a>(env: Env<'a>, reply: &MaskReply) -> Term<'a> {
    match reply {
        MaskReply::Stale => atoms::stale().encode(env),
        MaskReply::Masks { immediate, held } => (
            atoms::masks(),
            packed_ids(env, immediate),
            packed_ids(env, held),
        )
            .encode(env),
    }
}
