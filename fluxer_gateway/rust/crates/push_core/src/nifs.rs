// SPDX-License-Identifier: AGPL-3.0-or-later

use guild_rt::Scheduler;
use rustler::types::binary::{Binary, OwnedBinary};
use rustler::{Encoder, Env, Term};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        bad_input,
    }
}

fn bad_input<'a>(env: Env<'a>, reason: &str) -> Term<'a> {
    (atoms::error(), (atoms::bad_input(), reason)).encode(env)
}

fn decode_ids(users: Term<'_>) -> Option<Vec<u64>> {
    let mut ids = Vec::with_capacity(users.list_length().ok()?);
    for user in users.into_list_iterator().ok()? {
        ids.push(user.decode::<u64>().ok()?);
    }
    Some(ids)
}

const IDS_PER_PERCENT: usize = 512;
const BYTES_PER_PERCENT: usize = 16 * 1024;

pub fn job_body<'a>(env: Env<'a>, head: Term<'a>, users: Term<'a>, tail: Term<'a>) -> Term<'a> {
    body(env, head, users, tail, Scheduler::Normal)
}

pub fn job_body_dirty<'a>(
    env: Env<'a>,
    head: Term<'a>,
    users: Term<'a>,
    tail: Term<'a>,
) -> Term<'a> {
    guild_rt::dirty_call();
    body(env, head, users, tail, Scheduler::Dirty)
}

fn body<'a>(
    env: Env<'a>,
    head: Term<'a>,
    users: Term<'a>,
    tail: Term<'a>,
    on: Scheduler,
) -> Term<'a> {
    let Ok(head) = head.decode::<Binary>() else {
        return bad_input(env, "head");
    };
    let Ok(tail) = tail.decode::<Binary>() else {
        return bad_input(env, "tail");
    };
    let Some(ids) = decode_ids(users) else {
        return bad_input(env, "users");
    };
    let len = crate::job::body_len(&head, &ids, &tail);
    let mut out = OwnedBinary::new(len);
    let written = match crate::job::write_body(out.as_mut_slice(), &head, &ids, &tail) {
        Ok(()) => (atoms::ok(), out.release(env)).encode(env),
        Err(_) => bad_input(env, "size"),
    };
    if on == Scheduler::Normal {
        let percent = (ids.len() / IDS_PER_PERCENT)
            .saturating_add(len / BYTES_PER_PERCENT)
            .saturating_add(1)
            .min(100);
        rustler::schedule::consume_timeslice(env, i32::try_from(percent).unwrap_or(100));
    }
    written
}
