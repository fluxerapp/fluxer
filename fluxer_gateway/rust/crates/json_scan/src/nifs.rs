// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ops::Range;

use guild_rt::Scheduler;
use rustler::types::binary::{Binary, NewBinary};
use rustler::{Encoder, Env, Term};

use crate::{Key, Location, Malformed};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        malformed,
        found,
        absent,
    }
}

const DIRTY_BYTES: usize = 256 * 1024;
const DIRTY_ITEMS: usize = 4_096;
const BYTES_PER_PERCENT: usize = 16 * 1024;
const ITEMS_PER_PERCENT: usize = 256;

enum Scanned<'a> {
    Reply(Term<'a>),
    TooMany,
}

fn fits(on: Scheduler, items: usize) -> bool {
    on == Scheduler::Dirty || items <= DIRTY_ITEMS
}

fn malformed(env: Env<'_>) -> Term<'_> {
    (atoms::error(), atoms::malformed()).encode(env)
}

fn slice<'a>(binary: &Binary<'a>, range: Range<usize>) -> Result<Binary<'a>, Malformed> {
    binary
        .make_subbinary(range.start, range.len())
        .map_err(|_| Malformed)
}

fn key<'a>(env: Env<'a>, binary: &Binary<'a>, key: &Key) -> Result<Term<'a>, Malformed> {
    match key {
        Key::Plain(range) => {
            let raw = binary.as_slice().get(range.clone()).ok_or(Malformed)?;
            std::str::from_utf8(raw).map_err(|_| Malformed)?;
            Ok(slice(binary, range.clone())?.to_term(env))
        }
        Key::Escaped(range) => {
            let raw = binary.as_slice().get(range.clone()).ok_or(Malformed)?;
            let text = crate::unescape(raw)?;
            let mut out = NewBinary::new(env, text.len());
            out.as_mut_slice().copy_from_slice(text.as_bytes());
            Ok(Term::from(out))
        }
    }
}

fn members<'a>(env: Env<'a>, binary: &Binary<'a>, on: Scheduler) -> Result<Scanned<'a>, Malformed> {
    let members = crate::object(binary.as_slice())?;
    if !fits(on, members.len()) {
        return Ok(Scanned::TooMany);
    }
    let pairs = members
        .iter()
        .map(|member| {
            Ok((
                key(env, binary, &member.key)?,
                slice(binary, member.value.clone())?,
            ))
        })
        .collect::<Result<Vec<_>, Malformed>>()?;
    charge(env, on, binary.len(), pairs.len());
    Ok(Scanned::Reply((atoms::ok(), pairs).encode(env)))
}

fn items<'a>(env: Env<'a>, binary: &Binary<'a>, on: Scheduler) -> Result<Scanned<'a>, Malformed> {
    let ranges = crate::array(binary.as_slice())?;
    if !fits(on, ranges.len()) {
        return Ok(Scanned::TooMany);
    }
    let items = ranges
        .into_iter()
        .map(|range| slice(binary, range))
        .collect::<Result<Vec<_>, Malformed>>()?;
    charge(env, on, binary.len(), items.len());
    Ok(Scanned::Reply((atoms::ok(), items).encode(env)))
}

fn location<'a>(
    env: Env<'a>,
    binary: &Binary<'a>,
    wanted: &[u8],
    on: Scheduler,
) -> Result<Scanned<'a>, Malformed> {
    let found = match crate::locate(binary.as_slice(), wanted)? {
        Location::Found { value, cut } => {
            (atoms::found(), value.start, value.end, cut.start, cut.end).encode(env)
        }
        Location::Absent { close, empty } => (atoms::absent(), close, empty).encode(env),
    };
    charge(env, on, binary.len(), 0);
    Ok(Scanned::Reply((atoms::ok(), found).encode(env)))
}

fn charge(env: Env<'_>, on: Scheduler, bytes: usize, items: usize) {
    match on {
        Scheduler::Normal => {
            let percent = (bytes / BYTES_PER_PERCENT)
                .saturating_add(items / ITEMS_PER_PERCENT)
                .saturating_add(1)
                .min(100);
            rustler::schedule::consume_timeslice(env, i32::try_from(percent).unwrap_or(100));
        }
        Scheduler::Dirty => guild_rt::dirty_call(),
    }
}

fn run<'a>(
    env: Env<'a>,
    data: Term<'a>,
    on: Scheduler,
    body: impl FnOnce(&Binary<'a>) -> Result<Scanned<'a>, Malformed>,
) -> Option<Term<'a>> {
    let Ok(binary) = data.decode::<Binary>() else {
        return Some(malformed(env));
    };
    if on == Scheduler::Normal && binary.len() > DIRTY_BYTES {
        return None;
    }
    match body(&binary) {
        Ok(Scanned::Reply(term)) => Some(term),
        Ok(Scanned::TooMany) => None,
        Err(Malformed) => Some(malformed(env)),
    }
}

pub fn object<'a>(env: Env<'a>, data: Term<'a>, on: Scheduler) -> Option<Term<'a>> {
    run(env, data, on, |binary| members(env, binary, on))
}

pub fn array<'a>(env: Env<'a>, data: Term<'a>, on: Scheduler) -> Option<Term<'a>> {
    run(env, data, on, |binary| items(env, binary, on))
}

pub fn locate<'a>(
    env: Env<'a>,
    data: Term<'a>,
    wanted: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let Ok(wanted) = wanted.decode::<Binary>() else {
        return Some(malformed(env));
    };
    run(env, data, on, |binary| {
        location(env, binary, wanted.as_slice(), on)
    })
}
