// SPDX-License-Identifier: AGPL-3.0-or-later

use rustler::types::binary::{Binary, NewBinary, OwnedBinary};
use rustler::{Encoder, Env, ResourceArc, Term};

use guild_rt::Scheduler;

use crate::iodata::{Limit, Node, Refusal, gather};
use crate::resource::{CompressorResource, DecompressorResource, Locked};
use crate::stream::{
    Compressor, CompressorConfig, Decompressor, Output, StreamError, output_bound,
};

mod atoms {
    rustler::atoms! {
        ok,
        error,
        broken,
        bad_input,
        needs_dirty,
    }
}

fn ok<'a>(env: Env<'a>, value: impl Encoder) -> Term<'a> {
    (atoms::ok(), value).encode(env)
}

fn broken(env: Env<'_>) -> Term<'_> {
    (atoms::error(), atoms::broken()).encode(env)
}

fn bad_input<'a>(env: Env<'a>, reason: &str) -> Term<'a> {
    (atoms::error(), (atoms::bad_input(), reason)).encode(env)
}

const COPY_BOUND: usize = 4 * 1024;
const DIRTY_BYTES: usize = 64 * 1024;
const COMPRESS_BYTES_PER_PERCENT: usize = 4 * 1024;
const DECOMPRESS_BYTES_PER_PERCENT: usize = 8 * 1024;
const NORMAL_CELLS: usize = 64 * 1024;
const CELL_BYTES: usize = 2;

fn charge(env: Env<'_>, bytes: usize, per_percent: usize) {
    let percent = (bytes / per_percent).saturating_add(1).min(100);
    rustler::schedule::consume_timeslice(env, i32::try_from(percent).unwrap_or(100));
}

fn needs_dirty(env: Env<'_>) -> Term<'_> {
    (atoms::error(), atoms::needs_dirty()).encode(env)
}

fn copied<'a>(env: Env<'a>, bytes: &[u8]) -> Term<'a> {
    let mut out = NewBinary::new(env, bytes.len());
    out.as_mut_slice().copy_from_slice(bytes);
    ok(env, Term::from(out))
}

struct BinaryOutput(OwnedBinary);

impl Output for BinaryOutput {
    type Buffer = [u8];

    fn buffer(&mut self) -> &mut [u8] {
        self.0.as_mut_slice()
    }

    fn capacity(&self) -> usize {
        self.0.len()
    }

    fn reserve(&mut self, total: usize) -> Result<(), StreamError> {
        if total <= self.0.len() || self.0.realloc(total) {
            Ok(())
        } else {
            Err(StreamError::OutOfMemory)
        }
    }
}

fn written<'a>(env: Env<'a>, out: BinaryOutput, size: usize) -> Term<'a> {
    let BinaryOutput(mut bytes) = out;
    bytes.realloc_or_copy(size);
    ok(env, bytes.release(env))
}

impl<'a> Node<'a> for Term<'a> {
    fn bytes(self) -> Option<&'a [u8]> {
        Binary::from_term(self).ok().map(|binary| binary.as_slice())
    }

    fn byte(self) -> Option<u8> {
        self.decode::<u32>()
            .ok()
            .and_then(|value| u8::try_from(value).ok())
    }

    fn is_nil(self) -> bool {
        self.is_empty_list()
    }

    fn cell(self) -> Option<(Term<'a>, Term<'a>)> {
        self.list_get_cell().ok()
    }
}

const NORMAL_LIMIT: Limit = Limit {
    bytes: DIRTY_BYTES,
    cells: NORMAL_CELLS,
};

fn compress_error(env: Env<'_>, error: StreamError) -> Term<'_> {
    match error {
        StreamError::BadParameter(name) => bad_input(env, name),
        StreamError::OutOfMemory | StreamError::Zstd(_) | StreamError::Stuck => broken(env),
        StreamError::TooLarge => bad_input(env, "too_large"),
    }
}

fn decompress_error(env: Env<'_>, error: StreamError) -> Term<'_> {
    match error {
        StreamError::OutOfMemory => broken(env),
        other => bad_input(env, other.reason()),
    }
}

fn compress_chunks<'a>(env: Env<'a>, compressor: &mut Compressor, chunks: &[&[u8]]) -> Term<'a> {
    let bound = output_bound(chunks);
    if bound <= COPY_BOUND {
        let mut out = Vec::with_capacity(bound);
        return match compressor.compress_into(chunks, &mut out) {
            Ok(_) => copied(env, &out),
            Err(error) => compress_error(env, error),
        };
    }
    let mut out = BinaryOutput(OwnedBinary::new(bound));
    match compressor.compress_into(chunks, &mut out) {
        Ok(size) => written(env, out, size),
        Err(error) => compress_error(env, error),
    }
}

pub fn compressor_new<'a>(
    env: Env<'a>,
    level: Term<'a>,
    window_log: Term<'a>,
    max_block_size: Term<'a>,
) -> Term<'a> {
    let Some(level) = level.decode::<i32>().ok() else {
        return bad_input(env, "level");
    };
    let Some(window_log) = window_log.decode::<u32>().ok() else {
        return bad_input(env, "window_log");
    };
    let Some(max_block_size) = max_block_size.decode::<u32>().ok() else {
        return bad_input(env, "max_block_size");
    };
    let config = CompressorConfig {
        level,
        window_log,
        max_block_size,
    };
    match Compressor::new(config) {
        Ok(compressor) => ok(env, ResourceArc::new(CompressorResource::new(compressor))),
        Err(error) => compress_error(env, error),
    }
}

pub fn compress<'a>(env: Env<'a>, compressor: Term<'a>, data: Term<'a>) -> Term<'a> {
    compress_on(env, compressor, data, Scheduler::Normal)
}

pub fn compress_dirty<'a>(env: Env<'a>, compressor: Term<'a>, data: Term<'a>) -> Term<'a> {
    guild_rt::dirty_call();
    compress_on(env, compressor, data, Scheduler::Dirty)
}

fn compress_on<'a>(
    env: Env<'a>,
    compressor: Term<'a>,
    data: Term<'a>,
    scheduler: Scheduler,
) -> Term<'a> {
    let Ok(resource) = compressor.decode::<ResourceArc<CompressorResource>>() else {
        return bad_input(env, "compressor");
    };
    let mut compressor = match resource.lock(scheduler) {
        Locked::Held(guard) => guard,
        Locked::Busy => return needs_dirty(env),
        Locked::Poisoned => return broken(env),
    };
    let limit = (scheduler == Scheduler::Normal).then_some(NORMAL_LIMIT);
    let gathered = match gather(data, limit) {
        Ok(gathered) => gathered,
        Err(Refusal::OverLimit) => return needs_dirty(env),
        Err(Refusal::Invalid) => return bad_input(env, "data"),
    };
    let out = compress_chunks(env, &mut compressor, &gathered.chunks());
    if scheduler == Scheduler::Normal {
        charge(
            env,
            gathered
                .total()
                .saturating_add(gathered.cells().saturating_mul(CELL_BYTES)),
            COMPRESS_BYTES_PER_PERCENT,
        );
    }
    out
}

pub fn decompressor_new<'a>(env: Env<'a>, window_log_max: Term<'a>) -> Term<'a> {
    let Some(window_log_max) = window_log_max.decode::<u32>().ok() else {
        return bad_input(env, "window_log_max");
    };
    match Decompressor::new(window_log_max) {
        Ok(decompressor) => ok(
            env,
            ResourceArc::new(DecompressorResource::new(decompressor)),
        ),
        Err(error) => decompress_error(env, error),
    }
}

pub fn decompress<'a>(
    env: Env<'a>,
    decompressor: Term<'a>,
    data: Term<'a>,
    max_size: Term<'a>,
    on: Scheduler,
) -> Option<Term<'a>> {
    let Ok(resource) = decompressor.decode::<ResourceArc<DecompressorResource>>() else {
        return Some(bad_input(env, "decompressor"));
    };
    let Ok(data) = data.decode::<Binary>() else {
        return Some(bad_input(env, "data"));
    };
    let Some(max_size) = max_size
        .decode::<u64>()
        .ok()
        .and_then(|n| usize::try_from(n).ok())
    else {
        return Some(bad_input(env, "max_size"));
    };
    if on == Scheduler::Normal && (data.len() > DIRTY_BYTES || max_size > DIRTY_BYTES) {
        return None;
    }
    let mut decompressor = match resource.lock(on) {
        Locked::Held(guard) => guard,
        Locked::Busy => return None,
        Locked::Poisoned => return Some(broken(env)),
    };
    let out = match decompressor.decompress(data.as_slice(), max_size) {
        Ok(bytes) => {
            if on == Scheduler::Normal {
                charge(
                    env,
                    data.len().saturating_add(bytes.len()),
                    DECOMPRESS_BYTES_PER_PERCENT,
                );
            }
            copied(env, &bytes)
        }
        Err(error) => decompress_error(env, error),
    };
    if on == Scheduler::Dirty {
        guild_rt::dirty_call();
    }
    Some(out)
}
