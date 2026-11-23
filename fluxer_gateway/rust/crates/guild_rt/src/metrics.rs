// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ResourceKind {
    PermIndex,
    MemberListCore,
    Fragments,
    PayloadCache,
    ZstdContext,
}

impl ResourceKind {
    pub const ALL: [ResourceKind; 5] = [
        ResourceKind::PermIndex,
        ResourceKind::MemberListCore,
        ResourceKind::Fragments,
        ResourceKind::PayloadCache,
        ResourceKind::ZstdContext,
    ];

    pub fn name(self) -> &'static str {
        match self {
            ResourceKind::PermIndex => "perm_index",
            ResourceKind::MemberListCore => "member_list_core",
            ResourceKind::Fragments => "fragments",
            ResourceKind::PayloadCache => "payload_cache",
            ResourceKind::ZstdContext => "zstd_context",
        }
    }
}

struct Counters {
    bytes: AtomicU64,
    live: AtomicU64,
    dropping: AtomicU64,
}

impl Counters {
    const fn new() -> Counters {
        Counters {
            bytes: AtomicU64::new(0),
            live: AtomicU64::new(0),
            dropping: AtomicU64::new(0),
        }
    }
}

static PERM_INDEX: Counters = Counters::new();
static MEMBER_LIST_CORE: Counters = Counters::new();
static FRAGMENTS: Counters = Counters::new();
static PAYLOAD_CACHE: Counters = Counters::new();
static ZSTD_CONTEXT: Counters = Counters::new();

static DROPPER_QUEUE: AtomicU64 = AtomicU64::new(0);
static DROPPER_INLINE: AtomicU64 = AtomicU64::new(0);
static DIRTY_CALLS: AtomicU64 = AtomicU64::new(0);

fn counters(kind: ResourceKind) -> &'static Counters {
    match kind {
        ResourceKind::PermIndex => &PERM_INDEX,
        ResourceKind::MemberListCore => &MEMBER_LIST_CORE,
        ResourceKind::Fragments => &FRAGMENTS,
        ResourceKind::PayloadCache => &PAYLOAD_CACHE,
        ResourceKind::ZstdContext => &ZSTD_CONTEXT,
    }
}

fn increment(counter: &AtomicU64, by: u64) {
    let _ = counter.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |n| {
        Some(n.saturating_add(by))
    });
}

fn decrement(counter: &AtomicU64, by: u64) {
    let _ = counter.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |n| {
        Some(n.saturating_sub(by))
    });
}

pub(crate) fn queued(kind: ResourceKind) {
    increment(&DROPPER_QUEUE, 1);
    increment(&counters(kind).dropping, 1);
}

pub(crate) fn dequeued(kind: ResourceKind) {
    decrement(&DROPPER_QUEUE, 1);
    decrement(&counters(kind).dropping, 1);
}

pub(crate) fn dropped_inline() {
    increment(&DROPPER_INLINE, 1);
}

pub fn dirty_call() {
    increment(&DIRTY_CALLS, 1);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Sink(pub ResourceKind);

impl Sink {
    pub fn opened(self) {
        increment(&counters(self.0).live, 1);
    }

    pub fn closed(self) {
        decrement(&counters(self.0).live, 1);
    }

    pub fn add_bytes(self, bytes: u64) {
        increment(&counters(self.0).bytes, bytes);
    }

    pub fn sub_bytes(self, bytes: u64) {
        decrement(&counters(self.0).bytes, bytes);
    }

    pub fn bytes_moved(self, old: u64, new: u64) {
        if new >= old {
            self.add_bytes(new - old);
        } else {
            self.sub_bytes(old - new);
        }
    }
}

pub fn metrics() -> Vec<(&'static str, u64)> {
    let mut out = Vec::with_capacity(ResourceKind::ALL.len() * 3 + 3);
    for kind in ResourceKind::ALL {
        let c = counters(kind);
        let (bytes, live, dropping) = names(kind);
        out.push((bytes, c.bytes.load(Ordering::Relaxed)));
        out.push((live, c.live.load(Ordering::Relaxed)));
        out.push((dropping, c.dropping.load(Ordering::Relaxed)));
    }
    out.push(("dropper_queue", DROPPER_QUEUE.load(Ordering::Relaxed)));
    out.push((
        "dropper_inline_drops",
        DROPPER_INLINE.load(Ordering::Relaxed),
    ));
    out.push(("dirty_calls", DIRTY_CALLS.load(Ordering::Relaxed)));
    out
}

fn names(kind: ResourceKind) -> (&'static str, &'static str, &'static str) {
    match kind {
        ResourceKind::PermIndex => ("perm_index_bytes", "perm_index_live", "perm_index_dropping"),
        ResourceKind::MemberListCore => (
            "member_list_core_bytes",
            "member_list_core_live",
            "member_list_core_dropping",
        ),
        ResourceKind::Fragments => ("fragments_bytes", "fragments_live", "fragments_dropping"),
        ResourceKind::PayloadCache => (
            "payload_cache_bytes",
            "payload_cache_live",
            "payload_cache_dropping",
        ),
        ResourceKind::ZstdContext => (
            "zstd_context_bytes",
            "zstd_context_live",
            "zstd_context_dropping",
        ),
    }
}
