// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;

use foldhash::fast::FixedState;
use hashbrown::{Equivalent, HashMap};

use crate::ids::ChannelId;
use crate::ranges::Range;

pub const LOOK_BEHIND: usize = 8;

pub const LOOK_AHEAD: usize = 8;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Payload(Arc<Vec<u8>>);

pub struct Guarded<'a> {
    pub lead: &'a [u8],
    pub bytes: &'a [u8],
    pub tail: &'a [u8],
}

impl Payload {
    pub fn guarded(&self) -> Guarded<'_> {
        let whole = self.0.as_slice();
        let end = whole.len().saturating_sub(LOOK_AHEAD).max(LOOK_BEHIND);
        let (lead, rest) = whole.split_at(LOOK_BEHIND.min(whole.len()));
        let (bytes, tail) = rest.split_at(end.saturating_sub(lead.len()).min(rest.len()));
        Guarded { lead, bytes, tail }
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.guarded().bytes
    }

    pub fn len(&self) -> usize {
        self.as_bytes().len()
    }

    pub fn is_empty(&self) -> bool {
        self.as_bytes().is_empty()
    }
}

pub struct PayloadWriter(Vec<u8>);

impl PayloadWriter {
    pub fn with_capacity(capacity: usize) -> PayloadWriter {
        let mut buf = Vec::with_capacity(
            capacity
                .saturating_add(LOOK_BEHIND)
                .saturating_add(LOOK_AHEAD),
        );
        buf.resize(LOOK_BEHIND, 0);
        PayloadWriter(buf)
    }

    pub fn out(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }

    pub fn finish(self) -> Payload {
        let PayloadWriter(mut buf) = self;
        buf.extend_from_slice(&[0; LOOK_AHEAD]);
        Payload(Arc::new(buf))
    }
}

type Key = (ChannelId, Box<[Range]>);

#[derive(Hash)]
struct KeyRef<'a>(ChannelId, &'a [Range]);

impl Equivalent<Key> for KeyRef<'_> {
    fn equivalent(&self, key: &Key) -> bool {
        self.0 == key.0 && self.1 == key.1.as_ref()
    }
}

#[derive(Debug)]
pub struct PayloadCache {
    last: HashMap<Key, (Box<[u64]>, Payload), FixedState>,
    bytes: usize,
    budget: usize,
}

fn cost(stamp: &[u64], payload: &Payload) -> usize {
    payload
        .len()
        .saturating_add(stamp.len().saturating_mul(8))
        .saturating_add(96)
}

impl PayloadCache {
    pub fn new(budget: usize, seed: u64) -> PayloadCache {
        PayloadCache {
            last: HashMap::with_hasher(FixedState::with_seed(seed.rotate_left(53))),
            bytes: 0,
            budget,
        }
    }

    pub fn bytes(&self) -> usize {
        self.bytes
    }

    pub fn get(&self, channel: ChannelId, ranges: &[Range], stamp: &[u64]) -> Option<Payload> {
        match self.last.get(&KeyRef(channel, ranges)) {
            Some((kept, payload)) if kept.as_ref() == stamp => Some(payload.clone()),
            _ => None,
        }
    }

    pub fn put(&mut self, channel: ChannelId, ranges: &[Range], stamp: Vec<u64>, payload: Payload) {
        let added = cost(&stamp, &payload);
        if added > self.budget {
            return;
        }
        if let Some((old_stamp, old)) = self.last.remove(&KeyRef(channel, ranges)) {
            self.bytes = self.bytes.saturating_sub(cost(&old_stamp, &old));
        }
        if self.bytes.saturating_add(added) > self.budget {
            self.last.clear();
            self.bytes = 0;
        }
        self.bytes = self.bytes.saturating_add(added);
        self.last.insert(
            (channel, ranges.into()),
            (stamp.into_boxed_slice(), payload),
        );
    }

    pub fn check(&self) -> Result<(), String> {
        let sum = self
            .last
            .values()
            .map(|(s, p)| cost(s, p))
            .fold(0usize, usize::saturating_add);
        if sum != self.bytes || self.bytes > self.budget {
            return Err(format!(
                "payload cache bytes {} sum {sum} budget {}",
                self.bytes, self.budget
            ));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{LOOK_AHEAD, LOOK_BEHIND, PayloadWriter};

    #[test]
    fn payload_bytes_sit_between_owned_guard_bytes() {
        for capacity in [0, 1, 7, 4096, 1 << 20] {
            let mut writer = PayloadWriter::with_capacity(capacity);
            writer.out().extend_from_slice(b"{\"ops\":[]}");
            let payload = writer.finish();
            let guarded = payload.guarded();
            assert_eq!(payload.as_bytes(), b"{\"ops\":[]}");
            assert_eq!(payload.len(), 10);
            assert_eq!(guarded.lead, &[0; LOOK_BEHIND]);
            assert_eq!(guarded.tail, &[0; LOOK_AHEAD]);
            assert_eq!(
                payload.0.as_ptr().addr() + LOOK_BEHIND,
                payload.as_bytes().as_ptr().addr()
            );
            assert_eq!(
                guarded.bytes.as_ptr().addr() + guarded.bytes.len(),
                guarded.tail.as_ptr().addr()
            );
            assert_eq!(payload.0.len(), LOOK_BEHIND + 10 + LOOK_AHEAD);
        }
        let empty = PayloadWriter::with_capacity(0).finish();
        assert!(empty.is_empty());
        assert_eq!(empty.guarded().lead, &[0; LOOK_BEHIND]);
        assert_eq!(empty.guarded().tail, &[0; LOOK_AHEAD]);
        assert_eq!(empty.0.len(), LOOK_BEHIND + LOOK_AHEAD);
    }

    #[test]
    fn a_body_that_ends_in_zero_bytes_keeps_them() {
        let mut writer = PayloadWriter::with_capacity(4);
        writer.out().extend_from_slice(&[1, 0, 0, 0]);
        let payload = writer.finish();
        assert_eq!(payload.as_bytes(), &[1, 0, 0, 0]);
        assert_eq!(payload.guarded().tail.len(), LOOK_AHEAD);
    }
}
