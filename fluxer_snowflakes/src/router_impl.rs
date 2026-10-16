// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::{SERVICE_NAME, SnowflakeRequest, SnowflakeResponse};
use fluxer_svc::hash_ring::HashRing;
use fluxer_svc::router::RouterService;
use std::sync::atomic::{AtomicU32, Ordering};

pub struct RoundRobinShardPicker {
    next: AtomicU32,
    shard_count: u32,
}

impl RoundRobinShardPicker {
    pub fn new(shard_count: u32) -> Self {
        Self {
            next: AtomicU32::new(0),
            shard_count: shard_count.max(1),
        }
    }

    pub fn next_shard(&self) -> u32 {
        self.next.fetch_add(1, Ordering::Relaxed) % self.shard_count
    }
}

pub struct SnowflakeShardPicker {
    round_robin: RoundRobinShardPicker,
    hash_ring: HashRing,
}

impl SnowflakeShardPicker {
    pub fn new(shard_count: u32) -> Self {
        Self {
            round_robin: RoundRobinShardPicker::new(shard_count),
            hash_ring: HashRing::new(shard_count.max(1)),
        }
    }

    pub fn pick_shard(&self, request: &SnowflakeRequest) -> u32 {
        match request.routing_key() {
            Some(routing_key) => self.hash_ring.owner(routing_key),
            None => self.round_robin.next_shard(),
        }
    }
}

pub struct SnowflakesRouter {
    picker: SnowflakeShardPicker,
}

impl SnowflakesRouter {
    pub fn new(shard_count: u32) -> Self {
        Self {
            picker: SnowflakeShardPicker::new(shard_count),
        }
    }
}

impl RouterService for SnowflakesRouter {
    type Request = SnowflakeRequest;
    type Response = SnowflakeResponse;
    const CACHES_RESPONSES: bool = false;
    fn service_name(&self) -> &str {
        SERVICE_NAME
    }
    fn route_key(request: &Self::Request) -> String {
        request.routing_key().unwrap_or_default().to_owned()
    }
    fn shard_for(&self, request: &Self::Request, _ring: &HashRing) -> u32 {
        self.picker.pick_shard(request)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn picker_round_robins_across_shards() {
        let picker = RoundRobinShardPicker::new(3);
        let shards = (0..8).map(|_| picker.next_shard()).collect::<Vec<_>>();
        assert_eq!(shards, vec![0, 1, 2, 0, 1, 2, 0, 1]);
    }

    #[test]
    fn picker_handles_atomic_wraparound() {
        let picker = RoundRobinShardPicker::new(4);
        picker.next.store(u32::MAX - 1, Ordering::Relaxed);
        let shards = (0..4).map(|_| picker.next_shard()).collect::<Vec<_>>();
        assert_eq!(shards, vec![2, 3, 0, 1]);
    }

    #[test]
    fn picker_routes_same_key_to_same_shard() {
        let picker = SnowflakeShardPicker::new(8);
        let request = SnowflakeRequest::GenerateBatch {
            count: 1,
            routing_key: Some("channel:1510189013330296832".to_owned()),
        };
        let first = picker.pick_shard(&request);
        for _ in 0..100 {
            assert_eq!(picker.pick_shard(&request), first);
        }
    }

    #[test]
    fn picker_round_robins_unrouted_requests() {
        let picker = SnowflakeShardPicker::new(3);
        let request = SnowflakeRequest::GenerateBatch {
            count: 1,
            routing_key: None,
        };
        let shards = (0..8)
            .map(|_| picker.pick_shard(&request))
            .collect::<Vec<_>>();
        assert_eq!(shards, vec![0, 1, 2, 0, 1, 2, 0, 1]);
    }
}
