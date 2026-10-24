// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::{SERVICE_NAME, SnowflakeRequest, SnowflakeResponse};
use fluxer_svc::hash_ring::HashRing;
use fluxer_svc::router::RouterService;
use std::sync::atomic::{AtomicU32, Ordering};

pub struct SnowflakesRouter {
    next: AtomicU32,
    shard_count: u32,
}

impl SnowflakesRouter {
    pub fn new(shard_count: u32) -> Self {
        Self {
            next: AtomicU32::new(0),
            shard_count: shard_count.max(1),
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
    fn shard_candidates(&self, request: &Self::Request, ring: &HashRing) -> Vec<u32> {
        match request.routing_key() {
            Some(routing_key) => vec![ring.owner(routing_key)],
            None => {
                let first = self.next.fetch_add(1, Ordering::Relaxed) % self.shard_count;
                (0..self.shard_count)
                    .map(|offset| (first + offset) % self.shard_count)
                    .collect()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyed_requests_only_go_to_the_owner_and_unkeyed_ones_fail_over() {
        let router = SnowflakesRouter::new(8);
        let ring = HashRing::new(8);
        let keyed = SnowflakeRequest::GenerateBatch {
            count: 1,
            routing_key: Some("channel:1".to_owned()),
        };
        assert_eq!(
            router.shard_candidates(&keyed, &ring),
            vec![ring.owner("channel:1")]
        );
        let unkeyed = SnowflakeRequest::GenerateBatch {
            count: 1,
            routing_key: None,
        };
        assert_eq!(router.shard_candidates(&unkeyed, &ring).len(), 8);
    }
}
