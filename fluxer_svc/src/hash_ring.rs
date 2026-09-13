// SPDX-License-Identifier: AGPL-3.0-or-later

pub struct HashRing {
    shard_count: u32,
}

impl HashRing {
    pub fn new(shard_count: u32) -> Self {
        Self { shard_count }
    }

    pub fn shard_count(&self) -> u32 {
        self.shard_count
    }

    pub fn owner(&self, route_key: &str) -> u32 {
        let mut best_shard = 0u32;
        let mut best_score = 0u64;
        for shard_id in 0..self.shard_count {
            let score = rendezvous_score(route_key, shard_id);
            if score > best_score {
                best_score = score;
                best_shard = shard_id;
            }
        }
        best_shard
    }
}

fn rendezvous_score(key: &str, shard_id: u32) -> u64 {
    let mut hash = 0xcbf29ce484222325_u64;
    for byte in key.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash ^= b'|' as u64;
    hash = hash.wrapping_mul(0x100000001b3);
    let mut buf = [0u8; 10];
    let mut n = shard_id;
    let mut pos = buf.len();
    if n == 0 {
        pos -= 1;
        buf[pos] = b'0';
    } else {
        while n > 0 {
            pos -= 1;
            buf[pos] = b'0' + (n % 10) as u8;
            n /= 10;
        }
    }
    for byte in &buf[pos..] {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deterministic_routing() {
        let ring = HashRing::new(8);
        let key = "guild:123456";
        let first = ring.owner(key);
        for _ in 0..100 {
            assert_eq!(ring.owner(key), first);
        }
    }

    #[test]
    fn single_shard_always_zero() {
        let ring = HashRing::new(1);
        assert_eq!(ring.owner("anything"), 0);
        assert_eq!(ring.owner(""), 0);
    }

    #[test]
    fn distribution_across_shards() {
        let shard_count = 8;
        let ring = HashRing::new(shard_count);
        let mut counts = vec![0u32; shard_count as usize];
        for i in 0..10_000 {
            let key = format!("key:{i}");
            counts[ring.owner(&key) as usize] += 1;
        }
        for count in &counts {
            assert!(
                *count > 500,
                "shard received only {count} of 10000 keys across {shard_count} shards"
            );
        }
    }

    #[test]
    fn different_keys_can_route_to_different_shards() {
        let ring = HashRing::new(4);
        let mut seen = std::collections::HashSet::new();
        for i in 0..100 {
            seen.insert(ring.owner(&format!("key:{i}")));
        }
        assert!(seen.len() > 1, "all 100 keys routed to the same shard");
    }
}
