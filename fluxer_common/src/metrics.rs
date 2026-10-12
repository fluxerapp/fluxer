// SPDX-License-Identifier: AGPL-3.0-or-later

use libc::{CLOCK_MONOTONIC, clock_gettime, timespec};
use std::fmt::{self, Write as _};
use std::net::SocketAddr;
use std::sync::atomic::{AtomicU64, Ordering};

const HISTOGRAM_BUCKETS_MS: &[u64] = &[
    1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 30000,
];

pub struct Histogram {
    buckets: [AtomicU64; 13],
    inf: AtomicU64,
    sum_ms: AtomicU64,
    count: AtomicU64,
}

impl Histogram {
    pub const fn new() -> Self {
        Self {
            buckets: [const { AtomicU64::new(0) }; 13],
            inf: AtomicU64::new(0),
            sum_ms: AtomicU64::new(0),
            count: AtomicU64::new(0),
        }
    }

    pub fn observe(&self, ms: u64) {
        for (index, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
            if ms <= upper {
                self.buckets[index].fetch_add(1, Ordering::Relaxed);
                break;
            }
        }
        self.inf.fetch_add(1, Ordering::Relaxed);
        self.sum_ms.fetch_add(ms, Ordering::Relaxed);
        self.count.fetch_add(1, Ordering::Relaxed);
    }

    pub fn render_series(&self, out: &mut String, name: &str, labels: Option<&str>) -> fmt::Result {
        let labels = labels.filter(|labels| !labels.is_empty());
        let prefix = labels
            .map(|labels| format!("{labels},"))
            .unwrap_or_default();
        let suffix = labels
            .map(|labels| format!("{{{labels}}}"))
            .unwrap_or_default();
        let mut cumulative = 0;
        for (index, upper) in HISTOGRAM_BUCKETS_MS.iter().copied().enumerate() {
            cumulative += self.buckets[index].load(Ordering::Relaxed);
            writeln!(out, "{name}_bucket{{{prefix}le=\"{upper}\"}} {cumulative}")?;
        }
        writeln!(
            out,
            "{name}_bucket{{{prefix}le=\"+Inf\"}} {}",
            self.inf.load(Ordering::Relaxed)
        )?;
        writeln!(
            out,
            "{name}_sum{suffix} {}",
            self.sum_ms.load(Ordering::Relaxed)
        )?;
        writeln!(
            out,
            "{name}_count{suffix} {}",
            self.count.load(Ordering::Relaxed)
        )
    }
}

impl Default for Histogram {
    fn default() -> Self {
        Self::new()
    }
}

pub fn now_ms() -> i64 {
    let mut ts = timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    let rc = unsafe { clock_gettime(CLOCK_MONOTONIC, &mut ts) };
    if rc != 0 {
        return 0;
    }
    ts.tv_sec
        .saturating_mul(1_000)
        .saturating_add(ts.tv_nsec / 1_000_000)
}

pub fn render_counter(out: &mut String, name: &str, counter: &AtomicU64) -> fmt::Result {
    writeln!(out, "# TYPE {name} counter")?;
    writeln!(out, "{name} {}", counter.load(Ordering::Relaxed))
}

pub fn is_loopback_peer(peer: &SocketAddr) -> bool {
    peer.ip().to_canonical().is_loopback()
}

#[cfg(test)]
mod tests {
    use super::is_loopback_peer;
    use std::net::SocketAddr;

    fn peer(value: &str) -> SocketAddr {
        value.parse().expect("valid socket address")
    }

    #[test]
    fn accepts_loopback_peers() {
        assert!(is_loopback_peer(&peer("127.0.0.1:5000")));
        assert!(is_loopback_peer(&peer("127.0.0.2:5000")));
        assert!(is_loopback_peer(&peer("[::1]:5000")));
        assert!(is_loopback_peer(&peer("[::ffff:127.0.0.1]:5000")));
    }

    #[test]
    fn rejects_remote_peers() {
        assert!(!is_loopback_peer(&peer("8.8.8.8:5000")));
        assert!(!is_loopback_peer(&peer("10.0.0.5:5000")));
        assert!(!is_loopback_peer(&peer("172.18.0.4:5000")));
        assert!(!is_loopback_peer(&peer("[fe80::1]:5000")));
        assert!(!is_loopback_peer(&peer("[::ffff:8.8.8.8]:5000")));
    }
}
