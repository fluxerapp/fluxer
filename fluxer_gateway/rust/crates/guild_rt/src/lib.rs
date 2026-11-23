// SPDX-License-Identifier: AGPL-3.0-or-later

#![forbid(unsafe_code)]

mod dropper;
mod metrics;

pub use dropper::{drop_later, start};
pub use metrics::{ResourceKind, Sink, dirty_call, metrics};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Scheduler {
    Normal,
    Dirty,
}

impl perm_core::MetricsSink for Sink {
    fn opened(&self) {
        Sink::opened(*self);
    }

    fn closed(&self) {
        Sink::closed(*self);
    }

    fn bytes_moved(&self, old: u64, new: u64) {
        Sink::bytes_moved(*self, old, new);
    }

    fn dirty_call(&self) {
        dirty_call();
    }
}

#[cfg(test)]
mod tests {
    use super::{ResourceKind, Sink, drop_later, metrics, start};
    use std::time::{Duration, Instant};

    fn value(name: &str) -> u64 {
        metrics()
            .into_iter()
            .find(|(n, _)| *n == name)
            .map_or(0, |(_, v)| v)
    }

    #[test]
    fn counters_return_to_zero_after_the_dropper_runs() {
        assert!(start());
        let sink = Sink(ResourceKind::Fragments);
        sink.opened();
        sink.bytes_moved(0, 4_096);
        assert!(value("fragments_bytes") >= 4_096);
        sink.bytes_moved(4_096, 0);
        sink.closed();
        drop_later(vec![0u8; 1 << 20], ResourceKind::Fragments);
        let deadline = Instant::now() + Duration::from_secs(5);
        while value("fragments_dropping") != 0 && Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(1));
        }
        assert_eq!(value("fragments_dropping"), 0);
    }

    #[test]
    fn every_kind_is_reported() {
        let names: Vec<&str> = metrics().into_iter().map(|(n, _)| n).collect();
        for kind in ResourceKind::ALL {
            assert!(names.contains(&format!("{}_bytes", kind.name()).as_str()));
            assert!(names.contains(&format!("{}_live", kind.name()).as_str()));
        }
        assert!(names.contains(&"dropper_queue"));
        assert!(names.contains(&"dirty_calls"));
    }
}
