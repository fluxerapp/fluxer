// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

pub trait MetricsSink: Send + Sync {
    fn opened(&self) {}

    fn closed(&self) {}

    fn bytes_moved(&self, old: u64, new: u64) {
        let _ = (old, new);
    }

    fn dirty_call(&self) {}
}

pub(crate) struct Metrics {
    sink: Option<Arc<dyn MetricsSink>>,
    reported: AtomicU64,
}

impl fmt::Debug for Metrics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Metrics")
            .field("sink", &self.sink.is_some())
            .field("reported", &self.reported.load(Ordering::Relaxed))
            .finish()
    }
}

impl Metrics {
    pub(crate) fn new(sink: Option<Arc<dyn MetricsSink>>) -> Metrics {
        if let Some(sink) = &sink {
            sink.opened();
        }
        Metrics {
            sink,
            reported: AtomicU64::new(0),
        }
    }

    pub(crate) fn bytes(&self, now: u64) {
        let old = self.reported.swap(now, Ordering::AcqRel);
        if old != now
            && let Some(sink) = &self.sink
        {
            sink.bytes_moved(old, now);
        }
    }

    pub(crate) fn dirty_call(&self) {
        if let Some(sink) = &self.sink {
            sink.dirty_call();
        }
    }
}

impl Drop for Metrics {
    fn drop(&mut self) {
        if let Some(sink) = &self.sink {
            sink.bytes_moved(*self.reported.get_mut(), 0);
            sink.closed();
        }
    }
}
