// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::ids::{Millis, WallMillis};

pub trait Clock: Send + Sync {
    fn now(&self) -> Millis;

    fn wall_now(&self) -> WallMillis;

    fn elapsed_since(&self, earlier: Millis) -> u64 {
        self.now().saturating_since(earlier)
    }
}

pub type SharedClock = Arc<dyn Clock>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct WallAnchor {
    monotonic: Millis,
    wall: WallMillis,
}

impl WallAnchor {
    pub const fn new(monotonic: Millis, wall: WallMillis) -> Self {
        Self { monotonic, wall }
    }

    pub const fn projected(&self, monotonic: Millis) -> WallMillis {
        self.wall
            .saturating_add_millis(monotonic.saturating_since(self.monotonic))
    }

    pub const fn guard(&self, monotonic: Millis, observed: WallMillis) -> WallMillis {
        let projected = self.projected(monotonic);
        if observed.get() < projected.get() {
            observed
        } else {
            projected
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct SystemClock;

impl SystemClock {
    pub const fn new() -> Self {
        Self
    }

    pub fn shared() -> SharedClock {
        Arc::new(Self)
    }
}

impl Clock for SystemClock {
    fn now(&self) -> Millis {
        Millis::new(u64::try_from(fluxer_svc::metrics::now_ms()).unwrap_or(0))
    }

    fn wall_now(&self) -> WallMillis {
        let since_epoch = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default();
        WallMillis::new(u64::try_from(since_epoch.as_millis()).unwrap_or(u64::MAX))
    }
}

#[derive(Debug, Default)]
struct TestClockState {
    monotonic: AtomicU64,
    wall: AtomicU64,
}

#[derive(Clone, Debug, Default)]
pub struct TestClock {
    state: Arc<TestClockState>,
}

impl TestClock {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn starting_at(monotonic: Millis, wall: WallMillis) -> Self {
        Self {
            state: Arc::new(TestClockState {
                monotonic: AtomicU64::new(monotonic.get()),
                wall: AtomicU64::new(wall.get()),
            }),
        }
    }

    pub fn shared(&self) -> SharedClock {
        Arc::new(self.clone())
    }

    pub fn advance(&self, delta_ms: u64) -> Millis {
        let previous = self.state.monotonic.fetch_add(delta_ms, Ordering::SeqCst);
        Millis::new(previous.saturating_add(delta_ms))
    }

    pub fn advance_wall(&self, delta_ms: u64) -> WallMillis {
        let previous = self.state.wall.fetch_add(delta_ms, Ordering::SeqCst);
        WallMillis::new(previous.saturating_add(delta_ms))
    }

    pub fn set_wall(&self, wall: WallMillis) {
        self.state.wall.store(wall.get(), Ordering::SeqCst);
    }
}

impl Clock for TestClock {
    fn now(&self) -> Millis {
        Millis::new(self.state.monotonic.load(Ordering::SeqCst))
    }

    fn wall_now(&self) -> WallMillis {
        WallMillis::new(self.state.wall.load(Ordering::SeqCst))
    }
}
