// SPDX-License-Identifier: AGPL-3.0-or-later

use std::time::{Duration, Instant};

pub const MIN_STALL_THRESHOLD: Duration = Duration::from_millis(250);
pub const MAX_STALL_THRESHOLD: Duration = Duration::from_secs(30);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StallSignal {
    Quiet,
    Stalled { frames_seen: u64, elapsed_ms: u64 },
    Resumed { stalled_for_ms: u64 },
}

pub struct NoFrameStallTracker {
    threshold: Duration,
    last_frame_at: Instant,
    frames_seen: u64,
    reported: bool,
}

impl NoFrameStallTracker {
    pub fn new(threshold: Duration, started_at: Instant) -> Self {
        assert!(
            threshold >= MIN_STALL_THRESHOLD,
            "stall threshold at least the minimum"
        );
        assert!(
            threshold <= MAX_STALL_THRESHOLD,
            "stall threshold at most the maximum"
        );
        Self {
            threshold,
            last_frame_at: started_at,
            frames_seen: 0,
            reported: false,
        }
    }

    pub fn observe(
        &mut self,
        now: Instant,
        produced_frame: bool,
        target_expects_frames: bool,
    ) -> StallSignal {
        let idle = now.saturating_duration_since(self.last_frame_at);
        if produced_frame {
            self.frames_seen = self.frames_seen.saturating_add(1);
            self.last_frame_at = now;
            if !self.reported {
                return StallSignal::Quiet;
            }
            self.reported = false;
            return StallSignal::Resumed {
                stalled_for_ms: duration_ms(idle),
            };
        }
        if !target_expects_frames {
            self.last_frame_at = now;
            return StallSignal::Quiet;
        }
        if self.reported || idle < self.threshold {
            return StallSignal::Quiet;
        }
        self.reported = true;
        StallSignal::Stalled {
            frames_seen: self.frames_seen,
            elapsed_ms: duration_ms(idle),
        }
    }
}

fn duration_ms(duration: Duration) -> u64 {
    u64::try_from(duration.as_millis()).unwrap_or(u64::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;

    const THRESHOLD: Duration = Duration::from_millis(3000);

    fn tracker(start: Instant) -> NoFrameStallTracker {
        NoFrameStallTracker::new(THRESHOLD, start)
    }

    #[test]
    fn stays_quiet_before_the_threshold_elapses() {
        let start = Instant::now();
        let mut stall = tracker(start);
        assert_eq!(
            stall.observe(start + Duration::from_millis(2999), false, true),
            StallSignal::Quiet
        );
    }

    #[test]
    fn reports_a_stall_once_when_no_frame_ever_arrives() {
        let start = Instant::now();
        let mut stall = tracker(start);
        assert_eq!(
            stall.observe(start + THRESHOLD, false, true),
            StallSignal::Stalled {
                frames_seen: 0,
                elapsed_ms: 3000,
            }
        );
        assert_eq!(
            stall.observe(start + Duration::from_millis(9000), false, true),
            StallSignal::Quiet,
            "a stall episode is reported at most once"
        );
    }

    #[test]
    fn counts_delivered_frames_before_a_later_stall() {
        let start = Instant::now();
        let mut stall = tracker(start);
        assert_eq!(
            stall.observe(start + Duration::from_millis(16), true, true),
            StallSignal::Quiet
        );
        assert_eq!(
            stall.observe(start + Duration::from_millis(32), true, true),
            StallSignal::Quiet
        );
        assert_eq!(
            stall.observe(start + Duration::from_millis(3032), false, true),
            StallSignal::Stalled {
                frames_seen: 2,
                elapsed_ms: 3000,
            }
        );
    }

    #[test]
    fn resuming_frames_clears_the_episode_and_arms_the_next_one() {
        let start = Instant::now();
        let mut stall = tracker(start);
        assert_eq!(
            stall.observe(start + THRESHOLD, false, true),
            StallSignal::Stalled {
                frames_seen: 0,
                elapsed_ms: 3000,
            }
        );
        assert_eq!(
            stall.observe(start + Duration::from_millis(4000), true, true),
            StallSignal::Resumed {
                stalled_for_ms: 4000,
            }
        );
        assert_eq!(
            stall.observe(start + Duration::from_millis(7000), false, true),
            StallSignal::Stalled {
                frames_seen: 1,
                elapsed_ms: 3000,
            }
        );
    }

    #[test]
    fn a_target_that_is_not_expected_to_produce_frames_never_stalls() {
        let start = Instant::now();
        let mut stall = tracker(start);
        assert_eq!(
            stall.observe(start + Duration::from_millis(60_000), false, false),
            StallSignal::Quiet
        );
        assert_eq!(
            stall.observe(start + Duration::from_millis(62_000), false, true),
            StallSignal::Quiet,
            "the idle window restarts once the target can produce frames again"
        );
        assert_eq!(
            stall.observe(start + Duration::from_millis(63_000), false, true),
            StallSignal::Stalled {
                frames_seen: 0,
                elapsed_ms: 3000,
            }
        );
    }
}
