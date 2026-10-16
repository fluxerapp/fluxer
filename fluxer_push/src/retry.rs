// SPDX-License-Identifier: AGPL-3.0-or-later

use rand::RngExt as _;
use std::time::{Duration, Instant};

pub const RETRY_DEADLINE: Duration = Duration::from_secs(60);
const BASE_DELAY_MS: u64 = 500;
const MAX_DELAY_MS: u64 = 10_000;

pub fn next_attempt(attempt: u32, now: Instant, deadline: Instant) -> Option<Instant> {
    let at = now + backoff(attempt);
    (at <= deadline).then_some(at)
}

fn backoff(attempt: u32) -> Duration {
    let ceiling = MAX_DELAY_MS.min(BASE_DELAY_MS << attempt.min(8));
    Duration::from_millis(rand::rng().random_range(ceiling / 2..=ceiling))
}
