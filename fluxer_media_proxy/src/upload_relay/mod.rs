// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod target;
#[cfg(test)]
mod tests;
pub mod token;

use std::sync::atomic::{AtomicU64, Ordering};
use thiserror::Error;

pub const RELAY_PATH_PREFIX: &str = "/v1/relay/";

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum RelayError {
    #[error("missing relay token")]
    MissingToken,
    #[error("invalid relay token")]
    InvalidToken,
    #[error("relay token expired")]
    RelayTokenExpired,
    #[error("wrong bucket")]
    WrongBucket,
    #[error("key mismatch")]
    KeyMismatch,
    #[error("method mismatch")]
    MethodMismatch,
    #[error("part number mismatch")]
    PartNumberMismatch,
    #[error("upload id mismatch")]
    UploadIdMismatch,
    #[error("payload too large")]
    PayloadTooLarge,
    #[error("bad query")]
    BadQuery,
    #[error("client upload failed")]
    ClientUploadFailed,
    #[error("upstream S3 error")]
    UpstreamS3Error,
    #[error("upstream retryable error")]
    UpstreamRetryable,
    #[error("internal relay error")]
    InternalError,
}

static BUFFERED_RETRY_IN_FLIGHT: AtomicU64 = AtomicU64::new(0);
static SPOOL_IN_FLIGHT_BYTES: AtomicU64 = AtomicU64::new(0);

pub fn try_reserve_buffer_budget(needed: u64, ceiling: u64) -> bool {
    try_reserve(&BUFFERED_RETRY_IN_FLIGHT, needed, ceiling)
}

pub fn release_buffer_budget(amount: u64) {
    BUFFERED_RETRY_IN_FLIGHT.fetch_sub(amount, Ordering::AcqRel);
}

pub fn try_reserve_spool_budget(needed: u64, ceiling: u64) -> bool {
    try_reserve(&SPOOL_IN_FLIGHT_BYTES, needed, ceiling)
}

pub fn release_spool_budget(amount: u64) {
    SPOOL_IN_FLIGHT_BYTES.fetch_sub(amount, Ordering::AcqRel);
}

pub fn spool_in_flight_bytes() -> u64 {
    SPOOL_IN_FLIGHT_BYTES.load(Ordering::Relaxed)
}

fn try_reserve(counter: &AtomicU64, needed: u64, ceiling: u64) -> bool {
    if ceiling == 0 || needed > ceiling {
        return false;
    }
    let mut current = counter.load(Ordering::Acquire);
    loop {
        if current.saturating_add(needed) > ceiling {
            return false;
        }
        match counter.compare_exchange_weak(
            current,
            current + needed,
            Ordering::AcqRel,
            Ordering::Acquire,
        ) {
            Ok(_) => return true,
            Err(next) => current = next,
        }
    }
}

pub fn is_relay_path(path: &str) -> bool {
    path.starts_with(RELAY_PATH_PREFIX)
}
