// SPDX-License-Identifier: AGPL-3.0-or-later

pub const PROBE_GRANTED: &str = "granted";
pub const PROBE_DENIED: &str = "denied";
pub const PROBE_TIMEOUT: &str = "timeout";
pub const PROBE_UNSUPPORTED: &str = "unsupported";

pub const PROBE_TIMEOUT_NS: u64 = 3 * 1_000_000_000;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ShareableContentOutcome {
    Delivered,
    Failed,
    TimedOut,
}

pub const fn screen_recording_probe_result(
    sck_available: bool,
    outcome: ShareableContentOutcome,
) -> &'static str {
    if !sck_available {
        return PROBE_UNSUPPORTED;
    }
    match outcome {
        ShareableContentOutcome::Delivered => PROBE_GRANTED,
        ShareableContentOutcome::Failed => PROBE_DENIED,
        ShareableContentOutcome::TimedOut => PROBE_TIMEOUT,
    }
}
