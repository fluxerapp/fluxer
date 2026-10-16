// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_common::attachment_url_signature::Verdict;
use std::sync::atomic::{AtomicU64, Ordering};

pub(super) const VERDICTS: [Verdict; 5] = [
    Verdict::Valid,
    Verdict::Missing,
    Verdict::Malformed,
    Verdict::Mismatch,
    Verdict::Expired,
];

pub(super) const VERDICT_COUNT: usize = VERDICTS.len();

pub struct AttachmentSignatureMetrics {
    pub(super) verdicts: [AtomicU64; VERDICT_COUNT],
}

impl AttachmentSignatureMetrics {
    pub(crate) fn new() -> Self {
        Self {
            verdicts: [const { AtomicU64::new(0) }; VERDICT_COUNT],
        }
    }

    pub fn record(&self, verdict: Verdict) {
        self.verdicts[verdict_index(verdict)].fetch_add(1, Ordering::Relaxed);
    }
}

pub(super) fn verdict_index(verdict: Verdict) -> usize {
    match verdict {
        Verdict::Valid => 0,
        Verdict::Missing => 1,
        Verdict::Malformed => 2,
        Verdict::Mismatch => 3,
        Verdict::Expired => 4,
    }
}
