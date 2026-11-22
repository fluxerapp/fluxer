// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Debug, Default)]
pub struct StorageMetrics {
    pub storage_hits: Counter,
    pub storage_misses: Counter,
    pub storage_errors: Counter,
    pub fallback_hits: Counter,
    pub fallback_double_misses: Counter,
    pub fallback_errors: Counter,
}

impl StorageMetrics {}

use super::Counter;
