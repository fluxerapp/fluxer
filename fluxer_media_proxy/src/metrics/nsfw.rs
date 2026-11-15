// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Debug, Default)]
pub struct NSFWMetrics {
    pub nsfw_calls_ok: Counter,
    pub nsfw_calls_failed: Counter,
    pub nsfw_calls_disabled: Counter,
}

impl NSFWMetrics {}

use super::Counter;
