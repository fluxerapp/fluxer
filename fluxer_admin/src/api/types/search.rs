// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RefreshSearchIndexResponse {
    pub success: bool,
    pub job_id: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum IndexRefreshStatusResponse {
    NotFound {
        status: String,
    },
    Progress {
        status: String,
        index_type: Option<String>,
        phase: Option<String>,
        total: Option<u64>,
        indexed: Option<u64>,
        started_at: Option<String>,
        completed_at: Option<String>,
        failed_at: Option<String>,
        error: Option<String>,
    },
}
