// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ListJobsResponse {
    pub jobs: Vec<serde_json::Value>,
    pub next_cursor: Option<serde_json::Value>,
    #[serde(default)]
    pub cursor: Option<serde_json::Value>,
    pub has_more: Option<bool>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GetJobResponse {
    pub job: serde_json::Value,
    pub ledger: Option<Vec<serde_json::Value>>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CancelJobResponse {
    pub cancelled: bool,
}
