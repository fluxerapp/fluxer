// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct LookupMessageResponse {
    #[serde(flatten)]
    pub data: serde_json::Value,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct MessageShredResponse {
    pub success: Option<bool>,
    pub job_id: Option<String>,
    pub requested: Option<u64>,
    #[serde(flatten)]
    pub extra: serde_json::Value,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct MessageShredStatusResponse {
    #[serde(flatten)]
    pub data: serde_json::Value,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DeleteAllUserMessagesResponse {
    pub success: Option<bool>,
    pub dry_run: bool,
    pub channel_count: u64,
    pub message_count: u64,
    pub job_id: Option<String>,
    #[serde(flatten)]
    pub extra: serde_json::Value,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BrowseChannelResponse {
    #[serde(flatten)]
    pub data: serde_json::Value,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SearchChannelMessagesResponse {
    #[serde(flatten)]
    pub data: serde_json::Value,
}
