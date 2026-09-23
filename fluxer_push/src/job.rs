// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::Deserialize;
use thiserror::Error;

pub const SUBJECT_MESSAGE: &str = "push.job.message";
pub const SUBJECT_CLEAR: &str = "push.job.clear";
pub const SUBJECT_RING: &str = "push.job.ring";
pub const QUEUE_GROUP: &str = "fluxer-push";

const SUPPORTED_VERSION: u8 = 1;
const DIRECT_MESSAGE_GUILD_ID: &str = "0";

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct MessageJob {
    pub v: u8,
    pub config_version: u64,
    pub guild_id: String,
    pub channel_id: String,
    pub message_id: String,
    pub notification: NotificationFields,
    pub user_ids: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct NotificationFields {
    pub title: String,
    pub body: String,
    pub icon: String,
    pub badge: String,
    pub tag: String,
    pub notification_tag: String,
    pub url: String,
    #[serde(default)]
    pub image_url: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct ClearJob {
    pub v: u8,
    pub config_version: u64,
    pub user_id: String,
    pub channel_id: String,
    pub message_id: String,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct RingJob {
    pub v: u8,
    pub config_version: u64,
    pub user_id: String,
    pub channel_id: String,
    pub message_id: String,
    pub started_at_ms: i64,
    pub expires_at_ms: i64,
}

#[derive(Debug, Error)]
pub enum JobError {
    #[error("push job version {0} is not supported")]
    UnsupportedVersion(u8),
    #[error("push job is not a valid job document: {0}")]
    Decode(#[from] serde_json::Error),
}

impl MessageJob {
    pub fn is_direct_message(&self) -> bool {
        self.guild_id == DIRECT_MESSAGE_GUILD_ID
    }
}

pub fn decode_message(bytes: &[u8]) -> Result<MessageJob, JobError> {
    let job: MessageJob = serde_json::from_slice(bytes)?;
    supported(job.v)?;
    Ok(job)
}

pub fn decode_clear(bytes: &[u8]) -> Result<ClearJob, JobError> {
    let job: ClearJob = serde_json::from_slice(bytes)?;
    supported(job.v)?;
    Ok(job)
}

pub fn decode_ring(bytes: &[u8]) -> Result<RingJob, JobError> {
    let job: RingJob = serde_json::from_slice(bytes)?;
    supported(job.v)?;
    Ok(job)
}

fn supported(version: u8) -> Result<(), JobError> {
    if version == SUPPORTED_VERSION {
        return Ok(());
    }
    Err(JobError::UnsupportedVersion(version))
}
