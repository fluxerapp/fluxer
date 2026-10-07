// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::Deserialize;
use thiserror::Error;

pub const SUBJECT_MESSAGE: &str = "push.job.message";
pub const SUBJECT_CLEAR: &str = "push.job.clear";
pub const SUBJECT_RING: &str = "push.job.ring";
pub const QUEUE_GROUP: &str = "fluxer-push";

const SUPPORTED_VERSION: u8 = 1;
const DIRECT_MESSAGE_GUILD_ID: &str = "0";
const THREAD_KIND: &str = "thread";
const FORUM_THREAD_CREATED_KIND: &str = "forum_thread_created";

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct MessageJob {
    pub v: u8,
    pub guild_id: String,
    pub channel_id: String,
    pub message_id: String,
    pub notification: NotificationFields,
    pub user_ids: Vec<String>,
    #[serde(default)]
    pub kind: Option<String>,
    #[serde(default)]
    pub parent_id: Option<String>,
    #[serde(default)]
    pub parent_name: Option<String>,
    #[serde(default)]
    pub channel_name: Option<String>,
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
    pub user_id: String,
    pub channel_id: String,
    pub message_id: String,
    #[serde(default)]
    pub after_message_id: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct RingJob {
    pub v: u8,
    pub user_id: String,
    pub channel_id: String,
    pub message_id: String,
    pub started_at_ms: i64,
    pub expires_at_ms: i64,
    #[serde(default)]
    pub caller_id: Option<String>,
    #[serde(default)]
    pub caller_name: Option<String>,
    #[serde(default)]
    pub caller_avatar_url: Option<String>,
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

    pub fn is_thread(&self) -> bool {
        matches!(
            self.kind.as_deref(),
            Some(THREAD_KIND | FORUM_THREAD_CREATED_KIND)
        )
    }

    pub fn is_forum_thread_created(&self) -> bool {
        self.kind.as_deref() == Some(FORUM_THREAD_CREATED_KIND)
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{Value, json};

    fn clear_job(config_version: Option<u64>) -> Vec<u8> {
        let mut job = json!({
            "v": 1,
            "user_id": "1",
            "channel_id": "2",
            "message_id": "3",
        });
        if let Some(version) = config_version {
            job["config_version"] = Value::from(version);
        }
        job.to_string().into_bytes()
    }

    #[test]
    fn a_job_from_a_gateway_that_sends_a_config_version_decodes() {
        assert_eq!(
            decode_clear(&clear_job(Some(7))).expect("decodes").user_id,
            "1"
        );
    }

    #[test]
    fn a_job_without_a_config_version_decodes() {
        assert_eq!(
            decode_clear(&clear_job(None)).expect("decodes").user_id,
            "1"
        );
    }

    const BASE: &str = r#"{"v":1,"config_version":3,"guild_id":"5","channel_id":"6","message_id":"7","notification":{"title":"t","body":"b","icon":"i","badge":"g","tag":"x","notification_tag":"y","url":"/u"},"user_ids":["8"]"#;

    #[test]
    fn a_job_without_thread_fields_decodes_as_before() {
        let job = decode_message(format!("{BASE}}}").as_bytes()).expect("the job decodes");
        assert!(!job.is_thread());
        assert_eq!(job.parent_id, None);
        assert_eq!(job.parent_name, None);
        assert_eq!(job.channel_name, None);
    }

    #[test]
    fn a_thread_job_decodes_its_optional_fields() {
        let job = decode_message(
            format!(r#"{BASE},"kind":"thread","parent_id":"9","parent_name":"general","channel_name":"plans"}}"#)
                .as_bytes(),
        )
        .expect("the job decodes");
        assert!(job.is_thread());
        assert_eq!(job.parent_id.as_deref(), Some("9"));
        assert_eq!(job.parent_name.as_deref(), Some("general"));
        assert_eq!(job.channel_name.as_deref(), Some("plans"));
    }

    #[test]
    fn a_gateway_thread_job_decodes_as_a_thread() {
        let job = decode_message(
            br#"{"v":1,"config_version":3,"guild_id":"5","channel_id":"6","message_id":"7","notification":{"title":"ada (#ideas, #general, Guild)","body":"hi","icon":"icon","badge":"badge","tag":"channel:6:7","notification_tag":"channel:6","url":"/channels/5/6/7","image_url":null},"user_ids":["8"],"kind":"thread","parent_id":"200","parent_name":"general","channel_name":"ideas"}"#,
        )
        .expect("the job decodes");
        assert!(job.is_thread());
        assert_eq!(job.parent_id.as_deref(), Some("200"));
        assert_eq!(job.parent_name.as_deref(), Some("general"));
        assert_eq!(job.channel_name.as_deref(), Some("ideas"));
    }

    #[test]
    fn a_forum_thread_created_job_reaches_thread_capable_devices_only() {
        let job = decode_message(
            format!(
                r#"{BASE},"kind":"forum_thread_created","parent_id":"9","channel_name":"post"}}"#
            )
            .as_bytes(),
        )
        .expect("the job decodes");
        assert!(job.is_thread());
        assert!(job.is_forum_thread_created());
        let thread = decode_message(format!(r#"{BASE},"kind":"thread"}}"#).as_bytes())
            .expect("the job decodes");
        assert!(!thread.is_forum_thread_created());
    }

    #[test]
    fn thread_fields_nested_in_the_notification_do_not_mark_a_thread() {
        let job = decode_message(
            br#"{"v":1,"config_version":3,"guild_id":"5","channel_id":"6","message_id":"7","notification":{"title":"t","body":"b","icon":"i","badge":"g","tag":"x","notification_tag":"y","url":"/u","kind":"thread","parent_id":"200"},"user_ids":["8"]}"#,
        )
        .expect("the job decodes");
        assert!(!job.is_thread());
        assert_eq!(job.parent_id, None);
    }
}
