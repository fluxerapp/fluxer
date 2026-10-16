// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::Deserialize;

#[derive(Debug, Clone)]
pub struct ActivityPubContext {
    pub server_domain: String,
    pub server_title: String,
    pub server_icon: Option<String>,
    pub in_reply_to: Option<ActivityPubReplyContext>,
}

#[derive(Debug, Clone)]
pub struct ActivityPubReplyContext {
    pub author: String,
    pub url: String,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ActivityPubPost {
    pub url: Option<String>,
    pub published: Option<String>,
    pub attributed_to: Option<serde_json::Value>,
    pub content: Option<String>,
    pub summary: Option<String>,
    pub attachment: Option<Vec<ActivityPubAttachment>>,
    pub in_reply_to: Option<String>,
    pub likes: Option<ActivityPubCollectionCount>,
    pub shares: Option<ActivityPubCollectionCount>,
    pub replies: Option<ActivityPubCollection>,
    pub quote: Option<String>,
    pub quote_uri: Option<String>,
    #[serde(rename = "_misskey_quote")]
    pub misskey_quote: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ActivityPubAttachment {
    pub media_type: Option<String>,
    pub url: Option<String>,
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum ActivityPubCollectionCount {
    Count(u64),
    Collection(ActivityPubCollection),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ActivityPubCollection {
    pub total_items: Option<u64>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ActivityPubActor {
    pub id: Option<String>,
    pub name: Option<String>,
    pub preferred_username: Option<String>,
    pub url: Option<String>,
    pub icon: Option<ActivityPubIcon>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ActivityPubIcon {
    pub url: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MastodonPost {
    pub created_at: Option<String>,
    pub in_reply_to_id: Option<String>,
    pub in_reply_to_account_id: Option<String>,
    pub content: Option<String>,
    pub spoiler_text: Option<String>,
    pub url: Option<String>,
    pub account: Option<MastodonAccount>,
    pub media_attachments: Option<Vec<MastodonMediaAttachment>>,
    pub favourites_count: Option<u64>,
    pub reblogs_count: Option<u64>,
    pub replies_count: Option<u64>,
    pub reblog: Option<Box<MastodonPost>>,
    pub poll: Option<MastodonPoll>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MastodonAccount {
    pub username: Option<String>,
    pub display_name: Option<String>,
    pub url: Option<String>,
    pub avatar: Option<String>,
    pub acct: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MastodonMediaAttachment {
    #[serde(rename = "type")]
    pub attachment_type: Option<String>,
    pub url: Option<String>,
    pub preview_url: Option<String>,
    pub description: Option<String>,
    pub meta: Option<MastodonMediaMeta>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MastodonMediaMeta {
    pub original: Option<MastodonMediaMetaSize>,
    pub small: Option<MastodonMediaMetaSize>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MastodonMediaMetaSize {
    pub width: Option<u32>,
    pub height: Option<u32>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MastodonPoll {
    pub votes_count: Option<u64>,
    pub options: Vec<MastodonPollOption>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MastodonPollOption {
    pub title: String,
    pub votes_count: Option<u64>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MastodonInstance {
    pub domain: Option<String>,
    pub title: Option<String>,
    pub thumbnail: Option<MastodonThumbnail>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MastodonThumbnail {
    pub url: Option<String>,
}

impl MastodonInstance {
    pub fn thumbnail_url(&self) -> Option<String> {
        self.thumbnail.as_ref()?.url.clone()
    }
}

pub struct InstanceInfo {
    pub domain: Option<String>,
    pub title: Option<String>,
    pub thumbnail_url: Option<String>,
}

impl From<MastodonInstance> for InstanceInfo {
    fn from(inst: MastodonInstance) -> Self {
        let thumbnail_url = inst.thumbnail_url();
        Self {
            domain: inst.domain,
            title: inst.title,
            thumbnail_url,
        }
    }
}
