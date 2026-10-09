// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::mention_extractor::{
    MessageMentions, extend_mentions_from_markdown, extract_mentions_from_markdown,
};
use crate::types::{
    ApiChannelMentionResponse, ApiEmbedAuthorResponse, ApiEmbedFieldResponse,
    ApiEmbedFooterResponse, ApiEmbedMediaResponse, ApiEmbedProviderResponse,
    ApiMessageAttachmentResponse, ApiMessageCallResponse, ApiMessageEmbedChildResponse,
    ApiMessageEmbedResponse, ApiMessageReactionResponse, ApiMessageReferenceResponse,
    ApiMessageResponse, ApiMessageSnapshotResponse, ApiMessageStickerResponse,
    ApiReactionEmojiResponse, ApiUserPartialResponse, Message, MessageAttachment, MessageCall,
    MessageEmbed, MessageEmbedAuthor, MessageEmbedChild, MessageEmbedField, MessageEmbedFooter,
    MessageEmbedMedia, MessageEmbedProvider, MessagePoll, MessagePollAnswer,
    MessagePollAnswerCount, MessagePollEmoji, MessagePollMedia, MessagePollResults,
    MessageReference, MessageRequest, MessageResponse, MessageSnapshot, MessageStickerItem,
};
use crate::udt;
use base64::prelude::{BASE64_STANDARD, Engine};
use chrono::{DateTime, Utc};
use fluxer_common::user_flags::{USER_FLAG_STAFF, visible_user_flags};
use fluxer_svc::shard::ShardService;
use fluxer_svc::transport::Transport;
use fluxer_svc::{postgres, postgres::BigIntBound, postgres::KeyPart};
use futures::stream::{self, StreamExt};
#[cfg(feature = "scylla")]
use scylla::DeserializeRow;
#[cfg(feature = "scylla")]
use scylla::client::session::Session;
#[cfg(feature = "scylla")]
use scylla::response::query_result::QueryRowsResult;
#[cfg(feature = "scylla")]
use scylla::statement::prepared::PreparedStatement;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
#[cfg(feature = "scylla")]
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};
use url::Url;

const BUCKET_DURATION_MS: i64 = 864_000_000;
const FLUXER_EPOCH_MS: i64 = 1_420_070_400_000;
const SERVICE_REQUEST_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(5);
const MESSAGE_REFERENCE_TYPE_DEFAULT: i32 = 0;
const MESSAGE_REFERENCE_TYPE_FORWARD: i32 = 1;
const HIDDEN_REFILL_ROUNDS: usize = 5;
const HIDDEN_REFILL_MAX_PAGE: u32 = 800;
const MESSAGE_FLAG_IS_CROSSPOST: i64 = 1 << 1;

fn effective_reference_type(reference: &MessageReference) -> i32 {
    reference
        .reference_type
        .unwrap_or(MESSAGE_REFERENCE_TYPE_DEFAULT)
}

fn is_crosspost_copy(message: &Message) -> bool {
    (message.flags.unwrap_or_default() & MESSAGE_FLAG_IS_CROSSPOST) != 0
}

fn attachment_storage_channel_id(message: &Message) -> i64 {
    if !is_crosspost_copy(message) {
        return message.channel_id;
    }
    message
        .message_reference
        .as_ref()
        .and_then(|reference| reference.channel_id)
        .unwrap_or(message.channel_id)
}

fn reply_target(message: &Message) -> Option<(i64, i64)> {
    if is_crosspost_copy(message) {
        return None;
    }
    let reference = message.message_reference.as_ref()?;
    if effective_reference_type(reference) != MESSAGE_REFERENCE_TYPE_DEFAULT {
        return None;
    }
    Some((reference.channel_id?, reference.message_id?))
}

fn copy_source(message: &Message) -> Option<(i64, i64)> {
    let reference = message.message_reference.as_ref()?;
    if !is_crosspost_copy(message)
        && effective_reference_type(reference) != MESSAGE_REFERENCE_TYPE_FORWARD
    {
        return None;
    }
    Some((reference.channel_id?, reference.message_id?))
}

const MESSAGE_FLAG_SUPPRESS_EMBEDS: i64 = 1 << 2;
const MESSAGE_TYPE_THREAD_CREATED: i32 = 18;
const THREAD_MESSAGE_FLAG_MASK: i64 = 0x120;
const THREAD_FEATURE_CHANNEL_TYPES: [i32; 5] = [10, 11, 12, 15, 16];
const EMBED_MEDIA_OWNED_ATTACHMENT_FLAG: i32 = 1 << 30;
#[cfg(test)]
const USER_FLAG_DELETED: i64 = 1_i64 << 34;
const FLUXER_SYSTEM_USER_ID: i64 = 0;
const FLUXER_SYSTEM_USERNAME: &str = "Fluxer";
const FLUXER_SYSTEM_DISCRIMINATOR: &str = "0000";
const DELETED_USER_USERNAME: &str = "DeletedUser";
const DELETED_USER_GLOBAL_NAME: &str = "Deleted User";
const BUCKET_SCAN_CONCURRENCY: usize = 16;
const BUCKET_SCAN_WAVE: usize = 4;
const ENRICHMENT_QUERY_CONCURRENCY: usize = 16;
const REACTION_MESSAGE_BATCH_SIZE: usize = 64;
const ATTACHMENT_DECAY_BATCH_SIZE: usize = 128;
const BUCKET_INDEX_PAGE_SIZE: u32 = 200;
const EXCLUDED_TYPE_REFILL_PAGE_SIZE: u32 = 100;
const EXCLUDED_TYPE_SCAN_ROW_CAP: usize = 5_000;
const JS_MAX_SAFE_INTEGER: i64 = 9_007_199_254_740_991;

fn assert_safe_byte_size(value: i64) -> i64 {
    assert!(
        (0..=JS_MAX_SAFE_INTEGER).contains(&value),
        "attachment size must fit a non-negative JavaScript safe integer"
    );
    value
}

#[cfg(feature = "scylla")]
const MESSAGE_COLUMNS: &str = "\
    message_id, channel_id, bucket, author_id, type, \
    webhook_id, webhook_name, webhook_avatar_hash, \
    content, edited_timestamp, pinned_timestamp, flags, mention_everyone, \
    mention_users, mention_roles, mention_channels, \
    has_reaction, version, \
    attachments, embeds, sticker_items, message_reference, call, message_snapshots";

pub struct MessagesShard<T> {
    storage: MessagesStorage,
    transport: T,
}

#[cfg(test)]
type DeletedMessageKeys = std::sync::Arc<std::sync::Mutex<Vec<(i64, i32, i64)>>>;

#[derive(Clone)]
enum MessagesStorage {
    Postgres(PostgresMessagesStorage),
    #[cfg(feature = "scylla")]
    Scylla(Box<ScyllaMessagesStorage>),
    #[cfg(test)]
    Deletions(DeletedMessageKeys),
    #[cfg(test)]
    Memory(std::sync::Arc<Vec<Message>>),
}

#[cfg(test)]
fn memory_buckets(
    messages: &[Message],
    channel_id: i64,
    min_bucket: i32,
    max_bucket: i32,
    limit: u32,
    descending: bool,
) -> Vec<i32> {
    let mut buckets = messages
        .iter()
        .filter(|message| message.channel_id == channel_id)
        .map(|message| snowflake_to_bucket(message.message_id))
        .filter(|bucket| (min_bucket..=max_bucket).contains(bucket))
        .collect::<Vec<_>>();
    buckets.sort_unstable();
    buckets.dedup();
    if descending {
        buckets.reverse();
    }
    buckets.truncate(limit as usize);
    buckets
}

#[cfg(test)]
fn memory_bucket(
    messages: &[Message],
    channel_id: i64,
    bucket: i32,
    bound: Option<BucketBound>,
    limit: i32,
) -> Vec<Message> {
    let mut rows = messages
        .iter()
        .filter(|message| {
            message.channel_id == channel_id
                && snowflake_to_bucket(message.message_id) == bucket
                && match bound {
                    Some(BucketBound::Before(id)) => message.message_id < id,
                    Some(BucketBound::After(id)) => message.message_id > id,
                    None => true,
                }
        })
        .cloned()
        .collect::<Vec<_>>();
    if matches!(bound, Some(BucketBound::After(_))) {
        rows.sort_unstable_by_key(|message| message.message_id);
    } else {
        rows.sort_unstable_by_key(|message| std::cmp::Reverse(message.message_id));
    }
    rows.truncate(limit.max(0) as usize);
    rows
}

#[derive(Clone)]
struct PostgresMessagesStorage {
    kv: postgres::KvClient,
}

#[cfg(feature = "scylla")]
#[derive(Clone)]
struct ScyllaMessagesStorage {
    db: Arc<Session>,
    stmt_get_by_id: PreparedStatement,
    stmt_get_latest: PreparedStatement,
    stmt_get_before: PreparedStatement,
    stmt_get_after: PreparedStatement,
    stmt_list_buckets_desc: PreparedStatement,
    stmt_list_buckets_asc: PreparedStatement,
    stmt_get_reactions: PreparedStatement,
    stmt_get_reactions_for_messages: PreparedStatement,
    stmt_get_attachment_decay: PreparedStatement,
    stmt_get_attachment_decay_many: PreparedStatement,
    stmt_delete_message: PreparedStatement,
}

#[cfg_attr(feature = "scylla", derive(DeserializeRow))]
#[derive(Debug)]
struct MessageDbRow {
    message_id: i64,
    channel_id: i64,
    bucket: i32,
    author_id: Option<i64>,
    r#type: Option<i32>,
    webhook_id: Option<i64>,
    webhook_name: Option<String>,
    webhook_avatar_hash: Option<String>,
    content: Option<String>,
    edited_timestamp: Option<DateTime<Utc>>,
    pinned_timestamp: Option<DateTime<Utc>>,
    flags: Option<i32>,
    mention_everyone: Option<bool>,
    mention_users: Option<std::collections::HashSet<i64>>,
    mention_roles: Option<std::collections::HashSet<i64>>,
    mention_channels: Option<std::collections::HashSet<i64>>,
    has_reaction: Option<bool>,
    version: Option<i32>,
    attachments: Option<Vec<udt::AttachmentUdt>>,
    embeds: Option<Vec<udt::EmbedUdt>>,
    poll: Option<udt::PollUdt>,
    sticker_items: Option<Vec<udt::StickerItemUdt>>,
    message_reference: Option<udt::MessageReferenceUdt>,
    call: Option<udt::MessageCallUdt>,
    message_snapshots: Option<Vec<udt::MessageSnapshotUdt>>,
}

#[cfg_attr(feature = "scylla", derive(DeserializeRow))]
#[derive(Debug)]
struct MessageReactionDbRow {
    user_id: i64,
    emoji_id: i64,
    emoji_name: String,
    emoji_animated: Option<bool>,
    created_at: Option<DateTime<Utc>>,
}

#[cfg(feature = "scylla")]
#[derive(Debug, DeserializeRow)]
struct ChannelMessageBucketDbRow {
    bucket: i32,
}

#[cfg(feature = "scylla")]
#[derive(Debug, DeserializeRow)]
struct BatchedMessageReactionDbRow {
    message_id: i64,
    user_id: i64,
    emoji_id: i64,
    emoji_name: String,
    emoji_animated: Option<bool>,
    created_at: Option<DateTime<Utc>>,
}

#[cfg(feature = "scylla")]
#[derive(Debug, DeserializeRow)]
struct AttachmentDecayDbRow {
    attachment_id: i64,
    expires_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct ChannelMessageBucketKvRow {
    bucket: i32,
}

#[derive(Debug, Deserialize)]
struct MessageReactionKvRow {
    message_id: i64,
    user_id: i64,
    emoji_id: i64,
    emoji_name: String,
    emoji_animated: Option<bool>,
    created_at: Option<i64>,
}

#[derive(Debug, Deserialize)]
struct AttachmentDecayKvRow {
    attachment_id: i64,
    expires_at: i64,
}

#[derive(Debug, Clone, Deserialize)]
struct UserPartialServiceResponse {
    user_id: i64,
    username: String,
    discriminator: i32,
    global_name: Option<String>,
    avatar_hash: Option<String>,
    bot: Option<bool>,
    system: Option<bool>,
    flags: Option<i64>,
    avatar_color: Option<i32>,
    mention_flags: Option<i32>,
    #[serde(default)]
    content_hidden_since: Option<i64>,
}

#[derive(Debug, Default)]
struct UserLookup {
    partials: HashMap<i64, UserPartialServiceResponse>,
    requested: HashSet<i64>,
}

impl UserLookup {
    fn hides(&self, author_id: Option<i64>, message_id: i64) -> bool {
        author_id
            .and_then(|author_id| self.partials.get(&author_id))
            .and_then(|partial| partial.content_hidden_since)
            .is_some_and(|since| snowflake_to_epoch_millis(message_id) >= since)
    }

    fn merge(&mut self, other: UserLookup) {
        self.requested.extend(other.requested);
        self.partials.extend(other.partials);
    }
}

#[derive(Clone, Copy)]
enum PageCursor {
    Latest,
    Before(i64),
    After(i64),
}

impl PageCursor {
    fn next(self, page: &[Message]) -> Option<Self> {
        let ids = page.iter().map(|message| message.message_id);
        match self {
            PageCursor::Latest | PageCursor::Before(_) => ids.min().map(PageCursor::Before),
            PageCursor::After(_) => ids.max().map(PageCursor::After),
        }
    }
}

#[derive(Debug, Deserialize)]
enum UserServiceResponse {
    FoundPartials(Vec<UserPartialServiceResponse>),
    FoundPartial(UserPartialServiceResponse),
}

#[derive(Debug, Deserialize)]
struct GatewayRpcResponse<T> {
    ok: bool,
    result: Option<T>,
}

#[derive(Debug, Deserialize)]
struct GatewayChannelMentionsResult {
    channels: Option<Vec<GatewayChannelMention>>,
}

#[derive(Debug, Deserialize)]
struct GatewayChannelMention {
    id: String,
    name: String,
    #[serde(rename = "type")]
    channel_type: i32,
}

#[derive(Debug, Clone)]
struct ResponseBuildOptions {
    viewer_user_id: i64,
    source_guild_id: Option<i64>,
    message_history_cutoff_ms: Option<i64>,
    can_read_message_history: bool,
    media_endpoint: String,
    media_proxy_secret_key: String,
    attachment_url_secret: Vec<u8>,
    include_reactions: bool,
    nonce: Option<String>,
    tts: bool,
    include_hidden: bool,
    threads_mask: bool,
    excluded_types: Vec<i32>,
}

impl ResponseBuildOptions {
    fn hides(&self, message: &Message) -> bool {
        self.threads_mask && message.message_type == MESSAGE_TYPE_THREAD_CREATED
    }

    fn mask_flags(&self, flags: i64) -> i64 {
        if self.threads_mask {
            flags & !THREAD_MESSAGE_FLAG_MASK
        } else {
            flags
        }
    }
}

#[derive(Debug, Default)]
struct ResponseContext {
    users: HashMap<i64, ApiUserPartialResponse>,
    reactions: HashMap<i64, Vec<ApiMessageReactionResponse>>,
    referenced_messages: HashMap<(i64, i64), Message>,
    attachment_decay: HashMap<i64, DateTime<Utc>>,
    channel_mentions: HashMap<String, ApiChannelMentionResponse>,
    mention_context: HashMap<i64, MessageMentionContext>,
}

#[derive(Debug, Default)]
struct MessageMentionContext {
    content: MessageMentions,
    snapshots: Vec<MessageMentions>,
    embed_users: HashSet<i64>,
}

impl<T: Transport> MessagesShard<T> {
    pub fn new_postgres(kv: postgres::KvClient, transport: T) -> anyhow::Result<Self> {
        Ok(Self {
            storage: MessagesStorage::Postgres(PostgresMessagesStorage { kv }),
            transport,
        })
    }

    #[cfg(feature = "scylla")]
    pub async fn new_scylla(db: Arc<Session>, transport: T) -> anyhow::Result<Self> {
        let stmt_get_by_id = db
            .prepare(format!(
                "SELECT {MESSAGE_COLUMNS} FROM messages WHERE channel_id = ? AND bucket = ? AND message_id = ? LIMIT 1"
            ))
            .await?;
        let stmt_get_latest = db
            .prepare(format!(
                "SELECT {MESSAGE_COLUMNS} FROM messages WHERE channel_id = ? AND bucket = ? ORDER BY message_id DESC LIMIT ?"
            ))
            .await?;
        let stmt_get_before = db
            .prepare(format!(
                "SELECT {MESSAGE_COLUMNS} FROM messages WHERE channel_id = ? AND bucket = ? AND message_id < ? ORDER BY message_id DESC LIMIT ?"
            ))
            .await?;
        let stmt_get_after = db
            .prepare(format!(
                "SELECT {MESSAGE_COLUMNS} FROM messages WHERE channel_id = ? AND bucket = ? AND message_id > ? ORDER BY message_id ASC LIMIT ?"
            ))
            .await?;
        let stmt_list_buckets_desc = db
            .prepare(
                "SELECT bucket FROM channel_message_buckets WHERE channel_id = ? AND bucket >= ? AND bucket <= ? ORDER BY bucket DESC LIMIT ?"
            )
            .await?;
        let stmt_list_buckets_asc = db
            .prepare(
                "SELECT bucket FROM channel_message_buckets WHERE channel_id = ? AND bucket >= ? AND bucket <= ? ORDER BY bucket ASC LIMIT ?"
            )
            .await?;
        let stmt_get_reactions = db
            .prepare(
                "SELECT user_id, emoji_id, emoji_name, emoji_animated, created_at FROM message_reactions WHERE channel_id = ? AND bucket = ? AND message_id = ?"
            )
            .await?;
        let stmt_get_reactions_for_messages = db
            .prepare(
                "SELECT message_id, user_id, emoji_id, emoji_name, emoji_animated, created_at FROM message_reactions WHERE channel_id = ? AND bucket = ? AND message_id IN ?"
            )
            .await?;
        let stmt_get_attachment_decay = db
            .prepare(
                "SELECT attachment_id, expires_at FROM attachment_decay_by_id WHERE attachment_id = ? LIMIT 1"
            )
            .await?;
        let stmt_get_attachment_decay_many = db
            .prepare(
                "SELECT attachment_id, expires_at FROM attachment_decay_by_id WHERE attachment_id IN ?"
            )
            .await?;
        let stmt_delete_message = db
            .prepare("DELETE FROM messages WHERE channel_id = ? AND bucket = ? AND message_id = ?")
            .await?;

        Ok(Self {
            storage: MessagesStorage::Scylla(Box::new(ScyllaMessagesStorage {
                db,
                stmt_get_by_id,
                stmt_get_latest,
                stmt_get_before,
                stmt_get_after,
                stmt_list_buckets_desc,
                stmt_list_buckets_asc,
                stmt_get_reactions,
                stmt_get_reactions_for_messages,
                stmt_get_attachment_decay,
                stmt_get_attachment_decay_many,
                stmt_delete_message,
            })),
            transport,
        })
    }

    async fn get_by_id(&self, channel_id: i64, message_id: i64) -> anyhow::Result<Option<Message>> {
        let bucket = snowflake_to_bucket(message_id);
        self.storage.get_by_id(channel_id, bucket, message_id).await
    }

    async fn get_latest(&self, channel_id: i64, limit: u32) -> anyhow::Result<Vec<Message>> {
        if limit == 0 {
            return Ok(Vec::new());
        }
        self.scan_indexed_buckets_desc(channel_id, 0, current_bucket(), limit, None)
            .await
    }

    async fn get_before(
        &self,
        channel_id: i64,
        before_id: i64,
        limit: u32,
    ) -> anyhow::Result<Vec<Message>> {
        if limit == 0 {
            return Ok(Vec::new());
        }
        self.scan_indexed_buckets_desc(
            channel_id,
            0,
            snowflake_to_bucket(before_id),
            limit,
            Some(before_id),
        )
        .await
    }

    async fn get_after(
        &self,
        channel_id: i64,
        after_id: i64,
        limit: u32,
    ) -> anyhow::Result<Vec<Message>> {
        if limit == 0 {
            return Ok(Vec::new());
        }
        let min_bucket = snowflake_to_bucket(after_id);
        let max_bucket = current_bucket().max(min_bucket);
        self.scan_indexed_buckets_asc(channel_id, min_bucket, max_bucket, limit, after_id)
            .await
    }

    async fn scan_indexed_buckets_desc(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
        before_id: Option<i64>,
    ) -> anyhow::Result<Vec<Message>> {
        let mut messages = Vec::new();
        let mut cursor_max = max_bucket;
        let limit_i32 = limit.min(i32::MAX as u32) as i32;
        while messages.len() < limit as usize && cursor_max >= min_bucket {
            let buckets = self
                .list_buckets_desc(channel_id, min_bucket, cursor_max, bucket_page_limit(limit))
                .await?;
            let Some(last_bucket) = buckets.last().copied() else {
                break;
            };
            collect_bucket_waves(
                &buckets,
                limit as usize,
                &mut messages,
                |bucket| async move {
                    if let Some(before_id) = before_id {
                        self.fetch_before_bucket(channel_id, bucket, before_id, limit_i32)
                            .await
                    } else {
                        self.fetch_latest_bucket(channel_id, bucket, limit_i32)
                            .await
                    }
                },
            )
            .await?;
            if messages.len() >= limit as usize || last_bucket <= min_bucket {
                break;
            }
            cursor_max = last_bucket.saturating_sub(1);
        }
        messages.sort_unstable_by_key(|message| std::cmp::Reverse(message.message_id));
        messages.truncate(limit as usize);
        Ok(messages)
    }

    async fn scan_indexed_buckets_asc(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
        after_id: i64,
    ) -> anyhow::Result<Vec<Message>> {
        let mut messages = Vec::new();
        let mut cursor_min = min_bucket;
        let limit_i32 = limit.min(i32::MAX as u32) as i32;
        while messages.len() < limit as usize && cursor_min <= max_bucket {
            let buckets = self
                .list_buckets_asc(channel_id, cursor_min, max_bucket, bucket_page_limit(limit))
                .await?;
            let Some(last_bucket) = buckets.last().copied() else {
                break;
            };
            collect_bucket_waves(
                &buckets,
                limit as usize,
                &mut messages,
                |bucket| async move {
                    self.fetch_after_bucket(channel_id, bucket, after_id, limit_i32)
                        .await
                },
            )
            .await?;
            if messages.len() >= limit as usize || last_bucket >= max_bucket {
                break;
            }
            cursor_min = last_bucket.saturating_add(1);
        }
        messages.sort_unstable_by_key(|left| left.message_id);
        messages.truncate(limit as usize);
        Ok(messages)
    }

    async fn list_buckets_desc(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
    ) -> anyhow::Result<Vec<i32>> {
        if min_bucket > max_bucket || limit == 0 {
            return Ok(Vec::new());
        }
        self.storage
            .list_buckets_desc(channel_id, min_bucket, max_bucket, limit)
            .await
    }

    async fn list_buckets_asc(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
    ) -> anyhow::Result<Vec<i32>> {
        if min_bucket > max_bucket || limit == 0 {
            return Ok(Vec::new());
        }
        self.storage
            .list_buckets_asc(channel_id, min_bucket, max_bucket, limit)
            .await
    }

    async fn fetch_latest_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        self.storage
            .fetch_latest_bucket(channel_id, bucket, limit)
            .await
    }

    async fn fetch_before_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        before_id: i64,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        self.storage
            .fetch_before_bucket(channel_id, bucket, before_id, limit)
            .await
    }

    async fn fetch_after_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        after_id: i64,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        self.storage
            .fetch_after_bucket(channel_id, bucket, after_id, limit)
            .await
    }

    async fn fetch_page(
        &self,
        channel_id: i64,
        cursor: PageCursor,
        limit: u32,
        floor: Option<i64>,
        excluded_types: &[i32],
    ) -> anyhow::Result<Vec<Message>> {
        if excluded_types.is_empty() {
            return match cursor {
                PageCursor::Latest => self.get_latest(channel_id, limit).await,
                PageCursor::Before(before_id) => {
                    self.get_before(channel_id, before_id, limit).await
                }
                PageCursor::After(after_id) => self.get_after(channel_id, after_id, limit).await,
            };
        }
        match cursor {
            PageCursor::Latest => {
                collect_excluding(limit, excluded_types, None, |cursor, size| async move {
                    match cursor {
                        Some(cursor) => self.get_before(channel_id, cursor, size).await,
                        None => self.get_latest(channel_id, size).await,
                    }
                })
                .await
            }
            PageCursor::Before(before_id) => {
                collect_excluding(
                    limit,
                    excluded_types,
                    Some(before_id),
                    |cursor, size| async move {
                        let mut page = self
                            .get_before(channel_id, cursor.unwrap_or(before_id), size)
                            .await?;
                        if let Some(floor) = floor {
                            page.retain(|message| message.message_id > floor);
                        }
                        Ok(page)
                    },
                )
                .await
            }
            PageCursor::After(after_id) => {
                collect_excluding(limit, excluded_types, Some(after_id), |cursor, size| {
                    self.get_after(channel_id, cursor.unwrap_or(after_id), size)
                })
                .await
            }
        }
    }

    async fn visible_page(
        &self,
        channel_id: i64,
        start: PageCursor,
        limit: u32,
        floor: Option<i64>,
        options: &ResponseBuildOptions,
        users: &mut UserLookup,
    ) -> anyhow::Result<Vec<Message>> {
        let mut out = Vec::new();
        let mut cursor = start;
        let mut page_size = limit;
        for _ in 0..HIDDEN_REFILL_ROUNDS {
            if limit == 0 {
                break;
            }
            let page = self
                .fetch_page(
                    channel_id,
                    cursor,
                    page_size,
                    floor,
                    &options.excluded_types,
                )
                .await?;
            let exhausted = page.len() < page_size as usize;
            let reached_floor =
                floor.is_some_and(|floor| page.iter().any(|message| message.message_id <= floor));
            let next = cursor.next(&page);
            let hidden = self.hidden_message_ids(&page, options, users).await;
            let refill = !hidden.is_empty();
            out.extend(
                page.into_iter()
                    .filter(|message| !hidden.contains(&message.message_id)),
            );
            let Some(next) = next else {
                break;
            };
            if !refill || exhausted || reached_floor || out.len() >= limit as usize {
                break;
            }
            cursor = next;
            page_size = page_size
                .saturating_mul(2)
                .min(HIDDEN_REFILL_MAX_PAGE.max(limit));
        }
        if matches!(start, PageCursor::After(_)) {
            out.sort_unstable_by_key(|message| message.message_id);
        } else {
            out.sort_unstable_by_key(|message| std::cmp::Reverse(message.message_id));
        }
        out.truncate(limit as usize);
        Ok(out)
    }

    async fn visible_around(
        &self,
        channel_id: i64,
        around_id: i64,
        limit: u32,
        options: &ResponseBuildOptions,
        users: &mut UserLookup,
    ) -> anyhow::Result<Vec<Message>> {
        if limit == 0 {
            return Ok(Vec::new());
        }
        let (newer_limit, older_limit) = around_window_limits(limit);
        let mut target_users = UserLookup::default();
        let mut newer_users = UserLookup::default();
        let mut older_users = UserLookup::default();
        let (target, newer, older) = tokio::try_join!(
            async {
                let target = self.get_by_id(channel_id, around_id).await?;
                let Some(target) = target else {
                    return anyhow::Ok(None);
                };
                let hidden = self
                    .hidden_message_ids(std::slice::from_ref(&target), options, &mut target_users)
                    .await;
                Ok((!hidden.contains(&target.message_id)
                    && !options.excluded_types.contains(&target.message_type))
                .then_some(target))
            },
            self.visible_page(
                channel_id,
                PageCursor::After(around_id),
                newer_limit,
                None,
                options,
                &mut newer_users,
            ),
            self.visible_page(
                channel_id,
                PageCursor::Before(around_id),
                older_limit,
                None,
                options,
                &mut older_users,
            )
        )?;
        users.merge(target_users);
        users.merge(newer_users);
        users.merge(older_users);
        let mut out = Vec::new();
        let mut seen = HashSet::new();
        for message in newer.into_iter().rev() {
            if seen.insert(message.message_id) {
                out.push(message);
            }
        }
        if let Some(message) = target
            && seen.insert(message.message_id)
        {
            out.push(message);
        }
        for message in older {
            if seen.insert(message.message_id) {
                out.push(message);
            }
        }
        out.truncate(limit as usize);
        Ok(out)
    }

    async fn list_api_responses(
        &self,
        channel_id: i64,
        limit: u32,
        before_id: Option<i64>,
        after_id: Option<i64>,
        around_id: Option<i64>,
        options: ResponseBuildOptions,
    ) -> anyhow::Result<Vec<ApiMessageResponse>> {
        if !options.can_read_message_history && options.message_history_cutoff_ms.is_none() {
            return Ok(Vec::new());
        }
        let mut users = UserLookup::default();
        let mut messages = if let Some(around_id) = around_id {
            self.visible_around(channel_id, around_id, limit, &options, &mut users)
                .await?
        } else if let (Some(before_id), Some(after_id)) = (before_id, after_id) {
            let mut before = self
                .visible_page(
                    channel_id,
                    PageCursor::Before(before_id),
                    limit,
                    Some(after_id),
                    &options,
                    &mut users,
                )
                .await?;
            before.retain(|message| message.message_id > after_id);
            before
        } else {
            let cursor = match (before_id, after_id) {
                (Some(before_id), _) => PageCursor::Before(before_id),
                (None, Some(after_id)) => PageCursor::After(after_id),
                (None, None) => PageCursor::Latest,
            };
            self.visible_page(channel_id, cursor, limit, None, &options, &mut users)
                .await?
        };
        messages
            .retain(|message| self.is_message_visible_to_requester(message.message_id, &options));
        let (mut messages, orphaned_messages): (Vec<_>, Vec<_>) = messages
            .into_iter()
            .partition(|message| message.author_id.is_some() || message.webhook_id.is_some());
        self.cleanup_orphaned_messages(orphaned_messages).await;
        messages.sort_unstable_by_key(|message| std::cmp::Reverse(message.message_id));
        let context = self
            .build_response_context(&messages, &options, true, users)
            .await?;
        Ok(messages
            .iter()
            .map(|message| self.map_message_response(message, &options, &context, true))
            .collect())
    }

    async fn get_api_response(
        &self,
        channel_id: i64,
        message_id: i64,
        options: ResponseBuildOptions,
    ) -> anyhow::Result<Option<ApiMessageResponse>> {
        if !self.is_message_visible_to_requester(message_id, &options) {
            return Ok(None);
        }
        let Some(message) = self.get_by_id(channel_id, message_id).await? else {
            return Ok(None);
        };
        if options.hides(&message) {
            return Ok(None);
        }
        if message.author_id.is_none() && message.webhook_id.is_none() {
            self.cleanup_orphaned_messages(vec![message]).await;
            return Ok(None);
        }
        let mut users = UserLookup::default();
        let hidden = self
            .hidden_message_ids(std::slice::from_ref(&message), &options, &mut users)
            .await;
        if !hidden.is_empty() {
            return Ok(None);
        }
        let context = self
            .build_response_context(std::slice::from_ref(&message), &options, true, users)
            .await?;
        Ok(Some(
            self.map_message_response(&message, &options, &context, true),
        ))
    }

    async fn build_api_response_from_message(
        &self,
        message: Message,
        options: ResponseBuildOptions,
    ) -> anyhow::Result<Option<ApiMessageResponse>> {
        if !self.is_message_visible_to_requester(message.message_id, &options)
            || options.hides(&message)
        {
            return Ok(None);
        }
        if message.author_id.is_none() && message.webhook_id.is_none() {
            self.cleanup_orphaned_messages(vec![message]).await;
            return Ok(None);
        }
        let context = self
            .build_response_context(
                std::slice::from_ref(&message),
                &options,
                true,
                UserLookup::default(),
            )
            .await?;
        Ok(Some(
            self.map_message_response(&message, &options, &context, true),
        ))
    }

    async fn build_api_responses_from_messages(
        &self,
        messages: Vec<Message>,
        options: ResponseBuildOptions,
    ) -> anyhow::Result<Vec<ApiMessageResponse>> {
        let (messages, orphaned_messages): (Vec<_>, Vec<_>) = messages
            .into_iter()
            .filter(|message| {
                self.is_message_visible_to_requester(message.message_id, &options)
                    && !options.hides(message)
            })
            .partition(|message| message.author_id.is_some() || message.webhook_id.is_some());
        self.cleanup_orphaned_messages(orphaned_messages).await;
        let mut users = UserLookup::default();
        let hidden = self
            .hidden_message_ids(&messages, &options, &mut users)
            .await;
        let messages = messages
            .into_iter()
            .filter(|message| !hidden.contains(&message.message_id))
            .collect::<Vec<_>>();
        let context = self
            .build_response_context(&messages, &options, true, users)
            .await?;
        Ok(messages
            .iter()
            .map(|message| self.map_message_response(message, &options, &context, true))
            .collect())
    }

    async fn hidden_message_ids(
        &self,
        messages: &[Message],
        options: &ResponseBuildOptions,
        users: &mut UserLookup,
    ) -> HashSet<i64> {
        if options.include_hidden || messages.is_empty() {
            return HashSet::new();
        }
        let copies = messages
            .iter()
            .filter_map(|message| copy_source(message).map(|source| (message.message_id, source)))
            .collect::<Vec<_>>();
        let sources = stream::iter(copies)
            .map(|(message_id, (channel_id, source_id))| async move {
                match self.get_by_id(channel_id, source_id).await {
                    Ok(Some(source)) => Some((message_id, (source.author_id, source.message_id))),
                    _ => None,
                }
            })
            .buffer_unordered(ENRICHMENT_QUERY_CONCURRENCY)
            .filter_map(|source| async move { source })
            .collect::<HashMap<_, _>>()
            .await;
        let user_ids = messages
            .iter()
            .flat_map(|message| {
                message
                    .author_id
                    .into_iter()
                    .chain(message.mention_users.iter().copied())
            })
            .chain(sources.values().filter_map(|(author_id, _)| *author_id))
            .collect::<HashSet<_>>();
        self.load_user_partials(user_ids, users).await;
        messages
            .iter()
            .filter(|message| {
                users.hides(message.author_id, message.message_id)
                    || sources
                        .get(&message.message_id)
                        .is_some_and(|(author_id, source_id)| users.hides(*author_id, *source_id))
            })
            .map(|message| message.message_id)
            .collect()
    }

    fn is_message_visible_to_requester(
        &self,
        message_id: i64,
        options: &ResponseBuildOptions,
    ) -> bool {
        if options.can_read_message_history {
            return true;
        }
        let Some(cutoff) = options.message_history_cutoff_ms else {
            return false;
        };
        snowflake_to_epoch_millis(message_id) >= cutoff
    }

    async fn cleanup_orphaned_messages(&self, messages: Vec<Message>) {
        if messages.is_empty() {
            return;
        }
        let count = messages.len();
        let failures = stream::iter(messages)
            .map(|message| async move {
                let bucket = snowflake_to_bucket(message.message_id);
                self.storage
                    .delete_message(message.channel_id, bucket, message.message_id)
                    .await
                    .map_err(|error| (message.channel_id, message.message_id, error))
            })
            .buffer_unordered(ENRICHMENT_QUERY_CONCURRENCY)
            .filter_map(|result| async move { result.err() })
            .collect::<Vec<_>>()
            .await;
        if failures.is_empty() {
            tracing::info!(count, "cleaned orphaned messages during response build");
        } else {
            tracing::warn!(
                count,
                failure_count = failures.len(),
                first_channel_id = failures[0].0,
                first_message_id = failures[0].1,
                error = %failures[0].2,
                "failed to clean some orphaned messages during response build"
            );
        }
    }

    async fn build_response_context(
        &self,
        messages: &[Message],
        options: &ResponseBuildOptions,
        include_referenced_messages: bool,
        mut users: UserLookup,
    ) -> anyhow::Result<ResponseContext> {
        let mut referenced_messages = if include_referenced_messages {
            self.fetch_referenced_messages(messages, options).await
        } else {
            HashMap::new()
        };
        let all_messages = messages
            .iter()
            .chain(referenced_messages.values())
            .collect::<Vec<_>>();
        let mention_context = build_message_mention_context(&all_messages);
        let attachment_ids = collect_attachment_ids(&all_messages);
        let channel_ids = collect_channel_mention_ids(&all_messages, &mention_context);
        let user_ids = collect_user_ids(&all_messages, &mention_context);
        let reactions_future = self.fetch_reactions_for_messages(messages, options);
        let attachment_decay_future = self.fetch_attachment_decay(attachment_ids);
        let channel_mentions_future = self.resolve_channel_mentions(channel_ids, options);
        let users_future = self.load_user_partials(user_ids, &mut users);
        let (reactions, attachment_decay, channel_mentions, ()) = tokio::join!(
            reactions_future,
            attachment_decay_future,
            channel_mentions_future,
            users_future
        );
        let attachment_decay = attachment_decay?;
        if !options.include_hidden {
            referenced_messages
                .retain(|_, referenced| !users.hides(referenced.author_id, referenced.message_id));
        }
        let users = users
            .partials
            .into_values()
            .map(|partial| (partial.user_id, map_user_partial(partial)))
            .collect();
        Ok(ResponseContext {
            users,
            reactions,
            referenced_messages,
            attachment_decay,
            channel_mentions,
            mention_context,
        })
    }

    async fn fetch_referenced_messages(
        &self,
        messages: &[Message],
        options: &ResponseBuildOptions,
    ) -> HashMap<(i64, i64), Message> {
        let mut refs = HashSet::new();
        for message in messages {
            let Some((channel_id, message_id)) = reply_target(message) else {
                continue;
            };
            if !self.is_message_visible_to_requester(message_id, options) {
                continue;
            }
            refs.insert((channel_id, message_id));
        }
        stream::iter(refs)
            .map(|(channel_id, message_id)| async move {
                match self.get_by_id(channel_id, message_id).await {
                    Ok(Some(message))
                        if message.author_id.is_some() || message.webhook_id.is_some() =>
                    {
                        Some(((channel_id, message_id), message))
                    }
                    _ => None,
                }
            })
            .buffer_unordered(ENRICHMENT_QUERY_CONCURRENCY)
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .collect()
    }

    async fn fetch_reactions_for_messages(
        &self,
        messages: &[Message],
        options: &ResponseBuildOptions,
    ) -> HashMap<i64, Vec<ApiMessageReactionResponse>> {
        if !options.include_reactions {
            return HashMap::new();
        }
        let mut groups: HashMap<(i64, i32), Vec<i64>> = HashMap::new();
        for message in messages {
            if message.has_reaction == Some(false) {
                continue;
            }
            let bucket = snowflake_to_bucket(message.message_id);
            groups
                .entry((message.channel_id, bucket))
                .or_default()
                .push(message.message_id);
        }
        let mut batches = Vec::new();
        for ((channel_id, bucket), mut message_ids) in groups {
            message_ids.sort_unstable();
            message_ids.dedup();
            for chunk in message_ids.chunks(REACTION_MESSAGE_BATCH_SIZE) {
                batches.push((channel_id, bucket, chunk.to_vec()));
            }
        }
        stream::iter(batches)
            .map(|(channel_id, bucket, message_ids)| async move {
                self.fetch_reactions_for_message_batch(
                    channel_id,
                    bucket,
                    message_ids,
                    options.viewer_user_id,
                )
                .await
            })
            .buffer_unordered(ENRICHMENT_QUERY_CONCURRENCY)
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .collect()
    }

    async fn fetch_reactions_for_message_batch(
        &self,
        channel_id: i64,
        bucket: i32,
        message_ids: Vec<i64>,
        viewer_user_id: i64,
    ) -> HashMap<i64, Vec<ApiMessageReactionResponse>> {
        if message_ids.is_empty() {
            return HashMap::new();
        }
        self.storage
            .fetch_reactions_for_message_batch(channel_id, bucket, message_ids, viewer_user_id)
            .await
    }

    async fn fetch_attachment_decay(
        &self,
        attachment_ids: HashSet<i64>,
    ) -> anyhow::Result<HashMap<i64, DateTime<Utc>>> {
        if attachment_ids.is_empty() {
            return Ok(HashMap::new());
        }
        let mut attachment_ids = attachment_ids.into_iter().collect::<Vec<_>>();
        attachment_ids.sort_unstable();
        attachment_ids.dedup();
        let batches = attachment_ids
            .chunks(ATTACHMENT_DECAY_BATCH_SIZE)
            .map(<[i64]>::to_vec)
            .collect::<Vec<_>>();
        let maps =
            stream::iter(batches)
                .map(|attachment_ids| async move {
                    self.fetch_attachment_decay_batch(attachment_ids).await
                })
                .buffer_unordered(ENRICHMENT_QUERY_CONCURRENCY)
                .collect::<Vec<_>>()
                .await
                .into_iter()
                .collect::<anyhow::Result<Vec<_>>>()?;
        Ok(maps.into_iter().flatten().collect())
    }

    async fn fetch_attachment_decay_batch(
        &self,
        attachment_ids: Vec<i64>,
    ) -> anyhow::Result<HashMap<i64, DateTime<Utc>>> {
        self.storage
            .fetch_attachment_decay_batch(attachment_ids)
            .await
    }

    async fn load_user_partials(&self, user_ids: HashSet<i64>, users: &mut UserLookup) {
        let mut missing = user_ids
            .into_iter()
            .filter(|user_id| users.requested.insert(*user_id))
            .collect::<Vec<_>>();
        if missing.is_empty() {
            return;
        }
        missing.sort_unstable();
        let payload = serde_json::json!({
            "op": "GetPartialsByIds",
            "user_ids": missing,
        });
        let payload_bytes = serde_json::to_vec(&payload).unwrap_or_default();
        let response = self
            .transport
            .request(
                "svc.users",
                payload_bytes.as_slice(),
                SERVICE_REQUEST_TIMEOUT,
            )
            .await
            .ok()
            .and_then(|bytes| serde_json::from_slice::<UserServiceResponse>(&bytes).ok());
        let partials = match response {
            Some(UserServiceResponse::FoundPartials(partials)) => partials,
            Some(UserServiceResponse::FoundPartial(partial)) => vec![partial],
            _ => Vec::new(),
        };
        users.partials.extend(
            partials
                .into_iter()
                .map(|partial| (partial.user_id, partial)),
        );
    }

    async fn resolve_channel_mentions(
        &self,
        channel_ids: HashSet<i64>,
        options: &ResponseBuildOptions,
    ) -> HashMap<String, ApiChannelMentionResponse> {
        let Some(guild_id) = options.source_guild_id else {
            return HashMap::new();
        };
        if channel_ids.is_empty() {
            return HashMap::new();
        }
        let mut channel_ids: Vec<String> =
            channel_ids.into_iter().map(|id| id.to_string()).collect();
        channel_ids.sort();
        channel_ids.dedup();
        let mut payload = serde_json::json!({
            "guild_id": guild_id.to_string(),
            "channel_ids": channel_ids,
        });
        if options.threads_mask {
            payload["exclude_thread_channels"] = serde_json::Value::Bool(true);
        }
        let payload_bytes = serde_json::to_vec(&payload).unwrap_or_default();
        let response = self
            .transport
            .request(
                "rpc.gateway.guild.resolve_channel_mentions",
                payload_bytes.as_slice(),
                SERVICE_REQUEST_TIMEOUT,
            )
            .await
            .ok()
            .and_then(|bytes| {
                serde_json::from_slice::<GatewayRpcResponse<GatewayChannelMentionsResult>>(&bytes)
                    .ok()
            });
        let Some(response) = response else {
            return HashMap::new();
        };
        if !response.ok {
            return HashMap::new();
        }
        response
            .result
            .and_then(|result| result.channels)
            .unwrap_or_default()
            .into_iter()
            .filter(|channel| {
                !(options.threads_mask
                    && THREAD_FEATURE_CHANNEL_TYPES.contains(&channel.channel_type))
            })
            .map(|channel| {
                let item = ApiChannelMentionResponse {
                    id: channel.id,
                    name: channel.name,
                    channel_type: channel.channel_type,
                };
                (item.id.clone(), item)
            })
            .collect()
    }

    fn map_message_response(
        &self,
        message: &Message,
        options: &ResponseBuildOptions,
        context: &ResponseContext,
        include_referenced_message: bool,
    ) -> ApiMessageResponse {
        let author = self.resolve_author(message, context);
        let storage_channel_id = attachment_storage_channel_id(message);
        let attachments = message
            .attachments
            .as_deref()
            .unwrap_or_default()
            .iter()
            .filter_map(|attachment| {
                self.map_attachment(storage_channel_id, attachment, options, context)
            })
            .collect();
        let embeds = if (message.flags.unwrap_or_default() & MESSAGE_FLAG_SUPPRESS_EMBEDS) == 0 {
            message
                .embeds
                .as_deref()
                .unwrap_or_default()
                .iter()
                .map(|embed| self.map_embed(embed, options))
                .collect()
        } else {
            Vec::new()
        };
        let poll = message.poll.clone();
        let stickers = message
            .sticker_items
            .as_deref()
            .unwrap_or_default()
            .iter()
            .filter_map(map_sticker)
            .collect();
        let fallback_mentions;
        let message_mentions = match context.mention_context.get(&message.message_id) {
            Some(mentions) => mentions,
            None => {
                fallback_mentions = build_mention_context_entry(message);
                &fallback_mentions
            }
        };
        let content_mentions = &message_mentions.content;
        let mention_roles = ids_present_in_set(&message.mention_roles, &content_mentions.roles);
        let mention_channels =
            ids_present_in_set(&message.mention_channels, &content_mentions.channels)
                .into_iter()
                .filter_map(|id| context.channel_mentions.get(&id).cloned())
                .collect::<Vec<_>>();
        let mut referenced_user_ids = content_mentions.users.clone();
        referenced_user_ids.extend(message_mentions.embed_users.iter().copied());
        for snapshot_mentions in &message_mentions.snapshots {
            referenced_user_ids.extend(snapshot_mentions.users.iter().copied());
        }
        let mentioned_user_ids = message
            .mention_users
            .iter()
            .copied()
            .filter(|id| content_mentions.users.contains(id) || !referenced_user_ids.contains(id))
            .collect::<Vec<_>>();
        let mentions = mentioned_user_ids
            .iter()
            .filter_map(|id| context.users.get(id).cloned())
            .collect::<Vec<_>>();
        for id in &mentioned_user_ids {
            referenced_user_ids.remove(id);
        }
        let users = referenced_user_ids
            .into_iter()
            .filter_map(|id| context.users.get(&id).cloned())
            .collect::<Vec<_>>();
        let referenced_message = if include_referenced_message {
            reply_target(message).map(|(channel_id, message_id)| {
                context
                    .referenced_messages
                    .get(&(channel_id, message_id))
                    .filter(|referenced| !options.hides(referenced))
                    .map(|referenced| {
                        let mut referenced_options = options.clone();
                        referenced_options.nonce = None;
                        referenced_options.tts = false;
                        Box::new(self.map_message_response(
                            referenced,
                            &referenced_options,
                            context,
                            false,
                        ))
                    })
            })
        } else {
            None
        };
        ApiMessageResponse {
            id: message.message_id.to_string(),
            channel_id: message.channel_id.to_string(),
            author,
            webhook_id: message.webhook_id.map(|id| id.to_string()),
            message_type: message.message_type,
            flags: options.mask_flags(message.flags.unwrap_or_default()),
            content: message.content.clone().unwrap_or_default(),
            timestamp: epoch_millis_to_iso(snowflake_to_epoch_millis(message.message_id)),
            edited_timestamp: message.edited_timestamp.map(epoch_millis_to_iso),
            pinned: message.pinned.unwrap_or(false),
            mention_everyone: message.mention_everyone.unwrap_or(false),
            tts: options.tts,
            mentions,
            mention_roles,
            mention_channels: (!mention_channels.is_empty()).then_some(mention_channels),
            users: (!users.is_empty()).then_some(users),
            embeds,
            poll,
            attachments,
            stickers,
            reactions: context.reactions.get(&message.message_id).cloned(),
            message_reference: message
                .message_reference
                .as_ref()
                .and_then(map_message_reference),
            message_snapshots: message.message_snapshots.as_ref().and_then(|snapshots| {
                let mapped: Vec<_> = snapshots
                    .iter()
                    .enumerate()
                    .map(|(index, snapshot)| {
                        self.map_snapshot(
                            message.message_id,
                            message.channel_id,
                            index,
                            snapshot,
                            options,
                            context,
                        )
                    })
                    .collect();
                (!mapped.is_empty()).then_some(mapped)
            }),
            nonce: options.nonce.clone(),
            call: message.call.as_ref().map(map_call),
            referenced_message,
        }
    }

    fn resolve_author(
        &self,
        message: &Message,
        context: &ResponseContext,
    ) -> ApiUserPartialResponse {
        if let Some(author_id) = message.author_id {
            return context
                .users
                .get(&author_id)
                .cloned()
                .unwrap_or_else(|| deleted_user(author_id));
        }
        if let (Some(webhook_id), Some(webhook_name)) =
            (message.webhook_id, message.webhook_name.clone())
        {
            return ApiUserPartialResponse {
                id: webhook_id.to_string(),
                username: webhook_name,
                discriminator: "0000".to_owned(),
                global_name: None,
                avatar: message.webhook_avatar_hash.clone(),
                avatar_color: None,
                bot: Some(true),
                system: None,
                flags: 0,
                mention_flags: None,
            };
        }
        deleted_user(0)
    }

    fn map_attachment(
        &self,
        channel_id: i64,
        attachment: &MessageAttachment,
        options: &ResponseBuildOptions,
        context: &ResponseContext,
    ) -> Option<ApiMessageAttachmentResponse> {
        let attachment_id = attachment.attachment_id?;
        let filename = attachment.filename.clone().unwrap_or_default();
        let decay = context.attachment_decay.get(&attachment_id);
        let expired =
            decay.is_some_and(|expires_at| expires_at.timestamp_millis() <= now_epoch_millis());
        let url = (!expired).then(|| {
            attachment_cdn_url(
                channel_id,
                attachment_id,
                &filename,
                options,
                now_epoch_secs(),
            )
        });
        let content_type = attachment.content_type.clone().unwrap_or_else(|| {
            mime_guess::from_path(&filename)
                .first_or_octet_stream()
                .essence_str()
                .to_owned()
        });
        let is_audio = content_type.to_ascii_lowercase().starts_with("audio/");
        Some(ApiMessageAttachmentResponse {
            id: attachment_id.to_string(),
            filename,
            title: attachment.title.clone(),
            description: attachment.description.clone(),
            content_type: Some(content_type),
            content_hash: attachment.content_hash.clone(),
            size: assert_safe_byte_size(attachment.size.unwrap_or_default()),
            url: url.clone(),
            proxy_url: url,
            width: (!is_audio).then_some(attachment.width).flatten(),
            height: (!is_audio).then_some(attachment.height).flatten(),
            placeholder: attachment.placeholder.clone(),
            flags: attachment.flags.unwrap_or_default(),
            nsfw: attachment.nsfw,
            duration: attachment.duration_secs.or(attachment.duration),
            waveform: attachment.waveform.clone(),
            expires_at: decay.map(|dt| dt.to_rfc3339_opts(chrono::SecondsFormat::Millis, true)),
            expired: expired.then_some(true),
        })
    }

    fn map_embed(
        &self,
        embed: &MessageEmbed,
        options: &ResponseBuildOptions,
    ) -> ApiMessageEmbedResponse {
        let base = self.map_embed_child_like(
            embed.embed_type.clone(),
            embed.title.clone(),
            embed.description.clone(),
            embed.url.clone(),
            embed.timestamp,
            embed.color,
            embed.author.clone(),
            embed.provider.clone(),
            embed.thumbnail.clone(),
            embed.image.clone(),
            embed.video.clone(),
            embed.audio.clone(),
            embed.footer.clone(),
            embed.fields.clone(),
            embed.nsfw,
            embed.html.clone(),
            embed.html_width,
            embed.html_height,
            options,
        );
        let children = embed.children.as_ref().map(|children| {
            children
                .iter()
                .map(|child| {
                    self.map_embed_child_like(
                        child.embed_type.clone(),
                        child.title.clone(),
                        child.description.clone(),
                        child.url.clone(),
                        child.timestamp,
                        child.color,
                        child.author.clone(),
                        child.provider.clone(),
                        child.thumbnail.clone(),
                        child.image.clone(),
                        child.video.clone(),
                        child.audio.clone(),
                        child.footer.clone(),
                        child.fields.clone(),
                        child.nsfw,
                        child.html.clone(),
                        child.html_width,
                        child.html_height,
                        options,
                    )
                })
                .collect::<Vec<_>>()
        });
        ApiMessageEmbedResponse {
            base,
            children: children.filter(|children| !children.is_empty()),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn map_embed_child_like(
        &self,
        embed_type: Option<String>,
        title: Option<String>,
        description: Option<String>,
        url: Option<String>,
        timestamp: Option<i64>,
        color: Option<i32>,
        author: Option<MessageEmbedAuthor>,
        provider: Option<MessageEmbedProvider>,
        thumbnail: Option<MessageEmbedMedia>,
        image: Option<MessageEmbedMedia>,
        video: Option<MessageEmbedMedia>,
        audio: Option<MessageEmbedMedia>,
        footer: Option<MessageEmbedFooter>,
        fields: Option<Vec<MessageEmbedField>>,
        nsfw: Option<bool>,
        html: Option<String>,
        html_width: Option<i32>,
        html_height: Option<i32>,
        options: &ResponseBuildOptions,
    ) -> ApiMessageEmbedChildResponse {
        ApiMessageEmbedChildResponse {
            embed_type: embed_type.unwrap_or_else(|| "rich".to_owned()),
            title,
            description,
            url: url.map(|url| sign_own_url(&url, options)),
            timestamp: timestamp.map(epoch_millis_to_iso),
            color,
            author: author.and_then(|author| {
                author.name.map(|name| ApiEmbedAuthorResponse {
                    name,
                    url: author.url.map(|url| sign_own_url(&url, options)),
                    proxy_icon_url: author
                        .icon_url
                        .as_ref()
                        .map(|url| external_media_proxy_url(url, options)),
                    icon_url: author.icon_url.map(|url| sign_own_url(&url, options)),
                })
            }),
            provider: provider.and_then(|provider| {
                provider.name.map(|name| ApiEmbedProviderResponse {
                    name,
                    url: provider.url.map(|url| sign_own_url(&url, options)),
                    icon_url: None,
                    proxy_icon_url: None,
                })
            }),
            thumbnail: thumbnail.and_then(|media| self.map_embed_media(media, options)),
            image: image.and_then(|media| self.map_embed_media(media, options)),
            video: video.and_then(|media| self.map_embed_media(media, options)),
            audio: audio.and_then(|media| self.map_embed_media(media, options)),
            footer: footer.and_then(|footer| {
                footer.text.map(|text| ApiEmbedFooterResponse {
                    text,
                    proxy_icon_url: footer
                        .icon_url
                        .as_ref()
                        .map(|url| external_media_proxy_url(url, options)),
                    icon_url: footer.icon_url.map(|url| sign_own_url(&url, options)),
                })
            }),
            fields: fields.map(|fields| fields.into_iter().map(map_embed_field_response).collect()),
            nsfw,
            html,
            html_width,
            html_height,
        }
    }

    fn map_embed_media(
        &self,
        media: MessageEmbedMedia,
        options: &ResponseBuildOptions,
    ) -> Option<ApiEmbedMediaResponse> {
        let url = media.url?;
        Some(ApiEmbedMediaResponse {
            proxy_url: external_media_proxy_url(&url, options),
            url: sign_own_url(&url, options),
            width: media.width,
            height: media.height,
            duration: media.duration,
            description: media.description,
            content_type: media.content_type,
            content_hash: media.content_hash,
            placeholder: media.placeholder,
            flags: media
                .flags
                .map(|flags| flags & !EMBED_MEDIA_OWNED_ATTACHMENT_FLAG),
        })
    }

    fn map_snapshot(
        &self,
        message_id: i64,
        channel_id: i64,
        snapshot_index: usize,
        snapshot: &MessageSnapshot,
        options: &ResponseBuildOptions,
        context: &ResponseContext,
    ) -> ApiMessageSnapshotResponse {
        let snapshot_mentions = context
            .mention_context
            .get(&message_id)
            .and_then(|mentions| mentions.snapshots.get(snapshot_index))
            .cloned()
            .unwrap_or_else(|| extract_mentions_from_markdown(snapshot.content.as_deref()));
        let mention_channels =
            ids_present_in_set(&snapshot.mention_channels, &snapshot_mentions.channels)
                .into_iter()
                .filter_map(|id| context.channel_mentions.get(&id).cloned())
                .collect::<Vec<_>>();
        let embeds =
            if (snapshot.flags.unwrap_or_default() & MESSAGE_FLAG_SUPPRESS_EMBEDS as i32) == 0 {
                snapshot.embeds.as_deref().map(|embeds| {
                    embeds
                        .iter()
                        .map(|embed| self.map_embed(embed, options))
                        .collect()
                })
            } else {
                None
            };
        let attachments = snapshot.attachments.as_ref().map(|attachments| {
            attachments
                .iter()
                .filter_map(|attachment| {
                    self.map_attachment(channel_id, attachment, options, context)
                })
                .collect()
        });
        ApiMessageSnapshotResponse {
            content: snapshot.content.clone(),
            timestamp: snapshot
                .timestamp
                .map(epoch_millis_to_iso)
                .unwrap_or_else(|| epoch_millis_to_iso(0)),
            edited_timestamp: snapshot.edited_timestamp.map(epoch_millis_to_iso),
            mentions: (!snapshot.mention_users.is_empty()).then(|| {
                snapshot
                    .mention_users
                    .iter()
                    .map(ToString::to_string)
                    .collect()
            }),
            mention_roles: (!snapshot.mention_roles.is_empty()).then(|| {
                snapshot
                    .mention_roles
                    .iter()
                    .map(ToString::to_string)
                    .collect()
            }),
            mention_channels: (!mention_channels.is_empty()).then_some(mention_channels),
            embeds,
            attachments,
            stickers: snapshot
                .sticker_items
                .as_ref()
                .map(|stickers| stickers.iter().filter_map(map_sticker).collect::<Vec<_>>()),
            snapshot_type: snapshot.snapshot_type.unwrap_or_default(),
            flags: options.mask_flags(i64::from(snapshot.flags.unwrap_or_default())) as i32,
        }
    }
}

impl MessagesStorage {
    async fn get_by_id(
        &self,
        channel_id: i64,
        bucket: i32,
        message_id: i64,
    ) -> anyhow::Result<Option<Message>> {
        match self {
            MessagesStorage::Postgres(storage) => {
                storage.get_by_id(channel_id, bucket, message_id).await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage.get_by_id(channel_id, bucket, message_id).await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) => Ok(None),
            #[cfg(test)]
            MessagesStorage::Memory(messages) => Ok(messages
                .iter()
                .find(|message| {
                    message.channel_id == channel_id && message.message_id == message_id
                })
                .cloned()),
        }
    }

    async fn list_buckets_desc(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
    ) -> anyhow::Result<Vec<i32>> {
        match self {
            MessagesStorage::Postgres(storage) => {
                storage
                    .list_buckets(channel_id, min_bucket, max_bucket, limit, true)
                    .await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage
                    .list_buckets_desc(channel_id, min_bucket, max_bucket, limit)
                    .await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) => Ok(Vec::new()),
            #[cfg(test)]
            MessagesStorage::Memory(messages) => Ok(memory_buckets(
                messages, channel_id, min_bucket, max_bucket, limit, true,
            )),
        }
    }

    async fn list_buckets_asc(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
    ) -> anyhow::Result<Vec<i32>> {
        match self {
            MessagesStorage::Postgres(storage) => {
                storage
                    .list_buckets(channel_id, min_bucket, max_bucket, limit, false)
                    .await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage
                    .list_buckets_asc(channel_id, min_bucket, max_bucket, limit)
                    .await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) => Ok(Vec::new()),
            #[cfg(test)]
            MessagesStorage::Memory(messages) => Ok(memory_buckets(
                messages, channel_id, min_bucket, max_bucket, limit, false,
            )),
        }
    }

    async fn fetch_latest_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        match self {
            MessagesStorage::Postgres(storage) => {
                storage
                    .fetch_bucket(channel_id, bucket, None, true, limit)
                    .await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage.fetch_latest_bucket(channel_id, bucket, limit).await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) => Ok(Vec::new()),
            #[cfg(test)]
            MessagesStorage::Memory(messages) => {
                Ok(memory_bucket(messages, channel_id, bucket, None, limit))
            }
        }
    }

    async fn fetch_before_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        before_id: i64,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        match self {
            MessagesStorage::Postgres(storage) => {
                storage
                    .fetch_bucket(
                        channel_id,
                        bucket,
                        Some(BucketBound::Before(before_id)),
                        true,
                        limit,
                    )
                    .await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage
                    .fetch_before_bucket(channel_id, bucket, before_id, limit)
                    .await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) => Ok(Vec::new()),
            #[cfg(test)]
            MessagesStorage::Memory(messages) => Ok(memory_bucket(
                messages,
                channel_id,
                bucket,
                Some(BucketBound::Before(before_id)),
                limit,
            )),
        }
    }

    async fn fetch_after_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        after_id: i64,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        match self {
            MessagesStorage::Postgres(storage) => {
                storage
                    .fetch_bucket(
                        channel_id,
                        bucket,
                        Some(BucketBound::After(after_id)),
                        false,
                        limit,
                    )
                    .await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage
                    .fetch_after_bucket(channel_id, bucket, after_id, limit)
                    .await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) => Ok(Vec::new()),
            #[cfg(test)]
            MessagesStorage::Memory(messages) => Ok(memory_bucket(
                messages,
                channel_id,
                bucket,
                Some(BucketBound::After(after_id)),
                limit,
            )),
        }
    }

    async fn delete_message(
        &self,
        channel_id: i64,
        bucket: i32,
        message_id: i64,
    ) -> anyhow::Result<()> {
        match self {
            MessagesStorage::Postgres(storage) => {
                storage.delete_message(channel_id, bucket, message_id).await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage.delete_message(channel_id, bucket, message_id).await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(deleted) => {
                deleted
                    .lock()
                    .expect("deleted message keys mutex poisoned")
                    .push((channel_id, bucket, message_id));
                Ok(())
            }
            #[cfg(test)]
            MessagesStorage::Memory(_) => Ok(()),
        }
    }

    async fn fetch_reactions_for_message_batch(
        &self,
        channel_id: i64,
        bucket: i32,
        message_ids: Vec<i64>,
        viewer_user_id: i64,
    ) -> HashMap<i64, Vec<ApiMessageReactionResponse>> {
        match self {
            MessagesStorage::Postgres(storage) => {
                storage
                    .fetch_reactions_for_message_batch(
                        channel_id,
                        bucket,
                        message_ids,
                        viewer_user_id,
                    )
                    .await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage
                    .fetch_reactions_for_message_batch(
                        channel_id,
                        bucket,
                        message_ids,
                        viewer_user_id,
                    )
                    .await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) | MessagesStorage::Memory(_) => HashMap::new(),
        }
    }

    async fn fetch_attachment_decay_batch(
        &self,
        attachment_ids: Vec<i64>,
    ) -> anyhow::Result<HashMap<i64, DateTime<Utc>>> {
        match self {
            MessagesStorage::Postgres(storage) => {
                storage.fetch_attachment_decay_batch(attachment_ids).await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage.fetch_attachment_decay_batch(attachment_ids).await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) | MessagesStorage::Memory(_) => Ok(HashMap::new()),
        }
    }
}

#[derive(Clone, Copy)]
enum BucketBound {
    Before(i64),
    After(i64),
}

impl PostgresMessagesStorage {
    async fn get_by_id(
        &self,
        channel_id: i64,
        bucket: i32,
        message_id: i64,
    ) -> anyhow::Result<Option<Message>> {
        let key = message_row_key(channel_id, bucket, message_id)?;
        let Some(row) = self.kv.get_row("messages", &key).await? else {
            return Ok(None);
        };
        decode_postgres_message(row).map(Some)
    }

    async fn list_buckets(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
        desc: bool,
    ) -> anyhow::Result<Vec<i32>> {
        if min_bucket > max_bucket || limit == 0 {
            return Ok(Vec::new());
        }
        let partition_key = postgres::kv_key(&[KeyPart::BigInt(channel_id)])?;
        let mut buckets = self
            .kv
            .get_partition_rows("channel_message_buckets", &partition_key)
            .await?
            .into_iter()
            .map(|(_, row)| decode_postgres_bucket(row))
            .collect::<anyhow::Result<Vec<_>>>()?;
        buckets.retain(|bucket| *bucket >= min_bucket && *bucket <= max_bucket);
        if desc {
            buckets.sort_unstable_by(|left, right| right.cmp(left));
        } else {
            buckets.sort_unstable();
        }
        buckets.truncate(limit.min(i32::MAX as u32) as usize);
        Ok(buckets)
    }

    async fn fetch_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        bound: Option<BucketBound>,
        desc: bool,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        if limit <= 0 {
            return Ok(Vec::new());
        }
        let partition_key =
            postgres::kv_key(&[KeyPart::BigInt(channel_id), KeyPart::Number(bucket as i64)])?;
        let bound = match bound {
            Some(BucketBound::Before(message_id)) => Some(BigIntBound::LessThan(message_id)),
            Some(BucketBound::After(message_id)) => Some(BigIntBound::GreaterThan(message_id)),
            None => None,
        };
        self.kv
            .get_partition_rows_by_bigint_field(
                "messages",
                &partition_key,
                "message_id",
                bound,
                desc,
                limit as i64,
            )
            .await?
            .into_iter()
            .map(|(_, row)| decode_postgres_message(row))
            .collect::<anyhow::Result<Vec<_>>>()
    }

    async fn delete_message(
        &self,
        channel_id: i64,
        bucket: i32,
        message_id: i64,
    ) -> anyhow::Result<()> {
        let key = message_row_key(channel_id, bucket, message_id)?;
        self.kv.delete_row("messages", &key).await
    }

    async fn fetch_reactions_for_message_batch(
        &self,
        channel_id: i64,
        bucket: i32,
        message_ids: Vec<i64>,
        viewer_user_id: i64,
    ) -> HashMap<i64, Vec<ApiMessageReactionResponse>> {
        let wanted = message_ids.into_iter().collect::<HashSet<_>>();
        if wanted.is_empty() {
            return HashMap::new();
        }
        let partition_key = match postgres::kv_key(&[
            KeyPart::BigInt(channel_id),
            KeyPart::Number(bucket as i64),
        ]) {
            Ok(key) => key,
            Err(_) => return HashMap::new(),
        };
        let wanted_values = wanted.iter().copied().collect::<Vec<_>>();
        let rows = match self
            .kv
            .get_partition_rows_by_bigint_field_values(
                "message_reactions",
                &partition_key,
                "message_id",
                &wanted_values,
            )
            .await
        {
            Ok(rows) => rows,
            Err(_) => return HashMap::new(),
        };
        let mut by_message: HashMap<i64, Vec<MessageReactionDbRow>> = HashMap::new();
        for (_, row) in rows {
            let Ok((message_id, reaction)) = decode_postgres_reaction(row) else {
                continue;
            };
            if wanted.contains(&message_id) {
                by_message.entry(message_id).or_default().push(reaction);
            }
        }
        by_message
            .into_iter()
            .filter_map(|(message_id, reactions)| {
                let reactions = map_reactions(reactions, viewer_user_id);
                (!reactions.is_empty()).then_some((message_id, reactions))
            })
            .collect()
    }

    async fn fetch_attachment_decay_batch(
        &self,
        attachment_ids: Vec<i64>,
    ) -> anyhow::Result<HashMap<i64, DateTime<Utc>>> {
        if attachment_ids.is_empty() {
            return Ok(HashMap::new());
        }
        let keys = attachment_ids
            .iter()
            .map(|attachment_id| postgres::kv_key(&[KeyPart::BigInt(*attachment_id)]))
            .collect::<anyhow::Result<Vec<_>>>()?;
        let rows = self.kv.get_rows("attachment_decay_by_id", &keys).await?;
        rows.into_iter()
            .map(|(_, row)| decode_postgres_attachment_decay(row))
            .collect::<anyhow::Result<HashMap<_, _>>>()
    }
}

#[cfg(feature = "scylla")]
impl ScyllaMessagesStorage {
    async fn get_by_id(
        &self,
        channel_id: i64,
        bucket: i32,
        message_id: i64,
    ) -> anyhow::Result<Option<Message>> {
        let result = self
            .db
            .execute_unpaged(&self.stmt_get_by_id, (channel_id, bucket, message_id))
            .await?;
        let rows = result.into_rows_result()?;
        Ok(rows.maybe_first_row::<MessageDbRow>()?.map(Into::into))
    }

    async fn list_buckets_desc(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
    ) -> anyhow::Result<Vec<i32>> {
        let result = self
            .db
            .execute_unpaged(
                &self.stmt_list_buckets_desc,
                (
                    channel_id,
                    min_bucket,
                    max_bucket,
                    limit.min(i32::MAX as u32) as i32,
                ),
            )
            .await?;
        bucket_rows_to_vec(result.into_rows_result()?)
    }

    async fn list_buckets_asc(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
    ) -> anyhow::Result<Vec<i32>> {
        let result = self
            .db
            .execute_unpaged(
                &self.stmt_list_buckets_asc,
                (
                    channel_id,
                    min_bucket,
                    max_bucket,
                    limit.min(i32::MAX as u32) as i32,
                ),
            )
            .await?;
        bucket_rows_to_vec(result.into_rows_result()?)
    }

    async fn fetch_latest_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        let result = self
            .db
            .execute_unpaged(&self.stmt_get_latest, (channel_id, bucket, limit))
            .await?;
        rows_to_messages(result.into_rows_result()?)
    }

    async fn fetch_before_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        before_id: i64,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        let result = self
            .db
            .execute_unpaged(
                &self.stmt_get_before,
                (channel_id, bucket, before_id, limit),
            )
            .await?;
        rows_to_messages(result.into_rows_result()?)
    }

    async fn fetch_after_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        after_id: i64,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        let result = self
            .db
            .execute_unpaged(&self.stmt_get_after, (channel_id, bucket, after_id, limit))
            .await?;
        rows_to_messages(result.into_rows_result()?)
    }

    async fn delete_message(
        &self,
        channel_id: i64,
        bucket: i32,
        message_id: i64,
    ) -> anyhow::Result<()> {
        self.db
            .execute_unpaged(&self.stmt_delete_message, (channel_id, bucket, message_id))
            .await?;
        Ok(())
    }

    async fn fetch_reactions_for_message_batch(
        &self,
        channel_id: i64,
        bucket: i32,
        message_ids: Vec<i64>,
        viewer_user_id: i64,
    ) -> HashMap<i64, Vec<ApiMessageReactionResponse>> {
        let result = self
            .db
            .execute_unpaged(
                &self.stmt_get_reactions_for_messages,
                (channel_id, bucket, message_ids.clone()),
            )
            .await;
        let Ok(result) = result else {
            return self
                .fetch_reactions_for_messages_individually(
                    channel_id,
                    bucket,
                    message_ids,
                    viewer_user_id,
                )
                .await;
        };
        let Ok(rows) = result.into_rows_result() else {
            return self
                .fetch_reactions_for_messages_individually(
                    channel_id,
                    bucket,
                    message_ids,
                    viewer_user_id,
                )
                .await;
        };
        let Ok(rows) = rows.rows::<BatchedMessageReactionDbRow>() else {
            return HashMap::new();
        };
        let mut by_message: HashMap<i64, Vec<MessageReactionDbRow>> = HashMap::new();
        for row in rows.filter_map(Result::ok) {
            by_message
                .entry(row.message_id)
                .or_default()
                .push(MessageReactionDbRow {
                    user_id: row.user_id,
                    emoji_id: row.emoji_id,
                    emoji_name: row.emoji_name,
                    emoji_animated: row.emoji_animated,
                    created_at: row.created_at,
                });
        }
        by_message
            .into_iter()
            .filter_map(|(message_id, reactions)| {
                let reactions = map_reactions(reactions, viewer_user_id);
                (!reactions.is_empty()).then_some((message_id, reactions))
            })
            .collect()
    }

    async fn fetch_reactions_for_messages_individually(
        &self,
        channel_id: i64,
        bucket: i32,
        message_ids: Vec<i64>,
        viewer_user_id: i64,
    ) -> HashMap<i64, Vec<ApiMessageReactionResponse>> {
        stream::iter(message_ids)
            .map(|message_id| async move {
                let result = self
                    .db
                    .execute_unpaged(&self.stmt_get_reactions, (channel_id, bucket, message_id))
                    .await
                    .ok()?;
                let rows = result.into_rows_result().ok()?;
                let reactions: Vec<MessageReactionDbRow> = rows
                    .rows::<MessageReactionDbRow>()
                    .ok()?
                    .filter_map(Result::ok)
                    .collect();
                let reactions = map_reactions(reactions, viewer_user_id);
                (!reactions.is_empty()).then_some((message_id, reactions))
            })
            .buffer_unordered(ENRICHMENT_QUERY_CONCURRENCY)
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .collect()
    }

    async fn fetch_attachment_decay_batch(
        &self,
        attachment_ids: Vec<i64>,
    ) -> anyhow::Result<HashMap<i64, DateTime<Utc>>> {
        let result = self
            .db
            .execute_unpaged(
                &self.stmt_get_attachment_decay_many,
                (attachment_ids.clone(),),
            )
            .await;
        let Ok(result) = result else {
            return self
                .fetch_attachment_decay_individually(attachment_ids)
                .await;
        };
        let Ok(rows) = result.into_rows_result() else {
            return self
                .fetch_attachment_decay_individually(attachment_ids)
                .await;
        };
        let rows = rows.rows::<AttachmentDecayDbRow>()?;
        rows.map(|row| {
            row.map(|row| (row.attachment_id, row.expires_at))
                .map_err(Into::into)
        })
        .collect::<anyhow::Result<HashMap<_, _>>>()
    }

    async fn fetch_attachment_decay_individually(
        &self,
        attachment_ids: Vec<i64>,
    ) -> anyhow::Result<HashMap<i64, DateTime<Utc>>> {
        let rows = stream::iter(attachment_ids)
            .map(|attachment_id| async move {
                let result = self
                    .db
                    .execute_unpaged(&self.stmt_get_attachment_decay, (attachment_id,))
                    .await?;
                let rows = result.into_rows_result()?;
                let row = rows.maybe_first_row::<AttachmentDecayDbRow>()?;
                Ok::<_, anyhow::Error>(row.map(|row| (row.attachment_id, row.expires_at)))
            })
            .buffer_unordered(ENRICHMENT_QUERY_CONCURRENCY)
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .collect::<anyhow::Result<Vec<_>>>()?;
        Ok(rows.into_iter().flatten().collect())
    }
}

impl<T: Transport> ShardService for MessagesShard<T> {
    type Request = MessageRequest;
    type Response = MessageResponse;

    fn service_name(&self) -> &str {
        "messages"
    }

    async fn handle(&self, request: MessageRequest) -> anyhow::Result<MessageResponse> {
        match request {
            MessageRequest::GetById {
                channel_id,
                message_id,
            } => match self.get_by_id(channel_id, message_id).await? {
                Some(msg) => Ok(MessageResponse::Found(msg)),
                None => Ok(MessageResponse::NotFound),
            },
            MessageRequest::GetLatest { channel_id, limit } => {
                let messages = self.get_latest(channel_id, limit).await?;
                Ok(MessageResponse::FoundMany(messages))
            }
            MessageRequest::GetBefore {
                channel_id,
                before_id,
                limit,
            } => {
                let messages = self.get_before(channel_id, before_id, limit).await?;
                Ok(MessageResponse::FoundMany(messages))
            }
            MessageRequest::GetAfter {
                channel_id,
                after_id,
                limit,
            } => {
                let messages = self.get_after(channel_id, after_id, limit).await?;
                Ok(MessageResponse::FoundMany(messages))
            }
            MessageRequest::GetResponseById {
                channel_id,
                message_id,
                viewer_user_id,
                source_guild_id,
                message_history_cutoff_ms,
                can_read_message_history,
                media_endpoint,
                media_proxy_secret_key,
                attachment_url_secret_base64,
                include_reactions,
                nonce,
                tts,
                include_hidden,
                threads_mask,
            } => {
                let channel_id = parse_i64(&channel_id, "channel_id")?;
                let message_id = parse_i64(&message_id, "message_id")?;
                let viewer_user_id = parse_i64(&viewer_user_id, "viewer_user_id")?;
                let source_guild_id = source_guild_id
                    .as_deref()
                    .map(|id| parse_i64(id, "source_guild_id"))
                    .transpose()?;
                let response = self
                    .get_api_response(
                        channel_id,
                        message_id,
                        ResponseBuildOptions {
                            viewer_user_id,
                            source_guild_id,
                            message_history_cutoff_ms,
                            can_read_message_history,
                            media_endpoint,
                            media_proxy_secret_key,
                            attachment_url_secret: decode_attachment_url_secret(
                                attachment_url_secret_base64.as_deref(),
                            ),
                            include_reactions: include_reactions.unwrap_or(true),
                            nonce,
                            tts: tts.unwrap_or(false),
                            include_hidden,
                            threads_mask,
                            excluded_types: Vec::new(),
                        },
                    )
                    .await?;
                match response {
                    Some(response) => Ok(MessageResponse::FoundApi(response)),
                    None => Ok(MessageResponse::NotFound),
                }
            }
            MessageRequest::BuildResponse {
                message,
                viewer_user_id,
                source_guild_id,
                message_history_cutoff_ms,
                can_read_message_history,
                media_endpoint,
                media_proxy_secret_key,
                attachment_url_secret_base64,
                include_reactions,
                nonce,
                tts,
                include_hidden,
                threads_mask,
            } => {
                let viewer_user_id = parse_i64(&viewer_user_id, "viewer_user_id")?;
                let source_guild_id = source_guild_id
                    .as_deref()
                    .map(|id| parse_i64(id, "source_guild_id"))
                    .transpose()?;
                let response = self
                    .build_api_response_from_message(
                        message,
                        ResponseBuildOptions {
                            viewer_user_id,
                            source_guild_id,
                            message_history_cutoff_ms,
                            can_read_message_history,
                            media_endpoint,
                            media_proxy_secret_key,
                            attachment_url_secret: decode_attachment_url_secret(
                                attachment_url_secret_base64.as_deref(),
                            ),
                            include_reactions: include_reactions.unwrap_or(true),
                            nonce,
                            tts: tts.unwrap_or(false),
                            include_hidden,
                            threads_mask,
                            excluded_types: Vec::new(),
                        },
                    )
                    .await?;
                match response {
                    Some(response) => Ok(MessageResponse::FoundApi(response)),
                    None => Ok(MessageResponse::NotFound),
                }
            }
            MessageRequest::BuildResponses {
                messages,
                viewer_user_id,
                source_guild_id,
                message_history_cutoff_ms,
                can_read_message_history,
                media_endpoint,
                media_proxy_secret_key,
                attachment_url_secret_base64,
                include_reactions,
                include_hidden,
                threads_mask,
            } => {
                let viewer_user_id = parse_i64(&viewer_user_id, "viewer_user_id")?;
                let source_guild_id = source_guild_id
                    .as_deref()
                    .map(|id| parse_i64(id, "source_guild_id"))
                    .transpose()?;
                let responses = self
                    .build_api_responses_from_messages(
                        messages,
                        ResponseBuildOptions {
                            viewer_user_id,
                            source_guild_id,
                            message_history_cutoff_ms,
                            can_read_message_history,
                            media_endpoint,
                            media_proxy_secret_key,
                            attachment_url_secret: decode_attachment_url_secret(
                                attachment_url_secret_base64.as_deref(),
                            ),
                            include_reactions: include_reactions.unwrap_or(true),
                            nonce: None,
                            tts: false,
                            include_hidden,
                            threads_mask,
                            excluded_types: Vec::new(),
                        },
                    )
                    .await?;
                Ok(MessageResponse::FoundApiMany(responses))
            }
            MessageRequest::ListResponses {
                channel_id,
                viewer_user_id,
                limit,
                before_id,
                after_id,
                around_id,
                source_guild_id,
                message_history_cutoff_ms,
                can_read_message_history,
                media_endpoint,
                media_proxy_secret_key,
                attachment_url_secret_base64,
                include_reactions,
                include_hidden,
                threads_mask,
                mut exclude_types,
            } => {
                let channel_id = parse_i64(&channel_id, "channel_id")?;
                if threads_mask && !exclude_types.contains(&MESSAGE_TYPE_THREAD_CREATED) {
                    exclude_types.push(MESSAGE_TYPE_THREAD_CREATED);
                }
                let viewer_user_id = parse_i64(&viewer_user_id, "viewer_user_id")?;
                let before_id = before_id
                    .as_deref()
                    .map(|id| parse_i64(id, "before_id"))
                    .transpose()?;
                let after_id = after_id
                    .as_deref()
                    .map(|id| parse_i64(id, "after_id"))
                    .transpose()?;
                let around_id = around_id
                    .as_deref()
                    .map(|id| parse_i64(id, "around_id"))
                    .transpose()?;
                let source_guild_id = source_guild_id
                    .as_deref()
                    .map(|id| parse_i64(id, "source_guild_id"))
                    .transpose()?;
                let responses = self
                    .list_api_responses(
                        channel_id,
                        limit,
                        before_id,
                        after_id,
                        around_id,
                        ResponseBuildOptions {
                            viewer_user_id,
                            source_guild_id,
                            message_history_cutoff_ms,
                            can_read_message_history,
                            media_endpoint,
                            media_proxy_secret_key,
                            attachment_url_secret: decode_attachment_url_secret(
                                attachment_url_secret_base64.as_deref(),
                            ),
                            include_reactions: include_reactions.unwrap_or(true),
                            nonce: None,
                            tts: false,
                            include_hidden,
                            threads_mask,
                            excluded_types: exclude_types,
                        },
                    )
                    .await?;
                Ok(MessageResponse::FoundApiMany(responses))
            }
            MessageRequest::ExtractMentions { contents } => {
                let mentions = contents
                    .iter()
                    .map(|content| {
                        crate::types::ExtractedMentionsResponse::from(
                            extract_mentions_from_markdown(Some(content)),
                        )
                    })
                    .collect();
                Ok(MessageResponse::FoundMentions(mentions))
            }
        }
    }
}

fn now_epoch_millis() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64
}

fn now_epoch_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

fn message_row_key(channel_id: i64, bucket: i32, message_id: i64) -> anyhow::Result<String> {
    postgres::kv_key(&[
        KeyPart::BigInt(channel_id),
        KeyPart::Number(bucket as i64),
        KeyPart::BigInt(message_id),
    ])
}

fn decode_postgres_bucket(row: serde_json::Value) -> anyhow::Result<i32> {
    let row = postgres::decode_row_dates_as_millis(row)?;
    let row: ChannelMessageBucketKvRow = serde_json::from_value(row)?;
    Ok(row.bucket)
}

fn decode_postgres_message(row: serde_json::Value) -> anyhow::Result<Message> {
    let row = postgres::decode_row_dates_as_millis(row)?;
    let serde_json::Value::Object(mut row) = row else {
        anyhow::bail!("Postgres message row payload is not an object");
    };
    let pinned = row
        .get("pinned_timestamp")
        .is_some_and(|value| !value.is_null());
    row.insert("pinned".to_owned(), serde_json::Value::Bool(pinned));
    Ok(serde_json::from_value(serde_json::Value::Object(row))?)
}

fn decode_postgres_reaction(row: serde_json::Value) -> anyhow::Result<(i64, MessageReactionDbRow)> {
    let row = postgres::decode_row_dates_as_millis(row)?;
    let row: MessageReactionKvRow = serde_json::from_value(row)?;
    let created_at = row
        .created_at
        .and_then(DateTime::<Utc>::from_timestamp_millis);
    Ok((
        row.message_id,
        MessageReactionDbRow {
            user_id: row.user_id,
            emoji_id: row.emoji_id,
            emoji_name: row.emoji_name,
            emoji_animated: row.emoji_animated,
            created_at,
        },
    ))
}

fn decode_postgres_attachment_decay(
    row: serde_json::Value,
) -> anyhow::Result<(i64, DateTime<Utc>)> {
    let row = postgres::decode_row_dates_as_millis(row)?;
    let row: AttachmentDecayKvRow = serde_json::from_value(row)?;
    let expires_at = DateTime::<Utc>::from_timestamp_millis(row.expires_at)
        .ok_or_else(|| anyhow::anyhow!("invalid attachment decay timestamp"))?;
    Ok((row.attachment_id, expires_at))
}

#[cfg(feature = "scylla")]
fn rows_to_messages(rows: QueryRowsResult) -> anyhow::Result<Vec<Message>> {
    let rows: Vec<MessageDbRow> = rows.rows::<MessageDbRow>()?.collect::<Result<_, _>>()?;
    Ok(rows.into_iter().map(Message::from).collect())
}

#[cfg(feature = "scylla")]
fn bucket_rows_to_vec(rows: QueryRowsResult) -> anyhow::Result<Vec<i32>> {
    let rows: Vec<ChannelMessageBucketDbRow> = rows
        .rows::<ChannelMessageBucketDbRow>()?
        .collect::<Result<_, _>>()?;
    Ok(rows.into_iter().map(|row| row.bucket).collect())
}

fn bucket_page_limit(message_limit: u32) -> u32 {
    message_limit.clamp(32, BUCKET_INDEX_PAGE_SIZE)
}

fn next_bucket_wave(wave: usize) -> usize {
    wave.saturating_mul(2).min(BUCKET_SCAN_CONCURRENCY)
}

async fn collect_bucket_waves<T, Fetch, Fut>(
    buckets: &[i32],
    limit: usize,
    collected: &mut Vec<T>,
    fetch: Fetch,
) -> anyhow::Result<()>
where
    Fetch: Fn(i32) -> Fut,
    Fut: std::future::Future<Output = anyhow::Result<Vec<T>>>,
{
    let mut offset = 0;
    let mut wave = BUCKET_SCAN_WAVE;
    while offset < buckets.len() && collected.len() < limit {
        let end = buckets.len().min(offset + wave);
        let results = stream::iter(buckets[offset..end].iter().copied())
            .map(&fetch)
            .buffer_unordered(wave)
            .collect::<Vec<_>>()
            .await;
        for result in results {
            collected.extend(result?);
        }
        offset = end;
        wave = next_bucket_wave(wave);
    }
    Ok(())
}

async fn collect_excluding<Fetch, Fut>(
    limit: u32,
    excluded_types: &[i32],
    mut cursor: Option<i64>,
    fetch: Fetch,
) -> anyhow::Result<Vec<Message>>
where
    Fetch: Fn(Option<i64>, u32) -> Fut,
    Fut: std::future::Future<Output = anyhow::Result<Vec<Message>>>,
{
    let mut collected = Vec::new();
    let mut scanned = 0_usize;
    let mut page_size = limit;
    while collected.len() < limit as usize && scanned < EXCLUDED_TYPE_SCAN_ROW_CAP {
        let page = fetch(cursor, page_size).await?;
        let exhausted = page.len() < page_size as usize;
        let Some(last) = page.last() else {
            break;
        };
        cursor = Some(last.message_id);
        scanned += page.len();
        collected.extend(
            page.into_iter()
                .filter(|message| !excluded_types.contains(&message.message_type)),
        );
        if exhausted {
            break;
        }
        page_size = limit.max(EXCLUDED_TYPE_REFILL_PAGE_SIZE);
    }
    collected.truncate(limit as usize);
    Ok(collected)
}

fn around_window_limits(limit: u32) -> (u32, u32) {
    let newer_limit = limit / 2;
    let older_limit = limit.saturating_sub(1).saturating_sub(newer_limit);
    (newer_limit, older_limit)
}

fn parse_i64(value: &str, field_name: &str) -> anyhow::Result<i64> {
    value
        .parse::<i64>()
        .map_err(|error| anyhow::anyhow!("invalid {field_name}: {error}"))
}

fn snowflake_to_epoch_millis(snowflake: i64) -> i64 {
    (snowflake >> 22) + FLUXER_EPOCH_MS
}

fn epoch_millis_to_bucket(epoch_millis: i64) -> i32 {
    ((epoch_millis - FLUXER_EPOCH_MS) / BUCKET_DURATION_MS) as i32
}

fn snowflake_to_bucket(snowflake: i64) -> i32 {
    ((snowflake >> 22) / BUCKET_DURATION_MS) as i32
}

fn current_bucket() -> i32 {
    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64;
    epoch_millis_to_bucket(now_ms)
}

fn epoch_millis_to_iso(epoch_millis: i64) -> String {
    DateTime::<Utc>::from_timestamp_millis(epoch_millis)
        .unwrap_or(DateTime::<Utc>::UNIX_EPOCH)
        .to_rfc3339_opts(chrono::SecondsFormat::Millis, true)
}

fn map_embed_field_response(field: MessageEmbedField) -> ApiEmbedFieldResponse {
    ApiEmbedFieldResponse {
        name: field.name.unwrap_or_default(),
        value: field.value.unwrap_or_default(),
        is_inline: field.is_inline.unwrap_or(false),
    }
}

fn build_message_mention_context(messages: &[&Message]) -> HashMap<i64, MessageMentionContext> {
    messages
        .iter()
        .map(|message| (message.message_id, build_mention_context_entry(message)))
        .collect()
}

fn build_mention_context_entry(message: &Message) -> MessageMentionContext {
    let message_snapshots = message.message_snapshots.as_deref().unwrap_or_default();
    let snapshots = message_snapshots
        .iter()
        .map(|snapshot| extract_mentions_from_markdown(snapshot.content.as_deref()))
        .collect();
    let mut embed_users = HashSet::new();
    for embed in message.embeds.as_deref().unwrap_or_default() {
        collect_user_ids_from_embed(embed, &mut embed_users);
    }
    for snapshot in message_snapshots {
        for embed in snapshot.embeds.as_deref().unwrap_or_default() {
            collect_user_ids_from_embed(embed, &mut embed_users);
        }
    }
    MessageMentionContext {
        content: extract_mentions_from_markdown(message.content.as_deref()),
        snapshots,
        embed_users,
    }
}

fn ids_present_in_set(ids: &[i64], present: &HashSet<i64>) -> Vec<String> {
    ids.iter()
        .filter(|id| present.contains(id))
        .map(ToString::to_string)
        .collect()
}

fn collect_attachment_ids(messages: &[&Message]) -> HashSet<i64> {
    let mut ids = HashSet::new();
    for message in messages {
        if let Some(attachments) = &message.attachments {
            for attachment in attachments {
                if let Some(id) = attachment.attachment_id {
                    ids.insert(id);
                }
            }
        }
        if let Some(snapshots) = &message.message_snapshots {
            for snapshot in snapshots {
                if let Some(attachments) = &snapshot.attachments {
                    for attachment in attachments {
                        if let Some(id) = attachment.attachment_id {
                            ids.insert(id);
                        }
                    }
                }
            }
        }
    }
    ids
}

fn collect_channel_mention_ids(
    messages: &[&Message],
    mention_context: &HashMap<i64, MessageMentionContext>,
) -> HashSet<i64> {
    let mut ids = HashSet::new();
    for message in messages {
        let content_mentions = mention_context
            .get(&message.message_id)
            .map(|mentions| &mentions.content);
        for id in &message.mention_channels {
            if content_mentions.is_some_and(|mentions| mentions.channels.contains(id)) {
                ids.insert(*id);
            }
        }
        if let Some(snapshots) = &message.message_snapshots {
            for (index, snapshot) in snapshots.iter().enumerate() {
                let snapshot_mentions = mention_context
                    .get(&message.message_id)
                    .and_then(|mentions| mentions.snapshots.get(index));
                for id in &snapshot.mention_channels {
                    if snapshot_mentions.is_some_and(|mentions| mentions.channels.contains(id)) {
                        ids.insert(*id);
                    }
                }
            }
        }
    }
    ids
}

fn collect_user_ids(
    messages: &[&Message],
    mention_context: &HashMap<i64, MessageMentionContext>,
) -> HashSet<i64> {
    let mut ids = HashSet::new();
    for message in messages {
        if let Some(author_id) = message.author_id {
            ids.insert(author_id);
        }
        for id in &message.mention_users {
            ids.insert(*id);
        }
        if let Some(mentions) = mention_context.get(&message.message_id) {
            ids.extend(mentions.content.users.iter().copied());
            ids.extend(mentions.embed_users.iter().copied());
        }
        if let Some(snapshots) = &message.message_snapshots {
            for (index, snapshot) in snapshots.iter().enumerate() {
                for id in &snapshot.mention_users {
                    ids.insert(*id);
                }
                if let Some(mentions) = mention_context
                    .get(&message.message_id)
                    .and_then(|mentions| mentions.snapshots.get(index))
                {
                    ids.extend(mentions.users.iter().copied());
                }
            }
        }
    }
    ids
}

fn collect_user_ids_from_embed(embed: &MessageEmbed, target: &mut HashSet<i64>) {
    let mut mentions = MessageMentions::default();
    extend_mentions_from_markdown(embed.title.as_deref(), &mut mentions);
    extend_mentions_from_markdown(embed.description.as_deref(), &mut mentions);
    if let Some(author) = &embed.author {
        extend_mentions_from_markdown(author.name.as_deref(), &mut mentions);
    }
    if let Some(footer) = &embed.footer {
        extend_mentions_from_markdown(footer.text.as_deref(), &mut mentions);
    }
    for media in [&embed.image, &embed.thumbnail, &embed.video, &embed.audio]
        .into_iter()
        .flatten()
    {
        extend_mentions_from_markdown(media.description.as_deref(), &mut mentions);
    }
    if let Some(fields) = &embed.fields {
        for field in fields {
            extend_mentions_from_markdown(field.name.as_deref(), &mut mentions);
            extend_mentions_from_markdown(field.value.as_deref(), &mut mentions);
        }
    }
    target.extend(mentions.users);
    if let Some(children) = &embed.children {
        for child in children {
            collect_user_ids_from_embed_child(child, target);
        }
    }
}

fn collect_user_ids_from_embed_child(embed: &MessageEmbedChild, target: &mut HashSet<i64>) {
    let mut mentions = MessageMentions::default();
    extend_mentions_from_markdown(embed.title.as_deref(), &mut mentions);
    extend_mentions_from_markdown(embed.description.as_deref(), &mut mentions);
    if let Some(author) = &embed.author {
        extend_mentions_from_markdown(author.name.as_deref(), &mut mentions);
    }
    if let Some(footer) = &embed.footer {
        extend_mentions_from_markdown(footer.text.as_deref(), &mut mentions);
    }
    for media in [&embed.image, &embed.thumbnail, &embed.video, &embed.audio]
        .into_iter()
        .flatten()
    {
        extend_mentions_from_markdown(media.description.as_deref(), &mut mentions);
    }
    if let Some(fields) = &embed.fields {
        for field in fields {
            extend_mentions_from_markdown(field.name.as_deref(), &mut mentions);
            extend_mentions_from_markdown(field.value.as_deref(), &mut mentions);
        }
    }
    target.extend(mentions.users);
}

fn map_user_partial(partial: UserPartialServiceResponse) -> ApiUserPartialResponse {
    if partial.user_id == FLUXER_SYSTEM_USER_ID {
        return fluxer_system_user();
    }
    let flags = partial.flags.unwrap_or_default();
    ApiUserPartialResponse {
        id: partial.user_id.to_string(),
        username: partial.username,
        discriminator: format!("{:04}", partial.discriminator),
        global_name: partial.global_name,
        avatar: partial.avatar_hash,
        avatar_color: partial.avatar_color,
        bot: partial.bot.filter(|bot| *bot),
        system: partial.system.filter(|system| *system),
        flags: i64::from(visible_user_flags(flags)),
        mention_flags: partial.mention_flags.filter(|flags| *flags != 0),
    }
}

fn fluxer_system_user() -> ApiUserPartialResponse {
    ApiUserPartialResponse {
        id: FLUXER_SYSTEM_USER_ID.to_string(),
        username: FLUXER_SYSTEM_USERNAME.to_owned(),
        discriminator: FLUXER_SYSTEM_DISCRIMINATOR.to_owned(),
        global_name: None,
        avatar: None,
        avatar_color: None,
        bot: Some(true),
        system: Some(true),
        flags: USER_FLAG_STAFF,
        mention_flags: None,
    }
}

fn deleted_user(user_id: i64) -> ApiUserPartialResponse {
    if user_id == FLUXER_SYSTEM_USER_ID {
        return fluxer_system_user();
    }
    ApiUserPartialResponse {
        id: user_id.to_string(),
        username: DELETED_USER_USERNAME.to_owned(),
        discriminator: "0000".to_owned(),
        global_name: Some(DELETED_USER_GLOBAL_NAME.to_owned()),
        avatar: None,
        avatar_color: None,
        bot: None,
        system: None,
        flags: 0,
        mention_flags: None,
    }
}

fn map_reactions(
    reactions: Vec<MessageReactionDbRow>,
    viewer_user_id: i64,
) -> Vec<ApiMessageReactionResponse> {
    #[derive(Clone)]
    struct Group {
        emoji_id: Option<i64>,
        emoji_name: String,
        animated: bool,
        count: i32,
        me: bool,
        min_created_at: i64,
    }
    let mut groups: HashMap<String, Group> = HashMap::new();
    for reaction in reactions {
        let is_custom = reaction.emoji_id != 0;
        let key = if is_custom {
            format!("custom_{}", reaction.emoji_id)
        } else {
            format!("unicode_{}", reaction.emoji_name)
        };
        let created_at = reaction
            .created_at
            .map(|dt| dt.timestamp_millis())
            .unwrap_or_default();
        groups
            .entry(key)
            .and_modify(|group| {
                group.count += 1;
                group.me = group.me || reaction.user_id == viewer_user_id;
                if created_at < group.min_created_at {
                    group.min_created_at = created_at;
                }
            })
            .or_insert_with(|| Group {
                emoji_id: is_custom.then_some(reaction.emoji_id),
                emoji_name: reaction.emoji_name,
                animated: reaction.emoji_animated.unwrap_or(false),
                count: 1,
                me: reaction.user_id == viewer_user_id,
                min_created_at: created_at,
            });
    }
    let mut groups: Vec<Group> = groups.into_values().collect();
    groups.sort_by(|left, right| {
        left.min_created_at
            .cmp(&right.min_created_at)
            .then_with(|| left.emoji_name.cmp(&right.emoji_name))
            .then_with(|| left.emoji_id.cmp(&right.emoji_id))
    });
    groups
        .into_iter()
        .map(|group| ApiMessageReactionResponse {
            emoji: ApiReactionEmojiResponse {
                id: group.emoji_id.map(|id| id.to_string()),
                name: group.emoji_name,
                animated: group.animated.then_some(true),
            },
            count: group.count,
            me: group.me.then_some(true),
        })
        .collect()
}

fn map_message_reference(reference: &MessageReference) -> Option<ApiMessageReferenceResponse> {
    Some(ApiMessageReferenceResponse {
        channel_id: reference.channel_id?.to_string(),
        message_id: reference.message_id.map(|id| id.to_string()),
        guild_id: reference.guild_id.map(|id| id.to_string()),
        reference_type: effective_reference_type(reference),
    })
}

fn map_sticker(sticker: &MessageStickerItem) -> Option<ApiMessageStickerResponse> {
    Some(ApiMessageStickerResponse {
        id: sticker.sticker_id?.to_string(),
        name: sticker.name.clone().unwrap_or_default(),
        animated: sticker.animated.unwrap_or(false),
    })
}

fn map_call(call: &MessageCall) -> ApiMessageCallResponse {
    ApiMessageCallResponse {
        participants: call
            .participant_ids
            .iter()
            .map(ToString::to_string)
            .collect(),
        ended_timestamp: call.ended_timestamp.map(epoch_millis_to_iso),
    }
}

fn make_attachment_cdn_key(channel_id: i64, attachment_id: i64, filename: &str) -> String {
    format!("attachments/{channel_id}/{attachment_id}/{filename}")
}

fn make_attachment_cdn_url(
    media_endpoint: &str,
    channel_id: i64,
    attachment_id: i64,
    filename: &str,
) -> String {
    format!(
        "{}/{}",
        media_endpoint.trim_end_matches('/'),
        make_attachment_cdn_key(channel_id, attachment_id, filename)
    )
}

fn decode_attachment_url_secret(encoded: Option<&str>) -> Vec<u8> {
    let Some(encoded) = encoded.filter(|value| !value.is_empty()) else {
        return Vec::new();
    };
    match BASE64_STANDARD.decode(encoded) {
        Ok(secret) => secret,
        Err(error) => {
            tracing::warn!(%error, "attachment url secret is not standard base64");
            Vec::new()
        }
    }
}

fn attachment_anchor_secs(attachment_id: i64) -> u64 {
    snowflake_to_epoch_millis(attachment_id).max(0) as u64 / 1_000
}

fn sign_attachment_url(
    url: &str,
    storage_key: &str,
    attachment_id: i64,
    now_secs: u64,
    secret: &[u8],
) -> String {
    if secret.is_empty() {
        return url.to_owned();
    }
    fluxer_common::attachment_url_signature::with_signature(
        url,
        storage_key,
        attachment_anchor_secs(attachment_id),
        now_secs,
        secret,
    )
}

fn attachment_cdn_url(
    channel_id: i64,
    attachment_id: i64,
    filename: &str,
    options: &ResponseBuildOptions,
    now_secs: u64,
) -> String {
    let storage_key = make_attachment_cdn_key(channel_id, attachment_id, filename);
    let url = make_attachment_cdn_url(&options.media_endpoint, channel_id, attachment_id, filename);
    sign_attachment_url(
        &url,
        &storage_key,
        attachment_id,
        now_secs,
        &options.attachment_url_secret,
    )
}

fn is_own_endpoint(media_endpoint: &str, target: &Url) -> bool {
    let Ok(base) = Url::parse(media_endpoint) else {
        return false;
    };
    let same_host = match (base.host_str(), target.host_str()) {
        (Some(base_host), Some(target_host)) => base_host.eq_ignore_ascii_case(target_host),
        _ => false,
    };
    same_host
        && base.scheme().eq_ignore_ascii_case(target.scheme())
        && base.port_or_known_default() == target.port_or_known_default()
        && target
            .path()
            .strip_prefix(base.path().trim_end_matches('/'))
            .is_some_and(|rest| rest.starts_with('/'))
}

fn raw_url_path(url: &str) -> &str {
    let Some((_, after_scheme)) = url.split_once("://") else {
        return "";
    };
    let Some(start) = after_scheme.find(['/', '?', '#']) else {
        return "";
    };
    let path = &after_scheme[start..];
    if !path.starts_with('/') {
        return "";
    }
    path.find(['?', '#']).map_or(path, |end| &path[..end])
}

fn own_attachment_key(input_url: &str, media_endpoint: &str) -> Option<(String, i64)> {
    let endpoint_path = raw_url_path(media_endpoint).trim_end_matches('/');
    let path = raw_url_path(input_url).strip_prefix(endpoint_path)?;
    if !path.starts_with("/attachments/") {
        return None;
    }
    let storage_key = fluxer_common::attachment_url_signature::decode_key(path)?;
    let attachment_id = attachment_key_id(&storage_key)?;
    Some((storage_key, attachment_id))
}

fn attachment_key_id(storage_key: &str) -> Option<i64> {
    let segments: Vec<&str> = storage_key.split('/').collect();
    let ["attachments", channel_id, attachment_id, filename @ ..] = segments.as_slice() else {
        return None;
    };
    if !is_snowflake_segment(channel_id)
        || !is_snowflake_segment(attachment_id)
        || filename.is_empty()
        || filename
            .iter()
            .any(|part| matches!(*part, "" | "." | "..") || part.contains('\0'))
    {
        return None;
    }
    attachment_id.parse().ok()
}

fn is_snowflake_segment(segment: &str) -> bool {
    (1..=20).contains(&segment.len()) && segment.bytes().all(|byte| byte.is_ascii_digit())
}

fn sign_own_media_url(
    input_url: &str,
    media_endpoint: &str,
    now_secs: u64,
    secret: &[u8],
) -> String {
    let Some((storage_key, attachment_id)) = own_attachment_key(input_url, media_endpoint) else {
        return input_url.to_owned();
    };
    sign_attachment_url(input_url, &storage_key, attachment_id, now_secs, secret)
}

fn sign_own_url(input_url: &str, options: &ResponseBuildOptions) -> String {
    sign_own_media_url(
        input_url,
        options.media_endpoint.trim_end_matches('/'),
        now_epoch_secs(),
        &options.attachment_url_secret,
    )
}

fn external_media_proxy_url(input_url: &str, options: &ResponseBuildOptions) -> String {
    media_proxy_url_at(input_url, options, now_epoch_secs())
}

fn media_proxy_url_at(input_url: &str, options: &ResponseBuildOptions, now_secs: u64) -> String {
    let media_endpoint = options.media_endpoint.trim_end_matches('/');
    let parsed_url = match Url::parse(input_url) {
        Ok(url) => url,
        Err(_) => return input_url.to_owned(),
    };
    if is_own_endpoint(media_endpoint, &parsed_url) {
        return sign_own_media_url(
            input_url,
            media_endpoint,
            now_secs,
            &options.attachment_url_secret,
        );
    }
    if options.media_proxy_secret_key.is_empty() {
        return input_url.to_owned();
    }
    fluxer_common::external_media_path::build_external_media_proxy_url(
        media_endpoint,
        parsed_url.as_str(),
        options.media_proxy_secret_key.as_bytes(),
    )
    .unwrap_or_else(|| input_url.to_owned())
}

fn dt_to_epoch_millis(dt: &DateTime<Utc>) -> i64 {
    dt.timestamp_millis()
}

fn convert_attachment(a: udt::AttachmentUdt) -> MessageAttachment {
    MessageAttachment {
        attachment_id: a.attachment_id,
        filename: a.filename,
        size: a.size,
        title: a.title,
        description: a.description,
        width: a.width,
        height: a.height,
        duration: a.duration,
        content_type: a.content_type,
        content_hash: a.content_hash,
        placeholder: a.placeholder,
        flags: a.flags,
        nsfw: a.nsfw,
        duration_secs: a.duration_secs,
        waveform: a.waveform,
    }
}

fn convert_embed_author(a: udt::EmbedAuthorUdt) -> MessageEmbedAuthor {
    MessageEmbedAuthor {
        name: a.name,
        url: a.url,
        icon_url: a.icon_url,
    }
}

fn convert_embed_field(f: udt::EmbedFieldUdt) -> MessageEmbedField {
    MessageEmbedField {
        name: f.name,
        value: f.value,
        is_inline: f.is_inline,
    }
}

fn convert_embed_footer(f: udt::EmbedFooterUdt) -> MessageEmbedFooter {
    MessageEmbedFooter {
        text: f.text,
        icon_url: f.icon_url,
    }
}

fn convert_embed_media(m: udt::EmbedMediaUdt) -> MessageEmbedMedia {
    MessageEmbedMedia {
        url: m.url,
        width: m.width,
        height: m.height,
        duration: m.duration,
        description: m.description,
        content_type: m.content_type,
        content_hash: m.content_hash,
        placeholder: m.placeholder,
        flags: m.flags,
    }
}

fn convert_embed_provider(p: udt::EmbedProviderUdt) -> MessageEmbedProvider {
    MessageEmbedProvider {
        name: p.name,
        url: p.url,
    }
}

fn convert_embed_child(c: udt::EmbedChildUdt) -> MessageEmbedChild {
    MessageEmbedChild {
        embed_type: c.embed_type,
        title: c.title,
        description: c.description,
        url: c.url,
        timestamp: c.timestamp.as_ref().map(dt_to_epoch_millis),
        color: c.color,
        author: c.author.map(convert_embed_author),
        provider: c.provider.map(convert_embed_provider),
        thumbnail: c.thumbnail.map(convert_embed_media),
        image: c.image.map(convert_embed_media),
        video: c.video.map(convert_embed_media),
        footer: c.footer.map(convert_embed_footer),
        fields: c
            .fields
            .map(|v| v.into_iter().map(convert_embed_field).collect()),
        nsfw: c.nsfw,
        audio: c.audio.map(convert_embed_media),
        html: c.html,
        html_width: c.html_width,
        html_height: c.html_height,
    }
}

fn convert_embed(e: udt::EmbedUdt) -> MessageEmbed {
    MessageEmbed {
        embed_type: e.embed_type,
        title: e.title,
        description: e.description,
        url: e.url,
        timestamp: e.timestamp.as_ref().map(dt_to_epoch_millis),
        color: e.color,
        author: e.author.map(convert_embed_author),
        provider: e.provider.map(convert_embed_provider),
        thumbnail: e.thumbnail.map(convert_embed_media),
        image: e.image.map(convert_embed_media),
        video: e.video.map(convert_embed_media),
        footer: e.footer.map(convert_embed_footer),
        fields: e
            .fields
            .map(|v| v.into_iter().map(convert_embed_field).collect()),
        nsfw: e.nsfw,
        children: e
            .children
            .map(|v| v.into_iter().map(convert_embed_child).collect()),
        audio: e.audio.map(convert_embed_media),
        html: e.html,
        html_width: e.html_width,
        html_height: e.html_height,
    }
}

fn convert_poll_emoji(p: udt::PollEmojiUdt) -> MessagePollEmoji {
    MessagePollEmoji {
        id: p.id,
        name: p.name,
    }
}

fn convert_poll_media(p: udt::PollMediaUdt) -> MessagePollMedia {
    MessagePollMedia {
        emoji: p.emoji.map(convert_poll_emoji),
        text: p.text,
    }
}

fn convert_poll_answer(p: udt::PollAnswerUdt) -> MessagePollAnswer {
    MessagePollAnswer {
        answer_id: p.answer_id,
        poll_media: p.poll_media.map(convert_poll_media),
    }
}

fn convert_poll_answer_count(p: udt::PollAnswerCountUdt) -> MessagePollAnswerCount {
    MessagePollAnswerCount {
        id: p.id,
        count: p.count,
        me_voted: p.me_voted,
    }
}

fn convert_poll_results(p: udt::PollResultsUdt) -> MessagePollResults {
    MessagePollResults {
        answer_counts: p
            .answer_counts
            .map(|v| v.into_iter().map(convert_poll_answer_count).collect()),
        is_finalized: p.is_finalized,
    }
}

fn convert_poll(p: udt::PollUdt) -> MessagePoll {
    MessagePoll {
        question: p.question.map(convert_poll_media),
        answers: (p.answers).map(|v| v.into_iter().map(convert_poll_answer).collect()),
        expiry: (p.expiry)
            .map(|expiry| expiry.to_rfc3339_opts(chrono::SecondsFormat::Millis, true)),
        anonymous_voting: p.anonymous_voting,
        allow_multiselect: p.allow_multiselect,
        layout_type: p.layout_type,
        results: p.results.map(convert_poll_results),
    }
}

fn convert_sticker_item(s: udt::StickerItemUdt) -> MessageStickerItem {
    MessageStickerItem {
        sticker_id: s.sticker_id,
        name: s.name,
        format_type: s.format_type,
        animated: s.animated,
    }
}

fn convert_message_reference(r: udt::MessageReferenceUdt) -> MessageReference {
    MessageReference {
        channel_id: r.channel_id,
        message_id: r.message_id,
        guild_id: r.guild_id,
        reference_type: r.reference_type,
    }
}

fn convert_message_call(c: udt::MessageCallUdt) -> MessageCall {
    MessageCall {
        participant_ids: c
            .participant_ids
            .map(|s| s.into_iter().collect())
            .unwrap_or_default(),
        ended_timestamp: c.ended_timestamp.as_ref().map(dt_to_epoch_millis),
    }
}

fn convert_message_snapshot(s: udt::MessageSnapshotUdt) -> MessageSnapshot {
    let edited_ts = s
        .edited_timestamp
        .as_ref()
        .or(s.edited_timestmap.as_ref())
        .map(dt_to_epoch_millis);

    MessageSnapshot {
        content: s.content,
        timestamp: s.timestamp.as_ref().map(dt_to_epoch_millis),
        edited_timestamp: edited_ts,
        mention_users: s
            .mention_users
            .map(|s| s.into_iter().collect())
            .unwrap_or_default(),
        mention_roles: s
            .mention_roles
            .map(|s| s.into_iter().collect())
            .unwrap_or_default(),
        mention_channels: s
            .mention_channels
            .map(|s| s.into_iter().collect())
            .unwrap_or_default(),
        attachments: s
            .attachments
            .map(|v| v.into_iter().map(convert_attachment).collect()),
        embeds: s.embeds.map(|v| v.into_iter().map(convert_embed).collect()),
        sticker_items: s
            .sticker_items
            .map(|v| v.into_iter().map(convert_sticker_item).collect()),
        snapshot_type: s.snapshot_type,
        flags: s.flags,
    }
}

impl From<MessageDbRow> for Message {
    fn from(row: MessageDbRow) -> Self {
        Self {
            message_id: row.message_id,
            channel_id: row.channel_id,
            bucket: row.bucket,
            author_id: row.author_id,
            message_type: row.r#type.unwrap_or_default(),
            webhook_id: row.webhook_id,
            webhook_name: row.webhook_name,
            webhook_avatar_hash: row.webhook_avatar_hash,
            content: row.content,
            edited_timestamp: row.edited_timestamp.as_ref().map(dt_to_epoch_millis),
            pinned: Some(row.pinned_timestamp.is_some()),
            flags: row.flags.map(i64::from),
            mention_everyone: row.mention_everyone,
            mention_users: row
                .mention_users
                .map(|s| s.into_iter().collect())
                .unwrap_or_default(),
            mention_roles: row
                .mention_roles
                .map(|s| s.into_iter().collect())
                .unwrap_or_default(),
            mention_channels: row
                .mention_channels
                .map(|s| s.into_iter().collect())
                .unwrap_or_default(),
            has_reaction: row.has_reaction,
            version: row.version.unwrap_or_default(),
            attachments: row
                .attachments
                .map(|v| v.into_iter().map(convert_attachment).collect()),
            embeds: row
                .embeds
                .map(|v| v.into_iter().map(convert_embed).collect()),
            poll: row.poll.map(convert_poll),
            sticker_items: row
                .sticker_items
                .map(|v| v.into_iter().map(convert_sticker_item).collect()),
            message_reference: row.message_reference.map(convert_message_reference),
            call: row.call.map(convert_message_call),
            message_snapshots: row
                .message_snapshots
                .map(|v| v.into_iter().map(convert_message_snapshot).collect()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluxer_svc::transport::{
        InMemoryTransport, TransportMessage, TransportSubscriber, reply_message,
    };
    use serde_json::json;

    #[test]
    fn id_zero_is_always_fluxer_system_user() {
        let mapped = map_user_partial(UserPartialServiceResponse {
            user_id: 0,
            username: DELETED_USER_USERNAME.to_owned(),
            discriminator: 0,
            global_name: Some(DELETED_USER_GLOBAL_NAME.to_owned()),
            avatar_hash: None,
            bot: Some(true),
            system: Some(true),
            flags: Some(USER_FLAG_DELETED),
            avatar_color: None,
            mention_flags: None,
            content_hidden_since: None,
        });

        assert_eq!(mapped.id, "0");
        assert_eq!(mapped.username, "Fluxer");
        assert_eq!(mapped.discriminator, "0000");
        assert_eq!(mapped.global_name, None);
        assert_eq!(mapped.bot, Some(true));
        assert_eq!(mapped.system, Some(true));
        assert_eq!(mapped.flags, USER_FLAG_STAFF);
    }

    #[test]
    fn missing_id_zero_fallback_is_fluxer_system_user() {
        let mapped = deleted_user(0);

        assert_eq!(mapped.id, "0");
        assert_eq!(mapped.username, "Fluxer");
        assert_eq!(mapped.global_name, None);
        assert_eq!(mapped.bot, Some(true));
        assert_eq!(mapped.system, Some(true));
    }

    #[test]
    fn missing_regular_user_fallback_is_not_bot_or_system() {
        let mapped = deleted_user(42);

        assert_eq!(mapped.id, "42");
        assert_eq!(mapped.username, DELETED_USER_USERNAME);
        assert_eq!(
            mapped.global_name,
            Some(DELETED_USER_GLOBAL_NAME.to_owned())
        );
        assert_eq!(mapped.bot, None);
        assert_eq!(mapped.system, None);
    }

    #[test]
    fn deleted_flagged_user_partials_are_not_synthetically_masked() {
        let mapped = map_user_partial(UserPartialServiceResponse {
            user_id: 42,
            username: "Ada".to_owned(),
            discriminator: 7,
            global_name: Some("Ada Lovelace".to_owned()),
            avatar_hash: Some("avatar_hash".to_owned()),
            bot: Some(false),
            system: Some(false),
            flags: Some(USER_FLAG_DELETED),
            avatar_color: Some(0x336699),
            mention_flags: None,
            content_hidden_since: None,
        });

        assert_eq!(mapped.id, "42");
        assert_eq!(mapped.username, "Ada");
        assert_eq!(mapped.discriminator, "0007");
        assert_eq!(mapped.global_name, Some("Ada Lovelace".to_owned()));
        assert_eq!(mapped.avatar, Some("avatar_hash".to_owned()));
        assert_eq!(mapped.flags, 0);
    }

    fn reaction(
        user_id: i64,
        emoji_id: i64,
        emoji_name: &str,
        created_at_ms: i64,
    ) -> MessageReactionDbRow {
        MessageReactionDbRow {
            user_id,
            emoji_id,
            emoji_name: emoji_name.to_owned(),
            emoji_animated: Some(false),
            created_at: DateTime::<Utc>::from_timestamp_millis(created_at_ms),
        }
    }

    #[test]
    fn postgres_message_decoder_maps_tagged_kv_payload() {
        let message = decode_postgres_message(json!({
            "channel_id": {"__fluxer_type": "bigint", "value": "1497639278555484216"},
            "bucket": 416,
            "message_id": {"__fluxer_type": "bigint", "value": "1509197195776110592"},
            "author_id": {"__fluxer_type": "bigint", "value": "1472426752046002208"},
            "type": null,
            "content": "hello",
            "edited_timestamp": {"__fluxer_type": "date", "value": "2026-06-15T12:34:56.789Z"},
            "pinned_timestamp": {"__fluxer_type": "date", "value": "2026-06-15T12:35:00.000Z"},
            "mention_users": {"__fluxer_type": "set", "value": [
                {"__fluxer_type": "bigint", "value": "1472426752046002208"}
            ]},
            "mention_roles": {"__fluxer_type": "set", "value": []},
            "mention_channels": {"__fluxer_type": "set", "value": []},
            "attachments": [{
                "attachment_id": {"__fluxer_type": "bigint", "value": "1509197195776110593"},
                "filename": "a.png",
                "size": {"__fluxer_type": "bigint", "value": "12345"},
                "content_type": "image/png",
                "flags": 0
            }],
            "call": {
                "participant_ids": {"__fluxer_type": "set", "value": [
                    {"__fluxer_type": "bigint", "value": "1472426752046002208"}
                ]},
                "ended_timestamp": {"__fluxer_type": "date", "value": "2026-06-15T12:36:00.000Z"}
            },
            "version": null
        }))
        .unwrap();

        assert_eq!(message.message_id, 1_509_197_195_776_110_592);
        assert_eq!(message.channel_id, 1_497_639_278_555_484_216);
        assert_eq!(message.author_id, Some(1_472_426_752_046_002_208));
        assert_eq!(message.message_type, 0);
        assert_eq!(message.edited_timestamp, Some(1_781_526_896_789));
        assert_eq!(message.pinned, Some(true));
        assert_eq!(message.version, 0);
        assert_eq!(message.mention_users, vec![1_472_426_752_046_002_208]);
        assert_eq!(
            message.attachments.unwrap()[0].attachment_id,
            Some(1_509_197_195_776_110_593)
        );
        assert_eq!(
            message.call.unwrap().participant_ids,
            vec![1_472_426_752_046_002_208]
        );
    }

    #[test]
    fn build_responses_request_accepts_legacy_null_version_rows() {
        let request: MessageRequest = serde_json::from_value(json!({
            "op": "BuildResponses",
            "messages": [{
                "message_id": "1449544529132171273",
                "channel_id": "1431572375251247158",
                "bucket": 399,
                "author_id": "1130650140672000000",
                "type": null,
                "version": null,
                "content": ""
            }],
            "viewer_user_id": "1130650140672000000",
            "source_guild_id": null,
            "message_history_cutoff_ms": null,
            "can_read_message_history": true,
            "media_endpoint": "https://media.example",
            "media_proxy_secret_key": "secret",
            "include_reactions": true
        }))
        .unwrap();

        let MessageRequest::BuildResponses { messages, .. } = request else {
            panic!("expected BuildResponses");
        };
        assert_eq!(messages[0].message_type, 0);
        assert_eq!(messages[0].version, 0);
    }

    #[test]
    fn mention_context_includes_embed_user_ids_for_message_and_snapshots() {
        let message: Message = serde_json::from_value(json!({
            "message_id": "10",
            "channel_id": "20",
            "bucket": 1,
            "author_id": "30",
            "type": 0,
            "version": 0,
            "content": "hello <@40>",
            "mention_users": ["40"],
            "embeds": [{
                "title": "title <@50>",
                "description": "description <@60>",
                "footer": {"text": "footer <@70>"},
                "fields": [{"name": "field <@80>", "value": "value <@90>"}]
            }],
            "message_snapshots": [{
                "content": "snapshot <@100>",
                "embeds": [{"description": "snapshot embed <@110>"}]
            }]
        }))
        .unwrap();
        let message_ref = &message;
        let messages = std::slice::from_ref(&message_ref);

        let mention_context = build_message_mention_context(messages);
        let entry = mention_context.get(&10).unwrap();

        assert_eq!(entry.content.users, HashSet::from([40]));
        assert_eq!(entry.embed_users, HashSet::from([50, 60, 70, 80, 90, 110]));
        assert_eq!(entry.snapshots[0].users, HashSet::from([100]));
        assert_eq!(
            collect_user_ids(messages, &mention_context),
            HashSet::from([30, 40, 50, 60, 70, 80, 90, 100, 110])
        );
    }

    #[test]
    fn postgres_reaction_decoder_maps_created_at() {
        let (message_id, reaction) = decode_postgres_reaction(json!({
            "message_id": {"__fluxer_type": "bigint", "value": "1509197195776110592"},
            "user_id": {"__fluxer_type": "bigint", "value": "1472426752046002208"},
            "emoji_id": {"__fluxer_type": "bigint", "value": "0"},
            "emoji_name": "wave",
            "emoji_animated": false,
            "created_at": {"__fluxer_type": "date", "value": "2026-06-15T12:34:56.789Z"}
        }))
        .unwrap();

        assert_eq!(message_id, 1_509_197_195_776_110_592);
        assert_eq!(reaction.user_id, 1_472_426_752_046_002_208);
        assert_eq!(reaction.emoji_name, "wave");
        assert_eq!(
            reaction.created_at.map(|dt| dt.timestamp_millis()),
            Some(1_781_526_896_789)
        );
    }

    #[test]
    fn bucket_page_limit_is_large_enough_for_sparse_channels() {
        assert_eq!(bucket_page_limit(0), 32);
        assert_eq!(bucket_page_limit(25), 32);
        assert_eq!(bucket_page_limit(50), 50);
        assert_eq!(bucket_page_limit(500), BUCKET_INDEX_PAGE_SIZE);
    }

    #[test]
    fn bucket_wave_ramp_is_bounded_by_scan_concurrency() {
        assert_eq!(next_bucket_wave(BUCKET_SCAN_WAVE), BUCKET_SCAN_WAVE * 2);
        assert_eq!(
            next_bucket_wave(BUCKET_SCAN_CONCURRENCY),
            BUCKET_SCAN_CONCURRENCY
        );
        assert_eq!(next_bucket_wave(usize::MAX), BUCKET_SCAN_CONCURRENCY);
    }

    #[tokio::test]
    async fn bucket_scan_stops_once_the_newest_buckets_fill_the_page() {
        let buckets = (0..50).map(|index| 500 - index).collect::<Vec<i32>>();
        let scanned = std::sync::Mutex::new(Vec::new());
        let mut collected: Vec<i64> = Vec::new();

        collect_bucket_waves(&buckets, 50, &mut collected, |bucket| {
            let scanned = &scanned;
            async move {
                scanned.lock().expect("scan log").push(bucket);
                let rows = (0..200)
                    .map(|row| i64::from(bucket) * 1_000 + row)
                    .collect::<Vec<i64>>();
                Ok(rows)
            }
        })
        .await
        .expect("bucket scan succeeds");

        let mut scanned = scanned.into_inner().expect("scan log");
        scanned.sort_unstable_by(|left, right| right.cmp(left));
        assert_eq!(scanned, buckets[..BUCKET_SCAN_WAVE]);

        let newest_skipped_id = i64::from(buckets[BUCKET_SCAN_WAVE]) * 1_000 + 199;
        assert!(collected.iter().all(|id| *id > newest_skipped_id));
    }

    #[tokio::test]
    async fn bucket_scan_walks_the_whole_page_when_buckets_are_sparse() {
        let buckets = (0..50).map(|index| 500 - index).collect::<Vec<i32>>();
        let scanned = std::sync::Mutex::new(0_usize);
        let mut collected: Vec<i64> = Vec::new();

        collect_bucket_waves(&buckets, 50, &mut collected, |bucket| {
            let scanned = &scanned;
            async move {
                *scanned.lock().expect("scan count") += 1;
                Ok(vec![i64::from(bucket)])
            }
        })
        .await
        .expect("bucket scan succeeds");

        assert_eq!(scanned.into_inner().expect("scan count"), buckets.len());
        assert_eq!(collected.len(), buckets.len());
    }

    #[test]
    fn reference_with_no_stored_type_is_treated_as_a_reply_everywhere() {
        let legacy = MessageReference {
            channel_id: Some(1),
            message_id: Some(2),
            guild_id: None,
            reference_type: None,
        };
        let explicit = MessageReference {
            reference_type: Some(MESSAGE_REFERENCE_TYPE_DEFAULT),
            ..legacy.clone()
        };
        let forward = MessageReference {
            reference_type: Some(MESSAGE_REFERENCE_TYPE_DEFAULT + 1),
            ..legacy.clone()
        };

        assert_eq!(
            effective_reference_type(&legacy),
            MESSAGE_REFERENCE_TYPE_DEFAULT
        );
        assert_eq!(
            effective_reference_type(&legacy),
            effective_reference_type(&explicit)
        );
        assert_ne!(
            effective_reference_type(&forward),
            MESSAGE_REFERENCE_TYPE_DEFAULT
        );

        let mapped = map_message_reference(&legacy).expect("legacy reference maps");
        assert_eq!(mapped.reference_type, MESSAGE_REFERENCE_TYPE_DEFAULT);
    }

    #[test]
    fn around_window_limits_match_reference_api() {
        assert_eq!(around_window_limits(0), (0, 0));
        assert_eq!(around_window_limits(1), (0, 0));
        assert_eq!(around_window_limits(2), (1, 0));
        assert_eq!(around_window_limits(3), (1, 1));
        assert_eq!(around_window_limits(50), (25, 24));
        assert_eq!(around_window_limits(51), (25, 25));
    }

    #[test]
    fn epoch_bucket_matches_snowflake_bucket() {
        let message_id = 1_509_197_195_776_110_592;
        assert_eq!(
            epoch_millis_to_bucket(snowflake_to_epoch_millis(message_id)),
            snowflake_to_bucket(message_id)
        );
    }

    #[test]
    fn snowflake_bucket_matches_existing_channel_rows() {
        assert_eq!(snowflake_to_bucket(1_474_193_838_282_432_581), 406);
        assert_eq!(snowflake_to_bucket(1_488_946_116_942_273_651), 410);
        assert_eq!(snowflake_to_bucket(1_509_256_674_043_502_592), 416);
    }

    #[test]
    fn message_id_descending_order_matches_api_contract() {
        let mut ids = [
            1_506_928_431_237_816_380_i64,
            1_507_125_930_562_373_970_i64,
            1_507_125_959_675_038_036_i64,
        ];
        ids.sort_unstable_by(|left, right| right.cmp(left));
        assert_eq!(
            ids,
            [
                1_507_125_959_675_038_036_i64,
                1_507_125_930_562_373_970_i64,
                1_506_928_431_237_816_380_i64,
            ]
        );
    }

    #[test]
    fn embed_field_response_defaults_null_name_and_value_to_empty_strings() {
        let response = map_embed_field_response(MessageEmbedField {
            name: None,
            value: None,
            is_inline: None,
        });

        assert_eq!(response.name, "");
        assert_eq!(response.value, "");
        assert!(!response.is_inline);
    }

    #[test]
    fn embed_media_response_hides_the_owned_attachment_flag() {
        let shard = recording_shard(&DeletedMessageKeys::default());
        let response = shard
            .map_embed_media(
                MessageEmbedMedia {
                    url: Some("https://media.example.com/attachments/1/2/a.png".to_owned()),
                    width: None,
                    height: None,
                    duration: None,
                    description: None,
                    content_type: None,
                    content_hash: None,
                    placeholder: None,
                    flags: Some(EMBED_MEDIA_OWNED_ATTACHMENT_FLAG | (1 << 3)),
                },
                &build_options(),
            )
            .unwrap();

        assert_eq!(response.flags, Some(1 << 3));
    }

    #[test]
    fn reactions_are_grouped_sorted_and_viewer_aware() {
        let mapped = map_reactions(
            vec![
                reaction(10, 0, "z", 300),
                reaction(11, 0, "a", 100),
                reaction(12, 0, "a", 200),
                reaction(11, 99, "party", 50),
            ],
            12,
        );
        assert_eq!(mapped.len(), 3);
        assert_eq!(mapped[0].emoji.id.as_deref(), Some("99"));
        assert_eq!(mapped[0].emoji.name, "party");
        assert_eq!(mapped[0].count, 1);
        assert_eq!(mapped[1].emoji.name, "a");
        assert_eq!(mapped[1].count, 2);
        assert_eq!(mapped[1].me, Some(true));
        assert_eq!(mapped[2].emoji.name, "z");
    }

    #[test]
    fn response_context_id_collection_spans_referenced_messages() {
        let message = decode_postgres_message(json!({
            "channel_id": {"__fluxer_type": "bigint", "value": "10"},
            "bucket": 416,
            "message_id": {"__fluxer_type": "bigint", "value": "100"},
            "author_id": {"__fluxer_type": "bigint", "value": "1"},
            "content": "hey <@2> over in <#20>",
            "mention_users": {"__fluxer_type": "set", "value": [
                {"__fluxer_type": "bigint", "value": "2"}
            ]},
            "mention_channels": {"__fluxer_type": "set", "value": [
                {"__fluxer_type": "bigint", "value": "20"}
            ]},
            "attachments": [{
                "attachment_id": {"__fluxer_type": "bigint", "value": "1000"},
                "filename": "a.png"
            }]
        }))
        .unwrap();
        let referenced = decode_postgres_message(json!({
            "channel_id": {"__fluxer_type": "bigint", "value": "10"},
            "bucket": 416,
            "message_id": {"__fluxer_type": "bigint", "value": "99"},
            "author_id": {"__fluxer_type": "bigint", "value": "3"},
            "content": "look at <#21>",
            "mention_channels": {"__fluxer_type": "set", "value": [
                {"__fluxer_type": "bigint", "value": "21"}
            ]},
            "attachments": [{
                "attachment_id": {"__fluxer_type": "bigint", "value": "1001"},
                "filename": "b.png"
            }]
        }))
        .unwrap();
        let messages = [message];
        let referenced_messages: HashMap<(i64, i64), Message> =
            [((10, 99), referenced)].into_iter().collect();

        let all_messages = messages
            .iter()
            .chain(referenced_messages.values())
            .collect::<Vec<_>>();
        let mention_context = build_message_mention_context(&all_messages);

        assert_eq!(
            collect_attachment_ids(&all_messages),
            HashSet::from([1000, 1001])
        );
        assert_eq!(
            collect_channel_mention_ids(&all_messages, &mention_context),
            HashSet::from([20, 21])
        );
        assert_eq!(
            collect_user_ids(&all_messages, &mention_context),
            HashSet::from([1, 2, 3])
        );
    }

    fn recording_shard(deleted: &DeletedMessageKeys) -> MessagesShard<InMemoryTransport> {
        MessagesShard {
            storage: MessagesStorage::Deletions(deleted.clone()),
            transport: InMemoryTransport::new(),
        }
    }

    fn build_options() -> ResponseBuildOptions {
        ResponseBuildOptions {
            viewer_user_id: 1,
            source_guild_id: None,
            message_history_cutoff_ms: None,
            can_read_message_history: true,
            media_endpoint: "https://media.example.com".to_owned(),
            media_proxy_secret_key: "secret".to_owned(),
            attachment_url_secret: Vec::new(),
            include_reactions: false,
            nonce: None,
            tts: false,
            include_hidden: false,
            threads_mask: false,
            excluded_types: Vec::new(),
        }
    }

    fn orphaned_message(message_id: i64) -> Message {
        decode_postgres_message(json!({
            "channel_id": {"__fluxer_type": "bigint", "value": "10"},
            "bucket": 416,
            "message_id": {"__fluxer_type": "bigint", "value": message_id.to_string()},
            "content": "orphan"
        }))
        .unwrap()
    }

    fn webhook_message(message_id: i64) -> Message {
        decode_postgres_message(json!({
            "channel_id": {"__fluxer_type": "bigint", "value": "10"},
            "bucket": 416,
            "message_id": {"__fluxer_type": "bigint", "value": message_id.to_string()},
            "webhook_id": {"__fluxer_type": "bigint", "value": "77"},
            "webhook_name": "hook",
            "content": "kept"
        }))
        .unwrap()
    }

    fn authored_message(message_id: i64) -> Message {
        decode_postgres_message(json!({
            "channel_id": {"__fluxer_type": "bigint", "value": "10"},
            "bucket": 416,
            "message_id": {"__fluxer_type": "bigint", "value": message_id.to_string()},
            "author_id": {"__fluxer_type": "bigint", "value": "1472426752046002208"},
            "content": "kept"
        }))
        .unwrap()
    }

    fn legacy_string_author_message(message_id: i64) -> Message {
        decode_postgres_message(json!({
            "channel_id": {"__fluxer_type": "bigint", "value": "10"},
            "bucket": 416,
            "message_id": {"__fluxer_type": "bigint", "value": message_id.to_string()},
            "author_id": "1472426752046002208",
            "content": "kept"
        }))
        .unwrap()
    }

    async fn stub_user_service(transport: &InMemoryTransport) -> tokio::task::JoinHandle<()> {
        let mut subscriber = transport.subscribe("svc.users").await.unwrap();
        let transport = transport.clone();
        tokio::spawn(async move {
            while let Some(message) = subscriber.next().await {
                let _ = reply_message(&message, &transport, b"\"NotFound\"").await;
            }
        })
    }

    fn recorded_deletions(deleted: &DeletedMessageKeys) -> Vec<(i64, i32, i64)> {
        deleted.lock().unwrap().clone()
    }

    #[tokio::test]
    async fn build_path_keeps_authored_rows_from_older_releases() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let users = stub_user_service(&shard.transport).await;
        let wrapped_id = 1_509_197_195_776_110_592;
        let legacy_id = 1_509_197_195_776_110_593;

        let responses = shard
            .build_api_responses_from_messages(
                vec![
                    authored_message(wrapped_id),
                    legacy_string_author_message(legacy_id),
                ],
                build_options(),
            )
            .await
            .unwrap();
        users.abort();

        assert_eq!(responses.len(), 2);
        assert_eq!(responses[0].id, wrapped_id.to_string());
        assert_eq!(responses[1].id, legacy_id.to_string());
        assert_eq!(responses[0].author.id, "1472426752046002208");
        assert_eq!(responses[1].author.id, "1472426752046002208");
        assert!(recorded_deletions(&deleted).is_empty());
    }

    #[tokio::test]
    async fn build_path_reaps_orphaned_messages() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let orphan_id = 1_509_197_195_776_110_592;
        let kept_id = 1_509_197_195_776_110_593;

        let responses = shard
            .build_api_responses_from_messages(
                vec![orphaned_message(orphan_id), webhook_message(kept_id)],
                build_options(),
            )
            .await
            .unwrap();

        assert_eq!(responses.len(), 1);
        assert_eq!(responses[0].id, kept_id.to_string());
        assert_eq!(
            recorded_deletions(&deleted),
            vec![(10, snowflake_to_bucket(orphan_id), orphan_id)]
        );
    }

    #[tokio::test]
    async fn build_path_reaps_orphans_of_each_batch_separately() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let first_orphan_id = 1_509_197_195_776_110_592;
        let second_orphan_id = 1_509_197_195_776_110_594;

        shard
            .build_api_responses_from_messages(
                vec![
                    orphaned_message(first_orphan_id),
                    webhook_message(1_509_197_195_776_110_593),
                ],
                build_options(),
            )
            .await
            .unwrap();
        shard
            .build_api_responses_from_messages(
                vec![orphaned_message(second_orphan_id)],
                build_options(),
            )
            .await
            .unwrap();

        assert_eq!(
            recorded_deletions(&deleted),
            vec![
                (10, snowflake_to_bucket(first_orphan_id), first_orphan_id),
                (10, snowflake_to_bucket(second_orphan_id), second_orphan_id),
            ]
        );
    }

    #[tokio::test]
    async fn build_path_reaps_a_repeated_orphan_without_failing() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let orphan_id = 1_509_197_195_776_110_592;

        for _ in 0..2 {
            let responses = shard
                .build_api_responses_from_messages(
                    vec![orphaned_message(orphan_id)],
                    build_options(),
                )
                .await
                .unwrap();
            assert!(responses.is_empty());
        }

        assert_eq!(
            recorded_deletions(&deleted),
            vec![
                (10, snowflake_to_bucket(orphan_id), orphan_id),
                (10, snowflake_to_bucket(orphan_id), orphan_id),
            ]
        );
    }

    #[tokio::test]
    async fn build_path_leaves_orphans_the_requester_cannot_see() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let orphan_id = 1_509_197_195_776_110_592;
        let options = ResponseBuildOptions {
            can_read_message_history: false,
            message_history_cutoff_ms: Some(snowflake_to_epoch_millis(orphan_id) + 1),
            ..build_options()
        };

        let responses = shard
            .build_api_responses_from_messages(vec![orphaned_message(orphan_id)], options)
            .await
            .unwrap();

        assert!(responses.is_empty());
        assert!(recorded_deletions(&deleted).is_empty());
    }

    #[tokio::test]
    async fn single_build_path_reaps_an_orphaned_message() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let orphan_id = 1_509_197_195_776_110_592;
        let kept_id = 1_509_197_195_776_110_593;

        let orphan_response = shard
            .build_api_response_from_message(orphaned_message(orphan_id), build_options())
            .await
            .unwrap();
        let kept_response = shard
            .build_api_response_from_message(webhook_message(kept_id), build_options())
            .await
            .unwrap();

        assert!(orphan_response.is_none());
        assert_eq!(kept_response.unwrap().id, kept_id.to_string());
        assert_eq!(
            recorded_deletions(&deleted),
            vec![(10, snowflake_to_bucket(orphan_id), orphan_id)]
        );
    }

    #[tokio::test]
    async fn single_build_path_leaves_an_orphan_the_requester_cannot_see() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let orphan_id = 1_509_197_195_776_110_592;
        let options = ResponseBuildOptions {
            can_read_message_history: false,
            message_history_cutoff_ms: Some(snowflake_to_epoch_millis(orphan_id) + 1),
            ..build_options()
        };

        let response = shard
            .build_api_response_from_message(orphaned_message(orphan_id), options)
            .await
            .unwrap();

        assert!(response.is_none());
        assert!(recorded_deletions(&deleted).is_empty());
    }

    const SIGNATURE_VECTORS: &str =
        include_str!("../../fluxer_common/src/testdata/attachment_url_signature_vectors.json");
    const SIGNED_CHANNEL_ID: i64 = 1_544_725_486_800_732_163;
    const SIGNED_ATTACHMENT_ID: i64 = 1_544_971_349_200_470_016;
    const SIGNED_FILENAME: &str = "cat.gif";
    const SIGNED_ANCHOR_SECS: u64 = 1_788_420_273;

    fn signing_options() -> ResponseBuildOptions {
        ResponseBuildOptions {
            media_endpoint: "https://media.test".to_owned(),
            attachment_url_secret: (0u8..32).collect(),
            ..build_options()
        }
    }

    fn signed_attachment() -> MessageAttachment {
        serde_json::from_value(json!({
            "attachment_id": SIGNED_ATTACHMENT_ID.to_string(),
            "filename": SIGNED_FILENAME,
            "size": 1024,
        }))
        .unwrap()
    }

    fn signed_storage_key() -> String {
        make_attachment_cdn_key(SIGNED_CHANNEL_ID, SIGNED_ATTACHMENT_ID, SIGNED_FILENAME)
    }

    fn unsigned_attachment_url(options: &ResponseBuildOptions) -> String {
        make_attachment_cdn_url(
            &options.media_endpoint,
            SIGNED_CHANNEL_ID,
            SIGNED_ATTACHMENT_ID,
            SIGNED_FILENAME,
        )
    }

    fn assert_signs(url: &str, unsigned_prefix: &str, options: &ResponseBuildOptions, now: u64) {
        assert_eq!(
            Some(format!("{unsigned_prefix}?")),
            url.split_once("ex=").map(|(head, _)| head.to_owned()),
            "{url}"
        );
        assert!(!url.contains("/external/"), "{url}");
        assert!(!url.ends_with('&'), "{url}");
        assert!(!url.contains("&&"), "{url}");
        let query = url.split_once('?').expect("a signed url has a query").1;
        assert_eq!(
            fluxer_common::attachment_url_signature::Verdict::Valid,
            fluxer_common::attachment_url_signature::verify(
                &signed_storage_key(),
                Some(query),
                &[&options.attachment_url_secret],
                now,
            )
            .verdict,
            "{url}"
        );
    }

    #[test]
    fn the_signing_anchor_is_the_attachment_snowflake_second() {
        assert_eq!(
            SIGNED_ANCHOR_SECS,
            attachment_anchor_secs(SIGNED_ATTACHMENT_ID)
        );
        assert_eq!(0, attachment_anchor_secs(i64::MIN));
    }

    #[test]
    fn attachment_urls_match_the_shared_signature_fixture() {
        let fixture: serde_json::Value = serde_json::from_str(SIGNATURE_VECTORS).unwrap();
        let secret = BASE64_STANDARD
            .decode(fixture["secrets_base64"][0].as_str().unwrap())
            .unwrap();
        let options = ResponseBuildOptions {
            attachment_url_secret: secret,
            ..signing_options()
        };
        let parsed = fixture["sign"].as_array().unwrap();
        let mut run = 0;
        let mut data_packages = 0;
        for case in parsed {
            let name = case["name"].as_str().unwrap();
            if case["uc"].as_str().unwrap() == "dp" {
                data_packages += 1;
                continue;
            }
            assert_eq!("", case["uc"].as_str().unwrap(), "{name} uc");
            let channel_id: i64 = case["channel_id"].as_str().unwrap().parse().unwrap();
            let attachment_id: i64 = case["attachment_id"].as_str().unwrap().parse().unwrap();
            let filename = case["filename"].as_str().unwrap();
            let now = case["now"].as_u64().unwrap();
            let url = case["url"].as_str().unwrap();
            let signed = case["signed"].as_str().unwrap();
            assert_eq!(
                case["anchor"].as_u64().unwrap(),
                attachment_anchor_secs(attachment_id),
                "{name} anchor"
            );
            assert_eq!(
                case["storage_key"].as_str().unwrap(),
                make_attachment_cdn_key(channel_id, attachment_id, filename),
                "{name} key"
            );
            let unsigned = make_attachment_cdn_url(
                &options.media_endpoint,
                channel_id,
                attachment_id,
                filename,
            );
            assert_eq!(
                Some(unsigned.as_str()),
                url.split(['?', '#']).next(),
                "{name} unsigned url"
            );
            if url == unsigned {
                assert_eq!(
                    signed,
                    attachment_cdn_url(channel_id, attachment_id, filename, &options, now),
                    "{name} attachment url"
                );
            }
            assert_eq!(
                signed,
                media_proxy_url_at(url, &options, now),
                "{name} embed url"
            );
            run += 1;
        }
        assert_eq!(parsed.len(), run + data_packages);
        assert!(run >= 5);
        assert!(data_packages >= 1);
    }

    #[test]
    fn an_attachment_signs_url_and_proxy_url_identically() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let options = signing_options();
        let mapped = shard
            .map_attachment(
                SIGNED_CHANNEL_ID,
                &signed_attachment(),
                &options,
                &ResponseContext::default(),
            )
            .expect("an attachment with an id maps");

        let url = mapped.url.expect("a live attachment has a url");
        assert_eq!(Some(url.clone()), mapped.proxy_url);
        assert_signs(
            &url,
            &unsigned_attachment_url(&options),
            &options,
            now_epoch_secs(),
        );
    }

    #[test]
    fn an_own_url_whose_filename_contains_a_slash_is_signed() {
        let options = signing_options();
        let now = SIGNED_ANCHOR_SECS + 10;
        let key = "attachments/1544725486800732163/1544971349200470016/a/b.gif";

        for url in [
            format!("{}/{key}", options.media_endpoint),
            format!(
                "{}/attachments/1544725486800732163/1544971349200470016/a%2Fb.gif",
                options.media_endpoint
            ),
        ] {
            let signed = media_proxy_url_at(&url, &options, now);
            assert_eq!(
                Some(format!("{url}?")),
                signed.split_once("ex=").map(|(head, _)| head.to_owned()),
                "{url}"
            );
            let query = signed.split_once('?').expect("a signed url has a query").1;
            assert_eq!(
                fluxer_common::attachment_url_signature::Verdict::Valid,
                fluxer_common::attachment_url_signature::verify(
                    key,
                    Some(query),
                    &[&options.attachment_url_secret],
                    now,
                )
                .verdict,
                "{url}"
            );
        }

        for refused in [
            format!(
                "{}/attachments/1544725486800732163/1544971349200470016/a//b.gif",
                options.media_endpoint
            ),
            format!(
                "{}/attachments/1544725486800732163/1544971349200470016/a/../b.gif",
                options.media_endpoint
            ),
            format!(
                "{}/attachments/1544725486800732163/1544971349200470016/a/",
                options.media_endpoint
            ),
        ] {
            assert_eq!(
                refused,
                media_proxy_url_at(&refused, &options, now),
                "{refused}"
            );
        }
    }

    #[test]
    fn a_decayed_attachment_is_never_signed() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let options = signing_options();
        let context = ResponseContext {
            attachment_decay: [(
                SIGNED_ATTACHMENT_ID,
                DateTime::<Utc>::from_timestamp_millis(now_epoch_millis() - 1_000).unwrap(),
            )]
            .into_iter()
            .collect(),
            ..ResponseContext::default()
        };

        let mapped = shard
            .map_attachment(SIGNED_CHANNEL_ID, &signed_attachment(), &options, &context)
            .expect("a decayed attachment still maps");

        assert_eq!(None, mapped.url);
        assert_eq!(None, mapped.proxy_url);
        assert_eq!(Some(true), mapped.expired);
    }

    #[test]
    fn an_attachment_backed_embed_signs_every_own_url_field() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let options = signing_options();
        let unsigned = unsigned_attachment_url(&options);
        let embed: MessageEmbed = serde_json::from_value(json!({
            "type": "rich",
            "url": unsigned,
            "author": {"name": "author", "url": unsigned, "icon_url": unsigned},
            "provider": {"name": "provider", "url": unsigned},
            "footer": {"text": "footer", "icon_url": unsigned},
            "image": {"url": unsigned},
            "thumbnail": {"url": unsigned},
        }))
        .unwrap();

        let mapped = shard.map_embed(&embed, &options);

        let now = now_epoch_secs();
        let base = mapped.base;
        let author = base.author.expect("the embed has an author");
        let provider = base.provider.expect("the embed has a provider");
        let footer = base.footer.expect("the embed has a footer");
        let image = base.image.expect("the embed has an image");
        let thumbnail = base.thumbnail.expect("the embed has a thumbnail");
        for signed in [
            base.url.expect("the embed has a url"),
            author.url.expect("the author has a url"),
            author.icon_url.expect("the author has an icon url"),
            author
                .proxy_icon_url
                .expect("the author has a proxy icon url"),
            provider.url.expect("the provider has a url"),
            footer.icon_url.expect("the footer has an icon url"),
            footer
                .proxy_icon_url
                .expect("the footer has a proxy icon url"),
            image.url.clone(),
            image.proxy_url.clone(),
            thumbnail.url,
            thumbnail.proxy_url,
        ] {
            assert_signs(&signed, &unsigned, &options, now);
        }
        assert_eq!(image.url, image.proxy_url);
    }

    #[test]
    fn a_foreign_embed_url_is_never_rewritten() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let options = signing_options();
        let embed: MessageEmbed = serde_json::from_value(json!({
            "type": "link",
            "url": "https://example.com/article",
            "author": {"name": "author", "url": "https://example.com/author"},
            "provider": {"name": "provider", "url": "https://example.com"},
        }))
        .unwrap();

        let mapped = shard.map_embed(&embed, &options);

        let base = mapped.base;
        assert_eq!(
            Some("https://example.com/article".to_owned()),
            base.url,
            "a page link never reaches the media proxy"
        );
        assert_eq!(
            Some("https://example.com/author".to_owned()),
            base.author.expect("the embed has an author").url
        );
        assert_eq!(
            Some("https://example.com".to_owned()),
            base.provider.expect("the embed has a provider").url
        );
    }

    #[test]
    fn an_own_attachment_embed_url_is_signed_for_every_origin_spelling() {
        let options = signing_options();
        let now = SIGNED_ANCHOR_SECS;
        for prefix in [
            "https://media.test",
            "https://MEDIA.test",
            "https://Media.Test",
            "https://media.test:443",
        ] {
            let input = format!("{prefix}/{}", signed_storage_key());
            let signed = media_proxy_url_at(&input, &options, now);
            assert_signs(&signed, &input, &options, now);
        }
    }

    #[test]
    fn an_own_attachment_embed_url_keeps_its_transform_parameters_and_fragment() {
        let options = signing_options();
        let now = SIGNED_ANCHOR_SECS;
        let input = format!(
            "https://media.test/{}?width=64&format=webp#frame",
            signed_storage_key()
        );

        let signed = media_proxy_url_at(&input, &options, now);

        assert!(signed.ends_with("&width=64&format=webp#frame"), "{signed}");
        assert!(!signed.contains("/external/"), "{signed}");
        let query = signed
            .split_once('?')
            .expect("a signed url has a query")
            .1
            .split_once('#')
            .expect("the fragment is kept")
            .0;
        assert_eq!(
            fluxer_common::attachment_url_signature::Verdict::Valid,
            fluxer_common::attachment_url_signature::verify(
                &signed_storage_key(),
                Some(query),
                &[&options.attachment_url_secret],
                now,
            )
            .verdict
        );
    }

    #[test]
    fn an_own_endpoint_url_outside_attachments_passes_through_unsigned() {
        let options = signing_options();
        let now = SIGNED_ANCHOR_SECS;
        for input in [
            "https://media.test/emojis/1544725486800732163.webp",
            "https://media.test/avatars/1/abc.png",
            "https://media.test/stickers/1/abc.png",
            "https://media.test/attachments/1544725486800732163",
            "https://media.test/attachments/1544725486800732163/1544971349200470016",
            "https://media.test/attachments/1544725486800732163/1544971349200470016/",
            "https://media.test/attachments/1544725486800732163/not-a-snowflake/cat.gif",
        ] {
            assert_eq!(input, media_proxy_url_at(input, &options, now), "{input}");
        }
    }

    #[test]
    fn a_foreign_url_is_still_wrapped_into_the_external_path() {
        let options = signing_options();
        let now = SIGNED_ANCHOR_SECS;
        for input in [
            "https://example.com/attachments/1544725486800732163/1544971349200470016/cat.gif",
            "https://media.test.example.com/attachments/1/2/cat.gif",
            "http://media.test/attachments/1/2/cat.gif",
            "https://media.test:8443/attachments/1/2/cat.gif",
        ] {
            let wrapped = media_proxy_url_at(input, &options, now);
            assert!(
                wrapped.starts_with("https://media.test/external/"),
                "{input}"
            );
            assert!(!wrapped.contains("ex="), "{input}");
        }
    }

    #[test]
    fn an_absent_attachment_secret_leaves_every_url_unsigned() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let options = ResponseBuildOptions {
            attachment_url_secret: Vec::new(),
            ..signing_options()
        };
        let unsigned = unsigned_attachment_url(&options);

        let mapped = shard
            .map_attachment(
                SIGNED_CHANNEL_ID,
                &signed_attachment(),
                &options,
                &ResponseContext::default(),
            )
            .expect("an attachment with an id maps");

        assert_eq!(Some(unsigned.clone()), mapped.url);
        assert_eq!(Some(unsigned.clone()), mapped.proxy_url);
        assert_eq!(
            unsigned,
            media_proxy_url_at(&unsigned, &options, SIGNED_ANCHOR_SECS)
        );
        assert_eq!(
            unsigned,
            attachment_cdn_url(
                SIGNED_CHANNEL_ID,
                SIGNED_ATTACHMENT_ID,
                SIGNED_FILENAME,
                &options,
                SIGNED_ANCHOR_SECS
            )
        );
    }

    #[test]
    fn a_self_hosted_path_prefix_only_matches_whole_segments() {
        let options = ResponseBuildOptions {
            media_endpoint: "https://self.test/media".to_owned(),
            ..signing_options()
        };
        let now = SIGNED_ANCHOR_SECS;
        let own = format!("https://self.test/media/{}", signed_storage_key());

        let signed = media_proxy_url_at(&own, &options, now);

        assert_signs(&signed, &own, &options, now);
        let foreign = format!("https://self.test/mediaxyz/{}", signed_storage_key());
        assert!(
            media_proxy_url_at(&foreign, &options, now)
                .starts_with("https://self.test/media/external/")
        );
    }

    #[test]
    fn a_percent_encoded_filename_signs_the_decoded_key() {
        let options = signing_options();
        let now = SIGNED_ANCHOR_SECS;
        let encoded =
            "https://media.test/attachments/1544725486800732163/1544971349200470016/caf%C3%A9.gif";
        let decoded =
            "https://media.test/attachments/1544725486800732163/1544971349200470016/café.gif";
        let key = "attachments/1544725486800732163/1544971349200470016/café.gif";

        for input in [encoded, decoded] {
            let signed = media_proxy_url_at(input, &options, now);
            let query = signed.split_once('?').expect("a signed url has a query").1;
            assert_eq!(
                fluxer_common::attachment_url_signature::Verdict::Valid,
                fluxer_common::attachment_url_signature::verify(
                    key,
                    Some(query),
                    &[&options.attachment_url_secret],
                    now,
                )
                .verdict,
                "{input}"
            );
        }
    }

    #[test]
    fn a_percent_encoded_slash_signs_the_key_the_media_proxy_decodes() {
        let options = signing_options();
        let now = SIGNED_ANCHOR_SECS;
        for input in [
            "https://media.test/attachments/1544725486800732163%2F1544971349200470016/cat.gif",
            "https://media.test/attachments/1544725486800732163%2f1544971349200470016%2Fcat.gif",
            "https://media.test/attachments/%31544725486800732163/1544971349200470016/cat.gif",
        ] {
            let raw_path = input.strip_prefix("https://media.test").unwrap();
            assert_eq!(
                Some(signed_storage_key()),
                fluxer_common::attachment_url_signature::decode_key(raw_path),
                "{input}"
            );
            let signed = media_proxy_url_at(input, &options, now);
            assert_signs(&signed, input, &options, now);
        }
        for input in [
            "https://media.test/attachments/1544725486800732163/1544971349200470016/%2F",
            "https://media.test/attachments/1544725486800732163/1544971349200470016/cat.gif%00",
        ] {
            assert_eq!(input, media_proxy_url_at(input, &options, now), "{input}");
        }
    }

    #[test]
    fn a_dot_segment_is_never_signed_under_its_normalised_key() {
        let options = signing_options();
        let now = SIGNED_ANCHOR_SECS;
        for input in [
            "https://media.test/attachments/1544725486800732163/9/../1544971349200470016/cat.gif",
            "https://media.test/attachments/1544725486800732163/1544971349200470016/./cat.gif",
            "https://media.test/attachments/1544725486800732163/1544971349200470016/%2E/cat.gif",
            "https://media.test/attachments/1544725486800732163/9/%2e%2e/1544971349200470016/cat.gif",
        ] {
            assert_eq!(
                format!("/{}", signed_storage_key()),
                Url::parse(input).unwrap().path(),
                "{input}"
            );
            assert_eq!(input, media_proxy_url_at(input, &options, now), "{input}");
        }
        for input in [
            "https://media.test/attachments/1544725486800732163/1544971349200470016/..",
            "https://media.test/attachments/+1544725486800732163/1544971349200470016/cat.gif",
            "https://media.test/attachments/1544725486800732163/-1544971349200470016/cat.gif",
            "https://media.test/attachments/1544725486800732163/1544971349200470016/caf%C3%28.gif",
        ] {
            assert_eq!(input, media_proxy_url_at(input, &options, now), "{input}");
        }
    }

    #[test]
    fn a_request_without_the_secret_field_still_decodes() {
        let request: MessageRequest = serde_json::from_value(json!({
            "op": "ListResponses",
            "channel_id": "10",
            "viewer_user_id": "1",
            "limit": 50,
            "can_read_message_history": true,
            "media_endpoint": "https://media.test",
            "media_proxy_secret_key": "secret",
        }))
        .expect("an older caller omits the attachment secret");

        match request {
            MessageRequest::ListResponses {
                attachment_url_secret_base64,
                ..
            } => assert_eq!(None, attachment_url_secret_base64),
            other => panic!("unexpected request {other:?}"),
        }
    }

    #[test]
    fn a_secret_that_is_not_base64_disables_signing() {
        assert!(decode_attachment_url_secret(None).is_empty());
        assert!(decode_attachment_url_secret(Some("")).is_empty());
        assert!(decode_attachment_url_secret(Some("not base64!")).is_empty());
        assert_eq!(
            (0u8..32).collect::<Vec<u8>>(),
            decode_attachment_url_secret(Some("AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8="))
        );
    }

    fn typed_message(message_id: i64, message_type: i32, flags: i64) -> Message {
        serde_json::from_value(json!({
            "message_id": message_id.to_string(),
            "channel_id": "10",
            "bucket": 416,
            "author_id": "1472426752046002208",
            "type": message_type,
            "flags": flags,
            "version": 1
        }))
        .unwrap()
    }

    fn masked_options() -> ResponseBuildOptions {
        ResponseBuildOptions {
            threads_mask: true,
            ..build_options()
        }
    }

    #[tokio::test]
    async fn excluded_types_refill_the_page_past_hidden_rows() {
        let channel = [
            (100, 18),
            (99, 0),
            (98, 18),
            (97, 18),
            (96, 0),
            (95, 0),
            (94, 0),
        ]
        .map(|(id, message_type)| typed_message(id, message_type, 0));
        let fetches = std::sync::Mutex::new(Vec::new());

        let collected = collect_excluding(3, &[18], None, |cursor, size| {
            let fetches = &fetches;
            let channel = &channel;
            async move {
                fetches.lock().unwrap().push((cursor, size));
                Ok(channel
                    .iter()
                    .filter(|message| cursor.is_none_or(|cursor| message.message_id < cursor))
                    .take(size as usize)
                    .cloned()
                    .collect())
            }
        })
        .await
        .unwrap();

        assert_eq!(
            collected.iter().map(|m| m.message_id).collect::<Vec<_>>(),
            vec![99, 96, 95]
        );
        assert_eq!(
            *fetches.lock().unwrap(),
            vec![(None, 3), (Some(98), EXCLUDED_TYPE_REFILL_PAGE_SIZE)]
        );
    }

    #[tokio::test]
    async fn excluded_types_stop_at_the_end_of_the_channel() {
        let channel = [(100, 18), (99, 0), (98, 18)]
            .map(|(id, message_type)| typed_message(id, message_type, 0));
        let fetches = std::sync::Mutex::new(0_usize);

        let collected = collect_excluding(2, &[18], None, |cursor, size| {
            let fetches = &fetches;
            let channel = &channel;
            async move {
                *fetches.lock().unwrap() += 1;
                Ok(channel
                    .iter()
                    .filter(|message| cursor.is_none_or(|cursor| message.message_id < cursor))
                    .take(size as usize)
                    .cloned()
                    .collect())
            }
        })
        .await
        .unwrap();

        assert_eq!(
            collected.iter().map(|m| m.message_id).collect::<Vec<_>>(),
            vec![99]
        );
        assert_eq!(*fetches.lock().unwrap(), 2);
    }

    #[tokio::test]
    async fn excluded_types_reach_past_a_long_hidden_run() {
        let channel = (0..3_000_i64)
            .map(|offset| typed_message(10_000 - offset, 18, 0))
            .chain([typed_message(6_999, 0, 0), typed_message(6_998, 0, 0)])
            .collect::<Vec<_>>();

        let collected = collect_excluding(2, &[18], None, |cursor, size| {
            let channel = &channel;
            async move {
                Ok(channel
                    .iter()
                    .filter(|message| cursor.is_none_or(|cursor| message.message_id < cursor))
                    .take(size as usize)
                    .cloned()
                    .collect())
            }
        })
        .await
        .unwrap();

        assert_eq!(
            collected.iter().map(|m| m.message_id).collect::<Vec<_>>(),
            vec![6_999, 6_998]
        );
    }

    #[tokio::test]
    async fn excluded_types_stop_at_the_scan_row_cap() {
        let scanned = std::sync::Mutex::new(0_usize);

        let collected = collect_excluding(2, &[18], None, |cursor, size| {
            let scanned = &scanned;
            async move {
                *scanned.lock().unwrap() += size as usize;
                let top = cursor.unwrap_or(1_000_000);
                Ok((1..=i64::from(size))
                    .map(|offset| typed_message(top - offset, 18, 0))
                    .collect())
            }
        })
        .await
        .unwrap();

        assert!(collected.is_empty());
        let scanned = *scanned.lock().unwrap();
        assert!(scanned >= EXCLUDED_TYPE_SCAN_ROW_CAP);
        assert!(scanned < EXCLUDED_TYPE_SCAN_ROW_CAP + EXCLUDED_TYPE_REFILL_PAGE_SIZE as usize);
    }

    const LISTED_BASE_ID: i64 = 1_509_197_195_776_110_592;

    async fn listed_ids(
        types: &[i32],
        limit: u32,
        before: Option<i64>,
        after: Option<i64>,
        around: Option<i64>,
        excluded_types: &[i32],
    ) -> Vec<i64> {
        let rows = types
            .iter()
            .enumerate()
            .map(|(index, message_type)| {
                typed_message(LISTED_BASE_ID + index as i64 + 1, *message_type, 0)
            })
            .collect::<Vec<_>>();
        let shard = MessagesShard {
            storage: MessagesStorage::Memory(std::sync::Arc::new(rows)),
            transport: InMemoryTransport::new(),
        };
        let users = stub_user_service(&shard.transport).await;
        let at = |offset: i64| LISTED_BASE_ID + offset;
        let responses = shard
            .list_api_responses(
                10,
                limit,
                before.map(at),
                after.map(at),
                around.map(at),
                ResponseBuildOptions {
                    excluded_types: excluded_types.to_vec(),
                    ..build_options()
                },
            )
            .await
            .unwrap();
        users.abort();
        responses
            .iter()
            .map(|response| response.id.parse::<i64>().unwrap() - LISTED_BASE_ID)
            .collect()
    }

    #[tokio::test]
    async fn excluded_types_list_the_latest_page_past_hidden_rows() {
        let types = [0, 0, 18, 0, 18, 18, 0, 18];

        assert_eq!(
            listed_ids(&types, 3, None, None, None, &[18]).await,
            [7, 4, 2]
        );
        assert_eq!(
            listed_ids(&types, 3, None, None, None, &[]).await,
            [8, 7, 6]
        );
    }

    #[tokio::test]
    async fn excluded_types_list_after_an_id_in_ascending_pages() {
        let types = [18, 18, 18, 0, 18, 0, 0, 0];

        assert_eq!(
            listed_ids(&types, 3, None, Some(0), None, &[18]).await,
            [7, 6, 4]
        );
        assert_eq!(
            listed_ids(&types, 3, None, Some(0), None, &[]).await,
            [3, 2, 1]
        );
    }

    #[tokio::test]
    async fn excluded_types_keep_a_before_and_after_window_inside_its_bounds() {
        let types = [0, 0, 0, 18, 0, 18, 18, 0];

        assert_eq!(
            listed_ids(&types, 3, Some(9), Some(1), None, &[18]).await,
            [8, 5, 3]
        );
        assert_eq!(
            listed_ids(&types, 3, Some(9), Some(4), None, &[18]).await,
            [8, 5]
        );
        assert_eq!(
            listed_ids(&types, 3, Some(9), Some(1), None, &[]).await,
            [8, 7, 6]
        );
    }

    #[tokio::test]
    async fn excluded_types_drop_a_hidden_around_target_and_refill_both_sides() {
        let types = [0, 0, 18, 0, 18, 0, 18, 0, 0];

        assert_eq!(
            listed_ids(&types, 5, None, None, Some(5), &[18]).await,
            [8, 6, 4, 2]
        );
        assert_eq!(
            listed_ids(&types, 5, None, None, Some(5), &[]).await,
            [7, 6, 5, 4, 3]
        );
    }

    #[tokio::test]
    async fn threads_mask_drops_thread_created_rows_and_clears_thread_flags() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let users = stub_user_service(&shard.transport).await;
        let mut forwarded = typed_message(1_509_197_195_776_110_594, 0, 0x120 | 0x4);
        forwarded.message_snapshots = Some(vec![
            serde_json::from_value(json!({"content": "x", "type": 0, "flags": 0x124})).unwrap(),
        ]);
        let messages = vec![
            forwarded,
            typed_message(1_509_197_195_776_110_593, MESSAGE_TYPE_THREAD_CREATED, 0),
        ];

        let masked = shard
            .build_api_responses_from_messages(messages.clone(), masked_options())
            .await
            .unwrap();
        let control = shard
            .build_api_responses_from_messages(messages, build_options())
            .await
            .unwrap();
        users.abort();

        assert_eq!(masked.len(), 1);
        assert_eq!(masked[0].flags, 0x4);
        assert_eq!(masked[0].message_snapshots.as_ref().unwrap()[0].flags, 0x4);
        assert_eq!(control.len(), 2);
        assert_eq!(control[0].flags, 0x124);
        assert_eq!(
            control[0].message_snapshots.as_ref().unwrap()[0].flags,
            0x124
        );
        assert_eq!(control[1].message_type, MESSAGE_TYPE_THREAD_CREATED);
        assert!(recorded_deletions(&deleted).is_empty());
    }

    #[tokio::test]
    async fn threads_mask_drops_a_single_thread_created_row() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let users = stub_user_service(&shard.transport).await;
        let starter = typed_message(1_509_197_195_776_110_593, MESSAGE_TYPE_THREAD_CREATED, 0);

        let masked = shard
            .build_api_response_from_message(starter.clone(), masked_options())
            .await
            .unwrap();
        let control = shard
            .build_api_response_from_message(starter, build_options())
            .await
            .unwrap();
        users.abort();

        assert!(masked.is_none());
        assert!(control.is_some());
    }

    #[test]
    fn threads_mask_clears_nested_referenced_flags_and_nulls_a_thread_created_reference() {
        let shard = recording_shard(&DeletedMessageKeys::default());
        let reply_to = |target: i64| {
            let mut reply = typed_message(1_509_197_195_776_110_600, 19, 0x20);
            reply.message_reference = Some(MessageReference {
                channel_id: Some(10),
                message_id: Some(target),
                guild_id: None,
                reference_type: Some(0),
            });
            reply
        };
        let referenced = typed_message(1_509_197_195_776_110_590, 0, 0x120);
        let thread_created =
            typed_message(1_509_197_195_776_110_591, MESSAGE_TYPE_THREAD_CREATED, 0);
        let context = ResponseContext {
            referenced_messages: HashMap::from([
                ((10, referenced.message_id), referenced.clone()),
                ((10, thread_created.message_id), thread_created.clone()),
            ]),
            ..ResponseContext::default()
        };

        let masked = shard.map_message_response(
            &reply_to(referenced.message_id),
            &masked_options(),
            &context,
            true,
        );
        assert_eq!(masked.flags, 0);
        let nested = masked.referenced_message.unwrap().unwrap();
        assert_eq!(nested.flags, 0);

        let control = shard.map_message_response(
            &reply_to(referenced.message_id),
            &build_options(),
            &context,
            true,
        );
        assert_eq!(control.flags, 0x20);
        assert_eq!(control.referenced_message.unwrap().unwrap().flags, 0x120);

        let masked_starter = shard.map_message_response(
            &reply_to(thread_created.message_id),
            &masked_options(),
            &context,
            true,
        );
        assert!(matches!(masked_starter.referenced_message, Some(None)));
        let control_starter = shard.map_message_response(
            &reply_to(thread_created.message_id),
            &build_options(),
            &context,
            true,
        );
        assert!(matches!(control_starter.referenced_message, Some(Some(_))));
    }

    #[test]
    fn thread_options_default_to_off_for_older_callers() {
        let list: MessageRequest = serde_json::from_value(json!({
            "op": "ListResponses",
            "channel_id": "10",
            "viewer_user_id": "1",
            "limit": 50,
            "can_read_message_history": true,
            "media_endpoint": "https://media.test",
            "media_proxy_secret_key": "secret",
        }))
        .unwrap();
        match list {
            MessageRequest::ListResponses {
                threads_mask,
                exclude_types,
                ..
            } => {
                assert!(!threads_mask);
                assert!(exclude_types.is_empty());
            }
            other => panic!("unexpected request {other:?}"),
        }
        let get: MessageRequest = serde_json::from_value(json!({
            "op": "GetResponseById",
            "channel_id": "10",
            "message_id": "11",
            "viewer_user_id": "1",
            "can_read_message_history": true,
            "media_endpoint": "https://media.test",
            "media_proxy_secret_key": "secret",
        }))
        .unwrap();
        match get {
            MessageRequest::GetResponseById { threads_mask, .. } => assert!(!threads_mask),
            other => panic!("unexpected request {other:?}"),
        }
        let options = build_options();
        assert_eq!(options.mask_flags(0x1ff), 0x1ff);
        assert!(!options.hides(&typed_message(1, MESSAGE_TYPE_THREAD_CREATED, 0)));
    }

    #[test]
    fn a_reference_without_a_message_id_round_trips() {
        let message: Message = serde_json::from_value(json!({
            "message_id": "1509197195776110592",
            "channel_id": "10",
            "bucket": 416,
            "author_id": "1472426752046002208",
            "type": MESSAGE_TYPE_THREAD_CREATED,
            "version": 1,
            "message_reference": {"channel_id": "1509197195776110500", "guild_id": "7", "type": 0}
        }))
        .unwrap();
        let reference = message.message_reference.as_ref().unwrap();
        assert_eq!(reference.message_id, None);

        let mapped = map_message_reference(reference).expect("a thread reference maps");
        assert_eq!(mapped.message_id, None);
        let encoded = serde_json::to_value(&mapped).unwrap();
        assert_eq!(
            encoded,
            json!({"channel_id": "1509197195776110500", "guild_id": "7", "type": 0})
        );
        let decoded: ApiMessageReferenceResponse = serde_json::from_value(encoded).unwrap();
        assert_eq!(decoded.message_id, None);

        let shard = recording_shard(&DeletedMessageKeys::default());
        let response = shard.map_message_response(
            &message,
            &build_options(),
            &ResponseContext::default(),
            true,
        );
        assert!(response.referenced_message.is_none());
        let packed = rmp_serde::to_vec_named(&response).unwrap();
        let unpacked: ApiMessageResponse = rmp_serde::from_slice(&packed).unwrap();
        let unpacked_reference = unpacked.message_reference.unwrap();
        assert_eq!(unpacked_reference.message_id, None);
        assert_eq!(unpacked_reference.channel_id, "1509197195776110500");

        let reply = MessageReference {
            channel_id: Some(1),
            message_id: Some(2),
            guild_id: None,
            reference_type: None,
        };
        assert_eq!(
            serde_json::to_value(map_message_reference(&reply).unwrap()).unwrap(),
            json!({"channel_id": "1", "message_id": "2", "type": 0})
        );
    }

    async fn stub_channel_mentions(
        transport: &InMemoryTransport,
        payloads: std::sync::Arc<std::sync::Mutex<Vec<serde_json::Value>>>,
    ) -> tokio::task::JoinHandle<()> {
        let mut subscriber = transport
            .subscribe("rpc.gateway.guild.resolve_channel_mentions")
            .await
            .unwrap();
        let transport = transport.clone();
        tokio::spawn(async move {
            while let Some(message) = subscriber.next().await {
                payloads
                    .lock()
                    .unwrap()
                    .push(serde_json::from_slice(TransportMessage::payload(&message)).unwrap());
                let reply = json!({"ok": true, "result": {"channels": [
                    {"id": "20", "name": "general", "type": 0},
                    {"id": "21", "name": "thread", "type": 11},
                    {"id": "22", "name": "forum", "type": 15},
                    {"id": "23", "name": "news-thread", "type": 10}
                ]}});
                let _ = reply_message(&message, &transport, reply.to_string().as_bytes()).await;
            }
        })
    }

    #[tokio::test]
    async fn threads_mask_excludes_thread_channels_from_mention_resolution() {
        let shard = recording_shard(&DeletedMessageKeys::default());
        let payloads = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let stub = stub_channel_mentions(&shard.transport, payloads.clone()).await;
        let ids = HashSet::from([20, 21, 22, 23]);

        let masked = shard
            .resolve_channel_mentions(
                ids.clone(),
                &ResponseBuildOptions {
                    source_guild_id: Some(7),
                    ..masked_options()
                },
            )
            .await;
        let control = shard
            .resolve_channel_mentions(
                ids,
                &ResponseBuildOptions {
                    source_guild_id: Some(7),
                    ..build_options()
                },
            )
            .await;
        stub.abort();

        let mut masked_ids = masked.into_keys().collect::<Vec<_>>();
        masked_ids.sort();
        assert_eq!(masked_ids, vec!["20"]);
        assert_eq!(control.len(), 4);
        assert_eq!(control["23"].channel_type, 10);
        let payloads = payloads.lock().unwrap();
        assert_eq!(payloads[0]["exclude_thread_channels"], json!(true));
        assert!(payloads[1].get("exclude_thread_channels").is_none());
    }

    const CROSSPOST_SOURCE_CHANNEL_ID: i64 = 500;
    const CROSSPOST_SOURCE_GUILD_ID: i64 = 600;
    const CROSSPOST_SOURCE_MESSAGE_ID: i64 = 1_509_197_195_776_110_590;

    fn referencing_message(message_type: i32, flags: i64, reference: serde_json::Value) -> Message {
        decode_postgres_message(json!({
            "channel_id": {"__fluxer_type": "bigint", "value": "10"},
            "bucket": 416,
            "message_id": {"__fluxer_type": "bigint", "value": "1509197195776110600"},
            "webhook_id": {"__fluxer_type": "bigint", "value": "77"},
            "webhook_name": "Source #news",
            "type": message_type,
            "flags": flags,
            "content": "published",
            "message_reference": reference
        }))
        .unwrap()
    }

    fn source_reference() -> serde_json::Value {
        json!({
            "channel_id": CROSSPOST_SOURCE_CHANNEL_ID.to_string(),
            "message_id": CROSSPOST_SOURCE_MESSAGE_ID.to_string(),
            "guild_id": CROSSPOST_SOURCE_GUILD_ID.to_string(),
            "type": MESSAGE_REFERENCE_TYPE_DEFAULT
        })
    }

    fn source_message() -> Message {
        decode_postgres_message(json!({
            "channel_id": {"__fluxer_type": "bigint", "value": CROSSPOST_SOURCE_CHANNEL_ID.to_string()},
            "bucket": 416,
            "message_id": {"__fluxer_type": "bigint", "value": CROSSPOST_SOURCE_MESSAGE_ID.to_string()},
            "author_id": {"__fluxer_type": "bigint", "value": "1472426752046002208"},
            "flags": 1,
            "content": "published"
        }))
        .unwrap()
    }

    fn context_with_source() -> ResponseContext {
        ResponseContext {
            referenced_messages: [(
                (CROSSPOST_SOURCE_CHANNEL_ID, CROSSPOST_SOURCE_MESSAGE_ID),
                source_message(),
            )]
            .into_iter()
            .collect(),
            ..ResponseContext::default()
        }
    }

    fn serialized_response(message: &Message, context: &ResponseContext) -> serde_json::Value {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        serde_json::to_value(shard.map_message_response(message, &build_options(), context, true))
            .expect("response serialises")
    }

    fn signed_attachment_response(message: &Message) -> serde_json::Value {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let response = serde_json::to_value(shard.map_message_response(
            message,
            &signing_options(),
            &ResponseContext::default(),
            true,
        ))
        .expect("response serialises");
        response["attachments"][0].clone()
    }

    fn assert_attachment_signed_for_channel(attachment: &serde_json::Value, channel_id: i64) {
        let options = signing_options();
        let storage_key =
            make_attachment_cdn_key(channel_id, SIGNED_ATTACHMENT_ID, SIGNED_FILENAME);
        let unsigned = make_attachment_cdn_url(
            &options.media_endpoint,
            channel_id,
            SIGNED_ATTACHMENT_ID,
            SIGNED_FILENAME,
        );
        let url = attachment["url"]
            .as_str()
            .expect("a live attachment has a url");
        assert_eq!(attachment["proxy_url"], attachment["url"]);
        assert_eq!(attachment["id"], json!(SIGNED_ATTACHMENT_ID.to_string()));
        assert_eq!(
            Some(format!("{unsigned}?")),
            url.split_once("ex=").map(|(head, _)| head.to_owned()),
            "{url}"
        );
        let query = url.split_once('?').expect("a signed url has a query").1;
        assert_eq!(
            fluxer_common::attachment_url_signature::Verdict::Valid,
            fluxer_common::attachment_url_signature::verify(
                &storage_key,
                Some(query),
                &[&options.attachment_url_secret],
                now_epoch_secs(),
            )
            .verdict,
            "{url}"
        );
    }

    #[test]
    fn crosspost_copy_attachment_urls_point_at_the_source_channel() {
        let mut copy = referencing_message(0, MESSAGE_FLAG_IS_CROSSPOST, source_reference());
        copy.attachments = Some(vec![signed_attachment()]);

        assert_eq!(
            attachment_storage_channel_id(&copy),
            CROSSPOST_SOURCE_CHANNEL_ID
        );
        let attachment = signed_attachment_response(&copy);
        assert_attachment_signed_for_channel(&attachment, CROSSPOST_SOURCE_CHANNEL_ID);
        assert!(
            !attachment["url"]
                .as_str()
                .unwrap()
                .contains(&format!("/attachments/{}/", copy.channel_id)),
            "{attachment}"
        );
    }

    #[test]
    fn a_message_without_the_copy_flag_keeps_its_own_channel_for_attachments() {
        for message in [
            referencing_message(19, 0, source_reference()),
            referencing_message(0, 0, source_reference()),
            webhook_message(1_509_197_195_776_110_600),
        ] {
            let mut message = message;
            message.attachments = Some(vec![signed_attachment()]);
            assert_eq!(attachment_storage_channel_id(&message), message.channel_id);
            let attachment = signed_attachment_response(&message);
            assert_attachment_signed_for_channel(&attachment, message.channel_id);
        }
    }

    #[test]
    fn a_copy_without_a_reference_channel_falls_back_to_its_own_channel() {
        let mut copy = referencing_message(0, MESSAGE_FLAG_IS_CROSSPOST, source_reference());
        copy.message_reference = None;
        copy.attachments = Some(vec![signed_attachment()]);

        assert_eq!(attachment_storage_channel_id(&copy), copy.channel_id);
        let attachment = signed_attachment_response(&copy);
        assert_attachment_signed_for_channel(&attachment, copy.channel_id);
    }

    #[tokio::test]
    async fn crosspost_copy_omits_referenced_message_and_requests_no_fetch() {
        let copy = referencing_message(0, MESSAGE_FLAG_IS_CROSSPOST, source_reference());

        assert_eq!(reply_target(&copy), None);
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let fetched = shard
            .fetch_referenced_messages(std::slice::from_ref(&copy), &build_options())
            .await;
        assert!(fetched.is_empty());

        let response = serialized_response(&copy, &context_with_source());
        let object = response.as_object().expect("response is an object");
        assert!(!object.contains_key("referenced_message"));
        assert_eq!(
            response["message_reference"],
            json!({
                "channel_id": CROSSPOST_SOURCE_CHANNEL_ID.to_string(),
                "message_id": CROSSPOST_SOURCE_MESSAGE_ID.to_string(),
                "guild_id": CROSSPOST_SOURCE_GUILD_ID.to_string(),
                "type": MESSAGE_REFERENCE_TYPE_DEFAULT
            })
        );
        assert_eq!(response["flags"], json!(MESSAGE_FLAG_IS_CROSSPOST));
    }

    #[test]
    fn crosspost_copy_with_other_flags_still_omits_referenced_message() {
        let copy = referencing_message(
            0,
            MESSAGE_FLAG_IS_CROSSPOST | MESSAGE_FLAG_SUPPRESS_EMBEDS | (1 << 3),
            source_reference(),
        );

        assert_eq!(reply_target(&copy), None);
        let response = serialized_response(&copy, &context_with_source());
        assert!(response.get("referenced_message").is_none());
    }

    #[test]
    fn reply_with_the_same_reference_still_resolves() {
        let reply = referencing_message(19, 0, source_reference());

        assert_eq!(
            reply_target(&reply),
            Some((CROSSPOST_SOURCE_CHANNEL_ID, CROSSPOST_SOURCE_MESSAGE_ID))
        );
        let response = serialized_response(&reply, &context_with_source());
        assert_eq!(
            response["referenced_message"]["id"],
            json!(CROSSPOST_SOURCE_MESSAGE_ID.to_string())
        );
        assert_eq!(
            response["referenced_message"]["content"],
            json!("published")
        );
    }

    #[test]
    fn reply_whose_target_is_missing_serialises_referenced_message_null() {
        let reply = referencing_message(19, 0, source_reference());

        let response = serialized_response(&reply, &ResponseContext::default());
        assert_eq!(
            response.get("referenced_message"),
            Some(&serde_json::Value::Null)
        );
    }

    #[tokio::test]
    async fn follow_add_reference_without_message_id_keeps_channel_and_guild() {
        let follow_add = referencing_message(
            12,
            0,
            json!({
                "channel_id": CROSSPOST_SOURCE_CHANNEL_ID.to_string(),
                "guild_id": CROSSPOST_SOURCE_GUILD_ID.to_string(),
                "type": MESSAGE_REFERENCE_TYPE_DEFAULT
            }),
        );

        assert_eq!(reply_target(&follow_add), None);
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let fetched = shard
            .fetch_referenced_messages(std::slice::from_ref(&follow_add), &build_options())
            .await;
        assert!(fetched.is_empty());

        let response = serialized_response(&follow_add, &context_with_source());
        assert_eq!(response["type"], json!(12));
        assert!(response.get("referenced_message").is_none());
        assert_eq!(
            response["message_reference"],
            json!({
                "channel_id": CROSSPOST_SOURCE_CHANNEL_ID.to_string(),
                "guild_id": CROSSPOST_SOURCE_GUILD_ID.to_string(),
                "type": MESSAGE_REFERENCE_TYPE_DEFAULT
            })
        );
    }

    #[test]
    fn map_message_reference_emits_every_field_for_a_copy() {
        let mapped = map_message_reference(&MessageReference {
            channel_id: Some(CROSSPOST_SOURCE_CHANNEL_ID),
            message_id: Some(CROSSPOST_SOURCE_MESSAGE_ID),
            guild_id: Some(CROSSPOST_SOURCE_GUILD_ID),
            reference_type: Some(MESSAGE_REFERENCE_TYPE_DEFAULT),
        })
        .expect("copy reference maps");

        assert_eq!(mapped.channel_id, CROSSPOST_SOURCE_CHANNEL_ID.to_string());
        assert_eq!(
            mapped.message_id,
            Some(CROSSPOST_SOURCE_MESSAGE_ID.to_string())
        );
        assert_eq!(mapped.guild_id, Some(CROSSPOST_SOURCE_GUILD_ID.to_string()));
        assert_eq!(mapped.reference_type, MESSAGE_REFERENCE_TYPE_DEFAULT);
    }

    #[test]
    fn map_message_reference_still_requires_a_channel_id() {
        let mapped = map_message_reference(&MessageReference {
            channel_id: None,
            message_id: Some(CROSSPOST_SOURCE_MESSAGE_ID),
            guild_id: Some(CROSSPOST_SOURCE_GUILD_ID),
            reference_type: None,
        });

        assert!(mapped.is_none());
    }

    const HIDDEN_AUTHOR: i64 = 1_472_426_752_046_002_301;
    const OTHER_AUTHOR: i64 = 1_472_426_752_046_002_302;
    const CHANNEL: i64 = 10;
    const WINDOW_MS: i64 = 1_790_000_000_000;

    fn snowflake_at(epoch_millis: i64, sequence: i64) -> i64 {
        ((epoch_millis - FLUXER_EPOCH_MS) << 22) | sequence
    }

    fn message_by(author_id: i64, message_id: i64, extra: serde_json::Value) -> Message {
        let mut row = json!({
            "channel_id": {"__fluxer_type": "bigint", "value": CHANNEL.to_string()},
            "bucket": snowflake_to_bucket(message_id),
            "message_id": {"__fluxer_type": "bigint", "value": message_id.to_string()},
            "author_id": {"__fluxer_type": "bigint", "value": author_id.to_string()},
            "content": format!("message {message_id}"),
        });
        row.as_object_mut()
            .unwrap()
            .extend(extra.as_object().unwrap().clone());
        decode_postgres_message(row).unwrap()
    }

    async fn user_service(
        transport: &InMemoryTransport,
        hidden_since: Option<i64>,
    ) -> tokio::task::JoinHandle<()> {
        let mut subscriber = transport.subscribe("svc.users").await.unwrap();
        let transport = transport.clone();
        tokio::spawn(async move {
            while let Some(message) = subscriber.next().await {
                let request: serde_json::Value = serde_json::from_slice(message.payload()).unwrap();
                let partials = request["user_ids"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|id| {
                        let id = id.as_i64().unwrap();
                        json!({
                            "user_id": id,
                            "username": format!("user{id}"),
                            "discriminator": 1,
                            "content_hidden_since": (id == HIDDEN_AUTHOR).then_some(hidden_since).flatten(),
                        })
                    })
                    .collect::<Vec<_>>();
                let reply = serde_json::to_vec(&json!({"FoundPartials": partials})).unwrap();
                let _ = reply_message(&message, &transport, &reply).await;
            }
        })
    }

    fn memory_shard(messages: Vec<Message>) -> MessagesShard<InMemoryTransport> {
        MessagesShard {
            storage: MessagesStorage::Memory(std::sync::Arc::new(messages)),
            transport: InMemoryTransport::new(),
        }
    }

    fn channel_history() -> (Vec<Message>, Vec<i64>, Vec<i64>) {
        let mut messages = Vec::new();
        let mut hidden = Vec::new();
        let mut visible = Vec::new();
        for offset in 0..3 {
            let id = snowflake_at(WINDOW_MS - 60_000 + offset, 0);
            messages.push(message_by(HIDDEN_AUTHOR, id, json!({})));
            visible.push(id);
        }
        for offset in 0..2 {
            let id = snowflake_at(WINDOW_MS - 30_000 + offset, 0);
            messages.push(message_by(OTHER_AUTHOR, id, json!({})));
            visible.push(id);
        }
        hidden.push(snowflake_at(WINDOW_MS, 0));
        messages.push(message_by(HIDDEN_AUTHOR, hidden[0], json!({})));
        for offset in 1..7 {
            let id = snowflake_at(WINDOW_MS + offset * 1_000, 0);
            messages.push(message_by(HIDDEN_AUTHOR, id, json!({})));
            hidden.push(id);
        }
        visible.sort_unstable_by_key(|id| std::cmp::Reverse(*id));
        (messages, hidden, visible)
    }

    fn ids(responses: &[ApiMessageResponse]) -> Vec<i64> {
        responses
            .iter()
            .map(|response| response.id.parse().unwrap())
            .collect()
    }

    #[tokio::test]
    async fn hidden_window_drops_messages_from_every_list_shape_and_refills_the_page() {
        let (messages, hidden, visible) = channel_history();
        let shard = memory_shard(messages);
        let users = user_service(&shard.transport, Some(WINDOW_MS)).await;

        let latest = shard
            .list_api_responses(CHANNEL, 3, None, None, None, build_options())
            .await
            .unwrap();
        assert_eq!(ids(&latest), visible[..3].to_vec());

        let before = shard
            .list_api_responses(CHANNEL, 50, Some(hidden[6]), None, None, build_options())
            .await
            .unwrap();
        assert_eq!(ids(&before), visible);

        let after = shard
            .list_api_responses(CHANNEL, 50, None, Some(visible[1]), None, build_options())
            .await
            .unwrap();
        assert_eq!(ids(&after), vec![visible[0]]);

        let around = shard
            .list_api_responses(CHANNEL, 4, None, None, Some(hidden[0]), build_options())
            .await
            .unwrap();
        assert!(ids(&around).iter().all(|id| visible.contains(id)));
        assert!(!around.is_empty());

        let mut staff_view = build_options();
        staff_view.include_hidden = true;
        let all = shard
            .list_api_responses(CHANNEL, 50, None, None, None, staff_view)
            .await
            .unwrap();
        assert_eq!(all.len(), visible.len() + hidden.len());
        users.abort();
    }

    #[tokio::test]
    async fn clearing_the_window_restores_every_message() {
        let (messages, hidden, visible) = channel_history();
        let shard = memory_shard(messages);
        let users = user_service(&shard.transport, None).await;

        let latest = shard
            .list_api_responses(CHANNEL, 50, None, None, None, build_options())
            .await
            .unwrap();
        assert_eq!(latest.len(), visible.len() + hidden.len());
        let single = shard
            .get_api_response(CHANNEL, hidden[0], build_options())
            .await
            .unwrap();
        assert_eq!(single.unwrap().id, hidden[0].to_string());
        users.abort();
    }

    #[tokio::test]
    async fn single_fetch_hides_messages_inside_the_window_only() {
        let (messages, hidden, visible) = channel_history();
        let shard = memory_shard(messages);
        let users = user_service(&shard.transport, Some(WINDOW_MS)).await;

        for id in &hidden {
            let response = shard
                .get_api_response(CHANNEL, *id, build_options())
                .await
                .unwrap();
            assert!(response.is_none(), "{id}");
        }
        let earlier = visible.last().copied().unwrap();
        let response = shard
            .get_api_response(CHANNEL, earlier, build_options())
            .await
            .unwrap();
        assert_eq!(response.unwrap().author.id, HIDDEN_AUTHOR.to_string());

        let mut staff_view = build_options();
        staff_view.include_hidden = true;
        assert!(
            shard
                .get_api_response(CHANNEL, hidden[0], staff_view)
                .await
                .unwrap()
                .is_some()
        );
        users.abort();
    }

    #[tokio::test]
    async fn search_pins_and_saved_builds_drop_hidden_messages() {
        let (messages, hidden, visible) = channel_history();
        let shard = memory_shard(messages.clone());
        let users = user_service(&shard.transport, Some(WINDOW_MS)).await;

        let built = shard
            .build_api_responses_from_messages(messages.clone(), build_options())
            .await
            .unwrap();
        let mut built_ids = ids(&built);
        built_ids.sort_unstable_by_key(|id| std::cmp::Reverse(*id));
        assert_eq!(built_ids, visible);
        assert!(built_ids.iter().all(|id| !hidden.contains(id)));
        users.abort();

        let shard = memory_shard(messages.clone());
        let users = user_service(&shard.transport, None).await;
        let restored = shard
            .build_api_responses_from_messages(messages, build_options())
            .await
            .unwrap();
        assert_eq!(restored.len(), visible.len() + hidden.len());
        users.abort();
    }

    #[tokio::test]
    async fn replies_lose_their_preview_and_forwards_vanish_when_the_source_is_hidden() {
        let (mut messages, hidden, _) = channel_history();
        let reply_id = snowflake_at(WINDOW_MS + 20_000, 0);
        let forward_id = snowflake_at(WINDOW_MS + 21_000, 0);
        let reference = |kind: i32| {
            json!({"message_reference": {
                "channel_id": {"__fluxer_type": "bigint", "value": CHANNEL.to_string()},
                "message_id": {"__fluxer_type": "bigint", "value": hidden[1].to_string()},
                "type": kind,
            }})
        };
        messages.push(message_by(OTHER_AUTHOR, reply_id, reference(0)));
        let mut forward = reference(1);
        forward["message_snapshots"] = json!([{"content": "copied", "type": 0}]);
        forward["content"] = json!("");
        messages.push(message_by(OTHER_AUTHOR, forward_id, forward));
        let shard = memory_shard(messages);
        let users = user_service(&shard.transport, Some(WINDOW_MS)).await;

        let reply = shard
            .get_api_response(CHANNEL, reply_id, build_options())
            .await
            .unwrap()
            .unwrap();
        let reply = serde_json::to_value(&reply).unwrap();
        assert!(reply["referenced_message"].is_null());
        assert_eq!(
            reply["message_reference"]["message_id"],
            hidden[1].to_string()
        );
        assert!(
            shard
                .get_api_response(CHANNEL, forward_id, build_options())
                .await
                .unwrap()
                .is_none()
        );
        let latest = shard
            .list_api_responses(CHANNEL, 2, None, None, None, build_options())
            .await
            .unwrap();
        assert_eq!(ids(&latest)[0], reply_id);
        assert!(!ids(&latest).contains(&forward_id));
        users.abort();
    }
}
