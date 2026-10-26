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
    ApiReactionEmojiResponse, ApiUserPartial, Message, MessageAttachment, MessageCall,
    MessageEmbed, MessageEmbedAuthor, MessageEmbedChild, MessageEmbedField, MessageEmbedFooter,
    MessageEmbedMedia, MessageEmbedProvider, MessageReference, MessageRequest, MessageResponse,
    MessageSnapshot, MessageStickerItem,
};
use crate::udt;
use base64::prelude::{BASE64_STANDARD, Engine};
use chrono::{DateTime, Utc};
use fluxer_common::time::{unix_millis, unix_seconds};
use fluxer_common::user_partial::{FLUXER_SYSTEM_USER_ID, UserPartialFields, fluxer_system_user};
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
    #[serde(default)]
    hidden_profile_badges: Vec<String>,
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
    attachment_decay: bool,
    instance_name: String,
}

impl ResponseBuildOptions {
    fn expiring_attachment_ids(&self, messages: &[&Message]) -> HashSet<i64> {
        if self.attachment_decay {
            collect_attachment_ids(messages)
        } else {
            HashSet::new()
        }
    }

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
    users: HashMap<i64, ApiUserPartial>,
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
                .storage
                .list_buckets(
                    channel_id,
                    min_bucket,
                    cursor_max,
                    bucket_page_limit(limit),
                    true,
                )
                .await?;
            let Some(last_bucket) = buckets.last().copied() else {
                break;
            };
            collect_bucket_waves(&buckets, limit as usize, &mut messages, |bucket| {
                self.storage.fetch_bucket(
                    channel_id,
                    bucket,
                    before_id.map(BucketBound::Before),
                    limit_i32,
                )
            })
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
                .storage
                .list_buckets(
                    channel_id,
                    cursor_min,
                    max_bucket,
                    bucket_page_limit(limit),
                    false,
                )
                .await?;
            let Some(last_bucket) = buckets.last().copied() else {
                break;
            };
            collect_bucket_waves(&buckets, limit as usize, &mut messages, |bucket| {
                self.storage.fetch_bucket(
                    channel_id,
                    bucket,
                    Some(BucketBound::After(after_id)),
                    limit_i32,
                )
            })
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
        self.load_user_partials(user_ids, &options.instance_name, users)
            .await;
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
        let attachment_ids = options.expiring_attachment_ids(&all_messages);
        let channel_ids = collect_channel_mention_ids(&all_messages, &mention_context);
        let user_ids = collect_user_ids(&all_messages, &mention_context);
        let reactions_future = self.fetch_reactions_for_messages(messages, options);
        let attachment_decay_future = self.fetch_attachment_decay(attachment_ids);
        let channel_mentions_future = self.resolve_channel_mentions(channel_ids, options);
        let users_future = self.load_user_partials(user_ids, &options.instance_name, &mut users);
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

    async fn load_user_partials(
        &self,
        user_ids: HashSet<i64>,
        instance_name: &str,
        users: &mut UserLookup,
    ) {
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
            "instance_name": instance_name,
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
        let author = self.resolve_author(message, options, context);
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
        options: &ResponseBuildOptions,
        context: &ResponseContext,
    ) -> ApiUserPartial {
        if let Some(author_id) = message.author_id {
            return context
                .users
                .get(&author_id)
                .cloned()
                .unwrap_or_else(|| deleted_user(author_id, &options.instance_name));
        }
        if let (Some(webhook_id), Some(webhook_name)) =
            (message.webhook_id, message.webhook_name.clone())
        {
            return ApiUserPartial {
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
        deleted_user(0, &options.instance_name)
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
            decay.is_some_and(|expires_at| expires_at.timestamp_millis() <= unix_millis());
        let url = (!expired).then(|| {
            attachment_cdn_url(
                channel_id,
                attachment_id,
                &filename,
                options,
                unix_seconds(),
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
        }
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
        match self {
            MessagesStorage::Postgres(storage) => {
                storage
                    .list_buckets(channel_id, min_bucket, max_bucket, limit, desc)
                    .await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage
                    .list_buckets(channel_id, min_bucket, max_bucket, limit, desc)
                    .await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) => Ok(Vec::new()),
        }
    }

    async fn fetch_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        bound: Option<BucketBound>,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        match self {
            MessagesStorage::Postgres(storage) => {
                let desc = !matches!(bound, Some(BucketBound::After(_)));
                storage
                    .fetch_bucket(channel_id, bucket, bound, desc, limit)
                    .await
            }
            #[cfg(feature = "scylla")]
            MessagesStorage::Scylla(storage) => {
                storage.fetch_bucket(channel_id, bucket, bound, limit).await
            }
            #[cfg(test)]
            MessagesStorage::Deletions(_) => Ok(Vec::new()),
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
            MessagesStorage::Deletions(_) => HashMap::new(),
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
            MessagesStorage::Deletions(_) => Ok(HashMap::new()),
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

    async fn list_buckets(
        &self,
        channel_id: i64,
        min_bucket: i32,
        max_bucket: i32,
        limit: u32,
        desc: bool,
    ) -> anyhow::Result<Vec<i32>> {
        let stmt = if desc {
            &self.stmt_list_buckets_desc
        } else {
            &self.stmt_list_buckets_asc
        };
        let result = self
            .db
            .execute_unpaged(
                stmt,
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

    async fn fetch_bucket(
        &self,
        channel_id: i64,
        bucket: i32,
        bound: Option<BucketBound>,
        limit: i32,
    ) -> anyhow::Result<Vec<Message>> {
        let result = match bound {
            None => {
                self.db
                    .execute_unpaged(&self.stmt_get_latest, (channel_id, bucket, limit))
                    .await?
            }
            Some(BucketBound::Before(before_id)) => {
                self.db
                    .execute_unpaged(
                        &self.stmt_get_before,
                        (channel_id, bucket, before_id, limit),
                    )
                    .await?
            }
            Some(BucketBound::After(after_id)) => {
                self.db
                    .execute_unpaged(&self.stmt_get_after, (channel_id, bucket, after_id, limit))
                    .await?
            }
        };
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
                attachment_decay,
                nonce,
                tts,
                include_hidden,
                threads_mask,
                instance_name,
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
                            attachment_decay: attachment_decay.unwrap_or(true),
                            nonce,
                            tts: tts.unwrap_or(false),
                            include_hidden,
                            threads_mask,
                            excluded_types: Vec::new(),
                            instance_name,
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
                attachment_decay,
                nonce,
                tts,
                include_hidden,
                threads_mask,
                instance_name,
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
                            attachment_decay: attachment_decay.unwrap_or(true),
                            nonce,
                            tts: tts.unwrap_or(false),
                            include_hidden,
                            threads_mask,
                            excluded_types: Vec::new(),
                            instance_name,
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
                attachment_decay,
                include_hidden,
                threads_mask,
                instance_name,
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
                            attachment_decay: attachment_decay.unwrap_or(true),
                            nonce: None,
                            tts: false,
                            include_hidden,
                            threads_mask,
                            excluded_types: Vec::new(),
                            instance_name,
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
                attachment_decay,
                include_hidden,
                threads_mask,
                mut exclude_types,
                instance_name,
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
                            attachment_decay: attachment_decay.unwrap_or(true),
                            nonce: None,
                            tts: false,
                            include_hidden,
                            threads_mask,
                            excluded_types: exclude_types,
                            instance_name,
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
    epoch_millis_to_bucket(unix_millis())
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

fn map_user_partial(partial: UserPartialServiceResponse) -> ApiUserPartial {
    ApiUserPartial::from(UserPartialFields {
        user_id: partial.user_id,
        username: partial.username,
        discriminator: partial.discriminator,
        global_name: partial.global_name,
        avatar_hash: partial.avatar_hash,
        avatar_color: partial.avatar_color,
        bot: partial.bot,
        system: partial.system,
        flags: partial.flags,
        hidden_profile_badges: partial.hidden_profile_badges,
        mention_flags: partial.mention_flags,
    })
}

fn deleted_user(user_id: i64, instance_name: &str) -> ApiUserPartial {
    if user_id == FLUXER_SYSTEM_USER_ID {
        return fluxer_system_user(instance_name.to_owned());
    }
    ApiUserPartial {
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
        unix_seconds(),
        &options.attachment_url_secret,
    )
}

fn external_media_proxy_url(input_url: &str, options: &ResponseBuildOptions) -> String {
    media_proxy_url_at(input_url, options, unix_seconds())
}

fn media_proxy_url_at(input_url: &str, options: &ResponseBuildOptions, now_secs: u64) -> String {
    let media_endpoint = options.media_endpoint.trim_end_matches('/');
    let parsed_url = match Url::parse(input_url) {
        Ok(url) => url,
        Err(_) => return input_url.to_owned(),
    };
    if fluxer_common::external_media_path::is_own_media_url(media_endpoint, &parsed_url) {
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
    use fluxer_svc::transport::InMemoryTransport;
    use serde_json::json;

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
            attachment_decay: true,
            instance_name: "Fluxer".to_owned(),
        }
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
    fn a_decayed_attachment_is_never_signed() {
        let deleted = DeletedMessageKeys::default();
        let shard = recording_shard(&deleted);
        let options = signing_options();
        let context = ResponseContext {
            attachment_decay: [(
                SIGNED_ATTACHMENT_ID,
                DateTime::<Utc>::from_timestamp_millis(unix_millis() - 1_000).unwrap(),
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
}
