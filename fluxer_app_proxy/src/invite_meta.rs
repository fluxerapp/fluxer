// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::AppProxyConfig;
#[cfg(feature = "scylla")]
use anyhow::Context;
use fluxer_svc::config::DatabaseBackend;
use fluxer_svc::{postgres, postgres::KeyPart};
use moka::future::Cache;
#[cfg(feature = "scylla")]
use scylla::DeserializeRow;
#[cfg(feature = "scylla")]
use scylla::client::session::Session;
#[cfg(feature = "scylla")]
use scylla::statement::prepared::PreparedStatement;
use serde::Deserialize;
use std::collections::HashSet;
use std::sync::{Arc, OnceLock};
use std::time::Duration;
use tokio::task::JoinHandle;

const INVITE_TYPE_GUILD: i32 = 0;
const INVITE_TYPE_GROUP_DM: i32 = 1;
const CHANNEL_TYPE_GROUP_DM: i32 = 3;
const MEDIA_SIZE_DEFAULT: i32 = 160;
const DEFAULT_AVATAR_COUNT: i64 = 6;
const CONNECT_RETRY_BASE: Duration = Duration::from_secs(5);
const CONNECT_RETRY_MAX: Duration = Duration::from_secs(60);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InvitePageMeta {
    pub title: String,
    pub description: String,
    pub image_url: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub struct InviteMetaEndpoints {
    pub media_endpoint: Option<String>,
    pub static_cdn_endpoint: Option<String>,
}

pub struct InviteMetaResolver {
    storage: InviteMetaStorage,
    cache: Cache<String, Option<InvitePageMeta>>,
}

enum InviteMetaStorage {
    Postgres(PostgresInviteMetaStorage),
    #[cfg(feature = "scylla")]
    Scylla(Box<ScyllaInviteMetaStorage>),
}

struct PostgresInviteMetaStorage {
    kv: postgres::KvClient,
}

#[cfg(feature = "scylla")]
struct ScyllaInviteMetaStorage {
    db: Arc<Session>,
    stmt_invite: PreparedStatement,
    stmt_guild: PreparedStatement,
    stmt_channel: PreparedStatement,
    stmt_user: PreparedStatement,
}

#[cfg_attr(feature = "scylla", derive(DeserializeRow))]
#[derive(Debug, Deserialize)]
struct InviteDbRow {
    r#type: i32,
    guild_id: Option<i64>,
    channel_id: Option<i64>,
    inviter_id: Option<i64>,
}

#[cfg_attr(feature = "scylla", derive(DeserializeRow))]
#[derive(Debug, Deserialize)]
struct GuildDbRow {
    guild_id: i64,
    name: String,
    icon_hash: Option<String>,
    member_count: Option<i32>,
}

#[cfg_attr(feature = "scylla", derive(DeserializeRow))]
#[derive(Debug, Deserialize)]
struct ChannelDbRow {
    channel_id: i64,
    r#type: i32,
    name: Option<String>,
    icon_hash: Option<String>,
    recipient_ids: Option<HashSet<i64>>,
}

#[cfg_attr(feature = "scylla", derive(DeserializeRow))]
#[derive(Debug, Deserialize)]
struct UserDbRow {
    user_id: i64,
    username: String,
    discriminator: i32,
    global_name: Option<String>,
    avatar_hash: Option<String>,
}

impl InviteMetaResolver {
    pub async fn connect(config: &AppProxyConfig) -> anyhow::Result<Self> {
        match config.database_backend {
            DatabaseBackend::Postgres => {
                let postgres_config = fluxer_svc::postgres::PostgresConfig {
                    url: config.postgres_url.clone(),
                    host: config.postgres_host.clone(),
                    port: config.postgres_port,
                    database: config.postgres_database.clone(),
                    username: config.postgres_username.clone(),
                    password: config.postgres_password.clone(),
                    ssl: config.postgres_ssl,
                    ssl_ca: config.postgres_ssl_ca.clone(),
                    max_connections: config.postgres_max_connections,
                    kv_table: config.postgres_kv_table.clone(),
                    prepared_statements: config.postgres_prepared_statements,
                };
                let pool = fluxer_svc::postgres::connect(&postgres_config).await?;
                let kv = postgres::KvClient::new(pool, &postgres_config)?;
                Self::new_postgres(kv, config)
            }
            DatabaseBackend::Cassandra => {
                #[cfg(feature = "scylla")]
                {
                    let db = fluxer_svc::scylla::connect(&fluxer_svc::scylla::ScyllaConfig {
                        hosts: config.scylla_hosts.clone(),
                        keyspace: config.scylla_keyspace.clone(),
                        username: config.scylla_username.clone(),
                        password: config.scylla_password.clone(),
                    })
                    .await?;

                    Self::new_scylla(db, config).await
                }
                #[cfg(not(feature = "scylla"))]
                {
                    anyhow::bail!("FLUXER_DATABASE_BACKEND=cassandra requires the scylla feature");
                }
            }
        }
    }

    fn new_postgres(kv: postgres::KvClient, config: &AppProxyConfig) -> anyhow::Result<Self> {
        let cache = invite_meta_cache(config);

        Ok(Self {
            storage: InviteMetaStorage::Postgres(PostgresInviteMetaStorage { kv }),
            cache,
        })
    }

    #[cfg(feature = "scylla")]
    async fn new_scylla(db: Arc<Session>, config: &AppProxyConfig) -> anyhow::Result<Self> {
        let stmt_invite = db
            .prepare(
                "SELECT type, guild_id, channel_id, inviter_id \
                 FROM invites WHERE code = ? LIMIT 1",
            )
            .await
            .context("failed to prepare invite lookup")?;
        let stmt_guild = db
            .prepare(
                "SELECT guild_id, name, icon_hash, member_count \
                 FROM guilds WHERE guild_id = ? LIMIT 1",
            )
            .await
            .context("failed to prepare guild lookup")?;
        let stmt_channel = db
            .prepare(
                "SELECT channel_id, type, name, icon_hash, recipient_ids \
                 FROM channels WHERE channel_id = ? AND soft_deleted = false LIMIT 1",
            )
            .await
            .context("failed to prepare channel lookup")?;
        let stmt_user = db
            .prepare(
                "SELECT user_id, username, discriminator, global_name, avatar_hash \
                 FROM users WHERE user_id = ? LIMIT 1",
            )
            .await
            .context("failed to prepare user lookup")?;

        let cache = invite_meta_cache(config);

        Ok(Self {
            storage: InviteMetaStorage::Scylla(Box::new(ScyllaInviteMetaStorage {
                db,
                stmt_invite,
                stmt_guild,
                stmt_channel,
                stmt_user,
            })),
            cache,
        })
    }

    pub async fn resolve(
        &self,
        code: &str,
        endpoints: &InviteMetaEndpoints,
    ) -> anyhow::Result<Option<InvitePageMeta>> {
        let cache_key = format!(
            "{}\n{}\n{}",
            code,
            endpoints.media_endpoint.as_deref().unwrap_or_default(),
            endpoints.static_cdn_endpoint.as_deref().unwrap_or_default()
        );
        if let Some(cached) = self.cache.get(&cache_key).await {
            return Ok(cached);
        }

        let meta = self.resolve_uncached(code, endpoints).await?;
        self.cache.insert(cache_key, meta.clone()).await;
        Ok(meta)
    }

    async fn resolve_uncached(
        &self,
        code: &str,
        endpoints: &InviteMetaEndpoints,
    ) -> anyhow::Result<Option<InvitePageMeta>> {
        let Some(invite) = self.fetch_invite(code).await? else {
            return Ok(None);
        };

        match invite.r#type {
            INVITE_TYPE_GUILD => self.build_guild_meta(&invite, endpoints).await,
            INVITE_TYPE_GROUP_DM => self.build_group_dm_meta(&invite, endpoints).await,
            _ => Ok(None),
        }
    }

    async fn build_guild_meta(
        &self,
        invite: &InviteDbRow,
        endpoints: &InviteMetaEndpoints,
    ) -> anyhow::Result<Option<InvitePageMeta>> {
        let Some(guild_id) = invite.guild_id else {
            return Ok(None);
        };
        let Some(guild) = self.fetch_guild(guild_id).await? else {
            return Ok(None);
        };
        let channel = match invite.channel_id {
            Some(channel_id) => self.fetch_channel(channel_id).await?,
            None => None,
        };
        let inviter = match invite.inviter_id {
            Some(inviter_id) => self.fetch_user(inviter_id).await?,
            None => None,
        };

        Ok(Some(build_guild_meta(
            &guild,
            channel.as_ref(),
            inviter.as_ref(),
            endpoints,
        )))
    }

    async fn build_group_dm_meta(
        &self,
        invite: &InviteDbRow,
        endpoints: &InviteMetaEndpoints,
    ) -> anyhow::Result<Option<InvitePageMeta>> {
        let Some(channel_id) = invite.channel_id else {
            return Ok(None);
        };
        let Some(channel) = self.fetch_channel(channel_id).await? else {
            return Ok(None);
        };
        if channel.r#type != CHANNEL_TYPE_GROUP_DM {
            return Ok(None);
        }
        let inviter = match invite.inviter_id {
            Some(inviter_id) => self.fetch_user(inviter_id).await?,
            None => None,
        };

        Ok(Some(build_group_dm_meta(
            &channel,
            inviter.as_ref(),
            endpoints,
        )))
    }

    async fn fetch_invite(&self, code: &str) -> anyhow::Result<Option<InviteDbRow>> {
        self.storage.fetch_invite(code).await
    }

    async fn fetch_guild(&self, guild_id: i64) -> anyhow::Result<Option<GuildDbRow>> {
        self.storage.fetch_guild(guild_id).await
    }

    async fn fetch_channel(&self, channel_id: i64) -> anyhow::Result<Option<ChannelDbRow>> {
        self.storage.fetch_channel(channel_id).await
    }

    async fn fetch_user(&self, user_id: i64) -> anyhow::Result<Option<UserDbRow>> {
        self.storage.fetch_user(user_id).await
    }
}

pub fn start_background_connect(
    slot: &Arc<OnceLock<InviteMetaResolver>>,
    config: Arc<AppProxyConfig>,
) -> JoinHandle<()> {
    let slot = Arc::clone(slot);
    tokio::spawn(async move {
        let mut failures: u32 = 0;
        loop {
            match InviteMetaResolver::connect(&config).await {
                Ok(resolver) => {
                    let _ = slot.set(resolver);
                    tracing::info!("invite metadata resolver connected");
                    return;
                }
                Err(err) => {
                    failures = failures.saturating_add(1);
                    let backoff = connect_backoff(failures);
                    tracing::warn!(
                        %err,
                        failures,
                        backoff_ms = backoff.as_millis() as u64,
                        "invite metadata resolver connect failed; retrying"
                    );
                    tokio::time::sleep(backoff).await;
                }
            }
        }
    })
}

fn connect_backoff(failures: u32) -> Duration {
    CONNECT_RETRY_BASE
        .saturating_mul(1u32 << failures.min(5))
        .min(CONNECT_RETRY_MAX)
}

fn invite_meta_cache(config: &AppProxyConfig) -> Cache<String, Option<InvitePageMeta>> {
    Cache::builder()
        .max_capacity(config.invite_meta_cache_max_entries)
        .time_to_live(Duration::from_millis(config.invite_meta_cache_ttl_ms))
        .build()
}

impl InviteMetaStorage {
    async fn fetch_invite(&self, code: &str) -> anyhow::Result<Option<InviteDbRow>> {
        match self {
            InviteMetaStorage::Postgres(storage) => storage.fetch_invite(code).await,
            #[cfg(feature = "scylla")]
            InviteMetaStorage::Scylla(storage) => storage.fetch_invite(code).await,
        }
    }

    async fn fetch_guild(&self, guild_id: i64) -> anyhow::Result<Option<GuildDbRow>> {
        match self {
            InviteMetaStorage::Postgres(storage) => storage.fetch_guild(guild_id).await,
            #[cfg(feature = "scylla")]
            InviteMetaStorage::Scylla(storage) => storage.fetch_guild(guild_id).await,
        }
    }

    async fn fetch_channel(&self, channel_id: i64) -> anyhow::Result<Option<ChannelDbRow>> {
        match self {
            InviteMetaStorage::Postgres(storage) => storage.fetch_channel(channel_id).await,
            #[cfg(feature = "scylla")]
            InviteMetaStorage::Scylla(storage) => storage.fetch_channel(channel_id).await,
        }
    }

    async fn fetch_user(&self, user_id: i64) -> anyhow::Result<Option<UserDbRow>> {
        match self {
            InviteMetaStorage::Postgres(storage) => storage.fetch_user(user_id).await,
            #[cfg(feature = "scylla")]
            InviteMetaStorage::Scylla(storage) => storage.fetch_user(user_id).await,
        }
    }
}

impl PostgresInviteMetaStorage {
    async fn fetch_invite(&self, code: &str) -> anyhow::Result<Option<InviteDbRow>> {
        self.fetch_row("invites", &[KeyPart::String(code)]).await
    }

    async fn fetch_guild(&self, guild_id: i64) -> anyhow::Result<Option<GuildDbRow>> {
        self.fetch_row("guilds", &[KeyPart::BigInt(guild_id)]).await
    }

    async fn fetch_channel(&self, channel_id: i64) -> anyhow::Result<Option<ChannelDbRow>> {
        self.fetch_row(
            "channels",
            &[KeyPart::BigInt(channel_id), KeyPart::Bool(false)],
        )
        .await
    }

    async fn fetch_user(&self, user_id: i64) -> anyhow::Result<Option<UserDbRow>> {
        self.fetch_row("users", &[KeyPart::BigInt(user_id)]).await
    }

    async fn fetch_row<T>(&self, table_name: &str, key: &[KeyPart<'_>]) -> anyhow::Result<Option<T>>
    where
        T: serde::de::DeserializeOwned,
    {
        let key = postgres::kv_key(key)?;
        let Some(row) = self.kv.get_row(table_name, &key).await? else {
            return Ok(None);
        };
        let row = postgres::decode_row_dates_as_millis(row)?;
        Ok(Some(serde_json::from_value(row)?))
    }
}

#[cfg(feature = "scylla")]
impl ScyllaInviteMetaStorage {
    async fn fetch_invite(&self, code: &str) -> anyhow::Result<Option<InviteDbRow>> {
        let result = self.db.execute_unpaged(&self.stmt_invite, (code,)).await?;
        let rows = result.into_rows_result()?;
        Ok(rows.maybe_first_row::<InviteDbRow>()?)
    }

    async fn fetch_guild(&self, guild_id: i64) -> anyhow::Result<Option<GuildDbRow>> {
        let result = self
            .db
            .execute_unpaged(&self.stmt_guild, (guild_id,))
            .await?;
        let rows = result.into_rows_result()?;
        Ok(rows.maybe_first_row::<GuildDbRow>()?)
    }

    async fn fetch_channel(&self, channel_id: i64) -> anyhow::Result<Option<ChannelDbRow>> {
        let result = self
            .db
            .execute_unpaged(&self.stmt_channel, (channel_id,))
            .await?;
        let rows = result.into_rows_result()?;
        Ok(rows.maybe_first_row::<ChannelDbRow>()?)
    }

    async fn fetch_user(&self, user_id: i64) -> anyhow::Result<Option<UserDbRow>> {
        let result = self.db.execute_unpaged(&self.stmt_user, (user_id,)).await?;
        let rows = result.into_rows_result()?;
        Ok(rows.maybe_first_row::<UserDbRow>()?)
    }
}

pub fn invite_code_from_path(path: &str) -> Option<&str> {
    let mut segments = path.trim_start_matches('/').split('/');
    if segments.next()? != "invite" {
        return None;
    }
    let code = segments.next()?;
    if !is_valid_invite_code_segment(code) {
        return None;
    }
    match segments.next() {
        None => Some(code),
        Some("login") if segments.next().is_none() => Some(code),
        _ => None,
    }
}

pub fn inject_invite_meta(html: &str, meta: &InvitePageMeta) -> String {
    let mut html = replace_title(html, &meta.title);
    html = remove_meta_tags(
        &html,
        &[
            MetaSelector::Name("description"),
            MetaSelector::Property("og:title"),
            MetaSelector::Property("og:description"),
            MetaSelector::Property("og:image"),
            MetaSelector::Property("og:type"),
            MetaSelector::Name("twitter:card"),
            MetaSelector::Name("twitter:title"),
            MetaSelector::Name("twitter:description"),
            MetaSelector::Name("twitter:image"),
        ],
    );

    let tags = build_meta_tags(meta);
    insert_head_tags(&html, &tags)
}

fn build_guild_meta(
    guild: &GuildDbRow,
    channel: Option<&ChannelDbRow>,
    inviter: Option<&UserDbRow>,
    endpoints: &InviteMetaEndpoints,
) -> InvitePageMeta {
    let title = format!("Join {} on Fluxer", guild.name);
    let mut description = format!("You've been invited to join {}.", guild.name);
    if let Some(channel_name) =
        channel.and_then(|channel| clean_optional_text(channel.name.as_deref()))
    {
        description.push_str(&format!(" Channel: {channel_name}."));
    }
    if let Some(member_count) = guild.member_count {
        description.push_str(&format!(" {}", format_member_count(member_count as i64)));
    }

    let image_url = guild
        .icon_hash
        .as_deref()
        .and_then(|hash| {
            media_image_url(
                endpoints.media_endpoint.as_deref(),
                "icons",
                guild.guild_id,
                hash,
            )
        })
        .or_else(|| inviter.and_then(|user| user_avatar_url(user, endpoints)));

    InvitePageMeta {
        title,
        description,
        image_url,
    }
}

fn build_group_dm_meta(
    channel: &ChannelDbRow,
    inviter: Option<&UserDbRow>,
    endpoints: &InviteMetaEndpoints,
) -> InvitePageMeta {
    let channel_name = clean_optional_text(channel.name.as_deref());
    let inviter_name = inviter.map(display_user_name);
    let title = if let Some(channel_name) = channel_name {
        format!("Join {channel_name} on Fluxer")
    } else if let Some(inviter_name) = inviter_name.as_deref() {
        format!("Join {inviter_name}'s group DM on Fluxer")
    } else {
        "Join a group DM on Fluxer".to_owned()
    };

    let member_count = channel
        .recipient_ids
        .as_ref()
        .map(|recipients| recipients.len() as i64);
    let mut description = if let Some(inviter_name) = inviter_name {
        format!("{inviter_name} invited you to a group DM.")
    } else {
        "You've been invited to a group DM.".to_owned()
    };
    if let Some(member_count) = member_count {
        description.push_str(&format!(" {}", format_member_count(member_count)));
    }

    let image_url = channel
        .icon_hash
        .as_deref()
        .and_then(|hash| {
            media_image_url(
                endpoints.media_endpoint.as_deref(),
                "icons",
                channel.channel_id,
                hash,
            )
        })
        .or_else(|| inviter.and_then(|user| user_avatar_url(user, endpoints)));

    InvitePageMeta {
        title,
        description,
        image_url,
    }
}

fn user_avatar_url(user: &UserDbRow, endpoints: &InviteMetaEndpoints) -> Option<String> {
    user.avatar_hash
        .as_deref()
        .and_then(|hash| {
            media_image_url(
                endpoints.media_endpoint.as_deref(),
                "avatars",
                user.user_id,
                hash,
            )
        })
        .or_else(|| default_avatar_url(endpoints.static_cdn_endpoint.as_deref(), user.user_id))
}

fn media_image_url(endpoint: Option<&str>, path: &str, id: i64, hash: &str) -> Option<String> {
    let endpoint = clean_endpoint(endpoint?)?;
    let (hash, animated) = parse_media_hash(hash);
    if hash.is_empty() {
        return None;
    }
    let mut url = format!("{endpoint}/{path}/{id}/{hash}.webp?size={MEDIA_SIZE_DEFAULT}");
    if animated {
        url.push_str("&animated=false");
    }
    Some(url)
}

fn default_avatar_url(endpoint: Option<&str>, user_id: i64) -> Option<String> {
    let endpoint = clean_endpoint(endpoint?)?;
    let index = user_id.rem_euclid(DEFAULT_AVATAR_COUNT);
    Some(format!("{endpoint}/avatars/{index}.png"))
}

fn parse_media_hash(value: &str) -> (&str, bool) {
    value
        .strip_prefix("a_")
        .map(|hash| (hash, true))
        .unwrap_or((value, false))
}

fn clean_endpoint(value: &str) -> Option<String> {
    let value = value.trim().trim_end_matches('/');
    if value.is_empty() {
        None
    } else {
        Some(value.to_owned())
    }
}

fn display_user_name(user: &UserDbRow) -> String {
    clean_optional_text(user.global_name.as_deref())
        .map(ToOwned::to_owned)
        .unwrap_or_else(|| {
            if user.discriminator > 0 {
                format!("{}#{:04}", user.username, user.discriminator)
            } else {
                user.username.clone()
            }
        })
}

fn clean_optional_text(value: Option<&str>) -> Option<&str> {
    value.map(str::trim).filter(|value| !value.is_empty())
}

fn format_member_count(value: i64) -> String {
    let noun = if value == 1 { "member" } else { "members" };
    format!("{} {noun}", format_count(value))
}

fn format_count(value: i64) -> String {
    let negative = value < 0;
    let digits = value.abs().to_string();
    let mut out = String::with_capacity(digits.len() + digits.len() / 3 + usize::from(negative));
    if negative {
        out.push('-');
    }
    for (index, ch) in digits.chars().enumerate() {
        if index > 0 && (digits.len() - index).is_multiple_of(3) {
            out.push(',');
        }
        out.push(ch);
    }
    out
}

fn is_valid_invite_code_segment(code: &str) -> bool {
    !code.is_empty()
        && code.len() <= 128
        && code
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_'))
}

fn build_meta_tags(meta: &InvitePageMeta) -> String {
    let title = escape_html_attr(&meta.title);
    let description = escape_html_attr(&meta.description);
    let mut tags = format!(
        r#"<meta name="description" content="{description}">
<meta property="og:title" content="{title}">
<meta property="og:description" content="{description}">
<meta property="og:type" content="website">
<meta name="twitter:card" content="summary">
<meta name="twitter:title" content="{title}">
<meta name="twitter:description" content="{description}">"#
    );
    if let Some(image_url) = &meta.image_url {
        let image = escape_html_attr(image_url);
        tags.push_str(&format!(
            r#"
<meta property="og:image" content="{image}">
<meta name="twitter:image" content="{image}">"#
        ));
    }
    tags
}

fn replace_title(html: &str, title: &str) -> String {
    let escaped_title = escape_html_text(title);
    let lower = html.to_ascii_lowercase();
    let Some(start) = lower.find("<title>") else {
        return html.to_owned();
    };
    let content_start = start + "<title>".len();
    let Some(relative_end) = lower[content_start..].find("</title>") else {
        return html.to_owned();
    };
    let content_end = content_start + relative_end;

    let mut result = String::with_capacity(html.len() + escaped_title.len());
    result.push_str(&html[..content_start]);
    result.push_str(&escaped_title);
    result.push_str(&html[content_end..]);
    result
}

#[derive(Clone, Copy)]
enum MetaSelector {
    Name(&'static str),
    Property(&'static str),
}

fn remove_meta_tags(html: &str, selectors: &[MetaSelector]) -> String {
    let lower = html.to_ascii_lowercase();
    let mut result = String::with_capacity(html.len());
    let mut cursor = 0;

    while let Some(relative_start) = lower[cursor..].find("<meta") {
        let start = cursor + relative_start;
        let Some(relative_end) = lower[start..].find('>') else {
            break;
        };
        let end = start + relative_end + 1;
        let tag_lower = &lower[start..end];
        if selectors
            .iter()
            .any(|selector| meta_tag_matches(tag_lower, *selector))
        {
            result.push_str(&html[cursor..start]);
        } else {
            result.push_str(&html[cursor..end]);
        }
        cursor = end;
    }

    result.push_str(&html[cursor..]);
    result
}

fn meta_tag_matches(tag_lower: &str, selector: MetaSelector) -> bool {
    match selector {
        MetaSelector::Name(value) => attr_matches(tag_lower, "name", value),
        MetaSelector::Property(value) => attr_matches(tag_lower, "property", value),
    }
}

fn attr_matches(tag_lower: &str, attr: &str, value: &str) -> bool {
    tag_lower.contains(&format!(r#"{attr}="{value}""#))
        || tag_lower.contains(&format!("{attr}='{value}'"))
}

fn insert_head_tags(html: &str, tags: &str) -> String {
    let lower = html.to_ascii_lowercase();
    if let Some(title_end) = lower.find("</title>") {
        let insert_at = title_end + "</title>".len();
        return insert_at_pos(html, insert_at, tags);
    }
    if let Some(head_start) = lower
        .find("<head>")
        .map(|pos| pos + "<head>".len())
        .or_else(|| {
            lower
                .find("<head ")
                .and_then(|pos| lower[pos..].find('>').map(|end| pos + end + 1))
        })
    {
        return insert_at_pos(html, head_start, tags);
    }
    html.to_owned()
}

fn insert_at_pos(html: &str, insert_at: usize, tags: &str) -> String {
    let mut result = String::with_capacity(html.len() + tags.len() + 2);
    result.push_str(&html[..insert_at]);
    result.push('\n');
    result.push_str(tags);
    result.push_str(&html[insert_at..]);
    result
}

fn escape_html_text(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

fn escape_html_attr(value: &str) -> String {
    escape_html_text(value)
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_connect_backoff_grows_and_stops_at_the_ceiling() {
        assert_eq!(connect_backoff(1), Duration::from_secs(10));
        assert_eq!(connect_backoff(2), Duration::from_secs(20));
        assert_eq!(connect_backoff(3), Duration::from_secs(40));
        assert_eq!(connect_backoff(4), CONNECT_RETRY_MAX);
        assert_eq!(connect_backoff(64), CONNECT_RETRY_MAX);
    }

    #[tokio::test]
    async fn a_refused_database_never_fills_the_slot_and_never_gives_up() {
        let mut config = AppProxyConfig::from_env();
        config.database_backend = DatabaseBackend::Postgres;
        config.postgres_url = None;
        config.postgres_host = "127.0.0.1".to_owned();
        config.postgres_port = 1;
        config.postgres_ssl = false;
        let slot = Arc::new(OnceLock::new());

        let handle = start_background_connect(&slot, Arc::new(config));
        tokio::time::sleep(Duration::from_millis(200)).await;

        assert!(slot.get().is_none());
        assert!(
            !handle.is_finished(),
            "a failed connect ended the retry loop and disabled invite metadata for the process"
        );
        handle.abort();
    }

    fn endpoints() -> InviteMetaEndpoints {
        InviteMetaEndpoints {
            media_endpoint: Some("https://media.example.test/media/".to_owned()),
            static_cdn_endpoint: Some("https://static.example.test/".to_owned()),
        }
    }

    #[test]
    fn extracts_invite_code_from_register_and_login_paths() {
        assert_eq!(invite_code_from_path("/invite/abc123"), Some("abc123"));
        assert_eq!(
            invite_code_from_path("/invite/abc123/login"),
            Some("abc123")
        );
        assert_eq!(invite_code_from_path("/channels/@me"), None);
        assert_eq!(invite_code_from_path("/invite/bad.code"), None);
        assert_eq!(invite_code_from_path("/invite/abc123/settings"), None);
    }

    #[test]
    fn builds_guild_meta_with_icon_and_member_count() {
        let guild = GuildDbRow {
            guild_id: 42,
            name: "Rust Friends".to_owned(),
            icon_hash: Some("a_iconhash".to_owned()),
            member_count: Some(12345),
        };
        let channel = ChannelDbRow {
            channel_id: 7,
            r#type: 0,
            name: Some("general".to_owned()),
            icon_hash: None,
            recipient_ids: None,
        };

        let meta = build_guild_meta(&guild, Some(&channel), None, &endpoints());

        assert_eq!(meta.title, "Join Rust Friends on Fluxer");
        assert!(meta.description.contains("Channel: general."));
        assert!(meta.description.contains("12,345 members"));
        assert_eq!(
            meta.image_url.as_deref(),
            Some("https://media.example.test/media/icons/42/iconhash.webp?size=160&animated=false")
        );
    }

    #[test]
    fn builds_group_dm_meta_with_inviter_avatar_fallback() {
        let channel = ChannelDbRow {
            channel_id: 55,
            r#type: CHANNEL_TYPE_GROUP_DM,
            name: None,
            icon_hash: None,
            recipient_ids: Some(HashSet::from([1, 2, 3])),
        };
        let inviter = UserDbRow {
            user_id: 99,
            username: "ada".to_owned(),
            discriminator: 7,
            global_name: Some("Ada".to_owned()),
            avatar_hash: Some("avatarhash".to_owned()),
        };

        let meta = build_group_dm_meta(&channel, Some(&inviter), &endpoints());

        assert_eq!(meta.title, "Join Ada's group DM on Fluxer");
        assert!(meta.description.contains("3 members"));
        assert_eq!(
            meta.image_url.as_deref(),
            Some("https://media.example.test/media/avatars/99/avatarhash.webp?size=160")
        );
    }

    #[test]
    fn injects_invite_meta_and_removes_default_description() {
        let html = r#"<html><head><title>Fluxer</title><meta name="description" content="Default"><meta property="og:title" content="Old"></head><body></body></html>"#;
        let meta = InvitePageMeta {
            title: "Join A & B".to_owned(),
            description: "A < B \"test\"".to_owned(),
            image_url: Some("https://cdn.example.test/icon.png".to_owned()),
        };

        let result = inject_invite_meta(html, &meta);

        assert!(result.contains("<title>Join A &amp; B</title>"));
        assert!(!result.contains("content=\"Default\""));
        assert!(!result.contains("content=\"Old\""));
        assert!(result.contains(r#"name="description" content="A &lt; B &quot;test&quot;""#));
        assert!(
            result.contains(r#"property="og:image" content="https://cdn.example.test/icon.png""#)
        );
    }
}
