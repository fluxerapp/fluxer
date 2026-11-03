// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::{ApiUserPartial, UserPartial, UserRequest, UserResponse};
#[cfg(feature = "scylla")]
use chrono::{DateTime, Utc};
use fluxer_common::time::unix_millis;
use fluxer_common::user_flags::{AccountStanding, USER_FLAG_STAFF};
use fluxer_common::user_partial::{FLUXER_SYSTEM_DISCRIMINATOR, FLUXER_SYSTEM_USER_ID};
use fluxer_svc::shard::ShardService;
use fluxer_svc::{postgres, postgres::KeyPart};
use futures::stream::{self, StreamExt};
use moka::future::Cache;
#[cfg(feature = "scylla")]
use scylla::DeserializeRow;
#[cfg(feature = "scylla")]
use scylla::client::session::Session;
#[cfg(feature = "scylla")]
use scylla::statement::prepared::PreparedStatement;
#[cfg(feature = "scylla")]
use scylla::value::MaybeEmpty;
use serde::Deserialize;
use std::collections::{HashMap, hash_map::DefaultHasher};
use std::fmt::Write;
use std::future::Future;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

#[cfg(feature = "scylla")]
type OptionalTimestamp = Option<MaybeEmpty<DateTime<Utc>>>;

#[cfg(feature = "scylla")]
const PARTIAL_USER_COLUMNS: &str = "\
    user_id, username, discriminator, global_name, \
    avatar_hash, bot, system, flags, \
    banner_hash, banner_color, accent_color, avatar_color, \
    mention_flags, temp_banned_until, pending_deletion_at, deletion_reason_code, \
    content_hidden_since, hidden_profile_badges";
const USER_BATCH_SIZE: usize = 128;
const USER_BATCH_CONCURRENCY: usize = 8;
const USER_CACHE_MIN_GENERATION_STRIPES: usize = 4096;
const USER_CACHE_MAX_GENERATION_STRIPES: usize = 1 << 20;

pub struct UsersShard {
    storage: UsersStorage,
    caches: UserCaches,
}

struct UserCaches {
    partial: Cache<UserCacheKey, Option<UserPartial>>,
    generations: Box<[AtomicU64]>,
    generation_bumps: AtomicU64,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct UserCacheKey {
    user_id: i64,
    generation: u64,
}

#[derive(Clone)]
enum UsersStorage {
    Postgres(PostgresUsersStorage),
    #[cfg(feature = "scylla")]
    Scylla(Arc<ScyllaUsersStorage>),
}

#[derive(Clone)]
struct PostgresUsersStorage {
    kv: postgres::KvClient,
}

#[cfg(feature = "scylla")]
struct ScyllaUsersStorage {
    db: Arc<Session>,
    stmt_partial: PreparedStatement,
    stmt_partial_batch: PreparedStatement,
}

#[derive(Debug, Clone, Deserialize)]
struct PartialUserDbRow {
    user_id: i64,
    username: String,
    discriminator: i32,
    global_name: Option<String>,
    avatar_hash: Option<String>,
    bot: Option<bool>,
    system: Option<bool>,
    flags: Option<i64>,
    banner_hash: Option<String>,
    banner_color: Option<i32>,
    accent_color: Option<i32>,
    avatar_color: Option<i32>,
    mention_flags: Option<i32>,
    #[serde(default)]
    temp_banned_until: Option<i64>,
    #[serde(default)]
    pending_deletion_at: Option<i64>,
    #[serde(default)]
    deletion_reason_code: Option<i32>,
    #[serde(default)]
    content_hidden_since: Option<i64>,
    #[serde(default)]
    hidden_profile_badges: Option<Vec<String>>,
}

#[cfg(feature = "scylla")]
#[derive(Debug, DeserializeRow)]
struct PartialUserScyllaRow {
    user_id: i64,
    username: String,
    discriminator: i32,
    global_name: Option<String>,
    avatar_hash: Option<String>,
    bot: Option<bool>,
    system: Option<bool>,
    flags: Option<i64>,
    banner_hash: Option<String>,
    banner_color: Option<i32>,
    accent_color: Option<i32>,
    avatar_color: Option<i32>,
    mention_flags: Option<i32>,
    temp_banned_until: OptionalTimestamp,
    pending_deletion_at: OptionalTimestamp,
    deletion_reason_code: Option<i32>,
    content_hidden_since: OptionalTimestamp,
    hidden_profile_badges: Option<Vec<String>>,
}

#[cfg(feature = "scylla")]
impl From<PartialUserScyllaRow> for PartialUserDbRow {
    fn from(row: PartialUserScyllaRow) -> Self {
        Self {
            user_id: row.user_id,
            username: row.username,
            discriminator: row.discriminator,
            global_name: row.global_name,
            avatar_hash: row.avatar_hash,
            bot: row.bot,
            system: row.system,
            flags: row.flags,
            banner_hash: row.banner_hash,
            banner_color: row.banner_color,
            accent_color: row.accent_color,
            avatar_color: row.avatar_color,
            mention_flags: row.mention_flags,
            temp_banned_until: optional_timestamp_millis(row.temp_banned_until),
            pending_deletion_at: optional_timestamp_millis(row.pending_deletion_at),
            deletion_reason_code: row.deletion_reason_code,
            content_hidden_since: optional_timestamp_millis(row.content_hidden_since),
            hidden_profile_badges: row.hidden_profile_badges,
        }
    }
}

fn generation_stripes(max_entries: u64) -> usize {
    usize::try_from(max_entries)
        .unwrap_or(USER_CACHE_MAX_GENERATION_STRIPES)
        .clamp(
            USER_CACHE_MIN_GENERATION_STRIPES,
            USER_CACHE_MAX_GENERATION_STRIPES,
        )
        .next_power_of_two()
}

impl UserCaches {
    fn new(max_entries: u64, ttl: Duration) -> Self {
        Self {
            partial: Cache::builder()
                .max_capacity(max_entries)
                .time_to_live(ttl)
                .build(),
            generations: (0..generation_stripes(max_entries))
                .map(|_| AtomicU64::new(0))
                .collect(),
            generation_bumps: AtomicU64::new(0),
        }
    }

    fn generation(&self, user_id: i64) -> &AtomicU64 {
        let mut hasher = DefaultHasher::new();
        user_id.hash(&mut hasher);
        &self.generations[hasher.finish() as usize % self.generations.len()]
    }

    fn key(&self, user_id: i64) -> UserCacheKey {
        UserCacheKey {
            user_id,
            generation: self.generation(user_id).load(Ordering::SeqCst),
        }
    }

    async fn get_or_fetch_partial<F>(
        &self,
        user_id: i64,
        fetch: F,
    ) -> anyhow::Result<Option<UserPartial>>
    where
        F: Future<Output = anyhow::Result<Option<UserPartial>>>,
    {
        self.partial
            .try_get_with(self.key(user_id), fetch)
            .await
            .map_err(|e: Arc<anyhow::Error>| anyhow::anyhow!("{e}"))
    }

    async fn get_partial(&self, user_id: i64) -> Option<Option<UserPartial>> {
        self.partial.get(&self.key(user_id)).await
    }

    async fn insert_partial(&self, key: UserCacheKey, partial: Option<UserPartial>) {
        self.partial.insert(key, partial).await;
    }

    async fn invalidate(&self, user_id: i64) {
        let generation = self
            .generation(user_id)
            .try_update(Ordering::SeqCst, Ordering::SeqCst, |current| {
                current.checked_add(1)
            })
            .expect("user cache generation exhausted");
        self.generation_bumps.fetch_add(1, Ordering::Relaxed);
        let key = UserCacheKey {
            user_id,
            generation,
        };
        self.partial.invalidate(&key).await;
    }
}

impl UsersShard {
    pub fn new_postgres(kv: postgres::KvClient, max_entries: u64, ttl: Duration) -> Self {
        Self {
            storage: UsersStorage::Postgres(PostgresUsersStorage { kv }),
            caches: UserCaches::new(max_entries, ttl),
        }
    }

    #[cfg(feature = "scylla")]
    pub async fn new_scylla(
        db: Arc<Session>,
        max_entries: u64,
        ttl: Duration,
    ) -> anyhow::Result<Self> {
        let stmt_partial = db
            .prepare(format!(
                "SELECT {PARTIAL_USER_COLUMNS} FROM users WHERE user_id = ? LIMIT 1"
            ))
            .await?;
        let stmt_partial_batch = db
            .prepare(format!(
                "SELECT {PARTIAL_USER_COLUMNS} FROM users WHERE user_id IN ?"
            ))
            .await?;

        Ok(Self {
            storage: UsersStorage::Scylla(Arc::new(ScyllaUsersStorage {
                db,
                stmt_partial,
                stmt_partial_batch,
            })),
            caches: UserCaches::new(max_entries, ttl),
        })
    }

    async fn get_partial_user(&self, user_id: i64) -> anyhow::Result<Option<UserPartial>> {
        self.caches
            .get_or_fetch_partial(user_id, self.storage.fetch_partial_user(user_id))
            .await
    }

    async fn get_partial_users(
        &self,
        mut user_ids: Vec<i64>,
        instance_name: &str,
    ) -> anyhow::Result<Vec<UserPartial>> {
        user_ids.sort_unstable();
        user_ids.dedup();
        let mut partials = Vec::new();
        let mut misses = Vec::new();
        for user_id in user_ids {
            if user_id == FLUXER_SYSTEM_USER_ID {
                partials.push(system_user_partial(instance_name));
                continue;
            }
            match self.caches.get_partial(user_id).await {
                Some(Some(partial)) => partials.push(partial),
                Some(None) => {}
                None => misses.push(user_id),
            }
        }
        if misses.is_empty() {
            return Ok(partials);
        }
        let batches = misses
            .chunks(USER_BATCH_SIZE)
            .map(<[i64]>::to_vec)
            .collect::<Vec<_>>();
        let fetched_batches = stream::iter(batches)
            .map(|batch| async move { self.fetch_partial_batch(batch).await })
            .buffer_unordered(USER_BATCH_CONCURRENCY)
            .collect::<Vec<_>>()
            .await;
        for fetched in fetched_batches {
            partials.extend(fetched?);
        }
        Ok(partials)
    }

    async fn get_api_partial_users(
        &self,
        user_ids: Vec<String>,
        instance_name: &str,
    ) -> anyhow::Result<Vec<ApiUserPartial>> {
        let user_ids = user_ids
            .iter()
            .map(|user_id| parse_user_id(user_id))
            .collect::<anyhow::Result<Vec<_>>>()?;
        Ok(self
            .get_partial_users(user_ids, instance_name)
            .await?
            .into_iter()
            .map(|partial| partial.to_api_partial())
            .collect())
    }

    async fn fetch_partial_batch(&self, user_ids: Vec<i64>) -> anyhow::Result<Vec<UserPartial>> {
        let mut cache_keys = user_ids
            .iter()
            .map(|&user_id| (user_id, self.caches.key(user_id)))
            .collect::<HashMap<_, _>>();
        let fetched_partials = match self.storage.fetch_partial_batch(user_ids.clone()).await {
            Ok(fetched) => fetched,
            Err(err) => {
                tracing::warn!(
                    error = %err,
                    user_count = user_ids.len(),
                    "user partial batch read failed, retrying per user"
                );
                return self.fetch_partial_batch_individually(user_ids).await;
            }
        };
        let mut unexpected = 0usize;
        let mut partials = Vec::with_capacity(fetched_partials.len());
        for partial in fetched_partials {
            let Some(key) = cache_keys.remove(&partial.user_id) else {
                unexpected += 1;
                continue;
            };
            self.caches.insert_partial(key, Some(partial.clone())).await;
            partials.push(partial);
        }
        if unexpected > 0 {
            tracing::warn!(
                unexpected,
                "user batch returned duplicate or unrequested users"
            );
        }
        for key in cache_keys.into_values() {
            self.caches.insert_partial(key, None).await;
        }
        Ok(partials)
    }

    async fn fetch_partial_batch_individually(
        &self,
        user_ids: Vec<i64>,
    ) -> anyhow::Result<Vec<UserPartial>> {
        let partials = stream::iter(user_ids)
            .map(|user_id| async move { self.get_partial_user(user_id).await })
            .buffer_unordered(USER_BATCH_CONCURRENCY)
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect();
        Ok(partials)
    }
}

impl UsersStorage {
    async fn fetch_partial_user(&self, user_id: i64) -> anyhow::Result<Option<UserPartial>> {
        match self {
            UsersStorage::Postgres(storage) => storage.fetch_partial_user(user_id).await,
            #[cfg(feature = "scylla")]
            UsersStorage::Scylla(storage) => storage.fetch_partial_user(user_id).await,
        }
    }

    async fn fetch_partial_batch(&self, user_ids: Vec<i64>) -> anyhow::Result<Vec<UserPartial>> {
        match self {
            UsersStorage::Postgres(storage) => storage.fetch_partial_batch(user_ids).await,
            #[cfg(feature = "scylla")]
            UsersStorage::Scylla(storage) => storage.fetch_partial_batch(user_ids).await,
        }
    }
}

impl PostgresUsersStorage {
    async fn fetch_partial_user(&self, user_id: i64) -> anyhow::Result<Option<UserPartial>> {
        let key = postgres::kv_key(&[KeyPart::BigInt(user_id)])?;
        let Some(row) = self.kv.get_row("users", &key).await? else {
            return Ok(None);
        };
        decode_postgres_user_partial(row).map(Some)
    }

    async fn fetch_partial_batch(&self, user_ids: Vec<i64>) -> anyhow::Result<Vec<UserPartial>> {
        let keys = user_ids
            .iter()
            .map(|user_id| postgres::kv_key(&[KeyPart::BigInt(*user_id)]))
            .collect::<anyhow::Result<Vec<_>>>()?;
        let rows = self.kv.get_rows("users", &keys).await?;
        rows.into_iter()
            .map(|(_, row)| decode_postgres_user_partial(row))
            .collect()
    }
}

#[cfg(feature = "scylla")]
impl ScyllaUsersStorage {
    async fn fetch_partial_user(&self, user_id: i64) -> anyhow::Result<Option<UserPartial>> {
        let result = self
            .db
            .execute_unpaged(&self.stmt_partial, (user_id,))
            .await?;
        let rows = result.into_rows_result()?;
        let partial = rows
            .maybe_first_row::<PartialUserScyllaRow>()?
            .map(|row| UserPartial::from(PartialUserDbRow::from(row)));
        Ok(partial)
    }

    async fn fetch_partial_batch(&self, user_ids: Vec<i64>) -> anyhow::Result<Vec<UserPartial>> {
        let result = self
            .db
            .execute_unpaged(&self.stmt_partial_batch, (user_ids,))
            .await?;
        let rows = result.into_rows_result()?;
        let rows: Vec<PartialUserScyllaRow> = rows
            .rows::<PartialUserScyllaRow>()?
            .collect::<Result<_, _>>()?;
        Ok(rows
            .into_iter()
            .map(|row| UserPartial::from(PartialUserDbRow::from(row)))
            .collect::<Vec<_>>())
    }
}

fn decode_postgres_user_partial(row: serde_json::Value) -> anyhow::Result<UserPartial> {
    let row = postgres::decode_row_dates_as_millis(row)?;
    let row: PartialUserDbRow = serde_json::from_value(row)?;
    Ok(row.into())
}

fn system_user_partial(instance_name: &str) -> UserPartial {
    UserPartial {
        user_id: FLUXER_SYSTEM_USER_ID,
        username: instance_name.to_owned(),
        discriminator: FLUXER_SYSTEM_DISCRIMINATOR,
        global_name: None,
        avatar_hash: None,
        bot: Some(true),
        system: Some(true),
        flags: Some(USER_FLAG_STAFF),
        banner_hash: None,
        banner_color: None,
        accent_color: None,
        avatar_color: None,
        mention_flags: None,
        content_hidden_since: None,
        hidden_profile_badges: Vec::new(),
    }
}

impl ShardService for UsersShard {
    type Request = UserRequest;
    type Response = UserResponse;

    fn service_name(&self) -> &str {
        "users"
    }

    fn render_prometheus_metrics(&self, output: &mut String) {
        let _ = writeln!(
            output,
            "# TYPE fluxer_users_shard_cache_generation_bumps_total counter"
        );
        let _ = writeln!(
            output,
            "fluxer_users_shard_cache_generation_bumps_total {}",
            self.caches.generation_bumps.load(Ordering::Relaxed)
        );
        let _ = writeln!(
            output,
            "# TYPE fluxer_users_shard_cache_generation_stripes gauge"
        );
        let _ = writeln!(
            output,
            "fluxer_users_shard_cache_generation_stripes {}",
            self.caches.generations.len()
        );
    }

    async fn handle(&self, request: UserRequest) -> anyhow::Result<UserResponse> {
        match request {
            UserRequest::GetPartialsByIds {
                user_ids,
                instance_name,
            } => Ok(UserResponse::FoundPartials(
                self.get_partial_users(user_ids, &instance_name).await?,
            )),
            UserRequest::GetApiPartialsByIds {
                user_ids,
                instance_name,
            } => Ok(UserResponse::FoundApiPartials(
                self.get_api_partial_users(user_ids, &instance_name).await?,
            )),
            UserRequest::Invalidate { user_id } => {
                self.caches.invalidate(user_id).await;
                Ok(UserResponse::Invalidated)
            }
        }
    }
}

fn parse_user_id(user_id: &str) -> anyhow::Result<i64> {
    user_id
        .parse::<i64>()
        .map_err(|error| anyhow::anyhow!("invalid user id {user_id}: {error}"))
}

#[cfg(feature = "scylla")]
fn optional_timestamp_millis(value: OptionalTimestamp) -> Option<i64> {
    value.and_then(|maybe: MaybeEmpty<DateTime<Utc>>| match maybe {
        MaybeEmpty::Empty => None,
        MaybeEmpty::Value(dt) => Some(dt.timestamp_millis()),
    })
}

impl From<PartialUserDbRow> for UserPartial {
    fn from(row: PartialUserDbRow) -> Self {
        let standing = AccountStanding {
            flags: row.flags.unwrap_or_default(),
            temp_banned_until_ms: row.temp_banned_until,
            pending_deletion_at_ms: row.pending_deletion_at,
            deletion_reason_code: row.deletion_reason_code,
        };
        Self {
            user_id: row.user_id,
            username: row.username,
            discriminator: row.discriminator,
            global_name: row.global_name,
            avatar_hash: row.avatar_hash,
            bot: row.bot,
            system: row.system,
            flags: row.flags,
            banner_hash: row.banner_hash,
            banner_color: row.banner_color,
            accent_color: row.accent_color,
            avatar_color: row.avatar_color,
            mention_flags: row.mention_flags,
            content_hidden_since: row.content_hidden_since,
            hidden_profile_badges: row.hidden_profile_badges.unwrap_or_default(),
        }
        .visible_to_others(&standing, unix_millis())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_user(user_id: i64) -> PartialUserDbRow {
        PartialUserDbRow {
            user_id,
            username: "Ada".to_owned(),
            discriminator: 7,
            global_name: Some("Ada Lovelace".to_owned()),
            avatar_hash: Some("avatar_hash".to_owned()),
            bot: Some(false),
            system: Some(false),
            flags: Some(USER_FLAG_STAFF),
            banner_hash: None,
            banner_color: None,
            accent_color: None,
            avatar_color: None,
            mention_flags: None,
            temp_banned_until: None,
            pending_deletion_at: None,
            deletion_reason_code: None,
            content_hidden_since: None,
            hidden_profile_badges: None,
        }
    }

    const SPAMMER: i64 = 1 << 6;
    const DISABLED: i64 = 1 << 38;
    const PROFILE_HIDDEN: i64 = 1 << 7;

    fn styled(user_id: i64, flags: i64) -> PartialUserDbRow {
        let mut user = test_user(user_id);
        user.flags = Some(flags);
        user.banner_hash = Some("banner_hash".to_owned());
        user.banner_color = Some(3);
        user.accent_color = Some(5);
        user.avatar_color = Some(9);
        user
    }

    fn assert_hidden(partial: &UserPartial) {
        let expected = crate::pseudonym::pseudonym(partial.user_id);
        assert_eq!(partial.username, expected.username);
        assert_eq!(partial.discriminator, expected.discriminator);
        assert_ne!(partial.username, "Ada");
        assert_eq!(partial.global_name, None);
        assert_eq!(partial.avatar_hash, None);
        assert_eq!(partial.avatar_color, None);
        assert_eq!(partial.banner_hash, None);
        assert_eq!(partial.banner_color, None);
        assert_eq!(partial.accent_color, None);
        let api = partial.to_api_partial();
        assert_eq!(api.username, expected.username);
        assert_eq!(api.discriminator, format!("{:04}", expected.discriminator));
        assert_eq!(api.flags & PROFILE_HIDDEN as i32, 0);
    }

    #[test]
    fn a_masked_partial_matches_the_shared_development_vector() {
        let api = UserPartial::from(styled(1_174_109_840_998_400_001, SPAMMER)).to_api_partial();
        assert_eq!(
            (api.username.as_str(), api.discriminator.as_str()),
            ("MambaEgret", "6542")
        );
        assert_eq!(api.flags & PROFILE_HIDDEN as i32, 0);
        assert_eq!(api.global_name, None);
        assert_eq!(api.avatar, None);
    }

    #[test]
    fn hidden_and_enforced_accounts_get_a_neutral_partial_and_keep_their_stored_profile() {
        let now = unix_millis();
        let mut banned = styled(43, DISABLED);
        banned.temp_banned_until = Some(now + 60_000);
        let mut deleting = styled(44, 0);
        deleting.pending_deletion_at = Some(now + 60_000);
        deleting.deletion_reason_code = Some(20);
        for user in [
            styled(41, PROFILE_HIDDEN),
            styled(42, SPAMMER),
            banned,
            deleting,
        ] {
            assert_hidden(&UserPartial::from(user.clone()));
            assert_eq!(user.username, "Ada");
            assert_eq!(user.avatar_hash.as_deref(), Some("avatar_hash"));
        }
    }
}
