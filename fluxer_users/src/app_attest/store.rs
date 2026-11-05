// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{ChallengePurpose, Environment};
use base64::Engine;
use base64::engine::general_purpose::STANDARD;
use chrono::{DateTime, SecondsFormat, Utc};
use fluxer_svc::postgres::{self, KeyPart, KvClient};
use serde_json::{Value, json};

#[cfg(feature = "scylla")]
use scylla::client::session::Session;
#[cfg(feature = "scylla")]
use scylla::statement::prepared::PreparedStatement;
#[cfg(feature = "scylla")]
use std::sync::Arc;

const CHALLENGES_TABLE: &str = "app_attest_challenges";
const KEYS_TABLE: &str = "app_attest_keys";
const KEY_LOOKUP_TABLE: &str = "app_attest_key_lookup";
pub const CHALLENGE_TTL_SECONDS: i64 = 300;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ChallengeRecord {
    pub user_id: i64,
    pub purpose: ChallengePurpose,
    pub expires_at_ms: i64,
}

impl ChallengeRecord {
    pub fn accepts(&self, user_id: i64, purpose: ChallengePurpose, now_ms: i64) -> bool {
        self.user_id == user_id && self.purpose == purpose && now_ms < self.expires_at_ms
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StoredKey {
    pub key_id: String,
    pub public_key: Vec<u8>,
    pub environment: Environment,
    pub app_id: String,
    pub counter: i64,
    pub created_at_ms: i64,
    pub last_used_at_ms: Option<i64>,
}

#[derive(Clone)]
pub enum AppAttestStore {
    Postgres(KvClient),
    #[cfg(feature = "scylla")]
    Scylla(Arc<ScyllaAppAttestStore>),
}

impl AppAttestStore {
    pub async fn insert_challenge(
        &self,
        challenge: &str,
        record: &ChallengeRecord,
    ) -> anyhow::Result<()> {
        match self {
            AppAttestStore::Postgres(kv) => {
                let key = postgres::kv_key(&[KeyPart::String(challenge)])?;
                let row = json!({
                    "challenge": challenge,
                    "user_id": bigint(record.user_id),
                    "purpose": record.purpose.as_str(),
                    "expires_at": date(record.expires_at_ms),
                });
                if !kv
                    .insert_row_if_absent(
                        CHALLENGES_TABLE,
                        &key,
                        &key,
                        &row,
                        Some(record.expires_at_ms),
                    )
                    .await?
                {
                    anyhow::bail!("app attest challenge collision");
                }
                Ok(())
            }
            #[cfg(feature = "scylla")]
            AppAttestStore::Scylla(store) => store.insert_challenge(challenge, record).await,
        }
    }

    pub async fn consume_challenge(
        &self,
        challenge: &str,
        user_id: i64,
        purpose: ChallengePurpose,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        match self {
            AppAttestStore::Postgres(kv) => {
                let key = postgres::kv_key(&[KeyPart::String(challenge)])?;
                let Some(row) = kv.take_row(CHALLENGES_TABLE, &key).await? else {
                    return Ok(false);
                };
                Ok(decode_challenge(row)
                    .is_some_and(|record| record.accepts(user_id, purpose, now_ms)))
            }
            #[cfg(feature = "scylla")]
            AppAttestStore::Scylla(store) => {
                store
                    .consume_challenge(challenge, user_id, purpose, now_ms)
                    .await
            }
        }
    }

    pub async fn claim_key_id(&self, key_id: &str, user_id: i64) -> anyhow::Result<i64> {
        match self {
            AppAttestStore::Postgres(kv) => {
                let key = postgres::kv_key(&[KeyPart::String(key_id)])?;
                let row = json!({"key_id": key_id, "user_id": bigint(user_id)});
                if kv
                    .insert_row_if_absent(KEY_LOOKUP_TABLE, &key, &key, &row, None)
                    .await?
                {
                    return Ok(user_id);
                }
                let row = kv
                    .get_row(KEY_LOOKUP_TABLE, &key)
                    .await?
                    .ok_or_else(|| anyhow::anyhow!("app attest key lookup vanished"))?;
                let row = postgres::decode_row_dates_as_millis(row)?;
                row.get("user_id")
                    .and_then(Value::as_i64)
                    .ok_or_else(|| anyhow::anyhow!("app attest key lookup has no user"))
            }
            #[cfg(feature = "scylla")]
            AppAttestStore::Scylla(store) => store.claim_key_id(key_id, user_id).await,
        }
    }

    pub async fn insert_key_if_absent(
        &self,
        user_id: i64,
        key: &StoredKey,
    ) -> anyhow::Result<bool> {
        match self {
            AppAttestStore::Postgres(kv) => {
                let (partition, row_key) = key_row_keys(user_id, &key.key_id)?;
                let row = json!({
                    "user_id": bigint(user_id),
                    "key_id": key.key_id,
                    "public_key": {"__fluxer_type": "buffer", "value": STANDARD.encode(&key.public_key)},
                    "environment": key.environment.as_str(),
                    "app_id": key.app_id,
                    "counter": bigint(key.counter),
                    "created_at": date(key.created_at_ms),
                    "last_used_at": key.last_used_at_ms.map(date),
                });
                kv.insert_row_if_absent(KEYS_TABLE, &partition, &row_key, &row, None)
                    .await
            }
            #[cfg(feature = "scylla")]
            AppAttestStore::Scylla(store) => store.insert_key_if_absent(user_id, key).await,
        }
    }

    pub async fn get_key(&self, user_id: i64, key_id: &str) -> anyhow::Result<Option<StoredKey>> {
        match self {
            AppAttestStore::Postgres(kv) => {
                let (_, row_key) = key_row_keys(user_id, key_id)?;
                kv.get_row(KEYS_TABLE, &row_key)
                    .await?
                    .map(decode_key)
                    .transpose()
            }
            #[cfg(feature = "scylla")]
            AppAttestStore::Scylla(store) => store.get_key(user_id, key_id).await,
        }
    }

    pub async fn list_keys(&self, user_id: i64) -> anyhow::Result<Vec<StoredKey>> {
        match self {
            AppAttestStore::Postgres(kv) => {
                let partition = postgres::kv_key(&[KeyPart::BigInt(user_id)])?;
                kv.get_partition_rows(KEYS_TABLE, &partition)
                    .await?
                    .into_iter()
                    .map(|(_, row)| decode_key(row))
                    .collect()
            }
            #[cfg(feature = "scylla")]
            AppAttestStore::Scylla(store) => store.list_keys(user_id).await,
        }
    }

    pub async fn delete_key(&self, user_id: i64, key_id: &str) -> anyhow::Result<()> {
        match self {
            AppAttestStore::Postgres(kv) => {
                let (_, row_key) = key_row_keys(user_id, key_id)?;
                kv.delete_row(KEYS_TABLE, &row_key).await?;
                let lookup = postgres::kv_key(&[KeyPart::String(key_id)])?;
                kv.delete_row(KEY_LOOKUP_TABLE, &lookup).await
            }
            #[cfg(feature = "scylla")]
            AppAttestStore::Scylla(store) => store.delete_key(user_id, key_id).await,
        }
    }

    pub async fn advance_counter(
        &self,
        user_id: i64,
        key_id: &str,
        counter: i64,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        match self {
            AppAttestStore::Postgres(kv) => {
                let (_, row_key) = key_row_keys(user_id, key_id)?;
                let patch = json!({"counter": bigint(counter), "last_used_at": date(now_ms)});
                kv.merge_row_if_bigint_below(KEYS_TABLE, &row_key, "counter", counter, &patch)
                    .await
            }
            #[cfg(feature = "scylla")]
            AppAttestStore::Scylla(store) => {
                store
                    .advance_counter(user_id, key_id, counter, now_ms)
                    .await
            }
        }
    }
}

fn key_row_keys(user_id: i64, key_id: &str) -> anyhow::Result<(String, String)> {
    Ok((
        postgres::kv_key(&[KeyPart::BigInt(user_id)])?,
        postgres::kv_key(&[KeyPart::BigInt(user_id), KeyPart::String(key_id)])?,
    ))
}

fn bigint(value: i64) -> Value {
    json!({"__fluxer_type": "bigint", "value": value.to_string()})
}

fn date(ms: i64) -> Value {
    json!({"__fluxer_type": "date", "value": iso_millis(ms)})
}

pub fn iso_millis(ms: i64) -> String {
    DateTime::<Utc>::from_timestamp_millis(ms)
        .unwrap_or_default()
        .to_rfc3339_opts(SecondsFormat::Millis, true)
}

fn decode_challenge(row: Value) -> Option<ChallengeRecord> {
    let row = postgres::decode_row_dates_as_millis(row).ok()?;
    let purpose = match row.get("purpose")?.as_str()? {
        "attest" => ChallengePurpose::Attest,
        "assert" => ChallengePurpose::Assert,
        _ => return None,
    };
    Some(ChallengeRecord {
        user_id: row.get("user_id")?.as_i64()?,
        purpose,
        expires_at_ms: row.get("expires_at")?.as_i64()?,
    })
}

fn decode_key(row: Value) -> anyhow::Result<StoredKey> {
    let row = postgres::decode_row_dates_as_millis(row)?;
    let field = |name: &str| {
        row.get(name)
            .ok_or_else(|| anyhow::anyhow!("app attest key row has no {name}"))
    };
    let text = |name: &str| {
        field(name)?
            .as_str()
            .map(ToOwned::to_owned)
            .ok_or_else(|| anyhow::anyhow!("app attest key row {name} is not text"))
    };
    let number = |name: &str| {
        field(name)?
            .as_i64()
            .ok_or_else(|| anyhow::anyhow!("app attest key row {name} is not a number"))
    };
    Ok(StoredKey {
        key_id: text("key_id")?,
        public_key: STANDARD.decode(text("public_key")?)?,
        environment: Environment::parse(&text("environment")?)
            .ok_or_else(|| anyhow::anyhow!("app attest key row has an unknown environment"))?,
        app_id: text("app_id")?,
        counter: number("counter")?,
        created_at_ms: number("created_at")?,
        last_used_at_ms: row.get("last_used_at").and_then(Value::as_i64),
    })
}

#[cfg(feature = "scylla")]
pub struct ScyllaAppAttestStore {
    db: Arc<Session>,
    statements: tokio::sync::OnceCell<ScyllaStatements>,
}

#[cfg(feature = "scylla")]
struct ScyllaStatements {
    insert_challenge: PreparedStatement,
    consume_challenge: PreparedStatement,
    claim_key_id: PreparedStatement,
    insert_key: PreparedStatement,
    select_key: PreparedStatement,
    select_keys: PreparedStatement,
    delete_key: PreparedStatement,
    delete_lookup: PreparedStatement,
    advance_counter: PreparedStatement,
}

#[cfg(feature = "scylla")]
const SCYLLA_KEY_COLUMNS: &str =
    "key_id, public_key, environment, app_id, counter, created_at, last_used_at";

#[cfg(feature = "scylla")]
#[derive(scylla::DeserializeRow)]
struct ScyllaKeyRow {
    key_id: String,
    public_key: Vec<u8>,
    environment: String,
    app_id: String,
    counter: i64,
    created_at: DateTime<Utc>,
    last_used_at: Option<DateTime<Utc>>,
}

#[cfg(feature = "scylla")]
impl TryFrom<ScyllaKeyRow> for StoredKey {
    type Error = anyhow::Error;

    fn try_from(row: ScyllaKeyRow) -> anyhow::Result<Self> {
        Ok(StoredKey {
            key_id: row.key_id,
            public_key: row.public_key,
            environment: Environment::parse(&row.environment)
                .ok_or_else(|| anyhow::anyhow!("app attest key row has an unknown environment"))?,
            app_id: row.app_id,
            counter: row.counter,
            created_at_ms: row.created_at.timestamp_millis(),
            last_used_at_ms: row.last_used_at.map(|at| at.timestamp_millis()),
        })
    }
}

#[cfg(feature = "scylla")]
fn timestamp(ms: i64) -> DateTime<Utc> {
    DateTime::<Utc>::from_timestamp_millis(ms).unwrap_or_default()
}

#[cfg(feature = "scylla")]
struct LwtOutcome {
    applied: bool,
    user_id: Option<i64>,
}

#[cfg(feature = "scylla")]
fn lwt_outcome(result: scylla::response::query_result::QueryResult) -> anyhow::Result<LwtOutcome> {
    use scylla::value::{CqlValue, Row};
    let rows = result.into_rows_result()?;
    let names = rows
        .column_specs()
        .iter()
        .map(|spec| spec.name().to_owned())
        .collect::<Vec<_>>();
    let row = rows
        .maybe_first_row::<Row>()?
        .ok_or_else(|| anyhow::anyhow!("lightweight transaction returned no row"))?;
    let column = |name: &str| {
        names
            .iter()
            .position(|candidate| candidate == name)
            .and_then(|index| row.columns.get(index).cloned().flatten())
    };
    let applied = matches!(column("[applied]"), Some(CqlValue::Boolean(true)));
    let user_id = match column("user_id") {
        Some(CqlValue::BigInt(user_id)) => Some(user_id),
        _ => None,
    };
    Ok(LwtOutcome { applied, user_id })
}

#[cfg(feature = "scylla")]
impl ScyllaAppAttestStore {
    pub fn new(db: Arc<Session>) -> Self {
        Self {
            db,
            statements: tokio::sync::OnceCell::new(),
        }
    }

    async fn statements(&self) -> anyhow::Result<&ScyllaStatements> {
        self.statements
            .get_or_try_init(|| async {
                let db = &self.db;
                Ok::<_, anyhow::Error>(ScyllaStatements {
                    insert_challenge: db
                        .prepare(format!(
                            "INSERT INTO {CHALLENGES_TABLE} (challenge, user_id, purpose, expires_at) VALUES (?, ?, ?, ?) IF NOT EXISTS USING TTL {CHALLENGE_TTL_SECONDS}"
                        ))
                        .await?,
                    consume_challenge: db
                        .prepare(format!(
                            "DELETE FROM {CHALLENGES_TABLE} WHERE challenge = ? IF user_id = ? AND purpose = ? AND expires_at > ?"
                        ))
                        .await?,
                    claim_key_id: db
                        .prepare(format!(
                            "INSERT INTO {KEY_LOOKUP_TABLE} (key_id, user_id) VALUES (?, ?) IF NOT EXISTS"
                        ))
                        .await?,
                    insert_key: db
                        .prepare(format!(
                            "INSERT INTO {KEYS_TABLE} (user_id, {SCYLLA_KEY_COLUMNS}) VALUES (?, ?, ?, ?, ?, ?, ?, ?) IF NOT EXISTS"
                        ))
                        .await?,
                    select_key: db
                        .prepare(format!(
                            "SELECT {SCYLLA_KEY_COLUMNS} FROM {KEYS_TABLE} WHERE user_id = ? AND key_id = ?"
                        ))
                        .await?,
                    select_keys: db
                        .prepare(format!(
                            "SELECT {SCYLLA_KEY_COLUMNS} FROM {KEYS_TABLE} WHERE user_id = ?"
                        ))
                        .await?,
                    delete_key: db
                        .prepare(format!(
                            "DELETE FROM {KEYS_TABLE} WHERE user_id = ? AND key_id = ? IF EXISTS"
                        ))
                        .await?,
                    delete_lookup: db
                        .prepare(format!(
                            "DELETE FROM {KEY_LOOKUP_TABLE} WHERE key_id = ? IF user_id = ?"
                        ))
                        .await?,
                    advance_counter: db
                        .prepare(format!(
                            "UPDATE {KEYS_TABLE} SET counter = ?, last_used_at = ? WHERE user_id = ? AND key_id = ? IF counter < ?"
                        ))
                        .await?,
                })
            })
            .await
    }

    async fn insert_challenge(
        &self,
        challenge: &str,
        record: &ChallengeRecord,
    ) -> anyhow::Result<()> {
        let statements = self.statements().await?;
        let result = self
            .db
            .execute_unpaged(
                &statements.insert_challenge,
                (
                    challenge,
                    record.user_id,
                    record.purpose.as_str(),
                    timestamp(record.expires_at_ms),
                ),
            )
            .await?;
        if !lwt_outcome(result)?.applied {
            anyhow::bail!("app attest challenge collision");
        }
        Ok(())
    }

    async fn consume_challenge(
        &self,
        challenge: &str,
        user_id: i64,
        purpose: ChallengePurpose,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        let statements = self.statements().await?;
        let result = self
            .db
            .execute_unpaged(
                &statements.consume_challenge,
                (challenge, user_id, purpose.as_str(), timestamp(now_ms)),
            )
            .await?;
        Ok(lwt_outcome(result)?.applied)
    }

    async fn claim_key_id(&self, key_id: &str, user_id: i64) -> anyhow::Result<i64> {
        let statements = self.statements().await?;
        let result = self
            .db
            .execute_unpaged(&statements.claim_key_id, (key_id, user_id))
            .await?;
        let outcome = lwt_outcome(result)?;
        if outcome.applied {
            return Ok(user_id);
        }
        outcome
            .user_id
            .ok_or_else(|| anyhow::anyhow!("app attest key lookup has no user"))
    }

    async fn insert_key_if_absent(&self, user_id: i64, key: &StoredKey) -> anyhow::Result<bool> {
        let statements = self.statements().await?;
        let result = self
            .db
            .execute_unpaged(
                &statements.insert_key,
                (
                    user_id,
                    key.key_id.as_str(),
                    key.public_key.as_slice(),
                    key.environment.as_str(),
                    key.app_id.as_str(),
                    key.counter,
                    timestamp(key.created_at_ms),
                    key.last_used_at_ms.map(timestamp),
                ),
            )
            .await?;
        Ok(lwt_outcome(result)?.applied)
    }

    async fn get_key(&self, user_id: i64, key_id: &str) -> anyhow::Result<Option<StoredKey>> {
        let statements = self.statements().await?;
        let result = self
            .db
            .execute_unpaged(&statements.select_key, (user_id, key_id))
            .await?;
        result
            .into_rows_result()?
            .maybe_first_row::<ScyllaKeyRow>()?
            .map(StoredKey::try_from)
            .transpose()
    }

    async fn list_keys(&self, user_id: i64) -> anyhow::Result<Vec<StoredKey>> {
        let statements = self.statements().await?;
        let result = self
            .db
            .execute_unpaged(&statements.select_keys, (user_id,))
            .await?;
        result
            .into_rows_result()?
            .rows::<ScyllaKeyRow>()?
            .map(|row| StoredKey::try_from(row?))
            .collect()
    }

    async fn delete_key(&self, user_id: i64, key_id: &str) -> anyhow::Result<()> {
        let statements = self.statements().await?;
        self.db
            .execute_unpaged(&statements.delete_key, (user_id, key_id))
            .await?;
        self.db
            .execute_unpaged(&statements.delete_lookup, (key_id, user_id))
            .await?;
        Ok(())
    }

    async fn advance_counter(
        &self,
        user_id: i64,
        key_id: &str,
        counter: i64,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        let statements = self.statements().await?;
        let result = self
            .db
            .execute_unpaged(
                &statements.advance_counter,
                (counter, timestamp(now_ms), user_id, key_id, counter),
            )
            .await?;
        Ok(lwt_outcome(result)?.applied)
    }
}
