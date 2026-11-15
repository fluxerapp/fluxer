// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::model::{
    CapacityBudget, MAX_ACCOUNT_BYTES, MAX_ACCOUNTS, MAX_INSTANCE_BYTES, MAX_KNOWN_INSTANCE_BYTES,
    MAX_KNOWN_INSTANCES, StoreError, WriteRefusal, check_lookup_key,
};
use rusqlite::{Connection, OptionalExtension, Transaction, TransactionBehavior, params};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use serde_json::value::RawValue;

#[derive(Deserialize, Serialize)]
pub struct AccountEntry {
    #[serde(rename = "storageKey")]
    pub storage_key: String,
    pub record: Box<RawValue>,
}

#[derive(Deserialize)]
pub struct AccountCompareAndSwap {
    pub expected: AccountEntry,
    pub replacement: AccountEntry,
}

#[derive(Deserialize, Serialize)]
pub struct KnownInstanceEntry {
    pub key: String,
    pub record: Box<RawValue>,
}

pub enum AccountWrite {
    Written { instance_usable: bool },
    Refused(WriteRefusal),
}

struct AccountFacts {
    user_id: String,
    last_active: f64,
    instance_usable: bool,
}

fn account_record_fields(
    record: &RawValue,
) -> Result<serde_json::Map<String, Value>, WriteRefusal> {
    match serde_json::from_str::<Value>(record.get()) {
        Ok(Value::Object(fields)) => Ok(fields),
        _ => Err(WriteRefusal::Invalid(
            "account record must be a JSON object",
        )),
    }
}

fn account_facts(record: &RawValue) -> Result<AccountFacts, WriteRefusal> {
    let fields = account_record_fields(record)?;
    Ok(AccountFacts {
        user_id: fields
            .get("userId")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_owned(),
        last_active: numeric_field(&fields, "lastActive"),
        instance_usable: instance_is_usable(&fields),
    })
}

fn numeric_field(fields: &serde_json::Map<String, Value>, name: &str) -> f64 {
    fields
        .get(name)
        .and_then(Value::as_f64)
        .filter(|value| value.is_finite())
        .unwrap_or_default()
}

fn instance_is_usable(fields: &serde_json::Map<String, Value>) -> bool {
    let Some(Value::Object(instance)) = fields.get("instance") else {
        return false;
    };
    let endpoint_is_present = instance
        .get("apiEndpoint")
        .and_then(Value::as_str)
        .is_some_and(|endpoint| !endpoint.is_empty());
    endpoint_is_present
        && serde_json::to_string(instance)
            .is_ok_and(|serialized| serialized.len() <= MAX_INSTANCE_BYTES)
}

pub fn account_budget(conn: &Connection) -> Result<CapacityBudget, StoreError> {
    CapacityBudget::read(
        conn,
        "accounts",
        "SELECT COUNT(*), COALESCE(SUM(length(CAST(storage_key AS BLOB)) + length(CAST(record_json AS BLOB))), 0) FROM accounts",
        [],
        MAX_ACCOUNTS,
        MAX_ACCOUNT_BYTES,
    )
}

pub fn write_account(
    conn: &Connection,
    budget: &mut CapacityBudget,
    entry: &AccountEntry,
) -> Result<AccountWrite, StoreError> {
    if let Some(refusal) = check_lookup_key(&entry.storage_key, "account storageKey is unusable") {
        return Ok(AccountWrite::Refused(refusal));
    }
    let facts = match account_facts(&entry.record) {
        Ok(facts) => facts,
        Err(refusal) => return Ok(AccountWrite::Refused(refusal)),
    };
    let incoming = (entry.storage_key.len() + entry.record.get().len()) as i64;
    let replaces = stored_bytes(
        conn,
        "SELECT length(CAST(storage_key AS BLOB)) + length(CAST(record_json AS BLOB)) FROM accounts WHERE storage_key = ?1",
        params![entry.storage_key],
    )?;
    if let Err(refusal) = budget.check(replaces, incoming) {
        return Ok(AccountWrite::Refused(refusal.into()));
    }
    budget.commit(replaces, incoming);
    conn.execute(
        "INSERT INTO accounts(storage_key, user_id, last_active, record_json) VALUES (?1, ?2, ?3, ?4)
         ON CONFLICT(storage_key) DO UPDATE SET
            user_id = excluded.user_id,
            last_active = excluded.last_active,
            record_json = excluded.record_json",
        params![
            entry.storage_key,
            facts.user_id,
            facts.last_active,
            entry.record.get()
        ],
    )?;
    Ok(AccountWrite::Written {
        instance_usable: facts.instance_usable,
    })
}

pub fn upsert_account(conn: &Connection, entry: &AccountEntry) -> Result<(), StoreError> {
    let mut budget = account_budget(conn)?;
    match write_account(conn, &mut budget, entry)? {
        AccountWrite::Written { .. } => Ok(()),
        AccountWrite::Refused(refusal) => Err(refusal.into()),
    }
}

pub fn compare_and_swap_account(
    conn: &mut Connection,
    request: &AccountCompareAndSwap,
) -> Result<bool, StoreError> {
    if request.expected.storage_key != request.replacement.storage_key {
        return Err(StoreError::Refused(
            "account compare-and-swap records must use the same storageKey",
        ));
    }
    if let Some(refusal) = check_lookup_key(
        &request.expected.storage_key,
        "account storageKey is unusable",
    ) {
        return Err(refusal.into());
    }
    let expected = account_record_fields(&request.expected.record)?;
    let transaction = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
    let stored: Option<String> = transaction
        .query_row(
            "SELECT record_json FROM accounts WHERE storage_key = ?1",
            params![request.expected.storage_key],
            |row| row.get(0),
        )
        .optional()?;
    let Some(stored) = stored else {
        transaction.commit()?;
        return Ok(false);
    };
    let stored = RawValue::from_string(stored)?;
    let stored = account_record_fields(&stored)?;
    if stored != expected {
        transaction.commit()?;
        return Ok(false);
    }
    let mut budget = account_budget(&transaction)?;
    match write_account(&transaction, &mut budget, &request.replacement)? {
        AccountWrite::Written { .. } => {}
        AccountWrite::Refused(refusal) => return Err(refusal.into()),
    }
    transaction.commit()?;
    Ok(true)
}

pub fn all_accounts(conn: &Connection) -> Result<Vec<AccountEntry>, StoreError> {
    let mut statement = conn.prepare(
        "SELECT storage_key, record_json FROM accounts ORDER BY last_active DESC, storage_key ASC",
    )?;
    let mut rows = statement.query([])?;
    let mut accounts = Vec::new();
    while let Some(row) = rows.next()? {
        accounts.push(AccountEntry {
            storage_key: row.get(0)?,
            record: RawValue::from_string(row.get(1)?)?,
        });
    }
    Ok(accounts)
}

pub fn account(conn: &Connection, storage_key: &str) -> Result<Option<AccountEntry>, StoreError> {
    let record: Option<String> = conn
        .query_row(
            "SELECT record_json FROM accounts WHERE storage_key = ?1",
            params![storage_key],
            |row| row.get(0),
        )
        .optional()?;
    record
        .map(|record| {
            Ok(AccountEntry {
                storage_key: storage_key.to_owned(),
                record: RawValue::from_string(record)?,
            })
        })
        .transpose()
}

pub fn delete_account_rows(conn: &Connection, storage_key: &str) -> Result<(), StoreError> {
    conn.execute(
        "DELETE FROM accounts WHERE storage_key = ?1",
        params![storage_key],
    )?;
    conn.execute(
        "DELETE FROM scoped_storage WHERE scope = ?1",
        params![storage_key],
    )?;
    Ok(())
}

pub fn delete_account(conn: &Connection, storage_key: &str) -> Result<(), StoreError> {
    let transaction = Transaction::new_unchecked(conn, TransactionBehavior::Immediate)?;
    delete_account_rows(&transaction, storage_key)?;
    transaction.commit()?;
    Ok(())
}

pub fn known_instance_budget(conn: &Connection) -> Result<CapacityBudget, StoreError> {
    CapacityBudget::read(
        conn,
        "known instances",
        "SELECT COUNT(*), COALESCE(SUM(length(CAST(key AS BLOB)) + length(CAST(record_json AS BLOB))), 0) FROM known_instances",
        [],
        MAX_KNOWN_INSTANCES,
        MAX_KNOWN_INSTANCE_BYTES,
    )
}

pub fn upsert_known_instance(
    conn: &Connection,
    entry: &KnownInstanceEntry,
) -> Result<(), StoreError> {
    if let Some(refusal) = check_lookup_key(&entry.key, "known instance key is unusable") {
        return Err(refusal.into());
    }
    let Ok(Value::Object(fields)) = serde_json::from_str::<Value>(entry.record.get()) else {
        return Err(WriteRefusal::Invalid("known instance record must be a JSON object").into());
    };
    let budget = known_instance_budget(conn)?;
    let incoming = (entry.key.len() + entry.record.get().len()) as i64;
    let replaces = stored_bytes(
        conn,
        "SELECT length(CAST(key AS BLOB)) + length(CAST(record_json AS BLOB)) FROM known_instances WHERE key = ?1",
        params![entry.key],
    )?;
    budget.check(replaces, incoming)?;
    conn.execute(
        "INSERT INTO known_instances(key, last_used, record_json) VALUES (?1, ?2, ?3)
         ON CONFLICT(key) DO UPDATE SET
            last_used = excluded.last_used,
            record_json = excluded.record_json",
        params![
            entry.key,
            numeric_field(&fields, "lastUsed"),
            entry.record.get()
        ],
    )?;
    Ok(())
}

pub fn all_known_instances(conn: &Connection) -> Result<Vec<KnownInstanceEntry>, StoreError> {
    let mut statement = conn
        .prepare("SELECT key, record_json FROM known_instances ORDER BY last_used DESC, key ASC")?;
    let mut rows = statement.query([])?;
    let mut instances = Vec::new();
    while let Some(row) = rows.next()? {
        instances.push(KnownInstanceEntry {
            key: row.get(0)?,
            record: RawValue::from_string(row.get(1)?)?,
        });
    }
    Ok(instances)
}

pub fn delete_known_instance(conn: &Connection, key: &str) -> Result<(), StoreError> {
    conn.execute("DELETE FROM known_instances WHERE key = ?1", params![key])?;
    Ok(())
}

fn stored_bytes(
    conn: &Connection,
    sql: &str,
    args: impl rusqlite::Params,
) -> Result<Option<i64>, StoreError> {
    Ok(conn.query_row(sql, args, |row| row.get(0)).optional()?)
}
