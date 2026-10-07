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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Store, temporary_store_path};
    use crate::scoped_storage::{ScopedEntry, entry as scoped_entry, scope_entries, set_entry};

    fn entry(storage_key: &str, record: &str) -> AccountEntry {
        AccountEntry {
            storage_key: storage_key.to_owned(),
            record: RawValue::from_string(record.to_owned()).expect("valid JSON"),
        }
    }

    #[test]
    fn a_record_with_unknown_keys_round_trips_byte_exactly() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        let written = r#"{"userId":"1","instance":{"apiEndpoint":"https://a.example/api","futureKey":[1,2]},"unknownTopLevel":{"nested":true},"lastActive":7}"#;

        upsert_account(store.connection(), &entry("a.example::1", written))
            .expect("the account is stored");
        let read = account(store.connection(), "a.example::1")
            .expect("the account is readable")
            .expect("the account is present");

        assert_eq!(
            read.record.get(),
            written,
            "a record written by a newer renderer must survive an older shell byte-exactly"
        );
        assert_eq!(read.storage_key, "a.example::1");
    }

    #[test]
    fn accounts_are_ordered_by_last_active_then_storage_key() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        for (key, last_active) in [("b::2", 5.0), ("a::1", 9.0), ("a::3", 5.0)] {
            upsert_account(
                store.connection(),
                &entry(
                    key,
                    &format!(r#"{{"userId":"u","lastActive":{last_active}}}"#),
                ),
            )
            .expect("the account is stored");
        }

        let keys = all_accounts(store.connection())
            .expect("the accounts are readable")
            .into_iter()
            .map(|account| account.storage_key)
            .collect::<Vec<_>>();
        assert_eq!(keys, vec!["a::1", "a::3", "b::2"]);
    }

    #[test]
    fn a_record_the_addon_cannot_use_is_still_stored() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        let deployed_shape =
            r#"{"userId":"1","instance":{"gifProvider":"tenor","apiCodeVersion":9}}"#;

        upsert_account(store.connection(), &entry("legacy::1", deployed_shape))
            .expect("a record with an unusable instance is still stored");
        let mut budget = account_budget(store.connection());
        let write = write_account(
            store.connection(),
            budget.as_mut().expect("the budget is readable"),
            &entry("legacy::1", deployed_shape),
        )
        .expect("the write succeeds");
        assert!(matches!(
            write,
            AccountWrite::Written {
                instance_usable: false
            }
        ));
        assert_eq!(
            account(store.connection(), "legacy::1")
                .expect("the account is readable")
                .expect("the account is present")
                .record
                .get(),
            deployed_shape
        );
    }

    #[test]
    fn a_storage_key_crosses_the_boundary_opaquely() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        let opaque = "HTTPS://Mixed.Case.Example:8443/api::user::12345";

        upsert_account(store.connection(), &entry(opaque, r#"{"userId":"12345"}"#))
            .expect("the account is stored");
        assert!(
            account(store.connection(), opaque)
                .expect("the account is readable")
                .is_some(),
            "the addon must not normalise the storage key it was given"
        );
    }

    #[test]
    fn a_record_that_is_not_a_json_object_is_refused_rather_than_stored() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        assert!(upsert_account(store.connection(), &entry("a::1", "[1,2,3]")).is_err());
        assert!(
            all_accounts(store.connection())
                .expect("the accounts are readable")
                .is_empty()
        );
    }

    #[test]
    fn deleting_an_account_takes_its_scoped_storage_with_it() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        for key in ["a::1", "a::2"] {
            upsert_account(store.connection(), &entry(key, r#"{"userId":"1"}"#))
                .expect("the account is stored");
        }
        for scope in ["a::1", "a::2", "global"] {
            set_entry(
                store.connection(),
                &ScopedEntry {
                    store: "app".to_owned(),
                    scope: scope.to_owned(),
                    key: "theme".to_owned(),
                    value: "\"dark\"".to_owned(),
                    updated_at: 1.0,
                },
            )
            .expect("the entry is stored");
        }

        delete_account(store.connection(), "a::1").expect("the account is deleted");

        assert!(
            account(store.connection(), "a::1")
                .expect("the account is readable")
                .is_none()
        );
        assert!(
            scope_entries(store.connection(), "app", "a::1")
                .expect("entries are readable")
                .is_empty(),
            "a deleted account must not leave its scoped storage behind"
        );
        assert!(
            scoped_entry(store.connection(), "app", "a::2", "theme")
                .expect("the entry is readable")
                .is_some()
        );
        assert!(
            scoped_entry(store.connection(), "app", "global", "theme")
                .expect("the entry is readable")
                .is_some()
        );
    }

    #[test]
    fn a_compare_and_swap_lands_once_and_refuses_the_stale_retry() {
        let (_dir, path) = temporary_store_path();
        let mut store = Store::open(path, None).expect("a fresh store opens");
        let first = r#"{"userId":"1","token":"first"}"#;
        let second = r#"{"userId":"1","token":"second"}"#;
        let third = r#"{"userId":"1","token":"third"}"#;
        upsert_account(store.connection(), &entry("a.example::1", first))
            .expect("the account is stored");

        assert!(
            compare_and_swap_account(
                store.connection_mut(),
                &AccountCompareAndSwap {
                    expected: entry("a.example::1", first),
                    replacement: entry("a.example::1", second),
                },
            )
            .expect("the swap runs")
        );
        assert_eq!(
            account(store.connection(), "a.example::1")
                .expect("the account is readable")
                .expect("the account is present")
                .record
                .get(),
            second
        );

        assert!(
            !compare_and_swap_account(
                store.connection_mut(),
                &AccountCompareAndSwap {
                    expected: entry("a.example::1", first),
                    replacement: entry("a.example::1", third),
                },
            )
            .expect("the swap runs"),
            "a swap holding a record the store has already moved past must report that it did not land"
        );
        assert_eq!(
            account(store.connection(), "a.example::1")
                .expect("the account is readable")
                .expect("the account is present")
                .record
                .get(),
            second,
            "a refused swap must leave the newer record untouched"
        );
    }

    #[test]
    fn a_compare_and_swap_across_two_storage_keys_is_refused() {
        let (_dir, path) = temporary_store_path();
        let mut store = Store::open(path, None).expect("a fresh store opens");
        let record = r#"{"userId":"1"}"#;
        upsert_account(store.connection(), &entry("a.example::1", record))
            .expect("the account is stored");

        let outcome = compare_and_swap_account(
            store.connection_mut(),
            &AccountCompareAndSwap {
                expected: entry("a.example::1", record),
                replacement: entry("a.example::2", record),
            },
        );

        assert!(matches!(outcome, Err(StoreError::Refused(_))));
        assert!(
            account(store.connection(), "a.example::2")
                .expect("the accounts are readable")
                .is_none()
        );
    }

    #[test]
    fn known_instances_are_stored_opaquely_and_ordered_by_last_used() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        for (key, last_used) in [("beta.example", 1.0), ("alpha.example", 4.0)] {
            upsert_known_instance(
                store.connection(),
                &KnownInstanceEntry {
                    key: key.to_owned(),
                    record: RawValue::from_string(format!(
                        r#"{{"lastUsed":{last_used},"displayName":"{key}"}}"#
                    ))
                    .expect("valid JSON"),
                },
            )
            .expect("the known instance is stored");
        }

        let stored = all_known_instances(store.connection()).expect("the instances are readable");
        assert_eq!(
            stored
                .iter()
                .map(|row| row.key.as_str())
                .collect::<Vec<_>>(),
            vec!["alpha.example", "beta.example"]
        );

        delete_known_instance(store.connection(), "alpha.example")
            .expect("the instance is deleted");
        assert_eq!(
            all_known_instances(store.connection())
                .expect("the instances are readable")
                .len(),
            1
        );
    }
}
