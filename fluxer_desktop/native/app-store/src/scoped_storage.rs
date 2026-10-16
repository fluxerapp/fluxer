// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::model::{
    CapacityBudget, MAX_SCOPE_BYTES, MAX_SCOPE_ENTRIES, MAX_SCOPED_BYTES, MAX_SCOPED_ENTRIES,
    MAX_SCOPED_VALUE_BYTES, StoreError, WriteRefusal, check_lookup_key, require_lookup_key,
};
use rusqlite::{Connection, OptionalExtension, params};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Deserialize, Serialize)]
pub struct ScopedEntry {
    pub store: String,
    pub scope: String,
    pub key: String,
    pub value: String,
    #[serde(rename = "updatedAt")]
    pub updated_at: f64,
}

pub struct EntryBudgets {
    global: CapacityBudget,
    scopes: HashMap<(String, String), CapacityBudget>,
}

impl EntryBudgets {
    pub fn read(conn: &Connection) -> Result<Self, StoreError> {
        Ok(Self {
            global: CapacityBudget::read(
                conn,
                "scoped storage",
                "SELECT COUNT(*), COALESCE(SUM(length(CAST(store AS BLOB)) + length(CAST(scope AS BLOB)) + length(CAST(entry_key AS BLOB)) + length(CAST(value AS BLOB))), 0) FROM scoped_storage",
                [],
                MAX_SCOPED_ENTRIES,
                MAX_SCOPED_BYTES,
            )?,
            scopes: HashMap::new(),
        })
    }

    fn scope(
        &mut self,
        conn: &Connection,
        store: &str,
        scope: &str,
    ) -> Result<&mut CapacityBudget, StoreError> {
        let identity = (store.to_owned(), scope.to_owned());
        if !self.scopes.contains_key(&identity) {
            let budget = CapacityBudget::read(
                conn,
                "scoped storage store/scope",
                "SELECT COUNT(*), COALESCE(SUM(length(CAST(entry_key AS BLOB)) + length(CAST(value AS BLOB))), 0) FROM scoped_storage WHERE store = ?1 AND scope = ?2",
                params![store, scope],
                MAX_SCOPE_ENTRIES,
                MAX_SCOPE_BYTES,
            )?;
            self.scopes.insert(identity.clone(), budget);
        }
        Ok(self
            .scopes
            .get_mut(&identity)
            .expect("the scope budget was just inserted"))
    }
}

fn check_entry(entry: &ScopedEntry) -> Option<WriteRefusal> {
    check_lookup_key(&entry.store, "scoped storage store is unusable")
        .or_else(|| check_lookup_key(&entry.scope, "scoped storage scope is unusable"))
        .or_else(|| check_lookup_key(&entry.key, "scoped storage key is unusable"))
        .or_else(|| {
            (entry.value.len() > MAX_SCOPED_VALUE_BYTES)
                .then_some(WriteRefusal::Invalid("scoped storage value is too large"))
        })
}

pub fn write_entry(
    conn: &Connection,
    budgets: &mut EntryBudgets,
    entry: &ScopedEntry,
) -> Result<Option<WriteRefusal>, StoreError> {
    if let Some(refusal) = check_entry(entry) {
        return Ok(Some(refusal));
    }
    let replaces: Option<i64> = conn
        .query_row(
            "SELECT length(CAST(entry_key AS BLOB)) + length(CAST(value AS BLOB)) FROM scoped_storage WHERE store = ?1 AND scope = ?2 AND entry_key = ?3",
            params![entry.store, entry.scope, entry.key],
            |row| row.get(0),
        )
        .optional()?;
    let scope_incoming = (entry.key.len() + entry.value.len()) as i64;
    let global_incoming = (entry.store.len() + entry.scope.len()) as i64 + scope_incoming;
    let global_replaces =
        replaces.map(|bytes| bytes + (entry.store.len() + entry.scope.len()) as i64);
    if let Err(refusal) = budgets.global.check(global_replaces, global_incoming) {
        return Ok(Some(refusal.into()));
    }
    let scope_budget = budgets.scope(conn, &entry.store, &entry.scope)?;
    if let Err(refusal) = scope_budget.check(replaces, scope_incoming) {
        return Ok(Some(refusal.into()));
    }
    scope_budget.commit(replaces, scope_incoming);
    budgets.global.commit(global_replaces, global_incoming);
    conn.execute(
        "INSERT INTO scoped_storage(store, scope, entry_key, value, updated_at) VALUES (?1, ?2, ?3, ?4, ?5)
         ON CONFLICT(store, scope, entry_key) DO UPDATE SET
            value = excluded.value,
            updated_at = excluded.updated_at",
        params![
            entry.store,
            entry.scope,
            entry.key,
            entry.value,
            entry.updated_at
        ],
    )?;
    Ok(None)
}

pub fn set_entry(conn: &Connection, entry: &ScopedEntry) -> Result<(), StoreError> {
    let mut budgets = EntryBudgets::read(conn)?;
    match write_entry(conn, &mut budgets, entry)? {
        None => Ok(()),
        Some(refusal) => Err(refusal.into()),
    }
}

pub fn scope_entries(
    conn: &Connection,
    store: &str,
    scope: &str,
) -> Result<Vec<ScopedEntry>, StoreError> {
    require_lookup_key(store, "scoped storage store is unusable")?;
    require_lookup_key(scope, "scoped storage scope is unusable")?;
    let mut statement = conn.prepare(
        "SELECT entry_key, value, updated_at FROM scoped_storage WHERE store = ?1 AND scope = ?2 ORDER BY entry_key ASC",
    )?;
    let mut rows = statement.query(params![store, scope])?;
    let mut entries = Vec::new();
    while let Some(row) = rows.next()? {
        entries.push(ScopedEntry {
            store: store.to_owned(),
            scope: scope.to_owned(),
            key: row.get(0)?,
            value: row.get(1)?,
            updated_at: row.get(2)?,
        });
    }
    Ok(entries)
}

pub fn entry(
    conn: &Connection,
    store: &str,
    scope: &str,
    key: &str,
) -> Result<Option<ScopedEntry>, StoreError> {
    Ok(conn
        .query_row(
            "SELECT value, updated_at FROM scoped_storage WHERE store = ?1 AND scope = ?2 AND entry_key = ?3",
            params![store, scope, key],
            |row| {
                Ok(ScopedEntry {
                    store: store.to_owned(),
                    scope: scope.to_owned(),
                    key: key.to_owned(),
                    value: row.get(0)?,
                    updated_at: row.get(1)?,
                })
            },
        )
        .optional()?)
}

pub fn delete_entry(
    conn: &Connection,
    store: &str,
    scope: &str,
    key: &str,
) -> Result<(), StoreError> {
    conn.execute(
        "DELETE FROM scoped_storage WHERE store = ?1 AND scope = ?2 AND entry_key = ?3",
        params![store, scope, key],
    )?;
    Ok(())
}

pub fn clear_scope(conn: &Connection, scope: &str) -> Result<(), StoreError> {
    require_lookup_key(scope, "scoped storage scope is unusable")?;
    conn.execute(
        "DELETE FROM scoped_storage WHERE scope = ?1",
        params![scope],
    )?;
    Ok(())
}

pub fn clear_store_except(
    conn: &Connection,
    store: &str,
    keys_to_keep: &[String],
) -> Result<(), StoreError> {
    require_lookup_key(store, "scoped storage store is unusable")?;
    if keys_to_keep.is_empty() {
        conn.execute(
            "DELETE FROM scoped_storage WHERE store = ?1",
            params![store],
        )?;
        return Ok(());
    }
    let placeholders = vec!["?"; keys_to_keep.len()].join(", ");
    let mut args: Vec<&dyn rusqlite::ToSql> = Vec::with_capacity(keys_to_keep.len() + 1);
    args.push(&store);
    for key in keys_to_keep {
        args.push(key);
    }
    conn.execute(
        &format!(
            "DELETE FROM scoped_storage WHERE store = ? AND entry_key NOT IN ({placeholders})"
        ),
        args.as_slice(),
    )?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Store, temporary_store_path};

    fn entry(store: &str, scope: &str, key: &str, value: &str) -> ScopedEntry {
        ScopedEntry {
            store: store.to_owned(),
            scope: scope.to_owned(),
            key: key.to_owned(),
            value: value.to_owned(),
            updated_at: 1.0,
        }
    }

    #[test]
    fn entries_round_trip_and_are_ordered_by_key() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        for key in ["zeta", "alpha", "mu"] {
            set_entry(store.connection(), &entry("app", "a::1", key, key))
                .expect("the entry is stored");
        }

        let stored =
            scope_entries(store.connection(), "app", "a::1").expect("entries are readable");
        assert_eq!(
            stored
                .iter()
                .map(|row| row.key.as_str())
                .collect::<Vec<_>>(),
            vec!["alpha", "mu", "zeta"]
        );
        assert_eq!(
            entry_value(store.connection(), "app", "a::1", "mu"),
            Some("mu".to_owned())
        );

        set_entry(store.connection(), &entry("app", "a::1", "mu", "updated"))
            .expect("the entry is replaced");
        assert_eq!(
            entry_value(store.connection(), "app", "a::1", "mu"),
            Some("updated".to_owned())
        );
        assert_eq!(
            scope_entries(store.connection(), "app", "a::1")
                .expect("entries are readable")
                .len(),
            3,
            "a replacement must not grow the scope"
        );

        delete_entry(store.connection(), "app", "a::1", "mu").expect("the entry is deleted");
        assert_eq!(entry_value(store.connection(), "app", "a::1", "mu"), None);
    }

    #[test]
    fn clearing_a_scope_leaves_every_other_scope_alone() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        set_entry(store.connection(), &entry("app", "a::1", "k", "mine"))
            .expect("the entry is stored");
        set_entry(store.connection(), &entry("voice", "a::1", "k", "mine"))
            .expect("the entry is stored");
        set_entry(store.connection(), &entry("app", "global", "k", "shared"))
            .expect("the entry is stored");

        clear_scope(store.connection(), "a::1").expect("the scope is cleared");
        assert!(
            scope_entries(store.connection(), "app", "a::1")
                .expect("entries are readable")
                .is_empty()
        );
        assert!(
            scope_entries(store.connection(), "voice", "a::1")
                .expect("entries are readable")
                .is_empty()
        );
        assert_eq!(
            entry_value(store.connection(), "app", "global", "k"),
            Some("shared".to_owned())
        );
    }

    #[test]
    fn clearing_a_store_except_some_keys_spans_every_scope() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        for (scope, key) in [("a::1", "keep"), ("a::1", "drop"), ("global", "drop")] {
            set_entry(store.connection(), &entry("app", scope, key, "v"))
                .expect("the entry is stored");
        }
        set_entry(store.connection(), &entry("voice", "a::1", "drop", "v"))
            .expect("the entry is stored");

        clear_store_except(store.connection(), "app", &["keep".to_owned()])
            .expect("the store is cleared");
        assert_eq!(
            entry_value(store.connection(), "app", "a::1", "keep"),
            Some("v".to_owned())
        );
        assert_eq!(entry_value(store.connection(), "app", "a::1", "drop"), None);
        assert_eq!(
            entry_value(store.connection(), "app", "global", "drop"),
            None
        );
        assert_eq!(
            entry_value(store.connection(), "voice", "a::1", "drop"),
            Some("v".to_owned()),
            "another store must be untouched"
        );

        clear_store_except(store.connection(), "voice", &[]).expect("the store is cleared");
        assert_eq!(
            entry_value(store.connection(), "voice", "a::1", "drop"),
            None
        );
    }

    #[test]
    fn an_oversized_value_is_refused_before_it_is_inserted() {
        let (_dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        let oversized = "x".repeat(MAX_SCOPED_VALUE_BYTES + 1);

        let mut budgets = EntryBudgets::read(store.connection()).expect("budgets are readable");
        let refusal = write_entry(
            store.connection(),
            &mut budgets,
            &entry("app", "a::1", "big", &oversized),
        )
        .expect("the write completes")
        .expect("the write is refused");
        assert!(refusal.to_string().contains("too large"));
        assert!(
            scope_entries(store.connection(), "app", "a::1")
                .expect("entries are readable")
                .is_empty(),
            "a refused entry must never reach the table"
        );
    }

    fn entry_value(conn: &Connection, store: &str, scope: &str, key: &str) -> Option<String> {
        super::entry(conn, store, scope, key)
            .expect("the entry is readable")
            .map(|row| row.value)
    }
}
