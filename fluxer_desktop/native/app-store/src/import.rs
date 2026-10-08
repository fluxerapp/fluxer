// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::accounts::{
    AccountEntry, AccountWrite, account_budget, delete_account_rows, write_account,
};
use crate::model::{Store, StoreError, write_metadata};
use crate::scoped_storage::{EntryBudgets, ScopedEntry, write_entry};
use rusqlite::TransactionBehavior;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

#[derive(Deserialize)]
pub struct Marker {
    pub key: String,
    pub value: String,
}

#[derive(Deserialize)]
pub struct AccountImport {
    pub records: Vec<AccountEntry>,
    #[serde(default)]
    pub marker: Option<Marker>,
}

#[derive(Deserialize)]
pub struct EntryImport {
    pub entries: Vec<ScopedEntry>,
    #[serde(default)]
    pub marker: Option<Marker>,
}

#[derive(Deserialize)]
pub struct PruneRequest {
    #[serde(rename = "knownStorageKeys")]
    pub known_storage_keys: Vec<String>,
    #[serde(rename = "listIsAuthoritative")]
    pub list_is_authoritative: bool,
}

#[derive(Serialize)]
pub struct SkippedRecord {
    pub key: String,
    pub reason: String,
}

#[derive(Serialize)]
pub struct AccountImportReport {
    pub imported: u32,
    pub skipped: Vec<SkippedRecord>,
    #[serde(rename = "unusableInstances")]
    pub unusable_instances: Vec<String>,
}

#[derive(Serialize)]
pub struct EntryImportReport {
    pub imported: u32,
    pub skipped: Vec<SkippedRecord>,
}

#[derive(Serialize)]
pub struct PruneReport {
    pub pruned: Vec<String>,
    #[serde(rename = "refusedReason")]
    pub refused_reason: Option<&'static str>,
}

pub fn import_accounts(
    store: &mut Store,
    request: AccountImport,
) -> Result<AccountImportReport, StoreError> {
    let transaction = store
        .connection_mut()
        .transaction_with_behavior(TransactionBehavior::Immediate)?;
    let mut budget = account_budget(&transaction)?;
    let mut report = AccountImportReport {
        imported: 0,
        skipped: Vec::new(),
        unusable_instances: Vec::new(),
    };
    for entry in &request.records {
        match write_account(&transaction, &mut budget, entry)? {
            AccountWrite::Written { instance_usable } => {
                report.imported += 1;
                if !instance_usable {
                    report.unusable_instances.push(entry.storage_key.clone());
                }
            }
            AccountWrite::Refused(refusal) => report.skipped.push(SkippedRecord {
                key: entry.storage_key.clone(),
                reason: refusal.to_string(),
            }),
        }
    }
    if let Some(marker) = &request.marker {
        write_metadata(&transaction, &marker.key, &marker.value)?;
    }
    transaction.commit()?;
    Ok(report)
}

pub fn import_entries(
    store: &mut Store,
    request: EntryImport,
) -> Result<EntryImportReport, StoreError> {
    let transaction = store
        .connection_mut()
        .transaction_with_behavior(TransactionBehavior::Immediate)?;
    let mut budgets = EntryBudgets::read(&transaction)?;
    let mut report = EntryImportReport {
        imported: 0,
        skipped: Vec::new(),
    };
    for entry in &request.entries {
        match write_entry(&transaction, &mut budgets, entry)? {
            None => report.imported += 1,
            Some(refusal) => report.skipped.push(SkippedRecord {
                key: format!("{}/{}/{}", entry.store, entry.scope, entry.key),
                reason: refusal.to_string(),
            }),
        }
    }
    if let Some(marker) = &request.marker {
        write_metadata(&transaction, &marker.key, &marker.value)?;
    }
    transaction.commit()?;
    Ok(report)
}

pub fn prune(store: &mut Store, request: PruneRequest) -> Result<PruneReport, StoreError> {
    if !request.list_is_authoritative {
        return Ok(refused("the known account list was not authoritative"));
    }
    let transaction = store
        .connection_mut()
        .transaction_with_behavior(TransactionBehavior::Immediate)?;
    let stored = {
        let mut statement = transaction.prepare("SELECT storage_key FROM accounts")?;
        let mut rows = statement.query([])?;
        let mut stored = Vec::new();
        while let Some(row) = rows.next()? {
            stored.push(row.get::<_, String>(0)?);
        }
        stored
    };
    if request.known_storage_keys.is_empty() && !stored.is_empty() {
        return Ok(refused(
            "the known account list was empty while the store holds accounts",
        ));
    }
    let known = request
        .known_storage_keys
        .iter()
        .map(String::as_str)
        .collect::<HashSet<_>>();
    let mut pruned = Vec::new();
    for storage_key in stored {
        if known.contains(storage_key.as_str()) {
            continue;
        }
        delete_account_rows(&transaction, &storage_key)?;
        pruned.push(storage_key);
    }
    transaction.commit()?;
    Ok(PruneReport {
        pruned,
        refused_reason: None,
    })
}

fn refused(reason: &'static str) -> PruneReport {
    PruneReport {
        pruned: Vec::new(),
        refused_reason: Some(reason),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::accounts::{account, all_accounts};
    use crate::model::{MAX_SCOPED_VALUE_BYTES, temporary_store_path};
    use crate::scoped_storage::{entry, scope_entries};
    use serde_json::value::RawValue;

    fn open() -> (tempfile::TempDir, Store) {
        let (dir, path) = temporary_store_path();
        let store = Store::open(path, None).expect("a fresh store opens");
        (dir, store)
    }

    fn account_entry(storage_key: &str, record: &str) -> AccountEntry {
        AccountEntry {
            storage_key: storage_key.to_owned(),
            record: RawValue::from_string(record.to_owned()).expect("valid JSON"),
        }
    }

    fn scoped_entry(scope: &str, key: &str, value: &str) -> ScopedEntry {
        ScopedEntry {
            store: "app".to_owned(),
            scope: scope.to_owned(),
            key: key.to_owned(),
            value: value.to_owned(),
            updated_at: 1.0,
        }
    }

    #[test]
    fn importing_accounts_commits_the_records_and_the_phase_marker_together() {
        let (_dir, mut store) = open();
        let request = AccountImport {
            records: vec![
                account_entry(
                    "a::1",
                    r#"{"userId":"1","lastActive":2,"instance":{"apiEndpoint":"https://a/api"}}"#,
                ),
                account_entry("a::2", r#"{"userId":"2","lastActive":1}"#),
            ],
            marker: Some(Marker {
                key: "legacy_import.v1".to_owned(),
                value: r#"{"phase":"entries"}"#.to_owned(),
            }),
        };

        let report = import_accounts(&mut store, request).expect("the import commits");
        assert_eq!(report.imported, 2);
        assert!(report.skipped.is_empty());
        assert_eq!(report.unusable_instances, vec!["a::2".to_owned()]);
        assert_eq!(
            store
                .metadata("legacy_import.v1")
                .expect("metadata is readable"),
            Some(r#"{"phase":"entries"}"#.to_owned())
        );

        let again = import_accounts(
            &mut store,
            AccountImport {
                records: vec![account_entry("a::1", r#"{"userId":"1","lastActive":9}"#)],
                marker: None,
            },
        )
        .expect("a repeated import commits");
        assert_eq!(again.imported, 1);
        assert_eq!(
            all_accounts(store.connection())
                .expect("the accounts are readable")
                .len(),
            2,
            "re-importing a record must replace it, never duplicate it"
        );
    }

    #[test]
    fn importing_accounts_skips_and_reports_a_bad_record_without_losing_the_batch() {
        let (_dir, mut store) = open();
        let report = import_accounts(
            &mut store,
            AccountImport {
                records: vec![
                    account_entry("a::1", r#"{"userId":"1"}"#),
                    account_entry("", r#"{"userId":"2"}"#),
                    account_entry("a::3", r#""not-an-object""#),
                    account_entry("a::4", r#"{"userId":"4"}"#),
                ],
                marker: None,
            },
        )
        .expect("the import commits");

        assert_eq!(report.imported, 2);
        assert_eq!(
            report
                .skipped
                .iter()
                .map(|skipped| skipped.key.as_str())
                .collect::<Vec<_>>(),
            vec!["", "a::3"]
        );
        assert!(
            account(store.connection(), "a::4")
                .expect("the account is readable")
                .is_some()
        );
    }

    #[test]
    fn importing_entries_skips_and_reports_a_bad_record_and_commits_the_rest() {
        let (_dir, mut store) = open();
        let oversized = "x".repeat(MAX_SCOPED_VALUE_BYTES + 1);
        let report = import_entries(
            &mut store,
            EntryImport {
                entries: vec![
                    scoped_entry("a::1", "theme", "dark"),
                    scoped_entry("a::1", "", "no key"),
                    scoped_entry("a::1", "huge", &oversized),
                    scoped_entry("a::1", "zoom", "1.25"),
                ],
                marker: Some(Marker {
                    key: "legacy_import.v1".to_owned(),
                    value: r#"{"phase":"session"}"#.to_owned(),
                }),
            },
        )
        .expect("the import commits");

        assert_eq!(report.imported, 2);
        assert_eq!(report.skipped.len(), 2);
        assert!(report.skipped[0].reason.contains("key is unusable"));
        assert!(report.skipped[1].reason.contains("too large"));
        assert_eq!(
            scope_entries(store.connection(), "app", "a::1")
                .expect("entries are readable")
                .len(),
            2,
            "one oversized entry must not abort the batch"
        );
        assert_eq!(
            store
                .metadata("legacy_import.v1")
                .expect("metadata is readable"),
            Some(r#"{"phase":"session"}"#.to_owned())
        );
    }

    #[test]
    fn a_failure_inside_a_phase_rolls_back_the_data_and_the_marker_together() {
        let (_dir, mut store) = open();
        store
            .set_metadata("legacy_import.v1", r#"{"phase":"accounts"}"#)
            .expect("metadata is writable");
        store
            .connection()
            .execute_batch("DROP TABLE accounts")
            .expect("the table is dropped");

        let failure = import_accounts(
            &mut store,
            AccountImport {
                records: vec![account_entry("a::1", r#"{"userId":"1"}"#)],
                marker: Some(Marker {
                    key: "legacy_import.v1".to_owned(),
                    value: r#"{"phase":"entries"}"#.to_owned(),
                }),
            },
        );

        assert!(failure.is_err());
        assert_eq!(
            store
                .metadata("legacy_import.v1")
                .expect("metadata is readable"),
            Some(r#"{"phase":"accounts"}"#.to_owned()),
            "an interrupted phase must re-run rather than be marked complete"
        );
    }

    #[test]
    fn pruning_refuses_a_non_authoritative_or_transiently_empty_list() {
        let (_dir, mut store) = open();
        import_accounts(
            &mut store,
            AccountImport {
                records: vec![account_entry("a::1", r#"{"userId":"1"}"#)],
                marker: None,
            },
        )
        .expect("the import commits");

        let not_authoritative = prune(
            &mut store,
            PruneRequest {
                known_storage_keys: vec!["a::1".to_owned()],
                list_is_authoritative: false,
            },
        )
        .expect("prune completes");
        assert_eq!(
            not_authoritative.refused_reason,
            Some("the known account list was not authoritative")
        );

        let transiently_empty = prune(
            &mut store,
            PruneRequest {
                known_storage_keys: Vec::new(),
                list_is_authoritative: true,
            },
        )
        .expect("prune completes");
        assert_eq!(
            transiently_empty.refused_reason,
            Some("the known account list was empty while the store holds accounts")
        );
        assert_eq!(
            all_accounts(store.connection())
                .expect("the accounts are readable")
                .len(),
            1,
            "a refused prune must delete nothing"
        );
    }

    #[test]
    fn pruning_an_authoritative_list_removes_orphans_and_their_scoped_rows() {
        let (_dir, mut store) = open();
        import_accounts(
            &mut store,
            AccountImport {
                records: vec![
                    account_entry("a::1", r#"{"userId":"1"}"#),
                    account_entry("a::2", r#"{"userId":"2"}"#),
                ],
                marker: None,
            },
        )
        .expect("the import commits");
        import_entries(
            &mut store,
            EntryImport {
                entries: vec![
                    scoped_entry("a::1", "theme", "dark"),
                    scoped_entry("a::2", "theme", "light"),
                    scoped_entry("global", "theme", "system"),
                ],
                marker: None,
            },
        )
        .expect("the import commits");

        let report = prune(
            &mut store,
            PruneRequest {
                known_storage_keys: vec!["a::1".to_owned()],
                list_is_authoritative: true,
            },
        )
        .expect("prune completes");

        assert_eq!(report.pruned, vec!["a::2".to_owned()]);
        assert_eq!(report.refused_reason, None);
        assert!(
            account(store.connection(), "a::2")
                .expect("the account is readable")
                .is_none()
        );
        assert!(
            scope_entries(store.connection(), "app", "a::2")
                .expect("entries are readable")
                .is_empty()
        );
        assert!(
            entry(store.connection(), "app", "global", "theme")
                .expect("the entry is readable")
                .is_some()
        );
        assert!(
            entry(store.connection(), "app", "a::1", "theme")
                .expect("the entry is readable")
                .is_some()
        );
    }

    #[test]
    fn pruning_an_empty_store_against_an_empty_list_is_allowed() {
        let (_dir, mut store) = open();
        let report = prune(
            &mut store,
            PruneRequest {
                known_storage_keys: Vec::new(),
                list_is_authoritative: true,
            },
        )
        .expect("prune completes");
        assert_eq!(report.refused_reason, None);
        assert!(report.pruned.is_empty());
    }
}
