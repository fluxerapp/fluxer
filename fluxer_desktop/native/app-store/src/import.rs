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
