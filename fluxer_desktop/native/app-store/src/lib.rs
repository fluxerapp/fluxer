// SPDX-License-Identifier: AGPL-3.0-or-later

mod accounts;
mod import;
mod model;
mod scoped_storage;

use crate::accounts::{AccountCompareAndSwap, AccountEntry, KnownInstanceEntry};
use crate::import::{AccountImport, EntryImport, PruneRequest};
use crate::model::{OpenOutcome, Store, StoreError};
use crate::scoped_storage::ScopedEntry;
use napi::Task;
use napi::bindgen_prelude::{AsyncTask, Env, Error, Result, Status, ToNapiValue, TypeName};
use napi_derive::napi;
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

const MAX_PAYLOAD_BYTES: usize = 64 * 1024 * 1024;

impl From<StoreError> for Error {
    fn from(error: StoreError) -> Self {
        Self::new(Status::GenericFailure, error.to_string())
    }
}

#[napi(object)]
pub struct AppStoreOptions {
    pub path: String,
    pub derivation_version: Option<String>,
}

#[derive(Serialize)]
struct Initialization<'a> {
    path: &'a str,
    #[serde(rename = "schemaVersion")]
    schema_version: i32,
    #[serde(rename = "previousSchemaVersion")]
    previous_schema_version: i32,
    #[serde(rename = "appliedMigrations")]
    applied_migrations: u32,
    #[serde(rename = "quarantinedPath")]
    quarantined_path: Option<&'a str>,
    #[serde(rename = "quarantineReason")]
    quarantine_reason: Option<&'a str>,
}

#[derive(Deserialize)]
struct ClearStoreExceptRequest {
    store: String,
    #[serde(rename = "keysToKeep")]
    keys_to_keep: Vec<String>,
}

type StoreJob<T> = Box<dyn FnOnce(&mut Store) -> Result<T> + Send>;

pub struct StoreTask<T> {
    store: Arc<Mutex<Option<Store>>>,
    job: Option<StoreJob<T>>,
}

impl<T: Send + ToNapiValue + TypeName + 'static> Task for StoreTask<T> {
    type Output = T;
    type JsValue = T;

    fn compute(&mut self) -> Result<Self::Output> {
        let job = self.job.take().ok_or_else(|| {
            Error::new(
                Status::GenericFailure,
                "app store operation was already executed",
            )
        })?;
        let mut guard = self.store.lock().map_err(|_| {
            Error::new(
                Status::GenericFailure,
                "app store lock was poisoned by an earlier panic",
            )
        })?;
        let store = guard
            .as_mut()
            .ok_or_else(|| Error::new(Status::GenericFailure, "app store is closed"))?;
        job(store)
    }

    fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
        Ok(output)
    }
}

#[napi]
pub struct AppStore {
    store: Arc<Mutex<Option<Store>>>,
    initialization: String,
}

impl AppStore {
    fn task<T: Send + ToNapiValue + TypeName + 'static>(
        &self,
        job: impl FnOnce(&mut Store) -> Result<T> + Send + 'static,
    ) -> AsyncTask<StoreTask<T>> {
        AsyncTask::new(StoreTask {
            store: Arc::clone(&self.store),
            job: Some(Box::new(job)),
        })
    }
}

#[napi]
impl AppStore {
    #[napi(constructor)]
    pub fn new(options: AppStoreOptions) -> Result<Self> {
        let store = Store::open(
            PathBuf::from(options.path),
            options.derivation_version.as_deref(),
        )?;
        let initialization = describe(store.path().to_string_lossy().as_ref(), store.outcome())?;
        Ok(Self {
            store: Arc::new(Mutex::new(Some(store))),
            initialization,
        })
    }

    #[napi(getter)]
    pub fn initialization(&self) -> String {
        self.initialization.clone()
    }

    #[napi]
    pub fn close(&self) -> Result<()> {
        let mut guard = self.store.lock().map_err(|_| {
            Error::new(
                Status::GenericFailure,
                "app store lock was poisoned by an earlier panic",
            )
        })?;
        *guard = None;
        Ok(())
    }

    #[napi]
    pub fn get_metadata(&self, key: String) -> AsyncTask<StoreTask<String>> {
        self.task(move |app_store| json(&app_store.metadata(&key)?))
    }

    #[napi]
    pub fn set_metadata(&self, key: String, value: String) -> AsyncTask<StoreTask<()>> {
        self.task(move |app_store| Ok(app_store.set_metadata(&key, &value)?))
    }

    #[napi]
    pub fn get_all_accounts(&self) -> AsyncTask<StoreTask<String>> {
        self.task(|app_store| json(&accounts::all_accounts(app_store.connection())?))
    }

    #[napi]
    pub fn get_account(&self, storage_key: String) -> AsyncTask<StoreTask<String>> {
        self.task(move |app_store| json(&accounts::account(app_store.connection(), &storage_key)?))
    }

    #[napi]
    pub fn upsert_account(&self, payload: String) -> AsyncTask<StoreTask<()>> {
        self.task(move |app_store| {
            let entry: AccountEntry = parse(&payload)?;
            Ok(accounts::upsert_account(app_store.connection(), &entry)?)
        })
    }

    #[napi]
    pub fn compare_and_swap_account(&self, payload: String) -> AsyncTask<StoreTask<bool>> {
        self.task(move |app_store| {
            let request: AccountCompareAndSwap = parse(&payload)?;
            Ok(accounts::compare_and_swap_account(
                app_store.connection_mut(),
                &request,
            )?)
        })
    }

    #[napi]
    pub fn delete_account(&self, storage_key: String) -> AsyncTask<StoreTask<()>> {
        self.task(move |app_store| {
            Ok(accounts::delete_account(
                app_store.connection(),
                &storage_key,
            )?)
        })
    }

    #[napi]
    pub fn import_accounts(&self, payload: String) -> AsyncTask<StoreTask<String>> {
        self.task(move |app_store| {
            let request: AccountImport = parse(&payload)?;
            json(&import::import_accounts(app_store, request)?)
        })
    }

    #[napi]
    pub fn get_all_known_instances(&self) -> AsyncTask<StoreTask<String>> {
        self.task(|app_store| json(&accounts::all_known_instances(app_store.connection())?))
    }

    #[napi]
    pub fn upsert_known_instance(&self, payload: String) -> AsyncTask<StoreTask<()>> {
        self.task(move |app_store| {
            let entry: KnownInstanceEntry = parse(&payload)?;
            Ok(accounts::upsert_known_instance(
                app_store.connection(),
                &entry,
            )?)
        })
    }

    #[napi]
    pub fn delete_known_instance(&self, key: String) -> AsyncTask<StoreTask<()>> {
        self.task(move |app_store| {
            Ok(accounts::delete_known_instance(
                app_store.connection(),
                &key,
            )?)
        })
    }

    #[napi]
    pub fn get_entries(&self, store: String, scope: String) -> AsyncTask<StoreTask<String>> {
        self.task(move |app_store| {
            json(&scoped_storage::scope_entries(
                app_store.connection(),
                &store,
                &scope,
            )?)
        })
    }

    #[napi]
    pub fn get_entry(
        &self,
        store: String,
        scope: String,
        key: String,
    ) -> AsyncTask<StoreTask<String>> {
        self.task(move |app_store| {
            json(&scoped_storage::entry(
                app_store.connection(),
                &store,
                &scope,
                &key,
            )?)
        })
    }

    #[napi]
    pub fn set_entry(&self, payload: String) -> AsyncTask<StoreTask<()>> {
        self.task(move |app_store| {
            let entry: ScopedEntry = parse(&payload)?;
            Ok(scoped_storage::set_entry(app_store.connection(), &entry)?)
        })
    }

    #[napi]
    pub fn delete_entry(
        &self,
        store: String,
        scope: String,
        key: String,
    ) -> AsyncTask<StoreTask<()>> {
        self.task(move |app_store| {
            Ok(scoped_storage::delete_entry(
                app_store.connection(),
                &store,
                &scope,
                &key,
            )?)
        })
    }

    #[napi]
    pub fn clear_scope(&self, scope: String) -> AsyncTask<StoreTask<()>> {
        self.task(move |app_store| Ok(scoped_storage::clear_scope(app_store.connection(), &scope)?))
    }

    #[napi]
    pub fn clear_store_except(&self, payload: String) -> AsyncTask<StoreTask<()>> {
        self.task(move |app_store| {
            let request: ClearStoreExceptRequest = parse(&payload)?;
            Ok(scoped_storage::clear_store_except(
                app_store.connection(),
                &request.store,
                &request.keys_to_keep,
            )?)
        })
    }

    #[napi]
    pub fn import_entries(&self, payload: String) -> AsyncTask<StoreTask<String>> {
        self.task(move |app_store| {
            let request: EntryImport = parse(&payload)?;
            json(&import::import_entries(app_store, request)?)
        })
    }

    #[napi]
    pub fn prune(&self, payload: String) -> AsyncTask<StoreTask<String>> {
        self.task(move |app_store| {
            let request: PruneRequest = parse(&payload)?;
            json(&import::prune(app_store, request)?)
        })
    }
}

fn describe(path: &str, outcome: &OpenOutcome) -> Result<String> {
    json(&Initialization {
        path,
        schema_version: outcome.schema_version,
        previous_schema_version: outcome.previous_schema_version,
        applied_migrations: outcome.applied_migrations,
        quarantined_path: outcome.quarantined_path.as_deref(),
        quarantine_reason: outcome.quarantine_reason,
    })
}

fn json<T: Serialize>(value: &T) -> Result<String> {
    Ok(serde_json::to_string(value).map_err(StoreError::from)?)
}

fn parse<T: DeserializeOwned>(payload: &str) -> Result<T> {
    if payload.len() > MAX_PAYLOAD_BYTES {
        return Err(Error::new(
            Status::GenericFailure,
            format!("app store payload exceeds {MAX_PAYLOAD_BYTES} bytes"),
        ));
    }
    Ok(serde_json::from_str(payload).map_err(StoreError::from)?)
}
