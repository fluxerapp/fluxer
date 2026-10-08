// SPDX-License-Identifier: AGPL-3.0-or-later

use rusqlite::{Connection, ErrorCode, OptionalExtension, TransactionBehavior, params};
use std::ffi::OsString;
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

pub const SCHEMA_VERSION: i32 = 1;

pub const MAX_ACCOUNTS: i64 = 1024;
pub const MAX_ACCOUNT_BYTES: i64 = 64 * 1024 * 1024;
pub const MAX_KNOWN_INSTANCES: i64 = 1024;
pub const MAX_KNOWN_INSTANCE_BYTES: i64 = 64 * 1024 * 1024;
pub const MAX_SCOPED_ENTRIES: i64 = 65_536;
pub const MAX_SCOPED_BYTES: i64 = 256 * 1024 * 1024;
pub const MAX_SCOPE_ENTRIES: i64 = 4096;
pub const MAX_SCOPE_BYTES: i64 = 64 * 1024 * 1024;
pub const MAX_METADATA_ROWS: i64 = 1024;
pub const MAX_METADATA_BYTES: i64 = 8 * 1024 * 1024;
pub const MAX_LOOKUP_KEY_BYTES: usize = 1024;
pub const MAX_SCOPED_VALUE_BYTES: usize = 8 * 1024 * 1024;
pub const MAX_INSTANCE_BYTES: usize = 256 * 1024;

pub const METADATA_SCHEMA_VERSION: &str = "schema.version";
pub const METADATA_SCHEMA_APPLIED_AT: &str = "schema.applied_at";
pub const METADATA_DERIVATION_VERSION: &str = "derivation.version";

const INTERNAL_METADATA_KEYS: &[&str] = &[
    METADATA_SCHEMA_VERSION,
    METADATA_SCHEMA_APPLIED_AT,
    METADATA_DERIVATION_VERSION,
];

const BUSY_TIMEOUT: Duration = Duration::from_secs(5);
const SIDECAR_SUFFIXES: &[&str] = &["", "-journal", "-wal", "-shm"];

pub struct TableShape {
    pub name: &'static str,
    pub columns: &'static [&'static str],
}

pub struct IndexShape {
    pub name: &'static str,
    pub sql: &'static str,
}

const TABLES: &[TableShape] = &[
    TableShape {
        name: "store_metadata",
        columns: &["key", "value"],
    },
    TableShape {
        name: "accounts",
        columns: &["storage_key", "user_id", "last_active", "record_json"],
    },
    TableShape {
        name: "known_instances",
        columns: &["key", "last_used", "record_json"],
    },
    TableShape {
        name: "scoped_storage",
        columns: &["store", "scope", "entry_key", "value", "updated_at"],
    },
];

const INDEXES: &[IndexShape] = &[
    IndexShape {
        name: "accounts_last_active_idx",
        sql: "CREATE INDEX accounts_last_active_idx ON accounts(last_active DESC, storage_key ASC)",
    },
    IndexShape {
        name: "known_instances_last_used_idx",
        sql: "CREATE INDEX known_instances_last_used_idx ON known_instances(last_used DESC, key ASC)",
    },
    IndexShape {
        name: "scoped_storage_scope_idx",
        sql: "CREATE INDEX scoped_storage_scope_idx ON scoped_storage(scope, store, entry_key)",
    },
];

const BASELINE_TABLES: &[&str] = &[
    "CREATE TABLE store_metadata (key TEXT PRIMARY KEY NOT NULL, value TEXT NOT NULL)",
    "CREATE TABLE accounts (storage_key TEXT PRIMARY KEY NOT NULL, user_id TEXT NOT NULL, last_active REAL NOT NULL, record_json TEXT NOT NULL)",
    "CREATE TABLE known_instances (key TEXT PRIMARY KEY NOT NULL, last_used REAL NOT NULL, record_json TEXT NOT NULL)",
    "CREATE TABLE scoped_storage (store TEXT NOT NULL, scope TEXT NOT NULL, entry_key TEXT NOT NULL, value TEXT NOT NULL, updated_at REAL NOT NULL, PRIMARY KEY(store, scope, entry_key))",
];

pub struct Migration {
    pub from: i32,
    pub to: i32,
    pub statements: &'static [&'static str],
}

pub const MIGRATIONS: &[Migration] = &[];

const fn ladder_is_contiguous(migrations: &[Migration], target: i32) -> bool {
    if migrations.is_empty() {
        return target == 1;
    }
    if migrations[0].from != 1 {
        return false;
    }
    let mut index = 0;
    while index < migrations.len() {
        if migrations[index].to != migrations[index].from + 1 {
            return false;
        }
        if index > 0 && migrations[index].from != migrations[index - 1].to {
            return false;
        }
        index += 1;
    }
    migrations[migrations.len() - 1].to == target
}

const _: () = assert!(
    ladder_is_contiguous(MIGRATIONS, SCHEMA_VERSION),
    "app store migrations must be contiguous and end at SCHEMA_VERSION"
);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum QuarantineReason {
    Newer,
    Schema,
    Corrupt,
    Derivation,
}

impl QuarantineReason {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Newer => "newer",
            Self::Schema => "schema",
            Self::Corrupt => "corrupt",
            Self::Derivation => "derivation",
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CapacityRefusal {
    pub resource: &'static str,
    pub limit: i64,
    pub unit: &'static str,
}

impl Display for CapacityRefusal {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{} exceeds {} {}",
            self.resource, self.limit, self.unit
        )
    }
}

#[derive(Debug)]
pub enum StoreError {
    PathHasNoParent(PathBuf),
    Io {
        path: PathBuf,
        source: std::io::Error,
    },
    Sqlite(rusqlite::Error),
    Json(serde_json::Error),
    Capacity(CapacityRefusal),
    Refused(&'static str),
}

impl Display for StoreError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::PathHasNoParent(path) => write!(
                formatter,
                "{}: app store path must have a parent directory",
                path.display()
            ),
            Self::Io { path, source } => write!(formatter, "{}: {source}", path.display()),
            Self::Sqlite(source) => write!(formatter, "{source}"),
            Self::Json(source) => write!(formatter, "{source}"),
            Self::Capacity(refusal) => write!(formatter, "{refusal}"),
            Self::Refused(reason) => write!(formatter, "{reason}"),
        }
    }
}

impl std::error::Error for StoreError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io { source, .. } => Some(source),
            Self::Sqlite(source) => Some(source),
            Self::Json(source) => Some(source),
            _ => None,
        }
    }
}

impl From<rusqlite::Error> for StoreError {
    fn from(source: rusqlite::Error) -> Self {
        Self::Sqlite(source)
    }
}

impl From<serde_json::Error> for StoreError {
    fn from(source: serde_json::Error) -> Self {
        Self::Json(source)
    }
}

impl From<CapacityRefusal> for StoreError {
    fn from(refusal: CapacityRefusal) -> Self {
        Self::Capacity(refusal)
    }
}

fn io_error(path: &Path, source: std::io::Error) -> StoreError {
    StoreError::Io {
        path: path.to_owned(),
        source,
    }
}

enum OpenFailure {
    Quarantine(QuarantineReason),
    Fatal(StoreError),
}

impl From<StoreError> for OpenFailure {
    fn from(error: StoreError) -> Self {
        Self::Fatal(error)
    }
}

impl From<rusqlite::Error> for OpenFailure {
    fn from(error: rusqlite::Error) -> Self {
        match &error {
            rusqlite::Error::SqliteFailure(failure, _)
                if matches!(
                    failure.code,
                    ErrorCode::DatabaseCorrupt | ErrorCode::NotADatabase
                ) =>
            {
                Self::Quarantine(QuarantineReason::Corrupt)
            }
            _ => Self::Fatal(StoreError::Sqlite(error)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct OpenOutcome {
    pub schema_version: i32,
    pub previous_schema_version: i32,
    pub applied_migrations: u32,
    pub quarantined_path: Option<String>,
    pub quarantine_reason: Option<&'static str>,
}

pub struct Store {
    conn: Connection,
    path: PathBuf,
    outcome: OpenOutcome,
}

impl Store {
    pub fn open(path: PathBuf, derivation_version: Option<&str>) -> Result<Self, StoreError> {
        let parent = path
            .parent()
            .ok_or_else(|| StoreError::PathHasNoParent(path.clone()))?
            .to_owned();
        fs::create_dir_all(&parent).map_err(|source| io_error(&parent, source))?;
        restrict_path(&parent, PathKind::Directory);
        match Self::try_open(&path, derivation_version) {
            Ok(store) => Ok(store),
            Err(OpenFailure::Fatal(error)) => Err(error),
            Err(OpenFailure::Quarantine(reason)) => {
                let quarantined = quarantine(&path, reason)?;
                let mut store =
                    Self::try_open(&path, derivation_version).map_err(|failure| match failure {
                        OpenFailure::Fatal(error) => error,
                        OpenFailure::Quarantine(_) => {
                            StoreError::Refused("a freshly created app store still failed to open")
                        }
                    })?;
                store.outcome.quarantined_path = Some(quarantined);
                store.outcome.quarantine_reason = Some(reason.as_str());
                Ok(store)
            }
        }
    }

    fn try_open(path: &Path, derivation_version: Option<&str>) -> Result<Self, OpenFailure> {
        let mut conn = Connection::open(path)?;
        restrict_path(path, PathKind::File);
        conn.busy_timeout(BUSY_TIMEOUT)?;
        conn.pragma_update(None, "foreign_keys", true)?;
        let outcome = prepare_schema(&mut conn)?;
        require_derivation_version(&conn, derivation_version)?;
        Ok(Self {
            conn,
            path: path.to_owned(),
            outcome,
        })
    }

    pub fn outcome(&self) -> &OpenOutcome {
        &self.outcome
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn connection(&self) -> &Connection {
        &self.conn
    }

    pub fn connection_mut(&mut self) -> &mut Connection {
        &mut self.conn
    }

    pub fn metadata(&self, key: &str) -> Result<Option<String>, StoreError> {
        Ok(self
            .conn
            .query_row(
                "SELECT value FROM store_metadata WHERE key = ?1",
                params![key],
                |row| row.get::<_, String>(0),
            )
            .optional()?)
    }

    pub fn set_metadata(&self, key: &str, value: &str) -> Result<(), StoreError> {
        write_metadata(&self.conn, key, value)
    }
}

pub fn write_metadata(conn: &Connection, key: &str, value: &str) -> Result<(), StoreError> {
    if INTERNAL_METADATA_KEYS.contains(&key) {
        return Err(StoreError::Refused("store metadata key is reserved"));
    }
    require_lookup_key(key, "store metadata key is unusable")?;
    let budget = CapacityBudget::read(
        conn,
        "store metadata",
        "SELECT COUNT(*), COALESCE(SUM(length(CAST(key AS BLOB)) + length(CAST(value AS BLOB))), 0) FROM store_metadata",
        [],
        MAX_METADATA_ROWS,
        MAX_METADATA_BYTES,
    )?;
    let replaces: Option<i64> = conn
        .query_row(
            "SELECT length(CAST(key AS BLOB)) + length(CAST(value AS BLOB)) FROM store_metadata WHERE key = ?1",
            params![key],
            |row| row.get(0),
        )
        .optional()?;
    budget.check(replaces, (key.len() + value.len()) as i64)?;
    write_metadata_row(conn, key, value)
}

fn write_internal_metadata(conn: &Connection, key: &str, value: &str) -> Result<(), StoreError> {
    debug_assert!(INTERNAL_METADATA_KEYS.contains(&key));
    write_metadata_row(conn, key, value)
}

fn write_metadata_row(conn: &Connection, key: &str, value: &str) -> Result<(), StoreError> {
    conn.execute(
        "INSERT INTO store_metadata(key, value) VALUES (?1, ?2)
         ON CONFLICT(key) DO UPDATE SET value = excluded.value",
        params![key, value],
    )?;
    Ok(())
}

pub fn unix_millis() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |elapsed| {
            i64::try_from(elapsed.as_millis()).unwrap_or(i64::MAX)
        })
}

fn prepare_schema(conn: &mut Connection) -> Result<OpenOutcome, OpenFailure> {
    let recorded: i32 = conn.pragma_query_value(None, "user_version", |row| row.get(0))?;
    if recorded > SCHEMA_VERSION {
        return Err(OpenFailure::Quarantine(QuarantineReason::Newer));
    }
    let applied = if recorded == 0 {
        if !schema_is_empty(conn)? {
            return Err(OpenFailure::Quarantine(QuarantineReason::Schema));
        }
        apply_baseline(conn)?;
        0
    } else {
        run_ladder(conn, recorded, MIGRATIONS, SCHEMA_VERSION)?
    };
    verify_schema_presence(conn)?;
    Ok(OpenOutcome {
        schema_version: SCHEMA_VERSION,
        previous_schema_version: recorded,
        applied_migrations: applied,
        quarantined_path: None,
        quarantine_reason: None,
    })
}

fn schema_is_empty(conn: &Connection) -> Result<bool, StoreError> {
    let objects: i64 = conn.query_row(
        "SELECT COUNT(*) FROM sqlite_schema WHERE name NOT GLOB 'sqlite_*'",
        [],
        |row| row.get(0),
    )?;
    Ok(objects == 0)
}

fn apply_baseline(conn: &mut Connection) -> Result<(), StoreError> {
    let transaction = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
    for statement in BASELINE_TABLES {
        transaction.execute(statement, [])?;
    }
    for index in INDEXES {
        transaction.execute(index.sql, [])?;
    }
    transaction.pragma_update(None, "user_version", SCHEMA_VERSION)?;
    write_internal_metadata(
        &transaction,
        METADATA_SCHEMA_VERSION,
        &SCHEMA_VERSION.to_string(),
    )?;
    write_internal_metadata(
        &transaction,
        METADATA_SCHEMA_APPLIED_AT,
        &unix_millis().to_string(),
    )?;
    transaction.commit()?;
    Ok(())
}

pub fn run_ladder(
    conn: &mut Connection,
    from: i32,
    ladder: &[Migration],
    target: i32,
) -> Result<u32, StoreError> {
    let mut version = from;
    let mut applied = 0;
    while version < target {
        let step = ladder
            .iter()
            .find(|candidate| candidate.from == version)
            .ok_or(StoreError::Refused(
                "app store schema version has no migration step",
            ))?;
        let transaction = conn.transaction_with_behavior(TransactionBehavior::Immediate)?;
        for statement in step.statements {
            transaction.execute(statement, [])?;
        }
        transaction.pragma_update(None, "user_version", step.to)?;
        write_internal_metadata(&transaction, METADATA_SCHEMA_VERSION, &step.to.to_string())?;
        write_internal_metadata(
            &transaction,
            METADATA_SCHEMA_APPLIED_AT,
            &unix_millis().to_string(),
        )?;
        transaction.commit()?;
        version = step.to;
        applied += 1;
    }
    Ok(applied)
}

fn verify_schema_presence(conn: &Connection) -> Result<(), OpenFailure> {
    for table in TABLES {
        let mut statement = conn.prepare(&format!("PRAGMA table_info({})", table.name))?;
        let mut columns = statement
            .query_map([], |row| row.get::<_, String>(1))?
            .collect::<Result<Vec<_>, _>>()?;
        columns.sort();
        let mut expected = table
            .columns
            .iter()
            .map(|name| (*name).to_owned())
            .collect::<Vec<_>>();
        expected.sort();
        if columns != expected {
            return Err(OpenFailure::Quarantine(QuarantineReason::Schema));
        }
    }
    for index in INDEXES {
        let present: Option<i64> = conn
            .query_row(
                "SELECT 1 FROM sqlite_schema WHERE type = 'index' AND name = ?1",
                params![index.name],
                |row| row.get(0),
            )
            .optional()?;
        if present.is_none() {
            conn.execute(index.sql, [])?;
        }
    }
    Ok(())
}

fn require_derivation_version(
    conn: &Connection,
    derivation_version: Option<&str>,
) -> Result<(), OpenFailure> {
    let Some(expected) = derivation_version else {
        return Ok(());
    };
    let recorded: Option<String> = conn
        .query_row(
            "SELECT value FROM store_metadata WHERE key = ?1",
            params![METADATA_DERIVATION_VERSION],
            |row| row.get(0),
        )
        .optional()?;
    match recorded {
        None => {
            write_internal_metadata(conn, METADATA_DERIVATION_VERSION, expected)?;
            Ok(())
        }
        Some(recorded) if recorded == expected => Ok(()),
        Some(_) => Err(OpenFailure::Quarantine(QuarantineReason::Derivation)),
    }
}

pub fn quarantine(path: &Path, reason: QuarantineReason) -> Result<String, StoreError> {
    let parent = path
        .parent()
        .ok_or_else(|| StoreError::PathHasNoParent(path.to_owned()))?;
    let file_name = path
        .file_name()
        .ok_or_else(|| StoreError::PathHasNoParent(path.to_owned()))?;
    let mut quarantined_name = OsString::from(file_name);
    quarantined_name.push(format!(".{}-{}", reason.as_str(), unix_millis()));
    let quarantined = parent.join(quarantined_name);
    for suffix in SIDECAR_SUFFIXES {
        let from = sidecar_path(path, suffix);
        let to = sidecar_path(&quarantined, suffix);
        match fs::rename(&from, &to) {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(source) => return Err(io_error(&from, source)),
        }
    }
    Ok(quarantined.to_string_lossy().into_owned())
}

fn sidecar_path(path: &Path, suffix: &str) -> PathBuf {
    if suffix.is_empty() {
        return path.to_owned();
    }
    let mut value = path.as_os_str().to_os_string();
    value.push(suffix);
    PathBuf::from(value)
}

#[derive(Clone, Copy)]
enum PathKind {
    Directory,
    File,
}

#[cfg(unix)]
fn restrict_path(path: &Path, kind: PathKind) {
    use std::os::unix::fs::PermissionsExt;

    let mode = match kind {
        PathKind::Directory => 0o700,
        PathKind::File => 0o600,
    };
    let _ = fs::set_permissions(path, fs::Permissions::from_mode(mode));
}

#[cfg(not(unix))]
fn restrict_path(_path: &Path, _kind: PathKind) {}

pub struct CapacityBudget {
    resource: &'static str,
    max_entries: i64,
    max_bytes: i64,
    entries: i64,
    bytes: i64,
}

impl CapacityBudget {
    pub fn read(
        conn: &Connection,
        resource: &'static str,
        sql: &str,
        args: impl rusqlite::Params,
        max_entries: i64,
        max_bytes: i64,
    ) -> Result<Self, StoreError> {
        let (entries, bytes) = conn.query_row(sql, args, |row| Ok((row.get(0)?, row.get(1)?)))?;
        Ok(Self {
            resource,
            max_entries,
            max_bytes,
            entries,
            bytes,
        })
    }

    pub fn check(&self, replaces: Option<i64>, incoming: i64) -> Result<(), CapacityRefusal> {
        let (entries, bytes) = self.projected(replaces, incoming);
        if entries > self.max_entries {
            return Err(CapacityRefusal {
                resource: self.resource,
                limit: self.max_entries,
                unit: "entries",
            });
        }
        if bytes > self.max_bytes {
            return Err(CapacityRefusal {
                resource: self.resource,
                limit: self.max_bytes,
                unit: "bytes",
            });
        }
        Ok(())
    }

    pub fn commit(&mut self, replaces: Option<i64>, incoming: i64) {
        (self.entries, self.bytes) = self.projected(replaces, incoming);
    }

    fn projected(&self, replaces: Option<i64>, incoming: i64) -> (i64, i64) {
        (
            self.entries + i64::from(replaces.is_none()),
            self.bytes - replaces.unwrap_or(0) + incoming,
        )
    }
}

#[derive(Clone, Debug)]
pub enum WriteRefusal {
    Invalid(&'static str),
    Capacity(CapacityRefusal),
}

impl Display for WriteRefusal {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid(reason) => write!(formatter, "{reason}"),
            Self::Capacity(refusal) => write!(formatter, "{refusal}"),
        }
    }
}

impl From<CapacityRefusal> for WriteRefusal {
    fn from(refusal: CapacityRefusal) -> Self {
        Self::Capacity(refusal)
    }
}

impl From<WriteRefusal> for StoreError {
    fn from(refusal: WriteRefusal) -> Self {
        match refusal {
            WriteRefusal::Invalid(reason) => Self::Refused(reason),
            WriteRefusal::Capacity(capacity) => Self::Capacity(capacity),
        }
    }
}

pub fn check_lookup_key(value: &str, what: &'static str) -> Option<WriteRefusal> {
    if value.is_empty() || value.len() > MAX_LOOKUP_KEY_BYTES || value.as_bytes().contains(&0) {
        return Some(WriteRefusal::Invalid(what));
    }
    None
}

pub fn require_lookup_key(value: &str, what: &'static str) -> Result<(), StoreError> {
    match check_lookup_key(value, what) {
        Some(refusal) => Err(refusal.into()),
        None => Ok(()),
    }
}

#[cfg(test)]
pub(crate) fn temporary_store_path() -> (tempfile::TempDir, PathBuf) {
    let dir = tempfile::TempDir::new().expect("a temporary directory");
    let path = dir.path().join("desktop-app-store.sqlite3");
    (dir, path)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn store_dir() -> (tempfile::TempDir, PathBuf) {
        temporary_store_path()
    }

    fn table_exists(conn: &Connection, name: &str) -> bool {
        conn.query_row(
            "SELECT 1 FROM sqlite_schema WHERE type = 'table' AND name = ?1",
            params![name],
            |row| row.get::<_, i64>(0),
        )
        .optional()
        .expect("the schema is readable")
        .is_some()
    }

    fn user_version(conn: &Connection) -> i32 {
        conn.pragma_query_value(None, "user_version", |row| row.get(0))
            .expect("user_version is readable")
    }

    #[test]
    fn a_fresh_store_applies_the_baseline_and_reopens_unchanged() {
        let (_dir, path) = store_dir();
        let store = Store::open(path.clone(), None).expect("a fresh store opens");
        assert_eq!(store.outcome().schema_version, SCHEMA_VERSION);
        assert_eq!(store.outcome().previous_schema_version, 0);
        assert_eq!(store.outcome().applied_migrations, 0);
        assert!(store.outcome().quarantined_path.is_none());
        assert_eq!(
            store
                .metadata(METADATA_SCHEMA_VERSION)
                .expect("metadata is readable")
                .as_deref(),
            Some("1")
        );
        for table in TABLES {
            assert!(
                table_exists(store.connection(), table.name),
                "{}",
                table.name
            );
        }
        let journal: String = store
            .connection()
            .query_row("PRAGMA journal_mode", [], |row| row.get(0))
            .expect("the journal mode is readable");
        assert_eq!(journal, "delete", "the store must not use WAL");
        let foreign_keys: i64 = store
            .connection()
            .query_row("PRAGMA foreign_keys", [], |row| row.get(0))
            .expect("the foreign key pragma is readable");
        assert_eq!(foreign_keys, 1);
        drop(store);

        let reopened = Store::open(path, None).expect("the store reopens");
        assert_eq!(reopened.outcome().previous_schema_version, SCHEMA_VERSION);
        assert_eq!(reopened.outcome().applied_migrations, 0);
        assert!(reopened.outcome().quarantined_path.is_none());
    }

    #[test]
    fn the_ladder_runs_one_step_at_a_time_and_bumps_the_version_per_step() {
        let (_dir, path) = store_dir();
        drop(Store::open(path.clone(), None).expect("a fresh store opens"));
        let mut conn = Connection::open(&path).expect("the store reopens directly");

        const LADDER: &[Migration] = &[
            Migration {
                from: 1,
                to: 2,
                statements: &["CREATE TABLE step_two (id INTEGER PRIMARY KEY)"],
            },
            Migration {
                from: 2,
                to: 3,
                statements: &["CREATE TABLE step_three (id INTEGER PRIMARY KEY)"],
            },
        ];
        assert_eq!(
            run_ladder(&mut conn, 1, LADDER, 2).expect("the first step applies"),
            1
        );
        assert_eq!(user_version(&conn), 2);
        assert!(table_exists(&conn, "step_two"));
        assert!(!table_exists(&conn, "step_three"));

        assert_eq!(
            run_ladder(&mut conn, 2, LADDER, 3).expect("the second step applies"),
            1
        );
        assert_eq!(user_version(&conn), 3);
        assert!(table_exists(&conn, "step_three"));

        const BROKEN: &[Migration] = &[Migration {
            from: 3,
            to: 4,
            statements: &[
                "CREATE TABLE step_four (id INTEGER PRIMARY KEY)",
                "CREATE TABLE step_two (id INTEGER PRIMARY KEY)",
            ],
        }];
        assert!(run_ladder(&mut conn, 3, BROKEN, 4).is_err());
        assert_eq!(user_version(&conn), 3, "a failed step rolls back");
        assert!(!table_exists(&conn, "step_four"));

        assert!(
            run_ladder(&mut conn, 3, &[], 4).is_err(),
            "a missing step is refused rather than skipped"
        );
    }

    #[test]
    fn a_store_from_a_newer_release_is_quarantined_and_survives_under_its_new_name() {
        let (_dir, path) = store_dir();
        {
            let store = Store::open(path.clone(), None).expect("a fresh store opens");
            store
                .set_metadata("canary", "keep-me")
                .expect("metadata is writable");
            store
                .connection()
                .pragma_update(None, "user_version", SCHEMA_VERSION + 1)
                .expect("user_version is writable");
        }

        let store = Store::open(path.clone(), None).expect("a replacement store opens");
        assert_eq!(store.outcome().quarantine_reason, Some("newer"));
        let quarantined = store
            .outcome()
            .quarantined_path
            .clone()
            .expect("the downgrade is quarantined");
        assert!(path.exists(), "a fresh store replaces the quarantined one");
        assert!(
            Path::new(&quarantined).exists(),
            "the source file must still exist under its new name"
        );
        assert_eq!(
            store.metadata("canary").expect("metadata is readable"),
            None
        );

        let recovered = Connection::open(&quarantined).expect("the quarantined file opens");
        let canary: String = recovered
            .query_row(
                "SELECT value FROM store_metadata WHERE key = 'canary'",
                [],
                |row| row.get(0),
            )
            .expect("the quarantined rows are intact");
        assert_eq!(canary, "keep-me");
    }

    #[test]
    fn quarantine_renames_every_sidecar_and_unlinks_nothing() {
        let (_dir, path) = store_dir();
        fs::write(&path, b"main").expect("the main file is writable");
        for suffix in ["-journal", "-wal", "-shm"] {
            fs::write(sidecar_path(&path, suffix), suffix.as_bytes())
                .expect("the sidecar is writable");
        }

        let moved = quarantine(&path, QuarantineReason::Corrupt).expect("quarantine succeeds");
        assert!(moved.contains(".corrupt-"));
        assert!(!path.exists());
        assert_eq!(
            fs::read(&moved).expect("the moved file is readable"),
            b"main"
        );
        for suffix in ["-journal", "-wal", "-shm"] {
            assert_eq!(
                fs::read(sidecar_path(Path::new(&moved), suffix))
                    .expect("the moved sidecar is readable"),
                suffix.as_bytes()
            );
        }
    }

    #[test]
    fn a_file_that_is_not_a_database_is_quarantined_rather_than_deleted() {
        let (_dir, path) = store_dir();
        fs::write(&path, b"this is a note, not a database").expect("the file is writable");

        let store = Store::open(path.clone(), None).expect("a replacement store opens");
        assert_eq!(store.outcome().quarantine_reason, Some("corrupt"));
        let quarantined = store
            .outcome()
            .quarantined_path
            .clone()
            .expect("the unreadable file is quarantined");
        assert_eq!(
            fs::read(&quarantined).expect("the quarantined bytes are readable"),
            b"this is a note, not a database"
        );
    }

    #[test]
    fn a_vacuum_into_copy_still_verifies_as_compatible() {
        let (dir, path) = store_dir();
        let copy = dir.path().join("desktop-app-store.copy.sqlite3");
        {
            let store = Store::open(path, None).expect("a fresh store opens");
            store
                .set_metadata("canary", "keep-me")
                .expect("metadata is writable");
            store
                .connection()
                .execute("VACUUM INTO ?1", params![copy.to_string_lossy()])
                .expect("the copy is written");
        }

        let copied = Store::open(copy, None).expect("the copy opens");
        assert!(
            copied.outcome().quarantined_path.is_none(),
            "a vacuumed copy is compatible, not quarantined"
        );
        assert_eq!(copied.outcome().previous_schema_version, SCHEMA_VERSION);
        assert_eq!(
            copied.metadata("canary").expect("metadata is readable"),
            Some("keep-me".to_owned())
        );
    }

    #[test]
    fn a_schema_whose_ddl_text_differs_but_whose_columns_match_is_kept() {
        let (_dir, path) = store_dir();
        drop(Store::open(path.clone(), None).expect("a fresh store opens"));
        {
            let conn = Connection::open(&path).expect("the store reopens directly");
            conn.execute_batch(
                "DROP TABLE accounts;
                 CREATE TABLE accounts (
                     storage_key   TEXT    PRIMARY KEY  NOT NULL,
                     user_id       text    not null,
                     last_active   REAL    NOT NULL,
                     record_json   TEXT    NOT NULL
                 );
                 INSERT INTO accounts VALUES ('a::1', '1', 1.0, '{}');",
            )
            .expect("the table is rebuilt with different DDL text");
        }

        let store = Store::open(path, None).expect("the rebuilt store opens");
        assert!(
            store.outcome().quarantined_path.is_none(),
            "DDL text differences must not quarantine a compatible store"
        );
        let rows: i64 = store
            .connection()
            .query_row("SELECT COUNT(*) FROM accounts", [], |row| row.get(0))
            .expect("the accounts survive");
        assert_eq!(rows, 1);
    }

    #[test]
    fn a_missing_index_is_recreated_and_a_missing_table_is_quarantined() {
        let (_dir, path) = store_dir();
        drop(Store::open(path.clone(), None).expect("a fresh store opens"));
        {
            let conn = Connection::open(&path).expect("the store reopens directly");
            conn.execute_batch("DROP INDEX accounts_last_active_idx")
                .expect("the index is dropped");
        }

        let store = Store::open(path.clone(), None).expect("the store opens");
        assert!(store.outcome().quarantined_path.is_none());
        let index: Option<i64> = store
            .connection()
            .query_row(
                "SELECT 1 FROM sqlite_schema WHERE type = 'index' AND name = 'accounts_last_active_idx'",
                [],
                |row| row.get(0),
            )
            .optional()
            .expect("the schema is readable");
        assert!(index.is_some(), "a derivable index is repaired in place");
        drop(store);

        {
            let conn = Connection::open(&path).expect("the store reopens directly");
            conn.execute_batch("DROP TABLE known_instances")
                .expect("the table is dropped");
        }
        let store = Store::open(path, None).expect("a replacement store opens");
        assert_eq!(store.outcome().quarantine_reason, Some("schema"));
        assert!(store.outcome().quarantined_path.is_some());
    }

    #[test]
    fn a_changed_derivation_version_quarantines_rather_than_orphaning_rows() {
        let (_dir, path) = store_dir();
        {
            let store = Store::open(path.clone(), Some("v1")).expect("a fresh store opens");
            store
                .set_metadata("canary", "keep-me")
                .expect("metadata is writable");
        }

        let same = Store::open(path.clone(), Some("v1")).expect("the store reopens");
        assert!(same.outcome().quarantined_path.is_none());
        drop(same);

        let changed = Store::open(path, Some("v2")).expect("a replacement store opens");
        assert_eq!(changed.outcome().quarantine_reason, Some("derivation"));
        assert_eq!(
            changed
                .metadata(METADATA_DERIVATION_VERSION)
                .expect("metadata is readable"),
            Some("v2".to_owned())
        );
        assert_eq!(
            changed.metadata("canary").expect("metadata is readable"),
            None
        );
    }

    #[test]
    fn an_empty_ladder_is_contiguous_only_at_the_baseline_version() {
        assert!(ladder_is_contiguous(&[], 1));
        assert!(!ladder_is_contiguous(&[], 2));
    }

    #[test]
    fn a_contiguous_ladder_ending_at_the_target_is_accepted() {
        const LADDER: &[Migration] = &[
            Migration {
                from: 1,
                to: 2,
                statements: &[],
            },
            Migration {
                from: 2,
                to: 3,
                statements: &[],
            },
        ];
        assert!(ladder_is_contiguous(LADDER, 3));
        assert!(!ladder_is_contiguous(LADDER, 4));
    }

    #[test]
    fn a_ladder_with_a_gap_or_a_multi_step_edge_is_rejected() {
        const GAP: &[Migration] = &[
            Migration {
                from: 1,
                to: 2,
                statements: &[],
            },
            Migration {
                from: 3,
                to: 4,
                statements: &[],
            },
        ];
        const MULTI_STEP: &[Migration] = &[Migration {
            from: 1,
            to: 3,
            statements: &[],
        }];
        const LATE_START: &[Migration] = &[Migration {
            from: 2,
            to: 3,
            statements: &[],
        }];
        assert!(!ladder_is_contiguous(GAP, 4));
        assert!(!ladder_is_contiguous(MULTI_STEP, 3));
        assert!(!ladder_is_contiguous(LATE_START, 3));
    }

    #[test]
    fn a_capacity_budget_admits_replacements_without_growing_the_entry_count() {
        let mut budget = CapacityBudget {
            resource: "test",
            max_entries: 2,
            max_bytes: 100,
            entries: 2,
            bytes: 50,
        };
        budget.check(Some(20), 30).expect("a replacement fits");
        budget.commit(Some(20), 30);
        assert_eq!(budget.entries, 2);
        assert_eq!(budget.bytes, 60);
        assert_eq!(
            budget.check(None, 1).unwrap_err().unit,
            "entries",
            "a third entry must not be admitted"
        );
        assert_eq!(budget.check(Some(0), 41).unwrap_err().unit, "bytes");
        assert_eq!(budget.bytes, 60, "a refused write must not consume budget");
    }

    #[test]
    fn store_metadata_refuses_unusable_keys_and_writes_past_its_caps() {
        let (_dir, path) = store_dir();
        let store = Store::open(path, None).expect("a fresh store opens");
        let existing: i64 = store
            .connection()
            .query_row("SELECT COUNT(*) FROM store_metadata", [], |row| row.get(0))
            .expect("the metadata rows are countable");
        store
            .connection()
            .execute_batch("BEGIN IMMEDIATE")
            .expect("the fill transaction begins");
        for index in existing..MAX_METADATA_ROWS {
            store
                .set_metadata(&format!("marker.{index}"), "1")
                .expect("a marker below the caps is written");
        }
        store
            .connection()
            .execute_batch("COMMIT")
            .expect("the fill transaction commits");

        assert!(
            matches!(
                store.set_metadata("marker.new", "1"),
                Err(StoreError::Capacity(refusal)) if refusal.unit == "entries"
            ),
            "a new marker key past the row cap must be refused"
        );
        let occupied = format!("marker.{existing}");
        store
            .set_metadata(&occupied, "updated")
            .expect("replacing an existing marker still fits");
        assert!(
            matches!(
                store.set_metadata(&occupied, &"v".repeat(MAX_METADATA_BYTES as usize + 1)),
                Err(StoreError::Capacity(refusal)) if refusal.unit == "bytes"
            ),
            "a marker value past the byte cap must be refused"
        );
        let derivation_before = store
            .metadata(METADATA_DERIVATION_VERSION)
            .expect("reading the derivation stamp must succeed");
        for reserved in [
            METADATA_SCHEMA_VERSION,
            METADATA_SCHEMA_APPLIED_AT,
            METADATA_DERIVATION_VERSION,
        ] {
            assert!(
                matches!(
                    store.set_metadata(reserved, "tampered"),
                    Err(StoreError::Refused(_))
                ),
                "the public path must refuse the reserved key {reserved}"
            );
        }
        assert_eq!(
            store
                .metadata(METADATA_DERIVATION_VERSION)
                .expect("reading the derivation stamp must succeed"),
            derivation_before,
            "a refused write must leave the derivation stamp untouched"
        );
        assert!(matches!(
            store.set_metadata("", "1"),
            Err(StoreError::Refused(_))
        ));
        assert!(matches!(
            store.set_metadata("marker.\u{0}", "1"),
            Err(StoreError::Refused(_))
        ));
        let rows: i64 = store
            .connection()
            .query_row("SELECT COUNT(*) FROM store_metadata", [], |row| row.get(0))
            .expect("the metadata rows are countable");
        assert_eq!(rows, MAX_METADATA_ROWS);
    }
}
