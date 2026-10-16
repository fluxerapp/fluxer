// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, OpenOptions};
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

const LOCK_TIMEOUT: Duration = Duration::from_secs(30);
const LOCK_POLL_INTERVAL: Duration = Duration::from_millis(50);

type Locales = BTreeMap<String, Vec<ReviewedUnchangedEntry>>;

#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(from = "StoredEntry", into = "StoredEntry")]
pub struct ReviewedUnchangedEntry {
    pub msgctxt: Option<String>,
    pub msgid: String,
}

impl ReviewedUnchangedEntry {
    pub fn new(msgctxt: Option<&str>, msgid: &str) -> Self {
        Self {
            msgctxt: msgctxt.map(str::to_string),
            msgid: msgid.to_string(),
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
enum StoredEntry {
    Msgid(String),
    WithContext(String, String),
}

impl From<StoredEntry> for ReviewedUnchangedEntry {
    fn from(stored: StoredEntry) -> Self {
        match stored {
            StoredEntry::Msgid(msgid) => Self {
                msgctxt: None,
                msgid,
            },
            StoredEntry::WithContext(msgctxt, msgid) => Self {
                msgctxt: Some(msgctxt),
                msgid,
            },
        }
    }
}

impl From<ReviewedUnchangedEntry> for StoredEntry {
    fn from(entry: ReviewedUnchangedEntry) -> Self {
        match entry.msgctxt {
            Some(msgctxt) => Self::WithContext(msgctxt, entry.msgid),
            None => Self::Msgid(entry.msgid),
        }
    }
}

pub struct ReviewedUnchangedStore {
    path: PathBuf,
    data: Locales,
    dirty_locales: BTreeSet<String>,
}

impl ReviewedUnchangedStore {
    pub fn load(path: impl Into<PathBuf>) -> Result<Self> {
        let path = path.into();
        let data = read_store_file(&path)?;
        Ok(Self {
            path,
            data,
            dirty_locales: BTreeSet::new(),
        })
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn contains(&self, locale: &str, msgctxt: Option<&str>, msgid: &str) -> bool {
        let needle = ReviewedUnchangedEntry::new(msgctxt, msgid);
        self.data
            .get(locale)
            .is_some_and(|entries| entries.binary_search(&needle).is_ok())
    }

    pub fn mark(&mut self, locale: &str, msgctxt: Option<&str>, msgid: &str) {
        let entry = ReviewedUnchangedEntry::new(msgctxt, msgid);
        let entries = self.data.entry(locale.to_string()).or_default();
        match entries.binary_search(&entry) {
            Ok(_) => {}
            Err(index) => {
                entries.insert(index, entry);
                self.dirty_locales.insert(locale.to_string());
            }
        }
    }

    pub fn unmark(&mut self, locale: &str, msgctxt: Option<&str>, msgid: &str) {
        let entry = ReviewedUnchangedEntry::new(msgctxt, msgid);
        let Some(entries) = self.data.get_mut(locale) else {
            return;
        };
        let Ok(index) = entries.binary_search(&entry) else {
            return;
        };
        entries.remove(index);
        if entries.is_empty() {
            self.data.remove(locale);
        }
        self.dirty_locales.insert(locale.to_string());
    }

    pub fn locales(&self) -> impl Iterator<Item = (&str, &[ReviewedUnchangedEntry])> {
        self.data
            .iter()
            .map(|(locale, entries)| (locale.as_str(), entries.as_slice()))
    }

    pub fn retain_locale(
        &mut self,
        locale: &str,
        mut keep: impl FnMut(&ReviewedUnchangedEntry) -> bool,
    ) -> usize {
        let Some(entries) = self.data.get_mut(locale) else {
            return 0;
        };
        let before = entries.len();
        entries.retain(|entry| keep(entry));
        let removed = before - entries.len();
        if removed == 0 {
            return 0;
        }
        if entries.is_empty() {
            self.data.remove(locale);
        }
        self.dirty_locales.insert(locale.to_string());
        removed
    }

    pub fn clear_locale(&mut self, locale: &str) {
        if self.data.remove(locale).is_some() {
            self.dirty_locales.insert(locale.to_string());
        }
    }

    pub fn save_if_dirty(&mut self) -> Result<()> {
        if self.dirty_locales.is_empty() {
            return Ok(());
        }
        if let Some(parent) = self.path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        let _lock = SidecarLock::acquire(&self.path)?;
        let mut merged = read_store_file(&self.path)?;
        for locale in &self.dirty_locales {
            match self.data.get(locale) {
                Some(entries) if !entries.is_empty() => {
                    let mut entries = entries.clone();
                    normalize_entries(&mut entries);
                    merged.insert(locale.clone(), entries);
                }
                _ => {
                    merged.remove(locale);
                }
            }
        }
        write_store_file(&self.path, &merged)?;
        self.data = merged;
        self.dirty_locales.clear();
        Ok(())
    }
}

fn read_store_file(path: &Path) -> Result<Locales> {
    let content = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(error) if error.kind() == ErrorKind::NotFound => {
            return Ok(Locales::new());
        }
        Err(error) => {
            return Err(error).with_context(|| format!("failed to read {}", path.display()));
        }
    };
    let mut data = serde_json::from_str::<Locales>(&content)
        .with_context(|| format!("failed to parse {}", path.display()))?;
    for entries in data.values_mut() {
        normalize_entries(entries);
    }
    data.retain(|_, entries| !entries.is_empty());
    Ok(data)
}

fn normalize_entries(entries: &mut Vec<ReviewedUnchangedEntry>) {
    entries.sort();
    entries.dedup();
}

fn render_store(data: &Locales) -> Result<String> {
    let mut locales = Vec::with_capacity(data.len());
    for (locale, entries) in data {
        let mut lines = Vec::with_capacity(entries.len());
        for entry in entries {
            lines.push(format!("\t\t{}", serde_json::to_string(entry)?));
        }
        locales.push(format!(
            "\t{}: [\n{}\n\t]",
            serde_json::to_string(locale)?,
            lines.join(",\n")
        ));
    }
    if locales.is_empty() {
        return Ok("{}\n".to_string());
    }
    Ok(format!("{{\n{}\n}}\n", locales.join(",\n")))
}

fn write_store_file(path: &Path, data: &Locales) -> Result<()> {
    let content =
        render_store(data).with_context(|| format!("failed to serialize {}", path.display()))?;
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let temp_path = path.with_file_name(format!(
        ".{}.{}.{}.tmp",
        path.file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("auto-i18n-reviewed-unchanged.json"),
        std::process::id(),
        timestamp
    ));
    fs::write(&temp_path, content)
        .with_context(|| format!("failed to write {}", temp_path.display()))?;
    fs::rename(&temp_path, path)
        .with_context(|| format!("failed to replace {}", path.display()))?;
    Ok(())
}

struct SidecarLock {
    path: PathBuf,
}

impl SidecarLock {
    fn acquire(sidecar_path: &Path) -> Result<Self> {
        let lock_path = sidecar_path.with_extension("json.lock");
        let started = Instant::now();
        loop {
            match OpenOptions::new()
                .write(true)
                .create_new(true)
                .open(&lock_path)
            {
                Ok(_) => {
                    return Ok(Self { path: lock_path });
                }
                Err(error) if error.kind() == ErrorKind::AlreadyExists => {
                    if started.elapsed() >= LOCK_TIMEOUT {
                        bail!(
                            "timed out waiting for reviewed-unchanged sidecar lock {}",
                            lock_path.display()
                        );
                    }
                    thread::sleep(LOCK_POLL_INTERVAL);
                }
                Err(error) => {
                    return Err(error)
                        .with_context(|| format!("failed to create {}", lock_path.display()));
                }
            }
        }
    }
}

impl Drop for SidecarLock {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}
