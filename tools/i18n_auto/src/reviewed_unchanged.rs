// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, OpenOptions};
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

const REVIEWED_UNCHANGED_VERSION: u32 = 1;
const LOCK_TIMEOUT: Duration = Duration::from_secs(30);
const LOCK_POLL_INTERVAL: Duration = Duration::from_millis(50);

#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub struct ReviewedUnchangedEntry {
    #[serde(default, skip_serializing_if = "Option::is_none")]
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

#[derive(Clone, Debug, Deserialize, Serialize)]
struct ReviewedUnchangedFile {
    version: u32,
    #[serde(default)]
    locales: BTreeMap<String, Vec<ReviewedUnchangedEntry>>,
}

impl Default for ReviewedUnchangedFile {
    fn default() -> Self {
        Self {
            version: REVIEWED_UNCHANGED_VERSION,
            locales: BTreeMap::new(),
        }
    }
}

pub struct ReviewedUnchangedStore {
    path: PathBuf,
    data: ReviewedUnchangedFile,
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
            .locales
            .get(locale)
            .is_some_and(|entries| entries.binary_search(&needle).is_ok())
    }

    pub fn mark(&mut self, locale: &str, msgctxt: Option<&str>, msgid: &str) {
        let entry = ReviewedUnchangedEntry::new(msgctxt, msgid);
        let entries = self.data.locales.entry(locale.to_string()).or_default();
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
        let Some(entries) = self.data.locales.get_mut(locale) else {
            return;
        };
        let Ok(index) = entries.binary_search(&entry) else {
            return;
        };
        entries.remove(index);
        if entries.is_empty() {
            self.data.locales.remove(locale);
        }
        self.dirty_locales.insert(locale.to_string());
    }

    pub fn locales(&self) -> impl Iterator<Item = (&str, &[ReviewedUnchangedEntry])> {
        self.data
            .locales
            .iter()
            .map(|(locale, entries)| (locale.as_str(), entries.as_slice()))
    }

    pub fn retain_locale(
        &mut self,
        locale: &str,
        mut keep: impl FnMut(&ReviewedUnchangedEntry) -> bool,
    ) -> usize {
        let Some(entries) = self.data.locales.get_mut(locale) else {
            return 0;
        };
        let before = entries.len();
        entries.retain(|entry| keep(entry));
        let removed = before - entries.len();
        if removed == 0 {
            return 0;
        }
        if entries.is_empty() {
            self.data.locales.remove(locale);
        }
        self.dirty_locales.insert(locale.to_string());
        removed
    }

    pub fn clear_locale(&mut self, locale: &str) {
        if self.data.locales.remove(locale).is_some() {
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
            match self.data.locales.get(locale) {
                Some(entries) if !entries.is_empty() => {
                    let mut entries = entries.clone();
                    normalize_entries(&mut entries);
                    merged.locales.insert(locale.clone(), entries);
                }
                _ => {
                    merged.locales.remove(locale);
                }
            }
        }
        write_store_file(&self.path, &merged)?;
        self.data = merged;
        self.dirty_locales.clear();
        Ok(())
    }
}

fn read_store_file(path: &Path) -> Result<ReviewedUnchangedFile> {
    let content = match fs::read_to_string(path) {
        Ok(content) => content,
        Err(error) if error.kind() == ErrorKind::NotFound => {
            return Ok(ReviewedUnchangedFile::default());
        }
        Err(error) => {
            return Err(error).with_context(|| format!("failed to read {}", path.display()));
        }
    };
    let mut data = serde_json::from_str::<ReviewedUnchangedFile>(&content)
        .with_context(|| format!("failed to parse {}", path.display()))?;
    if data.version != REVIEWED_UNCHANGED_VERSION {
        bail!(
            "unsupported reviewed-unchanged sidecar version {} in {}",
            data.version,
            path.display()
        );
    }
    for entries in data.locales.values_mut() {
        normalize_entries(entries);
    }
    data.locales.retain(|_, entries| !entries.is_empty());
    Ok(data)
}

fn normalize_entries(entries: &mut Vec<ReviewedUnchangedEntry>) {
    entries.sort();
    entries.dedup();
}

fn write_store_file(path: &Path, data: &ReviewedUnchangedFile) -> Result<()> {
    let mut content = serde_json::to_string_pretty(data)
        .with_context(|| format!("failed to serialize {}", path.display()))?;
    content.push('\n');
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

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn stores_entries_sorted_and_deduplicated() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("reviewed.json");
        let mut store = ReviewedUnchangedStore::load(&path).unwrap();
        store.mark("de", Some("button"), "Save");
        store.mark("de", None, "Audio");
        store.mark("de", None, "Audio");
        store.save_if_dirty().unwrap();

        let saved = fs::read_to_string(&path).unwrap();
        assert!(saved.contains("\"version\": 1"));

        let loaded = ReviewedUnchangedStore::load(&path).unwrap();
        assert!(loaded.contains("de", None, "Audio"));
        assert!(loaded.contains("de", Some("button"), "Save"));
        assert!(!loaded.contains("de", Some("button"), "Audio"));
    }

    #[test]
    fn retain_locale_drops_entries_and_saves_only_that_locale() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("reviewed.json");
        let mut store = ReviewedUnchangedStore::load(&path).unwrap();
        store.mark("de", None, "Audio");
        store.mark("de", None, "Gone");
        store.mark("fr", None, "Gone");
        store.save_if_dirty().unwrap();

        let mut store = ReviewedUnchangedStore::load(&path).unwrap();
        assert_eq!(store.retain_locale("de", |entry| entry.msgid != "Gone"), 1);
        assert_eq!(store.retain_locale("de", |_| true), 0);
        assert_eq!(store.retain_locale("nl", |_| false), 0);
        store.save_if_dirty().unwrap();

        let loaded = ReviewedUnchangedStore::load(&path).unwrap();
        assert!(loaded.contains("de", None, "Audio"));
        assert!(!loaded.contains("de", None, "Gone"));
        assert!(loaded.contains("fr", None, "Gone"));
    }

    #[test]
    fn retain_locale_removes_emptied_locale() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("reviewed.json");
        let mut store = ReviewedUnchangedStore::load(&path).unwrap();
        store.mark("de", Some("button"), "Save");
        store.mark("fr", None, "Avatar");
        store.save_if_dirty().unwrap();

        let mut store = ReviewedUnchangedStore::load(&path).unwrap();
        assert_eq!(store.retain_locale("de", |_| false), 1);
        store.save_if_dirty().unwrap();

        let loaded = ReviewedUnchangedStore::load(&path).unwrap();
        assert_eq!(
            loaded
                .locales()
                .map(|(locale, _)| locale)
                .collect::<Vec<_>>(),
            vec!["fr"]
        );
        assert!(!fs::read_to_string(&path).unwrap().contains("\"de\""));
    }

    #[test]
    fn merge_save_preserves_other_locale_changes() {
        let temp = tempdir().unwrap();
        let path = temp.path().join("reviewed.json");
        let mut first = ReviewedUnchangedStore::load(&path).unwrap();
        let mut second = ReviewedUnchangedStore::load(&path).unwrap();

        first.mark("de", None, "Audio");
        second.mark("fr", None, "Avatar");
        first.save_if_dirty().unwrap();
        second.save_if_dirty().unwrap();

        let loaded = ReviewedUnchangedStore::load(&path).unwrap();
        assert!(loaded.contains("de", None, "Audio"));
        assert!(loaded.contains("fr", None, "Avatar"));
    }
}
