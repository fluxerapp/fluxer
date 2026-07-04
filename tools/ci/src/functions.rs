// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::{Context, Result};
use serde::Serialize;
use std::fs;
use std::io;
use std::path::Path;

pub(crate) fn remove_file_if_exists(path: &Path) -> Result<()> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error).with_context(|| format!("Failed to remove {}", path.display())),
    }
}

pub(crate) fn remove_dir_if_exists(path: &Path) -> Result<()> {
    match fs::remove_dir_all(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error).with_context(|| format!("Failed to remove {}", path.display())),
    }
}

pub(crate) fn write_json_pretty<T: Serialize + ?Sized>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create {}", parent.display()))?;
    }
    let mut bytes = serde_json::to_vec_pretty(value)?;
    bytes.push(b'\n');
    fs::write(path, bytes).with_context(|| format!("Failed to write {}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn remove_file_if_exists_removes_files_and_ignores_missing_paths() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("artifact.txt");
        fs::write(&path, "artifact").unwrap();

        remove_file_if_exists(&path).unwrap();
        remove_file_if_exists(&path).unwrap();

        assert!(!path.exists());
    }

    #[test]
    fn remove_dir_if_exists_removes_directories_and_ignores_missing_paths() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("dist");
        fs::create_dir_all(path.join("nested")).unwrap();
        fs::write(path.join("nested").join("artifact.txt"), "artifact").unwrap();

        remove_dir_if_exists(&path).unwrap();
        remove_dir_if_exists(&path).unwrap();

        assert!(!path.exists());
    }

    #[test]
    fn write_json_pretty_creates_parent_directories_and_writes_pretty_json() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("nested").join("manifest.json");

        write_json_pretty(&path, &json!({ "name": "fluxer", "version": 1 })).unwrap();

        assert_eq!(
            fs::read_to_string(path).unwrap(),
            "{\n  \"name\": \"fluxer\",\n  \"version\": 1\n}\n"
        );
    }
}
