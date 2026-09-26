// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::storage::StorageError;

pub(crate) fn safe_bucket(bucket: &str) -> Result<(), StorageError> {
    if !safe_path_component(bucket) {
        return Err(StorageError::InvalidBucket);
    }
    Ok(())
}

pub(crate) fn safe_key(key: &str) -> Result<(), StorageError> {
    if key.is_empty() || key.starts_with('/') {
        return Err(StorageError::InvalidKey);
    }
    for component in key.split('/') {
        if !safe_path_component(component) {
            return Err(StorageError::InvalidKey);
        }
    }
    Ok(())
}

pub(crate) fn validate_virtual_hosted_bucket(bucket: &str) -> Result<(), StorageError> {
    if bucket.len() < 3 || bucket.len() > 63 {
        return Err(StorageError::InvalidBucket);
    }
    if !bucket
        .bytes()
        .all(|b| b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'-' || b == b'.')
    {
        return Err(StorageError::InvalidBucket);
    }
    Ok(())
}

fn safe_path_component(component: &str) -> bool {
    !component.is_empty()
        && component != "."
        && component != ".."
        && !component.contains(['/', '\0'])
}
