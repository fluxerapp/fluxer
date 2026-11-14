// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::storage::StorageError;
use sha2::{Digest as _, Sha256};
use std::borrow::Cow;

const MAX_SEGMENT_BYTES: usize = 255;
const MAX_EXTENSION_BYTES: usize = 16;
const SEGMENT_HASH_HEX_CHARS: usize = 16;
const UPLOAD_ID_HEX_CHARS: usize = 32;

pub(crate) fn safe_bucket(bucket: &str) -> Result<(), StorageError> {
    if !safe_path_component(bucket) || bucket.starts_with('.') {
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

pub(crate) fn map_segment(segment: &str) -> Cow<'_, str> {
    if segment.len() <= MAX_SEGMENT_BYTES {
        return Cow::Borrowed(segment);
    }
    let extension = match segment.rfind('.') {
        Some(dot) if dot > 0 && segment.len() - dot <= MAX_EXTENSION_BYTES => &segment[dot..],
        _ => "",
    };
    let mut cut = MAX_SEGMENT_BYTES - 1 - SEGMENT_HASH_HEX_CHARS - extension.len();
    while !segment.is_char_boundary(cut) {
        cut -= 1;
    }
    let digest = hex::encode(Sha256::digest(segment.as_bytes()));
    Cow::Owned(format!(
        "{}~{}{}",
        &segment[..cut],
        &digest[..SEGMENT_HASH_HEX_CHARS],
        extension
    ))
}

pub(crate) fn valid_upload_id(upload_id: &str) -> bool {
    upload_id.len() == UPLOAD_ID_HEX_CHARS
        && upload_id
            .bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
}

fn safe_path_component(component: &str) -> bool {
    !component.is_empty()
        && component != "."
        && component != ".."
        && !component.contains(['/', '\0'])
}
