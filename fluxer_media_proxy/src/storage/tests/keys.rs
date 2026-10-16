// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::storage::keys::{safe_bucket, safe_key};

#[test]
fn safe_key_rejects_traversal() {
    assert!(safe_key("a/b").is_ok());
    assert!(safe_key("../x").is_err());
    assert!(safe_key("a//b").is_err());
    assert!(safe_key("/a").is_err());
    assert!(safe_key("users/1/file").is_ok());
    assert!(safe_key("users/../file").is_err());
    assert!(safe_key("users//file").is_err());
    assert!(safe_key("users/1\\file").is_ok());
    assert!(safe_key("users/1\0file").is_err());
    let longest_key = vec!["a".repeat(204); 5].join("/");
    assert_eq!(1_024, longest_key.len());
    assert!(safe_key(&longest_key).is_ok());
    assert!(safe_key(&format!("{longest_key}a")).is_ok());
    assert!(safe_key(&vec!["a"; 128].join("/")).is_ok());
    assert!(safe_key(&vec!["a"; 129].join("/")).is_ok());
    assert!(safe_key(&format!("a/{}", "b".repeat(256))).is_ok());

    assert!(safe_bucket("cdn").is_ok());
    assert!(safe_bucket("").is_err());
    assert!(safe_bucket(".").is_err());
    assert!(safe_bucket("..").is_err());
    assert!(safe_bucket("buck/et").is_err());
    assert!(safe_bucket("buck\\et").is_ok());
    assert!(safe_bucket("buck\0et").is_err());
    assert!(safe_bucket(&"a".repeat(255)).is_ok());
    assert!(safe_bucket(&"a".repeat(256)).is_ok());
}

#[test]
fn safe_key_accepts_the_longest_attachment_key_the_schema_can_produce() {
    let filename = format!("{}.png", "\u{597d}".repeat(255));
    assert_eq!(769, filename.len());
    let key = format!("attachments/1234567890123456789/9876543210987654321/{filename}");
    assert_eq!(821, key.len());
    assert!(safe_key(&key).is_ok());
}

#[test]
fn safe_key_accepts_a_backslash_and_still_rejects_traversal_and_nul() {
    assert!(safe_key("attachments/1/2/back\\slash.png").is_ok());
    assert!(safe_key("attachments/1/2/\\").is_ok());
    assert!(safe_bucket("buck\\et").is_ok());

    assert!(safe_key("attachments/1/2/nul\0.png").is_err());
    assert!(safe_bucket("buck\0et").is_err());
    assert!(safe_key("attachments/../secret.png").is_err());
    assert!(safe_key("attachments/./secret.png").is_err());
    assert!(safe_key("attachments/..\\secret.png").is_ok());
    assert!(safe_key("/attachments/1.png").is_err());
    assert!(safe_key("attachments//1.png").is_err());
}
