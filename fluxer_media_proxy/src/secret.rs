// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt;
use std::sync::atomic::{Ordering, compiler_fence};

const REDACTED: &str = "[REDACTED]";

fn zero(bytes: &mut [u8]) {
    for byte in bytes.iter_mut() {
        unsafe { std::ptr::write_volatile(byte, 0) };
    }
    compiler_fence(Ordering::SeqCst);
}

#[derive(Clone, Default)]
pub struct SecretString(String);

impl SecretString {
    pub fn new(value: String) -> Self {
        Self(value)
    }

    pub fn expose(&self) -> &str {
        self.0.as_str()
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Drop for SecretString {
    fn drop(&mut self) {
        zero(unsafe { self.0.as_mut_vec() });
    }
}

impl fmt::Debug for SecretString {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(REDACTED)
    }
}

#[derive(Clone, Default)]
pub struct SecretBytes(Vec<u8>);

impl SecretBytes {
    pub fn new(value: Vec<u8>) -> Self {
        Self(value)
    }

    pub fn expose(&self) -> &[u8] {
        self.0.as_slice()
    }
}

impl Drop for SecretBytes {
    fn drop(&mut self) {
        zero(self.0.as_mut_slice());
    }
}

impl fmt::Debug for SecretBytes {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(REDACTED)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zeroing_overwrites_every_byte_in_place() {
        let mut buffer = [1u8, 2, 3, 4, 5];
        zero(&mut buffer);
        assert_eq!([0u8; 5], buffer);
    }

    #[test]
    fn secrets_expose_their_value_without_printing_it() {
        let string = SecretString::new("hunter2".to_owned());
        assert_eq!("hunter2", string.expose());
        assert_eq!(b"hunter2", string.as_bytes());
        assert!(!string.is_empty());
        assert_eq!("[REDACTED]", format!("{string:?}"));

        let bytes = SecretBytes::new(vec![7u8; 32]);
        assert_eq!(&[7u8; 32][..], bytes.expose());
        assert_eq!("[REDACTED]", format!("{bytes:?}"));
    }

    #[test]
    fn empty_secrets_report_themselves_empty() {
        assert!(SecretString::default().is_empty());
        assert!(SecretString::new(String::new()).is_empty());
        assert!(SecretBytes::default().expose().is_empty());
    }

    #[test]
    fn cloning_a_secret_keeps_the_original_readable_after_the_clone_drops() {
        let original = SecretString::new("shared".to_owned());
        drop(original.clone());
        assert_eq!("shared", original.expose());
    }
}
