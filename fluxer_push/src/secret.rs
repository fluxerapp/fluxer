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

#[derive(Clone)]
pub struct SecretString(String);

impl SecretString {
    pub fn new(value: String) -> Self {
        Self(value)
    }

    pub fn expose(&self) -> &str {
        self.0.as_str()
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
