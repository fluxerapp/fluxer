// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt;

#[derive(Clone, Debug)]
pub struct ErrorReason {
    pub code: &'static str,
    pub source: Option<String>,
}

impl ErrorReason {
    pub fn new(code: &'static str) -> Self {
        Self { code, source: None }
    }

    pub fn with_source(code: &'static str, source: impl fmt::Debug) -> Self {
        Self {
            code,
            source: Some(format!("{source:?}")),
        }
    }

    pub fn with_message(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            source: Some(message.into()),
        }
    }
}
