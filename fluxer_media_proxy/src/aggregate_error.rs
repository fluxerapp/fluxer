// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct AggregateError {
    operation: &'static str,
    errors: Vec<anyhow::Error>,
}

impl AggregateError {
    pub const fn new(operation: &'static str) -> Self {
        Self {
            operation,
            errors: Vec::new(),
        }
    }

    pub fn push(&mut self, error: anyhow::Error) {
        self.errors.push(error);
    }

    pub fn push_result<T>(&mut self, result: anyhow::Result<T>) {
        if let Err(error) = result {
            self.push(error);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn finish(self) -> anyhow::Result<()> {
        if self.is_empty() {
            Ok(())
        } else {
            Err(anyhow::Error::new(self))
        }
    }
}

impl Display for AggregateError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{} produced {} independent failures",
            self.operation,
            self.len()
        )?;
        for (index, error) in self.errors.iter().enumerate() {
            write!(formatter, "\n{}. {error:#}", index + 1)?;
        }
        Ok(())
    }
}

impl std::error::Error for AggregateError {}

pub fn aggregate_results<T>(
    operation: &'static str,
    results: impl IntoIterator<Item = anyhow::Result<T>>,
) -> anyhow::Result<()> {
    let mut failures = AggregateError::new(operation);
    for result in results {
        failures.push_result(result);
    }
    failures.finish()
}
