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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_starts_empty() {
        let error = AggregateError::new("op");
        assert!(error.is_empty());
        assert_eq!(error.len(), 0);
    }

    #[test]
    fn push_result_ok_leaves_empty_and_err_records_error() {
        let mut error = AggregateError::new("op");
        error.push_result::<i32>(Ok(1));
        assert!(error.is_empty());
        error.push_result::<i32>(Err(anyhow::anyhow!("boom")));
        assert_eq!(error.len(), 1);
    }

    #[test]
    fn finish_ok_when_no_errors_were_recorded() {
        let error = AggregateError::new("op");
        assert!(error.finish().is_ok());
    }

    #[test]
    fn finish_err_returns_self_as_downcastable_error_when_non_empty() {
        let mut error = AggregateError::new("op");
        error.push(anyhow::anyhow!("boom"));
        let aggregate = error
            .finish()
            .expect_err("a recorded failure must surface")
            .downcast::<AggregateError>()
            .expect("finish wraps itself as the error");
        assert_eq!(aggregate.len(), 1);
    }

    #[test]
    fn display_numbers_every_failure_from_one() {
        let mut error = AggregateError::new("op");
        error.push(anyhow::anyhow!("first"));
        error.push(anyhow::anyhow!("second"));
        assert_eq!(
            format!("{error}"),
            "op produced 2 independent failures\n1. first\n2. second"
        );
    }

    #[test]
    fn aggregate_results_ok_when_all_succeed() {
        let results: Vec<anyhow::Result<i32>> = vec![Ok(1), Ok(2), Ok(3)];
        assert!(aggregate_results("agg", results).is_ok());
    }

    #[test]
    fn aggregate_results_collects_every_failure_without_short_circuiting() {
        let results: Vec<anyhow::Result<i32>> = vec![
            Ok(1),
            Err(anyhow::anyhow!("first")),
            Ok(2),
            Err(anyhow::anyhow!("second")),
        ];
        let aggregate = aggregate_results("agg", results)
            .expect_err("recorded failures must surface")
            .downcast::<AggregateError>()
            .expect("aggregate error");
        assert_eq!(aggregate.len(), 2);
        assert_eq!(
            format!("{aggregate}"),
            "agg produced 2 independent failures\n1. first\n2. second"
        );
    }
}
