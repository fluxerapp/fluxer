// SPDX-License-Identifier: AGPL-3.0-or-later

use bytes::Bytes;
use std::ops::Deref;
use std::sync::{
    Arc,
    atomic::{AtomicU64, Ordering},
};

#[derive(Clone, Debug)]
pub struct ByteBudget {
    inner: Arc<ByteBudgetInner>,
}

#[derive(Debug)]
struct ByteBudgetInner {
    limit: u64,
    used: AtomicU64,
}

#[derive(Debug)]
pub struct ByteReservation {
    inner: Arc<ByteReservationInner>,
}

#[derive(Debug)]
pub struct BudgetedBytes {
    data: Bytes,
    reservation: Option<ByteReservation>,
}

#[derive(Debug)]
struct ByteReservationInner {
    budget: ByteBudget,
    amount: AtomicU64,
}

impl ByteBudgetInner {
    fn release(&self, amount: u64) {
        if amount == 0 {
            return;
        }
        assert!(amount <= self.limit);
        let released = self
            .used
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |used| {
                used.checked_sub(amount)
            });
        assert!(released.is_ok());
    }
}

impl ByteBudget {
    pub fn new(limit: usize) -> Self {
        Self {
            inner: Arc::new(ByteBudgetInner {
                limit: limit as u64,
                used: AtomicU64::new(0),
            }),
        }
    }

    pub fn try_reserve(&self, amount: usize) -> Option<ByteReservation> {
        let amount = amount as u64;
        let mut used = self.inner.used.load(Ordering::Acquire);
        loop {
            let next = used
                .checked_add(amount)
                .filter(|next| *next <= self.inner.limit)?;
            match self.inner.used.compare_exchange_weak(
                used,
                next,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    return Some(ByteReservation {
                        inner: Arc::new(ByteReservationInner {
                            budget: self.clone(),
                            amount: AtomicU64::new(amount),
                        }),
                    });
                }
                Err(actual) => used = actual,
            }
        }
    }
}

impl ByteReservation {
    pub fn amount(&self) -> usize {
        self.inner.amount.load(Ordering::Acquire) as usize
    }

    pub fn try_grow(&mut self, additional: usize) -> bool {
        assert_eq!(Arc::strong_count(&self.inner), 1);
        if additional == 0 {
            return true;
        }
        let additional = additional as u64;
        let mut used = self.inner.budget.inner.used.load(Ordering::Acquire);
        loop {
            let Some(next) = used
                .checked_add(additional)
                .filter(|next| *next <= self.inner.budget.inner.limit)
            else {
                return false;
            };
            match self.inner.budget.inner.used.compare_exchange_weak(
                used,
                next,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    let previous = self.inner.amount.fetch_add(additional, Ordering::AcqRel);
                    assert!(previous.checked_add(additional).is_some());
                    return true;
                }
                Err(actual) => used = actual,
            }
        }
    }

    pub fn shrink_to(&mut self, amount: usize) {
        assert_eq!(Arc::strong_count(&self.inner), 1);
        let amount = amount as u64;
        let previous = self.inner.amount.load(Ordering::Acquire);
        assert!(amount <= previous);
        self.inner.amount.store(amount, Ordering::Release);
        self.inner.budget.inner.release(previous - amount);
    }
}

impl Clone for BudgetedBytes {
    fn clone(&self) -> Self {
        let reservation = self
            .reservation
            .as_ref()
            .map(|reservation| ByteReservation {
                inner: Arc::clone(&reservation.inner),
            });
        Self {
            data: self.data.clone(),
            reservation,
        }
    }
}

impl BudgetedBytes {
    pub fn unbudgeted(data: Bytes) -> Self {
        Self {
            data,
            reservation: None,
        }
    }

    pub fn budgeted(data: Bytes, reservation: ByteReservation) -> Self {
        assert!(data.len() <= reservation.amount());
        Self {
            data,
            reservation: Some(reservation),
        }
    }
    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn resident_bytes(&self) -> usize {
        self.reservation
            .as_ref()
            .map(ByteReservation::amount)
            .unwrap_or_else(|| self.data.len())
    }

    pub fn as_bytes(&self) -> &Bytes {
        &self.data
    }
}

impl From<Bytes> for BudgetedBytes {
    fn from(data: Bytes) -> Self {
        Self::unbudgeted(data)
    }
}

impl AsRef<[u8]> for BudgetedBytes {
    fn as_ref(&self) -> &[u8] {
        &self.data
    }
}

impl Deref for BudgetedBytes {
    type Target = Bytes;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl Drop for ByteReservationInner {
    fn drop(&mut self) {
        let amount = self.amount.load(Ordering::Acquire);
        self.budget.inner.release(amount);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_reserve_up_to_limit_succeeds() {
        let budget = ByteBudget::new(100);
        let reservation = budget.try_reserve(60).expect("reserve under limit");
        assert_eq!(reservation.amount(), 60);
    }

    #[test]
    fn try_reserve_exactly_at_limit_succeeds() {
        let budget = ByteBudget::new(100);
        let reservation = budget.try_reserve(100).expect("reserve at limit");
        assert_eq!(reservation.amount(), 100);
    }

    #[test]
    fn try_reserve_over_limit_fails() {
        let budget = ByteBudget::new(100);
        let _reservation = budget.try_reserve(100).expect("reserve at limit");
        assert!(budget.try_reserve(1).is_none());
    }

    #[test]
    fn try_grow_within_remaining_budget_succeeds() {
        let budget = ByteBudget::new(100);
        let mut reservation = budget.try_reserve(60).expect("reserve under limit");
        assert!(reservation.try_grow(30));
        assert_eq!(reservation.amount(), 90);
    }

    #[test]
    fn try_grow_past_remaining_budget_fails_and_leaves_reservation_unchanged() {
        let budget = ByteBudget::new(100);
        let mut reservation = budget.try_reserve(60).expect("reserve under limit");
        assert!(!reservation.try_grow(41));
        assert_eq!(reservation.amount(), 60);
        let remaining = budget.try_reserve(40).expect("untouched remaining budget");
        assert_eq!(remaining.amount(), 40);
    }

    #[test]
    fn try_grow_zero_is_a_no_op() {
        let budget = ByteBudget::new(100);
        let mut reservation = budget.try_reserve(60).expect("reserve under limit");
        assert!(reservation.try_grow(0));
        assert_eq!(reservation.amount(), 60);
    }

    #[test]
    fn shrink_to_releases_delta_back_to_budget() {
        let budget = ByteBudget::new(100);
        let mut reservation = budget.try_reserve(100).expect("reserve at limit");
        reservation.shrink_to(70);
        assert_eq!(reservation.amount(), 70);
        let released = budget.try_reserve(30).expect("released delta available");
        assert_eq!(released.amount(), 30);
    }

    #[test]
    fn drop_releases_full_reservation() {
        let budget = ByteBudget::new(100);
        let reservation = budget.try_reserve(100).expect("reserve at limit");
        drop(reservation);
        let reservation = budget
            .try_reserve(100)
            .expect("full budget available again");
        assert_eq!(reservation.amount(), 100);
    }

    #[test]
    fn resident_bytes_uses_reservation_amount_when_budgeted() {
        let budget = ByteBudget::new(100);
        let reservation = budget.try_reserve(50).expect("reserve under limit");
        let budgeted = BudgetedBytes::budgeted(Bytes::from_static(b"hello"), reservation);
        assert_eq!(budgeted.len(), 5);
        assert_eq!(budgeted.resident_bytes(), 50);
    }

    #[test]
    fn resident_bytes_uses_data_len_when_unbudgeted() {
        let unbudgeted = BudgetedBytes::unbudgeted(Bytes::from_static(b"hello"));
        assert_eq!(unbudgeted.resident_bytes(), 5);
    }

    #[test]
    fn empty_data_reports_empty_and_exposes_the_underlying_bytes() {
        let empty = BudgetedBytes::unbudgeted(Bytes::new());
        assert!(empty.is_empty());
        assert_eq!(empty.as_bytes(), &Bytes::new());
        let hello = BudgetedBytes::unbudgeted(Bytes::from_static(b"hello"));
        assert!(!hello.is_empty());
        assert_eq!(hello.as_bytes(), &Bytes::from_static(b"hello"));
    }
}
