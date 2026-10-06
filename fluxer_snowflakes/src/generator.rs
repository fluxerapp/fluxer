// SPDX-License-Identifier: AGPL-3.0-or-later

use std::time::{Duration, SystemTime, UNIX_EPOCH};

pub const FLUXER_EPOCH_MS: u64 = 1_420_070_400_000;
pub const WORKER_ID_BITS: u64 = 10;
pub const SEQUENCE_BITS: u64 = 12;
pub const WORKER_ID_SHIFT: u64 = SEQUENCE_BITS;
pub const TIMESTAMP_SHIFT: u64 = WORKER_ID_BITS + SEQUENCE_BITS;
pub const MAX_WORKER_ID: u32 = (1 << WORKER_ID_BITS) - 1;
pub const MAX_SEQUENCE: u16 = (1 << SEQUENCE_BITS) - 1;

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SnowflakeParts {
    pub timestamp_ms: u64,
    pub worker_id: u16,
    pub sequence: u16,
}

pub trait SnowflakeClock: Clone + Send + Sync + 'static {
    fn now_ms(&self) -> u64;
}

#[derive(Clone, Copy, Debug)]
pub struct SystemClock;

pub struct SnowflakeGenerator<C = SystemClock>
where
    C: SnowflakeClock,
{
    worker_id: u16,
    sequence: u16,
    last_timestamp: Option<u64>,
    clock: C,
}

impl SnowflakeClock for SystemClock {
    fn now_ms(&self) -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as u64
    }
}

impl SnowflakeGenerator<SystemClock> {
    pub fn new(worker_id: u32) -> anyhow::Result<Self> {
        Self::with_clock(worker_id, SystemClock)
    }
}

impl<C> SnowflakeGenerator<C>
where
    C: SnowflakeClock,
{
    pub fn with_clock(worker_id: u32, clock: C) -> anyhow::Result<Self> {
        let worker_id = validate_worker_id(worker_id)?;
        Ok(Self {
            worker_id,
            sequence: 0,
            last_timestamp: None,
            clock,
        })
    }

    pub async fn generate(&mut self) -> anyhow::Result<u64> {
        let mut timestamp = self.current_relative_timestamp()?;
        match self.last_timestamp {
            Some(last_timestamp) if timestamp <= last_timestamp => {
                timestamp = last_timestamp;
                self.sequence = (self.sequence + 1) & MAX_SEQUENCE;
                if self.sequence == 0 {
                    timestamp = self.wait_until_next_timestamp(last_timestamp).await?;
                }
            }
            _ => self.sequence = 0,
        }
        self.last_timestamp = Some(timestamp);
        Ok(create_snowflake_from_relative_timestamp(
            timestamp,
            self.worker_id,
            self.sequence,
        ))
    }

    pub async fn generate_many(&mut self, count: u32) -> anyhow::Result<Vec<u64>> {
        let mut ids = Vec::with_capacity(count as usize);
        for _ in 0..count {
            ids.push(self.generate().await?);
        }
        Ok(ids)
    }

    fn current_relative_timestamp(&self) -> anyhow::Result<u64> {
        to_relative_timestamp(self.clock.now_ms())
    }

    async fn wait_until_next_timestamp(&self, last_timestamp: u64) -> anyhow::Result<u64> {
        loop {
            let timestamp = self.current_relative_timestamp()?;
            if timestamp > last_timestamp {
                return Ok(timestamp);
            }
            tokio::time::sleep(Duration::from_millis(1)).await;
        }
    }
}

#[cfg(test)]
pub fn create_snowflake(timestamp_ms: u64, worker_id: u32, sequence: u32) -> anyhow::Result<u64> {
    let relative_timestamp = to_relative_timestamp(timestamp_ms)?;
    let worker_id = validate_worker_id(worker_id)?;
    let sequence = validate_sequence(sequence)?;
    Ok(create_snowflake_from_relative_timestamp(
        relative_timestamp,
        worker_id,
        sequence,
    ))
}

#[cfg(test)]
pub fn parse_snowflake(snowflake: u64) -> SnowflakeParts {
    let relative_timestamp = snowflake >> TIMESTAMP_SHIFT;
    let worker_id = ((snowflake >> WORKER_ID_SHIFT) & u64::from(MAX_WORKER_ID)) as u16;
    let sequence = (snowflake & u64::from(MAX_SEQUENCE)) as u16;
    SnowflakeParts {
        timestamp_ms: relative_timestamp + FLUXER_EPOCH_MS,
        worker_id,
        sequence,
    }
}

fn create_snowflake_from_relative_timestamp(
    relative_timestamp: u64,
    worker_id: u16,
    sequence: u16,
) -> u64 {
    (relative_timestamp << TIMESTAMP_SHIFT)
        | (u64::from(worker_id) << WORKER_ID_SHIFT)
        | u64::from(sequence)
}

fn to_relative_timestamp(timestamp_ms: u64) -> anyhow::Result<u64> {
    if timestamp_ms < FLUXER_EPOCH_MS {
        anyhow::bail!("timestamp must be on or after the Fluxer epoch");
    }
    Ok(timestamp_ms - FLUXER_EPOCH_MS)
}

fn validate_worker_id(worker_id: u32) -> anyhow::Result<u16> {
    if worker_id > MAX_WORKER_ID {
        anyhow::bail!("worker ID must be between 0 and {MAX_WORKER_ID}");
    }
    Ok(worker_id as u16)
}

#[cfg(test)]
fn validate_sequence(sequence: u32) -> anyhow::Result<u16> {
    if sequence > u32::from(MAX_SEQUENCE) {
        anyhow::bail!("sequence must be between 0 and {MAX_SEQUENCE}");
    }
    Ok(sequence as u16)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicU64, Ordering};

    #[derive(Clone)]
    struct ManualClock {
        now: Arc<AtomicU64>,
    }

    impl ManualClock {
        fn new(now_ms: u64) -> Self {
            Self {
                now: Arc::new(AtomicU64::new(now_ms)),
            }
        }

        fn set(&self, now_ms: u64) {
            self.now.store(now_ms, Ordering::SeqCst);
        }
    }

    impl SnowflakeClock for ManualClock {
        fn now_ms(&self) -> u64 {
            self.now.load(Ordering::SeqCst)
        }
    }

    #[test]
    fn constants_match_compatible_layout() {
        assert_eq!(FLUXER_EPOCH_MS, 1_420_070_400_000);
        assert_eq!(TIMESTAMP_SHIFT, 22);
        assert_eq!(MAX_WORKER_ID, 1023);
        assert_eq!(MAX_SEQUENCE, 4095);
    }

    #[test]
    fn creates_and_parses_known_snowflake() {
        let snowflake = create_snowflake(FLUXER_EPOCH_MS + 1_000_000, 3, 77).unwrap();
        let parsed = parse_snowflake(snowflake);
        assert_eq!(parsed.timestamp_ms, FLUXER_EPOCH_MS + 1_000_000);
        assert_eq!(parsed.worker_id, 3);
        assert_eq!(parsed.sequence, 77);
    }

    #[tokio::test]
    async fn generator_starts_sequence_at_zero() {
        let clock = ManualClock::new(FLUXER_EPOCH_MS + 1000);
        let mut generator = SnowflakeGenerator::with_clock(42, clock).unwrap();
        let id = generator.generate().await.unwrap();
        let parsed = parse_snowflake(id);
        assert_eq!(parsed.timestamp_ms, FLUXER_EPOCH_MS + 1000);
        assert_eq!(parsed.worker_id, 42);
        assert_eq!(parsed.sequence, 0);
    }

    #[tokio::test]
    async fn generator_increments_sequence_within_same_millisecond() {
        let clock = ManualClock::new(FLUXER_EPOCH_MS + 1000);
        let mut generator = SnowflakeGenerator::with_clock(7, clock).unwrap();
        let first = parse_snowflake(generator.generate().await.unwrap());
        let second = parse_snowflake(generator.generate().await.unwrap());
        assert_eq!(first.sequence, 0);
        assert_eq!(second.sequence, 1);
        assert_eq!(second.timestamp_ms, first.timestamp_ms);
    }

    #[tokio::test]
    async fn generator_remains_monotonic_when_clock_moves_backwards() {
        let clock = ManualClock::new(FLUXER_EPOCH_MS + 1002);
        let mut generator = SnowflakeGenerator::with_clock(1, clock.clone()).unwrap();
        let first = generator.generate().await.unwrap();
        clock.set(FLUXER_EPOCH_MS + 1001);
        let second = generator.generate().await.unwrap();
        assert!(second > first);
        assert_eq!(parse_snowflake(second).timestamp_ms, FLUXER_EPOCH_MS + 1002);
    }

    #[tokio::test]
    async fn generator_waits_for_next_millisecond_on_sequence_overflow() {
        let clock = ManualClock::new(FLUXER_EPOCH_MS + 1000);
        let mut generator = SnowflakeGenerator::with_clock(1, clock.clone()).unwrap();
        for _ in 0..=MAX_SEQUENCE {
            generator.generate().await.unwrap();
        }
        let advance_clock = clock.clone();
        tokio::spawn(async move {
            tokio::time::sleep(Duration::from_millis(2)).await;
            advance_clock.set(FLUXER_EPOCH_MS + 1001);
        });
        let id = generator.generate().await.unwrap();
        let parsed = parse_snowflake(id);
        assert_eq!(parsed.timestamp_ms, FLUXER_EPOCH_MS + 1001);
        assert_eq!(parsed.sequence, 0);
    }

    #[test]
    fn validates_worker_and_sequence_bounds() {
        assert!(create_snowflake(FLUXER_EPOCH_MS, 1023, 4095).is_ok());
        assert!(create_snowflake(FLUXER_EPOCH_MS, 1024, 0).is_err());
        assert!(create_snowflake(FLUXER_EPOCH_MS, 0, 4096).is_err());
        assert!(create_snowflake(FLUXER_EPOCH_MS - 1, 0, 0).is_err());
    }
}
