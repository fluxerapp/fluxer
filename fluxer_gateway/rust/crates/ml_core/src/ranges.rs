// SPDX-License-Identifier: AGPL-3.0-or-later

use smallvec::SmallVec;

pub const MAX_END: u32 = 100_000;
pub const MAX_SPAN: u32 = 99;
pub const MAX_RANGES: usize = 10;
pub const MAX_SYNC_RANGES: usize = 20;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Range {
    start: u32,
    end: u32,
}

impl Range {
    pub fn new(start: u32, end: u32) -> Option<Range> {
        let valid = start <= end && end <= MAX_END && end.saturating_sub(start) <= MAX_SPAN;
        valid.then_some(Range { start, end })
    }

    pub const fn start(self) -> u32 {
        self.start
    }

    pub const fn end(self) -> u32 {
        self.end
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Ranges(SmallVec<[Range; 2]>);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RangeError {
    Invalid(u32, u32),
    Unsorted,
    TooMany,
}

impl Ranges {
    pub fn empty() -> Ranges {
        Ranges::default()
    }

    pub fn normalized(items: &[(u32, u32)]) -> Result<Ranges, RangeError> {
        Ranges::bounded(items, MAX_RANGES)
    }

    pub fn sync(items: &[(u32, u32)]) -> Result<Ranges, RangeError> {
        Ranges::bounded(items, MAX_SYNC_RANGES)
    }

    fn bounded(items: &[(u32, u32)], max: usize) -> Result<Ranges, RangeError> {
        if items.len() > max {
            return Err(RangeError::TooMany);
        }
        let mut out: SmallVec<[Range; 2]> = SmallVec::new();
        for (start, end) in items {
            let range = Range::new(*start, *end).ok_or(RangeError::Invalid(*start, *end))?;
            if let Some(prev) = out.last()
                && range.start <= prev.end
            {
                return Err(RangeError::Unsorted);
            }
            out.push(range);
        }
        Ok(Ranges(out))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = Range> + '_ {
        self.0.iter().copied()
    }

    pub fn to_pairs(&self) -> Vec<(u32, u32)> {
        self.0.iter().map(|r| (r.start, r.end)).collect()
    }

    pub fn rows(&self) -> u64 {
        self.0
            .iter()
            .map(|r| u64::from(r.end.saturating_sub(r.start)).saturating_add(1))
            .fold(0, u64::saturating_add)
    }
}
