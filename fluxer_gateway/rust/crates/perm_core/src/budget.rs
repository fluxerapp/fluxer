// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub struct Units(u64);

impl Units {
    pub const ZERO: Units = Units(0);

    pub const fn new(units: u64) -> Units {
        Units(units)
    }

    pub const fn get(self) -> u64 {
        self.0
    }

    pub const fn plus(self, other: Units) -> Units {
        Units(self.0.saturating_add(other.0))
    }

    pub const fn times(self, factor: u64) -> Units {
        Units(self.0.saturating_mul(factor))
    }

    pub fn fraction_percent(self, budget: Budget) -> u8 {
        let limit = budget.limit().get().max(1);
        let percent = self.0.saturating_mul(100).div_euclid(limit);
        u8::try_from(percent.clamp(1, 100)).unwrap_or(100)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Budget {
    limit: Units,
}

impl Budget {
    pub const DEFAULT_UNITS: u64 = 250_000;

    pub const fn new(limit: Units) -> Budget {
        Budget { limit }
    }

    pub const fn limit(self) -> Units {
        self.limit
    }

    pub fn fits(self, estimate: Units) -> bool {
        estimate <= self.limit
    }

    pub fn runaway_cap(self, estimate: Units) -> Units {
        estimate.max(self.limit).times(10)
    }
}

impl Default for Budget {
    fn default() -> Budget {
        Budget::new(Units::new(Budget::DEFAULT_UNITS))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Meter {
    spent: Units,
    cap: Units,
}

impl Default for Meter {
    fn default() -> Meter {
        Meter::new()
    }
}

impl Meter {
    pub const fn new() -> Meter {
        Meter {
            spent: Units::ZERO,
            cap: Units(u64::MAX),
        }
    }

    pub const fn capped(cap: Units) -> Meter {
        Meter {
            spent: Units::ZERO,
            cap,
        }
    }

    pub fn charge(&mut self, units: u64) {
        self.spent = self.spent.plus(Units::new(units));
    }

    pub const fn spent(self) -> Units {
        self.spent
    }

    pub fn exhausted(self) -> bool {
        self.spent > self.cap
    }
}
