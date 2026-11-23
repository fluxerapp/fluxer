// SPDX-License-Identifier: AGPL-3.0-or-later

pub use perm_core::budget::{Budget, Meter, Units};

use crate::ranges::Ranges;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct Shape {
    pub members: u64,
    pub online: u64,
    pub role_sets: u64,
    pub overrides: u64,
    pub columns: u64,
    pub sections: u64,
    pub lists: u64,
    pub max_offline_build: u64,
}

pub fn log2(n: u64) -> u64 {
    u64::from(64u32.saturating_sub(n.leading_zeros())).max(1)
}

impl Shape {
    pub fn point_write(self) -> Units {
        let search = log2(self.members.max(self.online)).saturating_mul(4);
        let columns = self.columns.saturating_mul(6);
        let structural = self.columns.saturating_add(1).saturating_mul(512);
        Units::new(
            search
                .saturating_mul(4)
                .saturating_add(columns)
                .saturating_add(structural)
                .saturating_add(
                    self.columns
                        .saturating_mul(log2(1000).saturating_mul(2).saturating_add(32)),
                )
                .saturating_add(300),
        )
    }

    pub fn counts(self) -> Units {
        Units::new(
            self.role_sets
                .saturating_add(self.overrides.saturating_mul(2))
                .saturating_add(self.sections)
                .saturating_add(8),
        )
    }

    pub fn render(self, ranges: &Ranges) -> Units {
        let rows = ranges.rows();
        let walk = self
            .online
            .div_ceil(64)
            .saturating_add(self.sections.saturating_mul(4))
            .saturating_add(rows.saturating_mul(4));
        Units::new(walk.saturating_mul(ranges.len().max(1) as u64))
            .plus(self.counts())
            .plus(Units::new(self.max_offline_build))
    }

    pub fn column_fill(self) -> Units {
        Units::new(
            self.online
                .saturating_mul(2)
                .saturating_add(self.sections.saturating_mul(4))
                .saturating_add(self.role_sets)
                .saturating_add(self.overrides.saturating_mul(2))
                .saturating_add(64),
        )
    }

    pub fn offline_build(self, reach: u64) -> Units {
        Units::new(
            self.members
                .saturating_mul(2)
                .saturating_add(reach.saturating_mul(16))
                .saturating_add(64),
        )
    }

    pub fn rehoist(self) -> Units {
        let per_entry = self.columns.saturating_add(4);
        Units::new(
            self.online
                .saturating_mul(per_entry)
                .saturating_mul(4)
                .saturating_add(self.role_sets.saturating_mul(8))
                .saturating_add(512),
        )
    }

    pub fn load(self) -> Units {
        let sort = self
            .members
            .saturating_mul(log2(self.members))
            .saturating_mul(2);
        Units::new(
            sort.saturating_add(self.members.saturating_mul(24))
                .saturating_add(256),
        )
    }
}
