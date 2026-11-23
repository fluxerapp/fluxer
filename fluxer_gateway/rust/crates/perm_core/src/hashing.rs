// SPDX-License-Identifier: AGPL-3.0-or-later

use core::fmt;
use core::hash::{BuildHasher, Hash};

use foldhash::fast::RandomState;

#[derive(Clone, Default)]
pub(crate) struct Seeded(RandomState);

impl Seeded {
    pub(crate) fn hash<T: Hash + ?Sized>(&self, value: &T) -> u64 {
        self.0.hash_one(value)
    }
}

impl fmt::Debug for Seeded {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Seeded")
    }
}
