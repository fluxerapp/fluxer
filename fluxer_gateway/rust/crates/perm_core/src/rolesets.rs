// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;

use hashbrown::HashTable;
use smallvec::SmallVec;

use crate::bits;
use crate::flags::Permissions;
use crate::hashing::Seeded;
use crate::ids::{RoleIdx, RoleSetId};
use crate::roles::RoleTable;

pub const DEAD: u32 = u32::MAX;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Base {
    All,
    Bits(Permissions),
}

impl Base {
    pub fn of(permissions: Permissions) -> Base {
        if permissions.contains(Permissions::ADMINISTRATOR) {
            Base::All
        } else {
            Base::Bits(permissions)
        }
    }

    pub fn permissions(self) -> Permissions {
        match self {
            Base::All => Permissions::all(),
            Base::Bits(bits) => bits,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RoleSet {
    roles: SmallVec<[RoleIdx; 4]>,
    base: Base,
}

impl RoleSet {
    pub fn roles(&self) -> &[RoleIdx] {
        &self.roles
    }

    pub fn base(&self) -> Base {
        self.base
    }

    pub fn contains(&self, role: RoleIdx) -> bool {
        self.roles.binary_search(&role).is_ok()
    }
}

#[derive(Clone, Debug)]
pub struct RoleSets {
    sets: Vec<RoleSet>,
    lookup: HashTable<RoleSetId>,
    seeded: Seeded,
    generation: u32,
    remap: Option<Arc<[u32]>>,
}

impl RoleSets {
    pub fn new(roles: &RoleTable) -> RoleSets {
        RoleSets {
            sets: vec![RoleSet {
                roles: SmallVec::new(),
                base: Base::of(roles.everyone_permissions()),
            }],
            lookup: HashTable::new(),
            seeded: Seeded::default(),
            generation: 0,
            remap: None,
        }
    }

    pub fn len(&self) -> usize {
        self.sets.len()
    }

    pub fn is_empty(&self) -> bool {
        self.sets.is_empty()
    }

    pub fn words(&self) -> usize {
        bits::words_for(self.sets.len())
    }

    pub fn get(&self, id: RoleSetId) -> Option<&RoleSet> {
        self.sets.get(id.index())
    }

    pub fn base(&self, id: RoleSetId) -> Base {
        self.get(id)
            .map_or(Base::Bits(Permissions::empty()), RoleSet::base)
    }

    pub fn iter(&self) -> impl Iterator<Item = (RoleSetId, &RoleSet)> {
        self.sets
            .iter()
            .enumerate()
            .filter_map(|(i, set)| RoleSetId::from_index(i).map(|id| (id, set)))
    }

    pub fn generation(&self) -> u32 {
        self.generation
    }

    pub fn remap(&self) -> Option<&Arc<[u32]>> {
        self.remap.as_ref()
    }

    pub fn find(&self, roles: &[RoleIdx]) -> Option<RoleSetId> {
        if roles.is_empty() {
            return Some(RoleSetId::EVERYONE);
        }
        let hash = self.seeded.hash(roles);
        let sets = &self.sets;
        self.lookup
            .find(hash, |id| {
                sets.get(id.index())
                    .is_some_and(|set| set.roles.as_slice() == roles)
            })
            .copied()
    }

    pub(crate) fn holding_any(&self, roles: &[RoleIdx]) -> Vec<RoleSetId> {
        self.iter()
            .filter(|(_, set)| set.roles.iter().any(|r| roles.binary_search(r).is_ok()))
            .map(|(id, _)| id)
            .collect()
    }

    pub(crate) fn intern(&mut self, roles: &[RoleIdx], table: &RoleTable) -> Option<RoleSetId> {
        if let Some(id) = self.find(roles) {
            return Some(id);
        }
        let id = RoleSetId::from_index(self.sets.len())?;
        self.sets.push(RoleSet {
            roles: SmallVec::from_slice(roles),
            base: base_of(roles, table),
        });
        let hash = self.seeded.hash(roles);
        let seeded = &self.seeded;
        let sets = &self.sets;
        self.lookup.insert_unique(hash, id, |i| {
            sets.get(i.index())
                .map_or(0, |set| seeded.hash(set.roles.as_slice()))
        });
        Some(id)
    }

    pub(crate) fn rebase(&mut self, ids: &[RoleSetId], table: &RoleTable) {
        for id in ids {
            if let Some(set) = self.sets.get_mut(id.index()) {
                set.base = base_of(&set.roles, table);
            }
        }
    }

    pub(crate) fn rebase_all(&mut self, table: &RoleTable) {
        for set in &mut self.sets {
            set.base = base_of(&set.roles, table);
        }
    }

    pub(crate) fn compacted(&self, live: impl Fn(RoleSetId) -> bool) -> RoleSets {
        let mut remap = vec![DEAD; self.sets.len()];
        let mut sets = Vec::new();
        for (i, set) in self.sets.iter().enumerate() {
            let keep = i == 0 || RoleSetId::from_index(i).is_some_and(&live);
            if !keep {
                continue;
            }
            if let (Some(slot), Ok(next)) = (remap.get_mut(i), u32::try_from(sets.len())) {
                *slot = next;
            }
            sets.push(set.clone());
        }
        let seeded = self.seeded.clone();
        let mut lookup = HashTable::new();
        for (i, set) in sets.iter().enumerate().skip(1) {
            if let Some(id) = RoleSetId::from_index(i) {
                lookup.insert_unique(seeded.hash(set.roles.as_slice()), id, |j: &RoleSetId| {
                    sets.get(j.index())
                        .map_or(0, |s| seeded.hash(s.roles.as_slice()))
                });
            }
        }
        RoleSets {
            sets,
            lookup,
            seeded,
            generation: self.generation.wrapping_add(1),
            remap: Some(remap.into()),
        }
    }
}

pub(crate) fn normalize(mut roles: SmallVec<[RoleIdx; 4]>) -> SmallVec<[RoleIdx; 4]> {
    roles.sort_unstable();
    roles.dedup();
    roles
}

fn base_of(roles: &[RoleIdx], table: &RoleTable) -> Base {
    let permissions = roles.iter().fold(table.everyone_permissions(), |acc, idx| {
        acc | table.permissions(*idx)
    });
    Base::of(permissions)
}
