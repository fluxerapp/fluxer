// SPDX-License-Identifier: AGPL-3.0-or-later

use core::cmp::Ordering;

use hashbrown::HashTable;

use crate::flags::Permissions;
use crate::hashing::Seeded;
use crate::ids::{GuildId, RoleId, RoleIdx};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum RankPos {
    Int(i64),
    Undefined,
}

impl Ord for RankPos {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (RankPos::Int(a), RankPos::Int(b)) => a.cmp(b),
            (RankPos::Int(_), RankPos::Undefined) => Ordering::Less,
            (RankPos::Undefined, RankPos::Int(_)) => Ordering::Greater,
            (RankPos::Undefined, RankPos::Undefined) => Ordering::Equal,
        }
    }
}

impl PartialOrd for RankPos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Hoist {
    NotHoisted,
    Hoisted(Option<i64>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RoleSpec {
    pub id: RoleId,
    pub permissions: Permissions,
    pub position: RankPos,
    pub hoist: Hoist,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Role {
    id: RoleId,
    permissions: Permissions,
    position: RankPos,
    hoist: Hoist,
    input_order: u32,
    present: bool,
}

impl Role {
    fn ghost(id: RoleId) -> Role {
        Role {
            id,
            permissions: Permissions::empty(),
            position: RankPos::Int(0),
            hoist: Hoist::NotHoisted,
            input_order: u32::MAX,
            present: false,
        }
    }

    pub fn id(&self) -> RoleId {
        self.id
    }

    pub fn permissions(&self) -> Permissions {
        self.permissions
    }

    pub fn position(&self) -> RankPos {
        self.position
    }

    pub fn hoist(&self) -> Hoist {
        self.hoist
    }

    pub fn input_order(&self) -> u32 {
        self.input_order
    }

    pub fn is_present(&self) -> bool {
        self.present
    }
}

#[derive(Clone, Debug)]
pub struct RoleTable {
    guild: GuildId,
    roles: Vec<Role>,
    present: Vec<RoleIdx>,
    index: HashTable<RoleIdx>,
    seeded: Seeded,
}

impl PartialEq for RoleTable {
    fn eq(&self, other: &Self) -> bool {
        self.guild == other.guild && self.roles == other.roles && self.present == other.present
    }
}

impl Eq for RoleTable {}

impl RoleTable {
    pub fn new(guild: GuildId) -> RoleTable {
        let mut table = RoleTable {
            guild,
            roles: Vec::new(),
            present: Vec::new(),
            index: HashTable::new(),
            seeded: Seeded::default(),
        };
        table.intern(RoleId::new(guild.get()));
        table
    }

    pub fn guild(&self) -> GuildId {
        self.guild
    }

    pub fn everyone(&self) -> RoleIdx {
        RoleIdx::EVERYONE
    }

    pub fn len(&self) -> usize {
        self.roles.len()
    }

    pub fn is_empty(&self) -> bool {
        self.roles.is_empty()
    }

    pub fn get(&self, idx: RoleIdx) -> Option<&Role> {
        self.roles.get(idx.index())
    }

    pub fn resolve(&self, id: RoleId) -> Option<RoleIdx> {
        let hash = self.seeded.hash(&id);
        let roles = &self.roles;
        self.index
            .find(hash, |idx| {
                roles.get(idx.index()).is_some_and(|r| r.id == id)
            })
            .copied()
    }

    pub fn permissions(&self, idx: RoleIdx) -> Permissions {
        self.get(idx)
            .map_or(Permissions::empty(), Role::permissions)
    }

    pub fn everyone_permissions(&self) -> Permissions {
        self.permissions(RoleIdx::EVERYONE)
    }

    pub fn iter(&self) -> impl Iterator<Item = (RoleIdx, &Role)> {
        self.roles
            .iter()
            .enumerate()
            .filter_map(|(i, role)| RoleIdx::from_index(i).map(|idx| (idx, role)))
    }

    pub fn present_in_order(&self) -> &[RoleIdx] {
        &self.present
    }

    pub(crate) fn intern(&mut self, id: RoleId) -> RoleIdx {
        if let Some(idx) = self.resolve(id) {
            return idx;
        }
        let Some(idx) = RoleIdx::from_index(self.roles.len()) else {
            return RoleIdx::EVERYONE;
        };
        self.roles.push(Role::ghost(id));
        let seeded = &self.seeded;
        let roles = &self.roles;
        let hash = seeded.hash(&id);
        self.index.insert_unique(hash, idx, |i| {
            roles.get(i.index()).map_or(0, |r| seeded.hash(&r.id))
        });
        idx
    }

    pub(crate) fn replace(&mut self, specs: &[RoleSpec]) {
        for role in &mut self.roles {
            role.present = false;
        }
        for (order, spec) in specs.iter().enumerate() {
            let idx = self.intern(spec.id);
            let input_order = u32::try_from(order).unwrap_or(u32::MAX);
            if let Some(role) = self.roles.get_mut(idx.index()) {
                role.permissions = spec.permissions;
                role.position = spec.position;
                role.hoist = spec.hoist;
                role.input_order = input_order;
                role.present = true;
            }
        }
        for role in &mut self.roles {
            if !role.present {
                *role = Role::ghost(role.id);
            }
        }
        let mut present: Vec<RoleIdx> = self
            .iter()
            .filter(|(_, role)| role.present)
            .map(|(idx, _)| idx)
            .collect();
        present.sort_by_key(|idx| self.get(*idx).map_or(u32::MAX, Role::input_order));
        self.present = present;
    }
}
