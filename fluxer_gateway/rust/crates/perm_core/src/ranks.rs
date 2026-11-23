// SPDX-License-Identifier: AGPL-3.0-or-later

use core::cmp::Ordering;

use crate::flags::Permissions;
use crate::ids::{RoleId, UserId};
use crate::roles::{RankPos, Role};
use crate::shared::{PermsError, View};
use crate::views::Scope;

fn better(a: &Role, b: &Role) -> bool {
    match a.position().cmp(&b.position()) {
        Ordering::Greater => true,
        Ordering::Less => false,
        Ordering::Equal => a.id() < b.id(),
    }
}

impl View<'_> {
    fn present_role(&self, id: RoleId) -> Option<&Role> {
        let roles = &self.model.roles;
        roles
            .resolve(id)
            .and_then(|idx| roles.get(idx))
            .filter(|r| r.is_present())
    }

    fn member_roles(&self, user: UserId) -> impl Iterator<Item = &Role> {
        let model = &self.model;
        self.members
            .get(user)
            .filter(|e| !e.is_quarantined())
            .and_then(|e| model.rolesets.get(e.roleset()))
            .map(|set| set.roles())
            .unwrap_or(&[])
            .iter()
            .filter_map(|idx| model.roles.get(*idx))
            .filter(|r| r.is_present())
    }

    fn max_position(&self, user: UserId) -> RankPos {
        let mut max: Option<i64> = None;
        for role in self.member_roles(user) {
            let at = match role.position() {
                RankPos::Int(p) => p,
                RankPos::Undefined => 0,
            };
            max = Some(max.map_or(at, |m| m.max(at)));
        }
        RankPos::Int(max.unwrap_or(-1))
    }

    fn highest(&self, user: UserId) -> Option<&Role> {
        let mut best: Option<&Role> = None;
        for role in self.member_roles(user) {
            if best.is_none_or(|b| better(role, b)) {
                best = Some(role);
            }
        }
        best
    }

    fn outranks_role(&self, user: UserId, role: &Role) -> bool {
        match self.max_position(user).cmp(&role.position()) {
            Ordering::Greater => true,
            Ordering::Less => false,
            Ordering::Equal => self.highest(user).is_some_and(|h| h.id() < role.id()),
        }
    }

    pub fn can_manage_roles(&self, user: UserId, role: RoleId) -> Result<bool, PermsError> {
        if self.model.owner == Some(user) {
            return Ok(true);
        }
        let bits = self.permissions(user, Scope::Guild)?;
        if !bits.contains(Permissions::MANAGE_ROLES) {
            return Ok(false);
        }
        Ok(self
            .present_role(role)
            .is_some_and(|r| self.outranks_role(user, r)))
    }

    pub fn can_manage_role(&self, user: UserId, role: RoleId) -> bool {
        self.present_role(role)
            .is_some_and(|r| self.outranks_role(user, r))
    }

    fn rank(&self, user: UserId) -> (RankPos, i128) {
        match self.highest(user) {
            Some(role) => (
                role.position(),
                0i128.saturating_sub(i128::from(role.id().get())),
            ),
            None => (RankPos::Int(-1), 0),
        }
    }

    pub fn can_manage_member(&self, actor: UserId, target: UserId) -> bool {
        if self.model.owner == Some(actor) {
            return true;
        }
        if self.model.owner == Some(target) {
            return false;
        }
        self.rank(actor) > self.rank(target)
    }
}
