// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, Mutex};

use perm_core::budget::Units;
use perm_core::{
    Budget, ClassId, Delta, Hoist, Model, PermsError, PermsShared, RankPos, RoleIdx, RoleSetId,
    UserId, View, Visible, diff,
};

#[derive(Debug)]
pub struct Column {
    rows: u32,
}

impl Column {
    pub fn rows(&self) -> u32 {
        self.rows
    }
}

#[derive(Debug)]
struct State {
    last: Arc<Model>,
    roleset: BTreeMap<UserId, RoleSetId>,
    columns: BTreeMap<ClassId, Column>,
}

#[derive(Debug)]
pub struct MemberListShadow {
    perms: Arc<PermsShared>,
    state: Mutex<State>,
    budget: Budget,
}

pub fn row_bits() -> Visible {
    Visible::VIEW
}

fn contributes(model: &Model, class: ClassId, set: RoleSetId, user: UserId) -> bool {
    model.visible(class, set, user).contains(row_bits())
}

pub fn class_rows_from_sums(view: &View<'_>, class: ClassId) -> u32 {
    let model = view.model();
    let members = view.members();
    let Some(entry) = model.class(class) else {
        return 0;
    };
    let mut rows = 0u32;
    for (set, _) in model.rolesets().iter() {
        if entry.planes().get(set).contains(row_bits()) {
            rows = rows.saturating_add(members.refcount(set));
        }
    }
    for (user, visible) in entry.overrides() {
        let Some(set) = members.roleset(*user) else {
            continue;
        };
        let plane = entry.planes().get(set).contains(row_bits());
        let own = visible.contains(row_bits());
        if plane && !own {
            rows = rows.saturating_sub(1);
        } else if own && !plane {
            rows = rows.saturating_add(1);
        }
    }
    rows
}

pub fn top_hoist(model: &Model, set: RoleSetId) -> Option<RoleIdx> {
    let roles = model.roles();
    let guild = model.guild().get();
    model
        .rolesets()
        .get(set)?
        .roles()
        .iter()
        .filter_map(|idx| roles.get(*idx).map(|role| (*idx, role)))
        .filter(|(_, role)| role.is_present() && role.id().get() != guild)
        .filter_map(|(idx, role)| match role.hoist() {
            Hoist::NotHoisted => None,
            Hoist::Hoisted(Some(at)) => Some((idx, at, role.id().get())),
            Hoist::Hoisted(None) => match role.position() {
                RankPos::Int(at) => Some((idx, at, role.id().get())),
                RankPos::Undefined => Some((idx, 0, role.id().get())),
            },
        })
        .max_by_key(|(_, at, id)| (*at, core::cmp::Reverse(*id)))
        .map(|(idx, _, _)| idx)
}

impl MemberListShadow {
    pub fn new(perms: Arc<PermsShared>) -> Result<MemberListShadow, PermsError> {
        let last = perms.model()?;
        let budget = Budget::new(Units::new(perms.config().work_budget_units));
        Ok(MemberListShadow {
            perms,
            state: Mutex::new(State {
                last,
                roleset: BTreeMap::new(),
                columns: BTreeMap::new(),
            }),
            budget,
        })
    }

    pub fn budget(&self) -> Budget {
        self.budget
    }

    pub fn subscribe(&self, class: ClassId) -> Result<u32, PermsError> {
        let mut state = self.state.lock().map_err(|_| PermsError::Broken)?;
        let view = self.perms.view()?;
        let rows = recount(&view, class);
        state.columns.insert(class, Column { rows });
        Ok(rows)
    }

    pub fn column(&self, class: ClassId) -> Option<u32> {
        self.state
            .lock()
            .ok()
            .and_then(|s| s.columns.get(&class).map(Column::rows))
    }

    pub fn columns(&self) -> Vec<(ClassId, u32)> {
        self.state
            .lock()
            .map(|s| s.columns.iter().map(|(c, col)| (*c, col.rows)).collect())
            .unwrap_or_default()
    }

    pub fn apply(&self, touched: &[UserId]) -> Result<Delta, PermsError> {
        let mut state = self.state.lock().map_err(|_| PermsError::Broken)?;
        let view = self.perms.view()?;
        let current = view.model().clone();
        let delta = diff(&state.last, &current);
        let members = view.members();

        let mut affected: BTreeSet<UserId> = touched.iter().copied().collect();
        let flipped: BTreeSet<RoleSetId> = delta
            .classes
            .iter()
            .filter(|c| state.columns.contains_key(&c.class))
            .flat_map(|c| c.flipped.iter().map(|(set, _, _)| *set))
            .collect();
        let toggled = delta
            .classes
            .iter()
            .filter(|c| state.columns.contains_key(&c.class))
            .any(|c| c.toggled.intersects(row_bits()));
        if toggled || !flipped.is_empty() {
            affected.extend(
                state
                    .roleset
                    .iter()
                    .filter(|(_, set)| toggled || flipped.contains(set))
                    .map(|(user, _)| *user),
            );
        }
        for change in &delta.classes {
            if state.columns.contains_key(&change.class) {
                affected.extend(change.overrides.iter().map(|(user, _, _)| *user));
            }
        }

        let State {
            last,
            roleset,
            columns,
        } = &mut *state;
        for id in &delta.freed {
            columns.remove(id);
        }
        for (class, column) in columns.iter_mut() {
            if delta.created.contains(class) {
                column.rows = recount(&view, *class);
                continue;
            }
            for user in &affected {
                if let Some(set) = roleset.get(user)
                    && contributes(last, *class, *set, *user)
                {
                    column.rows = column.rows.saturating_sub(1);
                }
                if let Some(set) = members.roleset(*user)
                    && contributes(&current, *class, set, *user)
                {
                    column.rows = column.rows.saturating_add(1);
                }
            }
        }
        for user in &affected {
            match members.roleset(*user) {
                Some(set) => {
                    roleset.insert(*user, set);
                }
                None => {
                    roleset.remove(user);
                }
            }
        }
        *last = current;
        Ok(delta)
    }

    pub fn adopt_all(&self) -> Result<(), PermsError> {
        let mut state = self.state.lock().map_err(|_| PermsError::Broken)?;
        let view = self.perms.view()?;
        state.roleset = view
            .members()
            .iter()
            .map(|e| (e.user(), e.roleset()))
            .collect();
        state.last = view.model().clone();
        Ok(())
    }

    pub fn is_exception(&self, user: UserId) -> Result<bool, PermsError> {
        Ok(self.perms.model()?.is_exception(user))
    }
}

pub fn recount(view: &View<'_>, class: ClassId) -> u32 {
    let model = view.model();
    let mut rows = 0u32;
    for entry in view.members().iter() {
        if contributes(model, class, entry.roleset(), entry.user()) {
            rows = rows.saturating_add(1);
        }
    }
    rows
}
