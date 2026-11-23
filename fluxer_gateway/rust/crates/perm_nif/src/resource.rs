// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::{Arc, OnceLock};

use perm_core::{Budget, Delta, Model, PermsShared, Retired, Units, diff, diff_units};
use rustler::{Env, LocalPid, Monitor, NifResult, Resource, ResourceArc, Term};

const DROP_LATER_MEMBERS: usize = 10_000;

pub struct PermsResource {
    pub(crate) shared: Arc<PermsShared>,
}

pub struct ReaderResource {
    pub(crate) shared: Arc<PermsShared>,
}

pub struct ModelResource {
    pub(crate) shared: Arc<PermsShared>,
    pub(crate) model: Arc<Model>,
}

pub struct DeltaResource {
    before: Arc<Model>,
    after: Arc<Model>,
    delta: OnceLock<Delta>,
}

impl Resource for PermsResource {
    const IMPLEMENTS_DOWN: bool = true;

    fn down<'a>(&'a self, _env: Env<'a>, _pid: LocalPid, _monitor: Monitor) {
        release_now(&self.shared);
    }
}

impl Resource for ReaderResource {}

impl Resource for ModelResource {}

impl Resource for DeltaResource {}

impl PermsResource {
    pub fn shared(&self) -> &Arc<PermsShared> {
        &self.shared
    }
}

impl ReaderResource {
    pub fn shared(&self) -> &Arc<PermsShared> {
        &self.shared
    }
}

impl ModelResource {
    pub fn shared(&self) -> &Arc<PermsShared> {
        &self.shared
    }

    pub fn model(&self) -> &Arc<Model> {
        &self.model
    }
}

impl DeltaResource {
    pub fn new(before: Arc<Model>, after: Arc<Model>) -> DeltaResource {
        DeltaResource {
            before,
            after,
            delta: OnceLock::new(),
        }
    }

    pub fn delta(&self) -> &Delta {
        self.delta.get_or_init(|| diff(&self.before, &self.after))
    }

    pub fn computed(&self) -> Option<&Delta> {
        self.delta.get()
    }

    pub fn within(&self, budget: Budget) -> Option<(&Delta, Units)> {
        if let Some(delta) = self.delta.get() {
            return Some((delta, Units::ZERO));
        }
        let cost = diff_units(&self.before, &self.after);
        budget.fits(cost).then(|| (self.delta(), cost))
    }
}

pub(crate) fn release(shared: &PermsShared) {
    if let Some(retired) = shared.release() {
        retire(retired);
    }
}

pub(crate) fn release_now(shared: &Arc<PermsShared>) {
    match shared.try_release() {
        Ok(Some(retired)) => retire(retired),
        Ok(None) => {}
        Err(_) => guild_rt::drop_later(
            Releasing(Arc::clone(shared)),
            guild_rt::ResourceKind::PermIndex,
        ),
    }
}

fn retire(retired: Retired) {
    if retired.members() > DROP_LATER_MEMBERS {
        guild_rt::drop_later(retired, guild_rt::ResourceKind::PermIndex);
    }
}

struct Releasing(Arc<PermsShared>);

impl Drop for Releasing {
    fn drop(&mut self) {
        release(&self.0);
    }
}

pub fn link(env: Env<'_>) -> bool {
    env.register::<PermsResource>().is_ok()
        && env.register::<ReaderResource>().is_ok()
        && env.register::<ModelResource>().is_ok()
        && env.register::<DeltaResource>().is_ok()
}

pub fn shared_from_term(term: Term<'_>) -> NifResult<Arc<PermsShared>> {
    match term.decode::<ResourceArc<PermsResource>>() {
        Ok(perms) => Ok(perms.shared().clone()),
        Err(_) => term
            .decode::<ResourceArc<ReaderResource>>()
            .map(|reader| reader.shared().clone()),
    }
}

pub fn delta_from_term(term: Term<'_>) -> NifResult<ResourceArc<DeltaResource>> {
    term.decode::<ResourceArc<DeltaResource>>()
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::arithmetic_side_effects)]
mod tests {
    use perm_core::{
        Budget, ChannelId, ChannelKind, ChannelSpec, Config, Event, GuildId, Hoist, Nsfw,
        OverwriteSpec, OverwriteTarget, Permissions, PermsShared, RankPos, RoleId, RoleList,
        RoleSpec, Snapshot, Units, UserId, diff, diff_units,
    };

    use super::DeltaResource;

    const GUILD: u64 = 100;

    fn role(id: u64, position: i64) -> RoleSpec {
        RoleSpec {
            id: RoleId::new(id),
            permissions: if id == GUILD {
                Permissions::VIEW_CHANNEL
            } else {
                Permissions::empty()
            },
            position: RankPos::Int(position),
            hoist: Hoist::NotHoisted,
        }
    }

    fn channel(id: u64, roles: &[u64]) -> ChannelSpec {
        ChannelSpec {
            id: ChannelId::new(id),
            kind: ChannelKind::Text,
            parent: None,
            overwrites: roles
                .iter()
                .map(|role| OverwriteSpec {
                    target: OverwriteTarget::Role(RoleId::new(*role)),
                    allow: Permissions::empty(),
                    deny: Permissions::VIEW_CHANNEL,
                })
                .collect(),
            nsfw: Nsfw::Inherit,
            named: true,
        }
    }

    fn role_churn(roles: u64, channels: u64, members: u64) -> DeltaResource {
        let guild = GuildId::new(GUILD);
        let mut snapshot = Snapshot::new(guild, None);
        snapshot.roles = (0..=roles)
            .map(|i| {
                role(
                    if i == 0 { GUILD } else { 200 + i },
                    i64::try_from(i).unwrap_or(0),
                )
            })
            .collect();
        snapshot.channels = (0..channels)
            .map(|i| channel(1_000 + i, &[201 + i % roles.max(1)]))
            .collect();
        snapshot.members = (0..members)
            .map(|i| {
                let held = (0..roles)
                    .filter(|bit| (i >> bit) & 1 == 1)
                    .map(|bit| RoleId::new(201 + bit))
                    .collect();
                (UserId::new(10 + i), RoleList::Roles(held))
            })
            .collect();
        let shared = PermsShared::new(guild, Config::default());
        shared.load_dirty(&snapshot).unwrap();
        let effects = shared
            .apply_dirty(&[Event::MemberUpsert(
                UserId::new(10),
                RoleList::Roles((201..=200 + roles).map(RoleId::new).collect()),
            )])
            .unwrap();
        DeltaResource::new(effects.before().clone(), effects.after().clone())
    }

    #[test]
    fn a_small_delta_is_diffed_on_the_normal_scheduler() {
        let resource = role_churn(3, 4, 6);
        let cost = diff_units(&resource.before, &resource.after);
        assert!(cost.get() > 0);
        assert!(Budget::default().fits(cost));
        let (delta, charged) = resource.within(Budget::default()).unwrap();
        assert_eq!(charged, cost);
        assert_eq!(*delta, diff(&resource.before, &resource.after));
        let (again, recharged) = resource.within(Budget::new(Units::ZERO)).unwrap();
        assert_eq!(again, delta);
        assert_eq!(recharged, Units::ZERO);
    }

    #[test]
    fn a_delta_over_budget_is_left_for_a_dirty_scheduler() {
        let resource = role_churn(3, 4, 6);
        let cost = diff_units(&resource.before, &resource.after);
        assert!(
            resource
                .within(Budget::new(Units::new(cost.get() - 1)))
                .is_none()
        );
        assert!(resource.computed().is_none());
        assert!(resource.within(Budget::new(cost)).is_some());
        assert!(resource.computed().is_some());
    }

    #[test]
    fn the_estimate_grows_with_the_guild() {
        let small = role_churn(3, 4, 6);
        let large = role_churn(12, 400, 3_000);
        let small_cost = diff_units(&small.before, &small.after);
        let large_cost = diff_units(&large.before, &large.after);
        assert!(large_cost > small_cost.times(20));
    }
}
