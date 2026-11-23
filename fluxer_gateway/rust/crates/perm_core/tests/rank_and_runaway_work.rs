// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use std::sync::Arc;

use perm_core::reference::Reference;
use perm_core::{
    ChannelId, ChannelKind, ChannelSpec, Config, Event, Hoist, Nsfw, Permissions, PermsError,
    PermsShared, RankPos, RoleId, RoleList, RoleSpec, Snapshot, Status, UserId,
};
use support::{GUILD, guild};

fn role(id: u64, permissions: Permissions, position: i64) -> RoleSpec {
    RoleSpec {
        id: RoleId::new(id),
        permissions,
        position: RankPos::Int(position),
        hoist: Hoist::NotHoisted,
    }
}

fn roles(ids: &[u64]) -> RoleList {
    RoleList::Roles(ids.iter().map(|id| RoleId::new(*id)).collect())
}

fn rank_snapshot() -> Snapshot {
    let mut s = Snapshot::new(guild(), None);
    s.roles = vec![
        role(
            GUILD,
            Permissions::VIEW_CHANNEL | Permissions::MANAGE_ROLES,
            0,
        ),
        role(50, Permissions::empty(), 0),
        role(60, Permissions::empty(), 3),
        role(70, Permissions::empty(), 5),
    ];
    s.channels = vec![ChannelSpec {
        id: ChannelId::new(1),
        kind: ChannelKind::Text,
        parent: None,
        overwrites: vec![],
        nsfw: Nsfw::Inherit,
        named: true,
    }];
    s.members = vec![
        (UserId::new(1), roles(&[5])),
        (UserId::new(2), roles(&[])),
        (UserId::new(3), roles(&[70])),
        (UserId::new(4), roles(&[60])),
        (UserId::new(5), roles(&[5, 60])),
        (UserId::new(6), roles(&[50])),
        (UserId::new(7), roles(&[70, 50])),
    ];
    s
}

#[test]
fn ghost_roles_do_not_count_for_rank() {
    let snapshot = rank_snapshot();
    let shared = PermsShared::new(guild(), Config::default());
    shared.load(&snapshot).unwrap();
    let mut reference = Reference::new(&snapshot);
    let without_70 = Event::Roles(snapshot.roles[..3].to_vec());
    reference.apply(&without_70);
    shared.apply(&[without_70]).unwrap();
    let view = shared.view().unwrap();
    let model = view.model();
    for id in [5u64, 70] {
        let idx = model.roles().resolve(RoleId::new(id)).unwrap();
        let ghost = model.roles().get(idx).unwrap();
        assert!(!ghost.is_present());
        assert_eq!(ghost.position(), RankPos::Int(0));
    }
    let users: Vec<UserId> = (1..=8).map(UserId::new).collect();
    let ids = [GUILD, 5, 50, 60, 70, 77_777];
    for user in &users {
        for id in ids.map(RoleId::new) {
            assert_eq!(
                view.can_manage_role(*user, id),
                reference.can_manage_role(*user, id),
                "can_manage_role {user:?} {id:?}"
            );
            assert_eq!(
                view.can_manage_roles(*user, id).ok(),
                reference.can_manage_roles(*user, id).ok(),
                "can_manage_roles {user:?} {id:?}"
            );
        }
        for target in &users {
            assert_eq!(
                view.can_manage_member(*user, *target),
                reference.can_manage_member(*user, *target),
                "can_manage_member {user:?} {target:?}"
            );
        }
    }
    let (u1, u2, u3, u5, u6) = (
        UserId::new(1),
        UserId::new(2),
        UserId::new(3),
        UserId::new(5),
        UserId::new(6),
    );
    assert!(!view.can_manage_role(u1, RoleId::new(50)));
    assert_eq!(view.can_manage_roles(u1, RoleId::new(50)), Ok(false));
    assert!(!view.can_manage_member(u1, u2));
    assert!(!view.can_manage_role(u3, RoleId::new(60)));
    assert!(!view.can_manage_member(u3, u6));
    assert!(!view.can_manage_role(u5, RoleId::new(5)));
    assert!(view.can_manage_role(u5, RoleId::new(50)));
    assert!(view.can_manage_member(u5, u1));
}

fn runaway_snapshot() -> Snapshot {
    let mut s = rank_snapshot();
    s.roles.push(role(80, Permissions::SEND_MESSAGES, 6));
    s.roles.push(role(90, Permissions::CONNECT, 7));
    s
}

fn small_budget() -> Config {
    Config {
        work_budget_units: 1,
        dead_role_set_floor: 2,
        ..Config::default()
    }
}

fn loaded() -> Arc<PermsShared> {
    let shared = PermsShared::new(guild(), small_budget());
    assert_eq!(
        shared.load(&runaway_snapshot()),
        Err(PermsError::NeedsDirty)
    );
    shared.load_dirty(&runaway_snapshot()).unwrap();
    shared
}

fn churned() -> Arc<PermsShared> {
    let shared = loaded();
    for list in [
        &[80u64][..],
        &[90],
        &[80, 90],
        &[50, 80],
        &[50, 90],
        &[60, 80],
        &[60, 90],
        &[50, 60, 80],
        &[50, 60, 90],
        &[50, 60],
        &[50, 80, 90],
    ] {
        shared
            .apply_dirty(&[Event::MemberUpsert(UserId::new(9), roles(list))])
            .unwrap();
    }
    assert!(shared.compaction_due());
    shared
}

fn assert_broken(shared: &PermsShared, epoch: u64) {
    assert_eq!(shared.status(), Status::Broken);
    assert_eq!(shared.epoch().get(), epoch);
    assert_eq!(shared.view().err(), Some(PermsError::Broken));
}

#[test]
fn apply_dirty_breaks_past_ten_times_its_estimate() {
    let events = [Event::MemberUpsert(UserId::new(9), roles(&[60, 80, 90]))];
    let twin = loaded();
    let estimate = twin.apply_dirty(&events).unwrap().estimate().get();
    assert!(estimate > small_budget().work_budget_units);

    let below = loaded();
    let epoch = below.epoch().get();
    below.fault_extra_units(estimate * 9);
    let effects = below.apply_dirty(&events).unwrap();
    assert_eq!(below.status(), Status::Ready);
    assert_eq!(effects.epoch().get(), epoch + 1);
    assert!(effects.spent().get() > estimate * 9);
    assert!(effects.spent().get() <= estimate * 10);
    assert!(below.view().unwrap().roleset(UserId::new(9)).is_some());

    let above = loaded();
    let epoch = above.epoch().get();
    let model = above.model().unwrap();
    above.fault_extra_units(estimate * 10 + 1);
    assert_eq!(above.apply_dirty(&events).err(), Some(PermsError::Broken));
    assert_broken(&above, epoch);
    above.fault_extra_units(0);
    assert_eq!(above.apply_dirty(&events).err(), Some(PermsError::Broken));
    assert_eq!(above.epoch(), model.epoch());
}

#[test]
fn load_dirty_breaks_past_ten_times_its_estimate() {
    let snapshot = runaway_snapshot();
    let estimate = PermsShared::load_estimate(&snapshot).get();
    assert!(estimate > small_budget().work_budget_units);

    let below = PermsShared::new(guild(), small_budget());
    below.fault_extra_units(estimate * 9);
    assert_eq!(below.load_dirty(&snapshot).unwrap().get(), 1);
    assert_eq!(below.status(), Status::Ready);
    assert_eq!(below.stats().members, 7);

    let above = PermsShared::new(guild(), small_budget());
    above.fault_extra_units(estimate * 10 + 1);
    assert_eq!(above.load_dirty(&snapshot).err(), Some(PermsError::Broken));
    assert_broken(&above, 0);
}

#[test]
fn compact_dirty_breaks_past_ten_times_its_estimate() {
    let twin = churned();
    let estimate = twin.compaction_estimate().unwrap().get();
    assert!(estimate > small_budget().work_budget_units);
    assert_eq!(twin.compact().err(), Some(PermsError::NeedsDirty));

    let below = churned();
    let epoch = below.epoch().get();
    assert_eq!(below.compaction_estimate().unwrap().get(), estimate);
    below.fault_extra_units(estimate * 9);
    let done = below.compact_dirty().unwrap().unwrap();
    assert_eq!(below.status(), Status::Ready);
    assert_eq!(done.epoch().get(), epoch + 1);
    assert!(done.roleset_remap().is_some());
    assert!(done.spent().get() > estimate * 9);
    assert!(done.spent().get() <= estimate * 10);
    assert!(!below.compaction_due());

    let above = churned();
    let epoch = above.epoch().get();
    above.fault_extra_units(estimate * 10 + 1);
    assert_eq!(above.compact_dirty().err(), Some(PermsError::Broken));
    assert_broken(&above, epoch);
}
