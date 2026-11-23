// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use std::sync::Arc;

use perm_core::{
    Base, ChannelCause, ChannelId, ChannelKind, ChannelSpec, Config, Effects, Event, Hoist, Model,
    Nsfw, OverwriteSpec, OverwriteTarget, Permissions, PermsError, PermsShared, RankPos, RoleId,
    RoleList, RoleSetId, RoleSpec, SigId, Snapshot, UserId, Visible,
};
use support::{GUILD, guild};

const CATEGORIES: u64 = 10;
const TEXT: u64 = 190;
const SLACK: u64 = 2;

fn everyone_perms() -> Permissions {
    Permissions::VIEW_CHANNEL | Permissions::READ_MESSAGE_HISTORY | Permissions::SEND_MESSAGES
}

fn role_specs(roles: u64) -> Vec<RoleSpec> {
    let mut specs = vec![RoleSpec {
        id: RoleId::new(GUILD),
        permissions: everyone_perms(),
        position: RankPos::Int(0),
        hoist: Hoist::NotHoisted,
    }];
    for r in 1..=roles {
        specs.push(RoleSpec {
            id: RoleId::new(r),
            permissions: Permissions::SEND_MESSAGES,
            position: RankPos::Int(i64::try_from(r).unwrap()),
            hoist: Hoist::NotHoisted,
        });
    }
    specs
}

fn category(i: u64) -> ChannelSpec {
    ChannelSpec {
        id: ChannelId::new(2000 + i),
        kind: ChannelKind::Category,
        parent: None,
        overwrites: vec![],
        nsfw: Nsfw::Inherit,
        named: true,
    }
}

fn private_to(role: u64) -> Vec<OverwriteSpec> {
    vec![
        OverwriteSpec {
            target: OverwriteTarget::Role(RoleId::new(GUILD)),
            allow: Permissions::empty(),
            deny: Permissions::VIEW_CHANNEL,
        },
        OverwriteSpec {
            target: OverwriteTarget::Role(RoleId::new(role)),
            allow: Permissions::VIEW_CHANNEL,
            deny: Permissions::empty(),
        },
    ]
}

fn text(roles: u64, i: u64) -> ChannelSpec {
    ChannelSpec {
        id: ChannelId::new(3000 + i),
        kind: ChannelKind::Text,
        parent: Some(ChannelId::new(2000 + i % CATEGORIES)),
        overwrites: if i.is_multiple_of(2) {
            private_to(i % roles + 1)
        } else {
            vec![]
        },
        nsfw: Nsfw::Inherit,
        named: true,
    }
}

struct Fixture {
    roles: u64,
    members: u64,
}

impl Fixture {
    const LARGE: Fixture = Fixture {
        roles: 50,
        members: 1_225,
    };
    const SMALL: Fixture = Fixture {
        roles: 10,
        members: 45,
    };

    fn pairs(&self) -> Vec<(u64, u64)> {
        let mut out = Vec::new();
        for a in 1..=self.roles {
            for b in a + 1..=self.roles {
                out.push((a, b));
            }
        }
        out
    }

    fn snapshot(&self) -> Snapshot {
        let mut s = Snapshot::new(guild(), Some(UserId::new(1)));
        s.roles = role_specs(self.roles);
        s.channels = (0..CATEGORIES).map(category).collect();
        s.channels.extend((0..TEXT).map(|i| text(self.roles, i)));
        let pairs = self.pairs();
        for n in 0..self.members {
            let (a, b) = pairs[usize::try_from(n).unwrap() % pairs.len()];
            s.members.push((
                UserId::new(n + 1),
                RoleList::Roles(vec![RoleId::new(a), RoleId::new(b)]),
            ));
        }
        s
    }

    fn loaded(&self) -> Arc<PermsShared> {
        let shared = PermsShared::new(guild(), Config::default());
        let snapshot = self.snapshot();
        match shared.load(&snapshot) {
            Ok(_) => {}
            Err(PermsError::NeedsDirty) => {
                shared.load_dirty(&snapshot).unwrap();
            }
            Err(e) => panic!("{e:?}"),
        }
        shared
    }
}

struct Sizes {
    sets: u64,
    words: u64,
    sigs: u64,
    channels: u64,
    kids: u64,
    exceptions: u64,
    roles: u64,
}

fn n(x: usize) -> u64 {
    u64::try_from(x).unwrap()
}

fn sizes(model: &Model) -> Sizes {
    Sizes {
        sets: n(model.rolesets().len()),
        words: n(model.rolesets().words()),
        sigs: n(model.signatures().len()),
        channels: n(model.channels().len()),
        kids: model
            .channels()
            .live()
            .map(|(idx, _)| n(model.channels().children(idx).len()))
            .sum(),
        exceptions: n(model.exceptions().len()),
        roles: n(model.roles().len()),
    }
}

impl Sizes {
    fn effects(&self, regrouped: u64) -> u64 {
        regrouped * (4 * self.channels + 8)
    }

    fn planes_all(&self) -> u64 {
        (self.channels + self.kids) * self.words
    }

    fn slot_scan(&self) -> u64 {
        self.channels * self.words
    }

    fn planes_of(&self, model: &Model, channels: &[u64]) -> u64 {
        channels
            .iter()
            .map(|id| {
                let idx = model.channel_idx(ChannelId::new(*id)).unwrap();
                (1 + n(model.channels().children(idx).len())) * self.words + 4
            })
            .sum()
    }
}

fn normal(shared: &PermsShared, events: &[Event]) -> Effects {
    let effects = shared.apply(events).unwrap();
    assert!(
        effects.spent() <= effects.estimate(),
        "spent {:?} above estimate {:?}",
        effects.spent(),
        effects.estimate()
    );
    effects
}

fn within(effects: &Effects, row: u64) {
    let spent = effects.spent().get();
    assert!(
        spent <= SLACK * row,
        "spent {spent} above {SLACK} x the row bound {row}"
    );
}

fn sig_ids(model: &Model) -> Vec<(ChannelId, Option<SigId>)> {
    model
        .channels()
        .live()
        .map(|(idx, ch)| (ch.id(), model.signatures().of_channel(idx)))
        .collect()
}

fn class_id(model: &Model, id: u64) -> Option<perm_core::ClassId> {
    model.class_of_id(ChannelId::new(id))
}

fn bits(model: &Model, channel: ChannelId, set: RoleSetId) -> Visible {
    model
        .class(model.class_of_id(channel).unwrap())
        .unwrap()
        .planes()
        .get(set)
}

fn channel_ids(model: &Model) -> Vec<ChannelId> {
    model.channels().live().map(|(_, ch)| ch.id()).collect()
}

fn overrides_without(model: &Model, channel: ChannelId, users: &[u64]) -> Vec<(UserId, Visible)> {
    model
        .class(model.class_of_id(channel).unwrap())
        .unwrap()
        .overrides()
        .iter()
        .filter(|(u, _)| !users.contains(&u.get()))
        .copied()
        .collect()
}

fn same_parents(a: &Model, b: &Model) -> bool {
    channel_ids(a) == channel_ids(b)
        && channel_ids(a).iter().all(|id| {
            let pa = a.channel(a.channel_idx(*id).unwrap()).unwrap().parent_id();
            let pb = b.channel(b.channel_idx(*id).unwrap()).unwrap().parent_id();
            pa == pb
        })
}

fn upsert(user: u64, roles: &[u64]) -> Event {
    Event::MemberUpsert(
        UserId::new(user),
        RoleList::Roles(roles.iter().map(|r| RoleId::new(*r)).collect()),
    )
}

#[test]
fn member_upsert_with_an_existing_role_set_writes_only_the_index() {
    let shared = Fixture::LARGE.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    let effects = normal(&shared, &[upsert(90_001, &[1, 2])]);
    assert!(Arc::ptr_eq(effects.before(), effects.after()));
    assert_eq!(shared.epoch(), before.epoch());
    assert_eq!(effects.regroups().len(), 1);
    within(&effects, 40 + 8 * 2 + s.effects(1));
}

#[test]
fn member_upsert_with_a_new_role_set_evaluates_that_set_only() {
    let shared = Fixture::LARGE.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    assert_eq!(s.sets, 1_226);
    let effects = normal(&shared, &[upsert(90_002, &[1, 2, 3])]);
    let after = effects.after();
    assert_eq!(after.rolesets().len(), before.rolesets().len() + 1);
    assert_eq!(sig_ids(&before), sig_ids(after));
    for id in channel_ids(&before) {
        for (set, _) in before.rolesets().iter() {
            assert_eq!(bits(&before, id, set), bits(after, id, set));
        }
    }
    let new_set = RoleSetId::from_index(1_226).unwrap();
    assert_eq!(after.rolesets().get(new_set).unwrap().roles().len(), 3);
    assert!(
        channel_ids(after)
            .iter()
            .any(|id| bits(after, *id, new_set).contains(Visible::VIEW))
    );
    within(
        &effects,
        40 + 8 * 3 + s.sigs * 3 * 4 + s.planes_all() + s.slot_scan() + s.effects(1),
    );
}

fn holding(model: &Model, role: u64) -> Vec<RoleSetId> {
    let idx = model.roles().resolve(RoleId::new(role)).unwrap();
    model
        .rolesets()
        .iter()
        .filter(|(_, set)| set.contains(idx))
        .map(|(id, _)| id)
        .collect()
}

#[test]
fn role_edit_rebases_only_role_sets_holding_the_role() {
    let shared = Fixture::LARGE.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    let held = holding(&before, 7);
    assert_eq!(n(held.len()), 49);
    let mut specs = role_specs(50);
    specs[7].permissions = Permissions::ADMINISTRATOR;
    let effects = normal(&shared, &[Event::Roles(specs)]);
    let after = effects.after();
    assert_eq!(sig_ids(&before), sig_ids(after));
    for (set, roles) in before.rolesets().iter() {
        if held.contains(&set) {
            assert_ne!(roles.base(), Base::All);
            assert_eq!(after.rolesets().base(set), Base::All);
            continue;
        }
        assert_eq!(before.rolesets().base(set), after.rolesets().base(set));
        for id in channel_ids(&before) {
            assert_eq!(bits(&before, id, set), bits(after, id, set));
        }
    }
    let rebased = n(held.len());
    within(
        &effects,
        rebased * s.sigs * 8
            + s.planes_all()
            + s.exceptions * s.channels
            + s.slot_scan()
            + rebased * s.channels,
    );
}

#[test]
fn everyone_edit_rebases_every_role_set() {
    let large = Fixture::LARGE.loaded();
    let mut specs = role_specs(50);
    specs[0].permissions = Permissions::VIEW_CHANNEL;
    let s = sizes(&large.model().unwrap());
    assert!(s.sets * s.sigs > 30_000);
    assert_eq!(
        large.apply(&[Event::Roles(specs.clone())]).unwrap_err(),
        PermsError::NeedsDirty
    );
    let dirty = large.apply_dirty(&[Event::Roles(specs)]).unwrap();
    assert!(dirty.spent() <= dirty.estimate());

    let shared = Fixture::SMALL.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    assert!(s.sets * s.sigs <= 30_000);
    let mut specs = role_specs(10);
    specs[0].permissions = Permissions::VIEW_CHANNEL;
    let effects = normal(&shared, &[Event::Roles(specs)]);
    let after = effects.after();
    assert_eq!(sig_ids(&before), sig_ids(after));
    for (set, _) in before.rolesets().iter() {
        assert!(
            !after
                .rolesets()
                .base(set)
                .permissions()
                .contains(Permissions::READ_MESSAGE_HISTORY)
        );
    }
    let public = ChannelId::new(3001);
    for (set, _) in before.rolesets().iter() {
        assert!(bits(&before, public, set).contains(Visible::HISTORY));
        assert!(!bits(after, public, set).contains(Visible::HISTORY));
    }
    within(
        &effects,
        s.sets * s.sigs * 8
            + s.planes_all()
            + s.exceptions * s.channels
            + s.slot_scan()
            + s.sets * s.channels,
    );
}

#[test]
fn hoist_or_position_edit_touches_the_role_table_only() {
    let shared = Fixture::LARGE.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    let mut specs = role_specs(50);
    specs[9].position = RankPos::Int(500);
    specs[9].hoist = Hoist::Hoisted(Some(3));
    let effects = normal(&shared, &[Event::Roles(specs)]);
    let after = effects.after();
    assert_eq!(after.epoch(), before.epoch().next());
    assert!(std::ptr::eq(before.signatures(), after.signatures()));
    assert!(std::ptr::eq(before.classes(), after.classes()));
    assert!(std::ptr::eq(before.rolesets(), after.rolesets()));
    let idx = after.roles().resolve(RoleId::new(9)).unwrap();
    assert_eq!(
        after.roles().get(idx).unwrap().position(),
        RankPos::Int(500)
    );
    assert!(effects.changes().is_empty() && effects.regroups().is_empty());
    within(&effects, 8 * s.roles + s.exceptions * (4 * s.channels + 8));
}

fn unchanged_except(before: &Model, after: &Model, touched: &[u64]) {
    for id in channel_ids(before) {
        if touched.contains(&id.get()) || after.channel_idx(id).is_none() {
            continue;
        }
        let a = before
            .signatures()
            .of_channel(before.channel_idx(id).unwrap());
        let b = after
            .signatures()
            .of_channel(after.channel_idx(id).unwrap());
        assert_eq!(a, b, "signature of {id:?}");
        assert_eq!(
            class_id(before, id.get()),
            class_id(after, id.get()),
            "class of {id:?}"
        );
    }
}

#[test]
fn overwrite_edit_reevaluates_the_channel_and_its_category() {
    let shared = Fixture::LARGE.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    let mut spec = text(50, 4);
    spec.overwrites.push(OverwriteSpec {
        target: OverwriteTarget::Role(RoleId::new(33)),
        allow: Permissions::VIEW_CHANNEL,
        deny: Permissions::empty(),
    });
    let effects = normal(
        &shared,
        &[Event::ChannelUpsert(spec, ChannelCause::Updated)],
    );
    let after = effects.after();
    assert_eq!(after.signatures().len(), before.signatures().len() + 1);
    unchanged_except(&before, after, &[3004, 2004]);
    assert_ne!(class_id(&before, 3004), class_id(after, 3004));
    within(
        &effects,
        s.sets * 8
            + s.words * 4
            + s.exceptions
            + s.planes_of(after, &[3004, 2004])
            + s.slot_scan()
            + s.sets * 3,
    );
}

#[test]
fn overwrite_edit_at_1225_role_sets_runs_on_the_normal_scheduler() {
    let mut snapshot = Fixture::LARGE.snapshot();
    let pairs = Fixture::LARGE.pairs();
    for u in 1_226..=50_000u64 {
        let (a, b) = pairs[usize::try_from(u).unwrap() % pairs.len()];
        snapshot.members.push((
            UserId::new(u),
            RoleList::Roles(vec![RoleId::new(a), RoleId::new(b)]),
        ));
    }
    let shared = PermsShared::new(guild(), Config::default());
    assert_eq!(shared.load(&snapshot), Err(PermsError::NeedsDirty));
    shared.load_dirty(&snapshot).unwrap();
    let model = shared.model().unwrap();
    assert_eq!(model.rolesets().len(), 1_226);
    let mut spec = text(50, 10);
    spec.overwrites.push(OverwriteSpec {
        target: OverwriteTarget::Role(RoleId::new(21)),
        allow: Permissions::VIEW_CHANNEL,
        deny: Permissions::empty(),
    });
    let effects = shared
        .apply(&[Event::ChannelUpsert(spec, ChannelCause::Updated)])
        .unwrap();
    assert!(effects.spent() <= effects.estimate());
    assert!(effects.estimate().get() <= Config::default().work_budget_units);
}

#[test]
fn channel_create_delete_and_reparent_touch_the_channel_and_both_parents() {
    let shared = Fixture::LARGE.loaded();

    let before = shared.model().unwrap();
    let s = sizes(&before);
    let mut created = text(50, 400);
    created.overwrites = private_to(17);
    created.parent = Some(ChannelId::new(2003));
    let effects = normal(
        &shared,
        &[Event::ChannelUpsert(created, ChannelCause::Created)],
    );
    let after = effects.after();
    unchanged_except(&before, after, &[3400, 2003]);
    within(
        &effects,
        s.sets * 8
            + s.words * 4
            + s.exceptions
            + s.planes_of(after, &[3400, 2003])
            + s.slot_scan()
            + s.sets * 3,
    );

    let before = shared.model().unwrap();
    let s = sizes(&before);
    let effects = normal(&shared, &[Event::ChannelDelete(ChannelId::new(3006))]);
    let after = effects.after();
    assert!(after.channel_idx(ChannelId::new(3006)).is_none());
    unchanged_except(&before, after, &[3006, 2006]);
    within(
        &effects,
        s.words * 4 + s.exceptions + s.planes_of(after, &[2006]) + s.slot_scan() + s.sets * 3,
    );

    let before = shared.model().unwrap();
    let s = sizes(&before);
    let mut moved = text(50, 8);
    moved.parent = Some(ChannelId::new(2009));
    let effects = normal(
        &shared,
        &[Event::ChannelUpsert(moved, ChannelCause::Updated)],
    );
    let after = effects.after();
    unchanged_except(&before, after, &[3008, 2008, 2009]);
    within(
        &effects,
        s.sets * 8
            + s.words * 4
            + s.exceptions
            + s.planes_of(after, &[3008, 2008, 2009])
            + s.slot_scan()
            + s.sets * 4,
    );
}

fn overrides_only(before: &Model, after: &Model, users: &[u64]) {
    assert_eq!(sig_ids(before), sig_ids(after));
    assert!(same_parents(before, after));
    for id in channel_ids(before) {
        for (set, _) in before.rolesets().iter() {
            assert_eq!(bits(before, id, set), bits(after, id, set));
        }
        assert_eq!(
            overrides_without(before, id, users),
            overrides_without(after, id, users),
            "{id:?}"
        );
    }
}

#[test]
fn owner_change_rewrites_the_overrides_of_both_owners() {
    let shared = Fixture::LARGE.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    let effects = normal(&shared, &[Event::Owner(Some(UserId::new(2)))]);
    overrides_only(&before, effects.after(), &[1, 2]);
    within(&effects, 2 * s.channels * 8 + s.effects(2));
}

#[test]
fn member_target_change_rewrites_that_users_overrides() {
    let shared = Fixture::LARGE.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    let mut spec = text(50, 10);
    spec.overwrites.push(OverwriteSpec {
        target: OverwriteTarget::Member(UserId::new(5)),
        allow: Permissions::VIEW_CHANNEL,
        deny: Permissions::empty(),
    });
    let effects = normal(
        &shared,
        &[Event::ChannelUpsert(spec, ChannelCause::Updated)],
    );
    let after = effects.after();
    unchanged_except(&before, after, &[3010, 2000]);
    assert!(after.is_exception(UserId::new(5)));
    for id in channel_ids(&before) {
        if id.get() != 3010 {
            assert_eq!(
                overrides_without(&before, id, &[5]),
                overrides_without(after, id, &[5])
            );
        }
    }
    within(
        &effects,
        s.channels * 8 + s.planes_of(after, &[3010, 2000]) + s.slot_scan() + s.effects(1),
    );
}

#[test]
fn virtual_access_change_rewrites_that_users_overrides() {
    let shared = Fixture::LARGE.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    let effects = normal(
        &shared,
        &[Event::VirtualAccess(
            UserId::new(90_009),
            vec![ChannelId::new(3002)],
        )],
    );
    let after = effects.after();
    overrides_only(&before, after, &[90_009]);
    assert!(after.is_exception(UserId::new(90_009)));
    within(&effects, s.channels * 8 + s.effects(1));
}

#[test]
fn channel_order_changes_the_list_order_only() {
    let shared = Fixture::LARGE.loaded();
    let before = shared.model().unwrap();
    let s = sizes(&before);
    let mut order: Vec<ChannelId> = before
        .channels()
        .order()
        .iter()
        .map(|i| before.channel(*i).unwrap().id())
        .collect();
    order.reverse();
    let effects = normal(&shared, &[Event::ChannelOrder(order)]);
    let after = effects.after();
    assert_ne!(before.channels().order(), after.channels().order());
    assert!(std::ptr::eq(before.signatures(), after.signatures()));
    assert!(std::ptr::eq(before.classes(), after.classes()));
    assert!(std::ptr::eq(before.rolesets(), after.rolesets()));
    assert!(effects.changes().is_empty() && effects.regroups().is_empty());
    within(
        &effects,
        s.channels * 4 + s.channels * 16 + s.exceptions * (4 * s.channels + 8),
    );
}

fn load_within_estimate(snapshot: &Snapshot, dirty: bool) {
    let config = Config::default();
    let estimate = PermsShared::load_estimate(snapshot);
    let shared = PermsShared::new(guild(), config);
    shared.fault_extra_units(config.budget().runaway_cap(estimate).get() - estimate.get());
    let loaded = if dirty {
        assert_eq!(shared.load(snapshot), Err(PermsError::NeedsDirty));
        assert_eq!(shared.epoch().get(), 0);
        shared.load_dirty(snapshot)
    } else {
        shared.load(snapshot)
    };
    assert_eq!(loaded.unwrap().get(), 1, "estimate {estimate:?}");
}

#[test]
fn a_small_snapshot_loads_on_the_normal_scheduler() {
    let budget = Config::default().work_budget_units;
    let small = Fixture::SMALL.snapshot();
    assert!(PermsShared::load_estimate(&small).get() <= budget);
    load_within_estimate(&small, false);

    let mut medium = Fixture::SMALL.snapshot();
    let pairs = Fixture::SMALL.pairs();
    medium.members = (1..=1_500u64)
        .map(|u| {
            let (a, b) = pairs[usize::try_from(u).unwrap() % pairs.len()];
            (
                UserId::new(u),
                RoleList::Roles(vec![RoleId::new(b), RoleId::new(a), RoleId::new(a)]),
            )
        })
        .collect();
    assert!(PermsShared::load_estimate(&medium).get() <= budget);
    load_within_estimate(&medium, false);

    let large = Fixture::LARGE.snapshot();
    assert!(PermsShared::load_estimate(&large).get() <= budget);
    load_within_estimate(&large, false);

    let mut big = Fixture::LARGE.snapshot();
    let pairs = Fixture::LARGE.pairs();
    big.members = (1..=5_000u64)
        .map(|u| {
            let (a, b) = pairs[usize::try_from(u).unwrap() % pairs.len()];
            (
                UserId::new(u),
                RoleList::Roles(vec![RoleId::new(a), RoleId::new(b)]),
            )
        })
        .collect();
    assert!(PermsShared::load_estimate(&big).get() > budget);
    load_within_estimate(&big, true);
}
