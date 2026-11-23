// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use std::collections::BTreeMap;

use perm_core::{
    ChannelKind, ClassId, Config, Event, Grant, Group, Permissions, PermsError, PermsShared,
    RoleList, RoleSetId, Stage, UserId, Visible, diff,
};
use support::naive::{Rng, World};

fn check(shared: &PermsShared, world: &World) {
    let view = shared.view().unwrap();
    let model = view.model();
    let members = view.members();
    assert_eq!(members.len(), world.members.len());
    for (user, list) in &world.members {
        let entry = members.get(*user).unwrap();
        assert_eq!(entry.is_quarantined(), *list == RoleList::Malformed);
        assert_eq!(
            entry.is_exception(),
            model.is_exception(*user),
            "user {user:?}"
        );
        for spec in &world.channels {
            let idx = model.channel_idx(spec.id).unwrap();
            let class = model.class_of(idx).unwrap();
            let got = model.visible(class, entry.roleset(), *user);
            assert_eq!(
                got,
                world.visible(*user, spec),
                "user {user:?} channel {:?}",
                spec.id
            );
        }
        let expected_group = if model.is_exception(*user) {
            Group::User(*user)
        } else {
            Group::RoleSet(entry.roleset())
        };
        assert_eq!(view.group_of(*user), Some(expected_group));
    }
    for user in world.virtual_access.keys() {
        if world.members.contains_key(user) {
            continue;
        }
        assert_eq!(view.group_of(*user), Some(Group::User(*user)));
        for spec in &world.channels {
            let class = model.class(model.class_of_id(spec.id).unwrap()).unwrap();
            let got = class.override_of(*user).unwrap_or(Visible::empty());
            assert_eq!(
                got,
                world.visible(*user, spec),
                "va user {user:?} channel {:?}",
                spec.id
            );
        }
    }
    let mut counts: BTreeMap<RoleSetId, u32> = BTreeMap::new();
    for entry in members.iter() {
        *counts.entry(entry.roleset()).or_default() += 1;
    }
    for (set, _) in model.rolesets().iter() {
        assert_eq!(
            members.refcount(set),
            counts.get(&set).copied().unwrap_or(0)
        );
    }
    assert_eq!(members.live_sets() as usize, counts.len());

    let order: Vec<_> = model.channels().in_order().map(|(_, c)| c.id()).collect();
    let expected: Vec<_> = world.channels.iter().map(|c| c.id).collect();
    assert_eq!(order, expected);

    let mut seen = 0usize;
    let live: Vec<(ClassId, &perm_core::Class)> = model.classes().iter().collect();
    for (i, (id, class)) in live.iter().enumerate() {
        assert!(!class.channels().is_empty());
        for channel in class.channels() {
            assert_eq!(model.class_of(*channel), Some(*id));
        }
        seen += class.channels().len();
        let users: Vec<UserId> = class.overrides().iter().map(|(u, _)| *u).collect();
        let mut sorted = users.clone();
        sorted.sort();
        sorted.dedup();
        assert_eq!(users, sorted);
        assert!(users.iter().all(|u| model.is_exception(*u)));
        for (_, other) in live.iter().skip(i + 1) {
            assert!(!class.same_content(other));
        }
    }
    assert_eq!(seen, world.channels.len());
}

type Step = (Vec<(u64, Option<ClassId>)>, perm_core::Delta);

fn run(seed: u64, steps: usize) -> Vec<Step> {
    let mut rng = Rng::new(seed);
    let roles = 1 + rng.below(12);
    let channels = 1 + rng.below(25);
    let members = 1 + rng.below(60);
    let mut world = World::random(&mut rng, roles, channels, members);
    let shared = PermsShared::new(World::guild(), Config::default());
    shared.load(&world.snapshot()).unwrap();
    check(&shared, &world);
    let mut trace = Vec::new();
    for _ in 0..steps {
        let count = 1 + rng.below(3);
        let events: Vec<Event> = (0..count).map(|_| world.random_event(&mut rng)).collect();
        let effects = shared.apply(&events).unwrap();
        assert!(effects.spent() <= effects.estimate());
        check(&shared, &world);
        let delta = effects.delta();
        assert_eq!(delta, diff(effects.before(), effects.after()));
        let model = effects.after();
        let ids = world
            .channels
            .iter()
            .map(|c| (c.id.get(), model.class_of_id(c.id)))
            .collect();
        trace.push((ids, delta));
    }
    trace
}

#[test]
fn model_matches_naive_evaluation() {
    for seed in 1..=300 {
        run(seed, 40);
    }
}

#[test]
fn class_ids_are_deterministic() {
    for seed in 1..=40 {
        assert_eq!(run(seed, 30), run(seed, 30));
    }
}

#[test]
fn unchanged_content_keeps_class_ids() {
    let mut rng = Rng::new(7);
    let world = World::random(&mut rng, 6, 12, 30);
    let shared = PermsShared::new(World::guild(), Config::default());
    shared.load(&world.snapshot()).unwrap();
    let before = shared.model().unwrap();
    let effects = shared
        .apply(&[Event::ChannelOrder(
            world.channels.iter().rev().map(|c| c.id).collect(),
        )])
        .unwrap();
    let after = effects.after();
    for spec in &world.channels {
        assert_eq!(before.class_of_id(spec.id), after.class_of_id(spec.id));
    }
    let delta = effects.delta();
    assert!(delta.classes.is_empty() && delta.moves.is_empty());
    assert!(!delta.roles_changed);
}

#[test]
fn regroups_follow_role_and_exception_changes() {
    let mut rng = Rng::new(11);
    let mut world = World::random(&mut rng, 4, 4, 10);
    world.owner = None;
    world
        .members
        .insert(UserId::new(1), RoleList::Roles(vec![]));
    let shared = PermsShared::new(World::guild(), Config::default());
    shared.load(&world.snapshot()).unwrap();
    let effects = shared.apply(&[Event::Owner(Some(UserId::new(1)))]).unwrap();
    let regroup = effects
        .regroups()
        .iter()
        .find(|r| r.user == UserId::new(1))
        .copied()
        .unwrap();
    assert_eq!(regroup.new, Some(Group::User(UserId::new(1))));
    let effects = shared
        .apply(&[Event::MemberRemove(UserId::new(1))])
        .unwrap();
    assert_eq!(effects.regroups()[0].new, None);
    let effects = shared
        .apply(&[Event::MemberUpsert(
            UserId::new(77),
            RoleList::Roles(vec![]),
        )])
        .unwrap();
    assert_eq!(
        effects.regroups(),
        &[perm_core::Regroup {
            user: UserId::new(77),
            old: None,
            new: Some(Group::RoleSet(RoleSetId::EVERYONE)),
        }]
    );
    assert!(std::sync::Arc::ptr_eq(effects.before(), effects.after()));
}

#[test]
fn over_budget_mutates_nothing() {
    let mut rng = Rng::new(3);
    let mut world = World::random(&mut rng, 8, 20, 40);
    let config = Config {
        work_budget_units: 10,
        ..Config::default()
    };
    let shared = PermsShared::new(World::guild(), config);
    let snapshot = world.snapshot();
    assert_eq!(shared.load(&snapshot), Err(PermsError::NeedsDirty));
    shared.load_dirty(&snapshot).unwrap();
    let epoch = shared.epoch();
    let event = world.random_channel(&mut rng, None);
    world.channels.push(event.clone());
    let events = [Event::ChannelUpsert(
        event,
        perm_core::ChannelCause::Created,
    )];
    assert_eq!(shared.apply(&events).unwrap_err(), PermsError::NeedsDirty);
    assert_eq!(shared.epoch(), epoch);
    let effects = shared.apply_dirty(&events).unwrap();
    assert_eq!(effects.epoch(), epoch.next());
    check(&shared, &world);
    assert!(shared.stats().dirty_calls >= 2);
}

#[test]
fn metered_work_stays_within_the_estimate() {
    for seed in 1..=60 {
        let mut rng = Rng::new(seed);
        let mut world = World::random(&mut rng, 10, 30, 80);
        let shared = PermsShared::new(World::guild(), Config::default());
        shared.load(&world.snapshot()).unwrap();
        for _ in 0..20 {
            let event = world.random_event(&mut rng);
            let effects = shared.apply_dirty(std::slice::from_ref(&event)).unwrap();
            assert!(effects.spent() <= effects.estimate(), "seed {seed}");
        }
    }
}

#[test]
fn stage_fold_equals_sequential_application() {
    let mut rng = Rng::new(99);
    for _ in 0..200_000 {
        let n = rng.below(6) as usize;
        let stages: Vec<Stage> = (0..n)
            .map(|_| {
                Stage::new(
                    Permissions::from_bits_retain(rng.next()),
                    Permissions::from_bits_retain(rng.next()),
                )
            })
            .collect();
        let x = Permissions::from_bits_retain(rng.next());
        let sequential = stages.iter().fold(x, |acc, s| s.apply(acc));
        assert_eq!(Stage::fold(stages.iter().copied()).apply(x), sequential);
    }
}

#[test]
fn role_grants_or_merge_allow_over_deny() {
    let a = Grant::new(Permissions::VIEW_CHANNEL, Permissions::empty());
    let b = Grant::new(Permissions::empty(), Permissions::VIEW_CHANNEL);
    let merged = a.merge(b).as_stage();
    assert!(
        merged
            .apply(Permissions::empty())
            .contains(Permissions::VIEW_CHANNEL)
    );
}

#[test]
fn ghost_roles_still_match_overwrites() {
    let mut rng = Rng::new(5);
    let mut world = World::random(&mut rng, 3, 0, 0);
    world.roles.retain(|r| r.id.get() == support::GUILD);
    world.roles[0].permissions = Permissions::empty();
    let mut channel = world.random_channel(&mut rng, None);
    channel.kind = ChannelKind::Text;
    channel.parent = None;
    channel.overwrites = vec![perm_core::OverwriteSpec {
        target: perm_core::OverwriteTarget::Role(perm_core::RoleId::new(4242)),
        allow: Permissions::VIEW_CHANNEL,
        deny: Permissions::empty(),
    }];
    world.channels = vec![channel.clone()];
    world.owner = None;
    world.members.insert(
        UserId::new(1),
        RoleList::Roles(vec![perm_core::RoleId::new(4242)]),
    );
    world
        .members
        .insert(UserId::new(2), RoleList::Roles(vec![]));
    let shared = PermsShared::new(World::guild(), Config::default());
    shared.load(&world.snapshot()).unwrap();
    check(&shared, &world);
    let view = shared.view().unwrap();
    let class = view.model().class_of_id(channel.id).unwrap();
    let one = view.roleset(UserId::new(1)).unwrap();
    let two = view.roleset(UserId::new(2)).unwrap();
    assert_eq!(
        view.model().visible(class, one, UserId::new(1)),
        Visible::VIEW
    );
    assert_eq!(
        view.model().visible(class, two, UserId::new(2)),
        Visible::empty()
    );
}
