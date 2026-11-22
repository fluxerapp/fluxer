// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Config, Event, Meter, Nsfw, OverwriteSpec,
    OverwriteTarget, Permissions, PermsError, PermsShared, Read, RoleId, RoleList, Subject, UserId,
    Visible,
};
use support::naive::{Rng, World};

fn text(id: u64, overwrites: Vec<OverwriteSpec>) -> ChannelSpec {
    ChannelSpec {
        id: ChannelId::new(id),
        kind: ChannelKind::Text,
        parent: None,
        overwrites,
        nsfw: Nsfw::Inherit,
        named: true,
    }
}

fn hide_from_everyone() -> OverwriteSpec {
    OverwriteSpec {
        target: OverwriteTarget::Role(RoleId::new(support::GUILD)),
        allow: Permissions::empty(),
        deny: Permissions::VIEW_CHANNEL,
    }
}

#[test]
fn normal_apply_never_spends_more_than_its_budget() {
    let mut world = World::random(&mut Rng::new(1), 1, 0, 0);
    world.roles.retain(|r| r.id.get() == support::GUILD);
    world.owner = None;
    world.channels = vec![text(1, vec![])];
    for user in 1..=300 {
        world
            .members
            .insert(UserId::new(user), RoleList::Roles(vec![]));
    }
    let budget = 1_000;
    let shared = PermsShared::new(
        World::guild(),
        Config {
            work_budget_units: budget,
            ..Config::default()
        },
    );
    let snapshot = world.snapshot();
    assert_eq!(shared.load(&snapshot), Err(PermsError::NeedsDirty));
    shared.load_dirty(&snapshot).unwrap();
    let overwrites = (1..=300)
        .map(|user| OverwriteSpec {
            target: OverwriteTarget::Member(UserId::new(user)),
            allow: Permissions::VIEW_CHANNEL,
            deny: Permissions::empty(),
        })
        .chain([hide_from_everyone()])
        .collect();
    let events = [Event::ChannelUpsert(
        text(2, overwrites),
        ChannelCause::Created,
    )];
    match shared.apply(&events) {
        Ok(effects) => {
            assert!(effects.spent().get() <= budget, "{:?}", effects.spent());
        }
        Err(error) => assert_eq!(error, PermsError::NeedsDirty),
    }
    let effects = shared.apply_dirty(&events).unwrap();
    assert!(effects.spent() <= effects.estimate());
}

#[test]
fn metered_work_stays_within_the_real_estimate() {
    for seed in 1..=200 {
        let mut rng = Rng::new(seed);
        let mut world = World::random(&mut rng, 10, 30, 80);
        let shared = PermsShared::new(World::guild(), Config::default());
        shared.load(&world.snapshot()).unwrap();
        let uncached = PermsShared::new(
            World::guild(),
            Config {
                viewable_cache_bytes: 0,
                ..Config::default()
            },
        );
        uncached.load(&world.snapshot()).unwrap();
        for _ in 0..30 {
            let count = 1 + rng.below(4);
            let events: Vec<Event> = (0..count).map(|_| world.random_event(&mut rng)).collect();
            let effects = shared.apply_dirty(&events).unwrap();
            assert!(
                effects.spent() <= effects.estimate(),
                "seed {seed}: {:?} > {:?}",
                effects.spent(),
                effects.estimate()
            );
            uncached.apply_dirty(&events).unwrap();
            for perms in [&shared, &uncached] {
                filter_sharing_within_estimate(perms, &world, seed);
            }
        }
    }
}

fn filter_sharing_within_estimate(shared: &PermsShared, world: &World, seed: u64) {
    let view = shared.view().unwrap();
    let mut candidates: Vec<Subject> = (1..=world.user_pool + 2)
        .map(|u| Subject::Member(UserId::new(u)))
        .collect();
    candidates.extend(
        (1..=world.user_pool)
            .step_by(7)
            .map(|u| Subject::NonMember(UserId::new(u))),
    );
    candidates.extend(world.owner.map(Subject::Member));
    candidates.push(Subject::Visitor);
    candidates.push(Subject::Member(UserId::new(support::UNKNOWN_USER)));
    let estimate = view.estimate(Read::FilterSharing(candidates.len()));
    for target in candidates.iter().step_by(9) {
        let mut meter = Meter::new();
        view.filter_sharing(target, &candidates, &mut meter)
            .unwrap();
        assert!(
            meter.spent() <= estimate,
            "seed {seed} {target:?}: {:?} > {estimate:?}",
            meter.spent()
        );
    }
}

#[test]
fn owner_who_is_not_a_member_sees_only_virtual_access() {
    let mut world = World::random(&mut Rng::new(2), 1, 0, 0);
    world.roles.retain(|r| r.id.get() == support::GUILD);
    world.roles[0].permissions = Permissions::VIEW_CHANNEL;
    let owner = UserId::new(9);
    world.owner = Some(owner);
    world
        .members
        .insert(UserId::new(1), RoleList::Roles(vec![]));
    world.channels = vec![text(1, vec![hide_from_everyone()]), text(2, vec![])];
    world.virtual_access.insert(owner, vec![ChannelId::new(1)]);
    let shared = PermsShared::new(World::guild(), Config::default());
    shared.load(&world.snapshot()).unwrap();
    let model = shared.model().unwrap();
    let bits = |channel: u64| {
        model
            .class(model.class_of_id(ChannelId::new(channel)).unwrap())
            .unwrap()
            .override_of(owner)
            .unwrap_or(Visible::empty())
    };
    assert_eq!(bits(1), Visible::VIEW);
    assert_eq!(bits(2), Visible::empty());
    let hidden = text(3, vec![hide_from_everyone()]);
    shared
        .apply(&[Event::ChannelUpsert(hidden, ChannelCause::Created)])
        .unwrap();
    let model = shared.model().unwrap();
    let class = model
        .class(model.class_of_id(ChannelId::new(3)).unwrap())
        .unwrap();
    assert_eq!(class.override_of(owner), None);
}
