// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use std::sync::Arc;

use perm_core::{
    ChannelId, ChannelKind, ChannelSpec, Config, Event, Hoist, Meter, Nsfw, OverwriteSpec,
    OverwriteTarget, Permissions, PermsError, PermsShared, RankPos, RoleId, RoleList, RoleSpec,
    Snapshot, UserId,
};
use support::{Driver, GUILD, Op, Params, apply_step, check_effects, check_view, guild};

fn params(cache: u64) -> Params {
    Params {
        roles: 5,
        channels: 16,
        members: 32,
        seed: 1,
        budget: 250_000,
        floor: 4096,
        cache,
    }
}

fn snapshot(irregular: bool) -> Snapshot {
    let mut s = Snapshot::new(guild(), None);
    s.roles = [GUILD, 1, 2, 3, 4, 5]
        .into_iter()
        .map(|id| RoleSpec {
            id: RoleId::new(id),
            permissions: Permissions::VIEW_CHANNEL,
            position: RankPos::Int(0),
            hoist: Hoist::NotHoisted,
        })
        .collect();
    s.channels = (1..=16)
        .map(|id| ChannelSpec {
            id: ChannelId::new(id),
            kind: ChannelKind::Text,
            parent: (irregular && id == 2).then_some(ChannelId::new(1)),
            overwrites: vec![],
            nsfw: Nsfw::Safe,
            named: true,
        })
        .collect();
    s.members = (0..32)
        .map(|bits| {
            let roles = (0..5)
                .filter(|bit| bits & (1 << bit) != 0)
                .map(|bit| RoleId::new(bit + 1))
                .collect();
            (UserId::new(bits + 1), RoleList::Roles(roles))
        })
        .collect();
    s
}

fn member_overwrites(users: std::ops::RangeInclusive<u64>) -> Vec<OverwriteSpec> {
    users
        .map(|id| OverwriteSpec {
            target: OverwriteTarget::Member(UserId::new(id)),
            allow: Permissions::VIEW_CHANNEL,
            deny: Permissions::empty(),
        })
        .collect()
}

fn remove_channels(keep: u64) -> Vec<Op> {
    ((keep + 1)..=16)
        .map(|id| Op::Raw(Event::ChannelDelete(ChannelId::new(id))))
        .collect()
}

fn retained(data: &[u8], work: u64) {
    let case = support::bytes::decode(data).bounded(work);
    let mut driver = case.driver();
    for ops in &case.steps {
        let (before, effects) = apply_step(&mut driver, ops);
        check_effects(&driver, &before, &effects);
        check_view(&driver);
        support::maybe_compact(&mut driver, &effects);
    }
}

#[test]
fn retained_diff_accounting_crash() {
    retained(
        include_bytes!("seeds/diff/regression-visibility-accounting"),
        60_000,
    );
}

#[test]
fn minimized_diff_accounting_crash() {
    retained(
        include_bytes!("seeds/diff/regression-visibility-accounting-min"),
        60_000,
    );
}

#[test]
fn retained_events_accounting_crash() {
    retained(
        include_bytes!("seeds/events/regression-visibility-accounting"),
        40_000,
    );
}

#[test]
fn minimized_events_accounting_crash() {
    retained(
        include_bytes!("seeds/events/regression-visibility-accounting-min"),
        40_000,
    );
}

#[test]
fn shrinking_irregular_channels_accounts_for_both_models() {
    for cache in [0, 96, 1 << 20] {
        for warm in [false, true] {
            for keep in [0, 1, 8] {
                let mut driver = Driver::start(&params(cache), Some(&snapshot(true)));
                let model = driver.shared.model().unwrap();
                assert!(model.channels().has_irregular_parents());
                assert_eq!(model.rolesets().len(), 32);
                if warm {
                    check_view(&driver);
                }
                let (before, effects) = apply_step(&mut driver, &remove_channels(keep));
                check_effects(&driver, &before, &effects);
                check_view(&driver);
                assert_eq!(effects.after().channels().len(), keep as usize);
                assert!(effects.regroups().is_empty());
            }
        }
    }
}

#[test]
fn exception_removal_regroups_untouched_members() {
    for cache in [0, 1 << 20] {
        for warm in [false, true] {
            let mut s = snapshot(false);
            s.owner = Some(UserId::new(1));
            s.channels[0].overwrites = member_overwrites(1..=32);
            let mut driver = Driver::start(&params(cache), Some(&s));
            if warm {
                check_view(&driver);
            }
            let (before, effects) = apply_step(&mut driver, &remove_channels(0));
            assert_eq!(effects.regroups().len(), 31);
            assert!(effects.regroups().iter().all(|r| r.user != UserId::new(1)));
            check_effects(&driver, &before, &effects);
            check_view(&driver);
        }
    }
}

#[test]
fn unchanged_exceptions_account_for_the_old_channels() {
    for cache in [0, 1 << 20] {
        for irregular in [false, true] {
            for keep in [1, 2, 8] {
                let mut s = snapshot(irregular);
                s.owner = Some(UserId::new(1));
                s.channels[0].overwrites = member_overwrites(1..=32);
                let mut driver = Driver::start(&params(cache), Some(&s));
                let (before, effects) = apply_step(&mut driver, &remove_channels(keep));
                assert!(effects.regroups().is_empty());
                assert_eq!(effects.after().exceptions().len(), 32);
                check_effects(&driver, &before, &effects);
                check_view(&driver);
            }
        }
    }
}

#[test]
fn virtual_access_removal_accounts_for_the_old_list() {
    for size in [32, 256] {
        for remaining in [0, 1, 16] {
            let mut s = Snapshot::new(guild(), None);
            s.virtual_access
                .push((UserId::new(1), (1..=size).map(ChannelId::new).collect()));
            let mut driver = Driver::start(&params(0), Some(&s));
            let (before, effects) = apply_step(
                &mut driver,
                &[Op::Raw(Event::VirtualAccess(
                    UserId::new(1),
                    (1..=remaining).map(ChannelId::new).collect(),
                ))],
            );
            assert!(effects.spent().get() >= size);
            check_effects(&driver, &before, &effects);
            check_view(&driver);
        }
    }
}

#[test]
fn virtual_member_transitions_keep_visibility_and_accounting() {
    for initially_member in [false, true] {
        for listed in [16, 128] {
            let mut s = snapshot(false);
            let user = UserId::new(33);
            s.virtual_access
                .push((user, (1..=listed).map(ChannelId::new).collect()));
            if initially_member {
                s.members.push((user, RoleList::Roles(vec![])));
            }
            let mut driver = Driver::start(&params(0), Some(&s));
            let event = if initially_member {
                Event::MemberRemove(user)
            } else {
                Event::MemberUpsert(user, RoleList::Roles(vec![]))
            };
            let (before, effects) = apply_step(&mut driver, &[Op::Raw(event)]);
            check_effects(&driver, &before, &effects);
            check_view(&driver);
        }
    }
}

#[test]
fn refused_shrink_is_unchanged_and_dirty_matches_normal() {
    let s = snapshot(true);
    let normal = PermsShared::new(guild(), Config::default());
    let dirty = PermsShared::new(
        guild(),
        Config {
            work_budget_units: 1,
            ..Config::default()
        },
    );
    normal.load_dirty(&s).unwrap();
    dirty.load_dirty(&s).unwrap();
    let events: Vec<Event> = (1..=16)
        .map(|id| Event::ChannelDelete(ChannelId::new(id)))
        .collect();
    let viewable = |shared: &PermsShared| -> Vec<Vec<ChannelId>> {
        let view = shared.view().unwrap();
        let mut meter = Meter::new();
        (1..=32)
            .map(|user| view.viewable(UserId::new(user), &mut meter))
            .collect()
    };
    let before = dirty.model().unwrap();
    let seen = viewable(&dirty);
    assert_eq!(dirty.apply(&events).unwrap_err(), PermsError::NeedsDirty);
    assert!(Arc::ptr_eq(&before, &dirty.model().unwrap()));
    assert_eq!(dirty.epoch(), before.epoch());
    assert_eq!(seen, viewable(&dirty));
    for user in 1..=32 {
        assert!(dirty.view().unwrap().has_member(UserId::new(user)));
    }
    let a = normal.apply(&events).unwrap();
    let b = dirty.apply_dirty(&events).unwrap();
    assert!(a.spent() <= a.estimate());
    assert!(b.spent() <= b.estimate());
    assert_eq!(a.estimate(), b.estimate());
    assert_eq!(a.regroups(), b.regroups());
    assert_eq!(a.changes(), b.changes());
    assert_eq!(viewable(&normal), viewable(&dirty));
}
