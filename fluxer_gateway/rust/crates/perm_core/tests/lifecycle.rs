// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Config, Event, Nsfw, OverwriteSpec,
    OverwriteTarget, Permissions, PermsError, PermsShared, RoleId, RoleList, Snapshot, Status,
    UserId,
};
use support::{GUILD, guild};

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

fn snapshot(members: u64) -> Snapshot {
    let mut s = Snapshot::new(guild(), None);
    s.roles = vec![perm_core::RoleSpec {
        id: RoleId::new(GUILD),
        permissions: Permissions::VIEW_CHANNEL,
        position: perm_core::RankPos::Int(0),
        hoist: perm_core::Hoist::NotHoisted,
    }];
    s.channels = vec![text(1, vec![])];
    for u in 1..=members {
        s.members
            .push((UserId::new(u), RoleList::Roles(vec![RoleId::new(u % 7)])));
    }
    s
}

#[test]
fn a_loaded_resource_refuses_a_second_load() {
    let shared = PermsShared::new(guild(), Config::default());
    shared.load(&snapshot(20)).unwrap();
    let before = shared.model().unwrap();
    assert!(matches!(
        shared.load(&snapshot(21)),
        Err(PermsError::BadInput(_))
    ));
    let mut reordered = snapshot(20);
    reordered.members.reverse();
    assert!(matches!(
        shared.load(&reordered),
        Err(PermsError::BadInput(_))
    ));
    assert!(std::sync::Arc::ptr_eq(&before, &shared.model().unwrap()));
}

#[test]
fn a_load_over_budget_asks_for_the_dirty_twin() {
    let big = snapshot(20_000);
    assert!(PermsShared::load_estimate(&big).get() > Config::default().work_budget_units);
    let shared = PermsShared::new(guild(), Config::default());
    assert_eq!(shared.load(&big), Err(PermsError::NeedsDirty));
    assert_eq!(shared.epoch().get(), 0);
    shared.load_dirty(&big).unwrap();
    assert_eq!(shared.stats().members, 20_000);
    let small = snapshot(300);
    assert!(PermsShared::load_estimate(&small).get() <= Config::default().work_budget_units);
}

#[test]
fn over_budget_apply_mutates_nothing() {
    let shared = PermsShared::new(
        guild(),
        Config {
            work_budget_units: 50,
            ..Config::default()
        },
    );
    shared.load_dirty(&snapshot(40)).unwrap();
    let epoch = shared.epoch();
    let members = shared.stats().members;
    let hidden = OverwriteSpec {
        target: OverwriteTarget::Member(UserId::new(3)),
        allow: Permissions::empty(),
        deny: Permissions::VIEW_CHANNEL,
    };
    let events = [
        Event::ChannelUpsert(text(2, vec![hidden]), ChannelCause::Created),
        Event::MemberUpsert(UserId::new(500), RoleList::Roles(vec![RoleId::new(42)])),
    ];
    assert_eq!(shared.apply(&events).unwrap_err(), PermsError::NeedsDirty);
    assert_eq!(shared.epoch(), epoch);
    assert_eq!(shared.stats().members, members);
    let effects = shared.apply_dirty(&events).unwrap();
    assert_eq!(effects.epoch(), epoch.next());
    assert!(effects.spent() <= effects.estimate());
    assert_eq!(shared.stats().members, members + 1);
}

#[test]
fn release_answers_gone() {
    let shared = PermsShared::new(guild(), Config::default());
    shared.load(&snapshot(5)).unwrap();
    let held = shared.clone();
    shared.release();
    assert_eq!(held.status(), Status::Released);
    assert_eq!(held.view().err(), Some(PermsError::Gone));
    assert_eq!(held.apply(&[]).unwrap_err(), PermsError::Gone);
    assert_eq!(held.stats().members, 0);
}

#[test]
fn compaction_remaps_live_role_sets_and_keeps_answers() {
    let shared = PermsShared::new(
        guild(),
        Config {
            dead_role_set_floor: 3,
            ..Config::default()
        },
    );
    shared.load(&snapshot(0)).unwrap();
    let mut due = false;
    for r in 0..12u64 {
        let user = UserId::new(1 + r % 2);
        let effects = shared
            .apply(&[Event::MemberUpsert(
                user,
                RoleList::Roles(vec![RoleId::new(100 + r)]),
            )])
            .unwrap();
        due |= effects.compaction_due();
    }
    assert!(due);
    let before = shared.model().unwrap();
    let done = shared.compact().unwrap().unwrap();
    let delta = done.delta();
    let remap = delta.remap.clone().unwrap();
    assert_eq!(remap.len(), before.rolesets().len());
    let view = shared.view().unwrap();
    assert_eq!(view.model().rolesets().len(), 3);
    for user in [UserId::new(1), UserId::new(2)] {
        let set = view.roleset(user).unwrap();
        assert_ne!(set.get(), perm_core::rolesets::DEAD);
        assert_eq!(
            view.permissions(user, perm_core::Scope::Guild).unwrap(),
            Permissions::VIEW_CHANNEL
        );
    }
    assert!(shared.compact().unwrap().is_none());
}

#[test]
fn an_owner_change_reaches_a_member_whose_upsert_changed_nothing() {
    let shared = PermsShared::new(guild(), Config::default());
    let mut s = snapshot(3);
    let keep = OverwriteSpec {
        target: OverwriteTarget::Member(UserId::new(2)),
        allow: Permissions::empty(),
        deny: Permissions::empty(),
    };
    s.channels = vec![text(1, vec![keep])];
    s.owner = Some(UserId::new(2));
    shared.load(&s).unwrap();
    let roles = s.members[1].1.clone();
    shared
        .apply(&[
            Event::MemberUpsert(UserId::new(2), roles),
            Event::Owner(Some(UserId::new(3))),
        ])
        .unwrap();
    let view = shared.view().unwrap();
    let model = view.model();
    let class = model.class_of_id(ChannelId::new(1)).unwrap();
    for user in [UserId::new(2), UserId::new(3)] {
        let want = if user == UserId::new(3) {
            perm_core::Visible::all()
        } else {
            perm_core::Visible::VIEW
        };
        assert_eq!(
            model.visible(class, view.roleset(user).unwrap(), user),
            want,
            "{user:?}"
        );
    }
}

#[test]
fn member_writes_reach_readers_on_every_thread() {
    let shared = PermsShared::new(guild(), Config::default());
    shared.load(&snapshot(20)).unwrap();
    let threads = 80usize;
    let barrier = std::sync::Arc::new(std::sync::Barrier::new(threads + 1));
    let handles: Vec<_> = (0..threads)
        .map(|_| {
            let shared = shared.clone();
            let barrier = barrier.clone();
            std::thread::spawn(move || {
                let mut seen = Vec::new();
                for _ in 0..3 {
                    barrier.wait();
                    let view = shared.view().unwrap();
                    seen.push((view.members().len(), view.has_member(UserId::new(100))));
                    drop(view);
                    barrier.wait();
                }
                seen
            })
        })
        .collect();
    let steps = [
        vec![],
        vec![Event::MemberUpsert(
            UserId::new(100),
            RoleList::Roles(vec![RoleId::new(3)]),
        )],
        vec![Event::MemberRemove(UserId::new(100))],
    ];
    for events in &steps {
        if !events.is_empty() {
            shared.apply(events).unwrap();
        }
        barrier.wait();
        barrier.wait();
    }
    for handle in handles {
        assert_eq!(
            handle.join().unwrap(),
            vec![(20, false), (21, true), (20, false)]
        );
    }
}

#[test]
fn a_member_moving_to_an_existing_role_set_advances_the_revision() {
    let moderator = RoleId::new(7);
    let mut s = snapshot(0);
    s.roles.push(perm_core::RoleSpec {
        id: moderator,
        permissions: Permissions::empty(),
        position: perm_core::RankPos::Int(1),
        hoist: perm_core::Hoist::NotHoisted,
    });
    s.channels.push(text(
        10,
        vec![
            OverwriteSpec {
                target: OverwriteTarget::Role(RoleId::new(GUILD)),
                allow: Permissions::empty(),
                deny: Permissions::VIEW_CHANNEL,
            },
            OverwriteSpec {
                target: OverwriteTarget::Role(moderator),
                allow: Permissions::VIEW_CHANNEL,
                deny: Permissions::empty(),
            },
        ],
    ));
    let (a, b, c) = (UserId::new(1), UserId::new(2), UserId::new(3));
    s.members = vec![
        (a, RoleList::Roles(vec![moderator])),
        (b, RoleList::Roles(vec![moderator])),
        (c, RoleList::Roles(vec![])),
    ];
    let shared = PermsShared::new(guild(), Config::default());
    shared.load(&s).unwrap();
    let (moderators, plain) = {
        let view = shared.view().unwrap();
        (view.group_of(b), view.group_of(c))
    };
    assert_ne!(moderators, plain);
    assert_eq!(shared.view().unwrap().group_of(a), moderators);

    let loaded = shared.revision();
    shared.apply(&[]).unwrap();
    assert_eq!(shared.revision(), loaded);

    shared
        .apply(&[Event::MemberUpsert(a, RoleList::Roles(vec![]))])
        .unwrap();
    let demoted = shared.revision();
    assert!(demoted > loaded);
    assert_eq!(shared.view().unwrap().group_of(a), plain);

    shared
        .apply(&[Event::MemberUpsert(a, RoleList::Roles(vec![moderator]))])
        .unwrap();
    assert!(shared.revision() > demoted);
    assert_eq!(shared.view().unwrap().group_of(a), moderators);
}

#[test]
fn release_advances_the_revision() {
    let shared = PermsShared::new(guild(), Config::default());
    shared.load(&snapshot(3)).unwrap();
    let loaded = shared.revision();
    shared.release();
    assert!(shared.revision() > loaded);
}

#[test]
fn try_release_never_shows_a_live_model_with_an_emptied_index() {
    for _ in 0..400 {
        let shared = PermsShared::new(guild(), Config::default());
        shared.load(&snapshot(64)).unwrap();
        let stop = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let readers: Vec<_> = (0..4)
            .map(|_| {
                let shared = shared.clone();
                let stop = stop.clone();
                std::thread::spawn(move || {
                    let mut empty = 0u32;
                    while !stop.load(std::sync::atomic::Ordering::Acquire) {
                        if let Ok(view) = shared.view()
                            && view.members().is_empty()
                        {
                            empty += 1;
                        }
                        if shared.status() == Status::Released {
                            break;
                        }
                    }
                    empty
                })
            })
            .collect();
        std::thread::yield_now();
        while shared.try_release().is_err() {}
        stop.store(true, std::sync::atomic::Ordering::Release);
        let empty: u32 = readers.into_iter().map(|r| r.join().unwrap()).sum();
        assert_eq!(empty, 0);
        assert_eq!(shared.view().err(), Some(PermsError::Gone));
    }
}
