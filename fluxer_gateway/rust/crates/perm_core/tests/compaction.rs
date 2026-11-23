// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};

use perm_core::audit::audit;
use perm_core::reference::Reference;
use perm_core::rolesets::DEAD;
use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Config, Effects, Event, Hoist, Nsfw,
    OverwriteSpec, OverwriteTarget, Permissions, PermsError, PermsShared, RankPos, RoleId,
    RoleList, RoleSpec, Scope, Snapshot, UserId,
};
use support::{GUILD, guild};

const ROLE_BITS: [Permissions; 6] = [
    Permissions::SEND_MESSAGES,
    Permissions::MANAGE_ROLES,
    Permissions::MANAGE_CHANNELS,
    Permissions::VIEW_AUDIT_LOG,
    Permissions::CONNECT,
    Permissions::MANAGE_THREADS,
];

fn role(id: u64, permissions: Permissions, position: i64) -> RoleSpec {
    RoleSpec {
        id: RoleId::new(id),
        permissions,
        position: RankPos::Int(position),
        hoist: Hoist::NotHoisted,
    }
}

fn channel(
    id: u64,
    kind: ChannelKind,
    parent: Option<u64>,
    overwrites: Vec<OverwriteSpec>,
) -> ChannelSpec {
    ChannelSpec {
        id: ChannelId::new(id),
        kind,
        parent: parent.map(ChannelId::new),
        overwrites,
        nsfw: Nsfw::Inherit,
        named: true,
    }
}

fn deny_everyone() -> OverwriteSpec {
    OverwriteSpec {
        target: OverwriteTarget::Role(RoleId::new(GUILD)),
        allow: Permissions::empty(),
        deny: Permissions::VIEW_CHANNEL,
    }
}

fn allow_role(id: u64) -> OverwriteSpec {
    OverwriteSpec {
        target: OverwriteTarget::Role(RoleId::new(id)),
        allow: Permissions::VIEW_CHANNEL,
        deny: Permissions::empty(),
    }
}

fn allow_member(user: u64) -> OverwriteSpec {
    OverwriteSpec {
        target: OverwriteTarget::Member(UserId::new(user)),
        allow: Permissions::VIEW_CHANNEL,
        deny: Permissions::empty(),
    }
}

fn subset(bits: u64) -> RoleList {
    RoleList::Roles(
        (0..ROLE_BITS.len() as u64)
            .filter(|i| bits & (1 << i) != 0)
            .map(|i| RoleId::new(10 + i))
            .collect(),
    )
}

fn base_snapshot() -> Snapshot {
    let mut s = Snapshot::new(guild(), Some(UserId::new(1)));
    s.roles = vec![role(GUILD, Permissions::VIEW_CHANNEL, 0)];
    for (i, bits) in ROLE_BITS.iter().enumerate() {
        s.roles.push(role(10 + i as u64, *bits, 1 + i as i64));
    }
    s.channels = vec![
        channel(1, ChannelKind::Text, None, vec![]),
        channel(2, ChannelKind::Category, None, vec![deny_everyone()]),
        channel(
            3,
            ChannelKind::Text,
            Some(2),
            vec![deny_everyone(), allow_role(11)],
        ),
        channel(
            4,
            ChannelKind::Text,
            Some(2),
            vec![deny_everyone(), allow_member(5)],
        ),
    ];
    for user in 1..=8u64 {
        s.members.push((UserId::new(user), subset(user % 4)));
    }
    s.virtual_access = vec![(UserId::new(40), vec![ChannelId::new(4)])];
    s
}

fn users() -> Vec<UserId> {
    (1..=12u64).chain([40, 99]).map(UserId::new).collect()
}

fn audited(shared: &PermsShared) {
    let view = shared.view().unwrap();
    if let Err(e) = audit(view.model(), view.members()) {
        panic!("audit: {e}");
    }
}

fn agrees(shared: &PermsShared, reference: &Reference) {
    let view = shared.view().unwrap();
    let mut ids: Vec<ChannelId> = reference.channels.iter().map(|c| c.id).collect();
    ids.push(ChannelId::new(424_242));
    for user in users() {
        assert_eq!(
            view.permissions(user, Scope::Guild).ok(),
            reference.permissions(user, Scope::Guild).ok(),
            "{user:?}"
        );
        for id in &ids {
            assert_eq!(
                view.visible(user, *id),
                reference.can_view(user, *id),
                "{user:?} {id:?}"
            );
            assert_eq!(
                view.permissions(user, Scope::Channel(*id)).ok(),
                reference.permissions(user, Scope::Channel(*id)).ok(),
                "{user:?} {id:?}"
            );
        }
        let mut meter = perm_core::Meter::new();
        assert_eq!(
            view.viewable(user, &mut meter),
            reference.viewable(user),
            "{user:?}"
        );
    }
}

fn apply(shared: &PermsShared, reference: &mut Reference, events: &[Event]) -> Effects {
    for e in events {
        reference.apply(e);
    }
    match shared.apply(events) {
        Ok(effects) => effects,
        Err(PermsError::NeedsDirty) => shared.apply_dirty(events).unwrap(),
        Err(e) => panic!("{e:?}"),
    }
}

fn compact(shared: &PermsShared) -> Effects {
    match shared.compact() {
        Ok(done) => done.unwrap(),
        Err(PermsError::NeedsDirty) => shared.compact_dirty().unwrap().unwrap(),
        Err(e) => panic!("{e:?}"),
    }
}

fn create(id: u64, parent: Option<u64>) -> Event {
    let kind = if id.is_multiple_of(5) {
        ChannelKind::Category
    } else {
        ChannelKind::Text
    };
    Event::ChannelUpsert(
        channel(
            id,
            kind,
            parent,
            vec![allow_role(10 + id % 6), allow_member(id % 9)],
        ),
        ChannelCause::Created,
    )
}

#[test]
fn slot_compaction_remaps_channels_and_keeps_answers() {
    let floor = 4;
    let shared = PermsShared::new(
        guild(),
        Config {
            dead_role_set_floor: floor,
            ..Config::default()
        },
    );
    let snapshot = base_snapshot();
    shared.load(&snapshot).unwrap();
    let mut reference = Reference::new(&snapshot);
    for id in 100..112u64 {
        apply(&shared, &mut reference, &[create(id, Some(2))]);
    }
    apply(
        &shared,
        &mut reference,
        &[Event::ChannelOrder(
            (1..5u64)
                .chain(100..112)
                .rev()
                .map(ChannelId::new)
                .collect(),
        )],
    );
    let mut due = false;
    for id in [100u64, 102, 103, 105, 107, 108, 110, 111, 1] {
        due = apply(
            &shared,
            &mut reference,
            &[Event::ChannelDelete(ChannelId::new(id))],
        )
        .compaction_due();
    }
    assert!(due);
    agrees(&shared, &reference);
    let before = shared.model().unwrap();
    assert_eq!(before.channels().dead_slots(), 9);
    let done = compact(&shared);
    assert!(done.roleset_remap().is_none());
    let remap = done.channel_remap().unwrap().clone();
    assert_eq!(remap.len(), before.channels().slots());
    let delta = done.delta();
    assert_eq!(delta.channel_remap.as_deref(), Some(&*remap));
    assert!(delta.remap.is_none() && delta.classes.is_empty() && delta.moves.is_empty());
    assert!(delta.created.is_empty() && delta.freed.is_empty() && !delta.resync);
    let after = shared.model().unwrap();
    assert_eq!(after.channels().slots(), after.channels().len());
    assert_eq!(after.channels().len(), 7);
    for (old, target) in remap.iter().enumerate() {
        let old = perm_core::ChannelIdx::from_index(old).unwrap();
        match before.channel(old) {
            None => assert_eq!(*target, DEAD),
            Some(c) => {
                let new = perm_core::ChannelIdx::new(*target);
                assert_eq!(after.channel_idx(c.id()), Some(new));
                assert_eq!(after.channel(new).unwrap().id(), c.id());
                let a = before.class(before.class_of(old).unwrap()).unwrap();
                let b = after.class(after.class_of(new).unwrap()).unwrap();
                assert_eq!(before.class_of(old), after.class_of(new));
                assert_eq!(a.planes(), b.planes());
                assert_eq!(a.overrides(), b.overrides());
                let parent = |m: &perm_core::Model, idx| {
                    m.channel(idx)
                        .and_then(|c| c.parent())
                        .and_then(|p| m.channel(p))
                        .map(|p| p.id())
                };
                assert_eq!(parent(&before, old), parent(&after, new));
                let kids = |m: &perm_core::Model, idx| {
                    m.channels()
                        .children(idx)
                        .iter()
                        .filter_map(|k| m.channel(*k).map(|c| c.id()))
                        .collect::<Vec<_>>()
                };
                assert_eq!(kids(&before, old), kids(&after, new));
            }
        }
    }
    let order = |m: &perm_core::Model| {
        m.channels()
            .in_order()
            .map(|(_, c)| c.id())
            .collect::<Vec<_>>()
    };
    assert_eq!(order(&before), order(&after));
    audited(&shared);
    agrees(&shared, &reference);
    assert!(shared.compact().unwrap().is_none());
    apply(
        &shared,
        &mut reference,
        &[
            create(200, Some(2)),
            Event::ChannelDelete(ChannelId::new(3)),
        ],
    );
    audited(&shared);
    agrees(&shared, &reference);
}

#[test]
fn lagging_diff_reports_channels_dropped_by_a_slot_compaction() {
    let shared = PermsShared::new(
        guild(),
        Config {
            dead_role_set_floor: 4,
            ..Config::default()
        },
    );
    let snapshot = base_snapshot();
    shared.load(&snapshot).unwrap();
    let mut reference = Reference::new(&snapshot);
    let created: Vec<Event> = (100..106u64).map(|id| create(id, None)).collect();
    apply(&shared, &mut reference, &created);
    let last = shared.model().unwrap();
    let gone: Vec<u64> = (100..106u64).chain([1]).collect();
    let mut due = false;
    for id in &gone {
        due = apply(
            &shared,
            &mut reference,
            &[Event::ChannelDelete(ChannelId::new(*id))],
        )
        .compaction_due();
    }
    assert!(due);
    let done = compact(&shared);
    assert!(done.delta().dropped.is_empty());
    let now = shared.model().unwrap();
    let delta = perm_core::diff(&last, &now);
    assert!(!delta.resync);
    let remap = delta.channel_remap.clone().unwrap();
    let mut expected: Vec<_> = gone
        .iter()
        .map(|id| {
            let id = ChannelId::new(*id);
            (last.channel_idx(id).unwrap(), last.class_of_id(id).unwrap())
        })
        .collect();
    expected.sort();
    assert_eq!(delta.dropped, expected);
    for (idx, _) in &delta.dropped {
        assert_eq!(remap[idx.index()], DEAD);
    }
    assert!(delta.moves.is_empty());
    for id in [2u64, 3, 4] {
        let id = ChannelId::new(id);
        let old = last.channel_idx(id).unwrap();
        let new = now.channel_idx(id).unwrap();
        assert_eq!(remap[old.index()], new.index() as u32);
        assert_eq!(last.class_of(old), now.class_of(new));
    }
    audited(&shared);
    agrees(&shared, &reference);
}

#[test]
fn ten_thousand_channel_cycles_keep_the_slots_bounded() {
    let floor = 16u32;
    let shared = PermsShared::new(
        guild(),
        Config {
            dead_role_set_floor: floor,
            ..Config::default()
        },
    );
    let snapshot = base_snapshot();
    shared.load(&snapshot).unwrap();
    let mut reference = Reference::new(&snapshot);
    let mut slot_compactions = 0u32;
    let mut set_compactions = 0u32;
    let mut most = 0usize;
    for cycle in 0..10_000u64 {
        let id = 1_000 + cycle;
        let parent = if cycle.is_multiple_of(3) {
            Some(2)
        } else {
            None
        };
        let mut effects = vec![apply(
            &shared,
            &mut reference,
            &[
                create(id, parent),
                Event::MemberUpsert(UserId::new(9), subset(cycle % 64)),
            ],
        )];
        effects.push(apply(
            &shared,
            &mut reference,
            &[Event::ChannelDelete(ChannelId::new(id))],
        ));
        for e in &effects {
            if !e.compaction_due() || !shared.compaction_due() {
                continue;
            }
            let done = compact(&shared);
            let model = shared.model().unwrap();
            if done.channel_remap().is_some() {
                slot_compactions += 1;
                assert_eq!(model.channels().slots(), model.channels().len());
            }
            if done.roleset_remap().is_some() {
                set_compactions += 1;
            }
        }
        let model = shared.model().unwrap();
        let live = model.channels().len();
        let bound = live + (floor as usize).max(live) + 1;
        assert!(
            model.channels().slots() <= bound,
            "cycle {cycle}: {} slots for {live} live channels",
            model.channels().slots()
        );
        let view = shared.view().unwrap();
        let live_sets = view.members().live_sets() as usize;
        assert!(model.rolesets().len() <= 1 + live_sets + (floor as usize).max(live_sets) + 1);
        most = most.max(model.channels().slots());
        if cycle % 1_000 == 999 {
            drop(view);
            audited(&shared);
            agrees(&shared, &reference);
        }
    }
    println!(
        "10,000 cycles: {slot_compactions} slot compactions, {set_compactions} role set compactions, at most {most} slots"
    );
    assert!(slot_compactions >= 10_000 / (floor + 1) - 1);
    assert!(set_compactions > 0);
    assert!(most <= 4 + floor as usize + 1);
}

#[test]
fn readers_never_see_a_torn_model_during_compaction() {
    let floor = 2u32;
    let shared = PermsShared::new(
        guild(),
        Config {
            dead_role_set_floor: floor,
            ..Config::default()
        },
    );
    let snapshot = base_snapshot();
    shared.load(&snapshot).unwrap();
    let mut reference = Reference::new(&snapshot);
    let stable: Vec<(UserId, Permissions)> = (1..=8u64)
        .map(|u| {
            let user = UserId::new(u);
            (user, reference.permissions(user, Scope::Guild).unwrap())
        })
        .collect();
    let stable_views: Vec<(UserId, ChannelId, perm_core::Visible)> = (1..=8u64)
        .flat_map(|u| [1u64, 3, 4].map(|c| (u, c)))
        .map(|(u, c)| {
            let (user, id) = (UserId::new(u), ChannelId::new(c));
            (user, id, reference.can_view(user, id))
        })
        .collect();
    let stop = Arc::new(AtomicBool::new(false));
    let reads = Arc::new(AtomicU64::new(0));
    let readers: Vec<_> = (0..4)
        .map(|_| {
            let shared = shared.clone();
            let stop = stop.clone();
            let reads = reads.clone();
            let stable = stable.clone();
            let stable_views = stable_views.clone();
            std::thread::spawn(move || {
                while !stop.load(Ordering::Relaxed) {
                    let view = shared.view().unwrap();
                    let model = view.model();
                    assert_eq!(view.members().generation(), model.rolesets().generation());
                    for entry in view.members().iter() {
                        assert!(entry.roleset().index() < model.rolesets().len());
                    }
                    for (user, want) in &stable {
                        assert_eq!(view.permissions(*user, Scope::Guild).unwrap(), *want);
                    }
                    for (user, id, want) in &stable_views {
                        assert_eq!(view.visible(*user, *id), *want);
                    }
                    for (idx, c) in model.channels().live() {
                        assert_eq!(model.channel_idx(c.id()), Some(idx));
                        let class = model.class_of(idx).unwrap();
                        assert!(model.class(class).unwrap().channels().contains(&idx));
                    }
                    reads.fetch_add(1, Ordering::Relaxed);
                }
            })
        })
        .collect();
    let mut compactions = 0u32;
    let mut slot_compactions = 0u32;
    for cycle in 0..2_000u64 {
        let id = 5_000 + cycle;
        apply(
            &shared,
            &mut reference,
            &[
                create(id, Some(2)),
                Event::MemberUpsert(UserId::new(11), subset(cycle % 64)),
            ],
        );
        let effects = apply(
            &shared,
            &mut reference,
            &[Event::ChannelDelete(ChannelId::new(id))],
        );
        if effects.compaction_due() {
            let done = compact(&shared);
            compactions += 1;
            if done.channel_remap().is_some() {
                slot_compactions += 1;
            }
        }
    }
    stop.store(true, Ordering::Relaxed);
    for reader in readers {
        reader.join().unwrap();
    }
    println!(
        "readers: {} reads during {compactions} compactions ({slot_compactions} of channel slots)",
        reads.load(Ordering::Relaxed)
    );
    assert!(compactions > 100 && slot_compactions > 100);
    assert!(reads.load(Ordering::Relaxed) > 0);
    audited(&shared);
    agrees(&shared, &reference);
}

static RETRY_HOOK_CALLS: AtomicU32 = AtomicU32::new(0);

fn revive_a_dead_role_set(shared: &PermsShared) {
    if RETRY_HOOK_CALLS.fetch_add(1, Ordering::SeqCst) == 0 {
        shared
            .apply(&[Event::MemberUpsert(UserId::new(12), subset(0b11_1000))])
            .unwrap();
    }
}

#[test]
fn compaction_retries_when_a_write_lands_during_the_build() {
    let shared = PermsShared::new(
        guild(),
        Config {
            dead_role_set_floor: 2,
            ..Config::default()
        },
    );
    let snapshot = base_snapshot();
    shared.load(&snapshot).unwrap();
    let mut reference = Reference::new(&snapshot);
    for bits in [
        0b11_1000u64,
        0b10_1000,
        0b01_1000,
        0b11_0000,
        0b10_0000,
        0b01_0000,
        0b00_1000,
        0b00_0100,
        0b11_1100,
    ] {
        apply(
            &shared,
            &mut reference,
            &[Event::MemberUpsert(UserId::new(12), subset(bits))],
        );
    }
    let model = shared.model().unwrap();
    let dead = shared
        .view()
        .unwrap()
        .members()
        .refcounts()
        .iter()
        .take(model.rolesets().len())
        .filter(|c| **c == 0)
        .count();
    assert!(dead >= 8);
    assert!(shared.compaction_due());
    let epoch = shared.epoch();
    shared.fault_on_compaction_swap(Some(revive_a_dead_role_set));
    let done = compact(&shared);
    shared.fault_on_compaction_swap(None);
    reference.apply(&Event::MemberUpsert(UserId::new(12), subset(0b11_1000)));
    assert_eq!(RETRY_HOOK_CALLS.load(Ordering::SeqCst), 2);
    assert_eq!(done.before().epoch(), epoch);
    assert!(done.roleset_remap().is_some());
    let view = shared.view().unwrap();
    let set = view.roleset(UserId::new(12)).unwrap();
    assert_eq!(view.members().refcount(set), 1);
    drop(view);
    audited(&shared);
    agrees(&shared, &reference);
}

static WINDOW_HOOK_CALLS: AtomicU32 = AtomicU32::new(0);

fn write_members_in_the_window(shared: &PermsShared) {
    let call = WINDOW_HOOK_CALLS.fetch_add(1, Ordering::SeqCst);
    if call < 2 {
        let events: Vec<Event> = (0..64u64)
            .map(|k| Event::MemberUpsert(UserId::new(1_000 + k), subset((k + u64::from(call)) % 4)))
            .collect();
        shared.apply(&events).unwrap();
    }
}

#[test]
fn compaction_never_clones_the_member_index_under_the_lock() {
    let shared = PermsShared::new(
        guild(),
        Config {
            dead_role_set_floor: 2,
            ..Config::default()
        },
    );
    let mut snapshot = base_snapshot();
    for user in 1_000..51_000u64 {
        snapshot.members.push((UserId::new(user), subset(user % 4)));
    }
    match shared.load(&snapshot) {
        Ok(_) => {}
        Err(PermsError::NeedsDirty) => {
            shared.load_dirty(&snapshot).unwrap();
        }
        Err(e) => panic!("{e:?}"),
    }
    let mut reference = Reference::new(&snapshot);
    for bits in [
        0b11_1000u64,
        0b10_1000,
        0b01_1000,
        0b11_0000,
        0b10_0000,
        0b01_0000,
        0b00_1000,
        0b00_0100,
        0b11_1100,
    ] {
        apply(
            &shared,
            &mut reference,
            &[Event::MemberUpsert(UserId::new(12), subset(bits))],
        );
    }
    assert!(shared.compaction_due());
    shared.fault_on_compaction_swap(Some(write_members_in_the_window));
    let done = compact(&shared);
    shared.fault_on_compaction_swap(None);
    for call in 0..2u64 {
        for k in 0..64u64 {
            reference.apply(&Event::MemberUpsert(
                UserId::new(1_000 + k),
                subset((k + call) % 4),
            ));
        }
    }
    assert_eq!(WINDOW_HOOK_CALLS.load(Ordering::SeqCst), 3);
    assert!(done.roleset_remap().is_some());
    assert_eq!(shared.fault_member_index_clones(), 0);
    audited(&shared);
    agrees(&shared, &reference);
}
