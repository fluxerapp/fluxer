// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};

use perm_core::{
    ChannelKind, ChannelSpec, Config, Event, GuildId, Hoist, Meter, MetricsSink, Nsfw, Permissions,
    PermsShared, PushPrune, RankPos, RoleId, RoleList, RoleSpec, Snapshot, UserId,
};

const GUILD: u64 = 1_000;

fn snapshot(members: u64) -> Snapshot {
    let mut s = Snapshot::new(GuildId::new(GUILD), None);
    s.roles = (0..6u64)
        .map(|i| RoleSpec {
            id: RoleId::new(if i == 0 { GUILD } else { 2_000 + i }),
            permissions: if i == 0 {
                Permissions::VIEW_CHANNEL
            } else {
                Permissions::SEND_MESSAGES
            },
            position: RankPos::Int(i as i64),
            hoist: Hoist::NotHoisted,
        })
        .collect();
    s.channels = vec![ChannelSpec {
        id: perm_core::ChannelId::new(3_000),
        kind: ChannelKind::Text,
        parent: None,
        overwrites: Vec::new(),
        nsfw: Nsfw::Inherit,
        named: true,
    }];
    for n in 1..=members {
        let roles = vec![RoleId::new(2_000 + 1 + n % 5)];
        s.members.push((UserId::new(n), RoleList::Roles(roles)));
    }
    s
}

fn naive(shared: &PermsShared, role: RoleId) -> Vec<UserId> {
    let view = shared.view().unwrap();
    let model = view.model().clone();
    let idx = model.roles().resolve(role).unwrap();
    let mut out: Vec<UserId> = view
        .members()
        .iter()
        .filter(|e| !e.is_quarantined())
        .filter(|e| model.rolesets().get(e.roleset()).unwrap().contains(idx))
        .map(|e| e.user())
        .collect();
    out.sort_unstable();
    out
}

#[test]
fn role_set_members_follow_member_churn() {
    let shared = PermsShared::new(GuildId::new(GUILD), Config::default());
    shared.load(&snapshot(200)).unwrap();
    for step in 0..300u64 {
        let user = UserId::new(1 + (step * 37) % 260);
        let event = match step % 4 {
            0 => Event::MemberRemove(user),
            1 => Event::MemberUpsert(user, RoleList::Malformed),
            _ => Event::MemberUpsert(
                user,
                RoleList::Roles(vec![RoleId::new(2_001 + step % 5), RoleId::new(2_003)]),
            ),
        };
        shared.apply_dirty(&[event]).unwrap();
        let view = shared.view().unwrap();
        for r in 1..=5u64 {
            let role = RoleId::new(2_000 + r);
            let fast = view.role_set_members(&[role], &mut Meter::new());
            assert_eq!(fast, naive(&shared, role), "step {step} role {r}");
        }
    }
}

#[test]
fn push_prune_splits_mentions_from_wide_viewers() {
    let shared = PermsShared::new(GuildId::new(GUILD), Config::default());
    shared.load(&snapshot(20)).unwrap();
    let view = shared.view().unwrap();
    let channel = perm_core::ChannelId::new(3_000);
    let none = view.push_prune(
        &PushPrune {
            channel,
            mentioned: &[],
            roles: &[],
            everyone: false,
        },
        &mut Meter::new(),
    );
    assert!(none.immediate.is_empty() && none.held.is_empty());
    let mentioned = [UserId::new(3), UserId::new(99)];
    let roles = [RoleId::new(2_002)];
    let pruned = view.push_prune(
        &PushPrune {
            channel,
            mentioned: &mentioned,
            roles: &roles,
            everyone: true,
        },
        &mut Meter::new(),
    );
    assert!(pruned.immediate.contains(&UserId::new(3)));
    assert!(!pruned.immediate.contains(&UserId::new(99)));
    assert!(pruned.immediate.windows(2).all(|w| w[0] < w[1]));
    assert!(pruned.held.windows(2).all(|w| w[0] < w[1]));
    assert!(pruned.held.iter().all(|u| !pruned.immediate.contains(u)));
    assert_eq!(pruned.immediate.len() + pruned.held.len(), 20);
}

#[test]
fn push_prune_keeps_a_role_mentioned_owner_in_a_hidden_channel() {
    let owner = UserId::new(7);
    let mut s = snapshot(20);
    s.owner = Some(owner);
    let channel = perm_core::ChannelId::new(3_000);
    s.channels = vec![ChannelSpec {
        id: channel,
        kind: ChannelKind::Text,
        parent: None,
        overwrites: vec![perm_core::OverwriteSpec {
            target: perm_core::OverwriteTarget::Role(RoleId::new(GUILD)),
            allow: Permissions::empty(),
            deny: Permissions::VIEW_CHANNEL,
        }],
        nsfw: Nsfw::Inherit,
        named: true,
    }];
    let shared = PermsShared::new(GuildId::new(GUILD), Config::default());
    shared.load(&s).unwrap();
    let view = shared.view().unwrap();
    for r in 1..=5u64 {
        let role = RoleId::new(2_000 + r);
        let pruned = view.push_prune(
            &PushPrune {
                channel,
                mentioned: &[UserId::new(1), UserId::new(2)],
                roles: &[role],
                everyone: false,
            },
            &mut Meter::new(),
        );
        let want: Vec<UserId> = naive(&shared, role)
            .into_iter()
            .filter(|user| *user == owner)
            .collect();
        assert_eq!(pruned.immediate, want, "role {r}");
    }
}

#[test]
fn try_view_reads_when_no_writer_holds_the_index() {
    let shared = PermsShared::new(GuildId::new(GUILD), Config::default());
    shared.load(&snapshot(10)).unwrap();
    assert!(shared.try_view().is_ok());
}

#[derive(Default)]
struct Counting {
    bytes: AtomicU64,
    live: AtomicU64,
}

impl MetricsSink for Counting {
    fn opened(&self) {
        self.live.fetch_add(1, Ordering::Relaxed);
    }

    fn closed(&self) {
        self.live.fetch_sub(1, Ordering::Relaxed);
    }

    fn bytes_moved(&self, old: u64, new: u64) {
        self.bytes.fetch_add(new, Ordering::Relaxed);
        self.bytes.fetch_sub(old, Ordering::Relaxed);
    }
}

#[test]
fn metrics_return_to_zero_after_drop() {
    let sink = Arc::new(Counting::default());
    let shared = PermsShared::with_sink(GuildId::new(GUILD), Config::default(), sink.clone());
    shared.load(&snapshot(50)).unwrap();
    assert_eq!(sink.live.load(Ordering::Relaxed), 1);
    assert!(sink.bytes.load(Ordering::Relaxed) > 0);
    drop(shared);
    assert_eq!(sink.live.load(Ordering::Relaxed), 0);
    assert_eq!(sink.bytes.load(Ordering::Relaxed), 0);
}
