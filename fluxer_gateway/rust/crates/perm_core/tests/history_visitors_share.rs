// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use perm_core::reference::{RefNeed, Reference};
use perm_core::{
    ChannelId, ChannelKind, ChannelSpec, Config, Cutoff, Hoist, Meter, Need, Nsfw, OverwriteSpec,
    OverwriteTarget, Permissions, PermsShared, RankPos, RoleId, RoleList, RoleSetId, RoleSpec,
    Snapshot, UserId,
};
use support::{GUILD, guild};

const R: u64 = 1;

fn role(id: u64, permissions: Permissions) -> RoleSpec {
    RoleSpec {
        id: RoleId::new(id),
        permissions,
        position: RankPos::Int(0),
        hoist: Hoist::NotHoisted,
    }
}

fn everyone(allow: Permissions, deny: Permissions) -> OverwriteSpec {
    OverwriteSpec {
        target: OverwriteTarget::Role(RoleId::new(GUILD)),
        allow,
        deny,
    }
}

fn member(user: u64, allow: Permissions, deny: Permissions) -> OverwriteSpec {
    OverwriteSpec {
        target: OverwriteTarget::Member(UserId::new(user)),
        allow,
        deny,
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

fn load(s: &Snapshot) -> std::sync::Arc<PermsShared> {
    let shared = PermsShared::new(guild(), Config::default());
    shared.load(s).unwrap();
    shared
}

fn ids(list: &[u64]) -> Vec<UserId> {
    list.iter().copied().map(UserId::new).collect()
}

const VIEW: Permissions = Permissions::VIEW_CHANNEL;
const HISTORY: Permissions = Permissions::READ_MESSAGE_HISTORY;
const NONE: Permissions = Permissions::empty();

fn history_guild() -> Snapshot {
    let mut s = Snapshot::new(guild(), None);
    s.roles = vec![role(GUILD, VIEW), role(R, HISTORY)];
    s.channels = vec![
        channel(
            1,
            ChannelKind::Text,
            None,
            vec![member(3, HISTORY, NONE), member(4, NONE, HISTORY | VIEW)],
        ),
        channel(2, ChannelKind::Text, None, vec![everyone(NONE, VIEW)]),
    ];
    s.members = vec![
        (UserId::new(1), RoleList::Roles(vec![])),
        (UserId::new(2), RoleList::Roles(vec![RoleId::new(R)])),
        (UserId::new(3), RoleList::Roles(vec![])),
        (UserId::new(4), RoleList::Roles(vec![RoleId::new(R)])),
        (UserId::new(5), RoleList::Malformed),
    ];
    s.virtual_access = vec![(UserId::new(9), vec![ChannelId::new(1)])];
    s.cutoff = Cutoff::AtMillis(1000);
    s
}

fn sets(shared: &PermsShared) -> (RoleSetId, RoleSetId) {
    let view = shared.view().unwrap();
    let plain = view.group_of(UserId::new(1));
    let held = view.group_of(UserId::new(2));
    match (plain, held) {
        (Some(perm_core::Group::RoleSet(a)), Some(perm_core::Group::RoleSet(b))) => (a, b),
        other => panic!("groups {other:?}"),
    }
}

fn history(shared: &PermsShared, channel: u64, ms: i64) -> (bool, bool, Vec<UserId>) {
    let (plain, held) = sets(shared);
    let view = shared.view().unwrap();
    let mut meter = Meter::new();
    let audience = view
        .audience(ChannelId::new(channel), Need::History(ms), &mut meter)
        .unwrap();
    (
        audience.has_set(plain),
        audience.has_set(held),
        audience.users().to_vec(),
    )
}

fn reference_agrees(s: &Snapshot, channel: u64, ms: i64) {
    let r = Reference::new(s);
    let shared = load(s);
    let (plain, held, users) = history(&shared, channel, ms);
    let id = ChannelId::new(channel);
    assert_eq!(plain, r.roles_in_audience(&[], id, RefNeed::History(ms)));
    assert_eq!(
        held,
        r.roles_in_audience(&[RoleId::new(R)], id, RefNeed::History(ms))
    );
    let want: Vec<UserId> = [3, 4, 5, 9]
        .into_iter()
        .map(UserId::new)
        .filter(|u| r.in_audience(*u, id, RefNeed::History(ms)))
        .collect();
    assert_eq!(users, want);
}

#[test]
fn history_before_the_cutoff_follows_the_history_bit() {
    let shared = load(&history_guild());
    assert_eq!(history(&shared, 1, 999), (false, true, ids(&[3])));
    assert_eq!(history(&shared, 1, i64::MIN), (false, true, ids(&[3])));
    reference_agrees(&history_guild(), 1, 999);
}

#[test]
fn history_at_and_after_the_cutoff_admits_every_role_set() {
    let shared = load(&history_guild());
    assert_eq!(history(&shared, 1, 1000), (true, true, ids(&[3, 4, 9])));
    assert_eq!(history(&shared, 1, 5000), (true, true, ids(&[3, 4, 9])));
    reference_agrees(&history_guild(), 1, 1000);
    reference_agrees(&history_guild(), 1, 5000);
}

#[test]
fn history_needs_no_view() {
    let shared = load(&history_guild());
    assert_eq!(history(&shared, 2, 999), (false, true, ids(&[4])));
    let view = shared.view().unwrap();
    let mut meter = Meter::new();
    let seen = view
        .audience(ChannelId::new(2), Need::View, &mut meter)
        .unwrap();
    let (_, held) = sets(&shared);
    assert!(!seen.has_set(held));
    reference_agrees(&history_guild(), 2, 999);
}

#[test]
fn history_without_a_cutoff_never_opens() {
    for cutoff in [Cutoff::Missing, Cutoff::Malformed] {
        let mut s = history_guild();
        s.cutoff = cutoff;
        let shared = load(&s);
        assert_eq!(history(&shared, 1, i64::MAX), (false, true, ids(&[3])));
        reference_agrees(&s, 1, i64::MAX);
    }
}

#[test]
fn history_on_an_unknown_channel_answers_like_a_known_one() {
    let shared = load(&history_guild());
    let view = shared.view().unwrap();
    for ms in [999, 1000] {
        let mut meter = Meter::new();
        assert!(
            view.audience(ChannelId::new(77), Need::History(ms), &mut meter)
                .is_ok()
        );
    }
}

#[test]
fn history_for_an_admin_role_set_ignores_the_cutoff() {
    let mut s = history_guild();
    s.roles.push(role(2, Permissions::ADMINISTRATOR));
    s.members
        .push((UserId::new(6), RoleList::Roles(vec![RoleId::new(2)])));
    let shared = load(&s);
    let view = shared.view().unwrap();
    let Some(perm_core::Group::RoleSet(admin)) = view.group_of(UserId::new(6)) else {
        panic!("admin group");
    };
    let mut meter = Meter::new();
    let audience = view
        .audience(ChannelId::new(2), Need::History(0), &mut meter)
        .unwrap();
    assert!(audience.has_set(admin));
}

fn visitor_guild() -> Snapshot {
    let mut s = Snapshot::new(guild(), None);
    s.roles = vec![role(
        GUILD,
        VIEW | Permissions::ADMINISTRATOR | Permissions::SEND_MESSAGES,
    )];
    let mut adult = channel(3, ChannelKind::Text, None, vec![]);
    adult.nsfw = Nsfw::Adult;
    s.channels = vec![
        channel(1, ChannelKind::Text, None, vec![everyone(HISTORY, NONE)]),
        channel(2, ChannelKind::Text, None, vec![everyone(NONE, VIEW)]),
        adult,
    ];
    s
}

#[test]
fn visitor_permissions_on_an_unknown_channel_are_empty() {
    let s = visitor_guild();
    let r = Reference::new(&s);
    let shared = load(&s);
    let view = shared.view().unwrap();
    for id in [77, 99_999] {
        let id = ChannelId::new(id);
        assert_eq!(view.visitor_permissions(id), Permissions::empty());
        assert_eq!(r.visitor_permissions(id), Permissions::empty());
    }
}

#[test]
fn visitor_permissions_follow_the_everyone_overwrite_without_nsfw() {
    let s = visitor_guild();
    let r = Reference::new(&s);
    let shared = load(&s);
    let view = shared.view().unwrap();
    for (id, want) in [(1, VIEW | HISTORY), (2, NONE), (3, VIEW)] {
        let id = ChannelId::new(id);
        assert_eq!(view.visitor_permissions(id), want, "{id:?}");
        assert_eq!(r.visitor_permissions(id), want, "{id:?}");
    }
    let mut meter = Meter::new();
    assert_eq!(
        view.visitor_channels(false, &mut meter),
        vec![(ChannelId::new(1), VIEW | HISTORY)]
    );
}

fn share_guild(channels: Vec<ChannelSpec>, virtual_access: &[(u64, u64)]) -> Snapshot {
    let mut s = Snapshot::new(guild(), None);
    s.roles = vec![role(GUILD, VIEW)];
    s.channels = channels;
    for u in 1..=3 {
        s.members.push((UserId::new(u), RoleList::Roles(vec![])));
    }
    s.virtual_access = virtual_access
        .iter()
        .map(|(u, c)| (UserId::new(*u), vec![ChannelId::new(*c)]))
        .collect();
    s
}

fn shares(s: &Snapshot, pairs: &[(u64, u64, bool)]) {
    let r = Reference::new(s);
    let shared = load(s);
    let view = shared.view().unwrap();
    let mut meter = Meter::new();
    for (a, b, want) in pairs {
        let (a, b) = (UserId::new(*a), UserId::new(*b));
        assert_eq!(view.shares_view(a, b, &mut meter), *want, "{a:?} {b:?}");
        assert_eq!(r.shares_view(a, b), *want, "reference {a:?} {b:?}");
    }
    let users = ids(&[1, 2, 3, 4]);
    for a in &users {
        let many = view.shares_view_many(*a, &users, &mut meter);
        let want: Vec<bool> = users.iter().map(|b| r.shares_view(*a, *b)).collect();
        assert_eq!(many, want, "many {a:?}");
    }
}

#[test]
fn a_category_with_a_viewable_child_shares_only_through_the_child() {
    let s = share_guild(
        vec![
            channel(10, ChannelKind::Category, None, vec![everyone(NONE, VIEW)]),
            channel(
                11,
                ChannelKind::Text,
                Some(10),
                vec![everyone(VIEW, NONE), member(3, NONE, VIEW)],
            ),
        ],
        &[],
    );
    let r = Reference::new(&s);
    assert_eq!(
        r.viewable(UserId::new(1)),
        vec![ChannelId::new(10), ChannelId::new(11)]
    );
    assert!(r.viewable(UserId::new(3)).is_empty());
    shares(
        &s,
        &[
            (1, 2, true),
            (2, 1, true),
            (1, 3, false),
            (3, 1, false),
            (1, 4, false),
        ],
    );
}

#[test]
fn a_category_without_viewable_children_never_shares() {
    let s = share_guild(
        vec![
            channel(20, ChannelKind::Category, None, vec![everyone(VIEW, NONE)]),
            channel(21, ChannelKind::Text, Some(20), vec![everyone(NONE, VIEW)]),
        ],
        &[],
    );
    let r = Reference::new(&s);
    assert_eq!(r.viewable(UserId::new(1)), vec![ChannelId::new(20)]);
    assert_eq!(r.viewable(UserId::new(2)), vec![ChannelId::new(20)]);
    shares(&s, &[(1, 2, false), (2, 1, false), (1, 1, false)]);
}

#[test]
fn a_child_seen_only_through_virtual_access_shares_only_with_its_viewers() {
    let s = share_guild(
        vec![
            channel(30, ChannelKind::Category, None, vec![everyone(NONE, VIEW)]),
            channel(31, ChannelKind::Text, Some(30), vec![everyone(NONE, VIEW)]),
        ],
        &[(1, 31), (3, 31), (4, 31)],
    );
    let r = Reference::new(&s);
    assert_eq!(
        r.viewable(UserId::new(1)),
        vec![ChannelId::new(31), ChannelId::new(30)]
    );
    assert!(r.viewable(UserId::new(2)).is_empty());
    shares(
        &s,
        &[
            (1, 3, true),
            (3, 1, true),
            (1, 2, false),
            (2, 1, false),
            (1, 4, false),
            (4, 1, false),
        ],
    );
}
