// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use perm_core::reference::Reference;
use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Config, Event, Hoist, MAX_CANDIDATES, Meter,
    Nsfw, OverwriteSpec, OverwriteTarget, PAIR_MEMO_LIMIT, Permissions, PermsError, PermsShared,
    RankPos, Read, RoleId, RoleList, RoleSpec, Snapshot, Subject, UserId,
};
use support::{GUILD, UNKNOWN_USER, guild};

const R: u64 = 1;
const S: u64 = 2;
const VIEW: Permissions = Permissions::VIEW_CHANNEL;
const NONE: Permissions = Permissions::empty();

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

fn role_allow(id: u64) -> OverwriteSpec {
    OverwriteSpec {
        target: OverwriteTarget::Role(RoleId::new(id)),
        allow: VIEW,
        deny: NONE,
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

fn hidden() -> OverwriteSpec {
    everyone(NONE, VIEW)
}

fn m(user: u64) -> Subject {
    Subject::Member(UserId::new(user))
}

fn n(user: u64) -> Subject {
    Subject::NonMember(UserId::new(user))
}

fn snapshot(
    owner: Option<u64>,
    channels: Vec<ChannelSpec>,
    members: &[(u64, &[u64])],
    virtual_access: &[(u64, u64)],
) -> Snapshot {
    let mut s = Snapshot::new(guild(), owner.map(UserId::new));
    s.roles = vec![role(GUILD, VIEW), role(R, NONE), role(S, NONE)];
    s.channels = channels;
    s.members = members
        .iter()
        .map(|(u, roles)| {
            (
                UserId::new(*u),
                RoleList::Roles(roles.iter().copied().map(RoleId::new).collect()),
            )
        })
        .collect();
    s.virtual_access = virtual_access
        .iter()
        .map(|(u, c)| (UserId::new(*u), vec![ChannelId::new(*c)]))
        .collect();
    s
}

fn configs() -> [Config; 2] {
    [
        Config::default(),
        Config {
            viewable_cache_bytes: 0,
            ..Config::default()
        },
    ]
}

fn load(s: &Snapshot, config: Config) -> std::sync::Arc<PermsShared> {
    let shared = PermsShared::new(guild(), config);
    match shared.load(s) {
        Err(PermsError::NeedsDirty) => shared.load_dirty(s).unwrap(),
        other => other.unwrap(),
    };
    shared
}

fn pool(users: &[u64]) -> Vec<Subject> {
    let mut list: Vec<Subject> = users.iter().map(|u| m(*u)).collect();
    list.extend(users.iter().map(|u| n(*u)));
    list.push(Subject::Visitor);
    list.push(m(UNKNOWN_USER));
    list
}

fn fan_out(s: &Snapshot, target: Subject, candidates: &[Subject], want: &[bool]) {
    let r = Reference::new(s);
    assert_eq!(
        r.filter_sharing(&target, candidates),
        want,
        "reference {target:?}"
    );
    for config in configs() {
        let shared = load(s, config);
        let view = shared.view().unwrap();
        for _ in 0..2 {
            let mut meter = Meter::new();
            let got = view
                .filter_sharing(&target, candidates, &mut meter)
                .unwrap();
            assert_eq!(got, want, "filter_sharing {target:?}");
            assert!(meter.spent() <= view.estimate(Read::FilterSharing(candidates.len())));
        }
        for subject in candidates.iter().chain([&target]) {
            let (_, set) = view.subject_view_set(subject, &mut Meter::new());
            assert_eq!(set.to_vec(), r.view_set(subject), "view set {subject:?}");
        }
    }
}

fn whole_pool(s: &Snapshot, users: &[u64]) {
    let r = Reference::new(s);
    let candidates = pool(users);
    for target in &candidates {
        let want = r.filter_sharing(target, &candidates);
        fan_out(s, *target, &candidates, &want);
    }
}

fn mutual(s: &Snapshot, subscriber: u64, target: u64) -> bool {
    let r = Reference::new(s);
    let shared = load(s, Config::default());
    let view = shared.view().unwrap();
    let (a, b) = (UserId::new(subscriber), UserId::new(target));
    let got = view.shares_view(a, b, &mut Meter::new());
    assert_eq!(got, r.shares_view(a, b), "shares_view {a:?} {b:?}");
    got
}

#[test]
fn a_category_with_a_viewable_child_shares_through_the_category_in_the_fan_out_only() {
    let s = snapshot(
        None,
        vec![
            channel(
                10,
                ChannelKind::Category,
                None,
                vec![hidden(), role_allow(S)],
            ),
            channel(
                11,
                ChannelKind::Text,
                Some(10),
                vec![hidden(), role_allow(R)],
            ),
        ],
        &[(1, &[R]), (2, &[S]), (3, &[R])],
        &[],
    );
    let r = Reference::new(&s);
    assert_eq!(
        r.viewable(UserId::new(1)),
        vec![ChannelId::new(10), ChannelId::new(11)]
    );
    assert_eq!(r.viewable(UserId::new(2)), vec![ChannelId::new(10)]);
    fan_out(&s, m(2), &[m(1), m(3)], &[true, true]);
    fan_out(&s, m(1), &[m(2), m(3)], &[true, true]);
    assert!(!mutual(&s, 1, 2));
    assert!(!mutual(&s, 2, 1));
    assert!(mutual(&s, 1, 3));
    whole_pool(&s, &[1, 2, 3]);
}

#[test]
fn a_category_without_viewable_children_shares_only_in_the_fan_out() {
    let s = snapshot(
        None,
        vec![
            channel(20, ChannelKind::Category, None, vec![]),
            channel(21, ChannelKind::Text, Some(20), vec![hidden()]),
        ],
        &[(1, &[]), (2, &[])],
        &[],
    );
    let r = Reference::new(&s);
    assert_eq!(r.viewable(UserId::new(1)), vec![ChannelId::new(20)]);
    fan_out(&s, m(1), &[m(2), m(1), n(2)], &[true, false, false]);
    assert!(!mutual(&s, 1, 2));
    assert!(!mutual(&s, 2, 1));
    whole_pool(&s, &[1, 2]);
}

#[test]
fn a_child_seen_only_through_virtual_access_shares_its_parent() {
    let s = snapshot(
        None,
        vec![
            channel(30, ChannelKind::Category, None, vec![hidden()]),
            channel(31, ChannelKind::Text, Some(30), vec![hidden()]),
            channel(
                32,
                ChannelKind::Text,
                Some(30),
                vec![hidden(), role_allow(R)],
            ),
        ],
        &[(1, &[]), (2, &[]), (3, &[R]), (5, &[])],
        &[(1, 31), (4, 31)],
    );
    let r = Reference::new(&s);
    assert_eq!(
        r.viewable(UserId::new(1)),
        vec![ChannelId::new(31), ChannelId::new(30)]
    );
    assert_eq!(
        r.viewable(UserId::new(3)),
        vec![ChannelId::new(30), ChannelId::new(32)]
    );
    fan_out(
        &s,
        m(1),
        &[m(2), m(3), m(4), n(4), m(5)],
        &[false, true, false, false, false],
    );
    fan_out(&s, m(4), &[m(1), m(3)], &[false, false]);
    assert!(!mutual(&s, 1, 3));
    assert!(!mutual(&s, 3, 1));
    whole_pool(&s, &[1, 2, 3, 4, 5]);
}

#[test]
fn two_children_of_one_category_share_only_through_the_category() {
    let s = snapshot(
        None,
        vec![
            channel(40, ChannelKind::Category, None, vec![hidden()]),
            channel(
                41,
                ChannelKind::Text,
                Some(40),
                vec![hidden(), role_allow(R)],
            ),
            channel(
                42,
                ChannelKind::Voice,
                Some(40),
                vec![hidden(), role_allow(S)],
            ),
            channel(43, ChannelKind::Text, None, vec![hidden()]),
        ],
        &[(1, &[R]), (2, &[S]), (3, &[])],
        &[],
    );
    fan_out(&s, m(1), &[m(2), m(3)], &[true, false]);
    fan_out(&s, m(3), &[m(1), m(2)], &[false, false]);
    assert!(!mutual(&s, 1, 2));
    assert!(!mutual(&s, 2, 1));
    whole_pool(&s, &[1, 2, 3]);
}

#[test]
fn two_children_seen_through_virtual_access_share_only_the_appended_parent() {
    let s = snapshot(
        None,
        vec![
            channel(45, ChannelKind::Category, None, vec![hidden()]),
            channel(46, ChannelKind::Text, Some(45), vec![hidden()]),
            channel(47, ChannelKind::Voice, Some(45), vec![hidden()]),
        ],
        &[(1, &[]), (2, &[]), (3, &[])],
        &[(1, 46), (2, 47)],
    );
    let r = Reference::new(&s);
    assert_eq!(
        r.viewable(UserId::new(1)),
        vec![ChannelId::new(46), ChannelId::new(45)]
    );
    assert_eq!(
        r.viewable(UserId::new(2)),
        vec![ChannelId::new(47), ChannelId::new(45)]
    );
    fan_out(&s, m(1), &[m(2), m(3)], &[true, false]);
    fan_out(&s, m(2), &[m(1), m(3)], &[true, false]);
    assert!(!mutual(&s, 1, 2));
    assert!(!mutual(&s, 2, 1));
    whole_pool(&s, &[1, 2, 3]);
}

#[test]
fn visitors_hold_the_everyone_view_without_adult_channels() {
    let mut adult = channel(53, ChannelKind::Text, None, vec![]);
    adult.nsfw = Nsfw::Adult;
    let mut s = snapshot(
        None,
        vec![
            channel(51, ChannelKind::Text, None, vec![]),
            channel(52, ChannelKind::Text, None, vec![hidden(), role_allow(R)]),
            adult,
        ],
        &[(1, &[]), (2, &[R]), (3, &[])],
        &[],
    );
    s.channels[0].overwrites = vec![member(3, NONE, VIEW)];
    s.channels[2].overwrites = vec![member(1, NONE, VIEW), member(2, NONE, VIEW)];
    let r = Reference::new(&s);
    assert_eq!(r.view_ids(&Subject::Visitor), vec![ChannelId::new(51)]);
    fan_out(
        &s,
        m(1),
        &[Subject::Visitor, Subject::Visitor, m(2)],
        &[true, true, true],
    );
    fan_out(&s, m(3), &[Subject::Visitor, m(1)], &[false, false]);
    fan_out(
        &s,
        Subject::Visitor,
        &[m(1), Subject::Visitor],
        &[false, false],
    );
    whole_pool(&s, &[1, 2, 3]);
}

#[test]
fn an_owner_who_is_not_a_member_has_the_empty_set() {
    let s = snapshot(
        Some(9),
        vec![
            channel(60, ChannelKind::Text, None, vec![]),
            channel(61, ChannelKind::Text, None, vec![hidden()]),
        ],
        &[(1, &[]), (2, &[])],
        &[(9, 61)],
    );
    let r = Reference::new(&s);
    assert!(r.view_set(&m(9)).iter().all(|w| *w == 0));
    fan_out(&s, m(1), &[m(9), n(9), m(2)], &[false, false, true]);
    fan_out(&s, m(9), &[m(1), m(2)], &[false, false]);
    fan_out(&s, n(1), &[m(2)], &[false]);
    whole_pool(&s, &[1, 2, 9]);
}

#[test]
fn an_owner_who_is_a_member_sees_every_channel() {
    let s = snapshot(
        Some(1),
        vec![
            channel(70, ChannelKind::Category, None, vec![hidden()]),
            channel(
                71,
                ChannelKind::Text,
                Some(70),
                vec![hidden(), role_allow(R)],
            ),
        ],
        &[(1, &[]), (2, &[R]), (3, &[])],
        &[],
    );
    fan_out(&s, m(1), &[m(2), m(3)], &[true, false]);
    fan_out(&s, m(2), &[m(1)], &[true]);
    whole_pool(&s, &[1, 2, 3]);
}

#[test]
fn member_overwrites_give_a_per_user_set() {
    let s = snapshot(
        None,
        vec![
            channel(80, ChannelKind::Text, None, vec![member(1, NONE, VIEW)]),
            channel(
                81,
                ChannelKind::Text,
                None,
                vec![hidden(), member(1, VIEW, NONE)],
            ),
            channel(82, ChannelKind::Text, None, vec![hidden(), role_allow(R)]),
        ],
        &[(1, &[]), (2, &[]), (3, &[R]), (4, &[R])],
        &[],
    );
    fan_out(&s, m(1), &[m(2), m(3)], &[false, false]);
    fan_out(&s, m(2), &[m(1), m(3), m(4)], &[false, true, true]);
    whole_pool(&s, &[1, 2, 3, 4]);
    let shared = load(&s, Config::default());
    let view = shared.view().unwrap();
    let mut meter = Meter::new();
    let one = view.view_set_id(&m(1), &mut meter);
    let two = view.view_set_id(&m(2), &mut meter);
    assert!(one.is_some() && two.is_some() && one != two);
    assert_eq!(
        view.view_set_id(&m(3), &mut meter),
        view.view_set_id(&m(4), &mut meter)
    );
    assert_eq!(view.view_set_id(&n(3), &mut meter), None);
}

#[test]
fn unknown_users_and_the_target_itself_never_share() {
    let s = snapshot(
        None,
        vec![channel(90, ChannelKind::Text, None, vec![])],
        &[(1, &[]), (2, &[])],
        &[],
    );
    let unknown = m(UNKNOWN_USER);
    fan_out(
        &s,
        m(1),
        &[unknown, m(1), n(1), m(2), m(2)],
        &[false, false, false, true, true],
    );
    fan_out(&s, unknown, &[m(1), m(2)], &[false, false]);
    fan_out(&s, m(1), &[], &[]);
}

#[test]
fn candidates_are_bounded_at_4096() {
    let users: Vec<u64> = (1..=40).collect();
    let pairs: Vec<(u64, &[u64])> = users.iter().map(|u| (*u, &[][..])).collect();
    let s = snapshot(
        None,
        vec![channel(100, ChannelKind::Text, None, vec![])],
        &pairs,
        &[],
    );
    let shared = load(&s, Config::default());
    let view = shared.view().unwrap();
    let r = Reference::new(&s);
    let full: Vec<Subject> = (0..MAX_CANDIDATES)
        .map(|i| m(1 + (i as u64) % 50))
        .collect();
    let mut meter = Meter::new();
    let got = view.filter_sharing(&m(1), &full, &mut meter).unwrap();
    assert_eq!(got.len(), MAX_CANDIDATES);
    assert_eq!(got, r.filter_sharing(&m(1), &full));
    assert!(meter.spent() <= view.estimate(Read::FilterSharing(full.len())));
    let mut over = full.clone();
    over.push(m(2));
    assert!(matches!(
        view.filter_sharing(&m(1), &over, &mut Meter::new()),
        Err(PermsError::BadInput(_))
    ));
    assert!(view.view_sets_audit().memo.len() <= PAIR_MEMO_LIMIT);
}

#[test]
fn the_pair_memo_stays_within_its_bound_and_matches_fresh_results() {
    let users: Vec<u64> = (1..=120).collect();
    let mut channels = Vec::new();
    for u in &users {
        channels.push(channel(
            1000 + u,
            ChannelKind::Text,
            None,
            vec![
                hidden(),
                member(*u, VIEW, NONE),
                member(u % 120 + 1, VIEW, NONE),
            ],
        ));
    }
    let pairs: Vec<(u64, &[u64])> = users.iter().map(|u| (*u, &[][..])).collect();
    let s = snapshot(None, channels, &pairs, &[]);
    let shared = load(&s, Config::default());
    let view = shared.view().unwrap();
    let r = Reference::new(&s);
    let candidates: Vec<Subject> = users.iter().map(|u| m(*u)).collect();
    for target in &candidates {
        let got = view
            .filter_sharing(target, &candidates, &mut Meter::new())
            .unwrap();
        assert_eq!(got, r.filter_sharing(target, &candidates), "{target:?}");
    }
    let audit = view.view_sets_audit();
    assert_eq!(audit.memo.len(), PAIR_MEMO_LIMIT);
    for ((a, b), hit) in &audit.memo {
        let x = audit.sets.get(a.index()).unwrap();
        let y = audit.sets.get(b.index()).unwrap();
        assert_eq!(*hit, x.iter().zip(y.iter()).any(|(p, q)| p & q != 0));
    }
}

#[test]
fn view_sets_and_the_memo_start_empty_in_each_epoch() {
    let s = snapshot(
        None,
        vec![
            channel(110, ChannelKind::Text, None, vec![]),
            channel(111, ChannelKind::Text, None, vec![hidden(), role_allow(R)]),
        ],
        &[(1, &[]), (2, &[R])],
        &[],
    );
    let shared = load(&s, Config::default());
    {
        let view = shared.view().unwrap();
        let got = view
            .filter_sharing(&m(1), &[m(2)], &mut Meter::new())
            .unwrap();
        assert_eq!(got, vec![true]);
        assert!(!view.view_sets_audit().memo.is_empty());
    }
    let mut changed = s.channels[0].clone();
    changed.overwrites = vec![hidden()];
    let effects = shared
        .apply(&[Event::ChannelUpsert(changed, ChannelCause::Updated)])
        .unwrap();
    assert!(effects.delta().viewsets_changed);
    let view = shared.view().unwrap();
    assert_eq!(view.view_sets_audit(), Default::default());
    let got = view
        .filter_sharing(&m(1), &[m(2)], &mut Meter::new())
        .unwrap();
    assert_eq!(got, vec![false]);
}

#[test]
fn the_delta_signals_view_set_changes() {
    let s = snapshot(
        Some(1),
        vec![channel(120, ChannelKind::Text, None, vec![])],
        &[(1, &[]), (2, &[])],
        &[],
    );
    let shared = load(&s, Config::default());
    let added = shared
        .apply(&[Event::MemberUpsert(UserId::new(3), RoleList::Roles(vec![]))])
        .unwrap();
    assert!(!added.delta().viewsets_changed);
    let owner = shared.apply(&[Event::Owner(Some(UserId::new(2)))]).unwrap();
    assert!(owner.delta().viewsets_changed);
    let access = shared
        .apply(&[Event::VirtualAccess(
            UserId::new(2),
            vec![ChannelId::new(120)],
        )])
        .unwrap();
    assert!(access.delta().viewsets_changed);
    let order = shared
        .apply(&[
            Event::ChannelUpsert(
                channel(121, ChannelKind::Text, None, vec![]),
                ChannelCause::Created,
            ),
            Event::ChannelOrder(vec![ChannelId::new(121), ChannelId::new(120)]),
        ])
        .unwrap();
    assert!(order.delta().viewsets_changed);
}
