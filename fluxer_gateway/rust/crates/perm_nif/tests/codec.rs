// SPDX-License-Identifier: AGPL-3.0-or-later

use perm_core::{
    ChannelKind, Cutoff, Event, Hoist, Need, Nsfw, OverwriteTarget, Permissions, RankPos, RoleId,
    RoleList, Scope, UserId,
};
use perm_nif::decode::{Decoder, Name, Query, ReplyQuery, Shape, Source, snapshot_items};

#[derive(Debug)]
enum Mock {
    Atom(&'static str),
    Int(i128),
    Tuple(Vec<Mock>),
    Cons(Box<Mock>, Box<Mock>),
    Nil,
    Bin(Vec<u8>),
}

impl Source for &Mock {
    fn shape(self) -> Shape {
        match self {
            Mock::Atom(_) => Shape::Atom,
            Mock::Int(_) => Shape::Integer,
            Mock::Tuple(_) => Shape::Tuple,
            Mock::Cons(..) => Shape::List,
            Mock::Nil => Shape::Nil,
            Mock::Bin(_) => Shape::Binary,
        }
    }

    fn is(self, name: Name) -> bool {
        matches!(self, Mock::Atom(text) if *text == name.text())
    }

    fn uint(self) -> Option<u64> {
        match self {
            Mock::Int(v) => u64::try_from(*v).ok(),
            _ => None,
        }
    }

    fn int(self) -> Option<i64> {
        match self {
            Mock::Int(v) => i64::try_from(*v).ok(),
            _ => None,
        }
    }

    fn elements(self) -> Option<Vec<Self>> {
        match self {
            Mock::Tuple(items) => Some(items.iter().collect()),
            _ => None,
        }
    }

    fn cell(self) -> Option<(Self, Self)> {
        match self {
            Mock::Cons(head, tail) => Some((head.as_ref(), tail.as_ref())),
            _ => None,
        }
    }

    fn words(self) -> Option<Vec<u64>> {
        match self {
            Mock::Bin(bytes) if bytes.len().is_multiple_of(8) => Some(
                bytes
                    .as_chunks::<8>()
                    .0
                    .iter()
                    .map(|c| u64::from_be_bytes(*c))
                    .collect(),
            ),
            _ => None,
        }
    }
}

fn a(text: &'static str) -> Mock {
    Mock::Atom(text)
}

fn i(value: i128) -> Mock {
    Mock::Int(value)
}

fn t(items: Vec<Mock>) -> Mock {
    Mock::Tuple(items)
}

fn l(items: Vec<Mock>) -> Mock {
    items.into_iter().rev().fold(Mock::Nil, |tail, head| {
        Mock::Cons(Box::new(head), Box::new(tail))
    })
}

fn some(value: Mock) -> Mock {
    t(vec![a("some"), value])
}

fn packed(ids: &[u64]) -> Mock {
    t(vec![
        a("packed_ids"),
        Mock::Bin(ids.iter().flat_map(|id| id.to_be_bytes()).collect()),
    ])
}

#[test]
fn scopes_and_needs_decode() {
    let mut d = Decoder::new();
    assert_eq!(d.scope(&a("guild_scope")).unwrap(), Scope::Guild);
    let scope = t(vec![a("channel_scope"), i(7)]);
    assert!(matches!(d.scope(&scope).unwrap(), Scope::Channel(c) if c.get() == 7));
    let opaque = t(vec![a("channel_scope"), t(vec![a("channel_id"), i(9)])]);
    assert!(matches!(d.scope(&opaque).unwrap(), Scope::Channel(c) if c.get() == 9));
    assert_eq!(d.need(&a("view_members")).unwrap(), Need::ViewMembers);
    assert_eq!(
        d.need(&t(vec![a("message_access"), i(-5)])).unwrap(),
        Need::MessageAccess(-5)
    );
    assert!(d.scope(&a("elsewhere")).is_err());
}

#[test]
fn packed_ids_and_lists_both_decode() {
    let mut d = Decoder::new();
    assert_eq!(
        d.users(&packed(&[3, 1 << 63]), 10, "users").unwrap(),
        vec![UserId::new(3), UserId::new(1 << 63)]
    );
    assert_eq!(
        d.users(&l(vec![i(4), i(5)]), 10, "users").unwrap(),
        vec![UserId::new(4), UserId::new(5)]
    );
    assert!(d.users(&packed(&[0]), 10, "users").is_err());
    assert!(d.users(&packed(&[1, 2, 3]), 2, "users").is_err());
    assert!(
        d.users(&t(vec![a("packed_ids"), Mock::Bin(vec![1, 2, 3])]), 10, "u")
            .is_err()
    );
}

fn role_record() -> Mock {
    t(vec![
        a("perm_role"),
        i(1),
        Mock::Bin(b"everyone".to_vec()),
        i(1024),
        t(vec![a("position"), i(0)]),
        t(vec![a("hoisted"), a("none")]),
    ])
}

fn channel_record() -> Mock {
    t(vec![
        a("perm_channel"),
        i(10),
        i(0),
        some(i(11)),
        i(3),
        l(vec![t(vec![
            a("perm_overwrite"),
            t(vec![a("role_target"), i(1)]),
            i(0),
            i(1024),
        ])]),
        a("inherit_nsfw"),
        a("true"),
        a("true"),
    ])
}

#[test]
fn snapshot_records_decode_positionally() {
    let snapshot = t(vec![
        a("perm_snapshot"),
        i(1),
        some(i(50)),
        l(vec![role_record()]),
        l(vec![channel_record()]),
        l(vec![
            t(vec![a("perm_member"), i(50), l(vec![i(1)])]),
            t(vec![a("perm_member"), i(51), a("malformed")]),
        ]),
        l(vec![t(vec![i(52), l(vec![i(10)])])]),
        t(vec![a("cutoff_at"), i(1000)]),
        a("none"),
        i(3),
    ]);
    let s = Decoder::new().snapshot(&snapshot).unwrap();
    assert_eq!(s.owner, Some(UserId::new(50)));
    assert_eq!(s.roles.len(), 1);
    assert_eq!(s.roles[0].permissions, Permissions::VIEW_CHANNEL);
    assert_eq!(s.roles[0].position, RankPos::Int(0));
    assert_eq!(s.roles[0].hoist, Hoist::Hoisted(None));
    let channel = &s.channels[0];
    assert_eq!(channel.kind, ChannelKind::Text);
    assert_eq!(channel.parent.map(|c| c.get()), Some(11));
    assert_eq!(channel.nsfw, Nsfw::Adult);
    assert!(channel.named);
    assert_eq!(
        channel.overwrites[0].target,
        OverwriteTarget::Role(RoleId::new(1))
    );
    assert_eq!(s.members[0].1, RoleList::Roles(vec![RoleId::new(1)]));
    assert_eq!(s.members[1].1, RoleList::Malformed);
    assert_eq!(s.virtual_access[0].0, UserId::new(52));
    assert_eq!(s.cutoff, Cutoff::AtMillis(1000));
    assert_eq!(s.nsfw, None);
    assert_eq!(s.nsfw_level, 3);
}

fn plain_channel(id: i128, nsfw: &'static str) -> Mock {
    t(vec![
        a("perm_channel"),
        i(id),
        i(0),
        a("none"),
        l(vec![]),
        a(nsfw),
    ])
}

fn ids_of(roles: &RoleList) -> Vec<u64> {
    match roles {
        RoleList::Roles(ids) => ids.iter().map(|r| r.get()).collect(),
        RoleList::Malformed => Vec::new(),
    }
}

#[test]
fn event_snapshots_keep_the_last_write_per_key() {
    let events = l(vec![
        t(vec![
            a("channel_upsert"),
            plain_channel(10, "inherit_nsfw"),
            a("created"),
        ]),
        t(vec![
            a("channel_upsert"),
            plain_channel(11, "inherit_nsfw"),
            a("created"),
        ]),
        t(vec![
            a("channel_upsert"),
            plain_channel(12, "inherit_nsfw"),
            a("created"),
        ]),
        t(vec![
            a("channel_upsert"),
            plain_channel(10, "adult_only"),
            a("updated"),
        ]),
        t(vec![a("channel_delete"), i(12)]),
        t(vec![
            a("channel_upsert"),
            plain_channel(13, "inherit_nsfw"),
            a("created"),
        ]),
        t(vec![
            a("channel_order"),
            l(vec![i(13), i(13), i(99), i(11)]),
        ]),
        t(vec![a("member_upsert"), i(5), l(vec![i(1)])]),
        t(vec![a("member_upsert"), i(6), l(vec![i(1)])]),
        t(vec![a("member_upsert"), i(7), l(vec![])]),
        t(vec![a("member_upsert"), i(5), l(vec![i(2)])]),
        t(vec![a("member_remove"), i(6)]),
        t(vec![a("member_upsert"), i(6), l(vec![i(3)])]),
        t(vec![a("virtual_access"), i(7), l(vec![i(10)])]),
        t(vec![a("virtual_access"), i(8), l(vec![i(11)])]),
        t(vec![a("virtual_access"), i(7), l(vec![])]),
        t(vec![a("virtual_access"), i(8), l(vec![i(13)])]),
    ]);
    let s = Decoder::new()
        .snapshot(&t(vec![a("snapshot"), i(1), events]))
        .unwrap();
    let channels: Vec<(u64, Nsfw)> = s.channels.iter().map(|c| (c.id.get(), c.nsfw)).collect();
    assert_eq!(
        channels,
        vec![(13, Nsfw::Inherit), (11, Nsfw::Inherit), (10, Nsfw::Adult)]
    );
    let members: Vec<(u64, Vec<u64>)> = s
        .members
        .iter()
        .map(|(u, roles)| (u.get(), ids_of(roles)))
        .collect();
    assert_eq!(members, vec![(5, vec![2]), (7, vec![]), (6, vec![3])]);
    let virtual_access: Vec<(u64, Vec<u64>)> = s
        .virtual_access
        .iter()
        .map(|(u, view)| (u.get(), view.iter().map(|c| c.get()).collect()))
        .collect();
    assert_eq!(virtual_access, vec![(8, vec![13])]);
}

#[test]
fn snapshot_items_counts_top_level_entries_up_to_a_bound() {
    let members = |n: i128| {
        l((1..=n)
            .map(|u| t(vec![a("member_upsert"), i(u), l(vec![])]))
            .collect())
    };
    let events = t(vec![a("snapshot"), i(1), members(10)]);
    assert_eq!(snapshot_items(&events, 100), 10);
    assert_eq!(snapshot_items(&events, 3), 4);
    let record = t(vec![
        a("perm_snapshot"),
        i(1),
        a("none"),
        l(vec![role_record()]),
        l(vec![channel_record()]),
        l((1..=5)
            .map(|u| t(vec![a("perm_member"), i(u), l(vec![])]))
            .collect()),
        l(vec![t(vec![i(52), l(vec![i(10)])])]),
        a("no_cutoff"),
        a("none"),
        i(0),
    ]);
    assert_eq!(snapshot_items(&record, 100), 8);
    assert_eq!(snapshot_items(&record, 2), 3);
    assert_eq!(snapshot_items(&a("junk"), 100), 0);
}

#[test]
fn events_decode_and_reject_bad_tags() {
    let mut d = Decoder::new();
    let events = l(vec![
        t(vec![
            a("member_upsert"),
            i(5),
            t(vec![a("roles"), l(vec![i(2)])]),
        ]),
        t(vec![a("channel_upsert"), channel_record(), a("created")]),
        t(vec![a("history_cutoff"), a("no_cutoff")]),
        t(vec![a("guild_nsfw"), a("true"), i(0)]),
        t(vec![a("owner"), a("none")]),
    ]);
    let decoded = d.events(&events, 64).unwrap();
    assert_eq!(decoded.len(), 5);
    assert!(matches!(decoded[2], Event::HistoryCutoff(Cutoff::Missing)));
    assert!(matches!(decoded[3], Event::GuildNsfw(Some(true), 0)));
    assert!(d.event(&t(vec![a("unknown"), i(1)])).is_err());
    let many = l((0..65)
        .map(|n| t(vec![a("member_remove"), i(n + 1)]))
        .collect());
    assert!(d.events(&many, 64).is_err());
}

#[test]
fn queries_decode() {
    let mut d = Decoder::new();
    let prune = t(vec![
        a("push_prune"),
        i(10),
        i(99),
        packed(&[5]),
        l(vec![i(2)]),
        a("true"),
    ]);
    let Query::PushPrune(q) = d.query(&prune).unwrap() else {
        panic!("not a prune");
    };
    assert_eq!(q.mentioned, vec![UserId::new(5)]);
    assert!(q.everyone);
    assert!(matches!(
        d.query(&t(vec![a("group_of"), i(2)])).unwrap(),
        Query::GroupOf(u) if u == UserId::new(2)
    ));
    assert!(
        d.query(&t(vec![a("role_set_members"), l(vec![i(2)])]))
            .is_err()
    );
    let reply = t(vec![a("check_permission"), i(5), a("guild_scope"), i(8)]);
    assert!(matches!(
        d.reply_query(&reply).unwrap(),
        ReplyQuery::CheckPermission(_, Scope::Guild, bits) if bits == Permissions::ADMINISTRATOR
    ));
}
