// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use proptest::prelude::*;

use crate::support::{self, Op, Setup};

fn name_index() -> impl Strategy<Value = u8> {
    0u8..(support::NAMES.len() as u8)
}

fn roles() -> impl Strategy<Value = Vec<u8>> {
    prop_oneof![
        24 => proptest::collection::vec(0u8..9, 0..4),
        1 => Just(vec![support::MALFORMED]),
    ]
}

fn ranges() -> impl Strategy<Value = Vec<(u16, u8)>> {
    proptest::collection::vec((0u16..45, 0u8..110), 0..4)
}

pub fn op() -> impl Strategy<Value = Op> {
    prop_oneof![
        8 => (0u8..24, name_index(), roles(), any::<bool>()).prop_map(|(user, name, roles, unchanged)| Op::Upsert { user, name, roles, unchanged }),
        2 => (0u8..24).prop_map(|user| Op::Remove { user }),
        12 => (0u8..24, 0u8..6, any::<bool>(), any::<bool>()).prop_map(|(user, status, unchanged, deferred)| Op::Presence { user, status, unchanged, deferred }),
        1 => (0u8..24).prop_map(|user| Op::Cleared { user }),
        6 => (0u8..24, any::<bool>()).prop_map(|(user, changed)| Op::Connect { user, changed }),
        4 => (0u8..24).prop_map(|user| Op::Disconnect { user }),
        4 => (0u8..8, 0u8..8, 0u8..6, proptest::option::of(0u8..6), 0u8..10).prop_map(|(role, perms, position, hoist, extra)| Op::Role { role, perms, position, hoist, delete: extra == 0 }),
        3 => (0u8..8, proptest::option::of(0u8..2), proptest::collection::vec((any::<bool>(), 0u8..24, 0u8..4, 0u8..4), 0..3), 0u8..12).prop_map(|(channel, parent, overwrites, extra)| Op::Channel { channel, parent, overwrites, delete: extra == 0 }),
        1 => (0u8..26).prop_map(|user| Op::Owner { user }),
        1 => (0u8..24, proptest::collection::vec(0u8..8, 0..3)).prop_map(|(user, channels)| Op::Virtual { user, channels }),
        10 => (0u8..7, 0u8..24, 0u8..8, ranges()).prop_map(|(session, user, channel, ranges)| Op::Subscribe { session, user, channel, ranges }),
        2 => (0u8..7, 0u8..24, 0u8..8, 0u16..12, 2u16..10).prop_map(|(session, user, channel, first, gap)| Op::Widen { session, user, channel, first, gap }),
        2 => (0u8..7, 0u8..8).prop_map(|(session, channel)| Op::Unsubscribe { session, channel }),
        2 => (0u8..7).prop_map(|session| Op::Drop { session }),
        2 => (0u8..7, 0u8..8).prop_map(|(session, channel)| Op::ViewGained { session, channel }),
        5 => Just(Op::Flush),
        1 => any::<bool>().prop_map(|on| Op::Updates { on }),
        3 => Just(Op::Join),
    ]
}

pub fn setup() -> impl Strategy<Value = Setup> {
    (
        proptest::collection::vec(
            (0u8..24, name_index(), roles(), 0u8..6, any::<bool>()),
            0..20,
        ),
        proptest::collection::vec((0u8..8, 0u8..6, proptest::option::of(0u8..6)), 0..8),
        proptest::collection::vec(
            (
                0u8..8,
                proptest::option::of(0u8..2),
                proptest::collection::vec((any::<bool>(), 0u8..24, 0u8..4, 0u8..4), 0..3),
            ),
            1..8,
        ),
        0u8..4,
        0u8..4,
        0u8..9,
    )
        .prop_map(
            |(members, roles, channels, threshold, budget, fragments)| Setup {
                members,
                roles,
                channels,
                threshold,
                budget,
                fragments,
            },
        )
}
