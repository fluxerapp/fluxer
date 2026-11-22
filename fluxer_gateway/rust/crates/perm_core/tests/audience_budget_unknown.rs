// SPDX-License-Identifier: AGPL-3.0-or-later

use perm_core::{
    ChannelId, ChannelKind, ChannelSpec, Config, GuildId, Hoist, Meter, Need, Nsfw, OverwriteSpec,
    OverwriteTarget, Permissions, PermsShared, RankPos, Read, RoleId, RoleList, RoleSpec, Snapshot,
    UserId,
};

fn assert_audience_cost(snapshot: &Snapshot) {
    let shared = PermsShared::new(snapshot.guild, Config::default());
    shared.load_dirty(snapshot).unwrap();
    let view = shared.view().unwrap();
    let estimate = view.estimate(Read::Audience);
    let unknown = ChannelId::new(999);
    for channel in [unknown, ChannelId::new(1)] {
        for need in [
            Need::View,
            Need::ViewMembers,
            Need::MessageAccess(0),
            Need::History(0),
        ] {
            let mut meter = Meter::new();
            view.audience(channel, need, &mut meter).unwrap();
            assert!(
                meter.spent() <= estimate,
                "{channel:?} {need:?}: spent {:?}, estimate {:?}",
                meter.spent(),
                estimate
            );
        }
    }
}

#[test]
fn audience_on_an_unknown_channel_stays_within_the_estimate() {
    assert_audience_cost(&Snapshot::new(GuildId::new(1000), None));
}

#[test]
fn audience_estimate_covers_sets_and_exceptions() {
    let mut snapshot = Snapshot::new(GuildId::new(1000), Some(UserId::new(1)));
    snapshot.roles = (1..=4)
        .map(|id| RoleSpec {
            id: RoleId::new(id),
            permissions: Permissions::VIEW_CHANNEL | Permissions::READ_MESSAGE_HISTORY,
            position: RankPos::Int(id as i64),
            hoist: Hoist::NotHoisted,
        })
        .collect();
    snapshot.members = (0..16)
        .map(|mask| {
            (
                UserId::new(mask + 1),
                RoleList::Roles(
                    (0..4)
                        .filter(|bit| mask & (1 << bit) != 0)
                        .map(|bit| RoleId::new(bit + 1))
                        .collect(),
                ),
            )
        })
        .collect();
    snapshot
        .members
        .push((UserId::new(99), RoleList::Malformed));
    snapshot
        .virtual_access
        .push((UserId::new(98), vec![ChannelId::new(1)]));
    snapshot.channels.push(ChannelSpec {
        id: ChannelId::new(1),
        kind: ChannelKind::Text,
        parent: None,
        nsfw: Nsfw::Inherit,
        named: true,
        overwrites: vec![OverwriteSpec {
            target: OverwriteTarget::Member(UserId::new(2)),
            allow: Permissions::VIEW_CHANNEL,
            deny: Permissions::READ_MESSAGE_HISTORY,
        }],
    });
    assert_audience_cost(&snapshot);
}
