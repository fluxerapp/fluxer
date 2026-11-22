// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Event, Hoist, Nsfw, OverwriteSpec,
    OverwriteTarget, Permissions, RankPos, RoleId, RoleList, RoleSpec, Snapshot, UserId,
};
use support::{Driver, GUILD, Op, Params, apply_step, check_effects, check_view, guild};

fn channel(id: u64, visible: bool) -> ChannelSpec {
    ChannelSpec {
        id: ChannelId::new(id),
        kind: ChannelKind::Text,
        parent: None,
        overwrites: if visible {
            vec![]
        } else {
            vec![OverwriteSpec {
                target: OverwriteTarget::Role(RoleId::new(GUILD)),
                allow: Permissions::empty(),
                deny: Permissions::VIEW_CHANNEL,
            }]
        },
        nsfw: Nsfw::Safe,
        named: true,
    }
}

#[test]
fn reinserted_channels_report_only_net_visibility() {
    for old_visible in [false, true] {
        for new_visible in [false, true] {
            for repetitions in [1, 3] {
                let params = Params {
                    roles: 1,
                    channels: 2,
                    members: 1,
                    seed: 1,
                    budget: 250_000,
                    floor: 4096,
                    cache: 1 << 20,
                };
                let mut snapshot = Snapshot::new(guild(), None);
                snapshot.roles.push(RoleSpec {
                    id: RoleId::new(GUILD),
                    permissions: Permissions::VIEW_CHANNEL,
                    position: RankPos::Int(0),
                    hoist: Hoist::NotHoisted,
                });
                snapshot.channels = vec![channel(7, old_visible), channel(8, true)];
                snapshot
                    .members
                    .push((UserId::new(1), RoleList::Roles(vec![])));
                let mut driver = Driver::start(&params, Some(&snapshot));
                let mut ops = Vec::new();
                for i in 0..repetitions {
                    ops.push(Op::Raw(Event::ChannelDelete(ChannelId::new(7))));
                    ops.push(Op::Raw(Event::ChannelUpsert(
                        channel(
                            7,
                            if i + 1 == repetitions {
                                new_visible
                            } else {
                                !old_visible
                            },
                        ),
                        ChannelCause::Created,
                    )));
                }
                ops.push(Op::Raw(Event::ChannelOrder(vec![
                    ChannelId::new(8),
                    ChannelId::new(7),
                ])));
                let (before, effects) = apply_step(&mut driver, &ops);
                check_effects(&driver, &before, &effects);
                check_view(&driver);
                if old_visible == new_visible {
                    assert!(effects.changes().is_empty());
                } else {
                    assert!(!effects.changes().is_empty());
                    for change in effects.changes() {
                        assert_eq!(
                            change.lost,
                            if old_visible {
                                vec![ChannelId::new(7)]
                            } else {
                                vec![]
                            }
                        );
                        assert_eq!(
                            change.gained,
                            if new_visible {
                                vec![ChannelId::new(7)]
                            } else {
                                vec![]
                            }
                        );
                    }
                }
            }
        }
    }
}

#[test]
fn retained_channel_reinsert_fuzz_case_matches_reference() {
    let data = include_bytes!("seeds/diff/regression-channel-reinsert");
    let case = support::bytes::decode(data).bounded(60_000);
    let mut driver = case.driver();
    for ops in &case.steps {
        let (before, effects) = apply_step(&mut driver, ops);
        check_effects(&driver, &before, &effects);
        check_view(&driver);
        support::maybe_compact(&mut driver, &effects);
    }
}
