// SPDX-License-Identifier: AGPL-3.0-or-later

mod support;

use std::sync::Arc;

use perm_core::reference::row_diff;
use perm_core::{
    ChannelId, ChannelKind, ChannelSpec, Config, Delta, Event, Hoist, Model, Nsfw, OverwriteSpec,
    OverwriteTarget, Permissions, PermsShared, RankPos, RoleId, RoleList, RoleSetId, RoleSpec,
    Snapshot, UserId, Visible,
};
use support::{GUILD, check_class_rows, guild};

const ROLES: u64 = 12;

fn everyone_perms() -> Permissions {
    Permissions::VIEW_CHANNEL | Permissions::READ_MESSAGE_HISTORY | Permissions::SEND_MESSAGES
}

fn roles(everyone: Permissions, first: Permissions) -> Vec<RoleSpec> {
    let mut specs = vec![RoleSpec {
        id: RoleId::new(GUILD),
        permissions: everyone,
        position: RankPos::Int(0),
        hoist: Hoist::NotHoisted,
    }];
    for r in 1..=ROLES {
        specs.push(RoleSpec {
            id: RoleId::new(r),
            permissions: if r == 1 {
                first
            } else {
                Permissions::SEND_MESSAGES
            },
            position: RankPos::Int(i64::try_from(r).unwrap()),
            hoist: Hoist::NotHoisted,
        });
    }
    specs
}

fn channel(i: u64) -> ChannelSpec {
    let overwrites = if i.is_multiple_of(2) {
        vec![
            OverwriteSpec {
                target: OverwriteTarget::Role(RoleId::new(GUILD)),
                allow: Permissions::empty(),
                deny: Permissions::VIEW_CHANNEL,
            },
            OverwriteSpec {
                target: OverwriteTarget::Role(RoleId::new(i % (ROLES - 1) + 2)),
                allow: Permissions::VIEW_CHANNEL,
                deny: Permissions::empty(),
            },
        ]
    } else {
        vec![]
    };
    ChannelSpec {
        id: ChannelId::new(3000 + i),
        kind: ChannelKind::Text,
        parent: None,
        overwrites,
        nsfw: Nsfw::Inherit,
        named: true,
    }
}

fn shared(first: Permissions) -> Arc<PermsShared> {
    let mut s = Snapshot::new(guild(), Some(UserId::new(1)));
    s.roles = roles(everyone_perms(), first);
    s.channels = (0..12).map(channel).collect();
    for n in 0..ROLES * ROLES * ROLES {
        let a = RoleId::new(n % ROLES + 1);
        let b = RoleId::new((n / ROLES) % ROLES + 1);
        let c = RoleId::new((n / ROLES / ROLES) % ROLES + 1);
        s.members
            .push((UserId::new(100 + n), RoleList::Roles(vec![a, b, c])));
    }
    let shared = PermsShared::new(guild(), Config::default());
    shared.load(&s).unwrap();
    shared
}

fn edit(shared: &PermsShared, everyone: Permissions, first: Permissions) -> (Delta, Arc<Model>) {
    let effects = shared
        .apply(&[Event::Roles(roles(everyone, first))])
        .unwrap();
    let delta = effects.delta();
    assert_eq!(delta, perm_core::diff(effects.before(), effects.after()));
    check_class_rows(effects.before(), effects.after(), &delta);
    (delta, effects.after().clone())
}

fn holding_first(model: &Model) -> Vec<RoleSetId> {
    let first = model.roles().resolve(RoleId::new(1)).unwrap();
    model
        .rolesets()
        .iter()
        .filter(|(_, set)| set.contains(first))
        .map(|(id, _)| id)
        .collect()
}

#[test]
fn history_flip_is_one_toggle_per_class() {
    let shared = shared(Permissions::SEND_MESSAGES);
    let before = shared.model().unwrap();
    let rows = before.rolesets().len();
    assert!(rows > 128);
    let (delta, after) = edit(
        &shared,
        everyone_perms() - Permissions::READ_MESSAGE_HISTORY,
        Permissions::SEND_MESSAGES,
    );
    assert_eq!(delta.classes.len(), before.classes().iter().count());
    for change in &delta.classes {
        assert_eq!(change.toggled, Visible::HISTORY);
        assert!(change.flipped.is_empty() && change.overrides.is_empty());
        let expanded = change.expanded(&after);
        assert_eq!(expanded, row_diff(&before, &after, change.class));
        assert_eq!(expanded.len(), rows);
        for (_, a, b) in &expanded {
            assert!(a.contains(Visible::HISTORY) && !b.contains(Visible::HISTORY));
            assert_eq!(*a - Visible::HISTORY, *b);
        }
    }
    let (back, restored) = edit(&shared, everyone_perms(), Permissions::SEND_MESSAGES);
    for change in &back.classes {
        assert_eq!(change.toggled, Visible::HISTORY);
        assert!(change.flipped.is_empty());
        for (_, a, b) in change.expanded(&restored) {
            assert!(!a.contains(Visible::HISTORY) && b.contains(Visible::HISTORY));
        }
    }
}

#[test]
fn rows_that_keep_history_are_listed_against_the_toggle() {
    let shared = shared(Permissions::SEND_MESSAGES | Permissions::READ_MESSAGE_HISTORY);
    let before = shared.model().unwrap();
    let rows = before.rolesets().len();
    let keeping = holding_first(&before);
    assert!(!keeping.is_empty() && keeping.len() * 2 < rows);
    let (delta, after) = edit(
        &shared,
        everyone_perms() - Permissions::READ_MESSAGE_HISTORY,
        Permissions::SEND_MESSAGES | Permissions::READ_MESSAGE_HISTORY,
    );
    assert!(!delta.classes.is_empty());
    for change in &delta.classes {
        assert_eq!(change.toggled, Visible::HISTORY);
        let listed: Vec<_> = change.flipped.iter().map(|(set, _, _)| *set).collect();
        assert_eq!(listed, keeping);
        assert!(change.flipped.iter().all(|(_, a, b)| a == b));
        let expanded = change.expanded(&after);
        assert_eq!(expanded.len(), rows - keeping.len());
        assert!(expanded.iter().all(|(set, _, _)| !keeping.contains(set)));
    }
}

#[test]
fn minority_flips_stay_per_row() {
    let shared = shared(Permissions::SEND_MESSAGES);
    let before = shared.model().unwrap();
    let (delta, after) = edit(
        &shared,
        everyone_perms(),
        Permissions::SEND_MESSAGES | Permissions::VIEW_CHANNEL_MEMBERS,
    );
    assert!(!delta.classes.is_empty());
    for change in &delta.classes {
        assert!(change.toggled.is_empty());
        assert!(!change.flipped.is_empty());
        for (set, a, b) in &change.flipped {
            assert!(holding_first(&before).contains(set));
            assert_eq!(*a ^ *b, Visible::VIEW_MEMBERS);
        }
        assert_eq!(change.expanded(&after), change.flipped);
    }
}

#[test]
fn majority_view_flip_lists_the_sets_that_keep_their_view() {
    let viewing = Permissions::SEND_MESSAGES | Permissions::VIEW_CHANNEL;
    let shared_view = self::shared(viewing);
    let before = shared_view.model().unwrap();
    let rows = before.rolesets().len();
    let keeping = holding_first(&before);
    let (delta, after) = edit(
        &shared_view,
        everyone_perms() - Permissions::VIEW_CHANNEL,
        viewing,
    );
    let open = before.class_of_id(ChannelId::new(3001)).unwrap();
    let change = delta.classes.iter().find(|c| c.class == open).unwrap();
    assert_eq!(change.toggled, Visible::VIEW);
    let listed: Vec<_> = change.flipped.iter().map(|(set, _, _)| *set).collect();
    assert_eq!(listed, keeping);
    assert!(change.flipped.iter().all(|(_, a, b)| a == b));
    let expanded = change.expanded(&after);
    assert_eq!(expanded, row_diff(&before, &after, open));
    assert_eq!(expanded.len() + keeping.len(), rows);
}
