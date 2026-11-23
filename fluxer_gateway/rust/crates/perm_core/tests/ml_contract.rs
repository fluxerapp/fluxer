// SPDX-License-Identifier: AGPL-3.0-or-later

#[path = "support/ml_contract.rs"]
mod shadow;
mod support;

use perm_core::{Config, Event, PermsShared, UserId};
use shadow::{MemberListShadow, class_rows_from_sums, recount, top_hoist};
use support::naive::{Rng, World};

fn touched(events: &[Event]) -> Vec<UserId> {
    events
        .iter()
        .filter_map(|e| match e {
            Event::MemberUpsert(u, _) | Event::MemberRemove(u) => Some(*u),
            _ => None,
        })
        .collect()
}

#[test]
fn columns_follow_diff_and_match_role_set_sums() {
    for seed in 1..=200 {
        let mut rng = Rng::new(seed);
        let (roles, channels, members) = (1 + rng.below(10), 1 + rng.below(20), 1 + rng.below(80));
        let mut world = World::random(&mut rng, roles, channels, members);
        let shared = PermsShared::new(World::guild(), Config::default());
        shared.load(&world.snapshot()).unwrap();
        let shadow = MemberListShadow::new(shared.clone()).unwrap();
        shadow.adopt_all().unwrap();
        for _ in 0..40 {
            let count = 1 + rng.below(3);
            let events: Vec<Event> = (0..count).map(|_| world.random_event(&mut rng)).collect();
            shared.apply(&events).unwrap();
            shadow.apply(&touched(&events)).unwrap();
            let classes: Vec<_> = shared
                .model()
                .unwrap()
                .classes()
                .iter()
                .map(|(c, _)| c)
                .collect();
            if let Some(class) = classes.into_iter().find(|c| shadow.column(*c).is_none())
                && rng.chance(50)
            {
                shadow.subscribe(class).unwrap();
            }
            let view = shared.view().unwrap();
            for (class, rows) in shadow.columns() {
                assert!(view.model().class(class).is_some(), "seed {seed}");
                assert_eq!(rows, recount(&view, class), "seed {seed} class {class:?}");
                assert_eq!(
                    rows,
                    class_rows_from_sums(&view, class),
                    "seed {seed} class {class:?}"
                );
            }
            for (set, _) in view.model().rolesets().iter() {
                if let Some(idx) = top_hoist(view.model(), set) {
                    let role = view.model().roles().get(idx).unwrap();
                    assert!(role.is_present());
                }
            }
        }
    }
}
