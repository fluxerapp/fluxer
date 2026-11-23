// SPDX-License-Identifier: AGPL-3.0-or-later

use std::hint::black_box;
use std::sync::Arc;
use std::time::Instant;

use criterion::Criterion;
use perm_core::{
    ChannelId, ChannelKind, ChannelSpec, Config, GuildId, Hoist, Meter, Nsfw, OverwriteSpec,
    OverwriteTarget, Permissions, PermsError, PermsShared, PushPrune, RankPos, RoleId, RoleList,
    RoleSpec, Snapshot, UserId,
};
use push_core::{MaskKind, PushMasks};

const GUILD: u64 = 1_400_000_000_000_000_000;
const MEMBERS: u32 = 50_000;
const ROLES: u64 = 50;
const CATEGORIES: u64 = 10;
const CHANNELS: u64 = 200;
const UPDATE_MAX_NS: f64 = 1_000.0;
const QUERY_MAX_NS: f64 = 20_000.0;
const UNMENTIONED_MAX_NS: f64 = 50_000.0;

fn role(i: u64) -> RoleId {
    RoleId::new(1_410_000_000_000_000_000 + i)
}

fn channel(i: u64) -> ChannelId {
    ChannelId::new(1_420_000_000_000_000_000 + i)
}

fn user(slot: u32) -> UserId {
    UserId::new(1_430_000_000_000_000_000 + u64::from(slot) * 7_919)
}

fn splitmix(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

fn masks(slots: u32, seed: u64) -> PushMasks {
    let mut state = seed;
    let mut masks = PushMasks::with_capacity(slots);
    for slot in 0..slots {
        let roll = splitmix(&mut state) % 100;
        masks.set(slot, MaskKind::AllMessages, roll < 8);
        masks.set(slot, MaskKind::Uncertain, (8..12).contains(&roll));
        masks.set(slot, MaskKind::Muted, splitmix(&mut state) % 100 < 3);
        masks.set(slot, MaskKind::Active, splitmix(&mut state) % 100 < 15);
    }
    masks
}

fn snapshot(members: u32) -> Snapshot {
    let mut s = Snapshot::new(GuildId::new(GUILD), Some(user(0)));
    s.roles = std::iter::once(RoleSpec {
        id: RoleId::new(GUILD),
        permissions: Permissions::VIEW_CHANNEL
            | Permissions::READ_MESSAGE_HISTORY
            | Permissions::SEND_MESSAGES,
        position: RankPos::Int(0),
        hoist: Hoist::NotHoisted,
    })
    .chain((1..ROLES).map(|i| RoleSpec {
        id: role(i),
        permissions: Permissions::SEND_MESSAGES,
        position: RankPos::Int(i as i64),
        hoist: Hoist::NotHoisted,
    }))
    .collect();
    s.channels = (0..CATEGORIES)
        .map(|i| ChannelSpec {
            id: channel(i),
            kind: ChannelKind::Category,
            parent: None,
            overwrites: Vec::new(),
            nsfw: Nsfw::Inherit,
            named: true,
        })
        .chain((CATEGORIES..CHANNELS).map(|i| {
            let overwrites = if i % 2 == 0 {
                vec![
                    OverwriteSpec {
                        target: OverwriteTarget::Role(RoleId::new(GUILD)),
                        allow: Permissions::empty(),
                        deny: Permissions::VIEW_CHANNEL,
                    },
                    OverwriteSpec {
                        target: OverwriteTarget::Role(role(1 + i % (ROLES - 1))),
                        allow: Permissions::VIEW_CHANNEL,
                        deny: Permissions::empty(),
                    },
                ]
            } else {
                Vec::new()
            };
            ChannelSpec {
                id: channel(i),
                kind: ChannelKind::Text,
                parent: Some(channel(i % CATEGORIES)),
                overwrites,
                nsfw: Nsfw::Inherit,
                named: true,
            }
        }))
        .collect();
    let mut state = 11;
    for slot in 0..members {
        let roles = (0..splitmix(&mut state) % 3)
            .map(|_| role(1 + splitmix(&mut state) % (ROLES - 1)))
            .collect();
        s.members.push((user(slot), RoleList::Roles(roles)));
    }
    s
}

fn load(snapshot: &Snapshot) -> Arc<PermsShared> {
    let shared = PermsShared::new(GuildId::new(GUILD), Config::default());
    match shared.load(snapshot) {
        Ok(_) => {}
        Err(PermsError::NeedsDirty) => {
            shared.load_dirty(snapshot).expect("dirty load");
        }
        Err(error) => panic!("{error:?}"),
    }
    shared
}

struct Fixture {
    shared: Arc<PermsShared>,
    masks: PushMasks,
    slot_users: Vec<UserId>,
    channel: ChannelId,
}

fn fixture() -> Fixture {
    Fixture {
        shared: load(&snapshot(MEMBERS)),
        masks: masks(MEMBERS, 7),
        slot_users: (0..MEMBERS).map(user).collect(),
        channel: channel(CATEGORIES + 2),
    }
}

struct Scratch {
    slots: Vec<u32>,
    candidates: Vec<UserId>,
}

fn unmentioned(f: &Fixture, scratch: &mut Scratch) -> (usize, usize) {
    let view = f.shared.view().expect("view");
    let mut meter = Meter::new();
    let pruned = view.push_prune(
        &PushPrune {
            channel: f.channel,
            mentioned: &[],
            roles: &[],
            everyone: false,
        },
        &mut meter,
    );
    let mut pass = |query: fn(&PushMasks, &mut Vec<u32>), scratch: &mut Scratch| {
        query(&f.masks, &mut scratch.slots);
        scratch.candidates.clear();
        scratch.candidates.extend(
            scratch
                .slots
                .iter()
                .filter_map(|slot| f.slot_users.get(*slot as usize).copied()),
        );
        view.filter_viewers(f.channel, &scratch.candidates, &mut meter)
            .len()
    };
    let immediate = pass(PushMasks::immediate, &mut *scratch);
    let held = pass(PushMasks::held, &mut *scratch);
    (pruned.immediate.len() + immediate, pruned.held.len() + held)
}

fn median_ns(runs: usize, mut f: impl FnMut(usize)) -> f64 {
    let mut samples: Vec<f64> = (0..runs)
        .map(|n| {
            let start = Instant::now();
            f(n);
            start.elapsed().as_nanos() as f64
        })
        .collect();
    samples.sort_by(f64::total_cmp);
    samples.get(samples.len() / 2).copied().unwrap_or(f64::NAN)
}

fn update(masks: &mut PushMasks, n: usize) {
    let slot = ((n as u64).wrapping_mul(7_919) % u64::from(MEMBERS)) as u32;
    let on = n.is_multiple_of(2);
    masks.set(slot, MaskKind::AllMessages, on);
    masks.set(slot, MaskKind::Uncertain, !on);
    masks.set(slot, MaskKind::Muted, false);
    masks.set(slot, MaskKind::Active, on);
}

fn report(failed: &mut u32, ok: bool, line: String) {
    if !ok {
        *failed += 1;
    }
    println!("  {} {line}", if ok { "met   " } else { "MISSED" });
}

fn main() {
    let f = fixture();
    let mut updated = masks(MEMBERS, 7);
    let mut scratch = Scratch {
        slots: Vec::with_capacity(MEMBERS as usize),
        candidates: Vec::with_capacity(MEMBERS as usize),
    };
    let mut immediate = Vec::with_capacity(MEMBERS as usize);
    let mut held = Vec::with_capacity(MEMBERS as usize);

    let mut c = Criterion::default().configure_from_args();
    let mut n = 0usize;
    c.bench_function("push_mask_update", |b| {
        b.iter(|| {
            n = n.wrapping_add(1);
            update(&mut updated, black_box(n));
        })
    });
    c.bench_function("push_mask_query_50k", |b| {
        b.iter(|| {
            f.masks.immediate(&mut immediate);
            f.masks.held(&mut held);
            black_box((immediate.len(), held.len()))
        })
    });
    c.bench_function("push_unmentioned_50k", |b| {
        b.iter(|| black_box(unmentioned(&f, &mut scratch)))
    });
    c.final_summary();

    let mut failed = 0;
    println!("\ntarget check (median against target):");
    let update_ns = median_ns(100_001, |n| update(&mut updated, black_box(n)));
    report(
        &mut failed,
        update_ns <= UPDATE_MAX_NS,
        format!(
            "push_mask_update: {:.3} us, target {:.3} us",
            update_ns / 1e3,
            UPDATE_MAX_NS / 1e3
        ),
    );
    let query_ns = median_ns(2_001, |_| {
        f.masks.immediate(&mut immediate);
        f.masks.held(&mut held);
        black_box((immediate.len(), held.len()));
    });
    report(
        &mut failed,
        query_ns <= QUERY_MAX_NS,
        format!(
            "push_mask_query_50k: {:.3} us for {} immediate and {} held, target {:.3} us",
            query_ns / 1e3,
            immediate.len(),
            held.len(),
            QUERY_MAX_NS / 1e3
        ),
    );
    let unmentioned_ns = median_ns(2_001, |_| {
        black_box(unmentioned(&f, &mut scratch));
    });
    let (sent, waiting) = unmentioned(&f, &mut scratch);
    report(
        &mut failed,
        unmentioned_ns <= UNMENTIONED_MAX_NS,
        format!(
            "push_unmentioned_50k: {:.3} us, {sent} immediate and {waiting} held candidates pass VIEW, target {:.3} us",
            unmentioned_ns / 1e3,
            UNMENTIONED_MAX_NS / 1e3
        ),
    );
    println!("  masks hold {} bytes for {MEMBERS} slots", f.masks.bytes());
    println!("\ncriterion targets: {failed} missed");
    if failed > 0 {
        std::process::exit(1);
    }
}
