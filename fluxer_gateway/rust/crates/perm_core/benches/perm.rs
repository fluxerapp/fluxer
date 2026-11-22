// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeSet;
use std::hint::black_box;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Barrier};
use std::time::{Duration, Instant};

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use perm_core::{
    ChannelCause, ChannelId, ChannelKind, ChannelSpec, Config, Event, GuildId, Hoist, Meter, Need,
    Nsfw, OverwriteSpec, OverwriteTarget, Permissions, PermsError, PermsShared, PushPrune, RankPos,
    RoleId, RoleList, RoleSpec, Scope, Snapshot, Subject, UserId,
};

const GUILD: u64 = 1_400_000_000_000_000_000;
const MESSAGE_MS: i64 = 1_700_000_000_000;

fn role(i: u64) -> RoleId {
    RoleId::new(1_410_000_000_000_000_000 + i)
}

fn channel(i: u64) -> ChannelId {
    ChannelId::new(1_420_000_000_000_000_000 + i)
}

fn user(i: u64) -> UserId {
    UserId::new(1_430_000_000_000_000_000 + i)
}

fn splitmix(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

fn base() -> Permissions {
    Permissions::VIEW_CHANNEL | Permissions::READ_MESSAGE_HISTORY | Permissions::SEND_MESSAGES
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    Open,
    Private,
    Restricted,
}

#[derive(Clone, Copy)]
struct Shape {
    roles: u64,
    channels: u64,
    sets: u64,
    members: u64,
    kind: Kind,
}

impl Shape {
    const fn new(members: u64, sets: u64, kind: Kind) -> Shape {
        Shape {
            roles: 50,
            channels: 200,
            sets,
            members,
            kind,
        }
    }

    fn categories(&self) -> u64 {
        self.channels / 20
    }

    fn text_id(&self, j: u64) -> ChannelId {
        channel(self.categories() + j)
    }

    fn roles_list(
        &self,
        everyone: Permissions,
        edited: Option<(u64, Permissions)>,
    ) -> Vec<RoleSpec> {
        let mut out = vec![RoleSpec {
            id: RoleId::new(GUILD),
            permissions: everyone,
            position: RankPos::Int(0),
            hoist: Hoist::NotHoisted,
        }];
        for i in 1..self.roles {
            let permissions = match edited {
                Some((r, p)) if r == i => p,
                _ => Permissions::SEND_MESSAGES,
            };
            out.push(RoleSpec {
                id: role(i),
                permissions,
                position: RankPos::Int(i as i64),
                hoist: if i <= 5 {
                    Hoist::Hoisted(None)
                } else {
                    Hoist::NotHoisted
                },
            });
        }
        out
    }

    fn text(&self, j: u64, extra: Option<u64>) -> ChannelSpec {
        let mut overwrites = Vec::new();
        match self.kind {
            Kind::Private if j.is_multiple_of(2) => {
                overwrites.push(OverwriteSpec {
                    target: OverwriteTarget::Role(RoleId::new(GUILD)),
                    allow: Permissions::empty(),
                    deny: Permissions::VIEW_CHANNEL,
                });
                overwrites.push(OverwriteSpec {
                    target: OverwriteTarget::Role(role(1 + (j / 2) % (self.roles - 1))),
                    allow: Permissions::VIEW_CHANNEL,
                    deny: Permissions::empty(),
                });
                if j / 2 < 8 {
                    overwrites.push(OverwriteSpec {
                        target: OverwriteTarget::Member(user(1 + j / 2)),
                        allow: Permissions::VIEW_CHANNEL,
                        deny: Permissions::empty(),
                    });
                }
            }
            Kind::Restricted => overwrites.push(OverwriteSpec {
                target: OverwriteTarget::Role(role(1 + j % (self.roles - 1))),
                allow: Permissions::empty(),
                deny: Permissions::VIEW_CHANNEL,
            }),
            _ => {}
        }
        if let Some(r) = extra {
            overwrites.push(OverwriteSpec {
                target: OverwriteTarget::Role(role(r)),
                allow: Permissions::VIEW_CHANNEL,
                deny: Permissions::empty(),
            });
        }
        ChannelSpec {
            id: self.text_id(j),
            kind: ChannelKind::Text,
            parent: Some(channel(j % self.categories())),
            overwrites,
            nsfw: Nsfw::Inherit,
            named: true,
        }
    }

    fn rolesets(&self) -> Vec<Vec<RoleId>> {
        let mut seen = BTreeSet::new();
        let mut state = self.roles * 1_000_003 + self.sets;
        let mut out = Vec::new();
        while (out.len() as u64) < self.sets {
            let size = 1 + splitmix(&mut state) % 3;
            let mut picked = BTreeSet::new();
            while (picked.len() as u64) < size {
                picked.insert(1 + splitmix(&mut state) % (self.roles - 1));
            }
            let key: Vec<u64> = picked.into_iter().collect();
            if seen.insert(key.clone()) {
                out.push(key.into_iter().map(role).collect());
            }
        }
        out
    }

    fn snapshot(&self) -> Snapshot {
        let mut s = Snapshot::new(GuildId::new(GUILD), Some(user(0)));
        s.roles = self.roles_list(base(), None);
        s.channels = (0..self.categories())
            .map(|i| ChannelSpec {
                id: channel(i),
                kind: ChannelKind::Category,
                parent: None,
                overwrites: Vec::new(),
                nsfw: Nsfw::Inherit,
                named: true,
            })
            .collect();
        let texts = self.channels - self.categories();
        s.channels.extend((0..texts).map(|j| self.text(j, None)));
        let sets = self.rolesets();
        for n in 1..=self.members {
            let roles = sets[((n - 1) % self.sets) as usize].clone();
            s.members.push((user(n), RoleList::Roles(roles)));
        }
        s
    }

    fn load(&self) -> Arc<PermsShared> {
        load(&self.snapshot())
    }
}

fn load(snapshot: &Snapshot) -> Arc<PermsShared> {
    let shared = PermsShared::new(GuildId::new(GUILD), Config::default());
    match shared.load(snapshot) {
        Ok(_) => {}
        Err(PermsError::NeedsDirty) => {
            shared.load_dirty(snapshot).unwrap();
        }
        Err(e) => panic!("{e:?}"),
    }
    shared
}

fn apply(shared: &PermsShared, events: &[Event]) {
    match shared.apply(events) {
        Ok(effects) => {
            black_box(effects);
        }
        Err(PermsError::NeedsDirty) => {
            black_box(shared.apply_dirty(events).unwrap());
        }
        Err(e) => panic!("{e:?}"),
    }
}

fn target<'c>(
    c: &'c mut Criterion,
    id: &str,
    text: &str,
) -> criterion::BenchmarkGroup<'c, criterion::measurement::WallTime> {
    println!("{id}: target {text}");
    let mut group = c.benchmark_group(id);
    group.sample_size(30);
    group.measurement_time(Duration::from_secs(2));
    group.warm_up_time(Duration::from_millis(300));
    group
}

const OPEN_10K: Shape = Shape::new(10_000, 1_000, Kind::Open);
const PRIVATE_10K: Shape = Shape::new(10_000, 1_000, Kind::Private);

fn reads(c: &mut Criterion) {
    let shape = PRIVATE_10K;
    let shared = shape.load();
    let view = shared.view().unwrap();
    let pairs: Vec<(UserId, ChannelId)> = (0..1024u64)
        .map(|k| {
            (
                user(1 + (k * 7919) % shape.members),
                channel((k * 31) % shape.channels),
            )
        })
        .collect();
    let mut k = 0usize;

    let mut group = target(c, "perm_get_user_permissions", "3 us or less");
    group.bench_function("permissions", |b| {
        b.iter(|| {
            k = (k + 1) & 1023;
            let (u, ch) = pairs[k];
            black_box(view.permissions(u, Scope::Channel(ch)))
        })
    });
    group.finish();

    let mut group = target(c, "perm_check_permission", "3 us or less");
    group.bench_function("check", |b| {
        b.iter(|| {
            k = (k + 1) & 1023;
            let (u, ch) = pairs[k];
            black_box(view.check(u, Scope::Channel(ch), Permissions::SEND_MESSAGES))
        })
    });
    group.finish();

    let mut group = target(c, "perm_viewable_channels", "5 us or less");
    group.bench_function("viewable", |b| {
        b.iter(|| {
            k = (k + 1) & 1023;
            black_box(view.viewable(pairs[k].0, &mut Meter::new()))
        })
    });
    group.finish();

    let mut group = target(c, "perm_reply_bytes", "5 us or less");
    group.bench_function("viewable_channels_reply", |b| {
        b.iter(|| {
            k = (k + 1) & 1023;
            black_box(view.viewable_channels_reply(pairs[k].0, &mut Meter::new()))
        })
    });
    group.finish();

    let mut group = target(c, "perm_message_create_private", "0.1 ms or less");
    group.bench_function("audience", |b| {
        b.iter(|| {
            k = (k + 1) & 1023;
            let ch = shape.text_id((k as u64 * 2) % 190);
            black_box(view.audience(ch, Need::MessageAccess(MESSAGE_MS), &mut Meter::new()))
        })
    });
    group.finish();

    let mut group = target(
        c,
        "perm_push_share",
        "50 us or less per message, 10,000 members",
    );
    group.bench_function("viewers", |b| {
        b.iter(|| black_box(view.viewers(shape.text_id(10), &mut Meter::new())))
    });
    group.finish();

    let mut group = target(
        c,
        "perm_shares_view_many",
        "5 us or less for all 100 candidates",
    );
    let candidates: Vec<Subject> = (0..100u64)
        .map(|n| Subject::Member(user(1 + (n * 7919) % shape.members)))
        .collect();
    group.bench_function("filter_sharing_100", |b| {
        b.iter(|| {
            k = (k + 1) & 1023;
            black_box(view.filter_sharing(
                &Subject::Member(pairs[k].0),
                &candidates,
                &mut Meter::new(),
            ))
        })
    });
    group.finish();

    let mut group = target(
        c,
        "perm_passive_tick",
        "10 ms or less for 10,000 passive sessions",
    );
    group.sample_size(10);
    group.bench_function("view_sets_10000", |b| {
        b.iter(|| {
            let mut meter = Meter::new();
            for n in 1..=10_000u64 {
                black_box(view.subject_view_set(&Subject::Member(user(n)), &mut meter));
            }
        })
    });
    group.finish();
}

fn counts(c: &mut Criterion) {
    let shape = Shape::new(10_000, 1_000, Kind::Restricted);
    let shared = shape.load();
    let view = shared.view().unwrap();
    for (id, online, text) in [
        ("perm_public_online_1000", 1_000u64, "0.2 ms or less"),
        ("perm_public_online_100", 100u64, "0.1 ms or less"),
    ] {
        let users: Vec<UserId> = (0..online).map(|n| user(1 + n * 7)).collect();
        let mut group = target(c, id, text);
        group.bench_function("public_online", |b| {
            b.iter(|| black_box(view.public_online(&users, &mut Meter::new())))
        });
        group.finish();
    }
    let private = PRIVATE_10K.load();
    let view = private.view().unwrap();
    let users: Vec<UserId> = (0..1_000u64).map(|n| user(1 + n * 7)).collect();
    let mut group = target(c, "perm_mutual_online", "0.2 ms or less");
    group.bench_function("mutual_online", |b| {
        b.iter(|| black_box(view.mutual_online(user(9_999), &users, &mut Meter::new())))
    });
    group.finish();
}

fn edits(c: &mut Criterion) {
    for (id, shape, text) in [
        ("perm_everyone_edit_open", OPEN_10K, "3 ms or less"),
        ("perm_everyone_edit_private", PRIVATE_10K, "3 ms or less"),
    ] {
        let shared = shape.load();
        let flips = [
            vec![Event::Roles(
                shape.roles_list(base() | Permissions::STREAM, None),
            )],
            vec![Event::Roles(shape.roles_list(base(), None))],
        ];
        let mut n = 0usize;
        let mut group = target(c, id, text);
        group.bench_function("everyone", |b| {
            b.iter(|| {
                n ^= 1;
                apply(&shared, &flips[n]);
            })
        });
        group.finish();
    }

    let shape = PRIVATE_10K;
    let shared = shape.load();
    let x = 1 + shape.roles / 2;
    let flips = [
        vec![Event::Roles(shape.roles_list(
            base(),
            Some((
                x,
                Permissions::SEND_MESSAGES | Permissions::VIEW_CHANNEL_MEMBERS,
            )),
        ))],
        vec![Event::Roles(shape.roles_list(base(), None))],
    ];
    let mut n = 0usize;
    let mut group = target(c, "perm_role_edit", "0.4 ms or less");
    group.bench_function("role", |b| {
        b.iter(|| {
            n ^= 1;
            apply(&shared, &flips[n]);
        })
    });
    group.finish();

    let flips = [
        vec![Event::ChannelUpsert(
            shape.text(2, Some(x)),
            ChannelCause::Updated,
        )],
        vec![Event::ChannelUpsert(
            shape.text(2, None),
            ChannelCause::Updated,
        )],
    ];
    let mut group = target(c, "perm_overwrite_edit", "0.3 ms or less");
    group.bench_function("overwrite", |b| {
        b.iter(|| {
            n ^= 1;
            apply(&shared, &flips[n]);
        })
    });
    group.finish();

    let sets = shape.rolesets();
    let member = user(500);
    let flips = [
        vec![Event::MemberUpsert(
            member,
            RoleList::Roles(sets[3].clone()),
        )],
        vec![Event::MemberUpsert(
            member,
            RoleList::Roles(sets[4].clone()),
        )],
    ];
    let mut group = target(c, "perm_member_role_add", "20 us or less");
    group.bench_function("member_upsert", |b| {
        b.iter(|| {
            n ^= 1;
            apply(&shared, &flips[n]);
        })
    });
    group.finish();

    let mut hoisted = shape.roles_list(base(), None);
    if let Some(r) = hoisted.get_mut(7) {
        r.position = RankPos::Int(1_000);
    }
    let flips = [
        vec![Event::Roles(hoisted)],
        vec![Event::Roles(shape.roles_list(base(), None))],
    ];
    let mut group = target(c, "perm_publish", "60 us or less");
    group.bench_function("position_publish", |b| {
        b.iter(|| {
            n ^= 1;
            apply(&shared, &flips[n]);
        })
    });
    group.finish();
}

fn large(c: &mut Criterion) {
    let shape = Shape::new(50_000, 1_000, Kind::Private);
    let snapshot = shape.snapshot();
    let mut group = target(c, "perm_load_50k", "10 ms or less");
    group.sample_size(10);
    group.bench_function("load", |b| {
        b.iter_batched(
            || PermsShared::new(GuildId::new(GUILD), Config::default()),
            |shared| black_box(shared.load_dirty(&snapshot)),
            BatchSize::PerIteration,
        )
    });
    group.finish();

    let shared = load(&snapshot);
    let bytes = shared.stats().bytes;
    let verdict = if bytes <= 2 * 1024 * 1024 {
        "pass"
    } else {
        "FAIL"
    };
    println!("perm_memory_50k: {bytes} bytes held, target 2 MB or less: {verdict}");
    let mut group = target(c, "perm_memory_50k", "2 MB or less held");
    group.bench_function("stats", |b| b.iter(|| black_box(shared.stats())));
    group.finish();

    let view = shared.view().unwrap();
    let mut group = target(c, "perm_role_set_members", "20 us or less");
    group.bench_function("members_with_role", |b| {
        b.iter(|| black_box(view.role_set_members(&[role(17)], &mut Meter::new())))
    });
    group.finish();

    let mut group = target(c, "perm_push_prune_50k", "50 us or less");
    let prune = PushPrune {
        channel: shape.text_id(10),
        mentioned: &[],
        roles: &[],
        everyone: false,
    };
    group.bench_function("unmentioned", |b| {
        b.iter(|| black_box(view.push_prune(&prune, &mut Meter::new())))
    });
    let mentioned = [user(3), user(40_000)];
    let roles = [role(17)];
    let prune = PushPrune {
        channel: shape.text_id(10),
        mentioned: &mentioned,
        roles: &roles,
        everyone: false,
    };
    group.bench_function("mentioned_and_role", |b| {
        b.iter(|| black_box(view.push_prune(&prune, &mut Meter::new())))
    });
    group.finish();
}

fn contention(c: &mut Criterion) {
    let shape = PRIVATE_10K;
    let shared = shape.load();
    let flips = [
        vec![Event::Roles(
            shape.roles_list(base() | Permissions::STREAM, None),
        )],
        vec![Event::Roles(shape.roles_list(base(), None))],
    ];
    let stop = Arc::new(AtomicBool::new(false));
    let start = Arc::new(Barrier::new(64));
    let mut threads = Vec::new();
    {
        let shared = shared.clone();
        let stop = stop.clone();
        let start = start.clone();
        threads.push(std::thread::spawn(move || {
            start.wait();
            let mut n = 0usize;
            while !stop.load(Ordering::Relaxed) {
                n ^= 1;
                apply(&shared, &flips[n]);
            }
        }));
    }
    for t in 0..62u64 {
        let shared = shared.clone();
        let stop = stop.clone();
        let start = start.clone();
        threads.push(std::thread::spawn(move || {
            start.wait();
            let mut k = t;
            while !stop.load(Ordering::Relaxed) {
                k = k.wrapping_add(7919);
                if let Ok(view) = shared.view() {
                    black_box(
                        view.permissions(user(1 + k % 10_000), Scope::Channel(channel(k % 200))),
                    )
                    .ok();
                }
            }
        }));
    }
    start.wait();
    let mut samples: Vec<u64> = Vec::new();
    let mut group = target(c, "perm_reader_contention", "reader p99 under 5 us");
    group.bench_function("reader_during_everyone_edits", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for k in 0..iters {
                let at = Instant::now();
                let view = shared.view().unwrap();
                black_box(view.permissions(user(1 + k % 10_000), Scope::Channel(channel(k % 200))))
                    .ok();
                drop(view);
                let spent = at.elapsed();
                samples.push(u64::try_from(spent.as_nanos()).unwrap_or(u64::MAX));
                total += spent;
            }
            total
        })
    });
    group.finish();
    stop.store(true, Ordering::Relaxed);
    for t in threads {
        t.join().unwrap();
    }
    samples.sort_unstable();
    let p99 = samples.get(samples.len() * 99 / 100).copied().unwrap_or(0);
    let verdict = if p99 < 5_000 { "pass" } else { "FAIL" };
    println!("perm_reader_contention: reader p99 {p99} ns, target under 5000 ns: {verdict}");
}

criterion_group!(benches, reads, counts, edits, large, contention);
criterion_main!(benches);
