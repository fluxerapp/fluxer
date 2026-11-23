// SPDX-License-Identifier: AGPL-3.0-or-later

use std::hint::black_box;
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime};

use criterion::Criterion;
use ml_core::{
    Change, Config, Core, CoreError, Done, EncodedItem, Event, Mode, PageQuery, Ranges, Row,
    SessionKey, Snapshot, Status, Step, SyncScope, Timing,
};
use perm_core::{
    ChannelId, ChannelKind, ChannelSpec, Config as PermConfig, Event as PermEvent, GuildId, Hoist,
    Nsfw, OverwriteSpec, OverwriteTarget, Permissions, PermsError, PermsShared, RankPos, RoleId,
    RoleList, RoleSpec, Snapshot as PermSnapshot, UserId,
};

const GUILD: u64 = 1_400_000_000_000_000_000;
const MAIN: u64 = 1_420_000_000_000_000_001;
const SPARSE: u64 = 1_420_000_000_000_000_002;
const SPARE: u64 = 1_420_000_000_000_000_003;
const BIG_ROLE: u64 = 1_410_000_000_000_000_150;
const SPARSE_ROLE: u64 = 1_410_000_000_000_000_149;

const LIST_ITEM: &[u8] = br#"{"member":{"deaf":false,"joined_at":"2026-11-09T00:00:00.000000+00:00","mute":false,"nick":null,"roles":["1410000000000000001"],"user":{"avatar":"a_0123456789abcdef0123456789abcdef","bot":false,"global_name":"Some Member","id":"1430000000000000000","username":"some member"}},"presence":{"afk":false,"custom_status":null,"mobile":false,"status":"online"}}"#;
const PAGE_ITEM: &[u8] = br#"{"deaf":false,"joined_at":"2026-11-09T00:00:00.000000+00:00","mute":false,"nick":null,"roles":["1410000000000000001"],"user":{"avatar":"a_0123456789abcdef0123456789abcdef","bot":false,"global_name":"Some Member","id":"1430000000000000000","username":"some member"}}"#;

fn role(i: u64) -> u64 {
    1_410_000_000_000_000_000 + i
}

fn user(i: u64) -> UserId {
    UserId::new(1_430_000_000_000_000_000 + i * 7919)
}

struct Size {
    members: u64,
    online: u64,
}

const SIZE_2K: Size = Size {
    members: 2_000,
    online: 1_000,
};
const SIZE_50K: Size = Size {
    members: 50_000,
    online: 5_000,
};
const SIZE_250K: Size = Size {
    members: 250_000,
    online: 20_000,
};

fn name(i: u64) -> Vec<u8> {
    if i.is_multiple_of(10) {
        format!("Ärla {i}").into_bytes()
    } else {
        format!("member {i}").into_bytes()
    }
}

fn roles(big_hoisted: bool) -> Vec<RoleSpec> {
    let mut out = vec![RoleSpec {
        id: RoleId::new(GUILD),
        permissions: Permissions::VIEW_CHANNEL | Permissions::VIEW_CHANNEL_MEMBERS,
        position: RankPos::Int(0),
        hoist: Hoist::NotHoisted,
    }];
    for i in 100..150 {
        out.push(RoleSpec {
            id: RoleId::new(role(i)),
            permissions: Permissions::empty(),
            position: RankPos::Int(i as i64 - 99),
            hoist: if i < 105 {
                Hoist::Hoisted(None)
            } else {
                Hoist::NotHoisted
            },
        });
    }
    out.push(RoleSpec {
        id: RoleId::new(BIG_ROLE),
        permissions: Permissions::empty(),
        position: RankPos::Int(60),
        hoist: if big_hoisted {
            Hoist::Hoisted(None)
        } else {
            Hoist::NotHoisted
        },
    });
    out
}

fn channel(id: u64, overwrites: Vec<OverwriteSpec>) -> ChannelSpec {
    ChannelSpec {
        id: ChannelId::new(id),
        kind: ChannelKind::Text,
        parent: None,
        overwrites,
        nsfw: Nsfw::Inherit,
        named: true,
    }
}

fn deny_view(target: OverwriteTarget) -> OverwriteSpec {
    OverwriteSpec {
        target,
        allow: Permissions::empty(),
        deny: Permissions::VIEW_CHANNEL,
    }
}

fn channels() -> Vec<ChannelSpec> {
    let mut out = vec![
        channel(MAIN, Vec::new()),
        channel(
            SPARSE,
            vec![
                deny_view(OverwriteTarget::Role(RoleId::new(GUILD))),
                OverwriteSpec {
                    target: OverwriteTarget::Role(RoleId::new(SPARSE_ROLE)),
                    allow: Permissions::VIEW_CHANNEL,
                    deny: Permissions::empty(),
                },
            ],
        ),
        channel(
            SPARE,
            vec![deny_view(OverwriteTarget::Role(RoleId::new(role(101))))],
        ),
    ];
    for i in 0..197u64 {
        out.push(channel(
            1_420_000_000_000_001_000 + i,
            vec![deny_view(OverwriteTarget::Role(RoleId::new(role(
                100 + i % 50,
            ))))],
        ));
    }
    out
}

fn is_online(size: &Size, i: u64) -> bool {
    i.is_multiple_of(size.members / size.online)
}

fn member_roles(size: &Size, i: u64) -> Vec<RoleId> {
    let mut roles = vec![RoleId::new(role(100 + i % 49))];
    if i % 4 == 1 {
        roles.push(RoleId::new(BIG_ROLE));
    }
    let sparse_online =
        is_online(size, i) && (i / (size.members / size.online)) % (size.online / 50) == 3;
    if sparse_online || i % 997 == 5 {
        roles.push(RoleId::new(SPARSE_ROLE));
    }
    roles
}

struct Fixture {
    perms: Arc<PermsShared>,
    snapshot: Snapshot,
    core: Core<u64>,
    in_range: UserId,
    outside: UserId,
}

fn apply_perms(perms: &PermsShared, events: &[PermEvent]) {
    match perms.apply(events) {
        Err(PermsError::NeedsDirty) => {
            perms.apply_dirty(events).ok();
        }
        other => {
            other.ok();
        }
    }
}

fn load_perms(perms: &PermsShared, snapshot: &PermSnapshot) {
    match perms.load(snapshot) {
        Err(PermsError::NeedsDirty) => {
            perms.load_dirty(snapshot).ok();
        }
        other => {
            other.ok();
        }
    }
}

fn drive(core: &mut Core<u64>, mut step: Step<u64>) -> usize {
    let mut bytes = 0;
    for _ in 0..100_000 {
        let next = match step {
            Step::Done(Done::Sync(r)) => {
                return bytes + r.sent.iter().map(|s| s.payload.len()).sum::<usize>();
            }
            Step::Done(Done::Page(p)) => return bytes + p.payload.len(),
            Step::Partial(sent) => {
                bytes += sent.iter().map(|s| s.payload.len()).sum::<usize>();
                core.resume()
            }
            Step::NeedItems(users) => {
                let items = users
                    .iter()
                    .map(|u| EncodedItem {
                        user: *u,
                        list: LIST_ITEM.into(),
                        page: PAGE_ITEM.into(),
                    })
                    .collect();
                core.fill(items)
            }
        };
        match next {
            Ok(s) => step = s,
            Err(_) => return bytes,
        }
    }
    bytes
}

fn run(core: &mut Core<u64>, events: &[Event<u64>]) -> (Duration, bool) {
    let start = Instant::now();
    let normal = core.apply(events, None, Mode::Normal);
    let dirty = matches!(normal, Err(CoreError::NeedsDirty));
    let effects = if dirty {
        core.apply(events, None, Mode::Dirty).ok()
    } else {
        normal.ok()
    };
    if let Some(effects) = effects
        && let Ok(step) = core.render(effects)
    {
        black_box(drive(core, step));
    }
    (start.elapsed(), dirty)
}

fn session(i: u64) -> SessionKey {
    SessionKey::new(format!("session-{i:05}").as_bytes())
}

fn range(start: u32) -> Ranges {
    Ranges::normalized(&[(start, start + 99)]).unwrap_or_default()
}

fn new_core(perms: &Arc<PermsShared>, config: Config) -> Core<u64> {
    match Core::new(GuildId::new(GUILD), config, Arc::clone(perms)) {
        Ok(core) => core,
        Err(e) => panic!("core: {e:?}"),
    }
}

fn fixture(size: &Size, subscribed_lists: u64) -> Fixture {
    let perms = PermsShared::new(GuildId::new(GUILD), PermConfig::default());
    let mut snapshot = PermSnapshot::new(GuildId::new(GUILD), Some(user(0)));
    snapshot.roles = roles(false);
    snapshot.channels = channels();
    let mut ml = Snapshot::default();
    for i in 0..size.members {
        snapshot
            .members
            .push((user(i), RoleList::Roles(member_roles(size, i))));
        ml.members.push((user(i), name(i).into_boxed_slice()));
        if is_online(size, i) {
            ml.statuses.push((user(i), Status::Online));
            ml.connected.push(user(i));
        }
    }
    load_perms(&perms, &snapshot);
    let mut core = new_core(&perms, Config::default());
    if let Err(e) = core.load(&ml, Mode::Dirty) {
        panic!("load: {e:?}");
    }
    let mut events = Vec::new();
    for s in 0..100u64 {
        events.push(Event::Subscribe {
            session: session(s),
            pid: s,
            user: user(s * 31),
            channel: ChannelId::new(MAIN),
            ranges: range(0),
            sync: Ranges::empty(),
        });
    }
    for l in 1..subscribed_lists {
        events.push(Event::Subscribe {
            session: session(1000 + l),
            pid: 1000 + l,
            user: user(0),
            channel: ChannelId::new(1_420_000_000_000_001_000 + l),
            ranges: range(0),
            sync: Ranges::empty(),
        });
    }
    for chunk in events.chunks(64) {
        if let Err(e) = core.apply(chunk, None, Mode::Dirty) {
            panic!("subscribe: {e:?}");
        }
    }
    let rows = core
        .render_session(&session(0))
        .map(|body| body.ops)
        .unwrap_or_default();
    let in_range_rows: Vec<UserId> = rows
        .iter()
        .flat_map(|op| op.rows.iter())
        .filter_map(|r| match r {
            Row::Member(u) => Some(*u),
            Row::Group(_) => None,
        })
        .collect();
    let in_range = in_range_rows
        .get(in_range_rows.len() / 2)
        .copied()
        .unwrap_or(user(0));
    let outside = (0..size.members)
        .map(user)
        .find(|u| {
            !in_range_rows.contains(u)
                && ((u.get() - user(0).get()) / 7919).is_multiple_of(size.members / size.online)
        })
        .unwrap_or(user(size.members - 1));
    Fixture {
        perms,
        snapshot: ml,
        core,
        in_range,
        outside,
    }
}

fn presence(user: UserId, status: Status) -> Event<u64> {
    Event::Presence {
        user,
        status,
        change: Change::Changed,
        timing: Timing::Immediate,
    }
}

fn group<'a>(
    c: &'a mut Criterion,
    id: &str,
) -> criterion::BenchmarkGroup<'a, criterion::measurement::WallTime> {
    let mut group = c.benchmark_group(id);
    group.sample_size(20);
    group.measurement_time(Duration::from_secs(3));
    group.warm_up_time(Duration::from_millis(500));
    group
}

fn flip_bench(
    c: &mut Criterion,
    id: &str,
    core: &mut Core<u64>,
    events: impl Fn(bool) -> Vec<Event<u64>>,
) {
    let mut g = group(c, id);
    let mut flip = false;
    g.bench_function("run", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for _ in 0..iters {
                flip = !flip;
                total += run(core, &events(flip)).0;
            }
            total
        })
    });
    g.finish();
}

fn first_sync(c: &mut Criterion, id: &str, fx: &mut Fixture) {
    let mut g = group(c, id);
    g.bench_function("run", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for _ in 0..iters {
                let key = session(9999);
                total += run(
                    &mut fx.core,
                    &[Event::Subscribe {
                        session: key.clone(),
                        pid: 9999,
                        user: user(0),
                        channel: ChannelId::new(SPARE),
                        ranges: range(0),
                        sync: range(0),
                    }],
                )
                .0;
                run(&mut fx.core, &[Event::DropSession(key)]);
            }
            total
        })
    });
    g.finish();
}

fn bench_50k(c: &mut Criterion, fx: &mut Fixture) -> u64 {
    let in_range = fx.in_range;
    let outside = fx.outside;
    first_sync(c, "ml_first_sync_50k", fx);
    flip_bench(c, "ml_presence_in_range", &mut fx.core, |flip| {
        vec![presence(
            in_range,
            if flip { Status::Idle } else { Status::Online },
        )]
    });
    flip_bench(c, "ml_presence_out_of_range", &mut fx.core, |flip| {
        vec![presence(
            outside,
            if flip { Status::Idle } else { Status::Online },
        )]
    });
    flip_bench(c, "ml_section_move", &mut fx.core, |flip| {
        vec![presence(
            in_range,
            if flip {
                Status::Invisible
            } else {
                Status::Online
            },
        )]
    });
    flip_bench(c, "ml_nickname", &mut fx.core, |flip| {
        vec![Event::MemberUpsert {
            user: in_range,
            display_name: if flip {
                b"zz renamed".to_vec().into_boxed_slice()
            } else {
                b"aa renamed".to_vec().into_boxed_slice()
            },
            change: Change::Changed,
        }]
    });
    let perms = Arc::clone(&fx.perms);
    let mut g = group(c, "ml_hoist_toggle_50k");
    let mut hoisted = false;
    g.bench_function("run", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for _ in 0..iters {
                hoisted = !hoisted;
                apply_perms(&perms, &[PermEvent::Roles(roles(hoisted))]);
                total += run(&mut fx.core, &[Event::Permissions(SyncScope::AllLists)]).0;
            }
            total
        })
    });
    g.finish();
    let all: Vec<ChannelId> = channels().iter().map(|c| c.id).collect();
    let before = fx.core.stats().bytes;
    let mut g = group(c, "ml_counts_200x50k");
    g.bench_function("run", |b| {
        b.iter(|| black_box(fx.core.channel_counts(&all, Mode::Normal).ok()))
    });
    g.finish();
    fx.core.stats().bytes.saturating_sub(before)
}

fn bench_init(c: &mut Criterion, fx: &Fixture) {
    let perms = Arc::clone(&fx.perms);
    let snapshot = fx.snapshot.clone();
    let mut g = group(c, "ml_init_50k");
    g.bench_function("run", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for _ in 0..iters {
                let mut core = new_core(&perms, Config::default());
                let start = Instant::now();
                black_box(core.load(&snapshot, Mode::Dirty).ok());
                total += start.elapsed();
                drop(core);
            }
            total
        })
    });
    g.finish();
}

fn bench_2k(c: &mut Criterion, fx: &mut Fixture) -> bool {
    first_sync(c, "ml_first_sync_2k", fx);
    let perms = Arc::clone(&fx.perms);
    let snapshot = fx.snapshot.clone();
    let normal = new_core(&perms, Config::default())
        .load(&snapshot, Mode::Normal)
        .is_ok();
    let mut g = group(c, "ml_load_2k");
    g.bench_function("run", |b| {
        b.iter_custom(|iters| {
            let mut total = Duration::ZERO;
            for _ in 0..iters {
                let mut core = new_core(&perms, Config::default());
                let start = Instant::now();
                black_box(core.load(&snapshot, Mode::Normal).ok());
                total += start.elapsed();
                drop(core);
            }
            total
        })
    });
    g.finish();
    let after = user(fx.snapshot.members.len() as u64 / 2);
    let mut g = group(c, "ml_page_100");
    g.bench_function("run", |b| {
        b.iter(|| {
            let step = fx.core.page(PageQuery::After {
                after: Some(after),
                limit: 100,
            });
            match step {
                Ok(step) => black_box(drive(&mut fx.core, step)),
                Err(_) => 0,
            }
        })
    });
    g.finish();
    normal
}

fn bench_sparse(c: &mut Criterion, fx: &mut Fixture) {
    run(
        &mut fx.core,
        &[Event::Subscribe {
            session: session(8888),
            pid: 8888,
            user: user(0),
            channel: ChannelId::new(SPARSE),
            ranges: range(0),
            sync: Ranges::empty(),
        }],
    );
    let mut g = group(c, "ml_sparse_render");
    g.bench_function("run", |b| {
        b.iter(|| black_box(fx.core.render_session(&session(8888))))
    });
    g.finish();
}

fn targets() -> Vec<(&'static str, f64)> {
    vec![
        ("ml_first_sync_50k", 1_000_000.0),
        ("ml_first_sync_2k", 250_000.0),
        ("ml_presence_in_range", 50_000.0),
        ("ml_presence_out_of_range", 45_000.0),
        ("ml_section_move", 57_000.0),
        ("ml_hoist_toggle_50k", 1_000_000.0),
        ("ml_nickname", 20_000.0),
        ("ml_counts_200x50k", 1_000_000.0),
        ("ml_init_50k", 18_000_000.0),
        ("ml_sparse_render", 2_000.0),
        ("ml_load_2k", 1_000_000.0),
        ("ml_page_100", 20_000.0),
    ]
}

fn median_ns(group: &str, since: SystemTime) -> Option<f64> {
    let dir = std::env::var("CARGO_TARGET_DIR")
        .unwrap_or_else(|_| concat!(env!("CARGO_MANIFEST_DIR"), "/../../target").to_string());
    let path = format!("{dir}/criterion/{group}/run/new/estimates.json");
    let modified = std::fs::metadata(&path).ok()?.modified().ok()?;
    if modified < since {
        return None;
    }
    let text = std::fs::read_to_string(path).ok()?;
    let at = text.find("\"median\"")?;
    let rest = text.get(at..)?;
    let key = "\"point_estimate\":";
    let tail = rest.get(rest.find(key)? + key.len()..)?;
    let end = tail.find([',', '}'])?;
    tail.get(..end)?.trim().parse().ok()
}

fn check(label: &str, ok: bool, detail: String) -> u32 {
    println!(
        "  {} {label}: {detail}",
        if ok { "met   " } else { "MISSED" }
    );
    u32::from(!ok)
}

fn main() {
    let since = SystemTime::now();
    let mut c = Criterion::default().configure_from_args();
    let mut failed = 0u32;

    let mut fx2 = fixture(&SIZE_2K, 1);
    let load_normal = bench_2k(&mut c, &mut fx2);
    drop(fx2);

    let mut fx50 = fixture(&SIZE_50K, 1);
    let counts_kept = bench_50k(&mut c, &mut fx50);
    bench_init(&mut c, &fx50);
    drop(fx50);

    let mut fx250 = fixture(&SIZE_250K, 1);
    bench_sparse(&mut c, &mut fx250);
    let memory_250k = fx250.core.stats().bytes;
    drop(fx250);

    let memory_50k = fixture(&SIZE_50K, 5).core.stats().bytes;
    c.final_summary();

    println!("\nml_core targets (median against target):");
    for (id, target) in targets() {
        failed += match median_ns(id, since) {
            Some(m) => check(
                id,
                m <= target,
                format!("{:.3} us, target {:.3} us", m / 1000.0, target / 1000.0),
            ),
            None => check(id, false, "no estimate from this run".to_string()),
        };
    }
    failed += check(
        "ml_load_2k normal scheduler",
        load_normal,
        format!("load of 2,000 members on Mode::Normal returned ok = {load_normal}"),
    );
    failed += check(
        "ml_counts_200x50k kept bytes",
        counts_kept < 1_000_000,
        format!("{counts_kept} bytes kept, target under 1,000,000"),
    );
    failed += check(
        "ml_memory_50k",
        memory_50k <= 6_500_000,
        format!("{memory_50k} bytes with 5 subscribed lists, target 6,500,000"),
    );
    failed += check(
        "ml_memory_250k",
        memory_250k <= 17_000_000,
        format!("{memory_250k} bytes at 20,000 online, target 17,000,000"),
    );
    println!("\nml_core targets missed: {failed}");
    if failed > 0 {
        std::process::exit(1);
    }
}
