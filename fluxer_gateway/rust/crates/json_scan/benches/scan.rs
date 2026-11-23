// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt::Write as _;
use std::hint::black_box;
use std::time::Instant;

use criterion::Criterion;

const GUILD_CREATE_BYTES: usize = 1024 * 1024;
const MEMBERS: usize = 10_000;
const MESSAGE_BYTES: usize = 50 * 1024;
const SCAN_TOP_LEVEL_MAX_NS: f64 = 1_000_000.0;
const SLICE_MEMBERS_MAX_NS: f64 = 4_000_000.0;
const SPLICE_FIELD_MAX_NS: f64 = 50_000.0;

fn splitmix(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

fn snowflake(state: &mut u64) -> u64 {
    1_400_000_000_000_000_000 + splitmix(state) % 100_000_000_000_000_000
}

const WORDS: [&str; 12] = [
    "general", "memes", "voice", "music", "lounge", "art", "dev", "games", "news", "help",
    "events", "clips",
];

fn word(state: &mut u64) -> &'static str {
    WORDS[(splitmix(state) % WORDS.len() as u64) as usize]
}

fn member(out: &mut String, state: &mut u64) {
    let id = snowflake(state);
    let role = snowflake(state);
    let _ = write!(
        out,
        concat!(
            r#"{{"user":{{"id":"{id}","username":"{name}_{n}","discriminator":"0001","#,
            r#""global_name":"{name} \"{n}\"","avatar":"{avatar:032x}","avatar_color":null,"#,
            r#""bot":false,"flags":0}},"nick":null,"roles":["{role}"],"#,
            r#""joined_at":"2026-01-01T00:00:00.000000+00:00","deaf":false,"mute":false,"#,
            r#""communication_disabled_until":null}}"#
        ),
        id = id,
        name = word(state),
        n = splitmix(state) % 10_000,
        avatar = splitmix(state),
        role = role,
    );
}

fn channel(out: &mut String, state: &mut u64) {
    let _ = write!(
        out,
        r#"{{"id":"{}","type":0,"name":"{}","position":{},"permission_overwrites":[{{"id":"{}","type":0,"allow":"1024","deny":"0"}}],"topic":"talk about {} here","last_message_id":"{}"}}"#,
        snowflake(state),
        word(state),
        splitmix(state) % 500,
        snowflake(state),
        word(state),
        snowflake(state),
    );
}

fn guild_create(size: usize) -> Vec<u8> {
    let mut state = 7;
    let mut out = String::with_capacity(size + 4096);
    let _ = write!(
        out,
        r#"{{"id":"{}","name":"bench guild","icon":null,"owner_id":"{}","member_count":50000,"#,
        snowflake(&mut state),
        snowflake(&mut state),
    );
    out.push_str(r#""roles":["#);
    for n in 0..100 {
        if n > 0 {
            out.push(',');
        }
        let _ = write!(
            out,
            r#"{{"id":"{}","name":"{}","permissions":"2248473465835073","position":{n},"color":0,"hoist":false}}"#,
            snowflake(&mut state),
            word(&mut state),
        );
    }
    out.push_str(r#"],"channels":["#);
    for n in 0..300 {
        if n > 0 {
            out.push(',');
        }
        channel(&mut out, &mut state);
    }
    out.push_str(r#"],"members":["#);
    let mut first = true;
    while out.len() < size.saturating_sub(64) {
        if !first {
            out.push(',');
        }
        first = false;
        member(&mut out, &mut state);
    }
    out.push_str(r#"],"unavailable":false,"large":true}"#);
    out.into_bytes()
}

fn member_array(count: usize) -> Vec<u8> {
    let mut state = 11;
    let mut out = String::with_capacity(count * 400);
    out.push('[');
    for n in 0..count {
        if n > 0 {
            out.push(',');
        }
        member(&mut out, &mut state);
    }
    out.push(']');
    out.into_bytes()
}

fn message(size: usize) -> Vec<u8> {
    let mut state = 13;
    let mut content = String::new();
    while content.len() < size.saturating_sub(2048) {
        content.push_str(word(&mut state));
        content.push_str(if splitmix(&mut state).is_multiple_of(9) {
            " \\\"quoted\\\" "
        } else {
            " "
        });
    }
    let mut out = String::with_capacity(size + 1024);
    let _ = write!(
        out,
        r#"{{"id":"{}","channel_id":"{}","guild_id":"{}","author":{{"id":"{}","username":"bench"}},"content":"{content}","embeds":[{{"title":"t","fields":[{{"name":"a","value":"b"}}]}}],"mentions":[],"flags":0}}"#,
        snowflake(&mut state),
        snowflake(&mut state),
        snowflake(&mut state),
        snowflake(&mut state),
    );
    out.into_bytes()
}

fn median_ns(rounds: usize, mut run: impl FnMut()) -> f64 {
    let mut samples: Vec<f64> = (0..rounds)
        .map(|_| {
            let start = Instant::now();
            run();
            start.elapsed().as_nanos() as f64
        })
        .collect();
    samples.sort_by(f64::total_cmp);
    samples[samples.len() / 2]
}

fn report(failed: &mut usize, ok: bool, line: String) {
    if !ok {
        *failed += 1;
    }
    println!("  {} {line}", if ok { "met   " } else { "MISSED" });
}

fn main() {
    let guild = guild_create(GUILD_CREATE_BYTES);
    let members = member_array(MEMBERS);
    let message = message(MESSAGE_BYTES);
    assert_eq!(json_scan::object(&guild).expect("guild").len(), 10);
    assert_eq!(json_scan::array(&members).expect("members").len(), MEMBERS);

    let mut c = Criterion::default().configure_from_args();
    c.bench_function("scan_top_level_1mb_guild_create", |b| {
        b.iter(|| json_scan::object(black_box(&guild)))
    });
    c.bench_function("slice_member_array_10k", |b| {
        b.iter(|| json_scan::array(black_box(&members)))
    });
    c.bench_function("splice_field_50kb", |b| {
        b.iter(|| json_scan::set_field(black_box(&message), b"flags", b"64"))
    });
    c.final_summary();

    let mut failed = 0;
    println!("\ntarget check (median against target):");
    let scan = median_ns(200, || {
        black_box(json_scan::object(black_box(&guild)).expect("scan"));
    });
    let full = median_ns(50, || {
        black_box(serde_json::from_slice::<serde_json::Value>(black_box(&guild)).expect("parse"));
    });
    report(
        &mut failed,
        scan <= SCAN_TOP_LEVEL_MAX_NS,
        format!(
            "scan_top_level_1mb_guild_create: {:.1} us for {} KB ({:.2} GB/s), serde_json full parse {:.1} us, target {:.1} us",
            scan / 1e3,
            guild.len() / 1024,
            guild.len() as f64 / scan,
            full / 1e3,
            SCAN_TOP_LEVEL_MAX_NS / 1e3
        ),
    );
    let slice = median_ns(100, || {
        black_box(json_scan::array(black_box(&members)).expect("slice"));
    });
    report(
        &mut failed,
        slice <= SLICE_MEMBERS_MAX_NS,
        format!(
            "slice_member_array_10k: {:.1} us for {} KB, target {:.1} us",
            slice / 1e3,
            members.len() / 1024,
            SLICE_MEMBERS_MAX_NS / 1e3
        ),
    );
    let splice = median_ns(2_000, || {
        black_box(json_scan::set_field(black_box(&message), b"flags", b"64").expect("splice"));
    });
    report(
        &mut failed,
        splice <= SPLICE_FIELD_MAX_NS,
        format!(
            "splice_field_50kb: {:.2} us for {} KB, target {:.1} us",
            splice / 1e3,
            message.len() / 1024,
            SPLICE_FIELD_MAX_NS / 1e3
        ),
    );

    println!("\ncriterion targets: {failed} missed");
    if failed > 0 {
        std::process::exit(1);
    }
}
