// SPDX-License-Identifier: AGPL-3.0-or-later

use std::fmt::Write as _;
use std::hint::black_box;
use std::time::{Duration, Instant};

use criterion::{BatchSize, Criterion};
use zstd_stream::{Compressor, CompressorConfig, Decompressor};

const LEVEL: i32 = 3;
const WINDOW_LOG: u32 = 14;
const BLOCK_LIMIT: u32 = 16_384;
const NO_LIMIT: u32 = 0;
const DECOMPRESS_WINDOW_LOG_MAX: u32 = 21;
const MAX_DECOMPRESSED: usize = 10 * 1024 * 1024;
const READY_SIZES: [(&str, usize); 3] = [
    ("zstd_ready_0_5mb", 512 * 1024),
    ("zstd_ready_2_5mb", 2_560 * 1024),
    ("zstd_ready_10mb", 10 * 1024 * 1024),
];
const READY_SLOWDOWN_MAX: f64 = 1.10;
const CONTEXT_BYTES_MAX: usize = 520 * 1024;
const SMALL_DISPATCH_MAX_NS: f64 = 10_000.0;
const DECOMPRESS_FRAME_MAX_NS: f64 = 5_000.0;
const CORPUS_RATIO_MIN: f64 = 4.83;
const IOLIST_SLOWDOWN_MAX: f64 = 1.05;
const LARGE_DISPATCH_BYTES: usize = 51_200;
const LARGE_DISPATCHES: usize = 16;

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

const WORDS: [&str; 16] = [
    "general", "memes", "voice", "music", "lounge", "art", "dev", "games", "news", "help",
    "events", "clips", "media", "rules", "intro", "offtopic",
];

fn word(state: &mut u64) -> &'static str {
    WORDS[(splitmix(state) % WORDS.len() as u64) as usize]
}

fn member(out: &mut String, state: &mut u64) {
    let id = snowflake(state);
    let avatar = splitmix(state);
    let _ = write!(
        out,
        "{{\"user\":{{\"id\":\"{id}\",\"username\":\"{}_{}\",\"global_name\":\"{} {}\",\
         \"avatar\":\"{avatar:016x}{avatar:016x}\",\"discriminator\":\"{:04}\",\"flags\":{},\
         \"bot\":false}},\"nick\":null,\"roles\":[\"{}\",\"{}\"],\"joined_at\":\
         \"2025-0{}-1{}T12:34:56.000000+00:00\",\"deaf\":false,\"mute\":false,\
         \"communication_disabled_until\":null}}",
        word(state),
        splitmix(state) % 10_000,
        word(state),
        word(state),
        splitmix(state) % 10_000,
        splitmix(state) % 4,
        snowflake(state),
        snowflake(state),
        1 + splitmix(state) % 9,
        splitmix(state) % 10,
    );
}

fn channel(out: &mut String, state: &mut u64, position: u64) {
    let _ = write!(
        out,
        "{{\"id\":\"{}\",\"type\":{},\"name\":\"{}-{}\",\"position\":{position},\
         \"parent_id\":\"{}\",\"topic\":\"talk about {} and {}\",\"nsfw\":false,\
         \"rate_limit_per_user\":0,\"last_message_id\":\"{}\",\"permission_overwrites\":\
         [{{\"id\":\"{}\",\"type\":0,\"allow\":\"0\",\"deny\":\"1024\"}}]}}",
        snowflake(state),
        splitmix(state) % 3,
        word(state),
        splitmix(state) % 100,
        snowflake(state),
        word(state),
        word(state),
        snowflake(state),
        snowflake(state),
    );
}

fn ready_body(target: usize, seed: u64) -> Vec<u8> {
    let mut state = seed;
    let mut out = String::with_capacity(target + 4_096);
    let _ = write!(
        out,
        "{{\"op\":0,\"s\":1,\"t\":\"READY\",\"d\":{{\"v\":1,\"session_id\":\"{:032x}\",\
         \"user\":{{\"id\":\"{}\",\"username\":\"someone\"}},\"guilds\":[",
        splitmix(&mut state),
        snowflake(&mut state),
    );
    let mut first_guild = true;
    while out.len() < target {
        if !first_guild {
            out.push(',');
        }
        first_guild = false;
        let _ = write!(
            out,
            "{{\"id\":\"{}\",\"name\":\"{} {}\",\"member_count\":{},\"channels\":[",
            snowflake(&mut state),
            word(&mut state),
            word(&mut state),
            splitmix(&mut state) % 50_000,
        );
        for position in 0..40 {
            if position > 0 {
                out.push(',');
            }
            channel(&mut out, &mut state, position);
        }
        out.push_str("],\"members\":[");
        for i in 0..60 {
            if i > 0 {
                out.push(',');
            }
            member(&mut out, &mut state);
        }
        out.push_str("]}");
    }
    out.push_str("]}}");
    out.into_bytes()
}

fn dispatches(count: usize, size: usize) -> Vec<Vec<u8>> {
    let mut state = 77;
    (0..count)
        .map(|n| {
            let mut out = format!(
                "{{\"op\":0,\"s\":{},\"t\":\"MESSAGE_CREATE\",\"d\":{{\"id\":\"{}\",\
                 \"channel_id\":\"{}\",\"guild_id\":\"{}\",\"content\":\"",
                n + 2,
                snowflake(&mut state),
                snowflake(&mut state),
                snowflake(&mut state),
            );
            while out.len() < size.saturating_sub(64) {
                out.push_str(word(&mut state));
                out.push(' ');
            }
            out.push_str("\",\"author\":{\"id\":\"1\",\"username\":\"x\"}}}");
            out.into_bytes()
        })
        .collect()
}

fn compressor(max_block_size: u32) -> Compressor {
    Compressor::new(CompressorConfig {
        level: LEVEL,
        window_log: WINDOW_LOG,
        max_block_size,
    })
    .expect("bench compressor")
}

fn decompressor() -> Decompressor {
    Decompressor::new(DECOMPRESS_WINDOW_LOG_MAX).expect("bench decompressor")
}

fn median(mut samples: Vec<f64>) -> f64 {
    samples.sort_by(f64::total_cmp);
    samples.get(samples.len() / 2).copied().unwrap_or(f64::NAN)
}

fn ready_compress_ns(body: &[u8], max_block_size: u32, runs: usize) -> f64 {
    let samples = (0..runs)
        .map(|_| {
            let mut c = compressor(max_block_size);
            let start = Instant::now();
            black_box(c.compress(black_box(body)).expect("compress"));
            start.elapsed().as_nanos() as f64
        })
        .collect();
    median(samples)
}

fn warm_compressor(body: &[u8]) -> Compressor {
    let mut c = compressor(BLOCK_LIMIT);
    c.compress(body).expect("warm up");
    c
}

fn client_frames(count: usize) -> Vec<Vec<u8>> {
    let mut client = compressor(NO_LIMIT);
    dispatches(count, 2_048)
        .iter()
        .map(|payload| client.compress(payload).expect("client frame"))
        .collect()
}

fn corpus_ratio(bodies: &[(&'static str, Vec<u8>)]) -> f64 {
    let mut input = 0usize;
    let mut output = 0usize;
    let mut add = |c: &mut Compressor, payload: &[u8]| {
        input += payload.len();
        output += c.compress(payload).expect("corpus frame").len();
    };
    for (_, body) in bodies {
        add(&mut compressor(BLOCK_LIMIT), body);
    }
    let mut session = compressor(BLOCK_LIMIT);
    add(&mut session, &bodies[1].1);
    for payload in dispatches(64, 1_024) {
        add(&mut session, &payload);
    }
    let mut client = compressor(BLOCK_LIMIT);
    for payload in dispatches(4_096, 2_048) {
        add(&mut client, &payload);
    }
    input as f64 / output as f64
}

fn decompress_run(frames: &[Vec<u8>], iters: u64) -> Duration {
    let mut total = Duration::ZERO;
    let mut done = 0u64;
    while done < iters {
        let mut d = decompressor();
        for frame in frames {
            if done == iters {
                break;
            }
            let start = Instant::now();
            black_box(
                d.decompress(black_box(frame), MAX_DECOMPRESSED)
                    .expect("decompress"),
            );
            total += start.elapsed();
            done += 1;
        }
    }
    total
}

fn bench_ready(c: &mut Criterion, bodies: &[(&'static str, Vec<u8>)]) {
    for (id, body) in bodies {
        let mut group = c.benchmark_group(*id);
        group.sample_size(10);
        for (label, limit) in [("limit_16k", BLOCK_LIMIT), ("no_limit", NO_LIMIT)] {
            group.bench_function(label, |b| {
                b.iter_batched(
                    || compressor(limit),
                    |mut c| {
                        let out = c.compress(black_box(body)).expect("compress");
                        (c, out)
                    },
                    BatchSize::PerIteration,
                )
            });
        }
        group.finish();
    }
}

fn bench_small_dispatch(c: &mut Criterion, ready: &[u8]) {
    let payloads = dispatches(64, 1_024);
    let mut warm = warm_compressor(ready);
    let mut n = 0usize;
    c.bench_function("zstd_small_dispatch", |b| {
        b.iter(|| {
            n = (n + 1) % payloads.len();
            black_box(warm.compress(black_box(&payloads[n])).expect("compress"))
        })
    });
}

fn bench_decompress(c: &mut Criterion, frames: &[Vec<u8>]) {
    c.bench_function("zstd_decompress_frame", |b| {
        b.iter_custom(|iters| decompress_run(frames, iters))
    });
}

fn three_chunks(payload: &[u8]) -> [&[u8]; 3] {
    let head = payload.len().min(48);
    let tail = payload.len().saturating_sub(1).max(head);
    [&payload[..head], &payload[head..tail], &payload[tail..]]
}

fn compress_chunks(c: &mut Compressor, chunks: &[&[u8]]) -> Vec<u8> {
    let mut out = Vec::new();
    c.compress_into(chunks, &mut out).expect("compress");
    out
}

fn bench_iolist(c: &mut Criterion, payloads: &[Vec<u8>]) {
    let mut flat = compressor(BLOCK_LIMIT);
    let mut n = 0usize;
    c.bench_function("zstd_flat_input_50kb", |b| {
        b.iter(|| {
            n = (n + 1) % payloads.len();
            black_box(flat.compress(black_box(&payloads[n])).expect("compress"))
        })
    });
    let mut chunked = compressor(BLOCK_LIMIT);
    c.bench_function("zstd_iolist_3_chunks_50kb", |b| {
        b.iter(|| {
            n = (n + 1) % payloads.len();
            black_box(compress_chunks(
                &mut chunked,
                &three_chunks(black_box(&payloads[n])),
            ))
        })
    });
}

fn iolist_ratio(payloads: &[Vec<u8>], rounds: usize) -> (f64, f64) {
    let mut flat = compressor(BLOCK_LIMIT);
    let mut chunked = compressor(BLOCK_LIMIT);
    let mut flat_samples = Vec::with_capacity(rounds);
    let mut chunk_samples = Vec::with_capacity(rounds);
    for n in 0..rounds {
        let payload = &payloads[n % payloads.len()];
        let start = Instant::now();
        black_box(flat.compress(black_box(payload)).expect("compress"));
        flat_samples.push(start.elapsed().as_nanos() as f64);
        let start = Instant::now();
        black_box(compress_chunks(
            &mut chunked,
            &three_chunks(black_box(payload)),
        ));
        chunk_samples.push(start.elapsed().as_nanos() as f64);
    }
    (median(flat_samples), median(chunk_samples))
}

fn report(failed: &mut u32, ok: bool, line: String) {
    if !ok {
        *failed += 1;
    }
    println!("  {} {line}", if ok { "met   " } else { "MISSED" });
}

fn main() {
    let bodies: Vec<(&'static str, Vec<u8>)> = READY_SIZES
        .iter()
        .enumerate()
        .map(|(i, (id, size))| (*id, ready_body(*size, 1_000 + i as u64)))
        .collect();
    let frames = client_frames(4_096);
    let mut c = Criterion::default().configure_from_args();
    bench_ready(&mut c, &bodies);
    bench_small_dispatch(&mut c, &bodies[1].1);
    bench_decompress(&mut c, &frames);
    let large = dispatches(LARGE_DISPATCHES, LARGE_DISPATCH_BYTES);
    bench_iolist(&mut c, &large);
    c.final_summary();

    let mut failed = 0;
    println!("\ntarget check (median against target):");
    for (id, body) in &bodies {
        let limited = ready_compress_ns(body, BLOCK_LIMIT, 11);
        let unlimited = ready_compress_ns(body, NO_LIMIT, 11);
        let ratio = limited / unlimited;
        report(
            &mut failed,
            ratio <= READY_SLOWDOWN_MAX,
            format!(
                "{id}: {:.3} ms with 16 KB blocks, {:.3} ms without, ratio {ratio:.3}, target {READY_SLOWDOWN_MAX:.2}",
                limited / 1e6,
                unlimited / 1e6,
            ),
        );
    }

    for (label, limit) in [("16 KB blocks", BLOCK_LIMIT), ("no limit", NO_LIMIT)] {
        let mut c = compressor(limit);
        c.compress(&bodies[1].1).expect("first frame");
        let bytes = c.context_bytes();
        let ok = limit == NO_LIMIT || bytes <= CONTEXT_BYTES_MAX;
        let target = if limit == NO_LIMIT {
            "reference".to_string()
        } else {
            format!("target {} KB", CONTEXT_BYTES_MAX / 1024)
        };
        report(
            &mut failed,
            ok,
            format!(
                "zstd_context_bytes ({label}): {} KB, {target}",
                bytes / 1024
            ),
        );
    }

    let payloads = dispatches(64, 1_024);
    let mut warm = warm_compressor(&bodies[1].1);
    let samples = (0..20_000)
        .map(|n| {
            let payload = &payloads[n % payloads.len()];
            let start = Instant::now();
            black_box(warm.compress(black_box(payload)).expect("compress"));
            start.elapsed().as_nanos() as f64
        })
        .collect();
    let small = median(samples);
    report(
        &mut failed,
        small <= SMALL_DISPATCH_MAX_NS,
        format!(
            "zstd_small_dispatch: {:.3} us, target {:.3} us",
            small / 1e3,
            SMALL_DISPATCH_MAX_NS / 1e3
        ),
    );

    let per_frame = decompress_run(&frames, 20_000).as_nanos() as f64 / 20_000.0;
    report(
        &mut failed,
        per_frame <= DECOMPRESS_FRAME_MAX_NS,
        format!(
            "zstd_decompress_frame: {:.3} us mean, target {:.3} us",
            per_frame / 1e3,
            DECOMPRESS_FRAME_MAX_NS / 1e3
        ),
    );

    let (flat, chunked) = iolist_ratio(&large, 2_000);
    let ratio = chunked / flat;
    report(
        &mut failed,
        ratio <= IOLIST_SLOWDOWN_MAX,
        format!(
            "zstd_iolist_3_chunks_50kb: {:.3} us against {:.3} us flat, ratio {ratio:.3}, target {IOLIST_SLOWDOWN_MAX:.2}",
            chunked / 1e3,
            flat / 1e3,
        ),
    );

    let ratio = corpus_ratio(&bodies);
    report(
        &mut failed,
        ratio >= CORPUS_RATIO_MIN,
        format!("zstd_corpus_ratio: {ratio:.4}, target {CORPUS_RATIO_MIN:.2} or more"),
    );

    println!("\ncriterion targets: {failed} missed");
    if failed > 0 {
        std::process::exit(1);
    }
}
