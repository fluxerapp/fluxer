// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_media_proxy::media_process::{
    AnimationLimits, AnimationMode, ImageOptions, ImageQuality, MediaLimits, ResizeMode,
    transform_image,
};
use fluxer_media_proxy::metrics::Metrics;
use fluxer_media_proxy::output_format::OutputFormat;
use std::path::PathBuf;
use std::time::Instant;

#[derive(Debug)]
struct Result {
    path: PathBuf,
    src_dim: (u16, u16),
    src_frames: usize,
    src_transparent_frames: usize,
    src_bytes: usize,
    target_dim: u32,
    out_bytes: usize,
    out_frames: usize,
    out_transparent_frames: usize,
    runs_ms: Vec<u128>,
}

fn parse_gif(bytes: &[u8]) -> Option<(u16, u16, usize, usize)> {
    if bytes.len() < 13 || (&bytes[..6] != b"GIF89a" && &bytes[..6] != b"GIF87a") {
        return None;
    }
    let w = u16::from_le_bytes([bytes[6], bytes[7]]);
    let h = u16::from_le_bytes([bytes[8], bytes[9]]);
    let gct_size = if bytes[10] & 0x80 != 0 {
        2usize.pow(((bytes[10] & 0x07) + 1) as u32)
    } else {
        0
    };
    let mut off = 13 + gct_size * 3;
    let mut frames = 0;
    let mut trans = 0;
    while off < bytes.len() {
        match bytes[off] {
            0x21 => {
                if off + 1 < bytes.len() && bytes[off + 1] == 0xF9 {
                    frames += 1;
                    if off + 3 < bytes.len() && bytes[off + 3] & 0x01 != 0 {
                        trans += 1;
                    }
                    off += 8;
                    continue;
                }
                off += 2;
                while off < bytes.len() && bytes[off] != 0 {
                    off += bytes[off] as usize + 1;
                }
                off += 1;
            }
            0x2C => {
                let lf = bytes[off + 9];
                let lct = if lf & 0x80 != 0 {
                    2usize.pow(((lf & 0x07) + 1) as u32)
                } else {
                    0
                };
                off += 10 + lct * 3 + 1;
                while off < bytes.len() && bytes[off] != 0 {
                    off += bytes[off] as usize + 1;
                }
                off += 1;
            }
            0x3B => break,
            _ => off += 1,
        }
    }
    Some((w, h, frames, trans))
}

fn main() {
    let metrics = Metrics::new();
    let media_limits = MediaLimits::default_from_config();
    let animation = AnimationMode::Animated(
        AnimationLimits::new(20_000, 30_000).expect("valid animation limits"),
    );
    let runs_per_file = std::env::var("RUNS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(3usize);
    let dir = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "/tmp/bench-gifs".to_owned());
    let mut paths: Vec<PathBuf> = std::fs::read_dir(&dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| {
            p.extension().and_then(|s| s.to_str()) == Some("gif")
                && std::fs::metadata(p)
                    .map(|m| m.len() > 1024)
                    .unwrap_or(false)
        })
        .collect();
    paths.sort();
    let mut results: Vec<Result> = Vec::new();
    for path in paths {
        let bytes = std::fs::read(&path).unwrap();
        let Some((w, h, frames, trans)) = parse_gif(&bytes) else {
            continue;
        };
        let target = (w.min(h) as u32).max(2) / 2;
        let mut runs_ms = Vec::with_capacity(runs_per_file);
        let mut last_out: Vec<u8> = Vec::new();
        for _ in 0..runs_per_file {
            let t0 = Instant::now();
            let out = transform_image(
                &bytes,
                &ImageOptions {
                    width: Some(target),
                    height: Some(target),
                    format: OutputFormat::GIF,
                    quality: ImageQuality::High,
                    animation,
                    resize_mode: ResizeMode::Fit,
                    ..Default::default()
                },
                &media_limits,
                &metrics.transform(),
            )
            .expect("transform ok");
            runs_ms.push(t0.elapsed().as_millis());
            last_out = out.bytes;
        }
        let (_, _, out_frames, out_trans) = parse_gif(&last_out).unwrap_or((0, 0, 0, 0));
        results.push(Result {
            path,
            src_dim: (w, h),
            src_frames: frames,
            src_transparent_frames: trans,
            src_bytes: bytes.len(),
            target_dim: target,
            out_bytes: last_out.len(),
            out_frames,
            out_transparent_frames: out_trans,
            runs_ms,
        });
    }
    println!(
        "{:<35} {:>10} {:>6} {:>4}/{:<4} {:>4}  →  {:>9} {:>4}/{:<4} {:>10} {:>10}",
        "file",
        "src bytes",
        "src wh",
        "fr",
        "tr",
        "tgt",
        "out bytes",
        "fr",
        "tr",
        "min ms",
        "avg ms",
    );
    for r in &results {
        let min = r.runs_ms.iter().min().copied().unwrap_or(0);
        let avg = r.runs_ms.iter().sum::<u128>() / r.runs_ms.len() as u128;
        println!(
            "{:<35} {:>10} {:>3}x{:<3} {:>4}/{:<4} {:>4}  →  {:>9} {:>4}/{:<4} {:>10} {:>10}",
            r.path.file_name().unwrap().to_string_lossy(),
            r.src_bytes,
            r.src_dim.0,
            r.src_dim.1,
            r.src_frames,
            r.src_transparent_frames,
            r.target_dim,
            r.out_bytes,
            r.out_frames,
            r.out_transparent_frames,
            min,
            avg,
        );
    }
}
