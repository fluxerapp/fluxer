// SPDX-License-Identifier: AGPL-3.0-or-later

use criterion::{Criterion, criterion_group, criterion_main};
use fluxer_media_proxy::{
    asset_hash::AssetHash,
    aws_sigv4,
    image_quality::ImageQuality,
    image_transform::{EncodeEffort, ResizeMode},
    mime,
    output_format::OutputFormat,
    percent_decode, range,
    server::transform::{
        cache_key::{TransformCacheKeyInput, transform_cache_key},
        parameters::TransformRoute,
    },
    signing, thumbhash,
};
use std::hint::black_box;

fn vp8x_webp_header() -> [u8; 30] {
    let mut webp = [0u8; 30];
    webp[0..4].copy_from_slice(b"RIFF");
    webp[8..12].copy_from_slice(b"WEBP");
    webp[12..16].copy_from_slice(b"VP8X");
    webp[16..20].copy_from_slice(&10u32.to_le_bytes());
    webp[20] = 0x12;
    webp[24..27].copy_from_slice(&[0xff, 0x03, 0]);
    webp[27..30].copy_from_slice(&[0xff, 0x03, 0]);
    webp
}

fn external_cache_key_input(identity: &str) -> TransformCacheKeyInput<'_> {
    TransformCacheKeyInput {
        route: TransformRoute::External,
        asset_kind: None,
        cache_identity: identity,
        width: Some(1024),
        height: Some(1024),
        format: OutputFormat::WebP,
        quality: Some(ImageQuality::High),
        animated: false,
        effort: EncodeEffort::parse_lenient("4"),
        resize_mode: Some(ResizeMode::Fit),
    }
}

fn bench_range(c: &mut Criterion) {
    c.bench_function("range_parse_explicit", |b| {
        b.iter(|| range::parse_range(Some("bytes=1024-65535"), 10 * 1024 * 1024))
    });
    c.bench_function("content_range_parse", |b| {
        b.iter(|| range::parse_content_range(black_box(Some("bytes 1048576-2097151/8388608"))))
    });
}

fn bench_signing(c: &mut Criterion) {
    c.bench_function("external_signature", |b| {
        b.iter(|| {
            signing::create_signature("v2/aHR0cHM6Ly9leGFtcGxlLmNvbS9pbWFnZS5wbmc", b"secret")
        })
    });
    c.bench_function("aws_sigv4_get", |b| {
        b.iter(|| {
            let mut opts = aws_sigv4::Options::new(
                "GET",
                "https://examplebucket.s3.amazonaws.com/test.txt",
                "us-east-1",
                "AKIAIOSFODNN7EXAMPLE",
                "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
            );
            opts.timestamp = Some(aws_sigv4::format_timestamp(2013, 5, 24, 0, 0, 0));
            aws_sigv4::sign(opts).unwrap()
        })
    });
}

fn bench_mime(c: &mut Criterion) {
    let png = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x20\x00\x00\x00\x20\x08\x06";
    c.bench_function("mime_sniff_png", |b| b.iter(|| mime::sniff(png)));
    let webp = vp8x_webp_header();
    c.bench_function("mime_sniff_webp_vp8x", |b| {
        b.iter(|| mime::sniff(black_box(&webp)))
    });
}

fn bench_percent_decode(c: &mut Criterion) {
    let encoded = "users/not-decoded/photo%20name-%E2%82%AC.webp";
    c.bench_function("percent_decode_utf8", |b| {
        b.iter(|| percent_decode::decode_utf8(black_box(encoded)).unwrap())
    });
}

fn bench_asset_hash(c: &mut Criterion) {
    c.bench_function("asset_hash_parse", |b| {
        b.iter(|| AssetHash::parse(black_box("deadbeef")))
    });
    c.bench_function("asset_hash_parse_animated", |b| {
        b.iter(|| AssetHash::parse(black_box("a_deadbeef")))
    });
}

fn bench_transform_cache_key(c: &mut Criterion) {
    let identity =
        "https://example.invalid/media/asset.png?width=1024&height=1024&token=0123456789abcdef";
    c.bench_function("external_transform_cache_key", |b| {
        b.iter(|| transform_cache_key(external_cache_key_input(black_box(identity))))
    });
}

fn bench_thumbhash(c: &mut Criterion) {
    let mut pixels = vec![0u8; 64 * 64 * 4];
    for (i, px) in pixels.chunks_exact_mut(4).enumerate() {
        px[0] = (i % 64) as u8;
        px[1] = (i / 64) as u8;
        px[2] = 128;
        px[3] = 255;
    }
    c.bench_function("thumbhash_64_rgba", |b| {
        b.iter(|| thumbhash::encode_rgba(&pixels, 64, 64).unwrap())
    });
}

criterion_group!(
    benches,
    bench_range,
    bench_signing,
    bench_mime,
    bench_percent_decode,
    bench_asset_hash,
    bench_transform_cache_key,
    bench_thumbhash
);
criterion_main!(benches);
