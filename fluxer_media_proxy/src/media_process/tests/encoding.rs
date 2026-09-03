// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::encoding::anim_limits_from_options;
use super::super::transform_plan::{AutoQualitySource, output_suffix, resolve_auto_quality};
use super::super::{AnimatedProbe, EncodeEffort, ImageOptions, ImageQuality, ResolvedImageQuality};
use super::fixtures::effort;
use crate::{mime, output_format::OutputFormat};

#[test]
fn webp_effort_override_is_clamped_to_the_encoder_maximum() {
    for requested in [7u8, 8, 9, 200] {
        let suffix = webp_suffix(effort(requested));
        assert!(
            suffix.contains("effort=6"),
            "effort={requested} produced {suffix}, but libvips webpsave rejects effort above 6 \
             and silently falls back to its own default"
        );
    }
    assert!(webp_suffix(effort(5)).contains("effort=5"));
}

fn webp_suffix(requested: EncodeEffort) -> String {
    output_suffix(
        OutputFormat::WebP,
        ResolvedImageQuality::High,
        None,
        Some(requested),
    )
    .expect("webp output suffix")
    .into_string()
    .expect("utf8 output suffix")
}

#[test]
fn animated_webp_default_effort_matches_fast_tier() {
    assert_eq!(2, ResolvedImageQuality::Low.default_effort(true));
    assert_eq!(2, ResolvedImageQuality::High.default_effort(true));
    assert_eq!(4, ResolvedImageQuality::High.default_effort(false));
}

#[test]
fn auto_animated_palette_quality_matches_v1_thresholds() {
    let gif_header = b"GIF89a\x01\x00\x01\x00";
    let small_probe = AnimatedProbe {
        width: 300,
        height: 225,
        pages: 100,
    };
    let large_probe = AnimatedProbe {
        width: 480,
        height: 480,
        pages: 240,
    };
    let resolved = |quality, probe, requested| {
        resolve_auto_quality(
            AutoQualitySource {
                format: OutputFormat::WebP,
                animated: true,
                sniffed_mime: mime::sniff(gif_header).mime,
                input: gif_header,
                quality,
                probe: Some(probe),
            },
            requested,
        )
    };

    assert_eq!(
        ResolvedImageQuality::Lossless,
        resolved(ImageQuality::Auto, small_probe, None).quality
    );
    assert_eq!(
        ResolvedImageQuality::High,
        resolved(ImageQuality::Auto, large_probe, None).quality
    );
    assert_eq!(
        ResolvedImageQuality::High,
        resolved(ImageQuality::High, small_probe, None).quality
    );
    assert_eq!(
        ResolvedImageQuality::Low,
        resolved(ImageQuality::Low, small_probe, None).quality
    );
    assert_eq!(
        None,
        resolved(ImageQuality::Auto, small_probe, None).effort_override
    );
    assert_eq!(
        Some(EncodeEffort::minimum()),
        resolved(ImageQuality::Auto, large_probe, None).effort_override
    );
    assert_eq!(
        Some(effort(2)),
        resolved(ImageQuality::Auto, large_probe, Some(effort(2))).effort_override
    );
}

#[test]
fn animated_encode_deadline_keeps_flush_headroom() {
    assert_eq!(
        Some(17_000),
        anim_limits_from_options(&ImageOptions {
            deadline_ms: Some(20_000),
            ..Default::default()
        })
        .deadline_unix_ms
    );
    assert_eq!(
        Some(1_500),
        anim_limits_from_options(&ImageOptions {
            deadline_ms: Some(1_500),
            ..Default::default()
        })
        .deadline_unix_ms
    );
    assert_eq!(
        None,
        anim_limits_from_options(&ImageOptions::default()).deadline_unix_ms
    );
    assert_eq!(
        Some(20_000),
        anim_limits_from_options(&ImageOptions {
            deadline_ms: Some(20_000),
            ..Default::default()
        })
        .flush_deadline_unix_ms
    );
}
