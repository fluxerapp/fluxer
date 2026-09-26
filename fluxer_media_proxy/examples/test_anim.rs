// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_media_proxy::media_process::{
    AnimationLimits, AnimationMode, ImageOptions, ImageQuality, MediaLimits, ResizeMode,
    transform_image,
};
use fluxer_media_proxy::metrics::Metrics;
use fluxer_media_proxy::output_format::OutputFormat;

fn main() {
    let metrics = Metrics::new();
    let media_limits = MediaLimits::default_from_config();
    let animation = AnimationMode::Animated(
        AnimationLimits::new(20_000, 30_000).expect("valid animation limits"),
    );
    let input = std::fs::read("/tmp/source.bin").expect("read source");
    let out = transform_image(
        &input,
        &ImageOptions {
            width: Some(240),
            height: Some(240),
            format: OutputFormat::GIF,
            quality: ImageQuality::High,
            animation,
            resize_mode: ResizeMode::Fit,
            ..Default::default()
        },
        &media_limits,
        &metrics.transform(),
    )
    .expect("transform");
    std::fs::write("/tmp/out_current.gif", &out.bytes).expect("write");
    eprintln!(
        "current (ffmpeg path) wrote /tmp/out_current.gif: {} bytes",
        out.bytes.len()
    );
}
