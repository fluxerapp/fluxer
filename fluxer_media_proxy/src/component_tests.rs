// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{media_process::native_runtime::ensure_vips_init, native};
use std::ffi::CString;

#[test]
fn blocked_libvips_loaders_cannot_decode_untrusted_input() {
    ensure_vips_init().expect("libvips initialises");
    let ppm = b"P6\n2 2\n255\n\x00\x00\x00\xff\xff\xff\x00\x00\x00\xff\xff\xff".to_vec();
    let options = CString::new("access=sequential").expect("static string has no NUL");
    let raw = unsafe {
        native::fluxer_vips_image_new_from_buffer(ppm.as_ptr().cast(), ppm.len(), options.as_ptr())
    };
    assert!(raw.is_null());
    unsafe { native::fluxer_vips_error_clear() };
}

#[test]
fn full_size_wallpaper_fits_a_small_memory_budget_instead_of_failing() {
    use crate::{
        image_quality::ImageQuality,
        image_transform::{AnimationMode, ImageOptions, ResizeMode},
        media_process::{MediaLimits, NativeCost, image_transform_cost, transform_image},
        metrics::transform::TransformMetrics,
        mime,
        output_format::OutputFormat,
        test_fixtures::synthetic_png,
    };
    let budget = 240 << 20;
    let limits = MediaLimits::default_from_config().with_native_memory_budget(budget);
    let input = synthetic_png(3840, 2160);
    let options = ImageOptions {
        width: None,
        height: None,
        format: OutputFormat::WebP,
        quality: ImageQuality::High,
        animation: AnimationMode::Static,
        effort_override: None,
        resize_mode: ResizeMode::Fit,
        deadline_ms: None,
    };
    let NativeCost::Bytes(cost) = image_transform_cost(&input, &options, &limits).unwrap() else {
        panic!("a still image has a fixed cost");
    };
    assert!(cost <= budget);
    let output = transform_image(&input, &options, &limits, &TransformMetrics::default()).unwrap();
    let served = mime::sniff(&output.bytes);
    assert_eq!("image/webp", served.mime);
    assert!(served.width >= 2400 && served.width < 3840);
    assert!((served.width * 9).abs_diff(served.height * 16) <= 16);
}
