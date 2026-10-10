#![allow(non_snake_case)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use objc2::rc::Retained;
use objc2_core_foundation::{CFString, CGFloat, CGRect};
use objc2_core_media::{CMTime, CMTimeFlags};
use objc2_foundation::NSString;
use objc2_screen_capture_kit::{
    SCCaptureDynamicRange, SCContentFilter, SCDisplay, SCRunningApplication, SCStream,
    SCStreamConfiguration, SCWindow,
};

use crate::config::{SckCaptureConfig, SckColorSpace, SckPixelFormat};

pub use objc2_screen_capture_kit::SCStreamOutputType;

pub fn cmtime_seconds(value: i64, timescale: i32) -> CMTime {
    CMTime {
        value,
        timescale,
        flags: CMTimeFlags(1),
        epoch: 0,
    }
}

pub fn sc_running_application_process_id(app: &SCRunningApplication) -> i32 {
    unsafe { app.processID() }
}

pub fn sc_running_application_bundle_identifier(app: &SCRunningApplication) -> Retained<NSString> {
    unsafe { app.bundleIdentifier() }
}

pub fn sc_running_application_name(app: &SCRunningApplication) -> Retained<NSString> {
    unsafe { app.applicationName() }
}

pub fn sc_display_frame(display: &SCDisplay) -> CGRect {
    unsafe { display.frame() }
}

pub fn sc_display_display_id(display: &SCDisplay) -> u32 {
    unsafe { display.displayID() }
}

pub fn sc_display_width(display: &SCDisplay) -> isize {
    unsafe { display.width() }
}

pub fn sc_display_height(display: &SCDisplay) -> isize {
    unsafe { display.height() }
}

pub fn sc_window_window_id(win: &SCWindow) -> u32 {
    unsafe { win.windowID() }
}

pub fn sc_window_owning_application(win: &SCWindow) -> Option<Retained<SCRunningApplication>> {
    unsafe { win.owningApplication() }
}

pub fn sc_window_frame(win: &SCWindow) -> CGRect {
    unsafe { win.frame() }
}

pub fn sc_window_title(win: &SCWindow) -> Option<Retained<NSString>> {
    unsafe { win.title() }
}

pub fn sc_window_is_on_screen(win: &SCWindow) -> bool {
    unsafe { win.isOnScreen() }
}

pub fn filter_content_rect_if_available(filter: &SCContentFilter) -> Option<CGRect> {
    use objc2::runtime::NSObjectProtocol;
    use objc2::{msg_send, sel};
    let obj: &objc2::runtime::NSObject = filter.as_ref();
    if !obj.respondsToSelector(sel!(contentRect)) {
        return None;
    }
    let rect: CGRect = unsafe { msg_send![obj, contentRect] };
    if rect.size.width <= 0.0 || rect.size.height <= 0.0 {
        return None;
    }
    Some(rect)
}

pub fn filter_point_pixel_scale_if_available(filter: &SCContentFilter) -> Option<f32> {
    use objc2::runtime::NSObjectProtocol;
    use objc2::{msg_send, sel};
    let obj: &objc2::runtime::NSObject = filter.as_ref();
    if !obj.respondsToSelector(sel!(pointPixelScale)) {
        return None;
    }
    let scale: CGFloat = unsafe { msg_send![obj, pointPixelScale] };
    if !scale.is_finite() || scale <= 0.0 {
        return None;
    }
    Some(scale as f32)
}

pub fn cfg_set_scales_to_fit_if_available(cfg: &SCStreamConfiguration, scales_to_fit: bool) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::{msg_send, sel};
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setScalesToFit:)) {
        unsafe {
            let _: () = msg_send![obj, setScalesToFit: scales_to_fit];
        }
    }
}

pub fn cfg_set_width(cfg: &SCStreamConfiguration, v: usize) {
    unsafe { cfg.setWidth(v) }
}
pub fn cfg_set_height(cfg: &SCStreamConfiguration, v: usize) {
    unsafe { cfg.setHeight(v) }
}
pub fn cfg_set_queue_depth(cfg: &SCStreamConfiguration, v: isize) {
    unsafe { cfg.setQueueDepth(v) }
}
pub fn cfg_set_shows_cursor(cfg: &SCStreamConfiguration, v: bool) {
    unsafe { cfg.setShowsCursor(v) }
}
pub fn cfg_set_minimum_frame_interval(cfg: &SCStreamConfiguration, t: CMTime) {
    unsafe { cfg.setMinimumFrameInterval(t) }
}
pub fn cfg_set_pixel_format(cfg: &SCStreamConfiguration, format: u32) {
    unsafe { cfg.setPixelFormat(format) }
}

pub fn cfg_set_source_rect_if_available(cfg: &SCStreamConfiguration, rect: CGRect) -> bool {
    use objc2::runtime::NSObjectProtocol;
    use objc2::{msg_send, sel};
    assert!(rect.size.width > 0.0);
    assert!(rect.size.height > 0.0);
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if !obj.respondsToSelector(sel!(setSourceRect:)) {
        return false;
    }
    unsafe {
        let _: () = msg_send![obj, setSourceRect: rect];
    }
    true
}

pub fn cfg_set_capture_dynamic_range_sdr_if_available(cfg: &SCStreamConfiguration) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::sel;
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setCaptureDynamicRange:)) {
        unsafe {
            cfg.setCaptureDynamicRange(SCCaptureDynamicRange::SDR);
        }
    }
}

pub fn cfg_set_capture_dynamic_range_hdr_if_available(cfg: &SCStreamConfiguration) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::sel;
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setCaptureDynamicRange:)) {
        unsafe {
            cfg.setCaptureDynamicRange(SCCaptureDynamicRange::HDRLocalDisplay);
        }
    }
}

pub fn cfg_set_color_space_name_if_available(cfg: &SCStreamConfiguration, name: &str) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::sel;
    assert!(!name.is_empty());
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setColorSpaceName:)) {
        let cf = CFString::from_str(name);
        unsafe {
            cfg.setColorSpaceName(&cf);
        }
    }
}

pub fn obs_minimum_frame_interval(target_fps: u32) -> CMTime {
    assert!(target_fps >= crate::config::FPS_MIN);
    assert!(target_fps <= crate::config::FPS_MAX);
    CMTime {
        value: crate::config::FRAME_INTERVAL_FACTOR_NUM as i64,
        timescale: (crate::config::FRAME_INTERVAL_FACTOR_DEN as i32)
            .saturating_mul(target_fps as i32),
        flags: CMTimeFlags(1),
        epoch: 0,
    }
}

pub fn apply_capture_config(cfg: &SCStreamConfiguration, capture: &SckCaptureConfig) {
    assert!(capture.target_fps() >= crate::config::FPS_MIN);
    assert!(capture.queue_depth() >= crate::config::QUEUE_DEPTH_MIN);
    cfg_set_queue_depth(cfg, capture.queue_depth() as isize);
    cfg_set_pixel_format(cfg, capture.pixel_format().as_fourcc());
    cfg_set_minimum_frame_interval(cfg, obs_minimum_frame_interval(capture.target_fps()));
    cfg_set_color_space_name_if_available(cfg, capture.color_space().as_cf_name());
    match capture.pixel_format() {
        SckPixelFormat::L10rHdr => {
            assert!(capture.color_space() == SckColorSpace::DisplayP3);
            cfg_set_capture_dynamic_range_hdr_if_available(cfg);
        }
        SckPixelFormat::Bgra8 | SckPixelFormat::Nv12VideoRange | SckPixelFormat::Nv12FullRange => {
            cfg_set_capture_dynamic_range_sdr_if_available(cfg);
        }
    }
    if capture.captures_audio() {
        cfg_set_captures_audio_if_available(cfg, true);
        cfg_set_audio_sample_rate_if_available(cfg, capture.audio_sample_rate_hz());
        cfg_set_audio_channel_count_if_available(cfg, capture.audio_channels());
        cfg_set_excludes_current_process_audio_if_available(cfg, true);
    } else {
        cfg_set_captures_audio_if_available(cfg, false);
    }
}

pub fn cfg_set_captures_audio_if_available(cfg: &SCStreamConfiguration, captures_audio: bool) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::sel;
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setCapturesAudio:)) {
        unsafe {
            cfg.setCapturesAudio(captures_audio);
        }
    }
}

pub fn cfg_set_audio_sample_rate_if_available(cfg: &SCStreamConfiguration, sample_rate_hz: u32) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::sel;
    assert!(sample_rate_hz >= crate::config::AUDIO_SAMPLE_RATE_MIN_HZ);
    assert!(sample_rate_hz <= crate::config::AUDIO_SAMPLE_RATE_MAX_HZ);
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setSampleRate:)) {
        unsafe {
            cfg.setSampleRate(sample_rate_hz as isize);
        }
    }
}

pub fn cfg_set_audio_channel_count_if_available(cfg: &SCStreamConfiguration, channels: u32) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::sel;
    assert!(channels >= crate::config::AUDIO_CHANNEL_COUNT_MIN);
    assert!(channels <= crate::config::AUDIO_CHANNEL_COUNT_MAX);
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setChannelCount:)) {
        unsafe {
            cfg.setChannelCount(channels as isize);
        }
    }
}

pub fn cfg_set_excludes_current_process_audio_if_available(
    cfg: &SCStreamConfiguration,
    excludes_self: bool,
) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::sel;
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setExcludesCurrentProcessAudio:)) {
        unsafe {
            cfg.setExcludesCurrentProcessAudio(excludes_self);
        }
    }
}

pub fn cfg_set_stream_name_if_available(cfg: &SCStreamConfiguration, name: &NSString) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::sel;
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setStreamName:)) {
        unsafe {
            cfg.setStreamName(Some(name));
        }
    }
}

pub fn sc_stream_add_stream_output(
    stream: &SCStream,
    output: &objc2::runtime::ProtocolObject<dyn objc2_screen_capture_kit::SCStreamOutput>,
    kind: SCStreamOutputType,
    queue: Option<&dispatch2::DispatchQueue>,
) -> Result<(), Retained<objc2_foundation::NSError>> {
    unsafe { stream.addStreamOutput_type_sampleHandlerQueue_error(output, kind, queue) }
}
