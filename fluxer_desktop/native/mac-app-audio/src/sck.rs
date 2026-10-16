#![allow(non_snake_case)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use objc2_core_foundation::CGRect;
use objc2_core_media::{CMTime, CMTimeFlags};
use objc2_foundation::NSString;
use objc2_screen_capture_kit::SCStreamConfiguration;

pub use objc2_screen_capture_kit::SCStreamOutputType;

pub fn cgrect_standardized(r: CGRect) -> CGRect {
    let mut out = r;
    if out.size.width < 0.0 {
        out.origin.x += out.size.width;
        out.size.width = -out.size.width;
    }
    if out.size.height < 0.0 {
        out.origin.y += out.size.height;
        out.size.height = -out.size.height;
    }
    out
}

pub fn cgrect_intersection_area(a_raw: CGRect, b_raw: CGRect) -> f64 {
    let a = cgrect_standardized(a_raw);
    let b = cgrect_standardized(b_raw);
    if a.size.width <= 0.0 || a.size.height <= 0.0 || b.size.width <= 0.0 || b.size.height <= 0.0 {
        return 0.0;
    }
    let ax2 = a.origin.x + a.size.width;
    let ay2 = a.origin.y + a.size.height;
    let bx2 = b.origin.x + b.size.width;
    let by2 = b.origin.y + b.size.height;
    let x1 = a.origin.x.max(b.origin.x);
    let y1 = a.origin.y.max(b.origin.y);
    let x2 = ax2.min(bx2);
    let y2 = ay2.min(by2);
    if x2 <= x1 || y2 <= y1 {
        return 0.0;
    }
    (x2 - x1) * (y2 - y1)
}

pub fn cmtime_seconds(value: i64, timescale: i32) -> CMTime {
    CMTime {
        value,
        timescale,
        flags: CMTimeFlags(1),
        epoch: 0,
    }
}

pub fn cfg_set_capture_dynamic_range_sdr_if_available(cfg: &SCStreamConfiguration) {
    use objc2::runtime::NSObjectProtocol;
    use objc2::sel;
    let obj: &objc2::runtime::NSObject = cfg.as_ref();
    if obj.respondsToSelector(sel!(setCaptureDynamicRange:)) {
        unsafe {
            cfg.setCaptureDynamicRange(objc2_screen_capture_kit::SCCaptureDynamicRange(0));
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
