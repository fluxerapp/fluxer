// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::{Arc, Condvar, Mutex, MutexGuard};
use std::time::Duration;

use block2::RcBlock;
use napi::bindgen_prelude::Result;
use napi_derive::napi;

use objc2::rc::Retained;
use objc2_foundation::NSError;
use objc2_screen_capture_kit::SCShareableContent;

use crate::permission_probe::{
    PROBE_TIMEOUT_NS, ShareableContentOutcome, screen_recording_probe_result,
};

#[derive(Debug, Eq, PartialEq)]
enum AsyncError {
    Timeout,
    SckErr,
}

struct WakerInner {
    state: Mutex<WakerState>,
    cv: Condvar,
}

struct WakerState {
    done: bool,
    failed: bool,
    err: Option<Retained<NSError>>,
    content: Option<Retained<SCShareableContent>>,
}

unsafe impl Send for WakerInner {}
unsafe impl Sync for WakerInner {}

fn new_waker() -> Arc<WakerInner> {
    Arc::new(WakerInner {
        state: Mutex::new(WakerState {
            done: false,
            failed: false,
            err: None,
            content: None,
        }),
        cv: Condvar::new(),
    })
}

fn lock_waker_state(w: &WakerInner) -> MutexGuard<'_, WakerState> {
    w.state
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn wait_deadline(w: &Arc<WakerInner>, timeout_ns: u64) -> bool {
    let s = lock_waker_state(w);
    if s.done {
        return true;
    }
    let dur = Duration::from_nanos(timeout_ns);
    match w.cv.wait_timeout(s, dur) {
        Ok((state, _)) => state.done,
        Err(poisoned) => poisoned.into_inner().0.done,
    }
}

fn retain_error(err: *mut NSError) -> std::result::Result<Option<Retained<NSError>>, AsyncError> {
    if err.is_null() {
        Ok(None)
    } else {
        unsafe { Retained::retain(err) }
            .map(Some)
            .ok_or(AsyncError::SckErr)
    }
}

fn retain_content(
    content: *mut SCShareableContent,
) -> std::result::Result<Option<Retained<SCShareableContent>>, AsyncError> {
    if content.is_null() {
        Ok(None)
    } else {
        unsafe { Retained::retain(content) }
            .map(Some)
            .ok_or(AsyncError::SckErr)
    }
}

fn complete_waker(
    waker: &WakerInner,
    err: Option<Retained<NSError>>,
    content: Option<Retained<SCShareableContent>>,
    failed: bool,
) {
    let mut s = lock_waker_state(waker);
    s.err = err;
    s.content = content;
    s.failed = failed;
    s.done = true;
    waker.cv.notify_all();
}

fn get_shareable_content(
    timeout_ns: u64,
) -> std::result::Result<Retained<SCShareableContent>, AsyncError> {
    let waker = new_waker();
    let waker_cb = waker.clone();
    let blk = RcBlock::new(move |content: *mut SCShareableContent, err: *mut NSError| {
        let (err_opt, err_failed) = match retain_error(err) {
            Ok(opt) => (opt, false),
            Err(_) => (None, true),
        };
        let (content_opt, content_failed) = match retain_content(content) {
            Ok(opt) => (opt, false),
            Err(_) => (None, true),
        };
        complete_waker(
            &waker_cb,
            err_opt,
            content_opt,
            err_failed || content_failed,
        );
    });
    unsafe {
        SCShareableContent::getShareableContentExcludingDesktopWindows_onScreenWindowsOnly_completionHandler(
            false, false, &blk,
        );
    }
    if !wait_deadline(&waker, timeout_ns) {
        return Err(AsyncError::Timeout);
    }
    let mut s = lock_waker_state(&waker);
    if s.failed || s.err.is_some() {
        return Err(AsyncError::SckErr);
    }
    s.content.take().ok_or(AsyncError::SckErr)
}

fn shareable_content_outcome(timeout_ns: u64) -> ShareableContentOutcome {
    match get_shareable_content(timeout_ns) {
        Ok(_) => ShareableContentOutcome::Delivered,
        Err(AsyncError::Timeout) => ShareableContentOutcome::TimedOut,
        Err(AsyncError::SckErr) => ShareableContentOutcome::Failed,
    }
}

#[napi(js_name = "probeScreenRecordingAccess")]
pub async fn probe_screen_recording_access() -> Result<String> {
    use objc2::runtime::AnyClass;
    let sck_available = AnyClass::get(c"SCShareableContent").is_some();
    let outcome = if sck_available {
        shareable_content_outcome(PROBE_TIMEOUT_NS)
    } else {
        ShareableContentOutcome::Failed
    };
    Ok(screen_recording_probe_result(sck_available, outcome).to_owned())
}
