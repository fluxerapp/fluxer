// SPDX-License-Identifier: AGPL-3.0-or-later

use core::ffi::c_void;
use core::ptr;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread;
use std::time::Duration;

use block2::RcBlock;
use fluxer_screen_frame_bus::{NativeScreenFrameSinkHandle, NativeScreenFrameSinkHandleRef};
use napi::JsValue;
use napi::Status;
use napi::bindgen_prelude::{Float32Array, Function, Result, Unknown, ValueType};
use napi::threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode};
use napi_derive::napi;

use objc2::rc::Retained;
use objc2::{AllocAnyThread, Message};
use objc2_core_foundation::{CFDictionary, CFNumber, CFNumberType};
use objc2_core_graphics::{
    CGWindowID, CGWindowListCopyWindowInfo, CGWindowListOption, kCGWindowOwnerPID,
};
use objc2_foundation::{NSArray, NSError, NSString};
use objc2_screen_capture_kit::{
    SCContentFilter, SCDisplay, SCRunningApplication, SCStream, SCStreamConfiguration,
    SCStreamOutputType, SCWindow,
};

use crate::audio_source::{Source, SourceOptions};
use crate::coreaudio_tap::{self, Capture, CaptureScope, CreateError};
use crate::foundation;
use crate::os_version::{
    self, COREAUDIO_TAP_MIN_MACOS, SCK_MIN_MACOS, SupportClassification, classify_support,
    format_version,
};
use crate::pcm_pool::{PCM_POOL_CAP, PCM_SLOT_SAMPLES_MAX, PcmFramePool, PooledPcmFrame};
use crate::process_tree;
use crate::related_app::looks_related_by_strings;
use crate::sck;
use crate::sck_async;

const FRAME_QUEUE_LIMIT: usize = 64;

type FrameTsfn =
    ThreadsafeFunction<PooledPcmFrame, (), Float32Array, Status, false, false, FRAME_QUEUE_LIMIT>;
type LifecycleTsfn =
    ThreadsafeFunction<(String, String), (), (String, String), Status, false, false, 8>;
const MAX_CALLBACK_OUTPUT_FRAMES: u32 = 192_000;
const PCM_CALLBACK_CHANNELS: usize = 2;
const SCK_LATE_SPAWN_REFRESH_INTERVAL: Duration = Duration::from_secs(2);

struct LoopbackState {
    lifecycle_tsfn: Option<LifecycleTsfn>,
    source: Option<Box<Source>>,
    stream: Option<Retained<SCStream>>,
    coreaudio_capture: Option<Box<Capture>>,

    sck_refresher: Option<SckRefresherHandle>,
}

unsafe impl Send for LoopbackState {}
unsafe impl Sync for LoopbackState {}

struct SckRefresherHandle {
    alive: Arc<AtomicBool>,
    stream_holder: Arc<SckRefresherStream>,
}

impl SckRefresherHandle {
    fn shutdown(&mut self) {
        self.alive.store(false, Ordering::Release);

        if let Ok(mut guard) = self.stream_holder.stream.lock() {
            *guard = None;
        }
    }
}

impl Drop for SckRefresherHandle {
    fn drop(&mut self) {
        self.shutdown();
    }
}

struct SckRefresherStream {
    stream: Mutex<Option<Retained<SCStream>>>,
}

unsafe impl Send for SckRefresherStream {}
unsafe impl Sync for SckRefresherStream {}

struct LoopbackInner {
    state: Mutex<LoopbackState>,
    frame_tsfn: parking_lot::RwLock<Option<FrameTsfn>>,
    screen_audio_sink: parking_lot::RwLock<Option<Arc<NativeScreenFrameSinkHandleRef>>>,
    pcm_pool: PcmFramePool,
    pcm_frames_dropped: AtomicU64,
    running: std::sync::atomic::AtomicBool,
    tsfn_aborted: std::sync::atomic::AtomicBool,
}

fn generic_error(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::GenericFailure, reason.into())
}

fn lock_loopback_state(inner: &LoopbackInner) -> Result<MutexGuard<'_, LoopbackState>> {
    inner
        .state
        .lock()
        .map_err(|_| generic_error("ProcessLoopback state lock poisoned"))
}

fn note_tsfn_status(inner: &LoopbackInner, status: Status) {
    if status == Status::Closing {
        inner
            .tsfn_aborted
            .store(true, std::sync::atomic::Ordering::Release);
    }
}

fn retain_screen_audio_sink_handle(
    value: Unknown<'_>,
) -> Result<Arc<NativeScreenFrameSinkHandleRef>> {
    if value.get_type()? != ValueType::External {
        return Err(napi::Error::new(
            Status::InvalidArg,
            "ProcessLoopback.setScreenAudioSink expects a native external sink handle",
        ));
    }
    let raw_value = value.value();
    let mut data: *mut c_void = ptr::null_mut();
    let status =
        unsafe { napi::sys::napi_get_value_external(raw_value.env, raw_value.value, &mut data) };
    if status != napi::sys::Status::napi_ok || data.is_null() {
        return Err(napi::Error::new(
            Status::InvalidArg,
            "ProcessLoopback.setScreenAudioSink received an empty native external sink handle",
        ));
    }
    let handle = unsafe { data.cast::<NativeScreenFrameSinkHandle>().as_ref() }
        .and_then(NativeScreenFrameSinkHandle::retain_ref)
        .ok_or_else(|| {
            napi::Error::new(
                Status::InvalidArg,
                "ProcessLoopback.setScreenAudioSink received an invalid native sink handle",
            )
        })?;
    Ok(Arc::new(handle))
}

#[napi(js_name = "ProcessLoopback")]
pub struct ProcessLoopback {
    inner: std::sync::Arc<LoopbackInner>,
}

fn pooled_pcm_into_float32_array(slot: PooledPcmFrame) -> Float32Array {
    let (ptr, len, slot) = slot.into_external_parts();
    assert!(!ptr.is_null());
    assert!(len <= PCM_SLOT_SAMPLES_MAX);
    if len == 0 {
        drop(slot);
        let empty: Vec<f32> = Vec::new();
        return Float32Array::new(empty);
    }
    unsafe {
        Float32Array::with_external_data(ptr, len, move |_data, _len| {
            drop(slot);
        })
    }
}

unsafe extern "C" fn on_pcm_trampoline(
    ctx: *mut c_void,
    slot: *mut Option<PooledPcmFrame>,
    frames: u32,
) {
    if ctx.is_null() {
        return;
    }
    if slot.is_null() {
        return;
    }
    let inner = unsafe { &*(ctx as *const LoopbackInner) };
    if frames == 0 || frames > MAX_CALLBACK_OUTPUT_FRAMES {
        inner.pcm_frames_dropped.fetch_add(1, Ordering::Relaxed);
        return;
    }
    let Some(total) = (frames as usize).checked_mul(PCM_CALLBACK_CHANNELS) else {
        inner.pcm_frames_dropped.fetch_add(1, Ordering::Relaxed);
        return;
    };
    if total > PCM_SLOT_SAMPLES_MAX {
        inner.pcm_frames_dropped.fetch_add(1, Ordering::Relaxed);
        return;
    }
    if !inner.running.load(std::sync::atomic::Ordering::Acquire) {
        inner.pcm_frames_dropped.fetch_add(1, Ordering::Relaxed);
        return;
    }
    if let Some(sink) = inner.screen_audio_sink.read().as_ref().cloned() {
        let Some(frame) = (unsafe { &mut *slot }).take() else {
            return;
        };
        assert_eq!(frame.filled_len(), total);
        sink.enqueue_screen_audio_f32(
            frame.data_slice(),
            frames,
            PCM_CALLBACK_CHANNELS as u32,
            48_000,
            0,
        );
        return;
    }
    if inner
        .tsfn_aborted
        .load(std::sync::atomic::Ordering::Acquire)
    {
        inner.pcm_frames_dropped.fetch_add(1, Ordering::Relaxed);
        return;
    }
    let guard = inner.frame_tsfn.read();
    let Some(tsfn) = guard.as_ref() else {
        inner.pcm_frames_dropped.fetch_add(1, Ordering::Relaxed);
        return;
    };
    let Some(frame) = (unsafe { &mut *slot }).take() else {
        return;
    };
    assert_eq!(frame.filled_len(), total);
    let status = tsfn.call(frame, ThreadsafeFunctionCallMode::NonBlocking);
    note_tsfn_status(inner, status);
}

unsafe extern "C" fn on_stop_trampoline(ctx: *mut c_void, err: *mut NSError) {
    if ctx.is_null() {
        return;
    }
    let inner = unsafe { &*(ctx as *const LoopbackInner) };
    let was_running = inner
        .running
        .swap(false, std::sync::atomic::Ordering::AcqRel);
    if !was_running {
        return;
    }
    let Ok(state) = inner.state.lock() else {
        return;
    };
    if !err.is_null() {
        let err_ref: &NSError = unsafe { &*err };
        let msg = foundation::ns_error_localized_description(err_ref);
        let msg = if msg.is_empty() {
            "stream stopped".to_string()
        } else {
            msg
        };
        if let Some(tsfn) = state.lifecycle_tsfn.as_ref() {
            let status = tsfn.call(
                ("error".to_string(), msg),
                ThreadsafeFunctionCallMode::NonBlocking,
            );
            note_tsfn_status(inner, status);
        }
    }
    if let Some(tsfn) = state.lifecycle_tsfn.as_ref() {
        let status = tsfn.call(
            ("closed".to_string(), String::new()),
            ThreadsafeFunctionCallMode::NonBlocking,
        );
        note_tsfn_status(inner, status);
    }
}

enum BuildStreamErr {
    PidNotFound,
    NoApps,
    NoDisplays,
    Other,
}

struct BuiltStream {
    stream: Retained<SCStream>,
    source: Box<Source>,
}

fn build_stream_for_pid(
    pid: i32,
    include_process_tree: bool,
) -> std::result::Result<BuiltStream, BuildStreamErr> {
    let content =
        match sck_async::get_shareable_content(false, false, sck_async::DEFAULT_TIMEOUT_NS) {
            Ok(c) => c,
            Err(_) => return Err(BuildStreamErr::Other),
        };

    let apps: Retained<NSArray<SCRunningApplication>> = unsafe { content.content.applications() };
    let app_count = apps.count();
    if app_count == 0 {
        return Err(BuildStreamErr::NoApps);
    }

    let mut target_app: Option<Retained<SCRunningApplication>> = None;
    for i in 0..app_count {
        let app = apps.objectAtIndex(i);
        if sck::sc_running_application_process_id(&app) == pid {
            target_app = Some(app);
            break;
        }
    }
    let target_app = match target_app {
        Some(a) => a,
        None => return Err(BuildStreamErr::PidNotFound),
    };

    let (apps_array, pids) = collect_capture_apps(&apps, &target_app, include_process_tree)
        .ok_or(BuildStreamErr::Other)?;

    let empty_windows: Retained<NSArray<SCWindow>> = NSArray::new();

    let display =
        best_display_for_selection(&content.content, &pids).ok_or(BuildStreamErr::NoDisplays)?;

    let filter_alloc = SCContentFilter::alloc();
    let filter: Retained<SCContentFilter> = unsafe {
        SCContentFilter::initWithDisplay_includingApplications_exceptingWindows(
            filter_alloc,
            &display,
            &apps_array,
            &empty_windows,
        )
    };

    let cfg: Retained<SCStreamConfiguration> = unsafe { SCStreamConfiguration::new() };
    sck::cfg_set_captures_audio(&cfg, true);
    sck::cfg_set_excludes_current_process_audio(&cfg, true);
    sck::cfg_set_sample_rate(&cfg, 48_000);
    sck::cfg_set_channel_count(&cfg, 2);
    sck::cfg_set_queue_depth(&cfg, 8);
    sck::cfg_set_width(&cfg, 2);
    sck::cfg_set_height(&cfg, 2);
    sck::cfg_set_minimum_frame_interval(&cfg, sck::cmtime_seconds(1, 1));
    sck::cfg_set_shows_cursor(&cfg, false);
    sck::cfg_set_capture_dynamic_range_sdr_if_available(&cfg);

    let stream_name = format!("Fluxer ProcessLoopback Audio (pid {pid})");
    let nsname = NSString::from_str(&stream_name);
    sck::cfg_set_stream_name_if_available(&cfg, &nsname);

    let source = Source::create(SourceOptions::default()).map_err(|_| BuildStreamErr::Other)?;

    let stream_alloc = SCStream::alloc();
    let stream: Retained<SCStream> = unsafe {
        SCStream::initWithFilter_configuration_delegate(
            stream_alloc,
            &filter,
            &cfg,
            Some(source.delegate_as_delegate()),
        )
    };

    if sck::sc_stream_add_stream_output(
        &stream,
        source.delegate_as_output(),
        SCStreamOutputType::Screen,
        None,
    )
    .is_err()
    {
        return Err(BuildStreamErr::Other);
    }
    if sck::sc_stream_add_stream_output(
        &stream,
        source.delegate_as_output(),
        SCStreamOutputType::Audio,
        Some(source.output_queue()),
    )
    .is_err()
    {
        return Err(BuildStreamErr::Other);
    }

    Ok(BuiltStream { stream, source })
}

fn build_system_stream() -> std::result::Result<BuiltStream, BuildStreamErr> {
    let content =
        match sck_async::get_shareable_content(false, false, sck_async::DEFAULT_TIMEOUT_NS) {
            Ok(c) => c,
            Err(_) => return Err(BuildStreamErr::Other),
        };

    let displays: Retained<NSArray<SCDisplay>> = unsafe { content.content.displays() };
    if displays.count() == 0 {
        return Err(BuildStreamErr::NoDisplays);
    }
    let display = displays.objectAtIndex(0);
    let empty_windows: Retained<NSArray<SCWindow>> = NSArray::new();
    let apps: Retained<NSArray<SCRunningApplication>> = unsafe { content.content.applications() };
    let mut excluded_apps: Vec<Retained<SCRunningApplication>> = Vec::new();
    for i in 0..apps.count() {
        let app = apps.objectAtIndex(i);
        if is_self_or_related_process(sck::sc_running_application_process_id(&app)) {
            excluded_apps.push(app);
        }
    }
    let excluded_app_refs: Vec<&SCRunningApplication> =
        excluded_apps.iter().map(|app| app.as_ref()).collect();
    let excluded_apps_array = NSArray::from_slice(&excluded_app_refs);

    let filter_alloc = SCContentFilter::alloc();
    let filter: Retained<SCContentFilter> = unsafe {
        SCContentFilter::initWithDisplay_excludingApplications_exceptingWindows(
            filter_alloc,
            &display,
            &excluded_apps_array,
            &empty_windows,
        )
    };

    let cfg: Retained<SCStreamConfiguration> = unsafe { SCStreamConfiguration::new() };
    sck::cfg_set_captures_audio(&cfg, true);
    sck::cfg_set_excludes_current_process_audio(&cfg, true);
    sck::cfg_set_sample_rate(&cfg, 48_000);
    sck::cfg_set_channel_count(&cfg, 2);
    sck::cfg_set_queue_depth(&cfg, 8);
    sck::cfg_set_width(&cfg, 2);
    sck::cfg_set_height(&cfg, 2);
    sck::cfg_set_minimum_frame_interval(&cfg, sck::cmtime_seconds(1, 1));
    sck::cfg_set_shows_cursor(&cfg, false);
    sck::cfg_set_capture_dynamic_range_sdr_if_available(&cfg);

    let nsname = NSString::from_str("Fluxer System Audio");
    sck::cfg_set_stream_name_if_available(&cfg, &nsname);

    let source = Source::create(SourceOptions::default()).map_err(|_| BuildStreamErr::Other)?;

    let stream_alloc = SCStream::alloc();
    let stream: Retained<SCStream> = unsafe {
        SCStream::initWithFilter_configuration_delegate(
            stream_alloc,
            &filter,
            &cfg,
            Some(source.delegate_as_delegate()),
        )
    };

    if sck::sc_stream_add_stream_output(
        &stream,
        source.delegate_as_output(),
        SCStreamOutputType::Screen,
        None,
    )
    .is_err()
    {
        return Err(BuildStreamErr::Other);
    }
    if sck::sc_stream_add_stream_output(
        &stream,
        source.delegate_as_output(),
        SCStreamOutputType::Audio,
        Some(source.output_queue()),
    )
    .is_err()
    {
        return Err(BuildStreamErr::Other);
    }

    Ok(BuiltStream { stream, source })
}

fn collect_capture_apps(
    apps: &NSArray<SCRunningApplication>,
    target_app: &SCRunningApplication,
    include_tree: bool,
) -> Option<(Retained<NSArray<SCRunningApplication>>, Vec<i32>)> {
    let count = apps.count();
    let capacity = if include_tree { count.max(1) } else { 1 };
    let mut chosen: Vec<Retained<SCRunningApplication>> = Vec::with_capacity(capacity);
    let mut pids: Vec<i32> = Vec::with_capacity(capacity);

    let target_pid = sck::sc_running_application_process_id(target_app);
    let target_info = process_tree::info_for_pid(target_pid);
    let target_bundle = sck::sc_running_application_bundle_identifier(target_app).to_string();
    let target_name = sck::sc_running_application_name(target_app).to_string();

    if include_tree {
        for i in 0..count {
            if pids.len() >= capacity {
                break;
            }
            let candidate = apps.objectAtIndex(i);
            let candidate_pid = sck::sc_running_application_process_id(&candidate);
            let candidate_bundle =
                sck::sc_running_application_bundle_identifier(&candidate).to_string();
            let candidate_name = sck::sc_running_application_name(&candidate).to_string();
            let same_tree =
                process_tree::is_same_launch_tree(candidate_pid, target_pid, target_info);
            let related = candidate_pid == target_pid
                || looks_related_by_strings(
                    &candidate_bundle,
                    &target_bundle,
                    &candidate_name,
                    &target_name,
                );
            if (same_tree || related) && !pids.contains(&candidate_pid) {
                chosen.push(candidate);
                pids.push(candidate_pid);
            }
        }
    }

    if pids.is_empty() {
        let pid = sck::sc_running_application_process_id(target_app);
        chosen.push(target_app.retain());
        pids.push(pid);
    }
    let refs: Vec<&SCRunningApplication> = chosen.iter().map(|r| r.as_ref()).collect();
    Some((NSArray::from_slice(&refs), pids))
}

fn best_display_for_selection(
    content: &objc2_screen_capture_kit::SCShareableContent,
    pids: &[i32],
) -> Option<Retained<SCDisplay>> {
    let displays: Retained<NSArray<SCDisplay>> = unsafe { content.displays() };
    let display_count = displays.count();
    if display_count == 0 {
        return None;
    }
    let first_display = displays.objectAtIndex(0);

    let windows: Retained<NSArray<SCWindow>> = unsafe { content.windows() };
    let window_count = windows.count();
    if window_count == 0 {
        return Some(first_display);
    }

    let mut best_display = first_display.clone();
    let mut best_area = 0.0f64;
    let mut saw_app_window = false;

    for d in 0..display_count {
        let display = displays.objectAtIndex(d);
        let display_frame = sck::sc_display_frame(&display);
        let mut area = 0.0;
        for w in 0..window_count {
            let window = windows.objectAtIndex(w);
            let owner = match sck::sc_window_owning_application(&window) {
                Some(o) => o,
                None => continue,
            };
            let owner_pid = sck::sc_running_application_process_id(&owner);
            if !pids.contains(&owner_pid) {
                continue;
            }
            saw_app_window = true;
            area += sck::cgrect_intersection_area(display_frame, sck::sc_window_frame(&window));
        }
        if area > best_area {
            best_area = area;
            best_display = display;
        }
    }

    if !saw_app_window || best_area <= 0.0 {
        Some(first_display)
    } else {
        Some(best_display)
    }
}

fn is_self_or_related_process(pid: i32) -> bool {
    let self_pid = unsafe { libc::getpid() };
    pid == self_pid || process_tree::collect_related_pids(self_pid, 512).contains(&pid)
}

fn spawn_sck_late_spawn_refresher(
    target_pid: i32,
    stream: Retained<SCStream>,
) -> Option<SckRefresherHandle> {
    let alive = Arc::new(AtomicBool::new(true));
    let alive_for_thread = alive.clone();
    let stream_holder = Arc::new(SckRefresherStream {
        stream: Mutex::new(Some(stream)),
    });
    let stream_holder_for_thread = stream_holder.clone();

    thread::Builder::new()
        .name("fluxer-mac-sck-refresh".into())
        .spawn(move || {
            run_sck_late_spawn_refresher(target_pid, stream_holder_for_thread, alive_for_thread);
        })
        .ok()?;
    Some(SckRefresherHandle {
        alive,
        stream_holder,
    })
}

fn run_sck_late_spawn_refresher(
    target_pid: i32,
    stream_holder: Arc<SckRefresherStream>,
    alive: Arc<AtomicBool>,
) {
    let mut previous: Vec<i32> = Vec::new();
    while alive.load(Ordering::Acquire) {
        thread::sleep(SCK_LATE_SPAWN_REFRESH_INTERVAL);
        if !alive.load(Ordering::Acquire) {
            break;
        }

        let content =
            match sck_async::get_shareable_content(false, false, sck_async::DEFAULT_TIMEOUT_NS) {
                Ok(c) => c,
                Err(_) => continue,
            };
        let apps: Retained<NSArray<SCRunningApplication>> =
            unsafe { content.content.applications() };
        let app_count = apps.count();
        if app_count == 0 {
            continue;
        }
        let mut target_app: Option<Retained<SCRunningApplication>> = None;
        for i in 0..app_count {
            let app = apps.objectAtIndex(i);
            if sck::sc_running_application_process_id(&app) == target_pid {
                target_app = Some(app);
                break;
            }
        }
        let target_app = match target_app {
            Some(a) => a,

            None => continue,
        };
        let Some((apps_array, pids)) = collect_capture_apps(&apps, &target_app, true) else {
            continue;
        };

        let mut sorted_pids = pids.clone();
        sorted_pids.sort_unstable();
        sorted_pids.dedup();
        if sorted_pids == previous {
            continue;
        }

        let empty_windows: Retained<NSArray<SCWindow>> = NSArray::new();
        let Some(display) = best_display_for_selection(&content.content, &pids) else {
            continue;
        };
        let filter_alloc = SCContentFilter::alloc();
        let filter: Retained<SCContentFilter> = unsafe {
            SCContentFilter::initWithDisplay_includingApplications_exceptingWindows(
                filter_alloc,
                &display,
                &apps_array,
                &empty_windows,
            )
        };

        let stream_snapshot = match stream_holder.stream.lock() {
            Ok(guard) => guard.clone(),
            Err(_) => break,
        };
        let Some(stream) = stream_snapshot else {
            break;
        };

        let completion = RcBlock::new(|_err: *mut NSError| {});
        unsafe {
            stream.updateContentFilter_completionHandler(&filter, Some(&completion));
        }
        previous = sorted_pids;
    }
}

fn stop_loopback(inner: &LoopbackInner) -> Result<()> {
    inner
        .running
        .store(false, std::sync::atomic::Ordering::Release);
    let (stream_opt, mut source, coreaudio_capture, sck_refresher) = {
        let mut state = lock_loopback_state(inner)?;
        let stream = state.stream.take();
        let source = state.source.take();
        let coreaudio_capture = state.coreaudio_capture.take();
        let refresher = state.sck_refresher.take();
        (stream, source, coreaudio_capture, refresher)
    };

    if let Some(mut r) = sck_refresher {
        r.shutdown();
    }

    if stream_opt.is_none() && coreaudio_capture.is_none() {
        return Ok(());
    }

    if let Some(_capture) = coreaudio_capture {
        return Ok(());
    }

    if let Some(s) = source.as_mut() {
        s.clear_stop_callback();
        let _ = s.state.request_stop();
    }

    if let Some(stream) = stream_opt.as_ref() {
        let _ = sck_async::stop_capture(stream, sck_async::DEFAULT_TIMEOUT_NS);
    }

    if let Some(s) = source {
        let _ = s.state.mark_stopped();
        drop(s);
    }

    Ok(())
}

#[napi]
impl ProcessLoopback {
    #[napi(constructor)]
    pub fn new() -> Self {
        let pcm_pool = PcmFramePool::new(PCM_POOL_CAP, PCM_SLOT_SAMPLES_MAX).unwrap_or_else(|_| {
            unreachable!("PCM_POOL_CAP and PCM_SLOT_SAMPLES_MAX must validate")
        });
        Self {
            inner: std::sync::Arc::new(LoopbackInner {
                state: Mutex::new(LoopbackState {
                    lifecycle_tsfn: None,
                    source: None,
                    stream: None,
                    coreaudio_capture: None,
                    sck_refresher: None,
                }),
                frame_tsfn: parking_lot::RwLock::new(None),
                screen_audio_sink: parking_lot::RwLock::new(None),
                pcm_pool,
                pcm_frames_dropped: AtomicU64::new(0),
                running: std::sync::atomic::AtomicBool::new(false),
                tsfn_aborted: std::sync::atomic::AtomicBool::new(false),
            }),
        }
    }

    #[napi]
    pub fn set_frame_callback(&self, callback: Function<Float32Array, ()>) -> Result<()> {
        let tsfn: FrameTsfn = callback
            .build_threadsafe_function::<PooledPcmFrame>()
            .max_queue_size::<FRAME_QUEUE_LIMIT>()
            .build_callback(|ctx| Ok(pooled_pcm_into_float32_array(ctx.value)))?;
        *self.inner.frame_tsfn.write() = Some(tsfn);
        self.inner
            .tsfn_aborted
            .store(false, std::sync::atomic::Ordering::Release);
        Ok(())
    }

    #[napi(js_name = "setScreenAudioSink")]
    pub fn set_screen_audio_sink(&self, sink_handle: Unknown<'_>) -> Result<()> {
        let sink = retain_screen_audio_sink_handle(sink_handle)?;
        if !sink.supports_screen_audio() {
            return Err(napi::Error::new(
                Status::InvalidArg,
                "ProcessLoopback.setScreenAudioSink handle does not support screen audio",
            ));
        }
        *self.inner.screen_audio_sink.write() = Some(sink);
        Ok(())
    }

    #[napi(js_name = "clearScreenAudioSink")]
    pub fn clear_screen_audio_sink(&self) {
        self.inner.screen_audio_sink.write().take();
    }

    #[napi]
    pub fn set_lifecycle_callback(&self, callback: Function<(String, String), ()>) -> Result<()> {
        let tsfn: LifecycleTsfn = callback
            .build_threadsafe_function::<(String, String)>()
            .max_queue_size::<8>()
            .build_callback(|ctx| Ok(ctx.value))?;
        let mut state = lock_loopback_state(&self.inner)?;
        state.lifecycle_tsfn = Some(tsfn);
        self.inner
            .tsfn_aborted
            .store(false, std::sync::atomic::Ordering::Release);
        Ok(())
    }

    #[napi]
    pub async fn start(
        &self,
        pid: i32,
        _exclude_self: Option<bool>,
        include_process_tree: Option<bool>,
        backend: Option<String>,
        capture_scope: Option<String>,
    ) -> Result<()> {
        let include_tree = include_process_tree.unwrap_or(true);
        let backend_pref = backend.as_deref().unwrap_or("auto");
        let scope_pref = capture_scope.as_deref().unwrap_or("process");

        let scope = if scope_pref == "system" {
            CaptureScope::System
        } else {
            CaptureScope::Process
        };

        if pid < 1 {
            return Err(napi::Error::new(
                Status::InvalidArg,
                "ProcessLoopback.start pid must be a positive 32-bit integer",
            ));
        }

        {
            let detected = os_version::current_macos_version();
            let SupportClassification {
                supported,
                sck_available,
                coreaudio_available,
                reason,
            } = classify_support(detected);

            let backend_ok = match backend_pref {
                "coreaudio" => coreaudio_available,
                "sck" => sck_available,
                _ => supported,
            };
            if !backend_ok {
                return Err(napi::Error::new(Status::GenericFailure, reason));
            }
        }
        if scope == CaptureScope::Process && is_self_or_related_process(pid) {
            return Err(napi::Error::new(
                Status::GenericFailure,
                "ProcessLoopback refuses to capture Fluxer's own process tree",
            ));
        }

        {
            let state = lock_loopback_state(&self.inner)?;
            if state.stream.is_some()
                || state.source.is_some()
                || state.coreaudio_capture.is_some()
                || self
                    .inner
                    .running
                    .load(std::sync::atomic::Ordering::Acquire)
            {
                return Err(napi::Error::new(
                    Status::GenericFailure,
                    "ProcessLoopback is already running",
                ));
            }
        }

        let mut coreaudio_error: Option<CreateError> = None;
        let should_try_coreaudio = backend_pref == "coreaudio"
            || (backend_pref == "auto" && scope == CaptureScope::Process);
        if should_try_coreaudio {
            match Capture::create(pid, include_tree, scope) {
                Ok(mut capture) => {
                    let ctx = std::sync::Arc::as_ptr(&self.inner) as *mut c_void;
                    capture.set_pcm_callback(on_pcm_trampoline, ctx);
                    capture.set_pcm_pool(self.inner.pcm_pool.clone());
                    {
                        let mut state = lock_loopback_state(&self.inner)?;
                        state.coreaudio_capture = Some(capture);
                    }
                    self.inner
                        .running
                        .store(true, std::sync::atomic::Ordering::Release);
                    let start_res = {
                        let mut state = lock_loopback_state(&self.inner)?;
                        state
                            .coreaudio_capture
                            .as_mut()
                            .map(|c| c.start())
                            .unwrap_or(Err(CreateError::StartDeviceFailed))
                    };
                    if let Err(e) = start_res {
                        self.inner
                            .running
                            .store(false, std::sync::atomic::Ordering::Release);
                        let mut state = lock_loopback_state(&self.inner)?;
                        state.coreaudio_capture = None;
                        return Err(napi::Error::new(
                            Status::GenericFailure,
                            coreaudio_tap::coreaudio_error_message(&e),
                        ));
                    }
                    return Ok(());
                }
                Err(e) => {
                    if backend_pref == "coreaudio" || scope == CaptureScope::System {
                        return Err(napi::Error::new(
                            Status::GenericFailure,
                            coreaudio_tap::coreaudio_error_message(&e),
                        ));
                    }
                    coreaudio_error = Some(e);
                }
            }
        }

        let mut built = match if scope == CaptureScope::System {
            build_system_stream()
        } else {
            build_stream_for_pid(pid, include_tree)
        } {
            Ok(b) => b,
            Err(e) => {
                let msg = match e {
                    BuildStreamErr::PidNotFound => match &coreaudio_error {
                        Some(ce) => coreaudio_tap::coreaudio_error_message(ce),
                        None => "No running application for pid",
                    },
                    BuildStreamErr::NoApps | BuildStreamErr::NoDisplays => "No display available",
                    BuildStreamErr::Other => "ProcessLoopback.start failed",
                };
                return Err(napi::Error::new(Status::GenericFailure, msg));
            }
        };

        let ctx = std::sync::Arc::as_ptr(&self.inner) as *mut c_void;
        built.source.set_pcm_callback(on_pcm_trampoline, ctx);
        built.source.set_pcm_pool(self.inner.pcm_pool.clone());
        built.source.set_stop_callback(on_stop_trampoline, ctx);
        let _ = built.source.state.request_start();

        let stream = built.stream.clone();
        {
            let mut state = lock_loopback_state(&self.inner)?;
            state.source = Some(built.source);
            state.stream = Some(built.stream);
        }
        self.inner
            .running
            .store(true, std::sync::atomic::Ordering::Release);

        if sck_async::start_capture(&stream, sck_async::DEFAULT_TIMEOUT_NS).is_err() {
            self.inner
                .running
                .store(false, std::sync::atomic::Ordering::Release);
            let mut state = lock_loopback_state(&self.inner)?;
            state.source = None;
            state.stream = None;
            return Err(napi::Error::new(
                Status::GenericFailure,
                "SCStream startCapture failed",
            ));
        }

        {
            let mut state = lock_loopback_state(&self.inner)?;
            if let Some(s) = state.source.as_mut() {
                let _ = s.state.mark_running();
            }
        }

        if scope == CaptureScope::Process && include_tree {
            let refresher = spawn_sck_late_spawn_refresher(pid, stream.clone());
            let mut state = lock_loopback_state(&self.inner)?;
            state.sck_refresher = refresher;
        }

        Ok(())
    }

    #[napi]
    pub async fn stop(&self) -> Result<()> {
        stop_loopback(&self.inner)
    }
}

impl Drop for ProcessLoopback {
    fn drop(&mut self) {
        let _ = stop_loopback(&self.inner);
    }
}

#[napi(js_name = "pidFromWindowId")]
pub fn pid_from_window_id(window_id: i64) -> i32 {
    if window_id <= 0 || window_id > u32::MAX as i64 {
        return 0;
    }
    let wid: CGWindowID = window_id as u32;

    let Some(array) = CGWindowListCopyWindowInfo(CGWindowListOption::OptionIncludingWindow, wid)
    else {
        return 0;
    };
    if array.count() == 0 {
        return 0;
    }

    let raw_dict = unsafe { array.value_at_index(0) };
    if raw_dict.is_null() {
        return 0;
    }
    let dict: &CFDictionary = unsafe { &*(raw_dict as *const CFDictionary) };

    let key: &objc2_core_foundation::CFString = unsafe { kCGWindowOwnerPID };
    let key_ptr = (key as *const objc2_core_foundation::CFString).cast::<c_void>();
    let owner_val = unsafe { dict.value(key_ptr) };
    if owner_val.is_null() {
        return 0;
    }
    let owner_num: &CFNumber = unsafe { &*(owner_val as *const CFNumber) };
    let mut pid: i32 = 0;
    let ok =
        unsafe { owner_num.value(CFNumberType::IntType, (&mut pid) as *mut i32 as *mut c_void) };
    if ok { pid } else { 0 }
}

#[napi(object, js_name = "MacApplicationDescriptor")]
pub struct AppDescriptor {
    pub pid: i32,
    pub bundle_id: Option<String>,
    pub name: String,
}

#[napi(js_name = "listAudibleApplications")]
pub async fn list_audible_applications() -> Result<Vec<AppDescriptor>> {
    let content =
        match sck_async::get_shareable_content(false, false, sck_async::DEFAULT_TIMEOUT_NS) {
            Ok(c) => c,
            Err(_) => return Ok(Vec::new()),
        };
    let apps: Retained<NSArray<SCRunningApplication>> = unsafe { content.content.applications() };
    let mut out = Vec::new();
    let n = apps.count();
    for i in 0..n {
        let app = apps.objectAtIndex(i);
        let pid = sck::sc_running_application_process_id(&app);
        let bundle = sck::sc_running_application_bundle_identifier(&app).to_string();
        let name = sck::sc_running_application_name(&app).to_string();
        out.push(AppDescriptor {
            pid,
            bundle_id: Some(bundle),
            name,
        });
    }
    Ok(out)
}

#[napi(object, js_name = "MacBackendSckAvailability")]
pub struct SckAvailability {
    pub supported: bool,
    pub macos_version: Option<String>,
}

#[napi(object, js_name = "MacBackendCoreAudioAvailability")]
pub struct CoreAudioAvailability {
    pub supported: bool,
}

#[napi(object, js_name = "MacBackendAvailability")]
pub struct BackendAvailability {
    pub sck: SckAvailability,
    pub coreaudio: CoreAudioAvailability,
    pub screen_permission: String,
    pub audio_permission: String,
}

#[napi(object, js_name = "MacAppAudioBackendInfo")]
pub struct MacAppAudioBackendInfo {
    pub backend: String,
    pub supported: bool,
    pub reason: String,
    #[napi(js_name = "minMacosVersion")]
    pub min_macos_version: String,
    #[napi(js_name = "minMacosVersionCoreaudio")]
    pub min_macos_version_coreaudio: String,
    #[napi(js_name = "detectedMacosVersion")]
    pub detected_macos_version: Option<String>,
    #[napi(js_name = "sckAvailable")]
    pub sck_available: bool,
    #[napi(js_name = "coreaudioAvailable")]
    pub coreaudio_available: bool,
}

#[napi(js_name = "getBackendInfo")]
pub fn get_backend_info() -> MacAppAudioBackendInfo {
    let detected = os_version::current_macos_version();
    let SupportClassification {
        supported,
        sck_available,
        coreaudio_available,
        reason,
    } = classify_support(detected);
    MacAppAudioBackendInfo {
        backend: "mac-app-audio".to_owned(),
        supported,
        reason,
        min_macos_version: format_version(SCK_MIN_MACOS),
        min_macos_version_coreaudio: format_version(COREAUDIO_TAP_MIN_MACOS),
        detected_macos_version: detected.map(format_version),
        sck_available,
        coreaudio_available,
    }
}

#[napi(js_name = "getBackendAvailability")]
pub async fn get_backend_availability() -> Result<BackendAvailability> {
    use objc2::runtime::AnyClass;
    let sck_supported = AnyClass::get(c"SCStream").is_some();
    let version_str = Some(foundation::operating_system_version_string());
    Ok(BackendAvailability {
        sck: SckAvailability {
            supported: sck_supported,
            macos_version: version_str,
        },
        coreaudio: CoreAudioAvailability {
            supported: coreaudio_tap::is_supported(),
        },
        screen_permission: "not-determined".to_string(),
        audio_permission: "not-determined".to_string(),
    })
}
