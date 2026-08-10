// SPDX-License-Identifier: AGPL-3.0-or-later

#![deny(clippy::all)]
#![allow(unsafe_op_in_unsafe_fn)]

#[cfg(any(target_os = "windows", test))]
mod compatibility;
#[cfg(any(target_os = "windows", test))]
mod dxgi_capture;
pub mod encoder_attach;
mod fallback;
#[cfg(target_os = "windows")]
mod game_capture;
mod game_capture_abi;
mod gpu_priority;
mod hdr;
#[cfg(target_os = "windows")]
mod nv12_gpu;
mod sources;
#[cfg(any(target_os = "windows", test))]
mod stall;
#[cfg(target_os = "windows")]
mod vulkan_layer_registry;
#[cfg(target_os = "windows")]
mod wgc_capture;

pub use encoder_attach::{EncoderAttachError, EncoderAttachStats, EncoderAttachment};

use napi::bindgen_prelude::*;
use napi::threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode};
use napi::{JsValue, Status, ValueType};
use napi_derive::napi;
use parking_lot::{Mutex, RwLock};
use std::ffi::c_void;
use std::sync::Arc;

#[cfg(target_os = "windows")]
use dxgi_capture::DxgiCaptureSession;
use fluxer_encoder_ring::EncoderFrameRate;
#[cfg(target_os = "windows")]
use fluxer_screen_frame_bus::EnqueueOutcome;
#[cfg(target_os = "windows")]
use game_capture::GameCaptureSession;
#[cfg(target_os = "windows")]
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
#[cfg(target_os = "windows")]
use wgc_capture::WgcCaptureSession;

const LIFECYCLE_QUEUE_LIMIT: usize = 8;
const START_OPTION_UNSUPPORTED_LIMIT: usize = 4;
const GAME_CAPTURE_HOOK_FORCE_DISABLED: bool = true;

type LifecycleTsfn = Arc<
    ThreadsafeFunction<
        (String, String),
        (),
        (String, String),
        napi::Status,
        false,
        true,
        LIFECYCLE_QUEUE_LIMIT,
    >,
>;

#[napi(object)]
#[derive(Clone, Debug)]
pub struct ScreenCaptureRect {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

#[napi(object)]
#[derive(Clone, Debug, Default)]
pub struct ScreenCaptureStartOptions {
    #[napi(js_name = "showCursorClicks")]
    pub show_cursor_clicks: Option<bool>,
    #[napi(js_name = "captureRect")]
    pub capture_rect: Option<ScreenCaptureRect>,
    #[napi(js_name = "colorRange")]
    pub color_range: Option<String>,
    #[napi(js_name = "colorSpace")]
    pub color_space: Option<String>,
}

#[napi(object)]
#[derive(Clone, Debug, Default)]
pub struct CaptureStartOptionsDiagnostics {
    #[napi(js_name = "showCursorClicks")]
    pub show_cursor_clicks: Option<bool>,
    #[napi(js_name = "captureRect")]
    pub capture_rect: Option<ScreenCaptureRect>,
    #[napi(js_name = "colorRange")]
    pub color_range: Option<String>,
    #[napi(js_name = "colorSpace")]
    pub color_space: Option<String>,
    #[napi(js_name = "unsupportedOptions")]
    pub unsupported_options: Vec<String>,
}

#[napi(object)]
pub struct CaptureStartResult {
    pub width: u32,
    pub height: u32,
    #[napi(js_name = "frameRate")]
    pub frame_rate: u32,
    #[napi(js_name = "pixelFormat")]
    pub pixel_format: String,
}

#[napi(object)]
pub struct ScreenCaptureSourceDescriptor {
    pub kind: String,
    pub id: String,
    pub name: String,
    pub width: u32,
    pub height: u32,
    #[napi(js_name = "targetPid")]
    pub target_pid: Option<u32>,
}

#[napi(object)]
pub struct AvailabilityInfo {
    pub available: bool,
    pub backend: String,
    pub reason: Option<String>,
}

#[napi(object)]
pub struct CaptureDiagnostics {
    pub state: u32,
    #[napi(js_name = "apiType")]
    pub api_type: u32,
    pub transport: u32,
    #[napi(js_name = "fallbackReason")]
    pub fallback_reason: u32,
    #[napi(js_name = "captureFlags")]
    pub capture_flags: u32,
    pub width: u32,
    pub height: u32,
    #[napi(js_name = "dxgiFormat")]
    pub dxgi_format: u32,
    #[napi(js_name = "frameCounter")]
    pub frame_counter: f64,
    #[napi(js_name = "droppedFrameCounter")]
    pub dropped_frame_counter: f64,
    #[napi(js_name = "lastPresentTimestampUs")]
    pub last_present_timestamp_us: f64,
    #[napi(js_name = "lastError")]
    pub last_error: u32,
    #[napi(js_name = "requestedInjectionMethod")]
    pub requested_injection_method: String,
    #[napi(js_name = "injectionMethod")]
    pub injection_method: String,
    #[napi(js_name = "activeStrategy")]
    pub active_strategy: String,
    #[napi(js_name = "lastFallbackReason")]
    pub last_fallback_reason: String,
    #[napi(js_name = "startOptions")]
    pub start_options: CaptureStartOptionsDiagnostics,
    #[napi(js_name = "frameSinkAccepted")]
    pub frame_sink_accepted: f64,
    #[napi(js_name = "frameSinkCoalesced")]
    pub frame_sink_coalesced: f64,
    #[napi(js_name = "frameSinkRejected")]
    pub frame_sink_rejected: f64,
    #[napi(js_name = "mediaFramesDroppedWithoutSink")]
    pub media_frames_dropped_without_sink: f64,
    #[napi(js_name = "cpuFallbackFramesDropped")]
    pub cpu_fallback_frames_dropped: f64,
}

#[napi(object)]
pub struct EncoderAttachDiagnostics {
    pub attached: bool,
    pub width: u32,
    pub height: u32,
    pub capacity: u32,
    #[napi(js_name = "framesSubmitted")]
    pub frames_submitted: f64,
    #[napi(js_name = "framesDropped")]
    pub frames_dropped: f64,
    #[napi(js_name = "ringFullEvents")]
    pub ring_full_events: f64,
    #[napi(js_name = "failedBlits")]
    pub failed_blits: f64,
}

#[napi(object)]
pub struct FrameSinkDiagnostics {
    pub accepted: f64,
    pub coalesced: f64,
    pub rejected: f64,
    #[napi(js_name = "mediaFramesDroppedWithoutSink")]
    pub media_frames_dropped_without_sink: f64,
    #[napi(js_name = "cpuFallbackFramesDropped")]
    pub cpu_fallback_frames_dropped: f64,
}

#[napi(object)]
pub struct SharedTextureHandleInfo {
    pub handle: BigInt,
    pub width: u32,
    pub height: u32,
    #[napi(js_name = "dxgiFormat")]
    pub dxgi_format: u32,
    #[napi(js_name = "timestampUs")]
    pub timestamp_us: f64,
}

#[napi(object)]
pub struct VulkanLayerRegistrationState {
    pub registered: bool,
    #[napi(js_name = "manifestExists")]
    pub manifest_exists: bool,
    #[napi(js_name = "dllExists")]
    pub dll_exists: bool,
    #[napi(js_name = "manifestPath")]
    pub manifest_path: String,
}

pub struct CaptureInner {
    pub lifecycle_tsfn: Mutex<Option<LifecycleTsfn>>,
    #[cfg(target_os = "windows")]
    pub session: Mutex<Option<DxgiCaptureSession>>,
    #[cfg(target_os = "windows")]
    pub(crate) wgc_session: Mutex<Option<WgcCaptureSession>>,
    #[cfg(target_os = "windows")]
    pub game_session: Mutex<Option<Arc<GameCaptureSession>>>,
    pub running: std::sync::atomic::AtomicBool,
    pub fallback: Mutex<Option<fallback::FallbackTracker>>,
    pub capture_id: Mutex<Option<String>>,
    pub start_options: Mutex<CaptureStartOptionsDiagnostics>,
    pub encoder_attachment: RwLock<Option<Arc<EncoderAttachment>>>,
    pub native_frame_sink:
        Mutex<Option<Arc<fluxer_screen_frame_bus::NativeScreenFrameSinkHandleRef>>>,
    #[cfg(target_os = "windows")]
    pub frame_sink_accepted: AtomicU64,
    #[cfg(target_os = "windows")]
    pub frame_sink_coalesced: AtomicU64,
    #[cfg(target_os = "windows")]
    pub frame_sink_rejected: AtomicU64,
    #[cfg(target_os = "windows")]
    pub media_frames_dropped_without_sink: AtomicU64,
    #[cfg(target_os = "windows")]
    pub cpu_fallback_frames_dropped: AtomicU64,
    #[cfg(target_os = "windows")]
    pub frame_sink_backpressure_emitted: AtomicBool,
    #[cfg(target_os = "windows")]
    pub frame_sink_missing_emitted: AtomicBool,
    #[cfg(target_os = "windows")]
    pub cpu_fallback_emitted: AtomicBool,
}

pub fn emit_lifecycle(inner: &CaptureInner, event_type: &str, message: &str) {
    let guard = inner.lifecycle_tsfn.lock();
    if let Some(tsfn) = guard.as_ref() {
        let _ = tsfn.call(
            (event_type.to_string(), message.to_string()),
            ThreadsafeFunctionCallMode::NonBlocking,
        );
    }
}

#[cfg(target_os = "windows")]
struct BusSharedTexture {
    handle: u64,
    width: u32,
    height: u32,
    dxgi_format: u32,
    timestamp_us: i64,
}

#[cfg(target_os = "windows")]
impl BusSharedTexture {
    fn into_bus_desc(self) -> fluxer_screen_frame_bus::SharedTextureDesc {
        fluxer_screen_frame_bus::SharedTextureDesc {
            handle: self.handle,
            width: self.width,
            height: self.height,
            dxgi_format: self.dxgi_format,
            timestamp_us: self.timestamp_us,
        }
    }
}

#[derive(Clone, Copy)]
struct FrameSinkCounterSnapshot {
    accepted: u64,
    coalesced: u64,
    rejected: u64,
    dropped_without_sink: u64,
    cpu_fallback_dropped: u64,
}

#[cfg(target_os = "windows")]
fn frame_sink_counter_snapshot(inner: &CaptureInner) -> FrameSinkCounterSnapshot {
    FrameSinkCounterSnapshot {
        accepted: inner.frame_sink_accepted.load(Ordering::Acquire),
        coalesced: inner.frame_sink_coalesced.load(Ordering::Acquire),
        rejected: inner.frame_sink_rejected.load(Ordering::Acquire),
        dropped_without_sink: inner
            .media_frames_dropped_without_sink
            .load(Ordering::Acquire),
        cpu_fallback_dropped: inner.cpu_fallback_frames_dropped.load(Ordering::Acquire),
    }
}

#[cfg(target_os = "windows")]
fn frame_sink_diagnostics_from(snapshot: FrameSinkCounterSnapshot) -> FrameSinkDiagnostics {
    FrameSinkDiagnostics {
        accepted: snapshot.accepted as f64,
        coalesced: snapshot.coalesced as f64,
        rejected: snapshot.rejected as f64,
        media_frames_dropped_without_sink: snapshot.dropped_without_sink as f64,
        cpu_fallback_frames_dropped: snapshot.cpu_fallback_dropped as f64,
    }
}

#[cfg(target_os = "windows")]
pub(crate) enum FrameSinkRef {
    Native(Arc<fluxer_screen_frame_bus::NativeScreenFrameSinkHandleRef>),
    Bus(Arc<dyn fluxer_screen_frame_bus::ScreenFrameSink>),
}

#[cfg(target_os = "windows")]
pub(crate) fn resolve_frame_sink(
    inner: &CaptureInner,
    capture_id: Option<&str>,
) -> Option<FrameSinkRef> {
    if let Some(sink) = native_frame_sink_for(inner) {
        return Some(FrameSinkRef::Native(sink));
    }
    let capture_id = capture_id?;
    fluxer_screen_frame_bus::get_sink(capture_id).map(FrameSinkRef::Bus)
}

#[cfg(target_os = "windows")]
pub(crate) fn emit_shared_texture_frame(
    inner: &CaptureInner,
    sink: &FrameSinkRef,
    handle: u64,
    width: u32,
    height: u32,
    dxgi_format: u32,
    timestamp_us: i64,
) -> bool {
    assert!(handle != 0, "shared texture handle is non-zero");
    assert!(width > 0, "shared texture width is positive");
    assert!(height > 0, "shared texture height is positive");
    let desc = BusSharedTexture {
        handle,
        width,
        height,
        dxgi_format,
        timestamp_us,
    }
    .into_bus_desc();
    let outcome = match sink {
        FrameSinkRef::Native(sink) => sink.enqueue_shared_texture(desc),
        FrameSinkRef::Bus(sink) => {
            sink.enqueue(fluxer_screen_frame_bus::ScreenFrame::SharedTexture(desc))
        }
    };
    record_frame_sink_outcome(inner, outcome);
    frame_sink_outcome_delivered(outcome)
}

#[cfg(target_os = "windows")]
fn frame_sink_outcome_delivered(outcome: EnqueueOutcome) -> bool {
    !matches!(outcome, EnqueueOutcome::Rejected)
}

#[cfg(target_os = "windows")]
fn record_frame_sink_outcome(inner: &CaptureInner, outcome: EnqueueOutcome) {
    match outcome {
        EnqueueOutcome::Accepted => {
            inner.frame_sink_accepted.fetch_add(1, Ordering::AcqRel);
        }
        EnqueueOutcome::Coalesced => {
            inner.frame_sink_coalesced.fetch_add(1, Ordering::AcqRel);
            emit_frame_sink_backpressure_once(
                inner,
                "Windows shared texture frame coalesced by native frame sink",
            );
        }
        EnqueueOutcome::Rejected => {
            inner.frame_sink_rejected.fetch_add(1, Ordering::AcqRel);
            emit_frame_sink_backpressure_once(
                inner,
                "Windows shared texture frame rejected by native frame sink",
            );
        }
    }
}

#[cfg(target_os = "windows")]
fn emit_frame_sink_backpressure_once(inner: &CaptureInner, message: &'static str) {
    if inner
        .frame_sink_backpressure_emitted
        .swap(true, Ordering::AcqRel)
    {
        return;
    }
    emit_lifecycle(inner, "diagnostic", message);
}

#[cfg(target_os = "windows")]
pub(crate) fn note_media_frame_without_sink(inner: &CaptureInner, message: &'static str) {
    inner
        .media_frames_dropped_without_sink
        .fetch_add(1, Ordering::AcqRel);
    if inner
        .frame_sink_missing_emitted
        .swap(true, Ordering::AcqRel)
    {
        return;
    }
    emit_lifecycle(inner, "diagnostic", message);
}

#[cfg(target_os = "windows")]
pub(crate) fn note_cpu_fallback_frame_dropped(inner: &CaptureInner, message: &'static str) {
    inner
        .cpu_fallback_frames_dropped
        .fetch_add(1, Ordering::AcqRel);
    if inner.cpu_fallback_emitted.swap(true, Ordering::AcqRel) {
        return;
    }
    emit_lifecycle(inner, "diagnostic", message);
}

#[cfg(target_os = "windows")]
fn native_frame_sink_for(
    inner: &CaptureInner,
) -> Option<Arc<fluxer_screen_frame_bus::NativeScreenFrameSinkHandleRef>> {
    inner.native_frame_sink.lock().as_ref().cloned()
}

pub fn observe_fallback(
    inner: &CaptureInner,
    signature: fallback::FailureSignature,
) -> Option<fallback::FallbackDecision> {
    let decision = {
        let mut guard = inner.fallback.lock();
        guard.as_mut().map(|tracker| tracker.observe(signature))
    };
    if let Some(decision) = decision.as_ref() {
        let (kind, message) = fallback::decision_lifecycle(decision);
        emit_lifecycle(inner, kind, &message);
    }
    decision
}

#[napi]
pub struct ScreenCapture {
    inner: Arc<CaptureInner>,
}

#[napi]
impl ScreenCapture {
    #[allow(clippy::new_without_default)]
    #[napi(constructor)]
    pub fn new() -> Self {
        Self {
            inner: Arc::new(CaptureInner {
                lifecycle_tsfn: Mutex::new(None),
                #[cfg(target_os = "windows")]
                session: Mutex::new(None),
                #[cfg(target_os = "windows")]
                wgc_session: Mutex::new(None),
                #[cfg(target_os = "windows")]
                game_session: Mutex::new(None),
                running: std::sync::atomic::AtomicBool::new(false),
                fallback: Mutex::new(None),
                capture_id: Mutex::new(None),
                start_options: Mutex::new(CaptureStartOptionsDiagnostics::default()),
                encoder_attachment: RwLock::new(None),
                native_frame_sink: Mutex::new(None),
                #[cfg(target_os = "windows")]
                frame_sink_accepted: AtomicU64::new(0),
                #[cfg(target_os = "windows")]
                frame_sink_coalesced: AtomicU64::new(0),
                #[cfg(target_os = "windows")]
                frame_sink_rejected: AtomicU64::new(0),
                #[cfg(target_os = "windows")]
                media_frames_dropped_without_sink: AtomicU64::new(0),
                #[cfg(target_os = "windows")]
                cpu_fallback_frames_dropped: AtomicU64::new(0),
                #[cfg(target_os = "windows")]
                frame_sink_backpressure_emitted: AtomicBool::new(false),
                #[cfg(target_os = "windows")]
                frame_sink_missing_emitted: AtomicBool::new(false),
                #[cfg(target_os = "windows")]
                cpu_fallback_emitted: AtomicBool::new(false),
            }),
        }
    }

    #[napi(js_name = "setLifecycleCallback")]
    pub fn set_lifecycle_callback(&self, callback: Function<(String, String), ()>) -> Result<()> {
        let tsfn: LifecycleTsfn = callback
            .build_threadsafe_function::<(String, String)>()
            .weak::<true>()
            .callee_handled::<false>()
            .max_queue_size::<LIFECYCLE_QUEUE_LIMIT>()
            .build()
            .map(Arc::new)?;
        let mut guard = self.inner.lifecycle_tsfn.lock();
        *guard = Some(tsfn);
        Ok(())
    }

    #[napi(js_name = "setFrameSinkHandle")]
    pub fn set_frame_sink_handle(&self, frame_sink_handle: Unknown<'_>) -> Result<()> {
        let sink = retain_native_frame_sink_handle(frame_sink_handle)?;
        let mut guard = self.inner.native_frame_sink.lock();
        *guard = Some(sink);
        Ok(())
    }

    #[napi]
    #[allow(clippy::too_many_arguments)]
    pub fn start(
        &self,
        source_id: String,
        source_kind: String,
        width: Option<u32>,
        height: Option<u32>,
        frame_rate: Option<u32>,
        hook_path: Option<String>,
        hook_path_x86: Option<String>,
        injection_method: Option<String>,
        capture_id: Option<String>,
        start_options: Option<ScreenCaptureStartOptions>,
    ) -> Result<CaptureStartResult> {
        let start_options = record_start_options(&self.inner, start_options)?;
        let normalized_capture_id = capture_id
            .map(|raw| raw.trim().to_string())
            .filter(|trimmed| !trimmed.is_empty());
        {
            let mut guard = self.inner.capture_id.lock();
            *guard = normalized_capture_id;
        }
        #[cfg(target_os = "windows")]
        {
            self.start_windows(
                source_id,
                source_kind,
                width,
                height,
                frame_rate,
                hook_path,
                hook_path_x86,
                injection_method,
                start_options,
            )
        }
        #[cfg(not(target_os = "windows"))]
        {
            let _ = (
                source_id,
                source_kind,
                width,
                height,
                frame_rate,
                hook_path,
                hook_path_x86,
                injection_method,
                start_options,
            );
            Err(napi::Error::from_reason(
                "native game capture only supported on Windows",
            ))
        }
    }

    #[napi(js_name = "getDiagnostics")]
    pub fn get_diagnostics(&self) -> Option<CaptureDiagnostics> {
        let snapshot = {
            let guard = self.inner.fallback.lock();
            guard.as_ref().map(|tracker| tracker.snapshot())
        }?;

        #[cfg(target_os = "windows")]
        {
            let frame_sink = frame_sink_counter_snapshot(&self.inner);
            let guard = self.inner.game_session.lock();
            if let Some(session) = guard.as_ref() {
                let requested_injection_method = session.requested_injection_method().to_string();
                let injection_method = session.used_injection_method().to_string();
                if let Some(info) = session.read_shared_info() {
                    return Some(CaptureDiagnostics {
                        state: info.state,
                        api_type: info.api_type,
                        transport: info.transport,
                        fallback_reason: info.fallback_reason,
                        capture_flags: info.capture_flags,
                        width: info.width,
                        height: info.height,
                        dxgi_format: info.dxgi_format,
                        frame_counter: info.frame_counter as f64,
                        dropped_frame_counter: info.dropped_frame_counter as f64,
                        last_present_timestamp_us: info.last_present_timestamp_us as f64,
                        last_error: info.last_error,
                        requested_injection_method,
                        injection_method,
                        active_strategy: snapshot.active_strategy,
                        last_fallback_reason: snapshot.last_fallback_reason,
                        start_options: current_start_options(&self.inner),
                        frame_sink_accepted: frame_sink.accepted as f64,
                        frame_sink_coalesced: frame_sink.coalesced as f64,
                        frame_sink_rejected: frame_sink.rejected as f64,
                        media_frames_dropped_without_sink: frame_sink.dropped_without_sink as f64,
                        cpu_fallback_frames_dropped: frame_sink.cpu_fallback_dropped as f64,
                    });
                }
                return Some(strategy_only_diagnostics(
                    &snapshot,
                    requested_injection_method,
                    injection_method,
                    current_start_options(&self.inner),
                    frame_sink,
                ));
            }
            Some(strategy_only_diagnostics(
                &snapshot,
                String::new(),
                String::new(),
                current_start_options(&self.inner),
                frame_sink,
            ))
        }

        #[cfg(not(target_os = "windows"))]
        Some(strategy_only_diagnostics(
            &snapshot,
            String::new(),
            String::new(),
            current_start_options(&self.inner),
            FrameSinkCounterSnapshot {
                accepted: 0,
                coalesced: 0,
                rejected: 0,
                dropped_without_sink: 0,
                cpu_fallback_dropped: 0,
            },
        ))
    }

    #[napi(js_name = "getFrameSinkDiagnostics")]
    pub fn get_frame_sink_diagnostics(&self) -> FrameSinkDiagnostics {
        #[cfg(target_os = "windows")]
        {
            frame_sink_diagnostics_from(frame_sink_counter_snapshot(&self.inner))
        }
        #[cfg(not(target_os = "windows"))]
        {
            FrameSinkDiagnostics {
                accepted: 0.0,
                coalesced: 0.0,
                rejected: 0.0,
                media_frames_dropped_without_sink: 0.0,
                cpu_fallback_frames_dropped: 0.0,
            }
        }
    }

    #[napi(js_name = "getSharedTextureHandle")]
    pub fn get_shared_texture_handle(&self) -> Option<SharedTextureHandleInfo> {
        #[cfg(target_os = "windows")]
        {
            let guard = self.inner.game_session.lock();
            let session = guard.as_ref()?;
            if let Some(native_texture) = session.read_native_texture_info() {
                return Some(SharedTextureHandleInfo {
                    handle: BigInt::from(native_texture.handle),
                    width: native_texture.width,
                    height: native_texture.height,
                    dxgi_format: native_texture.dxgi_format,
                    timestamp_us: native_texture.timestamp_us as f64,
                });
            }
            None
        }
        #[cfg(not(target_os = "windows"))]
        {
            None
        }
    }

    #[napi]
    pub fn stop(&self) -> Result<()> {
        self.inner
            .running
            .store(false, std::sync::atomic::Ordering::Release);
        self.inner.capture_id.lock().take();
        self.inner.native_frame_sink.lock().take();
        if let Some(attachment) = self.inner.encoder_attachment.write().take() {
            attachment.detach();
        }
        #[cfg(target_os = "windows")]
        {
            let mut guard = self.inner.session.lock();
            *guard = None;
            let mut wgc_guard = self.inner.wgc_session.lock();
            *wgc_guard = None;
            let mut game_guard = self.inner.game_session.lock();
            *game_guard = None;
        }
        {
            let mut fallback_guard = self.inner.fallback.lock();
            *fallback_guard = None;
        }
        Ok(())
    }

    #[napi(js_name = "attachEncoder")]
    pub fn attach_encoder(&self, width: u32, height: u32, frame_rate: Option<u32>) -> Result<()> {
        if width == 0 || height == 0 {
            return Err(napi::Error::new(
                Status::InvalidArg,
                "ScreenCapture.attachEncoder requires positive dimensions",
            ));
        }
        let frame_rate = EncoderFrameRate::from_fps(frame_rate.unwrap_or(30));
        let attachment = EncoderAttachment::try_new_with_frame_rate(width, height, frame_rate)
            .map_err(|e| {
                napi::Error::new(Status::GenericFailure, format!("attachEncoder failed: {e}"))
            })?;
        *self.inner.encoder_attachment.write() = Some(attachment);
        emit_lifecycle(
            &self.inner,
            "diagnostic",
            &format!(
                "encoder ring attached: {width}x{height}@{}fps, capacity=8",
                frame_rate.numerator
            ),
        );
        Ok(())
    }

    #[napi(js_name = "detachEncoder")]
    pub fn detach_encoder(&self) -> Result<()> {
        if let Some(attachment) = self.inner.encoder_attachment.write().take() {
            attachment.detach();
        }
        emit_lifecycle(&self.inner, "diagnostic", "encoder ring detached");
        Ok(())
    }

    #[napi(js_name = "isEncoderAttached")]
    pub fn is_encoder_attached(&self) -> bool {
        self.inner
            .encoder_attachment
            .read()
            .as_ref()
            .map(|attachment| attachment.is_attached())
            .unwrap_or(false)
    }

    #[napi(js_name = "encoderRingFullCount")]
    pub fn encoder_ring_full_count(&self) -> u32 {
        self.inner
            .encoder_attachment
            .read()
            .as_ref()
            .map(|attachment| attachment.stats().ring_full_events.min(u32::MAX as u64) as u32)
            .unwrap_or(0)
    }

    #[napi(js_name = "getEncoderAttachDiagnostics")]
    pub fn get_encoder_attach_diagnostics(&self) -> Option<EncoderAttachDiagnostics> {
        let guard = self.inner.encoder_attachment.read();
        let attachment = guard.as_ref()?;
        let stats = attachment.stats();
        Some(EncoderAttachDiagnostics {
            attached: attachment.is_attached(),
            width: attachment.width(),
            height: attachment.height(),
            capacity: attachment.capacity().min(u32::MAX as usize) as u32,
            frames_submitted: stats.frames_submitted as f64,
            frames_dropped: stats.frames_dropped as f64,
            ring_full_events: stats.ring_full_events as f64,
            failed_blits: stats.failed_blits as f64,
        })
    }
}

fn record_start_options(
    inner: &CaptureInner,
    options: Option<ScreenCaptureStartOptions>,
) -> Result<CaptureStartOptionsDiagnostics> {
    let state = build_start_option_diagnostics(options.unwrap_or_default())?;
    if !state.unsupported_options.is_empty() {
        emit_lifecycle(
            inner,
            "diagnostic",
            &format!(
                "Windows capture start options currently unsupported: {}",
                state.unsupported_options.join(", ")
            ),
        );
    }
    let mut guard = inner.start_options.lock();
    *guard = state.clone();
    Ok(state)
}

fn current_start_options(inner: &CaptureInner) -> CaptureStartOptionsDiagnostics {
    inner.start_options.lock().clone()
}

fn build_start_option_diagnostics(
    options: ScreenCaptureStartOptions,
) -> Result<CaptureStartOptionsDiagnostics> {
    validate_capture_rect(options.capture_rect.as_ref())?;
    validate_enum_option(
        options.color_range.as_deref(),
        "colorRange",
        &["full", "limited"],
    )?;
    validate_enum_option(
        options.color_space.as_deref(),
        "colorSpace",
        &["rec709", "srgb"],
    )?;

    let mut unsupported_options = Vec::with_capacity(START_OPTION_UNSUPPORTED_LIMIT);
    if options.show_cursor_clicks.is_some() {
        unsupported_options.push("showCursorClicks".to_string());
    }
    if options.capture_rect.is_some() {
        unsupported_options.push("captureRect".to_string());
    }
    if options.color_range.is_some() {
        unsupported_options.push("colorRange".to_string());
    }
    if options.color_space.is_some() {
        unsupported_options.push("colorSpace".to_string());
    }
    assert!(
        unsupported_options.len() <= START_OPTION_UNSUPPORTED_LIMIT,
        "unsupported start-option list bounded"
    );

    Ok(CaptureStartOptionsDiagnostics {
        show_cursor_clicks: options.show_cursor_clicks,
        capture_rect: options.capture_rect,
        color_range: options.color_range,
        color_space: options.color_space,
        unsupported_options,
    })
}

fn validate_capture_rect(rect: Option<&ScreenCaptureRect>) -> Result<()> {
    let Some(rect) = rect else {
        return Ok(());
    };
    if !rect.x.is_finite() || !rect.y.is_finite() {
        return Err(napi::Error::new(
            Status::InvalidArg,
            "captureRect x/y must be finite numbers",
        ));
    }
    if !rect.width.is_finite() || !rect.height.is_finite() {
        return Err(napi::Error::new(
            Status::InvalidArg,
            "captureRect width/height must be finite numbers",
        ));
    }
    if rect.width <= 0.0 || rect.height <= 0.0 {
        return Err(napi::Error::new(
            Status::InvalidArg,
            "captureRect requires positive width and height",
        ));
    }
    Ok(())
}

fn validate_enum_option(value: Option<&str>, name: &str, allowed: &[&str]) -> Result<()> {
    let Some(value) = value else {
        return Ok(());
    };
    if allowed.contains(&value) {
        return Ok(());
    }
    Err(napi::Error::new(
        Status::InvalidArg,
        format!("invalid {name}: {value}"),
    ))
}

fn retain_native_frame_sink_handle(
    value: Unknown<'_>,
) -> Result<Arc<fluxer_screen_frame_bus::NativeScreenFrameSinkHandleRef>> {
    if value.get_type()? != ValueType::External {
        return Err(napi::Error::new(
            Status::InvalidArg,
            "ScreenCapture.setFrameSinkHandle expects a native external frame sink handle",
        ));
    }

    let raw_value = value.value();
    let mut data: *mut c_void = std::ptr::null_mut();
    let status =
        unsafe { napi::sys::napi_get_value_external(raw_value.env, raw_value.value, &mut data) };
    if status != napi::sys::Status::napi_ok || data.is_null() {
        return Err(napi::Error::new(
            Status::InvalidArg,
            "ScreenCapture.setFrameSinkHandle received an empty native external frame sink handle",
        ));
    }

    let handle = unsafe {
        fluxer_screen_frame_bus::NativeScreenFrameSinkHandle::retain_from_raw(
            data.cast::<fluxer_screen_frame_bus::NativeScreenFrameSinkHandle>(),
        )
    }
    .ok_or_else(|| {
        napi::Error::new(
            Status::InvalidArg,
            "ScreenCapture.setFrameSinkHandle received an invalid native frame sink handle",
        )
    })?;

    Ok(Arc::new(handle))
}

impl Drop for ScreenCapture {
    fn drop(&mut self) {
        self.inner.native_frame_sink.lock().take();
    }
}

#[cfg(target_os = "windows")]
impl ScreenCapture {
    #[allow(clippy::too_many_arguments)]
    fn start_windows(
        &self,
        source_id: String,
        source_kind: String,
        width: Option<u32>,
        height: Option<u32>,
        frame_rate: Option<u32>,
        hook_path: Option<String>,
        hook_path_x86: Option<String>,
        injection_method: Option<String>,
        _start_options: CaptureStartOptionsDiagnostics,
    ) -> Result<CaptureStartResult> {
        use std::sync::atomic::Ordering;

        if self.inner.running.load(Ordering::Acquire) {
            return Err(napi::Error::from_reason("Capture already running"));
        }

        if source_kind == "game" && !GAME_CAPTURE_HOOK_FORCE_DISABLED {
            return self.start_windows_game(
                source_id,
                source_kind,
                width,
                height,
                frame_rate,
                hook_path,
                hook_path_x86,
                injection_method,
                _start_options,
            );
        }
        let _ = (hook_path, hook_path_x86, injection_method);
        let target_frame_rate = frame_rate.unwrap_or(30).clamp(1, 144);

        let frame_interval =
            std::time::Duration::from_nanos(1_000_000_000 / target_frame_rate as u64);

        if source_kind == "screen" {
            let monitor = wgc_capture::parse_monitor_source_id(&source_id, &source_kind)
                .ok_or_else(|| {
                    napi::Error::from_reason(format!("Invalid source: {source_kind}:{source_id}"))
                })?;
            if !wgc_capture::wgc_capture_supported() {
                return Err(napi::Error::from_reason(
                    "Windows Graphics Capture is unavailable for screen capture",
                ));
            }
            let session = WgcCaptureSession::new_monitor(monitor, width, height).map_err(|e| {
                napi::Error::from_reason(format!("Failed to create WGC screen capture: {e}"))
            })?;
            return self.start_windows_wgc_session(session, target_frame_rate);
        }

        let hwnd = if source_kind == "game" {
            let target = game_capture::resolve_game_capture_target(&source_id, &source_kind)
                .map_err(|e| {
                    napi::Error::from_reason(format!("Failed to resolve game capture target: {e}"))
                })?;
            windows::Win32::Foundation::HWND(target as *mut _)
        } else {
            dxgi_capture::parse_window_source_id(&source_id, &source_kind).ok_or_else(|| {
                napi::Error::from_reason(format!("Invalid source: {source_kind}:{source_id}"))
            })?
        };

        if let Some(result) = self.try_start_windows_wgc(hwnd, width, height, target_frame_rate)? {
            return Ok(result);
        }

        let session = DxgiCaptureSession::new(hwnd, width, height)
            .map_err(|e| napi::Error::from_reason(format!("Failed to create DXGI capture: {e}")))?;

        let capture_width = session.capture_width();
        let capture_height = session.capture_height();

        {
            let mut guard = self.inner.session.lock();
            *guard = Some(session);
        }
        {
            let mut guard = self.inner.fallback.lock();
            *guard = Some(fallback::FallbackTracker::new(
                fallback::CaptureStrategy::DxgiDuplication,
            ));
        }

        self.inner.running.store(true, Ordering::Release);

        let inner = Arc::clone(&self.inner);

        std::thread::Builder::new()
            .name("dxgi-capture".into())
            .spawn(move || {
                dxgi_capture::capture_loop(&inner, frame_interval);
            })
            .map_err(|e| {
                napi::Error::from_reason(format!("Failed to spawn capture thread: {e}"))
            })?;

        Ok(CaptureStartResult {
            width: capture_width,
            height: capture_height,
            frame_rate: target_frame_rate,
            pixel_format: "bgra".to_string(),
        })
    }

    fn try_start_windows_wgc(
        &self,
        hwnd: windows::Win32::Foundation::HWND,
        width: Option<u32>,
        height: Option<u32>,
        target_frame_rate: u32,
    ) -> Result<Option<CaptureStartResult>> {
        assert!(target_frame_rate >= 1, "frame rate at least 1");
        assert!(target_frame_rate <= 144, "frame rate at most 144");
        if !wgc_capture::wgc_capture_supported() {
            return Ok(None);
        }
        let session = match WgcCaptureSession::new(hwnd, width, height) {
            Ok(session) => session,
            Err(e) => {
                emit_lifecycle(
                    &self.inner,
                    "diagnostic",
                    &format!("WGC window capture unavailable; using DXGI duplication: {e}"),
                );
                return Ok(None);
            }
        };
        self.start_windows_wgc_session(session, target_frame_rate)
            .map(Some)
    }

    fn start_windows_wgc_session(
        &self,
        session: WgcCaptureSession,
        target_frame_rate: u32,
    ) -> Result<CaptureStartResult> {
        assert!(target_frame_rate >= 1, "frame rate at least 1");
        assert!(target_frame_rate <= 144, "frame rate at most 144");
        let capture_width = session.capture_width();
        let capture_height = session.capture_height();

        {
            let mut guard = self.inner.wgc_session.lock();
            *guard = Some(session);
        }
        {
            let mut guard = self.inner.fallback.lock();
            *guard = Some(fallback::FallbackTracker::new(
                fallback::CaptureStrategy::Wgc,
            ));
        }

        self.inner.running.store(true, Ordering::Release);

        let inner = Arc::clone(&self.inner);
        let frame_interval =
            std::time::Duration::from_nanos(1_000_000_000 / target_frame_rate as u64);

        std::thread::Builder::new()
            .name("wgc-capture".into())
            .spawn(move || {
                wgc_capture::capture_loop(&inner, frame_interval);
            })
            .map_err(|e| {
                napi::Error::from_reason(format!("Failed to spawn WGC capture thread: {e}"))
            })?;

        Ok(CaptureStartResult {
            width: capture_width,
            height: capture_height,
            frame_rate: target_frame_rate,
            pixel_format: "bgra".to_string(),
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn start_windows_game(
        &self,
        source_id: String,
        source_kind: String,
        width: Option<u32>,
        height: Option<u32>,
        frame_rate: Option<u32>,
        hook_path: Option<String>,
        hook_path_x86: Option<String>,
        injection_method: Option<String>,
        _start_options: CaptureStartOptionsDiagnostics,
    ) -> Result<CaptureStartResult> {
        use std::sync::atomic::Ordering;

        if game_capture_abi::env_flag_enabled(game_capture_abi::ENV_DISABLE_HOOK) {
            return Err(napi::Error::from_reason(
                "game capture hook disabled via FLUXER_GAME_CAPTURE_DISABLE_HOOK",
            ));
        }

        let hook_path = hook_path
            .ok_or_else(|| napi::Error::from_reason("missing game capture hook DLL path"))?;
        let target_frame_rate = frame_rate.unwrap_or(30).clamp(1, 144);
        let session = GameCaptureSession::new(
            &source_id,
            &source_kind,
            width,
            height,
            target_frame_rate,
            &hook_path,
            hook_path_x86.as_deref(),
            injection_method.as_deref(),
        )
        .map_err(|e| napi::Error::from_reason(format!("Failed to create game capture: {e}")))?;
        let capture_width = session.capture_width();
        let capture_height = session.capture_height();
        let session = Arc::new(session);

        {
            let mut guard = self.inner.game_session.lock();
            *guard = Some(session);
        }
        {
            let mut guard = self.inner.fallback.lock();
            *guard = Some(fallback::FallbackTracker::new(
                fallback::CaptureStrategy::GameHook,
            ));
        }

        self.inner.running.store(true, Ordering::Release);

        let inner = Arc::clone(&self.inner);
        let frame_interval =
            std::time::Duration::from_nanos(1_000_000_000 / target_frame_rate as u64);

        std::thread::Builder::new()
            .name("game-capture".into())
            .spawn(move || {
                game_capture::capture_loop(&inner, frame_interval);
            })
            .map_err(|e| {
                napi::Error::from_reason(format!("Failed to spawn game capture thread: {e}"))
            })?;

        Ok(CaptureStartResult {
            width: capture_width,
            height: capture_height,
            frame_rate: target_frame_rate,
            pixel_format: "bgra".to_string(),
        })
    }
}

fn strategy_only_diagnostics(
    snapshot: &fallback::FallbackSnapshot,
    requested_injection_method: String,
    injection_method: String,
    start_options: CaptureStartOptionsDiagnostics,
    frame_sink: FrameSinkCounterSnapshot,
) -> CaptureDiagnostics {
    CaptureDiagnostics {
        state: 0,
        api_type: 0,
        transport: 0,
        fallback_reason: 0,
        capture_flags: 0,
        width: 0,
        height: 0,
        dxgi_format: 0,
        frame_counter: 0.0,
        dropped_frame_counter: 0.0,
        last_present_timestamp_us: 0.0,
        last_error: 0,
        requested_injection_method,
        injection_method,
        active_strategy: snapshot.active_strategy.clone(),
        last_fallback_reason: snapshot.last_fallback_reason.clone(),
        start_options,
        frame_sink_accepted: frame_sink.accepted as f64,
        frame_sink_coalesced: frame_sink.coalesced as f64,
        frame_sink_rejected: frame_sink.rejected as f64,
        media_frames_dropped_without_sink: frame_sink.dropped_without_sink as f64,
        cpu_fallback_frames_dropped: frame_sink.cpu_fallback_dropped as f64,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn start_options_are_kept_as_explicit_unsupported_state() {
        let state = build_start_option_diagnostics(ScreenCaptureStartOptions {
            show_cursor_clicks: Some(true),
            capture_rect: Some(ScreenCaptureRect {
                x: 10.0,
                y: 20.0,
                width: 300.0,
                height: 200.0,
            }),
            color_range: Some("full".to_string()),
            color_space: Some("rec709".to_string()),
        })
        .expect("valid options");

        assert_eq!(state.show_cursor_clicks, Some(true));
        assert_eq!(state.color_range.as_deref(), Some("full"));
        assert_eq!(state.color_space.as_deref(), Some("rec709"));
        assert_eq!(
            state.unsupported_options,
            vec![
                "showCursorClicks".to_string(),
                "captureRect".to_string(),
                "colorRange".to_string(),
                "colorSpace".to_string(),
            ]
        );
    }

    #[test]
    fn capture_rect_requires_positive_dimensions() {
        let err = build_start_option_diagnostics(ScreenCaptureStartOptions {
            capture_rect: Some(ScreenCaptureRect {
                x: 0.0,
                y: 0.0,
                width: 0.0,
                height: 10.0,
            }),
            ..ScreenCaptureStartOptions::default()
        })
        .err();
        assert!(err.is_some(), "invalid captureRect is rejected");
    }
}

#[napi(js_name = "isSupported")]
pub fn is_supported() -> bool {
    cfg!(target_os = "windows")
}

#[napi(js_name = "isGameCaptureHookAvailable")]
pub fn is_game_capture_hook_available() -> bool {
    cfg!(target_os = "windows") && !GAME_CAPTURE_HOOK_FORCE_DISABLED
}

#[napi(js_name = "getAvailability")]
pub fn get_availability() -> AvailabilityInfo {
    AvailabilityInfo {
        available: cfg!(target_os = "windows"),
        backend: "windows-game-capture".to_string(),
        reason: if cfg!(target_os = "windows") {
            None
        } else {
            Some("unsupported-platform".to_string())
        },
    }
}

#[napi(js_name = "listSources")]
pub fn list_sources() -> Result<Vec<ScreenCaptureSourceDescriptor>> {
    Ok(sources::list_sources())
}

#[napi(js_name = "elevateGpuSchedulingPriority")]
pub fn elevate_gpu_scheduling_priority(
    process_id: Option<u32>,
    priority_class: Option<String>,
) -> Result<()> {
    gpu_priority::elevate(process_id, priority_class).map_err(napi::Error::from_reason)
}

#[napi(js_name = "restoreGpuSchedulingPriority")]
pub fn restore_gpu_scheduling_priority(process_id: Option<u32>) -> Result<()> {
    gpu_priority::restore(process_id).map_err(napi::Error::from_reason)
}

#[napi(js_name = "registerVulkanLayerManifest")]
pub fn register_vulkan_layer_manifest(manifest_path: String) -> Result<()> {
    #[cfg(target_os = "windows")]
    {
        vulkan_layer_registry::register_manifest(&manifest_path).map_err(napi::Error::from_reason)
    }
    #[cfg(not(target_os = "windows"))]
    {
        let _ = manifest_path;
        Err(napi::Error::from_reason(
            "Vulkan game capture layer only supported on Windows",
        ))
    }
}

#[napi(js_name = "unregisterVulkanLayerManifest")]
pub fn unregister_vulkan_layer_manifest(manifest_path: String) -> Result<()> {
    #[cfg(target_os = "windows")]
    {
        vulkan_layer_registry::unregister_manifest(&manifest_path).map_err(napi::Error::from_reason)
    }
    #[cfg(not(target_os = "windows"))]
    {
        let _ = manifest_path;
        Err(napi::Error::from_reason(
            "Vulkan game capture layer only supported on Windows",
        ))
    }
}

#[napi(js_name = "getVulkanLayerRegistrationState")]
pub fn get_vulkan_layer_registration_state(manifest_path: String) -> VulkanLayerRegistrationState {
    #[cfg(target_os = "windows")]
    {
        let state = vulkan_layer_registry::registration_state(&manifest_path);
        VulkanLayerRegistrationState {
            registered: state.registered,
            manifest_exists: state.manifest_exists,
            dll_exists: state.dll_exists,
            manifest_path: state.manifest_path,
        }
    }
    #[cfg(not(target_os = "windows"))]
    {
        VulkanLayerRegistrationState {
            registered: false,
            manifest_exists: false,
            dll_exists: false,
            manifest_path,
        }
    }
}
