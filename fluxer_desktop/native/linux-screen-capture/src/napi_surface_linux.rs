// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ffi::c_void;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

use napi::bindgen_prelude::{Function, Result, Unknown};
use napi::threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode};
use napi::{JsValue, Status, ValueType};
use napi_derive::napi;

use fluxer_screen_frame_bus::{
    self as frame_bus, DmabufDesc as BusDmabufDesc, DmabufFrame as BusDmabufFrame, EnqueueOutcome,
    NativeScreenFrameSinkHandle, NativeScreenFrameSinkHandleRef, Nv12Frame as BusNv12Frame,
    ScreenFrame as BusScreenFrame,
};
use std::os::fd::{FromRawFd, OwnedFd};

use crate::game_capture::{GameCaptureDiagnostics, GameCaptureVideoStream};
use crate::pipewire_stream::{
    FrameCallback, LINUX_FRAME_BYTES_MAX, LINUX_FRAME_DIM_MAX, LifecycleCallback,
    LinuxFrameBufferPool, PipeWireVideoStream, PoolExhaustionCallback, VideoFrame,
    daemon_reachable,
};
use crate::portal::{self, LiveSession, PortalError, SOURCE_TYPE_WINDOW, StreamInfo};

fn generic_error(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::GenericFailure, reason.into())
}

fn invalid_arg(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::InvalidArg, reason.into())
}

fn portal_error_to_status(err: &PortalError) -> &'static str {
    match err {
        PortalError::CursorModeUnavailable => "cursor-mode-unavailable",
        PortalError::PortalTooOld(_) => "portal-too-old",
        PortalError::Cancelled => "cancelled",
        PortalError::PortalTimeout => "portal-timeout",
        PortalError::DbusError => "dbus-error",
        PortalError::InvalidReply => "invalid-reply",
        PortalError::SendFailed => "send-failed",
        PortalError::NoStreams => "no-streams",
    }
}

#[napi(object, js_name = "LinuxScreenCaptureSource")]
pub struct LinuxScreenCaptureSource {
    pub kind: String,
    pub id: String,
    pub name: String,
    pub width: u32,
    pub height: u32,
    pub app_name: Option<String>,
    pub bundle_id: Option<String>,

    pub target_pid: Option<u32>,
}

#[napi(object, js_name = "LinuxScreenCaptureCapabilities")]
pub struct Capabilities {
    pub process: bool,
    pub system: bool,
}

#[napi(object, js_name = "LinuxScreenCaptureAvailability")]
pub struct Availability {
    pub available: bool,
    pub backend: String,
    pub reason: Option<String>,
    pub detail: Option<String>,
    pub portal_version: Option<u32>,
    pub capabilities: Capabilities,
}

#[napi(object, js_name = "LinuxScreenCaptureBackendInfo")]
pub struct BackendInfo {
    pub backend: String,
    pub supported: bool,
    pub reason: String,
    pub portal_version: Option<u32>,
    pub pipewire_reachable: bool,
}

const BACKEND: &str = "linux-pipewire-portal";

#[napi(js_name = "getBackendInfo")]
pub fn get_backend_info() -> BackendInfo {
    let portal_version = portal::read_portal_version().ok();
    let pipewire_reachable = daemon_reachable();
    let supported = matches!(portal_version, Some(v) if v >= 4) && pipewire_reachable;
    let reason = if !pipewire_reachable {
        "PipeWire daemon unreachable".to_string()
    } else {
        match portal_version {
            None => "xdg-desktop-portal ScreenCast unavailable".to_string(),
            Some(v) if v < 4 => {
                format!("xdg-desktop-portal ScreenCast version {v} is below required 4")
            }
            _ => String::new(),
        }
    };
    BackendInfo {
        backend: BACKEND.to_string(),
        supported,
        reason,
        portal_version,
        pipewire_reachable,
    }
}

#[napi(js_name = "getAvailability")]
pub async fn get_availability() -> Result<Availability> {
    let info = napi::bindgen_prelude::spawn_blocking(get_backend_info)
        .await
        .map_err(|_| generic_error("getBackendInfo task panicked"))?;
    let (reason, detail) = if info.supported {
        (
            None,
            info.portal_version.map(|v| format!("portal version {v}")),
        )
    } else {
        let reason_code = if !info.pipewire_reachable {
            "pipewire-unreachable"
        } else if matches!(info.portal_version, Some(v) if v < 4) || info.portal_version.is_none() {
            "portal-too-old"
        } else {
            "load-failed"
        };
        (Some(reason_code.to_string()), Some(info.reason.clone()))
    };
    Ok(Availability {
        available: info.supported,
        backend: BACKEND.to_string(),
        reason,
        detail,
        portal_version: info.portal_version,
        capabilities: Capabilities {
            process: false,
            system: info.supported,
        },
    })
}

fn encode_source_id(node_id: u32) -> String {
    node_id.to_string()
}

fn parse_source_id(id: &str) -> Option<u32> {
    id.parse::<u32>().ok().filter(|n| *n > 0)
}

fn source_kind_for_type(source_type: u32) -> &'static str {
    if source_type & SOURCE_TYPE_WINDOW != 0 {
        "window"
    } else {
        "screen"
    }
}

fn display_name_for(stream: &StreamInfo) -> String {
    if let Some(mapping) = stream.mapping_id.as_ref()
        && !mapping.is_empty()
    {
        return mapping.clone();
    }
    let kind = if stream.source_type & SOURCE_TYPE_WINDOW != 0 {
        "Window"
    } else {
        "Display"
    };
    format!("{kind} ({})", stream.node_id)
}

#[napi(js_name = "listSources")]
pub async fn list_sources() -> Result<Vec<LinuxScreenCaptureSource>> {
    let result = napi::bindgen_prelude::spawn_blocking(portal::open_session_and_pick)
        .await
        .map_err(|_| generic_error("listSources task panicked"))?;
    let (session, streams) = match result {
        Ok(parts) => parts,
        Err(err) => {
            let code = portal_error_to_status(&err);
            return Err(napi::Error::new(
                Status::GenericFailure,
                format!("listSources failed: {code} ({err})"),
            ));
        }
    };
    let mut out = Vec::with_capacity(streams.len());
    for stream in &streams {
        out.push(LinuxScreenCaptureSource {
            kind: source_kind_for_type(stream.source_type).to_string(),
            id: encode_source_id(stream.node_id),
            name: display_name_for(stream),
            width: stream.width,
            height: stream.height,
            app_name: None,
            bundle_id: None,
            target_pid: None,
        });
    }

    park_session(session, streams);
    Ok(out)
}

struct ParkedSession {
    session: LiveSession,
    streams: Vec<StreamInfo>,
}

static PARKED_SESSION: Mutex<Option<ParkedSession>> = Mutex::new(None);

fn park_session(session: LiveSession, streams: Vec<StreamInfo>) {
    let mut guard = match PARKED_SESSION.lock() {
        Ok(g) => g,
        Err(p) => p.into_inner(),
    };
    if let Some(old) = guard.take() {
        old.session.close();
    }
    *guard = Some(ParkedSession { session, streams });
}

fn adopt_session() -> Option<ParkedSession> {
    let mut guard = match PARKED_SESSION.lock() {
        Ok(g) => g,
        Err(p) => p.into_inner(),
    };
    guard.take()
}

fn stream_dimensions_for_node(streams: &[StreamInfo], node_id: u32) -> (u32, u32) {
    streams
        .iter()
        .find(|stream| stream.node_id == node_id)
        .map(|stream| (stream.width, stream.height))
        .unwrap_or((0, 0))
}

type LifecycleTsfn =
    Arc<ThreadsafeFunction<(String, String), (), (String, String), Status, false, false, 8>>;

#[napi(object, js_name = "ScreenCaptureStartResult")]
pub struct ScreenCaptureStartResult {
    pub width: u32,
    pub height: u32,
    pub frame_rate: u32,
    pub pixel_format: String,
}

#[napi(object, js_name = "ScreenCaptureRect")]
pub struct ScreenCaptureRect {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

#[napi(object, js_name = "ScreenCaptureStartOptions")]
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

#[derive(Clone, Debug, Default, PartialEq)]
struct NormalizedStartOptions {
    show_cursor_clicks: bool,
    capture_rect: Option<NormalizedCaptureRect>,
    color_range: Option<LinuxColorRange>,
    color_space: Option<LinuxColorSpace>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct NormalizedCaptureRect {
    x: f64,
    y: f64,
    width: u32,
    height: u32,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum LinuxColorRange {
    Full,
    Limited,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum LinuxColorSpace {
    Rec709,
    Srgb,
}

#[napi(object, js_name = "FrameSinkDiagnostics")]
pub struct FrameSinkDiagnostics {
    pub accepted: f64,
    pub coalesced: f64,
    pub rejected: f64,
    #[napi(js_name = "mediaFramesDroppedWithoutSink")]
    pub media_frames_dropped_without_sink: f64,
}

#[napi(object, js_name = "LinuxScreenCaptureDiagnostics")]
pub struct ScreenCaptureDiagnostics {
    pub backend: Option<String>,
    #[napi(js_name = "activeStrategy")]
    pub active_strategy: Option<String>,
    #[napi(js_name = "requestedInjectionMethod")]
    pub requested_injection_method: Option<String>,
    #[napi(js_name = "injectionMethod")]
    pub injection_method: Option<String>,
    #[napi(js_name = "lastFallbackReason")]
    pub last_fallback_reason: Option<String>,
    #[napi(js_name = "frameTransport")]
    pub frame_transport: Option<String>,
    #[napi(js_name = "hostMappedCpuFallback")]
    pub host_mapped_cpu_fallback: Option<bool>,
    #[napi(js_name = "sourceDmabufMetadataAvailable")]
    pub source_dmabuf_metadata_available: Option<bool>,
    #[napi(js_name = "requestedImportMode")]
    pub requested_import_mode: Option<String>,
    #[napi(js_name = "importMode")]
    pub import_mode: Option<String>,
    #[napi(js_name = "mapHost")]
    pub map_host: Option<bool>,
    #[napi(js_name = "noModifiers")]
    pub no_modifiers: Option<bool>,
    pub linear: Option<bool>,
    #[napi(js_name = "zeroCopy")]
    pub zero_copy: Option<bool>,
    #[napi(js_name = "gpuImportAvailable")]
    pub gpu_import_available: Option<bool>,
    #[napi(js_name = "deviceUuidAdvertised")]
    pub device_uuid_advertised: Option<bool>,
    #[napi(js_name = "supportedImportModes")]
    pub supported_import_modes: Option<Vec<String>>,
    #[napi(js_name = "clientConnected")]
    pub client_connected: Option<bool>,
    #[napi(js_name = "connectedClient")]
    pub connected_client: Option<String>,
    #[napi(js_name = "connectedPid")]
    pub connected_pid: Option<i32>,
    pub width: Option<u32>,
    pub height: Option<u32>,
    #[napi(js_name = "textureFormat")]
    pub texture_format: Option<String>,
    #[napi(js_name = "textureModifier")]
    pub texture_modifier: Option<String>,
    #[napi(js_name = "frameCounter")]
    pub frame_counter: Option<f64>,
    #[napi(js_name = "droppedFrameCounter")]
    pub dropped_frame_counter: Option<f64>,
    #[napi(js_name = "laggedFrameCounter")]
    pub lagged_frame_counter: Option<f64>,
    #[napi(js_name = "convertQueueDroppedFrameCounter")]
    pub convert_queue_dropped_frame_counter: Option<f64>,
    #[napi(js_name = "unsupportedFrameCounter")]
    pub unsupported_frame_counter: Option<f64>,
    #[napi(js_name = "lastPresentTimestampUs")]
    pub last_present_timestamp_us: Option<i64>,
    #[napi(js_name = "lastDiagnostic")]
    pub last_diagnostic: Option<String>,
    #[napi(js_name = "lastAddonError")]
    pub last_addon_error: Option<String>,
}

impl ScreenCaptureDiagnostics {
    fn pipewire() -> Self {
        Self {
            backend: Some(BACKEND.to_string()),
            active_strategy: Some("pipewire-portal".to_string()),
            requested_injection_method: None,
            injection_method: None,
            last_fallback_reason: None,
            frame_transport: None,
            host_mapped_cpu_fallback: None,
            source_dmabuf_metadata_available: None,
            requested_import_mode: None,
            import_mode: None,
            map_host: None,
            no_modifiers: None,
            linear: None,
            zero_copy: None,
            gpu_import_available: None,
            device_uuid_advertised: None,
            supported_import_modes: None,
            client_connected: None,
            connected_client: None,
            connected_pid: None,
            width: None,
            height: None,
            texture_format: None,
            texture_modifier: None,
            frame_counter: None,
            dropped_frame_counter: None,
            lagged_frame_counter: None,
            convert_queue_dropped_frame_counter: None,
            unsupported_frame_counter: None,
            last_present_timestamp_us: None,
            last_diagnostic: None,
            last_addon_error: None,
        }
    }
}

impl From<GameCaptureDiagnostics> for ScreenCaptureDiagnostics {
    fn from(value: GameCaptureDiagnostics) -> Self {
        Self {
            backend: Some(value.backend),
            active_strategy: Some(value.active_strategy),
            requested_injection_method: Some(value.requested_injection_method),
            injection_method: Some(value.injection_method),
            last_fallback_reason: value.last_fallback_reason,
            frame_transport: Some(value.frame_transport),
            host_mapped_cpu_fallback: Some(value.host_mapped_cpu_fallback),
            source_dmabuf_metadata_available: Some(value.source_dmabuf_metadata_available),
            requested_import_mode: Some(value.requested_import_mode),
            import_mode: Some(value.import_mode),
            map_host: Some(value.map_host),
            no_modifiers: Some(value.no_modifiers),
            linear: Some(value.linear),
            zero_copy: Some(value.zero_copy),
            gpu_import_available: Some(value.gpu_import_available),
            device_uuid_advertised: Some(value.device_uuid_advertised),
            supported_import_modes: Some(value.supported_import_modes),
            client_connected: Some(value.client_connected),
            connected_client: value.connected_client,
            connected_pid: value.connected_pid,
            width: value.width,
            height: value.height,
            texture_format: value.texture_format,
            texture_modifier: value.texture_modifier,
            frame_counter: Some(value.frame_counter as f64),
            dropped_frame_counter: Some(value.dropped_frame_counter as f64),
            lagged_frame_counter: Some(value.lagged_frame_counter as f64),
            convert_queue_dropped_frame_counter: None,
            unsupported_frame_counter: Some(value.unsupported_frame_counter as f64),
            last_present_timestamp_us: value.last_present_timestamp_us,
            last_diagnostic: value.last_diagnostic,
            last_addon_error: value.last_addon_error,
        }
    }
}

struct CaptureState {
    lifecycle_tsfn: Option<LifecycleTsfn>,
    session: Option<LiveSession>,
    stream: Option<PipeWireVideoStream>,
    game_stream: Option<GameCaptureVideoStream>,
}

struct CaptureInner {
    state: Mutex<CaptureState>,
    running: Arc<AtomicBool>,
    capture_id: Arc<Mutex<Option<String>>>,
    native_frame_sink: Arc<Mutex<Option<Arc<NativeScreenFrameSinkHandleRef>>>>,
    frame_sink_accepted: AtomicU64,
    frame_sink_coalesced: AtomicU64,
    frame_sink_rejected: AtomicU64,
    media_frames_dropped_without_sink: AtomicU64,
    native_sink_cpu_fallback_emitted: AtomicBool,
}

#[napi(js_name = "ScreenCapture")]
pub struct ScreenCapture {
    inner: Arc<CaptureInner>,
}

#[napi]
impl ScreenCapture {
    #[napi(constructor)]
    pub fn new() -> Self {
        Self {
            inner: Arc::new(CaptureInner {
                state: Mutex::new(CaptureState {
                    lifecycle_tsfn: None,
                    session: None,
                    stream: None,
                    game_stream: None,
                }),
                running: Arc::new(AtomicBool::new(false)),
                capture_id: Arc::new(Mutex::new(None)),
                native_frame_sink: Arc::new(Mutex::new(None)),
                frame_sink_accepted: AtomicU64::new(0),
                frame_sink_coalesced: AtomicU64::new(0),
                frame_sink_rejected: AtomicU64::new(0),
                media_frames_dropped_without_sink: AtomicU64::new(0),
                native_sink_cpu_fallback_emitted: AtomicBool::new(false),
            }),
        }
    }

    #[napi]
    pub fn set_lifecycle_callback(&self, callback: Function<(String, String), ()>) -> Result<()> {
        let tsfn: LifecycleTsfn = Arc::new(
            callback
                .build_threadsafe_function::<(String, String)>()
                .max_queue_size::<8>()
                .build_callback(|ctx| Ok(ctx.value))?,
        );
        let mut state = lock_state(&self.inner)?;
        state.lifecycle_tsfn = Some(tsfn);
        Ok(())
    }

    #[napi(js_name = "setFrameSinkHandle")]
    pub fn set_frame_sink_handle(&self, frame_sink_handle: Unknown<'_>) -> Result<()> {
        let sink = retain_native_frame_sink_handle(frame_sink_handle)?;
        let mut guard = self
            .inner
            .native_frame_sink
            .lock()
            .map_err(|_| generic_error("ScreenCapture native frame sink lock poisoned"))?;
        *guard = Some(sink);
        Ok(())
    }

    #[napi]
    pub async fn start(
        &self,
        source_id: String,
        source_kind: String,
        width: u32,
        height: u32,
        frame_rate: u32,
        capture_id: Option<String>,
        capture_options: Option<ScreenCaptureStartOptions>,
    ) -> Result<ScreenCaptureStartResult> {
        if source_id.is_empty() {
            return Err(invalid_arg(
                "ScreenCapture.start sourceId must be non-empty",
            ));
        }
        if source_kind != "screen" && source_kind != "window" && source_kind != "game" {
            return Err(invalid_arg(
                "ScreenCapture.start sourceKind must be 'screen', 'window', or 'game'",
            ));
        }
        let start_options = normalize_start_options(capture_options)?;

        {
            let mut guard = self
                .inner
                .capture_id
                .lock()
                .map_err(|_| generic_error("ScreenCapture capture_id lock poisoned"))?;
            *guard = capture_id
                .map(|raw| raw.trim().to_string())
                .filter(|trimmed| !trimmed.is_empty());
        }

        let lifecycle_tsfn = {
            let state = lock_state(&self.inner)?;
            state.lifecycle_tsfn.clone()
        };

        let captured_width = Arc::new(std::sync::atomic::AtomicU32::new(0));
        let captured_height = Arc::new(std::sync::atomic::AtomicU32::new(0));
        let captured_width_for_cb = captured_width.clone();
        let captured_height_for_cb = captured_height.clone();
        let capture_id_value = self
            .inner
            .capture_id
            .lock()
            .ok()
            .and_then(|guard| guard.clone());
        let native_frame_sink = self
            .inner
            .native_frame_sink
            .lock()
            .ok()
            .and_then(|guard| guard.clone());
        let bus_sink = if native_frame_sink.is_some() {
            None
        } else {
            capture_id_value.as_deref().and_then(frame_bus::get_sink)
        };
        let inner_for_frames = Arc::clone(&self.inner);
        let frame_cb: FrameCallback = Arc::new(move |frame: VideoFrame| {
            captured_width_for_cb.store(frame.width, Ordering::Release);
            captured_height_for_cb.store(frame.height, Ordering::Release);
            if let Some(sink) = native_frame_sink.as_deref() {
                let cpu_fallback = frame.dmabuf.is_none();
                let outcome = enqueue_native_bus_video_frame(sink, &frame);
                record_frame_sink_outcome(&inner_for_frames, outcome);
                if cpu_fallback {
                    note_native_frame_sink_cpu_fallback(&inner_for_frames);
                }
            } else if let Some(sink) = bus_sink.as_deref() {
                let outcome = enqueue_bus_video_frame(sink, frame);
                record_frame_sink_outcome(&inner_for_frames, outcome);
            } else {
                inner_for_frames
                    .media_frames_dropped_without_sink
                    .fetch_add(1, Ordering::AcqRel);
            }
        });

        let inner_running = self.inner.running.clone();
        let lifecycle_cb: LifecycleCallback = Arc::new(move |kind: &str, message: &str| {
            if kind == "closed" || kind == "closed-clean" {
                inner_running.store(false, Ordering::Release);
            }
            let Some(tsfn) = lifecycle_tsfn.as_ref() else {
                return;
            };
            let _: Status = tsfn.call(
                (kind.to_string(), message.to_string()),
                ThreadsafeFunctionCallMode::NonBlocking,
            );
        });

        let effective_fps = if frame_rate == 0 {
            30
        } else {
            frame_rate.min(240)
        };

        emit_linux_start_option_diagnostics(&start_options, &lifecycle_cb);
        if source_kind == "game" {
            let game_stream = GameCaptureVideoStream::open(effective_fps, frame_cb, lifecycle_cb)
                .map_err(|e| {
                generic_error(format!("obs-vkcapture game stream open failed: {e}"))
            })?;
            {
                let mut state = lock_state(&self.inner)?;
                state.session = None;
                state.stream = None;
                state.game_stream = Some(game_stream);
            }
            self.inner.running.store(true, Ordering::Release);
            return Ok(ScreenCaptureStartResult {
                width,
                height,
                frame_rate: effective_fps,
                pixel_format: "nv12".to_string(),
            });
        }

        let node_id = parse_source_id(&source_id).ok_or_else(|| {
            invalid_arg("ScreenCapture.start sourceId must be a positive u32 string")
        })?;
        let parked = adopt_session().ok_or_else(|| {
            generic_error("no live portal session — call listSources() immediately before start()")
        })?;
        let session = parked.session;
        let (portal_width, portal_height) = stream_dimensions_for_node(&parked.streams, node_id);

        let fd = match portal::open_pipewire_remote(&session) {
            Ok(fd) => fd,
            Err(err) => {
                session.close();
                return Err(generic_error(format!(
                    "OpenPipeWireRemote failed: {} ({err})",
                    portal_error_to_status(&err)
                )));
            }
        };

        let pool_width = start_options
            .capture_rect
            .map(|rect| rect.width)
            .unwrap_or(width)
            .max(portal_width);
        let pool_height = start_options
            .capture_rect
            .map(|rect| rect.height)
            .unwrap_or(height)
            .max(portal_height);
        let pool = build_linux_screen_pool(pool_width, pool_height)?;
        let exhaust_cb: Option<PoolExhaustionCallback> = None;
        let stream =
            PipeWireVideoStream::open(fd, node_id, frame_cb, lifecycle_cb, pool, exhaust_cb)
                .map_err(|e| generic_error(format!("PipeWire stream open failed: {e}")))?;

        {
            let mut state = lock_state(&self.inner)?;
            state.session = Some(session);
            state.stream = Some(stream);
            state.game_stream = None;
        }
        self.inner.running.store(true, Ordering::Release);

        let captured_width = captured_width.load(Ordering::Acquire);
        let captured_height = captured_height.load(Ordering::Acquire);
        Ok(ScreenCaptureStartResult {
            width: if captured_width > 0 {
                captured_width
            } else {
                portal_width
            },
            height: if captured_height > 0 {
                captured_height
            } else {
                portal_height
            },
            frame_rate: effective_fps,
            pixel_format: "nv12".to_string(),
        })
    }

    #[napi]
    pub async fn stop(&self) -> Result<()> {
        let was_running = self.inner.running.swap(false, Ordering::AcqRel);
        if let Ok(mut guard) = self.inner.capture_id.lock() {
            guard.take();
        }
        if let Ok(mut guard) = self.inner.native_frame_sink.lock() {
            guard.take();
        }
        let (stream, game_stream, session) = {
            let mut state = lock_state(&self.inner)?;
            (
                state.stream.take(),
                state.game_stream.take(),
                state.session.take(),
            )
        };
        drop(stream);
        drop(game_stream);
        if let Some(s) = session {
            s.close();
        }
        if was_running {
            emit_lifecycle_tsfn(&self.inner, "closed-clean", "capture stopped");
        }
        Ok(())
    }

    #[napi(js_name = "getDiagnostics")]
    pub fn get_diagnostics(&self) -> Result<Option<ScreenCaptureDiagnostics>> {
        let state = lock_state(&self.inner)?;
        if let Some(game_stream) = state.game_stream.as_ref() {
            return Ok(Some(game_stream.diagnostics().into()));
        }
        if let Some(stream) = state.stream.as_ref() {
            let mut diagnostics = ScreenCaptureDiagnostics::pipewire();
            diagnostics.dropped_frame_counter = Some(stream.frames_dropped_pool_exhausted() as f64);
            diagnostics.convert_queue_dropped_frame_counter =
                Some(stream.frames_dropped_convert_queue_full() as f64);
            diagnostics.unsupported_frame_counter = Some(stream.frames_dropped_oversized() as f64);
            return Ok(Some(diagnostics));
        }
        Ok(None)
    }

    #[napi(js_name = "getFrameSinkDiagnostics")]
    pub fn get_frame_sink_diagnostics(&self) -> FrameSinkDiagnostics {
        FrameSinkDiagnostics {
            accepted: self.inner.frame_sink_accepted.load(Ordering::Acquire) as f64,
            coalesced: self.inner.frame_sink_coalesced.load(Ordering::Acquire) as f64,
            rejected: self.inner.frame_sink_rejected.load(Ordering::Acquire) as f64,
            media_frames_dropped_without_sink: self
                .inner
                .media_frames_dropped_without_sink
                .load(Ordering::Acquire) as f64,
        }
    }
}

impl Drop for ScreenCapture {
    fn drop(&mut self) {
        self.inner.running.store(false, Ordering::Release);
        if let Ok(mut guard) = self.inner.native_frame_sink.lock() {
            guard.take();
        }
        let (stream, game_stream, session) = match self.inner.state.lock() {
            Ok(mut s) => (s.stream.take(), s.game_stream.take(), s.session.take()),
            Err(_) => (None, None, None),
        };
        drop(stream);
        drop(game_stream);
        if let Some(s) = session {
            s.close();
        }
    }
}

fn lock_state(inner: &CaptureInner) -> Result<std::sync::MutexGuard<'_, CaptureState>> {
    inner
        .state
        .lock()
        .map_err(|_| generic_error("ScreenCapture state lock poisoned"))
}

fn normalize_start_options(
    options: Option<ScreenCaptureStartOptions>,
) -> Result<NormalizedStartOptions> {
    let Some(options) = options else {
        return Ok(NormalizedStartOptions::default());
    };
    Ok(NormalizedStartOptions {
        show_cursor_clicks: options.show_cursor_clicks.unwrap_or(false),
        capture_rect: normalize_capture_rect(options.capture_rect)?,
        color_range: normalize_color_range(options.color_range.as_deref())?,
        color_space: normalize_color_space(options.color_space.as_deref())?,
    })
}

fn normalize_capture_rect(
    rect: Option<ScreenCaptureRect>,
) -> Result<Option<NormalizedCaptureRect>> {
    let Some(rect) = rect else {
        return Ok(None);
    };
    if !rect.x.is_finite() || !rect.y.is_finite() {
        return Err(invalid_arg(
            "ScreenCapture.start captureRect origin must be finite",
        ));
    }
    if !rect.width.is_finite() || !rect.height.is_finite() {
        return Err(invalid_arg(
            "ScreenCapture.start captureRect size must be finite",
        ));
    }
    if rect.x < 0.0 || rect.y < 0.0 || rect.width <= 0.0 || rect.height <= 0.0 {
        return Err(invalid_arg(
            "ScreenCapture.start captureRect requires non-negative x/y and positive width/height",
        ));
    }
    if rect.width > u32::MAX as f64 || rect.height > u32::MAX as f64 {
        return Err(invalid_arg(
            "ScreenCapture.start captureRect width/height exceed u32",
        ));
    }
    Ok(Some(NormalizedCaptureRect {
        x: rect.x,
        y: rect.y,
        width: rect.width.round() as u32,
        height: rect.height.round() as u32,
    }))
}

fn normalize_color_range(value: Option<&str>) -> Result<Option<LinuxColorRange>> {
    match value {
        None => Ok(None),
        Some("full") => Ok(Some(LinuxColorRange::Full)),
        Some("limited") => Ok(Some(LinuxColorRange::Limited)),
        Some(_) => Err(invalid_arg(
            "ScreenCapture.start colorRange must be 'limited' or 'full'",
        )),
    }
}

fn normalize_color_space(value: Option<&str>) -> Result<Option<LinuxColorSpace>> {
    match value {
        None => Ok(None),
        Some("rec709") => Ok(Some(LinuxColorSpace::Rec709)),
        Some("srgb") => Ok(Some(LinuxColorSpace::Srgb)),
        Some(_) => Err(invalid_arg(
            "ScreenCapture.start colorSpace must be 'srgb' or 'rec709'",
        )),
    }
}

fn emit_linux_start_option_diagnostics(
    options: &NormalizedStartOptions,
    lifecycle_cb: &LifecycleCallback,
) {
    if options.show_cursor_clicks {
        lifecycle_cb(
            "diagnostic",
            "showCursorClicks requested but Linux portal capture uses hidden cursor mode",
        );
    }
    if options.capture_rect.is_some() {
        lifecycle_cb(
            "diagnostic",
            "captureRect requested; Linux PipeWire pool uses the requested size but portal capture cannot crop the compositor stream",
        );
    }
    if options.color_range.is_some() {
        lifecycle_cb(
            "diagnostic",
            "colorRange requested but Linux PipeWire portal capture cannot force color range",
        );
    }
    if options.color_space.is_some() {
        lifecycle_cb(
            "diagnostic",
            "colorSpace requested but Linux PipeWire portal capture cannot force color space",
        );
    }
}

fn record_frame_sink_outcome(inner: &CaptureInner, outcome: EnqueueOutcome) {
    match outcome {
        EnqueueOutcome::Accepted => {
            inner.frame_sink_accepted.fetch_add(1, Ordering::AcqRel);
        }
        EnqueueOutcome::Coalesced => {
            inner.frame_sink_coalesced.fetch_add(1, Ordering::AcqRel);
        }
        EnqueueOutcome::Rejected => {
            inner.frame_sink_rejected.fetch_add(1, Ordering::AcqRel);
        }
    }
}

fn note_native_frame_sink_cpu_fallback(inner: &CaptureInner) {
    if inner
        .native_sink_cpu_fallback_emitted
        .swap(true, Ordering::AcqRel)
    {
        return;
    }
    emit_lifecycle_tsfn(
        inner,
        "error",
        "Linux native frame sink requires DMA-BUF frames; refusing CPU-copy fallback",
    );
}

fn emit_lifecycle_tsfn(inner: &CaptureInner, kind: &str, message: &str) {
    let Ok(state) = inner.state.lock() else {
        return;
    };
    let Some(tsfn) = state.lifecycle_tsfn.as_ref() else {
        return;
    };
    let _: Status = tsfn.call(
        (kind.to_string(), message.to_string()),
        ThreadsafeFunctionCallMode::NonBlocking,
    );
}

fn retain_native_frame_sink_handle(
    value: Unknown<'_>,
) -> Result<Arc<NativeScreenFrameSinkHandleRef>> {
    if value.get_type()? != ValueType::External {
        return Err(invalid_arg(
            "ScreenCapture.setFrameSinkHandle expects a native external frame sink handle",
        ));
    }

    let raw_value = value.value();
    let mut data: *mut c_void = std::ptr::null_mut();
    let status =
        unsafe { napi::sys::napi_get_value_external(raw_value.env, raw_value.value, &mut data) };
    if status != napi::sys::Status::napi_ok || data.is_null() {
        return Err(invalid_arg(
            "ScreenCapture.setFrameSinkHandle received an empty native external frame sink handle",
        ));
    }

    let handle = unsafe { data.cast::<NativeScreenFrameSinkHandle>().as_ref() }
        .and_then(NativeScreenFrameSinkHandle::retain_ref)
        .ok_or_else(|| {
            invalid_arg(
                "ScreenCapture.setFrameSinkHandle received an invalid native frame sink handle",
            )
        })?;

    Ok(Arc::new(handle))
}

fn enqueue_native_bus_video_frame(
    sink: &NativeScreenFrameSinkHandleRef,
    frame: &VideoFrame,
) -> EnqueueOutcome {
    if let Some(dmabuf) = frame.dmabuf.as_ref() {
        if !sink.supports_dmabuf() {
            return EnqueueOutcome::Rejected;
        }
        let plane_count = dmabuf.plane_count as usize;
        if plane_count == 0 || plane_count > 4 || dmabuf.fds.len() < plane_count {
            return EnqueueOutcome::Rejected;
        }

        let mut duped_fds: Vec<OwnedFd> = Vec::with_capacity(plane_count);
        for raw in dmabuf.fds.iter().take(plane_count) {
            let duped = unsafe { libc::dup(*raw) };
            if duped < 0 {
                return EnqueueOutcome::Rejected;
            }
            duped_fds.push(unsafe { OwnedFd::from_raw_fd(duped) });
        }

        let desc = BusDmabufDesc {
            plane_count: dmabuf.plane_count as u8,
            width: frame.width,
            height: frame.height,
            drm_format: dmabuf.drm_format,
            modifier: dmabuf.modifier,
            strides: dmabuf.strides,
            offsets: dmabuf.offsets,
            device_uuid: dmabuf.device_uuid.unwrap_or([0u8; 16]),
            timestamp_us: frame.timestamp_us,
        };

        return sink.enqueue_dmabuf_take_fds(desc, duped_fds);
    }

    sink.enqueue_nv12_copy(
        &frame.data,
        frame.width,
        frame.height,
        frame.stride_y,
        frame.stride_uv,
        frame.timestamp_us,
    )
}

fn enqueue_bus_video_frame(
    sink: &dyn frame_bus::ScreenFrameSink,
    frame: VideoFrame,
) -> EnqueueOutcome {
    if let Some(dmabuf) = frame.dmabuf.as_ref() {
        let mut owned_fds: Vec<OwnedFd> = Vec::with_capacity(dmabuf.plane_count as usize);
        for raw in dmabuf.fds.iter().take(dmabuf.plane_count as usize) {
            let duped = unsafe { libc::dup(*raw) };
            if duped < 0 {
                return EnqueueOutcome::Rejected;
            }
            owned_fds.push(unsafe { OwnedFd::from_raw_fd(duped) });
        }
        let bus_dmabuf = BusDmabufFrame {
            desc: BusDmabufDesc {
                plane_count: dmabuf.plane_count as u8,
                width: frame.width,
                height: frame.height,
                drm_format: dmabuf.drm_format,
                modifier: dmabuf.modifier,
                strides: dmabuf.strides,
                offsets: dmabuf.offsets,
                device_uuid: dmabuf.device_uuid.unwrap_or([0u8; 16]),
                timestamp_us: frame.timestamp_us,
            },
            fds: owned_fds,
        };
        return sink.enqueue(BusScreenFrame::Dmabuf(bus_dmabuf));
    }
    let VideoFrame {
        width,
        height,
        stride_y,
        stride_uv,
        timestamp_us,
        data,
        ..
    } = frame;
    sink.enqueue(BusScreenFrame::Nv12(BusNv12Frame {
        data: data.into_bus_frame_data(),
        width,
        height,
        stride_y,
        stride_uv,
        timestamp_us,
    }))
}

const LINUX_POOL_DIM_MIN: usize = 64;
const LINUX_POOL_DIM_MAX: usize = LINUX_FRAME_DIM_MAX;
const LINUX_POOL_BYTES_MAX: usize = LINUX_FRAME_BYTES_MAX;
const LINUX_POOL_BYTES_MIN_NV12: usize = LINUX_POOL_DIM_MIN * LINUX_POOL_DIM_MIN * 3 / 2;

fn build_linux_screen_pool(width: u32, height: u32) -> Result<Arc<LinuxFrameBufferPool>> {
    let bytes_per_buffer = compute_pool_bytes_per_buffer(width, height);
    assert!(bytes_per_buffer >= LINUX_POOL_BYTES_MIN_NV12);
    assert!(bytes_per_buffer <= LINUX_POOL_BYTES_MAX);
    LinuxFrameBufferPool::new(bytes_per_buffer).map_err(|err| {
        generic_error(format!(
            "Failed to build Linux screen capture frame pool: {err}"
        ))
    })
}

fn compute_pool_bytes_per_buffer(width: u32, height: u32) -> usize {
    let w = (width as usize).clamp(LINUX_POOL_DIM_MIN, LINUX_POOL_DIM_MAX);
    let h = (height as usize).clamp(LINUX_POOL_DIM_MIN, LINUX_POOL_DIM_MAX);
    let nv12 = w.saturating_mul(h).saturating_mul(3) / 2;
    let clamped = nv12.clamp(LINUX_POOL_BYTES_MIN_NV12, LINUX_POOL_BYTES_MAX);
    assert!(clamped >= LINUX_POOL_BYTES_MIN_NV12);
    assert!(clamped <= LINUX_POOL_BYTES_MAX);
    clamped
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_source_id_roundtrip() {
        let encoded = encode_source_id(7);
        assert_eq!(encoded, "7");
        let node = parse_source_id(&encoded).expect("roundtrip");
        assert_eq!(node, 7);
    }

    #[test]
    fn parse_source_id_rejects_malformed() {
        assert!(parse_source_id("garbage").is_none());
        assert!(parse_source_id("0").is_none());
        assert!(parse_source_id("").is_none());
        assert!(parse_source_id("-1").is_none());
    }

    #[test]
    fn source_kind_for_type_maps_bits() {
        assert_eq!(source_kind_for_type(SOURCE_TYPE_WINDOW), "window");
        assert_eq!(
            source_kind_for_type(crate::portal::SOURCE_TYPE_MONITOR),
            "screen"
        );
        assert_eq!(source_kind_for_type(0), "screen");
    }

    #[test]
    fn stream_dimensions_for_node_uses_selected_portal_stream() {
        let streams = vec![
            StreamInfo {
                node_id: 11,
                source_type: 0,
                mapping_id: None,
                width: 1280,
                height: 720,
                position_x: 0,
                position_y: 0,
            },
            StreamInfo {
                node_id: 42,
                source_type: 0,
                mapping_id: None,
                width: 1920,
                height: 1080,
                position_x: 1280,
                position_y: 0,
            },
        ];

        assert_eq!(stream_dimensions_for_node(&streams, 42), (1920, 1080));
        assert_eq!(stream_dimensions_for_node(&streams, 99), (0, 0));
    }

    #[test]
    fn start_options_normalize_capture_rect_and_color_intent() {
        let options = normalize_start_options(Some(ScreenCaptureStartOptions {
            show_cursor_clicks: Some(true),
            capture_rect: Some(ScreenCaptureRect {
                x: 10.0,
                y: 20.0,
                width: 300.0,
                height: 200.0,
            }),
            color_range: Some("full".to_string()),
            color_space: Some("rec709".to_string()),
        }))
        .expect("valid options");
        assert!(options.show_cursor_clicks);
        assert_eq!(options.color_range, Some(LinuxColorRange::Full));
        assert_eq!(options.color_space, Some(LinuxColorSpace::Rec709));
        assert_eq!(
            options.capture_rect,
            Some(NormalizedCaptureRect {
                x: 10.0,
                y: 20.0,
                width: 300,
                height: 200,
            })
        );
    }

    #[test]
    fn pool_bytes_cover_4k_nv12_stream() {
        let bytes = compute_pool_bytes_per_buffer(3840, 2160);
        assert_eq!(bytes, 3840 * 2160 * 3 / 2);
        assert!(bytes <= LINUX_POOL_BYTES_MAX);
    }

    #[test]
    fn pool_bytes_clamp_dimensions_to_named_caps() {
        let oversized = compute_pool_bytes_per_buffer(u32::MAX, u32::MAX);
        assert_eq!(oversized, LINUX_POOL_DIM_MAX * LINUX_POOL_DIM_MAX * 3 / 2);
        let undersized = compute_pool_bytes_per_buffer(1, 1);
        assert_eq!(undersized, LINUX_POOL_BYTES_MIN_NV12);
    }

    #[test]
    fn start_options_reject_invalid_capture_rect() {
        let err = normalize_start_options(Some(ScreenCaptureStartOptions {
            show_cursor_clicks: None,
            capture_rect: Some(ScreenCaptureRect {
                x: 0.0,
                y: 0.0,
                width: 0.0,
                height: 200.0,
            }),
            color_range: None,
            color_space: None,
        }))
        .expect_err("invalid capture rect");
        assert_eq!(err.status, Status::InvalidArg);
    }
}
