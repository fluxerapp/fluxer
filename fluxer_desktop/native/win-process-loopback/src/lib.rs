// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ptr;
use std::sync::Arc;

use fluxer_desktop_native::win_process_loopback::audio_contract;
use fluxer_screen_frame_bus::{NativeScreenFrameSinkHandle, NativeScreenFrameSinkHandleRef};
use napi::{
    Env, JsValue,
    bindgen_prelude::{
        Error, Float32Array, Function, Result, Status, ToNapiValue, Unknown, ValueType,
    },
    sys,
    threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode, UnknownReturnValue},
};
use napi_derive::napi;

pub mod pcm_pool;

use crate::pcm_pool::{PCM_SLOT_SAMPLES_MAX, PooledPcmFrame};

fn retain_screen_audio_sink_handle(
    value: Unknown<'_>,
) -> Result<Arc<NativeScreenFrameSinkHandleRef>> {
    if value.get_type()? != ValueType::External {
        return Err(Error::new(
            Status::InvalidArg,
            "ProcessLoopback.setScreenAudioSink expects a native external sink handle",
        ));
    }
    let raw_value = value.value();
    let mut data: *mut std::ffi::c_void = ptr::null_mut();
    let status = unsafe { sys::napi_get_value_external(raw_value.env, raw_value.value, &mut data) };
    if status != sys::Status::napi_ok || data.is_null() {
        return Err(Error::new(
            Status::InvalidArg,
            "ProcessLoopback.setScreenAudioSink received an empty native external sink handle",
        ));
    }
    let handle = unsafe {
        NativeScreenFrameSinkHandle::retain_from_raw(data.cast::<NativeScreenFrameSinkHandle>())
    }
    .ok_or_else(|| {
        Error::new(
            Status::InvalidArg,
            "ProcessLoopback.setScreenAudioSink received an invalid native sink handle",
        )
    })?;
    Ok(Arc::new(handle))
}

pub struct ErrorMessage(pub String);

impl ToNapiValue for ErrorMessage {
    unsafe fn to_napi_value(env: sys::napi_env, val: Self) -> Result<sys::napi_value> {
        let napi_err = Error::new(Status::GenericFailure, val.0);
        let js_err = napi::JsError::from(napi_err);
        Ok(unsafe { js_err.into_value(env) })
    }
}

#[napi(object)]
pub struct LoopbackFrame {
    pub samples: napi::bindgen_prelude::Float32Array,
    pub sample_rate: u32,
    pub channels: u32,
    pub timestamp_us: i64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CaptureScope {
    Process,
    System,
    SessionMixer,
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(not(target_os = "windows"), allow(dead_code))]
struct LoopbackOptions {
    include_tree: bool,
    capture_scope: CaptureScope,
}

#[derive(Debug, Clone)]
struct ProcessLoopbackRuntimeProbe {
    include_supported: bool,
    include_detail: Option<String>,
    exclude_supported: bool,
    exclude_detail: Option<String>,
}

impl Default for LoopbackOptions {
    fn default() -> Self {
        Self {
            include_tree: true,
            capture_scope: CaptureScope::Process,
        }
    }
}

fn validate_options(raw: RawOptions) -> Result<LoopbackOptions> {
    if let Some(sample_rate) = raw.sample_rate
        && !audio_contract::validate_sample_rate(sample_rate)
    {
        return Err(Error::new(
            Status::InvalidArg,
            "unsupported sampleRate; expected 48000",
        ));
    }
    if let Some(channels) = raw.channels
        && !audio_contract::validate_channels(channels)
    {
        return Err(Error::new(
            Status::InvalidArg,
            "unsupported channels; expected 2",
        ));
    }
    let capture_scope = match raw.capture_scope.as_deref() {
        Some("session-mixer") => CaptureScope::SessionMixer,
        Some("system") => CaptureScope::System,
        Some("process") | None => CaptureScope::Process,
        Some(other) => {
            return Err(Error::new(
                Status::InvalidArg,
                format!("unknown captureScope: {other}"),
            ));
        }
    };
    let include_tree = match capture_scope {
        CaptureScope::System | CaptureScope::SessionMixer => false,
        CaptureScope::Process => raw.include_process_tree.unwrap_or(true),
    };
    Ok(LoopbackOptions {
        include_tree,
        capture_scope,
    })
}

#[derive(Debug, Default)]
struct RawOptions {
    include_process_tree: Option<bool>,
    capture_scope: Option<String>,
    sample_rate: Option<u32>,
    channels: Option<u32>,
}

fn read_raw_options(env: &Env, value: Unknown) -> Result<RawOptions> {
    use napi::ValueType;
    let value_type = value.get_type()?;
    if value_type != ValueType::Object {
        return Ok(RawOptions::default());
    }
    let object: napi::bindgen_prelude::Object = unsafe { value.cast() }?;
    Ok(RawOptions {
        include_process_tree: object.get("includeProcessTree").ok().flatten(),
        capture_scope: object.get("captureScope").ok().flatten(),
        sample_rate: object.get("sampleRate").ok().flatten(),
        channels: object.get("channels").ok().flatten(),
    })
    .map(|mut opts| {
        let _ = env;

        opts.capture_scope = opts.capture_scope.map(|s| s.to_ascii_lowercase());
        opts
    })
}

#[cfg_attr(not(target_os = "windows"), allow(dead_code))]
enum FrameSamples {
    Pooled(PooledPcmFrame),
    Owned(Vec<f32>),
}

#[cfg_attr(not(target_os = "windows"), allow(dead_code))]
struct FramePayload {
    samples: FrameSamples,
    timestamp_us: i64,
}

fn pooled_pcm_into_float32_array(slot: PooledPcmFrame) -> Float32Array {
    let (ptr, len, slot) = slot.into_external_parts();
    debug_assert!(!ptr.is_null());
    debug_assert!(len <= PCM_SLOT_SAMPLES_MAX);
    if ptr.is_null() || len == 0 || len > PCM_SLOT_SAMPLES_MAX {
        drop(slot);
        let empty: Vec<f32> = Vec::new();
        return Float32Array::new(empty);
    }
    unsafe {
        Float32Array::with_external_data(ptr, len, move |_data, _len| {
            let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                drop(slot);
            }));
        })
    }
}

fn loopback_frame_sample_truncation(len: usize) -> usize {
    let channels = usize::from(audio_contract::TARGET_CHANNELS);
    if channels == 0 {
        return 0;
    }
    len - (len % channels)
}

fn report_loopback_frame_truncation_once(original_len: usize, kept_len: usize) {
    use std::sync::atomic::{AtomicBool, Ordering};
    static REPORTED: AtomicBool = AtomicBool::new(false);
    if REPORTED.swap(true, Ordering::Relaxed) {
        return;
    }
    eprintln!(
        "win-process-loopback: dropping {} trailing screen-audio sample(s) to keep whole {}-channel frames",
        original_len.saturating_sub(kept_len),
        audio_contract::TARGET_CHANNELS
    );
}

fn owned_samples_into_whole_frame_array(mut samples: Vec<f32>) -> Float32Array {
    let kept = loopback_frame_sample_truncation(samples.len());
    if kept != samples.len() {
        report_loopback_frame_truncation_once(samples.len(), kept);
        samples.truncate(kept);
    }
    samples.into()
}

fn frame_payload_into_loopback_frame(payload: FramePayload) -> LoopbackFrame {
    let samples = match payload.samples {
        FrameSamples::Pooled(slot) => pooled_pcm_into_float32_array(slot),
        FrameSamples::Owned(samples) => owned_samples_into_whole_frame_array(samples),
    };
    LoopbackFrame {
        samples,
        sample_rate: audio_contract::TARGET_SAMPLE_RATE,
        channels: u32::from(audio_contract::TARGET_CHANNELS),
        timestamp_us: payload.timestamp_us,
    }
}

fn empty_loopback_frame(timestamp_us: i64) -> LoopbackFrame {
    LoopbackFrame {
        samples: Vec::<f32>::new().into(),
        sample_rate: audio_contract::TARGET_SAMPLE_RATE,
        channels: u32::from(audio_contract::TARGET_CHANNELS),
        timestamp_us,
    }
}

fn guard_frame_callback<F>(timestamp_us: i64, build: F) -> LoopbackFrame
where
    F: FnOnce() -> LoopbackFrame,
{
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(build)) {
        Ok(frame) => frame,
        Err(_) => {
            report_loopback_frame_callback_panic_once();
            empty_loopback_frame(timestamp_us)
        }
    }
}

fn frame_callback_into_loopback_frame(payload: FramePayload) -> LoopbackFrame {
    let timestamp_us = payload.timestamp_us;
    guard_frame_callback(timestamp_us, move || {
        frame_payload_into_loopback_frame(payload)
    })
}

fn report_loopback_frame_callback_panic_once() {
    use std::sync::atomic::{AtomicBool, Ordering};
    static REPORTED: AtomicBool = AtomicBool::new(false);
    if REPORTED.swap(true, Ordering::Relaxed) {
        return;
    }
    eprintln!(
        "win-process-loopback: screen-audio frame callback panicked; dropping frame instead of aborting the process"
    );
}

type FrameTsfn = Arc<
    ThreadsafeFunction<
        FramePayload,
        UnknownReturnValue,
        LoopbackFrame,
        Status,
        false,
        true,
        FRAME_QUEUE_LIMIT,
    >,
>;
type ErrorTsfn = Arc<
    ThreadsafeFunction<
        ErrorMessage,
        UnknownReturnValue,
        ErrorMessage,
        Status,
        false,
        true,
        ERROR_QUEUE_LIMIT,
    >,
>;
type VoidTsfn =
    Arc<ThreadsafeFunction<(), UnknownReturnValue, (), Status, false, true, LIFECYCLE_QUEUE_LIMIT>>;
const FRAME_QUEUE_LIMIT: usize = 64;
const ERROR_QUEUE_LIMIT: usize = 8;
const LIFECYCLE_QUEUE_LIMIT: usize = 8;

#[cfg_attr(not(target_os = "windows"), allow(dead_code))]
struct Callbacks {
    on_frame: FrameTsfn,
    on_error: ErrorTsfn,
    on_closed: VoidTsfn,
    on_started: Option<VoidTsfn>,
    screen_audio_sink: std::sync::RwLock<Option<Arc<NativeScreenFrameSinkHandleRef>>>,
}

#[cfg_attr(not(target_os = "windows"), allow(dead_code))]
impl Callbacks {
    fn set_screen_audio_sink(&self, sink: Arc<NativeScreenFrameSinkHandleRef>) {
        if let Ok(mut guard) = self.screen_audio_sink.write() {
            *guard = Some(sink);
        }
    }

    fn clear_screen_audio_sink(&self) {
        if let Ok(mut guard) = self.screen_audio_sink.write() {
            *guard = None;
        }
    }

    fn try_emit_frame_to_sink(&self, frame: &FramePayload) -> bool {
        let Ok(guard) = self.screen_audio_sink.read() else {
            return false;
        };
        let Some(sink) = guard.as_ref() else {
            return false;
        };
        let samples: &[f32] = match &frame.samples {
            FrameSamples::Pooled(slot) => slot.data_slice(),
            FrameSamples::Owned(owned) => owned.as_slice(),
        };
        let channels = u32::from(audio_contract::TARGET_CHANNELS);
        if channels == 0 || samples.is_empty() || samples.len() % (channels as usize) != 0 {
            return true;
        }
        let frames = samples.len() as u32 / channels;
        sink.enqueue_screen_audio_f32(
            samples,
            frames,
            channels,
            audio_contract::TARGET_SAMPLE_RATE,
            frame.timestamp_us,
        );
        true
    }

    fn emit_frame(&self, frame: FramePayload) {
        if self.try_emit_frame_to_sink(&frame) {
            return;
        }
        let _ = self
            .on_frame
            .call(frame, ThreadsafeFunctionCallMode::NonBlocking);
    }

    fn emit_error(&self, message: impl Into<String>) {
        let _ = self.on_error.call(
            ErrorMessage(message.into()),
            ThreadsafeFunctionCallMode::NonBlocking,
        );
    }

    fn emit_closed(&self) {
        let _ = self
            .on_closed
            .call((), ThreadsafeFunctionCallMode::NonBlocking);
    }

    fn emit_started(&self) {
        if let Some(started) = self.on_started.as_ref() {
            let _ = started.call((), ThreadsafeFunctionCallMode::NonBlocking);
        }
    }
}

fn build_frame_tsfn(
    env: &Env,
    callback: Function<LoopbackFrame, UnknownReturnValue>,
) -> Result<FrameTsfn> {
    let _ = env;
    callback
        .build_threadsafe_function::<FramePayload>()
        .weak::<true>()
        .callee_handled::<false>()
        .max_queue_size::<FRAME_QUEUE_LIMIT>()
        .build_callback(|ctx| Ok(frame_callback_into_loopback_frame(ctx.value)))
        .map(Arc::new)
        .map_err(clone_napi_err)
}

fn build_error_tsfn(
    env: &Env,
    callback: Function<ErrorMessage, UnknownReturnValue>,
) -> Result<ErrorTsfn> {
    let _ = env;
    callback
        .build_threadsafe_function::<ErrorMessage>()
        .weak::<true>()
        .callee_handled::<false>()
        .max_queue_size::<ERROR_QUEUE_LIMIT>()
        .build()
        .map(Arc::new)
        .map_err(clone_napi_err)
}

fn clone_napi_err(err: napi::Error) -> Error {
    Error::new(Status::GenericFailure, err.to_string())
}

fn build_void_tsfn(env: &Env, callback: Function<(), UnknownReturnValue>) -> Result<VoidTsfn> {
    let _ = env;
    callback
        .build_threadsafe_function::<()>()
        .weak::<true>()
        .callee_handled::<false>()
        .max_queue_size::<LIFECYCLE_QUEUE_LIMIT>()
        .build()
        .map(Arc::new)
        .map_err(clone_napi_err)
}

#[napi]
pub struct ProcessLoopback {
    inner: platform::Inner,
}

#[napi]
impl ProcessLoopback {
    #[allow(clippy::too_many_arguments)]
    #[napi(constructor)]
    pub fn new(
        env: Env,
        pid: u32,
        opts: Unknown,
        on_frame: Function<LoopbackFrame, UnknownReturnValue>,
        on_error: Function<ErrorMessage, UnknownReturnValue>,
        on_closed: Function<(), UnknownReturnValue>,
        on_started: Option<Function<(), UnknownReturnValue>>,
    ) -> Result<Self> {
        let raw = read_raw_options(&env, opts)?;
        let options = validate_options(raw)?;
        let callbacks = Callbacks {
            on_frame: build_frame_tsfn(&env, on_frame)?,
            on_error: build_error_tsfn(&env, on_error)?,
            on_closed: build_void_tsfn(&env, on_closed)?,
            on_started: on_started.map(|cb| build_void_tsfn(&env, cb)).transpose()?,
            screen_audio_sink: std::sync::RwLock::new(None),
        };
        Ok(Self {
            inner: platform::Inner::new(pid, options, callbacks)?,
        })
    }

    #[napi(js_name = "setScreenAudioSink")]
    pub fn set_screen_audio_sink(&self, sink_handle: Unknown<'_>) -> Result<()> {
        let sink = retain_screen_audio_sink_handle(sink_handle)?;
        if !sink.supports_screen_audio() {
            return Err(Error::new(
                Status::InvalidArg,
                "ProcessLoopback.setScreenAudioSink handle does not support screen audio",
            ));
        }
        self.inner.set_screen_audio_sink(sink);
        Ok(())
    }

    #[napi(js_name = "clearScreenAudioSink")]
    pub fn clear_screen_audio_sink(&self) {
        self.inner.clear_screen_audio_sink();
    }

    #[napi]
    pub fn start(&self) -> Result<()> {
        self.inner.start()
    }

    #[napi]
    pub fn stop(&self) {
        self.inner.stop();
    }

    #[napi]
    pub fn dispose(&self) {
        self.inner.dispose();
    }
}

impl Drop for ProcessLoopback {
    fn drop(&mut self) {
        self.inner.dispose();
    }
}

#[napi(js_name = "isSupported")]
pub fn is_supported() -> bool {
    platform::is_supported()
}

#[napi(object, js_name = "WinProcessLoopbackBackendInfo")]
pub struct WinProcessLoopbackBackendInfo {
    pub backend: String,
    pub supported: bool,
    pub reason: String,
    #[napi(js_name = "processSupported")]
    pub process_supported: bool,
    #[napi(js_name = "systemSupported")]
    pub system_supported: bool,
    #[napi(js_name = "systemExcludesSelf")]
    pub system_excludes_self: bool,
    #[napi(js_name = "processIncludeSupported")]
    pub process_include_supported: bool,
    #[napi(js_name = "processExcludeSupported")]
    pub process_exclude_supported: bool,
    #[napi(js_name = "sessionMixerSupported")]
    pub session_mixer_supported: bool,
    #[napi(js_name = "systemLoopbackMode")]
    pub system_loopback_mode: String,
    #[napi(js_name = "minWindowsBuild")]
    pub min_windows_build: u32,
    #[napi(js_name = "minWindowsVersionLabel")]
    pub min_windows_version_label: String,
    #[napi(js_name = "detectedWindowsBuild")]
    pub detected_windows_build: Option<u32>,
}

const MIN_WINDOWS_VERSION_LABEL: &str = "Windows build 20348 (Microsoft process-loopback minimum)";

fn classify_windows_process_loopback(
    detected: Option<u32>,
    min_build: u32,
    runtime_supported: bool,
    runtime_detail: Option<&str>,
) -> (bool, String) {
    let probe_detail = runtime_detail
        .filter(|detail| !detail.is_empty())
        .map(|detail| format!(" Probe detail: {detail}"))
        .unwrap_or_default();

    if runtime_supported {
        return (
            true,
            match detected {
                Some(build) if build < min_build => format!(
                    "Process loopback activation succeeded on Windows build {build}, \
                     below Microsoft's documented {MIN_WINDOWS_VERSION_LABEL}. \
                     Fluxer is enabling it because the runtime API probe passed."
                ),
                Some(build) => format!(
                    "Process loopback activation succeeded on Windows build {build} \
                     (documented minimum: {MIN_WINDOWS_VERSION_LABEL})."
                ),
                None => format!(
                    "Process loopback activation succeeded, but the Windows build could not \
                     be detected (documented minimum: {MIN_WINDOWS_VERSION_LABEL})."
                ),
            },
        );
    }

    match detected {
        None => (
            false,
            format!(
                "@fluxer/win-process-loopback only supports Windows. \
                 Per-process audio loopback requires {MIN_WINDOWS_VERSION_LABEL}; \
                 ActivateAudioInterfaceAsync's per-process virtual device is unavailable on \
                 this host.{probe_detail}"
            ),
        ),
        Some(build) if build >= min_build => (
            false,
            format!(
                "Windows build {build} meets Microsoft's documented \
                 {MIN_WINDOWS_VERSION_LABEL}, but Fluxer's runtime process-loopback probe \
                 failed. Per-process audio loopback is unavailable.{probe_detail}"
            ),
        ),
        Some(build) => (
            false,
            format!(
                "Microsoft documents process loopback as requiring \
                 {MIN_WINDOWS_VERSION_LABEL}. This host reports Windows build {build}, \
                 and Fluxer's runtime process-loopback probe failed. Fluxer must not use \
                 system-wide endpoint loopback because it cannot exclude Fluxer's WebRTC \
                 playback.{probe_detail}"
            ),
        ),
    }
}

fn classify_windows_capabilities(
    detected: Option<u32>,
    min_build: u32,
    include_supported: bool,
    include_detail: Option<&str>,
    exclude_supported: bool,
    exclude_detail: Option<&str>,
) -> (bool, bool, bool, bool, bool, bool, bool, String, String) {
    let (process_include_supported, include_reason) =
        classify_windows_process_loopback(detected, min_build, include_supported, include_detail);
    let (process_exclude_supported, exclude_reason) =
        classify_windows_process_loopback(detected, min_build, exclude_supported, exclude_detail);
    let session_mixer_supported = process_include_supported;
    let process_supported = process_include_supported;
    let system_supported = process_exclude_supported || session_mixer_supported;
    let system_excludes_self = system_supported;
    let system_loopback_mode = if process_exclude_supported {
        "process-exclude".to_owned()
    } else if session_mixer_supported {
        "session-mixer".to_owned()
    } else {
        "unavailable".to_owned()
    };
    let supported = process_supported || system_supported;
    let reason = if supported {
        if process_exclude_supported {
            "Per-process audio loopback is supported; desktop audio uses process-exclude loopback."
                .to_owned()
        } else {
            "Per-process audio loopback include mode is supported; desktop audio uses Fluxer's session mixer fallback."
                .to_owned()
        }
    } else if include_reason == exclude_reason {
        include_reason
    } else {
        format!("Include probe: {include_reason} Exclude probe: {exclude_reason}")
    };
    (
        supported,
        process_supported,
        system_supported,
        system_excludes_self,
        process_include_supported,
        process_exclude_supported,
        session_mixer_supported,
        system_loopback_mode,
        reason,
    )
}

#[napi(js_name = "getBackendInfo")]
pub fn get_backend_info() -> WinProcessLoopbackBackendInfo {
    let detected = platform::detected_build();
    let runtime_probe = platform::process_loopback_probe();
    let min_build =
        fluxer_desktop_native::win_process_loopback::windows_version::PROCESS_LOOPBACK_MIN_BUILD;
    let (
        supported,
        process_supported,
        system_supported,
        system_excludes_self,
        process_include_supported,
        process_exclude_supported,
        session_mixer_supported,
        system_loopback_mode,
        reason,
    ) = classify_windows_capabilities(
        detected,
        min_build,
        runtime_probe.include_supported,
        runtime_probe.include_detail.as_deref(),
        runtime_probe.exclude_supported,
        runtime_probe.exclude_detail.as_deref(),
    );
    WinProcessLoopbackBackendInfo {
        backend: "win-process-loopback".to_owned(),
        supported,
        reason,
        process_supported,
        system_supported,
        system_excludes_self,
        process_include_supported,
        process_exclude_supported,
        session_mixer_supported,
        system_loopback_mode,
        min_windows_build: min_build,
        min_windows_version_label: MIN_WINDOWS_VERSION_LABEL.to_owned(),
        detected_windows_build: detected,
    }
}

#[napi(js_name = "pidFromHwnd")]
pub fn pid_from_hwnd(hwnd: napi::bindgen_prelude::BigInt) -> u32 {
    let (_, value, _) = hwnd.get_u64();
    platform::pid_from_hwnd(value)
}

#[napi(js_name = "resolveAudioRootPid")]
pub fn resolve_audio_root_pid(pid: u32) -> u32 {
    platform::resolve_audio_root_pid(pid)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn raw_with_scope(scope: Option<&str>) -> RawOptions {
        RawOptions {
            include_process_tree: None,
            capture_scope: scope.map(|s| s.to_string()),
            sample_rate: None,
            channels: None,
        }
    }

    #[test]
    fn defaults_include_tree_under_process_scope() {
        let opts = validate_options(RawOptions::default()).expect("defaults validate");
        assert!(opts.include_tree);
        assert_eq!(opts.capture_scope, CaptureScope::Process);
    }

    #[test]
    fn system_scope_forces_exclude_target_tree() {
        let opts =
            validate_options(raw_with_scope(Some("system"))).expect("system scope validates");
        assert!(!opts.include_tree);
        assert_eq!(opts.capture_scope, CaptureScope::System);
    }

    #[test]
    fn session_mixer_scope_ignores_target_tree_flag() {
        let mut raw = raw_with_scope(Some("session-mixer"));
        raw.include_process_tree = Some(true);
        let opts = validate_options(raw).expect("session mixer scope validates");
        assert!(!opts.include_tree);
        assert_eq!(opts.capture_scope, CaptureScope::SessionMixer);
    }

    #[test]
    fn process_scope_respects_include_tree_override() {
        let mut raw = raw_with_scope(Some("process"));
        raw.include_process_tree = Some(false);
        let opts = validate_options(raw).expect("process scope validates");
        assert!(!opts.include_tree);
    }

    #[test]
    fn unknown_capture_scope_is_rejected() {
        let err = validate_options(raw_with_scope(Some("global")))
            .expect_err("unknown scope should fail validation");
        assert_eq!(err.status, Status::InvalidArg);
    }

    #[test]
    fn unsupported_sample_rate_is_rejected() {
        let raw = RawOptions {
            sample_rate: Some(44_100),
            ..Default::default()
        };
        let err = validate_options(raw).expect_err("44.1k should be rejected");
        assert_eq!(err.status, Status::InvalidArg);
        assert_eq!(err.reason, "unsupported sampleRate; expected 48000");
    }

    #[test]
    fn unsupported_channel_count_is_rejected() {
        let raw = RawOptions {
            channels: Some(1),
            ..Default::default()
        };
        let err = validate_options(raw).expect_err("mono should be rejected");
        assert_eq!(err.status, Status::InvalidArg);
        assert_eq!(err.reason, "unsupported channels; expected 2");
    }

    #[test]
    fn backend_info_classifier_rejects_unknown_host() {
        let (supported, reason) =
            classify_windows_process_loopback(None, 20_348, false, Some("probe unavailable"));
        assert!(!supported);
        assert!(reason.contains("only supports Windows"));
        assert!(reason.contains("build 20348"));
        assert!(reason.contains("probe unavailable"));
    }

    #[test]
    fn backend_info_classifier_rejects_old_build_when_probe_fails() {
        let (supported, reason) =
            classify_windows_process_loopback(Some(19_045), 20_348, false, Some("E_NOTIMPL"));
        assert!(!supported);
        assert!(reason.contains("build 19045"));
        assert!(reason.contains("Windows build 20348"));
        assert!(reason.contains("E_NOTIMPL"));
    }

    #[test]
    fn backend_info_classifier_accepts_old_build_when_probe_succeeds() {
        let (supported, reason) =
            classify_windows_process_loopback(Some(19_045), 20_348, true, None);
        assert!(supported);
        assert!(reason.contains("build 19045"));
        assert!(reason.contains("runtime API probe passed"));
    }

    #[test]
    fn backend_info_classifier_accepts_exact_floor_when_probe_succeeds() {
        let (supported, reason) =
            classify_windows_process_loopback(Some(20_348), 20_348, true, None);
        assert!(supported);
        assert!(reason.contains("build 20348"));
    }

    #[test]
    fn backend_info_classifier_rejects_newer_build_when_probe_fails() {
        let (supported, reason) =
            classify_windows_process_loopback(Some(26_100), 20_348, false, Some("timeout"));
        assert!(!supported);
        assert!(reason.contains("runtime process-loopback probe failed"));
        assert!(reason.contains("timeout"));
    }

    #[test]
    fn backend_info_classifier_accepts_newer_build_when_probe_succeeds() {
        let (supported, _) = classify_windows_process_loopback(Some(26_100), 20_348, true, None);
        assert!(supported);
    }

    #[test]
    fn backend_capabilities_reject_windows_10_for_isolated_audio() {
        let (
            supported,
            process_supported,
            system_supported,
            system_excludes_self,
            process_include_supported,
            process_exclude_supported,
            session_mixer_supported,
            system_loopback_mode,
            reason,
        ) = classify_windows_capabilities(
            Some(19_045),
            20_348,
            false,
            Some("include E_NOTIMPL"),
            false,
            Some("exclude E_NOTIMPL"),
        );
        assert!(!supported);
        assert!(!process_supported);
        assert!(!system_supported);
        assert!(!system_excludes_self);
        assert!(!process_include_supported);
        assert!(!process_exclude_supported);
        assert!(!session_mixer_supported);
        assert_eq!(system_loopback_mode, "unavailable");
        assert!(reason.contains("build 19045"));
        assert!(reason.contains("runtime process-loopback probe failed"));
    }

    #[test]
    fn backend_capabilities_use_session_mixer_when_only_include_mode_works() {
        let (
            supported,
            process_supported,
            system_supported,
            system_excludes_self,
            process_include_supported,
            process_exclude_supported,
            session_mixer_supported,
            system_loopback_mode,
            _,
        ) = classify_windows_capabilities(
            Some(19_045),
            20_348,
            true,
            Some("include ok"),
            false,
            Some("exclude E_NOTIMPL"),
        );
        assert!(supported);
        assert!(process_supported);
        assert!(system_supported);
        assert!(system_excludes_self);
        assert!(process_include_supported);
        assert!(!process_exclude_supported);
        assert!(session_mixer_supported);
        assert_eq!(system_loopback_mode, "session-mixer");
    }

    #[test]
    fn backend_capabilities_keep_runtime_supported_windows_process_exclude_system_mode() {
        let (
            supported,
            process_supported,
            system_supported,
            system_excludes_self,
            process_include_supported,
            process_exclude_supported,
            session_mixer_supported,
            system_loopback_mode,
            _,
        ) = classify_windows_capabilities(Some(22_000), 20_348, true, None, true, None);
        assert!(supported);
        assert!(process_supported);
        assert!(system_supported);
        assert!(system_excludes_self);
        assert!(process_include_supported);
        assert!(process_exclude_supported);
        assert!(session_mixer_supported);
        assert_eq!(system_loopback_mode, "process-exclude");
    }

    #[test]
    fn accepts_canonical_48k_stereo() {
        let raw = RawOptions {
            include_process_tree: Some(true),
            capture_scope: Some("process".to_string()),
            sample_rate: Some(48_000),
            channels: Some(2),
        };
        validate_options(raw).expect("canonical 48k stereo should validate");
    }

    #[test]
    fn truncation_keeps_only_whole_stereo_frames() {
        assert_eq!(loopback_frame_sample_truncation(0), 0);
        assert_eq!(loopback_frame_sample_truncation(1), 0);
        assert_eq!(loopback_frame_sample_truncation(2), 2);
        assert_eq!(loopback_frame_sample_truncation(3), 2);
        assert_eq!(loopback_frame_sample_truncation(5), 4);
        assert_eq!(loopback_frame_sample_truncation(10), 10);
    }

    #[test]
    fn owned_odd_length_samples_truncate_without_panic() {
        let odd = vec![0.1_f32, 0.2, 0.3];
        let array = owned_samples_into_whole_frame_array(odd);
        assert_eq!(array.len(), 2);
    }

    #[test]
    fn owned_even_length_samples_are_preserved() {
        let even = vec![0.1_f32, 0.2, 0.3, 0.4];
        let array = owned_samples_into_whole_frame_array(even);
        assert_eq!(array.len(), 4);
    }

    #[test]
    fn frame_callback_truncates_owned_payload_without_panic() {
        let payload = FramePayload {
            samples: FrameSamples::Owned(vec![0.5_f32, 0.6, 0.7]),
            timestamp_us: 1_234,
        };
        let frame = frame_callback_into_loopback_frame(payload);
        assert_eq!(frame.samples.len(), 2);
        assert_eq!(frame.channels, u32::from(audio_contract::TARGET_CHANNELS));
        assert_eq!(frame.timestamp_us, 1_234);
    }

    #[test]
    fn frame_callback_swallows_a_panicking_conversion() {
        let frame = guard_frame_callback(99, || panic!("forced frame conversion panic"));
        assert_eq!(frame.samples.len(), 0);
        assert_eq!(frame.timestamp_us, 99);
        assert_eq!(frame.sample_rate, audio_contract::TARGET_SAMPLE_RATE);
        assert_eq!(frame.channels, u32::from(audio_contract::TARGET_CHANNELS));
    }
}

#[cfg(target_os = "windows")]
mod platform {
    use super::{
        Callbacks, CaptureScope, FRAME_QUEUE_LIMIT, FramePayload, FrameSamples, LoopbackOptions,
        ProcessLoopbackRuntimeProbe,
    };
    use crate::pcm_pool::{PCM_SLOT_SAMPLES_MAX, PcmFramePool, PcmPoolError, PooledPcmFrame};
    use fluxer_desktop_native::win_process_loopback::{
        audio_contract, process_tree, session_mixer,
    };
    use fluxer_rt_thread::{PriorityProfile, RealtimePriorityGuard};
    use napi::bindgen_prelude::{Error, Result, Status};
    use std::{
        collections::BTreeSet,
        ffi::c_void,
        mem::{ManuallyDrop, size_of, zeroed},
        sync::{
            Arc, Mutex, MutexGuard, OnceLock,
            atomic::{AtomicBool, Ordering},
        },
        thread::{self, JoinHandle},
        time::{Duration, Instant},
    };
    use windows::Win32::Foundation::{
        CloseHandle, GetLastError, HANDLE, HWND, INVALID_HANDLE_VALUE, WAIT_TIMEOUT,
    };
    use windows::Win32::Media::Audio::{
        AUDCLNT_BUFFERFLAGS_SILENT, AUDCLNT_BUFFERFLAGS_TIMESTAMP_ERROR, AUDCLNT_SHAREMODE_SHARED,
        AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM, AUDCLNT_STREAMFLAGS_EVENTCALLBACK,
        AUDCLNT_STREAMFLAGS_LOOPBACK, AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY,
        AUDIOCLIENT_ACTIVATION_PARAMS, AUDIOCLIENT_ACTIVATION_PARAMS_0,
        AUDIOCLIENT_ACTIVATION_TYPE_PROCESS_LOOPBACK, AUDIOCLIENT_PROCESS_LOOPBACK_PARAMS,
        ActivateAudioInterfaceAsync, AudioSessionStateExpired, DEVICE_STATE_ACTIVE,
        IActivateAudioInterfaceAsyncOperation, IActivateAudioInterfaceCompletionHandler,
        IActivateAudioInterfaceCompletionHandler_Impl, IAudioCaptureClient, IAudioClient,
        IAudioSessionControl, IAudioSessionControl2, IAudioSessionManager2, IMMDeviceEnumerator,
        MMDeviceEnumerator, PROCESS_LOOPBACK_MODE_EXCLUDE_TARGET_PROCESS_TREE,
        PROCESS_LOOPBACK_MODE_INCLUDE_TARGET_PROCESS_TREE, WAVE_FORMAT_PCM, WAVEFORMATEX,
        WAVEFORMATEXTENSIBLE, WAVEFORMATEXTENSIBLE_0, eRender,
    };
    use windows::Win32::Media::KernelStreaming::WAVE_FORMAT_EXTENSIBLE;
    use windows::Win32::Media::Multimedia::KSDATAFORMAT_SUBTYPE_IEEE_FLOAT;
    use windows::Win32::Storage::FileSystem::{
        GetFileVersionInfoSizeW, GetFileVersionInfoW, VS_FIXEDFILEINFO, VerQueryValueW,
    };
    use windows::Win32::System::Com::StructuredStorage::{
        PROPVARIANT, PROPVARIANT_0, PROPVARIANT_0_0, PROPVARIANT_0_0_0,
    };
    use windows::Win32::System::Com::{
        BLOB, CLSCTX_ALL, COINIT_MULTITHREADED, CoCreateInstance, CoInitializeEx, CoUninitialize,
        IAgileObject, IAgileObject_Impl,
    };
    use windows::Win32::System::Diagnostics::ToolHelp::{
        CreateToolhelp32Snapshot, PROCESSENTRY32W, Process32FirstW, Process32NextW,
        TH32CS_SNAPPROCESS,
    };
    use windows::Win32::System::Threading::{
        CreateEventW, GetCurrentProcessId, INFINITE, SetEvent, WaitForMultipleObjects,
    };
    use windows::Win32::System::Variant::VT_BLOB;
    use windows::Win32::UI::WindowsAndMessaging::GetWindowThreadProcessId;
    use windows::core::{Interface, PCWSTR, implement};

    const PROCESS_LOOPBACK_DEVICE: &[u16] = &[
        b'V' as u16,
        b'A' as u16,
        b'D' as u16,
        b'\\' as u16,
        b'P' as u16,
        b'r' as u16,
        b'o' as u16,
        b'c' as u16,
        b'e' as u16,
        b's' as u16,
        b's' as u16,
        b'_' as u16,
        b'L' as u16,
        b'o' as u16,
        b'o' as u16,
        b'p' as u16,
        b'b' as u16,
        b'a' as u16,
        b'c' as u16,
        b'k' as u16,
        0,
    ];

    const KERNEL32_PATH_UTF16: &[u16] = {
        const BYTES: &[u8] = b"C:\\Windows\\System32\\kernel32.dll\0";

        macro_rules! literal_utf16 {
            ($($c:literal),* $(,)?) => { &[$($c as u16),*] };
        }
        let _ = BYTES;
        literal_utf16![
            'C', ':', '\\', 'W', 'i', 'n', 'd', 'o', 'w', 's', '\\', 'S', 'y', 's', 't', 'e', 'm',
            '3', '2', '\\', 'k', 'e', 'r', 'n', 'e', 'l', '3', '2', '.', 'd', 'l', 'l', '\0'
        ]
    };

    const BACKSLASH_UTF16: &[u16] = &[b'\\' as u16, 0];
    const PROCESS_LOOPBACK_PROBE_TIMEOUT_MS: u32 = 1_500;
    const PROCESS_LOOPBACK_PROBE_COUNT: u64 = 2;
    const SESSION_MIXER_REFRESH_INTERVAL: Duration = Duration::from_millis(1_000);
    const SESSION_MIXER_WAIT_TIMEOUT_MS: u32 = 250;
    const MAX_SESSION_MIXER_CAPTURES: usize = 48;
    const MAX_SESSION_MIXER_EMIT_FRAMES: usize = audio_contract::TARGET_SAMPLE_RATE as usize / 2;
    const PROCESS_LOOPBACK_ACTIVATION_ATTEMPTS: usize = 3;

    struct OwnedHandle(HANDLE);

    impl OwnedHandle {
        fn new(handle: HANDLE) -> Option<Self> {
            if handle.is_invalid() {
                None
            } else {
                Some(Self(handle))
            }
        }

        fn raw(&self) -> HANDLE {
            self.0
        }
    }

    impl Drop for OwnedHandle {
        fn drop(&mut self) {
            if !self.0.is_invalid() && self.0 != INVALID_HANDLE_VALUE {
                unsafe {
                    let _ = CloseHandle(self.0);
                }
            }
        }
    }

    unsafe impl Send for OwnedHandle {}
    unsafe impl Sync for OwnedHandle {}

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    enum CaptureSampleFormat {
        Float32,
        Pcm16,
    }

    impl CaptureSampleFormat {
        fn label(self) -> &'static str {
            match self {
                Self::Float32 => "float32",
                Self::Pcm16 => "pcm16",
            }
        }
    }

    struct WorkerState {
        worker: Option<JoinHandle<()>>,
        stop_event: Option<Arc<OwnedHandle>>,
    }

    fn lock_worker(worker: &Mutex<WorkerState>) -> MutexGuard<'_, WorkerState> {
        worker
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    pub(crate) struct Inner {
        pid: u32,
        options: LoopbackOptions,
        callbacks: Arc<Callbacks>,
        worker: Mutex<WorkerState>,
        stop_signal: Arc<AtomicBool>,
        disposed: AtomicBool,
    }

    impl Inner {
        pub(crate) fn new(
            pid: u32,
            options: LoopbackOptions,
            callbacks: Callbacks,
        ) -> Result<Self> {
            let is_excluding_self_system_scope = matches!(
                options.capture_scope,
                CaptureScope::System | CaptureScope::SessionMixer
            ) && !options.include_tree;
            if is_self_or_descendant_pid(pid) && !is_excluding_self_system_scope {
                return Err(Error::new(
                    Status::GenericFailure,
                    "ProcessLoopback refuses to capture Fluxer's own process tree",
                ));
            }
            Ok(Self {
                pid,
                options,
                callbacks: Arc::new(callbacks),
                worker: Mutex::new(WorkerState {
                    worker: None,
                    stop_event: None,
                }),
                stop_signal: Arc::new(AtomicBool::new(false)),
                disposed: AtomicBool::new(false),
            })
        }

        pub(crate) fn set_screen_audio_sink(
            &self,
            sink: std::sync::Arc<super::NativeScreenFrameSinkHandleRef>,
        ) {
            self.callbacks.set_screen_audio_sink(sink);
        }

        pub(crate) fn clear_screen_audio_sink(&self) {
            self.callbacks.clear_screen_audio_sink();
        }

        pub(crate) fn start(&self) -> Result<()> {
            if self.disposed.load(Ordering::Acquire) {
                return Err(Error::new(
                    Status::GenericFailure,
                    "ProcessLoopback is disposed",
                ));
            }
            let mut guard = lock_worker(&self.worker);
            if guard.worker.is_some() {
                return Ok(());
            }
            self.stop_signal.store(false, Ordering::SeqCst);

            let stop_event = unsafe { CreateEventW(None, true, false, PCWSTR::null()) }.map_err(
                |err: windows::core::Error| {
                    hresult_error("CreateEventW for stop signal", err.code().0)
                },
            )?;
            let stop_event = Arc::new(
                OwnedHandle::new(stop_event)
                    .ok_or_else(|| generic("CreateEventW returned invalid handle"))?,
            );
            guard.stop_event = Some(stop_event.clone());

            let pid = self.pid;
            let include_tree = self.options.include_tree;
            let capture_scope = self.options.capture_scope;
            let stop_signal = self.stop_signal.clone();
            let stop_event_for_thread = stop_event.clone();
            let callbacks = self.callbacks.clone();

            let join = match thread::Builder::new()
                .name("fluxer-win-process-loopback".to_string())
                .spawn(move || {
                    let result = capture_thread(
                        pid,
                        include_tree,
                        capture_scope,
                        &callbacks,
                        stop_event_for_thread.raw(),
                        stop_signal.as_ref(),
                    );
                    if let Err(message) = result {
                        callbacks.emit_error(message);
                    }
                    callbacks.emit_closed();
                }) {
                Ok(join) => join,
                Err(err) => {
                    guard.stop_event = None;
                    return Err(generic(format!(
                        "failed to spawn loopback worker thread: {err}"
                    )));
                }
            };
            guard.worker = Some(join);
            Ok(())
        }

        pub(crate) fn stop(&self) {
            let join = {
                let mut guard = lock_worker(&self.worker);
                self.stop_signal.store(true, Ordering::SeqCst);
                if let Some(event) = guard.stop_event.as_ref() {
                    unsafe {
                        let _ = SetEvent(event.raw());
                    }
                }
                guard.worker.take()
            };
            if let Some(join) = join {
                let _ = join.join();
            }
            let mut guard = lock_worker(&self.worker);
            guard.stop_event = None;
        }

        pub(crate) fn dispose(&self) {
            if self.disposed.swap(true, Ordering::AcqRel) {
                return;
            }
            self.stop();
        }
    }

    fn generic(message: impl Into<String>) -> Error {
        Error::new(Status::GenericFailure, message.into())
    }

    fn hresult_error(stage: &str, hr: i32) -> Error {
        generic(format!(
            "Windows loopback activation failed at {stage}: hr=0x{:08x}",
            hr as u32
        ))
    }

    struct CompletionShared {
        done_event: Arc<OwnedHandle>,
        result: Mutex<Option<std::result::Result<IAudioClient, i32>>>,
    }

    unsafe impl Send for CompletionShared {}
    unsafe impl Sync for CompletionShared {}

    #[implement(IActivateAudioInterfaceCompletionHandler, IAgileObject)]
    struct CompletionHandler {
        shared: Arc<CompletionShared>,
    }

    impl IAgileObject_Impl for CompletionHandler_Impl {}

    impl IActivateAudioInterfaceCompletionHandler_Impl for CompletionHandler_Impl {
        fn ActivateCompleted(
            &self,
            operation: windows::core::Ref<IActivateAudioInterfaceAsyncOperation>,
        ) -> windows::core::Result<()> {
            let outcome = unsafe {
                let Some(op) = operation.as_ref() else {
                    return Ok(());
                };
                let mut activate_hr = windows::core::HRESULT(0);
                let mut unknown_raw: Option<windows::core::IUnknown> = None;
                let method_hr = op.GetActivateResult(&mut activate_hr, &mut unknown_raw);
                if let Err(err) = method_hr {
                    Err(err.code().0)
                } else if activate_hr.0 < 0 {
                    Err(activate_hr.0)
                } else if let Some(unknown) = unknown_raw {
                    match unknown.cast::<IAudioClient>() {
                        Ok(client) => Ok(client),
                        Err(err) => Err(err.code().0),
                    }
                } else {
                    Err(0)
                }
            };
            let mut slot = self
                .shared
                .result
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            *slot = Some(outcome);
            drop(slot);
            unsafe {
                let _ = SetEvent(self.shared.done_event.raw());
            }
            Ok(())
        }
    }

    fn make_blob_propvariant(data: *mut u8, size: u32) -> PROPVARIANT {
        PROPVARIANT {
            Anonymous: PROPVARIANT_0 {
                Anonymous: ManuallyDrop::new(PROPVARIANT_0_0 {
                    vt: VT_BLOB,
                    wReserved1: 0,
                    wReserved2: 0,
                    wReserved3: 0,
                    Anonymous: PROPVARIANT_0_0_0 {
                        blob: BLOB {
                            cbSize: size,
                            pBlobData: data,
                        },
                    },
                }),
            },
        }
    }

    fn activate_loopback_client(
        pid: u32,
        include_tree: bool,
        capture_scope: CaptureScope,
        stop_event: HANDLE,
    ) -> std::result::Result<IAudioClient, Error> {
        match capture_scope {
            CaptureScope::Process => {
                activate_process_loopback_client(pid, include_tree, stop_event)
            }
            CaptureScope::System => activate_process_loopback_client(pid, false, stop_event),
            CaptureScope::SessionMixer => activate_process_loopback_client(pid, true, stop_event),
        }
    }

    fn activate_process_loopback_client(
        pid: u32,
        include_tree: bool,
        stop_event: HANDLE,
    ) -> std::result::Result<IAudioClient, Error> {
        let mut last_error: Option<Error> = None;
        for attempt in 0..PROCESS_LOOPBACK_ACTIVATION_ATTEMPTS {
            match activate_process_loopback_client_with_timeout(
                pid,
                include_tree,
                stop_event,
                INFINITE,
            ) {
                Ok(client) => return Ok(client),
                Err(err) => {
                    if !is_retryable_activation_error(&err)
                        || attempt + 1 == PROCESS_LOOPBACK_ACTIVATION_ATTEMPTS
                    {
                        return Err(err);
                    }
                    last_error = Some(err);
                    let backoff_ms = 50 + (attempt as u32 * 100);
                    let wait = unsafe { WaitForMultipleObjects(&[stop_event], false, backoff_ms) };
                    if wait.0 == 0 {
                        return Err(generic("ProcessLoopback activation cancelled"));
                    }
                }
            }
        }
        Err(last_error.unwrap_or_else(|| generic("ProcessLoopback activation failed")))
    }

    fn is_retryable_activation_error(err: &Error) -> bool {
        let reason = err.reason.to_ascii_lowercase();
        reason.contains("getactivateresult")
            && (reason.contains("0x80040155")
                || reason.contains("0x80070002")
                || reason.contains("0x80070490"))
    }

    fn activate_process_loopback_client_with_timeout(
        pid: u32,
        include_tree: bool,
        stop_event: HANDLE,
        timeout_ms: u32,
    ) -> std::result::Result<IAudioClient, Error> {
        let done = unsafe { CreateEventW(None, true, false, PCWSTR::null()) }
            .map_err(|err: windows::core::Error| hresult_error("CreateEventW", err.code().0))?;
        let done = Arc::new(
            OwnedHandle::new(done)
                .ok_or_else(|| generic("CreateEventW returned invalid handle"))?,
        );

        let shared = Arc::new(CompletionShared {
            done_event: done.clone(),
            result: Mutex::new(None),
        });
        let handler: IActivateAudioInterfaceCompletionHandler = CompletionHandler {
            shared: shared.clone(),
        }
        .into();

        let loopback_mode = if include_tree {
            PROCESS_LOOPBACK_MODE_INCLUDE_TARGET_PROCESS_TREE
        } else {
            PROCESS_LOOPBACK_MODE_EXCLUDE_TARGET_PROCESS_TREE
        };
        let mut params = AUDIOCLIENT_ACTIVATION_PARAMS {
            ActivationType: AUDIOCLIENT_ACTIVATION_TYPE_PROCESS_LOOPBACK,
            Anonymous: AUDIOCLIENT_ACTIVATION_PARAMS_0 {
                ProcessLoopbackParams: AUDIOCLIENT_PROCESS_LOOPBACK_PARAMS {
                    TargetProcessId: pid,
                    ProcessLoopbackMode: loopback_mode,
                },
            },
        };

        let prop = ManuallyDrop::new(make_blob_propvariant(
            &mut params as *mut AUDIOCLIENT_ACTIVATION_PARAMS as *mut u8,
            size_of::<AUDIOCLIENT_ACTIVATION_PARAMS>() as u32,
        ));

        let operation = unsafe {
            ActivateAudioInterfaceAsync(
                PCWSTR(PROCESS_LOOPBACK_DEVICE.as_ptr()),
                &IAudioClient::IID,
                Some(&*prop),
                &handler,
            )
        }
        .map_err(|err| hresult_error("ActivateAudioInterfaceAsync", err.code().0))?;
        let _operation_lifetime = operation;

        let handles = [done.raw(), stop_event];
        let wait = unsafe { WaitForMultipleObjects(&handles, false, timeout_ms) };
        if wait.0 == u32::MAX {
            return Err(generic("WaitForMultipleObjects failed during activation"));
        }
        if wait == WAIT_TIMEOUT {
            return Err(generic("ProcessLoopback activation probe timed out"));
        }
        if wait.0 == 1 {
            return Err(generic("ProcessLoopback activation cancelled"));
        }
        if wait.0 != 0 {
            return Err(generic(
                "WaitForMultipleObjects returned an unexpected wait result",
            ));
        }
        let outcome = shared
            .result
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .take()
            .ok_or_else(|| generic("activation completion signalled without a result"))?;
        outcome.map_err(|hr| hresult_error("GetActivateResult", hr))
    }

    fn capture_thread(
        pid: u32,
        include_tree: bool,
        capture_scope: CaptureScope,
        callbacks: &Callbacks,
        stop_event: HANDLE,
        stop_signal: &AtomicBool,
    ) -> std::result::Result<(), String> {
        let _com = ComApartment::initialize().map_err(|err| err.reason.to_string())?;
        let _rt_guard = RealtimePriorityGuard::acquire(PriorityProfile::Audio).ok();
        if capture_scope == CaptureScope::SessionMixer {
            return session_mixer_capture_thread(callbacks, stop_event, stop_signal);
        }

        let client = match activate_loopback_client(pid, include_tree, capture_scope, stop_event) {
            Ok(client) => client,
            Err(err) if stop_signal.load(Ordering::SeqCst) => return Ok(()),
            Err(err) => return Err(err.reason.to_string()),
        };

        let sample_format = initialize_capture_client(&client)?;

        let buffer_event_raw = unsafe { CreateEventW(None, false, false, PCWSTR::null()) }
            .map_err(|err: windows::core::Error| {
                format!(
                    "CreateEventW(buffer) failed: hr=0x{:08x}",
                    err.code().0 as u32
                )
            })?;
        let buffer_event = OwnedHandle::new(buffer_event_raw)
            .ok_or_else(|| "CreateEventW(buffer) returned invalid handle".to_string())?;

        unsafe {
            client.SetEventHandle(buffer_event.raw()).map_err(|err| {
                format!(
                    "AudioClient::SetEventHandle failed: hr=0x{:08x}",
                    err.code().0 as u32
                )
            })?;
        }

        let capture: IAudioCaptureClient = unsafe {
            client.GetService::<IAudioCaptureClient>().map_err(|err| {
                format!(
                    "AudioClient::GetService(IAudioCaptureClient) failed: hr=0x{:08x}",
                    err.code().0 as u32
                )
            })?
        };

        let pcm_pool = PcmFramePool::new(FRAME_QUEUE_LIMIT, PCM_SLOT_SAMPLES_MAX)
            .map_err(|err| format!("PcmFramePool::new failed: {err}"))?;

        unsafe {
            client.Start().map_err(|err| {
                format!(
                    "AudioClient::Start failed: hr=0x{:08x}",
                    err.code().0 as u32
                )
            })?;
        }
        callbacks.emit_started();

        let pump_result = pump_capture_loop(
            &capture,
            sample_format,
            &pcm_pool,
            buffer_event.raw(),
            stop_event,
            stop_signal,
            callbacks,
        );
        unsafe {
            let _ = client.Stop();
        }
        pump_result
    }

    type CapturedPacket = session_mixer::MixerPacket;

    struct SessionCapture {
        pid: u32,
        client: IAudioClient,
        capture: IAudioCaptureClient,
        buffer_event: OwnedHandle,
        sample_format: CaptureSampleFormat,
    }

    impl SessionCapture {
        fn start(pid: u32, stop_event: HANDLE) -> std::result::Result<Self, String> {
            let client = activate_process_loopback_client(pid, true, stop_event)
                .map_err(|err| err.reason.to_string())?;
            let sample_format = initialize_capture_client(&client)?;
            let buffer_event_raw = unsafe { CreateEventW(None, false, false, PCWSTR::null()) }
                .map_err(|err: windows::core::Error| {
                    format!(
                        "CreateEventW(session buffer) failed: hr=0x{:08x}",
                        err.code().0 as u32
                    )
                })?;
            let buffer_event = OwnedHandle::new(buffer_event_raw).ok_or_else(|| {
                "CreateEventW(session buffer) returned invalid handle".to_string()
            })?;
            unsafe {
                client.SetEventHandle(buffer_event.raw()).map_err(|err| {
                    format!(
                        "AudioClient::SetEventHandle(session) failed: hr=0x{:08x}",
                        err.code().0 as u32
                    )
                })?;
            }
            let capture: IAudioCaptureClient = unsafe {
                client.GetService::<IAudioCaptureClient>().map_err(|err| {
                    format!(
                        "AudioClient::GetService(session IAudioCaptureClient) failed: hr=0x{:08x}",
                        err.code().0 as u32
                    )
                })?
            };
            unsafe {
                client.Start().map_err(|err| {
                    format!(
                        "AudioClient::Start(session) failed for pid {pid}: hr=0x{:08x}",
                        err.code().0 as u32
                    )
                })?;
            }
            Ok(Self {
                pid,
                client,
                capture,
                buffer_event,
                sample_format,
            })
        }

        fn stop(&self) {
            unsafe {
                let _ = self.client.Stop();
            }
        }
    }

    fn initialize_capture_client(
        client: &IAudioClient,
    ) -> std::result::Result<CaptureSampleFormat, String> {
        match initialize_capture_client_with_format(client, CaptureSampleFormat::Float32) {
            Ok(()) => Ok(CaptureSampleFormat::Float32),
            Err(float_error) => {
                match initialize_capture_client_with_format(client, CaptureSampleFormat::Pcm16) {
                    Ok(()) => Ok(CaptureSampleFormat::Pcm16),
                    Err(pcm_error) => Err(format!(
                        "AudioClient::Initialize failed for float32 ({float_error}) and pcm16 ({pcm_error})"
                    )),
                }
            }
        }
    }

    fn initialize_capture_client_with_format(
        client: &IAudioClient,
        sample_format: CaptureSampleFormat,
    ) -> std::result::Result<(), String> {
        let target_sample_rate = audio_contract::TARGET_SAMPLE_RATE;
        let target_channels = audio_contract::TARGET_CHANNELS;
        let stream_flags = AUDCLNT_STREAMFLAGS_LOOPBACK
            | AUDCLNT_STREAMFLAGS_EVENTCALLBACK
            | AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM
            | AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY;
        match sample_format {
            CaptureSampleFormat::Float32 => {
                const CAPTURE_BITS_PER_SAMPLE: u16 = 32;
                const CAPTURE_BYTES_PER_SAMPLE: u16 = CAPTURE_BITS_PER_SAMPLE / 8;
                let block_align = target_channels * CAPTURE_BYTES_PER_SAMPLE;
                let avg_bytes = target_sample_rate * u32::from(block_align);
                let format = WAVEFORMATEXTENSIBLE {
                    Format: WAVEFORMATEX {
                        wFormatTag: WAVE_FORMAT_EXTENSIBLE as u16,
                        nChannels: target_channels,
                        nSamplesPerSec: target_sample_rate,
                        nAvgBytesPerSec: avg_bytes,
                        nBlockAlign: block_align,
                        wBitsPerSample: CAPTURE_BITS_PER_SAMPLE,
                        cbSize: (size_of::<WAVEFORMATEXTENSIBLE>() - size_of::<WAVEFORMATEX>())
                            as u16,
                    },
                    Samples: WAVEFORMATEXTENSIBLE_0 {
                        wValidBitsPerSample: CAPTURE_BITS_PER_SAMPLE,
                    },
                    dwChannelMask: 0x3,
                    SubFormat: KSDATAFORMAT_SUBTYPE_IEEE_FLOAT,
                };
                let format_ptr = (&format as *const WAVEFORMATEXTENSIBLE).cast::<WAVEFORMATEX>();
                unsafe {
                    client
                        .Initialize(
                            AUDCLNT_SHAREMODE_SHARED,
                            stream_flags,
                            0,
                            0,
                            &*format_ptr,
                            None,
                        )
                        .map_err(|err| {
                            format!(
                                "{} init hr=0x{:08x}",
                                sample_format.label(),
                                err.code().0 as u32
                            )
                        })
                }
            }
            CaptureSampleFormat::Pcm16 => {
                const CAPTURE_BITS_PER_SAMPLE: u16 = 16;
                const CAPTURE_BYTES_PER_SAMPLE: u16 = CAPTURE_BITS_PER_SAMPLE / 8;
                let block_align = target_channels * CAPTURE_BYTES_PER_SAMPLE;
                let avg_bytes = target_sample_rate * u32::from(block_align);
                let format = WAVEFORMATEX {
                    wFormatTag: WAVE_FORMAT_PCM as u16,
                    nChannels: target_channels,
                    nSamplesPerSec: target_sample_rate,
                    nAvgBytesPerSec: avg_bytes,
                    nBlockAlign: block_align,
                    wBitsPerSample: CAPTURE_BITS_PER_SAMPLE,
                    cbSize: 0,
                };
                unsafe {
                    client
                        .Initialize(AUDCLNT_SHAREMODE_SHARED, stream_flags, 0, 0, &format, None)
                        .map_err(|err| {
                            format!(
                                "{} init hr=0x{:08x}",
                                sample_format.label(),
                                err.code().0 as u32
                            )
                        })
                }
            }
        }
    }

    fn session_mixer_capture_thread(
        callbacks: &Callbacks,
        stop_event: HANDLE,
        stop_signal: &AtomicBool,
    ) -> std::result::Result<(), String> {
        let mut captures: Vec<SessionCapture> = Vec::new();
        refresh_session_mixer_captures(&mut captures, stop_event)?;
        callbacks.emit_started();
        let mut handles: Vec<HANDLE> = Vec::with_capacity(MAX_SESSION_MIXER_CAPTURES + 1);
        rebuild_session_mixer_handles(&mut handles, &captures, stop_event);
        let mut packets: Vec<CapturedPacket> = Vec::new();
        let mut last_refresh = Instant::now();
        while !stop_signal.load(Ordering::SeqCst) {
            if last_refresh.elapsed() >= SESSION_MIXER_REFRESH_INTERVAL {
                refresh_session_mixer_captures(&mut captures, stop_event)?;
                rebuild_session_mixer_handles(&mut handles, &captures, stop_event);
                last_refresh = Instant::now();
            }
            let wait =
                unsafe { WaitForMultipleObjects(&handles, false, SESSION_MIXER_WAIT_TIMEOUT_MS) };
            if wait == WAIT_TIMEOUT {
                continue;
            }
            if wait.0 == u32::MAX {
                return Err("WaitForMultipleObjects failed in session mixer".to_string());
            }
            if wait.0 == 0 {
                break;
            }
            packets.clear();
            for capture in &captures {
                drain_packets_into(&capture.capture, capture.sample_format, &mut packets)?;
            }
            packets = emit_mixed_packets(std::mem::take(&mut packets), callbacks);
        }
        for capture in &captures {
            capture.stop();
        }
        Ok(())
    }

    fn rebuild_session_mixer_handles(
        handles: &mut Vec<HANDLE>,
        captures: &[SessionCapture],
        stop_event: HANDLE,
    ) {
        assert!(captures.len() <= MAX_SESSION_MIXER_CAPTURES);
        handles.clear();
        handles.push(stop_event);
        for capture in captures {
            handles.push(capture.buffer_event.raw());
        }
        assert_eq!(handles.len(), captures.len() + 1);
    }

    fn refresh_session_mixer_captures(
        captures: &mut Vec<SessionCapture>,
        stop_event: HANDLE,
    ) -> std::result::Result<(), String> {
        let desired =
            enumerate_shareable_audio_session_pids().map_err(|err| err.reason.to_string())?;
        captures.retain(|capture| {
            let keep = desired.contains(&capture.pid);
            if !keep {
                capture.stop();
            }
            keep
        });
        for pid in desired {
            if captures.len() >= MAX_SESSION_MIXER_CAPTURES {
                break;
            }
            if captures.iter().any(|capture| capture.pid == pid) {
                continue;
            }
            match SessionCapture::start(pid, stop_event) {
                Ok(capture) => captures.push(capture),
                Err(_) => continue,
            }
        }
        Ok(())
    }

    fn enumerate_shareable_audio_session_pids() -> std::result::Result<Vec<u32>, Error> {
        let enumerator: IMMDeviceEnumerator = unsafe {
            CoCreateInstance(
                &MMDeviceEnumerator,
                None::<&windows::core::IUnknown>,
                CLSCTX_ALL,
            )
        }
        .map_err(|err| hresult_error("CoCreateInstance(MMDeviceEnumerator)", err.code().0))?;
        let devices = unsafe { enumerator.EnumAudioEndpoints(eRender, DEVICE_STATE_ACTIVE) }
            .map_err(|err| hresult_error("EnumAudioEndpoints", err.code().0))?;
        let device_count = unsafe { devices.GetCount() }
            .map_err(|err| hresult_error("IMMDeviceCollection::GetCount", err.code().0))?;
        let process_entries = snapshot_process_entries().unwrap_or_default();
        let self_pid = unsafe { GetCurrentProcessId() };
        let mut pids = BTreeSet::new();
        let mut excluded_pids = BTreeSet::new();
        for device_index in 0..device_count {
            let Ok(device) = (unsafe { devices.Item(device_index) }) else {
                continue;
            };
            let Ok(manager) =
                (unsafe { device.Activate::<IAudioSessionManager2>(CLSCTX_ALL, None) })
            else {
                continue;
            };
            let Ok(sessions) = (unsafe { manager.GetSessionEnumerator() }) else {
                continue;
            };
            let Ok(session_count) = (unsafe { sessions.GetCount() }) else {
                continue;
            };
            for session_index in 0..session_count {
                let Ok(session): std::result::Result<IAudioSessionControl, _> =
                    (unsafe { sessions.GetSession(session_index) })
                else {
                    continue;
                };
                if matches!(unsafe { session.GetState() }, Ok(state) if state == AudioSessionStateExpired)
                {
                    continue;
                }
                let Ok(session2) = session.cast::<IAudioSessionControl2>() else {
                    continue;
                };
                if unsafe { session2.IsSystemSoundsSession() }.0 <= 0 {
                    continue;
                }
                let Ok(pid) = (unsafe { session2.GetProcessId() }) else {
                    continue;
                };
                if pid == 0 {
                    continue;
                }
                if process_tree::pid_overlaps_our_process_tree(&process_entries, pid, self_pid) {
                    excluded_pids.insert(pid);
                    continue;
                }
                let root_pid = resolve_audio_root_pid(pid);
                if root_pid == 0 {
                    continue;
                }
                if process_tree::pid_overlaps_our_process_tree(&process_entries, root_pid, self_pid)
                {
                    excluded_pids.insert(root_pid);
                    continue;
                }
                pids.insert(root_pid);
            }
        }
        let capture_pids = pids.into_iter().collect::<Vec<_>>();
        let exclude_pids = excluded_pids.into_iter().collect::<Vec<_>>();
        Ok(
            process_tree::deduplicate_capture_roots(&process_entries, &capture_pids, &exclude_pids)
                .into_iter()
                .take(MAX_SESSION_MIXER_CAPTURES)
                .collect(),
        )
    }

    fn drain_packets_into(
        capture: &IAudioCaptureClient,
        sample_format: CaptureSampleFormat,
        packets: &mut Vec<CapturedPacket>,
    ) -> std::result::Result<(), String> {
        loop {
            let packet_size = unsafe {
                capture.GetNextPacketSize().map_err(|err| {
                    format!("GetNextPacketSize failed: hr=0x{:08x}", err.code().0 as u32)
                })?
            };
            if packet_size == 0 {
                return Ok(());
            }
            let mut data: *mut u8 = std::ptr::null_mut();
            let mut frames: u32 = 0;
            let mut flags: u32 = 0;
            let mut device_pos: u64 = 0;
            let mut qpc_pos: u64 = 0;
            unsafe {
                capture
                    .GetBuffer(
                        &mut data,
                        &mut frames,
                        &mut flags,
                        Some(&mut device_pos),
                        Some(&mut qpc_pos),
                    )
                    .map_err(|err| format!("GetBuffer failed: hr=0x{:08x}", err.code().0 as u32))?;
            }
            let release_result = (|| -> std::result::Result<(), String> {
                if frames == 0 {
                    return Ok(());
                }
                let sample_count = audio_contract::sample_count_for_frames(frames)
                    .ok_or_else(|| format!("frame count overflows sample buffer: {frames}"))?;
                let mut samples = Vec::<f32>::with_capacity(sample_count);
                append_capture_samples(&mut samples, data, sample_count, flags, sample_format);
                let timestamp_us = if (flags & AUDCLNT_BUFFERFLAGS_TIMESTAMP_ERROR.0 as u32) != 0 {
                    current_performance_timestamp_us()
                } else {
                    audio_contract::qpc_100ns_to_timestamp_us(qpc_pos)
                };
                packets.push(CapturedPacket {
                    timestamp_us,
                    samples,
                });
                Ok(())
            })();
            let release_hr = unsafe { capture.ReleaseBuffer(frames) };
            if let Err(err) = release_hr {
                return Err(format!(
                    "ReleaseBuffer failed: hr=0x{:08x}",
                    err.code().0 as u32
                ));
            }
            release_result?;
        }
    }

    fn emit_mixed_packets(
        packets: Vec<CapturedPacket>,
        callbacks: &Callbacks,
    ) -> Vec<CapturedPacket> {
        let mut mixed = session_mixer::mix_packets(
            packets,
            audio_contract::TARGET_CHANNELS,
            audio_contract::TARGET_SAMPLE_RATE,
            MAX_SESSION_MIXER_EMIT_FRAMES,
        );
        for packet in mixed.drain(..) {
            callbacks.emit_frame(FramePayload {
                samples: FrameSamples::Owned(packet.samples),
                timestamp_us: packet.timestamp_us,
            });
        }
        mixed
    }

    fn pump_capture_loop(
        capture: &IAudioCaptureClient,
        sample_format: CaptureSampleFormat,
        pcm_pool: &PcmFramePool,
        buffer_event: HANDLE,
        stop_event: HANDLE,
        stop_signal: &AtomicBool,
        callbacks: &Callbacks,
    ) -> std::result::Result<(), String> {
        while !stop_signal.load(Ordering::SeqCst) {
            let handles = [buffer_event, stop_event];
            let wait = unsafe { WaitForMultipleObjects(&handles, false, INFINITE) };
            if wait.0 == u32::MAX {
                return Err("WaitForMultipleObjects failed in capture loop".to_string());
            }
            if wait.0 == 1 {
                break;
            }
            if wait.0 != 0 {
                continue;
            }
            drain_packets(capture, sample_format, pcm_pool, callbacks)?;
        }
        Ok(())
    }

    fn drain_packets(
        capture: &IAudioCaptureClient,
        sample_format: CaptureSampleFormat,
        pcm_pool: &PcmFramePool,
        callbacks: &Callbacks,
    ) -> std::result::Result<(), String> {
        loop {
            let packet_size = unsafe {
                capture.GetNextPacketSize().map_err(|err| {
                    format!("GetNextPacketSize failed: hr=0x{:08x}", err.code().0 as u32)
                })?
            };
            if packet_size == 0 {
                return Ok(());
            }
            let mut data: *mut u8 = std::ptr::null_mut();
            let mut frames: u32 = 0;
            let mut flags: u32 = 0;
            let mut device_pos: u64 = 0;
            let mut qpc_pos: u64 = 0;
            unsafe {
                capture
                    .GetBuffer(
                        &mut data,
                        &mut frames,
                        &mut flags,
                        Some(&mut device_pos),
                        Some(&mut qpc_pos),
                    )
                    .map_err(|err| format!("GetBuffer failed: hr=0x{:08x}", err.code().0 as u32))?;
            }
            let release_result = (|| -> std::result::Result<(), String> {
                if frames == 0 {
                    return Ok(());
                }
                let sample_count = audio_contract::sample_count_for_frames(frames)
                    .ok_or_else(|| format!("frame count overflows sample buffer: {frames}"))?;
                let samples =
                    capture_samples_payload(pcm_pool, data, sample_count, flags, sample_format);
                let timestamp_us = if (flags & AUDCLNT_BUFFERFLAGS_TIMESTAMP_ERROR.0 as u32) != 0 {
                    current_performance_timestamp_us()
                } else {
                    audio_contract::qpc_100ns_to_timestamp_us(qpc_pos)
                };
                callbacks.emit_frame(FramePayload {
                    samples,
                    timestamp_us,
                });
                Ok(())
            })();

            let release_hr = unsafe { capture.ReleaseBuffer(frames) };
            if let Err(err) = release_hr {
                return Err(format!(
                    "ReleaseBuffer failed: hr=0x{:08x}",
                    err.code().0 as u32
                ));
            }
            release_result?;
        }
    }

    fn capture_samples_payload(
        pcm_pool: &PcmFramePool,
        data: *mut u8,
        sample_count: usize,
        flags: u32,
        sample_format: CaptureSampleFormat,
    ) -> FrameSamples {
        assert!(sample_count > 0);
        if sample_count <= pcm_pool.samples_per_slot() as usize
            && let Some(mut slot) = pcm_pool.try_acquire()
        {
            let written =
                write_capture_samples_to_slot(&mut slot, data, sample_count, flags, sample_format);
            if written.is_ok() {
                assert_eq!(slot.filled_len(), sample_count);
                return FrameSamples::Pooled(slot);
            }
        }
        let mut samples = Vec::<f32>::with_capacity(sample_count);
        append_capture_samples(&mut samples, data, sample_count, flags, sample_format);
        assert_eq!(samples.len(), sample_count);
        FrameSamples::Owned(samples)
    }

    fn write_capture_samples_to_slot(
        slot: &mut PooledPcmFrame,
        data: *mut u8,
        sample_count: usize,
        flags: u32,
        sample_format: CaptureSampleFormat,
    ) -> std::result::Result<(), PcmPoolError> {
        assert!(sample_count > 0);
        assert!(sample_count <= slot.capacity());
        let silent = (flags & AUDCLNT_BUFFERFLAGS_SILENT.0 as u32) != 0 || data.is_null();
        if silent {
            return slot.write_with(sample_count, |out| out.fill(0.0));
        }
        match sample_format {
            CaptureSampleFormat::Float32 => {
                let samples =
                    unsafe { std::slice::from_raw_parts(data as *const f32, sample_count) };
                slot.write(samples)
            }
            CaptureSampleFormat::Pcm16 => {
                let pcm = unsafe { std::slice::from_raw_parts(data as *const i16, sample_count) };
                slot.write_with(sample_count, |out| {
                    for (out_sample, pcm_sample) in out.iter_mut().zip(pcm.iter().copied()) {
                        *out_sample = audio_contract::pcm16_to_float32(pcm_sample);
                    }
                })
            }
        }
    }

    fn append_capture_samples(
        out: &mut Vec<f32>,
        data: *mut u8,
        sample_count: usize,
        flags: u32,
        sample_format: CaptureSampleFormat,
    ) {
        let silent = (flags & AUDCLNT_BUFFERFLAGS_SILENT.0 as u32) != 0 || data.is_null();
        if silent {
            out.resize(sample_count, 0.0);
            return;
        }
        match sample_format {
            CaptureSampleFormat::Float32 => {
                let samples =
                    unsafe { std::slice::from_raw_parts(data as *const f32, sample_count) };
                out.extend_from_slice(samples);
            }
            CaptureSampleFormat::Pcm16 => {
                let pcm = unsafe { std::slice::from_raw_parts(data as *const i16, sample_count) };
                out.extend(pcm.iter().copied().map(audio_contract::pcm16_to_float32));
            }
        }
    }

    fn current_performance_timestamp_us() -> i64 {
        use windows::Win32::System::Performance::{
            QueryPerformanceCounter, QueryPerformanceFrequency,
        };
        let mut counter: i64 = 0;
        let mut frequency: i64 = 0;
        unsafe {
            if QueryPerformanceCounter(&mut counter).is_err()
                || QueryPerformanceFrequency(&mut frequency).is_err()
                || frequency <= 0
            {
                return 0;
            }
        }
        let micros = (i128::from(counter) * 1_000_000) / i128::from(frequency);
        micros.min(i128::from(i64::MAX)) as i64
    }

    struct ComApartment;

    impl ComApartment {
        fn initialize() -> Result<Self> {
            unsafe {
                CoInitializeEx(None, COINIT_MULTITHREADED)
                    .ok()
                    .map_err(|err| {
                        Error::new(
                            Status::GenericFailure,
                            format!("CoInitializeEx failed: hr=0x{:08x}", err.code().0 as u32),
                        )
                    })?;
            }
            Ok(Self)
        }
    }

    impl Drop for ComApartment {
        fn drop(&mut self) {
            unsafe {
                CoUninitialize();
            }
        }
    }

    pub(crate) fn detected_build() -> Option<u32> {
        detected_kernel32_build()
    }

    pub(crate) fn process_loopback_probe() -> ProcessLoopbackRuntimeProbe {
        static PROBE: OnceLock<ProcessLoopbackRuntimeProbe> = OnceLock::new();
        PROBE.get_or_init(run_process_loopback_probe).clone()
    }

    fn run_process_loopback_probe() -> ProcessLoopbackRuntimeProbe {
        let (tx, rx) = std::sync::mpsc::channel();
        let _ = thread::Builder::new()
            .name("fluxer-win-process-loopback-probe".to_owned())
            .spawn(move || {
                let include_result = probe_process_loopback_activation(true);
                let exclude_result = probe_process_loopback_activation(false);
                let _ = tx.send((include_result, exclude_result));
            });

        match rx.recv_timeout(std::time::Duration::from_millis(
            u64::from(PROCESS_LOOPBACK_PROBE_TIMEOUT_MS) * PROCESS_LOOPBACK_PROBE_COUNT + 500,
        )) {
            Ok((include_result, exclude_result)) => ProcessLoopbackRuntimeProbe {
                include_supported: include_result.is_ok(),
                include_detail: Some(match include_result {
                    Ok(()) => "include-mode process loopback activation probe succeeded".to_owned(),
                    Err(err) => err.reason.to_string(),
                }),
                exclude_supported: exclude_result.is_ok(),
                exclude_detail: Some(match exclude_result {
                    Ok(()) => "exclude-mode process loopback activation probe succeeded".to_owned(),
                    Err(err) => err.reason.to_string(),
                }),
            },
            Err(_) => ProcessLoopbackRuntimeProbe {
                include_supported: false,
                include_detail: Some(
                    "process loopback activation probe worker timed out".to_owned(),
                ),
                exclude_supported: false,
                exclude_detail: Some(
                    "process loopback activation probe worker timed out".to_owned(),
                ),
            },
        }
    }

    fn probe_process_loopback_activation(include_tree: bool) -> std::result::Result<(), Error> {
        let _com = ComApartment::initialize()?;
        let stop_event = unsafe { CreateEventW(None, true, false, PCWSTR::null()) }
            .map_err(|err: windows::core::Error| hresult_error("CreateEventW", err.code().0))?;
        let stop_event = OwnedHandle::new(stop_event)
            .ok_or_else(|| generic("CreateEventW returned invalid probe stop handle"))?;
        let current_pid = unsafe { GetCurrentProcessId() };

        let client = activate_process_loopback_client_with_timeout(
            current_pid,
            include_tree,
            stop_event.raw(),
            PROCESS_LOOPBACK_PROBE_TIMEOUT_MS,
        )?;
        drop(client);

        Ok(())
    }

    fn detected_kernel32_build() -> Option<u32> {
        let path = PCWSTR(KERNEL32_PATH_UTF16.as_ptr());
        let size = unsafe { GetFileVersionInfoSizeW(path, None) };
        if size == 0 {
            return None;
        }
        let mut buffer = vec![0u8; size as usize];
        let ok = unsafe {
            GetFileVersionInfoW(path, Some(0), size, buffer.as_mut_ptr() as *mut c_void).is_ok()
        };
        if !ok {
            return None;
        }
        let mut info_ptr: *mut c_void = std::ptr::null_mut();
        let mut info_len: u32 = 0;
        let queried = unsafe {
            VerQueryValueW(
                buffer.as_ptr() as *const c_void,
                PCWSTR(BACKSLASH_UTF16.as_ptr()),
                &mut info_ptr,
                &mut info_len,
            )
            .as_bool()
        };
        if !queried || info_ptr.is_null() {
            return None;
        }
        let fixed: &VS_FIXEDFILEINFO = unsafe { &*(info_ptr as *const VS_FIXEDFILEINFO) };
        let build = (fixed.dwFileVersionLS >> 16) & 0xffff;
        Some(build)
    }

    pub(crate) fn is_supported() -> bool {
        let probe = process_loopback_probe();
        probe.include_supported || probe.exclude_supported
    }

    pub(crate) fn pid_from_hwnd(hwnd: u64) -> u32 {
        if hwnd == 0 {
            return 0;
        }
        let mut pid: u32 = 0;
        let tid = unsafe { GetWindowThreadProcessId(HWND(hwnd as *mut c_void), Some(&mut pid)) };
        if tid == 0 { 0 } else { pid }
    }

    pub(crate) fn resolve_audio_root_pid(pid: u32) -> u32 {
        resolve_audio_root_pid_impl(pid).unwrap_or(pid)
    }

    struct ResolveEntry {
        pid: u32,
        parent: u32,
        exe: String,
    }

    fn resolve_audio_root_pid_impl(start_pid: u32) -> Option<u32> {
        let entries = snapshot_processes_with_exe()?;
        let self_pid = unsafe { GetCurrentProcessId() };
        let mut current = start_pid;
        let mut last_browserlike: u32 = if start_pid == self_pid { 0 } else { start_pid };
        for _ in 0..16 {
            let Some(entry) = entries.iter().find(|e| e.pid == current) else {
                break;
            };
            if current != self_pid && is_browser_like(&entry.exe) {
                last_browserlike = current;
            }
            if entry.exe == "explorer.exe" || entry.parent == 0 || entry.parent == current {
                break;
            }
            current = entry.parent;
        }
        if last_browserlike == 0 || last_browserlike == self_pid {
            return Some(start_pid);
        }

        let mut self_walk = self_pid;
        for _ in 0..32 {
            let Some(entry) = entries.iter().find(|e| e.pid == self_walk) else {
                break;
            };
            if self_walk == last_browserlike {
                return Some(start_pid);
            }
            if entry.parent == 0 || entry.parent == self_walk {
                break;
            }
            self_walk = entry.parent;
        }
        Some(last_browserlike)
    }

    fn is_browser_like(exe: &str) -> bool {
        matches!(
            exe,
            "chrome.exe" | "msedge.exe" | "firefox.exe" | "electron.exe"
        )
    }

    fn snapshot_processes_with_exe() -> Option<Vec<ResolveEntry>> {
        let snapshot = unsafe { CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0) }.ok()?;
        let snapshot = OwnedHandle::new(snapshot)?;
        let mut pe: PROCESSENTRY32W = unsafe { zeroed() };
        pe.dwSize = size_of::<PROCESSENTRY32W>() as u32;
        if unsafe { Process32FirstW(snapshot.raw(), &mut pe) }.is_err() {
            return None;
        }
        let mut entries = Vec::new();
        loop {
            let exe = wide_until_nul(&pe.szExeFile)
                .iter()
                .map(|c| char::from_u32(u32::from(*c)).unwrap_or('?'))
                .collect::<String>()
                .to_ascii_lowercase();
            entries.push(ResolveEntry {
                pid: pe.th32ProcessID,
                parent: pe.th32ParentProcessID,
                exe,
            });
            if unsafe { Process32NextW(snapshot.raw(), &mut pe) }.is_err() {
                break;
            }
        }
        Some(entries)
    }

    fn snapshot_process_entries() -> Option<Vec<process_tree::ProcessEntry>> {
        let snapshot = unsafe { CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0) }.ok()?;
        let snapshot = OwnedHandle::new(snapshot)?;
        let mut pe: PROCESSENTRY32W = unsafe { zeroed() };
        pe.dwSize = size_of::<PROCESSENTRY32W>() as u32;
        if unsafe { Process32FirstW(snapshot.raw(), &mut pe) }.is_err() {
            return None;
        }
        let mut entries = Vec::new();
        loop {
            entries.push(process_tree::ProcessEntry {
                pid: pe.th32ProcessID,
                parent: pe.th32ParentProcessID,
            });
            if unsafe { Process32NextW(snapshot.raw(), &mut pe) }.is_err() {
                break;
            }
        }
        Some(entries)
    }

    fn wide_until_nul(buf: &[u16]) -> &[u16] {
        match buf.iter().position(|&c| c == 0) {
            Some(idx) => &buf[..idx],
            None => buf,
        }
    }

    fn is_self_or_descendant_pid(target: u32) -> bool {
        let self_pid = unsafe { GetCurrentProcessId() };
        if target == 0 {
            return false;
        }
        if target == self_pid {
            return true;
        }
        snapshot_process_entries()
            .map(|entries| process_tree::pid_is_our_descendant(&entries, target, self_pid))
            .unwrap_or(false)
    }

    #[allow(dead_code)]
    fn _touch_lasterror() -> u32 {
        unsafe { GetLastError().0 }
    }

    #[cfg(test)]
    mod blob_propvariant_tests {
        use super::*;

        #[test]
        fn blob_propvariant_round_trips_vt_size_and_pointer() {
            let mut buffer = [0u8; 32];
            let ptr = buffer.as_mut_ptr();
            let size = buffer.len() as u32;

            let prop = ManuallyDrop::new(make_blob_propvariant(ptr, size));
            unsafe {
                let inner = &prop.Anonymous.Anonymous;
                assert_eq!(inner.vt, VT_BLOB, "vt tag must be VT_BLOB");
                assert_eq!(inner.wReserved1, 0);
                assert_eq!(inner.wReserved2, 0);
                assert_eq!(inner.wReserved3, 0);
                let blob = inner.Anonymous.blob;
                assert_eq!(blob.cbSize, size, "BLOB cbSize must match input");
                assert_eq!(blob.pBlobData, ptr, "BLOB pBlobData must match input");
            }
        }

        #[test]
        fn propvariant_size_matches_sdk() {
            let expected = if cfg!(target_pointer_width = "64") {
                24
            } else {
                16
            };
            assert_eq!(size_of::<PROPVARIANT>(), expected);
        }
    }
}

#[cfg(not(target_os = "windows"))]
mod platform {
    use super::{Callbacks, LoopbackOptions, ProcessLoopbackRuntimeProbe};
    use napi::bindgen_prelude::{Error, Result, Status};

    pub(crate) struct Inner;

    impl Inner {
        pub(crate) fn new(
            _pid: u32,
            _options: LoopbackOptions,
            _callbacks: Callbacks,
        ) -> Result<Self> {
            Err(Error::new(
                Status::GenericFailure,
                "win-process-loopback only supports Windows",
            ))
        }

        pub(crate) fn set_screen_audio_sink(
            &self,
            _sink: std::sync::Arc<super::NativeScreenFrameSinkHandleRef>,
        ) {
        }

        pub(crate) fn clear_screen_audio_sink(&self) {}

        pub(crate) fn start(&self) -> Result<()> {
            Err(Error::new(
                Status::GenericFailure,
                "win-process-loopback only supports Windows",
            ))
        }

        pub(crate) fn stop(&self) {}

        pub(crate) fn dispose(&self) {}
    }

    pub(crate) fn is_supported() -> bool {
        false
    }

    pub(crate) fn process_loopback_probe() -> ProcessLoopbackRuntimeProbe {
        ProcessLoopbackRuntimeProbe {
            include_supported: false,
            include_detail: Some(
                "process loopback activation probe is only available on Windows".to_owned(),
            ),
            exclude_supported: false,
            exclude_detail: Some(
                "process loopback activation probe is only available on Windows".to_owned(),
            ),
        }
    }

    pub(crate) fn detected_build() -> Option<u32> {
        None
    }

    pub(crate) fn pid_from_hwnd(_hwnd: u64) -> u32 {
        0
    }

    pub(crate) fn resolve_audio_root_pid(pid: u32) -> u32 {
        pid
    }
}
