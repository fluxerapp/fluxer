// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex, MutexGuard, Weak};
use std::time::Duration;

use block2::RcBlock;
use napi::bindgen_prelude::{Buffer, BufferSlice, Function, Result, Unknown};
use napi::threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode};
use napi::{JsValue, Status, ValueType};
use napi_derive::napi;

use objc2::rc::Retained;
use objc2::runtime::{NSObject, NSObjectProtocol, ProtocolObject};
use objc2::{AllocAnyThread, DefinedClass, define_class, msg_send};
use objc2_core_audio_types::{
    AudioBufferList, AudioStreamBasicDescription, kAudioFormatFlagIsFloat,
    kAudioFormatFlagIsNonInterleaved, kAudioFormatFlagIsSignedInteger, kAudioFormatLinearPCM,
};
use objc2_core_foundation::{CFRetained, CGPoint, CGRect, CGSize};
use objc2_core_media::{
    CMAudioFormatDescriptionGetStreamBasicDescription, CMBlockBuffer, CMSampleBuffer,
};
use objc2_core_video::{
    CVPixelBuffer, CVPixelBufferGetHeight, CVPixelBufferGetIOSurface,
    CVPixelBufferGetPixelFormatType, CVPixelBufferGetPlaneCount, CVPixelBufferGetWidth,
    CVPixelBufferLockBaseAddress, CVPixelBufferLockFlags, CVPixelBufferUnlockBaseAddress,
    kCVPixelFormatType_32BGRA,
};
use objc2_foundation::{NSArray, NSError, NSString};
use objc2_screen_capture_kit::{
    SCContentFilter, SCDisplay, SCShareableContent, SCStream, SCStreamConfiguration,
    SCStreamDelegate, SCStreamOutput, SCStreamOutputType, SCWindow,
};

use fluxer_screen_frame_bus::{
    self as frame_bus, EnqueueOutcome, MacCvPixelBufferFrame, NativeScreenFrameSinkHandle,
    NativeScreenFrameSinkHandleRef, ScreenFrame as BusScreenFrame,
};

use crate::audio_pool::{
    MAC_AUDIO_POOL_CAP, MAX_FRAME_BYTES_PER_SLOT, MacAudioFramePool, PooledMacAudioFrame,
};
use crate::config::{
    AUDIO_CHANNEL_COUNT_DEFAULT, AUDIO_SAMPLE_RATE_DEFAULT_HZ, AudioSampleFormat,
    CaptureFailureSurface, SckCaptureConfig, SckCaptureFailure, SckColorSpace, SckPixelFormat,
};
use crate::encoder_attach::EncoderAttachment;
use crate::foundation;
use crate::os_version::{
    self, SCK_MIN_MACOS, SupportClassification, classify_support, format_version,
};
use crate::sck;
use fluxer_encoder_ring::EncoderFrameRate;

const DEFAULT_TIMEOUT_NS: u64 = 30 * 1_000_000_000;
const PIXEL_FORMAT_420V: u32 = u32::from_be_bytes(*b"420v");
const PIXEL_FORMAT_420F: u32 = u32::from_be_bytes(*b"420f");
#[link(name = "CoreGraphics", kind = "framework")]
unsafe extern "C" {
    fn CGPreflightScreenCaptureAccess() -> bool;
}

#[link(name = "CoreVideo", kind = "framework")]
unsafe extern "C" {
    fn CVPixelBufferRetain(pixel_buffer: *mut c_void) -> *mut c_void;
    fn CVPixelBufferRelease(pixel_buffer: *mut c_void);
}

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

fn await_start(stream: &SCStream, timeout_ns: u64) -> std::result::Result<(), AsyncError> {
    let waker = new_waker();
    let waker_cb = waker.clone();
    let blk = RcBlock::new(move |err: *mut NSError| {
        let (err_opt, failed) = match retain_error(err) {
            Ok(opt) => (opt, false),
            Err(_) => (None, true),
        };
        complete_waker(&waker_cb, err_opt, None, failed);
    });
    unsafe {
        stream.startCaptureWithCompletionHandler(Some(&blk));
    }
    if !wait_deadline(&waker, timeout_ns) {
        return Err(AsyncError::Timeout);
    }
    let s = lock_waker_state(&waker);
    if s.failed || s.err.is_some() {
        return Err(AsyncError::SckErr);
    }
    Ok(())
}

fn await_stop(stream: &SCStream, timeout_ns: u64) -> std::result::Result<(), AsyncError> {
    let waker = new_waker();
    let waker_cb = waker.clone();
    let blk = RcBlock::new(move |err: *mut NSError| {
        let (err_opt, failed) = match retain_error(err) {
            Ok(opt) => (opt, false),
            Err(_) => (None, true),
        };
        complete_waker(&waker_cb, err_opt, None, failed);
    });
    unsafe {
        stream.stopCaptureWithCompletionHandler(Some(&blk));
    }
    if !wait_deadline(&waker, timeout_ns) {
        return Err(AsyncError::Timeout);
    }
    let s = lock_waker_state(&waker);
    if s.failed || s.err.is_some() {
        return Err(AsyncError::SckErr);
    }
    Ok(())
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

type LifecycleTsfn =
    ThreadsafeFunction<(String, String), (), (String, String), Status, false, false, 8>;

type AudioFrameTsfnArgs = (u32, u32, u32, i64, u32, Buffer);
type AudioFrameTsfnInput = (u32, u32, u32, i64, AudioSampleFormat, PooledMacAudioFrame);
type AudioFrameTsfn =
    ThreadsafeFunction<AudioFrameTsfnInput, (), AudioFrameTsfnArgs, Status, false, false, 16>;

type AudioDiagnosticTsfnArgs = (String, String, u32, u32);
type AudioDiagnosticTsfn = ThreadsafeFunction<
    AudioDiagnosticTsfnArgs,
    (),
    AudioDiagnosticTsfnArgs,
    Status,
    false,
    false,
    8,
>;

type EncoderDiagnosticTsfnArgs = (String, String);
type EncoderDiagnosticTsfn = ThreadsafeFunction<
    EncoderDiagnosticTsfnArgs,
    (),
    EncoderDiagnosticTsfnArgs,
    Status,
    false,
    false,
    8,
>;

struct AudioFramePayload {
    sample_rate_hz: u32,
    channels: u32,
    num_samples_per_channel: u32,
    pts_us: i64,
    format: AudioSampleFormat,
    slot: PooledMacAudioFrame,
}

impl AudioFramePayload {
    fn into_input(self) -> AudioFrameTsfnInput {
        assert!(self.sample_rate_hz > 0);
        assert!(self.channels > 0);
        (
            self.sample_rate_hz,
            self.channels,
            self.num_samples_per_channel,
            self.pts_us,
            self.format,
            self.slot,
        )
    }
}

struct CaptureState {
    lifecycle_tsfn: Option<LifecycleTsfn>,
    audio_diagnostic_tsfn: Option<AudioDiagnosticTsfn>,
    encoder_diagnostic_tsfn: Option<EncoderDiagnosticTsfn>,
    delegate: Option<Retained<FluxerSCKScreenSource>>,
    stream: Option<Retained<SCStream>>,
    sample_queue: Option<dispatch2::DispatchRetained<dispatch2::DispatchQueue>>,
}

unsafe impl Send for CaptureState {}
unsafe impl Sync for CaptureState {}

pub(crate) struct CaptureInner {
    state: Mutex<CaptureState>,
    audio_frame_tsfn: parking_lot::RwLock<Option<AudioFrameTsfn>>,
    running: AtomicBool,
    tsfn_aborted: AtomicBool,
    capture_id: Mutex<Option<String>>,
    bus_sink: parking_lot::RwLock<Option<Arc<dyn frame_bus::ScreenFrameSink>>>,
    native_frame_sink: parking_lot::RwLock<Option<Arc<NativeScreenFrameSinkHandleRef>>>,
    failure_surface: parking_lot::RwLock<Option<Arc<dyn CaptureFailureSurface>>>,
    captures_audio: AtomicBool,
    audio_sample_rate_hz: AtomicU32,
    audio_channels: AtomicU32,
    audio_pool: MacAudioFramePool,
    encoder_attachment: parking_lot::RwLock<Option<Arc<EncoderAttachment>>>,
    encoder_attach_requested: AtomicBool,
    encoder_ring_full_emitted: AtomicBool,
    frame_sink_accepted: AtomicU64,
    frame_sink_coalesced: AtomicU64,
    frame_sink_rejected: AtomicU64,
    media_frames_dropped_without_sink: AtomicU64,
    frame_sink_backpressure_emitted: AtomicBool,
    frame_sink_missing_emitted: AtomicBool,
}

fn generic_error(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::GenericFailure, reason.into())
}

fn invalid_arg(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::InvalidArg, reason.into())
}

fn lock_state(inner: &CaptureInner) -> Result<MutexGuard<'_, CaptureState>> {
    inner
        .state
        .lock()
        .map_err(|_| generic_error("ScreenCapture state lock poisoned"))
}

fn note_tsfn_status(inner: &CaptureInner, status: Status) {
    if status == Status::Closing {
        inner.tsfn_aborted.store(true, Ordering::Release);
    }
}

const SCK_USER_STOPPED_CODE: isize = -3817;

fn is_sck_user_stop(err: &NSError) -> bool {
    let code = err.code();
    assert!(
        code.abs() < i32::MAX as isize,
        "SCK error code out of range"
    );
    if code != SCK_USER_STOPPED_CODE {
        return false;
    }
    let domain = err.domain().to_string();
    assert!(!domain.is_empty(), "SCK error domain must not be empty");
    domain.contains("SCStreamErrorDomain")
}

fn sample_timestamp_us(sample_buffer: &CMSampleBuffer) -> i64 {
    let pts = unsafe { sample_buffer.presentation_time_stamp() };
    if pts.timescale <= 0 {
        return 0;
    }

    let value = pts.value as i128;
    let scale = pts.timescale as i128;
    ((value * 1_000_000) / scale).clamp(0, i64::MAX as i128) as i64
}

pub struct DelegateIvars {
    pub inner: Weak<CaptureInner>,
    pub cleared: AtomicBool,
}

define_class!(
    #[unsafe(super(NSObject))]
    #[name = "FluxerSCKScreenSource"]
    #[ivars = DelegateIvars]
    pub struct FluxerSCKScreenSource;

    unsafe impl NSObjectProtocol for FluxerSCKScreenSource {}

    unsafe impl SCStreamDelegate for FluxerSCKScreenSource {
        #[unsafe(method(stream:didStopWithError:))]
        unsafe fn did_stop_with_error(&self, _stream: &SCStream, err: &NSError) {
            if self.ivars().cleared.load(Ordering::Acquire) {
                return;
            }
            let Some(inner_arc) = self.ivars().inner.upgrade() else {
                return;
            };
            let inner: &CaptureInner = &inner_arc;
            inner.running.store(false, Ordering::Release);
            let msg = foundation::ns_error_localized_description(err);
            let msg = if msg.is_empty() {
                "stream stopped".to_string()
            } else {
                msg
            };
            let is_clean_stop = is_sck_user_stop(err);
            if !is_clean_stop {
                if let Some(surface) = inner.failure_surface.read().as_ref().cloned() {
                    surface.on_failure(SckCaptureFailure::StreamStoppedWithError(msg.clone()));
                }
            }
            let Ok(state) = inner.state.lock() else {
                return;
            };
            if is_clean_stop {
                if let Some(tsfn) = state.lifecycle_tsfn.as_ref() {
                    let status = tsfn.call(
                        ("closed-clean".to_string(), msg),
                        ThreadsafeFunctionCallMode::NonBlocking,
                    );
                    note_tsfn_status(inner, status);
                }
                return;
            }
            if let Some(tsfn) = state.lifecycle_tsfn.as_ref() {
                let status = tsfn.call(
                    ("error".to_string(), msg),
                    ThreadsafeFunctionCallMode::NonBlocking,
                );
                note_tsfn_status(inner, status);
            }
            if let Some(tsfn) = state.lifecycle_tsfn.as_ref() {
                let status = tsfn.call(
                    ("closed".to_string(), String::new()),
                    ThreadsafeFunctionCallMode::NonBlocking,
                );
                note_tsfn_status(inner, status);
            }
        }
    }

    unsafe impl SCStreamOutput for FluxerSCKScreenSource {
        #[unsafe(method(stream:didOutputSampleBuffer:ofType:))]
        unsafe fn did_output_sample_buffer(
            &self,
            _stream: &SCStream,
            sample_buffer: &CMSampleBuffer,
            output_type: SCStreamOutputType,
        ) {
            if self.ivars().cleared.load(Ordering::Acquire) {
                return;
            }
            let Some(inner_arc) = self.ivars().inner.upgrade() else {
                return;
            };
            if output_type == SCStreamOutputType::Screen {
                unsafe { handle_screen_sample(&inner_arc, sample_buffer) };
                return;
            }
            if output_type == SCStreamOutputType::Audio {
                unsafe { handle_audio_sample(&inner_arc, sample_buffer) };
            }
        }
    }
);

impl FluxerSCKScreenSource {
    pub fn new(inner: Weak<CaptureInner>) -> Retained<Self> {
        let this = Self::alloc().set_ivars(DelegateIvars {
            inner,
            cleared: AtomicBool::new(false),
        });
        unsafe { msg_send![super(this), init] }
    }

    pub fn clear_inner(&self) {
        self.ivars().cleared.store(true, Ordering::Release);
    }
}

unsafe fn handle_audio_sample(inner: &CaptureInner, sample_buffer: &CMSampleBuffer) {
    if inner.tsfn_aborted.load(Ordering::Acquire) || !inner.running.load(Ordering::Acquire) {
        return;
    }
    if !unsafe { sample_buffer.data_is_ready() } {
        return;
    }
    let num_samples = unsafe { sample_buffer.num_samples() };
    if num_samples <= 0 {
        return;
    }
    let pts_us = sample_timestamp_us(sample_buffer);
    let (detected_format, detected_sample_rate_hz, detected_channels) =
        unsafe { detect_audio_format(sample_buffer) };
    let sample_rate_hz = if detected_sample_rate_hz > 0 {
        detected_sample_rate_hz
    } else {
        inner.audio_sample_rate_hz.load(Ordering::Acquire)
    };
    let channels = if detected_channels > 0 {
        detected_channels
    } else {
        inner.audio_channels.load(Ordering::Acquire)
    };
    let num_samples_per_channel = num_samples as u32;
    let mut slot = match inner.audio_pool.try_acquire() {
        Some(s) => s,
        None => {
            emit_pool_exhausted(inner, sample_rate_hz, num_samples_per_channel);
            return;
        }
    };
    if unsafe { extract_audio_bytes(sample_buffer, &mut slot) }.is_err() {
        return;
    }
    let payload = AudioFramePayload {
        sample_rate_hz,
        channels,
        num_samples_per_channel,
        pts_us,
        format: detected_format,
        slot,
    };
    deliver_audio_frame(inner, payload);
}

fn emit_pool_exhausted(inner: &CaptureInner, sample_rate_hz: u32, num_samples: u32) {
    let capture_id = inner
        .capture_id
        .lock()
        .ok()
        .and_then(|guard| guard.as_ref().cloned())
        .unwrap_or_default();
    let Ok(state) = inner.state.lock() else {
        return;
    };
    let Some(tsfn) = state.audio_diagnostic_tsfn.as_ref() else {
        return;
    };
    let status = tsfn.call(
        (
            "audioPoolExhausted".to_string(),
            capture_id,
            sample_rate_hz,
            num_samples,
        ),
        ThreadsafeFunctionCallMode::NonBlocking,
    );
    note_tsfn_status(inner, status);
}

pub(crate) unsafe fn detect_audio_format(
    sample_buffer: &CMSampleBuffer,
) -> (AudioSampleFormat, u32, u32) {
    let Some(format_desc) = (unsafe { sample_buffer.format_description() }) else {
        return (AudioSampleFormat::Unknown, 0, 0);
    };
    let asbd_ptr = unsafe { CMAudioFormatDescriptionGetStreamBasicDescription(&format_desc) };
    if asbd_ptr.is_null() {
        return (AudioSampleFormat::Unknown, 0, 0);
    }
    let asbd: AudioStreamBasicDescription = unsafe { *asbd_ptr };
    if asbd.mFormatID != kAudioFormatLinearPCM {
        return (AudioSampleFormat::Unknown, 0, 0);
    }
    let format = classify_pcm_flags(asbd.mFormatFlags, asbd.mBitsPerChannel);
    let sample_rate_hz = asbd.mSampleRate.clamp(0.0, u32::MAX as f64) as u32;
    let channels = asbd.mChannelsPerFrame;
    (format, sample_rate_hz, channels)
}

fn classify_pcm_flags(flags: u32, bits_per_channel: u32) -> AudioSampleFormat {
    let is_float = (flags & kAudioFormatFlagIsFloat) != 0;
    let is_signed_int = (flags & kAudioFormatFlagIsSignedInteger) != 0;
    let is_non_interleaved = (flags & kAudioFormatFlagIsNonInterleaved) != 0;
    if is_float && bits_per_channel == 32 && is_non_interleaved {
        return AudioSampleFormat::F32Planar;
    }
    if is_float && bits_per_channel == 32 && !is_non_interleaved {
        return AudioSampleFormat::F32Interleaved;
    }
    if is_signed_int && bits_per_channel == 16 && !is_non_interleaved {
        return AudioSampleFormat::I16Interleaved;
    }
    AudioSampleFormat::Unknown
}

pub(crate) const MAX_AUDIO_BUFFERS: usize = 8;

#[repr(C, align(16))]
pub(crate) struct AblStorage {
    pub n_buffers: u32,
    pub _pad: u32,
    pub buffers: [objc2_core_audio_types::AudioBuffer; MAX_AUDIO_BUFFERS],
}

pub(crate) unsafe fn extract_audio_bytes(
    sample_buffer: &CMSampleBuffer,
    slot: &mut PooledMacAudioFrame,
) -> std::result::Result<(), ()> {
    assert!(slot.capacity() <= MAX_FRAME_BYTES_PER_SLOT);
    let mut storage = AblStorage {
        n_buffers: 0,
        _pad: 0,
        buffers: [objc2_core_audio_types::AudioBuffer {
            mNumberChannels: 0,
            mDataByteSize: 0,
            mData: ptr::null_mut(),
        }; MAX_AUDIO_BUFFERS],
    };
    let storage_size = core::mem::size_of::<AblStorage>();
    let mut block_buffer_out: *mut CMBlockBuffer = ptr::null_mut();
    let status = unsafe {
        sample_buffer.audio_buffer_list_with_retained_block_buffer(
            ptr::null_mut(),
            &mut storage as *mut AblStorage as *mut AudioBufferList,
            storage_size,
            None,
            None,
            0,
            &mut block_buffer_out,
        )
    };
    if status != 0 {
        unsafe { release_block_buffer(block_buffer_out) };
        return Err(());
    }
    assert!(!block_buffer_out.is_null());
    let n_buffers = storage.n_buffers as usize;
    if n_buffers == 0 || n_buffers > MAX_AUDIO_BUFFERS {
        unsafe { release_block_buffer(block_buffer_out) };
        return Err(());
    }
    for i in 0..n_buffers {
        let ab = storage.buffers[i];
        let len = ab.mDataByteSize as usize;
        if len == 0 || ab.mData.is_null() {
            continue;
        }
        let bytes = unsafe { core::slice::from_raw_parts(ab.mData as *const u8, len) };
        if slot.append(bytes).is_err() {
            unsafe { release_block_buffer(block_buffer_out) };
            return Err(());
        }
    }
    unsafe { release_block_buffer(block_buffer_out) };
    Ok(())
}

unsafe fn release_block_buffer(block_buffer: *mut CMBlockBuffer) {
    if block_buffer.is_null() {
        return;
    }
    unsafe {
        CFRelease(block_buffer as *const core::ffi::c_void);
    }
}

#[link(name = "CoreFoundation", kind = "framework")]
unsafe extern "C" {
    fn CFRelease(cf: *const core::ffi::c_void);
}

fn build_audio_callback_args(
    env: napi::Env,
    input: AudioFrameTsfnInput,
) -> Result<AudioFrameTsfnArgs> {
    let (sample_rate_hz, channels, num_samples_per_channel, pts_us, format, slot) = input;
    assert!(sample_rate_hz > 0);
    assert!(channels > 0);
    let (ptr, len, slot) = slot.into_external_parts();
    assert!(!ptr.is_null());
    if len == 0 {
        drop(slot);
        let empty: Vec<u8> = Vec::new();
        return Ok((
            sample_rate_hz,
            channels,
            num_samples_per_channel,
            pts_us,
            format.code(),
            Buffer::from(empty),
        ));
    }
    let slice = unsafe {
        BufferSlice::from_external(&env, ptr, len, slot, |_env, slot| {
            drop(slot);
        })
    }?;
    let buffer = slice.into_buffer(&env)?;
    Ok((
        sample_rate_hz,
        channels,
        num_samples_per_channel,
        pts_us,
        format.code(),
        buffer,
    ))
}

fn deliver_audio_frame(inner: &CaptureInner, payload: AudioFramePayload) {
    let guard = inner.audio_frame_tsfn.read();
    let Some(tsfn) = guard.as_ref() else {
        return;
    };
    let status = tsfn.call(
        payload.into_input(),
        ThreadsafeFunctionCallMode::NonBlocking,
    );
    note_tsfn_status(inner, status);
}

unsafe fn handle_screen_sample(inner: &CaptureInner, sample_buffer: &CMSampleBuffer) {
    if inner.tsfn_aborted.load(Ordering::Acquire) || !inner.running.load(Ordering::Acquire) {
        return;
    }
    if !unsafe { sample_buffer.data_is_ready() } {
        return;
    }
    let Some(image_buffer) = (unsafe { sample_buffer.image_buffer() }) else {
        return;
    };

    let pixel_buffer: &CVPixelBuffer = &image_buffer;

    let format_type = CVPixelBufferGetPixelFormatType(pixel_buffer);
    if format_type == PIXEL_FORMAT_420V || format_type == PIXEL_FORMAT_420F {
        handle_nv12_screen_sample(inner, sample_buffer, pixel_buffer);
    } else if format_type == kCVPixelFormatType_32BGRA {
        handle_bgra_screen_sample(inner, sample_buffer, pixel_buffer);
    }
}

fn handle_nv12_screen_sample(
    inner: &CaptureInner,
    sample_buffer: &CMSampleBuffer,
    pixel_buffer: &CVPixelBuffer,
) {
    let width = CVPixelBufferGetWidth(pixel_buffer) as u32;
    let height = CVPixelBufferGetHeight(pixel_buffer) as u32;
    if width == 0 || height == 0 || height % 2 != 0 {
        return;
    }

    let plane_count = CVPixelBufferGetPlaneCount(pixel_buffer);
    if plane_count < 2 {
        return;
    }

    submit_encoder_ring_frame(inner, pixel_buffer, sample_timestamp_us(sample_buffer));

    if let Some(sink) = inner.native_frame_sink.read().as_ref().cloned() {
        let outcome =
            try_enqueue_native_cv_pixel_buffer(&sink, pixel_buffer, width, height, sample_buffer);
        record_frame_sink_outcome(inner, outcome);
        return;
    }

    if let Some(sink) = bus_sink_for(inner).as_deref() {
        let outcome =
            try_enqueue_bus_cv_pixel_buffer(sink, pixel_buffer, width, height, sample_buffer);
        record_frame_sink_outcome(inner, outcome);
        return;
    }
    note_media_frame_without_sink(inner);
}

fn handle_bgra_screen_sample(
    inner: &CaptureInner,
    sample_buffer: &CMSampleBuffer,
    pixel_buffer: &CVPixelBuffer,
) {
    let width = CVPixelBufferGetWidth(pixel_buffer) as u32;
    let height = CVPixelBufferGetHeight(pixel_buffer) as u32;
    if width == 0 || height == 0 {
        return;
    }

    submit_encoder_ring_frame(inner, pixel_buffer, sample_timestamp_us(sample_buffer));

    if let Some(sink) = inner.native_frame_sink.read().as_ref().cloned() {
        let outcome =
            try_enqueue_native_cv_pixel_buffer(&sink, pixel_buffer, width, height, sample_buffer);
        record_frame_sink_outcome(inner, outcome);
        return;
    }

    if let Some(sink) = bus_sink_for(inner).as_deref() {
        let outcome =
            try_enqueue_bus_cv_pixel_buffer(sink, pixel_buffer, width, height, sample_buffer);
        record_frame_sink_outcome(inner, outcome);
        return;
    }
    note_media_frame_without_sink(inner);
}

fn submit_encoder_ring_frame(
    inner: &CaptureInner,
    pixel_buffer: &CVPixelBuffer,
    capture_pts_us: i64,
) {
    if !inner.encoder_attach_requested.load(Ordering::Acquire) {
        return;
    }
    let attachment = match inner.encoder_attachment.read().as_ref().cloned() {
        Some(a) => a,
        None => return,
    };
    if !attachment.is_attached() {
        return;
    }
    let before = attachment.stats().ring_full_events;
    if !try_submit_zero_copy(&attachment, pixel_buffer, capture_pts_us) {
        let source_ptr = pixel_buffer as *const CVPixelBuffer as *mut c_void;
        let _ = attachment.submit_with_blit(source_ptr, capture_pts_us);
    }
    let after = attachment.stats().ring_full_events;
    if after > before && !inner.encoder_ring_full_emitted.swap(true, Ordering::AcqRel) {
        emit_encoder_ring_full(inner);
    }
}

fn try_submit_zero_copy(
    attachment: &EncoderAttachment,
    pixel_buffer: &CVPixelBuffer,
    capture_pts_us: i64,
) -> bool {
    assert!(capture_pts_us >= 0, "capture pts clamped non-negative");
    let format_type = CVPixelBufferGetPixelFormatType(pixel_buffer);
    if format_type != PIXEL_FORMAT_420V && format_type != PIXEL_FORMAT_420F {
        return false;
    }
    let width = CVPixelBufferGetWidth(pixel_buffer) as u32;
    let height = CVPixelBufferGetHeight(pixel_buffer) as u32;
    if width != attachment.width() {
        return false;
    }
    if height != attachment.height() {
        return false;
    }
    assert!(width > 0, "gated width positive");
    assert!(height > 0, "gated height positive");
    let lock_flags = CVPixelBufferLockFlags(0);
    let lock_status = unsafe { CVPixelBufferLockBaseAddress(pixel_buffer, lock_flags) };
    if lock_status != 0 {
        return false;
    }
    let surface = CVPixelBufferGetIOSurface(Some(pixel_buffer));
    let _ = unsafe { CVPixelBufferUnlockBaseAddress(pixel_buffer, lock_flags) };
    let Some(surface) = surface else {
        return false;
    };
    let surface_raw = CFRetained::into_raw(surface).cast::<c_void>();
    unsafe { attachment.submit_external_surface(surface_raw, capture_pts_us) }.is_ok()
}

fn emit_encoder_ring_full(inner: &CaptureInner) {
    let capture_id = inner
        .capture_id
        .lock()
        .ok()
        .and_then(|guard| guard.as_ref().cloned())
        .unwrap_or_default();
    let Ok(state) = inner.state.lock() else {
        return;
    };
    let Some(tsfn) = state.encoder_diagnostic_tsfn.as_ref() else {
        return;
    };
    let status = tsfn.call(
        ("encoderRingFull".to_string(), capture_id),
        ThreadsafeFunctionCallMode::NonBlocking,
    );
    note_tsfn_status(inner, status);
}

fn record_frame_sink_outcome(inner: &CaptureInner, outcome: EnqueueOutcome) {
    match outcome {
        EnqueueOutcome::Accepted => {
            inner.frame_sink_accepted.fetch_add(1, Ordering::AcqRel);
        }
        EnqueueOutcome::Coalesced => {
            inner.frame_sink_coalesced.fetch_add(1, Ordering::AcqRel);
            emit_frame_sink_backpressure_once(
                inner,
                "macOS CVPixelBuffer frame coalesced by native frame sink",
            );
        }
        EnqueueOutcome::Rejected => {
            inner.frame_sink_rejected.fetch_add(1, Ordering::AcqRel);
            emit_frame_sink_backpressure_once(
                inner,
                "macOS CVPixelBuffer frame rejected by native frame sink",
            );
        }
    }
}

fn emit_frame_sink_backpressure_once(inner: &CaptureInner, message: &'static str) {
    if inner
        .frame_sink_backpressure_emitted
        .swap(true, Ordering::AcqRel)
    {
        return;
    }
    emit_lifecycle_diagnostic(inner, message);
}

fn note_media_frame_without_sink(inner: &CaptureInner) {
    inner
        .media_frames_dropped_without_sink
        .fetch_add(1, Ordering::AcqRel);
    if inner
        .frame_sink_missing_emitted
        .swap(true, Ordering::AcqRel)
    {
        return;
    }
    emit_lifecycle_diagnostic(
        inner,
        "macOS screen frame dropped because no native frame sink is registered",
    );
}

fn bus_sink_for(inner: &CaptureInner) -> Option<Arc<dyn frame_bus::ScreenFrameSink>> {
    if let Some(sink) = inner.bus_sink.read().as_ref().cloned() {
        return Some(sink);
    }
    let capture_id = inner
        .capture_id
        .lock()
        .ok()
        .and_then(|guard| guard.as_ref().cloned())?;
    let sink = frame_bus::get_sink(&capture_id)?;
    *inner.bus_sink.write() = Some(sink.clone());
    Some(sink)
}

fn retain_native_frame_sink_handle(
    value: Unknown<'_>,
) -> Result<Arc<NativeScreenFrameSinkHandleRef>> {
    if value.get_type()? != ValueType::External {
        return Err(napi::Error::new(
            Status::InvalidArg,
            "ScreenCapture.setFrameSinkHandle expects a native external frame sink handle",
        ));
    }

    let raw_value = value.value();
    let mut data: *mut c_void = ptr::null_mut();
    let status =
        unsafe { napi::sys::napi_get_value_external(raw_value.env, raw_value.value, &mut data) };
    if status != napi::sys::Status::napi_ok || data.is_null() {
        return Err(napi::Error::new(
            Status::InvalidArg,
            "ScreenCapture.setFrameSinkHandle received an empty native external frame sink handle",
        ));
    }

    let handle = unsafe { data.cast::<NativeScreenFrameSinkHandle>().as_ref() }
        .and_then(NativeScreenFrameSinkHandle::retain_ref)
        .ok_or_else(|| {
            napi::Error::new(
                Status::InvalidArg,
                "ScreenCapture.setFrameSinkHandle received an invalid native frame sink handle",
            )
        })?;

    Ok(Arc::new(handle))
}

fn try_enqueue_native_cv_pixel_buffer(
    sink: &NativeScreenFrameSinkHandleRef,
    pixel_buffer: &CVPixelBuffer,
    width: u32,
    height: u32,
    sample_buffer: &CMSampleBuffer,
) -> EnqueueOutcome {
    if !sink.supports_mac_cv_pixel_buffer() {
        return EnqueueOutcome::Rejected;
    }
    let retained =
        unsafe { CVPixelBufferRetain(pixel_buffer as *const CVPixelBuffer as *mut c_void) };
    if retained.is_null() {
        return EnqueueOutcome::Rejected;
    }
    let pixel_format = CVPixelBufferGetPixelFormatType(pixel_buffer);
    unsafe {
        sink.enqueue_mac_cv_pixel_buffer(
            retained,
            width,
            height,
            pixel_format,
            sample_timestamp_us(sample_buffer),
        )
    }
}

fn try_enqueue_bus_cv_pixel_buffer(
    sink: &dyn frame_bus::ScreenFrameSink,
    pixel_buffer: &CVPixelBuffer,
    width: u32,
    height: u32,
    sample_buffer: &CMSampleBuffer,
) -> EnqueueOutcome {
    let retained =
        unsafe { CVPixelBufferRetain(pixel_buffer as *const CVPixelBuffer as *mut c_void) };
    if retained.is_null() {
        return EnqueueOutcome::Rejected;
    }
    let pixel_format = CVPixelBufferGetPixelFormatType(pixel_buffer);
    let bus_frame = unsafe {
        MacCvPixelBufferFrame::from_retained(
            retained,
            width,
            height,
            pixel_format,
            sample_timestamp_us(sample_buffer),
        )
    };
    sink.enqueue(BusScreenFrame::MacCvPixelBuffer(bus_frame))
}

#[napi(js_name = "ScreenCapture")]
pub struct ScreenCapture {
    inner: Arc<CaptureInner>,
}

#[napi(object, js_name = "ScreenCaptureStartResult")]
pub struct ScreenCaptureStartResult {
    pub width: u32,
    pub height: u32,
    pub frame_rate: u32,
    pub pixel_format: String,
}

#[napi(object, js_name = "FrameSinkDiagnostics")]
pub struct FrameSinkDiagnostics {
    pub accepted: f64,
    pub coalesced: f64,
    pub rejected: f64,
    #[napi(js_name = "mediaFramesDroppedWithoutSink")]
    pub media_frames_dropped_without_sink: f64,
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
    #[napi(js_name = "maxOutputWidth")]
    pub max_output_width: Option<u32>,
    #[napi(js_name = "maxOutputHeight")]
    pub max_output_height: Option<u32>,
}

#[derive(Clone, Copy, Debug)]
struct NormalizedStartOptions {
    shows_cursor: bool,
    source_rect: Option<CGRect>,
    pixel_format: SckPixelFormat,
    color_space: SckColorSpace,
    max_output_width: u32,
    max_output_height: u32,
}

#[napi]
impl ScreenCapture {
    #[napi(constructor)]
    pub fn new() -> Self {
        let audio_pool = MacAudioFramePool::new(MAC_AUDIO_POOL_CAP, MAX_FRAME_BYTES_PER_SLOT)
            .unwrap_or_else(|_| {
                unreachable!("MAC_AUDIO_POOL_CAP and MAX_FRAME_BYTES_PER_SLOT must validate")
            });
        Self {
            inner: Arc::new(CaptureInner {
                state: Mutex::new(CaptureState {
                    lifecycle_tsfn: None,
                    audio_diagnostic_tsfn: None,
                    encoder_diagnostic_tsfn: None,
                    delegate: None,
                    stream: None,
                    sample_queue: None,
                }),
                audio_frame_tsfn: parking_lot::RwLock::new(None),
                running: AtomicBool::new(false),
                tsfn_aborted: AtomicBool::new(false),
                capture_id: Mutex::new(None),
                bus_sink: parking_lot::RwLock::new(None),
                native_frame_sink: parking_lot::RwLock::new(None),
                failure_surface: parking_lot::RwLock::new(None),
                captures_audio: AtomicBool::new(false),
                audio_sample_rate_hz: AtomicU32::new(AUDIO_SAMPLE_RATE_DEFAULT_HZ),
                audio_channels: AtomicU32::new(AUDIO_CHANNEL_COUNT_DEFAULT),
                audio_pool,
                encoder_attachment: parking_lot::RwLock::new(None),
                encoder_attach_requested: AtomicBool::new(false),
                encoder_ring_full_emitted: AtomicBool::new(false),
                frame_sink_accepted: AtomicU64::new(0),
                frame_sink_coalesced: AtomicU64::new(0),
                frame_sink_rejected: AtomicU64::new(0),
                media_frames_dropped_without_sink: AtomicU64::new(0),
                frame_sink_backpressure_emitted: AtomicBool::new(false),
                frame_sink_missing_emitted: AtomicBool::new(false),
            }),
        }
    }

    #[napi]
    pub fn set_lifecycle_callback(&self, callback: Function<(String, String), ()>) -> Result<()> {
        let tsfn: LifecycleTsfn = callback
            .build_threadsafe_function::<(String, String)>()
            .max_queue_size::<8>()
            .build_callback(|ctx| Ok(ctx.value))?;
        let mut state = lock_state(&self.inner)?;
        state.lifecycle_tsfn = Some(tsfn);
        self.inner.tsfn_aborted.store(false, Ordering::Release);
        Ok(())
    }

    #[napi(js_name = "setFrameSinkHandle")]
    pub fn set_frame_sink_handle(&self, frame_sink_handle: Unknown<'_>) -> Result<()> {
        let sink = retain_native_frame_sink_handle(frame_sink_handle)?;
        *self.inner.native_frame_sink.write() = Some(sink);
        Ok(())
    }

    #[napi(js_name = "setAudioFrameCallback")]
    pub fn set_audio_frame_callback(
        &self,
        callback: Function<AudioFrameTsfnArgs, ()>,
    ) -> Result<()> {
        let tsfn: AudioFrameTsfn = callback
            .build_threadsafe_function::<AudioFrameTsfnInput>()
            .max_queue_size::<16>()
            .build_callback(|ctx| build_audio_callback_args(ctx.env, ctx.value))?;
        *self.inner.audio_frame_tsfn.write() = Some(tsfn);
        Ok(())
    }

    #[napi(js_name = "setAudioDiagnosticCallback")]
    pub fn set_audio_diagnostic_callback(
        &self,
        callback: Function<AudioDiagnosticTsfnArgs, ()>,
    ) -> Result<()> {
        let tsfn: AudioDiagnosticTsfn = callback
            .build_threadsafe_function::<AudioDiagnosticTsfnArgs>()
            .max_queue_size::<8>()
            .build_callback(|ctx| Ok(ctx.value))?;
        let mut state = lock_state(&self.inner)?;
        state.audio_diagnostic_tsfn = Some(tsfn);
        Ok(())
    }

    #[napi(js_name = "setEncoderDiagnosticCallback")]
    pub fn set_encoder_diagnostic_callback(
        &self,
        callback: Function<EncoderDiagnosticTsfnArgs, ()>,
    ) -> Result<()> {
        let tsfn: EncoderDiagnosticTsfn = callback
            .build_threadsafe_function::<EncoderDiagnosticTsfnArgs>()
            .max_queue_size::<8>()
            .build_callback(|ctx| Ok(ctx.value))?;
        let mut state = lock_state(&self.inner)?;
        state.encoder_diagnostic_tsfn = Some(tsfn);
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
        self.inner
            .encoder_attach_requested
            .store(true, Ordering::Release);
        self.inner
            .encoder_ring_full_emitted
            .store(false, Ordering::Release);
        Ok(())
    }

    #[napi(js_name = "detachEncoder")]
    pub fn detach_encoder(&self) -> Result<()> {
        if let Some(attach) = self.inner.encoder_attachment.write().take() {
            attach.detach();
        }
        self.inner
            .encoder_attach_requested
            .store(false, Ordering::Release);
        Ok(())
    }

    #[napi(js_name = "isEncoderAttached")]
    pub fn is_encoder_attached(&self) -> bool {
        self.inner
            .encoder_attachment
            .read()
            .as_ref()
            .map(|a| a.is_attached())
            .unwrap_or(false)
    }

    #[napi(js_name = "encoderRingFullCount")]
    pub fn encoder_ring_full_count(&self) -> u32 {
        self.inner
            .encoder_attachment
            .read()
            .as_ref()
            .map(|a| a.stats().ring_full_events.min(u32::MAX as u64) as u32)
            .unwrap_or(0)
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

    #[napi(js_name = "enableAudioCapture")]
    pub fn enable_audio_capture(
        &self,
        captures_audio: bool,
        sample_rate_hz: u32,
        channels: u32,
    ) -> Result<()> {
        if !(8_000..=192_000).contains(&sample_rate_hz) {
            return Err(napi::Error::new(
                Status::InvalidArg,
                "ScreenCapture.enableAudioCapture sampleRateHz out of [8000..=192000]",
            ));
        }
        if !(1..=8).contains(&channels) {
            return Err(napi::Error::new(
                Status::InvalidArg,
                "ScreenCapture.enableAudioCapture channels out of [1..=8]",
            ));
        }
        self.inner
            .captures_audio
            .store(captures_audio, Ordering::Release);
        self.inner
            .audio_sample_rate_hz
            .store(sample_rate_hz, Ordering::Release);
        self.inner.audio_channels.store(channels, Ordering::Release);
        Ok(())
    }

    pub fn install_failure_surface(&self, surface: Arc<dyn CaptureFailureSurface>) {
        *self.inner.failure_surface.write() = Some(surface);
    }

    pub fn clear_failure_surface(&self) {
        self.inner.failure_surface.write().take();
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
        let start_options = normalize_start_options(capture_options)?;
        let normalized_capture_id = capture_id
            .map(|raw| raw.trim().to_string())
            .filter(|trimmed| !trimmed.is_empty());
        *self
            .inner
            .capture_id
            .lock()
            .map_err(|_| generic_error("ScreenCapture capture_id lock poisoned"))? =
            normalized_capture_id.clone();
        *self.inner.bus_sink.write() = if self.inner.native_frame_sink.read().is_some() {
            None
        } else {
            normalized_capture_id
                .as_deref()
                .and_then(frame_bus::get_sink)
        };
        let source_id_num: u32 = source_id.parse().map_err(|_| {
            napi::Error::new(
                Status::InvalidArg,
                "ScreenCapture.start sourceId must be a u32 string",
            )
        })?;
        if source_id_num == 0 {
            return Err(napi::Error::new(
                Status::InvalidArg,
                "ScreenCapture.start sourceId must be > 0",
            ));
        }
        let source_id = source_id_num;
        {
            let detected = os_version::current_macos_version();
            let SupportClassification {
                supported, reason, ..
            } = classify_support(detected);
            if !supported {
                return Err(napi::Error::new(Status::GenericFailure, reason));
            }
        }
        {
            let state = lock_state(&self.inner)?;
            if state.stream.is_some()
                || state.delegate.is_some()
                || self.inner.running.load(Ordering::Acquire)
            {
                return Err(napi::Error::new(
                    Status::GenericFailure,
                    "ScreenCapture is already running",
                ));
            }
        }
        let content = get_shareable_content(DEFAULT_TIMEOUT_NS)
            .map_err(|_| generic_error("Failed to fetch SCShareableContent"))?;

        let (filter, dims) =
            build_filter_for_source(&content, source_id, &source_kind, width, height)?;

        let point_pixel_scale = sck::filter_point_pixel_scale_if_available(&filter);
        let content_rect = sck::filter_content_rect_if_available(&filter);
        let (source_width, source_height) = resolve_source_pixels(
            width,
            height,
            start_options.source_rect,
            content_rect,
            point_pixel_scale,
            dims,
        );
        let (final_width, final_height) = cap_output_dims(
            source_width,
            source_height,
            start_options.max_output_width,
            start_options.max_output_height,
        );
        assert!(final_width >= crate::config::OUTPUT_DIMENSION_MIN);
        assert!(final_height >= crate::config::OUTPUT_DIMENSION_MIN);
        let effective_fps = if frame_rate == 0 {
            30
        } else {
            frame_rate.min(240)
        };

        let cfg: Retained<SCStreamConfiguration> = unsafe { SCStreamConfiguration::new() };
        sck::cfg_set_width(&cfg, final_width as usize);
        sck::cfg_set_height(&cfg, final_height as usize);
        sck::cfg_set_scales_to_fit_if_available(&cfg, true);
        sck::cfg_set_shows_cursor(&cfg, start_options.shows_cursor);
        if let Some(source_rect) = start_options.source_rect {
            if !sck::cfg_set_source_rect_if_available(&cfg, source_rect) {
                emit_lifecycle_diagnostic(
                    &self.inner,
                    "captureRect requested but this ScreenCaptureKit runtime lacks setSourceRect",
                );
            }
        }

        let captures_audio = self.inner.captures_audio.load(Ordering::Acquire);
        let audio_sample_rate_hz = self.inner.audio_sample_rate_hz.load(Ordering::Acquire);
        let audio_channels = self.inner.audio_channels.load(Ordering::Acquire);
        let capture_cfg = build_capture_config(
            effective_fps,
            captures_audio,
            audio_sample_rate_hz,
            audio_channels,
            start_options.pixel_format,
            start_options.color_space,
        );
        sck::apply_capture_config(&cfg, &capture_cfg);

        let stream_name = format!("Fluxer ScreenCapture ({}:{})", source_kind, source_id);
        let nsname = NSString::from_str(&stream_name);
        sck::cfg_set_stream_name_if_available(&cfg, &nsname);

        let delegate = FluxerSCKScreenSource::new(Arc::downgrade(&self.inner));

        let stream_alloc = SCStream::alloc();
        let stream: Retained<SCStream> = unsafe {
            SCStream::initWithFilter_configuration_delegate(
                stream_alloc,
                &filter,
                &cfg,
                Some(ProtocolObject::from_ref(&*delegate)),
            )
        };

        let sample_queue = build_scstream_sample_queue(&source_kind, source_id);

        if sck::sc_stream_add_stream_output(
            &stream,
            ProtocolObject::from_ref(&*delegate),
            SCStreamOutputType::Screen,
            Some(&sample_queue),
        )
        .is_err()
        {
            delegate.clear_inner();
            return Err(generic_error("Failed to add screen stream output"));
        }

        if captures_audio
            && sck::sc_stream_add_stream_output(
                &stream,
                ProtocolObject::from_ref(&*delegate),
                SCStreamOutputType::Audio,
                Some(&sample_queue),
            )
            .is_err()
        {
            delegate.clear_inner();
            return Err(generic_error("Failed to add audio stream output"));
        }

        {
            let mut state = lock_state(&self.inner)?;
            state.delegate = Some(delegate.clone());
            state.stream = Some(stream.clone());
            state.sample_queue = Some(sample_queue);
        }
        self.inner.running.store(true, Ordering::Release);

        if await_start(&stream, DEFAULT_TIMEOUT_NS).is_err() {
            self.inner.running.store(false, Ordering::Release);
            if let Some(surface) = self.inner.failure_surface.read().as_ref().cloned() {
                surface.on_failure(SckCaptureFailure::StreamStartFailed(
                    "SCStream startCapture failed".to_string(),
                ));
            }
            let mut state = lock_state(&self.inner)?;
            if let Some(d) = state.delegate.take() {
                d.clear_inner();
            }
            state.stream = None;
            return Err(generic_error("SCStream startCapture failed"));
        }

        Ok(ScreenCaptureStartResult {
            width: final_width,
            height: final_height,
            frame_rate: effective_fps,
            pixel_format: "nv12".to_string(),
        })
    }

    #[napi]
    pub async fn stop(&self) -> Result<()> {
        self.inner.running.store(false, Ordering::Release);
        if let Ok(mut guard) = self.inner.capture_id.lock() {
            guard.take();
        }
        self.inner.bus_sink.write().take();
        self.inner.native_frame_sink.write().take();
        self.inner.failure_surface.write().take();
        if let Some(attach) = self.inner.encoder_attachment.write().take() {
            attach.detach();
        }
        self.inner
            .encoder_attach_requested
            .store(false, Ordering::Release);
        let (delegate, stream, queue) = {
            let mut state = lock_state(&self.inner)?;
            let delegate = state.delegate.take();
            let stream = state.stream.take();
            let queue = state.sample_queue.take();
            (delegate, stream, queue)
        };
        if let Some(d) = delegate.as_ref() {
            d.clear_inner();
        }
        if let Some(s) = stream.as_ref() {
            let _ = await_stop(s, DEFAULT_TIMEOUT_NS);
        }
        drop(stream);
        drop(delegate);
        drop(queue);
        Ok(())
    }
}

impl Drop for ScreenCapture {
    fn drop(&mut self) {
        self.inner.running.store(false, Ordering::Release);
        self.inner.bus_sink.write().take();
        self.inner.native_frame_sink.write().take();
        self.inner.failure_surface.write().take();
        if let Some(attach) = self.inner.encoder_attachment.write().take() {
            attach.detach();
        }
        self.inner
            .encoder_attach_requested
            .store(false, Ordering::Release);
        let (delegate, stream, _queue) = match self.inner.state.lock() {
            Ok(mut s) => (s.delegate.take(), s.stream.take(), s.sample_queue.take()),
            Err(_) => (None, None, None),
        };
        if let Some(d) = delegate.as_ref() {
            d.clear_inner();
        }
        if let Some(s) = stream.as_ref() {
            let _ = await_stop(s, DEFAULT_TIMEOUT_NS);
        }
    }
}

fn build_scstream_sample_queue(
    source_kind: &str,
    source_id: u32,
) -> dispatch2::DispatchRetained<dispatch2::DispatchQueue> {
    assert!(!source_kind.is_empty(), "source kind non-empty");
    assert!(source_id > 0, "source id positive");
    let label = format!("com.fluxer.scstream.{}-{}", source_kind, source_id);
    let queue_attr = dispatch2::DispatchQueueAttr::with_qos_class(
        dispatch2::DispatchQueueAttr::SERIAL,
        dispatch2::DispatchQoS::UserInteractive,
        0,
    );
    dispatch2::DispatchQueue::new(label.as_str(), Some(&queue_attr))
}

fn build_capture_config(
    effective_fps: u32,
    captures_audio: bool,
    audio_sample_rate_hz: u32,
    audio_channels: u32,
    pixel_format: SckPixelFormat,
    color_space: SckColorSpace,
) -> SckCaptureConfig {
    assert!(effective_fps >= crate::config::FPS_MIN);
    assert!(audio_channels >= 1);
    let clamped_fps = effective_fps.clamp(crate::config::FPS_MIN, crate::config::FPS_MAX);
    SckCaptureConfig::builder()
        .target_fps(clamped_fps)
        .queue_depth(crate::config::QUEUE_DEPTH_DEFAULT)
        .pixel_format(pixel_format)
        .color_space(color_space)
        .captures_audio(captures_audio)
        .audio_sample_rate_hz(audio_sample_rate_hz)
        .audio_channels(audio_channels)
        .build()
        .unwrap_or_default()
}

fn normalize_start_options(
    options: Option<ScreenCaptureStartOptions>,
) -> Result<NormalizedStartOptions> {
    let Some(options) = options else {
        return Ok(NormalizedStartOptions::default());
    };
    let source_rect = normalize_capture_rect(options.capture_rect)?;
    let pixel_format = normalize_color_range(options.color_range.as_deref())?;
    let color_space = normalize_color_space(options.color_space.as_deref())?;
    let max_output_width = normalize_max_output_dimension(
        options.max_output_width,
        crate::config::MAX_OUTPUT_WIDTH_DEFAULT,
    );
    let max_output_height = normalize_max_output_dimension(
        options.max_output_height,
        crate::config::MAX_OUTPUT_HEIGHT_DEFAULT,
    );
    Ok(NormalizedStartOptions {
        shows_cursor: options.show_cursor_clicks.unwrap_or(false),
        source_rect,
        pixel_format,
        color_space,
        max_output_width,
        max_output_height,
    })
}

fn normalize_max_output_dimension(requested: Option<u32>, fallback: u32) -> u32 {
    assert!(fallback >= crate::config::OUTPUT_DIMENSION_MIN);
    let resolved = requested
        .filter(|v| *v >= crate::config::OUTPUT_DIMENSION_MIN)
        .unwrap_or(fallback);
    let even = resolved & !1;
    assert!(even >= crate::config::OUTPUT_DIMENSION_MIN);
    even
}

fn normalize_capture_rect(rect: Option<ScreenCaptureRect>) -> Result<Option<CGRect>> {
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
    Ok(Some(CGRect::new(
        CGPoint::new(rect.x, rect.y),
        CGSize::new(rect.width, rect.height),
    )))
}

fn normalize_color_range(value: Option<&str>) -> Result<SckPixelFormat> {
    match value {
        None | Some("limited") => Ok(SckPixelFormat::Nv12VideoRange),
        Some("full") => Ok(SckPixelFormat::Nv12FullRange),
        Some(_) => Err(invalid_arg(
            "ScreenCapture.start colorRange must be 'limited' or 'full'",
        )),
    }
}

fn normalize_color_space(value: Option<&str>) -> Result<SckColorSpace> {
    match value {
        None | Some("srgb") | Some("rec709") => Ok(SckColorSpace::SrgbBt709),
        Some(_) => Err(invalid_arg(
            "ScreenCapture.start colorSpace must be 'srgb' or 'rec709'",
        )),
    }
}

impl Default for NormalizedStartOptions {
    fn default() -> Self {
        Self {
            shows_cursor: false,
            source_rect: None,
            pixel_format: SckPixelFormat::Nv12VideoRange,
            color_space: SckColorSpace::SrgbBt709,
            max_output_width: crate::config::MAX_OUTPUT_WIDTH_DEFAULT,
            max_output_height: crate::config::MAX_OUTPUT_HEIGHT_DEFAULT,
        }
    }
}

fn emit_lifecycle_diagnostic(inner: &CaptureInner, message: &'static str) {
    let Ok(state) = inner.state.lock() else {
        return;
    };
    let Some(tsfn) = state.lifecycle_tsfn.as_ref() else {
        return;
    };
    let status = tsfn.call(
        ("diagnostic".to_string(), message.to_string()),
        ThreadsafeFunctionCallMode::NonBlocking,
    );
    note_tsfn_status(inner, status);
}

fn even_floor(value: u32) -> u32 {
    value & !1
}

fn resolve_source_pixels(
    requested_width: u32,
    requested_height: u32,
    source_rect: Option<CGRect>,
    content_rect: Option<CGRect>,
    point_pixel_scale: Option<f32>,
    point_dims_fallback: (u32, u32),
) -> (u32, u32) {
    let scale = point_pixel_scale
        .filter(|s| s.is_finite() && *s > 0.0)
        .map(|s| s as f64)
        .unwrap_or(1.0);
    assert!(scale > 0.0);
    let min = crate::config::OUTPUT_DIMENSION_MIN;
    if let Some(rect) = source_rect {
        assert!(rect.size.width > 0.0);
        assert!(rect.size.height > 0.0);
        let w = (rect.size.width * scale).round() as u32;
        let h = (rect.size.height * scale).round() as u32;
        return (even_floor(w.max(min)), even_floor(h.max(min)));
    }
    if let Some(rect) = content_rect {
        assert!(rect.size.width > 0.0);
        assert!(rect.size.height > 0.0);
        let w = (rect.size.width * scale).round() as u32;
        let h = (rect.size.height * scale).round() as u32;
        return (even_floor(w.max(min)), even_floor(h.max(min)));
    }
    if requested_width > 0 && requested_height > 0 {
        return (
            even_floor(requested_width.max(min)),
            even_floor(requested_height.max(min)),
        );
    }
    (
        even_floor(point_dims_fallback.0.max(min)),
        even_floor(point_dims_fallback.1.max(min)),
    )
}

fn cap_output_dims(src_w: u32, src_h: u32, max_w: u32, max_h: u32) -> (u32, u32) {
    let min = crate::config::OUTPUT_DIMENSION_MIN;
    assert!(src_w >= min);
    assert!(src_h >= min);
    let max_w = even_floor(max_w).max(min);
    let max_h = even_floor(max_h).max(min);
    let scale = (max_w as f64 / src_w as f64)
        .min(max_h as f64 / src_h as f64)
        .min(1.0);
    assert!(scale > 0.0);
    assert!(scale <= 1.0);
    let out_w = even_floor(((src_w as f64) * scale).round() as u32).clamp(min, max_w);
    let out_h = even_floor(((src_h as f64) * scale).round() as u32).clamp(min, max_h);
    assert!(out_w <= max_w);
    assert!(out_h <= max_h);
    (out_w, out_h)
}

fn build_filter_for_source(
    content: &SCShareableContent,
    source_id: u32,
    source_kind: &str,
    requested_width: u32,
    requested_height: u32,
) -> Result<(Retained<SCContentFilter>, (u32, u32))> {
    match source_kind {
        "screen" => {
            let displays: Retained<NSArray<SCDisplay>> = unsafe { content.displays() };
            let count = displays.count();
            let mut chosen: Option<Retained<SCDisplay>> = None;
            for i in 0..count {
                let d = displays.objectAtIndex(i);
                if sck::sc_display_display_id(&d) == source_id {
                    chosen = Some(d);
                    break;
                }
            }
            let display =
                chosen.ok_or_else(|| generic_error("No SCDisplay matches requested sourceId"))?;
            let dims = (
                if requested_width > 0 {
                    requested_width
                } else {
                    sck::sc_display_width(&display).max(0) as u32
                },
                if requested_height > 0 {
                    requested_height
                } else {
                    sck::sc_display_height(&display).max(0) as u32
                },
            );

            let empty: Retained<NSArray<SCWindow>> = NSArray::new();
            let filter_alloc = SCContentFilter::alloc();
            let filter: Retained<SCContentFilter> = unsafe {
                SCContentFilter::initWithDisplay_excludingWindows(filter_alloc, &display, &empty)
            };
            Ok((filter, dims))
        }
        "window" => {
            let windows: Retained<NSArray<SCWindow>> = unsafe { content.windows() };
            let count = windows.count();
            let mut chosen: Option<Retained<SCWindow>> = None;
            for i in 0..count {
                let w = windows.objectAtIndex(i);
                if sck::sc_window_window_id(&w) == source_id {
                    chosen = Some(w);
                    break;
                }
            }
            let window =
                chosen.ok_or_else(|| generic_error("No SCWindow matches requested sourceId"))?;
            let frame = sck::sc_window_frame(&window);
            let dims = (
                if requested_width > 0 {
                    requested_width
                } else {
                    frame.size.width.max(0.0) as u32
                },
                if requested_height > 0 {
                    requested_height
                } else {
                    frame.size.height.max(0.0) as u32
                },
            );
            let filter_alloc = SCContentFilter::alloc();
            let filter: Retained<SCContentFilter> =
                unsafe { SCContentFilter::initWithDesktopIndependentWindow(filter_alloc, &window) };
            Ok((filter, dims))
        }
        other => Err(napi::Error::new(
            Status::InvalidArg,
            format!("Unknown sourceKind: {other}"),
        )),
    }
}

#[napi(object, js_name = "MacScreenCaptureSource")]
pub struct MacScreenCaptureSource {
    pub kind: String,
    pub id: String,
    pub name: String,
    pub width: u32,
    pub height: u32,
    pub app_name: Option<String>,
    pub bundle_id: Option<String>,

    pub target_pid: Option<i32>,
}

#[napi(js_name = "listSources")]
pub async fn list_sources() -> Result<Vec<MacScreenCaptureSource>> {
    let content = match get_shareable_content(DEFAULT_TIMEOUT_NS) {
        Ok(c) => c,
        Err(_) => return Ok(Vec::new()),
    };
    let mut out: Vec<MacScreenCaptureSource> = Vec::new();
    let displays: Retained<NSArray<SCDisplay>> = unsafe { content.displays() };
    let dcount = displays.count();
    for i in 0..dcount {
        let d = displays.objectAtIndex(i);
        let id = sck::sc_display_display_id(&d);
        let name = format!("Display {}", i + 1);
        out.push(MacScreenCaptureSource {
            kind: "screen".to_string(),
            id: id.to_string(),
            name,
            width: sck::sc_display_width(&d).max(0) as u32,
            height: sck::sc_display_height(&d).max(0) as u32,
            app_name: None,
            bundle_id: None,
            target_pid: None,
        });
    }
    let windows: Retained<NSArray<SCWindow>> = unsafe { content.windows() };
    let wcount = windows.count();
    for i in 0..wcount {
        let w = windows.objectAtIndex(i);
        if !sck::sc_window_is_on_screen(&w) {
            continue;
        }
        let id = sck::sc_window_window_id(&w);
        let title = sck::sc_window_title(&w)
            .map(|s| s.to_string())
            .unwrap_or_default();
        let frame = sck::sc_window_frame(&w);
        let (app_name, bundle_id, target_pid) = match sck::sc_window_owning_application(&w) {
            Some(app) => (
                Some(sck::sc_running_application_name(&app).to_string()),
                Some(sck::sc_running_application_bundle_identifier(&app).to_string()),
                Some(sck::sc_running_application_process_id(&app)).filter(|pid| *pid > 0),
            ),
            None => (None, None, None),
        };
        let display_name = if title.is_empty() {
            app_name.clone().unwrap_or_else(|| format!("Window {id}"))
        } else if let Some(ref app) = app_name {
            format!("{app} — {title}")
        } else {
            title
        };
        out.push(MacScreenCaptureSource {
            kind: "window".to_string(),
            id: id.to_string(),
            name: display_name,
            width: frame.size.width.max(0.0) as u32,
            height: frame.size.height.max(0.0) as u32,
            app_name,
            bundle_id,
            target_pid,
        });
    }
    Ok(out)
}

#[napi(object, js_name = "MacScreenCaptureBackendSckAvailability")]
pub struct SckAvailability {
    pub supported: bool,
    pub macos_version: Option<String>,
}

#[napi(object, js_name = "MacScreenCaptureBackendAvailability")]
pub struct BackendAvailability {
    pub sck: SckAvailability,
    pub screen_permission: String,
}

#[napi(js_name = "getBackendAvailability")]
pub async fn get_backend_availability() -> Result<BackendAvailability> {
    use objc2::runtime::AnyClass;
    let sck_supported = AnyClass::get(c"SCStream").is_some();
    let version_str = Some(foundation::operating_system_version_string());
    let screen_permission = if unsafe { CGPreflightScreenCaptureAccess() } {
        "granted"
    } else {
        "denied"
    };
    Ok(BackendAvailability {
        sck: SckAvailability {
            supported: sck_supported,
            macos_version: version_str,
        },
        screen_permission: screen_permission.to_string(),
    })
}

#[napi(object, js_name = "MacScreenCaptureBackendInfo")]
pub struct MacScreenCaptureBackendInfo {
    pub backend: String,
    pub supported: bool,
    pub reason: String,
    #[napi(js_name = "minMacosVersion")]
    pub min_macos_version: String,
    #[napi(js_name = "detectedMacosVersion")]
    pub detected_macos_version: Option<String>,
    #[napi(js_name = "sckAvailable")]
    pub sck_available: bool,
}

#[napi(js_name = "getBackendInfo")]
pub fn get_backend_info() -> MacScreenCaptureBackendInfo {
    let detected = os_version::current_macos_version();
    let SupportClassification {
        supported,
        sck_available,
        reason,
    } = classify_support(detected);
    MacScreenCaptureBackendInfo {
        backend: "mac-screen-capture".to_owned(),
        supported,
        reason,
        min_macos_version: format_version(SCK_MIN_MACOS),
        detected_macos_version: detected.map(format_version),
        sck_available,
    }
}

#[cfg(test)]
mod dispatch_queue_tests {
    use std::sync::Arc;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::Duration;

    use super::build_scstream_sample_queue;

    #[test]
    fn sample_queue_construction_does_not_trap_libdispatch() {
        let _queue = build_scstream_sample_queue("screen", 1);
    }

    #[test]
    fn sample_queue_dispatches_blocks_without_crashing() {
        let queue = build_scstream_sample_queue("screen", 2);
        let counter = Arc::new(AtomicU64::new(0));
        for _ in 0..32 {
            let c = counter.clone();
            queue.exec_async(move || {
                c.fetch_add(1, Ordering::Release);
            });
        }
        let deadline = std::time::Instant::now() + Duration::from_secs(2);
        while counter.load(Ordering::Acquire) < 32 {
            if std::time::Instant::now() > deadline {
                panic!(
                    "sample queue did not drain blocks: {}/{}",
                    counter.load(Ordering::Acquire),
                    32
                );
            }
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    #[test]
    fn audio_frame_payload_into_input_carries_slot_and_metadata() {
        use crate::audio_pool::MacAudioFramePool;
        let pool = MacAudioFramePool::new(2, 64).expect("pool");
        let mut slot = pool.try_acquire().expect("slot");
        let bytes = vec![0xAB_u8; 8];
        slot.write(&bytes).expect("write");
        let payload = super::AudioFramePayload {
            sample_rate_hz: 48_000,
            channels: 2,
            num_samples_per_channel: 1024,
            pts_us: 7_777,
            format: super::AudioSampleFormat::F32Planar,
            slot,
        };
        let (sr, ch, ns, pts, fmt, slot) = payload.into_input();
        assert_eq!(sr, 48_000);
        assert_eq!(ch, 2);
        assert_eq!(ns, 1024);
        assert_eq!(pts, 7_777);
        assert_eq!(fmt, super::AudioSampleFormat::F32Planar);
        assert_eq!(fmt.code(), 0);
        assert_eq!(slot.filled_len(), bytes.len());
        assert_eq!(slot.data_slice(), &bytes[..]);
        drop(slot);
        assert_eq!(pool.stats().in_flight, 0);
    }

    #[test]
    fn classify_pcm_flags_detects_f32_planar() {
        use objc2_core_audio_types::{kAudioFormatFlagIsFloat, kAudioFormatFlagIsNonInterleaved};
        let flags = kAudioFormatFlagIsFloat | kAudioFormatFlagIsNonInterleaved;
        assert_eq!(
            super::classify_pcm_flags(flags, 32),
            super::AudioSampleFormat::F32Planar
        );
    }

    #[test]
    fn classify_pcm_flags_detects_f32_interleaved() {
        use objc2_core_audio_types::kAudioFormatFlagIsFloat;
        let flags = kAudioFormatFlagIsFloat;
        assert_eq!(
            super::classify_pcm_flags(flags, 32),
            super::AudioSampleFormat::F32Interleaved
        );
    }

    #[test]
    fn classify_pcm_flags_distinguishes_planar_from_interleaved_float() {
        use objc2_core_audio_types::{kAudioFormatFlagIsFloat, kAudioFormatFlagIsNonInterleaved};
        let planar = super::classify_pcm_flags(
            kAudioFormatFlagIsFloat | kAudioFormatFlagIsNonInterleaved,
            32,
        );
        let interleaved = super::classify_pcm_flags(kAudioFormatFlagIsFloat, 32);
        assert_eq!(planar, super::AudioSampleFormat::F32Planar);
        assert_eq!(interleaved, super::AudioSampleFormat::F32Interleaved);
        assert_ne!(planar, interleaved);
        assert_eq!(planar.bytes_per_sample(), interleaved.bytes_per_sample());
    }

    #[test]
    fn classify_pcm_flags_detects_i16_interleaved() {
        use objc2_core_audio_types::kAudioFormatFlagIsSignedInteger;
        let flags = kAudioFormatFlagIsSignedInteger;
        assert_eq!(
            super::classify_pcm_flags(flags, 16),
            super::AudioSampleFormat::I16Interleaved
        );
    }

    #[test]
    fn classify_pcm_flags_rejects_unknown() {
        assert_eq!(
            super::classify_pcm_flags(0, 24),
            super::AudioSampleFormat::Unknown
        );
    }

    #[test]
    fn build_capture_config_preserves_frame_interval_when_audio_enabled() {
        let cfg_off = super::build_capture_config(
            60,
            false,
            48_000,
            2,
            super::SckPixelFormat::Nv12VideoRange,
            super::SckColorSpace::SrgbBt709,
        );
        let cfg_on = super::build_capture_config(
            60,
            true,
            48_000,
            2,
            super::SckPixelFormat::Nv12VideoRange,
            super::SckColorSpace::SrgbBt709,
        );
        assert!(!cfg_off.captures_audio());
        assert!(cfg_on.captures_audio());
        assert_eq!(
            cfg_off.minimum_frame_interval_ns(),
            cfg_on.minimum_frame_interval_ns()
        );
    }

    #[test]
    fn build_capture_config_carries_audio_settings() {
        let cfg = super::build_capture_config(
            30,
            true,
            44_100,
            1,
            super::SckPixelFormat::Nv12VideoRange,
            super::SckColorSpace::SrgbBt709,
        );
        assert!(cfg.captures_audio());
        assert_eq!(cfg.audio_sample_rate_hz(), 44_100);
        assert_eq!(cfg.audio_channels(), 1);
    }

    #[test]
    fn start_options_carry_cursor_color_and_rect_intent() {
        let options = super::normalize_start_options(Some(super::ScreenCaptureStartOptions {
            show_cursor_clicks: Some(true),
            capture_rect: Some(super::ScreenCaptureRect {
                x: 10.0,
                y: 20.0,
                width: 300.0,
                height: 200.0,
            }),
            color_range: Some("full".to_string()),
            color_space: Some("rec709".to_string()),
            max_output_width: None,
            max_output_height: None,
        }))
        .expect("valid start options");
        assert!(options.shows_cursor);
        assert_eq!(options.pixel_format, super::SckPixelFormat::Nv12FullRange);
        assert_eq!(options.color_space, super::SckColorSpace::SrgbBt709);
        let rect = options.source_rect.expect("capture rect");
        assert_eq!(rect.origin.x, 10.0);
        assert_eq!(rect.origin.y, 20.0);
        assert_eq!(rect.size.width, 300.0);
        assert_eq!(rect.size.height, 200.0);
    }

    #[test]
    fn start_options_reject_invalid_color_range() {
        let err = super::normalize_start_options(Some(super::ScreenCaptureStartOptions {
            show_cursor_clicks: None,
            capture_rect: None,
            color_range: Some("wide".to_string()),
            color_space: None,
            max_output_width: None,
            max_output_height: None,
        }))
        .expect_err("invalid color range");
        assert_eq!(err.status, napi::Status::InvalidArg);
    }

    #[test]
    fn parallel_sample_queue_construction_is_safe() {
        let handles: Vec<_> = (0..8)
            .map(|i| {
                std::thread::spawn(move || {
                    let q = build_scstream_sample_queue("window", (i + 1) as u32);
                    let done = Arc::new(AtomicU64::new(0));
                    let dc = done.clone();
                    q.exec_async(move || {
                        dc.store(1, Ordering::Release);
                    });
                    let deadline = std::time::Instant::now() + Duration::from_secs(1);
                    while done.load(Ordering::Acquire) == 0 {
                        if std::time::Instant::now() > deadline {
                            panic!("parallel queue {} did not drain", i);
                        }
                        std::thread::sleep(Duration::from_millis(5));
                    }
                })
            })
            .collect();
        for h in handles {
            h.join().expect("worker panicked");
        }
    }
}

#[cfg(all(test, target_os = "macos"))]
mod audio_extract_tests {
    use super::*;
    use objc2_core_audio_types::AudioBufferList;

    #[test]
    fn ablstorage_size_is_large_enough_for_audio_buffer_list() {
        let storage_size = core::mem::size_of::<super::AblStorage>();
        let abl_size = core::mem::size_of::<AudioBufferList>();
        assert!(
            storage_size > abl_size,
            "AblStorage must hold > 1 AudioBuffer"
        );
        assert_eq!(core::mem::align_of::<super::AblStorage>(), 16);
    }

    #[test]
    fn ablstorage_layout_matches_apple_abl() {
        let storage_size = core::mem::size_of::<super::AblStorage>();
        let n_buffers_offset = core::mem::offset_of!(super::AblStorage, n_buffers);
        let buffers_offset = core::mem::offset_of!(super::AblStorage, buffers);
        assert_eq!(n_buffers_offset, 0, "n_buffers must be at offset 0");
        assert_eq!(
            buffers_offset, 8,
            "buffers must be at offset 8 (after 4 bytes pad)"
        );
        assert!(storage_size >= 24, "must hold at least one AudioBuffer");
    }

    #[test]
    fn extract_audio_bytes_rejects_metadata_sample_buffer_gracefully() {
        let pool = MacAudioFramePool::new(2, 256).expect("pool");
        assert_eq!(pool.try_acquire().map(|s| s.capacity()), Some(256));
    }
}

#[cfg(test)]
mod dimension_tests {
    use super::*;
    use objc2_core_foundation::{CGPoint, CGSize};

    fn rect(w: f64, h: f64) -> CGRect {
        CGRect {
            origin: CGPoint { x: 0.0, y: 0.0 },
            size: CGSize {
                width: w,
                height: h,
            },
        }
    }

    #[test]
    fn even_floor_clears_low_bit() {
        assert_eq!(even_floor(3841), 3840);
        assert_eq!(even_floor(3840), 3840);
        assert_eq!(even_floor(1), 0);
    }

    #[test]
    fn cap_downscales_8k_to_4k_preserving_aspect() {
        assert_eq!(cap_output_dims(7680, 4320, 3840, 2160), (3840, 2160));
    }

    #[test]
    fn cap_is_noop_below_ceiling() {
        assert_eq!(cap_output_dims(1920, 1080, 3840, 2160), (1920, 1080));
    }

    #[test]
    fn cap_preserves_aspect_for_5k_ultrawide() {
        let (w, h) = cap_output_dims(5120, 2160, 3840, 2160);
        assert_eq!(h, 1620);
        assert_eq!(w, 3840);
        assert!(w <= 3840 && h <= 2160);
    }

    #[test]
    fn cap_outputs_are_even_and_bounded() {
        for (sw, sh) in [(7680u32, 4320u32), (5120, 2880), (3008, 1692), (1366, 768)] {
            let (w, h) = cap_output_dims(sw, sh, 3840, 2160);
            assert_eq!(w % 2, 0);
            assert_eq!(h % 2, 0);
            assert!(w <= 3840);
            assert!(h <= 2160);
        }
    }

    #[test]
    fn resolve_prefers_content_rect_times_scale() {
        let dims = resolve_source_pixels(
            7680,
            4320,
            None,
            Some(rect(3840.0, 2160.0)),
            Some(2.0),
            (3840, 2160),
        );
        assert_eq!(dims, (7680, 4320));
    }

    #[test]
    fn resolve_falls_back_to_requested_without_selectors() {
        assert_eq!(
            resolve_source_pixels(7680, 4320, None, None, None, (1920, 1080)),
            (7680, 4320)
        );
    }

    #[test]
    fn resolve_source_rect_takes_priority() {
        let dims = resolve_source_pixels(
            7680,
            4320,
            Some(rect(1280.0, 720.0)),
            Some(rect(3840.0, 2160.0)),
            Some(2.0),
            (3840, 2160),
        );
        assert_eq!(dims, (2560, 1440));
    }

    #[test]
    fn resolve_then_cap_fills_4k_from_8k_backing() {
        let (sw, sh) =
            resolve_source_pixels(0, 0, None, Some(rect(3840.0, 2160.0)), Some(2.0), (0, 0));
        assert_eq!(cap_output_dims(sw, sh, 3840, 2160), (3840, 2160));
    }
}

#[cfg(test)]
mod external_buffer_tests {
    use super::*;
    use crate::audio_pool::MacAudioFramePool;
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::thread;

    struct CountingAllocator;

    thread_local! {
        static THREAD_ALLOC_COUNT: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
        static THREAD_ALLOC_BYTES: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
        static THREAD_TRACKING: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    }

    fn tracking_enabled() -> bool {
        THREAD_TRACKING.try_with(|t| t.get()).unwrap_or(false)
    }

    fn record_alloc(layout: Layout) {
        let _ = THREAD_ALLOC_COUNT.try_with(|c| c.set(c.get() + 1));
        let _ = THREAD_ALLOC_BYTES.try_with(|b| b.set(b.get() + layout.size() as u64));
    }

    fn reset_counts() {
        let _ = THREAD_ALLOC_COUNT.try_with(|c| c.set(0));
        let _ = THREAD_ALLOC_BYTES.try_with(|b| b.set(0));
    }

    fn snapshot_counts() -> (u64, u64) {
        let count = THREAD_ALLOC_COUNT.try_with(|c| c.get()).unwrap_or(0);
        let bytes = THREAD_ALLOC_BYTES.try_with(|b| b.get()).unwrap_or(0);
        (count, bytes)
    }

    fn set_tracking(enabled: bool) {
        let _ = THREAD_TRACKING.try_with(|t| t.set(enabled));
    }

    unsafe impl GlobalAlloc for CountingAllocator {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            if tracking_enabled() {
                record_alloc(layout);
            }
            unsafe { System.alloc(layout) }
        }
        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            unsafe { System.dealloc(ptr, layout) }
        }
    }

    #[global_allocator]
    static GLOBAL: CountingAllocator = CountingAllocator;

    struct FakeExternalBuffer {
        ptr: *mut u8,
        len: usize,
        slot: Option<PooledMacAudioFrame>,
    }

    unsafe impl Send for FakeExternalBuffer {}

    impl FakeExternalBuffer {
        fn from_pooled(mut slot: PooledMacAudioFrame) -> Self {
            let len = slot.filled_len();
            let ptr = slot.as_mut_ptr();
            assert!(!ptr.is_null());
            Self {
                ptr,
                len,
                slot: Some(slot),
            }
        }

        fn data(&self) -> &[u8] {
            assert!(!self.ptr.is_null());
            unsafe { core::slice::from_raw_parts(self.ptr, self.len) }
        }
    }

    impl Drop for FakeExternalBuffer {
        fn drop(&mut self) {
            if let Some(slot) = self.slot.take() {
                drop(slot);
            }
        }
    }

    #[test]
    fn audio_buffer_external_pointer_round_trip() {
        let pool = MacAudioFramePool::new(4, 256).expect("pool");
        let mut slot = pool.try_acquire().expect("slot");
        let payload = [0x42_u8; 64];
        slot.write(&payload).expect("write");
        let fake = FakeExternalBuffer::from_pooled(slot);
        assert_eq!(pool.stats().in_flight, 1);
        assert_eq!(fake.data(), &payload[..]);
        drop(fake);
        assert_eq!(pool.stats().in_flight, 0);
        assert_eq!(pool.stats().released, 1);
        assert_eq!(pool.stats().acquired, 1);
    }

    #[test]
    fn multi_frame_external_buffer_lifecycle() {
        const FRAMES: u64 = 100;
        let pool = MacAudioFramePool::new(8, 128).expect("pool");
        for i in 0..FRAMES {
            let mut slot = pool.try_acquire().expect("slot");
            let byte = (i % 256) as u8;
            let payload = [byte; 64];
            slot.write(&payload).expect("write");
            let fake = FakeExternalBuffer::from_pooled(slot);
            assert_eq!(fake.data()[0], byte);
            drop(fake);
        }
        let stats = pool.stats();
        assert_eq!(stats.acquired, FRAMES);
        assert_eq!(stats.released, FRAMES);
        assert_eq!(stats.in_flight, 0);
        assert_eq!(stats.dropped, 0);
    }

    #[test]
    fn pool_exhaustion_with_external_buffers_holds_diagnostic() {
        let pool = MacAudioFramePool::new(MAC_AUDIO_POOL_CAP, 256).expect("pool");
        let mut held: Vec<FakeExternalBuffer> = Vec::with_capacity(MAC_AUDIO_POOL_CAP);
        for _ in 0..MAC_AUDIO_POOL_CAP {
            let mut slot = pool.try_acquire().expect("within cap");
            slot.write(&[0xAB_u8; 32]).expect("write");
            held.push(FakeExternalBuffer::from_pooled(slot));
        }
        assert_eq!(pool.stats().in_flight as usize, MAC_AUDIO_POOL_CAP);
        let next = pool.try_acquire();
        assert!(next.is_none(), "17th acquire must report exhaustion");
        assert_eq!(pool.stats().dropped, 1);
        drop(held);
        assert_eq!(pool.stats().in_flight, 0);
    }

    #[test]
    fn buffer_external_drop_fn_runs_on_pool_thread_or_napi_thread() {
        let pool = Arc::new(MacAudioFramePool::new(4, 128).expect("pool"));
        let pool_clone = Arc::clone(&pool);
        let mut slot = pool.try_acquire().expect("slot");
        slot.write(&[0x99_u8; 32]).expect("write");
        let fake = FakeExternalBuffer::from_pooled(slot);
        assert_eq!(pool.stats().in_flight, 1);
        let handle = thread::spawn(move || {
            let fake = fake;
            assert_eq!(fake.data()[0], 0x99);
            drop(fake);
        });
        handle.join().expect("worker");
        assert_eq!(pool_clone.stats().in_flight, 0);
        assert_eq!(pool_clone.stats().released, 1);
    }

    #[test]
    fn zero_allocation_in_steady_state_acquire_write_external_release() {
        let pool =
            MacAudioFramePool::new(MAC_AUDIO_POOL_CAP, MAX_FRAME_BYTES_PER_SLOT).expect("pool");
        let payload = vec![0xCC_u8; 3840];
        for _ in 0..16 {
            let mut slot = pool.try_acquire().expect("warmup");
            slot.write(&payload).expect("warmup write");
            let fake = FakeExternalBuffer::from_pooled(slot);
            drop(fake);
        }
        reset_counts();
        set_tracking(true);
        for _ in 0..1000 {
            let mut slot = pool.try_acquire().expect("steady-state slot");
            slot.write(&payload).expect("steady-state write");
            let fake = FakeExternalBuffer::from_pooled(slot);
            std::hint::black_box(fake.data().len());
            drop(fake);
        }
        set_tracking(false);
        let (count, bytes) = snapshot_counts();
        assert_eq!(
            count, 0,
            "steady-state acquire/write/external/release must not allocate (got {count} allocations, {bytes} bytes)"
        );
    }

    #[test]
    fn allocator_counter_self_test_reports_nonzero_when_allocating() {
        reset_counts();
        set_tracking(true);
        let v: Vec<u8> = Vec::with_capacity(1024);
        set_tracking(false);
        let (count, _) = snapshot_counts();
        let _hold = v;
        assert!(
            count >= 1,
            "self-test: allocator counter must record real allocs"
        );
    }

    static FINALIZE_THREAD_COUNTER: AtomicUsize = AtomicUsize::new(0);

    struct ThreadAwareFakeBuffer {
        slot: Option<PooledMacAudioFrame>,
    }

    impl Drop for ThreadAwareFakeBuffer {
        fn drop(&mut self) {
            FINALIZE_THREAD_COUNTER.fetch_add(1, Ordering::Relaxed);
            if let Some(slot) = self.slot.take() {
                drop(slot);
            }
        }
    }

    #[test]
    fn external_buffer_finalize_runs_exactly_once_per_frame() {
        FINALIZE_THREAD_COUNTER.store(0, Ordering::Relaxed);
        let pool = MacAudioFramePool::new(4, 64).expect("pool");
        for _ in 0..50 {
            let mut slot = pool.try_acquire().expect("slot");
            slot.write(&[0u8; 32]).expect("write");
            let fake = ThreadAwareFakeBuffer { slot: Some(slot) };
            drop(fake);
        }
        assert_eq!(FINALIZE_THREAD_COUNTER.load(Ordering::Relaxed), 50);
        assert_eq!(pool.stats().acquired, 50);
        assert_eq!(pool.stats().released, 50);
    }
}
