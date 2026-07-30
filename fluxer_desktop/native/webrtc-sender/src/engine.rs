// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::audio;
use crate::camera;
use crate::deep_filter::{self, DeepFilterProcessor};
use crate::events;
use crate::inbound_forwarder::{InboundForwarderRegistry, spawn_drain_forwarder};
use crate::send_control::{
    AdaptiveAudioStats, AdaptiveVideoStats, DEFAULT_AUDIO_BUFFER_MAX_MS, DEFAULT_MIN_VIDEO_FPS,
    SendHealthSnapshot, VideoTelemetryExtras,
};
use crate::speaking::{
    self, SPEAKING_FRAME_TIMEOUT_MS, SPEAKING_HEARTBEAT_INTERVAL_MS, SPEAKING_RELEASE_MS_LOCAL,
    SPEAKING_RELEASE_MS_REMOTE, SpeakingGate, SpeakingThresholds,
};
use crate::stats as stats_mod;
#[cfg(target_os = "windows")]
use crate::texture_source::TextureFrameDesc;
use crate::texture_source::{self, TextureCapability};
use crate::yuv;
use crossbeam_queue::ArrayQueue;
use fluxer_screen_frame_bus::{
    self as frame_bus, EnqueueOutcome, NativeScreenFrameSinkHandle, ScreenFrame as BusScreenFrame,
    ScreenFrameSink,
};
use napi::Status;
use napi::bindgen_prelude::{Buffer, Env, Function, ToNapiValue, Unknown};
use napi::threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode};
use napi_derive::napi;
use parking_lot::Mutex;
use std::collections::HashMap;
use std::ffi::c_void;
#[cfg(target_os = "linux")]
use std::os::fd::{AsRawFd, OwnedFd};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::sync::atomic::{AtomicBool, AtomicI64, AtomicU8, AtomicU64, Ordering};
use std::sync::{Arc, LazyLock, mpsc};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use futures_util::{FutureExt, StreamExt};
use livekit::DataPacket;
use livekit::e2ee::key_provider::{KeyProvider, KeyProviderOptions};
use livekit::e2ee::{E2eeOptions, EncryptionType};
use livekit::id::{ParticipantIdentity, TrackSid};
use livekit::options::{AudioEncoding, TrackPublishOptions, VideoCodec, VideoEncoding};
use livekit::participant::{LocalParticipant, RemoteParticipant};
use livekit::rtc_engine::lk_runtime::LkRuntime;
use livekit::track::{
    LocalAudioTrack, LocalTrack, LocalVideoTrack, RemoteTrack, TrackKind, TrackSource, VideoQuality,
};
use livekit::webrtc::MediaType;
use livekit::webrtc::audio_frame::AudioFrame;
use livekit::webrtc::audio_source::native::NativeAudioSource;
use livekit::webrtc::audio_source::{AudioSourceOptions, RtcAudioSource};
use livekit::webrtc::audio_stream::native::NativeAudioStream;
use livekit::webrtc::peer_connection_factory::PeerConnectionFactory;
use livekit::webrtc::peer_connection_factory::native::PeerConnectionFactoryExt;
use livekit::webrtc::prelude::RtcAudioTrack;
use livekit::webrtc::stats::RtcStats;
#[cfg(target_os = "macos")]
use livekit::webrtc::video_frame::native::NativeBuffer;
use livekit::webrtc::video_frame::{
    BoxVideoFrame, I420Buffer, NV12Buffer, VideoBuffer, VideoFrame, VideoRotation,
};
use livekit::webrtc::video_source::native::NativeVideoSource;
use livekit::webrtc::video_source::{RtcVideoSource, VideoResolution};
use livekit::webrtc::video_stream::native::NativeVideoStream;
use livekit::{PlatformAudio, RecordingDeviceId, Room, RoomEvent, RoomOptions};
use tokio::sync::{Notify, watch};

const S_IDLE: u8 = 0;
const S_CONNECTING: u8 = 1;
const S_CONNECTED: u8 = 2;
const S_CLOSED: u8 = 3;
const S_FAILED: u8 = 4;

const CONNECTION_STATES: [u8; 5] = [S_IDLE, S_CONNECTING, S_CONNECTED, S_CLOSED, S_FAILED];

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ConnectAdmission {
    Proceed,
    Superseded,
}

fn admit_connect_attempt(intent: u64, latest_intent: u64) -> ConnectAdmission {
    assert!(intent >= 1);
    assert!(latest_intent >= intent);
    if latest_intent == intent {
        ConnectAdmission::Proceed
    } else {
        ConnectAdmission::Superseded
    }
}

async fn wait_connect_cancelled(mut cancel_rx: watch::Receiver<u64>, connect_epoch: u64) {
    assert!(connect_epoch >= 1);
    let observed_epoch = *cancel_rx.borrow();
    if observed_epoch >= connect_epoch {
        return;
    }
    if cancel_rx.changed().await.is_ok() {
        let observed_epoch = *cancel_rx.borrow();
        assert!(observed_epoch >= connect_epoch);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RoomLoopAction {
    Forward,
    Exit,
}

fn room_event_loop_action(loop_epoch: u64, engine_epoch: u64) -> RoomLoopAction {
    assert!(loop_epoch >= 1);
    assert!(engine_epoch >= loop_epoch);
    if engine_epoch == loop_epoch {
        RoomLoopAction::Forward
    } else {
        RoomLoopAction::Exit
    }
}

fn connection_state_transition_valid(from: u8, to: u8) -> bool {
    assert!(CONNECTION_STATES.contains(&from));
    assert!(CONNECTION_STATES.contains(&to));
    matches!(
        (from, to),
        (S_IDLE, S_CLOSED)
            | (S_CLOSED, S_CONNECTING)
            | (S_CLOSED, S_CLOSED)
            | (S_CONNECTING, S_CONNECTED)
            | (S_CONNECTING, S_FAILED)
            | (S_CONNECTING, S_CLOSED)
            | (S_CONNECTED, S_CLOSED)
            | (S_FAILED, S_CLOSED)
    )
}

fn store_connection_state(state: &AtomicU8, next: u8) {
    let previous = state.swap(next, Ordering::AcqRel);
    assert!(connection_state_transition_valid(previous, next));
}

fn store_room_loop_closed(
    state_guard: &Mutex<()>,
    connect_epoch: &AtomicU64,
    state: &AtomicU8,
    loop_epoch: u64,
) {
    let _guard = state_guard.lock();
    let engine_epoch = connect_epoch.load(Ordering::Acquire);
    if room_event_loop_action(loop_epoch, engine_epoch) == RoomLoopAction::Exit {
        return;
    }
    store_connection_state(state, S_CLOSED);
}

const EVENT_QUEUE_LIMIT: usize = 64;
const CAMERA_PREVIEW_TRACK_SID: &str = "local-camera-preview";
const SCREEN_SHARE_AUDIO_MAX_BITRATE_BPS: u64 = 510_000;
const PLATFORM_RECORDING_DISABLE_REASSERT_DELAY_MS: u64 = 200;
const DEEP_FILTER_FIRST_FRAME_TIMEOUT_MS: u64 = 2_000;
const DEEP_FILTER_FRAME_TIMEOUT_MS: u64 = 1_000;
const DEEP_FILTER_READINESS_POLL_MS: u64 = 20;
const DEEP_FILTER_SOURCE_QUEUE_MS: u32 = 0;
const DEEP_FILTER_PIPE_QUEUE_FRAMES: usize = 8;
const MIN_AUDIO_SAMPLE_RATE_HZ: u32 = 8_000;
const MAX_AUDIO_SAMPLE_RATE_HZ: u32 = 192_000;
const MAX_AUDIO_CHANNELS: u32 = 8;
const MAX_PCM_FRAME_DURATION_MS: u64 = 1_000;
const MAX_PCM_FRAME_SAMPLES: usize = (MAX_AUDIO_SAMPLE_RATE_HZ as usize)
    * (MAX_AUDIO_CHANNELS as usize)
    * (MAX_PCM_FRAME_DURATION_MS as usize)
    / 1_000;
const ENCODER_QUEUE_CAPACITY: usize = 8;
const FPS_PACING_FALLBACK: f64 = 60.0;
const LIVEKIT_TRACK_SOURCE_CAMERA: &str = "camera";
const LIVEKIT_TRACK_SOURCE_SCREEN_SHARE: &str = "screen_share";
const LIVEKIT_TRACK_SOURCE_SCREEN_SHARE_AUDIO: &str = "screen_share_audio";
const REMOTE_TRACK_SUBSCRIPTION_PUBLICATIONS_MAX: usize = 64;
type EventTsfn = ThreadsafeFunction<
    (String, String),
    (),
    (String, String),
    napi::Status,
    false,
    true,
    EVENT_QUEUE_LIMIT,
>;

const VIDEO_FRAME_QUEUE_LIMIT: usize = 8;
type VideoFrameTsfn = ThreadsafeFunction<
    (String, Buffer),
    (),
    (String, Buffer),
    napi::Status,
    false,
    true,
    VIDEO_FRAME_QUEUE_LIMIT,
>;

static PREWARMED_LIVEKIT_RUNTIME: LazyLock<Mutex<Option<Arc<LkRuntime>>>> =
    LazyLock::new(|| Mutex::new(None));

struct ScreenSource {
    track_sid: TrackSid,
    video_sender: AdaptiveVideoSender,
    metadata: ScreenSourceMetadata,
    bus_capture_id: Option<String>,
    frame_sink: Option<Arc<BusSenderSink>>,
}

#[derive(Clone)]
struct ScreenSourceMetadata {
    track_sid: TrackSid,
    width: u32,
    height: u32,
    codec: String,
    target_bitrate_kbps: Option<f64>,
    configured_fps: f64,
}

type PcmScratch = Arc<tokio::sync::Mutex<Vec<i16>>>;

fn new_pcm_scratch(sample_rate: u32, num_channels: u32) -> PcmScratch {
    assert!(valid_audio_format(sample_rate, num_channels));
    let samples_max = (sample_rate as u64)
        .saturating_mul(u64::from(num_channels))
        .saturating_mul(MAX_PCM_FRAME_DURATION_MS)
        / 1_000;
    assert!(samples_max <= MAX_PCM_FRAME_SAMPLES as u64);
    Arc::new(tokio::sync::Mutex::new(Vec::with_capacity(
        samples_max as usize,
    )))
}

struct ScreenAudioSource {
    source: NativeAudioSource,
    sample_rate: u32,
    num_channels: u32,
    track_sid: TrackSid,
    pcm_scratch: PcmScratch,
    drain_stop: Arc<AtomicBool>,
    ring: Arc<ScreenAudioRing>,
}

impl ScreenAudioSource {
    fn matches_format(&self, sample_rate: u32, num_channels: u32) -> bool {
        same_audio_format(
            self.sample_rate,
            self.num_channels,
            sample_rate,
            num_channels,
        )
    }
}

impl Drop for ScreenAudioSource {
    fn drop(&mut self) {
        self.drain_stop.store(true, Ordering::Release);
        self.ring.wake.notify_one();
    }
}

fn same_audio_format(
    existing_sample_rate: u32,
    existing_num_channels: u32,
    sample_rate: u32,
    num_channels: u32,
) -> bool {
    existing_sample_rate == sample_rate && existing_num_channels == num_channels
}

const SCREEN_AUDIO_RING_CAP: usize = 32;
const SCREEN_AUDIO_RECYCLE_CAP: usize = 32;
const SCREEN_AUDIO_CHUNK_F32_MAX: usize = 16_384;

struct ScreenAudioChunk {
    samples: Vec<f32>,
    num_frames: u32,
    channels: u32,
    sample_rate_hz: u32,
}

struct ScreenAudioRing {
    filled: ArrayQueue<ScreenAudioChunk>,
    recycled: ArrayQueue<Vec<f32>>,
    dropped: AtomicU64,
    wake: Notify,
}

impl ScreenAudioRing {
    fn new() -> Self {
        Self {
            filled: ArrayQueue::new(SCREEN_AUDIO_RING_CAP),
            recycled: ArrayQueue::new(SCREEN_AUDIO_RECYCLE_CAP),
            dropped: AtomicU64::new(0),
            wake: Notify::new(),
        }
    }

    fn take_buffer(&self) -> Vec<f32> {
        self.recycled
            .pop()
            .unwrap_or_else(|| Vec::with_capacity(SCREEN_AUDIO_CHUNK_F32_MAX))
    }

    fn recycle(&self, mut buffer: Vec<f32>) {
        buffer.clear();
        let _ = self.recycled.push(buffer);
    }

    fn push(&self, chunk: ScreenAudioChunk) {
        if let Err(returned) = self.filled.push(chunk) {
            if let Some(old) = self.filled.pop() {
                self.dropped.fetch_add(1, Ordering::Relaxed);
                self.recycle(old.samples);
            }
            if let Err(dropped) = self.filled.push(returned) {
                self.dropped.fetch_add(1, Ordering::Relaxed);
                self.recycle(dropped.samples);
            }
        }
        self.wake.notify_one();
    }
}

fn screen_audio_chunk_as_bytes(samples: &[f32]) -> &[u8] {
    unsafe {
        std::slice::from_raw_parts(
            samples.as_ptr() as *const u8,
            std::mem::size_of_val(samples),
        )
    }
}

async fn forward_screen_audio_chunk(
    source: &NativeAudioSource,
    scratch: &PcmScratch,
    chunk: &ScreenAudioChunk,
) {
    assert!(chunk.channels > 0);
    let expected_samples = (chunk.num_frames as usize).checked_mul(chunk.channels as usize);
    if expected_samples != Some(chunk.samples.len()) {
        return;
    }
    let bytes = screen_audio_chunk_as_bytes(&chunk.samples);
    let mut samples = scratch.lock().await;
    if let Some(frame) =
        f32_audio_frame_into(bytes, chunk.sample_rate_hz, chunk.channels, &mut samples)
    {
        let _ = source.capture_frame(&frame).await;
    }
}

fn spawn_screen_audio_drain(
    ring: Arc<ScreenAudioRing>,
    source: NativeAudioSource,
    scratch: PcmScratch,
    drain_stop: Arc<AtomicBool>,
) {
    tokio::spawn(async move {
        loop {
            while let Some(chunk) = ring.filled.pop() {
                forward_screen_audio_chunk(&source, &scratch, &chunk).await;
                ring.recycle(chunk.samples);
            }
            if drain_stop.load(Ordering::Acquire) {
                break;
            }
            ring.wake.notified().await;
        }
    });
}

enum MicSource {
    Device {
        track_sid: TrackSid,
        track: LocalAudioTrack,
    },
    DeviceDeepFiltered {
        track_sid: TrackSid,
        track: LocalAudioTrack,
        capture_track: LocalAudioTrack,
        _tap_guard: RecordedAudioTapGuard,
        _pipe_stop: DeepFilterPipeStop,
    },
    Manual {
        source: NativeAudioSource,
        sample_rate: u32,
        num_channels: u32,
        track_sid: TrackSid,
        track: LocalAudioTrack,
        pcm_scratch: PcmScratch,
    },
}

impl MicSource {
    fn track_sid(&self) -> TrackSid {
        match self {
            Self::Device { track_sid, .. }
            | Self::DeviceDeepFiltered { track_sid, .. }
            | Self::Manual { track_sid, .. } => track_sid.clone(),
        }
    }

    fn track_sid_mut(&mut self) -> &mut TrackSid {
        match self {
            Self::Device { track_sid, .. }
            | Self::DeviceDeepFiltered { track_sid, .. }
            | Self::Manual { track_sid, .. } => track_sid,
        }
    }

    fn track(&self) -> LocalAudioTrack {
        match self {
            Self::Device { track, .. }
            | Self::DeviceDeepFiltered { track, .. }
            | Self::Manual { track, .. } => track.clone(),
        }
    }

    fn capture_track(&self) -> Option<LocalAudioTrack> {
        match self {
            Self::DeviceDeepFiltered { capture_track, .. } => Some(capture_track.clone()),
            Self::Device { .. } | Self::Manual { .. } => None,
        }
    }

    fn is_device(&self) -> bool {
        matches!(self, Self::Device { .. } | Self::DeviceDeepFiltered { .. })
    }
}

struct DeepFilterPipeStop {
    stop: Arc<AtomicBool>,
}

impl Drop for DeepFilterPipeStop {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Release);
    }
}

struct DeepFilterMicrophone {
    track: LocalAudioTrack,
    capture_track: LocalAudioTrack,
    pipe_stop: DeepFilterPipeStop,
    tap_guard: RecordedAudioTapGuard,
}

#[derive(Clone, Copy)]
struct DeepFilterCaptureFrame {
    samples: [i16; deep_filter::DEEP_FILTER_FRAME_SAMPLES],
}

struct RecordedAudioTapGuard {
    generation: u64,
}

impl Drop for RecordedAudioTapGuard {
    fn drop(&mut self) {
        livekit::webrtc::recorded_audio::native::clear_recorded_audio_sink(self.generation);
    }
}

struct DeepFilterPipeParts {
    source: NativeAudioSource,
    pipe_stop: DeepFilterPipeStop,
    diagnostics: DeepFilterDiagnostics,
    frame_sender: mpsc::SyncSender<DeepFilterCaptureFrame>,
    ready: tokio::sync::oneshot::Receiver<Result<(), String>>,
}

#[derive(Clone)]
struct DeepFilterDiagnostics {
    degraded_frames: Arc<AtomicU64>,
    events: Arc<Mutex<Option<EventTsfn>>>,
    dropped_engine_events: Arc<AtomicU64>,
}

impl DeepFilterDiagnostics {
    fn record_degraded_frame(&self, detail: &str) {
        let previous = self.degraded_frames.fetch_add(1, Ordering::Relaxed);
        if previous == 0 {
            emit_deep_filter_status(
                &self.events,
                &self.dropped_engine_events,
                "degraded",
                detail,
            );
        }
    }
}

fn spawn_deep_filter_processing_thread(
    noise_reduction_level: f64,
    source: NativeAudioSource,
    stop: Arc<AtomicBool>,
    diagnostics: DeepFilterDiagnostics,
    frame_receiver: mpsc::Receiver<DeepFilterCaptureFrame>,
) -> Result<tokio::sync::oneshot::Receiver<Result<(), String>>, String> {
    assert!(!stop.load(Ordering::Acquire));
    assert_eq!(diagnostics.degraded_frames.load(Ordering::Acquire), 0);
    let (ready_sender, ready_receiver) = tokio::sync::oneshot::channel();
    std::thread::Builder::new()
        .name("fluxer-deep-filter-mic".to_string())
        .spawn(move || {
            let processor = match DeepFilterProcessor::new(noise_reduction_level) {
                Ok(processor) => processor,
                Err(error) => {
                    let _ = ready_sender.send(Err(error));
                    return;
                }
            };
            if ready_sender.send(Ok(())).is_err() {
                return;
            }
            run_deep_filter_processing(processor, source, stop, diagnostics, frame_receiver);
        })
        .map_err(|error| format!("spawn deep filter thread: {error}"))?;
    Ok(ready_receiver)
}

fn run_deep_filter_processing(
    mut processor: DeepFilterProcessor,
    source: NativeAudioSource,
    stop: Arc<AtomicBool>,
    diagnostics: DeepFilterDiagnostics,
    frame_receiver: mpsc::Receiver<DeepFilterCaptureFrame>,
) {
    loop {
        if stop.load(Ordering::Acquire) {
            break;
        }
        let received =
            frame_receiver.recv_timeout(Duration::from_millis(DEEP_FILTER_FRAME_TIMEOUT_MS));
        match received {
            Ok(mut frame) => {
                process_and_capture_deep_filter_frame(
                    &mut processor,
                    &source,
                    &diagnostics,
                    &mut frame.samples,
                );
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {}
            Err(mpsc::RecvTimeoutError::Disconnected) => break,
        }
    }
}

fn process_and_capture_deep_filter_frame(
    processor: &mut DeepFilterProcessor,
    source: &NativeAudioSource,
    diagnostics: &DeepFilterDiagnostics,
    samples: &mut [i16; deep_filter::DEEP_FILTER_FRAME_SAMPLES],
) {
    assert_eq!(samples.len(), deep_filter::DEEP_FILTER_FRAME_SAMPLES);
    if let Err(error) = processor.process_frame(samples) {
        diagnostics.record_degraded_frame(&error);
    }
    let processed = AudioFrame {
        data: (&samples[..]).into(),
        sample_rate: deep_filter::DEEP_FILTER_SAMPLE_RATE_HZ,
        num_channels: deep_filter::DEEP_FILTER_NUM_CHANNELS,
        samples_per_channel: deep_filter::DEEP_FILTER_FRAME_SAMPLES as u32,
    };
    match source.capture_frame(&processed).now_or_never() {
        Some(Ok(())) => {}
        Some(Err(error)) => {
            diagnostics.record_degraded_frame(&format!("capture frame: {error}"));
        }
        None => {
            diagnostics.record_degraded_frame("capture frame did not complete synchronously");
        }
    }
}

fn install_deep_filter_capture_tap(
    frame_sender: mpsc::SyncSender<DeepFilterCaptureFrame>,
    diagnostics: DeepFilterDiagnostics,
    capture_count: Arc<AtomicU64>,
) -> u64 {
    livekit::webrtc::recorded_audio::native::set_recorded_audio_sink(
        move |data: &[i16], sample_rate: i32, channels: usize, frames: usize| {
            if sample_rate != deep_filter::DEEP_FILTER_SAMPLE_RATE_HZ as i32 {
                diagnostics.record_degraded_frame("unexpected capture sample rate");
                return;
            }
            if channels != deep_filter::DEEP_FILTER_NUM_CHANNELS as usize {
                diagnostics.record_degraded_frame("unexpected capture channel count");
                return;
            }
            if frames != deep_filter::DEEP_FILTER_FRAME_SAMPLES {
                diagnostics.record_degraded_frame("unexpected capture frame length");
                return;
            }
            if data.len() != deep_filter::DEEP_FILTER_FRAME_SAMPLES {
                diagnostics.record_degraded_frame("unexpected capture buffer length");
                return;
            }
            let mut frame = DeepFilterCaptureFrame {
                samples: [0i16; deep_filter::DEEP_FILTER_FRAME_SAMPLES],
            };
            frame.samples.copy_from_slice(data);
            match frame_sender.try_send(frame) {
                Ok(()) => {
                    capture_count.fetch_add(1, Ordering::Relaxed);
                }
                Err(mpsc::TrySendError::Full(_)) => {
                    diagnostics.record_degraded_frame("processing queue full");
                }
                Err(mpsc::TrySendError::Disconnected(_)) => {}
            }
        },
    )
}

async fn await_deep_filter_capture_started(capture_count: &AtomicU64) -> bool {
    let max_polls = DEEP_FILTER_FIRST_FRAME_TIMEOUT_MS / DEEP_FILTER_READINESS_POLL_MS;
    assert!(max_polls > 0);
    assert!(DEEP_FILTER_READINESS_POLL_MS > 0);
    for _ in 0..max_polls {
        if capture_count.load(Ordering::Acquire) > 0 {
            return true;
        }
        tokio::time::sleep(Duration::from_millis(DEEP_FILTER_READINESS_POLL_MS)).await;
    }
    capture_count.load(Ordering::Acquire) > 0
}

fn emit_deep_filter_status(
    events: &Mutex<Option<EventTsfn>>,
    dropped_engine_events: &AtomicU64,
    status: &str,
    detail: &str,
) {
    assert!(!status.is_empty());
    let payload = events::json_object(&[
        ("status", events::JsonValue::Str(status.to_string())),
        ("detail", events::JsonValue::Str(detail.to_string())),
    ]);
    emit_engine_event(
        events,
        dropped_engine_events,
        "deepFilterStatus".to_string(),
        payload,
    );
}

#[derive(Clone)]
struct DeviceCameraCapture {
    source: NativeVideoSource,
    participant_sid: String,
    participant_identity: String,
    track_name: String,
    track_source: String,
    request: camera::CameraRequest,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CameraSwapOrder {
    OpenNewThenStopOld,
    StopOldThenOpenNew,
}

fn camera_swap_order(
    current: &camera::CameraSelector,
    requested: &camera::CameraSelector,
) -> CameraSwapOrder {
    if current == requested {
        CameraSwapOrder::StopOldThenOpenNew
    } else {
        CameraSwapOrder::OpenNewThenStopOld
    }
}

enum CameraSource {
    Device {
        track_sid: Arc<Mutex<String>>,
        stop: Arc<AtomicBool>,
        capture: Option<DeviceCameraCapture>,
    },
    NativeBuffered {
        track_sid: Arc<Mutex<String>>,
        video_sender: AdaptiveVideoSender,
        frame_sink: Arc<BusSenderSink>,
    },
    Processed {
        track_sid: Arc<Mutex<String>>,
        video_sender: AdaptiveVideoSender,
    },
}

impl CameraSource {
    fn track_sid(&self) -> Arc<Mutex<String>> {
        match self {
            Self::Device { track_sid, .. }
            | Self::NativeBuffered { track_sid, .. }
            | Self::Processed { track_sid, .. } => track_sid.clone(),
        }
    }

    fn stop(&self) {
        match self {
            Self::Device { stop, .. } => stop.store(true, Ordering::Release),
            Self::NativeBuffered { video_sender, .. } | Self::Processed { video_sender, .. } => {
                video_sender.stop()
            }
        }
    }

    fn processed_sender(&self) -> Option<AdaptiveVideoSender> {
        match self {
            Self::Processed { video_sender, .. } => Some(video_sender.clone()),
            Self::Device { .. } | Self::NativeBuffered { .. } => None,
        }
    }

    fn native_frame_sink(&self) -> Option<Arc<BusSenderSink>> {
        match self {
            Self::NativeBuffered { frame_sink, .. } => Some(frame_sink.clone()),
            Self::Device { .. } | Self::Processed { .. } => None,
        }
    }
}

struct LocalTrackSlots<'a> {
    camera: &'a Mutex<Option<CameraSource>>,
    screen_camera: &'a Mutex<Option<CameraSource>>,
    screen: &'a Mutex<Option<ScreenSource>>,
    screen_audio: &'a Mutex<Option<ScreenAudioSource>>,
    mic: &'a Mutex<Option<MicSource>>,
}

fn apply_local_track_republish(
    slots: &LocalTrackSlots<'_>,
    previous_sid: &str,
    republished_sid: &str,
) -> bool {
    assert!(!previous_sid.is_empty());
    assert!(!republished_sid.is_empty());
    let mut matched: u32 = 0;
    for slot in [slots.camera, slots.screen_camera] {
        matched += republish_camera_slot_sid(slot, previous_sid, republished_sid);
    }
    matched += republish_screen_slot_sid(slots.screen, previous_sid, republished_sid);
    matched += republish_screen_audio_slot_sid(slots.screen_audio, previous_sid, republished_sid);
    matched += republish_mic_slot_sid(slots.mic, previous_sid, republished_sid);
    assert!(matched <= 1);
    matched == 1
}

fn republish_camera_slot_sid(
    slot: &Mutex<Option<CameraSource>>,
    previous_sid: &str,
    republished_sid: &str,
) -> u32 {
    let guard = slot.lock();
    let Some(source) = guard.as_ref() else {
        return 0;
    };
    let track_sid = source.track_sid();
    let mut sid = track_sid.lock();
    assert!(!sid.is_empty());
    if *sid == republished_sid {
        return 1;
    }
    if *sid != previous_sid {
        return 0;
    }
    *sid = republished_sid.to_string();
    1
}

fn republish_screen_slot_sid(
    slot: &Mutex<Option<ScreenSource>>,
    previous_sid: &str,
    republished_sid: &str,
) -> u32 {
    let mut guard = slot.lock();
    let Some(screen) = guard.as_mut() else {
        return 0;
    };
    republish_screen_sids(
        &mut screen.track_sid,
        &mut screen.metadata.track_sid,
        previous_sid,
        republished_sid,
    )
}

fn republish_screen_sids(
    track_sid: &mut TrackSid,
    metadata_track_sid: &mut TrackSid,
    previous_sid: &str,
    republished_sid: &str,
) -> u32 {
    assert_eq!(metadata_track_sid, track_sid);
    let matched = republish_track_sid_value(track_sid, previous_sid, republished_sid);
    if matched == 1 {
        *metadata_track_sid = track_sid.clone();
    }
    matched
}

fn republish_screen_audio_slot_sid(
    slot: &Mutex<Option<ScreenAudioSource>>,
    previous_sid: &str,
    republished_sid: &str,
) -> u32 {
    let mut guard = slot.lock();
    let Some(audio) = guard.as_mut() else {
        return 0;
    };
    republish_track_sid_value(&mut audio.track_sid, previous_sid, republished_sid)
}

fn republish_mic_slot_sid(
    slot: &Mutex<Option<MicSource>>,
    previous_sid: &str,
    republished_sid: &str,
) -> u32 {
    let mut guard = slot.lock();
    let Some(mic) = guard.as_mut() else {
        return 0;
    };
    republish_track_sid_value(mic.track_sid_mut(), previous_sid, republished_sid)
}

fn republish_track_sid_value(
    track_sid: &mut TrackSid,
    previous_sid: &str,
    republished_sid: &str,
) -> u32 {
    assert!(!previous_sid.is_empty());
    assert!(!republished_sid.is_empty());
    if track_sid.as_str() == republished_sid {
        return 1;
    }
    if track_sid.as_str() != previous_sid {
        return 0;
    }
    *track_sid = match TrackSid::try_from(republished_sid.to_string()) {
        Ok(sid) => sid,
        Err(sid) => panic!("republished track sid is not a track sid: {sid}"),
    };
    1
}

fn store_camera_slot(slot: &Mutex<Option<CameraSource>>, source: CameraSource) {
    assert!(!source.track_sid().lock().is_empty());
    let previous = slot.lock().replace(source);
    assert!(previous.is_none());
}

fn remove_camera_slot_if_held(slot: &Mutex<Option<CameraSource>>, track_sid: &Arc<Mutex<String>>) {
    let mut guard = slot.lock();
    let held = guard
        .as_ref()
        .is_some_and(|source| Arc::ptr_eq(&source.track_sid(), track_sid));
    if held {
        *guard = None;
    }
}

struct OpenedCameraWorker {
    opened: camera::OpenedCamera,
    sinks_tx: std::sync::mpsc::Sender<camera::CameraCaptureSinks>,
    stop: Arc<AtomicBool>,
}

async fn open_camera_capture_worker(
    request: camera::CameraRequest,
) -> napi::Result<OpenedCameraWorker> {
    let stop = Arc::new(AtomicBool::new(false));
    let (result_tx, result_rx) = std::sync::mpsc::channel();
    let (sinks_tx, sinks_rx) = std::sync::mpsc::channel();
    camera::spawn_capture_worker(request, result_tx, sinks_rx, stop.clone());
    let opened = tokio::task::spawn_blocking(move || result_rx.recv())
        .await
        .map_err(|e| napi::Error::from_reason(format!("camera open task: {e}")))?
        .map_err(|_| napi::Error::from_reason("camera worker exited before open"))?
        .map_err(napi::Error::from_reason)?;
    assert!(opened.width >= 2);
    assert!(opened.height >= 2);
    Ok(OpenedCameraWorker {
        opened,
        sinks_tx,
        stop,
    })
}

fn commit_device_camera_swap(
    slot: &Mutex<Option<CameraSource>>,
    track_sid: &Arc<Mutex<String>>,
    expected_stop: &Arc<AtomicBool>,
    swapped_stop: &Arc<AtomicBool>,
    swapped_request: &camera::CameraRequest,
) -> bool {
    let mut guard = slot.lock();
    let Some(CameraSource::Device {
        track_sid: held_sid,
        stop,
        capture,
    }) = guard.as_mut()
    else {
        return false;
    };
    if !Arc::ptr_eq(held_sid, track_sid) {
        return false;
    }
    if !Arc::ptr_eq(stop, expected_stop) {
        return false;
    }
    *stop = swapped_stop.clone();
    if let Some(capture) = capture.as_mut() {
        capture.request = swapped_request.clone();
    }
    true
}

fn reconcile_camera_slot_sid(
    local: &LocalParticipant,
    track_sid: &Mutex<String>,
    track_source: TrackSource,
) {
    let stored = track_sid.lock().clone();
    assert!(!stored.is_empty());
    let publications = local.track_publications();
    if publications.keys().any(|sid| sid.as_str() == stored) {
        return;
    }
    let mut candidates = publications
        .values()
        .filter(|publication| publication.source() == track_source);
    let Some(candidate) = candidates.next() else {
        return;
    };
    if candidates.next().is_some() {
        return;
    }
    let republished_sid = candidate.sid().to_string();
    assert!(!republished_sid.is_empty());
    assert_ne!(republished_sid, stored);
    *track_sid.lock() = republished_sid;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CameraPublicationKind {
    Camera,
    ScreenShare,
}

impl CameraPublicationKind {
    fn track_name(self) -> &'static str {
        match self {
            Self::Camera => LIVEKIT_TRACK_SOURCE_CAMERA,
            Self::ScreenShare => LIVEKIT_TRACK_SOURCE_SCREEN_SHARE,
        }
    }

    fn track_source(self) -> TrackSource {
        match self {
            Self::Camera => TrackSource::Camera,
            Self::ScreenShare => TrackSource::Screenshare,
        }
    }

    fn stream(self) -> Option<&'static str> {
        match self {
            Self::Camera => None,
            Self::ScreenShare => Some(LIVEKIT_TRACK_SOURCE_SCREEN_SHARE),
        }
    }

    fn is_screencast(self) -> bool {
        match self {
            Self::Camera => false,
            Self::ScreenShare => true,
        }
    }
}

fn set_platform_adm_recording_enabled(enabled: bool) {
    LkRuntime::instance()
        .pc_factory()
        .set_adm_recording_enabled(enabled);
}

fn audio_device_count_result(reported: i16, label: &str) -> napi::Result<usize> {
    audio::bounded_audio_device_count(reported)
        .map_err(|error| napi::Error::from_reason(format!("{label}: {error}")))
}

fn collect_factory_playout_devices_from(
    factory: &PeerConnectionFactory,
) -> napi::Result<Vec<(String, String, usize)>> {
    let count = audio_device_count_result(factory.playout_devices(), "list audio output devices")?;
    let mut raw = Vec::with_capacity(count);
    for index in 0..count {
        assert!(index < audio::MAX_PLATFORM_AUDIO_DEVICES);
        let adm_index = u16::try_from(index).expect("bounded audio device index must fit u16");
        raw.push((
            factory.playout_device_guid(adm_index),
            factory.playout_device_name(adm_index),
            index,
        ));
    }
    assert!(raw.len() <= audio::MAX_PLATFORM_AUDIO_DEVICES);
    Ok(raw)
}

fn collect_factory_recording_devices_from(
    factory: &PeerConnectionFactory,
) -> napi::Result<Vec<(String, String, usize)>> {
    let count = audio_device_count_result(factory.recording_devices(), "list audio input devices")?;
    let mut raw = Vec::with_capacity(count);
    for index in 0..count {
        assert!(index < audio::MAX_PLATFORM_AUDIO_DEVICES);
        let adm_index = u16::try_from(index).expect("bounded audio device index must fit u16");
        raw.push((
            factory.recording_device_guid(adm_index),
            factory.recording_device_name(adm_index),
            index,
        ));
    }
    assert!(raw.len() <= audio::MAX_PLATFORM_AUDIO_DEVICES);
    Ok(raw)
}

fn collect_factory_playout_devices() -> napi::Result<Vec<(String, String, usize)>> {
    let runtime = LkRuntime::instance();
    collect_factory_playout_devices_from(runtime.pc_factory())
}

fn collect_factory_recording_devices() -> napi::Result<Vec<(String, String, usize)>> {
    let runtime = LkRuntime::instance();
    collect_factory_recording_devices_from(runtime.pc_factory())
}

fn set_factory_playout_device_by_guid(
    factory: &PeerConnectionFactory,
    guid: &str,
) -> napi::Result<()> {
    assert!(!guid.trim().is_empty());
    if factory.set_playout_device_by_guid(guid) {
        return Ok(());
    }
    Err(napi::Error::from_reason(
        "set audio output device: device not found",
    ))
}

fn hot_swap_factory_playout_device(
    factory: &PeerConnectionFactory,
    guid: &str,
) -> napi::Result<()> {
    assert!(!guid.trim().is_empty());
    if !factory.stop_playout() {
        return Err(napi::Error::from_reason(
            "set audio output device: stop_playout failed",
        ));
    }
    set_factory_playout_device_by_guid(factory, guid)?;
    if !factory.init_playout() {
        return Err(napi::Error::from_reason(
            "set audio output device: init_playout failed",
        ));
    }
    if !factory.start_playout() {
        return Err(napi::Error::from_reason(
            "set audio output device: start_playout failed",
        ));
    }
    Ok(())
}

#[napi]
#[cfg_attr(test, allow(dead_code))]
pub fn prewarm_voice_engine() -> napi::Result<()> {
    match catch_unwind(AssertUnwindSafe(prewarm_voice_engine_inner)) {
        Ok(result) => result,
        Err(_) => Err(napi::Error::from_reason(
            "native voice engine prewarm panicked",
        )),
    }
}

fn prewarm_voice_engine_inner() -> napi::Result<()> {
    let runtime = {
        let mut slot = PREWARMED_LIVEKIT_RUNTIME.lock();
        match slot.as_ref() {
            Some(runtime) => runtime.clone(),
            None => {
                let runtime = LkRuntime::instance();
                *slot = Some(runtime.clone());
                runtime
            }
        }
    };
    let factory = runtime.pc_factory();
    let _ = factory.get_rtp_sender_capabilities(MediaType::Audio);
    let _ = factory.get_rtp_sender_capabilities(MediaType::Video);
    let _ = factory.get_rtp_receiver_capabilities(MediaType::Audio);
    let _ = factory.get_rtp_receiver_capabilities(MediaType::Video);
    let audio_source = NativeAudioSource::new(
        AudioSourceOptions::default(),
        48_000,
        1,
        DEFAULT_AUDIO_BUFFER_MAX_MS as u32,
    );
    let _audio_track =
        LocalAudioTrack::create_audio_track("prewarm-audio", RtcAudioSource::Native(audio_source));
    if tokio::runtime::Handle::try_current().is_ok() {
        let video_source = NativeVideoSource::new(
            VideoResolution {
                width: 16,
                height: 16,
            },
            false,
        );
        let _video_track = LocalVideoTrack::create_video_track(
            "prewarm-video",
            RtcVideoSource::Native(video_source),
        );
    }
    Ok(())
}

#[napi]
#[cfg_attr(test, allow(dead_code))]
pub async fn probe_audio_device_module() -> napi::Result<bool> {
    Ok(true)
}

const ADM_DISPATCH_QUEUE_OPS_MAX: usize = 64;

static ADM_DISPATCH: LazyLock<mpsc::SyncSender<Box<dyn FnOnce() + Send>>> = LazyLock::new(|| {
    let (sender, receiver) =
        mpsc::sync_channel::<Box<dyn FnOnce() + Send>>(ADM_DISPATCH_QUEUE_OPS_MAX);
    std::thread::Builder::new()
        .name("fluxer-adm-dispatch".to_string())
        .spawn(move || {
            while let Ok(operation) = receiver.recv() {
                operation();
            }
        })
        .expect("spawn audio device module dispatch thread");
    sender
});

fn enqueue_adm_operation(operation: Box<dyn FnOnce() + Send>) -> Result<(), String> {
    match ADM_DISPATCH.try_send(operation) {
        Ok(()) => Ok(()),
        Err(mpsc::TrySendError::Full(_)) => {
            Err("audio device module dispatch queue full".to_string())
        }
        Err(mpsc::TrySendError::Disconnected(_)) => {
            Err("audio device module dispatch thread exited".to_string())
        }
    }
}

async fn run_audio_device_module_blocking<T, F>(operation: F) -> napi::Result<T>
where
    T: Send + 'static,
    F: FnOnce() -> napi::Result<T> + Send + 'static,
{
    let (result_sender, result_receiver) = tokio::sync::oneshot::channel();
    enqueue_adm_operation(Box::new(move || {
        let _ = result_sender.send(operation());
    }))
    .map_err(napi::Error::from_reason)?;
    result_receiver
        .await
        .map_err(|_| napi::Error::from_reason("audio device module operation dropped"))?
}

enum PendingVideoFrame {
    I420Native {
        buffer: I420Buffer,
        timestamp_us: i64,
        enqueued_at: Instant,
    },
    #[allow(dead_code)]
    Bgra {
        data: Vec<u8>,
        width: u32,
        height: u32,
        stride: u32,
        timestamp_us: i64,
        enqueued_at: Instant,
    },
    #[allow(dead_code)]
    Nv12 {
        data: Vec<u8>,
        width: u32,
        height: u32,
        stride_y: u32,
        stride_uv: u32,
        timestamp_us: i64,
        enqueued_at: Instant,
    },
    #[cfg(target_os = "windows")]
    Texture {
        desc: TextureFrameDesc,
        capability: TextureCapability,
        enqueued_at: Instant,
    },
    #[cfg(target_os = "macos")]
    MacCvPixelBuffer {
        buffer: NativeBuffer,
        timestamp_us: i64,
        enqueued_at: Instant,
    },
    #[cfg(target_os = "linux")]
    Dmabuf {
        desc: texture_source::DmabufFrameDesc,
        capability: TextureCapability,
        fds: Vec<OwnedFd>,
        enqueued_at: Instant,
    },
}

impl PendingVideoFrame {
    fn enqueued_at(&self) -> Instant {
        match self {
            Self::I420Native { enqueued_at, .. } => *enqueued_at,
            Self::Bgra { enqueued_at, .. } | Self::Nv12 { enqueued_at, .. } => *enqueued_at,
            #[cfg(target_os = "windows")]
            Self::Texture { enqueued_at, .. } => *enqueued_at,
            #[cfg(target_os = "macos")]
            Self::MacCvPixelBuffer { enqueued_at, .. } => *enqueued_at,
            #[cfg(target_os = "linux")]
            Self::Dmabuf { enqueued_at, .. } => *enqueued_at,
        }
    }
}

#[derive(Clone)]
struct AdaptiveVideoSender {
    pending: Arc<ArrayQueue<PendingVideoFrame>>,
    notify: Arc<Notify>,
    stop: Arc<AtomicBool>,
    stats: Arc<AdaptiveVideoStats>,
    pacing: VideoPacingMode,
    target_fps: f64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum VideoPacingMode {
    Sender,
    Source,
}

impl VideoPacingMode {
    fn from_option(value: Option<&str>) -> Self {
        match value {
            Some("sender") => Self::Sender,
            _ => Self::Source,
        }
    }

    fn as_label(self) -> &'static str {
        match self {
            Self::Sender => "sender",
            Self::Source => "source",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EnqueueResult {
    Accepted,
    Coalesced,
    Rejected,
}

impl AdaptiveVideoSender {
    fn new(
        source: NativeVideoSource,
        stats: Arc<AdaptiveVideoStats>,
        pacing: VideoPacingMode,
        target_fps: f64,
    ) -> Self {
        let sanitized_target_fps = if target_fps.is_finite() && target_fps > 0.0 {
            target_fps
        } else {
            FPS_PACING_FALLBACK
        };
        let sender = Self {
            pending: Arc::new(ArrayQueue::new(ENCODER_QUEUE_CAPACITY)),
            notify: Arc::new(Notify::new()),
            stop: Arc::new(AtomicBool::new(false)),
            stats,
            pacing,
            target_fps: sanitized_target_fps,
        };
        sender.start_worker(source);
        sender
    }

    fn enqueue(&self, frame: PendingVideoFrame) -> EnqueueResult {
        if self.stop.load(Ordering::Acquire) {
            self.stats.record_reject();
            return EnqueueResult::Rejected;
        }
        let now_ms = now_millis();
        assert_eq!(self.pending.capacity(), ENCODER_QUEUE_CAPACITY);
        let coalesced = self.pending.force_push(frame).is_some();
        let depth = self.pending.len() as u64;
        assert!(depth <= ENCODER_QUEUE_CAPACITY as u64);
        self.stats
            .record_enqueue_with_depth(now_ms, coalesced, depth);
        self.notify.notify_one();
        if coalesced {
            EnqueueResult::Coalesced
        } else {
            EnqueueResult::Accepted
        }
    }

    fn stop(&self) {
        self.stop.store(true, Ordering::Release);
        while self.pending.pop().is_some() {}
        self.stats.record_queue_cleared();
        self.notify.notify_waiters();
    }

    fn start_worker(&self, source: NativeVideoSource) {
        let pending = self.pending.clone();
        let notify = self.notify.clone();
        let stop = self.stop.clone();
        let stats = self.stats.clone();
        let pacing = self.pacing;
        let target_fps = self.target_fps;
        tokio::spawn(async move {
            let mut last_capture_at: Option<tokio::time::Instant> = None;
            loop {
                if stop.load(Ordering::Acquire) {
                    break;
                }
                let Some(frame) = pending.pop() else {
                    notify.notified().await;
                    continue;
                };

                let effective_fps = stats.current_fps().min(target_fps).max(1.0);
                assert!(effective_fps.is_finite());
                assert!(effective_fps >= 1.0);
                let pacing_interval = Duration::from_secs_f64(1.0 / effective_fps);
                let throttled = effective_fps < target_fps;
                if pacing == VideoPacingMode::Sender || throttled {
                    if let Some(last_capture) = last_capture_at {
                        let next_capture = last_capture + pacing_interval;
                        let now = tokio::time::Instant::now();
                        if next_capture > now {
                            tokio::time::sleep_until(next_capture).await;
                        }
                    }
                }

                let enqueued_at = frame.enqueued_at();
                let capture_started_tokio = tokio::time::Instant::now();
                let capture_started_at = Instant::now();
                let queue_age_ms = elapsed_ms(enqueued_at, capture_started_at);
                let captured = publish_pending_video_frame(&source, frame);
                let now_ms = now_millis();
                if captured {
                    let latency_ms = elapsed_ms(enqueued_at, Instant::now());
                    stats.record_capture(now_ms, queue_age_ms, latency_ms);
                } else {
                    stats.record_capture_failure();
                }
                last_capture_at = Some(capture_started_tokio);
            }
        });
    }
}

struct BusSenderSink {
    sender: AdaptiveVideoSender,
    #[cfg_attr(target_os = "macos", allow(dead_code))]
    texture_capability: TextureCapability,
}

impl ScreenFrameSink for BusSenderSink {
    fn enqueue(&self, frame: BusScreenFrame) -> EnqueueOutcome {
        let timestamp_us = frame.timestamp_us();
        let pending = match frame {
            // CPU NV12 frames come from the X11 screen-capture backend (the portal
            // backend delivers DMABUF/GPU frames instead). Feed them through the
            // existing software NV12 encode path.
            BusScreenFrame::Nv12(nv12) => PendingVideoFrame::Nv12 {
                data: nv12.data.as_slice().to_vec(),
                width: nv12.width,
                height: nv12.height,
                stride_y: nv12.stride_y,
                stride_uv: nv12.stride_uv,
                timestamp_us,
                enqueued_at: Instant::now(),
            },
            BusScreenFrame::Bgra(_) => return EnqueueOutcome::Rejected,
            #[cfg(target_os = "macos")]
            BusScreenFrame::MacCvPixelBuffer(mac_frame) => {
                let raw = mac_frame.into_raw_pixel_buffer();
                if raw.is_null() {
                    return EnqueueOutcome::Rejected;
                }
                let buffer = unsafe { NativeBuffer::from_cv_pixel_buffer(raw) };
                PendingVideoFrame::MacCvPixelBuffer {
                    buffer,
                    timestamp_us,
                    enqueued_at: Instant::now(),
                }
            }
            #[cfg(target_os = "linux")]
            BusScreenFrame::Dmabuf(dmabuf) => {
                let frame_bus::DmabufDesc {
                    plane_count,
                    width,
                    height,
                    drm_format,
                    modifier,
                    strides,
                    offsets,
                    device_uuid,
                    ..
                } = dmabuf.desc;
                PendingVideoFrame::Dmabuf {
                    desc: texture_source::DmabufFrameDesc {
                        plane_count,
                        width,
                        height,
                        drm_format,
                        modifier,
                        strides,
                        offsets,
                        device_uuid,
                        timestamp_us,
                    },
                    capability: self.texture_capability,
                    fds: dmabuf.fds,
                    enqueued_at: Instant::now(),
                }
            }
            #[cfg(target_os = "windows")]
            BusScreenFrame::SharedTexture(bus_desc) => {
                let desc = texture_source::TextureFrameDesc {
                    handle: bus_desc.handle,
                    width: bus_desc.width,
                    height: bus_desc.height,
                    dxgi_format: bus_desc.dxgi_format,
                    timestamp_us,
                };
                if texture_source::should_attempt_texture_encode(&self.texture_capability, &desc)
                    .is_err()
                {
                    return EnqueueOutcome::Rejected;
                }
                PendingVideoFrame::Texture {
                    desc,
                    capability: self.texture_capability,
                    enqueued_at: Instant::now(),
                }
            }
        };
        match self.sender.enqueue(pending) {
            EnqueueResult::Accepted => EnqueueOutcome::Accepted,
            EnqueueResult::Coalesced => EnqueueOutcome::Coalesced,
            EnqueueResult::Rejected => EnqueueOutcome::Rejected,
        }
    }
}

struct ScreenFrameSinkHandleExternal {
    handle: NativeScreenFrameSinkHandle,
}

unsafe extern "C" fn finalize_screen_frame_sink_handle(
    _env: napi::sys::napi_env,
    data: *mut c_void,
    _hint: *mut c_void,
) {
    if data.is_null() {
        return;
    }
    let handle = unsafe { Box::from_raw(data as *mut NativeScreenFrameSinkHandle) };
    if handle.is_valid() {
        unsafe { (handle.release)(handle.context) };
    }
}

impl ToNapiValue for ScreenFrameSinkHandleExternal {
    unsafe fn to_napi_value(
        raw_env: napi::sys::napi_env,
        value: Self,
    ) -> napi::Result<napi::sys::napi_value> {
        let mut napi_value = std::ptr::null_mut();
        let handle_ptr = Box::into_raw(Box::new(value.handle));
        let status = unsafe {
            napi::sys::napi_create_external(
                raw_env,
                handle_ptr.cast(),
                Some(finalize_screen_frame_sink_handle),
                std::ptr::null_mut(),
                &mut napi_value,
            )
        };
        if status != napi::sys::Status::napi_ok {
            let handle = unsafe { Box::from_raw(handle_ptr) };
            if handle.is_valid() {
                unsafe { (handle.release)(handle.context) };
            }
            return Err(napi::Error::new(
                Status::GenericFailure,
                "failed to create native screen frame sink handle external",
            ));
        }
        Ok(napi_value)
    }
}

unsafe extern "C" fn retain_bus_sender_sink(context: *const c_void) {
    if !context.is_null() {
        unsafe { Arc::increment_strong_count(context as *const BusSenderSink) };
    }
}

unsafe extern "C" fn release_bus_sender_sink(context: *const c_void) {
    if !context.is_null() {
        drop(unsafe { Arc::from_raw(context as *const BusSenderSink) });
    }
}

fn bus_sender_sink_from_context<'a>(context: *const c_void) -> Option<&'a BusSenderSink> {
    if context.is_null() {
        return None;
    }
    Some(unsafe { &*(context as *const BusSenderSink) })
}

#[cfg(target_os = "macos")]
unsafe extern "C" fn enqueue_native_mac_cv_pixel_buffer(
    context: *const c_void,
    pixel_buffer: *mut c_void,
    width: u32,
    height: u32,
    pixel_format: u32,
    timestamp_us: i64,
) -> u32 {
    let Some(sink) = bus_sender_sink_from_context(context) else {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    };
    if pixel_buffer.is_null() {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    }
    let frame = unsafe {
        frame_bus::MacCvPixelBufferFrame::from_retained(
            pixel_buffer,
            width,
            height,
            pixel_format,
            timestamp_us,
        )
    };
    NativeScreenFrameSinkHandle::native_outcome(
        sink.enqueue(BusScreenFrame::MacCvPixelBuffer(frame)),
    )
}

unsafe extern "C" fn enqueue_native_nv12(
    context: *const c_void,
    data: *const u8,
    data_len: usize,
    width: u32,
    height: u32,
    stride_y: u32,
    stride_uv: u32,
    timestamp_us: i64,
) -> u32 {
    let Some(sink) = bus_sender_sink_from_context(context) else {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    };
    if data.is_null() || data_len == 0 || data_len > frame_bus::FRAME_DATA_BYTES_CAP {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    }
    let bytes = unsafe { std::slice::from_raw_parts(data, data_len) }.to_vec();
    NativeScreenFrameSinkHandle::native_outcome(sink.enqueue(BusScreenFrame::Nv12(
        frame_bus::Nv12Frame {
            data: frame_bus::FrameData::from_owned(bytes),
            width,
            height,
            stride_y,
            stride_uv,
            timestamp_us,
        },
    )))
}

#[cfg(target_os = "linux")]
unsafe extern "C" fn enqueue_native_dmabuf(
    context: *const c_void,
    desc: frame_bus::DmabufDesc,
    fds: *const i32,
    fd_count: usize,
) -> u32 {
    use std::os::fd::FromRawFd;

    let Some(sink) = bus_sender_sink_from_context(context) else {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    };
    let plane_count = desc.plane_count as usize;
    if fds.is_null() || plane_count == 0 || plane_count > 4 || fd_count < plane_count {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    }
    let raw_fds = unsafe { std::slice::from_raw_parts(fds, plane_count) };
    let owned_fds = raw_fds
        .iter()
        .map(|fd| unsafe { OwnedFd::from_raw_fd(*fd) })
        .collect();
    NativeScreenFrameSinkHandle::native_outcome(sink.enqueue(BusScreenFrame::Dmabuf(
        frame_bus::DmabufFrame {
            desc,
            fds: owned_fds,
        },
    )))
}

#[cfg(target_os = "windows")]
unsafe extern "C" fn enqueue_native_shared_texture(
    context: *const c_void,
    desc: frame_bus::SharedTextureDesc,
) -> u32 {
    let Some(sink) = bus_sender_sink_from_context(context) else {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    };
    NativeScreenFrameSinkHandle::native_outcome(sink.enqueue(BusScreenFrame::SharedTexture(desc)))
}

struct ScreenAudioSinkContext {
    ring: Arc<ScreenAudioRing>,
}

unsafe extern "C" fn retain_screen_audio_sink(context: *const c_void) {
    if !context.is_null() {
        unsafe { Arc::increment_strong_count(context as *const ScreenAudioSinkContext) };
    }
}

unsafe extern "C" fn release_screen_audio_sink(context: *const c_void) {
    if !context.is_null() {
        drop(unsafe { Arc::from_raw(context as *const ScreenAudioSinkContext) });
    }
}

unsafe extern "C" fn enqueue_native_screen_audio(
    context: *const c_void,
    samples: *const f32,
    num_frames: u32,
    channels: u32,
    sample_rate_hz: u32,
    _timestamp_us: i64,
) -> u32 {
    if context.is_null() {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    }
    let ctx = unsafe { &*(context as *const ScreenAudioSinkContext) };
    if samples.is_null() || num_frames == 0 || channels == 0 {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    }
    let total = (num_frames as usize).saturating_mul(channels as usize);
    if total == 0 || total > SCREEN_AUDIO_CHUNK_F32_MAX {
        return frame_bus::NATIVE_SCREEN_FRAME_SINK_REJECTED;
    }
    let slice = unsafe { std::slice::from_raw_parts(samples, total) };
    let mut buffer = ctx.ring.take_buffer();
    buffer.clear();
    buffer.extend_from_slice(slice);
    ctx.ring.push(ScreenAudioChunk {
        samples: buffer,
        num_frames,
        channels,
        sample_rate_hz,
    });
    frame_bus::NATIVE_SCREEN_FRAME_SINK_ACCEPTED
}

fn create_screen_audio_sink_handle(ring: Arc<ScreenAudioRing>) -> NativeScreenFrameSinkHandle {
    let context = Arc::new(ScreenAudioSinkContext { ring });
    NativeScreenFrameSinkHandle {
        magic: frame_bus::NATIVE_SCREEN_FRAME_SINK_HANDLE_MAGIC,
        version: frame_bus::NATIVE_SCREEN_FRAME_SINK_HANDLE_VERSION,
        context: Arc::into_raw(context) as *const c_void,
        retain: retain_screen_audio_sink,
        release: release_screen_audio_sink,
        enqueue_screen_audio: Some(enqueue_native_screen_audio),
        enqueue_nv12: None,
        enqueue_bgra: None,
        enqueue_mac_cv_pixel_buffer: None,
        enqueue_dmabuf: None,
        enqueue_shared_texture: None,
    }
}

fn create_native_screen_frame_sink_handle(sink: Arc<BusSenderSink>) -> NativeScreenFrameSinkHandle {
    NativeScreenFrameSinkHandle {
        magic: frame_bus::NATIVE_SCREEN_FRAME_SINK_HANDLE_MAGIC,
        version: frame_bus::NATIVE_SCREEN_FRAME_SINK_HANDLE_VERSION,
        context: Arc::into_raw(sink) as *const c_void,
        retain: retain_bus_sender_sink,
        release: release_bus_sender_sink,
        enqueue_screen_audio: None,
        enqueue_nv12: Some(enqueue_native_nv12),
        enqueue_bgra: None,
        #[cfg(target_os = "macos")]
        enqueue_mac_cv_pixel_buffer: Some(enqueue_native_mac_cv_pixel_buffer),
        #[cfg(not(target_os = "macos"))]
        enqueue_mac_cv_pixel_buffer: None,
        #[cfg(target_os = "linux")]
        enqueue_dmabuf: Some(enqueue_native_dmabuf),
        #[cfg(not(target_os = "linux"))]
        enqueue_dmabuf: None,
        #[cfg(target_os = "windows")]
        enqueue_shared_texture: Some(enqueue_native_shared_texture),
        #[cfg(not(target_os = "windows"))]
        enqueue_shared_texture: None,
    }
}

struct RoomEventForwarders {
    events: Arc<Mutex<Option<EventTsfn>>>,
    inbound_audio: Arc<AtomicU64>,
    inbound_video: Arc<AtomicU64>,
    dropped_video_frame_callbacks: Arc<AtomicU64>,
    dropped_engine_events: Arc<AtomicU64>,
    video_frames: Arc<Mutex<Option<VideoFrameTsfn>>>,
    camera: Arc<Mutex<Option<CameraSource>>>,
    screen_camera: Arc<Mutex<Option<CameraSource>>>,
    screen: Arc<Mutex<Option<ScreenSource>>>,
    screen_audio: Arc<Mutex<Option<ScreenAudioSource>>>,
    mic: Arc<Mutex<Option<MicSource>>>,
    inbound_forwarders: Arc<InboundForwarderRegistry>,
    count_inbound_audio: bool,
    speaking_thresholds: Arc<SpeakingThresholds>,
}

impl RoomEventForwarders {
    fn handle_room_event(&self, event: &RoomEvent) {
        if let RoomEvent::LocalTrackRepublished {
            previous_sid,
            publication,
            ..
        } = event
        {
            self.apply_republished_track_sid(previous_sid.as_str(), publication.sid().as_str());
        }
        if let RoomEvent::TrackSubscribed {
            track,
            publication,
            participant,
        } = event
        {
            self.wire_track_subscribed_media(track, publication, participant);
        }
        if let RoomEvent::TrackUnsubscribed { publication, .. } = event {
            self.cancel_inbound_forwarder(publication.sid().as_str());
        }
        if let RoomEvent::ParticipantDisconnected(participant) = event {
            self.inbound_forwarders
                .cancel_for_participant(participant.sid().as_str());
        }
        self.emit_mapped_event(event);
    }

    fn cancel_inbound_forwarder(&self, track_sid: &str) {
        self.inbound_forwarders.cancel(track_sid);
    }

    fn apply_republished_track_sid(&self, previous_sid: &str, republished_sid: &str) {
        let slots = LocalTrackSlots {
            camera: &self.camera,
            screen_camera: &self.screen_camera,
            screen: &self.screen,
            screen_audio: &self.screen_audio,
            mic: &self.mic,
        };
        let swapped = apply_local_track_republish(&slots, previous_sid, republished_sid);
        if !swapped {
            eprintln!(
                "webrtc-sender: local track republish matched no slot \
                 (previous {previous_sid}, republished {republished_sid}); \
                 republished track left published"
            );
        }
    }

    fn wire_track_subscribed_media(
        &self,
        track: &RemoteTrack,
        publication: &livekit::publication::RemoteTrackPublication,
        participant: &RemoteParticipant,
    ) {
        match track {
            RemoteTrack::Audio(audio) => {
                let source = events::track_source_str(publication.source());
                if source == "microphone" {
                    SpeakingTap {
                        participant_sid: participant.sid().to_string(),
                        identity: participant.identity().to_string(),
                        track_sid: publication.sid().to_string(),
                        source: "microphone",
                        is_local: false,
                        release_ms: SPEAKING_RELEASE_MS_REMOTE,
                        thresholds: self.speaking_thresholds.clone(),
                        events: self.events.clone(),
                        dropped_engine_events: self.dropped_engine_events.clone(),
                        stop: Arc::new(AtomicBool::new(false)),
                        inbound_audio: self.count_inbound_audio.then(|| self.inbound_audio.clone()),
                    }
                    .spawn(audio.rtc_track());
                    return;
                }
                if !self.count_inbound_audio {
                    return;
                }
                let counter = self.inbound_audio.clone();
                let stream = NativeAudioStream::new(audio.rtc_track(), 48000, 1);
                let handle = spawn_drain_forwarder(stream, move |_frame| {
                    counter.fetch_add(1, Ordering::Relaxed);
                });
                self.inbound_forwarders.register(
                    publication.sid().as_str(),
                    participant.sid().as_str(),
                    handle,
                );
            }
            RemoteTrack::Video(video) => {
                self.spawn_inbound_video_forwarder(video, publication, participant);
            }
        }
    }

    fn spawn_inbound_video_forwarder(
        &self,
        video: &livekit::track::RemoteVideoTrack,
        publication: &livekit::publication::RemoteTrackPublication,
        participant: &RemoteParticipant,
    ) {
        let counter = self.inbound_video.clone();
        let stream = NativeVideoStream::new(video.rtc_track());
        let meta_prefix = video_frame_meta_prefix(
            participant.sid().as_str(),
            participant.identity().as_str(),
            publication.sid().as_str(),
            &publication.name(),
            events::track_source_str(publication.source()),
        );
        let callback_slot = self.video_frames.clone();
        let dropped_callback_counter = self.dropped_video_frame_callbacks.clone();
        let mut skip_payload_after_queue_full = false;
        let handle = spawn_drain_forwarder(stream, move |frame| {
            counter.fetch_add(1, Ordering::Relaxed);
            if callback_slot.lock().is_none() {
                return;
            }
            if skip_payload_after_queue_full {
                skip_payload_after_queue_full = false;
                dropped_callback_counter.fetch_add(1, Ordering::Relaxed);
                return;
            }
            let Some((meta, buffer)) = frame_to_callback_payload(&frame, &meta_prefix) else {
                return;
            };
            if let Some(tsfn) = callback_slot.lock().as_ref() {
                let status = tsfn.call((meta, buffer), ThreadsafeFunctionCallMode::NonBlocking);
                if status == Status::QueueFull {
                    skip_payload_after_queue_full = true;
                    dropped_callback_counter.fetch_add(1, Ordering::Relaxed);
                }
            }
        });
        self.inbound_forwarders.register(
            publication.sid().as_str(),
            participant.sid().as_str(),
            handle,
        );
    }

    fn emit_mapped_event(&self, event: &RoomEvent) {
        let (type_name, payload) = match events::map_room_event(event) {
            Some((ty, json)) => (ty.to_string(), json),
            None => {
                let debug = format!("{event:?}");
                let variant = debug
                    .split([' ', '(', '{'])
                    .next()
                    .unwrap_or("unknown")
                    .to_string();
                (variant, "{}".to_string())
            }
        };
        emit_engine_event(
            &self.events,
            &self.dropped_engine_events,
            type_name,
            payload,
        );
    }
}

struct PlatformAudioInstall {
    platform_audio: Arc<Mutex<Option<PlatformAudio>>>,
    epoch_slot: Arc<AtomicU64>,
    expected_epoch: u64,
    device_mic_recording_requested: Arc<AtomicBool>,
    events: Arc<Mutex<Option<EventTsfn>>>,
    dropped_engine_events: Arc<AtomicU64>,
}

impl PlatformAudioInstall {
    fn run(self) {
        assert!(self.expected_epoch >= 1);
        if self.epoch_slot.load(Ordering::Acquire) != self.expected_epoch {
            return;
        }
        if self.platform_audio.lock().is_some() {
            return;
        }
        match PlatformAudio::new() {
            Ok(platform_audio) => self.install(platform_audio),
            Err(error) => {
                let payload = events::json_object(&[(
                    "message",
                    events::JsonValue::Str(format!("platform audio unavailable: {error}")),
                )]);
                emit_engine_event(
                    &self.events,
                    &self.dropped_engine_events,
                    "audioPlaybackUnavailable".to_string(),
                    payload,
                );
            }
        }
    }

    fn install(&self, platform_audio: PlatformAudio) {
        {
            let mut guard = self.platform_audio.lock();
            if self.epoch_slot.load(Ordering::Acquire) != self.expected_epoch {
                return;
            }
            if guard.is_some() {
                return;
            }
            *guard = Some(platform_audio);
        }
        if !self.device_mic_recording_requested.load(Ordering::Acquire) {
            set_platform_adm_recording_enabled(false);
            if let Some(audio) = self.platform_audio.lock().as_ref() {
                let _ = audio.stop_recording();
            }
        }
    }
}

#[napi]
pub struct VoiceEngine {
    state: Arc<AtomicU8>,
    room: Arc<Mutex<Option<Arc<Room>>>>,
    screen: Arc<Mutex<Option<ScreenSource>>>,
    screen_audio: Arc<Mutex<Option<ScreenAudioSource>>>,
    screen_audio_ring: Arc<ScreenAudioRing>,
    mic: Arc<Mutex<Option<MicSource>>>,
    camera: Arc<Mutex<Option<CameraSource>>>,
    camera_preview: Arc<Mutex<Option<Arc<AtomicBool>>>>,
    screen_camera: Arc<Mutex<Option<CameraSource>>>,
    events: Arc<Mutex<Option<EventTsfn>>>,
    platform_audio: Arc<Mutex<Option<PlatformAudio>>>,
    device_mic_recording_requested: Arc<AtomicBool>,
    participant_volumes: Arc<Mutex<HashMap<String, f64>>>,
    byte_samples: Arc<Mutex<HashMap<String, stats_mod::ByteRateSample>>>,
    last_stats_json: Arc<Mutex<Option<String>>>,
    stats_running: Arc<AtomicBool>,
    video_frames: Arc<Mutex<Option<VideoFrameTsfn>>>,
    inbound_audio: Arc<AtomicU64>,
    inbound_video: Arc<AtomicU64>,
    dropped_video_frame_callbacks: Arc<AtomicU64>,
    dropped_engine_events: Arc<AtomicU64>,
    inbound_forwarders: Arc<InboundForwarderRegistry>,
    count_inbound_audio: Arc<AtomicBool>,
    connect_epoch: Arc<AtomicU64>,
    connect_intent: Arc<AtomicU64>,
    connect_cancel: watch::Sender<u64>,
    connect_serial: Arc<tokio::sync::Mutex<()>>,
    state_guard: Arc<Mutex<()>>,
    texture_capability: Mutex<TextureCapability>,
    send_video_stats: Arc<Mutex<Option<Arc<AdaptiveVideoStats>>>>,
    send_audio_stats: Arc<AdaptiveAudioStats>,
    max_audio_buffer_ms: Arc<AtomicU64>,
    camera_live_background: crate::camera_background::CameraBackgroundLiveSlot,
    speaking_thresholds: Arc<SpeakingThresholds>,
    mic_speaking_stop: Arc<Mutex<Option<Arc<AtomicBool>>>>,
}

#[napi]
impl VoiceEngine {
    #[napi(constructor)]
    pub fn new() -> Self {
        Self {
            state: Arc::new(AtomicU8::new(S_IDLE)),
            room: Arc::new(Mutex::new(None)),
            screen: Arc::new(Mutex::new(None)),
            screen_audio: Arc::new(Mutex::new(None)),
            screen_audio_ring: Arc::new(ScreenAudioRing::new()),
            mic: Arc::new(Mutex::new(None)),
            camera: Arc::new(Mutex::new(None)),
            camera_preview: Arc::new(Mutex::new(None)),
            screen_camera: Arc::new(Mutex::new(None)),
            events: Arc::new(Mutex::new(None)),
            platform_audio: Arc::new(Mutex::new(None)),
            device_mic_recording_requested: Arc::new(AtomicBool::new(false)),
            participant_volumes: Arc::new(Mutex::new(HashMap::new())),
            byte_samples: Arc::new(Mutex::new(HashMap::new())),
            last_stats_json: Arc::new(Mutex::new(None)),
            stats_running: Arc::new(AtomicBool::new(false)),
            video_frames: Arc::new(Mutex::new(None)),
            inbound_audio: Arc::new(AtomicU64::new(0)),
            inbound_video: Arc::new(AtomicU64::new(0)),
            dropped_video_frame_callbacks: Arc::new(AtomicU64::new(0)),
            dropped_engine_events: Arc::new(AtomicU64::new(0)),
            inbound_forwarders: Arc::new(InboundForwarderRegistry::new()),
            count_inbound_audio: Arc::new(AtomicBool::new(false)),
            connect_epoch: Arc::new(AtomicU64::new(0)),
            connect_intent: Arc::new(AtomicU64::new(0)),
            connect_cancel: watch::channel(0).0,
            connect_serial: Arc::new(tokio::sync::Mutex::new(())),
            state_guard: Arc::new(Mutex::new(())),
            texture_capability: Mutex::new(TextureCapability::unavailable(
                texture_source::TextureEncodeError::UnsupportedCodec,
            )),
            send_video_stats: Arc::new(Mutex::new(None)),
            send_audio_stats: Arc::new(AdaptiveAudioStats::new(
                DEFAULT_AUDIO_BUFFER_MAX_MS,
                now_millis(),
            )),
            max_audio_buffer_ms: Arc::new(AtomicU64::new(DEFAULT_AUDIO_BUFFER_MAX_MS as u64)),
            camera_live_background: crate::camera_background::CameraBackgroundLiveSlot::new(),
            speaking_thresholds: Arc::new(SpeakingThresholds::new()),
            mic_speaking_stop: Arc::new(Mutex::new(None)),
        }
    }

    fn stop_platform_recording(&self) {
        if let Some(audio) = self.platform_audio.lock().as_ref() {
            let _ = audio.stop_recording();
        }
    }

    fn set_device_mic_recording_requested(&self, enabled: bool) {
        if !enabled {
            self.stop_platform_recording();
        }
        self.device_mic_recording_requested
            .store(enabled, Ordering::Release);
        set_platform_adm_recording_enabled(enabled);
        if !enabled {
            self.stop_platform_recording();
        }
    }

    fn disable_platform_recording_if_device_mic_inactive(&self) {
        if !self.device_mic_recording_requested.load(Ordering::Acquire) {
            set_platform_adm_recording_enabled(false);
            self.stop_platform_recording();
        }
    }

    fn schedule_delayed_platform_recording_disable_if_device_mic_inactive(&self) {
        let device_mic_recording_requested = self.device_mic_recording_requested.clone();
        let platform_audio = self.platform_audio.clone();
        tokio::spawn(async move {
            tokio::time::sleep(Duration::from_millis(
                PLATFORM_RECORDING_DISABLE_REASSERT_DELAY_MS,
            ))
            .await;
            if device_mic_recording_requested.load(Ordering::Acquire) {
                return;
            }
            set_platform_adm_recording_enabled(false);
            if let Some(audio) = platform_audio.lock().as_ref() {
                let _ = audio.stop_recording();
            }
        });
    }

    #[napi]
    pub fn set_count_inbound_audio(&self, enabled: bool) {
        self.count_inbound_audio.store(enabled, Ordering::Release);
    }

    #[napi]
    pub fn set_event_callback(&self, callback: Function<(String, String), ()>) -> napi::Result<()> {
        let tsfn: EventTsfn = callback
            .build_threadsafe_function::<(String, String)>()
            .weak::<true>()
            .callee_handled::<false>()
            .max_queue_size::<EVENT_QUEUE_LIMIT>()
            .build()?;
        *self.events.lock() = Some(tsfn);
        Ok(())
    }

    #[napi]
    pub fn set_video_frame_callback(
        &self,
        callback: Function<(String, Buffer), ()>,
    ) -> napi::Result<()> {
        let tsfn: VideoFrameTsfn = callback
            .build_threadsafe_function::<(String, Buffer)>()
            .weak::<true>()
            .callee_handled::<false>()
            .max_queue_size::<VIDEO_FRAME_QUEUE_LIMIT>()
            .build()?;
        *self.video_frames.lock() = Some(tsfn);
        Ok(())
    }

    #[napi]
    pub fn clear_video_frame_callback(&self) {
        *self.video_frames.lock() = None;
    }

    #[napi]
    pub async fn connect(
        &self,
        url: String,
        token: String,
        e2ee_key: Option<Buffer>,
        connect_options: Option<VoiceEngineV2BridgeConnectOptions>,
    ) -> napi::Result<()> {
        if url.trim().is_empty() || token.trim().is_empty() {
            return Err(napi::Error::from_reason("url and token are required"));
        }
        let intent = self
            .connect_intent
            .fetch_add(1, Ordering::AcqRel)
            .wrapping_add(1);
        let _serial_guard = self.connect_serial.lock().await;
        let latest_intent = self.connect_intent.load(Ordering::Acquire);
        if admit_connect_attempt(intent, latest_intent) == ConnectAdmission::Superseded {
            return Err(napi::Error::from_reason("connect superseded"));
        }
        self.shutdown_connection_locked().await;
        assert!(self.room.lock().is_none());
        assert_eq!(self.state.load(Ordering::Acquire), S_CLOSED);
        let connect_epoch = self.advance_connect_epoch();
        store_connection_state(&self.state, S_CONNECTING);
        self.dial_room_locked(url, token, e2ee_key, connect_options, connect_epoch)
            .await
    }

    async fn dial_room_locked(
        &self,
        url: String,
        token: String,
        e2ee_key: Option<Buffer>,
        connect_options: Option<VoiceEngineV2BridgeConnectOptions>,
        connect_epoch: u64,
    ) -> napi::Result<()> {
        assert!(connect_epoch >= 1);
        let encryption = e2ee_key.map(|key| E2eeOptions {
            key_provider: KeyProvider::with_shared_key(KeyProviderOptions::default(), key.to_vec()),
            encryption_type: EncryptionType::Gcm,
        });
        let connect_options = connect_options.unwrap_or_default();
        let mut room_options = RoomOptions::default();
        room_options.auto_subscribe = connect_options.auto_subscribe.unwrap_or(true);
        room_options.adaptive_stream = connect_options.adaptive_stream.unwrap_or(true);
        room_options.dynacast = connect_options.dynacast.unwrap_or(true);
        room_options.encryption = encryption;

        let connect_cancelled =
            wait_connect_cancelled(self.connect_cancel.subscribe(), connect_epoch);
        let connect = tokio::select! {
            connect = Room::connect(&url, &token, room_options) => connect,
            () = connect_cancelled => {
                return Err(napi::Error::from_reason("connect cancelled"));
            }
        };
        let (room, events_rx) = match connect {
            Ok(pair) => pair,
            Err(error) => {
                self.record_dial_failure(connect_epoch);
                return Err(napi::Error::from_reason(format!(
                    "livekit connect: {error}"
                )));
            }
        };
        let room = Arc::new(room);
        let previous_room = self.room.lock().replace(room.clone());
        assert!(previous_room.is_none());
        *self.last_stats_json.lock() = None;
        if !self.adopt_dialed_room(connect_epoch) {
            let abandoned = self.room.lock().take();
            assert!(abandoned.is_some());
            *self.platform_audio.lock() = None;
            let _ = room.close().await;
            return Err(napi::Error::from_reason("connect cancelled"));
        }
        self.setup_platform_audio_deferred(connect_epoch);
        self.spawn_room_event_loop(events_rx, connect_epoch);
        self.start_stats_task();
        Ok(())
    }

    fn advance_connect_epoch(&self) -> u64 {
        let _guard = self.state_guard.lock();
        self.connect_epoch
            .fetch_add(1, Ordering::AcqRel)
            .wrapping_add(1)
    }

    fn cancel_connection_intent(&self) {
        let _guard = self.state_guard.lock();
        let cancelled_epoch = self
            .connect_epoch
            .fetch_add(1, Ordering::AcqRel)
            .wrapping_add(1);
        store_connection_state(&self.state, S_CLOSED);
        self.connect_cancel.send_replace(cancelled_epoch);
    }

    fn record_dial_failure(&self, connect_epoch: u64) {
        assert!(connect_epoch >= 1);
        let _guard = self.state_guard.lock();
        if self.connect_epoch.load(Ordering::Acquire) != connect_epoch {
            return;
        }
        store_connection_state(&self.state, S_FAILED);
    }

    fn adopt_dialed_room(&self, connect_epoch: u64) -> bool {
        assert!(connect_epoch >= 1);
        let _guard = self.state_guard.lock();
        if self.connect_epoch.load(Ordering::Acquire) != connect_epoch {
            return false;
        }
        store_connection_state(&self.state, S_CONNECTED);
        true
    }

    fn spawn_room_event_loop(
        &self,
        mut events_rx: tokio::sync::mpsc::UnboundedReceiver<RoomEvent>,
        loop_epoch: u64,
    ) {
        assert!(loop_epoch >= 1);
        assert!(self.room.lock().is_some());
        let forwarders = self.room_event_forwarders();
        let state = self.state.clone();
        let epoch_slot = self.connect_epoch.clone();
        let state_guard = self.state_guard.clone();
        tokio::spawn(async move {
            while let Some(event) = events_rx.recv().await {
                let engine_epoch = epoch_slot.load(Ordering::Acquire);
                if room_event_loop_action(loop_epoch, engine_epoch) == RoomLoopAction::Exit {
                    return;
                }
                forwarders.handle_room_event(&event);
            }
            store_room_loop_closed(&state_guard, &epoch_slot, &state, loop_epoch);
        });
    }

    fn room_event_forwarders(&self) -> RoomEventForwarders {
        RoomEventForwarders {
            events: self.events.clone(),
            inbound_audio: self.inbound_audio.clone(),
            inbound_video: self.inbound_video.clone(),
            dropped_video_frame_callbacks: self.dropped_video_frame_callbacks.clone(),
            dropped_engine_events: self.dropped_engine_events.clone(),
            video_frames: self.video_frames.clone(),
            camera: self.camera.clone(),
            screen_camera: self.screen_camera.clone(),
            screen: self.screen.clone(),
            screen_audio: self.screen_audio.clone(),
            mic: self.mic.clone(),
            inbound_forwarders: self.inbound_forwarders.clone(),
            count_inbound_audio: self.count_inbound_audio.load(Ordering::Acquire),
            speaking_thresholds: self.speaking_thresholds.clone(),
        }
    }

    fn platform_audio_install(&self, expected_epoch: u64) -> PlatformAudioInstall {
        assert!(expected_epoch >= 1);
        PlatformAudioInstall {
            platform_audio: self.platform_audio.clone(),
            epoch_slot: self.connect_epoch.clone(),
            expected_epoch,
            device_mic_recording_requested: self.device_mic_recording_requested.clone(),
            events: self.events.clone(),
            dropped_engine_events: self.dropped_engine_events.clone(),
        }
    }

    fn setup_platform_audio_deferred(&self, connect_epoch: u64) {
        assert!(connect_epoch >= 1);
        let install = self.platform_audio_install(connect_epoch);
        if let Err(error) = enqueue_adm_operation(Box::new(move || install.run())) {
            eprintln!("webrtc-sender: platform audio install dispatch failed: {error}");
        }
    }

    #[napi]
    pub async fn ensure_platform_audio(&self) -> napi::Result<()> {
        if self.state.load(Ordering::Acquire) != S_CONNECTED {
            return Ok(());
        }
        if self.platform_audio.lock().is_some() {
            return Ok(());
        }
        let connect_epoch = self.connect_epoch.load(Ordering::Acquire);
        assert!(connect_epoch >= 1);
        let install = self.platform_audio_install(connect_epoch);
        run_audio_device_module_blocking(move || {
            install.run();
            Ok(())
        })
        .await
    }

    #[napi]
    pub async fn publish_screen_share(
        &self,
        width: u32,
        height: u32,
        codec: String,
        max_bitrate_bps: Option<f64>,
        max_framerate: Option<f64>,
        simulcast: Option<bool>,
        publish_options: Option<ScreenSharePublishOptions>,
    ) -> napi::Result<()> {
        if self.state.load(Ordering::Acquire) != S_CONNECTED {
            return Err(napi::Error::from_reason("not connected"));
        }
        if !valid_even_video_dims(width, height) {
            return Err(napi::Error::from_reason("invalid screen dimensions"));
        }
        let track_name = publish_options
            .as_ref()
            .and_then(|opts| opts.track_name.as_ref())
            .map(|raw| raw.trim())
            .filter(|trimmed| !trimmed.is_empty())
            .unwrap_or("screen");
        let source = NativeVideoSource::new(VideoResolution { width, height }, true);
        let track =
            LocalVideoTrack::create_video_track(track_name, RtcVideoSource::Native(source.clone()));
        let mut options = TrackPublishOptions {
            source: TrackSource::Screenshare,
            simulcast: simulcast.unwrap_or(true),
            ..Default::default()
        };
        if !codec.trim().is_empty() {
            let canonical_codec = crate::config::canonical_codec_name(&codec)
                .ok_or_else(|| napi::Error::from_reason("unsupported video codec"))?;
            crate::hardware_encoder::require_publish_codec_runtime_support(canonical_codec)
                .map_err(napi::Error::from_reason)?;
            let video_codec = parse_codec(&codec)
                .ok_or_else(|| napi::Error::from_reason("unsupported video codec"))?;
            options.video_codec = video_codec;
        }
        *self.texture_capability.lock() = texture_capability_for_screen_codec(&codec);
        if let Some(bitrate) = max_bitrate_bps.filter(|b| *b > 0.0) {
            options.video_encoding = Some(VideoEncoding {
                max_bitrate: bitrate as u64,
                max_framerate: max_framerate.filter(|f| *f > 0.0).unwrap_or(30.0),
            });
        }
        let adaptive_send = publish_options
            .as_ref()
            .and_then(|opts| opts.adaptive_send)
            .unwrap_or(true);
        let min_video_fps = publish_options
            .as_ref()
            .and_then(|opts| opts.min_video_fps)
            .filter(|fps| fps.is_finite() && *fps > 0.0)
            .unwrap_or(DEFAULT_MIN_VIDEO_FPS);
        let max_audio_buffer_ms = publish_options
            .as_ref()
            .and_then(|opts| opts.max_audio_buffer_ms)
            .map(crate::send_control::clamp_audio_buffer_ms)
            .unwrap_or(DEFAULT_AUDIO_BUFFER_MAX_MS);
        self.max_audio_buffer_ms
            .store(max_audio_buffer_ms as u64, Ordering::Relaxed);
        self.send_audio_stats
            .reset(max_audio_buffer_ms, now_millis());
        let local = {
            let guard = self.room.lock();
            guard
                .as_ref()
                .map(|room| room.local_participant())
                .ok_or_else(|| napi::Error::from_reason("not connected"))?
        };
        let existing_screen = { self.screen.lock().take() };
        if let Some(existing) = existing_screen {
            existing.video_sender.stop();
            if let Some(prev_id) = existing.bus_capture_id.as_deref() {
                frame_bus::unregister_sink(prev_id);
            }
            *self.send_video_stats.lock() = None;
            local
                .unpublish_track(&existing.track_sid)
                .await
                .map_err(|e| napi::Error::from_reason(format!("unpublish existing screen: {e}")))?;
        }
        let publication = local
            .publish_track(LocalTrack::Video(track), options)
            .await
            .map_err(|e| {
                *self.texture_capability.lock() = TextureCapability::unavailable(
                    texture_source::TextureEncodeError::UnsupportedCodec,
                );
                napi::Error::from_reason(format!("publish_track: {e}"))
            })?;
        let video_stats = Arc::new(AdaptiveVideoStats::new(
            max_framerate
                .filter(|fps| fps.is_finite() && *fps > 0.0)
                .unwrap_or(30.0),
            min_video_fps,
            adaptive_send,
            now_millis(),
        ));
        let pacing = VideoPacingMode::from_option(
            publish_options
                .as_ref()
                .and_then(|opts| opts.pacing.as_deref()),
        );
        let pacing_target_fps = max_framerate
            .filter(|fps| fps.is_finite() && *fps > 0.0)
            .unwrap_or(FPS_PACING_FALLBACK);
        let video_sender =
            AdaptiveVideoSender::new(source, video_stats.clone(), pacing, pacing_target_fps);
        let track_sid = publication.sid();
        *self.send_video_stats.lock() = Some(video_stats);
        let bus_capture_id = publish_options
            .as_ref()
            .and_then(|opts| opts.capture_id.as_ref())
            .map(|raw| raw.trim().to_string())
            .filter(|trimmed| !trimmed.is_empty());
        let frame_sink = bus_capture_id.as_ref().map(|capture_id| {
            let sink = Arc::new(BusSenderSink {
                sender: video_sender.clone(),
                texture_capability: *self.texture_capability.lock(),
            });
            frame_bus::register_sink(capture_id, sink.clone());
            sink
        });
        *self.screen.lock() = Some(ScreenSource {
            track_sid: track_sid.clone(),
            video_sender,
            metadata: ScreenSourceMetadata {
                track_sid,
                width,
                height,
                codec: codec.trim().to_string(),
                target_bitrate_kbps: max_bitrate_bps
                    .filter(|bps| bps.is_finite() && *bps > 0.0)
                    .map(|bps| bps / 1000.0),
                configured_fps: max_framerate
                    .filter(|fps| fps.is_finite() && *fps > 0.0)
                    .unwrap_or(30.0),
            },
            bus_capture_id,
            frame_sink,
        });
        Ok(())
    }

    #[napi]
    pub fn create_screen_frame_sink_handle<'env>(
        &self,
        env: Env,
        capture_id: String,
    ) -> napi::Result<Option<Unknown<'env>>> {
        let capture_id = capture_id.trim();
        if capture_id.is_empty() {
            return Ok(None);
        }
        let sink = {
            let guard = self.screen.lock();
            let Some(screen) = guard.as_ref() else {
                return Ok(None);
            };
            if screen.bus_capture_id.as_deref() != Some(capture_id) {
                return Ok(None);
            }
            let Some(sink) = screen.frame_sink.as_ref() else {
                return Ok(None);
            };
            sink.clone()
        };
        let external = ScreenFrameSinkHandleExternal {
            handle: create_native_screen_frame_sink_handle(sink),
        };
        let raw_env = env.raw();
        let value = unsafe { ScreenFrameSinkHandleExternal::to_napi_value(raw_env, external)? };
        Ok(Some(unsafe { Unknown::from_raw_unchecked(raw_env, value) }))
    }

    #[napi]
    pub fn create_screen_audio_sink_handle<'env>(
        &self,
        env: Env,
    ) -> napi::Result<Option<Unknown<'env>>> {
        let external = ScreenFrameSinkHandleExternal {
            handle: create_screen_audio_sink_handle(self.screen_audio_ring.clone()),
        };
        let raw_env = env.raw();
        let value = unsafe { ScreenFrameSinkHandleExternal::to_napi_value(raw_env, external)? };
        Ok(Some(unsafe { Unknown::from_raw_unchecked(raw_env, value) }))
    }

    fn local_participant(&self) -> napi::Result<LocalParticipant> {
        let guard = self.room.lock();
        guard
            .as_ref()
            .map(|room| room.local_participant())
            .ok_or_else(|| napi::Error::from_reason("not connected"))
    }

    fn platform_audio(&self) -> napi::Result<PlatformAudio> {
        let mut guard = self.platform_audio.lock();
        if let Some(audio) = guard.as_ref() {
            return Ok(audio.clone());
        }
        let audio = PlatformAudio::new()
            .map_err(|e| napi::Error::from_reason(format!("platform audio unavailable: {e}")))?;
        *guard = Some(audio.clone());
        drop(guard);
        self.disable_platform_recording_if_device_mic_inactive();
        Ok(audio)
    }

    fn start_mic_speaking_tap(
        &self,
        local: &LocalParticipant,
        track: &LocalAudioTrack,
        track_sid: &TrackSid,
    ) {
        let stop = Arc::new(AtomicBool::new(false));
        let previous = self.mic_speaking_stop.lock().replace(stop.clone());
        if let Some(previous) = previous {
            previous.store(true, Ordering::Release);
        }
        SpeakingTap {
            participant_sid: local.sid().to_string(),
            identity: local.identity().to_string(),
            track_sid: track_sid.to_string(),
            source: "microphone",
            is_local: true,
            release_ms: SPEAKING_RELEASE_MS_LOCAL,
            thresholds: self.speaking_thresholds.clone(),
            events: self.events.clone(),
            dropped_engine_events: self.dropped_engine_events.clone(),
            stop,
            inbound_audio: None,
        }
        .spawn(track.rtc_track());
        assert!(self.mic_speaking_stop.lock().is_some());
    }

    fn stop_mic_speaking_tap(&self) {
        let stop = self.mic_speaking_stop.lock().take();
        if let Some(stop) = stop {
            stop.store(true, Ordering::Release);
        }
        assert!(self.mic_speaking_stop.lock().is_none());
    }

    #[napi]
    pub fn set_speaking_detection(
        &self,
        local_threshold_rms: f64,
        remote_threshold_rms: f64,
    ) -> napi::Result<()> {
        if !local_threshold_rms.is_finite() {
            return Err(napi::Error::from_reason(
                "local speaking threshold must be finite",
            ));
        }
        if !remote_threshold_rms.is_finite() {
            return Err(napi::Error::from_reason(
                "remote speaking threshold must be finite",
            ));
        }
        self.speaking_thresholds
            .set(local_threshold_rms, remote_threshold_rms);
        Ok(())
    }

    async fn unpublish_existing_microphone(
        &self,
        local: &LocalParticipant,
        keep_device_recording_requested: bool,
    ) -> napi::Result<()> {
        self.stop_mic_speaking_tap();
        let existing = {
            let guard = self.mic.lock();
            guard.as_ref().map(|mic| (mic.track_sid(), mic.is_device()))
        };
        if let Some((track_sid, was_device_mic)) = existing {
            local
                .unpublish_track(&track_sid)
                .await
                .map_err(|e| napi::Error::from_reason(format!("unpublish mic: {e}")))?;
            if was_device_mic && !keep_device_recording_requested {
                self.set_device_mic_recording_requested(false);
            }
        }
        *self.mic.lock() = None;
        Ok(())
    }

    async fn unpublish_existing_screen_audio(&self, local: &LocalParticipant) -> napi::Result<()> {
        let existing = self.screen_audio.lock().take();
        if let Some(audio) = existing {
            local
                .unpublish_track(&audio.track_sid)
                .await
                .map_err(|e| napi::Error::from_reason(format!("unpublish screen audio: {e}")))?;
        }
        Ok(())
    }

    fn select_recording_device(
        &self,
        platform_audio: &PlatformAudio,
        device_id: Option<&str>,
    ) -> napi::Result<Option<String>> {
        let requested_device_id = device_id.map(str::trim).unwrap_or_default();
        if requested_device_id.is_empty() || requested_device_id == "default" {
            return Ok(Some("default".to_string()));
        }
        let raw = collect_factory_recording_devices()?;
        let found = raw
            .iter()
            .any(|(id, _, _)| id.trim() == requested_device_id);
        if !found {
            return Err(napi::Error::from_reason(format!(
                "recording device not found: {requested_device_id}"
            )));
        }
        let id = RecordingDeviceId::from_unchecked_guid(requested_device_id);
        if self.mic.lock().is_some() {
            platform_audio
                .switch_recording_device(&id)
                .map_err(|e| napi::Error::from_reason(format!("switch recording device: {e}")))?;
        } else {
            platform_audio
                .set_recording_device(&id)
                .map_err(|e| napi::Error::from_reason(format!("set recording device: {e}")))?;
        }
        Ok(Some(requested_device_id.to_string()))
    }

    #[napi]
    pub async fn publish_device_microphone(&self, opts: MicrophoneOptions) -> napi::Result<()> {
        if self.state.load(Ordering::Acquire) != S_CONNECTED {
            return Err(napi::Error::from_reason("not connected"));
        }
        let options = build_microphone_publish_options(&opts)?;
        let local = self.local_participant()?;
        let platform_audio = self.platform_audio()?;
        let _selected_device_id =
            self.select_recording_device(&platform_audio, opts.device_id.as_deref())?;
        let deep_filter_requested = opts.deep_filter.unwrap_or(false);
        let deep_filter_mic = if deep_filter_requested {
            self.set_device_mic_recording_requested(true);
            let noise_reduction_level = opts
                .deep_filter_noise_reduction_level
                .unwrap_or(audio::DEEP_FILTER_NOISE_REDUCTION_LEVEL_MAX);
            self.try_start_deep_filter_microphone(&platform_audio, noise_reduction_level)
                .await
        } else {
            None
        };
        let apm_intent = audio::resolve_microphone_apm_intent(
            opts.echo_cancellation,
            opts.noise_suppression,
            opts.auto_gain_control,
            deep_filter_requested,
            deep_filter_mic.is_some(),
        );
        platform_audio
            .configure_audio_processing(audio::processing_options(
                apm_intent.echo_cancellation,
                apm_intent.noise_suppression,
                apm_intent.auto_gain_control,
            ))
            .map_err(|e| napi::Error::from_reason(format!("configure audio processing: {e}")))?;
        self.unpublish_existing_microphone(&local, true).await?;
        self.set_device_mic_recording_requested(true);
        match deep_filter_mic {
            Some(deep_filter_mic) => {
                self.publish_deep_filter_microphone(&local, deep_filter_mic, options)
                    .await
            }
            None => self.publish_plain_device_microphone(&local, options).await,
        }
    }

    fn build_deep_filter_pipe(
        &self,
        noise_reduction_level: f64,
    ) -> Result<DeepFilterPipeParts, String> {
        let source = NativeAudioSource::new(
            AudioSourceOptions {
                echo_cancellation: false,
                noise_suppression: false,
                auto_gain_control: false,
            },
            deep_filter::DEEP_FILTER_SAMPLE_RATE_HZ,
            deep_filter::DEEP_FILTER_NUM_CHANNELS,
            DEEP_FILTER_SOURCE_QUEUE_MS,
        );
        let stop = Arc::new(AtomicBool::new(false));
        let diagnostics = DeepFilterDiagnostics {
            degraded_frames: Arc::new(AtomicU64::new(0)),
            events: self.events.clone(),
            dropped_engine_events: self.dropped_engine_events.clone(),
        };
        let (frame_sender, frame_receiver) =
            mpsc::sync_channel::<DeepFilterCaptureFrame>(DEEP_FILTER_PIPE_QUEUE_FRAMES);
        let ready = spawn_deep_filter_processing_thread(
            noise_reduction_level,
            source.clone(),
            stop.clone(),
            diagnostics.clone(),
            frame_receiver,
        )?;
        Ok(DeepFilterPipeParts {
            source,
            pipe_stop: DeepFilterPipeStop { stop },
            diagnostics,
            frame_sender,
            ready,
        })
    }

    async fn try_start_deep_filter_microphone(
        &self,
        platform_audio: &PlatformAudio,
        noise_reduction_level: f64,
    ) -> Option<DeepFilterMicrophone> {
        if let Err(error) = platform_audio.start_recording() {
            self.emit_deep_filter_fallback(&format!("start recording: {error}"));
            return None;
        }
        let capture_track =
            LocalAudioTrack::create_audio_track("mic-capture", platform_audio.rtc_source());
        let DeepFilterPipeParts {
            source,
            pipe_stop,
            diagnostics,
            frame_sender,
            ready,
        } = match self.build_deep_filter_pipe(noise_reduction_level) {
            Ok(parts) => parts,
            Err(error) => {
                self.emit_deep_filter_fallback(&error);
                return None;
            }
        };
        match ready.await {
            Ok(Ok(())) => {}
            Ok(Err(error)) => {
                self.emit_deep_filter_fallback(&error);
                return None;
            }
            Err(_) => {
                self.emit_deep_filter_fallback("deep filter thread exited before ready");
                return None;
            }
        }
        let capture_count = Arc::new(AtomicU64::new(0));
        let generation =
            install_deep_filter_capture_tap(frame_sender, diagnostics, capture_count.clone());
        let tap_guard = RecordedAudioTapGuard { generation };
        if !await_deep_filter_capture_started(&capture_count).await {
            self.emit_deep_filter_fallback("no capture frames from device source");
            return None;
        }
        let track = LocalAudioTrack::create_audio_track("mic", RtcAudioSource::Native(source));
        emit_deep_filter_status(&self.events, &self.dropped_engine_events, "active", "");
        Some(DeepFilterMicrophone {
            track,
            capture_track,
            pipe_stop,
            tap_guard,
        })
    }

    fn emit_deep_filter_fallback(&self, detail: &str) {
        emit_deep_filter_status(
            &self.events,
            &self.dropped_engine_events,
            "fallback",
            detail,
        );
    }

    async fn publish_deep_filter_microphone(
        &self,
        local: &LocalParticipant,
        deep_filter_mic: DeepFilterMicrophone,
        options: TrackPublishOptions,
    ) -> napi::Result<()> {
        let DeepFilterMicrophone {
            track,
            capture_track,
            pipe_stop,
            tap_guard,
        } = deep_filter_mic;
        let publication = match local
            .publish_track(LocalTrack::Audio(track.clone()), options)
            .await
        {
            Ok(publication) => publication,
            Err(error) => {
                if !self.mic.lock().as_ref().is_some_and(MicSource::is_device) {
                    self.set_device_mic_recording_requested(false);
                }
                return Err(napi::Error::from_reason(format!("publish mic: {error}")));
            }
        };
        self.start_mic_speaking_tap(local, &track, &publication.sid());
        *self.mic.lock() = Some(MicSource::DeviceDeepFiltered {
            track_sid: publication.sid(),
            track,
            capture_track,
            _tap_guard: tap_guard,
            _pipe_stop: pipe_stop,
        });
        Ok(())
    }

    async fn publish_plain_device_microphone(
        &self,
        local: &LocalParticipant,
        options: TrackPublishOptions,
    ) -> napi::Result<()> {
        let platform_audio = self.platform_audio()?;
        let track = LocalAudioTrack::create_audio_track("mic", platform_audio.rtc_source());
        let publication = match local
            .publish_track(LocalTrack::Audio(track.clone()), options)
            .await
        {
            Ok(publication) => publication,
            Err(error) => {
                if !self.mic.lock().as_ref().is_some_and(MicSource::is_device) {
                    self.set_device_mic_recording_requested(false);
                }
                return Err(napi::Error::from_reason(format!("publish mic: {error}")));
            }
        };
        self.start_mic_speaking_tap(local, &track, &publication.sid());
        *self.mic.lock() = Some(MicSource::Device {
            track_sid: publication.sid(),
            track,
        });
        Ok(())
    }

    #[napi]
    pub async fn publish_microphone(
        &self,
        sample_rate: u32,
        num_channels: u32,
    ) -> napi::Result<()> {
        if self.state.load(Ordering::Acquire) != S_CONNECTED {
            return Err(napi::Error::from_reason("not connected"));
        }
        if !valid_audio_format(sample_rate, num_channels) {
            return Err(napi::Error::from_reason("invalid sample_rate/num_channels"));
        }
        let local = self.local_participant()?;
        self.unpublish_existing_microphone(&local, false).await?;
        self.set_device_mic_recording_requested(false);
        let source = NativeAudioSource::new(
            AudioSourceOptions::default(),
            sample_rate,
            num_channels,
            self.max_audio_buffer_ms.load(Ordering::Relaxed) as u32,
        );
        let track =
            LocalAudioTrack::create_audio_track("mic", RtcAudioSource::Native(source.clone()));
        let options = TrackPublishOptions {
            source: TrackSource::Microphone,
            red: true,
            dtx: true,
            ..Default::default()
        };
        let publication = local
            .publish_track(LocalTrack::Audio(track.clone()), options)
            .await
            .map_err(|e| napi::Error::from_reason(format!("publish mic: {e}")))?;
        self.start_mic_speaking_tap(&local, &track, &publication.sid());
        *self.mic.lock() = Some(MicSource::Manual {
            source,
            sample_rate,
            num_channels,
            track_sid: publication.sid(),
            track,
            pcm_scratch: new_pcm_scratch(sample_rate, num_channels),
        });
        Ok(())
    }

    #[napi]
    pub async fn push_pcm(
        &self,
        data: Buffer,
        sample_rate: u32,
        num_channels: u32,
    ) -> napi::Result<bool> {
        let (source, scratch) = {
            let guard = self.mic.lock();
            match guard.as_ref() {
                Some(MicSource::Manual {
                    source,
                    sample_rate: mic_sample_rate,
                    num_channels: mic_num_channels,
                    pcm_scratch,
                    ..
                }) if *mic_sample_rate == sample_rate && *mic_num_channels == num_channels => {
                    (source.clone(), pcm_scratch.clone())
                }
                Some(_) => {
                    return Err(napi::Error::from_reason(
                        "pcm format does not match published mic or mic is device-backed",
                    ));
                }
                None => return Ok(false),
            }
        };
        let mut samples = scratch.lock().await;
        let Some(frame) =
            pcm16_audio_frame_into(data.as_ref(), sample_rate, num_channels, &mut samples)
        else {
            return Ok(false);
        };
        self.send_audio_stats.record_push(now_millis());
        source
            .capture_frame(&frame)
            .await
            .map_err(|e| napi::Error::from_reason(format!("capture_frame: {e}")))?;
        Ok(true)
    }

    #[napi]
    pub async fn disconnect(&self) -> napi::Result<()> {
        self.cancel_connection_intent();
        let _serial_guard = self.connect_serial.lock().await;
        self.shutdown_connection_locked().await;
        Ok(())
    }

    async fn shutdown_connection_locked(&self) {
        self.cancel_connection_intent();
        if let Some(screen) = self.screen.lock().take() {
            screen.video_sender.stop();
            if let Some(id) = screen.bus_capture_id.as_deref() {
                frame_bus::unregister_sink(id);
            }
        }
        *self.send_video_stats.lock() = None;
        *self.texture_capability.lock() =
            TextureCapability::unavailable(texture_source::TextureEncodeError::UnsupportedCodec);
        *self.screen_audio.lock() = None;
        self.stop_mic_speaking_tap();
        *self.mic.lock() = None;
        self.set_device_mic_recording_requested(false);
        self.stop_camera_preview_capture();
        if let Some(cam) = self.camera.lock().take() {
            cam.stop();
        }
        if let Some(cam) = self.screen_camera.lock().take() {
            cam.stop();
        }
        *self.platform_audio.lock() = None;
        self.participant_volumes.lock().clear();
        self.byte_samples.lock().clear();
        *self.last_stats_json.lock() = None;
        self.camera_live_background.clear();
        self.inbound_forwarders.clear();
        let room = self.room.lock().take();
        if let Some(room) = room {
            let _ = room.close().await;
        }
        assert!(self.room.lock().is_none());
        assert_eq!(self.state.load(Ordering::Acquire), S_CLOSED);
    }

    #[napi]
    pub fn is_connected(&self) -> bool {
        self.state.load(Ordering::Acquire) == S_CONNECTED
    }

    #[napi]
    pub async fn unpublish_screen_share(&self) -> napi::Result<()> {
        let screen = self.screen.lock().take();
        let screen_camera = self.screen_camera.lock().take();
        if screen.is_none() && screen_camera.is_none() {
            return Ok(());
        }
        let screen_track_sid = screen.as_ref().map(|screen| screen.track_sid.clone());
        let screen_camera_track_sid = screen_camera
            .as_ref()
            .map(|screen_camera| screen_camera.track_sid().lock().clone());
        if let Some(screen) = screen {
            screen.video_sender.stop();
            if let Some(id) = screen.bus_capture_id.as_deref() {
                frame_bus::unregister_sink(id);
            }
        }
        if let Some(screen_camera) = screen_camera {
            screen_camera.stop();
        }
        *self.texture_capability.lock() =
            TextureCapability::unavailable(texture_source::TextureEncodeError::UnsupportedCodec);
        *self.send_video_stats.lock() = None;
        let local = {
            let guard = self.room.lock();
            guard.as_ref().map(|room| room.local_participant())
        };
        let Some(local) = local else {
            return Ok(());
        };
        let mut first_error: Option<napi::Error> = None;
        if let Some(track_sid) = screen_track_sid.as_ref() {
            let result = local
                .unpublish_track(track_sid)
                .await
                .map(|_| ())
                .map_err(|e| napi::Error::from_reason(format!("unpublish screen: {e}")));
            record_first_error(&mut first_error, result);
        }
        if let Some(track_sid) = screen_camera_track_sid {
            assert!(!track_sid.is_empty());
            let result = match TrackSid::try_from(track_sid) {
                Ok(track_sid) => local
                    .unpublish_track(&track_sid)
                    .await
                    .map(|_| ())
                    .map_err(|e| {
                        napi::Error::from_reason(format!("unpublish device screen share: {e}"))
                    }),
                Err(sid) => Err(napi::Error::from_reason(format!(
                    "unpublish device screen share: invalid track sid {sid}"
                ))),
            };
            record_first_error(&mut first_error, result);
        }
        let audio_result = self.unpublish_existing_screen_audio(&local).await;
        record_first_error(&mut first_error, audio_result);
        match first_error {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    #[napi]
    pub fn is_publishing_screen(&self) -> bool {
        self.screen.lock().is_some() || self.screen_camera.lock().is_some()
    }

    #[napi]
    pub async fn publish_screen_share_audio(
        &self,
        sample_rate: u32,
        num_channels: u32,
    ) -> napi::Result<()> {
        if self.state.load(Ordering::Acquire) != S_CONNECTED {
            return Err(napi::Error::from_reason("not connected"));
        }
        if !valid_audio_format(sample_rate, num_channels) {
            return Err(napi::Error::from_reason("invalid sample_rate/num_channels"));
        }
        self.disable_platform_recording_if_device_mic_inactive();
        {
            let guard = self.screen_audio.lock();
            if guard
                .as_ref()
                .is_some_and(|audio| audio.matches_format(sample_rate, num_channels))
            {
                drop(guard);
                self.schedule_delayed_platform_recording_disable_if_device_mic_inactive();
                return Ok(());
            }
        }
        let local = self.local_participant()?;
        self.unpublish_existing_screen_audio(&local).await?;
        let source = NativeAudioSource::new(
            AudioSourceOptions::default(),
            sample_rate,
            num_channels,
            self.max_audio_buffer_ms.load(Ordering::Relaxed) as u32,
        );
        let track = LocalAudioTrack::create_audio_track(
            "screen-audio",
            RtcAudioSource::Native(source.clone()),
        );
        let options = TrackPublishOptions {
            audio_encoding: Some(AudioEncoding {
                max_bitrate: SCREEN_SHARE_AUDIO_MAX_BITRATE_BPS,
            }),
            source: TrackSource::ScreenshareAudio,
            stream: LIVEKIT_TRACK_SOURCE_SCREEN_SHARE.to_string(),
            red: true,
            dtx: false,
            ..Default::default()
        };
        let publication = local
            .publish_track(LocalTrack::Audio(track), options)
            .await
            .map_err(|e| napi::Error::from_reason(format!("publish screen audio: {e}")))?;
        let drain_stop = Arc::new(AtomicBool::new(false));
        let pcm_scratch = new_pcm_scratch(sample_rate, num_channels);
        spawn_screen_audio_drain(
            self.screen_audio_ring.clone(),
            source.clone(),
            pcm_scratch.clone(),
            drain_stop.clone(),
        );
        *self.screen_audio.lock() = Some(ScreenAudioSource {
            source,
            sample_rate,
            num_channels,
            track_sid: publication.sid(),
            pcm_scratch,
            drain_stop,
            ring: self.screen_audio_ring.clone(),
        });
        self.disable_platform_recording_if_device_mic_inactive();
        self.schedule_delayed_platform_recording_disable_if_device_mic_inactive();
        Ok(())
    }

    #[napi]
    pub async fn push_screen_share_pcm(
        &self,
        data: Buffer,
        sample_rate: u32,
        num_channels: u32,
    ) -> napi::Result<bool> {
        let (source, scratch) = {
            let guard = self.screen_audio.lock();
            match guard.as_ref() {
                Some(audio)
                    if audio.sample_rate == sample_rate && audio.num_channels == num_channels =>
                {
                    (audio.source.clone(), audio.pcm_scratch.clone())
                }
                Some(_) => {
                    return Err(napi::Error::from_reason(
                        "pcm format does not match published screen-share audio",
                    ));
                }
                None => return Ok(false),
            }
        };
        let mut samples = scratch.lock().await;
        let Some(frame) =
            pcm16_audio_frame_into(data.as_ref(), sample_rate, num_channels, &mut samples)
        else {
            return Ok(false);
        };
        self.send_audio_stats.record_push(now_millis());
        source
            .capture_frame(&frame)
            .await
            .map_err(|e| napi::Error::from_reason(format!("capture screen audio frame: {e}")))?;
        Ok(true)
    }

    #[napi]
    pub async fn push_screen_share_float(
        &self,
        data: Buffer,
        sample_rate: u32,
        num_channels: u32,
    ) -> napi::Result<bool> {
        let (source, scratch) = {
            let guard = self.screen_audio.lock();
            match guard.as_ref() {
                Some(audio)
                    if audio.sample_rate == sample_rate && audio.num_channels == num_channels =>
                {
                    (audio.source.clone(), audio.pcm_scratch.clone())
                }
                Some(_) => {
                    return Err(napi::Error::from_reason(
                        "float pcm format does not match published screen-share audio",
                    ));
                }
                None => return Ok(false),
            }
        };
        let mut samples = scratch.lock().await;
        let Some(frame) =
            f32_audio_frame_into(data.as_ref(), sample_rate, num_channels, &mut samples)
        else {
            return Ok(false);
        };
        self.send_audio_stats.record_push(now_millis());
        source
            .capture_frame(&frame)
            .await
            .map_err(|e| napi::Error::from_reason(format!("capture screen audio frame: {e}")))?;
        Ok(true)
    }

    #[napi]
    pub async fn unpublish_screen_share_audio(&self) -> napi::Result<()> {
        let local = {
            let guard = self.room.lock();
            guard.as_ref().map(|room| room.local_participant())
        };
        if let Some(local) = local {
            self.unpublish_existing_screen_audio(&local).await?;
        } else {
            *self.screen_audio.lock() = None;
        }
        Ok(())
    }

    #[napi]
    pub fn is_publishing_screen_audio(&self) -> bool {
        self.screen_audio.lock().is_some()
    }

    #[napi]
    pub async fn set_mic_enabled(&self, enabled: bool) {
        let mic = {
            let guard = self.mic.lock();
            guard
                .as_ref()
                .map(|mic| (mic.track(), mic.capture_track(), mic.is_device()))
        };
        let Some((track, capture_track, is_device_mic)) = mic else {
            if !enabled {
                self.set_device_mic_recording_requested(false);
            }
            return;
        };
        if is_device_mic {
            self.set_device_mic_recording_requested(enabled);
        }
        if enabled && capture_track.is_some() {
            self.restart_platform_recording_for_deep_filter();
        }
        for track in [Some(track), capture_track].into_iter().flatten() {
            if enabled {
                track.unmute();
            } else {
                track.mute();
            }
        }
    }

    fn restart_platform_recording_for_deep_filter(&self) {
        let Some(platform_audio) = self.platform_audio.lock().as_ref().cloned() else {
            return;
        };
        if let Err(error) = platform_audio.start_recording() {
            emit_deep_filter_status(
                &self.events,
                &self.dropped_engine_events,
                "degraded",
                &format!("restart recording: {error}"),
            );
        }
    }

    #[napi]
    pub async fn list_audio_output_devices(&self) -> napi::Result<String> {
        if self.platform_audio.lock().is_none() {
            return Ok(audio::default_output_devices_json());
        }
        run_audio_device_module_blocking(|| {
            let raw = collect_factory_playout_devices()?;
            let shaped = audio::shape_output_devices(&raw);
            Ok(audio::output_devices_json(&shaped))
        })
        .await
    }

    #[napi]
    pub async fn list_audio_input_devices(&self) -> napi::Result<String> {
        if self.platform_audio.lock().is_none() {
            return Ok(audio::default_input_devices_json());
        }
        run_audio_device_module_blocking(|| {
            let raw = collect_factory_recording_devices()?;
            let shaped = audio::shape_input_devices(&raw);
            Ok(audio::input_devices_json(&shaped))
        })
        .await
    }

    #[napi]
    pub async fn set_audio_output_device(&self, device_id: String) -> napi::Result<()> {
        run_audio_device_module_blocking(move || {
            let runtime = LkRuntime::instance();
            let factory = runtime.pc_factory();
            let raw = collect_factory_playout_devices_from(factory)?;
            let guid = audio::resolve_playout_device_guid(&device_id, &raw).map_err(|error| {
                napi::Error::from_reason(format!("set audio output device: {error}"))
            })?;
            let platform_playout_active =
                factory.is_platform_adm_active() && factory.adm_playout_enabled();
            let plan = audio::playout_switch_plan(
                platform_playout_active,
                factory.playout_is_initialized(),
            );
            match plan {
                audio::PlayoutSwitchPlan::ColdSelect => {
                    set_factory_playout_device_by_guid(factory, &guid)
                }
                audio::PlayoutSwitchPlan::HotSwap => {
                    hot_swap_factory_playout_device(factory, &guid)
                }
            }
        })
        .await
    }

    #[napi]
    pub async fn set_participant_volume(&self, participant_sid: String, volume: f64) {
        let clamped = audio::clamp_volume(volume);
        self.participant_volumes
            .lock()
            .insert(participant_sid.clone(), clamped);
        let muted = audio::is_muted_volume(clamped);

        let participant = {
            let guard = self.room.lock();
            guard.as_ref().and_then(|room| {
                room.remote_participants()
                    .into_values()
                    .find(|p| p.sid().to_string() == participant_sid)
            })
        };
        let Some(participant) = participant else {
            return;
        };
        for (_sid, publication) in participant.track_publications() {
            if let Some(RemoteTrack::Audio(track)) = publication.track() {
                if muted {
                    track.disable();
                } else {
                    track.enable();
                }
            }
        }
    }

    #[napi]
    pub async fn set_remote_track_subscription(
        &self,
        participant_identity: String,
        source: String,
        subscribed: bool,
        enabled: bool,
        quality: Option<String>,
    ) -> napi::Result<()> {
        let Some(target_source) = parse_track_source(&source) else {
            return Err(napi::Error::from_reason(format!(
                "unsupported remote track source: {source}"
            )));
        };
        let target_quality = quality.as_deref().and_then(parse_video_quality);
        let participant = {
            let guard = self.room.lock();
            guard.as_ref().and_then(|room| {
                room.remote_participants()
                    .into_values()
                    .find(|p| p.identity().to_string() == participant_identity)
            })
        };
        let Some(participant) = participant else {
            if subscribed {
                if is_optional_remote_subscription_target(target_source) {
                    return Ok(());
                }
                return Err(remote_subscription_target_error(
                    "participant",
                    &participant_identity,
                    &source,
                ));
            }
            return Ok(());
        };
        let mut visited = 0;
        let mut matched = 0;
        for publication in participant.track_publications().into_values() {
            visited += 1;
            if visited > REMOTE_TRACK_SUBSCRIPTION_PUBLICATIONS_MAX {
                return Err(napi::Error::from_reason(format!(
                    "remote track subscription publication scan exceeded cap {}",
                    REMOTE_TRACK_SUBSCRIPTION_PUBLICATIONS_MAX
                )));
            }
            if publication.source() != target_source {
                continue;
            }
            matched += 1;
            publication.set_subscribed(subscribed);
            if !subscribed {
                continue;
            }
            publication.set_enabled(enabled);
            if publication.kind() == TrackKind::Video
                && let Some(quality) = target_quality
            {
                publication.set_video_quality(quality);
            }
        }
        if subscribed && matched == 0 {
            if is_optional_remote_subscription_target(target_source) {
                return Ok(());
            }
            return Err(remote_subscription_target_error(
                "publication",
                &participant_identity,
                &source,
            ));
        }
        Ok(())
    }

    #[napi]
    pub async fn publish_data(
        &self,
        payload: Buffer,
        reliable: bool,
        topic: Option<String>,
        destination_identities: Option<Vec<String>>,
    ) -> napi::Result<()> {
        let local = self.local_participant()?;
        let packet = DataPacket {
            payload: payload.to_vec(),
            topic: topic.and_then(|value| {
                let trimmed = value.trim();
                if trimmed.is_empty() {
                    None
                } else {
                    Some(trimmed.to_string())
                }
            }),
            reliable,
            destination_identities: destination_identities
                .unwrap_or_default()
                .into_iter()
                .filter_map(|identity| {
                    let trimmed = identity.trim();
                    if trimmed.is_empty() {
                        None
                    } else {
                        Some(ParticipantIdentity(trimmed.to_string()))
                    }
                })
                .collect(),
        };
        local
            .publish_data(packet)
            .await
            .map_err(|e| napi::Error::from_reason(format!("publish data: {e}")))
    }

    async fn publish_camera_capture(
        &self,
        opts: CameraOptions,
        publication_kind: CameraPublicationKind,
        slot: &Mutex<Option<CameraSource>>,
    ) -> napi::Result<()> {
        if self.state.load(Ordering::Acquire) != S_CONNECTED {
            return Err(napi::Error::from_reason("not connected"));
        }
        let mut background = native_camera_background_config(&opts)?;
        background.live_background = Some(self.camera_live_background.clone());
        let request = camera::CameraRequest::from_opts(
            opts.device_id.as_deref(),
            opts.width,
            opts.height,
            opts.frame_rate,
            opts.mirror.unwrap_or(false),
            background,
        );
        request
            .background
            .ensure_supported_for_publish()
            .map_err(napi::Error::from_reason)?;

        let worker = open_camera_capture_worker(request.clone()).await?;
        let source = NativeVideoSource::new(
            VideoResolution {
                width: worker.opened.width & !1,
                height: worker.opened.height & !1,
            },
            publication_kind.is_screencast(),
        );
        let track = LocalVideoTrack::create_video_track(
            publication_kind.track_name(),
            RtcVideoSource::Native(source.clone()),
        );
        let options = build_camera_publish_options(&opts, publication_kind)?;
        let local = {
            let guard = self.room.lock();
            guard
                .as_ref()
                .map(|room| room.local_participant())
                .ok_or_else(|| napi::Error::from_reason("not connected"))?
        };
        let publication = match local.publish_track(LocalTrack::Video(track), options).await {
            Ok(p) => p,
            Err(e) => {
                worker.stop.store(true, Ordering::Release);
                return Err(napi::Error::from_reason(format!(
                    "publish {}: {e}",
                    publication_kind.track_name()
                )));
            }
        };

        let track_sid = Arc::new(Mutex::new(publication.sid().to_string()));
        let capture = DeviceCameraCapture {
            source,
            participant_sid: local.sid().to_string(),
            participant_identity: local.identity().to_string(),
            track_name: publication.name(),
            track_source: events::track_source_str(publication.source()).to_string(),
            request,
        };
        store_camera_slot(
            slot,
            CameraSource::Device {
                track_sid: track_sid.clone(),
                stop: worker.stop.clone(),
                capture: Some(capture.clone()),
            },
        );
        reconcile_camera_slot_sid(&local, &track_sid, publication_kind.track_source());
        let sinks = self.device_camera_capture_sinks(&capture, &track_sid);
        if worker.sinks_tx.send(sinks).is_err() {
            worker.stop.store(true, Ordering::Release);
            remove_camera_slot_if_held(slot, &track_sid);
            let current_sid = track_sid.lock().clone();
            assert!(!current_sid.is_empty());
            if let Ok(current_sid) = TrackSid::try_from(current_sid) {
                let _ = local.unpublish_track(&current_sid).await;
            }
            return Err(napi::Error::from_reason(
                "camera worker exited before publish",
            ));
        }

        Ok(())
    }

    fn device_camera_capture_sinks(
        &self,
        capture: &DeviceCameraCapture,
        track_sid: &Arc<Mutex<String>>,
    ) -> camera::CameraCaptureSinks {
        assert!(!capture.participant_sid.is_empty());
        assert!(!track_sid.lock().is_empty());
        let frame_sink = local_video_frame_sink(
            self.video_frames.clone(),
            self.dropped_video_frame_callbacks.clone(),
            capture.participant_sid.clone(),
            capture.participant_identity.clone(),
            track_sid.clone(),
            capture.track_name.clone(),
            capture.track_source.clone(),
        );
        let frame_sink_active = local_video_frame_sink_active(self.video_frames.clone());
        camera::CameraCaptureSinks {
            source: capture.source.clone(),
            frame_sink,
            frame_sink_active,
        }
    }

    async fn publish_native_camera_sink_source(
        &self,
        opts: CameraOptions,
        slot: &Mutex<Option<CameraSource>>,
    ) -> napi::Result<Arc<Mutex<String>>> {
        if self.state.load(Ordering::Acquire) != S_CONNECTED {
            return Err(napi::Error::from_reason("not connected"));
        }
        if !crate::native_camera::platform_native_backgrounds_available() {
            return Err(napi::Error::from_reason(
                crate::native_camera::unavailable_error(),
            ));
        }
        let width = opts.width.unwrap_or(camera::DEFAULT_WIDTH) & !1;
        let height = opts.height.unwrap_or(camera::DEFAULT_HEIGHT) & !1;
        let frame_rate = opts.frame_rate.unwrap_or(camera::DEFAULT_FPS);
        if !valid_even_video_dims(width, height) || frame_rate == 0 {
            return Err(napi::Error::from_reason(
                "invalid native camera sink options",
            ));
        }
        native_camera_background_config(&opts)?
            .ensure_supported_for_publish()
            .map_err(napi::Error::from_reason)?;

        let source = NativeVideoSource::new(VideoResolution { width, height }, false);
        let track = LocalVideoTrack::create_video_track(
            CameraPublicationKind::Camera.track_name(),
            RtcVideoSource::Native(source.clone()),
        );
        let options = build_camera_publish_options(&opts, CameraPublicationKind::Camera)?;
        let local = {
            let guard = self.room.lock();
            guard
                .as_ref()
                .map(|room| room.local_participant())
                .ok_or_else(|| napi::Error::from_reason("not connected"))?
        };
        let publication = local
            .publish_track(LocalTrack::Video(track), options)
            .await
            .map_err(|e| napi::Error::from_reason(format!("publish native camera sink: {e}")))?;

        let target_fps = f64::from(frame_rate);
        let stats = Arc::new(AdaptiveVideoStats::new(
            target_fps,
            DEFAULT_MIN_VIDEO_FPS.min(target_fps),
            false,
            now_millis(),
        ));
        let video_sender =
            AdaptiveVideoSender::new(source, stats, VideoPacingMode::Source, target_fps);
        let frame_sink = Arc::new(BusSenderSink {
            sender: video_sender.clone(),
            texture_capability: *self.texture_capability.lock(),
        });
        let track_sid = Arc::new(Mutex::new(publication.sid().to_string()));
        store_camera_slot(
            slot,
            CameraSource::NativeBuffered {
                track_sid: track_sid.clone(),
                video_sender,
                frame_sink,
            },
        );
        reconcile_camera_slot_sid(&local, &track_sid, TrackSource::Camera);
        Ok(track_sid)
    }

    #[napi]
    pub async fn publish_camera(&self, opts: CameraOptions) -> napi::Result<()> {
        if self.camera.lock().is_some() {
            return Err(napi::Error::from_reason("camera already published"));
        }
        self.stop_camera_preview_capture();
        self.publish_camera_capture(opts, CameraPublicationKind::Camera, &self.camera)
            .await
    }

    #[napi]
    pub async fn publish_native_camera_sink(
        &self,
        opts: CameraOptions,
    ) -> napi::Result<ProcessedCameraPublishResult> {
        if self.camera.lock().is_some() {
            return Err(napi::Error::from_reason("camera already published"));
        }
        self.stop_camera_preview_capture();
        let track_sid = self
            .publish_native_camera_sink_source(opts, &self.camera)
            .await?;
        let track_sid = track_sid.lock().clone();
        assert!(!track_sid.is_empty());
        Ok(ProcessedCameraPublishResult { track_sid })
    }

    #[napi]
    pub fn create_camera_frame_sink_handle<'env>(
        &self,
        env: Env,
    ) -> napi::Result<Option<Unknown<'env>>> {
        let sink = {
            let guard = self.camera.lock();
            let Some(camera) = guard.as_ref() else {
                return Ok(None);
            };
            let Some(sink) = camera.native_frame_sink() else {
                return Ok(None);
            };
            sink
        };
        let external = ScreenFrameSinkHandleExternal {
            handle: create_native_screen_frame_sink_handle(sink),
        };
        let raw_env = env.raw();
        let value = unsafe { ScreenFrameSinkHandleExternal::to_napi_value(raw_env, external)? };
        Ok(Some(unsafe { Unknown::from_raw_unchecked(raw_env, value) }))
    }

    async fn publish_processed_camera_source(
        &self,
        opts: ProcessedCameraOptions,
        slot: &Mutex<Option<CameraSource>>,
    ) -> napi::Result<Arc<Mutex<String>>> {
        if self.state.load(Ordering::Acquire) != S_CONNECTED {
            return Err(napi::Error::from_reason("not connected"));
        }
        if !valid_even_video_dims(opts.width, opts.height) || opts.frame_rate == 0 {
            return Err(napi::Error::from_reason("invalid processed camera options"));
        }
        let source = NativeVideoSource::new(
            VideoResolution {
                width: opts.width,
                height: opts.height,
            },
            false,
        );
        let track = LocalVideoTrack::create_video_track(
            CameraPublicationKind::Camera.track_name(),
            RtcVideoSource::Native(source.clone()),
        );
        let camera_opts = CameraOptions {
            device_id: None,
            width: Some(opts.width),
            height: Some(opts.height),
            frame_rate: Some(opts.frame_rate),
            mirror: None,
            background_mode: None,
            background_custom_media_path: None,
            background_custom_media_kind: None,
            background_blur_strength: None,
            codec: None,
            max_bitrate_bps: None,
            max_framerate: Some(f64::from(opts.frame_rate)),
        };
        let options = build_camera_publish_options(&camera_opts, CameraPublicationKind::Camera)?;
        let local = {
            let guard = self.room.lock();
            guard
                .as_ref()
                .map(|room| room.local_participant())
                .ok_or_else(|| napi::Error::from_reason("not connected"))?
        };
        let publication = local
            .publish_track(LocalTrack::Video(track), options)
            .await
            .map_err(|e| napi::Error::from_reason(format!("publish processed camera: {e}")))?;
        let target_fps = f64::from(opts.frame_rate);
        let stats = Arc::new(AdaptiveVideoStats::new(
            target_fps,
            DEFAULT_MIN_VIDEO_FPS.min(target_fps),
            false,
            now_millis(),
        ));
        let video_sender =
            AdaptiveVideoSender::new(source, stats, VideoPacingMode::Source, target_fps);
        let track_sid = Arc::new(Mutex::new(publication.sid().to_string()));
        store_camera_slot(
            slot,
            CameraSource::Processed {
                track_sid: track_sid.clone(),
                video_sender,
            },
        );
        reconcile_camera_slot_sid(&local, &track_sid, TrackSource::Camera);
        Ok(track_sid)
    }

    #[napi]
    pub async fn publish_processed_camera(
        &self,
        opts: ProcessedCameraOptions,
    ) -> napi::Result<ProcessedCameraPublishResult> {
        if self.camera.lock().is_some() {
            return Err(napi::Error::from_reason("camera already published"));
        }
        self.stop_camera_preview_capture();
        let track_sid = self
            .publish_processed_camera_source(opts, &self.camera)
            .await?;
        let track_sid = track_sid.lock().clone();
        assert!(!track_sid.is_empty());
        Ok(ProcessedCameraPublishResult { track_sid })
    }

    #[napi]
    pub fn push_processed_camera_frame(&self, frame: ProcessedCameraFrame) -> napi::Result<bool> {
        let sender = {
            let guard = self.camera.lock();
            guard.as_ref().and_then(CameraSource::processed_sender)
        };
        let Some(sender) = sender else {
            return Ok(false);
        };
        let pending = processed_camera_frame_to_pending(frame)?;
        Ok(!matches!(sender.enqueue(pending), EnqueueResult::Rejected))
    }

    #[napi]
    pub fn push_camera_background_frame(&self, frame: ProcessedCameraFrame) -> napi::Result<bool> {
        validate_processed_camera_frame(&frame)?;
        Ok(self.camera_live_background.store_tight_i420(
            frame.data.as_ref(),
            frame.width,
            frame.height,
        ))
    }

    #[napi]
    pub fn clear_camera_background_frame(&self) {
        self.camera_live_background.clear();
    }

    #[napi]
    pub async fn publish_device_screen_share(&self, opts: CameraOptions) -> napi::Result<()> {
        self.unpublish_screen_share().await?;
        self.publish_camera_capture(
            opts,
            CameraPublicationKind::ScreenShare,
            &self.screen_camera,
        )
        .await
    }

    #[napi]
    pub fn list_camera_devices(&self) -> napi::Result<Vec<CameraDeviceInfo>> {
        camera::list_devices()
            .map(|devices| {
                devices
                    .into_iter()
                    .map(|device| CameraDeviceInfo {
                        device_id: device.device_id,
                        label: device.label,
                        description: device.description,
                        index: device.index,
                        device_id_aliases: device.device_id_aliases,
                    })
                    .collect()
            })
            .map_err(napi::Error::from_reason)
    }

    #[napi]
    pub async fn unpublish_camera(&self) -> napi::Result<()> {
        let Some(cam) = self.camera.lock().take() else {
            return Ok(());
        };
        cam.stop();
        let track_sid = cam.track_sid().lock().clone();
        assert!(!track_sid.is_empty());
        let local = {
            let guard = self.room.lock();
            guard.as_ref().map(|room| room.local_participant())
        };
        let Some(local) = local else {
            return Ok(());
        };
        let track_sid = TrackSid::try_from(track_sid).map_err(|sid| {
            napi::Error::from_reason(format!("unpublish camera: invalid track sid {sid}"))
        })?;
        local
            .unpublish_track(&track_sid)
            .await
            .map_err(|e| napi::Error::from_reason(format!("unpublish camera: {e}")))?;
        Ok(())
    }

    #[napi]
    pub fn is_publishing_camera(&self) -> bool {
        self.camera.lock().is_some()
    }

    #[napi]
    pub async fn start_camera_preview(
        &self,
        opts: CameraOptions,
    ) -> napi::Result<CameraPreviewInfo> {
        if self.camera.lock().is_some() {
            return Err(napi::Error::from_reason(
                "camera already published; use the published camera stream for preview",
            ));
        }
        self.stop_camera_preview_capture();
        let mut background = native_camera_background_config(&opts)?;
        background.live_background = Some(self.camera_live_background.clone());
        let request = camera::CameraRequest::from_opts(
            opts.device_id.as_deref(),
            opts.width,
            opts.height,
            opts.frame_rate,
            opts.mirror.unwrap_or(false),
            background,
        );
        request
            .background
            .ensure_supported_for_publish()
            .map_err(napi::Error::from_reason)?;

        let stop = Arc::new(AtomicBool::new(false));
        let (result_tx, result_rx) = std::sync::mpsc::channel();
        let (source_tx, source_rx) = std::sync::mpsc::channel();
        camera::spawn_capture_worker(request, result_tx, source_rx, stop.clone());

        let opened = tokio::task::spawn_blocking(move || result_rx.recv())
            .await
            .map_err(|e| napi::Error::from_reason(format!("camera preview open task: {e}")))?
            .map_err(|_| napi::Error::from_reason("camera preview worker exited before open"))?
            .map_err(napi::Error::from_reason)?;
        let width = opened.width & !1;
        let height = opened.height & !1;
        assert!(width >= 2);
        assert!(height >= 2);

        let source = NativeVideoSource::new(VideoResolution { width, height }, false);
        let frame_sink = local_video_frame_sink(
            self.video_frames.clone(),
            self.dropped_video_frame_callbacks.clone(),
            CAMERA_PREVIEW_TRACK_SID.to_string(),
            String::new(),
            Arc::new(Mutex::new(CAMERA_PREVIEW_TRACK_SID.to_string())),
            "camera-preview".to_string(),
            events::track_source_str(TrackSource::Camera).to_string(),
        );
        let frame_sink_active = local_video_frame_sink_active(self.video_frames.clone());
        let sinks = camera::CameraCaptureSinks {
            source,
            frame_sink,
            frame_sink_active,
        };
        if source_tx.send(sinks).is_err() {
            stop.store(true, Ordering::Release);
            return Err(napi::Error::from_reason(
                "camera preview worker exited before start",
            ));
        }

        if let Some(previous) = self.camera_preview.lock().replace(stop) {
            previous.store(true, Ordering::Release);
        }
        Ok(CameraPreviewInfo {
            track_sid: CAMERA_PREVIEW_TRACK_SID.to_string(),
            width,
            height,
            frame_rate: opened.fps,
        })
    }

    #[napi]
    pub fn stop_camera_preview(&self) {
        self.stop_camera_preview_capture();
    }

    fn stop_camera_preview_capture(&self) {
        if let Some(stop) = self.camera_preview.lock().take() {
            stop.store(true, Ordering::Release);
        }
        assert!(self.camera_preview.lock().is_none());
    }

    fn device_camera_capture(
        &self,
    ) -> napi::Result<(DeviceCameraCapture, Arc<Mutex<String>>, Arc<AtomicBool>)> {
        let guard = self.camera.lock();
        match guard.as_ref() {
            None => Err(napi::Error::from_reason("camera is not published")),
            Some(CameraSource::Device {
                track_sid,
                stop,
                capture: Some(capture),
            }) => Ok((capture.clone(), track_sid.clone(), stop.clone())),
            Some(_) => Err(napi::Error::from_reason(
                "camera capture is not device-managed",
            )),
        }
    }

    #[napi]
    pub async fn update_camera_capture(&self, opts: CameraOptions) -> napi::Result<()> {
        let (capture, track_sid, old_stop) = self.device_camera_capture()?;
        self.stop_camera_preview_capture();
        let mut background = native_camera_background_config(&opts)?;
        background.live_background = Some(self.camera_live_background.clone());
        let request = camera::CameraRequest::from_opts(
            opts.device_id.as_deref(),
            opts.width,
            opts.height,
            opts.frame_rate,
            opts.mirror.unwrap_or(false),
            background,
        );
        request
            .background
            .ensure_supported_for_publish()
            .map_err(napi::Error::from_reason)?;
        match camera_swap_order(&capture.request.selector, &request.selector) {
            CameraSwapOrder::OpenNewThenStopOld => {
                self.swap_camera_open_new_then_stop_old(&capture, &track_sid, &old_stop, request)
                    .await
            }
            CameraSwapOrder::StopOldThenOpenNew => {
                self.swap_camera_stop_old_then_open_new(&capture, &track_sid, &old_stop, request)
                    .await
            }
        }
    }

    async fn start_device_camera_worker(
        &self,
        capture: &DeviceCameraCapture,
        track_sid: &Arc<Mutex<String>>,
        request: camera::CameraRequest,
    ) -> napi::Result<Arc<AtomicBool>> {
        let worker = open_camera_capture_worker(request).await?;
        let sinks = self.device_camera_capture_sinks(capture, track_sid);
        if worker.sinks_tx.send(sinks).is_err() {
            worker.stop.store(true, Ordering::Release);
            return Err(napi::Error::from_reason(
                "camera worker exited before capture",
            ));
        }
        Ok(worker.stop)
    }

    async fn swap_camera_open_new_then_stop_old(
        &self,
        capture: &DeviceCameraCapture,
        track_sid: &Arc<Mutex<String>>,
        old_stop: &Arc<AtomicBool>,
        request: camera::CameraRequest,
    ) -> napi::Result<()> {
        let swapped_stop = self
            .start_device_camera_worker(capture, track_sid, request.clone())
            .await?;
        old_stop.store(true, Ordering::Release);
        let committed =
            commit_device_camera_swap(&self.camera, track_sid, old_stop, &swapped_stop, &request);
        if !committed {
            swapped_stop.store(true, Ordering::Release);
            return Err(napi::Error::from_reason(
                "camera capture changed during update",
            ));
        }
        Ok(())
    }

    async fn swap_camera_stop_old_then_open_new(
        &self,
        capture: &DeviceCameraCapture,
        track_sid: &Arc<Mutex<String>>,
        old_stop: &Arc<AtomicBool>,
        request: camera::CameraRequest,
    ) -> napi::Result<()> {
        old_stop.store(true, Ordering::Release);
        let error = match self
            .start_device_camera_worker(capture, track_sid, request.clone())
            .await
        {
            Ok(swapped_stop) => {
                let committed = commit_device_camera_swap(
                    &self.camera,
                    track_sid,
                    old_stop,
                    &swapped_stop,
                    &request,
                );
                if !committed {
                    swapped_stop.store(true, Ordering::Release);
                    return Err(napi::Error::from_reason(
                        "camera capture changed during update",
                    ));
                }
                return Ok(());
            }
            Err(error) => error,
        };
        let restored_stop = self
            .start_device_camera_worker(capture, track_sid, capture.request.clone())
            .await
            .map_err(|restore_error| {
                napi::Error::from_reason(format!(
                    "{}; restoring previous camera capture failed: {}",
                    error.reason, restore_error.reason
                ))
            })?;
        let committed = commit_device_camera_swap(
            &self.camera,
            track_sid,
            old_stop,
            &restored_stop,
            &capture.request,
        );
        if !committed {
            restored_stop.store(true, Ordering::Release);
            return Err(napi::Error::from_reason(
                "camera capture changed during update",
            ));
        }
        Err(error)
    }

    #[napi]
    pub async fn get_connection_stats(&self) -> String {
        if let Some(json) = self.last_stats_json.lock().clone() {
            return json;
        }
        let stats = self.collect_stats().await;
        stats_mod::stats_to_json(&stats)
    }

    async fn collect_stats(&self) -> stats_mod::ConnectionStats {
        let snapshot = {
            let guard = self.room.lock();
            guard.as_ref().map(snapshot_room_participants)
        };
        let screen_metadata = self.screen_source_metadata();
        let Some((room, local, remotes)) = snapshot else {
            return stats_mod::ConnectionStats {
                send: Some(self.send_health_snapshot()),
                ..Default::default()
            };
        };
        let mut stats = collect_stats_for(local, remotes, &self.byte_samples).await;
        absorb_room_rtt(&room, &mut stats).await;
        self.record_outbound_video_egress_fps(&stats);
        let send = self.send_health_snapshot();
        annotate_screen_share_stats(&mut stats, screen_metadata, &send);
        stats.send = Some(send);
        stats
    }

    fn record_outbound_video_egress_fps(&self, stats: &stats_mod::ConnectionStats) {
        let video_stats = self.send_video_stats.lock().clone();
        if let (Some(video_stats), Some(fps)) = (video_stats, outbound_screenshare_video_fps(stats))
        {
            video_stats.record_egress_fps(now_millis(), fps);
        }
    }

    fn video_telemetry_extras(&self) -> VideoTelemetryExtras {
        let screen = self.screen.lock();
        match screen.as_ref() {
            Some(screen) => VideoTelemetryExtras {
                pacing_mode: screen.video_sender.pacing.as_label().to_string(),
                pacing_target_fps: screen.video_sender.target_fps,
                queue_capacity: ENCODER_QUEUE_CAPACITY as u64,
                bus_active: screen.bus_capture_id.is_some(),
            },
            None => VideoTelemetryExtras::default(),
        }
    }

    fn send_health_snapshot(&self) -> SendHealthSnapshot {
        let video_stats = self.send_video_stats.lock().clone();
        match video_stats {
            Some(stats) => stats.snapshot(&self.send_audio_stats, self.video_telemetry_extras()),
            None => SendHealthSnapshot::idle(&self.send_audio_stats),
        }
    }

    fn screen_source_metadata(&self) -> Option<ScreenSourceMetadata> {
        self.screen
            .lock()
            .as_ref()
            .map(|screen| screen.metadata.clone())
    }

    fn start_stats_task(&self) {
        if self.stats_running.swap(true, Ordering::AcqRel) {
            return;
        }
        let state = self.state.clone();
        let room_slot = self.room.clone();
        let screen_slot = self.screen.clone();
        let events_slot = self.events.clone();
        let dropped_engine_events = self.dropped_engine_events.clone();
        let byte_samples = self.byte_samples.clone();
        let last_stats_json = self.last_stats_json.clone();
        let send_video_stats = self.send_video_stats.clone();
        let send_audio_stats = self.send_audio_stats.clone();
        tokio::spawn(async move {
            let mut interval = tokio::time::interval(std::time::Duration::from_secs(1));
            loop {
                interval.tick().await;
                if state.load(Ordering::Acquire) != S_CONNECTED {
                    continue;
                }
                let snapshot = {
                    let guard = room_slot.lock();
                    guard.as_ref().map(snapshot_room_participants)
                };
                let Some((room, local, remotes)) = snapshot else {
                    continue;
                };
                let mut stats = collect_stats_for(local, remotes, &byte_samples).await;
                absorb_room_rtt(&room, &mut stats).await;
                let video_stats = send_video_stats.lock().clone();
                if let (Some(video), Some(fps)) =
                    (video_stats.as_ref(), outbound_screenshare_video_fps(&stats))
                {
                    video.record_egress_fps(now_millis(), fps);
                }
                let extras = {
                    let screen = screen_slot.lock();
                    match screen.as_ref() {
                        Some(screen) => VideoTelemetryExtras {
                            pacing_mode: screen.video_sender.pacing.as_label().to_string(),
                            pacing_target_fps: screen.video_sender.target_fps,
                            queue_capacity: ENCODER_QUEUE_CAPACITY as u64,
                            bus_active: screen.bus_capture_id.is_some(),
                        },
                        None => VideoTelemetryExtras::default(),
                    }
                };
                let send = match video_stats {
                    Some(video) => video.snapshot(&send_audio_stats, extras),
                    None => SendHealthSnapshot::idle(&send_audio_stats),
                };
                let screen_metadata = screen_slot
                    .lock()
                    .as_ref()
                    .map(|screen| screen.metadata.clone());
                annotate_screen_share_stats(&mut stats, screen_metadata, &send);
                stats.send = Some(send);
                let json = stats_mod::stats_to_json(&stats);
                *last_stats_json.lock() = Some(json.clone());
                emit_engine_event(
                    &events_slot,
                    &dropped_engine_events,
                    "stats".to_string(),
                    json,
                );
            }
        });
    }

    #[napi]
    pub fn inbound_audio_frames(&self) -> f64 {
        self.inbound_audio.load(Ordering::Relaxed) as f64
    }

    #[napi]
    pub fn inbound_video_frames(&self) -> f64 {
        self.inbound_video.load(Ordering::Relaxed) as f64
    }

    #[napi]
    pub fn dropped_video_frame_callbacks(&self) -> f64 {
        self.dropped_video_frame_callbacks.load(Ordering::Relaxed) as f64
    }

    #[napi]
    pub fn dropped_engine_events(&self) -> f64 {
        self.dropped_engine_events.load(Ordering::Relaxed) as f64
    }
}

#[napi(object)]
#[derive(Default)]
pub struct VoiceEngineV2BridgeConnectOptions {
    pub auto_subscribe: Option<bool>,
    pub adaptive_stream: Option<bool>,
    pub dynacast: Option<bool>,
}

#[napi(object)]
pub struct ScreenSharePublishOptions {
    pub adaptive_send: Option<bool>,
    pub min_video_fps: Option<f64>,
    pub max_audio_buffer_ms: Option<u32>,
    pub pacing: Option<String>,
    pub capture_id: Option<String>,
    pub track_name: Option<String>,
}

#[napi(object)]
pub struct MicrophoneOptions {
    pub device_id: Option<String>,
    pub echo_cancellation: Option<bool>,
    pub noise_suppression: Option<bool>,
    pub auto_gain_control: Option<bool>,
    pub deep_filter: Option<bool>,
    pub deep_filter_noise_reduction_level: Option<f64>,
    pub max_bitrate_bps: Option<f64>,
}

#[napi(object)]
pub struct CameraOptions {
    pub device_id: Option<String>,
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub frame_rate: Option<u32>,
    pub mirror: Option<bool>,
    pub background_mode: Option<String>,
    pub background_custom_media_path: Option<String>,
    pub background_custom_media_kind: Option<String>,
    pub background_blur_strength: Option<u32>,
    pub codec: Option<String>,
    pub max_bitrate_bps: Option<f64>,
    pub max_framerate: Option<f64>,
}

#[napi(object)]
pub struct ProcessedCameraOptions {
    pub width: u32,
    pub height: u32,
    pub frame_rate: u32,
}

#[napi(object)]
pub struct ProcessedCameraPublishResult {
    pub track_sid: String,
}

#[napi(object)]
pub struct CameraPreviewInfo {
    pub track_sid: String,
    pub width: u32,
    pub height: u32,
    pub frame_rate: u32,
}

#[napi(object)]
pub struct ProcessedCameraFrame {
    pub format: String,
    pub width: u32,
    pub height: u32,
    pub timestamp_us: i64,
    pub data: Buffer,
}

#[napi(object)]
pub struct CameraDeviceInfo {
    pub device_id: String,
    pub label: String,
    pub description: String,
    pub index: Option<u32>,
    pub device_id_aliases: Vec<String>,
}

fn snapshot_participants(room: &Room) -> (LocalParticipant, Vec<RemoteParticipant>) {
    (
        room.local_participant(),
        room.remote_participants().into_values().collect(),
    )
}

fn snapshot_room_participants(
    room: &Arc<Room>,
) -> (Arc<Room>, LocalParticipant, Vec<RemoteParticipant>) {
    let (local, remotes) = snapshot_participants(room);
    (room.clone(), local, remotes)
}

fn outbound_screenshare_video_fps(stats: &stats_mod::ConnectionStats) -> Option<f64> {
    stats
        .outbound
        .iter()
        .filter(|entry| entry.kind == "video" && entry.source == LIVEKIT_TRACK_SOURCE_SCREEN_SHARE)
        .filter_map(|entry| entry.fps)
        .filter(|fps| fps.is_finite() && *fps >= 0.0)
        .fold(None, |best, fps| {
            Some(best.map_or(fps, |value: f64| value.max(fps)))
        })
}

fn annotate_screen_share_stats(
    stats: &mut stats_mod::ConnectionStats,
    metadata: Option<ScreenSourceMetadata>,
    send: &SendHealthSnapshot,
) {
    let Some(metadata) = metadata else {
        return;
    };
    for entry in stats
        .outbound
        .iter_mut()
        .filter(|entry| entry.kind == "video" && entry.source == LIVEKIT_TRACK_SOURCE_SCREEN_SHARE)
    {
        if entry.track_sid != metadata.track_sid.to_string() {
            continue;
        }
        if entry.codec.is_none() && !metadata.codec.is_empty() {
            entry.codec = Some(metadata.codec.clone());
        }
        entry.width = Some(metadata.width);
        entry.height = Some(metadata.height);
        entry.source_width = Some(metadata.width);
        entry.source_height = Some(metadata.height);
        entry.target_bitrate_kbps = metadata.target_bitrate_kbps;
        entry.configured_fps = Some(metadata.configured_fps);
        entry.target_fps = Some(send.outgoing_video_target_fps);
        entry.effective_fps = Some(send.outgoing_video_effective_fps);
        entry.frames_produced = Some(send.outgoing_video_frames_produced);
        entry.frames_accepted = Some(send.outgoing_video_frames_accepted);
        entry.frames_dropped = Some(send.outgoing_video_frames_dropped);
        entry.frames_coalesced = Some(send.outgoing_video_frames_coalesced);
        entry.frames_captured = Some(send.outgoing_video_frames_captured);
        entry.capture_failures = Some(send.outgoing_video_capture_failures);
        entry.max_queue_age_ms = Some(send.outgoing_video_max_queue_age_ms);
        entry.max_push_latency_ms = Some(send.outgoing_video_max_push_latency_ms);
        entry.adaptive_send_tier = Some(send.adaptive_send_tier.clone());
        entry.adaptive_send_reason = Some(send.adaptive_send_reason.clone());
    }
}

async fn collect_stats_for(
    local: LocalParticipant,
    remotes: Vec<RemoteParticipant>,
    byte_samples: &Arc<Mutex<HashMap<String, stats_mod::ByteRateSample>>>,
) -> stats_mod::ConnectionStats {
    let mut out = stats_mod::ConnectionStats::default();

    for (sid, publication) in local.track_publications() {
        let Some(track) = publication.track() else {
            continue;
        };
        let reports = match track.get_stats().await {
            Ok(r) => r,
            Err(_) => continue,
        };
        let codec_map = codec_mime_map(&reports);
        let source = events::track_source_str(publication.source()).to_string();
        let kind = events::track_kind_str(publication.kind()).to_string();
        let track_sid = sid.to_string();
        let audio_level_by_source_id = audio_level_by_source_id(&reports);
        absorb_rtt(&reports, &mut out);
        for report in &reports {
            if let RtcStats::OutboundRtp(o) = report {
                let bitrate =
                    sample_bitrate(byte_samples, &o.rtc.id, o.sent.bytes_sent, o.rtc.timestamp);
                let fps = if kind == "video" {
                    Some(o.outbound.frames_per_second)
                } else {
                    None
                };
                out.outbound.push(stats_mod::OutboundEntry {
                    track_sid: track_sid.clone(),
                    source: source.clone(),
                    kind: kind.clone(),
                    codec: codec_mime_for(&codec_map, &o.stream.codec_id),
                    bitrate_kbps: bitrate,
                    packets_lost: 0,
                    packets_sent: o.sent.packets_sent,
                    fps,
                    audio_level: if kind == "audio" {
                        audio_level_by_source_id
                            .get(&o.outbound.media_source_id)
                            .copied()
                            .flatten()
                    } else {
                        None
                    },
                    ..Default::default()
                });
            } else if let RtcStats::RemoteInboundRtp(ri) = report
                && let Some(entry) = out.outbound.iter_mut().find(|e| e.track_sid == track_sid)
            {
                entry.packets_lost = ri.received.packets_lost;
            }
        }
    }

    for participant in remotes {
        let participant_sid = participant.sid().to_string();
        for (sid, publication) in participant.track_publications() {
            let Some(track) = publication.track() else {
                continue;
            };
            let reports = match track.get_stats().await {
                Ok(r) => r,
                Err(_) => continue,
            };
            let codec_map = codec_mime_map(&reports);
            let kind = events::track_kind_str(publication.kind()).to_string();
            let source = events::track_source_str(publication.source()).to_string();
            let participant_identity = participant.identity().to_string();
            let track_sid = sid.to_string();
            absorb_rtt(&reports, &mut out);
            for report in &reports {
                if let RtcStats::InboundRtp(i) = report {
                    let bitrate = sample_bitrate(
                        byte_samples,
                        &i.rtc.id,
                        i.inbound.bytes_received,
                        i.rtc.timestamp,
                    );
                    let jitter_ms = stats_mod::jitter_seconds_to_ms(i.received.jitter);
                    let audio_level = if kind == "audio" {
                        stats_mod::sanitize_audio_level(i.inbound.audio_level)
                    } else {
                        None
                    };
                    out.inbound.push(stats_mod::InboundEntry {
                        participant_sid: participant_sid.clone(),
                        participant_identity: Some(participant_identity.clone()),
                        track_sid: track_sid.clone(),
                        source: Some(source.clone()),
                        kind: kind.clone(),
                        codec: codec_mime_for(&codec_map, &i.stream.codec_id),
                        bitrate_kbps: bitrate,
                        packets_lost: i.received.packets_lost,
                        packets_received: i.received.packets_received,
                        jitter_ms,
                        audio_level,
                        fps: optional_non_negative_f64(i.inbound.frames_per_second),
                        width: optional_positive_u32(i.inbound.frame_width),
                        height: optional_positive_u32(i.inbound.frame_height),
                        source_width: optional_positive_u32(i.inbound.frame_width),
                        source_height: optional_positive_u32(i.inbound.frame_height),
                    });
                }
            }
        }
    }

    out
}

fn optional_positive_u32(value: u32) -> Option<u32> {
    if value > 0 { Some(value) } else { None }
}

fn optional_non_negative_f64(value: f64) -> Option<f64> {
    if value.is_finite() && value >= 0.0 {
        Some(value)
    } else {
        None
    }
}

fn audio_level_by_source_id(reports: &[RtcStats]) -> HashMap<String, Option<f64>> {
    reports
        .iter()
        .filter_map(|report| {
            if let RtcStats::MediaSource(source) = report
                && source.source.kind == "audio"
                && !source.rtc.id.is_empty()
            {
                return Some((
                    source.rtc.id.clone(),
                    stats_mod::sanitize_audio_level(source.audio.audio_level),
                ));
            }
            None
        })
        .collect()
}

async fn absorb_room_rtt(room: &Room, out: &mut stats_mod::ConnectionStats) {
    if out.rtt_ms.is_some() {
        return;
    }

    let Ok(stats) = room.get_stats().await else {
        return;
    };
    absorb_session_rtt(&stats.publisher_stats, &stats.subscriber_stats, out);
}

fn absorb_session_rtt(
    publisher_reports: &[RtcStats],
    subscriber_reports: &[RtcStats],
    out: &mut stats_mod::ConnectionStats,
) {
    absorb_rtt(publisher_reports, out);
    absorb_rtt(subscriber_reports, out);
}

fn codec_mime_map(reports: &[RtcStats]) -> HashMap<String, String> {
    reports
        .iter()
        .filter_map(|report| match report {
            RtcStats::Codec(codec)
                if !codec.rtc.id.is_empty() && !codec.codec.mime_type.is_empty() =>
            {
                Some((codec.rtc.id.clone(), codec.codec.mime_type.clone()))
            }
            _ => None,
        })
        .collect()
}

fn codec_mime_for(codec_map: &HashMap<String, String>, codec_id: &str) -> Option<String> {
    if codec_id.is_empty() {
        return None;
    }
    codec_map.get(codec_id).cloned()
}

fn absorb_rtt(reports: &[RtcStats], out: &mut stats_mod::ConnectionStats) {
    if out.rtt_ms.is_some() {
        return;
    }
    for report in reports {
        if let RtcStats::CandidatePair(cp) = report
            && cp.candidate_pair.nominated
            && let Some(ms) =
                stats_mod::rtt_seconds_to_ms(cp.candidate_pair.current_round_trip_time)
        {
            out.rtt_ms = Some(ms);
            return;
        }
    }
}

fn sample_bitrate(
    byte_samples: &Arc<Mutex<HashMap<String, stats_mod::ByteRateSample>>>,
    stat_id: &str,
    bytes: u64,
    timestamp_us: i64,
) -> f64 {
    let cur = stats_mod::ByteRateSample {
        bytes,
        timestamp_us,
    };
    let mut guard = byte_samples.lock();
    let prev = guard.get(stat_id).copied();
    let kbps = stats_mod::bitrate_kbps(prev, cur);
    guard.insert(stat_id.to_string(), cur);
    kbps
}

fn copy_plane(dst: &mut [u8], src: &[u8], width: usize, dst_stride: usize, rows: usize) {
    assert!(width >= 1);
    assert!(rows >= 1);
    assert!(dst_stride >= width);
    assert!(src.len() >= width * rows);
    assert!(dst.len() >= dst_stride * (rows - 1) + width);
    if dst_stride == width {
        dst[..width * rows].copy_from_slice(&src[..width * rows]);
        return;
    }
    for row in 0..rows {
        let s = row * width;
        let d = row * dst_stride;
        dst[d..d + width].copy_from_slice(&src[s..s + width]);
    }
}

fn tight_i420_len(width: u32, height: u32) -> usize {
    let w = width as usize;
    let h = height as usize;
    let cw = w / 2;
    let ch = h / 2;
    w * h + 2 * cw * ch
}

fn video_frame_meta_json(
    participant_sid: &str,
    participant_identity: &str,
    track_sid: &str,
    track_name: &str,
    track_source: &str,
    width: u32,
    height: u32,
    timestamp_us: i64,
) -> String {
    let prefix = video_frame_meta_prefix(
        participant_sid,
        participant_identity,
        track_sid,
        track_name,
        track_source,
    );
    video_frame_meta_json_with_prefix(&prefix, width, height, timestamp_us)
}

fn video_frame_meta_prefix(
    participant_sid: &str,
    participant_identity: &str,
    track_sid: &str,
    track_name: &str,
    track_source: &str,
) -> String {
    use events::JsonValue::{Raw, Str};
    events::json_object(&[
        (
            "bridgeVersion",
            Raw(crate::bridge_version::ENGINE_BRIDGE_VERSION.to_string()),
        ),
        ("participantSid", Str(participant_sid.to_string())),
        ("participantIdentity", Str(participant_identity.to_string())),
        ("trackSid", Str(track_sid.to_string())),
        ("trackName", Str(track_name.to_string())),
        ("source", Str(track_source.to_string())),
    ])
}

fn video_frame_meta_json_with_prefix(
    prefix: &str,
    width: u32,
    height: u32,
    timestamp_us: i64,
) -> String {
    use std::fmt::Write as _;
    assert!(prefix.starts_with('{'));
    assert!(prefix.ends_with('}'));
    let mut meta = String::with_capacity(prefix.len() + 64);
    meta.push_str(&prefix[..prefix.len() - 1]);
    let _ = write!(
        meta,
        ",\"width\":{width},\"height\":{height},\"timestampUs\":{timestamp_us}}}"
    );
    meta
}

fn i420_buffer_to_tight(buffer: &I420Buffer) -> Option<(Vec<u8>, u32, u32)> {
    let width = buffer.width();
    let height = buffer.height();
    if width < 2 || height < 2 || !width.is_multiple_of(2) || !height.is_multiple_of(2) {
        return None;
    }
    let w = width as usize;
    let h = height as usize;
    let cw = w / 2;
    let ch = h / 2;
    let (stride_y, stride_u, stride_v) = buffer.strides();
    let (src_y, src_u, src_v) = buffer.data();

    let mut out = Vec::with_capacity(tight_i420_len(width, height));
    if !extend_tight(&mut out, src_y, w, stride_y as usize, h) {
        return None;
    }
    if !extend_tight(&mut out, src_u, cw, stride_u as usize, ch) {
        return None;
    }
    if !extend_tight(&mut out, src_v, cw, stride_v as usize, ch) {
        return None;
    }
    assert_eq!(out.len(), tight_i420_len(width, height));
    Some((out, width, height))
}

fn extend_tight(
    out: &mut Vec<u8>,
    src: &[u8],
    width: usize,
    src_stride: usize,
    rows: usize,
) -> bool {
    assert!(width >= 1);
    assert!(rows >= 1);
    if src_stride < width {
        return false;
    }
    if src_stride == width {
        if src.len() < width * rows {
            return false;
        }
        out.extend_from_slice(&src[..width * rows]);
        return true;
    }
    if src.len() < src_stride * (rows - 1) + width {
        return false;
    }
    for row in 0..rows {
        let s = row * src_stride;
        out.extend_from_slice(&src[s..s + width]);
    }
    true
}

fn frame_to_callback_payload(frame: &BoxVideoFrame, meta_prefix: &str) -> Option<(String, Buffer)> {
    let (data, width, height) = if let Some(i420) = frame.buffer.as_i420() {
        i420_buffer_to_tight(i420)?
    } else {
        let i420 = frame.buffer.to_i420();
        i420_buffer_to_tight(&i420)?
    };
    let meta = video_frame_meta_json_with_prefix(meta_prefix, width, height, frame.timestamp_us);
    Some((meta, Buffer::from(data)))
}

fn record_first_error(slot: &mut Option<napi::Error>, result: napi::Result<()>) {
    if let Err(error) = result
        && slot.is_none()
    {
        *slot = Some(error);
    }
}

fn emit_engine_event(
    events_slot: &Mutex<Option<EventTsfn>>,
    dropped_engine_events: &AtomicU64,
    event_type: String,
    payload: String,
) {
    let guard = events_slot.lock();
    let Some(tsfn) = guard.as_ref() else {
        return;
    };
    let status = tsfn.call(
        (event_type, payload),
        ThreadsafeFunctionCallMode::NonBlocking,
    );
    if status == Status::QueueFull {
        dropped_engine_events.fetch_add(1, Ordering::Relaxed);
    }
}

const SPEAKING_TAP_SAMPLE_RATE_HZ: u32 = 48_000;
const SPEAKING_TAP_NUM_CHANNELS: u32 = 1;
const SPEAKING_TAP_FRAME_DURATION_MS_MAX: usize = 100;
const SPEAKING_TAP_FRAME_SAMPLES_MAX: usize = (SPEAKING_TAP_SAMPLE_RATE_HZ as usize)
    * (MAX_AUDIO_CHANNELS as usize)
    * SPEAKING_TAP_FRAME_DURATION_MS_MAX
    / 1_000;

struct SpeakingTap {
    participant_sid: String,
    identity: String,
    track_sid: String,
    source: &'static str,
    is_local: bool,
    release_ms: u64,
    thresholds: Arc<SpeakingThresholds>,
    events: Arc<Mutex<Option<EventTsfn>>>,
    dropped_engine_events: Arc<AtomicU64>,
    stop: Arc<AtomicBool>,
    inbound_audio: Option<Arc<AtomicU64>>,
}

impl SpeakingTap {
    fn spawn(self, rtc_track: RtcAudioTrack) {
        assert!(!self.participant_sid.is_empty());
        assert!(!self.track_sid.is_empty());
        assert_eq!(self.source, "microphone");
        tokio::spawn(self.run(rtc_track));
    }

    async fn run(self, rtc_track: RtcAudioTrack) {
        let mut stream = NativeAudioStream::new(
            rtc_track,
            SPEAKING_TAP_SAMPLE_RATE_HZ as i32,
            SPEAKING_TAP_NUM_CHANNELS as i32,
        );
        let started = Instant::now();
        let mut gate = SpeakingGate::new(speaking::SPEAKING_ATTACK_MS, self.release_ms);
        let mut last_emit_ms: u64 = 0;
        loop {
            if self.stop.load(Ordering::Acquire) {
                break;
            }
            let next_frame = tokio::time::timeout(
                Duration::from_millis(SPEAKING_FRAME_TIMEOUT_MS),
                stream.next(),
            )
            .await;
            let now_ms = started.elapsed().as_millis() as u64;
            let rms = match &next_frame {
                Ok(Some(frame)) => Self::frame_rms(frame),
                Ok(None) => break,
                Err(_) => 0.0,
            };
            if let Ok(Some(_)) = &next_frame {
                if let Some(counter) = &self.inbound_audio {
                    counter.fetch_add(1, Ordering::Relaxed);
                }
            }
            let threshold_rms = if self.is_local {
                self.thresholds.local_rms()
            } else {
                self.thresholds.remote_rms()
            };
            if let Some(speaking) = gate.update(rms, threshold_rms, now_ms) {
                self.emit_speaking(speaking);
                last_emit_ms = now_ms;
            } else if gate.speaking() {
                if now_ms - last_emit_ms >= SPEAKING_HEARTBEAT_INTERVAL_MS {
                    self.emit_speaking(true);
                    last_emit_ms = now_ms;
                }
            }
        }
        if gate.speaking() {
            self.emit_speaking(false);
        }
    }

    fn frame_rms(frame: &AudioFrame<'_>) -> f64 {
        if frame.data.is_empty() {
            return 0.0;
        }
        if frame.num_channels == 0 {
            return 0.0;
        }
        if frame.num_channels > MAX_AUDIO_CHANNELS {
            return 0.0;
        }
        if frame.data.len() > SPEAKING_TAP_FRAME_SAMPLES_MAX {
            return 0.0;
        }
        if frame.data.len() % frame.num_channels as usize != 0 {
            return 0.0;
        }
        speaking::frame_rms_i16(&frame.data)
    }

    fn emit_speaking(&self, speaking: bool) {
        let payload = events::json_object(&[
            (
                "participantSid",
                events::JsonValue::Str(self.participant_sid.clone()),
            ),
            ("identity", events::JsonValue::Str(self.identity.clone())),
            ("trackSid", events::JsonValue::Str(self.track_sid.clone())),
            ("source", events::JsonValue::Str(self.source.to_string())),
            ("isLocal", events::JsonValue::Raw(self.is_local.to_string())),
            ("speaking", events::JsonValue::Raw(speaking.to_string())),
        ]);
        emit_engine_event(
            &self.events,
            &self.dropped_engine_events,
            "speakingChanged".to_string(),
            payload,
        );
    }
}

fn local_video_frame_sink_active(
    video_frames: Arc<Mutex<Option<VideoFrameTsfn>>>,
) -> camera::LocalVideoFrameSinkActive {
    Box::new(move || video_frames.lock().is_some())
}

fn local_video_frame_sink(
    video_frames: Arc<Mutex<Option<VideoFrameTsfn>>>,
    dropped_video_frame_callbacks: Arc<AtomicU64>,
    participant_sid: String,
    participant_identity: String,
    track_sid: Arc<Mutex<String>>,
    track_name: String,
    track_source: String,
) -> camera::LocalVideoFrameSink {
    assert!(!participant_sid.is_empty());
    if !participant_identity.is_empty() {
        assert!(participant_identity.starts_with("user_"));
    }
    assert!(!track_sid.lock().is_empty());
    let last_timestamp_us = AtomicI64::new(i64::MIN);
    Box::new(move |frame, timestamp_us| {
        let previous_timestamp_us = last_timestamp_us.swap(timestamp_us, Ordering::Relaxed);
        assert!(timestamp_us >= previous_timestamp_us);
        if video_frames.lock().is_none() {
            return;
        }
        let (meta, buffer) = local_video_frame_sink_payload(
            frame,
            timestamp_us,
            &participant_sid,
            &participant_identity,
            &track_sid,
            &track_name,
            &track_source,
        );
        if let Some(tsfn) = video_frames.lock().as_ref() {
            let status = tsfn.call((meta, buffer), ThreadsafeFunctionCallMode::NonBlocking);
            if status == Status::QueueFull {
                dropped_video_frame_callbacks.fetch_add(1, Ordering::Relaxed);
            }
        }
    })
}

fn local_video_frame_sink_payload(
    frame: &yuv::I420,
    timestamp_us: i64,
    participant_sid: &str,
    participant_identity: &str,
    track_sid: &Mutex<String>,
    track_name: &str,
    track_source: &str,
) -> (String, Buffer) {
    let current_track_sid = track_sid.lock().clone();
    assert!(!current_track_sid.is_empty());
    local_i420_to_callback_payload(
        frame,
        timestamp_us,
        participant_sid,
        participant_identity,
        &current_track_sid,
        track_name,
        track_source,
    )
}

fn local_i420_to_callback_payload(
    frame: &yuv::I420,
    timestamp_us: i64,
    participant_sid: &str,
    participant_identity: &str,
    track_sid: &str,
    track_name: &str,
    track_source: &str,
) -> (String, Buffer) {
    assert!(frame.width >= 2);
    assert!(frame.height >= 2);
    assert!(frame.width.is_multiple_of(2));
    assert!(frame.height.is_multiple_of(2));
    let w = frame.width as usize;
    let h = frame.height as usize;
    let chroma_len = (w / 2) * (h / 2);
    assert_eq!(frame.y.len(), w * h);
    assert_eq!(frame.u.len(), chroma_len);
    assert_eq!(frame.v.len(), chroma_len);
    let tight_len = tight_i420_len(frame.width, frame.height);
    let mut data = Vec::with_capacity(tight_len);
    data.extend_from_slice(&frame.y);
    data.extend_from_slice(&frame.u);
    data.extend_from_slice(&frame.v);
    assert_eq!(data.len(), tight_len);
    let meta = video_frame_meta_json(
        participant_sid,
        participant_identity,
        track_sid,
        track_name,
        track_source,
        frame.width,
        frame.height,
        timestamp_us,
    );
    (meta, Buffer::from(data))
}

fn validate_processed_camera_frame(frame: &ProcessedCameraFrame) -> napi::Result<()> {
    if frame.format != "i420" {
        return Err(napi::Error::from_reason(
            "processed camera frame format must be i420",
        ));
    }
    if !valid_even_video_dims(frame.width, frame.height) {
        return Err(napi::Error::from_reason(
            "processed camera frame dimensions are invalid",
        ));
    }
    if frame.timestamp_us <= 0 {
        return Err(napi::Error::from_reason(
            "processed camera frame timestamp must be positive",
        ));
    }
    let expected = tight_i420_len(frame.width, frame.height);
    if frame.data.len() != expected {
        return Err(napi::Error::from_reason(
            "processed camera frame byte length does not match tight i420",
        ));
    }
    Ok(())
}

fn processed_camera_frame_to_pending(
    frame: ProcessedCameraFrame,
) -> napi::Result<PendingVideoFrame> {
    validate_processed_camera_frame(&frame)?;
    let buffer = tight_i420_to_native_buffer(frame.data.as_ref(), frame.width, frame.height);
    assert_eq!(buffer.width(), frame.width);
    assert_eq!(buffer.height(), frame.height);
    Ok(PendingVideoFrame::I420Native {
        buffer,
        timestamp_us: frame.timestamp_us,
        enqueued_at: Instant::now(),
    })
}

fn tight_i420_to_native_buffer(data: &[u8], width: u32, height: u32) -> I420Buffer {
    assert!(valid_even_video_dims(width, height));
    assert_eq!(data.len(), tight_i420_len(width, height));
    let y_len = (width as usize) * (height as usize);
    let chroma_len = y_len / 4;
    let mut buffer = I420Buffer::new(width, height);
    let (stride_y, stride_u, stride_v) = buffer.strides();
    {
        let (dy, du, dv) = buffer.data_mut();
        copy_plane(
            dy,
            &data[..y_len],
            width as usize,
            stride_y as usize,
            height as usize,
        );
        let chroma_width = (width / 2) as usize;
        let chroma_height = (height / 2) as usize;
        copy_plane(
            du,
            &data[y_len..y_len + chroma_len],
            chroma_width,
            stride_u as usize,
            chroma_height,
        );
        copy_plane(
            dv,
            &data[y_len + chroma_len..],
            chroma_width,
            stride_v as usize,
            chroma_height,
        );
    }
    buffer
}

fn publish_pending_video_frame(source: &NativeVideoSource, frame: PendingVideoFrame) -> bool {
    match frame {
        PendingVideoFrame::I420Native {
            buffer,
            timestamp_us,
            ..
        } => capture_buffer_to_source(source, buffer, timestamp_us),
        PendingVideoFrame::Bgra {
            data,
            width,
            height,
            stride,
            timestamp_us,
            ..
        } => capture_bgra_to_source(source, &data, width, height, stride, timestamp_us),
        PendingVideoFrame::Nv12 {
            data,
            width,
            height,
            stride_y,
            stride_uv,
            timestamp_us,
            ..
        } => capture_nv12_to_source(
            source,
            &data,
            width,
            height,
            stride_y,
            stride_uv,
            timestamp_us,
        ),
        #[cfg(target_os = "windows")]
        PendingVideoFrame::Texture {
            desc, capability, ..
        } => texture_source::bridge::try_publish_texture(source, &capability, &desc).is_ok(),
        #[cfg(target_os = "macos")]
        PendingVideoFrame::MacCvPixelBuffer {
            buffer,
            timestamp_us,
            ..
        } => capture_buffer_to_source(source, buffer, timestamp_us),
        #[cfg(target_os = "linux")]
        PendingVideoFrame::Dmabuf {
            desc,
            capability,
            fds,
            ..
        } => {
            let mut raw_fds = [-1; 4];
            for (index, fd) in fds.iter().enumerate().take(desc.plane_count as usize) {
                raw_fds[index] = fd.as_raw_fd();
            }
            texture_source::bridge::try_publish_dmabuf(source, &capability, &desc, raw_fds).is_ok()
        }
    }
}

fn capture_buffer_to_source<T: AsRef<dyn VideoBuffer>>(
    source: &NativeVideoSource,
    buffer: T,
    timestamp_us: i64,
) -> bool {
    source.capture_frame(&VideoFrame {
        rotation: VideoRotation::VideoRotation0,
        timestamp_us,
        frame_metadata: None,
        buffer,
    });
    true
}

fn capture_bgra_to_source(
    source: &NativeVideoSource,
    data: &[u8],
    width: u32,
    height: u32,
    stride: u32,
    timestamp_us: i64,
) -> bool {
    let mut buffer = I420Buffer::new(width, height);
    let (stride_y, stride_u, stride_v) = buffer.strides();
    let (dst_y, dst_u, dst_v) = buffer.data_mut();
    if !yuv::bgra_to_i420_planes(
        data, width, height, stride, dst_y, dst_u, dst_v, stride_y, stride_u, stride_v,
    ) {
        return false;
    }
    source.capture_frame(&VideoFrame {
        rotation: VideoRotation::VideoRotation0,
        timestamp_us,
        frame_metadata: None,
        buffer,
    });
    true
}

fn capture_nv12_to_source(
    source: &NativeVideoSource,
    data: &[u8],
    width: u32,
    height: u32,
    stride_y: u32,
    stride_uv: u32,
    timestamp_us: i64,
) -> bool {
    let mut buffer = NV12Buffer::with_strides(width, height, width, width);
    let (dst_stride_y, dst_stride_uv) = buffer.strides();
    let (dst_y, dst_uv) = buffer.data_mut();
    if !yuv::copy_nv12_planes(
        data,
        width,
        height,
        stride_y,
        stride_uv,
        dst_y,
        dst_uv,
        dst_stride_y,
        dst_stride_uv,
    ) {
        return false;
    }
    source.capture_frame(&VideoFrame {
        rotation: VideoRotation::VideoRotation0,
        timestamp_us,
        frame_metadata: None,
        buffer,
    });
    true
}

fn valid_even_video_dims(width: u32, height: u32) -> bool {
    width >= 2
        && height >= 2
        && width.is_multiple_of(2)
        && height.is_multiple_of(2)
        && width <= 8192
        && height <= 8192
}

fn now_millis() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64
}

fn elapsed_ms(start: Instant, end: Instant) -> u64 {
    end.saturating_duration_since(start).as_millis() as u64
}

fn texture_capability_for_screen_codec(codec: &str) -> TextureCapability {
    if !texture_source::codec_allows_native_gpu(codec) {
        return TextureCapability::for_screen_codec(codec, false);
    }
    TextureCapability::for_screen_codec(
        codec,
        crate::hardware_encoder::hardware_encoder_capability().available,
    )
}

fn build_microphone_publish_options(opts: &MicrophoneOptions) -> napi::Result<TrackPublishOptions> {
    let audio_encoding = audio::normalize_microphone_max_bitrate_bps(opts.max_bitrate_bps)
        .map_err(napi::Error::from_reason)?
        .map(|max_bitrate| AudioEncoding { max_bitrate });
    Ok(TrackPublishOptions {
        source: TrackSource::Microphone,
        red: true,
        dtx: true,
        audio_encoding,
        ..Default::default()
    })
}

fn build_camera_publish_options(
    opts: &CameraOptions,
    publication_kind: CameraPublicationKind,
) -> napi::Result<TrackPublishOptions> {
    let mut options = TrackPublishOptions {
        source: publication_kind.track_source(),
        simulcast: publication_kind.is_screencast(),
        ..Default::default()
    };
    if let Some(stream) = publication_kind.stream() {
        options.stream = stream.to_string();
    }
    if let Some(codec) = opts
        .codec
        .as_deref()
        .map(str::trim)
        .filter(|c| !c.is_empty())
    {
        let canonical_codec = crate::config::canonical_codec_name(codec)
            .ok_or_else(|| napi::Error::from_reason("unsupported video codec"))?;
        crate::hardware_encoder::require_publish_codec_runtime_support(canonical_codec)
            .map_err(napi::Error::from_reason)?;
        options.video_codec = parse_codec(codec)
            .ok_or_else(|| napi::Error::from_reason("unsupported video codec"))?;
    }
    if let Some(bitrate) = opts.max_bitrate_bps.filter(|b| b.is_finite() && *b > 0.0) {
        options.video_encoding = Some(VideoEncoding {
            max_bitrate: bitrate as u64,
            max_framerate: opts
                .max_framerate
                .filter(|fps| fps.is_finite() && *fps > 0.0)
                .or_else(|| opts.frame_rate.map(f64::from).filter(|fps| *fps > 0.0))
                .unwrap_or(30.0),
        });
    }
    Ok(options)
}

fn native_camera_background_config(
    opts: &CameraOptions,
) -> napi::Result<crate::camera_background::CameraBackgroundConfig> {
    crate::camera_background::CameraBackgroundConfig::from_bridge_values(
        opts.background_mode.as_deref(),
        opts.background_custom_media_path.as_deref(),
        opts.background_custom_media_kind.as_deref(),
        opts.background_blur_strength,
    )
    .map_err(napi::Error::from_reason)
}

fn parse_codec(name: &str) -> Option<VideoCodec> {
    match crate::config::canonical_codec_name(name)? {
        "vp8" => Some(VideoCodec::VP8),
        "vp9" => Some(VideoCodec::VP9),
        "h264" => Some(VideoCodec::H264),
        "av1" => Some(VideoCodec::AV1),
        "h265" => Some(VideoCodec::H265),
        other => unreachable!("unhandled canonical codec {other}"),
    }
}

fn pcm16_audio_frame_into<'a>(
    bytes: &[u8],
    sample_rate: u32,
    num_channels: u32,
    samples: &'a mut Vec<i16>,
) -> Option<AudioFrame<'a>> {
    if !valid_audio_format(sample_rate, num_channels) {
        return None;
    }
    let sample_count = bytes.len() / 2;
    if sample_count == 0 || bytes.len() % 2 != 0 || sample_count % (num_channels as usize) != 0 {
        return None;
    }
    let samples_per_channel = sample_count / num_channels as usize;
    let max_samples_per_channel =
        (sample_rate as u64).saturating_mul(MAX_PCM_FRAME_DURATION_MS) / 1_000;
    if samples_per_channel as u64 > max_samples_per_channel {
        return None;
    }
    let samples_per_channel = u32::try_from(samples_per_channel).ok()?;
    assert!(sample_count <= MAX_PCM_FRAME_SAMPLES);
    samples.clear();
    samples.reserve(sample_count);
    for chunk in bytes.chunks_exact(2) {
        samples.push(i16::from_le_bytes([chunk[0], chunk[1]]));
    }
    assert_eq!(samples.len(), sample_count);
    Some(AudioFrame {
        data: std::borrow::Cow::Borrowed(samples.as_slice()),
        sample_rate,
        num_channels,
        samples_per_channel,
    })
}

fn f32_sample_to_i16(sample: f32) -> i16 {
    if !sample.is_finite() {
        return 0;
    }
    let clamped = sample.clamp(-1.0, 1.0);
    if clamped < 0.0 {
        (clamped * 32768.0) as i16
    } else {
        (clamped * 32767.0) as i16
    }
}

fn f32_audio_frame_into<'a>(
    bytes: &[u8],
    sample_rate: u32,
    num_channels: u32,
    samples: &'a mut Vec<i16>,
) -> Option<AudioFrame<'a>> {
    if !valid_audio_format(sample_rate, num_channels) {
        return None;
    }
    let sample_count = bytes.len() / 4;
    if sample_count == 0 || bytes.len() % 4 != 0 || sample_count % (num_channels as usize) != 0 {
        return None;
    }
    let samples_per_channel = sample_count / num_channels as usize;
    let max_samples_per_channel =
        (sample_rate as u64).saturating_mul(MAX_PCM_FRAME_DURATION_MS) / 1_000;
    if samples_per_channel as u64 > max_samples_per_channel {
        return None;
    }
    let samples_per_channel = u32::try_from(samples_per_channel).ok()?;
    assert!(sample_count <= MAX_PCM_FRAME_SAMPLES);
    samples.clear();
    samples.reserve(sample_count);
    for chunk in bytes.chunks_exact(4) {
        samples.push(f32_sample_to_i16(f32::from_le_bytes([
            chunk[0], chunk[1], chunk[2], chunk[3],
        ])));
    }
    assert_eq!(samples.len(), sample_count);
    Some(AudioFrame {
        data: std::borrow::Cow::Borrowed(samples.as_slice()),
        sample_rate,
        num_channels,
        samples_per_channel,
    })
}

fn valid_audio_format(sample_rate: u32, num_channels: u32) -> bool {
    (MIN_AUDIO_SAMPLE_RATE_HZ..=MAX_AUDIO_SAMPLE_RATE_HZ).contains(&sample_rate)
        && (1..=MAX_AUDIO_CHANNELS).contains(&num_channels)
}

fn remote_subscription_target_error(
    missing_target: &str,
    participant_identity: &str,
    source: &str,
) -> napi::Error {
    assert!(!missing_target.is_empty());
    assert!(!participant_identity.is_empty());
    assert!(!source.is_empty());
    napi::Error::from_reason(format!(
        "remote track subscription {missing_target} not found \
         for participant {participant_identity} source {source}"
    ))
}

fn parse_track_source(source: &str) -> Option<TrackSource> {
    match source.trim() {
        LIVEKIT_TRACK_SOURCE_CAMERA => Some(TrackSource::Camera),
        "microphone" => Some(TrackSource::Microphone),
        LIVEKIT_TRACK_SOURCE_SCREEN_SHARE | "screenshare" => Some(TrackSource::Screenshare),
        LIVEKIT_TRACK_SOURCE_SCREEN_SHARE_AUDIO | "screenshareAudio" => {
            Some(TrackSource::ScreenshareAudio)
        }
        "unknown" => Some(TrackSource::Unknown),
        _ => None,
    }
}

fn is_optional_remote_subscription_target(source: TrackSource) -> bool {
    source == TrackSource::ScreenshareAudio
}

fn parse_video_quality(quality: &str) -> Option<VideoQuality> {
    match quality.trim() {
        "low" => Some(VideoQuality::Low),
        "medium" => Some(VideoQuality::Medium),
        "high" => Some(VideoQuality::High),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use livekit::webrtc::stats::{CandidatePairStats, dictionaries};

    fn sender_for_tests() -> (AdaptiveVideoSender, Arc<AdaptiveVideoStats>) {
        let stats = Arc::new(AdaptiveVideoStats::new(60.0, 15.0, true, 0));
        let sender = AdaptiveVideoSender {
            pending: Arc::new(ArrayQueue::new(ENCODER_QUEUE_CAPACITY)),
            notify: Arc::new(Notify::new()),
            stop: Arc::new(AtomicBool::new(false)),
            stats: stats.clone(),
            pacing: VideoPacingMode::Sender,
            target_fps: 60.0,
        };
        (sender, stats)
    }

    fn tiny_bgra_frame() -> PendingVideoFrame {
        tiny_bgra_frame_enqueued_at(Instant::now())
    }

    fn tiny_bgra_frame_enqueued_at(enqueued_at: Instant) -> PendingVideoFrame {
        PendingVideoFrame::Bgra {
            data: vec![0; 16],
            width: 2,
            height: 2,
            stride: 8,
            timestamp_us: 1,
            enqueued_at,
        }
    }

    fn audio_frame_for_tests(samples: &[i16], num_channels: u32) -> AudioFrame<'_> {
        AudioFrame {
            data: std::borrow::Cow::Borrowed(samples),
            sample_rate: SPEAKING_TAP_SAMPLE_RATE_HZ,
            num_channels,
            samples_per_channel: if num_channels == 0 {
                0
            } else {
                (samples.len() / num_channels as usize) as u32
            },
        }
    }

    fn valid_connection_state_transitions() -> [(u8, u8); 8] {
        [
            (S_IDLE, S_CLOSED),
            (S_CLOSED, S_CONNECTING),
            (S_CLOSED, S_CLOSED),
            (S_CONNECTING, S_CONNECTED),
            (S_CONNECTING, S_FAILED),
            (S_CONNECTING, S_CLOSED),
            (S_CONNECTED, S_CLOSED),
            (S_FAILED, S_CLOSED),
        ]
    }

    #[test]
    fn extend_tight_packs_strided_planes_and_rejects_short_sources() {
        let mut out = Vec::with_capacity(8);
        let src = [1u8, 2, 9, 9, 3, 4, 9, 9];
        assert!(extend_tight(&mut out, &src, 2, 4, 2));
        assert_eq!(out, vec![1, 2, 3, 4]);

        out.clear();
        assert!(extend_tight(&mut out, &src[..6], 2, 4, 2));
        assert_eq!(out, vec![1, 2, 3, 4]);

        out.clear();
        assert!(!extend_tight(&mut out, &src[..5], 2, 4, 2));

        out.clear();
        assert!(extend_tight(&mut out, &[5u8, 6, 7, 8], 4, 4, 1));
        assert_eq!(out, vec![5, 6, 7, 8]);

        out.clear();
        assert!(!extend_tight(&mut out, &[5u8, 6, 7], 4, 4, 1));

        out.clear();
        assert!(!extend_tight(&mut out, &src, 4, 2, 1));
    }

    #[test]
    fn connect_admission_proceeds_for_latest_intent() {
        assert_eq!(admit_connect_attempt(1, 1), ConnectAdmission::Proceed);
        assert_eq!(admit_connect_attempt(7, 7), ConnectAdmission::Proceed);
    }

    #[test]
    fn connect_admission_supersedes_stale_intent() {
        assert_eq!(admit_connect_attempt(1, 2), ConnectAdmission::Superseded);
        assert_eq!(admit_connect_attempt(3, 9), ConnectAdmission::Superseded);
    }

    #[test]
    #[should_panic]
    fn connect_admission_rejects_intent_newer_than_latest() {
        let _ = admit_connect_attempt(2, 1);
    }

    #[tokio::test]
    async fn connect_cancel_waiter_ignores_older_cancel_epoch() {
        let (cancel_tx, cancel_rx) = watch::channel(2);
        let mut waiter = Box::pin(wait_connect_cancelled(cancel_rx, 4));
        tokio::select! {
            biased;
            () = waiter.as_mut() => panic!("older cancel epoch must not cancel newer connect"),
            _ = tokio::task::yield_now() => {}
        }

        cancel_tx.send_replace(4);
        waiter.await;
    }

    #[tokio::test]
    async fn connect_cancel_waiter_finishes_for_current_cancel_epoch() {
        let (_cancel_tx, cancel_rx) = watch::channel(9);

        wait_connect_cancelled(cancel_rx, 9).await;
    }

    #[test]
    fn room_event_loop_forwards_for_current_epoch() {
        assert_eq!(room_event_loop_action(1, 1), RoomLoopAction::Forward);
        assert_eq!(room_event_loop_action(5, 5), RoomLoopAction::Forward);
    }

    #[test]
    fn room_event_loop_exits_for_stale_epoch() {
        assert_eq!(room_event_loop_action(1, 2), RoomLoopAction::Exit);
        assert_eq!(room_event_loop_action(4, 9), RoomLoopAction::Exit);
    }

    #[test]
    #[should_panic]
    fn room_event_loop_rejects_epoch_from_the_future() {
        let _ = room_event_loop_action(3, 2);
    }

    #[test]
    fn connection_state_transitions_accept_positive_space() {
        for (from, to) in valid_connection_state_transitions() {
            assert!(connection_state_transition_valid(from, to));
        }
    }

    #[test]
    fn connection_state_transitions_reject_negative_space() {
        let valid = valid_connection_state_transitions();
        for from in CONNECTION_STATES {
            for to in CONNECTION_STATES {
                if valid.contains(&(from, to)) {
                    continue;
                }
                assert!(!connection_state_transition_valid(from, to));
            }
        }
    }

    #[test]
    fn store_connection_state_applies_a_full_connect_cycle() {
        let state = AtomicU8::new(S_IDLE);
        store_connection_state(&state, S_CLOSED);
        store_connection_state(&state, S_CONNECTING);
        store_connection_state(&state, S_CONNECTED);
        store_connection_state(&state, S_CLOSED);
        store_connection_state(&state, S_CONNECTING);
        store_connection_state(&state, S_FAILED);
        store_connection_state(&state, S_CLOSED);
        assert_eq!(state.load(Ordering::Acquire), S_CLOSED);
    }

    #[test]
    #[should_panic]
    fn store_connection_state_panics_on_reconnect_without_close() {
        let state = AtomicU8::new(S_CONNECTED);
        store_connection_state(&state, S_CONNECTING);
    }

    #[test]
    fn stale_room_loop_end_does_not_clobber_newer_connection_state() {
        let state_guard = Mutex::new(());
        let connect_epoch = AtomicU64::new(2);
        let state = AtomicU8::new(S_CONNECTED);

        store_room_loop_closed(&state_guard, &connect_epoch, &state, 1);

        assert_eq!(state.load(Ordering::Acquire), S_CONNECTED);
    }

    #[test]
    fn current_room_loop_end_stores_closed() {
        let state_guard = Mutex::new(());
        let connect_epoch = AtomicU64::new(3);
        let state = AtomicU8::new(S_CONNECTED);

        store_room_loop_closed(&state_guard, &connect_epoch, &state, 3);

        assert_eq!(state.load(Ordering::Acquire), S_CLOSED);
    }

    #[test]
    fn video_pacing_mode_defaults_to_source_and_accepts_sender() {
        assert_eq!(VideoPacingMode::from_option(None), VideoPacingMode::Source);
        assert_eq!(
            VideoPacingMode::from_option(Some("source")),
            VideoPacingMode::Source
        );
        assert_eq!(
            VideoPacingMode::from_option(Some("sender")),
            VideoPacingMode::Sender
        );
    }

    fn microphone_options_with_bitrate(max_bitrate_bps: Option<f64>) -> MicrophoneOptions {
        MicrophoneOptions {
            device_id: None,
            echo_cancellation: None,
            noise_suppression: None,
            auto_gain_control: None,
            deep_filter: None,
            deep_filter_noise_reduction_level: None,
            max_bitrate_bps,
        }
    }

    #[test]
    fn microphone_publish_options_default_to_no_audio_encoding() {
        let options = build_microphone_publish_options(&microphone_options_with_bitrate(None))
            .expect("microphone publish options");

        assert_eq!(options.source, TrackSource::Microphone);
        assert!(options.red);
        assert!(options.dtx);
        assert!(options.audio_encoding.is_none());
    }

    #[test]
    fn microphone_publish_options_apply_the_clamped_max_bitrate() {
        let options =
            build_microphone_publish_options(&microphone_options_with_bitrate(Some(96_000.0)))
                .expect("microphone publish options");
        let encoding = options.audio_encoding.expect("audio encoding");
        assert_eq!(encoding.max_bitrate, 96_000);

        let floored = build_microphone_publish_options(&microphone_options_with_bitrate(Some(1.0)))
            .expect("microphone publish options")
            .audio_encoding
            .expect("audio encoding");
        assert_eq!(floored.max_bitrate, audio::MICROPHONE_MAX_BITRATE_BPS_FLOOR);

        let capped =
            build_microphone_publish_options(&microphone_options_with_bitrate(Some(1_000_000.0)))
                .expect("microphone publish options")
                .audio_encoding
                .expect("audio encoding");
        assert_eq!(capped.max_bitrate, audio::MICROPHONE_MAX_BITRATE_BPS_CAP);
    }

    #[test]
    fn microphone_publish_options_reject_invalid_max_bitrate() {
        assert!(
            build_microphone_publish_options(&microphone_options_with_bitrate(Some(0.0))).is_err()
        );
        assert!(
            build_microphone_publish_options(&microphone_options_with_bitrate(Some(-64_000.0)))
                .is_err()
        );
        assert!(
            build_microphone_publish_options(&microphone_options_with_bitrate(Some(64_000.5)))
                .is_err()
        );
        assert!(
            build_microphone_publish_options(&microphone_options_with_bitrate(Some(f64::NAN)))
                .is_err()
        );
    }

    #[test]
    fn camera_publish_options_use_camera_source_without_screen_stream() {
        assert_eq!(CameraPublicationKind::Camera.track_name(), "camera");

        let options = build_camera_publish_options(
            &CameraOptions {
                device_id: None,
                width: None,
                height: None,
                frame_rate: None,
                mirror: None,
                background_mode: None,
                background_custom_media_path: None,
                background_custom_media_kind: None,
                background_blur_strength: None,
                codec: None,
                max_bitrate_bps: None,
                max_framerate: None,
            },
            CameraPublicationKind::Camera,
        )
        .expect("camera publish options");

        assert_eq!(options.source, TrackSource::Camera);
        assert!(options.stream.is_empty());
        assert!(options.video_encoding.is_none());
    }

    #[test]
    fn camera_publish_options_can_publish_as_screen_share() {
        assert_eq!(
            CameraPublicationKind::ScreenShare.track_name(),
            "screen_share"
        );

        let options = build_camera_publish_options(
            &CameraOptions {
                device_id: Some("studio-display-camera".to_string()),
                width: Some(1280),
                height: Some(720),
                frame_rate: Some(24),
                mirror: None,
                background_mode: None,
                background_custom_media_path: None,
                background_custom_media_kind: None,
                background_blur_strength: None,
                codec: None,
                max_bitrate_bps: Some(4_000_000.0),
                max_framerate: None,
            },
            CameraPublicationKind::ScreenShare,
        )
        .expect("device screen share publish options");

        assert_eq!(options.source, TrackSource::Screenshare);
        assert_eq!(options.stream, "screen_share");
        let encoding = options.video_encoding.expect("video encoding");
        assert_eq!(encoding.max_bitrate, 4_000_000);
        assert_eq!(encoding.max_framerate, 24.0);
    }

    fn candidate_pair_report(nominated: bool, rtt_seconds: f64) -> RtcStats {
        RtcStats::CandidatePair(CandidatePairStats {
            candidate_pair: dictionaries::CandidatePairStats {
                nominated,
                current_round_trip_time: rtt_seconds,
                ..Default::default()
            },
            ..Default::default()
        })
    }

    #[test]
    fn parse_codec_maps_all_five_including_h265() {
        assert_eq!(parse_codec("vp8"), Some(VideoCodec::VP8));
        assert_eq!(parse_codec("vp9"), Some(VideoCodec::VP9));
        assert_eq!(parse_codec("h264"), Some(VideoCodec::H264));
        assert_eq!(parse_codec("av1"), Some(VideoCodec::AV1));
        assert_eq!(parse_codec("h265"), Some(VideoCodec::H265));
        assert_eq!(parse_codec("H265"), Some(VideoCodec::H265));
        assert_eq!(parse_codec("hevc"), Some(VideoCodec::H265));
        assert_eq!(parse_codec("HEVC"), Some(VideoCodec::H265));
        assert_eq!(parse_codec("H264"), Some(VideoCodec::H264));
    }

    #[test]
    fn parse_codec_rejects_empty_and_unknown() {
        assert_eq!(parse_codec(""), None);
        assert_eq!(parse_codec("rubbish"), None);
    }

    #[test]
    fn parse_track_source_accepts_renderer_and_livekit_spellings() {
        assert_eq!(parse_track_source("camera"), Some(TrackSource::Camera));
        assert_eq!(
            parse_track_source("screen_share"),
            Some(TrackSource::Screenshare)
        );
        assert_eq!(
            parse_track_source("screenshare"),
            Some(TrackSource::Screenshare)
        );
        assert_eq!(
            parse_track_source("screen_share_audio"),
            Some(TrackSource::ScreenshareAudio)
        );
        assert_eq!(
            parse_track_source("screenshareAudio"),
            Some(TrackSource::ScreenshareAudio)
        );
        assert_eq!(parse_track_source("rubbish"), None);
    }

    #[test]
    fn optional_remote_subscription_target_is_limited_to_screen_share_audio() {
        assert!(is_optional_remote_subscription_target(
            TrackSource::ScreenshareAudio
        ));
        assert!(!is_optional_remote_subscription_target(
            TrackSource::Screenshare
        ));
        assert!(!is_optional_remote_subscription_target(TrackSource::Camera));
    }

    fn pcm16_frame_accepted(bytes: &[u8], sample_rate: u32, num_channels: u32) -> bool {
        let mut samples = Vec::new();
        pcm16_audio_frame_into(bytes, sample_rate, num_channels, &mut samples).is_some()
    }

    fn f32_bytes(samples: &[f32]) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(samples.len() * 4);
        for sample in samples {
            bytes.extend_from_slice(&sample.to_le_bytes());
        }
        bytes
    }

    fn f32_frame_accepted(bytes: &[u8], sample_rate: u32, num_channels: u32) -> bool {
        let mut samples = Vec::new();
        f32_audio_frame_into(bytes, sample_rate, num_channels, &mut samples).is_some()
    }

    #[test]
    fn speaking_tap_frame_rms_accepts_interleaved_stereo_frames() {
        let samples = [8_192i16, -8_192, 8_192, -8_192];
        let frame = audio_frame_for_tests(&samples, 2);

        let rms = SpeakingTap::frame_rms(&frame);

        assert!((rms - 0.25).abs() < 0.001);
    }

    #[test]
    fn speaking_tap_frame_rms_rejects_bad_channel_counts_without_panic() {
        let samples = [8_192i16, -8_192];
        let zero_channels = audio_frame_for_tests(&samples, 0);
        let too_many_channels = audio_frame_for_tests(&samples, MAX_AUDIO_CHANNELS + 1);

        assert_eq!(SpeakingTap::frame_rms(&zero_channels), 0.0);
        assert_eq!(SpeakingTap::frame_rms(&too_many_channels), 0.0);
    }

    #[test]
    fn speaking_tap_frame_rms_rejects_misaligned_interleaved_frames() {
        let samples = [8_192i16, -8_192, 8_192];
        let frame = audio_frame_for_tests(&samples, 2);

        assert_eq!(SpeakingTap::frame_rms(&frame), 0.0);
    }

    #[test]
    fn speaking_tap_frame_rms_rejects_overlarge_frames_without_panic() {
        let samples = vec![1i16; SPEAKING_TAP_FRAME_SAMPLES_MAX + 1];
        let frame = audio_frame_for_tests(&samples, 1);

        assert_eq!(SpeakingTap::frame_rms(&frame), 0.0);
    }

    #[test]
    fn pcm16_audio_frame_shapes_interleaved_stereo_samples() {
        let bytes = [1u8, 0, 255, 255, 2, 0, 254, 255];
        let mut samples = Vec::new();
        let frame = pcm16_audio_frame_into(&bytes, 48_000, 2, &mut samples).expect("valid frame");

        assert_eq!(frame.sample_rate, 48_000);
        assert_eq!(frame.num_channels, 2);
        assert_eq!(frame.samples_per_channel, 2);
        assert_eq!(frame.data.as_ref(), &[1, -1, 2, -2]);
    }

    #[test]
    fn pcm16_audio_frame_reuses_scratch_capacity_across_frames() {
        let bytes = [1u8, 0, 255, 255, 2, 0, 254, 255];
        let mut samples = Vec::with_capacity(bytes.len() / 2);
        let scratch_ptr = samples.as_ptr();

        for _ in 0..3 {
            let frame =
                pcm16_audio_frame_into(&bytes, 48_000, 2, &mut samples).expect("valid frame");
            assert_eq!(frame.data.as_ref(), &[1, -1, 2, -2]);
        }

        assert_eq!(samples.as_ptr(), scratch_ptr);
        assert_eq!(samples.capacity(), bytes.len() / 2);
    }

    #[test]
    fn f32_audio_frame_shapes_and_clamps_samples() {
        let bytes = f32_bytes(&[-2.0, -1.0, -0.5, 0.0, 0.5, 1.0, 2.0, f32::NAN]);
        let mut samples = Vec::new();
        let frame = f32_audio_frame_into(&bytes, 48_000, 2, &mut samples).expect("valid frame");

        assert_eq!(frame.sample_rate, 48_000);
        assert_eq!(frame.num_channels, 2);
        assert_eq!(frame.samples_per_channel, 4);
        assert_eq!(
            frame.data.as_ref(),
            &[-32768, -32768, -16384, 0, 16383, 32767, 32767, 0]
        );
    }

    #[test]
    fn f32_audio_frame_reuses_scratch_capacity_across_frames() {
        let bytes = f32_bytes(&[0.25, -0.25, 0.75, -0.75]);
        let mut samples = Vec::with_capacity(bytes.len() / 4);
        let scratch_ptr = samples.as_ptr();

        for _ in 0..3 {
            let frame = f32_audio_frame_into(&bytes, 48_000, 2, &mut samples).expect("valid frame");
            assert_eq!(frame.data.as_ref(), &[8191, -8192, 24575, -24576]);
        }

        assert_eq!(samples.as_ptr(), scratch_ptr);
        assert_eq!(samples.capacity(), bytes.len() / 4);
    }

    #[test]
    fn pcm16_audio_frame_rejects_malformed_buffers() {
        assert!(!pcm16_frame_accepted(&[], 48_000, 2));
        assert!(!pcm16_frame_accepted(&[0], 48_000, 2));
        assert!(!pcm16_frame_accepted(&[0, 0, 1, 0], 48_000, 3));
        assert!(!pcm16_frame_accepted(&[0, 0], 0, 1));
        assert!(!pcm16_frame_accepted(&[0, 0], 48_000, 0));
    }

    #[test]
    fn f32_audio_frame_rejects_malformed_buffers() {
        assert!(!f32_frame_accepted(&[], 48_000, 2));
        assert!(!f32_frame_accepted(&[0, 0, 0], 48_000, 2));
        assert!(!f32_frame_accepted(&f32_bytes(&[0.0, 1.0]), 48_000, 3));
        assert!(!f32_frame_accepted(&f32_bytes(&[0.0]), 0, 1));
        assert!(!f32_frame_accepted(&f32_bytes(&[0.0]), 48_000, 0));
    }

    #[test]
    fn pcm16_audio_frame_rejects_implausible_format_and_overlong_chunks() {
        assert!(valid_audio_format(8_000, 1));
        assert!(valid_audio_format(192_000, 8));
        assert!(!valid_audio_format(7_999, 1));
        assert!(!valid_audio_format(192_001, 1));
        assert!(!valid_audio_format(48_000, 9));

        let one_second_stereo = vec![0u8; 48_000 * 2 * 2];
        assert!(pcm16_frame_accepted(&one_second_stereo, 48_000, 2));

        let overlong_stereo = vec![0u8; (48_000 + 1) * 2 * 2];
        assert!(!pcm16_frame_accepted(&overlong_stereo, 48_000, 2));
    }

    #[test]
    fn same_audio_format_matches_only_identical_sample_rate_and_channels() {
        assert!(same_audio_format(48_000, 2, 48_000, 2));
        assert!(!same_audio_format(48_000, 2, 44_100, 2));
        assert!(!same_audio_format(48_000, 2, 48_000, 1));
    }

    #[test]
    fn f32_audio_frame_rejects_overlong_chunks() {
        let one_second_stereo = vec![0u8; 48_000 * 2 * 4];
        assert!(f32_frame_accepted(&one_second_stereo, 48_000, 2));

        let overlong_stereo = vec![0u8; (48_000 + 1) * 2 * 4];
        assert!(!f32_frame_accepted(&overlong_stereo, 48_000, 2));
    }

    #[test]
    fn pcm_scratch_capacity_is_bounded_by_named_caps() {
        let scratch = new_pcm_scratch(48_000, 2);
        let capacity = scratch.blocking_lock().capacity();
        assert_eq!(capacity, 48_000 * 2);
        assert!(capacity <= MAX_PCM_FRAME_SAMPLES);
    }

    #[test]
    fn parse_video_quality_accepts_subscription_quality_levels() {
        assert_eq!(parse_video_quality("low"), Some(VideoQuality::Low));
        assert_eq!(parse_video_quality("medium"), Some(VideoQuality::Medium));
        assert_eq!(parse_video_quality("high"), Some(VideoQuality::High));
        assert_eq!(parse_video_quality("off"), None);
    }

    #[test]
    fn video_input_validation_rejects_degenerate_odd_and_oversized_frames() {
        assert!(valid_even_video_dims(2, 2));
        assert!(valid_even_video_dims(8192, 8192));
        assert!(!valid_even_video_dims(0, 2));
        assert!(!valid_even_video_dims(1, 2));
        assert!(!valid_even_video_dims(3, 2));
        assert!(!valid_even_video_dims(8194, 2));
    }

    #[test]
    fn processed_camera_frame_to_pending_accepts_tight_i420_payloads() {
        let frame = ProcessedCameraFrame {
            format: "i420".to_string(),
            width: 2,
            height: 2,
            timestamp_us: 123,
            data: Buffer::from(vec![1, 1, 1, 1, 2, 3]),
        };

        let pending = processed_camera_frame_to_pending(frame).expect("processed camera frame");

        match pending {
            PendingVideoFrame::I420Native {
                buffer,
                timestamp_us,
                ..
            } => {
                assert_eq!(buffer.width(), 2);
                assert_eq!(buffer.height(), 2);
                assert_eq!(timestamp_us, 123);
                let (stride_y, _, _) = buffer.strides();
                let (y, u, v) = buffer.data();
                assert_eq!(&y[..2], &[1, 1]);
                let second_row = stride_y as usize;
                assert_eq!(&y[second_row..second_row + 2], &[1, 1]);
                assert_eq!(u[0], 2);
                assert_eq!(v[0], 3);
            }
            _ => panic!("expected native i420 processed camera frame"),
        }
    }

    #[test]
    fn processed_camera_frame_to_pending_rejects_loose_i420_payloads() {
        let frame = ProcessedCameraFrame {
            format: "i420".to_string(),
            width: 2,
            height: 2,
            timestamp_us: 123,
            data: Buffer::from(vec![1, 2, 3]),
        };

        assert!(processed_camera_frame_to_pending(frame).is_err());
    }

    #[test]
    fn adaptive_video_sender_stop_clears_pending_and_rejects_new_frames() {
        let (sender, stats) = sender_for_tests();

        assert_eq!(sender.enqueue(tiny_bgra_frame()), EnqueueResult::Accepted);
        assert_eq!(sender.pending.len(), 1);

        sender.stop();
        assert_eq!(sender.pending.len(), 0);
        assert_eq!(sender.enqueue(tiny_bgra_frame()), EnqueueResult::Rejected);
        assert_eq!(sender.pending.len(), 0);

        let audio = AdaptiveAudioStats::new(DEFAULT_AUDIO_BUFFER_MAX_MS, 0);
        let snapshot = stats.snapshot(&audio, VideoTelemetryExtras::default());
        assert_eq!(snapshot.outgoing_video_queue_depth, 0);
        assert_eq!(snapshot.outgoing_video_frames_produced, 1);
        assert_eq!(snapshot.outgoing_video_frames_accepted, 1);
        assert_eq!(snapshot.outgoing_video_frames_dropped, 1);
        assert_eq!(snapshot.outgoing_video_frames_coalesced, 0);
    }

    #[test]
    fn adaptive_video_sender_accumulates_frames_until_capacity() {
        let (sender, stats) = sender_for_tests();

        for _ in 0..ENCODER_QUEUE_CAPACITY {
            assert_eq!(sender.enqueue(tiny_bgra_frame()), EnqueueResult::Accepted);
        }

        assert_eq!(sender.pending.len(), ENCODER_QUEUE_CAPACITY);
        let audio = AdaptiveAudioStats::new(DEFAULT_AUDIO_BUFFER_MAX_MS, 0);
        let snapshot = stats.snapshot(&audio, VideoTelemetryExtras::default());
        assert_eq!(
            snapshot.outgoing_video_queue_depth,
            ENCODER_QUEUE_CAPACITY as u64
        );
        assert_eq!(
            snapshot.outgoing_video_frames_produced,
            ENCODER_QUEUE_CAPACITY as u64
        );
        assert_eq!(
            snapshot.outgoing_video_frames_accepted,
            ENCODER_QUEUE_CAPACITY as u64
        );
        assert_eq!(snapshot.outgoing_video_frames_dropped, 0);
        assert_eq!(snapshot.outgoing_video_frames_coalesced, 0);
    }

    #[test]
    fn adaptive_video_sender_coalesces_oldest_when_capacity_exceeded() {
        let (sender, stats) = sender_for_tests();

        for _ in 0..ENCODER_QUEUE_CAPACITY {
            assert_eq!(sender.enqueue(tiny_bgra_frame()), EnqueueResult::Accepted);
        }
        let coalesced_pushes = 3;
        for _ in 0..coalesced_pushes {
            assert_eq!(sender.enqueue(tiny_bgra_frame()), EnqueueResult::Coalesced);
        }

        assert_eq!(sender.pending.len(), ENCODER_QUEUE_CAPACITY);
        let audio = AdaptiveAudioStats::new(DEFAULT_AUDIO_BUFFER_MAX_MS, 0);
        let snapshot = stats.snapshot(&audio, VideoTelemetryExtras::default());
        assert_eq!(
            snapshot.outgoing_video_queue_depth,
            ENCODER_QUEUE_CAPACITY as u64
        );
        let total_pushed = (ENCODER_QUEUE_CAPACITY + coalesced_pushes) as u64;
        assert_eq!(snapshot.outgoing_video_frames_produced, total_pushed);
        assert_eq!(snapshot.outgoing_video_frames_accepted, total_pushed);
        assert_eq!(snapshot.outgoing_video_frames_dropped, 0);
        assert_eq!(
            snapshot.outgoing_video_frames_coalesced,
            coalesced_pushes as u64
        );
    }

    #[test]
    fn adaptive_video_sender_concurrent_producers_never_reject_until_stopped() {
        let (sender, stats) = sender_for_tests();
        let producers = 8;
        let frames_per_producer = 128;
        let handles: Vec<_> = (0..producers)
            .map(|_| {
                let s = sender.clone();
                std::thread::spawn(move || {
                    for _ in 0..frames_per_producer {
                        let result = s.enqueue(tiny_bgra_frame());
                        assert!(
                            matches!(result, EnqueueResult::Accepted | EnqueueResult::Coalesced),
                            "expected Accepted|Coalesced, got {:?}",
                            result
                        );
                    }
                })
            })
            .collect();
        for h in handles {
            h.join().expect("producer panicked");
        }
        let total = (producers * frames_per_producer) as u64;
        let audio = AdaptiveAudioStats::new(DEFAULT_AUDIO_BUFFER_MAX_MS, 0);
        let snapshot = stats.snapshot(&audio, VideoTelemetryExtras::default());
        assert_eq!(snapshot.outgoing_video_frames_produced, total);
        assert_eq!(snapshot.outgoing_video_frames_accepted, total);
        assert_eq!(snapshot.outgoing_video_frames_dropped, 0);
        assert!(
            snapshot.outgoing_video_queue_depth <= ENCODER_QUEUE_CAPACITY as u64,
            "queue depth {} exceeded capacity {}",
            snapshot.outgoing_video_queue_depth,
            ENCODER_QUEUE_CAPACITY
        );
        assert!(
            snapshot.outgoing_video_frames_coalesced > 0,
            "producers overran capacity but no coalesce was recorded"
        );
    }

    #[test]
    fn bus_sender_sink_accepts_copied_nv12_frames() {
        // The X11 screen-capture backend delivers CPU NV12 frames; they must be
        // accepted and routed through the software NV12 encode path.
        let (sender, _stats) = sender_for_tests();
        let sink = BusSenderSink {
            sender: sender.clone(),
            texture_capability: texture_source::TextureCapability::unavailable(
                texture_source::TextureEncodeError::NoHardwareEncoder,
            ),
        };
        let frame = BusScreenFrame::Nv12(frame_bus::Nv12Frame {
            data: vec![0u8; 8 * 8 + 8 * 4].into(),
            width: 8,
            height: 8,
            stride_y: 8,
            stride_uv: 8,
            timestamp_us: 1234,
        });
        assert_eq!(sink.enqueue(frame), EnqueueOutcome::Accepted);
        assert_eq!(sender.pending.len(), 1);
    }

    #[test]
    fn bus_sender_sink_rejects_copied_bgra_frames() {
        let (sender, _stats) = sender_for_tests();
        let sink = BusSenderSink {
            sender: sender.clone(),
            texture_capability: texture_source::TextureCapability::unavailable(
                texture_source::TextureEncodeError::NoHardwareEncoder,
            ),
        };
        let frame = BusScreenFrame::Bgra(frame_bus::BgraFrame {
            data: vec![0; 4 * 8 * 8],
            width: 8,
            height: 8,
            stride: 32,
            timestamp_us: 5678,
        });
        assert_eq!(sink.enqueue(frame), EnqueueOutcome::Rejected);
        assert_eq!(sender.pending.len(), 0);
    }

    #[test]
    fn native_screen_frame_sink_handle_exposes_nv12_callback() {
        let (sender, _stats) = sender_for_tests();
        let sink = BusSenderSink {
            sender: sender.clone(),
            texture_capability: texture_source::TextureCapability::unavailable(
                texture_source::TextureEncodeError::NoHardwareEncoder,
            ),
        };
        let handle = create_native_screen_frame_sink_handle(Arc::new(sink));
        assert!(handle.is_valid());
        // CPU NV12 ingestion is wired for the X11 backend; BGRA remains unused.
        assert!(handle.enqueue_nv12.is_some());
        assert!(handle.enqueue_bgra.is_none());
        assert!(handle.enqueue_screen_audio.is_none());
        unsafe { (handle.release)(handle.context) };
    }

    #[test]
    fn screen_audio_sink_handle_exposes_only_audio_callback() {
        let handle = create_screen_audio_sink_handle(Arc::new(ScreenAudioRing::new()));
        assert!(handle.is_valid());
        assert!(handle.enqueue_screen_audio.is_some());
        assert!(handle.enqueue_nv12.is_none());
        assert!(handle.enqueue_mac_cv_pixel_buffer.is_none());
        unsafe { (handle.release)(handle.context) };
    }

    #[test]
    fn screen_audio_ring_drops_oldest_when_full_and_counts() {
        let ring = ScreenAudioRing::new();
        for i in 0..(SCREEN_AUDIO_RING_CAP + 4) {
            ring.push(ScreenAudioChunk {
                samples: vec![i as f32; 2],
                num_frames: 1,
                channels: 2,
                sample_rate_hz: 48_000,
            });
        }
        assert_eq!(ring.dropped.load(Ordering::Relaxed), 4);
        let mut drained = 0;
        while ring.filled.pop().is_some() {
            drained += 1;
        }
        assert_eq!(drained, SCREEN_AUDIO_RING_CAP);
    }

    #[test]
    fn screen_audio_ring_recycles_buffers() {
        let ring = ScreenAudioRing::new();
        let mut buffer = ring.take_buffer();
        buffer.extend_from_slice(&[1.0, 2.0, 3.0]);
        ring.recycle(buffer);
        let reused = ring.take_buffer();
        assert!(reused.is_empty());
        assert!(reused.capacity() >= 3);
    }

    #[test]
    fn native_buffered_camera_source_exposes_only_native_frame_sink() {
        let (sender, _stats) = sender_for_tests();
        let frame_sink = Arc::new(BusSenderSink {
            sender: sender.clone(),
            texture_capability: texture_source::TextureCapability::unavailable(
                texture_source::TextureEncodeError::NoHardwareEncoder,
            ),
        });
        let track_sid = Arc::new(Mutex::new("TR_native_camera".to_string()));
        let source = CameraSource::NativeBuffered {
            track_sid: track_sid.clone(),
            video_sender: sender.clone(),
            frame_sink: frame_sink.clone(),
        };

        assert!(source.processed_sender().is_none());
        assert!(Arc::ptr_eq(
            &source.native_frame_sink().unwrap(),
            &frame_sink
        ));
        assert!(Arc::ptr_eq(&source.track_sid(), &track_sid));

        source.stop();
        assert_eq!(sender.enqueue(tiny_bgra_frame()), EnqueueResult::Rejected);
    }

    #[test]
    fn adaptive_video_sender_returns_coalesced_when_queue_is_full() {
        let (sender, _stats) = sender_for_tests();
        for _ in 0..ENCODER_QUEUE_CAPACITY {
            assert_eq!(sender.enqueue(tiny_bgra_frame()), EnqueueResult::Accepted);
        }
        assert_eq!(sender.enqueue(tiny_bgra_frame()), EnqueueResult::Coalesced);
        assert_eq!(sender.pending.len(), ENCODER_QUEUE_CAPACITY);
    }

    #[test]
    fn adaptive_video_sender_returns_rejected_after_stopping() {
        let (sender, _stats) = sender_for_tests();
        sender.stop();
        assert_eq!(sender.enqueue(tiny_bgra_frame()), EnqueueResult::Rejected);
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn bus_sender_sink_rejects_mac_cv_pixel_buffer_with_null_pointer() {
        let (sender, _stats) = sender_for_tests();
        let sink = BusSenderSink {
            sender: sender.clone(),
            texture_capability: texture_source::TextureCapability::unavailable(
                texture_source::TextureEncodeError::NoHardwareEncoder,
            ),
        };
        let mac_frame = unsafe {
            fluxer_screen_frame_bus::MacCvPixelBufferFrame::from_retained(
                std::ptr::null_mut(),
                8,
                8,
                0,
                42,
            )
        };
        let frame = BusScreenFrame::MacCvPixelBuffer(mac_frame);
        assert_eq!(sink.enqueue(frame), EnqueueOutcome::Rejected);
        assert_eq!(sender.pending.len(), 0);
    }

    #[test]
    fn parse_codec_agrees_with_pure_canonicaliser() {
        for name in crate::config::SUPPORTED_CODECS {
            assert!(parse_codec(name).is_some(), "engine must accept {name}");
        }
        assert_eq!(parse_codec("hevc"), Some(VideoCodec::H265));
    }

    #[test]
    fn session_rtt_uses_peer_connection_candidate_pair_without_track_stats() {
        let mut stats = stats_mod::ConnectionStats::default();

        absorb_session_rtt(&[candidate_pair_report(true, 0.037)], &[], &mut stats);

        assert_eq!(stats.rtt_ms, Some(37.0));
    }

    #[test]
    fn session_rtt_falls_back_to_subscriber_and_preserves_existing_rtt() {
        let mut stats = stats_mod::ConnectionStats::default();

        absorb_session_rtt(
            &[candidate_pair_report(false, 0.011)],
            &[candidate_pair_report(true, 0.042)],
            &mut stats,
        );

        assert_eq!(stats.rtt_ms, Some(42.0));
        absorb_session_rtt(&[candidate_pair_report(true, 0.100)], &[], &mut stats);
        assert_eq!(stats.rtt_ms, Some(42.0));
    }

    #[test]
    fn local_i420_payload_concatenates_tight_planes_and_meta_matches_contract() {
        let frame = yuv::I420 {
            width: 4,
            height: 2,
            y: vec![1u8; 8],
            u: vec![2u8; 2],
            v: vec![3u8; 2],
        };
        let (meta, buffer) = local_i420_to_callback_payload(
            &frame,
            123456,
            "PA_localA12345",
            "user_1_connection_1",
            "TR_localCamera01",
            "camera",
            "camera",
        );
        let expected_meta = format!(
            "{{\"bridgeVersion\":{},\"participantSid\":\"PA_localA12345\",\"participantIdentity\":\"user_1_connection_1\",\"trackSid\":\"TR_localCamera01\",\"trackName\":\"camera\",\"source\":\"camera\",\"width\":4,\"height\":2,\"timestampUs\":123456}}",
            crate::bridge_version::ENGINE_BRIDGE_VERSION
        );
        assert_eq!(meta, expected_meta);
        assert_eq!(&buffer[..], &[1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3]);
    }

    #[test]
    #[should_panic]
    fn local_video_frame_sink_rejects_empty_track_sid() {
        let _ = local_video_frame_sink(
            Arc::new(Mutex::new(None)),
            Arc::new(AtomicU64::new(0)),
            "PA_localA12345".to_string(),
            "user_1_connection_1".to_string(),
            Arc::new(Mutex::new(String::new())),
            "camera".to_string(),
            "camera".to_string(),
        );
    }

    #[test]
    #[should_panic]
    fn local_video_frame_sink_rejects_backwards_timestamps() {
        let sink = local_video_frame_sink(
            Arc::new(Mutex::new(None)),
            Arc::new(AtomicU64::new(0)),
            "PA_localA12345".to_string(),
            "user_1_connection_1".to_string(),
            Arc::new(Mutex::new("TR_localCamera01".to_string())),
            "camera".to_string(),
            "camera".to_string(),
        );
        let frame = yuv::I420 {
            width: 2,
            height: 2,
            y: vec![0u8; 4],
            u: vec![0u8; 1],
            v: vec![0u8; 1],
        };
        sink(&frame, 100);
        sink(&frame, 99);
    }

    fn camera_source_for_tests(track_sid: &str) -> (CameraSource, Arc<Mutex<String>>) {
        let shared = Arc::new(Mutex::new(track_sid.to_string()));
        let source = CameraSource::Device {
            track_sid: shared.clone(),
            stop: Arc::new(AtomicBool::new(false)),
            capture: None,
        };
        (source, shared)
    }

    struct OwnedTrackSlots {
        camera: Mutex<Option<CameraSource>>,
        screen_camera: Mutex<Option<CameraSource>>,
        screen: Mutex<Option<ScreenSource>>,
        screen_audio: Mutex<Option<ScreenAudioSource>>,
        mic: Mutex<Option<MicSource>>,
    }

    impl OwnedTrackSlots {
        fn new() -> Self {
            Self {
                camera: Mutex::new(None),
                screen_camera: Mutex::new(None),
                screen: Mutex::new(None),
                screen_audio: Mutex::new(None),
                mic: Mutex::new(None),
            }
        }

        fn as_slots(&self) -> LocalTrackSlots<'_> {
            LocalTrackSlots {
                camera: &self.camera,
                screen_camera: &self.screen_camera,
                screen: &self.screen,
                screen_audio: &self.screen_audio,
                mic: &self.mic,
            }
        }
    }

    fn track_sid_for_tests(sid: &str) -> TrackSid {
        assert!(sid.starts_with("TR_"));
        TrackSid::try_from(sid.to_string()).expect("test sid is TR_-prefixed")
    }

    #[test]
    fn local_track_republish_swaps_matching_camera_sid() {
        let (source, shared) = camera_source_for_tests("TR_oldCamera0001");
        let owned = OwnedTrackSlots::new();
        *owned.camera.lock() = Some(source);

        let swapped =
            apply_local_track_republish(&owned.as_slots(), "TR_oldCamera0001", "TR_newCamera0001");

        assert!(swapped);
        assert_eq!(*shared.lock(), "TR_newCamera0001");
    }

    #[test]
    fn local_track_republish_swaps_matching_screen_camera_sid() {
        let (source, shared) = camera_source_for_tests("TR_oldScreen0001");
        let owned = OwnedTrackSlots::new();
        *owned.screen_camera.lock() = Some(source);

        let swapped =
            apply_local_track_republish(&owned.as_slots(), "TR_oldScreen0001", "TR_newScreen0001");

        assert!(swapped);
        assert_eq!(*shared.lock(), "TR_newScreen0001");
    }

    #[test]
    fn local_track_republish_ignores_unrelated_previous_sid() {
        let (camera_source, camera_shared) = camera_source_for_tests("TR_camera000001");
        let (screen_source, screen_shared) = camera_source_for_tests("TR_screen000001");
        let owned = OwnedTrackSlots::new();
        *owned.camera.lock() = Some(camera_source);
        *owned.screen_camera.lock() = Some(screen_source);

        let swapped =
            apply_local_track_republish(&owned.as_slots(), "TR_unrelated0001", "TR_newTrack0001");

        assert!(!swapped);
        assert_eq!(*camera_shared.lock(), "TR_camera000001");
        assert_eq!(*screen_shared.lock(), "TR_screen000001");
    }

    #[test]
    fn local_track_republish_reports_already_applied_sid_as_matched() {
        let (source, shared) = camera_source_for_tests("TR_newCamera0001");
        let owned = OwnedTrackSlots::new();
        *owned.camera.lock() = Some(source);

        let swapped =
            apply_local_track_republish(&owned.as_slots(), "TR_oldCamera0001", "TR_newCamera0001");

        assert!(swapped);
        assert_eq!(*shared.lock(), "TR_newCamera0001");
    }

    #[test]
    #[should_panic]
    fn local_track_republish_rejects_empty_previous_sid() {
        let owned = OwnedTrackSlots::new();
        apply_local_track_republish(&owned.as_slots(), "", "TR_newTrack0001");
    }

    #[test]
    #[should_panic]
    fn local_track_republish_rejects_empty_republished_sid() {
        let owned = OwnedTrackSlots::new();
        apply_local_track_republish(&owned.as_slots(), "TR_oldTrack0001", "");
    }

    #[test]
    #[should_panic]
    fn local_track_republish_rejects_multiple_matching_slots() {
        let (camera_source, _camera_shared) = camera_source_for_tests("TR_duplicate0001");
        let (screen_source, _screen_shared) = camera_source_for_tests("TR_duplicate0001");
        let owned = OwnedTrackSlots::new();
        *owned.camera.lock() = Some(camera_source);
        *owned.screen_camera.lock() = Some(screen_source);
        apply_local_track_republish(&owned.as_slots(), "TR_duplicate0001", "TR_newTrack0001");
    }

    #[test]
    fn republish_track_sid_value_swaps_matching_sid() {
        let mut sid = track_sid_for_tests("TR_oldMic00000001");
        let matched = republish_track_sid_value(&mut sid, "TR_oldMic00000001", "TR_newMic00000001");
        assert_eq!(matched, 1);
        assert_eq!(sid.as_str(), "TR_newMic00000001");
    }

    #[test]
    fn republish_track_sid_value_ignores_unrelated_sid() {
        let mut sid = track_sid_for_tests("TR_screenAudio001");
        let matched = republish_track_sid_value(&mut sid, "TR_unrelated0001", "TR_newTrack0001");
        assert_eq!(matched, 0);
        assert_eq!(sid.as_str(), "TR_screenAudio001");
    }

    #[test]
    fn republish_track_sid_value_reports_already_applied_sid_as_matched() {
        let mut sid = track_sid_for_tests("TR_newMic00000001");
        let matched = republish_track_sid_value(&mut sid, "TR_oldMic00000001", "TR_newMic00000001");
        assert_eq!(matched, 1);
        assert_eq!(sid.as_str(), "TR_newMic00000001");
    }

    #[test]
    fn republish_screen_sids_swaps_track_and_metadata_sids_together() {
        let mut track_sid = track_sid_for_tests("TR_oldScreen0001");
        let mut metadata_track_sid = track_sid_for_tests("TR_oldScreen0001");
        let matched = republish_screen_sids(
            &mut track_sid,
            &mut metadata_track_sid,
            "TR_oldScreen0001",
            "TR_newScreen0001",
        );
        assert_eq!(matched, 1);
        assert_eq!(track_sid.as_str(), "TR_newScreen0001");
        assert_eq!(metadata_track_sid.as_str(), "TR_newScreen0001");
    }

    #[test]
    fn republish_screen_sids_leaves_unrelated_sids_unchanged() {
        let mut track_sid = track_sid_for_tests("TR_screen000001");
        let mut metadata_track_sid = track_sid_for_tests("TR_screen000001");
        let matched = republish_screen_sids(
            &mut track_sid,
            &mut metadata_track_sid,
            "TR_unrelated0001",
            "TR_newScreen0001",
        );
        assert_eq!(matched, 0);
        assert_eq!(track_sid.as_str(), "TR_screen000001");
        assert_eq!(metadata_track_sid.as_str(), "TR_screen000001");
    }

    #[test]
    #[should_panic]
    fn republish_screen_sids_rejects_diverged_metadata_sid() {
        let mut track_sid = track_sid_for_tests("TR_screen000001");
        let mut metadata_track_sid = track_sid_for_tests("TR_diverged0001");
        republish_screen_sids(
            &mut track_sid,
            &mut metadata_track_sid,
            "TR_screen000001",
            "TR_newScreen0001",
        );
    }

    #[test]
    fn store_camera_slot_fills_empty_slot() {
        let (source, shared) = camera_source_for_tests("TR_camera000001");
        let slot = Mutex::new(None);
        store_camera_slot(&slot, source);
        assert!(slot.lock().is_some());
        assert_eq!(*shared.lock(), "TR_camera000001");
    }

    #[test]
    #[should_panic]
    fn store_camera_slot_rejects_occupied_slot() {
        let (first, _) = camera_source_for_tests("TR_camera000001");
        let (second, _) = camera_source_for_tests("TR_camera000002");
        let slot = Mutex::new(Some(first));
        store_camera_slot(&slot, second);
    }

    #[test]
    fn remove_camera_slot_if_held_removes_only_matching_source() {
        let (source, shared) = camera_source_for_tests("TR_camera000001");
        let slot = Mutex::new(Some(source));
        let unrelated = Arc::new(Mutex::new("TR_camera000002".to_string()));

        remove_camera_slot_if_held(&slot, &unrelated);
        assert!(slot.lock().is_some());

        remove_camera_slot_if_held(&slot, &shared);
        assert!(slot.lock().is_none());
    }

    fn camera_options_for_tests(device_id: Option<&str>) -> CameraOptions {
        CameraOptions {
            device_id: device_id.map(str::to_string),
            width: None,
            height: None,
            frame_rate: None,
            mirror: None,
            background_mode: None,
            background_custom_media_path: None,
            background_custom_media_kind: None,
            background_blur_strength: None,
            codec: None,
            max_bitrate_bps: None,
            max_framerate: None,
        }
    }

    #[test]
    fn camera_swap_order_opens_new_first_for_a_different_device() {
        use crate::camera::CameraSelector;
        assert_eq!(
            camera_swap_order(&CameraSelector::Index(0), &CameraSelector::Index(1)),
            CameraSwapOrder::OpenNewThenStopOld
        );
        assert_eq!(
            camera_swap_order(
                &CameraSelector::Index(0),
                &CameraSelector::Id("front".to_string())
            ),
            CameraSwapOrder::OpenNewThenStopOld
        );
        assert_eq!(
            camera_swap_order(
                &CameraSelector::Id("front".to_string()),
                &CameraSelector::Id("back".to_string())
            ),
            CameraSwapOrder::OpenNewThenStopOld
        );
    }

    #[test]
    fn camera_swap_order_stops_old_first_for_the_same_device() {
        use crate::camera::CameraSelector;
        assert_eq!(
            camera_swap_order(&CameraSelector::Index(2), &CameraSelector::Index(2)),
            CameraSwapOrder::StopOldThenOpenNew
        );
        assert_eq!(
            camera_swap_order(
                &CameraSelector::Id("front".to_string()),
                &CameraSelector::Id("front".to_string())
            ),
            CameraSwapOrder::StopOldThenOpenNew
        );
    }

    #[test]
    fn camera_background_config_threads_effect_strengths_from_options() {
        let mut opts = camera_options_for_tests(None);
        opts.background_mode = Some("blur".to_string());
        opts.background_blur_strength = Some(80);

        let config = native_camera_background_config(&opts).unwrap();
        assert_eq!(config.blur_strength, 80);

        let defaults = native_camera_background_config(&camera_options_for_tests(None)).unwrap();
        assert_eq!(
            defaults.blur_strength,
            crate::camera_background::CAMERA_EFFECT_STRENGTH_DEFAULT
        );
    }

    #[tokio::test]
    async fn update_camera_capture_requires_a_published_device_camera() {
        let engine = VoiceEngine::new();

        let error = engine
            .update_camera_capture(camera_options_for_tests(None))
            .await
            .unwrap_err();
        assert_eq!(error.reason, "camera is not published");

        let (sender, _stats) = sender_for_tests();
        *engine.camera.lock() = Some(CameraSource::Processed {
            track_sid: Arc::new(Mutex::new("TR_processed0001".to_string())),
            video_sender: sender,
        });
        let error = engine
            .update_camera_capture(camera_options_for_tests(None))
            .await
            .unwrap_err();
        assert_eq!(error.reason, "camera capture is not device-managed");
    }

    #[tokio::test]
    async fn update_camera_capture_rejects_device_slots_without_capture_context() {
        let engine = VoiceEngine::new();
        let (source, _shared) = camera_source_for_tests("TR_camera000001");
        *engine.camera.lock() = Some(source);

        let error = engine
            .update_camera_capture(camera_options_for_tests(Some("1")))
            .await
            .unwrap_err();
        assert_eq!(error.reason, "camera capture is not device-managed");
    }

    #[test]
    fn commit_device_camera_swap_replaces_only_the_expected_worker_stop() {
        let (source, shared) = camera_source_for_tests("TR_camera000001");
        let slot = Mutex::new(Some(source));
        let expected = match slot.lock().as_ref().unwrap() {
            CameraSource::Device { stop, .. } => stop.clone(),
            _ => panic!("device slot expected"),
        };
        let swapped = Arc::new(AtomicBool::new(false));
        let request =
            camera::CameraRequest::from_opts(None, None, None, None, false, Default::default());

        assert!(commit_device_camera_swap(
            &slot, &shared, &expected, &swapped, &request
        ));
        assert!(!commit_device_camera_swap(
            &slot, &shared, &expected, &swapped, &request
        ));

        match slot.lock().as_ref().unwrap() {
            CameraSource::Device { stop, .. } => assert!(Arc::ptr_eq(stop, &swapped)),
            _ => panic!("device slot expected"),
        }
    }

    #[test]
    fn commit_device_camera_swap_rejects_missing_or_foreign_slots() {
        let request =
            camera::CameraRequest::from_opts(None, None, None, None, false, Default::default());
        let expected = Arc::new(AtomicBool::new(false));
        let swapped = Arc::new(AtomicBool::new(false));

        let empty = Mutex::new(None);
        let foreign_sid = Arc::new(Mutex::new("TR_camera000001".to_string()));
        assert!(!commit_device_camera_swap(
            &empty,
            &foreign_sid,
            &expected,
            &swapped,
            &request
        ));

        let (source, _shared) = camera_source_for_tests("TR_camera000001");
        let slot = Mutex::new(Some(source));
        assert!(!commit_device_camera_swap(
            &slot,
            &foreign_sid,
            &expected,
            &swapped,
            &request
        ));
    }

    #[test]
    fn local_video_frame_sink_payload_reflects_republished_sid() {
        let (source, shared) = camera_source_for_tests("TR_oldCamera0001");
        let owned = OwnedTrackSlots::new();
        *owned.camera.lock() = Some(source);
        let frame = yuv::I420 {
            width: 4,
            height: 2,
            y: vec![1u8; 8],
            u: vec![2u8; 2],
            v: vec![3u8; 2],
        };

        let (meta_before, _) = local_video_frame_sink_payload(
            &frame,
            100,
            "PA_localA12345",
            "user_1_connection_1",
            &shared,
            "camera",
            "camera",
        );
        assert!(meta_before.contains("\"trackSid\":\"TR_oldCamera0001\""));

        assert!(apply_local_track_republish(
            &owned.as_slots(),
            "TR_oldCamera0001",
            "TR_newCamera0001"
        ));
        let (meta_after, _) = local_video_frame_sink_payload(
            &frame,
            200,
            "PA_localA12345",
            "user_1_connection_1",
            &shared,
            "camera",
            "camera",
        );
        assert!(meta_after.contains("\"trackSid\":\"TR_newCamera0001\""));
    }

    #[test]
    fn record_first_error_keeps_earliest_error() {
        let mut slot = None;
        record_first_error(&mut slot, Ok(()));
        assert!(slot.is_none());
        record_first_error(&mut slot, Err(napi::Error::from_reason("first")));
        record_first_error(&mut slot, Err(napi::Error::from_reason("second")));
        let error = slot.expect("first error retained");
        assert!(error.reason.contains("first"));
    }

    #[test]
    fn video_frame_meta_json_matches_contract() {
        let json = video_frame_meta_json(
            "PA_remoteB12345",
            "user_2_connection_2",
            "TR_remoteVideo01",
            "screen",
            "screen_share",
            1920,
            1080,
            123456,
        );
        let expected = format!(
            "{{\"bridgeVersion\":{},\"participantSid\":\"PA_remoteB12345\",\"participantIdentity\":\"user_2_connection_2\",\"trackSid\":\"TR_remoteVideo01\",\"trackName\":\"screen\",\"source\":\"screen_share\",\"width\":1920,\"height\":1080,\"timestampUs\":123456}}",
            crate::bridge_version::ENGINE_BRIDGE_VERSION
        );
        assert_eq!(json, expected);
    }

    #[test]
    fn tight_i420_len_matches_plane_geometry() {
        assert_eq!(tight_i420_len(4, 2), 8 + 2 + 2);
        assert_eq!(tight_i420_len(1920, 1080), 1920 * 1080 * 3 / 2);
    }
}
