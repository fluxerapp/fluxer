// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env;
use std::os::fd::OwnedFd;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc::{Receiver, SyncSender, TrySendError, sync_channel};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use pipewire as pw;
use pw::channel::{Receiver as PwReceiver, Sender as PwSender, channel as pw_channel};
use pw::context::ContextRc;
use pw::keys;
use pw::main_loop::MainLoopRc;
use pw::properties::properties;
use pw::spa;
use spa::buffer::{Data, DataType};
use spa::param::format::{MediaSubtype, MediaType};
use spa::param::format_utils;
use spa::param::video::{VideoFormat, VideoInfoRaw};
use spa::pod::{ChoiceValue, Object, Pod, Property, PropertyFlags, Value as SpaValue};
use spa::utils::{Choice, ChoiceEnum, ChoiceFlags, Fraction, Rectangle, SpaTypes};

use crate::capture_state::{LinuxCaptureEvent, LinuxCaptureFault, LinuxCaptureStateMachine};
use crate::frame_buffer_pool::PooledFrameBuffer;
use crate::nv12_packing::{Nv12Layout, bgra_to_nv12, pack_nv12};

pub use crate::frame_buffer_pool::{
    LINUX_FRAME_BYTES_MAX, LINUX_FRAME_DIM_MAX, LINUX_SCREEN_FRAME_POOL_CAP, LinuxFrameBufferPool,
};

const READY_TIMEOUT: Duration = Duration::from_millis(2_000);
const STAGING_SLOT_COUNT: usize = 2;
const MODIFIER_COUNT_MAX: usize = 16;
const SCREEN_CAPTURE_DMABUF_ENV: &str = "FLUXER_SCREEN_CAPTURE_DMABUF";

const DRM_FORMAT_XRGB8888: u32 = fourcc(*b"XR24");
const DRM_FORMAT_ARGB8888: u32 = fourcc(*b"AR24");
const DRM_FORMAT_NV12: u32 = fourcc(*b"NV12");
const DRM_FORMAT_MOD_LINEAR: u64 = 0;
const DRM_FORMAT_MOD_INVALID: u64 = (1u64 << 56) - 1;
const DMABUF_MODIFIERS_BASELINE: [u64; 2] = [DRM_FORMAT_MOD_LINEAR, DRM_FORMAT_MOD_INVALID];

const fn fourcc(bytes: [u8; 4]) -> u32 {
    bytes[0] as u32 | (bytes[1] as u32) << 8 | (bytes[2] as u32) << 16 | (bytes[3] as u32) << 24
}

pub type FrameCallback = Arc<dyn Fn(VideoFrame) + Send + Sync + 'static>;
pub type LifecycleCallback = Arc<dyn Fn(&str, &str) + Send + Sync + 'static>;
pub type PoolExhaustionCallback = Arc<dyn Fn(u64) + Send + Sync + 'static>;

pub struct VideoFrame {
    pub width: u32,
    pub height: u32,
    pub stride_y: u32,
    pub stride_uv: u32,
    pub timestamp_us: i64,
    pub data: VideoFrameData,
    pub dmabuf: Option<DmabufFrameMetadata>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DmabufFrameMetadata {
    pub fds: [i32; 4],
    pub plane_count: u32,
    pub drm_format: u32,
    pub modifier: u64,
    pub strides: [u32; 4],
    pub offsets: [u32; 4],
    pub device_uuid: Option<[u8; 16]>,
}

pub enum VideoFrameData {
    Empty,
    Owned(Vec<u8>),
    Pooled(PooledFrameBuffer),
}

impl VideoFrameData {
    pub fn empty() -> Self {
        Self::Empty
    }

    pub fn from_vec(buf: Vec<u8>) -> Self {
        let len = buf.len();
        let data = Self::Owned(buf);
        assert_eq!(data.len(), len);
        data
    }

    pub fn as_slice(&self) -> &[u8] {
        match self {
            Self::Empty => &[],
            Self::Owned(v) => v.as_slice(),
            Self::Pooled(p) => p.as_slice(),
        }
    }

    pub fn len(&self) -> usize {
        let len = match self {
            Self::Empty => 0,
            Self::Owned(v) => v.len(),
            Self::Pooled(p) => p.len(),
        };
        assert!(len <= LINUX_FRAME_BYTES_MAX);
        len
    }

    pub fn is_empty(&self) -> bool {
        let empty = match self {
            Self::Empty => true,
            Self::Owned(v) => v.is_empty(),
            Self::Pooled(p) => p.len() == 0,
        };
        assert!(empty == (self.len() == 0));
        empty
    }

    pub fn to_vec(&self) -> Vec<u8> {
        let cloned = match self {
            Self::Empty => Vec::new(),
            Self::Owned(v) => v.clone(),
            Self::Pooled(p) => p.as_slice().to_vec(),
        };
        assert_eq!(cloned.len(), self.len());
        cloned
    }

    pub fn into_bus_frame_data(self) -> fluxer_screen_frame_bus::FrameData {
        let len = self.len();
        let data = match self {
            Self::Empty => fluxer_screen_frame_bus::FrameData::from(Vec::new()),
            Self::Owned(v) => fluxer_screen_frame_bus::FrameData::from(v),
            Self::Pooled(p) => p.into_shared_frame_data(),
        };
        assert_eq!(data.len(), len);
        data
    }
}

impl std::ops::Deref for VideoFrameData {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        self.as_slice()
    }
}

impl std::fmt::Debug for VideoFrameData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => f.debug_tuple("VideoFrameData::Empty").finish(),
            Self::Owned(v) => f
                .debug_struct("VideoFrameData::Owned")
                .field("len", &v.len())
                .finish(),
            Self::Pooled(p) => f
                .debug_struct("VideoFrameData::Pooled")
                .field("len", &p.len())
                .field("slot_index", &p.slot_index())
                .finish(),
        }
    }
}

#[derive(Debug)]
pub enum BridgeError {
    DaemonUnreachable,
    Spawn,
    Negotiate,
}

impl std::fmt::Display for BridgeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DaemonUnreachable => f.write_str("DaemonUnreachable"),
            Self::Spawn => f.write_str("Spawn"),
            Self::Negotiate => f.write_str("Negotiate"),
        }
    }
}

enum Command {
    Stop,
}

pub struct PipeWireVideoStream {
    tx: PwSender<Command>,
    running: Arc<AtomicBool>,
    thread: std::sync::Mutex<Option<JoinHandle<()>>>,
    convert_thread: std::sync::Mutex<Option<JoinHandle<()>>>,
    pool: Arc<LinuxFrameBufferPool>,
    frames_dropped_convert_queue_full: Arc<AtomicU64>,
}

impl PipeWireVideoStream {
    pub fn open(
        portal_fd: OwnedFd,
        node_id: u32,
        on_frame: FrameCallback,
        on_lifecycle: LifecycleCallback,
        pool: Arc<LinuxFrameBufferPool>,
        on_pool_exhausted: Option<PoolExhaustionCallback>,
    ) -> Result<Self, BridgeError> {
        assert!(pool.capacity() == LINUX_SCREEN_FRAME_POOL_CAP);
        assert!(pool.bytes_per_buffer() > 0);
        let (tx, rx) = pw_channel::<Command>();
        let running = Arc::new(AtomicBool::new(true));
        let (ready_tx, ready_rx) = std::sync::mpsc::sync_channel::<bool>(1);
        let frames_dropped_convert_queue_full = Arc::new(AtomicU64::new(0));
        let (staging_rt, staging_worker) = build_staging_channels(&pool);
        let convert_thread = spawn_convert_worker(
            staging_worker,
            Arc::clone(&pool),
            on_frame.clone(),
            on_pool_exhausted,
        )
        .map_err(|_| BridgeError::Spawn)?;
        let spawned = spawn_pw_thread(
            portal_fd,
            node_id,
            rx,
            ready_tx,
            running.clone(),
            on_frame,
            on_lifecycle,
            Arc::clone(&pool),
            staging_rt,
            Arc::clone(&frames_dropped_convert_queue_full),
        );
        let handle = match spawned {
            Ok(handle) => handle,
            Err(_) => {
                let _ = convert_thread.join();
                return Err(BridgeError::Spawn);
            }
        };
        match ready_rx.recv_timeout(READY_TIMEOUT) {
            Ok(true) => Ok(Self {
                tx,
                running,
                thread: std::sync::Mutex::new(Some(handle)),
                convert_thread: std::sync::Mutex::new(Some(convert_thread)),
                pool,
                frames_dropped_convert_queue_full,
            }),
            _ => {
                let _ = tx.send(Command::Stop);
                let _ = handle.join();
                let _ = convert_thread.join();
                Err(BridgeError::DaemonUnreachable)
            }
        }
    }

    pub fn stop(&self) {
        self.running.store(false, Ordering::Release);
        let _ = self.tx.send(Command::Stop);
    }

    pub fn frames_dropped_pool_exhausted(&self) -> u64 {
        self.pool.frames_dropped_pool_exhausted()
    }

    pub fn frames_dropped_oversized(&self) -> u64 {
        self.pool.frames_dropped_oversized()
    }

    pub fn frames_dropped_convert_queue_full(&self) -> u64 {
        let dropped = self
            .frames_dropped_convert_queue_full
            .load(Ordering::Relaxed);
        assert!(dropped <= u64::MAX / 2);
        dropped
    }
}

impl Drop for PipeWireVideoStream {
    fn drop(&mut self) {
        self.stop();
        if let Ok(mut guard) = self.thread.lock()
            && let Some(handle) = guard.take()
        {
            let _ = handle.join();
        }
        if let Ok(mut guard) = self.convert_thread.lock()
            && let Some(handle) = guard.take()
        {
            let _ = handle.join();
        }
    }
}

struct UserData {
    on_frame: FrameCallback,
    width: u32,
    height: u32,
    pixel_format: VideoFormat,
    modifier: u64,
    pool: Arc<LinuxFrameBufferPool>,
    staging_free_rx: Receiver<StagingFrame>,
    staging_free_tx: SyncSender<StagingFrame>,
    staging_filled_tx: SyncSender<StagingFrame>,
    frames_dropped_convert_queue_full: Arc<AtomicU64>,
}

struct StagingFrame {
    bytes: Box<[u8]>,
    copied_len: usize,
    width: u32,
    height: u32,
    pixel_format: VideoFormat,
    timestamp_us: i64,
}

impl StagingFrame {
    fn new(bytes_capacity: usize) -> Self {
        assert!(bytes_capacity > 0);
        assert!(bytes_capacity <= LINUX_FRAME_BYTES_MAX);
        Self {
            bytes: vec![0u8; bytes_capacity].into_boxed_slice(),
            copied_len: 0,
            width: 0,
            height: 0,
            pixel_format: VideoFormat::Unknown,
            timestamp_us: 0,
        }
    }
}

struct StagingRtEndpoints {
    free_rx: Receiver<StagingFrame>,
    free_tx: SyncSender<StagingFrame>,
    filled_tx: SyncSender<StagingFrame>,
}

struct StagingWorkerEndpoints {
    filled_rx: Receiver<StagingFrame>,
    free_tx: SyncSender<StagingFrame>,
}

fn staging_bytes_for_pool(pool: &LinuxFrameBufferPool) -> usize {
    let nv12_bytes = pool.bytes_per_buffer();
    assert!(nv12_bytes > 0);
    let bgra_bytes = nv12_bytes
        .div_ceil(3)
        .saturating_mul(8)
        .min(LINUX_FRAME_BYTES_MAX);
    assert!(bgra_bytes >= nv12_bytes);
    assert!(bgra_bytes <= LINUX_FRAME_BYTES_MAX);
    bgra_bytes
}

fn build_staging_channels(
    pool: &LinuxFrameBufferPool,
) -> (StagingRtEndpoints, StagingWorkerEndpoints) {
    let staging_bytes = staging_bytes_for_pool(pool);
    assert!(staging_bytes > 0);
    let (free_tx, free_rx) = sync_channel::<StagingFrame>(STAGING_SLOT_COUNT);
    let (filled_tx, filled_rx) = sync_channel::<StagingFrame>(STAGING_SLOT_COUNT);
    for _ in 0..STAGING_SLOT_COUNT {
        let sent = free_tx.try_send(StagingFrame::new(staging_bytes));
        assert!(sent.is_ok());
    }
    (
        StagingRtEndpoints {
            free_rx,
            free_tx: free_tx.clone(),
            filled_tx,
        },
        StagingWorkerEndpoints { filled_rx, free_tx },
    )
}

fn spawn_pw_thread(
    portal_fd: OwnedFd,
    node_id: u32,
    rx: PwReceiver<Command>,
    ready_tx: std::sync::mpsc::SyncSender<bool>,
    running: Arc<AtomicBool>,
    on_frame: FrameCallback,
    on_lifecycle: LifecycleCallback,
    pool: Arc<LinuxFrameBufferPool>,
    staging_rt: StagingRtEndpoints,
    frames_dropped_convert_queue_full: Arc<AtomicU64>,
) -> std::io::Result<JoinHandle<()>> {
    assert!(pool.capacity() == LINUX_SCREEN_FRAME_POOL_CAP);
    assert!(pool.bytes_per_buffer() > 0);
    thread::Builder::new()
        .name("fluxer-linux-screen-capture-pw".to_string())
        .spawn(move || {
            run_worker(
                portal_fd,
                node_id,
                rx,
                ready_tx,
                running,
                on_frame,
                on_lifecycle,
                pool,
                staging_rt,
                frames_dropped_convert_queue_full,
            );
        })
}

fn spawn_convert_worker(
    endpoints: StagingWorkerEndpoints,
    pool: Arc<LinuxFrameBufferPool>,
    on_frame: FrameCallback,
    on_pool_exhausted: Option<PoolExhaustionCallback>,
) -> std::io::Result<JoinHandle<()>> {
    assert!(pool.capacity() == LINUX_SCREEN_FRAME_POOL_CAP);
    assert!(pool.bytes_per_buffer() > 0);
    thread::Builder::new()
        .name("fluxer-linux-screen-capture-convert".to_string())
        .spawn(move || {
            run_convert_worker(endpoints, pool, on_frame, on_pool_exhausted);
        })
}

fn run_convert_worker(
    endpoints: StagingWorkerEndpoints,
    pool: Arc<LinuxFrameBufferPool>,
    on_frame: FrameCallback,
    on_pool_exhausted: Option<PoolExhaustionCallback>,
) {
    assert!(pool.capacity() == LINUX_SCREEN_FRAME_POOL_CAP);
    assert!(pool.bytes_per_buffer() > 0);
    while let Ok(mut staging) = endpoints.filled_rx.recv() {
        convert_staged_frame(&staging, &pool, &on_frame, on_pool_exhausted.as_ref());
        staging.copied_len = 0;
        match endpoints.free_tx.try_send(staging) {
            Ok(()) => {}
            Err(TrySendError::Disconnected(_)) => return,
            Err(TrySendError::Full(_)) => {
                unreachable!("staging slots are conserved; free queue can never overflow")
            }
        }
    }
}

fn convert_staged_frame(
    staging: &StagingFrame,
    pool: &Arc<LinuxFrameBufferPool>,
    on_frame: &FrameCallback,
    on_pool_exhausted: Option<&PoolExhaustionCallback>,
) {
    assert!(staging.copied_len > 0);
    assert!(staging.copied_len <= staging.bytes.len());
    let layout = Nv12Layout {
        width: staging.width,
        height: staging.height,
        stride_y: staging.width,
        stride_uv: staging.width,
    };
    let Some(total_bytes) = layout.packed_size() else {
        return;
    };
    if total_bytes > pool.bytes_per_buffer() {
        pool.note_frame_dropped_oversized();
        return;
    }
    let Some(mut pooled) = acquire_or_drop(pool, on_pool_exhausted) else {
        return;
    };
    let ok = fill_pool_buffer_from_raw(
        &mut pooled,
        &staging.bytes[..staging.copied_len],
        layout,
        0,
        staging.pixel_format,
        total_bytes,
    );
    if !ok {
        return;
    }
    let video_frame = VideoFrame {
        width: layout.width,
        height: layout.height,
        stride_y: layout.packed_stride_y(),
        stride_uv: layout.packed_stride_uv(),
        timestamp_us: staging.timestamp_us,
        data: VideoFrameData::Pooled(pooled),
        dmabuf: None,
    };
    on_frame(video_frame);
}

fn monotonic_us() -> i64 {
    let us = monotonic_ns() / 1_000;
    assert!(us <= i64::MAX as u64);
    us as i64
}

fn monotonic_ns() -> u64 {
    let mut ts: libc::timespec = unsafe { std::mem::zeroed() };
    let rc = unsafe { libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut ts) };
    assert!(rc == 0);
    assert!(ts.tv_sec >= 0);
    assert!(ts.tv_nsec >= 0);
    (ts.tv_sec as u64)
        .saturating_mul(1_000_000_000)
        .saturating_add(ts.tv_nsec as u64)
}

fn run_worker(
    portal_fd: OwnedFd,
    node_id: u32,
    rx: PwReceiver<Command>,
    ready_tx: std::sync::mpsc::SyncSender<bool>,
    running: Arc<AtomicBool>,
    on_frame: FrameCallback,
    on_lifecycle: LifecycleCallback,
    pool: Arc<LinuxFrameBufferPool>,
    staging_rt: StagingRtEndpoints,
    frames_dropped_convert_queue_full: Arc<AtomicU64>,
) {
    assert!(pool.capacity() == LINUX_SCREEN_FRAME_POOL_CAP);
    assert!(pool.bytes_per_buffer() > 0);
    pw::init();
    let Ok(mainloop) = MainLoopRc::new(None) else {
        let _ = ready_tx.send(false);
        return;
    };
    let Ok(context) = ContextRc::new(&mainloop, None) else {
        let _ = ready_tx.send(false);
        return;
    };
    let Ok(core) = context.connect_fd_rc(portal_fd, None) else {
        let _ = ready_tx.send(false);
        return;
    };

    let stream_props = properties! {
        *keys::MEDIA_TYPE => "Video",
        *keys::MEDIA_CATEGORY => "Capture",
        *keys::MEDIA_ROLE => "Screen",
        *keys::NODE_NAME => "fluxer-screen-capture",
    };

    let Ok(stream) = pw::stream::StreamRc::new(core.clone(), "fluxer-screen-capture", stream_props)
    else {
        let _ = ready_tx.send(false);
        return;
    };

    let user_data = UserData {
        on_frame: on_frame.clone(),
        width: 0,
        height: 0,
        pixel_format: VideoFormat::Unknown,
        modifier: 0,
        pool: Arc::clone(&pool),
        staging_free_rx: staging_rt.free_rx,
        staging_free_tx: staging_rt.free_tx,
        staging_filled_tx: staging_rt.filled_tx,
        frames_dropped_convert_queue_full,
    };

    let on_lifecycle_for_state = on_lifecycle.clone();
    let capture_fsm_for_state: Arc<Mutex<LinuxCaptureStateMachine>> =
        Arc::new(Mutex::new(LinuxCaptureStateMachine::new(monotonic_ns())));

    let listener = stream
        .add_local_listener_with_user_data(user_data)
        .state_changed(move |_, _, old, new| {
            dispatch_state_change_to_fsm(&capture_fsm_for_state, &old, &new);
            match new {
                pw::stream::StreamState::Error(msg) => {
                    on_lifecycle_for_state("error", msg.as_str());
                }
                pw::stream::StreamState::Unconnected
                    if !matches!(old, pw::stream::StreamState::Connecting) =>
                {
                    on_lifecycle_for_state("closed-clean", "");
                }
                _ => {}
            }
        })
        .param_changed(move |stream, user_data, id, param| {
            let Some(param) = param else { return };
            if id != spa::param::ParamType::Format.as_raw() {
                return;
            }
            let Ok((media_type, media_subtype)) = format_utils::parse_format(param) else {
                return;
            };
            if media_type != MediaType::Video || media_subtype != MediaSubtype::Raw {
                return;
            }
            if let Some(chosen) = fixate_modifier_if_unfixated(stream, param) {
                user_data.modifier = chosen;
                return;
            }
            let mut info = VideoInfoRaw::new();
            if info.parse(param).is_err() {
                return;
            }
            let size = info.size();
            user_data.width = size.width;
            user_data.height = size.height;
            user_data.pixel_format = info.format();
            user_data.modifier = info.modifier();
        })
        .process(move |stream, user_data| {
            process_pipewire_buffer(stream, user_data);
        })
        .register();
    let Ok(listener) = listener else {
        let _ = ready_tx.send(false);
        return;
    };

    let mut params = match build_format_params() {
        Some(p) => p,
        None => {
            let _ = ready_tx.send(false);
            return;
        }
    };
    let param_refs: Vec<&Pod> = params.iter_mut().map(|p| p.as_ref()).collect();
    let mut param_refs = param_refs;
    if stream
        .connect(
            spa::utils::Direction::Input,
            Some(node_id),
            pw::stream::StreamFlags::AUTOCONNECT
                | pw::stream::StreamFlags::MAP_BUFFERS
                | pw::stream::StreamFlags::RT_PROCESS,
            &mut param_refs,
        )
        .is_err()
    {
        let _ = ready_tx.send(false);
        return;
    }

    let mainloop_weak = mainloop.downgrade();
    let _attached = rx.attach(mainloop.loop_(), move |cmd| match cmd {
        Command::Stop => {
            if let Some(ml) = mainloop_weak.upgrade() {
                ml.quit();
            }
        }
    });

    let _ = ready_tx.send(true);
    mainloop.run();
    drop(listener);
    let _ = stream.disconnect();
    running.store(false, Ordering::Release);
}

fn dispatch_state_change_to_fsm(
    fsm: &Arc<Mutex<LinuxCaptureStateMachine>>,
    old: &pw::stream::StreamState,
    new: &pw::stream::StreamState,
) {
    let Some(event) = stream_state_to_fsm_event(old, new) else {
        return;
    };
    let Ok(mut guard) = fsm.lock() else {
        return;
    };
    let _ = guard.dispatch(event, monotonic_ns());
}

fn stream_state_to_fsm_event(
    old: &pw::stream::StreamState,
    new: &pw::stream::StreamState,
) -> Option<LinuxCaptureEvent> {
    match new {
        pw::stream::StreamState::Streaming => Some(LinuxCaptureEvent::Connected),
        pw::stream::StreamState::Error(msg) => Some(LinuxCaptureEvent::Faulted(
            LinuxCaptureFault::StreamError(stream_error_code(msg)),
        )),
        pw::stream::StreamState::Unconnected => match old {
            pw::stream::StreamState::Streaming | pw::stream::StreamState::Paused => {
                Some(LinuxCaptureEvent::Faulted(LinuxCaptureFault::NodeRemoved))
            }
            _ => None,
        },
        pw::stream::StreamState::Connecting => None,
        pw::stream::StreamState::Paused => None,
    }
}

fn stream_error_code(msg: &str) -> i32 {
    let trimmed = msg.trim();
    if trimmed.is_empty() {
        return -1;
    }
    let mut acc: i32 = 0;
    for byte in trimmed.as_bytes().iter().take(8) {
        acc = acc.wrapping_mul(31).wrapping_add(*byte as i32);
    }
    if acc == 0 { -1 } else { acc }
}

struct OwnedPodBytes(Vec<u8>);

impl OwnedPodBytes {
    fn as_ref(&self) -> &Pod {
        Pod::from_bytes(&self.0).expect("serialized pod is valid")
    }
}

fn build_format_params() -> Option<Vec<OwnedPodBytes>> {
    let formats = [VideoFormat::NV12, VideoFormat::BGRA, VideoFormat::BGRx];
    let advertise_modifiers = dmabuf_modifiers_enabled();
    let mut params: Vec<OwnedPodBytes> = Vec::with_capacity(formats.len() * 2);
    for format in formats {
        if advertise_modifiers {
            let pod = serialize_video_format_pod(format, Some(&DMABUF_MODIFIERS_BASELINE))?;
            params.push(OwnedPodBytes(pod));
        }
        let pod = serialize_video_format_pod(format, None)?;
        params.push(OwnedPodBytes(pod));
    }
    assert!(params.len() >= formats.len());
    assert!(params.len() <= formats.len() * 2);
    Some(params)
}

fn dmabuf_modifiers_enabled() -> bool {
    match env::var(SCREEN_CAPTURE_DMABUF_ENV) {
        Ok(value) => !dmabuf_env_value_disables(&value),
        Err(_) => true,
    }
}

fn dmabuf_env_value_disables(value: &str) -> bool {
    let normalized = value.trim().to_ascii_lowercase();
    matches!(
        normalized.as_str(),
        "off" | "0" | "false" | "no" | "disabled"
    )
}

fn modifier_choice_property(modifiers: &[u64]) -> Property {
    assert!(!modifiers.is_empty());
    assert!(modifiers.len() <= MODIFIER_COUNT_MAX);
    let alternatives: Vec<i64> = modifiers.iter().map(|modifier| *modifier as i64).collect();
    Property {
        key: spa::sys::SPA_FORMAT_VIDEO_modifier,
        flags: PropertyFlags::MANDATORY | PropertyFlags::DONT_FIXATE,
        value: SpaValue::Choice(ChoiceValue::Long(Choice(
            ChoiceFlags::empty(),
            ChoiceEnum::Enum {
                default: alternatives[0],
                alternatives,
            },
        ))),
    }
}

fn serialize_video_format_pod(format: VideoFormat, modifiers: Option<&[u64]>) -> Option<Vec<u8>> {
    let mut properties = vec![
        Property {
            key: spa::sys::SPA_FORMAT_mediaType,
            flags: PropertyFlags::empty(),
            value: SpaValue::Id(spa::utils::Id(spa::sys::SPA_MEDIA_TYPE_video)),
        },
        Property {
            key: spa::sys::SPA_FORMAT_mediaSubtype,
            flags: PropertyFlags::empty(),
            value: SpaValue::Id(spa::utils::Id(spa::sys::SPA_MEDIA_SUBTYPE_raw)),
        },
        Property {
            key: spa::sys::SPA_FORMAT_VIDEO_format,
            flags: PropertyFlags::empty(),
            value: SpaValue::Id(spa::utils::Id(format.as_raw())),
        },
    ];
    if let Some(modifiers) = modifiers {
        assert!(!modifiers.is_empty());
        assert!(modifiers.len() <= MODIFIER_COUNT_MAX);
        properties.push(modifier_choice_property(modifiers));
    }
    properties.push(Property {
        key: spa::sys::SPA_FORMAT_VIDEO_size,
        flags: PropertyFlags::empty(),
        value: SpaValue::Choice(ChoiceValue::Rectangle(Choice(
            ChoiceFlags::empty(),
            ChoiceEnum::Range {
                default: Rectangle {
                    width: 1920,
                    height: 1080,
                },
                min: Rectangle {
                    width: 16,
                    height: 16,
                },
                max: Rectangle {
                    width: 8192,
                    height: 8192,
                },
            },
        ))),
    });
    properties.push(Property {
        key: spa::sys::SPA_FORMAT_VIDEO_framerate,
        flags: PropertyFlags::empty(),
        value: SpaValue::Fraction(Fraction { num: 0, denom: 1 }),
    });
    properties.push(Property {
        key: spa::sys::SPA_FORMAT_VIDEO_maxFramerate,
        flags: PropertyFlags::empty(),
        value: SpaValue::Choice(ChoiceValue::Fraction(Choice(
            ChoiceFlags::empty(),
            ChoiceEnum::Range {
                default: Fraction { num: 30, denom: 1 },
                min: Fraction { num: 1, denom: 1 },
                max: Fraction { num: 240, denom: 1 },
            },
        ))),
    });
    serialize_format_object(properties)
}

fn serialize_format_object(properties: Vec<Property>) -> Option<Vec<u8>> {
    assert!(properties.len() >= 3);
    let obj = Object {
        type_: SpaTypes::ObjectParamFormat.as_raw(),
        id: spa::param::ParamType::EnumFormat.as_raw(),
        properties,
    };
    let bytes = spa::pod::serialize::PodSerializer::serialize(
        std::io::Cursor::new(Vec::new()),
        &SpaValue::Object(obj),
    )
    .ok()?
    .0
    .into_inner();
    assert!(!bytes.is_empty());
    Some(bytes)
}

fn fixate_modifier_if_unfixated(stream: &pw::stream::Stream, param: &Pod) -> Option<u64> {
    let (chosen, fixated_bytes) = fixated_format_pod_bytes(param)?;
    let pod = Pod::from_bytes(&fixated_bytes)?;
    let mut params = [pod];
    if stream.update_params(&mut params).is_err() {
        return None;
    }
    Some(chosen)
}

fn fixated_format_pod_bytes(param: &Pod) -> Option<(u64, Vec<u8>)> {
    let (_, value) =
        spa::pod::deserialize::PodDeserializer::deserialize_any_from(param.as_bytes()).ok()?;
    let SpaValue::Object(mut obj) = value else {
        return None;
    };
    let mut chosen: Option<u64> = None;
    for prop in obj.properties.iter_mut() {
        if prop.key != spa::sys::SPA_FORMAT_VIDEO_modifier {
            continue;
        }
        let SpaValue::Choice(ChoiceValue::Long(choice)) = &prop.value else {
            continue;
        };
        let modifier = choose_dmabuf_modifier(choice);
        prop.flags = PropertyFlags::MANDATORY;
        prop.value = SpaValue::Long(modifier as i64);
        chosen = Some(modifier);
    }
    let chosen = chosen?;
    let bytes = spa::pod::serialize::PodSerializer::serialize(
        std::io::Cursor::new(Vec::new()),
        &SpaValue::Object(obj),
    )
    .ok()?
    .0
    .into_inner();
    assert!(!bytes.is_empty());
    Some((chosen, bytes))
}

fn choose_dmabuf_modifier(choice: &Choice<i64>) -> u64 {
    let candidates: Vec<i64> = match &choice.1 {
        ChoiceEnum::Enum {
            default,
            alternatives,
        } => {
            let mut all = Vec::with_capacity(MODIFIER_COUNT_MAX + 1);
            all.push(*default);
            for alternative in alternatives.iter().take(MODIFIER_COUNT_MAX) {
                all.push(*alternative);
            }
            all
        }
        ChoiceEnum::None(value) => vec![*value],
        ChoiceEnum::Range { default, .. } => vec![*default],
        ChoiceEnum::Step { default, .. } => vec![*default],
        ChoiceEnum::Flags { default, .. } => vec![*default],
    };
    assert!(!candidates.is_empty());
    assert!(candidates.len() <= MODIFIER_COUNT_MAX + 1);
    if candidates.contains(&(DRM_FORMAT_MOD_LINEAR as i64)) {
        return DRM_FORMAT_MOD_LINEAR;
    }
    if candidates.contains(&(DRM_FORMAT_MOD_INVALID as i64)) {
        return DRM_FORMAT_MOD_INVALID;
    }
    candidates[0] as u64
}

pub fn daemon_reachable() -> bool {
    pw::init();
    let Ok(mainloop) = MainLoopRc::new(None) else {
        return false;
    };
    let Ok(context) = ContextRc::new(&mainloop, None) else {
        return false;
    };
    context.connect_rc(None).is_ok()
}

fn dmabuf_frame_from_datas(
    datas: &[Data],
    width: u32,
    height: u32,
    pixel_format: VideoFormat,
    modifier: u64,
    timestamp_us: i64,
) -> Option<VideoFrame> {
    if width < 2 || height < 2 || width % 2 != 0 || height % 2 != 0 {
        return None;
    }
    let drm_format = drm_format_for_video_format(pixel_format)?;
    let mut plane_count = 0usize;
    let mut fds = [-1; 4];
    let mut strides = [0u32; 4];
    let mut offsets = [0u32; 4];
    for (idx, data) in datas.iter().enumerate().take(4) {
        if data.type_() != DataType::DmaBuf {
            break;
        }
        let fd = data.fd();
        if fd < 0 {
            return None;
        }
        let chunk = data.chunk();
        let stride = if chunk.stride() > 0 {
            chunk.stride() as u32
        } else {
            default_plane_stride(pixel_format, idx, width)?
        };
        fds[idx] = fd;
        strides[idx] = stride;
        offsets[idx] = chunk.offset();
        plane_count += 1;
    }
    if plane_count == 0 {
        return None;
    }
    Some(VideoFrame {
        width,
        height,
        stride_y: strides[0],
        stride_uv: if plane_count > 1 {
            strides[1]
        } else {
            strides[0]
        },
        timestamp_us,
        data: VideoFrameData::Empty,
        dmabuf: Some(DmabufFrameMetadata {
            fds,
            plane_count: plane_count as u32,
            drm_format,
            modifier,
            strides,
            offsets,
            device_uuid: None,
        }),
    })
}

fn drm_format_for_video_format(format: VideoFormat) -> Option<u32> {
    match format {
        VideoFormat::NV12 => Some(DRM_FORMAT_NV12),
        VideoFormat::BGRA => Some(DRM_FORMAT_ARGB8888),
        VideoFormat::BGRx => Some(DRM_FORMAT_XRGB8888),
        _ => None,
    }
}

fn default_plane_stride(format: VideoFormat, plane: usize, width: u32) -> Option<u32> {
    match format {
        VideoFormat::NV12 => Some(width),
        VideoFormat::BGRA | VideoFormat::BGRx if plane == 0 => width.checked_mul(4),
        _ => None,
    }
}

fn process_pipewire_buffer(stream: &pw::stream::Stream, user_data: &mut UserData) {
    assert!(user_data.pool.capacity() == LINUX_SCREEN_FRAME_POOL_CAP);
    let Some(mut buffer) = stream.dequeue_buffer() else {
        return;
    };
    let datas = buffer.datas_mut();
    if datas.is_empty() {
        return;
    }
    if let Some(video_frame) = dmabuf_frame_from_datas(
        datas,
        user_data.width,
        user_data.height,
        user_data.pixel_format,
        user_data.modifier,
        monotonic_us(),
    ) {
        (user_data.on_frame)(video_frame);
        return;
    }
    process_cpu_buffer(datas, user_data);
}

fn process_cpu_buffer(datas: &mut [Data], user_data: &mut UserData) {
    assert!(user_data.pool.capacity() == LINUX_SCREEN_FRAME_POOL_CAP);
    let data = &mut datas[0];
    let chunk = data.chunk();
    let chunk_size = chunk.size() as usize;
    let chunk_stride = chunk.stride() as usize;
    if chunk_size == 0 || user_data.width == 0 || user_data.height == 0 {
        return;
    }
    if user_data.height % 2 != 0 {
        return;
    }
    let Some(raw) = data.data() else { return };
    let layout = Nv12Layout {
        width: user_data.width,
        height: user_data.height,
        stride_y: user_data.width,
        stride_uv: user_data.width,
    };
    let Some(total_bytes) = layout.packed_size() else {
        return;
    };
    if total_bytes > user_data.pool.bytes_per_buffer() {
        user_data.pool.note_frame_dropped_oversized();
        return;
    }
    let timestamp_us = monotonic_us();
    let Ok(mut staging) = user_data.staging_free_rx.try_recv() else {
        note_convert_queue_drop(user_data);
        return;
    };
    let staged = stage_raw_frame(
        &mut staging,
        raw,
        layout,
        chunk_stride,
        user_data.pixel_format,
    );
    if !staged {
        return_staging_slot(user_data, staging);
        return;
    }
    staging.width = layout.width;
    staging.height = layout.height;
    staging.pixel_format = user_data.pixel_format;
    staging.timestamp_us = timestamp_us;
    submit_staged_frame(user_data, staging);
}

fn submit_staged_frame(user_data: &UserData, staging: StagingFrame) {
    assert!(staging.copied_len > 0);
    assert!(staging.copied_len <= staging.bytes.len());
    match user_data.staging_filled_tx.try_send(staging) {
        Ok(()) => {}
        Err(TrySendError::Full(frame)) => {
            return_staging_slot(user_data, frame);
            note_convert_queue_drop(user_data);
        }
        Err(TrySendError::Disconnected(frame)) => {
            return_staging_slot(user_data, frame);
            note_convert_queue_drop(user_data);
        }
    }
}

fn return_staging_slot(user_data: &UserData, staging: StagingFrame) {
    let returned = user_data.staging_free_tx.try_send(staging);
    assert!(returned.is_ok());
}

fn note_convert_queue_drop(user_data: &UserData) {
    let before = user_data
        .frames_dropped_convert_queue_full
        .fetch_add(1, Ordering::Relaxed);
    assert!(before < u64::MAX / 2);
}

fn stage_raw_frame(
    staging: &mut StagingFrame,
    raw: &[u8],
    layout: Nv12Layout,
    chunk_stride: usize,
    pixel_format: VideoFormat,
) -> bool {
    assert!(layout.width > 0);
    assert!(layout.height % 2 == 0);
    match pixel_format {
        VideoFormat::NV12 => stage_nv12_rows(staging, raw, layout, chunk_stride),
        VideoFormat::BGRA | VideoFormat::BGRx => {
            stage_bgra_rows(staging, raw, layout, chunk_stride)
        }
        _ => false,
    }
}

fn stage_bgra_rows(
    staging: &mut StagingFrame,
    raw: &[u8],
    layout: Nv12Layout,
    chunk_stride: usize,
) -> bool {
    let w = layout.width as usize;
    let h = layout.height as usize;
    let Some(row_bytes) = w.checked_mul(4) else {
        return false;
    };
    let src_stride = if chunk_stride == 0 {
        row_bytes
    } else {
        chunk_stride
    };
    if src_stride < row_bytes {
        return false;
    }
    let Some(src_needed) = src_stride.checked_mul(h) else {
        return false;
    };
    if raw.len() < src_needed {
        return false;
    }
    let Some(copied) = row_bytes.checked_mul(h) else {
        return false;
    };
    if copied > staging.bytes.len() {
        return false;
    }
    if src_stride == row_bytes {
        staging.bytes[..copied].copy_from_slice(&raw[..copied]);
    } else {
        for row in 0..h {
            let src_offset = row * src_stride;
            let dst_offset = row * row_bytes;
            staging.bytes[dst_offset..dst_offset + row_bytes]
                .copy_from_slice(&raw[src_offset..src_offset + row_bytes]);
        }
    }
    staging.copied_len = copied;
    true
}

fn stage_nv12_rows(
    staging: &mut StagingFrame,
    raw: &[u8],
    layout: Nv12Layout,
    chunk_stride: usize,
) -> bool {
    let h = layout.height as usize;
    let stride = if chunk_stride == 0 {
        layout.width as usize
    } else {
        chunk_stride
    };
    let Some(y_bytes) = stride.checked_mul(h) else {
        return false;
    };
    let Some(uv_bytes) = stride.checked_mul(h / 2) else {
        return false;
    };
    if raw.len() < y_bytes + uv_bytes {
        return false;
    }
    let Some(total) = layout.packed_size() else {
        return false;
    };
    if total > staging.bytes.len() {
        return false;
    }
    let strided = Nv12Layout {
        width: layout.width,
        height: layout.height,
        stride_y: stride as u32,
        stride_uv: stride as u32,
    };
    let ok = pack_nv12(
        strided,
        &raw[..y_bytes],
        &raw[y_bytes..y_bytes + uv_bytes],
        &mut staging.bytes[..total],
    );
    if !ok {
        return false;
    }
    staging.copied_len = total;
    true
}

fn acquire_or_drop(
    pool: &Arc<LinuxFrameBufferPool>,
    on_pool_exhausted: Option<&PoolExhaustionCallback>,
) -> Option<PooledFrameBuffer> {
    assert!(pool.capacity() == LINUX_SCREEN_FRAME_POOL_CAP);
    match pool.try_acquire() {
        Some(pooled) => Some(pooled),
        None => {
            let dropped = pool.frames_dropped_pool_exhausted();
            if let Some(cb) = on_pool_exhausted {
                cb(dropped);
            }
            None
        }
    }
}

fn fill_pool_buffer_from_raw(
    pooled: &mut PooledFrameBuffer,
    raw: &[u8],
    layout: Nv12Layout,
    chunk_stride: usize,
    pixel_format: VideoFormat,
    total_bytes: usize,
) -> bool {
    assert!(total_bytes <= pooled.buffer_mut().len());
    let dst = &mut pooled.buffer_mut()[..total_bytes];
    let ok = match pixel_format {
        VideoFormat::NV12 => fill_nv12_passthrough(dst, raw, layout, chunk_stride),
        VideoFormat::BGRA | VideoFormat::BGRx => fill_bgra_to_nv12(dst, raw, layout, chunk_stride),
        _ => false,
    };
    if !ok {
        return false;
    }
    pooled.set_len(total_bytes);
    true
}

fn fill_nv12_passthrough(
    dst: &mut [u8],
    raw: &[u8],
    layout: Nv12Layout,
    chunk_stride: usize,
) -> bool {
    let stride_y = if chunk_stride == 0 {
        layout.width as usize
    } else {
        chunk_stride
    };
    let stride_uv = stride_y;
    let y_bytes = stride_y * layout.height as usize;
    let uv_bytes = stride_uv * (layout.height as usize / 2);
    if raw.len() < y_bytes + uv_bytes {
        return false;
    }
    let strided_layout = Nv12Layout {
        width: layout.width,
        height: layout.height,
        stride_y: stride_y as u32,
        stride_uv: stride_uv as u32,
    };
    pack_nv12(
        strided_layout,
        &raw[..y_bytes],
        &raw[y_bytes..y_bytes + uv_bytes],
        dst,
    )
}

fn fill_bgra_to_nv12(dst: &mut [u8], raw: &[u8], layout: Nv12Layout, chunk_stride: usize) -> bool {
    let stride = if chunk_stride == 0 {
        layout.width as usize * 4
    } else {
        chunk_stride
    };
    let needed = stride * layout.height as usize;
    if raw.len() < needed {
        return false;
    }
    bgra_to_nv12(layout, &raw[..needed], stride as u32, dst, false)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn deserialize_format_object(bytes: &[u8]) -> Object {
        let (_, value) = spa::pod::deserialize::PodDeserializer::deserialize_any_from(bytes)
            .expect("serialized pod deserializes");
        let SpaValue::Object(obj) = value else {
            panic!("format pod must be an object");
        };
        obj
    }

    fn modifier_property_of(obj: &Object) -> Option<&Property> {
        obj.properties
            .iter()
            .find(|prop| prop.key == spa::sys::SPA_FORMAT_VIDEO_modifier)
    }

    #[test]
    fn format_pod_without_modifiers_matches_legacy_shape() {
        let bytes =
            serialize_video_format_pod(VideoFormat::BGRA, None).expect("legacy pod serializes");
        let obj = deserialize_format_object(&bytes);
        assert!(modifier_property_of(&obj).is_none());
        assert_eq!(obj.properties.len(), 6);
    }

    #[test]
    fn format_pod_with_modifiers_has_mandatory_dont_fixate_choice() {
        let bytes = serialize_video_format_pod(VideoFormat::BGRA, Some(&DMABUF_MODIFIERS_BASELINE))
            .expect("modifier pod serializes");
        let obj = deserialize_format_object(&bytes);
        let prop = modifier_property_of(&obj).expect("modifier property present");
        assert!(prop.flags.contains(PropertyFlags::MANDATORY));
        assert!(prop.flags.contains(PropertyFlags::DONT_FIXATE));
        let SpaValue::Choice(ChoiceValue::Long(choice)) = &prop.value else {
            panic!("modifier must be a long choice");
        };
        let ChoiceEnum::Enum {
            default,
            alternatives,
        } = &choice.1
        else {
            panic!("modifier choice must be an enum");
        };
        assert_eq!(*default, DRM_FORMAT_MOD_LINEAR as i64);
        assert!(alternatives.contains(&(DRM_FORMAT_MOD_LINEAR as i64)));
        assert!(alternatives.contains(&(DRM_FORMAT_MOD_INVALID as i64)));
    }

    #[test]
    fn build_format_params_always_keeps_modifier_less_fallback_pods() {
        let params = build_format_params().expect("params build");
        let mut plain_pods = 0usize;
        for param in &params {
            let obj = deserialize_format_object(&param.0);
            if modifier_property_of(&obj).is_none() {
                plain_pods += 1;
            }
        }
        assert_eq!(plain_pods, 3);
        assert!(params.len() == 3 || params.len() == 6);
    }

    #[test]
    fn fixation_picks_linear_and_strips_the_choice() {
        let bytes = serialize_video_format_pod(VideoFormat::BGRA, Some(&DMABUF_MODIFIERS_BASELINE))
            .expect("modifier pod serializes");
        let pod = Pod::from_bytes(&bytes).expect("pod parses");
        let (chosen, fixated) = fixated_format_pod_bytes(pod).expect("fixation happens");
        assert_eq!(chosen, DRM_FORMAT_MOD_LINEAR);
        let obj = deserialize_format_object(&fixated);
        let prop = modifier_property_of(&obj).expect("modifier property kept");
        assert!(prop.flags.contains(PropertyFlags::MANDATORY));
        assert!(!prop.flags.contains(PropertyFlags::DONT_FIXATE));
        assert_eq!(prop.value, SpaValue::Long(DRM_FORMAT_MOD_LINEAR as i64));
    }

    #[test]
    fn fixation_skips_already_fixated_pods() {
        let bytes =
            serialize_video_format_pod(VideoFormat::BGRA, None).expect("legacy pod serializes");
        let pod = Pod::from_bytes(&bytes).expect("pod parses");
        assert!(fixated_format_pod_bytes(pod).is_none());
    }

    #[test]
    fn modifier_choice_prefers_linear_then_invalid_then_first() {
        let linear = Choice(
            ChoiceFlags::empty(),
            ChoiceEnum::Enum {
                default: DRM_FORMAT_MOD_INVALID as i64,
                alternatives: vec![DRM_FORMAT_MOD_INVALID as i64, DRM_FORMAT_MOD_LINEAR as i64],
            },
        );
        assert_eq!(choose_dmabuf_modifier(&linear), DRM_FORMAT_MOD_LINEAR);

        let invalid_only = Choice(
            ChoiceFlags::empty(),
            ChoiceEnum::Enum {
                default: DRM_FORMAT_MOD_INVALID as i64,
                alternatives: vec![DRM_FORMAT_MOD_INVALID as i64],
            },
        );
        assert_eq!(
            choose_dmabuf_modifier(&invalid_only),
            DRM_FORMAT_MOD_INVALID
        );

        let vendor = Choice(
            ChoiceFlags::empty(),
            ChoiceEnum::Enum {
                default: 0x0100_0000_0000_0001,
                alternatives: vec![0x0100_0000_0000_0001, 0x0100_0000_0000_0002],
            },
        );
        assert_eq!(choose_dmabuf_modifier(&vendor), 0x0100_0000_0000_0001);
    }

    #[test]
    fn dmabuf_env_values_gate_modifier_advertisement() {
        assert!(dmabuf_env_value_disables("off"));
        assert!(dmabuf_env_value_disables(" OFF "));
        assert!(dmabuf_env_value_disables("0"));
        assert!(dmabuf_env_value_disables("false"));
        assert!(dmabuf_env_value_disables("no"));
        assert!(dmabuf_env_value_disables("disabled"));
        assert!(!dmabuf_env_value_disables(""));
        assert!(!dmabuf_env_value_disables("on"));
        assert!(!dmabuf_env_value_disables("auto"));
    }

    #[test]
    fn staging_channels_hold_exactly_two_preallocated_slots() {
        let pool = LinuxFrameBufferPool::new(64 * 64 * 3 / 2).expect("pool init");
        let (rt, _worker) = build_staging_channels(&pool);
        let first = rt.free_rx.try_recv().expect("first staging slot");
        let second = rt.free_rx.try_recv().expect("second staging slot");
        assert!(rt.free_rx.try_recv().is_err());
        assert_eq!(first.bytes.len(), 64 * 64 * 4);
        assert_eq!(second.bytes.len(), 64 * 64 * 4);
        assert_eq!(first.copied_len, 0);
        assert_eq!(second.copied_len, 0);
    }

    #[test]
    fn stage_bgra_rows_strips_stride_padding() {
        let layout = Nv12Layout {
            width: 4,
            height: 2,
            stride_y: 4,
            stride_uv: 4,
        };
        let stride = 24usize;
        let mut raw = vec![0u8; stride * 2];
        for row in 0..2usize {
            for byte in 0..16usize {
                raw[row * stride + byte] = (row * 16 + byte) as u8;
            }
        }
        let mut staging = StagingFrame::new(4 * 2 * 4);
        assert!(stage_bgra_rows(&mut staging, &raw, layout, stride));
        assert_eq!(staging.copied_len, 32);
        for row in 0..2usize {
            for byte in 0..16usize {
                assert_eq!(staging.bytes[row * 16 + byte], (row * 16 + byte) as u8);
            }
        }
    }

    #[test]
    fn stage_bgra_rows_rejects_short_source() {
        let layout = Nv12Layout {
            width: 4,
            height: 2,
            stride_y: 4,
            stride_uv: 4,
        };
        let raw = vec![0u8; 8];
        let mut staging = StagingFrame::new(4 * 2 * 4);
        assert!(!stage_bgra_rows(&mut staging, &raw, layout, 16));
        assert_eq!(staging.copied_len, 0);
    }

    #[test]
    fn staged_bgra_conversion_matches_direct_conversion() {
        let layout = Nv12Layout {
            width: 16,
            height: 4,
            stride_y: 16,
            stride_uv: 16,
        };
        let stride = 80usize;
        let mut raw = vec![0u8; stride * 4];
        for (index, byte) in raw.iter_mut().enumerate() {
            *byte = (index % 251) as u8;
        }

        let mut staging = StagingFrame::new(16 * 4 * 4);
        assert!(stage_raw_frame(
            &mut staging,
            &raw,
            layout,
            stride,
            VideoFormat::BGRA
        ));
        staging.width = layout.width;
        staging.height = layout.height;
        staging.pixel_format = VideoFormat::BGRA;
        staging.timestamp_us = 1_234;

        type ObservedFrame = Arc<Mutex<Option<(Vec<u8>, i64)>>>;
        let pool = LinuxFrameBufferPool::new(layout.packed_size().expect("layout")).expect("pool");
        let observed: ObservedFrame = Arc::new(Mutex::new(None));
        let observed_cb = Arc::clone(&observed);
        let on_frame: FrameCallback = Arc::new(move |frame| {
            let mut guard = observed_cb.lock().expect("observed lock");
            *guard = Some((frame.data.to_vec(), frame.timestamp_us));
        });
        convert_staged_frame(&staging, &pool, &on_frame, None);

        let total = layout.packed_size().expect("layout");
        let mut expected = vec![0u8; total];
        assert!(fill_bgra_to_nv12(&mut expected, &raw, layout, stride));

        let guard = observed.lock().expect("observed lock");
        let (frame_bytes, timestamp_us) = guard.as_ref().expect("frame delivered");
        assert_eq!(frame_bytes, &expected);
        assert_eq!(*timestamp_us, 1_234);
    }

    #[test]
    fn convert_worker_returns_slot_after_processing() {
        let pool = LinuxFrameBufferPool::new(64 * 64 * 3 / 2).expect("pool init");
        let (rt, worker) = build_staging_channels(&pool);
        let on_frame: FrameCallback = Arc::new(|_| {});
        let worker_pool = Arc::clone(&pool);
        let handle = thread::Builder::new()
            .name("fluxer-test-convert".to_string())
            .spawn(move || {
                run_convert_worker(worker, worker_pool, on_frame, None);
            })
            .expect("worker spawns");

        let mut staging = rt.free_rx.recv().expect("slot available");
        let layout = Nv12Layout {
            width: 16,
            height: 4,
            stride_y: 16,
            stride_uv: 16,
        };
        let raw = vec![0x40u8; 16 * 4 * 4];
        assert!(stage_raw_frame(
            &mut staging,
            &raw,
            layout,
            0,
            VideoFormat::BGRx
        ));
        staging.width = layout.width;
        staging.height = layout.height;
        staging.pixel_format = VideoFormat::BGRx;
        staging.timestamp_us = 7;
        rt.filled_tx.send(staging).expect("filled enqueue");

        let first_free = rt
            .free_rx
            .recv_timeout(Duration::from_millis(2_000))
            .expect("one free slot available");
        assert_eq!(first_free.copied_len, 0);
        let recycled = rt
            .free_rx
            .recv_timeout(Duration::from_millis(2_000))
            .expect("slot recycled by worker");
        assert_eq!(recycled.copied_len, 0);

        drop(rt);
        handle.join().expect("worker exits on disconnect");
    }
}
