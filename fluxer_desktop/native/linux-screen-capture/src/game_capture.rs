// SPDX-License-Identifier: AGPL-3.0-or-later

use std::env;
use std::ffi::c_void;
use std::mem::{MaybeUninit, size_of};
use std::os::fd::RawFd;
use std::ptr;
use std::slice;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use crate::nv12_packing::{Nv12Layout, bgra_to_nv12};
use crate::pipewire_stream::{
    DmabufFrameMetadata, FrameCallback, LifecycleCallback, LinuxFrameBufferPool, VideoFrame,
    VideoFrameData,
};

const VKCAPTURE_SOCKET_NAME: &[u8] = b"/com/obsproject/vkcapture";
const READY_TIMEOUT: Duration = Duration::from_millis(2_000);
const POLL_TIMEOUT_MS: i32 = 10;
const FRAME_ADVANCE_COUNT_MAX: u32 = 1 << 20;
const CLIENT_DATA_TYPE: u8 = 10;
const CLIENT_DATA_SIZE: usize = 128;
const TEXTURE_DATA_TYPE: u8 = 11;
const TEXTURE_DATA_SIZE: usize = 128;
const CONTROL_DATA_SIZE: usize = 32;
const MAX_FPS: u32 = 240;
const IMPORT_MODE_ENV: &str = "FLUXER_LINUX_GAME_CAPTURE_IMPORT_MODE";
const OBS_IMPORT_MODE_NAMES: [&str; 4] = [
    "default-dmabuf",
    "no-modifiers-dmabuf",
    "linear-dmabuf",
    "linear-host-mapped-dmabuf",
];

const DRM_FORMAT_XRGB8888: i32 = fourcc(*b"XR24") as i32;
const DRM_FORMAT_ARGB8888: i32 = fourcc(*b"AR24") as i32;
const DRM_FORMAT_XBGR8888: i32 = fourcc(*b"XB24") as i32;
const DRM_FORMAT_ABGR8888: i32 = fourcc(*b"AB24") as i32;
const DRM_FORMAT_XRGB2101010: i32 = fourcc(*b"XR30") as i32;
const DRM_FORMAT_ARGB2101010: i32 = fourcc(*b"AR30") as i32;
const DRM_FORMAT_XBGR2101010: i32 = fourcc(*b"XB30") as i32;
const DRM_FORMAT_ABGR2101010: i32 = fourcc(*b"AB30") as i32;
const DRM_FORMAT_XBGR16161616: i32 = fourcc(*b"XB48") as i32;
const DRM_FORMAT_ABGR16161616: i32 = fourcc(*b"AB48") as i32;
const DRM_FORMAT_XBGR16161616F: i32 = fourcc(*b"XB4H") as i32;
const DRM_FORMAT_ABGR16161616F: i32 = fourcc(*b"AB4H") as i32;
const DRM_FORMAT_NV12: i32 = fourcc(*b"NV12") as i32;
const DRM_FORMAT_MOD_INVALID: u64 = (1u64 << 56) - 1;
const SUPPORTED_HOST_MAPPED_FORMATS: [i32; 12] = [
    DRM_FORMAT_XRGB8888,
    DRM_FORMAT_ARGB8888,
    DRM_FORMAT_XBGR8888,
    DRM_FORMAT_ABGR8888,
    DRM_FORMAT_XRGB2101010,
    DRM_FORMAT_ARGB2101010,
    DRM_FORMAT_XBGR2101010,
    DRM_FORMAT_ABGR2101010,
    DRM_FORMAT_XBGR16161616,
    DRM_FORMAT_ABGR16161616,
    DRM_FORMAT_XBGR16161616F,
    DRM_FORMAT_ABGR16161616F,
];

const VK_COLOR_SPACE_HDR10_ST2084_EXT: u32 = 1_000_104_008;

const DMA_BUF_SYNC_READ: u64 = 1 << 0;
const DMA_BUF_SYNC_END: u64 = 1 << 2;
const DMA_BUF_IOCTL_SYNC: libc::c_ulong =
    (1u64 << 30 | (b'b' as u64) << 8 | 8u64 << 16) as libc::c_ulong;

const fn fourcc(bytes: [u8; 4]) -> u32 {
    bytes[0] as u32 | (bytes[1] as u32) << 8 | (bytes[2] as u32) << 16 | (bytes[3] as u32) << 24
}

#[derive(Debug)]
pub enum GameCaptureError {
    Bind,
    Listen,
    Spawn,
}

impl std::fmt::Display for GameCaptureError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bind => f.write_str("Bind"),
            Self::Listen => f.write_str("Listen"),
            Self::Spawn => f.write_str("Spawn"),
        }
    }
}

pub struct GameCaptureVideoStream {
    running: Arc<AtomicBool>,
    thread: std::sync::Mutex<Option<JoinHandle<()>>>,
    diagnostics: Arc<Mutex<GameCaptureDiagnostics>>,
}

impl GameCaptureVideoStream {
    pub fn open(
        frame_rate: u32,
        on_frame: FrameCallback,
        on_lifecycle: LifecycleCallback,
    ) -> Result<Self, GameCaptureError> {
        let running = Arc::new(AtomicBool::new(true));
        let import_state = GameCaptureImportState::from_env();
        let diagnostics = Arc::new(Mutex::new(GameCaptureDiagnostics::new_with_import_state(
            frame_rate,
            import_state,
        )));
        let (ready_tx, ready_rx) = std::sync::mpsc::sync_channel::<Result<(), GameCaptureError>>(1);
        let running_thread = running.clone();
        let diagnostics_thread = diagnostics.clone();
        let handle = thread::Builder::new()
            .name("fluxer-linux-game-capture-vkcapture".to_string())
            .spawn(move || {
                run_server(
                    frame_rate,
                    import_state,
                    running_thread,
                    diagnostics_thread,
                    ready_tx,
                    on_frame,
                    on_lifecycle,
                );
            })
            .map_err(|_| GameCaptureError::Spawn)?;
        match ready_rx.recv_timeout(READY_TIMEOUT) {
            Ok(Ok(())) => Ok(Self {
                running,
                thread: std::sync::Mutex::new(Some(handle)),
                diagnostics,
            }),
            Ok(Err(err)) => {
                running.store(false, Ordering::Release);
                let _ = handle.join();
                Err(err)
            }
            Err(_) => {
                running.store(false, Ordering::Release);
                let _ = handle.join();
                Err(GameCaptureError::Bind)
            }
        }
    }

    pub fn stop(&self) {
        self.running.store(false, Ordering::Release);
    }

    pub fn diagnostics(&self) -> GameCaptureDiagnostics {
        self.diagnostics
            .lock()
            .map(|state| state.clone())
            .unwrap_or_else(|poisoned| poisoned.into_inner().clone())
    }
}

impl Drop for GameCaptureVideoStream {
    fn drop(&mut self) {
        self.stop();
        if let Ok(mut guard) = self.thread.lock()
            && let Some(handle) = guard.take()
        {
            let _ = handle.join();
        }
    }
}

#[derive(Debug, Clone)]
pub struct GameCaptureDiagnostics {
    pub backend: String,
    pub active_strategy: String,
    pub import_mode: String,
    pub map_host: bool,
    pub linear: bool,
    pub client_connected: bool,
    pub connected_client: Option<String>,
    pub connected_pid: Option<i32>,
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub texture_format: Option<String>,
    pub texture_modifier: Option<String>,
    pub frame_counter: u64,
    pub dropped_frame_counter: u64,
    pub lagged_frame_counter: u64,
    pub unsupported_frame_counter: u64,
    pub last_present_timestamp_us: Option<i64>,
    pub last_diagnostic: Option<String>,
    pub last_addon_error: Option<String>,
    pub requested_injection_method: String,
    pub injection_method: String,
    pub requested_import_mode: String,
    pub last_fallback_reason: Option<String>,
    pub frame_transport: String,
    pub host_mapped_cpu_fallback: bool,
    pub source_dmabuf_metadata_available: bool,
    pub no_modifiers: bool,
    pub zero_copy: bool,
    pub gpu_import_available: bool,
    pub device_uuid_advertised: bool,
    pub supported_import_modes: Vec<String>,
}

impl GameCaptureDiagnostics {
    fn new(_frame_rate: u32) -> Self {
        Self::new_with_import_state(_frame_rate, GameCaptureImportState::from_env())
    }

    fn new_with_import_state(_frame_rate: u32, import_state: GameCaptureImportState) -> Self {
        Self {
            backend: "obs-vkcapture".to_string(),
            active_strategy: import_state.active_strategy().to_string(),
            import_mode: import_state.current.as_str().to_string(),
            map_host: import_state.current.map_host(),
            linear: import_state.current.linear(),
            client_connected: false,
            connected_client: None,
            connected_pid: None,
            width: None,
            height: None,
            texture_format: None,
            texture_modifier: None,
            frame_counter: 0,
            dropped_frame_counter: 0,
            lagged_frame_counter: 0,
            unsupported_frame_counter: 0,
            last_present_timestamp_us: None,
            last_diagnostic: None,
            last_addon_error: None,
            requested_injection_method: "obs-vkcapture".to_string(),
            injection_method: "obs-vkcapture".to_string(),
            requested_import_mode: import_state.requested_label().to_string(),
            last_fallback_reason: Some(import_state.initial_reason()),
            frame_transport: import_state.current.frame_transport().to_string(),
            host_mapped_cpu_fallback: import_state.current.map_host(),
            source_dmabuf_metadata_available: import_state
                .current
                .source_dmabuf_metadata_available(),
            no_modifiers: import_state.current.no_modifiers(),
            zero_copy: import_state.current.zero_copy(),
            gpu_import_available: import_state.current.zero_copy(),
            device_uuid_advertised: false,
            supported_import_modes: OBS_IMPORT_MODE_NAMES
                .iter()
                .map(|mode| (*mode).to_string())
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GameCaptureImportMode {
    Default,
    NoModifiers,
    Linear,
    LinearHostMapped,
}

impl GameCaptureImportMode {
    fn as_str(self) -> &'static str {
        match self {
            Self::Default => "default-dmabuf",
            Self::NoModifiers => "no-modifiers-dmabuf",
            Self::Linear => "linear-dmabuf",
            Self::LinearHostMapped => "linear-host-mapped-dmabuf",
        }
    }

    fn no_modifiers(self) -> bool {
        matches!(self, Self::NoModifiers)
    }

    fn linear(self) -> bool {
        matches!(self, Self::Linear | Self::LinearHostMapped)
    }

    fn map_host(self) -> bool {
        matches!(self, Self::LinearHostMapped)
    }

    fn next_fallback(self) -> Option<Self> {
        match self {
            Self::Default => Some(Self::NoModifiers),
            Self::NoModifiers => Some(Self::Linear),
            Self::Linear => Some(Self::LinearHostMapped),
            Self::LinearHostMapped => None,
        }
    }

    fn frame_transport(self) -> &'static str {
        if self.map_host() {
            "host-mapped-cpu-nv12-with-source-dmabuf"
        } else {
            "gpu-dmabuf-zero-copy"
        }
    }

    fn source_dmabuf_metadata_available(self) -> bool {
        true
    }

    fn zero_copy(self) -> bool {
        !self.map_host()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GameCaptureImportPolicy {
    AutoObsFallback,
    Fixed(GameCaptureImportMode),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct GameCaptureImportState {
    policy: GameCaptureImportPolicy,
    current: GameCaptureImportMode,
    invalid_env_value: Option<&'static str>,
}

impl GameCaptureImportState {
    fn from_env() -> Self {
        match env::var(IMPORT_MODE_ENV) {
            Ok(value) => Self::from_env_value(value.as_str()),
            Err(_) => Self::auto(),
        }
    }

    fn from_env_value(value: &str) -> Self {
        let normalized = value.trim().to_ascii_lowercase();
        match normalized.as_str() {
            "" | "auto" | "fallback" | "obs-fallback" => Self::auto(),
            "default" | "default-dmabuf" | "gpu" | "gpu-default" | "zero-copy" => {
                Self::fixed(GameCaptureImportMode::Default)
            }
            "no-modifiers" | "no-modifiers-dmabuf" | "gpu-no-modifiers" => {
                Self::fixed(GameCaptureImportMode::NoModifiers)
            }
            "linear" | "linear-dmabuf" | "gpu-linear" => Self::fixed(GameCaptureImportMode::Linear),
            "host-mapped" | "linear-host-mapped" | "linear-host-mapped-dmabuf" | "cpu" | "safe" => {
                Self::fixed(GameCaptureImportMode::LinearHostMapped)
            }
            _ => Self {
                invalid_env_value: Some("invalid"),
                ..Self::auto()
            },
        }
    }

    fn auto() -> Self {
        Self {
            policy: GameCaptureImportPolicy::AutoObsFallback,
            current: GameCaptureImportMode::Default,
            invalid_env_value: None,
        }
    }

    fn fixed(current: GameCaptureImportMode) -> Self {
        Self {
            policy: GameCaptureImportPolicy::Fixed(current),
            current,
            invalid_env_value: None,
        }
    }

    fn requested_label(self) -> &'static str {
        match self.policy {
            GameCaptureImportPolicy::AutoObsFallback => "auto-obs-fallback",
            GameCaptureImportPolicy::Fixed(mode) => mode.as_str(),
        }
    }

    fn active_strategy(self) -> &'static str {
        match self.policy {
            GameCaptureImportPolicy::AutoObsFallback => "game-hook-auto-fallback",
            GameCaptureImportPolicy::Fixed(GameCaptureImportMode::LinearHostMapped) => {
                "game-hook-host-mapped"
            }
            GameCaptureImportPolicy::Fixed(_) => "game-hook-gpu-import-requested",
        }
    }

    fn initial_reason(self) -> String {
        if self.invalid_env_value.is_some() {
            return format!(
                "{IMPORT_MODE_ENV} was invalid; using OBS import fallback ladder starting at default DMABUF"
            );
        }
        match self.policy {
            GameCaptureImportPolicy::AutoObsFallback => {
                "OBS import ladder starts at default DMABUF zero-copy and falls back only if native GPU import cannot consume the advertised texture".to_string()
            }
            GameCaptureImportPolicy::Fixed(GameCaptureImportMode::LinearHostMapped) => {
                "Fluxer requests OBS linear host-mapped capture for CPU NV12 conversion".to_string()
            }
            GameCaptureImportPolicy::Fixed(mode) => format!(
                "Fluxer requests OBS {} capture for Linux native GPU import",
                mode.as_str()
            ),
        }
    }

    fn advance_after_import_failure(
        &mut self,
    ) -> Option<(GameCaptureImportMode, GameCaptureImportMode)> {
        if !matches!(self.policy, GameCaptureImportPolicy::AutoObsFallback) {
            return None;
        }
        let from = self.current;
        let to = from.next_fallback()?;
        self.current = to;
        Some((from, to))
    }
}

type SharedDiagnostics = Arc<Mutex<GameCaptureDiagnostics>>;

#[derive(Debug)]
struct Client {
    fd: RawFd,
    exe: String,
    pid: Option<i32>,
}

impl Drop for Client {
    fn drop(&mut self) {
        close_fd(self.fd);
    }
}

struct MappedTexture {
    fds: [RawFd; 4],
    nfd: usize,
    width: u32,
    height: u32,
    format: i32,
    strides: [u32; 4],
    offsets: [u32; 4],
    stride: usize,
    offset: usize,
    modifier: u64,
    flip: bool,
    color_space: u32,
    zero_copy: bool,
    ptr: *mut c_void,
    map_size: usize,
    frame_pool: Option<Arc<LinuxFrameBufferPool>>,
    bgra_scratch: Vec<u8>,
}

impl Drop for MappedTexture {
    fn drop(&mut self) {
        if !self.ptr.is_null() && self.map_size > 0 {
            unsafe {
                libc::munmap(self.ptr, self.map_size);
            }
        }
        for fd in self.fds.iter_mut().take(self.nfd) {
            if *fd >= 0 {
                close_fd(*fd);
                *fd = -1;
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct TextureData {
    nfd: usize,
    width: u32,
    height: u32,
    format: i32,
    strides: [i32; 4],
    offsets: [i32; 4],
    modifier: u64,
    flip: bool,
    color_space: u32,
}

#[derive(Debug, Clone, Copy)]
struct HostMappedLayout {
    stride: usize,
    offset: usize,
    strides: [u32; 4],
    offsets: [u32; 4],
    width: u32,
    height: u32,
}

fn run_server(
    frame_rate: u32,
    mut import_state: GameCaptureImportState,
    running: Arc<AtomicBool>,
    diagnostics: SharedDiagnostics,
    ready_tx: std::sync::mpsc::SyncSender<Result<(), GameCaptureError>>,
    on_frame: FrameCallback,
    on_lifecycle: LifecycleCallback,
) {
    let listener = match bind_listener() {
        Ok(fd) => fd,
        Err(err) => {
            let _ = ready_tx.send(Err(err));
            return;
        }
    };
    let _listener_guard = FdGuard(listener);
    let _ = ready_tx.send(Ok(()));

    let interval = Duration::from_secs_f64(1.0 / frame_rate.clamp(1, MAX_FPS) as f64);
    let mut next_frame_at = Instant::now();
    let mut client: Option<Client> = None;
    let mut texture: Option<MappedTexture> = None;

    while running.load(Ordering::Acquire) {
        let timeout = next_poll_timeout(next_frame_at);
        let mut pollfds = [
            libc::pollfd {
                fd: listener,
                events: libc::POLLIN,
                revents: 0,
            },
            libc::pollfd {
                fd: client.as_ref().map(|c| c.fd).unwrap_or(-1),
                events: libc::POLLIN | libc::POLLHUP | libc::POLLERR,
                revents: 0,
            },
        ];
        let nfds = if client.is_some() { 2 } else { 1 };
        let poll_result = unsafe { libc::poll(pollfds.as_mut_ptr(), nfds, timeout) };
        if poll_result < 0 {
            let errno = last_errno();
            if errno != libc::EINTR {
                publish_lifecycle(
                    &diagnostics,
                    &on_lifecycle,
                    "error",
                    &format!("obs-vkcapture poll failed: errno {errno}"),
                );
                break;
            }
        }

        if poll_result > 0 && pollfds[0].revents & libc::POLLIN != 0 {
            match accept_client(listener) {
                Ok(next_client) => {
                    let exe = next_client.exe.clone();
                    update_diagnostics(&diagnostics, |state| {
                        state.client_connected = true;
                        state.connected_client = Some(exe.clone());
                        state.connected_pid = next_client.pid;
                        state.width = None;
                        state.height = None;
                        state.texture_format = None;
                        state.texture_modifier = None;
                    });
                    client = Some(next_client);
                    texture = None;
                    sync_import_diagnostics(&diagnostics, import_state, None);
                    if let Some(active) = client.as_ref() {
                        let _ = write_control(active.fd, true, import_state.current);
                    }
                    publish_lifecycle(
                        &diagnostics,
                        &on_lifecycle,
                        "diagnostic",
                        &format!("obs-vkcapture client connected: {exe}"),
                    );
                }
                Err(message) => {
                    publish_lifecycle(&diagnostics, &on_lifecycle, "diagnostic", &message)
                }
            }
        }

        if let Some(active) = client.as_ref() {
            let revents = pollfds[1].revents;
            if revents & (libc::POLLHUP | libc::POLLERR) != 0 {
                clear_connected_client(&diagnostics);
                publish_lifecycle(
                    &diagnostics,
                    &on_lifecycle,
                    "diagnostic",
                    "obs-vkcapture client disconnected",
                );
                client = None;
                texture = None;
            } else if revents & libc::POLLIN != 0 {
                loop {
                    match recv_client_message(active.fd) {
                        RecvMessage::ClientName(exe) => {
                            update_diagnostics(&diagnostics, |state| {
                                state.connected_client = Some(exe.clone());
                            });
                            publish_lifecycle(
                                &diagnostics,
                                &on_lifecycle,
                                "diagnostic",
                                &format!("obs-vkcapture client identified: {exe}"),
                            );
                            sync_import_diagnostics(&diagnostics, import_state, None);
                            let _ = write_control(active.fd, true, import_state.current);
                        }
                        RecvMessage::Texture(data, fds) => {
                            match map_texture(data, fds, import_state.current, &on_lifecycle) {
                                Some(mapped) => {
                                    update_diagnostics(&diagnostics, |state| {
                                        state.width = Some(mapped.width);
                                        state.height = Some(mapped.height);
                                        state.texture_format =
                                            Some(fourcc_to_string(mapped.format));
                                        state.texture_modifier =
                                            Some(format_modifier(mapped.modifier));
                                    });
                                    publish_lifecycle(
                                        &diagnostics,
                                        &on_lifecycle,
                                        "diagnostic",
                                        &format!(
                                            "obs-vkcapture texture mapped: {}x{} fourcc={} modifier={}",
                                            mapped.width,
                                            mapped.height,
                                            fourcc_to_string(mapped.format),
                                            mapped.modifier
                                        ),
                                    );
                                    texture = Some(mapped);
                                }
                                None => {
                                    update_diagnostics(&diagnostics, |state| {
                                        state.unsupported_frame_counter =
                                            state.unsupported_frame_counter.saturating_add(1);
                                    });
                                    close_fds(fds);
                                    if let Some((from, to)) =
                                        import_state.advance_after_import_failure()
                                    {
                                        let reason = format!(
                                            "OBS import mode {} was not usable in this Fluxer backend; requesting {}",
                                            from.as_str(),
                                            to.as_str()
                                        );
                                        sync_import_diagnostics(
                                            &diagnostics,
                                            import_state,
                                            Some(reason.clone()),
                                        );
                                        publish_lifecycle(
                                            &diagnostics,
                                            &on_lifecycle,
                                            "diagnostic",
                                            &reason,
                                        );
                                    } else {
                                        sync_import_diagnostics(&diagnostics, import_state, None);
                                    }
                                    let _ = write_control(active.fd, true, import_state.current);
                                }
                            }
                        }
                        RecvMessage::WouldBlock => break,
                        RecvMessage::Closed => {
                            clear_connected_client(&diagnostics);
                            publish_lifecycle(
                                &diagnostics,
                                &on_lifecycle,
                                "diagnostic",
                                "obs-vkcapture client closed",
                            );
                            client = None;
                            texture = None;
                            break;
                        }
                        RecvMessage::Invalid(message) => {
                            publish_lifecycle(&diagnostics, &on_lifecycle, "diagnostic", &message);
                            break;
                        }
                    }
                }
            }
        }

        let now = Instant::now();
        if now >= next_frame_at {
            if let Some(mapped) = texture.as_mut() {
                match mapped.read_frame() {
                    Some(frame) => {
                        update_diagnostics(&diagnostics, |state| {
                            state.frame_counter = state.frame_counter.saturating_add(1);
                            state.last_present_timestamp_us = Some(frame.timestamp_us);
                        });
                        on_frame(frame);
                    }
                    None => {
                        update_diagnostics(&diagnostics, |state| {
                            state.dropped_frame_counter =
                                state.dropped_frame_counter.saturating_add(1);
                        });
                    }
                }
            }
            let (deadline, lagged_frames) = advance_frame_deadline(next_frame_at, now, interval);
            next_frame_at = deadline;
            if lagged_frames > 0 {
                if texture.is_some() {
                    update_diagnostics(&diagnostics, |state| {
                        state.lagged_frame_counter = state
                            .lagged_frame_counter
                            .saturating_add(lagged_frames as u64);
                    });
                }
            }
        }
    }

    publish_lifecycle(&diagnostics, &on_lifecycle, "closed", "");
}

fn publish_lifecycle(
    diagnostics: &SharedDiagnostics,
    on_lifecycle: &LifecycleCallback,
    kind: &str,
    message: &str,
) {
    update_diagnostics(diagnostics, |state| match kind {
        "error" => state.last_addon_error = Some(message.to_string()),
        "diagnostic" | "stalled" => state.last_diagnostic = Some(message.to_string()),
        _ => {}
    });
    on_lifecycle(kind, message);
}

fn update_diagnostics(
    diagnostics: &SharedDiagnostics,
    update: impl FnOnce(&mut GameCaptureDiagnostics),
) {
    let mut state = diagnostics
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    update(&mut state);
}

fn clear_connected_client(diagnostics: &SharedDiagnostics) {
    update_diagnostics(diagnostics, |state| {
        state.client_connected = false;
        state.connected_client = None;
        state.connected_pid = None;
        state.width = None;
        state.height = None;
        state.texture_format = None;
        state.texture_modifier = None;
    });
}

fn sync_import_diagnostics(
    diagnostics: &SharedDiagnostics,
    import_state: GameCaptureImportState,
    fallback_reason: Option<String>,
) {
    update_diagnostics(diagnostics, |state| {
        state.active_strategy = import_state.active_strategy().to_string();
        state.import_mode = import_state.current.as_str().to_string();
        state.requested_import_mode = import_state.requested_label().to_string();
        state.no_modifiers = import_state.current.no_modifiers();
        state.linear = import_state.current.linear();
        state.map_host = import_state.current.map_host();
        state.frame_transport = import_state.current.frame_transport().to_string();
        state.host_mapped_cpu_fallback = import_state.current.map_host();
        state.source_dmabuf_metadata_available =
            import_state.current.source_dmabuf_metadata_available();
        state.zero_copy = import_state.current.zero_copy();
        state.gpu_import_available = import_state.current.zero_copy();
        state.device_uuid_advertised = false;
        if let Some(reason) = fallback_reason {
            state.last_fallback_reason = Some(reason);
        }
    });
}

fn next_poll_timeout(next_frame_at: Instant) -> i32 {
    let now = Instant::now();
    if now >= next_frame_at {
        return 0;
    }
    poll_timeout_for_remaining(next_frame_at.duration_since(now))
}

fn poll_timeout_for_remaining(remaining: Duration) -> i32 {
    assert!(remaining > Duration::ZERO);
    let ms = remaining
        .as_nanos()
        .div_ceil(1_000_000)
        .min(POLL_TIMEOUT_MS as u128);
    let timeout = ms.max(1) as i32;
    assert!(timeout >= 1);
    assert!(timeout <= POLL_TIMEOUT_MS);
    timeout
}

fn advance_frame_deadline(
    next_frame_at: Instant,
    now: Instant,
    interval: Duration,
) -> (Instant, u32) {
    assert!(now >= next_frame_at);
    assert!(interval > Duration::ZERO);
    let interval_ns = interval.as_nanos();
    assert!(interval_ns > 0);
    let behind = now.duration_since(next_frame_at);
    let count_unbounded = behind.as_nanos() / interval_ns + 1;
    let count = count_unbounded.min(FRAME_ADVANCE_COUNT_MAX as u128) as u32;
    assert!(count >= 1);
    assert!(count <= FRAME_ADVANCE_COUNT_MAX);
    let advanced = next_frame_at + interval.saturating_mul(count);
    if count_unbounded < FRAME_ADVANCE_COUNT_MAX as u128 {
        assert!(advanced > now);
    }
    (advanced, count - 1)
}

fn bind_listener() -> Result<RawFd, GameCaptureError> {
    let fd = unsafe { libc::socket(libc::AF_UNIX, libc::SOCK_STREAM | libc::SOCK_CLOEXEC, 0) };
    if fd < 0 {
        return Err(GameCaptureError::Bind);
    }
    let mut addr = MaybeUninit::<libc::sockaddr_un>::zeroed();
    let addr_ptr = addr.as_mut_ptr();
    unsafe {
        (*addr_ptr).sun_family = libc::AF_UNIX as libc::sa_family_t;
        (*addr_ptr).sun_path[0] = 0;
        for (idx, byte) in VKCAPTURE_SOCKET_NAME.iter().copied().enumerate() {
            (*addr_ptr).sun_path[idx + 1] = byte as libc::c_char;
        }
    }
    let addr = unsafe { addr.assume_init() };
    let len = (size_of::<libc::sa_family_t>() + 1 + VKCAPTURE_SOCKET_NAME.len()) as libc::socklen_t;
    let bind_result = unsafe { libc::bind(fd, &addr as *const _ as *const libc::sockaddr, len) };
    if bind_result != 0 {
        close_fd(fd);
        return Err(GameCaptureError::Bind);
    }
    let listen_result = unsafe { libc::listen(fd, 4) };
    if listen_result != 0 {
        close_fd(fd);
        return Err(GameCaptureError::Listen);
    }
    Ok(fd)
}

fn accept_client(listener: RawFd) -> Result<Client, String> {
    let fd = unsafe {
        libc::accept4(
            listener,
            ptr::null_mut(),
            ptr::null_mut(),
            libc::SOCK_CLOEXEC | libc::SOCK_NONBLOCK,
        )
    };
    if fd < 0 {
        return Err(format!(
            "obs-vkcapture accept failed: errno {}",
            last_errno()
        ));
    }
    let (exe, pid) = peer_process_label(fd);
    Ok(Client { fd, exe, pid })
}

fn peer_process_label(fd: RawFd) -> (String, Option<i32>) {
    let mut cred = MaybeUninit::<libc::ucred>::zeroed();
    let mut len = size_of::<libc::ucred>() as libc::socklen_t;
    let ok = unsafe {
        libc::getsockopt(
            fd,
            libc::SOL_SOCKET,
            libc::SO_PEERCRED,
            cred.as_mut_ptr() as *mut c_void,
            &mut len,
        )
    } == 0;
    if ok {
        let cred = unsafe { cred.assume_init() };
        return (format!("pid {}", cred.pid), Some(cred.pid));
    }
    ("unknown process".to_string(), None)
}

fn write_control(
    fd: RawFd,
    capturing: bool,
    import_mode: GameCaptureImportMode,
) -> std::io::Result<()> {
    let control = build_control_message(capturing, import_mode);
    let n = unsafe { libc::write(fd, control.as_ptr() as *const c_void, control.len()) };
    if n == control.len() as isize {
        Ok(())
    } else {
        Err(std::io::Error::last_os_error())
    }
}

fn build_control_message(
    capturing: bool,
    import_mode: GameCaptureImportMode,
) -> [u8; CONTROL_DATA_SIZE] {
    let mut control = [0u8; CONTROL_DATA_SIZE];
    control[0] = u8::from(capturing);
    control[1] = u8::from(import_mode.no_modifiers());
    control[2] = u8::from(import_mode.linear());
    control[3] = u8::from(import_mode.map_host());
    control
}

enum RecvMessage {
    ClientName(String),
    Texture(TextureData, [RawFd; 4]),
    WouldBlock,
    Closed,
    Invalid(String),
}

fn recv_client_message(fd: RawFd) -> RecvMessage {
    let mut buf = [0u8; TEXTURE_DATA_SIZE];
    let mut iov = libc::iovec {
        iov_base: buf.as_mut_ptr() as *mut c_void,
        iov_len: buf.len(),
    };
    let mut control = [0u8; 128];
    let mut msg = unsafe { MaybeUninit::<libc::msghdr>::zeroed().assume_init() };
    msg.msg_iov = &mut iov;
    msg.msg_iovlen = 1;
    msg.msg_control = control.as_mut_ptr() as *mut c_void;
    msg.msg_controllen = control.len();
    let n = unsafe { libc::recvmsg(fd, &mut msg, libc::MSG_NOSIGNAL) };
    if n < 0 {
        let errno = last_errno();
        if errno == libc::EAGAIN || errno == libc::EWOULDBLOCK {
            return RecvMessage::WouldBlock;
        }
        return RecvMessage::Invalid(format!("obs-vkcapture recvmsg failed: errno {errno}"));
    }
    if n == 0 {
        return RecvMessage::Closed;
    }
    match buf[0] {
        CLIENT_DATA_TYPE => {
            if n as usize != CLIENT_DATA_SIZE {
                return RecvMessage::Invalid(format!(
                    "invalid obs-vkcapture client data size: {n}"
                ));
            }
            RecvMessage::ClientName(parse_client_name(&buf))
        }
        TEXTURE_DATA_TYPE => {
            if n as usize != TEXTURE_DATA_SIZE {
                return RecvMessage::Invalid(format!(
                    "invalid obs-vkcapture texture data size: {n}"
                ));
            }
            let Some(data) = parse_texture_data(&buf) else {
                return RecvMessage::Invalid("invalid obs-vkcapture texture data".to_string());
            };
            let fds = parse_rights_fds(&control, msg.msg_controllen);
            let received = fds.iter().filter(|fd| **fd >= 0).count();
            if received != data.nfd {
                close_fds(fds);
                return RecvMessage::Invalid(format!(
                    "obs-vkcapture sent {} fds but metadata expected {}",
                    received, data.nfd
                ));
            }
            RecvMessage::Texture(data, fds)
        }
        other => RecvMessage::Invalid(format!("unknown obs-vkcapture message type: {other}")),
    }
}

fn parse_client_name(buf: &[u8; TEXTURE_DATA_SIZE]) -> String {
    let exe = &buf[1..49];
    let end = exe.iter().position(|b| *b == 0).unwrap_or(exe.len());
    String::from_utf8_lossy(&exe[..end]).trim().to_string()
}

fn parse_texture_data(buf: &[u8; TEXTURE_DATA_SIZE]) -> Option<TextureData> {
    let nfd = buf[1] as usize;
    if nfd == 0 || nfd > 4 {
        return None;
    }
    let width = read_i32(buf, 2)?;
    let height = read_i32(buf, 6)?;
    if width <= 0 || height <= 0 {
        return None;
    }
    let format = read_i32(buf, 10)?;
    let mut strides = [0i32; 4];
    let mut offsets = [0i32; 4];
    for (idx, stride) in strides.iter_mut().enumerate() {
        *stride = read_i32(buf, 14 + idx * 4)?;
    }
    for (idx, offset) in offsets.iter_mut().enumerate() {
        *offset = read_i32(buf, 30 + idx * 4)?;
    }
    Some(TextureData {
        nfd,
        width: width as u32,
        height: height as u32,
        format,
        strides,
        offsets,
        modifier: read_u64(buf, 46)?,
        flip: buf[58] != 0,
        color_space: read_u32(buf, 59)?,
    })
}

fn read_i32(buf: &[u8], offset: usize) -> Option<i32> {
    let bytes: [u8; 4] = buf.get(offset..offset + 4)?.try_into().ok()?;
    Some(i32::from_le_bytes(bytes))
}

fn read_u32(buf: &[u8], offset: usize) -> Option<u32> {
    let bytes: [u8; 4] = buf.get(offset..offset + 4)?.try_into().ok()?;
    Some(u32::from_le_bytes(bytes))
}

fn read_u64(buf: &[u8], offset: usize) -> Option<u64> {
    let bytes: [u8; 8] = buf.get(offset..offset + 8)?.try_into().ok()?;
    Some(u64::from_le_bytes(bytes))
}

fn parse_rights_fds(control: &[u8], controllen: usize) -> [RawFd; 4] {
    let mut fds = [-1; 4];
    let header_size = cmsg_align(size_of::<libc::cmsghdr>());
    if controllen < header_size || control.len() < header_size {
        return fds;
    }
    let header = unsafe { ptr::read_unaligned(control.as_ptr() as *const libc::cmsghdr) };
    if header.cmsg_level != libc::SOL_SOCKET || header.cmsg_type != libc::SCM_RIGHTS {
        return fds;
    }
    let cmsg_len = header.cmsg_len;
    if cmsg_len < header_size || cmsg_len > controllen || cmsg_len > control.len() {
        return fds;
    }
    let data_len = cmsg_len - header_size;
    let count = (data_len / size_of::<RawFd>()).min(4);
    for (idx, out) in fds.iter_mut().enumerate().take(count) {
        let offset = header_size + idx * size_of::<RawFd>();
        *out = unsafe { ptr::read_unaligned(control.as_ptr().add(offset) as *const RawFd) };
    }
    fds
}

fn cmsg_align(len: usize) -> usize {
    let align = size_of::<usize>();
    (len + align - 1) & !(align - 1)
}

fn map_texture(
    data: TextureData,
    fds: [RawFd; 4],
    import_mode: GameCaptureImportMode,
    on_lifecycle: &LifecycleCallback,
) -> Option<MappedTexture> {
    if !import_mode.map_host() {
        let dmabuf_layout = validate_dmabuf_texture_data(data, fds, on_lifecycle)?;
        on_lifecycle(
            "diagnostic",
            &format!(
                "obs-vkcapture texture imported as zero-copy DMA-BUF: {}x{} fourcc={} modifier={}",
                dmabuf_layout.width,
                dmabuf_layout.height,
                fourcc_to_string(data.format),
                data.modifier
            ),
        );
        return Some(MappedTexture {
            fds,
            nfd: data.nfd,
            width: dmabuf_layout.width,
            height: dmabuf_layout.height,
            format: data.format,
            strides: dmabuf_layout.strides,
            offsets: dmabuf_layout.offsets,
            stride: dmabuf_layout.strides[0] as usize,
            offset: dmabuf_layout.offsets[0] as usize,
            modifier: data.modifier,
            flip: data.flip,
            color_space: data.color_space,
            zero_copy: true,
            ptr: ptr::null_mut(),
            map_size: 0,
            frame_pool: None,
            bgra_scratch: Vec::new(),
        });
    }
    let host_layout = validate_host_mapped_texture_data(data, fds, on_lifecycle)?;
    let map_size = unsafe { libc::lseek(fds[0], 0, libc::SEEK_END) };
    if map_size <= 0 {
        on_lifecycle(
            "diagnostic",
            "obs-vkcapture dma-buf size could not be determined",
        );
        return None;
    }
    let map_size = map_size as usize;
    let Some(required) = host_layout.offset.checked_add(
        host_layout
            .stride
            .checked_mul(host_layout.height as usize)?,
    ) else {
        on_lifecycle(
            "diagnostic",
            "obs-vkcapture dma-buf layout overflows addressable memory",
        );
        return None;
    };
    if required > map_size {
        on_lifecycle(
            "diagnostic",
            &format!("obs-vkcapture dma-buf too small: required {required}, size {map_size}"),
        );
        return None;
    }
    let ptr = unsafe {
        libc::mmap(
            ptr::null_mut(),
            map_size,
            libc::PROT_READ,
            libc::MAP_SHARED,
            fds[0],
            0,
        )
    };
    if ptr == libc::MAP_FAILED {
        on_lifecycle(
            "diagnostic",
            &format!("obs-vkcapture mmap failed: errno {}", last_errno()),
        );
        return None;
    }
    let layout = Nv12Layout {
        width: host_layout.width,
        height: host_layout.height,
        stride_y: host_layout.width,
        stride_uv: host_layout.width,
    };
    let frame_pool = LinuxFrameBufferPool::new(layout.packed_size()?).ok()?;
    Some(MappedTexture {
        fds,
        nfd: data.nfd,
        width: layout.width,
        height: layout.height,
        format: data.format,
        strides: host_layout.strides,
        offsets: host_layout.offsets,
        stride: host_layout.stride,
        offset: host_layout.offset,
        modifier: data.modifier,
        flip: data.flip,
        color_space: data.color_space,
        zero_copy: false,
        ptr,
        map_size,
        frame_pool: Some(frame_pool),
        bgra_scratch: Vec::new(),
    })
}

fn validate_dmabuf_texture_data(
    data: TextureData,
    fds: [RawFd; 4],
    on_lifecycle: &LifecycleCallback,
) -> Option<HostMappedLayout> {
    if !(1..=4).contains(&data.nfd) {
        on_lifecycle(
            "diagnostic",
            &format!(
                "obs-vkcapture DMA-BUF backend received invalid fd count {}",
                data.nfd
            ),
        );
        return None;
    }
    let Some(min_stride) = dmabuf_min_stride(data.format, data.width) else {
        on_lifecycle(
            "diagnostic",
            &format!(
                "obs-vkcapture DMA-BUF backend does not support fourcc {}; supported zero-copy formats: {}",
                fourcc_to_string(data.format),
                supported_dmabuf_format_names()
            ),
        );
        return None;
    };
    let width = data.width & !1;
    let height = data.height & !1;
    if width < 16 || height < 16 {
        on_lifecycle(
            "diagnostic",
            &format!(
                "obs-vkcapture DMA-BUF dimensions are invalid: {}x{}",
                data.width, data.height
            ),
        );
        return None;
    }
    let mut strides = [0u32; 4];
    let mut offsets = [0u32; 4];
    for idx in 0..data.nfd {
        if fds[idx] < 0 {
            on_lifecycle(
                "diagnostic",
                "obs-vkcapture DMA-BUF backend received an invalid dma-buf fd",
            );
            return None;
        }
        let Some(stride) = data.strides.get(idx).and_then(|stride| {
            if *stride > 0 {
                u32::try_from(*stride).ok()
            } else {
                None
            }
        }) else {
            on_lifecycle(
                "diagnostic",
                "obs-vkcapture DMA-BUF backend received an invalid stride",
            );
            return None;
        };
        let Some(offset) = data.offsets.get(idx).and_then(|offset| {
            if *offset >= 0 {
                u32::try_from(*offset).ok()
            } else {
                None
            }
        }) else {
            on_lifecycle(
                "diagnostic",
                "obs-vkcapture DMA-BUF backend received an invalid offset",
            );
            return None;
        };
        if idx == 0 && stride < min_stride {
            on_lifecycle(
                "diagnostic",
                &format!(
                    "obs-vkcapture DMA-BUF stride is invalid: stride {} minimum {} for {}",
                    stride,
                    min_stride,
                    fourcc_to_string(data.format)
                ),
            );
            return None;
        }
        strides[idx] = stride;
        offsets[idx] = offset;
    }
    Some(HostMappedLayout {
        stride: strides[0] as usize,
        offset: offsets[0] as usize,
        strides,
        offsets,
        width,
        height,
    })
}

fn validate_host_mapped_texture_data(
    data: TextureData,
    fds: [RawFd; 4],
    on_lifecycle: &LifecycleCallback,
) -> Option<HostMappedLayout> {
    if data.nfd != 1 {
        on_lifecycle(
            "diagnostic",
            &format!(
                "obs-vkcapture host-mapped backend only supports one fd, got {}",
                data.nfd
            ),
        );
        return None;
    }
    if fds[0] < 0 {
        on_lifecycle(
            "diagnostic",
            "obs-vkcapture host-mapped backend received an invalid dma-buf fd",
        );
        return None;
    }
    let Some(format) = CpuFormat::from_drm(data.format) else {
        on_lifecycle(
            "diagnostic",
            &format!(
                "obs-vkcapture host-mapped backend does not support fourcc {}; supported host-mapped formats: {}",
                fourcc_to_string(data.format),
                supported_host_mapped_format_names()
            ),
        );
        return None;
    };
    let Some(stride) = data.strides.first().and_then(|stride| {
        if *stride > 0 {
            usize::try_from(*stride).ok()
        } else {
            None
        }
    }) else {
        on_lifecycle(
            "diagnostic",
            "obs-vkcapture host-mapped backend received an invalid stride",
        );
        return None;
    };
    let Some(offset) = data.offsets.first().and_then(|offset| {
        if *offset >= 0 {
            usize::try_from(*offset).ok()
        } else {
            None
        }
    }) else {
        on_lifecycle(
            "diagnostic",
            "obs-vkcapture host-mapped backend received an invalid offset",
        );
        return None;
    };
    let mut strides = [0u32; 4];
    let mut offsets = [0u32; 4];
    for idx in 0..data.nfd {
        let Some(stride) = data.strides.get(idx).and_then(|stride| {
            if *stride > 0 {
                u32::try_from(*stride).ok()
            } else {
                None
            }
        }) else {
            on_lifecycle(
                "diagnostic",
                "obs-vkcapture host-mapped backend received an invalid stride",
            );
            return None;
        };
        let Some(offset) = data.offsets.get(idx).and_then(|offset| {
            if *offset >= 0 {
                u32::try_from(*offset).ok()
            } else {
                None
            }
        }) else {
            on_lifecycle(
                "diagnostic",
                "obs-vkcapture host-mapped backend received an invalid offset",
            );
            return None;
        };
        strides[idx] = stride;
        offsets[idx] = offset;
    }
    let width = data.width & !1;
    let height = data.height & !1;
    let min_stride = width as usize * format.bytes_per_pixel();
    if width < 16 || height < 16 || stride < min_stride {
        on_lifecycle(
            "diagnostic",
            &format!(
                "obs-vkcapture texture dimensions or stride are invalid: {}x{} stride {} minimum {} for {}",
                data.width,
                data.height,
                stride,
                min_stride,
                fourcc_to_string(data.format)
            ),
        );
        return None;
    }
    Some(HostMappedLayout {
        stride,
        offset,
        strides,
        offsets,
        width,
        height,
    })
}

impl MappedTexture {
    fn read_frame(&mut self) -> Option<VideoFrame> {
        let timestamp_us = monotonic_us();
        if self.zero_copy {
            return Some(VideoFrame {
                width: self.width,
                height: self.height,
                stride_y: self.strides[0],
                stride_uv: if self.nfd > 1 {
                    self.strides[1]
                } else {
                    self.strides[0]
                },
                timestamp_us,
                data: VideoFrameData::Empty,
                dmabuf: Some(self.dmabuf_metadata()),
            });
        }

        dma_buf_sync(self.fds[0], false);
        let mapped = unsafe { slice::from_raw_parts(self.ptr as *const u8, self.map_size) };
        let src = mapped.get(self.offset..)?;
        let layout = Nv12Layout {
            width: self.width,
            height: self.height,
            stride_y: self.width,
            stride_uv: self.width,
        };
        let total_bytes = layout.packed_size()?;
        let mut pooled = self.frame_pool.as_ref()?.try_acquire()?;
        if total_bytes > pooled.buffer_mut().len() {
            return None;
        }
        let dst = &mut pooled.buffer_mut()[..total_bytes];
        let format = CpuFormat::from_drm(self.format)?;
        let ok = if format.is_bgra_passthrough() {
            bgra_to_nv12(layout, src, self.stride as u32, dst, self.flip)
        } else {
            self.copy_to_bgra_scratch(src)?;
            bgra_to_nv12(layout, &self.bgra_scratch, self.width * 4, dst, false)
        };
        dma_buf_sync(self.fds[0], true);
        if !ok {
            return None;
        }
        pooled.set_len(total_bytes);
        Some(VideoFrame {
            width: self.width,
            height: self.height,
            stride_y: layout.packed_stride_y(),
            stride_uv: layout.packed_stride_uv(),
            timestamp_us,
            data: VideoFrameData::Pooled(pooled),
            dmabuf: Some(self.dmabuf_metadata()),
        })
    }

    fn dmabuf_metadata(&self) -> DmabufFrameMetadata {
        DmabufFrameMetadata {
            fds: self.fds,
            plane_count: self.nfd.min(4) as u32,
            drm_format: self.format as u32,
            modifier: self.modifier,
            strides: self.strides,
            offsets: self.offsets,
            device_uuid: None,
        }
    }

    fn copy_to_bgra_scratch(&mut self, src: &[u8]) -> Option<()> {
        let width = self.width as usize;
        let height = self.height as usize;
        let dst_stride = width.checked_mul(4)?;
        let format = CpuFormat::from_drm(self.format)?;
        self.bgra_scratch.resize(dst_stride.checked_mul(height)?, 0);
        for row in 0..height {
            let src_row_index = if self.flip { height - 1 - row } else { row };
            let src_offset = src_row_index.checked_mul(self.stride)?;
            let dst_offset = row.checked_mul(dst_stride)?;
            let src_row = src.get(src_offset..src_offset + width * format.bytes_per_pixel())?;
            let dst_row = &mut self.bgra_scratch[dst_offset..dst_offset + dst_stride];
            format.write_bgra_row(
                src_row,
                width,
                dst_row,
                self.color_space == VK_COLOR_SPACE_HDR10_ST2084_EXT,
            );
        }
        Some(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CpuFormat {
    Bgra8,
    Rgba8,
    A2r10g10b10,
    A2b10g10r10,
    Rgba16Unorm,
    Rgba16Float,
}

impl CpuFormat {
    fn from_drm(format: i32) -> Option<Self> {
        match format {
            DRM_FORMAT_XRGB8888 | DRM_FORMAT_ARGB8888 => Some(Self::Bgra8),
            DRM_FORMAT_XBGR8888 | DRM_FORMAT_ABGR8888 => Some(Self::Rgba8),
            DRM_FORMAT_XRGB2101010 | DRM_FORMAT_ARGB2101010 => Some(Self::A2r10g10b10),
            DRM_FORMAT_XBGR2101010 | DRM_FORMAT_ABGR2101010 => Some(Self::A2b10g10r10),
            DRM_FORMAT_XBGR16161616 | DRM_FORMAT_ABGR16161616 => Some(Self::Rgba16Unorm),
            DRM_FORMAT_XBGR16161616F | DRM_FORMAT_ABGR16161616F => Some(Self::Rgba16Float),
            _ => None,
        }
    }

    fn bytes_per_pixel(self) -> usize {
        match self {
            Self::Bgra8 | Self::Rgba8 | Self::A2r10g10b10 | Self::A2b10g10r10 => 4,
            Self::Rgba16Unorm | Self::Rgba16Float => 8,
        }
    }

    fn is_bgra_passthrough(self) -> bool {
        matches!(self, Self::Bgra8)
    }

    fn write_bgra_row(self, src_row: &[u8], width: usize, dst_row: &mut [u8], hdr: bool) {
        match self {
            Self::Bgra8 => copy_bgra8_row(src_row, width, dst_row),
            Self::Rgba8 => rgba8_row_to_bgra(src_row, width, dst_row),
            Self::A2r10g10b10 => a2r10g10b10_row_to_bgra(src_row, width, dst_row, hdr),
            Self::A2b10g10r10 => a2b10g10r10_row_to_bgra(src_row, width, dst_row, hdr),
            Self::Rgba16Unorm => rgba16_unorm_row_to_bgra(src_row, width, dst_row),
            Self::Rgba16Float => rgba16f_row_to_bgra(src_row, width, dst_row, hdr),
        }
    }
}

fn copy_bgra8_row(src_row: &[u8], width: usize, dst_row: &mut [u8]) {
    let bytes = width
        .saturating_mul(4)
        .min(src_row.len())
        .min(dst_row.len());
    dst_row[..bytes].copy_from_slice(&src_row[..bytes]);
}

fn rgba8_row_to_bgra(src_row: &[u8], width: usize, dst_row: &mut [u8]) {
    for x in 0..width {
        let offset = x * 4;
        if offset + 4 > src_row.len() || offset + 4 > dst_row.len() {
            break;
        }
        dst_row[offset] = src_row[offset + 2];
        dst_row[offset + 1] = src_row[offset + 1];
        dst_row[offset + 2] = src_row[offset];
        dst_row[offset + 3] = src_row[offset + 3];
    }
}

fn a2b10g10r10_row_to_bgra(src_row: &[u8], width: usize, dst_row: &mut [u8], hdr: bool) {
    for x in 0..width {
        let so = x * 4;
        let dofs = x * 4;
        if so + 4 > src_row.len() || dofs + 4 > dst_row.len() {
            break;
        }
        let packed = u32::from_le_bytes([
            src_row[so],
            src_row[so + 1],
            src_row[so + 2],
            src_row[so + 3],
        ]);
        let r10 = (packed & 0x3ff) as u16;
        let g10 = ((packed >> 10) & 0x3ff) as u16;
        let b10 = ((packed >> 20) & 0x3ff) as u16;
        write_10bit_bgra(
            dst_row,
            dofs,
            r10,
            g10,
            b10,
            ((packed >> 30) & 0x3) as u8,
            hdr,
        );
    }
}

fn a2r10g10b10_row_to_bgra(src_row: &[u8], width: usize, dst_row: &mut [u8], hdr: bool) {
    for x in 0..width {
        let so = x * 4;
        let dofs = x * 4;
        if so + 4 > src_row.len() || dofs + 4 > dst_row.len() {
            break;
        }
        let packed = u32::from_le_bytes([
            src_row[so],
            src_row[so + 1],
            src_row[so + 2],
            src_row[so + 3],
        ]);
        let b10 = (packed & 0x3ff) as u16;
        let g10 = ((packed >> 10) & 0x3ff) as u16;
        let r10 = ((packed >> 20) & 0x3ff) as u16;
        write_10bit_bgra(
            dst_row,
            dofs,
            r10,
            g10,
            b10,
            ((packed >> 30) & 0x3) as u8,
            hdr,
        );
    }
}

fn write_10bit_bgra(
    dst_row: &mut [u8],
    offset: usize,
    r10: u16,
    g10: u16,
    b10: u16,
    a2: u8,
    hdr: bool,
) {
    let (r, g, b) = if hdr {
        tonemap_rec2020_pq_to_srgb8(r10, g10, b10)
    } else {
        (scale10_to_8(r10), scale10_to_8(g10), scale10_to_8(b10))
    };
    dst_row[offset] = b;
    dst_row[offset + 1] = g;
    dst_row[offset + 2] = r;
    dst_row[offset + 3] = (a2 as u16 * 255 / 3) as u8;
}

fn rgba16_unorm_row_to_bgra(src_row: &[u8], width: usize, dst_row: &mut [u8]) {
    for x in 0..width {
        let so = x * 8;
        let dofs = x * 4;
        if so + 8 > src_row.len() || dofs + 4 > dst_row.len() {
            break;
        }
        let r = u16::from_le_bytes([src_row[so], src_row[so + 1]]);
        let g = u16::from_le_bytes([src_row[so + 2], src_row[so + 3]]);
        let b = u16::from_le_bytes([src_row[so + 4], src_row[so + 5]]);
        let a = u16::from_le_bytes([src_row[so + 6], src_row[so + 7]]);
        dst_row[dofs] = scale16_to_8(b);
        dst_row[dofs + 1] = scale16_to_8(g);
        dst_row[dofs + 2] = scale16_to_8(r);
        dst_row[dofs + 3] = scale16_to_8(a);
    }
}

fn rgba16f_row_to_bgra(src_row: &[u8], width: usize, dst_row: &mut [u8], hdr: bool) {
    for x in 0..width {
        let so = x * 8;
        let dofs = x * 4;
        if so + 8 > src_row.len() || dofs + 4 > dst_row.len() {
            break;
        }
        let r = f16_to_f32(u16::from_le_bytes([src_row[so], src_row[so + 1]]));
        let g = f16_to_f32(u16::from_le_bytes([src_row[so + 2], src_row[so + 3]]));
        let b = f16_to_f32(u16::from_le_bytes([src_row[so + 4], src_row[so + 5]]));
        let a = f16_to_f32(u16::from_le_bytes([src_row[so + 6], src_row[so + 7]]));
        let (lr, lg, lb) = if hdr {
            (
                reinhard(r.max(0.0)),
                reinhard(g.max(0.0)),
                reinhard(b.max(0.0)),
            )
        } else {
            (r.clamp(0.0, 1.0), g.clamp(0.0, 1.0), b.clamp(0.0, 1.0))
        };
        dst_row[dofs] = linear_to_srgb8(lb);
        dst_row[dofs + 1] = linear_to_srgb8(lg);
        dst_row[dofs + 2] = linear_to_srgb8(lr);
        dst_row[dofs + 3] = (a.clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
    }
}

fn scale10_to_8(v10: u16) -> u8 {
    ((v10 as u32 * 255 + 511) / 1023) as u8
}

fn scale16_to_8(v16: u16) -> u8 {
    ((v16 as u32 * 255 + 32767) / 65535) as u8
}

fn reinhard(linear: f32) -> f32 {
    let v = linear.max(0.0);
    (v / (1.0 + v)).clamp(0.0, 1.0)
}

fn tonemap_rec2020_pq_to_srgb8(r10: u16, g10: u16, b10: u16) -> (u8, u8, u8) {
    const SDR_WHITE_NITS: f32 = 80.0;
    let lr = pq_eotf(r10 as f32 / 1023.0);
    let lg = pq_eotf(g10 as f32 / 1023.0);
    let lb = pq_eotf(b10 as f32 / 1023.0);
    let scale = 10000.0 / SDR_WHITE_NITS;
    let map = |v: f32| reinhard((v * scale).max(0.0));
    (
        linear_to_srgb8(map(lr)),
        linear_to_srgb8(map(lg)),
        linear_to_srgb8(map(lb)),
    )
}

fn pq_eotf(e: f32) -> f32 {
    const M1: f64 = 0.1593017578125;
    const M2: f64 = 78.84375;
    const C1: f64 = 0.8359375;
    const C2: f64 = 18.8515625;
    const C3: f64 = 18.6875;
    let e = (e.clamp(0.0, 1.0)) as f64;
    let ep = e.powf(1.0 / M2);
    let num = (ep - C1).max(0.0);
    let den = C2 - C3 * ep;
    if den <= 0.0 {
        return 0.0;
    }
    (num / den).powf(1.0 / M1) as f32
}

fn linear_to_srgb8(linear: f32) -> u8 {
    let l = linear.clamp(0.0, 1.0);
    let srgb = if l <= 0.0031308 {
        l * 12.92
    } else {
        1.055 * l.powf(1.0 / 2.4) - 0.055
    };
    (srgb.clamp(0.0, 1.0) * 255.0 + 0.5) as u8
}

fn f16_to_f32(h: u16) -> f32 {
    let sign = (h >> 15) & 0x1;
    let exp = (h >> 10) & 0x1f;
    let mant = h & 0x3ff;
    let sign_f = if sign == 1 { -1.0f32 } else { 1.0f32 };
    if exp == 0 {
        sign_f * (mant as f32) * 2f32.powi(-24)
    } else if exp == 0x1f {
        if mant == 0 { sign_f * 65504.0 } else { 0.0 }
    } else {
        sign_f * (1.0 + (mant as f32) / 1024.0) * 2f32.powi(exp as i32 - 15)
    }
}

fn fourcc_to_string(format: i32) -> String {
    let bytes = (format as u32).to_le_bytes();
    if bytes.iter().all(|b| b.is_ascii_graphic() || *b == b' ') {
        String::from_utf8_lossy(&bytes).to_string()
    } else {
        format!("0x{:08x}", format as u32)
    }
}

fn format_modifier(modifier: u64) -> String {
    if modifier == DRM_FORMAT_MOD_INVALID {
        "INVALID".to_string()
    } else {
        format!("0x{modifier:016x}")
    }
}

fn supported_host_mapped_format_names() -> String {
    SUPPORTED_HOST_MAPPED_FORMATS
        .iter()
        .map(|format| fourcc_to_string(*format))
        .collect::<Vec<_>>()
        .join(", ")
}

fn dmabuf_min_stride(format: i32, width: u32) -> Option<u32> {
    match format {
        DRM_FORMAT_XRGB8888
        | DRM_FORMAT_ARGB8888
        | DRM_FORMAT_XBGR8888
        | DRM_FORMAT_ABGR8888
        | DRM_FORMAT_XRGB2101010
        | DRM_FORMAT_ARGB2101010
        | DRM_FORMAT_XBGR2101010
        | DRM_FORMAT_ABGR2101010 => width.checked_mul(4),
        DRM_FORMAT_NV12 => Some(width),
        _ => None,
    }
}

fn supported_dmabuf_format_names() -> String {
    [
        DRM_FORMAT_XRGB8888,
        DRM_FORMAT_ARGB8888,
        DRM_FORMAT_XBGR8888,
        DRM_FORMAT_ABGR8888,
        DRM_FORMAT_XRGB2101010,
        DRM_FORMAT_ARGB2101010,
        DRM_FORMAT_XBGR2101010,
        DRM_FORMAT_ABGR2101010,
        DRM_FORMAT_NV12,
    ]
    .iter()
    .map(|format| fourcc_to_string(*format))
    .collect::<Vec<_>>()
    .join(", ")
}

#[repr(C)]
struct DmaBufSync {
    flags: u64,
}

fn dma_buf_sync(fd: RawFd, end: bool) {
    let mut sync = DmaBufSync {
        flags: DMA_BUF_SYNC_READ | if end { DMA_BUF_SYNC_END } else { 0 },
    };
    unsafe {
        libc::ioctl(fd, DMA_BUF_IOCTL_SYNC, &mut sync);
    }
}

fn monotonic_us() -> i64 {
    let mut ts: libc::timespec = unsafe { std::mem::zeroed() };
    let rc = unsafe { libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut ts) };
    assert!(rc == 0);
    assert!(ts.tv_sec >= 0);
    assert!(ts.tv_nsec >= 0);
    let us = (ts.tv_sec as u64)
        .saturating_mul(1_000_000)
        .saturating_add(ts.tv_nsec as u64 / 1_000);
    assert!(us <= i64::MAX as u64);
    us as i64
}

struct FdGuard(RawFd);

impl Drop for FdGuard {
    fn drop(&mut self) {
        if self.0 >= 0 {
            close_fd(self.0);
        }
    }
}

fn close_fds(mut fds: [RawFd; 4]) {
    for fd in &mut fds {
        if *fd >= 0 {
            close_fd(*fd);
            *fd = -1;
        }
    }
}

fn close_fd(fd: RawFd) {
    unsafe {
        libc::close(fd);
    }
}

fn last_errno() -> i32 {
    std::io::Error::last_os_error()
        .raw_os_error()
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_i32(buf: &mut [u8], offset: usize, value: i32) {
        buf[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
    }

    fn write_u32(buf: &mut [u8], offset: usize, value: u32) {
        buf[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
    }

    fn write_u64(buf: &mut [u8], offset: usize, value: u64) {
        buf[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
    }

    type LifecycleRecorder = (LifecycleCallback, Arc<Mutex<Vec<(String, String)>>>);

    fn lifecycle_recorder() -> LifecycleRecorder {
        let observed = Arc::new(Mutex::new(Vec::<(String, String)>::new()));
        let observed_for_cb = observed.clone();
        let callback: LifecycleCallback = Arc::new(move |kind, message| {
            observed_for_cb
                .lock()
                .expect("observed lock")
                .push((kind.to_string(), message.to_string()));
        });
        (callback, observed)
    }

    fn observed_messages(observed: &Arc<Mutex<Vec<(String, String)>>>) -> Vec<String> {
        observed
            .lock()
            .expect("observed lock")
            .iter()
            .map(|(_, message)| message.clone())
            .collect()
    }

    fn host_mapped_texture_data() -> TextureData {
        TextureData {
            nfd: 1,
            width: 16,
            height: 16,
            format: DRM_FORMAT_ARGB8888,
            strides: [64, 0, 0, 0],
            offsets: [0, 0, 0, 0],
            modifier: DRM_FORMAT_MOD_INVALID,
            flip: false,
            color_space: 0,
        }
    }

    fn memfd_with_data(name: &str, data: &[u8]) -> RawFd {
        let name = std::ffi::CString::new(name).expect("memfd name");
        let fd = unsafe { libc::memfd_create(name.as_ptr(), libc::MFD_CLOEXEC) };
        assert!(fd >= 0, "memfd_create failed: {}", last_errno());
        let resize = unsafe { libc::ftruncate(fd, data.len() as libc::off_t) };
        assert_eq!(resize, 0, "ftruncate failed: {}", last_errno());
        let written = unsafe { libc::write(fd, data.as_ptr() as *const c_void, data.len()) };
        assert_eq!(
            written,
            data.len() as isize,
            "write failed: {}",
            last_errno()
        );
        fd
    }

    #[test]
    fn diagnostics_default_to_obs_fallback_capture_policy() {
        let import_state = GameCaptureImportState::auto();
        let diagnostics = GameCaptureDiagnostics::new_with_import_state(60, import_state);
        assert_eq!(diagnostics.backend, "obs-vkcapture");
        assert_eq!(diagnostics.active_strategy, "game-hook-auto-fallback");
        assert_eq!(diagnostics.requested_import_mode, "auto-obs-fallback");
        assert_eq!(diagnostics.import_mode, "default-dmabuf");
        assert!(!diagnostics.map_host);
        assert!(!diagnostics.linear);
        assert!(!diagnostics.no_modifiers);
        assert!(diagnostics.zero_copy);
        assert!(diagnostics.gpu_import_available);
        assert!(!diagnostics.device_uuid_advertised);
        assert_eq!(diagnostics.frame_transport, "gpu-dmabuf-zero-copy");
        assert!(!diagnostics.host_mapped_cpu_fallback);
        assert!(diagnostics.source_dmabuf_metadata_available);
        assert_eq!(diagnostics.supported_import_modes, OBS_IMPORT_MODE_NAMES);
        assert!(!diagnostics.client_connected);
        assert_eq!(diagnostics.frame_counter, 0);
        assert_eq!(diagnostics.dropped_frame_counter, 0);
        assert_eq!(diagnostics.lagged_frame_counter, 0);
        assert_eq!(diagnostics.unsupported_frame_counter, 0);
        assert_eq!(diagnostics.requested_injection_method, "obs-vkcapture");
        assert_eq!(diagnostics.injection_method, "obs-vkcapture");
        assert!(diagnostics.last_fallback_reason.is_some());
    }

    #[test]
    fn diagnostics_can_pin_host_mapped_capture_policy() {
        let import_state = GameCaptureImportState::fixed(GameCaptureImportMode::LinearHostMapped);
        let diagnostics = GameCaptureDiagnostics::new_with_import_state(60, import_state);
        assert_eq!(diagnostics.active_strategy, "game-hook-host-mapped");
        assert_eq!(
            diagnostics.requested_import_mode,
            "linear-host-mapped-dmabuf"
        );
        assert_eq!(diagnostics.import_mode, "linear-host-mapped-dmabuf");
        assert!(diagnostics.map_host);
        assert!(diagnostics.linear);
        assert!(!diagnostics.no_modifiers);
        assert!(!diagnostics.zero_copy);
        assert!(!diagnostics.gpu_import_available);
        assert_eq!(
            diagnostics.frame_transport,
            "host-mapped-cpu-nv12-with-source-dmabuf"
        );
        assert!(diagnostics.host_mapped_cpu_fallback);
        assert!(diagnostics.source_dmabuf_metadata_available);
    }

    #[test]
    fn lifecycle_messages_update_diagnostics_snapshot() {
        let diagnostics = Arc::new(Mutex::new(GameCaptureDiagnostics::new(30)));
        let observed = Arc::new(Mutex::new(Vec::<(String, String)>::new()));
        let observed_for_cb = observed.clone();
        let callback: LifecycleCallback = Arc::new(move |kind, message| {
            observed_for_cb
                .lock()
                .expect("observed lock")
                .push((kind.to_string(), message.to_string()));
        });

        publish_lifecycle(
            &diagnostics,
            &callback,
            "diagnostic",
            "obs-vkcapture texture mapped",
        );
        publish_lifecycle(
            &diagnostics,
            &callback,
            "error",
            "obs-vkcapture poll failed",
        );

        let snapshot = diagnostics.lock().expect("diagnostics lock");
        assert_eq!(
            snapshot.last_diagnostic.as_deref(),
            Some("obs-vkcapture texture mapped")
        );
        assert_eq!(
            snapshot.last_addon_error.as_deref(),
            Some("obs-vkcapture poll failed")
        );
        assert_eq!(
            observed.lock().expect("observed lock").as_slice(),
            [
                (
                    "diagnostic".to_string(),
                    "obs-vkcapture texture mapped".to_string()
                ),
                ("error".to_string(), "obs-vkcapture poll failed".to_string())
            ]
        );
    }

    #[test]
    fn diagnostics_modifier_uses_obs_invalid_name() {
        assert_eq!(format_modifier(DRM_FORMAT_MOD_INVALID), "INVALID");
        assert_eq!(format_modifier(0x0102), "0x0000000000000102");
    }

    #[test]
    fn parses_obs_vkcapture_texture_layout() {
        let mut buf = [0u8; TEXTURE_DATA_SIZE];
        buf[0] = TEXTURE_DATA_TYPE;
        buf[1] = 1;
        write_i32(&mut buf, 2, 1280);
        write_i32(&mut buf, 6, 720);
        write_i32(&mut buf, 10, DRM_FORMAT_ARGB8888);
        write_i32(&mut buf, 14, 5120);
        write_i32(&mut buf, 30, 256);
        write_u64(&mut buf, 46, DRM_FORMAT_MOD_INVALID);
        buf[58] = 1;
        write_u32(&mut buf, 59, 42);

        let parsed = parse_texture_data(&buf).expect("texture data");
        assert_eq!(parsed.nfd, 1);
        assert_eq!(parsed.width, 1280);
        assert_eq!(parsed.height, 720);
        assert_eq!(parsed.format, DRM_FORMAT_ARGB8888);
        assert_eq!(parsed.strides[0], 5120);
        assert_eq!(parsed.offsets[0], 256);
        assert_eq!(parsed.modifier, DRM_FORMAT_MOD_INVALID);
        assert!(parsed.flip);
        assert_eq!(parsed.color_space, 42);
    }

    #[test]
    fn mapped_texture_exports_dmabuf_metadata_for_native_encoder() {
        let texture = MappedTexture {
            fds: [-1, -1, -1, -1],
            nfd: 1,
            width: 1280,
            height: 720,
            format: DRM_FORMAT_ABGR8888,
            strides: [5120, 0, 0, 0],
            offsets: [256, 0, 0, 0],
            stride: 5120,
            offset: 256,
            modifier: DRM_FORMAT_MOD_INVALID,
            flip: false,
            color_space: 0,
            zero_copy: false,
            ptr: ptr::null_mut(),
            map_size: 0,
            frame_pool: None,
            bgra_scratch: Vec::new(),
        };

        let metadata = texture.dmabuf_metadata();
        assert_eq!(metadata.plane_count, 1);
        assert_eq!(metadata.fds, [-1, -1, -1, -1]);
        assert_eq!(metadata.drm_format, DRM_FORMAT_ABGR8888 as u32);
        assert_eq!(metadata.modifier, DRM_FORMAT_MOD_INVALID);
        assert_eq!(metadata.strides, [5120, 0, 0, 0]);
        assert_eq!(metadata.offsets, [256, 0, 0, 0]);
        assert_eq!(metadata.device_uuid, None);
    }

    #[test]
    fn non_host_mapped_import_forwards_dmabuf_without_mapping() {
        let fd = memfd_with_data("fluxer-zero-copy-dmabuf-test", &[0u8; 16 * 16 * 4]);
        let (callback, observed) = lifecycle_recorder();
        let mut mapped = map_texture(
            host_mapped_texture_data(),
            [fd, -1, -1, -1],
            GameCaptureImportMode::Default,
            &callback,
        )
        .expect("zero-copy dmabuf texture");

        assert!(mapped.zero_copy);
        assert!(mapped.ptr.is_null());
        assert_eq!(mapped.map_size, 0);
        let frame = mapped.read_frame().expect("zero-copy frame");
        let metadata = frame.dmabuf.expect("dmabuf metadata");
        assert!(frame.data.is_empty());
        assert_eq!(metadata.fds[0], fd);
        assert_eq!(metadata.drm_format, DRM_FORMAT_ARGB8888 as u32);
        assert!(
            observed_messages(&observed)
                .iter()
                .any(|message| message.contains("zero-copy DMA-BUF"))
        );
    }

    #[test]
    fn host_mapped_import_rejects_invalid_fd_with_diagnostic() {
        let (callback, observed) = lifecycle_recorder();
        let mapped = map_texture(
            host_mapped_texture_data(),
            [-1, -1, -1, -1],
            GameCaptureImportMode::LinearHostMapped,
            &callback,
        );

        assert!(mapped.is_none());
        assert!(
            observed_messages(&observed)
                .iter()
                .any(|message| message.contains("invalid dma-buf fd"))
        );
    }

    #[test]
    fn host_mapped_import_rejects_multiplane_descriptors_with_diagnostic() {
        let mut data = host_mapped_texture_data();
        data.nfd = 2;
        let (callback, observed) = lifecycle_recorder();
        let mapped = map_texture(
            data,
            [4, -1, -1, -1],
            GameCaptureImportMode::LinearHostMapped,
            &callback,
        );

        assert!(mapped.is_none());
        assert!(
            observed_messages(&observed)
                .iter()
                .any(|message| message.contains("only supports one fd"))
        );
    }

    #[test]
    fn host_mapped_import_rejects_unsupported_formats_with_supported_list() {
        let mut data = host_mapped_texture_data();
        data.format = fourcc(*b"NV12") as i32;
        let (callback, observed) = lifecycle_recorder();
        let mapped = map_texture(
            data,
            [4, -1, -1, -1],
            GameCaptureImportMode::LinearHostMapped,
            &callback,
        );

        assert!(mapped.is_none());
        let messages = observed_messages(&observed);
        assert!(messages.iter().any(|message| {
            message.contains("does not support fourcc NV12")
                && message.contains("supported host-mapped formats")
                && message.contains("AR24")
                && message.contains("AB4H")
        }));
    }

    #[test]
    fn host_mapped_import_rejects_invalid_stride_and_offset_with_diagnostics() {
        let (callback, observed) = lifecycle_recorder();
        let mut bad_stride = host_mapped_texture_data();
        bad_stride.strides[0] = -1;
        assert!(
            validate_host_mapped_texture_data(bad_stride, [4, -1, -1, -1], &callback).is_none()
        );
        let mut bad_offset = host_mapped_texture_data();
        bad_offset.offsets[0] = -1;
        assert!(
            validate_host_mapped_texture_data(bad_offset, [4, -1, -1, -1], &callback).is_none()
        );

        let messages = observed_messages(&observed);
        assert!(
            messages
                .iter()
                .any(|message| message.contains("invalid stride"))
        );
        assert!(
            messages
                .iter()
                .any(|message| message.contains("invalid offset"))
        );
    }

    #[test]
    fn host_mapped_import_reads_cpu_nv12_and_preserves_source_dmabuf_metadata() {
        let mut bgra = vec![0u8; 16 * 16 * 4];
        for pixel in bgra.as_chunks_mut::<4>().0 {
            *pixel = [0, 0, 255, 255];
        }
        let fd = memfd_with_data("fluxer-host-mapped-texture-test", &bgra);
        let (callback, _observed) = lifecycle_recorder();
        let mut texture = map_texture(
            host_mapped_texture_data(),
            [fd, -1, -1, -1],
            GameCaptureImportMode::LinearHostMapped,
            &callback,
        )
        .expect("mapped texture");

        let frame = texture.read_frame().expect("frame");
        let metadata = frame.dmabuf.expect("source dmabuf metadata");
        assert_eq!(frame.width, 16);
        assert_eq!(frame.height, 16);
        assert_eq!(frame.stride_y, 16);
        assert_eq!(frame.stride_uv, 16);
        assert_eq!(frame.data.len(), 16 * 16 * 3 / 2);
        assert_eq!(metadata.fds[0], fd);
        assert_eq!(metadata.plane_count, 1);
        assert_eq!(metadata.drm_format, DRM_FORMAT_ARGB8888 as u32);
        assert_eq!(metadata.strides, [64, 0, 0, 0]);
        assert_eq!(metadata.offsets, [0, 0, 0, 0]);
        assert_eq!(metadata.device_uuid, None);
    }

    #[test]
    fn parses_client_name_as_c_string() {
        let mut buf = [0u8; TEXTURE_DATA_SIZE];
        buf[0] = CLIENT_DATA_TYPE;
        buf[1..8].copy_from_slice(b"vkcube\0");
        assert_eq!(parse_client_name(&buf), "vkcube");
    }

    #[test]
    fn identifies_rgba_memory_formats_that_need_swizzle() {
        assert_eq!(
            CpuFormat::from_drm(DRM_FORMAT_ARGB8888),
            Some(CpuFormat::Bgra8)
        );
        assert_eq!(
            CpuFormat::from_drm(DRM_FORMAT_XRGB8888),
            Some(CpuFormat::Bgra8)
        );
        assert_eq!(
            CpuFormat::from_drm(DRM_FORMAT_ABGR8888),
            Some(CpuFormat::Rgba8)
        );
        assert_eq!(
            CpuFormat::from_drm(DRM_FORMAT_XBGR8888),
            Some(CpuFormat::Rgba8)
        );
    }

    #[test]
    fn converts_fp16_rgba_row_to_bgra_srgb() {
        let src = [0x00, 0x3c, 0x00, 0x38, 0x00, 0x00, 0x00, 0x3c];
        let mut dst = [0u8; 4];
        rgba16f_row_to_bgra(&src, 1, &mut dst, false);
        assert_eq!(dst[0], 0);
        assert!(dst[1] > 180 && dst[1] < 190);
        assert_eq!(dst[2], 255);
        assert_eq!(dst[3], 255);
    }

    #[test]
    fn converts_abgr10_row_to_bgra() {
        let packed = (3u32 << 30) | (1023u32 << 20) | (512u32 << 10);
        let src = packed.to_le_bytes();
        let mut dst = [0u8; 4];
        a2b10g10r10_row_to_bgra(&src, 1, &mut dst, false);
        assert_eq!(dst[0], 255);
        assert!(dst[1] >= 127);
        assert_eq!(dst[2], 0);
        assert_eq!(dst[3], 255);
    }

    #[test]
    fn control_message_encodes_obs_import_modes() {
        let default = build_control_message(true, GameCaptureImportMode::Default);
        assert_eq!(&default[..4], &[1, 0, 0, 0]);

        let no_modifiers = build_control_message(true, GameCaptureImportMode::NoModifiers);
        assert_eq!(&no_modifiers[..4], &[1, 1, 0, 0]);

        let linear = build_control_message(true, GameCaptureImportMode::Linear);
        assert_eq!(&linear[..4], &[1, 0, 1, 0]);

        let host_mapped = build_control_message(true, GameCaptureImportMode::LinearHostMapped);
        assert_eq!(&host_mapped[..4], &[1, 0, 1, 1]);
    }

    #[test]
    fn auto_import_policy_walks_obs_fallback_ladder() {
        let mut state = GameCaptureImportState::auto();
        assert_eq!(state.current, GameCaptureImportMode::Default);
        assert_eq!(
            state.advance_after_import_failure(),
            Some((
                GameCaptureImportMode::Default,
                GameCaptureImportMode::NoModifiers
            ))
        );
        assert_eq!(
            state.advance_after_import_failure(),
            Some((
                GameCaptureImportMode::NoModifiers,
                GameCaptureImportMode::Linear
            ))
        );
        assert_eq!(
            state.advance_after_import_failure(),
            Some((
                GameCaptureImportMode::Linear,
                GameCaptureImportMode::LinearHostMapped
            ))
        );
        assert_eq!(state.advance_after_import_failure(), None);
    }

    #[test]
    fn frame_deadline_advances_by_exact_interval_multiples() {
        let interval = Duration::from_millis(10);
        let base = Instant::now();

        let (deadline, lagged) = advance_frame_deadline(base, base, interval);
        assert_eq!(deadline, base + interval);
        assert_eq!(lagged, 0);

        let (deadline, lagged) =
            advance_frame_deadline(base, base + Duration::from_millis(3), interval);
        assert_eq!(deadline, base + interval);
        assert_eq!(lagged, 0);

        let (deadline, lagged) =
            advance_frame_deadline(base, base + Duration::from_millis(25), interval);
        assert_eq!(deadline, base + interval * 3);
        assert_eq!(lagged, 2);
    }

    #[test]
    fn frame_deadline_preserves_timeline_phase_across_overruns() {
        let interval = Duration::from_millis(10);
        let base = Instant::now();
        let mut next_frame_at = base;
        let wakeups = [
            Duration::from_micros(900),
            Duration::from_micros(10_700),
            Duration::from_micros(20_400),
        ];
        for wakeup in wakeups {
            let now = base + wakeup;
            let (deadline, lagged) = advance_frame_deadline(next_frame_at, now, interval);
            assert_eq!(lagged, 0);
            next_frame_at = deadline;
        }
        assert_eq!(next_frame_at, base + interval * 3);
    }

    #[test]
    fn frame_deadline_caps_catch_up_count_after_long_idle() {
        let interval = Duration::from_millis(10);
        let base = Instant::now();
        let idle = interval
            .saturating_mul(FRAME_ADVANCE_COUNT_MAX)
            .saturating_mul(2);
        let (deadline, lagged) = advance_frame_deadline(base, base + idle, interval);
        assert_eq!(lagged, FRAME_ADVANCE_COUNT_MAX - 1);
        assert_eq!(
            deadline,
            base + interval.saturating_mul(FRAME_ADVANCE_COUNT_MAX)
        );
    }

    #[test]
    fn poll_timeout_rounds_up_and_caps_at_poll_window() {
        assert_eq!(poll_timeout_for_remaining(Duration::from_nanos(1)), 1);
        assert_eq!(poll_timeout_for_remaining(Duration::from_micros(999)), 1);
        assert_eq!(poll_timeout_for_remaining(Duration::from_micros(1_001)), 2);
        assert_eq!(poll_timeout_for_remaining(Duration::from_millis(5)), 5);
        assert_eq!(
            poll_timeout_for_remaining(Duration::from_secs(1)),
            POLL_TIMEOUT_MS
        );
    }

    #[test]
    fn import_mode_env_parses_nvidia_debug_shortcuts() {
        assert_eq!(
            GameCaptureImportState::from_env_value("zero-copy").current,
            GameCaptureImportMode::Default
        );
        assert_eq!(
            GameCaptureImportState::from_env_value("safe").current,
            GameCaptureImportMode::LinearHostMapped
        );
        assert_eq!(
            GameCaptureImportState::from_env_value("unknown").current,
            GameCaptureImportMode::Default
        );
    }
}
