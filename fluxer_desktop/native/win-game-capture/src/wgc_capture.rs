// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::{Arc, atomic::Ordering};

use windows::Graphics::Capture::{
    Direct3D11CaptureFrame, Direct3D11CaptureFramePool, GraphicsCaptureItem, GraphicsCaptureSession,
};
use windows::Graphics::DirectX::Direct3D11::IDirect3DDevice;
use windows::Graphics::DirectX::DirectXPixelFormat;
use windows::Graphics::SizeInt32;
use windows::Win32::Foundation::{HWND, LPARAM, RECT, RPC_E_CHANGED_MODE};
use windows::Win32::Graphics::Direct3D11::{
    D3D11_BIND_SHADER_RESOURCE, D3D11_BOX, D3D11_TEXTURE2D_DESC, D3D11_USAGE_DEFAULT, ID3D11Device,
    ID3D11DeviceContext, ID3D11Resource, ID3D11Texture2D,
};
use windows::Win32::Graphics::Dxgi::Common::{
    DXGI_FORMAT, DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_FORMAT_R16G16B16A16_FLOAT, DXGI_SAMPLE_DESC,
};
use windows::Win32::Graphics::Dxgi::IDXGIDevice;
use windows::Win32::Graphics::Gdi::{EnumDisplayMonitors, HDC, HMONITOR};
use windows::Win32::System::WinRT::Direct3D11::{
    CreateDirect3D11DeviceFromDXGIDevice, IDirect3DDxgiInterfaceAccess,
};
use windows::Win32::System::WinRT::Graphics::Capture::IGraphicsCaptureItemInterop;
use windows::Win32::System::WinRT::{RO_INIT_MULTITHREADED, RoInitialize};
use windows::Win32::UI::WindowsAndMessaging::{IsWindow, IsWindowVisible};
use windows::core::{BOOL, Interface};

use crate::dxgi_capture::{
    SharedTextureOutput, capture_timestamp_us, create_shared_output_texture,
    pacing_sleep_and_next_deadline, resolve_output_size,
};
use crate::nv12_gpu::Nv12GpuConverter;
use crate::stall::{NoFrameStallTracker, StallSignal};
use crate::{
    CaptureInner, emit_lifecycle, emit_shared_texture_frame, note_media_frame_without_sink,
    resolve_frame_sink,
};

const WGC_FRAME_POOL_BUFFERS: i32 = 2;
const WGC_FRAME_DRAIN_LIMIT: u32 = 4;
const WGC_MONITOR_ENUM_LIMIT: usize = 16;
const WGC_STALL_THRESHOLD: std::time::Duration = std::time::Duration::from_millis(3000);

fn ensure_winrt_initialized() {
    let result = unsafe { RoInitialize(RO_INIT_MULTITHREADED) };
    if let Err(error) = result {
        assert!(
            error.code() == RPC_E_CHANGED_MODE,
            "RoInitialize failed unexpectedly: {error:?}"
        );
    }
}

pub fn wgc_capture_supported() -> bool {
    ensure_winrt_initialized();
    GraphicsCaptureSession::IsSupported().unwrap_or(false)
}

#[derive(Clone, Copy)]
pub enum WgcCaptureTarget {
    Window(HWND),
    Monitor(HMONITOR),
}

impl WgcCaptureTarget {
    fn create_item(
        self,
        interop: &IGraphicsCaptureItemInterop,
    ) -> Result<GraphicsCaptureItem, String> {
        match self {
            Self::Window(hwnd) => unsafe { interop.CreateForWindow(hwnd) }
                .map_err(|e| format!("IGraphicsCaptureItemInterop::CreateForWindow: {e}")),
            Self::Monitor(monitor) => unsafe { interop.CreateForMonitor(monitor) }
                .map_err(|e| format!("IGraphicsCaptureItemInterop::CreateForMonitor: {e}")),
        }
    }

    fn is_alive(self) -> bool {
        match self {
            Self::Window(hwnd) => unsafe { IsWindow(Some(hwnd)) }.as_bool(),
            Self::Monitor(monitor) => !monitor.is_invalid(),
        }
    }

    fn expects_frames(self) -> bool {
        match self {
            Self::Window(hwnd) => unsafe { IsWindowVisible(hwnd) }.as_bool(),
            Self::Monitor(monitor) => !monitor.is_invalid(),
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Window(_) => "window",
            Self::Monitor(_) => "monitor",
        }
    }

    fn closed_message(self) -> &'static str {
        match self {
            Self::Window(_) => "window closed",
            Self::Monitor(_) => "monitor capture target unavailable",
        }
    }
}

pub fn parse_monitor_source_id(source_id: &str, source_kind: &str) -> Option<HMONITOR> {
    if source_kind != "screen" {
        return None;
    }
    let mut parts = source_id.split(':');
    if parts.next()? != "screen" {
        return None;
    }
    let index = parts.next()?.parse::<usize>().ok()?;
    let sub_id = parts.next()?;
    if sub_id != "0" && sub_id != "1" {
        return None;
    }
    if parts.next().is_some() || index >= WGC_MONITOR_ENUM_LIMIT {
        return None;
    }
    enumerate_monitors().get(index).copied()
}

fn enumerate_monitors() -> Vec<HMONITOR> {
    struct MonitorEnumState {
        monitors: Vec<HMONITOR>,
    }

    unsafe extern "system" fn enum_monitor(
        monitor: HMONITOR,
        _hdc: HDC,
        _rect: *mut RECT,
        param: LPARAM,
    ) -> BOOL {
        let state = unsafe { &mut *(param.0 as *mut MonitorEnumState) };
        if state.monitors.len() >= WGC_MONITOR_ENUM_LIMIT {
            return BOOL(0);
        }
        if !monitor.is_invalid() {
            state.monitors.push(monitor);
        }
        BOOL(1)
    }

    let mut state = MonitorEnumState {
        monitors: Vec::with_capacity(WGC_MONITOR_ENUM_LIMIT),
    };
    unsafe {
        let _ = EnumDisplayMonitors(
            None,
            None,
            Some(enum_monitor),
            LPARAM((&mut state as *mut MonitorEnumState) as isize),
        );
    }
    assert!(
        state.monitors.len() <= WGC_MONITOR_ENUM_LIMIT,
        "monitor enumeration bounded"
    );
    state.monitors
}

pub struct WgcCaptureSession {
    device: ID3D11Device,
    context: ID3D11DeviceContext,
    d3d_device: IDirect3DDevice,
    item: GraphicsCaptureItem,
    target: WgcCaptureTarget,
    output_width: u32,
    output_height: u32,
    requested_width: Option<u32>,
    requested_height: Option<u32>,
}

unsafe impl Send for WgcCaptureSession {}

impl WgcCaptureSession {
    pub fn new(
        hwnd: HWND,
        requested_width: Option<u32>,
        requested_height: Option<u32>,
    ) -> Result<Self, String> {
        Self::new_for_target(
            WgcCaptureTarget::Window(hwnd),
            requested_width,
            requested_height,
        )
    }

    pub fn new_monitor(
        monitor: HMONITOR,
        requested_width: Option<u32>,
        requested_height: Option<u32>,
    ) -> Result<Self, String> {
        Self::new_for_target(
            WgcCaptureTarget::Monitor(monitor),
            requested_width,
            requested_height,
        )
    }

    fn new_for_target(
        target: WgcCaptureTarget,
        requested_width: Option<u32>,
        requested_height: Option<u32>,
    ) -> Result<Self, String> {
        ensure_winrt_initialized();
        let (device, context) = crate::game_capture::create_shared_texture_device(None)?;
        let dxgi_device: IDXGIDevice = device
            .cast()
            .map_err(|e| format!("IDXGIDevice cast: {e}"))?;
        let inspectable = unsafe { CreateDirect3D11DeviceFromDXGIDevice(&dxgi_device) }
            .map_err(|e| format!("CreateDirect3D11DeviceFromDXGIDevice: {e}"))?;
        let d3d_device: IDirect3DDevice = inspectable
            .cast()
            .map_err(|e| format!("IDirect3DDevice cast: {e}"))?;
        let interop = windows::core::factory::<GraphicsCaptureItem, IGraphicsCaptureItemInterop>()
            .map_err(|e| format!("GraphicsCaptureItem interop factory: {e}"))?;
        let item = target.create_item(&interop)?;
        let size = item
            .Size()
            .map_err(|e| format!("GraphicsCaptureItem.Size: {e}"))?;
        let content_width = size.Width.max(1) as u32;
        let content_height = size.Height.max(1) as u32;
        let (output_width, output_height) = resolve_output_size(
            content_width,
            content_height,
            requested_width,
            requested_height,
        );
        assert!(output_width > 0, "WGC output width positive");
        assert!(output_height > 0, "WGC output height positive");
        Ok(Self {
            device,
            context,
            d3d_device,
            item,
            target,
            output_width,
            output_height,
            requested_width,
            requested_height,
        })
    }

    pub fn capture_width(&self) -> u32 {
        assert!(self.output_width > 0, "WGC output width positive");
        assert!(self.output_height > 0, "WGC output height positive");
        self.output_width
    }

    pub fn capture_height(&self) -> u32 {
        assert!(self.output_height > 0, "WGC output height positive");
        assert!(self.output_width > 0, "WGC output width positive");
        self.output_height
    }
}

struct WgcState {
    frame_pool: Direct3D11CaptureFramePool,
    session: GraphicsCaptureSession,
    output_pipeline: Option<WgcOutputPipeline>,
    pixel_format: DirectXPixelFormat,
    content_width: u32,
    content_height: u32,
    out_w: u32,
    out_h: u32,
}

impl WgcState {
    fn close(self) {
        let _ = self.session.Close();
        let _ = self.frame_pool.Close();
    }
}

struct WgcNv12Pipeline {
    input_resource: ID3D11Resource,
    converter: Nv12GpuConverter,
}

enum WgcOutputPipeline {
    Bgra(SharedTextureOutput),
    Nv12(WgcNv12Pipeline),
}

enum WgcFrameResult {
    Ok,
    NoFrame,
    Resized { width: u32, height: u32 },
    Error(String),
}

enum LoopStep {
    Paced,
    Restart,
}

struct WgcLoopContext {
    device: ID3D11Device,
    context: ID3D11DeviceContext,
    d3d_device: IDirect3DDevice,
    item: GraphicsCaptureItem,
    target: WgcCaptureTarget,
    requested_width: Option<u32>,
    requested_height: Option<u32>,
}

fn wgc_loop_context(inner: &Arc<CaptureInner>) -> Option<WgcLoopContext> {
    let guard = inner.wgc_session.lock();
    let session = guard.as_ref()?;
    Some(WgcLoopContext {
        device: session.device.clone(),
        context: session.context.clone(),
        d3d_device: session.d3d_device.clone(),
        item: session.item.clone(),
        target: session.target,
        requested_width: session.requested_width,
        requested_height: session.requested_height,
    })
}

fn teardown_wgc_state(wgc_state: &mut Option<WgcState>) {
    if let Some(state) = wgc_state.take() {
        state.close();
    }
}

fn sleep_with_backoff(backoff: &mut std::time::Duration) {
    assert!(
        *backoff >= std::time::Duration::from_millis(100),
        "backoff at least base interval"
    );
    assert!(
        *backoff <= std::time::Duration::from_secs(2),
        "backoff bounded"
    );
    std::thread::sleep(*backoff);
    *backoff = (*backoff * 2).min(std::time::Duration::from_secs(2));
}

pub fn capture_loop(inner: &Arc<CaptureInner>, frame_interval: std::time::Duration) {
    ensure_winrt_initialized();
    let Some(ctx) = wgc_loop_context(inner) else {
        emit_lifecycle(inner, "closed-clean", "no WGC session");
        return;
    };

    let capture_id = inner.capture_id.lock().clone();
    let mut wgc_state: Option<WgcState> = None;
    let mut recreate_backoff = std::time::Duration::from_millis(100);
    let capture_start = std::time::Instant::now();
    let mut next_frame_deadline = capture_start + frame_interval;
    let mut frames_dropped_coalesced: u64 = 0;
    let mut stall_tracker = NoFrameStallTracker::new(WGC_STALL_THRESHOLD, capture_start);

    while inner.running.load(Ordering::Acquire) {
        if !ctx.target.is_alive() {
            emit_lifecycle(inner, "closed", ctx.target.closed_message());
            break;
        }

        if wgc_state.is_none() {
            match setup_wgc_state(&ctx) {
                Ok(state) => {
                    wgc_state = Some(state);
                }
                Err(e) => {
                    if !inner.running.load(Ordering::Acquire) {
                        break;
                    }
                    emit_lifecycle(
                        inner,
                        "error",
                        &format!("Failed to create WGC capture session: {e}"),
                    );
                    sleep_with_backoff(&mut recreate_backoff);
                    continue;
                }
            }
        }

        let Some(state) = wgc_state.as_mut() else {
            continue;
        };
        let result = poll_and_emit_frame(
            inner,
            &ctx.context,
            state,
            capture_id.as_deref(),
            capture_start,
            &mut frames_dropped_coalesced,
        );
        let signal = stall_tracker.observe(
            std::time::Instant::now(),
            matches!(result, WgcFrameResult::Ok),
            ctx.target.expects_frames(),
        );
        emit_stall_signal(inner, ctx.target, signal);
        match handle_frame_result(inner, &ctx, &mut wgc_state, result, &mut recreate_backoff) {
            LoopStep::Paced => {}
            LoopStep::Restart => continue,
        }

        let now = std::time::Instant::now();
        let (sleep_duration, deadline) =
            pacing_sleep_and_next_deadline(now, next_frame_deadline, frame_interval);
        next_frame_deadline = deadline;
        if sleep_duration > std::time::Duration::ZERO {
            std::thread::sleep(sleep_duration);
        }
    }

    teardown_wgc_state(&mut wgc_state);
    inner.running.store(false, Ordering::Release);
    emit_lifecycle(inner, "closed-clean", "capture stopped");
}

fn emit_stall_signal(inner: &Arc<CaptureInner>, target: WgcCaptureTarget, signal: StallSignal) {
    match signal {
        StallSignal::Quiet => {}
        StallSignal::Stalled {
            frames_seen,
            elapsed_ms,
        } => {
            emit_lifecycle(
                inner,
                "stalled",
                &stall_detail(target, frames_seen, elapsed_ms),
            );
        }
        StallSignal::Resumed { stalled_for_ms } => {
            emit_lifecycle(
                inner,
                "diagnostic",
                &format!(
                    "WGC {} capture frames resumed after {stalled_for_ms}ms without a frame",
                    target.label()
                ),
            );
        }
    }
}

fn stall_detail(target: WgcCaptureTarget, frames_seen: u64, elapsed_ms: u64) -> String {
    let label = target.label();
    if frames_seen == 0 {
        return format!(
            "WGC {label} capture stalled: the target is still alive but Windows Graphics Capture \
             has delivered no frame in the first {elapsed_ms}ms; the stream is blank"
        );
    }
    format!(
        "WGC {label} capture stalled: no new frame for {elapsed_ms}ms after {frames_seen} frames; \
         the target may be paused, occluded, or no longer rendering"
    )
}

fn handle_frame_result(
    inner: &Arc<CaptureInner>,
    ctx: &WgcLoopContext,
    wgc_state: &mut Option<WgcState>,
    result: WgcFrameResult,
    recreate_backoff: &mut std::time::Duration,
) -> LoopStep {
    match result {
        WgcFrameResult::Ok => {
            *recreate_backoff = std::time::Duration::from_millis(100);
            LoopStep::Paced
        }
        WgcFrameResult::NoFrame => LoopStep::Paced,
        WgcFrameResult::Resized { width, height } => {
            let Some(state) = wgc_state.as_mut() else {
                return LoopStep::Restart;
            };
            match resize_wgc_state(ctx, state, width, height) {
                Ok(()) => LoopStep::Paced,
                Err(e) => {
                    emit_lifecycle(
                        inner,
                        "error",
                        &format!("WGC frame pool resize failed: {e}"),
                    );
                    teardown_wgc_state(wgc_state);
                    sleep_with_backoff(recreate_backoff);
                    LoopStep::Restart
                }
            }
        }
        WgcFrameResult::Error(e) => {
            emit_lifecycle(inner, "error", &e);
            {
                let mut guard = inner.fallback.lock();
                if let Some(tracker) = guard.as_mut() {
                    let _ = tracker.observe(crate::fallback::FailureSignature::DeviceLost);
                }
            }
            teardown_wgc_state(wgc_state);
            sleep_with_backoff(recreate_backoff);
            LoopStep::Restart
        }
    }
}

fn setup_wgc_state(ctx: &WgcLoopContext) -> Result<WgcState, String> {
    let size = ctx
        .item
        .Size()
        .map_err(|e| format!("GraphicsCaptureItem.Size: {e}"))?;
    let content_width = size.Width.max(1) as u32;
    let content_height = size.Height.max(1) as u32;
    let (out_w, out_h) = resolve_output_size(
        content_width,
        content_height,
        ctx.requested_width,
        ctx.requested_height,
    );
    create_wgc_state_for_format(
        ctx,
        DirectXPixelFormat::R16G16B16A16Float,
        content_width,
        content_height,
        out_w,
        out_h,
    )
    .or_else(|_| {
        create_wgc_state_for_format(
            ctx,
            DirectXPixelFormat::B8G8R8A8UIntNormalized,
            content_width,
            content_height,
            out_w,
            out_h,
        )
    })
}

fn create_wgc_state_for_format(
    ctx: &WgcLoopContext,
    pixel_format: DirectXPixelFormat,
    content_width: u32,
    content_height: u32,
    out_w: u32,
    out_h: u32,
) -> Result<WgcState, String> {
    let output_pipeline = create_wgc_output_pipeline(
        ctx,
        pixel_format,
        content_width,
        content_height,
        out_w,
        out_h,
    )?;
    let frame_pool = Direct3D11CaptureFramePool::CreateFreeThreaded(
        &ctx.d3d_device,
        pixel_format,
        WGC_FRAME_POOL_BUFFERS,
        SizeInt32 {
            Width: content_width as i32,
            Height: content_height as i32,
        },
    )
    .map_err(|e| format!("Direct3D11CaptureFramePool::CreateFreeThreaded: {e}"))?;
    let session = frame_pool
        .CreateCaptureSession(&ctx.item)
        .map_err(|e| format!("Direct3D11CaptureFramePool.CreateCaptureSession: {e}"))?;
    session
        .StartCapture()
        .map_err(|e| format!("GraphicsCaptureSession.StartCapture: {e}"))?;
    Ok(WgcState {
        frame_pool,
        session,
        output_pipeline,
        pixel_format,
        content_width,
        content_height,
        out_w,
        out_h,
    })
}

fn resize_wgc_state(
    ctx: &WgcLoopContext,
    state: &mut WgcState,
    width: u32,
    height: u32,
) -> Result<(), String> {
    assert!(width > 0, "WGC resize width positive");
    assert!(height > 0, "WGC resize height positive");
    let (out_w, out_h) =
        resolve_output_size(width, height, ctx.requested_width, ctx.requested_height);
    match resize_wgc_state_for_format(ctx, state, state.pixel_format, width, height, out_w, out_h) {
        Ok(()) => Ok(()),
        Err(first_error) if state.pixel_format == DirectXPixelFormat::R16G16B16A16Float => {
            resize_wgc_state_for_format(
                ctx,
                state,
                DirectXPixelFormat::B8G8R8A8UIntNormalized,
                width,
                height,
                out_w,
                out_h,
            )
            .map_err(|fallback_error| {
                format!(
                    "HDR resize failed ({first_error}); BGRA fallback failed ({fallback_error})"
                )
            })
        }
        Err(first_error) => Err(first_error),
    }
}

fn resize_wgc_state_for_format(
    ctx: &WgcLoopContext,
    state: &mut WgcState,
    pixel_format: DirectXPixelFormat,
    width: u32,
    height: u32,
    out_w: u32,
    out_h: u32,
) -> Result<(), String> {
    let output_pipeline =
        create_wgc_output_pipeline(ctx, pixel_format, width, height, out_w, out_h)?;
    state
        .frame_pool
        .Recreate(
            &ctx.d3d_device,
            pixel_format,
            WGC_FRAME_POOL_BUFFERS,
            SizeInt32 {
                Width: width as i32,
                Height: height as i32,
            },
        )
        .map_err(|e| format!("Direct3D11CaptureFramePool.Recreate: {e}"))?;
    state.content_width = width;
    state.content_height = height;
    state.out_w = out_w;
    state.out_h = out_h;
    state.pixel_format = pixel_format;
    state.output_pipeline = output_pipeline;
    Ok(())
}

fn create_wgc_output_pipeline(
    ctx: &WgcLoopContext,
    pixel_format: DirectXPixelFormat,
    content_width: u32,
    content_height: u32,
    out_w: u32,
    out_h: u32,
) -> Result<Option<WgcOutputPipeline>, String> {
    if pixel_format == DirectXPixelFormat::R16G16B16A16Float {
        return create_wgc_nv12_pipeline(
            ctx,
            DXGI_FORMAT_R16G16B16A16_FLOAT,
            content_width,
            content_height,
            out_w,
            out_h,
            crate::hdr::SourceFormat::Rgba16Float { hdr: true },
        )
        .map(WgcOutputPipeline::Nv12)
        .map(Some);
    }
    if out_w != content_width || out_h != content_height {
        return create_wgc_nv12_pipeline(
            ctx,
            DXGI_FORMAT_B8G8R8A8_UNORM,
            content_width,
            content_height,
            out_w,
            out_h,
            crate::hdr::SourceFormat::Bgra8,
        )
        .map(WgcOutputPipeline::Nv12)
        .map(Some);
    }
    Ok(
        create_shared_output_texture(&ctx.device, content_width, content_height)
            .ok()
            .map(WgcOutputPipeline::Bgra),
    )
}

fn create_wgc_nv12_pipeline(
    ctx: &WgcLoopContext,
    input_format: DXGI_FORMAT,
    content_width: u32,
    content_height: u32,
    out_w: u32,
    out_h: u32,
    source_format: crate::hdr::SourceFormat,
) -> Result<WgcNv12Pipeline, String> {
    assert!(content_width > 0, "WGC NV12 input width positive");
    assert!(content_height > 0, "WGC NV12 input height positive");
    let input_desc = D3D11_TEXTURE2D_DESC {
        Width: content_width,
        Height: content_height,
        MipLevels: 1,
        ArraySize: 1,
        Format: input_format,
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        },
        Usage: D3D11_USAGE_DEFAULT,
        BindFlags: D3D11_BIND_SHADER_RESOURCE.0 as u32,
        CPUAccessFlags: 0,
        MiscFlags: 0,
    };
    let mut input_texture = None;
    unsafe {
        ctx.device
            .CreateTexture2D(&input_desc, None, Some(&mut input_texture))
    }
    .map_err(|e| format!("CreateTexture2D WGC NV12 input: {e}"))?;
    let input_texture =
        input_texture.ok_or_else(|| "CreateTexture2D WGC NV12 input returned null".to_string())?;
    let input_resource: ID3D11Resource = input_texture
        .cast()
        .map_err(|e| format!("ID3D11Resource WGC NV12 input cast: {e}"))?;
    let converter = Nv12GpuConverter::new(
        &ctx.device,
        &ctx.context,
        &input_texture,
        content_width,
        content_height,
        out_w,
        out_h,
        source_format,
    )
    .ok_or_else(|| "WGC NV12 converter unavailable".to_string())?;
    Ok(WgcNv12Pipeline {
        input_resource,
        converter,
    })
}

fn poll_and_emit_frame(
    inner: &Arc<CaptureInner>,
    context: &ID3D11DeviceContext,
    state: &mut WgcState,
    capture_id: Option<&str>,
    capture_start: std::time::Instant,
    frames_dropped_coalesced: &mut u64,
) -> WgcFrameResult {
    let mut newest: Option<Direct3D11CaptureFrame> = None;
    let mut drained: u32 = 0;
    while drained < WGC_FRAME_DRAIN_LIMIT {
        let Ok(frame) = state.frame_pool.TryGetNextFrame() else {
            break;
        };
        if let Some(previous) = newest.replace(frame) {
            *frames_dropped_coalesced += 1;
            let _ = previous.Close();
        }
        drained += 1;
    }
    assert!(drained <= WGC_FRAME_DRAIN_LIMIT, "frame drain bounded");
    let Some(frame) = newest else {
        return WgcFrameResult::NoFrame;
    };
    let result = emit_wgc_frame(inner, context, state, capture_id, &frame, capture_start);
    let _ = frame.Close();
    result
}

fn wgc_frame_source_resource(frame: &Direct3D11CaptureFrame) -> Result<ID3D11Resource, String> {
    let surface = frame
        .Surface()
        .map_err(|e| format!("Direct3D11CaptureFrame.Surface: {e}"))?;
    let access: IDirect3DDxgiInterfaceAccess = surface
        .cast()
        .map_err(|e| format!("IDirect3DDxgiInterfaceAccess cast: {e}"))?;
    let source_texture: ID3D11Texture2D =
        unsafe { access.GetInterface() }.map_err(|e| format!("WGC surface GetInterface: {e}"))?;
    source_texture
        .cast()
        .map_err(|e| format!("ID3D11Resource WGC surface cast: {e}"))
}

fn emit_wgc_frame(
    inner: &Arc<CaptureInner>,
    context: &ID3D11DeviceContext,
    state: &mut WgcState,
    capture_id: Option<&str>,
    frame: &Direct3D11CaptureFrame,
    capture_start: std::time::Instant,
) -> WgcFrameResult {
    let content = match frame.ContentSize() {
        Ok(size) => size,
        Err(e) => {
            return WgcFrameResult::Error(format!("Direct3D11CaptureFrame.ContentSize: {e}"));
        }
    };
    let content_width = content.Width.max(1) as u32;
    let content_height = content.Height.max(1) as u32;
    if content_width != state.content_width || content_height != state.content_height {
        return WgcFrameResult::Resized {
            width: content_width,
            height: content_height,
        };
    }
    let Some(frame_sink) = resolve_frame_sink(inner, capture_id) else {
        note_media_frame_without_sink(
            inner,
            "WGC frame dropped because no native frame sink is registered",
        );
        return WgcFrameResult::Ok;
    };
    let source_resource = match wgc_frame_source_resource(frame) {
        Ok(resource) => resource,
        Err(e) => return WgcFrameResult::Error(e),
    };
    let timestamp_us = capture_timestamp_us(capture_start);
    let Some(output_pipeline) = state.output_pipeline.as_mut() else {
        return WgcFrameResult::Error("WGC shared texture output unavailable".into());
    };
    match output_pipeline {
        WgcOutputPipeline::Bgra(shared_output) => emit_wgc_bgra_frame(
            inner,
            context,
            &frame_sink,
            shared_output,
            &source_resource,
            content_width,
            content_height,
            timestamp_us,
        ),
        WgcOutputPipeline::Nv12(pipeline) => emit_wgc_nv12_frame(
            inner,
            context,
            &frame_sink,
            pipeline,
            &source_resource,
            content_width,
            content_height,
            timestamp_us,
        ),
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_wgc_bgra_frame(
    inner: &Arc<CaptureInner>,
    context: &ID3D11DeviceContext,
    frame_sink: &crate::FrameSinkRef,
    shared_output: &mut SharedTextureOutput,
    source_resource: &ID3D11Resource,
    content_width: u32,
    content_height: u32,
    timestamp_us: i64,
) -> WgcFrameResult {
    if shared_output.width != content_width || shared_output.height != content_height {
        return WgcFrameResult::Error(
            "WGC BGRA shared output size does not match content size".into(),
        );
    }
    let slot_index = shared_output.next_slot_index();
    let slot = &shared_output.slots[slot_index];
    let output_resource: ID3D11Resource = match slot.texture.cast() {
        Ok(resource) => resource,
        Err(e) => return WgcFrameResult::Error(format!("ID3D11Resource shared output cast: {e}")),
    };
    let src_box = D3D11_BOX {
        left: 0,
        top: 0,
        front: 0,
        right: content_width,
        bottom: content_height,
        back: 1,
    };
    unsafe {
        context.CopySubresourceRegion(
            &output_resource,
            0,
            0,
            0,
            0,
            source_resource,
            0,
            Some(&src_box),
        );
        context.Flush();
    }
    let _ = emit_shared_texture_frame(
        inner,
        frame_sink,
        slot.handle,
        shared_output.width,
        shared_output.height,
        shared_output.dxgi_format,
        timestamp_us,
    );
    WgcFrameResult::Ok
}

#[allow(clippy::too_many_arguments)]
fn emit_wgc_nv12_frame(
    inner: &Arc<CaptureInner>,
    context: &ID3D11DeviceContext,
    frame_sink: &crate::FrameSinkRef,
    pipeline: &mut WgcNv12Pipeline,
    source_resource: &ID3D11Resource,
    content_width: u32,
    content_height: u32,
    timestamp_us: i64,
) -> WgcFrameResult {
    let src_box = D3D11_BOX {
        left: 0,
        top: 0,
        front: 0,
        right: content_width,
        bottom: content_height,
        back: 1,
    };
    unsafe {
        context.CopySubresourceRegion(
            &pipeline.input_resource,
            0,
            0,
            0,
            0,
            source_resource,
            0,
            Some(&src_box),
        );
    }
    let frame = match pipeline.converter.convert_shared_texture() {
        Ok(frame) => frame,
        Err(error) => return WgcFrameResult::Error(error),
    };
    let _ = emit_shared_texture_frame(
        inner,
        frame_sink,
        frame.handle,
        frame.width,
        frame.height,
        frame.dxgi_format,
        timestamp_us,
    );
    WgcFrameResult::Ok
}
