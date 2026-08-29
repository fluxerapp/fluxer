// SPDX-License-Identifier: AGPL-3.0-or-later

#[cfg(target_os = "windows")]
use windows::{
    Win32::{
        Foundation::{HMODULE, HWND, RECT},
        Graphics::{
            Direct3D::D3D_DRIVER_TYPE_UNKNOWN,
            Direct3D11::{
                D3D11_BOX, D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_RESOURCE_MISC_SHARED,
                D3D11_SDK_VERSION, D3D11_TEXTURE2D_DESC, D3D11_USAGE_DEFAULT, D3D11CreateDevice,
                ID3D11Device, ID3D11DeviceContext, ID3D11Resource, ID3D11Texture2D,
            },
            Dxgi::{
                Common::{
                    DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_MODE_ROTATION, DXGI_MODE_ROTATION_IDENTITY,
                    DXGI_MODE_ROTATION_UNSPECIFIED, DXGI_SAMPLE_DESC,
                },
                CreateDXGIFactory1, DXGI_ERROR_ACCESS_LOST, DXGI_ERROR_MORE_DATA,
                DXGI_ERROR_WAIT_TIMEOUT, DXGI_OUTDUPL_FRAME_INFO, DXGI_OUTDUPL_POINTER_SHAPE_INFO,
                DXGI_OUTDUPL_POINTER_SHAPE_TYPE_COLOR,
                DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MASKED_COLOR,
                DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MONOCHROME, IDXGIAdapter, IDXGIAdapter1,
                IDXGIDevice, IDXGIFactory1, IDXGIOutput, IDXGIOutput1, IDXGIOutputDuplication,
                IDXGIResource,
            },
            Gdi::{HMONITOR, MONITOR_DEFAULTTONULL, MonitorFromWindow},
        },
        UI::WindowsAndMessaging::{GetWindowRect, IsWindow},
    },
    core::Interface,
};

#[cfg(target_os = "windows")]
use crate::{
    CaptureInner, emit_lifecycle, emit_shared_texture_frame, note_media_frame_without_sink,
    resolve_frame_sink,
};

#[cfg(target_os = "windows")]
use std::sync::{Arc, atomic::Ordering};

#[cfg(target_os = "windows")]
pub fn parse_window_source_id(source_id: &str, source_kind: &str) -> Option<HWND> {
    if source_kind != "window" {
        return None;
    }
    let hwnd_str = source_id.strip_prefix("window:")?;
    let hwnd_token = hwnd_str.split(':').next()?;
    let hwnd_num = parse_hwnd_token(hwnd_token)?;
    let hwnd = HWND(hwnd_num as *mut _);
    if unsafe { IsWindow(Some(hwnd)) }.as_bool() {
        Some(hwnd)
    } else {
        None
    }
}

#[cfg(target_os = "windows")]
fn parse_hwnd_token(token: &str) -> Option<isize> {
    if let Some(hex) = token
        .strip_prefix("0x")
        .or_else(|| token.strip_prefix("0X"))
    {
        return isize::from_str_radix(hex, 16).ok();
    }
    token.parse().ok()
}

#[cfg(target_os = "windows")]
fn validate_output_rotation(rotation: DXGI_MODE_ROTATION) -> Result<(), String> {
    if rotation.0 == DXGI_MODE_ROTATION_UNSPECIFIED.0 || rotation.0 == DXGI_MODE_ROTATION_IDENTITY.0
    {
        return Ok(());
    }
    Err(format!(
        "Rotated DXGI outputs are not supported by this fallback (rotation={})",
        rotation.0
    ))
}

#[cfg(target_os = "windows")]
pub struct DxgiCaptureSession {
    device: ID3D11Device,
    context: ID3D11DeviceContext,
    hwnd: HWND,
    capture_width: u32,
    capture_height: u32,
    requested_width: Option<u32>,
    requested_height: Option<u32>,
}

#[cfg(target_os = "windows")]
unsafe impl Send for DxgiCaptureSession {}

#[cfg(target_os = "windows")]
#[derive(Default)]
struct PointerShapeCache {
    buffer: Vec<u8>,
    info: Option<DXGI_OUTDUPL_POINTER_SHAPE_INFO>,
}

#[cfg(target_os = "windows")]
enum PointerShapeUpdate {
    Ok,
    AccessLost,
    Error(String),
}

#[cfg(target_os = "windows")]
struct DuplicationState {
    duplication: IDXGIOutputDuplication,
    monitor: HMONITOR,
    monitor_rect: RECT,
    shared_output: Option<SharedTextureOutput>,
    cap_w: u32,
    cap_h: u32,
    out_w: u32,
    out_h: u32,
}

#[cfg(target_os = "windows")]
pub(crate) const SHARED_OUTPUT_SLOT_COUNT: usize = 3;

#[cfg(target_os = "windows")]
pub(crate) struct SharedOutputSlot {
    pub(crate) texture: ID3D11Texture2D,
    pub(crate) handle: u64,
}

#[cfg(target_os = "windows")]
pub(crate) struct SharedTextureOutput {
    pub(crate) slots: [SharedOutputSlot; SHARED_OUTPUT_SLOT_COUNT],
    pub(crate) slot_cursor: usize,
    pub(crate) width: u32,
    pub(crate) height: u32,
    pub(crate) dxgi_format: u32,
}

#[cfg(target_os = "windows")]
impl SharedTextureOutput {
    pub(crate) fn next_slot_index(&mut self) -> usize {
        assert!(
            self.slot_cursor < SHARED_OUTPUT_SLOT_COUNT,
            "slot cursor in range"
        );
        assert!(self.width > 0, "shared output width positive");
        let slot_index = self.slot_cursor;
        self.slot_cursor = (slot_index + 1) % SHARED_OUTPUT_SLOT_COUNT;
        slot_index
    }
}

#[cfg(target_os = "windows")]
impl DxgiCaptureSession {
    pub fn new(
        hwnd: HWND,
        requested_width: Option<u32>,
        requested_height: Option<u32>,
    ) -> Result<Self, String> {
        let adapter = find_dxgi_adapter_for_window(hwnd)?;
        let (device, context) = create_d3d11_device(&adapter)?;

        let mut window_rect = RECT::default();
        unsafe {
            GetWindowRect(hwnd, &mut window_rect).map_err(|e| format!("GetWindowRect: {e}"))?;
        }
        let width = (window_rect.right - window_rect.left).max(1) as u32;
        let height = (window_rect.bottom - window_rect.top).max(1) as u32;
        let (capture_width, capture_height) =
            resolve_output_size(width, height, requested_width, requested_height);

        Ok(Self {
            device,
            context,
            hwnd,
            capture_width,
            capture_height,
            requested_width,
            requested_height,
        })
    }

    pub fn capture_width(&self) -> u32 {
        self.capture_width
    }

    pub fn capture_height(&self) -> u32 {
        self.capture_height
    }
}

#[cfg(target_os = "windows")]
fn find_dxgi_adapter_for_window(hwnd: HWND) -> Result<IDXGIAdapter1, String> {
    let target_monitor = unsafe { MonitorFromWindow(hwnd, MONITOR_DEFAULTTONULL) };
    if target_monitor.is_invalid() {
        return Err("MonitorFromWindow returned null".into());
    }
    let factory = unsafe { CreateDXGIFactory1::<IDXGIFactory1>() }
        .map_err(|e| format!("CreateDXGIFactory1: {e}"))?;
    let mut adapter_index = 0u32;
    loop {
        let adapter = match unsafe { factory.EnumAdapters1(adapter_index) } {
            Ok(adapter) => adapter,
            Err(_) => break,
        };
        adapter_index += 1;
        let mut output_index = 0u32;
        loop {
            let output: IDXGIOutput = match unsafe { adapter.EnumOutputs(output_index) } {
                Ok(output) => output,
                Err(_) => break,
            };
            output_index += 1;
            let desc = unsafe { output.GetDesc().map_err(|e| format!("GetDesc: {e}"))? };
            if desc.Monitor == target_monitor {
                validate_output_rotation(desc.Rotation)?;
                return Ok(adapter);
            }
        }
    }
    Err("Could not find DXGI adapter for the target window's monitor".into())
}

#[cfg(target_os = "windows")]
fn create_d3d11_device(
    adapter: &IDXGIAdapter1,
) -> Result<(ID3D11Device, ID3D11DeviceContext), String> {
    let adapter: IDXGIAdapter = adapter
        .cast()
        .map_err(|e| format!("IDXGIAdapter cast: {e}"))?;
    let mut device = None;
    let mut context = None;
    unsafe {
        D3D11CreateDevice(
            Some(&adapter),
            D3D_DRIVER_TYPE_UNKNOWN,
            HMODULE::default(),
            D3D11_CREATE_DEVICE_BGRA_SUPPORT,
            None,
            D3D11_SDK_VERSION,
            Some(&mut device),
            None,
            Some(&mut context),
        )
        .map_err(|e| format!("D3D11CreateDevice: {e}"))?;
    }
    let device = device.ok_or("D3D11 device was None")?;
    let context = context.ok_or("D3D11 context was None")?;

    if let Ok(dxgi_device) = device.cast::<IDXGIDevice>() {
        let _ = unsafe { dxgi_device.SetGPUThreadPriority(7) };
    }

    Ok((device, context))
}

#[cfg(target_os = "windows")]
fn create_output_duplication(
    device: &ID3D11Device,
    hwnd: HWND,
) -> Result<(IDXGIOutputDuplication, RECT, HMONITOR), String> {
    let dxgi_device: IDXGIDevice = device
        .cast()
        .map_err(|e| format!("IDXGIDevice cast: {e}"))?;
    let adapter: IDXGIAdapter1 = unsafe { dxgi_device.GetAdapter() }
        .map_err(|e| format!("GetAdapter: {e}"))?
        .cast()
        .map_err(|e| format!("IDXGIAdapter1 cast: {e}"))?;

    let target_monitor = unsafe { MonitorFromWindow(hwnd, MONITOR_DEFAULTTONULL) };
    if target_monitor.is_invalid() {
        return Err("MonitorFromWindow returned null".into());
    }

    let mut output_index = 0u32;
    loop {
        let output: IDXGIOutput = match unsafe { adapter.EnumOutputs(output_index) } {
            Ok(o) => o,
            Err(_) => break,
        };
        output_index += 1;

        let desc = unsafe { output.GetDesc().map_err(|e| format!("GetDesc: {e}"))? };

        if desc.Monitor == target_monitor {
            validate_output_rotation(desc.Rotation)?;
            let output1: IDXGIOutput1 = output
                .cast()
                .map_err(|e| format!("IDXGIOutput1 cast: {e}"))?;
            let duplication = unsafe {
                output1
                    .DuplicateOutput(device)
                    .map_err(|e| format!("DuplicateOutput: {e}"))?
            };
            return Ok((duplication, desc.DesktopCoordinates, target_monitor));
        }
    }

    Err("Could not find DXGI output for the target window's monitor".into())
}

#[cfg(target_os = "windows")]
pub(crate) fn create_shared_output_texture(
    device: &ID3D11Device,
    width: u32,
    height: u32,
) -> Result<SharedTextureOutput, String> {
    assert!(width > 0, "shared output width positive");
    assert!(height > 0, "shared output height positive");
    let mut slots = Vec::with_capacity(SHARED_OUTPUT_SLOT_COUNT);
    for _ in 0..SHARED_OUTPUT_SLOT_COUNT {
        slots.push(create_shared_output_slot(device, width, height)?);
    }
    assert_eq!(
        slots.len(),
        SHARED_OUTPUT_SLOT_COUNT,
        "all shared output slots created"
    );
    let slots = <[SharedOutputSlot; SHARED_OUTPUT_SLOT_COUNT]>::try_from(slots)
        .map_err(|_| "shared output slot count mismatch".to_string())?;
    Ok(SharedTextureOutput {
        slots,
        slot_cursor: 0,
        width,
        height,
        dxgi_format: DXGI_FORMAT_B8G8R8A8_UNORM.0 as u32,
    })
}

#[cfg(target_os = "windows")]
fn create_shared_output_slot(
    device: &ID3D11Device,
    width: u32,
    height: u32,
) -> Result<SharedOutputSlot, String> {
    assert!(width > 0, "shared output slot width positive");
    assert!(height > 0, "shared output slot height positive");
    let desc = D3D11_TEXTURE2D_DESC {
        Width: width,
        Height: height,
        MipLevels: 1,
        ArraySize: 1,
        Format: DXGI_FORMAT_B8G8R8A8_UNORM,
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        },
        Usage: D3D11_USAGE_DEFAULT,
        BindFlags: Default::default(),
        CPUAccessFlags: 0,
        MiscFlags: D3D11_RESOURCE_MISC_SHARED.0 as u32,
    };
    let mut texture = None;
    let texture = unsafe {
        device
            .CreateTexture2D(&desc, None, Some(&mut texture))
            .map_err(|e| format!("CreateTexture2D shared output: {e}"))?;
        texture.ok_or("D3D11 shared output texture was None")?
    };
    let resource: IDXGIResource = texture
        .cast()
        .map_err(|e| format!("IDXGIResource shared output cast: {e}"))?;
    let handle = unsafe { resource.GetSharedHandle() }
        .map_err(|e| format!("GetSharedHandle shared output: {e}"))?;
    if handle.is_invalid() {
        return Err("GetSharedHandle shared output returned an invalid handle".into());
    }
    Ok(SharedOutputSlot {
        texture,
        handle: handle.0 as usize as u64,
    })
}

#[cfg(target_os = "windows")]
pub(crate) fn resolve_output_size(
    src_width: u32,
    src_height: u32,
    requested_width: Option<u32>,
    requested_height: Option<u32>,
) -> (u32, u32) {
    let max_width = requested_width.unwrap_or(src_width).max(1);
    let max_height = requested_height.unwrap_or(src_height).max(1);
    let scale = (max_width as f64 / src_width.max(1) as f64)
        .min(max_height as f64 / src_height.max(1) as f64)
        .min(1.0);
    if scale >= 1.0 {
        return (src_width.max(1), src_height.max(1));
    }
    (
        ((src_width as f64 * scale).floor() as u32).max(1),
        ((src_height as f64 * scale).floor() as u32).max(1),
    )
}

#[cfg(target_os = "windows")]
pub(crate) fn capture_timestamp_us(capture_start: std::time::Instant) -> i64 {
    let elapsed_us = capture_start.elapsed().as_micros();
    assert!(elapsed_us < i64::MAX as u128);
    elapsed_us as i64
}

pub(crate) fn pacing_sleep_and_next_deadline(
    now: std::time::Instant,
    deadline: std::time::Instant,
    frame_interval: std::time::Duration,
) -> (std::time::Duration, std::time::Instant) {
    assert!(frame_interval > std::time::Duration::ZERO);
    if now < deadline {
        let sleep_duration = deadline - now;
        assert!(sleep_duration <= frame_interval);
        (sleep_duration, deadline + frame_interval)
    } else {
        (std::time::Duration::ZERO, now + frame_interval)
    }
}

#[cfg(target_os = "windows")]
pub fn capture_loop(inner: &Arc<CaptureInner>, frame_interval: std::time::Duration) {
    let hwnd;
    let device;
    let context;
    let requested_width;
    let requested_height;
    {
        let guard = inner.session.lock();
        let session = match guard.as_ref() {
            Some(s) => s,
            None => {
                emit_lifecycle(inner, "closed-clean", "no session");
                return;
            }
        };
        hwnd = session.hwnd;
        device = session.device.clone();
        context = session.context.clone();
        requested_width = session.requested_width;
        requested_height = session.requested_height;
    }

    let capture_id = inner.capture_id.lock().clone();
    let mut duplication_state: Option<DuplicationState> = None;
    let mut pointer_shape = PointerShapeCache::default();
    let mut recreate_backoff = std::time::Duration::from_millis(100);
    let capture_start = std::time::Instant::now();
    let mut next_frame_deadline = capture_start + frame_interval;

    while inner.running.load(Ordering::Acquire) {
        if !unsafe { IsWindow(Some(hwnd)) }.as_bool() {
            emit_lifecycle(inner, "closed", "window closed");
            break;
        }

        if duplication_state.is_none() {
            match setup_duplication(&device, hwnd, requested_width, requested_height) {
                Ok(state) => {
                    duplication_state = Some(state);
                    pointer_shape = PointerShapeCache::default();
                }
                Err(e) => {
                    if !inner.running.load(Ordering::Acquire) {
                        break;
                    }
                    emit_lifecycle(
                        inner,
                        "error",
                        &format!("Failed to create DXGI output duplication: {e}"),
                    );
                    std::thread::sleep(recreate_backoff);
                    recreate_backoff =
                        (recreate_backoff * 2).min(std::time::Duration::from_secs(2));
                    continue;
                }
            }
        }

        let Some(state) = duplication_state.as_mut() else {
            continue;
        };

        match acquire_and_emit_frame(
            inner,
            &context,
            state,
            hwnd,
            capture_id.as_deref(),
            &mut pointer_shape,
            capture_start,
        ) {
            FrameResult::Ok => {
                recreate_backoff = std::time::Duration::from_millis(100);
            }
            FrameResult::Timeout => {}
            FrameResult::Resized { width, height } => {
                state.cap_w = width;
                state.cap_h = height;
                let (out_w, out_h) =
                    resolve_output_size(width, height, requested_width, requested_height);
                state.out_w = out_w;
                state.out_h = out_h;
                state.shared_output = create_shared_output_texture(&device, width, height).ok();
            }
            FrameResult::AccessLost => {
                duplication_state = None;
                pointer_shape = PointerShapeCache::default();
                std::thread::sleep(recreate_backoff);
                recreate_backoff = (recreate_backoff * 2).min(std::time::Duration::from_secs(2));
                continue;
            }
            FrameResult::WindowGone => {
                emit_lifecycle(inner, "closed", "window closed during capture");
                break;
            }
            FrameResult::Error(e) => {
                emit_lifecycle(inner, "error", &e);
                {
                    let mut guard = inner.fallback.lock();
                    if let Some(tracker) = guard.as_mut() {
                        let _ = tracker.observe(crate::fallback::FailureSignature::DeviceLost);
                    }
                }
                duplication_state = None;
                pointer_shape = PointerShapeCache::default();
                continue;
            }
        }

        let now = std::time::Instant::now();
        let (sleep_duration, deadline) =
            pacing_sleep_and_next_deadline(now, next_frame_deadline, frame_interval);
        next_frame_deadline = deadline;
        if sleep_duration > std::time::Duration::ZERO {
            std::thread::sleep(sleep_duration);
        }
    }

    inner.running.store(false, Ordering::Release);
    emit_lifecycle(inner, "closed-clean", "capture stopped");
}

#[cfg(target_os = "windows")]
fn setup_duplication(
    device: &ID3D11Device,
    hwnd: HWND,
    requested_width: Option<u32>,
    requested_height: Option<u32>,
) -> Result<DuplicationState, String> {
    let (duplication, monitor_rect, monitor) = create_output_duplication(device, hwnd)?;

    let mut window_rect = RECT::default();
    unsafe {
        GetWindowRect(hwnd, &mut window_rect).map_err(|e| format!("GetWindowRect: {e}"))?;
    }

    let crop_left = (window_rect.left - monitor_rect.left).max(0) as u32;
    let crop_top = (window_rect.top - monitor_rect.top).max(0) as u32;
    let monitor_width = (monitor_rect.right - monitor_rect.left) as u32;
    let monitor_height = (monitor_rect.bottom - monitor_rect.top) as u32;
    let crop_right = ((window_rect.right - monitor_rect.left) as u32).min(monitor_width);
    let crop_bottom = ((window_rect.bottom - monitor_rect.top) as u32).min(monitor_height);

    let cap_w = crop_right.saturating_sub(crop_left).max(1);
    let cap_h = crop_bottom.saturating_sub(crop_top).max(1);
    let (out_w, out_h) = resolve_output_size(cap_w, cap_h, requested_width, requested_height);

    let shared_output = create_shared_output_texture(device, cap_w, cap_h).ok();

    Ok(DuplicationState {
        duplication,
        monitor,
        monitor_rect,
        shared_output,
        cap_w,
        cap_h,
        out_w,
        out_h,
    })
}

#[cfg(target_os = "windows")]
enum FrameResult {
    Ok,
    Timeout,
    Resized { width: u32, height: u32 },
    AccessLost,
    WindowGone,
    Error(String),
}

#[cfg(target_os = "windows")]
fn acquire_and_emit_frame(
    inner: &Arc<CaptureInner>,
    context: &ID3D11DeviceContext,
    state: &mut DuplicationState,
    hwnd: HWND,
    capture_id: Option<&str>,
    _pointer_shape: &mut PointerShapeCache,
    capture_start: std::time::Instant,
) -> FrameResult {
    let mut frame_info = DXGI_OUTDUPL_FRAME_INFO::default();
    let mut resource = None;
    match unsafe {
        state
            .duplication
            .AcquireNextFrame(50, &mut frame_info, &mut resource)
    } {
        Ok(()) => {}
        Err(e) if e.code() == DXGI_ERROR_WAIT_TIMEOUT => return FrameResult::Timeout,
        Err(e) if e.code() == DXGI_ERROR_ACCESS_LOST => return FrameResult::AccessLost,
        Err(e) => return FrameResult::Error(format!("AcquireNextFrame: {e}")),
    }

    let desktop_texture: ID3D11Texture2D = match resource
        .as_ref()
        .and_then(|r| r.cast::<ID3D11Texture2D>().ok())
    {
        Some(t) => t,
        None => {
            let _ = unsafe { state.duplication.ReleaseFrame() };
            return FrameResult::Error("Failed to cast desktop resource to ID3D11Texture2D".into());
        }
    };

    if !unsafe { IsWindow(Some(hwnd)) }.as_bool() {
        let _ = unsafe { state.duplication.ReleaseFrame() };
        return FrameResult::WindowGone;
    }

    let mut window_rect = RECT::default();
    if unsafe { GetWindowRect(hwnd, &mut window_rect) }.is_err() {
        let _ = unsafe { state.duplication.ReleaseFrame() };
        return FrameResult::WindowGone;
    }

    let monitor_rect = &state.monitor_rect;
    let crop_left = (window_rect.left - monitor_rect.left).max(0) as u32;
    let crop_top = (window_rect.top - monitor_rect.top).max(0) as u32;
    let monitor_width = (monitor_rect.right - monitor_rect.left) as u32;
    let monitor_height = (monitor_rect.bottom - monitor_rect.top) as u32;
    let crop_right = ((window_rect.right - monitor_rect.left) as u32).min(monitor_width);
    let crop_bottom = ((window_rect.bottom - monitor_rect.top) as u32).min(monitor_height);

    let cur_w = crop_right.saturating_sub(crop_left).max(1);
    let cur_h = crop_bottom.saturating_sub(crop_top).max(1);

    if cur_w != state.cap_w || cur_h != state.cap_h {
        let current_monitor = unsafe { MonitorFromWindow(hwnd, MONITOR_DEFAULTTONULL) };
        let _ = unsafe { state.duplication.ReleaseFrame() };
        if current_monitor != state.monitor {
            return FrameResult::AccessLost;
        }
        return FrameResult::Resized {
            width: cur_w,
            height: cur_h,
        };
    }

    let src_box = D3D11_BOX {
        left: crop_left,
        top: crop_top,
        front: 0,
        right: crop_left + state.cap_w,
        bottom: crop_top + state.cap_h,
        back: 1,
    };

    let desktop_resource: ID3D11Resource = match desktop_texture.cast() {
        Ok(resource) => resource,
        Err(e) => {
            let _ = unsafe { state.duplication.ReleaseFrame() };
            return FrameResult::Error(format!("ID3D11Resource desktop cast: {e}"));
        }
    };

    let result = emit_acquired_frame(
        inner,
        context,
        state,
        capture_id,
        &desktop_resource,
        &src_box,
        capture_start,
    );
    let _ = unsafe { state.duplication.ReleaseFrame() };
    result
}

#[cfg(target_os = "windows")]
fn emit_acquired_frame(
    inner: &Arc<CaptureInner>,
    context: &ID3D11DeviceContext,
    state: &mut DuplicationState,
    capture_id: Option<&str>,
    desktop_resource: &ID3D11Resource,
    src_box: &D3D11_BOX,
    capture_start: std::time::Instant,
) -> FrameResult {
    assert!(src_box.right > src_box.left, "source box width positive");
    assert!(src_box.bottom > src_box.top, "source box height positive");
    let Some(frame_sink) = resolve_frame_sink(inner, capture_id) else {
        note_media_frame_without_sink(
            inner,
            "DXGI frame dropped because no native frame sink is registered",
        );
        return FrameResult::Ok;
    };
    if state.out_w != state.cap_w || state.out_h != state.cap_h {
        return FrameResult::Error(
            "DXGI native bus scaling requires a GPU scaler; refusing CPU readback fallback".into(),
        );
    }
    let Some(shared_output) = state.shared_output.as_mut() else {
        return FrameResult::Error("DXGI shared texture output unavailable".into());
    };
    let slot_index = shared_output.next_slot_index();
    let slot = &shared_output.slots[slot_index];
    let output_resource: ID3D11Resource = match slot.texture.cast() {
        Ok(resource) => resource,
        Err(e) => {
            return FrameResult::Error(format!("ID3D11Resource shared output cast: {e}"));
        }
    };
    unsafe {
        context.CopySubresourceRegion(
            &output_resource,
            0,
            0,
            0,
            0,
            desktop_resource,
            0,
            Some(src_box),
        );
        context.Flush();
    }
    let _ = emit_shared_texture_frame(
        inner,
        &frame_sink,
        slot.handle,
        shared_output.width,
        shared_output.height,
        shared_output.dxgi_format,
        capture_timestamp_us(capture_start),
    );
    FrameResult::Ok
}

#[cfg(target_os = "windows")]
fn update_pointer_shape(
    duplication: &IDXGIOutputDuplication,
    frame_info: &DXGI_OUTDUPL_FRAME_INFO,
    cache: &mut PointerShapeCache,
) -> PointerShapeUpdate {
    if frame_info.PointerShapeBufferSize == 0 {
        return PointerShapeUpdate::Ok;
    }

    let buffer_size = frame_info.PointerShapeBufferSize as usize;
    if cache.buffer.len() < buffer_size {
        cache.buffer.resize(buffer_size, 0);
    }

    let mut required_size = 0u32;
    let mut shape_info = DXGI_OUTDUPL_POINTER_SHAPE_INFO::default();
    let result = unsafe {
        duplication.GetFramePointerShape(
            cache.buffer.len() as u32,
            cache.buffer.as_mut_ptr().cast(),
            &mut required_size,
            &mut shape_info,
        )
    };

    match result {
        Ok(()) => {
            cache.buffer.truncate(required_size as usize);
            cache.info = Some(shape_info);
            PointerShapeUpdate::Ok
        }
        Err(e) if e.code() == DXGI_ERROR_MORE_DATA && required_size > cache.buffer.len() as u32 => {
            cache.buffer.resize(required_size as usize, 0);
            let result = unsafe {
                duplication.GetFramePointerShape(
                    cache.buffer.len() as u32,
                    cache.buffer.as_mut_ptr().cast(),
                    &mut required_size,
                    &mut shape_info,
                )
            };
            match result {
                Ok(()) => {
                    cache.buffer.truncate(required_size as usize);
                    cache.info = Some(shape_info);
                    PointerShapeUpdate::Ok
                }
                Err(e) if e.code() == DXGI_ERROR_ACCESS_LOST => PointerShapeUpdate::AccessLost,
                Err(e) => PointerShapeUpdate::Error(format!("GetFramePointerShape: {e}")),
            }
        }
        Err(e) if e.code() == DXGI_ERROR_ACCESS_LOST => PointerShapeUpdate::AccessLost,
        Err(e) => PointerShapeUpdate::Error(format!("GetFramePointerShape: {e}")),
    }
}

#[cfg(target_os = "windows")]
#[allow(clippy::too_many_arguments)]
fn composite_pointer_shape(
    frame: &mut [u8],
    width: u32,
    height: u32,
    stride: u32,
    frame_info: &DXGI_OUTDUPL_FRAME_INFO,
    crop_left: u32,
    crop_top: u32,
    cache: &PointerShapeCache,
) {
    if !frame_info.PointerPosition.Visible.as_bool() {
        return;
    }
    let Some(shape_info) = cache.info else {
        return;
    };
    if shape_info.Width == 0 || shape_info.Height == 0 || shape_info.Pitch == 0 {
        return;
    }

    let pointer_x = frame_info.PointerPosition.Position.x - crop_left as i32;
    let pointer_y = frame_info.PointerPosition.Position.y - crop_top as i32;
    if pointer_x >= width as i32 || pointer_y >= height as i32 {
        return;
    }

    if shape_info.Type == DXGI_OUTDUPL_POINTER_SHAPE_TYPE_COLOR.0 as u32 {
        composite_color_pointer(
            frame,
            width,
            height,
            stride,
            pointer_x,
            pointer_y,
            &shape_info,
            &cache.buffer,
        );
    } else if shape_info.Type == DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MASKED_COLOR.0 as u32 {
        composite_masked_color_pointer(
            frame,
            width,
            height,
            stride,
            pointer_x,
            pointer_y,
            &shape_info,
            &cache.buffer,
        );
    } else if shape_info.Type == DXGI_OUTDUPL_POINTER_SHAPE_TYPE_MONOCHROME.0 as u32 {
        composite_monochrome_pointer(
            frame,
            width,
            height,
            stride,
            pointer_x,
            pointer_y,
            &shape_info,
            &cache.buffer,
        );
    }
}

#[cfg(target_os = "windows")]
#[allow(clippy::too_many_arguments)]
fn composite_color_pointer(
    frame: &mut [u8],
    width: u32,
    height: u32,
    stride: u32,
    pointer_x: i32,
    pointer_y: i32,
    shape_info: &DXGI_OUTDUPL_POINTER_SHAPE_INFO,
    shape: &[u8],
) {
    let pitch = shape_info.Pitch as usize;
    for y in 0..shape_info.Height as i32 {
        let dst_y = pointer_y + y;
        if dst_y < 0 || dst_y >= height as i32 {
            continue;
        }
        for x in 0..shape_info.Width as i32 {
            let dst_x = pointer_x + x;
            if dst_x < 0 || dst_x >= width as i32 {
                continue;
            }
            let src_offset = y as usize * pitch + x as usize * 4;
            let dst_offset = dst_y as usize * stride as usize + dst_x as usize * 4;
            if src_offset + 4 > shape.len() || dst_offset + 4 > frame.len() {
                continue;
            }
            blend_bgra_pixel(frame, dst_offset, shape, src_offset);
        }
    }
}

#[cfg(target_os = "windows")]
#[allow(clippy::too_many_arguments)]
fn composite_masked_color_pointer(
    frame: &mut [u8],
    width: u32,
    height: u32,
    stride: u32,
    pointer_x: i32,
    pointer_y: i32,
    shape_info: &DXGI_OUTDUPL_POINTER_SHAPE_INFO,
    shape: &[u8],
) {
    let pitch = shape_info.Pitch as usize;
    for y in 0..shape_info.Height as i32 {
        let dst_y = pointer_y + y;
        if dst_y < 0 || dst_y >= height as i32 {
            continue;
        }
        for x in 0..shape_info.Width as i32 {
            let dst_x = pointer_x + x;
            if dst_x < 0 || dst_x >= width as i32 {
                continue;
            }
            let src_offset = y as usize * pitch + x as usize * 4;
            let dst_offset = dst_y as usize * stride as usize + dst_x as usize * 4;
            if src_offset + 4 > shape.len() || dst_offset + 4 > frame.len() {
                continue;
            }
            let mask = shape[src_offset + 3];
            if mask == 0 {
                frame[dst_offset..dst_offset + 3]
                    .copy_from_slice(&shape[src_offset..src_offset + 3]);
                frame[dst_offset + 3] = 255;
            } else if mask == 0xff {
                frame[dst_offset] ^= shape[src_offset];
                frame[dst_offset + 1] ^= shape[src_offset + 1];
                frame[dst_offset + 2] ^= shape[src_offset + 2];
                frame[dst_offset + 3] = 255;
            } else {
                blend_bgra_pixel(frame, dst_offset, shape, src_offset);
            }
        }
    }
}

#[cfg(target_os = "windows")]
#[allow(clippy::too_many_arguments)]
fn composite_monochrome_pointer(
    frame: &mut [u8],
    width: u32,
    height: u32,
    stride: u32,
    pointer_x: i32,
    pointer_y: i32,
    shape_info: &DXGI_OUTDUPL_POINTER_SHAPE_INFO,
    shape: &[u8],
) {
    let pitch = shape_info.Pitch as usize;
    let full_height = shape_info.Height as usize;
    if pitch == 0 || full_height == 0 {
        return;
    }

    let visible_height = if shape.len() >= pitch * full_height * 2 {
        full_height
    } else {
        full_height / 2
    };
    if visible_height == 0 || shape.len() < pitch * visible_height * 2 {
        return;
    }

    let xor_mask_offset = pitch * visible_height;
    for y in 0..visible_height as i32 {
        let dst_y = pointer_y + y;
        if dst_y < 0 || dst_y >= height as i32 {
            continue;
        }
        for x in 0..shape_info.Width as i32 {
            let dst_x = pointer_x + x;
            if dst_x < 0 || dst_x >= width as i32 {
                continue;
            }
            let byte_index = y as usize * pitch + x as usize / 8;
            let mask = 0x80 >> (x as usize % 8);
            if byte_index >= xor_mask_offset || xor_mask_offset + byte_index >= shape.len() {
                continue;
            }

            let and_set = shape[byte_index] & mask != 0;
            let xor_set = shape[xor_mask_offset + byte_index] & mask != 0;
            let dst_offset = dst_y as usize * stride as usize + dst_x as usize * 4;
            if dst_offset + 4 > frame.len() {
                continue;
            }

            match (and_set, xor_set) {
                (true, false) => {}
                (true, true) => {
                    frame[dst_offset] ^= 0xff;
                    frame[dst_offset + 1] ^= 0xff;
                    frame[dst_offset + 2] ^= 0xff;
                    frame[dst_offset + 3] = 255;
                }
                (false, false) => {
                    frame[dst_offset] = 0;
                    frame[dst_offset + 1] = 0;
                    frame[dst_offset + 2] = 0;
                    frame[dst_offset + 3] = 255;
                }
                (false, true) => {
                    frame[dst_offset] = 255;
                    frame[dst_offset + 1] = 255;
                    frame[dst_offset + 2] = 255;
                    frame[dst_offset + 3] = 255;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::pacing_sleep_and_next_deadline;
    use std::time::{Duration, Instant};

    const TEST_FRAME_INTERVAL: Duration = Duration::from_millis(33);

    #[test]
    fn pacing_sleeps_remaining_time_and_advances_deadline_by_interval() {
        let start = Instant::now();
        let deadline = start + TEST_FRAME_INTERVAL;
        let now = start + Duration::from_millis(10);
        let (sleep_duration, next_deadline) =
            pacing_sleep_and_next_deadline(now, deadline, TEST_FRAME_INTERVAL);
        assert_eq!(sleep_duration, Duration::from_millis(23));
        assert_eq!(next_deadline, deadline + TEST_FRAME_INTERVAL);
    }

    #[test]
    fn pacing_does_not_drift_across_iterations_with_varying_work() {
        let start = Instant::now();
        let mut deadline = start + TEST_FRAME_INTERVAL;
        let mut now = start;
        for iteration in 1..=100u32 {
            now += Duration::from_millis(7);
            let (sleep_duration, next_deadline) =
                pacing_sleep_and_next_deadline(now, deadline, TEST_FRAME_INTERVAL);
            assert!(sleep_duration <= TEST_FRAME_INTERVAL);
            now += sleep_duration;
            deadline = next_deadline;
            assert_eq!(now, start + TEST_FRAME_INTERVAL * iteration);
        }
    }

    #[test]
    fn pacing_resets_deadline_without_sleeping_when_behind() {
        let start = Instant::now();
        let deadline = start + TEST_FRAME_INTERVAL;
        let now = deadline + Duration::from_millis(50);
        let (sleep_duration, next_deadline) =
            pacing_sleep_and_next_deadline(now, deadline, TEST_FRAME_INTERVAL);
        assert_eq!(sleep_duration, Duration::ZERO);
        assert_eq!(next_deadline, now + TEST_FRAME_INTERVAL);
    }

    #[test]
    fn pacing_at_exact_deadline_resets_without_sleeping() {
        let start = Instant::now();
        let deadline = start + TEST_FRAME_INTERVAL;
        let (sleep_duration, next_deadline) =
            pacing_sleep_and_next_deadline(deadline, deadline, TEST_FRAME_INTERVAL);
        assert_eq!(sleep_duration, Duration::ZERO);
        assert_eq!(next_deadline, deadline + TEST_FRAME_INTERVAL);
    }
}

#[cfg(target_os = "windows")]
fn blend_bgra_pixel(frame: &mut [u8], dst_offset: usize, shape: &[u8], src_offset: usize) {
    let alpha = shape[src_offset + 3] as u32;
    if alpha == 0 {
        return;
    }
    if alpha == 255 {
        frame[dst_offset..dst_offset + 4].copy_from_slice(&shape[src_offset..src_offset + 4]);
        return;
    }
    let inv_alpha = 255 - alpha;
    for channel in 0..3 {
        let src = shape[src_offset + channel] as u32;
        let dst = frame[dst_offset + channel] as u32;
        frame[dst_offset + channel] = ((src * alpha + dst * inv_alpha + 127) / 255) as u8;
    }
    frame[dst_offset + 3] = 255;
}
