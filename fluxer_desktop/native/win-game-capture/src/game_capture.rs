// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::fallback;
use crate::game_capture_abi::{ENV_FORCE_CPU, ENV_VERBOSE, env_flag_enabled};
use crate::nv12_gpu::Nv12GpuConverter;
use crate::{
    CaptureInner,
    compatibility::{InjectionPolicy, injection_policy_for_window},
    dxgi_capture::{resolve_output_size, wall_clock_us},
    emit_lifecycle, emit_shared_texture_frame,
    game_capture_abi::{
        GAME_CAPTURE_API_OPENGL, GAME_CAPTURE_BUFFER_COUNT,
        GAME_CAPTURE_CONTROL_DISABLE_SHARED_TEXTURE, GAME_CAPTURE_FRAME_PREFIX,
        GAME_CAPTURE_INFO_PREFIX, GAME_CAPTURE_KEEPALIVE_PREFIX, GAME_CAPTURE_READY_PREFIX,
        GAME_CAPTURE_STATE_ERROR, GAME_CAPTURE_STATE_RESIZE_REQUIRED, GAME_CAPTURE_STATE_STOPPED,
        GAME_CAPTURE_STOP_PREFIX, GAME_CAPTURE_TRANSPORT_MEMORY,
        GAME_CAPTURE_TRANSPORT_SHARED_TEXTURE, GameCaptureSharedInfo, mutex_name, object_name,
        presented_recently, qpc_now_us, shared_frame_mapping_size,
    },
    note_cpu_fallback_frame_dropped, note_media_frame_without_sink,
};
use std::{
    ffi::c_void,
    path::Path,
    ptr::{null, null_mut},
    sync::{Arc, Mutex, atomic::Ordering},
};
use windows::Win32::{
    Foundation::{HANDLE as WinHandle, HMODULE},
    Graphics::{
        Direct3D::{D3D_DRIVER_TYPE_HARDWARE, D3D_DRIVER_TYPE_UNKNOWN},
        Direct3D11::{
            D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_SDK_VERSION, D3D11_TEXTURE2D_DESC,
            D3D11CreateDevice, ID3D11Device, ID3D11DeviceContext, ID3D11Texture2D,
        },
        Dxgi::{CreateDXGIFactory1, IDXGIAdapter, IDXGIDevice, IDXGIFactory1},
    },
};
use windows::core::Interface;
use windows_sys::Win32::{
    Foundation::{
        CloseHandle, FreeLibrary, HANDLE, HINSTANCE, HMODULE as WinSysHmodule, HWND,
        INVALID_HANDLE_VALUE, LPARAM, RECT, WAIT_ABANDONED, WAIT_OBJECT_0, WAIT_TIMEOUT,
    },
    Graphics::Gdi::{
        EnumDisplayMonitors, GetMonitorInfoW, HMONITOR, MONITOR_DEFAULTTONEAREST, MONITORINFO,
        MonitorFromRect,
    },
    System::{
        Diagnostics::Debug::WriteProcessMemory,
        LibraryLoader::{GetModuleHandleW, GetProcAddress, LoadLibraryW},
        Memory::{
            CreateFileMappingW, FILE_MAP_ALL_ACCESS, MEM_COMMIT, MEM_RELEASE, MEM_RESERVE,
            MEMORY_MAPPED_VIEW_ADDRESS, MapViewOfFile, PAGE_READWRITE, UnmapViewOfFile,
            VirtualAllocEx, VirtualFreeEx,
        },
        SystemInformation::{
            IMAGE_FILE_MACHINE, IMAGE_FILE_MACHINE_AMD64, IMAGE_FILE_MACHINE_ARM,
            IMAGE_FILE_MACHINE_ARM64, IMAGE_FILE_MACHINE_ARMNT, IMAGE_FILE_MACHINE_I386,
            IMAGE_FILE_MACHINE_IA64, IMAGE_FILE_MACHINE_UNKNOWN,
        },
        Threading::{
            CreateEventW, CreateMutexW, CreateRemoteThread, GetCurrentProcessId, IsWow64Process,
            IsWow64Process2, OpenProcess, PROCESS_CREATE_THREAD, PROCESS_QUERY_INFORMATION,
            PROCESS_QUERY_LIMITED_INFORMATION, PROCESS_VM_OPERATION, PROCESS_VM_WRITE, ResetEvent,
            SetEvent, WaitForSingleObject,
        },
    },
    UI::WindowsAndMessaging::{
        EnumWindows, GW_OWNER, GWL_STYLE, GetClientRect, GetForegroundWindow, GetWindow,
        GetWindowLongPtrW, GetWindowRect, GetWindowThreadProcessId, HHOOK, IsWindow,
        IsWindowVisible, PostThreadMessageW, SetWindowsHookExW, UnhookWindowsHookEx, WH_GETMESSAGE,
        WM_NULL, WS_BORDER, WS_MAXIMIZE,
    },
};

const MAX_GAME_CAPTURE_DIMENSION: u32 = 8192;
const MAX_GAME_CAPTURE_FRAME_BYTES: usize = 384 * 1024 * 1024;
const FRAME_EVENT_WAIT_MS: u32 = 100;

const HOOK_PROC_EXPORT: &[u8] = b"FluxerGetMsgProc\0";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InjectionMethod {
    Auto,
    RemoteThread,
    SetWindowsHook,
}

impl InjectionMethod {
    fn resolve(explicit: Option<&str>) -> Self {
        if let Some(explicit) = explicit {
            return Self::parse(explicit).unwrap_or(Self::Auto);
        }
        if let Some(value) = std::env::var_os(crate::game_capture_abi::ENV_INJECT_METHOD) {
            return Self::parse(&value.to_string_lossy()).unwrap_or(Self::Auto);
        }
        Self::Auto
    }

    fn parse(value: &str) -> Option<Self> {
        match value.trim().to_ascii_lowercase().as_str() {
            "auto" => Some(Self::Auto),
            "remote-thread" => Some(Self::RemoteThread),
            "set-windows-hook" => Some(Self::SetWindowsHook),
            _ => None,
        }
    }

    fn as_str(self) -> &'static str {
        match self {
            Self::Auto => "auto",
            Self::RemoteThread => "remote-thread",
            Self::SetWindowsHook => "set-windows-hook",
        }
    }
}

struct InstalledWindowsHook {
    hook: HHOOK,
    module: WinSysHmodule,
}

unsafe impl Send for InstalledWindowsHook {}
unsafe impl Sync for InstalledWindowsHook {}

impl Drop for InstalledWindowsHook {
    fn drop(&mut self) {
        unsafe {
            if !self.hook.is_null() {
                UnhookWindowsHookEx(self.hook);
                self.hook = null_mut();
            }
            if !self.module.is_null() {
                FreeLibrary(self.module);
                self.module = null_mut();
            }
        }
    }
}

const STALL_THRESHOLD: std::time::Duration = std::time::Duration::from_millis(1000);
const RECENT_PRESENT_WINDOW_US: i64 = 1_000_000;

#[derive(Debug)]
struct OwnedHandle(HANDLE);

impl OwnedHandle {
    fn new(handle: HANDLE, context: &str) -> Result<Self, String> {
        if handle.is_null() || handle == INVALID_HANDLE_VALUE {
            return Err(format!("{context}: failed to create/open handle"));
        }
        Ok(Self(handle))
    }

    fn raw(&self) -> HANDLE {
        self.0
    }
}

unsafe impl Send for OwnedHandle {}
unsafe impl Sync for OwnedHandle {}

impl Drop for OwnedHandle {
    fn drop(&mut self) {
        if !self.0.is_null() && self.0 != INVALID_HANDLE_VALUE {
            unsafe {
                CloseHandle(self.0);
            }
        }
        self.0 = null_mut();
    }
}

#[derive(Debug)]
struct MappedView<T> {
    _mapping: OwnedHandle,
    ptr: *mut T,
}

unsafe impl<T> Send for MappedView<T> {}
unsafe impl<T> Sync for MappedView<T> {}

impl<T> MappedView<T> {
    fn create(name: &str, size: usize) -> Result<Self, String> {
        if size == 0 || size > u32::MAX as usize {
            return Err(format!("invalid mapping size for {name}: {size}"));
        }
        let wide_name = to_wide(name);
        let mapping = unsafe {
            CreateFileMappingW(
                INVALID_HANDLE_VALUE,
                null(),
                PAGE_READWRITE,
                0,
                size as u32,
                wide_name.as_ptr(),
            )
        };
        let mapping = OwnedHandle::new(mapping, "CreateFileMappingW")?;
        let view = unsafe { MapViewOfFile(mapping.raw(), FILE_MAP_ALL_ACCESS, 0, 0, size) };
        if view.Value.is_null() {
            return Err(format!("MapViewOfFile failed for {name}"));
        }
        Ok(Self {
            _mapping: mapping,
            ptr: view.Value.cast(),
        })
    }
}

impl<T> Drop for MappedView<T> {
    fn drop(&mut self) {
        if !self.ptr.is_null() {
            unsafe {
                UnmapViewOfFile(MEMORY_MAPPED_VIEW_ADDRESS {
                    Value: self.ptr.cast(),
                });
            }
        }
        self.ptr = null_mut();
    }
}

pub struct GameCaptureSession {
    target_hwnd: HWND,
    capture_width: u32,
    capture_height: u32,
    output_width: u32,
    output_height: u32,
    info: MappedView<GameCaptureSharedInfo>,
    frames: MappedView<u8>,
    frame_buffer_capacity: usize,
    ready_event: OwnedHandle,
    stop_event: OwnedHandle,
    _keepalive_mutex: OwnedHandle,
    frame_mutexes: [OwnedHandle; GAME_CAPTURE_BUFFER_COUNT],
    force_cpu_readback: bool,
    requested_injection_method: InjectionMethod,
    used_injection_method: InjectionMethod,
    native_texture: Mutex<Option<NativeTextureHandleInfo>>,
    _windows_hook: Option<InstalledWindowsHook>,
}

unsafe impl Send for GameCaptureSession {}
unsafe impl Sync for GameCaptureSession {}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NativeTextureHandleInfo {
    pub handle: u64,
    pub width: u32,
    pub height: u32,
    pub dxgi_format: u32,
    pub timestamp_us: i64,
}

struct SharedTextureReader {
    device: ID3D11Device,
    context: ID3D11DeviceContext,
    handle: u64,
    width: u32,
    height: u32,
    format: u32,
    source_format: crate::hdr::SourceFormat,
    nv12: Option<Nv12GpuConverter>,
}

impl Drop for GameCaptureSession {
    fn drop(&mut self) {
        unsafe {
            SetEvent(self.stop_event.raw());
        }
    }
}

impl GameCaptureSession {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        source_id: &str,
        source_kind: &str,
        requested_width: Option<u32>,
        requested_height: Option<u32>,
        frame_rate: u32,
        hook_path: &str,
        hook_path_x86: Option<&str>,
        injection_method: Option<&str>,
    ) -> Result<Self, String> {
        let requested_injection_method = InjectionMethod::resolve(injection_method);
        let target_hwnd = resolve_game_capture_target(source_id, source_kind)?;
        let target_pid = target_process_id(target_hwnd)?;
        if target_pid == unsafe { GetCurrentProcessId() } {
            return Err("refusing to inject game capture hook into Fluxer".into());
        }

        let force_cpu_readback = match injection_policy_for_window(target_pid, target_hwnd) {
            InjectionPolicy::Deny(reason) => return Err(reason),
            InjectionPolicy::ForceCpuReadback => true,
            InjectionPolicy::Allow => false,
        };

        let target_machine = target_process_machine(target_pid)?;
        let target_is_32_bit = machine_is_32_bit(target_machine)?;
        let selected_hook_path = if target_is_32_bit {
            hook_path_x86.ok_or(
                "target game process is 32-bit but no 32-bit game capture hook DLL was provided",
            )?
        } else if target_machine == HOST_MACHINE {
            hook_path
        } else {
            return Err(format!(
                "target game process architecture (IMAGE_FILE_MACHINE 0x{target_machine:04x}) \
                 differs from the host architecture and no matching game capture hook DLL ships for it"
            ));
        };

        let (capture_width, capture_height) = window_capture_size(target_hwnd)?;
        validate_capture_size(capture_width, capture_height)?;
        let (output_width, output_height) = resolve_output_size(
            capture_width,
            capture_height,
            requested_width,
            requested_height,
        );
        let (buffer_width, buffer_height) =
            shared_buffer_dimensions_for_window(target_hwnd, capture_width, capture_height);
        let frame_map_size = shared_frame_mapping_size(buffer_width, buffer_height)
            .ok_or("game capture frame mapping size overflow")?;
        if frame_map_size > MAX_GAME_CAPTURE_FRAME_BYTES {
            return Err(format!(
                "game capture frame mapping too large: {frame_map_size} bytes"
            ));
        }

        let info_name = object_name(GAME_CAPTURE_INFO_PREFIX, target_pid);
        let frame_name = object_name(GAME_CAPTURE_FRAME_PREFIX, target_pid);
        let ready_name = object_name(GAME_CAPTURE_READY_PREFIX, target_pid);
        let stop_name = object_name(GAME_CAPTURE_STOP_PREFIX, target_pid);
        let keepalive_name = object_name(GAME_CAPTURE_KEEPALIVE_PREFIX, target_pid);
        let mutex_names = [mutex_name(target_pid, 0), mutex_name(target_pid, 1)];

        let info = MappedView::<GameCaptureSharedInfo>::create(
            &info_name,
            std::mem::size_of::<GameCaptureSharedInfo>(),
        )?;
        unsafe {
            std::ptr::write_volatile(
                info.ptr,
                GameCaptureSharedInfo::new(
                    target_hwnd as usize as u64,
                    buffer_width,
                    buffer_height,
                    frame_rate,
                ),
            );
        }
        let frames = MappedView::<u8>::create(&frame_name, frame_map_size)?;
        let ready_event = create_event(&ready_name, false, false)?;
        let stop_event = create_event(&stop_name, true, false)?;
        let keepalive_mutex = create_mutex(&keepalive_name)?;
        let frame_mutexes = [
            create_mutex(&mutex_names[0])?,
            create_mutex(&mutex_names[1])?,
        ];
        unsafe {
            ResetEvent(ready_event.raw());
            ResetEvent(stop_event.raw());
        }
        let Injected {
            method: used_injection_method,
            windows_hook,
        } = inject(
            requested_injection_method,
            target_hwnd,
            target_pid,
            target_is_32_bit,
            selected_hook_path,
        )?;

        Ok(Self {
            target_hwnd,
            capture_width: buffer_width,
            capture_height: buffer_height,
            output_width,
            output_height,
            info,
            frames,
            frame_buffer_capacity: frame_map_size / GAME_CAPTURE_BUFFER_COUNT,
            ready_event,
            stop_event,
            _keepalive_mutex: keepalive_mutex,
            frame_mutexes,
            force_cpu_readback,
            requested_injection_method,
            used_injection_method,
            native_texture: Mutex::new(None),
            _windows_hook: windows_hook,
        })
    }

    #[allow(clippy::misnamed_getters)]
    pub fn capture_width(&self) -> u32 {
        self.output_width
    }

    #[allow(clippy::misnamed_getters)]
    pub fn capture_height(&self) -> u32 {
        self.output_height
    }

    pub fn force_cpu_readback(&self) -> bool {
        self.force_cpu_readback
    }

    pub fn requested_injection_method(&self) -> &'static str {
        self.requested_injection_method.as_str()
    }

    pub fn used_injection_method(&self) -> &'static str {
        self.used_injection_method.as_str()
    }

    pub fn read_shared_info(&self) -> Option<GameCaptureSharedInfo> {
        let info = unsafe { std::ptr::read_volatile(self.info.ptr) };
        if info.magic == crate::game_capture_abi::GAME_CAPTURE_MAGIC {
            Some(info)
        } else {
            None
        }
    }

    pub fn read_native_texture_info(&self) -> Option<NativeTextureHandleInfo> {
        self.native_texture.lock().ok().and_then(|guard| *guard)
    }

    fn set_native_texture_info(&self, info: Option<NativeTextureHandleInfo>) {
        if let Ok(mut guard) = self.native_texture.lock() {
            *guard = info;
        }
    }

    fn request_hook_disable_shared_texture(&self) {
        unsafe {
            let control = &mut (*self.info.ptr).control;
            std::ptr::write_volatile(
                control,
                std::ptr::read_volatile(control) | GAME_CAPTURE_CONTROL_DISABLE_SHARED_TEXTURE,
            );
        }
    }
}

fn to_wide(value: &str) -> Vec<u16> {
    value.encode_utf16().chain(std::iter::once(0)).collect()
}

fn create_event(
    name: &str,
    manual_reset: bool,
    initial_state: bool,
) -> Result<OwnedHandle, String> {
    let wide_name = to_wide(name);
    let handle = unsafe {
        CreateEventW(
            null(),
            i32::from(manual_reset),
            i32::from(initial_state),
            wide_name.as_ptr(),
        )
    };
    OwnedHandle::new(handle, "CreateEventW")
}

fn create_mutex(name: &str) -> Result<OwnedHandle, String> {
    let wide_name = to_wide(name);
    let handle = unsafe { CreateMutexW(null(), 0, wide_name.as_ptr()) };
    OwnedHandle::new(handle, "CreateMutexW")
}

fn verbose_log(message: &str) {
    if !env_flag_enabled(ENV_VERBOSE) {
        return;
    }
    let text = format!("[fluxer-game-capture] {message}");
    let line = to_wide(&text);
    unsafe {
        windows_sys::Win32::System::Diagnostics::Debug::OutputDebugStringW(line.as_ptr());
    }
    use std::io::Write;
    let _ = writeln!(std::io::stderr(), "{text}");
}

struct Injected {
    method: InjectionMethod,
    windows_hook: Option<InstalledWindowsHook>,
}

const HOST_IS_32_BIT: bool = cfg!(target_pointer_width = "32");
const HOST_MACHINE: IMAGE_FILE_MACHINE = if cfg!(target_arch = "x86_64") {
    IMAGE_FILE_MACHINE_AMD64
} else if cfg!(target_arch = "aarch64") {
    IMAGE_FILE_MACHINE_ARM64
} else if cfg!(target_arch = "x86") {
    IMAGE_FILE_MACHINE_I386
} else {
    IMAGE_FILE_MACHINE_UNKNOWN
};

fn inject(
    method: InjectionMethod,
    target_hwnd: HWND,
    target_pid: u32,
    target_is_32_bit: bool,
    hook_path: &str,
) -> Result<Injected, String> {
    let set_windows_hook_possible = target_is_32_bit == HOST_IS_32_BIT;
    verbose_log(&format!(
        "injecting hook (method={}, target_pid={target_pid}, target_32bit={target_is_32_bit}, \
         set_windows_hook_possible={set_windows_hook_possible})",
        method.as_str()
    ));

    match method {
        InjectionMethod::RemoteThread => {
            inject_via_remote_thread(target_pid, hook_path, target_is_32_bit)?;
            Ok(Injected {
                method: InjectionMethod::RemoteThread,
                windows_hook: None,
            })
        }
        InjectionMethod::SetWindowsHook => {
            if !set_windows_hook_possible {
                return Err(
                    "the SetWindowsHookEx injection method requires the target game to match \
                     Fluxer's bitness; this target is a different bitness, so pick the \
                     remote-thread or automatic method instead"
                        .into(),
                );
            }
            let windows_hook = inject_via_set_windows_hook(target_hwnd, target_pid, hook_path)?;
            Ok(Injected {
                method: InjectionMethod::SetWindowsHook,
                windows_hook: Some(windows_hook),
            })
        }
        InjectionMethod::Auto => {
            match inject_via_remote_thread(target_pid, hook_path, target_is_32_bit) {
                Ok(()) => Ok(Injected {
                    method: InjectionMethod::RemoteThread,
                    windows_hook: None,
                }),
                Err(remote_thread_error) => {
                    if !set_windows_hook_possible {
                        return Err(remote_thread_error);
                    }
                    verbose_log(&format!(
                        "remote-thread injection failed ({remote_thread_error}); falling back to \
                         SetWindowsHookEx"
                    ));
                    match inject_via_set_windows_hook(target_hwnd, target_pid, hook_path) {
                        Ok(windows_hook) => Ok(Injected {
                            method: InjectionMethod::SetWindowsHook,
                            windows_hook: Some(windows_hook),
                        }),
                        Err(set_hook_error) => Err(format!(
                            "game capture injection failed: remote-thread method failed \
                             ({remote_thread_error}); SetWindowsHookEx fallback also failed \
                             ({set_hook_error})"
                        )),
                    }
                }
            }
        }
    }
}

fn inject_via_remote_thread(
    target_pid: u32,
    hook_path: &str,
    target_is_32_bit: bool,
) -> Result<(), String> {
    let hook_path_buf = Path::new(hook_path);
    if !hook_path_buf.exists() {
        return Err(format!(
            "game capture hook DLL missing: {}",
            hook_path_buf.display()
        ));
    }

    if target_is_32_bit != HOST_IS_32_BIT {
        return inject_via_helper(target_pid, hook_path);
    }

    let hook_path = to_wide(&hook_path_buf.to_string_lossy());
    let hook_path_bytes = hook_path
        .len()
        .checked_mul(std::mem::size_of::<u16>())
        .ok_or("hook path size overflow")?;

    unsafe {
        let process = OpenProcess(
            PROCESS_CREATE_THREAD
                | PROCESS_QUERY_INFORMATION
                | PROCESS_VM_OPERATION
                | PROCESS_VM_WRITE,
            0,
            target_pid,
        );
        let process = OwnedHandle::new(process, "OpenProcess")?;
        let remote_path = VirtualAllocEx(
            process.raw(),
            null(),
            hook_path_bytes,
            MEM_COMMIT | MEM_RESERVE,
            PAGE_READWRITE,
        );
        if remote_path.is_null() {
            return Err("VirtualAllocEx failed while injecting game capture hook".into());
        }

        let mut written = 0usize;
        let write_ok = WriteProcessMemory(
            process.raw(),
            remote_path,
            hook_path.as_ptr().cast(),
            hook_path_bytes,
            &mut written,
        ) != 0;
        if !write_ok || written != hook_path_bytes {
            VirtualFreeEx(process.raw(), remote_path, 0, MEM_RELEASE);
            return Err("WriteProcessMemory failed while injecting game capture hook".into());
        }

        let kernel32_name = to_wide("kernel32.dll");
        let kernel32 = GetModuleHandleW(kernel32_name.as_ptr());
        if kernel32.is_null() {
            VirtualFreeEx(process.raw(), remote_path, 0, MEM_RELEASE);
            return Err(
                "GetModuleHandleW(kernel32.dll) failed while injecting game capture hook".into(),
            );
        }
        let load_library = GetProcAddress(kernel32, c"LoadLibraryW".as_ptr().cast());
        let Some(load_library) = load_library else {
            VirtualFreeEx(process.raw(), remote_path, 0, MEM_RELEASE);
            return Err(
                "GetProcAddress(LoadLibraryW) failed while injecting game capture hook".into(),
            );
        };
        let start_routine: unsafe extern "system" fn(*mut c_void) -> u32 =
            std::mem::transmute(load_library);
        let thread = CreateRemoteThread(
            process.raw(),
            null(),
            0,
            Some(start_routine),
            remote_path,
            0,
            null_mut(),
        );
        let thread = match OwnedHandle::new(thread, "CreateRemoteThread") {
            Ok(thread) => thread,
            Err(error) => {
                VirtualFreeEx(process.raw(), remote_path, 0, MEM_RELEASE);
                return Err(error);
            }
        };
        let wait = WaitForSingleObject(thread.raw(), 5000);
        VirtualFreeEx(process.raw(), remote_path, 0, MEM_RELEASE);
        if wait != WAIT_OBJECT_0 && wait != WAIT_ABANDONED {
            return Err("game capture hook injection timed out".into());
        }
    }
    Ok(())
}

fn helper_stage_reason(code: i32) -> &'static str {
    match code {
        2 => "bad helper arguments",
        3 => "hook DLL missing (as seen by the helper)",
        4 => "OpenProcess failed in the helper (target gone or insufficient rights)",
        5 => "VirtualAllocEx failed in the helper",
        6 => "WriteProcessMemory failed in the helper",
        7 => "GetModuleHandleW(kernel32) failed in the helper",
        8 => "GetProcAddress(LoadLibraryW) failed in the helper",
        9 => "CreateRemoteThread failed in the helper",
        10 => "the remote LoadLibraryW thread timed out",
        11 => "LoadLibraryW returned NULL in the target (the hook DLL failed to load)",
        64 => "the inject-helper exe is not a Windows build",
        _ => "unknown helper failure",
    }
}

fn helper_path_for_hook(hook_path: &str) -> Result<std::path::PathBuf, String> {
    let hook = Path::new(hook_path);
    let dir = hook
        .parent()
        .ok_or("could not resolve the hook DLL directory for the inject-helper")?;
    let file_name = hook
        .file_name()
        .and_then(|name| name.to_str())
        .ok_or("could not resolve the hook DLL file name for the inject-helper")?;
    let tag = file_name
        .strip_prefix("fluxer-game-hook.")
        .and_then(|rest| rest.strip_suffix(".dll"))
        .ok_or_else(|| {
            format!("unexpected hook DLL file name (cannot derive inject-helper): {file_name}")
        })?;
    Ok(dir.join(format!("fluxer-inject-helper.{tag}.exe")))
}

fn inject_via_helper(target_pid: u32, hook_path: &str) -> Result<(), String> {
    let helper = helper_path_for_hook(hook_path)?;
    if !helper.exists() {
        return Err(format!(
            "cross-bitness game capture needs the matching-bitness inject-helper, but it is \
             missing: {}",
            helper.display()
        ));
    }

    verbose_log(&format!(
        "spawning cross-bitness inject-helper {} (target_pid={target_pid})",
        helper.display()
    ));

    let status = std::process::Command::new(&helper)
        .arg(target_pid.to_string())
        .arg(hook_path)
        .status()
        .map_err(|error| {
            format!(
                "failed to launch the cross-bitness inject-helper ({}): {error}",
                helper.display()
            )
        })?;

    match status.code() {
        Some(0) => Ok(()),
        Some(code) => Err(format!(
            "cross-bitness inject-helper failed (exit {code}: {})",
            helper_stage_reason(code)
        )),
        None => Err(format!(
            "cross-bitness inject-helper terminated without an exit code ({})",
            helper.display()
        )),
    }
}

fn inject_via_set_windows_hook(
    target_hwnd: HWND,
    target_pid: u32,
    hook_path: &str,
) -> Result<InstalledWindowsHook, String> {
    let hook_path = Path::new(hook_path);
    if !hook_path.exists() {
        return Err(format!(
            "game capture hook DLL missing: {}",
            hook_path.display()
        ));
    }

    let mut resolved_pid = 0u32;
    let thread_id = unsafe { GetWindowThreadProcessId(target_hwnd, &mut resolved_pid) };
    if thread_id == 0 {
        return Err("failed to resolve target window thread for SetWindowsHookEx injection".into());
    }
    if resolved_pid != target_pid {
        return Err(
            "target window thread no longer belongs to the expected process; aborting \
             SetWindowsHookEx injection"
                .into(),
        );
    }

    let wide_path = to_wide(&hook_path.to_string_lossy());
    let module = unsafe { LoadLibraryW(wide_path.as_ptr()) };
    if module.is_null() {
        return Err(
            "LoadLibraryW failed to load the game capture hook DLL for SetWindowsHookEx injection"
                .into(),
        );
    }

    let proc = unsafe { GetProcAddress(module, HOOK_PROC_EXPORT.as_ptr()) };
    let Some(proc) = proc else {
        unsafe {
            FreeLibrary(module);
        }
        return Err(
            "GetProcAddress(FluxerGetMsgProc) failed; the hook DLL is missing the SetWindowsHookEx \
             entry point"
                .into(),
        );
    };

    type HookFn =
        unsafe extern "system" fn(i32, windows_sys::Win32::Foundation::WPARAM, LPARAM) -> isize;
    let hook_fn: HookFn = unsafe { std::mem::transmute(proc) };
    let hook =
        unsafe { SetWindowsHookExW(WH_GETMESSAGE, Some(hook_fn), module as HINSTANCE, thread_id) };
    if hook.is_null() {
        unsafe {
            FreeLibrary(module);
        }
        return Err("SetWindowsHookExW failed while injecting the game capture hook".into());
    }

    unsafe {
        PostThreadMessageW(thread_id, WM_NULL, 0, 0);
    }

    Ok(InstalledWindowsHook { hook, module })
}

fn parse_hwnd_source_id(source_id: &str) -> Option<HWND> {
    let token = source_id.strip_prefix("window:")?.split(':').next()?;
    let value = if let Some(hex) = token
        .strip_prefix("0x")
        .or_else(|| token.strip_prefix("0X"))
    {
        isize::from_str_radix(hex, 16).ok()?
    } else {
        token.parse::<isize>().ok()?
    };
    let hwnd = value as HWND;
    if unsafe { IsWindow(hwnd) } != 0 {
        Some(hwnd)
    } else {
        None
    }
}

fn parse_screen_ordinal(source_id: &str) -> Option<usize> {
    let token = source_id.strip_prefix("screen:")?.split(':').next()?;
    token.parse::<usize>().ok()
}

pub(crate) fn resolve_game_capture_target(
    source_id: &str,
    source_kind: &str,
) -> Result<HWND, String> {
    if let Some(hwnd) = parse_hwnd_source_id(source_id) {
        return Ok(hwnd);
    }
    if source_kind == "game" || source_kind == "screen" {
        let monitor = monitor_for_screen_source(source_id)?;
        if let Some(hwnd) = find_fullscreen_window_on_monitor(monitor) {
            return Ok(hwnd);
        }
        if let Some(hwnd) = find_foreground_fullscreen_window() {
            return Ok(hwnd);
        }
        if let Some(hwnd) = find_fullscreen_window_on_any_monitor() {
            return Ok(hwnd);
        }
        return Err("no fullscreen game window found on selected display".into());
    }
    Err(format!(
        "invalid game capture source: {source_kind}:{source_id}"
    ))
}

fn monitor_for_screen_source(source_id: &str) -> Result<HMONITOR, String> {
    let ordinal = parse_screen_ordinal(source_id).unwrap_or(0);
    let monitors = enumerate_monitors();
    monitors
        .get(ordinal)
        .copied()
        .or_else(|| monitors.first().copied())
        .ok_or_else(|| "no monitors available for game capture".to_string())
}

fn enumerate_monitors() -> Vec<HMONITOR> {
    unsafe extern "system" fn enum_monitor(
        monitor: HMONITOR,
        _hdc: windows_sys::Win32::Graphics::Gdi::HDC,
        _rect: *mut RECT,
        param: LPARAM,
    ) -> i32 {
        let monitors = &mut *(param as *mut Vec<HMONITOR>);
        monitors.push(monitor);
        1
    }
    let mut monitors = Vec::new();
    unsafe {
        EnumDisplayMonitors(
            null_mut(),
            null(),
            Some(enum_monitor),
            &mut monitors as *mut _ as LPARAM,
        );
    }
    monitors
}

fn target_process_id(hwnd: HWND) -> Result<u32, String> {
    let mut pid = 0u32;
    unsafe {
        GetWindowThreadProcessId(hwnd, &mut pid);
    }
    if pid == 0 {
        Err("failed to resolve target process id".into())
    } else {
        Ok(pid)
    }
}

fn target_process_machine(target_pid: u32) -> Result<IMAGE_FILE_MACHINE, String> {
    let process = unsafe {
        OpenProcess(
            PROCESS_QUERY_LIMITED_INFORMATION | PROCESS_QUERY_INFORMATION,
            0,
            target_pid,
        )
    };
    let process = OwnedHandle::new(process, "OpenProcess(query bitness)")?;

    let mut process_machine: IMAGE_FILE_MACHINE = IMAGE_FILE_MACHINE_UNKNOWN;
    let mut native_machine: IMAGE_FILE_MACHINE = IMAGE_FILE_MACHINE_UNKNOWN;
    let ok = unsafe { IsWow64Process2(process.raw(), &mut process_machine, &mut native_machine) };
    if ok != 0 {
        let effective_machine = if process_machine == IMAGE_FILE_MACHINE_UNKNOWN {
            native_machine
        } else {
            process_machine
        };
        return Ok(effective_machine);
    }

    let mut is_wow64: windows_sys::core::BOOL = 0;
    let ok = unsafe { IsWow64Process(process.raw(), &mut is_wow64) };
    if ok == 0 {
        return Err("failed to query target process bitness".into());
    }
    if is_wow64 != 0 {
        return Ok(IMAGE_FILE_MACHINE_I386);
    }
    Ok(HOST_MACHINE)
}

fn machine_is_32_bit(machine: IMAGE_FILE_MACHINE) -> Result<bool, String> {
    match machine {
        IMAGE_FILE_MACHINE_I386 | IMAGE_FILE_MACHINE_ARM | IMAGE_FILE_MACHINE_ARMNT => Ok(true),
        IMAGE_FILE_MACHINE_AMD64 | IMAGE_FILE_MACHINE_ARM64 | IMAGE_FILE_MACHINE_IA64 => Ok(false),
        _ => Err(format!(
            "unsupported target process architecture (IMAGE_FILE_MACHINE 0x{machine:04x}); \
             cannot pick a game capture hook DLL for it"
        )),
    }
}

fn window_capture_size(hwnd: HWND) -> Result<(u32, u32), String> {
    let mut rect = RECT::default();
    if unsafe { GetClientRect(hwnd, &mut rect) } != 0 {
        let width = (rect.right - rect.left).max(0) as u32;
        let height = (rect.bottom - rect.top).max(0) as u32;
        if width > 0 && height > 0 {
            return Ok((width, height));
        }
    }
    if unsafe { GetWindowRect(hwnd, &mut rect) } == 0 {
        return Err("failed to resolve target window size".into());
    }
    let width = (rect.right - rect.left).max(1) as u32;
    let height = (rect.bottom - rect.top).max(1) as u32;
    Ok((width, height))
}

fn validate_capture_size(width: u32, height: u32) -> Result<(), String> {
    if width == 0 || height == 0 {
        return Err("game capture target has zero size".into());
    }
    if width > MAX_GAME_CAPTURE_DIMENSION || height > MAX_GAME_CAPTURE_DIMENSION {
        return Err(format!("game capture target too large: {width}x{height}"));
    }
    Ok(())
}

fn rect_size(rect: &RECT) -> Option<(u32, u32)> {
    let width = (rect.right as i64 - rect.left as i64).abs();
    let height = (rect.bottom as i64 - rect.top as i64).abs();
    if width <= 0 || height <= 0 {
        return None;
    }
    Some((width as u32, height as u32))
}

fn shared_buffer_dimensions_for_window(
    hwnd: HWND,
    capture_width: u32,
    capture_height: u32,
) -> (u32, u32) {
    let mut window_rect = RECT::default();
    let monitor = if unsafe { GetWindowRect(hwnd, &mut window_rect) } != 0 {
        unsafe { MonitorFromRect(&window_rect, MONITOR_DEFAULTTONEAREST) }
    } else {
        null_mut()
    };
    let Some(monitor_rect) = (!monitor.is_null())
        .then(|| monitor_rect(monitor))
        .flatten()
    else {
        return (capture_width, capture_height);
    };
    let Some((monitor_width, monitor_height)) = rect_size(&monitor_rect) else {
        return (capture_width, capture_height);
    };
    let Some((buffer_width, buffer_height)) = choose_shared_buffer_dimensions(
        capture_width,
        capture_height,
        monitor_width,
        monitor_height,
    ) else {
        verbose_log(&format!(
            "nearest monitor buffer {monitor_width}x{monitor_height} exceeds capture mapping limit; \
             using initial window buffer {capture_width}x{capture_height}"
        ));
        return (capture_width, capture_height);
    };
    (buffer_width, buffer_height)
}

fn choose_shared_buffer_dimensions(
    capture_width: u32,
    capture_height: u32,
    monitor_width: u32,
    monitor_height: u32,
) -> Option<(u32, u32)> {
    let buffer_width = capture_width
        .max(monitor_width)
        .min(MAX_GAME_CAPTURE_DIMENSION);
    let buffer_height = capture_height
        .max(monitor_height)
        .min(MAX_GAME_CAPTURE_DIMENSION);
    let frame_map_size = shared_frame_mapping_size(buffer_width, buffer_height)?;
    if frame_map_size > MAX_GAME_CAPTURE_FRAME_BYTES {
        return None;
    }
    Some((buffer_width, buffer_height))
}

fn monitor_rect(monitor: HMONITOR) -> Option<RECT> {
    let mut info = MONITORINFO {
        cbSize: std::mem::size_of::<MONITORINFO>() as u32,
        rcMonitor: RECT::default(),
        rcWork: RECT::default(),
        dwFlags: 0,
    };
    if unsafe { GetMonitorInfoW(monitor, &mut info) } == 0 {
        return None;
    }
    Some(info.rcMonitor)
}

fn rect_matches_monitor(window_rect: &RECT, monitor_rect: &RECT) -> bool {
    let tolerance = 2;
    (window_rect.left - monitor_rect.left).abs() <= tolerance
        && (window_rect.top - monitor_rect.top).abs() <= tolerance
        && (window_rect.right - monitor_rect.right).abs() <= tolerance
        && (window_rect.bottom - monitor_rect.bottom).abs() <= tolerance
}

fn is_regular_maximized_window(hwnd: HWND) -> bool {
    let style = unsafe { GetWindowLongPtrW(hwnd, GWL_STYLE) } as u32;
    (style & WS_MAXIMIZE) != 0 && (style & WS_BORDER) != 0
}

fn is_fullscreen_window_on_monitor(hwnd: HWND, monitor: HMONITOR, monitor_rect: &RECT) -> bool {
    if unsafe { IsWindowVisible(hwnd) } == 0 || !unsafe { GetWindow(hwnd, GW_OWNER) }.is_null() {
        return false;
    }
    let mut pid = 0u32;
    unsafe {
        GetWindowThreadProcessId(hwnd, &mut pid);
    }
    if pid == 0 || pid == unsafe { GetCurrentProcessId() } {
        return false;
    }
    if is_regular_maximized_window(hwnd) {
        return false;
    }
    let mut rect = RECT::default();
    if unsafe { GetWindowRect(hwnd, &mut rect) } == 0 {
        return false;
    }
    let window_monitor = unsafe { MonitorFromRect(&rect, MONITOR_DEFAULTTONEAREST) };
    window_monitor == monitor && rect_matches_monitor(&rect, monitor_rect)
}

fn find_foreground_fullscreen_window() -> Option<HWND> {
    let hwnd = unsafe { GetForegroundWindow() };
    if hwnd.is_null() {
        return None;
    }
    let mut rect = RECT::default();
    if unsafe { GetWindowRect(hwnd, &mut rect) } == 0 {
        return None;
    }
    let monitor = unsafe { MonitorFromRect(&rect, MONITOR_DEFAULTTONEAREST) };
    let monitor_rect = monitor_rect(monitor)?;
    if is_fullscreen_window_on_monitor(hwnd, monitor, &monitor_rect) {
        Some(hwnd)
    } else {
        None
    }
}

fn find_fullscreen_window_on_any_monitor() -> Option<HWND> {
    for monitor in enumerate_monitors() {
        if let Some(hwnd) = find_fullscreen_window_on_monitor(monitor) {
            return Some(hwnd);
        }
    }
    None
}

fn find_fullscreen_window_on_monitor(monitor: HMONITOR) -> Option<HWND> {
    struct Search {
        monitor: HMONITOR,
        monitor_rect: RECT,
        result: HWND,
        own_pid: u32,
    }
    unsafe extern "system" fn enum_window(hwnd: HWND, param: LPARAM) -> i32 {
        let search = &mut *(param as *mut Search);
        let mut pid = 0u32;
        GetWindowThreadProcessId(hwnd, &mut pid);
        if pid != 0
            && pid != search.own_pid
            && is_fullscreen_window_on_monitor(hwnd, search.monitor, &search.monitor_rect)
        {
            search.result = hwnd;
            return 0;
        }
        1
    }

    let monitor_rect = monitor_rect(monitor)?;
    let mut search = Search {
        monitor,
        monitor_rect,
        result: null_mut(),
        own_pid: unsafe { GetCurrentProcessId() },
    };
    unsafe {
        let _ = EnumWindows(Some(enum_window), &mut search as *mut _ as LPARAM);
    }
    if search.result.is_null() {
        None
    } else {
        Some(search.result)
    }
}

pub(crate) fn create_shared_texture_device(
    adapter: Option<&IDXGIAdapter>,
) -> Result<(ID3D11Device, ID3D11DeviceContext), String> {
    let mut device = None;
    let mut context = None;
    let driver_type = if adapter.is_some() {
        D3D_DRIVER_TYPE_UNKNOWN
    } else {
        D3D_DRIVER_TYPE_HARDWARE
    };
    unsafe {
        D3D11CreateDevice(
            adapter,
            driver_type,
            HMODULE::default(),
            D3D11_CREATE_DEVICE_BGRA_SUPPORT,
            None,
            D3D11_SDK_VERSION,
            Some(&mut device),
            None,
            Some(&mut context),
        )
        .map_err(|e| format!("D3D11CreateDevice for shared game texture: {e}"))?;
    }
    let device = device.ok_or("D3D11 shared texture device was None")?;
    let context = context.ok_or("D3D11 shared texture context was None")?;
    set_gpu_thread_priority(&device);
    Ok((device, context))
}

fn set_gpu_thread_priority(device: &ID3D11Device) {
    if let Ok(dxgi_device) = device.cast::<IDXGIDevice>() {
        let _ = unsafe { dxgi_device.SetGPUThreadPriority(7) };
    }
}

const SHARED_TEXTURE_ADAPTER_LIMIT: u32 = 16;

fn shared_texture_adapter_candidates() -> Vec<Option<IDXGIAdapter>> {
    let mut candidates: Vec<Option<IDXGIAdapter>> =
        Vec::with_capacity(1 + SHARED_TEXTURE_ADAPTER_LIMIT as usize);
    candidates.push(None);
    if let Ok(factory) = unsafe { CreateDXGIFactory1::<IDXGIFactory1>() } {
        let mut index = 0u32;
        while index < SHARED_TEXTURE_ADAPTER_LIMIT {
            match unsafe { factory.EnumAdapters(index) } {
                Ok(adapter) => candidates.push(Some(adapter)),
                Err(_) => break,
            }
            index += 1;
        }
    }
    assert!(!candidates.is_empty(), "default device candidate present");
    assert!(
        candidates.len() <= 1 + SHARED_TEXTURE_ADAPTER_LIMIT as usize,
        "adapter candidates bounded"
    );
    candidates
}

impl SharedTextureReader {
    fn new(
        handle: u64,
        width: u32,
        height: u32,
        format: u32,
        capture_flags: u32,
        output_width: u32,
        output_height: u32,
    ) -> Result<Self, String> {
        assert!(handle != 0, "shared texture handle is non-zero");
        let source_format = crate::hdr::SourceFormat::classify(format, capture_flags)
            .ok_or_else(|| format!("unsupported game shared texture DXGI format {format}"))?;
        let mut last_error = None;
        for adapter in shared_texture_adapter_candidates() {
            let (device, context) = match create_shared_texture_device(adapter.as_ref()) {
                Ok(created) => created,
                Err(error) => {
                    last_error = Some(error);
                    continue;
                }
            };
            match Self::new_with_device(
                device,
                context,
                handle,
                width,
                height,
                format,
                source_format,
                output_width,
                output_height,
            ) {
                Ok(reader) => return Ok(reader),
                Err(error) => last_error = Some(error),
            }
        }
        Err(last_error.unwrap_or_else(|| "no D3D11 devices available for shared texture".into()))
    }

    #[allow(clippy::too_many_arguments)]
    fn new_with_device(
        device: ID3D11Device,
        context: ID3D11DeviceContext,
        handle: u64,
        width: u32,
        height: u32,
        format: u32,
        source_format: crate::hdr::SourceFormat,
        output_width: u32,
        output_height: u32,
    ) -> Result<Self, String> {
        assert!(handle != 0, "shared texture handle is non-zero");
        assert!(width > 0, "shared texture width is positive");
        assert!(height > 0, "shared texture height is positive");
        let mut texture = None;
        unsafe {
            device
                .OpenSharedResource::<ID3D11Texture2D>(
                    WinHandle(handle as usize as *mut c_void),
                    &mut texture,
                )
                .map_err(|e| format!("OpenSharedResource game texture: {e}"))?;
        }
        let texture = texture.ok_or("OpenSharedResource returned no game texture")?;
        let mut desc = D3D11_TEXTURE2D_DESC::default();
        unsafe {
            texture.GetDesc(&mut desc);
        }
        if desc.Width != width || desc.Height != height || desc.Format.0 as u32 != format {
            return Err(format!(
                "game shared texture metadata mismatch: info={width}x{height}/{format}, texture={}x{}/{}",
                desc.Width, desc.Height, desc.Format.0
            ));
        }
        let nv12 = if env_flag_enabled(ENV_FORCE_CPU) {
            None
        } else {
            Nv12GpuConverter::new(
                &device,
                &context,
                &texture,
                width,
                height,
                output_width,
                output_height,
                source_format,
            )
        };
        Ok(Self {
            device,
            context,
            handle,
            width,
            height,
            format,
            source_format,
            nv12,
        })
    }

    fn rebuild_on_existing_device(
        existing: SharedTextureReader,
        info: &GameCaptureSharedInfo,
        output_width: u32,
        output_height: u32,
    ) -> Result<Self, String> {
        assert!(
            info.texture_handle != 0,
            "shared texture handle is non-zero"
        );
        assert!(output_width > 0, "output width is positive");
        let source_format =
            crate::hdr::SourceFormat::classify(info.dxgi_format, info.capture_flags).ok_or_else(
                || {
                    format!(
                        "unsupported game shared texture DXGI format {}",
                        info.dxgi_format
                    )
                },
            )?;
        Self::new_with_device(
            existing.device,
            existing.context,
            info.texture_handle,
            info.width,
            info.height,
            info.dxgi_format,
            source_format,
            output_width,
            output_height,
        )
    }

    fn matches(
        &self,
        handle: u64,
        width: u32,
        height: u32,
        format: u32,
        capture_flags: u32,
    ) -> bool {
        self.handle == handle
            && self.width == width
            && self.height == height
            && self.format == format
            && Some(self.source_format) == crate::hdr::SourceFormat::classify(format, capture_flags)
    }
}

fn emit_shared_texture_to_bus(
    reader: &mut Option<SharedTextureReader>,
    session: &GameCaptureSession,
    info: &GameCaptureSharedInfo,
    inner: &CaptureInner,
    frame_sink: &crate::FrameSinkRef,
) -> Result<bool, String> {
    let reader = shared_texture_reader(reader, session, info)?;
    let timestamp_us = shared_texture_timestamp(info);

    if let Some(nv12) = reader.nv12.as_mut() {
        match nv12.convert_shared_texture() {
            Ok(frame) => {
                session.set_native_texture_info(Some(NativeTextureHandleInfo {
                    handle: frame.handle,
                    width: frame.width,
                    height: frame.height,
                    dxgi_format: frame.dxgi_format,
                    timestamp_us,
                }));
                return Ok(emit_shared_texture_frame(
                    inner,
                    frame_sink,
                    frame.handle,
                    frame.width,
                    frame.height,
                    frame.dxgi_format,
                    timestamp_us,
                ));
            }
            Err(error) => {
                verbose_log(&format!(
                    "disabling GPU NV12 conversion for this shared texture after failure: {error}"
                ));
                reader.nv12 = None;
                session.set_native_texture_info(None);
            }
        }
    }

    session.set_native_texture_info(Some(NativeTextureHandleInfo {
        handle: reader.handle,
        width: reader.width,
        height: reader.height,
        dxgi_format: reader.format,
        timestamp_us,
    }));
    Ok(emit_shared_texture_frame(
        inner,
        frame_sink,
        reader.handle,
        reader.width,
        reader.height,
        reader.format,
        timestamp_us,
    ))
}

fn shared_texture_reader<'a>(
    reader: &'a mut Option<SharedTextureReader>,
    session: &GameCaptureSession,
    info: &GameCaptureSharedInfo,
) -> Result<&'a mut SharedTextureReader, String> {
    let handle = info.texture_handle;
    if handle == 0 {
        return Err("game shared texture handle was empty".into());
    }
    if reader.as_ref().map(|reader| {
        reader.matches(
            handle,
            info.width,
            info.height,
            info.dxgi_format,
            info.capture_flags,
        )
    }) != Some(true)
    {
        let reused = reader.take().and_then(|existing| {
            SharedTextureReader::rebuild_on_existing_device(
                existing,
                info,
                session.output_width,
                session.output_height,
            )
            .ok()
        });
        *reader = Some(match reused {
            Some(rebuilt) => rebuilt,
            None => SharedTextureReader::new(
                handle,
                info.width,
                info.height,
                info.dxgi_format,
                info.capture_flags,
                session.output_width,
                session.output_height,
            )?,
        });
    }
    let reader = reader
        .as_mut()
        .ok_or_else(|| "game shared texture reader was not initialized".to_string())?;
    Ok(reader)
}

fn shared_texture_timestamp(info: &GameCaptureSharedInfo) -> i64 {
    if info.timestamp_us > 0 {
        info.timestamp_us
    } else {
        wall_clock_us()
    }
}

struct StallTracker {
    last_frame_counter: Option<u64>,
    last_change_at: std::time::Instant,
    stall_emitted: bool,
}

impl StallTracker {
    fn new() -> Self {
        Self {
            last_frame_counter: None,
            last_change_at: std::time::Instant::now(),
            stall_emitted: false,
        }
    }

    fn observe(
        &mut self,
        inner: &Arc<CaptureInner>,
        session: &GameCaptureSession,
        info: &GameCaptureSharedInfo,
    ) {
        if info.magic != crate::game_capture_abi::GAME_CAPTURE_MAGIC {
            self.last_change_at = std::time::Instant::now();
            return;
        }

        let counter = info.frame_counter;
        let advanced = match self.last_frame_counter {
            Some(previous) => counter != previous,
            None => {
                self.last_frame_counter = Some(counter);
                self.last_change_at = std::time::Instant::now();
                return;
            }
        };

        if advanced {
            self.last_frame_counter = Some(counter);
            self.last_change_at = std::time::Instant::now();
            if self.stall_emitted {
                self.stall_emitted = false;
                emit_lifecycle(
                    inner,
                    "diagnostic",
                    "game capture frames resumed after a stall",
                );
            }
            return;
        }

        if self.stall_emitted {
            return;
        }
        if self.last_change_at.elapsed() < STALL_THRESHOLD {
            return;
        }
        if unsafe { IsWindow(session.target_hwnd) } == 0 {
            return;
        }
        if unsafe { IsWindowVisible(session.target_hwnd) } == 0 {
            return;
        }

        let presenting_recently = presented_recently(
            info.present_clock,
            info.last_present_timestamp_us,
            wall_clock_us(),
            qpc_now_us(),
            RECENT_PRESENT_WINDOW_US,
        );
        let stalled_ms = self.last_change_at.elapsed().as_millis();
        let detail = if presenting_recently {
            format!(
                "game capture stalled: producer is still presenting (api={}, transport={}) but \
                 has published no new frame for {stalled_ms}ms; the fast path appears to be \
                 failing (fallback_reason={}, dropped={})",
                info.api_type, info.transport, info.fallback_reason, info.dropped_frame_counter
            )
        } else {
            format!(
                "game capture stalled: no presents observed for {stalled_ms}ms (api={}, \
                 transport={}, last_present_us={}); the game may be paused, occluded, or no \
                 longer rendering",
                info.api_type, info.transport, info.last_present_timestamp_us
            )
        };
        emit_lifecycle(inner, "stalled", &detail);
        self.stall_emitted = true;
    }
}

pub fn capture_loop(inner: &Arc<CaptureInner>, _frame_interval: std::time::Duration) {
    let session = {
        let guard = inner.game_session.lock();
        match guard.as_ref() {
            Some(session) => Arc::clone(session),
            None => {
                emit_lifecycle(inner, "closed-clean", "no game capture session");
                return;
            }
        }
    };

    {
        let requested = session.requested_injection_method();
        let used = session.used_injection_method();
        let detail = if requested == used {
            format!("game capture injected via {used}")
        } else {
            format!("game capture injected via {used} (requested {requested})")
        };
        emit_lifecycle(inner, "diagnostic", &detail);
    }

    let capture_id = inner.capture_id.lock().clone();
    let mut shared_texture_reader: Option<SharedTextureReader> = None;
    let mut stall_tracker = StallTracker::new();
    while inner.running.load(Ordering::Acquire) {
        if unsafe { IsWindow(session.target_hwnd) } == 0 {
            emit_lifecycle(inner, "closed", "game window closed");
            break;
        }
        let wait = unsafe { WaitForSingleObject(session.ready_event.raw(), FRAME_EVENT_WAIT_MS) };
        if wait == WAIT_TIMEOUT {
            let info = unsafe { std::ptr::read_volatile(session.info.ptr) };
            stall_tracker.observe(inner, &session, &info);
            continue;
        }
        if wait != WAIT_OBJECT_0 && wait != WAIT_ABANDONED {
            emit_lifecycle(inner, "error", "game capture frame event wait failed");
            break;
        }
        let info = unsafe { std::ptr::read_volatile(session.info.ptr) };
        stall_tracker.observe(inner, &session, &info);
        if info.state == GAME_CAPTURE_STATE_ERROR {
            emit_lifecycle(
                inner,
                "diagnostic",
                &format!("game capture hook reported error {}", info.last_error),
            );
            if !handle_fallback_signature(inner, fallback::FailureSignature::DeviceLost) {
                break;
            }
            continue;
        }
        if info.state == GAME_CAPTURE_STATE_RESIZE_REQUIRED {
            handle_fallback_signature(inner, fallback::FailureSignature::UnsupportedFormat);
            break;
        }
        if info.state == GAME_CAPTURE_STATE_STOPPED {
            emit_lifecycle(inner, "closed", "game capture hook stopped");
            break;
        }
        if info.width == 0 || info.height == 0 || info.pitch == 0 {
            continue;
        }
        if info.width > session.capture_width || info.height > session.capture_height {
            emit_lifecycle(inner, "error", "game capture frame exceeded shared buffer");
            handle_fallback_signature(inner, fallback::FailureSignature::UnsupportedFormat);
            break;
        }
        let Some(frame_sink) = crate::resolve_frame_sink(inner, capture_id.as_deref()) else {
            note_media_frame_without_sink(
                inner,
                "Windows game capture frame dropped because no native frame sink is registered",
            );
            continue;
        };
        if info.transport == GAME_CAPTURE_TRANSPORT_SHARED_TEXTURE {
            match emit_shared_texture_to_bus(
                &mut shared_texture_reader,
                &session,
                &info,
                inner,
                &frame_sink,
            ) {
                Ok(_) => continue,
                Err(error) => {
                    if info.api_type == GAME_CAPTURE_API_OPENGL {
                        emit_lifecycle(
                            inner,
                            "diagnostic",
                            &format!(
                                "OpenGL shared texture was rejected by the parent reader; \
                                 requesting hook CPU fallback: {error}"
                            ),
                        );
                        session.request_hook_disable_shared_texture();
                        shared_texture_reader = None;
                        continue;
                    }
                    emit_lifecycle(inner, "error", &error);
                    handle_fallback_signature(
                        inner,
                        fallback::FailureSignature::UnsupportedTransport,
                    );
                    break;
                }
            }
        }
        if info.transport != GAME_CAPTURE_TRANSPORT_MEMORY {
            emit_lifecycle(
                inner,
                "error",
                "game capture hook used unknown frame transport",
            );
            handle_fallback_signature(inner, fallback::FailureSignature::UnsupportedTransport);
            break;
        }
        note_cpu_fallback_frame_dropped(
            inner,
            "Windows game capture CPU-memory transport dropped; native sender requires shared textures",
        );
    }

    inner.running.store(false, Ordering::Release);
    emit_lifecycle(inner, "closed-clean", "game capture stopped");
}

fn handle_fallback_signature(
    inner: &Arc<CaptureInner>,
    signature: fallback::FailureSignature,
) -> bool {
    matches!(
        crate::observe_fallback(inner, signature),
        Some(fallback::FallbackDecision::Stay {
            retry_in_place: true
        })
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shared_buffer_grows_to_nearest_monitor_for_window_resize_recovery() {
        assert_eq!(
            choose_shared_buffer_dimensions(640, 360, 1920, 1080),
            Some((1920, 1080))
        );
    }

    #[test]
    fn shared_buffer_keeps_initial_size_when_window_is_larger_than_monitor() {
        assert_eq!(
            choose_shared_buffer_dimensions(2560, 1440, 1920, 1080),
            Some((2560, 1440))
        );
    }

    #[test]
    fn shared_buffer_rejects_unreasonably_large_monitor_mapping() {
        assert_eq!(
            choose_shared_buffer_dimensions(
                640,
                360,
                MAX_GAME_CAPTURE_DIMENSION,
                MAX_GAME_CAPTURE_DIMENSION
            ),
            None
        );
    }

    #[test]
    fn emulated_x64_target_on_arm64_host_is_not_classified_as_32_bit() {
        assert_eq!(machine_is_32_bit(IMAGE_FILE_MACHINE_AMD64), Ok(false));
        assert_eq!(machine_is_32_bit(IMAGE_FILE_MACHINE_ARM64), Ok(false));
    }

    #[test]
    fn wow64_guest_machines_are_classified_as_32_bit() {
        assert_eq!(machine_is_32_bit(IMAGE_FILE_MACHINE_I386), Ok(true));
        assert_eq!(machine_is_32_bit(IMAGE_FILE_MACHINE_ARMNT), Ok(true));
        assert_eq!(machine_is_32_bit(IMAGE_FILE_MACHINE_ARM), Ok(true));
    }

    #[test]
    fn unknown_target_machine_fails_loudly_instead_of_guessing_bitness() {
        assert!(machine_is_32_bit(IMAGE_FILE_MACHINE_UNKNOWN).is_err());
    }
}
