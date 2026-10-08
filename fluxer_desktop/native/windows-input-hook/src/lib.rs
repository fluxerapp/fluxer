#![cfg_attr(not(target_os = "windows"), allow(dead_code))]

// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::{
    Env,
    bindgen_prelude::{Function, Object, Result, ToNapiValue},
    sys,
    threadsafe_function::{ThreadsafeFunction, UnknownReturnValue},
};
use napi_derive::napi;

const KEY_NAME_BUF: usize = 32;
const LLKHF_EXTENDED_FLAG: u32 = 0x01;
const VK_PACKET: u32 = 0xe7;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum EventKind {
    #[default]
    KeyDown,
    KeyUp,
    MouseDown,
    MouseUp,
}

impl EventKind {
    fn as_str(self) -> &'static str {
        match self {
            Self::KeyDown => "keydown",
            Self::KeyUp => "keyup",
            Self::MouseDown => "mousedown",
            Self::MouseUp => "mouseup",
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct QueuedEvent {
    pub kind: EventKind,
    pub keycode: u32,
    pub scan_code: u32,
    pub extended: bool,
    pub button: u8,
    pub x: i32,
    pub y: i32,
    pub has_xy: bool,
    pub key_name_buf: [u8; KEY_NAME_BUF],
    pub key_name_len: u8,
    pub ctrl: bool,
    pub alt: bool,
    pub shift: bool,
    pub meta: bool,
}

impl QueuedEvent {
    fn key_name(&self) -> &str {
        let len = (self.key_name_len as usize).min(KEY_NAME_BUF);
        std::str::from_utf8(&self.key_name_buf[..len]).unwrap_or("")
    }

    fn same_input(&self, other: &QueuedEvent) -> bool {
        match self.kind {
            EventKind::KeyDown | EventKind::KeyUp => {
                matches!(other.kind, EventKind::KeyDown | EventKind::KeyUp)
                    && self.keycode == other.keycode
                    && self.scan_code == other.scan_code
                    && self.extended == other.extended
            }
            EventKind::MouseDown | EventKind::MouseUp => {
                matches!(other.kind, EventKind::MouseDown | EventKind::MouseUp)
                    && self.button == other.button
            }
        }
    }

    fn release(&self) -> QueuedEvent {
        QueuedEvent {
            kind: match self.kind {
                EventKind::KeyDown | EventKind::KeyUp => EventKind::KeyUp,
                EventKind::MouseDown | EventKind::MouseUp => EventKind::MouseUp,
            },
            has_xy: false,
            x: 0,
            y: 0,
            ctrl: false,
            alt: false,
            shift: false,
            meta: false,
            ..*self
        }
    }
}

#[derive(Default)]
struct HeldInputs {
    pressed: Vec<QueuedEvent>,
}

impl HeldInputs {
    fn track(&mut self, event: &QueuedEvent) {
        self.pressed.retain(|held| !held.same_input(event));
        if matches!(event.kind, EventKind::KeyDown | EventKind::MouseDown) {
            self.pressed.push(*event);
        }
    }

    fn take_releases(&mut self) -> Vec<QueuedEvent> {
        std::mem::take(&mut self.pressed)
            .iter()
            .map(QueuedEvent::release)
            .collect()
    }
}

fn write_ascii_key_name(buf: &mut [u8; KEY_NAME_BUF], len_out: &mut u8, name: &str) {
    let bytes = name.as_bytes();
    let n = bytes.len().min(KEY_NAME_BUF);
    buf[..n].copy_from_slice(&bytes[..n]);
    *len_out = n as u8;
}

fn scan_aware_key_name(vk: u16, flags: u32) -> Option<&'static str> {
    let extended = (flags & LLKHF_EXTENDED_FLAG) != 0;
    match (vk, extended) {
        (0x0d, true) => Some("NumpadEnter"),
        (0x2d, false) => Some("Numpad0"),
        (0x23, false) => Some("Numpad1"),
        (0x28, false) => Some("Numpad2"),
        (0x22, false) => Some("Numpad3"),
        (0x25, false) => Some("Numpad4"),
        (0x0c, false) => Some("Numpad5"),
        (0x27, false) => Some("Numpad6"),
        (0x24, false) => Some("Numpad7"),
        (0x26, false) => Some("Numpad8"),
        (0x21, false) => Some("Numpad9"),
        (0x2e, false) => Some("NumpadDecimal"),
        _ => None,
    }
}

pub fn write_key_name(buf: &mut [u8; KEY_NAME_BUF], len_out: &mut u8, vk: u16) {
    use fluxer_desktop_native::input::keymap::{fallback_name, windows::vk_to_name};
    if let Some(name) = vk_to_name(vk) {
        write_ascii_key_name(buf, len_out, name);
    } else {
        write_ascii_key_name(buf, len_out, &fallback_name(vk));
    }
}

pub fn write_packet_key_name(buf: &mut [u8; KEY_NAME_BUF], len_out: &mut u8, unit: u32) {
    match char::from_u32(unit) {
        Some(' ') => write_ascii_key_name(buf, len_out, "Space"),
        Some(ch) if ch.is_ascii_alphanumeric() => {
            let mut utf8 = [0u8; 4];
            write_ascii_key_name(buf, len_out, ch.to_ascii_uppercase().encode_utf8(&mut utf8));
        }
        _ => *len_out = 0,
    }
}

pub fn write_key_name_from_hook(
    buf: &mut [u8; KEY_NAME_BUF],
    len_out: &mut u8,
    vk: u16,
    flags: u32,
) {
    if let Some(name) = scan_aware_key_name(vk, flags) {
        write_ascii_key_name(buf, len_out, name);
        return;
    }
    write_key_name(buf, len_out, vk);
}

impl ToNapiValue for QueuedEvent {
    unsafe fn to_napi_value(raw_env: sys::napi_env, event: Self) -> Result<sys::napi_value> {
        let env = Env::from_raw(raw_env);
        let mut object = Object::new(&env)?;
        object.set("type", event.kind.as_str())?;
        object.set("ctrlKey", event.ctrl)?;
        object.set("altKey", event.alt)?;
        object.set("shiftKey", event.shift)?;
        object.set("metaKey", event.meta)?;
        match event.kind {
            EventKind::KeyDown | EventKind::KeyUp => {
                object.set("keycode", event.keycode)?;
                object.set("keyName", event.key_name())?;
                object.set("scanCode", event.scan_code)?;
                object.set("extended", event.extended)?;
            }
            EventKind::MouseDown | EventKind::MouseUp => {
                object.set("button", u32::from(event.button))?;
                if event.has_xy {
                    object.set("x", event.x)?;
                    object.set("y", event.y)?;
                }
            }
        }
        unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, object) }
    }
}

type EventThreadsafeFunction =
    ThreadsafeFunction<QueuedEvent, UnknownReturnValue, QueuedEvent, napi::Status, false, true>;

#[cfg(target_os = "windows")]
mod platform {
    use super::{
        EventKind, EventThreadsafeFunction, HeldInputs, QueuedEvent, VK_PACKET,
        write_key_name_from_hook, write_packet_key_name,
    };
    use fluxer_desktop_native::input::modifiers::{
        Modifiers,
        windows::{from_sampled, with_pressed_key},
    };
    use napi::threadsafe_function::ThreadsafeFunctionCallMode;
    use std::sync::{
        Arc, Mutex, MutexGuard, OnceLock,
        atomic::{AtomicBool, AtomicI32, AtomicU64, Ordering},
        mpsc,
    };
    use std::thread::{self, JoinHandle};
    use std::time::Duration;
    use windows::Win32::Foundation::{LPARAM, LRESULT, POINT, WPARAM};
    use windows::Win32::System::LibraryLoader::GetModuleHandleW;
    use windows::Win32::UI::Input::KeyboardAndMouse::{
        GetKeyState, GetKeyboardLayout, MAPVK_VSC_TO_VK_EX, MapVirtualKeyExW, VK_CONTROL, VK_LWIN,
        VK_MENU, VK_RWIN, VK_SHIFT,
    };
    use windows::Win32::UI::WindowsAndMessaging::{
        CallNextHookEx, DispatchMessageW, GetCursorPos, GetForegroundWindow, GetMessageW,
        GetWindowThreadProcessId, HHOOK, KBDLLHOOKSTRUCT, MSG, MSLLHOOKSTRUCT, PM_NOREMOVE,
        PeekMessageW, PostThreadMessageW, SetWindowsHookExW, TranslateMessage, UnhookWindowsHookEx,
        WH_KEYBOARD_LL, WH_MOUSE_LL, WM_APP, WM_KEYDOWN, WM_KEYUP, WM_QUIT, WM_SYSKEYDOWN,
        WM_SYSKEYUP,
    };

    const WATCHDOG_INTERVAL_MS: u64 = 1000;
    const WATCHDOG_TIMEOUT_MS: u64 = 5000;
    const WM_APP_REINSTALL: u32 = WM_APP + 1;

    pub(crate) struct HookHandle {
        pub(crate) tsfn: EventThreadsafeFunction,
        pub(crate) dropped_events: AtomicU64,
        pub(crate) keyboard_hook: Mutex<Option<HHOOK>>,
        pub(crate) mouse_hook: Mutex<Option<HHOOK>>,
        pub(crate) thread_id: AtomicU64,
        pub(crate) started: AtomicBool,
        pub(crate) watchdog_stop: AtomicBool,
        pub(crate) worker_thread: Mutex<Option<JoinHandle<()>>>,
        pub(crate) watchdog_thread: Mutex<Option<JoinHandle<()>>>,
        pub(crate) last_cursor_x: AtomicI32,
        pub(crate) last_cursor_y: AtomicI32,
        pub(crate) last_event_ms: AtomicU64,
        pub(crate) last_cursor_change_ms: AtomicU64,
        pub(crate) reinstall_count: AtomicU64,
        held: Mutex<HeldInputs>,
    }

    unsafe impl Send for HookHandle {}
    unsafe impl Sync for HookHandle {}

    static G_HANDLE: OnceLock<Mutex<Option<Arc<HookHandle>>>> = OnceLock::new();

    fn handle_slot() -> &'static Mutex<Option<Arc<HookHandle>>> {
        G_HANDLE.get_or_init(|| Mutex::new(None))
    }

    fn lock_recover<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
        mutex
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn current_handle() -> Option<Arc<HookHandle>> {
        lock_recover(handle_slot()).clone()
    }

    fn clear_handle_slot(handle: &HookHandle) {
        let mut slot = lock_recover(handle_slot());
        if let Some(existing) = slot.as_ref()
            && std::ptr::eq(existing.as_ref(), handle)
        {
            *slot = None;
        }
    }

    fn now_ms() -> u64 {
        unsafe { windows::Win32::System::SystemInformation::GetTickCount64() }
    }

    fn sample_modifiers() -> Modifiers {
        unsafe {
            from_sampled(
                GetKeyState(VK_SHIFT.0 as i32) as u16,
                GetKeyState(VK_CONTROL.0 as i32) as u16,
                GetKeyState(VK_MENU.0 as i32) as u16,
                GetKeyState(VK_LWIN.0 as i32) as u16,
                GetKeyState(VK_RWIN.0 as i32) as u16,
            )
        }
    }

    fn foreground_keyboard_thread_id() -> u32 {
        let hwnd = unsafe { GetForegroundWindow() };
        if hwnd.0.is_null() {
            return 0;
        }
        unsafe { GetWindowThreadProcessId(hwnd, None) }
    }

    fn layout_aware_vk_from_scan_code(fallback_vk: u16, scan_code: u32, flags: u32) -> u16 {
        let mut scan = scan_code;
        if (flags & super::LLKHF_EXTENDED_FLAG) != 0 {
            scan |= 0xe000;
        }
        let layout = unsafe { GetKeyboardLayout(foreground_keyboard_thread_id()) };
        let mapped = unsafe { MapVirtualKeyExW(scan, MAPVK_VSC_TO_VK_EX, Some(layout)) };
        if mapped == 0 {
            fallback_vk
        } else {
            mapped as u16
        }
    }

    fn enqueue(handle: &HookHandle, event: QueuedEvent) {
        handle.last_event_ms.store(now_ms(), Ordering::Release);
        lock_recover(&handle.held).track(&event);
        send(handle, event);
    }

    fn send(handle: &HookHandle, event: QueuedEvent) {
        let status = handle
            .tsfn
            .call(event, ThreadsafeFunctionCallMode::NonBlocking);
        if !matches!(status, napi::Status::Ok) {
            handle.dropped_events.fetch_add(1, Ordering::Relaxed);
        }
    }

    unsafe extern "system" fn keyboard_proc(code: i32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
        if code >= 0
            && let Some(handle) = current_handle()
        {
            let info = unsafe { &*(lparam.0 as *const KBDLLHOOKSTRUCT) };
            let vk = info.vkCode as u16;
            let msg = wparam.0 as u32;
            let is_down = msg == WM_KEYDOWN || msg == WM_SYSKEYDOWN;
            let is_up = msg == WM_KEYUP || msg == WM_SYSKEYUP;
            if is_down || is_up {
                let sampled = sample_modifiers();
                let Modifiers {
                    ctrl,
                    alt,
                    shift,
                    meta,
                } = if is_down {
                    with_pressed_key(sampled, vk)
                } else {
                    sampled
                };
                let mut event = QueuedEvent {
                    kind: if is_down {
                        EventKind::KeyDown
                    } else {
                        EventKind::KeyUp
                    },
                    keycode: info.vkCode,
                    scan_code: info.scanCode,
                    extended: (info.flags.0 & super::LLKHF_EXTENDED_FLAG) != 0,
                    ctrl,
                    alt,
                    shift,
                    meta,
                    ..QueuedEvent::default()
                };
                if info.vkCode == VK_PACKET {
                    write_packet_key_name(
                        &mut event.key_name_buf,
                        &mut event.key_name_len,
                        info.scanCode,
                    );
                } else {
                    let layout_vk = layout_aware_vk_from_scan_code(vk, info.scanCode, info.flags.0);
                    write_key_name_from_hook(
                        &mut event.key_name_buf,
                        &mut event.key_name_len,
                        layout_vk,
                        info.flags.0,
                    );
                }
                enqueue(&handle, event);
            }
        }
        unsafe { CallNextHookEx(None, code, wparam, lparam) }
    }

    unsafe extern "system" fn mouse_proc(code: i32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
        use fluxer_desktop_native::input::windows_mouse::{Action, classify};
        if code >= 0
            && let Some(handle) = current_handle()
        {
            let info = unsafe { &*(lparam.0 as *const MSLLHOOKSTRUCT) };
            let md = info.mouseData;
            let x_button = ((md >> 16) & 0xffff) as u16;
            let action = classify(wparam.0 as u32, x_button);
            match action {
                Action::Button { down, button } => {
                    let Modifiers {
                        ctrl,
                        alt,
                        shift,
                        meta,
                    } = sample_modifiers();
                    enqueue(
                        &handle,
                        QueuedEvent {
                            kind: if down {
                                EventKind::MouseDown
                            } else {
                                EventKind::MouseUp
                            },
                            button,
                            x: info.pt.x,
                            y: info.pt.y,
                            has_xy: true,
                            ctrl,
                            alt,
                            shift,
                            meta,
                            ..QueuedEvent::default()
                        },
                    );
                }
                Action::Activity => {
                    handle.last_event_ms.store(now_ms(), Ordering::Release);
                }
                Action::Ignored => {}
            }
        }
        unsafe { CallNextHookEx(None, code, wparam, lparam) }
    }

    fn install_hooks(handle: &HookHandle) -> Result<(), String> {
        unsafe {
            let hmod =
                GetModuleHandleW(None).map_err(|err| format!("GetModuleHandleW failed: {err}"))?;
            let mut kbd = lock_recover(&handle.keyboard_hook);
            if kbd.is_none() {
                let hook =
                    SetWindowsHookExW(WH_KEYBOARD_LL, Some(keyboard_proc), Some(hmod.into()), 0)
                        .map_err(|err| format!("SetWindowsHookExW keyboard failed: {err}"))?;
                *kbd = Some(hook);
            }
            drop(kbd);
            let mut mouse = lock_recover(&handle.mouse_hook);
            if mouse.is_none() {
                let hook = SetWindowsHookExW(WH_MOUSE_LL, Some(mouse_proc), Some(hmod.into()), 0)
                    .map_err(|err| format!("SetWindowsHookExW mouse failed: {err}"))?;
                *mouse = Some(hook);
            }
        }
        Ok(())
    }

    fn uninstall_hooks(handle: &HookHandle) {
        if let Some(hook) = lock_recover(&handle.keyboard_hook).take() {
            let _ = unsafe { UnhookWindowsHookEx(hook) };
        }
        if let Some(hook) = lock_recover(&handle.mouse_hook).take() {
            let _ = unsafe { UnhookWindowsHookEx(hook) };
        }
    }

    fn worker_main(handle: Arc<HookHandle>, ready: mpsc::Sender<Result<(), String>>) {
        unsafe {
            let mut msg = MSG::default();

            let _ = PeekMessageW(
                &mut msg,
                None,
                WM_APP_REINSTALL,
                WM_APP_REINSTALL,
                PM_NOREMOVE,
            );
        }
        handle.thread_id.store(
            u64::from(unsafe { windows::Win32::System::Threading::GetCurrentThreadId() }),
            Ordering::Release,
        );
        if let Err(err) = install_hooks(&handle) {
            uninstall_hooks(&handle);
            handle.thread_id.store(0, Ordering::Release);
            let _ = ready.send(Err(err));
            return;
        }
        handle.last_event_ms.store(now_ms(), Ordering::Release);
        let _ = ready.send(Ok(()));
        let mut msg = MSG::default();
        loop {
            let got = unsafe { GetMessageW(&mut msg, None, 0, 0) };
            if got.0 <= 0 {
                break;
            }
            if msg.message == WM_APP_REINSTALL {
                let releases = lock_recover(&handle.held).take_releases();
                for event in releases {
                    send(&handle, event);
                }
                uninstall_hooks(&handle);
                if install_hooks(&handle).is_ok() {
                    handle.last_event_ms.store(now_ms(), Ordering::Release);
                    handle.reinstall_count.fetch_add(1, Ordering::Relaxed);
                } else {
                    uninstall_hooks(&handle);
                    handle.last_event_ms.store(now_ms(), Ordering::Release);
                }
                continue;
            }
            unsafe {
                let _ = TranslateMessage(&msg);
                DispatchMessageW(&msg);
            }
        }
        uninstall_hooks(&handle);
        handle.thread_id.store(0, Ordering::Release);
    }

    fn watchdog_main(handle: Arc<HookHandle>) {
        while !handle.watchdog_stop.load(Ordering::Acquire) {
            thread::sleep(Duration::from_millis(WATCHDOG_INTERVAL_MS));
            if handle.watchdog_stop.load(Ordering::Acquire) {
                break;
            }
            let mut pt = POINT::default();
            if unsafe { GetCursorPos(&mut pt) }.is_err() {
                continue;
            }
            let now = now_ms();
            let lx = handle.last_cursor_x.load(Ordering::Relaxed);
            let ly = handle.last_cursor_y.load(Ordering::Relaxed);
            if pt.x != lx || pt.y != ly {
                handle.last_cursor_x.store(pt.x, Ordering::Relaxed);
                handle.last_cursor_y.store(pt.y, Ordering::Relaxed);
                handle.last_cursor_change_ms.store(now, Ordering::Release);
            }
            let last_event = handle.last_event_ms.load(Ordering::Acquire);
            let last_cursor_change = handle.last_cursor_change_ms.load(Ordering::Acquire);
            if last_cursor_change > last_event && now.wrapping_sub(last_event) > WATCHDOG_TIMEOUT_MS
            {
                let tid = handle.thread_id.load(Ordering::Acquire) as u32;
                if tid != 0 {
                    unsafe {
                        let _ = PostThreadMessageW(tid, WM_APP_REINSTALL, WPARAM(0), LPARAM(0));
                    }
                    handle.last_event_ms.store(now, Ordering::Release);
                }
            }
        }
    }

    pub(crate) fn start(handle: Arc<HookHandle>) -> Result<(), String> {
        let mut slot = lock_recover(handle_slot());
        if let Some(existing) = slot.as_ref()
            && !Arc::ptr_eq(existing, &handle)
        {
            return Err("another InputHook is already active".to_owned());
        }
        if handle.started.swap(true, Ordering::AcqRel) {
            return Ok(());
        }
        *slot = Some(handle.clone());
        drop(slot);

        handle.watchdog_stop.store(false, Ordering::Release);
        let now = now_ms();
        handle.last_event_ms.store(now, Ordering::Release);
        handle.last_cursor_change_ms.store(now, Ordering::Release);
        let mut pt = POINT::default();
        if unsafe { GetCursorPos(&mut pt) }.is_ok() {
            handle.last_cursor_x.store(pt.x, Ordering::Relaxed);
            handle.last_cursor_y.store(pt.y, Ordering::Relaxed);
        }

        let (ready_tx, ready_rx) = mpsc::channel();
        let worker = {
            let h = handle.clone();
            match thread::Builder::new()
                .name("fluxer-input-hook-worker".into())
                .spawn(move || worker_main(h, ready_tx))
            {
                Ok(worker) => worker,
                Err(_) => {
                    handle.started.store(false, Ordering::Release);
                    clear_handle_slot(&handle);
                    return Err("failed to spawn worker thread".to_owned());
                }
            }
        };
        match ready_rx.recv() {
            Ok(Ok(())) => {}
            Ok(Err(err)) => {
                handle.started.store(false, Ordering::Release);
                handle.thread_id.store(0, Ordering::Release);
                clear_handle_slot(&handle);
                let _ = worker.join();
                return Err(err);
            }
            Err(_) => {
                handle.started.store(false, Ordering::Release);
                handle.thread_id.store(0, Ordering::Release);
                clear_handle_slot(&handle);
                let _ = worker.join();
                return Err("worker thread exited before installing hooks".to_owned());
            }
        }
        *lock_recover(&handle.worker_thread) = Some(worker);

        let watchdog = {
            let h = handle.clone();
            match thread::Builder::new()
                .name("fluxer-input-hook-watchdog".into())
                .spawn(move || watchdog_main(h))
            {
                Ok(watchdog) => watchdog,
                Err(_) => {
                    stop(&handle);
                    return Err("failed to spawn watchdog thread".to_owned());
                }
            }
        };
        *lock_recover(&handle.watchdog_thread) = Some(watchdog);
        Ok(())
    }

    pub(crate) fn stop(handle: &HookHandle) {
        if !handle.started.swap(false, Ordering::AcqRel) {
            return;
        }
        handle.watchdog_stop.store(true, Ordering::Release);
        let tid = handle.thread_id.swap(0, Ordering::AcqRel) as u32;
        if tid != 0 {
            unsafe {
                let _ = PostThreadMessageW(tid, WM_QUIT, WPARAM(0), LPARAM(0));
            }
        }
        if let Some(t) = lock_recover(&handle.worker_thread).take() {
            let _ = t.join();
        }
        if let Some(t) = lock_recover(&handle.watchdog_thread).take() {
            let _ = t.join();
        }
        clear_handle_slot(handle);
    }

    pub(crate) fn new_handle(tsfn: EventThreadsafeFunction) -> Arc<HookHandle> {
        Arc::new(HookHandle {
            tsfn,
            dropped_events: AtomicU64::new(0),
            keyboard_hook: Mutex::new(None),
            mouse_hook: Mutex::new(None),
            thread_id: AtomicU64::new(0),
            started: AtomicBool::new(false),
            watchdog_stop: AtomicBool::new(false),
            worker_thread: Mutex::new(None),
            watchdog_thread: Mutex::new(None),
            last_cursor_x: AtomicI32::new(0),
            last_cursor_y: AtomicI32::new(0),
            last_event_ms: AtomicU64::new(0),
            last_cursor_change_ms: AtomicU64::new(0),
            reinstall_count: AtomicU64::new(0),
            held: Mutex::new(HeldInputs::default()),
        })
    }
}

#[cfg(not(target_os = "windows"))]
mod platform {
    use super::EventThreadsafeFunction;
    use std::sync::Arc;
    use std::sync::atomic::AtomicU64;

    pub(crate) struct HookHandle {
        pub(crate) tsfn: EventThreadsafeFunction,
        pub(crate) dropped_events: AtomicU64,
        pub(crate) reinstall_count: AtomicU64,
    }

    pub(crate) fn new_handle(tsfn: EventThreadsafeFunction) -> Arc<HookHandle> {
        Arc::new(HookHandle {
            tsfn,
            dropped_events: AtomicU64::new(0),
            reinstall_count: AtomicU64::new(0),
        })
    }

    pub(crate) fn start(_handle: Arc<HookHandle>) -> Result<(), String> {
        Err("windows-input-hook is only supported on Windows".to_owned())
    }

    pub(crate) fn stop(_handle: &HookHandle) {}
}

use platform::HookHandle;

#[napi]
pub struct InputHook {
    inner: std::sync::Arc<HookHandle>,
}

#[napi]
impl InputHook {
    #[napi(constructor)]
    pub fn new(callback: Function<QueuedEvent, UnknownReturnValue>) -> Result<Self> {
        let tsfn: EventThreadsafeFunction = callback
            .build_threadsafe_function::<QueuedEvent>()
            .weak::<true>()
            .callee_handled::<false>()
            .build()
            .map_err(|err| {
                napi::Error::new(
                    napi::Status::GenericFailure,
                    format!("failed to create InputHook callback: {}", err.reason),
                )
            })?;
        Ok(Self {
            inner: platform::new_handle(tsfn),
        })
    }

    #[napi]
    pub fn start(&self) -> Result<()> {
        platform::start(self.inner.clone()).map_err(|msg| {
            napi::Error::new(
                napi::Status::GenericFailure,
                format!("InputHook.start failed: {msg}"),
            )
        })
    }

    #[napi]
    pub fn stop(&self) {
        platform::stop(&self.inner);
    }

    #[napi(getter, js_name = "droppedEvents")]
    pub fn dropped_events(&self) -> f64 {
        self.inner
            .dropped_events
            .load(std::sync::atomic::Ordering::Relaxed) as f64
    }

    #[napi(getter, js_name = "reinstallCount")]
    pub fn reinstall_count(&self) -> f64 {
        self.inner
            .reinstall_count
            .load(std::sync::atomic::Ordering::Relaxed) as f64
    }
}

impl Drop for InputHook {
    fn drop(&mut self) {
        platform::stop(&self.inner);
    }
}

#[napi(js_name = "isAvailable")]
pub fn is_available() -> bool {
    cfg!(target_os = "windows")
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluxer_desktop_native::input::ring::Ring;

    #[test]
    fn write_key_name_known_vk() {
        let mut buf = [0u8; KEY_NAME_BUF];
        let mut len = 0u8;
        write_key_name(&mut buf, &mut len, 0x41);
        assert_eq!(&buf[..len as usize], b"A");
    }

    #[test]
    fn write_key_name_function_keys() {
        let mut buf = [0u8; KEY_NAME_BUF];
        let mut len = 0u8;
        write_key_name(&mut buf, &mut len, 0x7b);
        assert_eq!(&buf[..len as usize], b"F12");
    }

    #[test]
    fn write_key_name_from_hook_distinguishes_keypad_navigation() {
        let mut buf = [0u8; KEY_NAME_BUF];
        let mut len = 0u8;

        write_key_name_from_hook(&mut buf, &mut len, 0x2d, 0);
        assert_eq!(&buf[..len as usize], b"Numpad0");

        buf = [0u8; KEY_NAME_BUF];
        len = 0;
        write_key_name_from_hook(&mut buf, &mut len, 0x2d, LLKHF_EXTENDED_FLAG);
        assert_eq!(&buf[..len as usize], b"Insert");

        buf = [0u8; KEY_NAME_BUF];
        len = 0;
        write_key_name_from_hook(&mut buf, &mut len, 0x0d, LLKHF_EXTENDED_FLAG);
        assert_eq!(&buf[..len as usize], b"NumpadEnter");
    }

    #[test]
    fn write_key_name_fallback() {
        let mut buf = [0u8; KEY_NAME_BUF];
        let mut len = 0u8;
        write_key_name(&mut buf, &mut len, 0x0fff);
        assert_eq!(&buf[..len as usize], b"Key4095");
    }

    #[test]
    fn write_packet_key_name_uses_the_injected_character() {
        let name = |unit: u32| {
            let mut buf = [0xffu8; KEY_NAME_BUF];
            let mut len = 7u8;
            write_packet_key_name(&mut buf, &mut len, unit);
            String::from_utf8(buf[..len as usize].to_vec()).unwrap()
        };
        assert_eq!(name(u32::from('g')), "G");
        assert_eq!(name(u32::from('G')), "G");
        assert_eq!(name(u32::from('7')), "7");
        assert_eq!(name(u32::from(' ')), "Space");
        assert_eq!(name(u32::from('.')), "");
        assert_eq!(name(0x00f6), "");
        assert_eq!(name(0xd83d), "");
    }

    #[test]
    fn queued_event_default_is_empty_keydown() {
        let event = QueuedEvent::default();
        assert_eq!(event.kind, EventKind::KeyDown);
        assert_eq!(event.key_name_len, 0);
    }

    #[test]
    fn event_kind_strings_match_contract() {
        assert_eq!(EventKind::KeyDown.as_str(), "keydown");
        assert_eq!(EventKind::KeyUp.as_str(), "keyup");
        assert_eq!(EventKind::MouseDown.as_str(), "mousedown");
        assert_eq!(EventKind::MouseUp.as_str(), "mouseup");
    }

    #[test]
    fn held_inputs_release_each_pressed_input_once_without_modifiers() {
        let key = |kind, keycode, scan_code, extended| QueuedEvent {
            kind,
            keycode,
            scan_code,
            extended,
            ctrl: true,
            ..QueuedEvent::default()
        };
        let mut held = HeldInputs::default();
        held.track(&key(EventKind::KeyDown, 0x41, 0x1e, false));
        held.track(&key(EventKind::KeyDown, 0x41, 0x1e, false));
        held.track(&key(EventKind::KeyDown, 0x0d, 0x1c, true));
        held.track(&key(EventKind::KeyDown, 0x0d, 0x1c, false));
        held.track(&key(EventKind::KeyUp, 0x0d, 0x1c, false));
        held.track(&QueuedEvent {
            kind: EventKind::MouseDown,
            button: 3,
            has_xy: true,
            ..QueuedEvent::default()
        });
        let releases = held.take_releases();
        let summary: Vec<(EventKind, u32, bool, u8, bool, bool)> = releases
            .iter()
            .map(|event| {
                (
                    event.kind,
                    event.keycode,
                    event.extended,
                    event.button,
                    event.ctrl,
                    event.has_xy,
                )
            })
            .collect();
        assert_eq!(
            summary,
            vec![
                (EventKind::KeyUp, 0x41, false, 0, false, false),
                (EventKind::KeyUp, 0x0d, true, 0, false, false),
                (EventKind::MouseUp, 0, false, 3, false, false),
            ]
        );
        assert!(held.take_releases().is_empty());
    }

    #[test]
    fn is_available_matches_target() {
        assert_eq!(is_available(), cfg!(target_os = "windows"));
    }

    #[test]
    fn ring_holds_queued_events() {
        let ring: Ring<QueuedEvent, 4> = Ring::new();
        let idx = ring.claim().expect("claim slot") as usize;

        unsafe {
            let slots = ring.slots.as_ptr() as *mut QueuedEvent;
            (*slots.add(idx)).kind = EventKind::MouseUp;
            (*slots.add(idx)).button = 4;
        }
        let pop_idx = ring.pop().expect("pop slot") as usize;
        assert_eq!(ring.slots[pop_idx].kind, EventKind::MouseUp);
        assert_eq!(ring.slots[pop_idx].button, 4);
        ring.release();
        assert!(ring.pop().is_none());
    }
}
