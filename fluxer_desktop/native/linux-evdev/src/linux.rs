// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
    ops::Range,
    os::{
        fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd, RawFd},
        unix::fs::OpenOptionsExt,
    },
    path::{Path, PathBuf},
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

use evdev::{EventType, KeyCode, SynchronizationCode, raw_stream::RawDevice};
use napi::{
    Env, Status,
    bindgen_prelude::{Function, Object, Result, ToNapiValue},
    sys,
    threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode, UnknownReturnValue},
};
use napi_derive::napi;
use nix::{
    fcntl::OFlag,
    sys::{
        eventfd::{EfdFlags, EventFd},
        inotify::{AddWatchFlags, InitFlags, Inotify},
    },
};
use polling::{Event as PollEvent, Events, Poller};

use crate::keymap;
use crate::logind::{SessionGate, SessionWatch};

const EXIT_KEY: usize = 0;
const MONITOR_KEY: usize = 1;
const INOTIFY_KEY: usize = 2;
const DEVICE_KEY_BASE: usize = 3;
const DEV_INPUT_DIR: &str = "/dev/input";
const DEV_INPUT_PREFIX: &str = "/dev/input/event";
const EVENT_NODE_PREFIX: &str = "event";
const FLATPAK_INFO_PATH: &str = "/.flatpak-info";
const PERMISSION_RETRY_DELAY: Duration = Duration::from_millis(500);
const FULL_KEYBOARD_KEY_CODES: std::ops::Range<u16> = 1..32;

fn poll_key_for_device_fd(fd: RawFd) -> Option<usize> {
    usize::try_from(fd).ok()?.checked_add(DEVICE_KEY_BASE)
}

fn device_fd_from_poll_key(key: usize) -> Option<RawFd> {
    let fd = key.checked_sub(DEVICE_KEY_BASE)?;
    RawFd::try_from(fd).ok()
}

#[derive(Debug)]
pub enum NativeEvent {
    Key {
        kind: KeyKind,
        keycode: u16,
        key_name: &'static str,
        ctrl: bool,
        alt: bool,
        shift: bool,
        meta: bool,
    },
    Mouse {
        kind: MouseKind,
        button: u8,
        ctrl: bool,
        alt: bool,
        shift: bool,
        meta: bool,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum KeyKind {
    Down,
    Up,
}

#[derive(Debug, Clone, Copy)]
pub enum MouseKind {
    Down,
    Up,
}

impl ToNapiValue for NativeEvent {
    unsafe fn to_napi_value(raw_env: sys::napi_env, event: Self) -> Result<sys::napi_value> {
        let env = Env::from_raw(raw_env);
        let mut object = Object::new(&env)?;
        match event {
            Self::Key {
                kind,
                keycode,
                key_name,
                ctrl,
                alt,
                shift,
                meta,
            } => {
                object.set(
                    "type",
                    match kind {
                        KeyKind::Down => "keydown",
                        KeyKind::Up => "keyup",
                    },
                )?;
                object.set("keycode", u32::from(keycode))?;
                object.set("keyName", key_name)?;
                object.set("ctrlKey", ctrl)?;
                object.set("altKey", alt)?;
                object.set("shiftKey", shift)?;
                object.set("metaKey", meta)?;
            }
            Self::Mouse {
                kind,
                button,
                ctrl,
                alt,
                shift,
                meta,
            } => {
                object.set(
                    "type",
                    match kind {
                        MouseKind::Down => "mousedown",
                        MouseKind::Up => "mouseup",
                    },
                )?;
                object.set("button", u32::from(button))?;
                object.set("ctrlKey", ctrl)?;
                object.set("altKey", alt)?;
                object.set("shiftKey", shift)?;
                object.set("metaKey", meta)?;
            }
        }
        unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, object) }
    }
}

type EventTsfn =
    Arc<ThreadsafeFunction<NativeEvent, UnknownReturnValue, NativeEvent, Status, false, true>>;

struct ExitFd {
    fd: EventFd,
}

impl ExitFd {
    fn new() -> std::io::Result<Self> {
        let fd = EventFd::from_value_and_flags(0, EfdFlags::EFD_CLOEXEC | EfdFlags::EFD_NONBLOCK)
            .map_err(std::io::Error::from)?;
        Ok(Self { fd })
    }

    fn signal(&self) {
        let _ = self.fd.write(1);
    }

    fn drain(&self) {
        let _ = self.fd.read();
    }

    fn as_borrowed(&self) -> BorrowedFd<'_> {
        self.fd.as_fd()
    }
}

fn resolve_seat() -> String {
    std::env::var("XDG_SEAT")
        .ok()
        .filter(|value| !value.is_empty())
        .unwrap_or_else(|| "seat0".to_string())
}

fn read_seat_from_device(device: &udev::Device) -> Option<String> {
    if let Some(value) = device.property_value("ID_SEAT") {
        return Some(value.to_string_lossy().into_owned());
    }
    let mut parent = device.parent();
    while let Some(p) = parent {
        if let Some(value) = p.property_value("ID_SEAT") {
            return Some(value.to_string_lossy().into_owned());
        }
        parent = p.parent();
    }
    Some("seat0".to_string())
}

fn lookup_input_seat(sysname: &str) -> Option<String> {
    let mut enumerator = udev::Enumerator::new().ok()?;
    enumerator.match_subsystem("input").ok()?;
    enumerator.match_sysname(sysname).ok()?;
    let device = enumerator.scan_devices().ok()?.next()?;
    read_seat_from_device(&device)
}

struct SeatFilter {
    udev_handle_available: bool,
    seat: String,
}

impl SeatFilter {
    fn detect() -> Self {
        Self {
            udev_handle_available: udev::Enumerator::new().is_ok(),
            seat: resolve_seat(),
        }
    }

    fn includes(&self, sysname: &str) -> bool {
        if !self.udev_handle_available || self.seat.is_empty() {
            return true;
        }
        match lookup_input_seat(sysname) {
            Some(found) => found == self.seat,
            None => true,
        }
    }

    fn event_node_paths(&self) -> Vec<PathBuf> {
        let Ok(entries) = std::fs::read_dir(DEV_INPUT_DIR) else {
            return Vec::new();
        };
        entries
            .flatten()
            .filter(|entry| {
                entry
                    .file_name()
                    .to_str()
                    .is_some_and(|name| is_event_node_name(name) && self.includes(name))
            })
            .map(|entry| entry.path())
            .collect()
    }
}

fn is_event_node_name(name: &str) -> bool {
    name.starts_with(EVENT_NODE_PREFIX)
}

fn is_sandboxed() -> bool {
    Path::new(FLATPAK_INFO_PATH).exists()
}

struct OpenedDevice {
    device: RawDevice,
    path: PathBuf,
    held: HashSet<u16>,
    dropped: bool,
}

enum DeviceInput {
    Key(u16, i32),
    Resync,
}

struct PermissionRetry {
    path: PathBuf,
    due: Instant,
}

struct Reader {
    poller: Arc<Poller>,
    exit_fd: Arc<ExitFd>,
    gate: Arc<SessionGate>,
    seats: SeatFilter,
    devices: HashMap<RawFd, OpenedDevice>,
    retries: Vec<PermissionRetry>,
    delivering: bool,
    callback: EventTsfn,
    stop: Arc<AtomicBool>,
}

impl Reader {
    fn new(
        callback: EventTsfn,
        stop: Arc<AtomicBool>,
        exit_fd: Arc<ExitFd>,
    ) -> std::io::Result<Self> {
        let poller = Arc::new(Poller::new()?);
        unsafe {
            poller.add(&exit_fd.as_borrowed(), PollEvent::readable(EXIT_KEY))?;
        }
        let gate = Arc::new(SessionGate::new(poller.clone()));

        Ok(Self {
            poller,
            exit_fd,
            gate,
            seats: SeatFilter::detect(),
            devices: HashMap::new(),
            retries: Vec::new(),
            delivering: false,
            callback,
            stop,
        })
    }

    fn has_full_keyboard(&self) -> bool {
        self.devices
            .values()
            .any(|opened| device_is_full_keyboard(&opened.device))
    }

    fn try_attach_monitor(&self) -> Option<udev::MonitorSocket> {
        if is_sandboxed() {
            return None;
        }
        let socket = udev::MonitorBuilder::new()
            .and_then(|b| b.match_subsystem("input"))
            .and_then(|b| b.listen())
            .ok()?;
        let monitor_fd = socket.as_raw_fd();
        let borrowed = unsafe { BorrowedFd::borrow_raw(monitor_fd) };
        if unsafe { self.poller.add(&borrowed, PollEvent::readable(MONITOR_KEY)) }.is_ok() {
            Some(socket)
        } else {
            None
        }
    }

    fn try_attach_inotify(&self) -> Option<Inotify> {
        let inotify = Inotify::init(InitFlags::IN_CLOEXEC | InitFlags::IN_NONBLOCK).ok()?;
        inotify
            .add_watch(
                DEV_INPUT_DIR,
                AddWatchFlags::IN_CREATE | AddWatchFlags::IN_ATTRIB,
            )
            .ok()?;
        if unsafe {
            self.poller
                .add(&inotify.as_fd(), PollEvent::readable(INOTIFY_KEY))
        }
        .is_ok()
        {
            Some(inotify)
        } else {
            None
        }
    }

    fn open_all_devices(&mut self) {
        for path in self.seats.event_node_paths() {
            let _ = self.open_device(&path);
        }
    }

    fn open_hotplugged_device(&mut self, path: &Path) {
        if let Err(err) = self.open_device(path)
            && err.kind() == std::io::ErrorKind::PermissionDenied
            && !self.retries.iter().any(|retry| retry.path == path)
        {
            self.retries.push(PermissionRetry {
                path: path.to_path_buf(),
                due: Instant::now() + PERMISSION_RETRY_DELAY,
            });
        }
    }

    fn next_retry_timeout(&self) -> Option<Duration> {
        let now = Instant::now();
        self.retries
            .iter()
            .map(|retry| retry.due.saturating_duration_since(now))
            .min()
    }

    fn run_due_retries(&mut self) {
        let now = Instant::now();
        let (due, pending): (Vec<PermissionRetry>, Vec<PermissionRetry>) =
            self.retries.drain(..).partition(|retry| retry.due <= now);
        self.retries = pending;
        for retry in due {
            let _ = self.open_device(&retry.path);
        }
    }

    fn open_device(&mut self, path: &Path) -> std::io::Result<()> {
        if self
            .devices
            .values()
            .any(|opened| opened.path.as_path() == path)
        {
            return Ok(());
        }
        let device = open_read_only(path)?;
        if !device_has_routable_input(&device) {
            return Ok(());
        }
        let fd = device.as_raw_fd();
        let poll_key = poll_key_for_device_fd(fd).ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "input device fd cannot be represented as a poll key",
            )
        })?;
        unsafe {
            let borrowed = BorrowedFd::borrow_raw(fd);
            self.poller.add(&borrowed, PollEvent::readable(poll_key))?;
        }
        self.devices.insert(
            fd,
            OpenedDevice {
                device,
                path: path.to_path_buf(),
                held: HashSet::new(),
                dropped: false,
            },
        );
        self.retries.retain(|retry| retry.path != path);
        Ok(())
    }

    fn close_device_by_path(&mut self, path: &Path) {
        let fd = self
            .devices
            .iter()
            .find(|(_, opened)| opened.path.as_path() == path)
            .map(|(fd, _)| *fd);
        if let Some(fd) = fd {
            self.close_device_by_fd(fd);
        }
    }

    fn close_device_by_fd(&mut self, fd: RawFd) {
        if let Some(opened) = self.devices.remove(&fd) {
            let borrowed = unsafe { BorrowedFd::borrow_raw(fd) };
            let _ = self.poller.delete(borrowed);
            let OpenedDevice { device, held, .. } = opened;
            drop(device);
            self.release_codes(held);
        }
    }

    fn release_all(&mut self) {
        let mut held = HashSet::new();
        for opened in self.devices.values_mut() {
            held.extend(opened.held.drain());
        }
        self.release_codes(held);
    }

    fn release_codes(&mut self, codes: HashSet<u16>) {
        let mut codes: Vec<u16> = codes
            .into_iter()
            .filter(|code| !self.is_held(*code))
            .collect();
        codes.sort_unstable();
        for code in codes {
            self.dispatch(self.native_event(code, false));
        }
    }

    fn apply_session_gate(&mut self) {
        let active = self.gate.is_active();
        if self.delivering && !active {
            self.release_all();
        }
        self.delivering = active;
    }

    fn run(&mut self) {
        let monitor = self.try_attach_monitor();
        let inotify = self.try_attach_inotify();
        self.open_all_devices();
        let mut events = Events::new();
        loop {
            if self.stop.load(Ordering::Acquire) {
                break;
            }
            events.clear();
            if self
                .poller
                .wait(&mut events, self.next_retry_timeout())
                .is_err()
            {
                break;
            }
            let mut device_fds_to_drain: Vec<RawFd> = Vec::new();
            let mut drain_monitor = false;
            let mut drain_inotify = false;
            let mut got_exit = false;
            for event in events.iter() {
                match event.key {
                    EXIT_KEY => got_exit = true,
                    MONITOR_KEY => drain_monitor = true,
                    INOTIFY_KEY => drain_inotify = true,
                    fd_key => {
                        if let Some(fd) = device_fd_from_poll_key(fd_key) {
                            device_fds_to_drain.push(fd);
                        }
                    }
                }
            }
            if got_exit {
                self.exit_fd.drain();
                break;
            }

            self.rearm(
                monitor.as_ref(),
                drain_monitor,
                inotify.as_ref().filter(|_| drain_inotify),
                &device_fds_to_drain,
            );

            self.apply_session_gate();
            if drain_monitor && let Some(monitor) = monitor.as_ref() {
                self.drain_monitor(monitor);
            }
            if drain_inotify && let Some(inotify) = inotify.as_ref() {
                self.drain_inotify(inotify);
            }
            self.run_due_retries();
            for fd in device_fds_to_drain {
                self.drain_device(fd);
            }
        }

        if !self.stop.load(Ordering::Acquire) {
            self.release_all();
        }
        if let Some(monitor) = monitor.as_ref() {
            let borrowed = unsafe { BorrowedFd::borrow_raw(monitor.as_raw_fd()) };
            let _ = self.poller.delete(borrowed);
        }
        if let Some(inotify) = inotify.as_ref() {
            let _ = self.poller.delete(inotify.as_fd());
        }
    }

    fn rearm(
        &self,
        monitor: Option<&udev::MonitorSocket>,
        drain_monitor: bool,
        inotify: Option<&Inotify>,
        device_fds: &[RawFd],
    ) {
        let _ = self
            .poller
            .modify(self.exit_fd.as_borrowed(), PollEvent::readable(EXIT_KEY));
        if drain_monitor && let Some(monitor) = monitor {
            let borrowed = unsafe { BorrowedFd::borrow_raw(monitor.as_raw_fd()) };
            let _ = self
                .poller
                .modify(borrowed, PollEvent::readable(MONITOR_KEY));
        }
        if let Some(inotify) = inotify {
            let _ = self
                .poller
                .modify(inotify.as_fd(), PollEvent::readable(INOTIFY_KEY));
        }
        for fd in device_fds {
            if self.devices.contains_key(fd) {
                let Some(poll_key) = poll_key_for_device_fd(*fd) else {
                    continue;
                };
                let borrowed = unsafe { BorrowedFd::borrow_raw(*fd) };
                let _ = self.poller.modify(borrowed, PollEvent::readable(poll_key));
            }
        }
    }

    fn drain_monitor(&mut self, monitor: &udev::MonitorSocket) {
        let mut pending: Vec<(String, PathBuf, Option<String>)> = Vec::new();
        for event in monitor.iter() {
            let action = match event.action() {
                Some(a) => a.to_string_lossy().into_owned(),
                None => continue,
            };
            let devnode = match event.devnode() {
                Some(p) => p.to_path_buf(),
                None => continue,
            };
            let subsystem = event.subsystem().map(|s| s.to_string_lossy().into_owned());
            if subsystem.as_deref() != Some("input") {
                continue;
            }
            let devnode_str = devnode.to_string_lossy().into_owned();
            if !devnode_str.starts_with(DEV_INPUT_PREFIX) {
                continue;
            }
            let seat = read_seat_from_device(&event);
            pending.push((action, devnode, seat));
        }
        for (action, devnode, seat) in pending {
            match action.as_str() {
                "add" => {
                    if !self.seats.seat.is_empty()
                        && let Some(seat) = seat.as_deref()
                        && seat != self.seats.seat
                    {
                        continue;
                    }
                    self.open_hotplugged_device(&devnode);
                }
                "remove" => self.close_device_by_path(&devnode),
                _ => {}
            }
        }
    }

    fn drain_inotify(&mut self, inotify: &Inotify) {
        let mut paths: Vec<PathBuf> = Vec::new();
        while let Ok(events) = inotify.read_events() {
            if events.is_empty() {
                break;
            }
            for event in events {
                let Some(name) = event.name.as_deref().and_then(OsStr::to_str) else {
                    continue;
                };
                if !is_event_node_name(name) || !self.seats.includes(name) {
                    continue;
                }
                let path = Path::new(DEV_INPUT_DIR).join(name);
                if !paths.contains(&path) {
                    paths.push(path);
                }
            }
        }
        for path in paths {
            self.open_hotplugged_device(&path);
        }
    }

    fn drain_device(&mut self, fd: RawFd) {
        let mut decoded: Vec<DeviceInput> = Vec::new();
        let mut device_dead = false;
        {
            let Some(opened) = self.devices.get_mut(&fd) else {
                return;
            };
            loop {
                match opened.device.fetch_events() {
                    Ok(events) => {
                        for ev in events {
                            match (ev.event_type(), ev.code()) {
                                (EventType::SYNCHRONIZATION, code)
                                    if code == SynchronizationCode::SYN_DROPPED.0 =>
                                {
                                    opened.dropped = true;
                                }
                                (EventType::SYNCHRONIZATION, code)
                                    if opened.dropped
                                        && code == SynchronizationCode::SYN_REPORT.0 =>
                                {
                                    opened.dropped = false;
                                    decoded.push(DeviceInput::Resync);
                                }
                                _ if opened.dropped => {}
                                (EventType::KEY, code) => {
                                    decoded.push(DeviceInput::Key(code, ev.value()));
                                }
                                _ => {}
                            }
                        }
                    }
                    Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => break,
                    Err(_) => {
                        device_dead = true;
                        break;
                    }
                }
            }
        }
        if device_dead {
            self.close_device_by_fd(fd);
            return;
        }
        if !self.delivering {
            return;
        }
        for input in decoded {
            match input {
                DeviceInput::Key(_, 2) => {}
                DeviceInput::Key(code, value) => self.translate_event(fd, code, value == 1),
                DeviceInput::Resync => self.resync_device(fd),
            }
        }
    }

    fn resync_device(&mut self, fd: RawFd) {
        let Some(opened) = self.devices.get_mut(&fd) else {
            return;
        };
        let released = match opened.device.get_key_state() {
            Ok(state) => {
                let down: HashSet<u16> = state
                    .iter()
                    .map(|key| key.code())
                    .filter(|code| is_routable_key_code(*code))
                    .collect();
                let released: HashSet<u16> = opened.held.difference(&down).copied().collect();
                opened.held = down;
                released
            }
            Err(_) => std::mem::take(&mut opened.held),
        };
        self.release_codes(released);
    }

    fn translate_event(&mut self, fd: RawFd, code: u16, is_press: bool) {
        if !is_routable_key_code(code) {
            return;
        }
        if let Some(opened) = self.devices.get_mut(&fd) {
            if is_press {
                opened.held.insert(code);
            } else {
                opened.held.remove(&code);
            }
        }
        self.dispatch(self.native_event(code, is_press));
    }

    fn native_event(&self, code: u16, is_press: bool) -> NativeEvent {
        let ctrl = self.modifier_state_ctrl();
        let alt = self.modifier_state_alt();
        let shift = self.modifier_state_shift();
        let meta = self.modifier_state_meta();
        if let Some(button) = keymap::evdev_button_to_browser_button(code) {
            return NativeEvent::Mouse {
                kind: if is_press {
                    MouseKind::Down
                } else {
                    MouseKind::Up
                },
                button,
                ctrl,
                alt,
                shift,
                meta,
            };
        }
        NativeEvent::Key {
            kind: if is_press { KeyKind::Down } else { KeyKind::Up },
            keycode: code,
            key_name: keymap::keycode_to_name(code).unwrap_or_default(),
            ctrl,
            alt,
            shift,
            meta,
        }
    }

    fn is_held(&self, code: u16) -> bool {
        self.devices
            .values()
            .any(|opened| opened.held.contains(&code))
    }

    fn dispatch(&self, event: NativeEvent) {
        let status = self
            .callback
            .call(event, ThreadsafeFunctionCallMode::NonBlocking);
        if status == Status::Closing {
            self.stop.store(true, Ordering::Release);
        }
    }

    fn modifier_state_ctrl(&self) -> bool {
        self.is_held(keymap::LEFT_CTRL) || self.is_held(keymap::RIGHT_CTRL)
    }
    fn modifier_state_alt(&self) -> bool {
        self.is_held(keymap::LEFT_ALT) || self.is_held(keymap::RIGHT_ALT)
    }
    fn modifier_state_shift(&self) -> bool {
        self.is_held(keymap::LEFT_SHIFT) || self.is_held(keymap::RIGHT_SHIFT)
    }
    fn modifier_state_meta(&self) -> bool {
        self.is_held(keymap::LEFT_META) || self.is_held(keymap::RIGHT_META)
    }
}

const KEYBOARD_KEY_CODES: Range<u16> = KeyCode::KEY_ESC.code()..KeyCode::BTN_0.code();
const EXTENDED_KEY_CODES: Range<u16> = KeyCode::KEY_OK.code()..KeyCode::BTN_TRIGGER_HAPPY1.code();

fn is_routable_key_code(code: u16) -> bool {
    KEYBOARD_KEY_CODES.contains(&code)
        || EXTENDED_KEY_CODES.contains(&code)
        || keymap::evdev_button_to_browser_button(code).is_some()
}

fn device_has_routable_input(device: &RawDevice) -> bool {
    device.supported_keys().is_some_and(|keys| {
        keys.iter()
            .any(|key: KeyCode| is_routable_key_code(key.code()))
    })
}

fn is_full_keyboard_key_set(supports: impl Fn(u16) -> bool) -> bool {
    FULL_KEYBOARD_KEY_CODES.into_iter().all(supports)
}

fn device_is_full_keyboard(device: &RawDevice) -> bool {
    device
        .supported_keys()
        .is_some_and(|keys| is_full_keyboard_key_set(|code| keys.contains(KeyCode::new(code))))
}

fn open_read_only(path: &Path) -> std::io::Result<RawDevice> {
    let file = std::fs::OpenOptions::new()
        .read(true)
        .custom_flags(OFlag::O_NONBLOCK.bits())
        .open(path)?;
    RawDevice::from_fd(OwnedFd::from(file))
}

impl Drop for Reader {
    fn drop(&mut self) {
        let fds: Vec<RawFd> = self.devices.keys().copied().collect();
        for fd in fds {
            let borrowed = unsafe { BorrowedFd::borrow_raw(fd) };
            let _ = self.poller.delete(borrowed);
        }

        let _ = self.poller.delete(self.exit_fd.as_borrowed());
    }
}

struct HookInner {
    stop: Option<Arc<AtomicBool>>,
    exit_fd: Option<Arc<ExitFd>>,
    thread: Option<JoinHandle<()>>,
    session_watch: Option<SessionWatch>,
}

impl HookInner {
    fn stop_and_join(&mut self) {
        self.session_watch = None;
        if let Some(stop) = &self.stop {
            stop.store(true, Ordering::Release);
        }
        if let Some(exit_fd) = &self.exit_fd {
            exit_fd.signal();
        }
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
        self.stop = None;
        self.exit_fd = None;
    }
}

#[napi]
pub struct EvdevHook {
    callback: EventTsfn,
    inner: Mutex<HookInner>,
}

#[napi]
impl EvdevHook {
    #[napi(constructor)]
    pub fn new(on_event: Function<NativeEvent, UnknownReturnValue>) -> Result<Self> {
        let callback = Arc::new(
            on_event
                .build_threadsafe_function::<NativeEvent>()
                .weak::<true>()
                .callee_handled::<false>()
                .build()
                .map_err(|err| {
                    generic_error(format!("failed to build callback: {}", err.reason))
                })?,
        );
        Ok(Self {
            callback,
            inner: Mutex::new(HookInner {
                stop: None,
                exit_fd: None,
                thread: None,
                session_watch: None,
            }),
        })
    }

    #[napi]
    pub fn start(&self) -> Result<bool> {
        let mut inner = self
            .inner
            .lock()
            .map_err(|_| generic_error("hook lock poisoned"))?;
        if let Some(thread) = inner.thread.as_ref() {
            if !thread.is_finished() {
                return Ok(true);
            }
            inner.stop_and_join();
        }
        let stop = Arc::new(AtomicBool::new(false));
        let exit_fd = Arc::new(
            ExitFd::new()
                .map_err(|err| generic_error(format!("failed to allocate eventfd: {err}")))?,
        );
        let mut reader = Reader::new(self.callback.clone(), stop.clone(), exit_fd.clone())
            .map_err(|err| generic_error(format!("evdev start failed: {err}")))?;

        reader.open_all_devices();
        if !reader.has_full_keyboard() {
            return Ok(false);
        }

        let session_watch = SessionWatch::spawn(reader.gate.clone())
            .map_err(|err| generic_error(format!("failed to spawn logind thread: {err}")))?;
        let join = thread::Builder::new()
            .name("fluxer-linux-evdev-reader".to_string())
            .spawn(move || {
                let mut reader = reader;
                reader.run();
            })
            .map_err(|err| generic_error(format!("failed to spawn reader thread: {err}")))?;

        inner.stop = Some(stop);
        inner.exit_fd = Some(exit_fd);
        inner.thread = Some(join);
        inner.session_watch = Some(session_watch);
        Ok(true)
    }

    #[napi]
    pub fn stop(&self) -> Result<()> {
        let mut inner = self
            .inner
            .lock()
            .map_err(|_| generic_error("hook lock poisoned"))?;
        inner.stop_and_join();
        Ok(())
    }
}

impl Drop for EvdevHook {
    fn drop(&mut self) {
        if let Ok(mut inner) = self.inner.lock() {
            inner.stop_and_join();
        }
    }
}

#[napi(js_name = "isKeyboardReadable")]
pub fn is_keyboard_readable() -> bool {
    SeatFilter::detect()
        .event_node_paths()
        .iter()
        .any(|path| open_read_only(path).is_ok_and(|device| device_is_full_keyboard(&device)))
}

fn generic_error(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::GenericFailure, reason.into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn control_poll_keys_are_legal_and_outside_fd_range() {
        assert_eq!(EXIT_KEY, 0);
        assert_eq!(MONITOR_KEY, 1);
        assert_eq!(INOTIFY_KEY, 2);
        assert_eq!(poll_key_for_device_fd(0), Some(DEVICE_KEY_BASE));
        assert_eq!(device_fd_from_poll_key(DEVICE_KEY_BASE), Some(0));
        assert_eq!(device_fd_from_poll_key(EXIT_KEY), None);
        assert_eq!(device_fd_from_poll_key(MONITOR_KEY), None);
        assert_eq!(device_fd_from_poll_key(INOTIFY_KEY), None);
    }

    #[test]
    fn full_keyboard_requires_every_key_from_escape_to_s() {
        assert!(is_full_keyboard_key_set(|code| code < 200));
        assert!(!is_full_keyboard_key_set(|code| code != 0 && code != 17));
        assert!(!is_full_keyboard_key_set(|code| code == 0 || code >= 32));
        assert!(is_full_keyboard_key_set(|code| (1..32).contains(&code)));
    }

    #[test]
    fn event_node_names_match_only_event_devices() {
        assert!(is_event_node_name("event0"));
        assert!(is_event_node_name("event17"));
        assert!(!is_event_node_name("mouse0"));
        assert!(!is_event_node_name("js0"));
        assert!(!is_event_node_name("by-id"));
    }

    #[test]
    fn session_gate_notifies_only_on_change() {
        let poller = Arc::new(Poller::new().expect("create poller"));
        let gate = SessionGate::new(poller.clone());
        assert!(!gate.is_active());
        gate.set_active(false);
        let mut events = Events::new();
        poller
            .wait(&mut events, Some(Duration::from_millis(0)))
            .expect("poll without notification");
        gate.set_active(true);
        assert!(gate.is_active());
        let started = Instant::now();
        poller
            .wait(&mut events, Some(Duration::from_secs(5)))
            .expect("poll with notification");
        assert!(started.elapsed() < Duration::from_secs(5));
        gate.set_active(false);
        assert!(!gate.is_active());
    }

    #[test]
    fn poller_accepts_exit_control_key() {
        let poller = Poller::new().expect("create poller");
        let exit_fd = ExitFd::new().expect("create exit fd");
        unsafe {
            poller
                .add(&exit_fd.as_borrowed(), PollEvent::readable(EXIT_KEY))
                .expect("register exit fd");
        }
        poller
            .delete(exit_fd.as_borrowed())
            .expect("delete exit fd");
    }

    #[test]
    fn routable_key_code_filter_keeps_keyboards_and_dom_mouse_buttons() {
        assert!(is_routable_key_code(KeyCode::KEY_A.code()));
        assert!(is_routable_key_code(KeyCode::KEY_LEFTCTRL.code()));
        assert!(is_routable_key_code(KeyCode::BTN_LEFT.code()));
        assert!(is_routable_key_code(KeyCode::BTN_FORWARD.code()));
    }

    #[test]
    fn every_key_main_maps_to_a_dom_code_is_routable() {
        let source = include_str!("../../../src/main/GlobalShortcutKeys.ts");
        let table = source
            .split("const EVDEV_KEY_CODES")
            .nth(1)
            .and_then(|rest| rest.split("]);").next())
            .expect("EVDEV_KEY_CODES table");
        let codes: Vec<u16> = table
            .split("[0x")
            .skip(1)
            .map(|entry| {
                let hex = entry.split(',').next().expect("code");
                u16::from_str_radix(hex, 16).expect("hex code")
            })
            .collect();
        assert!(codes.len() > 150);
        for code in codes {
            assert!(is_routable_key_code(code), "evdev code {code:#x}");
        }
    }

    #[test]
    fn routable_key_code_filter_keeps_keys_without_a_native_name() {
        assert!(is_routable_key_code(KeyCode::KEY_CALC.code()));
        assert!(is_routable_key_code(KeyCode::KEY_PLAYCD.code()));
        assert!(is_routable_key_code(KeyCode::KEY_FN.code()));
        assert!(!is_routable_key_code(0));
        assert!(!is_routable_key_code(KeyCode::BTN_TRIGGER_HAPPY1.code()));
    }

    #[test]
    fn routable_key_code_filter_ignores_tablet_pad_and_tool_buttons() {
        assert!(!is_routable_key_code(KeyCode::BTN_0.code()));
        assert!(!is_routable_key_code(KeyCode::BTN_TOOL_PEN.code()));
        assert!(!is_routable_key_code(KeyCode::BTN_STYLUS.code()));
    }
}
