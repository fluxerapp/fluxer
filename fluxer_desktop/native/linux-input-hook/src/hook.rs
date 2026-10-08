#![allow(non_snake_case)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeSet;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc::{self, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use napi::{
    Env, Status, Task,
    bindgen_prelude::{AsyncTask, Function, Object, Result, ToNapiValue},
    sys,
    threadsafe_function::{ThreadsafeFunction, ThreadsafeFunctionCallMode, UnknownReturnValue},
};
use napi_derive::napi;
use x11rb::connection::{Connection, RequestConnection};
use x11rb::errors::ReplyError;
use x11rb::protocol::Event;
use x11rb::protocol::record::{
    self, ConnectionExt as RecordConnectionExt, ExtRange, Range, Range8, Range16,
};
use x11rb::protocol::xproto::{
    ConnectionExt as XprotoConnectionExt, GetKeyboardMappingReply, Keycode, Mapping,
};
use x11rb::rust_connection::RustConnection;
use x11rb::wrapper::ConnectionExt as WrapperConnectionExt;
use x11rb::x11_utils::TryParse;

use crate::env::detect_display_server;
use crate::keymap;
use crate::modifiers::{self, Modifiers};
use crate::mouse;

const RECORD_FROM_SERVER: u8 = 0;
const RECORD_START_OF_DATA: u8 = 4;

const KEY_PRESS: u8 = 2;
const KEY_RELEASE: u8 = 3;
const BUTTON_PRESS: u8 = 4;
const BUTTON_RELEASE: u8 = 5;
const START_TIMEOUT: Duration = Duration::from_secs(2);

#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct XKeyButtonProto {
    type_: u8,
    detail: u8,
    seq_l: u8,
    seq_h: u8,
    time: u32,
    root: u32,
    event: u32,
    child: u32,
    root_x: i16,
    root_y: i16,
    event_x: i16,
    event_y: i16,
    state: u16,
    same_screen: u8,
    pad0: u8,
}

#[derive(Debug, Clone)]
pub enum EventKind {
    KeyDown,
    KeyUp,
    MouseDown,
    MouseUp,
}

impl EventKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::KeyDown => "keydown",
            Self::KeyUp => "keyup",
            Self::MouseDown => "mousedown",
            Self::MouseUp => "mouseup",
        }
    }
}

#[derive(Debug, Clone)]
pub struct DecodedEvent {
    pub kind: EventKind,
    pub keycode: u32,
    pub x11_keycode: u8,
    pub key_name: String,
    pub button: u8,
    pub x: i32,
    pub y: i32,
    pub has_xy: bool,
    pub mods: Modifiers,
}

impl DecodedEvent {
    fn new(kind: EventKind, mods: Modifiers) -> Self {
        Self {
            kind,
            keycode: 0,
            x11_keycode: 0,
            key_name: String::new(),
            button: 0,
            x: 0,
            y: 0,
            has_xy: false,
            mods,
        }
    }
}

impl ToNapiValue for DecodedEvent {
    unsafe fn to_napi_value(raw_env: sys::napi_env, event: Self) -> Result<sys::napi_value> {
        let env = Env::from_raw(raw_env);
        let mut object = Object::new(&env)?;
        object.set("type", event.kind.as_str())?;
        object.set("ctrlKey", event.mods.ctrl)?;
        object.set("altKey", event.mods.alt)?;
        object.set("shiftKey", event.mods.shift)?;
        object.set("metaKey", event.mods.meta)?;
        match event.kind {
            EventKind::KeyDown | EventKind::KeyUp => {
                object.set("keycode", event.keycode)?;
                object.set("x11Keycode", u32::from(event.x11_keycode))?;
                object.set("keyName", event.key_name.as_str())?;
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

type EventTsfn =
    Arc<ThreadsafeFunction<DecodedEvent, UnknownReturnValue, DecodedEvent, Status, false, true>>;

#[derive(Clone)]
struct KeysymCache {
    min_keycode: Keycode,
    syms: Vec<u32>,
}

impl KeysymCache {
    fn build(reply: &GetKeyboardMappingReply, min_keycode: Keycode) -> Self {
        let per = reply.keysyms_per_keycode as usize;
        let count = reply.keysyms.len().checked_div(per).unwrap_or(0);
        let mut syms = Vec::with_capacity(count);
        if per > 0 {
            for i in 0..count {
                syms.push(reply.keysyms[i * per]);
            }
        }
        Self { min_keycode, syms }
    }

    fn fetch(conn: &RustConnection) -> std::result::Result<Self, String> {
        let setup = conn.setup();
        let min_keycode = setup.min_keycode;
        let count = setup
            .max_keycode
            .saturating_sub(min_keycode)
            .saturating_add(1);
        let mapping = conn
            .get_keyboard_mapping(min_keycode, count)
            .map_err(|err| format!("GetKeyboardMapping: {err}"))?
            .reply()
            .map_err(|err| format!("GetKeyboardMapping: {err}"))?;
        Ok(Self::build(&mapping, min_keycode))
    }

    fn lookup(&self, keycode: u8) -> u32 {
        if keycode < self.min_keycode {
            return 0;
        }
        let idx = (keycode - self.min_keycode) as usize;
        self.syms.get(idx).copied().unwrap_or(0)
    }
}

struct Active {
    ctrl_conn: Arc<RustConnection>,
    record_ctx: record::Context,
    worker: Option<JoinHandle<()>>,
    stop: Arc<AtomicBool>,
}

struct Inner {
    callback: EventTsfn,
    active: Mutex<Option<Active>>,
    generation: AtomicU64,
}

impl Inner {
    fn lock(&self) -> Result<std::sync::MutexGuard<'_, Option<Active>>> {
        self.active
            .lock()
            .map_err(|_| generic_error("InputHook lock poisoned"))
    }

    fn start(&self, generation: u64) -> Result<()> {
        let dead = {
            let mut guard = self.lock()?;
            if guard
                .as_ref()
                .is_some_and(|active| !active.stop.load(Ordering::Acquire))
            {
                return Ok(());
            }
            guard.take()
        };
        if let Some(dead) = dead {
            tear_down(dead);
        }
        let started = start_record(self.callback.clone())?;
        let mut guard = self.lock()?;
        if self.generation.load(Ordering::Acquire) != generation {
            drop(guard);
            tear_down(started);
            return Err(generic_error(
                "InputHook.start failed: stopped during start",
            ));
        }
        if guard
            .as_ref()
            .is_some_and(|active| !active.stop.load(Ordering::Acquire))
        {
            drop(guard);
            tear_down(started);
            return Ok(());
        }
        let replaced = guard.replace(started);
        drop(guard);
        if let Some(dead) = replaced {
            tear_down(dead);
        }
        Ok(())
    }

    fn stop(&self) -> Result<Option<Active>> {
        let mut guard = self.lock()?;
        self.generation.fetch_add(1, Ordering::AcqRel);
        Ok(guard.take())
    }
}

pub struct StartTask {
    inner: Arc<Inner>,
    generation: u64,
}

impl Task for StartTask {
    type Output = ();
    type JsValue = ();

    fn compute(&mut self) -> Result<Self::Output> {
        self.inner.start(self.generation)
    }

    fn resolve(&mut self, _env: Env, _output: Self::Output) -> Result<Self::JsValue> {
        Ok(())
    }
}

#[napi]
pub struct InputHook {
    inner: Arc<Inner>,
}

#[napi]
impl InputHook {
    #[napi(constructor)]
    pub fn new(callback: Function<DecodedEvent, UnknownReturnValue>) -> Result<Self> {
        let tsfn = callback
            .build_threadsafe_function::<DecodedEvent>()
            .weak::<true>()
            .callee_handled::<false>()
            .build()
            .map_err(|err| generic_error(format!("failed to create TSFN: {}", err.reason)))?;
        Ok(Self {
            inner: Arc::new(Inner {
                callback: Arc::new(tsfn),
                active: Mutex::new(None),
                generation: AtomicU64::new(0),
            }),
        })
    }

    #[napi]
    pub fn start(&self) -> AsyncTask<StartTask> {
        AsyncTask::new(StartTask {
            inner: self.inner.clone(),
            generation: self.inner.generation.load(Ordering::Acquire),
        })
    }

    #[napi]
    pub fn stop(&self) -> Result<()> {
        if let Some(active) = self.inner.stop()? {
            tear_down(active);
        }
        Ok(())
    }
}

impl Drop for InputHook {
    fn drop(&mut self) {
        if let Ok(Some(active)) = self.inner.stop() {
            tear_down(active);
        }
    }
}

fn start_record(callback: EventTsfn) -> Result<Active> {
    if !detect_display_server().supports_global_xrecord() {
        return Err(generic_error(
            "InputHook.start failed: WaylandUnsupported — global input \
             capture is blocked by the Wayland security model. Use \
             @fluxer/linux-evdev for kernel-level capture when the user has \
             input device access."
                .to_string(),
        ));
    }

    let (ctrl_conn, _) = x11rb::connect(None)
        .map_err(|err| generic_error(format!("InputHook.start failed: NoXDisplay: {err}")))?;
    let (data_conn, _) = x11rb::connect(None)
        .map_err(|err| generic_error(format!("InputHook.start failed: NoXDisplay: {err}")))?;
    let ctrl_conn = Arc::new(ctrl_conn);

    let has_record = ctrl_conn
        .extension_information(record::X11_EXTENSION_NAME)
        .map_err(|err| generic_error(format!("InputHook.start failed: {err}")))?
        .is_some();
    if !has_record {
        return Err(generic_error(
            "InputHook.start failed: RecordExtensionUnavailable",
        ));
    }
    ctrl_conn
        .record_query_version(
            record::X11_XML_VERSION.0 as _,
            record::X11_XML_VERSION.1 as _,
        )
        .map_err(|err| generic_error(format!("InputHook.start failed: RecordQueryVersion: {err}")))?
        .reply()
        .map_err(|err| {
            generic_error(format!("InputHook.start failed: RecordQueryVersion: {err}"))
        })?;

    let keysyms = KeysymCache::fetch(&ctrl_conn)
        .map_err(|err| generic_error(format!("InputHook.start failed: {err}")))?;

    let record_ctx = ctrl_conn
        .generate_id()
        .map_err(|err| generic_error(format!("InputHook.start failed: GenerateId: {err}")))?;
    let empty = Range8 { first: 0, last: 0 };
    let empty_ext = ExtRange {
        major: empty,
        minor: Range16 { first: 0, last: 0 },
    };
    let range = Range {
        core_requests: empty,
        core_replies: empty,
        ext_requests: empty_ext,
        ext_replies: empty_ext,
        delivered_events: empty,
        device_events: Range8 {
            first: KEY_PRESS,
            last: BUTTON_RELEASE,
        },
        errors: empty,
        client_started: false,
        client_died: false,
    };
    ctrl_conn
        .record_create_context(record_ctx, 0, &[record::CS::ALL_CLIENTS.into()], &[range])
        .map_err(|err| {
            generic_error(format!(
                "InputHook.start failed: RecordCreateContext: {err}"
            ))
        })?
        .check()
        .map_err(|err| {
            generic_error(format!(
                "InputHook.start failed: RecordCreateContext: {err}"
            ))
        })?;

    let stop = Arc::new(AtomicBool::new(false));
    let (ready_tx, ready_rx) = mpsc::sync_channel(1);

    let worker_ctrl = ctrl_conn.clone();
    let worker_callback = callback.clone();
    let worker_stop = stop.clone();
    let worker = thread::Builder::new()
        .name("fluxer-linux-input-hook".to_string())
        .spawn(move || {
            worker_main(
                &data_conn,
                Worker {
                    ctrl_conn: worker_ctrl,
                    record_ctx,
                    keysyms,
                    callback: worker_callback,
                    stop: worker_stop,
                    ready: Some(ready_tx),
                    held: HeldInputs::default(),
                },
            );
        });
    let worker = match worker {
        Ok(worker) => worker,
        Err(err) => {
            let _ = ctrl_conn.record_free_context(record_ctx);
            let _ = ctrl_conn.sync();
            return Err(generic_error(format!(
                "InputHook.start failed: thread spawn: {err}"
            )));
        }
    };

    let active = Active {
        ctrl_conn,
        record_ctx,
        worker: Some(worker),
        stop,
    };
    let failure = match ready_rx.recv_timeout(START_TIMEOUT) {
        Ok(Ok(())) => return Ok(active),
        Ok(Err(reason)) => reason,
        Err(mpsc::RecvTimeoutError::Timeout) => "RecordEnableContext timed out".to_string(),
        Err(mpsc::RecvTimeoutError::Disconnected) => {
            "RecordEnableContext worker exited".to_string()
        }
    };
    abandon(active);
    Err(generic_error(format!("InputHook.start failed: {failure}")))
}

struct Worker {
    ctrl_conn: Arc<RustConnection>,
    record_ctx: record::Context,
    keysyms: KeysymCache,
    callback: EventTsfn,
    stop: Arc<AtomicBool>,
    ready: Option<SyncSender<std::result::Result<(), String>>>,
    held: HeldInputs,
}

#[derive(Default)]
struct HeldInputs {
    keys: BTreeSet<u8>,
    buttons: BTreeSet<u8>,
}

impl HeldInputs {
    fn track(&mut self, event: &DecodedEvent) {
        match event.kind {
            EventKind::KeyDown => {
                self.keys.insert(event.x11_keycode);
            }
            EventKind::KeyUp => {
                self.keys.remove(&event.x11_keycode);
            }
            EventKind::MouseDown => {
                self.buttons.insert(event.button);
            }
            EventKind::MouseUp => {
                self.buttons.remove(&event.button);
            }
        }
    }

    fn take_releases(&mut self, keysyms: &KeysymCache) -> Vec<DecodedEvent> {
        let mods = modifiers::from_state(0);
        let keys = std::mem::take(&mut self.keys)
            .into_iter()
            .map(|x11_keycode| key_event(EventKind::KeyUp, x11_keycode, keysyms, mods));
        let buttons = std::mem::take(&mut self.buttons).into_iter().map(|button| {
            let mut event = DecodedEvent::new(EventKind::MouseUp, mods);
            event.button = button;
            event
        });
        keys.chain(buttons).collect()
    }
}

impl Worker {
    fn report_ready(&mut self, result: std::result::Result<(), String>) {
        if let Some(ready) = self.ready.take() {
            let _ = ready.send(result);
        }
    }

    fn refresh_keysyms_on_mapping_change(&mut self, data_conn: &RustConnection) {
        let mut keyboard_changed = false;
        while let Ok(Some(event)) = data_conn.poll_for_event() {
            if let Event::MappingNotify(notify) = event
                && notify.request == Mapping::KEYBOARD
            {
                keyboard_changed = true;
            }
        }
        if !keyboard_changed {
            return;
        }
        if let Ok(keysyms) = KeysymCache::fetch(&self.ctrl_conn) {
            self.keysyms = keysyms;
        }
        while let Ok(Some(_)) = self.ctrl_conn.poll_for_event() {}
    }
}

fn worker_main(data_conn: &RustConnection, mut worker: Worker) {
    let cookie = match data_conn.record_enable_context(worker.record_ctx) {
        Ok(c) => c,
        Err(err) => {
            worker.stop.store(true, Ordering::Release);
            worker.report_ready(Err(format!("RecordEnableContext: {err}")));
            return;
        }
    };

    for reply in cookie {
        if worker.stop.load(Ordering::Acquire) {
            break;
        }
        let reply = match reply {
            Ok(r) => r,
            Err(err) if worker.ready.is_some() => {
                worker.report_ready(Err(format!("RecordEnableContext: {err}")));
                break;
            }
            Err(ReplyError::ConnectionError(_)) => break,
            Err(_) => continue,
        };
        if reply.category == RECORD_START_OF_DATA {
            worker.report_ready(Ok(()));
            continue;
        }
        if reply.client_swapped || reply.category != RECORD_FROM_SERVER {
            continue;
        }
        worker.refresh_keysyms_on_mapping_change(data_conn);
        let mut data: &[u8] = &reply.data;
        while !data.is_empty() {
            let (consumed, event) = decode_one(data, &worker.keysyms);
            if let Some(event) = event {
                worker.held.track(&event);
                dispatch(&worker.callback, &worker.stop, event);
            }
            if consumed == 0 || consumed > data.len() {
                break;
            }
            data = &data[consumed..];
        }
    }
    if !worker.stop.swap(true, Ordering::AcqRel) {
        for event in worker.held.take_releases(&worker.keysyms) {
            dispatch(&worker.callback, &worker.stop, event);
        }
    }
    worker.report_ready(Err("RecordEnableContext ended before data".to_string()));
}

fn decode_one(data: &[u8], keysyms: &KeysymCache) -> (usize, Option<DecodedEvent>) {
    if data.is_empty() {
        return (0, None);
    }
    match data[0] {
        KEY_PRESS | KEY_RELEASE | BUTTON_PRESS | BUTTON_RELEASE => {
            if data.len() < std::mem::size_of::<XKeyButtonProto>() {
                return (0, None);
            }

            let evt: XKeyButtonProto =
                unsafe { std::ptr::read_unaligned(data.as_ptr() as *const XKeyButtonProto) };
            (32, decode_event(&evt, keysyms))
        }
        0 => {
            if data.len() < 8 {
                return (0, None);
            }
            let (length, _) = match u32::try_parse(&data[4..]) {
                Ok(v) => v,
                Err(_) => return (0, None),
            };
            (32 + (length as usize) * 4, None)
        }
        _ => (32, None),
    }
}

fn key_event(
    kind: EventKind,
    x11_keycode: u8,
    keysyms: &KeysymCache,
    mods: Modifiers,
) -> DecodedEvent {
    let keysym = keysyms.lookup(x11_keycode);
    let mut event = DecodedEvent::new(kind, mods);
    event.keycode = keysym;
    event.x11_keycode = x11_keycode;
    event.key_name = keymap::keysym_to_name(keysym)
        .unwrap_or_default()
        .to_string();
    event
}

fn decode_event(evt: &XKeyButtonProto, keysyms: &KeysymCache) -> Option<DecodedEvent> {
    let mods = modifiers::from_state(u32::from(evt.state));
    match evt.type_ {
        KEY_PRESS => Some(key_event(EventKind::KeyDown, evt.detail, keysyms, mods)),
        KEY_RELEASE => Some(key_event(EventKind::KeyUp, evt.detail, keysyms, mods)),
        BUTTON_PRESS | BUTTON_RELEASE => {
            let button = mouse::browser_button(u32::from(evt.detail))?;
            let mut event = DecodedEvent::new(
                if evt.type_ == BUTTON_PRESS {
                    EventKind::MouseDown
                } else {
                    EventKind::MouseUp
                },
                mods,
            );
            event.button = button;
            event.x = i32::from(evt.root_x);
            event.y = i32::from(evt.root_y);
            event.has_xy = true;
            Some(event)
        }
        _ => None,
    }
}

fn dispatch(callback: &EventTsfn, stop: &Arc<AtomicBool>, event: DecodedEvent) {
    let status = callback.call(event, ThreadsafeFunctionCallMode::NonBlocking);
    if status == Status::Closing {
        stop.store(true, Ordering::Release);
    }
}

fn abandon(active: Active) {
    active.stop.store(true, Ordering::Release);
    let _ = active.ctrl_conn.record_disable_context(active.record_ctx);
    let _ = active.ctrl_conn.record_free_context(active.record_ctx);
    let _ = active.ctrl_conn.flush();
}

fn tear_down(mut active: Active) {
    active.stop.store(true, Ordering::Release);

    if active.record_ctx != 0 {
        let _ = active.ctrl_conn.record_disable_context(active.record_ctx);
        let _ = active.ctrl_conn.sync();
    }
    if let Some(worker) = active.worker.take() {
        let _ = worker.join();
    }
    if active.record_ctx != 0 {
        let _ = active.ctrl_conn.record_free_context(active.record_ctx);
        let _ = active.ctrl_conn.sync();
    }
}

#[napi(js_name = "isAvailable")]
pub fn is_available() -> bool {
    if !detect_display_server().supports_global_xrecord() {
        return false;
    }
    x11rb::connect(None).is_ok()
}

fn generic_error(reason: impl Into<String>) -> napi::Error {
    napi::Error::new(Status::GenericFailure, reason.into())
}

#[allow(dead_code)]
const _ASSERT_PROTO_LAYOUT: fn() = || {
    use std::mem::offset_of;
    let _ = offset_of!(XKeyButtonProto, seq_l);
    let _ = offset_of!(XKeyButtonProto, seq_h);
    let _ = offset_of!(XKeyButtonProto, event);
    let _ = offset_of!(XKeyButtonProto, child);
    let _ = offset_of!(XKeyButtonProto, event_x);
    let _ = offset_of!(XKeyButtonProto, event_y);
    let _ = offset_of!(XKeyButtonProto, same_screen);
    let _ = offset_of!(XKeyButtonProto, pad0);
    let _ = offset_of!(XKeyButtonProto, root);
    let _ = offset_of!(XKeyButtonProto, time);
};

#[cfg(test)]
mod tests {
    use super::*;

    fn cache_with(x11_keycode: u8, keysym: u32) -> KeysymCache {
        let mut syms = vec![0; 248];
        syms[usize::from(x11_keycode - 8)] = keysym;
        KeysymCache {
            min_keycode: 8,
            syms,
        }
    }

    fn wire_event(type_: u8, detail: u8, state: u16) -> [u8; 32] {
        let mut data = [0u8; 32];
        data[0] = type_;
        data[1] = detail;
        data[28..30].copy_from_slice(&state.to_ne_bytes());
        data
    }

    #[test]
    fn decoded_keydown_has_keysym_raw_keycode_and_name() {
        let (consumed, event) = decode_one(&wire_event(KEY_PRESS, 38, 0), &cache_with(38, 0x0061));
        let event = event.expect("key event");
        assert_eq!(consumed, 32);
        assert!(matches!(event.kind, EventKind::KeyDown));
        assert_eq!(event.keycode, 0x0061);
        assert_eq!(event.x11_keycode, 38);
        assert_eq!(event.key_name, "A");
        assert!(!event.has_xy);
    }

    #[test]
    fn unknown_keysym_has_no_layout_key_name() {
        let (_, event) = decode_one(&wire_event(KEY_PRESS, 24, 0x4), &cache_with(24, 0x6ca));
        let event = event.expect("key event");
        assert_eq!(event.keycode, 0x6ca);
        assert_eq!(event.x11_keycode, 24);
        assert_eq!(event.key_name, "");
        assert!(event.mods.ctrl);
    }

    #[test]
    fn wheel_buttons_are_not_decoded() {
        let cache = cache_with(38, 0x0061);
        assert!(
            decode_one(&wire_event(BUTTON_PRESS, 4, 0), &cache)
                .1
                .is_none()
        );
        let (_, event) = decode_one(&wire_event(BUTTON_PRESS, 8, 0), &cache);
        assert_eq!(event.expect("button event").button, 3);
    }

    #[test]
    fn held_inputs_release_every_pressed_key_and_button_once() {
        let cache = cache_with(38, 0x0061);
        let mut held = HeldInputs::default();
        for data in [
            wire_event(KEY_PRESS, 38, 0),
            wire_event(KEY_PRESS, 37, 0),
            wire_event(KEY_RELEASE, 37, 0),
            wire_event(BUTTON_PRESS, 9, 0),
        ] {
            held.track(&decode_one(&data, &cache).1.expect("event"));
        }
        let releases = held.take_releases(&cache);
        assert_eq!(releases.len(), 2);
        assert!(matches!(releases[0].kind, EventKind::KeyUp));
        assert_eq!(releases[0].x11_keycode, 38);
        assert_eq!(releases[0].key_name, "A");
        assert!(!releases[0].mods.ctrl);
        assert!(matches!(releases[1].kind, EventKind::MouseUp));
        assert_eq!(releases[1].button, 4);
        assert!(held.take_releases(&cache).is_empty());
    }

    #[test]
    fn x_key_button_proto_layout_offsets_match_x11_wire_format() {
        use std::mem::offset_of;
        assert_eq!(offset_of!(XKeyButtonProto, type_), 0);
        assert_eq!(offset_of!(XKeyButtonProto, detail), 1);

        assert_eq!(offset_of!(XKeyButtonProto, time), 4);
        assert_eq!(offset_of!(XKeyButtonProto, root_x), 20);
        assert_eq!(offset_of!(XKeyButtonProto, root_y), 22);
        assert_eq!(offset_of!(XKeyButtonProto, state), 28);
    }

    #[test]
    fn event_kind_to_string_matches_js_contract() {
        assert_eq!(EventKind::KeyDown.as_str(), "keydown");
        assert_eq!(EventKind::KeyUp.as_str(), "keyup");
        assert_eq!(EventKind::MouseDown.as_str(), "mousedown");
        assert_eq!(EventKind::MouseUp.as_str(), "mouseup");
    }

    #[test]
    fn keysym_cache_returns_zero_below_min_keycode() {
        let cache = KeysymCache {
            min_keycode: 8,
            syms: vec![0x61, 0x62, 0x63],
        };
        assert_eq!(cache.lookup(7), 0);
        assert_eq!(cache.lookup(8), 0x61);
        assert_eq!(cache.lookup(10), 0x63);
        assert_eq!(cache.lookup(255), 0);
    }
}
