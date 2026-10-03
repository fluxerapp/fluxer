// SPDX-License-Identifier: AGPL-3.0-or-later

use std::collections::BTreeSet;
use std::ffi::c_void;
use std::ptr;
use std::sync::{
    Arc, Mutex, MutexGuard,
    atomic::{AtomicBool, AtomicPtr, AtomicU64, Ordering},
    mpsc::{Receiver, Sender, channel},
};
use std::thread::{self, JoinHandle};

use core_foundation::base::TCFType;
use core_foundation::mach_port::CFMachPortRef;
use core_foundation::runloop::{CFRunLoop, kCFRunLoopCommonModes};
use core_graphics::event::{
    CGEvent, CGEventTap, CGEventTapLocation, CGEventTapOptions, CGEventTapPlacement,
    CGEventTapProxy, CGEventType, CallbackResult, EventField,
};
use napi::bindgen_prelude::{Env, Error, Function, Object, Result, Status, ToNapiValue};
use napi::sys;
use napi::threadsafe_function::{
    ThreadsafeFunction, ThreadsafeFunctionCallMode, UnknownReturnValue,
};
use napi_derive::napi;

use crate::caps_lock_hid::CapsLockHidListener;
use crate::keymap;
use crate::modifiers::{self, Modifiers};
use crate::mouse::{self, CgEventType, Classification};

#[link(name = "CoreGraphics", kind = "framework")]
unsafe extern "C" {
    fn CGEventTapEnable(tap: CFMachPortRef, enable: bool);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InputEventKind {
    KeyDown,
    KeyUp,
    MouseDown,
    MouseUp,
}

#[derive(Debug)]
pub struct InputEventPayload {
    kind: InputEventKind,
    mods: Modifiers,
    keycode: u32,
    key_name: String,
    button: u8,
    x: i32,
    y: i32,
    has_xy: bool,
}

impl ToNapiValue for InputEventPayload {
    unsafe fn to_napi_value(raw_env: sys::napi_env, value: Self) -> Result<sys::napi_value> {
        let env = Env::from_raw(raw_env);
        let mut obj = Object::new(&env)?;
        let kind_str = match value.kind {
            InputEventKind::KeyDown => "keydown",
            InputEventKind::KeyUp => "keyup",
            InputEventKind::MouseDown => "mousedown",
            InputEventKind::MouseUp => "mouseup",
        };
        obj.set("type", kind_str)?;
        obj.set("ctrlKey", value.mods.ctrl)?;
        obj.set("altKey", value.mods.alt)?;
        obj.set("shiftKey", value.mods.shift)?;
        obj.set("metaKey", value.mods.meta)?;

        match value.kind {
            InputEventKind::KeyDown | InputEventKind::KeyUp => {
                obj.set("keycode", value.keycode)?;
                obj.set("keyName", value.key_name)?;
            }
            InputEventKind::MouseDown | InputEventKind::MouseUp => {
                obj.set("button", u32::from(value.button))?;
                if value.has_xy {
                    obj.set("x", value.x)?;
                    obj.set("y", value.y)?;
                }
            }
        }
        unsafe { <Object<'_> as ToNapiValue>::to_napi_value(raw_env, obj) }
    }
}

type EventTsfn = Arc<
    ThreadsafeFunction<
        InputEventPayload,
        UnknownReturnValue,
        InputEventPayload,
        Status,
        false,
        true,
    >,
>;

struct WorkerState {
    run_loop: CFRunLoop,
    join: JoinHandle<()>,
}

#[napi]
pub struct InputHook {
    tsfn: EventTsfn,
    state: Mutex<Option<WorkerState>>,
}

#[napi]
impl InputHook {
    #[napi(constructor)]
    pub fn new(callback: Function<InputEventPayload, UnknownReturnValue>) -> Result<Self> {
        let tsfn = Arc::new(
            callback
                .build_threadsafe_function::<InputEventPayload>()
                .weak::<true>()
                .callee_handled::<false>()
                .build()
                .map_err(|err| {
                    Error::new(
                        Status::GenericFailure,
                        format!("failed to create input-hook callback: {}", err.reason),
                    )
                })?,
        );
        Ok(Self {
            tsfn,
            state: Mutex::new(None),
        })
    }

    #[napi]
    pub fn start(&self) -> Result<()> {
        let mut guard = self
            .state
            .lock()
            .map_err(|_| Error::new(Status::GenericFailure, "input hook state mutex poisoned"))?;
        if guard.is_some() {
            return Ok(());
        }

        let (loop_tx, loop_rx): (Sender<std::result::Result<CFRunLoop, String>>, Receiver<_>) =
            channel();
        let tsfn = self.tsfn.clone();
        let join = thread::Builder::new()
            .name("macos-input-hook".to_owned())
            .spawn(move || worker_main(tsfn, loop_tx))
            .map_err(|err| {
                Error::new(
                    Status::GenericFailure,
                    format!("InputHook.start failed: thread spawn failed: {err}"),
                )
            })?;

        let run_loop = match loop_rx.recv() {
            Ok(Ok(run_loop)) => run_loop,
            Ok(Err(err)) => {
                let _ = join.join();
                return Err(Error::new(
                    Status::GenericFailure,
                    format!("InputHook.start failed: {err}"),
                ));
            }
            Err(_) => {
                let _ = join.join();
                return Err(Error::new(
                    Status::GenericFailure,
                    "InputHook.start failed: worker exited before signalling",
                ));
            }
        };

        *guard = Some(WorkerState { run_loop, join });
        Ok(())
    }

    #[napi]
    pub fn stop(&self) -> Result<()> {
        let state = {
            let mut guard = self.state.lock().map_err(|_| {
                Error::new(Status::GenericFailure, "input hook state mutex poisoned")
            })?;
            guard.take()
        };
        if let Some(WorkerState { run_loop, join }) = state {
            run_loop.stop();
            let _ = join.join();
        }
        Ok(())
    }
}

impl Drop for InputHook {
    fn drop(&mut self) {
        if let Ok(mut guard) = self.state.lock()
            && let Some(WorkerState { run_loop, join }) = guard.take()
        {
            run_loop.stop();
            let _ = join.join();
        }
    }
}

const EVENTS_OF_INTEREST: &[CGEventType] = &[
    CGEventType::KeyDown,
    CGEventType::KeyUp,
    CGEventType::FlagsChanged,
    CGEventType::LeftMouseDown,
    CGEventType::LeftMouseUp,
    CGEventType::RightMouseDown,
    CGEventType::RightMouseUp,
    CGEventType::OtherMouseDown,
    CGEventType::OtherMouseUp,
];

const CAPS_LOCK_KEYCODE: u16 = 0x39;

fn worker_main(tsfn: EventTsfn, loop_tx: Sender<std::result::Result<CFRunLoop, String>>) {
    let tap_ref = Arc::new(AtomicPtr::new(ptr::null_mut()));
    let last_flags = Arc::new(AtomicU64::new(0));
    let caps_lock_via_hid = Arc::new(AtomicBool::new(false));
    let dispatcher = EventDispatcher {
        tsfn: tsfn.clone(),
        tap_ref: tap_ref.clone(),
        last_flags: last_flags.clone(),
        caps_lock_via_hid: caps_lock_via_hid.clone(),
        held: Mutex::new(HeldInputs::default()),
    };
    let tap_result = CGEventTap::new(
        CGEventTapLocation::Session,
        CGEventTapPlacement::HeadInsertEventTap,
        CGEventTapOptions::ListenOnly,
        EVENTS_OF_INTEREST.to_vec(),
        move |proxy, event_type, event| dispatcher.dispatch(proxy, event_type, event),
    );

    let tap = match tap_result {
        Ok(tap) => tap,
        Err(()) => {
            let _ = loop_tx.send(Err(
                "CGEventTapCreate returned NULL (accessibility permission denied?)".to_owned(),
            ));
            return;
        }
    };

    let loop_source = match tap.mach_port().create_runloop_source(0) {
        Ok(source) => source,
        Err(()) => {
            let _ = loop_tx.send(Err("CFMachPortCreateRunLoopSource returned NULL".to_owned()));
            return;
        }
    };

    let run_loop = CFRunLoop::get_current();
    unsafe { run_loop.add_source(&loop_source, kCFRunLoopCommonModes) };
    tap_ref.store(
        tap.mach_port().as_concrete_TypeRef().cast::<c_void>(),
        Ordering::Release,
    );
    tap.enable();

    let caps_tsfn = tsfn.clone();
    let caps_flags = last_flags.clone();
    let caps_listener = CapsLockHidListener::start(Box::new(move |pressed| {
        let payload = caps_lock_payload(pressed, caps_flags.load(Ordering::Relaxed));
        let _ = caps_tsfn.call(payload, ThreadsafeFunctionCallMode::NonBlocking);
    }));
    caps_lock_via_hid.store(caps_listener.is_some(), Ordering::Release);

    if loop_tx.send(Ok(run_loop)).is_err() {
        tap_ref.store(ptr::null_mut(), Ordering::Release);
        return;
    }

    CFRunLoop::run_current();
    tap_ref.store(ptr::null_mut(), Ordering::Release);
    drop(caps_listener);
    drop(tap);
    drop(loop_source);
}

fn caps_lock_payload(pressed: bool, flags: u64) -> InputEventPayload {
    InputEventPayload {
        kind: if pressed {
            InputEventKind::KeyDown
        } else {
            InputEventKind::KeyUp
        },
        mods: modifiers::from_flags(flags),
        keycode: u32::from(CAPS_LOCK_KEYCODE),
        key_name: keymap::keycode_name_or_fallback(CAPS_LOCK_KEYCODE),
        button: 0,
        x: 0,
        y: 0,
        has_xy: false,
    }
}

#[derive(Default)]
struct HeldInputs {
    keys: BTreeSet<u16>,
    buttons: BTreeSet<u8>,
}

impl HeldInputs {
    fn track(&mut self, payload: &InputEventPayload) {
        #[allow(clippy::cast_possible_truncation)]
        let keycode = payload.keycode as u16;
        match payload.kind {
            InputEventKind::KeyDown => {
                self.keys.insert(keycode);
            }
            InputEventKind::KeyUp => {
                self.keys.remove(&keycode);
            }
            InputEventKind::MouseDown => {
                self.buttons.insert(payload.button);
            }
            InputEventKind::MouseUp => {
                self.buttons.remove(&payload.button);
            }
        }
    }

    fn take_releases(&mut self) -> Vec<InputEventPayload> {
        let mods = modifiers::from_flags(0);
        let keys = std::mem::take(&mut self.keys)
            .into_iter()
            .map(|keycode| key_payload(InputEventKind::KeyUp, keycode, mods));
        let buttons =
            std::mem::take(&mut self.buttons)
                .into_iter()
                .map(|button| InputEventPayload {
                    kind: InputEventKind::MouseUp,
                    mods,
                    keycode: 0,
                    key_name: String::new(),
                    button,
                    x: 0,
                    y: 0,
                    has_xy: false,
                });
        keys.chain(buttons).collect()
    }
}

fn key_payload(kind: InputEventKind, keycode: u16, mods: Modifiers) -> InputEventPayload {
    InputEventPayload {
        kind,
        mods,
        keycode: u32::from(keycode),
        key_name: keymap::keycode_name_or_fallback(keycode),
        button: 0,
        x: 0,
        y: 0,
        has_xy: false,
    }
}

struct EventDispatcher {
    tsfn: EventTsfn,
    tap_ref: Arc<AtomicPtr<c_void>>,
    last_flags: Arc<AtomicU64>,
    caps_lock_via_hid: Arc<AtomicBool>,
    held: Mutex<HeldInputs>,
}

impl EventDispatcher {
    fn dispatch(
        &self,
        _proxy: CGEventTapProxy,
        event_type: CGEventType,
        event: &CGEvent,
    ) -> CallbackResult {
        match event_type {
            CGEventType::TapDisabledByTimeout | CGEventType::TapDisabledByUserInput => {
                self.reenable_tap();
                self.release_all();
                return CallbackResult::Keep;
            }
            _ => {}
        }

        let flags = event.get_flags();
        self.last_flags.store(flags.bits(), Ordering::Relaxed);
        let mods = modifiers::from_flags(flags.bits());
        let Some(cls) = CgEventType::from_u32(event_type as u32) else {
            return CallbackResult::Keep;
        };

        match cls {
            CgEventType::KeyDown | CgEventType::KeyUp => {
                let raw = event.get_integer_value_field(EventField::KEYBOARD_EVENT_KEYCODE);
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                let keycode = (raw as u64) as u16;
                let kind = if matches!(cls, CgEventType::KeyDown) {
                    InputEventKind::KeyDown
                } else {
                    InputEventKind::KeyUp
                };
                self.send_key(kind, keycode, mods);
            }
            CgEventType::FlagsChanged => {
                let raw = event.get_integer_value_field(EventField::KEYBOARD_EVENT_KEYCODE);
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                let keycode = (raw as u64) as u16;
                if keycode == CAPS_LOCK_KEYCODE {
                    if self.caps_lock_via_hid.load(Ordering::Acquire) {
                        return CallbackResult::Keep;
                    }
                    self.send_key(InputEventKind::KeyDown, keycode, mods);
                    self.send_key(InputEventKind::KeyUp, keycode, mods);
                    return CallbackResult::Keep;
                }
                let was_held = self.held().keys.contains(&keycode);
                let Some(is_down) =
                    modifiers::modifier_key_down_from_flags(keycode, flags.bits(), was_held)
                else {
                    return CallbackResult::Keep;
                };
                let kind = if is_down {
                    InputEventKind::KeyDown
                } else {
                    InputEventKind::KeyUp
                };
                self.send_key(kind, keycode, mods);
            }
            CgEventType::LeftMouseDown
            | CgEventType::LeftMouseUp
            | CgEventType::RightMouseDown
            | CgEventType::RightMouseUp
            | CgEventType::OtherMouseDown
            | CgEventType::OtherMouseUp => {
                let raw_button =
                    event.get_integer_value_field(EventField::MOUSE_EVENT_BUTTON_NUMBER);
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                let other_btn = (raw_button as u64) as u32;
                if let Classification::Button(b) = mouse::classify(cls, other_btn) {
                    let point = event.location();
                    #[allow(clippy::cast_possible_truncation)]
                    let payload = InputEventPayload {
                        kind: if mouse::is_down(cls) {
                            InputEventKind::MouseDown
                        } else {
                            InputEventKind::MouseUp
                        },
                        mods,
                        keycode: 0,
                        key_name: String::new(),
                        button: b,
                        x: point.x as i32,
                        y: point.y as i32,
                        has_xy: true,
                    };
                    self.send(payload);
                }
            }
        }

        CallbackResult::Keep
    }

    fn send_key(&self, kind: InputEventKind, keycode: u16, mods: Modifiers) {
        self.send(key_payload(kind, keycode, mods));
    }

    fn held(&self) -> MutexGuard<'_, HeldInputs> {
        self.held
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn send(&self, payload: InputEventPayload) {
        self.held().track(&payload);
        let _ = self
            .tsfn
            .call(payload, ThreadsafeFunctionCallMode::NonBlocking);
    }

    fn release_all(&self) {
        let releases = self.held().take_releases();
        for payload in releases {
            let _ = self
                .tsfn
                .call(payload, ThreadsafeFunctionCallMode::NonBlocking);
        }
    }

    fn reenable_tap(&self) {
        let tap: CFMachPortRef = self.tap_ref.load(Ordering::Acquire).cast();
        if !tap.is_null() {
            unsafe { CGEventTapEnable(tap, true) };
        }
    }
}

pub fn has_accessibility_permission() -> bool {
    objc2_core_graphics::CGPreflightListenEventAccess()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn held_inputs_release_every_pressed_key_and_button_once() {
        let mods = modifiers::from_flags(0);
        let mut held = HeldInputs::default();
        held.track(&key_payload(InputEventKind::KeyDown, 0x31, mods));
        held.track(&key_payload(InputEventKind::KeyDown, 0x3b, mods));
        held.track(&key_payload(InputEventKind::KeyDown, 0x00, mods));
        held.track(&key_payload(InputEventKind::KeyUp, 0x00, mods));
        held.track(&InputEventPayload {
            kind: InputEventKind::MouseDown,
            mods,
            keycode: 0,
            key_name: String::new(),
            button: 3,
            x: 0,
            y: 0,
            has_xy: true,
        });
        let releases = held.take_releases();
        let summary: Vec<(InputEventKind, u32, u8)> = releases
            .iter()
            .map(|payload| (payload.kind, payload.keycode, payload.button))
            .collect();
        assert_eq!(
            summary,
            vec![
                (InputEventKind::KeyUp, 0x31, 0),
                (InputEventKind::KeyUp, 0x3b, 0),
                (InputEventKind::MouseUp, 0, 3),
            ]
        );
        assert!(releases.iter().all(|payload| !payload.mods.ctrl));
        assert!(held.take_releases().is_empty());
    }
}
