// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::live_items::{LiveItems, WeakItem};
use std::{
    cell::RefCell,
    ffi::c_void,
    mem::{offset_of, size_of},
    sync::{
        OnceLock,
        atomic::{AtomicPtr, AtomicU64, Ordering},
    },
};
use windows::{
    Win32::{
        Foundation::{HMODULE, HWND},
        System::{
            LibraryLoader::{
                GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, GET_MODULE_HANDLE_EX_FLAG_PIN,
                GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT, GetModuleHandleExW,
            },
            Memory::{PAGE_PROTECTION_FLAGS, PAGE_READWRITE, VirtualProtect},
            WinRT::{
                Graphics::Capture::{
                    IGraphicsCaptureItemInterop, IGraphicsCaptureItemInterop_Vtbl,
                },
                RoGetActivationFactory,
            },
        },
    },
    core::{GUID, HRESULT, IUnknown, Interface, PCWSTR, Weak, h, w},
};

type CreateForWindow =
    unsafe extern "system" fn(*mut c_void, HWND, *const GUID, *mut *mut c_void) -> HRESULT;

const _: () = assert!(
    offset_of!(IGraphicsCaptureItemInterop_Vtbl, CreateForWindow) == 3 * size_of::<usize>()
);
const _: () = assert!(size_of::<CreateForWindow>() == size_of::<*mut c_void>());

static INSTALLED: OnceLock<Result<(), String>> = OnceLock::new();
static CREATE_FOR_WINDOW_ORIGINAL: OnceLock<CreateForWindow> = OnceLock::new();
static CALLS_FORWARDED: AtomicU64 = AtomicU64::new(0);
static CALLS_REUSED: AtomicU64 = AtomicU64::new(0);

pub struct CreateForWindowCalls {
    pub forwarded: u64,
    pub reused: u64,
}

thread_local! {
    static LIVE_ITEMS: RefCell<LiveItems<Weak<IUnknown>>> =
        const { RefCell::new(LiveItems::new()) };
}

impl WeakItem for Weak<IUnknown> {
    type Strong = IUnknown;

    fn upgrade(&self) -> Option<IUnknown> {
        Weak::upgrade(self)
    }
}

fn live_item(window: isize) -> Option<IUnknown> {
    LIVE_ITEMS
        .try_with(|items| items.try_borrow_mut().ok()?.lookup(window))
        .ok()
        .flatten()
}

fn remember_item(window: isize, item: &IUnknown) {
    let Ok(weak) = item.downgrade() else {
        return;
    };
    let _ = LIVE_ITEMS.try_with(|items| {
        if let Ok(mut items) = items.try_borrow_mut() {
            items.remember(window, weak);
        }
    });
}

unsafe fn reuse_live_item(
    window: isize,
    iid: *const GUID,
    result: *mut *mut c_void,
) -> Option<HRESULT> {
    if iid.is_null() {
        return None;
    }
    if result.is_null() {
        return None;
    }
    let item = live_item(window)?;
    let reused = unsafe { item.query(iid, result) };
    reused.is_ok().then_some(reused)
}

unsafe extern "system" fn create_for_window_reusing_live_item(
    this: *mut c_void,
    window: HWND,
    iid: *const GUID,
    result: *mut *mut c_void,
) -> HRESULT {
    let Some(original) = CREATE_FOR_WINDOW_ORIGINAL.get().copied() else {
        unreachable!("the slot is replaced only after the original is recorded");
    };
    let window_key = window.0 as isize;
    if let Some(reused) = unsafe { reuse_live_item(window_key, iid, result) } {
        CALLS_REUSED.fetch_add(1, Ordering::Relaxed);
        return reused;
    }
    CALLS_FORWARDED.fetch_add(1, Ordering::Relaxed);
    let created = unsafe { original(this, window, iid, result) };
    if created.is_err() {
        return created;
    }
    if result.is_null() {
        return created;
    }
    if let Some(item) = unsafe { IUnknown::from_raw_borrowed(&*result) } {
        remember_item(window_key, item);
    }
    created
}

fn module_at(address: *const c_void, flags: u32) -> Result<HMODULE, String> {
    let mut module = HMODULE::default();
    unsafe {
        GetModuleHandleExW(
            GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | flags,
            PCWSTR(address.cast()),
            &mut module,
        )
    }
    .map_err(|error| format!("GetModuleHandleExW(address): {error}"))?;
    Ok(module)
}

fn pin_modules(slot: *const c_void) -> Result<(), String> {
    let mut graphics_capture = HMODULE::default();
    unsafe {
        GetModuleHandleExW(
            GET_MODULE_HANDLE_EX_FLAG_PIN,
            w!("GraphicsCapture.dll"),
            &mut graphics_capture,
        )
    }
    .map_err(|error| format!("GetModuleHandleExW(GraphicsCapture.dll): {error}"))?;
    if module_at(slot, GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT)? != graphics_capture {
        return Err("capture item factory vtable is outside GraphicsCapture.dll".into());
    }
    let replacement: CreateForWindow = create_for_window_reusing_live_item;
    module_at(replacement as *const c_void, GET_MODULE_HANDLE_EX_FLAG_PIN)?;
    Ok(())
}

fn replace_slot(slot: *mut CreateForWindow, original: CreateForWindow) -> Result<(), String> {
    let replacement: CreateForWindow = create_for_window_reusing_live_item;
    let size = size_of::<CreateForWindow>();
    let mut protection = PAGE_PROTECTION_FLAGS(0);
    unsafe { VirtualProtect(slot.cast(), size, PAGE_READWRITE, &mut protection) }
        .map_err(|error| format!("VirtualProtect(PAGE_READWRITE): {error}"))?;
    let slot_value = unsafe { AtomicPtr::<c_void>::from_ptr(slot.cast()) };
    slot_value.store(replacement as *mut c_void, Ordering::Release);
    let mut replaced = PAGE_PROTECTION_FLAGS(0);
    let restored = unsafe { VirtualProtect(slot.cast(), size, protection, &mut replaced) };
    if let Err(error) = restored {
        slot_value.store(original as *mut c_void, Ordering::Release);
        return Err(format!("VirtualProtect(restore): {error}"));
    }
    Ok(())
}

fn install_once() -> Result<(), String> {
    let interop: IGraphicsCaptureItemInterop =
        unsafe { RoGetActivationFactory(h!("Windows.Graphics.Capture.GraphicsCaptureItem")) }
            .map_err(|error| format!("GraphicsCaptureItem activation factory: {error}"))?;
    let vtable = unsafe {
        *interop
            .as_raw()
            .cast::<*mut IGraphicsCaptureItemInterop_Vtbl>()
    };
    assert!(!vtable.is_null());
    let slot = unsafe { &raw mut (*vtable).CreateForWindow };
    pin_modules(slot.cast())?;
    let original = unsafe { slot.read() };
    let replacement: CreateForWindow = create_for_window_reusing_live_item;
    assert!(!std::ptr::fn_addr_eq(original, replacement));
    CREATE_FOR_WINDOW_ORIGINAL
        .set(original)
        .map_err(|_| "CreateForWindow original already recorded".to_string())?;
    replace_slot(slot, original)
}

pub fn install() -> Result<(), String> {
    INSTALLED.get_or_init(install_once).clone()
}

pub fn create_for_window_calls() -> CreateForWindowCalls {
    CreateForWindowCalls {
        forwarded: CALLS_FORWARDED.load(Ordering::Relaxed),
        reused: CALLS_REUSED.load(Ordering::Relaxed),
    }
}
