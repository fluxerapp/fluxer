// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::Task;
use napi::bindgen_prelude::{AsyncTask, Env, Error, Result, Status};
use napi_derive::napi;

#[cfg(target_os = "windows")]
const DROPFILES_HEADER_SIZE: usize = 20;
#[cfg(target_os = "windows")]
const DROPFILES_PFILES_OFFSET: usize = 0;
#[cfg(target_os = "windows")]
const DROPFILES_FWIDE_OFFSET: usize = 16;

pub struct WriteFileReferenceTask {
    path: String,
}

#[napi(js_name = "writeFileReferenceToClipboard")]
pub fn write_file_reference_to_clipboard(
    file_path: String,
) -> Result<AsyncTask<WriteFileReferenceTask>> {
    validate_file_path(&file_path)?;
    Ok(AsyncTask::new(WriteFileReferenceTask { path: file_path }))
}

impl Task for WriteFileReferenceTask {
    type Output = ();
    type JsValue = ();

    fn compute(&mut self) -> Result<Self::Output> {
        platform::write_file_reference(&self.path)
    }

    fn resolve(&mut self, _env: Env, _output: Self::Output) -> Result<Self::JsValue> {
        Ok(())
    }
}

fn validate_file_path(path: &str) -> Result<()> {
    if path.is_empty() {
        return Err(Error::new(Status::InvalidArg, "path must be non-empty"));
    }
    if path.as_bytes().contains(&0) {
        return Err(Error::new(
            Status::InvalidArg,
            "path must not contain NUL bytes",
        ));
    }
    Ok(())
}

#[cfg(target_os = "windows")]
fn build_hdrop_payload(path: &str) -> Vec<u8> {
    let utf16: Vec<u16> = path.encode_utf16().collect();
    let mut payload = vec![0; DROPFILES_HEADER_SIZE + ((utf16.len() + 2) * size_of::<u16>())];

    payload[DROPFILES_PFILES_OFFSET..DROPFILES_PFILES_OFFSET + size_of::<u32>()]
        .copy_from_slice(&(DROPFILES_HEADER_SIZE as u32).to_le_bytes());
    payload[DROPFILES_FWIDE_OFFSET..DROPFILES_FWIDE_OFFSET + size_of::<i32>()]
        .copy_from_slice(&1_i32.to_le_bytes());

    let mut offset = DROPFILES_HEADER_SIZE;
    for code_unit in utf16 {
        payload[offset..offset + size_of::<u16>()].copy_from_slice(&code_unit.to_le_bytes());
        offset += size_of::<u16>();
    }
    payload
}

#[cfg(target_os = "windows")]
mod platform {
    use super::build_hdrop_payload;
    use napi::bindgen_prelude::{Error, Result, Status};
    use std::ffi::c_void;
    use std::ptr::{copy_nonoverlapping, null_mut};
    use windows_sys::Win32::Foundation::{GetLastError, GlobalFree, HGLOBAL, HWND};
    use windows_sys::Win32::System::DataExchange::{
        CloseClipboard, EmptyClipboard, OpenClipboard, SetClipboardData,
    };
    use windows_sys::Win32::System::Memory::{
        GMEM_MOVEABLE, GMEM_ZEROINIT, GlobalAlloc, GlobalLock, GlobalUnlock,
    };
    use windows_sys::Win32::System::Ole::CF_HDROP;
    use windows_sys::Win32::UI::WindowsAndMessaging::{CreateWindowExW, DestroyWindow};

    const STATIC_WINDOW_CLASS: [u16; 7] = [
        'S' as u16, 'T' as u16, 'A' as u16, 'T' as u16, 'I' as u16, 'C' as u16, 0,
    ];

    struct OwnedGlobal {
        handle: HGLOBAL,
    }

    impl OwnedGlobal {
        fn new(handle: HGLOBAL) -> Self {
            Self { handle }
        }

        fn handle(&self) -> HGLOBAL {
            self.handle
        }

        fn into_clipboard(mut self) -> HGLOBAL {
            let handle = self.handle;
            self.handle = null_mut();
            handle
        }
    }

    impl Drop for OwnedGlobal {
        fn drop(&mut self) {
            if !self.handle.is_null() {
                unsafe {
                    let _ = GlobalFree(self.handle);
                }
            }
        }
    }

    struct LockedGlobal {
        handle: HGLOBAL,
        ptr: *mut c_void,
    }

    impl LockedGlobal {
        fn new(handle: HGLOBAL) -> Result<Self> {
            let ptr = unsafe { GlobalLock(handle) };
            if ptr.is_null() {
                return Err(last_error("GlobalLock failed"));
            }
            Ok(Self { handle, ptr })
        }

        fn as_mut_ptr(&self) -> *mut u8 {
            self.ptr.cast()
        }
    }

    impl Drop for LockedGlobal {
        fn drop(&mut self) {
            unsafe {
                let _ = GlobalUnlock(self.handle);
            }
        }
    }

    struct ClipboardOwnerWindow {
        hwnd: HWND,
    }

    impl ClipboardOwnerWindow {
        fn create() -> Result<Self> {
            let hwnd = unsafe {
                CreateWindowExW(
                    0,
                    STATIC_WINDOW_CLASS.as_ptr(),
                    null_mut(),
                    0,
                    0,
                    0,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                    null_mut(),
                    null_mut(),
                )
            };
            if hwnd.is_null() {
                return Err(last_error("CreateWindowExW failed"));
            }
            Ok(Self { hwnd })
        }

        fn hwnd(&self) -> HWND {
            self.hwnd
        }
    }

    impl Drop for ClipboardOwnerWindow {
        fn drop(&mut self) {
            unsafe {
                let _ = DestroyWindow(self.hwnd);
            }
        }
    }

    struct ClipboardGuard;

    impl ClipboardGuard {
        fn open(hwnd: HWND) -> Result<Self> {
            if unsafe { OpenClipboard(hwnd) } == 0 {
                return Err(last_error("OpenClipboard failed"));
            }
            Ok(Self)
        }
    }

    impl Drop for ClipboardGuard {
        fn drop(&mut self) {
            unsafe {
                let _ = CloseClipboard();
            }
        }
    }

    pub(super) fn write_file_reference(path: &str) -> Result<()> {
        let payload = build_hdrop_payload(path);
        let handle = unsafe { GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, payload.len()) };
        if handle.is_null() {
            return Err(last_error("GlobalAlloc failed"));
        }
        let memory = OwnedGlobal::new(handle);

        {
            let locked = LockedGlobal::new(memory.handle())?;
            unsafe {
                copy_nonoverlapping(payload.as_ptr(), locked.as_mut_ptr(), payload.len());
            }
        }

        let owner = ClipboardOwnerWindow::create()?;
        let _clipboard = ClipboardGuard::open(owner.hwnd())?;
        if unsafe { EmptyClipboard() } == 0 {
            return Err(last_error("EmptyClipboard failed"));
        }
        if unsafe { SetClipboardData(CF_HDROP as u32, memory.handle()) }.is_null() {
            return Err(last_error("SetClipboardData failed"));
        }
        let _ = memory.into_clipboard();
        Ok(())
    }

    fn last_error(message: &str) -> Error {
        Error::new(
            Status::GenericFailure,
            format!("{message} (err {})", unsafe { GetLastError() }),
        )
    }
}

#[cfg(not(target_os = "windows"))]
mod platform {
    use napi::bindgen_prelude::{Error, Result, Status};

    pub(super) fn write_file_reference(_path: &str) -> Result<()> {
        Err(Error::new(
            Status::GenericFailure,
            "win-clipboard not supported on this platform",
        ))
    }
}
