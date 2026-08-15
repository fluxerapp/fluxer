// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::backend::{BackendError, KeyedMutexBackend, TextureFormat};

#[derive(Clone)]
pub struct D3D11SharedHandle {
    pub raw_handle: u64,
    pub slot_index: u32,
    pub width: u32,
    pub height: u32,
}

#[cfg(target_os = "windows")]
pub struct D3D11KeyedMutexBackend {
    device: Option<windows::Win32::Graphics::Direct3D11::ID3D11Device>,
    #[allow(dead_code, reason = "held for RAII: device context lifetime")]
    context: Option<windows::Win32::Graphics::Direct3D11::ID3D11DeviceContext>,
    slots: Vec<D3D11SlotState>,
    width: u32,
    height: u32,
    format: TextureFormat,
}

#[cfg(target_os = "windows")]
struct D3D11SlotState {
    #[allow(
        dead_code,
        reason = "held for RAII: texture lifetime tied to keyed mutex"
    )]
    texture: windows::Win32::Graphics::Direct3D11::ID3D11Texture2D,
    keyed_mutex: windows::Win32::Graphics::Dxgi::IDXGIKeyedMutex,
    #[allow(dead_code, reason = "exposed via D3D11SharedHandle to clients")]
    shared_handle: u64,
    slot_index: u32,
    expected_key: u64,
    acquired: bool,
    completed: bool,
}

#[cfg(target_os = "windows")]
impl D3D11KeyedMutexBackend {
    pub fn new() -> Result<Self, BackendError> {
        let (device, context) = unsafe { create_d3d11_device_windows()? };
        let backend = Self {
            device: Some(device),
            context: Some(context),
            slots: Vec::with_capacity(<Self as KeyedMutexBackend>::NUM_SLOTS),
            width: 0,
            height: 0,
            format: TextureFormat::Nv12,
        };
        assert!(backend.slots.is_empty(), "fresh backend has no slots");
        assert!(backend.device.is_some(), "device created");
        Ok(backend)
    }

    fn find_slot(&mut self, slot: &D3D11SharedHandle) -> Option<usize> {
        assert!((slot.slot_index as usize) < <Self as KeyedMutexBackend>::NUM_SLOTS);
        self.slots
            .iter()
            .position(|s| s.slot_index == slot.slot_index)
    }

    pub fn texture_for_slot(
        &self,
        slot_index: u32,
    ) -> Option<windows::Win32::Graphics::Direct3D11::ID3D11Texture2D> {
        assert!(
            slot_index < (<Self as KeyedMutexBackend>::NUM_SLOTS as u32),
            "slot_index in range"
        );
        assert!(!self.slots.is_empty(), "slots have been created");
        for state in self.slots.iter() {
            if state.slot_index == slot_index {
                return Some(state.texture.clone());
            }
        }
        None
    }

    pub fn device(&self) -> Option<windows::Win32::Graphics::Direct3D11::ID3D11Device> {
        let dev = self.device.clone();
        assert!(dev.is_some(), "device exists");
        assert!(
            !self.slots.is_empty() || self.width == 0,
            "post-init invariant"
        );
        dev
    }

    pub fn context(&self) -> Option<windows::Win32::Graphics::Direct3D11::ID3D11DeviceContext> {
        let ctx = self.context.clone();
        assert!(ctx.is_some(), "context exists");
        assert!(
            self.width > 0 || self.slots.is_empty(),
            "post-init invariant"
        );
        ctx
    }
}

#[cfg(target_os = "windows")]
unsafe fn create_d3d11_device_windows() -> Result<
    (
        windows::Win32::Graphics::Direct3D11::ID3D11Device,
        windows::Win32::Graphics::Direct3D11::ID3D11DeviceContext,
    ),
    BackendError,
> {
    use windows::Win32::Foundation::HMODULE;
    use windows::Win32::Graphics::Direct3D::{D3D_DRIVER_TYPE_HARDWARE, D3D_FEATURE_LEVEL_11_0};
    use windows::Win32::Graphics::Direct3D11::{
        D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_SDK_VERSION, D3D11CreateDevice, ID3D11Device,
        ID3D11DeviceContext,
    };
    let mut device: Option<ID3D11Device> = None;
    let mut context: Option<ID3D11DeviceContext> = None;
    let feature_levels = [D3D_FEATURE_LEVEL_11_0];
    unsafe {
        D3D11CreateDevice(
            None,
            D3D_DRIVER_TYPE_HARDWARE,
            HMODULE::default(),
            D3D11_CREATE_DEVICE_BGRA_SUPPORT,
            Some(&feature_levels),
            D3D11_SDK_VERSION,
            Some(&mut device),
            None,
            Some(&mut context),
        )
    }
    .map_err(|_| BackendError::PlatformUnsupported {
        reason: "D3D11CreateDevice failed",
    })?;
    let device = device.ok_or(BackendError::PlatformUnsupported {
        reason: "D3D11 device null",
    })?;
    let context = context.ok_or(BackendError::PlatformUnsupported {
        reason: "D3D11 context null",
    })?;
    Ok((device, context))
}

#[cfg(target_os = "windows")]
unsafe fn create_keyed_mutex_texture_windows(
    device: &windows::Win32::Graphics::Direct3D11::ID3D11Device,
    width: u32,
    height: u32,
    slot_index: u32,
) -> Result<
    (
        windows::Win32::Graphics::Direct3D11::ID3D11Texture2D,
        windows::Win32::Graphics::Dxgi::IDXGIKeyedMutex,
        u64,
    ),
    BackendError,
> {
    use windows::Win32::Graphics::Direct3D11::{
        D3D11_BIND_RENDER_TARGET, D3D11_BIND_SHADER_RESOURCE,
        D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX, D3D11_RESOURCE_MISC_SHARED_NTHANDLE,
        D3D11_TEXTURE2D_DESC, D3D11_USAGE_DEFAULT, ID3D11Texture2D,
    };
    use windows::Win32::Graphics::Dxgi::Common::{DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_SAMPLE_DESC};
    use windows::Win32::Graphics::Dxgi::{IDXGIKeyedMutex, IDXGIResource1};
    use windows::core::Interface;
    assert!(width > 0);
    assert!(height > 0);
    assert!(slot_index < 8);
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
        BindFlags: (D3D11_BIND_RENDER_TARGET.0 | D3D11_BIND_SHADER_RESOURCE.0) as u32,
        CPUAccessFlags: 0,
        MiscFlags: (D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX.0 | D3D11_RESOURCE_MISC_SHARED_NTHANDLE.0)
            as u32,
    };
    let mut texture: Option<ID3D11Texture2D> = None;
    unsafe { device.CreateTexture2D(&desc, None, Some(&mut texture)) }.map_err(|_| {
        BackendError::PlatformUnsupported {
            reason: "CreateTexture2D failed",
        }
    })?;
    let texture = texture.ok_or(BackendError::PlatformUnsupported {
        reason: "texture null",
    })?;
    let keyed_mutex: IDXGIKeyedMutex =
        texture
            .cast()
            .map_err(|_| BackendError::PlatformUnsupported {
                reason: "IDXGIKeyedMutex cast failed",
            })?;
    let resource1: IDXGIResource1 =
        texture
            .cast()
            .map_err(|_| BackendError::PlatformUnsupported {
                reason: "IDXGIResource1 cast failed",
            })?;
    let access_rw: u32 = windows::Win32::Graphics::Dxgi::DXGI_SHARED_RESOURCE_READ.0
        | windows::Win32::Graphics::Dxgi::DXGI_SHARED_RESOURCE_WRITE.0;
    let shared =
        unsafe { resource1.CreateSharedHandle(None, access_rw, windows::core::PCWSTR::null()) }
            .map_err(|_| BackendError::PlatformUnsupported {
                reason: "CreateSharedHandle failed",
            })?;
    Ok((texture, keyed_mutex, shared.0 as u64))
}

#[cfg(target_os = "windows")]
const ACQUIRE_SYNC_WAIT_TIMEOUT: i32 = 0x102;
#[cfg(target_os = "windows")]
const ACQUIRE_SYNC_WAIT_ABANDONED: i32 = 0x80;

#[cfg(not(target_os = "windows"))]
pub struct D3D11KeyedMutexBackend;

#[cfg(not(target_os = "windows"))]
impl D3D11KeyedMutexBackend {
    pub fn new() -> Result<Self, BackendError> {
        Err(BackendError::PlatformUnsupported {
            reason: "D3D11 keyed-mutex backend is only available on Windows",
        })
    }
}

impl KeyedMutexBackend for D3D11KeyedMutexBackend {
    type SlotHandle = D3D11SharedHandle;
    const NUM_SLOTS: usize = 8;

    #[cfg(target_os = "windows")]
    fn create_slots(
        &mut self,
        width: u32,
        height: u32,
        format: TextureFormat,
    ) -> Result<Vec<D3D11SharedHandle>, BackendError> {
        assert!(self.slots.is_empty(), "slots created once");
        if width == 0 || height == 0 {
            return Err(BackendError::DimensionsOutOfRange { width, height });
        }
        if !matches!(format, TextureFormat::Nv12) {
            return Err(BackendError::UnsupportedFormat { format });
        }
        let device = self
            .device
            .as_ref()
            .ok_or(BackendError::PlatformUnsupported {
                reason: "device dropped",
            })?;
        let mut out: Vec<D3D11SharedHandle> = Vec::with_capacity(Self::NUM_SLOTS);
        for idx in 0..Self::NUM_SLOTS {
            let idx_u32 = idx as u32;
            let (texture, keyed_mutex, shared_handle) =
                unsafe { create_keyed_mutex_texture_windows(device, width, height, idx_u32)? };
            self.slots.push(D3D11SlotState {
                texture,
                keyed_mutex,
                shared_handle,
                slot_index: idx_u32,
                expected_key: 0,
                acquired: false,
                completed: false,
            });
            out.push(D3D11SharedHandle {
                raw_handle: shared_handle,
                slot_index: idx_u32,
                width,
                height,
            });
        }
        self.width = width;
        self.height = height;
        self.format = format;
        assert_eq!(out.len(), Self::NUM_SLOTS);
        assert_eq!(self.slots.len(), Self::NUM_SLOTS);
        Ok(out)
    }

    #[cfg(not(target_os = "windows"))]
    fn create_slots(
        &mut self,
        _width: u32,
        _height: u32,
        _format: TextureFormat,
    ) -> Result<Vec<D3D11SharedHandle>, BackendError> {
        Err(BackendError::PlatformUnsupported {
            reason: "real D3D11 create_slots requires Windows",
        })
    }

    #[cfg(target_os = "windows")]
    fn acquire_write(&mut self, slot: &D3D11SharedHandle, key: u64) -> Result<(), BackendError> {
        use windows::core::Interface;
        let position = self
            .find_slot(slot)
            .ok_or(BackendError::PlatformUnsupported {
                reason: "slot not found",
            })?;
        let state = &mut self.slots[position];
        if state.expected_key != key {
            return Err(BackendError::KeyMismatch {
                expected: state.expected_key,
                observed: key,
            });
        }
        if state.acquired {
            return Err(BackendError::AcquireWhileWriting {
                slot_index: state.slot_index,
            });
        }
        let hr = unsafe {
            (Interface::vtable(&state.keyed_mutex).AcquireSync)(
                Interface::as_raw(&state.keyed_mutex),
                key,
                0,
            )
        };
        if hr.0 == ACQUIRE_SYNC_WAIT_TIMEOUT || hr.0 == ACQUIRE_SYNC_WAIT_ABANDONED {
            return Err(BackendError::WouldBlock {
                slot_index: state.slot_index,
            });
        }
        if hr.is_err() {
            return Err(BackendError::KeyMismatch {
                expected: key,
                observed: u64::MAX,
            });
        }
        state.acquired = true;
        state.completed = false;
        assert!(state.acquired);
        Ok(())
    }

    #[cfg(not(target_os = "windows"))]
    fn acquire_write(&mut self, _slot: &D3D11SharedHandle, _key: u64) -> Result<(), BackendError> {
        Err(BackendError::PlatformUnsupported {
            reason: "real D3D11 acquire_write requires Windows",
        })
    }

    #[cfg(target_os = "windows")]
    fn release_write(
        &mut self,
        slot: &D3D11SharedHandle,
        next_key: u64,
    ) -> Result<(), BackendError> {
        let position = self
            .find_slot(slot)
            .ok_or(BackendError::PlatformUnsupported {
                reason: "slot not found",
            })?;
        let state = &mut self.slots[position];
        if !state.acquired {
            return Err(BackendError::ReleaseWithoutAcquire {
                slot_index: state.slot_index,
            });
        }
        unsafe { state.keyed_mutex.ReleaseSync(next_key) }.map_err(|_| {
            BackendError::KeyMismatch {
                expected: next_key,
                observed: u64::MAX,
            }
        })?;
        state.acquired = false;
        state.expected_key = next_key;
        state.completed = true;
        assert!(!state.acquired);
        assert!(state.completed);
        Ok(())
    }

    #[cfg(not(target_os = "windows"))]
    fn release_write(
        &mut self,
        _slot: &D3D11SharedHandle,
        _next_key: u64,
    ) -> Result<(), BackendError> {
        Err(BackendError::PlatformUnsupported {
            reason: "real D3D11 release_write requires Windows",
        })
    }

    #[cfg(target_os = "windows")]
    fn poll_complete(&mut self, slot: &D3D11SharedHandle) -> bool {
        match self.find_slot(slot) {
            Some(position) => self.slots[position].completed,
            None => false,
        }
    }

    #[cfg(not(target_os = "windows"))]
    fn poll_complete(&mut self, _slot: &D3D11SharedHandle) -> bool {
        false
    }

    #[cfg(target_os = "windows")]
    fn mark_consumed(&mut self, slot: &D3D11SharedHandle) {
        if let Some(position) = self.find_slot(slot) {
            self.slots[position].completed = false;
        }
    }

    #[cfg(not(target_os = "windows"))]
    fn mark_consumed(&mut self, _slot: &D3D11SharedHandle) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(not(target_os = "windows"))]
    fn instance_creation_fails_on_non_windows() {
        let err = D3D11KeyedMutexBackend::new().err();
        assert!(matches!(
            err,
            Some(BackendError::PlatformUnsupported { .. })
        ));
    }

    #[test]
    fn handle_clones_preserve_index() {
        let h = D3D11SharedHandle {
            raw_handle: 0xdead,
            slot_index: 3,
            width: 1920,
            height: 1080,
        };
        let h2 = h.clone();
        assert_eq!(h2.slot_index, 3);
        assert_eq!(h2.raw_handle, 0xdead);
    }
}
