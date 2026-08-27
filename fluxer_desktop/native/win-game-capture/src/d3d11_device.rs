// SPDX-License-Identifier: AGPL-3.0-or-later

use windows::Win32::{
    Foundation::HMODULE,
    Graphics::{
        Direct3D::{D3D_DRIVER_TYPE_HARDWARE, D3D_DRIVER_TYPE_UNKNOWN},
        Direct3D11::{
            D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_SDK_VERSION, D3D11CreateDevice, ID3D11Device,
            ID3D11DeviceContext,
        },
        Dxgi::{IDXGIAdapter, IDXGIDevice},
    },
};
use windows::core::Interface;

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
        .map_err(|error| format!("D3D11CreateDevice for shared capture texture: {error}"))?;
    }
    let device = device.ok_or("D3D11 shared texture device was None")?;
    let context = context.ok_or("D3D11 shared texture context was None")?;
    if let Ok(dxgi_device) = device.cast::<IDXGIDevice>() {
        let _ = unsafe { dxgi_device.SetGPUThreadPriority(7) };
    }
    Ok((device, context))
}
