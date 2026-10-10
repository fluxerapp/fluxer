// SPDX-License-Identifier: AGPL-3.0-or-later

use napi_derive::napi;

pub const SOURCE_METAL: &str = "metal";
pub const SOURCE_DXGI: &str = "dxgi";
pub const SOURCE_LINUX_SYSFS: &str = "linux-sysfs";

#[napi(object)]
pub struct PlatformGpuDeviceInfo {
    pub active: bool,
    #[napi(js_name = "vendorId")]
    pub vendor_id: u32,
    #[napi(js_name = "deviceId")]
    pub device_id: u32,
    #[napi(js_name = "vendorName")]
    pub vendor_name: Option<String>,
    #[napi(js_name = "deviceString")]
    pub device_string: Option<String>,
    #[napi(js_name = "driverVendor")]
    pub driver_vendor: Option<String>,
    #[napi(js_name = "driverVersion")]
    pub driver_version: Option<String>,
    #[napi(js_name = "dedicatedVideoMemory")]
    pub dedicated_video_memory: Option<f64>,
    #[napi(js_name = "sharedSystemMemory")]
    pub shared_system_memory: Option<f64>,
    #[napi(js_name = "subsystemVendorId")]
    pub subsystem_vendor_id: Option<u32>,
    #[napi(js_name = "subsystemDeviceId")]
    pub subsystem_device_id: Option<u32>,
    #[napi(js_name = "registryId")]
    pub registry_id: Option<String>,
    #[napi(js_name = "adapterLuid")]
    pub adapter_luid: Option<String>,
    #[napi(js_name = "pciPath")]
    pub pci_path: Option<String>,
    pub integrated: Option<bool>,
    pub removable: Option<bool>,
    pub headless: Option<bool>,
    pub source: String,
}

#[napi(object)]
pub struct PlatformGpuInfo {
    pub devices: Vec<PlatformGpuDeviceInfo>,
    pub source: String,
    pub error: Option<String>,
}

#[napi(js_name = "getGpuInfo")]
pub fn get_gpu_info() -> PlatformGpuInfo {
    platform::gpu_info()
}

#[allow(dead_code)]
fn base_device(
    active: bool,
    vendor_id: u32,
    device_id: u32,
    source: &str,
) -> PlatformGpuDeviceInfo {
    PlatformGpuDeviceInfo {
        active,
        vendor_id,
        device_id,
        vendor_name: fluxer_desktop_native::platform_info::probe_helpers::vendor_name(vendor_id)
            .map(str::to_owned),
        device_string: None,
        driver_vendor: None,
        driver_version: None,
        dedicated_video_memory: None,
        shared_system_memory: None,
        subsystem_vendor_id: None,
        subsystem_device_id: None,
        registry_id: None,
        adapter_luid: None,
        pci_path: None,
        integrated: None,
        removable: None,
        headless: None,
        source: source.to_owned(),
    }
}

#[allow(dead_code)]
fn empty_info(source: &str) -> PlatformGpuInfo {
    PlatformGpuInfo {
        devices: Vec::new(),
        source: source.to_owned(),
        error: None,
    }
}

#[cfg(target_os = "macos")]
mod platform {
    use fluxer_desktop_native::platform_info::probe_helpers::{
        vendor_id_from_name, vendor_name, write_hex_u64,
    };
    use objc2::exception::catch;
    use objc2::rc::autoreleasepool;
    use objc2_metal::{MTLCopyAllDevices, MTLCreateSystemDefaultDevice, MTLDevice};

    use super::{PlatformGpuDeviceInfo, PlatformGpuInfo, SOURCE_METAL, empty_info};

    pub(super) fn gpu_info() -> PlatformGpuInfo {
        match catch(|| autoreleasepool(|_| gpu_info_inner())) {
            Ok(info) => info,
            Err(exception) => {
                let mut info = empty_info(SOURCE_METAL);
                info.error = Some(exception.map_or_else(
                    || "macOS GPU probe failed".to_owned(),
                    |exception| exception.to_string(),
                ));
                info
            }
        }
    }

    fn gpu_info_inner() -> PlatformGpuInfo {
        let default_device = MTLCreateSystemDefaultDevice();
        let default_registry_id = default_device.as_deref().map(MTLDevice::registryID);
        let all_devices = MTLCopyAllDevices();
        let mut devices = Vec::new();

        for index in 0..all_devices.count() {
            let device = all_devices.objectAtIndex(index);
            let active =
                default_registry_id.is_some_and(|registry_id| registry_id == device.registryID());
            devices.push(mac_device(&device, active));
        }

        if devices.is_empty() {
            if let Some(device) = default_device {
                devices.push(mac_device(&device, true));
            }
        } else if !devices.iter().any(|device| device.active) {
            devices[0].active = true;
        }

        PlatformGpuInfo {
            devices,
            source: SOURCE_METAL.to_owned(),
            error: None,
        }
    }

    fn mac_device(
        device: &objc2::runtime::ProtocolObject<dyn MTLDevice>,
        active: bool,
    ) -> PlatformGpuDeviceInfo {
        let name = device.name().to_string();
        let vendor_id = vendor_id_from_name(&name);
        PlatformGpuDeviceInfo {
            active,
            vendor_id,
            device_id: 0,
            vendor_name: vendor_name(vendor_id).map(str::to_owned),
            device_string: Some(if name.is_empty() {
                "Metal GPU".to_owned()
            } else {
                name
            }),
            driver_vendor: None,
            driver_version: None,
            dedicated_video_memory: None,
            shared_system_memory: None,
            subsystem_vendor_id: None,
            subsystem_device_id: None,
            registry_id: Some(write_hex_u64(device.registryID())),
            adapter_luid: None,
            pci_path: None,
            integrated: Some(device.isLowPower() || vendor_id == 0x106b),
            removable: Some(device.isRemovable()),
            headless: Some(device.isHeadless()),
            source: SOURCE_METAL.to_owned(),
        }
    }
}

#[allow(dead_code)]
mod dxgi_helpers {
    pub fn utf16_description(raw: &[u16; 128]) -> String {
        let end = raw.iter().position(|ch| *ch == 0).unwrap_or(raw.len());
        let text = String::from_utf16_lossy(&raw[..end]);
        if text.is_empty() {
            "DXGI adapter".to_owned()
        } else {
            text
        }
    }

    pub fn luid_string(high_part: i32, low_part: u32) -> String {
        format!("{:08x}:{low_part:08x}", high_part as u32)
    }
}

#[cfg(target_os = "windows")]
mod platform {
    use super::dxgi_helpers::{luid_string, utf16_description};
    use fluxer_desktop_native::platform_info::probe_helpers::vendor_name;
    use windows::Win32::Graphics::Dxgi::{
        CreateDXGIFactory1, DXGI_ADAPTER_DESC1, DXGI_ADAPTER_FLAG_SOFTWARE,
        DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE, IDXGIAdapter1, IDXGIFactory1, IDXGIFactory6,
    };

    use super::{PlatformGpuDeviceInfo, PlatformGpuInfo, SOURCE_DXGI, empty_info};

    pub(super) fn gpu_info() -> PlatformGpuInfo {
        let mut info = empty_info(SOURCE_DXGI);
        info.devices = enumerate_dxgi_adapters();
        info
    }

    fn enumerate_dxgi_adapters() -> Vec<PlatformGpuDeviceInfo> {
        unsafe {
            if let Ok(factory) = CreateDXGIFactory1::<IDXGIFactory6>() {
                return enumerate_factory6(&factory);
            }
            if let Ok(factory) = CreateDXGIFactory1::<IDXGIFactory1>() {
                return enumerate_factory1(&factory);
            }
        }
        Vec::new()
    }

    unsafe fn enumerate_factory6(factory: &IDXGIFactory6) -> Vec<PlatformGpuDeviceInfo> {
        let mut devices = Vec::new();
        let mut ordered_index = 0;
        loop {
            let Ok(adapter) = (unsafe {
                factory.EnumAdapterByGpuPreference::<IDXGIAdapter1>(
                    ordered_index,
                    DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE,
                )
            }) else {
                break;
            };
            if let Ok(desc) = unsafe { adapter.GetDesc1() } {
                push_dxgi_desc(&mut devices, &desc, ordered_index);
            }
            ordered_index += 1;
        }
        devices
    }

    unsafe fn enumerate_factory1(factory: &IDXGIFactory1) -> Vec<PlatformGpuDeviceInfo> {
        let mut devices = Vec::new();
        let mut ordered_index = 0;
        loop {
            let Ok(adapter) = (unsafe { factory.EnumAdapters1(ordered_index) }) else {
                break;
            };
            if let Ok(desc) = unsafe { adapter.GetDesc1() } {
                push_dxgi_desc(&mut devices, &desc, ordered_index);
            }
            ordered_index += 1;
        }
        devices
    }

    fn push_dxgi_desc(
        devices: &mut Vec<PlatformGpuDeviceInfo>,
        desc: &DXGI_ADAPTER_DESC1,
        ordered_index: u32,
    ) {
        if (desc.Flags & DXGI_ADAPTER_FLAG_SOFTWARE.0 as u32) != 0 {
            return;
        }
        let mut device = PlatformGpuDeviceInfo {
            active: ordered_index == 0,
            vendor_id: desc.VendorId,
            device_id: desc.DeviceId,
            vendor_name: vendor_name(desc.VendorId).map(str::to_owned),
            device_string: Some(utf16_description(&desc.Description)),
            driver_vendor: None,
            driver_version: None,
            dedicated_video_memory: Some(desc.DedicatedVideoMemory as f64),
            shared_system_memory: Some(desc.SharedSystemMemory as f64),
            subsystem_vendor_id: Some(desc.SubSysId >> 16),
            subsystem_device_id: Some(desc.SubSysId & 0xffff),
            registry_id: None,
            adapter_luid: None,
            pci_path: None,
            integrated: None,
            removable: None,
            headless: None,
            source: SOURCE_DXGI.to_owned(),
        };
        device.adapter_luid = Some(luid_string(
            desc.AdapterLuid.HighPart,
            desc.AdapterLuid.LowPart,
        ));
        devices.push(device);
    }
}

#[cfg(target_os = "linux")]
mod platform {
    use std::fs;
    use std::path::Path;

    use fluxer_desktop_native::platform_info::probe_helpers::{
        basename, is_drm_card_name, parse_hex_id, vendor_name,
    };

    use super::{PlatformGpuDeviceInfo, PlatformGpuInfo, SOURCE_LINUX_SYSFS, base_device};

    pub(super) fn gpu_info() -> PlatformGpuInfo {
        linux_gpu_info_from_drm_root(Path::new("/sys/class/drm"))
    }

    fn linux_gpu_info_from_drm_root(root: &Path) -> PlatformGpuInfo {
        let mut info = PlatformGpuInfo {
            devices: Vec::new(),
            source: SOURCE_LINUX_SYSFS.to_owned(),
            error: None,
        };
        let Ok(entries) = fs::read_dir(root) else {
            return info;
        };

        let mut card_names = entries
            .filter_map(Result::ok)
            .filter_map(|entry| entry.file_name().into_string().ok())
            .filter(|name| is_drm_card_name(name))
            .collect::<Vec<_>>();
        card_names.sort_unstable_by_key(|name| card_sort_key(name));

        for card_name in card_names {
            if let Some(device) = linux_card(root, &card_name, info.devices.is_empty()) {
                info.devices.push(device);
            }
        }
        info
    }

    fn linux_card(root: &Path, card_name: &str, active: bool) -> Option<PlatformGpuDeviceInfo> {
        let base = root.join(card_name).join("device");
        let vendor_id = read_hex_file(base.join("vendor"))?;
        let device_id = read_hex_file(base.join("device")).unwrap_or(0);

        let mut device = base_device(active, vendor_id, device_id, SOURCE_LINUX_SYSFS);
        device.subsystem_vendor_id =
            Some(read_hex_file(base.join("subsystem_vendor")).unwrap_or(0));
        device.subsystem_device_id =
            Some(read_hex_file(base.join("subsystem_device")).unwrap_or(0));
        device.pci_path = read_link_basename(&base);
        device.driver_vendor = read_link_basename(base.join("driver"));

        if let Some(uevent) = read_trimmed_file(base.join("uevent")) {
            if let Some(driver) = uevent_value(&uevent, "DRIVER") {
                device.driver_vendor = Some(driver.to_owned());
            }
            if let Some(pci_id) = uevent_value(&uevent, "PCI_ID") {
                device.device_string = Some(match vendor_name(vendor_id) {
                    Some(vendor) => format!("{vendor} GPU ({pci_id})"),
                    None => format!("GPU ({pci_id})"),
                });
            }
        }

        if let Some(vram) = read_trimmed_file(base.join("mem_info_vram_total"))
            && let Ok(bytes) = vram.parse::<u64>()
        {
            device.dedicated_video_memory = Some(bytes as f64);
        }

        Some(device)
    }

    fn card_sort_key(name: &str) -> u32 {
        name.strip_prefix("card")
            .and_then(|rest| rest.parse::<u32>().ok())
            .unwrap_or(u32::MAX)
    }

    fn read_hex_file(path: impl AsRef<Path>) -> Option<u32> {
        let raw = fs::read_to_string(path).ok()?;
        parse_hex_id(&raw).ok()
    }

    fn read_trimmed_file(path: impl AsRef<Path>) -> Option<String> {
        let raw = fs::read_to_string(path).ok()?;
        Some(raw.trim_matches([' ', '\t', '\r', '\n']).to_owned())
    }

    fn read_link_basename(path: impl AsRef<Path>) -> Option<String> {
        let target = fs::read_link(path).ok()?;
        Some(path_basename(&target))
    }

    fn path_basename(path: &Path) -> String {
        path.file_name()
            .and_then(|name| name.to_str())
            .map(str::to_owned)
            .unwrap_or_else(|| basename(&path.to_string_lossy()).to_owned())
    }

    fn uevent_value<'a>(blob: &'a str, key: &str) -> Option<&'a str> {
        blob.lines().find_map(|line| {
            let (line_key, value) = line.split_once('=')?;
            if line_key == key {
                Some(value.trim_matches([' ', '\t', '\r', '\n']))
            } else {
                None
            }
        })
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
mod platform {
    use super::{PlatformGpuInfo, SOURCE_LINUX_SYSFS, empty_info};

    pub(super) fn gpu_info() -> PlatformGpuInfo {
        empty_info(SOURCE_LINUX_SYSFS)
    }
}
