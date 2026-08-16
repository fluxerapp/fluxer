// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(dead_code)]

use napi_derive::napi;

const NVENC_NATIVE_INPUTS: &[&str] = &["dmabuf"];
const VIDEOTOOLBOX_CODECS: &[&str] = &["h264", "h265"];

#[napi(object)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HardwareEncoderCapability {
    pub available: bool,
    pub backend: String,
    pub compiled: bool,
    pub runtime: bool,
    pub codecs: Vec<String>,
    pub zero_copy: bool,
    pub native_inputs: Vec<String>,
    pub reason: Option<String>,
    pub detail: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct NvencRuntimeProbe {
    libcuda: bool,
    libnvidia_encode: bool,
    get_max_supported_version: bool,
    driver_version_compatible: bool,
    create_instance: bool,
    cuda_init: bool,
    cuda_device_count: Option<i32>,
    cuda_device: bool,
    cuda_context: bool,
    checked_cuda_devices: u32,
    supported_cuda_devices: u32,
    open_encode_session: bool,
    query_encode_guids: bool,
    supports_h264: bool,
    supports_h265: bool,
    last_open_encode_session_status: Option<i32>,
}

impl NvencRuntimeProbe {
    fn missing() -> Self {
        Self {
            libcuda: false,
            libnvidia_encode: false,
            get_max_supported_version: false,
            driver_version_compatible: false,
            create_instance: false,
            cuda_init: false,
            cuda_device_count: None,
            cuda_device: false,
            cuda_context: false,
            checked_cuda_devices: 0,
            supported_cuda_devices: 0,
            open_encode_session: false,
            query_encode_guids: false,
            supports_h264: false,
            supports_h265: false,
            last_open_encode_session_status: None,
        }
    }

    fn ready(self) -> bool {
        self.libcuda
            && self.libnvidia_encode
            && self.get_max_supported_version
            && self.driver_version_compatible
            && self.create_instance
            && self.cuda_init
            && self.cuda_device
            && self.cuda_context
            && self.open_encode_session
            && self.has_supported_codec()
    }

    fn has_supported_codec(self) -> bool {
        self.supports_h264 || self.supports_h265
    }

    fn supported_codecs(self) -> Vec<&'static str> {
        let mut codecs = Vec::new();
        if self.supports_h264 {
            codecs.push("h264");
        }
        if self.supports_h265 {
            codecs.push("h265");
        }
        codecs
    }

    fn missing_detail(self, platform: CapabilityPlatform) -> String {
        let (cuda_library, encode_library) = nvenc_library_names(platform);
        let mut missing = Vec::new();
        if !self.libcuda {
            missing.push(cuda_library.to_string());
        }
        if !self.libnvidia_encode {
            missing.push(encode_library.to_string());
        }
        if self.libnvidia_encode && !self.get_max_supported_version {
            missing.push("NvEncodeAPIGetMaxSupportedVersion".to_string());
        }
        if self.get_max_supported_version && !self.driver_version_compatible {
            missing.push("NVENC driver support for the compiled SDK version".to_string());
        }
        if self.libnvidia_encode && !self.create_instance {
            missing.push("NvEncodeAPICreateInstance".to_string());
        }
        if self.libcuda && !self.cuda_init {
            missing.push("cuInit".to_string());
        }
        if self.cuda_init && self.cuda_device_count == Some(0) {
            missing.push("CUDA devices".to_string());
        }
        if self.cuda_init && !self.cuda_device {
            missing.push("CUDA device".to_string());
        }
        if self.cuda_device && !self.cuda_context {
            missing.push("CUDA context".to_string());
        }
        if self.cuda_context && !self.open_encode_session {
            missing.push(format!(
                "NVENC encode session ({})",
                self.open_encode_session_detail()
            ));
        }
        if self.open_encode_session && !self.query_encode_guids {
            missing.push("NVENC encode GUID query".to_string());
        }
        if self.open_encode_session && self.query_encode_guids && !self.has_supported_codec() {
            missing.push("NVENC H264/H265 encode GUID".to_string());
        }
        format!("missing {}", missing.join(", "))
    }

    fn unavailable_reason(self) -> &'static str {
        if self.get_max_supported_version && !self.driver_version_compatible {
            return "outdated_driver";
        }
        if self.cuda_init && self.cuda_device_count == Some(0) {
            return "no_devices";
        }
        if self.cuda_context && !self.open_encode_session && self.checked_cuda_devices > 0 {
            return "no_supported_devices";
        }
        if self.open_encode_session && !self.has_supported_codec() {
            return "no_supported_codecs";
        }
        "runtime_prerequisite_missing"
    }

    fn open_encode_session_detail(self) -> String {
        let mut parts = vec![format!(
            "checked {} CUDA device{}",
            self.checked_cuda_devices,
            if self.checked_cuda_devices == 1 {
                ""
            } else {
                "s"
            }
        )];
        if let Some(status) = self.last_open_encode_session_status {
            parts.push(format!(
                "last status {} ({status})",
                nvenc_status_name(status)
            ));
        }
        parts.join("; ")
    }
}

fn nvenc_status_name(status: i32) -> &'static str {
    match status {
        0 => "NV_ENC_SUCCESS",
        1 => "NV_ENC_ERR_NO_ENCODE_DEVICE",
        2 => "NV_ENC_ERR_UNSUPPORTED_DEVICE",
        3 => "NV_ENC_ERR_INVALID_ENCODERDEVICE",
        4 => "NV_ENC_ERR_INVALID_DEVICE",
        5 => "NV_ENC_ERR_DEVICE_NOT_EXIST",
        6 => "NV_ENC_ERR_INVALID_PTR",
        7 => "NV_ENC_ERR_INVALID_EVENT",
        8 => "NV_ENC_ERR_INVALID_PARAM",
        9 => "NV_ENC_ERR_INVALID_CALL",
        10 => "NV_ENC_ERR_OUT_OF_MEMORY",
        11 => "NV_ENC_ERR_ENCODER_NOT_INITIALIZED",
        12 => "NV_ENC_ERR_UNSUPPORTED_PARAM",
        13 => "NV_ENC_ERR_LOCK_BUSY",
        14 => "NV_ENC_ERR_NOT_ENOUGH_BUFFER",
        15 => "NV_ENC_ERR_INVALID_VERSION",
        16 => "NV_ENC_ERR_MAP_FAILED",
        17 => "NV_ENC_ERR_NEED_MORE_INPUT",
        18 => "NV_ENC_ERR_ENCODER_BUSY",
        19 => "NV_ENC_ERR_EVENT_NOT_REGISTERD",
        20 => "NV_ENC_ERR_GENERIC",
        21 => "NV_ENC_ERR_INCOMPATIBLE_CLIENT_KEY",
        22 => "NV_ENC_ERR_UNIMPLEMENTED",
        23 => "NV_ENC_ERR_RESOURCE_REGISTER_FAILED",
        24 => "NV_ENC_ERR_RESOURCE_NOT_REGISTERED",
        25 => "NV_ENC_ERR_RESOURCE_NOT_MAPPED",
        26 => "NV_ENC_ERR_NEED_MORE_OUTPUT",
        _ => "NV_ENC_ERR_UNKNOWN",
    }
}

fn videotoolbox_codec_detail(create_status: Option<i32>, prepare_status: Option<i32>) -> String {
    match (create_status, prepare_status) {
        (Some(0), Some(0)) => "hardware session ready".to_string(),
        (Some(0), Some(status)) => format!(
            "created, prepare status {} ({status})",
            videotoolbox_status_name(status)
        ),
        (Some(status), _) => format!(
            "create status {} ({status})",
            videotoolbox_status_name(status)
        ),
        (None, _) => "probe did not run".to_string(),
    }
}

fn videotoolbox_status_name(status: i32) -> &'static str {
    match status {
        0 => "noErr",
        -12901 => "kVTPropertyNotSupportedErr",
        -12902 => "kVTPropertyReadOnlyErr",
        -12903 => "kVTParameterErr",
        -12904 => "kVTInvalidSessionErr",
        -12905 => "kVTAllocationFailedErr",
        -12906 => "kVTPixelTransferNotSupportedErr",
        -12907 => "kVTCouldNotFindVideoDecoderErr",
        -12908 => "kVTCouldNotCreateInstanceErr",
        -12909 => "kVTCouldNotFindVideoEncoderErr",
        -12910 => "kVTVideoDecoderBadDataErr",
        -12911 => "kVTVideoDecoderUnsupportedDataFormatErr",
        -12912 => "kVTVideoDecoderMalfunctionErr",
        -12913 => "kVTVideoEncoderMalfunctionErr",
        -12914 => "kVTVideoDecoderNotAvailableNowErr",
        -12915 => "kVTVideoEncoderNotAvailableNowErr",
        -12916 => "kVTFormatDescriptionChangeNotSupportedErr",
        _ => "OSStatus",
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CapabilityPlatform {
    Linux,
    MacOS,
    Windows,
    Unsupported(&'static str),
}

fn nvenc_library_names(platform: CapabilityPlatform) -> (&'static str, &'static str) {
    match platform {
        CapabilityPlatform::Windows => {
            if cfg!(target_pointer_width = "32") {
                ("nvcuda.dll", "nvEncodeAPI.dll")
            } else {
                ("nvcuda.dll", "nvEncodeAPI64.dll")
            }
        }
        _ => ("libcuda.so.1", "libnvidia-encode.so.1"),
    }
}

fn nvenc_zero_copy_inputs(platform: CapabilityPlatform) -> &'static [&'static str] {
    match platform {
        CapabilityPlatform::Linux => NVENC_NATIVE_INPUTS,
        _ => &[],
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct VideoToolboxRuntimeProbe {
    h264_create_status: Option<i32>,
    h264_prepare_status: Option<i32>,
    h265_create_status: Option<i32>,
    h265_prepare_status: Option<i32>,
}

impl VideoToolboxRuntimeProbe {
    fn missing() -> Self {
        Self {
            h264_create_status: None,
            h264_prepare_status: None,
            h265_create_status: None,
            h265_prepare_status: None,
        }
    }

    fn supports_h264(self) -> bool {
        self.h264_create_status == Some(0) && self.h264_prepare_status == Some(0)
    }

    fn supports_h265(self) -> bool {
        self.h265_create_status == Some(0) && self.h265_prepare_status == Some(0)
    }

    fn ready(self) -> bool {
        self.supports_h264() || self.supports_h265()
    }

    fn supported_codecs(self) -> Vec<&'static str> {
        let mut codecs = Vec::new();
        if self.supports_h264() {
            codecs.push("h264");
        }
        if self.supports_h265() {
            codecs.push("h265");
        }
        codecs
    }

    fn missing_detail(self) -> String {
        format!(
            "VideoToolbox hardware encoder sessions unavailable: h264 {}; h265 {}",
            videotoolbox_codec_detail(self.h264_create_status, self.h264_prepare_status),
            videotoolbox_codec_detail(self.h265_create_status, self.h265_prepare_status)
        )
    }
}

#[napi]
pub fn get_hardware_encoder_capability() -> HardwareEncoderCapability {
    hardware_encoder_capability()
}

pub fn hardware_encoder_capability() -> HardwareEncoderCapability {
    if cfg!(target_os = "macos") {
        return describe_videotoolbox_capability(probe_macos_videotoolbox_runtime());
    }

    let platform = current_platform();
    let compiled = match platform {
        CapabilityPlatform::Linux => cfg!(fluxer_linux_nvenc),
        CapabilityPlatform::Windows => {
            cfg!(fluxer_windows_nvenc) && cfg!(fluxer_windows_nvenc_encoder)
        }
        _ => false,
    };
    describe_capability(
        platform,
        compiled,
        if compiled {
            probe_nvenc_runtime()
        } else {
            None
        },
    )
}

pub fn require_publish_codec_runtime_support(canonical_codec: &str) -> Result<(), String> {
    let capability = hardware_encoder_capability();
    require_publish_codec_runtime_support_for_capability(canonical_codec, &capability)
}

fn require_publish_codec_runtime_support_for_capability(
    canonical_codec: &str,
    capability: &HardwareEncoderCapability,
) -> Result<(), String> {
    if !canonical_codec.eq_ignore_ascii_case("h265") {
        return Ok(());
    }
    if capability.available
        && capability
            .codecs
            .iter()
            .any(|codec| codec.eq_ignore_ascii_case(canonical_codec))
    {
        return Ok(());
    }
    let detail = capability
        .detail
        .as_deref()
        .or(capability.reason.as_deref())
        .unwrap_or("hardware encoder is unavailable");
    Err(format!(
        "H.265 publishing requires hardware encoder support; {detail}"
    ))
}

fn current_platform() -> CapabilityPlatform {
    if cfg!(target_os = "linux") {
        CapabilityPlatform::Linux
    } else if cfg!(target_os = "macos") {
        CapabilityPlatform::MacOS
    } else if cfg!(target_os = "windows") {
        CapabilityPlatform::Windows
    } else {
        CapabilityPlatform::Unsupported(std::env::consts::OS)
    }
}

fn describe_videotoolbox_capability(
    runtime_probe: Option<VideoToolboxRuntimeProbe>,
) -> HardwareEncoderCapability {
    let runtime_probe = runtime_probe.unwrap_or_else(VideoToolboxRuntimeProbe::missing);
    let runtime = runtime_probe.ready();
    if runtime {
        let codecs = runtime_probe.supported_codecs();
        return HardwareEncoderCapability {
            available: true,
            backend: "videotoolbox".to_string(),
            compiled: true,
            runtime: true,
            codecs: string_vec(&codecs),
            zero_copy: false,
            native_inputs: Vec::new(),
            reason: None,
            detail: Some(format!(
                "VideoToolbox can create hardware-accelerated compression sessions for {}; supported hardware-codec probe set: {}",
                codecs.join(", "),
                VIDEOTOOLBOX_CODECS.join(", ")
            )),
        };
    }

    HardwareEncoderCapability {
        available: false,
        backend: "none".to_string(),
        compiled: cfg!(fluxer_macos_videotoolbox),
        runtime: false,
        codecs: Vec::new(),
        zero_copy: false,
        native_inputs: Vec::new(),
        reason: Some("no_supported_codecs".to_string()),
        detail: Some(runtime_probe.missing_detail()),
    }
}

fn describe_capability(
    platform: CapabilityPlatform,
    compiled: bool,
    runtime_probe: Option<NvencRuntimeProbe>,
) -> HardwareEncoderCapability {
    match platform {
        CapabilityPlatform::Unsupported(os) => HardwareEncoderCapability {
            available: false,
            backend: "none".to_string(),
            compiled: false,
            runtime: false,
            codecs: Vec::new(),
            zero_copy: false,
            native_inputs: Vec::new(),
            reason: Some("unsupported_platform".to_string()),
            detail: Some(format!(
                "NVENC hardware encoder capability probing is supported on Linux and Windows only; current platform is {os}"
            )),
        },
        CapabilityPlatform::MacOS => describe_videotoolbox_capability(None),
        CapabilityPlatform::Linux | CapabilityPlatform::Windows => {
            let runtime_probe = runtime_probe.unwrap_or_else(NvencRuntimeProbe::missing);
            let runtime = runtime_probe.ready();
            if compiled && runtime {
                let codecs = runtime_probe.supported_codecs();
                let native_inputs = nvenc_zero_copy_inputs(platform);
                let native_input_detail = if native_inputs.is_empty() {
                    "none".to_string()
                } else {
                    native_inputs.join(", ")
                };
                let (cuda_library, encode_library) = nvenc_library_names(platform);
                return HardwareEncoderCapability {
                    available: true,
                    backend: "nvenc".to_string(),
                    compiled: true,
                    runtime: true,
                    codecs: string_vec(&codecs),
                    zero_copy: !native_inputs.is_empty(),
                    native_inputs: string_vec(native_inputs),
                    reason: None,
                    detail: Some(format!(
                        "{cuda_library} and {encode_library} can open an NVENC encode session for {}; native zero-copy inputs: {}",
                        codecs.join(", "),
                        native_input_detail
                    )),
                };
            }

            let (reason, detail) = if !compiled {
                match platform {
                    CapabilityPlatform::Linux => (
                        "nvenc_not_compiled",
                        "NVENC support was not compiled for this Linux target; verify the architecture is supported and the vendored CUDA Driver API header is present".to_string(),
                    ),
                    CapabilityPlatform::Windows => (
                        "nvenc_not_compiled",
                        "Windows NVENC publishing is not compiled into this addon yet; CUDA/NVENC runtime probing alone is not enough to safely publish H.265 without codec fallback".to_string(),
                    ),
                    _ => unreachable!(),
                }
            } else {
                (
                    runtime_probe.unavailable_reason(),
                    runtime_probe.missing_detail(platform),
                )
            };

            HardwareEncoderCapability {
                available: false,
                backend: "none".to_string(),
                compiled,
                runtime,
                codecs: Vec::new(),
                zero_copy: false,
                native_inputs: Vec::new(),
                reason: Some(reason.to_string()),
                detail: Some(detail),
            }
        }
    }
}

fn string_vec(values: &[&str]) -> Vec<String> {
    values.iter().map(|value| (*value).to_string()).collect()
}

#[cfg(any(target_os = "linux", target_os = "windows"))]
fn probe_nvenc_runtime() -> Option<NvencRuntimeProbe> {
    nvenc_probe::probe_runtime()
}

#[cfg(any(target_os = "linux", target_os = "windows"))]
mod nvenc_probe {
    use super::{NvencRuntimeProbe, current_platform, nvenc_library_names};
    use libloading::Library;
    use std::ffi::c_void;
    use std::mem::transmute;

    const CUDA_SUCCESS: i32 = 0;
    const NV_ENC_SUCCESS: i32 = 0;
    const NV_ENC_DEVICE_TYPE_CUDA: u32 = 1;
    const NVENCAPI_MAJOR_VERSION: u32 = 12;
    const NVENCAPI_MINOR_VERSION: u32 = 0;
    const NVENCAPI_VERSION: u32 = NVENCAPI_MAJOR_VERSION | (NVENCAPI_MINOR_VERSION << 24);
    const NVENCAPI_COMPARABLE_VERSION: u32 = (NVENCAPI_MAJOR_VERSION << 4) | NVENCAPI_MINOR_VERSION;
    const NV_ENCODE_API_FUNCTION_LIST_VER: u32 = nvencapi_struct_version(2);
    const NV_ENC_OPEN_ENCODE_SESSION_EX_PARAMS_VER: u32 = nvencapi_struct_version(1);

    type CuInit = unsafe extern "system" fn(u32) -> i32;
    type CuDeviceGetCount = unsafe extern "system" fn(*mut i32) -> i32;
    type CuDeviceGet = unsafe extern "system" fn(*mut i32, i32) -> i32;
    type CuCtxCreateLegacy = unsafe extern "system" fn(*mut *mut c_void, u32, i32) -> i32;
    type CuCtxCreateCuda13 =
        unsafe extern "system" fn(*mut *mut c_void, *mut c_void, u32, i32) -> i32;
    type CuCtxDestroy = unsafe extern "system" fn(*mut c_void) -> i32;
    type NvEncodeApiGetMaxSupportedVersion = unsafe extern "system" fn(*mut u32) -> i32;
    type NvEncodeApiCreateInstance = unsafe extern "system" fn(*mut NvEncodeApiFunctionList) -> i32;
    type NvEncGetEncodeGuidCount = unsafe extern "system" fn(*mut c_void, *mut u32) -> i32;
    type NvEncGetEncodeGuids =
        unsafe extern "system" fn(*mut c_void, *mut Guid, u32, *mut u32) -> i32;
    type NvEncOpenEncodeSessionEx =
        unsafe extern "system" fn(*mut NvEncOpenEncodeSessionExParams, *mut *mut c_void) -> i32;
    type NvEncDestroyEncoder = unsafe extern "system" fn(*mut c_void) -> i32;

    #[repr(C)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Guid {
        data1: u32,
        data2: u16,
        data3: u16,
        data4: [u8; 8],
    }

    const NV_ENC_CODEC_H264_GUID: Guid = Guid {
        data1: 0x6bc82762,
        data2: 0x4e63,
        data3: 0x4ca4,
        data4: [0xaa, 0x85, 0x1e, 0x50, 0xf3, 0x21, 0xf6, 0xbf],
    };
    const NV_ENC_CODEC_HEVC_GUID: Guid = Guid {
        data1: 0x790cdc88,
        data2: 0x4522,
        data3: 0x4d7b,
        data4: [0x94, 0x25, 0xbd, 0xa9, 0x97, 0x5f, 0x76, 0x03],
    };

    #[repr(C)]
    struct NvEncodeApiFunctionList {
        version: u32,
        reserved: u32,
        functions: [*mut c_void; 41],
        reserved2: [*mut c_void; 277],
    }

    impl NvEncodeApiFunctionList {
        fn new() -> Self {
            Self {
                version: NV_ENCODE_API_FUNCTION_LIST_VER,
                reserved: 0,
                functions: [std::ptr::null_mut(); 41],
                reserved2: [std::ptr::null_mut(); 277],
            }
        }

        fn destroy_encoder(&self) -> Option<NvEncDestroyEncoder> {
            let pointer = self.functions[27];
            (!pointer.is_null()).then(|| unsafe { transmute(pointer) })
        }

        fn get_encode_guid_count(&self) -> Option<NvEncGetEncodeGuidCount> {
            let pointer = self.functions[1];
            (!pointer.is_null()).then(|| unsafe { transmute(pointer) })
        }

        fn get_encode_guids(&self) -> Option<NvEncGetEncodeGuids> {
            let pointer = self.functions[4];
            (!pointer.is_null()).then(|| unsafe { transmute(pointer) })
        }

        fn open_encode_session_ex(&self) -> Option<NvEncOpenEncodeSessionEx> {
            let pointer = self.functions[29];
            (!pointer.is_null()).then(|| unsafe { transmute(pointer) })
        }
    }

    #[repr(C)]
    struct NvEncOpenEncodeSessionExParams {
        version: u32,
        device_type: u32,
        device: *mut c_void,
        reserved: *mut c_void,
        api_version: u32,
        reserved1: [u32; 253],
        reserved2: [*mut c_void; 64],
    }

    impl NvEncOpenEncodeSessionExParams {
        fn cuda(device: *mut c_void) -> Self {
            Self {
                version: NV_ENC_OPEN_ENCODE_SESSION_EX_PARAMS_VER,
                device_type: NV_ENC_DEVICE_TYPE_CUDA,
                device,
                reserved: std::ptr::null_mut(),
                api_version: NVENCAPI_VERSION,
                reserved1: [0; 253],
                reserved2: [std::ptr::null_mut(); 64],
            }
        }
    }

    enum CuCtxCreate {
        Legacy(CuCtxCreateLegacy),
        Cuda13(CuCtxCreateCuda13),
    }

    impl CuCtxCreate {
        unsafe fn call(&self, context: *mut *mut c_void, device: i32) -> i32 {
            match self {
                Self::Legacy(create) => unsafe { create(context, 0, device) },
                Self::Cuda13(create) => unsafe { create(context, std::ptr::null_mut(), 0, device) },
            }
        }
    }

    const fn nvencapi_struct_version(version: u32) -> u32 {
        NVENCAPI_VERSION | (version << 16) | (0x7 << 28)
    }

    fn load_cuda_context_create(cuda: &Library) -> Option<CuCtxCreate> {
        let cuda_version = option_env!("FLUXER_CUDA_VERSION")
            .and_then(|value| value.parse::<u32>().ok())
            .unwrap_or(0);
        if cuda_version >= 13000 {
            unsafe { cuda.get::<CuCtxCreateCuda13>(b"cuCtxCreate\0") }
                .map(|symbol| CuCtxCreate::Cuda13(*symbol))
                .ok()
                .or_else(|| {
                    unsafe { cuda.get::<CuCtxCreateLegacy>(b"cuCtxCreate_v2\0") }
                        .map(|symbol| CuCtxCreate::Legacy(*symbol))
                        .ok()
                })
        } else {
            unsafe { cuda.get::<CuCtxCreateLegacy>(b"cuCtxCreate_v2\0") }
                .or_else(|_| unsafe { cuda.get::<CuCtxCreateLegacy>(b"cuCtxCreate\0") })
                .map(|symbol| CuCtxCreate::Legacy(*symbol))
                .ok()
        }
    }

    fn load_cuda_context_destroy(cuda: &Library) -> Option<CuCtxDestroy> {
        unsafe { cuda.get::<CuCtxDestroy>(b"cuCtxDestroy_v2\0") }
            .or_else(|_| unsafe { cuda.get::<CuCtxDestroy>(b"cuCtxDestroy\0") })
            .map(|symbol| *symbol)
            .ok()
    }

    fn query_nvenc_codecs(
        api: &NvEncodeApiFunctionList,
        encoder: *mut c_void,
        probe: &mut NvencRuntimeProbe,
    ) {
        let (Some(get_count), Some(get_guids)) =
            (api.get_encode_guid_count(), api.get_encode_guids())
        else {
            return;
        };
        let mut count = 0;
        if unsafe { get_count(encoder, &mut count) } != NV_ENC_SUCCESS || count == 0 {
            return;
        }
        let mut guids = vec![
            Guid {
                data1: 0,
                data2: 0,
                data3: 0,
                data4: [0; 8],
            };
            count as usize
        ];
        let mut written = 0;
        if unsafe { get_guids(encoder, guids.as_mut_ptr(), count, &mut written) } != NV_ENC_SUCCESS
        {
            return;
        }
        probe.query_encode_guids = true;
        for guid in guids.into_iter().take(written as usize) {
            if guid == NV_ENC_CODEC_H264_GUID {
                probe.supports_h264 = true;
            } else if guid == NV_ENC_CODEC_HEVC_GUID {
                probe.supports_h265 = true;
            }
        }
    }

    pub(super) fn probe_runtime() -> Option<NvencRuntimeProbe> {
        let mut probe = NvencRuntimeProbe::missing();
        let Some((cuda, encode)) = load_nvenc_libraries(&mut probe) else {
            return Some(probe);
        };
        let Some(api) = create_encode_api(&encode, &mut probe) else {
            return Some(probe);
        };
        let Some((cu_device_get, cuda_device_count)) = init_cuda(&cuda, &mut probe) else {
            return Some(probe);
        };
        probe_encode_sessions(&cuda, &api, cu_device_get, cuda_device_count, &mut probe);
        Some(probe)
    }

    fn load_nvenc_libraries(probe: &mut NvencRuntimeProbe) -> Option<(Library, Library)> {
        let (cuda_library, encode_library) = nvenc_library_names(current_platform());
        let Ok(cuda) = (unsafe { Library::new(cuda_library) }) else {
            return None;
        };
        probe.libcuda = true;

        let Ok(encode) = (unsafe { Library::new(encode_library) }) else {
            return None;
        };
        probe.libnvidia_encode = true;
        Some((cuda, encode))
    }

    fn create_encode_api(
        encode: &Library,
        probe: &mut NvencRuntimeProbe,
    ) -> Option<NvEncodeApiFunctionList> {
        let Ok(get_max_supported_version) = (unsafe {
            encode.get::<NvEncodeApiGetMaxSupportedVersion>(b"NvEncodeAPIGetMaxSupportedVersion\0")
        }) else {
            return None;
        };
        probe.get_max_supported_version = true;

        let mut max_supported_version = 0;
        if unsafe { get_max_supported_version(&mut max_supported_version) } != NV_ENC_SUCCESS
            || NVENCAPI_COMPARABLE_VERSION > max_supported_version
        {
            return None;
        }
        probe.driver_version_compatible = true;

        let Ok(create_instance) =
            (unsafe { encode.get::<NvEncodeApiCreateInstance>(b"NvEncodeAPICreateInstance\0") })
        else {
            return None;
        };
        let mut api = NvEncodeApiFunctionList::new();
        if unsafe { create_instance(&mut api) } != NV_ENC_SUCCESS {
            return None;
        }
        probe.create_instance = true;
        Some(api)
    }

    fn init_cuda(cuda: &Library, probe: &mut NvencRuntimeProbe) -> Option<(CuDeviceGet, i32)> {
        let Ok(cu_init) = (unsafe { cuda.get::<CuInit>(b"cuInit\0") }) else {
            return None;
        };
        if unsafe { cu_init(0) } != CUDA_SUCCESS {
            return None;
        }
        probe.cuda_init = true;

        let Ok(cu_device_get) = (unsafe { cuda.get::<CuDeviceGet>(b"cuDeviceGet\0") }) else {
            return None;
        };
        let cu_device_get = *cu_device_get;

        let Ok(cu_device_get_count) =
            (unsafe { cuda.get::<CuDeviceGetCount>(b"cuDeviceGetCount\0") })
        else {
            return None;
        };
        let mut cuda_device_count = 0;
        if unsafe { cu_device_get_count(&mut cuda_device_count) } != CUDA_SUCCESS {
            return None;
        }
        probe.cuda_device_count = Some(cuda_device_count);
        if cuda_device_count <= 0 {
            return None;
        }
        Some((cu_device_get, cuda_device_count))
    }

    fn probe_encode_sessions(
        cuda: &Library,
        api: &NvEncodeApiFunctionList,
        cu_device_get: CuDeviceGet,
        cuda_device_count: i32,
        probe: &mut NvencRuntimeProbe,
    ) {
        let Some(cu_ctx_create) = load_cuda_context_create(cuda) else {
            return;
        };
        let Some(cu_ctx_destroy) = load_cuda_context_destroy(cuda) else {
            return;
        };
        let Some(open_encode_session_ex) = api.open_encode_session_ex() else {
            return;
        };
        let Some(destroy_encoder) = api.destroy_encoder() else {
            return;
        };

        for ordinal in 0..cuda_device_count {
            let mut cuda_device = 0;
            if unsafe { cu_device_get(&mut cuda_device, ordinal) } != CUDA_SUCCESS {
                continue;
            }
            probe.cuda_device = true;

            let mut cuda_context = std::ptr::null_mut();
            if unsafe { cu_ctx_create.call(&mut cuda_context, cuda_device) } != CUDA_SUCCESS
                || cuda_context.is_null()
            {
                continue;
            }
            probe.cuda_context = true;
            probe.checked_cuda_devices += 1;

            let mut params = NvEncOpenEncodeSessionExParams::cuda(cuda_context);
            let mut encoder = std::ptr::null_mut();
            let status = unsafe { open_encode_session_ex(&mut params, &mut encoder) };
            probe.last_open_encode_session_status = Some(status);
            if status == NV_ENC_SUCCESS && !encoder.is_null() {
                query_nvenc_codecs(api, encoder, probe);
                let _ = unsafe { destroy_encoder(encoder) };
                probe.supported_cuda_devices += 1;
                probe.open_encode_session = true;
            }

            let _ = unsafe { cu_ctx_destroy(cuda_context) };

            if probe.open_encode_session && probe.has_supported_codec() {
                break;
            }
        }
    }
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
fn probe_nvenc_runtime() -> Option<NvencRuntimeProbe> {
    None
}

#[cfg(target_os = "macos")]
fn probe_macos_videotoolbox_runtime() -> Option<VideoToolboxRuntimeProbe> {
    use std::ffi::c_void;

    type CFAllocatorRef = *const c_void;
    type CFDictionaryRef = *const c_void;
    type CFIndex = isize;
    type CFStringRef = *const c_void;
    type CFTypeRef = *const c_void;
    type CMVideoCodecType = u32;
    type OSStatus = i32;
    type VTCompressionSessionRef = *mut c_void;
    type VTCompressionOutputCallback = Option<
        unsafe extern "C" fn(
            output_callback_refcon: *mut c_void,
            source_frame_refcon: *mut c_void,
            status: OSStatus,
            info_flags: u32,
            sample_buffer: *mut c_void,
        ),
    >;

    const K_CM_VIDEO_CODEC_TYPE_H264: CMVideoCodecType = 0x6176_6331;
    const K_CM_VIDEO_CODEC_TYPE_HEVC: CMVideoCodecType = 0x6876_6331;

    unsafe extern "C" {
        static kCFBooleanTrue: CFTypeRef;
        static kVTVideoEncoderSpecification_RequireHardwareAcceleratedVideoEncoder: CFStringRef;

        fn CFDictionaryCreate(
            allocator: CFAllocatorRef,
            keys: *const *const c_void,
            values: *const *const c_void,
            num_values: CFIndex,
            key_callbacks: *const c_void,
            value_callbacks: *const c_void,
        ) -> CFDictionaryRef;
        fn CFRelease(value: CFTypeRef);
        fn VTCompressionSessionCreate(
            allocator: CFAllocatorRef,
            width: i32,
            height: i32,
            codec_type: CMVideoCodecType,
            encoder_specification: CFDictionaryRef,
            image_buffer_attributes: CFDictionaryRef,
            compressed_data_allocator: CFAllocatorRef,
            output_callback: VTCompressionOutputCallback,
            output_callback_refcon: *mut c_void,
            compression_session_out: *mut VTCompressionSessionRef,
        ) -> OSStatus;
        fn VTCompressionSessionInvalidate(session: VTCompressionSessionRef);
        fn VTCompressionSessionPrepareToEncodeFrames(session: VTCompressionSessionRef) -> OSStatus;
    }

    unsafe fn hardware_required_dictionary() -> CFDictionaryRef {
        let key = unsafe {
            kVTVideoEncoderSpecification_RequireHardwareAcceleratedVideoEncoder as *const c_void
        };
        let value = unsafe { kCFBooleanTrue as *const c_void };
        let keys = [key];
        let values = [value];
        unsafe {
            CFDictionaryCreate(
                std::ptr::null(),
                keys.as_ptr(),
                values.as_ptr(),
                1,
                std::ptr::null(),
                std::ptr::null(),
            )
        }
    }

    unsafe fn probe_codec(codec_type: CMVideoCodecType) -> (Option<i32>, Option<i32>) {
        let encoder_specification = unsafe { hardware_required_dictionary() };
        let mut session = std::ptr::null_mut();
        let create_status = unsafe {
            VTCompressionSessionCreate(
                std::ptr::null(),
                1280,
                720,
                codec_type,
                encoder_specification,
                std::ptr::null(),
                std::ptr::null(),
                None,
                std::ptr::null_mut(),
                &mut session,
            )
        };
        if !encoder_specification.is_null() {
            unsafe { CFRelease(encoder_specification as CFTypeRef) };
        }
        if create_status != 0 || session.is_null() {
            return (Some(create_status), None);
        }

        let prepare_status = unsafe { VTCompressionSessionPrepareToEncodeFrames(session) };
        unsafe {
            VTCompressionSessionInvalidate(session);
            CFRelease(session as CFTypeRef);
        }
        (Some(create_status), Some(prepare_status))
    }

    let (h264_create_status, h264_prepare_status) =
        unsafe { probe_codec(K_CM_VIDEO_CODEC_TYPE_H264) };
    let (h265_create_status, h265_prepare_status) =
        unsafe { probe_codec(K_CM_VIDEO_CODEC_TYPE_HEVC) };

    Some(VideoToolboxRuntimeProbe {
        h264_create_status,
        h264_prepare_status,
        h265_create_status,
        h265_prepare_status,
    })
}

#[cfg(not(target_os = "macos"))]
fn probe_macos_videotoolbox_runtime() -> Option<VideoToolboxRuntimeProbe> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn runtime_ready() -> NvencRuntimeProbe {
        NvencRuntimeProbe {
            libcuda: true,
            libnvidia_encode: true,
            get_max_supported_version: true,
            driver_version_compatible: true,
            create_instance: true,
            cuda_init: true,
            cuda_device_count: Some(1),
            cuda_device: true,
            cuda_context: true,
            checked_cuda_devices: 1,
            supported_cuda_devices: 1,
            open_encode_session: true,
            query_encode_guids: true,
            supports_h264: true,
            supports_h265: true,
            last_open_encode_session_status: Some(0),
        }
    }

    fn videotoolbox_ready() -> VideoToolboxRuntimeProbe {
        VideoToolboxRuntimeProbe {
            h264_create_status: Some(0),
            h264_prepare_status: Some(0),
            h265_create_status: Some(0),
            h265_prepare_status: Some(0),
        }
    }

    #[test]
    fn reports_available_nvenc_when_compiled_and_runtime_ready() {
        let capability =
            describe_capability(CapabilityPlatform::Linux, true, Some(runtime_ready()));

        assert!(capability.available);
        assert_eq!(capability.backend, "nvenc");
        assert!(capability.compiled);
        assert!(capability.runtime);
        assert_eq!(capability.codecs, vec!["h264", "h265"]);
        assert!(capability.zero_copy);
        assert_eq!(capability.native_inputs, vec!["dmabuf"]);
        assert_eq!(capability.reason, None);
        assert!(
            capability
                .detail
                .as_deref()
                .unwrap_or_default()
                .contains("h264, h265")
        );
    }

    #[test]
    fn reports_h264_only_when_hevc_guid_is_absent() {
        let capability = describe_capability(
            CapabilityPlatform::Linux,
            true,
            Some(NvencRuntimeProbe {
                supports_h265: false,
                ..runtime_ready()
            }),
        );

        assert!(capability.available);
        assert_eq!(capability.codecs, vec!["h264"]);
        assert!(capability.zero_copy);
    }

    #[test]
    fn reports_windows_nvenc_without_linux_zero_copy_inputs() {
        let capability =
            describe_capability(CapabilityPlatform::Windows, true, Some(runtime_ready()));

        assert!(capability.available);
        assert_eq!(capability.backend, "nvenc");
        assert!(capability.compiled);
        assert!(capability.runtime);
        assert_eq!(capability.codecs, vec!["h264", "h265"]);
        assert!(!capability.zero_copy);
        assert_eq!(capability.native_inputs, Vec::<String>::new());
        assert_eq!(capability.reason, None);
        let detail = capability.detail.as_deref().unwrap_or_default();
        assert!(detail.contains("nvcuda.dll"), "{detail}");
        assert!(detail.contains("native zero-copy inputs: none"), "{detail}");
    }

    #[test]
    fn reports_windows_nvenc_runtime_dll_names_when_missing() {
        let capability = describe_capability(CapabilityPlatform::Windows, true, None);

        assert!(!capability.available);
        assert_eq!(
            capability.reason.as_deref(),
            Some("runtime_prerequisite_missing")
        );
        let detail = capability.detail.as_deref().unwrap_or_default();
        assert!(detail.contains("nvcuda.dll"), "{detail}");
        if cfg!(target_pointer_width = "32") {
            assert!(detail.contains("nvEncodeAPI.dll"), "{detail}");
        } else {
            assert!(detail.contains("nvEncodeAPI64.dll"), "{detail}");
        }
    }

    #[test]
    fn reports_windows_nvenc_publish_backend_not_compiled() {
        let capability = describe_capability(CapabilityPlatform::Windows, false, None);

        assert!(!capability.available);
        assert_eq!(capability.reason.as_deref(), Some("nvenc_not_compiled"));
        let detail = capability.detail.as_deref().unwrap_or_default();
        assert!(
            detail.contains("Windows NVENC publishing is not compiled"),
            "{detail}"
        );
        let error =
            require_publish_codec_runtime_support_for_capability("h265", &capability).unwrap_err();
        assert!(
            error.contains("Windows NVENC publishing is not compiled"),
            "{error}"
        );
    }

    #[test]
    fn publish_codec_runtime_gate_only_requires_hardware_for_h265() {
        let unavailable = describe_capability(CapabilityPlatform::Linux, false, None);

        assert!(require_publish_codec_runtime_support_for_capability("h264", &unavailable).is_ok());
        assert!(require_publish_codec_runtime_support_for_capability("vp8", &unavailable).is_ok());
        let error =
            require_publish_codec_runtime_support_for_capability("h265", &unavailable).unwrap_err();
        assert!(error.contains("H.265 publishing requires hardware encoder support"));
        assert!(error.contains("vendored CUDA Driver API header"));
    }

    #[test]
    fn publish_codec_runtime_gate_accepts_h265_when_reported_available() {
        let capability = describe_videotoolbox_capability(Some(videotoolbox_ready()));

        assert!(require_publish_codec_runtime_support_for_capability("h265", &capability).is_ok());
        assert!(require_publish_codec_runtime_support_for_capability("H265", &capability).is_ok());
    }

    #[test]
    fn reports_no_supported_codecs_when_guid_query_finds_no_fluxer_codecs() {
        let capability = describe_capability(
            CapabilityPlatform::Linux,
            true,
            Some(NvencRuntimeProbe {
                supports_h264: false,
                supports_h265: false,
                ..runtime_ready()
            }),
        );

        assert!(!capability.available);
        assert_eq!(capability.reason.as_deref(), Some("no_supported_codecs"));
        assert_eq!(capability.codecs, Vec::<String>::new());
        assert!(
            capability
                .detail
                .as_deref()
                .unwrap_or_default()
                .contains("NVENC H264/H265 encode GUID")
        );
    }

    #[test]
    fn reports_not_compiled_before_runtime_missing() {
        let capability = describe_capability(
            CapabilityPlatform::Linux,
            false,
            Some(NvencRuntimeProbe {
                libcuda: false,
                libnvidia_encode: false,
                get_max_supported_version: false,
                driver_version_compatible: false,
                create_instance: false,
                cuda_init: false,
                cuda_device_count: None,
                cuda_device: false,
                cuda_context: false,
                checked_cuda_devices: 0,
                supported_cuda_devices: 0,
                open_encode_session: false,
                query_encode_guids: false,
                supports_h264: false,
                supports_h265: false,
                last_open_encode_session_status: None,
            }),
        );

        assert!(!capability.available);
        assert_eq!(capability.backend, "none");
        assert!(!capability.compiled);
        assert!(!capability.runtime);
        assert_eq!(capability.codecs, Vec::<String>::new());
        assert!(!capability.zero_copy);
        assert_eq!(capability.native_inputs, Vec::<String>::new());
        assert_eq!(capability.reason.as_deref(), Some("nvenc_not_compiled"));
        assert!(
            capability
                .detail
                .as_deref()
                .unwrap_or_default()
                .contains("vendored CUDA Driver API header")
        );
    }

    #[test]
    fn reports_missing_runtime_prerequisites_when_compiled() {
        let capability = describe_capability(
            CapabilityPlatform::Linux,
            true,
            Some(NvencRuntimeProbe {
                libcuda: true,
                libnvidia_encode: true,
                get_max_supported_version: true,
                driver_version_compatible: true,
                create_instance: true,
                cuda_init: true,
                cuda_device_count: Some(1),
                cuda_device: true,
                cuda_context: true,
                checked_cuda_devices: 1,
                supported_cuda_devices: 0,
                open_encode_session: false,
                query_encode_guids: false,
                supports_h264: false,
                supports_h265: false,
                last_open_encode_session_status: Some(2),
            }),
        );

        assert!(!capability.available);
        assert_eq!(capability.backend, "none");
        assert!(capability.compiled);
        assert!(!capability.runtime);
        assert!(!capability.zero_copy);
        assert_eq!(capability.native_inputs, Vec::<String>::new());
        assert_eq!(capability.reason.as_deref(), Some("no_supported_devices"));
        assert!(
            capability
                .detail
                .as_deref()
                .unwrap_or_default()
                .contains("NVENC encode session")
        );
        assert!(
            capability
                .detail
                .as_deref()
                .unwrap_or_default()
                .contains("NV_ENC_ERR_UNSUPPORTED_DEVICE")
        );
    }

    #[test]
    fn reports_missing_runtime_prerequisites_when_probe_is_absent() {
        let capability = describe_capability(CapabilityPlatform::Linux, true, None);

        assert!(!capability.available);
        assert_eq!(capability.backend, "none");
        assert!(capability.compiled);
        assert!(!capability.runtime);
        assert!(!capability.zero_copy);
        assert_eq!(capability.native_inputs, Vec::<String>::new());
        assert_eq!(
            capability.reason.as_deref(),
            Some("runtime_prerequisite_missing")
        );
        let detail = capability.detail.as_deref().unwrap_or_default();
        assert!(detail.contains("libcuda.so.1"), "{detail}");
        assert!(detail.contains("libnvidia-encode.so.1"), "{detail}");
    }

    #[test]
    fn reports_driver_sdk_incompatibility_before_encode_session() {
        let capability = describe_capability(
            CapabilityPlatform::Linux,
            true,
            Some(NvencRuntimeProbe {
                driver_version_compatible: false,
                create_instance: false,
                cuda_init: false,
                cuda_device: false,
                cuda_context: false,
                open_encode_session: false,
                ..runtime_ready()
            }),
        );

        assert!(!capability.available);
        assert_eq!(capability.backend, "none");
        assert!(capability.compiled);
        assert!(!capability.runtime);
        assert!(!capability.zero_copy);
        assert_eq!(capability.native_inputs, Vec::<String>::new());
        assert_eq!(capability.reason.as_deref(), Some("outdated_driver"));
        let detail = capability.detail.as_deref().unwrap_or_default();
        assert!(
            detail.contains("NVENC driver support for the compiled SDK version"),
            "{detail}"
        );
        assert!(!detail.contains("NVENC encode session"), "{detail}");
    }

    #[test]
    fn reports_no_cuda_devices_using_obs_style_reason() {
        let capability = describe_capability(
            CapabilityPlatform::Linux,
            true,
            Some(NvencRuntimeProbe {
                cuda_device_count: Some(0),
                cuda_device: false,
                cuda_context: false,
                open_encode_session: false,
                ..runtime_ready()
            }),
        );

        assert!(!capability.available);
        assert_eq!(capability.reason.as_deref(), Some("no_devices"));
        assert!(
            capability
                .detail
                .as_deref()
                .unwrap_or_default()
                .contains("CUDA devices")
        );
    }

    #[test]
    fn reports_available_videotoolbox_with_h264_and_h265() {
        let capability = describe_videotoolbox_capability(Some(videotoolbox_ready()));

        assert!(capability.available);
        assert_eq!(capability.backend, "videotoolbox");
        assert!(capability.compiled);
        assert!(capability.runtime);
        assert_eq!(capability.codecs, vec!["h264", "h265"]);
        assert!(!capability.zero_copy);
        assert_eq!(capability.native_inputs, Vec::<String>::new());
        assert_eq!(capability.reason, None);
        assert!(
            capability
                .detail
                .as_deref()
                .unwrap_or_default()
                .contains("h264, h265")
        );
    }

    #[test]
    fn reports_videotoolbox_h264_only_when_hevc_session_is_unavailable() {
        let capability = describe_videotoolbox_capability(Some(VideoToolboxRuntimeProbe {
            h265_create_status: Some(-12909),
            h265_prepare_status: None,
            ..videotoolbox_ready()
        }));

        assert!(capability.available);
        assert_eq!(capability.backend, "videotoolbox");
        assert_eq!(capability.codecs, vec!["h264"]);
        assert_eq!(capability.reason, None);
    }

    #[test]
    fn reports_videotoolbox_unavailable_with_status_details() {
        let capability = describe_videotoolbox_capability(Some(VideoToolboxRuntimeProbe {
            h264_create_status: Some(-12909),
            h264_prepare_status: None,
            h265_create_status: Some(-12915),
            h265_prepare_status: None,
        }));

        assert!(!capability.available);
        assert_eq!(capability.backend, "none");
        assert!(!capability.runtime);
        assert_eq!(capability.codecs, Vec::<String>::new());
        assert_eq!(capability.reason.as_deref(), Some("no_supported_codecs"));
        let detail = capability.detail.as_deref().unwrap_or_default();
        assert!(
            detail.contains("kVTCouldNotFindVideoEncoderErr"),
            "{detail}"
        );
        assert!(
            detail.contains("kVTVideoEncoderNotAvailableNowErr"),
            "{detail}"
        );
    }

    #[test]
    fn reports_unsupported_platforms_clearly() {
        let capability = describe_capability(
            CapabilityPlatform::Unsupported("macos"),
            true,
            Some(runtime_ready()),
        );

        assert!(!capability.available);
        assert_eq!(capability.backend, "none");
        assert!(!capability.compiled);
        assert!(!capability.runtime);
        assert!(!capability.zero_copy);
        assert_eq!(capability.native_inputs, Vec::<String>::new());
        assert_eq!(capability.reason.as_deref(), Some("unsupported_platform"));
        assert!(
            capability
                .detail
                .as_deref()
                .unwrap_or_default()
                .contains("macos")
        );
    }
}
