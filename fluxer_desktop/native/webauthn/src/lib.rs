#![allow(clippy::too_many_lines)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::Task;
use napi::bindgen_prelude::{
    AsyncTask, Buffer, Env, Error, JsObjectValue, Object, Result, Status, Unknown,
};
use napi::{JsValue, ValueType};
use napi_derive::napi;

mod common;
use common::{
    CreateInput, CreateResult, DescriptorInput, GetInput, GetResult, attachment_from_transport,
};

#[cfg(target_os = "linux")]
mod linux;
#[cfg(target_os = "macos")]
mod macos;
#[cfg(target_os = "windows")]
mod windows;

#[cfg(target_os = "macos")]
const BACKEND_NAME: &str = "macos-authenticationservices";
#[cfg(target_os = "windows")]
const BACKEND_NAME: &str = "windows-webauthn";
#[cfg(target_os = "linux")]
const BACKEND_NAME: &str = "linux-libfido2";
#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "linux")))]
const BACKEND_NAME: &str = "unavailable";

#[cfg(target_os = "macos")]
const BACKEND_REASON: &str = "macOS WebAuthn backend (AuthenticationServices). \
     Requires macOS 12.0+ (ASAuthorizationPlatformPublicKeyCredentialProvider, \
     introduced in Monterey). On older macOS the bundled \
     ASAuthorizationPlatform/SecurityKey provider classes are not present and \
     `isSupported()` returns false.";
#[cfg(target_os = "windows")]
const BACKEND_REASON: &str = "Windows WebAuthn broker backend (webauthn.dll). \
     Requires Windows 10 build 18362 (May 2019 / version 1903) or newer; \
     webauthn.dll is delay-loaded via /DELAYLOAD, so the addon still loads on \
     older builds but `isSupported()` returns false there.";
#[cfg(target_os = "linux")]
const BACKEND_REASON: &str = "Linux WebAuthn backend (bundled libfido2, HID transport, U2F \
     fallback disabled). Requires a CTAP authenticator on USB-HID; \
     `isSupported()` returns false when no authenticator is enumerated.";
#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "linux")))]
const BACKEND_REASON: &str = "no WebAuthn backend is compiled in for this platform";

const TARGET_PLATFORM: &str = if cfg!(target_os = "macos") {
    "darwin"
} else if cfg!(target_os = "windows") {
    "win32"
} else if cfg!(target_os = "linux") {
    "linux"
} else {
    "unknown"
};

const TARGET_ARCH: &str = if cfg!(target_arch = "x86_64") {
    "x64"
} else if cfg!(target_arch = "aarch64") {
    "arm64"
} else {
    "unknown"
};

fn target_string() -> String {
    format!("{TARGET_PLATFORM}/{TARGET_ARCH}")
}

fn platform_broker_available() -> bool {
    #[cfg(target_os = "windows")]
    {
        windows::api_version() > 0
    }
    #[cfg(target_os = "linux")]
    {
        linux::is_supported()
    }
    #[cfg(target_os = "macos")]
    {
        macos::is_supported()
    }
    #[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
    {
        false
    }
}

fn platform_authenticator_available() -> bool {
    #[cfg(target_os = "windows")]
    {
        windows::is_user_verifying_platform_authenticator_available()
    }
    #[cfg(not(target_os = "windows"))]
    {
        false
    }
}

fn ceremonies_implemented() -> bool {
    cfg!(any(
        target_os = "macos",
        target_os = "windows",
        target_os = "linux"
    ))
}

fn is_supported_native() -> bool {
    ceremonies_implemented() && platform_broker_available()
}

fn api_version() -> u32 {
    #[cfg(target_os = "windows")]
    {
        windows::api_version()
    }
    #[cfg(not(target_os = "windows"))]
    {
        0
    }
}

fn backend_reason() -> String {
    #[cfg(target_os = "linux")]
    {
        format!("{} {}", BACKEND_REASON, linux::support_diagnostics())
    }
    #[cfg(not(target_os = "linux"))]
    {
        BACKEND_REASON.to_owned()
    }
}

#[napi(object, js_name = "WebAuthnBackendInfo")]
pub struct WebAuthnBackendInfoJs {
    pub target: String,
    pub backend: String,
    #[napi(js_name = "nativeLoaded")]
    pub native_loaded: bool,
    #[napi(js_name = "ceremoniesImplemented")]
    pub ceremonies_implemented: bool,
    #[napi(js_name = "platformBrokerAvailable")]
    pub platform_broker_available: bool,
    #[napi(js_name = "platformAuthenticatorAvailable")]
    pub platform_authenticator_available: bool,
    pub supported: bool,
    #[napi(js_name = "apiVersion")]
    pub api_version: u32,
    pub reason: String,
}

#[napi(js_name = "getBackendInfo")]
pub fn get_backend_info() -> WebAuthnBackendInfoJs {
    WebAuthnBackendInfoJs {
        target: target_string(),
        backend: BACKEND_NAME.to_owned(),
        native_loaded: true,
        ceremonies_implemented: ceremonies_implemented(),
        platform_broker_available: platform_broker_available(),
        platform_authenticator_available: platform_authenticator_available(),
        supported: is_supported_native(),
        api_version: api_version(),
        reason: backend_reason(),
    }
}

pub struct IsSupportedTask;
impl Task for IsSupportedTask {
    type Output = bool;
    type JsValue = bool;
    fn compute(&mut self) -> Result<Self::Output> {
        Ok(is_supported_native())
    }
    fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
        Ok(output)
    }
}

#[napi(js_name = "isSupported")]
pub fn is_supported() -> AsyncTask<IsSupportedTask> {
    AsyncTask::new(IsSupportedTask)
}

#[napi(object)]
pub struct CreateCeremonyResultJs {
    #[napi(js_name = "rawId")]
    pub raw_id: Buffer,
    #[napi(js_name = "attestationObject")]
    pub attestation_object: Buffer,
    #[napi(js_name = "clientDataJSON")]
    pub client_data_json: Buffer,

    #[napi(js_name = "authenticatorAttachment")]
    pub authenticator_attachment: Option<String>,
}

#[napi(object)]
pub struct GetCeremonyResultJs {
    #[napi(js_name = "rawId")]
    pub raw_id: Buffer,
    #[napi(js_name = "authenticatorData")]
    pub authenticator_data: Buffer,
    pub signature: Buffer,

    #[napi(js_name = "userHandle")]
    pub user_handle: Option<Buffer>,
    #[napi(js_name = "clientDataJSON")]
    pub client_data_json: Buffer,
    #[napi(js_name = "authenticatorAttachment")]
    pub authenticator_attachment: Option<String>,
}

pub struct CreateTask {
    input: CreateInput,
}

impl Task for CreateTask {
    type Output = CreateResult;
    type JsValue = CreateCeremonyResultJs;

    fn compute(&mut self) -> Result<Self::Output> {
        run_create(&mut self.input)
    }

    fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
        Ok(CreateCeremonyResultJs {
            raw_id: Buffer::from(output.raw_id),
            attestation_object: Buffer::from(output.attestation_object),
            client_data_json: Buffer::from(output.client_data_json),
            authenticator_attachment: attachment_from_transport(output.used_transport)
                .map(str::to_owned),
        })
    }
}

pub struct GetTask {
    input: GetInput,
}

impl Task for GetTask {
    type Output = GetResult;
    type JsValue = GetCeremonyResultJs;

    fn compute(&mut self) -> Result<Self::Output> {
        run_get(&mut self.input)
    }

    fn resolve(&mut self, _env: Env, output: Self::Output) -> Result<Self::JsValue> {
        Ok(GetCeremonyResultJs {
            raw_id: Buffer::from(output.raw_id),
            authenticator_data: Buffer::from(output.authenticator_data),
            signature: Buffer::from(output.signature),
            user_handle: output.user_handle.map(Buffer::from),
            client_data_json: Buffer::from(output.client_data_json),
            authenticator_attachment: attachment_from_transport(output.used_transport)
                .map(str::to_owned),
        })
    }
}

fn run_create(input: &mut CreateInput) -> Result<CreateResult> {
    #[cfg(target_os = "windows")]
    {
        windows::make_credential(input)
    }
    #[cfg(target_os = "linux")]
    {
        linux::make_credential(input)
    }
    #[cfg(target_os = "macos")]
    {
        macos::make_credential(input)
    }
    #[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
    {
        let _ = input;
        Err(common::ceremony_error(
            common::CREATE_PREFIX,
            "UnknownPlatform",
        ))
    }
}

fn run_get(input: &mut GetInput) -> Result<GetResult> {
    #[cfg(target_os = "windows")]
    {
        windows::get_assertion(input)
    }
    #[cfg(target_os = "linux")]
    {
        linux::get_assertion(input)
    }
    #[cfg(target_os = "macos")]
    {
        macos::get_assertion(input)
    }
    #[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
    {
        let _ = input;
        Err(common::ceremony_error(
            common::GET_PREFIX,
            "UnknownPlatform",
        ))
    }
}

#[napi(js_name = "create")]
pub fn create(options: Object) -> Result<AsyncTask<CreateTask>> {
    let input = parse_create_input(&options)?;
    Ok(AsyncTask::new(CreateTask { input }))
}

#[napi(js_name = "get")]
pub fn get(options: Object) -> Result<AsyncTask<GetTask>> {
    let input = parse_get_input(&options)?;
    Ok(AsyncTask::new(GetTask { input }))
}

fn invalid_create_options() -> Error {
    Error::new(
        Status::InvalidArg,
        "invalid WebAuthn registration options".to_owned(),
    )
}

fn invalid_get_options() -> Error {
    Error::new(
        Status::InvalidArg,
        "invalid WebAuthn assertion options".to_owned(),
    )
}

fn parse_create_input(object: &Object) -> Result<CreateInput> {
    let map_err = |_e: Error| invalid_create_options();
    Ok(CreateInput {
        rp_id: required_string(object, "rpId").map_err(map_err)?,
        rp_name: required_string(object, "rpName").map_err(map_err)?,
        challenge: required_buffer(object, "challenge").map_err(map_err)?,
        user_id: required_buffer(object, "userId").map_err(map_err)?,
        user_name: required_string(object, "userName").map_err(map_err)?,
        user_display_name: required_string(object, "userDisplayName").map_err(map_err)?,
        client_data_json: required_buffer(object, "clientDataJSON").map_err(map_err)?,
        client_data_hash: required_buffer(object, "clientDataHash").map_err(map_err)?,
        pub_key_algs: required_alg_array(object).map_err(map_err)?,
        exclude_credentials: optional_descriptor_array(object, "excludeCredentials")
            .map_err(map_err)?,
        timeout_ms: optional_u32(object, "timeout", 0).map_err(map_err)?,
        authenticator_attachment: optional_u32(
            object,
            "authenticatorAttachment",
            default_attachment(),
        )
        .map_err(map_err)?,
        user_verification: optional_u32(object, "userVerification", default_user_verification())
            .map_err(map_err)?,
        attestation: optional_u32(object, "attestation", default_attestation()).map_err(map_err)?,
        enterprise_attestation: optional_u32(
            object,
            "enterpriseAttestation",
            common::ENTERPRISE_NONE,
        )
        .map_err(map_err)?,
        require_resident_key: optional_bool(object, "requireResidentKey", false)
            .map_err(map_err)?,
        prefer_resident_key: optional_bool(object, "preferResidentKey", false).map_err(map_err)?,
        window_handle: optional_window_handle(object).map_err(map_err)?,
        pin: optional_string(object, "pin").map_err(map_err)?,
    })
}

fn parse_get_input(object: &Object) -> Result<GetInput> {
    let map_err = |_e: Error| invalid_get_options();
    Ok(GetInput {
        rp_id: required_string(object, "rpId").map_err(map_err)?,
        challenge: required_buffer(object, "challenge").map_err(map_err)?,
        client_data_json: required_buffer(object, "clientDataJSON").map_err(map_err)?,
        client_data_hash: required_buffer(object, "clientDataHash").map_err(map_err)?,
        allow_credentials: optional_descriptor_array(object, "allowCredentials")
            .map_err(map_err)?,
        timeout_ms: optional_u32(object, "timeout", 0).map_err(map_err)?,
        authenticator_attachment: optional_u32(
            object,
            "authenticatorAttachment",
            default_attachment(),
        )
        .map_err(map_err)?,
        user_verification: optional_u32(object, "userVerification", default_user_verification())
            .map_err(map_err)?,
        window_handle: optional_window_handle(object).map_err(map_err)?,
        pin: optional_string(object, "pin").map_err(map_err)?,
    })
}

fn optional_string(object: &Object, key: &str) -> Result<Option<String>> {
    let Some(value) = read_unknown(object, key) else {
        return Ok(None);
    };
    if is_nullish(&value) {
        return Ok(None);
    }
    if value.get_type()? != ValueType::String {
        return Err(Error::new(
            Status::InvalidArg,
            format!("{key} must be a string"),
        ));
    }
    let s: String = unsafe { value.cast::<String>()? };
    if s.is_empty() { Ok(None) } else { Ok(Some(s)) }
}

fn optional_window_handle(object: &Object) -> Result<u64> {
    let Some(value) = read_unknown(object, "windowHandle") else {
        return Ok(0);
    };
    if is_nullish(&value) {
        return Ok(0);
    }
    let bytes = read_buffer_bytes(&value, "windowHandle")?;
    if bytes.len() > 8 {
        return Err(Error::new(
            Status::InvalidArg,
            "windowHandle must be at most 8 bytes".to_owned(),
        ));
    }
    let mut padded = [0u8; 8];
    padded[..bytes.len()].copy_from_slice(&bytes);
    Ok(u64::from_le_bytes(padded))
}

#[cfg(target_os = "windows")]
fn default_attachment() -> u32 {
    common::ATTACHMENT_ANY
}
#[cfg(target_os = "windows")]
fn default_user_verification() -> u32 {
    common::USER_VERIFICATION_PREFERRED
}
#[cfg(target_os = "windows")]
fn default_attestation() -> u32 {
    common::ATTESTATION_NONE
}
#[cfg(not(target_os = "windows"))]
fn default_attachment() -> u32 {
    0
}
#[cfg(not(target_os = "windows"))]
fn default_user_verification() -> u32 {
    0
}
#[cfg(not(target_os = "windows"))]
fn default_attestation() -> u32 {
    0
}

fn read_unknown<'a>(object: &Object<'a>, key: &str) -> Option<Unknown<'a>> {
    object.get::<Unknown>(key).ok().flatten()
}

fn is_nullish(v: &Unknown<'_>) -> bool {
    match v.get_type() {
        Ok(t) => t == ValueType::Null || t == ValueType::Undefined,
        Err(_) => true,
    }
}

fn required_string(object: &Object, key: &str) -> Result<String> {
    let value = read_unknown(object, key)
        .ok_or_else(|| Error::new(Status::InvalidArg, format!("missing {key}")))?;
    if is_nullish(&value) {
        return Err(Error::new(Status::InvalidArg, format!("missing {key}")));
    }
    if value.get_type()? != ValueType::String {
        return Err(Error::new(
            Status::InvalidArg,
            format!("{key} must be a string"),
        ));
    }
    let s: String = unsafe { value.cast::<String>()? };
    Ok(s)
}

fn optional_u32(object: &Object, key: &str, default: u32) -> Result<u32> {
    let Some(value) = read_unknown(object, key) else {
        return Ok(default);
    };
    if is_nullish(&value) {
        return Ok(default);
    }
    if value.get_type()? != ValueType::Number {
        return Err(Error::new(
            Status::InvalidArg,
            format!("{key} must be a number"),
        ));
    }
    let n: u32 = unsafe { value.cast::<u32>()? };
    Ok(n)
}

fn optional_bool(object: &Object, key: &str, default: bool) -> Result<bool> {
    let Some(value) = read_unknown(object, key) else {
        return Ok(default);
    };
    if is_nullish(&value) {
        return Ok(default);
    }
    if value.get_type()? != ValueType::Boolean {
        return Err(Error::new(
            Status::InvalidArg,
            format!("{key} must be a boolean"),
        ));
    }
    let b: bool = unsafe { value.cast::<bool>()? };
    Ok(b)
}

fn required_buffer(object: &Object, key: &str) -> Result<Vec<u8>> {
    let value = read_unknown(object, key)
        .ok_or_else(|| Error::new(Status::InvalidArg, format!("missing {key}")))?;
    if is_nullish(&value) {
        return Err(Error::new(Status::InvalidArg, format!("missing {key}")));
    }
    read_buffer_bytes(&value, key)
}

fn read_buffer_bytes(value: &Unknown<'_>, key: &str) -> Result<Vec<u8>> {
    let raw = value.raw();
    let raw_env = value.value().env;
    let mut is_buffer = false;
    let status = unsafe { napi::sys::napi_is_buffer(raw_env, raw, &mut is_buffer) };
    if status != napi::sys::Status::napi_ok || !is_buffer {
        return Err(Error::new(
            Status::InvalidArg,
            format!("{key} must be a Buffer"),
        ));
    }
    let mut data_ptr: *mut std::ffi::c_void = std::ptr::null_mut();
    let mut len: usize = 0;
    let status = unsafe { napi::sys::napi_get_buffer_info(raw_env, raw, &mut data_ptr, &mut len) };
    if status != napi::sys::Status::napi_ok {
        return Err(Error::new(
            Status::InvalidArg,
            format!("{key} buffer read failed"),
        ));
    }
    if len == 0 {
        return Ok(Vec::new());
    }
    if data_ptr.is_null() {
        return Err(Error::new(
            Status::InvalidArg,
            format!("{key} buffer pointer was null"),
        ));
    }

    let slice = unsafe { std::slice::from_raw_parts(data_ptr.cast::<u8>(), len) };
    Ok(slice.to_vec())
}

fn required_alg_array(object: &Object) -> Result<Vec<i32>> {
    let Some(value) = read_unknown(object, "pubKeyCredParams") else {
        return Err(Error::new(
            Status::InvalidArg,
            "missing pubKeyCredParams".to_owned(),
        ));
    };
    if is_nullish(&value) {
        return Err(Error::new(
            Status::InvalidArg,
            "missing pubKeyCredParams".to_owned(),
        ));
    }

    let raw = value.raw();
    let raw_env = value.value().env;
    let mut is_array = false;
    let status = unsafe { napi::sys::napi_is_array(raw_env, raw, &mut is_array) };
    if status != napi::sys::Status::napi_ok || !is_array {
        return Err(Error::new(
            Status::InvalidArg,
            "pubKeyCredParams must be an array".to_owned(),
        ));
    }
    let arr: Object = unsafe { value.cast::<Object>()? };
    let len = read_array_length(&arr)?;
    if len == 0 {
        return Err(Error::new(
            Status::InvalidArg,
            "pubKeyCredParams must not be empty".to_owned(),
        ));
    }
    let mut algs = Vec::with_capacity(len as usize);
    for i in 0..len {
        let mut elem_raw: napi::sys::napi_value = std::ptr::null_mut();
        let status = unsafe { napi::sys::napi_get_element(raw_env, raw, i, &mut elem_raw) };
        if status != napi::sys::Status::napi_ok {
            return Err(Error::new(
                Status::InvalidArg,
                "pubKeyCredParams element read failed".to_owned(),
            ));
        }

        let elem_value: Unknown = unsafe { Unknown::from_raw_unchecked(raw_env, elem_raw) };
        if elem_value.get_type()? != ValueType::Object {
            return Err(Error::new(
                Status::InvalidArg,
                "pubKeyCredParams[] must be objects".to_owned(),
            ));
        }
        let elem: Object = unsafe { elem_value.cast::<Object>()? };
        algs.push(optional_i32_field(&elem, "alg", 0)?);
    }
    Ok(algs)
}

fn optional_i32_field(object: &Object, key: &str, default: i32) -> Result<i32> {
    let Some(value) = read_unknown(object, key) else {
        return Ok(default);
    };
    if is_nullish(&value) {
        return Ok(default);
    }
    if value.get_type()? != ValueType::Number {
        return Err(Error::new(
            Status::InvalidArg,
            format!("{key} must be a number"),
        ));
    }
    let n: i32 = unsafe { value.cast::<i32>()? };
    Ok(n)
}

fn optional_descriptor_array(object: &Object, key: &str) -> Result<Vec<DescriptorInput>> {
    let Some(value) = read_unknown(object, key) else {
        return Ok(Vec::new());
    };
    if is_nullish(&value) {
        return Ok(Vec::new());
    }
    let raw = value.raw();
    let raw_env = value.value().env;
    let mut is_array = false;
    let status = unsafe { napi::sys::napi_is_array(raw_env, raw, &mut is_array) };
    if status != napi::sys::Status::napi_ok || !is_array {
        return Err(Error::new(
            Status::InvalidArg,
            format!("{key} must be an array"),
        ));
    }
    let arr: Object = unsafe { value.cast::<Object>()? };
    let len = read_array_length(&arr)?;
    let mut out = Vec::with_capacity(len as usize);
    for i in 0..len {
        let mut elem_raw: napi::sys::napi_value = std::ptr::null_mut();
        let status = unsafe { napi::sys::napi_get_element(raw_env, raw, i, &mut elem_raw) };
        if status != napi::sys::Status::napi_ok {
            return Err(Error::new(
                Status::InvalidArg,
                format!("{key}[] read failed"),
            ));
        }
        let elem_value: Unknown = unsafe { Unknown::from_raw_unchecked(raw_env, elem_raw) };
        if elem_value.get_type()? != ValueType::Object {
            return Err(Error::new(
                Status::InvalidArg,
                format!("{key}[] must be objects"),
            ));
        }
        let elem: Object = unsafe { elem_value.cast::<Object>()? };
        let id = required_buffer(&elem, "id")?;
        let transports = optional_u32(&elem, "transports", 0)?;
        out.push(DescriptorInput { id, transports });
    }
    Ok(out)
}

fn read_array_length(obj: &Object) -> Result<u32> {
    let length: u32 = obj.get_named_property::<u32>("length")?;
    Ok(length)
}
