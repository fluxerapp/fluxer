// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ffi::OsString;
use std::mem::transmute;
use std::os::windows::ffi::OsStrExt;
use std::sync::OnceLock;

use napi::Result;
use windows::Win32::Foundation::{FreeLibrary, HMODULE, HWND, LPARAM};
use windows::Win32::Networking::WindowsWebServices::{
    WEBAUTHN_ASSERTION, WEBAUTHN_AUTHENTICATOR_ATTACHMENT_CROSS_PLATFORM,
    WEBAUTHN_AUTHENTICATOR_ATTACHMENT_PLATFORM, WEBAUTHN_AUTHENTICATOR_GET_ASSERTION_OPTIONS,
    WEBAUTHN_AUTHENTICATOR_GET_ASSERTION_OPTIONS_CURRENT_VERSION,
    WEBAUTHN_AUTHENTICATOR_MAKE_CREDENTIAL_OPTIONS,
    WEBAUTHN_AUTHENTICATOR_MAKE_CREDENTIAL_OPTIONS_CURRENT_VERSION, WEBAUTHN_CLIENT_DATA,
    WEBAUTHN_CLIENT_DATA_CURRENT_VERSION, WEBAUTHN_COSE_CREDENTIAL_PARAMETER,
    WEBAUTHN_COSE_CREDENTIAL_PARAMETER_CURRENT_VERSION, WEBAUTHN_COSE_CREDENTIAL_PARAMETERS,
    WEBAUTHN_CREDENTIAL, WEBAUTHN_CREDENTIAL_ATTESTATION, WEBAUTHN_CREDENTIAL_CURRENT_VERSION,
    WEBAUTHN_CREDENTIALS, WEBAUTHN_CTAP_TRANSPORT_INTERNAL, WEBAUTHN_CTAP_TRANSPORT_USB,
    WEBAUTHN_HASH_ALGORITHM_SHA_256, WEBAUTHN_LARGE_BLOB_SUPPORT_NONE,
    WEBAUTHN_RP_ENTITY_INFORMATION, WEBAUTHN_RP_ENTITY_INFORMATION_CURRENT_VERSION,
    WEBAUTHN_USER_ENTITY_INFORMATION, WEBAUTHN_USER_ENTITY_INFORMATION_CURRENT_VERSION,
};
use windows::Win32::System::LibraryLoader::{
    GetProcAddress, LOAD_LIBRARY_SEARCH_SYSTEM32, LoadLibraryExW,
};
use windows::Win32::System::Threading::GetCurrentProcessId;
use windows::Win32::UI::WindowsAndMessaging::{
    EnumWindows, GetDesktopWindow, GetForegroundWindow, GetWindowThreadProcessId, IsWindow,
    IsWindowVisible,
};
use windows_core::{BOOL, HRESULT, PCSTR, PCWSTR, s, w};

use crate::common::{
    CREATE_PREFIX, CreateInput, CreateResult, DescriptorInput, GET_PREFIX, GetInput, GetResult,
    ceremony_error,
};

type RawExport = unsafe extern "system" fn() -> isize;
type GetApiVersionNumberFn = unsafe extern "system" fn() -> u32;
type IsUserVerifyingPlatformAuthenticatorAvailableFn =
    unsafe extern "system" fn(*mut BOOL) -> HRESULT;
type MakeCredentialFn = unsafe extern "system" fn(
    HWND,
    *const WEBAUTHN_RP_ENTITY_INFORMATION,
    *const WEBAUTHN_USER_ENTITY_INFORMATION,
    *const WEBAUTHN_COSE_CREDENTIAL_PARAMETERS,
    *const WEBAUTHN_CLIENT_DATA,
    *const WEBAUTHN_AUTHENTICATOR_MAKE_CREDENTIAL_OPTIONS,
    *mut *mut WEBAUTHN_CREDENTIAL_ATTESTATION,
) -> HRESULT;
type GetAssertionFn = unsafe extern "system" fn(
    HWND,
    PCWSTR,
    *const WEBAUTHN_CLIENT_DATA,
    *const WEBAUTHN_AUTHENTICATOR_GET_ASSERTION_OPTIONS,
    *mut *mut WEBAUTHN_ASSERTION,
) -> HRESULT;
type FreeCredentialAttestationFn =
    unsafe extern "system" fn(*const WEBAUTHN_CREDENTIAL_ATTESTATION);
type FreeAssertionFn = unsafe extern "system" fn(*const WEBAUTHN_ASSERTION);

struct WebAuthnApi {
    version: u32,
    is_user_verifying_platform_authenticator_available:
        IsUserVerifyingPlatformAuthenticatorAvailableFn,
    make_credential: MakeCredentialFn,
    get_assertion: GetAssertionFn,
    free_credential_attestation: FreeCredentialAttestationFn,
    free_assertion: FreeAssertionFn,
}

static WEBAUTHN_API: OnceLock<Option<WebAuthnApi>> = OnceLock::new();

fn webauthn_api() -> Option<&'static WebAuthnApi> {
    WEBAUTHN_API
        .get_or_init(|| load_webauthn_api(w!("webauthn.dll")))
        .as_ref()
}

fn load_webauthn_api(library: PCWSTR) -> Option<WebAuthnApi> {
    let module = unsafe { LoadLibraryExW(library, None, LOAD_LIBRARY_SEARCH_SYSTEM32) }.ok()?;
    let api = resolve_webauthn_api(module);
    if api.is_none() {
        let _ = unsafe { FreeLibrary(module) };
    }
    api
}

fn resolve_webauthn_api(module: HMODULE) -> Option<WebAuthnApi> {
    let get_api_version_number = unsafe {
        transmute::<RawExport, GetApiVersionNumberFn>(export(
            module,
            s!("WebAuthNGetApiVersionNumber"),
        )?)
    };
    let version = unsafe { get_api_version_number() };
    if version == 0 {
        return None;
    }
    unsafe {
        Some(WebAuthnApi {
            version,
            is_user_verifying_platform_authenticator_available: transmute::<
                RawExport,
                IsUserVerifyingPlatformAuthenticatorAvailableFn,
            >(export(
                module,
                s!("WebAuthNIsUserVerifyingPlatformAuthenticatorAvailable"),
            )?),
            make_credential: transmute::<RawExport, MakeCredentialFn>(export(
                module,
                s!("WebAuthNAuthenticatorMakeCredential"),
            )?),
            get_assertion: transmute::<RawExport, GetAssertionFn>(export(
                module,
                s!("WebAuthNAuthenticatorGetAssertion"),
            )?),
            free_credential_attestation: transmute::<RawExport, FreeCredentialAttestationFn>(
                export(module, s!("WebAuthNFreeCredentialAttestation"))?,
            ),
            free_assertion: transmute::<RawExport, FreeAssertionFn>(export(
                module,
                s!("WebAuthNFreeAssertion"),
            )?),
        })
    }
}

fn export(module: HMODULE, name: PCSTR) -> Option<RawExport> {
    unsafe { GetProcAddress(module, name) }
}

pub fn api_version() -> u32 {
    webauthn_api().map_or(0, |api| api.version)
}

pub fn is_user_verifying_platform_authenticator_available() -> bool {
    let Some(api) = webauthn_api() else {
        return false;
    };
    let mut available = BOOL(0);
    let result =
        unsafe { (api.is_user_verifying_platform_authenticator_available)(&mut available) };
    result.is_ok() && available.as_bool()
}

fn require_webauthn_api(prefix: &str) -> Result<&'static WebAuthnApi> {
    webauthn_api().ok_or_else(|| ceremony_error(prefix, "WebAuthnApiUnavailable"))
}

fn to_wide(value: &str) -> Vec<u16> {
    OsString::from(value).encode_wide().chain(Some(0)).collect()
}

fn window_from_handle(handle: u64) -> Option<HWND> {
    if handle == 0 {
        return None;
    }
    let hwnd = HWND(handle as usize as *mut core::ffi::c_void);
    if unsafe { IsWindow(Some(hwnd)) }.as_bool() {
        Some(hwnd)
    } else {
        None
    }
}

fn window_process_id(hwnd: HWND) -> u32 {
    let mut pid = 0u32;
    unsafe { GetWindowThreadProcessId(hwnd, Some(&mut pid)) };
    pid
}

unsafe extern "system" fn collect_own_visible_window(hwnd: HWND, lparam: LPARAM) -> BOOL {
    let slot = unsafe { &mut *(lparam.0 as *mut Option<HWND>) };
    if window_process_id(hwnd) == unsafe { GetCurrentProcessId() }
        && unsafe { IsWindowVisible(hwnd) }.as_bool()
    {
        *slot = Some(hwnd);
        return BOOL(0);
    }
    BOOL(1)
}

fn interaction_window(handle: u64) -> HWND {
    if let Some(hwnd) = window_from_handle(handle) {
        return hwnd;
    }
    let current_pid = unsafe { GetCurrentProcessId() };
    let foreground = unsafe { GetForegroundWindow() };
    if !foreground.is_invalid() && window_process_id(foreground) == current_pid {
        return foreground;
    }
    let mut own_window: Option<HWND> = None;
    let _ = unsafe {
        EnumWindows(
            Some(collect_own_visible_window),
            LPARAM(&raw mut own_window as isize),
        )
    };
    if let Some(hwnd) = own_window {
        return hwnd;
    }
    if !foreground.is_invalid() {
        return foreground;
    }
    unsafe { GetDesktopWindow() }
}

const PUBLIC_KEY_WIDE: &[u16] = &[
    'p' as u16, 'u' as u16, 'b' as u16, 'l' as u16, 'i' as u16, 'c' as u16, '-' as u16, 'k' as u16,
    'e' as u16, 'y' as u16, 0,
];

fn build_credential_list(
    descriptors: &[DescriptorInput],
) -> (Vec<WEBAUTHN_CREDENTIAL>, WEBAUTHN_CREDENTIALS) {
    let mut backing: Vec<WEBAUTHN_CREDENTIAL> = descriptors
        .iter()
        .map(|d| WEBAUTHN_CREDENTIAL {
            dwVersion: WEBAUTHN_CREDENTIAL_CURRENT_VERSION,
            cbId: d.id.len() as u32,
            pbId: d.id.as_ptr() as *mut u8,
            pwszCredentialType: PCWSTR(PUBLIC_KEY_WIDE.as_ptr()),
        })
        .collect();
    let list = WEBAUTHN_CREDENTIALS {
        cCredentials: backing.len() as u32,
        pCredentials: if backing.is_empty() {
            std::ptr::null_mut()
        } else {
            backing.as_mut_ptr()
        },
    };
    (backing, list)
}

fn copy_buffer(ptr: *const u8, len: u32) -> Result<Vec<u8>> {
    if len == 0 {
        return Ok(Vec::new());
    }
    if ptr.is_null() {
        return Err(ceremony_error(CREATE_PREFIX, "InvalidWebAuthnResult"));
    }

    let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
    Ok(slice.to_vec())
}

pub fn make_credential(input: &mut CreateInput) -> Result<CreateResult> {
    let api = require_webauthn_api(CREATE_PREFIX)?;
    let rp_id_w = to_wide(&input.rp_id);
    let rp_name_w = to_wide(&input.rp_name);
    let user_name_w = to_wide(&input.user_name);
    let display_w = to_wide(&input.user_display_name);

    let rp = WEBAUTHN_RP_ENTITY_INFORMATION {
        dwVersion: WEBAUTHN_RP_ENTITY_INFORMATION_CURRENT_VERSION,
        pwszId: PCWSTR(rp_id_w.as_ptr()),
        pwszName: PCWSTR(rp_name_w.as_ptr()),
        pwszIcon: PCWSTR::null(),
    };
    let user = WEBAUTHN_USER_ENTITY_INFORMATION {
        dwVersion: WEBAUTHN_USER_ENTITY_INFORMATION_CURRENT_VERSION,
        cbId: input.user_id.len() as u32,
        pbId: input.user_id.as_ptr() as *mut u8,
        pwszName: PCWSTR(user_name_w.as_ptr()),
        pwszIcon: PCWSTR::null(),
        pwszDisplayName: PCWSTR(display_w.as_ptr()),
    };

    let mut pub_key_params: Vec<WEBAUTHN_COSE_CREDENTIAL_PARAMETER> = input
        .pub_key_algs
        .iter()
        .map(|&alg| WEBAUTHN_COSE_CREDENTIAL_PARAMETER {
            dwVersion: WEBAUTHN_COSE_CREDENTIAL_PARAMETER_CURRENT_VERSION,
            pwszCredentialType: PCWSTR(PUBLIC_KEY_WIDE.as_ptr()),
            lAlg: alg,
        })
        .collect();
    let params = WEBAUTHN_COSE_CREDENTIAL_PARAMETERS {
        cCredentialParameters: pub_key_params.len() as u32,
        pCredentialParameters: pub_key_params.as_mut_ptr(),
    };

    let client_data = WEBAUTHN_CLIENT_DATA {
        dwVersion: WEBAUTHN_CLIENT_DATA_CURRENT_VERSION,
        cbClientDataJSON: input.client_data_json.len() as u32,
        pbClientDataJSON: input.client_data_json.as_ptr() as *mut u8,
        pwszHashAlgId: WEBAUTHN_HASH_ALGORITHM_SHA_256,
    };

    let (_exclude_keep, exclude_list) = build_credential_list(&input.exclude_credentials);

    let options = WEBAUTHN_AUTHENTICATOR_MAKE_CREDENTIAL_OPTIONS {
        dwVersion: api
            .version
            .min(WEBAUTHN_AUTHENTICATOR_MAKE_CREDENTIAL_OPTIONS_CURRENT_VERSION),
        dwTimeoutMilliseconds: input.timeout_ms,
        CredentialList: exclude_list,
        dwAuthenticatorAttachment: input.authenticator_attachment,
        bRequireResidentKey: BOOL(input.require_resident_key as i32),
        dwUserVerificationRequirement: input.user_verification,
        dwAttestationConveyancePreference: input.attestation,
        dwEnterpriseAttestation: input.enterprise_attestation,
        dwLargeBlobSupport: WEBAUTHN_LARGE_BLOB_SUPPORT_NONE,
        bPreferResidentKey: BOOL(input.prefer_resident_key as i32),
        ..Default::default()
    };

    let mut attestation_ptr: *mut WEBAUTHN_CREDENTIAL_ATTESTATION = std::ptr::null_mut();
    unsafe {
        (api.make_credential)(
            interaction_window(input.window_handle),
            &rp,
            &user,
            &params,
            &client_data,
            &options,
            &mut attestation_ptr,
        )
    }
    .ok()
    .map_err(|err| ceremony_error(CREATE_PREFIX, &format!("WebAuthnCallFailed: {err}")))?;
    if attestation_ptr.is_null() {
        return Err(ceremony_error(CREATE_PREFIX, "InvalidWebAuthnResult"));
    }

    let att = unsafe { &*attestation_ptr };
    let raw_id = copy_buffer(att.pbCredentialId, att.cbCredentialId)?;
    let attestation_object = copy_buffer(att.pbAttestationObject, att.cbAttestationObject)?;
    let used_transport = if att.dwVersion >= 3 {
        att.dwUsedTransport
    } else {
        0
    };

    unsafe { (api.free_credential_attestation)(attestation_ptr) };

    Ok(CreateResult {
        raw_id,
        attestation_object,
        client_data_json: input.client_data_json.clone(),
        used_transport,
    })
}

pub fn get_assertion(input: &mut GetInput) -> Result<GetResult> {
    let api = require_webauthn_api(GET_PREFIX)?;
    let rp_id_w = to_wide(&input.rp_id);

    let client_data = WEBAUTHN_CLIENT_DATA {
        dwVersion: WEBAUTHN_CLIENT_DATA_CURRENT_VERSION,
        cbClientDataJSON: input.client_data_json.len() as u32,
        pbClientDataJSON: input.client_data_json.as_ptr() as *mut u8,
        pwszHashAlgId: WEBAUTHN_HASH_ALGORITHM_SHA_256,
    };

    let (_allow_keep, allow_list) = build_credential_list(&input.allow_credentials);

    let options = WEBAUTHN_AUTHENTICATOR_GET_ASSERTION_OPTIONS {
        dwVersion: api
            .version
            .min(WEBAUTHN_AUTHENTICATOR_GET_ASSERTION_OPTIONS_CURRENT_VERSION),
        dwTimeoutMilliseconds: input.timeout_ms,
        CredentialList: allow_list,
        dwAuthenticatorAttachment: input.authenticator_attachment,
        dwUserVerificationRequirement: input.user_verification,
        ..Default::default()
    };

    let mut assertion_ptr: *mut WEBAUTHN_ASSERTION = std::ptr::null_mut();
    unsafe {
        (api.get_assertion)(
            interaction_window(input.window_handle),
            PCWSTR(rp_id_w.as_ptr()),
            &client_data,
            &options,
            &mut assertion_ptr,
        )
    }
    .ok()
    .map_err(|err| ceremony_error(GET_PREFIX, &format!("WebAuthnCallFailed: {err}")))?;
    if assertion_ptr.is_null() {
        return Err(ceremony_error(GET_PREFIX, "InvalidWebAuthnResult"));
    }

    let asrt = unsafe { &*assertion_ptr };
    let raw_id = copy_buffer(asrt.Credential.pbId, asrt.Credential.cbId)?;
    let authenticator_data = copy_buffer(asrt.pbAuthenticatorData, asrt.cbAuthenticatorData)?;
    let signature = copy_buffer(asrt.pbSignature, asrt.cbSignature)?;
    let user_handle = if asrt.cbUserId > 0 {
        Some(copy_buffer(asrt.pbUserId, asrt.cbUserId)?)
    } else {
        None
    };

    let used_transport = if asrt.dwVersion >= 4 {
        asrt.dwUsedTransport
    } else if input.authenticator_attachment == WEBAUTHN_AUTHENTICATOR_ATTACHMENT_PLATFORM {
        WEBAUTHN_CTAP_TRANSPORT_INTERNAL
    } else if input.authenticator_attachment == WEBAUTHN_AUTHENTICATOR_ATTACHMENT_CROSS_PLATFORM {
        WEBAUTHN_CTAP_TRANSPORT_USB
    } else {
        0
    };

    unsafe { (api.free_assertion)(assertion_ptr) };

    Ok(GetResult {
        raw_id,
        authenticator_data,
        signature,
        user_handle,
        client_data_json: input.client_data_json.clone(),
        used_transport,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn library_without_webauthn_exports_reports_no_api() {
        assert!(load_webauthn_api(w!("kernel32.dll")).is_none());
    }

    #[test]
    fn missing_library_reports_no_api() {
        assert!(load_webauthn_api(w!("fluxer-missing-webauthn.dll")).is_none());
    }
}
