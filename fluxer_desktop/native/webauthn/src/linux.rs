#![allow(clippy::manual_c_str_literals)]

// SPDX-License-Identifier: AGPL-3.0-or-later

use std::ffi::{CStr, CString, c_char, c_int};
use std::ptr;

use libfido2_sys as f;
use napi::Result;

use crate::common::{
    CREATE_PREFIX, CreateInput, CreateResult, GET_PREFIX, GetInput, GetResult, TRANSPORT_USB,
    USER_VERIFICATION_REQUIRED, build_attestation_object, ceremony_error,
};

const MANIFEST_COUNT: usize = 64;
const SUPPORT_DIAGNOSTIC_DEVICE_LIMIT: usize = 8;

fn init() {
    unsafe { f::fido_init(f::FIDO_DISABLE_U2F_FALLBACK as c_int) };
}

pub fn is_supported() -> bool {
    init();

    unsafe {
        let infos = f::fido_dev_info_new(MANIFEST_COUNT);
        if infos.is_null() {
            return false;
        }
        let mut found: usize = 0;
        let rc = f::fido_dev_info_manifest(infos, MANIFEST_COUNT, &mut found);
        let mut local = infos;
        f::fido_dev_info_free(&mut local, MANIFEST_COUNT);
        rc == f::FIDO_OK as c_int
    }
}

fn c_string(ptr: *const c_char) -> Option<String> {
    if ptr.is_null() {
        return None;
    }
    let value = unsafe { CStr::from_ptr(ptr) }
        .to_string_lossy()
        .into_owned();
    if value.is_empty() { None } else { Some(value) }
}

fn fido_error_message(code: c_int) -> String {
    c_string(unsafe { f::fido_strerr(code) }).unwrap_or_else(|| "unknown libfido2 error".to_owned())
}

fn fido_call_detail(operation: &str, code: c_int) -> String {
    format!("{operation} rc={code} error={}", fido_error_message(code))
}

fn describe_device_info(info: *const f::fido_dev_info_t) -> String {
    if info.is_null() {
        return "deviceInfo=null".to_owned();
    }
    let path =
        c_string(unsafe { f::fido_dev_info_path(info) }).unwrap_or_else(|| "<unknown>".to_owned());
    let manufacturer = c_string(unsafe { f::fido_dev_info_manufacturer_string(info) })
        .unwrap_or_else(|| "<unknown>".to_owned());
    let product_name = c_string(unsafe { f::fido_dev_info_product_string(info) })
        .unwrap_or_else(|| "<unknown>".to_owned());
    let vendor_id = unsafe { f::fido_dev_info_vendor(info) } as u16;
    let product_id = unsafe { f::fido_dev_info_product(info) } as u16;
    format!(
        "path={path} vendorId=0x{vendor_id:04x} productId=0x{product_id:04x} manufacturer={manufacturer:?} product={product_name:?}"
    )
}

pub fn support_diagnostics() -> String {
    init();
    unsafe {
        let infos = f::fido_dev_info_new(MANIFEST_COUNT);
        if infos.is_null() {
            return "Probe: fido_dev_info_new returned null.".to_owned();
        }
        let mut infos_owned = infos;
        let mut found: usize = 0;
        let rc = f::fido_dev_info_manifest(infos, MANIFEST_COUNT, &mut found);
        let detail = if rc != f::FIDO_OK as c_int {
            format!("Probe: {}.", fido_call_detail("fido_dev_info_manifest", rc))
        } else if found == 0 {
            "Probe: fido_dev_info_manifest found 0 devices. Check that a USB-HID CTAP security key is connected and that udev/hidraw permissions allow this user to read it.".to_owned()
        } else {
            let mut devices = Vec::new();
            for index in 0..found.min(SUPPORT_DIAGNOSTIC_DEVICE_LIMIT) {
                devices.push(describe_device_info(f::fido_dev_info_ptr(infos, index)));
            }
            let suffix = if found > SUPPORT_DIAGNOSTIC_DEVICE_LIMIT {
                format!(
                    "; truncated={} more",
                    found - SUPPORT_DIAGNOSTIC_DEVICE_LIMIT
                )
            } else {
                String::new()
            };
            format!(
                "Probe: fido_dev_info_manifest found {found} device(s): [{}]{suffix}.",
                devices.join("; ")
            )
        };
        f::fido_dev_info_free(&mut infos_owned, MANIFEST_COUNT);
        detail
    }
}

fn uv_opt(requirement: u32, pin: Option<&CString>, built_in_uv_needed: bool) -> f::fido_opt_t {
    if pin.is_some() {
        return f::fido_opt_t_FIDO_OPT_OMIT;
    }
    if requirement == USER_VERIFICATION_REQUIRED || built_in_uv_needed {
        f::fido_opt_t_FIDO_OPT_TRUE
    } else {
        f::fido_opt_t_FIDO_OPT_OMIT
    }
}

fn pin_cstring(pin: Option<&str>, prefix: &str) -> Result<Option<CString>> {
    let Some(pin) = pin.filter(|value| !value.is_empty()) else {
        return Ok(None);
    };
    CString::new(pin)
        .map(Some)
        .map_err(|_| ceremony_error(prefix, "PinInvalid (PIN contains an invalid character)"))
}

fn pin_ptr(pin: Option<&CString>) -> *const c_char {
    pin.map_or(ptr::null(), |value| value.as_ptr())
}

fn dev_has_pin(dev: *mut f::fido_dev_t) -> bool {
    unsafe { f::fido_dev_has_pin(dev) }
}

fn dev_has_uv(dev: *mut f::fido_dev_t) -> bool {
    unsafe { f::fido_dev_has_uv(dev) }
}

fn dev_supports_rk(dev: *mut f::fido_dev_t) -> bool {
    unsafe {
        let mut info = f::fido_cbor_info_new();
        if info.is_null() {
            return false;
        }
        let mut supported = false;
        if f::fido_dev_get_cbor_info(dev, info) == f::FIDO_OK as c_int {
            let len = f::fido_cbor_info_options_len(info);
            let names = f::fido_cbor_info_options_name_ptr(info);
            let values = f::fido_cbor_info_options_value_ptr(info);
            if len > 0 && !names.is_null() && !values.is_null() {
                let names = std::slice::from_raw_parts(names, len);
                let values = std::slice::from_raw_parts(values, len);
                supported = names
                    .iter()
                    .zip(values)
                    .any(|(name, value)| *value && c_string(*name).as_deref() == Some("rk"));
            }
        }
        f::fido_cbor_info_free(&mut info);
        supported
    }
}

fn wants_resident_key(input: &CreateInput, dev: *mut f::fido_dev_t) -> bool {
    input.require_resident_key
        || (input.prefer_resident_key
            && dev_supports_rk(dev)
            && (dev_has_pin(dev) || dev_has_uv(dev)))
}

fn pin_retry_count(dev: *mut f::fido_dev_t) -> Option<c_int> {
    let mut retries: c_int = 0;
    let rc = unsafe { f::fido_dev_get_retry_count(dev, &mut retries) };
    if rc == f::FIDO_OK as c_int {
        Some(retries)
    } else {
        None
    }
}

fn ceremony_failure(prefix: &str, dev: &DevGuard, operation: &str, rc: c_int) -> napi::Error {
    if rc == f::FIDO_ERR_PIN_REQUIRED as c_int || rc == f::FIDO_ERR_PIN_AUTH_INVALID as c_int {
        return ceremony_error(prefix, "PinRequired");
    }
    if rc == f::FIDO_ERR_PIN_INVALID as c_int {
        let detail = match pin_retry_count(dev.0) {
            Some(retries) => format!("PinInvalid retriesRemaining={retries}"),
            None => "PinInvalid".to_owned(),
        };
        return ceremony_error(prefix, &detail);
    }
    if rc == f::FIDO_ERR_PIN_AUTH_BLOCKED as c_int {
        return ceremony_error(
            prefix,
            "PinAuthBlocked (too many failed attempts; unplug and reinsert the security key)",
        );
    }
    if rc == f::FIDO_ERR_PIN_BLOCKED as c_int {
        return ceremony_error(
            prefix,
            "PinBlocked (the security key PIN is locked; the key must be reset)",
        );
    }
    if rc == f::FIDO_ERR_PIN_NOT_SET as c_int {
        return ceremony_error(
            prefix,
            "PinNotSet (the security key requires a PIN to be configured first)",
        );
    }
    if rc == f::FIDO_ERR_UV_BLOCKED as c_int || rc == f::FIDO_ERR_UV_INVALID as c_int {
        return ceremony_error(
            prefix,
            "UserVerificationBlocked (on-key user verification failed; use the key PIN or reset the key)",
        );
    }
    if rc == f::FIDO_ERR_ACTION_TIMEOUT as c_int || rc == f::FIDO_ERR_USER_ACTION_TIMEOUT as c_int {
        return ceremony_error(prefix, "Timeout (the security key was not touched in time)");
    }
    if rc == f::FIDO_ERR_OPERATION_DENIED as c_int || rc == f::FIDO_ERR_KEEPALIVE_CANCEL as c_int {
        return ceremony_error(
            prefix,
            "NotAllowed (the request was denied on the security key)",
        );
    }
    if rc == f::FIDO_ERR_NO_CREDENTIALS as c_int {
        return ceremony_error(
            prefix,
            "NotAllowed (the security key has no passkey for this site)",
        );
    }
    if rc == f::FIDO_ERR_UNSUPPORTED_OPTION as c_int {
        return ceremony_error(
            prefix,
            "UnsupportedOption (the security key does not support the requested options)",
        );
    }
    let message = format!("Fido2CallFailed ({})", fido_call_detail(operation, rc));
    ceremony_error(prefix, &message)
}

fn selected_cose_alg(algs: &[i32]) -> c_int {
    for &alg in algs {
        if alg == f::COSE_ES256
            || alg == f::COSE_RS256
            || alg == f::COSE_EDDSA
            || alg == f::COSE_ES384
        {
            return alg;
        }
    }
    f::COSE_ES256
}

struct DevGuard(*mut f::fido_dev_t);
impl Drop for DevGuard {
    fn drop(&mut self) {
        if !self.0.is_null() {
            unsafe {
                f::fido_dev_close(self.0);
                let mut p = self.0;
                f::fido_dev_free(&mut p);
            }
        }
    }
}

fn open_first_device(timeout_ms: u32, prefix: &str) -> Result<DevGuard> {
    init();

    unsafe {
        let infos = f::fido_dev_info_new(MANIFEST_COUNT);
        if infos.is_null() {
            return Err(ceremony_error(prefix, "OutOfMemory"));
        }
        let mut infos_owned = infos;
        let mut found: usize = 0;
        let rc = f::fido_dev_info_manifest(infos, MANIFEST_COUNT, &mut found);
        if rc != f::FIDO_OK as c_int {
            f::fido_dev_info_free(&mut infos_owned, MANIFEST_COUNT);
            let message = format!(
                "Fido2CallFailed ({})",
                fido_call_detail("fido_dev_info_manifest", rc)
            );
            return Err(ceremony_error(prefix, &message));
        }
        if found == 0 {
            f::fido_dev_info_free(&mut infos_owned, MANIFEST_COUNT);
            return Err(ceremony_error(
                prefix,
                "NoAuthenticator (fido_dev_info_manifest found 0 devices; check USB-HID CTAP authenticator presence and udev/hidraw permissions)",
            ));
        }

        let mut opened: Option<*mut f::fido_dev_t> = None;
        let mut open_failures: Vec<String> = Vec::new();
        for index in 0..found {
            let info = f::fido_dev_info_ptr(infos, index);
            if info.is_null() {
                continue;
            }
            let path = f::fido_dev_info_path(info);
            if path.is_null() {
                continue;
            }
            let dev = f::fido_dev_new();
            if dev.is_null() {
                f::fido_dev_info_free(&mut infos_owned, MANIFEST_COUNT);
                return Err(ceremony_error(prefix, "OutOfMemory"));
            }
            let open_rc = f::fido_dev_open(dev, path);
            if open_rc == f::FIDO_OK as c_int {
                if timeout_ms > 0 {
                    let clamped: c_int = timeout_ms.min(c_int::MAX as u32) as c_int;
                    let _ = f::fido_dev_set_timeout(dev, clamped);
                }
                opened = Some(dev);
                break;
            }
            open_failures.push(format!(
                "{} ({})",
                describe_device_info(info),
                fido_call_detail("fido_dev_open", open_rc)
            ));
            let mut p = dev;
            f::fido_dev_free(&mut p);
        }

        f::fido_dev_info_free(&mut infos_owned, MANIFEST_COUNT);
        opened.map(DevGuard).ok_or_else(|| {
            let message = format!(
                "NoAuthenticator (manifest found {found} device(s), but none opened: {})",
                open_failures.join("; ")
            );
            ceremony_error(prefix, &message)
        })
    }
}

fn check(code: c_int, prefix: &str) -> Result<()> {
    if code == f::FIDO_OK as c_int {
        Ok(())
    } else {
        let message = format!("Fido2CallFailed ({})", fido_call_detail("libfido2", code));
        Err(ceremony_error(prefix, &message))
    }
}

fn copy_bytes(ptr: *const u8, len: usize) -> Vec<u8> {
    if len == 0 || ptr.is_null() {
        return Vec::new();
    }

    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
    slice.to_vec()
}

pub fn make_credential(input: &mut CreateInput) -> Result<CreateResult> {
    let dev = open_first_device(input.timeout_ms, CREATE_PREFIX)?;
    let pin = pin_cstring(input.pin.as_deref(), CREATE_PREFIX)?;
    if pin.is_none() && dev_has_pin(dev.0) && !dev_has_uv(dev.0) {
        return Err(ceremony_error(CREATE_PREFIX, "PinRequired"));
    }

    unsafe {
        let cred = f::fido_cred_new();
        if cred.is_null() {
            return Err(ceremony_error(CREATE_PREFIX, "OutOfMemory"));
        }
        struct CredGuard(*mut f::fido_cred_t);
        impl Drop for CredGuard {
            fn drop(&mut self) {
                let mut p = self.0;
                unsafe { f::fido_cred_free(&mut p) };
            }
        }
        let cred_owned = CredGuard(cred);

        check(
            f::fido_cred_set_type(cred, selected_cose_alg(&input.pub_key_algs)),
            CREATE_PREFIX,
        )?;
        check(
            f::fido_cred_set_clientdata(
                cred,
                input.client_data_json.as_ptr(),
                input.client_data_json.len(),
            ),
            CREATE_PREFIX,
        )?;
        let rp_id_c = CString::new(input.rp_id.clone())
            .map_err(|_| ceremony_error(CREATE_PREFIX, "InvalidRpId"))?;
        let rp_name_c = CString::new(input.rp_name.clone())
            .map_err(|_| ceremony_error(CREATE_PREFIX, "InvalidRpName"))?;
        check(
            f::fido_cred_set_rp(cred, rp_id_c.as_ptr(), rp_name_c.as_ptr()),
            CREATE_PREFIX,
        )?;
        let user_name_c = CString::new(input.user_name.clone())
            .map_err(|_| ceremony_error(CREATE_PREFIX, "InvalidUserName"))?;
        let display_c = CString::new(input.user_display_name.clone())
            .map_err(|_| ceremony_error(CREATE_PREFIX, "InvalidDisplayName"))?;
        check(
            f::fido_cred_set_user(
                cred,
                input.user_id.as_ptr(),
                input.user_id.len(),
                user_name_c.as_ptr(),
                display_c.as_ptr(),
                ptr::null(),
            ),
            CREATE_PREFIX,
        )?;
        let resident_key = wants_resident_key(input, dev.0);
        if resident_key {
            check(
                f::fido_cred_set_rk(cred, f::fido_opt_t_FIDO_OPT_TRUE),
                CREATE_PREFIX,
            )?;
        }
        check(
            f::fido_cred_set_uv(
                cred,
                uv_opt(
                    input.user_verification,
                    pin.as_ref(),
                    resident_key && dev_has_uv(dev.0),
                ),
            ),
            CREATE_PREFIX,
        )?;
        for credential in &input.exclude_credentials {
            check(
                f::fido_cred_exclude(cred, credential.id.as_ptr(), credential.id.len()),
                CREATE_PREFIX,
            )?;
        }

        let mut rc = f::fido_dev_make_cred(dev.0, cred, pin_ptr(pin.as_ref()));
        if rc == f::FIDO_ERR_KEY_STORE_FULL as c_int && resident_key && !input.require_resident_key
        {
            check(
                f::fido_cred_set_rk(cred, f::fido_opt_t_FIDO_OPT_OMIT),
                CREATE_PREFIX,
            )?;
            rc = f::fido_dev_make_cred(dev.0, cred, pin_ptr(pin.as_ref()));
        }
        if rc != f::FIDO_OK as c_int {
            let _ = f::fido_dev_cancel(dev.0);
            return Err(ceremony_failure(
                CREATE_PREFIX,
                &dev,
                "fido_dev_make_cred",
                rc,
            ));
        }

        let raw_ptr = f::fido_cred_authdata_raw_ptr(cred);
        let auth_data = if !raw_ptr.is_null() {
            let len = f::fido_cred_authdata_raw_len(cred);
            copy_bytes(raw_ptr, len)
        } else {
            let ptr = f::fido_cred_authdata_ptr(cred);
            let len = f::fido_cred_authdata_len(cred);
            copy_bytes(ptr, len)
        };
        let att_stmt = copy_bytes(
            f::fido_cred_attstmt_ptr(cred),
            f::fido_cred_attstmt_len(cred),
        );
        let fmt_ptr = f::fido_cred_fmt(cred);
        let fmt = if fmt_ptr.is_null() {
            "none".to_owned()
        } else {
            CStr::from_ptr(fmt_ptr).to_string_lossy().into_owned()
        };
        let attestation_object = build_attestation_object(&fmt, &auth_data, &att_stmt)?;

        let raw_id = copy_bytes(f::fido_cred_id_ptr(cred), f::fido_cred_id_len(cred));
        let client_data_json = input.client_data_json.clone();

        drop(cred_owned);
        drop(dev);

        Ok(CreateResult {
            raw_id,
            attestation_object,
            client_data_json,
            used_transport: TRANSPORT_USB,
        })
    }
}

pub fn get_assertion(input: &mut GetInput) -> Result<GetResult> {
    let dev = open_first_device(input.timeout_ms, GET_PREFIX)?;
    let pin = pin_cstring(input.pin.as_deref(), GET_PREFIX)?;
    if pin.is_none()
        && input.user_verification == USER_VERIFICATION_REQUIRED
        && dev_has_pin(dev.0)
        && !dev_has_uv(dev.0)
    {
        return Err(ceremony_error(GET_PREFIX, "PinRequired"));
    }

    unsafe {
        let assertion = f::fido_assert_new();
        if assertion.is_null() {
            return Err(ceremony_error(GET_PREFIX, "OutOfMemory"));
        }
        struct AssertGuard(*mut f::fido_assert_t);
        impl Drop for AssertGuard {
            fn drop(&mut self) {
                let mut p = self.0;
                unsafe { f::fido_assert_free(&mut p) };
            }
        }
        let assert_owned = AssertGuard(assertion);

        check(
            f::fido_assert_set_clientdata(
                assertion,
                input.client_data_json.as_ptr(),
                input.client_data_json.len(),
            ),
            GET_PREFIX,
        )?;
        let rp_id_c = CString::new(input.rp_id.clone())
            .map_err(|_| ceremony_error(GET_PREFIX, "InvalidRpId"))?;
        check(
            f::fido_assert_set_rp(assertion, rp_id_c.as_ptr()),
            GET_PREFIX,
        )?;
        check(
            f::fido_assert_set_up(assertion, f::fido_opt_t_FIDO_OPT_TRUE),
            GET_PREFIX,
        )?;
        check(
            f::fido_assert_set_uv(
                assertion,
                uv_opt(input.user_verification, pin.as_ref(), false),
            ),
            GET_PREFIX,
        )?;
        for credential in &input.allow_credentials {
            check(
                f::fido_assert_allow_cred(assertion, credential.id.as_ptr(), credential.id.len()),
                GET_PREFIX,
            )?;
        }

        let rc = f::fido_dev_get_assert(dev.0, assertion, pin_ptr(pin.as_ref()));
        if rc != f::FIDO_OK as c_int {
            let _ = f::fido_dev_cancel(dev.0);
            return Err(ceremony_failure(
                GET_PREFIX,
                &dev,
                "fido_dev_get_assert",
                rc,
            ));
        }
        if f::fido_assert_count(assertion) == 0 {
            return Err(ceremony_error(GET_PREFIX, "InvalidWebAuthnResult"));
        }

        let raw_ptr = f::fido_assert_authdata_raw_ptr(assertion, 0);
        let auth_data = if !raw_ptr.is_null() {
            let len = f::fido_assert_authdata_raw_len(assertion, 0);
            copy_bytes(raw_ptr, len)
        } else {
            let ptr = f::fido_assert_authdata_ptr(assertion, 0);
            let len = f::fido_assert_authdata_len(assertion, 0);
            copy_bytes(ptr, len)
        };
        let user_id_len = f::fido_assert_user_id_len(assertion, 0);
        let user_handle = if user_id_len > 0 {
            Some(copy_bytes(
                f::fido_assert_user_id_ptr(assertion, 0),
                user_id_len,
            ))
        } else {
            None
        };

        let mut raw_id = copy_bytes(
            f::fido_assert_id_ptr(assertion, 0),
            f::fido_assert_id_len(assertion, 0),
        );
        if raw_id.is_empty() && input.allow_credentials.len() == 1 {
            raw_id = input.allow_credentials[0].id.clone();
        }
        if raw_id.is_empty() {
            return Err(ceremony_error(
                GET_PREFIX,
                "InvalidWebAuthnResult (authenticator omitted the credential id)",
            ));
        }
        let signature = copy_bytes(
            f::fido_assert_sig_ptr(assertion, 0),
            f::fido_assert_sig_len(assertion, 0),
        );
        let client_data_json = input.client_data_json.clone();

        drop(assert_owned);
        drop(dev);

        Ok(GetResult {
            raw_id,
            authenticator_data: auth_data,
            signature,
            user_handle,
            client_data_json,
            used_transport: TRANSPORT_USB,
        })
    }
}
