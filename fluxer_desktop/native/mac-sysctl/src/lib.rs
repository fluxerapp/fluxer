// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::{Error, Result, Status};
use napi_derive::napi;

#[cfg(target_os = "macos")]
use sysctl::{Ctl, CtlValue, Sysctl, SysctlError};

fn invalid_name_error(function_name: &str) -> Error {
    Error::new(
        Status::InvalidArg,
        format!("{function_name} requires a non-empty name string"),
    )
}

#[cfg(target_os = "macos")]
fn map_sysctl_error(error: SysctlError) -> Error {
    match error {
        SysctlError::IoError(ref io_error) => {
            if let Some(errno) = io_error.raw_os_error() {
                return Error::new(
                    Status::GenericFailure,
                    format!("sysctlbyname failed (errno {errno})"),
                );
            }
        }
        SysctlError::MissingImplementation
        | SysctlError::ExtractionError
        | SysctlError::ParseError
        | SysctlError::InvalidCStr(_)
        | SysctlError::InvalidCString(_) => {
            return Error::new(
                Status::GenericFailure,
                "sysctlbyname failed (errno 22)".to_owned(),
            );
        }
        SysctlError::NotFound(_)
        | SysctlError::Utf8Error(_)
        | SysctlError::NoReadAccess
        | SysctlError::NoWriteAccess
        | SysctlError::NotSupported
        | SysctlError::ShortRead { .. } => {}
    }

    Error::new(
        Status::GenericFailure,
        format!("sysctlbyname failed ({error})"),
    )
}

#[cfg(not(target_os = "macos"))]
fn unsupported_platform_error() -> Error {
    Error::new(
        Status::GenericFailure,
        "sysctlbyname failed (errno 22)".to_owned(),
    )
}

#[cfg(target_os = "macos")]
fn ctl_for_name(name: &str) -> Result<Option<Ctl>> {
    match Ctl::new(name) {
        Ok(ctl) => Ok(Some(ctl)),
        Err(SysctlError::NotFound(_)) => Ok(None),
        Err(error) => Err(map_sysctl_error(error)),
    }
}

#[cfg(target_os = "macos")]
fn value_to_number(value: CtlValue) -> Result<Option<f64>> {
    let number = match value {
        CtlValue::None => return Ok(None),
        CtlValue::Int(value) => f64::from(value),
        CtlValue::Uint(value) => f64::from(value),
        CtlValue::Long(value) => value as f64,
        CtlValue::Ulong(value) => value as f64,
        CtlValue::S64(value) => value as f64,
        CtlValue::U64(value) => value as f64,
        CtlValue::S32(value) => f64::from(value),
        CtlValue::U32(value) => f64::from(value),
        CtlValue::S16(value) => f64::from(value),
        CtlValue::U16(value) => f64::from(value),
        CtlValue::S8(value) => f64::from(value),
        CtlValue::U8(value) => f64::from(value),
        CtlValue::String(_) | CtlValue::Struct(_) | CtlValue::Node(_) => {
            return Err(Error::new(
                Status::GenericFailure,
                "sysctlbyname failed (errno 22)".to_owned(),
            ));
        }
    };
    Ok(Some(number))
}

#[cfg(target_os = "macos")]
fn read_int_blocking(name: String) -> Result<Option<f64>> {
    let Some(ctl) = ctl_for_name(&name)? else {
        return Ok(None);
    };
    ctl.value()
        .map_err(map_sysctl_error)
        .and_then(value_to_number)
}

#[cfg(not(target_os = "macos"))]
fn read_int_blocking(_: String) -> Result<Option<f64>> {
    Err(unsupported_platform_error())
}

#[cfg(target_os = "macos")]
fn read_string_blocking(name: String) -> Result<Option<String>> {
    let Some(ctl) = ctl_for_name(&name)? else {
        return Ok(None);
    };
    match ctl.value().map_err(map_sysctl_error)? {
        CtlValue::None => Ok(None),
        CtlValue::String(value) => Ok(Some(value)),
        _ => Err(Error::new(
            Status::GenericFailure,
            "sysctlbyname failed (errno 22)".to_owned(),
        )),
    }
}

#[cfg(not(target_os = "macos"))]
fn read_string_blocking(_: String) -> Result<Option<String>> {
    Err(unsupported_platform_error())
}

async fn spawn_sysctl<T: Send + 'static>(
    work: impl FnOnce() -> Result<T> + Send + 'static,
) -> Result<T> {
    tokio::task::spawn_blocking(work).await.map_err(|error| {
        Error::new(
            Status::GenericFailure,
            format!("sysctl worker failed: {error}"),
        )
    })?
}

#[napi(js_name = "sysctlByNameInt")]
pub async fn sysctl_by_name_int(name: String) -> Result<Option<f64>> {
    if name.is_empty() {
        return Err(invalid_name_error("sysctlByNameInt"));
    }
    spawn_sysctl(move || read_int_blocking(name)).await
}

#[napi(js_name = "sysctlByNameString")]
pub async fn sysctl_by_name_string(name: String) -> Result<Option<String>> {
    if name.is_empty() {
        return Err(invalid_name_error("sysctlByNameString"));
    }
    spawn_sysctl(move || read_string_blocking(name)).await
}

#[cfg(all(test, target_os = "macos"))]
mod tests {
    use super::*;

    #[test]
    fn missing_sysctl_maps_to_null() {
        assert!(
            ctl_for_name("fluxer.definitely_missing_sysctl")
                .unwrap()
                .is_none()
        );
    }
}
