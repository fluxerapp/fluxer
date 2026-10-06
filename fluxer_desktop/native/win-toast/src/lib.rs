// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::Task;
use napi::bindgen_prelude::{AsyncTask, Env, Error, Result, Status, Unknown};
use napi_derive::napi;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawToastNotifyOptions {
    aumid: Option<String>,
    tag: Option<String>,
    group: Option<String>,
    expiration_time: Option<String>,
    scenario: Option<String>,
    audio: Option<RawAudio>,
    lines: Option<Vec<RawToastText>>,
    images: Option<Vec<RawToastImage>>,
    inputs: Option<Vec<RawToastInput>>,
    actions: Option<Vec<RawToastAction>>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawToastText {
    text: Option<String>,
    hint_max_lines: Option<i32>,
    hint: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawToastImage {
    uri: Option<String>,
    placement: Option<String>,
    hint_crop: Option<String>,
    alt: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawToastInput {
    id: Option<String>,
    r#type: Option<String>,
    placeholder: Option<String>,
    title: Option<String>,
    options: Option<Vec<RawToastInputOption>>,
}

#[derive(Debug, Deserialize)]
struct RawToastInputOption {
    id: Option<String>,
    content: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawToastAction {
    label: Option<String>,
    args: Option<String>,
    activation_type: Option<String>,
    image_uri: Option<String>,
    hint_input_id: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum RawAudio {
    Keyword(String),
    Object(RawAudioObject),
}

#[derive(Debug, Deserialize)]
struct RawAudioObject {
    silent: Option<bool>,
    #[serde(rename = "loop")]
    loop_audio: Option<bool>,
    src: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ToastSpec {
    aumid: String,
    tag: Option<String>,
    group: Option<String>,
    expiration_time: Option<WindowsDateTimeTicks>,
    scenario: Option<String>,
    audio_src: Option<String>,
    audio_silent: bool,
    audio_loop: bool,
    lines: Vec<ToastTextSpec>,
    images: Vec<ToastImageSpec>,
    inputs: Vec<ToastInputSpec>,
    actions: Vec<ToastActionSpec>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct WindowsDateTimeTicks(i64);

#[derive(Debug, Clone, PartialEq, Eq)]
struct ToastTextSpec {
    text: String,
    hint_max_lines: Option<i32>,
    is_attribution: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ToastImageSpec {
    uri: String,
    placement: Option<String>,
    hint_crop: Option<String>,
    alt: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ToastInputSpec {
    id: String,
    r#type: String,
    placeholder: Option<String>,
    title: Option<String>,
    options: Vec<ToastInputOption>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ToastInputOption {
    id: String,
    content: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ToastActionSpec {
    label: String,
    args: String,
    activation_type: Option<String>,
    image_uri: Option<String>,
    hint_input_id: Option<String>,
}

#[derive(Debug, Deserialize)]
struct RawDismissOptions {
    aumid: Option<String>,
    tag: Option<String>,
    group: Option<String>,
}

#[derive(Debug, Deserialize)]
struct RawClearOptions {
    aumid: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum HistoryOp {
    Clear,
    RemoveTag { tag: String, group: Option<String> },
}

#[napi(object)]
pub struct ToastSupport {
    pub supported: bool,
    pub reason: Option<String>,
}

pub struct NotifyTask {
    spec: ToastSpec,
}

pub struct HistoryTask {
    aumid: String,
    op: HistoryOp,
}

#[napi(js_name = "isSupported")]
pub fn is_supported() -> ToastSupport {
    if cfg!(target_os = "windows") {
        ToastSupport {
            supported: true,
            reason: None,
        }
    } else {
        ToastSupport {
            supported: false,
            reason: Some("win-toast only runs on Windows".to_string()),
        }
    }
}

#[napi]
pub fn notify(env: Env, opts: Unknown) -> Result<AsyncTask<NotifyTask>> {
    let raw = env
        .from_js_value::<RawToastNotifyOptions, _>(opts)
        .map_err(|_| invalid_arg("invalid toast spec"))?;
    Ok(AsyncTask::new(NotifyTask {
        spec: validate_toast_spec(raw)?,
    }))
}

#[napi]
pub fn dismiss(env: Env, opts: Unknown) -> Result<AsyncTask<HistoryTask>> {
    let raw = env
        .from_js_value::<RawDismissOptions, _>(opts)
        .map_err(|_| invalid_arg("history op requires an options object"))?;
    let aumid = require_non_empty(raw.aumid, "aumid is required")?;
    let tag = require_non_empty(raw.tag, "tag is required for dismiss")?;
    Ok(AsyncTask::new(HistoryTask {
        aumid,
        op: HistoryOp::RemoveTag {
            tag,
            group: raw.group,
        },
    }))
}

#[napi]
pub fn clear(env: Env, opts: Unknown) -> Result<AsyncTask<HistoryTask>> {
    let raw = env
        .from_js_value::<RawClearOptions, _>(opts)
        .map_err(|_| invalid_arg("history op requires an options object"))?;
    Ok(AsyncTask::new(HistoryTask {
        aumid: require_non_empty(raw.aumid, "aumid is required")?,
        op: HistoryOp::Clear,
    }))
}

impl Task for NotifyTask {
    type Output = ();
    type JsValue = ();

    fn compute(&mut self) -> Result<Self::Output> {
        platform::show_toast(&self.spec)
    }

    fn resolve(&mut self, _env: Env, _output: Self::Output) -> Result<Self::JsValue> {
        Ok(())
    }
}

impl Task for HistoryTask {
    type Output = ();
    type JsValue = ();

    fn compute(&mut self) -> Result<Self::Output> {
        platform::history_op(&self.aumid, &self.op)
    }

    fn resolve(&mut self, _env: Env, _output: Self::Output) -> Result<Self::JsValue> {
        Ok(())
    }
}

fn validate_toast_spec(raw: RawToastNotifyOptions) -> Result<ToastSpec> {
    let (audio_silent, audio_loop, audio_src) = parse_audio(raw.audio);
    Ok(ToastSpec {
        aumid: require_non_empty(raw.aumid, "aumid is required")?,
        tag: raw.tag,
        group: raw.group,
        expiration_time: raw
            .expiration_time
            .as_deref()
            .map(parse_windows_datetime)
            .transpose()?,
        scenario: raw.scenario,
        audio_src,
        audio_silent,
        audio_loop,
        lines: parse_lines(raw.lines)?,
        images: parse_images(raw.images)?,
        inputs: parse_inputs(raw.inputs)?,
        actions: parse_actions(raw.actions)?,
    })
}

fn parse_audio(raw: Option<RawAudio>) -> (bool, bool, Option<String>) {
    match raw {
        Some(RawAudio::Keyword(keyword)) if keyword == "silent" => (true, false, None),
        Some(RawAudio::Object(raw)) => (
            raw.silent.unwrap_or(false),
            raw.loop_audio.unwrap_or(false),
            raw.src,
        ),
        _ => (false, false, None),
    }
}

fn parse_lines(raw: Option<Vec<RawToastText>>) -> Result<Vec<ToastTextSpec>> {
    let raw = raw.ok_or_else(|| invalid_arg("invalid toast spec"))?;
    if raw.is_empty() {
        return Err(invalid_arg("invalid toast spec"));
    }
    raw.into_iter()
        .map(|line| {
            Ok(ToastTextSpec {
                text: require_non_empty(line.text, "invalid toast spec")?,
                hint_max_lines: line.hint_max_lines,
                is_attribution: line.hint.as_deref() == Some("attribution"),
            })
        })
        .collect()
}

fn parse_images(raw: Option<Vec<RawToastImage>>) -> Result<Vec<ToastImageSpec>> {
    raw.unwrap_or_default()
        .into_iter()
        .map(|image| {
            Ok(ToastImageSpec {
                uri: require_non_empty(image.uri, "invalid toast spec")?,
                placement: image.placement,
                hint_crop: image.hint_crop,
                alt: image.alt,
            })
        })
        .collect()
}

fn parse_inputs(raw: Option<Vec<RawToastInput>>) -> Result<Vec<ToastInputSpec>> {
    raw.unwrap_or_default()
        .into_iter()
        .map(|input| {
            Ok(ToastInputSpec {
                id: require_non_empty(input.id, "invalid toast spec")?,
                r#type: require_non_empty(input.r#type, "invalid toast spec")?,
                placeholder: input.placeholder,
                title: input.title,
                options: input
                    .options
                    .unwrap_or_default()
                    .into_iter()
                    .map(|option| {
                        Ok(ToastInputOption {
                            id: require_non_empty(option.id, "invalid toast spec")?,
                            content: require_non_empty(option.content, "invalid toast spec")?,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?,
            })
        })
        .collect()
}

fn parse_actions(raw: Option<Vec<RawToastAction>>) -> Result<Vec<ToastActionSpec>> {
    raw.unwrap_or_default()
        .into_iter()
        .map(|action| {
            Ok(ToastActionSpec {
                label: require_non_empty(action.label, "invalid toast spec")?,
                args: require_non_empty(action.args, "invalid toast spec")?,
                activation_type: action.activation_type,
                image_uri: action.image_uri,
                hint_input_id: action.hint_input_id,
            })
        })
        .collect()
}

#[cfg(any(target_os = "windows", test))]
fn build_toast_xml(spec: &ToastSpec) -> String {
    let mut xml = String::new();
    xml.push_str("<toast");
    if let Some(scenario) = spec.scenario.as_deref() {
        xml.push_str(" scenario=\"");
        push_escaped(&mut xml, scenario);
        xml.push('"');
    }
    xml.push('>');
    xml.push_str("<visual><binding template=\"ToastGeneric\">");

    for image in &spec.images {
        xml.push_str("<image src=\"");
        push_escaped(&mut xml, &image.uri);
        xml.push('"');
        if let Some(placement) = image.placement.as_deref() {
            xml.push_str(" placement=\"");
            push_escaped(&mut xml, placement);
            xml.push('"');
        }
        if let Some(hint_crop) = image.hint_crop.as_deref() {
            xml.push_str(" hint-crop=\"");
            push_escaped(&mut xml, hint_crop);
            xml.push('"');
        }
        if let Some(alt) = image.alt.as_deref() {
            xml.push_str(" alt=\"");
            push_escaped(&mut xml, alt);
            xml.push('"');
        }
        xml.push_str("/>");
    }

    for line in &spec.lines {
        xml.push_str("<text");
        if line.is_attribution {
            xml.push_str(" placement=\"attribution\"");
        }
        if let Some(max_lines) = line.hint_max_lines {
            xml.push_str(" hint-maxLines=\"");
            xml.push_str(&max_lines.to_string());
            xml.push('"');
        }
        xml.push('>');
        push_escaped(&mut xml, &line.text);
        xml.push_str("</text>");
    }

    xml.push_str("</binding></visual>");

    if !spec.inputs.is_empty() || !spec.actions.is_empty() {
        xml.push_str("<actions>");
        for input in &spec.inputs {
            xml.push_str("<input id=\"");
            push_escaped(&mut xml, &input.id);
            xml.push_str("\" type=\"");
            push_escaped(&mut xml, &input.r#type);
            xml.push('"');
            if let Some(placeholder) = input.placeholder.as_deref() {
                xml.push_str(" placeHolderContent=\"");
                push_escaped(&mut xml, placeholder);
                xml.push('"');
            }
            if let Some(title) = input.title.as_deref() {
                xml.push_str(" title=\"");
                push_escaped(&mut xml, title);
                xml.push('"');
            }
            if input.options.is_empty() {
                xml.push_str("/>");
            } else {
                xml.push('>');
                for option in &input.options {
                    xml.push_str("<selection id=\"");
                    push_escaped(&mut xml, &option.id);
                    xml.push_str("\" content=\"");
                    push_escaped(&mut xml, &option.content);
                    xml.push_str("\"/>");
                }
                xml.push_str("</input>");
            }
        }

        for action in &spec.actions {
            xml.push_str("<action content=\"");
            push_escaped(&mut xml, &action.label);
            xml.push_str("\" arguments=\"");
            push_escaped(&mut xml, &action.args);
            xml.push_str("\" activationType=\"");
            push_escaped(
                &mut xml,
                action.activation_type.as_deref().unwrap_or("foreground"),
            );
            xml.push('"');
            if let Some(image_uri) = action.image_uri.as_deref() {
                xml.push_str(" imageUri=\"");
                push_escaped(&mut xml, image_uri);
                xml.push('"');
            }
            if let Some(hint_input_id) = action.hint_input_id.as_deref() {
                xml.push_str(" hint-inputId=\"");
                push_escaped(&mut xml, hint_input_id);
                xml.push('"');
            }
            xml.push_str("/>");
        }
        xml.push_str("</actions>");
    }

    if spec.audio_silent {
        xml.push_str("<audio silent=\"true\"/>");
    } else if let Some(src) = spec.audio_src.as_deref() {
        xml.push_str("<audio src=\"");
        push_escaped(&mut xml, src);
        xml.push('"');
        if spec.audio_loop {
            xml.push_str(" loop=\"true\"");
        }
        xml.push_str("/>");
    }

    xml.push_str("</toast>");
    xml
}

#[cfg(any(target_os = "windows", test))]
fn push_escaped(out: &mut String, value: &str) {
    for ch in value.chars() {
        match ch {
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&apos;"),
            _ => out.push(ch),
        }
    }
}

fn parse_windows_datetime(value: &str) -> Result<WindowsDateTimeTicks> {
    parse_rfc3339_utc_100ns(value)
        .map(WindowsDateTimeTicks)
        .ok_or_else(|| invalid_arg("expirationTime must be an ISO timestamp"))
}

fn parse_rfc3339_utc_100ns(value: &str) -> Option<i64> {
    let bytes = value.as_bytes();
    if bytes.len() < 20 {
        return None;
    }

    let year = parse_digits(bytes, 0, 4)? as i32;
    expect_byte(bytes, 4, b'-')?;
    let month = parse_digits(bytes, 5, 2)?;
    expect_byte(bytes, 7, b'-')?;
    let day = parse_digits(bytes, 8, 2)?;
    if bytes.get(10).copied()? != b'T' && bytes.get(10).copied()? != b't' {
        return None;
    }
    let hour = parse_digits(bytes, 11, 2)?;
    expect_byte(bytes, 13, b':')?;
    let minute = parse_digits(bytes, 14, 2)?;
    expect_byte(bytes, 16, b':')?;
    let second = parse_digits(bytes, 17, 2)?;

    if !valid_date(year, month, day) || hour > 23 || minute > 59 || second > 59 {
        return None;
    }

    let mut index = 19;
    let mut fractional_100ns = 0_i64;
    if bytes.get(index).copied() == Some(b'.') {
        index += 1;
        let start = index;
        let mut digits = 0;
        while let Some(byte) = bytes.get(index).copied() {
            if !byte.is_ascii_digit() {
                break;
            }
            if digits < 7 {
                fractional_100ns = (fractional_100ns * 10) + i64::from(byte - b'0');
            }
            digits += 1;
            index += 1;
        }
        if index == start {
            return None;
        }
        for _ in digits..7 {
            fractional_100ns *= 10;
        }
    }

    let offset_seconds = match bytes.get(index).copied()? {
        b'Z' | b'z' => {
            index += 1;
            0_i64
        }
        b'+' | b'-' => {
            let sign = if bytes[index] == b'+' { 1_i64 } else { -1_i64 };
            index += 1;
            let offset_hour = parse_digits(bytes, index, 2)? as i64;
            index += 2;
            expect_byte(bytes, index, b':')?;
            index += 1;
            let offset_minute = parse_digits(bytes, index, 2)? as i64;
            index += 2;
            if offset_hour > 23 || offset_minute > 59 {
                return None;
            }
            sign * ((offset_hour * 3600) + (offset_minute * 60))
        }
        _ => return None,
    };
    if index != bytes.len() {
        return None;
    }

    let days = days_from_civil(year, month, day);
    let unix_seconds = (i128::from(days) * 86_400) + i128::from(hour * 3600 + minute * 60 + second)
        - i128::from(offset_seconds);
    const WINDOWS_TICK_OFFSET_SECONDS: i128 = 11_644_473_600;
    let windows_ticks =
        (unix_seconds + WINDOWS_TICK_OFFSET_SECONDS) * 10_000_000 + i128::from(fractional_100ns);
    i64::try_from(windows_ticks)
        .ok()
        .filter(|ticks| *ticks >= 0)
}

fn parse_digits(bytes: &[u8], start: usize, len: usize) -> Option<u32> {
    let mut value = 0_u32;
    for index in start..start + len {
        let byte = *bytes.get(index)?;
        if !byte.is_ascii_digit() {
            return None;
        }
        value = (value * 10) + u32::from(byte - b'0');
    }
    Some(value)
}

fn expect_byte(bytes: &[u8], index: usize, expected: u8) -> Option<()> {
    (*bytes.get(index)? == expected).then_some(())
}

fn valid_date(year: i32, month: u32, day: u32) -> bool {
    if !(1..=12).contains(&month) {
        return false;
    }
    let max_day = match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap_year(year) => 29,
        2 => 28,
        _ => return false,
    };
    (1..=max_day).contains(&day)
}

fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

fn days_from_civil(year: i32, month: u32, day: u32) -> i64 {
    let year = year - i32::from(month <= 2);
    let era = if year >= 0 { year } else { year - 399 } / 400;
    let yoe = year - era * 400;
    let month = month as i32;
    let doy = (153 * (month + if month > 2 { -3 } else { 9 }) + 2) / 5 + day as i32 - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    i64::from(era * 146_097 + doe - 719_468)
}

fn require_non_empty(value: Option<String>, message: &'static str) -> Result<String> {
    match value {
        Some(value) if !value.is_empty() => Ok(value),
        _ => Err(invalid_arg(message)),
    }
}

fn invalid_arg(message: &'static str) -> Error {
    Error::new(Status::InvalidArg, message)
}

#[cfg(target_os = "windows")]
mod platform {
    use super::{HistoryOp, ToastSpec, build_toast_xml};
    use napi::bindgen_prelude::{Error, Result, Status};
    use windows::Data::Xml::Dom::XmlDocument;
    use windows::Foundation::{DateTime, IReference, PropertyValue};
    use windows::UI::Notifications::{ToastNotification, ToastNotificationManager};
    use windows::Win32::System::WinRT::{RO_INIT_MULTITHREADED, RoInitialize, RoUninitialize};
    use windows::core::{HSTRING, Interface};

    pub(super) fn show_toast(spec: &ToastSpec) -> Result<()> {
        let _winrt = WinRtApartment::initialize()?;

        let xml = build_toast_xml(spec);
        let document = XmlDocument::new()
            .map_err(|err| hresult_error("XmlDocument activation failed", err))?;
        document
            .LoadXml(&HSTRING::from(xml.as_str()))
            .map_err(|err| hresult_error("Toast XML load failed", err))?;

        let notification = ToastNotification::CreateToastNotification(&document)
            .map_err(|err| hresult_error("ToastNotification creation failed", err))?;
        if let Some(tag) = spec.tag.as_deref() {
            notification
                .SetTag(&HSTRING::from(tag))
                .map_err(|err| hresult_error("ToastNotification SetTag failed", err))?;
        }
        if let Some(group) = spec.group.as_deref() {
            notification
                .SetGroup(&HSTRING::from(group))
                .map_err(|err| hresult_error("ToastNotification SetGroup failed", err))?;
        }
        if let Some(expiration_time) = spec.expiration_time {
            let value = PropertyValue::CreateDateTime(DateTime {
                UniversalTime: expiration_time.0,
            })
            .map_err(|err| hresult_error("PropertyValue::CreateDateTime failed", err))?;
            let reference: IReference<DateTime> = value
                .cast()
                .map_err(|err| hresult_error("QueryInterface(IReference<DateTime>) failed", err))?;
            notification
                .SetExpirationTime(&reference)
                .map_err(|err| hresult_error("ToastNotification SetExpirationTime failed", err))?;
        }

        let notifier = ToastNotificationManager::CreateToastNotifierWithId(&HSTRING::from(
            spec.aumid.as_str(),
        ))
        .map_err(|err| hresult_error("ToastNotifier creation failed", err))?;

        notifier
            .Show(&notification)
            .map_err(|err| hresult_error("ToastNotifier Show failed", err))
    }

    pub(super) fn history_op(aumid: &str, op: &HistoryOp) -> Result<()> {
        let _winrt = WinRtApartment::initialize()?;
        let history = ToastNotificationManager::History()
            .map_err(|err| hresult_error("ToastNotificationHistory activation failed", err))?;
        let aumid = HSTRING::from(aumid);
        match op {
            HistoryOp::Clear => history
                .ClearWithId(&aumid)
                .map_err(|err| hresult_error("ToastNotificationHistory ClearWithId failed", err)),
            HistoryOp::RemoveTag { tag, group } => {
                let tag_value = tag.as_str();
                let tag = HSTRING::from(tag_value);
                let group = HSTRING::from(group.as_deref().unwrap_or(tag_value));
                history
                    .RemoveGroupedTagWithId(&tag, &group, &aumid)
                    .map_err(|err| {
                        hresult_error(
                            "ToastNotificationHistory RemoveGroupedTagWithId failed",
                            err,
                        )
                    })
            }
        }
    }

    struct WinRtApartment;

    impl WinRtApartment {
        fn initialize() -> Result<Self> {
            unsafe {
                RoInitialize(RO_INIT_MULTITHREADED)
                    .map_err(|err| hresult_error("RoInitialize failed", err))?;
            }
            Ok(Self)
        }
    }

    impl Drop for WinRtApartment {
        fn drop(&mut self) {
            unsafe {
                RoUninitialize();
            }
        }
    }

    fn hresult_error(label: &'static str, err: windows::core::Error) -> Error {
        Error::new(
            Status::GenericFailure,
            format!("{label} (hr 0x{:x})", err.code().0 as u32),
        )
    }
}

#[cfg(not(target_os = "windows"))]
mod platform {
    use super::{HistoryOp, ToastSpec};
    use napi::bindgen_prelude::{Error, Result, Status};

    pub(super) fn show_toast(_spec: &ToastSpec) -> Result<()> {
        Err(Error::new(
            Status::GenericFailure,
            "win-toast not supported on this platform",
        ))
    }

    pub(super) fn history_op(_aumid: &str, _op: &HistoryOp) -> Result<()> {
        Err(Error::new(
            Status::GenericFailure,
            "win-toast not supported on this platform",
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn valid_raw_options() -> RawToastNotifyOptions {
        RawToastNotifyOptions {
            aumid: Some("com.fluxer.Desktop".to_string()),
            tag: None,
            group: None,
            expiration_time: None,
            scenario: None,
            audio: None,
            lines: Some(vec![RawToastText {
                text: Some("Fluxer".to_string()),
                hint_max_lines: None,
                hint: None,
            }]),
            images: None,
            inputs: None,
            actions: None,
        }
    }

    #[test]
    fn validation_rejects_missing_aumid() {
        let mut raw = valid_raw_options();
        raw.aumid = None;
        let err = validate_toast_spec(raw).expect_err("missing AUMID should be rejected");
        assert_eq!(err.status, Status::InvalidArg);
        assert_eq!(err.reason, "aumid is required");
    }

    #[test]
    fn validation_rejects_empty_lines() {
        let mut raw = valid_raw_options();
        raw.lines = Some(vec![]);
        let err = validate_toast_spec(raw).expect_err("empty lines should be rejected");
        assert_eq!(err.status, Status::InvalidArg);
        assert_eq!(err.reason, "invalid toast spec");
    }

    #[test]
    fn validation_maps_audio_keyword_and_object() {
        let mut silent = valid_raw_options();
        silent.audio = Some(RawAudio::Keyword("silent".to_string()));
        let spec = validate_toast_spec(silent).expect("silent audio should validate");
        assert!(spec.audio_silent);
        assert!(!spec.audio_loop);
        assert_eq!(spec.audio_src, None);

        let mut custom = valid_raw_options();
        custom.audio = Some(RawAudio::Object(RawAudioObject {
            silent: Some(false),
            loop_audio: Some(true),
            src: Some("ms-appx:///tone.mp3".to_string()),
        }));
        let spec = validate_toast_spec(custom).expect("custom audio should validate");
        assert!(!spec.audio_silent);
        assert!(spec.audio_loop);
        assert_eq!(spec.audio_src.as_deref(), Some("ms-appx:///tone.mp3"));
    }

    #[test]
    fn toast_xml_escapes_values_and_preserves_shape() {
        let spec = ToastSpec {
            aumid: "com.fluxer.Desktop".to_string(),
            tag: Some("tag".to_string()),
            group: Some("group".to_string()),
            expiration_time: None,
            scenario: Some("reminder&urgent".to_string()),
            audio_src: Some("ms-appx:///sounds/a&b.mp3".to_string()),
            audio_silent: false,
            audio_loop: true,
            lines: vec![
                ToastTextSpec {
                    text: "A < B & \"quoted\"".to_string(),
                    hint_max_lines: Some(2),
                    is_attribution: false,
                },
                ToastTextSpec {
                    text: "via Fluxer".to_string(),
                    hint_max_lines: None,
                    is_attribution: true,
                },
            ],
            images: vec![ToastImageSpec {
                uri: "file:///C:/a'b.png".to_string(),
                placement: Some("hero".to_string()),
                hint_crop: Some("circle".to_string()),
                alt: Some("A&B".to_string()),
            }],
            inputs: vec![ToastInputSpec {
                id: "reply".to_string(),
                r#type: "selection".to_string(),
                placeholder: Some("Say \"hi\"".to_string()),
                title: Some("Pick".to_string()),
                options: vec![ToastInputOption {
                    id: "yes".to_string(),
                    content: "Yes & go".to_string(),
                }],
            }],
            actions: vec![ToastActionSpec {
                label: "Open <now>".to_string(),
                args: "fluxer://open?x=1&y=2".to_string(),
                activation_type: None,
                image_uri: Some("file:///icon.png".to_string()),
                hint_input_id: Some("reply".to_string()),
            }],
        };

        assert_eq!(
            build_toast_xml(&spec),
            concat!(
                "<toast scenario=\"reminder&amp;urgent\">",
                "<visual><binding template=\"ToastGeneric\">",
                "<image src=\"file:///C:/a&apos;b.png\" placement=\"hero\" hint-crop=\"circle\" alt=\"A&amp;B\"/>",
                "<text hint-maxLines=\"2\">A &lt; B &amp; &quot;quoted&quot;</text>",
                "<text placement=\"attribution\">via Fluxer</text>",
                "</binding></visual>",
                "<actions>",
                "<input id=\"reply\" type=\"selection\" placeHolderContent=\"Say &quot;hi&quot;\" title=\"Pick\">",
                "<selection id=\"yes\" content=\"Yes &amp; go\"/>",
                "</input>",
                "<action content=\"Open &lt;now&gt;\" arguments=\"fluxer://open?x=1&amp;y=2\" activationType=\"foreground\" imageUri=\"file:///icon.png\" hint-inputId=\"reply\"/>",
                "</actions>",
                "<audio src=\"ms-appx:///sounds/a&amp;b.mp3\" loop=\"true\"/>",
                "</toast>"
            )
        );
    }

    #[test]
    fn parses_js_iso_timestamp_to_windows_ticks() {
        assert_eq!(
            parse_rfc3339_utc_100ns("1970-01-01T00:00:00.000Z"),
            Some(116_444_736_000_000_000)
        );
        assert_eq!(
            parse_rfc3339_utc_100ns("1970-01-01T01:30:00.1234567+01:00"),
            Some(116_444_754_001_234_567)
        );
    }

    #[test]
    fn rejects_invalid_iso_timestamp() {
        assert_eq!(parse_rfc3339_utc_100ns("2026-02-29T00:00:00Z"), None);
        assert_eq!(parse_rfc3339_utc_100ns("2026-01-01 00:00:00Z"), None);
        assert_eq!(parse_rfc3339_utc_100ns("2026-01-01T00:00:00"), None);
    }

    #[test]
    fn history_validation_rejects_missing_tag() {
        let raw = RawDismissOptions {
            aumid: Some("com.fluxer.Desktop".to_string()),
            tag: None,
            group: None,
        };
        let err = require_non_empty(raw.tag, "tag is required for dismiss")
            .expect_err("missing tag should be rejected");
        assert_eq!(err.status, Status::InvalidArg);
        assert_eq!(err.reason, "tag is required for dismiss");
    }

    #[cfg(not(target_os = "windows"))]
    #[test]
    fn non_windows_worker_preserves_stub_error_contract() {
        let spec = validate_toast_spec(valid_raw_options()).expect("options should validate");
        let err = platform::show_toast(&spec).expect_err("non-Windows should fail");
        assert_eq!(err.status, Status::GenericFailure);
        assert_eq!(err.reason, "win-toast not supported on this platform");

        let err = platform::history_op("com.fluxer.Desktop", &HistoryOp::Clear)
            .expect_err("non-Windows should fail");
        assert_eq!(err.status, Status::GenericFailure);
        assert_eq!(err.reason, "win-toast not supported on this platform");
    }
}
