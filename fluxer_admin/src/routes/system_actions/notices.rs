// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{InstanceConfigUpdateRequest, InstanceNoticeUpdateRequest},
    utils::forms::MultiValueForm,
};

const NOTICE_MAX_CHARS: usize = 1000;
const LOCAL_MINUTE_TIMESTAMP_LEN: usize = 16;

fn parse_notice_time(form: &MultiValueForm, key: &str) -> Result<Option<String>, &'static str> {
    let Some(value) = form.clean(key) else {
        return Ok(None);
    };
    let with_seconds = if value.len() == LOCAL_MINUTE_TIMESTAMP_LEN {
        format!("{value}:00")
    } else {
        value
    };
    let format = time::format_description::parse_borrowed::<2>(
        "[year]-[month]-[day]T[hour]:[minute]:[second]",
    )
    .map_err(|_| "Failed to read the notice time")?;
    let parsed = time::PrimitiveDateTime::parse(&with_seconds, &format)
        .map_err(|_| "Enter the notice times as a date and time")?;
    parsed
        .assume_utc()
        .format(&time::format_description::well_known::Rfc3339)
        .map(Some)
        .map_err(|_| "Failed to format the notice time")
}

pub(super) fn build_notice_update(
    form: &MultiValueForm,
) -> Result<InstanceConfigUpdateRequest, &'static str> {
    let message = if form.bool_value("notice_clear") {
        None
    } else {
        form.clean("notice_message")
    };
    if message
        .as_ref()
        .is_some_and(|value| value.chars().count() > NOTICE_MAX_CHARS)
    {
        return Err("The notice must be 1000 characters or fewer");
    }
    let url = form.clean("notice_url");
    if url
        .as_deref()
        .is_some_and(|value| !value.starts_with("https://") && !value.starts_with("http://"))
    {
        return Err("The notice link must start with https:// or http://");
    }
    let starts_at = parse_notice_time(form, "notice_starts_at")?;
    let ends_at = parse_notice_time(form, "notice_ends_at")?;
    if let (Some(start), Some(end)) = (&starts_at, &ends_at)
        && end <= start
    {
        return Err("The notice must end after it starts");
    }
    Ok(InstanceConfigUpdateRequest {
        notice: Some(InstanceNoticeUpdateRequest {
            message,
            url,
            starts_at,
            ends_at,
        }),
        ..Default::default()
    })
}
