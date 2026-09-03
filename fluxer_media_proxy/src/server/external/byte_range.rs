// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::range;
use axum::http::HeaderValue;

#[derive(Clone, Copy)]
pub(super) enum ExternalRangeSelection<'a> {
    Bounded { start: usize, end: usize },
    From { start: usize },
    Suffix { length: usize },
    Verbatim { spec: &'a str },
}

impl ExternalRangeSelection<'_> {
    pub(super) fn header_value(self) -> HeaderValue {
        let value = match self {
            Self::Bounded { start, end } => format!("bytes={start}-{end}"),
            Self::From { start } => format!("bytes={start}-"),
            Self::Suffix { length } => format!("bytes=-{length}"),
            Self::Verbatim { spec } => format!("bytes={spec}"),
        };
        HeaderValue::from_str(&value).expect("a parsed external byte range is a valid header")
    }

    fn matches(self, actual: range::ContentRange, actual_length: usize) -> bool {
        match (self, actual.size) {
            (Self::Verbatim { .. }, _) => true,
            (Self::Bounded { start, end }, Some(size)) => {
                start < size && actual.start == start && actual.end <= end.min(size - 1)
            }
            (Self::Bounded { start, end }, None) => actual.start == start && actual.end <= end,
            (Self::From { start }, Some(size)) => {
                start < size && actual.start == start && actual.end < size
            }
            (Self::From { start }, None) => actual.start == start,
            (Self::Suffix { length }, Some(size)) => {
                actual.start == size.saturating_sub(length) && actual.end == size - 1
            }
            (Self::Suffix { length }, None) => {
                actual_length == length || (actual.start == 0 && actual_length < length)
            }
        }
    }
}

#[derive(Clone)]
pub(super) enum ExternalPartial {
    Validated {
        content_range: range::ContentRange,
        content_length: u64,
    },
    Forwarded {
        content_range: Option<HeaderValue>,
    },
}

impl ExternalPartial {
    pub(super) fn content_length(&self) -> Option<u64> {
        match self {
            Self::Validated { content_length, .. } => Some(*content_length),
            Self::Forwarded { .. } => None,
        }
    }

    pub(super) fn header_value(&self) -> Option<HeaderValue> {
        match self {
            Self::Validated { content_range, .. } => {
                let value = match content_range.size {
                    Some(size) => {
                        format!("bytes {}-{}/{size}", content_range.start, content_range.end)
                    }
                    None => format!("bytes {}-{}/*", content_range.start, content_range.end),
                };
                Some(
                    HeaderValue::from_str(&value)
                        .expect("a validated external content range is a valid header"),
                )
            }
            Self::Forwarded { content_range } => content_range.clone(),
        }
    }
}

pub(super) fn parse_external_requested_range(header: &str) -> Option<ExternalRangeSelection<'_>> {
    let header = header.trim_matches([' ', '\t']);
    let (unit, spec) = header.split_once('=')?;
    if !unit.eq_ignore_ascii_case("bytes") {
        return None;
    }
    parse_external_range_spec(spec).or_else(|| forwardable_external_range_spec(spec))
}

fn parse_external_range_spec(spec: &str) -> Option<ExternalRangeSelection<'_>> {
    if spec.is_empty() || spec.contains(',') {
        return None;
    }
    let (start, end) = spec.split_once('-')?;
    match (
        range::parse_decimal_usize(start),
        range::parse_decimal_usize(end),
    ) {
        (Some(start), Some(end)) if end >= start => {
            Some(ExternalRangeSelection::Bounded { start, end })
        }
        (Some(start), None) if end.is_empty() => Some(ExternalRangeSelection::From { start }),
        (None, Some(length)) if start.is_empty() && length > 0 => {
            Some(ExternalRangeSelection::Suffix { length })
        }
        _ => None,
    }
}

fn forwardable_external_range_spec(spec: &str) -> Option<ExternalRangeSelection<'_>> {
    if spec.is_empty() || !spec.bytes().all(|byte| byte.is_ascii_graphic()) {
        return None;
    }
    Some(ExternalRangeSelection::Verbatim { spec })
}

pub(super) fn validate_external_partial(
    requested_range: Option<ExternalRangeSelection<'_>>,
    content_range: Option<&str>,
    content_length: Option<u64>,
    max_media_proxy_bytes: usize,
) -> Option<ExternalPartial> {
    let requested = requested_range?;
    if let ExternalRangeSelection::Verbatim { .. } = requested {
        return Some(ExternalPartial::Forwarded {
            content_range: content_range.and_then(|raw| HeaderValue::from_str(raw).ok()),
        });
    }
    let actual = range::parse_content_range(content_range)?;
    let actual_length = actual
        .end
        .checked_sub(actual.start)
        .and_then(|length| length.checked_add(1))?;
    if actual_length > max_media_proxy_bytes
        || content_length.is_some_and(|length| length != actual_length as u64)
        || !requested.matches(actual, actual_length)
    {
        return None;
    }
    Some(ExternalPartial::Validated {
        content_range: actual,
        content_length: u64::try_from(actual_length).ok()?,
    })
}
