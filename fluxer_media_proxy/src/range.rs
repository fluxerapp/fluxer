// SPDX-License-Identifier: AGPL-3.0-or-later

pub use fluxer_common::http_range::{ByteRange, RangeSelection, parse_decimal_usize, parse_range};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ContentRange {
    pub start: usize,
    pub end: usize,
    pub size: Option<usize>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RequestRange<'a> {
    Absent,
    Forwardable(&'a str),
    Unsatisfiable,
}

pub fn classify_request_range(header: Option<&str>) -> RequestRange<'_> {
    let Some(raw) = header else {
        return RequestRange::Absent;
    };
    let trimmed = raw.trim_matches([' ', '\t']);
    let Some(spec) = trimmed.strip_prefix("bytes=") else {
        return RequestRange::Absent;
    };
    if spec.contains(',') {
        return RequestRange::Absent;
    }
    let Some(dash) = spec.find('-') else {
        return RequestRange::Absent;
    };
    let start_part = &spec[..dash];
    let end_part = &spec[dash + 1..];
    if start_part.is_empty() && end_part.is_empty() {
        return RequestRange::Absent;
    }
    if start_part.is_empty() {
        return match end_part.parse::<usize>() {
            Ok(0) => RequestRange::Unsatisfiable,
            Ok(_) => RequestRange::Forwardable(trimmed),
            Err(_) => RequestRange::Absent,
        };
    }
    let Ok(start) = start_part.parse::<usize>() else {
        return RequestRange::Absent;
    };
    if end_part.is_empty() {
        return RequestRange::Forwardable(trimmed);
    }
    match end_part.parse::<usize>() {
        Ok(end) if end < start => RequestRange::Unsatisfiable,
        Ok(_) => RequestRange::Forwardable(trimmed),
        Err(_) => RequestRange::Absent,
    }
}

pub fn parse_unsatisfiable_content_range(header: Option<&str>) -> Option<usize> {
    let raw = header?;
    let spec = raw.trim_matches([' ', '\t']).strip_prefix("bytes ")?;
    let size_part = spec.strip_prefix('*')?.strip_prefix('/')?;
    size_part.trim_matches([' ', '\t']).parse::<usize>().ok()
}

pub fn parse_content_range(header: Option<&str>) -> Option<ContentRange> {
    let raw = header?.trim_matches([' ', '\t']);
    let (unit, spec) = raw.split_once(' ')?;
    if !unit.eq_ignore_ascii_case("bytes") {
        return None;
    }
    let dash = spec.find('-')?;
    let range_end_start = dash.checked_add(1)?;
    let slash = spec[range_end_start..]
        .find('/')?
        .checked_add(range_end_start)?;
    let start = parse_decimal_usize(spec[..dash].trim_matches([' ', '\t']))?;
    let end = parse_decimal_usize(spec[range_end_start..slash].trim_matches([' ', '\t']))?;
    if end < start {
        return None;
    }
    let size_start = slash.checked_add(1)?;
    let size_part = spec[size_start..].trim_matches([' ', '\t']);
    if size_part.is_empty() {
        return None;
    }
    if size_part == "*" {
        return Some(ContentRange {
            start,
            end,
            size: None,
        });
    }
    let size = parse_decimal_usize(size_part)?;
    if size == 0 || end >= size {
        return None;
    }
    Some(ContentRange {
        start,
        end,
        size: Some(size),
    })
}
