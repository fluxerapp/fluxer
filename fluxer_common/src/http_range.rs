// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ByteRange {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum RangeSelection {
    #[default]
    Full,
    Partial(ByteRange),
    Unsatisfiable,
}

pub fn parse_range(header: Option<&str>, file_size: usize) -> RangeSelection {
    let Some(raw) = header else {
        return RangeSelection::Full;
    };
    let trimmed = raw.trim_matches([' ', '\t']);
    // The unit is matched the way classify_request_range matches it, which is what decides
    // whether a GET forwards the Range upstream at all. Accepting more units here alone would
    // answer 206 on a HEAD whose GET serves the whole body.
    let Some(spec) = trimmed.strip_prefix("bytes=") else {
        return RangeSelection::Full;
    };
    if spec.contains(',') {
        return RangeSelection::Full;
    }
    let Some(dash) = spec.find('-') else {
        return RangeSelection::Full;
    };
    let end_offset = dash
        .checked_add(1)
        .expect("range delimiter offset must fit usize");
    let start_part = &spec[..dash];
    let end_part = &spec[end_offset..];
    if start_part.is_empty() && end_part.is_empty() {
        return RangeSelection::Full;
    }
    // A range-spec that does not parse is ignored outright and the full resource is served
    // (RFC 9110 s14.2). That check has to happen before the zero-length shortcut below, or a
    // malformed spec against an empty object reports 416 instead of 200.
    if !start_part.is_empty() {
        let Some(start) = parse_decimal_usize(start_part) else {
            return RangeSelection::Full;
        };
        let parsed_end = if end_part.is_empty() {
            None
        } else if let Some(end) = parse_decimal_usize(end_part) {
            Some(end)
        } else {
            return RangeSelection::Full;
        };
        if file_size == 0 {
            return RangeSelection::Unsatisfiable;
        }
        let last_byte = file_size
            .checked_sub(1)
            .expect("nonempty media resource must have a last byte");
        let requested_end = parsed_end.unwrap_or(last_byte);
        if start >= file_size || requested_end < start {
            return RangeSelection::Unsatisfiable;
        }
        return RangeSelection::Partial(ByteRange {
            start,
            end: requested_end.min(last_byte),
        });
    }
    let Some(suffix_len) = parse_decimal_usize(end_part) else {
        return RangeSelection::Full;
    };
    if file_size == 0 {
        return RangeSelection::Unsatisfiable;
    }
    let last_byte = file_size
        .checked_sub(1)
        .expect("nonempty media resource must have a last byte");
    if suffix_len == 0 {
        return RangeSelection::Unsatisfiable;
    }
    let resolved_len = suffix_len.min(file_size);
    RangeSelection::Partial(ByteRange {
        start: file_size
            .checked_sub(resolved_len)
            .expect("resolved suffix length must not exceed the media resource"),
        end: last_byte,
    })
}

pub fn parse_decimal_usize(raw: &str) -> Option<usize> {
    if raw.is_empty() || !raw.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    raw.parse().ok()
}
