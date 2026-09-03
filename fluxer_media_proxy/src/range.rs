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

pub fn parse_bounded_request_range(header: Option<&str>, max_len: usize) -> Option<ByteRange> {
    let raw = header?.trim_matches([' ', '\t']);
    let spec = raw.strip_prefix("bytes=")?;
    if spec.contains(',') {
        return None;
    }
    let dash = spec.find('-')?;
    let end_offset = dash.checked_add(1)?;
    let start = parse_decimal_usize(spec[..dash].trim_matches([' ', '\t']))?;
    let end = parse_decimal_usize(spec[end_offset..].trim_matches([' ', '\t']))?;
    if end < start {
        return None;
    }
    let len = end.checked_sub(start)?.checked_add(1)?;
    if len > max_len {
        return None;
    }
    Some(ByteRange { start, end })
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

pub(crate) fn parse_decimal_usize(raw: &str) -> Option<usize> {
    if raw.is_empty() || !raw.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    raw.parse().ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::ADVERSARIAL_RANGE_HEADERS;

    #[test]
    fn range_parser() {
        assert_eq!(
            RangeSelection::Partial(ByteRange { start: 0, end: 9 }),
            parse_range(Some("bytes=0-9"), 100)
        );
        assert_eq!(
            RangeSelection::Partial(ByteRange { start: 95, end: 99 }),
            parse_range(Some("bytes=-5"), 100)
        );
        assert_eq!(
            RangeSelection::Unsatisfiable,
            parse_range(Some("bytes=100-200"), 100)
        );
    }

    #[test]
    fn open_ended_range() {
        assert_eq!(
            RangeSelection::Partial(ByteRange { start: 50, end: 99 }),
            parse_range(Some("bytes=50-"), 100)
        );
    }

    #[test]
    fn suffix_larger_than_file_clamps_to_whole_file() {
        assert_eq!(
            RangeSelection::Partial(ByteRange { start: 0, end: 99 }),
            parse_range(Some("bytes=-9999"), 100)
        );
    }

    #[test]
    fn zero_length_suffix_is_unsatisfiable() {
        assert_eq!(
            RangeSelection::Unsatisfiable,
            parse_range(Some("bytes=-0"), 100)
        );
    }

    #[test]
    fn a_malformed_spec_is_ignored_even_against_a_zero_length_object() {
        // RFC 9110 s14.2: an unparsable range-spec is ignored and the full resource is served.
        // The zero-length shortcut must not pre-empt that check. The old era reached the same
        // answer on GET by classifying the spec before it ever consulted the length; only its
        // HEAD branch went through this parser and reported 416, which is the bug being closed.
        for raw in [
            "bytes=abc-def",
            "bytes=--",
            "bytes=1-2-3",
            "bytes=x-y",
            "bytes=-abc",
        ] {
            assert_eq!(
                RangeSelection::Full,
                parse_range(Some(raw), 0),
                "{raw} against an empty object must be ignored, not 416"
            );
            assert_eq!(
                RangeSelection::Full,
                parse_range(Some(raw), 100),
                "{raw} against a sized object must be ignored"
            );
        }
        // A well-formed spec against an empty object stays unsatisfiable.
        for raw in ["bytes=0-9", "bytes=5-", "bytes=-5", "bytes=10-5"] {
            assert_eq!(
                RangeSelection::Unsatisfiable,
                parse_range(Some(raw), 0),
                "{raw} against an empty object must stay unsatisfiable"
            );
        }
    }

    #[test]
    fn missing_or_malformed_range_falls_through_to_no_range() {
        assert_eq!(RangeSelection::Full, parse_range(None, 100));
        assert_eq!(RangeSelection::Full, parse_range(Some("rows=0-9"), 100));
        assert_eq!(RangeSelection::Full, parse_range(Some("bytes="), 100));
        assert_eq!(
            RangeSelection::Full,
            parse_range(Some("bytes=abc-def"), 100)
        );
        assert_eq!(RangeSelection::Full, parse_range(Some("bytes=1"), 100));
        assert_eq!(RangeSelection::Full, parse_range(Some("bytes= x-y"), 100));
    }

    #[test]
    fn multi_range_not_supported() {
        assert_eq!(
            RangeSelection::Full,
            parse_range(Some("bytes=0-1, 2-3"), 100)
        );
    }

    #[test]
    fn reversed_start_end_is_unsatisfiable() {
        assert_eq!(
            RangeSelection::Unsatisfiable,
            parse_range(Some("bytes=10-5"), 100)
        );
    }

    #[test]
    fn empty_file_is_unsatisfiable_for_any_byte_range() {
        assert_eq!(
            RangeSelection::Unsatisfiable,
            parse_range(Some("bytes=0-9"), 0)
        );
        assert_eq!(
            RangeSelection::Unsatisfiable,
            parse_range(Some("bytes=-5"), 0)
        );
    }

    #[test]
    fn end_past_eof_clamps() {
        assert_eq!(
            RangeSelection::Partial(ByteRange { start: 0, end: 99 }),
            parse_range(Some("bytes=0-9999"), 100)
        );
    }

    #[test]
    fn a_request_range_unit_is_matched_exactly_but_a_response_unit_is_not() {
        // Every request-side parser has to agree with classify_request_range, which forwards
        // only a lowercase "bytes=" spec. A HEAD that read an uppercase unit as a range would
        // report 206 and a partial length for an object its own GET serves whole.
        assert_eq!(RangeSelection::Full, parse_range(Some("BYTES=0-1"), 100));
        assert_eq!(RangeSelection::Full, parse_range(Some("Bytes=0-1"), 100));
        assert_eq!(RangeSelection::Full, parse_range(Some("items=0-1"), 100));
        assert_eq!(
            RequestRange::Absent,
            classify_request_range(Some("BYTES=0-1"))
        );
        assert_eq!(None, parse_bounded_request_range(Some("BYTES=0-1"), 32));
        // The response side reads the object store's own reply rather than client input, so a
        // lenient unit there only rescues a Content-Range the strict match would drop.
        assert_eq!(
            Some(ContentRange {
                start: 0,
                end: 1,
                size: Some(2)
            }),
            parse_content_range(Some("Bytes 0-1/2"))
        );
        assert_eq!(None, parse_content_range(Some("items 0-1/2")));
    }

    #[test]
    fn only_ascii_digits_are_accepted_as_range_offsets() {
        // A deliberate divergence from origin/main, which parsed offsets with usize::from_str
        // and so accepted the leading sign that RFC 9110's first-pos = 1*DIGIT forbids. The old
        // era already served "bytes=+5-" whole on GET, because the spec reached the object store
        // verbatim and was ignored there, and only its HEAD branch answered 206. The strict
        // parse is what makes the two agree.
        assert_eq!(None, parse_decimal_usize(""));
        assert_eq!(None, parse_decimal_usize("+5"));
        assert_eq!(None, parse_decimal_usize(" 5"));
        assert_eq!(None, parse_decimal_usize("5_0"));
        assert_eq!(Some(50), parse_decimal_usize("50"));
        assert_eq!(RangeSelection::Full, parse_range(Some("bytes=+5-"), 100));
        assert_eq!(RangeSelection::Full, parse_range(Some("bytes=-+5"), 100));
        assert_eq!(
            RequestRange::Forwardable("bytes=+5-"),
            classify_request_range(Some("bytes=+5-")),
            "forwarding is unchanged: the object store still settles the spec it always saw"
        );
    }

    #[test]
    fn bounded_request_range_only_accepts_explicit_spans_within_cap() {
        assert_eq!(
            Some(ByteRange { start: 10, end: 19 }),
            parse_bounded_request_range(Some("bytes=10-19"), 32)
        );
        assert_eq!(None, parse_bounded_request_range(Some("bytes=10-"), 32));
        assert_eq!(None, parse_bounded_request_range(Some("bytes=-10"), 32));
        assert_eq!(None, parse_bounded_request_range(Some("bytes=10-9"), 32));
        assert_eq!(None, parse_bounded_request_range(Some("bytes=0-32"), 32));
        assert_eq!(
            None,
            parse_bounded_request_range(Some("bytes=0-1, 2-3"), 32)
        );
        assert_eq!(
            None,
            parse_bounded_request_range(Some("bytes=0-18446744073709551615"), usize::MAX)
        );
    }

    #[test]
    fn classified_request_ranges_agree_with_the_size_aware_parser() {
        for raw in ["bytes=0-9", "bytes=-5", "bytes=50-", "bytes=0-9999"] {
            assert_eq!(
                RequestRange::Forwardable(raw),
                classify_request_range(Some(raw)),
                "range={raw} must reach the upstream"
            );
            assert!(matches!(
                parse_range(Some(raw), 100),
                RangeSelection::Partial(_)
            ));
        }
        assert_eq!(
            RequestRange::Forwardable("bytes=0-9"),
            classify_request_range(Some(" bytes=0-9 "))
        );
        assert_eq!(
            RequestRange::Forwardable("bytes=100-200"),
            classify_request_range(Some("bytes=100-200")),
            "only the object size can settle a range that starts past the end"
        );
        for raw in ["bytes=10-5", "bytes=-0"] {
            assert_eq!(
                RequestRange::Unsatisfiable,
                classify_request_range(Some(raw)),
                "range={raw} is unsatisfiable at every size"
            );
            assert_eq!(RangeSelection::Unsatisfiable, parse_range(Some(raw), 100));
            assert_eq!(RangeSelection::Unsatisfiable, parse_range(Some(raw), 1));
        }
        for raw in [
            "rows=0-9",
            "bytes=",
            "bytes=abc-def",
            "bytes=0-abc",
            "bytes=0-1, 2-3",
            "bytes=0",
        ] {
            assert_eq!(
                RequestRange::Absent,
                classify_request_range(Some(raw)),
                "range={raw} must not reach the upstream"
            );
            assert_eq!(RangeSelection::Full, parse_range(Some(raw), 100));
        }
        assert_eq!(RequestRange::Absent, classify_request_range(None));
    }

    #[test]
    fn unsatisfiable_content_range_parser_reads_the_total() {
        assert_eq!(
            Some(100),
            parse_unsatisfiable_content_range(Some("bytes */100"))
        );
        assert_eq!(
            Some(0),
            parse_unsatisfiable_content_range(Some("bytes */0"))
        );
        assert_eq!(None, parse_unsatisfiable_content_range(Some("bytes */*")));
        assert_eq!(
            None,
            parse_unsatisfiable_content_range(Some("bytes 0-9/100"))
        );
        assert_eq!(None, parse_unsatisfiable_content_range(Some("*/100")));
        assert_eq!(None, parse_unsatisfiable_content_range(None));
    }

    #[test]
    fn content_range_parser_accepts_known_and_unknown_totals() {
        assert_eq!(
            Some(ContentRange {
                start: 0,
                end: 9,
                size: Some(100)
            }),
            parse_content_range(Some("bytes 0-9/100"))
        );
        assert_eq!(
            Some(ContentRange {
                start: 0,
                end: 9,
                size: None
            }),
            parse_content_range(Some("bytes 0-9/*"))
        );
        assert_eq!(None, parse_content_range(Some("bytes */100")));
        assert_eq!(None, parse_content_range(Some("bytes 10-9/100")));
        assert_eq!(None, parse_content_range(Some("bytes 0-100/100")));
        assert_eq!(None, parse_content_range(Some("bytes 0-0/0")));
        assert_eq!(None, parse_content_range(Some("bytes 0-0/")));
        assert_eq!(
            None,
            parse_content_range(Some("bytes 0-0/184467440737095516160"))
        );
    }

    #[test]
    fn range_parsing_stays_within_the_declared_file_size_on_adversarial_headers() {
        for header in ADVERSARIAL_RANGE_HEADERS {
            for file_size in [0_usize, 1, 100, usize::from(u16::MAX)] {
                if let RangeSelection::Partial(selected) = parse_range(Some(header), file_size) {
                    assert!(file_size > 0, "partial range for empty file from {header}");
                    assert!(
                        selected.start <= selected.end,
                        "inverted range from {header}"
                    );
                    assert!(selected.end < file_size, "range beyond file from {header}");
                }
                assert!(
                    parse_bounded_request_range(Some(header), file_size)
                        .is_none_or(|selected| selected.start <= selected.end
                            && selected.end - selected.start < file_size)
                );
            }
            if let Some(parsed) = parse_content_range(Some(header)) {
                assert!(
                    parsed.start <= parsed.end,
                    "inverted content range {header}"
                );
                if let Some(size) = parsed.size {
                    assert!(size > 0, "zero-sized content range {header}");
                    assert!(parsed.end < size, "content range beyond size {header}");
                }
            }
        }
    }
}
