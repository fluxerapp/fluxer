// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ByteRange {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ParsedRange {
    pub range: Option<ByteRange>,
    pub unsatisfiable: bool,
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

pub fn parse_range(header: Option<&str>, file_size: usize) -> ParsedRange {
    let Some(raw) = header else {
        return ParsedRange::default();
    };
    let trimmed = raw.trim_matches([' ', '\t']);
    let Some(spec) = trimmed.strip_prefix("bytes=") else {
        return ParsedRange::default();
    };
    if spec.contains(',') {
        return ParsedRange::default();
    }
    let Some(dash) = spec.find('-') else {
        return ParsedRange::default();
    };
    let start_part = &spec[..dash];
    let end_part = &spec[dash + 1..];
    if start_part.is_empty() && end_part.is_empty() {
        return ParsedRange::default();
    }
    if file_size == 0 {
        return ParsedRange {
            range: None,
            unsatisfiable: true,
        };
    }
    if !start_part.is_empty() {
        let Ok(start) = start_part.parse::<usize>() else {
            return ParsedRange::default();
        };
        let requested_end = if end_part.is_empty() {
            file_size - 1
        } else if let Ok(end) = end_part.parse::<usize>() {
            end
        } else {
            return ParsedRange::default();
        };
        if start >= file_size || requested_end < start {
            return ParsedRange {
                range: None,
                unsatisfiable: true,
            };
        }
        return ParsedRange {
            range: Some(ByteRange {
                start,
                end: requested_end.min(file_size - 1),
            }),
            unsatisfiable: false,
        };
    }
    let Ok(suffix_len) = end_part.parse::<usize>() else {
        return ParsedRange::default();
    };
    if suffix_len == 0 {
        return ParsedRange {
            range: None,
            unsatisfiable: true,
        };
    }
    let resolved_len = suffix_len.min(file_size);
    ParsedRange {
        range: Some(ByteRange {
            start: file_size - resolved_len,
            end: file_size - 1,
        }),
        unsatisfiable: false,
    }
}

pub fn parse_bounded_request_range(header: Option<&str>, max_len: usize) -> Option<ByteRange> {
    let raw = header?;
    let spec = raw.trim_matches([' ', '\t']).strip_prefix("bytes=")?;
    if spec.contains(',') {
        return None;
    }
    let dash = spec.find('-')?;
    let start_part = spec[..dash].trim_matches([' ', '\t']);
    let end_part = spec[dash + 1..].trim_matches([' ', '\t']);
    if start_part.is_empty() || end_part.is_empty() {
        return None;
    }
    let start = start_part.parse::<usize>().ok()?;
    let end = end_part.parse::<usize>().ok()?;
    if end < start {
        return None;
    }
    let len = end - start + 1;
    if len == 0 || len > max_len {
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
    let raw = header?;
    let spec = raw.trim_matches([' ', '\t']).strip_prefix("bytes ")?;
    let dash = spec.find('-')?;
    let slash = spec[dash + 1..].find('/')? + dash + 1;
    let start = spec[..dash]
        .trim_matches([' ', '\t'])
        .parse::<usize>()
        .ok()?;
    let end = spec[dash + 1..slash]
        .trim_matches([' ', '\t'])
        .parse::<usize>()
        .ok()?;
    if end < start {
        return None;
    }
    let size_part = spec[slash + 1..].trim_matches([' ', '\t']);
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
    let size = size_part.parse::<usize>().ok()?;
    if size == 0 || end >= size {
        return None;
    }
    Some(ContentRange {
        start,
        end,
        size: Some(size),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn range_parser() {
        assert_eq!(
            Some(ByteRange { start: 0, end: 9 }),
            parse_range(Some("bytes=0-9"), 100).range
        );
        assert_eq!(
            Some(ByteRange { start: 95, end: 99 }),
            parse_range(Some("bytes=-5"), 100).range
        );
        assert!(parse_range(Some("bytes=100-200"), 100).unsatisfiable);
    }

    #[test]
    fn open_ended_range() {
        assert_eq!(
            Some(ByteRange { start: 50, end: 99 }),
            parse_range(Some("bytes=50-"), 100).range
        );
    }

    #[test]
    fn suffix_larger_than_file_clamps_to_whole_file() {
        assert_eq!(
            Some(ByteRange { start: 0, end: 99 }),
            parse_range(Some("bytes=-9999"), 100).range
        );
    }

    #[test]
    fn zero_length_suffix_is_unsatisfiable() {
        assert!(parse_range(Some("bytes=-0"), 100).unsatisfiable);
    }

    #[test]
    fn missing_or_malformed_range_falls_through_to_no_range() {
        assert_eq!(None, parse_range(None, 100).range);
        assert!(!parse_range(None, 100).unsatisfiable);
        assert_eq!(None, parse_range(Some("rows=0-9"), 100).range);
        assert_eq!(None, parse_range(Some("bytes="), 100).range);
        assert_eq!(None, parse_range(Some("bytes=abc-def"), 100).range);
    }

    #[test]
    fn multi_range_not_supported() {
        assert_eq!(None, parse_range(Some("bytes=0-1, 2-3"), 100).range);
    }

    #[test]
    fn reversed_start_end_is_unsatisfiable() {
        assert!(parse_range(Some("bytes=10-5"), 100).unsatisfiable);
    }

    #[test]
    fn empty_file_is_unsatisfiable_for_any_byte_range() {
        assert!(parse_range(Some("bytes=0-9"), 0).unsatisfiable);
        assert!(parse_range(Some("bytes=-5"), 0).unsatisfiable);
    }

    #[test]
    fn end_past_eof_clamps() {
        assert_eq!(
            Some(ByteRange { start: 0, end: 99 }),
            parse_range(Some("bytes=0-9999"), 100).range
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
    }

    #[test]
    fn classified_request_ranges_agree_with_the_size_aware_parser() {
        for raw in ["bytes=0-9", "bytes=-5", "bytes=50-", "bytes=0-9999"] {
            assert_eq!(
                RequestRange::Forwardable(raw),
                classify_request_range(Some(raw)),
                "range={raw} must reach the upstream"
            );
            assert!(parse_range(Some(raw), 100).range.is_some());
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
            assert!(parse_range(Some(raw), 100).unsatisfiable);
            assert!(parse_range(Some(raw), 1).unsatisfiable);
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
            assert_eq!(None, parse_range(Some(raw), 100).range);
            assert!(!parse_range(Some(raw), 100).unsatisfiable);
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
    }
}
