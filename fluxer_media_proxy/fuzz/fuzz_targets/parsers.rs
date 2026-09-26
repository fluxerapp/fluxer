#![no_main]

// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_media_proxy::{disposition, mime, percent_decode, public_net_policy, query, range};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let _ = mime::sniff(data);
    let _ = mime::sniff_prefix(data, data.len());
    let _ = mime::sniff_prefix(data, data.len().saturating_mul(2).saturating_add(1));
    if let Ok(text) = std::str::from_utf8(data) {
        let _ = range::parse_range(Some(text), data.len().saturating_mul(3).saturating_add(1));
        let _ = range::parse_bounded_request_range(Some(text), 1024 * 1024);
        let _ = range::parse_content_range(Some(text));
        let _ = query::split_target(text);
        let _ = query::Query::parse(text);
        let _ = public_net_policy::parse_url(text);
        let _ = public_net_policy::resolve_redirect("https://example.com/a/b/c?x=1", text);
        let _ = percent_decode::decode_utf8(text);
        let _ = disposition::is_inline_viewable(text);
        let inline = disposition::decide(text, false);
        let attachment = disposition::decide(text, true);
        let _ = disposition::header(inline, Some(text));
        let _ = disposition::header(attachment, Some(text));
        let _ = disposition::header(attachment, None);
    }
});
