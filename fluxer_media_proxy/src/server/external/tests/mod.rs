// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{
    fetch::external_streamed_source_is_svg,
    response::{ExternalStreamDecision, external_stream_decision},
};

#[test]
fn a_mislabeled_svg_body_never_takes_the_external_streaming_path() {
    let svg = b"<svg xmlns=\"http://www.w3.org/2000/svg\"><script/></svg>";
    assert_eq!(
        ExternalStreamDecision::Stream(Some(svg.len() as u64)),
        external_stream_decision(true, Some(svg.len() as u64), "image/png"),
        "the declared headers alone still look streamable"
    );
    assert!(
        external_streamed_source_is_svg(svg, "logo.png"),
        "an SVG body behind an image/png label must be rasterized, not proxied through"
    );
    assert!(
        external_streamed_source_is_svg(b"\x89PNG\r\n\x1a\n", "logo.svg"),
        "a .svg final url is refused even when the bytes are not SVG"
    );
    assert!(!external_streamed_source_is_svg(
        b"\x89PNG\r\n\x1a\n",
        "logo.png"
    ));
    assert!(!external_streamed_source_is_svg(b"", "clip.mp4"));
}
