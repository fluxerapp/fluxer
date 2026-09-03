// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    asset_hash::AssetHash,
    constants::{DEFAULT_IMAGE_SIZE, parse_image_size},
    image_quality::ImageQuality,
    image_transform::EncodeEffort,
    query::Query,
};

#[test]
fn asset_hash_parsing_stays_lenient_about_the_digest_shape() {
    for raw in [
        "",
        "deadbee",
        "deadbeef0",
        "DEADBEEF",
        "gggggggg",
        "0123abcd",
        "a-deadbeef",
    ] {
        let parsed = AssetHash::parse(raw);
        assert_eq!(parsed.digest(), raw, "the digest rewrote {raw}");
        assert!(!parsed.is_animated(), "{raw} was read as animated");
    }
    for (raw, digest) in [
        ("a_deadbeef", "deadbeef"),
        ("a_DEADBEEF", "DEADBEEF"),
        ("a_gggggggg", "gggggggg"),
        ("a_", ""),
    ] {
        let parsed = AssetHash::parse(raw);
        assert_eq!(parsed.digest(), digest, "the digest rewrote {raw}");
        assert!(parsed.is_animated(), "{raw} lost its animation prefix");
    }
}

#[test]
fn malformed_query_values_fall_back_instead_of_failing_the_request() {
    for raw in ["", "-1", "1e3", "99999999999", "128px", " 128"] {
        assert_eq!(
            parse_image_size(Some(raw)),
            DEFAULT_IMAGE_SIZE,
            "size {raw} did not fall back"
        );
    }
    // A value that parses is snapped up the ladder, never dropped to the default: the old era
    // pinned this so an off-ladder request can never be served fewer pixels than it asked for.
    assert_eq!(parse_image_size(Some("0")), 16);
    assert_eq!(parse_image_size(Some("777")), 1024);
    assert_eq!(parse_image_size(Some("256")), 256);
    assert_eq!(parse_image_size(None), DEFAULT_IMAGE_SIZE);

    for raw in ["", " ", "LOW", "lossy", "%FF", "high "] {
        assert_eq!(
            ImageQuality::parse_lenient(raw),
            ImageQuality::High,
            "quality {raw} did not fall back"
        );
    }
    assert_eq!(ImageQuality::parse_lenient("low"), ImageQuality::Low);

    for raw in ["", "-1", "nine", "10.5", "256"] {
        assert_eq!(
            EncodeEffort::parse_lenient(raw),
            None,
            "effort {raw} was accepted"
        );
    }
    assert_eq!(
        EncodeEffort::parse_lenient("250").map(EncodeEffort::get),
        Some(9)
    );

    let query = Query::parse("animated=yes&download=TRUE&passthrough=1&empty=");
    assert!(!query.bool_value("animated", false));
    assert!(!query.bool_value("animated", true));
    assert!(query.bool_value("download", false));
    assert!(query.bool_value("passthrough", false));
    assert!(!query.bool_value("empty", true));
    assert!(query.bool_value("missing", true));
    assert!(!query.bool_value("missing", false));
}
