// SPDX-License-Identifier: AGPL-3.0-or-later

use super::*;

const WHATWG_JAVASCRIPT_ESSENCES: [&str; 16] = [
    "application/ecmascript",
    "application/javascript",
    "application/x-ecmascript",
    "application/x-javascript",
    "text/ecmascript",
    "text/javascript",
    "text/javascript1.0",
    "text/javascript1.1",
    "text/javascript1.2",
    "text/javascript1.3",
    "text/javascript1.4",
    "text/javascript1.5",
    "text/jscript",
    "text/livescript",
    "text/x-ecmascript",
    "text/x-javascript",
];

#[test]
fn javascript_content_type_matches_every_whatwg_essence() {
    for essence in WHATWG_JAVASCRIPT_ESSENCES {
        assert!(is_javascript_content_type(essence), "{essence}");
        assert!(
            is_javascript_content_type(&essence.to_ascii_uppercase()),
            "{essence} upper case"
        );
    }
}

#[test]
fn javascript_content_type_checks_every_comma_separated_member() {
    assert!(is_javascript_content_type("image/png, text/javascript"));
    assert!(is_javascript_content_type("text/javascript, image/png"));
    assert!(!is_javascript_content_type(
        "video/mp4; codecs=\"avc1.42E01E, mp4a.40.2\""
    ));
}

#[test]
fn javascript_content_type_ends_the_essence_where_browsers_do() {
    for content_type in [
        "text/javascript x",
        "text/javascript\tx",
        "text/javascript(x)",
        "TEXT/JAVASCRIPT (x)",
        " text/javascript ;charset=x",
        "\ttext/javascript\t;goal=module",
        "application/x-javascript(",
        "text/jscript x;charset=utf-8",
    ] {
        assert!(is_javascript_content_type(content_type), "{content_type:?}");
    }
}
