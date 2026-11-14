// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    disposition::{self, Decision, PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES},
    test_fixtures::ADVERSARIAL_TEXT_INPUTS,
};

#[test]
fn adversarial_filenames_never_break_the_content_disposition_header() {
    for text in ADVERSARIAL_TEXT_INPUTS {
        for (decision, directive) in [
            (Decision::Inline, "inline"),
            (Decision::Attachment, "attachment"),
        ] {
            let value = disposition::header(decision, Some(text))
                .expect("disposition header")
                .into_header_value();
            let rendered = value.to_str().expect("the disposition header is ascii");
            assert!(
                rendered.len() <= PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES,
                "unbounded disposition for {text}"
            );
            assert!(
                !rendered.contains(['\r', '\n', '\0']),
                "the disposition for {text} can split the header block"
            );
            assert!(
                rendered.starts_with(directive),
                "the disposition for {text} lost its directive"
            );
            if text.is_empty() {
                assert_eq!(rendered, directive);
                continue;
            }
            assert_eq!(
                rendered.matches("; filename=\"").count(),
                1,
                "the disposition for {text} repeated the filename parameter"
            );
            let quoted = rendered
                .strip_prefix(directive)
                .and_then(|rest| rest.strip_prefix("; filename=\""))
                .and_then(|rest| rest.split('"').next())
                .expect("a terminated quoted filename");
            assert!(
                !quoted.contains(['\\', '/']),
                "the ascii fallback for {text} kept a path or escape character"
            );
        }
    }
}
