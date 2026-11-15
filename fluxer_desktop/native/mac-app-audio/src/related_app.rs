// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn trim_ascii_whitespace(value: &str) -> &str {
    value.trim_matches(|c: char| matches!(c, ' ' | '\t' | '\r' | '\n'))
}

pub fn helper_bundle_base(value: &str) -> &str {
    let suffixes = [".helper", ".Helper", "-helper", "-Helper"];
    for suffix in suffixes {
        if let Some(idx) = value.rfind(suffix) {
            if idx == 0 {
                continue;
            }

            if value[..idx].find('.').is_none() {
                continue;
            }
            let after = idx + suffix.len();
            let bytes = value.as_bytes();
            if after == bytes.len() || bytes[after] == b'.' || bytes[after] == b'-' {
                return &value[..idx];
            }
        }
    }
    value
}

pub fn helper_name_base(value: &str) -> &str {
    let trimmed = trim_ascii_whitespace(value);
    if let Some(idx) = trimmed.find(" Helper") {
        if idx > 0 {
            return trim_ascii_whitespace(&trimmed[..idx]);
        }
    }
    trimmed
}

pub fn looks_related_by_strings(
    candidate_bundle: &str,
    target_bundle: &str,
    candidate_name: &str,
    target_name: &str,
) -> bool {
    if !target_bundle.is_empty()
        && !candidate_bundle.is_empty()
        && helper_bundle_base(candidate_bundle) == helper_bundle_base(target_bundle)
    {
        return true;
    }
    let tn = trim_ascii_whitespace(target_name);
    !tn.is_empty() && helper_name_base(candidate_name) == helper_name_base(tn)
}

#[cfg(test)]
mod tests {
    use super::looks_related_by_strings;

    #[test]
    fn helpers_are_related_to_their_app() {
        assert!(looks_related_by_strings(
            "com.google.Chrome.helper",
            "com.google.Chrome",
            "Google Chrome Helper",
            "Google Chrome",
        ));
        assert!(looks_related_by_strings(
            "com.google.Chrome.helper.renderer",
            "com.google.Chrome",
            "Google Chrome Helper (Renderer)",
            "Google Chrome",
        ));
        assert!(looks_related_by_strings(
            "",
            "com.google.Chrome",
            "Google Chrome Helper (Renderer)",
            "Google Chrome",
        ));
    }

    #[test]
    fn sibling_builds_are_not_related() {
        assert!(!looks_related_by_strings(
            "com.google.Chrome.canary",
            "com.google.Chrome",
            "Google Chrome Canary",
            "Google Chrome",
        ));
        assert!(!looks_related_by_strings(
            "com.microsoft.VSCodeInsiders",
            "com.microsoft.VSCode",
            "Code - Insiders",
            "Code",
        ));
        assert!(!looks_related_by_strings(
            "app.fluxer.canary",
            "app.fluxer",
            "Fluxer Canary",
            "Fluxer",
        ));
        assert!(!looks_related_by_strings(
            "org.mozilla.nightly",
            "org.mozilla.firefox",
            "Firefox Nightly",
            "Firefox",
        ));
    }
}
