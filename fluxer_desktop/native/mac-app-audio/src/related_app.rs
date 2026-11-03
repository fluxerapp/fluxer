// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn trim_ascii_whitespace(value: &str) -> &str {
    value.trim_matches(|c: char| matches!(c, ' ' | '\t' | '\r' | '\n'))
}

pub fn has_prefix_with_suffix(value: &str, prefix: &str, suffix: &str) -> bool {
    let pb = prefix.as_bytes();
    let sb = suffix.as_bytes();
    let vb = value.as_bytes();
    if vb.len() < pb.len() + sb.len() {
        return false;
    }
    &vb[..pb.len()] == pb && &vb[pb.len()..pb.len() + sb.len()] == sb
}

pub fn related_by_prefix_either_way(a: &str, b: &str, suffix: &str) -> bool {
    has_prefix_with_suffix(a, b, suffix) || has_prefix_with_suffix(b, a, suffix)
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
    if !target_bundle.is_empty() && !candidate_bundle.is_empty() {
        let tb = helper_bundle_base(target_bundle);
        let cb = helper_bundle_base(candidate_bundle);
        if candidate_bundle == target_bundle
            || cb == tb
            || related_by_prefix_either_way(candidate_bundle, target_bundle, ".")
            || related_by_prefix_either_way(candidate_bundle, target_bundle, "-")
        {
            return true;
        }
    }
    let tn = trim_ascii_whitespace(target_name);
    let cn = trim_ascii_whitespace(candidate_name);
    if !tn.is_empty() {
        let tnb = helper_name_base(tn);
        let cnb = helper_name_base(cn);
        if cn == tn
            || cnb == tnb
            || related_by_prefix_either_way(cn, tn, " ")
            || related_by_prefix_either_way(cn, tn, " Helper")
        {
            return true;
        }
    }
    false
}
