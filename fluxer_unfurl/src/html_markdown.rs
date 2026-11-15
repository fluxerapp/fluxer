// SPDX-License-Identifier: AGPL-3.0-or-later

use regex::{Captures, Regex};
use std::sync::LazyLock;

struct WrappedTag {
    re: Regex,
    prefix: &'static str,
    suffix: &'static str,
}

static SIMPLE_REPLACEMENTS: LazyLock<Vec<(Regex, &'static str)>> = LazyLock::new(|| {
    [
        (r"(?i)<p>", "\n\n"),
        (r"(?i)</p>", ""),
        (r"(?i)<br\s*/?>", "\n"),
        (r"(?i)<h[1-6]>", "\n\n**"),
        (r"(?i)</h[1-6]>", "**\n\n"),
        (r"(?i)<li>", "• "),
        (r"(?i)</li>", "\n"),
        (r"(?i)<ul>|<ol>", "\n"),
        (r"(?i)</ul>|</ol>", "\n"),
    ]
    .into_iter()
    .map(|(pattern, replacement)| (Regex::new(pattern).expect("valid regex"), replacement))
    .collect()
});

static WRAPPED_TAGS: LazyLock<Vec<WrappedTag>> = LazyLock::new(|| {
    [
        ("pre><code", "code", "```\n", "\n```"),
        ("code", "code", "`", "`"),
        ("strong", "strong", "**", "**"),
        ("b", "b", "**", "**"),
        ("em", "em", "_", "_"),
        ("i", "i", "_", "_"),
    ]
    .into_iter()
    .map(|(open, close, prefix, suffix)| WrappedTag {
        re: Regex::new(&format!(r"(?is)<{open}>([\s\S]*?)</{close}>")).expect("valid regex"),
        prefix,
        suffix,
    })
    .collect()
});

static LINK_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"(?is)<a\s+(?:[^>]*?\s+)?href="([^"]*)"[^>]*>([\s\S]*?)</a>"#)
        .expect("valid regex")
});

static TAG_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?s)<[^>]*>").expect("valid regex"));

static EXCESS_NEWLINES_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\n{3,}").expect("valid regex"));

pub fn to_markdown(html: &str, decode_entities: impl Fn(&str) -> String) -> String {
    let mut md = html.to_owned();
    for (re, replacement) in SIMPLE_REPLACEMENTS.iter() {
        md = re.replace_all(&md, *replacement).into_owned();
    }
    for tag in WRAPPED_TAGS.iter() {
        md = tag
            .re
            .replace_all(&md, |caps: &Captures<'_>| {
                format!("{}{}{}", tag.prefix, &caps[1], tag.suffix)
            })
            .into_owned();
    }
    md = LINK_RE
        .replace_all(&md, |caps: &Captures<'_>| {
            let href = &caps[1];
            let text = &caps[2];
            if href_is_safe(href) {
                format!("[{text}]({href})")
            } else {
                text.to_owned()
            }
        })
        .into_owned();
    md = TAG_RE.replace_all(&md, "").into_owned();
    let decoded = decode_entities(&md);
    EXCESS_NEWLINES_RE
        .replace_all(decoded.trim_end(), "\n\n")
        .trim()
        .to_owned()
}

fn href_is_safe(href: &str) -> bool {
    let href = href.trim();
    let Some(colon) = href.find(':') else {
        return true;
    };
    let scheme = &href[..colon];
    let looks_like_scheme = scheme
        .bytes()
        .next()
        .is_some_and(|b| b.is_ascii_alphabetic())
        && scheme
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'+' | b'-' | b'.'));
    !looks_like_scheme
        || scheme.eq_ignore_ascii_case("http")
        || scheme.eq_ignore_ascii_case("https")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn identity(s: &str) -> String {
        s.to_owned()
    }

    #[test]
    fn drops_dangerous_link_schemes_but_keeps_text() {
        assert_eq!(
            to_markdown(r#"<a href="javascript:alert(1)">click</a>"#, identity),
            "click"
        );
        assert_eq!(
            to_markdown(r#"<a href="data:text/html,x">click</a>"#, identity),
            "click"
        );
    }
}
