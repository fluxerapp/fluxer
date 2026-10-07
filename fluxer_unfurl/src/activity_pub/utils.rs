// SPDX-License-Identifier: AGPL-3.0-or-later

use url::Url;

pub fn resolve_relative_url(base: &Url, relative: &str) -> Option<String> {
    if relative.is_empty() {
        return None;
    }
    match Url::parse(relative) {
        Ok(url) => Some(url.to_string()),
        Err(_) => base.join(relative).ok().map(|u| u.to_string()),
    }
}

pub fn extract_username_from_url(value: &str) -> Option<String> {
    let parsed = Url::parse(value).ok()?;
    let segs: Vec<&str> = parsed.path_segments()?.filter(|s| !s.is_empty()).collect();
    if segs.is_empty() {
        return None;
    }
    if let Some(stripped) = segs[0].strip_prefix('@') {
        return Some(stripped.to_owned());
    }
    if let Some(users_index) = segs.iter().position(|segment| *segment == "users")
        && let Some(username) = segs.get(users_index + 1)
    {
        return Some((*username).to_owned());
    }
    segs.last().map(|s| s.to_string())
}

pub fn extract_post_id(url: &Url) -> Option<String> {
    static POST_ID_PATTERNS: std::sync::LazyLock<Vec<regex::Regex>> =
        std::sync::LazyLock::new(|| {
            [
                r"/@[^/]+/(\w+)",
                r"/users/[^/]+/status(?:es)?/(\w+)",
                r"/[^/]+/status(?:es)?/(\w+)",
                r"/notice/([a-zA-Z0-9]+)",
                r"/notes/([a-zA-Z0-9]+)",
            ]
            .into_iter()
            .map(|pattern| regex::Regex::new(pattern).expect("valid regex"))
            .collect()
        });

    let path = url.path();
    POST_ID_PATTERNS.iter().find_map(|re| {
        re.captures(path)
            .and_then(|caps| caps.get(1))
            .map(|m| m.as_str().to_owned())
    })
}

pub fn is_http_url(url: &str) -> bool {
    matches!(
        Url::parse(url).ok().map(|u| u.scheme().to_owned()),
        Some(s) if s == "http" || s == "https"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_mastodon_post_id() {
        let url = Url::parse("https://mastodon.social/@user/12345").unwrap();
        assert_eq!(extract_post_id(&url), Some("12345".to_owned()));
    }

    #[test]
    fn extract_pleroma_notice_id() {
        let url = Url::parse("https://pleroma.example/notice/abc123").unwrap();
        assert_eq!(extract_post_id(&url), Some("abc123".to_owned()));
    }

    #[test]
    fn extract_generic_status_id() {
        let url = Url::parse("https://example.social/alice/status/12345").unwrap();
        assert_eq!(extract_post_id(&url), Some("12345".to_owned()));
    }

    #[test]
    fn extract_misskey_note_id() {
        let url = Url::parse("https://misskey.io/notes/xyz789").unwrap();
        assert_eq!(extract_post_id(&url), Some("xyz789".to_owned()));
    }

    #[test]
    fn resolve_absolute_url() {
        let base = Url::parse("https://example.com/page").unwrap();
        let result = resolve_relative_url(&base, "https://other.com/img.png");
        assert_eq!(result, Some("https://other.com/img.png".to_owned()));
    }

    #[test]
    fn resolve_relative_path() {
        let base = Url::parse("https://example.com/page").unwrap();
        let result = resolve_relative_url(&base, "/img.png");
        assert_eq!(result, Some("https://example.com/img.png".to_owned()));
    }

    #[test]
    fn extract_mastodon_users_statuses_path() {
        let url = Url::parse("https://mastodon.social/users/alice/statuses/12345").unwrap();
        assert_eq!(extract_post_id(&url), Some("12345".to_owned()));
    }

    #[test]
    fn extract_goblin_social_status_path() {
        let url = Url::parse("https://goblin.social/alice/status/98765").unwrap();
        assert_eq!(extract_post_id(&url), Some("98765".to_owned()));
    }

    #[test]
    fn extract_unrecognised_path_returns_none() {
        let url = Url::parse("https://example.com/about").unwrap();
        assert!(extract_post_id(&url).is_none());
    }

    #[test]
    fn resolve_empty_string_returns_none() {
        let base = Url::parse("https://example.com/page").unwrap();
        assert!(resolve_relative_url(&base, "").is_none());
    }

    #[test]
    fn resolve_protocol_relative_url() {
        let base = Url::parse("https://example.com/page").unwrap();
        let result = resolve_relative_url(&base, "//cdn.example.com/img.png");
        assert_eq!(result, Some("https://cdn.example.com/img.png".to_owned()));
    }

    #[test]
    fn is_http_url_validates() {
        assert!(is_http_url("https://example.com"));
        assert!(is_http_url("http://example.com"));
        assert!(!is_http_url("ftp://example.com"));
        assert!(!is_http_url("not-a-url"));
    }

    #[test]
    fn extract_username_from_at_prefix() {
        assert_eq!(
            extract_username_from_url("https://mastodon.social/@alice").as_deref(),
            Some("alice")
        );
    }

    #[test]
    fn extract_username_from_users_path() {
        assert_eq!(
            extract_username_from_url("https://example.com/users/bob").as_deref(),
            Some("bob")
        );
    }

    #[test]
    fn extract_username_from_deep_users_path() {
        assert_eq!(
            extract_username_from_url("https://example.com/api/users/carol/statuses").as_deref(),
            Some("carol")
        );
    }

    #[test]
    fn extract_username_fallback_to_last_segment() {
        assert_eq!(
            extract_username_from_url("https://example.com/actors/dave").as_deref(),
            Some("dave")
        );
    }

    #[test]
    fn extract_username_empty_path_returns_none() {
        assert!(extract_username_from_url("https://example.com").is_none());
        assert!(extract_username_from_url("https://example.com/").is_none());
    }
}
