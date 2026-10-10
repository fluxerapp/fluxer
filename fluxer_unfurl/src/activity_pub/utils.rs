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
