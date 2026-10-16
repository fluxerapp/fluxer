// SPDX-License-Identifier: AGPL-3.0-or-later

pub const TITLE_MAX: usize = 256;

pub const DESCRIPTION_MAX: usize = 4096;

pub const DESCRIPTION_LINES_MAX: usize = 20;

pub const AUTHOR_NAME_MAX: usize = 256;

pub const PROVIDER_NAME_MAX: usize = 256;

pub const FOOTER_TEXT_MAX: usize = 2048;

pub const MEDIA_DESCRIPTION_MAX: usize = 4096;

pub const FIELD_NAME_MAX: usize = 256;

pub const FIELD_VALUE_MAX: usize = 1024;

pub const HTML_MAX: usize = 12000;

pub const MAX_FIELDS: usize = 25;

pub const MAX_CHILDREN: usize = 1;

pub fn truncate(s: &str, max_len: usize) -> String {
    if s.chars().count() <= max_len {
        return s.to_owned();
    }
    let truncated: String = s.chars().take(max_len.saturating_sub(1)).collect();
    format!("{truncated}\u{2026}")
}

pub fn clamp_lines(s: &str, max_lines: usize) -> String {
    let lines: Vec<&str> = s.lines().collect();
    if lines.len() <= max_lines {
        return s.to_owned();
    }
    let mut result: Vec<&str> = lines[..max_lines].to_vec();
    result.push("\u{2026}");
    result.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn truncate_preserves_multibyte_boundaries() {
        let s = "hello 🎉🎊🎁 world";
        let result = truncate(s, 10);
        assert!(result.chars().count() <= 10);
        assert!(result.ends_with('\u{2026}'));
    }
}
