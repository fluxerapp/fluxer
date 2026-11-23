// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_markdown_parser::ParserFlags;
use fluxer_markdown_parser::plaintext::{Names, PlaintextOptions, parse_and_render_plaintext};

#[cfg(feature = "nif")]
pub mod nifs;

pub const MAX_CONTENT_BYTES: usize = 16 * 1024;
pub const MAX_CONTEXT_NAME_BYTES: usize = 64 * 1024;
pub const MAX_CONTEXT_NAME_ENTRIES: usize = 512;
pub const MAX_RENDERED_BYTES: usize = 64 * 1024;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PreviewError {
    ContentTooLarge,
    ContextTooLarge,
    InvalidUtf8,
    InvalidContext,
    TooManyNames,
    ParseFailed,
    RenderedTooLarge,
}

impl PreviewError {
    pub fn reason(self) -> &'static str {
        match self {
            PreviewError::ContentTooLarge => "content_too_large",
            PreviewError::ContextTooLarge => "context_too_large",
            PreviewError::InvalidUtf8 => "invalid_utf8",
            PreviewError::InvalidContext => "invalid_context",
            PreviewError::TooManyNames => "too_many_names",
            PreviewError::ParseFailed => "parse_failed",
            PreviewError::RenderedTooLarge => "rendered_too_large",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PreviewContext {
    pub preserve_markdown: bool,
    pub hide_spoilers: bool,
    pub include_emoji_names: bool,
    pub include_link_urls: bool,
    pub users: Vec<(u64, String)>,
    pub roles: Vec<(u64, String)>,
    pub channels: Vec<(u64, String)>,
}

impl Default for PreviewContext {
    fn default() -> Self {
        Self {
            preserve_markdown: false,
            hide_spoilers: false,
            include_emoji_names: true,
            include_link_urls: false,
            users: Vec::new(),
            roles: Vec::new(),
            channels: Vec::new(),
        }
    }
}

impl PreviewContext {
    fn validate(&self) -> Result<(), PreviewError> {
        let maps = [&self.users, &self.roles, &self.channels];
        if maps
            .iter()
            .any(|names| names.len() > MAX_CONTEXT_NAME_ENTRIES)
        {
            return Err(PreviewError::TooManyNames);
        }
        let name_bytes: usize = maps
            .iter()
            .flat_map(|names| names.iter())
            .map(|(_, name)| name.len())
            .sum();
        if name_bytes > MAX_CONTEXT_NAME_BYTES {
            return Err(PreviewError::ContextTooLarge);
        }
        Ok(())
    }

    pub fn into_options(self) -> PlaintextOptions {
        PlaintextOptions {
            preserve_markdown: self.preserve_markdown,
            hide_spoilers: self.hide_spoilers,
            include_emoji_names: self.include_emoji_names,
            include_link_urls: self.include_link_urls,
            users: Names::new(self.users),
            roles: Names::new(self.roles),
            channels: Names::new(self.channels),
        }
    }
}

pub fn render_push_preview(
    content: &[u8],
    context: PreviewContext,
) -> Result<String, PreviewError> {
    if content.len() > MAX_CONTENT_BYTES {
        return Err(PreviewError::ContentTooLarge);
    }
    let content = std::str::from_utf8(content).map_err(|_| PreviewError::InvalidUtf8)?;
    context.validate()?;
    let options = context.into_options();
    let rendered = parse_and_render_plaintext(content, ParserFlags::ALL, "", &options)
        .map_err(|_| PreviewError::ParseFailed)?;
    if rendered.len() > MAX_RENDERED_BYTES {
        return Err(PreviewError::RenderedTooLarge);
    }
    Ok(rendered)
}

#[cfg(test)]
mod tests {
    use super::{MAX_CONTEXT_NAME_ENTRIES, PreviewContext, PreviewError, render_push_preview};

    #[test]
    fn resolves_user_mentions() {
        let context = PreviewContext {
            users: vec![(123, "alice".to_owned())],
            ..PreviewContext::default()
        };
        let rendered = render_push_preview(b"hi <@123> **there**", context).unwrap();
        assert!(rendered.contains("alice"));
        assert!(!rendered.contains("**"));
    }

    #[test]
    fn resolves_snowflake_roles_and_channels() {
        let context = PreviewContext {
            roles: vec![(1_400_000_000_000_000_200, "mods".to_owned())],
            channels: vec![(1_400_000_000_000_000_100, "general".to_owned())],
            ..PreviewContext::default()
        };
        let rendered = render_push_preview(
            b"<@&1400000000000000200> in <#1400000000000000100> and <#1400000000000000101>",
            context,
        )
        .unwrap();
        assert_eq!(rendered, "@mods in #general and #unknown-channel");
    }

    #[test]
    fn matches_only_canonical_mention_ids() {
        let context = PreviewContext {
            users: vec![
                (5, "old".to_owned()),
                (123, "alice".to_owned()),
                (5, "new".to_owned()),
            ],
            channels: vec![(0, "zero".to_owned())],
            ..PreviewContext::default()
        };
        let rendered = render_push_preview(b"<@123> <@0123> <@5> <#0> <#00>", context).unwrap();
        assert_eq!(rendered, "@alice @0123 @new #zero #unknown-channel");
    }

    #[test]
    fn empty_context_is_valid() {
        assert!(render_push_preview(b"plain", PreviewContext::default()).is_ok());
    }

    #[test]
    fn rejects_large_content() {
        let content = vec![b'a'; 16 * 1024 + 1];
        assert_eq!(
            render_push_preview(&content, PreviewContext::default()),
            Err(PreviewError::ContentTooLarge)
        );
    }

    #[test]
    fn rejects_bad_utf8() {
        assert_eq!(
            render_push_preview(&[0xff, 0xfe], PreviewContext::default()),
            Err(PreviewError::InvalidUtf8)
        );
    }

    #[test]
    fn rejects_too_many_names() {
        let users = (0..=MAX_CONTEXT_NAME_ENTRIES as u64)
            .map(|id| (id, "x".to_owned()))
            .collect();
        let context = PreviewContext {
            users,
            ..PreviewContext::default()
        };
        assert_eq!(
            render_push_preview(b"x", context),
            Err(PreviewError::TooManyNames)
        );
    }

    #[test]
    fn rejects_oversized_names() {
        let context = PreviewContext {
            channels: vec![(1, "n".repeat(64 * 1024 + 1))],
            ..PreviewContext::default()
        };
        assert_eq!(
            render_push_preview(b"x", context),
            Err(PreviewError::ContextTooLarge)
        );
    }
}
