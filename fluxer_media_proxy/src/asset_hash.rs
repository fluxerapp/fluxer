// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AssetHash<'a> {
    digest: &'a str,
    animated: bool,
}

impl<'a> AssetHash<'a> {
    pub fn parse(raw: &'a str) -> Self {
        Self {
            digest: strip_animation_prefix(raw),
            animated: has_animation_prefix(raw),
        }
    }

    pub fn digest(self) -> &'a str {
        self.digest
    }

    pub fn is_animated(self) -> bool {
        self.animated
    }
}

pub fn strip_animation_prefix(hash: &str) -> &str {
    hash.strip_prefix("a_").unwrap_or(hash)
}

pub fn has_animation_prefix(hash: &str) -> bool {
    hash.starts_with("a_")
}
