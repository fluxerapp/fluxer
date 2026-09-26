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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strip_animation_prefix_removes_only_virtual_animated_prefix() {
        assert_eq!("abc123", strip_animation_prefix("a_abc123"));
        assert_eq!("abc123", strip_animation_prefix("abc123"));
    }

    #[test]
    fn parsed_hashes_split_the_animation_prefix_from_the_digest() {
        let plain = AssetHash::parse("abc123");
        assert_eq!("abc123", plain.digest());
        assert!(!plain.is_animated());

        let animated = AssetHash::parse("a_abc123");
        assert_eq!("abc123", animated.digest());
        assert!(animated.is_animated());

        let empty = AssetHash::parse("");
        assert_eq!("", empty.digest());
        assert!(!empty.is_animated());

        let doubled = AssetHash::parse("a_a_abc123");
        assert_eq!("a_abc123", doubled.digest());
        assert!(doubled.is_animated());
    }
}
