// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    asset_hash::AssetHash,
    constants::{AssetExtension, AssetKind},
    external_path,
    storage::safe_key,
};
use std::fmt;

pub(in crate::server) struct ParsedAssetPath {
    pub(in crate::server) storage_key: String,
    pub(in crate::server) original_ext: AssetExtension,
    pub(in crate::server) hash: String,
    pub(in crate::server) kind: AssetKind,
    pub(in crate::server) forced_output_format: Option<AssetExtension>,
}

pub(in crate::server) fn parse_standard_asset_path(path: &str) -> Option<ParsedAssetPath> {
    let mut parts = canonical_public_path(path)?.split('/');
    let prefix = parts.next()?;
    let owner_key = parts.next()?;
    let filename = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    let kind = match prefix {
        "avatars" => AssetKind::Avatar,
        "icons" => AssetKind::GuildIcon,
        "branding" => AssetKind::GuildIcon,
        "banners" => AssetKind::Banner,
        "splashes" => AssetKind::Splash,
        "embed-splashes" => AssetKind::EmbedSplash,
        _ => return None,
    };
    if !valid_asset_owner_key(prefix, owner_key) {
        return None;
    }
    let parsed = parse_asset_filename(filename)?;
    let storage_hash = AssetHash::parse(parsed.hash).digest();
    Some(ParsedAssetPath {
        storage_key: format!("{prefix}/{owner_key}/{storage_hash}"),
        original_ext: parsed.ext,
        hash: parsed.hash.to_owned(),
        kind,
        forced_output_format: None,
    })
}

pub(in crate::server) fn parse_guild_member_asset_path(path: &str) -> Option<ParsedAssetPath> {
    let mut parts = canonical_public_path(path)?.split('/');
    if parts.next()? != "guilds" {
        return None;
    }
    let guild_id = SnowflakeId::parse(parts.next()?)?;
    if parts.next()? != "users" {
        return None;
    }
    let user_id = SnowflakeId::parse(parts.next()?)?;
    let prefix = parts.next()?;
    let filename = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    let kind = match prefix {
        "avatars" => AssetKind::Avatar,
        "banners" => AssetKind::Banner,
        _ => return None,
    };
    let parsed = parse_asset_filename(filename)?;
    let storage_hash = AssetHash::parse(parsed.hash).digest();
    Some(ParsedAssetPath {
        storage_key: format!("guilds/{guild_id}/users/{user_id}/{prefix}/{storage_hash}"),
        original_ext: parsed.ext,
        hash: parsed.hash.to_owned(),
        kind,
        forced_output_format: None,
    })
}

pub(in crate::server) fn parse_simple_asset_path(
    path: &str,
    kind: AssetKind,
) -> Option<ParsedAssetPath> {
    let expected_prefix = match kind {
        AssetKind::Emoji => "emojis",
        AssetKind::Sticker => "stickers",
        _ => return None,
    };
    let mut parts = canonical_public_path(path)?.split('/');
    let prefix = parts.next()?;
    if prefix != expected_prefix {
        return None;
    }
    let filename = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    let parsed = parse_asset_filename(filename)?;
    let id = SnowflakeId::parse(parsed.hash)?;
    Some(ParsedAssetPath {
        storage_key: format!("{prefix}/{id}"),
        original_ext: parsed.ext,
        hash: parsed.hash.to_owned(),
        kind,
        forced_output_format: (kind == AssetKind::Sticker).then_some(AssetExtension::Webp),
    })
}

pub(in crate::server) fn parse_entrance_sound_path(path: &str) -> Option<String> {
    let mut parts = canonical_public_path(path)?.split('/');
    if parts.next()? != "entrance-sounds" {
        return None;
    }
    let user_id = parts.next()?;
    let filename = parts.next()?;
    if parts.next().is_some() {
        return None;
    }
    if user_id.is_empty() || !user_id.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let (hash, ext) = filename.split_once('.')?;
    if hash.is_empty() || !hash.bytes().all(|b| b.is_ascii_alphanumeric()) {
        return None;
    }
    if !matches!(ext, "mp3" | "ogg" | "m4a" | "wav") {
        return None;
    }
    Some(format!("entrance-sounds/{user_id}/{filename}"))
}

struct ParsedAssetFilename<'a> {
    hash: &'a str,
    ext: AssetExtension,
}

fn parse_asset_filename(filename: &str) -> Option<ParsedAssetFilename<'_>> {
    let (hash, ext_raw) = filename.split_once('.')?;
    if hash.is_empty() || ext_raw.is_empty() || ext_raw.contains('.') {
        return None;
    }
    if !hash.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_') {
        return None;
    }
    if !ext_raw.bytes().all(|b| b.is_ascii_alphanumeric()) {
        return None;
    }
    Some(ParsedAssetFilename {
        hash,
        ext: AssetExtension::parse(ext_raw)?,
    })
}

pub(in crate::server) fn asset_filename_hint(asset: &ParsedAssetPath) -> String {
    let hash = AssetHash::parse(&asset.hash).digest();
    format!("{hash}.{}", asset.original_ext.name())
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct SnowflakeId(u64);

impl SnowflakeId {
    fn parse(raw: &str) -> Option<Self> {
        if raw.starts_with('0') || !raw.bytes().all(|byte| byte.is_ascii_digit()) {
            return None;
        }
        let value = raw.parse::<u64>().ok()?;
        (value > 0).then_some(Self(value))
    }
}

impl fmt::Display for SnowflakeId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

fn valid_asset_owner_key(prefix: &str, owner_key: &str) -> bool {
    if prefix == "branding" {
        return !owner_key.is_empty()
            && owner_key.len() <= ASSET_OWNER_KEY_MAX_BYTES
            && owner_key
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_'));
    }
    SnowflakeId::parse(owner_key).is_some()
}

const ASSET_OWNER_KEY_MAX_BYTES: usize = 128;

fn canonical_public_path(path: &str) -> Option<&str> {
    let relative_path = path.strip_prefix('/')?;
    if relative_path.starts_with('/') {
        return None;
    }
    Some(relative_path)
}

#[derive(Debug, thiserror::Error)]
pub(in crate::server) enum StorageKeyDecodeError {
    #[error("decoded storage key is invalid")]
    InvalidKey,
}

pub(in crate::server) fn decode_storage_key(path: &str) -> Result<String, StorageKeyDecodeError> {
    let key = external_path::percent_decode_string(path.trim_start_matches('/'), false);
    safe_key(&key).map_err(|_| StorageKeyDecodeError::InvalidKey)?;
    Ok(key)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::server::params::asset_wants_animated;
    use std::collections::HashMap;

    #[test]
    fn entrance_sound_path_parses_valid_keys() {
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/1130650140672000000/eb417d05ad2e14c4.wav"),
            Some("entrance-sounds/1130650140672000000/eb417d05ad2e14c4.wav".to_owned())
        );
        for ext in ["mp3", "ogg", "m4a", "wav"] {
            assert_eq!(
                parse_entrance_sound_path(&format!("/entrance-sounds/42/abc123.{ext}")),
                Some(format!("entrance-sounds/42/abc123.{ext}"))
            );
        }
    }

    #[test]
    fn entrance_sound_path_rejects_invalid_keys() {
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/42/abc.flac"),
            None
        );
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/abc/abc.wav"),
            None
        );
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/42/abc.wav/x"),
            None
        );
        assert_eq!(
            parse_entrance_sound_path("/entrance-sounds/42/../secret.wav"),
            None
        );
        assert_eq!(parse_entrance_sound_path("/entrance-sounds/42"), None);
        assert_eq!(parse_entrance_sound_path("/entrance-sounds//abc.wav"), None);
        assert_eq!(parse_entrance_sound_path("/avatars/42/abc.wav"), None);
    }

    #[test]
    fn standard_asset_path_strips_virtual_animation_prefix_and_extension() {
        let parsed =
            parse_standard_asset_path("/avatars/1216100949629702144/a_d2f35261.webp").unwrap();
        assert_eq!("avatars/1216100949629702144/d2f35261", parsed.storage_key);
        assert_eq!(AssetExtension::Webp, parsed.original_ext);
        assert_eq!(AssetKind::Avatar, parsed.kind);
        assert!(asset_wants_animated(&HashMap::new(), &parsed.hash));
        assert!(!asset_wants_animated(
            &HashMap::from([("animated".to_owned(), "false".to_owned())]),
            &parsed.hash
        ));
    }

    #[test]
    fn guild_member_and_simple_asset_paths_match_v1_storage_keys() {
        let guild =
            parse_guild_member_asset_path("/guilds/1/users/2/avatars/a_memberhash.gif").unwrap();
        assert_eq!("guilds/1/users/2/avatars/memberhash", guild.storage_key);
        assert_eq!(AssetKind::Avatar, guild.kind);
        assert_eq!(AssetExtension::Gif, guild.original_ext);
        assert!(guild.forced_output_format.is_none());

        let emoji =
            parse_simple_asset_path("/emojis/1501314428688998182.webp", AssetKind::Emoji).unwrap();
        assert_eq!("emojis/1501314428688998182", emoji.storage_key);
        assert_eq!(AssetKind::Emoji, emoji.kind);
        assert!(emoji.forced_output_format.is_none());

        let sticker =
            parse_simple_asset_path("/stickers/1501314428688998182.png", AssetKind::Sticker)
                .unwrap();
        assert_eq!("stickers/1501314428688998182", sticker.storage_key);
        assert_eq!(Some(AssetExtension::Webp), sticker.forced_output_format);
    }

    #[test]
    fn asset_filename_rejects_unstable_shapes() {
        assert!(parse_asset_filename("hash.extra.png").is_none());
        assert!(parse_asset_filename("hash-with-dash.png").is_none());
        assert!(parse_asset_filename(".png").is_none());
        assert!(parse_asset_filename("hash.").is_none());
    }

    #[test]
    fn asset_paths_reject_noncanonical_owners_extensions_and_prefixes() {
        for invalid in [
            "avatars/1216100949629702144/deadbeef.png",
            "//avatars/1216100949629702144/deadbeef.png",
            "/avatars/0/deadbeef.png",
            "/avatars/01/deadbeef.png",
            "/avatars/1216100949629702144/deadbeef.exe",
            "/avatars/1216100949629702144/deadbeef.png/extra",
        ] {
            assert!(
                parse_standard_asset_path(invalid).is_none(),
                "accepted {invalid}"
            );
        }
        assert!(parse_standard_asset_path("/branding/fluxer-wordmark/deadbeef.png").is_some());
        assert!(parse_guild_member_asset_path("/guilds/01/users/2/avatars/h.gif").is_none());
        assert!(parse_simple_asset_path("/emojis/012.webp", AssetKind::Emoji).is_none());
    }

    #[test]
    fn storage_keys_decode_the_way_the_old_era_decoded_them() {
        assert_eq!(
            "attachments/1216100949629702144/1216100949629702145/name/with-slash.png",
            decode_storage_key(
                "/attachments/1216100949629702144/1216100949629702145/name%2Fwith-slash.png"
            )
            .expect("an encoded slash still resolves to a live object")
        );
        assert_eq!(
            "attachments/1/a%.png",
            decode_storage_key("/attachments/1/a%.png").expect("malformed escapes stay literal")
        );
        assert_eq!(
            "attachments/1/\u{fffd}.png",
            decode_storage_key("/attachments/1/%FF.png").expect("non utf8 decodes lossily")
        );
        assert_eq!(
            "attachments/1/a\\b.png",
            decode_storage_key("/attachments/1/a%5Cb.png")
                .expect("a backslash is an ordinary key byte the old era served")
        );
        for traversal in [
            "/attachments/%2E%2E/photo.png",
            "/attachments/1/..%2Fsecret.png",
            "/attachments/1/%2E%2E%2Fsecret.png",
            "/attachments/1/a%00b.png",
            "/attachments%2F%2Fphoto.png",
        ] {
            assert!(
                matches!(
                    decode_storage_key(traversal),
                    Err(StorageKeyDecodeError::InvalidKey)
                ),
                "safe_key accepted {traversal}"
            );
        }
    }

    #[test]
    fn storage_keys_reject_path_changing_escapes_and_empty_components() {
        assert_eq!(
            "attachments/1/2/photo.png",
            decode_storage_key("/attachments/1/2/photo.png").expect("valid storage key")
        );
        assert_eq!(
            "attachments/1/a b.png",
            decode_storage_key("/attachments/1/a%20b.png").expect("valid storage key")
        );
        assert_eq!(
            "attachments/1/photo.png",
            decode_storage_key("/attachments%2F1%2Fphoto.png").expect("valid storage key")
        );
        assert!(matches!(
            decode_storage_key("/attachments/%2E%2E/photo.png"),
            Err(StorageKeyDecodeError::InvalidKey)
        ));
        assert!(matches!(
            decode_storage_key("/attachments//photo.png"),
            Err(StorageKeyDecodeError::InvalidKey)
        ));
        assert!(decode_storage_key("/").is_err());
    }
}
