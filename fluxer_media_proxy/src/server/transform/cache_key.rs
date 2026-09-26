// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    constants::AssetKind,
    image_quality::ImageQuality,
    image_transform::{EncodeEffort, ResizeMode},
    output_format::OutputFormat,
    server::transform::parameters::TransformRoute,
};
use sha2::{Digest, Sha256};

pub struct TransformCacheKeyInput<'a> {
    pub route: TransformRoute,
    pub asset_kind: Option<AssetKind>,
    pub cache_identity: &'a str,
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub format: OutputFormat,
    pub quality: Option<ImageQuality>,
    pub animated: bool,
    pub effort: Option<EncodeEffort>,
    pub resize_mode: Option<ResizeMode>,
}

pub fn transform_cache_key(input: TransformCacheKeyInput<'_>) -> String {
    let prefix = match input.route {
        TransformRoute::Attachment => "attachment",
        TransformRoute::External => "external",
        TransformRoute::Stored => "stored",
        TransformRoute::Asset => "asset",
    };
    let identity = match input.route {
        TransformRoute::Attachment | TransformRoute::Stored | TransformRoute::Asset => {
            input.cache_identity.to_owned()
        }
        TransformRoute::External => sha256_hex(input.cache_identity.as_bytes()),
    };
    format!(
        "{prefix}:{identity}|asset_kind={}|w={}|h={}|fmt={}|q={}|anim={}|effort={}|resize={}",
        serialize_asset_kind(input.route, input.asset_kind),
        serialize_optional_number(input.width),
        serialize_optional_number(input.height),
        input.format.cache_serialization(),
        input
            .quality
            .map(ImageQuality::cache_serialization)
            .unwrap_or("default"),
        input.animated,
        serialize_optional_number(input.effort.map(EncodeEffort::get)),
        serialize_resize_mode(input.resize_mode),
    )
}

fn serialize_asset_kind(route: TransformRoute, kind: Option<AssetKind>) -> &'static str {
    match (route, kind) {
        (TransformRoute::Asset, Some(kind)) => asset_kind_name(kind),
        (TransformRoute::Attachment | TransformRoute::External | TransformRoute::Stored, None) => {
            "not-applicable"
        }
        (TransformRoute::Asset, None) => panic!("an asset transform cache key requires its kind"),
        (
            TransformRoute::Attachment | TransformRoute::External | TransformRoute::Stored,
            Some(_),
        ) => panic!("a non-asset transform cache key cannot carry an asset kind"),
    }
}

fn asset_kind_name(kind: AssetKind) -> &'static str {
    match kind {
        AssetKind::Avatar => "avatar",
        AssetKind::GuildIcon => "guild_icon",
        AssetKind::Banner => "banner",
        AssetKind::Splash => "splash",
        AssetKind::EmbedSplash => "embed_splash",
        AssetKind::Emoji => "emoji",
        AssetKind::Sticker => "sticker",
        AssetKind::Attachment => "attachment",
    }
}

fn serialize_resize_mode(mode: Option<ResizeMode>) -> &'static str {
    match mode {
        Some(ResizeMode::Fit) => "fit",
        Some(ResizeMode::Cover) => "cover",
        None => "not-applicable",
    }
}

fn serialize_optional_number(value: Option<impl ToString>) -> String {
    match value {
        Some(value) => value.to_string(),
        None => "none".to_owned(),
    }
}

pub(in crate::server) fn sha256_hex(data: &[u8]) -> String {
    hex::encode(Sha256::digest(data))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn input<'a>(route: TransformRoute, identity: &'a str) -> TransformCacheKeyInput<'a> {
        TransformCacheKeyInput {
            route,
            asset_kind: None,
            cache_identity: identity,
            width: None,
            height: None,
            format: OutputFormat::WebP,
            quality: None,
            animated: false,
            effort: None,
            resize_mode: None,
        }
    }

    #[test]
    fn cache_key_serializes_every_transform_decision() {
        let key = transform_cache_key(TransformCacheKeyInput {
            route: TransformRoute::Attachment,
            asset_kind: None,
            cache_identity: "attachment/1",
            width: Some(128),
            height: Some(256),
            format: OutputFormat::PNG,
            quality: Some(ImageQuality::Lossless),
            animated: true,
            effort: EncodeEffort::parse_lenient("3"),
            resize_mode: Some(ResizeMode::Cover),
        });
        assert_eq!(
            "attachment:attachment/1|asset_kind=not-applicable|w=128|h=256|fmt=png|q=lossless|anim=true|effort=3|resize=cover",
            key
        );
        assert_ne!(
            key,
            transform_cache_key(input(TransformRoute::Attachment, "attachment/1"))
        );
    }

    #[test]
    fn external_cache_identity_is_hashed_and_route_namespaces_are_distinct() {
        let raw = "https://user:secret@example.invalid/media.png?token=private";
        let external = transform_cache_key(input(TransformRoute::External, raw));
        assert!(external.starts_with("external:"));
        assert!(!external.contains(raw));
        assert!(!external.contains("secret"));
        assert_ne!(
            external,
            transform_cache_key(input(TransformRoute::Stored, raw))
        );
        assert_eq!(64, sha256_hex(b"abc").len());
    }

    #[test]
    fn asset_keys_name_their_kind_and_stay_distinct_per_kind() {
        let emoji = transform_cache_key(TransformCacheKeyInput {
            asset_kind: Some(AssetKind::Emoji),
            resize_mode: Some(ResizeMode::Cover),
            ..input(TransformRoute::Asset, "identity")
        });
        assert_eq!(
            "asset:identity|asset_kind=emoji|w=none|h=none|fmt=webp|q=default|anim=false|effort=none|resize=cover",
            emoji
        );
        assert_ne!(
            emoji,
            transform_cache_key(TransformCacheKeyInput {
                asset_kind: Some(AssetKind::Sticker),
                resize_mode: Some(ResizeMode::Cover),
                ..input(TransformRoute::Asset, "identity")
            })
        );
    }

    #[test]
    #[should_panic(expected = "an asset transform cache key requires its kind")]
    fn an_asset_key_without_a_kind_is_a_programming_error() {
        let _ = transform_cache_key(input(TransformRoute::Asset, "identity"));
    }
}
