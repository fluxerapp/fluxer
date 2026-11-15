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
        ) => panic!("a non-asset transform cache key cannot have an asset kind"),
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
        AssetKind::Branding => "branding",
        AssetKind::RoleIcon => "role_icon",
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
