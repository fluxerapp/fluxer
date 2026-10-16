// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::AdminConfig;

const DEFAULT_AVATAR_COUNT: u128 = 6;

pub fn user_avatar_url(
    config: &AdminConfig,
    user_id: &str,
    avatar: Option<&str>,
    size: u32,
    force_static: bool,
) -> String {
    if let Some(hash) = avatar.filter(|value| !value.trim().is_empty()) {
        return media_asset_url(
            &config.media_endpoint,
            "avatars",
            user_id,
            hash,
            size,
            force_static,
        );
    }
    let index = user_id
        .parse::<u128>()
        .map(|value| value % DEFAULT_AVATAR_COUNT)
        .unwrap_or(0);
    format!(
        "{}/avatars/{index}.png",
        config.static_cdn_endpoint.trim_end_matches('/')
    )
}

pub fn user_banner_url(
    config: &AdminConfig,
    user_id: &str,
    banner: Option<&str>,
    size: u32,
    force_static: bool,
) -> Option<String> {
    banner.filter(|value| !value.trim().is_empty()).map(|hash| {
        media_asset_url(
            &config.media_endpoint,
            "banners",
            user_id,
            hash,
            size,
            force_static,
        )
    })
}

pub fn guild_icon_url(
    config: &AdminConfig,
    guild_id: &str,
    icon: Option<&str>,
    size: u32,
    force_static: bool,
) -> Option<String> {
    guild_asset_url(config, "icons", guild_id, icon, size, force_static)
}

pub fn guild_asset_url(
    config: &AdminConfig,
    path: &str,
    guild_id: &str,
    hash: Option<&str>,
    size: u32,
    force_static: bool,
) -> Option<String> {
    hash.filter(|value| !value.trim().is_empty()).map(|hash| {
        media_asset_url(
            &config.media_endpoint,
            path,
            guild_id,
            hash,
            size,
            force_static,
        )
    })
}

pub fn initials(name: &str) -> String {
    let mut parts = name.split_whitespace();
    let Some(first) = parts.next() else {
        return "?".to_owned();
    };
    std::iter::once(first)
        .chain(parts.next_back())
        .flat_map(|part| part.chars().take(1))
        .flat_map(char::to_uppercase)
        .collect()
}

fn media_asset_url(
    media_endpoint: &str,
    path: &str,
    id: &str,
    raw_hash: &str,
    size: u32,
    force_static: bool,
) -> String {
    let animated = raw_hash.starts_with("a_") && !force_static;
    let hash = raw_hash.strip_prefix("a_").unwrap_or(raw_hash);
    let mut url = format!(
        "{}/{path}/{id}/{hash}.webp?size={size}",
        media_endpoint.trim_end_matches('/')
    );
    if animated {
        url.push_str("&animated=true");
    }
    url
}
