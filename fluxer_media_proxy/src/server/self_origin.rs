// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::DeploymentMode,
    constants::{AssetExtension, AssetKind},
    public_net_policy,
    server::{
        asset_path::{
            ParsedAssetPath, decode_storage_key, parse_entrance_sound_path,
            parse_guild_member_asset_path, parse_simple_asset_path, parse_standard_asset_path,
        },
        params::requests_transform,
        routes::dispatch::without_signature_parameters,
        state::AppState,
    },
};
use std::collections::HashMap;

pub(in crate::server) enum SelfOrigin {
    Stored {
        bucket: String,
        key: String,
        fallback_ext: Option<AssetExtension>,
    },
    External {
        url: String,
    },
}

pub(in crate::server) struct ServedVariant {
    pub(in crate::server) route: ServedRoute,
    pub(in crate::server) params: HashMap<String, String>,
}

pub(in crate::server) enum ServedRoute {
    Asset(ParsedAssetPath),
    Attachment { key: String },
    External { rest: String },
}

pub(in crate::server) fn resolve(app: &AppState, url: &str) -> Option<SelfOrigin> {
    let endpoint = app.cfg.public_endpoint.as_deref()?;
    let (path, _) = own_path(endpoint, url)?;
    let cdn = &app.cfg.storage.bucket_cdn;
    if app.cfg.mode == DeploymentMode::Static {
        return stored(
            &app.cfg.storage.bucket_static,
            decode_storage_key(path).ok()?,
        );
    }
    if let Some(rest) = path.strip_prefix("/external/") {
        let (signature, proxy_path) = rest.split_once('/')?;
        if !fluxer_common::external_media_path::verify_signature(
            proxy_path,
            signature,
            app.cfg.secret_key.as_bytes(),
        ) {
            return None;
        }
        let target =
            fluxer_common::external_media_path::reconstruct_original_url(proxy_path).ok()?;
        if own_path(endpoint, &target).is_some() {
            return None;
        }
        return Some(SelfOrigin::External { url: target });
    }
    if path.starts_with("/attachments/") {
        return stored(cdn, decode_storage_key(path).ok()?);
    }
    if path.starts_with("/themes/") && path.ends_with(".css") {
        return stored(cdn, decode_storage_key(path).ok()?);
    }
    if let Some(key) = parse_entrance_sound_path(path) {
        return stored(cdn, key);
    }
    let asset = parse_asset_path(path)?;
    Some(SelfOrigin::Stored {
        bucket: cdn.clone(),
        key: asset.storage_key,
        fallback_ext: Some(asset.original_ext),
    })
}

pub(in crate::server) fn served_variant(app: &AppState, url: &str) -> Option<ServedVariant> {
    let endpoint = app.cfg.public_endpoint.as_deref()?;
    let (path, query) = own_path(endpoint, url)?;
    if app.cfg.mode == DeploymentMode::Static {
        return None;
    }
    let params: HashMap<String, String> = url::form_urlencoded::parse(query.as_bytes())
        .into_owned()
        .collect();
    if let Some(rest) = path.strip_prefix("/external/") {
        return requests_transform(&params).then(|| ServedVariant {
            route: ServedRoute::External {
                rest: rest.to_owned(),
            },
            params,
        });
    }
    if path.starts_with("/attachments/") {
        let params = without_signature_parameters(&params);
        let key = decode_storage_key(path).ok()?;
        return requests_transform(&params).then_some(ServedVariant {
            route: ServedRoute::Attachment { key },
            params,
        });
    }
    if path.starts_with("/themes/") || parse_entrance_sound_path(path).is_some() {
        return None;
    }
    Some(ServedVariant {
        route: ServedRoute::Asset(parse_asset_path(path)?),
        params,
    })
}

fn parse_asset_path(path: &str) -> Option<ParsedAssetPath> {
    parse_guild_member_asset_path(path)
        .or_else(|| parse_simple_asset_path(path, AssetKind::Emoji))
        .or_else(|| parse_simple_asset_path(path, AssetKind::Sticker))
        .or_else(|| parse_standard_asset_path(path))
}

fn stored(bucket: &str, key: String) -> Option<SelfOrigin> {
    Some(SelfOrigin::Stored {
        bucket: bucket.to_owned(),
        key,
        fallback_ext: None,
    })
}

fn own_path<'a>(endpoint: &str, url: &'a str) -> Option<(&'a str, &'a str)> {
    let base = public_net_policy::parse_url(endpoint).ok()?;
    let target = public_net_policy::parse_url(url).ok()?;
    if !base.scheme.eq_ignore_ascii_case(target.scheme) {
        return None;
    }
    if !base.host.eq_ignore_ascii_case(target.host) {
        return None;
    }
    if effective_port(base.scheme, base.port) != effective_port(target.scheme, target.port) {
        return None;
    }
    let prefix = split_query(base.path_query).0.trim_end_matches('/');
    let (path, query) = split_query(target.path_query);
    let rest = path.strip_prefix(prefix)?;
    rest.starts_with('/').then_some((rest, query))
}

fn split_query(path_query: &str) -> (&str, &str) {
    path_query.split_once('?').unwrap_or((path_query, ""))
}

fn effective_port(scheme: &str, port: Option<u16>) -> u16 {
    port.unwrap_or(if scheme.eq_ignore_ascii_case("https") {
        443
    } else {
        80
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn own_path_rejects_other_origins_and_sibling_prefixes() {
        assert_eq!(
            own_path(
                "https://chat.example.com/media",
                "https://evil.example.com/media/attachments/1/2/cat.gif"
            ),
            None
        );
        assert_eq!(
            own_path(
                "https://chat.example.com/media",
                "http://chat.example.com/media/attachments/1/2/cat.gif"
            ),
            None
        );
        assert_eq!(
            own_path(
                "https://chat.example.com/media",
                "https://chat.example.com:8443/media/attachments/1/2/cat.gif"
            ),
            None
        );
        assert_eq!(
            own_path(
                "https://chat.example.com/media",
                "https://chat.example.com/mediafiles/attachments/1/2/cat.gif"
            ),
            None
        );
    }
}
