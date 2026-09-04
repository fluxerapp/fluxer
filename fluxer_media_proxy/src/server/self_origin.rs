// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::DeploymentMode,
    constants::{AssetExtension, AssetKind},
    external_path, public_net_policy,
    server::{
        asset_path::{
            decode_storage_key, parse_entrance_sound_path, parse_guild_member_asset_path,
            parse_simple_asset_path, parse_standard_asset_path,
        },
        state::AppState,
    },
    signing,
};

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

pub(in crate::server) fn resolve(app: &AppState, url: &str) -> Option<SelfOrigin> {
    let endpoint = app.cfg.public_endpoint.as_deref()?;
    let path = own_path(endpoint, url)?;
    let cdn = &app.cfg.storage.bucket_cdn;
    if app.cfg.mode == DeploymentMode::Static {
        return stored(
            &app.cfg.storage.bucket_static,
            decode_storage_key(path).ok()?,
        );
    }
    if let Some(rest) = path.strip_prefix("/external/") {
        let (signature, proxy_path) = rest.split_once('/')?;
        if !signing::verify_signature(proxy_path, signature, app.cfg.secret_key.as_bytes()) {
            return None;
        }
        let target = external_path::reconstruct_original_url(proxy_path).ok()?;
        if own_path(endpoint, &target).is_some() {
            return None;
        }
        return Some(SelfOrigin::External { url: target });
    }
    if path.starts_with("/attachments/") || (path.starts_with("/themes/") && path.ends_with(".css"))
    {
        return stored(cdn, decode_storage_key(path).ok()?);
    }
    if let Some(key) = parse_entrance_sound_path(path) {
        return stored(cdn, key);
    }
    let asset = parse_guild_member_asset_path(path)
        .or_else(|| parse_simple_asset_path(path, AssetKind::Emoji))
        .or_else(|| parse_simple_asset_path(path, AssetKind::Sticker))
        .or_else(|| parse_standard_asset_path(path))?;
    Some(SelfOrigin::Stored {
        bucket: cdn.clone(),
        key: asset.storage_key,
        fallback_ext: Some(asset.original_ext),
    })
}

fn stored(bucket: &str, key: String) -> Option<SelfOrigin> {
    Some(SelfOrigin::Stored {
        bucket: bucket.to_owned(),
        key,
        fallback_ext: None,
    })
}

fn own_path<'a>(endpoint: &str, url: &'a str) -> Option<&'a str> {
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
    let prefix = path_of(base.path_query).trim_end_matches('/');
    let rest = path_of(target.path_query).strip_prefix(prefix)?;
    rest.starts_with('/').then_some(rest)
}

fn path_of(path_query: &str) -> &str {
    path_query
        .split_once('?')
        .map(|(path, _)| path)
        .unwrap_or(path_query)
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
    use crate::config::Config;

    const ENDPOINT: &str = "https://chat.example.com/media";

    fn app() -> AppState {
        AppState::for_tests(
            Config::load_from_iter([
                (
                    "FLUXER_MEDIA_PROXY_SECRET_KEY".to_owned(),
                    "secret".to_owned(),
                ),
                (
                    "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT".to_owned(),
                    format!("{ENDPOINT}/"),
                ),
                (
                    "FLUXER_MEDIA_PROXY_STORAGE_BACKEND".to_owned(),
                    "local".to_owned(),
                ),
            ])
            .expect("self origin test config"),
        )
    }

    #[test]
    fn own_attachments_read_from_the_cdn_bucket() {
        let app = app();
        let resolved = resolve(
            &app,
            &format!("{ENDPOINT}/attachments/1544725486800732163/1544971349200470016/cat.gif"),
        );
        match resolved {
            Some(SelfOrigin::Stored { bucket, key, .. }) => {
                assert_eq!(app.cfg.storage.bucket_cdn, bucket);
                assert_eq!(
                    "attachments/1544725486800732163/1544971349200470016/cat.gif",
                    key
                );
            }
            _ => panic!("own attachment paths resolve to stored objects"),
        }
    }

    #[test]
    fn own_avatars_carry_the_extension_fallback() {
        let app = app();
        match resolve(
            &app,
            &format!("{ENDPOINT}/avatars/1216100949629702144/a1b2c3d4e5f6.png?size=64"),
        ) {
            Some(SelfOrigin::Stored {
                key, fallback_ext, ..
            }) => {
                assert_eq!("avatars/1216100949629702144/a1b2c3d4e5f6", key);
                assert_eq!(Some(AssetExtension::Png), fallback_ext);
            }
            _ => panic!("own asset paths resolve to stored objects"),
        }
    }

    #[test]
    fn own_external_paths_unwrap_to_the_origin_url() {
        let app = app();
        let target = "https://static.klipy.com/ii/c8/28/HkAKKCzZ.webp";
        let proxied = fluxer_common::external_media_path::build_external_media_proxy_url(
            ENDPOINT,
            target,
            app.cfg.secret_key.as_bytes(),
        )
        .expect("proxy url");
        match resolve(&app, &proxied) {
            Some(SelfOrigin::External { url }) => assert_eq!(target, url),
            _ => panic!("own external paths unwrap to the origin url"),
        }
        let tampered = proxied.replace("/external/", "/external/x");
        assert!(resolve(&app, &tampered).is_none());
    }

    #[test]
    fn foreign_and_unknown_paths_stay_external_fetches() {
        let app = app();
        assert!(resolve(&app, "https://static.klipy.com/ii/c8/28/HkAKKCzZ.webp").is_none());
        assert!(resolve(&app, &format!("{ENDPOINT}/channels/1/2")).is_none());
    }

    #[test]
    fn own_path_matches_the_endpoint_prefix() {
        assert_eq!(
            own_path(
                "https://chat.example.com/media",
                "https://chat.example.com/media/attachments/1/2/cat.gif"
            ),
            Some("/attachments/1/2/cat.gif")
        );
        assert_eq!(
            own_path(
                "https://cdn.example.com",
                "https://cdn.example.com/avatars/1/abc.png?size=64"
            ),
            Some("/avatars/1/abc.png")
        );
    }

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

    #[test]
    fn own_path_keeps_explicit_default_ports() {
        assert_eq!(
            own_path(
                "https://chat.example.com:443",
                "https://chat.example.com/a.png"
            ),
            Some("/a.png")
        );
    }
}
