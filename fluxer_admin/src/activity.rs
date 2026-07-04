// SPDX-License-Identifier: AGPL-3.0-or-later

use bytes::BytesMut;
use reqwest::{StatusCode, redirect::Policy};
use std::{
    net::{IpAddr, Ipv4Addr, Ipv6Addr},
    sync::OnceLock,
    time::Duration,
};

const MAX_ACTIVITY_IMAGE_BYTES: usize = 5 * 1024 * 1024;
const MAX_ACTIVITY_IMAGE_REDIRECTS: usize = 3;

pub fn normalize_activity_timestamp(value: i64) -> i64 {
    if value > 10_000_000_000 {
        value / 1000
    } else {
        value
    }
}

pub fn resolve_activity_image_url(
    image: Option<&str>,
    _application_id: Option<&str>,
) -> Option<String> {
    let image = image?.trim();
    if image.is_empty() {
        return None;
    }
    if image.starts_with("http://") || image.starts_with("https://") {
        return sanitize_activity_image_url(image);
    }
    None
}

pub async fn fetch_activity_image(
    url: &str,
) -> Result<(String, bytes::Bytes), ActivityImageFetchError> {
    let mut current =
        sanitize_activity_image_url(url).ok_or(ActivityImageFetchError::InvalidUrl)?;
    for _ in 0..=MAX_ACTIVITY_IMAGE_REDIRECTS {
        validate_activity_image_target(&current)
            .await
            .map_err(|_| ActivityImageFetchError::UntrustedHost)?;
        let response = activity_image_http_client()
            .get(current.clone())
            .send()
            .await
            .map_err(ActivityImageFetchError::Upstream)?;
        if response.status().is_redirection() {
            let Some(location) = response.headers().get(reqwest::header::LOCATION) else {
                return Err(ActivityImageFetchError::BadStatus(response.status()));
            };
            let location = location
                .to_str()
                .map_err(|_| ActivityImageFetchError::InvalidUrl)?;
            current = response
                .url()
                .join(location)
                .ok()
                .and_then(|next| sanitize_activity_image_url(next.as_str()))
                .ok_or(ActivityImageFetchError::InvalidUrl)?;
            continue;
        }
        if !response.status().is_success() {
            return Err(ActivityImageFetchError::BadStatus(response.status()));
        }
        if response
            .content_length()
            .is_some_and(|value| value > MAX_ACTIVITY_IMAGE_BYTES as u64)
        {
            return Err(ActivityImageFetchError::TooLarge);
        }
        let content_type = response
            .headers()
            .get(reqwest::header::CONTENT_TYPE)
            .and_then(|value| value.to_str().ok())
            .map(str::to_owned)
            .ok_or(ActivityImageFetchError::BadContentType)?;
        if !is_allowed_activity_content_type(&content_type) {
            return Err(ActivityImageFetchError::BadContentType);
        }
        let mut response = response;
        let mut bytes = BytesMut::new();
        while let Some(chunk) = response
            .chunk()
            .await
            .map_err(ActivityImageFetchError::Upstream)?
        {
            if bytes.len() + chunk.len() > MAX_ACTIVITY_IMAGE_BYTES {
                return Err(ActivityImageFetchError::TooLarge);
            }
            bytes.extend_from_slice(&chunk);
        }
        return Ok((content_type, bytes.freeze()));
    }
    Err(ActivityImageFetchError::InvalidUrl)
}

#[derive(Debug)]
pub enum ActivityImageFetchError {
    InvalidUrl,
    UntrustedHost,
    Upstream(reqwest::Error),
    BadStatus(StatusCode),
    BadContentType,
    TooLarge,
}

fn sanitize_activity_image_url(url: &str) -> Option<String> {
    let parsed = url::Url::parse(url).ok()?;
    match parsed.scheme() {
        "http" | "https" => {}
        _ => return None,
    }
    let host = parsed.host_str()?;
    if host.eq_ignore_ascii_case("localhost") || !parsed.username().is_empty() {
        return None;
    }
    if parsed.password().is_some() {
        return None;
    }
    if let Ok(ip) = host.parse::<IpAddr>() {
        if !is_public_ip(ip) {
            return None;
        }
    }
    let port = parsed.port_or_known_default()?;
    if port != 80 && port != 443 {
        return None;
    }
    Some(parsed.into())
}

fn is_allowed_activity_content_type(content_type: &str) -> bool {
    let mime_type = content_type
        .split(';')
        .next()
        .map(str::trim)
        .unwrap_or_default();
    mime_type.starts_with("image/") && !mime_type.eq_ignore_ascii_case("image/svg+xml")
}

fn activity_image_http_client() -> &'static reqwest::Client {
    static CLIENT: OnceLock<reqwest::Client> = OnceLock::new();
    CLIENT.get_or_init(|| {
        reqwest::Client::builder()
            .redirect(Policy::none())
            .timeout(Duration::from_secs(10))
            .build()
            .expect("failed to build activity image client")
    })
}

async fn validate_activity_image_target(url: &str) -> Result<(), ()> {
    let parsed = url::Url::parse(url).map_err(|_| ())?;
    let host = parsed.host_str().ok_or(())?;
    if let Ok(ip) = host.parse::<IpAddr>() {
        return is_public_ip(ip).then_some(()).ok_or(());
    }
    let port = parsed.port_or_known_default().ok_or(())?;
    let addresses = tokio::net::lookup_host((host, port))
        .await
        .map_err(|_| ())?;
    let mut found = false;
    for address in addresses {
        found = true;
        if !is_public_ip(address.ip()) {
            return Err(());
        }
    }
    found.then_some(()).ok_or(())
}

fn is_public_ip(ip: IpAddr) -> bool {
    match ip {
        IpAddr::V4(ip) => is_public_ipv4(ip),
        IpAddr::V6(ip) => is_public_ipv6(ip),
    }
}

fn is_public_ipv4(ip: Ipv4Addr) -> bool {
    !(ip.is_private()
        || ip.is_loopback()
        || ip.is_link_local()
        || ip.is_multicast()
        || ip.is_unspecified()
        || ip.is_broadcast()
        || ip.is_documentation()
        || ip.octets()[0] == 0)
}

fn is_public_ipv6(ip: Ipv6Addr) -> bool {
    let segments = ip.segments();
    let is_documentation = segments[0] == 0x2001 && segments[1] == 0x0db8;
    !(ip.is_loopback()
        || ip.is_multicast()
        || ip.is_unspecified()
        || ip.is_unique_local()
        || ip.is_unicast_link_local()
        || is_documentation)
}

#[cfg(test)]
mod tests {
    use super::{
        fetch_activity_image, is_allowed_activity_content_type, normalize_activity_timestamp,
        resolve_activity_image_url, sanitize_activity_image_url,
    };

    #[test]
    fn normalizes_millisecond_timestamps() {
        assert_eq!(
            normalize_activity_timestamp(1_717_261_234_567),
            1_717_261_234
        );
        assert_eq!(normalize_activity_timestamp(1_717_261_234), 1_717_261_234);
    }

    #[test]
    fn accepts_public_activity_images() {
        assert_eq!(
            resolve_activity_image_url(Some("http://coverartarchive.org/release/x/y.jpg"), None),
            Some("http://coverartarchive.org/release/x/y.jpg".to_owned())
        );
        assert_eq!(
            resolve_activity_image_url(Some("https://example.com/track.png"), None),
            Some("https://example.com/track.png".to_owned())
        );
    }

    #[test]
    fn rejects_non_url_activity_images() {
        assert_eq!(
            resolve_activity_image_url(Some("custom:abc123"), None),
            None
        );
        assert_eq!(
            resolve_activity_image_url(Some("opaque:abc123"), None),
            None
        );
        assert_eq!(resolve_activity_image_url(Some("asset-key"), None), None);
    }

    #[test]
    fn rejects_svg_activity_images() {
        assert!(!is_allowed_activity_content_type("image/svg+xml"));
        assert!(!is_allowed_activity_content_type(
            "image/svg+xml; charset=utf-8"
        ));
        assert!(is_allowed_activity_content_type("image/png"));
    }

    #[test]
    fn rejects_local_activity_images() {
        assert_eq!(
            sanitize_activity_image_url("http://localhost/test.png"),
            None
        );
        assert_eq!(
            sanitize_activity_image_url("http://127.0.0.1/test.png"),
            None
        );
    }

    #[tokio::test]
    async fn fetch_rejects_local_activity_images() {
        let result = fetch_activity_image("http://127.0.0.1/test.png").await;
        assert!(result.is_err());
    }
}
