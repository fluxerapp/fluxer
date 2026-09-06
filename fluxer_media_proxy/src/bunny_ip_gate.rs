// SPDX-License-Identifier: AGPL-3.0-or-later

use axum::{
    body::Body,
    extract::{ConnectInfo, State},
    http::{Request, StatusCode},
    middleware::Next,
    response::{IntoResponse, Response},
};
use parking_lot::RwLock;
use std::{
    collections::HashSet,
    net::{IpAddr, SocketAddr},
    sync::Arc,
    time::Duration,
};
use tracing::{info, warn};

pub const BUNNY_IPV4_URL: &str = "https://api.bunny.net/system/edgeserverlist";
pub const BUNNY_IPV6_URL: &str = "https://api.bunny.net/system/edgeserverlist/IPv6";

#[derive(Clone, Debug, Default)]
pub struct Allowlist {
    ips: Arc<HashSet<IpAddr>>,
}

impl Allowlist {
    fn is_empty(&self) -> bool {
        self.ips.is_empty()
    }

    fn len(&self) -> usize {
        self.ips.len()
    }

    pub fn contains(&self, ip: &IpAddr) -> bool {
        self.ips.contains(ip)
    }

    #[cfg(test)]
    pub fn from_ips<I: IntoIterator<Item = IpAddr>>(iter: I) -> Self {
        Self {
            ips: Arc::new(iter.into_iter().collect()),
        }
    }
}

pub struct BunnyIpGate {
    inner: RwLock<Allowlist>,
    trusted_proxies: HashSet<IpAddr>,
    client: reqwest::Client,
    ipv4_url: String,
    ipv6_url: String,
}

impl BunnyIpGate {
    pub fn new(client: reqwest::Client, trusted_proxies: Vec<IpAddr>) -> Self {
        Self::with_urls(
            client,
            trusted_proxies,
            BUNNY_IPV4_URL.to_owned(),
            BUNNY_IPV6_URL.to_owned(),
        )
    }

    pub fn with_urls(
        client: reqwest::Client,
        trusted_proxies: Vec<IpAddr>,
        ipv4_url: String,
        ipv6_url: String,
    ) -> Self {
        Self {
            inner: RwLock::new(Allowlist::default()),
            trusted_proxies: trusted_proxies.into_iter().collect(),
            client,
            ipv4_url,
            ipv6_url,
        }
    }

    pub fn snapshot(&self) -> Allowlist {
        self.inner.read().clone()
    }

    pub async fn refresh_once(&self) -> anyhow::Result<usize> {
        let allow = fetch_allowlist(&self.client, &self.ipv4_url, &self.ipv6_url).await?;
        let len = allow.len();
        *self.inner.write() = allow;
        Ok(len)
    }

    pub fn spawn_background_refresher(self: Arc<Self>, interval: Duration) {
        tokio::spawn(async move {
            let mut ticker = tokio::time::interval(interval);
            ticker.tick().await;
            loop {
                ticker.tick().await;
                match self.refresh_once().await {
                    Ok(count) => info!(count, "bunny ip allowlist refreshed"),
                    Err(err) => warn!(
                        error = %err,
                        "bunny ip allowlist refresh failed; serving previous list"
                    ),
                }
            }
        });
    }

    fn is_trusted_proxy(&self, ip: &IpAddr) -> bool {
        self.trusted_proxies.contains(ip)
    }

    #[cfg(test)]
    pub fn install_for_test(&self, allow: Allowlist) {
        *self.inner.write() = allow;
    }
}

async fn fetch_allowlist(
    client: &reqwest::Client,
    ipv4_url: &str,
    ipv6_url: &str,
) -> anyhow::Result<Allowlist> {
    let (v4, v6) = tokio::join!(
        fetch_ip_list(client, ipv4_url),
        fetch_ip_list(client, ipv6_url)
    );
    let v4 = v4?;
    let v6 = v6?;
    let mut set: HashSet<IpAddr> = HashSet::with_capacity(v4.len() + v6.len());
    set.extend(v4);
    set.extend(v6);
    anyhow::ensure!(
        !set.is_empty(),
        "bunny returned an empty edge ip list from {ipv4_url} + {ipv6_url}"
    );
    Ok(Allowlist { ips: Arc::new(set) })
}

async fn fetch_ip_list(client: &reqwest::Client, url: &str) -> anyhow::Result<Vec<IpAddr>> {
    let raw: Vec<String> = client
        .get(url)
        .send()
        .await?
        .error_for_status()?
        .json()
        .await?;
    Ok(parse_ip_list(&raw, url))
}

fn parse_ip_list(raw: &[String], url: &str) -> Vec<IpAddr> {
    let mut out = Vec::with_capacity(raw.len());
    for entry in raw {
        match entry.trim().parse::<IpAddr>() {
            Ok(ip) => out.push(ip),
            Err(_) => warn!(value = %entry, url, "skipping unparseable bunny ip"),
        }
    }
    out
}

pub async fn gate_middleware(
    State(gate): State<Arc<BunnyIpGate>>,
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    request: Request<Body>,
    next: Next,
) -> Response {
    if is_exempt_path(request.uri().path()) {
        return next.run(request).await;
    }
    let allow = gate.snapshot();
    if allow.is_empty() {
        warn!("bunny ip allowlist is empty; refusing public request");
        return (
            StatusCode::SERVICE_UNAVAILABLE,
            "bunny allowlist not loaded",
        )
            .into_response();
    }
    let client_ip = resolve_client_ip(&gate, &peer, request.headers());
    if !allow.contains(&client_ip) {
        warn!(%client_ip, path = request.uri().path(), "rejecting non-bunny origin");
        return (StatusCode::FORBIDDEN, "origin not in bunny allowlist").into_response();
    }
    next.run(request).await
}

fn is_exempt_path(path: &str) -> bool {
    matches!(
        path,
        "/_health" | "/_metrics" | "/_metadata" | "/_thumbnail" | "/_frames"
    ) || path.starts_with("/v1/relay/")
}

fn resolve_client_ip(
    gate: &BunnyIpGate,
    peer: &SocketAddr,
    headers: &axum::http::HeaderMap,
) -> IpAddr {
    let peer_ip = peer.ip();
    if !gate.is_trusted_proxy(&peer_ip) {
        return peer_ip;
    }
    if let Some(xff) = headers.get("x-forwarded-for").and_then(|v| v.to_str().ok()) {
        for hop in xff.split(',').rev() {
            if let Ok(ip) = hop.trim().parse::<IpAddr>()
                && !gate.is_trusted_proxy(&ip)
            {
                return ip;
            }
        }
    }
    peer_ip
}

pub fn build_refresh_client() -> reqwest::Result<reqwest::Client> {
    reqwest::Client::builder()
        .connect_timeout(Duration::from_secs(5))
        .timeout(Duration::from_secs(20))
        .user_agent(crate::constants::OUTBOUND_USER_AGENT)
        .build()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::ADVERSARIAL_TEXT_INPUTS;
    use axum::{Router, middleware, routing::get};
    use std::net::{Ipv4Addr, Ipv6Addr};
    use tower::ServiceExt;

    fn ip4(a: u8, b: u8, c: u8, d: u8) -> IpAddr {
        IpAddr::V4(Ipv4Addr::new(a, b, c, d))
    }

    fn sock(ip: IpAddr) -> SocketAddr {
        SocketAddr::new(ip, 0)
    }

    #[test]
    fn parse_ip_list_skips_garbage_keeps_good() {
        let raw = vec![
            "1.2.3.4".to_owned(),
            "not-an-ip".to_owned(),
            "  5.6.7.8 ".to_owned(),
            "2a01:4f8::1".to_owned(),
            "".to_owned(),
        ];
        let parsed = parse_ip_list(&raw, "test");
        assert_eq!(parsed.len(), 3);
        assert!(parsed.contains(&ip4(1, 2, 3, 4)));
        assert!(parsed.contains(&ip4(5, 6, 7, 8)));
        assert!(parsed.contains(&IpAddr::V6("2a01:4f8::1".parse::<Ipv6Addr>().unwrap())));
    }

    #[test]
    fn untrusted_peer_xff_is_ignored() {
        let gate = BunnyIpGate::new(reqwest::Client::new(), vec![ip4(10, 0, 0, 1)]);
        let mut headers = axum::http::HeaderMap::new();
        headers.insert("x-forwarded-for", "9.9.9.9".parse().unwrap());
        let resolved = resolve_client_ip(&gate, &sock(ip4(8, 8, 8, 8)), &headers);
        assert_eq!(resolved, ip4(8, 8, 8, 8));
    }

    #[test]
    fn trusted_peer_uses_rightmost_untrusted_xff() {
        let gate = BunnyIpGate::new(
            reqwest::Client::new(),
            vec![ip4(10, 0, 0, 1), ip4(10, 0, 0, 2)],
        );
        let mut headers = axum::http::HeaderMap::new();
        headers.insert(
            "x-forwarded-for",
            "89.187.188.227, 10.0.0.2, 10.0.0.1".parse().unwrap(),
        );
        let resolved = resolve_client_ip(&gate, &sock(ip4(10, 0, 0, 1)), &headers);
        assert_eq!(resolved, ip4(89, 187, 188, 227));
    }

    #[test]
    fn trusted_peer_falls_back_to_peer_when_no_xff() {
        let gate = BunnyIpGate::new(reqwest::Client::new(), vec![ip4(10, 0, 0, 1)]);
        let headers = axum::http::HeaderMap::new();
        let resolved = resolve_client_ip(&gate, &sock(ip4(10, 0, 0, 1)), &headers);
        assert_eq!(resolved, ip4(10, 0, 0, 1));
    }

    #[test]
    fn exempt_paths_cover_internal_routes() {
        assert!(is_exempt_path("/_health"));
        assert!(is_exempt_path("/_metrics"));
        assert!(is_exempt_path("/_metadata"));
        assert!(is_exempt_path("/_thumbnail"));
        assert!(is_exempt_path("/_frames"));
        assert!(is_exempt_path("/v1/relay/abc/def"));
        assert!(!is_exempt_path("/external/some/url"));
        assert!(!is_exempt_path("/some/asset.png"));
        assert!(!is_exempt_path("/"));
    }

    #[test]
    fn snapshot_lookup_matches_loaded_ips() {
        let gate = BunnyIpGate::new(reqwest::Client::new(), vec![]);
        gate.install_for_test(Allowlist::from_ips([ip4(89, 187, 188, 227)]));
        let snap = gate.snapshot();
        assert!(snap.contains(&ip4(89, 187, 188, 227)));
        assert!(!snap.contains(&ip4(1, 1, 1, 1)));
        assert_eq!(snap.len(), 1);
    }

    fn ip6(text: &str) -> IpAddr {
        IpAddr::V6(text.parse::<Ipv6Addr>().unwrap())
    }

    fn forwarded_for(gate: &BunnyIpGate, peer: IpAddr, xff: &str) -> IpAddr {
        let mut headers = axum::http::HeaderMap::new();
        headers.insert("x-forwarded-for", xff.parse().unwrap());
        resolve_client_ip(gate, &sock(peer), &headers)
    }

    #[test]
    fn a_long_forwarded_chain_still_resolves_the_rightmost_untrusted_hop() {
        let trusted: Vec<IpAddr> = (1..=24).map(|last| ip4(10, 0, 0, last)).collect();
        let gate = BunnyIpGate::new(reqwest::Client::new(), trusted.clone());
        let mut chain = vec!["89.187.188.227".to_owned()];
        chain.extend(trusted.iter().map(ToString::to_string));
        assert_eq!(25, chain.len());
        assert_eq!(
            ip4(89, 187, 188, 227),
            forwarded_for(&gate, ip4(10, 0, 0, 24), &chain.join(", "))
        );
        assert_eq!(
            ip4(10, 0, 0, 24),
            forwarded_for(&gate, ip4(10, 0, 0, 24), &chain[1..].join(", "))
        );
    }

    #[test]
    fn unparseable_hops_are_skipped_instead_of_ending_the_scan() {
        let gate = BunnyIpGate::new(
            reqwest::Client::new(),
            vec![ip4(10, 0, 0, 1), ip4(10, 0, 0, 2)],
        );
        assert_eq!(
            ip4(89, 187, 188, 227),
            forwarded_for(
                &gate,
                ip4(10, 0, 0, 1),
                "89.187.188.227, unknown, 10.0.0.2, 10.0.0.1"
            )
        );
        assert_eq!(
            ip4(89, 187, 188, 227),
            forwarded_for(
                &gate,
                ip4(10, 0, 0, 1),
                "89.187.188.227, 10.0.0.1, , not-an-ip"
            )
        );
        assert_eq!(
            ip4(10, 0, 0, 1),
            forwarded_for(&gate, ip4(10, 0, 0, 1), "unknown, also-unknown")
        );
    }

    #[test]
    fn forwarded_ipv6_hops_are_only_accepted_in_their_bare_form() {
        let gate = BunnyIpGate::new(reqwest::Client::new(), vec![ip4(10, 0, 0, 1)]);
        assert_eq!(
            ip6("2a01:4f8::1"),
            forwarded_for(&gate, ip4(10, 0, 0, 1), "2a01:4f8::1, 10.0.0.1")
        );
        assert_eq!(
            ip4(10, 0, 0, 1),
            forwarded_for(&gate, ip4(10, 0, 0, 1), "[2a01:4f8::1]:443, 10.0.0.1")
        );
        assert_eq!(
            ip6("2a01:4f8::2"),
            forwarded_for(
                &gate,
                ip4(10, 0, 0, 1),
                "2a01:4f8::2, [2a01:4f8::1]:443, 10.0.0.1"
            )
        );
    }

    #[test]
    fn a_trusted_ipv6_proxy_is_skipped_like_any_other_hop() {
        let gate = BunnyIpGate::new(reqwest::Client::new(), vec![ip6("2a01:4f8::1")]);
        assert_eq!(
            ip4(89, 187, 188, 227),
            forwarded_for(&gate, ip6("2a01:4f8::1"), "89.187.188.227, 2a01:4f8::1")
        );
    }

    #[test]
    fn adversarial_paths_are_never_exempt_from_the_bunny_gate() {
        for text in ADVERSARIAL_TEXT_INPUTS {
            for path in [
                (*text).to_owned(),
                format!("/{text}"),
                format!("/avatars/1/{text}.png"),
                format!("/external/{text}"),
                format!("/_health{text}"),
                format!("/v1/relay{text}"),
                format!("/v1/relay/{text}"),
            ] {
                if !is_exempt_path(&path) {
                    continue;
                }
                assert!(
                    path == "/_health" || path.starts_with("/v1/relay/"),
                    "{path:?} is exempt from the bunny gate"
                );
            }
        }
    }

    #[tokio::test]
    async fn gate_after_successful_refresh_never_returns_503() {
        let gate = Arc::new(BunnyIpGate::new(reqwest::Client::new(), vec![]));
        gate.install_for_test(Allowlist::from_ips([ip4(89, 187, 188, 227)]));
        let app = Router::new()
            .route("/asset.png", get(|| async { "ok" }))
            .layer(middleware::from_fn_with_state(gate, gate_middleware));
        let allowed = app
            .clone()
            .oneshot(
                Request::builder()
                    .uri("/asset.png")
                    .extension(ConnectInfo(sock(ip4(89, 187, 188, 227))))
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(StatusCode::OK, allowed.status());
        let foreign = app
            .oneshot(
                Request::builder()
                    .uri("/asset.png")
                    .extension(ConnectInfo(sock(ip4(1, 1, 1, 1))))
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(StatusCode::FORBIDDEN, foreign.status());
    }

    #[tokio::test]
    async fn an_empty_allowlist_refuses_a_public_request() {
        let gate = Arc::new(BunnyIpGate::new(reqwest::Client::new(), vec![]));
        let app = Router::new()
            .route("/asset.png", get(|| async { "ok" }))
            .layer(middleware::from_fn_with_state(gate, gate_middleware));
        let refused = app
            .oneshot(
                Request::builder()
                    .uri("/asset.png")
                    .extension(ConnectInfo(sock(ip4(89, 187, 188, 227))))
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(StatusCode::SERVICE_UNAVAILABLE, refused.status());
    }
}
