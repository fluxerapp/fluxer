// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Context as _;
use std::{
    env,
    net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr},
    time::Duration,
};

pub async fn run() -> anyhow::Result<()> {
    let addr = target(
        env::var("FLUXER_MEDIA_PROXY_HOST").ok().as_deref(),
        env::var("FLUXER_MEDIA_PROXY_PORT").ok().as_deref(),
    )?;
    probe(addr).await
}

fn target(host: Option<&str>, port: Option<&str>) -> anyhow::Result<SocketAddr> {
    let host = host
        .map(str::trim)
        .filter(|host| !host.is_empty())
        .unwrap_or("127.0.0.1");
    let ip = host
        .parse::<IpAddr>()
        .with_context(|| format!("FLUXER_MEDIA_PROXY_HOST is not an IP address: {host}"))?;
    let ip = match ip {
        IpAddr::V4(ip) if ip.is_unspecified() => IpAddr::V4(Ipv4Addr::LOCALHOST),
        IpAddr::V6(ip) if ip.is_unspecified() => IpAddr::V6(Ipv6Addr::LOCALHOST),
        ip => ip,
    };
    let port = match port.map(str::trim).filter(|port| !port.is_empty()) {
        Some(port) => port
            .parse::<u16>()
            .with_context(|| format!("FLUXER_MEDIA_PROXY_PORT is not a port number: {port}"))?,
        None => 8080,
    };
    Ok(SocketAddr::new(ip, port))
}

async fn probe(addr: SocketAddr) -> anyhow::Result<()> {
    let client = reqwest::Client::builder()
        .connect_timeout(Duration::from_millis(500))
        .timeout(Duration::from_millis(2_000))
        .no_proxy()
        .build()?;
    let status = client
        .get(format!("http://{addr}/_health"))
        .send()
        .await
        .with_context(|| format!("health request to {addr} failed"))?
        .status();
    anyhow::ensure!(
        status == reqwest::StatusCode::OK,
        "health returned {status}"
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn target_defaults_to_loopback() {
        assert_eq!(
            SocketAddr::from(([127, 0, 0, 1], 8080)),
            target(Some("0.0.0.0"), None).unwrap()
        );
        assert_eq!(
            SocketAddr::from(([127, 0, 0, 1], 9000)),
            target(None, Some("9000")).unwrap()
        );
        assert_eq!(
            "[::1]:8080".parse::<SocketAddr>().unwrap(),
            target(Some("::"), Some("")).unwrap()
        );
        assert!(target(Some("0.0.0.0"), Some("nope")).is_err());
    }

    #[tokio::test]
    async fn probe_accepts_ok() {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let app = axum::Router::new().route("/_health", axum::routing::get(async || "OK"));
        tokio::spawn(async move { axum::serve(listener, app).await.unwrap() });
        probe(addr).await.unwrap();
    }

    #[tokio::test]
    async fn probe_rejects_non_ok() {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let addr = listener.local_addr().unwrap();
        let app = axum::Router::new().route(
            "/_health",
            axum::routing::get(async || axum::http::StatusCode::SERVICE_UNAVAILABLE),
        );
        tokio::spawn(async move { axum::serve(listener, app).await.unwrap() });
        assert!(probe(addr).await.is_err());
    }
}
