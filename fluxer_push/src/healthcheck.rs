// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::Mode;
use anyhow::Context as _;
use std::{
    net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr},
    time::Duration,
};

pub async fn run(mode: Mode) -> anyhow::Result<()> {
    let addr = target(
        fluxer_svc::config::optional_env("FLUXER_PUSH_SERVICE_HOST").as_deref(),
        fluxer_svc::config::optional_env("FLUXER_PUSH_SERVICE_PORT").as_deref(),
        mode,
    )?;
    probe(addr).await
}

fn target(host: Option<&str>, port: Option<&str>, mode: Mode) -> anyhow::Result<SocketAddr> {
    let host = host.map(str::trim).unwrap_or("127.0.0.1");
    let ip = host
        .parse::<IpAddr>()
        .with_context(|| format!("FLUXER_PUSH_SERVICE_HOST is not an IP address: {host}"))?;
    let ip = match ip {
        IpAddr::V4(ip) if ip.is_unspecified() => IpAddr::V4(Ipv4Addr::LOCALHOST),
        IpAddr::V6(ip) if ip.is_unspecified() => IpAddr::V6(Ipv6Addr::LOCALHOST),
        ip => ip,
    };
    let port = match port.map(str::trim) {
        Some(port) => port
            .parse::<u16>()
            .with_context(|| format!("FLUXER_PUSH_SERVICE_PORT is not a port number: {port}"))?,
        None => mode.default_port(),
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
