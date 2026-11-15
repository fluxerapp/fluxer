// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Context as _;
use std::{
    net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr},
    time::Duration,
};

pub fn address(
    env: &crate::config::EnvMap,
    host_var: &str,
    port_var: &str,
    default_port: u16,
) -> anyhow::Result<SocketAddr> {
    let host = env.get(host_var).map(str::trim).unwrap_or("127.0.0.1");
    let ip = host
        .parse::<IpAddr>()
        .with_context(|| format!("{host_var} is not an IP address: {host}"))?;
    let port = match env.get(port_var).map(str::trim) {
        Some(port) => port
            .parse::<u16>()
            .with_context(|| format!("{port_var} is not a port number: {port}"))?,
        None => default_port,
    };
    Ok(SocketAddr::new(ip, port))
}

pub async fn run(addr: SocketAddr) -> anyhow::Result<()> {
    let ip = match addr.ip() {
        IpAddr::V4(ip) if ip.is_unspecified() => IpAddr::V4(Ipv4Addr::LOCALHOST),
        IpAddr::V6(ip) if ip.is_unspecified() => IpAddr::V6(Ipv6Addr::LOCALHOST),
        ip => ip,
    };
    let addr = SocketAddr::new(ip, addr.port());
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
