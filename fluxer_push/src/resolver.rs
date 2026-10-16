// SPDX-License-Identifier: AGPL-3.0-or-later

use reqwest::dns::{Addrs, Name, Resolve, Resolving};
use std::net::{IpAddr, SocketAddr};
use std::sync::Arc;
use tokio::net::lookup_host;

type ResolveError = Box<dyn std::error::Error + Send + Sync>;

pub const BLOCKED_ADDRESS_ERROR: &str = "host resolved into blocked address space";

pub struct PublicOnlyResolver {
    private_hosts: Arc<[String]>,
}

impl PublicOnlyResolver {
    pub fn new(private_hosts: &[String]) -> Self {
        Self {
            private_hosts: private_hosts.into(),
        }
    }
}

impl Resolve for PublicOnlyResolver {
    fn resolve(&self, name: Name) -> Resolving {
        let host = name.as_str().to_owned();
        let allow_private = is_private_host(&host, &self.private_hosts);
        Box::pin(async move {
            let resolved: Vec<SocketAddr> = lookup_host((host.as_str(), 0)).await?.collect();
            screen(resolved, allow_private)
        })
    }
}

pub fn is_private_host(host: &str, private_hosts: &[String]) -> bool {
    let host = host.strip_suffix('.').unwrap_or(host);
    private_hosts
        .iter()
        .any(|private| private.eq_ignore_ascii_case(host))
}

fn screen(resolved: Vec<SocketAddr>, allow_private: bool) -> Result<Addrs, ResolveError> {
    if resolved.is_empty() {
        return Err("host resolved to no addresses".into());
    }
    if !allow_private && resolved.iter().any(|addr| is_blocked(addr.ip())) {
        return Err(BLOCKED_ADDRESS_ERROR.into());
    }
    Ok(Box::new(resolved.into_iter()))
}

pub fn is_blocked(ip: IpAddr) -> bool {
    !fluxer_common::public_ip::is_public_ip(ip)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lan_address() -> Vec<SocketAddr> {
        vec![SocketAddr::from(([192, 168, 1, 20], 0))]
    }

    #[test]
    fn a_private_address_is_refused_by_default() {
        let error = screen(lan_address(), false).err().unwrap();
        assert_eq!(error.to_string(), BLOCKED_ADDRESS_ERROR);
    }

    #[test]
    fn a_listed_private_host_may_resolve_to_a_private_address() {
        let hosts = vec!["ntfy.example.com".to_owned()];
        assert!(is_private_host("NTFY.example.com.", &hosts));
        assert!(!is_private_host("other.example.com", &hosts));
        assert!(screen(lan_address(), true).is_ok());
    }
}
