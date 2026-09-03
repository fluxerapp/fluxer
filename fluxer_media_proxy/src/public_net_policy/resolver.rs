// SPDX-License-Identifier: AGPL-3.0-or-later

use super::ip_tables::is_public_ip;
use reqwest::dns::{Addrs, Name, Resolve, Resolving};
use std::net::SocketAddr;
use thiserror::Error;
use tokio::net::lookup_host;

pub(super) const MAX_PUBLIC_DNS_ADDRESSES: usize = 64;

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub(super) enum ResolveError {
    #[error("host lookup failed")]
    LookupFailed,
    #[error("host resolved to no address")]
    HostResolvedToNoAddress,
    #[error("host resolved to a blocked network address")]
    BlockedAddress,
    #[error("host resolved to too many network addresses")]
    TooManyAddresses,
}

pub(super) fn screen_resolved_addresses(
    addresses: impl Iterator<Item = SocketAddr>,
) -> Result<Vec<SocketAddr>, ResolveError> {
    let mut screened = Vec::with_capacity(MAX_PUBLIC_DNS_ADDRESSES);
    for address in addresses {
        if screened.len() == MAX_PUBLIC_DNS_ADDRESSES {
            return Err(ResolveError::TooManyAddresses);
        }
        if !is_public_ip(address.ip()) {
            return Err(ResolveError::BlockedAddress);
        }
        screened.push(address);
    }
    if screened.is_empty() {
        return Err(ResolveError::HostResolvedToNoAddress);
    }
    Ok(screened)
}

pub struct PinnedDnsResolver;

impl Resolve for PinnedDnsResolver {
    fn resolve(&self, name: Name) -> Resolving {
        Box::pin(async move {
            let host = name.as_str().to_owned();
            let looked_up = lookup_host((host.as_str(), 0))
                .await
                .map_err(|_| ResolveError::LookupFailed)?;
            let resolved = screen_resolved_addresses(looked_up)?;
            Ok(Box::new(resolved.into_iter()) as Addrs)
        })
    }
}

// reqwest and the hyper connector box this rejection behind errors of their own, so the source
// chain is the only way back to the decision the resolver made.
pub fn is_pinned_dns_failure(error: &(dyn std::error::Error + 'static)) -> bool {
    let mut current = Some(error);
    while let Some(error) = current {
        if error.downcast_ref::<ResolveError>().is_some() {
            return true;
        }
        current = std::error::Error::source(error);
    }
    false
}
